// Based on "Simple and Efficient SSA Construction":
// https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf
#include "../tb_internal.h"

static bool tb_is_stack_slot_coherent(TB_Function* f, TB_Register address, TB_DataType* dt);

#define MAX_IR_REGISTER_NAMES

typedef struct Mem2Reg_Ctx {
	TB_Function* f;
	size_t label_count;
	
	// Stack slots we're going to convert into
	// SSA form
	size_t to_promote_count;
	TB_Register* to_promote;
	
	// [to_promote_count][label_count]
	TB_Register* current_def;
	
	// [to_promote_count][label_count]
	TB_Register* incomplete_phis;
	
	// [label_count]
	bool* sealed_blocks;
	
	// [label_count]
	int* pred_count;
	// [label_count][pred_count[i]]
	TB_Label** preds;
	
	// NOTE(NeGate): This is a system used to bind registers
	// to another value so that if new registers are inserted
	// you have a consistent lookup
	int name_capacity, name_count;
	TB_Register* names;
} Mem2Reg_Ctx;

static int bind_name(Mem2Reg_Ctx* restrict c, TB_Register r) {
	assert(c->name_count+1 < c->name_capacity && "Ran out of names");
	
	int name = c->name_count++;
	c->names[name] = r;
	return name;
}

static TB_Register read_name(Mem2Reg_Ctx* restrict c, int name) {
	assert(name < c->name_count);
	return c->names[name];
}

static void unbind_name(Mem2Reg_Ctx* restrict c, int name) {
	assert(name < c->name_count);
	
	// remove swap
	c->name_count--;
	if (c->name_count > 0) {
		c->names[name] = c->names[c->name_count - 1];
	}
}

static int get_variable_id(Mem2Reg_Ctx* restrict c, TB_Register r) {
	// TODO(NeGate): Maybe we speed this up... maybe it doesn't matter :P
	loop(i, c->to_promote_count) {
		if (c->to_promote[i] == r) return (int)i;
	}
	
	return -1;
}

// NOTE(NeGate): Be careful when using this function because it will shift over registers
// it does correct the ones in the context but not any you loaded because... i cant?
// I just recommend not caching indices across this function.
//
// This doesn't really generate a PHI node, it just produces a NULL node which will
// be mutated into a PHI node by the rest of the code.
static TB_Register new_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Label block, TB_DataType dt) {
	TB_Register label_reg = tb_find_reg_from_label(f, block);
	
	TB_Register new_phi_reg = label_reg + 1;
	tb_insert_op(f, new_phi_reg);
	f->nodes.dt[new_phi_reg] = dt;
	
	// Update the register references
	//
	// TODO(NeGate): We don't update the to_promote list because it's hoisted to the entry label
	// so they should always be infront of it.
	loop(i, c->to_promote_count) {
		if (c->to_promote[i] + 1 >= new_phi_reg) c->to_promote[i]++;
	}
	
	loop(i, c->label_count * c->to_promote_count) {
		if (c->current_def[i] + 1 >= new_phi_reg) c->current_def[i]++;
	}
	
	loop(i, c->label_count * c->to_promote_count) {
		if (c->incomplete_phis[i] + 1 >= new_phi_reg) c->incomplete_phis[i]++;
	}
	
	loop(i, c->name_count) {
		if (c->names[i] + 1 >= new_phi_reg) c->names[i]++;
	}
	
	return new_phi_reg;
}

static TB_Register read_variable_recursive(Mem2Reg_Ctx* restrict c, int var, TB_Label block);
static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Register phi_reg, TB_Label label, TB_Register reg);
static TB_Register try_remove_trivial_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Register phi_reg);
static TB_Register add_phi_operands(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Register phi_reg, TB_Label label, int var);

////////////////////////////////
// Algorithm 1: Implementation of local value numbering
////////////////////////////////
static void write_variable(Mem2Reg_Ctx* c, int var, TB_Label block, TB_Register value) {
	c->current_def[(var * c->label_count) + block] = value;
}

static TB_Register read_variable(Mem2Reg_Ctx* restrict c, int var, TB_Label block) {
	if (c->current_def[(var * c->label_count) + block] != 0) {
		return c->current_def[(var * c->label_count) + block];
	}
	
	return read_variable_recursive(c, var, block);
}

////////////////////////////////
// Algorithm 2: Implementation of global value numbering
////////////////////////////////
static TB_Register read_variable_recursive(Mem2Reg_Ctx* restrict c, int var, TB_Label block) {
	TB_Register val = 0;
	
	if (!c->sealed_blocks[block]) {
		// incomplete CFG
		val = new_phi(c, c->f, block, c->f->nodes.dt[c->to_promote[var]]);
		c->incomplete_phis[(block * c->to_promote_count) + var] = val;
	} else if (c->pred_count[block] == 0) {
		// TODO(NeGate): Idk how to handle this ngl, i
		// don't think it's possible tho
		abort();
	} else if (c->pred_count[block] == 1) {
		// Optimize the common case of one predecessor: No phi needed
		val = read_variable(c, var, c->preds[block][0]);
	} else {
		// Break potential cycles with operandless phi
		val = new_phi(c, c->f, block, c->f->nodes.dt[c->to_promote[var]]);
		write_variable(c, var, block, val);
		val = add_phi_operands(c, c->f, val, block, var);
	}
	
	write_variable(c, var, block, val);
	return val;
}

static TB_Register add_phi_operands(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Register phi_reg, TB_Label block, int var) {
	// Determine operands from predecessors
	int name = bind_name(c, phi_reg);
	
	loop(i, c->pred_count[block]) {
		TB_Register val = read_variable(c, var, c->preds[block][i]);
		
		add_phi_operand(c, c->f, read_name(c, name), c->preds[block][i], val);
	}
	
	TB_Register reg = try_remove_trivial_phi(c, c->f, read_name(c, name));
	unbind_name(c, name);
	
	return reg;
}

// Algorithm 3: Detect and recursively remove a trivial phi function
static TB_Register try_remove_trivial_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Register phi_reg) {
	//TB_Register same = TB_NULL_REG;
	return phi_reg;
}

static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Register phi_reg, TB_Label label, TB_Register reg) {
	// we're using NULL nodes as the baseline PHI0
	if (f->nodes.type[phi_reg] == TB_NULL) {
		f->nodes.type[phi_reg] = TB_PHI1;
		f->nodes.dt[phi_reg] = f->nodes.dt[reg];
		f->nodes.payload[phi_reg] = (TB_RegPayload){
			.phi1 = {
				.a_label = tb_find_reg_from_label(f, label),
				.a = reg
			}
		};
	} else if (f->nodes.type[phi_reg] == TB_PHI1) {
		TB_Register a_label = f->nodes.payload[phi_reg].phi1.a_label;
		TB_Register a = f->nodes.payload[phi_reg].phi1.a;
		
		if (a == reg) {
			f->nodes.type[phi_reg] = TB_PASS;
			f->nodes.dt[phi_reg] = f->nodes.dt[a];
			f->nodes.payload[phi_reg] = (TB_RegPayload){
				.pass = a
			};
		} else {
			f->nodes.type[phi_reg] = TB_PHI2;
			f->nodes.dt[phi_reg] = f->nodes.dt[a];
			f->nodes.payload[phi_reg] = (TB_RegPayload){
				.phi2 = {
					.a_label = a_label,
					.a = a,
					.b_label = tb_find_reg_from_label(f, label),
					.b = reg
				}
			};
		}
	} else {
		// TODO(NeGate): 
		abort();
	}
}

// Algorithm 4: Handling incomplete CFGs
static void seal_block(Mem2Reg_Ctx* restrict c, TB_Label block) {
	loop(i, c->to_promote_count) {
		TB_Register phi_reg = c->incomplete_phis[(block * c->to_promote_count) + i];
		if (phi_reg) add_phi_operands(c, c->f, phi_reg, block, i);
	}
	
	c->sealed_blocks[block] = true;
}

// NOTE(NeGate): All locals were moved into the first basic block by
// opt_hoist_locals earlier
bool tb_opt_mem2reg(TB_Function* f) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	////////////////////////////////
	// Decide which stack slots to promote
	////////////////////////////////
	size_t to_promote_count = 0;
	TB_Register* to_promote = tb_tls_push(tls, 0);
	
	TB_Register entry_terminator = f->nodes.payload[1].label.terminator;
	loop_range(i, 1, entry_terminator) {
		if (f->nodes.type[i] == TB_LOCAL || f->nodes.type[i] == TB_PARAM_ADDR) {
			TB_DataType dt;
			if (tb_is_stack_slot_coherent(f, i, &dt)) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = i;
				to_promote_count++;
				
				f->nodes.dt[i] = dt;
			}
		}
	}
	
	if (to_promote_count == 0) {
		// doesn't need to mem2reg
		return false;
	}
	
	Mem2Reg_Ctx c = { 0 };
	c.f = f;
	
	c.to_promote_count = to_promote_count;
	c.to_promote = to_promote;
	
	c.label_count = f->label_count;
	c.current_def = tb_tls_push(tls, to_promote_count * c.label_count * sizeof(TB_Register));
	memset(c.current_def, 0, to_promote_count * c.label_count * sizeof(TB_Register));
	
	c.incomplete_phis = tb_tls_push(tls, to_promote_count * c.label_count * sizeof(TB_Register));
	memset(c.incomplete_phis, 0, to_promote_count * c.label_count * sizeof(TB_Register));
	
	// TODO(NeGate): Maybe we should bitpack this?
	c.sealed_blocks = tb_tls_push(tls, c.label_count * sizeof(bool));
	memset(c.sealed_blocks, 0, c.label_count * sizeof(bool));
	
	c.name_count = 0;
	c.name_capacity = 1024;
	c.names = tb_tls_push(tls, 1024 * sizeof(TB_Register));
	
	// Calculate all the immediate predecessors
	// First BB has no predecessors
	{
		c.pred_count = tb_tls_push(tls, c.label_count * sizeof(int));
		c.preds = tb_tls_push(tls, c.label_count * sizeof(TB_Label*));
		
		// entry label has no predecessors
		c.pred_count[0] = 0;
		c.preds[0] = NULL;
		
		loop_range(j, 1, c.label_count) {
			c.preds[j] = (TB_Label*)tb_tls_push(tls, 0);
			tb_calculate_immediate_predeccessors(f, tls, j, &c.pred_count[j]);
		}
	}
	
	TB_Label block = 0;
	TB_Register j = 1;
	while (j < f->nodes.count) {
		TB_RegType reg_type = f->nodes.type[j];
		TB_RegPayload p = f->nodes.payload[j];
		
		if (reg_type == TB_LABEL) {
			block = p.label.id;
		} else if (reg_type == TB_LOCAL) {
			int var = get_variable_id(&c, j);
			if (var >= 0) tb_kill_op(f, j);
		} else if (reg_type == TB_PARAM_ADDR) {
			// Parameter stack slots map to parameter registers
			// so we need to tell mem2reg about that.
			int var = get_variable_id(&c, j);
			if (var >= 0) {
				write_variable(&c, var, block, f->nodes.payload[j].param_addr.param);
				tb_kill_op(f, j);
			}
		} else if (reg_type == TB_LOAD) {
			int var = get_variable_id(&c, p.load.address);
			if (var >= 0) {
				int name = bind_name(&c, j);
				
				TB_Register value = read_variable(&c, var, block);
				assert(value);
				
				TB_Register k = read_name(&c, name);
				f->nodes.type[k] = TB_PASS;
				f->nodes.payload[k].pass = value;
				unbind_name(&c, name);
			}
		} else if (reg_type == TB_STORE) {
			int var = get_variable_id(&c, p.store.address);
			if (var >= 0) {
				tb_kill_op(f, j);
				write_variable(&c, var, block, p.store.value);
			}
		}
		
		j++;
	}
	
	loop_range(j, 1, c.label_count) {
		seal_block(&c, j);
	}
	
	return true;
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and properties.
static bool tb_is_stack_slot_coherent(TB_Function* f, TB_Register address, TB_DataType* out_dt) {
	bool initialized = false;
	TB_DataType dt;
	
	// pick the first load/store and use that as the baseline
	for (TB_Register i = address; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOAD && f->nodes.payload[i].load.address == address) {
			if (!initialized) dt = f->nodes.dt[i];
			else if (TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[i])) return false;
			else if (f->nodes.payload[i].load.is_volatile) return false;
		} else if (f->nodes.type[i] == TB_STORE && f->nodes.payload[i].store.address == address) {
			if (!initialized) dt = f->nodes.dt[i];
			else if (TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[i])) return false;
			else if (f->nodes.payload[i].store.is_volatile) return false;
		} else if (f->nodes.type[i] == TB_MEMSET && f->nodes.payload[i].mem_op.dst == address) {
			return false;
		} else if (f->nodes.type[i] == TB_ARRAY_ACCESS && f->nodes.payload[i].array_access.base == address) {
			return false;
		} else if (f->nodes.type[i] == TB_MEMBER_ACCESS && f->nodes.payload[i].member_access.base == address) {
			return false;
		}
	}
	
	*out_dt = dt;
	return true;
}
