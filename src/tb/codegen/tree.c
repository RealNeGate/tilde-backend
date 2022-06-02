#include "tree.h"

static TreeNode* append(TreeNodeArena* arena) {
    TreeNodePage* p = arena->top;
    if (p->count + 1 >= COUNTOF(p->nodes)) {
        // recycle / spawn new page
        TreeNodePage* new_p;
        if (p->next) {
            new_p = p->next;
            new_p->count = 0;
        } else {
            new_p = tb_platform_heap_alloc(sizeof(TreeNodePage));
            if (!new_p) { tb_panic("Out of memory!"); }

            new_p->next  = NULL;
            new_p->count = 0;
        }

        p->next = new_p;
        p = new_p;
    }

    return &p->nodes[p->count++];
}

static TreeNode* push_leaf(TreeNodeArena* arena, TB_Reg r) {
    TreeNode* n = append(arena);
    *n = (TreeNode) { .reg = r };
    return n;
}

static TreeNode* push_unary(TreeNodeArena* arena, TB_Reg r, TreeNode* a) {
    TreeNode* n = append(arena);
    *n = (TreeNode) { .reg = r, .operands = { a } };
    return n;
}

static TreeNode* push_binary(TreeNodeArena* arena, TB_Reg r, TreeNode* a, TreeNode* b) {
    TreeNode* n = append(arena);
    *n = (TreeNode) { .reg = r, .operands = { a, b } };
    return n;
}

static TreeNode* find(TreeNodeArena* arena, TB_Reg r) {
    for (TreeNodePage* p = arena->base; p; p = p->next) {
        loop(i, p->count) if (p->nodes[i].reg == r) return &p->nodes[i];
    }

    return NULL;
}

static TreeNode* walk(TreeNodeArena* arena, TB_Function* f, TB_Reg* use_count, TB_Reg r) {
    TB_Node* restrict n = &f->nodes.data[r];
    TreeNode* result = NULL;
    switch (n->type) {
		case TB_PARAM:
		case TB_LOCAL:
		case TB_PARAM_ADDR:
		case TB_INTEGER_CONST:
		case TB_FLOAT_CONST:
		case TB_STRING_CONST:
		case TB_GLOBAL_ADDRESS:
		case TB_FUNC_ADDRESS:
		case TB_EXTERN_ADDRESS:
		case TB_PHI2:
		case TB_CALL:
		case TB_ECALL:
		case TB_VCALL:
		result = push_leaf(arena, r);
		break;

		case TB_LOAD:
		result = push_unary(arena, r, walk(arena, f, use_count, n->load.address));
		break;

		case TB_TRUNCATE:
		case TB_ZERO_EXT:
		case TB_SIGN_EXT:
		case TB_FLOAT_EXT:
		case TB_INT2PTR:
		case TB_PTR2INT:
		case TB_FLOAT2INT:
		case TB_INT2FLOAT:
		case TB_NEG:
		case TB_NOT:
		case TB_X86INTRIN_SQRT:
		case TB_X86INTRIN_RSQRT:
		result = push_unary(arena, r, walk(arena, f, use_count, n->unary.src));
		break;

		case TB_ARRAY_ACCESS:
		result = push_binary(arena, r, walk(arena, f, use_count, n->array_access.base), walk(arena, f, use_count, n->array_access.index));
		break;

		case TB_MEMBER_ACCESS:
		result = push_unary(arena, r, walk(arena, f, use_count, n->member_access.base));
		break;

		case TB_FADD:
		case TB_FSUB:
		case TB_FMUL:
		case TB_FDIV:
		result = push_binary(arena, r, walk(arena, f, use_count, n->f_arith.a), walk(arena, f, use_count, n->f_arith.b));
		break;

		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL:
		case TB_SHL:
		case TB_SHR:
		case TB_SAR:
		case TB_UDIV:
		case TB_SDIV:
		case TB_UMOD:
		case TB_SMOD:
		result = push_binary(arena, r, walk(arena, f, use_count, n->i_arith.a), walk(arena, f, use_count, n->i_arith.b));
		break;

		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE:
		case TB_CMP_FLT:
		case TB_CMP_FLE:
		result = push_binary(arena, r, walk(arena, f, use_count, n->cmp.a), walk(arena, f, use_count, n->cmp.b));
		break;

		default: tb_unreachable();
    }

    // we need to either make a decision to spawn a shared point or
    // duplicate the node, this is target dependent but for now we'll
    // just use some simple heuristics
    if (TB_UNLIKELY(use_count[r] > 1)) {
        // volatile load always shares, immediates never split
        if (n->type == TB_INTEGER_CONST) {
			return result;
		}

        // shared point
        TreeNode* search = find(arena, r);
        if (search && search->use_count) {
            // kill the one we generated...
            *result = (TreeNode) { 0 };

            search->use_count++;
            return search;
        }

        result->use_count = 1;
    }

    return result;
}

static void schedule_phis(TreeNodeArena* arena, TB_Function* f, TB_Reg* use_count, TB_Reg label_reg, TB_Reg start, TB_Reg end) {
    TB_FOR_EACH_NODE_RANGE(n, f, start, end) {
		TB_Reg i = n - f->nodes.data;

        if (tb_node_is_phi_node(f, i)) {
			int count = tb_node_get_phi_width(f, i);
			TB_PhiInput* inputs = tb_node_get_phi_inputs(f, i);

			loop(i, count) {
				if (inputs[i].label == label_reg) {
					TB_Reg src = inputs[i].val;
					TreeNode* tree_node = push_unary(arena, n - f->nodes.data, walk(arena, f, use_count, src));

					TreeNode* chain = append(arena);
					*chain = (TreeNode) { 0, .operands = { tree_node } };

					if (arena->last_link) arena->last_link->operands[1] = chain;
					else arena->first_link = chain;

					arena->last_link = chain;
					goto success;
				}
			}

			tb_unreachable();
			success:;
		}
    }
}

TreeNode* tb_tree_generate(TreeNodeArena* arena, TB_Function* f, TB_Reg* use_count, TB_Reg bb, TB_Reg bb_end) {
    if (!arena->base) {
        arena->base = arena->top = tb_platform_heap_alloc(sizeof(TreeNodePage));
        arena->base->next = NULL;
        arena->base->count = 0;
    }

    arena->first_link = NULL;
    arena->last_link = NULL;

    TB_Reg label_reg = bb;

    // first node in the basic block
    bb = f->nodes.data[bb].next;
    if (bb != bb_end) {
        // Evaluate all side effect instructions
        TB_FOR_EACH_NODE_RANGE(n, f, bb, bb_end) {
            TB_Reg r = n - f->nodes.data;
            TB_NodeTypeEnum reg_type = n->type;

            switch (reg_type) {
				case TB_STORE: {
					TreeNode* tree_node = push_binary(arena, r, walk(arena, f, use_count, n->store.address), walk(arena, f, use_count, n->store.value));

					TreeNode* chain = append(arena);
					*chain = (TreeNode) { 0, .operands = { tree_node } };

					if (arena->last_link) arena->last_link->operands[1] = chain;
					else arena->first_link = chain;

					arena->last_link = chain;
					break;
				}

				default:
                if (TB_IS_NODE_SIDE_EFFECT(reg_type)) tb_todo();
                break;
            }
        }
    }

    TB_Node*  end = &f->nodes.data[bb_end];
    TreeNode* tree_node = NULL;
    if (end->type == TB_IF) {
        TB_Reg if_true_reg = tb_find_reg_from_label(f, end->if_.if_true);
        TB_Reg if_true_reg_end = f->nodes.data[if_true_reg].label.terminator;
        schedule_phis(arena, f, use_count, label_reg, if_true_reg, if_true_reg_end);

        TB_Reg if_false_reg = tb_find_reg_from_label(f, end->if_.if_false);
        TB_Reg if_false_reg_end = f->nodes.data[if_false_reg].label.terminator;
        schedule_phis(arena, f, use_count, label_reg, if_false_reg, if_false_reg_end);

        tree_node = push_unary(arena, bb_end, walk(arena, f, use_count, end->if_.cond));
    } else if (end->type == TB_GOTO) {
        TB_Reg target = tb_find_reg_from_label(f, end->goto_.label);
        TB_Reg target_end = f->nodes.data[target].label.terminator;
        schedule_phis(arena, f, use_count, label_reg, target, target_end);

        tree_node = push_leaf(arena, bb_end);
    } else if (end->type == TB_LABEL) {
        TB_Reg next_terminator = end->label.terminator;
        schedule_phis(arena, f, use_count, label_reg, bb_end, next_terminator);

        tree_node = push_leaf(arena, bb_end);
    } else if (end->type == TB_RET) {
        if (end->ret.value) {
            tree_node = push_unary(arena, bb_end, walk(arena, f, use_count, end->ret.value));
        } else {
            tree_node = push_leaf(arena, bb_end);
        }
    } else if (end->type == TB_TRAP || end->type == TB_UNREACHABLE) {
        tree_node = push_leaf(arena, bb_end);
    } else {
        tb_todo();
    }

    // Append terminator
    TreeNode* chain = append(arena);
    *chain = (TreeNode) { 0, .operands = { tree_node } };

    if (arena->last_link) arena->last_link->operands[1] = chain;
    else arena->first_link = chain;

    return arena->first_link;
}

static void print_node(TreeNode* n, int depth) {
    for (int i = 0; i < depth; i++) printf("  ");

	if (n->use_count) printf("Node SHARED r%d (%p):\n", n->reg, n);
    else printf("Node r%d (%p):\n", n->reg, n);

    if (n->operands[0]) print_node(n->operands[0], depth + 1);
    if (n->operands[1]) print_node(n->operands[1], depth + 1);
}

void tb_tree_print(TreeNode* node) {
    while (node) {
        assert(node->reg == TB_NULL_REG);

        print_node(node->operands[0], 0);
        printf("\n");

        node = node->operands[1];
    }
}

void tb_tree_clear(TreeNodeArena* arena) {
    for (TreeNodePage* p = arena->base; p; p = p->next) {
        p->count = 0;
    }
    arena->top = arena->base;
}

void tb_tree_free(TreeNodeArena* arena) {
    // TODO(NeGate): why did i comment this out...
    /*TreeNodePage* p = arena->base;
    while (p) {
        TreeNodePage* next = p->next;
        tb_platform_heap_free(p);
        p = next;
    }

    arena->base = arena->top = NULL;*/
}
