#include "tb_internal.h"

// IR ANALYSIS
void tb_function_calculate_use_count(const TB_Function* f, int use_count[]) {
    for (size_t i = 0; i < f->nodes.count; i++)
        use_count[i] = 0;

#define X(reg) use_count[reg] += 1
    TB_FOR_EACH_NODE(n, f) {
        switch (n->type) {
            TB_FOR_EACH_REG_IN_NODE(X);
			default: tb_todo();
        }
    }
#undef X
}

int tb_function_find_uses_of_node(const TB_Function* f, TB_Reg def, TB_Reg uses[]) {
    size_t count = 0;

#define X(reg) \
if (reg == def) { uses[count++] = reg; }
    TB_FOR_EACH_NODE(n, f) {
        switch (n->type) {
            TB_FOR_EACH_REG_IN_NODE(X);
			default: tb_todo();
        }
    }
#undef X

    return count;
}

void tb_function_find_replace_reg(TB_Function* f, TB_Reg find, TB_Reg replace) {
#define X(reg) \
if (reg == find) reg = replace
    TB_FOR_EACH_NODE(n, f) {
        switch (n->type) {
            TB_FOR_EACH_REG_IN_NODE(X);
			default: tb_panic("Unknown node type: %d", n->type);
        }
    }
#undef X
}

TB_Reg tb_find_reg_from_label(TB_Function* f, TB_Label id) {
    TB_FOR_EACH_NODE(n, f) {
        if (n->type == TB_LABEL && n->label.id == id) return (n - f->nodes.data);
    }

    return TB_NULL_REG;
}

TB_Reg tb_function_insert_after(TB_Function* f, TB_Reg at) {
    tb_function_reserve_nodes(f, 1);

    TB_Reg next = f->nodes.data[at].next;

    TB_Reg r = f->nodes.count++;
    f->nodes.data[r] = (TB_Node) { .type = TB_NULL, .dt = TB_TYPE_VOID, .next = next };
    f->nodes.data[at].next = r;

    if (f->nodes.end == at) f->nodes.end = r;
    return r;
}

// NOTE(NeGate): Any previous TB_Reg you have saved locally,
// update them or at least shift over all the indices based on `at`
//
// TODO(NeGate): Move this out of this file once it's relevant
// TODO(NeGate): Implement multiple return statements, VLA insertion, and proper labels
TB_Reg tb_insert_copy_ops(TB_Function* f, const TB_Reg* params, TB_Reg at,
						  const TB_Function* src_func, TB_Reg src_base, int count) {
    tb_panic("TODO: implement tb_insert_copy_ops");
    return 0;
}

TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count) {
    size_t count = 0;
    TB_Label* preds = tb_tls_push(tls, 0);

    TB_Node* nodes = f->nodes.data;
    TB_Reg label = 1;
    do {
        TB_Node* start = &nodes[label];
        TB_Reg terminator = start->label.terminator;
        TB_Label id = start->label.id;

        TB_Node* end = &nodes[terminator];
        if (start == end) break;

		switch (end->type) {
			case TB_LABEL: {
				if (l == end->label.id) {
					*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = id;
					count++;
				}

				label = terminator;
				break;
			}
			case TB_IF: {
				if (l == end->if_.if_true) {
					*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = id;
					count++;
				}

				if (l == end->if_.if_false) {
					*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = id;
					count++;
				}

				label = end->next;
				break;
			}
			case TB_GOTO: {
				if (l == end->goto_.label) {
					*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = id;
					count++;
				}

				label = end->next;
				break;
			}
			case TB_SWITCH: {
				size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
				loop(i, entry_count) {
					TB_SwitchEntry* entry =
                    (TB_SwitchEntry*)&f->vla.data[end->switch_.entries_start + (i * 2)];

					if (l == entry->key) {
						*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = id;
						count++;
					}
				}

				if (l == end->switch_.default_label) {
					*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = id;
					count++;
				}

				label = end->next;
				break;
			}

			case TB_RET: label = end->next; break;

			case TB_NULL: goto done;
			default: tb_todo();
		}
    } while (label);

	done:
    *dst_count = count;
    return preds;
}
