// This file contains generic analysis functions for operating on the TBIR
#include "tb_internal.h"

typedef struct {
    int* doms;

    size_t traversal_filled;
    TB_Label* traversal;

    bool* visited;
} DomContext;

// ignores the start node when doing the traversal
static void postorder(TB_Function* f, DomContext* ctx, TB_Reg bb) {
    assert(f->nodes[bb].type == TB_LABEL);

    TB_Node* start = &f->nodes[bb];
    TB_Label label_id = start->label.id;

    if (ctx->visited[label_id]) {
        return;
    }

    ctx->visited[label_id] = true;

    TB_Reg bb_end = start->label.terminator;
    TB_Node* end = &f->nodes[bb_end];

    if (end->type == TB_RET) {
        /* RET can't do shit in this context */
    } else if (end->type == TB_LABEL) {
        postorder(f, ctx, bb_end);
    } else if (end->type == TB_GOTO) {
        postorder(f, ctx, tb_find_reg_from_label(f, end->goto_.label));
    } else if (end->type == TB_IF) {
        postorder(f, ctx, tb_find_reg_from_label(f, end->if_.if_true));
        postorder(f, ctx, tb_find_reg_from_label(f, end->if_.if_false));
    } else if (end->type == TB_SWITCH) {
        size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
        TB_SwitchEntry* entries = (TB_SwitchEntry*) &f->vla.data[end->switch_.entries_start];

        FOREACH_N(i, 0, entry_count) {
            postorder(f, ctx, tb_find_reg_from_label(f, entries[i].value));
        }

        postorder(f, ctx, tb_find_reg_from_label(f, end->switch_.default_label));
    } else tb_todo();

    ctx->traversal[ctx->traversal_filled++] = label_id;
}

static int find_traversal_index(DomContext* ctx, TB_Label l) {
    loop(i, ctx->traversal_filled) {
        if (ctx->traversal[i] == l) return i;
    }

    tb_todo();
}

// this takes in postorder indices
static TB_Label intersect(DomContext* ctx, int a, int b) {
    while (a != b) {
        // while (finger1 < finger2)
        //   finger1 = doms[finger1]
        while (a < b) {
            a = find_traversal_index(ctx, ctx->doms[ctx->traversal[a]]);
        }

        // while (finger2 < finger1)
        //   finger2 = doms[finger2]
        while (b < a) {
            b = find_traversal_index(ctx, ctx->doms[ctx->traversal[b]]);
        }
    }

    return a;
}

TB_Predeccesors tb_get_temp_predeccesors(TB_Function* f, TB_TemporaryStorage* tls) {
    TB_Predeccesors p = { 0 };
    p.count = tb_tls_push(tls, f->label_count * sizeof(int));
    p.preds = tb_tls_push(tls, f->label_count * sizeof(TB_Label*));

    // entry label has no predecessors
    p.count[0] = 0;
    p.preds[0] = NULL;

    loop_range(j, 1, f->label_count) {
        p.preds[j] = (TB_Label*) tb_tls_push(tls, 0);
        tb_calculate_immediate_predeccessors(f, tls, j, &p.count[j]);
    }

    return p;
}

TB_API TB_Predeccesors tb_get_predeccesors(TB_Function* f) {
    TB_Predeccesors p = { 0 };
    p.count = tb_platform_heap_alloc(f->label_count * sizeof(int));
    p.preds = tb_platform_heap_alloc(f->label_count * sizeof(TB_Label*));

    // entry label has no predecessors
    p.count[0] = 0;
    p.preds[0] = NULL;

    loop_range(j, 1, f->label_count) {
        p.preds[j] = tb_calculate_immediate_predeccessors(f, NULL, j, &p.count[j]);
    }

    return p;
}

// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
TB_API size_t tb_get_dominators(TB_Function* f, TB_Predeccesors preds, TB_Label* doms) {
    if (doms == NULL) {
        return f->label_count;
    }

    DomContext ctx = {
        .doms = doms
    };

    // undef all dominator entries
    loop(i, f->label_count) {
        doms[i] = -1;
    }

    // entrypoint dominates itself
    doms[0] = 0;

    // identify post order traversal order
    {
        TB_TemporaryStorage* tls = tb_tls_steal();

        ctx.traversal_filled = 0;
        ctx.traversal = tb_tls_push(tls, f->label_count * sizeof(TB_Label));

        // we only need the visited array for this scope, it's just to avoid
        // recursing forever on post order traversal stuff
        ctx.visited = tb_tls_push(tls, f->label_count * sizeof(bool));
        memset(ctx.visited, 0, f->label_count * sizeof(bool));

        postorder(f, &ctx, 1);

        /*printf("Post order traversal: \n");
        loop(i, ctx.traversal_filled) {
            printf("L%d ", ctx.traversal[i]);
        }
        printf("\n");*/

        tb_tls_restore(tls, ctx.visited);
        ctx.visited = NULL;
    }

    bool changed = true;
    while (changed) {
        changed = false;

        // for all nodes, b, in reverse postorder (except start node)
        loop_reverse(i, ctx.traversal_filled - 1) {
            TB_Label b = ctx.traversal[i];
            TB_Label new_idom = preds.preds[b][0];

            // for all other predecessors, p, of b
            loop_range(j, 1, preds.count[b]) {
                TB_Label p = preds.preds[b][j];

                if (doms[p] != -1) { // i.e., if doms[p] already calculated
                    new_idom = ctx.traversal[intersect(
                            &ctx,
                            find_traversal_index(&ctx, p),
                            find_traversal_index(&ctx, new_idom)
                        )];
                }
            }

            if (doms[b] != new_idom) {
                doms[b] = new_idom;
                changed = true;
            }
        }
    }

    return f->label_count;
}

TB_API bool tb_is_dominated_by(TB_Label* doms, TB_Label expected_dom, TB_Label bb) {
    while (bb != 0 && expected_dom != bb) {
        bb = doms[bb];
    }

    return (expected_dom == bb);
}

TB_API TB_LoopInfo tb_get_loop_info(TB_Function* f, TB_Predeccesors preds, TB_Label* doms) {
    // Find loops
    DynArray(TB_Loop) loops = dyn_array_create(TB_Loop);
    loop(bb, f->label_count) {
        TB_Label backedge = 0;
        loop(j, preds.count[bb]) {
            if (tb_is_dominated_by(doms, bb, preds.preds[bb][j])) {
                backedge = preds.preds[bb][j];
                break;
            }
        }

        if (backedge) {
            printf("L%zu is a loop (backedge: L%d)\n    ", bb, backedge);

            TB_Loop l = { .parent_loop = -1, .header = bb, .backedge = backedge };
            l.body = malloc(f->label_count * sizeof(TB_Label));

            loop(j, f->label_count) {
                if (tb_is_dominated_by(doms, bb, j)) {
                    printf("L%zu ", j);

                    l.body[l.body_count++] = j;
                }
            }

            l.body = realloc(l.body, l.body_count * sizeof(TB_Label));

            // check if we have a parent...
            loop_reverse(o, dyn_array_length(loops)) {
                loop(j, loops[o].body_count) {
                    if (bb == loops[o].body[j]) {
                        l.parent_loop = o;
                        goto fatherfull_behavior;
                    }
                }
            }

            fatherfull_behavior:
            dyn_array_put(loops, l);

            printf("\n\n");
        }
    }

    return (TB_LoopInfo){ .count = dyn_array_length(loops), .loops = &loops[0] };
}

TB_API void tb_free_loop_info(TB_LoopInfo l) {
    dyn_array_destroy(l.loops);
}
