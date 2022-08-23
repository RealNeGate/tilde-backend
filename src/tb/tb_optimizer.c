#include "tb_internal.h"
#include <stdarg.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <luajit.h>

#include "opt/lua_glue.h"
#include "opt/lua_prelude.inc"

#ifdef _WIN32
#define strdup(s) _strdup(s)
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#define DIFF_BUFFER_SIZE 131072

static void print_to_buffer(void* user_data, const char* fmt, ...) {
    char* buffer = (char*) user_data;
    size_t len = strlen(buffer);

    va_list ap;
    va_start(ap, fmt);
    int result = vsnprintf(buffer + len, DIFF_BUFFER_SIZE - len, fmt, ap);
    va_end(ap);

    if (result < 0 || result >= (DIFF_BUFFER_SIZE - len)) {
        tb_panic("Ran out of space in my internal buffer");
    }
}

#if 1
#define GREEN_TEXT "\x1b[32m"
#define RED_TEXT   "\x1b[31m"
#define RESET_TEXT "\x1b[0m"
#else
#define GREEN_TEXT ""
#define RED_TEXT   ""
#define RESET_TEXT ""
#endif

#if 0
static DynArray(char*) split_lines(const char* src) {
    DynArray(char*) lines = dyn_array_create(char*);
    char* clone = strdup(src);

    // iterate and split the clone's lines
    char* ctx;
    char* line = strtok_r(clone, "\n", &ctx);
    while (line != NULL) {
        dyn_array_put(lines, line);
        line = strtok_r(NULL, "\n", &ctx);
    }

    return lines;
}

// https://github.com/alexdzyoba/diff
static int* lcslen(size_t xl, char** x, size_t yl, char** y) {
    size_t stride = xl*1;
    int* c = calloc((xl+1) * (yl+1), sizeof(int));

    loop(i, xl) {
        loop(j, yl) {
            if (strcmp(x[i], y[j]) == 0) {
                c[(i+1) + ((j+1) * stride)] = 1 + c[i + (j * stride)];
            } else {
                int a = c[i + ((j+1) * stride)];
                int b = c[(i+1) + (j * stride)];

                c[(i+1) + ((j+1) * stride)] = a > b ? a : b;
            }
        }
    }

    return c;
}

static void print_diff2(int* c, size_t cstride, char** x, char** y, int i, int j) {
    if (i < 0 && j < 0) {
        // do nothing?
        return;
    } else if (i < 0) {
        print_diff2(c, cstride, x, y, i, j - 1);
        printf(GREEN_TEXT"+ %s\n"RESET_TEXT, y[j]);
    } else if (j < 0) {
        print_diff2(c, cstride, x, y, i - 1, j);
        printf(RED_TEXT"- %s\n"RESET_TEXT, x[i]);
    } else if (strcmp(x[i], y[j]) == 0) {
        print_diff2(c, cstride, x, y, i - 1, j - 1);
        printf("  %s\n", x[i]);
    } else if (c[(i+1) + (j * cstride)] >= c[i + ((j+1) * cstride)]) {
        print_diff2(c, cstride, x, y, i, j - 1);
        printf(GREEN_TEXT"+ %s\n"RESET_TEXT, y[j]);
    } else {
        print_diff2(c, cstride, x, y, i - 1, j);
        printf(RED_TEXT"- %s\n"RESET_TEXT, x[i]);
    }
}

static void diff(size_t xl, char** x, size_t yl, char** y) {
    int* c = lcslen(xl, x, yl, y);
    print_diff2(c, xl + 1, x, y, xl - 1, yl - 1);
}

static void print_diff(const char* description, const char* oldstr, const char* newstr) {
    fprintf(debug_file, "  %s\n", description);

    DynArray(char*) old_lines = split_lines(oldstr);
    DynArray(char*) new_lines = split_lines(newstr);
    diff(dyn_array_length(old_lines), old_lines, dyn_array_length(new_lines), new_lines);

    fprintf(debug_file, RESET_TEXT "\n\n\n");

    dyn_array_destroy(old_lines);
    dyn_array_destroy(new_lines);
}
#endif

static void html_print(void* user_data, const char* fmt, ...) {
    char tmp[1024];

    va_list ap;
    va_start(ap, fmt);
    int result = vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);

    if (result < 0 || result >= sizeof(tmp)) {
        tb_panic("Ran out of space in my internal buffer");
    }

    // print but replace the escape chars
    FILE* f = (FILE*) user_data;

    const char* start = tmp;
    const char* s = start;
    for (; *s; s++) {
        if (*s == '<' || *s == '>' || *s == '"' || *s == '\'' || *s == '&') {
            fprintf(f, "%.*s", (int)(s - start), start);

            switch (*s) {
                case '<': fprintf(f, "&lt;"); break;
                case '>': fprintf(f, "&gt;"); break;
                case '"': fprintf(f, "&quot;"); break;
                case '\'': fprintf(f, "&#39;"); break;
                case '&': fprintf(f, "&amp;"); break;
                default: tb_todo();
            }

            start = s + 1;
        }
    }

    if (start != s) {
        fprintf(f, "%.*s", (int)(s - start), start);
    }
}

static void log_function(FILE* out, const char* title, TB_Function* f) {
    #if 0
    tb_function_print2(f, tb_default_print_callback, out, false);
    #else
    fprintf(out, "<td valign=\"top\">\n");
    fprintf(out, "%s:<br>\n", title);
    fprintf(out, "<pre>\n");
    tb_function_print2(f, html_print, out, false);
    fprintf(out, "</pre>\n");
    fprintf(out, "</td>\n");
    #endif
}

static FILE* debug_file;
static void end_crap() {
    fprintf(debug_file, "</table>\n");
    fprintf(debug_file, "</body>\n");
    fprintf(debug_file, "</html>\n");
    fclose(debug_file);
}

static lua_State* begin_lua_pass(void* l_state) {
    lua_State* L = lua_newthread(l_state);
    lua_getglobal(L, "DA_FUNC");
    return L;
}

static bool end_lua_pass(lua_State* L, int arg_count) {
    int ret = lua_pcall(L, arg_count, 0, 0);
    if (ret > 1) {
        const char* str = lua_tostring(L, -1);
        tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
    } else if (ret == 1) {
        return true;
    } else {
        return false;
    }
}

static bool schedule_function_level_opts(TB_Module* m, size_t pass_count, const TB_Pass passes[]) {
    TB_Function* functions = m->functions.data;
    bool changes = false;

    FOREACH_N(i, 0, m->functions.count) {
        TB_Function* f = &functions[i];

        FOREACH_N(j, 0, pass_count) {
            switch (passes[j].mode) {
                case TB_BASIC_BLOCK_PASS:
                TB_Reg bb = 1;
                while (bb != 0) {
                    TB_Node* start = &f->nodes[bb];
                    assert(start->type == TB_LABEL);

                    if (passes[j].l_state != NULL) {
                        lua_State* L = begin_lua_pass(passes[j].l_state);
                        lua_pushlightuserdata(L, f);
                        lua_pushinteger(L, bb);
                        changes |= end_lua_pass(L, 2);
                    } else {
                        changes |= passes[j].bb_run(f, bb);
                    }

                    TB_Reg terminator = start->label.terminator;
                    bb = start->type == TB_LABEL ? terminator : f->nodes[terminator].next;
                }
                break;

                case TB_LOOP_PASS:
                // We probably want a function to get all this info together
                TB_TemporaryStorage* tls = tb_tls_allocate();
                TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);

                TB_Label* doms = tb_tls_push(tls, f->label_count * sizeof(TB_Label));
                tb_get_dominators(f, preds, doms);

                // probably don't wanna do this using heap allocations
                TB_LoopInfo loops = tb_get_loop_info(f, preds, doms);

                FOREACH_N(k, 0, loops.count) {
                    const TB_Loop* l = &loops.loops[i];

                    if (passes[j].l_state != NULL) {
                        lua_State* L = begin_lua_pass(passes[j].l_state);
                        lua_pushlightuserdata(L, f);
                        lua_pushlightuserdata(L, (void*) l);
                        changes |= end_lua_pass(L, 2);
                    } else {
                        changes |= passes[j].loop_run(f, l);
                    }
                }

                tb_free_loop_info(loops);
                break;

                case TB_FUNCTION_PASS:
                if (passes[j].l_state != NULL) {
                    lua_State* L = begin_lua_pass(passes[j].l_state);
                    lua_pushlightuserdata(L, f);
                    changes |= end_lua_pass(L, 1);
                } else {
                    changes |= passes[j].func_run(f);
                }
                break;

                default: tb_unreachable();
            }
        }
    }

    return changes;
}

static bool schedule_module_level_opt(TB_Module* m, const TB_Pass* pass) {
    // this is the only module level mode we have rn
    if (pass->mode != TB_MODULE_PASS) {
        tb_unreachable();
    }

    if (pass->l_state != NULL) {
        lua_State* L = begin_lua_pass(pass->l_state);
        lua_pushlightuserdata(L, m);
        return end_lua_pass(L, 1);
    } else {
        return pass->mod_run(m);
    }
}

TB_API bool tb_module_optimize(TB_Module* m, size_t pass_count, const TB_Pass passes[]) {
    bool changes = false;

    size_t i = 0;
    while (i < pass_count) {
        // anything below or equal to function-level passes can be trivially
        // parallel and thus we handle this as a
        size_t sync = i;
        for (; sync < pass_count; sync++) {
            if (passes[sync].mode > TB_FUNCTION_PASS) break;
        }

        // TODO(NeGate): convert this into a trivial parallel dispatch
        if (sync != i) {
            changes |= schedule_function_level_opts(m, sync - i, &passes[i]);
            i = sync;
        }

        // synchronize and handle the module level stuff
        // TODO(NeGate): this requires special scheduling to be threaded
        // but it's completely possible
        for (; i < pass_count; i++) {
            if (passes[i].mode <= TB_FUNCTION_PASS) break;

            // run module level
            changes |= schedule_module_level_opt(m, &passes[i]);
        }
    }

    return changes;
}

TB_API TB_Pass tb_opt_load_lua_pass(const char* path, enum TB_PassMode mode) {
    lua_State *L = luaL_newstate();
    if (L == NULL) abort();

    // Load prelude into block
    luaL_openlibs(L);
    int status = luaL_loadstring(L, PRELUDE);
    if (status != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("could not load lua script: %d\n%s\n", status, str);
    }

    // pcall will call that block as a function
    int ret = lua_pcall(L, 0, 0, 0);
    if (ret != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
    }

    // Load passes
    status = luaL_loadfile(L, path);
    if (status != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("could not load lua script: %d\n%s\n", status, str);
    }

    // Returns the function for the pass
    ret = lua_pcall(L, 0, 1, 0);
    if (ret != 0) {
        const char* str = lua_tostring(L, -1);
        tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
    }

    lua_setglobal(L, "DA_FUNC");
    return (TB_Pass){ .mode = mode, .name = NULL, .l_state = L };
}

TB_API void tb_opt_unload_lua_pass(TB_Pass* p) {
    lua_close((lua_State*) p->l_state);
}
