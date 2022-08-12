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

#define OPT(x) { #x, tb_opt_ ## x }
static const TB_FunctionPass default_passes[] = {
    // canonicalization
    OPT(hoist_locals),
    OPT(merge_rets),
    OPT(hoist_invariants),
    OPT(canonicalize),

    // real optimizations
    OPT(mem2reg),
    OPT(remove_pass_node),
    OPT(canonicalize),

    /*
    OPT(load_elim),
    OPT(copy_elision),
    OPT(deshort_circuit),
    */

    OPT(dead_expr_elim),
    OPT(dead_block_elim),

    OPT(refinement),

    OPT(compact_dead_regs),
};
enum { OPTIMIZATION_COUNT = COUNTOF(default_passes) };
#undef OPT

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

TB_API bool tb_function_optimize(TB_Function* f, size_t pass_count, const TB_FunctionPass* passes) {
    /* if (debug_file == NULL) {
        #if 0
        debug_file = stdout;
        #else
        debug_file = fopen("foo.html", "wb"); // stdout;
        assert(debug_file);

        fprintf(debug_file, "<html>\n");
        fprintf(debug_file, "<style>\n");
        fprintf(debug_file, "table, th {\n");
        fprintf(debug_file, "border:1px solid black;\n");
        fprintf(debug_file, "}\n");
        fprintf(debug_file, "td {\n");
        fprintf(debug_file, "border:1px solid black;\n");
        fprintf(debug_file, "padding: 0.5em;\n");
        fprintf(debug_file, "}\n");
        fprintf(debug_file, "</style>\n");
        fprintf(debug_file, "<body>\n");
        fprintf(debug_file, "<table>\n");

        atexit(end_crap);
        #endif
    } */

    if (pass_count == 0) {
        pass_count = sizeof(default_passes) / sizeof(default_passes[0]);
        passes = default_passes;
    }

    /* bool diff_opts = false; // !strcmp(f->name, "StringsAreEqual");
    if (diff_opts) {
        log_function(debug_file, "initial", f);
    } */

    bool changes = false;
    for (size_t i = 0; i < pass_count; i++) {
        bool success = false;

        if (passes[i].l_state != NULL) {
            // Invokes the pass
            lua_State* L = lua_newthread(passes[i].l_state);

            lua_getglobal(L, "DA_FUNC");
            lua_pushlightuserdata(L, f);
            int ret = lua_pcall(L, 1, 0, 0);
            if (ret > 1) {
                const char* str = lua_tostring(L, -1);
                tb_panic("Lua runtime exited with %d\n%s\n", ret, str);
            } else if (ret == 1) {
                success = true;
            }
        } else {
            success = passes[i].execute(f);
        }

        if (success) {
            /* if (diff_opts) {
                log_function(debug_file, passes[i].name ? passes[i].name : passes[i].l_state ? "lua unknown" : "C unknown", f);
            } */

            changes = true;
        }
    }

    changes |= tb_opt_remove_pass_node(f);
    return changes;
}

TB_API bool tb_module_optimize(TB_Module* m) {
    return false;
}

TB_API TB_FunctionPass tb_opt_load_lua_pass(const char* path) {
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
    return (TB_FunctionPass){ .name = NULL, .l_state = L };
}

TB_API void tb_opt_unload_lua_pass(TB_FunctionPass* p) {
    lua_close((lua_State*) p->l_state);
}
