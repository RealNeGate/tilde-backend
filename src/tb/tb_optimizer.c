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
enum { OPTIMIZATION_COUNT = TB_ARRLEN(default_passes) };
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
                c[i + (j * stride)] = 1 + c[(i-1) + ((j-1) * stride)];
            } else {
                int a = c[(i-1) + (j * stride)];
                int b = c[i + ((j-1) * stride)];

                c[i + (j * stride)] = a > b ? a : b;
            }
        }
    }

    return c;
}

static void print_diff2(int* c, size_t cstride, char** x, char** y, int i, int j) {
    // 0 for x, 1 for y
    // negative for removal
    bool remove = false;
    bool on_y = false;

    if (i < 0 && j < 0) {
        // do nothing?
        return;
    } else if (i < 0) {
        print_diff2(c, cstride, x, y, i, j - 1);
        on_y = true, remove = true;
    } else if (j < 0) {
        print_diff2(c, cstride, x, y, i - 1, j);
        on_y = false, remove = true;
    } else if (strcmp(x[i], y[j]) == 0) {
        print_diff2(c, cstride, x, y, i - 1, j - 1);

        printf(RESET_TEXT "  %-80s|  %-80s\n", x[i], y[j]);
        return;
    } else if (c[i + ((j-1) * cstride)] >= c[(i-1) + (j * cstride)]) {
        print_diff2(c, cstride, x, y, i, j - 1);
        on_y = true, remove = false;
    } else {
        print_diff2(c, cstride, x, y, i - 1, j);
        on_y = false, remove = true;
    }

    const char* prefix1 = remove ? RED_TEXT"- " : GREEN_TEXT"+ ";
    const char* prefix2 = "  ";

    if (on_y) {
        tb_swap(const char*, prefix1, prefix2);
    }

    bool a = (!on_y && i >= 0);
    bool b = (on_y && j >= 0);

    // left side
    printf("%03d %s%-80s|", i, prefix1, a ? x[i] : "");

    // right side
    printf("%03d %s%-80s|", j, prefix2, b ? y[j] : "");

    // next line
    printf("\n");
}

static void diff(size_t xl, char** x, size_t yl, char** y) {
    int* c = lcslen(xl, x, yl, y);
    print_diff2(c, xl + 1, x, y, xl - 1, yl - 1);
}

static FILE* debug_file;
static void print_diff(const char* description, const char* oldstr, const char* newstr) {
    fprintf(debug_file, "  %s\n", description);

    DynArray(char*) old_lines = split_lines(oldstr);
    DynArray(char*) new_lines = split_lines(newstr);
    diff(dyn_array_length(old_lines), old_lines, dyn_array_length(new_lines), new_lines);

    fprintf(debug_file, RESET_TEXT "\n\n\n");
}

TB_API bool tb_function_optimize(TB_Function* f, size_t pass_count, const TB_FunctionPass* passes) {
    if (debug_file == NULL) {
        debug_file = stdout;
    }

    if (pass_count == 0) {
        pass_count = sizeof(default_passes) / sizeof(default_passes[0]);
        passes = default_passes;
    }

    bool diff_opts = false;
    char* big_boy = NULL;
    int in_use = 1;
    if (diff_opts) {
        big_boy = tb_platform_heap_alloc(2*DIFF_BUFFER_SIZE);

        big_boy[0] = 0;
        tb_function_print(f, print_to_buffer, big_boy);
        printf("INITIAL\n%s\n\n\n", big_boy);
    }

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
            if (diff_opts) {
                // double buffering amirite
                char* oldstr = &big_boy[in_use ? 0 : DIFF_BUFFER_SIZE];
                char* newstr = &big_boy[in_use ? DIFF_BUFFER_SIZE : 0];

                // Reset then write IR dump into newstr
                *newstr = 0;
                tb_function_print(f, print_to_buffer, newstr);

                print_diff(passes[i].name ? passes[i].name : passes[i].l_state ? "lua unknown" : "C unknown", oldstr, newstr);
                in_use = (in_use + 1) & 1;
            }

            changes = true;
        }
    }

    changes |= tb_opt_remove_pass_node(f);

    if (diff_opts) {
        tb_platform_heap_free(big_boy);
    }

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
