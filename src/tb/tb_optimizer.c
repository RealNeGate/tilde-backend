#include "tb_internal.h"
#include <stdarg.h>

#include <luajit-2.1/lua.h>
#include <luajit-2.1/lualib.h>
#include <luajit-2.1/lauxlib.h>
#include <luajit-2.1/luajit.h>

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

static FILE* debug_file;
static void print_diff(const char* description, char* oldstr, char* newstr) {
    char *old_ctx, *old_line = strtok_r(oldstr, "\n", &old_ctx);
    char *new_ctx, *new_line = strtok_r(newstr, "\n", &new_ctx);

    fprintf(debug_file, "  %s\n", description);
    for (; old_line != NULL || new_line != NULL;
        old_line = strtok_r(NULL, "\n", &old_ctx),
        new_line = strtok_r(NULL, "\n", &new_ctx)) {
        if (new_line == NULL) {
            fprintf(debug_file,
                GREEN_TEXT  "%-80s"
                RESET_TEXT  "|"
                RED_TEXT    "%-80s\n"
                RESET_TEXT, "", old_line);
        } else if (old_line == NULL) {
            fprintf(debug_file,
                GREEN_TEXT "%-80s"
                RESET_TEXT "|"
                RED_TEXT   "%-80s\n"
                RESET_TEXT, new_line, "");
        } else {
            // just compare them
            if (strcmp(old_line, new_line) == 0) {
                fprintf(debug_file, "%-80s|\n", new_line);
            } else {
                fprintf(debug_file,
                    GREEN_TEXT "%-80s"
                    RESET_TEXT "|"
                    RED_TEXT   "%-80s\n"
                    RESET_TEXT, new_line, old_line);
            }
        }
    }

    fprintf(debug_file, "\n\n\n");
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

TB_API bool tb_function_optimize(TB_Function* f, size_t pass_count, const TB_FunctionPass* passes) {
    if (debug_file == NULL) {
        debug_file = stdout;
    }

    if (pass_count == 0) {
        pass_count = sizeof(default_passes) / sizeof(default_passes[0]);
        passes = default_passes;
    }

    //TB_FunctionPass p = tb_opt_load_lua_pass("W:/Workspace/tilde-backend/src/tb/opt/usercode.lua");

    #if LOGGING_OPTS
    char* big_boy = tb_platform_heap_alloc(2*DIFF_BUFFER_SIZE);
    int in_use = 1;

    big_boy[0] = 0;
    tb_function_print(f, print_to_buffer, big_boy);
    printf("INITIAL\n%s\n\n\n", big_boy);
    #endif

    bool changes = false;
    for (size_t i = 0; i < pass_count; i++) {
        bool success = false;
        if (passes[i].l_state != NULL) {
            // Invokes the pass
            lua_State* L = passes[i].l_state;

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
            #if LOGGING_OPTS
            // double buffering amirite
            char* oldstr = &big_boy[in_use ? 0 : DIFF_BUFFER_SIZE];
            char* newstr = &big_boy[in_use ? DIFF_BUFFER_SIZE : 0];

            // Reset then write IR dump into newstr
            *newstr = 0;
            tb_function_print(f, print_to_buffer, newstr);

            char* tmp = strdup(newstr);
            print_diff(passes[i].name ? passes[i].name : "?", oldstr, tmp);
            free(tmp);

            in_use = (in_use + 1) & 1;
            #endif

            changes = true;
        }
    }

    #if LOGGING_OPTS
    tb_platform_heap_free(big_boy);
    #endif

    return changes;
}

TB_API bool tb_module_optimize(TB_Module* m) {
    return false;
}
