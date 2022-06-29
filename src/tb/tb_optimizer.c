#include "tb_internal.h"
#include <stdarg.h>

#ifdef _WIN32
#define strdup(s) _strdup(s)
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#define DIFF_BUFFER_SIZE 131072

#define OPT(x) { #x, tb_opt_ ## x }
static TB_FunctionPass opts[] = {
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
enum { OPTIMIZATION_COUNT = TB_ARRLEN(opts) };
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
    //__debugbreak();
}

TB_API bool tb_function_optimize(TB_Function* f) {
    if (debug_file == NULL) {
        //debug_file = fopen("foo.txt", "wb");
        //setvbuf(debug_file, NULL, _IONBF, 0);
        //assert(debug_file != NULL);
        debug_file = stdout;
    }


    #if LOGGING_OPTS
    char* big_boy = tb_platform_heap_alloc(2*DIFF_BUFFER_SIZE);
    int in_use = 1;

    big_boy[0] = 0;
    tb_function_print(f, print_to_buffer, big_boy);
    printf("INITIAL\n%s\n\n\n", big_boy);
    #endif

    bool changes = false;
    for (int i = 0; i < OPTIMIZATION_COUNT; i++) {
        bool success = opts[i].execute(f);
        if (success) {
            #if LOGGING_OPTS
            // double buffering amirite
            char* oldstr = &big_boy[in_use ? 0 : DIFF_BUFFER_SIZE];
            char* newstr = &big_boy[in_use ? DIFF_BUFFER_SIZE : 0];

            // Reset then write IR dump into newstr
            *newstr = 0;
            tb_function_print(f, print_to_buffer, newstr);

            char* tmp = strdup(newstr);
            print_diff(opts[i].name, oldstr, tmp);
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
