#include "tb_internal.h"

#ifdef _WIN32
#define strdup(s) _strdup(s)
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#if LOGGING_OPTS
#define OPT(x) \
if (tb_opt_##x(f)) { \
char* oldstr = &big_boy[in_use ? 0 : 65536]; \
char* newstr = &big_boy[in_use ? 65536 : 0]; \
*newstr = 0; \
tb_function_print(f, print_to_buffer, newstr); \
char* tmp = strdup(newstr); \
print_diff(#x, oldstr, tmp); \
free(tmp); \
in_use = (in_use + 1) & 1; \
changes = true; \
goto repeat_opt; \
}
#else
#define OPT(x) \
if (tb_opt_##x(f)) { \
changes = true; \
goto repeat_opt; \
}
#endif

static void print_to_buffer(void* user_data, const char* fmt, ...) {
	char* buffer = (char*) user_data;
	size_t len = strlen(buffer);

	va_list ap;
	va_start(ap, fmt);
	int result = vsnprintf(buffer + len, 65536 - len, fmt, ap);
	va_end(ap);

	if (result < 0 || result >= (65536 - len)) {
		tb_panic("Ran out of space in my internal buffer");
	}
}

static void print_diff(const char* description, char* oldstr, char* newstr) {
	char *old_ctx, *old_line = strtok_r(oldstr, "\n", &old_ctx);
	char *new_ctx, *new_line = strtok_r(newstr, "\n", &new_ctx);

	printf("  %s\n", description);
	for (; old_line != NULL || new_line != NULL;
		 old_line = strtok_r(NULL, "\n", &old_ctx),
		 new_line = strtok_r(NULL, "\n", &new_ctx)) {
		if (new_line == NULL) {
			printf("\x1b[32m" "%-40s"
				   "\x1b[0m"  "|"
				   "\x1b[31m" "%-40s\n"
				   "\x1b[0m", "", old_line);
		} else if (old_line == NULL) {
			printf("\x1b[32m" "%-40s"
				   "\x1b[0m"  "|"
				   "\x1b[31m" "%-40s\n"
				   "\x1b[0m", new_line, "");
		} else {
			// just compare them
			if (strcmp(old_line, new_line) == 0) {
				printf("%-40s|\n", new_line);
			} else {
				printf("\x1b[32m" "%-40s"
					   "\x1b[0m"  "|"
					   "\x1b[31m" "%-40s\n"
					   "\x1b[0m", new_line, old_line);
			}
		}
	}

	printf("\n\n\n");
	//__debugbreak();
}

TB_API bool tb_function_optimize(TB_Function* f) {
	bool changes = false;

	// only needs to run once
	changes |= tb_opt_hoist_locals(f);
	changes |= tb_opt_merge_rets(f);

#if LOGGING_OPTS
	char* big_boy = malloc(2*65536);
	int in_use = 1;

	big_boy[0] = 0;
	tb_function_print(f, print_to_buffer, big_boy);
	printf("INITIAL\n%s\n\n\n", big_boy);
#endif

	repeat_opt: {
		// Passive optimizations
		OPT(dead_expr_elim);
		OPT(remove_pass_node);
		OPT(subexpr_elim);
		OPT(hoist_invariants);
		OPT(fold);
		OPT(load_elim);
		OPT(strength_reduction);
		OPT(copy_elision);
		OPT(canonicalize);
		OPT(mem2reg);
		OPT(dead_block_elim);
		OPT(deshort_circuit);
		OPT(compact_dead_regs);
	}

#if LOGGING_OPTS
	free(big_boy);
#endif

	return changes;
}

TB_API bool tb_module_optimize(TB_Module* m) {
	return false;
}
#undef OPT
