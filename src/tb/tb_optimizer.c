#include "tb_internal.h"

#ifdef _WIN32
#define strdup(s) _strdup(s)
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#define OPT(x) { #x, tb_opt_ ## x, false }
#define OPT_RUN_ONCE(x) { #x, tb_opt_ ## x, true }
static TB_FunctionPass opts[] = {
	OPT_RUN_ONCE(hoist_locals),
	OPT_RUN_ONCE(merge_rets),

	OPT(dead_expr_elim),
	OPT(remove_pass_node),
	OPT(subexpr_elim),
	OPT(hoist_invariants),
	OPT(fold),
	OPT(load_elim),
	OPT(strength_reduction),
	OPT(copy_elision),
	OPT(canonicalize),
	OPT(mem2reg),
	OPT(dead_block_elim),
	OPT(deshort_circuit),
	OPT(compact_dead_regs)
};
enum {
	OPTIMIZATION_COUNT = TB_ARRLEN(opts)
};
#undef OPT

#if LOGGING_OPTS
#define OPT(x) \
if (tb_opt_##x(f)) { \
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
	for (int i = 0; i < OPTIMIZATION_COUNT; i++) {
		if (opts[i].run_once) {
			changes |= opts[i].execute(f);
		}
	}

#if LOGGING_OPTS
	char* big_boy = malloc(2*65536);
	int in_use = 1;

	big_boy[0] = 0;
	tb_function_print(f, print_to_buffer, big_boy);
	printf("INITIAL\n%s\n\n\n", big_boy);
#endif

	int current = 0;
	while (current < OPTIMIZATION_COUNT) {
		if (!opts[current].run_once && opts[current].execute(f)) {
#if LOGGING_OPTS
			// double buffering amirite
			char* oldstr = &big_boy[in_use ? 0 : 65536];
			char* newstr = &big_boy[in_use ? 65536 : 0];

			// Reset then write IR dump into newstr
			*newstr = 0;
			tb_function_print(f, print_to_buffer, newstr);

			char* tmp = strdup(newstr);
			print_diff(opts[current].name, oldstr, tmp);
			free(tmp);

			in_use = (in_use + 1) & 1;
#endif

			changes = true;
			current = 0;
			continue;
		}

		current += 1;
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
