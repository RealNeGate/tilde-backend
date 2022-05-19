#include "../src/tb/tb.h"

typedef const char* (*MatcherFunc)(const char* str);

static const char* demo(const char* str) {
	while (*str && *str != ' ') str++;
	return str;
}

static TB_Label handle_group(TB_Function* func, const char* pattern, TB_Label failure_label) {
	TB_Label entry = tb_inst_new_label_id(func);
	tb_inst_label(func, entry);
	
	return entry;
}

static TB_Label handle_set(TB_Function* func, const char* pattern, TB_Label failure_label) {
	TB_Label entry = tb_inst_new_label_id(func);
	tb_inst_label(func, entry);
	
	do {
		int min = *pattern++;
		
		if (*pattern == '-') {
			fprintf(stderr, "error: pattern is shit: %s\n", pattern);
			exit(1);
		}
		
		int max = *pattern++;
	} while (0);
	
	return entry;
}

static TB_Label handle_toplevel(TB_Function* func, const char* pattern) {
	if (*pattern == '[') {
		return handle_set(func, pattern+1, failure_label);
	} else {
		fprintf(stderr, "error: pattern is shit: %s\n", pattern);
		exit(1);
	}
}

static TB_Function* generate_jit_func(TB_Module* mod, const char* pattern) {
	TB_FunctionPrototype* proto = tb_prototype_create(mod, TB_STDCALL, TB_TYPE_PTR, 1, false);
	tb_prototype_add_param(proto, TB_TYPE_PTR);
	TB_Function* func = tb_prototype_build(mod, proto, "regex", TB_LINKAGE_PUBLIC);
	
	TB_Label failure_label = tb_inst_new_label_id(func);
	handle_toplevel(func, pattern);
	
	// failure
	tb_inst_label(func, failure_label);
	tb_inst_ret(func, tb_inst_ptr(func, 0));
	
	tb_module_compile_func(mod, func, TB_ISEL_FAST);
	return func;
}

int main(int argc, char* argv[]) {
	if (argc < 3) {
		fprintf(stderr, "error: expected regex pattern and input string\n");
		return 1;
	}
	
	printf("Pattern: %s\n", argv[1]);
	
	TB_FeatureSet features = { 0 };
	TB_Module* mod = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, TB_DEBUGFMT_NONE, &features);

	// create & compile main function
	TB_Function* func = generate_jit_func(mod, argv[1]);
	tb_module_export_jit(mod, TB_ISEL_FAST);
	
	const char* str   = argv[2];
	MatcherFunc match = demo;//(MatcherFunc)tb_module_get_jit_func(mod, func);
	
	while (*str) {
		const char* end = match(str);
		if (end == NULL) break;
		
		printf("'%.*s'\n", (int)(end-str), str);
		str = end+1;
	}
	
	tb_module_destroy(mod);
	return 0;
}
