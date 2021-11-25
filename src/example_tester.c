#include "tb/tb.h"
#include <time.h>

void jit_import_print_num(int x) {
    printf("OUT %d\n", x);
}

//
// Generators
//
TB_Function* gen_fib(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register n = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Label if_true = tb_inst_new_label_id(func);
	TB_Label if_false = tb_inst_new_label_id(func);
	
	tb_inst_if(func, tb_inst_cmp_slt(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2)), if_true, if_false);
	
	tb_inst_label(func, if_true); // .L1:
	tb_inst_ret(func, TB_TYPE_I32(1), n);
	
	tb_inst_label(func, if_false); // .L2:
	
	TB_Register n_minus_one = tb_inst_sub(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 1), TB_ASSUME_NUW);
	
	TB_Register call1 = tb_inst_call(func, TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_one });
	
	TB_Register n_minus_two = tb_inst_sub(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2), TB_ASSUME_NUW);
	
	TB_Register call2 = tb_inst_call(func, TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_two });
	
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), call1, call2, TB_ASSUME_NUW);
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	
	return func;
}

TB_Function* gen_fact(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_F32(1));
	
	TB_Register n = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register n_addr = tb_inst_param_addr(func, n);
	TB_Register f_addr = tb_inst_local(func, 4, 4);
	tb_inst_store(func, TB_TYPE_I32(1), f_addr, tb_inst_iconst(func, TB_TYPE_I32(1), 1), 4);
	
	TB_Label loop_entry = tb_inst_new_label_id(func);
	TB_Label loop_body = tb_inst_new_label_id(func);
	TB_Label loop_exit = tb_inst_new_label_id(func);
	
	// Loop entry
	{
		tb_inst_label(func, loop_entry);
		TB_Register n_ld = tb_inst_load(func, TB_TYPE_I32(1), n_addr, 4);
		tb_inst_if(func, tb_inst_cmp_ne(func, TB_TYPE_I32(1), n_ld, tb_inst_iconst(func, TB_TYPE_I32(1), 0)), loop_body, loop_exit);
	}
	
	// Loop body
	{
		tb_inst_label(func, loop_body);
		
		// f = f * n
		TB_Register f_ld = tb_inst_load(func, TB_TYPE_I32(1), f_addr, 4);
		TB_Register n_ld = tb_inst_load(func, TB_TYPE_I32(1), n_addr, 4);
		tb_inst_store(func, TB_TYPE_I32(1), f_addr, tb_inst_mul(func, TB_TYPE_I32(1), f_ld, n_ld, TB_ASSUME_NUW), 4);
		
		// n = n - 1
		TB_Register n_ld2 = tb_inst_load(func, TB_TYPE_I32(1), n_addr, 4);
		tb_inst_store(func, TB_TYPE_I32(1), n_addr, tb_inst_sub(func, TB_TYPE_I32(1), n_ld2, tb_inst_iconst(func, TB_TYPE_I32(1), 1), TB_ASSUME_NUW), 4);
		
		tb_inst_goto(func, loop_entry);
	}
	
	tb_inst_label(func, loop_exit);
	tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), f_addr, 4));
	return func;
}

TB_Function* gen_print(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_F32(1));
	
    TB_Register print = tb_inst_param(func, TB_TYPE_PTR());
	TB_Register x = tb_inst_iconst(func, TB_TYPE_I32(1), 64);
    
    tb_inst_vcall(func, TB_TYPE_VOID(), print, 1, (TB_Register[]) { x });
    tb_inst_ret(func, TB_TYPE_VOID(), TB_NULL_REG);
	return func;
}

//
// Testers
//
static bool test_fib(TB_Module* m, void* func) {
	typedef int F(int n);
	F* f = (F*)func;
	
	const static int results[] = {
		0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765
	};
	
	for (int x = 0; x < 20; x++) {
		printf("    fib(%d)...", x);
		int y = f(x);
		printf("\t%d\n", y);
		
		if (y != results[x]) {
			printf("    > expected %d\n", results[x]);
			return false;
		}
	}
	
	printf("    fib(35)...");
	int y = f(35);
	printf("\t%d\n", y);
	
	if (y != 9227465) {
		printf("    > expected 9227465\n");
		return false;
	}
	
	return true;
}

static bool test_fact(TB_Module* m, void* func) {
	typedef int F(int n);
	F* f = (F*)func;
	
	const static int results[10] = {
		1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800
	};
	
	for (int x = 1; x <= 10; x++) {
		printf("    fact(%d)...", x);
		int y = f(x);
		printf("\t%d\n", y);
		
		if (y != results[x-1]) {
			printf("    > expected %d\n", results[x-1]);
			return false;
		}
	}
	
	return true;
}

static bool test_print(TB_Module* m, void* func) {
	typedef void Print(int n);
    typedef int F(Print* p);
	F* f = (F*)func;
    
    f(jit_import_print_num);
	f(jit_import_print_num);
    
	return true;
}

typedef TB_Function* GeneratorFunction(TB_Module* m);
typedef bool TestFunction(TB_Module* m, void* func);

typedef struct Test {
	const char* name;
	GeneratorFunction* generate;
	TestFunction* test;
} Test;

static const Test tests[] = {
	{ "Fibonacci", gen_fib, test_fib },
	{ "Factorial", gen_fact, test_fact },
	{ "Print", gen_print, test_print }
};

enum {
	NUM_TESTS = sizeof(tests) / sizeof(tests[0])
};

int main(int argc, char** argv) {
	do {
		clock_t t1 = clock();
		TB_FeatureSet features = { 0 };
		TB_Module* m = tb_module_create(TB_ARCH_X86_64,
										TB_SYSTEM_WINDOWS,
										&features, TB_OPT_O0, 1,
										false);
		
		for (size_t i = 0; i < NUM_TESTS; i++) {
			TB_Function* f = tests[i].generate(m);
			
			tb_function_print(f, stdout);
			printf("\n\n\n");
			
			tb_module_compile_func(m, f);
		}
		
		tb_module_compile(m);
		tb_module_export_jit(m);
		
		clock_t t2 = clock();
		printf("compile took %f ms\n", ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0);
		
		int total_successes = 0;
		for (size_t i = 0; i < NUM_TESTS; i++) {
			printf("%s...\n", tests[i].name);
			
			void* func_ptr = tb_module_get_jit_func_by_id(m, i);
			bool success = tests[i].test(m, func_ptr);
			total_successes += success;
			
			if (success) printf("    DONE!\n");
			else printf("    FAILED!\n");
		}
		
		clock_t t3 = clock();
		printf("tests took %f ms\n", ((t3 - t2) / (double)CLOCKS_PER_SEC) * 1000.0);
		printf("results: %d / %d succeeded\n", total_successes, NUM_TESTS);
		tb_module_destroy(m);
	} while (true);
	
	return 0;
}
