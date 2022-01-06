#include "tb/tb.h"
#include <time.h>

TB_Function* test_fib(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 1, false);
	tb_prototype_add_params(p, 1, (TB_DataType[]) { TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__);
	
	TB_Register n = tb_inst_param(func, 0);
	
	TB_Label if_true = tb_inst_new_label_id(func);
	TB_Label if_false = tb_inst_new_label_id(func);
	
	// if (n < 2) return n
	{
		tb_inst_if(func, tb_inst_cmp_ilt(func, n, tb_inst_sconst(func, TB_TYPE_I32, 2), true), if_true, if_false);
		
		tb_inst_label(func, if_true); // .L1:
		tb_inst_ret(func, n);
	}
	
	// else return fib(n + 1) + fib(n + 2)
	{
		tb_inst_label(func, if_false); // .L2:
		
		TB_Register n_minus_one = tb_inst_sub(func, n, tb_inst_sconst(func, TB_TYPE_I32, 1), TB_ASSUME_NUW);
		
		TB_Register call1 = tb_inst_call(func, TB_TYPE_I32, func, 1, (TB_Register[]) { n_minus_one });
		
		TB_Register n_minus_two = tb_inst_sub(func, n, tb_inst_sconst(func, TB_TYPE_I32, 2), TB_ASSUME_NUW);
		
		TB_Register call2 = tb_inst_call(func, TB_TYPE_I32, func, 1, (TB_Register[]) { n_minus_two });
		
		TB_Register sum = tb_inst_add(func, call1, call2, TB_ASSUME_NUW);
		tb_inst_ret(func, sum);
	}
	
	return func;
}

int main(int argc, char** argv) {
	clock_t t1 = clock();
	
	TB_FeatureSet features = { 0 };
	TB_Module* m = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
	
	TB_Function* fib_func = test_fib(m);
	tb_module_compile_func(m, fib_func);
	
	tb_module_compile(m);
	tb_module_export_jit(m);
	
	clock_t t2 = clock();
	printf("compile took %f ms\n", ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0);
	
	typedef int(*FibFunction)(int n);
	FibFunction compiled_fib_func = (FibFunction)tb_module_get_jit_func(m, fib_func);
	int t = compiled_fib_func(35);
	
	if (t != 9227465) {
		printf("Failure!\n");
		abort();
	}
	
	clock_t t3 = clock();
	printf("fib took %f ms\n", ((t3 - t2) / (double)CLOCKS_PER_SEC) * 1000.0);
	printf("Fib(%d) = %d\n", 35, t);
	
	tb_module_destroy(m);
	return 0;
}
