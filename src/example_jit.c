#include "tb/tb.h"

static TB_Function* test_func_ref = NULL;
TB_Function* test_fib(TB_Module* m);

static size_t my_fib(size_t n) {
	size_t a = 0, b = 1;
	for (size_t i = 0; i < n - 1; i++) {
		size_t c = a + b;
		a = b;
		b = c;
	}
	
	return b;
}

int main(int argc, char** argv) {
	clock_t t1 = clock();
	
	TB_FeatureSet features = { 0 };
	TB_Module* m = tb_module_create(TB_ARCH_X86_64,
									TB_SYSTEM_WINDOWS,
									&features, TB_OPT_O0, 1);
	
	tb_module_compile_func(m, test_fib(m));
	
	tb_module_compile(m);
	tb_module_export_jit(m);
	
	clock_t t2 = clock();
	printf("compile took %f ms\n", ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0);
	
	typedef int(*FibFunction)(int n);
	FibFunction jitted_func = (FibFunction)tb_module_get_jit_func(m, test_func_ref);
	int t = jitted_func(35);
	
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

TB_Function* test_fib(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	test_func_ref = func;
	
	TB_Register n = tb_inst_param(func, TB_TYPE_I32(1));
	
	tb_inst_if(func, tb_inst_cmp_slt(func,
									 TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2)), 1, 2);
	tb_inst_label(func, 1); // .L1:
	tb_inst_ret(func, TB_TYPE_I32(1), n);
	
	tb_inst_label(func, 2); // .L2:
	
	TB_Register n_minus_one = tb_inst_sub(func,
										  TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 1), TB_NO_WRAP);
	TB_Register call1 = tb_inst_call(func,
									 TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_one });
	
	TB_Register n_minus_two = tb_inst_sub(func,
										  TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2), TB_NO_WRAP);
	
	TB_Register call2 = tb_inst_call(func,
									 TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_two });
	
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), call1, call2, TB_NO_WRAP);
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	
	return func;
}
