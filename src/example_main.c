#include "tb/tb.h"

void do_tests(FILE* f, TB_Arch arch, TB_System system, const TB_FeatureSet* features);
TB_Function* test_add_i8(TB_Module* m);
TB_Function* test_add_i16(TB_Module* m);
TB_Function* test_add_i32(TB_Module* m);
TB_Function* test_sat_add_i32(TB_Module* m);
TB_Function* test_add_i64(TB_Module* m);
TB_Function* test_add_f32(TB_Module* m);
TB_Function* test_muladd_f32(TB_Module* m);
TB_Function* test_locals_1(TB_Module* m);
TB_Function* test_params_1(TB_Module* m);
TB_Function* test_params_2(TB_Module* m);
TB_Function* test_locals_params_1(TB_Module* m);
TB_Function* test_add_sub_i32(TB_Module* m);
TB_Function* test_fib(TB_Module* m);
TB_Function* test_entry(TB_Module* m);

int main(int argc, char** argv) {
	TB_FeatureSet features = { 0 };
    
	// Currently it only supports binary output
	FILE* f = fopen("./test_x64.obj", "wb");
	do_tests(f, TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
	fclose(f);
    
	return 0;
}

void do_tests(FILE* f, TB_Arch arch, TB_System system, const TB_FeatureSet* features) {
	TB_Module* m = tb_module_create(arch, system, features);
    
#if 1
	typedef TB_Function*(*TestFunction)(TB_Module* m);
    
	static const TestFunction test_functions[] = {
		test_add_i8,
		test_add_i16,
		test_add_i32,
		test_sat_add_i32,
		test_add_i64,
		test_add_f32,
		test_muladd_f32,
		test_locals_1,
		test_params_1,
		test_params_2,
		test_locals_params_1,
		test_add_sub_i32,
		test_fib,
		test_entry
	};
	size_t count = sizeof(test_functions) / sizeof(test_functions[0]);
    
	for (size_t i = 0; i < count; i++) {
		test_functions[i](m);
	}
#elif 0
	{
		TB_Function* func = tb_function_create(m, "test", TB_TYPE_I32(1));
        
		TB_Register result = tb_inst_local(func, 4, 4);
		TB_Register index = tb_inst_local(func, 4, 4);
		TB_Register count = tb_inst_local(func, 4, 4);
        
		tb_inst_store(func, TB_TYPE_I32(1), result, tb_inst_iconst(func, TB_TYPE_I32(1), 0), 4);
		tb_inst_store(func, TB_TYPE_I32(1), index, tb_inst_iconst(func, TB_TYPE_I32(1), 0), 4);
		tb_inst_store(func, TB_TYPE_I32(1), count, tb_inst_iconst(func, TB_TYPE_I32(1), 4), 4);
        
		tb_inst_label(func, 1); // .L1:
        
		TB_Register sum = tb_inst_add(
                                      func,
                                      TB_TYPE_I32(1),
                                      tb_inst_load(func, TB_TYPE_I32(1), result, 4),
                                      tb_inst_iconst(func, TB_TYPE_I32(1), 0x40),
                                      TB_NO_WRAP
                                      );
		tb_inst_store(func, TB_TYPE_I32(1), result, sum, 4);
        
		tb_inst_store(func, TB_TYPE_I32(1), index, tb_inst_add(
                                                               func,
                                                               TB_TYPE_I32(1),
                                                               tb_inst_load(func, TB_TYPE_I32(1), index, 4),
                                                               tb_inst_iconst(func, TB_TYPE_I32(1), 1),
                                                               TB_NO_WRAP
                                                               ), 4);
        
		tb_inst_if(
                   func, 
                   tb_inst_cmp_ne(
                                  func, TB_TYPE_I32(1),
                                  tb_inst_load(func, TB_TYPE_I32(1), index, 4),
                                  tb_inst_load(func, TB_TYPE_I32(1), count, 4)
                                  ), 1, 2
                   );
        
		tb_inst_label(func, 2); // .L2:
		tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), result, 4));
		
		tb_function_print(func);
		printf("\n\n\n");
	}
#else
	{
		TB_Function* func = tb_function_create(m, "test", TB_TYPE_I64(1));
        
		TB_Register b = tb_inst_param(func, TB_TYPE_I64(1));
		TB_Register c = tb_inst_param(func, TB_TYPE_I64(1));
		TB_Register d = tb_inst_param(func, TB_TYPE_I64(1));
        
		b = tb_inst_param_addr(func, b);
		c = tb_inst_param_addr(func, c);
		d = tb_inst_param_addr(func, d);
        
		TB_Register a = tb_inst_local(func, 8, 8);
        
		for (int i = 0; i < 100000; i++) {
			TB_Register factor = tb_inst_mul(func, TB_TYPE_I64(1), 
                                             tb_inst_load(func, TB_TYPE_I64(1), c, 8),
                                             tb_inst_load(func, TB_TYPE_I64(1), d, 8),
                                             TB_NO_WRAP
                                             );
            
			TB_Register sum = tb_inst_add(func, TB_TYPE_I64(1),
                                          factor,
                                          tb_inst_load(func, TB_TYPE_I64(1), b, 8),
                                          TB_NO_WRAP
                                          );
            
			tb_inst_store(func, TB_TYPE_I64(1), a, sum, 8);
		}
        
		tb_inst_ret(func, TB_TYPE_I64(1), tb_inst_load(func, TB_TYPE_I64(1), a, 8));
	}
#endif
    
	tb_module_compile(m, TB_OPT_O0, 1);
	tb_module_export(m, f);
	tb_module_destroy(m);
}

TB_Function* test_add_i8(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I8(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I8(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I8(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I8(1), a, b, TB_NO_WRAP);
    
	tb_inst_ret(func, TB_TYPE_I8(1), sum);
	return func;
}

TB_Function* test_add_i16(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I16(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I16(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I16(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I16(1), a, b, TB_NO_WRAP);
    
	tb_inst_ret(func, TB_TYPE_I16(1), sum);
	return func;
}

TB_Function* test_add_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I32(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I32(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), a, b, TB_NO_WRAP);
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_sat_add_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
    
	TB_Register sum = tb_inst_add(
                                  func, TB_TYPE_I32(1),
                                  tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                  tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                  TB_SATURATED_UNSIGNED
                                  );
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_add_i64(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I64(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I64(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I64(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I64(1), a, b, TB_NO_WRAP);
    
	tb_inst_ret(func, TB_TYPE_I64(1), sum);
	return func;
}

TB_Function* test_add_f32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_F32(1));
	
	TB_Register a = tb_inst_fconst(func, TB_TYPE_F32(1), 0.0);
	TB_Register b = tb_inst_fconst(func, TB_TYPE_F32(1), 2.0);
	TB_Register sum = tb_inst_fadd(func, TB_TYPE_F32(1), a, b);
    
	tb_inst_ret(func, TB_TYPE_F32(1), sum);
	return func;
}

TB_Function* test_muladd_f32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_F32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_F32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_F32(1));
	TB_Register c = tb_inst_param(func, TB_TYPE_F32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
	TB_Register cp = tb_inst_param_addr(func, c);
    
	TB_Register result = tb_inst_fadd(func, TB_TYPE_F32(1), 
									  tb_inst_fmul(
												   func, TB_TYPE_F32(1),
												   tb_inst_load(func, TB_TYPE_F32(1), ap, 4),
												   tb_inst_load(func, TB_TYPE_F32(1), bp, 4)
												   ),
									  tb_inst_load(func, TB_TYPE_F32(1), cp, 4)
									  );
	
	tb_inst_ret(func, TB_TYPE_F32(1), result);
	return func;
}

TB_Function* test_locals_1(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register local = tb_inst_local(func, 4, 4);
    
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I32(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I32(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), a, b, TB_NO_WRAP);
    
	tb_inst_store(func, TB_TYPE_I32(1), local, sum, 4);
	tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), local, 4));
	return func;
}

TB_Function* test_params_1(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), a, b, TB_NO_WRAP);
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_params_2(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
    
	TB_Register sum = tb_inst_add(
                                  func, TB_TYPE_I32(1),
                                  tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                  tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                  TB_NO_WRAP
                                  );
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_locals_params_1(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
	TB_Register local = tb_inst_local(func, 4, 4);
    
	TB_Register sum = tb_inst_add(
                                  func, TB_TYPE_I32(1),
                                  tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                  tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                  TB_NO_WRAP
                                  );
    
	tb_inst_store(func, TB_TYPE_I32(1), local, sum, 4);
	tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), local, 4));
	return func;
}

TB_Function* test_add_sub_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register c = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
	TB_Register cp = tb_inst_param_addr(func, c);
    
	TB_Register result = tb_inst_add(func, TB_TYPE_I32(1), 
                                     tb_inst_sub(
                                                 func, TB_TYPE_I32(1),
                                                 tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                                 tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                                 TB_NO_WRAP
                                                 ),
                                     tb_inst_load(func, TB_TYPE_I32(1), cp, 4),
                                     TB_NO_WRAP
                                     );
    
	tb_inst_ret(func, TB_TYPE_I32(1), result);
	return func;
}

static TB_Function* test_fib_func_ref = NULL;

TB_Function* test_fib(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	test_fib_func_ref = func;
	
	TB_Register n = tb_inst_param(func, TB_TYPE_I32(1));
	
	tb_inst_if(func, tb_inst_cmp_slt(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2)), 1, 2);
	tb_inst_label(func, 1); // .L1:
	tb_inst_ret(func, TB_TYPE_I32(1), n);
	
	tb_inst_label(func, 2); // .L2:
	
	TB_Register n_minus_one = tb_inst_sub(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 1), TB_NO_WRAP);
	TB_Register call1 = tb_inst_call(func, TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_one });
	
	TB_Register n_minus_two = tb_inst_sub(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2), TB_NO_WRAP);
	TB_Register call2 = tb_inst_call(func, TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_two });
	
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), call1, call2, TB_NO_WRAP);
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	
	return func;
}

TB_Function* test_entry(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register num = tb_inst_iconst(func, TB_TYPE_I32(1), 45);
	TB_Register result = tb_inst_call(func, TB_TYPE_I32(1), test_fib_func_ref, 1, &num);
	tb_inst_ret(func, TB_TYPE_I32(1), result);
	
	return func;
}
