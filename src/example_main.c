#include "tb/tb.h"

void do_tests(FILE* f, TB_Arch arch, TB_System system, const TB_FeatureSet* features);

TB_Function* test_add_i8(TB_Module* m);
TB_Function* test_add_i16(TB_Module* m);
TB_Function* test_add_i32(TB_Module* m);
TB_Function* test_add_i64(TB_Module* m);
TB_Function* test_mul_i64(TB_Module* m);
TB_Function* test_div_i64(TB_Module* m);
TB_Function* test_andor_i32(TB_Module* m);
TB_Function* test_sat_uadd_i32(TB_Module* m);
TB_Function* test_sat_sadd_i32(TB_Module* m);
TB_Function* test_safe_add_i32(TB_Module* m);
TB_Function* test_add_f32(TB_Module* m);
TB_Function* test_vadd_f32x4(TB_Module* m);
TB_Function* test_vmuladd_f32x4(TB_Module* m);
TB_Function* test_muladd_f32(TB_Module* m);
TB_Function* test_locals_1(TB_Module* m);
TB_Function* test_locals_2(TB_Module* m);
TB_Function* test_params_1(TB_Module* m);
TB_Function* test_params_2(TB_Module* m);
TB_Function* test_bools_1(TB_Module* m);
TB_Function* test_locals_params_1(TB_Module* m);
TB_Function* test_add_sub_i32(TB_Module* m);
TB_Function* test_fib(TB_Module* m);
TB_Function* test_foo(TB_Module* m);
TB_Function* test_fact(TB_Module* m);
TB_Function* test_array_access(TB_Module* m);
TB_Function* test_zero_mem(TB_Module* m);
TB_Function* test_switch_case(TB_Module* m);
TB_Function* test_entry(TB_Module* m);

static TB_ExternalID test_get_module_handle;

int main(int argc, char** argv) {
	TB_FeatureSet features = { 0 };
	
	// Currently it only supports binary output
#if 0
	FILE* f = fopen("./test_x64.o", "wb");
	do_tests(f, TB_ARCH_X86_64, TB_SYSTEM_LINUX, &features);
	fclose(f);
#else
	FILE* f = fopen("./test_x64.obj", "wb");
	do_tests(f, TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
	fclose(f);
#endif
	
	return 0;
}

void do_tests(FILE* f, TB_Arch arch, TB_System system, const TB_FeatureSet* features) {
	TB_Module* m = tb_module_create(arch, system, features, TB_OPT_O0, 1, true);
    
	typedef TB_Function*(*TestFunction)(TB_Module* m);
	static const TestFunction test_functions[] = {
		test_fact,
		test_add_i8,
		test_add_i16,
		test_add_i32,
		test_mul_i64,
		test_sat_uadd_i32,
		test_safe_add_i32,
		test_add_i64,
		test_div_i64,
		test_locals_1,
		test_locals_2,
		test_params_1,
		test_params_2,
		test_bools_1,
		test_zero_mem,
		test_add_sub_i32,
		test_locals_params_1,
		test_add_f32,
		test_andor_i32,
		test_muladd_f32,
		test_fib,
		test_vadd_f32x4,
		test_vmuladd_f32x4,
		test_array_access,
		test_foo,
        test_entry
	};
	size_t count = sizeof(test_functions) / sizeof(test_functions[0]);
	test_get_module_handle = tb_module_extern(m, "GetModuleHandleA");
	
	for (size_t i = 0; i < count; i++) {
		TB_Function* func = test_functions[i](m);
        
        tb_function_print(func, stdout);
        tb_module_compile_func(m, func);
	}
    
	if (!tb_module_compile(m)) abort();
	if (!tb_module_export(m, f)) abort();
	
	tb_module_destroy(m);
}

TB_Function* test_div_i64(TB_Module* m) {
	TB_Function* func = tb_function_create(m, "main", TB_TYPE_I64(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I64(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I64(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
    
	TB_Register sum = tb_inst_div(
                                  func, TB_TYPE_I64(1),
                                  tb_inst_load(func, TB_TYPE_I64(1), ap, 4),
                                  tb_inst_load(func, TB_TYPE_I64(1), bp, 4),
                                  true
                                  );
    
	tb_inst_ret(func, TB_TYPE_I64(1), sum);
	return func;
}

TB_Function* test_zero_mem(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_VOID());
	
	TB_Register cells = tb_inst_local(func, 32, 4);
	tb_inst_memset(func, cells, tb_inst_iconst(func, TB_TYPE_I32(1), 0), tb_inst_iconst(func, TB_TYPE_I32(1), 32), 4);
	
	tb_inst_ret(func, TB_TYPE_VOID(), TB_NULL_REG);
	return func;
}

TB_Function* test_add_i8(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I8(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I8(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I8(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I8(1), a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, TB_TYPE_I8(1), sum);
	return func;
}

TB_Function* test_add_i16(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I16(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I16(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I16(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I16(1), a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, TB_TYPE_I16(1), sum);
	return func;
}

TB_Function* test_add_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I32(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I32(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_mul_i64(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I64(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I64(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I64(1));
	TB_Register c = tb_inst_param(func, TB_TYPE_I64(1));
	
	TB_Register ap = tb_inst_param_addr(func, a);
	/*TB_Register bp = */tb_inst_param_addr(func, b);
	TB_Register cp = tb_inst_param_addr(func, c);
    
	TB_Register factor = tb_inst_mul(
									 func, TB_TYPE_I64(1),
									 tb_inst_load(func, TB_TYPE_I64(1), ap, 4),
									 tb_inst_mul(
												 func, TB_TYPE_I64(1),
												 tb_inst_load(func, TB_TYPE_I64(1), cp, 4),
												 tb_inst_load(func, TB_TYPE_I64(1), cp, 4),
												 TB_UNSIGNED_TRAP_ON_WRAP
												 ),
									 TB_SATURATED_UNSIGNED
									 );
    
	tb_inst_ret(func, TB_TYPE_I64(1), factor);
	return func;
}

TB_Function* test_sat_uadd_i32(TB_Module* m) {
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

TB_Function* test_sat_sadd_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
    
	TB_Register sum = tb_inst_add(
                                  func, TB_TYPE_I32(1),
                                  tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                  tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                  TB_SATURATED_SIGNED
                                  );
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_safe_add_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
    
	TB_Register sum = tb_inst_add(
                                  func, TB_TYPE_I32(1),
                                  tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                  tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                  TB_UNSIGNED_TRAP_ON_WRAP
                                  );
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_andor_i32(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register c = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
	TB_Register cp = tb_inst_param_addr(func, c);
    
	TB_Register result = tb_inst_and(
									 func, TB_TYPE_I32(1),
									 tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
									 tb_inst_or(
												func, TB_TYPE_I32(1),
												tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
												tb_inst_load(func, TB_TYPE_I32(1), cp, 4)
												)
									 );
    
	tb_inst_ret(func, TB_TYPE_I32(1), result);
	return func;
}

TB_Function* test_add_i64(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I64(1));
	
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I64(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I64(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I64(1), a, b, TB_ASSUME_NUW);
    
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

TB_Function* test_vadd_f32x4(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_F32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_F32(4));
	TB_Register b = tb_inst_param(func, TB_TYPE_F32(4));
	
	TB_Register result = tb_inst_fadd(func, TB_TYPE_F32(4), a, b);
	
	tb_inst_ret(func, TB_TYPE_F32(4), result);
	return func;
}

TB_Function* test_vmuladd_f32x4(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_F32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_F32(4));
	TB_Register b = tb_inst_param(func, TB_TYPE_F32(4));
	TB_Register c = tb_inst_param(func, TB_TYPE_F32(4));
	
	TB_Register result = tb_inst_fadd(func, TB_TYPE_F32(4), tb_inst_fmul(func, TB_TYPE_F32(4), a, b), c);
	
	tb_inst_ret(func, TB_TYPE_F32(4), result);
	return func;
}

TB_Function* test_locals_1(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register local = tb_inst_local(func, 4, 4);
    
	TB_Register a = tb_inst_iconst(func, TB_TYPE_I32(1), 64);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I32(1), 32);
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), a, b, TB_ASSUME_NUW);
    
	tb_inst_store(func, TB_TYPE_I32(1), local, sum, 4);
	tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), local, 4));
	return func;
}

TB_Function* test_locals_2(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register local = tb_inst_local(func, 8, 4);
    
	TB_Register a = tb_inst_member_access(func, local, 0);
	TB_Register b = tb_inst_member_access(func, local, 4);
	
	tb_inst_store(func, TB_TYPE_I32(1), a, tb_inst_iconst(func, TB_TYPE_I32(1), 69), 4);
	tb_inst_store(func, TB_TYPE_I32(1), b, tb_inst_iconst(func, TB_TYPE_I32(1), 69), 4);
    
	tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), b, 4));
	return func;
}

TB_Function* test_params_1(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), a, b, TB_ASSUME_NUW);
    
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
                                  TB_ASSUME_NUW
                                  );
    
	tb_inst_ret(func, TB_TYPE_I32(1), sum);
	return func;
}

TB_Function* test_bools_1(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_BOOL(1));
	
	TB_Register local = tb_inst_local(func, 1, 1);
	tb_inst_store(func, TB_TYPE_BOOL(1), local, tb_inst_iconst(func, TB_TYPE_BOOL(1), 1), 1);
    
	tb_inst_ret(func, TB_TYPE_BOOL(1), tb_inst_load(func, TB_TYPE_BOOL(1), local, 4));
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
                                  TB_ASSUME_NUW
                                  );
    
	tb_inst_store(func, TB_TYPE_I32(1), local, sum, 4);
	tb_inst_ret(func, TB_TYPE_I32(1), tb_inst_load(func, TB_TYPE_I32(1), local, 4));
	return func;
}

TB_Function* test_array_access(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_PTR());
	
	TB_Register a = tb_inst_param(func, TB_TYPE_PTR());
	TB_Register b = tb_inst_param(func, TB_TYPE_PTR());
	
	tb_inst_ret(func, TB_TYPE_PTR(), tb_inst_array_access(func, a, b, 24));
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
    
	TB_Register result = tb_inst_sub(func, TB_TYPE_I32(1), 
                                     tb_inst_load(func, TB_TYPE_I32(1), cp, 4),
                                     tb_inst_add(
                                                 func, TB_TYPE_I32(1),
                                                 tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
                                                 tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
                                                 TB_ASSUME_NUW
                                                 ),
                                     TB_ASSUME_NUW
                                     );
    
	tb_inst_ret(func, TB_TYPE_I32(1), result);
	return func;
}

static TB_Function* test_fib_func_ref = NULL;
static TB_Function* test_foo_func_ref = NULL;

TB_Function* test_fib(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	test_fib_func_ref = func;
	
	TB_Register n = tb_inst_param(func, TB_TYPE_I32(1));
	
	TB_Label if_true = tb_inst_new_label_id(func);
	TB_Label if_false = tb_inst_new_label_id(func);
	
	// if (n < 2)
	tb_inst_if(func, tb_inst_cmp_slt(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2)), if_true, if_false);
	
	// then
	{
		tb_inst_label(func, if_true);
		tb_inst_ret(func, TB_TYPE_I32(1), n);
	}
	
	// else
	{
		tb_inst_label(func, if_false);
		
		TB_Register n_minus_one = tb_inst_sub(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 1), TB_ASSUME_NUW);
		TB_Register call1 = tb_inst_call(func, TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_one });
		
		TB_Register n_minus_two = tb_inst_sub(func, TB_TYPE_I32(1), n, tb_inst_iconst(func, TB_TYPE_I32(1), 2), TB_ASSUME_NUW);
		TB_Register call2 = tb_inst_call(func, TB_TYPE_I32(1), func, 1, (TB_Register[]) { n_minus_two });
		
		TB_Register sum = tb_inst_add(func, TB_TYPE_I32(1), call1, call2, TB_ASSUME_NUW);
		tb_inst_ret(func, TB_TYPE_I32(1), sum);
	}
	
	return func;
}

TB_Function* test_fact(TB_Module* m) {
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
		tb_inst_if(func, tb_inst_cmp_slt(func, TB_TYPE_I32(1), n_ld, tb_inst_iconst(func, TB_TYPE_I32(1), 0)), loop_body, loop_exit);
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

TB_Function* test_foo(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	test_foo_func_ref = func;
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register b = tb_inst_param(func, TB_TYPE_I32(1));
    
	TB_Register ap = tb_inst_param_addr(func, a);
	TB_Register bp = tb_inst_param_addr(func, b);
	
	TB_Register factor = tb_inst_mul(func, TB_TYPE_I32(1),
									 tb_inst_load(func, TB_TYPE_I32(1), ap, 4),
									 tb_inst_load(func, TB_TYPE_I32(1), bp, 4),
									 TB_ASSUME_NUW
									 );
	
	tb_inst_ret(func, TB_TYPE_I32(1), factor);
	return func;
}

TB_Function* test_entry(TB_Module* m) {
	TB_Function* func = tb_function_create(m, __FUNCTION__, TB_TYPE_I32(1));
	
	TB_Register a = tb_inst_param(func, TB_TYPE_I32(1));
	TB_Register ap = tb_inst_param_addr(func, a);
	
	TB_Register al = tb_inst_load(func, TB_TYPE_I32(1), ap, 4);
	TB_Register b = tb_inst_iconst(func, TB_TYPE_I32(1), 2);
	
	tb_inst_ecall(func, TB_TYPE_VOID(), test_get_module_handle, 1, (TB_Register[]) {
					  tb_inst_iconst(func, TB_TYPE_I64(1), 0)
				  });
	
	TB_Register result = tb_inst_call(func, TB_TYPE_I32(1), test_foo_func_ref, 2, (TB_Register[]) { al, b });
	tb_inst_ret(func, TB_TYPE_I32(1), result);
	
	//TB_Register num = tb_inst_iconst(func, TB_TYPE_I32(1), 45);
	//TB_Register result = tb_inst_call(func, TB_TYPE_I32(1), test_fib_func_ref, 1, &num);
	//tb_inst_ret(func, TB_TYPE_I32(1), result);
	
	return func;
}
