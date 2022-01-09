#include "tb/tb.h"

static TB_ExternalID test_external1;
static TB_ExternalID test_external2;
static TB_GlobalID test_global;
static TB_Function* test_fib_func_ref = NULL;
static TB_Function* test_foo_func_ref = NULL;

TB_Function* test_atomics(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_VOID, 4, false);
	tb_prototype_add_params(p, 4, (TB_DataType[]) { TB_TYPE_PTR, TB_TYPE_PTR, TB_TYPE_PTR, TB_TYPE_PTR });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register params[4];
	for (int i = 0; i < 4; i++) {
		params[i] = tb_inst_param_addr(func, i);
	}
	
	static const TB_DataType dt[] = { TB_TYPE_I8, TB_TYPE_I16, TB_TYPE_I32, TB_TYPE_I64 };
	for (int i = 0; i < 4; i++) {
		// try all the atomics
		TB_Register addr = tb_inst_load(func, TB_TYPE_PTR, params[i], 8);
		
		tb_inst_atomic_xchg(func, addr, tb_inst_sint(func, dt[i], 16), TB_MEM_ORDER_SEQ_CST);
		tb_inst_atomic_add(func, addr, tb_inst_sint(func, dt[i], 32), TB_MEM_ORDER_SEQ_CST);
		tb_inst_atomic_sub(func, addr, tb_inst_sint(func, dt[i], 64), TB_MEM_ORDER_SEQ_CST);
		tb_inst_atomic_and(func, addr, tb_inst_sint(func, dt[i], 24), TB_MEM_ORDER_SEQ_CST);
		tb_inst_atomic_xor(func, addr, tb_inst_sint(func, dt[i], 48), TB_MEM_ORDER_SEQ_CST);
		tb_inst_atomic_or(func, addr, tb_inst_sint(func, dt[i], 56), TB_MEM_ORDER_SEQ_CST);
	}
    
	tb_inst_ret(func, TB_NULL_REG);
	return func;
}

TB_Function* test_div_i64(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I64, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I64, TB_TYPE_I64 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
    
	TB_Register sum = tb_inst_div(func,
                                  tb_inst_load(func, TB_TYPE_I64, a, 4),
                                  tb_inst_load(func, TB_TYPE_I64, b, 4),
                                  true);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_zero_mem(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_VOID, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register cells = tb_inst_local(func, 32, 4);
	tb_inst_memset(func, cells, tb_inst_sint(func, TB_TYPE_I32, 0), tb_inst_sint(func, TB_TYPE_I32, 32), 4);
	
	tb_inst_ret(func, TB_NULL_REG);
	return func;
}

TB_Function* test_add_i8(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I8, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_sint(func, TB_TYPE_I8, 64);
	TB_Register b = tb_inst_sint(func, TB_TYPE_I8, 32);
	TB_Register sum = tb_inst_add(func, a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_add_i16(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I16, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_sint(func, TB_TYPE_I16, 64);
	TB_Register b = tb_inst_sint(func, TB_TYPE_I16, 32);
	TB_Register sum = tb_inst_add(func, a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_add_i32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_sint(func, TB_TYPE_I32, 64);
	TB_Register b = tb_inst_sint(func, TB_TYPE_I32, 32);
	TB_Register sum = tb_inst_add(func, a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_add_i64(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I64, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_sint(func, TB_TYPE_I64, 64);
	TB_Register b = tb_inst_sint(func, TB_TYPE_I64, 32);
	TB_Register sum = tb_inst_add(func, a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_add_f32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_F32, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_float(func, TB_TYPE_F32, 0.0);
	TB_Register b = tb_inst_float(func, TB_TYPE_F32, 2.0);
	TB_Register sum = tb_inst_fadd(func, a, b);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_add_f64(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_F64, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_float(func, TB_TYPE_F64, 0.0);
	TB_Register b = tb_inst_float(func, TB_TYPE_F64, 2.0);
	TB_Register sum = tb_inst_fadd(func, a, b);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_cvt_f32f64(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_F64, 1, false);
	tb_prototype_add_params(p, 1, (TB_DataType[]) { TB_TYPE_F32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param(func, 0);
	tb_inst_ret(func, tb_inst_fpxt(func, a, TB_TYPE_F64));
	return func;
}

TB_Function* test_mul_i64(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I64, 3, false);
	tb_prototype_add_params(p, 3, (TB_DataType[]) { TB_TYPE_I64, TB_TYPE_I64, TB_TYPE_I64 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
	TB_Register c = tb_inst_param_addr(func, 2);
    
	TB_Register factor = tb_inst_mul(func, 
									 tb_inst_load(func, TB_TYPE_I64, a, 4),
									 tb_inst_mul(func,
												 tb_inst_load(func, TB_TYPE_I64, b, 4),
												 tb_inst_load(func, TB_TYPE_I64, c, 4),
												 TB_UNSIGNED_TRAP_ON_WRAP),
									 TB_SATURATED_UNSIGNED
									 );
    
	tb_inst_ret(func, factor);
	return func;
}

TB_Function* test_sat_uadd_i32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
    
	TB_Register sum = tb_inst_add(func,
                                  tb_inst_load(func, TB_TYPE_I32, a, 4),
                                  tb_inst_load(func, TB_TYPE_I32, b, 4),
                                  TB_SATURATED_UNSIGNED);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_sat_sadd_i32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
    
	TB_Register sum = tb_inst_add(func,
                                  tb_inst_load(func, TB_TYPE_I32, a, 4),
                                  tb_inst_load(func, TB_TYPE_I32, b, 4),
                                  TB_SATURATED_SIGNED);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_safe_add_i32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
    
	TB_Register sum = tb_inst_add(func,
                                  tb_inst_load(func, TB_TYPE_I32, a, 4),
                                  tb_inst_load(func, TB_TYPE_I32, b, 4),
                                  TB_UNSIGNED_TRAP_ON_WRAP);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_andor_i32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 3, false);
	tb_prototype_add_params(p, 3, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
	TB_Register c = tb_inst_param_addr(func, 2);
    
	TB_Register result = tb_inst_and(func, 
									 tb_inst_load(func, TB_TYPE_I32, a, 4),
									 tb_inst_or(func,
												tb_inst_load(func, TB_TYPE_I32, b, 4),
												tb_inst_load(func, TB_TYPE_I32, c, 4)
												));
    
	tb_inst_ret(func, result);
	return func;
}

TB_Function* test_muladd_f32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_F32, 3, false);
	tb_prototype_add_params(p, 3, (TB_DataType[]) { TB_TYPE_F32, TB_TYPE_F32, TB_TYPE_F32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
	TB_Register c = tb_inst_param_addr(func, 2);
    
	TB_Register result = tb_inst_fadd(func, tb_inst_fmul(func, 
														 tb_inst_load(func, TB_TYPE_F32, a, 4),
														 tb_inst_load(func, TB_TYPE_F32, b, 4)),
									  tb_inst_load(func, TB_TYPE_F32, c, 4));
	
	tb_inst_ret(func, result);
	return func;
}

TB_Function* test_locals_1(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register local = tb_inst_local(func, 4, 4);
    
	TB_Register a = tb_inst_sint(func, TB_TYPE_I32, 64);
	TB_Register b = tb_inst_sint(func, TB_TYPE_I32, 32);
	TB_Register sum = tb_inst_add(func, a, b, TB_ASSUME_NUW);
    
	tb_inst_store(func, TB_TYPE_I32, local, sum, 4);
	tb_inst_ret(func, tb_inst_load(func, TB_TYPE_I32, local, 4));
	return func;
}

TB_Function* test_locals_2(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register local = tb_inst_local(func, 8, 4);
    
	TB_Register a = tb_inst_member_access(func, local, 0);
	TB_Register b = tb_inst_member_access(func, local, 4);
	
	tb_inst_store(func, TB_TYPE_I32, a, tb_inst_sint(func, TB_TYPE_I32, 69), 4);
	tb_inst_store(func, TB_TYPE_I32, b, tb_inst_sint(func, TB_TYPE_I32, 69), 4);
    
	tb_inst_ret(func, tb_inst_load(func, TB_TYPE_I32, b, 4));
	return func;
}

TB_Function* test_params_1(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param(func, 0);
	TB_Register b = tb_inst_param(func, 1);
	TB_Register sum = tb_inst_add(func, a, b, TB_ASSUME_NUW);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_params_2(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
    
	TB_Register sum = tb_inst_add(func,
                                  tb_inst_load(func, TB_TYPE_I32, a, 4),
                                  tb_inst_load(func, TB_TYPE_I32, b, 4),
                                  TB_ASSUME_NUW);
    
	tb_inst_ret(func, sum);
	return func;
}

TB_Function* test_bools_1(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_BOOL, 0, false);
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register local = tb_inst_local(func, 1, 1);
	tb_inst_store(func, TB_TYPE_BOOL, local, tb_inst_sint(func, TB_TYPE_BOOL, 1), 1);
    
	tb_inst_ret(func, tb_inst_load(func, TB_TYPE_BOOL, local, 4));
	return func;
}

TB_Function* test_locals_params_1(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
	TB_Register local = tb_inst_local(func, 4, 4);
    
	TB_Register sum = tb_inst_add(func,
                                  tb_inst_load(func, TB_TYPE_I32, a, 4),
                                  tb_inst_load(func, TB_TYPE_I32, b, 4),
                                  TB_ASSUME_NUW);
    
	tb_inst_store(func, TB_TYPE_I32, local, sum, 4);
	tb_inst_ret(func, tb_inst_load(func, TB_TYPE_I32, local, 4));
	return func;
}

TB_Function* test_array_access(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_PTR, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_PTR, TB_TYPE_I64 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param(func, 0);
	TB_Register b = tb_inst_param(func, 1);
	
	tb_inst_ret(func, tb_inst_array_access(func, a, b, 24));
	return func;
}

TB_Function* test_add_sub_i32(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 3, false);
	tb_prototype_add_params(p, 3, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
	TB_Register c = tb_inst_param_addr(func, 2);
    
	TB_Register result = tb_inst_sub(func,
                                     tb_inst_load(func, TB_TYPE_I32, c, 4),
                                     tb_inst_add(func,
                                                 tb_inst_load(func, TB_TYPE_I32, a, 4),
                                                 tb_inst_load(func, TB_TYPE_I32, b, 4),
                                                 TB_ASSUME_NUW),
                                     TB_ASSUME_NUW);
    
	tb_inst_ret(func, result);
	return func;
}

TB_Function* test_fib(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 1, false);
	tb_prototype_add_params(p, 1, (TB_DataType[]) { TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	test_fib_func_ref = func;
	
	TB_Register n = tb_inst_param(func, 0);
	
	TB_Label if_true = tb_inst_new_label_id(func);
	TB_Label if_false = tb_inst_new_label_id(func);
	
	// if (n < 2)
	tb_inst_if(func, tb_inst_cmp_ilt(func, n, tb_inst_sint(func, TB_TYPE_I32, 2), true), if_true, if_false);
	
	// then
	{
		tb_inst_label(func, if_true);
		tb_inst_ret(func, n);
	}
	
	// else
	{
		tb_inst_label(func, if_false);
		
		TB_Register n_minus_one = tb_inst_sub(func, n, tb_inst_sint(func, TB_TYPE_I32, 1), TB_ASSUME_NUW);
		TB_Register call1 = tb_inst_call(func, TB_TYPE_I32, func, 1, (TB_Register[]) { n_minus_one });
		
		TB_Register n_minus_two = tb_inst_sub(func, n, tb_inst_sint(func, TB_TYPE_I32, 2), TB_ASSUME_NUW);
		TB_Register call2 = tb_inst_call(func, TB_TYPE_I32, func, 1, (TB_Register[]) { n_minus_two });
		
		TB_Register sum = tb_inst_add(func, call1, call2, TB_ASSUME_NUW);
		tb_inst_ret(func, sum);
	}
	
	return func;
}

TB_Function* test_fact(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 1, false);
	tb_prototype_add_params(p, 1, (TB_DataType[]) { TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register n_addr = tb_inst_param_addr(func, 0);
	TB_Register f_addr = tb_inst_local(func, 4, 4);
	tb_inst_store(func, TB_TYPE_I32, f_addr, tb_inst_sint(func, TB_TYPE_I32, 1), 4);
	
	TB_Label loop_entry = tb_inst_new_label_id(func);
	TB_Label loop_body = tb_inst_new_label_id(func);
	TB_Label loop_exit = tb_inst_new_label_id(func);
	
	// Loop entry
	{
		tb_inst_label(func, loop_entry);
		TB_Register n_ld = tb_inst_load(func, TB_TYPE_I32, n_addr, 4);
		tb_inst_if(func, tb_inst_cmp_ilt(func, n_ld, tb_inst_sint(func, TB_TYPE_I32, 0), true), loop_body, loop_exit);
	}
	
	// Loop body
	{
		tb_inst_label(func, loop_body);
		
		// f = f * n
		TB_Register f_ld = tb_inst_load(func, TB_TYPE_I32, f_addr, 4);
		TB_Register n_ld = tb_inst_load(func, TB_TYPE_I32, n_addr, 4);
		tb_inst_store(func, TB_TYPE_I32, f_addr, tb_inst_mul(func, f_ld, n_ld, TB_ASSUME_NUW), 4);
		
		// n = n - 1
		TB_Register n_ld2 = tb_inst_load(func, TB_TYPE_I32, n_addr, 4);
		tb_inst_store(func, TB_TYPE_I32, n_addr, tb_inst_sub(func, n_ld2, tb_inst_sint(func, TB_TYPE_I32, 1), TB_ASSUME_NUW), 4);
		
		tb_inst_goto(func, loop_entry);
	}
	
	tb_inst_label(func, loop_exit);
	tb_inst_ret(func, tb_inst_load(func, TB_TYPE_I32, f_addr, 4));
	return func;
}

TB_Function* test_foo(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I32, 2, false);
	tb_prototype_add_params(p, 2, (TB_DataType[]) { TB_TYPE_I32, TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	test_foo_func_ref = func;
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register b = tb_inst_param_addr(func, 1);
	
	TB_Register factor = tb_inst_mul(func,
									 tb_inst_load(func, TB_TYPE_I32, a, 4),
									 tb_inst_load(func, TB_TYPE_I32, b, 4),
									 TB_ASSUME_NUW);
	
	tb_inst_ret(func, factor);
	return func;
}

TB_Function* test_entry(TB_Module* m) {
	TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_PTR, 1, false);
	tb_prototype_add_params(p, 1, (TB_DataType[]) { TB_TYPE_I32 });
	TB_Function* func = tb_prototype_build(m, p, __FUNCTION__, TB_LINKAGE_PUBLIC);
	
	TB_Register a = tb_inst_param_addr(func, 0);
	TB_Register al = tb_inst_load(func, TB_TYPE_I32, a, 4);
	TB_Register b = tb_inst_sint(func, TB_TYPE_I32, 2);
	
	tb_inst_ecall(func, TB_TYPE_VOID, test_external1, 1, (TB_Register[]) {
					  tb_inst_sint(func, TB_TYPE_I64, 0)
				  });
	
	tb_inst_ecall(func, TB_TYPE_VOID, test_external2, 1, (TB_Register[]) {
					  tb_inst_cstring(func, "Hello, World!")
				  });
	
	tb_inst_call(func, TB_TYPE_I32, test_foo_func_ref, 2, (TB_Register[]) { al, b });
	
	tb_inst_store(func, TB_TYPE_I32, tb_inst_get_global_address(func, test_global),
				  tb_inst_sint(func, TB_TYPE_I32, 69), 4);
	
	TB_Register ptr = tb_inst_get_func_address(func, test_fib_func_ref);
	tb_inst_ret(func, ptr);
	
	return func;
}

void do_tests(const char* obj_path, TB_Arch arch, TB_System system, const TB_FeatureSet* features) {
	TB_Module* m = tb_module_create(arch, system, features);
    
	typedef TB_Function*(*TestFunction)(TB_Module* m);
	static const TestFunction test_functions[] = {
		test_fact,
		test_add_f64,
		test_cvt_f32f64,
		test_atomics,
#if 1
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
		test_add_f64,
		test_cvt_f32f64,
		test_andor_i32,
		test_muladd_f32,
		test_fib,
		test_array_access,
		test_foo,
		test_entry
#endif
	};
	size_t count = sizeof(test_functions) / sizeof(test_functions[0]);
	test_external1 = tb_extern_create(m, "GetModuleHandleA");
	test_external2 = tb_extern_create(m, "OutputDebugStringA");
	
	TB_InitializerID test_init = tb_initializer_create(m, 4, 4, 0);
	test_global = tb_global_create(m, test_init, "some_global", TB_LINKAGE_PUBLIC);
	
	for (size_t i = 0; i < count; i++) {
		TB_Function* func = test_functions[i](m);
        
		tb_function_optimize(func, TB_OPT_O1);
        tb_module_compile_func(m, func);
		//tb_function_print(func, stdout);
	}
    
	if (!tb_module_compile(m)) abort();
	if (!tb_module_export(m, obj_path)) abort();
	
	tb_module_destroy(m);
}

int main(int argc, char** argv) {
	TB_FeatureSet features = { 0 };
	
#if 0
	do_tests("./test_x64.o", TB_ARCH_X86_64, TB_SYSTEM_LINUX, &features);
#else
	do_tests("./test_x64.obj", TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
#endif
	
	return 0;
}
