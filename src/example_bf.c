#include "tb/tb.h"

static TB_Function* test_putc = NULL;
static TB_Function* test_getc = NULL;

static const char* compile_bf(TB_Function* func, const char* at, TB_Register cells, TB_Register ptr) {
	while (*at) {
		char c = *at++;
		switch (c) {
			case '+': {
				TB_Register index = tb_inst_load(func, TB_TYPE_I32(1), ptr, 4);
				TB_Register addr = tb_inst_array_access(func, cells, index, 1);
				
				TB_Register ld = tb_inst_load(func, TB_TYPE_I8(1), addr, 1);
				TB_Register one = tb_inst_iconst(func, TB_TYPE_I8(1), 1);
				TB_Register result = tb_inst_add(func, TB_TYPE_I8(1), ld, one, TB_CAN_WRAP);
				
				tb_inst_store(func, TB_TYPE_I32(1), addr, result, 4);
				break;
			}
			case '-': {
				TB_Register index = tb_inst_load(func, TB_TYPE_I32(1), ptr, 4);
				TB_Register addr = tb_inst_array_access(func, cells, index, 1);
				
				TB_Register ld = tb_inst_load(func, TB_TYPE_I8(1), addr, 1);
				TB_Register one = tb_inst_iconst(func, TB_TYPE_I8(1), 1);
				TB_Register result = tb_inst_sub(func, TB_TYPE_I8(1), ld, one, TB_CAN_WRAP);
				
				tb_inst_store(func, TB_TYPE_I32(1), addr, result, 4);
				break;
			}
			case '[': {
				
				break;
			}
			case ']': return at;
			default: break;
		}
	}
	
	return at;
}

int main(int argc, char** argv) {
	TB_FeatureSet features = { 0 };
	TB_Module* m = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
	
	test_putc = tb_function_create(m, "putc", TB_TYPE_VOID());
	test_getc = tb_function_create(m, "getc", TB_TYPE_I8(1));
	
	{
		const char* at = "-";
		TB_Function* func = tb_function_create(m, "getc", TB_TYPE_I32(1));
		
		TB_Register ptr = tb_inst_local(func, 4, 4);
		TB_Register cells = tb_inst_local(func, 512, 4);
		
		compile_bf(func, at, cells, ptr);
	}
	
	tb_module_compile(m, TB_OPT_O0, 1);
	
	FILE* file = fopen("./test_x64.obj", "wb");
	tb_module_export(m, file);
	fclose(file);
	
	tb_module_destroy(m);
	return 0;
}
