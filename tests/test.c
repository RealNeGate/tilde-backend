#include "../src/tb/tb.h"

int main() {
	TB_FeatureSet features = { 0 };
	TB_Module* mod = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, TB_DEBUGFMT_NONE, &features);

	// import puts
	TB_ExternalID puts_func = tb_extern_create(mod, "puts");

	// create & compile main function
	{
		TB_FunctionPrototype* proto = tb_prototype_create(mod, TB_STDCALL, TB_TYPE_VOID, 0, false);
		TB_Function* func = tb_prototype_build(mod, proto, "main", TB_LINKAGE_PUBLIC);
		
		// main():
		//   str = "Hello, World!"
		//   call puts(str)
		//   ret
		TB_Reg str = tb_inst_cstring(func, "Hello, World!");
		tb_inst_ecall(func, TB_TYPE_VOID, puts_func, 1, (TB_Reg[]) { str });
		tb_inst_ret(func, TB_NULL_REG);
		
		tb_module_compile_func(mod, func, TB_ISEL_FAST);
	}
	
	if (!tb_module_export(mod, "lmao.obj")) {
		fprintf(stderr, "fuck!\n");
		abort();
	}
	
	system("link lmao.obj /subsystem:console /entry:main /defaultlib:ucrt");
	tb_module_destroy(mod);
	return 0;
}
