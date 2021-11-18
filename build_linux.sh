clang_settings="-march=nehalem -O0 -Werror -Wall -Wno-unused-function -g"

tb_source_files="../src/tb/tb.c 
	../src/tb/tb_builder.c 
	../src/tb/x64/codegen.c 
	../src/tb/tb_opt_mem2reg.c 
	../src/tb/tb_opt_inline.c 
	../src/tb/tb_opt_dce.c 
	../src/tb/tb_opt_sr.c 
	../src/tb/tb_opt_canonical.c 
	../src/tb/tb_coff.c 
	../src/tb/tb_elf64.c 
	../src/tb/tb_jit_win32.c 
	../src/tb/tb_validate.c 
	../src/tb/tb_helper.c"

cd build
gcc $clang_settings $tb_source_files -lpthread -fPIC -c
ar -rcs tb.a *.o

