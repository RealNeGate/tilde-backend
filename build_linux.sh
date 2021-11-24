clang_settings="-march=nehalem -O0 -Werror -Wall -Wno-unused-function -g"

tb_source_files="../src/tb/tb.c 
	../src/tb/tb_builder.c 
	../src/tb/x64/x64.c 
	../src/tb/opt/tb_opt.c
	../src/tb/tb_coff.c 
	../src/tb/tb_elf64.c 
	../src/tb/tb_jit_win32.c 
	../src/tb/tb_validate.c 
	../src/tb/tb_helper.c"

cd build
gcc $clang_settings $tb_source_files -lpthread -fPIC -c
ar -rcs tinybackend.a *.o

