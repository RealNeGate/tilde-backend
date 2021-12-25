clang_settings="-march=nehalem -O2 -DNDEBUG -Werror -Wall -Wno-unused-function -g"

tb_source_files="../src/tb/tb.c 
	../src/tb/tb_builder.c 
	../src/tb/x64/x64.c 
	../src/tb/stb_ds.c 
	../src/tb/opt/tb_opt.c
	../src/tb/tb_coff.c 
	../src/tb/tb_elf64.c 
	../src/tb/tb_jit.c 
	../src/tb/tb_posix.c 
	../src/tb/tb_internal.c"

cd build
gcc $clang_settings $tb_source_files -lpthread -fPIC -c
ar -rcs tinybackend.a *.o

