call vcvars64

set clang_settings=-O0 -Werror -Wall -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set tb_source_files=src/tb/tb.c ^
	src/tb/tb_aarch64.c ^
	src/tb/tb_x86_64_fast2.c ^
	src/tb/tb_opt_mem2reg.c ^
	src/tb/tb_opt_dce.c ^
	src/tb/tb_coff.c ^
	src/tb/tb_elf64.c ^
	src/tb/tb_helper.c

mkdir build
clang %clang_settings% src/example_main.c %tb_source_files% -o build/example.exe
