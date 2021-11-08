call vcvars64

set clang_settings=-march=nehalem -O0 -Werror -Wall -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set tb_source_files=src/tb/tb.c ^
	src/tb/tb_x86_64_fast2.c ^
	src/tb/tb_opt_mem2reg.c ^
	src/tb/tb_opt_inline.c ^
	src/tb/tb_opt_dce.c ^
	src/tb/tb_opt_sr.c ^
	src/tb/tb_opt_canonical.c ^
	src/tb/tb_coff.c ^
	src/tb/tb_elf64.c ^
	src/tb/tb_jit_win32.c ^
	src/tb/tb_validate.c ^
	src/tb/tb_helper.c

mkdir build
clang %clang_settings% src/example_fuzzer.c %tb_source_files% -o build/example.exe
