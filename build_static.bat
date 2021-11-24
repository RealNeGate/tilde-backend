@echo off
setlocal EnableDelayedExpansion

call vcvars64

set clang_settings=-march=nehalem -O0 -Werror -Wall -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set tb_source_files=../src/tb/tb.c ^
	../src/tb/tb_builder.c ^
	../src/tb/x64/x64.c ^
	../src/tb/opt/tb_opt.c ^
	../src/tb/tb_coff.c ^
	../src/tb/tb_elf64.c ^
	../src/tb/tb_jit_win32.c ^
	../src/tb/tb_validate.c ^
	../src/tb/tb_helper.c

IF NOT exist build (mkdir build)

cd build
clang %clang_settings% %tb_source_files% -c

set tb_object_files=tb.o ^
	tb_builder.o ^
	x64.o ^
	tb_opt.o ^
	tb_coff.o ^
	tb_elf64.o ^
	tb_jit_win32.o ^
	tb_validate.o ^
	tb_helper.o
	
llvm-ar rc tinybackend.lib %tb_object_files%
