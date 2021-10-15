call vcvars64

set clang_settings=-O0 -Werror -Wall -g -gcodeview -D_CRT_SECURE_NO_WARNINGS
set clang_libraries=-lkernel32 -luser32 -lshell32 -lole32 -lopengl32 -lgdi32

set tb_source_files=src/tb/tb.c ^
	src/tb/tb_aarch64.c ^
	src/tb/tb_x86_64.c ^
	src/tb/tb_coff.c ^
	src/tb/tb_elf64.c ^
	src/tb/tb_opt.c


mkdir build
clang %clang_settings% src/example_main.c %tb_source_files% %clang_libraries% -o build/example.exe
