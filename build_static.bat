call vcvars64

set clang_settings=-march=nehalem -O0 -Werror -Wall -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set tb_source_files=../src/tb/tb.c ^
	../src/tb/tb_builder.c ^
	../src/tb/x64/x64.c ^
	../src/tb/opt/*.c ^
	../src/tb/tb_coff.c ^
	../src/tb/tb_elf64.c ^
	../src/tb/stb_ds.c ^
	../src/tb/tb_jit.c ^
	../src/tb/tb_win32.c ^
	../src/tb/tb_internal.c

IF NOT exist build (mkdir build)

cd build
clang %clang_settings% %tb_source_files% -c

llvm-ar rc W:/Workspace/Cuik/tinybackend.lib *.o
del *.o

cd W:/Workspace/Cuik/
build.bat
