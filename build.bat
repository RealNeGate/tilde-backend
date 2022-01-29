@echo off
setlocal enabledelayedexpansion
call vcvars64

if "%VSCMD_ARG_TGT_ARCH%" neq "x64" (
  echo ERROR: please run this from MSVC x64 native tools command prompt, 32-bit target is not supported!
  exit /b 1
)

set cl_settings=/arch:AVX /MTd /WX /Od /Zi /D_DEBUG /RTC1 /D_CRT_SECURE_NO_WARNINGS

set tb_source_files=src/tb/tb.c ^
	src/tb/tb_atomic.c ^
	src/tb/tb_builder.c ^
	src/tb/x64/x64.c ^
	src/tb/opt/*.c ^
	src/tb/tb_coff.c ^
	src/tb/tb_elf64.c ^
	src/tb/tb_jit.c ^
	src/tb/tb_win32.c ^
	src/tb/tb_internal.c ^
	src/tb/stb_ds.c

IF NOT exist build (mkdir build)

cl /MP src/example_main.c %tb_source_files% %cl_settings% /Fo:build\ /Fe:build\example.exe /link /INCREMENTAL:NO /SUBSYSTEM:CONSOLE

del build\*.obj
