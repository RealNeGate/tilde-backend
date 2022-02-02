call vcvars64

if "%VSCMD_ARG_TGT_ARCH%" neq "x64" (
  echo ERROR: please run this from MSVC x64 native tools command prompt, 32-bit target is not supported!
  exit /b 1
)

if "%1" equ "debug" (
  set cl_settings=/arch:AVX /MTd /Od /WX /Zi /D_DEBUG /RTC1 /D_CRT_SECURE_NO_WARNINGS
) else (
  set cl_settings=/arch:AVX /GL /Ox /WX /GS- /DNDEBUG /D_CRT_SECURE_NO_WARNINGS
)

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

cl /c /MP src/example_main.c %tb_source_files% %cl_settings% /Fo:build\
if %errorlevel% neq 0 (
	echo "TB failed to compile!"
	exit 1
)

if "%2" equ "cuik" (
	lib /out:W:\Workspace\Cuik\tildebackend.lib build\*.obj
  
	cd W:/Workspace/Cuik/
	build.bat
) else (
	lib /out:build\tildebackend.lib build\*.obj
)
