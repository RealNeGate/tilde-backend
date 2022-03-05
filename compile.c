// This is the build script for TB, real simple imo
// just call it with the C11 compiler of your choice
//
// It's inspired by nobuild but different
#include "compile.h"
//#define RELEASE_BUILD

static const char* output_lib_path = NULL;

static void delete_intermediates(const char* ext) {
	char temp[PATH_MAX];
	
	const char* path;
	FileIter it = file_iter_open("build" SLASH);
	while ((path = file_iter_next(&it))) {
		if (str_ends_with(path, ext)) {
			sprintf(temp, "build" SLASH "%s", path);
			remove(temp);
		}
	}
	file_iter_close(&it);
}

static void compile_with_cl() {
	cmd_append("cl /Fo:build\\ /MP /arch:AVX /D_CRT_SECURE_NO_WARNINGS ");
	
#if defined(BUILD_FUZZER)
	cmd_append("/Fe:build\\fuzzer.exe ");
#else
	cmd_append("/c ");
#endif
	
#if defined(RELEASE_BUILD)
	cmd_append("/GL /Ox /WX /GS- /DNDEBUG ");
#else
	cmd_append("/MTd /Od /WX /Zi /D_DEBUG /RTC1 ");
#endif
	
	// all the source files
	const char* path;
	FileIter it = file_iter_open("src" SLASH "tb");
	while ((path = file_iter_next(&it))) {
		// ignore tb_posix on MSVC compiles
		if (str_ends_with(path, ".c") && strcmp(path, "tb_posix.c")) {
			cmd_append("src" SLASH "tb" SLASH);
			cmd_append(path);
			cmd_append(" ");
		}
	}
	file_iter_close(&it);
	
	// optimizer
	it = file_iter_open("src" SLASH "tb" SLASH "opt" SLASH);
	while ((path = file_iter_next(&it))) {
		// ignore tb_posix on MSVC compiles
		if (str_ends_with(path, ".c")) {
			cmd_append("src" SLASH "tb" SLASH "opt" SLASH);
			cmd_append(path);
			cmd_append(" ");
		}
	}
	file_iter_close(&it);
	
	// codegen module
	cmd_append("src\\tb\\codegen\\tree.c ");
	
	// x64 target module
	cmd_append("src\\tb\\x64\\x64.c ");
	
	// fuzzer.c
#if defined(BUILD_FUZZER)
	cmd_append("src\\fuzzer.c ");
#endif
	
	cmd_run();
	
	////////////////////////////////
	// Run Linker
	////////////////////////////////
#if !defined(BUILD_FUZZER)
	cmd_append("lib /out:");
	cmd_append(output_lib_path);
	cmd_append(" build\\*.obj");
	cmd_run();
#endif
	
	delete_intermediates(".obj");
}

static void compile_file_with_cc(const char* cc_command, const char* directory, const char* input, const char* output) {
	cmd_append(cc_command);
	cmd_append(" -march=nehalem -Werror -Wall -Wno-unused-function -g ");
	
#if !defined(BUILD_FUZZER)
	cmd_append("-c ");
#endif
	
#if _WIN32
	cmd_append("-D_CRT_SECURE_NO_WARNINGS -gcodeview ");
#endif
	
#if defined(RELEASE_BUILD)
	cmd_append("-O2 -DNDEBUG ");
#else
	cmd_append("-O0 -D_DEBUG ");
#endif
	
	cmd_append("src" SLASH);
	cmd_append(directory);
	cmd_append(input);
	
	cmd_append(" -o build" SLASH);
	cmd_append(output ? output : str_no_ext(input));
	cmd_append(".o");
	
	cmd_run();
}

static void compile_with_cc(const char* cc_command) {
	////////////////////////////////
	// Run Compiler
	////////////////////////////////
	const char* path;
	
	FileIter it = file_iter_open("src" SLASH "tb");
	while ((path = file_iter_next(&it))) {
		bool ignore = false;
#if _WIN32
		ignore = (strcmp(path, "tb_posix.c") == 0);
#else
		ignore = (strcmp(path, "tb_win32.c") == 0);
#endif
		
		if (str_ends_with(path, ".c") && !ignore) {
			compile_file_with_cc(cc_command, "tb" SLASH, path, NULL);
		}
	}
	file_iter_close(&it);
	
	// optimizer
	it = file_iter_open("src" SLASH "tb" SLASH "opt");
	while ((path = file_iter_next(&it))) {
		// ignore tb_posix on MSVC compiles
		if (str_ends_with(path, ".c")) {
			compile_file_with_cc(cc_command, "tb" SLASH "opt" SLASH, path, NULL);
		}
	}
	file_iter_close(&it);
	
	// x64 target module
	compile_file_with_cc(cc_command, "tb" SLASH, "x64" SLASH "x64.c", "x64");
	
	// fuzzer.c
#if defined(BUILD_FUZZER)
	compile_file_with_cc(cc_command, "", "src" SLASH "fuzzer.c", "fuzzer");
#endif
	
	////////////////////////////////
	// Run Linker
	////////////////////////////////
#if defined(BUILD_FUZZER)
	printf("Figure it out, i can't think rn\n");
	abort();
#else
#if _WIN32
	cmd_append("llvm-ar rc ");
#else
	cmd_append("ar -rcs ");
#endif
	
	cmd_append(output_lib_path);
	cmd_append(" ");
	
	it = file_iter_open("build" SLASH);
	while ((path = file_iter_next(&it))) {
		if (str_ends_with(path, ".o")) {
			cmd_append("build" SLASH);
			cmd_append(path);
			cmd_append(" ");
		}
	}
	file_iter_close(&it);
	cmd_run();
	
	delete_intermediates(".o");
#endif
}

int main(int argc, char** argv) {
	if (argc >= 2) {
		output_lib_path = argv[1];
	} else {
#if _WIN32
		output_lib_path = "build\\tildebackend.lib";
#else
		output_lib_path = "build/tildebackend.a";
#endif
	}
	
	// don't wanna buffer stdout
	setvbuf(stdout, NULL, _IONBF, 0);
	
#if defined(_WIN32)
	// sets environment vars for compiler
	system("call vcvars64");
#endif
	
#if defined(__GNUC__)
	printf("Compiling on GCC %d.%d...\n", __GNUC__, __GNUC_MINOR__);
	compile_with_cc("gcc");
#elif defined(__clang__)
	printf("Compiling on Clang %d.%d.%d...\n", __clang_major__, __clang_minor__, __clang_patchlevel__);
	compile_with_cc("clang");
#elif defined(_MSC_VER)
	printf("Compiling on MSVC %d...\n", _MSC_VER);
	compile_with_cl();
#else
	printf("Compiling on unknown compiler (cc)\n");
	compile_with_cc("cc");
#endif
	
#if !defined(BUILD_FUZZER)
	printf("Outputting to: %s...\n", output_lib_path);
	
#if defined(NEGATE)
	// personal crap
	system("cd W:/Workspace/Cuik/ && clang compile.c -o compile.exe && compile.exe");
#endif
	
#else
	printf("Outputting fuzzer to: build/fuzzer.exe...\n");
#endif
	
	return 0;
}
