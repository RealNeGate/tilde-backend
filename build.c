// This is the build script for TB, real simple imo
// just call it with the C11 compiler of your choice
//
// It's inspired by nobuild but different
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#ifdef _WIN32
#define WIN32_MEAN_AND_LEAN
#include "windows.h"

#define SLASH "\\"
#define PATH_MAX MAX_PATH
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

#define SLASH "/"
#endif

bool str_ends_with(const char* cstr, const char* postfix) {
    const size_t cstr_len = strlen(cstr);
    const size_t postfix_len = strlen(postfix);
	
    return postfix_len <= cstr_len && strcmp(cstr + cstr_len - postfix_len, postfix) == 0;
}

const char* str_no_ext(const char* path) {
    size_t n = strlen(path);
    while (n > 0 && path[n - 1] != '.') {
        n -= 1;
    }
	
    if (n > 0) {
        char* result = malloc(n);
        memcpy(result, path, n);
        result[n - 1] = '\0';
		
        return result;
    } else {
        return path;
    }
}

#ifdef _WIN32
// https://bobobobo.wordpress.com/2009/02/02/getlasterror-and-getlasterrorasstring/
LPSTR GetLastErrorAsString(void) {
	LPSTR buf = NULL;
	
	int result = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, 
							   0, GetLastError(), 0, (LPSTR)&buf, 0, 0);
	
	return buf;
}

typedef struct FileIter {
    HANDLE find_handle;
    WIN32_FIND_DATA find_data;
} FileIter;

static FileIter file_iter_open(const char* directory) {
    char buffer[PATH_MAX];
    snprintf(buffer, PATH_MAX, "%s\\*", directory);
	
	FileIter iter = { 0 };
    iter.find_handle = FindFirstFile(buffer, &iter.find_data);
	if (iter.find_handle == INVALID_HANDLE_VALUE) {
		printf("File iterator failed to open!!!\n");
		abort();
	}
	
	return iter;
}

static char* file_iter_next(FileIter* iter) {
	if(!FindNextFile(iter->find_handle, &iter->find_data)) {
		if (GetLastError() != ERROR_NO_MORE_FILES) {
			printf("File iterator failed to iterate!!!\n");
			abort();
		}
		
		return NULL;
	}
	
	return iter->find_data.cFileName;
}

static void file_iter_close(FileIter* iter) {
    if (!FindClose(iter->find_handle)) {
		printf("File iterator failed to close!!!\n");
		abort();
	}
}
#else
typedef struct FileIter {
    DIR* dir;
} FileIter;

static FileIter file_iter_open(const char* directory) {
    return (FileIter){ opendir(directory) };
}

static char* file_iter_next(FileIter* iter) {
	struct dirent* dp = readdir(iter->dir);
	return dp->d_name;
}

static void file_iter_close(FileIter* iter) {
    closedir(iter->dir);
}
#endif

static char command_buffer[4096];
static size_t command_length = 0;

static void cmd_append(const char* str) {
	size_t l = strlen(str);
	assert(command_length + l + 1 < sizeof(command_buffer));
	
	memcpy(&command_buffer[command_length], str, l + 1);
	command_length += l;
}

#ifdef _WIN32
static void cmd_run() {
    STARTUPINFO siStartInfo = {
		.cb = sizeof(STARTUPINFO),
		.hStdError = GetStdHandle(STD_ERROR_HANDLE),
		.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE),
		.hStdInput = GetStdHandle(STD_INPUT_HANDLE),
		.dwFlags = STARTF_USESTDHANDLES
	};
	
    PROCESS_INFORMATION piProcInfo = { 0 };
    BOOL bSuccess = CreateProcess(NULL, command_buffer, NULL, NULL, TRUE,
								  0, NULL, NULL, &siStartInfo, &piProcInfo);
	
    if (!bSuccess) {
        printf("Could not create child process %s: %s\n", command_buffer, GetLastErrorAsString());
		abort();
    }
	
	
    DWORD result = WaitForSingleObject(piProcInfo.hProcess, INFINITE);
    if (result == WAIT_FAILED) {
        printf("could not wait on child process: %s\n", GetLastErrorAsString());
		abort();
    }
	
    DWORD exit_status;
    if (GetExitCodeProcess(piProcInfo.hProcess, &exit_status) == 0) {
        printf("could not get process exit code: %s\n", GetLastErrorAsString());
		abort();
    }
	
	if (exit_status) {
		printf("exited with code: %lu\n", exit_status);
		abort();
	}
	
    CloseHandle(piProcInfo.hThread);
    CloseHandle(piProcInfo.hProcess);
	
	command_buffer[0] = 0;
	command_length = 0;
}
#else
static void cmd_run() {
	int exit_status = system(command_buffer);
	if (exit_status != 0) {
		printf("command exited with exit code %d\n", exit_status);
		abort();
	}
	
	command_buffer[0] = 0;
	command_length = 0;
}
#endif

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
	////////////////////////////////
	// Run Compiler
	////////////////////////////////
	cmd_append("cl /c /MP /Fo:build\\ ");
	
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
	
	// x64 target module
	cmd_append("src\\tb\\x64\\x64.c ");
	
	// compiler settings
#if defined(RELEASE_BUILD)
	cmd_append("/arch:AVX /GL /Ox /WX /GS- /DNDEBUG /D_CRT_SECURE_NO_WARNINGS");
#else
	cmd_append("/arch:AVX /MTd /Od /WX /Zi /D_DEBUG /RTC1 /D_CRT_SECURE_NO_WARNINGS");
#endif
	
	cmd_run();
	
	////////////////////////////////
	// Run Linker
	////////////////////////////////
	cmd_append("lib /out:");
	cmd_append(output_lib_path);
	cmd_append(" build\\*.obj");
	cmd_run();
	
	delete_intermediates(".obj");
}

static void compile_file_with_cc(const char* cc_command, const char* directory, const char* input, const char* output) {
	cmd_append(cc_command);
	cmd_append(" -march=nehalem -Werror -Wall -Wno-unused-function -g -c ");
	
#if _WIN32
	cmd_append("-D_CRT_SECURE_NO_WARNINGS -gcodeview ");
#endif
	
#if defined(RELEASE_BUILD)
	cmd_append("-O2 -DNDEBUG ");
#else
	cmd_append("-O0 -D_DEBUG ");
#endif
	
	cmd_append(directory);
	cmd_append(input);
	cmd_append(" -c -o build" SLASH);
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
			compile_file_with_cc(cc_command, "src" SLASH "tb" SLASH, path, NULL);
		}
	}
	file_iter_close(&it);
	
	// optimizer
	it = file_iter_open("src" SLASH "tb" SLASH "opt");
	while ((path = file_iter_next(&it))) {
		// ignore tb_posix on MSVC compiles
		if (str_ends_with(path, ".c")) {
			compile_file_with_cc(cc_command, "src" SLASH "tb" SLASH "opt" SLASH, path, NULL);
		}
	}
	file_iter_close(&it);
	
	// x64 target module
	compile_file_with_cc(cc_command, "src" SLASH "tb" SLASH, "x64" SLASH "x64.c", "x64");
	
	////////////////////////////////
	// Run Linker
	////////////////////////////////
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
	
	printf("Outputting to: %s...\n", output_lib_path);
	return 0;
}
