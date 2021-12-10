#include "tb_internal.h"

void* tb_platform_valloc(size_t size) {
	return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void tb_platform_vfree(void* ptr, size_t size) {
	VirtualFree(ptr, 0, MEM_RELEASE);
}

bool tb_platform_vprotect(void* ptr, size_t size, bool execute) {
	DWORD old_protect;
	return VirtualProtect(ptr, size, execute ? PAGE_EXECUTE_READ : PAGE_READWRITE, &old_protect);
}

void* tb_platform_heap_alloc(size_t size) {
	//printf("Heap alloc: %zu\n", size);
	return malloc(size);
}

void* tb_platform_heap_realloc(void* ptr, size_t size) {
	//printf("Heap realloc: %zu\n", size);
	return realloc(ptr, size);
}

void tb_platform_heap_free(void* ptr) {
	//printf("Heap free\n");
	free(ptr);
}

static char* string_buffer;
static _Atomic size_t string_head;

char* tb_platform_string_alloc(const char* str) {
	if (!string_buffer) {
		string_buffer = VirtualAlloc(NULL,
									 64 << 20,
									 MEM_RESERVE | MEM_COMMIT,
									 PAGE_READWRITE);
	}
	
	size_t len = strlen(str) + 1;
	size_t pos = atomic_fetch_add(&string_head, len);

	char* new_str = &string_buffer[pos];
	strcpy(new_str, str);

	return new_str;
}

void tb_platform_string_free() {
	tb_platform_vfree(string_buffer, 64 << 20);
}

