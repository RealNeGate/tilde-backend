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

