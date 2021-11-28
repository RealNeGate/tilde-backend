#include "tb_internal.h"

void* tb_platform_valloc(size_t size) {
	return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void tb_platform_vfree(void* ptr, size_t size) {
	munmap(ptr, size);
}

bool tb_platform_vprotect(void* ptr, size_t size, bool execute) {
	return mprotect(ptr, size, PROT_READ | (execute ? PROT_EXEC : PROT_WRITE)) == 0;
}

