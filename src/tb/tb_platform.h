#pragma once

void* tb_platform_valloc(size_t size);
void tb_platform_vfree(void* ptr, size_t size);
bool tb_platform_vprotect(void* ptr, size_t size, bool execute_read);
// NOTE(NeGate): It's either execute-read or read-write, no other options... yet
// returns false, if it failed

void* tb_platform_heap_alloc(size_t size);
void* tb_platform_heap_realloc(void* ptr, size_t size);
void tb_platform_heap_free(void* ptr);

char* tb_platform_string_alloc(const char* str);
void tb_platform_string_free();
// frees all strings allocated
