#include "tb_internal.h"
#include <stdalign.h>

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
	return malloc(size);
}

void* tb_platform_heap_realloc(void* ptr, size_t size) {
	return realloc(ptr, size);
}

void tb_platform_heap_free(void* ptr) {
	free(ptr);
}

static char* string_buffer;
static _Atomic size_t string_head;

char* tb_platform_string_alloc(const char* str) {
	if (!string_buffer) {
		string_buffer = tb_platform_valloc(64 << 20);
	}
	
	size_t len = strlen(str) + 1;
	size_t pos = atomic_fetch_add(&string_head, len);
	
	char* new_str = &string_buffer[pos];
	strcpy(new_str, str);
	
	return new_str;
}

void tb_platform_string_free() {
	tb_platform_vfree(string_buffer, 64 << 20);
	string_buffer = NULL;
}

////////////////////////////////
// Persistent arena allocator
////////////////////////////////
#define ARENA_SEGMENT_SIZE (4 * 1024 * 1024)

// It's a linked list :)
typedef struct Segment {
	struct Segment* next;
	size_t used;
	unsigned char data[];
} Segment;

static Segment* arena_base;
static Segment* arena_top;

// weird bootleg mutex because i dont get threads.h on windows :(
static atomic_int arena_lock = 0;

void tb_platform_arena_init() {
	Segment* s = (Segment*)VirtualAlloc(NULL, ARENA_SEGMENT_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
	if (!s) abort();
	
	arena_base = arena_top = s;
}

void* tb_platform_arena_alloc(size_t size) {
	// align to max_align
	size_t align_mask = _Alignof(max_align_t)-1;
	size = (size + align_mask) & align_mask;
	
	// If this ever happens... literally how...
	assert(size < ARENA_SEGMENT_SIZE);
	
	// lock
	int expected = 0;
	while (!atomic_compare_exchange_strong(&arena_lock, &expected, 1)) {}
	
	void* ptr;
	if (arena_top->used + size - sizeof(Segment) < ARENA_SEGMENT_SIZE) {
		ptr = &arena_top->data[arena_top->used];
		arena_top->used += size;
	}
	else {
		// Add new page
		Segment* s = (Segment*)VirtualAlloc(NULL, ARENA_SEGMENT_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
		if (!s) {
			printf("Out of memory!\n");
			abort();
		}
		
		s->next = NULL;
		s->used = size;
		ptr = s->data;
		
		// Insert to top of nodes
		arena_top->next = s;
		arena_top = s;
	}
	
	// unlock
	arena_lock = 0;
	return ptr;
}

void tb_platform_arena_free() {
	Segment* c = arena_base;
	while (c) {
		Segment* next = c->next;
		VirtualFree(c, 0, MEM_RELEASE);
		c = next;
	}
	
	arena_base = arena_top = NULL;
}
