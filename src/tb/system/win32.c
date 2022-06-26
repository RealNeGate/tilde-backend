#include "../tb_internal.h"
#include "../scalable_allocator.h"

void* tb_platform_valloc(size_t size) {
    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void* tb_platform_valloc_guard(size_t size) {
	void* ptr = VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE | PAGE_GUARD);
	if (ptr == NULL) return NULL;

	SYSTEM_INFO sys_info;
	GetSystemInfo(&sys_info);

	// mark the last page as a noaccess, this means it should segfault safely on running out of memory
	DWORD old_protect;
    VirtualProtect((char*)ptr + (size - sys_info.dwPageSize), sys_info.dwPageSize, PAGE_NOACCESS, &old_protect);

	return ptr;
}

void tb_platform_vfree(void* ptr, size_t size) {
	VirtualFree(ptr, 0, MEM_RELEASE);
}

bool tb_platform_vprotect(void* ptr, size_t size, bool execute) {
    DWORD old_protect;
    return VirtualProtect(ptr, size, execute ? PAGE_EXECUTE_READ : PAGE_READWRITE, &old_protect);
}

void* tb_platform_heap_alloc(size_t size) {
	void* ptr = scalable_malloc(size);
	if (ptr == NULL) tb_panic("out of memory!");
	return ptr;
}

void* tb_platform_heap_realloc(void* ptr, size_t size) {
	return scalable_realloc(ptr, size);
}

void tb_platform_heap_free(void* ptr) {
	scalable_free(ptr);
}

static char*  string_buffer;
static size_t string_head;

char* tb_platform_string_alloc(const char* str) {
    if (!string_buffer) {
		string_buffer = tb_platform_valloc(32 << 20);
	}

    size_t len = strlen(str);
    size_t pos = tb_atomic_size_add(&string_head, len + 1);

    char* new_str = &string_buffer[pos];
    memcpy(new_str, str, len);
    new_str[len] = '\0';
    return new_str;
}

void tb_platform_string_free() {
    tb_platform_vfree(string_buffer, 32 << 20);
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

static CRITICAL_SECTION arena_lock;

void tb_platform_arena_init() {
    Segment* s = (Segment*) tb_platform_valloc(ARENA_SEGMENT_SIZE);
    if (!s) abort();

    arena_base = arena_top = s;
    InitializeCriticalSection(&arena_lock);
}

void* tb_platform_arena_alloc(size_t size) {
    // align to max_align
    size_t align_mask = _Alignof(intmax_t) - 1;
    size = (size + align_mask) & ~align_mask;

    // If this ever happens... literally how...
    assert(size < ARENA_SEGMENT_SIZE);

    // lock
    EnterCriticalSection(&arena_lock);

    void* ptr;
    if (arena_top->used + size < ARENA_SEGMENT_SIZE - sizeof(Segment)) {
        ptr = &arena_top->data[arena_top->used];
        arena_top->used += size;
    } else {
        // Add new page
        Segment* s = (Segment*) tb_platform_valloc(ARENA_SEGMENT_SIZE);
        if (!s) {
            printf("Out of memory!\n");
            LeaveCriticalSection(&arena_lock);

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
    LeaveCriticalSection(&arena_lock);
    return ptr;
}

void tb_platform_arena_free() {
    Segment* c = arena_base;
    while (c) {
        Segment* next = c->next;
        tb_platform_vfree(c, ARENA_SEGMENT_SIZE);
        c = next;
    }

    arena_base = arena_top = NULL;
}