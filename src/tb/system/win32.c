#include "../tb_internal.h"
#include <mimalloc.h>
#include <ctype.h>

// NOTE(NeGate): i'm sorry but i had to do it to em
// this is just a random hack around mimalloc needing toupper but having weird
// CRT compat issues
#pragma comment(linker, "/alternatename:__imp_toupper=toupper")

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
    #if USING_DA_ASAN
    void* ptr = malloc(size);
    #else
    void* ptr = mi_malloc(size);
    #endif

    if (ptr == NULL) {
        tb_panic("tb_platform_heap_alloc: out of memory!");
    }
    return ptr;
}

void* tb_platform_heap_realloc(void* ptr, size_t size) {
    #if USING_DA_ASAN
    return realloc(ptr, size);
    #else
    return mi_realloc(ptr, size);
    #endif
}

void tb_platform_heap_free(void* ptr) {
    #if USING_DA_ASAN
    free(ptr);
    #else
    mi_free(ptr);
    #endif
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

typedef struct Arena {
    Segment *base, *top;
} Arena;

static Arena tb__global_arena;
static CRITICAL_SECTION tb__global_arena_lock;

void tb__arena_init(Arena* a) {
    Segment* s = (Segment*) tb_platform_valloc(ARENA_SEGMENT_SIZE);
    if (!s) abort();

    a->base = a->top = s;
}

void* tb__arena_alloc(Arena* a, size_t size) {
    // align to max_align
    size_t align_mask = _Alignof(intmax_t) - 1;
    size = (size + align_mask) & ~align_mask;

    // If this ever happens... literally how...
    assert(size < ARENA_SEGMENT_SIZE);

    void* ptr;
    if (a->top->used + size < ARENA_SEGMENT_SIZE - sizeof(Segment)) {
        ptr = &a->top->data[a->top->used];
        a->top->used += size;
    } else {
        // Add new page
        Segment* s = (Segment*) tb_platform_valloc(ARENA_SEGMENT_SIZE);
        if (!s) {
            fprintf(stderr, "Out of memory!\n");
            abort();
        }

        s->next = NULL;
        s->used = size;
        ptr = s->data;

        // Insert to top of nodes
        a->top->next = s;
        a->top = s;
    }

    // unlock
    return ptr;
}

void tb__arena_free(Arena* a) {
    Segment* c = a->base;
    while (c) {
        Segment* next = c->next;
        tb_platform_vfree(c, ARENA_SEGMENT_SIZE);
        c = next;
    }

    a->base = a->top = NULL;
}

void tb_platform_arena_init() {
    tb__arena_init(&tb__global_arena);
    InitializeCriticalSection(&tb__global_arena_lock);
}

void* tb_platform_arena_alloc(size_t size) {
    EnterCriticalSection(&tb__global_arena_lock);
    void* r = tb__arena_alloc(&tb__global_arena, size);
    LeaveCriticalSection(&tb__global_arena_lock);
    return r;
}

void tb_platform_arena_free() {
    tb__arena_free(&tb__global_arena);
}
