// This is the JIT heap, lower half is code, upper half is data and all the
// management stuff lives outside of it because i don't want it taking up my
// precious space.
#include "../tb_internal.h"

typedef struct {
    // 4GB reserved block, bottom half is executable
    char* block;
} TB_JITHeap;

TB_JITHeap tb_jitheap_create(void);
void* tb_jitheap_alloc_region(TB_JITHeap* c, size_t s, bool is_code);
