
TB_JITHeap tb_create_jit_heap(void) {
    return (TB_JITHeap){
        .block = tb_platform_valloc(2u << 30u)
    };
}

void tb_jitheap_alloc_region(TB_JITHeap* c, size_t s, bool is_code) {

}
