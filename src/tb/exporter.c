#include "tb_internal.h"

TB_API TB_ModuleExporter tb_make_exporter(TB_Module* m, TB_OutputFlavor flavor) {
    assert(flavor == TB_FLAVOR_OBJECT);
    switch (m->target_system) {
        case TB_SYSTEM_WINDOWS: return (TB_ModuleExporter){ tb_coff__make(m), tb_coff__next };
        case TB_SYSTEM_LINUX: return (TB_ModuleExporter){ tb_elf64__make(m), tb_elf64__next };
        default: tb_todo();
    }
}

TB_API bool tb_exporter_to_file(TB_Module* m, TB_ModuleExporter exporter, const char* filepath) {
    FILE* file = fopen(filepath, "wb");
    if (file == NULL) {
        return false;
    }

    size_t tmp_capacity = 4 * 1024 * 1024;
    char* tmp_memory = malloc(tmp_capacity);

    TB_ModuleExportPacket packet;
    while (tb_exporter_next(m, exporter, &packet)) {
        switch (packet.type) {
            case TB_EXPORT_PACKET_ALLOC:
            if (packet.alloc.request_size > tmp_capacity) {
                tmp_capacity = (packet.alloc.request_size * 2);
                tmp_memory = realloc(tmp_memory, tmp_capacity);

                // fprintf(stderr, "exporting: resized to %zu\n", tmp_capacity);
                if (tmp_memory == NULL) {
                    return false;
                }
            }

            packet.alloc.memory = tmp_memory;
            break;

            case TB_EXPORT_PACKET_WRITE:
            fwrite(packet.write.data, packet.write.length, 1, file);
            break;

            default: return false;
        }
    }

    free(tmp_memory);
    return true;
}

TB_API uint8_t* tb_exporter_to_buffer(TB_Module* m, TB_ModuleExporter exporter, size_t* out_length) {
    TB_Emitter e = { 0 };

    size_t tmp_capacity = 4 * 1024 * 1024;
    char* tmp_memory = tb_platform_heap_alloc(tmp_capacity);

    TB_ModuleExportPacket packet;
    while (tb_exporter_next(m, exporter, &packet)) {
        switch (packet.type) {
            case TB_EXPORT_PACKET_ALLOC:
            if (packet.alloc.request_size > tmp_capacity) {
                tmp_capacity = (packet.alloc.request_size * 2);
                char* new_mem = tb_platform_heap_realloc(tmp_memory, tmp_capacity);

                if (new_mem == NULL) goto error;
                tmp_memory = new_mem;
            }

            packet.alloc.memory = tmp_memory;
            break;

            case TB_EXPORT_PACKET_WRITE:
            tb_out_reserve(&e, packet.write.length);
            tb_outs_UNSAFE(&e, packet.write.length, packet.write.data);
            break;

            default: goto error;
        }
    }

    tb_platform_heap_free(tmp_memory);

    *out_length = e.count;
    return e.data;

    error:
    if (e.data) tb_platform_heap_free(e.data);
    if (tmp_memory) tb_platform_heap_free(tmp_memory);

    *out_length = -1;
    return NULL;
}

TB_API void tb_exporter_free_buffer(uint8_t* buffer) {
    tb_platform_heap_free(buffer);
}
