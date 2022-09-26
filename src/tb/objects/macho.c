#include "macho.h"

void tb_export_macho(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, const IDebugFormat* debug_fmt) {
    //TB_TemporaryStorage* tls = tb_tls_allocate();

    TB_Emitter string_table = { 0 };

    MO_Header64 header = {
        .magic    = MH_MAGIC_64,
        .filetype = MH_OBJECT,
        .ncmds    = 2,
        .flags    = 0x2000
    };

    // fill in CPU type and subtype based on target
    switch (m->target_arch) {
        case TB_ARCH_X86_64:  header.cputype = CPU_TYPE_X86_64; header.cpusubtype = 3; break;
        case TB_ARCH_AARCH64: header.cputype = CPU_TYPE_AARCH64; header.cpusubtype = 0; break;
        default: tb_todo();
    }

    // function layout
    size_t text_section_size = tb_helper_get_text_section_layout(m, 0);

    // segments
    MO_Section64 sections[] = {
        {
            .sectname = { "__text" },
            .segname  = { "__TEXT" },
            .align    = 2,
            .size     = text_section_size,
            .flags    = S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS
        }
    };
    enum { NUMBER_OF_SECTIONS = COUNTOF(sections) };

    MO_SegmentCmd64 segment_cmd = {
        .header = {
            .cmd = LC_SEGMENT_64,
            .cmdsize = sizeof(MO_SegmentCmd64) + sizeof(MO_Section64)*NUMBER_OF_SECTIONS,
        },
        .nsects = NUMBER_OF_SECTIONS
    };

    // generate symbol table
    MO_SymtabCmd symtab_cmd = {
        .header = {
            .cmd     = LC_SYMTAB,
            .cmdsize = sizeof(MO_SymtabCmd)
        }
    };

    // count symbols
    {
        symtab_cmd.nsyms += m->compiled_function_count;

        FOREACH_N(i, 0, m->max_threads) {
            size_t external_len = pool_popcount(m->thread_info[i].externals);
            symtab_cmd.nsyms += external_len ? external_len - 1 : 0;
        }

        FOREACH_N(i, 0, m->max_threads) {
            symtab_cmd.nsyms += pool_popcount(m->thread_info[i].globals);
        }
    }

    //size_t load_cmds_start = sizeof(MO_Header64);

    fprintf(stderr, "TB warning: Mach-O output isn't ready yet :p sorry\n");

    FILE* f = fopen(path, "wb");
    fwrite(&header, sizeof(header), 1, f);
    fwrite(&symtab_cmd, sizeof(symtab_cmd), 1, f);
    fwrite(&segment_cmd, sizeof(segment_cmd), 1, f);
    fwrite(&sections, sizeof(MO_Section64), NUMBER_OF_SECTIONS, f);

    // emit section contents
    FOREACH_N(i, 0, NUMBER_OF_SECTIONS) {

    }

    //fwrite(string_table.data, string_table.count, 1, f);
    fclose(f);

    tb_platform_heap_free(string_table.data);
}
