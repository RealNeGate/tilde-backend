#include "coff.h"

//#define NL_STRING_MAP_INLINE
//#define NL_STRING_MAP_IMPL
//#include "../string_map.h"

typedef struct {
    TB_LinkerInput inputs;

    // symbol name -> libpath
    //NL_Strmap(TB_Slice) import_nametable;
} LinkerCtx;

const static uint8_t dos_stub[] = {
    // header
    0x4d,0x5a,0x78,0x00,0x01,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x78,0x00,0x00,0x00,

    // machine code
    0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,0x54,0x68,
    0x69,0x73,0x20,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x20,0x63,0x61,0x6e,0x6e,0x6f,
    0x74,0x20,0x62,0x65,0x20,0x72,0x75,0x6e,0x20,0x69,0x6e,0x20,0x44,0x4f,0x53,0x20,
    0x6d,0x6f,0x64,0x65,0x2e,0x24,0x00,0x00
};

static TB_Slice read_file_into_slice(FILE* file) {
    if (!file) {
        printf("Internal error: could not read file\n");
        abort();
    }

    int descriptor = fileno(file);
    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fclose(file);
        abort();
    }

    fseek(file, 0, SEEK_SET);
    uint8_t* buffer = malloc(file_stats.st_size);
    size_t length_read = fread(buffer, 1, file_stats.st_size, file);

    fclose(file);
    return (TB_Slice){length_read, buffer};
}

static FILE* locate_file(LinkerCtx* restrict ctx, const char* path) {
    FILE* file = fopen(path, "rb");
    if (file) return file;

    char temp_str[FILENAME_MAX];
    loop(i, ctx->inputs.search_dir_count) {
        snprintf(temp_str, FILENAME_MAX, "%s/%s", ctx->inputs.search_dirs[i], path);

        file = fopen(temp_str, "rb");
        if (file) return file;
    }

    return NULL;
}

static void pad_file(FILE* file, char pad, size_t align) {
    size_t align_mask = align - 1;

    long i = ftell(file);
    long end = (i + align_mask) & ~align_mask;
    while (i < end) {
        fwrite(&pad, 1, 1, file);
        i += 1;
    }
}

static void import_archive(LinkerCtx* restrict ctx, TB_ArchiveFile* restrict ar) {
    loop(i, ar->import_count) {
        TB_ArchiveImport* restrict import = &ar->imports[i];

        //TB_Slice libname = { strlen(import->libname), (uint8_t*) import->libname };
        //nl_strmap_put_cstr(ctx->import_nametable, import->name, libname);

        printf("%s : %s\n", import->libname, import->name);
    }
}

void tb_export_pe(TB_Module* m, const ICodeGen* restrict code_gen, const TB_LinkerInput* restrict input, const char* path, const IDebugFormat* debug_fmt) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    LinkerCtx ctx = { .inputs = *input };

    // The prologue and epilogue generators need some storage
    uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc((1+m->functions.count) * sizeof(uint32_t));

    // Generate import lookup table
    loop(i, ctx.inputs.input_count) {
        FILE* file = locate_file(&ctx, ctx.inputs.inputs[i]);
        if (file == NULL) {
            tb_panic("Could not locate file: %s\n", ctx.inputs.inputs[i]);
        }

        printf("Symbols: %s\n", ctx.inputs.inputs[i]);
        TB_Slice buffer = read_file_into_slice(file);

        const char* dot = strrchr(ctx.inputs.inputs[i], '.');
        if (dot && strcmp(dot, ".lib") == 0) {
            TB_ArchiveFile* archive = tb_archive_parse_lib(buffer);
            import_archive(&ctx, archive);

            assert(archive->object_file_count == 0);
        } else {
            tb_todo();
        }
    }

    // Layout functions
    size_t text_section_size = 0;
    size_t entrypoint = SIZE_MAX;
    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        func_layout[i] = text_section_size;
        if (out_f == NULL) continue;

        if (m->functions.data[i].name[0] == 'm') {
            if (strcmp(m->functions.data[i].name, "main") == 0) {
                entrypoint = text_section_size;
            }
        }

        uint64_t meta = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;

        size_t code_size = out_f->code_size;
        size_t prologue = code_gen->get_prologue_length(meta, stack_usage);
        size_t epilogue = code_gen->get_epilogue_length(meta, stack_usage);
        assert(prologue + epilogue < PROEPI_BUFFER);

        text_section_size += prologue;
        text_section_size += epilogue;
        text_section_size += code_size;
    }
    func_layout[m->functions.count] = text_section_size;
    assert(entrypoint != SIZE_MAX);
    (void)entrypoint;

    // Target specific: resolve internal call patches
    code_gen->emit_call_patches(m, func_layout);

    FILE* f = fopen(path, "wb");

    // write DOS header
    fwrite(dos_stub, sizeof(dos_stub), 1, f);

    // write PE header
    PE_Header header = {
        .magic = 0x00004550,
        .machine = 0x8664,
        .section_count = 1,
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .size_of_optional_header = sizeof(PE_OptionalHeader64),
        .characteristics = 2 /* executable */
    };
    fwrite(&header, sizeof(header), 1, f);

    long size_of_headers = ftell(f)
        + sizeof(PE_OptionalHeader64)
        + (header.section_count * sizeof(PE_SectionHeader));

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,

        .size_of_code = (text_section_size + 0xFFF) & ~0xFFF,
        .size_of_initialized_data = 0,
        .size_of_uninitialized_data = 0,

        .base_of_code = 0x1000,

        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x00400000,
        .entrypoint = 0x1000,

        // 6.0
        .major_os_ver = 6,
        .minor_os_ver = 0,

        // 6.0
        .major_subsystem_ver = 6,
        .minor_subsystem_ver = 0,

        .size_of_image = 0x2000,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = 2 /* WINDOWS_GUI */,

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit  = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
    };
    fwrite(&opt_header, sizeof(opt_header), 1, f);

    // generate text section
    {
        long data_pos = ftell(f) + sizeof(PE_SectionHeader);
        data_pos = (data_pos + 0x1FF) & ~0x1FF;

        PE_SectionHeader sec = {
            .name = { ".text" },
            .virtual_size = text_section_size,
            .virtual_address = 0x1000,
            .size_of_raw_data = (text_section_size + 0x1FF) & ~0x1FF,
            .pointer_to_raw_data = data_pos,
            .characteristics = 0x60000020,
        };
        fwrite(&sec, sizeof(sec), 1, f);

        pad_file(f, 0x00, 0x200);

        loop(i, m->functions.count) {
            TB_FunctionOutput* out_f = m->functions.data[i].output;
            if (!out_f) continue;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            const uint8_t* code = out_f->code;
            size_t code_size = out_f->code_size;

            uint8_t* prologue = proepi_buffer;
            size_t prologue_len = code_gen->emit_prologue(prologue, meta, stack_usage);

            uint8_t* epilogue = proepi_buffer + prologue_len;
            size_t epilogue_len = code_gen->emit_epilogue(epilogue, meta, stack_usage);

            fwrite(prologue, prologue_len, 1, f);
            fwrite(code, code_size, 1, f);
            fwrite(epilogue, epilogue_len, 1, f);
        }
        pad_file(f, 0xCC, 0x200);
    }

    fclose(f);
}
