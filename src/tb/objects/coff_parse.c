#include "coff.h"

typedef struct {
    TB_Slice file;
    size_t pos;
} FileReader;

typedef struct {
    const char* name;
    int date;
    int mode;
    int size;
} ArchiveEntry;

typedef struct {
    char name[16];
    char date[12];

    // Microsoft tools don't actually do anything with this
    char user_id[6];
    char group_id[6];

    char mode[8];
    char size[10];

    uint8_t newline[2];
    uint8_t contents[];
} COFF_ArchiveMemberHeader;

// section related crap likes to be sorted in lexical order :p
static int compare_sections(const void* a, const void* b) {
    const TB_ObjectSection* sec_a = (const TB_ObjectSection*)a;
    const TB_ObjectSection* sec_b = (const TB_ObjectSection*)b;

    size_t shortest_len = sec_a->name.length < sec_b->name.length ? sec_a->name.length : sec_b->name.length;
    return memcmp(sec_a->name.data, sec_b->name.data, shortest_len);
}

static long long parse_decimal_int(size_t n, const char* str) {
    const char* end = &str[n];

    int result = 0;
    while (str != end) {
        if (*str < '0' || *str > '9') break;

        result *= 10;
        result += *str - '0';
        str++;
    }

    return result;
}

static uint32_t read32be(uint8_t* ptr) {
    return (ptr[0] << 24u) | (ptr[1] << 16u) | (ptr[2] << 8u) | (ptr[3]);
}

TB_ArchiveFile* tb_archive_parse_lib(const TB_Slice file) {
    uint8_t* signature = &file.data[0];
    if (memcmp(signature, "!<arch>\n", 8) != 0) {
        // TODO(NeGate): maybe we should make a custom error stream...
        fprintf(stderr, "TB archive parser: invalid .lib header!\n");
        return NULL;
    }

    size_t symbol_count = 0;
    uint32_t* symbols = NULL;
    TB_Slice strtbl = { 0 };

    DynArray(struct TB_NamedObjectPair) objs = dyn_array_create(struct TB_NamedObjectPair, 256);

    // Process first, second and long name members
    size_t file_offset = 8;
    {
        COFF_ArchiveMemberHeader* first = (COFF_ArchiveMemberHeader*) &file.data[file_offset];
        if (memcmp(first->name, (char[16]) { "/               " }, 16) != 0) {
            fprintf(stderr, "TB archive parser: first archive member name is invalid\n");
            return NULL;
        }
        size_t first_content_length = parse_decimal_int(sizeof(first->size), first->size);

        // Extract number of symbols & string table
        assert(first_content_length >= 8);
        symbol_count = read32be(&first->contents[0]);
        symbols = (uint32_t*) &first->contents[4];

        size_t string_table_pos = 8 + (symbol_count * sizeof(uint32_t));
        assert(first_content_length >= string_table_pos);
        ((void)string_table_pos);

        /*string_table = (TB_Slice){
            .length = first_content_length - string_table_pos,
            .data   = &first->contents[string_table_pos]
        };*/

        file_offset += sizeof(COFF_ArchiveMemberHeader) + first_content_length;
        file_offset = (file_offset + 1u) & ~1u;

        // Process second member
        COFF_ArchiveMemberHeader* second = (COFF_ArchiveMemberHeader*) &file.data[file_offset];
        if (memcmp(second->name, (char[16]) { "/               " }, 16) != 0) {
            fprintf(stderr, "TB archive parser: second archive member name is invalid\n");
            return NULL;
        }
        size_t second_content_length = parse_decimal_int(sizeof(second->size), second->size);

        // Advance
        file_offset += sizeof(COFF_ArchiveMemberHeader) + second_content_length;
        file_offset = (file_offset + 1u) & ~1u;

        // Process long name member
        COFF_ArchiveMemberHeader* longnames = (COFF_ArchiveMemberHeader*) &file.data[file_offset];
        if (memcmp(longnames->name, (char[16]) { "//              " }, 16) == 0) {
            size_t longname_content_length = parse_decimal_int(sizeof(second->size), second->size);
            strtbl = (TB_Slice){ longname_content_length, longnames->contents };

            // Advance
            file_offset += sizeof(COFF_ArchiveMemberHeader) + longname_content_length;
            file_offset = (file_offset + 1u) & ~1u;
        }
    }

    TB_ArchiveFile* archive = tb_platform_heap_alloc(sizeof(TB_ArchiveFile));
    *archive = (TB_ArchiveFile){ 0 };

    // dynamic array of imports
    size_t import_capacity = 256, import_count = 0;
    TB_ArchiveImport* imports = tb_platform_heap_alloc(import_capacity * sizeof(TB_ArchiveImport));

    for (size_t i = 0; i < symbol_count; i++) {
        size_t sym_i = read32be((uint8_t*) &symbols[i]);
        COFF_ArchiveMemberHeader* sym = (COFF_ArchiveMemberHeader*) &file.data[sym_i];
        size_t len = parse_decimal_int(sizeof(sym->size), sym->size);

        TB_Slice sym_name = { strchr(sym->name, ' ') - sym->name, (uint8_t*) sym->name };
        if (sym_name.data[0] == '/') {
            size_t num = parse_decimal_int(sym_name.length - 1, (char*)sym_name.data + 1);
            sym_name = (TB_Slice){ strlen((const char*) &strtbl.data[num]), &strtbl.data[num] };
        }

        uint32_t short_form_header = *(uint32_t*)sym->contents;
        if (short_form_header == 0xFFFF0000) {
            COFF_ImportHeader* import = (COFF_ImportHeader*) sym->contents;
            if (import->name_type != 0 && import->name_type != 1) {
                tb_todo();
            }

            /*switch (import->type) {
                case 0: printf("IMPORT_CODE  ");  break;
                case 1: printf("IMPORT_DATA  ");  break;
                case 2: printf("IMPORT_CONST "); break;
                default: printf("??\n");          break;
            }*/

            const char* imported_symbol = (const char*) &sym->contents[sizeof(COFF_ImportHeader)];
            const char* dll_path = (const char*) &sym->contents[sizeof(COFF_ImportHeader) + strlen(imported_symbol) + 1];

            // add import
            if (import_count + 1 > import_capacity) {
                import_capacity = (import_count + 1) * 2;
                imports = realloc(imports, import_capacity * sizeof(TB_ArchiveImport));
                if (imports == NULL) tb_panic("out of memory!");
            }

            imports[import_count++] = (TB_ArchiveImport){
                .libname = dll_path, .name = imported_symbol
            };
            //printf("%s : %s\n", dll_path, imported_symbol);
        } else if (1) {
            TB_ObjectFile* long_mode = tb_object_parse_coff((TB_Slice){ len, sym->contents });

            // Sort by lexical order
            qsort(long_mode->sections, long_mode->section_count, sizeof(TB_ObjectSection), compare_sections);

            // Join .idata sections together
            bool matching_idata = false;

            size_t idata_length = 0;
            uint8_t* idata = NULL;

            for (size_t i = 0; i < long_mode->section_count; i++) {
                TB_ObjectSection* sec = &long_mode->sections[i];

                if (sec->name.length >= sizeof(".idata")-1 &&
                    memcmp(sec->name.data, ".idata", sizeof(".idata")-1) == 0) {
                    if (matching_idata) {
                        //printf("\n\nLong mode:\n");
                    }
                    matching_idata = true;

                    //printf("  Joining %.*s\n", (int)sec->name.length, sec->name.data);

                    // Join onto the idata stream
                    idata = realloc(idata, idata_length + sec->raw_data.length);
                    tb_assert(idata != NULL, "Fuck... we ran out of space...");

                    memcpy(&idata[idata_length], sec->raw_data.data, sec->raw_data.length);
                    idata_length += sec->raw_data.length;
                } else {
                    // once we stop matching idata sections after matching it's over since
                    // it's sorted so they're all contigous
                    if (matching_idata) break;
                }
            }

            if (idata_length) {
                // iterate all import directories and extract imports
                /*COFF_ImportDirectory* dirs = (COFF_ImportDirectory*) idata;

                size_t i = 0;
                for (; dirs[i].import_lookup_table != 0; i++) {
                    printf("  IDT:\n");
                    printf("    Import Lookup table: 0x%x\n", dirs[i].import_lookup_table);
                    printf("    Time stamp:          0x%x\n", dirs[i].timestamp);
                    printf("    Forwarder chain:     0x%x\n", dirs[i].forwarder_chain);
                    printf("    Name RVA:            0x%x\n", dirs[i].name);
                    printf("    Import adress table: 0x%x\n", dirs[i].import_address_table);

                    if (dirs[i].import_lookup_table) {
                        // NOTE(NeGate): PE32+ uses 64bit entries and PE32 uses 32bit, we'll
                        // be assuming 64bit for now
                        // assert(file_offset % 8 == 0);
                        uint64_t* import_lookup_table = (uint64_t*) &idata[dirs[i].import_lookup_table];

                        printf("    Lookup:\n");
                        for (size_t j = 0; import_lookup_table[j] != 0; j++) {
                            printf("      0x%016llx\n", (long long)import_lookup_table[j]);
                        }
                    }
                }*/

                tb_object_free(long_mode);
            } else {
                //printf(" %.*s (%zu bytes)\n", (int)sym_name.length, sym_name.data, len);

                struct TB_NamedObjectPair o = { sym_name, long_mode };
                dyn_array_put(objs, o);
            }
        }
    }

    archive->object_file_count = dyn_array_length(objs);
    archive->object_files = objs;

    archive->import_count = import_count;
    archive->imports = imports;
    return archive;
}

// let's ignore error handling for now :p
// buffered reading i guess?
TB_ObjectFile* tb_object_parse_coff(const TB_Slice file) {
    COFF_FileHeader* header = (COFF_FileHeader*) &file.data[0];

    TB_ObjectFile* obj_file = malloc(sizeof(TB_ObjectFile) + (header->num_sections * sizeof(TB_ObjectSection)));

    // not using calloc since i only really wanna clear the header
    memset(obj_file, 0, sizeof(TB_ObjectFile));
    obj_file->type = TB_OBJECT_FILE_COFF;

    switch (header->machine) {
        case COFF_MACHINE_AMD64: obj_file->arch = TB_ARCH_X86_64; break;
        case COFF_MACHINE_ARM64: obj_file->arch = TB_ARCH_AARCH64; break;
        default: obj_file->arch = TB_ARCH_UNKNOWN; break;
    }

    size_t string_table_pos = header->symbol_table + (header->symbol_count * sizeof(COFF_Symbol));

    // Read string table
    TB_Slice string_table = {
        .length = file.length - string_table_pos,
        .data   = &file.data[string_table_pos]
    };

    obj_file->symbols = malloc(header->symbol_count * sizeof(TB_ObjectSymbol));
    obj_file->symbol_count = 0;

    size_t sym_id = 0;
    while (sym_id < header->symbol_count) {
        size_t symbol_offset = header->symbol_table + (sym_id * sizeof(COFF_Symbol));
        COFF_Symbol* sym = (COFF_Symbol*) &file.data[symbol_offset];

        TB_ObjectSymbol* out_sym = &obj_file->symbols[obj_file->symbol_count++];
        *out_sym = (TB_ObjectSymbol) { 0 };

        // Parse string table name stuff
        if (sym->long_name[0] == 0) {
            // string table access (read a cstring)
            // TODO(NeGate): bounds check this
            uint8_t* data = &string_table.data[sym->long_name[1]];
            out_sym->name = (TB_Slice){ strlen((const char*) data), data };
        } else {
            // normal inplace string
            size_t len = strlen((const char*) sym->short_name);
            out_sym->name = (TB_Slice){ len, sym->short_name };
        }

        // Process aux symbols
        FOREACH_N(j, 0, sym->aux_symbols_count) {
            // TODO(NeGate): idk do something
        }

        sym_id += sym->aux_symbols_count + 1;
    }

    // trim the symbol table
    obj_file->symbols = realloc(obj_file->symbols, obj_file->symbol_count * sizeof(TB_ObjectSymbol));

    obj_file->section_count = header->num_sections;
    FOREACH_N(i, 0, header->num_sections) {
        // TODO(NeGate): bounds check this
        size_t section_offset = sizeof(COFF_FileHeader) + (i * sizeof(COFF_SectionHeader));
        COFF_SectionHeader* sec = (COFF_SectionHeader*) &file.data[section_offset];

        TB_ObjectSection* restrict out_sec = &obj_file->sections[i];
        *out_sec = (TB_ObjectSection) { .flags = sec->characteristics };

        // Parse string table name stuff
        uint32_t long_name[2];
        memcpy(long_name, sec->name, sizeof(uint8_t[8]));
        if (long_name[0] == 0) {
            // string table access
            tb_todo();
        } else {
            // normal inplace string
            size_t len = strlen(sec->name);
            out_sec->name = (TB_Slice){ len, (uint8_t*) sec->name };
        }

        // Parse relocations
        if (sec->num_reloc > 0) {
            out_sec->relocation_count = sec->num_reloc;
            COFF_ImageReloc* src_relocs = (COFF_ImageReloc*) &file.data[sec->pointer_to_reloc];

            TB_ObjectReloc* dst_relocs = malloc(sec->num_reloc * sizeof(TB_ObjectReloc));
            FOREACH_N(j, 0, sec->num_reloc) {
                dst_relocs[j] = (TB_ObjectReloc){ 0 };
                switch (src_relocs[j].Type) {
                    case IMAGE_REL_AMD64_ADDR32NB: dst_relocs[j].type = TB_OBJECT_RELOC_ADDR32NB; break;
                    case IMAGE_REL_AMD64_ADDR32:   dst_relocs[j].type = TB_OBJECT_RELOC_ADDR32; break;
                    case IMAGE_REL_AMD64_ADDR64:   dst_relocs[j].type = TB_OBJECT_RELOC_ADDR64; break;
                    case IMAGE_REL_AMD64_SECREL:   dst_relocs[j].type = TB_OBJECT_RELOC_SECREL; break;
                    case IMAGE_REL_AMD64_SECTION:  dst_relocs[j].type = TB_OBJECT_RELOC_SECTION; break;

                    case IMAGE_REL_AMD64_REL32:
                    case IMAGE_REL_AMD64_REL32_1:
                    case IMAGE_REL_AMD64_REL32_2:
                    case IMAGE_REL_AMD64_REL32_3:
                    case IMAGE_REL_AMD64_REL32_4:
                    case IMAGE_REL_AMD64_REL32_5:
                    dst_relocs[j].type = TB_OBJECT_RELOC_REL32;
                    break;

                    default: tb_todo();
                }

                if (src_relocs[j].Type >= IMAGE_REL_AMD64_REL32 && src_relocs[j].Type <= IMAGE_REL_AMD64_REL32_5) {
                    dst_relocs[j].addend = src_relocs[j].Type - IMAGE_REL_AMD64_REL32;
                }

                dst_relocs[j].symbol_index = src_relocs[j].SymbolTableIndex;
                dst_relocs[j].virtual_address = src_relocs[j].VirtualAddress;
            }

            out_sec->relocations = dst_relocs;
        }

        // Parse virtual region
        out_sec->virtual_address = sec->virtual_address;
        out_sec->virtual_size = sec->misc.virtual_size;

        // Read raw data (if applies)
        if (sec->raw_data_size) {
            assert(sec->raw_data_pos + sec->raw_data_size < file.length);
            out_sec->raw_data = (TB_Slice){ sec->raw_data_size, &file.data[sec->raw_data_pos] };
        }
    }

    return obj_file;
}
