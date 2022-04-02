#include "tb_coff.h"

// let's ignore error handling for now :p
// buffered reading i guess?
TB_ObjectFile* tb_object_parse_coff(FILE* file) {
    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fclose(file);
        return NULL;
    }

    COFF_FileHeader header;
    fread(&header, 1, sizeof(COFF_FileHeader), file);

    TB_ObjectFile* obj_file =
        malloc(sizeof(TB_ObjectFile) + (header.num_sections * sizeof(TB_ObjectSection)));
    memset(obj_file, 0, sizeof(TB_ObjectFile));

    obj_file->type = TB_OBJECT_FILE_COFF;

    switch (header.machine) {
    case COFF_MACHINE_AMD64: obj_file->arch = TB_ARCH_X86_64; break;
    case COFF_MACHINE_ARM64: obj_file->arch = TB_ARCH_AARCH64; break;
    default: tb_todo();
    }

    size_t string_table_pos  = header.symbol_table + (header.symbol_count * sizeof(COFF_Symbol));
    size_t string_table_size = file_stats.st_size - string_table_pos;

    // Read string table
    {
        obj_file->string_table_size = string_table_size;
        obj_file->string_table      = malloc(string_table_size);

        fseek(file, string_table_pos, SEEK_SET);
        fread(obj_file->string_table, string_table_size, 1, file);
    }

    obj_file->symbols      = malloc(header.symbol_count * sizeof(TB_ObjectSymbol));
    obj_file->symbol_count = 0;

    size_t sym_id = 0;
    while (sym_id < header.symbol_count) {
        size_t symbol_offset = header.symbol_table + (sym_id * sizeof(COFF_Symbol));

        COFF_Symbol sym;
        fseek(file, symbol_offset, SEEK_SET);
        fread(&sym, 1, sizeof(COFF_Symbol), file);

        TB_ObjectSymbol* out_sym = &obj_file->symbols[obj_file->symbol_count++];
        *out_sym                 = (TB_ObjectSymbol) { 0 };

        // Parse string table name stuff
        if (sym.long_name[0] == 0) {
            // string table access (read a cstring)
            // TODO(NeGate): bounds check this
            out_sym->name = &obj_file->string_table[sym.long_name[1]];
        } else {
            // normal inplace string
            out_sym->name = malloc(9);

            memcpy(out_sym->name, sym.short_name, sizeof(uint8_t[8]));
            out_sym->name[8] = '\0';
        }

        // Process aux symbols
        loop(j, sym.aux_symbols_count) {
            // TODO(NeGate): idk do something
        }

        sym_id += sym.aux_symbols_count + 1;
    }
    // trim the symbol table
    obj_file->symbols =
        realloc(obj_file->symbols, obj_file->symbol_count * sizeof(TB_ObjectSymbol));

    obj_file->section_count = header.num_sections;
    loop(i, header.num_sections) {
        // TODO(NeGate): bounds check this
        size_t section_offset = sizeof(COFF_FileHeader) + (i * sizeof(COFF_SectionHeader));

        COFF_SectionHeader sec;
        fseek(file, section_offset, SEEK_SET);
        fread(&sec, 1, sizeof(COFF_SectionHeader), file);

        TB_ObjectSection* out_sec = &obj_file->sections[i];
        *out_sec                  = (TB_ObjectSection) { 0 };

        // Parse string table name stuff
        uint32_t long_name[2];
        memcpy(long_name, sec.name, sizeof(uint8_t[8]));
        if (long_name[0] == 0) {
            // string table access
            tb_todo();
        } else {
            // normal inplace string
            out_sec->name = malloc(9);

            memcpy(out_sec->name, sec.name, sizeof(uint8_t[8]));
            out_sec->name[8] = '\0';
        }

        // Parse virtual region
        out_sec->virtual_address = sec.virtual_address;
        out_sec->virtual_size    = sec.misc.virtual_size;

        // Read raw data (if applies)
        if (sec.raw_data_size) {
            out_sec->raw_data_size = sec.raw_data_size;
            out_sec->raw_data      = malloc(sec.raw_data_size);

            // We should probably bounds check something here? idk
            fseek(file, sec.raw_data_pos, SEEK_SET);
            fread(out_sec->raw_data, 1, sec.raw_data_size, file);
        }
    }

    return obj_file;
}
