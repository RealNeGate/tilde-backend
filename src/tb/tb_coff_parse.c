#include "tb_coff.h"

TB_ArchiveFile* tb_archive_parse_lib(const TB_Slice file) {
	uint8_t* signature = &file.data[0];
	if (memcmp(signature, "!<arch>\n", 8) != 0) {
		// TODO(NeGate): maybe we should make a custom error stream...
		fprintf(stderr, "TB archive parser: invalid .lib header!\n");
		return NULL;
	}

	// Process first, second and long name members
	COFF_ArchiveMemberHeader* first = (COFF_ArchiveMemberHeader*) &file.data[8];

	// TODO: shit
	return NULL;
}

// let's ignore error handling for now :p
// buffered reading i guess?
TB_ObjectFile* tb_object_parse_coff(const TB_Slice file) {
    COFF_FileHeader* header = (COFF_FileHeader*) &file.data[0];

    TB_ObjectFile* obj_file =
        malloc(sizeof(TB_ObjectFile) + (header->num_sections * sizeof(TB_ObjectSection)));

	// not using calloc since i only really wanna clear the header
    memset(obj_file, 0, sizeof(TB_ObjectFile));
    obj_file->type = TB_OBJECT_FILE_COFF;

    switch (header->machine) {
    case COFF_MACHINE_AMD64: obj_file->arch = TB_ARCH_X86_64; break;
    case COFF_MACHINE_ARM64: obj_file->arch = TB_ARCH_AARCH64; break;
    default: tb_todo();
    }

    size_t string_table_pos = header->symbol_table + (header->symbol_count * sizeof(COFF_Symbol));

    // Read string table
	TB_Slice string_table = {
		.length = file.length - string_table_pos,
		.data   = &file.data[string_table_pos]
	};

    obj_file->symbols      = malloc(header->symbol_count * sizeof(TB_ObjectSymbol));
    obj_file->symbol_count = 0;

    size_t sym_id = 0;
    while (sym_id < header->symbol_count) {
        size_t symbol_offset = header->symbol_table + (sym_id * sizeof(COFF_Symbol));
        COFF_Symbol* sym = (COFF_Symbol*) &file.data[symbol_offset];

        TB_ObjectSymbol* out_sym = &obj_file->symbols[obj_file->symbol_count++];
        *out_sym                 = (TB_ObjectSymbol) { 0 };

        // Parse string table name stuff
        if (sym->long_name[0] == 0) {
            // string table access (read a cstring)
            // TODO(NeGate): bounds check this
			uint8_t* data = &string_table.data[sym->long_name[1]];
            out_sym->name = (TB_Slice){ strlen(data), data };
        } else {
            // normal inplace string
			size_t len = strlen(sym->short_name);
			out_sym->name = (TB_Slice){ len, sym->short_name }; 
        }

        // Process aux symbols
        loop(j, sym->aux_symbols_count) {
            // TODO(NeGate): idk do something
        }

        sym_id += sym->aux_symbols_count + 1;
    }

    // trim the symbol table
    obj_file->symbols =
        realloc(obj_file->symbols, obj_file->symbol_count * sizeof(TB_ObjectSymbol));

    obj_file->section_count = header->num_sections;
    loop(i, header->num_sections) {
        // TODO(NeGate): bounds check this
        size_t section_offset = sizeof(COFF_FileHeader) + (i * sizeof(COFF_SectionHeader));
        COFF_SectionHeader* sec = (COFF_SectionHeader*) &file.data[section_offset];

        TB_ObjectSection* out_sec = &obj_file->sections[i];
        *out_sec                  = (TB_ObjectSection) { 0 };

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

        // Parse virtual region
        out_sec->virtual_address = sec->virtual_address;
        out_sec->virtual_size    = sec->misc.virtual_size;

        // Read raw data (if applies)
        if (sec->raw_data_size) {
			assert(sec->raw_data_pos + sec->raw_data_size < file.length);
            out_sec->raw_data = (TB_Slice){ sec->raw_data_size, &file.data[sec->raw_data_pos] };
        }
    }

    return obj_file;
}
