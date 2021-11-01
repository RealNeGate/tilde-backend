#define TB_INTERNAL
#include "tb.h"

// IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES
#define COFF_CHARACTERISTICS_TEXT 0x60500020u
// IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ
#define COFF_CHARACTERISTICS_DATA 0xC0000040u
// IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ
#define COFF_CHARACTERISTICS_RODATA 0x40000040u
// IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES
#define COFF_CHARACTERISTICS_BSS 0xC0500080u

#define IMAGE_SYM_CLASS_EXTERNAL      0x0002
#define IMAGE_SYM_CLASS_STATIC        0x0003

#define IMAGE_FILE_LINE_NUMS_STRIPPED 0x0004

#define IMAGE_REL_AMD64_REL32         0x0004

typedef struct COFF_SectionHeader {
	char name[8];
	union {
		uint32_t physical_address;
		uint32_t virtual_size;
	} misc;
	uint32_t  virtual_address;
	uint32_t  raw_data_size;
	uint32_t  raw_data_pos;
	uint32_t  pointer_to_reloc;
	uint32_t  pointer_to_lineno;
	uint16_t  num_reloc;
	uint16_t  num_lineno;
	uint32_t  characteristics;
} COFF_SectionHeader;
_Static_assert(sizeof(COFF_SectionHeader) == 40, "COFF Section header size != 40 bytes");

typedef struct COFF_FileHeader {
	uint16_t machine;
	uint16_t num_sections;
	uint32_t timestamp;
	uint32_t symbol_table;
	uint32_t symbol_count;
	uint16_t optional_header_size;
	uint16_t characteristics;
} COFF_FileHeader;
_Static_assert(sizeof(COFF_FileHeader) == 20, "COFF File header size != 20 bytes");

// NOTE: Symbols, relocations, and line numbers are 2 byte packed
#pragma pack(push,2)
typedef struct COFF_ImageReloc {
	union {
		uint32_t VirtualAddress;
		uint32_t RelocCount;
	};
	uint32_t SymbolTableIndex;
	uint16_t Type;
} COFF_ImageReloc;
_Static_assert(sizeof(COFF_ImageReloc) == 10, "COFF Image Relocation size != 10 bytes");

typedef struct COFF_Symbol {
	union {
		uint8_t short_name[8];
		uint32_t long_name[2];
	};
	uint32_t value;
	int16_t section_number;
	uint16_t type;
	uint8_t storage_class;
	uint8_t aux_symbols_count;
} COFF_Symbol;
_Static_assert(sizeof(COFF_Symbol) == 18, "COFF Symbol size != 18 bytes");

typedef struct COFF_AuxSectionSymbol {
	uint32_t length;		// section length
	uint16_t reloc_count;	// number of relocation entries
	uint16_t lineno_count;	// number of line numbers
	uint32_t checksum;		// checksum for communal
	int16_t number;			// section number to associate with
	uint8_t selection;		// communal selection type
	uint8_t reserved;
	int16_t high_bits;		// high bits of the section number
} COFF_AuxSectionSymbol;
_Static_assert(sizeof(COFF_AuxSectionSymbol) == 18, "COFF Aux Section Symbol size != 18 bytes");
#pragma pack(pop)

enum {
	COFF_MACHINE_AMD64 = 0x8664,  // AMD64 (K8)
	COFF_MACHINE_ARM64 = 0xAA64,  // ARM64 Little-Endian
};

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, FILE* f) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	// The prologue and epilogue generators need some storage
	char* mini_out_buffer = tb_tls_push(tls, 64);
	
	// Buffer stores all the positions of each 
	// function relative to the .text section start.
	uint32_t* func_layout = tb_tls_push(tls, m->compiled_functions.count * sizeof(uint32_t));
	
	// String table array, stores the strings which will be put 
	// into the string table
	uint32_t string_table_length = 0;
	uint32_t string_table_mark = 4;
	const char** string_table = (const char**)&tls->data[tls->used];
	
	const int number_of_sections = 2;
	COFF_FileHeader header = {
		.num_sections = number_of_sections,
		.timestamp = time(NULL),
		.symbol_count = 0,
		.symbol_table = 0,
		.characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED
	};
	
	COFF_SectionHeader text_section = {
		.name = { '.', 't', 'e', 'x', 't' }, // .text
		.characteristics = COFF_CHARACTERISTICS_TEXT
	};
	
	COFF_SectionHeader rdata_section = {
		.name = { '.', 'r', 'd', 'a', 't', 'a' }, // .rdata
		.characteristics = COFF_CHARACTERISTICS_RODATA,
		
		// TODO(NeGate): Optimize this a bit, we should probably deduplicate
		// the values
		.raw_data_size = m->const32_patches.count * sizeof(uint32_t)
	};
	
	switch (m->target_arch) {
		case TB_ARCH_X86_64: {
			header.machine = COFF_MACHINE_AMD64;
			
			for (size_t i = 0; i < m->compiled_functions.count; i++) {
				TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
				func_layout[i] = text_section.raw_data_size;
				
				// TODO(NeGate): This data could be arranged better for streaming
				size_t prologue = code_gen->get_prologue_length(out_f->prologue_epilogue_metadata,
																out_f->stack_usage);
				
				size_t epilogue = code_gen->get_epilogue_length(out_f->prologue_epilogue_metadata,
																out_f->stack_usage);
				
				text_section.raw_data_size += prologue;
				text_section.raw_data_size += epilogue;
				text_section.raw_data_size += m->compiled_functions.data[i].emitter.count;
			}
			break;
		}
		case TB_ARCH_AARCH64: {
			header.machine = COFF_MACHINE_ARM64;
			tb_unreachable(); // TODO(NeGate): Implement prologue and epilogue stuff
			break;
		}
		default: tb_unreachable();
	}
	
	for (size_t i = 0; i < m->call_patches.count; i++) {
		TB_FunctionPatch* p = &m->call_patches.data[i];
		TB_FunctionOutput* out_f = &m->compiled_functions.data[p->func_id];
		uint8_t* code = out_f->emitter.data;
		
		// TODO(NeGate): Consider caching this value if it gets expensive to calculate.
		uint32_t actual_pos = func_layout[p->func_id] + p->pos + 4;
		
		actual_pos += code_gen->get_prologue_length(out_f->prologue_epilogue_metadata,
													out_f->stack_usage);
		
		// TODO(NeGate): Figure out how big they need to be on Aarch64
		assert(m->target_arch == TB_ARCH_X86_64);
		*((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - actual_pos;
	}
	
	text_section.raw_data_pos = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));
	rdata_section.raw_data_pos = text_section.raw_data_pos + text_section.raw_data_size;
	
	text_section.num_reloc = m->const32_patches.count;
	text_section.pointer_to_reloc = rdata_section.raw_data_pos + rdata_section.raw_data_size;
	
	header.symbol_count = (number_of_sections * 2) + m->compiled_functions.count;
	header.symbol_table = text_section.pointer_to_reloc + (text_section.num_reloc * sizeof(COFF_ImageReloc));
	
	size_t string_table_pos = header.symbol_table + (header.symbol_count * sizeof(COFF_Symbol));
	
	// it's only here for an assertion, so i'm making
	// sure it doesn't get mark as unused in release.
	((void)string_table_pos);
	
	fwrite(&header, sizeof(header), 1, f);
	fwrite(&text_section, sizeof(text_section), 1, f);
	fwrite(&rdata_section, sizeof(rdata_section), 1, f);
	
	assert(ftell(f) == text_section.raw_data_pos);
	switch (m->target_arch) {
		case TB_ARCH_X86_64: {
			header.machine = COFF_MACHINE_AMD64;
			for (size_t i = 0; i < m->compiled_functions.count; i++) {
				TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
				
				// prologue
				size_t prologue_len = code_gen->emit_prologue(mini_out_buffer,
															  out_f->prologue_epilogue_metadata,
															  out_f->stack_usage);
				fwrite(mini_out_buffer, prologue_len, 1, f);
				
				// body
				fwrite(out_f->emitter.data, out_f->emitter.count, 1, f);
				
				// epilogue
				size_t epilogue_len = code_gen->emit_epilogue(mini_out_buffer,
															  out_f->prologue_epilogue_metadata,
															  out_f->stack_usage);
				fwrite(mini_out_buffer, epilogue_len, 1, f);
			}
			break;
		}
		case TB_ARCH_AARCH64: {
			tb_unreachable(); // TODO(NeGate)
			break;
		}
		default: tb_unreachable();
	}
	
	assert(ftell(f) == rdata_section.raw_data_pos);
	for (size_t i = 0; i < m->const32_patches.count; i++) {
		TB_ConstPool32Patch* p = &m->const32_patches.data[i];
		fwrite(&p->raw_data, sizeof(uint32_t), 1, f);
	}
	
	assert(ftell(f) == text_section.pointer_to_reloc);
	for (size_t i = 0; i < m->const32_patches.count; i++) {
		TB_ConstPool32Patch* p = &m->const32_patches.data[i];
		TB_FunctionOutput* out_f = &m->compiled_functions.data[p->func_id];
		size_t actual_pos = func_layout[p->func_id] + p->pos;
		
		// TODO(NeGate): Consider caching this value if it gets expensive to calculate.
		actual_pos += code_gen->get_prologue_length(out_f->prologue_epilogue_metadata,
													out_f->stack_usage);
		
		fwrite(&(COFF_ImageReloc) {
				   .Type = IMAGE_REL_AMD64_REL32,
				   .SymbolTableIndex = 2, // rdata section
				   .VirtualAddress = actual_pos
			   }, sizeof(COFF_ImageReloc), 1, f);
	}
	
	assert(ftell(f) == header.symbol_table);
	fwrite(&(COFF_Symbol) {
			   .short_name = { '.', 't', 'e', 'x', 't' },
			   .section_number = 1,
			   .storage_class = IMAGE_SYM_CLASS_STATIC,
			   .aux_symbols_count = 1
		   }, sizeof(COFF_Symbol), 1, f);
	
	fwrite(&(COFF_AuxSectionSymbol) {
			   .length = text_section.raw_data_size,
			   .reloc_count = text_section.num_reloc,
			   .number = 1
		   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	
	fwrite(&(COFF_Symbol) {
			   .short_name = { '.', 'r', 'd', 'a', 't', 'a' },
			   .section_number = 2,
			   .storage_class = IMAGE_SYM_CLASS_STATIC,
			   .aux_symbols_count = 1
		   }, sizeof(COFF_Symbol), 1, f);
	
	fwrite(&(COFF_AuxSectionSymbol) {
			   .length = rdata_section.raw_data_size,
			   .number = 2
		   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		COFF_Symbol sym = {
			.value = func_layout[i],
			.section_number = 1,
			.storage_class = IMAGE_SYM_CLASS_EXTERNAL
		};
		
		size_t name_len = strlen(m->compiled_functions.data[i].name);
		assert(name_len < UINT16_MAX);
		
		if (name_len >= 8) {
			sym.long_name[0] = 0; // this value is 0 for long names
			sym.long_name[1] = string_table_mark;
			
			string_table[string_table_length++] = m->compiled_functions.data[i].name;
			string_table_mark += name_len + 1;
		}
		else {
			memcpy(sym.short_name, m->compiled_functions.data[i].name, name_len + 1);
		}
		
		fwrite(&sym, sizeof(sym), 1, f);
	}
	
	// String table
	// First 4 bytes are the size of the string table
	assert(ftell(f) == string_table_pos);
	fwrite(&string_table_mark, sizeof(string_table_mark), 1, f);
	
	for (size_t i = 0; i < string_table_length; i++) {
		const char* s = string_table[i];
		
		fwrite(s, 1, strlen(s) + 1, f);
	}
}
