#include "tb_internal.h"

#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

// IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES
#define COFF_CHARACTERISTICS_TEXT 0x60500020u
// IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ
#define COFF_CHARACTERISTICS_DATA 0xC0000040u
// IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ
#define COFF_CHARACTERISTICS_RODATA 0x40000040u
// IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES
#define COFF_CHARACTERISTICS_BSS 0xC0500080u
// IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_ALIGN_8BYTES | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE
#define COFF_CHARACTERISTICS_CV 0x42100040u

#define IMAGE_SYM_CLASS_EXTERNAL      0x0002
#define IMAGE_SYM_CLASS_STATIC        0x0003
#define IMAGE_SYM_CLASS_LABEL         0x0006
#define IMAGE_SYM_CLASS_FILE          0x0067

#define IMAGE_FILE_LINE_NUMS_STRIPPED 0x0004

#define IMAGE_REL_AMD64_REL32         0x0004
#define IMAGE_REL_AMD64_SECTION       0x000A
#define IMAGE_REL_AMD64_SECREL        0x000B

#define MD5_HASHBYTES 16

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

typedef struct {
	union {
		unsigned long l_symndx;  /* function name symbol index */
		unsigned long l_paddr;   /* address of line number     */
	} l_addr;
	unsigned short l_lnno;     /* line number                */
} LINENO;
#pragma pack(pop)

enum {
	COFF_MACHINE_AMD64 = 0x8664,  // AMD64 (K8)
	COFF_MACHINE_ARM64 = 0xAA64,  // ARM64 Little-Endian
};

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, FILE* f) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	// The prologue and epilogue generators need some storage
	uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);
	
	// Buffer stores all the positions of each 
	// function relative to the .text section start.
	uint32_t* func_layout = malloc(m->compiled_functions.count * sizeof(uint32_t));
	
	// String table array, stores the strings which will be put 
	// into the string table
	uint32_t string_table_length = 0;
	uint32_t string_table_mark = 4;
	
	uint32_t string_table_cap = 0;
	uint32_t* external_symbol_relative_id = tb_tls_push(tls, TB_MAX_THREADS * sizeof(uint32_t));
	
	loop(i, m->max_threads) {
		external_symbol_relative_id[i] = string_table_cap;
		string_table_cap += arrlen(m->externals[i]);
	}
	string_table_cap += m->compiled_functions.count;
	
	const char** string_table = malloc(string_table_cap * sizeof(const char*));
	
	// drops the debug sections if no line info exists
	const int number_of_sections = 4;
	COFF_FileHeader header = {
		.num_sections = number_of_sections,
		.timestamp = time(NULL),
		.symbol_count = 0,
		.symbol_table = 0,
		.characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED
	};
	
	COFF_SectionHeader text_section = {
		.name = { ".text" }, // .text
		.characteristics = COFF_CHARACTERISTICS_TEXT
	};
	
	COFF_SectionHeader rdata_section = {
		.name = { ".rdata" }, // .rdata
		.characteristics = COFF_CHARACTERISTICS_RODATA,
		.raw_data_size = m->rdata_region_size
	};
	
	COFF_SectionHeader debugt_section = {
		.name = { ".debug$T" },
		.characteristics = COFF_CHARACTERISTICS_CV
	};
	
	COFF_SectionHeader debugs_section = {
		.name = { ".debug$S" },
		.characteristics = COFF_CHARACTERISTICS_CV
	};
	
	switch (m->target_arch) {
		case TB_ARCH_X86_64: header.machine = COFF_MACHINE_AMD64; break;
		case TB_ARCH_AARCH64: header.machine = COFF_MACHINE_ARM64; break;
		default: tb_todo();
	}
	
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
		func_layout[i] = text_section.raw_data_size;
		
		uint64_t meta = out_f->prologue_epilogue_metadata;
		uint64_t stack_usage = out_f->stack_usage;
		
		size_t code_size = out_f->code_size;
		size_t prologue = code_gen->get_prologue_length(meta, stack_usage);
		size_t epilogue = code_gen->get_epilogue_length(meta, stack_usage);
		assert(prologue + epilogue < PROEPI_BUFFER);
		
		text_section.raw_data_size += prologue;
		text_section.raw_data_size += epilogue;
		text_section.raw_data_size += code_size;
	}
	
    TB_Emitter debugs_out = { 0 };
    TB_Emitter debugt_out = { 0 };
	
	// used for some relocations later
	uint32_t cv_field_base = 0;
	
	// This is actually going to alias various different values throught this debug 
	// section generation pipeline and at the end stores an array of the procedure
	// relocations
	size_t file_table_size = m->compiled_functions.count;
	if (file_table_size < m->files.count) file_table_size = m->files.count;
	
	// if the codeview stuff is never done, this is never actually needed so it's
	// fine that it's NULL
	uint32_t* file_table_offset = NULL;
	if (m->line_info_count != 0) {
		// Based on this, it's the only nice CodeView source out there:
		// https://github.com/netwide-assembler/nasm/blob/master/output/codeview.c
		tb_out4b(&debugs_out, 0x00000004);
		file_table_offset = tb_tls_push(tls, file_table_size * sizeof(uint32_t));
		
		//
		// Write file name table
		//
		{
			tb_out4b(&debugs_out, 0x000000F3);
			
			uint32_t patch = debugs_out.count;
			tb_out4b(&debugs_out, 0x00);
			tb_out1b(&debugs_out, 0x00);
			
			size_t off = 1;
			for (size_t i = 1; i < m->files.count; i++) {
				const char* filename = m->files.data[i].path;
				size_t filename_len = strlen(filename) + 1;
				
				tb_out_reserve(&debugs_out, filename_len);
				tb_outs_UNSAFE(&debugs_out, filename_len, (const uint8_t*)filename);
				
				file_table_offset[i] = off;
				off += filename_len;
			}
			
			// patch total filename space
			*((uint32_t*)&debugs_out.data[patch]) = off;
			
			size_t pad = 4 - (debugs_out.count % 4);
			if (pad == 4) pad = 0;
			while (pad--) tb_out1b(&debugs_out, 0x00);
		}
		
		//
		// Write Source file table
		//
		{
			size_t entry_size = (4 + 2 + MD5_HASHBYTES + 2);
			
			tb_out4b(&debugs_out, 0x000000F4);
			tb_out4b(&debugs_out, entry_size * (m->files.count - 1));
			
			size_t off = 0;
			for (size_t i = 1; i < m->files.count; i++) {
				// TODO(NeGate): Implement a MD5 sum function
				uint8_t md5sum[16] = { 
					//0xD9, 0x9C, 0x59, 0x2E, 0x44, 0x42, 0x73, 0x40, 
					//0xB5, 0x2A, 0xCF, 0x89, 0x6A, 0x91, 0x9F, 0x70
				};
				
				tb_out4b(&debugs_out, file_table_offset[i]);
				tb_out2b(&debugs_out, 0x0110);
				tb_out_reserve(&debugs_out, MD5_HASHBYTES);
				tb_outs_UNSAFE(&debugs_out, MD5_HASHBYTES, md5sum);
				tb_out2b(&debugs_out, 0x0000);
				
				file_table_offset[i] = off;
				off += entry_size;
			}
			
			size_t pad = 4 - (debugs_out.count % 4);
			if (pad == 4) pad = 0;
			while (pad--) tb_out1b(&debugs_out, 0x00);
		}
		
		//
		// Write line number table
		//
		{
			const uint32_t file_field_len = 12;
			const uint32_t line_field_len = 8;
			
			size_t field_length = 12
				+ ((m->files.count - 1) * file_field_len)
				+ (m->line_info_count * line_field_len);
			
			tb_out4b(&debugs_out, 0x000000F2);
			tb_out4b(&debugs_out, field_length);
			
			cv_field_base = debugs_out.count;
			tb_out4b(&debugs_out, 0); /* SECREL, updated by relocation */
			tb_out2b(&debugs_out, 0); /* SECTION, updated by relocation*/
			tb_out2b(&debugs_out, 0); /* pad */
			tb_out4b(&debugs_out, text_section.raw_data_size);
			
			for (size_t i = 1; i < m->files.count; i++) {
				// source file mapping
				tb_out4b(&debugs_out, file_table_offset[i]);
				
				uint32_t patch = debugs_out.count;
				tb_out4b(&debugs_out, 0);
				tb_out4b(&debugs_out, 0);
				
				size_t line_count_in_file = 0;
				
				// NOTE(NeGate): Holy shit i didn't think ahead but i'll make this
				// work for now, essentially each file keeps track of it's own line
				// table so i should have arranged it like that but i didn't so now
				// i have to filter out from the possibly millions of nodes (well for
				// now it shouldn't matter much)
				// TODO(NeGate): Optimize this... please future me!
				for (size_t j = 0; j < m->functions.count; j++) {
					TB_Function* f = &m->functions.data[j];
					
#if TB_HOST_ARCH == TB_HOST_X86_64
					__m128i pattern = _mm_set1_epi8(TB_LINE_INFO);
					
					for (size_t k = 0; k < f->nodes.count; k += 16) {
						__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[k]);
						unsigned int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, pattern));
						if (mask == 0) continue;
						
						// this one is guarentee to not be zero so it's fine
						// to not check that FFS.
						size_t offset = __builtin_ffs(mask) - 1;
						
						size_t l = k + offset;
						// skip over the mask bit for the next iteration
						mask >>= (offset + 1);
						
						// We know it loops at least once by this point
						do {
							TB_RegPayload p = f->nodes.payload[l];
							if (p.line_info.file == i) {
								TB_FunctionOutput* out_f = &m->compiled_functions.data[j];
								
								// emit line entry
								uint32_t actual_pos = func_layout[j] + p.line_info.pos;
								if (p.line_info.pos) {
									actual_pos += code_gen->get_prologue_length(out_f->prologue_epilogue_metadata,
																				out_f->stack_usage);
								}
								
								tb_out4b(&debugs_out, actual_pos);
								tb_out4b(&debugs_out, 0x80000000 | p.line_info.line);
								line_count_in_file++;
							}
							
							size_t ffs = __builtin_ffs(mask);
							if (ffs == 0) break;
							
							// skip over the mask bit for the next iteration
							mask >>= ffs;
							l += ffs;
						} while (true);
					}
#else
					for (size_t k = 2; k < f->nodes.count; k++) {
						if (f->nodes.type[k] == TB_LINE_INFO
							&& f->nodes.payload[k].line_info.file == i) {
							TB_RegPayload p = f->nodes.payload[k];
							
							TB_FunctionOutput* out_f = &m->compiled_functions.data[j];
							
							// emit line entry
							uint32_t actual_pos = func_layout[j] + p.line_info.pos;
							if (p.line_info.pos) {
								actual_pos += code_gen->get_prologue_length(out_f->prologue_epilogue_metadata,
																			out_f->stack_usage);
							}
							
							tb_out4b(&debugs_out, actual_pos);
							tb_out4b(&debugs_out, 0x80000000 | p.line_info.line);
							line_count_in_file++;
						}
					}
#endif
				}
				
				*((uint32_t*)&debugs_out.data[patch]) = line_count_in_file;
				*((uint32_t*)&debugs_out.data[patch + 4]) = file_field_len + (line_count_in_file * line_field_len);
			}
			
			size_t pad = 4 - (debugs_out.count % 4);
			if (pad == 4) pad = 0;
			while (pad--) tb_out1b(&debugs_out, 0x00);
		}
		
		//
		// Write symbol info table
		//
		{
			static const char creator_str[] = "Cuik";
			static const char obj_file_name[] = "main";//"W:\\Workspace\\Cuik\\a.obj";
			
			uint32_t creator_length = 2 + 4 + 2 + (3 * 2) + (3 * 2) + sizeof(creator_str) + 2;
			
			/*sym_length = (cv8_state.num_syms[SYMTYPE_CODE] * 7) 
				+ (cv8_state.num_syms[SYMTYPE_PROC]  * 7) 
				+ (cv8_state.num_syms[SYMTYPE_LDATA] * 10) 
				+ (cv8_state.num_syms[SYMTYPE_GDATA] * 10) 
				+ (cv8_state.symbol_lengths);*/
			
			uint32_t sym_length = (m->compiled_functions.count * 7);
			
			for (size_t i = 0; i < m->compiled_functions.count; i++) {
				TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
				sym_length += strlen(out_f->name) + 1;
			}
			
			uint32_t obj_length = 2 + 4 + sizeof(obj_file_name);
			uint32_t field_length = (2 + obj_length) 
				+ (2 + creator_length) 
				+ (4 * m->compiled_functions.count)
				+ sym_length;
			
			tb_out4b(&debugs_out, 0x000000F1);
			tb_out4b(&debugs_out, field_length);
			
			// Symbol info object
			{
				tb_out2b(&debugs_out, obj_length);
				tb_out2b(&debugs_out, 0x1101);
				tb_out4b(&debugs_out, 0); /* ASM language */
				
				tb_outstr_UNSAFE(&debugs_out, obj_file_name);
				tb_out1b(&debugs_out, 0);
			}
			
			// Symbol info properties
			{
				tb_out2b(&debugs_out, creator_length);
				tb_out2b(&debugs_out, 0x1116);
				tb_out4b(&debugs_out, 'N'); /* language: 'N' (0x4e) for "NASM"; flags are 0 */
				
				tb_out2b(&debugs_out, 0x00D0); /* machine */
				tb_out2b(&debugs_out, 0); /* verFEMajor */
				tb_out2b(&debugs_out, 0); /* verFEMinor */
				tb_out2b(&debugs_out, 0); /* verFEBuild */
				
				/* BinScope/WACK insist on version >= 8.0.50727 */
				tb_out2b(&debugs_out, 9); /* verMajor */
				tb_out2b(&debugs_out, 9); /* verMinor */
				tb_out2b(&debugs_out, 65535); /* verBuild */
				
				tb_out_reserve(&debugs_out, sizeof(creator_str));
				tb_outs_UNSAFE(&debugs_out, sizeof(creator_str), (const uint8_t*)creator_str);
				
				tb_out2b(&debugs_out, 0);
			}
			
			// Symbols
			for (size_t i = 0; i < m->compiled_functions.count; i++) {
				TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
				const char* name = out_f->name;
				size_t name_len = strlen(out_f->name) + 1;
				
				tb_out2b(&debugs_out, 9 + name_len);
				tb_out2b(&debugs_out, 0x1105);
				
				file_table_offset[i] = debugs_out.count;
				tb_out4b(&debugs_out, 0); /* SECREL */
				tb_out2b(&debugs_out, 0); /* SECTION */
				tb_out1b(&debugs_out, 0); /* FLAG */
				
				tb_out_reserve(&debugs_out, name_len);
				tb_outs_UNSAFE(&debugs_out, name_len, (const uint8_t*)name);
			}
			
			size_t pad = 4 - (debugs_out.count % 4);
			if (pad == 4) pad = 0;
			while (pad--) tb_out1b(&debugs_out, 0x00);
		}
		
		//
		// Write type table
		//
		if (m->line_info_count != 0) {
			uint32_t field_len;
			uint32_t typeindex = 0x1000;
			uint32_t idx_arglist;
			
			tb_out4b(&debugt_out, 0x00000004);
			
			/* empty argument list type */
			field_len = 2 + 4;
			tb_out2b(&debugt_out, field_len);
			tb_out2b(&debugt_out, 0x1201); /* ARGLIST */
			tb_out4b(&debugt_out, 0); /* num params */
			idx_arglist = typeindex++;
			
			/* procedure type: void proc(void) */
			field_len = 2 + 4 + 1 + 1 + 2 + 4;
			tb_out2b(&debugt_out, field_len);
			tb_out2b(&debugt_out, 0x1008); /* PROC type */
			
			tb_out4b(&debugt_out, 0x00000003); /* return type VOID */
			tb_out1b(&debugt_out, 0);  /* calling convention (default) */
			tb_out1b(&debugt_out, 0);  /* function attributes */
			tb_out2b(&debugt_out, 0); /* # params */
			tb_out4b(&debugt_out, idx_arglist); /* argument list type */
			/* idx_voidfunc = typeindex++; */
			
			size_t pad = 4 - (debugt_out.count % 4);
			if (pad == 4) pad = 0;
			while (pad--) tb_out1b(&debugt_out, 0x00);
		}
	}
	
	debugs_section.raw_data_size = debugs_out.count;
	debugt_section.raw_data_size = debugt_out.count;
	
	// Target specific: resolve internal call patches
	code_gen->emit_call_patches(m, func_layout);
	
	text_section.raw_data_pos = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));
	rdata_section.raw_data_pos = text_section.raw_data_pos + text_section.raw_data_size;
	debugt_section.raw_data_pos = rdata_section.raw_data_pos + rdata_section.raw_data_size;
	debugs_section.raw_data_pos = debugt_section.raw_data_pos + debugt_section.raw_data_size;
	
	text_section.num_reloc = 0;
	loop(i, m->max_threads) {
		text_section.num_reloc += arrlen(m->const_patches[i]);
		text_section.num_reloc += arrlen(m->ecall_patches[i]);
	}
	
	// A bunch of relocations are made by the CodeView sections, if there's no
	// debug info then these are ignored/non-existent.
	debugs_section.num_reloc = 0;
	if (m->line_info_count) {
		debugs_section.num_reloc = 2 + (2 * m->compiled_functions.count);
	}
	
	text_section.pointer_to_reloc = debugs_section.raw_data_pos
		+ debugs_section.raw_data_size;
	
	debugs_section.pointer_to_reloc = text_section.pointer_to_reloc 
		+ (text_section.num_reloc * sizeof(COFF_ImageReloc));
	
	header.symbol_count = (number_of_sections * 2)
		+ m->compiled_functions.count;
	
	loop(i, m->max_threads) {
		header.symbol_count += arrlen(m->externals[i]);
	}
	
#if !TB_STRIP_LABELS
	header.symbol_count += m->label_symbols.count;
#endif
	
	header.symbol_table = debugs_section.pointer_to_reloc 
		+ (debugs_section.num_reloc * sizeof(COFF_ImageReloc));
	
	size_t string_table_pos = header.symbol_table 
		+ (header.symbol_count * sizeof(COFF_Symbol));
	
	// it's only here for an assertion, so i'm making
	// sure it doesn't get mark as unused in release.
	((void)string_table_pos);
	
	fwrite(&header, sizeof(header), 1, f);
	fwrite(&text_section, sizeof(text_section), 1, f);
	fwrite(&rdata_section, sizeof(rdata_section), 1, f);
	fwrite(&debugt_section, sizeof(debugt_section), 1, f);
	fwrite(&debugs_section, sizeof(debugs_section), 1, f);
	
	assert(ftell(f) == text_section.raw_data_pos);
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
		
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
	
	assert(ftell(f) == rdata_section.raw_data_pos);
	{
		char* rdata = tb_platform_heap_alloc(m->rdata_region_size);
		
		loop(i, m->max_threads) {
			loop(j, arrlen(m->const_patches[i])) {
				TB_ConstPoolPatch* p = &m->const_patches[i][j];
				memcpy(&rdata[p->rdata_pos], p->data, p->length);
			}
		}
		
		fwrite(rdata, m->rdata_region_size, 1, f);
		tb_platform_heap_free(rdata);
	}
	
	// Emit debug info
	assert(ftell(f) == debugt_section.raw_data_pos);
	fwrite(debugt_out.data, debugt_out.count, 1, f);
	
	assert(ftell(f) == debugs_section.raw_data_pos);
	fwrite(debugs_out.data, debugs_out.count, 1, f);
	
	assert(ftell(f) == text_section.pointer_to_reloc);
	loop(i, m->max_threads) {
		loop(j, arrlen(m->const_patches[i])) {
			TB_ConstPoolPatch* p = &m->const_patches[i][j];
			TB_FunctionOutput* out_f = &m->compiled_functions.data[p->func_id];
			
			uint64_t meta = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;
			
			size_t actual_pos = func_layout[p->func_id] 
				+ code_gen->get_prologue_length(meta, stack_usage)
				+ p->pos;
			
			fwrite(&(COFF_ImageReloc) {
					   .Type = IMAGE_REL_AMD64_REL32,
					   .SymbolTableIndex = 2, // rdata section
					   .VirtualAddress = actual_pos
				   }, sizeof(COFF_ImageReloc), 1, f);
		}
	}
	
	size_t extern_func_sym_start = 8 + m->compiled_functions.count;
	loop(i, m->max_threads) {
		loop(j, arrlen(m->ecall_patches[i])) {
			TB_ExternFunctionPatch* p = &m->ecall_patches[i][j];
			TB_FunctionOutput* out_f = &m->compiled_functions.data[p->func_id];
			
			uint64_t meta = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;
			
			size_t actual_pos = func_layout[p->func_id] 
				+ code_gen->get_prologue_length(meta, stack_usage)
				+ p->pos;
			
			TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
			int symbol_id = external_symbol_relative_id[p->target_id / per_thread_stride]
				+ (p->target_id % per_thread_stride);
			
			fwrite(&(COFF_ImageReloc) {
					   .Type = IMAGE_REL_AMD64_REL32,
					   .SymbolTableIndex = extern_func_sym_start + symbol_id,
					   .VirtualAddress = actual_pos
				   }, sizeof(COFF_ImageReloc), 1, f);
		}
	}
	
	// Field base relocations in .debug$S
	assert(ftell(f) == debugs_section.pointer_to_reloc);
	if (m->line_info_count != 0) {
		fwrite(&(COFF_ImageReloc) {
				   .Type = IMAGE_REL_AMD64_SECREL,
				   .SymbolTableIndex = 0, // text section
				   .VirtualAddress = cv_field_base
			   }, sizeof(COFF_ImageReloc), 1, f);
		
		fwrite(&(COFF_ImageReloc) {
				   .Type = IMAGE_REL_AMD64_SECTION,
				   .SymbolTableIndex = 0, // text section
				   .VirtualAddress = cv_field_base + 4
			   }, sizeof(COFF_ImageReloc), 1, f);
		
		for (size_t i = 0; i < m->compiled_functions.count; i++) {
			uint32_t off = file_table_offset[i];
			
			fwrite(&(COFF_ImageReloc) {
					   .Type = IMAGE_REL_AMD64_SECREL,
					   .SymbolTableIndex = 8 + i, // text section
					   .VirtualAddress = off
				   }, sizeof(COFF_ImageReloc), 1, f);
			
			fwrite(&(COFF_ImageReloc) {
					   .Type = IMAGE_REL_AMD64_SECTION,
					   .SymbolTableIndex = 8 + i, // text section
					   .VirtualAddress = off + 4
				   }, sizeof(COFF_ImageReloc), 1, f);
		}
	}
	
	assert(ftell(f) == header.symbol_table);
	fwrite(&(COFF_Symbol) {
			   .short_name = { ".text" },
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
			   .short_name = { ".rdata" },
			   .section_number = 2,
			   .storage_class = IMAGE_SYM_CLASS_STATIC,
			   .aux_symbols_count = 1
		   }, sizeof(COFF_Symbol), 1, f);
	
	fwrite(&(COFF_AuxSectionSymbol) {
			   .length = rdata_section.raw_data_size,
			   .number = 2
		   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	
	fwrite(&(COFF_Symbol) {
			   .short_name = { ".debug$T" },
			   .section_number = 3,
			   .storage_class = IMAGE_SYM_CLASS_STATIC,
			   .aux_symbols_count = 1
		   }, sizeof(COFF_Symbol), 1, f);
	
	fwrite(&(COFF_AuxSectionSymbol) {
			   .length = debugt_section.raw_data_size,
			   .number = 3
		   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	
	fwrite(&(COFF_Symbol) {
			   .short_name = { ".debug$S" },
			   .section_number = 4,
			   .storage_class = IMAGE_SYM_CLASS_STATIC,
			   .aux_symbols_count = 1
		   }, sizeof(COFF_Symbol), 1, f);
	
	fwrite(&(COFF_AuxSectionSymbol) {
			   .length = debugs_section.raw_data_size,
			   .number = 4
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
	
	loop(i, m->max_threads) {
		loop(j, arrlen(m->externals[i])) {
			const TB_External* restrict e = &m->externals[i][j];
			COFF_Symbol sym = {
				.value = 0,
				.section_number = 0,
				.storage_class = IMAGE_SYM_CLASS_EXTERNAL
			};
			
			size_t name_len = strlen(e->name);
			assert(name_len < UINT16_MAX);
			
			if (name_len >= 8) {
				sym.long_name[0] = 0; // this value is 0 for long names
				sym.long_name[1] = string_table_mark;
				
				string_table[string_table_length++] = e->name;
				string_table_mark += name_len + 1;
			}
			else {
				memcpy(sym.short_name, e->name, name_len + 1);
			}
			
			fwrite(&sym, sizeof(sym), 1, f);
		}
	}
	
#if !TB_STRIP_LABELS
	for (size_t i = 0; i < m->label_symbols.count; i++) {
		TB_LabelSymbol* l = &m->label_symbols.data[i];
		TB_FunctionOutput* out_f = &m->compiled_functions.data[l->func_id];
		
		uint64_t meta = out_f->prologue_epilogue_metadata;
		uint64_t stack_usage = out_f->stack_usage;
		
		size_t actual_pos = func_layout[l->func_id] 
			+ code_gen->get_prologue_length(meta, stack_usage)
			+ l->pos;
		
		COFF_Symbol sym = {
			.value = actual_pos,
			.section_number = 1,
			.storage_class = IMAGE_SYM_CLASS_LABEL
		};
		
		assert(l->label_id < 65536);
		sprintf((char*)&sym.short_name[0], ".L%x", l->label_id);
		fwrite(&sym, sizeof(sym), 1, f);
	}
#endif
	
	// String table
	// First 4 bytes are the size of the string table
	assert(ftell(f) == string_table_pos);
	fwrite(&string_table_mark, sizeof(string_table_mark), 1, f);
	
	for (size_t i = 0; i < string_table_length; i++) {
		const char* s = string_table[i];
		
		fwrite(s, 1, strlen(s) + 1, f);
	}
	
	free(debugs_out.data);
	free(debugt_out.data);
	free(string_table);
	free(func_layout);
}
