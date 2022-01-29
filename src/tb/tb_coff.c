#include "tb_internal.h"

#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

#if TB_HOST_ARCH == TB_HOST_X86_64
#include <emmintrin.h>
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

#define IMAGE_REL_AMD64_ADDR64        0x0001
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
static_assert(sizeof(COFF_SectionHeader) == 40, "COFF Section header size != 40 bytes");

typedef struct COFF_FileHeader {
	uint16_t machine;
	uint16_t num_sections;
	uint32_t timestamp;
	uint32_t symbol_table;
	uint32_t symbol_count;
	uint16_t optional_header_size;
	uint16_t characteristics;
} COFF_FileHeader;
static_assert(sizeof(COFF_FileHeader) == 20, "COFF File header size != 20 bytes");

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
static_assert(sizeof(COFF_ImageReloc) == 10, "COFF Image Relocation size != 10 bytes");

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
static_assert(sizeof(COFF_Symbol) == 18, "COFF Symbol size != 18 bytes");

typedef struct COFF_AuxSectionSymbol {
	uint32_t length;		    // section length
	uint16_t reloc_count;	   // number of relocation entries
	uint16_t lineno_count;	  // number of line numbers
	uint32_t checksum;		  // checksum for communal
	int16_t number;			 // section number to associate with
	uint8_t selection;		  // communal selection type
	uint8_t reserved;
	int16_t high_bits;		  // high bits of the section number
} COFF_AuxSectionSymbol;
static_assert(sizeof(COFF_AuxSectionSymbol) == 18, "COFF Aux Section Symbol size != 18 bytes");

typedef struct {
	union {
		unsigned long l_symndx;  /* function name symbol index */
		unsigned long l_paddr;   /* address of line number     */
	} l_addr;
	unsigned short l_lnno;       /* line number                */
} LINENO;
#pragma pack(pop)

enum {
	COFF_MACHINE_AMD64 = 0x8664,  // AMD64 (K8)
	COFF_MACHINE_ARM64 = 0xAA64,  // ARM64 Little-Endian
};

// leftrotate function definition
#define LEFTROTATE(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

// These vars will contain the hash
uint32_t h0, h1, h2, h3;

// https://gist.github.com/creationix/4710780
void md5(uint8_t *initial_msg, size_t initial_len) {
	
    // Message (to prepare)
    uint8_t *msg = NULL;
	
    // Note: All variables are unsigned 32 bit and wrap modulo 2^32 when calculating
	
    // r specifies the per-round shift amounts
	
    uint32_t r[] = {7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
		5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
		4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
		6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21};
	
    // Use binary integer part of the sines of integers (in radians) as constants// Initialize variables:
    uint32_t k[] = {
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391};
	
    h0 = 0x67452301;
    h1 = 0xefcdab89;
    h2 = 0x98badcfe;
    h3 = 0x10325476;
	
    // Pre-processing: adding a single 1 bit
    //append "1" bit to message    
    /* Notice: the input bytes are considered as bits strings,
       where the first bit is the most significant bit of the byte.[37] */
	
    // Pre-processing: padding with zeros
    //append "0" bit until message length in bit ≡ 448 (mod 512)
    //append length mod (2 pow 64) to message
	
    int new_len = ((((initial_len + 8) / 64) + 1) * 64) - 8;
	
    msg = calloc(new_len + 64, 1); // also appends "0" bits 
	// (we alloc also 64 extra bytes...)
    memcpy(msg, initial_msg, initial_len);
    msg[initial_len] = 128; // write the "1" bit
	
    uint32_t bits_len = 8*initial_len; // note, we append the len
    memcpy(msg + new_len, &bits_len, 4);           // in bits at the end of the buffer
	
    // Process the message in successive 512-bit chunks:
    //for each 512-bit chunk of message:
    int offset;
    for(offset=0; offset<new_len; offset += (512/8)) {
		
        // break chunk into sixteen 32-bit words w[j], 0 ≤ j ≤ 15
        uint32_t *w = (uint32_t *) (msg + offset);
		
        // Initialize hash value for this chunk:
        uint32_t a = h0;
        uint32_t b = h1;
        uint32_t c = h2;
        uint32_t d = h3;
		
        // Main loop:
        uint32_t i;
        for(i = 0; i<64; i++) {
            uint32_t f, g;
			
			if (i < 16) {
                f = (b & c) | ((~b) & d);
                g = i;
            } else if (i < 32) {
                f = (d & b) | ((~d) & c);
                g = (5*i + 1) % 16;
            } else if (i < 48) {
                f = b ^ c ^ d;
                g = (3*i + 5) % 16;          
            } else {
                f = c ^ (b | (~d));
                g = (7*i) % 16;
            }
			
            uint32_t temp = d;
            d = c;
            c = b;
            
			b = b + LEFTROTATE((a + f + k[i] + w[g]), r[i]);
            a = temp;
        }
		
        // Add this chunk's hash to result so far:
		
        h0 += a;
        h1 += b;
        h2 += c;
        h3 += d;
		
    }
	
    // cleanup
    free(msg);
	
}

static void md5sum_file(uint8_t* out_bytes, const char* filepath) {
	FILE* file = fopen(filepath, "rb");
	if (!file) {
		printf("Could not read file: %s\n", filepath);
		abort();
	}
	
	int descriptor = fileno(file);
	
	struct stat file_stats;
	if (fstat(descriptor, &file_stats) == -1) {
		fclose(file);
		abort();
	}
	
	size_t len = file_stats.st_size;
	char* data = malloc(len + 17);
	
	fseek(file, 0, SEEK_SET);
	size_t length_read = fread(data, 1, len, file);
	
	md5(data, len);
	
	uint8_t* p;
	
	p = (uint8_t*) &h0;
	out_bytes[0] = p[0];
	out_bytes[1] = p[1];
	out_bytes[2] = p[2];
	out_bytes[3] = p[3];
	
	p = (uint8_t*) &h1;
	out_bytes[4] = p[0];
	out_bytes[5] = p[1];
	out_bytes[6] = p[2];
	out_bytes[7] = p[3];
	
	p = (uint8_t*) &h2;
	out_bytes[8]  = p[0];
	out_bytes[9]  = p[1];
	out_bytes[10] = p[2];
	out_bytes[11] = p[3];
	
	p = (uint8_t*) &h3;
	out_bytes[12] = p[0];
	out_bytes[13] = p[1];
	out_bytes[14] = p[2];
	out_bytes[15] = p[3];
	
	fclose(file);
}

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, const char* path) {
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
	
	uint32_t data_relocation_count = 0;
	loop(t, m->max_threads) {
		loop(j, arrlen(m->globals[t])) {
			TB_Global* g = &m->globals[t][j];
			
			TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
			TB_Initializer* i = (TB_Initializer*)&m->initializers[g->init / per_thread_stride][g->init % per_thread_stride];
			
			loop(k, i->obj_count) {
				data_relocation_count += (i->objects[k].type != TB_INIT_OBJ_REGION);
			}
		}
	}
	
	// NOTE(NeGate): The symbol ids per thread of these symbol groups are all sequencial
	// symbol id = symbol type base + thread's baseline + local id
	// both the global and external symbols share a baseline
	uint32_t* external_symbol_relative_id = tb_tls_push(tls, TB_MAX_THREADS * sizeof(uint32_t));
	uint32_t* global_symbol_relative_id = tb_tls_push(tls, TB_MAX_THREADS * sizeof(uint32_t));
	
	loop(i, m->max_threads) {
		external_symbol_relative_id[i] = string_table_cap;
		string_table_cap += arrlen(m->externals[i]);
	}
	
	loop(i, m->max_threads) {
		global_symbol_relative_id[i] = string_table_cap;
		string_table_cap += arrlen(m->globals[i]);
	}
	string_table_cap += m->compiled_functions.count;
	
	char** string_table = malloc(string_table_cap * sizeof(const char*));
	
	// drops the debug sections if no line info exists
	bool is_emitting_codeview = (m->line_info_count != 0);
	
	const int number_of_sections = is_emitting_codeview ? 5 : 3;
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
	
	COFF_SectionHeader data_section = {
		.name = { ".data" }, // .data
		.characteristics = COFF_CHARACTERISTICS_DATA,
		.raw_data_size = m->data_region_size
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
	
	// if the codeview stuff is never done, this is never actually needed so it's
	// fine that it's NULL
	uint32_t* file_table_offset = NULL;
	
	if (is_emitting_codeview) {
		// This is actually going to alias various different values throught this debug 
		// section generation pipeline and at the end stores an array of the procedure
		// relocations
		size_t file_table_size = m->compiled_functions.count;
		if (file_table_size < m->files.count) file_table_size = m->files.count;
		
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
				uint8_t md5sum[16];
				md5sum_file(md5sum, m->files.data[i].path);
				
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
						size_t offset = tb_ffs(mask) - 1;
						
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
							
							size_t ffs = tb_ffs(mask);
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
		{
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
		
		debugs_section.raw_data_size = debugs_out.count;
		debugt_section.raw_data_size = debugt_out.count;
	}
	
	// Target specific: resolve internal call patches
	code_gen->emit_call_patches(m, func_layout);
	
	text_section.raw_data_pos = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));
	rdata_section.raw_data_pos = text_section.raw_data_pos + text_section.raw_data_size;
	data_section.raw_data_pos = rdata_section.raw_data_pos + rdata_section.raw_data_size;
	
	if (is_emitting_codeview) {
		debugt_section.raw_data_pos = data_section.raw_data_pos + data_section.raw_data_size;
		debugs_section.raw_data_pos = debugt_section.raw_data_pos + debugt_section.raw_data_size;
	}
	
	text_section.num_reloc = 0;
	loop(i, m->max_threads) {
		text_section.num_reloc += arrlen(m->const_patches[i]);
		text_section.num_reloc += arrlen(m->ecall_patches[i]);
		text_section.num_reloc += arrlen(m->global_patches[i]);
	}
	
	data_section.num_reloc = data_relocation_count;
	
	// A bunch of relocations are made by the CodeView sections, if there's no
	// debug info then these are ignored/non-existent.
	if (is_emitting_codeview) {
		debugs_section.num_reloc = 0;
		debugs_section.num_reloc = 2 + (2 * m->compiled_functions.count);
		
		text_section.pointer_to_reloc = debugs_section.raw_data_pos
			+ debugs_section.raw_data_size;
	} else {
		text_section.pointer_to_reloc = data_section.raw_data_pos 
			+ data_section.raw_data_size;
	}
	
	data_section.pointer_to_reloc = text_section.pointer_to_reloc 
		+ (text_section.num_reloc * sizeof(COFF_ImageReloc));
	
	debugs_section.pointer_to_reloc = data_section.pointer_to_reloc 
		+ (data_section.num_reloc * sizeof(COFF_ImageReloc));
	
	header.symbol_count = (number_of_sections * 2)
		+ m->compiled_functions.count;
	
	loop(i, m->max_threads) {
		header.symbol_count += arrlen(m->externals[i]);
	}
	
	loop(i, m->max_threads) {
		header.symbol_count += arrlen(m->globals[i]);
	}
	
#if !TB_STRIP_LABELS
	header.symbol_count += arrlen(m->label_symbols);
#endif
	
	header.symbol_table = debugs_section.pointer_to_reloc 
		+ (debugs_section.num_reloc * sizeof(COFF_ImageReloc));
	
	size_t string_table_pos = header.symbol_table 
		+ (header.symbol_count * sizeof(COFF_Symbol));
	
	// it's only here for an assertion, so i'm making
	// sure it doesn't get mark as unused in release.
	((void)string_table_pos);
	
	FILE* f = fopen(path, "wb");
	fwrite(&header, sizeof(header), 1, f);
	fwrite(&text_section, sizeof(text_section), 1, f);
	fwrite(&rdata_section, sizeof(rdata_section), 1, f);
	fwrite(&data_section, sizeof(data_section), 1, f);
	
	if (is_emitting_codeview) {
		fwrite(&debugt_section, sizeof(debugt_section), 1, f);
		fwrite(&debugs_section, sizeof(debugs_section), 1, f);
	}
	
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
		
		assert(ftell(f) == (text_section.raw_data_pos + func_layout[i]));
		
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
	
	assert(ftell(f) == data_section.raw_data_pos);
	{
		// TODO(NeGate): Optimize this for size and speed, sometimes
		// there's huge sections will be filled with just empty regions
		// so it's probably best not to represent everything in a big
		// buffer.
		char* data = tb_platform_heap_alloc(m->data_region_size);
		
		loop(i, m->max_threads) {
			loop(j, arrlen(m->globals[i])) {
				TB_Global* g = &m->globals[i][j];
				
				TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Initializer* init = (TB_Initializer*)&m->initializers[g->init / per_thread_stride][g->init % per_thread_stride];
				
				// clear out space
				memset(&data[g->pos], 0, init->size);
				
				loop(k, init->obj_count) if (init->objects[k].type == TB_INIT_OBJ_REGION) {
					memcpy(&data[g->pos + init->objects[k].offset],
						   init->objects[k].region.ptr,
						   init->objects[k].region.size);
				}
			}
		}
		
		fwrite(data, m->data_region_size, 1, f);
		tb_platform_heap_free(data);
	}
	
	// Emit debug info
	if (is_emitting_codeview) {
		assert(ftell(f) == debugt_section.raw_data_pos);
		fwrite(debugt_out.data, debugt_out.count, 1, f);
		
		assert(ftell(f) == debugs_section.raw_data_pos);
		fwrite(debugs_out.data, debugs_out.count, 1, f);
	}
	
	assert(ftell(f) == text_section.pointer_to_reloc);
	loop(i, m->max_threads) {
		loop(j, arrlen(m->const_patches[i])) {
			TB_ConstPoolPatch* p = &m->const_patches[i][j];
			TB_FunctionOutput* out_f = &m->compiled_functions.data[p->source];
			
			uint64_t meta = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;
			
			size_t actual_pos = func_layout[p->source] 
				+ code_gen->get_prologue_length(meta, stack_usage)
				+ p->pos;
			
			fwrite(&(COFF_ImageReloc) {
					   .Type = IMAGE_REL_AMD64_REL32,
					   .SymbolTableIndex = 2, // rdata section
					   .VirtualAddress = actual_pos
				   }, sizeof(COFF_ImageReloc), 1, f);
		}
	}
	
	size_t function_sym_start = (number_of_sections * 2);
	size_t extern_func_sym_start = function_sym_start + m->compiled_functions.count;
	loop(i, m->max_threads) {
		loop(j, arrlen(m->ecall_patches[i])) {
			TB_ExternFunctionPatch* p = &m->ecall_patches[i][j];
			TB_FunctionOutput* out_f = &m->compiled_functions.data[p->source];
			
			uint64_t meta = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;
			
			size_t actual_pos = func_layout[p->source] 
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
	
	loop(i, m->max_threads) {
		loop(j, arrlen(m->global_patches[i])) {
			TB_GlobalPatch* p = &m->global_patches[i][j];
			TB_FunctionOutput* out_f = &m->compiled_functions.data[p->source];
			
			uint64_t meta = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;
			
			size_t actual_pos = func_layout[p->source] 
				+ code_gen->get_prologue_length(meta, stack_usage)
				+ p->pos;
			
			TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
			int symbol_id = global_symbol_relative_id[p->global / per_thread_stride]
				+ (p->global % per_thread_stride);
			
			fwrite(&(COFF_ImageReloc) {
					   .Type = IMAGE_REL_AMD64_REL32,
					   .SymbolTableIndex = extern_func_sym_start + symbol_id,
					   .VirtualAddress = actual_pos
				   }, sizeof(COFF_ImageReloc), 1, f);
		}
	}
	
	assert(ftell(f) == data_section.pointer_to_reloc);
	loop(i, m->max_threads) {
		loop(j, arrlen(m->globals[i])) {
			TB_Global* g = &m->globals[i][j];
			
			TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
			TB_Initializer* init = (TB_Initializer*)&m->initializers[g->init / per_thread_stride][g->init % per_thread_stride];
			
			loop(k, init->obj_count) {
				size_t actual_pos = g->pos + init->objects[k].offset;
				
				switch (init->objects[k].type) {
					case TB_INIT_OBJ_RELOC_GLOBAL: {
						TB_GlobalID global_id = init->objects[k].reloc_global;
						
						TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
						int symbol_id = global_symbol_relative_id[global_id / per_thread_stride]
							+ (global_id % per_thread_stride);
						
						fwrite(&(COFF_ImageReloc) {
								   .Type = IMAGE_REL_AMD64_ADDR64,
								   .SymbolTableIndex = extern_func_sym_start + symbol_id,
								   .VirtualAddress = actual_pos
							   }, sizeof(COFF_ImageReloc), 1, f);
						break;
					}
					
					case TB_INIT_OBJ_RELOC_EXTERN: {
						TB_ExternalID extern_id = init->objects[k].reloc_extern;
						
						TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
						int symbol_id = external_symbol_relative_id[extern_id / per_thread_stride]
							+ (extern_id % per_thread_stride);
						
						fwrite(&(COFF_ImageReloc) {
								   .Type = IMAGE_REL_AMD64_ADDR64,
								   .SymbolTableIndex = extern_func_sym_start + symbol_id,
								   .VirtualAddress = actual_pos
							   }, sizeof(COFF_ImageReloc), 1, f);
						break;
					}
					
					case TB_INIT_OBJ_RELOC_FUNCTION: {
						int symbol_id = init->objects[k].reloc_function;
						
						fwrite(&(COFF_ImageReloc) {
								   .Type = IMAGE_REL_AMD64_ADDR64,
								   .SymbolTableIndex = function_sym_start + symbol_id,
								   .VirtualAddress = actual_pos
							   }, sizeof(COFF_ImageReloc), 1, f);
						break;
					}
					
					default: break;
				}
			}
		}
	}
	
	// Field base relocations in .debug$S
	if (is_emitting_codeview) {
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
			   .length = data_section.raw_data_size,
			   .number = 2
		   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	
	fwrite(&(COFF_Symbol) {
			   .short_name = { ".data" },
			   .section_number = 3,
			   .storage_class = IMAGE_SYM_CLASS_STATIC,
			   .aux_symbols_count = 1
		   }, sizeof(COFF_Symbol), 1, f);
	
	fwrite(&(COFF_AuxSectionSymbol) {
			   .length = data_section.raw_data_size,
			   .reloc_count = data_section.num_reloc,
			   .number = 3
		   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	
	if (is_emitting_codeview) {
		fwrite(&(COFF_Symbol) {
				   .short_name = { ".debug$T" },
				   .section_number = 4,
				   .storage_class = IMAGE_SYM_CLASS_STATIC,
				   .aux_symbols_count = 1
			   }, sizeof(COFF_Symbol), 1, f);
		
		fwrite(&(COFF_AuxSectionSymbol) {
				   .length = debugt_section.raw_data_size,
				   .number = 4
			   }, sizeof(COFF_AuxSectionSymbol), 1, f);
		
		fwrite(&(COFF_Symbol) {
				   .short_name = { ".debug$S" },
				   .section_number = 5,
				   .storage_class = IMAGE_SYM_CLASS_STATIC,
				   .aux_symbols_count = 1
			   }, sizeof(COFF_Symbol), 1, f);
		
		fwrite(&(COFF_AuxSectionSymbol) {
				   .length = debugs_section.raw_data_size,
				   .reloc_count = debugs_section.num_reloc,
				   .number = 5
			   }, sizeof(COFF_AuxSectionSymbol), 1, f);
	}
	
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		bool is_extern = m->compiled_functions.data[i].linkage == TB_LINKAGE_PUBLIC;
		
		COFF_Symbol sym = {
			.value = func_layout[i],
			.section_number = 1,
			.storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
		};
		
		size_t name_len = strlen(m->compiled_functions.data[i].name);
		assert(name_len < UINT16_MAX);
		
		if (name_len >= 8) {
			sym.long_name[0] = 0; // this value is 0 for long names
			sym.long_name[1] = string_table_mark;
			
			string_table[string_table_length++] = (char*)m->compiled_functions.data[i].name;
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
	
	loop(i, m->max_threads) {
		loop(j, arrlen(m->globals[i])) {
			const TB_Global* restrict g = &m->globals[i][j];
			
			bool is_extern = g->linkage == TB_LINKAGE_PUBLIC;
			COFF_Symbol sym = {
				.value = g->pos,
				.section_number = 3, // data section
				.storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
			};
			
			size_t name_len = strlen(g->name);
			assert(name_len < UINT16_MAX);
			
			if (name_len >= 8) {
				sym.long_name[0] = 0; // this value is 0 for long names
				sym.long_name[1] = string_table_mark;
				
				string_table[string_table_length++] = g->name;
				string_table_mark += name_len + 1;
			} else {
				memcpy(sym.short_name, g->name, name_len + 1);
			}
			
			fwrite(&sym, sizeof(sym), 1, f);
		}
	}
	
#if !TB_STRIP_LABELS
	loop(i, arrlen(m->label_symbols)) {
		TB_LabelSymbol* l = &m->label_symbols[i];
		TB_FunctionOutput* out_f = &m->compiled_functions.data[l->source];
		
		uint64_t meta = out_f->prologue_epilogue_metadata;
		uint64_t stack_usage = out_f->stack_usage;
		
		size_t actual_pos = func_layout[l->source] 
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
	fclose(f);
	
	free(debugs_out.data);
	free(debugt_out.data);
	free(string_table);
	free(func_layout);
}
