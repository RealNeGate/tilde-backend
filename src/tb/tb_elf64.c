#include "tb_internal.h"

#define	EI_MAG0       0
#define	EI_MAG1       1
#define	EI_MAG2       2
#define	EI_MAG3       3
#define	EI_CLASS      4	/* Class of machine. */
#define	EI_DATA       5	/* Data format. */
#define	EI_VERSION    6	/* ELF format version. */
#define	EI_OSABI      7	/* Operating system / ABI identification */
#define	EI_ABIVERSION 8	/* ABI version */
#define	OLD_EI_BRAND  8	/* Start of architecture identification. */
#define	EI_PAD        9	/* Start of padding (per SVR4 ABI). */
#define	EI_NIDENT     16	/* Size of e_ident array. */

/* Values for e_type. */
#define	ET_NONE       0	/* Unknown type. */
#define	ET_REL        1	/* Relocatable. */
#define	ET_EXEC       2	/* Executable. */
#define	ET_DYN        3	/* Shared object. */
#define	ET_CORE       4	/* Core file. */
#define	ET_LOOS       0xfe00	/* First operating system specific. */
#define	ET_HIOS       0xfeff	/* Last operating system-specific. */
#define	ET_LOPROC     0xff00	/* First processor-specific. */
#define	ET_HIPROC     0xffff	/* Last processor-specific. */

/* Values for e_machine. */
#define	EM_NONE       0	/* Unknown machine. */
#define	EM_X86_64     62	/* Advanced Micro Devices x86-64 */
#define	EM_AARCH64    183	/* AArch64 (64-bit ARM) */

/* sh_type */
#define	SHT_NULL      0	/* inactive */
#define	SHT_PROGBITS  1	/* program defined information */
#define	SHT_SYMTAB    2	/* symbol table section */
#define	SHT_STRTAB    3	/* string table section */
#define	SHT_NOBITS    8	/* no space section */

/* Flags for sh_flags. */
#define	SHF_WRITE            0x1	/* Section contains writable data. */
#define	SHF_ALLOC            0x2	/* Section occupies memory. */
#define	SHF_EXECINSTR        0x4	/* Section contains instructions. */
#define	SHF_MERGE            0x10	/* Section may be merged. */
#define	SHF_STRINGS          0x20	/* Section contains strings. */
#define	SHF_INFO_LINK        0x40	/* sh_info holds section index. */
#define	SHF_LINK_ORDER       0x80	/* Special ordering requirements. */
#define	SHF_OS_NONCONFORMING 0x100	/* OS-specific processing required. */
#define	SHF_GROUP            0x200	/* Member of section group. */
#define	SHF_TLS              0x400	/* Section contains TLS data. */
#define	SHF_MASKOS           0x0ff00000	/* OS-specific semantics. */
#define	SHF_MASKPROC         0xf0000000	/* Processor-specific semantics. */

typedef uint64_t    Elf64_Addr;
typedef uint16_t    Elf64_Half;
typedef uint64_t    Elf64_Off;
typedef int32_t     Elf64_Sword;
typedef int64_t     Elf64_Sxword;
typedef uint32_t    Elf64_Word;
typedef uint64_t    Elf64_Lword;
typedef uint64_t    Elf64_Xword;

/* Macros for accessing the fields of st_info. */
#define	ELF64_ST_BIND(info)		((info) >> 4)
#define	ELF64_ST_TYPE(info)		((info) & 0xf)

// http://web.mit.edu/freebsd/head/sys/sys/elf64.h
// https://cirosantilli.com/elf-hello-world#minimal-elf-file
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
# define EI_NIDENT 16

typedef struct {
	unsigned char   e_ident[EI_NIDENT];
	Elf64_Half      e_type;
	Elf64_Half      e_machine;
	Elf64_Word      e_version;
	Elf64_Addr      e_entry;
	Elf64_Off       e_phoff;
	Elf64_Off       e_shoff;
	Elf64_Word      e_flags;
	Elf64_Half      e_ehsize;
	Elf64_Half      e_phentsize;
	Elf64_Half      e_phnum;
	Elf64_Half      e_shentsize;
	Elf64_Half      e_shnum;
	Elf64_Half      e_shstrndx;
} Elf64_Ehdr;

typedef struct {
	Elf64_Word  sh_name;
	Elf64_Word  sh_type;
	Elf64_Xword sh_flags;
	Elf64_Addr  sh_addr;
	Elf64_Off   sh_offset;
	Elf64_Xword sh_size;
	Elf64_Word  sh_link;
	Elf64_Word  sh_info;
	Elf64_Xword sh_addralign;
	Elf64_Xword sh_entsize;
} Elf64_Shdr;

typedef struct {
	Elf64_Word      st_name;
	unsigned char   st_info;
	unsigned char   st_other;
	Elf64_Half      st_shndx;
	Elf64_Addr      st_value;
	Elf64_Xword     st_size;
} Elf64_Sym;

void tb_export_elf64(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, bool emit_debug_info) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	// The prologue and epilogue generators need some storage
	uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);
	
	// Buffer stores all the positions of each 
	// function relative to the .text section start.
	uint32_t* func_layout = malloc(m->compiled_functions.count * sizeof(uint32_t));
	
	uint16_t machine;
	switch (m->target_arch) {
		case TB_ARCH_X86_64: machine = EM_X86_64; break;
		case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
		default: tb_todo();
	}
	
	// Generate the header structs
	const int number_of_sections = 7;
	Elf64_Ehdr header = {
		.e_ident = {
			[EI_MAG0] = 0x7F, // magic number
			[EI_MAG1] = 'E',
			[EI_MAG2] = 'L',
			[EI_MAG3] = 'F',
			[EI_CLASS] = 2, // 64bit ELF file
			[EI_DATA] = 1, // little-endian
			[EI_VERSION] = 1, // 1.0
			[EI_OSABI] = 0,
			[EI_ABIVERSION] = 0
		},
		.e_type = ET_REL, // relocatable
		.e_version = 1,
		.e_machine = machine,
		.e_entry = 0,
		
		// section headers go at the end of the file
		// and are filed in later.
		.e_shoff = 0,
		.e_flags = 0,
		
		.e_ehsize = sizeof(Elf64_Ehdr),
		
		.e_shentsize = sizeof(Elf64_Shdr),
		.e_shnum = number_of_sections,
		.e_shstrndx = 1
	};
	
	Elf64_Shdr null_section = { 0 };
	
	Elf64_Shdr strtab_section = {
		.sh_type = SHT_STRTAB,
		.sh_flags = 0,
		.sh_addralign = 1
	};
	
	Elf64_Shdr code_section = {
		.sh_type = SHT_PROGBITS,
		.sh_flags = SHF_EXECINSTR | SHF_ALLOC,
		.sh_addralign = 16
	};
	
	Elf64_Shdr data_section = {
		.sh_type = SHT_PROGBITS,
		.sh_flags = SHF_ALLOC | SHF_WRITE,
		.sh_addralign = 16
	};
	
	Elf64_Shdr rodata_section = {
		.sh_type = SHT_PROGBITS,
		.sh_flags = SHF_ALLOC,
		.sh_addralign = 16
	};
	
	Elf64_Shdr bss_section = {
		.sh_type = SHT_NOBITS,
		.sh_flags = SHF_ALLOC | SHF_WRITE,
		.sh_addralign = 16
	};
	
	Elf64_Shdr stab_section = {
		.sh_type = SHT_SYMTAB,
		.sh_flags = 0,
		.sh_addralign = 1,
		.sh_link = 1,
		.sh_entsize = sizeof(Elf64_Sym)
	};
	
	// Calculate some section content sizes, it's important that we can
	// generate the final result with minimal effort so if everything
	// is in place we are doing great.
	
	// Section string table:
    TB_Emitter strtbl = { 0 };
	tb_out_reserve(&strtbl, 1024);
	{
		tb_out1b(&strtbl, 0);
		
		strtab_section.sh_name = strtbl.count;
		tb_outstr_UNSAFE(&strtbl, ".strtab");
		tb_out1b_UNSAFE(&strtbl, 0);
		
		code_section.sh_name = strtbl.count;
		tb_outstr_UNSAFE(&strtbl, ".text");
		tb_out1b_UNSAFE(&strtbl, 0);
		
		data_section.sh_name = strtbl.count;
		tb_outstr_UNSAFE(&strtbl, ".data");
		tb_out1b_UNSAFE(&strtbl, 0);
		
		rodata_section.sh_name = strtbl.count;
		tb_outstr_UNSAFE(&strtbl, ".rodata");
		tb_out1b_UNSAFE(&strtbl, 0);
		
		bss_section.sh_name = strtbl.count;
		tb_outstr_UNSAFE(&strtbl, ".bss");
		tb_out1b_UNSAFE(&strtbl, 0);
		
		stab_section.sh_name = strtbl.count;
		tb_outstr_UNSAFE(&strtbl, ".stab");
		tb_out1b_UNSAFE(&strtbl, 0);
	}
	
	// Code section:
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
		func_layout[i] = code_section.sh_size;
		
		uint64_t meta = out_f->prologue_epilogue_metadata;
		uint64_t stack_usage = out_f->stack_usage;
		
		size_t code_size = out_f->code_size;
		size_t prologue = code_gen->get_prologue_length(meta, stack_usage);
		size_t epilogue = code_gen->get_epilogue_length(meta, stack_usage);
		assert(prologue + epilogue < PROEPI_BUFFER);
		
		code_section.sh_size += prologue;
		code_section.sh_size += epilogue;
		code_section.sh_size += code_size;
	}
	
	// Target specific: resolve internal call patches
	code_gen->emit_call_patches(m, func_layout);
	TB_Emitter stab = { 0 };
	
	// Symbol table:
	{
		Elf64_Sym null_sym = { 0 };
		
		tb_out_reserve(&stab, sizeof(Elf64_Sym));
		tb_outs_UNSAFE(&stab, sizeof(Elf64_Sym), (uint8_t*)&null_sym);
	}
	
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
		
		// Fill up the symbol's string table
		const char* name = out_f->name;
		size_t name_len = strlen(name);
		size_t name_pos = strtbl.count;
		
		tb_out_reserve(&strtbl, name_len + 1);
		tb_outs_UNSAFE(&strtbl, name_len + 1, (uint8_t*)name);
		
		// Emit symbol
		size_t next_func = code_section.sh_size;
		if (i + 1 < m->compiled_functions.count) {
			next_func = func_layout[i + 1];
		}
		size_t func_size = next_func - func_layout[i];
		
		Elf64_Sym sym = {
			.st_name = name_pos,
			.st_info = ELF64_ST_BIND(1) | ELF64_ST_TYPE(2), // idk
			.st_shndx = 2, // .text
			.st_value = func_layout[i],
			.st_size = func_size
		};
		
		tb_out_reserve(&stab, sizeof(Elf64_Sym));
		tb_outs_UNSAFE(&stab, sizeof(Elf64_Sym), (uint8_t*)&sym);
	}
	
	stab_section.sh_size = stab.count;
	strtab_section.sh_size = strtbl.count;
	
	// Data section:
	data_section.sh_size = 16;
	
	// Read-only Data section:
	rodata_section.sh_size = 0;
	
	// Calculate file offsets
	strtab_section.sh_offset = sizeof(Elf64_Ehdr);
	code_section.sh_offset = strtab_section.sh_offset + strtab_section.sh_size;
	data_section.sh_offset = code_section.sh_offset + code_section.sh_size;
	rodata_section.sh_offset = data_section.sh_offset + data_section.sh_size;
	stab_section.sh_offset = rodata_section.sh_offset + rodata_section.sh_size;
	
	header.e_shoff = stab_section.sh_offset + stab_section.sh_size;
	
	// Output file
	FILE* f = fopen(path, "wb");
	fwrite(&header, sizeof(header), 1, f);
	
	assert(ftell(f) == strtab_section.sh_offset);
	fwrite(strtbl.data, strtbl.count, 1, f);
	
	assert(ftell(f) == code_section.sh_offset);
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
	
	assert(ftell(f) == data_section.sh_offset);
	for (size_t i = 0; i < 4; i++) {
		uint32_t x = 0x90909090;
		fwrite(&x, 4, 1, f);
	}
	
	assert(ftell(f) == rodata_section.sh_offset);
	
	assert(ftell(f) == stab_section.sh_offset);
	fwrite(stab.data, stab.count, 1, f);
	
	assert(ftell(f) == header.e_shoff);
	fwrite(&null_section, sizeof(null_section), 1, f);
	fwrite(&strtab_section, sizeof(strtab_section), 1, f);
	fwrite(&code_section, sizeof(code_section), 1, f);
	fwrite(&data_section, sizeof(data_section), 1, f);
	fwrite(&rodata_section, sizeof(rodata_section), 1, f);
	fwrite(&bss_section, sizeof(bss_section), 1, f);
	fwrite(&stab_section, sizeof(stab_section), 1, f);
	
	free(stab.data);
	free(strtbl.data);
	free(func_layout);
	fclose(f);
}
