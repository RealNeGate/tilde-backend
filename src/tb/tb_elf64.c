#define TB_INTERNAL
#include "tb.h"

#define	EI_MAG0			0
#define	EI_MAG1			1
#define	EI_MAG2			2
#define	EI_MAG3			3
#define	EI_CLASS		4	/* Class of machine. */
#define	EI_DATA			5	/* Data format. */
#define	EI_VERSION		6	/* ELF format version. */
#define	EI_OSABI		7	/* Operating system / ABI identification */
#define	EI_ABIVERSION	8	/* ABI version */
#define	OLD_EI_BRAND	8	/* Start of architecture identification. */
#define	EI_PAD			9	/* Start of padding (per SVR4 ABI). */
#define	EI_NIDENT		16	/* Size of e_ident array. */

/* Values for e_type. */
#define	ET_NONE		0	/* Unknown type. */
#define	ET_REL		1	/* Relocatable. */
#define	ET_EXEC		2	/* Executable. */
#define	ET_DYN		3	/* Shared object. */
#define	ET_CORE		4	/* Core file. */
#define	ET_LOOS		0xfe00	/* First operating system specific. */
#define	ET_HIOS		0xfeff	/* Last operating system-specific. */
#define	ET_LOPROC	0xff00	/* First processor-specific. */
#define	ET_HIPROC	0xffff	/* Last processor-specific. */

/* Values for e_machine. */
#define	EM_NONE		0	/* Unknown machine. */
#define	EM_X86_64	62	/* Advanced Micro Devices x86-64 */
#define	EM_AARCH64	183	/* AArch64 (64-bit ARM) */

/* sh_type */
#define	SHT_NULL		0	/* inactive */
#define	SHT_PROGBITS	1	/* program defined information */
#define	SHT_SYMTAB		2	/* symbol table section */
#define	SHT_STRTAB		3	/* string table section */
#define	SHT_NOBITS		8	/* no space section */

/* Flags for sh_flags. */
#define	SHF_WRITE				0x1	/* Section contains writable data. */
#define	SHF_ALLOC				0x2	/* Section occupies memory. */
#define	SHF_EXECINSTR			0x4	/* Section contains instructions. */
#define	SHF_MERGE				0x10	/* Section may be merged. */
#define	SHF_STRINGS				0x20	/* Section contains strings. */
#define	SHF_INFO_LINK			0x40	/* sh_info holds section index. */
#define	SHF_LINK_ORDER			0x80	/* Special ordering requirements. */
#define	SHF_OS_NONCONFORMING	0x100	/* OS-specific processing required. */
#define	SHF_GROUP				0x200	/* Member of section group. */
#define	SHF_TLS					0x400	/* Section contains TLS data. */
#define	SHF_MASKOS				0x0ff00000	/* OS-specific semantics. */
#define	SHF_MASKPROC			0xf0000000	/* Processor-specific semantics. */

typedef uint64_t	Elf64_Addr;
typedef uint16_t	Elf64_Half;
typedef uint64_t	Elf64_Off;
typedef int32_t		Elf64_Sword;
typedef int64_t		Elf64_Sxword;
typedef uint32_t	Elf64_Word;
typedef uint64_t	Elf64_Lword;
typedef uint64_t	Elf64_Xword;

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

void tb_export_elf64(TB_Module* m, TB_Arch arch, FILE* f) {
	char strtbl[1024] = { 0 };
	
	uint16_t machine;
	switch (arch) {
		case TB_ARCH_X86_64: machine = EM_X86_64; break;
		case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
		default: tb_unreachable();
	}
	
	// Generate the header structs
	const int number_of_sections = 6;
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
		
		// section headers go right after the ELF header
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
	
	// Calculate some section content sizes, it's important that we can
	// generate the final result with minimal effort so if everything
	// is in place we are doing great.
	
	// Section string table:
	{
		size_t strtlb_curr = 1;
		
		strtab_section.sh_name = strtlb_curr;
		strcpy(&strtbl[strtlb_curr], ".strtab");
		strtlb_curr += sizeof(".strtab");
		
		code_section.sh_name = strtlb_curr;
		strcpy(&strtbl[strtlb_curr], ".text");
		strtlb_curr += sizeof(".text");
		
		data_section.sh_name = strtlb_curr;
		strcpy(&strtbl[strtlb_curr], ".data");
		strtlb_curr += sizeof(".data");
		
		rodata_section.sh_name = strtlb_curr;
		strcpy(&strtbl[strtlb_curr], ".rodata");
		strtlb_curr += sizeof(".rodata");
		
		bss_section.sh_name = strtlb_curr;
		strcpy(&strtbl[strtlb_curr], ".bss");
		strtlb_curr += sizeof(".bss");
		
		strtab_section.sh_size = strtlb_curr;
	}
	
	// Code section:
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		code_section.sh_size += m->compiled_functions.data[i].emitter.count;
	}
	
	// Data section:
	data_section.sh_size = 16;
	
	// Read-only Data section:
	rodata_section.sh_size = 0;
	
	// Calculate file offsets
	strtab_section.sh_offset = sizeof(Elf64_Ehdr);
	code_section.sh_offset = strtab_section.sh_offset + strtab_section.sh_size;
	data_section.sh_offset = code_section.sh_offset + code_section.sh_size;
	rodata_section.sh_offset = data_section.sh_offset + data_section.sh_size;
	
	header.e_shoff = rodata_section.sh_offset + rodata_section.sh_size;
	
	// Output file
	fwrite(&header, sizeof(header), 1, f);
	
	assert(ftell(f) == strtab_section.sh_offset);
	fwrite(strtbl, strtab_section.sh_size, 1, f);
	
	assert(ftell(f) == code_section.sh_offset);
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		fwrite(m->compiled_functions.data[i].emitter.data, m->compiled_functions.data[i].emitter.count, 1, f);
	}
	
	assert(ftell(f) == data_section.sh_offset);
	for (size_t i = 0; i < 4; i++) {
		uint32_t x = 0x69696969;
		fwrite(&x, 4, 1, f);
	}
	
	assert(ftell(f) == rodata_section.sh_offset);
	
	assert(ftell(f) == header.e_shoff);
	fwrite(&null_section, sizeof(null_section), 1, f);
	fwrite(&strtab_section, sizeof(strtab_section), 1, f);
	fwrite(&code_section, sizeof(code_section), 1, f);
	fwrite(&data_section, sizeof(data_section), 1, f);
	fwrite(&rodata_section, sizeof(rodata_section), 1, f);
	fwrite(&bss_section, sizeof(bss_section), 1, f);
}
