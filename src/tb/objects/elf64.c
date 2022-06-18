#include "../tb_internal.h"

#define EI_MAG0       0
#define EI_MAG1       1
#define EI_MAG2       2
#define EI_MAG3       3
#define EI_CLASS      4  /* Class of machine. */
#define EI_DATA       5  /* Data format. */
#define EI_VERSION    6  /* ELF format version. */
#define EI_OSABI      7  /* Operating system / ABI identification */
#define EI_ABIVERSION 8  /* ABI version */
#define OLD_EI_BRAND  8  /* Start of architecture identification. */
#define EI_PAD        9  /* Start of padding (per SVR4 ABI). */
#define EI_NIDENT     16 /* Size of e_ident array. */

/* Values for e_type. */
#define ET_NONE   0      /* Unknown type. */
#define ET_REL    1      /* Relocatable. */
#define ET_EXEC   2      /* Executable. */
#define ET_DYN    3      /* Shared object. */
#define ET_CORE   4      /* Core file. */
#define ET_LOOS   0xfe00 /* First operating system specific. */
#define ET_HIOS   0xfeff /* Last operating system-specific. */
#define ET_LOPROC 0xff00 /* First processor-specific. */
#define ET_HIPROC 0xffff /* Last processor-specific. */

/* Values for e_machine. */
#define EM_NONE    0   /* Unknown machine. */
#define EM_X86_64  62  /* Advanced Micro Devices x86-64 */
#define EM_AARCH64 183 /* AArch64 (64-bit ARM) */

/* sh_type */
#define SHT_NULL     0 /* inactive */
#define SHT_PROGBITS 1 /* program defined information */
#define SHT_SYMTAB   2 /* symbol table section */
#define SHT_STRTAB   3 /* string table section */
#define SHT_RELA     4 /* relocation section with addends */
#define SHT_NOBITS   8 /* no space section */

/* Flags for sh_flags. */
#define SHF_WRITE            0x1        /* Section contains writable data. */
#define SHF_ALLOC            0x2        /* Section occupies memory. */
#define SHF_EXECINSTR        0x4        /* Section contains instructions. */
#define SHF_MERGE            0x10       /* Section may be merged. */
#define SHF_STRINGS          0x20       /* Section contains strings. */
#define SHF_INFO_LINK        0x40       /* sh_info holds section index. */
#define SHF_LINK_ORDER       0x80       /* Special ordering requirements. */
#define SHF_OS_NONCONFORMING 0x100      /* OS-specific processing required. */
#define SHF_GROUP            0x200      /* Member of section group. */
#define SHF_TLS              0x400      /* Section contains TLS data. */
#define SHF_MASKOS           0x0ff00000 /* OS-specific semantics. */
#define SHF_MASKPROC         0xf0000000 /* Processor-specific semantics. */

/* Values for relocation */
#define R_X86_64_NONE  0
#define R_X86_64_64    1
#define R_X86_64_PC32  2
#define R_X86_64_GOT32 3
#define R_X86_64_PLT32 4

typedef uint64_t Elf64_Addr;
typedef uint16_t Elf64_Half;
typedef uint64_t Elf64_Off;
typedef int32_t  Elf64_Sword;
typedef int64_t  Elf64_Sxword;
typedef uint32_t Elf64_Word;
typedef uint64_t Elf64_Lword;
typedef uint64_t Elf64_Xword;

/* Macros for accessing the fields of st_info. */
#define ELF64_ST_BIND(info) ((info) >> 4)
#define ELF64_ST_TYPE(info) ((info) & 0xf)

#define ELF64_ST_INFO(b, t) (((b) << 4) | ((t) & 0xF))

// http://web.mit.edu/freebsd/head/sys/sys/elf64.h
// https://cirosantilli.com/elf-hello-world#minimal-elf-file
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
#define EI_NIDENT 16

#define ELF64_R_SYM(i)     ((i) >> 32u)
#define ELF64_R_TYPE(i)    ((i)&0xffffffffULL)
#define ELF64_R_INFO(s, t) (((uint64_t)(s) << 32ULL) + ((uint64_t)(t) & 0xffffffffULL))

typedef struct {
    unsigned char e_ident[EI_NIDENT];
    Elf64_Half    e_type;
    Elf64_Half    e_machine;
    Elf64_Word    e_version;
    Elf64_Addr    e_entry;
    Elf64_Off     e_phoff;
    Elf64_Off     e_shoff;
    Elf64_Word    e_flags;
    Elf64_Half    e_ehsize;
    Elf64_Half    e_phentsize;
    Elf64_Half    e_phnum;
    Elf64_Half    e_shentsize;
    Elf64_Half    e_shnum;
    Elf64_Half    e_shstrndx;
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
    Elf64_Word    st_name;
    unsigned char st_info;
    unsigned char st_other;
    Elf64_Half    st_shndx;
    Elf64_Addr    st_value;
    Elf64_Xword   st_size;
} Elf64_Sym;

typedef struct {
    Elf64_Addr   r_offset;
    Elf64_Xword  r_info;
    Elf64_Sxword r_addend;
} Elf64_Rela;

typedef struct {
    Elf64_Addr  r_offset;
    Elf64_Xword r_info;
} Elf64_Rel;

static void put_symbol(TB_Emitter* strtbl, TB_Emitter* stab, const char* name, uint8_t sym_info, Elf64_Half section_index, Elf64_Addr value, Elf64_Xword size) {
	// Fill up the symbol's string table
	size_t name_len = strlen(name);
	size_t name_pos = strtbl->count;

	tb_out_reserve(strtbl, name_len + 1);
	tb_outs_UNSAFE(strtbl, name_len + 1, (uint8_t*)name);

	// Emit symbol
	Elf64_Sym sym = {
		.st_name  = name_pos,
		.st_info  = sym_info,
		.st_shndx = section_index,
		.st_value = value,
		.st_size  = size
	};

	tb_out_reserve(stab, sizeof(Elf64_Sym));
	tb_outs_UNSAFE(stab, sizeof(Elf64_Sym), (uint8_t*)&sym);
}

void tb_export_elf64(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, const IDebugFormat* debug_fmt) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // The prologue and epilogue generators need some storage
    uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc(m->functions.compiled_count * sizeof(uint32_t));

    uint32_t* external_symbol_relative_id = tb_tls_push(tls, TB_MAX_THREADS * sizeof(uint32_t));
    size_t symbol_id_counter = 0;

	// generate a mapping between external symbols and the symbol ids
	loop(i, m->max_threads) {
        external_symbol_relative_id[i] = symbol_id_counter;

        size_t external_len = arrlen(m->externals[i]);
        symbol_id_counter += external_len ? external_len - 1 : 0;
    }

	uint16_t machine;
    switch (m->target_arch) {
		case TB_ARCH_X86_64: machine = EM_X86_64; break;
		case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
		default: tb_todo();
    }

    // Generate the header structs
    const int  number_of_sections = 8;
    Elf64_Ehdr header = {
		.e_ident = {
			[EI_MAG0]          = 0x7F, // magic number
			[EI_MAG1]          = 'E',
			[EI_MAG2]          = 'L',
			[EI_MAG3]          = 'F',
			[EI_CLASS]         = 2, // 64bit ELF file
			[EI_DATA]          = 1, // little-endian
			[EI_VERSION]       = 1, // 1.0
			[EI_OSABI]         = 0,
			[EI_ABIVERSION]    = 0
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
        .e_shnum     = number_of_sections,
        .e_shstrndx  = 1
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

    Elf64_Shdr code_reloc_section = {
		.sh_type = SHT_RELA,
		.sh_flags = SHF_INFO_LINK,
		.sh_link = 7,
		.sh_info = 2,
		.sh_addralign = 16,
		.sh_entsize = sizeof(Elf64_Rela)
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
		.sh_flags = 0, .sh_addralign = 1,
		.sh_link = 1, .sh_info = number_of_sections + 1,
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

        code_reloc_section.sh_name = strtbl.count;
        tb_outstr_UNSAFE(&strtbl, ".rela.text");
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
    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        func_layout[i]           = code_section.sh_size;
        if (!out_f) continue;

        uint64_t meta        = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;

        size_t code_size = out_f->code_size;
        size_t prologue  = code_gen->get_prologue_length(meta, stack_usage);
        size_t epilogue  = code_gen->get_epilogue_length(meta, stack_usage);
        assert(prologue + epilogue < PROEPI_BUFFER);

        code_section.sh_size += prologue;
        code_section.sh_size += epilogue;
        code_section.sh_size += code_size;
    }

	loop(i, m->max_threads) {
        code_reloc_section.sh_size += arrlen(m->ecall_patches[i]) * sizeof(Elf64_Rela);
    	code_reloc_section.sh_size += arrlen(m->const_patches[i]) * sizeof(Elf64_Rela);
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

	static const char* SECTION_NAMES[] = {
		NULL, ".strtab", ".text", ".rela.text", ".data", ".rodata", ".bss", ".symtab"
	};

	loop_range(i, 1, number_of_sections) {
		put_symbol(&strtbl, &stab, SECTION_NAMES[i], ELF64_ST_INFO(0, 3), i, 0, 0);
	}

	loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        if (!out_f) continue;

		// calculate size
		size_t next_func = code_section.sh_size;
		if (i + 1 < m->functions.count) { next_func = func_layout[i + 1]; }
		size_t func_size = next_func - func_layout[i];

		put_symbol(&strtbl, &stab, m->functions.data[i].name, ELF64_ST_INFO(1, 2), 2, func_layout[i], func_size);
    }

	loop(i, m->max_threads) {
        loop_range(j, 1, arrlen(m->externals[i])) {
            const TB_External* restrict e = &m->externals[i][j];

			put_symbol(&strtbl, &stab, e->name, ELF64_ST_INFO(1, 0), 0, 0, 0);
        }
    }

    stab_section.sh_size   = stab.count;
    strtab_section.sh_size = strtbl.count;

    // Data section:
    data_section.sh_size = m->data_region_size;

    // Read-only Data section:
    rodata_section.sh_size = m->rdata_region_size;

    // Calculate file offsets
    strtab_section.sh_offset     = sizeof(Elf64_Ehdr);
    code_section.sh_offset       = strtab_section.sh_offset + strtab_section.sh_size;
    code_reloc_section.sh_offset = code_section.sh_offset + code_section.sh_size;
    data_section.sh_offset       = code_reloc_section.sh_offset + code_reloc_section.sh_size;
    rodata_section.sh_offset     = data_section.sh_offset + data_section.sh_size;
    stab_section.sh_offset       = rodata_section.sh_offset + rodata_section.sh_size;

    header.e_shoff = stab_section.sh_offset + stab_section.sh_size;

    // Output file
    FILE* f = fopen(path, "wb");
    fwrite(&header, sizeof(header), 1, f);

	assert(ftell(f) == strtab_section.sh_offset);
    fwrite(strtbl.data, strtbl.count, 1, f);

	assert(ftell(f) == code_section.sh_offset);
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

		printf("%s: %zu %zu %zu\n", m->functions.data[i].name, prologue_len, epilogue_len, code_size);
        fwrite(prologue, prologue_len, 1, f);
        fwrite(code, code_size, 1, f);
        fwrite(epilogue, epilogue_len, 1, f);
    }

	assert(ftell(f) == code_reloc_section.sh_offset);
	uint64_t external_symbol_baseline = number_of_sections + m->functions.count;
	loop(i, m->max_threads) {
        loop(j, arrlen(m->ecall_patches[i])) {
            TB_ExternFunctionPatch* p = &m->ecall_patches[i][j];
            TB_FunctionOutput* out_f  = p->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] +
				code_gen->get_prologue_length(meta, stack_usage) + p->pos;

            TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;

            uint64_t symbol_id = external_symbol_relative_id[p->target_id / per_thread_stride];

            // Workaround since each external set has a null slot
            uint64_t local_id = (p->target_id % per_thread_stride);
            if (local_id) local_id--;
            symbol_id += local_id;
			symbol_id += external_symbol_baseline;

			Elf64_Rela rela = {
				.r_offset = actual_pos,
				.r_info   = ELF64_R_INFO(symbol_id, R_X86_64_PLT32),
				.r_addend = -4
			};
            fwrite(&rela, sizeof(rela), 1, f);
        }
    }

	loop(i, m->max_threads) {
        loop(j, arrlen(m->const_patches[i])) {
            TB_ConstPoolPatch* p     = &m->const_patches[i][j];
            TB_FunctionOutput* out_f = p->source->output;

            uint64_t meta        = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] +
				code_gen->get_prologue_length(meta, stack_usage) + p->pos;

			Elf64_Rela rela = {
				.r_offset = actual_pos,
				.r_info   = ELF64_R_INFO(5, R_X86_64_PLT32), /* 5 is .rodata section */
				.r_addend = -4
			};
            fwrite(&rela, sizeof(rela), 1, f);
        }
    }

	assert(ftell(f) == data_section.sh_offset);
    if (m->data_region_size) {
        // TODO(NeGate): Optimize this for size and speed, sometimes
        // there's huge sections will be filled with just empty regions
        // so it's probably best not to represent everything in a big
        // buffer.
        char* data = tb_platform_heap_alloc(m->data_region_size);

		loop(i, m->max_threads) {
            loop(j, arrlen(m->globals[i])) {
                TB_Global* g = &m->globals[i][j];
				if (g->storage != TB_STORAGE_DATA) continue;

                TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
                TB_Initializer*  init = (TB_Initializer*)&m->initializers[g->init / per_thread_stride][g->init % per_thread_stride];

                // clear out space
                memset(&data[g->pos], 0, init->size);

                loop(k, init->obj_count) if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                    memcpy(&data[g->pos + init->objects[k].offset], init->objects[k].region.ptr, init->objects[k].region.size);
                }
            }
        }

        fwrite(data, m->data_region_size, 1, f);
        tb_platform_heap_free(data);
    }

	assert(ftell(f) == rodata_section.sh_offset);
    {
        char* rdata = tb_platform_heap_alloc(m->rdata_region_size);

		// we reserve the first 16 bytes... for literally no reason
		memset(rdata, 0, 16);

        loop(i, m->max_threads) {
            loop(j, arrlen(m->const_patches[i])) {
                TB_ConstPoolPatch* p = &m->const_patches[i][j];
                memcpy(&rdata[p->rdata_pos], p->data, p->length);
            }
        }

        fwrite(rdata, m->rdata_region_size, 1, f);
        tb_platform_heap_free(rdata);
    }

    assert(ftell(f) == stab_section.sh_offset);
    fwrite(stab.data, stab.count, 1, f);

    assert(ftell(f) == header.e_shoff);
    fwrite(&null_section,       sizeof(null_section), 1, f);
    fwrite(&strtab_section,     sizeof(strtab_section), 1, f);
    fwrite(&code_section,       sizeof(code_section), 1, f);
    fwrite(&code_reloc_section, sizeof(code_section), 1, f);
    fwrite(&data_section,       sizeof(data_section), 1, f);
    fwrite(&rodata_section,     sizeof(rodata_section), 1, f);
    fwrite(&bss_section,        sizeof(bss_section), 1, f);
    fwrite(&stab_section,       sizeof(stab_section), 1, f);

    tb_platform_heap_free(stab.data);
    tb_platform_heap_free(strtbl.data);
    tb_platform_heap_free(func_layout);
    fclose(f);
}
