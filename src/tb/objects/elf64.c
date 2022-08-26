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

// ST_INFO
#define ELF64_STB_LOCAL  0
#define ELF64_STB_GLOBAL 1
#define ELF64_STB_WEAK   2

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

// my section numbers in TB_ModuleExporterELF.sections
enum {
    S_NULL,
    S_STRTAB,
    S_TEXT,
    S_TEXT_REL,
    S_DATA,
    S_RODATA,
    S_BSS,
    S_STAB,
    S_MAX
};

typedef struct TB_ModuleExporterELF {
    enum {
        STAGE__WRITE_FILE_HEADER,
        STAGE__WRITE_STRING_TABLE,
        STAGE__WRITE_TEXT_SECTION,
        STAGE__WRITE_TEXT_PATCHES,
        STAGE__WRITE_DATA_SECTION,
        STAGE__WRITE_RDATA_SECTION,
        STAGE__WRITE_STAB_SECTION,
        STAGE__WRITE_SECTION_HEADERS,
        STAGE__DONE,
    } stage;
    // depends on the stage, go check the code for them
    size_t tick[2];
    size_t write_pos;

    // temporary memory allocation, usually just used to store the contents
    // of file writing
    bool alloc_request_ongoing;
    void* temporary_memory;

    // [m->functions.count + 1] last slot is the size of the text section
    uint32_t* func_layout;

    Elf64_Ehdr header;
    union {
        Elf64_Shdr sections[S_MAX];
        struct {
            Elf64_Shdr null_section;
            Elf64_Shdr strtab_section;
            Elf64_Shdr code_section;
            Elf64_Shdr code_reloc_section;
            Elf64_Shdr data_section;
            Elf64_Shdr rodata_section;
            Elf64_Shdr bss_section;
            Elf64_Shdr stab_section;
        };
    };

    size_t function_sym_start;
    size_t external_sym_start;

    size_t string_table_pos;
    size_t tls_section_num;

    TB_Emitter strtbl, stab;
    uint8_t proepi_buffer[PROEPI_BUFFER];
} TB_ModuleExporterELF;

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

static void send_write_message(TB_ModuleExporterELF* e, TB_ModuleExportPacket* packet, const void* data, size_t length) {
    packet->type = TB_EXPORT_PACKET_WRITE;
    packet->write.length = length;
    packet->write.data = data;

    e->write_pos += length;
}

// it'll return false once it's got a valid memory region in e->temporary_memory
// if it returns true then the coroutine should return to the user
static bool send_alloc_message(TB_ModuleExporterELF* e, TB_ModuleExportPacket* packet, size_t request_size) {
    if (e->temporary_memory == NULL) {
        if (!e->alloc_request_ongoing) {
            e->alloc_request_ongoing = true;

            packet->type = TB_EXPORT_PACKET_ALLOC;
            packet->alloc.request_size = request_size;
            packet->alloc.memory = NULL;
            return true;
        } else {
            assert(packet->alloc.memory != NULL && "expected valid memory region");

            e->alloc_request_ongoing = false;
            e->temporary_memory = packet->alloc.memory;
        }
    }

    return false;
}

TB_ModuleExporter* tb_elf64__make(TB_Module* m) {
    return memset(tb_platform_heap_alloc(sizeof(TB_ModuleExporterELF)), 0, sizeof(TB_ModuleExporterELF));
}

bool tb_elf64__next(TB_Module* m, TB_ModuleExporter* exporter, TB_ModuleExportPacket* packet) {
    TB_ModuleExporterELF* restrict e = (TB_ModuleExporterELF*) exporter;

    switch (e->stage) {
        case STAGE__WRITE_FILE_HEADER: {
            // tally up .data relocations
            /*uint32_t data_relocation_count = 0;

            FOREACH_N(t, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[t].globals) {
                    TB_Initializer* init = g->init;
                    FOREACH_N(k, 0, init->obj_count) {
                        data_relocation_count += (init->objects[k].type != TB_INIT_OBJ_REGION);
                    }
                }
            }*/

            int number_of_sections = 8;
            assert(number_of_sections == S_MAX);

            // mark each with a unique id
            e->function_sym_start = number_of_sections;
            e->external_sym_start = e->function_sym_start + m->functions.compiled_count;

            size_t unique_id_counter = 0;
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, ext, m->thread_info[i].externals) {
                    int id = e->external_sym_start + unique_id_counter;
                    ext->address = (void*) (uintptr_t) id;
                    unique_id_counter += 1;
                }

                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    g->id = e->external_sym_start + unique_id_counter;
                    unique_id_counter += 1;
                }
            }

            uint16_t machine;
            switch (m->target_arch) {
                case TB_ARCH_X86_64: machine = EM_X86_64; break;
                case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
                default: tb_todo();
            }

            e->header = (Elf64_Ehdr){
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

            e->strtab_section = (Elf64_Shdr){
                .sh_type = SHT_STRTAB,
                .sh_flags = 0,
                .sh_addralign = 1
            };

            e->code_section = (Elf64_Shdr){
                .sh_type = SHT_PROGBITS,
                .sh_flags = SHF_EXECINSTR | SHF_ALLOC,
                .sh_addralign = 16
            };

            e->code_reloc_section = (Elf64_Shdr){
                .sh_type = SHT_RELA,
                .sh_flags = SHF_INFO_LINK,
                .sh_link = 7,
                .sh_info = 2,
                .sh_addralign = 16,
                .sh_entsize = sizeof(Elf64_Rela)
            };

            e->data_section = (Elf64_Shdr){
                .sh_type = SHT_PROGBITS,
                .sh_flags = SHF_ALLOC | SHF_WRITE,
                .sh_addralign = 16
            };

            e->rodata_section = (Elf64_Shdr){
                .sh_type = SHT_PROGBITS,
                .sh_flags = SHF_ALLOC,
                .sh_addralign = 16
            };

            e->bss_section = (Elf64_Shdr){
                .sh_type = SHT_NOBITS,
                .sh_flags = SHF_ALLOC | SHF_WRITE,
                .sh_addralign = 16
            };

            e->stab_section = (Elf64_Shdr){
                .sh_type = SHT_SYMTAB,
                .sh_flags = 0, .sh_addralign = 1,
                .sh_link = 1, .sh_info = e->header.e_shnum,
                .sh_entsize = sizeof(Elf64_Sym)
            };

            // Section string table:
            TB_Emitter strtbl = { 0 };
            tb_out_reserve(&strtbl, 1024);
            {
                tb_out1b(&strtbl, 0);

                e->strtab_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".strtab");
                tb_out1b_UNSAFE(&strtbl, 0);

                e->code_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".text");
                tb_out1b_UNSAFE(&strtbl, 0);

                e->code_reloc_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".rela.text");
                tb_out1b_UNSAFE(&strtbl, 0);

                e->data_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".data");
                tb_out1b_UNSAFE(&strtbl, 0);

                e->rodata_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".rodata");
                tb_out1b_UNSAFE(&strtbl, 0);

                e->bss_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".bss");
                tb_out1b_UNSAFE(&strtbl, 0);

                e->stab_section.sh_name = strtbl.count;
                tb_outstr_UNSAFE(&strtbl, ".stab");
                tb_out1b_UNSAFE(&strtbl, 0);
            }

            // Code section
            e->func_layout = tb_platform_heap_alloc((m->functions.count + 1) * sizeof(uint32_t));

            const ICodeGen* restrict code_gen = tb__find_code_generator(m);

            e->code_section.sh_size = 0;
            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                e->func_layout[i] = e->code_section.sh_size;
                if (!out_f) continue;

                uint64_t meta = out_f->prologue_epilogue_metadata;
                uint64_t stack_usage = out_f->stack_usage;

                size_t code_size = out_f->code_size;
                size_t prologue = code_gen->get_prologue_length(meta, stack_usage);
                size_t epilogue = code_gen->get_epilogue_length(meta, stack_usage);
                assert(prologue + epilogue < PROEPI_BUFFER);

                e->code_section.sh_size += prologue;
                e->code_section.sh_size += epilogue;
                e->code_section.sh_size += code_size;
            }
            e->func_layout[m->functions.count] = e->code_section.sh_size;

            FOREACH_N(i, 0, m->max_threads) {
                e->code_reloc_section.sh_size += dyn_array_length(m->thread_info[i].ecall_patches) * sizeof(Elf64_Rela);
                e->code_reloc_section.sh_size += dyn_array_length(m->thread_info[i].const_patches) * sizeof(Elf64_Rela);
            }

            // Target specific: resolve internal call patches
            code_gen->emit_call_patches(m, e->func_layout);

            // write symbol table
            TB_Emitter stab = { 0 };
            {
                Elf64_Sym null_sym = { 0 };
                tb_out_reserve(&stab, sizeof(Elf64_Sym));
                tb_outs_UNSAFE(&stab, sizeof(Elf64_Sym), (uint8_t*)&null_sym);
            }

            static const char* SECTION_NAMES[] = {
                NULL, ".strtab", ".text", ".rela.text", ".data", ".rodata", ".bss", ".symtab"
            };

            FOREACH_N(i, 1, number_of_sections) {
                put_symbol(&strtbl, &stab, SECTION_NAMES[i], ELF64_ST_INFO(ELF64_STB_LOCAL, 3), i, 0, 0);
            }

            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                if (!out_f) continue;

                // calculate size
                size_t func_size = e->func_layout[i + 1] - e->func_layout[i];
                put_symbol(&strtbl, &stab, m->functions.data[i].name, ELF64_ST_INFO(ELF64_STB_GLOBAL, 2), 2, e->func_layout[i], func_size);
            }

            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, e, m->thread_info[i].externals) {
                    put_symbol(&strtbl, &stab, e->name, ELF64_ST_INFO(ELF64_STB_GLOBAL, 0), 0, 0, 0);
                }
            }

            // set some sizes and pass the stab and string table to the context
            e->stab                   = stab;
            e->strtbl                 = strtbl;
            e->stab_section.sh_size   = e->stab.count;
            e->strtab_section.sh_size = e->strtbl.count;
            e->data_section.sh_size   = m->data_region_size;
            e->rodata_section.sh_size = m->rdata_region_size;

            // Calculate file offsets
            size_t counter = sizeof(Elf64_Ehdr);
            FOREACH_N(i, 0, S_MAX) {
                e->sections[i].sh_offset = counter;
                counter += e->sections[i].sh_size;
            }
            e->header.e_shoff = counter;

            ////////////////////////////////
            // advance state & write out file header
            ////////////////////////////////
            send_write_message(e, packet, &e->header, sizeof(Elf64_Ehdr));
            e->stage += 1;
            return true;
        }
        case STAGE__WRITE_STRING_TABLE: {
            send_write_message(e, packet, e->strtbl.data, e->strtbl.count);
            e->stage += 1;
            return true;
        }
        case STAGE__WRITE_TEXT_SECTION: {
            const ICodeGen* restrict code_gen = tb__find_code_generator(m);

            TB_FunctionOutput* out_f = m->functions.data[e->tick[0]].output;
            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            // tick[0]
            //   is the function index
            //
            // tick[1]
            //   0 is prologue
            //   1 is body
            //   2 is epilogue
            switch (e->tick[1]++) {
                case 0: {
                    assert(e->write_pos == e->code_section.sh_offset + e->func_layout[e->tick[0]]);

                    size_t len = code_gen->emit_prologue(e->proepi_buffer, meta, stack_usage);
                    send_write_message(e, packet, e->proepi_buffer, len);
                    break;
                }

                case 1: {
                    send_write_message(e, packet, out_f->code, out_f->code_size);
                    break;
                }

                case 2: {
                    size_t len = code_gen->emit_epilogue(e->proepi_buffer, meta, stack_usage);
                    send_write_message(e, packet, e->proepi_buffer, len);

                    // next compiled function
                    size_t i = e->tick[0] + 1;
                    while (i < m->functions.count && m->functions.data[i].output == NULL) {
                        i += 1;
                    }
                    e->tick[0] = i;
                    e->tick[1] = 0;

                    if (i >= m->functions.count) {
                        // reset tickers and advance
                        e->tick[0] = 0;
                        e->stage += 1;
                    }
                    break;
                }
            }

            break;
        }
        case STAGE__WRITE_TEXT_PATCHES: {
            // *beg* the user for free memory
            if (send_alloc_message(e, packet, e->code_reloc_section.sh_size)) return true;
            assert(e->write_pos == e->code_reloc_section.sh_offset);

            const ICodeGen* restrict code_gen = tb__find_code_generator(m);
            uint64_t external_symbol_baseline = e->external_sym_start;

            TB_FIXED_ARRAY(Elf64_Rela) relocs = {
                .cap = e->code_reloc_section.sh_size / sizeof(Elf64_Rela),
                .elems = e->temporary_memory
            };

            FOREACH_N(i, 0, m->max_threads) {
                FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].ecall_patches)) {
                    TB_ExternFunctionPatch* p = &m->thread_info[i].ecall_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    uint64_t meta = out_f->prologue_epilogue_metadata;
                    uint64_t stack_usage = out_f->stack_usage;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data] +
                        code_gen->get_prologue_length(meta, stack_usage) + p->pos;

                    int symbol_id = external_symbol_baseline + (uintptr_t) p->target->address;
                    Elf64_Rela rela = {
                        .r_offset = actual_pos,
                        .r_info   = ELF64_R_INFO(symbol_id, R_X86_64_PLT32),
                        .r_addend = -4
                    };
                    TB_FIXED_ARRAY_APPEND(relocs, rela);
                }

                FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    uint64_t meta = out_f->prologue_epilogue_metadata;
                    uint64_t stack_usage = out_f->stack_usage;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data] +
                        code_gen->get_prologue_length(meta, stack_usage) + p->pos;

                    Elf64_Rela rela = {
                        .r_offset = actual_pos,
                        .r_info   = ELF64_R_INFO(5, R_X86_64_PLT32), /* 5 is .rodata section */
                        .r_addend = -4
                    };
                    TB_FIXED_ARRAY_APPEND(relocs, rela);
                }
            }

            send_write_message(e, packet, relocs.elems, relocs.count * sizeof(Elf64_Rela));
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_DATA_SECTION: {
            if (send_alloc_message(e, packet, m->rdata_region_size)) return true;
            char* data = e->temporary_memory;

            assert(e->write_pos == e->data_section.sh_offset);
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    if (g->storage != TB_STORAGE_DATA) continue;

                    TB_Initializer* init = g->init;

                    // clear out space
                    memset(&data[g->pos], 0, init->size);

                    FOREACH_N(k, 0, init->obj_count) {
                        if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                            memcpy(&data[g->pos + init->objects[k].offset], init->objects[k].region.ptr, init->objects[k].region.size);
                        }
                    }
                }
            }

            send_write_message(e, packet, data, m->data_region_size);
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_RDATA_SECTION: {
            if (send_alloc_message(e, packet, m->rdata_region_size)) return true;
            char* rdata = e->temporary_memory;

            assert(e->write_pos == e->rodata_section.sh_offset);
            loop(i, m->max_threads) {
                loop(j, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    memcpy(&rdata[p->rdata_pos], p->data, p->length);
                }
            }

            send_write_message(e, packet, rdata, m->rdata_region_size);
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_STAB_SECTION: {
            assert(e->write_pos == e->stab_section.sh_offset);
            send_write_message(e, packet, e->stab.data, e->stab.count);
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_SECTION_HEADERS: {
            assert(e->write_pos == e->header.e_shoff);
            send_write_message(e, packet, e->sections, S_MAX * sizeof(Elf64_Shdr));
            e->stage += 1;
            return true;
        }
        case STAGE__DONE: {
            tb_platform_heap_free(e->strtbl.data);
            tb_platform_heap_free(e->stab.data);
            tb_platform_heap_free(e->func_layout);
            tb_platform_heap_free(e);
            return false;
        }
        default: tb_todo();
    }

    // reset temporary allocation
    e->temporary_memory = NULL;
    return true;
}
