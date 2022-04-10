#include "tb_coff.h"

// leftrotate function definition
#define LEFTROTATE(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

static const uint32_t crc_table[256] = { 0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419,
    0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988, 0x09b64c2b,
    0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de, 0x1adad47d,
    0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f,
    0x63066cd9, 0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172, 0x3c03e4d1,
    0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3,
    0x45df5c75, 0xdcd60dcf, 0xabd13d59, 0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5,
    0x56b3c423, 0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924, 0x2f6f7c87,
    0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589,
    0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb,
    0x086d3d2d, 0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed,
    0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf,
    0x15da2d49, 0x8cd37cf3, 0xfbd44c65, 0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541,
    0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73,
    0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086, 0x5768b525,
    0x206f85b3, 0xb966d409, 0xce61e49f, 0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17,
    0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a, 0xead54739,
    0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b,
    0x9309ff9d, 0x0a00ae27, 0x7d079eb1, 0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d,
    0x806567cb, 0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc, 0xf9b9df6f,
    0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1,
    0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3,
    0xa867df55, 0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236, 0xcc0c7795,
    0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7,
    0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d, 0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9,
    0xeb0e363f, 0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b,
    0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e, 0x81be16cd,
    0xf6b9265b, 0x6fb077e1, 0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff,
    0xf862ae69, 0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661,
    0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0, 0xa9bcae53,
    0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9, 0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605,
    0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94, 0xb40bbe37,
    0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d };

// shamelessly ripped from LLVM:
// https://llvm.org/doxygen/CRC_8cpp_source.html#l00103
static uint32_t crc32(uint32_t crc, size_t length, uint8_t* data) {
    crc ^= 0xFFFFFFFFU;
    loop(i, length) {
        int table_index = (crc ^ data[i]) & 0xff;
        crc             = crc_table[table_index] ^ (crc >> 8);
    }
    return crc ^ 0xFFFFFFFFU;
}

// These vars will contain the hash
uint32_t h0, h1, h2, h3;

// https://gist.github.com/creationix/4710780
void md5(uint8_t* initial_msg, size_t initial_len) {

    // Message (to prepare)
    uint8_t* msg = NULL;

    // Note: All variables are unsigned 32 bit and wrap modulo 2^32 when
    // calculating

    // r specifies the per-round shift amounts

    uint32_t r[] = { 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9,
        14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16,
        23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21 };

    // Use binary integer part of the sines of integers (in radians) as
    // constants// Initialize variables:
    uint32_t k[] = { 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a,
        0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122,
        0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87,
        0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681,
        0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6,
        0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d,
        0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235,
        0x2ad7d2bb, 0xeb86d391 };

    h0 = 0x67452301;
    h1 = 0xefcdab89;
    h2 = 0x98badcfe;
    h3 = 0x10325476;

    // Pre-processing: adding a single 1 bit
    // append "1" bit to message
    /* Notice: the input bytes are considered as bits strings,
       where the first bit is the most significant bit of the byte.[37] */

    // Pre-processing: padding with zeros
    // append "0" bit until message length in bit ≡ 448 (mod 512)
    // append length mod (2 pow 64) to message

    int new_len = ((((initial_len + 8) / 64) + 1) * 64) - 8;

    msg = calloc(new_len + 64, 1); // also appends "0" bits
                                   // (we alloc also 64 extra bytes...)
    memcpy(msg, initial_msg, initial_len);
    msg[initial_len] = 128; // write the "1" bit

    uint32_t bits_len = 8 * initial_len; // note, we append the len
    memcpy(msg + new_len, &bits_len, 4); // in bits at the end of the buffer

    // Process the message in successive 512-bit chunks:
    // for each 512-bit chunk of message:
    int offset;
    for (offset = 0; offset < new_len; offset += (512 / 8)) {

        // break chunk into sixteen 32-bit words w[j], 0 ≤ j ≤ 15
        uint32_t* w = (uint32_t*)(msg + offset);

        // Initialize hash value for this chunk:
        uint32_t a = h0;
        uint32_t b = h1;
        uint32_t c = h2;
        uint32_t d = h3;

        // Main loop:
        uint32_t i;
        for (i = 0; i < 64; i++) {
            uint32_t f, g;

            if (i < 16) {
                f = (b & c) | ((~b) & d);
                g = i;
            } else if (i < 32) {
                f = (d & b) | ((~d) & c);
                g = (5 * i + 1) % 16;
            } else if (i < 48) {
                f = b ^ c ^ d;
                g = (3 * i + 5) % 16;
            } else {
                f = c ^ (b | (~d));
                g = (7 * i) % 16;
            }

            uint32_t temp = d;
            d             = c;
            c             = b;

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

    size_t         len  = file_stats.st_size;
    unsigned char* data = malloc(len + 17);

    fseek(file, 0, SEEK_SET);
    fread(data, 1, len, file);

    md5(data, len);

    uint8_t* p;

    p            = (uint8_t*)&h0;
    out_bytes[0] = p[0];
    out_bytes[1] = p[1];
    out_bytes[2] = p[2];
    out_bytes[3] = p[3];

    p            = (uint8_t*)&h1;
    out_bytes[4] = p[0];
    out_bytes[5] = p[1];
    out_bytes[6] = p[2];
    out_bytes[7] = p[3];

    p             = (uint8_t*)&h2;
    out_bytes[8]  = p[0];
    out_bytes[9]  = p[1];
    out_bytes[10] = p[2];
    out_bytes[11] = p[3];

    p             = (uint8_t*)&h3;
    out_bytes[12] = p[0];
    out_bytes[13] = p[1];
    out_bytes[14] = p[2];
    out_bytes[15] = p[3];

    fclose(file);
}

static uint16_t get_codeview_type(TB_DataType dt) {
    assert(dt.width == 0 && "TODO: implement vector types in CodeView output");
    switch (dt.type) {
    case TB_VOID: return 0x0003; // T_VOID
    case TB_BOOL: return 0x0030; // T_BOOL08
    case TB_I8:  return 0x0020;   // T_UCHAR
    case TB_I16: return 0x0073;  // T_UINT4
    case TB_I32: return 0x0075;  // T_UINT4
    case TB_I64: return 0x0023;  // T_UQUAD
    case TB_F32: return 0x0040;  // T_REAL32
    case TB_F64: return 0x0041;  // T_REAL64
    case TB_PTR: return 0x0023;  // T_64PUCHAR
    default: assert(0 && "TODO: missing type in CodeView output");
    }

    return 0x0003; // T_VOID
}

static uint16_t find_or_make_cv_type(TB_Emitter* sect, uint32_t* type_entry_count,
    CV_TypeEntry* lookup_table, size_t length, uint16_t* key) {
    assert(length % 4 == 0);

    // Hash it
    uint32_t hash = 0;
    loop(i, length) {
        hash ^= key[i] << (i&1 ? 8 : 0);
        hash = LEFTROTATE(hash, 3);
    }

    // Search (if there's a collision replace the old one)
    size_t       index  = hash % MAX_TYPE_ENTRY_LOOKUP_SIZE;
    CV_TypeEntry lookup = lookup_table[index];

    // printf("Lookup %zu (%x hash, has match? %s)\n", index, hash, lookup.key ?
    // "yea" : "naw");
    if (lookup.key) {
        // verify it even matches
        size_t lookup_size = tb_get2b(sect, lookup.key) + 2;

        if (length == lookup_size && memcmp(key, &sect->data[lookup.key], length) == 0) {
            // printf("Saved %zu bytes (%d)\n", length, lookup.value);
            return lookup.value;
        }
    }

    uint16_t type_index = *type_entry_count;
    *type_entry_count += 1;

    // printf("Used %zu bytes (%d)\n", length, type_index);

    lookup_table[index].key   = sect->count;
    lookup_table[index].value = type_index;

    tb_out_reserve(sect, length);
    tb_outs_UNSAFE(sect, length, (const uint8_t*)key);
    return type_index;
}

void tb_export_coff(
    TB_Module* m, const ICodeGen* restrict code_gen, const char* path, bool emit_debug_info) {
    TB_TemporaryStorage* tls              = tb_tls_allocate();
    const bool           emit_unwind_info = false;

    // The prologue and epilogue generators need some storage
    uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = malloc(m->functions.count * sizeof(uint32_t));

    // String table array, stores the strings which will be put
    // into the string table
    uint32_t string_table_length = 0;
    uint32_t string_table_mark   = 4;
    uint32_t string_table_cap    = 0;

    uint32_t data_relocation_count = 0;
    loop(t, m->max_threads) {
        loop(j, arrlen(m->globals[t])) {
            TB_Global* g = &m->globals[t][j];

            TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
            TB_Initializer*  i =
                (TB_Initializer*)&m->initializers[g->init / per_thread_stride][g->init % per_thread_stride];

            loop(k, i->obj_count) {
                data_relocation_count += (i->objects[k].type != TB_INIT_OBJ_REGION);
            }
        }
    }

    // NOTE(NeGate): The symbol ids per thread of these symbol groups are all
    // sequencial symbol id = symbol type base + thread's baseline + local id both
    // the global and external symbols share a baseline
    uint32_t* external_symbol_relative_id = tb_tls_push(tls, TB_MAX_THREADS * sizeof(uint32_t));
    uint32_t* global_symbol_relative_id   = tb_tls_push(tls, TB_MAX_THREADS * sizeof(uint32_t));

    loop(i, m->max_threads) {
        external_symbol_relative_id[i] = string_table_cap;

        size_t external_len = arrlen(m->externals[i]);
        string_table_cap += external_len ? external_len - 1 : 0;
    }

    loop(i, m->max_threads) {
        global_symbol_relative_id[i] = string_table_cap;
        string_table_cap += arrlen(m->globals[i]);
    }
    string_table_cap += m->functions.compiled_count;

    char** string_table = malloc(string_table_cap * sizeof(const char*));

    const int       number_of_sections = 3 + (emit_unwind_info ? 2 : 0) 
										   + (emit_debug_info ? 2 : 0)
										   + (m->tls_region_size ? 1 : 0);

    COFF_FileHeader header             = { .num_sections = number_of_sections,
        .timestamp                           = time(NULL),
        .symbol_count                        = 0,
        .symbol_table                        = 0,
        .characteristics                     = IMAGE_FILE_LINE_NUMS_STRIPPED };

    COFF_SectionHeader text_section = { .name = { ".text" }, // .text
        .characteristics                      = COFF_CHARACTERISTICS_TEXT };

    COFF_SectionHeader rdata_section = { .name = { ".rdata" }, // .rdata
        .characteristics                       = COFF_CHARACTERISTICS_RODATA,
        .raw_data_size                         = m->rdata_region_size };

    COFF_SectionHeader data_section = { .name = { ".data" }, // .data
        .characteristics                      = COFF_CHARACTERISTICS_DATA,
        .raw_data_size                        = m->data_region_size };

    COFF_SectionHeader pdata_section = { .name = { ".pdata" },
        .characteristics                       = COFF_CHARACTERISTICS_RODATA };

    COFF_SectionHeader xdata_section = { .name = { ".xdata" },
        .characteristics                       = COFF_CHARACTERISTICS_RODATA };

    COFF_SectionHeader debugt_section = { .name = { ".debug$T" },
        .characteristics                        = COFF_CHARACTERISTICS_CV };

    COFF_SectionHeader debugs_section = { .name = { ".debug$S" },
        .characteristics                        = COFF_CHARACTERISTICS_CV };

	COFF_SectionHeader tls_section = { .name = { ".tls$" },
		.characteristics                        = COFF_CHARACTERISTICS_DATA,
		.raw_data_size                          = m->tls_region_size };

    switch (m->target_arch) {
    case TB_ARCH_X86_64: header.machine = COFF_MACHINE_AMD64; break;
    case TB_ARCH_AARCH64: header.machine = COFF_MACHINE_ARM64; break;
    default: tb_todo();
    }

    for (size_t i = 0; i < m->functions.count; i++) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        func_layout[i]           = text_section.raw_data_size;
        if (!out_f) continue;

        uint64_t meta        = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;

        size_t code_size = out_f->code_size;
        size_t prologue  = code_gen->get_prologue_length(meta, stack_usage);
        size_t epilogue  = code_gen->get_epilogue_length(meta, stack_usage);
        assert(prologue + epilogue < PROEPI_BUFFER);

        text_section.raw_data_size += prologue;
        text_section.raw_data_size += epilogue;
        text_section.raw_data_size += code_size;
    }

    TB_Emitter debugs_out = { 0 };
    TB_Emitter debugt_out = { 0 };

    // if the codeview stuff is never done, this is never actually needed so it's
    // fine that it's NULL
    uint32_t* file_table_offset   = NULL;
    uint32_t* function_type_table = NULL;
    uint32_t* line_secrel_patch   = NULL;

    // Based on this, it's the only nice CodeView source out there:
    // https://github.com/netwide-assembler/nasm/blob/master/output/codeview.c
    if (emit_debug_info) {
        file_table_offset   = tb_tls_push(tls, m->functions.count * sizeof(uint32_t));
        function_type_table = tb_tls_push(tls, m->functions.count * sizeof(uint32_t));
        line_secrel_patch   = tb_tls_push(tls, m->functions.count * sizeof(uint32_t));

        // Write type table
        {
            tb_out4b(&debugt_out, 0x00000004);

            CV_TypeEntry* lookup_table =
                tb_tls_push(tls, MAX_TYPE_ENTRY_LOOKUP_SIZE * sizeof(CV_TypeEntry));
            memset(lookup_table, 0, MAX_TYPE_ENTRY_LOOKUP_SIZE * sizeof(CV_TypeEntry));

            uint32_t type_entry_count = 0x1000;
            loop(i, m->functions.count) {
                const TB_FunctionPrototype* proto = m->functions.data[i].prototype;

                uint16_t arg_list;
                {
                    size_t param_count = proto->param_count + proto->has_varargs;

                    size_t    length = 2 + 2 + 4 + (4 * param_count);
                    uint16_t* data   = tb_tls_push(tls, length);

                    data[0]                = length - 2;
                    data[1]                = 0x1201; // ARGLIST type
                    *((uint32_t*)&data[2]) = param_count;

                    uint32_t* param_data = (uint32_t*)&data[4];
                    loop(j, proto->param_count) {
                        uint16_t param_type_entry = get_codeview_type(proto->params[j]);
                        param_data[j]             = param_type_entry;
                    }

                    // varargs add a dummy type at the end
                    if (proto->has_varargs) { param_data[proto->param_count] = 0; }

                    arg_list = find_or_make_cv_type(
                        &debugt_out, &type_entry_count, lookup_table, length, data);
                    tb_tls_restore(tls, data);
                }

                uint16_t proc_type;
                {
                    CV_LFProc* data = tb_tls_push(tls, sizeof(CV_LFProc));

                    uint16_t return_type = get_codeview_type(proto->return_dt);

                    *data = (CV_LFProc) { .len = sizeof(CV_LFProc) - 2,
                        .leaf                  = 0x1008, // LF_PROCEDURE
                        .rvtype                = return_type,
                        .parmcount             = proto->param_count,
                        .arglist               = arg_list };

                    proc_type = find_or_make_cv_type(&debugt_out, &type_entry_count, lookup_table,
                        sizeof(CV_LFProc), (uint16_t*)data);
                    tb_tls_restore(tls, data);
                }

                {
                    size_t name_len   = strlen(m->functions.data[i].name);
                    size_t entry_size = sizeof(CV_LFFuncID) + name_len;
                    entry_size        = align_up(entry_size + 1, 4);

                    CV_LFFuncID* data = tb_tls_push(tls, entry_size);

                    *data = (CV_LFFuncID) { .len = entry_size - 2,
                        .leaf                    = 0x1601, // LF_FUNC_ID
                        .type                    = proc_type };

                    memcpy(data->name, m->functions.data[i].name, name_len);
                    data->name[name_len] = 0;

                    function_type_table[i] = find_or_make_cv_type(
                        &debugt_out, &type_entry_count, lookup_table, entry_size, (uint16_t*)data);
                    tb_tls_restore(tls, data);
                }
            }

            tb_tls_restore(tls, lookup_table);

            size_t pad = 4 - (debugt_out.count % 4);
            if (pad == 4) pad = 0;
            while (pad--)
                tb_out1b(&debugt_out, 0x00);
        }

        // Write symbol info table
        {
            static const char creator_str[] = "Cuik";

            uint32_t creator_length = 2 + 4 + 2 + (3 * 2) + (3 * 2) + sizeof(creator_str) + 2;

            /*sym_length = (cv8_state.num_syms[SYMTYPE_CODE] * 7)
                    + (cv8_state.num_syms[SYMTYPE_PROC]  * 7)
                    + (cv8_state.num_syms[SYMTYPE_LDATA] * 10)
                    + (cv8_state.num_syms[SYMTYPE_GDATA] * 10)
                    + (cv8_state.symbol_lengths);*/

            size_t path_len = strlen(path) + 1;

            tb_out4b(&debugs_out, 0x00000004);

            // File nametable
            if (true) {
                tb_out4b(&debugs_out, 0x000000F3);

                size_t field_length_patch = debugs_out.count;
                tb_out4b(&debugs_out, 0);
                tb_out1b(&debugs_out, 0);

                // skip the NULL file entry
                size_t pos = 1;
                loop_range(i, 1, m->files.count) {
                    size_t len = strlen(m->files.data[i].path);

                    tb_out_reserve(&debugs_out, len + 1);
                    tb_outs_UNSAFE(&debugs_out, len + 1, (const uint8_t*)m->files.data[i].path);

                    file_table_offset[i] = pos;
                    pos += len + 1;
                }

                tb_patch4b(
                    &debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);
            }

            // Source file table
            // we practically transmute the file_table_offset from meaning file string
            // table entries into source file entries.
            if (true) {
                tb_out4b(&debugs_out, 0x000000F4);

                size_t field_length_patch = debugs_out.count;
                tb_out4b(&debugs_out, 0);

                size_t pos = 0;
                loop_range(i, 1, m->files.count) {
                    uint8_t sum[16];
                    md5sum_file(sum, m->files.data[i].path);

                    tb_out4b(&debugs_out, file_table_offset[i]);
                    tb_out2b(&debugs_out, 0x0110);
                    tb_out_reserve(&debugs_out, MD5_HASHBYTES);
                    tb_outs_UNSAFE(&debugs_out, MD5_HASHBYTES, sum);
                    tb_out2b(&debugs_out, 0);

                    file_table_offset[i] = pos;
                    pos += 4 + 2 + MD5_HASHBYTES + 2;
                }

                tb_patch4b(&debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);
            }

            // Line info table
            if (true) {
                loop(func_id, m->functions.count) {
                    TB_FunctionOutput* out_f = m->functions.data[func_id].output;
                    if (!out_f) continue;

                    uint64_t meta        = out_f->prologue_epilogue_metadata;
                    uint64_t stack_usage = out_f->stack_usage;

                    // Layout crap
                    uint32_t function_start = func_layout[func_id];
                    uint32_t function_end   = (func_id + 1) < m->functions.count
                                                  ? func_layout[func_id + 1]
                                                  : text_section.raw_data_size;

                    uint32_t body_start = code_gen->get_prologue_length(meta, stack_usage);

                    size_t line_count       = m->functions.data[func_id].line_count;
                    TB_Line* restrict lines = m->functions.data[func_id].lines;

                    // printf("FUNC %s\n", m->functions.data[func_id].name);

                    tb_out4b(&debugs_out, 0x000000F2);
                    size_t field_length_patch = debugs_out.count;
                    tb_out4b(&debugs_out, 0);

                    // Source mapping header
                    line_secrel_patch[func_id] = debugs_out.count;
                    tb_out4b(&debugs_out, 0); // SECREL  | .text
                    tb_out4b(&debugs_out, 0); // SECTION | .text
                    tb_out4b(&debugs_out, function_end - function_start);

                    // when we make new file line regions
                    // we backpatch the line count for the
                    // region we just finished
                    uint32_t  backpatch          = 0;
                    int       last_line          = 0;
                    TB_FileID last_file          = 0;
                    uint32_t  current_line_count = 0;

                    loop(line_id, line_count) {
                        TB_Line line = lines[line_id];

                        if (last_file != line.file) {
                            if (backpatch) {
                                tb_patch4b(&debugs_out, backpatch, current_line_count);
                                tb_patch4b(
                                    &debugs_out, backpatch + 4, 12 + (current_line_count * 8));
                            }
                            last_file = line.file;

                            // File entry
                            tb_out4b(&debugs_out, file_table_offset[line.file]);
                            backpatch = debugs_out.count;
                            tb_out4b(&debugs_out, 0);
                            tb_out4b(&debugs_out, 0);

                            // printf("  FILE %d\n", line.file);
                            current_line_count = 0;
                            last_line          = 0;
                        }

                        if (last_line != line.line) {
                            last_line = line.line;
                            // printf("  * LINE %d : %x\n", line.line, body_start + line.pos);

                            tb_out4b(&debugs_out, line.pos ? body_start + line.pos : line.pos);
                            tb_out4b(&debugs_out, line.line);
                            current_line_count++;
                        }
                    }

                    // finalize the patch work
                    if (backpatch) {
                        tb_patch4b(&debugs_out, backpatch, current_line_count);
                        tb_patch4b(&debugs_out, backpatch + 4, 12 + (current_line_count * 8));
                    }

                    tb_patch4b(&debugs_out, field_length_patch,
                        (debugs_out.count - field_length_patch) - 4);
                    // printf("\n");
                }
            }

            // Symbol table
            tb_out4b(&debugs_out, 0x000000F1);

            size_t field_length_patch = debugs_out.count;
            tb_out4b(&debugs_out, 0);

            // Symbol info object
            {
                uint32_t obj_length = 2 + 4 + path_len;
                tb_out2b(&debugs_out, obj_length);
                tb_out2b(&debugs_out, 0x1101);
                tb_out4b(&debugs_out, 0);

                tb_out_reserve(&debugs_out, sizeof(creator_str));
                tb_outs_UNSAFE(&debugs_out, path_len, (const uint8_t*)path);
            }

            // Symbol info properties
            {
                tb_out2b(&debugs_out, creator_length);
                tb_out2b(&debugs_out, 0x1116);
                tb_out4b(&debugs_out, 0);

                tb_out2b(&debugs_out, 0x00D0); /* machine */
                tb_out2b(&debugs_out, 0);      /* verFEMajor */
                tb_out2b(&debugs_out, 0);      /* verFEMinor */
                tb_out2b(&debugs_out, 0);      /* verFEBuild */

                tb_out2b(&debugs_out, TB_VERSION_MAJOR); /* verMajor */
                tb_out2b(&debugs_out, TB_VERSION_MINOR); /* verMinor */
                tb_out2b(&debugs_out, TB_VERSION_PATCH); /* verBuild */

                tb_out_reserve(&debugs_out, sizeof(creator_str));
                tb_outs_UNSAFE(&debugs_out, sizeof(creator_str), (const uint8_t*)creator_str);

                tb_out2b(&debugs_out, 0);
            }

            // Symbols
            loop(i, m->functions.count) {
                TB_Function*       f     = &m->functions.data[i];
                TB_FunctionOutput* out_f = f->output;
                if (!out_f) continue;

                const char* name     = f->name;
                size_t      name_len = strlen(f->name) + 1;

                size_t baseline = debugs_out.count;
                tb_out2b(&debugs_out, 0);
                tb_out2b(&debugs_out, S_GPROC32_ID);

                tb_out4b(&debugs_out, 0); // pointer to the parent
                tb_out4b(&debugs_out, 0); // pointer to this blocks end (left as zero?)
                tb_out4b(&debugs_out, 0); // pointer to the next symbol (left as zero?)

                // TODO(NeGate): correctly fill this
                tb_out4b(&debugs_out, 1);                      // procedure length
                tb_out4b(&debugs_out, 0);                      // debug start offset (?)
                tb_out4b(&debugs_out, 0);                      // debug end offset (?)
                tb_out4b(&debugs_out, function_type_table[i]); // type index

                // we save this location because there's two relocations
                // we'll put there:
                //   type      target     size
                //   SECREL    .text     4 bytes
                //   SECTION   .text     2 bytes
                function_type_table[i] = debugs_out.count;
                tb_out4b(&debugs_out, 0); // offset
                tb_out2b(&debugs_out, 0); // segment

                // the 1 means we have a frame pointer present
                tb_out1b(&debugs_out, 1); // flags

                tb_out_reserve(&debugs_out, name_len);
                tb_outs_UNSAFE(&debugs_out, name_len, (const uint8_t*)name);

                // patch field length
                tb_patch2b(&debugs_out, baseline, (debugs_out.count - baseline) - 2);

                {
                    // frameproc
                    size_t frameproc_baseline = debugs_out.count;

                    tb_out2b(&debugs_out, 0);
                    tb_out2b(&debugs_out, S_FRAMEPROC);

                    size_t stack_usage = out_f->stack_usage == 8 ? 0 : out_f->stack_usage;

                    tb_out4b(&debugs_out,
                        stack_usage);         // count of bytes of total frame of procedure
                    tb_out4b(&debugs_out, 0); // count of bytes of padding in the frame
                    tb_out4b(&debugs_out,
                        0); // offset (relative to frame poniter) to where padding starts
                    tb_out4b(&debugs_out, 0); // count of bytes of callee save registers
                    tb_out4b(&debugs_out, 0); // offset of exception handler
                    tb_out4b(&debugs_out, 0); // section id of exception handler
                    tb_out4b(&debugs_out, 0); // flags

                    tb_patch2b(&debugs_out, frameproc_baseline,
                        (debugs_out.count - frameproc_baseline) - 2);
                }

                // end the block
                tb_out2b(&debugs_out, 2);
                tb_out2b(&debugs_out, S_PROC_ID_END);
            }
            tb_patch4b(
                &debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);

            size_t pad = 4 - (debugs_out.count % 4);
            if (pad == 4) pad = 0;
            while (pad--)
                tb_out1b(&debugs_out, 0x00);
        }

        debugs_section.raw_data_size = debugs_out.count;
        debugt_section.raw_data_size = debugt_out.count;
    }

    bool      is_xdata_function_info_heap = false;
    uint32_t* xdata_function_info         = NULL;

    TB_Emitter xdata_out = { 0 };
    if (emit_unwind_info) {
        xdata_function_info = tb_tls_try_push(tls, m->functions.count * sizeof(uint32_t));
        if (xdata_function_info == NULL) {
            xdata_function_info         = malloc(m->functions.count * sizeof(uint32_t));
            is_xdata_function_info_heap = true;
        }

        loop(i, m->functions.count) {
            TB_FunctionOutput* out_f = m->functions.data[i].output;
            if (!out_f->code) continue;

            uint64_t saved       = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            assert(xdata_out.count == (uint32_t)xdata_out.count);
            xdata_function_info[i] = xdata_out.count;

            // version (bottom 3bits), flags (top 5bits)
            tb_out1b(&xdata_out, 1);

            // size of prologue
            size_t prolog_len = code_gen->get_prologue_length(saved, stack_usage);
            assert(prolog_len == (uint8_t)prolog_len);
            tb_out1b(&xdata_out, prolog_len);

            // unwind code count
            size_t unwind_code_count = stack_usage > 8 ? tb_popcount(saved & 0xFFFF) : 0;
            assert(unwind_code_count == (uint8_t)unwind_code_count);
            tb_out1b(&xdata_out, unwind_code_count);

            // frame register (bottom 4bit), frame register offset (top 4bits)
            tb_out1b(&xdata_out, 5);

            if (stack_usage > 8) {
                // push rbp ; mov rbp, rsp
                size_t offset = 1 + 3;

                if ((tb_popcount(saved) & 1) == 0) offset++;

                loop(j, 16) if (saved & (1ull << j)) {
                    assert(offset == (uint8_t)offset);
                    tb_out1b(&xdata_out, offset);
                    tb_out1b(&xdata_out, (j << 4) | UWOP_SAVE_NONVOL);

                    offset += (i < 8) ? 1 : 2;
                }
            }
        }

        xdata_section.raw_data_size = xdata_out.count;
        pdata_section.raw_data_size = m->functions.compiled_count * 12;
    }

    // Target specific: resolve internal call patches
    code_gen->emit_call_patches(m, func_layout);

    text_section.raw_data_pos = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));
    rdata_section.raw_data_pos = text_section.raw_data_pos + text_section.raw_data_size;
    data_section.raw_data_pos  = rdata_section.raw_data_pos + rdata_section.raw_data_size;
	tls_section.raw_data_pos = data_section.raw_data_pos + data_section.raw_data_size;
    pdata_section.raw_data_pos = tls_section.raw_data_pos + tls_section.raw_data_size;
    xdata_section.raw_data_pos = pdata_section.raw_data_pos + pdata_section.raw_data_size;

    if (emit_debug_info) {
        debugt_section.raw_data_pos = xdata_section.raw_data_pos + xdata_section.raw_data_size;
        debugs_section.raw_data_pos = debugt_section.raw_data_pos + debugt_section.raw_data_size;
    }

    text_section.num_reloc = 0;
    loop(i, m->max_threads) {
        text_section.num_reloc += arrlen(m->const_patches[i]);
        text_section.num_reloc += arrlen(m->ecall_patches[i]);
        text_section.num_reloc += arrlen(m->global_patches[i]);
    }

    data_section.num_reloc = data_relocation_count;

    if (emit_unwind_info) { pdata_section.num_reloc = 3 * m->functions.compiled_count; }

    // A bunch of relocations are made by the CodeView sections, if there's no
    // debug info then these are ignored/non-existent.
    if (emit_debug_info) {
        debugt_section.num_reloc = 0;
        debugs_section.num_reloc = (4 * m->functions.compiled_count);

        text_section.pointer_to_reloc = debugs_section.raw_data_pos + debugs_section.raw_data_size;
    } else {
        text_section.pointer_to_reloc = xdata_section.raw_data_pos + xdata_section.raw_data_size;
    }

    data_section.pointer_to_reloc =
        text_section.pointer_to_reloc + (text_section.num_reloc * sizeof(COFF_ImageReloc));

    pdata_section.pointer_to_reloc =
        data_section.pointer_to_reloc + (data_section.num_reloc * sizeof(COFF_ImageReloc));

    debugs_section.pointer_to_reloc =
        pdata_section.pointer_to_reloc + (pdata_section.num_reloc * sizeof(COFF_ImageReloc));

    header.symbol_count = (number_of_sections * 2) + m->functions.compiled_count;

    loop(i, m->max_threads) {
        size_t external_len = arrlen(m->externals[i]);
        header.symbol_count += external_len ? external_len - 1 : 0;
    }

    loop(i, m->max_threads) { header.symbol_count += arrlen(m->globals[i]); }

#if !TB_STRIP_LABELS
    header.symbol_count += arrlen(m->label_symbols);
#endif

    header.symbol_table =
        debugs_section.pointer_to_reloc + (debugs_section.num_reloc * sizeof(COFF_ImageReloc));

    size_t string_table_pos = header.symbol_table + (header.symbol_count * sizeof(COFF_Symbol));

    // it's only here for an assertion, so i'm making
    // sure it doesn't get mark as unused in release.
    ((void)string_table_pos);

    FILE* f = fopen(path, "wb");
    fwrite(&header, sizeof(header), 1, f);
    fwrite(&text_section, sizeof(text_section), 1, f);
    fwrite(&rdata_section, sizeof(rdata_section), 1, f);
    fwrite(&data_section, sizeof(data_section), 1, f);

	if (tls_section.raw_data_size) {
        fwrite(&tls_section, sizeof(tls_section), 1, f);
	}

    if (emit_unwind_info) {
        fwrite(&pdata_section, sizeof(pdata_section), 1, f);
        fwrite(&xdata_section, sizeof(xdata_section), 1, f);
    }

    if (emit_debug_info) {
        fwrite(&debugt_section, sizeof(debugt_section), 1, f);
        fwrite(&debugs_section, sizeof(debugs_section), 1, f);
    }

    assert(ftell(f) == text_section.raw_data_pos);
    for (size_t i = 0; i < m->functions.count; i++) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        if (!out_f) continue;

        uint64_t       meta        = out_f->prologue_epilogue_metadata;
        uint64_t       stack_usage = out_f->stack_usage;
        const uint8_t* code        = out_f->code;
        size_t         code_size   = out_f->code_size;

        uint8_t* prologue     = proepi_buffer;
        size_t   prologue_len = code_gen->emit_prologue(prologue, meta, stack_usage);

        uint8_t* epilogue     = proepi_buffer + prologue_len;
        size_t   epilogue_len = code_gen->emit_epilogue(epilogue, meta, stack_usage);

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
				if (g->storage != TB_STORAGE_DATA) continue;

                TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
                TB_Initializer*  init =
                    (TB_Initializer*)&m
                        ->initializers[g->init / per_thread_stride][g->init % per_thread_stride];

                // clear out space
                memset(&data[g->pos], 0, init->size);

                loop(k, init->obj_count) if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                    memcpy(&data[g->pos + init->objects[k].offset], init->objects[k].region.ptr,
                        init->objects[k].region.size);
                }
            }
        }

        fwrite(data, m->data_region_size, 1, f);
        tb_platform_heap_free(data);
    }
	
	assert(ftell(f) == tls_section.raw_data_pos);
    {
        // TODO(NeGate): Optimize this for size and speed, sometimes
        // there's huge sections will be filled with just empty regions
        // so it's probably best not to represent everything in a big
        // buffer.
        char* data = tb_platform_heap_alloc(m->tls_region_size);

        loop(i, m->max_threads) {
            loop(j, arrlen(m->globals[i])) {
                TB_Global* g = &m->globals[i][j];
				if (g->storage != TB_STORAGE_TLS) continue;

                TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
                TB_Initializer*  init =
                    (TB_Initializer*)&m->initializers[g->init / per_thread_stride][g->init % per_thread_stride];

                // clear out space
                memset(&data[g->pos], 0, init->size);

                loop(k, init->obj_count) if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                    memcpy(&data[g->pos + init->objects[k].offset], init->objects[k].region.ptr,
                        init->objects[k].region.size);
                }
            }
        }

        fwrite(data, m->tls_region_size, 1, f);
        tb_platform_heap_free(data);
    }

    if (emit_unwind_info) {
        assert(ftell(f) == pdata_section.raw_data_pos);
        loop(i, m->functions.count) {
            TB_FunctionOutput* out_f = m->functions.data[i].output;
            if (!out_f) continue;

            uint32_t payload[3];
            payload[0] = 0;
            payload[1] = out_f->code_size;
            payload[2] = xdata_function_info[i];

            fwrite(payload, sizeof(uint32_t), 3, f);
        }

        assert(ftell(f) == xdata_section.raw_data_pos);
        fwrite(xdata_out.data, xdata_out.count, 1, f);
    }

    // Emit debug info
    if (emit_debug_info) {
        assert(ftell(f) == debugt_section.raw_data_pos);
        fwrite(debugt_out.data, debugt_out.count, 1, f);

        assert(ftell(f) == debugs_section.raw_data_pos);
        fwrite(debugs_out.data, debugs_out.count, 1, f);
    }

    assert(ftell(f) == text_section.pointer_to_reloc);
    loop(i, m->max_threads) {
        loop(j, arrlen(m->const_patches[i])) {
            TB_ConstPoolPatch* p     = &m->const_patches[i][j];
            TB_FunctionOutput* out_f = p->source->output;

            uint64_t meta        = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] +
                                code_gen->get_prologue_length(meta, stack_usage) + p->pos;

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_REL32,
                       .SymbolTableIndex      = 2, // rdata section
                       .VirtualAddress        = actual_pos },
                sizeof(COFF_ImageReloc), 1, f);
        }
    }

    size_t function_sym_start    = (number_of_sections * 2);
    size_t extern_func_sym_start = function_sym_start + m->functions.compiled_count;
    loop(i, m->max_threads) {
        loop(j, arrlen(m->ecall_patches[i])) {
            TB_ExternFunctionPatch* p     = &m->ecall_patches[i][j];
            TB_FunctionOutput*      out_f = p->source->output;

            uint64_t meta        = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] +
                                code_gen->get_prologue_length(meta, stack_usage) + p->pos;

            TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;

            int symbol_id = external_symbol_relative_id[p->target_id / per_thread_stride];

            // Workaround since each external set has a null slot
            int local_id = (p->target_id % per_thread_stride);
            if (local_id) local_id--;
            symbol_id += local_id;

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_REL32,
                       .SymbolTableIndex      = extern_func_sym_start + symbol_id,
                       .VirtualAddress        = actual_pos },
                sizeof(COFF_ImageReloc), 1, f);
        }
    }

    loop(i, m->max_threads) {
        loop(j, arrlen(m->global_patches[i])) {
			TB_GlobalPatch*    p     = &m->global_patches[i][j];
			TB_FunctionOutput* out_f = p->source->output;

			uint64_t meta        = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;

			size_t actual_pos = func_layout[p->source - m->functions.data] +
                                code_gen->get_prologue_length(meta, stack_usage) + p->pos;

			TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
			TB_Global*  global = &m->globals[p->global / per_thread_stride][p->global % per_thread_stride];

			int         symbol_id = global_symbol_relative_id[p->global / per_thread_stride] +
                            (p->global % per_thread_stride);

			fwrite(&(COFF_ImageReloc) { 
					.Type = global->storage == TB_STORAGE_TLS ?
						IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                    .SymbolTableIndex      = extern_func_sym_start + symbol_id,
                    .VirtualAddress        = actual_pos },
                sizeof(COFF_ImageReloc), 1, f);
        }
    }

    assert(ftell(f) == data_section.pointer_to_reloc);
    loop(i, m->max_threads) {
        loop(j, arrlen(m->globals[i])) {
            TB_Global* g = &m->globals[i][j];

            TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
            TB_Initializer*  init =
                (TB_Initializer*)&m
                    ->initializers[g->init / per_thread_stride][g->init % per_thread_stride];

            loop(k, init->obj_count) {
                size_t actual_pos = g->pos + init->objects[k].offset;

                switch (init->objects[k].type) {
                case TB_INIT_OBJ_RELOC_GLOBAL: {
                    TB_GlobalID global_id = init->objects[k].reloc_global;

                    TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
                    int symbol_id = global_symbol_relative_id[global_id / per_thread_stride] +
                                    (global_id % per_thread_stride);

                    fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_ADDR64,
                               .SymbolTableIndex      = extern_func_sym_start + symbol_id,
                               .VirtualAddress        = actual_pos },
                        sizeof(COFF_ImageReloc), 1, f);
                    break;
                }

                case TB_INIT_OBJ_RELOC_EXTERN: {
                    TB_ExternalID extern_id = init->objects[k].reloc_extern;

                    TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
                    int symbol_id = external_symbol_relative_id[extern_id / per_thread_stride];

                    // Workaround since each external set has a null slot
                    int local_id = (extern_id % per_thread_stride);
                    if (local_id) local_id--;
                    symbol_id += local_id;

                    fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_ADDR64,
                               .SymbolTableIndex      = extern_func_sym_start + symbol_id,
                               .VirtualAddress        = actual_pos },
                        sizeof(COFF_ImageReloc), 1, f);
                    break;
                }

                case TB_INIT_OBJ_RELOC_FUNCTION: {
                    int symbol_id = init->objects[k].reloc_function;

                    fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_ADDR64,
                               .SymbolTableIndex      = function_sym_start + symbol_id,
                               .VirtualAddress        = actual_pos },
                        sizeof(COFF_ImageReloc), 1, f);
                    break;
                }

                default: break;
                }
            }
        }
    }

    if (emit_unwind_info) {
        assert(ftell(f) == pdata_section.pointer_to_reloc);
        loop(i, m->functions.count) {
            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_ADDR32NB,
                       .SymbolTableIndex      = function_sym_start + i,
                       .VirtualAddress        = (i * 12) },
                sizeof(COFF_ImageReloc), 1, f);

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_ADDR32NB,
                       .SymbolTableIndex      = function_sym_start + i,
                       .VirtualAddress        = (i * 12) + 4 },
                sizeof(COFF_ImageReloc), 1, f);

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_ADDR32NB,
                       .SymbolTableIndex      = 8, // xdata section
                       .VirtualAddress        = (i * 12) + 8 },
                sizeof(COFF_ImageReloc), 1, f);
        }
    }

    if (emit_debug_info) {
        assert(ftell(f) == debugs_section.pointer_to_reloc);

        loop(i, m->functions.count) {
            if (!m->functions.data[i].output) continue;

            uint32_t off = line_secrel_patch[i];
            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_SECREL,
                       .SymbolTableIndex      = function_sym_start + i,
                       .VirtualAddress        = off },
                sizeof(COFF_ImageReloc), 1, f);

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_SECTION,
                       .SymbolTableIndex      = function_sym_start + i,
                       .VirtualAddress        = off + 4 },
                sizeof(COFF_ImageReloc), 1, f);
        }

        loop(i, m->functions.count) {
            if (!m->functions.data[i].output) continue;

            uint32_t off = function_type_table[i];

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_SECREL,
                       .SymbolTableIndex      = function_sym_start + i, // text section
                       .VirtualAddress        = off },
                sizeof(COFF_ImageReloc), 1, f);

            fwrite(&(COFF_ImageReloc) { .Type = IMAGE_REL_AMD64_SECTION,
                       .SymbolTableIndex      = function_sym_start + i, // text section
                       .VirtualAddress        = off + 4 },
                sizeof(COFF_ImageReloc), 1, f);
        }
    }

    assert(ftell(f) == header.symbol_table);
	int section_num = 1;

    fwrite(&(COFF_Symbol) { .short_name = { ".text" },
               .section_number          = section_num,
               .storage_class           = IMAGE_SYM_CLASS_STATIC,
               .aux_symbols_count       = 1 },
        sizeof(COFF_Symbol), 1, f);

    fwrite(&(COFF_AuxSectionSymbol) { .length = text_section.raw_data_size,
               .reloc_count                   = text_section.num_reloc,
               .number                        = section_num },
        sizeof(COFF_AuxSectionSymbol), 1, f);
	section_num += 1;

    assert(section_num == 2); // things expect it to be 2
	fwrite(&(COFF_Symbol) { .short_name = { ".rdata" },
               .section_number          = section_num,
               .storage_class           = IMAGE_SYM_CLASS_STATIC,
               .aux_symbols_count       = 1 },
        sizeof(COFF_Symbol), 1, f);

    fwrite(&(COFF_AuxSectionSymbol) { 
			.length = data_section.raw_data_size,
			.number = section_num },
        sizeof(COFF_AuxSectionSymbol), 1, f);
	section_num += 1;

	assert(section_num == 3); // things expect it to be 3
    fwrite(&(COFF_Symbol) { .short_name = { ".data" },
               .section_number          = section_num,
               .storage_class           = IMAGE_SYM_CLASS_STATIC,
               .aux_symbols_count       = 1 },
        sizeof(COFF_Symbol), 1, f);

    fwrite(&(COFF_AuxSectionSymbol) { .length = data_section.raw_data_size,
               .reloc_count                   = data_section.num_reloc,
               .number                        = section_num },
        sizeof(COFF_AuxSectionSymbol), 1, f);
	section_num += 1;

	int tls_section_num = section_num;
	if (m->tls_region_size) {
		fwrite(&(COFF_Symbol) { .short_name = { ".tls$" },
				   .section_number          = section_num,
				   .storage_class           = IMAGE_SYM_CLASS_STATIC,
				   .aux_symbols_count       = 1 },
			sizeof(COFF_Symbol), 1, f);

		fwrite(&(COFF_AuxSectionSymbol) { .length = data_section.raw_data_size,
				   .reloc_count                   = data_section.num_reloc,
				   .number                        = section_num },
			sizeof(COFF_AuxSectionSymbol), 1, f);
		section_num += 1;
	}

    if (emit_unwind_info) {
        fwrite(&(COFF_Symbol) { .short_name = { ".pdata" },
                   .section_number          = section_num,
                   .storage_class           = IMAGE_SYM_CLASS_STATIC,
                   .aux_symbols_count       = 1 },
            sizeof(COFF_Symbol), 1, f);

        fwrite(&(COFF_AuxSectionSymbol) { .length = pdata_section.raw_data_size,
                   .reloc_count                   = pdata_section.num_reloc,
                   .number                        = section_num },
            sizeof(COFF_AuxSectionSymbol), 1, f);
		section_num += 1;

        fwrite(&(COFF_Symbol) { .short_name = { ".xdata" },
                   .section_number          = section_num,
                   .storage_class           = IMAGE_SYM_CLASS_STATIC,
                   .aux_symbols_count       = 1 },
            sizeof(COFF_Symbol), 1, f);

        fwrite(&(COFF_AuxSectionSymbol) { .length = xdata_section.raw_data_size,
                   .reloc_count                   = xdata_section.num_reloc,
                   .number                        = section_num },
            sizeof(COFF_AuxSectionSymbol), 1, f);
		section_num += 1;
    }

    if (emit_debug_info) {
        fwrite(&(COFF_Symbol) { .short_name = { ".debug$T" },
                   .section_number          = section_num,
                   .storage_class           = IMAGE_SYM_CLASS_STATIC,
                   .aux_symbols_count       = 1 },
            sizeof(COFF_Symbol), 1, f);

        fwrite(&(COFF_AuxSectionSymbol) {
				.length = debugt_section.raw_data_size,
				.number = section_num }, 
			sizeof(COFF_AuxSectionSymbol), 1, f);
		section_num += 1;
		
        fwrite(&(COFF_Symbol) { .short_name = { ".debug$S" },
                   .section_number          = section_num,
                   .storage_class           = IMAGE_SYM_CLASS_STATIC,
                   .aux_symbols_count       = 1 },
            sizeof(COFF_Symbol), 1, f);

        fwrite(&(COFF_AuxSectionSymbol) { .length = debugs_section.raw_data_size,
                   .reloc_count                   = debugs_section.num_reloc,
                   .number                        = section_num },
            sizeof(COFF_AuxSectionSymbol), 1, f);
		section_num += 1;
    }

    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        if (!out_f) continue;

        bool is_extern = out_f->linkage == TB_LINKAGE_PUBLIC;
        COFF_Symbol sym = {
			.value = func_layout[i],
            .section_number = 1,
            .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
		};

        const char* name     = m->functions.data[i].name;
        size_t      name_len = strlen(name);
        assert(name_len < UINT16_MAX);

        if (name_len >= 8) {
            sym.long_name[0] = 0; // this value is 0 for long names
            sym.long_name[1] = string_table_mark;

            string_table[string_table_length++] = (char*)name;
            string_table_mark += name_len + 1;
        } else {
            memcpy(sym.short_name, name, name_len + 1);
        }

        fwrite(&sym, sizeof(sym), 1, f);
    }

    loop(i, m->max_threads) {
        loop_range(j, 1, arrlen(m->externals[i])) {
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
            } else {
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
                .section_number = g->storage == TB_STORAGE_TLS ? tls_section_num : 3, // data or tls section
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
        TB_LabelSymbol*    l     = &m->label_symbols[i];
        TB_FunctionOutput* out_f = &m->compiled_functions.data[l->func_id];

        uint64_t meta        = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;

        size_t actual_pos =
            func_layout[l->func_id] + code_gen->get_prologue_length(meta, stack_usage) + l->pos;

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

    if (is_xdata_function_info_heap) { free(xdata_function_info); }

    free(xdata_out.data);
    free(debugs_out.data);
    free(debugt_out.data);
    free(string_table);
    free(func_layout);
}
