#include "../tb_internal.h"
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat  _fstat
#define stat   _stat
#define strdup _strdup
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
// IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ |
// IMAGE_SCN_ALIGN_16BYTES
#define COFF_CHARACTERISTICS_BSS 0xC0500080u
// IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_ALIGN_8BYTES | IMAGE_SCN_MEM_READ |
// IMAGE_SCN_MEM_DISCARDABLE
#define COFF_CHARACTERISTICS_CV 0x42100040u

#define IMAGE_SYM_CLASS_EXTERNAL 0x0002
#define IMAGE_SYM_CLASS_STATIC   0x0003
#define IMAGE_SYM_CLASS_LABEL    0x0006
#define IMAGE_SYM_CLASS_FILE     0x0067

#define IMAGE_FILE_LINE_NUMS_STRIPPED 0x0004

#define IMAGE_REL_AMD64_ADDR64   0x0001
#define IMAGE_REL_AMD64_ADDR32   0x0002
#define IMAGE_REL_AMD64_ADDR32NB 0x0003
#define IMAGE_REL_AMD64_REL32    0x0004
#define IMAGE_REL_AMD64_REL32_1  0x0005
#define IMAGE_REL_AMD64_REL32_2  0x0006
#define IMAGE_REL_AMD64_REL32_3  0x0007
#define IMAGE_REL_AMD64_REL32_4  0x0008
#define IMAGE_REL_AMD64_REL32_5  0x0009
#define IMAGE_REL_AMD64_SECTION  0x000A
#define IMAGE_REL_AMD64_SECREL   0x000B

#define IMAGE_SCN_MEM_DISCARDABLE 0x02000000
#define IMAGE_SCN_MEM_EXECUTE     0x20000000
#define IMAGE_SCN_MEM_READ        0x40000000
#define IMAGE_SCN_MEM_WRITE       0x80000000

#define IMAGE_SCN_CNT_CODE                   0x00000020  /* Section contains code. */
#define IMAGE_SCN_CNT_INITIALIZED_DATA       0x00000040  /* Section contains initialized data. */
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA     0x00000080  /* Section contains uninitialized data. */

#define IMAGE_DIRECTORY_ENTRY_IMPORT 1
#define IMAGE_DIRECTORY_ENTRY_RELOC  5

#define MD5_HASHBYTES 16

typedef struct {
    char name[16];
    char date[12];

    // Microsoft tools don't actually do anything with this
    char user_id[6];
    char group_id[6];

    char mode[8];
    char size[10];

    uint8_t newline[2];
    uint8_t contents[];
} COFF_ArchiveMemberHeader;

typedef struct {
    uint16_t sig1;
    uint16_t sig2;
    uint16_t version;
    uint16_t machine;
    uint32_t timestamp;
    uint32_t size_of_data;
    uint16_t ordinal_hint;

    uint16_t type      : 2;
    uint16_t name_type : 3;
    uint16_t reserved  : 11;
} COFF_ImportHeader;

typedef struct {
    uint32_t import_lookup_table; // RVA
    uint32_t timestamp;
    uint32_t forwarder_chain;
    uint32_t name;
    uint32_t import_address_table; // RVA; Thunk table
} COFF_ImportDirectory;

typedef struct COFF_SectionHeader {
    char name[8];
    union {
        uint32_t physical_address;
        uint32_t virtual_size;
    } misc;
    uint32_t virtual_address;
    uint32_t raw_data_size;
    uint32_t raw_data_pos;
    uint32_t pointer_to_reloc;
    uint32_t pointer_to_lineno;
    uint16_t num_reloc;
    uint16_t num_lineno;
    uint32_t characteristics;
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
#pragma pack(push, 2)
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
        uint8_t  short_name[8];
        uint32_t long_name[2];
    };
    uint32_t value;
    int16_t  section_number;
    uint16_t type;
    uint8_t  storage_class;
    uint8_t  aux_symbols_count;
} COFF_Symbol;
static_assert(sizeof(COFF_Symbol) == 18, "COFF Symbol size != 18 bytes");

typedef struct COFF_AuxSectionSymbol {
    uint32_t length;       // section length
    uint16_t reloc_count;  // number of relocation entries
    uint16_t lineno_count; // number of line numbers
    uint32_t checksum;     // checksum for communal
    int16_t  number;       // section number to associate with
    uint8_t  selection;    // communal selection type
    uint8_t  reserved;
    int16_t  high_bits;    // high bits of the section number
} COFF_AuxSectionSymbol;
static_assert(sizeof(COFF_AuxSectionSymbol) == 18, "COFF Aux Section Symbol size != 18 bytes");

typedef struct {
    union {
        unsigned long l_symndx; /* function name symbol index */
        unsigned long l_paddr;  /* address of line number     */
    } l_addr;
    unsigned short l_lnno; /* line number                */
} LINENO;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct {
    uint16_t e_magic;    // Magic number
    uint16_t e_cblp;     // Bytes on last page of file
    uint16_t e_cp;       // Pages in file
    uint16_t e_crlc;     // Relocations
    uint16_t e_cparhdr;  // Size of header in paragraphs
    uint16_t e_minalloc; // Minimum extra paragraphs needed
    uint16_t e_maxalloc; // Maximum extra paragraphs needed
    uint16_t e_ss;       // Initial (relative) SS value
    uint16_t e_sp;       // Initial SP value
    uint16_t e_csum;     // Checksum
    uint16_t e_ip;       // Initial IP value
    uint16_t e_cs;       // Initial (relative) CS value
    uint16_t e_lfarlc;   // File address of relocation table
    uint16_t e_ovno;     // Overlay number
    uint16_t e_res[4];   // Reserved words
    uint16_t e_oemid;    // OEM identifier (for e_oeminfo)
    uint16_t e_oeminfo;  // OEM information; e_oemid specific
    uint16_t e_res2[10]; // Reserved words
    uint32_t e_lfanew;   // File address of new exe header
} PE_DosHeader;

typedef struct {
    uint32_t magic; // PE\0\0 or 0x00004550
    uint16_t machine;
    uint16_t section_count;
    uint32_t timestamp;
    uint32_t symbol_table;
    uint32_t symbol_count;
    uint16_t size_of_optional_header;
    uint16_t characteristics;
} PE_Header;

typedef struct {
    uint32_t virtual_address;
    uint32_t size;
} PE_ImageDataDirectory;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16
typedef struct {
    uint16_t magic;
    uint8_t major_linker_version;
    uint8_t minor_linker_version;
    uint32_t size_of_code;
    uint32_t size_of_initialized_data;
    uint32_t size_of_uninitialized_data;
    uint32_t entrypoint;
    uint32_t base_of_code;
    uint64_t image_base;
    uint32_t section_alignment;
    uint32_t file_alignment;
    uint16_t major_os_ver;
    uint16_t minor_os_ver;
    uint16_t major_image_ver;
    uint16_t minor_image_ver;
    uint16_t major_subsystem_ver;
    uint16_t minor_subsystem_ver;
    uint32_t win32_version_value;
    uint32_t size_of_image;
    uint32_t size_of_headers;
    uint32_t checksum;
    uint16_t subsystem;
    uint16_t dll_characteristics;
    uint64_t size_of_stack_reserve;
    uint64_t size_of_stack_commit;
    uint64_t size_of_heap_reserve;
    uint64_t size_of_heap_commit;
    uint32_t loader_flags;
    uint32_t rva_size_count;
    PE_ImageDataDirectory data_directories[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} PE_OptionalHeader64;

typedef struct { // size 40 bytes
    char name[8];
    uint32_t virtual_size;
    uint32_t virtual_address;
    uint32_t size_of_raw_data;
    uint32_t pointer_to_raw_data;
    uint32_t pointer_to_relocs;
    uint32_t pointer_to_linenos;
    uint16_t relocation_count;
    uint16_t linenos_count;
    uint32_t characteristics;
} PE_SectionHeader;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct {
    uint16_t len;       // doesn't include itself, so sizeof(T)-2
    uint16_t leaf;      // LF_PROCEDURE
    uint32_t rvtype;    // type index of return value
    uint8_t  calltype;  // calling convention (CV_call_t)
    uint8_t  funcattr;  // attributes
    uint16_t parmcount; // number of parameters
    uint32_t arglist;   // type index of argument list
} CV_LFProc;

typedef struct {
    uint16_t len;     // doesn't include itself, so sizeof(T)-2
    uint16_t leaf;    // LF_FUNC_ID
    uint32_t scopeId; // parent scope of the ID, 0 if global
    uint32_t type;    // function type
    uint8_t  name[];
} CV_LFFuncID;

typedef struct {
    uint16_t reclen; // Record length
    uint16_t rectyp; // S_REGREL32
    uint32_t off;    // offset of symbol
    uint32_t typind; // Type index or metadata token
    uint16_t reg;    // register index for symbol
    uint8_t  name[]; // Length-prefixed name
} CV_RegRel32;
#pragma pack(pop)

// represents a CodeView type entry, they start with 16bits for length field
typedef struct CV_TypeEntry {
    uint32_t key;   // points to somewhere in the debug$T section, 0 is assumed to mean nothing
    uint16_t value; // type index
} CV_TypeEntry;

enum {
    COFF_MACHINE_AMD64 = 0x8664, // AMD64 (K8)
    COFF_MACHINE_ARM64 = 0xAA64, // ARM64 Little-Endian
};

enum {
    S_LPROC32_ID = 0x1146,
    S_GPROC32_ID = 0x1147,

    S_INLINESITE     = 0x114d, // inlined function callsite.
    S_INLINESITE_END = 0x114e,
    S_PROC_ID_END    = 0x114f,

    S_FRAMEPROC = 0x1012, // extra frame and proc information
    S_REGREL32  = 0x1111, // register relative address
};

typedef enum
{
    UWOP_PUSH_NONVOL = 0, /* info == register number */
    UWOP_ALLOC_LARGE,     /* no info, alloc size in next 2 slots */
    UWOP_ALLOC_SMALL,     /* info == size of allocation / 8 - 1 */
    UWOP_SET_FPREG,       /* no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 */
    UWOP_SAVE_NONVOL,     /* info == register number, offset in next slot */
    UWOP_SAVE_NONVOL_FAR, /* info == register number, offset in next 2 slots */
    UWOP_SAVE_XMM128 = 8, /* info == XMM reg number, offset in next slot */
    UWOP_SAVE_XMM128_FAR, /* info == XMM reg number, offset in next 2 slots */
    UWOP_PUSH_MACHFRAME   /* info == 0: no error-code, 1: error-code */
} UNWIND_CODE_OPS;
