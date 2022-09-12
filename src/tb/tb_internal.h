#pragma once

// Windows likes it's secure functions, i kinda do too
// but only sometimes and this isn't one of them
#if defined(_WIN32) && !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "tb.h"
#include <limits.h>
#include <time.h>
#include <stdalign.h>

#if defined(_MSC_VER) && !defined(__clang__)
#include <immintrin.h>
#define thread_local __declspec(thread)
#else
#define thread_local _Thread_local
#endif

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
// NOTE(NeGate): I love how we assume that if it's not windows
// its just posix, these are the only options i guess
#include <fcntl.h>
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

#include "tb_platform.h"
#include "bigint/BigInt.h"
#include "dyn_array.h"
#include "builtins.h"
#include "pool.h"

#define NL_STRING_MAP_INLINE
#define NL_STRING_MAP_IMPL
#include "string_map.h"

// ***********************************
// Constraints
// ***********************************
// TODO: get rid of all these
#ifndef TB_MAX_THREADS
#define TB_MAX_THREADS 16
#endif

// Per-thread
#ifndef TB_TEMPORARY_STORAGE_SIZE
#define TB_TEMPORARY_STORAGE_SIZE (1 << 20)
#endif

#ifndef TB_MAX_FUNCTIONS
#define TB_MAX_FUNCTIONS (1 << 22)
#endif

// ***********************************
// Atomics
// ***********************************
// since some modern C11 compilers (MSVC...) don't support
// C11 atomics we'll just write a stripped down layer that
// handles all we really want from atomics.
//
// These return the old values.
typedef int    tb_atomic_int;
typedef size_t tb_atomic_size_t;

int tb_atomic_int_load(int* dst);
int tb_atomic_int_add(int* dst, int src);
int tb_atomic_int_store(int* dst, int src);

size_t tb_atomic_size_load(size_t* dst);
size_t tb_atomic_size_add(size_t* dst, size_t src);
size_t tb_atomic_size_store(size_t* dst, size_t src);

#define PROTOTYPES_ARENA_SIZE   (32u << 20u)
#define CODE_REGION_BUFFER_SIZE (128 * 1024 * 1024)

typedef struct TB_Emitter {
    size_t capacity, count;
    uint8_t* data;
} TB_Emitter;

#define TB_FIXED_ARRAY(T) \
struct { size_t cap, count; T* elems; }

#define TB_FIXED_ARRAY_APPEND(arr, elem) \
(((arr).count + 1 <= (arr).cap) ? ((arr).elems[(arr).count++] = (elem)) : (void) assert(!"out of bounds"))

#define TB_DATA_TYPE_EQUALS(a, b) ((a).raw == (b).raw)
#define TB_DT_EQUALS(a, b) ((a).raw == (b).raw)

#ifdef TB_FOR_BASIC_BLOCK
#undef TB_FOR_BASIC_BLOCK

// simple optimization of it within the TB code
// since we have access to the structures
#define TB_FOR_BASIC_BLOCK(it, f) for (TB_Label it = 0; it < f->bb_count; it++)
#endif

#define TB_FOR_NODE(it, f, bb) for (TB_Reg it = f->bbs[bb].start; it != 0; it = f->nodes[it].next)
#define TB_FOR_NODE_AFTER(it, f, first) for (TB_Reg it = f->nodes[reg].start; it != 0; it = f->nodes[it].next)

typedef struct TB_ConstPoolPatch {
    TB_Function* source;
    uint32_t pos; // relative to the start of the function body

    size_t rdata_pos;

    size_t length;
    const void* data;
} TB_ConstPoolPatch;

typedef struct TB_GlobalPatch {
    TB_Function* source;
    uint32_t pos; // relative to the start of the function body
    const TB_Global* target;
} TB_GlobalPatch;

typedef struct TB_FunctionPatch {
    TB_Function* source;
    uint32_t pos; // relative to the start of the function body
    const TB_Function* target;
} TB_FunctionPatch;

typedef struct TB_ExternFunctionPatch {
    TB_Function* source;
    uint32_t pos; // relative to the start of the function body
    const TB_External* target;
} TB_ExternFunctionPatch;

typedef struct TB_File {
    char* path;
} TB_File;

struct TB_External {
    char* name;
    void* address;
};

struct TB_Global {
    char* name;
    TB_Linkage linkage;
    TB_Initializer* init;
    int id;
    uint32_t pos;
    TB_StorageClass storage;
};

typedef struct TB_PrototypeParam {
    TB_DataType dt;
    char* name;
    TB_DebugType* debug_type;
} TB_PrototypeParam;

// function prototypes are stored
// in streams as inplace arrays:
//
// PROTOTYPE0, arg0, arg1, PROTOTYPE1, arg0, arg1
struct TB_FunctionPrototype {
    // header
    TB_CallingConv call_conv;

    short param_capacity;
    short param_count;

    TB_DataType return_dt;
    bool has_varargs;

    // payload
    TB_PrototypeParam params[];
};

typedef struct TB_InitObj {
    enum {
        TB_INIT_OBJ_REGION,

        // relocations
        TB_INIT_OBJ_RELOC_EXTERN,
        TB_INIT_OBJ_RELOC_FUNCTION,
        TB_INIT_OBJ_RELOC_GLOBAL
    } type;
    TB_CharUnits offset;
    union {
        struct {
            TB_CharUnits size;
            const void* ptr;
        } region;

        const TB_External* reloc_extern;
        const TB_Function* reloc_function;
        const TB_Global* reloc_global;
    };
} TB_InitObj;

struct TB_Initializer {
    // header
    TB_CharUnits size, align;
    uint32_t obj_capacity;
    uint32_t obj_count;

    // payload
    TB_InitObj objects[];
};

struct TB_DebugType {
    enum {
        TB_DEBUG_TYPE_VOID,
        TB_DEBUG_TYPE_BOOL,

        TB_DEBUG_TYPE_UINT,
        TB_DEBUG_TYPE_INT,
        TB_DEBUG_TYPE_FLOAT,

        TB_DEBUG_TYPE_ARRAY,
        TB_DEBUG_TYPE_POINTER,

        // special types
        TB_DEBUG_TYPE_FIELD,

        // aggregates
        // TODO(NeGate): apparently codeview has cool vector and matrix types... yea
        TB_DEBUG_TYPE_STRUCT,
        TB_DEBUG_TYPE_UNION,
    } tag;

    // debug-info target specific data
    union {
        struct {
            uint16_t cv_type_id;
            uint16_t cv_type_id_fwd; // used by records to manage forward decls
        };
    };

    // tag specific
    union {
        int int_bits;
        TB_FloatFormat float_fmt;
        TB_DebugType* ptr_to;
        struct {
            TB_DebugType* base;
            size_t count;
        } array;
        struct {
            char* name;
            TB_CharUnits offset;
            TB_DebugType* type;
        } field;
        struct TB_DebugTypeRecord {
            TB_CharUnits size, align;

            size_t count;
            TB_DebugType** members;
        } record;
    };
};

typedef struct TB_Line {
    TB_FileID file;
    int line;
    uint32_t pos;
} TB_Line;

typedef enum {
    TB_ATTRIB_NONE,
    TB_ATTRIB_VARIABLE,
} TB_AttribType;

struct TB_Attrib {
    TB_Attrib* next;
    TB_AttribType type;

    union {
        struct {
            char* name;
            TB_DebugType* storage;
        } var;
    };
};

typedef struct TB_StackSlot {
    TB_Reg source;
    // TODO(NeGate): support complex variable descriptions
    // currently we only support stack relative
    int position;

    const char* name;
    TB_DebugType* storage_type;
} TB_StackSlot;

typedef struct TB_FunctionOutput {
    TB_Linkage linkage;
    uint8_t prologue_length;
    uint8_t epilogue_length;

    // NOTE(NeGate): This data is actually specific to the
    // architecture run but generically can be thought of as
    // 64bits which keep track of which registers to save.
    uint64_t prologue_epilogue_metadata;
    uint64_t stack_usage;

    uint8_t* code;
    size_t code_size;

    DynArray(TB_StackSlot) stack_slots;
} TB_FunctionOutput;

struct TB_Function {
    char* name;
    // It's kinda a weird circular reference but yea
    TB_Module* module;

    const TB_FunctionPrototype* prototype;
    TB_Linkage linkage;

    // Parameter acceleration structure
    TB_Reg* params;

    // Basic block array (also makes up the CFG as an implicit graph)
    size_t bb_capacity, bb_count;
    TB_BasicBlock* bbs;

    // Nodes array
    TB_Reg node_capacity, node_count, node_end;
    TB_Node* nodes;

    // Used by the IR building
    TB_Reg last_reg;
    TB_Reg current_label;

    // Used by nodes which have variable
    // length arguments like PHI and CALL.
    // SWITCH has arguments here too but they
    // are two slots each
    struct {
        size_t capacity;
        size_t count;
        TB_Reg* data;
    } vla;

    // Attribute pool
    size_t attrib_pool_capacity;
    size_t attrib_pool_count;
    TB_Attrib* attrib_pool;

    // Part of the debug info
    size_t line_count;
    TB_Line* lines;

    // Compilation output
    void* compiled_pos;
    TB_FunctionOutput* output;
};

typedef struct {
    uint32_t func_id;
    uint32_t label_id;
    uint32_t pos; // relative to the start of the function
} TB_LabelSymbol;

typedef struct {
    size_t size;
    uint8_t data[CODE_REGION_BUFFER_SIZE - sizeof(size_t)];
} TB_CodeRegion;

struct TB_Module {
    int max_threads;
    bool is_jit;

    TB_ABI target_abi;
    TB_Arch target_arch;
    TB_System target_system;
    TB_FeatureSet features;

    // This is a hack for windows since they've got this idea
    // of a _tls_index
    TB_External* tls_index_extern;

    // Convert this into a dynamic memory arena... maybe
    tb_atomic_size_t prototypes_arena_size;
    uint64_t* prototypes_arena;

    alignas(64) struct {
        Pool(TB_DebugType) debug_types;
        Pool(TB_Global) globals;
        Pool(TB_External) externals;

        DynArray(TB_GlobalPatch) global_patches;
        DynArray(TB_ConstPoolPatch) const_patches;
        DynArray(TB_FunctionPatch) call_patches;
        DynArray(TB_ExternFunctionPatch) ecall_patches;
    } thread_info[TB_MAX_THREADS];

    struct {
        size_t count;
        size_t capacity;
        TB_File* data;
    } files;

    struct {
        tb_atomic_size_t compiled_count;
        tb_atomic_size_t count;
        TB_Function* data;
    } functions;

    void* jit_region;
    size_t jit_region_size;

    // we need to keep track of these for layout reasons
    tb_atomic_size_t data_region_size;
    tb_atomic_size_t rdata_region_size;
    tb_atomic_size_t tls_region_size;

    // The code is stored into giant buffers
    // there's on per code gen thread so that
    // each can work at the same time without
    // making any allocations within the code
    // gen.
    TB_CodeRegion* code_regions[TB_MAX_THREADS];
};

typedef struct {
    size_t length;
    TB_ObjectSection* data;
} TB_SectionGroup;

typedef struct {
    uint32_t used;
    uint8_t data[];
} TB_TemporaryStorage;

// the maximum size the prologue and epilogue can be for any machine code impl
#define PROEPI_BUFFER 256

typedef struct {
    // what does CHAR_BIT mean on said platform
    int minimum_addressable_size, pointer_size;

    void (*get_data_type_size)(TB_DataType dt, TB_CharUnits* out_size, TB_CharUnits* out_align);
    void (*emit_call_patches)(TB_Module* restrict m, uint32_t* restrict func_layout);

    size_t (*emit_prologue)(uint8_t* out, uint64_t saved, uint64_t stack_usage);
    size_t (*emit_epilogue)(uint8_t* out, uint64_t saved, uint64_t stack_usage);

    // NULLable if doesn't apply
    void (*emit_win64eh_unwind_info)(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t saved, uint64_t stack_usage);

    TB_FunctionOutput (*fast_path)(TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
    TB_FunctionOutput (*complex_path)(TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
} ICodeGen;

// All debug formats i know of boil down to adding some extra sections to the object file
typedef struct {
    const char* name;

    bool (*supported_target)(TB_Module* m);
    int (*number_of_debug_sections)(TB_Module* m);

    // functions are laid out linearly based on their function IDs and
    // thus function_sym_start tells you what the starting point is in the symbol table
    TB_SectionGroup (*generate_debug_info)(TB_Module* m, TB_TemporaryStorage* tls, const ICodeGen* code_gen, const char* path, size_t function_sym_start, uint32_t* func_layout);
} IDebugFormat;

// Macro enjoyer
#define tb_swap(T, a, b) \
do {                     \
    T temp = a;          \
    a = b;               \
    b = temp;            \
} while (0)

#ifndef NDEBUG
#define TB_DEBUG_BUILD 1
#else
#define TB_DEBUG_BUILD 0
#endif

#define TB_FITS_INTO(T,x) ((x) == (T)(x))

// tb_todo means it's something we fill in later
// tb_unreachable means it's logically impossible to reach
// tb_assume means we assume some expression cannot be false
//
// in debug builds these are all checked and tb_todo is some sort of trap
#if defined(_MSC_VER) && !defined(__clang__)
#if TB_DEBUG_BUILD
#define tb_todo()            (assert(0 && "TODO"), __assume(0))
#define tb_unreachable()     (assert(0), __assume(0))
#define tb_assume(condition) assert(condition)
#else
#define tb_todo()            abort()
#define tb_unreachable()     __assume(0)
#define tb_assume(condition) __assume(condition)
#endif
#else
#if TB_DEBUG_BUILD
#define tb_todo()            __builtin_trap()
#define tb_unreachable()     assert(0)
#define tb_assume(condition) assert(condition)
#else
#define tb_todo()            __builtin_trap()
#define tb_unreachable()     __builtin_unreachable()
#define tb_assume(condition) ((condition) ? 0 : (void) __builtin_unreachable())
#endif
#endif

#define tb_assert(condition, ...)    \
do {                                 \
    if (!(condition)) {              \
        fprintf(stderr, __VA_ARGS__);\
        abort();                     \
    }                                \
} while (0)

#define tb_assert_once(msg) (fprintf(stderr, "%s:%d: assert_once \"%s\"\n", __FILE__, __LINE__, msg), abort())

#ifdef _WIN32
#define tb_panic(...)                     \
do {                                      \
    printf(__VA_ARGS__);                  \
    __fastfail(FAST_FAIL_FATAL_APP_EXIT); \
} while (0)
#else
#define tb_panic(...)                     \
do {                                      \
    printf(__VA_ARGS__);                  \
    abort();                              \
} while (0)
#endif

#ifndef COUNTOF
#define COUNTOF(...) (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))
#endif

#define FOREACH_N(it, start, end) \
for (ptrdiff_t it = (start), end__ = (end); it < end__; ++it)

#define FOREACH_REVERSE_N(it, start, end) \
for (ptrdiff_t it = (end), start__ = (start); (it--) > start__;)

#define loop(iterator, count) \
for (size_t iterator = 0, end__ = (count); iterator < end__; ++iterator)

#define loop_range(iterator, start, count) \
for (size_t iterator = (start), end__ = (count); iterator < end__; ++iterator)

#define loop_reverse(iterator, count) \
for (size_t iterator = (count); iterator--;)

// sometimes you just gotta do it to em'
// imagine i++ but like i++y (more like ((i += y) - y)) or something idk
inline static size_t tb_post_inc(size_t* a, size_t b) {
    size_t old = *a;
    *a = old + b;
    return old;
}

// NOTE(NeGate): if you steal it you should restore the used amount back to what it was before
TB_TemporaryStorage* tb_tls_steal(void);
TB_TemporaryStorage* tb_tls_allocate(void);
void* tb_tls_push(TB_TemporaryStorage* store, size_t size);
void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size);
void tb_tls_restore(TB_TemporaryStorage* store, void* ptr);
void* tb_tls_pop(TB_TemporaryStorage* store, size_t size);
void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance);
bool tb_tls_can_fit(TB_TemporaryStorage* store, size_t size);

ICodeGen* tb__find_code_generator(TB_Module* m);

// object file output
void* tb_elf64__make(TB_Module* m, const IDebugFormat* dbg);
bool tb_elf64__next(TB_Module* m, void* exporter, TB_ModuleExportPacket* packet);

void* tb_coff__make(TB_Module* m, const IDebugFormat* dbg);
bool tb_coff__next(TB_Module* m, void* exporter, TB_ModuleExportPacket* packet);

void* tb_out_reserve(TB_Emitter* o, size_t count);
// The return value is the start of the empty region after
// the data, this is where you can start appending new data
// and you're guarenteed `count` bytes. if this function is
// called again assume this pointer is invalid because it
// might be reallocated.

void tb_out_commit(TB_Emitter* o, size_t count);
// Commits `count` bytes to the emitter, increments the size
// of the data, you should have used `tb_out_reserve` to get
// that data pointer and thus guarentee the space was available

void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i);
void tb_out4b_UNSAFE(TB_Emitter* o, uint32_t i);
void tb_outstr_UNSAFE(TB_Emitter* o, const char* str);
void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const void* str);
void tb_outs(TB_Emitter* o, size_t len, const void* str);

void tb_out_zero(TB_Emitter* o, size_t len);
// fills region with zeros

void tb_out1b(TB_Emitter* o, uint8_t i);
void tb_out2b(TB_Emitter* o, uint16_t i);
void tb_out4b(TB_Emitter* o, uint32_t i);
void tb_out8b(TB_Emitter* o, uint64_t i);
void tb_patch1b(TB_Emitter* o, uint32_t pos, uint8_t i);
void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i);
void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i);

uint8_t  tb_get1b(TB_Emitter* o, uint32_t pos);
uint16_t tb_get2b(TB_Emitter* o, uint32_t pos);
uint32_t tb_get4b(TB_Emitter* o, uint32_t pos);

////////////////////////////////
// IR ANALYSIS
////////////////////////////////
TB_Label tb_find_label_from_reg(TB_Function* f, TB_Reg target);
TB_Reg tb_find_first_use(const TB_Function* f, TB_Reg find, size_t start, size_t end);
void tb_function_find_replace_reg(TB_Function* f, TB_Reg find, TB_Reg replace);
size_t tb_count_uses(const TB_Function* f, TB_Reg find, size_t start, size_t end);
void tb_function_reserve_nodes(TB_Function* f, size_t extra);
TB_Reg tb_insert_copy_ops(TB_Function* f, const TB_Reg* params, TB_Reg at, const TB_Function* src_func, TB_Reg src_base, int count);
TB_Reg tb_function_insert_before(TB_Function* f, TB_Reg at);
TB_Reg tb_function_insert_after(TB_Function* f, TB_Label bb, TB_Reg at);

inline static void tb_murder_node(TB_Function* f, TB_Node* n) {
    n->type = TB_NULL;
}

inline static void tb_murder_reg(TB_Function* f, TB_Reg r) {
    f->nodes[r].type = TB_NULL;
}

inline static void tb_kill_op(TB_Function* f, TB_Reg at) {
    f->nodes[at].type = TB_NULL;
}

inline static uint64_t align_up(uint64_t a, uint64_t b) {
    return a + (b - (a % b)) % b;
}

// NOTE(NeGate): Considers 0 as a power of two
inline static bool tb_is_power_of_two(uint64_t x) {
    return (x & (x - 1)) == 0;
}

// gets the next biggest number to 'v' in the sorted array
// if 'v' is too big, then it'll return false, if not it's
// true and 'result' will store the number we got
#define TB_NEXT_BIGGEST(result, v, ...) \
tb_next_biggest(result, v, COUNTOF((int[]) { __VA_ARGS__ }), (int[]) { __VA_ARGS__ })

inline static bool tb_next_biggest(int* result, int v, size_t n, const int* arr) {
    loop(i, n) if (v <= arr[i]) {
        *result = arr[i];
        return true;
    }

    return false;
}

////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////
#ifdef _MSC_VER
#define TB_LIKELY(x)   (!!(x))
#define TB_UNLIKELY(x) (!!(x))
#else
#define TB_LIKELY(x)   __builtin_expect(!!(x), 1)
#define TB_UNLIKELY(x) __builtin_expect(!!(x), 0)
#endif

// NOTE(NeGate): clean this up
#if 1
#define OPTIMIZER_LOG(at, ...) ((void) (at))
#else
#define OPTIMIZER_LOG(at, ...)                 \
do {                                           \
    printf("%s:r%d: ", f->name, (TB_Reg)(at)); \
    printf(__VA_ARGS__);                       \
    printf(" (part of %s)\n", __FUNCTION__);   \
} while (0)
#endif

#define CALL_NODE_PARAM_COUNT(n) (n->call.param_end - n->call.param_start)

////////////////////////////////
// ANALYSIS
////////////////////////////////
int tb__get_local_tid(void);

// TODO(NeGate): refactor this stuff such that it starts with two underscores, it makes
// it more clear that these are TB private
void tb_function_calculate_use_count(const TB_Function* f, int use_count[]);
int tb_function_find_uses_of_node(const TB_Function* f, TB_Reg def, TB_Reg uses[]);

// if tls is NULL then the return value is heap allocated
TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count);
TB_Predeccesors tb_get_temp_predeccesors(TB_Function* f, TB_TemporaryStorage* tls);

uint32_t tb_emit_const_patch(TB_Module* m, TB_Function* source, size_t pos, const void* ptr,size_t len, size_t local_thread_id);
void tb_emit_global_patch(TB_Module* m, TB_Function* source, size_t pos, const TB_Global* target, size_t local_thread_id);
void tb_emit_call_patch(TB_Module* m, TB_Function* source, const TB_Function* target, size_t pos, size_t local_thread_id);
void tb_emit_ecall_patch(TB_Module* m, TB_Function* source, const TB_External* target, size_t pos, size_t local_thread_id);

TB_Reg* tb_vla_reserve(TB_Function* f, size_t count);

// trusty lil hash functions
uint32_t tb__crc32(uint32_t crc, size_t length, const void* data);

// out_bytes needs at least 16 bytes
void tb__md5sum(uint8_t* out_bytes, uint8_t* initial_msg, size_t initial_len);

inline static bool tb_data_type_match(const TB_DataType* a, const TB_DataType* b) {
    return a->type == b->type && a->width == b->width;
}

// NOTE(NeGate): Place all the codegen interfaces down here
extern ICodeGen tb__x64_codegen;
extern ICodeGen tb__x64v2_codegen;
extern ICodeGen tb__aarch64_codegen;

// And all debug formats here
//extern IDebugFormat dwarf_debug_format;
extern IDebugFormat tb__codeview_debug_format;
