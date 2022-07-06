#pragma once

// Windows likes it's secure functions, i kinda do too
// but only sometimes and this isn't one of them
#if defined(_WIN32) && !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "tb.h"
#include <limits.h>
#include <time.h>

#if defined(_MSC_VER) && !defined(__clang__)
#include <immintrin.h>
#define thread_local __declspec(thread)
#else
#define thread_local _Thread_local
#endif

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
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

// ***********************************
// STB Data structures
// ***********************************
#include "stb_ds.h"

// cool part about stb_ds is that the dynamic arrays
// look and act like normal arrays for access and types.
#define DynArray(T) T*

#define PROTOTYPES_ARENA_SIZE   (32u << 20u)
#define CODE_REGION_BUFFER_SIZE (128 * 1024 * 1024)

typedef struct TB_Emitter {
    size_t capacity, count;
    uint8_t* data;
} TB_Emitter;

#define TB_DATA_TYPE_EQUALS(a, b) ((a).raw == (b).raw)
#define TB_DT_EQUALS(a, b) ((a).raw == (b).raw)

// first is the null register, then the entry
// label, then parameters, then everything else
#define TB_FIRST_PARAMETER_REG 2
#define TB_NEXT_INST(node, f) (&(f)->nodes.data[(node)->next])

#define TB_FOR_EACH_NODE(elem, f) \
for (TB_Node* elem = &f->nodes.data[1]; elem != &f->nodes.data[0]; elem = &f->nodes.data[elem->next])

#define TB_FOR_EACH_NODE_BB(elem, f, start) \
for (TB_Node* elem = &f->nodes.data[f->nodes.data[start].next], *end__ = &f->nodes.data[0]; elem != end__ && elem->type != TB_LABEL; \
    elem = &f->nodes.data[elem->next])

#define TB_FOR_EACH_NODE_RANGE(elem, f, start, end)                                      \
for (TB_Node* elem = &f->nodes.data[start], *end__ = &f->nodes.data[end]; elem != end__; \
    elem = &f->nodes.data[elem->next])

#define TB_FOR_EACH_REG_IN_NODE(macro)                                                \
case TB_NULL:                                                                         \
case TB_INTEGER_CONST:                                                                \
case TB_FLOAT_CONST:                                                                  \
case TB_STRING_CONST:                                                                 \
case TB_LOCAL:                                                                        \
case TB_PARAM:                                                                        \
case TB_GOTO:                                                                         \
case TB_LINE_INFO:                                                                    \
case TB_FUNC_ADDRESS:                                                                 \
case TB_EXTERN_ADDRESS:                                                               \
case TB_GLOBAL_ADDRESS:                                                               \
case TB_DEBUGBREAK: break;                                                            \
case TB_LABEL: macro(n->label.terminator); break;                                     \
case TB_INITIALIZE: macro(n->init.addr); break;                                       \
case TB_KEEPALIVE:                                                                    \
case TB_VA_START:                                                                     \
case TB_NOT:                                                                          \
case TB_NEG:                                                                          \
case TB_X86INTRIN_SQRT:                                                               \
case TB_X86INTRIN_RSQRT:                                                              \
case TB_INT2PTR:                                                                      \
case TB_PTR2INT:                                                                      \
case TB_UINT2FLOAT:                                                                   \
case TB_FLOAT2UINT:                                                                   \
case TB_INT2FLOAT:                                                                    \
case TB_FLOAT2INT:                                                                    \
case TB_TRUNCATE:                                                                     \
case TB_BITCAST: macro(n->unary.src); break;                                          \
case TB_ATOMIC_XCHG:                                                                  \
case TB_ATOMIC_ADD:                                                                   \
case TB_ATOMIC_SUB:                                                                   \
case TB_ATOMIC_AND:                                                                   \
case TB_ATOMIC_XOR:                                                                   \
case TB_ATOMIC_OR:                                                                    \
macro(n->atomic.addr);                                                                \
macro(n->atomic.src);                                                                 \
break;                                                                                \
case TB_MEMCPY:                                                                       \
case TB_MEMSET:                                                                       \
macro(n->mem_op.dst);                                                                 \
macro(n->mem_op.src);                                                                 \
macro(n->mem_op.size);                                                                \
break;                                                                                \
case TB_MEMBER_ACCESS: macro(n->member_access.base); break;                           \
case TB_ARRAY_ACCESS:                                                                 \
macro(n->array_access.base);                                                          \
macro(n->array_access.index);                                                         \
break;                                                                                \
case TB_PARAM_ADDR: macro(n->param_addr.param); break;                                \
case TB_PASS: macro(n->pass.value); break;                                            \
case TB_PHI1:                                                                         \
macro(n->phi1.inputs[0].label);                                                       \
macro(n->phi1.inputs[0].val);                                                         \
break;                                                                                \
case TB_PHI2:                                                                         \
macro(n->phi2.inputs[0].label);                                                       \
macro(n->phi2.inputs[0].val);                                                         \
macro(n->phi2.inputs[1].label);                                                       \
macro(n->phi2.inputs[1].val);                                                         \
break;                                                                                \
case TB_PHIN:                                                                         \
loop(i, n->phi.count) {                                                               \
    macro(n->phi.inputs[i].label);                                                    \
    macro(n->phi.inputs[i].val);                                                      \
}                                                                                     \
break;                                                                                \
case TB_LOAD: macro(n->load.address); break;                                          \
case TB_STORE:                                                                        \
macro(n->store.address);                                                              \
macro(n->store.value);                                                                \
break;                                                                                \
case TB_ZERO_EXT:                                                                     \
case TB_SIGN_EXT:                                                                     \
case TB_FLOAT_EXT: macro(n->unary.src); break;                                        \
case TB_AND:                                                                          \
case TB_OR:                                                                           \
case TB_XOR:                                                                          \
case TB_ADD:                                                                          \
case TB_SUB:                                                                          \
case TB_MUL:                                                                          \
case TB_UDIV:                                                                         \
case TB_SDIV:                                                                         \
case TB_UMOD:                                                                         \
case TB_SMOD:                                                                         \
case TB_SAR:                                                                          \
case TB_SHL:                                                                          \
case TB_SHR:                                                                          \
macro(n->i_arith.a);                                                                  \
macro(n->i_arith.b);                                                                  \
break;                                                                                \
case TB_FADD:                                                                         \
case TB_FSUB:                                                                         \
case TB_FMUL:                                                                         \
case TB_FDIV:                                                                         \
macro(n->f_arith.a);                                                                  \
macro(n->f_arith.b);                                                                  \
break;                                                                                \
case TB_CMP_EQ:                                                                       \
case TB_CMP_NE:                                                                       \
case TB_CMP_SLT:                                                                      \
case TB_CMP_SLE:                                                                      \
case TB_CMP_ULT:                                                                      \
case TB_CMP_ULE:                                                                      \
case TB_CMP_FLT:                                                                      \
case TB_CMP_FLE:                                                                      \
macro(n->cmp.a);                                                                      \
macro(n->cmp.b);                                                                      \
break;                                                                                \
case TB_VCALL:                                                                        \
macro(n->vcall.target);                                                               \
for (size_t slot__ = n->call.param_start; slot__ < n->call.param_end; slot__++) {     \
    macro(f->vla.data[slot__]);                                                       \
}                                                                                     \
break;                                                                                \
case TB_CALL:                                                                         \
case TB_ICALL:                                                                        \
case TB_ECALL:                                                                        \
for (size_t slot__ = n->call.param_start; slot__ < n->call.param_end; slot__++) {     \
    macro(f->vla.data[slot__]);                                                       \
}                                                                                     \
break;                                                                                \
case TB_SWITCH: macro(n->switch_.key); break;                                         \
case TB_IF: macro(n->if_.cond); break;                                                \
case TB_RET: macro(n->ret.value); break

typedef struct TB_ConstPoolPatch {
    TB_Function* source;
    uint32_t pos; // relative to the start of the function

    size_t rdata_pos;

    size_t length;
    const void* data;
} TB_ConstPoolPatch;

typedef struct TB_GlobalPatch {
    TB_Function* source;
    uint32_t pos; // relative to the start of the function
    TB_GlobalID global;
} TB_GlobalPatch;

typedef struct TB_FunctionPatch {
    TB_Function* source;
    uint32_t target_id;
    uint32_t pos; // relative to the start of the function
} TB_FunctionPatch;

typedef struct TB_ExternFunctionPatch {
    TB_Function* source;
    TB_ExternalID target_id;
    uint32_t pos; // relative to the start of the function
} TB_ExternFunctionPatch;

typedef struct TB_File {
    char* path;
} TB_File;

typedef struct TB_External {
    char* name;
    void* address;
} TB_External;

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
    TB_DataType params[];
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

        TB_ExternalID reloc_extern;
        TB_FunctionID reloc_function;
        TB_GlobalID reloc_global;
    };
} TB_InitObj;

typedef struct TB_Initializer {
    // header
    TB_CharUnits size, align;
    uint32_t obj_capacity;
    uint32_t obj_count;

    // payload
    TB_InitObj objects[];
} TB_Initializer;

typedef struct TB_Global {
    char* name;
    TB_Linkage linkage;
    TB_InitializerID init;
    uint32_t pos;
    TB_StorageClass storage;
} TB_Global;

typedef struct TB_Line {
    TB_FileID file;
    int line;
    uint32_t pos;
} TB_Line;

typedef enum {
    TB_ATTRIB_NONE,
    TB_ATTRIB_RESTRICT,
    TB_ATTRIB_SCOPE
} TB_AttribType;

typedef struct {
    TB_AttribType type;
    TB_AttributeID ref;
} TB_Attrib;

// linked lists amirite
struct TB_AttribList {
    TB_AttribList* next;
    TB_AttributeID attrib;
};

typedef struct TB_FunctionOutput {
    TB_Linkage linkage;

    // NOTE(NeGate): This data is actually specific to the
    // architecture run but generically can be thought of as
    // 64bits which keep track of which registers to save.
    uint64_t prologue_epilogue_metadata;
    uint64_t stack_usage;

    uint8_t* code;
    size_t code_size;
} TB_FunctionOutput;

struct TB_Function {
    char* name;
    // It's kinda a weird circular reference but yea
    TB_Module* module;

    const TB_FunctionPrototype* prototype;
    TB_Linkage linkage;

    struct TB_NodeStream {
        TB_Reg capacity;
        TB_Reg count;
        TB_Reg end;
        TB_Node* data;
    } nodes;

    // Used by the IR building
    TB_AttributeID active_attrib;
    TB_Reg last_reg;
    TB_Reg current_label;
    TB_Label label_count;

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

    TB_ABI target_abi;
    TB_Arch target_arch;
    TB_System target_system;
    TB_DebugFormat debug_fmt;
    TB_FeatureSet features;

    // This is a hack for windows since they've got this idea
    // of a _tls_index
    TB_ExternalID tls_index_extern;

    // Convert this into a dynamic memory arena... maybe
    tb_atomic_size_t prototypes_arena_size;
    uint64_t* prototypes_arena;

    // TODO(NeGate): I should probably re-organize these to avoid
    // false sharing
    DynArray(TB_GlobalPatch) global_patches[TB_MAX_THREADS];
    DynArray(TB_ConstPoolPatch) const_patches[TB_MAX_THREADS];
    DynArray(uint64_t) initializers[TB_MAX_THREADS];
    DynArray(TB_External) externals[TB_MAX_THREADS];
    DynArray(TB_Global) globals[TB_MAX_THREADS];
    DynArray(TB_FunctionPatch) call_patches[TB_MAX_THREADS];
    DynArray(TB_ExternFunctionPatch) ecall_patches[TB_MAX_THREADS];

    // TODO(NeGate): start migrating all those dyn arrays into this array
    struct {
        int empty_for_now;
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

    // If not NULL, there's JITted functions in each
    // non NULL entry which map to the `compiled_functions`
    void** compiled_function_pos;

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
    void (*emit_call_patches)(TB_Module* m, uint32_t* func_layout);

    size_t (*get_prologue_length)(uint64_t saved, uint64_t stack_usage);
    size_t (*get_epilogue_length)(uint64_t saved, uint64_t stack_usage);
    size_t (*emit_prologue)(uint8_t* out, uint64_t saved, uint64_t stack_usage);
    size_t (*emit_epilogue)(uint8_t* out, uint64_t saved, uint64_t stack_usage);

    TB_FunctionOutput (*fast_path)(TB_FunctionID id, TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
    TB_FunctionOutput (*complex_path)(TB_FunctionID id, TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
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
#define tb_todo()            assert(0 && "TODO")
#define tb_unreachable()     assert(0)
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
#define tb_assume(condition) __builtin_unreachable()
#endif
#endif

#define tb_assert(condition, ...) \
do {                              \
    if (!(condition)) {           \
        printf(__VA_ARGS__);      \
        abort();                  \
    }                             \
} while (0)

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

#define TB_ARRLEN(...) (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))

#ifndef COUNTOF
#define COUNTOF(...) (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))
#endif

#define loop(iterator, count) \
for (size_t iterator = 0, end__ = (count); iterator < end__; ++iterator)

#define loop_range(iterator, start, count) \
for (size_t iterator = (start), end__ = (count); iterator < end__; ++iterator)

#define loop_reverse(iterator, count) \
for (size_t iterator = (count); iterator--;)

typedef struct TB_MultiplyResult {
    uint64_t lo;
    uint64_t hi;
} TB_MultiplyResult;

#if defined(_MSC_VER) && !defined(__clang__)
inline static int tb_ffs(uint32_t x) {
    unsigned long index;
    return _BitScanForward(&index, x) ? 1 + index : 0;
}

inline static int tb_popcount(uint32_t x) {
    return __popcnt(x);
}

inline static uint64_t tb_next_pow2(uint64_t x) {
    return x == 1 ? 1 : 1 << (64 - _lzcnt_u64(x - 1));
}

inline static bool tb_add_overflow(uint64_t a, uint64_t b, uint64_t* result) {
    uint64_t c = a + b;
    *result = c;
    return c < a;
}

inline static bool tb_sub_overflow(uint64_t a, uint64_t b, uint64_t* result) {
    uint64_t c = a - b;
    *result = c;
    return c > a;
}

#pragma intrinsic(_umul128)
inline static TB_MultiplyResult tb_mul64x128(uint64_t a, uint64_t b) {
    uint64_t hi;
    uint64_t lo = _umul128(a, b, &hi);
    return (TB_MultiplyResult) { lo, hi };
}
#else
inline static int tb_ffs(uint32_t x) {
    return __builtin_ffs(x);
}

inline static int tb_popcount(uint32_t x) {
    return __builtin_popcount(x);
}

inline static uint64_t tb_next_pow2(uint64_t x) {
    return x == 1 ? 1 : 1 << (64 - __builtin_clzl(x - 1));
}

inline static bool tb_add_overflow(uint64_t a, uint64_t b, uint64_t* result) {
    return __builtin_add_overflow(a, b, result);
}

inline static bool tb_sub_overflow(uint64_t a, uint64_t b, uint64_t* result) {
    return __builtin_sub_overflow(a, b, result);
}

inline static TB_MultiplyResult tb_mul64x128(uint64_t a, uint64_t b) {
    __uint128_t product = (__uint128_t)a * (__uint128_t)b;

    return (TB_MultiplyResult) { (uint64_t)(product & 0xFFFFFFFFFFFFFFFF), (uint64_t)(product >> 64) };
}
#endif

// sometimes you just gotta do it to em'
// imagine i++ but like i++y or something idk
inline static size_t tb_post_inc(size_t* a, size_t b) {
    size_t old = *a;
    *a = old + b;
    return old;
}

bool tb_validate(TB_Function* f);

TB_TemporaryStorage* tb_tls_allocate();
void* tb_tls_push(TB_TemporaryStorage* store, size_t size);
void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size);
void tb_tls_restore(TB_TemporaryStorage* store, void* ptr);
void* tb_tls_pop(TB_TemporaryStorage* store, size_t size);
void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance);
bool tb_tls_can_fit(TB_TemporaryStorage* store, size_t size);

// object file output
void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, const IDebugFormat* debug_fmt);
void tb_export_macho(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, const IDebugFormat* debug_fmt);
void tb_export_elf64(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, const IDebugFormat* debug_fmt);

// executable file format
void tb_export_pe(TB_Module* m, const ICodeGen* restrict code_gen, const TB_LinkerInput* restrict input, const char* path, const IDebugFormat* debug_fmt);

uint8_t* tb_out_reserve(TB_Emitter* o, size_t count);
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
void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const uint8_t* str);

void tb_out1b(TB_Emitter* o, uint8_t i);
void tb_out2b(TB_Emitter* o, uint16_t i);
void tb_out4b(TB_Emitter* o, uint32_t i);
void tb_out8b(TB_Emitter* o, uint64_t i);
void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i);
void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i);

uint8_t  tb_get1b(TB_Emitter* o, uint32_t pos);
uint16_t tb_get2b(TB_Emitter* o, uint32_t pos);
uint32_t tb_get4b(TB_Emitter* o, uint32_t pos);

////////////////////////////////
// IR ANALYSIS
////////////////////////////////
TB_Reg tb_find_reg_from_label(TB_Function* f, TB_Label id);
TB_Reg tb_find_first_use(const TB_Function* f, TB_Reg find, size_t start, size_t end);
void tb_function_find_replace_reg(TB_Function* f, TB_Reg find, TB_Reg replace);
size_t tb_count_uses(const TB_Function* f, TB_Reg find, size_t start, size_t end);
void tb_function_reserve_nodes(TB_Function* f, size_t extra);
TB_Reg tb_insert_copy_ops(TB_Function* f, const TB_Reg* params, TB_Reg at, const TB_Function* src_func, TB_Reg src_base, int count);
TB_Reg tb_function_insert_after(TB_Function* f, TB_Reg at);

inline static void tb_murder_node(TB_Function* f, TB_Node* n) {
    n->type = TB_NULL;
}

inline static void tb_kill_op(TB_Function* f, TB_Reg at) {
    f->nodes.data[at].type = TB_NULL;
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
#define LOGGING_OPTS 0
#else
#define OPTIMIZER_LOG(at, ...)                 \
do {                                           \
    printf("%s:r%d: ", f->name, (TB_Reg)(at)); \
    printf(__VA_ARGS__);                       \
    printf(" (part of %s)\n", __FUNCTION__);   \
} while (0)
#define LOGGING_OPTS 1
#endif

#define CALL_NODE_PARAM_COUNT(n) (n->call.param_end - n->call.param_start)

////////////////////////////////
// CONSTANT FOLDING UTILS
////////////////////////////////
uint64_t tb_fold_add(TB_ArithmaticBehavior ab, TB_DataType dt, uint64_t a, uint64_t b);
uint64_t tb_fold_sub(TB_ArithmaticBehavior ab, TB_DataType dt, uint64_t a, uint64_t b);
uint64_t tb_fold_mul(TB_ArithmaticBehavior ab, TB_DataType dt, uint64_t a, uint64_t b);
uint64_t tb_fold_div(TB_DataType dt, uint64_t a, uint64_t b);

////////////////////////////////
// ANALYSIS
////////////////////////////////
void tb_function_calculate_use_count(const TB_Function* f, int use_count[]);
int tb_function_find_uses_of_node(const TB_Function* f, TB_Reg def, TB_Reg uses[]);
TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count);

uint32_t tb_emit_const_patch(TB_Module* m, TB_Function* source, size_t pos, const void* ptr,size_t len, size_t local_thread_id);
void tb_emit_global_patch(TB_Module* m, TB_Function* source, size_t pos, TB_GlobalID global, size_t local_thread_id);
void tb_emit_call_patch(TB_Module* m, TB_Function* source, uint32_t target_id, size_t pos, size_t local_thread_id);
void tb_emit_ecall_patch(TB_Module* m, TB_Function* source, TB_ExternalID target_id, size_t pos, size_t local_thread_id);

TB_Reg* tb_vla_reserve(TB_Function* f, size_t count);

// trusty lil hash functions
uint32_t tb__crc32(uint32_t crc, size_t length, uint8_t* data);

// out_bytes needs at least 16 bytes
void tb__md5sum(uint8_t* out_bytes, uint8_t* initial_msg, size_t initial_len);

inline static bool tb_data_type_match(const TB_DataType* a, const TB_DataType* b) {
    return a->type == b->type && a->width == b->width;
}

// NOTE(NeGate): Place all the codegen interfaces down here
extern ICodeGen x64_codegen;

// And all debug formats here
//extern IDebugFormat dwarf_debug_format;
extern IDebugFormat codeview_debug_format;
