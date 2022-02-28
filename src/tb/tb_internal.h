#pragma once
#include "tb.h"
#include <time.h>
#include <limits.h>

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
#include <unistd.h>
#include <sys/mman.h>
#include <pthread.h>
#include <fcntl.h>
#endif

#include "tb_platform.h"

typedef uint32_t TB_CompiledFunctionID;

// ***********************************
// Atomics
// ***********************************
// since some modern C11 compilers (MSVC...) don't support
// C11 atomics we'll just write a stripped down layer that
// handles all we really want from atomics.
//
// These return the old values.
typedef int tb_atomic_int;
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
#define STBDS_REALLOC(c,p,s) tb_platform_heap_realloc(p,s)
#define STBDS_FREE(c,p) tb_platform_heap_free(p)
#include "stb_ds.h"

// cool part about stb_ds is that the dynamic arrays
// look and act like normal arrays for access and types.
#define dyn_array(T) T*

#define PROTOTYPES_ARENA_SIZE (32u << 20u)
#define CODE_REGION_BUFFER_SIZE (512 * 1024 * 1024)

typedef struct TB_Emitter {
	size_t capacity, count;
	uint8_t* data;
} TB_Emitter;

#define TB_DATA_TYPE_EQUALS(a, b) tb_data_type_match(&(a), &(b))

// first is the null register, then the entry
// label, then parameters, then everything else
#define TB_FIRST_PARAMETER_REG 2

#define TB_FOR_EACH_NODE(elem, f) \
for (TB_Node* elem = &f->nodes.data[1]; elem != &f->nodes.data[0]; elem = &f->nodes.data[elem->next])

#define TB_FOR_EACH_NODE_RANGE(elem, f, start, end) \
for (TB_Node* elem = &f->nodes.data[start], *end__ = &f->nodes.data[end]; elem != end__; elem = &f->nodes.data[elem->next])

#define TB_FOR_EACH_REG_IN_NODE(macro) \
case TB_NULL: \
case TB_SIGNED_CONST: \
case TB_UNSIGNED_CONST: \
case TB_FLOAT_CONST: \
case TB_STRING_CONST: \
case TB_LOCAL: \
case TB_PARAM: \
case TB_GOTO: \
case TB_LINE_INFO: \
case TB_FUNC_ADDRESS: \
case TB_EXTERN_ADDRESS: \
case TB_GLOBAL_ADDRESS: \
case TB_DEBUGBREAK: \
break; \
case TB_LABEL: \
macro(n->label.terminator); \
break; \
case TB_INITIALIZE: \
macro(n->init.addr); \
break; \
case TB_KEEPALIVE: \
case TB_RESTRICT: \
case TB_VA_START: \
case TB_NOT: \
case TB_NEG: \
case TB_X86INTRIN_SQRT: \
case TB_X86INTRIN_RSQRT: \
case TB_INT2PTR: \
case TB_PTR2INT: \
case TB_INT2FLOAT: \
case TB_FLOAT2INT: \
case TB_TRUNCATE: \
case TB_BITCAST: \
macro(n->unary.src); \
break; \
case TB_ATOMIC_XCHG: \
case TB_ATOMIC_ADD: \
case TB_ATOMIC_SUB: \
case TB_ATOMIC_AND: \
case TB_ATOMIC_XOR: \
case TB_ATOMIC_OR: \
macro(n->atomic.addr); \
macro(n->atomic.src); \
break; \
case TB_MEMCPY: \
case TB_MEMSET: \
macro(n->mem_op.dst); \
macro(n->mem_op.src); \
macro(n->mem_op.size); \
break; \
case TB_MEMBER_ACCESS: \
macro(n->member_access.base); \
break; \
case TB_ARRAY_ACCESS: \
macro(n->array_access.base); \
macro(n->array_access.index); \
break; \
case TB_PARAM_ADDR: \
macro(n->param_addr.param); \
break; \
case TB_PASS: \
macro(n->pass.value); \
break; \
case TB_PHI1: \
macro(n->phi1.a); \
macro(n->phi1.a_label); \
break; \
case TB_PHI2: \
macro(n->phi2.a); \
macro(n->phi2.b); \
macro(n->phi2.a_label); \
macro(n->phi2.b_label); \
break; \
case TB_LOAD: \
macro(n->load.address); \
break; \
case TB_STORE: \
macro(n->store.address); \
macro(n->store.value); \
break; \
case TB_ZERO_EXT: \
case TB_SIGN_EXT: \
case TB_FLOAT_EXT: \
macro(n->unary.src); \
break; \
case TB_AND: \
case TB_OR: \
case TB_XOR: \
case TB_ADD: \
case TB_SUB: \
case TB_MUL: \
case TB_UDIV: \
case TB_SDIV: \
case TB_UMOD: \
case TB_SMOD: \
case TB_SAR: \
case TB_SHL: \
case TB_SHR: \
macro(n->i_arith.a); \
macro(n->i_arith.b); \
break; \
case TB_FADD: \
case TB_FSUB: \
case TB_FMUL: \
case TB_FDIV: \
macro(n->f_arith.a); \
macro(n->f_arith.b); \
break; \
case TB_CMP_EQ: \
case TB_CMP_NE: \
case TB_CMP_SLT: \
case TB_CMP_SLE: \
case TB_CMP_ULT: \
case TB_CMP_ULE: \
case TB_CMP_FLT: \
case TB_CMP_FLE: \
macro(n->cmp.a); \
macro(n->cmp.b); \
break; \
case TB_VCALL: \
macro(n->vcall.target); \
for (size_t slot__ = n->call.param_start; slot__ < n->call.param_end; slot__++) { \
macro(f->vla.data[slot__]); \
} \
break; \
case TB_CALL: \
case TB_ICALL: \
case TB_ECALL: \
for (size_t slot__ = n->call.param_start; slot__ < n->call.param_end; slot__++) { \
macro(f->vla.data[slot__]); \
} \
break; \
case TB_SWITCH: \
macro(n->switch_.key); \
break; \
case TB_IF: \
macro(n->if_.cond); \
break; \
case TB_RET: \
macro(n->ret.value); \
break

typedef struct TB_ConstPoolPatch {
	TB_CompiledFunctionID source;
	uint32_t pos; // relative to the start of the function
	
	size_t rdata_pos;
	
	const void* data;
	size_t length;
} TB_ConstPoolPatch;

typedef struct TB_GlobalPatch {
	TB_CompiledFunctionID source;
	uint32_t pos; // relative to the start of the function
	TB_GlobalID global;
} TB_GlobalPatch;

typedef struct TB_FunctionPatch {
	TB_CompiledFunctionID source;
	uint32_t target_id;
	uint32_t pos; // relative to the start of the function
} TB_FunctionPatch;

typedef struct TB_ExternFunctionPatch {
	TB_CompiledFunctionID source;
	TB_ExternalID target_id;
	uint32_t pos; // relative to the start of the function
} TB_ExternFunctionPatch;

typedef struct TB_FunctionOutput {
	const char* name;
	TB_FunctionID function;
	TB_Linkage linkage;
	
	// NOTE(NeGate): This data is actually specific to the
	// architecture run but generically can be thought of as
	// 64bits which keep track of which registers to save.
	uint64_t prologue_epilogue_metadata;
	uint64_t stack_usage;
	
	uint8_t* code;
	size_t code_size;
} TB_FunctionOutput;

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
	size_t pos;
} TB_Global;

typedef struct TB_Line {
	TB_FileID file;
	int line;
	uint32_t pos;
} TB_Line;

struct TB_Function {
	char* name;
	
	// It's kinda a weird circular reference but yea
	TB_Module* module;
	
	const TB_FunctionPrototype* prototype;
	TB_Linkage linkage;
	
	struct TB_NodeStream {
		TB_Reg    capacity;
		TB_Reg    count;
		TB_Reg    end;
		TB_Node*  data;
	} nodes;
	
	// Used by the IR building
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
	
	// Part of the debug info
	size_t line_count;
	TB_Line* lines;
};

typedef struct TB_LabelSymbol {
	uint32_t func_id;
	uint32_t label_id;
	uint32_t pos; // relative to the start of the function
} TB_LabelSymbol;

typedef struct TB_CodeRegion {
	size_t size;
	uint8_t data[CODE_REGION_BUFFER_SIZE - sizeof(size_t)];
} TB_CodeRegion;

struct TB_Module {
	int max_threads;
    
	TB_Arch target_arch;
	TB_System target_system;
	TB_FeatureSet features;
	
	// Convert this into a dynamic memory arena... maybe
	tb_atomic_size_t prototypes_arena_size;
	uint64_t* prototypes_arena;
	
#if !TB_STRIP_LABELS
	dyn_array(TB_LabelSymbol) label_symbols;
#endif
	
#if _WIN32
	CRITICAL_SECTION mutex;
#else
	pthread_mutex_t mutex;
#endif
	
	// TODO(NeGate): I should probably re-organize these to avoid
	// false sharing
	dyn_array(TB_GlobalPatch) global_patches[TB_MAX_THREADS];
	dyn_array(TB_ConstPoolPatch) const_patches[TB_MAX_THREADS];
	dyn_array(uint64_t) initializers[TB_MAX_THREADS];
	dyn_array(TB_External) externals[TB_MAX_THREADS];
	dyn_array(TB_Global) globals[TB_MAX_THREADS];
	dyn_array(TB_FunctionPatch) call_patches[TB_MAX_THREADS];
    dyn_array(TB_ExternFunctionPatch) ecall_patches[TB_MAX_THREADS];
	
	struct {
		size_t count;
		size_t capacity;
		TB_File* data;
	} files;
    
	struct {
		tb_atomic_size_t count;
		TB_Function* data;
	} functions;
	
	struct {
		tb_atomic_size_t count;
		TB_FunctionOutput* data;
	} compiled_functions;
	
	void* jit_region;
	size_t jit_region_size;
	
	// If not NULL, there's JITted functions in each 
	// non NULL entry which map to the `compiled_functions` 
	void** compiled_function_pos;
	
	// we need to keep track of these for layout reasons
	tb_atomic_size_t data_region_size;
	tb_atomic_size_t rdata_region_size;
	
	// The code is stored into giant buffers
	// there's on per code gen thread so that
	// each can work at the same time without
	// making any allocations within the code
	// gen.
	TB_CodeRegion* code_regions[TB_MAX_THREADS];
};

typedef struct TB_TemporaryStorage {
	uint32_t used;
	uint8_t data[];
} TB_TemporaryStorage;

// the maximum size the prologue and epilogue can be for any machine code impl
#define PROEPI_BUFFER 256

typedef struct ICodeGen {
	void(*emit_call_patches)(TB_Module* m, uint32_t* func_layout);
	
	size_t(*get_prologue_length)(uint64_t saved, uint64_t stack_usage);
	size_t(*get_epilogue_length)(uint64_t saved, uint64_t stack_usage);
	size_t(*emit_prologue)(uint8_t* out, uint64_t saved, uint64_t stack_usage);
	size_t(*emit_epilogue)(uint8_t* out, uint64_t saved, uint64_t stack_usage);
	
	TB_FunctionOutput(*fast_path)(TB_CompiledFunctionID id, TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
	TB_FunctionOutput(*complex_path)(TB_CompiledFunctionID id, TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
} ICodeGen;

#define tb_swap(type, a, b) do { \
type temp = a; \
a = b; \
b = temp; \
} while(0)

#ifndef NDEBUG
#define TB_DEBUG_BUILD 1
#else
#define TB_DEBUG_BUILD 0
#endif

#if defined(_MSC_VER) && !defined(__clang__)
#  if TB_DEBUG_BUILD
#    define tb_todo() assert(0 && "TODO")
#    define tb_unreachable() assert(0)
#    define tb_assume(condition) assert(condition)
#  else
#    define tb_todo() abort()
#    define tb_unreachable() __assume(0)
#    define tb_assume(condition) __assume(condition)
#  endif
#else
#  if TB_DEBUG_BUILD
#    define tb_todo() __builtin_trap()
#    define tb_unreachable() assert(0)
#    define tb_assume(condition) assert(condition)
#  else
#    define tb_todo() __builtin_trap()
#    define tb_unreachable() __builtin_unreachable()
#    define tb_assume(condition) __builtin_unreachable()
#  endif
#endif

#define tb_assert(condition, ...) do { if (!(condition)) { printf(__VA_ARGS__); abort(); } } while (0)
#define tb_panic(...) do { printf(__VA_ARGS__); abort(); } while (0)
#define tb_arrlen(a) (sizeof(a) / sizeof(a[0]))

#ifndef COUNTOF
#define COUNTOF(a) (sizeof(a) / sizeof(a[0]))
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
	
	if (_BitScanForward(&index, x)) return 1+index;
	else return 0;
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
	return (TB_MultiplyResult){ lo, hi };
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
    
	return (TB_MultiplyResult){ 
		(uint64_t)(product & 0xFFFFFFFFFFFFFFFF),
		(uint64_t)(product >> 64)
	};
}
#endif

bool tb_validate(TB_Function* f);

TB_TemporaryStorage* tb_tls_allocate();
void* tb_tls_push(TB_TemporaryStorage* store, size_t size);
void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size);
void tb_tls_restore(TB_TemporaryStorage* store, void* ptr);
void* tb_tls_pop(TB_TemporaryStorage* store, size_t size);
void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance);
bool tb_tls_can_fit(TB_TemporaryStorage* store, size_t size);

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, bool emit_debug_info);
void tb_export_elf64(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, bool emit_debug_info);

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

uint8_t tb_get1b(TB_Emitter* o, uint32_t pos);
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

inline static void tb_kill_op(TB_Function* f, TB_Reg at) {
	f->nodes.data[at].type = TB_NULL;
}

inline static int align_up(int a, int b) { return a + (b - (a % b)) % b; }

////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////
#ifdef _MSC_VER
#define TB_LIKELY(x)      (!!(x))
#define TB_UNLIKELY(x)    (!!(x))
#else
#define TB_LIKELY(x)      __builtin_expect(!!(x), 1)
#define TB_UNLIKELY(x)    __builtin_expect(!!(x), 0)
#endif

// NOTE(NeGate): clean this up
#if 1
#define OPTIMIZER_LOG(at, ...)
#else
#define OPTIMIZER_LOG(at, ...) do { \
printf("%s:r%d: ", f->name, (TB_Reg)(at)); \
printf(__VA_ARGS__); \
printf(" (part of %s)\n", __FUNCTION__); \
} while (0)
#endif

#define CALL_NODE_PARAM_COUNT(n) \
(n->call.param_end - n->call.param_start)

// NOTE(NeGate): Considers 0 as a power of two
inline static bool tb_is_power_of_two(size_t x) {
    return (x & (x - 1)) == 0;
}

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

uint32_t tb_emit_const_patch(TB_Module* m, TB_CompiledFunctionID source, size_t pos, const void* ptr, size_t len, size_t local_thread_id);
void tb_emit_global_patch(TB_Module* m, TB_CompiledFunctionID source, size_t pos, TB_GlobalID global, size_t local_thread_id);
void tb_emit_call_patch(TB_Module* m, TB_CompiledFunctionID source, uint32_t target_id, size_t pos, size_t local_thread_id);
void tb_emit_ecall_patch(TB_Module* m, TB_CompiledFunctionID source, TB_ExternalID target_id, size_t pos, size_t local_thread_id);

#if !TB_STRIP_LABELS
void tb_emit_label_symbol(TB_Module* m, uint32_t func_id, uint32_t label_id, size_t pos);
#endif

TB_Reg* tb_vla_reserve(TB_Function* f, size_t count);

inline static bool tb_data_type_match(const TB_DataType* a, const TB_DataType* b) {
	return a->type == b->type && a->width == b->width;
}

// NOTE(NeGate): Place all the codegen interfaces down here
extern ICodeGen x64_codegen;
