// Private header stuff, don't include TB_INTERNAL into your code,
// it's for the other implementation files of TB
//
//
// The internal structure of a function's IR:
// It's broken down into streams which are easy to scan through and 
// they are:
// 
// TB_RegType      reg_types[reg_count]
// TB_DataType     reg_data_types[reg_count]
// TB_RegPayload   reg_payload[reg_count]
//
// the TB_Register is an index into these streams and each unique value
// maps to a valid IR register except 0 which is reserved as the null register.
// TODO(NeGate): Consider refactoring the internals of this... please
#pragma once
#include "tb.h"
#include <time.h>
#include <limits.h>
#include <stdatomic.h>

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
#include <semaphore.h>
#endif

#include "tb_platform.h"

#define STBDS_REALLOC(c,p,s) tb_platform_heap_realloc(p,s)
#define STBDS_FREE(c,p) tb_platform_heap_free(p)
#include "stb_ds.h"

// cool part about stb_ds is that the dynamic arrays
// look and act like normal arrays for access and types.
#define dyn_array(T) T*

#define safe_cast(T, src) ({ \
typeof(src) val = (src); \
T casted = (T)val; \
if (val != casted) __builtin_trap(); \
casted; \
})

#define MAX_JOBS_PER_JOB_SYSTEM 4096

// TODO(NeGate): eventually i'll make it dynamic
#define PROTOTYPES_ARENA_SIZE (16u << 20u)

#define CODE_REGION_BUFFER_SIZE (512 * 1024 * 1024)

typedef struct TB_Emitter {
	size_t capacity, count;
	uint8_t* data;
} TB_Emitter;

// NOTE(NeGate): Don't reorder this stuff... bitch
typedef enum TB_RegTypeEnum {
    TB_NULL,
	
	// instructions with side-effects
	TB_LINE_INFO,
	TB_KEEPALIVE,
	TB_INITIALIZE,
	
	TB_ICALL, /* internal use only, inline call */
	TB_CALL,
    TB_VCALL, /* virtual call */
	TB_ECALL, /* extern call */
	
	TB_LOAD,
    TB_STORE,
	
	TB_MEMCPY,
	TB_MEMSET,
	TB_MEMCMP,
	
	// Terminators
    TB_LABEL,
    TB_GOTO,
    TB_SWITCH,
    TB_IF,
    TB_RET,
	TB_TRAP,
	TB_UNREACHABLE, /* it's undefined behavior to reach this */
	
	// Immediates
	TB_INT_CONST,
    TB_FLOAT_CONST,
    TB_STRING_CONST,
    
    // Casts
    TB_TRUNCATE,
    TB_FLOAT_EXT,
    TB_SIGN_EXT,
    TB_ZERO_EXT,
    
	// Unary ops
	TB_NOT,
	TB_NEG,
	
    // Integer arithmatic
    TB_AND,
	TB_OR,
	TB_XOR,
    TB_ADD,
    TB_SUB,
    TB_MUL,
	
    TB_SHL,
    TB_SHR,
    TB_SAR,
    TB_UDIV,
    TB_SDIV,
    
    // Float arithmatic
    TB_FADD,
    TB_FSUB,
    TB_FMUL,
    TB_FDIV,
    
    // Comparisons
    TB_CMP_EQ,
    TB_CMP_NE,
    TB_CMP_SLT,
    TB_CMP_SLE,
    TB_CMP_ULT,
    TB_CMP_ULE,
    TB_CMP_FLT,
    TB_CMP_FLE,
    
    // Conversions
    TB_INT2PTR,
    TB_PTR2INT,
    
    // Memory
    TB_LOCAL,
    TB_PARAM,
    TB_PARAM_ADDR,
	
	TB_FUNC_ADDRESS,
	TB_EFUNC_ADDRESS,
	TB_GLOBAL_ADDRESS,
	
	// Pointer math
	TB_MEMBER_ACCESS,
	TB_ARRAY_ACCESS,
    
    // PHI
    // NOTE(NeGate): I provide multiple shorthands
	// because we ideally don't want to use the VLA
	// space.
	TB_PHI1,
    TB_PHI2,
	TB_PHIN,
	
    // NOTE(NeGate): only used internally, if you
    // see one in normal IR things went wrong in
    // an optimization pass
    TB_PASS,
	
	// helpful for certain scans
	TB_SIDE_EFFECT_MIN = TB_LINE_INFO,
	TB_SIDE_EFFECT_MAX = TB_MEMCMP,
	
	TB_TERMINATOR_MIN = TB_LABEL,
	TB_TERMINATOR_MAX = TB_RET
} TB_RegTypeEnum;

typedef uint8_t TB_RegType;

#define TB_IS_NODE_SIDE_EFFECT(type) ((type) >= TB_SIDE_EFFECT_MIN && (type) <= TB_SIDE_EFFECT_MAX)
#define TB_IS_NODE_TERMINATOR(type) ((type) >= TB_TERMINATOR_MIN && (type) <= TB_TERMINATOR_MAX)
#define TB_DATA_TYPE_EQUALS(a, b) (memcmp(&(a), &(b), sizeof(TB_DataType)) == 0)
#define TB_DATA_TYPE_NOT_EQUALS(a, b) (memcmp(&(a), &(b), sizeof(TB_DataType)) != 0)

#define TB_FIRST_PARAMETER_REG 2

typedef union TB_RegPayload {
	uint32_t raw[4];
	
	uint64_t i_const;
	double f_const;
	struct {
		size_t len;
		const char* data;
	} str_const;
	TB_Register keepalive;
	TB_Register ext;
	TB_Register trunc;
	TB_Register ptrcast;
	const TB_Function* func_addr;
	TB_ExternalID efunc_addr;
	TB_GlobalID global_addr;
	struct {
		TB_FileID file;
		int line;
		
		// NOTE(NeGate): Used by the object code generation
		uint32_t pos;
	} line_info;
	struct {
		TB_Register base;
		int32_t offset;
	} member_access;
	struct {
		TB_Register base;
		TB_Register index;
		int32_t stride;
	} array_access;
	struct {
		uint32_t id;
		uint32_t size;
	} param;
	struct {
		TB_Register param;
		
		uint32_t size;
		uint32_t alignment;
	} param_addr;
	struct {
		uint32_t size;
		uint32_t alignment;
	} local;
	TB_Register unary;
	struct {
		TB_Register a;
		TB_Register b;
		TB_ArithmaticBehavior arith_behavior;
	} i_arith;
	struct {
		TB_Register a;
		TB_Register b;
	} f_arith;
	struct {
		TB_Register a;
		TB_Register b;
		TB_DataType dt;
	} cmp;
	struct {
		TB_Register src;
	} cvt;
	struct {
		TB_Register address;
		uint32_t alignment;
		bool is_volatile;
	} load;
	struct {
		TB_Register address;
		TB_Register value;
		uint32_t alignment;
		bool is_volatile;
	} store;
	struct {
		TB_Register value;
	} ret;
	TB_Register pass;
	struct {
		TB_Register a_label;
		TB_Register a;
	} phi1;
	struct {
		TB_Register a_label;
		TB_Register a;
		TB_Register b_label;
		TB_Register b;
	} phi2;
	struct {
		int param_start, param_end;
	} phin;
	struct {
		TB_Label id;
		TB_Register terminator;
		bool is_loop;
	} label;
	struct {
		TB_Register cond;
		TB_Label if_true;
		TB_Label if_false;
	} if_;
	struct {
		TB_Label label;
	} goto_;
	struct {
		int param_start, param_end;
		TB_ExternalID target;
	} ecall;
	struct {
		int param_start, param_end;
		TB_Register target;
	} vcall;
	struct {
		int param_start, param_end;
		const TB_Function* target;
	} call;
	struct {
		TB_Register key;
		TB_Label default_label;
		int entries_start, entries_end;
	} switch_;
	struct {
		TB_Register dst;
		TB_Register src;
		TB_Register size;
		int align;
	} mem_op;
	struct {
		TB_Register addr;
		TB_InitializerID id;
	} init;
} TB_RegPayload;
_Static_assert(sizeof(TB_RegPayload) == 16, "TB_RegPayload shouldn't exceed 128bits for any option");

typedef struct TB_ConstPoolPatch {
	uint32_t func_id;
	uint32_t pos; // relative to the start of the function
	
	size_t rdata_pos;
	
	const void* data;
	size_t length;
} TB_ConstPoolPatch;

typedef struct TB_GlobalPatch {
	uint32_t func_id;
	uint32_t pos; // relative to the start of the function
	TB_GlobalID global;
} TB_GlobalPatch;

typedef struct TB_FunctionPatch {
	uint32_t func_id;
	uint32_t target_id;
	uint32_t pos; // relative to the start of the function
} TB_FunctionPatch;

typedef struct TB_ExternFunctionPatch {
	uint32_t func_id;
	TB_ExternalID target_id;
	uint32_t pos; // relative to the start of the function
} TB_ExternFunctionPatch;

typedef struct TB_FunctionOutput {
	const char* name;
	
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
		TB_INIT_OBJ_RELOC_FUNC,
		TB_INIT_OBJ_RELOC_GLOBAL
	} type;
	uint32_t offset;
	union {
		struct {
			uint32_t size;
			const void* ptr;
		} region;
		
		TB_ExternalID reloc_extern;
		TB_Function* reloc_function;
		TB_GlobalID reloc_global;
	};
} TB_InitObj;

typedef struct TB_Initializer {
	// header
	uint32_t size, align;
	uint32_t obj_capacity;
	uint32_t obj_count;
	
	// payload
	TB_InitObj objects[];
} TB_Initializer;

typedef struct TB_Global {
	char* name;
	TB_InitializerID init;
	size_t pos;
} TB_Global;

struct TB_Function {
	// It's kinda a weird circular reference but yea
	TB_Module* module;
	
	char* name;
	const TB_FunctionPrototype* prototype;
	
	struct TB_NodeStream {
		TB_Register    capacity;
		TB_Register    count;
		
		TB_RegType*    type;
		TB_DataType*   dt;
		TB_RegPayload* payload;
	} nodes;
	
	// Used by the IR building
	TB_Register current_label;
	TB_Label label_count;
	
	// Used by nodes which have variable
	// length arguments like PHI and CALL.
	// SWITCH has arguments here too but they
	// are two slots each
	struct {
		size_t capacity;
		size_t count;
		TB_Register* data;
	} vla;
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
	
	// This number is calculated while the builders are running
	// if the optimizations are run this number is set to SIZE_MAX
	// which means it needs to be re-evaluated.
	_Atomic size_t line_info_count;
	
	// Convert this into a dynamic memory arena... maybe
	_Atomic size_t prototypes_arena_size;
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
		_Atomic size_t count;
		TB_Function* data;
	} functions;
	
	struct {
		_Atomic size_t count;
		TB_FunctionOutput* data;
	} compiled_functions;
	
	void* jit_region;
	size_t jit_region_size;
	
	// If not NULL, there's JITted functions in each 
	// non NULL entry which map to the `compiled_functions` 
	void** compiled_function_pos;
	
	// we need to keep track of these for layout reasons
	_Atomic size_t data_region_size;
	_Atomic size_t rdata_region_size;
	
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
#define PROEPI_BUFFER 512

typedef struct ICodeGen {
	void(*emit_call_patches)(TB_Module* m, uint32_t func_layout[]);
	
	size_t(*get_prologue_length)(uint64_t saved, uint64_t stack_usage);
	size_t(*get_epilogue_length)(uint64_t saved, uint64_t stack_usage);
	size_t(*emit_prologue)(uint8_t out[], uint64_t saved, uint64_t stack_usage);
	size_t(*emit_epilogue)(uint8_t out[], uint64_t saved, uint64_t stack_usage);
	
	TB_FunctionOutput(*compile_function)(TB_Function* f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id);
} ICodeGen;

// Used internally for some optimizations
#define tb_swap(a, b) do { \
typeof(a) temp = a; \
a = b; \
b = temp; \
} while(0)

#ifndef NDEBUG
#define tb_unreachable() __builtin_trap()
#define tb_todo() __builtin_trap()
#else
#define tb_unreachable() __builtin_unreachable()
#define tb_todo() __builtin_unreachable()
#endif

#define tb_arrlen(a) (sizeof(a) / sizeof(a[0]))

#define loop(iterator, count) \
for (typeof(count) iterator = 0, end__ = (count); iterator < end__; ++iterator)

#define loop_range(iterator, start, count) \
for (typeof(count) iterator = (start), end__ = (count); iterator < end__; ++iterator)

#define loop_reverse(iterator, count) \
for (typeof(count) iterator = (count); iterator--;)

inline static uint64_t tb_next_pow2(uint64_t x) {
	return x == 1 ? 1 : 1 << (64 - __builtin_clzl(x - 1));
}

bool tb_validate(TB_Function* f);

TB_TemporaryStorage* tb_tls_allocate();
void* tb_tls_push(TB_TemporaryStorage* store, size_t size);
void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size);
void* tb_tls_pop(TB_TemporaryStorage* store, size_t size);
void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance);

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, const char* path);
void tb_export_elf64(TB_Module* m, const ICodeGen* restrict code_gen, const char* path);

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

//
// IR ANALYSIS
//
TB_Register tb_find_reg_from_label(TB_Function* f, TB_Label id);
TB_Register tb_find_first_use(const TB_Function* f, TB_Register find, size_t start, size_t end);
void tb_function_find_replace_reg(TB_Function* f, TB_Register find, TB_Register replace);
size_t tb_count_uses(const TB_Function* f, TB_Register find, size_t start, size_t end);
void tb_insert_op(TB_Function* f, TB_Register at);
void tb_resize_node_stream(TB_Function* f, size_t cap);
TB_Register tb_insert_copy_ops(TB_Function* f, const TB_Register* params, TB_Register at, const TB_Function* src_func, TB_Register src_base, int count);

inline static void tb_kill_op(TB_Function* f, TB_Register at) {
	f->nodes.type[at] = TB_NULL;
	f->nodes.dt[at] = (TB_DataType){ 0 };
	f->nodes.payload[at] = (TB_RegPayload){ 0 };
}

//
// HELPER FUNCTIONS
//
#define TB_LIKELY(x)      __builtin_expect(!!(x), 1)
#define TB_UNLIKELY(x)    __builtin_expect(!!(x), 0)

#define CALL_NODE_PARAM_COUNT(f, i) \
((f)->nodes.payload[i].call.param_end - (f)->nodes.payload[i].call.param_start)

#if TB_HOST_ARCH == TB_HOST_X86_64
#define FOR_EACH_NODE(iterator, func, target, ...) \
for (size_t __i = 0, cnt = (func)->nodes.count; __i < cnt; __i += 16) { \
__m128i bytes = _mm_loadu_si128((__m128i*)&(func)->nodes.type[__i]); \
unsigned int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(target))); \
if (mask == 0) continue; \
/* We know it loops at least once by this point */ \
for (size_t __j = 0; __j < 16; __j++) if (mask & (1u << __j)) { \
size_t iterator = __i + __j; \
__VA_ARGS__; \
} \
}
#else
#define FOR_EACH_NODE(iterator, func, target, ...) \
for (size_t iterator = 0; iterator < (func)->nodes.count; iterator++) { \
if ((func)->nodes.type[iterator] == (target)) { \
__VA_ARGS__; \
} \
}
#endif

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
void tb_find_live_intervals(const TB_Function* f, TB_Register intervals[]);
void tb_find_use_count(const TB_Function* f, int use_count[]);
TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count);

uint32_t tb_emit_const_patch(TB_Module* m, uint32_t func_id, size_t pos, const void* ptr, size_t len, size_t local_thread_id);
void tb_emit_global_patch(TB_Module* m, uint32_t func_id, size_t pos, TB_GlobalID global, size_t local_thread_id);
void tb_emit_call_patch(TB_Module* m, uint32_t func_id, uint32_t target_id, size_t pos, size_t local_thread_id);
void tb_emit_ecall_patch(TB_Module* m, uint32_t func_id, TB_ExternalID target_id, size_t pos, size_t local_thread_id);

#if !TB_STRIP_LABELS
void tb_emit_label_symbol(TB_Module* m, uint32_t func_id, uint32_t label_id, size_t pos);
#endif

TB_Register* tb_vla_reserve(TB_Function* f, size_t count);

// NOTE(NeGate): Place all the codegen interfaces down here
extern ICodeGen x64_fast_code_gen;
