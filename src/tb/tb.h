//  _______ _             ____             _                  _ 
// |__   __(_)           |  _ \           | |                | |
//    | |   _ _ __  _   _| |_) | __ _  ___| | _____ _ __   __| |
//    | |  | | '_ \| | | |  _ < / _` |/ __| |/ / _ \ '_ \ / _` |
//    | |  | | | | | |_| | |_) | (_| | (__|   <  __/ | | | (_| |
//    |_|  |_|_| |_|\__, |____/ \__,_|\___|_|\_\___|_| |_|\__,_|
//                   __/ |                                      
//                  |___/
// 
//    It's a relatively small compiler backend all behind a
//    simple C API! To get started: TODO
//
#ifndef _TINYBACKEND_H_
#define _TINYBACKEND_H_

// Windows likes it's secure functions, i kinda do too
// but only sometimes and this isn't one of them
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <inttypes.h>

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

#define TB_HOST_UNKNOWN 0
#define TB_HOST_X86_64 1

// While generating the IR, it's possible to
// perform some optimizations on the spot such
// as CSE and constant folding, if this define
// is 0, the CSE is disabled.
#define TB_FRONTEND_OPT 0

// If on, the labels aren't marked in the object file
// might save on performance at the cost of some assembly
// readability.
#define TB_STRIP_LABELS 0

#if defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
#define TB_HOST_ARCH TB_HOST_X86_64
#else
#define TB_HOST_ARCH TB_HOST_UNKNOWN
#endif

#define TB_API extern

#define TB_NULL_REG ((TB_Register)0)
#define TB_REG_MAX ((TB_Register)INT32_MAX)

typedef enum TB_ArithmaticBehavior {
	// No overflow will assume the value does not 
	// overflow and if it does this can be considered
	// undefined behavior with unknown consequences.
	TB_NO_WRAP,
    
	// Wrapping will allow the integer to safely wrap.
	TB_CAN_WRAP,
	
	// Overflow check will throw an error if the result
	// cannot be represented in the resulting type.
	TB_SIGNED_TRAP_ON_WRAP,
	TB_UNSIGNED_TRAP_ON_WRAP,
    
	// Saturated arithmatic will clamp the results in the
	// event of overflow/underflow.
	TB_SATURATED_UNSIGNED,
	TB_SATURATED_SIGNED
} TB_ArithmaticBehavior;

typedef enum TB_Arch {
	TB_ARCH_X86_64,
	TB_ARCH_AARCH64
} TB_Arch;

typedef enum TB_System {
	TB_SYSTEM_WINDOWS,
	TB_SYSTEM_LINUX,
    
    // TODO(NeGate): Actually implement these lol
	TB_SYSTEM_MACOS,
	TB_SYSTEM_ANDROID
} TB_System;

typedef enum TB_BranchHint {
	TB_BRANCH_HINT_NONE,
	TB_BRANCH_HINT_LIKELY,
	TB_BRANCH_HINT_UNLIKELY
} TB_BranchHint;

typedef enum TB_OptLevel {
	TB_OPT_O0,
    
	// DCE
	// CSE
	// MEM2REG
	TB_OPT_O1,
    
	// DCE
	// CSE
	// MEM2REG
	TB_OPT_SIZE,
    
	// DCE
	// CSE
	// MEM2REG
	// LOOP_UNROLL
	TB_OPT_SPEED,
} TB_OptLevel;

enum {
	TB_VOID,
	// Boolean
	TB_BOOL,
	// Integers
	TB_I8, TB_I16, TB_I32, TB_I64, TB_I128,
	// IEEE 754 Floating point
	TB_F32, TB_F64,
	// Pointers
	// NOTE(NeGate): consider support for multiple address spaces
	TB_PTR,
    
    TB_MAX_TYPES
};

#define TB_IS_INTEGER_TYPE(x) ((x) >= TB_I8 && (x) <= TB_I128)
#define TB_IS_FLOAT_TYPE(x) ((x) >= TB_F32 && (x) <= TB_F64)
#define TB_IS_POINTER_TYPE(x) ((x) == TB_PTR)

typedef struct TB_DataType {
	uint8_t type;
	uint8_t count; // 0 is illegal, except on VOID, it doesn't matter there
} TB_DataType;

typedef struct TB_Int128 {
	uint64_t lo;
	uint64_t hi;
} TB_Int128;

typedef struct TB_FeatureConstraints {
	int max_vector_width[TB_MAX_TYPES];
} TB_FeatureConstraints;

typedef struct TB_FeatureSet {
	struct {
		bool sse3 : 1;
        
		bool popcnt : 1;
		bool lzcnt : 1;
		bool sse41 : 1;
		bool sse42 : 1;
        
		bool clmul : 1;
		bool f16c : 1;
        
		bool bmi1 : 1;
		bool bmi2 : 1;
        
		bool avx : 1;
		bool avx2 : 1;
	} x64;
	struct {
		bool bf16 : 1;
	} aarch64;
} TB_FeatureSet;

typedef int TB_Label;

typedef struct TB_SwitchEntry {
	uint32_t key;
	TB_Label value;
} TB_SwitchEntry;

typedef int TB_FileID;
typedef struct TB_Module TB_Module;
typedef struct TB_Function TB_Function;
typedef int32_t TB_Register;
typedef struct TB_FunctionOutput TB_FunctionOutput;

// *******************************
// Public macros
// *******************************
#define TB_TYPE_VOID() (TB_DataType){ .type = TB_VOID }

#define TB_TYPE_I8(c) (TB_DataType){ .type = TB_I8, .count = c }
#define TB_TYPE_I16(c) (TB_DataType){ .type = TB_I16, .count = c }
#define TB_TYPE_I32(c) (TB_DataType){ .type = TB_I32, .count = c }
#define TB_TYPE_I64(c) (TB_DataType){ .type = TB_I64, .count = c }
#define TB_TYPE_I128(c) (TB_DataType){ .type = TB_I128, .count = c }

#define TB_TYPE_F32(c) (TB_DataType){ .type = TB_F32, .count = c }
#define TB_TYPE_F64(c) (TB_DataType){ .type = TB_F64, .count = c }

#define TB_TYPE_BOOL(c) (TB_DataType){ .type = TB_BOOL, .count = c }
#define TB_TYPE_PTR() (TB_DataType){ .type = TB_PTR, .count = 1 }

// *******************************
// Public functions
// *******************************
TB_API void tb_get_constraints(TB_Arch target_arch, const TB_FeatureSet* features, TB_FeatureConstraints* constraints);

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features, int optimization_level, int max_threads);
TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f);
TB_API size_t tb_DEBUG_module_get_full_node_count(TB_Module* m);
TB_API void tb_module_destroy(TB_Module* m);

TB_API bool tb_module_compile(TB_Module* m);
TB_API bool tb_module_export(TB_Module* m, FILE* f);
TB_API void tb_module_export_jit(TB_Module* m);

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name);
TB_API void* tb_module_get_jit_func_by_id(TB_Module* m, size_t i);
TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f);

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_DataType return_dt);
TB_API TB_FileID tb_register_file(TB_Module* m, const char* path);

TB_API TB_Label tb_get_current_label(TB_Function* f);
TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line);

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt);
TB_API TB_Register tb_inst_param_addr(TB_Function* f, TB_Register param);

TB_API TB_Register tb_inst_sxt(TB_Function* f, TB_Register src, TB_DataType dt);
TB_API TB_Register tb_inst_zxt(TB_Function* f, TB_Register src, TB_DataType dt);

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment);
TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment);
TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment);

TB_API TB_Register tb_inst_iconst(TB_Function* f, TB_DataType dt, uint64_t imm);
TB_API TB_Register tb_inst_iconst128(TB_Function* f, TB_DataType dt, TB_Int128 imm);

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm);

TB_API TB_Register tb_inst_array_access(TB_Function* f, TB_Register base, TB_Register index, uint32_t stride);
TB_API TB_Register tb_inst_member_access(TB_Function* f, TB_Register base, int32_t offset);
TB_API TB_Register tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Register* params);

TB_API void tb_inst_memset(TB_Function* f, TB_Register dst, TB_Register val, TB_Register size, int align);
TB_API void tb_inst_memcpy(TB_Function* f, TB_Register dst, TB_Register src, TB_Register size, int align);

TB_API TB_Register tb_inst_not(TB_Function* f, TB_DataType dt, TB_Register n);
TB_API TB_Register tb_inst_neg(TB_Function* f, TB_DataType dt, TB_Register n);

TB_API TB_Register tb_inst_and(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_or(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_xor(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_add(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_sub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_div(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness);

TB_API TB_Register tb_inst_shl(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_sar(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_shr(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_fadd(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_fsub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_fmul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_fdiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_eq(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_ne(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_slt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_sle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_sgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_sge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_ult(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_ule(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_ugt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_uge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_cmp_flt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_cmp_fge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

TB_API TB_Register tb_inst_phi2(TB_Function* f, TB_DataType dt, TB_Label a_label, TB_Register a, TB_Label b_label, TB_Register b);
TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id);
TB_API void tb_inst_goto(TB_Function* f, TB_Label id);
TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false);
TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Register key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries);
TB_API void tb_inst_ret(TB_Function* f, TB_DataType dt, TB_Register value);

TB_API void tb_function_print(TB_Function* f);

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
#ifdef TB_INTERNAL

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif

typedef struct TB_Emitter {
	size_t capacity, count;
	uint8_t* data;
} TB_Emitter;

// NOTE(NeGate): Don't reorder this stuff... bitch
typedef enum TB_RegTypeEnum {
    TB_NULL,
	
	// instructions with side-effects
	TB_LINE_INFO,
	TB_ICALL, /* internal use only, inline call */
	TB_CALL,
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
	
	// Immediates
	TB_INT_CONST,
    TB_FLOAT_CONST,
    TB_STRING_CONST,
    
    // Casts
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
    TB_CVT_INT2PTR,
    TB_CVT_PTR2INT,
    
    // Memory
    TB_LOCAL,
    TB_PARAM,
    TB_PARAM_ADDR,
	
	// Pointer math
	TB_MEMBER_ACCESS,
	TB_ARRAY_ACCESS,
    
    // PHI
    TB_PHI1,
    TB_PHI2,
	
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

#define TB_DATA_TYPE_EQUALS(a, b) (memcmp(&(a), &(b), sizeof(TB_DataType)) == 0)
#define TB_DATA_TYPE_NOT_EQUALS(a, b) (memcmp(&(a), &(b), sizeof(TB_DataType)) != 0)

typedef union TB_RegPayload {
	// NOTE(NeGate): Shouldn't exceed 128bits for any option
	uint32_t raw[4];
	
	TB_Int128 i_const;
	double f_const;
	TB_Register ext;
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
		uint32_t stride;
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
	} load;
	struct {
		TB_Register address;
		TB_Register value;
		uint32_t alignment;
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
		const TB_Function* target;
		int param_start, param_end;
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
} TB_RegPayload;

typedef struct TB_ConstPool32Patch {
	uint32_t func_id;
	uint32_t pos; // relative to the start of the function
	uint32_t raw_data;
} TB_ConstPool32Patch;

typedef struct TB_FunctionPatch {
	uint32_t func_id;
	uint32_t target_id;
	uint32_t pos; // relative to the start of the function
} TB_FunctionPatch;

struct TB_FunctionOutput {
	const char* name;
	
	// NOTE(NeGate): This data is actually specific to the
	// architecture run but generically can be thought of as
	// 64bits which keep track of which registers to save.
	uint64_t prologue_epilogue_metadata;
	uint64_t stack_usage;
	
	uint8_t* code;
	size_t code_size;
};

typedef struct TB_File {
	const char* path;
} TB_File;

typedef struct TB_NodeStream {
	TB_Register    capacity;
	TB_Register    count;
	
	TB_RegType*    type;
	TB_DataType*   dt;
	TB_RegPayload* payload;
} TB_NodeStream;

struct TB_Function {
	bool is_ir_free;
	bool validated;
	
	char* name;
	TB_DataType return_dt;
	
	// It's kinda a weird circular but still
	struct TB_Module* module;
	
	// Used by the IR building
	TB_Register current_label;
	TB_NodeStream nodes;
	
	// Used by nodes which have variable
	// length arguements like PHI and CALL.
	// SWITCH has arguments here too but they
	// are two slots each
	struct {
		TB_Register capacity;
		TB_Register count;
		TB_Register* data;
	} vla;
	
	uint32_t parameter_count;
    
	uint32_t locals_stack_usage;
	uint32_t parameter_stack_usage;
	uint32_t stack_usage;
};

typedef struct TB_LabelSymbol {
	uint32_t func_id;
	uint32_t label_id;
	uint32_t pos; // relative to the start of the function
} TB_LabelSymbol;

typedef struct CodegenThreadInfo {
	TB_Module* m;
	size_t id;
} CodegenThreadInfo;

typedef void(*CompileRoutine)(TB_Function* f);

#define MAX_JOBS_PER_JOB_SYSTEM 4096

typedef struct TB_JobSystem {
#if _WIN32
	CRITICAL_SECTION mutex;
#endif
	
	// FIFO queue
	_Atomic bool running;
	_Atomic uint32_t read_pointer; // Read
	_Atomic uint32_t write_pointer; // Write
	
	uint32_t thread_count;
	void* semaphore;
	void* threads[TB_MAX_THREADS];
	
	TB_Function* functions[MAX_JOBS_PER_JOB_SYSTEM];
} TB_JobSystem;

struct TB_Module {
	int optimization_level;
	
	TB_Arch target_arch;
	TB_System target_system;
	TB_FeatureSet features;
	TB_JobSystem* jobs;
	
	// This number is calculated while the builders are running
	// if the optimizations are run this number is set to SIZE_MAX
	// which means it needs to be re-evaluated.
	_Atomic size_t line_info_count;
	
#if !TB_STRIP_LABELS
	struct {
		size_t count;
		size_t capacity;
		TB_LabelSymbol* data;
	} label_symbols;
#endif
	
	struct {
		size_t count;
		size_t capacity;
		TB_ConstPool32Patch* data;
	} const32_patches;
    
	struct {
		size_t count;
		size_t capacity;
		TB_FunctionPatch* data;
	} call_patches;
    
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
		size_t count;
		TB_FunctionOutput* data;
	} compiled_functions;
	
	void* jit_region;
	size_t jit_region_size;
	
	// If not NULL, there's JITted functions in each 
	// non NULL entry which map to the `compiled_functions` 
	void** compiled_function_pos;
	
	// The code is stored into giant buffers
	// there's on per code gen thread so that
	// each can work at the same time without
	// making any allocations within the code
	// gen.
	_Atomic size_t code_region_count;
	struct {
		size_t size;
		uint8_t* data;
	} code_regions[TB_MAX_THREADS];
};

typedef enum TB_DataflowPattern {
	TB_DataflowPattern_Unknown,
	TB_DataflowPattern_IntConstant,
	TB_DataflowPattern_IntStep // y = mx + b
} TB_DataflowPattern;

typedef struct TB_RegisterDataflow {
	TB_DataflowPattern pattern;
	union {
		struct {
			uint64_t v;
		} iconst;
		struct {
			TB_Register loop_label;
			bool pre_iterator; // or post if false
			uint64_t m, b;
		} istep;
	};
} TB_RegisterDataflow;

typedef struct TB_TemporaryStorage {
	uint32_t used;
	uint8_t data[];
} TB_TemporaryStorage;

typedef struct ICodeGen {
	size_t(*get_prologue_length)(uint64_t saved, uint64_t stack_usage);
	size_t(*get_epilogue_length)(uint64_t saved, uint64_t stack_usage);
	size_t(*emit_prologue)(char out[64], uint64_t saved, uint64_t stack_usage);
	size_t(*emit_epilogue)(char out[64], uint64_t saved, uint64_t stack_usage);
	TB_FunctionOutput(*compile_function)(TB_Function* f, const TB_FeatureSet* features, uint8_t* out);
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

#define loop(iterator, count) for (typeof(count) iterator = 0, end__ = (count); iterator != end__; ++iterator)
#define loop_range(iterator, start, count) for (typeof(count) iterator = (start), end__ = (count); iterator != end__; ++iterator)
#define loop_range_step(iterator, start, count, step) for (typeof(count) iterator = (start), end__ = (count); iterator != end__; iterator += step)
#define loop_reverse(iterator, count) for (typeof(count) iterator = (count); iterator--;)

inline static uint64_t tb_next_pow2(uint64_t x) {
	return x == 1 ? 1 : 1 << (64 - __builtin_clzl(x - 1));
}

TB_TemporaryStorage* tb_tls_allocate();
// Allocates/Clears the temporary storage. It won't zero out the memory just mark the TLS as empty.

bool tb_validate(TB_Function* f);

void* tb_tls_push(TB_TemporaryStorage* store, size_t size);
// Reserves memory from the TLS and returns pointer to the beginning of the allocation.

void* tb_tls_pop(TB_TemporaryStorage* store, size_t size);
// Frees the top `size` bytes of the TLS.

void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance);
// Looks back `distance` bytes in the TLS.

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, FILE* f);
void tb_export_elf64(TB_Module* m, const ICodeGen* restrict code_gen, FILE* f);

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
#define CALL_NODE_PARAM_COUNT(f, i) \
((f)->nodes.payload[i].call.param_end - (f)->nodes.payload[i].call.param_start)

#if TB_HOST_ARCH == TB_HOST_X86_64
#define FOR_EACH_NODE(iterator, func, target, ...) \
for (size_t i = 0, cnt = (func)->nodes.count; i < cnt; i += 16) { \
__m128i bytes = _mm_load_si128((__m128i*)&(func)->nodes.type[i]); \
unsigned int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(target))); \
if (mask == 0) continue; \
/* this one is guarentee to not be zero */ \
size_t offset = __builtin_ffs(mask) - 1; \
size_t j = i + offset; \
/* skip over the mask bit for the next iteration */ \
mask >>= (offset + 1); \
/* We know it loops at least once by this point */ \
do { \
{ \
size_t iterator = j; \
__VA_ARGS__; \
} \
/* scan for next, if one exists */ \
size_t ffs = __builtin_ffs(mask); \
if (ffs == 0) break; \
/* skip over the mask bit for the next iteration */ \
mask >>= ffs; \
j += ffs; \
} while (true); \
}
#else
#define FOR_EACH_NODE(iterator, func, target, ...) \
for (size_t iterator = 0; iterator < (func)->nodes.count; iterator++) { \
if ((func)->nodes.type[iterator] == (target)) { \
__VA_ARGS__; \
} \
}
#endif

//
// OPTIMIZATION FUNCTIONS
//
bool tb_opt_mem2reg(TB_Function* f);
bool tb_opt_dce(TB_Function* f);
bool tb_opt_inline(TB_Function* f);
bool tb_opt_canonicalize(TB_Function* f);
bool tb_opt_remove_pass_node(TB_Function* f);
bool tb_opt_strength_reduction(TB_Function* f);
bool tb_opt_compact_dead_regs(TB_Function* f);

TB_API void tb_find_live_intervals(const TB_Function* f, TB_Register intervals[]);

//
// BACKEND UTILITIES
//
uint32_t tb_emit_const32_patch(TB_Module* m, uint32_t func_id, size_t pos, uint32_t data);
// TODO(NeGate): Not thread safe yet

void tb_emit_call_patch(TB_Module* m, uint32_t func_id, uint32_t target_id, size_t pos);
// TODO(NeGate): Not thread safe yet

#if !TB_STRIP_LABELS
void tb_emit_label_symbol(TB_Module* m, uint32_t func_id, uint32_t label_id, size_t pos);
// TODO(NeGate): Not thread safe yet
#endif

extern ICodeGen x64_fast_code_gen;

#endif /* TB_INTERNAL */
#endif /* _TINYBACKEND_H_ */
