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
//    simple C API! To get started:
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

#ifndef TB_MAX_THREADS
#define TB_MAX_THREADS 1
#endif

// Per-thread
#ifndef TB_TEMPORARY_STORAGE_SIZE
#define TB_TEMPORARY_STORAGE_SIZE (1 << 22)
#endif

#ifndef TB_MAX_FUNCTIONS
#define TB_MAX_FUNCTIONS (1 << 16)
#endif

#define TB_API extern

#define TB_NULL_REG ((TB_Register)0)

typedef enum TB_ArithmaticBehavior {
	// No overflow will assume the value does not 
	// overflow and if it does this can be considered
	// undefined behavior with unknown consequences.
	TB_NO_WRAP,
    
	// Overflow check will throw an error if the result
	// cannot be represented in the resulting type.
	TB_WRAP_CHECK,
    
	// Wrapping will allow the integer to safely wrap.
	TB_CAN_WRAP,
    
	// Saturated arithmatic will clamp the results in the
	// event of overflow/underflow.
	TB_SATURATED_SIGNED,
	TB_SATURATED_UNSIGNED
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
	// Floating point
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
		bool omit_frame_pointer : 1;
        
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

typedef struct TB_Module TB_Module;
typedef struct TB_Function TB_Function;
typedef size_t TB_Register;
typedef struct TB_FunctionOutput TB_FunctionOutput;

// *******************************
// Public macros
// *******************************
#define TB_TYPE_VOID(c) (TB_DataType){ .type = TB_VOID }

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

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features);
TB_API void tb_module_destroy(TB_Module* m);

TB_API void tb_module_compile(TB_Module* m, int optimization_level, int max_threads);
TB_API void tb_module_export(TB_Module* m, FILE* f);

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name);

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt);
TB_API TB_Register tb_inst_param_addr(TB_Function* f, TB_Register param);

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment);
TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment);
TB_API TB_Register tb_inst_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment);

TB_API TB_Register tb_inst_iconst(TB_Function* f, TB_DataType dt, uint64_t imm);
TB_API TB_Register tb_inst_iconst128(TB_Function* f, TB_DataType dt, TB_Int128 imm);

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm);

TB_API TB_Register tb_inst_add(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_sub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Register tb_inst_udiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);
TB_API TB_Register tb_inst_sdiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b);

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

TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id);
TB_API TB_Register tb_inst_goto(TB_Function* f, TB_Label id);
TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false);
TB_API TB_Register tb_inst_ret(TB_Function* f, TB_DataType dt, TB_Register value);

TB_API void tb_function_print(TB_Function* f);
TB_API void tb_function_optimize(TB_Function* f);
TB_API void tb_find_live_intervals(size_t intervals[], const TB_Function* f);

// These emulator functions have the same semantics as
// their equivalent IR operations.
TB_API TB_Int128 tb_emulate_add(TB_Function* f, TB_ArithmaticBehavior arith_behavior, TB_DataType dt, TB_Int128 a, TB_Int128 b);

// Private header stuff, don't include TB_INTERNAL into your code,
// it's for the other implementation files of TB
#ifdef TB_INTERNAL

typedef struct TB_Emitter {
	size_t capacity, count;
	uint8_t* data;
} TB_Emitter;

struct TB_FunctionOutput {
	const char* name;
	bool has_no_prologue;
	TB_Emitter emitter;
};

typedef struct TB_Node {
	enum TB_RegisterType {
		TB_NULL,
        
		// Immediates
		TB_INT_CONST,
		TB_FLOAT_CONST,
		TB_STRING_CONST,
        
		// Integer arithmatic
		TB_ADD,
		TB_SUB,
		TB_MUL,
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
		TB_LOAD,
		TB_STORE,
        
		TB_LOCAL,
		TB_PARAM,
		TB_PARAM_ADDR,
        
		// Control flow
        // NOTE(NeGate): only used internally, if you
        // see one in normal IR things went wrong in
        // an optimization pass
		TB_PASS,
		TB_PHI1,
		TB_PHI2,
        
		TB_LABEL,
		TB_GOTO,
		TB_IF,
		TB_RET
	} type;
	TB_DataType dt;
	union {
		TB_Int128 i_const;
		double f_const;
		struct {
			uint32_t id;
			uint32_t size;
		} param;
		struct {
			TB_Register param;
            
			uint32_t position;
			uint32_t size;
			uint32_t alignment;
		} param_addr;
		struct {
			// NOTE(NeGate): The position value provided here are only hints, they can be ignored.
			uint32_t position;
			uint32_t size;
			uint32_t alignment;
		} local;
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
	};
} TB_Node;

struct TB_Function {
	char* name;
    
	size_t capacity;
	size_t count;
	TB_Node* nodes;
    
	TB_Register current_label;
    
	uint32_t parameter_count;
    
	uint32_t locals_stack_usage;
	uint32_t parameter_stack_usage;
	uint32_t stack_usage;
};

struct TB_Module {
	TB_Arch target_arch;
	TB_System target_system;
	TB_FeatureSet features;
    
	struct {
		size_t count;
		TB_Function* data;
	} functions;
    
	struct {
		size_t functions_compiled;
		size_t count;
		TB_FunctionOutput* data;
	} compiled_functions;
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

// Used internally for some optimizations
#define tb_swap(a, b) do { \
typeof(a) temp = a; \
a = b; \
b = temp; \
} while(0)

#define loop(iterator, count) for (size_t iterator = 0, end__ = (count); iterator != end__; ++iterator)
#define loop_range(iterator, start, count) for (size_t iterator = (start), end__ = (count); iterator != end__; ++iterator)
#define loop_range_step(iterator, start, count, step) for (size_t iterator = (start), end__ = (count); iterator != end__; iterator += step)
#define loop_reverse(iterator, count) for (size_t iterator = (count); iterator--;)

inline static uint64_t tb_next_pow2(uint64_t x) {
	return x == 1 ? 1 : 1 << (64 - __builtin_clzl(x - 1));
}

TB_TemporaryStorage* tb_tls_allocate();
// Allocates/Clears the temporary storage. It won't zero out the memory just mark the TLS as empty.

void* tb_tls_push(TB_TemporaryStorage* store, size_t size);
// Reserves memory from the TLS and returns pointer to the beginning of the allocation.

void* tb_tls_pop(TB_TemporaryStorage* store, size_t size);
// Frees the top `size` bytes of the TLS.

void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance);
// Looks back `distance` bytes in the TLS.

void tb_export_coff(TB_Module* m, TB_Arch arch, FILE* f);
// Writes out COFF object file with the compiled functions

void tb_export_elf64(TB_Module* m, TB_Arch arch, FILE* f);
// Writes out ELF object file with the compiled functions

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
bool tb_can_reach(TB_Function* f, TB_Register label, TB_Register end);
TB_RegisterDataflow tb_analyze_register_dataflow(TB_Function* f, TB_Register label_reg, TB_Register reg);
size_t tb_loop_analysis(TB_TemporaryStorage* tls, TB_Function* f);

//
// OPTIMIZATION FUNCTIONS
//
bool tb_opt_loop_unroll(TB_Function* f);
bool tb_opt_mem2reg(TB_Function* f);
bool tb_opt_phi_cleanup(TB_Function* f);
bool tb_opt_cse(TB_Function* f);
bool tb_opt_dce(TB_Function* f);

TB_FunctionOutput aarch64_compile_function(TB_Function* f, const TB_FeatureSet* features);
// Machine code converter for Aarch64

TB_FunctionOutput x64_compile_function(TB_Function* f, const TB_FeatureSet* features);
// Machine code converter for x64 built for fast compilation

#endif /* TB_INTERNAL */
#endif /* _TINYBACKEND_H_ */
