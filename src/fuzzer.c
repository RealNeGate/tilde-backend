#include "tb/tb.h"
#include <emmintrin.h>
#include <windows.h>

#ifdef _MSC_VER
#define thread_local __declspec(thread)
#define __builtin_bswap64(x) _byteswap_uint64(x)
#else
#define thread_local _Thread_local
#endif

#define TRIAL_COUNT 349525 // for max power 349525 

static uint32_t gen_random_any();
static uint32_t gen_random(uint32_t min, uint32_t max);
static uint32_t gen_random_bool();
static TB_DataType gen_random_dt();
static TB_DataType gen_random_int_dt();
static TB_ArithmaticBehavior gen_random_arith();
static int get_dt_size(TB_DataType dt) {
	switch (dt.type) {
		case TB_I8: return 1;
		case TB_I16: return 2;
		case TB_I32: return 4;
		case TB_I64: return 8;
		default: __assume(0);
	}
}

static uint64_t get_timer_counter() {
    LARGE_INTEGER t;
    QueryPerformanceCounter(&t);
    return t.QuadPart;
}

static double get_timer_frequency() {
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    return 1.0 / (double)freq.QuadPart;
}

// NOTE(NeGate): Maybe performance wise it's not nice to put so many big things
// into the thread local storage.
typedef struct FuzzerInfo {
	// Which functions should it generate
	int thread_id;
	uint64_t initial_state, initial_inc;
} FuzzerInfo;

// https://www.pcg-random.org/download.html#minimal-c-implementation
static thread_local struct { uint64_t state;  uint64_t inc; } rng;

static thread_local TB_Reg pool[512];
static thread_local TB_Reg var_pool[512];

static TB_Module* m;

static TB_Reg cast_into(TB_Function* f, TB_Reg in, TB_DataType to) {
	TB_DataType dt = tb_function_get_node(f, in)->dt;
	
	if (dt.type < to.type) return tb_inst_zxt(f, in, to);
	else if (dt.type > to.type) return tb_inst_trunc(f, in, to);
	
	return in;
}

static int ir_gen(FuzzerInfo* i) {
	rng.state = i->initial_state;
	rng.inc = i->initial_inc;
	
	size_t n = 0;
	while (n < TRIAL_COUNT) {
		int pool_size = 0;
		int var_pool_size = 0;
		
		TB_DataType return_dt = gen_random_int_dt();
		int param_count = gen_random(1, 8);
		
		TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, return_dt, param_count, false);
		for (int i = 0; i < param_count; i++) tb_prototype_add_param(p, gen_random_int_dt());
		
		char temp[64];
		sprintf_s(temp, 64, "trial_%d_%zu", i->thread_id, n);
		TB_Function* f = tb_prototype_build(m, p, temp, TB_LINKAGE_PUBLIC);
		for (int i = 0; i < param_count; i++) pool[pool_size++] = tb_inst_param(f, i);
		
		int inst_count = gen_random(1, 50);
		for (int i = 0; i < inst_count; i++) {
			TB_DataType dt = gen_random_int_dt();
			
			if (var_pool_size > 0) {
				int start_search = pool_size > 5 ? pool_size - 5 : 0;
				
				TB_Reg a = pool[gen_random(start_search, pool_size)];
				TB_Reg b = pool[gen_random(start_search, pool_size)];
				
				a = cast_into(f, a, dt);
				
				int rng = gen_random(0, 10);
				if (rng > 0) {
					// only needed for 1-6
					if (rng < 8) b = cast_into(f, b, dt);
					
					TB_Reg dst = TB_NULL_REG;
					if (rng == 1) {
						dst = tb_inst_add(f, a, b, gen_random_arith());
					} else if (rng == 2) {
						dst = tb_inst_sub(f, a, b, gen_random_arith());
					} else if (rng == 3) {
						dst = tb_inst_mul(f, a, b, gen_random_arith());
					} else if (rng == 4) {
						dst = tb_inst_div(f, a, b, gen_random_bool());
					} else if (rng == 5) {
						dst = tb_inst_and(f, a, b);
					} else if (rng == 6) {
						dst = tb_inst_or(f, a, b);
					} else if (rng == 7) {
						dst = tb_inst_xor(f, a, b);
					} else if (rng == 8) {
						dst = tb_inst_neg(f, a);
					} else if (rng == 9) {
						dst = tb_inst_not(f, a);
					} 
					
					pool[pool_size++] = dst;
					continue;
				}
			}
			
			int rng = gen_random(0, 4);
			if (rng == 0) {
				pool[pool_size++] = tb_inst_sint(f, dt, gen_random_any() & 0xFFFFull);
			} else if (rng == 1 && var_pool_size > 0) {
				pool[pool_size++] = tb_inst_load(f, dt, var_pool[gen_random(0, var_pool_size)], dt.type == TB_I32 ? 4 : 8);
			} else if (rng == 2 && var_pool_size > 0) {
				TB_Reg addr = var_pool[gen_random(0, var_pool_size)];
				
				int size, align;
				tb_get_function_get_local_info(f, addr, &size, &align);
				
				// just use the type of the allocation
				switch (size) {
					case 1: dt = TB_TYPE_I8; break;
					case 2: dt = TB_TYPE_I16; break;
					case 4: dt = TB_TYPE_I32; break;
					case 8: dt = TB_TYPE_I64; break;
					default: __assume(0);
				}
				
				tb_inst_store(f, dt, addr, pool[gen_random(0, pool_size)], align);
			} else if (rng == 3) {
				int size;
				switch (dt.type) {
					case TB_I8: size = 1; break;
					case TB_I16: size = 2;  break;
					case TB_I32: size = 4; break;
					case TB_I64: size = 8; break;
					default: __assume(0);
				}
				
				TB_Reg addr = tb_inst_local(f, size, size);
				var_pool[var_pool_size++] = addr;
				
				tb_inst_store(f, dt, addr, pool[gen_random(0, pool_size)], size);
			}
		}
		
		// try to use later values for return
		TB_Reg ret = pool[gen_random(pool_size / 2, pool_size)];
		tb_inst_ret(f, cast_into(f, ret, return_dt));
		
		//tb_function_print(f, tb_default_print_callback, stdout);
		//printf("\n\n\n");
		
		tb_module_compile_func(m, f);
		tb_function_free(f);
		n++;
	}
	
	return 0;
}

int main(int argc, char** argv) {
	int thread_count = 1;
	if (argc > 1) {
		thread_count = atoi(argv[1]);
	}
	
	rng.state = __rdtsc();
	printf("Executing %d x %d (%d) trials\n", thread_count, TRIAL_COUNT, thread_count * TRIAL_COUNT);
	
	double freq = get_timer_frequency();
	uint64_t t1 = get_timer_counter();
	
	TB_FeatureSet features = { 0 };
	m = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
	
	rng.inc = __builtin_bswap64(__rdtsc()) >> 7;
	printf("Initial seed: 0x%llx 0x%llx\n", rng.state, rng.inc);
	
	{
		FuzzerInfo info[TB_MAX_THREADS];
		HANDLE threads[TB_MAX_THREADS];
		for (size_t i = 0; i < thread_count; i++) {
			info[i] = (FuzzerInfo){ 0 };
			info[i].thread_id = i;
			info[i].initial_state = (((uint64_t)gen_random_any()) << 32ull) | ((uint64_t)gen_random_any());
			info[i].initial_inc = (((uint64_t)gen_random_any()) << 32ull) | ((uint64_t)gen_random_any());
			
			//printf("Thread seed: 0x%llx 0x%llx\n", info[i].initial_state, info[i].initial_inc);
		}
		
		for (size_t i = 0; i < thread_count; i++) {
			threads[i] = CreateThread(NULL, 2 << 21, (LPTHREAD_START_ROUTINE)ir_gen, &info[i], 0, 0);
		}
		
		WaitForMultipleObjects(thread_count, threads, TRUE, -1);
		for (size_t i = 0; i < thread_count; i++) CloseHandle(threads[i]);
	}
	
	uint64_t t2 = get_timer_counter();
	double ir_gen_time = (t2 - t1) * freq;
	printf("IR generation took %f ms\n", ir_gen_time * 1000.0);
	
	if (!tb_module_export(m, "./test_x64.obj", false)) abort();
	
	uint64_t t3 = get_timer_counter();
	printf("Object file output took %f ms\n", ((t3 - t2) * freq) * 1000.0);
	
	double compile_time = ((t3 - t1) * freq);
	printf("=========================================\n"
		   "Compilation took %f ms\n", compile_time * 1000.0);
	
	size_t node_count = tb_DEBUG_module_get_full_node_count(m);
	printf("Node count: %zu\n", node_count);
	
	double speed = ((double)node_count / ir_gen_time) / 1000000.0;
	printf("  %f million nodes/second (IR gen time)\n", speed);
	
	speed = ((double)node_count / compile_time) / 1000000.0;
	printf("  %f million nodes/second (Full)\n", speed);
	
	tb_module_destroy(m);
	return 0;
}

// https://www.pcg-random.org/download.html#minimal-c-implementation
// *Really* minimal PCG32 code / (c) 2014 M.E. O'Neill / pcg-random.org
// Licensed under Apache License 2.0 (NO WARRANTY, etc. see website)
static uint32_t gen_random_any() {
    uint64_t oldstate = rng.state;
    // Advance internal state
    rng.state = oldstate * 6364136223846793005ULL + (rng.inc|1);
    // Calculate output function (XSH RR), uses old state for max ILP
    uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
    uint32_t rot = oldstate >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

static uint32_t gen_random(uint32_t min, uint32_t max) {
	return min + (gen_random_any() % (max - min));
}

static uint32_t gen_random_bool() {
	return gen_random_any() & 1;
}

static TB_DataType gen_random_dt() {
	return (TB_DataType){
		.type = (gen_random_any() % TB_MAX_TYPES),
		.width = 0
	};
}

static TB_DataType gen_random_int_dt() {
	return (TB_DataType){
		.type = TB_I8 + (gen_random_any() % 4),
		.width = 0
	};
}

static TB_ArithmaticBehavior gen_random_arith() {
	return gen_random(0, TB_SATURATED_SIGNED); // it's ignoring sat_signed for now
}