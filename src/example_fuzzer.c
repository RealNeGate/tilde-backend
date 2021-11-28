#include "tb/tb.h"
#include <x86intrin.h>
#include <windows.h>

#define TRIAL_COUNT 50000 // 349525

static uint32_t gen_random_any();
static uint32_t gen_random(uint32_t min, uint32_t max);
static uint32_t gen_random_bool();
static TB_DataType gen_random_dt();
static TB_DataType gen_random_int_dt();
static TB_ArithmaticBehavior gen_random_arith();

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
static _Thread_local struct { uint64_t state;  uint64_t inc; } rng;

static _Thread_local TB_Register pool[512];
static _Thread_local TB_Register var_pool[512];

static TB_Module* m;

static TB_Register cast_into(TB_Function* f, TB_Register in, TB_DataType to) {
	TB_DataType dt = tb_node_get_data_type(f, in);
	
	if (dt.type < to.type) return tb_inst_zxt(f, in, to);
	else if (dt.type > to.type) return tb_inst_trunc(f, in, to);
	
	return in;
}

static __stdcall int ir_gen(FuzzerInfo* i) {
	rng.state = i->initial_state;
	rng.inc = i->initial_inc;
	
	size_t n = 0;
	while (n < TRIAL_COUNT) {
		int pool_size = 0;
		int var_pool_size = 0;
		
		char temp[64];
		sprintf_s(temp, 64, "trial_%d_%d", i->thread_id, n);
		
		TB_DataType return_dt = gen_random_int_dt();
		TB_Function* f = tb_function_create(m, temp, return_dt);
		
		int param_count = gen_random(1, 8);
		for (int i = 0; i < param_count; i++) {
			pool[pool_size++] = tb_inst_param(f, return_dt);
		}
		
		int inst_count = gen_random(1, 400);
		for (int i = 0; i < inst_count; i++) {
			TB_DataType dt = gen_random_int_dt();
			
			if (var_pool_size > 0) {
				TB_Register a = pool[gen_random(0, pool_size)];
				TB_Register b = pool[gen_random(0, pool_size)];
				
				a = cast_into(f, a, dt);
				
				int rng = gen_random(0, 10);
				if (rng > 0) {
					// only needed for 1-6
					if (rng < 8) b = cast_into(f, b, dt);
					
					TB_Register dst = TB_NULL_REG;
					if (rng == 1) {
						dst = tb_inst_add(f, dt, a, b, gen_random_arith());
					} else if (rng == 2) {
						dst = tb_inst_sub(f, dt, a, b, gen_random_arith());
					} else if (rng == 3) {
						dst = tb_inst_mul(f, dt, a, b, gen_random_arith());
					} else if (rng == 4) {
						dst = tb_inst_div(f, dt, a, b, gen_random_bool());
					} else if (rng == 5) {
						dst = tb_inst_and(f, dt, a, b);
					} else if (rng == 6) {
						dst = tb_inst_or(f, dt, a, b);
					} else if (rng == 7) {
						dst = tb_inst_xor(f, dt, a, b);
					} else if (rng == 8) {
						dst = tb_inst_neg(f, dt, a);
					} else if (rng == 9) {
						dst = tb_inst_not(f, dt, a);
					} 
					
					pool[pool_size++] = dst;
					continue;
				}
			}
			
			int rng = gen_random(0, 4);
			if (rng == 0) {
				pool[pool_size++] = tb_inst_iconst(f, dt, gen_random_any());
			} else if (rng == 1 && var_pool_size > 0) {
				pool[pool_size++] = tb_inst_load(f, dt, var_pool[gen_random(0, var_pool_size)], dt.type == TB_I32 ? 4 : 8);
			} else if (rng == 2 && var_pool_size > 0) {
				TB_Register addr = var_pool[gen_random(0, var_pool_size)];
				
				int size, align;
				tb_get_function_get_local_info(f, addr, &size, &align);
				
				// just use the type of the allocation
				dt = align == 8 ? TB_TYPE_I64(1) : TB_TYPE_I32(1);
				
				tb_inst_store(f, dt, addr, pool[gen_random(0, pool_size)], align);
			} else if (rng == 3) {
				var_pool[var_pool_size++] = tb_inst_local(f, dt.type == TB_I64 ? 8 : 4, dt.type == TB_I64 ? 8 : 4);
			}
		}
		
		// try to use later values for return
		TB_Register ret = pool[pool_size - 1];
		tb_inst_ret(f, return_dt, cast_into(f, ret, return_dt));
		
		//tb_function_print(f, stdout);
		//printf("\n\n\n");
		
		tb_module_compile_func(m, f);
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
	
	m = tb_module_create(TB_ARCH_X86_64,
						 TB_SYSTEM_WINDOWS, &features,
                         TB_OPT_O0, thread_count,
                         false);
	
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
	printf("IR gen took %f ms\n", ((t2 - t1) * freq) * 1000.0);
	
	if (!tb_module_compile(m)) abort();
	
	uint64_t t3 = get_timer_counter();
	printf("Machine code gen took %f ms\n", ((t3 - t2) * freq) * 1000.0);
	
	FILE* file = fopen("./test_x64.obj", "wb");
	if (!tb_module_export(m, file)) abort();
	fclose(file);
	
	uint64_t t4 = get_timer_counter();
	printf("Object file output took %f ms\n", ((t4 - t3) * freq) * 1000.0);
	
	double compile_time = ((t4 - t1) * freq);
	printf("=========================================\n"
		   "Compilation took %f ms\n", compile_time * 1000.0);
	
	size_t node_count = tb_DEBUG_module_get_full_node_count(m);
	double speed = ((double)node_count / compile_time) / 1000000.0;
	
	printf("Node count: %zu\n", node_count);
	printf("  %f million nodes/second\n", speed);
	
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
		.count = 1
	};
}

static TB_DataType gen_random_int_dt() {
	return (TB_DataType){
		.type = (gen_random_any() & 1) ? TB_I64 : TB_I32, // ignores i128
		.count = 1
	};
}

static TB_ArithmaticBehavior gen_random_arith() {
	return gen_random(0, TB_SATURATED_SIGNED); // it's ignoring sat_signed for now
}
