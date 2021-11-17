#include "tb/tb.h"
#include <x86intrin.h>
#include <windows.h>

#define TRIAL_COUNT 100000

static uint32_t gen_random_any();
static uint32_t gen_random(uint32_t min, uint32_t max);
static uint32_t gen_random_bool();
static TB_DataType gen_random_dt();
static TB_DataType gen_random_int_dt();
static TB_ArithmaticBehavior gen_random_arith();

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

static __stdcall int ir_gen(FuzzerInfo* i) {
	rng.state = i->initial_state;
	rng.inc = i->initial_inc;
	
	size_t n = 0;
	while (n < TRIAL_COUNT) {
		int pool_size = 0;
		int var_pool_size = 0;
		
		char temp[64];
		sprintf_s(temp, 64, "trial_%d_%d", i->thread_id, n);
		
		TB_DataType dt = gen_random_int_dt();
		TB_Function* f = tb_function_create(m, temp, dt);
		
		int param_count = gen_random(1, 8);
		for (int i = 0; i < param_count; i++) {
			pool[pool_size] = tb_inst_param(f, dt);
			pool_size += 1;
		}
		
		int inst_count = gen_random(1, 500);
		for (int i = 0; i < inst_count; i++) {
			int rng = gen_random(0, 14);
			if (pool_size < 4) rng = 0;
			if (rng >= 11 && var_pool_size == 0) rng = 0;
			
			switch (rng) {
				case 0: 
				pool[pool_size] = tb_inst_iconst(f, dt, gen_random_any());
				pool_size += 1;
				break;
				case 1: 
				pool[pool_size] = tb_inst_add(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], gen_random_arith());
				pool_size += 1;
				break;
				case 2: 
				pool[pool_size] = tb_inst_sub(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], gen_random_arith());
				pool_size += 1;
				break;
				case 3: 
				case 4: 
				pool[pool_size] = tb_inst_mul(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], gen_random_arith());
				pool_size += 1;
				break;
				case 5:
				pool[pool_size] = tb_inst_div(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], gen_random_bool());
				pool_size += 1;
				break;
				break;
				case 6:
				pool[pool_size] = tb_inst_and(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)]);
				pool_size += 1;
				break;
				case 7:
				pool[pool_size] = tb_inst_and(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)]);
				pool_size += 1;
				break;
				case 8:
				pool[pool_size] = tb_inst_or(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)]);
				pool_size += 1;
				break;
				case 9:
				pool[pool_size] = tb_inst_not(f, dt, pool[gen_random(0, pool_size)]);
				pool_size += 1;
				break;
				case 10:
				pool[pool_size] = tb_inst_neg(f, dt, pool[gen_random(0, pool_size)]);
				pool_size += 1;
				break;
				case 11: 
				var_pool[var_pool_size] = tb_inst_local(f, dt.type == TB_I32 ? 4 : 8, dt.type == TB_I32 ? 4 : 8);
				
				tb_inst_store(f, dt, var_pool[var_pool_size], pool[gen_random(0, pool_size)], dt.type == TB_I32 ? 4 : 8);
				var_pool_size += 1;
				break;
				case 12: 
				pool[pool_size] = tb_inst_load(f, dt, var_pool[gen_random(0, var_pool_size)], dt.type == TB_I32 ? 4 : 8);
				pool_size += 1;
				break;
				case 13: 
				tb_inst_store(f, dt, var_pool[gen_random(0, var_pool_size)], pool[gen_random(0, pool_size)], dt.type == TB_I32 ? 4 : 8);
				break;
			}
		}
		
		// try to use later values for return
		tb_inst_ret(f, dt, pool[gen_random(pool_size / 2, pool_size)]);
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
	printf("Executing %d x 50,000 (%d) trials\n", thread_count, thread_count * TRIAL_COUNT);
	
	clock_t t1 = clock();
	TB_FeatureSet features = { 0 };
	
	m = tb_module_create(TB_ARCH_X86_64,
						 TB_SYSTEM_WINDOWS, &features,
						 TB_OPT_O0, thread_count);
	
	rng.inc = __builtin_bswap64(__rdtsc()) >> 7;
	printf("Initial seed: 0x%llx 0x%llx\n", rng.state, rng.inc);
	
	{
		FuzzerInfo info[TB_MAX_THREADS];
		HANDLE threads[TB_MAX_THREADS];
		for (size_t i = 0; i < thread_count; i++) {
			info[i] = (FuzzerInfo){ 0 };
			info[i].thread_id = i;
			info[i].initial_state = (((uint64_t)gen_random_any()) << 32ull) 
				| ((uint64_t)gen_random_any());
			info[i].initial_inc = (((uint64_t)gen_random_any()) << 32ull) 
				| ((uint64_t)gen_random_any());
			
			//printf("Thread seed: 0x%llx 0x%llx\n", info[i].initial_state, info[i].initial_inc);
		}
		
		for (size_t i = 0; i < thread_count; i++) {
			threads[i] = CreateThread(NULL, 2 << 21, (LPTHREAD_START_ROUTINE)ir_gen, &info[i], 0, 0);
		}
		
		WaitForMultipleObjects(thread_count, threads, TRUE, -1);
		for (size_t i = 0; i < thread_count; i++) CloseHandle(threads[i]);
	}
	
	clock_t t2 = clock();
	printf("IR gen took %f ms\n", ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0);
	
	if (!tb_module_compile(m)) abort();
	
	clock_t t3 = clock();
	printf("Machine code gen took %f ms\n", ((t3 - t2) / (double)CLOCKS_PER_SEC) * 1000.0);
	
	FILE* file = fopen("./test_x64.obj", "wb");
	if (!tb_module_export(m, file)) abort();
	fclose(file);
	
	clock_t t4 = clock();
	printf("Object file output took %f ms\n", ((t4 - t3) / (double)CLOCKS_PER_SEC) * 1000.0);
	
	double compile_time = ((t4 - t1) / (double)CLOCKS_PER_SEC);
	printf("=========================================\n"
		   "Compilation took %f ms\n", ((t4 - t1) / (double)CLOCKS_PER_SEC) * 1000.0);
	
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
