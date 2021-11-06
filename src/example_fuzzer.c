#include "tb/tb.h"
#include <x86intrin.h>

static uint32_t seed;

static uint32_t gen_random(uint32_t min, uint32_t max) {
	uint32_t v = seed = _mm_crc32_u32(0, seed);
	
	return min + (v % (max - min));
}

static TB_DataType gen_random_dt() {
	return (TB_DataType){
		.type = gen_random(0, TB_MAX_TYPES),
		.count = 1
	};
}

static TB_DataType gen_random_int_dt() {
	return (TB_DataType){
		.type = gen_random(TB_I32, TB_I64 + 1), // ignores i128
		.count = 1
	};
}

static TB_ArithmaticBehavior gen_random_arith() {
	return gen_random(0, TB_SATURATED_SIGNED); // it's ignoring sat_signed for now
}

static int pool_size = 0;
static TB_Register pool[512];

int main(int argc, char** argv) {
	int trial_count;
	if (argc == 1) {
		printf("Defaulting to 10 trials\n");
		trial_count = 10;
	} else {
		trial_count = atoi(argv[1]);
	}
	
	clock_t t1 = clock();
	TB_FeatureSet features = { 0 };
	
	int n = 0;
	TB_Module* m = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
	while (n < trial_count) {
		seed = _rdtsc();
		pool_size = 0;
		
		static char temp[64];
		sprintf_s(temp, 64, "trial_%d", n);
		
		TB_DataType dt = gen_random_int_dt();
		TB_Function* f = tb_function_create(m, temp, dt);
		
		int param_count = gen_random(1, 8);
		for (int i = 0; i < param_count; i++) {
			pool[pool_size] = tb_inst_param(f, dt);
			pool_size += 1;
		}
		
		int inst_count = gen_random(1, 50);
		for (int i = 0; i < inst_count; i++) {
			int rng = gen_random(0, 4);
			if (pool_size < 4) rng = 0;
			
			switch (rng) {
				case 0: 
				pool[pool_size] = tb_inst_iconst(f, dt, gen_random(0, 10000));
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
				pool[pool_size] = tb_inst_mul(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], gen_random_arith());
				pool_size += 1;
				break;
				/*case 4: 
				pool[pool_size] = tb_inst_div(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], false);
				pool_size += 1;
				break;
				case 5: 
				pool[pool_size] = tb_inst_div(f, dt, pool[gen_random(0, pool_size)], pool[gen_random(0, pool_size)], true);
				pool_size += 1;
				break;*/
			}
		}
		
		// try to use later values for return
		tb_inst_ret(f, dt, pool[gen_random(pool_size / 2, pool_size)]);
		tb_function_print(f);
		n++;
	}
	
	if (!tb_module_compile(m, TB_OPT_O0, 1)) abort();
	
	FILE* file = fopen("./test_x64.obj", "wb");
	if (!tb_module_export(m, file)) abort();
	fclose(file);
	
	tb_module_destroy(m);
	
	clock_t t2 = clock();
	double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("compilation took %f ms\n", delta_ms);
	return 0;
}
