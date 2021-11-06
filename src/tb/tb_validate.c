#define TB_INTERNAL
#include "tb.h"

int tb_validate(TB_Function* f) {
	if (f->validated) return 0;
	int error_count = 0;
	
	//tb_function_print(f);
	
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_SIGN_EXT ||
			f->nodes[i].type == TB_ZERO_EXT) {
			// Must all take in integral types
			if (!TB_IS_INTEGER_TYPE(f->nodes[i].dt.type)) {
				printf("error on %s:r%u: Integer extensions has to use an integral type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register src = f->nodes[i].ext;
			if (f->nodes[i].dt.type >= f->nodes[src].dt.type) {
				printf("error on %s:r%u: destination type must be bigger than the source type.\n", f->name, i);
				error_count++;
			}
		} else if (f->nodes[i].type >= TB_AND &&
				   f->nodes[i].type <= TB_SAR) {
			// Must all take in integral types
			if (!TB_IS_INTEGER_TYPE(f->nodes[i].dt.type)) {
				printf("error on %s:r%u: Integer arithmatic has to use an integral type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register a = f->nodes[i].i_arith.a;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes[i].dt, f->nodes[a].dt)) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register b = f->nodes[i].i_arith.b;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes[i].dt, f->nodes[b].dt)) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
		} else if (f->nodes[i].type >= TB_FADD &&
				   f->nodes[i].type <= TB_FDIV) {
			// Must all take in float types
			if (!TB_IS_FLOAT_TYPE(f->nodes[i].dt.type)) {
				printf("error on %s:r%u: Float arithmatic has to use a floating-point type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register a = f->nodes[i].f_arith.a;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes[i].dt, f->nodes[a].dt)) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register b = f->nodes[i].f_arith.b;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes[i].dt, f->nodes[b].dt)) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
		}
	}
	
	f->validated = true;
	return error_count;
}
