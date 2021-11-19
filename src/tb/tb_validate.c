#include "tb_internal.h"

bool tb_validate(TB_Function* f) {
	if (f->validated) return true;
	int error_count = 0;
	
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_SIGN_EXT ||
			f->nodes.type[i] == TB_ZERO_EXT) {
			// Must all take in integral types
			if (!TB_IS_INTEGER_TYPE(f->nodes.dt[i].type)) {
				printf("error on %s:r%u: Integer extensions has to use an integral type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register src = f->nodes.payload[i].ext;
			if (f->nodes.dt[i].type < f->nodes.dt[src].type) {
				printf("error on %s:r%u: destination type must be bigger than the source type.\n", f->name, i);
				error_count++;
			}
		} else if (f->nodes.type[i] >= TB_AND &&
				   f->nodes.type[i] <= TB_SAR) {
			// Must all take in integral types
			if (!TB_IS_INTEGER_TYPE(f->nodes.dt[i].type)) {
				printf("error on %s:r%u: Integer arithmatic has to use an integral type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register a = f->nodes.payload[i].i_arith.a;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes.dt[i], f->nodes.dt[a])) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register b = f->nodes.payload[i].i_arith.b;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes.dt[i], f->nodes.dt[b])) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
		} else if (f->nodes.type[i] >= TB_FADD &&
				   f->nodes.type[i] <= TB_FDIV) {
			// Must all take in float types
			if (!TB_IS_FLOAT_TYPE(f->nodes.dt[i].type)) {
				printf("error on %s:r%u: Float arithmatic has to use a floating-point type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register a = f->nodes.payload[i].f_arith.a;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes.dt[i], f->nodes.dt[a])) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register b = f->nodes.payload[i].f_arith.b;
			if (TB_DATA_TYPE_NOT_EQUALS(f->nodes.dt[i], f->nodes.dt[b])) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
		}
	}
	
	if (error_count) tb_function_print(f, stdout);
	f->validated = true;
	return (error_count == 0);
}
