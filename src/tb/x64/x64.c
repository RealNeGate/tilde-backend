// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces. Because it's a
// separate TU completely from the rest of the project, it means that we can use
// tiny function names so long as they don't internally collide since they're static.
#define USING_FAST_PATH (0)

#include "x64.h"
#include "x64_emitter.h"
#include "x64_proepi.h"
#include "x64_fast.h"

#include "x64_complex.h"

#if 0
#define DEBUG_LOG(...) printf(__VA_ARGS__)
#else
#define DEBUG_LOG(...) ((void)0)
#endif

void x64_emit_call_patches(TB_Module* m, uint32_t* func_layout) {
    loop(i, m->max_threads) {
        TB_FunctionPatch* patches = m->call_patches[i];

        loop(j, arrlen(patches)) {
            TB_FunctionPatch*  p     = &patches[j];
            TB_FunctionOutput* out_f = p->source->output;
			assert(out_f && "Patch cannot be applied to function with no compiled output");

            uint64_t meta        = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            uint8_t* code        = out_f->code;

            // x64 thinks of relative addresses as being relative
            // to the end of the instruction or in this case just
            // 4 bytes ahead hence the +4.
            size_t actual_pos = func_layout[p->source - m->functions.data] +
				x64_get_prologue_length(meta, stack_usage) + p->pos + 4;

            *((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - actual_pos;
        }
    }
}

static int get_data_type_size(const TB_DataType dt) {
    assert(dt.width <= 2 && "Vector width too big!");

    switch (dt.type) {
		case TB_INT: {
			// round up bits to a byte
			bool is_big_int = dt.data > 64;
			int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

			return ((bits+7) / 8) << dt.width;
		}
		case TB_FLOAT: {
			int s = 0;
			if (dt.data == TB_FLT_32) s = 4;
			else if (dt.data == TB_FLT_64) s = 8;
			else tb_unreachable();

			return s << dt.width;
		}
		case TB_PTR: {
			return 8;
		}
		default: {
			tb_unreachable();
			return 0;
		}
    }
}

void x64_get_data_type_size(TB_DataType dt, TB_CharUnits* out_size, TB_CharUnits* out_align) {
	switch (dt.type) {
		case TB_INT: {
			// round up bits to a byte
			bool is_big_int = dt.data > 64;
			int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

			*out_size  = ((bits+7) / 8) << dt.width;
			*out_align = is_big_int ? 8 : bits/8;
			break;
		}
		case TB_FLOAT: {
			int s = 0;
			if (dt.data == TB_FLT_32) s = 4;
			else if (dt.data == TB_FLT_64) s = 8;
			else tb_unreachable();

			*out_size = s << dt.width;
			*out_align = s;
			break;
		}
		case TB_PTR: {
			*out_size = 8;
			*out_align = 8;
			break;
		}
		default: tb_unreachable();
	}
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen x64_codegen = {
	.minimum_addressable_size = 8,
	.pointer_size = 64,

	.get_data_type_size  = x64_get_data_type_size,
	.emit_call_patches   = x64_emit_call_patches,
	.get_prologue_length = x64_get_prologue_length,
	.get_epilogue_length = x64_get_epilogue_length,
	.emit_prologue       = x64_emit_prologue,
	.emit_epilogue       = x64_emit_epilogue,

	.fast_path    = x64_fast_compile_function,
	//.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
