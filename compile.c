#include "compile.h"

static const char* INPUT_FILES[] = {
    "src/tb/stb_ds.c",
    "src/tb/tb.c",
    "src/tb/tb_analysis.c",
    "src/tb/tb_atomic.c",
    "src/tb/tb_builder.c",
    "src/tb/tb_internal.c",
    "src/tb/tb_jit.c",
    "src/tb/hash.c",
    "src/tb/bigint/BigInt.c",

    // Generic codegen stuff
    "src/tb/codegen/tree.c",

    // Object file formats
    "src/tb/tb_coff.c",
    "src/tb/tb_coff_parse.c",
    "src/tb/tb_elf64.c",
    "src/tb/tb_macho.c",

    // Targets go here
    "src/tb/x64/x64.c",

    // Debug formats here
    "src/tb/debug/codeview.c",

    // Optimizer
    "src/tb/tb_optimizer.c",
    "src/tb/opt/merge_ret.c",
    "src/tb/opt/canonical.c",
    "src/tb/opt/const_fold.c",
    "src/tb/opt/copy_elision.c",
    "src/tb/opt/subexpr_elim.c",
    "src/tb/opt/dead_code_elim.c",
    "src/tb/opt/deshort_circuit.c",
    "src/tb/opt/load_elim.c",
    "src/tb/opt/hoist_locals.c",
    "src/tb/opt/mem2reg.c",
    "src/tb/opt/strength_reduction.c",

    // Platform specific
#if defined(_WIN32)
    "src/tb/tb_win32.c"
#else
    "src/tb/tb_posix.c"
#endif
};
enum { INPUT_FILE_COUNT = sizeof(INPUT_FILES) / sizeof(INPUT_FILES[0]) };

int main(int argc, char* argv[]) {
    const char* output_lib_path = NULL;
    if (argc >= 2) {
        output_lib_path = argv[1];
    } else {
        output_lib_path = "build/tildebackend";
    }

    builder_init();
    builder_compile(BUILD_MODE_STATIC_LIB, INPUT_FILE_COUNT, INPUT_FILES, output_lib_path, ON_WINDOWS ? "external/tbbmalloc.lib" : "");

#if defined(NEGATE)
    // personal crap
    system("cd W:/Workspace/Cuik/ && clang compile.c -o compile.exe && compile.exe");
#endif

    return 0;
}
