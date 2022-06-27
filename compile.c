#include "compile.h"

#ifdef RELEASE_BUILD
#define EXTRA_DEFINES_FOR_NEGATE "-DRELEASE_BUILD"
#else
#define EXTRA_DEFINES_FOR_NEGATE ""
#endif

static const char* INPUT_FILES[] = {
    "src/tb/stb_ds.c",
    "src/tb/tb.c",
    "src/tb/tb_analysis.c",
    "src/tb/tb_atomic.c",
    "src/tb/tb_builder.c",
    "src/tb/tb_internal.c",
    "src/tb/tb_jit.c",
    "src/tb/ir_printer.c",
    "src/tb/hash.c",
    "src/tb/bigint/BigInt.c",

    // Generic codegen stuff
    "src/tb/codegen/tree.c",

    // Object file formats
    "src/tb/objects/coff.c",
    "src/tb/objects/coff_parse.c",
    "src/tb/objects/elf64.c",
    "src/tb/objects/macho.c",

    // Executable formats
    "src/tb/objects/pe.c",

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
    "src/tb/system/win32.c"
        #else
    "src/tb/system/posix.c"
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

    nbuild_init();
    create_dir_if_not_exists("bin"SLASH);

    #ifdef RELEASE_BUILD
    printf("Compiling a release build!\n");
    #endif

    CC_Options options = {
        .output_dir = "bin"SLASH,

        #ifdef RELEASE_BUILD
        .opt = CC_Ox,
        #else
        .opt = CC_O0,
        #endif

        // there's some SSE42 stuff in the lexer
        .vector_ext = CC_VECTOR_SSE42,

        // warning stuff
        .warnings = {
            .mode = CC_WARN_ALL,
            .as_errors = true,
            .missing_declarations = true,
            .unused_declarations = true,
        },

        .debug_info = true
    };

    for (int i = 0; i < INPUT_FILE_COUNT; i++) {
        cc_invoke(&options, INPUT_FILES[i], NULL);
    }

    cmd_wait_for_all();

    printf("Converting to a library...\n");
    ar_invoke(output_lib_path, 2, (const char*[]) {
            ON_WINDOWS ? "bin"SLASH"*.obj" : "bin"SLASH"*.o",
            ON_WINDOWS ? "external/tbbmalloc.lib" : ""
        });
    cmd_wait_for_all();
    clean("bin"SLASH);

    #if defined(NEGATE)
    // personal crap
    system("cd W:/Workspace/Cuik/ && clang compile.c "EXTRA_DEFINES_FOR_NEGATE" -o compile.exe && compile.exe");
    #endif

    return 0;
}
