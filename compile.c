#include "compile.h"

#ifdef RELEASE_BUILD
#define EXTRA_DEFINES_FOR_NEGATE "-DRELEASE_BUILD"
#else
#define EXTRA_DEFINES_FOR_NEGATE "-DDOING_TB_CRAP"
#endif

static const char* INPUT_FILES[] = {
    "src/tb/tb.c",
    "src/tb/tb_analysis.c",
    "src/tb/tb_atomic.c",
    "src/tb/tb_builder.c",
    "src/tb/tb_internal.c",
    "src/tb/tb_jit.c",
    "src/tb/ir_printer.c",
    "src/tb/debug_builder.c",
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
    "src/tb/aarch64/aarch64.c",

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
    "src/tb/opt/refinement.c",
    "src/tb/opt/mem2reg.c",
    "src/tb/opt/strength_reduction.c",

    // Platform specific
    #if defined(_WIN32)
    "src/tb/system/win32.c",
    #else
    "src/tb/system/posix.c",
    #endif
};
enum { INPUT_FILE_COUNT = sizeof(INPUT_FILES) / sizeof(INPUT_FILES[0]) };

static int convert_into_c_array(const char* name, const char* in, const char* out) {
    FILE *fp = fopen(in, "rb");
    if (!fp) {
        fprintf(stderr, "Error opening file: %s\n", out);
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    const int fsize = ftell(fp);

    fseek(fp, 0, SEEK_SET);
    unsigned char *b = malloc(fsize);

    fread(b, fsize, 1, fp);
    fclose(fp);

    FILE *outf = fopen(out, "wb");
    fprintf(outf, "enum { %s_SIZE = %d };\n", name, fsize);
    fprintf(outf, "static const char %s[%d+1] = {\n", name, fsize);
    for (int i = 0; i < fsize; ++i) {
        fprintf(outf, "0x%02x%s", b[i], i == fsize-1 ? "" : ((i+1) % 16 == 0 ? ",\n" : ","));
    }
    fprintf(outf, "\n};\n");
    fclose(outf);
    return 0;
}

int main(int argc, char* argv[]) {
    const char* output_lib_path = NULL;
    if (argc >= 2) {
        output_lib_path = argv[1];
    } else {
        output_lib_path = "bin/tildebackend";
    }

    // Clone and compile LuaJIT if not done already
    bool luajit_build = false;
    #ifdef _WIN32
    luajit_build =
        !file_exists("deps/luajit/src/luajit.lib") ||
        !file_exists("deps/luajit/src/lua51.lib");
    #else
    luajit_build =
        !file_exists("deps/luajit/src/libluajit.a");
    #endif

    if (luajit_build) {
        printf("Compiling LuaJIT...\n");
        if (!file_exists("deps/luajit")) {
            cmd_append("git clone --depth 1 https://github.com/LuaJIT/LuaJIT.git deps/luajit");
            cmd_run();
            cmd_wait_for_all();
        }

        // Invoke build script
        if (ON_WINDOWS) {
            cmd_append("cd deps/luajit/src && msvcbuild.bat");
        } else {
            create_dir_if_not_exists("deps/luajit/lua");
            cmd_append("cd deps/luajit && make");
        }

        cmd_run();
        cmd_wait_for_all();
    }

    nbuild_init();
    create_dir_if_not_exists("bin"SLASH);

    convert_into_c_array("PRELUDE", "src/tb/opt/prelude.lua", "src/tb/opt/lua_prelude.inc");

    #ifdef RELEASE_BUILD
    printf("Compiling a release build!\n");
    #endif

    size_t include_len = 0;
    const char** includes = NULL;
    APPEND(include_len, includes, "deps/luajit/src");
    APPEND(include_len, includes, "include");

    CC_Options options = {
        .output_dir = "bin"SLASH,

        .include_len = include_len,
        .includes = includes,

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

        .use_asan = false,
        .debug_info = true
    };

    for (int i = 0; i < INPUT_FILE_COUNT; i++) {
        cc_invoke(&options, INPUT_FILES[i], NULL);
    }

    cmd_wait_for_all();

    if (!ON_WINDOWS) {
        // unwrap luajit into object files
        cmd_append("cd bin/ && ar -x ../deps/luajit/src/libluajit.a");
        cmd_run();
        cmd_wait_for_all();
    }

    printf("Converting to a library...\n");

    size_t ar_input_len = 0;
    const char** ar_inputs = NULL;

    #if ON_WINDOWS
    APPEND(ar_input_len, ar_inputs, "bin"SLASH"*.obj");
    APPEND(ar_input_len, ar_inputs, "deps/tbbmalloc.lib");
    APPEND(ar_input_len, ar_inputs, "deps/luajit/src/lua51.lib");
    APPEND(ar_input_len, ar_inputs, "deps/luajit/src/luajit.lib");
    #else
    APPEND(ar_input_len, ar_inputs, "bin"SLASH"*.o");
    #endif

    ar_invoke(output_lib_path, ar_input_len, ar_inputs);
    cmd_wait_for_all();
    clean("bin"SLASH);

    //printf("TB done... %s\n", output_lib_path);

    #if defined(NEGATE)
    // personal crap
    system("cd W:/Workspace/Cuik/ && clang compile.c "EXTRA_DEFINES_FOR_NEGATE" && a.exe");
    #endif

    return 0;
}
