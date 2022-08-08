#include "tb_internal.h"
#include <stdarg.h>

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <emmintrin.h>
#endif

static thread_local uint8_t* tb_thread_storage;
static thread_local int tid;
static tb_atomic_int total_tid;

const IDebugFormat* tb__find_debug_format(TB_Module* m) {
    switch (m->debug_fmt) {
        //case TB_DEBUGFMT_DWARF: return &tb__dwarf_debug_format;
        case TB_DEBUGFMT_CODEVIEW: return &tb__codeview_debug_format;
        default: return NULL;
    }
}

ICodeGen* tb__find_code_generator(TB_Module* m) {
    switch (m->target_arch) {
        case TB_ARCH_X86_64: return &tb__x64_codegen;
        case TB_ARCH_AARCH64: return &tb__aarch64_codegen;
        default: return NULL;
    }
}

int tb__get_local_tid(void) {
    // the value it spits out is zero-based, but
    // the TIDs consider zero as a NULL space.
    if (tid == 0) {
        int new_id = tb_atomic_int_add(&total_tid, 1);
        tid = new_id + 1;
    }

    return tid - 1;
}

static TB_CodeRegion* get_or_allocate_code_region(TB_Module* m, int tid) {
    if (m->code_regions[tid] == NULL) {
        m->code_regions[tid] = tb_platform_valloc(CODE_REGION_BUFFER_SIZE);
        if (m->code_regions[tid] == NULL) tb_panic("could not allocate code region!");
    }

    return m->code_regions[tid];
}

TB_API void tb_function_free(TB_Function* f) {
    // NOTE(NeGate): Doesn't free the name, it's kept forever
    if (f->nodes == NULL) return;

    tb_platform_heap_free(f->nodes);
    tb_platform_heap_free(f->vla.data);

    f->nodes = NULL;
    f->vla.data = NULL;
}

TB_API TB_DataType tb_vector_type(TB_DataTypeEnum type, int width) {
    assert(tb_is_power_of_two(width));
    return (TB_DataType) { .type = type, .width = tb_ffs(width) - 1 };
}

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, TB_DebugFormat debug_fmt, const TB_FeatureSet* features) {
    TB_Module* m = tb_platform_heap_alloc(sizeof(TB_Module));
    memset(m, 0, sizeof(TB_Module));

    m->max_threads = TB_MAX_THREADS;
    m->target_abi = (target_system == TB_SYSTEM_WINDOWS) ? TB_ABI_WIN64 : TB_ABI_SYSTEMV;
    m->target_arch = target_arch;
    m->target_system = target_system;
    m->debug_fmt = debug_fmt;
    m->features = *features;

    m->prototypes_arena = tb_platform_valloc(PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));

    m->files.count = 1;
    m->files.capacity = 64;
    m->files.data = tb_platform_heap_alloc(64 * sizeof(TB_File));
    m->files.data[0] = (TB_File) { 0 };

    m->functions.count = 0;
    m->functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_Function));

    loop(i, TB_MAX_THREADS) {
        m->thread_info[i].global_patches = dyn_array_create(TB_GlobalPatch);
        m->thread_info[i].const_patches  = dyn_array_create(TB_ConstPoolPatch);
        m->thread_info[i].call_patches   = dyn_array_create(TB_FunctionPatch);
        m->thread_info[i].ecall_patches  = dyn_array_create(TB_ExternFunctionPatch);
    }

    // we start a little off the start just because
    m->rdata_region_size = 16;

    tb_platform_arena_init();
    return m;
}

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f, TB_ISelMode isel_mode) {
    ICodeGen* restrict codegen = tb__find_code_generator(m);

    // Machine code gen
    int id = tb__get_local_tid();
    assert(id < TB_MAX_THREADS);

    TB_CodeRegion* region = get_or_allocate_code_region(m, id);
    TB_FunctionOutput* func_out = tb_platform_arena_alloc(sizeof(TB_FunctionOutput));

    TB_FunctionID index = tb_function_get_id(m, f);
    if (isel_mode == TB_ISEL_COMPLEX && codegen->complex_path == NULL) {
        // TODO(NeGate): we need better logging...
        fprintf(stderr, "TB warning: complex path is missing, defaulting to fast path.\n");
        isel_mode = TB_ISEL_FAST;
    }

    if (isel_mode == TB_ISEL_COMPLEX) {
        *func_out = codegen->complex_path(index, f, &m->features, &region->data[region->size], id);
    } else {
        *func_out = codegen->fast_path(index, f, &m->features, &region->data[region->size], id);
    }
    tb_atomic_size_add(&m->functions.compiled_count, 1);
    region->size += func_out->code_size;

    if (region->size > CODE_REGION_BUFFER_SIZE) {
        tb_panic("Code region buffer: out of memory!\n");
    }

    f->output = func_out;
    return true;
}

TB_API TB_ModuleExporter* tb_module_make_exporter(TB_Module* m) {
    switch (m->target_system) {
        case TB_SYSTEM_WINDOWS: return tb_coff__make(m);
        default:                tb_panic("TinyBackend error: Unknown system!\n");
    }
}

TB_API bool tb_module_exporter_next(TB_Module* m, TB_ModuleExporter* exporter, TB_ModuleExportPacket* packet) {
    switch (m->target_system) {
        case TB_SYSTEM_WINDOWS: return tb_coff__next(m, exporter, packet);
        default:                tb_panic("TinyBackend error: Unknown system!\n");
    }
}

TB_API bool tb_module_export_exec(TB_Module* m, const char* path, const TB_LinkerInput* input) {
    const ICodeGen* restrict code_gen = tb__find_code_generator(m);
    const IDebugFormat* restrict debug_fmt = tb__find_debug_format(m);

    switch (m->target_system) {
        case TB_SYSTEM_WINDOWS: tb_export_pe(m, code_gen, input, path, debug_fmt); break;
        default:                tb_panic("TinyBackend error: Unknown system!\n");
    }

    return true;
}

TB_API void tb_module_destroy(TB_Module* m) {
    tb_platform_arena_free();
    tb_platform_string_free();

    loop(i, m->functions.count) {
        tb_function_free(&m->functions.data[i]);
    }

    loop(i, m->max_threads) if (m->code_regions[i]) {
        tb_platform_vfree(m->code_regions[i], CODE_REGION_BUFFER_SIZE);
        m->code_regions[i] = NULL;
    }

    if (m->jit_region) {
        tb_platform_vfree(m->jit_region, m->jit_region_size);
        m->jit_region = NULL;
    }

    loop(i, m->max_threads) {
        // TODO(NeGate): free pools
        //dyn_array_destroy(m->thread_info[i].initializers);
        //dyn_array_destroy(m->thread_info[i].externals);

        dyn_array_destroy(m->thread_info[i].call_patches);
        dyn_array_destroy(m->thread_info[i].ecall_patches);
        dyn_array_destroy(m->thread_info[i].const_patches);
    }

    tb_platform_vfree(m->prototypes_arena, PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));

    tb_platform_heap_free(m->files.data);
    tb_platform_heap_free(m->functions.data);
    tb_platform_heap_free(m);
}

TB_API TB_FileID tb_file_create(TB_Module* m, const char* path) {
    // skip the NULL file entry
    loop_range(i, 1, m->files.count) {
        if (strcmp(m->files.data[i].path, path) == 0) return i;
    }

    if (m->files.count + 1 >= m->files.capacity) {
        m->files.capacity *= 2;
        m->files.data = tb_platform_heap_realloc(m->files.data, m->files.capacity * sizeof(TB_File));
    }

    char* str = tb_platform_string_alloc(path);

    size_t r = m->files.count++;
    m->files.data[r] = (TB_File) { .path = str };
    return r;
}

void tb_function_reserve_nodes(TB_Function* f, size_t extra) {
    if (f->node_count + extra >= f->node_capacity) {
        f->node_capacity = (f->node_count + extra) * 2;

        f->nodes = tb_platform_heap_realloc(f->nodes, sizeof(TB_Node) * f->node_capacity);
        if (f->nodes == NULL) tb_panic("Out of memory");
    }
}

TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, int num_params, bool has_varargs) {
    assert(num_params == (uint32_t)num_params);

    size_t space_needed = (sizeof(TB_FunctionPrototype) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);
    space_needed += ((num_params * sizeof(TB_DataType)) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);

    size_t len = tb_atomic_size_add(&m->prototypes_arena_size, space_needed);
    if (len + space_needed >= PROTOTYPES_ARENA_SIZE) {
        tb_panic("Prototype arena: out of memory!\n");
    }

    TB_FunctionPrototype* p = (TB_FunctionPrototype*)&m->prototypes_arena[len];
    p->call_conv = conv;
    p->param_capacity = num_params;
    p->param_count = 0;
    p->return_dt = return_dt;
    p->has_varargs = has_varargs;
    return p;
}

TB_API void tb_prototype_add_param(TB_FunctionPrototype* p, TB_DataType dt) {
    assert(p->param_count + 1 <= p->param_capacity);
    p->params[p->param_count++] = dt;
}

TB_API void tb_prototype_add_params(TB_FunctionPrototype* p, size_t count, const TB_DataType* dt) {
    assert(p->param_count + count <= p->param_capacity);
    memcpy(&p->params[p->param_count], dt, count * sizeof(TB_DataType));
    p->param_count += count;
}

TB_API TB_Function* tb_prototype_build(TB_Module* m, TB_FunctionPrototype* p, const char* name, TB_Linkage linkage) {
    size_t i = tb_atomic_size_add(&m->functions.count, 1);
    if (i >= TB_MAX_FUNCTIONS) {
        tb_panic("cannot spawn more TB functions\n");
    }

    assert(p->param_count == p->param_capacity);

    TB_Function* f = &m->functions.data[i];
    *f = (TB_Function) {
        .module = m, .linkage = linkage, .prototype = p, .name = tb_platform_string_alloc(name)
    };

    f->node_capacity = 64;
    f->node_count = 2 + p->param_count;
    f->nodes = tb_platform_heap_alloc(f->node_capacity * sizeof(TB_Node));

    f->attrib_pool_capacity = 64;
    f->attrib_pool_count = 1; // 0 is reserved
    f->attrib_pool = tb_platform_heap_alloc(64 * sizeof(TB_Attrib));

    // Null slot, Entry label & Parameters
    f->nodes[0] = (TB_Node) { .next = 1 };
    f->nodes[1] = (TB_Node) { .type = TB_LABEL, .dt = TB_TYPE_PTR, .label = { 0, 0 } };

    const ICodeGen* restrict code_gen = tb__find_code_generator(m);
    loop(i, p->param_count) {
        TB_DataType dt = p->params[i];

        TB_CharUnits size, align;
        code_gen->get_data_type_size(dt, &size, &align);

        f->nodes[1 + i].next = 2 + i;
        f->nodes[2 + i] = (TB_Node) {
            .type = TB_PARAM, .dt = dt, .next = 2, .param = { .id = i, .size = size }
        };
    }

    f->node_end = 2 + (p->param_count - 1);
    f->label_count = f->current_label = 1;
    return f;
}

TB_API TB_Initializer* tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects) {
    tb_assume(size == (uint32_t)size);
    tb_assume(align == (uint32_t)align);
    tb_assume(max_objects == (uint32_t)max_objects);

    size_t space_needed = (sizeof(TB_Initializer) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);
    space_needed += ((max_objects * sizeof(TB_InitObj)) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);

    TB_Initializer* init = tb_platform_heap_alloc(sizeof(TB_Initializer) + (space_needed * sizeof(uint64_t)));
    init->size = size;
    init->align = align;
    init->obj_capacity = max_objects;
    init->obj_count = 0;
    return init;
}

TB_API void* tb_initializer_add_region(TB_Module* m, TB_Initializer* init, size_t offset, size_t size) {
    assert(offset == (uint32_t)offset);
    assert(size == (uint32_t)size);
    assert(init->obj_count + 1 <= init->obj_capacity);

    void* ptr = tb_platform_heap_alloc(size);
    init->objects[init->obj_count++] = (TB_InitObj) {
        .type = TB_INIT_OBJ_REGION, .offset = offset, .region = { .size = size, .ptr = ptr }
    };

    return ptr;
}

TB_API void tb_initializer_add_global(TB_Module* m, TB_Initializer* init, size_t offset, const TB_Global* global) {
    assert(offset == (uint32_t)offset);
    assert(init->obj_count + 1 <= init->obj_capacity);
    init->objects[init->obj_count++] = (TB_InitObj) { .type = TB_INIT_OBJ_RELOC_GLOBAL, .offset = offset, .reloc_global = global };
}

TB_API void tb_initializer_add_function(TB_Module* m, TB_Initializer* init, size_t offset, const TB_Function* func) {
    assert(offset == (uint32_t)offset);
    assert(init->obj_count + 1 <= init->obj_capacity);
    init->objects[init->obj_count++] = (TB_InitObj) {  .type = TB_INIT_OBJ_RELOC_FUNCTION, .offset = offset, .reloc_function = func };
}

TB_API void tb_initializer_add_extern(TB_Module* m, TB_Initializer* init, size_t offset, const TB_External* external) {
    assert(offset == (uint32_t)offset);

    assert(init->obj_count + 1 <= init->obj_capacity);
    init->objects[init->obj_count++] = (TB_InitObj) {
        .type = TB_INIT_OBJ_RELOC_EXTERN, .offset = offset, .reloc_extern = external
    };
}

TB_API TB_Global* tb_global_create(TB_Module* m, const char* name, TB_StorageClass storage, TB_Linkage linkage) {
    int tid = tb__get_local_tid();

    TB_Global* g = pool_put(m->thread_info[tid].globals);
    *g = (TB_Global){
        .name = tb_platform_string_alloc(name),
        .linkage = linkage,
        .storage = storage
    };

    return g;
}

TB_API void tb_global_set_initializer(TB_Module* m, TB_Global* global, TB_Initializer* init) {
    tb_atomic_size_t* region_size = global->storage == TB_STORAGE_TLS ? &m->tls_region_size : &m->data_region_size;
    size_t pos = tb_atomic_size_add(region_size, init->size + init->align);

    // TODO(NeGate): Assert on non power of two alignment
    size_t align_mask = init->align - 1;
    pos = (pos + align_mask) & ~align_mask;

    assert(pos < UINT32_MAX && "Cannot fit global into space");
    assert((pos + init->size) < UINT32_MAX && "Cannot fit global into space");

    global->pos = pos;
    global->init = init;
}

TB_API void tb_module_set_tls_index(TB_Module* m, TB_External* e) {
    m->tls_index_extern = e;
}

TB_API bool tb_jit_import(TB_Module* m, const char* name, void* address) {
    // TODO(NeGate): Maybe speed this up but also maybe don't... idk
    loop(i, m->max_threads) {
        pool_for(TB_External, e, m->thread_info[i].externals) {
            if (strcmp(e->name, name) == 0) {
                e->address = address;
                return true;
            }
        }
    }

    return false;
}

TB_API TB_External* tb_extern_create(TB_Module* m, const char* name) {
    assert(name != NULL);
    int tid = tb__get_local_tid();

    TB_External* e = pool_put(m->thread_info[tid].externals);
    *e = (TB_External){ .name = tb_platform_string_alloc(name) };
    return e;
}

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name) {
    for (size_t i = 0; i < m->functions.count; i++) {
        if (strcmp(m->functions.data[i].name, name) == 0) return m->compiled_function_pos[i];
    }

    return NULL;
}

TB_API void* tb_module_get_jit_func_by_id(TB_Module* m, size_t i) {
    assert(m->compiled_function_pos);
    return m->compiled_function_pos[i];
}

TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f) {
    assert(m->compiled_function_pos);
    return m->compiled_function_pos[f - m->functions.data];
}

TB_API TB_Label tb_inst_get_current_label(TB_Function* f) {
    if (!f->current_label) return 0;

    return f->nodes[f->current_label].label.id;
}

//
// TLS - Thread local storage
//
// Certain backend elements require memory but we would prefer to avoid
// making any heap allocations when possible to there's a preallocated
// block per thread that can run TB.
//
void tb_free_thread_resources(void) {
    if (tb_thread_storage != NULL) {
        tb_platform_vfree(tb_thread_storage, TB_TEMPORARY_STORAGE_SIZE);
        tb_thread_storage = NULL;
    }
}

TB_TemporaryStorage* tb_tls_allocate() {
    if (tb_thread_storage == NULL) {
        tb_thread_storage = tb_platform_valloc(TB_TEMPORARY_STORAGE_SIZE);
        if (tb_thread_storage == NULL) {
            tb_panic("out of memory");
        }
    }

    TB_TemporaryStorage* store = (TB_TemporaryStorage*)tb_thread_storage;
    store->used = 0;
    return store;
}

TB_TemporaryStorage* tb_tls_steal() {
    if (tb_thread_storage == NULL) {
        tb_thread_storage = tb_platform_valloc(TB_TEMPORARY_STORAGE_SIZE);
        if (tb_thread_storage == NULL) {
            tb_panic("out of memory");
        }
    }

    return (TB_TemporaryStorage*)tb_thread_storage;
}

bool tb_tls_can_fit(TB_TemporaryStorage* store, size_t size) {
    return (sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);
}

void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size) {
    if (sizeof(TB_TemporaryStorage) + store->used + size >= TB_TEMPORARY_STORAGE_SIZE) {
        return NULL;
    }

    void* ptr = &store->data[store->used];
    store->used += size;
    return ptr;
}

void* tb_tls_push(TB_TemporaryStorage* store, size_t size) {
    assert(sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);

    void* ptr = &store->data[store->used];
    store->used += size;
    return ptr;
}

void* tb_tls_pop(TB_TemporaryStorage* store, size_t size) {
    assert(sizeof(TB_TemporaryStorage) + store->used > size);

    store->used -= size;
    return &store->data[store->used];
}

void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance) {
    assert(sizeof(TB_TemporaryStorage) + store->used > distance);

    return &store->data[store->used - distance];
}

void tb_tls_restore(TB_TemporaryStorage* store, void* ptr) {
    size_t i = ((uint8_t*)ptr) - store->data;
    assert(i <= store->used);

    store->used = i;
}

void tb_emit_call_patch(TB_Module* m, TB_Function* source, const TB_Function* target, size_t pos, size_t local_thread_id) {
    assert(pos == (uint32_t)pos);

    TB_FunctionPatch p = { .source = source, .target = target, .pos = pos };
    dyn_array_put(m->thread_info[local_thread_id].call_patches, p);
}

void tb_emit_ecall_patch(TB_Module* m, TB_Function* source, const TB_External* target, size_t pos, size_t local_thread_id) {
    assert(pos == (uint32_t)pos);

    TB_ExternFunctionPatch p = { .source = source, .target = target, .pos = pos };
    dyn_array_put(m->thread_info[local_thread_id].ecall_patches, p);
}

void tb_emit_global_patch(TB_Module* m, TB_Function* source, size_t pos, const TB_Global* target, size_t local_thread_id) {
    assert(pos == (uint32_t)pos);

    TB_GlobalPatch p = { .source = source, .pos = pos, .target = target };
    dyn_array_put(m->thread_info[local_thread_id].global_patches, p);
}

uint32_t tb_emit_const_patch(TB_Module* m, TB_Function* source, size_t pos, const void* ptr, size_t len, size_t local_thread_id) {
    assert(pos == (uint32_t)pos);
    assert(len == (uint32_t)len);

    size_t align = len > 8 ? 16 : 0;
    size_t alloc_pos = tb_atomic_size_add(&m->rdata_region_size, len + align);

    size_t rdata_pos = len > 8 ? align_up(alloc_pos, 16) : alloc_pos;
    TB_ConstPoolPatch p = {
        .source = source, .pos = pos, .rdata_pos = rdata_pos, .data = ptr, .length = len
    };
    dyn_array_put(m->thread_info[local_thread_id].const_patches, p);

    assert(rdata_pos == (uint32_t)rdata_pos);
    return rdata_pos;
}

//
// OBJECT FILE
//
void tb_object_free(TB_ObjectFile* obj) {
    loop(i, obj->section_count) {
        free(obj->sections[i].relocations);
    }
    free(obj);
}

//
// EMITTER CODE
//
// Simple linear allocation for the backend's to output code with
//
void* tb_out_reserve(TB_Emitter* o, size_t count) {
    if (o->count + count >= o->capacity) {
        if (o->capacity == 0) {
            o->capacity = 64;
        } else {
            o->capacity += count;
            o->capacity *= 2;
        }

        o->data = tb_platform_heap_realloc(o->data, o->capacity);
        if (o->data == NULL) tb_todo();
    }

    return &o->data[o->count];
}

void tb_out_commit(TB_Emitter* o, size_t count) {
    assert(o->count + count < o->capacity);
    o->count += count;
}

void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i) {
    assert(o->count + 1 < o->capacity);

    o->data[o->count] = i;
    o->count += 1;
}

void tb_out4b_UNSAFE(TB_Emitter* o, uint32_t i) {
    tb_out_reserve(o, 4);

    *((uint32_t*)&o->data[o->count]) = i;
    o->count += 4;
}

void tb_out1b(TB_Emitter* o, uint8_t i) {
    tb_out_reserve(o, 1);

    o->data[o->count] = i;
    o->count += 1;
}

void tb_out2b(TB_Emitter* o, uint16_t i) {
    tb_out_reserve(o, 2);

    *((uint16_t*)&o->data[o->count]) = i;
    o->count += 2;
}

void tb_out4b(TB_Emitter* o, uint32_t i) {
    tb_out_reserve(o, 4);

    *((uint32_t*)&o->data[o->count]) = i;
    o->count += 4;
}

void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i) {
    *((uint16_t*)&o->data[pos]) = i;
}

void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i) {
    *((uint32_t*)&o->data[pos]) = i;
}

uint8_t tb_get1b(TB_Emitter* o, uint32_t pos) {
    return *((uint8_t*)&o->data[pos]);
}

uint16_t tb_get2b(TB_Emitter* o, uint32_t pos) {
    return *((uint16_t*)&o->data[pos]);
}

uint32_t tb_get4b(TB_Emitter* o, uint32_t pos) {
    return *((uint32_t*)&o->data[pos]);
}

void tb_out8b(TB_Emitter* o, uint64_t i) {
    tb_out_reserve(o, 8);

    *((uint64_t*)&o->data[o->count]) = i;
    o->count += 8;
}

void tb_out_zero(TB_Emitter* o, size_t len) {
    tb_out_reserve(o, len);
    memset(&o->data[o->count], 0, len);
    o->count += len;
}

void tb_outstr_UNSAFE(TB_Emitter* o, const char* str) {
    while (*str) o->data[o->count++] = *str++;
}

void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const uint8_t* str) {
    memcpy(&o->data[o->count], str, len);
    o->count += len;
}
