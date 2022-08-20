// #define TB_CORE_H 
// #define TB_REG_MAX ((TB_Reg)INT_MAX)
// #define TB_TYPE_F32 (TB_DataType){ { TB_FLOAT, 0, TB_FLT_32 } }
// #define TB_TYPE_I8 (TB_DataType){ { TB_INT,   0, 8 } }
// #define TB_TYPE_PTR (TB_DataType){ { TB_PTR,   0, 0 } }
// #define TB_VERSION_PATCH 0
// #define TB_TYPE_F64 (TB_DataType){ { TB_FLOAT, 0, TB_FLT_64 } }
// #define TB_IS_VOID_TYPE ((x).type == TB_INT && (x).data == 0)
// #define TB_FOR_FUNCTIONS for (TB_FunctionIter it = { .module_ = (module) }; tb_next_function(&it);)
// #define TB_TYPE_I64 (TB_DataType){ { TB_INT,   0, 64 } }
// #define TB_TYPE_I16 (TB_DataType){ { TB_INT,   0, 16 } }
// #define TB_IS_INTEGER_TYPE ((x).type == TB_INT)
// #define TB_IS_FLOAT_TYPE ((x).type == TB_FLOAT)
// #define TB_GET_INT_BITWIDTH ((x).data)
// #define TB_TYPE_VOID (TB_DataType){ { TB_INT,   0, 0 } }
// #define TB_IS_NODE_TERMINATOR ((type) >= TB_LABEL && (type) <= TB_RET)
// #define TB_GET_FLOAT_FORMAT ((x).data)
// #define TB_API extern
// #define TB_FOR_EXTERNALS for (TB_ExternalIter it = tb_external_iter(module); tb_next_external(&it);)
// #define TB_VERSION_MINOR 2
// #define TB_TYPE_PTRN (TB_DataType){ { TB_PTR,  0, (N) } }
// #define TB_NULL_REG ((TB_Reg)0)
// #define TB_TYPE_BOOL (TB_DataType){ { TB_INT,   0, 1 } }
// #define TB_FOR_INPUT_IN_REG for (TB_NodeInputIter it = { .parent_ = (parent) }; tb_next_node_input(f, &it);)
// #define TB_IS_NODE_SIDE_EFFECT ((type) >= TB_LINE_INFO && (type) <= TB_DEBUGBREAK)
// #define TB_HOST_ARCH TB_HOST_X86_64
// #define TB_HOST_UNKNOWN 0
// #define TB_GET_PTR_ADDRSPACE ((x).data)
// #define TB_HOST_X86_64 1
// #define TB_FOR_INPUT_IN_NODE for (TB_NodeInputIter it = { .parent_ = (parent) - f->nodes }; tb_next_node_input(f, &it);)
// #define TB_IS_POINTER_TYPE ((x).type == TB_PTR)
// #define TB_VERSION_MAJOR 0
// #define TB_TYPE_I32 (TB_DataType){ { TB_INT,   0, 32 } }
// #define TB_IS_BOOL_TYPE ((x).type == TB_INT && (x).data == 1)



// tb.h
TB_ArithmaticBehavior :: enum {
    TB_ARITHMATIC_NSW = 1,
    TB_ARITHMATIC_NUW = 2,
}
TB_DebugFormat :: enum {
    TB_DEBUGFMT_NONE = 0,
    TB_DEBUGFMT_DWARF = 1,
    TB_DEBUGFMT_CODEVIEW = 2,
    TB_DEBUGFMT_COLINPILLED = 3,
}
TB_Arch :: enum {
    TB_ARCH_UNKNOWN = 0,
    TB_ARCH_X86_64 = 1,
    TB_ARCH_AARCH64 = 2,
}
TB_System :: enum {
    TB_SYSTEM_WINDOWS = 0,
    TB_SYSTEM_LINUX = 1,
    TB_SYSTEM_MACOS = 2,
    TB_SYSTEM_ANDROID = 3,
}
TB_ABI :: enum {
    TB_ABI_WIN64 = 0,
    TB_ABI_SYSTEMV = 1,
}
TB_OutputFlavor :: enum {
    TB_FLAVOR_OBJECT = 0,
    TB_FLAVOR_SHARED = 1,
    TB_FLAVOR_STATIC = 2,
    TB_FLAVOR_EXECUTABLE = 3,
}
TB_CallingConv :: enum {
    TB_CDECL = 0,
    TB_STDCALL = 1,
}
TB_FeatureSet :: struct {
    x64: struct {
        sse3: c.bool,
        popcnt: c.bool,
        lzcnt: c.bool,
        sse41: c.bool,
        sse42: c.bool,
        clmul: c.bool,
        f16c: c.bool,
        bmi1: c.bool,
        bmi2: c.bool,
        avx: c.bool,
        avx2: c.bool,
    },
    aarch64: struct {
        bf16: c.bool,
    },
}
TB_BranchHint :: enum {
    TB_BRANCH_HINT_NONE = 0,
    TB_BRANCH_HINT_LIKELY = 1,
    TB_BRANCH_HINT_UNLIKELY = 2,
}
TB_Linkage :: enum {
    TB_LINKAGE_PUBLIC = 0,
    TB_LINKAGE_PRIVATE = 1,
}
TB_StorageClass :: enum {
    TB_STORAGE_DATA = 0,
    TB_STORAGE_TLS = 1,
}
TB_MemoryOrder :: enum {
    TB_MEM_ORDER_RELAXED = 0,
    TB_MEM_ORDER_CONSUME = 1,
    TB_MEM_ORDER_ACQUIRE = 2,
    TB_MEM_ORDER_RELEASE = 3,
    TB_MEM_ORDER_ACQ_REL = 4,
    TB_MEM_ORDER_SEQ_CST = 5,
}
TB_ISelMode :: enum {
    TB_ISEL_FAST = 0,
    TB_ISEL_COMPLEX = 1,
}
TB_DataTypeEnum :: enum {
    TB_INT = 0,
    TB_FLOAT = 1,
    TB_PTR = 2,
}
TB_FloatFormat :: enum {
    TB_FLT_32 = 0,
    TB_FLT_64 = 1,
}
TB_DataType :: struct #raw_union {
    using _: struct {
        type: uint16_t,
        width: uint16_t,
        data: uint16_t,
    },
    raw: uint16_t,
}
TB_NodeTypeEnum :: enum {
    TB_NULL = 0,
    TB_LINE_INFO = 1,
    TB_KEEPALIVE = 2,
    TB_POISON = 3,
    TB_ICALL = 4,
    TB_CALL = 5,
    TB_VCALL = 6,
    TB_ECALL = 7,
    TB_STORE = 8,
    TB_MEMCLR = 9,
    TB_MEMCPY = 10,
    TB_MEMSET = 11,
    TB_MEMCMP = 12,
    TB_INITIALIZE = 13,
    TB_ATOMIC_TEST_AND_SET = 14,
    TB_ATOMIC_CLEAR = 15,
    TB_ATOMIC_LOAD = 16,
    TB_ATOMIC_XCHG = 17,
    TB_ATOMIC_ADD = 18,
    TB_ATOMIC_SUB = 19,
    TB_ATOMIC_AND = 20,
    TB_ATOMIC_XOR = 21,
    TB_ATOMIC_OR = 22,
    TB_ATOMIC_CMPXCHG = 23,
    TB_ATOMIC_CMPXCHG2 = 24,
    TB_DEBUGBREAK = 25,
    TB_LABEL = 26,
    TB_GOTO = 27,
    TB_SWITCH = 28,
    TB_IF = 29,
    TB_RET = 30,
    TB_TRAP = 31,
    TB_UNREACHABLE = 32,
    TB_LOAD = 33,
    TB_LOCAL = 34,
    TB_PARAM_ADDR = 35,
    TB_PARAM = 36,
    TB_FUNC_ADDRESS = 37,
    TB_EXTERN_ADDRESS = 38,
    TB_GLOBAL_ADDRESS = 39,
    TB_MEMBER_ACCESS = 40,
    TB_ARRAY_ACCESS = 41,
    TB_INTEGER_CONST = 42,
    TB_FLOAT32_CONST = 43,
    TB_FLOAT64_CONST = 44,
    TB_STRING_CONST = 45,
    TB_TRUNCATE = 46,
    TB_FLOAT_EXT = 47,
    TB_SIGN_EXT = 48,
    TB_ZERO_EXT = 49,
    TB_INT2PTR = 50,
    TB_PTR2INT = 51,
    TB_UINT2FLOAT = 52,
    TB_FLOAT2UINT = 53,
    TB_INT2FLOAT = 54,
    TB_FLOAT2INT = 55,
    TB_BITCAST = 56,
    TB_SELECT = 57,
    TB_NOT = 58,
    TB_NEG = 59,
    TB_AND = 60,
    TB_OR = 61,
    TB_XOR = 62,
    TB_ADD = 63,
    TB_SUB = 64,
    TB_MUL = 65,
    TB_SHL = 66,
    TB_SHR = 67,
    TB_SAR = 68,
    TB_UDIV = 69,
    TB_SDIV = 70,
    TB_UMOD = 71,
    TB_SMOD = 72,
    TB_FADD = 73,
    TB_FSUB = 74,
    TB_FMUL = 75,
    TB_FDIV = 76,
    TB_CMP_EQ = 77,
    TB_CMP_NE = 78,
    TB_CMP_SLT = 79,
    TB_CMP_SLE = 80,
    TB_CMP_ULT = 81,
    TB_CMP_ULE = 82,
    TB_CMP_FLT = 83,
    TB_CMP_FLE = 84,
    TB_PHI1 = 85,
    TB_PHI2 = 86,
    TB_PHIN = 87,
    TB_PASS = 88,
    TB_VA_START = 89,
    TB_X86INTRIN_LDMXCSR = 90,
    TB_X86INTRIN_STMXCSR = 91,
    TB_X86INTRIN_SQRT = 92,
    TB_X86INTRIN_RSQRT = 93,
}
TB_NodeType :: uint8_t
TB_Label :: c.int
TB_SwitchEntry :: struct {
    key: int32_t,
    value: TB_Label,
}
TB_Slice :: struct {
    length: size_t,
    data: ^uint8_t,
}
TB_CharUnits :: uint32_t
TB_AttributeID :: c.int
TB_FileID :: c.int
TB_FunctionID :: c.int
TB_Module :: struct
TB_External :: struct
TB_Global :: struct
TB_DebugType :: struct
TB_Initializer :: struct
TB_Function :: struct
TB_AttribList :: struct
TB_FunctionPrototype :: struct
TB_ModuleExporter :: struct
TB_Reg :: c.int
TB_Register :: c.int
TB_PhiInput :: struct {
    label: TB_Reg,
    val: TB_Reg,
}
TB_Node :: struct {
    type: TB_NodeType,
    dt: TB_DataType,
    next: TB_Reg,
    first_attrib: ^TB_AttribList,
    using _: struct #raw_union {
        integer: struct {
            num_words: size_t,
            using _: struct #raw_union {
                single_word: uint64_t,
                words: ^uint64_t,
            },
        },
        flt32: struct {
            value: c.float,
        },
        flt64: struct {
            value: c.double,
        },
        string: struct {
            length: size_t,
            data: ^c.char,
        },
        func: struct {
            value: ^TB_Function,
        },
        external: struct {
            value: ^TB_External,
        },
        global: struct {
            value: ^TB_Global,
        },
        line_info: struct {
            file: TB_FileID,
            line: c.int,
        },
        member_access: struct {
            base: TB_Reg,
            offset: int32_t,
        },
        array_access: struct {
            base: TB_Reg,
            index: TB_Reg,
            stride: TB_CharUnits,
        },
        ptrdiff: struct {
            a: TB_Reg,
            b: TB_Reg,
            stride: TB_CharUnits,
        },
        param: struct {
            id: uint32_t,
            size: TB_CharUnits,
        },
        param_addr: struct {
            param: TB_Reg,
            size: TB_CharUnits,
            alignment: TB_CharUnits,
        },
        local: struct {
            size: TB_CharUnits,
            alignment: TB_CharUnits,
            name: ^c.char,
        },
        unary: struct {
            src: TB_Reg,
        },
        i_arith: struct {
            a: TB_Reg,
            b: TB_Reg,
            arith_behavior: TB_ArithmaticBehavior,
        },
        f_arith: struct {
            a: TB_Reg,
            b: TB_Reg,
        },
        cmp: struct {
            a: TB_Reg,
            b: TB_Reg,
            dt: TB_DataType,
        },
        select: struct {
            a: TB_Reg,
            b: TB_Reg,
            cond: TB_Reg,
        },
        load: struct {
            address: TB_Reg,
            _: TB_Reg,
            alignment: TB_CharUnits,
            is_volatile: c.bool,
        },
        store: struct {
            address: TB_Reg,
            value: TB_Reg,
            alignment: TB_CharUnits,
            is_volatile: c.bool,
        },
        atomic: struct {
            addr: TB_Reg,
            src: TB_Reg,
            order: TB_MemoryOrder,
            order2: TB_MemoryOrder,
        },
        ret: struct {
            value: TB_Reg,
        },
        pass: struct {
            value: TB_Reg,
        },
        phi1: struct {
            inputs: [1]TB_PhiInput,
        },
        phi2: struct {
            inputs: [2]TB_PhiInput,
        },
        phi: struct {
            count: size_t,
            inputs: ^TB_PhiInput,
        },
        label: struct {
            id: TB_Label,
            terminator: TB_Reg,
        },
        if_: struct {
            cond: TB_Reg,
            if_true: TB_Label,
            if_false: TB_Label,
        },
        goto_: struct {
            label: TB_Label,
        },
        ecall: struct {
            param_start: c.int,
            param_end: c.int,
            target: ^TB_External,
        },
        vcall: struct {
            param_start: c.int,
            param_end: c.int,
            target: TB_Reg,
        },
        call: struct {
            param_start: c.int,
            param_end: c.int,
            target: ^TB_Function,
        },
        switch_: struct {
            key: TB_Reg,
            default_label: TB_Label,
            entries_start: c.int,
            entries_end: c.int,
        },
        mem_op: struct {
            dst: TB_Reg,
            src: TB_Reg,
            size: TB_Reg,
            align: TB_CharUnits,
        },
        clear: struct {
            dst: TB_Reg,
            size: TB_CharUnits,
            align: TB_CharUnits,
        },
        init: struct {
            addr: TB_Reg,
            src: ^TB_Initializer,
        },
    },
}
TB_CmpXchgResult :: struct {
    success: TB_Reg,
    old_value: TB_Reg,
}
TB_Loop :: struct {
    parent_loop: ptrdiff_t,
    header: TB_Label,
    backedge: TB_Label,
    body_count: size_t,
    body: ^TB_Label,
}
TB_LoopInfo :: struct {
    count: size_t,
    loops: ^TB_Loop,
}
TB_Predeccesors :: struct {
    count: ^c.int,
    preds: ^^TB_Label,
}
TB_ObjectRelocType :: enum {
    TB_OBJECT_RELOC_NONE = 0,
    TB_OBJECT_RELOC_ADDR32 = 1,
    TB_OBJECT_RELOC_ADDR64 = 2,
    TB_OBJECT_RELOC_SECREL = 3,
    TB_OBJECT_RELOC_SECTION = 4,
    TB_OBJECT_RELOC_ADDR32NB = 5,
    TB_OBJECT_RELOC_REL32 = 6,
    TB_OBJECT_RELOC_REL32_1 = 7,
    TB_OBJECT_RELOC_REL32_2 = 8,
    TB_OBJECT_RELOC_REL32_3 = 9,
    TB_OBJECT_RELOC_REL32_4 = 10,
    TB_OBJECT_RELOC_REL32_5 = 11,
    TB_OBJECT_RELOC_BRANCH26 = 12,
    TB_OBJECT_RELOC_REL21 = 13,
}
TB_ObjectReloc :: struct {
    type: TB_ObjectRelocType,
    symbol_index: uint32_t,
    virtual_address: size_t,
    addend: size_t,
}
TB_ObjectSection :: struct {
    name: TB_Slice,
    virtual_address: size_t,
    virtual_size: size_t,
    raw_data: TB_Slice,
    relocation_count: size_t,
    relocations: ^TB_ObjectReloc,
}
TB_ObjectSymbolType :: enum {
    TB_OBJECT_SYMBOL_SECTION = 0,
}
TB_ObjectSymbol :: struct {
    type: TB_ObjectSymbolType,
    name: TB_Slice,
    user_data: ^void,
}
TB_ObjectFileType :: enum {
    TB_OBJECT_FILE_UNKNOWN = 0,
    TB_OBJECT_FILE_COFF = 1,
    TB_OBJECT_FILE_ELF64 = 2,
}
TB_ObjectFile :: struct {
    type: TB_ObjectFileType,
    arch: TB_Arch,
    symbol_count: size_t,
    symbols: ^TB_ObjectSymbol,
    section_count: size_t,
    sections: []TB_ObjectSection,
}
TB_ArchiveImport :: struct {
    libname: ^c.char,
    name: ^c.char,
}
TB_ArchiveFile :: struct {
    object_file_count: size_t,
    import_count: size_t,
    imports: ^TB_ArchiveImport,
    object_file_names: ^^c.char,
    object_files: []TB_Slice,
}
TB_LinkerInput :: struct {
    input_count: size_t,
    inputs: ^^c.char,
    search_dir_count: size_t,
    search_dirs: ^^c.char,
}
TB_ModuleExportPacket :: struct {
    type: enum {
        TB_EXPORT_PACKET_NONE = 0,
        TB_EXPORT_PACKET_ALLOC = 1,
        TB_EXPORT_PACKET_WRITE = 2,
    },
    using _: struct #raw_union {
        alloc: struct {
            request_size: size_t,
            memory: ^void,
        },
        write: struct {
            length: size_t,
            data: ^uint8_t,
        },
    },
}
TB_PrintCallback :: ^proc(user_data: ^void, fmt: ^c.char) -> void
tb_module_create :: proc(target_arch: TB_Arch, target_system: TB_System, debug_fmt: TB_DebugFormat, features: ^TB_FeatureSet) -> ^TB_Module ---
tb_module_compile_func :: proc(m: ^TB_Module, f: ^TB_Function, isel_mode: TB_ISelMode) -> c.bool ---
tb_module_destroy :: proc(m: ^TB_Module) -> void ---
tb_module_set_tls_index :: proc(m: ^TB_Module, e: ^TB_External) -> void ---
tb_make_exporter :: proc(m: ^TB_Module) -> ^TB_ModuleExporter ---
tb_exporter_next :: proc(m: ^TB_Module, exporter: ^TB_ModuleExporter, packet: ^TB_ModuleExportPacket) -> c.bool ---
tb_module_export_exec :: proc(m: ^TB_Module, path: ^c.char, input: ^TB_LinkerInput) -> c.bool ---
tb_module_export_jit :: proc(m: ^TB_Module, isel_mode: TB_ISelMode) -> void ---
TB_FunctionIter :: struct {
    f: ^TB_Function,
    module_: ^TB_Module,
    index_: size_t,
}
tb_function_iter :: proc(m: ^TB_Module) -> TB_FunctionIter ---
tb_next_function :: proc(it: ^TB_FunctionIter) -> c.bool ---
TB_ExternalIter :: struct {
    e: ^TB_External,
    module_: ^TB_Module,
    p_: ^void,
    a_: size_t,
    b_: size_t,
    c_: size_t,
}
tb_external_iter :: proc(m: ^TB_Module) -> TB_ExternalIter ---
tb_next_external :: proc(it: ^TB_ExternalIter) -> c.bool ---
tb_extern_get_name :: proc(e: ^TB_External) -> ^c.char ---
tb_jit_import :: proc(m: ^TB_Module, name: ^c.char, address: ^void) -> c.bool ---
tb_extern_create :: proc(m: ^TB_Module, name: ^c.char) -> ^TB_External ---
tb_file_create :: proc(m: ^TB_Module, path: ^c.char) -> TB_FileID ---
tb_free_thread_resources :: proc() -> void ---
tb_prototype_create :: proc(m: ^TB_Module, conv: TB_CallingConv, return_dt: TB_DataType, num_params: c.int, has_varargs: c.bool) -> ^TB_FunctionPrototype ---
tb_prototype_add_param :: proc(p: ^TB_FunctionPrototype, dt: TB_DataType) -> void ---
tb_prototype_add_params :: proc(p: ^TB_FunctionPrototype, count: size_t, dt: ^TB_DataType) -> void ---
tb_prototype_build :: proc(m: ^TB_Module, p: ^TB_FunctionPrototype, name: ^c.char, linkage: TB_Linkage) -> ^TB_Function ---
tb_initializer_create :: proc(m: ^TB_Module, size: size_t, align: size_t, max_objects: size_t) -> ^TB_Initializer ---
tb_initializer_add_region :: proc(m: ^TB_Module, id: ^TB_Initializer, offset: size_t, size: size_t) -> ^void ---
tb_initializer_add_global :: proc(m: ^TB_Module, id: ^TB_Initializer, offset: size_t, global: ^TB_Global) -> void ---
tb_initializer_add_function :: proc(m: ^TB_Module, id: ^TB_Initializer, offset: size_t, func: ^TB_Function) -> void ---
tb_initializer_add_extern :: proc(m: ^TB_Module, id: ^TB_Initializer, offset: size_t, external: ^TB_External) -> void ---
tb_global_create :: proc(m: ^TB_Module, name: ^c.char, storage: TB_StorageClass, linkage: TB_Linkage) -> ^TB_Global ---
tb_global_set_initializer :: proc(m: ^TB_Module, global: ^TB_Global, initializer: ^TB_Initializer) -> void ---
tb_function_attrib_restrict :: proc(f: ^TB_Function, scope: TB_AttributeID) -> TB_AttributeID ---
tb_function_attrib_scope :: proc(f: ^TB_Function, parent_scope: TB_AttributeID) -> TB_AttributeID ---
tb_function_append_attrib :: proc(f: ^TB_Function, r: TB_Reg, a: TB_AttributeID) -> void ---
tb_debug_get_void :: proc(m: ^TB_Module) -> ^TB_DebugType ---
tb_debug_get_integer :: proc(m: ^TB_Module, is_signed: c.bool, bits: c.int) -> ^TB_DebugType ---
tb_debug_get_float :: proc(m: ^TB_Module, fmt: TB_FloatFormat) -> ^TB_DebugType ---
tb_debug_create_ptr :: proc(m: ^TB_Module, base: ^TB_DebugType) -> ^TB_DebugType ---
tb_debug_create_array :: proc(m: ^TB_Module, base: ^TB_DebugType, count: size_t) -> ^TB_DebugType ---
TB_NodeInputIter :: struct {
    r: TB_Reg,
    index_: c.int,
    parent_: TB_Reg,
}
tb_node_input_iter :: proc(r: TB_Reg) -> TB_NodeInputIter ---
tb_next_node_input :: proc(f: ^TB_Function, iter: ^TB_NodeInputIter) -> c.bool ---
tb_default_print_callback :: proc(user_data: ^void, fmt: ^c.char) -> void ---
tb_vector_type :: proc(type: TB_DataTypeEnum, width: c.int) -> TB_DataType ---
tb_function_insert :: proc(f: ^TB_Function, r: TB_Reg, n: TB_Node) -> TB_Reg ---
tb_function_set :: proc(f: ^TB_Function, r: TB_Reg, n: TB_Node) -> TB_Reg ---
tb_function_append :: proc(f: ^TB_Function, n: TB_Node) -> TB_Reg ---
tb_function_print :: proc(f: ^TB_Function, callback: TB_PrintCallback, user_data: ^void) -> void ---
tb_function_print2 :: proc(f: ^TB_Function, callback: TB_PrintCallback, user_data: ^void, display_nops: c.bool) -> void ---
tb_function_free :: proc(f: ^TB_Function) -> void ---
tb_inst_get_current_label :: proc(f: ^TB_Function) -> TB_Label ---
tb_inst_loc :: proc(f: ^TB_Function, file: TB_FileID, line: c.int) -> void ---
tb_inst_set_scope :: proc(f: ^TB_Function, scope: TB_AttributeID) -> void ---
tb_inst_get_scope :: proc(f: ^TB_Function) -> TB_AttributeID ---
tb_inst_unreachable :: proc(f: ^TB_Function) -> void ---
tb_inst_debugbreak :: proc(f: ^TB_Function) -> void ---
tb_inst_param :: proc(f: ^TB_Function, param_id: c.int) -> TB_Reg ---
tb_inst_param_addr :: proc(f: ^TB_Function, param_id: c.int) -> TB_Reg ---
tb_inst_fpxt :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType) -> TB_Reg ---
tb_inst_sxt :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType) -> TB_Reg ---
tb_inst_zxt :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType) -> TB_Reg ---
tb_inst_trunc :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType) -> TB_Reg ---
tb_inst_int2ptr :: proc(f: ^TB_Function, src: TB_Reg) -> TB_Reg ---
tb_inst_ptr2int :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType) -> TB_Reg ---
tb_inst_int2float :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType, is_signed: c.bool) -> TB_Reg ---
tb_inst_float2int :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType, is_signed: c.bool) -> TB_Reg ---
tb_inst_bitcast :: proc(f: ^TB_Function, src: TB_Reg, dt: TB_DataType) -> TB_Reg ---
tb_inst_local :: proc(f: ^TB_Function, size: uint32_t, align: TB_CharUnits, name: ^c.char) -> TB_Reg ---
tb_inst_load :: proc(f: ^TB_Function, dt: TB_DataType, addr: TB_Reg, align: TB_CharUnits) -> TB_Reg ---
tb_inst_store :: proc(f: ^TB_Function, dt: TB_DataType, addr: TB_Reg, val: TB_Reg, align: TB_CharUnits) -> void ---
tb_inst_volatile_load :: proc(f: ^TB_Function, dt: TB_DataType, addr: TB_Reg, alignment: TB_CharUnits) -> TB_Reg ---
tb_inst_volatile_store :: proc(f: ^TB_Function, dt: TB_DataType, addr: TB_Reg, val: TB_Reg, alignment: TB_CharUnits) -> void ---
tb_inst_bool :: proc(f: ^TB_Function, imm: c.bool) -> TB_Reg ---
tb_inst_ptr :: proc(f: ^TB_Function, imm: uint64_t) -> TB_Reg ---
tb_inst_sint :: proc(f: ^TB_Function, dt: TB_DataType, imm: int64_t) -> TB_Reg ---
tb_inst_uint :: proc(f: ^TB_Function, dt: TB_DataType, imm: uint64_t) -> TB_Reg ---
tb_inst_float32 :: proc(f: ^TB_Function, imm: c.float) -> TB_Reg ---
tb_inst_float64 :: proc(f: ^TB_Function, imm: c.double) -> TB_Reg ---
tb_inst_cstring :: proc(f: ^TB_Function, str: ^c.char) -> TB_Reg ---
tb_inst_string :: proc(f: ^TB_Function, len: size_t, str: ^c.char) -> TB_Reg ---
tb_inst_initialize_mem :: proc(f: ^TB_Function, addr: TB_Reg, src: ^TB_Initializer) -> void ---
tb_inst_memset :: proc(f: ^TB_Function, dst: TB_Reg, val: TB_Reg, count: TB_Reg, align: TB_CharUnits) -> void ---
tb_inst_memcpy :: proc(f: ^TB_Function, dst: TB_Reg, src: TB_Reg, count: TB_Reg, align: TB_CharUnits) -> void ---
tb_inst_memclr :: proc(f: ^TB_Function, addr: TB_Reg, size: TB_CharUnits, align: TB_CharUnits) -> void ---
tb_inst_array_access :: proc(f: ^TB_Function, base: TB_Reg, index: TB_Reg, stride: uint32_t) -> TB_Reg ---
tb_inst_member_access :: proc(f: ^TB_Function, base: TB_Reg, offset: int32_t) -> TB_Reg ---
tb_inst_get_func_address :: proc(f: ^TB_Function, target: ^TB_Function) -> TB_Reg ---
tb_inst_get_extern_address :: proc(f: ^TB_Function, target: ^TB_External) -> TB_Reg ---
tb_inst_get_global_address :: proc(f: ^TB_Function, target: ^TB_Global) -> TB_Reg ---
tb_inst_select :: proc(f: ^TB_Function, cond: TB_Reg, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_add :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, arith_behavior: TB_ArithmaticBehavior) -> TB_Reg ---
tb_inst_sub :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, arith_behavior: TB_ArithmaticBehavior) -> TB_Reg ---
tb_inst_mul :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, arith_behavior: TB_ArithmaticBehavior) -> TB_Reg ---
tb_inst_div :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, signedness: c.bool) -> TB_Reg ---
tb_inst_mod :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, signedness: c.bool) -> TB_Reg ---
tb_inst_not :: proc(f: ^TB_Function, n: TB_Reg) -> TB_Reg ---
tb_inst_neg :: proc(f: ^TB_Function, n: TB_Reg) -> TB_Reg ---
tb_inst_and :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_or :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_xor :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_sar :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_shl :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, arith_behavior: TB_ArithmaticBehavior) -> TB_Reg ---
tb_inst_shr :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_atomic_test_and_set :: proc(f: ^TB_Function, addr: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_clear :: proc(f: ^TB_Function, addr: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_load :: proc(f: ^TB_Function, addr: TB_Reg, dt: TB_DataType, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_xchg :: proc(f: ^TB_Function, addr: TB_Reg, src: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_add :: proc(f: ^TB_Function, addr: TB_Reg, src: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_sub :: proc(f: ^TB_Function, addr: TB_Reg, src: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_and :: proc(f: ^TB_Function, addr: TB_Reg, src: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_xor :: proc(f: ^TB_Function, addr: TB_Reg, src: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_or :: proc(f: ^TB_Function, addr: TB_Reg, src: TB_Reg, order: TB_MemoryOrder) -> TB_Reg ---
tb_inst_atomic_cmpxchg :: proc(f: ^TB_Function, addr: TB_Reg, expected: TB_Reg, desired: TB_Reg, succ: TB_MemoryOrder, fail: TB_MemoryOrder) -> TB_CmpXchgResult ---
tb_inst_fadd :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_fsub :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_fmul :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_fdiv :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_cmp_eq :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_cmp_ne :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_cmp_ilt :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, signedness: c.bool) -> TB_Reg ---
tb_inst_cmp_ile :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, signedness: c.bool) -> TB_Reg ---
tb_inst_cmp_igt :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, signedness: c.bool) -> TB_Reg ---
tb_inst_cmp_ige :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg, signedness: c.bool) -> TB_Reg ---
tb_inst_cmp_flt :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_cmp_fle :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_cmp_fgt :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_cmp_fge :: proc(f: ^TB_Function, a: TB_Reg, b: TB_Reg) -> TB_Reg ---
tb_inst_va_start :: proc(f: ^TB_Function, a: TB_Reg) -> TB_Reg ---
tb_inst_x86_ldmxcsr :: proc(f: ^TB_Function, a: TB_Reg) -> TB_Reg ---
tb_inst_x86_stmxcsr :: proc(f: ^TB_Function) -> TB_Reg ---
tb_inst_x86_sqrt :: proc(f: ^TB_Function, a: TB_Reg) -> TB_Reg ---
tb_inst_x86_rsqrt :: proc(f: ^TB_Function, a: TB_Reg) -> TB_Reg ---
tb_inst_call :: proc(f: ^TB_Function, dt: TB_DataType, target: ^TB_Function, param_count: size_t, params: ^TB_Reg) -> TB_Reg ---
tb_inst_vcall :: proc(f: ^TB_Function, dt: TB_DataType, target: TB_Reg, param_count: size_t, params: ^TB_Reg) -> TB_Reg ---
tb_inst_ecall :: proc(f: ^TB_Function, dt: TB_DataType, target: ^TB_External, param_count: size_t, params: ^TB_Reg) -> TB_Reg ---
tb_inst_new_label_id :: proc(f: ^TB_Function) -> TB_Label ---
tb_inst_phi2 :: proc(f: ^TB_Function, a_label: TB_Label, a: TB_Reg, b_label: TB_Label, b: TB_Reg) -> TB_Reg ---
tb_inst_label :: proc(f: ^TB_Function, id: TB_Label) -> TB_Reg ---
tb_inst_goto :: proc(f: ^TB_Function, id: TB_Label) -> void ---
tb_inst_if :: proc(f: ^TB_Function, cond: TB_Reg, if_true: TB_Label, if_false: TB_Label) -> TB_Reg ---
tb_inst_switch :: proc(f: ^TB_Function, dt: TB_DataType, key: TB_Reg, default_label: TB_Label, entry_count: size_t, entries: ^TB_SwitchEntry) -> void ---
tb_inst_ret :: proc(f: ^TB_Function, value: TB_Reg) -> void ---
TB_FunctionPass :: struct {
    name: ^c.char,
    execute: ^proc(f: ^TB_Function) -> c.bool,
    l_state: ^void,
}
TB_OptimizerCallback :: struct {
    start: ^proc(user_data: ^void, f: ^TB_Function, pass: ^TB_FunctionPass) -> void,
    pass: ^proc(user_data: ^void, f: ^TB_Function, pass: ^TB_FunctionPass, success: c.bool) -> void,
    stop: ^proc(user_data: ^void, f: ^TB_Function, pass: ^TB_FunctionPass) -> void,
}
tb_function_optimize :: proc(f: ^TB_Function, pass_count: size_t, passes: ^TB_FunctionPass) -> c.bool ---
tb_module_optimize :: proc(m: ^TB_Module) -> c.bool ---
tb_opt_load_lua_pass :: proc(path: ^c.char) -> TB_FunctionPass ---
tb_opt_unload_lua_pass :: proc(p: ^TB_FunctionPass) -> void ---
tb_get_predeccesors :: proc(f: ^TB_Function) -> TB_Predeccesors ---
tb_get_dominators :: proc(f: ^TB_Function, p: TB_Predeccesors, out_doms: ^TB_Label) -> size_t ---
tb_is_dominated_by :: proc(doms: ^TB_Label, expected_dom: TB_Label, bb: TB_Label) -> c.bool ---
tb_get_loop_info :: proc(f: ^TB_Function, preds: TB_Predeccesors, doms: ^TB_Label) -> TB_LoopInfo ---
tb_free_loop_info :: proc(loops: TB_LoopInfo) -> void ---
tb_opt_canonicalize :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_merge_rets :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_mem2reg :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_branchless :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_subexpr_elim :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_dead_expr_elim :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_dead_block_elim :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_fold :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_refinement :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_load_elim :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_hoist_invariants :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_hoist_locals :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_deshort_circuit :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_remove_pass_node :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_strength_reduction :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_compact_dead_regs :: proc(f: ^TB_Function) -> c.bool ---
tb_opt_copy_elision :: proc(f: ^TB_Function) -> c.bool ---
tb_function_get_id :: proc(m: ^TB_Module, f: ^TB_Function) -> TB_FunctionID ---
tb_function_from_id :: proc(m: ^TB_Module, id: TB_FunctionID) -> ^TB_Function ---
tb_node_get_last_register :: proc(f: ^TB_Function) -> TB_Reg ---
tb_function_get_node :: proc(f: ^TB_Function, r: TB_Reg) -> ^TB_Node ---
tb_node_is_constant_zero :: proc(f: ^TB_Function, r: TB_Reg) -> c.bool ---
tb_get_function_get_local_info :: proc(f: ^TB_Function, r: TB_Reg, size: ^c.int, align: ^c.int) -> void ---
tb_node_is_phi_node :: proc(f: ^TB_Function, r: TB_Reg) -> c.bool ---
tb_node_get_phi_width :: proc(f: ^TB_Function, r: TB_Reg) -> c.int ---
tb_node_get_phi_inputs :: proc(f: ^TB_Function, r: TB_Reg) -> ^TB_PhiInput ---
tb_node_is_conditional :: proc(f: ^TB_Function, r: TB_Reg) -> c.bool ---
tb_node_is_terminator :: proc(f: ^TB_Function, r: TB_Reg) -> c.bool ---
tb_node_is_label :: proc(f: ^TB_Function, r: TB_Reg) -> c.bool ---
tb_node_store_get_address :: proc(f: ^TB_Function, r: TB_Reg) -> TB_Reg ---
tb_node_store_get_value :: proc(f: ^TB_Function, r: TB_Reg) -> TB_Reg ---
tb_node_load_get_address :: proc(f: ^TB_Function, r: TB_Reg) -> TB_Reg ---
tb_node_arith_get_left :: proc(f: ^TB_Function, r: TB_Reg) -> TB_Reg ---
tb_node_arith_get_right :: proc(f: ^TB_Function, r: TB_Reg) -> TB_Reg ---
tb_archive_parse_lib :: proc(file: TB_Slice) -> ^TB_ArchiveFile ---
tb_archive_free :: proc(archive: ^TB_ArchiveFile) -> void ---
tb_object_parse_coff :: proc(file: TB_Slice) -> ^TB_ObjectFile ---
tb_object_free :: proc(obj: ^TB_ObjectFile) -> void ---
