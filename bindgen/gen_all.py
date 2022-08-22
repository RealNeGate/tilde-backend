import os, gen_zig, gen_odin

tasks = [
    [ '../include/tb.h', 'tb_', [] ],
]

# Odin
gen_odin.prepare()
for task in tasks:
    c_header_path = task[0]
    main_prefix = task[1]
    dep_prefixes = task[2]
    gen_odin.gen(c_header_path, main_prefix, dep_prefixes)

# Zig
gen_zig.prepare()
for task in tasks:
    c_header_path = task[0]
    main_prefix = task[1]
    dep_prefixes = task[2]
    gen_zig.gen(c_header_path, main_prefix, dep_prefixes)
