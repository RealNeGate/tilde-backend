
local files = {
    "src/tb/",
    "src/tb/codegen/",
    "src/tb/bigint/",
    "src/tb/objects/",
    "src/tb/debug/",
    "src/tb/debug/cv/",
    "src/tb/x64/",
    "src/tb/aarch64/",
    "src/tb/opt/"
}

local deps = ""
local output_lib = ""
if config.os == "Windows" then
    build.command("build_deps.bat")
    table.insert(files, "src/tb/system/win32.c")
    
    deps = "deps/luajit/src/lua51.lib"
    output_lib = "tildebackend.lib"
else
    build.command("./build_deps.sh")
    table.insert(files, "src/tb/system/posix.c")
    
    deps = "bin/luajit/*.o"
    output_lib = "tildebackend.a"
end

local options = "-g -Wall -Werror -Wno-unused-function "
options = options.."-I include "
options = options.."-I deps/mimalloc/include "
options = options.."-I deps/luajit/src "
if config.opt then
    options = options.."-O2 -DNDEBUG "
end

local outputs, changes = build.compile("TB.cache", files, options)
if changes then
    build.lib(output_lib, deps, outputs)
end

return changes
