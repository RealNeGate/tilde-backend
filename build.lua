
local files = {
    "src/tb/",
    "src/tb/codegen/",
    "src/tb/bigint/",
    "src/tb/objects/",
    "src/tb/debug/",
    "src/tb/debug/cv/",
    "src/tb/x64/",
    "src/tb/aarch64/",
    "src/tb/opt/",
}

if config.os == "Windows" then
    table.insert(files, "src/tb/system/win32.c")
else
    table.insert(files, "src/tb/system/posix.c")
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
    build.lib("tildebackend.lib", "deps/mimalloc-static.lib deps/luajit/src/lua51.lib", outputs)
end

return changes
