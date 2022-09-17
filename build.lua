
local files = {
    "src/tb/*.c",
    "src/tb/codegen/*.c",
    "src/tb/bigint/*.c",
    "src/tb/objects/*.c",
    "src/tb/debug/*.c",
    "src/tb/debug/cv/*.c",
    "src/tb/x64/*.c",
    "src/tb/aarch64/*.c",
    "src/tb/opt/*.c",

    -- platform support code
    "src/tb/system/"..(config.os == "Windows" and "win32.c" or "posix.c")
}

local luajit_binary = ""
local output_lib = ""
if config.os == "Windows" then
    build.command("build_deps.bat")
    luajit_binary = "deps/luajit/src/lua51.lib"
else
    build.command("./build_deps.sh")
    luajit_binary = "bin/luajit/*.o"
end

flags = "-g -msse4.2 -maes "
flags = flags.."-Wall -Werror -Wno-unused-function -Wno-unused-variable "
flags = flags.."-I include -I deps -I tilde-backend/include -I deps/luajit/src "
flags = flags.."-DTB_COMPILE_TESTS "

if config.opt then
    flags = flags.."-O2 -DNDEBUG "
end

-- for k,v in pairs(files) do print("["..k.."] = "..v) end

local objs = build.foreach_chain(files, "clang %f "..flags.." -c -o bin/%F.o", "bin/%F.o")
table.insert(objs, luajit_binary)

build.ar_chain(objs, "tildebackend"..config.lib_ext)
build.done()
