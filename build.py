import glob
import os
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles TB')
parser.add_argument('targets', metavar='N', type=str, nargs='+', help='decide which targets to compile with')

args = parser.parse_args()
source_patterns = [
	"src/tb/*.c",
	"src/tb/codegen/*.c",
	"src/tb/bigint/*.c",
	"src/tb/objects/*.c",
	"src/tb/system/*.c",
	"src/tb/debug/*.c",
	"src/tb/debug/cv/*.c",
	"src/tb/opt/*.c"
]

for i in args.targets:
	source_patterns.append("src/tb/"+i+"/*.c")

ninja = open('build.ninja', 'w')
cflags = "-I include -I deps/luajit/src -Wall -Werror -Wno-unused-function"

os_name = platform.system()
if os_name == "Windows":
	cflags += " -D_CRT_SECURE_NO_WARNINGS"

# configure architecture-specific targeting
if platform.machine() == "AMD64":
	cflags += " -msse4.2"

# write out config
ninja.write(f"""
cflags = {cflags}
""")

# write some rules
ninja.write("""
rule cc
  depfile = $out.d
  command = clang $in $cflags -MD -MF $out.d -c -o $out
  description = cc $in $out

""")

if os_name == "Windows":
	ninja.write("""
rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	ninja.write("""
rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

# compile TB
objs = []
for pattern in source_patterns:
	list = glob.glob(pattern)
	for f in list:
		obj = os.path.basename(f).replace('.c', '.o')
		ninja.write(f"build bin/{obj}: cc {f}\n")
		objs.append("bin/"+obj)

ninja.write("build tildebackend.lib: lib " + " ".join(objs) + "\n")
ninja.close()

# run ninja
subprocess.check_call(['ninja'])
