# file extensions
ifeq ($(OS),Windows_NT)
	O = .obj
	X = .lib
else
	O = .o
	X = .a
endif

# C compiler
CC = clang
CFLAGS = -g -I include -msse4.2 -Wall -Werror -Wno-unused-function -D_CRT_SECURE_NO_WARNINGS

# linker
LD = clang -fuse-ld=lld
LDFLAGS = -g

SOURCE_PATTERN :=       \
	src/tb/*.c          \
	src/tb/codegen/*.c  \
	src/tb/bigint/*.c   \
	src/tb/objects/*.c  \
	src/tb/system/*.c   \
	src/tb/debug/*.c    \
	src/tb/debug/cv/*.c \
	src/tb/opt/*.c      \
	src/tb/x64/*.c      \
	src/tb/aarch64/*.c  \
	src/tb/wasm/*.c     \

SOURCES  := $(wildcard $(SOURCE_PATTERN))
OBJECTS  := $(SOURCES:%.c=%.o)

# Convert into library
all: tildebackend$(X)

ifeq ($(OS),Windows_NT)
tildebackend$(X): $(OBJECTS)
	lib /nologo /out:$@ $^
else
tildebackend$(X): $(OBJECTS)
	ar -rcs $@ $^
endif

# C source code
%.o: %.c
	$(CC) $(CFLAGS) $< -c -o $@

.PHONY: clean
clean:
	rm -f $(OBJECTS)
