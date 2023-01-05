CC  = clang
OPT ?= 0

# file extensions
ifeq ($(OS),Windows_NT)
	X = .lib
else
	X = .a
endif

# C compiler
CFLAGS = -g -O$(OPT) -I include -msse4.2 -Wall -Werror -Wno-unused-function -D_CRT_SECURE_NO_WARNINGS

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
%.o: %.c Makefile
	$(CC) $(CFLAGS) $< -c -o $@

.PHONY: clean
ifeq ($(OS),Windows_NT)
clean:
	: del /F /Q $(subst /,\,$(OBJECTS)) tildebackend$(X)
	: del /F /Q tb.tar.gz
else
clean:
	: rm -f $(OBJECTS) tildebackend$(X)
	: rm -f tb.tar.gz
endif
