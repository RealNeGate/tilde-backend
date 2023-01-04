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
CFLAGS = -g -I include -I deps/luajit/src -msse4.2 -Wall -Werror -Wno-unused-function -D_CRT_SECURE_NO_WARNINGS

# linker
LD = clang -fuse-ld=lld
LDFLAGS = -g

# utils
RM = rm -f

SOURCES  := src/tb/tb.c
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
ifeq ($(OS),Windows_NT)
clean:
	del /F /Q $(subst /,\,$(OBJECTS))
else
clean:
	rm $(objects)
endif
