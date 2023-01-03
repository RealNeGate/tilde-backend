CC     = clang
CFLAGS = -Wall -Werror -Wno-unused-function -I include -I deps/luajit/src
LDLIBS =

SOURCES  := src/tb/tb.c
OBJECTS  := $(addprefix bin/, $(notdir $(SOURCES:%.c=%.o)))
INCLUDES := include

# Convert into library
all: tildebackend.lib

tildebackend.lib: $(OBJECTS)
	lib /nologo /out:$out $(inputs)

# C source code
%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $(@)
