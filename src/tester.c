#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "tb/tb.h"
#include <x86intrin.h>

enum {
    TOKEN_ACCESSOR = '.',
    
    TOKEN_ADDITION = '+',
    TOKEN_SUBTRACT = '-',
    TOKEN_MULTIPLY = '*',
    TOKEN_DIVISION = '/',
    TOKEN_ASSIGN = '=',
    
    TOKEN_PAREN_OPEN = '(',
    TOKEN_PAREN_CLOSE = ')',
    
    TOKEN_BRACE_OPEN = '{',
    TOKEN_BRACE_CLOSE = '}',
    
    TOKEN_IDENTIFIER = 256,
    
    TOKEN_INVALID,
    TOKEN_STRING,
    
    TOKEN_EQUALITY,   /* == */
    TOKEN_FAT_ARROW,  /* => */
};

typedef struct Lexer {
    const char* current;
    
    // current token info
    int token_type;
    const char* token_start;
    const char* token_end;
} Lexer;

static _Alignas(64) uint8_t identifier_char_tbl[] = {
	0x00,0x00,0x00,0x00,
	0x10,0x00,0xff,0x03,
	0xfe,0xff,0xff,0x87,
	0xfe,0xff,0xff,0x07,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00
};

__attribute__((always_inline))
static bool is_identifier_not_first_char(char ch) {
	size_t i = ch;
	size_t index = i / 8;
	size_t shift = i & 7;
	return (identifier_char_tbl[index] >> shift) & 1;
}

static _Alignas(64) uint8_t space_char_tbl[] = {
	0x00,0x26,0x00,0x00,
	0x01,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00
};

__attribute__((always_inline))
static bool is_space(char ch) {
	size_t i = ch;
	size_t index = i / 8;
	size_t shift = i & 7;
	return (space_char_tbl[index] >> shift) & 1;
}

static void read(Lexer* restrict l) {
    const char* current = l->current;
    
    // branchless space skip
    current += (*current == ' ');
    
    redo_lex:;
    const char* start = current;
    switch (*start) {
        case '\0':
        l->token_type = '\0';
        break;
        case '\r':
        current++;
        
        // it's expected these are next to each other because
        // Windows, fast path a fallthrough
        if (*current != '\n') goto redo_lex;
        case '\n': {
			current++;
			
            // Do a branchless SIMD skip of up to 16 indentation spaces after a newline.
            __m128i chars = _mm_loadu_si128((__m128i *)current);
            int len = __builtin_ffs(~_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8(' '))));
            current += len;
            goto redo_lex;
        }
        case ' ':
        case '\t':
        case '\v':
        // slow path
        do {
            current++;
        } while (is_space(*current));
        goto redo_lex;
        case '\"':
        current++;
		
        do {
            __m128i chars = _mm_loadu_si128((__m128i *)current);
            int len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('\"'))));
            
            if (len) {
                current += len;
                if (current[-1] == '\"' && current[-2] != '\\') break;
            } else {
                current += 16;
            }
        } while (*current);
        
        l->token_type = TOKEN_STRING;
        break;
        case '=':
        current++;
        if (*current == '>') {
            current++;
            l->token_type = TOKEN_FAT_ARROW;
            break;
        }
        
        current += (*current == '=');
        current += (*current == '=');
        
        l->token_type = (current-start) == 2 ? TOKEN_EQUALITY : TOKEN_ASSIGN;
        break;
        case '#':
        do {
            current++;
        } while (*current != '\0' && *current != '\n');
        break;
        case '+':
        case '-':
        case '/':
        case '*':
        case '.':
        case '(':
        case ')':
        case '{':
        case '}':
        current++;
        
        l->token_type = *start;
        break;
        case 'A' ... 'Z':
        case 'a' ... 'z':
        case '_': case '$': {
            __builtin_prefetch(identifier_char_tbl);
            do {
                current++;
            } while (is_identifier_not_first_char(*((unsigned char*)current)));
			
            l->token_type = TOKEN_IDENTIFIER;
            break;
        }
        default: 
        l->token_type = TOKEN_INVALID;
        abort();
        break;
    }
    
    l->token_start = start;
    l->token_end = current;
    l->current = current;
}

static char* newstr(Lexer* restrict l) {
    size_t len = l->token_end - l->token_start;
    char* dst = malloc(len + 1);
    
    memcpy(dst, l->token_start, len);
    dst[len] = '\0';
    
    read(l);
    return dst;
}

static bool match(Lexer* restrict l, const char* t) {
    size_t str_len = strlen(t);
    size_t token_len = l->token_end - l->token_start;
    
    return str_len == token_len &&
        memcmp(l->token_start, t, token_len) == 0;
}

static int def_count = 0;
static char* def_names[256];
static TB_DataType defs[256];

static TB_Function* current_fn;

static TB_DataType parse_data_type(Lexer* l) {
    if (l->token_type != '[') abort();
    
    read(l);
    if (l->token_type != TOKEN_IDENTIFIER) abort();
    
    if (match(l, "i8")) {
        return TB_TYPE_I8(1);
    } else if (match(l, "i16")) {
        return TB_TYPE_I16(1);
    } else if (match(l, "i32")) {
        return TB_TYPE_I32(1);
    } else if (match(l, "i64")) {
        return TB_TYPE_I64(1);
    } else if (match(l, "i128")) {
        return TB_TYPE_I128(1);
    } else if (match(l, "f32")) {
        return TB_TYPE_F32(1);
    } else if (match(l, "f64")) {
        return TB_TYPE_F64(1);
    } else if (match(l, "bool")) {
        return TB_TYPE_I32(1);
    } else if (match(l, "ptr")) {
        return TB_TYPE_PTR();
    } else {
        int i = def_count;
        while (i--) {
            if (match(l, def_names[i])) return defs[i];
        }
        
        abort();
    }
}

int main(int argc, char** argv) {
    if (argc == 1) {
        printf("No input files!\n");
        return 1;
    }
    
    //
    // Read file
    //
    char *text;
    {
        FILE *f = fopen(argv[1],"rb");
        text = (char *) malloc(1 << 20);
        int len = f ? (int) fread(text, 1, 1<<20, f) : -1;
        text[len] = '\0';
        
        if (len < 0) {
            printf("Error opening file\n");
            free(text);
            fclose(f);
            return 1;
        }
        fclose(f);
    }
    
	TB_FeatureSet features = { 0 };
    TB_Module* m = tb_module_create(TB_ARCH_X86_64,
                                    TB_SYSTEM_WINDOWS,
                                    &features,
                                    TB_OPT_O0, 
                                    1, true);
    
    //
    // Parse file
    //
    Lexer l = { .current = text };
    while (true) {
        // either def, fn or EOF
        read(&l);
        if (l.token_type == '\0') break;
        else if (l.token_type == TOKEN_IDENTIFIER) {
            if (match(&l, "fn")) {
                read(&l);
                
                TB_DataType dt = parse_data_type(&l);
                char* name = newstr(&l);
                
                current_fn = tb_function_create(m, name, dt);
                free(name); // no longer needed
                
                
            } else if (match(&l, "def")) {
                read(&l);
                
                TB_DataType dt = parse_data_type(&l);
                
                int i = def_count++;
                def_names[i] = newstr(&l);
                defs[i] = dt;
            } else {
                abort();
            }
        }
    }
    
    //
    // Export
    //
    {
        tb_module_compile(m);
        
        FILE* f = fopen("./test_x64.obj", "wb");
        tb_module_export(m, f);
        fclose(f);
	}
    
	return 0;
}
