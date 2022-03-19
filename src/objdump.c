#include "tb/tb.h"

// width is just how big this stream can get, if length < width then we
// wanna pad things out
static void dump_raw_data_as_ascii(size_t width, char out[], size_t length, const uint8_t data[]) {
	for (size_t i = 0; i < length; i++) {
		if (data[i] >= 32 && data[i] < 128) {
			out[i] = data[i];
		} else {
			out[i] = '.';
		}
	}
	
	for (size_t i = length; i < width; i++) {
		out[i] = ' ';
	}
}

static void dump_raw_data(size_t length, const uint8_t* data) {
	char ascii_temp_str[16];
	printf("  %08llx: ", 0ll);
	
	for (size_t i = 0; i < length; i++) {
		printf("%02x ", data[i]);
		if (i && i % 16 == 15) {
			printf("\t");
			dump_raw_data_as_ascii(16, ascii_temp_str, 16, &data[i & ~15]);
			printf("%.*s\n  %08llx: ", 16, ascii_temp_str, i);
		}
	}
	
	size_t leftover = 15 - (length & 15);
	for (size_t i = 0; i < leftover; i++) {
		printf("   ");
	}
	printf("\t");
	dump_raw_data_as_ascii(16, ascii_temp_str, leftover, &data[length - leftover]);
	printf("%.*s\n", 16, ascii_temp_str);
}

int main(int argc, char** argv) {
	FILE* file = fopen("./crc32_test.obj", "rb");
	TB_ObjectFile* obj = tb_object_parse_coff(file);
	
	for (size_t i = 0; i < obj->section_count; i++) {
		TB_ObjectSection* sec = &obj->sections[i];
		
		printf("%s\n", sec->name);
		dump_raw_data(sec->raw_data_size, sec->raw_data);
		printf("\n");
	}
	
	printf("SYMBOL TABLE\n\n");
	
	for (size_t i = 0; i < obj->symbol_count; i++) {
		TB_ObjectSymbol* sym = &obj->symbols[i];
		printf("  %s\n", sym->name);
	}
	
	printf("\nSUMMARY\n\n");
	for (size_t i = 0; i < obj->section_count; i++) {
		TB_ObjectSection* sec = &obj->sections[i];
		printf("  %8zx %s\n", sec->raw_data_size, sec->name);
	}
	printf("\n");
	
	fclose(file);
	return 0;
}
