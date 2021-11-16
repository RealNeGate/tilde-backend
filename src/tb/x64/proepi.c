
size_t x64_get_prologue_length(uint64_t saved, uint64_t stack_usage) {
	// If the stack usage is zero we don't need a prologue
	if (stack_usage == 8) return 0;
	
	bool is_stack_usage_imm8 = (stack_usage == (int8_t)stack_usage);
	
	// takes one byte to save the lower 8 GPRs, two for the top 8
	return (is_stack_usage_imm8 ? 4 : 7) + 4
		+ (__builtin_popcount(saved & 0xFF00) * 2) 
		+ __builtin_popcount(saved & 0x00FF);
}

size_t x64_get_epilogue_length(uint64_t saved, uint64_t stack_usage) {
	if (stack_usage == 8) return 1;
	
	bool is_stack_usage_imm8 = (stack_usage == (int8_t)stack_usage);
	
	// takes one byte to restore the lower 8 GPRs, two for the top 8
	return (is_stack_usage_imm8 ? 5 : 8) + 1 
		+ (__builtin_popcount(saved & 0xFF00) * 2) 
		+ __builtin_popcount(saved & 0x00FF);
}

size_t x64_emit_prologue(char out[64], uint64_t saved, uint64_t stack_usage) {
	// If the stack usage is zero we don't need a prologue
	if (stack_usage == 8) return 0;
	
	size_t used = 0;
	out[used++] = 0x50 + RBP;
	
	for (size_t i = 0; i < 16; i++) if (saved & (1ull << i)) {
		if (i < 8) {
			out[used++] = 0x50 + i;
		} else {
			out[used++] = 0x41;
			out[used++] = 0x50 + (i & 0b111);
		}
	}
	
	out[used++] = rex(true, RSP, RBP, 0);
	out[used++] = 0x89;
	out[used++] = mod_rx_rm(MOD_DIRECT, RSP, RBP);
	
	if (stack_usage == (int8_t)stack_usage) {
		out[used++] = rex(true, 0x00, RSP, 0);
		out[used++] = 0x83;
		out[used++] = mod_rx_rm(MOD_DIRECT, 0x05, RSP);
		out[used++] = (int8_t)stack_usage;
	} else {
		out[used++] = rex(true, 0x00, RSP, 0);
		out[used++] = 0x81;
		out[used++] = mod_rx_rm(MOD_DIRECT, 0x05, RSP);
		*((uint32_t*)&out[used]) = stack_usage;
		used += 4;
	}
	
	return used;
}

size_t x64_emit_epilogue(char out[64], uint64_t saved, uint64_t stack_usage) {
	// if the stack isn't used then just return
	if (stack_usage == 8) {
		out[0] = 0xC3;
		return 1;
	}
	
	size_t used = 0;
	
	if (stack_usage == (int8_t)stack_usage) {
		out[used++] = rex(true, 0x00, RSP, 0);
		out[used++] = 0x83;
		out[used++] = mod_rx_rm(MOD_DIRECT, 0x00, RSP);
		out[used++] = (int8_t)stack_usage;
	} else {
		out[used++] = rex(true, 0x00, RSP, 0);
		out[used++] = 0x81;
		out[used++] = mod_rx_rm(MOD_DIRECT, 0x00, RSP);
		*((uint32_t*)&out[used]) = stack_usage;
		used += 4;
	}
	
	for (size_t i = 16; i--;) if (saved & (1ull << i)) {
		if (i < 8) {
			out[used++] = 0x58 + i;
		} else {
			out[used++] = 0x41;
			out[used++] = 0x58 + (i & 0b111);
		}
	}
	
	out[used++] = 0x58 + RBP;
	
	out[used++] = 0xC3;
	return used;
}
