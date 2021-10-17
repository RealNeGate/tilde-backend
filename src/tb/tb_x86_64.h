#pragma once

#define TB_INTERNAL
#include "tb.h"

#define GPR_NONE 0xFF

typedef enum X64_Cond {
	X64_O, X64_NO, X64_B, X64, X64_E, X64_NE, X64_BE, X64_A,
	X64_S, X64_NS, X64_P, X64_NP, X64_L, X64_GE, X64_LE, X64_G,
} X64_Cond;

typedef enum X64_GPR {
	X64_RAX, X64_RCX, X64_RDX, X64_RBX, X64_RSP, X64_RBP, X64_RSI, X64_RDI,
	X64_R8, X64_R9, X64_R10, X64_R11, X64_R12, X64_R13, X64_R14, X64_R15,
    
    X64_GPR_NONE = -1
} X64_GPR;

typedef enum X64_XMM {
	X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3, X64_XMM4, X64_XMM5, X64_XMM6, X64_XMM7,  
    X64_XMM8, X64_XMM9, X64_XMM10, X64_XMM11, X64_XMM12, X64_XMM13, X64_XMM14, X64_XMM15  
} X64_XMM;

inline static uint8_t x64_inst_mod_rx_rm(uint8_t mod, uint8_t rx, uint8_t rm) {
	return ((mod & 3) << 6) | ((rx & 7) << 3) | (rm & 7);
}

inline static uint8_t x64_inst_rex(bool is_64bit, uint8_t rx, uint8_t base, uint8_t index) {
	return 0x40 | (is_64bit ? 8 : 0) | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2);
}
