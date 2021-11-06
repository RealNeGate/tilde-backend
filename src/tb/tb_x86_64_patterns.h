#pragma once
// This file is specifically just to store all the instruction selection patterns

// NOTE(NeGate): LEA addition only works in NO_WRAP and CAN_WRAP
static const X64_ISel_Pattern IADD_PATTERNS[] = {
    { 1, "rrr", (uint8_t[]){ X64_LEA, 0, '[', 1, 2, 0x7F }, false, true },
    { 1, "rri", (uint8_t[]){ X64_LEA, 0, '[', 1, 2, 0x7F }, false, true },
    { 2, "rrr", (uint8_t[]){ X64_ADD, 0, 2, 0x7F }, true },
    { 2, "rri", (uint8_t[]){ X64_ADD, 0, 2, 0x7F }, true },
    { 3, "rrm", (uint8_t[]){ X64_ADD, 0, 2, 0x7F }, true },
    { 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, false },
    { 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, false },
    { 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, false },
	
	// This case is a fallback and should never actually be used
	{ 10, "rii", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, true }
};

static const X64_ISel_Pattern ISUB_PATTERNS[] = {
	{ 1, "rrr", (uint8_t[]){ X64_SUB, 0, 2, 0x7F }, true },
	{ 2, "rri", (uint8_t[]){ X64_SUB, 0, 2, 0x7F }, true },
	{ 3, "rir", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, true },
	{ 3, "rrm", (uint8_t[]){ X64_SUB, 0, 2, 0x7F }, true },
	{ 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, false },
	{ 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, false },
	{ 5, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, false },
	{ 5, "rim", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, true },
	
	// This case is a fallback and should never actually be used
	{ 10, "rii", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, true }
};

static const X64_ISel_Pattern AND_PATTERNS[] = {
	{ 1, "rrr", (uint8_t[]){ X64_AND, 0, 2, 0x7F }, true },
	{ 2, "rri", (uint8_t[]){ X64_AND, 0, 2, 0x7F }, true },
	{ 3, "rrm", (uint8_t[]){ X64_AND, 0, 2, 0x7F }, true },
	{ 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_AND, 0, 2, 0x7F }, false },
	{ 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_AND, 0, 2, 0x7F }, false },
	{ 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_AND, 0, 2, 0x7F }, false },
	
	// This case is a fallback and should never actually be used
	{ 10, "rii", (uint8_t[]){ X64_MOV, 0, 1, X64_AND, 0, 2, 0x7F }, true }
};

static const X64_ISel_Pattern OR_PATTERNS[] = {
	{ 1, "rrr", (uint8_t[]){ X64_OR, 0, 2, 0x7F }, true },
	{ 2, "rri", (uint8_t[]){ X64_OR, 0, 2, 0x7F }, true },
	{ 3, "rrm", (uint8_t[]){ X64_OR, 0, 2, 0x7F }, true },
	{ 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_OR, 0, 2, 0x7F }, false },
	{ 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_OR, 0, 2, 0x7F }, false },
	{ 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_OR, 0, 2, 0x7F }, false },
	
	// This case is a fallback and should never actually be used
	{ 10, "rii", (uint8_t[]){ X64_MOV, 0, 1, X64_OR, 0, 2, 0x7F }, true }
};

static const X64_ISel_Pattern IMUL_PATTERNS[] = {
	{ 1, "rrr", (uint8_t[]){ X64_IMUL, 0, 2, 0x7F }, true },
	{ 2, "rri", (uint8_t[]){ X64_IMUL, 0, 2, 0x7F }, true },
	{ 3, "rrm", (uint8_t[]){ X64_IMUL, 0, 2, 0x7F }, true },
	{ 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_IMUL, 0, 2, 0x7F }, false },
	{ 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_IMUL, 0, 2, 0x7F }, false },
	{ 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_IMUL, 0, 2, 0x7F }, false },
	
	// This case is a fallback and should never actually be used
	{ 10, "rii", (uint8_t[]){ X64_MOV, 0, 1, X64_OR, 0, 2, 0x7F }, true }
};

// NOTE(NeGate): These SSE variants must take into account that they can't
// directly store without using MOVSS
static const X64_ISel_Pattern F32ADD_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_ADDSS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_ADDSS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_ADDSS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32SUB_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_SUBSS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_SUBSS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_SUBSS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32MUL_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_MULSS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_MULSS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_MULSS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32DIV_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_DIVSS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_DIVSS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_DIVSS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32X4ADD_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_ADDPS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_ADDPS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVAPS, 0, 1, X64_ADDPS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32X4SUB_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_SUBPS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_SUBPS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVAPS, 0, 1, X64_SUBPS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32X4MUL_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_MULPS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_MULPS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVAPS, 0, 1, X64_MULPS, 0, 2, 0x7F }, false }
};

static const X64_ISel_Pattern F32X4DIV_PATTERNS[] = {
	{ 1, "xxx", (uint8_t[]){ X64_DIVPS, 0, 2, 0x7F }, true },
	{ 2, "xxm", (uint8_t[]){ X64_DIVPS, 0, 2, 0x7F }, true },
	{ 3, "xmm", (uint8_t[]){ X64_MOVAPS, 0, 1, X64_DIVPS, 0, 2, 0x7F }, false }
};

