#pragma once
// This file is specifically just to store all the instruction selection patterns

static const X64_IselInfo* SELECTION_IADD = &(X64_IselInfo){
	.inst = X64_ADD,
	.has_immediates = true,
	.communitive = true
};

static const X64_IselInfo* SELECTION_ISUB = &(X64_IselInfo){
	.inst = X64_SUB,
	.has_immediates = true
};

static const X64_IselInfo* SELECTION_IMUL = &(X64_IselInfo){
	.inst = X64_IMUL,
	.communitive = true
};

static const X64_IselInfo* SELECTION_AND = &(X64_IselInfo){
	.inst = X64_AND,
	.communitive = true,
	.has_immediates = true
};

static const X64_IselInfo* SELECTION_OR = &(X64_IselInfo){
	.inst = X64_OR,
	.communitive = true,
	.has_immediates = true
};

static const X64_IselInfo* SELECTION_F32_ADD = &(X64_IselInfo){
	.inst = X64_ADDSS,
	.communitive = true
};

static const X64_IselInfo* SELECTION_F32_SUB = &(X64_IselInfo){
	.inst = X64_SUBSS
};

static const X64_IselInfo* SELECTION_F32_MUL = &(X64_IselInfo){
	.inst = X64_MULSS,
	.communitive = true
};

static const X64_IselInfo* SELECTION_F32_DIV = &(X64_IselInfo){
	.inst = X64_DIVSS
};

static const X64_IselInfo* SELECTION_F32X4_ADD = &(X64_IselInfo){
	.inst = X64_ADDPS,
	.communitive = true
};

static const X64_IselInfo* SELECTION_F32X4_SUB = &(X64_IselInfo){
	.inst = X64_SUBPS
};

static const X64_IselInfo* SELECTION_F32X4_MUL = &(X64_IselInfo){
	.inst = X64_MULPS,
	.communitive = true
};

static const X64_IselInfo* SELECTION_F32X4_DIV = &(X64_IselInfo){
	.inst = X64_DIVPS
};

