
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

int tb_atomic_int_load(int* dst) {
	return InterlockedOr(dst, 0);
}

int tb_atomic_int_add(int* dst, int src) {
	return InterlockedExchangeAdd(dst, src);
}

int tb_atomic_int_store(int* dst, int src) {
	return InterlockedExchange(dst, src);
}

size_t tb_atomic_size_load(size_t* dst) {
	return InterlockedOr64((LONG64*)dst, 0);
}

size_t tb_atomic_size_add(size_t* dst, size_t src) {
	return InterlockedExchangeAdd64((LONG64*)dst, src);
}

size_t tb_atomic_size_store(size_t* dst, size_t src) {
	return InterlockedExchange64((LONG64*)dst, src);
}
#else
#error "Implement atomics for this platform"
#endif
