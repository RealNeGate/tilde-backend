
#if defined(_MSC_VER)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static_assert(sizeof(int) == sizeof(long), "expected LLP64");

int tb_atomic_int_load(int* dst) {
	return InterlockedOr((long*)dst, 0);
}

int tb_atomic_int_add(int* dst, int src) {
	return InterlockedExchangeAdd((long*)dst, src);
}

int tb_atomic_int_store(int* dst, int src) {
	return InterlockedExchange((long*)dst, src);
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
#elif defined(__GNUC__)
#include <stddef.h>

int tb_atomic_int_load(int* dst) {
	return __atomic_load_n(dst, __ATOMIC_SEQ_CST);
}

int tb_atomic_int_add(int* dst, int src) {
	return __atomic_fetch_add(dst, src, __ATOMIC_SEQ_CST);
}

int tb_atomic_int_store(int* dst, int src) {
	return __atomic_exchange_n(dst, src, __ATOMIC_SEQ_CST);
}

size_t tb_atomic_size_load(size_t* dst) {
	return __atomic_load_n(dst, __ATOMIC_SEQ_CST);
}

size_t tb_atomic_size_add(size_t* dst, size_t src) {
	return __atomic_fetch_add(dst, src, __ATOMIC_SEQ_CST);
}

size_t tb_atomic_size_store(size_t* dst, size_t src) {
	return __atomic_exchange_n(dst, src, __ATOMIC_SEQ_CST);
}

#else
#error "Implement atomics for this platform"
#endif
