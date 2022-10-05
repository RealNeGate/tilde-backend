#pragma once
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#define SET_INITIAL_CAP 32

typedef struct Set {
    size_t capacity;
    uint64_t* data;
} Set;

inline static Set set_create(size_t cap) {
    return (Set){
        .capacity = cap,
        .data = calloc((cap + 63) / 64, sizeof(uint64_t)),
    };
}

inline static void set_free(Set* s) {
    free(s->data);
}

inline static void set_clear(Set* s) {
    memset(s->data, 0, s->capacity * sizeof(uint64_t));
}

inline static bool set_first_time(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot >= s->capacity) {
        size_t old = s->capacity;

        s->capacity = d.quot * 2;
        s->data = realloc(s->data, s->capacity * sizeof(uint64_t));
        if (s->data == NULL) {
            fprintf(stderr, "TB error: Set out of memory!");
            abort();
        }

        memset(s->data + old, 0, (s->capacity - old) * sizeof(uint64_t));
    }

    if ((s->data[d.quot] & (1ull << d.rem)) == 0) {
        s->data[d.quot] |= (1ull << d.rem);
        return true;
    }

    return false;
}

inline static void set_put(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot >= s->capacity) {
        size_t old = s->capacity;

        s->capacity = d.quot * 2;
        s->data = realloc(s->data, s->capacity * sizeof(uint64_t));
        if (s->data == NULL) {
            fprintf(stderr, "TB error: Set out of memory!");
            abort();
        }

        memset(s->data + old, 0, (s->capacity - old) * sizeof(uint64_t));
    }

    s->data[d.quot] |= (1ull << d.rem);
}

inline static void set_remove(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot < s->capacity) {
        s->data[d.quot] &= ~(1ull << d.rem);
    }
}

inline static bool set_get(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot >= s->capacity) {
        return false;
    }

    return s->data[d.quot] & (1ull << d.rem);
}
