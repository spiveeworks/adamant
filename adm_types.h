/* Adamant types -  b/c/u/s/f types in the style of Adamant, itself inspired by
                        Rust, llvm, JAI, etc. */
#ifndef ADM_INCLUDE_ADM_TYPES_H
#define ADM_INCLUDE_ADM_TYPES_H
#include <stdint.h>

typedef uint8_t b8;
typedef uint16_t b16;
typedef uint32_t b32;
typedef uint64_t b64;
typedef uintptr_t bxx;

typedef b8 c8;
typedef b16 c16;
typedef b32 c32;
typedef b64 c64;
typedef bxx cxx;

typedef b8 u8;
typedef b16 u16;
typedef b32 u32;
typedef b64 u64;
typedef bxx uxx;

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;
typedef intptr_t sxx;

typedef float f32;
typedef double f64;

#endif
