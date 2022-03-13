#ifndef BF_C
#define BF_C

#define bfalloc(length) calloc(1, ((length) + 7) >> 3)
#define bfclone(bitfield, length) memcpy(malloc(((length) + 7) >> 3), (bitfield), ((length) + 7) >> 3)
#define bfzero(bitfield, length) memset(bitfield, 0, ((length) + 7) >> 3)
#define bfget(bitfield, index) ((bitfield)[(index) >> 3] & (1 << ((index) & 7)))
#define bfset(bitfield, index) ((bitfield)[(index) >> 3] |= (1 << ((index) & 7)))
#define bfunset(bitfield, index) ((bitfield)[(index) >> 3] &= ~(1 << ((index) & 7)))
#define BITFIELD char*
#endif
