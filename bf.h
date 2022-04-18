#ifndef BF_C
#define BF_C

//This type is a simple definition of a bitfield using macros
//BITFIELD type is defined to be char*
#define BITFIELD char*
#define bfalloc(length) calloc(1, ((length) + 7) >> 3) //allocate bitfield of given number of bits
#define bfclone(bitfield, length) memcpy(malloc(((length) + 7) >> 3), (bitfield), ((length) + 7) >> 3) //clone existing bitfield of given number of bits
#define bfzero(bitfield, length) memset(bitfield, 0, ((length) + 7) >> 3) //zero out bitfield of given number of bits
#define bfget(bitfield, index) ((bitfield)[(index) >> 3] & (1 << ((index) & 7))) //get nth bit value from bitfield (to be handled as a true/false value, not guaranteed to be 0 or 1, could be 0x2, 0x4, 0x8, 0x10 etc.)
#define bfset(bitfield, index) ((bitfield)[(index) >> 3] |= (1 << ((index) & 7))) //sets nth bit value of bitfield
#define bfunset(bitfield, index) ((bitfield)[(index) >> 3] &= ~(1 << ((index) & 7))) //unsets nth bit value of bitfield
#endif
