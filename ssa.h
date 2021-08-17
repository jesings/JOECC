#ifndef SSA_H
#define SSA_H
#include "3ac.h"

typedef struct {
  DYNARR* equivs;
  union {
    long intconst;
    double floatconst;
    char* strconst;
  };
  enum {NOCONST, INTCONST, FLOATCONST, STRCONST} hasconst;
  int index;
  ADDRTYPE commontype;
} GVNNUM;

typedef struct {
  enum opcode_3ac o;
  char size1; //bottom 4 bits size, 5th bit sign
  char size2; //bottom 4 bits size, 5th bit sign
  unsigned int p1;
  unsigned int p2;
} VALUESTRUCT;


typedef struct {
  DYNARR* nodes;//of GVNNUMS
  HASHTABLE* intconsthash;
  HASHTABLE* floatconsthash;
  HASHTABLE* strconsthash;
  BIGHASHTABLE* ophash;
} EQONTAINER;

void ssa(PROGRAM* prog);
void gvn(PROGRAM* prog);
void ssaout(PROGRAM* prog);
BBLOCK* intersect(BBLOCK* n1, BBLOCK* n2);
char fixedintersect(const BBLOCK* fb, BBLOCK* gb);
void annotateuse(PROGRAM* prog);
void killreg(PROGRAM* prog);
#define bfalloc(length) calloc(1, ((length) + 7) >> 3)
#define bfclone(bitfield, length) memcpy(malloc(((length) + 7) >> 3), (bitfield), ((length) + 7) >> 3)
#define bfget(bitfield, index) ((bitfield)[(index) >> 3] & (1 << ((index) & 7)))
#define bfset(bitfield, index) ((bitfield)[(index) >> 3] |= (1 << ((index) & 7)))
#define bfunset(bitfield, index) ((bitfield)[(index) >> 3] &= ~(1 << ((index) & 7)))
#define BITFIELD char*

static inline char supersize(ADDRTYPE adt) {
  return (adt & 0xf) | (adt & ISSIGNED ? 0x10 : 0);
}
static inline VALUESTRUCT* ctvalstruct(enum opcode_3ac o, char size1, char size2, unsigned int p1, unsigned int p2) {
  VALUESTRUCT* irval = malloc(sizeof(VALUESTRUCT));
  irval->o = o;
  irval->size1 = size1;
  irval->size2 = size2;
  irval->p1 = p1;
  irval->p2 = p2;
  return irval;
}
#endif
