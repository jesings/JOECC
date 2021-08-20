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
} GVNNUM;

typedef struct {
  enum opcode_3ac o;
  unsigned int p1;
  unsigned int p2;
  short size1; //bottom 4 bits size, 5th bit sign
  short size2; //bottom 4 bits size, 5th bit sign
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
  return (adt & 0xf) | (adt & ISSIGNED ? 0x10 : 0) | (adt & ISFLOAT ? 0x20 : 0);
}
static inline ADDRTYPE downsize(char super) {
  return (super & 0xf) | (super & 0x10 ? ISSIGNED : 0) | (super & 0x20 ? ISFLOAT : 0);
}
static inline VALUESTRUCT* ctvalstruct(enum opcode_3ac o, unsigned int p1, unsigned int p2, char size1, char size2) {
  VALUESTRUCT* irval = malloc(sizeof(VALUESTRUCT));
  irval->o = o;
  irval->p1 = p1;
  irval->p2 = p2;
  irval->size1 = size1;
  irval->size2 = size2;
  return irval;
}
#endif
