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
  int regno; //-1 if no regno
} GVNNUM;

typedef struct {
  enum opcode_3ac o;
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
#define bfunset(bitfield, index) ((bitfield)[(index) >> 3] & ~(1 << ((index) & 7)))
#define BITFIELD char*

static inline VALUESTRUCT* ctvalstruct(enum opcode_3ac o, unsigned int p1, unsigned int p2) {
  VALUESTRUCT* irval = malloc(sizeof(VALUESTRUCT));
  irval->o = o;
  irval->p1 = p1;
  irval->p2 = p2;
  return irval;
}
#endif
