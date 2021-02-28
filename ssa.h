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
} EQNODE;

typedef struct {
  enum opcode_3ac op; //if INIT_3 (or PARAM_3) then it's a regno
  union {
    struct {
      EQNODE* arg0;
      EQNODE* arg1;
    };
    unsigned int regno;
  };
} EQITEM;


typedef struct {
  DYNARR* nodes;//of EQNODES
  EQNODE** varnodes;
  HASHTABLE* intconsthash;
  HASHTABLE* floatconsthash;
  HASHTABLE* strconsthash;
  DYNARR* opnodes;//one entry for every operation
} EQONTAINER;

void ssa(PROGRAM* prog);
void gvn(PROGRAM* prog);
BBLOCK* intersect(BBLOCK* n1, BBLOCK* n2);
char fixedintersect(const BBLOCK* fb, BBLOCK* gb);
#define bfalloc(length) calloc(1, ((length) + 7) >> 3)
#define bfclone(bitfield, length) memcpy(malloc(((length) + 7) >> 3), (bitfield), ((length) + 7) >> 3)
#define bfget(bitfield, index) ((bitfield)[(index) >> 3] & (1 << ((index) & 7)))
#define bfset(bitfield, index) ((bitfield)[(index) >> 3] |= (1 << ((index) & 7)))
#define bfunset(bitfield, index) ((bitfield)[(index) >> 3] & ~(1 << ((index) & 7)))
#define BITFIELD char*
#endif
