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
} SEDNODE;

typedef struct {
  enum opcode_3ac op; //if INIT_3 (or PARAM_3) then it's a regno
  union {
    struct {
      SEDNODE* arg0;
      SEDNODE* arg1;
    };
    unsigned int regno;
  };
} SEDITEM;


typedef struct {
  DYNARR* nodes;//of SEDNODES
  DYNARR* varnodes;
  HASHTABLE* intconsthash;
  HASHTABLE* floatconsthash;
  HASHTABLE* strconsthash;
  DYNARR* opnodes;//one entry for every operation
} SEDAG;

void ctdtree(PROGRAM* prog);
void popsedag(PROGRAM* prog);
#define bfalloc(length) calloc(1, (length + 7) >> 3)
#define bfclone(bitfield, length) memcpy(malloc((length + 7) >> 3), bitfield, (length + 7) >> 3)
#define bfget(bitfield, index) bitfield[index >> 3] & (1 << (index & 7))
#define bfset(bitfield, index) bitfield[index >> 3] | (1 << (index & 7))
#define bfunset(bitfield, index) bitfield[index >> 3] & ~(1 << (index & 7))
#define BITFIELD char*
#endif
