#ifndef SSA_H
#define SSA_H
#include "3ac.h"

typedef struct sedi{
  enum opcode_3ac op; //if INIT_3 (or PARAM_3) then it's a regno
  union {
    struct {
      struct sedi* arg0;
      struct sedi* arg1;
    };
    int regno;
  };
} SEDITEM;

typedef struct {
  DYNARR* equivs;
  FULLADDR constant;
  char hasconst;
} SEDNODE;

typedef struct {
  DYNARR* nodes;//of SEDNODES
  DYNARR* varnodes;
  HASHTABLE* intconsthash;
  HASHTABLE* floatconsthash;
  DYNARR* opnodes;//one entry for every operation
} SEDAG;

void ctdtree(PROGRAM* prog);
void popsedag(BBLOCK* blk);
#endif
