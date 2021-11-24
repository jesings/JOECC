#ifndef SSA_H
#define SSA_H
#include "3ac.h"

/**
 * Represents a number as in global value numbering. This number contains a list of
 * equivalent values (VALUESTRUCTs), the equivalent constant and tag information, and
 * the index in the list to prevent unnecesary storage of the index.
**/
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

/**
 * Represents an instance of a value as in global value numbering, Primarily used
 * for GVNPRE, to represent anticipable expressions. Typically p1 (and p2 if binary) 
 * are the indices of the values of the operands, however in the special case of
 * INIT_3 (PARAM_3 is replaced with INIT_3 for simplicity), the value p1 refers to
 * the actual regnum of the variable. This is done to allow for operand replacement
 * in GVNPRE
**/
typedef struct {
  enum opcode_3ac o;
  unsigned int p1;
  unsigned int p2;
  short size1; //bottom 4 bits size, 5th bit sign
  short size2; //bottom 4 bits size, 5th bit sign
} VALUESTRUCT;

/**
 * Contains all of the equivalence information for a PROGRAM. These are the GVNNUMS,
 * the information about which regnums map to which constants, and the information
 * about which operations map to which values.
**/
typedef struct {
  DYNARR* uniq_vals;//of GVNNUMS
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
