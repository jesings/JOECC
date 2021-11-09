#ifndef REG_C
#define REG_C
#include "3ac.h"
#include <assert.h>

enum reguse {
  DI = 1, 
  SI = 2, 
  DX = 4,
  CX = 8,
  R8 = 0x20,
  R9 = 0x40,
  AX = 0x10,
  R10 = 0x80,
  R11 = 0x100,
  R12 = 0x200,
  R13 = 0x400,
  R14 = 0x800,
  R15 = 0x1000,
  BX = 0x2000,
  //missing BP and SP
  //This is because we don't allow them to be used in that way
};

#define callermask BX | R12 | R13 | R14 | R15

extern const char* ireg64[];
extern const char* ireg32[];
extern const char* ireg16[];
extern const char* ireg8[];
extern const char* freg256[];
extern const char* freg128[];

//Separate things for floats, ints
void liveness(PROGRAM* prog);
typedef struct {
  DYNARR* neighbors; //array of REGINF pointers
  short banneighbors; //clobbers, inputs, output to go elsewhere
  short color; //which register
} REGINF;
void regalloc(PROGRAM* prog);
BITFIELD liveadjmatrix(PROGRAM* prog, DYNARR** usedefs);
#endif
