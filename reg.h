#ifndef REG_C
#define REG_C
#include "3ac.h"
//Separate things for floats, ints
typedef struct {
  DYNARR* neighbors; //array of REGINF pointers
  short banneighbors; //clobbers, inputs, output to go elsewhere
  short color; //which register
} REGINF;
void regalloc(PROGRAM* prog);
#endif
