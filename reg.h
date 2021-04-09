#ifndef REG_C
#define REG_C
#include "3ac.h"
//Separate things for floats, ints
typedef struct {
  DYNARR* neighbors;
  short banneighbors; //clobbers, inputs, output to go elsewhere
  short color; //which register
} REGINF;
#endif
