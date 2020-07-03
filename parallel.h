#ifndef PARALLEL_H
#define PARALLEL_H
#include <stdlib.h>
#include "dynarr.h"
#include "hash.h"
typedef struct {
  HASHTABLE* ht;
  DYNARR* da;
} PARALLEL;
PARALLEL* paralector();
void* psearch(PARALLEL* p, char* key);
void pinsert(PARALLEL* p, char* key, void* value);
#endif

