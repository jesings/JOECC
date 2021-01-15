#ifndef PARALLEL_H
#define PARALLEL_H
#include <stdlib.h>
#include "dynarr.h"
#include "hash.h"
typedef struct {
  HASHTABLE* ht;
  DYNARR* da;
} PARALLEL;
PARALLEL* paralector(void);
PARALLEL* paraclector(int i);
void pinsert(PARALLEL* p, const char* key, void* value);
void pfinsert(PARALLEL* p, long unsigned key, void* value);
void paraledtor(PARALLEL* p);
void paraledtorcfr(PARALLEL* p, void (*freefunc)(void*));
void fparaledtorcfr(PARALLEL* p, void (*freefunc) (void*));
#define pget(p, i) search(p->ht, daget(p->da, i))
#endif

