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
void* psearch(PARALLEL* p, const char* key);
void* pisearch(PARALLEL* p, int index);
void pinsert(PARALLEL* p, const char* key, void* value);
char pquery(PARALLEL* p, const char* key);
void* pfsearch(PARALLEL* p, long unsigned key);
char pfquery(PARALLEL* p, long unsigned key);
void pfinsert(PARALLEL* p, long unsigned key, void* value);
void paraledtor(PARALLEL* p);
void paraledtorcfr(PARALLEL* p, void (*freefunc)(void*));
void fparaledtorcfr(PARALLEL* p, void (*freefunc) (void*));
#define pget(p, i) search(p->ht, daget(p->da, i))
#endif

