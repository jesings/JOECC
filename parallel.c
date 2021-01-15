#include "parallel.h"
PARALLEL* paralector(void) {
  PARALLEL* p = malloc(sizeof(PARALLEL));
  p->da = dactor(256);
  p->ht = htctor();
  return p;
}
PARALLEL* paraclector(int i) {
  PARALLEL* p = malloc(sizeof(PARALLEL));
  p->da = dactor(i);
  p->ht = htctor();
  return p;
}
void pinsert(PARALLEL* p, const char* key, void* value) {
  dapush(p->da, (char *)(unsigned long) key);
  insert(p->ht, key, value);
}
void pfinsert(PARALLEL* p, long unsigned key, void* value) {
  dapush(p->da, (char *)(unsigned long) key);
  fixedinsert(p->ht, key, value);
}

void paraledtor(PARALLEL* p) {
  dadtor(p->da);
  htdtor(p->ht);
  free(p);
}
void paraledtorcfr(PARALLEL* p, void (*freefunc) (void*)) {
  dadtorfr(p->da);
  htdtorcfr(p->ht, freefunc);
  free(p);
}
void fparaledtorcfr(PARALLEL* p, void (*freefunc) (void*)) {
  dadtor(p->da);
  fhtdtorcfr(p->ht, freefunc);
  free(p);
}
