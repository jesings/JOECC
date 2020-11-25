#include "parallel.h"
PARALLEL* paralector(void) {
  PARALLEL* p = malloc(sizeof(PARALLEL));
  p->da = dactor(256);
  p->ht = htctor();
  return p;
}
void* psearch(PARALLEL* p, const char* key) {
  return search(p->ht, key);
}
char pquery(PARALLEL* p, const char* key) {
  return queryval(p->ht, key);
}
void pinsert(PARALLEL* p, const char* key, void* value) {
  dapush(p->da, (char *)(unsigned long) key);
  insert(p->ht, key, value);
}

void* pfsearch(PARALLEL* p, long unsigned key) {
  return fixedsearch(p->ht, key);
}
char pfquery(PARALLEL* p, long unsigned key) {
  return fixedqueryval(p->ht, key);
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
void paraledtorfr(PARALLEL* p) {
  htdtorfr(p->ht);
  dadtorfr(p->da);
  free(p);
}
void paraledtorcfr(PARALLEL* p, void (*freefunc) (void*)) {
  dadtorfr(p->da);
  htdtorcfr(p->ht, freefunc);
  free(p);
}
