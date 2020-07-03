#include "parallel.h"
PARALLEL* paralector() {
  PARALLEL* p = malloc(sizeof(PARALLEL));
  p->da = dactor(256);
  p->ht = htctor();
  return p;
}
void* psearch(PARALLEL* p, char* key) {
  return search(p->ht, key);
}
char pquery(PARALLEL* p, char* key) {
  return queryval(p->ht, key);
}
void pinsert(PARALLEL* p, char* key, void* value) {
  dapush(p->da, key);
  insert(p->ht, key, value);
}
