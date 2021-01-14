#ifndef DYNARR_H
#define DYNARR_H
typedef struct {
  void** arr;
  int length;
  int maxlength;
} DYNARR;

DYNARR* dactor(int initiallen);
DYNARR* damerge(DYNARR* arr1, DYNARR* arr2);
DYNARR* daclone(DYNARR* orig);
void dadtor(DYNARR* da);
void dadtorfr(DYNARR* da);
void dadtorcfr(DYNARR* da, void (*freep)(void*));

void dainsert(DYNARR* da, void* val);
void dainsertc(DYNARR* da, void* val);
#define dapush dainsert
#define dapushc dainsertc
void* dapop(DYNARR* da);
void dharma(DYNARR* da, void* val);
void darpa(DYNARR* da, void* val, void* rpval);
#define dapeek(A) ((A)->arr[(A)->length-1])
#define daget(A,I) ((A)->arr[(I)])
// vim: set ft=c:
#endif
