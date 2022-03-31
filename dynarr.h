#ifndef DYNARR_H
#define DYNARR_H
/**
 * Represents a dynamically resizable array of pointers
**/
typedef struct {
  void** arr;
  int length;
  int maxlength;
} DYNARR;

/**
 * Represents a dynamically resizable array of integers
**/
typedef struct {
  union {
    int* arr;
    unsigned int* uarr;
  };
  int length;
  int maxlength;
} DYNINT;

DYNARR* dactor(int initiallen);
DYNARR* damerge(DYNARR* arr1, DYNARR* arr2);
DYNARR* daclone(DYNARR* orig);
void dadtor(DYNARR* da);
void dadtorfr(DYNARR* da);
void dadtorcfr(DYNARR* da, void (*freep)(void*));

DYNINT* dictor(int initiallen);
void dipush(DYNINT* di, int i);
int dipop(DYNINT* di);
void didtor(DYNINT* di);
void disort(DYNINT* di);
void didup(DYNINT* di);
DYNINT* dimerge(DYNINT* arr1, DYNINT* arr2);
DYNINT* diclone(DYNINT* di);

void dapush(DYNARR* da, void* val);
void dapushc(DYNARR* da, void* val);
void* dapop(DYNARR* da);
void* dharma(DYNARR* da, void* val);
void darpa(DYNARR* da, void* val, void* rpval);
#define dapeek(A) ((A)->arr[(A)->length-1])
#define dipeek(A) ((A)->arr[(A)->length-1])
#define daget(A,I) ((A)->arr[(I)])
#define diget(A,I) ((A)->arr[(I)])
// vim: set ft=c:
#endif
