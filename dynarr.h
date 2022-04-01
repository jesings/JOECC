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

#define DYNDECL(type_suffix, prefix, elemtype) \
DYN ## type_suffix* prefix ## ctor(int initiallen) ; \
void prefix ## dtor(DYN ## type_suffix* da) ; \
void prefix ## push(DYN ## type_suffix* da, elemtype val) ; \
DYN ## type_suffix* prefix ## merge(DYN ## type_suffix* arr1, DYN ## type_suffix* arr2) ; \
DYN ## type_suffix* prefix ## clone(DYN ## type_suffix* orig) ; \
elemtype prefix ## pop(DYN ## type_suffix* da) ;

DYNDECL(ARR, da, void*)
DYNDECL(INT, di, int)
#define dapeek(A) ((A)->arr[(A)->length-1])
#define daget(A,I) ((A)->arr[(I)])
#define dipeek(A) ((A)->arr[(A)->length-1])
#define diget(A,I) ((A)->arr[(I)])

void dadtorfr(DYNARR* da);
void dadtorcfr(DYNARR* da, void (*freep)(void*));
void dapushc(DYNARR* da, void* val);
void* dapop(DYNARR* da);
void* dharma(DYNARR* da, void* val);
void darpa(DYNARR* da, void* val, void* rpval);

void disort(DYNINT* di);
void didup(DYNINT* di);
DYNINT* dimerge(DYNINT* arr1, DYNINT* arr2);
DYNINT* diclone(DYNINT* di);

// vim: set ft=c:
#endif
