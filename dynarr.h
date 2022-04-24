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

//constructs the function prototypes necessary for our dynamic array interface on invocation, see dynarr.c's comment for more information
#define DYNDECL(type_suffix, prefix, elemtype) \
DYN ## type_suffix* prefix ## ctor(int initiallen) ; \
void prefix ## dtor(DYN ## type_suffix* da) ; \
void prefix ## push(DYN ## type_suffix* da, elemtype val) ; \
DYN ## type_suffix* prefix ## merge(DYN ## type_suffix* arr1, DYN ## type_suffix* arr2) ; \
DYN ## type_suffix* prefix ## clone(DYN ## type_suffix* orig) ; \
elemtype prefix ## pop(DYN ## type_suffix* da) ; \
elemtype prefix ## remove_swap(DYN ## type_suffix* da, elemtype val);

DYNDECL(ARR, da, void*) //DYNARR/dapush etc. generic dynamic array
DYNDECL(INT, di, int) //DYNINT/dipush etc. dynamic array of integers
#define dapeek(A) ((A)->arr[(A)->length-1])
#define daget(A,I) ((A)->arr[(I)])
#define dipeek(A) ((A)->arr[(A)->length-1])
#define diget(A,I) ((A)->arr[(I)])

void dadtorfr(DYNARR* da);
void dadtorcfr(DYNARR* da, void (*freep)(void*));
void dapushc(DYNARR* da, void* val);
void darpa(DYNARR* da, void* val, void* rpval);
void dainsertat(DYNARR* da, int index, void* value);

void disort(DYNINT* di);
void didup(DYNINT* di);

// vim: set ft=c:
#endif
