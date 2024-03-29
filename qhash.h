#undef _GNU_SOURCE
#define _GNU_SOURCE
#ifndef QHASH_H
#define QHASH_H
#define HASHSIZE 256
#define PROBECOUNT 5
#include <stdlib.h>
#include <string.h>
#include "dynarr.h"
#include "joecc_assert.h"
#include "bf.h"

#define HASHPROTO(type_prefix, prefix, keytype, valtype) \
typedef struct { \
  keytype key; \
  valtype value; \
} type_prefix ## PAIR; \
typedef struct { \
  int keys;\
  int slotmask; /*this is in the form of 2^n - 1 for a ht with n bits*/ \
  type_prefix ## PAIR* hashtable; \
  BITFIELD bf; \
} type_prefix ## TABLE; \
void prefix ## insert(type_prefix ## TABLE* qh, const keytype key, valtype value); \
valtype prefix ## search(type_prefix ## TABLE* qh, const keytype key); \
char prefix ## queryval(type_prefix ## TABLE* qh, const keytype key); \
void prefix ## rmpaircfr(type_prefix ## TABLE* qh, const keytype key, void (*cfree)(valtype)); \
void prefix ## rmpair(type_prefix ## TABLE* qh, const keytype key); \
void prefix ## resize(type_prefix ## TABLE* qh); \
DYNARR* prefix ## htpairs(type_prefix ## TABLE* ht); \
type_prefix ## TABLE* prefix ## htctor(void); \
type_prefix ## TABLE* prefix ## chtctor(int size); \
void prefix ## htdtor(type_prefix ## TABLE* ht); \
void prefix ## chtdtor(type_prefix ## TABLE* ht, void (*freep)(valtype)); \
void prefix ## insertcfr(type_prefix ## TABLE* qh, const keytype key, valtype value, void (*cfree)(valtype));

HASHPROTO(QHASH, q, char*, void*); //qhash, named for generic quadratic probing
HASHPROTO(OPHASH, op, void*, void*); //ophash, has OPERATIONs as keys! Special checking needs to be done
HASHPROTO(IIHASH, ii, int, int); //IIhash, goes from ints to ints
HASHPROTO(LVHASH, lv, long, void*); //lvhash, goes from longs to void*s
HASHPROTO(FVHASH, fv, double, void*); //fvhash, goes from floats (doubles) to void*s

IIHASHTABLE* iiclone(IIHASHTABLE* ht);
LVHASHTABLE* lvhtcclone(LVHASHTABLE* ht, void*(*clonefunc)(void*));


#define SETPROTO(type_prefix, prefix, keytype) \
typedef struct { \
    keytype key; \
} type_prefix ## SETENT; \
typedef struct { \
    int keys; \
    int slotmask; \
    type_prefix ## SETENT* hashtable; \
    BITFIELD bf; \
} type_prefix ## SET; \
type_prefix ## SET* prefix ## setctor(int size); \
void prefix ## setdtor(type_prefix ## SET*); \
char prefix ## setcontains(type_prefix ## SET*, const keytype key); \
void prefix ## setinsert(type_prefix ## SET*, const keytype key); \
void prefix ## setresize(type_prefix ## SET* qh); \
type_prefix ## SET* prefix ## setclone(type_prefix ## SET* qh);

SETPROTO(IHASH, i, int); //ihashset, uses integer keys
SETPROTO(LHASH, l, long); //lhashset, uses long keys

void iiwipehash(IHASHSET* hs);

DYNINT* isetelems(IHASHSET* ihs);
#endif
