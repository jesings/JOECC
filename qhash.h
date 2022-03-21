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

/**
 * Represents a key/value pair in the hashmap with quadratic probing.
 * Can store values either as ints or pointers.
 * Keys are typically string pointers. 
**/

#define HASHPROTO(type_prefix, prefix, keytype, valtype) \
typedef struct prefix ## hp { \
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
char prefix ## htequal(type_prefix ## TABLE* ht1, type_prefix ## TABLE* ht2); \
DYNARR* prefix ## htpairs(type_prefix ## TABLE* ht); \
type_prefix ## TABLE* prefix ## htctor(void); \
type_prefix ## TABLE* prefix ## chtctor(int size); \
void prefix ## htdtor(type_prefix ## TABLE* ht); \
void prefix ## chtdtor(type_prefix ## TABLE* ht, void (*freep)(valtype));

HASHPROTO(QHASH, q, char*, void*);
HASHPROTO(IIHASH, ii, int, int);
HASHPROTO(LVHASH, lv, long, void*);
HASHPROTO(LFHASH, lf, long, double);

void qinsertcfr(QHASHTABLE* qh, const char* key, void* value, void (*cfree)(void*));
IIHASHTABLE* iiclone(IIHASHTABLE* ht);
LVHASHTABLE* lvhtcclone(LVHASHTABLE* ht, void*(*clonefunc)(void*));

#endif
