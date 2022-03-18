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
typedef struct qhp {
  char* key;
  void* value;
} QHASHPAIR;

typedef struct {
  int keys;
  int slotmask; //this is in the form of 2^n - 1 for a ht with n bits
  QHASHPAIR* hashtable;
  BITFIELD bf;
} QHASHTABLE;

void qinsert(QHASHTABLE* qh, const char* key, void* value);
void* qsearch(QHASHTABLE* qh, const char* key);
char qqueryval(QHASHTABLE* qh, const char* key);
void qrmpaircfr(QHASHTABLE* qh, const char* key, void (*cfree)(void*));
void qrmpair(QHASHTABLE* qh, const char* key);
void qinsertcfr(QHASHTABLE* qh, const char* key, void* value, void (*cfree)(void*));
void qresize(QHASHTABLE* qh);
char qhtequal(QHASHTABLE* ht1, QHASHTABLE* ht2);
DYNARR* qhtpairs(QHASHTABLE* ht);
QHASHTABLE* qhtctor(void);
QHASHTABLE* qchtctor(int size);
void qhtdtor(QHASHTABLE* ht);
void qchtdtor(QHASHTABLE* ht, void (*freep)(void*));

typedef struct ihp {
  int key;
  int value;
} IIHASHPAIR;

typedef struct {
  int keys;
  int slotmask; //this is in the form of 2^n - 1 for a ht with n bits
  IIHASHPAIR* hashtable;
  BITFIELD bf;
} IIHASHTABLE;


void iiinsert(IIHASHTABLE* qh, const int key, int value);
int iisearch(IIHASHTABLE* qh, const int key);
char iiqueryval(IIHASHTABLE* qh, const int key);
void iirmpair(IIHASHTABLE* qh, const int key);
void iiresize(IIHASHTABLE* qh);
DYNARR* iihtpairs(IIHASHTABLE* ht);
IIHASHTABLE* iihtctor(void);
IIHASHTABLE* iichtctor(int size);
void iihtdtor(IIHASHTABLE* ht);
IIHASHTABLE* iiclone(IIHASHTABLE* ht);
void iichtdtor(IIHASHTABLE* ht, void (*freep)(int));

typedef struct lvhp {
  long key;
  void* value;
} LVHASHPAIR;

typedef struct {
  int keys;
  int slotmask; //this is in the form of 2^n - 1 for a ht with n bits
  LVHASHPAIR* hashtable;
  BITFIELD bf;
} LVHASHTABLE;


void lvinsert(LVHASHTABLE* qh, const long key, void* value);
void* lvsearch(LVHASHTABLE* qh, const long key);
char lvqueryval(LVHASHTABLE* qh, const long key);
void lvrmpair(LVHASHTABLE* qh, const long key);
void lvresize(LVHASHTABLE* qh);
DYNARR* lvhtpairs(LVHASHTABLE* ht);
LVHASHTABLE* lvhtctor(void);
LVHASHTABLE* lvchtctor(int size);
void lvhtdtor(LVHASHTABLE* ht);
void lvchtdtor(LVHASHTABLE* ht, void (*freep)(void*));
#endif
