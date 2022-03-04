#define _GNU_SOURCE
#ifndef QHASH_H
#define QHASH_H
#define HASHSIZE 256
#define PROBECOUNT 5
#include <stdlib.h>
#include <string.h>
#include "dynarr.h"
#include "joecc_assert.h"

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
} QHASHTABLE;

void insert(QHASHTABLE* qh, const char* key, void* value);
void* search(QHASHTABLE* qh, const char* key);
char queryval(QHASHTABLE* qh, const char* key);
void rmpaircfr(QHASHTABLE* qh, const char* key, void (*cfree)(void*));
void insertcfr(QHASHTABLE* qh, const char* key, void* value, void (*cfree)(void*));
void resize(QHASHTABLE* qh);
char htequal(QHASHTABLE* ht1, QHASHTABLE* ht2);
#endif
