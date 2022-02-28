#ifndef QHASH_H
#define QHASH_H
#define HASHSIZE 256
#define PROBECOUNT 5
#include <stdlib.h>
#include <string.h>
#include "dynarr.h"

/**
 * Represents a key/value pair in the hashmap with quadratic probing.
 * Can store values either as ints or pointers.
 * Keys are typically string pointers. 
**/
typedef struct qhp {
  union {
    char* key;
    long fixedkey;
  };
  union {
    void* value;
    long ivalue;
  };
} QHASHPAIR;

typedef struct {
  int keys;
  int slotmask; //this is in the form of 2^n - 1 for a ht with n bits
  QHASHPAIR* hashtable;
} QHASHTABLE;

#endif
