#ifndef HASH_H
#define HASH_H
#define HASHSIZE 4096
#include <stdlib.h>
#include <string.h>
#include "dynarr.h"

typedef struct hp{
  union {
    char* key;
    long fixedkey;
  };
  union {
    void* value;
    long ivalue;
  };
  struct hp* next;
} HASHPAIR;
typedef struct{
  HASHPAIR pairs[HASHSIZE];
  int keys;
} HASHTABLE;

HASHTABLE* htctor(void);
HASHTABLE* htclone(HASHTABLE* ht);
void htdtor(HASHTABLE* ht);
void htdtorfr(HASHTABLE* ht);
void htdtorcfr(HASHTABLE* ht, void (*freep)(void*));
void insert(HASHTABLE* ht, const char* key, void* value);
void insertfr(HASHTABLE* ht, const char* key, void* value);
void* search(HASHTABLE* ht, const char* key);
void* searchval(HASHTABLE* ht, const char* key, char* vallocate);
char queryval(HASHTABLE* ht, const char* key);
void rmpair(HASHTABLE* ht, const char* key);
void rmpairfr(HASHTABLE* ht, const char* key);
DYNARR* htpairs(HASHTABLE* ht);
unsigned long fixedhash(const char* data, char lbits);
void fixedinsert(HASHTABLE* ht, long fixedkey, void* value);
void* fixedsearch(HASHTABLE* ht, long fixedkey);
char fixedqueryval(HASHTABLE* ht, long fixedkey);
void intsert(HASHTABLE* ht, const char* key, long value);
#endif

