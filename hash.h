#ifndef HASH_H
#define HASH_H
#define HASHSIZE 256
#define BIGHASHSIZE 8192
#include <stdlib.h>
#include <string.h>
#include "dynarr.h"

typedef struct hp {
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
typedef struct {
  int keys;
  HASHPAIR pairs[HASHSIZE];
} HASHTABLE;
typedef struct {
  int keys;
  HASHPAIR pairs[BIGHASHSIZE];
} BIGHASHTABLE;

HASHTABLE* htctor(void);
HASHTABLE* htclone(HASHTABLE* ht);
BIGHASHTABLE* bightctor(void);
void htdtor(HASHTABLE* ht);
void htdtorfr(HASHTABLE* ht);
void htdtorcfr(HASHTABLE* ht, void (*freep)(void*));
void bightdtorcfr(BIGHASHTABLE* ht, void (*freep)(void*));
void fhtdtor(HASHTABLE* ht);
void fhtdtorcfr(HASHTABLE* ht, void(*freef)(void*));
void insert(HASHTABLE* ht, const char* key, void* value);
void biginsert(BIGHASHTABLE* ht, const char* key, void* value);
void bigfinsertfr(BIGHASHTABLE* ht, const char* key, void* value, int len);
void insertcfr(HASHTABLE* ht, const char* key, void* value, void (*cfree)(void*));
void* search(HASHTABLE* ht, const char* key);
void* bigsearch(BIGHASHTABLE* ht, const char* key, int flen);
char queryval(HASHTABLE* ht, const char* key);
char bigqueryval(BIGHASHTABLE* ht, const char* key);
void rmpair(HASHTABLE* ht, const char* key);
void frmpair(HASHTABLE* ht, long fixedkey);
void rmpaircfr(HASHTABLE* ht, const char* key, void (*cfree)(void*));
void bigrmpaircfr(BIGHASHTABLE* ht, const char* key, void (*cfree)(void*), int flen);
DYNARR* htpairs(HASHTABLE* ht);
DYNARR* htfpairs(HASHTABLE* ht);
void fixedinsert(HASHTABLE* ht, long fixedkey, void* value);
void* fixedsearch(HASHTABLE* ht, long fixedkey);
char fixedqueryval(HASHTABLE* ht, long fixedkey);
void intsert(HASHTABLE* ht, const char* key, long value);
#endif

