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
  void* value;
  struct hp* next;
} HASHPAIR;
typedef struct{
  HASHPAIR pairs[HASHSIZE];
  int keys;
} HASHTABLE;

HASHTABLE* htctor();
HASHTABLE* htclone(HASHTABLE* ht);
void htdtor(HASHTABLE* ht);
void htdtorfr(HASHTABLE* ht);
void insert(HASHTABLE* ht, char* key, void* value);
void insertfr(HASHTABLE* ht, char* key, void* value);
void* search(HASHTABLE* ht, char* key);
void* searchval(HASHTABLE* ht, char* key, char* vallocate);
char queryval(HASHTABLE* ht, char* key);
void rmpair(HASHTABLE* ht, char* key);
void rmpairfr(HASHTABLE* ht, char* key);
DYNARR* htpairs(HASHTABLE* ht);
unsigned long fixedhash(char* data, char lbits);
void fixedinsert(HASHTABLE* ht, long fixedkey, void* value);
void* fixedsearch(HASHTABLE* ht, long fixedkey);
#endif

