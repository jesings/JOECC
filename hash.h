#ifndef HASH_H
#define HASH_H
#define HASHSIZE 4096
#include "dynarr.h"

struct hp;
typedef struct hp{
  char* key;
  void* value;
  struct hp* next;
} HASHPAIR;
typedef struct{
  HASHPAIR pairs[HASHSIZE];
} HASHTABLE;

HASHTABLE* htctor();
HASHTABLE* htclone(HASHTABLE* ht);
void htdtor(HASHTABLE* ht);
void htdtorfr(HASHTABLE* ht);
void insert(HASHTABLE* ht, char* key, void* value);
void insertfr(HASHTABLE* ht, char* key, void* value);
void* search(HASHTABLE* ht, char* key);
void rmpair(HASHTABLE* ht, char* key);
void rmpairfr(HASHTABLE* ht, char* key);
DYNARR* htpairs(HASHTABLE* ht);
#endif
