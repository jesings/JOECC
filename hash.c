#define HASHSIZE 4096
#include <stdlib.h>
#include <string.h>
#include "hash.h"

unsigned long hash(unsigned char *str){/*courtesy of http://www.cse.yorku.ca/~oz/hash.html*/
  unsigned long hash = 5381;
  int c;
  while (c = *str++)
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  return hash % HASHSIZE;
}

HASHTABLE* htctor(){
  return calloc(1,sizeof(HASHTABLE));
}
HASHTABLE* htclone(HASHTABLE* ht){
  HASHTABLE* retval = calloc(1,sizeof(HASHTABLE));
  memcpy(retval, ht, sizeof(HASHTABLE));
  return retval;
}

void hpdtor(HASHPAIR* hp){
  if(hp->next)
      hpdtor(hp->next);
  free(hp);
}
void htdtor(HASHTABLE* ht){
  for(int i; i<HASHSIZE; i++)
    if(ht->pairs[i].next)
      free(ht->pairs[i].next);
  free(ht);
}

void insert(HASHTABLE* ht, char* key, void* value){
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)){
    hp->key = key;
    hp->value = value;
  }
  else{
    for(; hp->next; hp = hp->next){
      if(!strcmp(hp->key, key)){
        free(hp->value);
        hp->value = value;
        return;
      }
    }
    if(!strcmp(hp->key, key)){
      free(hp->value);
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = calloc(1,sizeof(HASHPAIR));
    newpair->key = key;
    newpair->value = value;
    hp->next = newpair;
  }
}

void* search(HASHTABLE* ht, char* key){
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return NULL;
  for(; hp; hp = hp->next){
    if(!strcmp(hp->key, key))
      return hp->value;
  }
  return NULL;
}
