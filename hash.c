#include <stdlib.h>
#include <string.h>
#include "hash.h"

unsigned long hash(unsigned char* str) {	/*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while(c = *str++)
    hash = ((hash << 5) + hash) + c;	/* hash* 33 + c */
  return hash % HASHSIZE;
}

HASHTABLE* htctor() {
  return calloc(1, sizeof(HASHTABLE));
}
HASHTABLE* htclone(HASHTABLE* ht) {
  HASHTABLE* retval = malloc(sizeof(HASHTABLE));
  memcpy(retval, ht, sizeof(HASHTABLE));
  for(int i = 0; i < HASHSIZE; i++) {
    HASHPAIR* clonepair;
    HASHPAIR* curpair = &(ht->pairs[i]), *parpar = &(retval->pairs[i]);
    if(!(curpair->key))
      continue;
    parpar->key = strdup(curpair->key);
    if(curpair->next) {
      for(curpair = curpair->next; curpair->next; curpair = curpair->next) {
	clonepair = malloc(sizeof(HASHPAIR));
	clonepair->value = curpair->value;
	clonepair->key = strdup(curpair->key);
	parpar = parpar->next = clonepair;
      }
    }
    parpar->next = NULL;
  }
  return retval;
}

void hpdtor(HASHPAIR * hp) {
  if(hp->next) {
    free(hp->key);
    hpdtor(hp->next);
  }
  free(hp);
}
void htdtor(HASHTABLE* ht) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].next)
      hpdtor(ht->pairs[i].next);
    if(ht->pairs[i].key)
      free(ht->pairs[i].key);
  }
  free(ht);
}

void hpdtorfr(HASHPAIR * hp) {
  if(hp->next) {
    free(hp->key);
    free(hp->value);
    hpdtorfr(hp->next);
  }
  free(hp);
}
void htdtorfr(HASHTABLE* ht) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      free(ht->pairs[i].value);
    }
    if(ht->pairs[i].next)
      hpdtorfr(ht->pairs[i].next);
  }
  free(ht);
}

void insert(HASHTABLE* ht, char* key, void* value) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)) {
    hp->key = strdup(key);
    hp->value = value;
  } else {
    for(; hp->next; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
	//free(hp->value);
	hp->value = value;
	return;
      }
    }
    if(!strcmp(hp->key, key)) {
      //free(hp->value);
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = calloc(1, sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    hp->next = newpair;
  }
}

void insertfr(HASHTABLE* ht, char* key, void* value) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)) {
    hp->key = strdup(key);
    hp->value = value;
  } else {
    for(; hp->next; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
	free(hp->value);
	hp->value = value;
	return;
      }
    }
    if(!strcmp(hp->key, key)) {
      free(hp->value);
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = calloc(1, sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    hp->next = newpair;
  }
}

void rmpair(HASHTABLE* ht, char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      if(hp->next) {
	HASHPAIR* temp = hp->next;
	free(hp->key);
	memcpy(hp, hp->next, sizeof(HASHPAIR));
	free(temp);
      } else {
	free(hp->key);
	hp->key = NULL;
      }
      return;
    }
  }
}

void rmpairfr(HASHTABLE* ht, char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      if(hp->next) {
	HASHPAIR* temp = hp->next;
	free(hp->key);
	free(hp->value);
	memcpy(hp, hp->next, sizeof(HASHPAIR));
	free(temp);
      } else {
	free(hp->key);
	hp->key = NULL;
	free(hp->value);
      }
      return;
    }
  }
}

void* search(HASHTABLE* ht, char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return NULL;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key))
      return hp->value;
  }
  return NULL;
}

DYNARR* htpairs(HASHTABLE* ht) {
  DYNARR* da = dactor(4096);
  for(int i = 0; i < HASHSIZE; i++) {
    HASHPAIR* current = &(ht->pairs[i]);
    if(current->key) {
      dapush(da, current);
      while(current->next) {
        current = current->next;
        dapush(da, current);
      }
    }
  }
  return da;
}
