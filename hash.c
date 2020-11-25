#include "hash.h"

static unsigned long hash(const char* str) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while((c = *str++))
    hash = ((hash << 5) + hash) ^ c;    /* hash* 33 + c */
  return hash % HASHSIZE;
}

HASHTABLE* htctor(void) {
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

static void hpdtor(HASHPAIR* hp) {
  if(hp->next) {
    free(hp->key);
    hpdtor(hp->next);
  }
  free(hp);
}

static void hpdtorfr(HASHPAIR* hp) {
  free(hp->key);
  free(hp->value);
  if(hp->next) {
    hpdtorfr(hp->next);
  }
  free(hp);
}

void htdtor(HASHTABLE* ht) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
    }
    if(ht->pairs[i].next)
      hpdtor(ht->pairs[i].next);
  }
  free(ht);
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

static void hpdtorcfr(HASHPAIR* hp, void (*freep)(void*)) {
  if(hp->next) {
    free(hp->key);
    freep(hp->value);
    hpdtorcfr(hp->next, freep);
  }
  free(hp);
}

void htdtorcfr(HASHTABLE* ht, void (*freep)(void*)) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      freep(ht->pairs[i].value);
    }
    if(ht->pairs[i].next)
      hpdtorcfr(ht->pairs[i].next, freep);
  }
  free(ht);
}

void insert(HASHTABLE* ht, const char* key, void* value) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)) {
    hp->key = strdup(key);
    hp->value = value;
  } else {
    for(; hp->next; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
        hp->value = value;
        return;
      }
    }
    if(!strcmp(hp->key, key)) {
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = calloc(1, sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    hp->next = newpair;
  }
  ++ht->keys;
}

void insertcfr(HASHTABLE* ht, const char* key, void* value, void (*cfree)(void*)) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)) {
    hp->key = strdup(key);
    hp->value = value;
  } else {
    for(; hp->next; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
        cfree(hp->value);
        hp->value = value;
        return;
      }
    }
    if(!strcmp(hp->key, key)) {
      cfree(hp->value);
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = calloc(1, sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    hp->next = newpair;
  }
  ++ht->keys;
}

void rmpair(HASHTABLE* ht, const char* key) {
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
      --ht->keys;
      return;
    }
  }
}

void rmpaircfr(HASHTABLE* ht, const char* key, void (*cfree)(void*)) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return;
  HASHPAIR* prev = NULL;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      if(hp->next) {
        HASHPAIR* temp = hp->next;
        free(hp->key);
        cfree(hp->value);
        memcpy(hp, hp->next, sizeof(HASHPAIR));
        free(temp);
      } else {
        free(hp->key);
        hp->key = NULL;
        cfree(hp->value);
        if(prev)
          prev->next = NULL;
      }
      --ht->keys;
      return;
    }
    prev = hp;
  }
  return;
}

void* search(HASHTABLE* ht, const char* key) {
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

void* searchval(HASHTABLE* ht, const char* key, char* vallocate) {
  unsigned long i = hash(key);
  *vallocate = 0;
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return NULL;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      *vallocate = 1;
      return hp->value;
    }
  }
  return NULL;
}

char queryval(HASHTABLE* ht, const char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return 0;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      return 1;
    }
  }
  return 0;
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

void intsert(HASHTABLE* ht, const char* key, long value) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)) {
    hp->key = strdup(key);
    hp->ivalue = value;
  } else {
    for(; hp->next; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
        hp->ivalue = value;
        return;
      }
    }
    if(!strcmp(hp->key, key)) {
      hp->ivalue = value;
      return;
    }
    HASHPAIR* newpair = calloc(1, sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->ivalue = value;
    hp->next = newpair;
  }
  ++ht->keys;
}
