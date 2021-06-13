#include "hash.h"

static unsigned long hash(const char* str) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while((c = *str++))
    hash = ((hash << 5) + hash) ^ c;    /* hash* 33 + c */
  return hash % HASHSIZE;
}
static unsigned long bighash(const char* str) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while((c = *str++))
    hash = ((hash << 5) + hash) ^ c;    /* hash* 33 + c */
  return hash % BIGHASHSIZE;
}
static unsigned long bighashfl(const char* str, int maxlen) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int len = 0;
  while(len++ < maxlen)
    hash = ((hash << 5) + hash) ^ *str++;    /* hash* 33 + c */
  return hash % BIGHASHSIZE;
}

HASHTABLE* htctor(void) {
  HASHTABLE* ht = malloc(sizeof(HASHTABLE));
  for(int i = 0; i < HASHSIZE; i++) {
    ht->pairs[i].key = NULL;
    ht->pairs[i].next = NULL;
  }
  ht->keys = 0;
  return ht;
}

BIGHASHTABLE* bightctor(void) {
  BIGHASHTABLE* ht = malloc(sizeof(BIGHASHTABLE));
  for(int i = 0; i < BIGHASHSIZE; i++) {
    ht->pairs[i].key = NULL;
    ht->pairs[i].next = NULL;
  }
  ht->keys = 0;
  return ht;
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
  free(hp->key);
  if(hp->next) {
    hpdtor(hp->next);
  }
  free(hp);
}

void htdtor(HASHTABLE* ht) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      if(ht->pairs[i].next)
        hpdtor(ht->pairs[i].next);
    }
  }
  free(ht);
}

//hashpair destructor with custom free
static void hpdtorcfr(HASHPAIR* hp, void (*freep)(void*)) {
  if(hp->key) {
    free(hp->key);
    freep(hp->value);
    if(hp->next) {
      hpdtorcfr(hp->next, freep);
    }
  }
  free(hp);
}


//hashtable destructor with free
void htdtorfr(HASHTABLE* ht) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      free(ht->pairs[i].value);
      if(ht->pairs[i].next)
        hpdtorcfr(ht->pairs[i].next, free);
    }
  }
  free(ht);
}

//hashtable destructor with custom free
void htdtorcfr(HASHTABLE* ht, void (*freep)(void*)) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      freep(ht->pairs[i].value);
      if(ht->pairs[i].next)
        hpdtorcfr(ht->pairs[i].next, freep);
    }
  }
  free(ht);
}

void bightdtor(BIGHASHTABLE* ht) {
  for(int i = 0; i < BIGHASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      if(ht->pairs[i].next)
        hpdtor(ht->pairs[i].next);
    }
  }
  free(ht);
}
//big hashtable destructor with custom free
void bightdtorcfr(BIGHASHTABLE* ht, void (*freep)(void*)) {
  for(int i = 0; i < BIGHASHSIZE; i++) {
    if(ht->pairs[i].key) {
      free(ht->pairs[i].key);
      freep(ht->pairs[i].value);
      if(ht->pairs[i].next)
        hpdtorcfr(ht->pairs[i].next, freep);
    }
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
    HASHPAIR* newpair = malloc(sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    newpair->next = NULL;
    hp->next = newpair;
  }
  ++ht->keys;
}

void biginsert(BIGHASHTABLE* ht, const char* key, void* value) {
  unsigned long i = bighash(key);
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
    HASHPAIR* newpair = malloc(sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    newpair->next = NULL;
    hp->next = newpair;
  }
  ++ht->keys;
}

//insert into hashtable, with custom free on previous element
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
    HASHPAIR* newpair = malloc(sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->value = value;
    newpair->next = NULL;
    hp->next = newpair;
  }
  ++ht->keys;
}

void bigfinsertfr(BIGHASHTABLE* ht, char* key, void* value, int len) {
  unsigned long i = bighashfl(key, len);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key)) {
    hp->key = key;
    hp->value = value;
  } else {
    for(; hp->next; hp = hp->next) {
      if(!memcmp(hp->key, key, len)) {
        //free(hp->value);
        free(key);
        hp->value = value;
        return;
      }
    }
    if(!memcmp(hp->key, key, len)) {
      //free(hp->value);
      free(key);
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = malloc(sizeof(HASHPAIR));
    newpair->key = key;
    newpair->value = value;
    newpair->next = NULL;
    hp->next = newpair;
  }
  ++ht->keys;
}

void rmpair(HASHTABLE* ht, const char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return;
  HASHPAIR* prev = NULL;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      free(hp->key);
      if(hp->next) {
        HASHPAIR* temp = hp->next;
        *hp = *hp->next;
        free(temp);
      } else {
        hp->key = NULL;
        if(prev) {
          prev->next = NULL;
          free(hp);
        }
      }
      --ht->keys;
      return;
    }
    prev = hp;
  }
}

//remove hashpair with custom free
void rmpaircfr(HASHTABLE* ht, const char* key, void (*cfree)(void*)) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return;
  HASHPAIR* prev = NULL;
  for(; hp; hp = hp->next) {
    if(!strcmp(hp->key, key)) {
      free(hp->key);
      cfree(hp->value);
      if(hp->next) {
        HASHPAIR* temp = hp->next;
        memcpy(hp, hp->next, sizeof(HASHPAIR));
        free(temp);
      } else {
        hp->key = NULL;
        if(prev) {
          prev->next = NULL;
          free(hp);
        }
      }
      --ht->keys;
      return;
    }
    prev = hp;
  }
}

//remove hashpair from bighashtable with custom free
void bigrmpaircfr(BIGHASHTABLE* ht, const char* key, void (*cfree)(void*), int flen) {
  unsigned long i = flen > 0 ? bighashfl(key, flen) : bighash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->key))
    return;
  HASHPAIR* prev = NULL;
  for(; hp; hp = hp->next) {
    if(flen > 0 ? !memcmp(hp->key, key, flen) : !strcmp(hp->key, key)) {
      free(hp->key);
      cfree(hp->value);
      if(hp->next) {
        HASHPAIR* temp = hp->next;
        memcpy(hp, hp->next, sizeof(HASHPAIR));
        free(temp);
      } else {
        hp->key = NULL;
        if(prev) {
          prev->next = NULL;
          free(hp);
        }
      }
      --ht->keys;
      return;
    }
    prev = hp;
  }
}

void* search(HASHTABLE* ht, const char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(hp->key) {
    for(; hp; hp = hp->next) {
      if(!strcmp(hp->key, key))
        return hp->value;
    }
  }
  return NULL;
}

void* bigsearch(BIGHASHTABLE* ht, const char* key, int flen) {
  unsigned long i = flen > 0 ? bighashfl(key, flen) : bighash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(hp->key) {
    for(; hp; hp = hp->next) {
      if(flen > 0 ? !memcmp(hp->key, key, flen) : !strcmp(hp->key, key))
        return hp->value;
    }
  }
  return NULL;
}

char queryval(HASHTABLE* ht, const char* key) {
  unsigned long i = hash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(hp->key) {
    for(; hp; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
        return 1;
      }
    }
  }
  return 0;
}

char bigqueryval(BIGHASHTABLE* ht, const char* key) {
  unsigned long i = bighash(key);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(hp->key) {
    for(; hp; hp = hp->next) {
      if(!strcmp(hp->key, key)) {
        return 1;
      }
    }
  }
  return 0;
}

DYNARR* htpairs(HASHTABLE* ht) {
  DYNARR* da = dactor(ht->keys);
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

char htequal(HASHTABLE* ht1, HASHTABLE* ht2) {
  if(!ht1) if(ht2) return 0;
  if(!ht2) return 0;
  for(int i = 0; i < HASHSIZE; i++) {
    HASHPAIR* current1 = &(ht1->pairs[i]);
    HASHPAIR* current2 = &(ht2->pairs[i]);
    if(current1->key) {
      do {
        if(!((current1->key == current2->key) && (current1->value == current2->value))) return 0;
        current1 = current1->next;
        current2 = current2->next;
      } while(current1 && current2);
      if(current1 || current2) return 0;
    } else if(current2->key) {
      return 0;
    }
  }
  return 1;
}

//insert into hashtable with integer (rather than pointer) value
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
    HASHPAIR* newpair = malloc(sizeof(HASHPAIR));
    newpair->key = strdup(key);
    newpair->ivalue = value;
    newpair->next = NULL;
    hp->next = newpair;
  }
  ++ht->keys;
}
