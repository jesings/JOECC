#include "hash.h"
static unsigned long fixedhash(long data) {
  data = (data ^ (data >> 30)) * 0xbf58476d1ce4e5b9l;
  data = (data ^ (data >> 27)) * 0x94d049bb133111ebl;
  data = data ^ (data >> 31);
  return data % HASHSIZE;
}
void fixedinsert(HASHTABLE* ht, long fixedkey, void* value) {
  unsigned long i = fixedhash(fixedkey);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!hp->next) {
    hp->fixedkey = fixedkey;
    hp->value = value;
    hp->next = (void*) 1;
  } else {
    for(; (unsigned long) hp->next > 1; hp = hp->next) {
      if(hp->fixedkey == fixedkey) {
        hp->value = value;
        return;
      }
    }
    if(hp->next && hp->fixedkey == fixedkey) {
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = malloc(sizeof(HASHPAIR));
    newpair->fixedkey = fixedkey;
    newpair->value = value;
    newpair->next = (void*) 1;
    hp->next = newpair;
  }
  ++ht->keys;
}
void* fixedsearch(HASHTABLE* ht, long fixedkey) {
  unsigned long i = fixedhash(fixedkey);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!hp->next)
    return NULL;
  for(; (unsigned long) hp > 1; hp = hp->next) {
    if(hp->next && hp->fixedkey == fixedkey)
      return hp->value;
  }
  return NULL;
}

char fixedqueryval(HASHTABLE* ht, long fixedkey) {
  unsigned long i = fixedhash(fixedkey);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!hp->next)
    return 0;
  for(; (unsigned long) hp > 1; hp = hp->next) {
    if(hp->next && hp->fixedkey == fixedkey)
      return 1;
  }
  return 0;
}

HASHTABLE* fhtclone(HASHTABLE* ht) {
  HASHTABLE* retval = malloc(sizeof(HASHTABLE));
  memcpy(retval, ht, sizeof(HASHTABLE));
  for(int i = 0; i < HASHSIZE; i++) {
    HASHPAIR* curpair = &(ht->pairs[i]), *parpar = &(retval->pairs[i]);
    if(!(curpair->next))
      continue;
    do {
      parpar->fixedkey = curpair->fixedkey;
      parpar->value = curpair->value;
      if((unsigned long)curpair->next == 1) {
        parpar->next = (void*) 1;
        break;
      } else {
        parpar = parpar->next = malloc(sizeof(HASHPAIR));
        curpair = curpair->next;
      }
    } while(1);
  }
  return retval;
}

//fixed hashpair destructor with custom free
static void fhpdtorcfr(HASHPAIR* hp, void(*freef)(void*)) {
  if(hp->next) {
    freef(hp->value);
    if((unsigned long) hp->next > 1) fhpdtorcfr(hp->next, freef);
  }
  free(hp);
}

//fixed hashtable destructor with custom free
void fhtdtorcfr(HASHTABLE* ht, void(*freef)(void*)) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].next) {
      freef(ht->pairs[i].value);
    }
    if((unsigned long) ht->pairs[i].next > 1) {
      fhpdtorcfr(ht->pairs[i].next, freef);
    }
  }
  free(ht);
}

DYNARR* htfpairs(HASHTABLE* ht) {
  DYNARR* da = dactor(256);
  for(int i = 0; i < HASHSIZE; i++) {
    HASHPAIR* current = &(ht->pairs[i]);
    if(current->next) {
      dapush(da, current);
      while((unsigned long) current->next > 1) {
        current = current->next;
        dapush(da, current);
      }
    }
  }
  return da;
}

//fixed hashpair destructor
static void fhpdtor(HASHPAIR* hp) {
  if((unsigned long) hp->next > 1) {
    fhpdtor(hp->next);
  }
  free(hp);
}

//fixed hashtable destructor
void fhtdtor(HASHTABLE* ht) {
  for(int i = 0; i < HASHSIZE; i++) {
    if(ht->pairs[i].next) {
      if((long) ht->pairs[i].next > 1)
        fhpdtor(ht->pairs[i].next);
    }
  }
  free(ht);
}

//fixed hashtable remove hashpair
void frmpair(HASHTABLE* ht, long fixedkey) {
  long i = fixedhash(fixedkey);
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!(hp->fixedkey))
    return;
  HASHPAIR* prev = NULL;
  for(; hp; hp = hp->next) {
    if(hp->next && hp->fixedkey == fixedkey) {
      if((unsigned long) hp->next > 1) {
        HASHPAIR* temp = hp->next;
        *hp = *hp->next;
        free(temp);
      } else {
        if(prev) {
          prev->next = (void*) 1;
          free(hp);
        } else {
          hp->next = NULL;
        }
      }
      --ht->keys;
      return;
    }
    prev = hp;
  }
}
