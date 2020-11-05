#include "hash.h"
static unsigned long fixedhash(long data, char lbits) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  data = (data ^ (data >> 30)) * 0xbf58476d1ce4e5b9l;
  data = (data ^ (data >> 27)) * 0x94d049bb133111ebl;
  data = data ^ (data >> 31);
  return data % HASHSIZE;
}
void fixedinsert(HASHTABLE* ht, long fixedkey, void* value) {
  unsigned long i = fixedhash(fixedkey, sizeof(fixedkey));
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!hp->next) {
    hp->fixedkey = fixedkey;
    hp->value = value;
    hp->next = (void*) 1;
  } else {
    for(; (long) hp->next > 1; hp = hp->next) {
      if(hp->next && hp->fixedkey != fixedkey) {
        hp->value = value;
        hp->next = (void*) 1;
        return;
      }
    }
    if(hp->next && hp->fixedkey == fixedkey) {
      hp->value = value;
      return;
    }
    HASHPAIR* newpair = calloc(1, sizeof(HASHPAIR));
    newpair->fixedkey = fixedkey;
    newpair->value = value;
    newpair->next = (void*) 1;
    hp->next = newpair;
  }
  ++ht->keys;
}
void* fixedsearch(HASHTABLE* ht, long fixedkey) {
  unsigned long i = fixedhash(fixedkey, sizeof(fixedkey));
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!hp->next)
    return NULL;
  for(; (long) hp > 1; hp = hp->next) {
    if(hp->next && hp->fixedkey == fixedkey)
      return hp->value;
  }
  return NULL;
}

char fixedqueryval(HASHTABLE* ht, long fixedkey) {
  unsigned long i = fixedhash(fixedkey, sizeof(fixedkey));
  HASHPAIR* hp = &(ht->pairs[i]);
  if(!hp->next)
    return 0;
  for(; (long) hp > 1; hp = hp->next) {
    if(hp->next && hp->fixedkey == fixedkey)
      return 1;
  }
  return 0;
}

