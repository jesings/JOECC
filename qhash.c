#include "qhash.h"

static int hash(QHASHTABLE* qh, const char* str) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while((c = *str++))
    hash = ((hash << 5) + hash) ^ c;    /* hash* 33 + c */
  return hash & qh->slotmask;
}

QHASHTABLE* qhtctor(void) {
  QHASHTABLE* ht = malloc(sizeof(QHASHTABLE));
  ht->keys = 0;
  ht->slotmask = HASHSIZE - 1;
  ht->hashtable = calloc(sizeof(QHASHPAIR), HASHSIZE);
  return ht;
}

void qresize(QHASHTABLE* qh) {
  QHASHPAIR* oldhashtable = qh->hashtable;
  int qoldsize = qh->slotmask;
  QHASHPAIR* newq = calloc((qh->slotmask + 1) >> 2, sizeof(QHASHPAIR));
  qh->keys = 0;
  qh->slotmask = ((qh->slotmask + 1) << 2) - 1;
  qh->hashtable = newq;
  for(int i = 0; i <= qoldsize; i++) {
    if(oldhashtable[i].key) {
      insert(qh, oldhashtable[i].key, oldhashtable[i].value); //if this needs to resize we've got no problem
    }
  }
}

void insert(QHASHTABLE* qh, const char* key, void* value) {
  int hashval = hash(qh, key);
  int i;
  do {
    for(i = 0; i < PROBECOUNT; i++) {
      QHASHPAIR *qhp = qh->hashtable + ((hashval + i * i) & qh->slotmask);
      if(!qhp->key) {
        qhp->key = strdup(key);
        qhp->value = value;
        break;
      }
    }
    if(i == PROBECOUNT) {
      qresize(qh);
    } else {
      break;
    }
  } while(1);

  ++qh->keys;
}
