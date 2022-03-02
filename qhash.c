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

void* search(QHASHTABLE* qh, const char* key) {
  int hashval = hash(qh, key);
  for(int i = 0; i < PROBECOUNT; i++) {
    QHASHPAIR *qhp = qh->hashtable + ((hashval + i * i) & qh->slotmask);
    if(qhp->key) {
      if(!strcmp(qhp->key, key)) {
        return qhp->value;
      }
    } else {
      break;
    }
  }
  return NULL;
}
char queryval(QHASHTABLE* qh, const char* key) {
  int hashval = hash(qh, key);
  for(int i = 0; i < PROBECOUNT; i++) {
    QHASHPAIR *qhp = qh->hashtable + ((hashval + i * i) & qh->slotmask);
    if(qhp->key) {
      if(!strcmp(qhp->key, key)) {
        return 1;
      }
    } else {
      break;
    }
  }
  return 0;
}

void rmpaircfr(QHASHTABLE* qh, const char* key, void (*cfree)(void*)) {
  int hashval = hash(qh, key);
  for(int i = 0; i < PROBECOUNT; i++) {
    QHASHPAIR *qhp = qh->hashtable + ((hashval + i * i) & qh->slotmask);
    if(qhp->key) {
      if(!strcmp(qhp->key, key)) {
        free(qhp->key);
        cfree(qhp->value);
        qhp->key = NULL;
        break;
      }
    } else {
      break;
    }
  }
}

char htequal(QHASHTABLE* ht1, QHASHTABLE* ht2) {
  if(ht1) if(!ht2) return 0;
  if(!ht1) return 0;
  if(ht1->slotmask != ht2->slotmask ||
     ht1->keys != ht2->keys) return 0;
  for(int i = 0; i <= ht1->slotmask; i++) {
    QHASHPAIR* current1 = &(ht1->hashtable[i]);
    QHASHPAIR* current2 = &(ht2->hashtable[i]);
    if(current1->key != current2->key) return 0;
    //we don't compare values actually
  }
  return 1;
}

void htdtor(QHASHTABLE* ht, void (*freep)(void*)) {
  if(ht->keys != 0) {
    for(int i = 0; i <= ht->slotmask; i++) {
      QHASHPAIR* current = &(ht->hashtable[i]);
      if(current->key) {
        free(current->key);
        freep(current->value);
        if(--ht->keys) break;
      }
    }
  }
  free(ht->hashtable);
  free(ht);
}
