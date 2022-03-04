#include "qhash.h"

static int hash(QHASHTABLE* qh, const char* str) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while((c = *str++))
    hash = ((hash << 5) + hash) ^ c;    /* hash* 33 + c */
  return hash & qh->slotmask;
}

QHASHTABLE* htctor(void) {
  QHASHTABLE* ht = malloc(sizeof(QHASHTABLE));
  ht->keys = 0;
  ht->slotmask = HASHSIZE - 1;
  ht->hashtable = calloc(sizeof(QHASHPAIR), HASHSIZE);
  return ht;
}
QHASHTABLE* chtctor(int size) {
  QHASHTABLE* ht = malloc(sizeof(QHASHTABLE));
  assert((size & (size-1)) == 0); //assert that the size is a power of 2
  ht->keys = 0;
  ht->slotmask = size - 1;
  ht->hashtable = calloc(sizeof(QHASHPAIR), size);
  return ht;
}

static void resizeinsert(QHASHTABLE* qh, const char* key, void* value) {
  int hashval = hash(qh, key);
  int i;
  do {
    for(i = 0; i < PROBECOUNT; i++) {
      QHASHPAIR *qhp = qh->hashtable + ((hashval + i * i) & qh->slotmask);
      if(!qhp->key) {
        qhp->key = (char*) key;
        qhp->value = value;
        break;
      }
    }
    if(i == PROBECOUNT) {
      resize(qh);
    } else {
      break;
    }
  } while(1);
}
void resize(QHASHTABLE* qh) {
  QHASHPAIR* oldhashtable = qh->hashtable;
  int qoldsize = qh->slotmask;
  int qoldkeys = qh->keys;
  QHASHPAIR* newq = calloc((qh->slotmask + 1) << 1, sizeof(QHASHPAIR));
  qh->slotmask = (qh->slotmask << 1) | 1;
  qh->hashtable = newq;
  for(int i = 0; i <= qoldsize; i++) {
    if(oldhashtable[i].key) {
      resizeinsert(qh, oldhashtable[i].key, oldhashtable[i].value); //if this needs to resize we've got no problem
    }
  }
  qh->keys = qoldkeys;
  free(oldhashtable);
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
      if(!strcmp(qhp->key, key)) {
        qhp->value = value;
      }
    }
    if(i == PROBECOUNT) {
      resize(qh);
    } else {
      break;
    }
  } while(1);

  ++qh->keys;
}

void insertcfr(QHASHTABLE* qh, const char* key, void* value, void (*cfree)(void*)) {
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
      if(!strcmp(qhp->key , key)) {
        free((void*) key);
        qhp->value = value;
      }
    }
    if(i == PROBECOUNT) {
      resize(qh);
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
        if(! --ht->keys) break;
      }
    }
  }
  free(ht->hashtable);
  free(ht);
}

DYNARR* htpairs(QHASHTABLE* ht) {
  DYNARR* da = dactor(ht->keys);
  for(int i = 0; i <= ht->slotmask; i++) {
    QHASHPAIR* curpair = &(ht->hashtable[i]);
    if(curpair->key) dapush(da, curpair);
  }
  return da;
}

/*
#include "stdio.h"
int main() {
  QHASHTABLE* q = htctor();
  QHASHTABLE* q2 = chtctor(16);
  insert(q, "asdf", strdup("hooray"));
  insert(q2, "foo", strdup("bar"));
  for(int i = 0; i < 0x2000; i++) {
    char numstrbuffer[10];
    snprintf(numstrbuffer, 10, "%d", i);
    insert(q, numstrbuffer, strdup("number"));
  }
  for(int i = -76; i < 0x1000; i++) {
    char numstrbuffer[10];
    snprintf(numstrbuffer, 10, "%d", i);
    rmpaircfr(q, numstrbuffer, free);
  }
  printf("queried %d for asdf\n", queryval(q, "asdf"));
  printf("searched %s for foo\n", (char*) search(q2, "foo"));
  printf("searched %s for shoe\n", (char*) search(q2, "shoe"));
  printf("queried %d for banana\n", queryval(q, "banana"));
  printf("searched %s for 6459\n", (char*) search(q, "6459"));

  for(int i = 0x1100; i < 0x2000; i++) {
    char numstrbuffer[10];
    snprintf(numstrbuffer, 10, "%d", i);
    rmpaircfr(q, numstrbuffer, free);
  }

  DYNARR* da = htpairs(q);
  printf("{\n");
  for(int i = 0; i < da->length; i++) {
    QHASHPAIR* qhp = daget(da, i);
    printf("%s: %s\n", qhp->key, (char*) qhp->value);
  }
  printf("}\n");

  dadtor(da);
  htdtor(q, free);
  htdtor(q2, free);
}
*/
