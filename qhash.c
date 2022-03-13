#include "qhash.h"


static int qhash(const char* str) {    /*courtesy of http://www.cse.yorku.ca/~oz/hash.html */
  unsigned long hash = 5381;
  int c;
  while((c = *str++))
    hash = ((hash << 5) + hash) ^ c;    /* hash* 33 + c */
  return hash;
}
static unsigned int inthash(unsigned int x) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

#define HASHIMPL(type_prefix, prefix, hashfunc, cmpfunc, freefunc, dupfunc, keytype, valtype) \
type_prefix ## TABLE* prefix ## htctor(void) { \
  type_prefix ## TABLE* ht = malloc(sizeof(type_prefix ## TABLE)); \
  ht->keys = 0; \
  ht->slotmask = HASHSIZE - 1; \
  ht->hashtable = calloc(sizeof(type_prefix ## PAIR), HASHSIZE); \
  ht->bf = bfalloc(HASHSIZE); \
  return ht; \
} \
type_prefix ## TABLE* prefix ## chtctor(int size) { \
  type_prefix ## TABLE* ht = malloc(sizeof(type_prefix ## TABLE)); \
  assert((size & (size-1)) == 0); /*assert that the size is a power of 2*/ \
  ht->keys = 0; \
  ht->slotmask = size - 1; \
  ht->hashtable = calloc(sizeof(type_prefix ## PAIR), size); \
  ht->bf = bfalloc(size); \
  return ht; \
} \
static void prefix ## resizeinsert(type_prefix ## TABLE* qh, keytype key, valtype value) { \
  int hashval; \
  int i; \
  do { \
    hashval = hashfunc(key); \
    for(i = 0; i < PROBECOUNT; i++) { \
      int hashloc = ((hashval + i * i) & qh->slotmask); \
      if(!bfget(qh->bf, hashloc)) { \
        type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
        qhp->key = (keytype) key; \
        qhp->value = value; \
        break; \
      } \
    } \
    if(i == PROBECOUNT) { \
      prefix ## resize(qh); \
    } else { \
      break; \
    } \
  } while(1); \
} \
void prefix ## resize(type_prefix ## TABLE* qh) { \
  type_prefix ## PAIR* oldhashtable = qh->hashtable; \
  int qoldsize = qh->slotmask; \
  int qoldkeys = qh->keys; \
  type_prefix ## PAIR* newq = calloc((qh->slotmask + 1) << 1, sizeof(type_prefix ## PAIR)); \
  qh->slotmask = (qh->slotmask << 1) | 1; \
  qh->hashtable = newq; \
  free(qh->bf); \
  qh->bf = bfalloc(qh->slotmask + 1); \
  for(int i = 0; i <= qoldsize; i++) { \
    if(oldhashtable[i].key) { \
      prefix ## resizeinsert(qh, oldhashtable[i].key, oldhashtable[i].value); /*if this needs to resize we've got no problem*/ \
    } \
  } \
  qh->keys = qoldkeys; \
  free(oldhashtable); \
} \
void prefix ## insert(type_prefix ## TABLE* qh, const keytype key, valtype value) { \
  int hashval; \
  int i; \
  do { \
    hashval = hashfunc(key); \
    for(i = 0; i < PROBECOUNT; i++) { \
      int hashloc = ((hashval + i * i) & qh->slotmask); \
      if(!bfget(qh->bf, hashloc)) { \
        type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
        qhp->key = dupfunc(key); \
        qhp->value = value; \
        bfset(qh->bf, hashloc); \
        break; \
      } \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(!cmpfunc(qhp->key, key)) { \
        qhp->value = value; \
        break; \
      } \
    } \
    if(i == PROBECOUNT) { \
      prefix ## resize(qh); \
    } else { \
      break; \
    } \
  } while(1); \
 \
  ++qh->keys; \
} \
valtype prefix ## search(type_prefix ## TABLE* qh, const keytype key) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = ((hashval + i * i) & qh->slotmask); \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(!cmpfunc(qhp->key, key)) { \
        return qhp->value; \
      } \
    } \
  } \
  return (valtype) 0; \
} \
char prefix ## queryval(type_prefix ## TABLE* qh, const keytype key) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = ((hashval + i * i) & qh->slotmask); \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(!cmpfunc(qhp->key, key)) { \
        return 1; \
      } \
    } \
  } \
  return 0; \
} \
void prefix ## rmpair(type_prefix ## TABLE* qh, const keytype key) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = ((hashval + i * i) & qh->slotmask); \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(!cmpfunc(qhp->key, key)) { \
        freefunc(qhp->key); \
        qhp->key = (keytype) 0; \
        --qh->keys; \
        bfunset(qh->bf, hashloc); \
        break; \
      } \
    } \
  } \
} \
void prefix ## htdtor(type_prefix ## TABLE* ht) { \
  if(ht->keys != 0) { \
    for(int i = 0; i <= ht->slotmask; i++) { \
      type_prefix ## PAIR* current = &(ht->hashtable[i]); \
      if(current->key) { \
        freefunc(current->key); \
        if(! --ht->keys) break; \
      } \
    } \
  } \
  free(ht->bf); \
  free(ht->hashtable); \
  free(ht); \
} \
DYNARR* prefix ## htpairs(type_prefix ## TABLE* ht) { \
  DYNARR* da = dactor(ht->keys); \
  for(int i = 0; i <= ht->slotmask; i++) { \
    if(bfget(ht->bf, i)) { \
      type_prefix ## PAIR* curpair = &(ht->hashtable[i]); \
      dapush(da, curpair); \
    } \
  } \
  return da; \
}

HASHIMPL(QHASH, q, qhash, strcmp, free, strdup, char*, void*)
#define NOP(X) 
#define VERBATIM(X) X
#define COMPARATOR(i1, i2) ((i1) == (i2))
HASHIMPL(IIHASH, ii, inthash, COMPARATOR, NOP, VERBATIM, int, int)
#undef COMPARATOR
#undef VERBATIM
#undef NOP

void qinsertcfr(QHASHTABLE* qh, const char* key, void* value, void (*cfree)(void*)) {
  int hashval;
  int i;
  do {
    hashval = qhash(key);
    for(i = 0; i < PROBECOUNT; i++) {
      int hashloc = ((hashval + i * i) & qh->slotmask);
      if(!bfget(qh->bf, hashloc)) {
        QHASHPAIR *qhp = qh->hashtable + hashloc;
        qhp->key = strdup(key);
        qhp->value = value;
        bfset(qh->bf, hashloc);
        break;
      }
      QHASHPAIR *qhp = qh->hashtable + hashloc;
      if(!strcmp(qhp->key , key)) {
        free((void*) qhp->value);
        qhp->value = value;
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


void qrmpaircfr(QHASHTABLE* qh, const char* key, void (*cfree)(void*)) {
  int hashval = qhash(key);
  for(int i = 0; i < PROBECOUNT; i++) {
    int hashloc = ((hashval + i * i) & qh->slotmask);
    if(bfget(qh->bf, hashloc)) {
      QHASHPAIR *qhp = qh->hashtable + hashloc;
      if(!strcmp(qhp->key, key)) {
        free(qhp->key);
        cfree(qhp->value);
        qhp->key = NULL;
        --qh->keys;
        bfunset(qh->bf, hashloc);
        break;
      }
    } else {
      break;
    }
  }
}

char qhtequal(QHASHTABLE* ht1, QHASHTABLE* ht2) {
  if(ht1) if(!ht2) return 0;
  if(!ht1) return 0;
  if(ht1->slotmask != ht2->slotmask ||
     ht1->keys != ht2->keys) return 0;
  for(int i = 0; i <= ht1->slotmask; i++) {
    if(bfget(ht1->bf, i)) {
      if(bfget(ht2->bf, i)) {
        QHASHPAIR* current1 = &(ht1->hashtable[i]);
        QHASHPAIR* current2 = &(ht2->hashtable[i]);
        if(current1->key != current2->key) return 0;
        //we don't compare values actually
      } else {
        return 0;
      }
    } else if(bfget(ht2->bf, i)) {
      return 0;
    }
  }
  return 1;
}

void qchtdtor(QHASHTABLE* ht, void (*freep)(void*)) {
  if(ht->keys != 0) {
    for(int i = 0; i <= ht->slotmask; i++) {
      if(bfget(ht->bf, i)) {
        QHASHPAIR* current = &(ht->hashtable[i]);
        free(current->key);
        freep(current->value);
        if(!--ht->keys) break;
      }
    }
  }
  free(ht->bf);
  free(ht->hashtable);
  free(ht);
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
