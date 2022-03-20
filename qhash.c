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

static unsigned long longhash(long data) {
  data = (data ^ (data >> 30)) * 0xbf58476d1ce4e5b9l;
  data = (data ^ (data >> 27)) * 0x94d049bb133111ebl;
  return data ^ (data >> 31);
}

#define HASHIMPL(type_prefix, prefix, hashfunc, cmpfunc, freefunc, dupfunc, keytype, valtype) \
type_prefix ## TABLE* prefix ## htctor(void) { \
  type_prefix ## TABLE* ht = malloc(sizeof(type_prefix ## TABLE)); \
  ht->keys = 0; \
  ht->slotmask = HASHSIZE - 1; \
  ht->hashtable = malloc(sizeof(type_prefix ## PAIR) * HASHSIZE); \
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
        bfset(qh->bf, hashloc); \
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
  BITFIELD oldbf = qh->bf; \
  qh->slotmask = (qh->slotmask << 1) | 1; \
  qh->hashtable = newq; \
  qh->bf = bfalloc(qh->slotmask + 1); \
  for(int i = 0; i <= qoldsize; i++) { \
    if(bfget(oldbf, i)) { \
      prefix ## resizeinsert(qh, oldhashtable[i].key, oldhashtable[i].value); /*if this needs to resize we've got no problem*/ \
    } \
  } \
  free(oldbf); \
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
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(bfget(qh->bf, hashloc)) { \
        if(cmpfunc(qhp->key, key)) { \
          qhp->value = value; \
          break; \
        } \
      } else { \
        qhp->key = dupfunc(key); \
        qhp->value = value; \
        bfset(qh->bf, hashloc); \
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
    int hashloc = (hashval + i * i) & qh->slotmask; \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(cmpfunc(qhp->key, key)) { \
        return qhp->value; \
      } \
    } \
  } \
  return (valtype) 0; \
} \
char prefix ## queryval(type_prefix ## TABLE* qh, const keytype key) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = (hashval + i * i) & qh->slotmask; \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(cmpfunc(qhp->key, key)) { \
        return 1; \
      } \
    } \
  } \
  return 0; \
} \
void prefix ## rmpair(type_prefix ## TABLE* qh, const keytype key) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = (hashval + i * i) & qh->slotmask; \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(cmpfunc(qhp->key, key)) { \
        freefunc(qhp->key); \
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
      if(bfget(ht->bf, i)) { \
        type_prefix ## PAIR *current = ht->hashtable + i; \
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
} \
void prefix ## chtdtor(type_prefix ## TABLE* ht, void (*freep)(valtype)) { \
  if(ht->keys != 0) { \
    for(int i = 0; i <= ht->slotmask; i++) { \
      if(bfget(ht->bf, i)) { \
        type_prefix ## PAIR* current = &(ht->hashtable[i]); \
        freefunc(current->key); \
        freep(current->value); \
        if(!--ht->keys) break; \
      } \
    } \
  } \
  free(ht->bf); \
  free(ht->hashtable); \
  free(ht); \
} \
void prefix ## rmpaircfr(type_prefix ## TABLE* qh, const keytype key, void (*cfree)(valtype)) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = ((hashval + i * i) & qh->slotmask); \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
      if(cmpfunc(qhp->key, key)) { \
        freefunc(qhp->key); \
        cfree(qhp->value); \
        --qh->keys; \
        bfunset(qh->bf, hashloc); \
        break; \
      } \
    } else { \
      break; \
    } \
  } \
} \
char prefix ## htequal(type_prefix ## TABLE* ht1, type_prefix ## TABLE* ht2) { \
  if(ht1) if(!ht2) return 0; \
  if(!ht1) return 0; \
  if(ht1->slotmask != ht2->slotmask || \
     ht1->keys != ht2->keys) return 0; \
  for(int i = 0; i <= ht1->slotmask; i++) { \
    if(bfget(ht1->bf, i)) { \
      if(bfget(ht2->bf, i)) { \
        type_prefix ## PAIR* current1 = &(ht1->hashtable[i]); \
        type_prefix ## PAIR* current2 = &(ht2->hashtable[i]); \
        if(current1->key != current2->key) return 0; /*we compare only keys, not values*/\
      } else { \
        return 0; \
      } \
    } else if(bfget(ht2->bf, i)) { \
      return 0; \
    } \
  } \
  return 1; \
}


HASHIMPL(QHASH, q, qhash, !strcmp, free, strdup, char*, void*)
#define NOP(X) (void) X
#define VERBATIM(X) X
#define COMPARATOR(i1, i2) ((i1) == (i2))
HASHIMPL(IIHASH, ii, inthash, COMPARATOR, NOP, VERBATIM, int, int)
HASHIMPL(LVHASH, lv, longhash, COMPARATOR, NOP, VERBATIM, long, void*)
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


IIHASHTABLE* iiclone(IIHASHTABLE* ht) {
  IIHASHTABLE* newht = malloc(sizeof(IIHASHTABLE));
  newht->keys = ht->keys;
  newht->slotmask = ht->slotmask;
  newht->hashtable = malloc((ht->slotmask + 1) * sizeof(IIHASHPAIR));
  newht->bf = bfclone(ht->bf, ht->slotmask + 1);
  memcpy(newht->hashtable, ht->hashtable, (ht->slotmask + 1) * sizeof(IIHASHPAIR));
  return newht;
}

LVHASHTABLE* lvhtcclone(LVHASHTABLE* ht, void*(*clonefunc)(void*)) {
  LVHASHTABLE* newht = malloc(sizeof(LVHASHTABLE));
  newht->keys = ht->keys;
  newht->slotmask = ht->slotmask;
  newht->hashtable = malloc((ht->slotmask + 1) * sizeof(LVHASHPAIR));
  newht->bf = bfclone(ht->bf, ht->slotmask + 1);
  for(int i = 0; i <= ht->slotmask; i++) {
      if(bfget(newht->bf, i)) {
          newht->hashtable[i].key = ht->hashtable[i].key;
          newht->hashtable[i].value = clonefunc(ht->hashtable[i].value);
      }
  }
  return newht;
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
