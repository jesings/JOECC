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
static int memophash(const void* mem) {
    const long* memlong = mem; //16 bytes which is the length of op, is 2 longs
    return longhash(memlong[0]) + longhash(memlong[1]);
}

static unsigned long doublehash(double data) {
    return longhash(*(long*)&data);
}


static char opmemcmp(const void* mem1, const void* mem2) {
    return memcmp(mem1, mem2, 16);
}
static void* opmemdup(const void* oldmem) {
    void* newmem = malloc(16);
    memcpy(newmem, oldmem, 16);
    return newmem;
}


/**
 * The following macro, HASHIMPL, defines a factory for implementing a hashtable for the given types.
 * The parameters of the macro are as follows:
 * type_prefix defines the prefix to be added on to TABLE for the type of the hash table: i.e. ARR for DYNARR
 * prefix defines the prefix to be put before the functions to differentiate them: i.e. qh -> qhtctor ii -> iisearch
 * hashfunc specifies the function that is used to hash the keys for the hashtable
 * cmpfunc specifies the function that is used in order to compare the equality of 2 different keys (not their hashed values
 * freefunc specifies the function that is used to free the keys when we remove or destroy etc.
 * dupfunc specifies the function that is used to duplicate the key from the param when we insert
 * keytype defines the type of the key of the hashtable
 * valtype defines the type of the value of the hashtable
 *
 * Calling this macro on types with a prefix defines the following functions (prefix omitted
 * htctor, which constructs/allocates a hashtable of the given types which can store HASHSIZE values
 * chtctor, which constructs/allocates a hastable of the given types of a specified size
 * resizeinsert, a static function not intended to be called by users which inserts an element of the old backing hashtable after we have run out of space and allocated a new backing hashtable
 * resize, a function not intended to be called by users, which expands the backing store of the hashtable if we have too many collisions (see PROBECOUNT)
 * insert, which inserts a key, value pair of the requisite type into the hashtable, replacing an old value if one exists there. Insertion uses quadratic probing with a depth of PROBECOUNT
 * insertcfr, which inserts a key, value pair of the requisite type into the hashtable, replacing an old value if one exists there, and freeing the old value with the passed in function. Insertion uses quadratic probing with a depth of PROBECOUNT. 
 * queryval, which returns 1 if the key is present in the hashtable, otherwise it returns 0
 * search, which returns the value associated with a key if one exists otherwise returns 0/NULL
 * rmpair, which removes the key/value pair associated with the passed-in key if it exists
 * rmpaircfr, which removes the key/value pair associated with the passed-in key if it exists, and frees the value with the provided function if it exists
 * htdtor, which destructs/frees a hashtable
 * chtdtor, which destructs/frees a hashtable and runs the passed-in freeing function on all of the values remaining in the hashtable
 * htpairs which constructs a dynamic array which contains references to the key/value pairs present in the hashtable. Note that modifications to the hashtable may cause the pointers in the dynamic array over to have information we don't expect in them.
**/
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
  ht->hashtable = malloc(sizeof(type_prefix ## PAIR) * size); \
  ht->bf = bfalloc(size); \
  return ht; \
} \
static void prefix ## resizeinsert(type_prefix ## TABLE* qh, keytype key, valtype value) { \
  /*no need for any holes logic as nothing is ever removed before/in a resizeinsert*/ \
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
  type_prefix ## PAIR* newq = malloc(((qh->slotmask + 1) << 1) * sizeof(type_prefix ## PAIR)); \
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
  int minempty = -1; \
  while(1) { \
    hashval = hashfunc(key); \
    for(i = 0; i < PROBECOUNT; i++) { \
      int hashloc = ((hashval + i * i) & qh->slotmask); \
      if(bfget(qh->bf, hashloc)) { \
        type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
        if(cmpfunc(qhp->key, key)) { \
          qhp->value = value; \
          break; \
        } \
      } else { \
        if(minempty == -1) { \
          minempty = hashloc; \
        } \
      } \
    } \
    if(i == PROBECOUNT) { \
      if(minempty != -1) { \
        type_prefix ## PAIR *qhp = qh->hashtable + minempty; \
        qhp->key = dupfunc(key); \
        qhp->value = value; \
        bfset(qh->bf, minempty); \
        break; \
      } \
      prefix ## resize(qh); \
    } else { \
      break; \
    } \
  } \
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
void prefix ## insertcfr(type_prefix ## TABLE* qh, const keytype key, valtype value, void (*cfree)(valtype)) { \
  int hashval; \
  int i; \
  int minempty = -1; \
  while(1) { \
    hashval = hashfunc(key); \
    for(i = 0; i < PROBECOUNT; i++) { \
      int hashloc = ((hashval + i * i) & qh->slotmask); \
      if(bfget(qh->bf, hashloc)) { \
        type_prefix ## PAIR *qhp = qh->hashtable + hashloc; \
        if(cmpfunc(qhp->key, key)) { \
          cfree(qhp->value); \
          qhp->value = value; \
          break; \
        } \
      } else { \
        if(minempty == -1) { \
          minempty = hashloc; \
        } \
      } \
    } \
    if(i == PROBECOUNT) { \
      if(minempty != -1) { \
        type_prefix ## PAIR *qhp = qh->hashtable + minempty; \
        qhp->key = dupfunc(key); \
        qhp->value = value; \
        bfset(qh->bf, minempty); \
        break; \
      } else { \
        prefix ## resize(qh); \
      } \
    } else { \
      break; \
    } \
  } \
  ++qh->keys; \
}

/**
 * The following macro, SETIMPL, defines a factory for implementing a hashset for the given types.
 * The parameters of the macro are as follows:
 * type_prefix defines the prefix to be added on to TABLE for the type of the hashset: i.e. ARR for DYNARR
 * prefix defines the prefix to be put before the functions to differentiate them: i.e. qh -> qhtctor ii -> iisearch
 * hashfunc specifies the function that is used to hash the entries for the hashtable
 * cmpfunc specifies the function that is used in order to compare the equality of 2 different entries (not their hashed values
 * freefunc specifies the function that is used to free the entries when we remove or destroy etc.
 * dupfunc specifies the function that is used to duplicate the key from the param when we insert
 * keytype defines the type of the hashset entry
 *
 * Calling this macro on types with a prefix defines the following functions (prefix omitted
 * setctor, which constructs/allocates a hashset of the given types of a specified size
 * setresizeinsert, a static function not intended to be called by users which inserts an element of the old backing hashset after we have run out of space and allocated a new backing hashset
 * setresize, a function not intended to be called by users, which expands the backing store of the hashset if we have too many collisions (see PROBECOUNT)
 * setinsert, which inserts an entry into the hashset, replacing an old value if one exists there. Insertion uses quadratic probing with a depth of PROBECOUNT
 * setcontains, which returns 1 if the key is present in the hashset, otherwise it returns 0
 * setdtor, which destructs/frees a hash set
 * setclone which clones a hash table and returns a newly allocated hash table with the same entries as the previous one
**/
#define SETIMPL(type_prefix, prefix, hashfunc, cmpfunc, freefunc, dupfunc, keytype) \
type_prefix ## SET* prefix ## setctor(int size) { \
  type_prefix ## SET* ht = malloc(sizeof(type_prefix ## SET)); \
  assert((size & (size-1)) == 0); /*assert that the size is a power of 2*/ \
  ht->keys = 0; \
  ht->slotmask = size - 1; \
  ht->hashtable = malloc(sizeof(type_prefix ## SETENT) * size); \
  ht->bf = bfalloc(size); \
  return ht; \
} \
static void prefix ## setresizeinsert(type_prefix ## SET* qh, keytype key) { \
  /*no need for any holes logic as nothing is ever removed before/in a resizeinsert*/ \
  int hashval; \
  int i; \
  do { \
    hashval = hashfunc(key); \
    for(i = 0; i < PROBECOUNT; i++) { \
      int hashloc = (hashval + i * i) & qh->slotmask; \
      if(!bfget(qh->bf, hashloc)) { \
        type_prefix ## SETENT *qhp = qh->hashtable + hashloc; \
        qhp->key = (keytype) key; \
        bfset(qh->bf, hashloc); \
        break; \
      } \
    } \
    if(i == PROBECOUNT) { \
      prefix ## setresize(qh); \
    } else { \
      break; \
    } \
  } while(1); \
} \
void prefix ## setresize(type_prefix ## SET* qh) { \
  type_prefix ## SETENT* oldhashtable = qh->hashtable; \
  int qoldsize = qh->slotmask; \
  int qoldkeys = qh->keys; \
  type_prefix ## SETENT* newq = malloc(((qh->slotmask + 1) << 1) * sizeof(type_prefix ## SETENT)); \
  BITFIELD oldbf = qh->bf; \
  qh->slotmask = (qh->slotmask << 1) | 1; \
  qh->hashtable = newq; \
  qh->bf = bfalloc(qh->slotmask + 1); \
  for(int i = 0; i <= qoldsize; i++) { \
    if(bfget(oldbf, i)) { \
      prefix ## setresizeinsert(qh, oldhashtable[i].key); /*if this needs to resize we've got no problem*/ \
    } \
  } \
  free(oldbf); \
  qh->keys = qoldkeys; \
  free(oldhashtable); \
} \
void prefix ## setinsert(type_prefix ## SET* qh, const keytype key) { \
  int hashval; \
  int i; \
  int minempty = -1; \
  while(1) { \
    hashval = hashfunc(key); \
    for(i = 0; i < PROBECOUNT; i++) { \
      int hashloc = (hashval + i * i) & qh->slotmask; \
      if(bfget(qh->bf, hashloc)) { \
        type_prefix ## SETENT *qhp = qh->hashtable + hashloc; \
        if(cmpfunc(qhp->key, key)) \
          break; \
      } else { \
        if(minempty == -1) { \
          minempty = hashloc; \
        } \
      } \
    } \
    if(i == PROBECOUNT) { \
      if(minempty != -1) { \
        type_prefix ## SETENT *qhp = qh->hashtable + minempty; \
        qhp->key = dupfunc(key); \
        bfset(qh->bf, minempty); \
        break; \
      } \
      prefix ## setresize(qh); \
    } else { \
      break; \
    } \
  } \
  ++qh->keys; \
} \
char prefix ## setcontains(type_prefix ## SET* qh, const keytype key) { \
  int hashval = hashfunc(key); \
  for(int i = 0; i < PROBECOUNT; i++) { \
    int hashloc = (hashval + i * i) & qh->slotmask; \
    if(bfget(qh->bf, hashloc)) { \
      type_prefix ## SETENT *qhp = qh->hashtable + hashloc; \
      if(cmpfunc(qhp->key, key)) { \
        return 1; \
      } \
    } \
  } \
  return 0; \
} \
void prefix ## setdtor(type_prefix ## SET* ht) { \
  if(ht->keys != 0) { \
    for(int i = 0; i <= ht->slotmask; i++) { \
      if(bfget(ht->bf, i)) { \
        type_prefix ## SETENT *current = ht->hashtable + i; \
        freefunc(current->key); \
        if(! --ht->keys) break; \
      } \
    } \
  } \
  free(ht->bf); \
  free(ht->hashtable); \
  free(ht); \
} \
type_prefix ## SET* prefix ## setclone(type_prefix ## SET* qh) { \
    type_prefix ## SET* retval = malloc(sizeof(type_prefix ## SET)); \
    retval->keys = qh->keys; \
    retval->slotmask = qh->slotmask; \
    retval->hashtable = malloc((qh->slotmask + 1) * sizeof(type_prefix ## SETENT)); \
    memcpy(retval->hashtable, qh->hashtable, (qh->slotmask + 1) * sizeof(type_prefix ## SETENT)); \
    retval->bf = bfclone(qh->bf, qh->slotmask + 1); \
    return retval; \
}

HASHIMPL(QHASH, q, qhash, !strcmp, free, strdup, char*, void*)
HASHIMPL(OPHASH, op, memophash, !opmemcmp, free, opmemdup, void*, void*)
#define NOP(X) (void) X
#define VERBATIM(X) X
#define COMPARATOR(i1, i2) ((i1) == (i2))
HASHIMPL(IIHASH, ii, inthash, COMPARATOR, NOP, VERBATIM, int, int)
HASHIMPL(LVHASH, lv, longhash, COMPARATOR, NOP, VERBATIM, long, void*)
HASHIMPL(FVHASH, fv, doublehash, COMPARATOR, NOP, VERBATIM, double, void*)
SETIMPL(IHASH, i, inthash, COMPARATOR, NOP, VERBATIM, int)
SETIMPL(LHASH, l, longhash, COMPARATOR, NOP, VERBATIM, long)
#undef COMPARATOR
#undef VERBATIM
#undef NOP


//clones integer->integer hash table into new hash table
IIHASHTABLE* iiclone(IIHASHTABLE* ht) {
  IIHASHTABLE* newht = malloc(sizeof(IIHASHTABLE));
  newht->keys = ht->keys;
  newht->slotmask = ht->slotmask;
  newht->hashtable = malloc((ht->slotmask + 1) * sizeof(IIHASHPAIR));
  newht->bf = bfclone(ht->bf, ht->slotmask + 1);
  memcpy(newht->hashtable, ht->hashtable, (ht->slotmask + 1) * sizeof(IIHASHPAIR));
  return newht;
}

//clones long->void* hash table into new hash table, cloning the values as well using the specified function
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

//returns and constructs a DYNINT array containing the entries of an integer hash set, this one contains ints, not pointers, so the hashset can be modified while this is in use
DYNINT* isetelems(IHASHSET* ihs) {
  DYNINT* di = dictor(ihs->keys + 2);
  for(int i = 0; i <= ihs->slotmask; i++) {
    if(bfget(ihs->bf, i)) {
      dipush(di, ihs->hashtable[i].key);
    }
  }
  return di;
}

void iiwipehash(IHASHSET* hs) {
  hs->keys = 0;
  memset(hs->bf, 0, (hs->slotmask + 8) >> 3);
}
