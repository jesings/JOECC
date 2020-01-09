struct hp;
typedef struct hp{
  char* key;
  void* value;
  struct hp* next;
} HASHPAIR;
typedef struct{
  HASHPAIR pairs[HASHSIZE];
} HASHTABLE;

HASHTABLE* htctor();
void htdtor(HASHTABLE* ht);
void insert(HASHTABLE* ht, char* key, void* value);
void* search(HASHTABLE* ht, char* key);
