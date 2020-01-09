typedef struct{
  void** arr;
  int length;
  int maxlength;
} DYNARR;

DYNARR* dactor(int initiallen);
void dadtor(DYNARR* da);

void dainsert(DYNARR* da, void* val);
#define dapush dainsert
void* dapop(DYNARR* da);
#define dapeek(A) ((A)->arr[(A)->length-1])
