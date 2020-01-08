#include <stdlib.h>
#include "dynarr.h"

DYNARR* dactor(int initiallen){
  DYNARR* retval = malloc(sizeof(DYNARR));
  retval->length = 0;
  retval->maxlength = initiallen;
  retval->arr = malloc(sizeof(void*)*initiallen);
  return retval;
}
void dadtor(DYNARR* da){
  free(da->arr);
  free(da);
}

void dainsert(DYNARR* da, void* val){
  if(da->length == da->maxlength)
    da->arr = reallocarray(da->arr,da->maxlength *= 1.5,sizeof(void*));
  da->arr[(da->length)++] = val;
}

void* dapop(DYNARR* da){
  return da->arr[--(da->length)];
}
