#include <stdlib.h>
#include <string.h>
#include "dynarr.h"

DYNARR* dactor(int initiallen){
  if(initiallen < 2) 
    return NULL;
  DYNARR* retval = malloc(sizeof(DYNARR));
  retval->length = 0;
  retval->maxlength = initiallen;
  retval->arr = malloc(sizeof(void*)*initiallen);
  return retval;
}

DYNARR* damerge(DYNARR* arr1, DYNARR* arr2){
  DYNARR* retval = malloc(sizeof(DYNARR));
  retval->maxlength = arr1->maxlength + arr2->maxlength;
  retval->arr = realloc(arr1->arr, retval->maxlength * sizeof(void*));
  retval->length = arr1->length + arr2->length;
  memcpy(retval->arr + arr1->length, arr2->arr, arr2->length * sizeof(void*));
  free(arr1);
  free(arr2->arr);
  free(arr2);
  return retval;
}

void dadtor(DYNARR* da){
  //for(int i = 0; i<da->length; i++)
  //  free((da->arr)[i]);
  free(da->arr);
  free(da);
}
void dadtorfr(DYNARR* da){
  for(int i = 0; i<da->length; i++)
    free((da->arr)[i]);
  free(da->arr);
  free(da);
}

void dainsert(DYNARR* da, void* val){
  if(da->length == da->maxlength)
    da->arr = reallocarray(da->arr, da->maxlength *= 1.5, sizeof(void*));
  da->arr[(da->length)++] = val;
}

void* dapop(DYNARR* da){
  return da->arr[--(da->length)];
}
