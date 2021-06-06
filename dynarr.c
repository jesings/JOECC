#include <stdlib.h>
#include <string.h>
#include "dynarr.h"

DYNARR* dactor(int initiallen) {
  DYNARR* retval = malloc(sizeof(DYNARR));
  retval->length = 0;
  retval->maxlength = initiallen;
  if(initiallen)
    retval->arr = malloc(sizeof(void*) * initiallen);
  return retval;
}

DYNARR* damerge(DYNARR* arr1, DYNARR* arr2) {
  if(!arr1->length) {
    if(arr1->maxlength) free(arr1->arr);
    *arr1 = *arr2;
    free(arr2);
    return arr1;
  } else if(!arr2->length) {
    dadtor(arr2);
    return arr1;
  } else if(arr1->length + arr2->length < arr1->maxlength) {
    memcpy(arr1->arr + arr1->length, arr2->arr, arr2->length * sizeof(void*));
    arr1->length += arr2->length;
    dadtor(arr2);
    return arr1;
  }
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

DYNARR* daclone(DYNARR* orig) {
  DYNARR* retval = malloc(sizeof(DYNARR));
  retval->length = orig->length;
  retval->maxlength = orig->maxlength;
  retval->arr = malloc(sizeof(void*) * orig->maxlength);
  memcpy(retval->arr, orig->arr, orig->length * sizeof(void*));
  return retval;
}

void dadtor(DYNARR* da) {
  if(da->maxlength)
    free(da->arr);
  free(da);
}
void dadtorfr(DYNARR* da) {
  for(int i = 0; i<da->length; i++)
    free((da->arr)[i]);
  if(da->maxlength)
    free(da->arr);
  free(da);
}

void dadtorcfr(DYNARR* da, void (*freep)(void*)) {
  if(da->maxlength) {
    for(int i = 0; i<da->length; i++)
      freep((da->arr)[i]);
    free(da->arr);
  }
  free(da);
}

void dainsert(DYNARR* da, void* val) {
  if(da->length == da->maxlength)
    da->arr = reallocarray(da->arr, da->maxlength *= 1.5, sizeof(void*));
  da->arr[(da->length)++] = val;
}

void dainsertc(DYNARR* da, void* val) {
  da->arr[(da->length)++] = val;
}

void* dapop(DYNARR* da) {
  return da->arr[--(da->length)];
}

//dynamic array remove value (was going to be called darm, but couldn't pass up opportunity for punny name)
//returns null if no element is removed, returns removed value otherwise (could be null, beware)
void* dharma(DYNARR* da, void* val) { //order not preserved
  int i;
  for(i = 0; i < da->length && da->arr[i] != val; i++) ;
  if(i != da->length)
    return da->arr[i] = da->arr[--da->length];
  return NULL;
}

//dynamic array replace value (was going to be called darp, but couldn't pass up opportunity for punny name)
void darpa(DYNARR* da, void* val, void* rpval) { //order not preserved
  int i;
  for(i = 0; i < da->length && da->arr[i] != val; i++) ;
  if(i != da->length) da->arr[i] = rpval;
}

DYNINT* dinctor(int initiallen) {
  DYNINT* retval = malloc(sizeof(DYNINT));
  retval->length = 0;
  retval->maxlength = initiallen;
  if(initiallen)
    retval->arr = malloc(sizeof(int) * initiallen);
  return retval;
}
void dipush(DYNINT* di, int i) {
  if(di->length == di->maxlength)
    di->arr = reallocarray(di->arr, di->maxlength *= 1.5, sizeof(void*));
  di->arr[(di->length)++] = i;
}
void didtor(DYNINT* di) {
  if(di->maxlength) free(di->arr);
  free(di);
}

void disort(DYNINT* di) {
  //mergesort for stability
  int n = di->maxlength;
  int* other = malloc(sizeof(int) * n);
  for(int width = 1; width < n; width *= 2) {
    for(int i = 0; i < n; i += 2 * width) {
      int left = i;
      int right = i + width < n ? i + width : n;
      int end = i + 2 * width < n ? i + 2 * width : n;
      int origright = right;
      for(int k = left; k < end; k++) {
        if(left < origright && (right >= end || di->arr[left] <= di->arr[right])) {
          other[k] = di->arr[left];
          left++;
        } else {
          other[k] = di->arr[right];
          right++;
        }
      }
    }
    int* tmp = di->arr;
    di->arr = other;
    other = tmp;
  }
  free(other);
}

void didup(DYNINT* di) {
  disort(di);
  int last = -1;
  int wrind = 0;
  for(int i = 0; i < di->length; i++) {
    if(di->arr[i] != last) {
      last = di->arr[i];
      di->arr[wrind++] = last;
    }
  }
  di->length = wrind;
}
