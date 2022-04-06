#include <stdlib.h>
#include <string.h>
#include "dynarr.h"

/*
 * The following macro, DYNIMPL, defines a factory for implementing a dynamic array for the given types.
 * The parameters of the macro are as follows:
 * type_suffix defines the suffix to be added on to DYN for the type of the array: i.e. ARR for DYNARR
 * prefix defines the prefix to be put before the functions to differentiate them: i.e. da -> dactor di ->dictor
 * elemtype defines the type of an element of the array, i.e. void*, or int
 * Calling this macro on a certain type defines the following functions, prefix omitted
 * ctor, which constructs a dynamic array of the requisite type with an initial max length of initiallen
 * dtor, which destructs and frees a dynamic array of the requisite type
 * push, which pushes a value on to the end of the filled part of the dynamic array of the requisite type
 * merge, which takes in two dynamic arrays of the requisite type, and returns a new dynamic array of the requisite type
 * destructing the two passed in dynamic arrays, and placing in the returned array the elements of the first concatenated with the elements of the second parameter array
 * clone, which constructs a copy of the dynamic array of the requisite type
 * pop, which removes the last populated element of a dynamic array from the array, and returns it
 */
#define DYNIMPL(type_suffix, prefix, elemtype) \
DYN ## type_suffix* prefix ## ctor(int initiallen) { \
  DYN ## type_suffix* retval = malloc(sizeof(DYN ## type_suffix)); \
  retval->length = 0; \
  retval->maxlength = initiallen; \
  if(initiallen) \
    retval->arr = malloc(sizeof(elemtype) * initiallen); \
  return retval; \
} \
void prefix ## dtor(DYN ## type_suffix* da) { \
  if(da == NULL) return; \
  if(da->maxlength) \
    free(da->arr); \
  free(da); \
} \
void prefix ## push(DYN ## type_suffix* da, elemtype val) { \
  if(da->length == da->maxlength) \
    da->arr = reallocarray(da->arr, da->maxlength *= 1.5, sizeof(elemtype)); \
  da->arr[(da->length)++] = val; \
} \
DYN ## type_suffix* prefix ## merge(DYN ## type_suffix* arr1, DYN ## type_suffix* arr2) { \
  if(!arr1->length) { \
    if(arr1->maxlength) free(arr1->arr); \
    *arr1 = *arr2; \
    free(arr2); \
    return arr1; \
  } else if(!arr2->length) { \
    prefix ## dtor(arr2); \
    return arr1; \
  } else if(arr1->length + arr2->length < arr1->maxlength) { \
    memcpy(arr1->arr + arr1->length, arr2->arr, arr2->length * sizeof(elemtype)); \
    arr1->length += arr2->length; \
    prefix ## dtor(arr2); \
    return arr1; \
  } \
  DYN ## type_suffix* retval = malloc(sizeof(DYN ## type_suffix)); \
  retval->maxlength = arr1->maxlength + arr2->maxlength; \
  retval->arr = realloc(arr1->arr, retval->maxlength * sizeof(elemtype)); \
  retval->length = arr1->length + arr2->length; \
  memcpy(retval->arr + arr1->length, arr2->arr, arr2->length * sizeof(elemtype)); \
  free(arr1); \
  prefix ## dtor(arr2); \
  return retval; \
} \
DYN ## type_suffix* prefix ## clone(DYN ## type_suffix* orig) { \
  DYN ## type_suffix* retval = malloc(sizeof(DYN ## type_suffix)); \
  retval->length = orig->length; \
  retval->maxlength = orig->maxlength; \
  retval->arr = malloc(sizeof(elemtype) * orig->maxlength); \
  memcpy(retval->arr, orig->arr, orig->length * sizeof(elemtype)); \
  return retval; \
} \
elemtype prefix ## pop(DYN ## type_suffix* da) { \
  return da->arr[--(da->length)]; \
}

DYNIMPL(ARR, da, void*)
DYNIMPL(INT, di, int)

void dadtorfr(DYNARR* da) {
  for(int i = 0; i< da->length; i++)
    free((da->arr)[i]);
  dadtor(da);
}

void dadtorcfr(DYNARR* da, void (*freep)(void*)) {
  if(da->maxlength) {
    for(int i = 0; i< da->length; i++)
      freep((da->arr)[i]);
    free(da->arr);
  }
  free(da);
}

void dapushc(DYNARR* da, void* val) {
  da->arr[(da->length)++] = val;
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

void dainsertat(DYNARR* da, int index, void* val) {
  if(da->length == da->maxlength)
    da->arr = reallocarray(da->arr, da->maxlength *= 1.5, sizeof(void*));
  memmove(da->arr + index + 1, da->arr + index, (da->length - index) * sizeof(void*));
  ++da->length;
  da->arr[index] = val;
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
