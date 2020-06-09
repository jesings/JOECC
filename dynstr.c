#include <stdlib.h>
#include <string.h>
#include "dynstr.h"

DYNSTR* strctor(char* strptr, int len, int maxlen) {
  DYNSTR* retval = malloc(sizeof(DYNSTR));
  retval->strptr = strptr;
  retval->lenstr = len;
  retval->maxlenstr = maxlen;
  return retval;
}

void strdtor(DYNSTR* ds) {
  free(ds->strptr);
  free(ds);
}

void dsmodsize(DYNSTR* ds, int len) {
  if(ds->maxlenstr < (ds->lenstr += len)) {
    while(ds->maxlenstr < ds->lenstr) 
      ds->maxlenstr *= 1.5;
    ds->strptr = realloc(ds->strptr, ds->maxlenstr);
  }
}

void dscat(DYNSTR* ds, char* txt, int len) {
  int lp = ds->lenstr;
  dsmodsize(ds, len);
  memcpy(ds->strptr + len, txt, len);
}
