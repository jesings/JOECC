#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "joecc_assert.h"
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
  assert(ds->maxlenstr > 1);
  if(ds->maxlenstr < (ds->lenstr += len)) {
    while(ds->maxlenstr < ds->lenstr) 
      ds->maxlenstr *= 1.5;
    ds->strptr = realloc(ds->strptr, ds->maxlenstr);
  }
}

//dynamic string concatenate
void dscat(DYNSTR* ds, char* txt, int len) {
  int lp = ds->lenstr;
  dsmodsize(ds, len);
  memcpy(ds->strptr + lp, txt, len);
}

//dynamic string concatenate character
void dsccat(DYNSTR* ds, char txt) {
  int lp = ds->lenstr;
  dsmodsize(ds, 1);
  ds->strptr[lp] = txt;
}

void dsws(DYNSTR* ds) { //remove trailing whitespace
  if(ds->strptr[ds->lenstr - 1]) { //no null terminator
    while(ds->lenstr && isblank(ds->strptr[ds->lenstr - 1])) {
      --ds->lenstr;
    }
  } else {
    --ds->lenstr;
    while(ds->lenstr && isblank(ds->strptr[ds->lenstr - 1])) {
      --ds->lenstr;
    }
    dsccat(ds, 0);
  }
}
