#ifndef DYNSTR_H
#define DYNSTR_H
typedef struct {
  char* strptr;
  int lenstr;
  int maxlenstr;
} DYNSTR;
DYNSTR* strctor(char* strptr, int len, int maxlen);
void dsmodsize(DYNSTR* ds, int len);
void strdtor(DYNSTR* ds);
void dscat(DYNSTR* ds, char* txt, int len);
void dsccat(DYNSTR* ds, char txt);
#endif

