#ifndef CODEGEN_H
#define CODEGEN_H
#include "reg.h"
void startgenfile(FILE* outputfile, struct lexctx* lctx);
void genprogfile(FILE* outputfile, char* funcname, PROGRAM* prog);
void ldstrsep(PROGRAM* prog);

struct opinfo {
  const char* opname;
  int numargs;
  int fixedclobbers;
  enum reguse retloc;
  char reqreg;
};
extern struct opinfo op2op[];
#endif
