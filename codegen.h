#ifndef CODEGEN_H
#define CODEGEN_H
#include "reg.h"
void startgenfile(FILE* outputfile, struct lexctx* lctx);
void genprogfile(FILE* outputfile, IDTYPE* rettype, char* funcname, PROGRAM* prog);
void ldstrsep(PROGRAM* prog);

/**
 * Contains info for mapping a 3ac operation to a machine code operation
 * An array of such translations will be in the op2op global variable.
**/
struct opinfo {
  const char* opname;
  int numargs;
  int fixedclobbers;
  enum reguse retloc;
  char reqreg;
};
extern struct opinfo op2op[];
#endif
