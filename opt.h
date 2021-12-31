#ifndef OPT_H
#define OPT_H
#include "3ac.h"
void domark(BBLOCK* blk);
char constfold(PROGRAM* prog);
void rmunreach(PROGRAM* prog);
char remove_nops(PROGRAM* prog);
void prunebranch(PROGRAM* prog);
int countops(PROGRAM* prog);
void splitcrit(PROGRAM* prog);
void tailcall(PROGRAM* prog);
void collatealloc(PROGRAM* prog);
void blockunblock(PROGRAM* prog);
void intoinline(PROGRAM* prog, PROGRAM* candidate);
void renumber(PROGRAM* prog);
#endif
