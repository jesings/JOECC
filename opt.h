#ifndef OPT_H
#define OPT_H
#include "3ac.h"
void domark(BBLOCK* blk);
char constfold(PROGRAM* prog);
void rmunreach(PROGRAM* prog);
char markunreach(DYNARR* pb);
char remove_nops(PROGRAM* prog);
void prunebranch(PROGRAM* prog);
int countops(PROGRAM* prog);
OPERATION* exciseop(BBLOCK* blk, OPERATION* prevop, OPERATION* curop);
void splitcrit(PROGRAM* prog);
#endif
