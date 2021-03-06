#ifndef TREEDUCE_H
#define TREEDUCE_H
#include "dynarr.h"
#include "compintern.h"
char puritree(EXPRESSION* cexpr);
char purestmt(STATEMENT* stmt);
char typequality(IDTYPE* t1, IDTYPE* t2);
char treequals(EXPRESSION* e1, EXPRESSION* e2);
IDTYPE typex(EXPRESSION* ex);
char foldconst(EXPRESSION* ex);
char pleatstate(STATEMENT** stated);
#endif

