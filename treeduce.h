#ifndef TREEDUCE_H
#define TREEDUCE_H
char puritree(EXPRESSION* cexpr);
char purestmt(STATEMENT* stmt);
char typequality(IDTYPE* t1, IDTYPE* t2);
char treequals(EXPRESSION* e1, EXPRESSION* e2);
char foldconst(EXPRESSION** exa);
#endif

