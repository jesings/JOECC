#ifndef CODEGEN_H
#define CODEGEN_H
void startgenfile(FILE* outputfile, struct lexctx* lctx);
void genprogfile(FILE* outputfile, char* funcname, PROGRAM* prog);
void ldstrsep(PROGRAM* prog);
#endif
