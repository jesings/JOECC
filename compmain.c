#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"
struct lexctx* ctx;
int yylex(void);
int yyparse(void);
DYNARR* locs;
DYNARR* file2compile;
int ppdebug;
//TODO: Variadic macros?
int main(int argc, char** argv) {
  ctx = ctxinit();
  locs = dactor(128);
  file2compile = dactor(128);
  int inval;
  int compilefile;
  if(argc > 1) {
    dapush(file2compile, argv[1]);
    inval = dup(STDIN_FILENO);
    compilefile = open(argv[1], O_RDONLY);
    if(compilefile == -1)
      exit(1);
    dup2(compilefile, STDIN_FILENO);
  } else {
    dapush(file2compile, (void*)(unsigned long) "stdin");
  }
  extern int yydebug;
  extern int zzdebug;
  zzdebug = 0;
  yydebug = 0;
  ppdebug = 0;
  yyparse();
  if(argc > 1) {
    close(compilefile);
    dup2(STDIN_FILENO, inval);
    close(inval);
  }
  //do some kind of scopestack traversal
  //for(int i = 0; i < ctx->structs->length; i++) {
  //  feedstruct(daget(ctx->structs, i));
  //}
  //for(int i = 0; i < ctx->unions->length; i++) {
  //  unionlen(daget(ctx->unions, i));
  //}
  chdir("./functions");
  DYNARR* funcky = htpairs(ctx->funcs);
  puts("Functions defined:");
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      puts(pairthere->key);
      treefunc(pairthere->value);
      PROGRAM* prog = linefunc(pairthere->value);
      printprog(prog);
    }
  }
}
