#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "compintern.h"
struct lexctx* ctx;
int yylex();
int yyparse();
DYNARR* locs;
DYNARR* file2compile;
int main(int argc, char** argv) {
  ctx = malloc(sizeof(struct lexctx));
  //ctx->layer = 0;
  ctx->funcs = htctor();
  ctx->defines = htctor();
  ctx->definestack = dactor(64);
  ctx->scopes = dactor(64);
  SCOPE* rootscope = malloc(sizeof(SCOPE));
  rootscope->members = htctor();
  rootscope->unions = htctor();
  rootscope->structs = htctor();
  rootscope->enums = htctor();
  dapush(ctx->scopes, rootscope);
  ctx->symtab = htctor();
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
    dapush(file2compile, "stdin");
  }
  extern int yydebug;
  //yydebug = 1;

  yyparse();
  if(argc > 1) {
    close(compilefile);
    dup2(STDIN_FILENO, inval);
    close(inval);
  }
  DYNARR* funcky = htpairs(ctx->funcs);
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    puts(pairthere->key);
  }
}