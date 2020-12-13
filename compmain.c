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
static void freev(void* v) {
  HASHPAIR* v2 = v;
  rfreefunc(v2->value);
}
int main(int argc, char** argv) {
  ctx = ctxinit();
  locs = dactor(128);
  file2compile = dactor(128);
  FILE* compilefile;
  if(argc > 1) {
    dapush(file2compile, argv[1]);
    compilefile = fopen(argv[1], "r");
    if(compilefile == NULL)
      exit(1);
    extern FILE* yyin;
    yyin = compilefile;
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
    fclose(compilefile);
  }
  dadtorcfr(ctx->enumerat2free, (void(*)(void*)) freenum);
  htdtorcfr(ctx->defines, (void (*)(void*)) freemd);
  htdtorfr(ctx->withindefines);
  chdir("./functions");
  DYNARR* funcky = htpairs(ctx->funcs);
  puts("Functions defined:");
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      putchar('\n');
      puts(pairthere->key);
      //treefunc(pairthere->value);
      PROGRAM* prog = linefunc(pairthere->value);
      puts("---------------------------------------");
      printprog(prog);
      puts("---------------------------------------");
      freeprog(prog);
    }
  }
  dadtorcfr(funcky, freev);
  dadtorcfr(ctx->enstruct2free, (void(*)(void*)) wipestruct);
  //dadtorcfr(ctx->globals, (void(*)(void*))freeinit);
}
