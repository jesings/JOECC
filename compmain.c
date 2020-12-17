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
    exit(0);
  }
  extern int yydebug;
  extern int zzdebug;
  zzdebug = 0;
  yydebug = 0;
  ppdebug = 0;
  yyparse();
  dadtorcfr(ctx->enumerat2free, (void(*)(void*)) freenum);
  htdtorcfr(ctx->defines, (void (*)(void*)) freemd);
  htdtorcfr(ctx->withindefines, (void (*)(void*)) freemd);
  dadtor(ctx->definestack);
  //chdir("./functions");
  DYNARR* funcky = htpairs(ctx->funcs);
  puts("Functions defined:");
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      FUNC* f = pairthere->value;
      putchar('\n');
      puts(pairthere->key);
      //treefunc(pairthere->value);
      PROGRAM* prog = linefunc(f);
      if(!strcmp(pairthere->key, "main"))
        dapush(prog->ops, ct_3ac_op1(RET_3, ISCONST | 4, (ADDRESS) 0L));
      puts("---------------------------------------");
      printprog(prog);
      puts("---------------------------------------");
      freeprog(prog);
    }
  }
  scopepop(ctx);
  dadtorcfr(funcky, freev);
  dadtor(ctx->scopes);
  dadtorcfr(ctx->enstruct2free, (void(*)(void*)) wipestruct);
  dadtorcfr(ctx->externglobals, (void(*)(void*))freeinit);
  dadtorcfr(ctx->globals, (void(*)(void*))freeinit);
  htdtor(ctx->funcs);
  dadtor(ctx->argpp);
  free(ctx);
  dadtor(locs);
  dadtor(file2compile);
}
