#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"
struct lexctx* ctx;
int yyparse(void);
int yyset_in(FILE*, void*);
int yyset_debug(int flag, void*);
void yylex_init(void**);
void yylex_destroy(void*);
DYNARR* locs;
DYNARR* file2compile;
int ppdebug;
void* scanner;
static void freev(void* v) {
  HASHPAIR* v2 = v;
  rfreefunc(v2->value);
}
static void filecomp(char* filename) {
  FILE* yyin = fopen(filename, "r");
  if(yyin == NULL)
    exit(1);
  yyset_in(yyin, scanner);
  yyset_debug(1, scanner);
  ctx = ctxinit();
  locs = dactor(128);
  file2compile = dactor(128);
  dapush(file2compile, strdup(filename));
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
  dadtorfr(file2compile);
}

int main(int argc, char** argv) {
  extern int yydebug;
  extern int zzdebug;
  zzdebug = 0;
  yydebug = 0;
  ppdebug = 0;
  if(argc <= 1) {
    exit(0);
  }
  for(int i = 1; i < argc; i++) {
    yylex_init(&scanner);
    filecomp(argv[i]);
    yylex_destroy(scanner);
  }
}
