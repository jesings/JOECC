#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"
int yyparse(void* scanner);
int yyset_in(FILE*, void*);
int yyset_debug(int flag, void*);
void yylex_init_extra(void*, void**);
void yylex_destroy(void*);
DYNARR* locs;
DYNARR* file2compile;
int ppdebug;
static void freev(void* v) {
  HASHPAIR* v2 = v;
  rfreefunc(v2->value);
}
static void filecomp(char* filename) {
  void* scanner;
  FILE* yyin = fopen(filename, "r");
  if(yyin == NULL)
    exit(1);
  struct lexctx* lctx = ctxinit();
  locs = dactor(128);
  file2compile = dactor(128);
  dapush(file2compile, strdup(filename));
  yylex_init_extra(lctx, &scanner);
  yyset_in(yyin, scanner);
  yyset_debug(1, scanner);
  yyparse(scanner);
  yylex_destroy(scanner);
  dadtorcfr(lctx->enumerat2free, (void(*)(void*)) freenum);
  htdtorcfr(lctx->defines, (void (*)(void*)) freemd);
  htdtorcfr(lctx->withindefines, (void (*)(void*)) freemd);
  dadtor(lctx->definestack);
  //chdir("./functions");
  DYNARR* funcky = htpairs(lctx->funcs);
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
  scopepop(lctx);
  dadtorcfr(funcky, freev);
  dadtor(lctx->scopes);
  dadtorcfr(lctx->enstruct2free, (void(*)(void*)) wipestruct);
  dadtorcfr(lctx->externglobals, (void(*)(void*))freeinit);
  dadtorcfr(lctx->globals, (void(*)(void*))freeinit);
  htdtor(lctx->funcs);
  dadtor(lctx->argpp);
  free(lctx);
  dadtor(locs);
  dadtorfr(file2compile);
}

int main(int argc, char** argv) {
  ppdebug = 0;
  if(argc <= 1) {
    exit(0);
  }
  for(int i = 1; i < argc; i++) {
    filecomp(argv[i]);
  }
}
