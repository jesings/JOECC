#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <pthread.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"
#include "ssa.h"
#include "opt.h"
#include <coz.h>

int yyparse(void* scanner);
int yyset_in(FILE*, void*);
int yyset_debug(int flag, void*);
void yylex_init_extra(void*, void**);
void yylex_destroy(void*);
static void freev(void* v) {
  HASHPAIR* v2 = v;
  rfreefunc(v2->value);
}
static void* filecomp(char* filename) {
  void* scanner;
  FILE* yyin = fopen(filename, "r");
  if(yyin == NULL)
    return NULL;
  struct lexctx* lctx = ctxinit();
  dapush(lctx->ls->file2compile, strdup(filename));
  yylex_init_extra(lctx, &scanner);
  yyset_in(yyin, scanner);
#ifdef NODEBUG
  yyset_debug(0, scanner);
#else
  yyset_debug(1, scanner);
#endif
  yyparse(scanner);
  yylex_destroy(scanner);
  dadtorcfr(lctx->enumerat2free, (void(*)(void*)) freenum);
  htdtorcfr(lctx->defines, (void (*)(void*)) freemd);
  htdtorcfr(lctx->withindefines, (void (*)(void*)) freemd);
  dadtor(lctx->definestack);
  dadtor(lctx->ls->locs);
  dadtor(lctx->ls->argpp);
  dadtorfr(lctx->ls->file2compile);
  //chdir("./functions");
  DYNARR* funcky = htpairs(lctx->funcs);
  puts("Functions defined:");
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      FUNC* f = pairthere->value;
      //treefunc(pairthere->value);
      PROGRAM* prog = linefunc(f); //fix main func
#ifndef NODEBUG
      putchar('\n');
      puts(pairthere->key);
      puts("---------------------------------------");
      printprog(prog);
      puts("---------------------------------------");
      treeprog(prog, pairthere->key);
#endif
      ctdtree(prog);
      freeprog(prog);
    }
  }
  scopepop(lctx);
  dadtorcfr(funcky, freev);
  dadtor(lctx->scopes);
  dadtorcfr(lctx->enstruct2free, (void(*)(void*)) wipestruct);
  dadtorcfr(lctx->externglobals, (void(*)(void*)) freeinit);
  dadtorcfr(lctx->globals, (void(*)(void*)) freeinit);
  htdtor(lctx->funcs);
  free(lctx->ls);
  free(lctx);
  return NULL;
}

int main(int argc, char** argv) {
  if(argc <= 1) {
    exit(0);
  }
  //pthread_t pt;
  for(int i = 1; i < argc; i += 1) {
    //if(i + 1 == argc) {
    filecomp(argv[i]);
    COZ_PROGRESS;
    //} else {
    //  pthread_create(&pt, NULL, (void* (*)(void*)) filecomp, argv[i + 1]);
    //  filecomp(argv[i]);
    //  pthread_join(pt, NULL);
    //}
  }
}
