#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <pthread.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"
#include "ssa.h"
#include "opt.h"

const char magic[16] = {0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};
pthread_mutex_t printlock, listlock;
int listptr, glargc;



int yyparse(void* scanner);
int yyset_in(FILE*, void*);
int yyset_debug(int flag, void*);
void yylex_init_extra(void*, void**);
void yylex_destroy(void*);
static void freev(void* v) {
  HASHPAIR* v2 = v;
  rfreefunc(v2->value);
}
static void filecomp(char* filename) {
  FILE* yyin = fopen(filename, "r");
  if(yyin == NULL)
    return;
  char maymagic[16];
  size_t magiclen = fread(maymagic, 1, 16, yyin);
  if(magiclen == 16 && !memcmp(magic, maymagic, 16)) {
    fclose(yyin);
    return;
    //It's an ELF file
    char type = fgetc(yyin);
    switch(type) {
      case 0: case -1:
      case 4: //core dump
        return; //error
      case 1:
        return; //need to link
      case 2: 
        return; //static exe
      case 3: 
        return; //dynamic exe
    }
  } else {
    rewind(yyin);
  }
  void* scanner;
  struct lexctx* lctx = ctxinit();
  dapush(lctx->ls->file2compile, strdup(filename));
  yylex_init_extra(lctx, &scanner);
  yyset_in(yyin, scanner);
#ifdef NODEBUG
  yyset_debug(0, scanner);//not debugging lexer for now
#endif
  yyparse(scanner);
  yylex_destroy(scanner);
  dadtorcfr(lctx->enumerat2free, (void(*)(void*)) freenum);
  bightdtorcfr(lctx->defines, (void (*)(void*)) freemd);
  free(lctx->withindefines);
  dadtor(lctx->definestack);
  dadtor(lctx->ls->locs);
  dadtor(lctx->ls->argpp);
  dadtorfr(lctx->ls->file2compile);
  DYNARR* funcky = htpairs(lctx->funcs);
#ifndef NODEBUG
  pthread_mutex_lock(&printlock);
#endif
  puts("Functions defined:");
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      FUNC* f = pairthere->value;
      //treefunc(pairthere->value);
#ifndef NODEBUG
      putchar('\n');
      puts(pairthere->key);
#endif
      PROGRAM* prog = linefunc(f); //fix main func
      prunebranch(prog); //esp for do while 0
      printf("Ops before SSA %d\n", countops(prog)); 
      ctdtree(prog);
      printf("Ops after SSA %d\n", countops(prog)); 
#ifndef NODEBUG
      treeprog(prog, pairthere->key, "justssa");
#endif
      constfold(prog);
      gvn(prog);
      remove_nops(prog);
      printf("Ops after GVN %d\n", countops(prog));
#ifndef NODEBUG
      //puts("---------------------------------------");
      //printprog(prog);
      //puts("---------------------------------------");
      treeprog(prog, pairthere->key, "withgvn");
#endif
      freeprog(prog);
    }
  }
#ifndef NODEBUG
  pthread_mutex_unlock(&printlock);
#endif
  scopepop(lctx);
  dadtorcfr(funcky, freev);
  dadtor(lctx->scopes);
  dadtorcfr(lctx->enstruct2free, (void(*)(void*)) wipestruct);
  dadtorcfr(lctx->externglobals, (void(*)(void*)) freeinit);
  dadtorcfr(lctx->globals, (void(*)(void*)) freeinit);
  htdtor(lctx->funcs);
  free(lctx->ls);
  free(lctx);
  return;
}

static void* ldeleg(void* arg) {
  char** largv = (char**) arg;
  while(1) {
    pthread_mutex_lock(&listlock);
    int ws = listptr++;
    if(ws >= glargc) {
      pthread_mutex_unlock(&listlock);
      return NULL;
    }
    pthread_mutex_unlock(&listlock);
    filecomp(largv[ws]);
  }
}

int main(int argc, char** argv) {
  if(argc <= 1) {
    exit(0);
  }
  pthread_t pt2, pt3, pt4;
  listptr = 1;
  glargc = argc;
  pthread_mutex_init(&printlock, NULL);
  pthread_mutex_init(&listlock, NULL);
  switch(argc) {
    default:
      pthread_create(&pt4, NULL, ldeleg, argv);
      //fall through
    case 4:
      pthread_create(&pt3, NULL, ldeleg, argv);
      //fall through
    case 3:
      pthread_create(&pt2, NULL, ldeleg, argv);
      //fall through
    case 2:
      ldeleg(argv);
    case 1:
      break;
  }
  switch(argc) {
    default:
      pthread_join(pt4, NULL);
      //fall through
    case 4:
      pthread_join(pt3, NULL);
      //fall through
    case 3:
      pthread_join(pt2, NULL);
    case 2:
    case 1:
      break;
  }
  return 0;
}
