#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "compintern.h"
#include "printree.h"
struct lexctx* ctx;
int yylex(void);
int yyparse(void);
DYNARR* locs;
DYNARR* file2compile;
//TODO: Predefined macros
//__FILE__, __LINE__, __DATE__, __TIME__, __STDC__ = 1, __STDC_VERSION__ = '199901L', __STDC_HOSTED__ = 1
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
  yydebug = 0;
  yyparse();
  if(argc > 1) {
    close(compilefile);
    dup2(STDIN_FILENO, inval);
    close(inval);
  }
  chdir("./functions");
  DYNARR* funcky = htpairs(ctx->funcs);
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      puts(pairthere->key);
      treefunc(pairthere->value);
    }
  }
}
