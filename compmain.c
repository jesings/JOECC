#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <pthread.h>
#include <getopt.h>
#include <sys/wait.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"
#include "ssa.h"
#include "opt.h"
#include "codegen.h"
#include "reg.h"

#ifdef NODEBUG
#define DEBUG(a)
#else
#define DEBUG(a) a
#endif
#if !defined(USECLANG) && !defined(USEMUSL) && !defined(USEGCC)
#define USEGCC
#endif


const char magic[16] = {0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};
pthread_mutex_t printlock, listlock;
int listptr;
int predefines;
DYNARR* includepath;

struct yyltype {int first_line, last_line, first_column, last_column; char* filename;};

struct yyltype* yyget_lloc(void*);
int yyparse(void* scanner, char* filename);
int yyset_in(FILE*, void*);
int yyset_debug(int flag, void*);
void yylex_init_extra(void*, void**);
void yylex_destroy(void*);

static void freev(void* v) {
  HASHPAIR* v2 = v;
  rfreefunc(v2->value);
}

static char* explainjoke(char* filename, char lastchar) {
  char* lastind = strrchr(filename, '.');
  int len;
  if(lastind) {
    len = lastind - filename;
  } else {
    len = strlen(filename);
  }
  char* newname = malloc(len + 8);
  strncpy(newname, filename, len);
  strcpy(newname + len, ".joecc");
  newname[len + 6] = lastchar;
  newname[len + 7] = '\0';
  return newname;
}

static void filecomp(char* filename) {
  FILE* precontext = fdopen(dup(predefines), "r");
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
  struct lexctx* lctx = ctxinit(yyin);
  yylex_init_extra(lctx, &scanner);
  DEBUG(yyset_debug(0, scanner));//not debugging lexer for now
  yyset_in(precontext, scanner);
  yyparse(scanner, strdup(filename));
  free(yyget_lloc(scanner)->filename);
  yylex_destroy(scanner);
  dadtorcfr(lctx->enumerat2free, (void(*)(void*)) freenum);
  bightdtorcfr(lctx->defines, (void (*)(void*)) freemd);
  free(lctx->withindefines);
  dadtor(lctx->definestack);
  dadtor(lctx->ls->locs);
  dadtor(lctx->ls->argpp);
  DYNARR* funcky = htpairs(lctx->funcs);

  char* newname = explainjoke(filename, 's');
  FILE* objf = fopen(newname, "w");
  startgenfile(objf, lctx);

  DEBUG(pthread_mutex_lock(&printlock));
  puts("Functions defined:");
  for(int i = 0; i < funcky->length; i++) {
    HASHPAIR* pairthere = daget(funcky, i);
    if(pairthere->value) {
      FUNC* f = pairthere->value;
      //treefunc(pairthere->value);
      DEBUG(putchar('\n'));
      DEBUG(puts(pairthere->key));
      PROGRAM* prog = linefunc(f); //fix main func
      splitcrit(prog); //for GVN
      prunebranch(prog); //esp for do while 0
      blockunblock(prog);//remove unnecessary edges
      rmunreach(prog);//maybe?
      collatealloc(prog);
      remove_nops(prog);
      DEBUG(printf("Ops before SSA %d\n", countops(prog)));
      DEBUG(treeprog(prog, pairthere->key, "pressa"));
      ssa(prog);
      remove_nops(prog);
      DEBUG(printf("Ops after SSA %d\n", countops(prog)));
      DEBUG(treeprog(prog, pairthere->key, "justssa"));
      constfold(prog);
      gvn(prog);
      remove_nops(prog);
      DEBUG(printf("Ops after GVN %d\n", countops(prog)));
      DEBUG(treeprog(prog, pairthere->key, "withgvn"));
      ldstrsep(prog);
      DEBUG(printf("Ops after ldstrsep %d\n", countops(prog)));
      DEBUG(treeprog(prog, pairthere->key, "ldstrsep"));

      renumber(prog);
      liveness(prog);
      remove_nops(prog);
      DEBUG(treeprog(prog, pairthere->key, "liveness"));

      genprogfile(objf, pairthere->key, prog);
      freeprog(prog);
    }
  }
  fclose(objf);
  free(newname);

  DEBUG(pthread_mutex_unlock(&printlock));
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
  char** argv = arg;
  while(1) {
    char* fn;
    pthread_mutex_lock(&listlock);
    fn = argv[listptr];
    if(!fn) {
      pthread_mutex_unlock(&listlock);
      return NULL;
    }
    ++listptr;
    pthread_mutex_unlock(&listlock);
    filecomp(fn);
  }
}

static void linkall(char const* outfile, char** argv) {
  DYNARR* fnames = dactor(8);
  dapush(fnames, strdup("/bin/as"));
  char* humorless;
  //++ to skip first element
  int forkpid;
  while((humorless = *(++argv))) {
    char* humored = explainjoke(humorless, 's');
    char* humored2 = explainjoke(humorless, 'o');
    switch((forkpid = fork())) {
      case -1:
        exit(-1);
      case 0:
        execl("/bin/as", "/bin/as", humored, "-o", humored2, NULL);
        break;
      default:
        waitpid(forkpid, NULL, 0);
        break;
    }
    dapush(fnames, humored2);
    free(humored);
  }
  dapush(fnames, strdup("-o"));
  dapush(fnames, strdup(outfile));
  dapush(fnames, NULL);
  switch((forkpid = fork())) {
    case -1:
      exit(-1);
    case 0:
      execv("/bin/ld", (char* const*) fnames->arr);
      break;
    default:
      waitpid(forkpid, NULL, 0);
      break;
  }
  dapop(fnames);
  dadtorfr(fnames);
}
const char* searchpath[] = {
#ifdef USECLANG
  "/usr/lib/clang/" HEADERS_VERSION "/include",
#elif defined(USEMUSL)
  "/usr/lib/musl/include",
#elif defined(USEGCC)
  "/usr/lib/gcc/x86_64-pc-linux-gnu/" HEADERS_VERSION "/include",
#else
#error
#endif
  "/usr/local/include",
#ifdef USEGCC
  "/usr/lib/gcc/x86_64-pc-linux-gnu/" HEADERS_VERSION "/include-fixed",
#endif
  "/usr/include",
};


int main(int argc, char** argv) {
  if(argc <= 1) {
    exit(0);
  }
  pthread_t pt2, pt3, pt4;
  int opt, opt_ind;
  char const* filedest = "a.out";
  static struct option long_options[] = {
    {"help", no_argument, NULL, 'h'},
    {"version", no_argument, NULL, 'v'},
    {NULL, 0, NULL, 0},
  };
  char tmpname[] = "precompilationXXXXXX";
  predefines = mkstemp(tmpname);
  includepath = dactor(8);
  for(unsigned int i = 0; i < sizeof(searchpath) / sizeof(char*); i++)
      dapush(includepath, strdup(searchpath[i]));
  while((opt = getopt_long(argc, argv, "cl:o:hvI:D:", long_options, &opt_ind)) != -1) {
    char* c;
    switch(opt) {
      case 'l':
        break;
      case 'c': 
        break;
      case 'o':
        filedest = optarg;
        if(!filedest) {
          fprintf(stderr, "No output file specified\n");
          exit(-1);
        }
        break;
      case 'h':
        printf("JOECC Compiler version 0.0.1-alpha\n");
        printf("JOECC Copyright (C) 2020-2021 Jonathan Singer\n");
        printf("This program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, see the license for further details.\n");
        return 0;
      case 'v':
        printf("JOECC Compiler version 0.0.1-alpha\n");
        break;
      case 'D': ;
        //figure out how to actually use predefines
        c = strchr(optarg, '=');
        if(c != NULL)
          *c = ' ';
        dprintf(predefines, "#define %s\n", c);
        break;
      case 'I': //figure out how to add to include path
        c = realpath(optarg, NULL);
        //handle errno
        dapush(includepath,c);
        break;
      case 0:
        break;
      default:
        break;
    }
  }
  listptr = optind;
  pthread_mutex_init(&printlock, NULL);
  pthread_mutex_init(&listlock, NULL);
  switch(argc - optind) {
    default:
      pthread_create(&pt4, NULL, ldeleg, argv);
      //fall through
    case 3:
      pthread_create(&pt3, NULL, ldeleg, argv);
      //fall through
    case 2:
      pthread_create(&pt2, NULL, ldeleg, argv);
      //fall through
    case 1:
      ldeleg(argv);
    case 0:
      break;
  }
  switch(argc - optind) {
    default:
      pthread_join(pt4, NULL);
      //fall through
    case 3:
      pthread_join(pt3, NULL);
      //fall through
    case 2:
      pthread_join(pt2, NULL);
    case 1:
    case 0:
      break;
  }
  dadtorfr(includepath);
  close(predefines);
  unlink(tmpname);
  linkall(filedest, argv);
  return 0;
}
