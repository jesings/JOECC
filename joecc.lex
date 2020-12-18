BIN [0-1]
OCT [0-7]
IDENT [[:alpha:]_][[:alnum:]_]*
EXP [Ee][+-]?[[:digit:]]+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*
%{
//TODO: computed include? variadic macros?

#include <math.h>
#include <time.h>
#include <assert.h>
#include "joecc.tab.h"
#include "compintern.h"
#include "treeduce.h"

#define YY_USER_ACTION \
  do { \
    yylloc->first_line = yylloc->last_line; \
    yylloc->first_column = yylloc->last_column; \
    for(int i = 0; i < yyleng; i++) { \
        if(yytext[i] == '\n') { \
            yylloc->last_line++; \
            yylloc->last_column = 0; \
        } \
        else { \
            yylloc->last_column++; \
        } \
    } \
  } while(0);
#define SIGNEDCHAR 0
#undef unput
#define unput(c, yyscanner) yyunput(c, ((struct yyguts_t*) yyscanner)->yytext_ptr, yyscanner)
#define lctx ((struct lexctx*) yyget_extra(yyscanner))

int zzparse(yyscan_t scanner);
int check_type(char* symb, char frominitial, yyscan_t yyscanner);
char stmtover, skipping;
char* defname, * strconst;
int paren_depth;
struct macrodef* md;
char charconst, argeaten;
DYNARR* parg;
HASHTABLE* defargs = NULL;
DYNSTR* dstrdly, * mdstrdly, * strcur;
extern DYNARR* locs, * file2compile;
extern int ppdebug;

struct arginfo {
  DYNSTR* argi;
  int pdepth;
  char* defname;
  DYNARR* parg;
};

#define GOC(c) yylval_param->unum = c; yy_pop_state(yyscanner); return UNSIGNED_LITERAL
#define ZGOC(c) yylval_param->unum = c; yy_pop_state(yyscanner); return UNSIGNED_LITERAL
#define UNPUTSTR(str) for(int l = strlen(str); l; unput(str[--l], yyscanner)) --yyget_lloc(yyscanner)->last_column
%}
%option yylineno
%option noyywrap
%option stack
%option never-interactive
%option reentrant
%option bison-bridge
%option bison-locations
%option debug
/*%option full*/

%option warn
%option nodefault
%option noyy_top_state
%option noyy_scan_string
%option noyywrap

%x MULTILINE_COMMENT SINGLELINE_COMMENT
%x PREPROCESSOR INCLUDE INCLUDENEXT
%x DEFINE UNDEF DEFARG DEFINE2
%x WARNING ERROR
%x IFNDEF IFDEF PPSKIP
%x KILLBLANK KILLUNTIL
%x STRINGLIT CHARLIT ZCHARLIT
%x CALLMACRO FINDREPLACE
%x WITHINIF CHECKDEFINED CHECKDEFINED2 ENDWITHINIF

%%
<KILLBLANK>{
  ([[:blank:]]+|\\[[:blank:]]*\n)* {yy_pop_state(yyscanner);}
}

<KILLUNTIL>{
  [^/\n\\]+ {}
  \n {yy_pop_state(yyscanner);}
  "/" {}
  \\ {}
  "//" {yy_pop_state(yyscanner); yy_push_state(SINGLELINE_COMMENT, yyscanner);}
  "/*" {yy_push_state(MULTILINE_COMMENT, yyscanner);}
}

<INITIAL,PREPROCESSOR,INCLUDE,DEFINE,UNDEF,DEFARG,DEFINE2,IFDEF,IFNDEF,CALLMACRO,PPSKIP,KILLBLANK,WITHINIF>{
  "/*" {yy_push_state(MULTILINE_COMMENT, yyscanner);}
  "//" {yy_push_state(SINGLELINE_COMMENT, yyscanner);}
}

<MULTILINE_COMMENT>{
  [^*]+ {/*The multiline comment is not terminated*/}
  "*"+ {/*The multiline comment is not terminated*/}
  "*"+"/" {yy_pop_state(yyscanner);}
}

<SINGLELINE_COMMENT>{
  [^\\\n]+ {/*The single line comment is not terminated*/}
  \\+ {/*The single line comment is not terminated*/}
  [^\\\n]+$ {yy_pop_state(yyscanner);}
}

<PPSKIP>{
  ^[[:blank:]]*#[[:blank:]]* {
    yy_push_state(PREPROCESSOR, yyscanner);
    stmtover = 0;
    skipping = 1;
    }
  [^\n#\/]+ {}
  [\/\n#] {}
}

^[[:blank:]]*#[[:blank:]]* {yy_push_state(PREPROCESSOR, yyscanner); stmtover = 0; skipping = 0;}
<PREPROCESSOR>{
  include[[:blank:]]+ {if(!skipping) yy_push_state(INCLUDE, yyscanner); else yy_pop_state(yyscanner);}
  include_next[[:blank:]]+ {if(!skipping) yy_push_state(INCLUDENEXT, yyscanner); else yy_pop_state(yyscanner);}
  define[[:blank:]]+ {if(!skipping) {yy_push_state(DEFINE, yyscanner); md = calloc(1, sizeof(struct macrodef));} else yy_pop_state(yyscanner);}
  undef[[:blank:]]+ {if(!skipping) yy_push_state(UNDEF, yyscanner); else yy_pop_state(yyscanner);}
  ifdef[[:blank:]]+ {yy_push_state(IFDEF, yyscanner);}
  ifndef[[:blank:]]+ {yy_push_state(IFNDEF, yyscanner);}
  else {
    yy_pop_state(yyscanner);
    DYNARR* ds = lctx->definestack;
    assert(ds->length || !fprintf(stderr, "ERROR: else without preceding if %s %d.%d-%d.%d\n", locprint2(yylloc)));
    enum ifdefstate* ids = dapeek(ds);
    switch(*ids) {
      case IFANDTRUE: 
        *ids = ELSEANDTRUE;
        yy_push_state(PPSKIP, yyscanner);
        break;
      case IFANDFALSE: 
        *ids = ELSEANDFALSE;
        yy_pop_state(yyscanner);
        break;
      default:
        fprintf(stderr, "Error: Unexpected #else %s %d.%d-%d.%d\n", locprint2(yylloc));
      case IFDEFDUMMY:
        break;
    }
    yy_push_state(KILLUNTIL, yyscanner);
    }
  elif {
    yy_pop_state(yyscanner);
    DYNARR* ds = lctx->definestack;
    assert(ds->length || !fprintf(stderr, "ERROR: elif without preceding if %s %d.%d-%d.%d\n", locprint2(yylloc)));
    enum ifdefstate* ids = dapeek(ds);
    switch(*ids) {
      case IFANDTRUE: 
        *ids = IFDEFDUMMY;
        yy_push_state(PPSKIP, yyscanner);
        break;
      case IFANDFALSE: 
        yy_pop_state(yyscanner);
        free(dapop(ds));
        yy_push_state(WITHINIF, yyscanner);
        zzparse(yyscanner);
        break;
      case IFDEFDUMMY:
        break;
      default:
        fprintf(stderr, "Error: Unexpected #elif %s %d.%d-%d.%d\n", locprint2(yylloc));
    }
    }
  endif {/*handle endif case*/
    yy_pop_state(yyscanner);
    DYNARR* ds = lctx->definestack;
    if(ds->length > 0) {
      enum ifdefstate* ids = dapop(ds);
      switch(*ids) {
        case IFANDTRUE: case ELSEANDFALSE:
          break;
        case IFANDFALSE: case ELSEANDTRUE: case IFDEFDUMMY:
          yy_pop_state(yyscanner);
          break;
      }
      free(ids);
    } else {
      fprintf(stderr, "Error: Unexpected #endif %s %d.%d-%d.%d\n", locprint2(yylloc));
    }
    yy_push_state(KILLUNTIL, yyscanner);
    }
  if {/*handle if case*/
    yy_pop_state(yyscanner);
    DYNARR* ds = lctx->definestack;
    enum ifdefstate ids = ds->length > 0 ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;
    enum ifdefstate* rids;
    switch(ids) {
      default:
        rids = malloc(sizeof(enum ifdefstate));
        *rids = IFDEFDUMMY;
        dapush(ds, rids);
        yy_push_state(PPSKIP, yyscanner);
        break;
      case IFANDTRUE: case ELSEANDFALSE:
        yy_push_state(WITHINIF, yyscanner);
        zzparse(yyscanner);
        break;
    }
    }
  line {if(!skipping){yy_pop_state(yyscanner); yy_push_state(SINGLELINE_COMMENT, yyscanner);} else {yy_pop_state(yyscanner); yy_push_state(KILLUNTIL, yyscanner);}/*TODO: line directive currently doesn't apply requisite information*/}
  warning {if(!skipping){yy_pop_state(yyscanner); yy_push_state(WARNING, yyscanner); yy_push_state(KILLBLANK, yyscanner);} else {yy_pop_state(yyscanner); yy_push_state(KILLUNTIL, yyscanner);}}
  error {if(!skipping){yy_pop_state(yyscanner); yy_push_state(ERROR, yyscanner); yy_push_state(KILLBLANK, yyscanner);} else {yy_pop_state(yyscanner); yy_push_state(KILLUNTIL, yyscanner);}}
  \n {yy_pop_state(yyscanner);fprintf(stderr, "PREPROCESSOR: Incorrect line end %d %s %d.%d-%d.%d\n", yyget_lloc(yyscanner)->first_line, locprint2(yylloc));}
  . {fprintf(stderr, "PREPROCESSOR: Unexpected character encountered: %d %c %s %d.%d-%d.%d\n", *yytext, *yytext, locprint2(yylloc));}
}
<WARNING>\"(\\.|[^\\"]|\/[[:space:]]*\n)*\" {
    /*WARNING, ERROR, only support single string const*/
    yy_pop_state(yyscanner); 
    yy_push_state(KILLUNTIL, yyscanner); 
    yytext[yyleng - 1] = '\0'; 
    fprintf(stderr, "WARNING: %s\n", yytext + 1);
    }
<ERROR>\"(\\.|[^\\"]|\/[[:space:]]*\n)*\" {/*"*/
    yy_pop_state(yyscanner); 
    yy_push_state(KILLUNTIL, yyscanner); 
    yytext[yyleng - 1] = '\0'; 
    fprintf(stderr, "ERROR: %s\n", yytext + 1);
    exit(0);
    }

<INCLUDE>{
  "<"[^>\n]*">" {
    if(stmtover){
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yyget_lloc(yyscanner)->last_line, locprint2(yylloc));
    } else {
      stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing >
      char pathbuf[256];
      static const char* searchpath[] = {
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include",
        "/usr/local/include",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include-fixed",
        "/usr/include",
        };
      yy_pop_state(yyscanner);
      yy_pop_state(yyscanner);
      int i;
      for(i = 0; i < 4 /*sizeof searchpath*/; i++) {
        FILE* newbuf;
        snprintf(pathbuf, 256, "%s/%s", searchpath[i], yytext + 1); //ignore opening <
        if((newbuf = fopen(pathbuf, "r")) != NULL) {
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = *yylloc;
          dapush(locs, ylt);
          yylloc->first_line = yylloc->last_line = 1;
          yylloc->first_column = yylloc->last_column = 0;
          dapush(file2compile, strdup(pathbuf));
          YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE, yyscanner);
          yy_push_state(INITIAL, yyscanner);
          yypush_buffer_state(ybs, yyscanner);
          break;
        }
      } 
      if(i == 4){
        fprintf(stderr, "Invalid system file %s included!\n", yytext + 1);
      }
    }
    }
  \"[^\"\n]*\" {/*"*/
    if(stmtover) {
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yyget_lloc(yyscanner)->last_line, locprint2(yylloc));
    } else {
      stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing "
      yy_pop_state(yyscanner);
      yy_pop_state(yyscanner);
      FILE* newbuf;
      char* pfstr = dapeek(file2compile);
      char* fname = yytext + 1;
      if(strchr(pfstr, '/')) {
        char pathbuf[512];
        strncpy(pathbuf, pfstr, 256);
        char* nextptr = strrchr(pathbuf, '/') + 1;
        strncpy(nextptr, yytext + 1, 256);
        fname = strdup(pathbuf);
      } else {
        fname = strdup(fname);
      }
      if((newbuf = fopen(fname, "r")) != NULL) { //ignore opening "
        YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
        *ylt = *yylloc;
        dapush(locs, ylt);
        yylloc->first_line = yylloc->last_line = 1;
        yylloc->first_column = yylloc->last_column = 0;
        dapush(file2compile, fname);
        YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE, yyscanner);
        yy_push_state(INITIAL, yyscanner);
        yypush_buffer_state(ybs, yyscanner);
      } else {
        fprintf(stderr, "Invalid local file %s included! %s %d.%d-%d.%d\n", fname, locprint2(yylloc));
      }
    }
    }
}
<INCLUDE,INCLUDENEXT>{
  [[:space:]]+[<\"] {/*"*/yyless(1);}
  [[:space:]]*\n {yy_pop_state(yyscanner); yy_pop_state(yyscanner);if(!stmtover) fprintf(stderr, "Error: incomplete include %s %d.%d-%d.%d\n", locprint2(yylloc));}
  . {fprintf(stderr, "INCLUDE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<INCLUDENEXT>{
  ["<][^>\n]*[>"] {
    if(stmtover){
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yyget_lloc(yyscanner)->last_line, locprint2(yylloc));
    } else {
      stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing >
      char pathbuf[256];
      static const char* searchpath[] = {
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include/",
        "/usr/local/include/",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include-fixed/",
        "/usr/include/",
        };
      yy_pop_state(yyscanner);
      yy_pop_state(yyscanner);
      int i = 0;
      for(; i < 3 && strncmp(dapeek(file2compile), searchpath[i], strlen(searchpath[i])); ++i) ;
      ++i;
      for(; i < 4 /*sizeof searchpath*/; ++i) {
        FILE* newbuf;
        snprintf(pathbuf, 256, "%s%s", searchpath[i], yytext + 1); //ignore opening
        if((newbuf = fopen(pathbuf, "r")) != NULL) {
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = *yylloc;
          dapush(locs, ylt);
          yylloc->first_line = yylloc->last_line = 1;
          yylloc->first_column = yylloc->last_column = 0;
          dapush(file2compile, strdup(pathbuf));
          YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE, yyscanner);
          yy_push_state(INITIAL, yyscanner);
          yypush_buffer_state(ybs, yyscanner);
          break;
        }
      } 
      if(i == 4){
        fprintf(stderr, "Invalid system file %s included next!\n", yytext + 1);
      }
    }
    }
}


<DEFINE>{
  {IDENT} {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFINE2, yyscanner); 
    mdstrdly = strctor(malloc(256), 0, 256); 
    defname = strdup(yytext);
    insert(lctx->withindefines, yytext, NULL);
    }
  {IDENT}/[[:blank:]] {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFINE2, yyscanner); 
    mdstrdly = strctor(malloc(256), 0, 256); 
    yy_push_state(KILLBLANK, yyscanner);
    defname = strdup(yytext);
    insert(lctx->withindefines, yytext, NULL);
    }
  {IDENT}\( {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFARG, yyscanner); 
    yytext[yyleng - 1] = '\0'; 
    defname = strdup(yytext);
    md->args = dactor(8); 
    argeaten = 0;
    insert(lctx->withindefines, yytext, NULL);
    }
  {IDENT}\(/[[:blank:]] {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFARG, yyscanner); 
    yy_push_state(KILLBLANK, yyscanner); 
    yytext[yyleng - 1] = '\0';
    defname = strdup(yytext); 
    md->args = dactor(8); 
    argeaten = 0;
    insert(lctx->withindefines, yytext, NULL);
    }
  \n {yy_pop_state(yyscanner); yy_pop_state(yyscanner);/*error state*/}
  . {fprintf(stderr, "DEFINE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<DEFARG>{
  {IDENT} {
    if(argeaten) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    argeaten = 1; /*new arg encountered*/ 
    dapush(md->args, strdup(yytext));
    /*probably should confirm no 2 args have the same name*/
    }
  {IDENT}/[[:blank:]] {
    if(argeaten) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    argeaten = 1;
    /*new arg encountered*/ yy_push_state(KILLBLANK, yyscanner); 
    dapush(md->args, strdup(yytext));
    /*probably should confirm no 2 args have the same name*/
    }
  \, {
    if(argeaten) 
      argeaten = 0; 
    else 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc));
    }
  \,/[[:blank:]] {
    if(argeaten) 
      argeaten = 0; 
    else 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc));
    yy_push_state(KILLBLANK, yyscanner);
    }
  \) {
    if(!argeaten && md->args->length != 0) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    /*last arg encountered*/
    yy_pop_state(yyscanner);
    yy_push_state(DEFINE2, yyscanner); 
    mdstrdly = strctor(malloc(256), 0, 256);
    }
  \)/[[:blank:]] {
    if(!argeaten && md->args->length != 0) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    /*last arg encountered*/
    yy_pop_state(yyscanner);
    yy_push_state(DEFINE2, yyscanner); 
    mdstrdly = strctor(malloc(256), 0, 256); 
    yy_push_state(KILLBLANK, yyscanner);
    }
  \n {yy_pop_state(yyscanner); yy_pop_state(yyscanner);/*error state*/}
  . {fprintf(stderr, "DEFINE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<DEFINE2>{
  [^\\/\n]+ {dscat(mdstrdly, yytext, yyleng);}
  "/" {dsccat(mdstrdly, '/');}
  \\ {dsccat(mdstrdly, '\\');}
  \n {
    yy_pop_state(yyscanner);
    yy_pop_state(yyscanner);
    dsccat(mdstrdly, 0);
    struct macrodef* isinplace;
    if((isinplace = search(lctx->defines, defname))) {
      assert(!strcmp(isinplace->text->strptr, mdstrdly->strptr));
      freemd(isinplace);
    }
    md->text = mdstrdly;
    insert(lctx->defines, defname, md);
    rmpair(lctx->withindefines, defname);
    free(defname);
    defname = NULL;
    }
}

<UNDEF>{
  {IDENT} {rmpaircfr(lctx->defines, yytext, (void(*)(void*)) freemd);}
  {IDENT}/[[:blank:]] {rmpaircfr(lctx->defines, yytext, (void(*)(void*)) freemd); yy_push_state(KILLBLANK, yyscanner);}
  \n {yy_pop_state(yyscanner); yy_pop_state(yyscanner);/*error state if expr not over?*/}
  . {fprintf(stderr, "UNDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}


<IFDEF>{
  {IDENT} {stmtover = 1; defname = strdup(yytext);}
  {IDENT}/[[:blank:]] {
    stmtover = 1;
    defname = strdup(yytext);
    yy_push_state(KILLBLANK, yyscanner);
    }
  \n {
    yy_pop_state(yyscanner);
    yy_pop_state(yyscanner); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifdef! %s %d.%d-%d.%d\n", locprint2(yylloc));
    } else {
      DYNARR* ds = lctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;
      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
          if(ppdebug) 
              fprintf(stderr, "Value of identifier %s is %d at %s %d.%d-%d.%d\n", defname, queryval(lctx->defines, defname), locprint2(yylloc));
          if(queryval(lctx->defines, defname)) {
            *rids = IFANDTRUE;
          } else {
            *rids = IFANDFALSE;
            yy_push_state(PPSKIP, yyscanner);
          }
          break;
        default:
          *rids= IFDEFDUMMY;
          yy_push_state(PPSKIP, yyscanner);
          break;
      }
      dapush(ds, rids);
    }
    free(defname);
    defname = NULL;
    }
  . {fprintf(stderr, "IFDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}
<IFNDEF>{
  {IDENT} {stmtover = 1; defname = strdup(yytext);}
  {IDENT}/[[:blank:]] {
    stmtover = 1;
    defname = strdup(yytext);
    yy_push_state(KILLBLANK, yyscanner);
    }
  \n {
    yy_pop_state(yyscanner);
    yy_pop_state(yyscanner); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifndef! %s %d.%d-%d.%d\n", locprint2(yylloc));
    } else {
      DYNARR* ds = lctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;

      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
          if(queryval(lctx->defines, defname)) {
            *rids = IFANDFALSE;
            yy_push_state(PPSKIP, yyscanner);
          } else {
            *rids = IFANDTRUE;
          }
          break;
        default:
          *rids= IFDEFDUMMY;
          yy_push_state(PPSKIP, yyscanner);
          break;
      }
      dapush(ds, rids);
    }
    free(defname);
    defname = NULL;
    }
  . {fprintf(stderr, "IFNDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<CALLMACRO>{
  \( {
    ++paren_depth;
    dsccat(dstrdly, '(');
    }
  [[:space:]]*\) {
    /*if 0 arguments, don't add new argument here*/
    if(paren_depth) {
      --paren_depth;
      dsccat(dstrdly, ')');
    } else {
      dapush(parg, dstrdly);

      struct macrodef* mdl;
      if(!(mdl = search(lctx->defines, defname))) {
        fprintf(stderr, "Error: Malformed function-like macro call %s %d.%d-%d.%d\n", locprint2(yylloc));
        //error state
        yy_pop_state(yyscanner);
      } else {
        if(mdl->args->length == 0 && parg->length == 1) {
          strdtor(dapop(parg));
        }
        if(parg->length != mdl->args->length) {
          fprintf(stderr, "Error: the number of arguments passed to function-like macro is different than the number of parameters %s %d.%d-%d.%d\n", locprint2(yylloc));
          //error state
        } else {
          insert(lctx->withindefines, defname, NULL);
          DYNARR* argn = mdl->args;
          defargs = htctor();
          char** prma = (char**) argn->arr;
          DYNSTR** arga = (DYNSTR**) parg->arr;
          for(int i = 0; i < parg->length; i++) {
            insert(defargs, prma[i], arga[i]);
          }
          dadtor(parg);
          dstrdly = strctor(malloc(256), 0, 256);
          yy_pop_state(yyscanner);
          yy_push_state(FINDREPLACE, yyscanner);
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = *yylloc;
          dapush(locs, ylt);
          yylloc->first_line = yylloc->last_line = 1;
          yylloc->first_column = yylloc->last_column = 0;
          FILE* fmm = fmemopen(NULL, mdl->text->lenstr, "r+");
          fwrite(mdl->text->strptr, 1, mdl->text->lenstr, fmm);
          fseek(fmm, 0, SEEK_SET);
          rewind(fmm);
          YY_BUFFER_STATE ybs = yy_create_buffer(fmm, YY_BUF_SIZE, yyscanner);
          yypush_buffer_state(ybs, yyscanner);
        }
      }
    }
    }

  {IDENT} {
    char* dupdstr = strdup(yytext);
    if(check_type(dupdstr, 2, yyscanner) >= 0) {
      dscat(dstrdly, yytext, yyleng);
      free(dupdstr);
    }
    }
  (0[bB]{BIN}+|0{OCT}+|[[:digit:]]+|0[xX][[:xdigit:]]+){INTSIZE}? {
    dscat(dstrdly, yytext, yyleng);
    }
  ([[:digit:]]+|[[:digit:]]*"."?|[[:digit:]]+"."?)[[:digit:]]*({EXP})?{FLOATSIZE}? {
    dscat(dstrdly, yytext, yyleng);
    }
  [^\(\)\",[:alnum:]_\0]*[^[:space:]\(\)\",[:alnum:]_\0] {/*"*/
    dscat(dstrdly, yytext, yyleng);
    }
  \"(\\.|[^\\"]|\/[[:space:]]*\n)*\" {/*"*/
  	//this is here to make sure that parenthesis depth isn't changed within strings
    dscat(dstrdly, yytext, yyleng);
    }
  \'(\\.|[^\\']|\/[[:space:]]*\n)*\' {
  	//this is here to make sure that parenthesis depth isn't changed within char literals
    dscat(dstrdly, yytext, yyleng);
    }
  [[:space:]]+ {
    dsccat(dstrdly, ' ');
    }
  [[:space:]]*,[[:space:]]* {
    if(paren_depth) {
      char tmpstr[3];
      int tmpstrl = 0;
      if(yytext[0] == ' ')
        tmpstr[tmpstrl++] = ' ';
      tmpstr[tmpstrl++] = ',';
      if(yytext[yyleng - 1] == ' ')
        tmpstr[tmpstrl++] = ' ';
      dscat(dstrdly, tmpstr, tmpstrl);
    } else {
      dapush(parg, dstrdly);
      dstrdly = strctor(malloc(256), 0, 256);
    }
    }
  \0 {
    fclose(YY_CURRENT_BUFFER->yy_input_file);
    yypop_buffer_state(yyscanner);
    if ( !YY_CURRENT_BUFFER ) {
      yyterminate();
    } else {
      yy_pop_state(yyscanner);
      YYLTYPE* ylt = dapop(locs);
      *yylloc = *ylt;
      free(ylt);
      free(dapop(file2compile));
      struct arginfo* ai = dapop(lctx->argpp);
      rmpair(lctx->withindefines, defname);
      free(defname);
      defname = ai->defname;
      free(ai);
    }
    }
  <<EOF>> {
    fclose(YY_CURRENT_BUFFER->yy_input_file);
    yypop_buffer_state(yyscanner);
    if ( !YY_CURRENT_BUFFER ) {
      yyterminate();
    } else {
      yy_pop_state(yyscanner);
      YYLTYPE* ylt = dapop(locs);
      *yylloc = *ylt;
      free(ylt);
      free(dapop(file2compile));
      struct arginfo* ai = dapop(lctx->argpp);
      rmpair(lctx->withindefines, defname);
      free(defname);
      defname = ai->defname;
      free(ai);
    }
    }
  . {fprintf(stderr, "Error: unexpected character in function macro call %s %d.%d-%d.%d\n", locprint2(yylloc));}
}

<FINDREPLACE>{
  {IDENT} {
    DYNSTR* argy = search(defargs, yytext);
    if(argy) {
      dscat(dstrdly, argy->strptr, argy->lenstr);
    } else {
      dscat(dstrdly, yytext, yyleng);
    }
    }
  #{IDENT} {
    DYNSTR* argy = search(defargs, yytext + 1);
    if(argy) {
      dsccat(dstrdly, '"');
      for(int i = 0; i < argy->lenstr; ++i) {
        if(argy->strptr[i] == '"') {
          dsccat(dstrdly, '\\');
        }
        dsccat(dstrdly, argy->strptr[i]);
      }
      dsccat(dstrdly, '"');
    } else {
      dscat(dstrdly, yytext, yyleng);
    }
    }
  ([[:blank:]]+|"/*"([^*]|\*)*"*/")*##([[:blank:]]+|"/*"([^*]|\*)*"*/")* { }
  \"(\\.|[^\\"]|\/[[:blank:]]*\n)*\" {/*"*/
    dscat(dstrdly, yytext, yyleng);
    }
  '(\\.|[^\\'\n])+' {
    dscat(dstrdly, yytext, yyleng);
    }
  [^[:alnum:]_\'\"\n[:blank:]/#]+ {/*cntrl/delim expr*/
    dscat(dstrdly, yytext, yyleng);
    }
  [[:blank:]]+ {dsccat(dstrdly, ' ');}
  [[:cntrl:][:print:]] {
    dsccat(dstrdly, *yytext);
    }
  <<EOF>> {
    fclose(YY_CURRENT_BUFFER->yy_input_file);
    yypop_buffer_state(yyscanner);
    yy_pop_state(yyscanner);
    if(ppdebug) printf("now lexing buffer containing %s\n", dstrdly->strptr);
    FILE* fmm = fmemopen(NULL, dstrdly->lenstr, "r+");
    fwrite(dstrdly->strptr, 1, dstrdly->lenstr, fmm);
    fseek(fmm, 0, SEEK_SET);
    rewind(fmm);
    YY_BUFFER_STATE ybs = yy_create_buffer(fmm, YY_BUF_SIZE, yyscanner);
    strdtor(dstrdly);
    yypush_buffer_state(ybs, yyscanner);
    char buf[256];
    snprintf(buf, 256, "%s", defname);
    yylloc->first_line = yylloc->last_line = 1;
    yylloc->first_column = yylloc->last_column = 0;
    dapush(file2compile, strdup(buf));
    rmpair(lctx->withindefines, defname);
    htdtorcfr(defargs, (void(*)(void*)) strdtor);
    if(lctx->argpp->length) {
      struct arginfo* argi = dapop(lctx->argpp);
      if(argi->argi) {
        dstrdly = argi->argi;
        paren_depth = argi->pdepth;
        parg = argi->parg;
        argi->argi = NULL;
        dapush(lctx->argpp, argi);
        insert(lctx->withindefines, defname, NULL);
      } else {
        free(defname);
        defname = argi->defname;
        free(argi);
      }
    } else {
      free(defname);
      defname = NULL;
    }
    }
}

<CHECKDEFINED>{
  [[:blank:]]*\) {yy_pop_state(yyscanner);}
  {IDENT} {
    //maybe check completion
    yylval_param->unum = queryval(lctx->defines, yytext);
    if(ppdebug) 
      fprintf(stderr, "Value of identifier %s is %lu at %s %d.%d-%d.%d\n", yytext, yylval_param->unum, locprint2(yylloc));
    return UNSIGNED_LITERAL;
    }
}

<CHECKDEFINED2>{
  {IDENT} {
    yy_pop_state(yyscanner);
    yylval_param->unum = queryval(lctx->defines, yytext);
    if(ppdebug) 
      fprintf(stderr, "Value of identifier %s is %lu at %s %d.%d-%d.%d\n", yytext, yylval_param->unum, locprint2(yylloc));
    return UNSIGNED_LITERAL;
    }
}


<INITIAL,SINGLELINE_COMMENT,PREPROCESSOR,INCLUDE,DEFINE,DEFINE2,IFDEF,IFNDEF,STRINGLIT,WITHINIF,KILLUNTIL,KILLBLANK>\\+[[:blank:]]*\n {/*the newline is ignored*/}
"->" {return ARROWTK;}
"++" {return INC;}
"--" {return DEC;}
"/=" {return DIV_GETS;}
"*=" {return MUL_GETS;}
"%=" {return MOD_GETS;}
"+=" {return ADD_GETS;}
"-=" {return SUB_GETS;}
"<<=" {return SHL_GETS;}
">>=" {return SHR_GETS;}
"&=" {return AND_GETS;}
"^=" {return XOR_GETS;}
"|=" {return OR_GETS;}
"..." {return ELLIPSIS;}
"typedef" {return TYPEDEF;}
"static" {return STATIC;}
"extern" {return EXTERN;}
"inline" {return INLINE;}
"signed" {return SIGNED;}
"unsigned" {return UNSIGNED;}
"const" {return CONST;}
"volatile" {return VOLATILE;}
"restrict" {return RESTRICT;}
"char" {return CHAR;}
"short" {return INT16;}
"int" {return INT32;}
"long" {return INT64;}
"int8" {return INT8;}
"int16" {return INT16;}
"int32" {return INT32;}
"int64" {return INT64;}
"byte" {return BYTE;}
"dbyte" {return DBYTE;}
"qbyte" {return QBYTE;}
"obyte" {return OBYTE;}
"single" {return SINGLE;}
"float" {return SINGLE;}
"double" {return DOUBLE;}
"void" {return VOID;}
"case" {return CASETK;}
"default" {return DEFAULTTK;}
"if" {return IF;}
"else" {return ELSE;}
"switch" {return SWITCHTK;}
"while" {return WHILE;}
"do" {return DO;}
"for" {return FOR;}
"goto" {return GOTO;}
"continue" {return CONTINUE;}
"break" {return BREAK;}
"return" {return RETURN;}
"sizeof" {return SIZEOF;}
"struct" {return STRUCTTK;}
"enum" {return ENUMTK;}
"union" {return UNIONTK;}
<INITIAL,WITHINIF>{
  "<<" {return SHLTK;}
  ">>" {return SHRTK;}
  "&&" {return AND;}
  "||" {return OR;}
  "!=" {return NEQTK;}
  "==" {return EQTK;}
  "<=" {return LE;}
  ">=" {return GE;}
  "<" {return '<';}
  ">" {return '>';}
  "!" {return '!';}
  "-" {return '-';}
  "(" {return '(';}
  ")" {return ')';}
  "|" {return '|';}
  "+" {return '+';}
  "/" {return '/';}
  "*" {return '*';}
  "~" {return '~';}
  "^" {return '^';}
  "&" {return '&';}
  "?" {return '?';}
  ":" {return ':';}
}
"=" {return '=';}
"[" {return '[';}
"]" {return ']';}
"," {return ',';}
"{" {return '{';}
"}" {return '}';}
";" {return ';';}
"%" {return '%';}
"." {return '.';}

"__FILE__" {char* fstr = dapeek(file2compile); unput('"', yyscanner); UNPUTSTR(fstr); unput('"', yyscanner);}
"__LINE__" {char linebuf[16]; sprintf(linebuf, "%d", yyget_lloc(yyscanner)->last_line); unput('"', yyscanner); UNPUTSTR(linebuf); unput('"', yyscanner);}
"__DATE__" {time_t tim = time(NULL); struct tm* tms = localtime(&tim); char datebuf[14]; strftime(datebuf, 14, "\"%b %e %Y\"", tms); UNPUTSTR(datebuf);}
"__TIME__" {time_t tim = time(NULL); struct tm* tms = localtime(&tim); char timebuf[11]; strftime(timebuf, 11, "\"%T\"",tms); UNPUTSTR(timebuf);}
"__func__" {char* namestr = lctx->func->name; unput('"', yyscanner); UNPUTSTR(namestr); unput('"', yyscanner);}

<WITHINIF>{
  "defined"[[:blank:]]*"("[[:blank:]]* {yy_push_state(CHECKDEFINED, yyscanner);}
  "defined"[[:blank:]]* {yy_push_state(CHECKDEFINED2, yyscanner);}
  {IDENT} {
    char* ds = strdup(yytext);
    int mt = check_type(ds, 0, yyscanner);
    switch(mt) {
      default:
        free(ds);
        yylval_param->unum = 0;
        return UNSIGNED_LITERAL;
      case -1:
        break;
    } 
  }
  0[bB]{BIN}+{INTSIZE}? {
        yylval_param->unum = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                         return UNSIGNED_LITERAL;}
  0{OCT}+{INTSIZE}? {
        yylval_param->unum = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                     return UNSIGNED_LITERAL;}
  [[:digit:]]+{INTSIZE}?  {
        yylval_param->snum = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                           if(strchr(yytext,'u') || strchr(yytext,'U')) return UNSIGNED_LITERAL; return INTEGER_LITERAL;}
  0[xX][[:xdigit:]]+{INTSIZE}? {
        yylval_param->unum = strtoul(yytext,NULL,16);
                              return UNSIGNED_LITERAL;}
  L?\' {yy_push_state(ZCHARLIT, yyscanner);}
}

{IDENT} {
  char* ylstr = strdup(yytext);
  int mt = check_type(ylstr, 1, yyscanner);
  switch(mt) {
    case SYMBOL: case TYPE_NAME: ;
      yylval_param->str = ylstr;
      return mt;
  } 
}

0[bB]{BIN}+{INTSIZE}? {
      yylval_param->unum = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                       return UNSIGNED_LITERAL;}
0{OCT}+{INTSIZE}? {
      yylval_param->unum = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                   return UNSIGNED_LITERAL;}
[[:digit:]]+{INTSIZE}?  {
      yylval_param->snum = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                         if(strchr(yytext,'u') || strchr(yytext,'U')) return UNSIGNED_LITERAL; return INTEGER_LITERAL;}
0[xX][[:xdigit:]]+{INTSIZE}? {
      yylval_param->unum = strtoul(yytext,NULL,16);
                              return UNSIGNED_LITERAL;}
L?\' {yy_push_state(CHARLIT, yyscanner);}

[[:digit:]]+{EXP}{FLOATSIZE}?|[[:digit:]]*"."?[[:digit:]]+({EXP})?{FLOATSIZE}?|[[:digit:]]+"."?[[:digit:]]*({EXP})?{FLOATSIZE}?  {
     sscanf(yytext, "%lf", &yylval_param->dbl); return FLOAT_LITERAL;}

<ZCHARLIT>{
  \' {
    fprintf(stderr, "Error: 0 length character literal %s %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    ZGOC('?');
  	}
  [\n\v] {
    fprintf(stderr, "Error: character literal terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint2(yylloc));
    yy_pop_state(yyscanner);
    }
  \\a\' {ZGOC('\a');}
  \\b\' {ZGOC('\b');}
  \\e\' {ZGOC('\33');}
  \\f\' {ZGOC('\f');}
  \\n\' {ZGOC('\n');}
  \\r\' {ZGOC('\r');}
  \\t\' {ZGOC('\t');}
  \\v\' {ZGOC('\v');}
  \\\'\' {ZGOC('\'');}
  \\\"\' {/*"*/ZGOC('\"');}
  \\\\\' {ZGOC('\\');}
  \\\?\' {ZGOC('\?');}
  \\[0-7]{1,3}\' {
    unsigned int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    }
    ZGOC((char) result);
    }
  \\0x[[:xdigit:]]{1,2}\' {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    ZGOC((char) result);
    }
  \\.\' {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    ZGOC(yytext[1]);
    }
  [^\\\'\n\v]\' {
    ZGOC(yytext[0]);
    }
  [^\']{2,}\' {
    fprintf(stderr, "Error: character literal too long %s %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    ZGOC(yytext[1]);
    }
}

<CHARLIT>{
  \' {
    fprintf(stderr, "Error: 0 length character literal %s %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    GOC('?');
  	}
  [\n\v] {
    fprintf(stderr, "Error: character literal terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint2(yylloc));
    yy_pop_state(yyscanner);
    }
  \\a\' {GOC('\a');}
  \\b\' {GOC('\b');}
  \\e\' {GOC('\33');}
  \\f\' {GOC('\f');}
  \\n\' {GOC('\n');}
  \\r\' {GOC('\r');}
  \\t\' {GOC('\t');}
  \\v\' {GOC('\v');}
  \\\'\' {GOC('\'');}
  \\\"\' {/*"*/GOC('\"');}
  \\\\\' {GOC('\\');}
  \\\?\' {GOC('\?');}
  \\[0-7]{1,3}\' {
    unsigned int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    }
    GOC((char) result);
    }
  \\0x[[:xdigit:]]{1,2}\' {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    GOC((char) result);
    }
  \\.\' {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    GOC(yytext[1]);
    }
  [^\\\'\n\v]\' {
    GOC(yytext[0]);
    }
  [^\']{2,}\' {
    fprintf(stderr, "Error: character literal too long %s %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    GOC(yytext[1]);
    }
}

L?\" {/*"*/yy_push_state(STRINGLIT, yyscanner); strcur = strctor(malloc(256), 0, 256);}
<STRINGLIT>{
  \" {/*"*/
    dsccat(strcur, 0);
    yylval_param->dstr = strcur;
  	yy_pop_state(yyscanner); 
  	return STRING_LITERAL;
  	}
  [\n\v] {
    strdtor(strcur);
    free(strconst); 
    fprintf(stderr, "Error: String terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint2(yylloc));
    yy_pop_state(yyscanner);
    }
  \\a {dsccat(strcur, '\a');}
  \\b {dsccat(strcur, '\b');}
  \\e {dsccat(strcur, '\33');}
  \\f {dsccat(strcur, '\f');}
  \\n {dsccat(strcur, '\n');}
  \\r {dsccat(strcur, '\r');}
  \\t {dsccat(strcur, '\t');}
  \\v {dsccat(strcur, '\v');}
  \\\' {dsccat(strcur, '\'');}
  \\\" {dsccat(strcur, '\"');/*"*/}
  \\\\ {dsccat(strcur, '\\');}
  \\\? {dsccat(strcur, '\?');}
  \\[0-7]{1,3} {
    unsigned int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    }
    dsccat(strcur, result);
    }
  \\0x[[:xdigit:]]{1,2} {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    dsccat(strcur, result);
    }
  \\. {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    dsccat(strcur, yytext[1]);
  }
  [^\\\"\v]+ {/*"*/
    dscat(strcur, yytext, yyleng);
  }
}
<INITIAL,WITHINIF>[[:blank:]]+ {/*Whitespace, ignored*/}
<WITHINIF>\n {
  yy_pop_state(yyscanner);
  yy_push_state(ENDWITHINIF, yyscanner);
  unput('\n', yyscanner);
  return 0;
}
<ENDWITHINIF>\n {
  yy_pop_state(yyscanner); 
  while(foldconst(&lctx->ifexpr)) ;
  enum ifdefstate* rids;
  switch(lctx->ifexpr->type) {
    case INT: case UINT:
      if(ppdebug)
        fprintf(stderr, "exprval %ld %s %d.%d-%d.%d\n", lctx->ifexpr->intconst, locprint2(yylloc));
      if(lctx->ifexpr->intconst != 0) {
        rids = malloc(sizeof(enum ifdefstate));
        *rids = IFANDTRUE;
        break;
      }
      //fall through
    case NOP:
      rids = malloc(sizeof(enum ifdefstate));
      *rids = IFANDFALSE;
      yy_push_state(PPSKIP, yyscanner);
      break;
    default:
      yy_pop_state(yyscanner);
      fprintf(stderr, "ERROR: subsidiary parser reduced if or elif into non-rectifiable expression %s %d.%d-%d.%d\n", locprint2(yylloc));
  }
  rfreexpr(lctx->ifexpr);
  lctx->ifexpr = NULL;
  dapush(lctx->definestack, rids);
}
[[:space:]] {/*Whitespace, ignored*/}

<<EOF>> {
  fclose(YY_CURRENT_BUFFER->yy_input_file);
  yypop_buffer_state(yyscanner);
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state(yyscanner);
    stmtover = 1;
    YYLTYPE* ylt = dapop(locs);
    *yylloc = *ylt;
    free(ylt);
    char* ismac = dapop(file2compile);
    rmpair(lctx->withindefines, ismac);
    free(ismac);
    if(defname) {
      free(defname);
      defname = NULL;
    }
    //rmpair is a no-op if not in hash
  }
}

<*>\0 {//same as EOF
  fclose(YY_CURRENT_BUFFER->yy_input_file);
  yypop_buffer_state(yyscanner);
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state(yyscanner);
    stmtover = 1;
    YYLTYPE* ylt = dapop(locs);
    *yylloc = *ylt;
    free(ylt);
    char* ismac = dapop(file2compile);
    rmpair(lctx->withindefines, ismac);
    free(ismac);
    if(defname) {
      free(defname);
      defname = NULL;
    }
    //rmpair is a no-op if not in hash
  }
}

<*>. {fprintf(stderr, "Unexpected character encountered: '%c' %d %s %d.%d-%d.%d\n", *yytext, *yytext, locprint2(yylloc));}
<*>\n {fprintf(stderr, "Unexpected newline encountered:  %s %d.%d-%d.%d\n", locprint2(yylloc));}
%%

int check_type(char* symb, char frominitial, yyscan_t yyscanner) {
  struct macrodef* macdef = search(lctx->defines, symb);
  if(macdef && !queryval(lctx->withindefines, symb)) {
    char* oldname = defname;
    defname = symb;
    switch(frominitial) {
      case 0:
        yy_push_state(WITHINIF, yyscanner);
        break;
      case 1:
        yy_push_state(INITIAL, yyscanner);
        break;
      case 2:
        //don't push callmacro yet
        break;
    }
    YYLTYPE* yltg = yyget_lloc(yyscanner);
    if(macdef->args) {
      char c;
      while(1) {
        c = input(yyscanner);
        ++yltg->last_column;
        switch(c) {
          case '\n': 
            yltg->last_column = 0;
            ++yltg->last_line;
          case ' ': case '\t': case '\v':
          	break;
          case '(':
          	goto whiledone;
          default:
            --yltg->last_column;
            unput(c, yyscanner);
            goto nofcall;

        }
      }
      whiledone:
      yy_push_state(CALLMACRO, yyscanner);
      if(frominitial == 2) {
        struct arginfo* argi;
        argi = malloc(sizeof(struct arginfo));
        argi->argi = dstrdly;
        argi->pdepth = paren_depth;
        argi->defname = oldname;
        argi->parg = parg;
        dapush(lctx->argpp, argi);
        yy_push_state(CALLMACRO, yyscanner);
      }
      paren_depth = 0;
      dstrdly = strctor(malloc(256), 0, 256);
      parg = dactor(64);
    } else {
      char* buf = malloc(256);
      snprintf(buf, 256, "%s", symb);
      dapush(file2compile, buf);
      YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
      *ylt = *yltg;
      dapush(locs, ylt);
      yltg->first_line = yltg->last_line = 1;
      yltg->first_column = yltg->last_column = 0;
      FILE* fmm = fmemopen(NULL, macdef->text->lenstr, "r+");
      fwrite(macdef->text->strptr, 1, macdef->text->lenstr, fmm);
      fseek(fmm, 0, SEEK_SET);
      rewind(fmm);
      YY_BUFFER_STATE yms = yy_create_buffer(fmm, YY_BUF_SIZE, yyscanner);
      yypush_buffer_state(yms, yyscanner);
      insert(lctx->withindefines, symb, NULL);
      if(frominitial == 2) {
        struct arginfo* argi = calloc(1, sizeof(struct arginfo));
        argi->defname = oldname;
        dapush(lctx->argpp, argi);
        yy_push_state(CALLMACRO, yyscanner);
      } else {
        free(defname);
        defname = NULL;
      }
    }
    return -1;
  }
  nofcall:
  if(scopequeryval(lctx, M_TYPEDEF, symb)) {
    return TYPE_NAME;
  }
  return SYMBOL;
}

