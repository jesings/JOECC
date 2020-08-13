BIN [0-1]
OCT [0-7]
IDENT [[:alpha:]_][[:alnum:]_]*
EXP [Ee][+-]?[[:digit:]]+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*
%{
//TODO: computed include?

#include <math.h>
#include <time.h>
#include <sys/stat.h>
#include "joecc.tab.h"
#include "compintern.h"
#include "treeduce.h"

#define YY_USER_ACTION \
    yylloc.first_line = yylloc.last_line; \
    yylloc.first_column = yylloc.last_column; \
    for(int i = 0; i < yyleng; i++) { \
        if(yytext[i] == '\n') { \
            yylloc.last_line++; \
            yylloc.last_column = 0; \
        } \
        else { \
            yylloc.last_column++; \
        } \
    }
#define SIGNEDCHAR 0

int zzparse(void);
int check_type(char* symb, char frominitial);
extern struct lexctx* ctx;
char stmtover, skipping;
char* defname, * strconst;
int paren_depth;
struct macrodef* md;
char charconst, argeaten;
DYNARR* parg;
HASHTABLE* defargs = NULL;
DYNSTR* dstrdly, * mdstrdly, * strcur;
extern DYNARR* locs, * file2compile;

struct arginfo {
  DYNSTR* argi;
  int pdepth;
  char* defname;
  DYNARR* parg;
};

extern union {
  unsigned long unum;
  EXPRESSION* exprvariant;
} zzlval;

#define GOC(c) yylval.unum = c, yy_pop_state(); return UNSIGNED_LITERAL
#define ZGOC(c) zzlval.unum = c, yy_pop_state(); return UNSIGNED_LITERAL
#define UNPUTSTR(str) for(int l = strlen(str); l; unput(str[--l])) --yylloc.last_column
%}
%option yylineno
%option noyywrap
%option stack

%option debug
%option warn
%option nodefault

%x MULTILINE_COMMENT SINGLELINE_COMMENT
%x PREPROCESSOR INCLUDE INCLUDENEXT
%x DEFINE UNDEF DEFARG DEFINE2
%x WARNING ERROR
%x IFNDEF IFDEF PPSKIP
%x KILLBLANK KILLCONCAT KILLUNTIL
%x STRINGLIT CHARLIT ZCHARLIT
%x CALLMACRO FINDREPLACE
%x WITHINIF CHECKDEFINED CHECKDEFINED2 ENDWITHINIF

%%
<KILLBLANK>{
  ([[:blank:]]+|\\[[:blank:]]*\n)* {yy_pop_state();}
}
<KILLCONCAT>{
  ##/("/*"([^*]|\*)*"*/")* {yy_pop_state();}
  ##/([[:blank:]]+|"/*"([^*]|\*)*"*/")* {yy_pop_state();yy_push_state(KILLBLANK);}
}

<KILLUNTIL>{
  [^/\n\\]+ {}
  \n {yy_pop_state();}
  "/" {}
  \\ {}
  "//" {yy_pop_state(); yy_push_state(SINGLELINE_COMMENT);}
  "/*" {yy_push_state(MULTILINE_COMMENT);}
}

<INITIAL,PREPROCESSOR,INCLUDE,DEFINE,UNDEF,DEFARG,DEFINE2,IFDEF,IFNDEF,CALLMACRO,PPSKIP,KILLBLANK,KILLCONCAT>{
  "/*" {yy_push_state(MULTILINE_COMMENT);}
  "//" {yy_push_state(SINGLELINE_COMMENT);}
}

<MULTILINE_COMMENT>{
  [^*]+ {/*The multiline comment is not terminated*/}
  "*"+ {/*The multiline comment is not terminated*/}
  "*"+"/" {yy_pop_state();}
}

<SINGLELINE_COMMENT>{
  [^\\\n]+ {/*The single line comment is not terminated*/}
  \\+ {/*The single line comment is not terminated*/}
  [^\\\n]+$ {yy_pop_state();}
}

<PPSKIP>{
  ^[[:blank:]]*#[[:blank:]]* {
    yy_push_state(PREPROCESSOR);
    stmtover = 0;
    skipping = 1;
    }
  [^\n#\/]+ {}
  [\/\n#] {}
}

^[[:blank:]]*#[[:blank:]]* {yy_push_state(PREPROCESSOR); stmtover = 0; skipping = 0;}
<PREPROCESSOR>{
  include[[:blank:]]+ {if(!skipping) yy_push_state(INCLUDE); else yy_pop_state();}
  include_next[[:blank:]]+ {if(!skipping) yy_push_state(INCLUDENEXT); else yy_pop_state();}
  define[[:blank:]]+ {if(!skipping) {yy_push_state(DEFINE); md = calloc(1, sizeof(struct macrodef));} else yy_pop_state();}
  undef[[:blank:]]+ {if(!skipping) yy_push_state(UNDEF); else yy_pop_state();}
  ifdef[[:blank:]]+ {yy_push_state(IFDEF);}
  ifndef[[:blank:]]+ {yy_push_state(IFNDEF);}
  else {
    yy_pop_state();
    DYNARR* ds = ctx->definestack;
    if(!ds->length) {
      fprintf(stderr, "ERROR: else without preceding if %s %d.%d-%d.%d\n", locprint(yylloc));
      exit(-1);
    }
    enum ifdefstate* ids = dapeek(ds);
    switch(*ids) {
      case IFANDTRUE: 
        *ids = ELSEANDTRUE;
        yy_push_state(PPSKIP);
        break;
      case IFANDFALSE: 
        *ids = ELSEANDFALSE;
        yy_pop_state();
        break;
      default:
        fprintf(stderr, "Error: Unexpected #else %s %d.%d-%d.%d\n", locprint(yylloc));
      case IFDEFDUMMY:
        break;
    }
    yy_push_state(KILLUNTIL);
    }
  elif {
    yy_pop_state();
    DYNARR* ds = ctx->definestack;
    if(!ds->length) {
      fprintf(stderr, "ERROR: elif without preceding if %s %d.%d-%d.%d\n", locprint(yylloc));
      exit(-1);
    }
    enum ifdefstate* ids = dapeek(ds);
    switch(*ids) {
      case IFANDTRUE: 
        *ids = IFDEFDUMMY;
        yy_push_state(PPSKIP);
        break;
      case IFANDFALSE: 
        yy_pop_state();
        free(dapop(ds));
        yy_push_state(WITHINIF);
        zzparse();
        break;
      case IFDEFDUMMY:
        break;
      default:
        fprintf(stderr, "Error: Unexpected #elif %s %d.%d-%d.%d\n", locprint(yylloc));
    }
    }
  endif {/*handle endif case*/
    yy_pop_state();
    DYNARR* ds = ctx->definestack;
    if(ds->length > 0) {
      enum ifdefstate* ids = dapop(ds);
      switch(*ids) {
        case IFANDTRUE: case ELSEANDFALSE:
          break;
        default: //case IFANDFALSE: case ELSEANDTRUE: case IFDEFDUMMY:
          yy_pop_state();
          break;
      }
      free(ids);
    } else {
      fprintf(stderr, "Error: Unexpected #endif %s %d.%d-%d.%d\n", locprint(yylloc));
    }
    yy_push_state(KILLUNTIL);
    }
  if {/*handle if case*/
    yy_pop_state();
    DYNARR* ds = ctx->definestack;
    enum ifdefstate ids = ds->length > 0 ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;
    enum ifdefstate* rids;
    switch(ids) {
      default:
        rids = malloc(sizeof(enum ifdefstate));
        *rids = IFDEFDUMMY;
        dapush(ds, rids);
        yy_push_state(PPSKIP);
        break;
      case IFANDTRUE: case ELSEANDFALSE:
        yy_push_state(WITHINIF);
        zzparse();
        break;
    }
    }
  line {if(!skipping){yy_pop_state(); yy_push_state(SINGLELINE_COMMENT);} else {yy_pop_state(); yy_push_state(KILLUNTIL);}/*TODO: line directive currently doesn't print requisite information*/}
  warning {if(!skipping){yy_pop_state(); yy_push_state(WARNING); yy_push_state(KILLBLANK);} else {yy_pop_state(); yy_push_state(KILLUNTIL);}}
  error {if(!skipping){yy_pop_state(); yy_push_state(ERROR); yy_push_state(KILLBLANK);} else {yy_pop_state(); yy_push_state(KILLUNTIL);}}
  \n {yy_pop_state();fprintf(stderr, "PREPROCESSOR: Incorrect line end %d %s %d.%d-%d.%d\n", yylloc.first_line, locprint(yylloc));}
  . {fprintf(stderr, "PREPROCESSOR: Unexpected character encountered: %d %c %s %d.%d-%d.%d\n", *yytext, *yytext, locprint(yylloc));}
}
<WARNING>\"(\\.|[^\\"]|\/[[:space:]]*\n)*\" {/*"*/
    /*WARNING, ERROR, only support single string const*/
    yy_pop_state(); 
    yy_push_state(KILLUNTIL); 
    yytext[yyleng - 1] = '\0'; 
    fprintf(stderr, "WARNING: %s\n", yytext + 1);
    }
<ERROR>\"(\\.|[^\\"]|\/[[:space:]]*\n)*\" {/*"*/
    yy_pop_state(); 
    yy_push_state(KILLUNTIL); 
    yytext[yyleng - 1] = '\0'; 
    fprintf(stderr, "ERROR: %s\n", yytext + 1);
    exit(-1);
    }

<INCLUDE>{
  "<"[^>\n]*">" {
    if(stmtover){
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yylloc.last_line, locprint(yylloc));
    } else {
      stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing >
      char pathbuf[2048];
      static const char* searchpath[] = {
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.1.0/include",
        "/usr/local/include",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.1.0/include-fixed",
        "/usr/include",
        };
      yy_pop_state();
      yy_pop_state();
      int i;
      for(i = 0; i < 4 /*sizeof searchpath*/; i++) {
        FILE* newbuf;
        snprintf(pathbuf, 2048, "%s/%s", searchpath[i], yytext + 1); //ignore opening <
        if((newbuf = fopen(pathbuf, "r")) != NULL) {
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = yylloc;
          dapush(locs, ylt);
          yylloc.first_line = yylloc.last_line = 1;
          yylloc.first_column = yylloc.last_column = 0;
          dapush(file2compile, strdup(pathbuf));
          YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE);
          yy_push_state(INITIAL);
          yypush_buffer_state(ybs);
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
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yylloc.last_line, locprint(yylloc));
    } else {
      stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing "
      yy_pop_state();
      yy_pop_state();
      FILE* newbuf;
      char* pfstr = dapeek(file2compile);
      char* fname = yytext + 1;
      if(strchr(pfstr, '/')) {
          char pathbuf[2048];
          strncpy(pathbuf, pfstr, 1792);
          char* nextptr = strrchr(pathbuf, '/') + 1;
          strncpy(nextptr, yytext + 1, 256);
          fname = strdup(pathbuf);
      }
      if((newbuf = fopen(fname, "r")) != NULL) { //ignore opening "
        YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
        *ylt = yylloc;
        dapush(locs, ylt);
        yylloc.first_line = yylloc.last_line = 1;
        yylloc.first_column = yylloc.last_column = 0;
        dapush(file2compile, fname);
        YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE);
        yy_push_state(INITIAL);
        yypush_buffer_state(ybs);
      } else {
        fprintf(stderr, "Invalid local file %s included! %s %d.%d-%d.%d\n", fname, locprint(yylloc));
      }
    }
    }
}
<INCLUDE,INCLUDENEXT>{
  [[:space:]]+[<\"] {/*"*/yyless(1);}
  [[:space:]]*\n {yy_pop_state(); yy_pop_state();if(!stmtover) fprintf(stderr, "Error: incomplete include %s %d.%d-%d.%d\n", locprint(yylloc));}
  . {fprintf(stderr, "INCLUDE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint(yylloc));}
}

<INCLUDENEXT>{
  ["<][^>\n]*[>"] {
    if(stmtover){
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yylloc.last_line, locprint(yylloc));
    } else {
      stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing >
      char pathbuf[2048];
      static const char* searchpath[] = {
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.1.0/include/",
        "/usr/local/include/",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/10.1.0/include-fixed/",
        "/usr/include/",
        };
      yy_pop_state();
      yy_pop_state();
      int i = 0;
      for(; i < 3 && strncmp(dapeek(file2compile), searchpath[i], strlen(searchpath[i])); ++i) ;
      ++i;
      for(; i < 4 /*sizeof searchpath*/; ++i) {
        FILE* newbuf;
        snprintf(pathbuf, 2048, "%s%s", searchpath[i], yytext + 1); //ignore opening
        if((newbuf = fopen(pathbuf, "r")) != NULL) {
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = yylloc;
          dapush(locs, ylt);
          yylloc.first_line = yylloc.last_line = 1;
          yylloc.first_column = yylloc.last_column = 0;
          dapush(file2compile, strdup(pathbuf));
          YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE);
          yy_push_state(INITIAL);
          yypush_buffer_state(ybs);
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
    yy_pop_state(); 
    yy_push_state(DEFINE2); 
    mdstrdly = strctor(malloc(2048), 0, 2048); 
    defname = strdup(yytext);
    insert(ctx->withindefines, strdup(yytext), NULL);
    }
  {IDENT}/[[:blank:]] {
    yy_pop_state(); 
    yy_push_state(DEFINE2); 
    mdstrdly = strctor(malloc(2048), 0, 2048); 
    yy_push_state(KILLBLANK);
    defname = strdup(yytext);
    insert(ctx->withindefines, strdup(yytext), NULL);
    }
  {IDENT}\( {
    yy_pop_state(); 
    yy_push_state(DEFARG); 
    yytext[yyleng - 1] = '\0'; 
    defname = strdup(yytext);
    md->args = dactor(8); 
    argeaten = 0;
    insert(ctx->withindefines, strdup(yytext), NULL);
    }
  {IDENT}\(/[[:blank:]] {
    yy_pop_state(); 
    yy_push_state(DEFARG); 
    yy_push_state(KILLBLANK); 
    yytext[yyleng - 1] = '\0';
    defname = strdup(yytext); 
    md->args = dactor(8); 
    argeaten = 0;
    insert(ctx->withindefines, strdup(yytext), NULL);
    }
  \n {yy_pop_state(); yy_pop_state();/*error state*/}
  . {fprintf(stderr, "DEFINE: Unexpected character encountered: %c %s %d.%d-%d.%d %s %d.%d-%d.%d\n", *yytext, locprint(yylloc), locprint(yylloc));}
}

<DEFARG>{
  {IDENT} {
    if(argeaten) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint(yylloc)); 
    argeaten = 1; /*new arg encountered*/ 
    dapush(md->args, strdup(yytext));
    /*probably should confirm no 2 args have the same name*/
    }
  {IDENT}/[[:blank:]] {
    if(argeaten) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint(yylloc)); 
    argeaten = 1;
    /*new arg encountered*/ yy_push_state(KILLBLANK); 
    dapush(md->args, strdup(yytext));
    /*probably should confirm no 2 args have the same name*/
    }
  \, {
    if(argeaten) 
      argeaten = 0; 
    else 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint(yylloc));
    }
  \,/[[:blank:]] {
    if(argeaten) 
      argeaten = 0; 
    else 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint(yylloc));
    yy_push_state(KILLBLANK);
    }
  \) {
    if(!argeaten && md->args->length != 0) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint(yylloc)); 
    /*last arg encountered*/
    yy_pop_state();
    yy_push_state(DEFINE2); 
    mdstrdly = strctor(malloc(2048), 0, 2048);
    }
  \)/[[:blank:]] {
    if(!argeaten && md->args->length != 0) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint(yylloc)); 
    /*last arg encountered*/
    yy_pop_state();
    yy_push_state(DEFINE2); 
    mdstrdly = strctor(malloc(2048), 0, 2048); 
    yy_push_state(KILLBLANK);
    }
  \n {yy_pop_state(); yy_pop_state();/*error state*/}
  . {fprintf(stderr, "DEFINE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint(yylloc));}
}

<DEFINE2>{
  [^\\/\n]+ {dscat(mdstrdly, yytext, yyleng);}
  "/" {dsccat(mdstrdly, '/');}
  \\ {dsccat(mdstrdly, '\\');}
  \n {
    yy_pop_state();
    yy_pop_state();
    dsccat(mdstrdly, 0);
    md->text = mdstrdly;
    insert(ctx->defines, defname, md);
    rmpair(ctx->withindefines, defname);
    }
}

<UNDEF>{
  {IDENT} {rmpairfr(ctx->defines, yytext);}
  {IDENT}/[[:blank:]] {rmpairfr(ctx->defines, yytext); yy_push_state(KILLBLANK);}
  \n {yy_pop_state(); yy_pop_state();/*error state if expr not over?*/}
  . {fprintf(stderr, "UNDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint(yylloc));}
}


<IFDEF>{
  {IDENT} {stmtover = 1; defname = strdup(yytext);}
  {IDENT}/[[:blank:]] {
    stmtover = 1;
    defname = strdup(yytext);
    yy_push_state(KILLBLANK);
    }
  \n {
    yy_pop_state();
    yy_pop_state(); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifdef! %s %d.%d-%d.%d\n", locprint(yylloc));
    } else {
      DYNARR* ds = ctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;
      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
          if(queryval(ctx->defines, defname)) {
            *rids = IFANDTRUE;
          } else {
            *rids = IFANDFALSE;
            yy_push_state(PPSKIP);
          }
          break;
        default:
          *rids= IFDEFDUMMY;
          yy_push_state(PPSKIP);
          break;
      }
      dapush(ds, rids);
    }
    }
  . {fprintf(stderr, "IFDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint(yylloc));}
}
<IFNDEF>{
  {IDENT} {stmtover = 1; defname = strdup(yytext);}
  {IDENT}/[[:blank:]] {
    stmtover = 1;
    defname = strdup(yytext);
    yy_push_state(KILLBLANK);
    }
  \n {
    yy_pop_state();
    yy_pop_state(); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifndef! %s %d.%d-%d.%d\n", locprint(yylloc));
    } else {
      DYNARR* ds = ctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;

      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
          if(queryval(ctx->defines, defname)) {
            *rids = IFANDFALSE;
            yy_push_state(PPSKIP);
          } else {
            *rids = IFANDTRUE;
          }
          break;
        default:
          *rids= IFDEFDUMMY;
          yy_push_state(PPSKIP);
          break;
      }
      dapush(ds, rids);
    }
    }
  . {fprintf(stderr, "IFNDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint(yylloc));}
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

      struct macrodef* md;
      if(!(md = search(ctx->defines, defname))) {
        fprintf(stderr, "Error: Malformed function-like macro call %s %d.%d-%d.%d\n", locprint(yylloc));
        //error state
      } else if(parg->length != md->args->length) {
        insert(ctx->withindefines, defname, NULL);
        if(md->args->length == 0 && parg->length == 1
           && *(((DYNSTR*) (parg->arr[0]))->strptr) == 0) {
          free(((DYNSTR*) (parg->arr[0]))->strptr);
          free(parg->arr[0]);
          free(parg);
          yy_pop_state();
          YY_BUFFER_STATE ybs = yy_create_buffer(fmemopen(defname, strlen(defname), "r"), YY_BUF_SIZE);//TODO: strlen inefficient
          yypush_buffer_state(ybs);
          char buf[256];
          snprintf(buf, 256, "%s", defname);
          yylloc.first_line = yylloc.last_line = 1;
          yylloc.first_column = yylloc.last_column = 0;
          dapush(file2compile, strdup(buf));
        } else {
          fprintf(stderr, "Error: the number of arguments passed to function-like macro is different than the number of parameters %s %d.%d-%d.%d\n", locprint(yylloc));
          //error state
        }
      } else {
        insert(ctx->withindefines, defname, NULL);
        DYNARR* argn = md->args;
        //make hash table with keys as param names, values as arguments, make 
        //special lexer mode to parse this into string literal, use this as new buffer
        //at the end of this mode, switch to string literal, parse in parent mode then
        defargs = htctor();
        char** prma = (char**) argn->arr;
        DYNSTR** arga = (DYNSTR**) parg->arr;
        for(int i = 0; i < parg->length; i++) {
          insert(defargs, prma[i], arga[i]);
        }
        dstrdly = strctor(malloc(2048), 0, 2048);
        yy_pop_state();
        yy_push_state(FINDREPLACE);
        YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
        *ylt = yylloc;
        dapush(locs, ylt);
        yylloc.first_line = yylloc.last_line = 1;
        yylloc.first_column = yylloc.last_column = 0;
        YY_BUFFER_STATE ybs = yy_create_buffer(fmemopen(md->text->strptr, md->text->lenstr, "r"), YY_BUF_SIZE);
        yypush_buffer_state(ybs);
      }
    }
    }

  {IDENT} {
    if(check_type(yytext, 2) != -1) {
      dscat(dstrdly, yytext, yyleng);
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
      dstrdly = strctor(malloc(2048), 0, 2048);
    }
    }
  "\0" {
    yypop_buffer_state();
    if ( !YY_CURRENT_BUFFER ) {
      yyterminate();
    } else {
      YYLTYPE* yl = dapop(locs);
      yylloc = *yl;
      free(yl);
      free(dapop(file2compile));
      if(ctx->argpp->length) {
        struct arginfo* argi = dapop(ctx->argpp);
        //fprintf(stderr, "Popping %s from stack\n", argi->defname);
        defname = argi->defname;
        free(argi);
      } else {
        //fprintf(stderr, "ERROR: macrostack state corrupted within macro call %s %d.%d-%d.%d\n", locprint(yylloc));
        //exit(-1);
        rmpair(ctx->withindefines, defname);
      }
    }
    }
  <<EOF>> {
    yypop_buffer_state();
    if ( !YY_CURRENT_BUFFER ) {
      yyterminate();
    } else {
      YYLTYPE* yl = dapop(locs);
      yylloc = *yl;
      free(yl);
      free(dapop(file2compile));
      if(ctx->argpp->length) {
        struct arginfo* argi = dapop(ctx->argpp);
        //fprintf(stderr, "Popping %s from stack\n", argi->defname);
        defname = argi->defname;
        free(argi);
      } else {
        fprintf(stderr, "ERROR: macrostack state corrupted within macro call %s %d.%d-%d.%d\n", locprint(yylloc));
        exit(-1);
      }
    }
    }
  . {fprintf(stderr, "Error: unexpected character in function macro call %s %d.%d-%d.%d\n", locprint(yylloc));}
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
      dscat(dstrdly, argy->strptr, argy->lenstr);
      dsccat(dstrdly, '"');
    } else {
      dscat(dstrdly, yytext, yyleng);
    }
    }
  {IDENT}/("/*"([^*]|\*)*"*/")*##[^#] {
    DYNSTR* argy = search(defargs, yytext);
    if(argy) {
      dscat(dstrdly, argy->strptr, argy->lenstr);
    } else {
      dscat(dstrdly, yytext, yyleng);
    }
    yy_push_state(KILLCONCAT);
  }
  {IDENT}/([[:blank:]]+|"/*"([^*]|\*)*"*/")+##[^#] {
    DYNSTR* argy = search(defargs, yytext);
    if(argy) {
      dscat(dstrdly, argy->strptr, argy->lenstr);
    } else {
      dscat(dstrdly, yytext, yyleng);
    }
    yy_push_state(KILLCONCAT);
    yy_push_state(KILLBLANK);
    }
  /*we do no concatenation with strings, character constants, or stringized things*/
  \"(\\.|[^\\"]|\/[[:blank:]]*\n)*\" {/*"*/
    dscat(dstrdly, yytext, yyleng);
    }
  '(\\.|[^\\'\n])+' {
    dscat(dstrdly, yytext, yyleng);
    }
  [^[:alnum:]_\'\"\n[:blank:]/#]+ {/*cntrl/delim expr*/
    dscat(dstrdly, yytext, yyleng);
    }
  [^[:alnum:]_\'\"\n[:blank:]/#]+/("/*"([^*]|\*)*"*/")*## {
    dscat(dstrdly, yytext, yyleng);
    yy_push_state(KILLCONCAT);
    }
  [^[:alnum:]_\'\"\n[:blank:]/#]+/([[:blank:]]+|"/*"([^*]|\*)*"*/")*## {
    dscat(dstrdly, yytext, yyleng);
    yy_push_state(KILLCONCAT);
    yy_push_state(KILLBLANK);
    }
  [[:blank:]]+ {dsccat(dstrdly, ' ');}
  [[:cntrl:][:print:]] {
    dsccat(dstrdly, *yytext);
    }
  [[:cntrl:][:print:]]/("/*"([^*]|\*)*"*/")*## {
    dsccat(dstrdly, *yytext);
    yy_push_state(KILLCONCAT);
    }
  [[:cntrl:][:print:]]/([[:blank:]]+|"/*"([^*]|\*)*"*/")*## {
    dsccat(dstrdly, *yytext);
    yy_push_state(KILLCONCAT);
    yy_push_state(KILLBLANK);
    }
  <<EOF>> {
    //TODO:free hashtable and stuff?
    yypop_buffer_state();
    yy_pop_state();
    YY_BUFFER_STATE ybs = yy_create_buffer(fmemopen(dstrdly->strptr, dstrdly->lenstr, "r"), YY_BUF_SIZE);
    free(dstrdly);
    yypush_buffer_state(ybs);
    char buf[256];
    snprintf(buf, 256, "%s", defname);
    yylloc.first_line = yylloc.last_line = 1;
    yylloc.first_column = yylloc.last_column = 0;
    dapush(file2compile, strdup(buf));
    rmpair(ctx->withindefines, defname);
    if(ctx->argpp->length) {
      struct arginfo* argi = dapop(ctx->argpp);
      //fprintf(stderr, "Popping %s from stack\n", argi->defname);
      defname = argi->defname;
      if(argi->argi) {
        dstrdly = argi->argi;
        paren_depth = argi->pdepth;
        parg = argi->parg;
      }
      free(argi);
    }
    }
}

<CHECKDEFINED>{
  [[:blank:]]*\) {yy_pop_state();}
  {IDENT}/[[:blank:]]*\) {
    zzlval.unum = queryval(ctx->defines, yytext);
    return UNSIGNED_LITERAL;
    }
}

<CHECKDEFINED2>{
  {IDENT} {
    yy_pop_state();
    zzlval.unum = queryval(ctx->defines, yytext);
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

((?i:"infinity")|(?i:"inf")) {yylval.dbl = 0x7f800000; return FLOAT_LITERAL;}
(?i:"nan") {yylval.dbl = 0x7fffffff; return FLOAT_LITERAL;}

"__FILE__" {char* fstr = dapeek(file2compile); unput('"'); UNPUTSTR(fstr); unput('"');}
"__LINE__" {char linebuf[16]; sprintf(linebuf, "%d", yylloc.last_line); unput('"'); UNPUTSTR(linebuf); unput('"');}
"__DATE__" {time_t tim = time(NULL); struct tm* tms = localtime(&tim); char datebuf[14]; strftime(datebuf, 14, "\"%b %e %Y\"", tms); UNPUTSTR(datebuf);}
"__TIME__" {time_t tim = time(NULL); struct tm* tms = localtime(&tim); char timebuf[11]; strftime(timebuf, 11, "\"%T\"",tms); UNPUTSTR(timebuf);}
"__func__" {char* namestr = ctx->func->name; unput('"'); UNPUTSTR(namestr); unput('"');}

<WITHINIF>{
  "defined"[[:blank:]]*"("[[:blank:]]* {yy_push_state(CHECKDEFINED);}
  "defined"[[:blank:]]*/{IDENT} {yy_push_state(CHECKDEFINED2);}
  {IDENT} {
    char* ylstr = strdup(yytext);
    int mt = check_type(ylstr, 0);
    switch(mt) {
      default:
        zzlval.unum = 0;
        return UNSIGNED_LITERAL;
      case -1:
        break;
    } 
  }
  0[bB]{BIN}+{INTSIZE}? {zzlval.unum = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                         return UNSIGNED_LITERAL;}
  0{OCT}+{INTSIZE}? {zzlval.unum = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                     return UNSIGNED_LITERAL;}
  [[:digit:]]+{INTSIZE}?  {zzlval.unum = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                           if(strchr(yytext,'u') || strchr(yytext,'U')) return UNSIGNED_LITERAL; return INTEGER_LITERAL;}
  0[xX][[:xdigit:]]+{INTSIZE}? {zzlval.unum = strtoul(yytext,NULL,16);
                                return UNSIGNED_LITERAL;}
  L?\' {yy_push_state(ZCHARLIT);}
}

{IDENT} {
  char* ylstr = strdup(yytext);
  int mt = check_type(ylstr, 1);
  switch(mt) {
    case SYMBOL: case TYPE_NAME:
      yylval.str = ylstr;
      return mt;
  } 
}

0[bB]{BIN}+{INTSIZE}? {yylval.unum = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                       return UNSIGNED_LITERAL;}
0{OCT}+{INTSIZE}? {yylval.unum = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                   return UNSIGNED_LITERAL;}
[[:digit:]]+{INTSIZE}?  {yylval.snum = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                         if(strchr(yytext,'u') || strchr(yytext,'U')) return UNSIGNED_LITERAL; return INTEGER_LITERAL;}
0[xX][[:xdigit:]]+{INTSIZE}? {yylval.unum = strtoul(yytext,NULL,16);
                              return UNSIGNED_LITERAL;}
L?\' {yy_push_state(CHARLIT);}

[[:digit:]]+{EXP}{FLOATSIZE}? {sscanf(yytext, "%lf", &yylval.dbl);return FLOAT_LITERAL;}
[[:digit:]]*"."?[[:digit:]]+({EXP})?{FLOATSIZE}? {sscanf(yytext, "%lf", &yylval.dbl);return FLOAT_LITERAL;}
[[:digit:]]+"."?[[:digit:]]*({EXP})?{FLOATSIZE}? {sscanf(yytext, "%lf", &yylval.dbl);return FLOAT_LITERAL;}

<ZCHARLIT>{
  \' {
    fprintf(stderr, "Error: 0 length character literal %s %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    ZGOC('?');
  	}
  [\n\v] {
    fprintf(stderr, "Error: character literal terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint(yylloc));
    yy_pop_state();
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
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    }
    ZGOC((char) result);
    }
  \\0x[[:xdigit:]]{1,2}\' {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    ZGOC((char) result);
    }
  \\.\' {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    ZGOC(yytext[1]);
    }
  [^\\\'\n\v]\' {
    ZGOC(yytext[0]);
    }
  [^\']{2,}\' {
    fprintf(stderr, "Error: character literal too long %s %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    ZGOC(yytext[1]);
    }
}

<CHARLIT>{
  \' {
    fprintf(stderr, "Error: 0 length character literal %s %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    GOC('?');
  	}
  [\n\v] {
    fprintf(stderr, "Error: character literal terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint(yylloc));
    yy_pop_state();
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
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    }
    GOC((char) result);
    }
  \\0x[[:xdigit:]]{1,2}\' {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    GOC((char) result);
    }
  \\.\' {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    GOC(yytext[1]);
    }
  [^\\\'\n\v]\' {
    GOC(yytext[0]);
    }
  [^\']{2,}\' {
    fprintf(stderr, "Error: character literal too long %s %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    GOC(yytext[1]);
    }
}

L?\" {/*"*/yy_push_state(STRINGLIT); strcur = strctor(malloc(2048), 0, 2048);}
<STRINGLIT>{
  \" {/*"*/
    dsccat(strcur, 0);
  	yylval.dstr = strcur; 
  	yy_pop_state(); 
  	return STRING_LITERAL;
  	}
  [\n\v] {
    strdtor(strcur);
    free(strconst); 
    fprintf(stderr, "Error: String terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint(yylloc));
    yy_pop_state();
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
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    }
    dsccat(strcur, result);
    }
  \\0x[[:xdigit:]]{1,2} {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    dsccat(strcur, result);
    }
  \\. {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal %s %d.%d-%d.%d\n", yytext, locprint(yylloc));
    dsccat(strcur, yytext[1]);
  }
  [^\\\"\v]+ {/*"*/
    dscat(strcur, yytext, yyleng);
  }
}
<INITIAL,WITHINIF>[[:blank:]]+ {/*Whitespace, ignored*/}
<WITHINIF>\n {
  yy_pop_state();
  yy_push_state(ENDWITHINIF);
  unput('\n');
  return 0;
}
<ENDWITHINIF>\n {
  yy_pop_state(); 
  while(foldconst(&ctx->ifexpr)) ;
  enum ifdefstate* rids;
  switch(ctx->ifexpr->type) {
    case INT: case UINT:
      if(ctx->ifexpr->intconst != 0) {
        rids = malloc(sizeof(enum ifdefstate));
        *rids = IFANDTRUE;
        break;
      }
      //fall through
    case NOP:
      rids = malloc(sizeof(enum ifdefstate));
      *rids = IFANDFALSE;
      yy_push_state(PPSKIP);
      break;
    default:
      yy_pop_state();
      fprintf(stderr, "ERROR: subsidiary parser reduced if or elif into non-rectifiable expression %s %d.%d-%d.%d\n", locprint(yylloc));
      exit(-1);
  }
  dapush(ctx->definestack, rids);
}
[[:space:]] {/*Whitespace, ignored*/}

<<EOF>> {
  yypop_buffer_state();
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state();
    stmtover = 1;
    YYLTYPE* yl = dapop(locs);
    yylloc = *yl;
    free(yl);
    char* ismac = dapop(file2compile);
    rmpair(ctx->withindefines, ismac);
    //rmpair is a no-op if not in hash
  }
}

"\0" {//same as EOF
  yypop_buffer_state();
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state();
    stmtover = 1;
    YYLTYPE* yl = dapop(locs);
    yylloc = *yl;
    free(yl);
    char* ismac = dapop(file2compile);
    rmpair(ctx->withindefines, ismac);
    //rmpair is a no-op if not in hash
  }
}

<*>. {fprintf(stderr, "Unexpected character encountered: %c %d %s %d.%d-%d.%d\n", *yytext, *yytext, locprint(yylloc));}
<*>\n {fprintf(stderr, "Unexpected newline encountered:  %s %d.%d-%d.%d\n", locprint(yylloc));}
%%
int check_type(char* symb, char frominitial) {
  struct macrodef* macdef = search(ctx->defines, symb);
  if(macdef && !queryval(ctx->withindefines, symb)) {
    char* oldname = defname;
    defname = symb;
    switch(frominitial) {
      case 0:
        yy_push_state(WITHINIF);
        break;
      case 1:
        yy_push_state(INITIAL);
        break;
      case 2:
        //don't push callmacro
        break;
    }
    if(macdef->args) {
      char c;
      while(1) {
        c = input();
        ++yylloc.last_column;
        switch(c) {
          case '\n': 
            yylloc.last_column = 0;
            ++yylloc.last_line;
          case ' ': case '\t': case '\v':
          	break;
          case '(':
          	goto whiledone;
          default:
            --yylloc.last_column;
            unput(c);
            goto nofcall;

        }
      }
      whiledone:
      yy_push_state(CALLMACRO);
      if(frominitial == 2) {
        struct arginfo* argi;
        argi = malloc(sizeof(struct arginfo));
        argi->argi = dstrdly;
        argi->pdepth = paren_depth;
        argi->defname = oldname;
        argi->parg = parg;
        dapush(ctx->argpp, argi);
        //fprintf(stderr, "Pushing %s, fcall macro, to stack\n", argi->defname);
      }
      paren_depth = 0;
      dstrdly = strctor(malloc(2048), 0, 2048);
      parg = dactor(64); 

    } else {
      char* buf = malloc(256);
      snprintf(buf, 256, "%s", symb);
      dapush(file2compile, buf);
      YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
      *ylt = yylloc;
      dapush(locs, ylt);
      yylloc.first_line = yylloc.last_line = 1;
      yylloc.first_column = yylloc.last_column = 0;
      YY_BUFFER_STATE yms = yy_create_buffer(fmemopen(macdef->text->strptr, macdef->text->lenstr, "r"), 
                                             YY_BUF_SIZE);
      yypush_buffer_state(yms);
      insert(ctx->withindefines, strdup(symb), NULL);
      if(frominitial == 2) {
        struct arginfo* argi = calloc(1, sizeof(struct arginfo));
        argi->defname = oldname;
        dapush(ctx->argpp, argi);
        //fprintf(stderr, "Pushing %s to stack\n", argi->defname);
      }
    }
    return -1;
  }
  nofcall:
  if(scopequeryval(ctx, M_TYPEDEF, symb)) {
    return TYPE_NAME;
  }
  return SYMBOL;
}

