BIN [0-1]
OCT [0-7]
IDENT [[:alpha:]_][[:alnum:]_]*
EXP [Ee][+-]?[[:digit:]]+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*
SKIPNEWL \\[[:blank:]]*\n
SKIPNEWALL \\+[[:blank:]]*[^[:blank:]]
SCOMMENT \/\/([^\\\n]|{SKIPNEWALL})*
MCOMMENT "/*"("*"+[^*/]|[^*])*"*"+"/"
BLANKC ([[:blank:]]|{MCOMMENT})*
PPSTART [[:blank:]]*#{BLANKC}
MSTRING \"(\\.|[^\\"]|{SKIPNEWL})*\"
%{
//TODO: computed include?

#include <math.h>
#include <time.h>
#include <assert.h>
#include <libgen.h>
#include "joecc.tab.h"
#include "compintern.h"
#include "treeduce.h"
#define YYLTYPE struct yyltype

#define YY_USER_ACTION \
  yylloc->first_line = yylloc->last_line; \
  yylloc->first_column = yylloc->last_column; \
  for(int i = 0; i < yyleng; i++) { \
      if(yytext[i] == '\n') { \
          yylloc->last_line++; \
          yylloc->last_column = 0; \
      } else { \
          yylloc->last_column++; \
      } \
  } \

#undef unput
#define unput(c, yyscanner) yyunput(c, ((struct yyguts_t*) yyscanner)->yytext_ptr, yyscanner) //fix your damn program flex
#define lctx ((struct lexctx*) yyget_extra(yyscanner))

int zzparse(yyscan_t scanner);
int check_type(char* symb, char frominitial, YYLTYPE* yltg, yyscan_t yyscanner);
void yypush_stringbuffer(char* str, int length, const char* macname, YY_BUFFER_STATE ybs, yyscan_t yyscanner);

struct arginfo {
  DYNSTR* argi;
  int pdepth;
  char* defname;
  DYNARR* parg;
};

//get one character
#define GOC(c) yylval_param->unum = c; yy_pop_state(yyscanner); return UNSIGNED_LITERAL
%}
%option stack
%option never-interactive
%option reentrant
%option bison-bridge
%option bison-locations
%option debug

%option warn
%option nodefault
%option noyy_scan_string
%option noyywrap
%option noyymore

%x PPSKIP KILLBLANK KILLUNTIL
%x INCLUDE INCLUDENEXT
%x IFDEF DEFINE UNDEF DEFARG DEFINE2
%x ERROR WARNING LINE
%x STRINGLIT CHARLIT
%x CALLMACRO FINDREPLACE
%x WITHINIF CHECKDEFINED CHECKDEFINED2 HASINCLUDENEXT

%%
<CALLMACRO,WITHINIF,INITIAL>{
  "__FILE__" {
    char filebuf[300];
    int i = lctx->withindefines->keys;
    YYLTYPE* ylt = i ? daget(lctx->ls->locs, lctx->ls->locs->length - i) : yylloc;
    int bsize = sprintf(filebuf, "\"%s\"", ylt->filename);
    int t = yy_top_state(yyscanner);
    if(t == CALLMACRO) {
      dscat(lctx->ls->dstrdly, filebuf, bsize);
    } else {
      yypush_stringbuffer(filebuf, bsize, "__FILE__", YY_CURRENT_BUFFER, yyscanner);
      yy_push_state(t, yyscanner);
    }
  }
  "__LINE__" {
    char linebuf[16]; 
    int i = lctx->withindefines->keys;
    YYLTYPE* ylt = i ? daget(lctx->ls->locs, lctx->ls->locs->length - i) : yylloc;
    int bsize = sprintf(linebuf, "%d", ylt->last_line);
    int t = yy_top_state(yyscanner);
    if(t == CALLMACRO) {
      dscat(lctx->ls->dstrdly, linebuf, bsize);
    } else {
      yypush_stringbuffer(linebuf, bsize, "__LINE__", YY_CURRENT_BUFFER, yyscanner);
      yy_push_state(t, yyscanner);
    }
  }
  "__DATE__" {
    time_t tim = time(NULL); 
    struct tm tms;
    localtime_r(&tim, &tms);
    char datebuf[20]; 
    size_t datesize = strftime(datebuf, 16, "\"%b %e %Y\"", &tms);
    int t = yy_top_state(yyscanner);
    if(t == CALLMACRO) {
      dscat(lctx->ls->dstrdly, datebuf, datesize);
    } else {
      yypush_stringbuffer(datebuf, datesize, "__DATE__", YY_CURRENT_BUFFER, yyscanner);
      yy_push_state(t, yyscanner);
    }
  }
  "__TIME__" {
    time_t tim = time(NULL); 
    struct tm tms;
    localtime_r(&tim, &tms); 
    char timebuf[16]; 
    size_t timesize = strftime(timebuf, 13, "\"%T\"", &tms);
    int t = yy_top_state(yyscanner);
    if(t == CALLMACRO) {
      dscat(lctx->ls->dstrdly, timebuf, timesize);
    } else {
      yypush_stringbuffer(timebuf, timesize, "__TIME__", YY_CURRENT_BUFFER, yyscanner);
      yy_push_state(t, yyscanner);
    }
  }
  "__func__" {
    char funcbuf[300];
    int bsize = sprintf(funcbuf, "\"%s\"", lctx->func->name);
    int t = yy_top_state(yyscanner);
    if(t == CALLMACRO) {
      dscat(lctx->ls->dstrdly, funcbuf, bsize);
    } else {
      yypush_stringbuffer(funcbuf, bsize, "__func__", YY_CURRENT_BUFFER, yyscanner);
      yy_push_state(t, yyscanner);
    }
  }
}

<KILLBLANK>{
  ([[:blank:]]+|{SKIPNEWL})* {yy_pop_state(yyscanner);}
}

<KILLUNTIL>{
  ([^/\n\\]|{SKIPNEWALL}|"/"[^/\n*]|{MCOMMENT})*({SCOMMENT}|"/")?\n {yy_pop_state(yyscanner);}
}

<INITIAL,INCLUDE,DEFINE,UNDEF,DEFARG,DEFINE2,IFDEF,CALLMACRO,KILLBLANK,WITHINIF>{
  {MCOMMENT}+|{SCOMMENT} {}
}

^{PPSTART}include{BLANKC} {yy_push_state(INCLUDE, yyscanner); lctx->ls->stmtover = 0;}
^{PPSTART}include_next{BLANKC} {yy_push_state(INCLUDENEXT, yyscanner); lctx->ls->stmtover = 0;}
^{PPSTART}define{BLANKC} {yy_push_state(DEFINE, yyscanner); lctx->ls->md = calloc(1, sizeof(struct macrodef));}
^{PPSTART}undef{BLANKC} {yy_push_state(UNDEF, yyscanner);}

^{PPSTART}line{BLANKC} {yy_push_state(LINE, yyscanner); lctx->ls->stmtover = 0;}
^{PPSTART}warning{BLANKC} {yy_push_state(WARNING, yyscanner);}
^{PPSTART}error{BLANKC} {yy_push_state(ERROR, yyscanner);}

<INITIAL,PPSKIP>{
  ^{PPSTART}ifdef{BLANKC} {yy_push_state(IFDEF, yyscanner); lctx->ls->argeaten = 0; lctx->ls->stmtover = 0;}
  ^{PPSTART}ifndef{BLANKC} {yy_push_state(IFDEF, yyscanner); lctx->ls->argeaten = 1;}
  ^{PPSTART}if{BLANKC} {
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
  ^{PPSTART}else{BLANKC} {
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
        yy_push_state(KILLUNTIL, yyscanner);
        break;
      default:
        fprintf(stderr, "Error: Unexpected #else %s %d.%d-%d.%d\n", locprint2(yylloc));
        yy_push_state(KILLUNTIL, yyscanner);
      case IFDEFDUMMY:
        break;
    }
    }
  ^{PPSTART}elif{BLANKC} {
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
  ^{PPSTART}endif{BLANKC} {
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
}

<PPSKIP>{
    /*nasty but really greedy regular expression to match even many kinds of preprocessor statements if we are skipping*/
  ([^\n\/#]|\n[[:space:]]*({PPSTART}(define|include|include_next|undef|warning|error|line)|[^#\/[:space:]])|{MCOMMENT}|{SCOMMENT}|"/"[^*/]|\n[[:blank:]]*[^#[:blank:]])+ {}
  [\n#] {}
}

^{PPSTART} {yy_push_state(KILLUNTIL, yyscanner); fprintf(stderr, "Error: unkown preprocessor keyword encountered on line %d! %s %d.%d-%d.%d\n", yylloc->last_line, locprint2(yylloc));}

<WARNING>{
  {MSTRING} {
    /*WARNING, ERROR, only support single string const*/
    yy_pop_state(yyscanner); 
    yy_push_state(KILLUNTIL, yyscanner); 
    yytext[yyleng - 1] = '\0'; 
    fprintf(stderr, "WARNING: %s\n", yytext + 1);
  }
}
<ERROR>{
  {MSTRING} {/*"*/
    yy_pop_state(yyscanner); 
    yy_push_state(KILLUNTIL, yyscanner); 
    yytext[yyleng - 1] = '\0'; 
    fprintf(stderr, "ERROR: %s\n", yytext + 1);
    exit(0);
  }
}

<LINE>{
  [0-9]+ {
    int i;
    sscanf(yytext, "%d", &i);
    yylloc->first_line = yylloc->last_line = i;
  }

  {MSTRING}? {
    free(yylloc->filename);
    yytext[yyleng - 1] = '\0';
    yylloc->filename = strdup(yytext + 1);
  }

  ({BLANKC}|{SKIPNEWL})* {}

  \n {
    yy_pop_state(yyscanner);
  }
}

<INCLUDE>{
  "<"[^>\n]*">" {
    if(lctx->ls->stmtover){
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yylloc->last_line, locprint2(yylloc));
    } else {
      lctx->ls->stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing >
      char pathbuf[256];
      yy_pop_state(yyscanner);
      int i;
      for(i = 0; i < includepath->length; i++) {
        FILE* newbuf;
        snprintf(pathbuf, 256, "%s/%s", (char*) daget(includepath, i), yytext + 1); //ignore opening <
        if((newbuf = fopen(pathbuf, "r")) != NULL) {
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = *yylloc;
          dapush(lctx->ls->locs, ylt);
          yylloc->first_line = yylloc->last_line = 1;
          yylloc->first_column = yylloc->last_column = 0;
          yylloc->filename = strdup(pathbuf);
          YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE, yyscanner);
          yy_push_state(INITIAL, yyscanner);
          yypush_buffer_state(ybs, yyscanner);
          break;
        }
      } 
      if(i >= includepath->length){
        fprintf(stderr, "Invalid system file %s included!\n", yytext + 1);
      }
    }
    }
  \"[^\"\n]*\" {/*"*/
    if(lctx->ls->stmtover) {
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yylloc->last_line, locprint2(yylloc));
    } else {
      lctx->ls->stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing "
      yy_pop_state(yyscanner);
      FILE* newbuf;
      char* pfstr = yylloc->filename;
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
        dapush(lctx->ls->locs, ylt);
        yylloc->first_line = yylloc->last_line = 1;
        yylloc->first_column = yylloc->last_column = 0;
        yylloc->filename = fname;
        YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE, yyscanner);
        yy_push_state(INITIAL, yyscanner);
        yypush_buffer_state(ybs, yyscanner);
      } else {
        fprintf(stderr, "Invalid local file %s included! %s %d.%d-%d.%d\n", fname, locprint2(yylloc));
        free(fname);
      }
    }
    }
}
<INCLUDE,INCLUDENEXT>{
    {BLANKC}\r?\n {yy_pop_state(yyscanner);if(!lctx->ls->stmtover) fprintf(stderr, "Error: incomplete include %s %d.%d-%d.%d\n", locprint2(yylloc));}
  . {fprintf(stderr, "INCLUDE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<INCLUDENEXT>{
  ["<][^>\n]*[>"] {
    if(lctx->ls->stmtover){
      fprintf(stderr, "Error: tried to include multiple files with single directive on line %d! %s %d.%d-%d.%d\n", yylloc->last_line, locprint2(yylloc));
    } else {
      lctx->ls->stmtover = 1;
      yytext[yyleng - 1] = '\0'; //ignore closing >
      char pathbuf[256];
      yy_pop_state(yyscanner);
      int i = 0;
      char* strpardir = strdup(yylloc->filename);
      char* pardir = dirname(strpardir);
      for(; i < includepath->length; ++i) {
        if(!strcmp(pardir, daget(includepath, i))) break;
      }
      free(strpardir);
      int j = i;
      ++i;
      for(; i < includepath->length; ++i) {
        FILE* newbuf;
        snprintf(pathbuf, 256, "%s/%s", (char*) daget(includepath, i), yytext + 1); //ignore opening
        if((newbuf = fopen(pathbuf, "r")) != NULL) {
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = *yylloc;
          dapush(lctx->ls->locs, ylt);
          yylloc->first_line = yylloc->last_line = 1;
          yylloc->first_column = yylloc->last_column = 0;
          yylloc->filename = strdup(pathbuf);
          YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE, yyscanner);
          yy_push_state(INITIAL, yyscanner);
          yypush_buffer_state(ybs, yyscanner);
          break;
        }
      } 
      if(i >= includepath->length){
        fprintf(stderr, "Invalid system file %s included next from directory %s!\n", yytext + 1, (char*) daget(includepath, j));
      }
    }
    }
}


<DEFINE>{
  {IDENT}|["][^"\n]*["] {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFINE2, yyscanner); 
    lctx->ls->mdstrdly = strctor(malloc(256), 0, 256);
    lctx->ls->defname = strdup(yytext);
    insert(lctx->withindefines, yytext, NULL);
    }
  {IDENT}|["][^"\n]*["]/[[:blank:]] {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFINE2, yyscanner); 
    lctx->ls->mdstrdly = strctor(malloc(256), 0, 256); 
    yy_push_state(KILLBLANK, yyscanner);
    lctx->ls->defname = strdup(yytext);
    insert(lctx->withindefines, yytext, NULL);
    }
  {IDENT}\( {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFARG, yyscanner); 
    yytext[yyleng - 1] = '\0'; 
    lctx->ls->defname = strdup(yytext);
    lctx->ls->md->args = dactor(8);
    lctx->ls->argeaten = 0;
    insert(lctx->withindefines, yytext, NULL);
    }
  {IDENT}\(/[[:blank:]] {
    yy_pop_state(yyscanner); 
    yy_push_state(DEFARG, yyscanner); 
    yy_push_state(KILLBLANK, yyscanner); 
    yytext[yyleng - 1] = '\0';
    lctx->ls->defname = strdup(yytext); 
    lctx->ls->md->args = dactor(8); 
    lctx->ls->argeaten = 0;
    insert(lctx->withindefines, yytext, NULL);
    }
  \r?\n {yy_pop_state(yyscanner);/*error state*/}
  . {fprintf(stderr, "DEFINE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<DEFARG>{
  {IDENT} {
    if(lctx->ls->argeaten) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    lctx->ls->argeaten = 1; /*new arg encountered*/ 
    dapush(lctx->ls->md->args, strdup(yytext));
    /*probably should confirm no 2 args have the same name*/
    }
  {IDENT}/[[:blank:]] {
    if(lctx->ls->argeaten) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    lctx->ls->argeaten = 1; /*new arg encountered*/
    yy_push_state(KILLBLANK, yyscanner); 
    dapush(lctx->ls->md->args, strdup(yytext));
    /*probably should confirm no 2 args have the same name*/
    }
  "..."[[:blank:]]*\) {
    dapush(lctx->ls->md->args, strdup("__VA_ARGS__"));
    yy_pop_state(yyscanner);
    yy_push_state(DEFINE2, yyscanner); 
    lctx->ls->mdstrdly = strctor(malloc(256), 0, 256);
    }
  \,[[:blank:]]* {
    if(lctx->ls->argeaten) 
      lctx->ls->argeaten = 0; 
    else 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc));
    }
  \)[[:blank:]]* {
    if(!lctx->ls->argeaten && lctx->ls->md->args->length != 0) 
      fprintf(stderr, "Error: unexpected macro argument %s %d.%d-%d.%d\n", locprint2(yylloc)); 
    /*last arg encountered*/
    yy_pop_state(yyscanner);
    yy_push_state(DEFINE2, yyscanner); 
    lctx->ls->mdstrdly = strctor(malloc(256), 0, 256);
    }
  \r?\n {yy_pop_state(yyscanner); /*error state*/}
  . {fprintf(stderr, "DEFINE: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<DEFINE2>{
  ([^/\n]|"/"[^*/]|{SKIPNEWL})+ {dscat(lctx->ls->mdstrdly, yytext, yyleng);}
  \r?\n {
    yy_pop_state(yyscanner);
    dsccat(lctx->ls->mdstrdly, 0);
    struct macrodef* isinplace;
    if((isinplace = bigsearch(lctx->defines, lctx->ls->defname, 0))) {
      if(strcmp(isinplace->text->strptr, lctx->ls->mdstrdly->strptr)) {
        dsws(isinplace->text);
        dsws(lctx->ls->mdstrdly);
        assert(!strcmp(isinplace->text->strptr, lctx->ls->mdstrdly->strptr));
      }
      freemd(isinplace);
    }
    lctx->ls->md->text = lctx->ls->mdstrdly;
    biginsert(lctx->defines, lctx->ls->defname, lctx->ls->md);
    rmpair(lctx->withindefines, lctx->ls->defname);
    free(lctx->ls->defname);
    lctx->ls->defname = NULL;
    }
}

<UNDEF>{
  {IDENT}|["][^"\n]*["] {
    bigrmpaircfr(lctx->defines, yytext, (void(*)(void*)) freemd, 0);}
  {IDENT}|["][^"\n]*["]/[[:blank:]] {
    bigrmpaircfr(lctx->defines, yytext, (void(*)(void*)) freemd, 0); yy_push_state(KILLBLANK, yyscanner);}
  \r?\n {yy_pop_state(yyscanner);/*error state if expr not over?*/}
  . {fprintf(stderr, "UNDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}


<IFDEF>{
  {IDENT}|["][^"\n]*["] {
    lctx->ls->stmtover = 1; lctx->ls->defname = strdup(yytext);}
  {IDENT}|["][^"\n]*["]/[[:blank:]] {
    lctx->ls->stmtover = 1;
    lctx->ls->defname = strdup(yytext);
    yy_push_state(KILLBLANK, yyscanner);
    }
  \r?\n {
    yy_pop_state(yyscanner);
    if(!lctx->ls->stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifdef! %s %d.%d-%d.%d\n", locprint2(yylloc));
    } else {
      DYNARR* ds = lctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length ? *(enum ifdefstate*) dapeek(ds) : IFANDTRUE;
      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
#if PPDEBUG
              fprintf(stderr, "Value of identifier %s is %d at %s %d.%d-%d.%d\n", lctx->ls->defname, bigqueryval(lctx->defines, lctx->ls->defname), locprint2(yylloc));
#endif
          if(lctx->ls->argeaten ^ bigqueryval(lctx->defines, lctx->ls->defname)) {
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
    free(lctx->ls->defname);
    lctx->ls->defname = NULL;
    }
  . {fprintf(stderr, "IFDEF: Unexpected character encountered: %c %s %d.%d-%d.%d\n", *yytext, locprint2(yylloc));}
}

<CALLMACRO>{
  \( {
    ++lctx->ls->paren_depth;
    dsccat(lctx->ls->dstrdly, '(');
    }
  [[:space:]]*\) {
    /*if 0 arguments, don't add new argument here*/
    if(lctx->ls->paren_depth) {
      --lctx->ls->paren_depth;
      dsccat(lctx->ls->dstrdly, ')');
    } else {
      dapush(lctx->ls->parg, lctx->ls->dstrdly);

      struct macrodef* mdl;
      if(!(mdl = bigsearch(lctx->defines, lctx->ls->defname, 0))) {
        fprintf(stderr, "Error: Malformed function-like macro call %s %d.%d-%d.%d\n", locprint2(yylloc));
        //error state
        yy_pop_state(yyscanner);
      } else {
        if(mdl->args->length == 0 && lctx->ls->parg->length == 1) {
          strdtor(dapop(lctx->ls->parg));
        }
        if(lctx->ls->parg->length != mdl->args->length) {
          fprintf(stderr, "Error: the number of arguments passed to function-like macro is different than the number of parameters %s %d.%d-%d.%d\n", locprint2(yylloc));
          //error state
        } else {
          insert(lctx->withindefines, lctx->ls->defname, NULL);
          DYNARR* argn = mdl->args;
          lctx->ls->defargs = htctor();
          char** prma = (char**) argn->arr;
          DYNSTR** arga = (DYNSTR**) lctx->ls->parg->arr;
          for(int i = 0; i < lctx->ls->parg->length; i++) {
            insert(lctx->ls->defargs, prma[i], arga[i]);
          }
          dadtor(lctx->ls->parg);
          lctx->ls->dstrdly = strctor(malloc(256), 0, 256);
          yy_pop_state(yyscanner);
          yy_push_state(FINDREPLACE, yyscanner);
          yypush_stringbuffer(mdl->text->strptr, mdl->text->lenstr, NULL, YY_CURRENT_BUFFER, yyscanner);
        }
      }
    }
    }

  {IDENT} {
    char* dupdstr = strdup(yytext);
    if(check_type(dupdstr, 2, yylloc, yyscanner) >= 0) {
      dscat(lctx->ls->dstrdly, yytext, yyleng);
      free(dupdstr);
    }
    }
  (0[bB]{BIN}+|0{OCT}+|[[:digit:]]+|0[xX][[:xdigit:]]+){INTSIZE}? {
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  ([[:digit:]]+|[[:digit:]]*"."?|[[:digit:]]+"."?)[[:digit:]]*({EXP})?{FLOATSIZE}? {
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  [^\(\)\",[:alnum:]_\0]*[^[:space:]\(\)\",[:alnum:]_\0\/\\] {/*"*/
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  [\/\\] {
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  \"(\\.|[^\\"]|{SKIPNEWL})*\" {/*"*/
  	//this is here to make sure that parenthesis depth isn't changed within strings
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  \'(\\.|[^\\']|{SKIPNEWL})*\' {
  	//this is here to make sure that parenthesis depth isn't changed within char literals
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  [[:space:]]+ {
    dsccat(lctx->ls->dstrdly, ' ');
    }
  [[:space:]]*,[[:space:]]* {
    struct macrodef* mdl = bigsearch(lctx->defines, lctx->ls->defname, 0);
    if(lctx->ls->paren_depth || !strcmp(daget(mdl->args, lctx->ls->parg->length), "__VA_ARGS__")) {
      char tmpstr[3];
      int tmpstrl = 0;
      if(yytext[0] == ' ')
        tmpstr[tmpstrl++] = ' ';
      tmpstr[tmpstrl++] = ',';
      if(yytext[yyleng - 1] == ' ')
        tmpstr[tmpstrl++] = ' ';
      dscat(lctx->ls->dstrdly, tmpstr, tmpstrl);
    } else {
      dapush(lctx->ls->parg, lctx->ls->dstrdly);
      lctx->ls->dstrdly = strctor(malloc(256), 0, 256);
    }
    }
  \0 {
    if(YY_CURRENT_BUFFER->yy_input_file) fclose(YY_CURRENT_BUFFER->yy_input_file);
    yypop_buffer_state(yyscanner);
    if ( !YY_CURRENT_BUFFER ) {
      yyterminate();
    } else {
      yy_pop_state(yyscanner);
      YYLTYPE* ylt = dapop(lctx->ls->locs);
      free(yylloc->filename);
      *yylloc = *ylt;
      free(ylt);
      struct arginfo* ai = dapop(lctx->ls->argpp);
      rmpair(lctx->withindefines, lctx->ls->defname);
      free(lctx->ls->defname);
      lctx->ls->defname = ai->defname;
      free(ai);
    }
    }
  <<EOF>> {
    if(YY_CURRENT_BUFFER->yy_input_file) fclose(YY_CURRENT_BUFFER->yy_input_file);
    yypop_buffer_state(yyscanner);
    if ( !YY_CURRENT_BUFFER ) {
      yyterminate();
    } else {
      yy_pop_state(yyscanner);
      YYLTYPE* ylt = dapop(lctx->ls->locs);
      free(yylloc->filename);
      *yylloc = *ylt;
      free(ylt);
      struct arginfo* ai = dapop(lctx->ls->argpp);
      rmpair(lctx->withindefines, lctx->ls->defname);
      free(lctx->ls->defname);
      lctx->ls->defname = ai->defname;
      free(ai);
    }
    }
  . {fprintf(stderr, "Error: unexpected character in function macro call %s %d.%d-%d.%d\n", locprint2(yylloc));}
}

<FINDREPLACE>{
  {IDENT} {
    DYNSTR* argy = search(lctx->ls->defargs, yytext);
    if(argy) {
      dscat(lctx->ls->dstrdly, argy->strptr, argy->lenstr);
    } else {
      dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
    }
  #{IDENT} {
    DYNSTR* argy = search(lctx->ls->defargs, yytext + 1);
    if(argy) {
      dsccat(lctx->ls->dstrdly, '"');
      for(int i = 0; i < argy->lenstr; ++i) {
        if(argy->strptr[i] == '"') {
          dsccat(lctx->ls->dstrdly, '\\');
        }
        dsccat(lctx->ls->dstrdly, argy->strptr[i]);
      }
      dsccat(lctx->ls->dstrdly, '"');
    } else {
      dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
    }
  ([[:blank:]]+|{MCOMMENT})*##([[:blank:]]+|{MCOMMENT})* { }
  (\"(\\.|[^\\"]|{SKIPNEWL})*\"|'(\\.|[^\\'\n])+'|[^[:alnum:]_\'\"\n[:blank:]/#])+ {
    dscat(lctx->ls->dstrdly, yytext, yyleng);
    }
  [[:blank:]]+ {dsccat(lctx->ls->dstrdly, ' ');}
  [[:cntrl:][:print:]] {
    dsccat(lctx->ls->dstrdly, *yytext);
    }
  <<EOF>> {
    if(YY_CURRENT_BUFFER->yy_input_file) fclose(YY_CURRENT_BUFFER->yy_input_file);
    yypop_buffer_state(yyscanner);
    yy_pop_state(yyscanner);
#if PPDEBUG
    printf("now lexing buffer containing %s\n", lctx->ls->dstrdly->strptr);
#endif
    YY_BUFFER_STATE ybs = YY_CURRENT_BUFFER;
    YY_BUFFER_STATE ylbs = yy_scan_bytes(lctx->ls->dstrdly->strptr, lctx->ls->dstrdly->lenstr, yyscanner);
    yy_switch_to_buffer(ybs, yyscanner);
    yypush_buffer_state(ylbs, yyscanner);
    char buf[256];
    snprintf(buf, 256, "%s", lctx->ls->defname);
    yylloc->first_line = yylloc->last_line = 1;
    yylloc->first_column = yylloc->last_column = 0;
    yylloc->filename = strdup(buf);
    rmpair(lctx->withindefines, lctx->ls->defname);
    htdtorcfr(lctx->ls->defargs, (void(*)(void*)) strdtor);
    strdtor(lctx->ls->dstrdly);
    if(lctx->ls->argpp->length) {
      struct arginfo* argi = dapop(lctx->ls->argpp);
      if(argi->argi) {
        lctx->ls->dstrdly = argi->argi;
        lctx->ls->paren_depth = argi->pdepth;
        lctx->ls->parg = argi->parg;
        argi->argi = NULL;
        dapush(lctx->ls->argpp, argi);
        insert(lctx->withindefines, lctx->ls->defname, NULL);
      } else {
        free(lctx->ls->defname);
        lctx->ls->defname = argi->defname;
        free(argi);
      }
    } else {
      free(lctx->ls->defname);
      lctx->ls->defname = NULL;
    }
    }
}

<HASINCLUDENEXT>{
  [[:blank:]]*\) {yy_pop_state(yyscanner);}
  [<"][^">]*[">] {
    //maybe check completion
    int i = 0;
    char pathbuf[256];
    yytext[yyleng - 1] = 0;
    yylval_param->unum = 0;
    char* strpardir = strdup(yylloc->filename);
    char* pardir = dirname(strpardir);
    for(; i < includepath->length; ++i) {
      if(!strcmp(pardir, daget(includepath, i))) break;
    }
    free(strpardir);
    ++i;
    for(; i < includepath->length; ++i) {
      FILE* newbuf;
      snprintf(pathbuf, 256, "%s/%s", (char*) daget(includepath, i), yytext + 1);
      if((newbuf = fopen(pathbuf, "r")) != NULL) {
        yylval_param->unum = 1;
        break;
      }
    } 
    return UNSIGNED_LITERAL;
    }
}

<CHECKDEFINED>{
  [[:blank:]]*\) {yy_pop_state(yyscanner);}
  {IDENT} {
    //maybe check completion
    yylval_param->unum = bigqueryval(lctx->defines, yytext);
#if PPDEBUG
      fprintf(stderr, "Value of identifier %s is %lu at %s %d.%d-%d.%d\n", yytext, yylval_param->unum, locprint2(yylloc));
#endif
    return UNSIGNED_LITERAL;
    }
}

<CHECKDEFINED2>{
  {IDENT} {
    yy_pop_state(yyscanner);
    yylval_param->unum = bigqueryval(lctx->defines, yytext);
#if PPDEBUG
      fprintf(stderr, "Value of identifier %s is %lu at %s %d.%d-%d.%d\n", yytext, yylval_param->unum, locprint2(yylloc));
#endif
    return UNSIGNED_LITERAL;
    }
}

<INCLUDE,INCLUDENEXT,DEFINE,UNDEF,IFDEF,DEFARG,DEFINE2,STRINGLIT,CALLMACRO>{SKIPNEWL} {/*the newline is ignored*/}
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
typedef {return TYPEDEF;}
static {return STATIC;}
extern {return EXTERN;}
inline {return INLINE;}
signed {return SIGNED;}
unsigned {return UNSIGNED;}
const {return CONST;}
volatile {return VOLATILE;}
restrict {return RESTRICT;}
char {return CHAR;}
short {return INT16;}
int {return INT32;}
long {return INT64;}
int8 {return INT8;}
int16 {return INT16;}
int32 {return INT32;}
int64 {return INT64;}
byte {return BYTE;}
dbyte {return DBYTE;}
qbyte {return QBYTE;}
obyte {return OBYTE;}
single {return SINGLE;}
float {return SINGLE;}
double {return DOUBLE;}
void {return VOID;}
case {return CASETK;}
default {return DEFAULTTK;}
if {return IF;}
else {return ELSE;}
switch {return SWITCHTK;}
while {return WHILE;}
do {return DO;}
for {return FOR;}
goto {return GOTO;}
continue {return CONTINUE;}
break {return BREAK;}
return {return RETURN;}
sizeof {return SIZEOF;}
struct {return STRUCTTK;}
enum {return ENUMTK;}
union {return UNIONTK;}
asm {return ASM;}
__asm__ {return ASM;}
_Noreturn {return NORETURN;}

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


<WITHINIF>{
  defined{BLANKC}"("{BLANKC} {yy_push_state(CHECKDEFINED, yyscanner);}
  defined{BLANKC} {yy_push_state(CHECKDEFINED2, yyscanner);}
  __has_include_next{BLANKC}"("{BLANKC} {yy_push_state(HASINCLUDENEXT, yyscanner);}
  {IDENT} {
    char* ds = strdup(yytext);
    int mt = check_type(ds, 0, yylloc, yyscanner);
    switch(mt) {
      default:
        free(ds);
        yylval_param->unum = 0;
        return UNSIGNED_LITERAL;
      case -1:
        break;
    } 
  }
}

{IDENT} {
  char* ylstr = strdup(yytext);
  int mt = check_type(ylstr, 1, yylloc, yyscanner);
  switch(mt) {
    case SYMBOL: case TYPE_NAME:
      yylval_param->str = ylstr;
      return mt;
  } 
}

<INITIAL,WITHINIF>{
  0[bB]{BIN}+{INTSIZE}? {
    yylval_param->unum = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
    return UNSIGNED_LITERAL;
  }
  0{OCT}+{INTSIZE}? {
    yylval_param->unum = strtoul(yytext,NULL,8);//every intconst is 8 bytes
    return UNSIGNED_LITERAL;
  }
  [[:digit:]]+{INTSIZE}?  {
    yylval_param->snum = strtoul(yytext,NULL,10);//every intconst is 8 bytes
    if(strchr(yytext,'u') || strchr(yytext,'U')) return UNSIGNED_LITERAL; return INTEGER_LITERAL;
  }
  0[xX][[:xdigit:]]+{INTSIZE}? {
    yylval_param->unum = strtoul(yytext,NULL,16);
    return UNSIGNED_LITERAL;
  }
}

[[:digit:]]+{EXP}{FLOATSIZE}?|[[:digit:]]*"."?[[:digit:]]+({EXP})?{FLOATSIZE}?|[[:digit:]]+"."?[[:digit:]]*({EXP})?{FLOATSIZE}?  {
     sscanf(yytext, "%lf", &yylval_param->dbl); return FLOAT_LITERAL;}

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
      fprintf(stderr, "Warning: octal character %s in character literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    }
    GOC((char) result);
    }
  \\0x[[:xdigit:]]{1,2}\' {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    GOC((char) result);
    }
  \\x[[:xdigit:]]{1,2}\' {
    unsigned int result;
    sscanf(yytext + 2, "%x", &result);
    GOC((char) result);
    }
  \\.\' {
    fprintf(stderr, "Warning: Unknown escape sequence \\%c %x in character literal %s %d.%d-%d.%d\n", *(yytext + 1), *(yytext + 1), locprint2(yylloc));
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

L?\" {/*"*/yy_push_state(STRINGLIT, yyscanner); lctx->ls->strcur = strctor(malloc(256), 0, 256);}
<STRINGLIT>{
  \" {/*"*/
    dsccat(lctx->ls->strcur, 0);
    yylval_param->dstr = lctx->ls->strcur;
  	yy_pop_state(yyscanner); 
  	return STRING_LITERAL;
  	}
  [\n\v] {
    strdtor(lctx->ls->strcur);
    fprintf(stderr, "Error: String terminated with newline unexpectedly %s %d.%d-%d.%d\n", locprint2(yylloc));
    yy_pop_state(yyscanner);
    }
  \\a {dsccat(lctx->ls->strcur, '\a');}
  \\b {dsccat(lctx->ls->strcur, '\b');}
  \\e {dsccat(lctx->ls->strcur, '\33');}
  \\f {dsccat(lctx->ls->strcur, '\f');}
  \\n {dsccat(lctx->ls->strcur, '\n');}
  \\r {dsccat(lctx->ls->strcur, '\r');}
  \\t {dsccat(lctx->ls->strcur, '\t');}
  \\v {dsccat(lctx->ls->strcur, '\v');}
  \\\' {dsccat(lctx->ls->strcur, '\'');}
  \\\" {dsccat(lctx->ls->strcur, '\"');/*"*/}
  \\\\ {dsccat(lctx->ls->strcur, '\\');}
  \\\? {dsccat(lctx->ls->strcur, '\?');}
  \\[0-7]{1,3} {
    unsigned int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds %s %d.%d-%d.%d\n", yytext, locprint2(yylloc));
    }
    dsccat(lctx->ls->strcur, result);
    }
  \\x[[:xdigit:]]{1,2} {
    unsigned int result;
    sscanf(yytext + 3, "%x", &result);
    dsccat(lctx->ls->strcur, result);
    }
  \\. {
    fprintf(stderr, "Warning: Unknown escape sequence \\%c %x in string literal %s %d.%d-%d.%d\n", *(yytext + 1), *(yytext + 1), locprint2(yylloc));
    dsccat(lctx->ls->strcur, yytext[1]);
  }
  [^\\\"\v]+ {/*"*/
    dscat(lctx->ls->strcur, yytext, yyleng);
  }
}
<INITIAL,WITHINIF>{
  ({SKIPNEWL}|[[:blank:]]|{MCOMMENT})+ {/*Whitespace, ignored*/}
  L?\' {yy_push_state(CHARLIT, yyscanner);}
}
<WITHINIF>\n {
  yy_pop_state(yyscanner);
  return 0;
}
[[:space:]] {/*Whitespace, ignored*/}

<<EOF>> {
  if(YY_CURRENT_BUFFER->yy_input_file) fclose(YY_CURRENT_BUFFER->yy_input_file);
  yypop_buffer_state(yyscanner);
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state(yyscanner);
    YYLTYPE* ylt = dapop(lctx->ls->locs);
    rmpair(lctx->withindefines, yylloc->filename);
    free(yylloc->filename);
    *yylloc = *ylt;
    free(ylt);
    if(lctx->ls->defname) {
      free(lctx->ls->defname);
      lctx->ls->defname = NULL;
    }
    //rmpair is a no-op if not in hash
  }
}

<*>\0 {//same as EOF
  if(YY_CURRENT_BUFFER->yy_input_file) fclose(YY_CURRENT_BUFFER->yy_input_file);
  yypop_buffer_state(yyscanner);
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state(yyscanner);
    YYLTYPE* ylt = dapop(lctx->ls->locs);
    rmpair(lctx->withindefines, yylloc->filename);
    free(yylloc->filename);
    *yylloc = *ylt;
    free(ylt);
    if(lctx->ls->defname) {
      free(lctx->ls->defname);
      lctx->ls->defname = NULL;
    }
    //rmpair is a no-op if not in hash
  }
}

<*>. {fprintf(stderr, "Unexpected character encountered: '%c' %d %s %d.%d-%d.%d\n", *yytext, *yytext, locprint2(yylloc));}
<*>\n {fprintf(stderr, "Unexpected newline encountered:  %s %d.%d-%d.%d\n", locprint2(yylloc));}
%%

int check_type(char* symb, char frominitial, YYLTYPE* yltg, yyscan_t yyscanner) {
  struct macrodef* macdef = bigsearch(lctx->defines, symb, 0);
  if(macdef && !queryval(lctx->withindefines, symb)) {
    char* oldname = lctx->ls->defname;
    lctx->ls->defname = symb;
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
    if(macdef->args) {
      char c;
      while(1) {
        c = input(yyscanner);
        ++yltg->last_column;
        switch(c) {
          case '\n': case '\v':
            yltg->last_column = 0;
            ++yltg->last_line;
          case ' ': case '\t':
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
        argi->argi = lctx->ls->dstrdly;
        argi->pdepth = lctx->ls->paren_depth;
        argi->defname = oldname;
        argi->parg = lctx->ls->parg;
        dapush(lctx->ls->argpp, argi);
        yy_push_state(CALLMACRO, yyscanner);
      }
      lctx->ls->paren_depth = 0;
      lctx->ls->dstrdly = strctor(malloc(256), 0, 256);
      lctx->ls->parg = dactor(64);
    } else {
      char buf[256];
      snprintf(buf, 256, "%s", symb);
      struct yyguts_t* yyg = (struct yyguts_t*) yyscanner;
      yypush_stringbuffer(macdef->text->strptr, macdef->text->lenstr, buf, YY_CURRENT_BUFFER, yyscanner);
      insert(lctx->withindefines, symb, NULL);
      if(frominitial == 2) {
        struct arginfo* argi = calloc(1, sizeof(struct arginfo));
        argi->defname = oldname;
        dapush(lctx->ls->argpp, argi);
        yy_push_state(CALLMACRO, yyscanner);
      } else {
        free(lctx->ls->defname);
        lctx->ls->defname = NULL;
      }
    }
    return -1;
  }
  nofcall:
  if(scopequeryval(lctx, M_TYPEDEF, symb)) {
#if PPDEBUG
    printf("Token %s detected as typedef\n", symb);
#endif
    return TYPE_NAME;
  }
#if PPDEBUG
    printf("Token %s treated as generic\n", symb);
#endif
  return SYMBOL;
}

inline void yypush_stringbuffer(char* str, int length, const char* macname, YY_BUFFER_STATE ybs, yyscan_t yyscanner) {
  YY_BUFFER_STATE ylbs = yy_scan_bytes(str, length, yyscanner);
  yy_switch_to_buffer(ybs, yyscanner);
  yypush_buffer_state(ylbs, yyscanner);
  YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
  YYLTYPE* ylc = yyget_lloc(yyscanner);
  *ylt = *ylc;
  dapush(lctx->ls->locs, ylt);
  ylc->first_line = ylc->last_line = 1;
  ylc->first_column = ylc->last_column = 0;
  if(macname) ylc->filename = strdup(macname);
  else ylc->filename = NULL;
}

void zz_pop_state(void*);
void zz_push_skip(void*);
void zz_pop_state(void* v) {yy_pop_state(v);}
void zz_push_skip(void* yyscanner) {yy_push_state(PPSKIP, yyscanner);}
