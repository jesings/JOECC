BIN [0-1]
OCT [0-7]
IDENT [[:alpha:]_][[:alnum:]_]*
EXP [Ee][+-]?[[:digit:]]+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*
%{

#include <math.h>
#include <stdarg.h>
#include "joecc.tab.h"
#include "compintern.h"

#define YY_USER_ACTION \
    yylloc.first_line = yylloc.last_line; \
    yylloc.first_column = yylloc.last_column; \
    for(int i = 0; yytext[i] != '\0'; i++) { \
        if(yytext[i] == '\n') { \
            yylloc.last_line++; \
            yylloc.last_column = 0; \
        } \
        else { \
            yylloc.last_column++; \
        } \
    }
#define SIGNEDCHAR 0

extern struct lexctx* ctx;
int check_type(void** garbage, char* symb);
char stmtover, skipping;
char* defname, * strconst;
int paren_depth;
char charconst;
struct macrodef* md;
char argeaten;
DYNARR* parg;
HASHTABLE* defargs = NULL;
DYNSTR* dstrdly;
extern DYNARR* locs;
extern DYNARR* file2compile;
DYNSTR* strcur;

#define GOC(c) yylval.ii.num = c, yy_pop_state(); return INTEGER_LITERAL
%}
%option yylineno
%option noyywrap
%option stack

%option debug
%option warn
%option nodefault

%x MULTILINE_COMMENT SINGLELINE_COMMENT
%x PREPROCESSOR INCLUDE 
%x DEFINE UNDEF DEFARG DEFINE2
%x IFNDEF IFDEF PPSKIP
%x KILLBLANK KILLSPACE
%x STRINGLIT CHARLIT
%x CALLMACRO FINDREPLACE

%%
<KILLBLANK>{
  [[:blank:]]+ {}
  [^[:blank:]] {yy_pop_state(); unput(*yytext);}
}
<KILLSPACE>{
  [[:space:]]+ {}
  [^[:space:]] {yy_pop_state(); unput(*yytext);}
}

<INITIAL,PREPROCESSOR,INCLUDE,DEFINE,UNDEF,DEFARG,DEFINE2,IFDEF,IFNDEF,CALLMACRO,PPSKIP>{
  "/*" {yy_push_state(MULTILINE_COMMENT);}
  "//" {yy_push_state(SINGLELINE_COMMENT);}
}

<MULTILINE_COMMENT>{
  [^"*"]+ {/*The multiline comment is not terminated*/}
  "*"+ {/*The multiline comment is not terminated*/}
  "*"+"/" {yy_pop_state();}
}

<SINGLELINE_COMMENT>{
  [^\\\n]* {/*The single line comment is not terminated*/}
  \\+ {/*The single line comment is not terminated*/}
  \n {yy_pop_state(); unput('\n');}
}

<PPSKIP>{
  [^\n#\/]+ {}
  [\/\n#] {}
  ^[[:blank:]]*#[[:blank:]]* {yy_push_state(PREPROCESSOR); stmtover = 0; skipping = 1;}
}

^[[:blank:]]*#[[:blank:]]* {yy_push_state(PREPROCESSOR); stmtover = 0; skipping = 0;}
<PREPROCESSOR>{
  include[[:blank:]]+ {if(!skipping) yy_push_state(INCLUDE); else yy_pop_state();}
  define[[:blank:]]+ {if(!skipping) yy_push_state(DEFINE); else yy_pop_state(); md = calloc(1, sizeof(struct macrodef));}
  undef[[:blank:]]+ {if(!skipping) yy_push_state(UNDEF); else yy_pop_state();}
  ifdef[[:blank:]]+ {yy_push_state(IFDEF);}
  ifndef[[:blank:]]+ {yy_push_state(IFNDEF);}
  else[[:blank:]]* {
    yy_pop_state();
    DYNARR* ds = ctx->definestack;
    enum ifdefstate ids = ds->length > 0 ? *(enum ifdefstate*) dapeek(ds) : ELSEANDFALSE;
    switch(ids) {
      case IFANDTRUE: 
        *(enum ifdefstate*) dapeek(ds) = ELSEANDTRUE;
        yy_push_state(PPSKIP);
        break;
      case IFANDFALSE: 
        *(enum ifdefstate*) dapeek(ds) = ELSEANDFALSE;
        yy_pop_state();
        break;
      default:
        fputs("Error: Unexpected #else", stderr);
      case IFDEFDUMMY:
        break;
    }
    }
  endif[[:blank:]]* {/*handle endif case*/
    yy_pop_state();
    DYNARR* ds = ctx->definestack;
    if(ds->length > 0) {
      enum ifdefstate ids = *(enum ifdefstate*) dapop(ds);
      switch(ids) {
        case IFANDTRUE: case ELSEANDFALSE:
          break;
        default: //case IFANDFALSE: case ELSEANDTRUE: case IFDEFDUMMY:
          yy_pop_state();
          break;
      }
    } else {
      fputs("Error: Unexpected #endif", stderr);
    }
    }
  \n {yy_pop_state();/*error state*/}
  . {fprintf(stderr, "PREPROCESSOR: I made a stupid: %c\n", *yytext);}
}

<INCLUDE>{
  "<"[^>\n]*">" {
    if(stmtover) REJECT;
    stmtover = 1;
    yytext[yyleng - 1] = '\0'; //ignore closing >
    char pathbuf[2048];
    snprintf(pathbuf, 2048, "/usr/include/%s", yytext + 1); //ignore opening <
    FILE* newbuf;
    if((newbuf = fopen(pathbuf, "r")) != NULL) {
      //YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE);
      //yy_push_state(INITIAL);
      //yypush_buffer_state(ybs);
    } else {
      fprintf(stderr, "Invalid system file %s included!\n", yytext + 1);
    }
  }
  \"[^\"\n]*\" {/*"*/
  	if(stmtover) REJECT;
    stmtover = 1;
    yytext[yyleng - 1] = '\0'; //ignore closing "
    FILE* newbuf;
    if((newbuf = fopen(yytext + 1, "r")) != NULL) { //ignore opening "
      YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
      *ylt = yylloc;
      dapush(locs, ylt);
      yylloc.first_line = yylloc.last_line = yylloc.first_column = yylloc.last_column = 1;
      dapush(file2compile, strdup(yytext + 1));
      YY_BUFFER_STATE ybs = yy_create_buffer(newbuf, YY_BUF_SIZE);
      yy_push_state(INITIAL);
      yypush_buffer_state(ybs);
    } else {
      fprintf(stderr, "Invalid local file %s included!\n", yytext + 1);
    }
  }
  [[:space:]]+[<\"] {if(stmtover) REJECT;/*"*/yyless(1);}
  [[:space:]]*\n {yy_pop_state(); yy_pop_state(); }
  \n {yy_pop_state(); yy_pop_state();if(!stmtover) fprintf(stderr, "Error: incomplete include\n");}
  . {fprintf(stderr, "INCLUDE: I made a stupid: %c\n", *yytext);}
}

<DEFINE>{
  {IDENT} {yy_pop_state(); yy_push_state(DEFINE2); yy_push_state(KILLBLANK); defname = strdup(yytext);}
  {IDENT}\( {yy_pop_state(); yy_push_state(DEFARG); yy_push_state(KILLBLANK); yytext[yyleng - 1] = '\0'; defname = strdup(yytext); md->args = dactor(8); argeaten = 0;}
  \n {yy_pop_state(); yy_pop_state();/*error state*/}
  . {fprintf(stderr, "DEFINE: I made a stupid: %c\n", *yytext);}
}

<DEFARG>{
  {IDENT} {if(argeaten) fprintf(stderr, "Error: unexpected macro argument\n"); argeaten = 1;/*new arg encountered*/ yy_push_state(KILLBLANK); dapush(md->args, strdup(yytext));/*probably should confirm no 2 args have the same name*/}
  \, {if(argeaten) argeaten = 0; else fprintf(stderr, "Error: unexpected macro argument\n"); yy_push_state(KILLBLANK);}
  \) {if(!argeaten && md->args->length != 0) fprintf(stderr, "Error: unexpected macro argument\n"); /*last arg encountered*/yy_pop_state(); yy_push_state(DEFINE2); yy_push_state(KILLBLANK);}
  \n {yy_pop_state(); yy_pop_state();/*error state*/}
  . {fprintf(stderr, "DEFINE: I made a stupid: %c\n", *yytext);}
}

<DEFINE2>{
  [^\\/\n]+ {yymore();}
  "/" {yymore();}
  \\ {yymore();}
  \n {yy_pop_state(); yy_pop_state();  md->text = malloc(yyleng); memcpy(md->text, yytext, yyleng - 1); md->text[yyleng - 1] = '\0';  insert(ctx->defines, defname, md);}
}

<UNDEF>{
  {IDENT} {rmpairfr(ctx->defines, yytext); yy_push_state(KILLBLANK);}
  \n {yy_pop_state(); yy_pop_state();/*error state if expr not over?*/}
  . {fprintf(stderr, "UNDEF: I made a stupid: %c\n", *yytext);}
}


<IFDEF>{
  {IDENT} {stmtover = 1; defname = strdup(yytext); yy_push_state(KILLBLANK);}
  \n {
    yy_pop_state();
    yy_pop_state(); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifdef!\n");
    } else {
      DYNARR* ds = ctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length <= 0 ? IFANDTRUE : *(enum ifdefstate*) dapeek(ds);
      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
          if(search(ctx->defines, defname)) {
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
  . {fprintf(stderr, "IFDEF: I made a stupid: %c\n", *yytext);}
}
<IFNDEF>{
  {IDENT} {stmtover = 1; defname = strdup(yytext); yy_push_state(KILLBLANK);}
  \n {
    yy_pop_state();
    yy_pop_state(); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifndef!\n");
    } else {
      DYNARR* ds = ctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      enum ifdefstate contval = ds->length <= 0 ? IFANDTRUE : *(enum ifdefstate*) dapeek(ds);
      switch(contval) {
        case IFANDTRUE: case ELSEANDFALSE:
          if(search(ctx->defines, defname)) {
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
  . {fprintf(stderr, "IFNDEF: I made a stupid: %c\n", *yytext);}
}

<CALLMACRO>{
  /*we are not going to handle stringizing or concatenation in macros*/
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
      dsccat(dstrdly, 0);
      dapush(parg, dstrdly->strptr);
      free(dstrdly);
      if(!parg) {
        fprintf(stderr, "Error: Malformed function-like macro call, arguments not accessible\n");
        //big error
      } else {
        struct macrodef* md;

        if(!(md = search(ctx->defines, defname))) {
          fprintf(stderr, "Error: Malformed function-like macro call\n");
          //error state
        } else if(parg->length != md->args->length) {
          if(md->args->length == 0 && parg->length == 1) {
            if(*(char*) (parg->arr[0]) == 0) {
              free(parg->arr[0]);
              free(parg);
              yy_pop_state();
              YY_BUFFER_STATE ybs = yy_create_buffer(fmemopen(defname, strlen(defname), "r"), YY_BUF_SIZE);//strlen inefficient
              yypush_buffer_state(ybs);
              char buf[256];
              snprintf(buf, 256, "call to macro %s", defname);
              yylloc.first_line = yylloc.last_line = yylloc.first_column = yylloc.last_column = 1;
              dapush(file2compile, strdup(buf));
              yy_push_state(INITIAL);
            }
          } else {
            fprintf(stderr, "Error: the number of arguments passed to function-like macro is different than the number of parameters\n");
            //error state
          }
        } else {
          DYNARR* argn = md->args;
          //make hash table with keys as param names, values as arguments, make 
          //special lexer mode to parse this into string literal, use this as new buffer
          //at the end of this mode, switch to string literal, parse in parent mode then
          defargs = htctor();
          char** prma = (char**) argn->arr;
          char** arga = (char**) parg->arr;
          for(int i = 0; i < parg->length; i++) {
            insert(defargs, prma[i], arga[i]);
          }
          dstrdly = strctor(malloc(2048), 0, 2048);
          //yydebug = 1;
          yy_pop_state();
          yy_push_state(FINDREPLACE);
          YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
          *ylt = yylloc;
          dapush(locs, ylt);
          yylloc.first_line = yylloc.last_line = yylloc.first_column = yylloc.last_column = 1;
          YY_BUFFER_STATE ybs = yy_create_buffer(fmemopen(md->text, strlen(md->text), "r"), YY_BUF_SIZE);//strlen inefficient
          yypush_buffer_state(ybs);
        }
      }
    }
    }
  [^\(\)\",]*[^[:space:]\(\)\",] {/*"*/
    dscat(dstrdly, yytext, yyleng);
    }
  \"(\\.|[^\\"]|\/[[:space:]]*\n)*\" {/*"*/
  	//this is here to make sure that parenthesis depth isn't changed within strings
    dscat(dstrdly, yytext, yyleng);
    }
  [[:space:]]+ {
    dsccat(dstrdly, ' ');
  }
  [[:space:]]*,[[:space:]]* {
    if(paren_depth) {
      char tmpstr[3], tmpstrl = 0;
      if(yytext[0] == ' ')
        tmpstr[tmpstrl++] = ' ';
      tmpstr[tmpstrl++] = ',';
      if(yytext[yyleng - 1] == ' ')
        tmpstr[tmpstrl++] = ' ';
      dscat(dstrdly, tmpstr, tmpstrl);
    } else {
      dsccat(dstrdly, 0);
      dapush(parg, dstrdly->strptr);
      free(dstrdly);
      dstrdly = strctor(malloc(4096), 0, 4096);
    }
    }
  . {fprintf(stderr, "Error: unexpected character in function macro call\n");}
}

<FINDREPLACE>{
  {IDENT} {
    char* argy = search(defargs, yytext);
    if(argy) {
      int sl = strlen(argy); //TODO:find more efficient way to get length
      dscat(dstrdly, argy, sl);
    } else {
      dscat(dstrdly, yytext, yyleng);
    }
    }
  \"(\\.|[^\\"]|\/[[:blank:]]*\n)*\" {/*"*/
    dscat(dstrdly, yytext, yyleng);
    }
  '(\\.|[^\\'\n])+' {
    dscat(dstrdly, yytext, yyleng);
    }
  [^[:alnum:]_\'\"\n]+ {/*cntrl/delim expr*/
    dscat(dstrdly, yytext, yyleng);
    }
  [[:cntrl:][:print:]] {
    dsccat(dstrdly, *yytext);
    }
  <<EOF>> {
    yypop_buffer_state();
    yy_pop_state();
    YY_BUFFER_STATE ybs = yy_create_buffer(fmemopen(dstrdly->strptr, dstrdly->lenstr, "r"), YY_BUF_SIZE);
    free(dstrdly);
    yypush_buffer_state(ybs);
    char buf[256];
    snprintf(buf, 256, "call to macro %s", defname);
    yylloc.first_line = yylloc.last_line = yylloc.first_column = yylloc.last_column = 1;
    dapush(file2compile, strdup(buf));
    yy_push_state(INITIAL);
    }
}


<INITIAL,SINGLELINE_COMMENT,PREPROCESSOR,INCLUDE,DEFINE,DEFINE2,IFDEF,IFNDEF,STRINGLIT,KILLBLANK,KILLSPACE>\\+[[:blank:]]*\n {/*the newline is ignored*/}
"->" {return ARROWTK;}
"++" {return INC;}
"--" {return DEC;}
"<<" {return SHLTK;}
">>" {return SHRTK;}
"<=" {return LE;}
">=" {return GE;}
"==" {return EQTK;}
"!=" {return NEQTK;}
"&&" {return AND;}
"||" {return OR;}
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
"typedef" {return TYPEDEF;}
"static" {return STATIC;}
"extern" {return EXTERN;}
"signed" {return SIGNED;}
"unsigned" {return UNSIGNED;}
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
"&" {return '&';}
"*" {return '*';}
"<" {return '<';}
">" {return '>';}
"[" {return '[';}
"]" {return ']';}
":" {return ':';}
"," {return ',';}
"{" {return '{';}
"}" {return '}';}
"=" {return '=';}
"!" {return '!';}
"-" {return '-';}
"(" {return '(';}
")" {return ')';}
"%" {return '%';}
"." {return '.';}
"|" {return '|';}
"+" {return '+';}
"?" {return '?';}
";" {return ';';}
"/" {return '/';}
"~" {return '~';}
"^" {return '^';}

((?i:"infinity")|(?i:"inf")) {yylval.dbl = 0x7f800000; return FLOAT_LITERAL;}
(?i:"nan") {yylval.dbl = 0x7fffffff; return FLOAT_LITERAL;}

{IDENT} {
  char* ylstr = strdup(yytext);
  void* v;
  int mt = check_type(&v, ylstr);
  switch(mt) {
    case TYPE_NAME:
      yylval.idvariant = v;
      return mt;
    case ENUM_CONST:
      yylval.exprvariant = v;
      return mt;
    case IDENTIFIER:
      yylval.str = v;
      return mt;
    case LABEL:
      return YYUNDEF;
  } 
  }

0[bB]{BIN}+{INTSIZE}? {yylval.ii.num = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                       yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
0{OCT}+{INTSIZE}? {yylval.ii.num = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                   yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
[[:digit:]]+{INTSIZE}?  {yylval.ii.num = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                   yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
0[xX][[:xdigit:]]+{INTSIZE}? {yylval.ii.num = strtoul(yytext,NULL,16); /*specify intsize here in yylval.ii.size maybe?*/
                       yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}

[[:digit:]]+{EXP}{FLOATSIZE}? {sscanf(yytext, "%lf", &yylval.dbl);return FLOAT_LITERAL;}
[[:digit:]]*"."?[[:digit:]]+({EXP})?{FLOATSIZE}? {sscanf(yytext, "%lf", &yylval.dbl);return FLOAT_LITERAL;}
[[:digit:]]+"."?[[:digit:]]*({EXP})?{FLOATSIZE}? {sscanf(yytext, "%lf", &yylval.dbl);return FLOAT_LITERAL;}

\' {yy_push_state(CHARLIT); }
<CHARLIT>{
  \' {
    fprintf(stderr, "Error: 0 length character literal %s\n", yytext);
    GOC('?');
  	}
  [\n\v] {
    fputs("ERROR: character literal terminated with newline unexpectedly", stderr);
    yy_pop_state();
    }
  \\a\' {GOC('\a');}
  \\b\' {GOC('\b');}
  \\e\' {GOC('\e');}
  \\f\' {GOC('\f');}
  \\n\' {GOC('\n');}
  \\r\' {GOC('\r');}
  \\t\' {GOC('\t');}
  \\v\' {GOC('\v');}
  \\\'\' {GOC('\'');}
  \\\"\' {GOC('\"');/*"*/}
  \\\\\' {GOC('\\');}
  \\\?\' {GOC('\?');}
  \\[0-7]{1,3}\' {
    int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds\n", yytext);
    }
    GOC((char) result);
    }
  \\0x[[:xdigit:]]{1,2}\' {
    int result;
    sscanf(yytext + 3, "%x", &result);
    GOC((char) result);
    }
  \\.\' {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal\n", yytext);
    GOC(yytext[1]);
  }
  [^\\\'\n\v]\' {
    GOC(yytext[0]);
  }
  [^\']{2,}\' {
    fprintf(stderr, "Error: character literal too long %s\n", yytext);
    GOC(yytext[1]);
  }
}

\" {/*"*/yy_push_state(STRINGLIT); strcur = strctor(malloc(2048), 0, 2048);}
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
    fputs("ERROR: String terminated with newline unexpectedly", stderr);
    yy_pop_state();
    }
  \\a {dsccat(strcur, '\a');}
  \\b {dsccat(strcur, '\b');}
  \\e {dsccat(strcur, '\e');}
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
    int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds\n", yytext);
    }
    dsccat(strcur, result);
    }
  \\0x[[:xdigit:]]{1,2} {
    int result;
    sscanf(yytext + 3, "%x", &result);
    dsccat(strcur, result);
    }
  \\. {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal\n", yytext);
    dsccat(strcur, yytext[1]);
  }
  [^\\\"\v]+ {/*"*/
    dscat(strcur, yytext, yyleng);
  }
}

[[:space:]]+ {/*Whitespace, ignored*/}
. {fprintf(stderr, "Unexpected character encountered: %c\n", *yytext);}

<<EOF>> {
  //fprintf(stderr, "I want for death\n");
  yypop_buffer_state();
  if ( !YY_CURRENT_BUFFER ) {
    yyterminate();
  } else {
    yy_pop_state();
    stmtover = 1;
    YYLTYPE* yl = dapop(locs);
    yylloc = *yl;
    free(yl);
    dapop(file2compile);
  }
}
%%
int check_type(void** garbage, char* symb) {
  struct macrodef* macdef = search(ctx->defines, symb);
  if(macdef) {
    defname = symb;
    if(macdef->args) {
      //handle function like macro
      //TODO: also confirm that is followed by parentheses
      char c;
      while(1) {
        c = input();
        switch(c) {
          case ' ': case '\t': case '\n': case '\v':
          	break;
          case '(':
          	goto whiledone;
          default:
            unput(c);
            goto nofcall;

        }
      }
      whiledone:
      yy_push_state(CALLMACRO);
      paren_depth = 0;
      dstrdly = strctor(malloc(4096), 0, 4096);
      parg = dactor(64); 
    } else {
      //yy_push_state(yy_top_state());
      yy_push_state(INITIAL);
      char* buf = malloc(256);
      snprintf(buf, 256, "Macro %s", symb);
      dapush(file2compile, buf);
      YYLTYPE* ylt = malloc(sizeof(YYLTYPE));
      *ylt = yylloc;
      dapush(locs, ylt);
      yylloc.first_line = yylloc.last_line = yylloc.first_column = yylloc.last_column = 1;
      //YY_BUFFER_STATE yms = yy_scan_string(macdef->text);
      YY_BUFFER_STATE yms = yy_create_buffer(fmemopen(macdef->text, 
      	strlen(macdef->text), "r"), YY_BUF_SIZE);// strlen is inefficient
      yypush_buffer_state(yms);
      //yydebug = 1;
    }
    return -1;
  }
  nofcall: ;
  SCOPEMEMBER* defntype = search(scopepeek(ctx)->typesdef, symb);
  if(defntype) {
    *garbage = defntype->typememb;
    return TYPE_NAME;
  }
  SCOPEMEMBER* symtab_ent = search(scopepeek(ctx)->members, symb);
  if(!symtab_ent) {
    *garbage = symb;
    return IDENTIFIER;
  }
  switch(symtab_ent->mtype) {
    case M_ENUM_CONST:
      *garbage = (void*) symtab_ent->enumnum;
      return ENUM_CONST;
    default:
    case M_VARIABLE: 
      *garbage = symb;
      return IDENTIFIER;
    case M_LABEL:
      return LABEL;
  }
}

