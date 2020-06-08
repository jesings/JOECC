BIN [0-1]
OCT [0-7]
IDENT [[:alpha:]_][[:alnum:]_]*
EXP [Ee][+-]?[[:digit:]]+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*
%{

#include <math.h>
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
int strconstlen, strconstindex;
char charconst;
extern DYNARR* locs;
extern DYNARR* file2compile;
void nc(char c) {
  if(strconstindex + 1 >= strconstlen) {
    strconst = realloc(strconst, strconstlen *= 1.5);
  }
  strconst[strconstindex++] = c;
}
#define GOC(c) yylval.ii.num = c, yy_pop_state(); return INTEGER_LITERAL

%}
%option yylineno
%option noyywrap
%option stack

%option debug
%option nodefault
%option warn

%x MULTILINE_COMMENT
%x SINGLELINE_COMMENT
%x PREPROCESSOR
%x INCLUDE
%x DEFINE
%x DEFARG
%x DEFINE2
%x IFNDEF
%x IFDEF
%x KILLSPACE
%x PPSKIP
%x STRINGLIT
%x CHARLIT

%%
<KILLSPACE>{
  [[:blank:]]+ {}
  [^[:blank:]] {yy_pop_state(); unput(*yytext);}
}

<INITIAL,PREPROCESSOR,INCLUDE,DEFINE,DEFARG,DEFINE2,IFDEF,IFNDEF>{
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
  [^\n#]+ {}
  # {}
  \n {}
  ^[[:blank:]]*#[[:blank:]]* {yy_push_state(PREPROCESSOR); stmtover = 0; skipping = 1;}
}

^[[:blank:]]*#[[:blank:]]* {yy_push_state(PREPROCESSOR); stmtover = 0; skipping = 0;}
<PREPROCESSOR>{
  include[[:blank:]]+ {if(!skipping) yy_push_state(INCLUDE); else yy_pop_state();}
  define[[:blank:]]+ {if(!skipping) yy_push_state(DEFINE); else yy_pop_state();}
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
  [[:space:]]*\n {yy_pop_state(); BEGIN(INITIAL); }
  \n {yy_pop_state();BEGIN(INITIAL);if(!stmtover) fprintf(stderr, "Error: incomplete include\n");}
  . {fprintf(stderr, "INCLUDE: I made a stupid: %c\n", *yytext);}
}

<DEFINE>{
  {IDENT} {yy_pop_state(); yy_push_state(DEFINE2); yy_push_state(KILLSPACE); defname = yytext;}
  {IDENT}\( {yy_pop_state(); yy_push_state(DEFARG); yy_push_state(KILLSPACE); yytext[yyleng - 1] = '\0'; defname = yytext;}
  \n {yy_pop_state();BEGIN(INITIAL);/*error state*/}
  . {fprintf(stderr, "DEFINE: I made a stupid: %c\n", *yytext);}
}

<DEFARG>{
  {IDENT}[[:blank:]]*\, {/*new arg encountered*/yy_push_state(KILLSPACE);}
  {IDENT}[[:blank:]]*\) {/*last arg encountered*/yy_pop_state(); yy_push_state(DEFINE2); yy_push_state(KILLSPACE);}
  \n {yy_pop_state();BEGIN(INITIAL);/*error state*/}
  . {fprintf(stderr, "DEFINE: I made a stupid: %c\n", *yytext);}
}

<DEFINE2>{
  [^\\/\n]+ {yymore();}
  "/" {yymore();}
  \\ {yymore();}
  \n {yy_pop_state(); yy_pop_state(); yytext[yyleng - 1] = '\0'; insert(ctx->defines, strdup(defname), strdup(yytext));}
}

<IFDEF>{
  [[:alpha:]_][[:alnum:]_]* {stmtover = 1; defname = yytext; yy_push_state(KILLSPACE);}
  \n {
    yy_pop_state();
    yy_pop_state(); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifdef!\n");
    } else {
      DYNARR* ds = ctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      if(ds->length > 0) {
        switch(*(enum ifdefstate*) dapeek(ds)) {
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
      } else {
        if(search(ctx->defines, defname)) {
          *rids = IFANDTRUE;
        } else {
          *rids = IFANDFALSE;
          yy_push_state(PPSKIP);
        }
      }
      dapush(ds, rids);
    }
    }
  . {fprintf(stderr, "IFDEF: I made a stupid: %c\n", *yytext);}
}
<IFNDEF>{
  [[:alpha:]_][[:alnum:]_]* {stmtover = 1; defname = yytext; yy_push_state(KILLSPACE);}
  \n {
    yy_pop_state();
    yy_pop_state(); 
    if(!stmtover) {
      /*error state*/
      fprintf(stderr, "Incomplete ifndef!\n");
    } else {
      DYNARR* ds = ctx->definestack;
      enum ifdefstate* rids = malloc(sizeof(enum ifdefstate));
      if(ds->length > 0) {
        switch(*(enum ifdefstate*) dapeek(ds)) {
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
      } else {
        if(search(ctx->defines, defname)) {
          *rids = IFANDFALSE;
          yy_push_state(PPSKIP);
        } else {
          *rids = IFANDTRUE;
        }
      }
      dapush(ds, rids);
    }
    }
  . {fprintf(stderr, "IFNDEF: I made a stupid: %c\n", *yytext);}
}

<INITIAL,SINGLELINE_COMMENT,PREPROCESSOR,INCLUDE,DEFINE,DEFINE2,IFDEF,IFNDEF,STRINGLIT>\\+[[:blank:]]*\n {/*the newline is ignored*/}
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
      yylval.idvariant = v; break;
    case ENUM_CONST:
      yylval.exprvariant = v; break;
    case IDENTIFIER:
      yylval.str = v; break;
    case LABEL:
    default:
      return YYUNDEF;
  } 
  return mt;
  }
0[bB]{BIN}+{INTSIZE}? {yylval.ii.num = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                       yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
0{OCT}+{INTSIZE}? {yylval.ii.num = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                   yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
[[:digit:]]+{INTSIZE}?  {yylval.ii.num = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                   yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
0[xX][[:xdigit:]]+{INTSIZE}? {yylval.ii.num = strtoul(yytext,NULL,16); /*specify intsize here in yylval.ii.size maybe?*/
                       yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}

[[:digit:]]+{EXP}{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl);return INTEGER_LITERAL;}
[[:digit:]]*"."?[[:digit:]]+({EXP})?{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl);return INTEGER_LITERAL;}
[[:digit:]]+"."?[[:digit:]]*({EXP})?{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl);return INTEGER_LITERAL;}

\' {yy_push_state(CHARLIT); }
<CHARLIT>{
  \' {
    fprintf(stderr, "Error: 0 length character literal %s\n", yytext);
    GOC('?');
  	}
  \n {
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
  [^\\\'] {
    int previndex = strconstindex;
    strconstindex += yyleng;
    while(strconstindex >= strconstlen) {
      strconst = realloc(strconst, strconstlen *= 1.5);
    }
    strcpy(strconst + previndex, yytext);
  }
  [^\']{2,}\' {
    fprintf(stderr, "Error: character literal too long %s\n", yytext);
    GOC(yytext[1]);
  }
}

\" {/*"*/yy_push_state(STRINGLIT); strconst = malloc(2048);}
<STRINGLIT>{
  \" {/*"*/
  	yylval.str = strconst; 
  	strconst[strconstindex < strconstlen ? strconstindex: strconstlen - 1] = 0; 
  	yy_pop_state(); 
  	puts(strconst);
  	return STRING_LITERAL;
  	}
  \n {
    free(strconst); 
    fputs("ERROR: String terminated with newline unexpectedly", stderr);
    yy_pop_state();
    }
  \\a {nc('\a');}
  \\b {nc('\b');}
  \\e {nc('\e');}
  \\f {nc('\f');}
  \\n {nc('\n');}
  \\r {nc('\r');}
  \\t {nc('\t');}
  \\v {nc('\v');}
  \\\' {nc('\'');}
  \\\" {nc('\"');/*"*/}
  \\\\ {nc('\\');}
  \\\? {nc('\?');}
  \\[0-7]{1,3} {
    int result;
    sscanf(yytext + 1, "%o", &result);
    if(result >= 1 << 8) {
      fprintf(stderr, "Warning: octal character %s in string literal out of bounds\n", yytext);
    }
    nc((char) result);
    }
  \\0x[[:xdigit:]]{1,2} {
    int result;
    sscanf(yytext + 3, "%x", &result);
    nc((char) result);
    }
  \\. {
    fprintf(stderr, "Warning: Unknown escape sequence %s in string literal\n", yytext);
    nc(yytext[1]);
  }
  [^\\]+ {
    int previndex = strconstindex;
    strconstindex += yyleng;
    while(strconstindex >= strconstlen) {
      strconst = realloc(strconst, strconstlen *= 1.5);
    }
    strcpy(strconst + previndex, yytext);
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
    yylloc = *(YYLTYPE*) dapop(locs);
    dapop(file2compile);
  }
}
%%
int check_type(void** garbage, char* symb) {
  SCOPEMEMBER* symtab_ent = search(scopepeek(ctx)->members,symb);
  if(!symtab_ent) {
    *garbage = symb;
    return IDENTIFIER;
  }
  switch(symtab_ent->mtype) {
    case M_TYPEDEF:
      *garbage = symtab_ent->typememb;
      return TYPE_NAME;
    case M_ENUM_CONST:
      *garbage = (void*) symtab_ent->enumnum;
      return ENUM_CONST;
    case M_VARIABLE:
      *garbage = symb;
      return IDENTIFIER;
    case M_LABEL:
      return LABEL;
    default:
      return 0;
  }
}

