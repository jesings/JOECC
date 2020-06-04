BIN [0-1]
OCT [0-7]
DEC [0-9]
HEX [0-9A-Fa-f]
LET [a-zA-Z_]
EXP [Ee][+-]?{DEC}+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*

%{

#include <math.h>
#include "joecc.tab.h"
#include "conv.h"
#include "compintern.h"

//#define YY_USER_ACTION \
//    yylloc->first_line = yylloc->last_line; \
//    yylloc->first_column = yylloc->last_column; \
//    for(int i = 0; yytext[i] != '\0'; i++) { \
//        if(yytext[i] == '\n') { \
//            yylloc->last_line++; \
//            yylloc->last_column = 0; \
//        } \
//        else { \
//            yylloc->last_column++; \
//        } \
//    }
#define SIGNEDCHAR 0

extern struct lexctx* ctx;
int check_type(void** garbage, char* symb);
%}
%option yylineno
%option noyywrap

%x MULTILINE_COMMENT
%x SINGLELINE_COMMENT

%%
"/*" {BEGIN(MULTILINE_COMMENT);}
<MULTILINE_COMMENT>"*/" {BEGIN(INITIAL);}
<MULTILINE_COMMENT>[\n|.] {/*The multiline comment is not terminated*/}
"//" {BEGIN(SINGLELINE_COMMENT);}
<SINGLELINE_COMMENT>. {/*The single line comment is not terminated*/}
<SINGLELINE_COMMENT>\n {BEGIN(INITIAL);}
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

{LET}({LET}|{DEC})* {
                      yylval.str = strdup(yytext);
                      void* v;
                      int mt = check_type(&v, yytext);
                      switch(mt) {
                        case M_TYPEDEF:
                          yylval.idvariant = v; break;
                        case M_ENUM_CONST:
                          yylval.exprvariant = v; break;
                        case IDENTIFIER:
                          yylval.str = v; break;
                        default:
                          return YYUNDEF;
                      } 
                      return mt;
                    }
0[bB]{BIN}+{INTSIZE}? {yylval.ii.num = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                       yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
0{OCT}+{INTSIZE}? {yylval.ii.num = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                   yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
{DEC}+{INTSIZE}?  {yylval.ii.num = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                   yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}
0[xX]{HEX}+{INTSIZE}? {yylval.ii.num = strtoul(yytext,NULL,16); /*specify intsize here in yylval.ii.size maybe?*/
                       yylval.ii.sign = !(strchr(yytext,'u') || strchr(yytext,'U')); return INTEGER_LITERAL;}

{DEC}+{EXP}{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl);return INTEGER_LITERAL;}
{DEC}*"."?{DEC}+({EXP})?{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl);return INTEGER_LITERAL;}
{DEC}+"."?{DEC}*({EXP})?{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl);return INTEGER_LITERAL;}

'(\\.|[^\\'])+'	{yylval.ii.num = charconv(&yytext); return INTEGER_LITERAL;}
\"(\\.|[^\\"])*\" {yylval.str = strconv(yytext); yylval.ii.sign = 0; return STRING_LITERAL;}/*"to fix syntax highlighting*/

<<EOF>>  {
  fprintf(stderr, "Fuck you\n");
  yyterminate();
}

[\t\v\n\f] {/*Whitespace, ignored*/}
. {/*Other char, ignored*/}
%%
//int yywrap() {
//  return 1;
//}
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

