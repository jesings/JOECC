BIN [0-1]
OCT [0-7]
DEC [0-9]
HEX [0-9A-Fa-f]
LET [a-zA-Z_]
EXP [Ee][+-]?{DEC}+
FLOATSIZE (f|F|l|L)
INTSIZE (u|U|l|L)*

%{

#include "highCs.tab.h"
#include "hash.h"
#include <math.h>

#define YY_USER_ACTION \
    yylloc->first_line = yylloc->last_line; \
    yylloc->first_column = yylloc->last_column; \
    for(int i = 0; yytext[i] != '\0'; i++) { \
        if(yytext[i] == '\n') { \
            yylloc->last_line++; \
            yylloc->last_column = 0; \
        } \
        else { \
            yylloc->last_column++; \
        } \
    }
#define SIGNEDCHAR 0

int check_type(struct lexctx* ctx);
%}

%option yylineno
%x MULTILINE_COMMENT
%x SINGLELINE_COMMENT

%%
"/*" {BEGIN(MULTILINE_COMMENT);}
<MULTILINE_COMMENT>"*/" {BEGIN(INITIAL);}
<MULTILINE_COMMENT>[\n|.] {/*The multiline comment is not terminated*/}
"//" {BEGIN(SINGLELINE_COMMENT);}
<SINGLELINE_COMMENT>. {/*The single line comment is not terminated*/}
<SINGLELINE_COMMENT>\n {BEGIN(INITIAL);}
"->" {return ARROW;}
"++" {return INC;}
"--" {return DEC;}
"<<" {return SHL;}
">>" {return SHR;}
"<=" {return LE;}
">=" {return GE;}
"==" {return EQ;}
"!=" {return NEQ;}
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
"case" {return CASE;}
"default" {return DEFAULT;}
"if" {return IF;}
"else" {return ELSE;}
"switch" {return SWITCH;}
"while" {return WHILE;}
"do" {return DO;}
"for" {return FOR;}
"goto" {return GOTO;}
"continue" {return CONTINUE;}
"break" {return BREAK;}
"return" {return RETURN;}
"sizeof" {return SIZEOF;}
"struct" {return STRUCT;}
"enum" {return ENUM;}
"union" {return UNION;}
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

((?i:"infinity")|(?i:"inf")) {yylval.dbl = 0x7f800000; return FLOAT;}
(?i:"nan") {yylval.dbl = 0x7fffffff; return FLOAT;}

{LET}({LET}|{DEC})* {yylval.str = strdup(yytext);
                     return check_type(); }
0[bB]{BIN}+{INTSIZE}? {yylval.num = strtoul(yytext+2,NULL,2);//every intconst is 8 bytes
                       yylval.sign = !(strchr(yytext,'u') || strchr(yytext,'U'));
                       return INT; }
0{OCT}+{INTSIZE}? {yylval.num = strtoul(yytext,NULL,8);//every intconst is 8 bytes
                   yylval.sign = !(strchr(yytext,'u') || strchr(yytext,'U'));
                   return INT; }
{DEC}+{INTSIZE}?  {yylval.num = strtoul(yytext,NULL,10);//every intconst is 8 bytes
                   yylval.sign = !(strchr(yytext,'u') || strchr(yytext,'U'));
                   return INT; }
0[xX]{HEX}+{INTSIZE}? {yylval.num = strtoul(yytext,NULL,16); /*specify intsize here in yylval.size maybe?*/
                       yylval.sign = !(strchr(yytext,'u') || strchr(yytext,'U'));
                       return INT; }

{DEC}+{EXP}{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl); return FLOAT;}
{DEC}*"."?{DEC}+({EXP})?{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl); return FLOAT;}
{DEC}+"."?{DEC}*({EXP})?{FLOATSIZE}? {sscanf(yytext, "%ld", &yylval.dbl); return FLOAT;}

'(\\.|[^\\'])+'	{yylval.num = charconv(strdup(yytext));
                 yylval.sign = IFSIGNEDCHAR;
                 return INT;}
\"(\\.|[^\\"])*\" {yylval.str = strconv(strdup(yytext));
                  return STRING_LITERAL;}
L'(\\.|[^\\'])+' {yylval.num = widecharconv(strdup(yytext));
                  yylval.sign = IFSIGNEDCHAR;
                  return INT;}
L\"(\\.|[^\\"])*\" {yylval.str = widestrconv(strdup(yytext));
                    return STRING_LITERAL;}

[\t\v\n\f] {/*Whitespace, ignored*/}
. {/*Other char, ignored*/}
%%
int yywrap(){
  return 1;
}
int check_type(struct lexctx* ctx){
  IDTYPE* possible_type =  search(ctx->typedefs,yytext);
  if(possible_type)
    return TYPE_NAME;
  return IDENTIFIER;
}

