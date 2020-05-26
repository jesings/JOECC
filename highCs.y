%locations
%token ARROWTK "->" INC "++" DEC "--" SHLTK "<<" SHRTK ">>" LE "<=" GE ">=" EQTK "=="
%token NEQTK "!=" AND "&&" OR "||" DIV_GETS "/=" MUL_GETS "*=" MOD_GETS "%="
%token ADD_GETS "+=" SUB_GETS "-=" SHL_GETS "<<=" SHR_GETS ">>=" AND_GETS "&=" 
%token XOR_GETS "^=" OR_GETS "|="

%token TYPEDEF "typedef" STATIC "static" EXTERN "extern" CHAR "char"
%token INT8 "int8" INT16 "int16" INT32 "int32" INT64 "int64" BYTE "byte"
%token DBYTE "dbyte" QBYTE "qbyte" OBYTE "obyte" SINGLE "single" DOUBLE "double" 
%token CASETK "case" DEFAULTTK "default" IF "if" ELSE "else" SWITCHTK "switch"
%token WHILE "while" DO "do" FOR "for" GOTO "goto" CONTINUE "continue" 
%token BREAK "break" RETURN "return" SIZEOF "sizeof" UNSIGNED "unsigned"
%token STRUCTTK "struct" ENUMTK "enum" UNIONTK "union" SIGNED "signed"

%right THEN "else"
%start program
%define parse.assert
%define parse.error verbose

%token<ii> INTEGER_LITERAL
%token<dbl> FLOAT_LITERAL;
%token<str> IDENTIFIER STRING_LITERAL TYPE_NAME;
%token<wstr> WSTRING_LITERAL;

%code requires{
#include <stdint.h>
#include "compintern.h"
#include "lex.h"


#define aget(param, index) ((INITIALIZER*) (param)->arr[(index)])
#define dget(param, index) ((DECLARATION*) (param)->arr[(index)])

}

%param {struct lexctx* ctx}
%union {
  struct intinfo ii;
  char* str;
  wchar_t* wstr;
  double dbl;
  TYPEBITS typevariant;
  EXPRESSION* exprvariant;
  IDTYPE* idvariant;
  INITIALIZER* initvariant;
  STATEMENT* stmtvariant;
  DYNARR* arrvariant;
  UNION* unionvariant;
  STRUCT* structvariant;
  ENUM* enumvariant;
  DECLARATION* declvariant;
  FUNC* funcvariant;
}

%type<typevariant> types1 types2 types1o
%type<idvariant> typem typews1 /*typebs*/ type
%type<exprvariant> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee
%type<stmtvariant> statement compound_statement
%type<arrvariant> statements_and_initializers struct_decls struct_decl cs_decls enums escl abstract_ptr params cs_inits cs_minutes initializer program
%type<unionvariant> union
%type<structvariant> struct
%type<enumvariant> enum
%type<declvariant> declarator declname param_decl sdecl
%type<funcvariant> function

%code {
  //ctx->layer = 0;
  //ctx->scopes = dactor(64);
  //SCOPE* rootscope = malloc(sizeof(SCOPE));
  //rootscope->identifiers = htctor();
  //rootscope->structs = htctor();
  //rootscope->unions = htctor();
  //rootscope->enums = htctor();
  //dapush(ctx->scopes, rootscope);
  //ctx->labels = htctor();
  //cs_inits is a dynarr of initializers
  //not sure what to do with typedefs
}

%%
program:
  function {$$ = dactor(4096); dapush($$, gtb(1, $1));}
| initializer {$$ = dactor(4096); for(int i = 0; i < $1->length; i++) dapush($$, gtb(1, daget($1, i))); free($1);}
| program function {$$ = $1; dapush($$, gtb(1, $2));}
| program initializer {$$ = $1; for(int i = 0; i < $1->length; i++) dapush($$, gtb(1, daget($2, i))); free($2);};
initializer:
"typedef" type/*bs*/ cs_minutes ';' {
  SCOPE* current = scopepeek(ctx);
  for(int i = 0; i < $3->length; i++) {
    aget($3, i)->decl->type->tb |= $2->tb; 
    //if($1->pointerstack->length) 
    aget($3, 0)->decl->type->pointerstack = damerge($2->pointerstack, aget($3, 0)->decl->type->pointerstack);
    add2scope(current, aget($3, i)->decl->varname, M_TYPEDEF, aget($3, i)->decl->type);
    free(aget($3, i));
    free(aget($3, i)->decl);
  }
  $$ = dactor(0);
}
| type/*bs*/ cs_inits ';' {
  SCOPE* current = scopepeek(ctx);
  $$ = $2;
  for(int i = 0; i < $$->length; i++) {
    aget($$, i)->decl->type->tb |= $1->tb; 
    //if($1->pointerstack->length) 
    aget($$, 0)->decl->type->pointerstack = damerge($1->pointerstack, aget($$, 0)->decl->type->pointerstack);
  }
};
cs_inits:
  cs_inits ',' declarator '=' esc {$$ = $1; dapush($$, geninit($3, $5));}
| declarator '=' esc {$$ = dactor(8); dapush($$, geninit($1, $3));}
| cs_inits ',' declarator {$$ = $1; dapush($$, geninit($3, NULL));}
| declarator {$$ = dactor(8); dapush($$, geninit($1, NULL));};
cs_minutes:
  cs_minutes ',' declarator {$$ = $1; dapush($1, $3);}
| declarator {$$ = dactor(8); dapush($$, $1);};
declarator:
  abstract_ptr declname {$$ = $2; $2->type->pointerstack = damerge($1, $2->type->pointerstack);}
| declname {$$ = $1;};
declname:
  IDENTIFIER {$$ = mkdeclaration($1);}
| '(' declarator ')' {$$ = $2;}
| declname '[' ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, NULL));}
| declname '[' expression ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, $3));}
| declname '(' ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, NULL));}
| declname '(' params ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, $3));};
params:
  param_decl {$$ = dactor(8); dapush($$, $1);}
| params ',' param_decl {$$ = $1; dapush($$, $3);};
param_decl:
  type/*bs*/ declarator {$$ = $2; $$->type->tb = $1->tb;/*TODO: THIS IS NOT RIGHT, I need proper type handling*/free($2);};
typem:
  "char" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1;}
| "int8" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1;}
| "int16" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2;}
| "int32" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4;}
| "int64" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8;}
| "byte"  {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1 | UNSIGNEDNUM;}
| "dbyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2 | UNSIGNEDNUM;}
| "qbyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4 | UNSIGNEDNUM;}
| "obyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8 | UNSIGNEDNUM;}
| "single" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4 | FLOATNUM;}
| "double" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8 | FLOATNUM;}
| "void" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = VOIDNUM;}
| "signed" {$$ = calloc(1,sizeof(IDTYPE)); $$->tb = 0;}
| "unsigned" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = UNSIGNEDNUM;}
| struct {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = STRUCTVAL; $$->structtype = $1;}
| union {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = UNIONVAL; $$->uniontype = $1;}
| enum {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = ENUMVAL; $$->enumtype = $1;};
types1:
  "const" {$$ = CONSTNUM;}
| "volatile" {$$ = VOLATILENUM;};
types2:
  "extern" {$$ = EXTERNNUM;}
| "static" {$$ = STATICNUM;};
typews1:
  TYPE_NAME {$$ = malloc(sizeof(IDTYPE)); SCOPEMEMBER* sm = ((SCOPEMEMBER*) search(((SCOPE*) dapeek(ctx->scopes))->members, $1)); memcpy($$, sm->typememb, sizeof(IDTYPE));/*TODO: extract, and duplicate*/ }
| typem {$$ = $1;}
| types1 typem {$$ = $2; $$->tb |= $1;}
| types1 TYPE_NAME {$$ = malloc(sizeof(IDTYPE)); SCOPEMEMBER* sm = ((SCOPEMEMBER*) search(((SCOPE*) dapeek(ctx->scopes))->members, $2)); memcpy($$, sm->typememb, sizeof(IDTYPE)); $$->tb |= 1;/*TODO: extract, and duplicate*/ }
| types2 typem {$$ = $2; $$->tb |= $1;}
| types2 TYPE_NAME {$$ = malloc(sizeof(IDTYPE)); SCOPEMEMBER* sm = ((SCOPEMEMBER*) search(((SCOPE*) dapeek(ctx->scopes))->members, $2)); memcpy($$, sm->typememb, sizeof(IDTYPE)); $$->tb |= 1;/*TODO: extract, and duplicate*/ };
/* typebs: */
/*   typem {$$ = $1;} */
/* | types1 typebs {$$ = $2; $$->tb |= $1;} */
/* | types2 typebs {$$ = $2; $$->tb |= $1;}; */
type:
  typews1 {$$ = $1;};
types1o:
  types1 {$$ = $1;}
| types1o types1 {$$ = $1 | $2;};
abstract_ptr:
  '*' {$$ = dactor(4); dapush($$,mkdeclptr(sizeof(intptr_t)));}
| '*' abstract_ptr {$$ = $2; dapush($$, mkdeclptr(sizeof(intptr_t)));}
| '*' types1o {$$ = dactor(4); dapush($$,mkdeclptr(sizeof(intptr_t) | $2));}
| '*' types1o abstract_ptr {$$ = $3; dapush($$, mkdeclptr(sizeof(intptr_t) | $2));};
expression:
  expression ',' esc {$$ = ct_binary_expr(COMMA, $1, $3);}
| esc {$$ = $1;};
esc:
  esc '=' esa {$$ = ct_binary_expr(ASSIGN, $1, $3);}
| esc "/=" esa {$$ = ct_binary_expr(DIVASSIGN, $1, $3);}
| esc "*=" esa {$$ = ct_binary_expr(MULTASSIGN, $1, $3);}
| esc "%=" esa {$$ = ct_binary_expr(MODASSIGN, $1, $3);}
| esc "+=" esa {$$ = ct_binary_expr(ADDASSIGN, $1, $3);}
| esc "-=" esa {$$ = ct_binary_expr(SUBASSIGN, $1, $3);}
| esc "<<=" esa {$$ = ct_binary_expr(SHLASSIGN, $1, $3);}
| esc ">>=" esa {$$ = ct_binary_expr(SHRASSIGN, $1, $3);}
| esc "&=" esa {$$ = ct_binary_expr(ANDASSIGN, $1, $3);}
| esc "^=" esa {$$ = ct_binary_expr(XORASSIGN, $1, $3);}
| esc "|=" esa {$$ = ct_binary_expr(ORASSIGN, $1, $3);}
| esa {$$ = $1;};
esa:
  est '?' expression ':' esa {$$ = ct_ternary_expr($1, $3, $5);}
| est {$$ = $1;};
est:
  est "||" eslo {$$ = ct_binary_expr(L_OR, $1, $3);}
| eslo {$$ = $1;};
eslo:
  eslo "&&" esla {$$ = ct_binary_expr(L_AND, $1, $3);}
| esla {$$ = $1;};
esla:
  esla '|' esbo {$$ = ct_binary_expr(B_OR, $1, $3);}
| esbo {$$ = $1;};
esbo:
  esbo '^' esbx {$$ = ct_binary_expr(B_XOR, $1, $3);}
| esbx {$$ = $1;};
esbx:
  esbx '&' esba {$$ = ct_binary_expr(B_AND, $1, $3);}
| esba {$$ = $1;};
esba:
  esba "==" eseq {$$ = ct_binary_expr(EQ, $1, $3);}
| esba "!=" eseq {$$ = ct_binary_expr(NEQ, $1, $3);}
| eseq {$$ = $1;};
eseq:
  eseq '<' escmp {$$ = ct_binary_expr(LT, $1, $3);}
| eseq '>' escmp {$$ = ct_binary_expr(GT, $1, $3);}
| eseq "<=" escmp {$$ = ct_binary_expr(LTE, $1, $3);}
| eseq ">=" escmp {$$ = ct_binary_expr(GTE, $1, $3);}
| escmp {$$ = $1;};
escmp:
  escmp "<<" essh {$$ = ct_binary_expr(SHL, $1, $3);}
| escmp ">>" essh {$$ = ct_binary_expr(SHR, $1, $3);}
| essh;
essh:
  essh '+' esas {$$ = ct_binary_expr(ADD, $1, $3);}
| essh '-' esas {$$ = ct_binary_expr(SUB, $1, $3);}
| esas {$$ = $1;};
esas:
  esas '*' esm {$$ = ct_binary_expr(MULT, $1, $3);}
| esas '/' esm {$$ = ct_binary_expr(DIVI, $1, $3);}
| esas '%' esm {$$ = ct_binary_expr(MOD, $1, $3);}
| esm {$$ = $1;};
esm:
  '(' type ')' esm {$$ = ct_cast_expr($2, $4);}
| esca {$$ = $1;};
esca:
  "++" esca {$$ = ct_unary_expr(PREINC, $2);}
| "--" esca {$$ = ct_unary_expr(PREDEC, $2);}
| '+' esm {$$ = ct_unary_expr(IDENT, $2);}
| '-' esm {$$ = ct_unary_expr(NEG, $2);}
| '!' esm {$$ = ct_unary_expr(L_NOT, $2);}
| '~' esm {$$ = ct_unary_expr(B_NOT, $2);}
| '*' esm {$$ = ct_unary_expr(DEREF, $2);}
| '&' esm {$$ = ct_unary_expr(ADDR, $2);}
| "sizeof" '(' type ')' {$$ = ct_sztype($3);}
| "sizeof" esca {$$ = ct_unary_expr(SZOFEXPR,$2);}
| esp {$$ = $1;};
esp:
  esp "++" {$$ = ct_unary_expr(POSTINC, $1);}
| esp "--" {$$ = ct_unary_expr(POSTDEC, $1);}
| esp '(' ')' {$$ = ct_fcall_expr($1,0,NULL);}
| esp '(' escl ')' {$$ = ct_fcall_expr($1,$3->length,(EXPRESSION*)*$3->arr); free($3);}
| esp '[' expression ']' {$$ = ct_unary_expr(DEREF, ct_binary_expr(ADD, $1, $3));}
| esp '.' IDENTIFIER {$$ = ct_binary_expr(DOTOP, $1, ct_ident_expr($3));}
| esp  "->" IDENTIFIER {$$ = ct_binary_expr(ARROW, $1, ct_ident_expr($3));}
| esu {$$ = $1;};
esu:
  '(' expression ')' {$$ = $2;}
| STRING_LITERAL {$$ = ct_strconst_expr($1/*.str*/);}
| WSTRING_LITERAL {$$ = ct_wstrconst_expr($1/*.str*/);}
| INTEGER_LITERAL {$$ = $1.sign ? ct_intconst_expr($1.num) : ct_uintconst_expr($1.num);}
| FLOAT_LITERAL {$$ = ct_floatconst_expr($1/*.dbl*/);}
| IDENTIFIER {$$ = ct_ident_expr($1/*.str*/);};
escl:
  esc {$$ = dactor(32); dapush($$, $1);}
| escl ',' esc {$$ = $1; dapush($$, $3); };

function:
  type/*bs*/ declname compound_statement {struct declarator_part* dp = dapop($2->type->pointerstack); $1->pointerstack = damerge($1->pointerstack, $2->type->pointerstack);/*TODO: have this extract only pointers and arrays and not i.e. params)*/ $$ = ct_function($2->varname, $3, dp->params, $1); free($2);/*check that it is in fact a param spec*/};
statement:
  compound_statement {$$ = $1;}
|  IDENTIFIER ':' /*statement*/ {$$ = mklblstmt($1/*, $3*/);}
| "case" esc ':' /*statement*/ {$$ = mkcasestmt($2/*, $4*/);}
| "default" ':' statement {$$ = mkdefaultstmt($3);}
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7);}
| "switch" '(' expression ')' statement {$$ = mklsstmt(SWITCH, $3, $5);}
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2);}
| "for" '(' ee ';' ee ';' ee ')' statement {$$ = mkforstmt($3, $5, $7, $9);}
| "goto" IDENTIFIER ';' {$$ = mkgotostmt($2/*.yytext*/);/*find label within scopes at some point, probably not now though*/}
| "break" ';' {$$ = mkexprstmt(LBREAK,NULL);}
| "continue" ';' {$$ = mkexprstmt(LCONT,NULL);}
| "return" ';' {$$ = mkexprstmt(FRET,NULL);}
| "return" expression ';' {$$ = mkexprstmt(FRET,$2);}
| expression ';' {$$ = mkexprstmt(EXPR,$1);}
| ';' {$$ = mkexprstmt(NOPSTMT, NULL);};
ee: 
  expression {$$ = $1;}
| %empty {$$ = NULL;};
compound_statement:/*add new scope to scope stack, remove when done*/
  '{' '}' {$$ = mkcmpndstmt(NULL);}
| '{' statements_and_initializers '}' {scopepush(ctx); $$ = mkcmpndstmt($2); scopepop(ctx);};
statements_and_initializers:
  initializer {$$ = dactor(256); dapush($$,soii($1));}
| statement {$$ = dactor(256); dapush($$,sois($1));}
| statements_and_initializers initializer {$$ = $1; dapush($$,soii($2));}
| statements_and_initializers statement {$$ = $1; dapush($$,sois($2));};

union:
  "union" IDENTIFIER '{' struct_decls '}' {$$ = unionctor($2/*.yytext*/, $4);}
| "union" '{' struct_decls '}' {$$ = unionctor(NULL, $3);}
| "union" IDENTIFIER {$$ = unionctor($2/*.yytext*/, NULL);};
struct:
  "struct" IDENTIFIER '{' struct_decls '}' {$$ = structor($2/*.yytext*/, $4);}
| "struct" '{' struct_decls '}' {$$ = structor(NULL, $3);}
| "struct" IDENTIFIER {$$ = structor($2/*.yytext*/, NULL);};
struct_decls:
  struct_decl {$$ = $1;}
| struct_decls struct_decl {$$ = damerge($1, $2);};
struct_decl:
  type cs_decls ';' {$$ = $2; for(int i = 0; i < $2->length; i++) dget($2, i)->type->tb |= $1->tb; if($1->pointerstack->length) dget($$, 0)->type->pointerstack = damerge($1->pointerstack, dget($$, 0)->type->pointerstack);};
cs_decls:
  cs_decls ',' sdecl {$$ = $1; dapush($$, $3);}
| sdecl {$$ = dactor(8); dapush($$, $1);};
sdecl: 
  declarator {$$ = $1;}
| declarator ':' esc {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $3));}
| ':' esc {$$ = mkdeclaration(NULL); dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $2));};
enum:
  "enum" IDENTIFIER '{' enums '}' {$$ = enumctor($2/*.yytext*/, $4);}
| "enum" '{' enums '}' {$$ = enumctor(NULL, $3);}
| "enum" IDENTIFIER {$$ = enumctor($2/*.yytext*/, NULL);};
enums:
  IDENTIFIER {$$ = dactor(256);dapush($$, genenumfield($1/*.yytext*/,ct_intconst_expr(0)));}
| IDENTIFIER '=' esc {$$ = dactor(256); dapush($$, genenumfield($1/*.yytext*/,$3));}
| enums ',' IDENTIFIER {$$ = $1; dapush($$, genenumfield($3/*.yytext*/,ct_binary_expr(ADD,ct_intconst_expr(1),dapeek($$))));}
| enums ',' IDENTIFIER '=' esc {$$ = $1; dapush($$, genenumfield($3/*.yytext*/,$5));};
%%
#include <stdio.h>
int yyerror(char* s){
  //printf("\ncolumn: %d\n%s\n", yylloc->first_column, s);
  return 0;
}

