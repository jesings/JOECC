%locations
%token ARROW "->" INC "++" DEC "--" SHL "<<" SHR ">>" LE "<=" GE ">=" EQ "==" 
%token NEQ "!=" AND "&&" OR "||" DIV_GETS "/=" MUL_GETS "*=" MOD_GETS "%=" 
%token ADD_GETS "+=" SUB_GETS "-=" SHL_GETS "<<=" SHR_GETS ">>=" AND_GETS "&=" 
%token XOR_GETS "^=" OR_GETS "|="

%token TYPEDEF "typedef" STATIC "static" EXTERN "extern" CHAR "char"
%token INT8 "int8" INT16 "int16" INT32 "int32" INT64 "int64" BYTE "byte"
%token DBYTE "dbyte" QBYTE "qbyte" OBYTE "obyte" SINGLE "single" DOUBLE "double" 
%token CASE "case" DEFAULT "default" IF "if" ELSE "else" SWITCH "switch" 
%token WHILE "while" DO "do" FOR "for" GOTO "goto" CONTINUE "continue" 
%token BREAK "break" RETURN "return" SIZEOF "sizeof" UNSIGNED "unsigned"
%token STRUCT "struct" ENUM "enum" UNION "union" SIGNED "signed" 

%right THEN "else"
%start program
%define parse.assert
%define parse.error verbose

%union {
  struct intinfo{
    long num;
    char sign;
  }; 
  char* str;
  double dbl;
}
%token<struct intinfo> INT;
%token<dbl> FLOAT;
%token<str> IDENTIFIER STRING_LITERAL TYPE_NAME;

%code requires{
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "compintern.h"
#include "hash.h"
#include "dynarr.h"

struct expr;
typedef enum{
  UNDEFINED,
  FUNCTION,
  PARAMETER,
  LOCAL_VAR,
  GLOBAL_VAR
} IDTYPE1;
struct idtypeval;
typedef struct {
  struct idtypeval* type;
  struct expr* bitflen;
  //int index;//index of type within scope (i.e. parameter index)
  char* name;
} IDENTIFIER;
IDENTIFIER* genident(char* name, struct idtypeval* id){
  IDENTIFIER* retval = malloc(sizeof(IDENTIFIER));
  retval->name = name;
  retval->type = id;
  retval->bitflen = 0;
  return retval;
}
IDENTIFIER* genidentbit(char* name, struct expr* expret){
  IDENTIFIER* retval = malloc(sizeof(IDENTIFIER));
  retval->name = name;
  retval->type = 0;
  retval->bitflen = expret;
  return retval;
}

int* allocnum(int i){
  int* j = malloc(sizeof(int));
  *j = i;
  return j;
}

//typedef enum{
//  VOID, CHAR, INT8, INT16, INT32, INT64, BYTE, DBYTE, QBYTE, OBYTE, SINGLE, DOUBLE, STRUCT, UNION, ENUM
//} VARTYPE;
//typedef enum{
//  NONE, CONST, VOLATILE
//} TYPESPEC;
//typedef enum{
//  EXTERN, STATIC, PARAM, SIGNED, UNSIGNED
//} TYPEQUAL;

typedef struct{
  DYNARR* fields;//Each entry is a struct that contains a full identifier and a size
  char* name;
} STRUCTDEF;
typedef struct{
  DYNARR* fields;//Each entry is a struct that contains a full identifier and a size
  char* name;
} UNIONDEF;
typedef struct{
  DYNARR* fields;//Each entry is a string-int hybrid struct
  char* name;
} ENUMDEF;

struct structdef;
typedef struct{
  DYNARR* pointerstack;
  TYPEBITS tb;
  union{
    STRUCTDEF structtype;
    UNIONDEF uniontype;
    ENUMDEF enumtype;
  };
} IDTYPE;

typedef enum{
  NOP, STRING, INT, UINT, FLOAT, IDENT,
  ADD, NEG, SUB, EQ, NEQ, GT, LT, GTE, LTE, MULT, DIVI, MOD,
  PREINC, POSTINC, PREDEC, POSTDEC,
  L_AND, L_OR, L_NOT, B_AND, B_OR, B_XOR, B_NOT, SHL, SHR,
  DOTOP, ARROW,
  SZOF, SZOFEXPR,
  ASSIGN.
  ADDASSIGN, SUBASSIGN, SHLASSIGN, SHRASSIGN, ANDASSIGN, 
  XORASSIGN, ORASSIGN, DIVASSIGN, MULTASSIGN, MODASSIGN,
  SWITCHS, CASES,
  CAST,
  COMMA,
  WHILEL, FORL, DOWHILE, 
  ADDR, DEREF, 
  FCALL,FCOPY,
  TERNARY
} EXPTYPE;

struct function;
typedef struct expr{
  EXPRTYPE type;
  union{
    struct{
      int numparams;
      struct expr* params;
      struct function* func;
    };
    struct{
      struct expr* param1;
      struct expr* param2;
    }
    struct{
      struct expr* ifexpr;
      struct expr* thenexpr;
      struct expr* elseexpr;
    }
    struct{
      IDTYPE* casttype;
      struct expr* castexpr;
    }
    struct expr* unaryparam;
    char* strconst;
    long intconst;
    long uintconst;
    double floatconst;
    IDENTIFIER id;
    DYNARR* dynvals;
    /*possible struct const for later struct initializations*/
  };
} EXPRESSION;

typedef struct{
  char* name;
  DYNARR* fields;
} STRUCT;
STRUCT* structor(char* name, DYNARR* fields){
    STRUCT* retval = malloc(sizeof(STRUCT));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

typedef struct{
  char* name;
  DYNARR* fields;
} UNION;
UNION* unionctor(char* name, DYNARR* fields){
    STRUCT* retval = malloc(sizeof(UNION));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

typedef struct{
  char* name;
  DYNARR* fields;
} ENUM;
ENUM* enumctor(char* name, DYNARR* fields){
    STRUCT* retval = malloc(sizeof(ENUM));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

typedef struct function{
  char* name;
  STATEMENT* code;
  unsigned int num_vars, num_params;
  VARTYPE retType;
} FUNC;
struct lexctx;

EXPRESSION* cloneexpr(EXPRESSION* orig){
  EXPRESSION* clone = malloc(sizeof(EXPRESSION));
  memcpy(clone, orig, sizeof(EXPRESSION));
  return clone;
}

EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION* param){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->unaryparam = param;
  return retval;
}
EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->param1 = param1
  retval->param2 = param2;
  return retval;
}
EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr ){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = CAST;
  retval->castexpr = expr;
  retval->casttype = type;
  return retval;
}
EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = TERNARY;
  retval->ifexpr = param1
  retval->thenexpr = param2;
  retval->elseexpr = param2;
  return retval;
}
EXPRESSION* ct_fcall_expr(FUNCTION* func, int num, EXPRESSION** params){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FCALL;
  retval->func = func;
  retval->numparams = num;
  retval->params = params;
  return retval;
}
EXPRESSION* ct_strconst_expr(EXPRTYPE t, char* str){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->strconst = strdup(str);//may or may not need to duplicate
  return retval;
}
EXPRESSION* ct_intconst_expr(long num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->intconst = num;
  return retval;
}
EXPRESSION* ct_uintconst_expr(unsigned long num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->uintconst = num;
  return retval;
}
EXPRESSION* ct_floatconst_expr(EXPRTYPE t, double num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = FLOAT;
  retval->floatconst = num;
  return retval;
}
EXPRESSION* ct_ident_expr(IDENTIFIER* id){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = IDENT;
  retval->id = id;
  return retval;
}

enum stmttype{
  FRET, LBREAK, JGOTO, LCONT,
  FORL, WHILEL, DOWHILEL,
  IFS,
  SWITCH,
  CASE, LABEL,
  CMPND,
  EXPR, NOPSTMT
};
struct stmt;
typedef struct{
  char isstmt;
  union{
    struct stmt* state;
    INITIALIZER* init;
  }
} SOI;
SOI* sois(struct stmt* state){
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 1;
  retval->state = state;
  return retval;
}
SOI* soii(INITIALIZER* init){
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 0;
  retval->init = init;
  return retval;
}

typedef struct stmt{
  enum stmttype type;
  union{
    EXPRESSION* expression;
    struct{
      EXPRESSION* ifcond;
      struct stmt* thencond;
      struct stmt* elsecond;
    };
    struct{
      EXPRESSION* cond;
      struct stmt* body;
    };
    struct{
      EXPRESSION* init;
      EXPRESSION* forcond;
      EXPRESSION* increment;
      struct stmt* forbody;
    };
    IDENTIFIER* gotoloc;
    DYNARR* stmtsandinits;
    STATEMENT* substatement;
  }
} STATEMENT;

STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->expression = express;
  return retval;
}
STATEMENT* mkgotostmt(IDENTIFIER* gotoloc){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = JGOTO;
  retval->gotoloc = gotoloc;
  return retval;
}
STATEMENT* mkforstmt(EXPRESSION* e1, EXPRESSION* e2, EXPRESSION* e3, STATEMENT* bdy){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = FORL;
  retval->init = e1;
  retval->forcond = e2;
  retval->increment = e3;
  retval->forbody = bdy;
  return retval;
}
STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->cond = condition;
  retval->body = bdy;
  return retval;
}
STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = IFS;
  retval->ifcond = condition;
  retval->thencond = ifbdy;
  retval->elsecond = elsebdy;
  return retval;
}
STATEMENT* mkcmpndstmt(DYNARR* stmtsandinits){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CMPND;
  retval->stmtsandinits = stmtsandinits;
  return retval;
}
STATEMENT* mklblstmt(IDENTIFIER* identifier){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = LABEL;
  retval->label = identifier;
  return retval;
}
STATEMENT* mkcasestmt(EXPRESSION* casexpr, STATEMENT* stmt){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CASE;
  retval->cond = casexpr;
  retval->body = stmt;
  return retval;
}
STATEMENT* mkdefaultstmt(STATEMENT* stmt){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = DEFAULT;
  retval->substatement = stmt;
  return retval;
}

typedef struct{
  char* name;
  long value;
} ENUMFIELD;
ENUMFIELD* genenumfield(char* name, STATEMENT* value){
  ENUMFIELD retval = malloc(sizeof(ENUMFIELD));
  retval->name = name;
  retval->value = value;
  return retval;
}

typedef struct{
  EXPRESSION* expr;
} ARRAYSPEC;

enum declpart_info{
  POINTERSPEC, ARRAYSPEC, PARAMSSPEC
};
struct declarator_part{
  enum declpart_info type;
  union decloptions{
    DYNARR* params;
    EXPRESSION* arrspec;
    DYNARR* ptrspec;
  };
};
struct declarator_part* mkdeclpart(enum declpart_info typ, union decloptions d){
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = typ;
  retval->decloptions = d;
}
typedef struct{
  DYNARR* declparts;
  char* idname;
  TYPEBITS tb;
} DECLARATOR;
DECLARATOR* mkdeclarator(char* name){
  DECLARATOR* retval = malloc(sizeof(declarator));
  retval->idname = name;
  retval->declparts = dactor(4);
  return retval;
}

typedef struct{
  char* varname;
  IDTYPE* type;
} DECLARATION;
DECLARATION* mkdeclaration(char* name){
  DECLARATOR* retval = malloc(sizeof(declarator));
  retval->idname = name;
  retval->declparts = dactor(4);
  return retval;
}
EXPRESSION* exprfromdecl(char* name, IDTYPE* id){
  EXPRESSION* outer = ct_ident_expr(name);
  DYNARR* ptrs = IDTYPE->pointerstack;
  if(ptrs)
    for(int i = 0; i<ptrs->length; i++){
    }
}


typedef struct{
  DECLARATION* declaration;
  EXPRESSION* assign;
} INITBITS;

struct lexctx{
  FUNCTION* funclist;
  unsigned int fllast, fllen;
  FUNCTION* curfunc;
  DYNARR* scopes;
  unsigned int layer;//Necessary?
  HASHTABLE* labels;
};
typedef struct{
  HASHTABLE* identifiers;
  HASHTABLE* structs;
  HASHTABLE* unions;
  HASHTABLE* enums;
  HASHTABLE* typedefs;
} SCOPE;
SCOPE* mkscope(SCOPE* parent){
  SCOPE* child = malloc(sizeof(SCOPE));
  child->identifiers = htclone(parent->identifiers);
  child->structs = htclone(parent->structs);
  child->unions = htclone(parent->unions);
  child->enums = htclone(parent->enums);
  return child;
}
int scopepush(struct lexctx* ctx){
  SCOPE* child = mkscope(dapeek(ctx->scopes));
}
}

%param {struct lexctx* ctx}
%type<TYPEBITS> type types1 types2 types1o
%type<EXPRESSION*> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee
%type<IDTYPE*> typem typews1 typebs
%type<IDENTIFIER*> sdecl initializer
%type<STATEMENT*> statement compound_statement
%type<DYNARR*> statements_and_initializers struct_decls struct_decl cs_decls enums escl abstract_ptr params cs_inits 
%type<UNION*> union
%type<STRUCT*> struct
%type<ENUM*> enum
%type<DECLARATOR*> declarator declname param_decl

%code {
  ctx->layer = 0;
  ctx->scopes = dactor(64);
  SCOPE* rootscope = malloc(sizeof(SCOPE));
  rootscope->identifiers = htctor();
  rootscope->structs = htctor();
  rootscope->unions = htctor();
  rootscope->enums = htctor();
  dapush(ctx->scopes, rootscope);
  ctx->labels = htctor();
}

%%
program:
  function 
| initializer
| program function
| program initializer;
initializer:
  "typedef" typebs cs_minutes ';'
| typebs cs_inits ';';
cs_inits:
  declarator '=' esc ',' cs_inits
| declarator '=' esc
| declarator ',' cs_inits
| declarator;
cs_minutes:
  declarator ',' cs_minutes
| declarator;
declarator:
  abstract_ptr declname {$$ = $2; $2->declparts = damerge($2->declparts, $1);}
| declname {$$ = $1;};
declname:
  IDENTIFIER {$$ = mkdeclarator($1.str);}
| '(' declarator ')' {$$ = $2;}
| declname '[' ']' {$$ = mkdeclpart(ARRAYSPEC, NULL);}
| declname '[' expression ']' {$$ = mkdeclpart(ARRAYSPEC, $3);}
| declname '(' ')' {$$ = mkdeclpart(PARAMSSPEC, NULL);}
| declname '(' params ')' {$$ = mkdeclpart(PARAMSSPEC, $3);};
params:
  param_decl {$$ = dactor(8); dapush($$, $1);}
| params ',' param_decl {$$ = $1; dapush($$, $3);};
param_decl:
  typebs declarator {$$ = $2; $$->typebs = $1;};
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
| "void" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = VOIDVAL;}
| "signed" {$$ = calloc(1,sizeof(IDTYPE));}
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
  TYPE_NAME {$$ = search(ctx->types, $1.yytext);}
| typem {$$ = $1;}
| types1 typews1 {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = $1 | $2;};
typebs:
  typem {$$ = $1;}
| types1 typebs {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = $1 | $2;}
| types2 typebs {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = $1 | $2;};
type:
  typews1 {$$ = $1;};
types1o:
  types1 {$$ = $1}
| types1o types1 {$$ = $1 | $2;};
abstract_ptr:
  '*' {$$ = dactor(4); dapush($$,allocnum(sizeof(intptr_t)));}
| '*' abstract_ptr {$$ = $2; dapush($$, allocnum(sizeof(intptr_t)));}
| '*' types1o {$$ = dactor(4); dapush($$,allocnum(sizeof(intptr_t) | $2));}
| '*' types1o abstract_ptr {$$ = $3; dapush($$, allocnum(sizeof(intptr_t) | $2));};
expression:
  expression ',' esc {$$ = ct_binary_expr(COMMA, $1, $3);}
| esc {$$ = $1};
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
| esa {$$ = $1};
esa:
  est '?' expression ':' esa {$$ = ct_ternary_expr($1, $3, $5);}
| est {$$ = $1};
est:
  est "||" eslo {$$ = ct_binary_expr(L_OR, $1, $3);}
| eslo {$$ = $1};
eslo:
  eslo "&&" esla {$$ = ct_binary_expr(L_AND, $1, $3);}
| esla {$$ = $1};
esla:
  esla '|' esbo {$$ = ct_binary_expr(B_OR, $1, $3);}
| esbo {$$ = $1};
esbo:
  esbo '^' esbx {$$ = ct_binary_expr(B_XOR, $1, $3);}
| esbx {$$ = $1};
esbx:
  esbx '&' esba {$$ = ct_binary_expr(B_AND, $1, $3);}
| esba {$$ = $1};
esba:
  esba "==" eseq {$$ = ct_binary_expr(EQ, $1, $3);}
| esba "!=" eseq {$$ = ct_binary_expr(NEQ, $1, $3);}
| eseq {$$ = $1};
eseq:
  eseq '<' escmp {$$ = ct_binary_expr(LT, $1, $3);}
| eseq '>' escmp {$$ = ct_binary_expr(GT, $1, $3);}
| eseq "<=" escmp {$$ = ct_binary_expr(LTE, $1, $3);}
| eseq ">=" escmp {$$ = ct_binary_expr(GTE, $1, $3);}
| escmp {$$ = $1};
escmp:
  escmp "<<" essh {$$ = ct_binary_expr(SHL, $1, $3);}
| escmp ">>" essh {$$ = ct_binary_expr(SHR, $1, $3);}
| essh;
essh:
  essh '+' esas {$$ = ct_binary_expr(ADD, $1, $3);}
| essh '-' esas {$$ = ct_binary_expr(SUB, $1, $3);}
| esas {$$ = $1};
esas:
  esas '*' esm {$$ = ct_binary_expr(MULT, $1, $3);}
| esas '/' esm {$$ = ct_binary_expr(DIVI, $1, $3);}
| esas '%' esm {$$ = ct_binary_expr(MOD, $1, $3);}
| esm {$$ = $1};
esm:
  '(' type ')' esm {$$ = ct_cast_expr($2, $4);}
| esca {$$ = $1};
esca:
  "++" esca {$$ = ct_unary_expr(PREINC, $2);}
| "--" esca {$$ = ct_unary_expr(PREDEC, $2);}
| '+' esm {$$ = ct_unary_expr(IDENT, $2);}
| '-' esm {$$ = ct_unary_expr(NEG, $2);}
| '!' esm {$$ = ct_unary_expr(L_NOT, $2);}
| '~' esm {$$ = ct_unary_expr(B_NOT, $2);}
| '*' esm {$$ = ct_unary_expr(DEREF, $2);}
| '&' esm {$$ = ct_unary_expr(ADDR, $2);}
| "sizeof" '(' type ')' {$$ = ct_unary_expr(SZOF,$3);}
| "sizeof" esca {$$ = ct_unary_expr(SZOFEXPR,$2);}
| esp {$$ = $1};
esp:
  esp "++" {$$ = ct_unary_expr(POSTINC, $1);}
| esp "--" {$$ = ct_unary_expr(POSTDEC, $1);}
| esp '(' ')' {$$ = ct_nary_expr($1,0,NULL);}
| esp '(' escl ')' {$$ = ct_fcall_expr($1,$3->numargs,$3->argl);}
| esp '[' expression ']' {$$ = ct_unary_expr(DEREF, ct_binary_expr(ADD, $1, $3));}
| esp '.' IDENTIFIER {$$ = ct_binary_expr(DOTOP, $1, ct_ident_expr($3));}
| esp  "->" IDENTIFIER {$$ = ct_binary_expr(ARROW, $1, ct_ident_expr($3));}
| esu {$$ = $1};
esu:
  '(' expression ')' {$$ = $2;}
| STRING_LITERAL {$$ = ct_strconst_expr($1.str);}
| INT {$$ = $1.sign ? ct_intconst_expr($1.num) : ct_uintconst_expr($1.num);}
| FLOAT {$$ = ct_floatconst_expr($1.dbl);}
| IDENTIFIER {$$ = ct_ident_expr($1.str);};
escl:
  esc {$$ = dactor(32); dapush($$, $1);}
| escl ',' esc {$$ = $1; dapush($$, $3); };

function:
  typebs declarator compound_statement;
statement:
  compound_statement {$$ = $1}
|  IDENTIFIER ':' statement {$$ = mklblstmt($1, $3);}
| "case" esc ':' statement {$$ = mkcasestmt($2, $4);}
| "default" ':' statement {$$ = mkdefaultstmt($3);}
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7);}
| "switch" '(' expression ')' statement {$$ = mklsstmt(SWITCH, $3, $5);}
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2);}
| "for" '(' ee ';' ee ';' ee ')' statement {$$ = mkforstmt($3, $5, $7, $9);}
| "goto" IDENTIFIER ';' {$$ = mkgotostmt($2.yytext);/*find label within scopes at some point, probably not now though*/}
| "break" ';' {$$ = mkexprstmt(LBREAK,NULL);}
| "continue" ';' {$$ = mkexprstmt(LCONT,NULL);}
| "return" ';' {$$ = mkexprstmt(FRET,NULL);}
| "return" expression ';' {$$ = mkexprstmt(FRET,$2);}
| expression ';' {$$ = mkexprstmt(EXPR,$1);}
| ';' {$$ = mkexprstmt(NOPSTMT, NULL);};
ee: 
  expression {$$ = $1}
| %empty {$$ = NULL};
compound_statement:/*add new scope to scope stack here*/
  '{' '}' {$$ = mkcmpndstmt(NULL);}
| '{' statements_and_initializers '}' {$$ = mkcmpndstmt($2);};
statements_and_initializers:
  initializer {$$ = dactor(256); dapush($$,soii($1));}
| statement {$$ = dactor(256); dapush($$,sois($1));}
| statements_and_initializers initializer {$$ = $1; dapush($$,soii($2));}
| statements_and_initializers statement {$$ = $1; dapush($$,sois($2));};

union:
  "union" IDENTIFIER '{' struct_decls '}' {$$ = unionctor($2.yytext, $4);}
| "union" '{' struct_decls '}' {$$ = unionctor(NULL, $3);}
| "union" IDENTIFIER {$$ = unionctor($2.yytext, NULL);};
struct:
  "struct" IDENTIFIER '{' struct_decls '}' {$$ = structor($2.yytext, $4);}
| "struct" '{' struct_decls '}' {$$ = structor(NULL, $3);}
| "struct" IDENTIFIER {$$ = structor($2.yytext, NULL);};
struct_decls:
  struct_decl {$$ = $1;}
| struct_decls struct_decl {$$ = damerge($1, $2);};
struct_decl:
  type cs_decls ';' {$$ = $2; for(int i = 0; i < $2->length; i++) $2->arr[i]->type |= $1;};
cs_decls:
  sdecl ',' cs_decls {$$ = $3; dapush($$, $1);}
| sdecl {$$ = dactor(8); dapush($$, $1);};
sdecl: 
  declarator {$$ = genident($1, 0);}
| declarator ':' esc {$$ = genidentbit($1, $3);}
| ':' esc {$$ = genidentbit(NULL, $2);};
enum:
  "enum" IDENTIFIER '{' enums '}' {$$ = enumctor($2.yytext, $4);}
| "enum" '{' enums '}' {$$ = enumctor(NULL, $3);}
| "enum" IDENTIFIER {$$ = enumctor($2.yytext, NULL);};
enums:
  IDENTIFIER {$$ = dactor(256);$$.dapush(genenumfield($1.yytext,ct_intconst_expr(0)));}
| IDENTIFIER '=' esc {$$ = dactor(256); $$.dapush(genenumfield($1.yytext,$3));}
| enums ',' IDENTIFIER {$$ = $1; $$.dapush(genenumfield($3.yytext,ct_binary_expr(ADD,ct_intconst_expr(1),dapeek($$))));}
| enums ',' IDENTIFIER '=' esc {$$ = $1; $$.dapush(genenumfield($3.yytext,$5));};
%%
#include <stdio.h>
int yyerror(char* s){
    printf("\ncolumn: %d\n%s\n", yylloc->first_column, s);
}

