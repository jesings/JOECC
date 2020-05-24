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

%code requires{
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "compintern.h"

struct expr;

enum ident_type{
  UNDEFINED,
  FUNCTION,
  PARAMETER,
  LOCAL_VAR,
  GLOBAL_VAR,
  TYPE_DEFN
};

typedef struct {
  //int index;//index of type within scope (i.e. parameter index)
  //value perhaps?
  char* name;
} IDENTIFIERINFO;

typedef enum{
  NOP, STRING, INT, UINT, FLOAT, IDENT,
  ADD, NEG, SUB, EQ, NEQ, GT, LT, GTE, LTE, MULT, DIVI, MOD,
  PREINC, POSTINC, PREDEC, POSTDEC,
  L_AND, L_OR, L_NOT, B_AND, B_OR, B_XOR, B_NOT, SHL, SHR,
  DOTOP, ARROW,
  SZOF, SZOFEXPR,
  ASSIGN,
  ADDASSIGN, SUBASSIGN, SHLASSIGN, SHRASSIGN, ANDASSIGN, 
  XORASSIGN, ORASSIGN, DIVASSIGN, MULTASSIGN, MODASSIGN,
  CASES,
  CAST,
  COMMA,
  ADDR, DEREF,
  FCALL,FCOPY,
  TERNARY
} EXPRTYPE;

typedef struct expr{
  EXPRTYPE type;
  union{
    struct{
      int numparams;
      struct expr* params;
      struct expr* ftocall;
    };
    struct{
      struct expr* param1;
      struct expr* param2;
    };
    struct{
      struct expr* ifexpr;
      struct expr* thenexpr;
      struct expr* elseexpr;
    };
    struct{
      IDTYPE* casttype;
      struct expr* castexpr;
    };
    struct expr* unaryparam;
    char* strconst;
    char* ident;
    long intconst;
    unsigned long uintconst;
    double floatconst;
    IDENTIFIERINFO* id;//for identifier expressions???????
    IDTYPE* typesz;//for sizeof
    DYNARR* dynvals;//?
    /*possible struct const for later struct initializations*/
  };
} EXPRESSION;

STRUCT* structor(char* name, DYNARR* fields){
    STRUCT* retval = malloc(sizeof(STRUCT));
    retval->name = name;
    retval->fields = fields;
    return retval;
}
UNION* unionctor(char* name, DYNARR* fields){
    UNION* retval = malloc(sizeof(UNION));
    retval->name = name;
    retval->fields = fields;
    return retval;
}
ENUM* enumctor(char* name, DYNARR* fields){
    ENUM* retval = malloc(sizeof(ENUM));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

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
EXPRESSION* ct_sztype(IDTYPE* whichtype){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = SZOF;
  retval->typesz = whichtype;
  return retval;
}
EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));

  retval->type = t;
  retval->param1 = param1;
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
  retval->ifexpr = param1;
  retval->thenexpr = param2;
  retval->elseexpr = param2;
  return retval;
}

EXPRESSION* ct_fcall_expr(EXPRESSION* func, int num, EXPRESSION* params){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FCALL;
  retval->ftocall = func;
  retval->numparams = num;
  retval->params = params;
  return retval;
}
EXPRESSION* ct_strconst_expr(char* str){
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
EXPRESSION* ct_floatconst_expr(double num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = FLOAT;
  retval->floatconst = num;
  return retval;
}
EXPRESSION* ct_ident_expr(/*IDENTIFIERINFO* id*/ char* ident){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = IDENT;
  retval->ident = ident;
  return retval;
}

enum stmttype{
  FRET, LBREAK, JGOTO, LCONT,
  FORL, WHILEL, DOWHILEL,
  IFS, IFELSES,
  SWITCH,
  CASE, LABEL,
  CMPND,
  EXPR, NOPSTMT,
  DEFAULT
};

typedef struct{
  DYNARR* declparts;
  char* idname;
  TYPEBITS tb;
} DECLARATOR;
DECLARATOR* mkdeclarator(char* name){
  DECLARATOR* retval = calloc(1,sizeof(DECLARATOR));
  retval->idname = name;
  retval->declparts = dactor(4);
  return retval;
}

typedef struct {
  DECLARATOR* decl;
  EXPRESSION* expr;//null if it's only a declaration not an initialization as well
} INITIALIZER;

INITIALIZER* geninit(DECLARATOR* decl, EXPRESSION* expr) {
  INITIALIZER* retval = malloc(sizeof(INITIALIZER));
  retval->decl = decl;
  retval->expr = expr;
  return retval;
}

struct stmt;
typedef struct{
  char isstmt;
  union{
    struct stmt* state;
    DYNARR* init;
  };
} SOI;

SOI* sois(struct stmt* state){
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 1;
  retval->state = state;
  return retval;
}
SOI* soii(DYNARR* init){
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 0;
  retval->init = init;
  return retval;
}

typedef struct stmt{
  enum stmttype type;
  union{
    EXPRESSION* expression;
    struct{ //if or if/else
      EXPRESSION* ifcond;
      struct stmt* thencond;
      struct stmt* elsecond;
    };
    struct{ //while or dowhile or switch(?)
      EXPRESSION* cond;
      struct stmt* body;
    };
    struct{ //for
      EXPRESSION* init;
      EXPRESSION* forcond;
      EXPRESSION* increment;
      struct stmt* forbody;
    };
    IDENTIFIERINFO* label; //case or label, maybe also goto?
    char* glabel; //for label and goto
    DYNARR* stmtsandinits; //compound
    struct stmt* substatement; //default
  };
} STATEMENT;

STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->expression = express;
  return retval;
}
STATEMENT* mkgotostmt(char* gotoloc){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = JGOTO;
  retval->glabel = gotoloc;
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
STATEMENT* mklblstmt(char* identifier){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = LABEL;
  retval->glabel = identifier;
  return retval;
}
STATEMENT* mkcasestmt(EXPRESSION* casexpr/*, STATEMENT* stmt*/){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CASE;
  retval->cond = casexpr;
  //retval->body = stmt;
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
  EXPRESSION* value;
} ENUMFIELD;
ENUMFIELD* genenumfield(char* name, EXPRESSION* value){
  ENUMFIELD* retval = malloc(sizeof(ENUMFIELD));
  retval->name = name;
  //confirm is expression statement
  //confirm is expr that will resolve into something uintconstifiable that is known at compile time
  retval->value = value;//this is worthless TODO: FIX
  return retval;
}

enum declpart_info{
  POINTERSPEC, ARRAYSPEC, PARAMSSPEC, BITFIELDSPEC
};
struct declarator_part{
  enum declpart_info type;
  union{
    DYNARR* params;
    EXPRESSION* arrspec;
    EXPRESSION* bfspec;
    TYPEBITS ptrspec;
    void* garbage;
  };
};

struct declarator_part* mkdeclpart(enum declpart_info typ, void* d){
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = typ;
  retval->garbage = d;
  return retval;
}
struct declarator_part* mkdeclptr(TYPEBITS d){
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = POINTERSPEC;
  retval->ptrspec = d;
  return retval;
}

typedef struct{
  char* varname;
  IDTYPE* type;
} DECLARATION;//???
DECLARATION* mkdeclaration(char* name){
  DECLARATION* retval = malloc(sizeof(DECLARATION));
  retval->varname = name;
  //retval->type = dactor(4);
  return retval;
}
EXPRESSION* exprfromdecl(char* name, IDTYPE* id){
  EXPRESSION* outer = malloc(sizeof(EXPRESSION));
  outer->type = IDENT;
  //outer->id = id;
  DYNARR* ptrs = id->pointerstack;
  if(ptrs) {
    for(int i = 0; i<ptrs->length; i++){
      //TODO
    }
  }
  return outer;
}


typedef struct{
  DECLARATION* declaration;
  EXPRESSION* assign;
} INITBITS;//?

FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn) {
  FUNC* func = malloc(sizeof(FUNC));
  func->name = name;
  func->body = body;
  func->params = params;
  func->retrn = retrn;
  return func;
}

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
void scopepush(struct lexctx* ctx){
  SCOPE* child = mkscope(dapeek(ctx->scopes));
}

struct intinfo{
  long num;
  char sign;
}; 
#define aget(param, index) ((INITIALIZER*) (param)->arr[(index)])

typedef struct {
  char isfunc;
  union {
    INITIALIZER* i;
    FUNC* f;
    void* garbage;
  };
} TOPBLOCK;

TOPBLOCK* gtb(char isfunc, void* assign) {
  TOPBLOCK* retval = malloc(sizeof(TOPBLOCK));
  retval->isfunc = isfunc;
  retval->garbage = assign;
  return retval;
}
}

%param {struct lexctx* ctx}
%union {
  struct intinfo ii;
  char* str;
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
  DECLARATOR* declvariant;
  FUNC* funcvariant;
}

%type<typevariant> types1 types2 types1o
%type<idvariant> typem typews1 typebs type
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
  "typedef" typebs cs_minutes ';' {/*add to context but do not push anything, maybe return null?*/}
| typebs cs_inits ';' 
{$$ = $2; for(int i = 0; i < $$->length; i++){
  aget($$, i)->decl->tb |= $1->tb; 
  if($1->pointerstack->length) 
    aget($$, 0)->decl->declparts = damerge($1->pointerstack, aget($$, 0)->decl->declparts);
}};
cs_inits:
  cs_inits ',' declarator '=' esc {$$ = $1; dapush($$, geninit($3, $5));}
| declarator '=' esc {$$ = dactor(8); dapush($$, geninit($1, $3));}
| cs_inits ',' declarator {$$ = $1; dapush($$, geninit($3, NULL));}
| declarator {$$ = dactor(8); dapush($$, geninit($1, NULL));};
cs_minutes:
  cs_minutes ',' declarator {$$ = $1; dapush($1, $3);}
| declarator {$$ = dactor(8); dapush($$, $1);};
declarator:
  abstract_ptr declname {$$ = $2; $2->declparts = damerge($1, $2->declparts);}
| declname {$$ = $1;};
declname:
  IDENTIFIER {$$ = mkdeclarator($1);}
| '(' declarator ')' {$$ = $2;}
| declname '[' ']' {$$ = $1; dapush($$->declparts,mkdeclpart(ARRAYSPEC, NULL));}
| declname '[' expression ']' {$$ = $1; dapush($$->declparts,mkdeclpart(ARRAYSPEC, $3));}
| declname '(' ')' {$$ = $1; dapush($$->declparts, mkdeclpart(PARAMSSPEC, NULL));}
| declname '(' params ')' {$$ = $1; dapush($$->declparts, mkdeclpart(PARAMSSPEC, $3));};
params:
  param_decl {$$ = dactor(8); dapush($$, $1);}
| params ',' param_decl {$$ = $1; dapush($$, $3);};
param_decl:
  typebs declarator {$$ = $2; $$->tb = $1->tb;/*TODO: THIS IS NOT RIGHT, I need proper type handling*/free($2);};
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
  TYPE_NAME {$$ = search(ctx->idents, $1);}
| typem {$$ = $1;}
| types1 typews1 {$$ = $2; $$->tb |= $1;};
typebs:
  typem {$$ = $1;}
| types1 typebs {$$ = $2; $$->tb |= $1;}
| types2 typebs {$$ = $2; $$->tb |= $1;};
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
| INTEGER_LITERAL {$$ = $1.sign ? ct_intconst_expr($1.num) : ct_uintconst_expr($1.num);}
| FLOAT_LITERAL {$$ = ct_floatconst_expr($1/*.dbl*/);}
| IDENTIFIER {$$ = ct_ident_expr($1/*.str*/);};
escl:
  esc {$$ = dactor(32); dapush($$, $1);}
| escl ',' esc {$$ = $1; dapush($$, $3); };

function:
  typebs declname compound_statement {struct declarator_part* dp = dapop($2->declparts); $1->pointerstack = damerge($1->pointerstack, $2->declparts);/*TODO: have this extract only pointers and arrays and not i.e. params)*/ $$ = ct_function($2->idname, $3, dp->params, $1); free($2);/*check that it is in fact a param spec*/};
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
compound_statement:/*add new scope to scope stack here*/
  '{' '}' {$$ = mkcmpndstmt(NULL);}
| '{' statements_and_initializers '}' {$$ = mkcmpndstmt($2);};
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
  type cs_decls ';' {$$ = $2; for(int i = 0; i < $2->length; i++) ((DECLARATOR*) $2->arr[i])->tb |= $1->tb; if($1->pointerstack->length) ((DECLARATOR*)$$->arr[0])->declparts = damerge($1->pointerstack, ((DECLARATOR*)$$->arr[0])->declparts);};
cs_decls:
  sdecl ',' cs_decls {$$ = $3; dapush($$, $1);}
| sdecl {$$ = dactor(8); dapush($$, $1);};
sdecl: 
  declarator {$$ = $1;}
| declarator ':' esc {$$ = $1; dapush($$->declparts, mkdeclpart(BITFIELDSPEC, $3));}
| ':' esc {$$ = mkdeclarator(NULL); dapush($$->declparts, mkdeclpart(BITFIELDSPEC, $2));};
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

