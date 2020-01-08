%locations
%token IDENTIFIER INT FLOAT STRING_LITERAL
%token ARROW "->" INC "++" DEC "--" SHL "<<" SHR ">>" LE "<=" GE ">=" EQ "==" 
%token NEQ "!=" AND "&&" OR "||" DIV_GETS "/=" MUL_GETS "*=" MOD_GETS "%=" 
%token ADD_GETS "+=" SUB_GETS "-=" SHL_GETS "<<=" SHR_GETS ">>=" AND_GETS "&=" 
%token XOR_GETS "^=" OR_GETS "|=" TYPE_NAME

%token TYPEDEF "typedef" STATIC "static" EXTERN "extern" CHAR "char"
%token INT8 "int8" INT16 "int16" INT32 "int32" INT64 "int64" BYTE "byte"
%token DBYTE "dbyte" QBYTE "qbyte" OBYTE "obyte" SINGLE "single" DOUBLE "double" 
%token CASE "case" DEFAULT "default" IF "if" ELSE "else" SWITCH "switch" 
%token WHILE "while" DO "do" FOR "for" GOTO "goto" CONTINUE "continue" 
%token BREAK "break" RETURN "return" SIZEOF "sizeof"
%token STRUCT "struct" ENUM "enum" UNION "union"

%right THEN "else"
%start program
%define parse.assert
%define parse.error verbose

%code requires{
#include <string.h>
#include "hash.h"
#include "dynarr.h"

typedef enum{
  undefined,
  function,
  parameter,
  local_var,
  global_var
} IDTYPE;
typedef struct {
  IDTYPE type;
  //int index;//index of type within scope (i.e. parameter index)
  char* name;
} IDENTIFIER;

typedef enum{
  INT8, INT16, INT32, INT64, BYTE, DBYTE, QBYTE, OBYTE, SINGLE, DOUBLE
} VARTYPE;

typedef enum{
  NOP, STRING, INT, FLOAT, IDENT,
  ADD, NEG, SUB, EQ, NEQ, GT, LT, GTE, LTE, MULT, DIVI, MOD,
  PREINC, POSTINC, PREDEC, POSTDEC,
  L_AND, L_OR, L_NOT, B_AND, B_OR, B_XOR, B_NOT, SHL, SHR,
  DOTOP, ARROW,
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

struct expr;
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
    struct expr unaryparam;
    char* strconst;
    long intconst;
    double floatconst;
    IDENTIFIER id;
    /*possible struct const for later struct initializations*/
  };
} EXPRESSION;

typedef struct{
  char* name;
  char** fieldnames;
  VARTYPE* fieldtypes;
} STRUCT;

typedef struct{
  char* name;
  char** fieldnames;
  VARTYPE* fieldtypes;
} UNION;

typedef struct{
  char* name;
  char** fieldnames;
  int* fieldvals;
} ENUM;

typedef struct function{
  char* name;
  EXPRESSION code;
  unsigned int num_vars, num_params;
  VARTYPE retType;
} FUNC;
struct lexctx;

EXPRESSION* cloneexpr(EXPRESSION* orig){
  EXPRESSION* clone = malloc(sizeof(EXPRESSION));
  memcpy(clone, orig, sizeof(EXPRESSION));
  return clone;
}

EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION param{
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->unaryparam = param;
  return retval;
}
EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION param1, EXPRESSION param2){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->param1 = param1
  retval->param2 = param2;
  return retval;
}
EXPRESSION* ct_ternary_expr(EXPRESSION param1, EXPRESSION param2, EXPRESSION param3){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = TERNARY;
  retval->ifexpr = param1
  retval->thenexpr = param2;
  retval->elseexpr = param2;
  return retval;
}
EXPRESSION* ct_fcall_expr(FUNCTION* func, int num, EXPRESSION* params){
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
EXPRESSION* ct_intconst_expr(EXPRTYPE t, long num){
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->intconst = num;
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

typedef struct{
  char numargs;
  EXPRESSION* argl[256];
} ARGS;
ARGS* mkargs(){
  ARGS* a = malloc(sizeof(ARGS));
  a->numargs = 1;
  return a;
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
typedef struct statement_or_initializer{
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
      struct stmt* loopbody;
    };
    struct{
      EXPRESSION* init;
      EXPRESSION* forcond;
      EXPRESSION* increment;
      struct stmt* forloopbody;
    };
    IDENTIFIER* gotoloc;
    DYNARR* stmtsandinits;
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
  retval->forloopbody = bdy;
  return retval;
}
STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy){
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->cond = condition;
  retval->loopbody = bdy;
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
  retval->type = CMPND
  retval->stmtsandinits = stmtsandinits;
  return retval;
}
}

%param {struct lexctx* ctx}
%type<EXPRESSION*> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee
%type<IDENTIFIER*> IDENTIFIER
%type<char*> STRING_LITERAL
%type<long> INT
%type<double> FLOAT
%type<ARGS*> escl
%type<STATEMENT*> statement compound_statement
%type<DYNARR*> statements_and_initializers

%code {
  struct lexctx{
    //Figure out some way to have scopes here
    FUNCTION* funclist;
    unsigned int fllast, fllen;
    FUNCTION* curfunc;
    MAP* scopes[512];
    unsigned int layer;
  };
  ctx->layer = 0;
  DYNARR* scopes = dactor(64);
  dapush(scopes, htctor());
}

%%
program:
  function 
| initializer
| function program
| initializer program;
initializer:
  typebs cs_inits ';';
cs_inits:
  declarator '=' esc ',' cs_inits
| declarator '=' esc
| declarator ',' cs_inits
| declarator;
declarator:
  abstract_ptr declname
| declname;
declname:
  IDENTIFIER
| '(' declarator ')'
| declname '[' ']'
| declname '[' expression ']'
| declname '(' ')'
| declname '(' params ')';
params:
  param_decl
| param_decl ',' params;
param_decl:
  typebs declarator;
typem:
  "char"
| "int8" 
| "int16"
| "int32"
| "int64"
| "byte" 
| "dbyte"
| "qbyte"
| "obyte" 
| "single"
| "double"
| "void" 
|  enum 
|  struct 
|  union 
|  TYPE_NAME;
types1:
  "const" 
| "volatile";
types2:
  "typedef" 
| "extern" 
| "static";
typews1:
  typem
| types1 typews1;
typebs:
  typem
| types1 typebs
| types2 typebs;
type:
  typews1;
types1o:
  types1
| types1o types1;
abstract_ptr:
  '*'
| '*' abstract_ptr
| '*' types1o
| '*' types1o abstract_ptr;
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
  est '?' expression ':' esa {$$ = ct_ternary_expr($1, $2, $3);}
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
  '(' type ')' esm {/*$$ = coerce_type($2, $4);*/}
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
| "sizeof" '(' type ')' {/*$$ = ct_intconst_expr(szof($3));*/}
| "sizeof" esca {$$ = ct_intconst_expr(szofexpr($2));}
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
| STRING_LITERAL {$$ = ct_strconst_expr($1);}
| INT {$$ = ct_intconst_expr($1);}
| FLOAT {$$ = ct_floatconst_expr($1);}
| IDENTIFIER {$$ = ct_ident_expr($1);};
escl:
  esc {$$ = mkargs()->argl[0] = $1;}
| escl ',' esc {$$ = $1->argl[$1->numargs++] = $3;};

function:
  typebs declarator compound_statement;
statement:
  compound_statement
|  IDENTIFIER ':' statement
| "case" esc ':' statement
| "default" ':' statement
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7);}
| "switch" '(' expression ')' statement {$$ = mklsstmt(SWITCH, $3, $5);}
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2);}
| "for" '(' ee ';' ee ';' ee ')' statement {$$ = mkforstmt($3, $5, $7, $9);}
| "goto" IDENTIFIER ';' {$$ = mkgotostmt($2);}
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
  "union" IDENTIFIER '{' struct_decls '}'
| "union" '{' struct_decls '}'
| "union" IDENTIFIER;
struct:
  "struct" IDENTIFIER '{' struct_decls '}'
| "struct" '{' struct_decls '}'
| "struct" IDENTIFIER;
struct_decls:
  struct_decl 
| struct_decls struct_decl;
struct_decl:
  type cs_decls ';'
cs_decls:
  cs_decls ',' sdecl
| sdecl;
sdecl: 
  declarator
| declarator ':' esc
| ':' esc;
enum:
  "enum" IDENTIFIER '{' enums '}'
| "enum" '{' enums '}'
| "enum" IDENTIFIER;
enums:
  enumerator
| enums ',' enumerator;
enumerator:
  IDENTIFIER
| IDENTIFIER '=' esc;
  

%%
#include <stdio.h>
int yyerror(char* s){
    printf("\ncolumn: %d\n%s\n", yylloc->first_column, s);
}

