%locations
%token ARROWTK "->" INC "++" DEC "--" SHLTK "<<" SHRTK ">>" LE "<=" GE ">=" EQTK "=="
%token NEQTK "!=" AND "&&" OR "||" DIV_GETS "/=" MUL_GETS "*=" MOD_GETS "%="
%token ADD_GETS "+=" SUB_GETS "-=" SHL_GETS "<<=" SHR_GETS ">>=" AND_GETS "&=" 
%token XOR_GETS "^=" OR_GETS "|="

%token TYPEDEF "typedef" STATIC "static" EXTERN "extern" CHAR "char" VOID "void"
%token INT8 "int8" INT16 "int16" INT32 "int32" INT64 "int64" BYTE "byte"
%token DBYTE "dbyte" QBYTE "qbyte" OBYTE "obyte" SINGLE "single" DOUBLE "double" 
%token CASETK "case" DEFAULTTK "default" IF "if" ELSE "else" SWITCHTK "switch"
%token WHILE "while" DO "do" FOR "for" GOTO "goto" CONTINUE "continue" 
%token BREAK "break" RETURN "return" SIZEOF "sizeof" UNSIGNED "unsigned"
%token STRUCTTK "struct" ENUMTK "enum" UNIONTK "union" SIGNED "signed"

%right THEN "else"
%start program
%define parse.trace
%define parse.assert
%define parse.error verbose

%token<ii> INTEGER_LITERAL;
%token<dbl> FLOAT_LITERAL;
%token<str> SYMBOL TYPE_NAME
%token<dstr> STRING_LITERAL;

%code requires{
  #include <stdint.h>
  #include <stdio.h>
  #include "compintern.h"
  #include "dynarr.h"
  #include "parallel.h"
  extern DYNARR* file2compile;


  #define aget(param, index) ((INITIALIZER*) (param)->arr[(index)])
  #define dget(param, index) ((DECLARATION*) (param)->arr[(index)])
  //TODO: Compound literals?
  //TODO: Consider designated initializers?
}

%{
  extern struct lexctx* ctx;
  int yylex();
  int yyerror();
%}

%union {
  struct intinfo ii;
  int integert;
  char* str;
  DYNSTR* dstr;
  double dbl;
  TYPEBITS typevariant;
  EXPRESSION* exprvariant;
  IDTYPE* idvariant;
  INITIALIZER* initvariant;
  EOI* firforvariant;
  STATEMENT* stmtvariant;
  DYNARR* arrvariant;
  UNION* unionvariant;
  STRUCT* structvariant;
  ENUM* enumvariant;
  DECLARATION* declvariant;
  FUNC* funcvariant;
}

%type<dstr> multistring
%type<integert> typemsign
%type<typevariant> types1 types2 types1o
%type<idvariant> typem typews1 type typemintkw inttypem
%type<exprvariant> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee escoa
%type<stmtvariant> statement compound_statement
%type<arrvariant> statements_and_initializers struct_decls struct_decl cs_decls enums escl escoal abstract_ptr params cs_inits cs_minutes initializer program array_literal structbody enumbody
%type<unionvariant> union fullunion
%type<structvariant> struct fullstruct
%type<enumvariant> enum fullenum
%type<declvariant> declarator declname param_decl sdecl
%type<funcvariant> function
%type<firforvariant> dee

%%
program:
  function {
    $$ = dactor(4096);
    if(!search(ctx->funcs, $1->name)) {
      insert(ctx->funcs, $1->name, $1);
      dapush($$, gtb(1, $1));
    }
    else {}
  }
| initializer {
    $$ = dactor(4096);
    for(int i = 0; i < $1->length; i++) {
      //TODO: Validator to handle redefinitions, error case as well
      if(!scopequeryval(ctx, M_VARIABLE, aget($1, i)->decl->varname)) {
        add2scope(scopepeek(ctx), aget($1, i)->decl->varname, M_VARIABLE, aget($1, i)->expr);
      }
      else {}
      dapush($$, gtb(0, $1));
    }
    free($1);
  }
| program function {
    $$ = $1;
    char* cs = $2->name;
    if(!search(ctx->funcs, $2->name)) {
      insert(ctx->funcs, $2->name, $2);
      dapush($$, gtb(1, $2));
    }
    else {}
  }
| program initializer {
    $$ = $1;
    for(int i = 0; i < $2->length; i++) {
      //TODO: Validator to handle redefinitions, error case as well
      if(!scopequeryval(ctx, M_VARIABLE, aget($2, i)->decl->varname)) {
        add2scope(scopepeek(ctx), aget($2, i)->decl->varname, M_VARIABLE, aget($2, i)->expr);
      }
      else {}
      dapush($$, gtb(0, $2));
    }
    free($2);
  };
initializer:
"typedef" type cs_minutes ';' {
  DECLARATION* dc;
  for(int i = 0; i < $3->length; i++) {
    dc = dget($3, i);
    dc->type->tb |= $2->tb;
    if($2->pointerstack) {
      DYNARR* nptr = daclone($2->pointerstack);
      if(dc->type->pointerstack)
        dc->type->pointerstack = damerge(nptr, dc->type->pointerstack);
      else
        dc->type->pointerstack = nptr;
    }
    if($2->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
      if($2->structtype->fields) {
         dc->type->structtype = $2->structtype;
      } else {
        HASHTABLE* ht;
        if($2->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($2->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        } else if($2->tb & ENUMVAL) {
          ht = scopepeek(ctx)->forwardenums;
        } else {
          fprintf(stderr, "Error: forward definition of unknown type %s in %s %d.%d-%d.%d\n", $2->structtype->name, dapeek(file2compile), locprint(@$));
          continue;
        }
        DYNARR* da = search(ht, $2->structtype->name);
        dapush(da, &(dc->type->structtype));
      }
    }
    add2scope(scopepeek(ctx), dc->varname, M_TYPEDEF, dc->type);
    free(dc);
  }
  $$ = dactor(0);
  }
| type cs_inits ';' {
  SCOPE* current = scopepeek(ctx);
  $$ = $2;
  INITIALIZER* ac;
  for(int i = 0; i < $$->length; i++) {
    ac = aget($$, i);
    ac->decl->type->tb |= $1->tb; 
    if($1->pointerstack) {
      DYNARR* nptrst = daclone($1->pointerstack);
      if(ac->decl->type->pointerstack)
        ac->decl->type->pointerstack = damerge(nptrst, ac->decl->type->pointerstack);
      else 
        ac->decl->type->pointerstack = nptrst;
    }
    if($1->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)) {
      if($1->structtype->fields) {
        ac->decl->type->structtype = $1->structtype;
      } else {
        HASHTABLE* ht;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        } else if($1->tb & ENUMVAL) {
          ht = scopepeek(ctx)->forwardenums;
        } else {
          fprintf(stderr, "Error: forward definition of unknown type %s in %s %d.%d-%d.%d\n", $1->structtype->name, dapeek(file2compile), locprint(@$));
          continue;
        }
        DYNARR* da = search(ht, $1->structtype->name);
        dapush(da, &(ac->decl->type->structtype));
      }
    }
    add2scope(current, ac->decl->varname, M_VARIABLE, ac->decl->type);
  }}
| "struct" SYMBOL ';' {
  $$ = dactor(0);
  if(!scopequeryval(ctx, M_STRUCT, $2)) {
    add2scope(scopepeek(ctx), $2, M_STRUCT, NULL);
    insert(scopepeek(ctx)->forwardstructs, $2, dactor(16));
  } else 
    fprintf(stderr, "Error: redefinition of struct %s in %s %d.%d-%d.%d\n", $2, dapeek(file2compile),  locprint(@$));
  }
| "enum" SYMBOL ';' {
  $$ = dactor(0);
  if(!scopequeryval(ctx, M_ENUM, $2)) {
    add2scope(scopepeek(ctx), $2, M_ENUM, NULL);
    insert(scopepeek(ctx)->forwardenums, $2, dactor(16));
  } else 
    fprintf(stderr, "Error: redefinition of enum %s in %s %d.%d-%d.%d\n", $2, dapeek(file2compile),  locprint(@$));
  }
| "union" SYMBOL ';' {
  $$ = dactor(0);
  if(!scopequeryval(ctx, M_UNION, $2)) {
    add2scope(scopepeek(ctx), $2, M_UNION, NULL);
    insert(scopepeek(ctx)->forwardunions, $2, dactor(16));
  } else 
    fprintf(stderr, "Error: redefinition of union %s in %s %d.%d-%d.%d\n", $2, dapeek(file2compile),  locprint(@$));
  }
| fullstruct';' {$$ = dactor(0);}
| fullenum ';' {$$ = dactor(0);}
| fullunion ';' {$$ = dactor(0);};
/*some garbage with checking whether already defined must be done*/
cs_inits:
  cs_inits ',' declarator '=' escoa {$$ = $1; dapush($$, geninit($3, $5));}
| declarator '=' escoa {$$ = dactor(8); dapush($$, geninit($1, $3));}
| cs_inits ',' declarator {$$ = $1; dapush($$, geninit($3, NULL));}
| declarator {$$ = dactor(8); dapush($$, geninit($1, NULL));};
escoa:
  esc {$$ = $1;}
| array_literal {$$ = ct_array_lit($1);};
cs_minutes:
  cs_minutes ',' declarator {$$ = $1; dapush($1, $3);}
| declarator {$$ = dactor(8); dapush($$, $1);};
declarator:
  abstract_ptr declname {$$ = $2; $2->type->pointerstack = damerge($1, $2->type->pointerstack);}
| declname {$$ = $1;};
declname:
  SYMBOL {$$ = mkdeclaration($1);}
| '(' declarator ')' {$$ = $2;}
| declname '[' ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, NULL));}
| declname '[' expression ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, $3));}
| declname '(' ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, NULL));}
| declname '(' params ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, $3));};
params: /*TODO: confirm no collisions*/
  param_decl {$$ = dactor(8); dapush($$, $1);}
| params ',' param_decl {$$ = $1; dapush($$, $3);};
param_decl:
  type declarator {
    $2->type->tb |= $1->tb;
    if($1->pointerstack) {
      DYNARR* nptr = daclone($1->pointerstack);
      if($1->pointerstack)
        $2->type->pointerstack = damerge(nptr, $2->type->pointerstack); 
      else 
        $2->type->pointerstack = nptr;
    }
    if($1->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)) {
      if($1->structtype->fields) {
        $2->type->structtype = $1->structtype;
      } else {
        HASHTABLE* ht;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        } else if($1->tb & ENUMVAL) {
          ht = scopepeek(ctx)->forwardenums;
        }
        DYNARR* da = search(ht, $1->structtype->name);
        dapush(da, &($2->type->structtype));
      }
    }
    $$ = $2; 
    };
typemsign:
  "signed" {$$ = 0;}
| "unsigned" {$$ = UNSIGNEDNUM;}
typemintkw:
  "char" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1;}
| "int8" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1;}
| "int16" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2;}
| "int32" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4;}
| "int64" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8;}
inttypem:
  typemintkw {$$ = $1;}
| typemsign typemintkw {$$ = $2; $$->tb |= $1;}
typem:
  inttypem {$$ = $1;}
| "byte"  {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1 | UNSIGNEDNUM;}
| "dbyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2 | UNSIGNEDNUM;}
| "qbyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4 | UNSIGNEDNUM;}
| "obyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8 | UNSIGNEDNUM;}
| "void" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = VOIDNUM;}
| "single" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4 | FLOATNUM;}
| "double" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8 | FLOATNUM;}
| struct {
    $$ = calloc(1, sizeof(IDTYPE)); 
    $$->structtype = $1;
    $$->tb = STRUCTVAL;
    if($1->fields == NULL) {
      DYNARR* da = (DYNARR*) search(scopepeek(ctx)->forwardstructs, $1->name); 
      if(!da) 
        fprintf(stderr, "Error: struct %s undeclared in %s %d.%d-%d.%d\n", $1->name, dapeek(file2compile), locprint(@$));
      else
        dapush(da, &($$->structtype));
    }}
| union {
    $$ = calloc(1, sizeof(IDTYPE)); 
    $$->uniontype = $1;
    $$->tb = UNIONVAL;
    if($1->fields == NULL) {
      DYNARR* da = (DYNARR*) search(scopepeek(ctx)->forwardunions, $1->name); 
      if(!da) 
        fprintf(stderr, "Error: union %s undeclared in %s %d.%d-%d.%d\n", $1->name, dapeek(file2compile), locprint(@$));
      else
        dapush(da, &($$->uniontype));
    }}
| enum {
    $$ = calloc(1, sizeof(IDTYPE)); 
    $$->enumtype = $1;
    $$->tb = ENUMVAL; 
    if($1->fields == NULL) {
      DYNARR* da = (DYNARR*) search(scopepeek(ctx)->forwardenums, $1->name); 
      if(!da) 
        fprintf(stderr, "Error: enum %s undeclared in %s %d.%d-%d.%d\n", $1->name, dapeek(file2compile), locprint(@$));
      else
        dapush(da, &($$->enumtype));
    }};
types1:
  "const" {$$ = CONSTNUM;}
| "volatile" {$$ = VOLATILENUM;};
types2:
  "extern" {$$ = EXTERNNUM;}
| "static" {$$ = STATICNUM;};
typews1:
  TYPE_NAME {
    $$ = malloc(sizeof(IDTYPE));
    IDTYPE* idt = scopesearch(ctx, M_TYPEDEF, $1);
    if(idt) {
      memcpy($$, idt, sizeof(IDTYPE));
    } else {
      fprintf(stderr, "Error: use of unknown type name %s in %s %d.%d-%d.%d\n", $1, dapeek(file2compile), locprint(@$));
    }
    }
| typem {$$ = $1;}
| types1 typem {$$ = $2; $$->tb |= $1;}
| types1 TYPE_NAME {
    $$ = malloc(sizeof(IDTYPE));
    IDTYPE* idt = scopesearch(ctx, M_TYPEDEF, $2);
    if(idt) {
      memcpy($$, idt, sizeof(IDTYPE));
    } else {
      fprintf(stderr, "Error: use of unknown type name %s in %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
    }
    $$->tb |= $1; 
    }
| types2 typem {$$ = $2; $$->tb |= $1;}
| types2 TYPE_NAME {
    $$ = malloc(sizeof(IDTYPE));
    IDTYPE* idt = scopesearch(ctx, M_TYPEDEF, $2);
    if(idt) {
      memcpy($$, idt, sizeof(IDTYPE));
    } else {
      fprintf(stderr, "Error: use of unknown type name %s in %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
    }
    $$->tb |= $1; 
    };
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
| '(' type abstract_ptr ')' esm {
  IDTYPE* idt = malloc(sizeof(IDTYPE));
  memcpy(idt, $2, sizeof(IDTYPE*));
  if(idt->pointerstack) {
    idt->pointerstack = damerge(daclone(idt->pointerstack), $3);
  } else {
    idt->pointerstack = $3;
  }
  $$ = ct_cast_expr(idt, $5);}
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
| "sizeof" '(' type abstract_ptr ')' {$$ = ct_uintconst_expr(sizeof(uintptr_t));}
| "sizeof" '(' type ')' {$$ = ct_sztype($3);}
| "sizeof" esca {$$ = ct_unary_expr(SZOFEXPR,$2);}
| esp {$$ = $1;};
esp:
  esp "++" {$$ = ct_unary_expr(POSTINC, $1);}
| esp "--" {$$ = ct_unary_expr(POSTDEC, $1);}
| esp '(' ')' {$$ = ct_fcall_expr($1, dactor(0));}
| esp '(' escl ')' {$$ = ct_fcall_expr($1, $3);}
| esp '[' expression ']' {$$ = ct_unary_expr(DEREF, ct_binary_expr(ADD, $1, $3));}
| esp '.' SYMBOL {
  $$ = ct_binary_expr(DOTOP, $1, ct_member_expr($3));}
| esp "->" SYMBOL {$$ = ct_binary_expr(ARROW, $1, ct_member_expr($3));}
| esu {$$ = $1;};
esu:
  '(' expression ')' {$$ = $2;}
| multistring {$$ = ct_strconst_expr($1->strptr); free($1);}
| INTEGER_LITERAL {$$ = $1.sign ? ct_intconst_expr($1.num) : ct_uintconst_expr($1.num);}
| FLOAT_LITERAL {$$ = ct_floatconst_expr($1);}
| SYMBOL {
    EXPRESSION* expr = scopesearch(ctx, M_ENUM_CONST, $1);
    if(!expr) {
      $$ = ct_ident_expr(ctx, $1);
    } else {
      char* foobar = $1;
      $$ = expr;
    }
    }
| error {
    $$ = ct_nop_expr(); 
    fprintf (stderr, "%d.%d-%d.%d in %s: error encountered\n",
             @1.first_line, @1.first_column,
             @1.last_line, @1.last_column,
             dapeek(file2compile));
    };
escl:
  esc {$$ = dactor(32); dapush($$, $1);}
| escl ',' esc {$$ = $1; dapush($$, $3); };

escoal:
  escoa {$$ = dactor(32); dapush($$, $1);}
| escoal ',' escoa {$$ = $1; dapush($$, $3); };
array_literal:
  '{' escoal commaopt '}' {$$ = $2;};

multistring:
  STRING_LITERAL {$$ = $1;}
| STRING_LITERAL STRING_LITERAL {
    $$ = $1;
    dscat($1, $2->strptr, $2->lenstr);
    free($2->strptr);
    free($2);
    }

function: /*TODO: midrule action and getting parameters into local scope of compound statement*/
  type declarator <funcvariant>{
    DYNARR* parammemb;
    struct declarator_part* dp = dapop($2->type->pointerstack);
    $2->type->tb |= $1->tb;
    if($1->pointerstack) {
      $2->type->pointerstack = damerge(daclone($1->pointerstack), $2->type->pointerstack);
    }
    if($1->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)) {
      if($1->structtype->fields) {
        $2->type->structtype = $1->structtype;
      } else {
        HASHTABLE* ht;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        } else if($1->tb & ENUMVAL) {
          ht = scopepeek(ctx)->forwardenums;
        }
        DYNARR* da = search(ht, $1->structtype->name);
        dapush(da, &($2->type->structtype));
      }
    }
    if(dp->params)
      parammemb = dp->params;
    else
      parammemb = dactor(0);
    $$ = ct_function($2->varname, NULL, parammemb, $2->type);
    free($2);
    ctx->func = $$;
    /*check that it is in fact a param spec*/
    } compound_statement {
    $$ = $3;
    $$->body = $4;
    ctx->func = NULL;
    }
| error compound_statement {
  fprintf (stderr, "%d.%d-%d.%d in %s: error encountered in function definition\n",
           @1.first_line, @1.first_column,
           @1.last_line, @1.last_column,
           dapeek(file2compile));
  };
statement:
  compound_statement {$$ = $1;}
|  SYMBOL ':' {$$ = mklblstmt(ctx, $1);}
| "case" esc ':' { 
    char* caselbl = malloc(128);
    snprintf(caselbl, 128, "__joecc__%s__%d", ctx->func->name, (ctx->func->caseindex)++);
    $$ = mkcasestmt(ctx, $2, caselbl);
    }
| "default" ':' {
    char* caselbl = malloc(128);
    snprintf(caselbl, 128, "__joecc__%s__%d", ctx->func->name, (ctx->func->caseindex)++);
    $$ = mkdefaultstmt(ctx, caselbl);
    }
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7);}
| "switch" '(' expression ')' switch_midrule compound_statement {
    PARALLEL* pll = dapop(ctx->func->switchstack);
    $$ = mkswitchstmt($3, $6, pll);
    //pop from switchstack, differentiate statement with case hashtable for ints
    }
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2);}
| "for" '(' dee  ee ';' ee ')' statement {$$ = mkforstmt($3, $4, $6, $8);}
| "goto" SYMBOL ';' {$$ = mkgotostmt($2);/*find label within function at some point, probably not now though*/}
| "break" ';' {$$ = mkexprstmt(LBREAK,NULL);}
| "continue" ';' {$$ = mkexprstmt(LCONT,NULL);}
| "return" ';' {$$ = mkexprstmt(FRET,NULL);}
| "return" expression ';' {$$ = mkexprstmt(FRET,$2);}
| expression ';' {$$ = mkexprstmt(EXPR,$1);}
| ';' {$$ = mkexprstmt(NOPSTMT, NULL);};
ee: 
  expression {$$ = $1;}
| %empty {$$ = ct_nop_expr();};
dee:
  initializer {$$ = malloc(sizeof(EOI)); $$->isE = 0; $$->I = $1;}
| ee ';' {$$ = malloc(sizeof(EOI)); $$->isE = 1; $$->E = $1;};
compound_statement:/*add new scope to scope stack, remove when done*/
  '{' '}' {$$ = mkcmpndstmt(NULL);}
| '{' compound_midrule statements_and_initializers '}' {
    $$ = mkcmpndstmt($3); 
    scopepop(ctx);
    };
compound_midrule: %empty {
    scopepush(ctx);
    };
statements_and_initializers:
  initializer {$$ = dactor(4096); dapush($$,soii($1));}
| statement {$$ = dactor(4096); dapush($$,sois($1));}
| statements_and_initializers initializer {$$ = $1; dapush($$,soii($2));}
| statements_and_initializers statement {$$ = $1; dapush($$,sois($2));};
switch_midrule:
  %empty {
    dapush(ctx->func->switchstack, paralector());
    };
/*for struct enum union make sure no redefinitions are happening*/
fullunion:
  "union" SYMBOL {
    if(!scopesearch(ctx, M_UNION, $2)) {
      if(!queryval(scopepeek(ctx)->forwardunions, $2)) {
        add2scope(scopepeek(ctx), $2, M_UNION, NULL);
        insert(scopepeek(ctx)->forwardunions, $2, dactor(16));
      }
    } else {
      fprintf(stderr, "Error: redefinition of union %s at %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
    }} structbody  {
    $$ = unionctor($2, $4); 
    add2scope(scopepeek(ctx), $2, M_UNION, $$); 
    defbackward(ctx, M_UNION, $2, $$);
    };
union:
  fullunion {$$ = $1;}
| "union" structbody  {$$ = unionctor(NULL, $2);}
| "union" SYMBOL {
    $$ = (UNION*) scopesearch(ctx, M_UNION, $2);
    if(!$$) {
      if(queryval(scopepeek(ctx)->forwardunions, $2)) {
        $$ = malloc(sizeof(UNION));
        $$->name = $2;
        $$->fields = NULL;
      } else {
        fprintf(stderr, "Error: reference to undefined union %s at %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
      }
    }};
fullstruct:
  "struct" SYMBOL {
    if(!scopesearch(ctx, M_STRUCT, $2)) {
      if(!queryval(scopepeek(ctx)->forwardstructs, $2)) {
        add2scope(scopepeek(ctx), $2, M_STRUCT, NULL);
        insert(scopepeek(ctx)->forwardstructs, $2, dactor(16));
      }
    } else {
      fprintf(stderr, "Error: redefinition of struct %s at %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
    }} structbody {
    $$ = structor($2, $4); 
    add2scope(scopepeek(ctx), $2, M_STRUCT, $$);
    defbackward(ctx, M_STRUCT, $2, $$);
    };
struct:
  fullstruct {$$ = $1;}
| "struct" structbody {$$ = structor(NULL, $2);}
| "struct" SYMBOL {
    $$ = (STRUCT*) scopesearch(ctx, M_STRUCT, $2);
    if(!$$) {
      if(queryval(scopepeek(ctx)->forwardstructs, $2)) {
        $$ = malloc(sizeof(STRUCT));
        $$->name = $2;
        $$->fields = NULL;
      } else {
        fprintf(stderr, "Error: reference to undefined struct %s at %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
      }
    }};
structbody: '{' struct_decls '}' {$$ = $2;};
struct_decls: /*TODO: Confirm no collisions*/
  struct_decl {$$ = $1;}
| struct_decls struct_decl {$$ = damerge($1, $2);};
struct_decl:
  type cs_decls ';' {
    $$ = $2; 
    DECLARATION* dc;
    for(int i = 0; i < $2->length; i++) {
      dc = dget($$, i);
      dc->type->tb |= $1->tb; 
      if($1->pointerstack) {
        DYNARR* nptr = daclone($1->pointerstack);
        if(dc->type->pointerstack)
          dc->type->pointerstack = damerge(nptr, dc->type->pointerstack);
        else
          dc->type->pointerstack = nptr;
      }
      if($1->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
        if($1->structtype->fields) {
           dc->type->structtype = $1->structtype;
        } else {
          HASHTABLE* ht;
          if($1->tb & STRUCTVAL) {
            ht = scopepeek(ctx)->forwardstructs;
          } else if($1->tb & UNIONVAL) {
            ht = scopepeek(ctx)->forwardunions;
          } else {
            fprintf(stderr, "Error: forward declaration of unknown type %s at %s %d.%d-%d.%d\n", $1->structtype->name, dapeek(file2compile), locprint(@$));
            continue;
          }
          DYNARR* da = search(ht, $1->structtype->name);
          dapush(da, &(dc->type->structtype));
        }
      }
    }}
| "struct" structbody ';' {
    $$ = dactor(1);
    IDTYPE* tt = malloc(sizeof(IDTYPE));
    tt->structtype = structor(NULL, $2);
    tt->pointerstack = NULL;
    tt->tb = STRUCTVAL | ANONMEMB;
    DECLARATION* dec = malloc(sizeof(DECLARATION));
    dec->type = tt;
    dec->varname = NULL;
    dapush($$, dec);
    }
| "union" structbody ';' {
    $$ = dactor(1);
    IDTYPE* tt = malloc(sizeof(IDTYPE));
    tt->uniontype= unionctor(NULL, $2);
    tt->pointerstack = NULL;
    tt->tb = UNIONVAL | ANONMEMB;
    DECLARATION* dec = malloc(sizeof(DECLARATION));
    dec->type = tt;
    dec->varname = NULL;
    dapush($$, dec);
    };
cs_decls:
  cs_decls ',' sdecl {$$ = $1; dapush($$, $3);}
| sdecl {$$ = dactor(8); dapush($$, $1);};
sdecl: 
  declarator {$$ = $1;}
| declarator ':' esc {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $3));}
| ':' esc {$$ = mkdeclaration(NULL); dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $2));};
fullenum:
  "enum" SYMBOL {
    if(!scopesearch(ctx, M_ENUM, $2)) {
      if(!queryval(scopepeek(ctx)->forwardenums, $2)) {
        add2scope(scopepeek(ctx), $2, M_STRUCT, NULL);
        insert(scopepeek(ctx)->forwardenums, $2, dactor(16));
      }
    } else {
      fprintf(stderr, "Error: redefinition of enum %s at %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
    }
    } enumbody {
    $$ = enumctor($2, $4); 
    add2scope(scopepeek(ctx), $2, M_ENUM, $$);
    defbackward(ctx, M_ENUM, $2, $$);
    };
enum:
  fullenum {$$ = $1;}
| "enum" enumbody {$$ = enumctor(NULL, $2);}
| "enum" SYMBOL {
    $$ = (ENUM*) scopesearch(ctx, M_ENUM, $2);
    if(!$$) {
      if(queryval(scopepeek(ctx)->forwardenums, $2)) {
        $$ = malloc(sizeof(ENUM));
        $$->name = $2;
        $$->fields = NULL;
      } else {
        fprintf(stderr, "Error: reference to undefined enum %s at %s %d.%d-%d.%d\n", $2, dapeek(file2compile), locprint(@$));
      }
    }};
enumbody:
  '{' enums commaopt '}' {$$ = $2;};
enums:
  SYMBOL {$$ = dactor(256);
    EXPRESSION* const0 = ct_intconst_expr(0);
    dapush($$, genenumfield($1,const0)); 
    add2scope(scopepeek(ctx), $1, M_ENUM_CONST, const0);
    }
| SYMBOL '=' esc {$$ = dactor(256);
    dapush($$, genenumfield($1,$3)); 
    add2scope(scopepeek(ctx), $1, M_ENUM_CONST, $3);
    }
| enums ',' SYMBOL {
    $$ = $1; 
    EXPRESSION* prevexpr = ((ENUMFIELD*) dapeek($$))->value;
    EXPRESSION* newexpr = malloc(sizeof(EXPRESSION));
    switch(prevexpr->type) {
      case INT:
        memcpy(newexpr, prevexpr, sizeof(EXPRESSION));
        ++(newexpr->intconst);
        break;
      case UINT:
        memcpy(newexpr, prevexpr, sizeof(EXPRESSION));
        ++(newexpr->uintconst);
        break;
      case ADD: ;
        memcpy(newexpr, daget(prevexpr->params, 0), sizeof(EXPRESSION));
        if(newexpr->type == INT) {
          ++(newexpr->intconst);
          break;
        }
      default: //TODO: clone whole expression tree
        newexpr->type = ADD;
        dapush(newexpr->params, ct_intconst_expr(1));
        dapush(newexpr->params, prevexpr);
    }
    dapush($$, genenumfield($3, newexpr));
    add2scope(scopepeek(ctx), $3, M_ENUM_CONST, newexpr); //TODO: Confirm no collisions
    }
| enums ',' SYMBOL '=' esc {$$ = $1;
    dapush($$, genenumfield($3,$5)); 
    add2scope(scopepeek(ctx), $3, M_ENUM_CONST, $5);
    };
commaopt: ',' | %empty;
%%
int yyerror(char* s){
  //printf("\ncolumn: %d\n%s\n", yylloc->first_column, s);
  return 0;
}

