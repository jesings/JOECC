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
%token<str> IDENTIFIER 
%token<dstr> STRING_LITERAL;
%token<idvariant> TYPE_NAME;
%token<exprvariant> ENUM_CONST;

%code requires{
  #include <stdint.h>
  #include <stdio.h>
  #include "compintern.h"


  #define aget(param, index) ((INITIALIZER*) (param)->arr[(index)])
  #define dget(param, index) ((DECLARATION*) (param)->arr[(index)])
  #define YYDEBUG 1
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
%type<exprvariant> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee
%type<stmtvariant> statement compound_statement
%type<arrvariant> statements_and_initializers struct_decls struct_decl cs_decls enums escl abstract_ptr params cs_inits cs_minutes initializer program array_literal structbody enumbody
%type<unionvariant> union
%type<structvariant> struct
%type<enumvariant> enum
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
      if(!search(scopepeek(ctx)->members, aget($1, i)->decl->varname)) {
        add2scope(scopepeek(ctx), aget($1, i)->decl->varname, M_VARIABLE, aget($1, i)->expr);
      }
      else {}
      dapush($$, gtb(0, $1));
    }
    free($1);
  }
| program function {
    $$ = $1;
    if(!search(ctx->funcs, $2->name)) {
      insert(ctx->funcs, $2->name, $2);
      dapush($$, gtb(1, $2));
    }
    else {}
  }
| program initializer {
    $$ = $1;
    for(int i = 0; i < $2->length; i++) {
      if(!search(scopepeek(ctx)->members, aget($2, i)->decl->varname)) {
        add2scope(scopepeek(ctx), aget($2, i)->decl->varname, M_VARIABLE, aget($2, i)->expr);
      }
      else {}
      dapush($$, gtb(0, $2));
    }
    free($2);
  };
initializer:
"typedef" type/*bs*/ cs_minutes ';' {
  SCOPE* current = scopepeek(ctx);
  for(int i = 0; i < $3->length; i++) {
    dget($3, i)->type->tb |= $2->tb; 
    IDTYPE* idt = malloc(sizeof(IDTYPE));
    memcpy(idt, $2, sizeof(IDTYPE));
    if(idt->pointerstack) {
      idt->pointerstack = daclone($2->pointerstack);
      idt->pointerstack = damerge(idt->pointerstack, dget($3, i)->type->pointerstack);
    }
    add2scope(current, dget($3, i)->varname, M_TYPEDEF, idt);
    free(dget($3, i)->type);
    free(dget($3, i));
  }
  free($2->pointerstack);
  free($2);
  $$ = dactor(0);
  }
| type/*bs*/ cs_inits ';' {
  SCOPE* current = scopepeek(ctx);
  $$ = $2;
  for(int i = 0; i < $$->length; i++) {
    aget($$, i)->decl->type->tb |= $1->tb; 
    if($1->pointerstack) {
      DYNARR* nptrst = daclone($1->pointerstack);
      aget($$, i)->decl->type->pointerstack = damerge(nptrst, aget($$, i)->decl->type->pointerstack);
    }
    if($1->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)) {
      aget($$, i)->decl->type->structtype = $1->structtype;
    }
    add2scope(current, aget($$, i)->decl->varname, M_VARIABLE, aget($$, i)->decl->type);
  }
  free($1->pointerstack);
  free($1);
}
| "struct" IDENTIFIER ';' {$$ = dactor(0); add2scope(scopepeek(ctx), $2, M_STRUCT, NULL);}
| "enum" IDENTIFIER ';' {$$ = dactor(0); add2scope(scopepeek(ctx), $2, M_ENUM, NULL);}
| "union" IDENTIFIER ';' {$$ = dactor(0); add2scope(scopepeek(ctx), $2, M_UNION, NULL);}
| "struct" IDENTIFIER '{' struct_decls '}' ';' {$$ = dactor(0); add2scope(scopepeek(ctx), $2, M_STRUCT, structor($2, $4));}
| "enum" IDENTIFIER '{' enums '}' ';' {$$ = dactor(0); add2scope(scopepeek(ctx), $2, M_ENUM, enumctor($2, $4));}
| "union" IDENTIFIER '{' struct_decls '}' ';' {$$ = dactor(0); add2scope(scopepeek(ctx), $2, M_UNION, unionctor($2, $4));}
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
  type/*bs*/ declarator {
    $$ = $2; 
    if($1->pointerstack) 
      $1->pointerstack = damerge($1->pointerstack, $$->type->pointerstack); 
    else 
      $1->pointerstack = $$->type->pointerstack;
    free($$->type); 
    $$->type = $1;
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
  TYPE_NAME {$$ = malloc(sizeof(IDTYPE)); memcpy($$, $1, sizeof(IDTYPE));}
| typem {$$ = $1;}
| types1 typem {$$ = $2; $$->tb |= $1;}
| types1 TYPE_NAME {$$ = malloc(sizeof(IDTYPE)); memcpy($$, $2, sizeof(IDTYPE)); $$->tb |= $1;/*TODO: extract, and duplicate*/ }
| types2 typem {$$ = $2; $$->tb |= $1;}
| types2 TYPE_NAME {$$ = malloc(sizeof(IDTYPE)); memcpy($$, $2, sizeof(IDTYPE)); $$->tb |= $1;/*TODO: extract, and duplicate*/ };
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
  if($2->pointerstack)
    $2->pointerstack = damerge($2->pointerstack, $3);
  else
    $2->pointerstack = $3;
  $$ = ct_cast_expr($2, $5);}
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
| esp '.' IDENTIFIER {$$ = ct_binary_expr(DOTOP, $1, ct_ident_expr($3));}
| esp  "->" IDENTIFIER {$$ = ct_binary_expr(ARROW, $1, ct_ident_expr($3));}
| esu {$$ = $1;};
esu:
  '(' expression ')' {$$ = $2;}
| multistring {$$ = ct_strconst_expr($1->strptr); free($1);}
| INTEGER_LITERAL {$$ = $1.sign ? ct_intconst_expr($1.num) : ct_uintconst_expr($1.num);}
| ENUM_CONST {$$ = $1;}
| FLOAT_LITERAL {$$ = ct_floatconst_expr($1);}
| IDENTIFIER {$$ = ct_ident_expr($1);}
| array_literal {$$ = ct_array_lit($1);}
| error {$$ = ct_nop_expr(); 
  extern DYNARR* file2compile;
  fprintf (stderr, "%d.%d-%d.%d in %s: error encountered\n",
           @1.first_line, @1.first_column,
           @1.last_line, @1.last_column,
           dapeek(file2compile));
           /*TODO: print error, location, etc.*/};
escl:
  esc {$$ = dactor(32); dapush($$, $1);}
| escl ',' esc {$$ = $1; dapush($$, $3); };

array_literal:
  '{' expression '}' {$$ = e2dynarr($2);}
| '{' expression ',' '}' {$$ = e2dynarr($2);};

multistring:
  STRING_LITERAL {$$ = $1;}
| STRING_LITERAL STRING_LITERAL {$$ = $1; dscat($1, $2->strptr, $2->lenstr); free($2->strptr); free($2);}

function:
  type/*bs*/ declarator compound_statement {
    DYNARR* parammemb;
    struct declarator_part* dp = dapop($2->type->pointerstack);
    if($1->pointerstack)
      $1->pointerstack = damerge($1->pointerstack, $2->type->pointerstack);
    else
      $1->pointerstack = $2->type->pointerstack;
    if(dp->params)
      parammemb = dp->params;
    else
      parammemb = dactor(0);
    $$ = ct_function($2->varname, $3, parammemb, $1);
    free($2);
    /*check that it is in fact a param spec*/
  }
| error compound_statement {
  extern DYNARR* file2compile;
  fprintf (stderr, "%d.%d-%d.%d in %s: error encountered in function definition\n",
           @1.first_line, @1.first_column,
           @1.last_line, @1.last_column,
           dapeek(file2compile));
  };
statement:
  compound_statement {$$ = $1;}
|  IDENTIFIER ':' /*statement*/ {$$ = mklblstmt($1/*, $3*/); /* not sure if necessary*/ add2scope(scopepeek(ctx), $1, M_LABEL, NULL);}
| "case" esc ':' /*statement*/ {$$ = mkcasestmt($2/*, $4*/); add2scope(scopepeek(ctx), NULL/*no clue what this should be*/, M_CASE/*?*/, NULL);}
| "default" ':' statement {$$ = mkdefaultstmt($3);add2scope(scopepeek(ctx), "default", M_CASE/*?*/, NULL);}/*case labels are scoped and regular labels arent?? This will be difficult*/
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7);}
| "switch" '(' expression ')' compound_statement {$$ = mklsstmt(SWITCH, $3, $5);}
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2);}
| "for" '(' dee  ee ';' ee ')' statement {$$ = mkforstmt($3, $4, $6, $8);}
| "goto" IDENTIFIER ';' {$$ = mkgotostmt($2);/*find label within scopes at some point, probably not now though*/}
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
| '{' statements_and_initializers '}' {scopepush(ctx); $$ = mkcmpndstmt($2); scopepop(ctx);};
statements_and_initializers:
  initializer {$$ = dactor(4096); dapush($$,soii($1));}
| statement {$$ = dactor(4096); dapush($$,sois($1));}
| statements_and_initializers initializer {$$ = $1; dapush($$,soii($2));}
| statements_and_initializers statement {$$ = $1; dapush($$,sois($2));};

/*for struct enum union make sure no redefinitions are happening*/
union:
  "union" IDENTIFIER structbody {$$ = unionctor($2, $3); add2scope(scopepeek(ctx), $2, M_UNION, $$);}
| "union" structbody  {$$ = unionctor(NULL, $2);}
| "union" IDENTIFIER {$$ = (UNION*) search(scopepeek(ctx)->unions, $2);};
struct:
  "struct" IDENTIFIER structbody {$$ = structor($2, $3); add2scope(scopepeek(ctx), $2, M_STRUCT, $$);}
| "struct" structbody {$$ = structor(NULL, $2);}
| "struct" IDENTIFIER {$$ = (STRUCT*) search(scopepeek(ctx)->structs, $2);};
structbody:
  '{' struct_decls '}' {$$ = $2;}
| '{' struct_decls ',' '}' {$$ = $2;};
struct_decls:
  struct_decl {$$ = $1;}
| struct_decls struct_decl {$$ = damerge($1, $2);};
struct_decl:
  type cs_decls ';' {
    $$ = $2; 
    for(int i = 0; i < $2->length; i++) {
      dget($$, i)->type->tb |= $1->tb; 
      if($1->pointerstack) {
        DYNARR* nptr = daclone($1->pointerstack);
        dget($$, i)->type->pointerstack = damerge(nptr, dget($$, 0)->type->pointerstack);
      }
      if($1->tb & (ENUMVAL | STRUCTVAL | UNIONVAL))
         dget($$, 0)->type->structtype = $1->structtype;
    }
    };
cs_decls:
  cs_decls ',' sdecl {$$ = $1; dapush($$, $3);}
| sdecl {$$ = dactor(8); dapush($$, $1);};
sdecl: 
  declarator {$$ = $1;}
| declarator ':' esc {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $3));}
| ':' esc {$$ = mkdeclaration(NULL); dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $2));};
enum:
  "enum" IDENTIFIER enumbody {$$ = enumctor($2, $3); add2scope(scopepeek(ctx), $2, M_ENUM, $$);}
| "enum" enumbody {$$ = enumctor(NULL, $2);}
| "enum" IDENTIFIER {$$ = (ENUM*) search(scopepeek(ctx)->enums, $2);/*TODO: check validity*/};
enumbody:
  '{' enums '}' {$$ = $2;}
| '{' enums ',' '}' {$$ = $2;};
enums:
  IDENTIFIER {$$ = dactor(256);
    dapush($$, genenumfield($1,ct_intconst_expr(0))); 
    add2scope(scopepeek(ctx), $1, M_ENUM_CONST, dapeek($$));
    }
| IDENTIFIER '=' esc {$$ = dactor(256); 
    dapush($$, genenumfield($1,$3)); 
    add2scope(scopepeek(ctx), $1, M_ENUM_CONST, $3);
    }
| enums ',' IDENTIFIER {$$ = $1; 
    dapush($$, genenumfield($3,ct_binary_expr(ADD,ct_intconst_expr(1),dapeek($$)))); 
    add2scope(scopepeek(ctx), $3, M_ENUM_CONST, dapeek($$));
    }
| enums ',' IDENTIFIER '=' esc {$$ = $1;
    dapush($$, genenumfield($3,$5)); 
    add2scope(scopepeek(ctx), $3, M_ENUM_CONST, $5);
    /*TODO: somehow confirm no collisions*/
    };
%%
#include <stdio.h>
int yyerror(char* s){
  //printf("\ncolumn: %d\n%s\n", yylloc->first_column, s);
  return 0;
}

