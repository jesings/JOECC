%locations
%token<unum> UNSIGNED_LITERAL;
%token<snum> INTEGER_LITERAL;
%token SHLTK "<<" SHRTK ">>" LE "<=" GE ">=" EQTK "==" NEQTK "!=" AND "&&" OR "||" 

%token<dbl> FLOAT_LITERAL;
%token<str> SYMBOL TYPE_NAME
%token<dstr> STRING_LITERAL;

%token ARROWTK "->" INC "++" DEC "--" DIV_GETS "/=" MUL_GETS "*=" MOD_GETS "%="
%token ADD_GETS "+=" SUB_GETS "-=" SHL_GETS "<<=" SHR_GETS ">>=" AND_GETS "&=" 
%token XOR_GETS "^=" OR_GETS "|=" ELLIPSIS "..."

%token TYPEDEF "typedef" STATIC "static" EXTERN "extern" CHAR "char" VOID "void"
%token INT8 "int8" INT16 "int16" INT32 "int32" INT64 "int64" BYTE "byte"
%token DBYTE "dbyte" QBYTE "qbyte" OBYTE "obyte" SINGLE "single" DOUBLE "double" 
%token CASETK "case" DEFAULTTK "default" IF "if" ELSE "else" SWITCHTK "switch"
%token WHILE "while" DO "do" FOR "for" GOTO "goto" CONTINUE "continue" 
%token BREAK "break" RETURN "return" SIZEOF "sizeof" UNSIGNED "unsigned"
%token STRUCTTK "struct" ENUMTK "enum" UNIONTK "union" SIGNED "signed"
%token CONST "const" VOLATILE "volatile" RESTRICT "restrict" INLINE "inline"

%right THEN "else"
/*probably could do this smarter with redesign*/
%start program
%define parse.trace
%define parse.assert
%define parse.error verbose

%code requires{
  #include <stdint.h>
  #include <stdio.h>
  #include <assert.h>
  #include "compintern.h"
  #include "dynarr.h"
  #include "parallel.h"
  extern DYNARR* file2compile;


  #define aget(param, index) ((INITIALIZER*) (param)->arr[(index)])
  #define dget(param, index) ((DECLARATION*) (param)->arr[(index)])
  //TODO: Compound literals, array literals
  //TODO: Consider designated initializers?
  //TODO: do enums right
}

%{
  extern struct lexctx* ctx;
  int yylex(void);
  int yyerror(const char* s);
%}

%union {
  long snum;
  unsigned long unum;
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
  PARALLEL* paravariant;
  void* vvar;
}

%type<str> generic_symbol
%type<dstr> multistring
%type<integert> typemsign
%type<typevariant> types1 types2 types1o
%type<idvariant> typem typews1 type typemintkw inttypem namelesstype
%type<exprvariant> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee escoa
%type<stmtvariant> statement compound_statement
%type<arrvariant> statements_and_initializers soiorno struct_decls struct_decl cs_decls enums escl escoal abstract_ptr cs_inits cs_minutes initializer array_literal structbody enumbody nameless
%type<unionvariant> union fullunion
%type<structvariant> struct fullstruct
%type<enumvariant> enum fullenum
%type<declvariant> declarator declname param_decl sdecl fptr spefptr
%type<funcvariant> function
%type<firforvariant> dee
%type<paravariant> params
%type<vvar> program

%%
program:
  function {
    $$ = NULL;
    insert(ctx->funcs, $1->name, $1);
    //dapush($$, gtb(1, $1));
    //first function can't be a redefinition
  }
| initializer {
    $$ = NULL;
    if($1) {
      for(int i = 0; i < $1->length; i++) {
        INITIALIZER* a2 = daget($1, i);
        if(!scopequeryval(ctx, M_VARIABLE, a2->decl->varname)) {
          IDENTIFIERINFO* id = malloc(sizeof(IDENTIFIERINFO));
          id->index = -1;
          id->name = a2->decl->varname;
          id->type = a2->decl->type;
          add2scope(ctx, a2->decl->varname, M_GLOBAL, id);
          dapush(ctx->globals, a2);//check if function prototype, only insert if not? don't free decl? if not func?
        } else {
          assert(0);//HOW??
        }
        free(a2->decl);
        if(a2->expr) rfreexpr(a2->expr);
      }
      dadtorfr($1);
    }
  }
| program function {
    $$ = $1;
    char* cs = $2->name;
    if(!search(ctx->funcs, cs)) {
      insert(ctx->funcs, cs, $2);
    } else {
      fprintf(stderr, "Error: redefinition of function %s in %s %d.%d-%d.%d\n", $2->name, locprint(@$));
    }
  }
| program initializer {
    $$ = $1;
    if($2) {
      for(int i = 0; i < $2->length; i++) {
        INITIALIZER* a2 = daget($2, i);
        if(!scopequeryval(ctx, M_VARIABLE, a2->decl->varname)) {
          IDENTIFIERINFO* id = malloc(sizeof(IDENTIFIERINFO));
          id->index = -1;
          id->name = a2->decl->varname;
          id->type = a2->decl->type;
          add2scope(ctx, a2->decl->varname, M_GLOBAL, id);
          dapush(ctx->globals, a2);//check if function prototype, only insert if not? don't free decl? if not func?
        } else {
          IDENTIFIERINFO* id = scopesearch(ctx, M_VARIABLE, a2->decl->varname);
          if(!(id->type->tb & EXTERNNUM)) {
            fprintf(stderr, "Error: redefinition of global symbol %s in %s %d.%d-%d.%d\n", a2->decl->varname, locprint(@$));
          } else {
            free(a2->decl->varname);
            freetype(id->type);
            id->type = a2->decl->type;
          }
        }
        free(a2->decl);
        if(a2->expr) rfreexpr(a2->expr);//don't free like this
      }
      dadtorfr($2);
    }
  };
initializer:
"typedef" type cs_minutes ';' {
  DECLARATION* dc;
  DYNARR* da = NULL;
  if($2->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
    if(!$2->structtype->fields) {
      HASHTABLE* ht;
      if($2->tb & STRUCTVAL) {
        ht = scopepeek(ctx)->forwardstructs;
      } else if($2->tb & UNIONVAL) {
        ht = scopepeek(ctx)->forwardunions;
      } else {
        fprintf(stderr, "Error: forward declaration of unknown type %s at %s %d.%d-%d.%d\n", $2->structtype->name, locprint(@$));
      }
      da = search(ht, $2->structtype->name);
      dapop(da);
      free($2->structtype->name);
      free($2->structtype);
    }
  }
  for(int i = 0; i < $3->length; i++) {
    dc = dget($3, i);
    dc->type->tb |= $2->tb;
    if($2->pointerstack) {
      DYNARR* nptr = ptrdaclone($2->pointerstack);
      if(dc->type->pointerstack)
        dc->type->pointerstack = damerge(nptr, dc->type->pointerstack);
      else
        dc->type->pointerstack = nptr;
    }
    if($2->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
      if(da) {
        dapush(da, &(dc->type->structtype));
      } else {
         dc->type->structtype = $2->structtype;
      }
    }
    add2scope(ctx, dc->varname, M_TYPEDEF, dc->type);
    free(dc->varname);
    free(dc);
  }

  dadtor($3);
  free($2);
  $$ = NULL;
  }
| type cs_inits ';' {
  $$ = $2;
  INITIALIZER* ac;
  DYNARR* da = NULL;
  if($1->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
    if(!$1->structtype->fields) {
      HASHTABLE* ht;
      if($1->tb & STRUCTVAL) {
        ht = scopepeek(ctx)->forwardstructs;
      } else if($1->tb & UNIONVAL) {
        ht = scopepeek(ctx)->forwardunions;
      } else {
        fprintf(stderr, "Error: forward declaration of unknown type %s at %s %d.%d-%d.%d\n", $1->structtype->name, locprint(@$));
      }
      da = search(ht, $1->structtype->name);
      dapop(da);
      free($1->structtype->name);
      free($1->structtype);
    }
  }
  for(int i = 0; i < $$->length; i++) {
    ac = aget($$, i);
    ac->decl->type->tb |= $1->tb; 
    if($1->pointerstack) {
      DYNARR* nptrst = ptrdaclone($1->pointerstack);
      if(ac->decl->type->pointerstack)
        ac->decl->type->pointerstack = damerge(nptrst, ac->decl->type->pointerstack);
      else 
        ac->decl->type->pointerstack = nptrst;
    }
    if($1->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)) {
      if(da) {
        dapush(da, &(ac->decl->type->structtype));
      } else {
         ac->decl->type->structtype = $1->structtype;
      }
    }
    if(!ac->decl->type->pointerstack->length ||
       ((((struct declarator_part*) dapeek(ac->decl->type->pointerstack))->type != PARAMSSPEC) &&
       (((struct declarator_part*) dapeek(ac->decl->type->pointerstack))->type != NAMELESS_PARAMSSPEC))) {
      if(ctx->func) {
        HASHTABLE* ht = scopepeek(ctx)->members;
        SCOPEMEMBER* sm = search(ht, ac->decl->varname);
        if(!sm || (sm->mtype == M_VARIABLE && (sm->idi->type->tb & EXTERNNUM))) {
          add2scope(ctx, ac->decl->varname, M_VARIABLE, ac->decl->type);
          ac->decl->varid = ((SCOPEMEMBER*) search(ht, ac->decl->varname))->idi->index;
        } else {
          fprintf(stderr, "Error: redefinition of identifier %s in %s %d.%d-%d.%d\n", ac->decl->varname, locprint(@$));
        }
      } else {
      }
    } else {
      //TODO: ensure no prior definition
      insert(ctx->funcs, ac->decl->varname, NULL);
    }
    if(ac->expr && ac->expr->type == ARRAY_LIT) {
      DYNARR* pointy = ac->decl->type->pointerstack;
      if(ac->decl->type->tb & (STRUCTVAL | UNIONVAL)) {
        assert(!(pointy && pointy->length));
        /*TODO: handle struct initializer case*/
      } else {
        assert(pointy && pointy->length);
        int arrdim = 0;
        for(int i = pointy->length - 1; i >= 0; i--, arrdim++) {
          struct declarator_part* pointtop = daget(pointy, i);
          if(pointtop->type != ARRAYSPEC) break;
        }
        assert(arrdim);
        process_array_lit(ac->decl->type, ac->expr, arrdim);
      }
    }
  }
  free($1);
  }
| "struct" SYMBOL ';' {
  $$ = NULL;
  if(!scopequeryval(ctx, M_STRUCT, $2)) {
    add2scope(ctx, $2, M_STRUCT, NULL);
    insert(scopepeek(ctx)->forwardstructs, $2, dactor(16));
  }
  free($2);
  }
| "union" SYMBOL ';' {
  $$ = NULL;
  if(!scopequeryval(ctx, M_UNION, $2)) {
    add2scope(ctx, $2, M_UNION, NULL);
    insert(scopepeek(ctx)->forwardunions, $2, dactor(16));
  }
  free($2);
  }
| "enum" enumbody ';' {$$ = NULL; enumctor(NULL, $2, ctx);}
| fullstruct ';' {$$ = NULL;}
| fullenum ';' {$$ = NULL;}
| fullunion ';' {$$ = NULL;};
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
| abstract_ptr fptr {$$ = $2; $2->type->pointerstack = damerge($2->type->pointerstack, $1);}
| declname {$$ = $1;}
| fptr {$$ = $1;};
declname:
  SYMBOL {$$ = mkdeclaration($1);}
| '(' declname ')' {$$ = $2;}
| declname '[' ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, NULL));}
| declname '[' expression ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, $3));/*foldconst*/}
| declname'(' ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(NAMELESS_PARAMSSPEC, NULL));}
| declname '(' nameless ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(NAMELESS_PARAMSSPEC, $3));}
| declname '(' params ')' {$$ = $1; dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, $3));}
| declname '(' nameless ',' "..." ')' {$$ = $1; 
    dapush($3, NULL);//represent variadic with trailing null?
    dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, $3));
    }
| declname '(' params ',' "..." ')' {$$ = $1; 
    pinsert($3, "...", NULL); 
    dapush($$->type->pointerstack, mkdeclpart(PARAMSSPEC, $3));
    };
fptr:
  '(' abstract_ptr SYMBOL ')' {$$ = mkdeclaration($3); $$->type->pointerstack = damerge($$->type->pointerstack, $2);}
| '(' '(' abstract_ptr ')' SYMBOL ')' {$$ = mkdeclaration($5); $$->type->pointerstack = damerge($$->type->pointerstack, $3);}
| '(' fptr ')' {$$ = $2;}
| fptr'[' ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, NULL));}
| fptr'[' expression ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, $3));/*foldconst*/}
| fptr'(' ')' {$$ = $1;
    DYNARR* da = dactor(1);
    dapush($$->type->pointerstack, mkdeclpart(NAMELESS_PARAMSSPEC, NULL));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| fptr '(' nameless ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    dapush(da, mkdeclpart(NAMELESS_PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| fptr '(' params ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    dapush(da, mkdeclpart(PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| fptr '(' nameless ',' "..." ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    dapush($3, NULL);//represent variadic with trailing null?
    dapush(da, mkdeclpart(PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| fptr '(' params ',' "..." ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    pinsert($3, "...", NULL); 
    dapush(da, mkdeclpart(PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    };
spefptr:
  '(' abstract_ptr SYMBOL ')' {$$ = mkdeclaration($3); $$->type->pointerstack = damerge($$->type->pointerstack, $2);}
| '(' '(' abstract_ptr ')' SYMBOL ')' {$$ = mkdeclaration($5); $$->type->pointerstack = damerge($$->type->pointerstack, $3);}
| '(' abstract_ptr ')' {$$ = mkdeclaration(NULL); $$->type->pointerstack = damerge($$->type->pointerstack, $2);}
| '(' spefptr ')' {$$ = $2;}
| spefptr'[' ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, NULL));}
| spefptr'[' expression ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, $3));/*foldconst*/}
| spefptr'(' ')' {$$ = $1;
    DYNARR* da = dactor(1);
    dapush($$->type->pointerstack, mkdeclpart(NAMELESS_PARAMSSPEC, NULL));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| spefptr '(' nameless ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    dapush(da, mkdeclpart(NAMELESS_PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| spefptr '(' params ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    dapush(da, mkdeclpart(PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| spefptr '(' nameless ',' "..." ')' {$$ = $1; 
    DYNARR* da = dactor(1);
    dapush($3, NULL);//represent variadic with trailing null?
    dapush(da, mkdeclpart(PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    }
| spefptr '(' params ',' "..." ')' {$$ = $1;
    DYNARR* da = dactor(1);
    pinsert($3, "...", NULL); 
    dapush(da, mkdeclpart(PARAMSSPEC, $3));
    $$->type->pointerstack = damerge(da, $$->type->pointerstack);
    };
params:
  param_decl {
   $$ = paralector(); 
   pinsert($$, $1->varname, $1);
   }
| params ',' param_decl {
    $$ = $1;
    if(psearch($$, $3->varname)) {
      fprintf(stderr, "Error: param with duplicate name %s in %s %d.%d-%d.%d\n", $3->varname,  locprint(@$));
    } else {
      pinsert($$, $3->varname, $3);
    }};
param_decl:
  type declarator {
    $2->type->tb |= $1->tb;
    if($1->pointerstack) {
      DYNARR* nptr = ptrdaclone($1->pointerstack);
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
        }
        DYNARR* da = search(ht, $1->structtype->name);
        dapush(da, &($2->type->structtype));
        free($1->structtype->name);
        free($1->structtype);
      }
    }
    free($1);
    $$ = $2; 
    };
nameless:
  namelesstype {$$ = dactor(16); dapush($$, $1);/*read only*/ }
| nameless ',' namelesstype {$$ = $1; dapush($$, $3);/*read only*/ }
| params ',' namelesstype {$$ = dactor($1->ht->keys + 16); 
    for(int i = 0; i < $1->da->length; i++) {
      IDTYPE* idt = pisearch($1, i);
      dapush($$, idt);
    }
    dapush($$, $3);
    dadtorfr($1->da);
    htdtor($1->ht);
    free($1);
    }
| nameless ',' param_decl {$$ = $1; dapush($$, $3->type); free($3->varname); free($3);/*read only*/ };
namelesstype:
  type {$$ = $1; if($$->pointerstack) $$->pointerstack = ptrdaclone($$->pointerstack);}
| type abstract_ptr {$$ = $1;
    if($$->pointerstack) {
      $$->pointerstack = damerge(ptrdaclone($1->pointerstack), $2);
    } else {
      $$->pointerstack = $2;
    }};
typemsign:
  "signed" {$$ = 0;}
| "unsigned" {$$ = UNSIGNEDNUM;};
typemintkw:
  "char" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1;}
| "int8" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1;}
| "int16" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2;}
| "int32" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4;}
| "int64" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8;}
| "int16" "int32" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2;/*garbage feature only here for compatibility*/}
| "int64" "int32" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8;/*garbage feature only here for compatibility*/}
| "int64" "int64" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8;/*garbage feature only here for compatibility*/}
| "int64" "int64" "int32" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8;/*garbage feature only here for compatibility*/};
inttypem:
  typemintkw {$$ = $1;}
| typemsign typemintkw {$$ = $2; $$->tb |= $1;}
| typemintkw typemsign {$$ = $1; $$->tb |= $2;/*garbage feature only here for compatibility (long unsigned)*/}
| typemintkw typemsign typemintkw {$$ = $1; $$->tb |= $2; free($3);/*even more garbage feature only here for compatibility (long unsigned int)*/};
typem:
  inttypem {$$ = $1;}
| "byte"  {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 1 | UNSIGNEDNUM;}
| "dbyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 2 | UNSIGNEDNUM;}
| "qbyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4 | UNSIGNEDNUM;}
| "obyte" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8 | UNSIGNEDNUM;}
| "void"  {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = VOIDNUM;}
| "single" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 4 | FLOATNUM;}
| "double" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 8 | FLOATNUM;}
| "int64" "double" {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = 10;/*garbage feature only here for compatibility(?)*/}
| struct {
    $$ = calloc(1, sizeof(IDTYPE)); 
    $$->structtype = $1;
    $$->tb = STRUCTVAL;
    if($1->fields == NULL) {
      DYNARR* da = (DYNARR*) search(scopepeek(ctx)->forwardstructs, $1->name);
      if(!da) 
        fprintf(stderr, "Error: struct %s undeclared in %s %d.%d-%d.%d\n", $1->name, locprint(@$));
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
        fprintf(stderr, "Error: union %s undeclared in %s %d.%d-%d.%d\n", $1->name, locprint(@$));
      else
        dapush(da, &($$->uniontype));
    }}
| enum {
    $$ = calloc(1, sizeof(IDTYPE)); 
    $$->enumtype = $1;
    $$->tb = ENUMVAL; 
    };
types1:
  "const" {$$ = CONSTNUM;}
| "volatile" {$$ = VOLATILENUM;}
| "restrict" {$$ = RESTRICTNUM;};
types2:
  "extern" {$$ = EXTERNNUM;}
| "static" {$$ = STATICNUM;}
| "inline" {$$ = INLINED;};
typews1:
  TYPE_NAME {
    $$ = malloc(sizeof(IDTYPE));
    IDTYPE* idt = scopesearch(ctx, M_TYPEDEF, $1);
    if(idt) {
      memcpy($$, idt, sizeof(IDTYPE));
    } else {
      fprintf(stderr, "Error: use of unknown type name %s in %s %d.%d-%d.%d\n", $1, locprint(@$));
    }
    free($1);
    }
| typem{ $$ = $1;}
| types1 typews1 {$$ = $2; $$->tb |= $1;}
| types2 typews1 {$$ = $2; $$->tb |= $1;} ;
type:
  typews1 {$$ = $1;}
| typews1 types1 {$$ = $1; $$->tb |= $2;};
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
  '(' type ')' esm {if($2->pointerstack) $2->pointerstack = ptrdaclone($2->pointerstack); $$ = ct_cast_expr($2, $4);}
| '(' type abstract_ptr ')' esm {
  if($2->pointerstack) {
    $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $3);
  } else {
    $2->pointerstack = $3;
  }
  $$ = ct_cast_expr($2, $5);}
| '(' type spefptr ')' esm {
  if($2->pointerstack) {
    $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $3->type->pointerstack);
  } else {
    $2->pointerstack = $3->type->pointerstack;
  }
  free($3->type);
  free($3);
  $$ = ct_cast_expr($2, $5);}
| '(' type abstract_ptr spefptr ')' esm {
  if($2->pointerstack) {
    $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $3);
  } else {
    $2->pointerstack = $3;
  }
  $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $4->type->pointerstack);
  free($4->type);
  free($4);
  $$ = ct_cast_expr($2, $6);}
| esca {$$ = $1;};
esca:
  "++" esca {$$ = ct_unary_expr(PREINC, $2);}
| "--" esca {$$ = ct_unary_expr(PREDEC, $2);}
| '+' esm {$$ = $2;}
| '-' esm {$$ = ct_unary_expr(NEG, $2);}
| '!' esm {$$ = ct_unary_expr(L_NOT, $2);}
| '~' esm {$$ = ct_unary_expr(B_NOT, $2);}
| '*' esm {$$ = ct_unary_expr(DEREF, $2);}
| '&' esm {$$ = ct_unary_expr(ADDR, $2);}
| "sizeof" '(' type abstract_ptr ')' {$$ = ct_uintconst_expr(sizeof(uintptr_t)); free($3); dadtorfr($4);}
| "sizeof" '(' type ')' {
    if($3->pointerstack && $3->pointerstack->length) {
      free($3);
      $$ = ct_uintconst_expr(sizeof(uintptr_t));
    } else {
    $$ = ct_sztype($3);
    }
    }
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
| INTEGER_LITERAL {$$ = ct_intconst_expr($1);}
| UNSIGNED_LITERAL {$$ = ct_uintconst_expr($1);}
| FLOAT_LITERAL {$$ = ct_floatconst_expr($1);}
| SYMBOL {
    EXPRESSION* expr = scopesearch(ctx, M_ENUM_CONST, $1);
    if(!expr) {
      $$ = ct_ident_expr(ctx, $1);
    } else {
      free($1);
      $$ = ct_intconst_expr(expr->intconst);
    }
    }
| error {
    $$ = ct_nop_expr(); 
    fprintf (stderr, "Malformed expression at %s %d.%d-%d.%d\n", locprint(@1));
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

function:
  type declarator <funcvariant>{
    PARALLEL* parammemb;
    struct declarator_part* dp = dapop($2->type->pointerstack);
    $2->type->tb |= $1->tb;
    if($1->pointerstack) {
      $2->type->pointerstack = damerge(ptrdaclone($1->pointerstack), $2->type->pointerstack);
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
        }
        DYNARR* da = search(ht, $1->structtype->name);
        dapush(da, &($2->type->structtype));
      }
    }
    if(!dp->params) {
      parammemb = paralector();
    } else if(dp->type == PARAMSSPEC) {
      parammemb = dp->params;
    } else if(dp->type == NAMELESS_PARAMSSPEC) {
      IDTYPE* prm = dp->nameless_params->arr[0];
      assert((dp->nameless_params->length == 1 && (!prm->pointerstack || prm->pointerstack->length == 0) && prm->tb == VOIDNUM) || !fprintf (stderr, "Function has unnamed parameters at %s %d.%d-%d.%d\n", locprint(@1)));
      dadtorcfr(dp->nameless_params, (void(*)(void*)) freetype);
      parammemb = paralector();
    } else {
      fprintf(stderr, "Function has malformed parameters at %s %d.%d-%d.%d\n", locprint(@1));
      assert(0);
    }
    $$ = ct_function($2->varname, NULL, parammemb, $2->type);
    //TODO: ensure compliant overwriting?
    IDENTIFIERINFO* fordecid;
    if((fordecid = scopesearch(ctx, M_VARIABLE, $2->varname))) {
      free(fordecid->name);
      freetype(fordecid->type);
      free(fordecid);
    }
    IDENTIFIERINFO* id = malloc(sizeof(IDENTIFIERINFO));
    id->index = -1;
    id->name = $2->varname;
    dp->type = PARAMSSPEC;
    dp->params = parammemb;
    dapush($2->type->pointerstack, dp);
    id->type = $2->type;
    rmpaircfr(scopepeek(ctx)->members, $2->varname, free); //no-op if not predefined
    add2scope(ctx, $2->varname, M_GLOBAL, id);
    free($2);
    free($1);
    ctx->func = $$;

    scopepush(ctx);
    for(int i = 0; i < parammemb->da->length; i++) {
      char* pname = daget(parammemb->da, i);
      DECLARATION* declwhole = (DECLARATION*) search(parammemb->ht, pname);
      add2scope(ctx, pname, M_VARIABLE, declwhole->type);
      declwhole->varid = ((SCOPEMEMBER*) search(scopepeek(ctx)->members, pname))->idi->index;
    }
    }
    '{' soiorno '}' {
    scopepop(ctx);
    $$ = $3;
    $$->body = mkcmpndstmt($5); 
    ctx->func = NULL;
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
    snprintf(caselbl, 128, "__joecc__%s__default_%d", ctx->func->name, (ctx->func->caseindex)++);
    $$ = mkdefaultstmt(ctx, caselbl);
    }
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7);}
| "switch" '(' expression ')' switch_midrule compound_statement {
    SWITCHINFO* swi = dapop(ctx->func->switchstack);
    $$ = mkswitchstmt($3, $6, swi);
    free(swi);
    }
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2);}
| "for" '(' {
    scopepush(ctx);
    } dee  ee ';' ee ')' statement {$$ = mkforstmt($4, $5, $7, $9); scopepop(ctx);}
| "goto" SYMBOL ';' {$$ = mkgotostmt($2);}
| "break" ';' {$$ = mkexprstmt(LBREAK,NULL);}
| "continue" ';' {$$ = mkexprstmt(LCONT,NULL);}
| "return" ';' {$$ = mkexprstmt(FRET,NULL);}
| "return" expression ';' {$$ = mkexprstmt(FRET,$2);}
| expression ';' {$$ = mkexprstmt(EXPR,$1);}
| ';' {$$ = mknopstmt();};
ee: 
  expression {$$ = $1;}
| %empty {$$ = ct_nop_expr();};
dee:
  initializer {if($1) {$$ = malloc(sizeof(EOI)); $$->isE = 0; $$->I = $1;} else {$$ = malloc(sizeof(EOI)); $$->isE = 1; $$->E = ct_nop_expr();};}
| ee ';' {$$ = malloc(sizeof(EOI)); $$->isE = 1; $$->E = $1;};
compound_statement:/*add new scope to scope stack, remove when done*/
  '{' compound_midrule soiorno'}' {
    $$ = mkcmpndstmt($3); 
    scopepop(ctx);
    };
compound_midrule: %empty {
    scopepush(ctx);
    };
statements_and_initializers:
  initializer {$$ = dactor(4096); if($1) dapush($$,soii($1));}
| statement {$$ = dactor(4096); dapush($$,sois($1));}
| statements_and_initializers initializer {$$ = $1; if($2) dapush($$,soii($2));}
| statements_and_initializers statement {$$ = $1; dapush($$,sois($2));};
soiorno:
  statements_and_initializers {$$ = $1;}
| %empty {$$ = NULL;};
switch_midrule:
  %empty {
    SWITCHINFO* swi = calloc(1, sizeof(SWITCHINFO));
    swi->cases = paralector();
    dapush(ctx->func->switchstack, swi);
    };

generic_symbol:
  SYMBOL {$$ = $1;}
| TYPE_NAME {$$ = $1;};
/*for struct enum union make sure no redefinitions are happening*/
fullunion:
  "union" generic_symbol {
    if(!scopesearch(ctx, M_UNION, $2)) {
      if(!queryval(scopepeek(ctx)->forwardunions, $2)) {
        add2scope(ctx, $2, M_UNION, NULL);
        insert(scopepeek(ctx)->forwardunions, $2, dactor(16));
      }
    } else {
      fprintf(stderr, "Error: redefinition of union %s at %s %d.%d-%d.%d\n", $2, locprint(@$));
    }} structbody {
    $$ = unionctor($2, $4, ctx); 
    rmpaircfr(scopepeek(ctx)->unions, $2, free); //no-op if not predefined
    add2scope(ctx, $2, M_UNION, $$); 
    defbackward(ctx, M_UNION, $2, $$);
    };
union:
  fullunion {$$ = $1;}
| "union" structbody  {$$ = unionctor(NULL, $2, ctx);}
| "union" generic_symbol {
    $$ = (UNION*) scopesearch(ctx, M_UNION, $2);
    if(!$$) {
      if(!queryval(scopepeek(ctx)->forwardunions, $2)) {
        add2scope(ctx, $2, M_UNION, NULL);
        insert(scopepeek(ctx)->forwardunions, $2, dactor(16));
      }
      $$ = malloc(sizeof(UNION));
      $$->name = $2;
      $$->fields = NULL;
    } else {
      free($2);
    }
    };
fullstruct:
  "struct" generic_symbol {
    if(!scopesearch(ctx, M_STRUCT, $2)) {
      if(!queryval(scopepeek(ctx)->forwardstructs, $2)) {
        add2scope(ctx, $2, M_STRUCT, NULL);
        insert(scopepeek(ctx)->forwardstructs, $2, dactor(16));
      }
    } else {
      fprintf(stderr, "Error: redefinition of struct %s at %s %d.%d-%d.%d\n", $2, locprint(@$));
    }} structbody {
    $$ = structor($2, $4, ctx); 
    rmpaircfr(scopepeek(ctx)->structs, $2, free); //no-op if not predefined
    add2scope(ctx, $2, M_STRUCT, $$);
    defbackward(ctx, M_STRUCT, $2, $$);
    };
struct:
  fullstruct {$$ = $1;}
| "struct" structbody {$$ = structor(NULL, $2, ctx);}
| "struct" generic_symbol {
    $$ = (STRUCT*) scopesearch(ctx, M_STRUCT, $2);
    if(!$$) {
      if(!queryval(scopepeek(ctx)->forwardstructs, $2)) {
        add2scope(ctx, $2, M_STRUCT, NULL);
        insert(scopepeek(ctx)->forwardstructs, $2, dactor(16));
      }
      $$ = malloc(sizeof(STRUCT));
      $$->name = $2;
      $$->fields = NULL;
    } else {
      free($2);
    }
    };
structbody: '{' {fakescopepush(ctx);} struct_decls '}' {$$ = $3; scopepop(ctx);};
struct_decls:
  struct_decl {$$ = $1;}
| struct_decls struct_decl {$$ = damerge($1, $2);};
struct_decl:
  type cs_decls ';' {
    for(int i = 0; i < $2->length; i++) {
      char* vn = dget($2, i)->varname;
      if(vn) {
        if(queryval(fakescopepeek(ctx)->fakescope, vn)) {
          fprintf(stderr, "Error: redefinition of struct or union member %s at %s %d.%d-%d.%d\n", vn, locprint(@$));
        } else {
          insert(fakescopepeek(ctx)->fakescope, vn, NULL);
        }
      }
    }
    $$ = $2; 
    DYNARR* da = NULL;
    if($1->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
      if(!$1->structtype->fields) {
        HASHTABLE* ht;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        } else {
          fprintf(stderr, "Error: forward declaration of unknown type %s at %s %d.%d-%d.%d\n", $1->structtype->name, locprint(@$));
        }
        da = search(ht, $1->structtype->name);
        dapop(da);
        free($1->structtype->name);
        free($1->structtype);
      }
    }
    
    for(int i = 0; i < $2->length; i++) {
      DECLARATION* dc = dget($$, i);
      dc->type->tb |= $1->tb; 
      if($1->pointerstack) {
        DYNARR* nptr = ptrdaclone($1->pointerstack);
        if(dc->type->pointerstack) {
          dc->type->pointerstack = damerge(nptr, dc->type->pointerstack);
        } else {
          dc->type->pointerstack = nptr;
        }
      }
      if($1->tb & (ENUMVAL | STRUCTVAL | UNIONVAL)) {
        if(da) {
          dapush(da, &(dc->type->structtype));
        } else {
           dc->type->structtype = $1->structtype;
        }
      }
    }
    free($1);
    }
| "struct" structbody ';' {
    for(int i = 0; i < $2->length; i++) {
      char* vn = dget($2, i)->varname;
      if(vn) {
        if(queryval(fakescopepeek(ctx)->fakescope, vn)) {
          fprintf(stderr, "Error: redefinition of struct or union member %s at %s %d.%d-%d.%d\n", vn, locprint(@$));
        } else {
          insert(fakescopepeek(ctx)->fakescope, vn, NULL);
        }
      }
    }
    $$ = dactor(1);
    IDTYPE* tt = malloc(sizeof(IDTYPE));
    tt->structtype = calloc(1, sizeof(STRUCT));
    tt->structtype->fields = $2;
    tt->pointerstack = NULL;
    tt->tb = STRUCTVAL | ANONMEMB;
    DECLARATION* dec = malloc(sizeof(DECLARATION));
    dec->type = tt;
    dec->varname = NULL;
    dapush($$, dec);
    }
| "union" structbody ';' {
    for(int i = 0; i < $2->length; i++) {
      char* vn = dget($2, i)->varname;
      if(vn) {
        if(queryval(fakescopepeek(ctx)->fakescope, vn)) {
          fprintf(stderr, "Error: redefinition of struct or union member %s at %s %d.%d-%d.%d\n", vn, locprint(@$));
        } else {
          insert(fakescopepeek(ctx)->fakescope, vn, NULL);
        }
      }
    }
    $$ = dactor(1);
    IDTYPE* tt = malloc(sizeof(IDTYPE));
    tt->uniontype = calloc(1, sizeof(UNION));
    tt->uniontype->fields = $2;
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
  "enum" generic_symbol {
    if(scopesearch(ctx, M_ENUM, $2)) {
      fprintf(stderr, "Error: redefinition of enum %s at %s %d.%d-%d.%d\n", $2, locprint(@$));
    }
    } enumbody {
    $$ = enumctor($2, $4, ctx); 
    add2scope(ctx, $2, M_ENUM, $$);
    };
enum:
  fullenum {$$ = $1;}
| "enum" enumbody {$$ = enumctor(NULL, $2, ctx);}
| "enum" generic_symbol  {
    $$ = (ENUM*) scopesearch(ctx, M_ENUM, $2);
    if(!$$) {
      fprintf(stderr, "Error: reference to undefined enum %s at %s %d.%d-%d.%d\n", $2, locprint(@$));
    }
    free($2);
    };
enumbody:
  '{' enums commaopt '}' {$$ = $2;};
enums:
  SYMBOL {$$ = dactor(256);
    EXPRESSION* const0 = ct_intconst_expr(0);
    dapush($$, genenumfield($1,const0)); 
    add2scope(ctx, $1, M_ENUM_CONST, const0);
    }
| SYMBOL '=' esc {$$ = dactor(256);
    ENUMFIELD* ef = genenumfield($1,$3);
    dapush($$, ef); 
    add2scope(ctx, $1, M_ENUM_CONST, ef->value);
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
        //fall through
      default:
        newexpr->type = ADD;
        dapush(newexpr->params, ct_intconst_expr(1));
        dapush(newexpr->params, rclonexpr(prevexpr));
    }
    dapush($$, genenumfield($3, newexpr));
    if(scopequeryval(ctx, M_ENUM_CONST, $3) ||
       scopequeryval(ctx, M_VARIABLE, $3)) {
      fprintf(stderr, "Error: redefinition of symbol %s as enum constant at %s %d.%d-%d.%d\n", $3, locprint(@$));
    } else {
      add2scope(ctx, $3, M_ENUM_CONST, newexpr);
    }
    }
| enums ',' SYMBOL '=' esc {$$ = $1;
    ENUMFIELD* ef = genenumfield($3,$5);
    dapush($$, ef); 
    if(scopequeryval(ctx, M_ENUM_CONST, $3) ||
       scopequeryval(ctx, M_VARIABLE, $3)) {
      fprintf(stderr, "Error: redefinition of symbol %s as enum constant at %s %d.%d-%d.%d\n", $3, locprint(@$));
    } else {
      add2scope(ctx, $3, M_ENUM_CONST, ef->value);
    }
    };
commaopt: ',' | %empty;
%%
int yyerror(const char* s){
  fprintf(stderr, "ERROR: %s %s %d.%d-%d.%d\n", s, locprint(yylloc));
  (void)s;
  return 0;
}

