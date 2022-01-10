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
%token CONST "const" VOLATILE "volatile" RESTRICT "restrict" INLINE "inline" ASM "asm"
%token NORETURN "_Noreturn"

%right THEN "else"
/*probably could do this smarter with redesign*/
/*for production builds, parse.assert should probably not be set*/
%start program
%define parse.assert
%define parse.error detailed
%define api.pure full

%param {void *scanner}
%parse-param {void *filename}
%define api.location.type {LOCTYPE}


%code requires{
  #include "compintern.h"
  #include "dynarr.h"
  #include "parallel.h"
  #include "treeduce.h"

  #define aget(param, index) ((INITIALIZER*) (param)->arr[(index)])
  #define dget(param, index) ((DECLARATION*) (param)->arr[(index)])
  #define YYPARSE_PARAM void* scanner
  #define YYLEX_PARAM scanner
  //TODO: union initializers, optional brace nesting?
  //TODO: Consider designated initializers
}

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
  DESIGNARR* designarrvariant;
  USTRUCT* structvariant;
  ENUM* enumvariant;
  DECLARATION* declvariant;
  struct declarator_part* declpartvariant;
  FUNC* funcvariant;
  PARALLEL* paravariant;
  void* vvar;
}

%{
  
  int yylex(YYSTYPE* yst, LOCTYPE* ylt, void* yscanner);
  int yyerror(LOCTYPE* ylt, void* scanner, char* filename, const char* s); //filename is ignored
  void* yyget_extra(void* scanner);
%}

%initial-action {
  @$.filename = filename;
}

%type<str> generic_symbol
%type<dstr> multistring
%type<integert> typemsign
%type<typevariant> types1 types2 types1o
%type<idvariant> typem typews1 type typemintkw inttypem namelesstype arbitrary_cast
%type<exprvariant> expression esc esa est eslo esla esbo esbx esba eseq escmp essh esas esm esca esp esu ee escoa
%type<stmtvariant> statement compound_statement
%type<arrvariant> statements_and_initializers soiorno struct_decls struct_decl cs_decls enums escl escoal abstract_ptr spefptr cs_inits cs_minutes initializer array_literal structbody enumbody nameless params clobberlist clobbers operands operandlist structescoal
%type<designarrvariant> arrescoal
%type<structvariant> struct fullstruct union fullunion
%type<enumvariant> enum fullenum
%type<declvariant> declarator declname param_decl sdecl
%type<declpartvariant> paramparen
%type<funcvariant> function
%type<firforvariant> dee
%type<vvar> program

%%
program:
  function {
    $$ = NULL;
    insert(ctx->funcs, $1->name, $1);
  }
| initializer {
    $$ = NULL;
    if($1) {
      for(int i = 0; i < $1->length; i++) {
        INITIALIZER* a2 = daget($1, i);
        if(!(a2->decl->type->tb & EXTERNNUM)) {
          dapush(ctx->globals, a2);
        } else {
          dapush(ctx->externglobals, a2);
        }
      }
      dadtor($1);
    }
  }
| program function {
    $$ = $1;
    char* cs = $2->name;
    if(!search(ctx->funcs, cs)) {
      insert(ctx->funcs, cs, $2);
    } else {
      fprintf(stderr, "Error: redefinition of function %s in %s %d.%d-%d.%d\n", $2->name, locprint(@2));
    }
  }
| program initializer {
    $$ = $1;
    if($2) {
      for(int i = 0; i < $2->length; i++) {
        INITIALIZER* a2 = daget($2, i);
        IDENTIFIERINFO* id = scopesearch(ctx, M_VARIABLE, a2->decl->varname);
        if(id->type == a2->decl->type) {
          //not redefinition
          if(!(a2->decl->type->tb & EXTERNNUM)) {
            dapush(ctx->globals, a2);
          } else {
            dapush(ctx->externglobals, a2);
          }
        } else {
          if(id->type->tb & EXTERNNUM) {
            id->type->tb &= ~EXTERNNUM;
            dapush(ctx->globals, a2);
          } else {
            char errorstatus = 1;
            if(id->type->pointerstack) {
              struct declarator_part* dp = dapop(id->type->pointerstack);
              if(dp->type == PARAMSSPEC || dp->type == NAMELESS_PARAMSSPEC) {
                dapush(id->type->pointerstack, dp);
                errorstatus = 0;
              }
              //TODO: confirm compatibility of prototypes
            }
            if(errorstatus)
              fprintf(stderr, "Error: redefinition of non-extern global identifier in %s %d.%d-%d.%d\n", locprint(@2));
            freeinit(a2);
          }
        }
      }
      dadtor($2);
    }
  };
initializer:
"typedef" type cs_minutes ';' {
  DECLARATION* dc;
  DYNARR* da = NULL;
  if($2->tb & (STRUCTVAL | UNIONVAL)) {
    if(!$2->structtype->fields) {
      HASHTABLE* ht = NULL;
      if($2->tb & STRUCTVAL) {
        ht = scopepeek(ctx)->forwardstructs;
      } else if($2->tb & UNIONVAL) {
        ht = scopepeek(ctx)->forwardunions;
      } else {
        fprintf(stderr, "Error: forward declaration of unknown type %s at %s %d.%d-%d.%d\n", $2->structtype->name, locprint(@2));
      }
      if(ht) {
        da = search(ht, $2->structtype->name);
        if(da) dapop(da); //else opaque struct
      }
    }
  }
  SCOPE* topscope = dapeek(ctx->scopes);
  for(int i = 0; i < $3->length; i++) {
    dc = dget($3, i);
    if(!queryval(topscope->typesdef, dc->varname)) {
      dc->type->tb |= $2->tb;
      if(ispointer($2)) {
        DYNARR* nptr = ptrdaclone($2->pointerstack);
        if(dc->type->pointerstack)
          dc->type->pointerstack = damerge(nptr, dc->type->pointerstack);
        else
          dc->type->pointerstack = nptr;
      } else {
        if(!ispointer(dc->type)) {
          dadtor(dc->type->pointerstack);
          dc->type->pointerstack = NULL;
        }
      }
      if($2->tb & (STRUCTVAL | UNIONVAL)) {
        if(da) {
          dc->type->structtype = ustructor(strdup($2->structtype->name), NULL, ctx);
          dapush(da, &(dc->type->structtype));
        } else {
           dc->type->structtype = $2->structtype;
        }
      }
      add2scope(ctx, dc->varname, M_TYPEDEF, dc->type);
    } else {
      assert(0);
    }
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
  if($1->tb & (STRUCTVAL | UNIONVAL)) {
    if(!$1->structtype->fields) {
      HASHTABLE* ht;
      if($1->tb & STRUCTVAL) {
        ht = scopepeek(ctx)->forwardstructs;
      } else {
        ht = scopepeek(ctx)->forwardunions;
      }
      da = search(ht, $1->structtype->name);
      if(da) dapop(da); //else opaque struct
    }
  }
  for(int i = 0; i < $$->length; i++) {
    ac = aget($$, i);
    ac->decl->type->tb |= $1->tb; 
    if(ispointer($1)) {
      DYNARR* nptrst = ptrdaclone($1->pointerstack);
      if(ac->decl->type->pointerstack)
        ac->decl->type->pointerstack = damerge(nptrst, ac->decl->type->pointerstack);
      else 
        ac->decl->type->pointerstack = nptrst;
    }
    if($1->tb & (STRUCTVAL | UNIONVAL)) {
      if(da) {
        dapush(da, &(ac->decl->type->structtype));
      } else {
         ac->decl->type->structtype = $1->structtype;
      }
    }
    if(!ctx->func) {
        //uhh... what?
    }
    if(ac->expr && ac->expr->type == ARRAY_LIT) {
      DYNARR* pointy = ac->decl->type->pointerstack;
      if(pointy && pointy->length && ((struct declarator_part*) dapeek(pointy))->type == ARRAYSPEC) {
        process_array_lit(ac->decl->type, ac->expr);
        ac->expr->rettype = fcid2(ac->decl->type);
      } else if(!(pointy && pointy->length) && ac->decl->type->tb & (STRUCTVAL | UNIONVAL)) {
        process_struct_lit(ac->decl->type, ac->expr);
        ac->expr->rettype = fcid2(ac->decl->type);
      } else {
        //disgusting unnecessary brace syntax allowed
        EXPRESSION* unnex = ac->expr;
        assert(unnex->params->length == 1);
        ac->expr = daget(unnex->params, 0);
        dadtor(unnex->params);
        free(unnex);
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
| "enum" enumbody ';' {$$ = NULL; 
  for(int i = 0; i < $2->length; i++) {
    ENUMFIELD* enf = daget($2, i);
    free(enf->name);
    free(enf);
  }
  dadtor($2);
}
| fullstruct ';' {$$ = NULL;}
| fullenum ';' {$$ = NULL;}
| fullunion ';' {$$ = NULL;};
cs_inits:
  cs_inits ',' declarator '=' escoa {$$ = $1; 
    INITIALIZER* id = decl2scope($3, $5, ctx);
    if(id) dapush($$, id); else fprintf(stderr, "Error: redefinition of identifier in %s %d.%d-%d.%d\n", locprint(@3)); }
| declarator '=' escoa {$$ = dactor(8);
    INITIALIZER* id = decl2scope($1, $3, ctx);
    if(id) dapushc($$, id); else fprintf(stderr, "Error: redefinition of identifier in %s %d.%d-%d.%d\n", locprint(@1)); }
| cs_inits ',' declarator {$$ = $1; 
    INITIALIZER* id = decl2scope($3, NULL, ctx);
    if(id) dapush($$, id); else fprintf(stderr, "Error: redefinition of identifier in %s %d.%d-%d.%d\n", locprint(@1)); }
| declarator {$$ = dactor(8); 
    INITIALIZER* id = decl2scope($1, NULL, ctx);
    if(id) dapushc($$, id); else fprintf(stderr, "Error: redefinition of identifier in %s %d.%d-%d.%d\n", locprint(@1)); };
escoa:
  esc {$$ = $1;}
| array_literal {$$ = ct_array_lit($1);};
cs_minutes:
  cs_minutes ',' declarator {$$ = $1; dapush($1, $3);}
| declarator {$$ = dactor(8); dapushc($$, $1);};
declarator:
  abstract_ptr declname {$$ = $2; $2->type->pointerstack = damerge($1, $2->type->pointerstack);}
| declname {$$ = $1;};
paramparen:
  '(' ')' {$$ = mkdeclpart(NAMELESS_PARAMSSPEC, NULL);}
| '(' nameless ')' {$$ = mkdeclpart(NAMELESS_PARAMSSPEC, $2);}
| '(' params ')' {$$ = mkdeclpart(PARAMSSPEC, $2);}
| '(' nameless ',' "..." ')' {
    dapush($2, NULL);
    $$ = mkdeclpart(NAMELESS_PARAMSSPEC, $2);
    }
| '(' params ',' "..." ')' {
    dapush($2, NULL);
    $$ = mkdeclpart(PARAMSSPEC, $2);
    };
declname:
  SYMBOL {$$ = mkdeclaration($1);}
| SYMBOL paramparen {$$ = mkdeclaration($1); dapush($$->type->pointerstack, $2);}
| '(' SYMBOL ')' paramparen {$$ = mkdeclaration($2); dapush($$->type->pointerstack, $4);/*for function type*/}
| '(' abstract_ptr declname ')' paramparen {$$ = $3; $$->type->pointerstack = damerge($$->type->pointerstack, $2); dapush($$->type->pointerstack, $5);}
/*| '(' declname ')' {$$ = $2;}*/
| declname '[' ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpart(ARRAYSPEC, NULL));}
| declname '[' expression ']' {$$ = $1; dapush($$->type->pointerstack,mkdeclpartarr(ARRAYSPEC, $3));};
spefptr:
/*  '(' abstract_ptr SYMBOL ')' {$$ = $2; free($3);}*/
  '(' abstract_ptr ')' {$$ = $2;}
| '(' spefptr ')' {$$ = $2;}
| '[' ']' {$$ = dactor(2); dapush($$, mkdeclpart(ARRAYSPEC, NULL));}
| '[' expression ']' {$$ = dactor(2); dapush($$, mkdeclpartarr(ARRAYSPEC, $2));}
| spefptr '[' ']' {$$ = $1; dapush($$, mkdeclpart(ARRAYSPEC, NULL));}
| spefptr '[' expression ']' {$$ = $1; dapush($$, mkdeclpart(ARRAYSPEC, $3));}
| spefptr paramparen {
    DYNARR* da = dactor(1 + $$->length);
    dapushc(da, $2);
    $$ = damerge(da, $1);
    };
params:
  param_decl {
   $$ = dactor(8);
   dapushc($$, $1);
   }
| params ',' param_decl {
    $$ = $1;
    dapush($$, $3);
    };
param_decl:
  type declarator {
    $2->type->tb |= $1->tb;
    if(ispointer($1)) {
      DYNARR* nptr = ptrdaclone($1->pointerstack);
      if($2->type->pointerstack)
        $2->type->pointerstack = damerge(nptr, $2->type->pointerstack); 
      else 
        $2->type->pointerstack = nptr;
    }
    if($1->tb & (STRUCTVAL | UNIONVAL)) {
      if($1->structtype->fields) {
        $2->type->structtype = $1->structtype;
      } else {
        HASHTABLE* ht = NULL;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        }
        if(ht) {
          DYNARR* da = search(ht, $1->structtype->name);
          if(da) {
            dapop(da);
            dapush(da, &($2->type->structtype));
          } //else opaque struct
        }
      }
    }
    free($1);
    $$ = $2;
    };
nameless:
  namelesstype {$$ = dactor(16); dapushc($$, $1);}
| nameless ',' namelesstype {$$ = $1; dapush($$, $3);}
| params ',' namelesstype {
    $$ = dactor($1->length + 16); 
    for(int i = 0; i < $1->length; i++) {
      DECLARATION* idt = daget($1, i);
      dapush($$, idt->type);
      free(idt->varname);
      free(idt);
    }
    dapush($$, $3);
    dadtor($1);
    }
| nameless ',' param_decl {$$ = $1; dapush($$, $3->type); free($3->varname); free($3);};
namelesstype:
  type {$$ = $1; if(ispointer($$)) $$->pointerstack = ptrdaclone($$->pointerstack); else $$->pointerstack = NULL;}
| type abstract_ptr {$$ = $1;
    if(ispointer($$)) {
      $$->pointerstack = damerge(ptrdaclone($1->pointerstack), $2);
    } else {
      $$->pointerstack = $2;
    }}
| type spefptr {$$ = $1;
    if(ispointer($$)) {
      $$->pointerstack = damerge(ptrdaclone($1->pointerstack), $2);
    } else {
      $$->pointerstack = $2;
    }}
| type abstract_ptr spefptr {$$ = $1;
    if(ispointer($$)) {
      $$->pointerstack = damerge(ptrdaclone($1->pointerstack), $2);
    } else {
      $$->pointerstack = $2;
    }
    $$->pointerstack = damerge($$->pointerstack, $3);
    };
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
| typemsign {$$ = calloc(1, sizeof(IDTYPE)); $$->tb = $1 | 4;/*garbage feature only here for compatibility (unsigned/signed w/o int)*/}
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
    $$->tb = ENUMVAL | 0x8;//for now they're all 8 bits
    };
types1:
  "const" {$$ = CONSTNUM;}
| "_Noreturn" {$$ = 0;/*does nothing*/}
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
| typem {$$ = $1;}
| types1 typews1 {$$ = $2; $$->tb |= $1;}
| types2 typews1 {$$ = $2; $$->tb |= $1;} ;
type:
  typews1 {$$ = $1;}
| typews1 types1 {$$ = $1; $$->tb |= $2;};
types1o:
  types1 {$$ = $1;}
| types1o types1 {$$ = $1 | $2;};
abstract_ptr:
  '*' {$$ = dactor(2); dapushc($$,mkdeclptr(sizeof(intptr_t)));}
| '*' abstract_ptr {$$ = $2; dapush($$, mkdeclptr(sizeof(intptr_t)));}
| '*' types1o {$$ = dactor(2); dapushc($$,mkdeclptr(sizeof(intptr_t) | $2));}
| '*' types1o abstract_ptr {$$ = $3; dapush($$, mkdeclptr(sizeof(intptr_t) | $2));};
arbitrary_cast:
  '(' type ')' {if(ispointer($2)) $2->pointerstack = ptrdaclone($2->pointerstack); else $2->pointerstack = NULL; $$ = $2;}
| '(' type abstract_ptr ')' {
  if(ispointer($2)) {
    $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $3);
  } else {
    $2->pointerstack = $3;
  }
  $$ = $2;}
| '(' type spefptr ')' {
  if(ispointer($2)) {
    $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $3);
  } else {
    $2->pointerstack = $3;
  }
  $$ = $2;}
| '(' type abstract_ptr spefptr ')' {
  if(ispointer($2)) {
    $2->pointerstack = damerge(ptrdaclone($2->pointerstack), $3);
  } else {
    $2->pointerstack = $3;
  }
  $2->pointerstack = damerge($2->pointerstack, $4);
  $$ = $2;};

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
  arbitrary_cast esm {$$ = ct_cast_expr($1, $2);}
| esca {$$ = $1;};
esca:
  "++" esca {$$ = ct_unary_expr(PREINC, $2);}
| "--" esca {$$ = ct_unary_expr(PREDEC, $2);}
| '+' esm {$$ = $2;}
| '-' esm {
    switch($2->type) {
      case INT: case UINT:
        $$ = $2;
        $2->intconst = -$2->intconst;
        break;
      case FLOAT:
        $$ = $2;
        $2->floatconst = -$2->floatconst;
        break;
      default:
        $$ = ct_unary_expr(NEG, $2);
        break;
    }}
| '!' esm {$$ = ct_unary_expr(L_NOT, $2);}
| '~' esm {$$ = ct_unary_expr(B_NOT, $2);}
| '*' esm {$$ = ct_unary_expr(DEREF, $2);}
| '&' esm {$$ = ct_unary_expr(ADDR, $2);}
| "sizeof" '(' type abstract_ptr ')' {$$ = ct_uintconst_expr(sizeof(uintptr_t)); free($3); dadtorfr($4);}
| "sizeof" '(' type ')' {
    //exclude arrays from pointer check
    if(ispointer($3)) {
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
| esp '.' SYMBOL {$$ = ct_binary_expr(DOTOP, $1, ct_member_expr($3));}
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
| arbitrary_cast array_literal {
    DYNARR* pointy = $1->pointerstack;
    $$ = ct_array_lit($2);
    if(pointy && pointy->length && ((struct declarator_part*) dapeek(pointy))->type == ARRAYSPEC) {
      process_array_lit($1, $$);
      $$->rettype = $1;
    } else if(!(pointy && pointy->length) && $1->tb & (STRUCTVAL | UNIONVAL)) {
      process_struct_lit($1, $$);
      $$->rettype = $1;
    } else {
      //disgusting unnecessary brace syntax allowed
      EXPRESSION* unnex = $$;
      assert($$->params->length == 1);
      $$ = daget($$->params, 0);
      $$->rettype = $1;
      dadtor(unnex->params);
      free(unnex);
    }
    }/*compound literal*/
| error {
    $$ = ct_nop_expr(); 
    fprintf (stderr, "Malformed expression at %s %d.%d-%d.%d\n", locprint(@1));
    };
escl:
  esc {$$ = dactor(32); dapushc($$, $1);}
| escl ',' esc {$$ = $1; dapush($$, $3); };
arrescoal:
  arrescoal ',' escoa {$$ = $1;
    if($$->inits->maxlength < $$->curpt + 1)
      $$->inits->arr = reallocarray($$->inits->arr, $$->inits->maxlength = ($1->curpt + 1) * 1.5, sizeof(void*));
    if($$->inits->length < $1->curpt + 1) {
      ++$$->inits->length;
    } else if($$->inits->arr[$$->curpt]) {
      rfreexpr($$->inits->arr[$$->curpt]);
    }
    $$->inits->arr[$$->curpt] = $3;
    ++$$->curpt;
    }
| '[' expression ']' '=' escoa {
    $$ = malloc(sizeof(DESIGNARR));
    foldconst($2);
    assert($2->type == INT || $2->type == UINT);
    $$->inits = dactor(32 > $2->intconst + 1 ? 32 : $2->intconst + 1);
    for(int i = $$->inits->length; i < $2->intconst; i++) {
      $$->inits->arr[i] = NULL;
    }
    $$->inits->arr[$2->intconst] = $5;
    $$->inits->length = $2->intconst + 1;
    $$->curpt = $$->inits->length;
    free($2);
    }
| arrescoal ',' '[' expression ']' '=' escoa {
    $$ = $1;
    foldconst($4);
    assert($4->type == INT || $4->type == UINT);
    if($$->inits->maxlength < $4->intconst + 1)
      $$->inits->arr = reallocarray($$->inits->arr, $$->inits->maxlength = ($4->intconst + 1) * 1.5, sizeof(void*));
    if($$->inits->length < $4->intconst + 1) {
      for(int i = $$->inits->length; i < $4->intconst; i++) {
        $$->inits->arr[i] = NULL;
      }
      $$->inits->length = $4->intconst + 1;
    } else if($$->inits->arr[$4->intconst]) {
      rfreexpr($$->inits->arr[$4->intconst]);
    }
    $$->inits->arr[$4->intconst] = $7;
    $$->curpt = $4->intconst + 1;
    free($4);
    }
| escoal ',' '[' expression ']' '=' escoa {
    $$ = malloc(sizeof(DESIGNARR)); 
    $$->inits = $1; 
    $$->curpt = $1->length;
    foldconst($4);
    assert($4->type == INT || $4->type == UINT);
    if($$->inits->maxlength < $4->intconst + 1)
      $$->inits->arr = reallocarray($$->inits->arr, $$->inits->maxlength = ($4->intconst + 1) * 1.5, sizeof(void*));
    if($$->inits->length < $4->intconst + 1) {
      for(int i = $$->inits->length; i < $4->intconst; i++) {
        $$->inits->arr[i] = NULL;
      }
      $$->inits->length = $4->intconst + 1;
    } else if($$->inits->arr[$4->intconst]) {
      rfreexpr($$->inits->arr[$4->intconst]);
    }
    $$->inits->arr[$4->intconst] = $7;
    $$->curpt = $4->intconst + 1;
    free($4);
    };
structescoal:
  '.' SYMBOL '=' escoa {
    $$ = dactor(32);
    free($2);
    dapush($$, $4);
    }
| structescoal ',' '.' SYMBOL '=' escoa {
    $$ = $1;
    free($4);
    dapush($$, $6);
    }
| escoal ',' '.' SYMBOL '=' escoa {
    $$ = $1;
    free($4);
    dapush($$, $6);
    }
| structescoal ',' escoa {
    $$ = $1;
    dapush($$, $3);
    };

escoal:
  escoa {$$ = dactor(32); dapushc($$, $1);}
| escoal ',' escoa {$$ = $1; dapush($$, $3);};

array_literal:
  '{' escoal commaopt '}' {$$ = $2;}
| '{' arrescoal commaopt '}' {$$ = $2->inits; free($2);}
| '{' structescoal commaopt '}' {$$ = $2;};

multistring:
  STRING_LITERAL {$$ = $1;}
| multistring STRING_LITERAL {
    $$ = $1;
    --$1->lenstr; //remove null terminator
    dscat($1, $2->strptr, $2->lenstr);
    strdtor($2);
    }

function:
  type declarator <funcvariant>{
    DYNARR* parammemb;
    struct declarator_part* dp = dapop($2->type->pointerstack);
    $2->type->tb |= $1->tb;
    if(ispointer($1)) {
      $2->type->pointerstack = damerge(ptrdaclone($1->pointerstack), $2->type->pointerstack);
    }
    if($1->tb & (STRUCTVAL | UNIONVAL)) {
      if($1->structtype->fields) {
        $2->type->structtype = $1->structtype;
      } else {
        HASHTABLE* ht = NULL;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        }
        if(ht) {
          DYNARR* da = search(ht, $1->structtype->name);
          if(da) {
            dapop(da);
            dapush(da, &($2->type->structtype));
          } //else opaque struct
        }
      }
    }
    if(!dp->params) {
      parammemb = dactor(16);
    } else if(dp->type == PARAMSSPEC) {
      parammemb = dp->params;
    } else if(dp->type == NAMELESS_PARAMSSPEC) {
      IDTYPE* prm = dp->nameless_params->arr[0];
      assert((dp->nameless_params->length == 1 && (!prm->pointerstack || prm->pointerstack->length == 0) && prm->tb == VOIDNUM) || !fprintf (stderr, "Function has unnamed parameters at %s %d.%d-%d.%d\n", locprint(@1)));
      dadtorcfr(dp->nameless_params, (void(*)(void*)) freetype);
      parammemb = dactor(16);
    } else {
      fprintf(stderr, "Function has malformed parameters at %s %d.%d-%d.%d\n", locprint(@1));
      assert(0);
    }
    $$ = ct_function($2->varname, NULL, parammemb, $2->type);
    IDENTIFIERINFO* fordecid;
    if((fordecid = scopesearch(ctx, M_VARIABLE, $2->varname))) {
      //TODO: ensure compliant overwriting?
      //free(fordecid->name);
      //freetype(fordecid->type);
      free(fordecid);
    }
    IDENTIFIERINFO* id = malloc(sizeof(IDENTIFIERINFO));
    id->index = -1;
    id->name = $2->varname;
    dp->type = PARAMSSPEC;
    dp->params = parammemb;
    dapush($2->type->pointerstack, dp);
    id->type = $2->type;
    id->type->tb |= GLOBALFUNC;
    rmpaircfr(scopepeek(ctx)->members, $2->varname, free); //no-op if not predefined
    add2scope(ctx, $2->varname, M_GLOBAL, id);
    free($2);
    free($1);
    ctx->func = $$;

    scopepush(ctx);
    for(int i = 0; i < parammemb->length; i++) {
      DECLARATION* declwhole = daget(parammemb, i);
      add2scope(ctx, declwhole->varname, M_VARIABLE, declwhole->type);
      declwhole->varid = ((SCOPEMEMBER*) search(scopepeek(ctx)->members, declwhole->varname))->idi->index;
    }
    }
    '{' soiorno '}' {
    scopepop(ctx);
    $$ = $3;
    $$->body = mkcmpndstmt($5, @$); 
    ctx->func = NULL;
    };
clobberlist:
  clobberlist ',' multistring {$$ = $1; dapush($$, $3->strptr); free($3);}
| multistring {$$ = dactor(8); dapush($$, $1->strptr); free($1);};
clobbers:
  %empty {$$ = NULL;}
| clobberlist {$$ = $1;};
operandlist:
  operandlist ',' multistring '(' expression ')' {$$ = $1; dapush($$, genoperand($3->strptr, $5)); free($3);}
| multistring '(' expression ')' {$$ = dactor(8); dapush($$, genoperand($1->strptr, $3)); free($1);};
operands:
  %empty {$$ = NULL;}
| operandlist {$$ = $1;};
statement:
  compound_statement {$$ = $1;}
|  SYMBOL ':' {$$ = mklblstmt(ctx, $1, @1);}
| "case" esc ':' { 
    char* caselbl = malloc(128);
    snprintf(caselbl, 128, "__joecc__%s__%d", ctx->func->name, (ctx->func->caseindex)++);
    $$ = mkcasestmt(ctx, $2, caselbl, @$);
    }
| "case" INTEGER_LITERAL "..." INTEGER_LITERAL ':' {
    DYNARR* cases = dactor($4 - $2 + 1);
    for(int i = $2; i <= $4; i++) {
      char* caselbl = malloc(128);
      snprintf(caselbl, 128, "__joecc__%s__%d", ctx->func->name, (ctx->func->caseindex)++);
      dapushc(cases, sois(mkcasestmt(ctx, ct_intconst_expr(i), caselbl, @$)));
    }
    $$ = mkcmpndstmt(cases, @$);
    }
| "default" ':' {
    char* caselbl = malloc(128);
    snprintf(caselbl, 128, "__joecc__%s__default__%d", ctx->func->name, (ctx->func->caseindex)++);
    $$ = mkdefaultstmt(ctx, caselbl, @1);
    }
| "if" '(' expression ')' statement %prec THEN {$$ = mkifstmt($3, $5, NULL, @$);}
| "if" '(' expression ')' statement "else" statement {$$ = mkifstmt($3, $5, $7, @$);}
| "switch" '(' expression ')' switch_midrule compound_statement {
    SWITCHINFO* swi = dapop(ctx->func->switchstack);
    $$ = mkswitchstmt($3, $6, swi, @$);
    free(swi);
    }
| "while" '(' expression ')' statement {$$ = mklsstmt(WHILEL, $3, $5, @$);}
| "do" statement "while" '(' expression ')' ';' {$$ = mklsstmt(DOWHILEL, $5, $2, @$);}
| "for" '(' {
    scopepush(ctx);
    } dee  ee ';' ee ')' statement {$$ = mkforstmt($4, $5, $7, $9, @$); scopepop(ctx);}
| "goto" SYMBOL ';' {$$ = mkgotostmt($2, @$);}
| "break" ';' {$$ = mkexprstmt(LBREAK, NULL, @$);}
| "continue" ';' {$$ = mkexprstmt(LCONT, NULL, @$);}
| "return" ';' {$$ = mkexprstmt(FRET, NULL, @$);}
| "return" expression ';' {$$ = mkexprstmt(FRET, $2, @$);}
| expression ';' {$$ = mkexprstmt(EXPR, $1, @$);}
| "asm" '(' multistring ')' ';' {$$ = mkasmstmt($3->strptr, NULL, NULL, NULL, @$); free($3);}
| "asm" '(' multistring ':' operands ')' ';' {$$ = mkasmstmt($3->strptr, $5, NULL, NULL, @$); free($3);}
| "asm" '(' multistring ':' operands ':' operands ')' ';' {$$ = mkasmstmt($3->strptr, $5, $7, NULL, @$); free($3);}
| "asm" '(' multistring ':' operands ':' operands ':' clobbers ')' ';' {$$ = mkasmstmt($3->strptr, $5, $7, $9, @$); free($3);}
| ';' {$$ = mknopstmt();};
ee: 
  expression {$$ = $1;}
| %empty {$$ = ct_nop_expr();};
dee:
  initializer {if($1) {$$ = malloc(sizeof(EOI)); $$->isE = 0; $$->I = $1;} else {$$ = malloc(sizeof(EOI)); $$->isE = 1; $$->E = ct_nop_expr();};}
| ee ';' {$$ = malloc(sizeof(EOI)); $$->isE = 1; $$->E = $1;};
compound_statement:/*add new scope to scope stack, remove when done*/
  '{' compound_midrule soiorno'}' {
    $$ = mkcmpndstmt($3, @$); 
    scopepop(ctx);
    };
compound_midrule: %empty {
    scopepush(ctx);
    };
statements_and_initializers:
  initializer {$$ = dactor(256); if($1) dapushc($$,soii($1));}
| statement {$$ = dactor(256); dapushc($$,sois($1));}
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
    $$ = ustructor($2, $4, ctx);
    rmpaircfr(scopepeek(ctx)->unions, $2, free); //no-op if not predefined
    add2scope(ctx, $2, M_UNION, $$); 
    defbackward(ctx, M_UNION, $2, (USTRUCT*) $$);
    };
union:
  fullunion {$$ = $1;}
| "union" structbody  {$$ = ustructor(NULL, $2, ctx);}
| "union" generic_symbol {
    $$ = (USTRUCT*) scopesearch(ctx, M_UNION, $2);
    if(!$$) {
      if(!queryval(scopepeek(ctx)->forwardunions, $2)) {
        add2scope(ctx, $2, M_UNION, NULL);
        insert(scopepeek(ctx)->forwardunions, $2, dactor(16));
      }
      $$ = ustructor($2, NULL, ctx);
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
    $$ = ustructor($2, $4, ctx); 
    rmpaircfr(scopepeek(ctx)->structs, $2, free); //no-op if not predefined
    add2scope(ctx, $2, M_STRUCT, $$);
    defbackward(ctx, M_STRUCT, $2, $$);
    };
struct:
  fullstruct {$$ = $1;}
| "struct" structbody {$$ = ustructor(NULL, $2, ctx);}
| "struct" generic_symbol {
    $$ = (USTRUCT*) scopesearch(ctx, M_STRUCT, $2);
    if(!$$) {
      if(!queryval(scopepeek(ctx)->forwardstructs, $2)) {
        add2scope(ctx, $2, M_STRUCT, NULL);
        insert(scopepeek(ctx)->forwardstructs, $2, dactor(16));
      }
      $$ = ustructor($2, NULL, ctx);
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
    if($1->tb & (STRUCTVAL | UNIONVAL)) {
      if(!$1->structtype->fields) {
        HASHTABLE* ht = NULL;
        if($1->tb & STRUCTVAL) {
          ht = scopepeek(ctx)->forwardstructs;
        } else if($1->tb & UNIONVAL) {
          ht = scopepeek(ctx)->forwardunions;
        } else {
          fprintf(stderr, "Error: forward declaration of unknown type %s at %s %d.%d-%d.%d\n", $1->structtype->name, locprint(@$));
        }
        if(ht) {
          da = search(ht, $1->structtype->name);
          if(da) dapop(da); //else opaque struct
        }
      }
    }
    
    for(int i = 0; i < $2->length; i++) {
      DECLARATION* dc = dget($$, i);
      dc->type->tb |= $1->tb; 
      if(ispointer($1)) {
        DYNARR* nptr = ptrdaclone($1->pointerstack);
        if(dc->type->pointerstack) {
          dc->type->pointerstack = damerge(nptr, dc->type->pointerstack);
        } else {
          dc->type->pointerstack = nptr;
        }
      }
      if($1->tb & (STRUCTVAL | UNIONVAL)) {
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
    tt->structtype = calloc(1, sizeof(USTRUCT));
    tt->structtype->fields = $2;
    tt->pointerstack = NULL;
    tt->tb = STRUCTVAL | ANONMEMB;
    DECLARATION* dec = malloc(sizeof(DECLARATION));
    dec->type = tt;
    dec->varname = NULL;
    dapushc($$, dec);
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
    tt->uniontype = calloc(1, sizeof(USTRUCT));
    tt->uniontype->fields = $2;
    tt->pointerstack = NULL;
    tt->tb = UNIONVAL | ANONMEMB;
    DECLARATION* dec = malloc(sizeof(DECLARATION));
    dec->type = tt;
    dec->varname = NULL;
    dapushc($$, dec);
    };
cs_decls:
  cs_decls ',' sdecl {$$ = $1; dapush($$, $3);}
| sdecl {$$ = dactor(8); dapushc($$, $1);};
sdecl: 
  declarator {$$ = $1;}
| declarator ':' esc {$$ = $1; rfreexpr($3);/*dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $3));*/}
| ':' esc {$$ = mkdeclaration(NULL); rfreexpr($2);/*dapush($$->type->pointerstack, mkdeclpart(BITFIELDSPEC, $2));*/};
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
    dapushc($$, genenumfield($1,const0)); 
    add2scope(ctx, $1, M_ENUM_CONST, const0);
    }
| SYMBOL '=' esc {$$ = dactor(256);
    ENUMFIELD* ef = genenumfield($1,$3);
    dapushc($$, ef); 
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
      case ADD:
        memcpy(newexpr, daget(prevexpr->params, 0), sizeof(EXPRESSION));
        if(newexpr->type == INT) {
          ++(newexpr->intconst);
          break;
        }
        //fall through
      default:
        assert(0);
        //newexpr->type = ADD;
        //dapush(newexpr->params, ct_intconst_expr(1));
        //dapush(newexpr->params, rclonexpr(prevexpr));
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
int yyerror(LOCTYPE* ylt, void* scanner, char* filename, const char* s) {
  fprintf(stderr, "ERROR: %s %s %d.%d-%d.%d\n", s, dlocprint(ylt));
  return 0;
}

