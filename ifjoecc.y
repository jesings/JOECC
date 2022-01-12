%token<unum> UNSIGNED_LITERAL;
%token<unum> INTEGER_LITERAL;
%token SHLTK "<<" SHRTK ">>" LE "<=" GE ">=" EQTK "==" NEQTK "!=" AND "&&" OR "||"

%start fullifexpr
%define api.prefix {zz}
%define parse.trace
%define parse.assert
%define parse.error verbose
%define api.pure full
%locations
%define api.location.type {LOCTYPE}
%type<exprvariant> expression est eslo esla esbo esbx esba eseq escmp essh esas esca esu

%code requires{
  #include <stdio.h>
  #include <fcntl.h>
  #include <unistd.h>
  #include "compintern.h"
  #include "treeduce.h"
}

%param {void *scanner}

%union {
  unsigned long unum;
  EXPRESSION* exprvariant;
}

%{
  #undef yylex
  typedef YYLTYPE ZZLTYPE;
  int yylex(YYSTYPE* yst, ZZLTYPE* ylt, void* yyscanner);
  int yyerror(void* yyscanner, const char* s);
  void* yyget_extra(void* scanner);
  ZZLTYPE* yyget_lloc(void* scanner);
  void zz_pop_state(void*);
  void zz_push_skip(void*);

  static int zzlex(YYSTYPE* yst, void* yyscanner) {
    return yylex(yst, yyget_lloc(yyscanner), yyscanner);
  }
  #define yylex zzlex
%}

%%
fullifexpr:
  expression {
    while(foldconst($1)) ;
    enum ifdefstate* rids = NULL;
    switch($1->type) {
      case INT: case UINT:
        if(!$1->intconst) {
      case NOP:
          rids = malloc(sizeof(enum ifdefstate));
          *rids = IFANDFALSE;
          zz_push_skip(scanner);
          break;
        }
        rids = malloc(sizeof(enum ifdefstate));
        *rids = IFANDTRUE;
        break;
      default:
        zz_pop_state(scanner);
        fprintf(stderr, "ERROR: subsidiary parser reduced if or elif into non-rectifiable expression %s %d.%d-%d.%d\n", dlocprint(yyget_lloc(scanner)));
    }
    rfreexpr($1);
    if(rids)
      dapush(ctx->definestack, rids);
    };
expression:
  est '?' expression ':' est {$$ = ct_ternary_expr($1, $3, $5, @$);}
| est {$$ = $1;};
est:
  est "||" eslo {$$ = ct_binary_expr(L_OR, $1, $3, @$);}
| eslo {$$ = $1;};
eslo:
  eslo "&&" esla {$$ = ct_binary_expr(L_AND, $1, $3, @$);}
| esla {$$ = $1;};
esla:
  esla '|' esbo {$$ = ct_binary_expr(B_OR, $1, $3, @$);}
| esbo {$$ = $1;};
esbo:
  esbo '^' esbx {$$ = ct_binary_expr(B_XOR, $1, $3, @$);}
| esbx {$$ = $1;};
esbx:
  esbx '&' esba {$$ = ct_binary_expr(B_AND, $1, $3, @$);}
| esba {$$ = $1;};
esba:
  esba "==" eseq {$$ = ct_binary_expr(EQ, $1, $3, @$);}
| esba "!=" eseq {$$ = ct_binary_expr(NEQ, $1, $3, @$);}
| eseq {$$ = $1;};
eseq:
  eseq '<' escmp {$$ = ct_binary_expr(LT, $1, $3, @$);}
| eseq '>' escmp {$$ = ct_binary_expr(GT, $1, $3, @$);}
| eseq "<=" escmp {$$ = ct_binary_expr(LTE, $1, $3, @$);}
| eseq ">=" escmp {$$ = ct_binary_expr(GTE, $1, $3, @$);}
| escmp {$$ = $1;};
escmp:
  escmp "<<" essh {$$ = ct_binary_expr(SHL, $1, $3, @$);}
| escmp ">>" essh {$$ = ct_binary_expr(SHR, $1, $3, @$);}
| essh {$$ = $1;};
essh:
  essh '+' esas {$$ = ct_binary_expr(ADD, $1, $3, @$);}
| essh '-' esas {$$ = ct_binary_expr(SUB, $1, $3, @$);}
| esas {$$ = $1;};
esas:
  esas '*' esca {$$ = ct_binary_expr(MULT, $1, $3, @$);}
| esas '/' esca {$$ = ct_binary_expr(DIVI, $1, $3, @$);}
| esas '%' esca {$$ = ct_binary_expr(MOD, $1, $3, @$);}
| esca {$$ = $1;};
esca:
  '+' esca {$$ = ct_unary_expr(IDENT, $2, @$);}
| '-' esca {$$ = ct_unary_expr(NEG, $2, @$);}
| '!' esca {$$ = ct_unary_expr(L_NOT, $2, @$);}
| '~' esca {$$ = ct_unary_expr(B_NOT, $2, @$);}
| esu {$$ = $1;};
esu:
  '(' expression ')' {$$ = $2;}
| UNSIGNED_LITERAL {$$ = ct_uintconst_expr($1, @$);}
| INTEGER_LITERAL {$$ = ct_intconst_expr($1, @$);};
%%
int yyerror(void* scanner, const char* s) {
  fprintf(stderr, "Subsidiary parser encountered error %s %s %d.%d-%d.%d\n", s, dlocprint(yyget_lloc(scanner)));
  return 0;
}
