%locations
%token<unum> UNSIGNED_LITERAL;
%token<unum> INTEGER_LITERAL;
%token SHLTK "<<" SHRTK ">>" LE "<=" GE ">=" EQTK "==" NEQTK "!=" AND "&&" OR "||" 

%start fullifexpr
%define api.prefix {zz}
%define parse.trace
%define parse.assert
%define parse.error verbose
%type<exprvariant> expression est eslo esla esbo esbx esba eseq escmp essh esas esca esu

%code requires{
  #include <stdio.h>
  #include <fcntl.h>
  #include <unistd.h>
  #include "compintern.h"
  #include "printree.h"
  extern DYNARR* file2compile;
}

%{
  extern struct lexctx* ctx;
  #define zzlex yylex
  int yylex(void);
  int yyerror(const char* s);
%}

%union {
  unsigned long unum;
  EXPRESSION* exprvariant;
}

%%
fullifexpr:
  expression {
    //extern int funcfile, nodenumber;
    //nodenumber = 0;
    //int fnn = nodenumber++;
    //funcfile = creat("ppexpr.dot", 0666);
    //dprintf(funcfile, "digraph %s {\ngraph [rankdir=LR];\nnode [shape=box];\ngraph [splines=ortho, nodesep=1];", "ppexpr");
    //dprintf(funcfile, "n%d [label=\"%s\"];\n", fnn, "contingent on"); 
    //dprintf(funcfile, "n%d -> n%d;\n", fnn, treexpr($1));
    //dprintf(funcfile, "}\n");
    //close(funcfile);
    ctx->ifexpr = $1;
    };
expression:
  est '?' expression ':' est {$$ = ct_ternary_expr($1, $3, $5);}
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
| essh {$$ = $1;};
essh:
  essh '+' esas {$$ = ct_binary_expr(ADD, $1, $3);}
| essh '-' esas {$$ = ct_binary_expr(SUB, $1, $3);}
| esas {$$ = $1;};
esas:
  esas '*' esca {$$ = ct_binary_expr(MULT, $1, $3);}
| esas '/' esca {$$ = ct_binary_expr(DIVI, $1, $3);}
| esas '%' esca {$$ = ct_binary_expr(MOD, $1, $3);}
| esca {$$ = $1;};
esca:
  '+' esu {$$ = ct_unary_expr(IDENT, $2);}
| '-' esu {$$ = ct_unary_expr(NEG, $2);}
| '!' esu {$$ = ct_unary_expr(L_NOT, $2);}
| '~' esu {$$ = ct_unary_expr(B_NOT, $2);}
| esu {$$ = $1;};
esu:
  '(' expression ')' {$$ = $2;}
| UNSIGNED_LITERAL {$$ = ct_uintconst_expr($1);}
| INTEGER_LITERAL {$$ = ct_intconst_expr($1);};
%%
int yyerror(const char* s){
  fprintf(stderr, "Subsidiary parser encountered error %s %s %d.%d-%d.%d\n", s, locprint(yylloc));
  return 0;
}
