#ifndef COMPINTERN_H
#define COMPINTERN_H
#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "dynarr.h"

typedef enum {
  FLOATNUM    = 0x10,
  UNSIGNEDNUM = 0x20,
  CONSTNUM    = 0x40,
  VOLATILENUM = 0x80,
  STATICNUM   = 0x100,
  EXTERNNUM   = 0x200,
  PARAMNUM    = 0x400,
  VOIDNUM     = 0x800,
  ENUMVAL     = 0x1000,
  STRUCTVAL   = 0x2000,
  UNIONVAL    = 0x4000,
} TYPEBITS;

enum ident_type {
  UNDEFINED,
  FUNCTION,
  PARAMETER,
  LOCAL_VAR,
  GLOBAL_VAR,
  TYPE_DEFN
};

typedef enum {
  NOP, STRING, WSTRING, INT, UINT, FLOAT, IDENT,
  ADD, NEG, SUB, EQ, NEQ, GT, LT, GTE, LTE, MULT, DIVI, MOD,
  PREINC, POSTINC, PREDEC, POSTDEC,
  L_AND, L_OR, L_NOT, B_AND, B_OR, B_XOR, B_NOT, SHL, SHR,
  DOTOP, ARROW,
  SZOF, SZOFEXPR,
  ASSIGN,
  ADDASSIGN, SUBASSIGN, SHLASSIGN, SHRASSIGN, ANDASSIGN, 
  XORASSIGN, ORASSIGN, DIVASSIGN, MULTASSIGN, MODASSIGN,
  /*CASES,*/
  CAST,
  COMMA,
  ADDR, DEREF,
  FCALL, /*FCOPY,*/
  TERNARY
} EXPRTYPE;

enum stmttype {
  FRET, LBREAK, JGOTO, LCONT,
  FORL, WHILEL, DOWHILEL,
  IFS, IFELSES,
  SWITCH,
  CASE, LABEL,
  CMPND,
  EXPR, NOPSTMT,
  DEFAULT
};

enum declpart_info {
  POINTERSPEC, ARRAYSPEC, PARAMSSPEC, BITFIELDSPEC
};

struct stmt;

typedef struct {
  DYNARR* fields;//Each entry is a struct that contains a full identifier and a size
  char* name;
} STRUCT;
typedef struct {
  DYNARR* fields;//Each entry is a struct that contains a full identifier and a size
  char* name;
} UNION;
typedef struct {
  char* name;
  DYNARR* fields;
} ENUM;

typedef struct {
  DYNARR* pointerstack;
  TYPEBITS tb;
  union {
    STRUCT* structtype;
    UNION* uniontype;
    ENUM* enumtype;
  };
} IDTYPE;

typedef struct {
  //int index;//index of type within scope (i.e. parameter index)
  //value perhaps?
  IDTYPE* type;
  char* name;
} IDENTIFIERINFO;

typedef struct {
  char* name;
  struct stmt* body; //compound statement
  DYNARR* params;
  IDTYPE* retrn;
} FUNC;

struct lexctx {//TODO: FIX
  HASHTABLE* funcs;
  //unsigned int fllast, fllen;
  DYNARR* scopes;
  //unsigned int layer;//Necessary?
  HASHTABLE* symtab;
  HASHTABLE* defines;
  DYNARR* definestack;
};


typedef struct expr {
  EXPRTYPE type;
  union {
    struct {
      int numparams;
      struct expr* params;
      struct expr* ftocall;
    };
    struct {
      struct expr* param1;
      struct expr* param2;
    };
    struct {
      struct expr* ifexpr;
      struct expr* thenexpr;
      struct expr* elseexpr;
    };
    struct {
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

typedef struct stmt {
  enum stmttype type;
  union {
    EXPRESSION* expression;
    struct { //if or if/else
      EXPRESSION* ifcond;
      struct stmt* thencond;
      struct stmt* elsecond;
    };
    struct { //while or dowhile or switch(?)
      EXPRESSION* cond;
      struct stmt* body;
    };
    struct { //for
      EXPRESSION* init;
      EXPRESSION* forcond;
      EXPRESSION* increment;
      struct stmt* forbody;
    };
    //IDENTIFIERINFO* label; //case or label, maybe also goto?
    char* glabel; //for label and goto
    DYNARR* stmtsandinits; //compound
    struct stmt* substatement; //default
  };
} STATEMENT;

typedef struct {
  char isstmt;
  union {
    struct stmt* state;
    DYNARR* init;
  };
} SOI;

typedef struct {
  char* name;
  EXPRESSION* value;
} ENUMFIELD;

struct declarator_part {
  enum declpart_info type;
  union {
    DYNARR* params;
    EXPRESSION* arrspec;
    EXPRESSION* bfspec;
    TYPEBITS ptrspec;
    void* garbage;
  };
};

typedef struct {
  char* varname;
  IDTYPE* type;
} DECLARATION;

typedef struct {
  DECLARATION* decl;
  EXPRESSION* expr;
} INITIALIZER;//?

struct intinfo {
  long num;
  char sign;
}; 

typedef struct {
  char isfunc;
  union {
    INITIALIZER* i;
    FUNC* f;
    void* garbage;
  };
} TOPBLOCK;

enum membertype {
  M_LABEL, M_TYPEDEF, M_VARIABLE, M_STRUCT, M_UNION, M_ENUM, M_ENUM_CONST, M_CASE
};

typedef struct {
  enum membertype mtype;
  union {
    STRUCT* structmemb;
    ENUM* enummemb;
    UNION* unionmemb;
    IDTYPE* typememb;
    union {
      IDTYPE* vartype;
      long varcount;
    };
    EXPRESSION* enumnum;
    EXPRESSION* caseval;
    //label needs nothing?
    void* garbage;
  };
} SCOPEMEMBER;

typedef struct {
  //one for vars
  //one for struct enum and union
  //one for typedef
  //one for labels
  HASHTABLE* members;//SCOPEMEMBER argument
} SCOPE;

enum ifdefstate {
  IFDEFDUMMY, IFANDTRUE, IFANDFALSE, ELSEANDTRUE, ELSEANDFALSE
};

STRUCT* structor(char* name, DYNARR* fields);
UNION* unionctor(char* name, DYNARR* fields);
ENUM* enumctor(char* name, DYNARR* fields);
EXPRESSION* cloneexpr(EXPRESSION* orig);
EXPRESSION* ct_nop_expr();
EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION* param);
EXPRESSION* ct_sztype(IDTYPE* whichtype);
EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2);
EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr );
EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3);
EXPRESSION* ct_fcall_expr(EXPRESSION* func, int num, EXPRESSION* params);
EXPRESSION* ct_strconst_expr(char* str);
EXPRESSION* ct_intconst_expr(long num); 
EXPRESSION* ct_uintconst_expr(unsigned long num);
EXPRESSION* ct_floatconst_expr(double num);
EXPRESSION* ct_ident_expr(/*IDENTIFIERINFO* id*/ char* ident);
DECLARATION* mkdeclaration(char* name);
INITIALIZER* geninit(DECLARATION* decl, EXPRESSION* expr);
SOI* sois(struct stmt* state);
SOI* soii(DYNARR* init);
STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express);
STATEMENT* mkgotostmt(char* gotoloc);
STATEMENT* mkforstmt(EXPRESSION* e1, EXPRESSION* e2, EXPRESSION* e3, STATEMENT* bdy);
STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy);
STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy);
STATEMENT* mkcmpndstmt(DYNARR* stmtsandinits);
STATEMENT* mklblstmt(char* identifier);
STATEMENT* mkcasestmt(EXPRESSION* casexpr/*, STATEMENT* stmt*/);
STATEMENT* mkdefaultstmt(STATEMENT* stmt);
ENUMFIELD* genenumfield(char* name, EXPRESSION* value);
struct declarator_part* mkdeclpart(enum declpart_info typ, void* d);
struct declarator_part* mkdeclptr(TYPEBITS d);
EXPRESSION* exprfromdecl(char* name, IDTYPE* id);
FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn);
SCOPE* mkscope(SCOPE* parent);
void scopepush(struct lexctx* ctx);
void scopepop(struct lexctx* ctx);
SCOPE* scopepeek(struct lexctx* ctx);
void add2scope(SCOPE* scope, char* memname, enum membertype mtype, void* memberval);
TOPBLOCK* gtb(char isfunc, void* assign);

#endif
