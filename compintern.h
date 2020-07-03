#ifndef COMPINTERN_H
#define COMPINTERN_H
#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "dynarr.h"
#include "dynstr.h"

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
  ANONMEMB    = 0x8000,
} TYPEBITS;

#define EXPRTYPELIST  \
  X(NOP), X(STRING), X(INT), X(UINT), X(FLOAT), X(MEMBER), X(IDENT), X(ARRAY_LIT), \
  X(ADD), X(NEG), X(SUB), X(EQ), X(NEQ), X(GT), X(LT), X(GTE), X(LTE), X(MULT), X(DIVI), X(MOD), \
  X(PREINC), X(POSTINC), X(PREDEC), X(POSTDEC), \
  X(L_AND), X(L_OR), X(L_NOT), X(B_AND), X(B_OR), X(B_XOR), X(B_NOT), X(SHL), X(SHR), \
  X(DOTOP), X(ARROW), \
  X(SZOF), X(SZOFEXPR), \
  X(ASSIGN), \
  X(ADDASSIGN), X(SUBASSIGN), X(SHLASSIGN), X(SHRASSIGN), X(ANDASSIGN),  \
  X(XORASSIGN), X(ORASSIGN), X(DIVASSIGN), X(MULTASSIGN), X(MODASSIGN), \
  X(CAST), \
  X(COMMA), \
  X(ADDR), X(DEREF), \
  X(FCALL), \
  X(TERNARY) 
#define X(name) name
typedef enum {
  EXPRTYPELIST
} EXPRTYPE;
#undef X

#define STMTTYPE_LIST \
  X(FRET), X(LBREAK), X(JGOTO), X(LCONT), \
  X(FORL), X(WHILEL), X(DOWHILEL), \
  X(IFS), X(IFELSES), \
  X(SWITCH), \
  X(CASE), X(LABEL), \
  X(CMPND), \
  X(EXPR), X(NOPSTMT), \
  X(DEFAULT)
#define X(name) name
enum stmttype {
  STMTTYPE_LIST
};
#undef X

#define DECLPART_TYPE X(POINTERSPEC), X(ARRAYSPEC), X(PARAMSSPEC), X(BITFIELDSPEC)
#define X(name) name
enum declpart_info {
  DECLPART_TYPE
};
#undef X

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
  DYNARR* fields;
  char* name;
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
  long index;
} IDENTIFIERINFO;

typedef struct {
  char* name;
  struct stmt* body; //compound statement
  DYNARR* params;
  IDTYPE* retrn;
  HASHTABLE* lbls;
  DYNARR* switchstack;
} FUNC;

struct lexctx {//TODO: FIX
  HASHTABLE* funcs;
  //unsigned int fllast, fllen;
  DYNARR* scopes;
  DYNARR* definestack;
  FUNC* func;
  HASHTABLE* defines;
};


typedef struct expr {
  EXPRTYPE type;
  DYNARR* params; //arr of exprs
  union {
    IDTYPE* casttype;
    char* strconst;
    char* member;
    long intconst;
    unsigned long uintconst;
    double floatconst;
    IDENTIFIERINFO* id;//for identifier expressions?
    IDTYPE* typesz;//for sizeof
    DYNARR* dynvals;//?
    /*possible struct const for later struct initializations*/
  };
} EXPRESSION;

typedef struct {
  char isE;
  union {
    DYNARR* I;
    EXPRESSION* E;
  };
} EOI;

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
      EOI* init;
      EXPRESSION* forcond;
      EXPRESSION* increment;
      struct stmt* forbody;
    };
    struct { //case
      EXPRESSION* casecond;
      char* caselabel;
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

#define MEMBERTYPELIST X(M_LABEL), X(M_TYPEDEF), X(M_VARIABLE), X(M_STRUCT), X(M_UNION), X(M_ENUM), X(M_ENUM_CONST), X(M_CASE)
#define X(name) name
enum membertype {
  MEMBERTYPELIST
};
#undef X

typedef struct {
  enum membertype mtype;
  union {
    STRUCT* structmemb;
    ENUM* enummemb;
    UNION* unionmemb;
    IDTYPE* typememb;
    IDENTIFIERINFO* idi;
    EXPRESSION* enumnum;
    EXPRESSION* caseval;
    //label needs nothing?
    void* garbage;
  };
} SCOPEMEMBER;

typedef struct {
  //one for vars
  //one for labels--in function type, ctx needs to track this
  HASHTABLE* typesdef;//SCOPEMEMBER argument
  HASHTABLE* members;//SCOPEMEMBER argument
  HASHTABLE* structs;
  HASHTABLE* enums;
  HASHTABLE* unions;
  HASHTABLE* forwardstructs;
  HASHTABLE* forwardenums;
  HASHTABLE* forwardunions;
} SCOPE;

enum ifdefstate {
  IFDEFDUMMY, IFANDTRUE, IFANDFALSE, ELSEANDTRUE, ELSEANDFALSE
};

struct macrodef {
  char* text;//will be a format string if function like
  DYNARR* args;//NULL if not function like
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
EXPRESSION* ct_fcall_expr(EXPRESSION* func, DYNARR* params);
EXPRESSION* ct_strconst_expr(char* str);
EXPRESSION* ct_intconst_expr(long num); 
EXPRESSION* ct_uintconst_expr(unsigned long num);
EXPRESSION* ct_floatconst_expr(double num);
EXPRESSION* ct_array_lit(DYNARR* da);
EXPRESSION* ct_member_expr(char* member);
EXPRESSION* ct_ident_expr(struct lexctx* lct, char* ident);
DECLARATION* mkdeclaration(char* name);
INITIALIZER* geninit(DECLARATION* decl, EXPRESSION* expr);
SOI* sois(struct stmt* state);
SOI* soii(DYNARR* init);
STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express);
STATEMENT* mkgotostmt(char* gotoloc);
STATEMENT* mkforstmt(EOI* e1, EXPRESSION* e2, EXPRESSION* e3, STATEMENT* bdy);
STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy);
STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy);
STATEMENT* mkcmpndstmt(DYNARR* stmtsandinits);
STATEMENT* mklblstmt(char* identifier);
STATEMENT* mkcasestmt(EXPRESSION* casexpr, char* label);
STATEMENT* mkdefaultstmt();
ENUMFIELD* genenumfield(char* name, EXPRESSION* value);
struct declarator_part* mkdeclpart(enum declpart_info typ, void* d);
struct declarator_part* mkdeclptr(TYPEBITS d);
EXPRESSION* exprfromdecl(char* name, IDTYPE* id);
FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn);
struct lexctx* ctxinit();
SCOPE* mkscope();
void scopepush(struct lexctx* ctx);
void scopepop(struct lexctx* ctx);
SCOPE* scopepeek(struct lexctx* ctx);
void* scopesearch(struct lexctx* lct, enum membertype mt, char* key);
SCOPEMEMBER* scopesearchmem(struct lexctx* lct, enum membertype mt, char* key);
char scopequeryval(struct lexctx* lct, enum membertype mt, char* key);
void defbackward(struct lexctx* lct, enum membertype mt, char* defnd, void* assignval);
void add2scope(SCOPE* scope, char* memname, enum membertype mtype, void* memberval);
TOPBLOCK* gtb(char isfunc, void* assign);

#define locprint(lv) lv.first_line, lv.first_column, lv.last_line, lv.last_column
#endif
