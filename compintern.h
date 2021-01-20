#ifndef COMPINTERN_H
#define COMPINTERN_H
#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "dynarr.h"
#include "dynstr.h"
#include "parallel.h"

#define PPDEBUG 0

typedef enum {
  FLOATNUM    = 0x10,
  UNSIGNEDNUM = 0x20,
  CONSTNUM    = 0x40,
  VOLATILENUM = 0x80,
  STATICNUM   = 0x100,
  EXTERNNUM   = 0x200,
  RESTRICTNUM = 0x400,
  GLOBALFUNC  = 0x800,
  VOIDNUM     = 0x1000,
  ENUMVAL     = 0x2000,
  STRUCTVAL   = 0x4000,
  UNIONVAL    = 0x8000,
  ANONMEMB    = 0x10000,
  INLINED     = 0x20000,
} TYPEBITS;

#define EXPRTYPELIST \
  X(NOP), X(STRING), X(INT), X(UINT), X(FLOAT), X(MEMBER), X(IDENT), X(ARRAY_LIT), X(STRUCT_LIT), \
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
  X(WHILEL), X(DOWHILEL), X(FORL), \
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

#define DECLPART_TYPE X(POINTERSPEC), X(ARRAYSPEC), X(PARAMSSPEC), X(BITFIELDSPEC), X(NAMELESS_PARAMSSPEC)
#define X(name) name
enum declpart_info {
  DECLPART_TYPE
};
#undef X

struct stmt;

typedef struct {
  DYNARR* fields;//Each entry is a struct that contains a full identifier
  char* name;
  HASHTABLE* offsets; //each entry is a struct that contains a full identifier and offset
  int size;
} STRUCT;
typedef struct {
  DYNARR* fields;//Each entry is a struct that contains a full identifier
  char* name;
  HASHTABLE* hfields;//Each entry is a STRUCTFIELD
  int size;
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
  long offset;
  IDTYPE* type;
} STRUCTFIELD;

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
  int caseindex;
  int numvars;
} FUNC;

typedef struct expr {
  EXPRTYPE type;
  DYNARR* params;
  IDTYPE* rettype;
  union {
    IDTYPE* vartype;
    char* strconst;
    char* member;
    long intconst;
    unsigned long uintconst;
    double floatconst;
    IDENTIFIERINFO* id;
  };
} EXPRESSION;

struct lstate {
  DYNARR* argpp;
  DYNARR* locs;
  DYNARR* file2compile;
  DYNARR* parg;
  char stmtover, argeaten;
  char* defname;
  HASHTABLE* defargs;
  int paren_depth;
  struct macrodef* md;
  DYNSTR* dstrdly, * mdstrdly, * strcur;
};
struct lexctx {
  HASHTABLE* funcs;
  DYNARR* scopes;
  DYNARR* definestack;
  FUNC* func;
  BIGHASHTABLE* defines;
  HASHTABLE* withindefines;
  DYNARR* enstruct2free;
  DYNARR* enumerat2free;
  DYNARR* globals;
  DYNARR* externglobals;
  struct lstate* ls;
};

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
    struct { //while or dowhile, switch
      EXPRESSION* cond;
      struct stmt* body;
      PARALLEL* labeltable;
      char* defaultlbl;
    };
    struct { //for
      EOI* forinit;
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
    DYNARR* nameless_params;
    struct {
      int arrlen;
      int arrmaxind;
    };
    EXPRESSION* bfspec;
    TYPEBITS ptrspec;
    void* garbage;
  };
};

typedef struct {
  char* varname;
  IDTYPE* type;
  long varid;
} DECLARATION;

typedef struct {
  DECLARATION* decl;
  EXPRESSION* expr;
} INITIALIZER;

struct intinfo {
  long num;
  char sign;
}; 

typedef struct {
  PARALLEL* cases;
  char* defaultval;
} SWITCHINFO;

typedef struct {
  char isfunc;
  union {
    INITIALIZER* i;
    FUNC* f;
    void* garbage;
  };
} TOPBLOCK;

#define MEMBERTYPELIST X(M_TYPEDEF), X(M_VARIABLE), X(M_GLOBAL), X(M_STRUCT), X(M_UNION), X(M_ENUM), X(M_ENUM_CONST)
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
    void* garbage;
  };
} SCOPEMEMBER;

typedef struct {
  char truescope;
  union {
    struct {
      HASHTABLE* typesdef;//SCOPEMEMBER argument
      HASHTABLE* members;//SCOPEMEMBER argument
      HASHTABLE* structs;
      HASHTABLE* enums;
      HASHTABLE* unions;
      HASHTABLE* forwardstructs;
      HASHTABLE* forwardunions;
    };
    HASHTABLE* fakescope;
  };
} SCOPE;

enum ifdefstate {
  IFDEFDUMMY, IFANDTRUE, IFANDFALSE, ELSEANDTRUE, ELSEANDFALSE
};

struct macrodef {
  DYNSTR* text;
  DYNARR* args;//NULL if not function like
};

STRUCT* structor(char* name, DYNARR* fields, struct lexctx* lct);
UNION* unionctor(char* name, DYNARR* fields, struct lexctx* lct);
ENUM* enumctor(char* name, DYNARR* fields, struct lexctx* lct);
EXPRESSION* cloneexpr(EXPRESSION* orig);
DYNARR* ptrdaclone(DYNARR* opointerstack);
EXPRESSION* ct_nop_expr(void);
EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION* param);
EXPRESSION* ct_sztype(IDTYPE* whichtype);
EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2);
EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr );
EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3);
EXPRESSION* ct_fcall_expr(EXPRESSION* func, DYNARR* params);
EXPRESSION* ct_strconst_expr(const char* str);
EXPRESSION* ct_intconst_expr(long num); 
EXPRESSION* ct_uintconst_expr(unsigned long num);
EXPRESSION* ct_floatconst_expr(double num);
EXPRESSION* ct_array_lit(DYNARR* da);
EXPRESSION* ct_member_expr(char* member);
EXPRESSION* ct_ident_expr(struct lexctx* lct, char* ident);
char typecompat(IDTYPE* t1, IDTYPE* t2);
int process_array_lit(IDTYPE* arr_memtype, EXPRESSION* arr_expr, int arr_dim);
int process_struct_lit(IDTYPE* struct_memtype, EXPRESSION* struct_expr);
char type_compat(IDTYPE* id1, IDTYPE* id2);
char isglobal(struct lexctx* lct, char* ident);
void wipestruct(STRUCT* strct);
void freenum(ENUM* enm);
void freetype(IDTYPE* id);
void freeinit(INITIALIZER* i);
void rfreexpr(EXPRESSION* e);
void rfreestate(STATEMENT* s);
void rfreefunc(FUNC* f);
void freemd(struct macrodef* mds);
void freemd2(struct macrodef* mds);
EXPRESSION* rclonexpr(EXPRESSION* e);
DECLARATION* mkdeclaration(char* name);
INITIALIZER* geninit(DECLARATION* decl, EXPRESSION* expr);
SOI* sois(struct stmt* state);
SOI* soii(DYNARR* init);
STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express);
STATEMENT* mknopstmt(void);
STATEMENT* mkgotostmt(char* gotoloc);
STATEMENT* mkforstmt(EOI* e1, EXPRESSION* e2, EXPRESSION* e3, STATEMENT* bdy);
STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy);
STATEMENT* mkswitchstmt(EXPRESSION* contingent, STATEMENT* bdy, SWITCHINFO* swi);
STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy);
STATEMENT* mkcmpndstmt(DYNARR* stmtsandinits);
STATEMENT* mklblstmt(struct lexctx* lct, char* lblval);
STATEMENT* mkcasestmt(struct lexctx* lct, EXPRESSION* casexpr, char* label);
STATEMENT* mkdefaultstmt(struct lexctx* lct, char* label);
ENUMFIELD* genenumfield(char* name, EXPRESSION* value);
struct declarator_part* mkdeclpart(enum declpart_info typ, void* d);
struct declarator_part* mkdeclpartarr(enum declpart_info typ, EXPRESSION* d);
struct declarator_part* mkdeclptr(TYPEBITS d);
FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn);
struct lexctx* ctxinit(void);
SCOPE* mkscope(void);
SCOPE* mkfakescope(void);
void scopepush(struct lexctx* lct);
void fakescopepush(struct lexctx* lct);
void scopepop(struct lexctx* lct);
SCOPE* fakescopepeek(struct lexctx* lct);
SCOPE* scopepeek(struct lexctx* lct);
void* scopesearch(struct lexctx* lct, enum membertype mt, char* key);
char scopequeryval(struct lexctx* lct, enum membertype mt, char* key);
void defbackward(struct lexctx* lct, enum membertype mt, char* defnd, STRUCT* assignval);
INITIALIZER* decl2scope(DECLARATION* dec, EXPRESSION* ex, struct lexctx* lct);
void add2scope(struct lexctx* lct, char* memname, enum membertype mtype, void* memberval);
TOPBLOCK* gtb(char isfunc, void* assign);
void feedstruct(STRUCT* s);
int unionlen(UNION* u);

#define locprint(lv) (char*) dapeek(ctx->ls->file2compile), lv.first_line, lv.first_column, lv.last_line, lv.last_column
#define dlocprint(lv) (char*) dapeek(ctx->ls->file2compile), lv->first_line, lv->first_column, lv->last_line, lv->last_column
#define locprint2(lv) (char*) dapeek(lctx->ls->file2compile), lv->first_line, lv->first_column, lv->last_line, lv->last_column
#define ctx ((struct lexctx*) yyget_extra(scanner))
#define ispointer(x) ((x)->pointerstack && (x)->pointerstack->length)
#define ispointer2(x) ((x).pointerstack && (x).pointerstack->length)

static inline int lentype(IDTYPE* idt) {
  if(ispointer(idt)) {
    struct declarator_part* pointtop = dapeek(idt->pointerstack);
    if(pointtop->type != ARRAYSPEC) return 0x8;
    else return pointtop->arrlen;
  }
  if(idt->tb & (STRUCTVAL | UNIONVAL)) {
    return idt->structtype->size;
  }
  return idt->tb & 0xf;
}
#endif
