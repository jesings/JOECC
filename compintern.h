#ifndef COMPINTERN_H
#define COMPINTERN_H
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include "hash.h"
#include "dynarr.h"
#include "dynstr.h"
#include "parallel.h"

#define PPDEBUG 0

extern DYNARR* includepath;

typedef struct yyltype {
  int first_line;
  int last_line;
  int first_column;
  int last_column;
  char* filename;
} LOCTYPE;

/**
 * An enum describing a bitfield with a bunch of information about the type of an AST value.
 * The lowest 4 bits in this enum are not listed here, they describe the number of bytes of the value.
**/
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

/**
 * An enum describing the type of an expression in the AST.
**/
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
  X(ASMSTMT), \
  X(DEFAULT)
/**
 * An enum describing the type of a statement in the AST.
**/
#define X(name) name
enum stmttype {
  STMTTYPE_LIST
};
#undef X

#define DECLPART_TYPE X(POINTERSPEC), X(ARRAYSPEC), X(VLASPEC), X(PARAMSSPEC), X(BITFIELDSPEC), X(NAMELESS_PARAMSSPEC)
/**
 * An enum describing the type of an element of the pointerstack, whether it's a generic pointer, an array, a function pointer, a VLA, or something else
**/
#define X(name) name
enum declpart_info {
  DECLPART_TYPE
};
#undef X

struct stmt;

/**
 * A struct containing the information to describe a struct or union: most imporatntly the fields, their byte offsets from the base location of the struct
**/
typedef struct {
  DYNARR* fields;//Each entry is a struct that contains a full identifier
  char* name;
  HASHTABLE* offsets; //each entry is a struct that contains a full identifier and offset
  int size;
} USTRUCT;
/**
 * A struct containing the information necessary to describe an enum
**/
typedef struct {
  DYNARR* fields;
  char* name;
} ENUM;

/**
 * A struct containing the information to describe a type, whether it contains a struct, has multiple pointer indirection, or not.
**/
typedef struct {
  DYNARR* pointerstack;
  TYPEBITS tb;
  union {
    USTRUCT* structtype;
    USTRUCT* uniontype;
    ENUM* enumtype;
  };
} IDTYPE;

/**
 * A struct containing the information to describe a single field of a struct
**/
typedef struct {
  long offset;
  IDTYPE* type;
} STRUCTFIELD;

/**
 * A struct describing a single operand in the operands portion of an asm statement
**/
typedef struct {
  char* constraint;
  struct expr* varin;
} OPERAND;

/**
 * A struct describing the information associated with a single identifier
 * An index of -1 refers to a not yet specified index and an index of -2 refers to a function with global scope.
**/
typedef struct {
  IDTYPE* type;
  char* name;
  long index;
} IDENTIFIERINFO;

/**
 * A struct containing all the information necessary to process/describe a compilation unit (function), in AST form.
 * The body field contains the root of the actual AST in a compound statement.
 * The params, retrn, lbls, and numvars fields should be self-evident.
 * The switchstack field is necessary during compilation, it contains a stack of the switch statements that enclose
 * the currently compiling statement/expression. This is needed to convert switch cases into labels to clarify the
 * AST representation of switches. The caseindex is used to generate unique label names for each case in a function.
 * is currently 
**/
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

/**
 * A struct containing all the information necessary to describe an expression in the AST representation.
 * Type tags which anonymous union member is used, the id member is used only for sizeof.
**/
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
  LOCTYPE location;
} EXPRESSION;


/**
 * A struct storing state necessary for the processing of an array with designated initializers. Curpt
 * is the last number designator that we have seen, and non designated elements increment that.
**/
typedef struct {
  DYNARR* inits;
  int curpt;
} DESIGNARR;

/**
 * Stores some "global" state information for the lexer, which we need in a struct for the reentrant parser.
 * argpp is the array of arguments passed to the current macro that is being called
 * locs is the stack of location information structs
 * parg is the array of arguments that we have parsed so far to the current macro call
 * stmtover, argeaten are simple status flags
 * defname is the name of hte macro currently being defined
 * defargs are the arguments of the macro currently being called, i.e. which things to replace
 * paren_depth is the number of nested parens while lexing macro arguments, to tell when a comma ends an arg.
 * md is the macro currently being defined
 * dstrdly is the dynamic string currently being parsed, mdstrdly is the same but for a macro dynstring
 * strcur is the same but for an actual string literal.
 **/
struct lstate {
  DYNARR* argpp;
  DYNARR* locs;
  DYNARR* parg;
  char stmtover, argeaten;
  char* defname;
  HASHTABLE* defargs;
  int paren_depth;
  struct macrodef* md;
  DYNSTR* dstrdly, * mdstrdly, * strcur;
};

/**
 * Stores some "global" state information for the parser and lexer, which we need to struct wrap for a reentrant parser.
 * funcs stores the already declared functions.
 * scopes is a stack of the scopes within which we are currently parsing, useful for variable definitions
 * definestack is the stack of macros that we are currently in the process of parsing
 * withindefines takes all the names of the macros from definestack into a hashmap, used for disallowing recursive macro execution
 * enstruct2free allows STRUCT structs and UNION structs to be lazily freed rather than doing any complex reference counting
 * enumerat2free is the same but for enums
 * globals, externglobals, ls should be evident
 * actualroot holds the actual file that we want to start parsing/lexing at. We start the lexer off looking at a file which
 * we have constructed which contains the -D definitions passed on the command line, and switch to this new actualroot and 
 * reassign it to null when that is done.
**/
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
  FILE* actualroot;
};

/**
 * Stores an expression or initializer, tagged, used for stuff before the first semicolon in a for loop head.
**/
typedef struct {
  char isE;
  union {
    DYNARR* I;
    EXPRESSION* E;
  };
} EOI;

/**
 * Stores a (surprise, surprise) statement, type tags which element of the anonymous enum is used.
**/
typedef struct stmt {
  enum stmttype type;
  LOCTYPE location;
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
    struct {
      char* asmstmts;
      DYNARR* outputs;
      DYNARR* inputs;
      DYNARR* clobbers;
    };
    char* glabel; //for label and goto
    DYNARR* stmtsandinits; //compound
  };
} STATEMENT;

/**
 * Stores a statement or initializer, used as a generic list element for i.e. a compound statement.
**/
typedef struct {
  char isstmt;
  union {
    struct stmt* state;
    DYNARR* init;
  };
} SOI;

/**
 * Stores the name, and an expression giving a value, to an element, describes a value in an enum.
**/
typedef struct {
  char* name;
  EXPRESSION* value;
} ENUMFIELD;

/**
 * Describes a single part of a pointer, type tags which field is used. These are put on the pointerstack of an IDTYPE.
 * PARAMSSPEC describes a function (NOT a function pointer, any POINTERSPECs on top of it make it a function pointer)
 * PARAMSSPEC has an array of its arguments. Anything above it on the pointerstack describes it (i.e. whether it's a 
 * function pointer, array of function pointers, etc), anything below it describes the return type.
 * NAMELESS_PARAMSSPEC is the same but the parameter representation does not include the names of each parameter.
 * ARRAYSPEC describes an array, it contains the maximum index, and the length of the array in bytes, but these aren't
 * guaranteed to be populated right after its declaration is processed due to i.e. int foo[] syntax.
 * VLASPEC describes a VLA, it has an expression describing its length, and addrun and addrty describing the
 * location and type respectively of the place where the length of the VLA is stored in 3ac, only accessible/useful
 * while the 3ac gets generated.
**/
struct declarator_part {
  enum declpart_info type;
  union {
    DYNARR* params;
    DYNARR* nameless_params;
    struct {
      int arrlen;
      int arrmaxind;
    };
    struct {
      EXPRESSION* vlaent;
      void* addrun;
      int addrty;
    };
    EXPRESSION* bfspec;
    TYPEBITS ptrspec;
    void* garbage;
  };
};

/**
 * Describes a declaration, including the name, type, and index
**/
typedef struct {
  char* varname;
  IDTYPE* type;
  long varid;
} DECLARATION;

/**
 * It is very obvious what this is
**/
typedef struct {
  DECLARATION* decl;
  EXPRESSION* expr;
} INITIALIZER;

/**
 * Contains the info for constructing a switch statement--the names of each case label, and the name of the default case
**/
typedef struct {
  PARALLEL* cases;
  char* defaultval;
} SWITCHINFO;

/**
 * Tags the type of a scope member
**/
#define MEMBERTYPELIST X(M_TYPEDEF), X(M_VARIABLE), X(M_GLOBAL), X(M_STRUCT), X(M_UNION), X(M_ENUM), X(M_ENUM_CONST)
#define X(name) name
enum membertype {
  MEMBERTYPELIST
};
#undef X

/**
 * Describes a member of a scope, tagged by mtype. Members of scopes are any scope-bound thing that is usable (i.e. variables, typedefs, structs, etc.)
**/
typedef struct {
  enum membertype mtype;
  union {
    USTRUCT* structmemb;
    ENUM* enummemb;
    USTRUCT* unionmemb;
    IDTYPE* typememb;
    IDENTIFIERINFO* idi;
    EXPRESSION* enumnum;
    void* garbage;
  };
} SCOPEMEMBER;

/**
 * Describes a scope, as placed on the scopestack--truescope tags whether it is a scope with the normal members or a fakescope.
 * Fakescopes are used within union/struct bodies in order to scope-bind the members declared there.
**/
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

/**
 * Possible states for the lexer to be in, to be pushed onto a stack. These states are pushed to the stack/updated when we encounter a #if #ifdef #else, etc.
 * IFDEFDUMMY, IFANDFALSE, ELSEANDTRUE tell the lexer that we have not been lexing actual code since the value was pushed to the stack.
**/
enum ifdefstate {
  IFDEFDUMMY, IFANDTRUE, IFANDFALSE, ELSEANDTRUE, ELSEANDFALSE
};

/**
 * Describes a macro, complete with the body of the macro in text, and the arguments of the macro, in string form, in args
 * Used within the lexer to actually effect the macro replacements
**/
struct macrodef {
  DYNSTR* text;
  DYNARR* args;//NULL if macro is not function like
};

#define ispointer(x) ((x)->pointerstack && (x)->pointerstack->length)
#define ispointer2(x) ((x).pointerstack && (x).pointerstack->length)
//gets size of IDTYPE in bytes
static inline int lentype(IDTYPE* idt) {
  if(ispointer(idt)) {
    struct declarator_part* pointtop = dapeek(idt->pointerstack);
    if(pointtop->type == VLASPEC) {
      return -1;
    } else if(pointtop->type != ARRAYSPEC) {
      return 0x8;
    } else {
      if(pointtop->arrlen == -1) {
          idt->pointerstack->length--;
          pointtop->arrlen = lentype(idt) * pointtop->arrmaxind;
          idt->pointerstack->length++;
          assert(pointtop->arrlen > 0);
      }
      return pointtop->arrlen;
    }
  } else if(idt->tb & (STRUCTVAL | UNIONVAL)) {
    return idt->structtype->size;
  }
  return idt->tb & 0xf;
}


USTRUCT* ustructor(char* name, DYNARR* fields, struct lexctx* lct);
ENUM* enumctor(char* name, DYNARR* fields, struct lexctx* lct);
IDTYPE* fcid2(IDTYPE* idt);
OPERAND* genoperand(char* constraint, EXPRESSION* varin);
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
int process_array_lit(IDTYPE* arr_memtype, EXPRESSION* arr_expr);
int process_struct_lit(IDTYPE* struct_memtype, EXPRESSION* struct_expr);
char type_compat(IDTYPE* id1, IDTYPE* id2);
char isglobal(struct lexctx* lct, char* ident);
void wipestruct(USTRUCT* strct);
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
STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express, LOCTYPE* loc);
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
STATEMENT* mkasmstmt(char* asmstmts, DYNARR* outputs, DYNARR* inputs, DYNARR* clobbers);
ENUMFIELD* genenumfield(char* name, EXPRESSION* value);
struct declarator_part* mkdeclpart(enum declpart_info typ, void* d);
struct declarator_part* mkdeclpartarr(enum declpart_info typ, EXPRESSION* d);
struct declarator_part* mkdeclptr(TYPEBITS d);
FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn);
struct lexctx* ctxinit(FILE *);
SCOPE* mkscope(void);
SCOPE* mkfakescope(void);
void scopepush(struct lexctx* lct);
void fakescopepush(struct lexctx* lct);
void scopepop(struct lexctx* lct);
SCOPE* fakescopepeek(struct lexctx* lct);
SCOPE* scopepeek(struct lexctx* lct);
void* scopesearch(struct lexctx* lct, enum membertype mt, char* key);
char scopequeryval(struct lexctx* lct, enum membertype mt, char* key);
void defbackward(struct lexctx* lct, enum membertype mt, char* defnd, USTRUCT* assignval);
INITIALIZER* decl2scope(DECLARATION* dec, EXPRESSION* ex, struct lexctx* lct);
void add2scope(struct lexctx* lct, char* memname, enum membertype mtype, void* memberval);
void feedstruct(USTRUCT* s);
int unionlen(USTRUCT* u);

#define locprint(lv) yyget_lloc(scanner)->filename, lv.first_line, lv.first_column, lv.last_line, lv.last_column
#define dlocprint(lv) yyget_lloc(scanner)->filename, lv->first_line, lv->first_column, lv->last_line, lv->last_column
#define locprint2(lv) yyget_lloc(yyscanner)->filename, lv->first_line, lv->first_column, lv->last_line, lv->last_column
#define ctx ((struct lexctx*) yyget_extra(scanner))

#define bfalloc(length) calloc(1, ((length) + 7) >> 3)
#define bfclone(bitfield, length) memcpy(malloc(((length) + 7) >> 3), (bitfield), ((length) + 7) >> 3)
#define bfzero(bitfield, length) memset(bitfield, 0, ((length) + 7) >> 3)
#define bfget(bitfield, index) ((bitfield)[(index) >> 3] & (1 << ((index) & 7)))
#define bfset(bitfield, index) ((bitfield)[(index) >> 3] |= (1 << ((index) & 7)))
#define bfunset(bitfield, index) ((bitfield)[(index) >> 3] &= ~(1 << ((index) & 7)))
#define BITFIELD char*
#endif
