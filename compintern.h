#include "hash.h"
#include "dynarr.h"

typedef enum{
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

struct stmt;

typedef struct{
  DYNARR* fields;//Each entry is a struct that contains a full identifier and a size
  char* name;
} STRUCT;
typedef struct{
  DYNARR* fields;//Each entry is a struct that contains a full identifier and a size
  char* name;
} UNION;
typedef struct{
  char* name;
  DYNARR* fields;
} ENUM;

typedef struct{
  DYNARR* pointerstack;
  TYPEBITS tb;
  union{
    STRUCT* structtype;
    UNION* uniontype;
    ENUM* enumtype;
  };
} IDTYPE;
typedef struct function{
  char* name;
  struct stmt* body; //compound statement
  DYNARR* params;
  IDTYPE* retrn;
} FUNC;
struct lexctx {//TODO: FIX
  FUNC* funclist;
  unsigned int fllast, fllen;
  FUNC* curfunc;
  DYNARR* scopes;
  unsigned int layer;//Necessary?
  HASHTABLE* idents;
};
