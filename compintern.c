#include <stdio.h>
#include "compintern.h"

STRUCT* structor(char* name, DYNARR* fields) {
    STRUCT* retval = malloc(sizeof(STRUCT));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

UNION* unionctor(char* name, DYNARR* fields) {
    UNION* retval = malloc(sizeof(UNION));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

ENUM* enumctor(char* name, DYNARR* fields) {
    ENUM* retval = malloc(sizeof(ENUM));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

EXPRESSION* cloneexpr(EXPRESSION* orig) {
  EXPRESSION* clone = malloc(sizeof(EXPRESSION));
  memcpy(clone, orig, sizeof(EXPRESSION));
  return clone;
}

EXPRESSION* ct_nop_expr() {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = NOP;
  return retval;
}

EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION* param) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->params = dactor(1);
  dapush(retval->params, param);
  return retval;
}

EXPRESSION* ct_sztype(IDTYPE* whichtype) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = SZOF;
  retval->typesz = whichtype;
  return retval;
}

EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->params = dactor(2);
  dapush(retval->params, param1);
  dapush(retval->params, param2);
  return retval;
}

EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = CAST;
  retval->params = dactor(1);
  dapush(retval->params, expr);
  retval->casttype = type;
  return retval;
}

EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = TERNARY;
  retval->params = dactor(3);
  dapush(retval->params, param1);
  dapush(retval->params, param2);
  dapush(retval->params, param3);
  return retval;
}

EXPRESSION* ct_fcall_expr(EXPRESSION* func, DYNARR* params) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FCALL;
  DYNARR* dd = dactor(1);
  dapush(dd, func);
  retval->params = damerge(dd, params);
  return retval;
}

EXPRESSION* ct_strconst_expr(char* str) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->strconst = str;
  return retval;
}

EXPRESSION* ct_intconst_expr(long num) { 
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->intconst = num;
  return retval;
}

EXPRESSION* ct_uintconst_expr(unsigned long num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->uintconst = num;
  return retval;
}

EXPRESSION* ct_floatconst_expr(double num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = FLOAT;
  retval->floatconst = num;
  return retval;
}

EXPRESSION* ct_array_lit(DYNARR* da) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = ARRAY_LIT;
  retval->dynvals = da;
  return retval;
}

EXPRESSION* ct_member_expr(char* member) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = MEMBER;
  retval->member = member;
  return retval;
}

EXPRESSION* ct_ident_expr(struct lexctx* lct, char* ident) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = IDENT;
  retval->id = scopesearch(lct, M_VARIABLE, ident);
  if(!retval->id) {
    retval->id = malloc(sizeof(IDENTIFIERINFO));
    retval->id->index = -1;
    retval->id->name = ident;
    retval->id->type = NULL;
  }
  return retval;
}

DECLARATION* mkdeclaration(char* name) {
  DECLARATION* retval = calloc(1,sizeof(DECLARATION));
  retval->varname = name;
  IDTYPE* idt = calloc(1, sizeof(IDTYPE));
  idt->pointerstack = dactor(4);
  retval->type = idt;
  return retval;
}

INITIALIZER* geninit(DECLARATION* decl, EXPRESSION* expr) {
  INITIALIZER* retval = malloc(sizeof(INITIALIZER));
  retval->decl = decl;
  retval->expr = expr;
  return retval;
}

SOI* sois(struct stmt* state) {
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 1;
  retval->state = state;
  return retval;
}

SOI* soii(DYNARR* init) {
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 0;
  retval->init = init;
  return retval;
}

STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->expression = express;
  return retval;
}

STATEMENT* mkgotostmt(char* gotoloc) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = JGOTO;
  retval->glabel = gotoloc;
  return retval;
}

STATEMENT* mkforstmt(EOI* e1, EXPRESSION* e2, EXPRESSION* e3, STATEMENT* bdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = FORL;
  retval->init = e1;
  retval->forcond = e2;
  retval->increment = e3;
  retval->forbody = bdy;
  return retval;
}

STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->cond = condition;
  retval->body = bdy;
  return retval;
}

STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = IFS;
  retval->ifcond = condition;
  retval->thencond = ifbdy;
  retval->elsecond = elsebdy;
  return retval;
}

STATEMENT* mkcmpndstmt(DYNARR* stmtsandinits) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CMPND;
  retval->stmtsandinits = stmtsandinits;
  return retval;
}

STATEMENT* mklblstmt(char* identifier) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = LABEL;
  retval->glabel = identifier;
  return retval;
}

STATEMENT* mkcasestmt(EXPRESSION* casexpr, char* label) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CASE;
  retval->casecond = casexpr;
  retval->caselabel = label;
  return retval;
}

STATEMENT* mkdefaultstmt() {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = DEFAULT;
  return retval;
}

ENUMFIELD* genenumfield(char* name, EXPRESSION* value) {
  ENUMFIELD* retval = malloc(sizeof(ENUMFIELD));
  retval->name = name;
  //confirm is expr that will resolve into something uintconstifiable that is known at compile time
  retval->value = value;
  return retval;
}

struct declarator_part* mkdeclpart(enum declpart_info typ, void* d) {
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = typ;
  retval->garbage = d;
  return retval;
}

struct declarator_part* mkdeclptr(TYPEBITS d) {
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = POINTERSPEC;
  retval->ptrspec = d;
  return retval;
}

EXPRESSION* exprfromdecl(char* name, IDTYPE* id) {
  EXPRESSION* outer = malloc(sizeof(EXPRESSION));
  outer->type = IDENT;
  //outer->id = id;
  DYNARR* ptrs = id->pointerstack;
  if(ptrs) {
    for(int i = 0; i<ptrs->length; i++) {
      //TODO
    }
  }
  return outer;
}

FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn) {
  FUNC* func = malloc(sizeof(FUNC));
  func->name = name;
  func->body = body;
  func->params = params;
  func->retrn = retrn;
  return func;
}

SCOPE* mkscope() {
  SCOPE* child = malloc(sizeof(SCOPE));
  child->members = htctor();
  child->structs = htctor();
  child->enums = htctor();
  child->unions = htctor();
  child->typesdef = htctor();
  //the below have values consisting of dynarrs of pointers where the address of the STRUCT* should be placed
  child->forwardstructs = htctor();
  child->forwardenums = htctor();
  child->forwardunions = htctor();
  return child;
}

void defbackward(struct lexctx* lct, enum membertype mt, char* defnd, void* assignval) {
  DYNARR* da;
  switch(mt) {
    case M_STRUCT:
      da = (DYNARR*) search(scopepeek(lct)->forwardstructs, defnd);
      rmpair(scopepeek(lct)->forwardstructs, defnd);
      break;
    case M_ENUM:
      da = (DYNARR*) search(scopepeek(lct)->forwardenums, defnd);
      rmpair(scopepeek(lct)->forwardenums, defnd);
      break;
    case M_UNION:
      da = (DYNARR*) search(scopepeek(lct)->forwardunions, defnd);
      rmpair(scopepeek(lct)->forwardunions, defnd);
      break;
    default:
      fprintf(stderr, "Error: attempt to backwards define symbol of wrong type: %s\n", defnd);
      return;
  }
  for(int i = 0; i < da->length; i++)
    *(void**) daget(da, i) = assignval;
  dadtor(da);
}

void* scopesearch(struct lexctx* lct, enum membertype mt, char* key){
  for(int i = lct->scopes->length - 1; i >= 0; i--) {
    SCOPE* htp = daget(lct->scopes, i);
    HASHTABLE* ht;
    switch(mt) {
      default:
      case M_VARIABLE:
        ht = htp->members;
        break;
      case M_STRUCT:
        ht = htp->structs;
        break;
      case M_ENUM:
        ht = htp->enums;
        break;
      case M_UNION:
        ht = htp->unions;
        break;
      case M_TYPEDEF:
        ht = htp->typesdef;
        break;
    }
    SCOPEMEMBER* rv = (SCOPEMEMBER*) search(ht, key);
    if(rv) {
      switch(rv->mtype) {
        case M_ENUM_CONST:
          return rv->enumnum;
        case M_CASE:
          return rv->caseval;
        case M_VARIABLE:
          return rv->idi;
        case M_STRUCT:
          return rv->structmemb;
        case M_ENUM:
          return rv->enummemb;
        case M_UNION:
          return rv->unionmemb;
        case M_TYPEDEF:
          return rv->typememb;
        case M_LABEL:
          //not needed?
          break;
      }
    }
  }
  return NULL;
}

SCOPEMEMBER* scopesearchmem(struct lexctx* lct, enum membertype mt, char* key) {
  for(int i = lct->scopes->length - 1; i >= 0; i--) {
    SCOPE* htp = daget(lct->scopes, i);
    HASHTABLE* ht;
    switch(mt) {
      default:
      case M_VARIABLE:
        ht = htp->members;
        break;
      case M_STRUCT:
        ht = htp->structs;
        break;
      case M_ENUM:
        ht = htp->enums;
        break;
      case M_UNION:
        ht = htp->unions;
        break;
      case M_TYPEDEF:
        ht = htp->typesdef;
        break;
    }
    SCOPEMEMBER* rv = (SCOPEMEMBER*) search(ht, key);
    if(rv) return rv;
  }
  return NULL;
}

char scopequeryval(struct lexctx* lct, enum membertype mt, char* key) {
  for(int i = lct->scopes->length - 1; i >= 0; i--) {
    SCOPE* htp = daget(lct->scopes, i);
    HASHTABLE* ht;
    switch(mt) {
      default:
      case M_VARIABLE:
        ht = htp->members;
        break;
      case M_STRUCT:
        ht = htp->structs;
        break;
      case M_ENUM:
        ht = htp->enums;
        break;
      case M_UNION:
        ht = htp->unions;
        break;
      case M_TYPEDEF:
        ht = htp->typesdef;
        break;
    }
    void* rv = search(ht, key);//will return scope object
    if(rv) return 1;
  }
  return 0;
}

struct lexctx* ctxinit() {
  struct lexctx* lct =  malloc(sizeof(struct lexctx));
  lct->funcs = htctor();
  lct->defines = htctor();
  lct->definestack = dactor(64);
  lct->scopes = dactor(64);
  dapush(lct->scopes, mkscope());
  return lct;
}

void scopepush(struct lexctx* ctx) {
  dapush(ctx->scopes, mkscope());
}

void scopepop(struct lexctx* ctx) {
  SCOPE* cleanup = dapop(ctx->scopes);
  if(cleanup->forwardstructs->keys != 0 ||
     cleanup->forwardunions->keys != 0 ||
     cleanup->forwardenums->keys != 0)
    fprintf(stderr, "Error: not all forward declarations processed by end of scope\n");
  free(cleanup);
}

SCOPE* scopepeek(struct lexctx* ctx) {
  return dapeek(ctx->scopes);
}

static long numvars = 0;//maybe do something special with global variables
void add2scope(SCOPE* scope, char* memname, enum membertype mtype, void* memberval) {
  SCOPEMEMBER* sm = malloc(sizeof(SCOPEMEMBER));
  sm->mtype = mtype;
  switch(mtype) {
    case M_STRUCT:
      sm->structmemb = memberval;
      insert(scope->structs, memname, sm);
      break;
    case M_UNION:
      sm->unionmemb = memberval;
      insert(scope->unions, memname, sm);
      break;
    case M_ENUM:
      sm->enummemb = memberval;
      insert(scope->enums, memname, sm);
      break;
    case M_TYPEDEF:
      sm->typememb = memberval;
      insert(scope->typesdef, memname, sm);
      break;
    case M_VARIABLE:
      sm->idi = malloc(sizeof(IDENTIFIERINFO));
      sm->idi->name = memname;
      sm->idi->type = memberval;
      sm->idi->index= numvars++;
      insert(scope->members, memname, sm);
      break;
     default:
      sm->garbage = memberval;
      insert(scope->members, memname, sm);
      break;
  }
}

TOPBLOCK* gtb(char isfunc, void* assign) {
  TOPBLOCK* retval = malloc(sizeof(TOPBLOCK));
  retval->isfunc = isfunc;
  retval->garbage = assign;
  return retval;
}
