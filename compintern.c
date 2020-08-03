#include <stdio.h>
#include "compintern.h"
#include "treeduce.h"
#include "joecc.tab.h"

extern DYNARR* file2compile;
extern YYLTYPE yylloc;

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

EXPRESSION* ct_nop_expr(void) {
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
  if(!(whichtype->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)))
    return ct_intconst_expr(whichtype->tb & 0xf);
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = SZOF;
  retval->vartype = whichtype;
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
  retval->vartype = type;
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

EXPRESSION* ct_strconst_expr(const char* str) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->strconst = (char*)(unsigned long) str;
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
    printf("Error: use of undefined variable %s at %s %d.%d-%d.%d\n", ident, locprint(yylloc));
    exit(-1);
  }
  return retval;
}

char isglobal(struct lexctx* lct, char* ident) {
  SCOPE* sc = daget(lct->scopes, 0);
  return queryval(sc->members, ident);
}

void wipestruct(STRUCT* strct) {
  for(int i = 0; i < strct->fields->length; ++i) {
    DECLARATION* dcl = strct->fields->arr[i];
    if(dcl->varname) {
      free(dcl->varname);
    }
    freetype(dcl->type);
    free(dcl);
  }
  free(strct);
}

void freetype(IDTYPE* id) {
  if(id->pointerstack)
    for(int i = 0; i < id->pointerstack->length; i++)
      free(id->pointerstack->arr[i]);
  if(id->tb & (STRUCTVAL | UNIONVAL)) {
    if(!id->structtype->name)
      wipestruct(id->structtype);
  }
  else if(id->tb & ENUMVAL) {
    for(int i = 0; i < id->enumtype->fields->length; ++i) {
      ENUMFIELD* enf = id->enumtype->fields->arr[i];
      rfreexpr(enf->value);
      free(enf->name);
      free(enf);
    }
    free(id->enumtype);
  }
  free(id);
}

void rfreexpr(EXPRESSION* e) {
  switch(e->type) {
    default:
      for(int i = 0; i < e->params->length; i++)
        rfreexpr(e->params->arr[i]);
    case NOP:
      break;
    case SZOF:
      if(e->vartype->pointerstack)
        for(int i = 0; i < e->vartype->pointerstack->length; i++)
          free(e->vartype->pointerstack->arr[i]);
      free(e->vartype);
      break;
    case CAST:
      freetype(e->vartype);
      for(int i = 0; i < e->params->length; i++)
        rfreexpr(e->params->arr[i]);
      break;
    case STRING:
      free(e->strconst);
      break;
    case MEMBER:
      free(e->member);
      break;
    case INT: case UINT: case FLOAT:
      break;
    case IDENT:
      break;//do not free identifier info
    case ARRAY_LIT:
      for(int i = 0; i < e->dynvals->length; i++)
        rfreexpr(e->dynvals->arr[i]);
      break;
  }
  free(e);
}
EXPRESSION* rclonexpr(EXPRESSION* e) {
  EXPRESSION* e2 = malloc(sizeof(EXPRESSION));
  memcpy(e2, e, sizeof(EXPRESSION));
  switch(e->type) {
    default:
      e2->params = dactor(e->params->length);
      for(int i = 0; i < e->params->length; i++)
        dapush(e2->params, rclonexpr(e->params->arr[i]));
    case NOP:
      break;
    case SZOF:
      e2->vartype = malloc(sizeof(IDTYPE));
      memcpy(e2->vartype, e->vartype, sizeof(IDTYPE));
      if(e->vartype->pointerstack) {
        for(int i = 0; i < e->vartype->pointerstack->length; i++) {
          struct declarator_part* dp = malloc(sizeof(struct declarator_part));
          memcpy(dp, e->vartype->pointerstack->arr[i], sizeof(struct declarator_part));
          dapush(e2->vartype->pointerstack, dp);
        }
      }
      break;
    case CAST:
      e2->vartype = malloc(sizeof(IDTYPE));
      memcpy(e2->vartype, e->vartype, sizeof(IDTYPE));
      if(e->vartype->pointerstack) {
        for(int i = 0; i < e->vartype->pointerstack->length; i++) {
          struct declarator_part* dp = malloc(sizeof(struct declarator_part));
          memcpy(dp, e->vartype->pointerstack->arr[i], sizeof(struct declarator_part));
          dapush(e2->vartype->pointerstack, dp);
        }
      }
      e2->params = dactor(e->params->length);
      for(int i = 0; i < e->params->length; i++)
        dapush(e2->params, rclonexpr(e->params->arr[i]));
      break;
    case STRING:
      e2->strconst = strdup(e->strconst);
      break;
    case MEMBER:
      e2->member = strdup(e->member);
      break;
    case INT: case UINT: case FLOAT:
      break;
    case IDENT:
      break;//do not free identifier info
    case ARRAY_LIT:
      e2->dynvals = dactor(e->dynvals->length);
      for(int i = 0; i < e->dynvals->length; i++)
        dapush(e2->dynvals, rclonexpr(e->dynvals->arr[i]));
      break;
  }
  return e2;
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
  retval->type = CMPND;
  retval->stmtsandinits = dactor(8);
  if(e1->isE) {
    dapush(retval->stmtsandinits, sois(mkexprstmt(EXPR, e1->E)));
  } else {
    dapush(retval->stmtsandinits, soii(e1->I));
  }
  free(e1);
  STATEMENT* subloop = malloc(sizeof(STATEMENT));
  subloop->type = WHILEL;
  subloop->cond = e2;
  if(bdy->type == CMPND && bdy->stmtsandinits) {
    dapush(bdy->stmtsandinits, sois(mkexprstmt(EXPR, e3)));
    subloop->body = bdy;
  } else {
    STATEMENT* loopbdy = malloc(sizeof(STATEMENT));
    loopbdy->type = CMPND;
    loopbdy->stmtsandinits = dactor(2);
    dapush(loopbdy->stmtsandinits, sois(bdy));
    dapush(loopbdy->stmtsandinits, sois(mkexprstmt(EXPR, e3)));
    subloop->body = loopbdy;
  }
  dapush(retval->stmtsandinits, sois(subloop));

  return retval;
}

STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->cond = condition;
  retval->body = bdy;
  return retval;
}

STATEMENT* mkswitchstmt(EXPRESSION* contingent, STATEMENT* bdy, PARALLEL* lbltbl) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = SWITCH;
  retval->cond = contingent;
  retval->body = bdy;
  retval->labeltable = lbltbl;
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

STATEMENT* mklblstmt(struct lexctx* lct, char* lblval) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = LABEL;
  retval->glabel = lblval;
  insert(lct->func->lbls, lblval, NULL);
  //confirm no collision
  return retval;
}

STATEMENT* mkcasestmt(struct lexctx* lct, EXPRESSION* casexpr, char* label) {
  PARALLEL* pl = dapeek(lct->func->switchstack);
  pinsert(pl, label, casexpr);//TODO reverse the order of these--expr rectified to int should point to label
  return mklblstmt(lct, label);
}

STATEMENT* mkdefaultstmt(struct lexctx* lct, char* label) {
  PARALLEL* pl = dapeek(lct->func->switchstack);
  pinsert(pl, label, ct_strconst_expr((char*)(unsigned long)"default"));//TODO reverse the order of these--expr rectified to int should point to label
  return mklblstmt(lct, label);
}

ENUMFIELD* genenumfield(char* name, EXPRESSION* value) {
  ENUMFIELD* retval = malloc(sizeof(ENUMFIELD));
  retval->name = name;
  //confirm is expr that will resolve into something uintconstifiable that is known at compile time
  while(foldconst(&value)) ;
  switch(value->type) {
    case INT: case UINT:
      break;
    default:
      printf("Error: enum has nonrectifiable value\n");
      exit(-1);
  }
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

FUNC* ct_function(char* name, STATEMENT* body, PARALLEL* params, IDTYPE* retrn) {
  FUNC* func = malloc(sizeof(FUNC));
  func->name = name;
  func->body = body;
  func->params = params;
  func->retrn = retrn;
  func->lbls = htctor();
  func->switchstack = dactor(8);
  func->caseindex = 0;
  func->numvars = 0;
  func->purity = -1;
  return func;
}

SCOPE* mkscope(void) {
  SCOPE* child = malloc(sizeof(SCOPE));
  child->truescope = 1;
  child->members = htctor();
  child->structs = htctor();
  child->enums = htctor();
  child->unions = htctor();
  child->typesdef = htctor();
  //the below have values consisting of dynarrs of pointers where the address of the STRUCT* should be placed
  child->forwardstructs = htctor();
  child->forwardunions = htctor();
  return child;
}

SCOPE* mkfakescope(void) {
  SCOPE* child = malloc(sizeof(SCOPE));
  child->truescope = 0;
  child->fakescope = htctor();
  return child;
}

void defbackward(struct lexctx* lct, enum membertype mt, char* defnd, void* assignval) {
  DYNARR* da;
  switch(mt) {
    case M_STRUCT:
      da = (DYNARR*) search(scopepeek(lct)->forwardstructs, defnd);
      rmpair(scopepeek(lct)->forwardstructs, defnd);
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
    if(!htp->truescope)
      continue;
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
    if(rv && rv->mtype == mt) {
      switch(rv->mtype) {
        case M_ENUM_CONST:
          return rv->enumnum;
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
        case M_GLOBAL: 
          //global should never be encountered, they're coerced to variables on declaration
          fprintf(stderr, "Error: corrupted global variable encountered");
          return rv->idi;
      }
    }
  }
  return NULL;
}

char scopequeryval(struct lexctx* lct, enum membertype mt, char* key) {
  for(int i = lct->scopes->length - 1; i >= 0; i--) {
    SCOPE* htp = daget(lct->scopes, i);
    if(!htp->truescope)
      continue;
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
    SCOPEMEMBER* rv = search(ht, key);//will return scope object
    if(rv && rv->mtype == mt) 
      return 1;
  }
  return 0;
}

static void declmacro(HASHTABLE* ht, const char* macroname, const char* body) {
  struct macrodef* md = calloc(1, sizeof(struct macrodef));
  md->text = (char*)(unsigned long) body;
  insert(ht, macroname, md);
}

struct lexctx* ctxinit(void) {
  struct lexctx* lct =  malloc(sizeof(struct lexctx));
  lct->funcs = htctor();
  lct->definestack = dactor(64);
  lct->scopes = dactor(64);
  lct->func = NULL;
  dapush(lct->scopes, mkscope());
  lct->withindefines = htctor();
  lct->defines = htctor();
  declmacro(lct->defines, "__STDC__", "1");
  declmacro(lct->defines, "__STDC_VERSION__", "201710L");
  declmacro(lct->defines, "__STDC_HOSTED__", "1"); 
  declmacro(lct->defines, "_XOPEN_SOURCE", "700"); 
  declmacro(lct->defines, "_DEFAULT_SOURCE", "700"); 
  declmacro(lct->defines, "_POSIX_C_SOURCE", "200809L"); 
  declmacro(lct->defines, "_XOPEN_SOURCE_EXTENDED", "1"); 
  declmacro(lct->defines, "_USE_XOPEN_EXTENDED", "1"); 
  declmacro(lct->defines, "__FILE__", NULL); 
  declmacro(lct->defines, "__LINE__", NULL); 
  declmacro(lct->defines, "__DATE__", NULL); 
  declmacro(lct->defines, "__TIME__", NULL); 
  declmacro(lct->defines, "__func__", NULL); 
  declmacro(lct->defines, "__x86_64__", "1"); 
  declmacro(lct->defines, "__linux__", "1"); 
  declmacro(lct->defines, "__builtin_va_list", "byte*"); //should be typedef
  return lct;
}

void scopepush(struct lexctx* lct) {
  dapush(lct->scopes, mkscope());
}
void fakescopepush(struct lexctx* lct) {
  dapush(lct->scopes, mkfakescope());
}

void scopepop(struct lexctx* lct) {
  SCOPE* cleanup = dapop(lct->scopes);
  if(cleanup->truescope && (
     cleanup->forwardstructs->keys != 0 ||
     cleanup->forwardunions->keys != 0))
    fprintf(stderr, "Error: not all forward declarations processed by end of scope\n");
  free(cleanup); //free all members???
}

SCOPE* fakescopepeek(struct lexctx* lct) {
  return dapeek(lct->scopes);
}
SCOPE* scopepeek(struct lexctx* lct) {
  for(int i = lct->scopes->length - 1; i >= 0; i--) {
    SCOPE* htp = daget(lct->scopes, i);
    if(htp->truescope)
      return htp;
  }
  fprintf(stderr, "Error: corrupted scope environment encountered\n");
  return NULL;
}

void add2scope(struct lexctx* lct, char* memname, enum membertype mtype, void* memberval) {
  SCOPE* scope = scopepeek(lct);
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
      sm->idi->index= lct->func->numvars++;
      insert(scope->members, memname, sm);
      break;
    case M_GLOBAL:
      sm->mtype = M_VARIABLE;
      //because we manually construct identifierinfo
      //fall through
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
