#include <stdio.h>
#include <assert.h>
#include "compintern.h"
#include "treeduce.h"
#include "joecc.tab.h"

extern DYNARR* file2compile;
extern YYLTYPE yylloc;

STRUCT* structor(char* name, DYNARR* fields) {
    STRUCT* retval = malloc(sizeof(STRUCT));
    retval->name = name;
    retval->fields = fields;
    retval->offsets = NULL;
    retval->size = 0;
    return retval;
}

UNION* unionctor(char* name, DYNARR* fields) {
    UNION* retval = malloc(sizeof(UNION));
    retval->name = name;
    retval->fields = fields;
    retval->size = 0;
    return retval;
}

ENUM* enumctor(char* name, DYNARR* fields) {
    ENUM* retval = malloc(sizeof(ENUM));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

static IDTYPE* fcid(IDTYPE* idt) {
  IDTYPE* idr = malloc(sizeof(IDTYPE));
  memcpy(idr, idt, sizeof(IDTYPE));
  idr->pointerstack = daclone(idt->pointerstack);
  return idr;
}

EXPRESSION* cloneexpr(EXPRESSION* orig) {
  EXPRESSION* clone = malloc(sizeof(EXPRESSION));
  memcpy(clone, orig, sizeof(EXPRESSION));
  return clone;
}

EXPRESSION* ct_nop_expr(void) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = NOP;
  retval->rettype = NULL;
  return retval;
}

EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION* param) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->rettype = NULL;
  retval->params = dactor(1);
  dapush(retval->params, param);
  return retval;
}

EXPRESSION* ct_sztype(IDTYPE* whichtype) {
  if(!(whichtype->tb & (STRUCTVAL | ENUMVAL | UNIONVAL)))
    return ct_intconst_expr(whichtype->tb & 0xf);
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = SZOF;
  retval->rettype = NULL;//TODO: prepopulate here
  retval->vartype = whichtype;
  return retval;
}

EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->rettype = NULL;
  retval->params = dactor(2);
  dapush(retval->params, param1);
  dapush(retval->params, param2);
  return retval;
}

EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = CAST;
  retval->rettype = NULL;
  retval->params = dactor(1);
  dapush(retval->params, expr);
  retval->vartype = type;
  return retval;
}

EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = TERNARY;
  retval->rettype = NULL;
  retval->params = dactor(3);
  dapush(retval->params, param1);
  dapush(retval->params, param2);
  dapush(retval->params, param3);
  return retval;
}

EXPRESSION* ct_fcall_expr(EXPRESSION* func, DYNARR* params) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FCALL;
  assert(func->type & IDENT);
  DYNARR* ptrs = func->id->type->pointerstack;
  assert(ptrs);

  IDTYPE* retid = fcid(func->id->type);
  if(((struct declarator_part*) dapeek(ptrs))->type == POINTERSPEC) {
    if(((struct declarator_part*) daget(ptrs, ptrs->length - 2))->type == PARAMSSPEC ||
    ((struct declarator_part*) daget(ptrs, ptrs->length - 2))->type == NAMELESS_PARAMSSPEC) {
      retid->pointerstack->length -= 2; //shorten but no pop needed
    } else {
      assert(0);
    }
  } else {
    dapop(retid->pointerstack);
    //clone pointer stack, remove function type from it
  }
  retval->rettype = retid;
  DYNARR* dd = dactor(1);
  dapush(dd, func);
  retval->params = damerge(dd, params);
  return retval;
}

EXPRESSION* ct_strconst_expr(const char* str) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->rettype = malloc(sizeof(IDTYPE));
  retval->rettype->pointerstack = dactor(1);
  dapush(retval->rettype->pointerstack, mkdeclpart(POINTERSPEC, 0));
  retval->rettype->tb = 1 | UNSIGNEDNUM;
  retval->strconst = (char*)(unsigned long) str;
  return retval;
}

EXPRESSION* ct_intconst_expr(long num) { 
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->rettype = malloc(sizeof(IDTYPE));
  retval->rettype->pointerstack = NULL;
  retval->rettype->tb = 8;
  retval->intconst = num;
  return retval;
}

EXPRESSION* ct_uintconst_expr(unsigned long num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->rettype = malloc(sizeof(IDTYPE));
  retval->rettype->pointerstack = NULL;
  retval->rettype->tb = 8 | UNSIGNEDNUM;
  retval->uintconst = num;
  return retval;
}

EXPRESSION* ct_floatconst_expr(double num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = FLOAT;
  retval->rettype = malloc(sizeof(IDTYPE));
  retval->rettype->pointerstack = NULL;
  retval->rettype->tb = 8 | FLOATNUM;
  retval->floatconst = num;
  return retval;
}

EXPRESSION* ct_array_lit(DYNARR* da) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = ARRAY_LIT;
  retval->rettype = NULL;//TODO: prepopulate rettype
  retval->dynvals = da;
  return retval;
}

EXPRESSION* ct_member_expr(char* member) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = MEMBER;
  retval->rettype = NULL;
  retval->member = member;
  return retval;
}

EXPRESSION* ct_ident_expr(struct lexctx* lct, char* ident) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = IDENT;
  retval->id = scopesearch(lct, M_VARIABLE, ident);
  assert(retval->id || ! fprintf(stderr, "Error: use of undefined variable %s at %s %d.%d-%d.%d\n", ident, locprint(yylloc)));
  retval->rettype = retval->id->type;
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

static void fpdecl(DECLARATION* dc) {
  if(!dc) return;
  freetype(dc->type);
  free(dc); //dc->varname should be freed in dadtor
}

void freetype(IDTYPE* id) {
  if(id->pointerstack)
    for(int i = 0; i < id->pointerstack->length; i++) {
      struct declarator_part* dclp = id->pointerstack->arr[i];
      switch(dclp->type) {
        case PARAMSSPEC:
          paraledtorcfr(dclp->params, (void (*)(void*)) fpdecl);
          break;
        case NAMELESS_PARAMSSPEC:
          dadtorcfr(dclp->nameless_params, (void (*)(void*)) freetype);
          break;
        case POINTERSPEC:
          break;
        case ARRAYSPEC:
          rfreexpr(dclp->arrspec);
          break;
        case BITFIELDSPEC:
          rfreexpr(dclp->bfspec);
          assert(0); //TODO: handle bitfields
      }
    }
  dadtor(id->pointerstack);
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

void freeinit(INITIALIZER* i) {
  if(i->expr) {
    rfreexpr(i->expr);
  }
  free(i->decl->varname);
  freetype(i->decl->type);
  free(i->decl);
  free(i);
}

void rfreestate(STATEMENT* s) {
  switch(s->type) {
    case LBREAK: case LCONT: case DEFAULT: case NOPSTMT:
      //We don't reduce case statement here
      break;
    case CASE: //maybe this needs to be freed from the labeltable
    case JGOTO:  case LABEL:
      free(s->glabel);
      break;
    case SWITCH: ;
      DYNARR* da = htpairs(s->labeltable->ht);
      for(int i = 0; i < da->length; i++) {
        HASHPAIR* hp = daget(da, i);
        EXPRESSION* ex = hp->value;
        rfreexpr(ex);
      }
      paraledtor(s->labeltable);
      dadtor(da);
      //fall through
    case WHILEL: case DOWHILEL:
      rfreestate(s->body);
      rfreexpr(s->cond);
      break;
    case FORL:
      rfreestate(s->forbody);
      rfreexpr(s->forcond);
      rfreexpr(s->increment);
      if(s->forinit->isE) {
        rfreexpr(s->forinit->E);
      } else {
        dadtorcfr(s->forinit->I, (void (*)(void*)) freeinit);
      }
      free(s->forinit);
      break;
    case CMPND:
      for(int i = 0; i < s->stmtsandinits->length; i++) {
        SOI* soi = daget(s->stmtsandinits, i);
        if(soi->isstmt) {
          rfreestate(soi->state);
        } else {
          for(int j = 0; j < soi->init->length; j++) {
            INITIALIZER* in = daget(soi->init, j);
            if(in->expr) {
              rfreexpr(in->expr);
            }
            free(in->decl->type);
            free(in->decl->varname);
            free(in->decl);
            free(in);
          }
          dadtor(soi->init);
        }
        free(soi);
      }
      dadtor(s->stmtsandinits);
      break;
    case FRET:
    case EXPR: 
      rfreexpr(s->expression);
      break;
    case IFELSES:
      rfreestate(s->elsecond);
      //fall through
    case IFS:
      rfreestate(s->thencond);
      rfreexpr(s->ifcond);
      break;
  }
  free(s);
}

void rfreefunc(FUNC* f) {
  if(!f) return;
  free(f->name);
  rfreestate(f->body);
  //paraledtorcfr(f->params, (void(*)(void*)) fpdecl);
  freetype(f->retrn);
  htdtorfr(f->lbls);
  dadtor(f->switchstack);
  free(f);
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

STATEMENT* mknopstmt(void) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = NOPSTMT;
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
  retval->forinit = e1;
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

STATEMENT* mkswitchstmt(EXPRESSION* contingent, STATEMENT* bdy, SWITCHINFO* swi) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = SWITCH;
  retval->cond = contingent;
  retval->body = bdy;
  retval->labeltable = swi->cases;
  retval->defaultlbl = swi->defaultval;
  return retval;
}

STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->ifcond = condition;
  retval->thencond = ifbdy;
  if(elsebdy) {
    retval->elsecond = elsebdy;
    retval->type = IFELSES;
  } else {
    retval->type = IFS;
  }
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
  PARALLEL* pl = ((SWITCHINFO*) dapeek(lct->func->switchstack))->cases;
  while(foldconst(&casexpr)) ;
  switch(casexpr->type) {
    case INT: case UINT:
      pfinsert(pl, casexpr->uintconst, label);
      break;
    default:
      fprintf(stderr, "Error: case has nonrectifiable value\n");
      assert(0);
  }
  return mklblstmt(lct, label);
}

STATEMENT* mkdefaultstmt(struct lexctx* lct, char* label) {
  ((SWITCHINFO*) dapeek(lct->func->switchstack))->defaultval = label;
  return mklblstmt(lct, label);
}

ENUMFIELD* genenumfield(char* name, EXPRESSION* value) {
  ENUMFIELD* retval = malloc(sizeof(ENUMFIELD));
  retval->name = name;
  while(foldconst(&value)) ;
  switch(value->type) {
    case INT: case UINT:
      break;
    default:
      fprintf(stderr,"Error: enum has nonrectifiable value\n");
      assert(0);
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
  if(body) {
    int blen = strlen(body);
    md->text = strctor(strdup(body), blen, blen);
  }
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
  lct->argpp = dactor(16);
  lct->enstruct2free = dactor(1024);
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
  declmacro(lct->defines, "PTRDIFF_MAX", "(9223372036854775807L)");
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
  if(!cleanup->truescope) {
    htdtorfr(cleanup->fakescope);
  } else {
    htdtorfr(cleanup->typesdef);//SCOPEMEMBER argument
    htdtorfr(cleanup->members);//SCOPEMEMBER argument
    htdtorfr(cleanup->structs);
    htdtorfr(cleanup->enums);
    htdtorfr(cleanup->unions);
    htdtorfr(cleanup->forwardstructs);
    htdtorfr(cleanup->forwardunions);
  } //TODO: free all members???
  free(cleanup);
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

void freemd(struct macrodef* mds) {
  if(mds->text) strdtor(mds->text);
  if(mds->args) dadtorfr(mds->args);
  free(mds);
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

void feedstruct(STRUCT* s) {
  switch(s->size) {
    case 0:
      if(s->offsets)
        return;
      s->offsets = htctor();
      s->size = -1;
      DYNARR* mm = s->fields;
      long totalsize = 0;
      for(int i = 0; i < mm->length; i++) {
        DECLARATION* mmi = daget(mm, i);
        int esize;
        //TODO: handle bitfield
        if(mmi->type->pointerstack && mmi->type->pointerstack->length) {
          esize = 8;
        } else {
          TYPEBITS mtb = mmi->type->tb;
          if(mtb & (STRUCTVAL | UNIONVAL)) {
            mtb & STRUCTVAL ? feedstruct(mmi->type->structtype) : unionlen(mmi->type->uniontype);
            if(mmi->type->tb & ANONMEMB) {
              DYNARR* anonf = mmi->type->structtype->fields;
              for(int j = 0; j < anonf->length; j++) {
                DECLARATION* mmi2 = daget(anonf, j);
                STRUCTFIELD* sf = search(mmi->type->structtype->offsets, mmi2->varname);
                STRUCTFIELD* sf2 = malloc(sizeof(STRUCTFIELD));
                *sf2 = *sf;
                sf2->offset += totalsize;
                insert(s->offsets, mmi2->varname, sf2);
              }
            }
            esize = mmi->type->structtype->size;
          } else {
            //TODO: unique enum case?
            esize = mtb & 0x7f;
          }
        }
        int padding = esize > 8 ? 8 : esize;
        totalsize = (totalsize + padding - 1) & ~(padding - 1);
        if(mmi->varname) {
          STRUCTFIELD* sf = malloc(sizeof(STRUCTFIELD));
          sf->type = mmi->type;
          sf->offset = totalsize;
          insert(s->offsets, mmi->varname, sf);
        } //ignore anonymous members
        totalsize += esize;
      }
      s->size = totalsize;
      return;
    case -1:
      //circular structs!!!!!
      assert(0);
    default:
      //struct already fed
      return;
  }
}

int unionlen(UNION* u) {
  switch(u->size) {
    case 0:
      u->hfields = htctor();
      u->size = -1;
      DYNARR* mm = u->fields;
      for(int i = 0; i < mm->length; i++) {
        DECLARATION* mmi = daget(mm, i);
        int esize;
        if(mmi->type->pointerstack && mmi->type->pointerstack->length) {
          esize = 8;
        } else {
          TYPEBITS mtb = mmi->type->tb;
          if(mtb & (STRUCTVAL | UNIONVAL)) {
            mtb & STRUCTVAL ? feedstruct(mmi->type->structtype) : unionlen(mmi->type->uniontype);
            if(mmi->type->tb & ANONMEMB) {
              DYNARR* anonf = mmi->type->structtype->fields;
              for(int j = 0; j < anonf->length; j++) {
                DECLARATION* mmi2 = daget(anonf, j);
                STRUCTFIELD* sf = search(mmi->type->structtype->offsets, mmi2->varname);
                STRUCTFIELD* sf2 = malloc(sizeof(STRUCTFIELD));
                *sf2 = *sf;
                insert(u->hfields, mmi2->varname, sf2);
              }
            }
            esize = mmi->type->structtype->size;
          } else {
            //TODO: unique enum case?
            esize = mtb & 0x7f;
          }
        }
        if(esize > u->size)
          u->size = esize;
        if(mmi->varname) {
          STRUCTFIELD* sf = malloc(sizeof(STRUCTFIELD));
          sf->type = mmi->type;
          sf->offset = 0;
          insert(u->hfields, mmi->varname, sf);
        } //ignore anonymous members
      }
      break;
    case -1:
      //circular union!!!!!
      //TODO: error out
    default:
      break;
  }
  return u->size;
}
