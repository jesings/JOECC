#include <stdio.h>
#include <assert.h>
#include "compintern.h"
#include "treeduce.h"
#include "joecc.tab.h"

USTRUCT* ustructor(char* name, DYNARR* fields, struct lexctx* lct) {
    USTRUCT* retval = malloc(sizeof(USTRUCT));
    retval->name = name;
    retval->fields = fields;
    retval->offsets = NULL;
    retval->size = 0;
    dapush(lct->enstruct2free, retval);
    return retval;
}
ENUM* enumctor(char* name, DYNARR* fields, struct lexctx* lct) {
    ENUM* retval = malloc(sizeof(ENUM));
    retval->name = name;
    retval->fields = fields;
    dapush(lct->enumerat2free, retval);
    return retval;
}

OPERAND* genoperand(char* constraint, EXPRESSION* varin) {
  OPERAND* retval = malloc(sizeof(OPERAND));
  retval->constraint = constraint;
  retval->varin = varin;
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

IDTYPE* fcid2(IDTYPE* idt) {
  IDTYPE* idr = malloc(sizeof(IDTYPE));
  memcpy(idr, idt, sizeof(IDTYPE));
  if(idt->pointerstack) idr->pointerstack = ptrdaclone(idt->pointerstack);
  else idr->pointerstack = NULL;
  return idr;
}

DYNARR* ptrdaclone(DYNARR* opointerstack) {
  DYNARR* npointerstack = dactor(opointerstack->maxlength);
  for(int i = 0; i < opointerstack->length; i++) {
    struct declarator_part* dclp = malloc(sizeof(struct declarator_part));
    memcpy(dclp, opointerstack->arr[i], sizeof(struct declarator_part));
    switch(dclp->type) {
      case PARAMSSPEC: ;
        DYNARR* newp = dactor(dclp->params->length);
        for(int j = 0; j < dclp->params->length; j++) {
          DECLARATION* parid = daget(dclp->params, j);
          if(!parid) {
            dapush(newp, NULL);
          } else {
            DECLARATION* newdecl = malloc(sizeof(DECLARATION));
            newdecl->type = fcid2(parid->type);
            newdecl->varname = strdup(parid->varname);
            dapush(newp, newdecl);
          }
        }
        dclp->params = newp;
        break;
      case NAMELESS_PARAMSSPEC:
        if(dclp->nameless_params) {
          dclp->nameless_params = daclone(dclp->nameless_params);
          for(int j = 0; j < dclp->nameless_params->length; j++) {
            if(dclp->nameless_params->arr[j]) {
              dclp->nameless_params->arr[j] = fcid2(dclp->nameless_params->arr[j]);
            }
          }
        }
        break;
      case POINTERSPEC: case ARRAYSPEC:
        break;
      case VLASPEC:
        dclp->vlaent = rclonexpr(dclp->vlaent);
        break;
      case BITFIELDSPEC:
        assert(0); //TODO: handle bitfields
    }
    npointerstack->arr[i] = dclp;
  }
  npointerstack->length = opointerstack->length;
  return npointerstack;
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
  retval->params->arr[0] = param;
  retval->params->length = 1;
  return retval;
}

EXPRESSION* ct_sztype(IDTYPE* whichtype) {
  if(!(whichtype->tb & (STRUCTVAL | ENUMVAL | UNIONVAL))) {
    EXPRESSION* ic =  ct_intconst_expr(whichtype->tb & 0xf);
    //if(whichtype->pointerstack) dadtor(whichtype->pointerstack);
    free(whichtype);
    return ic;
  }
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = SZOF;
  retval->rettype = NULL;
  retval->vartype = whichtype;
  return retval;
}

EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->rettype = NULL;
  retval->params = dactor(2);
  retval->params->arr[0] = param1;
  retval->params->arr[1] = param2;
  retval->params->length = 2;
  return retval;
}

EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = CAST;
  retval->params = dactor(1);
  retval->params->arr[0] = expr;
  retval->params->length = 1;
  retval->rettype = retval->vartype = type;
  return retval;
}

EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = TERNARY;
  retval->rettype = NULL;
  retval->params = dactor(3);
  retval->params->arr[0] = param1;
  retval->params->arr[1] = param2;
  retval->params->arr[2] = param3;
  retval->params->length = 3;
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
    struct declarator_part* da2 = daget(ptrs, ptrs->length - 2);
    assert(da2->type == PARAMSSPEC || da2->type == NAMELESS_PARAMSSPEC);
    retid->pointerstack->length -= 2; //shorten but no pop needed
  } else {
    dapop(retid->pointerstack);
    //clone pointer stack, remove function type from it
  }
  retval->rettype = retid;
  DYNARR* dd = dactor(1 + params->length);
  dd->arr[0] = func;
  dd->length = 1;
  retval->params = damerge(dd, params);
  return retval;
}

EXPRESSION* ct_strconst_expr(const char* str) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->rettype = malloc(sizeof(IDTYPE));
  retval->rettype->pointerstack = dactor(1);
  dapushc(retval->rettype->pointerstack, mkdeclpart(POINTERSPEC, 0));
  retval->rettype->tb = 1 | UNSIGNEDNUM;
  retval->strconst = (char*)(unsigned long) str;
  return retval;
}

EXPRESSION* ct_intconst_expr(long num) { 
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->intconst = num;
  retval->rettype = NULL;
  return retval;
}

EXPRESSION* ct_uintconst_expr(unsigned long num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = UINT;
  retval->rettype = NULL;
  retval->uintconst = num;
  return retval;
}

EXPRESSION* ct_floatconst_expr(double num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FLOAT;
  retval->rettype = malloc(sizeof(IDTYPE));
  retval->rettype->pointerstack = NULL;
  retval->rettype->tb = 8 | FLOATNUM;
  retval->floatconst = num;
  return retval;
}

EXPRESSION* ct_array_lit(DYNARR* da) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = ARRAY_LIT;
  retval->rettype = NULL;
  retval->params = da;
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
  IDENTIFIERINFO* ids = scopesearch(lct, M_VARIABLE, ident);
  if(!ids) {
    if(!lct->func) {
      //error out, this may be not to spec
      assert(0);
    }
  }
  retval->id = malloc(sizeof(IDENTIFIERINFO));
  retval->id->type = ids->type;
  retval->id->name = ident;
  if(ids->type->tb & GLOBALFUNC) {
    retval->id->index = -2;
  } else {
    retval->id->index = ids->index;
  }
  assert(retval->id);
  retval->rettype = retval->id->type;
  return retval;
}

char isglobal(struct lexctx* lct, char* ident) {
  SCOPE* sc = daget(lct->scopes, 0);
  return queryval(sc->members, ident);
}

char typecompat(IDTYPE* t1, IDTYPE* t2) {
  if(ispointer(t1))
    return ispointer(t2); //array special case?
  if(ispointer(t2))
    return 0;
  if(t1->tb & (STRUCTVAL | UNIONVAL)) {
    if(!(t2->tb & (STRUCTVAL | UNIONVAL)))
      return 0;
    USTRUCT* st1 = t1->structtype;
    USTRUCT* st2 = t2->structtype;
    if(st1->fields->length != st2->fields->length)
      return 0;
    for(int i = 0; i < st1->fields->length; i++) {
      STRUCTFIELD* sf1 = daget(st1->fields, i);
      STRUCTFIELD* sf2 = daget(st2->fields, i);
      if(sf1->offset != sf2->offset)
        return 0;
      if(!typecompat(sf1->type, sf2->type))
        return 0;
    }
    return 1;
  }
  if(t2->tb & (STRUCTVAL | UNIONVAL))
    return 1;
  return !((t1->tb & FLOATNUM)^(t2->tb & FLOATNUM));
}

int process_array_lit(IDTYPE* arr_memtype, EXPRESSION* arr_expr) {
  struct declarator_part* tdclp = dapeek(arr_memtype->pointerstack);
  tdclp->arrlen = 0;
  arr_memtype->pointerstack->length -= 1;
  if(!ispointer(arr_memtype) || ((struct declarator_part*) dapeek(arr_memtype->pointerstack))->type != ARRAYSPEC) {
    int szstep;
    if(ispointer(arr_memtype))
      szstep = 0x8;//arr dim is one so must be real pointer
    else if(arr_memtype->tb & (STRUCTVAL | UNIONVAL))
      szstep = arr_memtype->structtype->size;
    else szstep =  arr_memtype->tb & 0xf;
    if(0 == arr_memtype->pointerstack->length) {
      if(arr_memtype->tb & (STRUCTVAL | UNIONVAL)) {
        int i;
        for(i = 0; i < arr_expr->params->length; i++) {
          EXPRESSION* arrv = daget(arr_expr->params, i);
          process_struct_lit(arr_memtype, arrv);
          arrv->rettype = fcid2(arr_memtype);
          tdclp->arrlen += szstep;
        }
        for(; i < tdclp->arrmaxind; i++) {
          EXPRESSION* tofill = ct_array_lit(dactor(8));
          process_struct_lit(arr_memtype, tofill);
          tofill->rettype = fcid2(arr_memtype);
          dapush(arr_expr->params, tofill);
          tdclp->arrlen += szstep;
        }
      } else {
        int i;
        for(i = 0; i < arr_expr->params->length; i++) {
          EXPRESSION* arrv = daget(arr_expr->params, i);
          IDTYPE arrt = typex(arrv);
          assert(typecompat(&arrt, arr_memtype));
          tdclp->arrlen += szstep;
        }
        for(; i < tdclp->arrmaxind; i++) {
          if(ispointer(arr_memtype)) {
            if(((struct declarator_part*) dapeek(arr_memtype->pointerstack))->type == ARRAYSPEC) {
              EXPRESSION* tofill = ct_array_lit(dactor(8));
              process_array_lit(arr_memtype, tofill);
              tofill->rettype = fcid2(arr_memtype);
              dapush(arr_expr->params, tofill);
            } else {
              dapush(arr_expr->params, ct_uintconst_expr(0));
            }
          } else if(arr_memtype->tb & FLOATNUM) {
            dapush(arr_expr->params, ct_floatconst_expr(0.0));
          } else if(arr_memtype->tb & UNSIGNEDNUM) {
            dapush(arr_expr->params, ct_uintconst_expr(0));
          } else {
            dapush(arr_expr->params, ct_intconst_expr(0));
          }
          tdclp->arrlen += szstep;
        }
        //params are fine, no further processing necessary
      }
    } else {
      int i;
      for(i = 0; i < arr_expr->params->length; i++) {
        EXPRESSION* arrv = daget(arr_expr->params, i);
        IDTYPE arrt = typex(arrv);
        assert(typecompat(&arrt, arr_memtype));
        tdclp->arrlen += szstep;
      }
      for(; i < tdclp->arrmaxind; i++) {
        dapush(arr_expr->params, ct_uintconst_expr(0));
        tdclp->arrlen += szstep;
      }
      //params are fine, no further processing necessary
    }
  } else {
    int i;
    for(i = 0; i < arr_expr->params->length; i++) {
      EXPRESSION* arrv = daget(arr_expr->params, i);
      tdclp->arrlen += process_array_lit(arr_memtype, arrv);
      arrv->rettype = fcid2(arr_memtype);
      assert(typecompat(arrv->rettype, arr_memtype));
    }
    for(; i < tdclp->arrmaxind; i++) {
      struct declarator_part* ldclp = dapeek(arr_memtype->pointerstack);
      EXPRESSION* arrv = ct_array_lit(dactor(ldclp->arrmaxind));
      tdclp->arrlen += process_array_lit(arr_memtype, arrv);
      arrv->rettype = fcid2(arr_memtype);
      assert(typecompat(arrv->rettype, arr_memtype));
    }
  }
  if(!tdclp->arrmaxind) tdclp->arrmaxind = arr_expr->params->length;
  arr_memtype->pointerstack->length += 1;
  return tdclp->arrlen;
}

int process_struct_lit(IDTYPE* struct_memtype, EXPRESSION* struct_expr) {
  struct_expr->type = STRUCT_LIT;
  USTRUCT* imptype = struct_memtype->structtype;
  feedstruct(imptype);
  if(struct_memtype->tb & UNIONVAL) {
    //union initializers only do the first item in the union (without designators)
    assert(struct_expr->params->length == 1);
  } else {
    assert(struct_expr->params->length == imptype->fields->length);
  }
  int i;
  for(i = 0; i < struct_expr->params->length; i++) {
    EXPRESSION* member = daget(struct_expr->params, i);
    DECLARATION* decl = daget(imptype->fields, i);
    if(member->type == ARRAY_LIT) {
      if(decl->type->pointerstack && decl->type->pointerstack->length && 
         ((struct declarator_part*) dapeek(decl->type->pointerstack))->type == ARRAYSPEC) {
        int arrdim = 0;
        for(int j = decl->type->pointerstack->length - 1; j >= 0; j--, arrdim++) {
          struct declarator_part* pointtop = daget(decl->type->pointerstack, j);
          if(pointtop->type != ARRAYSPEC) break;
        }
        assert(arrdim);
        process_array_lit(decl->type, member);
        member->rettype = fcid2(decl->type);
      } else if(decl->type->tb & (STRUCTVAL | UNIONVAL)) {
        process_struct_lit(decl->type, member);
        member->rettype = fcid2(decl->type);
      } else {
        assert(0);
      }
    } else {
      IDTYPE memty = typex(member);
      assert(typecompat(&memty, decl->type));
    }
  }
  for(;i < struct_memtype->structtype->fields->length; i++) {
    DECLARATION* decl = daget(imptype->fields, i);
    if(ispointer(decl->type)) {
      if(((struct declarator_part*) dapeek(decl->type->pointerstack))->type == ARRAYSPEC) {
        EXPRESSION* tofill = ct_array_lit(dactor(8));
        process_array_lit(decl->type, tofill);
        tofill->rettype = fcid2(decl->type);
        dapush(struct_expr->params, tofill);
      } else {
        dapush(struct_expr->params, ct_uintconst_expr(0));
      }
    } else if(decl->type->tb & (STRUCTVAL | UNIONVAL)) {
      EXPRESSION* tofill = ct_array_lit(dactor(8));
      process_struct_lit(decl->type, tofill);
      tofill->rettype = fcid2(decl->type);
      dapush(struct_expr->params, tofill);
    } else if(decl->type->tb & FLOATNUM) {
      dapush(struct_expr->params, ct_floatconst_expr(0.0));
    } else if(decl->type->tb & UNSIGNEDNUM) {
      dapush(struct_expr->params, ct_uintconst_expr(0));
    } else {
      dapush(struct_expr->params, ct_intconst_expr(0));
    }
  }
  return imptype->size;
}

void wipestruct(USTRUCT* strct) {
  if(strct->fields) {
    for(int i = 0; i < strct->fields->length; ++i) {
      DECLARATION* dcl = strct->fields->arr[i];
      if(dcl->varname) {
        free(dcl->varname);
      }
      if(!(ispointer(dcl->type)) && dcl->type->tb & (ANONMEMB)) {
        wipestruct(dcl->type->structtype);
      }
      freetype(dcl->type);
      free(dcl);
    }
    dadtor(strct->fields);
  }
  if(strct->offsets) htdtorfr(strct->offsets);
  if(strct->name) free(strct->name);
  free(strct);
}

void freenum(ENUM* enm) {
  if(enm->name) free(enm->name);
  for(int i = 0; i < enm->fields->length; i++) {
    ENUMFIELD* enf = daget(enm->fields, i);
    free(enf->name);
    free(enf);
  }
  dadtor(enm->fields);
  free(enm);
}

static void fpdecl(DECLARATION* dc) {
  if(!dc) return;
  freetype(dc->type);
  free(dc); //dc->varname should be freed in dadtor
}

static void fpdecl2(DECLARATION* dc) {
  if(!dc) return;
  freetype(dc->type);
  free(dc->varname);
  free(dc); //dc->varname should be freed in dadtor
}

void freetype(IDTYPE* id) {
  if(id->pointerstack) {
    for(int i = 0; i < id->pointerstack->length; i++) {
      struct declarator_part* dclp = id->pointerstack->arr[i];
      switch(dclp->type) {
        case PARAMSSPEC:
          dadtorcfr(dclp->params, (void (*)(void*)) fpdecl2);
          break;
        case NAMELESS_PARAMSSPEC:
          if(dclp->nameless_params) dadtorcfr(dclp->nameless_params, (void (*)(void*)) freetype);
          break;
        case VLASPEC:
          rfreexpr(dclp->vlaent);
          break;
        case POINTERSPEC: case ARRAYSPEC:
          break;
        case BITFIELDSPEC:
          assert(0); //TODO: handle bitfields
      }
      free(dclp);
    }
    dadtor(id->pointerstack);
    id->pointerstack = NULL;
  }
  free(id);
}

void rfreexpr(EXPRESSION* e) {
  switch(e->type) {
    case MEMBER:
      free(e->member);
      free(e);
      return;
    case NOP:
      break;
    case ARRAY_LIT: case STRUCT_LIT:
    case L_AND: case L_OR: case L_NOT:
    case EQ: case NEQ: case GT: case LT: case GTE: case LTE:
    case SZOFEXPR:
    case CAST://rettype and vartype are the same pointer
      dadtorcfr(e->params, (void(*)(void*)) rfreexpr);
      break;
    case SZOF: 
      free(e->vartype);
      break;
    case UINT: case INT: case FLOAT:
      break;
    case STRING:
      free(e->strconst);
      if(e->rettype) freetype(e->rettype);
      free(e);
      return;
    case NEG: case COMMA:
    case B_NOT: case POSTINC: case POSTDEC:
    case PREINC: case PREDEC:
    case ASSIGN: case ADDASSIGN: case SUBASSIGN:
    case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN:
    case MULTASSIGN: case MODASSIGN:
    case B_AND: case B_OR: case B_XOR:
    case ADD: case SUB: case SHR: case SHL:
    case MULT: case DIVI: case MOD: case TERNARY:
    case DOTOP: case ARROW:
      dadtorcfr(e->params, (void(*)(void*)) rfreexpr);
      if(e->rettype) free(e->rettype);
      free(e);
      return;
    case IDENT:
      free(e->id->name);
      free(e->id);
      free(e);
      return;
    case FCALL:
      dadtorcfr(e->params, (void(*)(void*)) rfreexpr);
      dadtor(e->rettype->pointerstack);//only free storage, declarator parts from global
      free(e->rettype);
      free(e);
      return;
    case ADDR:
      if(e->rettype) free(dapop(e->rettype->pointerstack));
      //fall through
    case DEREF:
      dadtorcfr(e->params, (void(*)(void*)) rfreexpr);
      if(e->rettype) {
        dadtor(e->rettype->pointerstack);
        free(e->rettype);
      }
      free(e);
      return;

  }
  if(e->rettype)
    freetype(e->rettype);
  free(e);
}

void freeinit(INITIALIZER* i) {
  if(i->expr) {
    rfreexpr(i->expr);
  }
  free(i->decl->varname);
  fpdecl(i->decl);
  free(i);
}

void rfreestate(STATEMENT* s) {
  switch(s->type) {
    case LBREAK: case LCONT: case DEFAULT: case NOPSTMT:
      //We don't reduce case statement here
      break;
    case LABEL:
    case JGOTO: 
      free(s->glabel); 
      break;
    case CASE:
      break;
    case SWITCH:
      fhtdtor(s->labeltable->ht);//labels already freed in 3ac
      dadtor(s->labeltable->da);
      free(s->labeltable);
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
      if(s->stmtsandinits) {
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
              freetype(in->decl->type);
              free(in->decl->varname);
              free(in->decl);
              free(in);
            }
            dadtor(soi->init);
          }
          free(soi);
        }
        dadtor(s->stmtsandinits);
      }
      break;
    case FRET:
      if(!s->expression) break;
      //fall through
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
    case ASMSTMT:
      free(s->asmstmts);
      if(s->inputs) {
        for(int i = 0; i < s->inputs->length; i++) {
          OPERAND* op = daget(s->inputs, i);
          free(op->constraint);
          rfreexpr(op->varin);
          free(op);
        }
        dadtor(s->inputs);
      }
      if(s->outputs) {
        for(int i = 0; i < s->outputs->length; i++) {
          OPERAND* op = daget(s->outputs, i);
          free(op->constraint);
          rfreexpr(op->varin);
          free(op);
        }
        dadtor(s->outputs);
      }
      if(s->clobbers) {
        dadtorfr(s->clobbers);
      }
      break;
  }
  free(s);
}

void rfreefunc(FUNC* f) {
  if(!f) return;
  free(f->name);
  freetype(f->retrn);
  rfreestate(f->body);
  if(f->lbls) htdtorfr(f->lbls);
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
        dapushc(e2->params, rclonexpr(e->params->arr[i]));
    case NOP:
      break;
    case SZOF:
      e2->vartype = malloc(sizeof(IDTYPE));
      memcpy(e2->vartype, e->vartype, sizeof(IDTYPE));
      if(e->vartype->pointerstack) {
        e2->vartype->pointerstack = dactor(e->vartype->pointerstack->length);
        for(int i = 0; i < e->vartype->pointerstack->length; i++) {
          struct declarator_part* dp = malloc(sizeof(struct declarator_part));
          memcpy(dp, e->vartype->pointerstack->arr[i], sizeof(struct declarator_part));
          dapushc(e2->vartype->pointerstack, dp);
        }
      }
      break;
    case CAST:
      e2->vartype = malloc(sizeof(IDTYPE));
      memcpy(e2->vartype, e->vartype, sizeof(IDTYPE));
      if(e->vartype->pointerstack) {
        e2->vartype->pointerstack = dactor(e->vartype->pointerstack->length);
        for(int i = 0; i < e->vartype->pointerstack->length; i++) {
          struct declarator_part* dp = malloc(sizeof(struct declarator_part));
          memcpy(dp, e->vartype->pointerstack->arr[i], sizeof(struct declarator_part));
          dapushc(e2->vartype->pointerstack, dp);
        }
      }
      e2->params = dactor(e->params->length);
      for(int i = 0; i < e->params->length; i++)
        dapushc(e2->params, rclonexpr(e->params->arr[i]));
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
  }
  return e2;
}

DECLARATION* mkdeclaration(char* name) {
  DECLARATION* retval = calloc(1,sizeof(DECLARATION));
  retval->varname = name;
  IDTYPE* idt = calloc(1, sizeof(IDTYPE));
  idt->pointerstack = dactor(2);
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
  if(!lct->func->lbls) lct->func->lbls = htctor();
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
      free(casexpr);
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

STATEMENT* mkasmstmt(char* asmstmts, DYNARR* outputs, DYNARR* inputs, DYNARR* clobbers) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->asmstmts = asmstmts;
  retval->outputs = outputs;
  retval->inputs = inputs;
  retval->clobbers = clobbers;
  return retval;
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

struct declarator_part* mkdeclpartarr(enum declpart_info typ, EXPRESSION* d) {
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = typ;
  foldconst(&d);
  switch(d->type) {
    case INT: case UINT:
      retval->arrmaxind = d->intconst;
      break;
    default:
      retval->type = VLASPEC;
      retval->vlaent = d;
      return retval;

  }
  rfreexpr(d);
  return retval;
}

struct declarator_part* mkdeclptr(TYPEBITS d) {
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = POINTERSPEC;
  retval->ptrspec = d;
  return retval;
}

FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn) {
  FUNC* func = malloc(sizeof(FUNC));
  func->name = name;
  func->body = body;
  func->params = params;
  func->retrn = retrn;
  func->lbls = NULL;
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

void defbackward(struct lexctx* lct, enum membertype mt, char* defnd, USTRUCT* assignval) {
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
  for(int i = 0; i < da->length; i++) {
    USTRUCT** vloc = daget(da, i);
    *vloc = assignval;
  }
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

static void declmacro(BIGHASHTABLE* ht, const char* macroname, const char* body) {
  struct macrodef* md = calloc(1, sizeof(struct macrodef));
  if(body) {
    int blen = strlen(body);
    md->text = strctor(strdup(body), blen + 1, blen + 1);
  }
  biginsert(ht, macroname, md);
}

static void declfmacro(BIGHASHTABLE* ht, const char* macroname, const char* param, const char* body) {
  struct macrodef* md = calloc(1, sizeof(struct macrodef));
  int blen = strlen(body);
  md->text = strctor(strdup(body), blen + 1, blen + 1);
  md->args = dactor(1);
  dapushc(md->args, strdup(param));
  biginsert(ht, macroname, md);
}

struct lexctx* ctxinit(void) {
  struct lexctx* lct =  malloc(sizeof(struct lexctx));
  lct->funcs = htctor();
  lct->definestack = dactor(64);
  lct->scopes = dactor(64);
  lct->func = NULL;
  dapush(lct->scopes, mkscope());
  lct->enstruct2free = dactor(512);
  lct->enumerat2free = dactor(256);
  lct->globals = dactor(512);
  lct->externglobals = dactor(128);
  lct->defines = bightctor();
  lct->withindefines = htctor();
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
  declmacro(lct->defines, "SDL_DISABLE_IMMINTRIN_H", "1");
  declfmacro(lct->defines, "__attribute__", "a", "");
  lct->ls = malloc(sizeof(struct lstate));
  lct->ls->locs = dactor(32);
  lct->ls->defargs = NULL;
  lct->ls->argpp = dactor(16);
  return lct;
}

void scopepush(struct lexctx* lct) {
  dapush(lct->scopes, mkscope());
}
void fakescopepush(struct lexctx* lct) {
  dapush(lct->scopes, mkfakescope());
}

static void freeidi(void* sidi) {
  SCOPEMEMBER* sidi2 = sidi;
  free(sidi2->idi);
  free(sidi);
}
static void freeidibidi(void* sidi) {
  SCOPEMEMBER* sidi2 = sidi;
  freetype(sidi2->typememb);
  free(sidi);
}
void scopepop(struct lexctx* lct) {
  SCOPE* cleanup = dapop(lct->scopes);
  if(cleanup->truescope && (
     cleanup->forwardstructs->keys != 0 ||
     cleanup->forwardunions->keys != 0)
     && lct->scopes->length != 0)
    fprintf(stderr, "Error: not all forward declarations processed by end of scope\n");
  if(!cleanup->truescope) {
    htdtorfr(cleanup->fakescope);
  } else {
    htdtorcfr(cleanup->typesdef, freeidibidi);//SCOPEMEMBER argument
    htdtorcfr(cleanup->members, freeidi);//SCOPEMEMBER argument
    htdtorfr(cleanup->structs);
    htdtorfr(cleanup->enums);
    htdtorfr(cleanup->unions);
    htdtorcfr(cleanup->forwardstructs, (void(*)(void*)) dadtor);
    htdtorcfr(cleanup->forwardunions, (void(*)(void*)) dadtor);
  }
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
void freemd2(struct macrodef* mds) {
  if(mds->args) dadtorfr(mds->args);
  free(mds);
}

INITIALIZER* decl2scope(DECLARATION* dec, EXPRESSION* ex, struct lexctx* lct) {
  INITIALIZER* ac = geninit(dec, ex);
  if(lct->func) {
    HASHTABLE* ht = scopepeek(lct)->members;
    SCOPEMEMBER* sm = search(ht, dec->varname);
    if(!sm || (sm->mtype == M_VARIABLE && (sm->idi->type->tb & EXTERNNUM))) {
      add2scope(lct, dec->varname, M_VARIABLE, dec->type);
      dec->varid = ((SCOPEMEMBER*) search(ht, dec->varname))->idi->index;
    } else {
      freeinit(ac);
      ac = NULL;
    }
  } else {
    IDENTIFIERINFO* id = scopesearch(lct, M_VARIABLE, dec->varname);
    if(!id) {
      id = malloc(sizeof(IDENTIFIERINFO));
      id->index = -1;
      id->name = dec->varname;
      id->type = dec->type;
      if(ispointer(id->type)) {
        struct declarator_part* dclp = dapeek(id->type->pointerstack);
        if(dclp->type == PARAMSSPEC) {
          id->type->tb |= GLOBALFUNC;
        }
      }
      add2scope(lct, dec->varname, M_GLOBAL, id);
    } //do nothing yet if redefinition
  }
  return ac;
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

void feedstruct(USTRUCT* s) {
  switch(s->size) {
    case 0:
      if(s->offsets)
        return;
      s->offsets = htctor();
      s->size = -1;
      DYNARR* mm = s->fields;
      DYNARR* newmm = dactor(mm->maxlength);
      long totalsize = 0;
      for(int i = 0; i < mm->length; i++) {
        DECLARATION* mmi = daget(mm, i);
        int esize;
        //TODO: handle bitfield
        char keepmmi = 1;
        if(ispointer(mmi->type)) {
          esize = 8;
          dapush(newmm, mmi);
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
                dapush(newmm, mmi2);
                free(sf);
              }
              esize = mmi->type->structtype->size;
              
              dadtor(mmi->type->structtype->fields);
              htdtor(mmi->type->structtype->offsets);
              free(mmi->type->structtype);
              freetype(mmi->type);
              free(mmi->varname);
              free(mmi);
              keepmmi = 0;
            } else {
              dapush(newmm, mmi);
              esize = mmi->type->structtype->size;
            }
          } else {
            esize = mtb & 0x7f;
            dapush(newmm, mmi);
          }
        }
        int padding = esize > 8 ? 8 : esize;
        totalsize = (totalsize + padding - 1) & ~(padding - 1);
        if(keepmmi) {
          STRUCTFIELD* sf = malloc(sizeof(STRUCTFIELD));
          sf->type = mmi->type;
          sf->offset = totalsize;
          insert(s->offsets, mmi->varname, sf);
        }        
        totalsize += esize;
      }
      s->size = totalsize;
      dadtor(mm);
      s->fields = newmm;
      return;
    case -1:
      //circular structs!!!!!
      assert(0);
    default:
      //struct already fed
      return;
  }
}

int unionlen(USTRUCT* u) {
  switch(u->size) {
    case 0:
      u->offsets = htctor();
      u->size = -1;
      DYNARR* mm = u->fields;
      DYNARR* newmm = dactor(mm->maxlength);
      for(int i = 0; i < mm->length; i++) {
        DECLARATION* mmi = daget(mm, i);
        int esize;
        char keepmmi = 1;
        if(ispointer(mmi->type)) {
          esize = 8;
          dapush(newmm, mmi);
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
                insert(u->offsets, mmi2->varname, sf2);
                dapush(newmm, mmi2);
                free(sf);
              }
              esize = mmi->type->structtype->size;

              htdtor(mmi->type->structtype->offsets);
              dadtor(mmi->type->structtype->fields);
              free(mmi->type->structtype);
              freetype(mmi->type);
              free(mmi->varname);
              free(mmi);
              keepmmi = 0;
            } else {
              esize = mmi->type->structtype->size;
              dapush(newmm, mmi);
            }
          } else {
            esize = mtb & 0x7f;
            dapush(newmm, mmi);
          }
        }
        if(esize > u->size)
          u->size = esize;
        if(keepmmi) {
          STRUCTFIELD* sf = malloc(sizeof(STRUCTFIELD));
          sf->type = mmi->type;
          sf->offset = 0;
          insert(u->offsets, mmi->varname, sf);
        }
      }
      dadtor(mm);
      u->fields = newmm;
      break;
    case -1:
      //circular union!!!!!
      assert(0);
    default:
      break;
  }
  return u->size;
}
