#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "dynarr.h"
#include "compintern.h"
#include "treeduce.h"
#define EPARAM(EVA, IND) ((EXPRESSION*)((EVA)->params->arr[IND]))
#define LPARAM(EVA, IND) ((EVA)->params->arr[IND])

char puritree(EXPRESSION* cexpr) {
  switch(cexpr->type){
    case STRING: case INT: case UINT: case FLOAT: case NOP: case IDENT: case ARRAY_LIT: case SZOF: case MEMBER:
    case NEG: case L_NOT: case B_NOT: case ADDR: case DEREF:
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE: case MULT: case DIVI: 
    case MOD: case L_AND: case L_OR: case B_AND: case B_OR: case B_XOR: case SHL: case SHR: case COMMA:
    case DOTOP: case ARROW:
    case SZOFEXPR: case CAST: 
    case TERNARY:
      for(int i = 0; i < cexpr->params->length; i++) {
        if(!puritree(EPARAM(cexpr, i)))
          return 0;
      }
      return 1;
    case FCALL:
      return 0;//check function for purity
    case ASSIGN: case PREINC: case PREDEC: case POSTINC: case POSTDEC:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      return 0;
  }
  fprintf(stderr, "Error: determining purity of expression failed\n");
  return 0;
}

char purestmt(STATEMENT* stmt) {
  switch(stmt->type) {
    case FRET: case EXPR:
      return puritree(stmt->expression);
    case LBREAK: case LCONT: case LABEL: case CASE: case NOPSTMT: case DEFAULT:
      return 1;
    case JGOTO: case WHILEL: case DOWHILEL:
      return 0;
    case CMPND:
      for(int i = 0; i < stmt->stmtsandinits->length; i++) {
        SOI* s = stmt->stmtsandinits->arr[i];
        if(s->isstmt && !purestmt(s->state))
            return 0;
        if(!s->isstmt) {
          for(int j = 0; j < s->init->length; j++) {
            if(!puritree(((INITIALIZER*) s->init->arr[j])->expr))
              return 0;
          }
        }
      }
      return 1;
    case SWITCH:
      return puritree(stmt->cond) && purestmt(stmt->body);
    case IFELSES:
      if(!purestmt(stmt->elsecond))
        return 0;
      //fall through
    case IFS:
      return puritree(stmt->ifcond) && purestmt(stmt->thencond);
  }
  fprintf(stderr, "Error: determining purity of statement failed\n");
  return 0;
}
//confirm function call is pure
//Criteria: global var as lvalue of assign, or inc/dec
//dereferencing of lvalue in assign or in inc/dec
//arrow/dot op in lvalue
//loop or goto anywhere
//A more sophisticated version of the above is possible but I won't do that work
//The above will not balk at initializers, as those aren't considered assignment ops
//maybe confirm identifier is local and not a param?
//calling other function that is impure (or indirect function)
//lots of work will need to be done in order to ignore circular dependencies

char typequality(IDTYPE* t1, IDTYPE* t2) {
  if(t1->tb != t2->tb)
    return 0;
  if(t1->pointerstack->length != t2->pointerstack->length)
    return 0;
  //we don't really care about the specifics of each pointerstack member as
  //they all specify the same amount of indirection
  if(t1->tb & (STRUCTVAL | ENUMVAL | UNIONVAL) && t1->structtype != t2->structtype)
    return 0;
  return 1;
}


#define FREE2RET \
              subexpr->type = UINT; \
              free(rectexpr);  \
              free(ex);  \
              *exa = subexpr; \
              return 1  

#define CMPOP(OP)  do {\
      subexpr = EPARAM(ex, 0); \
      rectexpr = EPARAM(ex, 1); \
      switch(subexpr->type) { \
        case UINT: case INT:\
          switch(rectexpr->type) { \
            case UINT: case INT:\
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->uintconst); \
              FREE2RET; \
            case FLOAT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->floatconst); \
              FREE2RET; \
            default: \
              return 0; \
          } \
        case FLOAT: \
          switch(rectexpr->type) { \
            case UINT: case INT:\
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->uintconst); \
              FREE2RET; \
            case FLOAT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->floatconst); \
              FREE2RET; \
            default: \
              return 0; \
          } \
        default: \
          return 0; \
      } } while (0)

#define CMPOP2(OP)  do {\
      subexpr = EPARAM(ex, 0); \
      rectexpr = EPARAM(ex, 1); \
      switch(subexpr->type) { \
        case UINT:          \
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->uintconst); \
              break;\
            case INT: \
              subexpr->uintconst = ((signed long) subexpr->uintconst OP rectexpr->intconst); \
              break;\
            case FLOAT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->floatconst); \
              break;\
            default: \
              return 0; \
          } \
          break; \
        case INT: \
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst = (subexpr->intconst OP (signed long) rectexpr->uintconst); \
              break;\
            case INT: \
              subexpr->uintconst = (subexpr->intconst OP rectexpr->intconst); \
              break;\
            case FLOAT: \
              subexpr->uintconst = (subexpr->intconst OP rectexpr->floatconst); \
              break;\
            default: \
              return 0; \
          } \
          break; \
        case FLOAT:\
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->uintconst); \
              break;\
            case INT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->intconst); \
              break;\
            case FLOAT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->floatconst); \
              break;\
            default: \
              return 0; \
          } \
          break; \
        default: \
          return 0; \
      }\
      FREE2RET; \
    } while (0)

#define INTOP(OP) do {\
      subexpr = EPARAM(ex, 0); \
      rectexpr = EPARAM(ex, 1); \
      switch(subexpr->type) { \
        case UINT: \
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst OP rectexpr->uintconst; \
              break; \
            case INT: \
              subexpr->intconst OP rectexpr->uintconst; \
              break; \
            default: \
              return 0; \
          } \
          free(rectexpr); \
          free(ex); \
          *exa = subexpr; \
          return 1; \
        case INT: \
          switch(rectexpr->type) { \
            case UINT: \
              rectexpr->type = INT; \
              subexpr->intconst OP rectexpr->intconst; \
              break; \
            case INT: \
              subexpr->intconst OP rectexpr->intconst; \
              break; \
            default: \
              return 0; \
          } \
          free(rectexpr); \
          free(ex); \
          *exa = subexpr; \
          return 1; \
        default: \
          return 0; \
      } } while (0)





//check 2 trees for equality
char treequals(EXPRESSION* e1, EXPRESSION* e2) {
  if(e1->type != e2->type)
    return 0;
  switch(e1->type) {
    case IDENT: 
      return (e1->id->index != -1) && (e1->id->index == e2->id->index);
    case INT: 
      return e1->intconst == e2->intconst;
    case UINT: 
      return e1->uintconst == e2->uintconst;
    case FLOAT: 
      return e1->floatconst == e2->floatconst;
    case STRING: 
      return !strcmp(e1->strconst, e2->strconst);
    case MEMBER: 
      return !strcmp(e1->member, e2->member);
    case NOP:
      return 1;
    case ARRAY_LIT:
      return 0;
    case SZOF:
      return typequality(e1->vartype, e2->vartype);
    case CAST:
      if(!typequality(e1->vartype, e2->vartype))
        return 0; //else fallthrough
    case NEG: case L_NOT: case B_NOT: case ADDR: case DEREF:
    case TERNARY:
    case FCALL:
    case SZOFEXPR:
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE: case MULT: case DIVI: 
    case MOD: case L_AND: case L_OR: case B_AND: case B_OR: case B_XOR: case SHL: case SHR: case COMMA:
    case ASSIGN: case PREINC: case PREDEC: case POSTINC: case POSTDEC: case DOTOP: case ARROW:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      if(e1->params->length != e2->params->length)//big problem here
        return 0;
      for(int i = 0; i < e1->params->length; i++) {
        if(!treequals(EPARAM(e1, i), EPARAM(e2, i)))
          return 0;
      }
      return 1;
  }
  fprintf(stderr, "Error: determining equality of 2 expressions failed\n");
  return 0;
}

char foldconst(EXPRESSION** exa) {
  EXPRESSION* ex = *exa;
  EXPRESSION* subexpr;
  DYNARR* newdyn;
  EXPRESSION* rectexpr;
//  EXPRTYPE eventualtype;
  char rove;

  //call on each param before the switch
  switch(ex->type) {
    case UINT: case INT: case FLOAT: case ARRAY_LIT: case STRING: case SZOF: case NOP: case IDENT:
      break;
    default:
      for(int i = 0; i < ex->params->length; i++) {
        while(foldconst((EXPRESSION**) &LPARAM(ex, i))) ;
      }
      break;
  }
  switch(ex->type) {
    case IDENT: case INT: case UINT: case FLOAT: case STRING: case NOP: case ARRAY_LIT: case DOTOP: case ARROW:
      return 0;
    case CAST:
    case MEMBER: 
      //get addr for deref, as struct should be fully populated at this point
      return 0;
    case SZOF:
      //turn into intconst
      return 0;
    case NEG:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case NEG:
          //free expr and subexpr
          subexpr = EPARAM(subexpr, 0);
          foldconst((EXPRESSION**) &LPARAM(subexpr, 0));
          break;
        case INT: case UINT:
          //free expr
          subexpr->intconst = -subexpr->intconst;
          subexpr->type = INT;
          break;
        case FLOAT:
          //free expr
          subexpr->floatconst = -subexpr->floatconst;
          break;
        case COMMA:
          //look at end of expr
        default:
          return 0;
      }
      *exa = subexpr;
      return 1;
    case L_NOT: 
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case L_NOT:
          subexpr = EPARAM(subexpr, 0);
          break;
        case EQ:
          subexpr->type = NEQ;
          break;
        case NEQ:
          subexpr->type = EQ;
          break;
        case GT:
          subexpr->type = LTE;
          break;
        case LT:
          subexpr->type = GTE;
          break;
        case LTE:
          subexpr->type = GT;
          break;
        case GTE:
          subexpr->type = LT;
          break;
        case INT: case UINT:
          subexpr->intconst = !subexpr->intconst;
          break;
        //we don't really deal with float
        case COMMA:
          //look at end of expr
        default:
          return 0;
      }
      free(ex);
      *exa = subexpr;
      return 1;
    case B_NOT: 
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case B_NOT:
          rectexpr = subexpr;
          subexpr = EPARAM(subexpr, 0);
          free(ex);
          free(rectexpr);
          *exa = subexpr;
          return 1;
        case COMMA:
          //look at end of expr
        default:
          return 0;
      }
    case ADDR:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case DEREF:
          rectexpr = subexpr;
          subexpr = EPARAM(subexpr, 0);
          free(ex);
          free(rectexpr);
          *exa = subexpr;
          return 1;
        case COMMA:
          //look at end of expr
        default:
          return 0;
      }
    case DEREF:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case ADDR:
          rectexpr = subexpr;
          subexpr = EPARAM(subexpr, 0);
          free(ex);
          free(rectexpr);
          *exa = subexpr;
          return 1;
        case COMMA:
          //look at end of expr
        default:
          return 0;
      }
    case ADD:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
      rove = 0;
      //handle type? (likely unnecessary)
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case ADD:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          case COMMA:
            //look at end of expr
          default:
            dapush(newdyn, subexpr);
            break;
          case UINT:
            switch(rectexpr->type) {
              case UINT:
                rectexpr->uintconst += subexpr->uintconst;
                break;
              case INT:
                rectexpr->intconst += subexpr->uintconst;
                break;
              case FLOAT:
                rectexpr->floatconst += subexpr->uintconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case INT:
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = INT;
                rectexpr->intconst += subexpr->intconst;
                break;
              case INT:
                rectexpr->intconst += subexpr->intconst;
                break;
              case FLOAT:
                rectexpr->floatconst += subexpr->intconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case FLOAT:
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->uintconst;
                rectexpr->floatconst += subexpr->floatconst;
                break;
              case INT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->intconst;
                rectexpr->floatconst += subexpr->floatconst;
                break;
              case FLOAT:
                rectexpr->floatconst += subexpr->floatconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
        }
      }
      if(((rectexpr->type != UINT) ||
          rectexpr->uintconst != 0))
        dapush(newdyn, rectexpr);
      else
        free(rectexpr);
      dadtor(ex->params);
      if(newdyn->length == 1) {
        EXPRESSION* rv = newdyn->arr[0];
        dadtor(newdyn);
        free(ex);
        *exa = rv;
        return 1;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        *exa = rectexpr;
        return 1;
      }
      ex->params = newdyn;
      return rove;
    case SUB:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case SUB:
            if(!i) {
              for(int j = 0; j < subexpr->params->length; j++) {
                dapush(newdyn, EPARAM(subexpr, j)); 
              }
              dadtor(subexpr->params);
              free(subexpr);
              rove = 1;
              break;
            } //else fallthrough
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT:
            //Test if this is all correct/necessary
            if(i != 0) {
              switch(rectexpr->type) {
                case UINT:
                  rectexpr->uintconst += subexpr->uintconst;
                  break;
                case INT:
                  rectexpr->intconst += subexpr->uintconst;
                  break;
                case FLOAT:
                  rectexpr->floatconst += subexpr->uintconst;
                  break;
                default:
                  break;
            }} else {
              switch(rectexpr->type) {
                case UINT:
                  rectexpr->uintconst -= subexpr->uintconst;
                  break;
                case INT:
                  rectexpr->intconst -= subexpr->uintconst;
                  break;
                case FLOAT:
                  rectexpr->floatconst -= subexpr->uintconst;
                  break;
                default:
                  break;
              }
            }
            free(subexpr);
            rove = 1;
            break;
          case INT:
            if(i) {
              subexpr->intconst = -subexpr->intconst;
            }
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = INT;
                rectexpr->intconst += subexpr->intconst;
                break;
              case INT:
                rectexpr->intconst += subexpr->intconst;
                break;
              case FLOAT:
                rectexpr->floatconst += subexpr->intconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case FLOAT:
            if(i) {
              subexpr->floatconst = -subexpr->floatconst;
            }
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->uintconst;
                rectexpr->floatconst += subexpr->floatconst;
                break;
              case INT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->intconst;
                rectexpr->floatconst += subexpr->floatconst;
                break;
              case FLOAT:
                rectexpr->floatconst += subexpr->floatconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
        }
      }
      if(((rectexpr->type != UINT) ||
          rectexpr->uintconst != 0)) {
        dapush(newdyn, rectexpr);
      } else {
        free(rectexpr);
      }
      dadtor(ex->params);
      if(newdyn->length == 1) {
        EXPRESSION* rv = newdyn->arr[0];
        dadtor(newdyn);
        free(ex);
        *exa = rv;
        return 1;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        *exa = rectexpr;
        return 1;
      }
      return rove;
    case MULT: 
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(1);
      //TODO: destroy all pure (integer because NaN/inf) arguments when multiply by 0
      //TODO: handle type(?)
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case MULT:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr,j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          case COMMA:
            //look at end of expr
          default:
            dapush(newdyn, subexpr);
            break;
          case UINT:
            switch(rectexpr->type) {
              case UINT:
                rectexpr->uintconst *= subexpr->uintconst;
                break;
              case INT:
                rectexpr->intconst *= subexpr->uintconst;
                break;
              case FLOAT:
                rectexpr->floatconst *= subexpr->uintconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case INT:
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = INT;
                rectexpr->intconst *= subexpr->intconst;
                break;
              case INT:
                rectexpr->intconst *= subexpr->intconst;
                break;
              case FLOAT:
                rectexpr->floatconst *= subexpr->intconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case FLOAT:
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->uintconst;
                rectexpr->floatconst *= subexpr->floatconst;
                break;
              case INT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->intconst;
                rectexpr->floatconst *= subexpr->floatconst;
                break;
              case FLOAT:
                rectexpr->floatconst *= subexpr->floatconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
        }
      }
      if(((rectexpr->type != UINT) || rectexpr->uintconst != 1))
        dapush(newdyn, rectexpr);
      else
        free(rectexpr);
      dadtor(ex->params);
      if(newdyn->length == 1) {
        EXPRESSION* rv = newdyn->arr[0];
        dadtor(newdyn);
        free(ex);
        *exa = rv;
        return 1;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        *exa = rectexpr;
        return 1;
      }
      ex->params = newdyn;
      return rove;
    case DIVI:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case DIVI:
            if(!i) {
              for(int j = 0; j < subexpr->params->length; j++) {
                dapush(newdyn, EPARAM(subexpr, j)); 
              }
              dadtor(subexpr->params);
              free(subexpr);
              rove = 1;
              break;
            } //else fallthrough
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT:
            if(i == 0) {
              dapush(newdyn, subexpr);
              break;
            }
            switch(rectexpr->type) {
              case UINT:
                rectexpr->uintconst *= subexpr->uintconst;
                break;
              case INT:
                rectexpr->intconst *= subexpr->uintconst;
                break;
              case FLOAT:
                rectexpr->floatconst *= subexpr->uintconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case INT:
            if(i == 0) {
              dapush(newdyn, subexpr);
              break;
            }
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = INT;
                rectexpr->intconst *= subexpr->intconst;
                break;
              case INT:
                rectexpr->intconst *= subexpr->intconst;
                break;
              case FLOAT:
                rectexpr->floatconst *= subexpr->intconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
          case FLOAT:
            if(i == 0) {
              dapush(newdyn, subexpr);
              break;
            }
            switch(rectexpr->type) {
              case UINT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->uintconst;
                rectexpr->floatconst *= subexpr->floatconst;
                break;
              case INT:
                rectexpr->type = FLOAT;
                rectexpr->floatconst = rectexpr->intconst;
                rectexpr->floatconst *= subexpr->floatconst;
                break;
              case FLOAT:
                rectexpr->floatconst *= subexpr->floatconst;
                break;
              default:
                break;
            }
            free(subexpr);
            rove = 1;
            break;
        }
      }
      if(((rectexpr->type != UINT) ||
          rectexpr->uintconst != 0)) {
        dapush(newdyn, rectexpr);
      } else {
        free(rectexpr);
      }
      dadtor(ex->params);
      if(newdyn->length == 1) {
        EXPRESSION* rv = newdyn->arr[0];
        dadtor(newdyn);
        free(ex);
        *exa = rv;
        return 1;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        *exa = rectexpr;
        return 1;
      }
      return rove;
    case MOD: 
      INTOP(%=);
      //we don't flatten mods--that can happen in SSA and also chaining lots of modulos
      //is not a case that is realistic or one I will handle
      return 1; //If it reaches the end of this block, it's definitely changed
    case L_AND:
      newdyn = dactor(32);
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case L_AND:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          case COMMA:
            //look at end of expr
          default:
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(subexpr->uintconst == 0) {
              for(++i; i < ex->params->length; ++i) {
                rfreexpr(EPARAM(ex, i));
              }
              dadtor(ex->params);
              if(!newdyn->length) {
                free(ex);
                dadtor(newdyn);
                *exa = subexpr;
              } else {
                dapush(newdyn, subexpr);
                ex->params = newdyn;
              }
              return 1;
            } else {
              free(subexpr);
            }
            rove = 1;
            break;
        }
      }
      dadtor(ex->params);
      if(newdyn->length <= 1) {
        if(newdyn->length == 0) {
          free(ex);
          *exa = ct_intconst_expr(1);
          dadtor(newdyn);
          return 1;
        } else {
          dapush(newdyn, ct_intconst_expr(1));
        }
      }
      ex->params = newdyn;
      return rove;
    case L_OR:
      newdyn = dactor(32);
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        rove = 0;
        switch(subexpr->type) {
          case L_OR:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(subexpr->uintconst != 0) {
              for(++i; i < ex->params->length; ++i) {
                rfreexpr(EPARAM(ex, i));
              }
              dadtor(ex->params);
              if(!newdyn->length) {
                free(ex);
                dadtor(newdyn);
                *exa = subexpr;
              }
              else {
                dapush(newdyn, subexpr);
                ex->params = newdyn;
              }
              return 1;
            } else {
              free(subexpr);
            }
            rove = 1;
            break;
        }
      }
      dadtor(ex->params);
      if(newdyn->length <= 1) {
        if(newdyn->length == 0) {
          free(ex);
          *exa = ct_intconst_expr(1);
          dadtor(newdyn);
          return 1;
        } else {
          dapush(newdyn, ct_intconst_expr(1));
        }
      }
      ex->params = newdyn;
      return rove;
    case B_AND: 
      newdyn = dactor(32);
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        rectexpr = ct_uintconst_expr(-1UL);
        switch(subexpr->type) {
          case B_AND:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          case COMMA:
            //look at end of expr
          default:
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(rectexpr->uintconst != -1UL)
              rove = 1;
            rectexpr->uintconst &= subexpr->uintconst;
            free(subexpr);
            if(rectexpr->uintconst == 0) {
              for(++i; i < ex->params->length; ++i) {
                EXPRESSION* free2 = EPARAM(ex, i);
                if(puritree(free2)) {
                  rfreexpr(free2);
                } else {
                  dapush(newdyn, free2);
                  //perhaps reduce into comma?
                }
              }
            }
            break;
        }
      }
      dadtor(ex->params);
      if(newdyn->length == 0) {
        free(ex);
        dadtor(newdyn);
        *exa = rectexpr;
        return 1;
      }
      if(rectexpr->uintconst != -1UL) {
        dapush(newdyn, rectexpr);
      } else {
        free(rectexpr);
      }

      ex->params = newdyn;
      return rove;
      //flatten, make const, if const is 0, eliminate impure
    case B_OR:
      newdyn = dactor(32);
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        rectexpr = ct_uintconst_expr(0);
        switch(subexpr->type) {
          case B_OR:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          case COMMA:
            //look at end of expr
          default:
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(rectexpr->uintconst != 0)
              rove = 1;
            rectexpr->uintconst |= subexpr->uintconst;
            free(subexpr);
            if(rectexpr->uintconst == -1UL) {
              for(++i; i < ex->params->length; ++i) {
                EXPRESSION* free2 = EPARAM(ex, i);
                if(puritree(free2)) {
                  rfreexpr(free2);
                } else {
                  dapush(newdyn, free2);
                  //perhaps reduce into comma?
                }
              }
            }
            break;
        }
      }
      dadtor(ex->params);
      if(newdyn->length == 0) {
        free(ex);
        dadtor(newdyn);
        *exa = rectexpr;
        return 1;
      }
      if(rectexpr->uintconst != 0) {
        dapush(newdyn, rectexpr);
      } else {
        free(rectexpr);
      }

      ex->params = newdyn;
      return rove;

    case B_XOR:
      newdyn = dactor(32);
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        rectexpr = ct_uintconst_expr(0);
        rove = 0;
        switch(subexpr->type) {
          case B_XOR:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            rove = 1;
            break;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(rectexpr->intconst != 0)
              return rove;
            rectexpr->uintconst ^= subexpr->uintconst;
            free(subexpr);
            break;
        }
      }
      dadtor(ex->params);
      if(newdyn->length == 0) {
        free(ex);
        dadtor(newdyn);
        *exa = rectexpr;
        return 1;
      }
      if(rectexpr->uintconst != 0) {
        dapush(newdyn, rectexpr);
      } else {
        free(rectexpr);
      }

      ex->params = newdyn;
      return rove;
    case SHL://maybe check if right side is within bounds of type size
      INTOP(<<=);
      return 1;
    case SHR:
      INTOP(>>=);
      return 1;

    case COMMA:
      newdyn = dactor(32);
      rove = 0;
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        if(i != ex->params->length - 1) {
          if(puritree(subexpr)) {
            rfreexpr(subexpr); 
            rove = 1;
            continue;
          }
        }
        switch(subexpr->type) {
          case COMMA:
            for(int j = 0; j < subexpr->params->length; j++) {
              EXPRESSION* fse = EPARAM(subexpr, j);
              if(j != subexpr->params->length - 1) {
                if(puritree(fse)) {
                  rfreexpr(fse);
                  rove = 1;
                  continue;
                }
              }
              dapush(newdyn, fse); 
            }
            rove = 1;
            break;
          default:
            dapush(newdyn, subexpr);
            break;
        }
      }
      dadtor(ex->params);
      ex->params = newdyn;
      return rove;
      //adopt/flatten in subexprs, remove pure exprs except the last one
      //if impure call to pure function, extract out impure params, and eval those only????
    case EQ: //should be kept binary
      CMPOP(==);
      return 1;
    case NEQ:
      CMPOP(!=);
      return 1;
    case GT: 
      CMPOP2(>);
      return 1;
    case LT:
      CMPOP2(<);
      return 1;
    case GTE: 
      CMPOP2(>=);
      return 1;
    case LTE: 
      CMPOP2(<=);
      return 1;
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case XORASSIGN: case ORASSIGN:  //0 is identity case 
    case ANDASSIGN: case MODASSIGN: //no identity case (for our purposes)
    case DIVASSIGN: case MULTASSIGN: 
      //if identity on right side of expression (mostly 0, sometimes 1, none in case of mod (and for our purposes AND), etc) and expression is pure, ellide
      return 0;
    case ASSIGN:
      if(puritree(EPARAM(ex, 0)) && treequals(EPARAM(ex, 0), EPARAM(ex, 1))) {
        rfreexpr(EPARAM(ex, 0));
        *exa = EPARAM(ex, 1);
        dadtor(ex->params);
        free(ex);
        return 1;
      }
      return 0;
      //Further fix assign op
    case PREINC: case PREDEC: case POSTINC: case POSTDEC:
      return 0;
    case TERNARY:
      subexpr = EPARAM(ex, 0);
      rove = 0;
      switch(subexpr->type) {
        case INT: case UINT: case FLOAT:
          if(subexpr->uintconst == 0) {
            rfreexpr(EPARAM(ex, 1)); 
            free(subexpr);
            *exa = EPARAM(ex, 2);
            return 1;
          } else {
            rfreexpr(EPARAM(ex, 2)); 
            free(subexpr);
            *exa = EPARAM(ex, 1);
            return 1;
          }
          rove = 1;
          break;
        case COMMA:
          rectexpr = dapeek(subexpr->params);
          switch(rectexpr->type) {
            case INT: case UINT: case FLOAT:
              if(subexpr->uintconst == 0) {
                rfreexpr(EPARAM(ex, 1)); 
                free(subexpr);
                dapush(subexpr->params, EPARAM(ex, 2));
              } else {
                rfreexpr(EPARAM(ex, 2)); 
                free(subexpr);
                dapush(subexpr->params, EPARAM(ex, 1));
              }
              free(ex);
              *exa = subexpr;
              return 1;
            default:
              break;
          }
          default:
            break;
      }
      return rove;
    case FCALL:
    case SZOFEXPR:
      return 0;
  }
  fprintf(stderr, "Error: reducing expression failed\n");
  return 0;
}

//do the same as above but with statements
char pleatstate(STATEMENT** stated) {
  STATEMENT* st = *stated;
  DYNARR* newsdyn;
  int i = 0;
  switch(st->type) {
    case LBREAK: case JGOTO: case LCONT: case LABEL: case CASE: case DEFAULT: case NOPSTMT:
      //We don't reduce case statement here
      return 0;
    case SWITCH: case DOWHILEL:
      while(foldconst(&st->cond)) i = 1;
      return i || pleatstate(&st->body);
    case WHILEL:
      //for while and maybe do while do something different if cond evaluates to false
      while(foldconst(&st->cond)) i = 1;
      switch(st->cond->type) {
        case INT: case UINT: ;
          if(st->cond->uintconst) {
            //plain infinite loop, maybe optimize?
          } else {
            rfreestate(st);
            *stated = mknopstmt();
            return 1;
          }
          break;
        default:
          break;
      }
      return i || pleatstate(&st->body);
    case CMPND:
      newsdyn = dactor(st->stmtsandinits->length);
      for(int i = 0; i < st->stmtsandinits->length; i++) {
        CMPNDLCONT: ;
        SOI* soi = daget(st->stmtsandinits, i);
        if(soi->isstmt) {
          if((soi->state->type == FRET || soi->state->type == JGOTO) && i != st->stmtsandinits->length - 1) {
            ++i;
            for(; i < st->stmtsandinits->length; i++) {
              rfreestate(st);
              SOI* free2 = daget(st->stmtsandinits, i);
              if(soi->isstmt) {
                if(soi->state->type == LABEL || soi->state->type == CASE || soi->state->type == DEFAULT) {
                  goto CMPNDLCONT;
                }
                free(free2->state);
              } else {
                for(int j = 0; j < free2->init->length; j++) {
                  INITIALIZER* in = daget(free2->init, j);
                  if(in->expr) {
                    rfreexpr(in->expr);
                  }
                  freetype(in->decl->type);
                  free(in->decl->varname);
                  free(in->decl);
                  free(in);
                }
                dadtor(free2->init);
              }
              free(free2);
            }
            return 1;
          }
          if(pleatstate(&soi->state)) i = 1;
          if(soi->state->type == NOPSTMT) {
            i = 1;
          } else {
            dapush(newsdyn, soi);
          }
        } else {
          for(int j = 0; j < soi->init->length; j++) {
            INITIALIZER* in = daget(soi->init, j);
            while(foldconst(&in->expr)) i = 1;
          }
          dapush(newsdyn, soi);
        }
      }
      dadtor(st->stmtsandinits);
      st->stmtsandinits = newsdyn;
      return i;
    case FRET:
      while(foldconst(&st->expression)) i = 1;
      return i;
    case EXPR: 
      while(foldconst(&st->expression)) i = 1;
      if(puritree(st->expression)) {
        rfreexpr(st->expression);
        st->type = NOPSTMT;
      }
      return i;
    case IFELSES:
      while(foldconst(&st->ifcond)) i = 1;
      switch(st->ifcond->type) {
        case INT: case UINT:
          if(st->ifcond->uintconst) {
            rfreestate(st->elsecond);
            *stated = st->thencond;
          } else {
            rfreestate(st->thencond);
            *stated = st->elsecond;
          }
          free(st->ifcond);
          free(st);
          pleatstate(stated);
          return 1;
        default:
          break;
      }
      return i || pleatstate(&st->thencond) || pleatstate(&st->elsecond);
    case IFS:
      switch(st->ifcond->type) {
        case INT: case UINT:
          if(st->ifcond->uintconst) {
            *stated = st->thencond;
          } else {
            *stated = mknopstmt();
          }
          free(st->ifcond);
          free(st);
          pleatstate(stated);
          return 1;
        default:
          break;
      }
      return foldconst(&st->ifcond) || pleatstate(&st->thencond);
  }
  fprintf(stderr, "Error: reducing statement failed\n");
  return 0;
}
