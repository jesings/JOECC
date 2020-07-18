#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "dynarr.h"
#include "compintern.h"
#define EPARAM(EVA, IND) ((EXPRESSION*)((EVA)->params->arr[IND]))
#define LPARAM(EVA, IND) ((EVA)->params->arr[IND])
//  X(NOP), X(STRING), X(INT), X(UINT), X(FLOAT), X(IDENT), X(ARRAY_LIT), 
//  X(ADD), X(NEG), X(SUB), X(EQ), X(NEQ), X(GT), X(LT), X(GTE), X(LTE), X(MULT), X(DIVI), X(MOD), 
//  X(PREINC), X(POSTINC), X(PREDEC), X(POSTDEC), 
//  X(L_AND), X(L_OR), X(L_NOT), X(B_AND), X(B_OR), X(B_XOR), X(B_NOT), X(SHL), X(SHR), 
//  X(DOTOP), X(ARROW), 
//  X(SZOF), X(SZOFEXPR), 
//  X(ASSIGN), 
//  X(ADDASSIGN), X(SUBASSIGN), X(SHLASSIGN), X(SHRASSIGN), X(ANDASSIGN),  
//  X(XORASSIGN), X(ORASSIGN), X(DIVASSIGN), X(MULTASSIGN), X(MODASSIGN), 
//  X(CAST), 
//  X(COMMA), 
//  X(ADDR), X(DEREF), 
//  X(FCALL), 
//  X(TERNARY)
char puritree(EXPRESSION* cexpr) {
  switch(cexpr->type){
    case STRING: case INT: case UINT: case FLOAT: case NOP: case IDENT: case ARRAY_LIT: case SZOF: case MEMBER:
      return 1;
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
// X(FRET), X(LBREAK), X(JGOTO), X(LCONT), 
// X(WHILEL), X(DOWHILEL), 
// X(IFS), X(IFELSES), 
// X(SWITCH), 
// X(CASE), X(LABEL), 
// X(CMPND), 
// X(EXPR), X(NOPSTMT), 
// X(DEFAULT)
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
              return subexpr;  

#define CMPOP(OP)  do {\
      subexpr = EPARAM(ex, 0); \
      rectexpr = EPARAM(ex, 1); \
      switch(subexpr->type) { \
        case UINT: case INT: case FLOAT:\
          switch(rectexpr->type) { \
            case UINT: case INT: case FLOAT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->uintconst); \
              FREE2RET; \
            default: \
              return ex; \
          } \
        default: \
          return ex; \
      } } while (0)

#define CMPOP2(OP)  do {\
      subexpr = EPARAM(ex, 0); \
      rectexpr = EPARAM(ex, 1); \
      switch(subexpr->type) { \
        case UINT:          \
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->uintconst); \
              FREE2RET;\
            case INT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->intconst); \
              FREE2RET;\
            case FLOAT: \
              subexpr->uintconst = (subexpr->uintconst OP rectexpr->floatconst); \
              FREE2RET;\
            default: \
              return ex; \
          }\
        case INT: \
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst = (subexpr->intconst OP rectexpr->uintconst); \
              FREE2RET;\
            case INT: \
              subexpr->uintconst = (subexpr->intconst OP rectexpr->intconst); \
              FREE2RET;\
            case FLOAT: \
              subexpr->uintconst = (subexpr->intconst OP rectexpr->floatconst); \
              FREE2RET;\
            default: \
              return ex; \
          } \
        case FLOAT:\
          switch(rectexpr->type) { \
            case UINT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->uintconst); \
              FREE2RET;\
            case INT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->intconst); \
              FREE2RET;\
            case FLOAT: \
              subexpr->uintconst = (subexpr->floatconst OP rectexpr->floatconst); \
              FREE2RET;\
            default: \
              return ex; \
          } \
        default: \
          return ex; \
      } } while (0)

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
              return ex; \
          } \
          free(rectexpr); \
          free(ex); \
          return subexpr; \
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
              return ex; \
          } \
          free(rectexpr); \
          free(ex); \
          return subexpr; \
        default: \
          return ex; \
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

EXPRESSION* foldconst(EXPRESSION* ex) {
  EXPRESSION* subexpr;
  DYNARR* newdyn;
  EXPRESSION* rectexpr;
//  EXPRTYPE eventualtype;

  //call on each param before the switch
  for(int i = 0; i < ex->params->length; i++) {
    ex->params->arr[i] = foldconst(EPARAM(ex, i));
  }
  switch(ex->type) {
    case IDENT: case INT: case UINT: case FLOAT: case STRING: case NOP: case ARRAY_LIT: case CAST: case DOTOP: case ARROW:
      return ex;
    case MEMBER: 
      //get addr for deref, as struct should be fully populated at this point
      return ex;
    case SZOF:
      //turn into intconst
      return ex;
    case NEG:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case NEG:
          //free expr and subexpr
          subexpr = EPARAM(subexpr, 0);
          LPARAM(subexpr, 0) = foldconst(EPARAM(subexpr, 0));
          return subexpr;
        case INT: case UINT:
          //free expr
          subexpr->intconst = -subexpr->intconst;
          subexpr->type = INT;
          return subexpr;
        case FLOAT:
          //free expr
          subexpr->floatconst = -subexpr->floatconst;
          return subexpr;
        case COMMA:
          //look at end of expr
        default:
          return ex;
      }
    case L_NOT: 
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case L_NOT:
          //free expr and subexpr
          subexpr = EPARAM(subexpr, 0);
          return subexpr;
        case NEG: //neg doesn't change the result of logical nots
          //free expr
          LPARAM(ex, 0) = EPARAM(subexpr, 0);
          return ex;
        case EQ:
          //free expr
          subexpr->type = NEQ;
          return subexpr;
        case NEQ:
          //free expr
          subexpr->type = EQ;
          return subexpr;
        case GT:
          //free expr
          subexpr->type = LTE;
          return subexpr;
        case LT:
          //free expr
          subexpr->type = GTE;
          return subexpr;
        case LTE:
          //free expr
          subexpr->type = GT;
          return subexpr;
        case GTE:
          //free expr
          subexpr->type = LT;
          return subexpr;
        case INT: case UINT: case FLOAT: ;//float is forcibly cast
          //free expr
          subexpr = EPARAM(ex, 0);
          subexpr->intconst = subexpr->intconst == 0;
          return subexpr;
        case COMMA:
          //look at end of expr
        default:
          return ex;
      }
    case B_NOT: 
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case B_NOT:
          rectexpr = subexpr;
          subexpr = EPARAM(subexpr, 0);
          free(ex);
          free(rectexpr);
          return subexpr;
        case COMMA:
          //look at end of expr
        default:
          return ex;
      }
    case ADDR:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case DEREF:
          rectexpr = subexpr;
          subexpr = EPARAM(subexpr, 0);
          free(ex);
          free(rectexpr);
          return subexpr;
        case COMMA:
          //look at end of expr
        default:
          return ex;
      }
    case DEREF:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case ADDR:
          rectexpr = subexpr;
          subexpr = EPARAM(subexpr, 0);
          free(ex);
          free(rectexpr);
          return subexpr;
        case COMMA:
          //look at end of expr
        default:
          return ex;
      }
    case ADD:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
      //handle type?
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case ADD:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
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
        return rv;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        return ct_nop_expr();
      }
      ex->params = newdyn;
      return ex;
    case SUB:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
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
            break;
          case INT:
            if(i == 0) {
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
            break;
          case FLOAT:
            if(i == 0) {
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
        return rv;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        return ct_nop_expr();
      }
      //if first element is 0, transform into ADD if more than one other arg, wrap in NEG
      //if one remaining param, remove SUB wrapper
      return ex;
    case MULT: 
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(1);
      //TODO: destroy all pure (integer because NaN/inf) arguments when multiply by 0
      //TODO: handle type
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case MULT:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr,j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
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
        return rv;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        return ct_nop_expr();
      }
      ex->params = newdyn;
      return ex;
    case DIVI:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
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
        return rv;
      } else if(newdyn->length == 0) {
        dadtor(newdyn);
        free(ex);
        return ct_nop_expr();
      }
      return ex;
    case MOD: 
      INTOP(%=);
      //we don't flatten mods--that can happen in SSA and also chaining lots of modulos
      //is not a case that is realistic or one I will handle
      return ex;
    case L_AND:
      newdyn = dactor(32);
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case L_AND:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            return subexpr;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(subexpr->uintconst == 0) {
              for(; i < ex->params->length; i++) {
                rfreexpr(ex->params->arr[i]);
              }
            }
            break;
        }
      }
      dadtor(ex->params);
      while(newdyn->length && puritree(dapeek(newdyn))){
        EXPRESSION* ex = dapop(newdyn);
        rfreexpr(ex);
      }
      if(newdyn->length < 1) {
        free(ex);
        if(newdyn->length == 0) {
          dadtor(newdyn);
          return subexpr;
        }
        EXPRESSION* p1 = newdyn->arr[0];
        dadtor(newdyn);
        return ct_binary_expr(COMMA, p1, subexpr);
      }
      ex->params = newdyn;
      return ct_binary_expr(COMMA, ex, subexpr);
    case L_OR:
      newdyn = dactor(32);
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case L_OR:
            for(int j = 0; j < subexpr->params->length; j++) {
              dapush(newdyn, EPARAM(subexpr, j)); 
            }
            dadtor(subexpr->params);
            free(subexpr);
            return subexpr;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
            if(subexpr->uintconst != 0) {
              for(; i < ex->params->length; i++) {
                rfreexpr(ex->params->arr[i]); 
              }
            }
            break;
        }
      }
      dadtor(ex->params);
      while(newdyn->length && puritree(dapeek(newdyn))){
        EXPRESSION* ex = dapop(newdyn);
        rfreexpr(ex);
      }
      if(newdyn->length < 1) {
        free(ex);
        if(newdyn->length == 0) {
          dadtor(newdyn);
          return subexpr;
        }
        EXPRESSION* p1 = newdyn->arr[0];
        dadtor(newdyn);
        return ct_binary_expr(COMMA, p1, subexpr);
      }
      ex->params = newdyn;
      return ct_binary_expr(COMMA, ex, subexpr);
    case B_AND: 
      newdyn = dactor(32);
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
            return subexpr;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
              rectexpr->uintconst &= subexpr->uintconst;
              free(subexpr);
              if(rectexpr->uintconst == 0) {
                for(; i < ex->params->length; i++) {
                  EXPRESSION* free2 = EPARAM(ex, i);
                  if(puritree(free2)) {
                    rfreexpr(free2);
                  } else {
                    dapush(newdyn, free2);
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
        return rectexpr;
      }
      if(rectexpr->uintconst != -1UL) {
        dapush(newdyn, rectexpr);
      }

      ex->params = newdyn;
      return ex;
      //flatten, make const, if const is 0, eliminate impure
    case B_OR:
      newdyn = dactor(32);
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
            return subexpr;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
              rectexpr->uintconst |= subexpr->uintconst;
              free(subexpr);
              if(rectexpr->uintconst == -1UL) {
                for(; i < ex->params->length; i++) {
                  EXPRESSION* free2 = EPARAM(ex, i);
                  if(puritree(free2)) {
                    rfreexpr(free2);
                  } else {
                    dapush(newdyn, free2);
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
        return rectexpr;
      }
      if(rectexpr->uintconst != 0) {
        dapush(newdyn, rectexpr);
      }

      ex->params = newdyn;
      return ex;

    case B_XOR:
      newdyn = dactor(32);
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
            return subexpr;
          default:
            dapush(newdyn, subexpr);
            break;
          case COMMA:
            //look at end of expr
            dapush(newdyn, subexpr);
            break;
          case UINT: case INT: case FLOAT:
              rectexpr->uintconst ^= subexpr->uintconst;
              free(subexpr);
            break;
        }
      }
      dadtor(ex->params);
      if(newdyn->length == 0) {
        free(ex);
        dadtor(newdyn);
        return rectexpr;
      }
      if(rectexpr->uintconst != 0) {
        dapush(newdyn, rectexpr);
      }

      ex->params = newdyn;
      return ex;
    case SHL://maybe check if right side is within bounds of type size
      INTOP(<<=);
      return ex;
    case SHR:
      INTOP(>>=);
      return ex;

    case COMMA:
      newdyn = dactor(32);
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        if(i != ex->params->length - 1) {
          if(puritree(subexpr)) {
            rfreexpr(subexpr); 
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
                  continue;
                }
              }
              dapush(newdyn, fse); 
            }
          default:
            dapush(newdyn, subexpr);
            break;
        }
        dadtor(ex->params);
        ex->params = newdyn;
        return ex;
      }
      //adopt/flatten in subexprs, remove pure exprs except the last one
      //if impure call to pure function, extract out impure params, and eval those only????
    case EQ: //should be kept binary
      CMPOP(==);
      //if both INT, UINT, FLOAT, and equal return intconst 1, else return intconst 0
      //That's all I can think of for now
      return ex;
    case NEQ:
      CMPOP(!=);
      return ex;
    case GT: 
      CMPOP2(>);
      return ex;
    case LT:
      CMPOP2(<);
      return ex;
    case GTE: 
      CMPOP2(>=);
      return ex;
    case LTE: 
      CMPOP2(<=);
      return ex;
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case XORASSIGN: case ORASSIGN:  //0 is identity case 
    case ANDASSIGN: case MODASSIGN: //no identity case (for our purposes)
    case DIVASSIGN: case MULTASSIGN: 
      //if identity on right side of expression (mostly 0, sometimes 1, none in case of mod (and for our purposes AND), etc) and expression is pure, ellide
    case ASSIGN:
      //if same value on both sides, ellide if pure
      //Further fix assign op
    case PREINC: case PREDEC: case POSTINC: case POSTDEC:
      return ex;
    case TERNARY:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case INT: case UINT: case FLOAT:
          if(subexpr->uintconst== 0) {
            rfreexpr(EPARAM(ex, 1)); 
            free(subexpr);
            return EPARAM(ex, 2);
          } else {
            rfreexpr(EPARAM(ex, 2)); 
            free(subexpr);
            return EPARAM(ex, 1);
          }
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
            default:
              break;
          }
          default:
            break;
      }
      return ex;
    case FCALL:
    case SZOFEXPR:
      return ex;
  }
  fprintf(stderr, "Error: reducing expression failed\n");
  return NULL;
}

//do the same as above but with statements
