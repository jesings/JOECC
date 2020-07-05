#include <string.h>
#include <stdlib.h>
#include "dynarr.h"
#include "compintern.h"
#define EPARAM(EVA, IND) ((EXPRESSION*)((EVA)->params->arr[IND]))
#define LPARAM(EVA, IND) ((EVA)->params->arr[IND])
//  X(NOP), X(STRING), X(INT), X(UINT), X(FLOAT), X(IDENT), X(ARRAY_LIT), \
//  X(ADD), X(NEG), X(SUB), X(EQ), X(NEQ), X(GT), X(LT), X(GTE), X(LTE), X(MULT), X(DIVI), X(MOD), \
//  X(PREINC), X(POSTINC), X(PREDEC), X(POSTDEC), \
//  X(L_AND), X(L_OR), X(L_NOT), X(B_AND), X(B_OR), X(B_XOR), X(B_NOT), X(SHL), X(SHR), \
//  X(DOTOP), X(ARROW), \
//  X(SZOF), X(SZOFEXPR), \
//  X(ASSIGN), \
//  X(ADDASSIGN), X(SUBASSIGN), X(SHLASSIGN), X(SHRASSIGN), X(ANDASSIGN),  \
//  X(XORASSIGN), X(ORASSIGN), X(DIVASSIGN), X(MULTASSIGN), X(MODASSIGN), \
//  X(CAST), \
//  X(COMMA), \
//  X(ADDR), X(DEREF), \
//  X(FCALL), \
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
  //factor out params into loop for ease of use, probably use DYNARR
}

EXPRESSION* foldconst(EXPRESSION* ex) {
  EXPRESSION* subexpr;
  DYNARR* newdyn;
  EXPRESSION* rectexpr;
  EXPRTYPE eventualtype;

  //call on each param before the switch
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
        default:
          return ex;
      }
    case B_NOT: 
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case B_NOT:
          //free expr and subexpr
          subexpr = EPARAM(subexpr, 0);
          return subexpr;
        default:
          return ex;
      }
    case ADDR:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case DEREF:
          //free expr and subexpr
          subexpr = EPARAM(subexpr, 0);
          return subexpr;
        default:
          return ex;
      }
    case DEREF:
      subexpr = EPARAM(ex, 0);
      switch(subexpr->type) {
        case ADDR:
          //free expr and subexpr
          subexpr = EPARAM(subexpr, 0);
          return subexpr;
        default:
          return ex;
      }
    case ADD:
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(0);
      //TODO: remove 0s, handle type
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
      //Not sure this is right
      for(int i = 0; i < ex->params->length; i++) {
        subexpr = EPARAM(ex, i);
        switch(subexpr->type) {
          case SUB:
            LPARAM(ex, i) = ct_unary_expr(NEG, EPARAM(subexpr, 0)); 
            for(int j = 1; j < subexpr->params->length; j++) {
              dapush(ex->params, EPARAM(subexpr, j)); 
            }
            //free subexpr and its params dynarr
            return subexpr;
          case INT:
          case UINT:
          case FLOAT:
            ;//TODO:
        }
      }
      //if first element is 0, transform into ADD if more than one other arg, wrap in NEG
      //if one remaining param, remove SUB wrapper
      return ex;
    case MULT: 
      newdyn = dactor(32);
      rectexpr = ct_uintconst_expr(1);
      //TODO: destroy all pure arguments when multiply by 0
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
            return subexpr;
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
            }
            free(subexpr);
            break;
        }
      }
      if(((rectexpr->type != UINT) ||
          rectexpr->uintconst != 1))
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
      //TODO: figure out what can be done here
      return ex;
    case MOD: 
      //TODO: figure out what can be done here
      return ex;
    case L_AND:
      //adopt/flatten subexprs in
      //preserve order
      //Remove all params after first constant 0 param, remove all pure params that are between 
      //the last impure param and the constant 0, wrap in comma with constant 0 at end if necessary
      return ex;
    case L_OR:
      //Remove all params after first constant 1 param, remove all pure params that are between 
      //the last impure param and the constant 1, wrap in comma with constant 1 at end if necessary
      return ex;
    case B_AND: case B_OR: case B_XOR: case SHL: case SHR:
      ///no clue what should be done here except adopt/flatten in subexprs
    case COMMA:
      //adopt/flatten in subexprs, remove pure exprs except the last one
      //if impure call to pure function, extract out impure params, and eval those only
    case EQ: //should be kept binary
      //if both INT, UINT, FLOAT, and equal return intconst 1, else return intconst 0
      //That's all I can think of for now
    case NEQ: case GT: case LT: case GTE: case LTE: 
      //see above, adapt to specifics
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      //if identity on right side of expression and expression is pure, ellide
    case ASSIGN:
      //if same value on both sides, ellide
      //Further fix assign op
    case PREINC: case PREDEC: case POSTINC: case POSTDEC:
      return ex;
    case TERNARY:
    case FCALL:
    case SZOFEXPR:
      return ex;
  }
}

//do the same as above but with statements
