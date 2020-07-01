#include <string.h>
#include <stdlib.h>
#include "dynarr.h"
#include "compintern.h"
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
        if(!puritree(cexpr->params->arr[i]))
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
//A more sophisticated version of the above is possible but I won't do that work
//The above will not balk at initializers, as those aren't considered assignment ops
//maybe confirm identifier is local and not a param?
//calling other function that is impure (or indirect function)
//lots of work will need to be done in order to ignore circular dependencies

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
    case SZOF:
      return e1->typesz == e2->typesz;//probably stupid, there're much better ways to do this, TODO: fix
    case NOP:
      return 1;
    case ARRAY_LIT:
      return 0;
    case CAST:  case SZOFEXPR:
      //compare idtype as well
    case NEG: case L_NOT: case B_NOT: case ADDR: case DEREF:
    case TERNARY:
    case FCALL:
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE: case MULT: case DIVI: 
    case MOD: case L_AND: case L_OR: case B_AND: case B_OR: case B_XOR: case SHL: case SHR: case COMMA:
    case ASSIGN: case PREINC: case PREDEC: case POSTINC: case POSTDEC: case DOTOP: case ARROW:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      if(e1->params->length != e2->params->length)//big problem here
        return 0;
      for(int i = 0; i < e1->params->length; i++) {
        if(!treequals(e1->params->arr[i], e2->params->arr[i]))
          return 0;
      }
      return 1;
  }
  //factor out params into loop for ease of use, probably use DYNARR
}
