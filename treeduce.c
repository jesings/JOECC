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
#define subswitch(TYUINT, TYINT, TYFLOAT) 
#define CONSTOP(OP) do { \
  char valflag = 1; \
  int64_t valdeposit[1]; \
  switch(expr->param1->type) { \
    case UINT: \
      switch(expr->param2->type) { \
        case UINT: \
          expr->type = UINT; \
          *valdeposit = expr->param1->uintconst OP expr->param2->uintconst; \
          break; \
        case INT: \
          expr->type = INT; \
          *valdeposit = expr->param1->uintconst OP expr->param2->intconst; \
          break; \
        case FLOAT: \
          expr->type = FLOAT; \
          *(float*) valdeposit = expr->param1->uintconst OP expr->param2->floatconst; \
          break; \
        default: \
          valflag = 0; \
          break; \
      } \
      break; \
    case INT: \
      switch(expr->param2->type) { \
        case UINT: \
          expr->type = INT; \
          *valdeposit = expr->param1->intconst OP expr->param2->uintconst; \
          break; \
        case INT: \
          expr->type = INT; \
          *valdeposit = expr->param1->intconst OP expr->param2->intconst; \
          break; \
        case FLOAT: \
          expr->type = FLOAT; \
          *(float*) valdeposit = expr->param1->intconst OP expr->param2->floatconst; \
          break; \
        default: \
          valflag = 0; \
          break; \
      } \
      break; \
    case FLOAT: \
      switch(expr->param2->type) { \
        case UINT: \
          expr->type = FLOAT; \
          *(float*) valdeposit = expr->param1->uintconst OP expr->param2->floatconst; \
          break; \
        case INT: \
          expr->type = FLOAT; \
          *(float*) valdeposit = expr->param1->intconst OP expr->param2->floatconst; \
          break; \
        case FLOAT: \
          expr->type = FLOAT; \
          *(float*) valdeposit = expr->param1->floatconst OP expr->param2->floatconst; \
          break; \
        default: \
          valflag = 0; \
          break; \
      } \
      break; \
    default: \
      valflag = 0; \
      break; \
  } \
  if(valflag) { \
    free(expr->param1); \
    free(expr->param2); \
    expr->intconst = *valdeposit; \
  } \
  } while(0)
char constree(EXPRESSION* expr) {
  switch(expr->type){
    case STRING: case INT: case UINT: case FLOAT:
      return 1;
    case NEG: case L_NOT: case B_NOT: case COMMA: case CAST: case ADDR:
    case DEREF:
      //
    case ADD: 
      CONSTOP(+);
      break;
    case SUB: 
      CONSTOP(-);
      break;
    case MULT: 
      CONSTOP(*);
      break;
    case DIVI: 
      CONSTOP(/);
      break;
    case EQ: case NEQ: case GT: case LT: case GTE:
    case LTE: case MOD: case L_AND: case L_OR:
    case B_AND: case B_XOR: case SHL: case SHR: case DOTOP:
      //
    case SZOFEXPR:
      //
    case TERNARY:
      //Ignore then or else depending on if
    case IDENT:
      //do something with const?
      return 0; 
    default:
      return 0;
  }
}
char isconstexpr(EXPRESSION* cexpr) {
  switch(cexpr->type){
    case STRING: case INT: case UINT: case FLOAT:
      return 1;
    case NEG: case L_NOT: case B_NOT: case COMMA: case CAST: case ADDR:
    case DEREF:
      return isconstexpr(cexpr->unaryparam);
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE:
    case LTE: case MULT: case DIVI: case MOD: case L_AND: case L_OR:
    case B_AND: case B_XOR: case SHL: case SHR: case DOTOP:
      return isconstexpr(cexpr->param1) && isconstexpr(cexpr->param2);
    case SZOFEXPR:
      return isconstexpr(cexpr->castexpr);
    case TERNARY:
      return isconstexpr(cexpr->ifexpr) ;
    case IDENT:
      return 1; 
    default:
      return 0;
  }
}
