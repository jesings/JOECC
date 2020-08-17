#include <stdio.h>
#include <assert.h>
#include "compintern.h"
#include "3ac.h"

OPERATION* ct_3ac_op0(enum opcode_3ac opcode) {
  OPERATION* retval = malloc(sizeof(OPERATION));
  retval->opcode = opcode;
  return retval;
}
OPERATION* ct_3ac_op1(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0) {
  OPERATION* retval = malloc(sizeof(OPERATION));
  retval->opcode = opcode;
  retval->addr0_type = addr0_type;
  retval->addr0 = addr0;
  return retval;
}
OPERATION* ct_3ac_op2(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0, ADDRTYPE dest_type, ADDRESS dest) {
  OPERATION* retval = malloc(sizeof(OPERATION));
  retval->opcode = opcode;
  retval->addr0_type = addr0_type;
  retval->addr0 = addr0;
  retval->dest_type = dest_type;
  retval->dest = dest;
  return retval;
}
OPERATION* ct_3ac_op3(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0, 
                     ADDRTYPE addr1_type, ADDRESS addr1, ADDRTYPE dest_type, ADDRESS dest) {
  OPERATION* retval = malloc(sizeof(OPERATION));
  retval->opcode = opcode;
  retval->addr0_type = addr0_type;
  retval->addr0 = addr0;
  retval->addr1_type = addr1_type;
  retval->addr1 = addr1;
  retval->dest_type = dest_type;
  retval->dest = dest;
  return retval;
}

//returns destination for use in calling function
OPERATION* linearitree(EXPRESSION* cexpr, DYNARR* prog) {
  switch(cexpr->type){
    case STRING: case INT: case UINT: case FLOAT: case NOP: case IDENT: case ARRAY_LIT: case SZOF: case MEMBER:
    case NEG: case L_NOT: case B_NOT: case ADDR: case DEREF:
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE: case MULT: case DIVI: 
    case MOD: case L_AND: case L_OR: case B_AND: case B_OR: case B_XOR: case SHL: case SHR: case COMMA:
    case DOTOP: case ARROW:
    case SZOFEXPR: case CAST: 
    case TERNARY:
    case FCALL:
    case ASSIGN: case PREINC: case PREDEC: case POSTINC: case POSTDEC:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      break;
  }
  fprintf(stderr, "Error: reduction of expression to 3 address code failed\n");
  return NULL;
}

ADDRTYPE cmptype(EXPRESSION* cmpexpr) {
  switch(cmpexpr->type) {
    case EQ:
      return (ADDRTYPE) BEQ_I;//figure out signedness here or elsewhere
    case NEQ:
      return (ADDRTYPE) BNE_I;
    case GT:
      return (ADDRTYPE) BGT_I;
    case LT:
      return (ADDRTYPE) BLT_I;
    case GTE:
      return (ADDRTYPE) BGE_I;
    case LTE:
      return (ADDRTYPE) BLE_I;
    case L_NOT:
      return (ADDRTYPE) BEZ_3;
    default:
      return (ADDRTYPE) BNZ_3;
  }
}

//store some state about enclosing switch statement and its labeltable (how to represent?), about enclosing loop as well (for continue)
void solidstate(STATEMENT* cst, DYNARR* prog) {
  OPERATION* ret_op;
  switch(cst->type){
    case FRET:
      ret_op = linearitree(cst->expression, prog);
      assert(ret_op);//assert its dest as well?
      dapush(prog, ct_3ac_op1(RETURN_3, ret_op->dest_type, ret_op->dest));
      return;
    case LBREAK: case LCONT:
      break;
    case JGOTO:
      dapush(prog, ct_3ac_op1(RETURN_3, ISCONST | ISLABEL, (ADDRESS) cst->glabel));
      return;
    case WHILEL: 
    case DOWHILEL: 
    case IFS: case IFELSES:
    case SWITCH:
      //TODO: figure out how to represent switch stmt!!! Either jump table or multiple ifs w/ goto but how to decide and how to represent jump table
    case LABEL:
      dapush(prog, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) cst->glabel));
      return;
    case CMPND:  case EXPR: case DEFAULT:
      break;
    case NOPSTMT: 
      return;
    case CASE: 
      break; //should never see case
  }
  fprintf(stderr, "Error: reduction of statement to 3 address code failed\n");
}
