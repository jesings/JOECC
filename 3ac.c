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
OPERATION* linearitree(EXPRESSION* cexpr, PROGRAM* prog) {
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

char* proglabel(PROGRAM* prog) {
  char* c = malloc(8);
  snprintf(c, 8, ".L%d", (prog->labelcnt)++);
  return c;
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
void solidstate(STATEMENT* cst, PROGRAM* prog) {
  OPERATION* ret_op;
  switch(cst->type){
    case FRET:
      ret_op = linearitree(cst->expression, prog);
      assert(ret_op);//assert its dest as well?
      dapush(prog->ops, ct_3ac_op1(RETURN_3, ret_op->dest_type, ret_op->dest));
      return;
    case LBREAK: //for break and continue we may need to do stack manipulation
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, (ADDRESS) (char*) dapeek(prog->breaklabels)));
      return;
    case LCONT:
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, (ADDRESS) (char*) dapeek(prog->continuelabels)));
      return;
    case JGOTO:
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, (ADDRESS) cst->glabel));
      return;
    case WHILEL: {
      char* contlabel = proglabel(prog);
      char* brklabel = proglabel(prog);
      dapush(prog->continuelabels, contlabel);
      dapush(prog->breaklabels, brklabel);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) contlabel));
      //cmptype garbage
      solidstate(cst->body, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
      }
    case DOWHILEL: {
      char* contlabel = proglabel(prog);
      char* brklabel = proglabel(prog);
      dapush(prog->continuelabels, contlabel);
      dapush(prog->breaklabels, brklabel);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) contlabel));
      solidstate(cst->body, prog);
      linearitree(cst->cond, prog);
      //cmptype garbage
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
      }
    case IFS: {
      linearitree(cst->ifcond, prog);
      char* afteriflbl = proglabel(prog);
      //cmptype garbage, jump on condition
      solidstate(cst->thencond, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) afteriflbl));
      return;
      }
    case IFELSES: {
      linearitree(cst->ifcond, prog);
      char* afteriflbl = proglabel(prog);
      char* afterelselbl = proglabel(prog);
      //cmptype garbage, jump on condition
      solidstate(cst->thencond, prog);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, (ADDRESS) afterelselbl));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) afteriflbl));
      solidstate(cst->elsecond, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) afterelselbl));
      return;
      }
    case SWITCH:
      //TODO: figure out how to represent switch stmt!!! Either jump table or multiple ifs w/ goto but how to decide and how to represent jump table
      break;
    case LABEL:
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) cst->glabel));
      return;
    case CMPND: 
      solidstate(cst->body, prog);
      //probably more stack stuff will need to be done here?
      return;
    case EXPR:
      linearitree(cst->expression, prog);
      return;
    case NOPSTMT: 
      return;
    case CASE: case DEFAULT:
      break; //should never see case or default
  }
  fprintf(stderr, "Error: reduction of statement to 3 address code failed\n");
}
