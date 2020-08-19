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
FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr, destaddr;
  switch(cexpr->type){
    case STRING: ;
      curaddr.addr_type = ISCONST | ISSTRCONST;
      curaddr.addr.strconst = cexpr->strconst;
      return curaddr;
    case INT:
      curaddr.addr_type = ISCONST | ISSIGNED | 0x40;
      curaddr.addr.intconst_64 = cexpr->intconst;
      return curaddr;
    case UINT: 
      curaddr.addr_type = ISCONST | 0x40;
      curaddr.addr.uintconst_64 = cexpr->uintconst;
      return curaddr;
    case FLOAT:
      curaddr.addr_type = ISCONST | ISFLOAT | 0x40;
      curaddr.addr.floatconst_64 = cexpr->floatconst;
      return curaddr;
    case IDENT: 
    case ARRAY_LIT:
    case NEG:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & ~ISCONST;
      destaddr.addr = (ADDRESS) proglabel(prog);
      dapush(prog->ops, ct_3ac_op2(destaddr.addr_type & ISFLOAT ? NEG_F : NEG_I,
                                   curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;
    case L_NOT:
    case B_NOT:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & ~ISCONST;
      destaddr.addr = (ADDRESS) proglabel(prog);
      dapush(prog->ops, ct_3ac_op2(destaddr.addr_type & ISFLOAT ? NOT_F : NOT_U,
                                   curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;
    case ADDR:
    case DEREF: //Turn deref of addition, subtraction, into array index?
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE: case MULT: case DIVI: 
    case MOD: case L_AND: case L_OR: case B_AND: case B_OR: case B_XOR: case SHL: case SHR:
    case COMMA:
      for(int i = 0; i < cexpr->params->length - 1; i++) {
        linearitree(daget(cexpr->params, i), prog);
      }
      return linearitree(daget(cexpr->params, cexpr->params->length - 1), prog);
    case DOTOP: case ARROW:
    case SZOFEXPR: case CAST: 
    case TERNARY:
    case ASSIGN: case PREINC: case PREDEC: case POSTINC: case POSTDEC:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      break;
    case NOP: case SZOF: case MEMBER:
      break;
    case FCALL: ;
      DYNARR* params = dactor(cexpr->params->length);
      EXPRESSION* fname = daget(cexpr->params, 0);
      for(int i = 1; i < cexpr->params->length; ++i) {
        curaddr = linearitree(daget(cexpr->params, i), prog);
        dapush(params, ct_3ac_op1(PARAM_3, curaddr.addr_type, curaddr.addr));
      }
      damerge(prog->ops, params);
      dapush(prog->ops, ct_3ac_op1(CALL_3, ISCONST | ISLABEL, (ADDRESS) fname->strconst));
      //TODO: above shoud be an op with a target, which is the result. Check func return type and garbage?
  }
  fprintf(stderr, "Error: reduction of expression to 3 address code failed\n");
  return curaddr;
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
  FULLADDR ret_op;
  switch(cst->type){
    case FRET:
      ret_op = linearitree(cst->expression, prog);
      dapush(prog->ops, ct_3ac_op1(RETURN_3, ret_op.addr_type, ret_op.addr));
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
      //TODO: figure out how to represent switch stmt!!! Either jump table or multiple ifs w/ goto but how to decide and how to represent jump table-- put switch and hash in data?
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
