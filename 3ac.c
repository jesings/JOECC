#include <stdio.h>
#include <assert.h>
#include "compintern.h"
#include "3ac.h"
//TODO: For loops with continue don't work I think

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

ADDRTYPE conv_type_type(IDTYPE* idt) {
  if(idt->pointerstack && idt->pointerstack->length) {
    return 64 | ISPOINTER;
  }
  if(idt->tb & ENUMVAL) {
    return 64 | ISSIGNED;
  }
  if(idt->tb & (STRUCTVAL | UNIONVAL)) {
    return 64 | ISSTRUCT;
  }
  if(idt->tb & FLOATNUM) {
    return (idt->tb & 0x7f) | ISSIGNED | ISFLOAT;
  }
  if(idt->tb & UNSIGNEDNUM) {
    return (idt->tb & 0x7f);
  }
  return (idt->tb & 0x7f) | ISSIGNED;
}

OPERATION* implicit_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog) {
  ADDRTYPE retaddr_type;
  char opmod;

  if((addr0_type & ISFLOAT)) {
    if(!(addr1_type & ISFLOAT)) {
      if(addr1_type & ISCONST) {
        if(addr1_type & ISSIGNED) {
          addr1.floatconst_64 = (float) addr1.intconst_64;
        } else {
          addr1.floatconst_64 = (float) addr1.uintconst_64;
        }
      } else {
        ADDRESS tmpaddr;
        tmpaddr.fregnum = prog->fregcnt++;
        dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, addr1_type, addr1, addr1_type | ISFLOAT | ISSIGNED, tmpaddr));
        addr1 = tmpaddr;
      }
      addr1_type |= ISFLOAT | ISSIGNED; 
      retaddr_type = addr0_type;
    } else {
      retaddr_type = addr0_type & 0x7f;
      if(addr1_type & (0x7f > retaddr_type))
        retaddr_type= addr1_type & 0x7f;
      retaddr_type |= ISFLOAT | ISSIGNED; 
    }
    opmod = 2;
  } else if (!(addr0_type & ISFLOAT) && (addr1_type & ISFLOAT)) {
    if(addr0_type & ISCONST) {
      if(addr1_type & ISSIGNED) {
        addr0.floatconst_64 = (float) addr0.intconst_64;
      } else {
        addr0.floatconst_64 = (float) addr0.uintconst_64;
      }
    } else {
      ADDRESS tmpaddr;
      tmpaddr.fregnum = prog->fregcnt++;
      dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, addr0_type, addr0, addr0_type | ISFLOAT | ISSIGNED, tmpaddr));
      addr0 = tmpaddr;
    }
    addr0_type |= ISFLOAT | ISSIGNED; 
    opmod = 2;
    retaddr_type = addr1_type;
  } else if ((addr0_type & ISSIGNED) || (addr1_type & ISSIGNED)) {
    opmod = 1;
    retaddr_type = addr0_type & 0x7f;
    if(addr1_type & (0x7f > retaddr_type))
      retaddr_type = addr1_type & 0x7f;
    retaddr_type |= ISSIGNED;
  } else {
    opmod = 0;
    retaddr_type = addr0_type & 0x7f;
    if(addr1_type & (0x7f > retaddr_type))
      retaddr_type= addr1_type & 0x7f;
  }
  ADDRESS retaddr;
  if(retaddr_type & ISFLOAT) 
    retaddr.fregnum = prog->fregcnt++;
  else
    retaddr.iregnum = prog->iregcnt++;
  return ct_3ac_op3(opcode_unsigned + opmod, addr0_type, addr0, addr1_type, addr1, retaddr_type, retaddr);
}

OPERATION* nocoerce_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                          ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog) {
  ADDRTYPE retaddr_type;
  char opmod;

  if((addr0_type & ISFLOAT) && (addr1_type & ISFLOAT)) {
    opmod = 1;
    retaddr_type = addr0_type & 0x7f;
    if(addr1_type & (0x7f > retaddr_type))
      retaddr_type = addr1_type & 0x7f;
    retaddr_type |= ISFLOAT | ISSIGNED;
  } else if((addr0_type & ISFLOAT) || (addr1_type & ISFLOAT)) {
    //TODO: error
  } else if ((addr0_type & ISSIGNED) || (addr1_type & ISSIGNED)) {
    opmod = 0;
    retaddr_type = addr0_type & 0x7f;
    if(addr1_type & (0x7f > retaddr_type))
      retaddr_type = addr1_type & 0x7f;
    retaddr_type |= ISSIGNED;
  } else {
    opmod = 0;
    retaddr_type = addr0_type & 0x7f;
    if(addr1_type & (0x7f > retaddr_type))
      retaddr_type= addr1_type & 0x7f;
  }
  ADDRESS retaddr;
  if(retaddr_type & ISFLOAT) 
    retaddr.fregnum = prog->fregcnt++;
  else
    retaddr.iregnum = prog->iregcnt++;
  return ct_3ac_op3(opcode_unsigned + opmod, addr0_type, addr0, addr1_type, addr1, retaddr_type, retaddr);
}

OPERATION* implicit_nary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR prevaddr = linearitree(daget(cexpr->params, 0), prog);
  OPERATION* cur_op;
  for(int i = 1; i < cexpr->params->length; i++) {
    FULLADDR secondaddr = linearitree(daget(cexpr->params, i), prog);
    dapush(prog->ops, cur_op);
    cur_op = implicit_3ac_3(opcode_unsigned, prevaddr.addr_type, prevaddr.addr, secondaddr.addr_type, secondaddr.addr, prog);
    prevaddr = op2addr(cur_op);
  }
  return cur_op;
}

OPERATION* nocoerce_nary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR prevaddr = linearitree(daget(cexpr->params, 0), prog);
  OPERATION* cur_op;
  for(int i = 1; i < cexpr->params->length; i++) {
    FULLADDR secondaddr = linearitree(daget(cexpr->params, i), prog);
    dapush(prog->ops, cur_op);
    cur_op = nocoerce_3ac_3(opcode_unsigned, prevaddr.addr_type, prevaddr.addr, secondaddr.addr_type, secondaddr.addr, prog);
    prevaddr = op2addr(cur_op);
  }
  return cur_op;
}

FULLADDR implicit_shortcircuit_3(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, ADDRESS complete_val, ADDRESS shortcircuit_val, PROGRAM* prog) {
  ADDRESS doneaddr, afterdoneaddr;
  doneaddr.labelname = proglabel(prog);
  afterdoneaddr.labelname = proglabel(prog);
  for(int i = 0; i < cexpr->params->length; i++) {
    FULLADDR addr2use = linearitree(daget(cexpr->params, i), prog);
    dapush(prog->ops, ct_3ac_op2(op_to_cmp, addr2use.addr_type, addr2use.addr, ISLABEL | ISCONST, doneaddr));
  }
  FULLADDR destaddr;
  destaddr.addr_type = 0;
  destaddr.addr.iregnum = prog->iregcnt++;//TODO: maybe make it signed?
  dapush(prog->ops, ct_3ac_op2(MOV_3, ISCONST, complete_val, destaddr.addr_type, destaddr.addr));
  dapush(prog->ops, ct_3ac_op1(JMP_3, ISLABEL | ISCONST, afterdoneaddr));
  dapush(prog->ops, ct_3ac_op1(LBL_3, ISLABEL | ISCONST, doneaddr));
  dapush(prog->ops, ct_3ac_op2(MOV_3, ISCONST, shortcircuit_val, destaddr.addr_type, destaddr.addr));
  dapush(prog->ops, ct_3ac_op1(LBL_3, ISLABEL | ISCONST, afterdoneaddr));
  return destaddr;
}

OPERATION* cmpret_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR otheraddr = linearitree(daget(cexpr->params, 1), prog);
  OPERATION* retop = implicit_3ac_3(opcode_unsigned, curaddr.addr_type, curaddr.addr, otheraddr.addr_type, otheraddr.addr, prog);
  if(retop->dest_type & ISFLOAT) {
    retop->dest_type = curaddr.addr_type & 0x7f;
    if(retop->dest_type < (otheraddr.addr_type & 0x7f)) {
      retop->dest_type = otheraddr.addr_type & 0x7f;
    }
    retop->dest_type |= curaddr.addr_type & otheraddr.addr_type & ISSIGNED;
    //free up float register???
    retop->dest.iregnum = prog->iregcnt++;
  }
  return retop;
}

OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  //check for no floats?
  enum opcode_3ac shlop = opcode_unsigned + (a1.addr_type & ISSIGNED ? 1 : 0);
  ADDRESS adr;
  adr.iregnum = prog->iregcnt++;
  return ct_3ac_op3(shlop, a1.addr_type, a1.addr, a2.addr_type, a2.addr, a1.addr_type, adr);
}

//returns destination for use in calling function
FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr, otheraddr, destaddr;
  ADDRESS initlbl, scndlbl;

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
      curaddr.addr_type = ISCONST | ISFLOAT | ISSIGNED | 0x40;
      curaddr.addr.floatconst_64 = cexpr->floatconst;
      return curaddr;
    case IDENT: 
    case ARRAY_LIT:

    case NEG:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & ~ISCONST;
      if(destaddr.addr_type & ISFLOAT) {
        destaddr.addr.fregnum = prog->fregcnt++;
        dapush(prog->ops, ct_3ac_op2(NEG_F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else {
        destaddr.addr.iregnum = prog->iregcnt++;
        dapush(prog->ops, ct_3ac_op2(NEG_I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;
    case L_NOT:
      break;
    case B_NOT:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & ~ISCONST;
      if(destaddr.addr_type & ISFLOAT) {
        destaddr.addr.fregnum = prog->fregcnt++;
        dapush(prog->ops, ct_3ac_op2(NOT_F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else {
        destaddr.addr.iregnum = prog->iregcnt++;
        dapush(prog->ops, ct_3ac_op2(NOT_U, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;

    case ADDR:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = ISPOINTER | 0x40;
      destaddr.addr.ptaddr = malloc(sizeof(POINTADDR));
      if (curaddr.addr_type & ISPOINTER) {
        destaddr.addr.ptaddr->pointerdepth = curaddr.addr.ptaddr->pointerdepth + 1;
        destaddr.addr.ptaddr->adt = curaddr.addr.ptaddr->adt;
        destaddr.addr.ptaddr->pointersize = curaddr.addr.ptaddr->pointersize;
      } else {
        destaddr.addr.ptaddr->pointerdepth = 1;
        destaddr.addr.ptaddr->adt = curaddr.addr_type & ~ISCONST;
        destaddr.addr.ptaddr->pointersize = curaddr.addr_type & 0x7f;
      }
      destaddr.addr.ptaddr->iregnum = prog->iregcnt++;
      if(destaddr.addr_type & ISFLOAT) {
        dapush(prog->ops, ct_3ac_op2(ADDR_F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else {
        dapush(prog->ops, ct_3ac_op2(ADDR_U, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;

    case DEREF: //Turn deref of addition, subtraction, into array index?
      //TODO: not sure if this is right
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      if (curaddr.addr_type & ISPOINTER) {
        if(curaddr.addr.ptaddr->pointerdepth > 1) {
          destaddr.addr_type = ISPOINTER;
          destaddr.addr.ptaddr = malloc(sizeof(POINTADDR));
          destaddr.addr.ptaddr->pointerdepth = curaddr.addr.ptaddr->pointerdepth + 1;
          destaddr.addr.ptaddr->adt = curaddr.addr.ptaddr->adt;
          destaddr.addr.ptaddr->pointersize = curaddr.addr.ptaddr->pointersize;
        } else {
          destaddr.addr_type = curaddr.addr.ptaddr->adt;
        }
      } else {
        //error
      }
      //only for rvalue
      if(destaddr.addr_type & ISFLOAT) {
        destaddr.addr.fregnum = prog->fregcnt++;
      } else {
        if(destaddr.addr_type & ISPOINTER) {
          destaddr.addr.ptaddr->iregnum = prog->iregcnt++;
        } else {
          destaddr.addr.iregnum = prog->iregcnt++;
        }
      }
      dapush(prog->ops, ct_3ac_op2(MOV_FROM_PTR, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;

    case ADD:
      return op2ret(prog->ops, implicit_nary_3(ADD_U, cexpr, prog));
    case SUB: 
      return op2ret(prog->ops, implicit_nary_3(SUB_U, cexpr, prog));
    case MULT:
      return op2ret(prog->ops, implicit_nary_3(MULT_U, cexpr, prog));
    case DIVI: 
      return op2ret(prog->ops, implicit_nary_3(DIV_U, cexpr, prog));

    case EQ:
      return op2ret(prog->ops, cmpret_binary_3(EQ_U, cexpr, prog));
    case NEQ: 
      return op2ret(prog->ops, cmpret_binary_3(NE_U, cexpr, prog));
    case GT:
      return op2ret(prog->ops, cmpret_binary_3(GT_U, cexpr, prog));
    case LT:
      return op2ret(prog->ops, cmpret_binary_3(LT_U, cexpr, prog));
    case GTE:
      return op2ret(prog->ops, cmpret_binary_3(GE_U, cexpr, prog));
    case LTE:
      return op2ret(prog->ops, cmpret_binary_3(LE_U, cexpr, prog));

    case MOD: //TODO: prevent float
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      otheraddr = linearitree(daget(cexpr->params, 1), prog);
      return op2ret(prog->ops, implicit_3ac_3(MOD_U, curaddr.addr_type, curaddr.addr, otheraddr.addr_type, otheraddr.addr, prog));

    case L_AND:
      return implicit_shortcircuit_3(BEZ_3, cexpr, (ADDRESS) 1ul, (ADDRESS) 0ul, prog);
    case L_OR:
      return implicit_shortcircuit_3(BNZ_3, cexpr, (ADDRESS) 0ul, (ADDRESS) 1ul, prog);

    case B_AND:
      return op2ret(prog->ops, implicit_nary_3(AND_U, cexpr, prog));
    case B_OR:
      return op2ret(prog->ops, implicit_nary_3(OR_U, cexpr, prog));
    case B_XOR:
      return op2ret(prog->ops, implicit_nary_3(XOR_U, cexpr, prog));

    case SHL:
      return op2ret(prog->ops, binshift_3(SHL_U, cexpr, prog));
    case SHR:
      return op2ret(prog->ops, binshift_3(SHR_U, cexpr, prog));
    case COMMA:
      for(int i = 0; i < cexpr->params->length - 1; i++) {
        linearitree(daget(cexpr->params, i), prog);
      }
      return linearitree(daget(cexpr->params, cexpr->params->length - 1), prog);

    case DOTOP: case ARROW:
      break;
    case SZOFEXPR:
      //TODO: handle structs properly
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr.uintconst_64 = curaddr.addr_type & 0x7fL;
      destaddr.addr_type = ISCONST;
      return destaddr;
    case CAST:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      if(cexpr->vartype->pointerstack && cexpr->vartype->pointerstack->length) {
        break;
        //move to unsigned int reg, make 64 bit, anything else?
      } else if(cexpr->vartype->tb & INT) {
        destaddr.addr_type = (cexpr->vartype->tb & 0x7f) | ISSIGNED;
        destaddr.addr.iregnum = prog->iregcnt++;
        if(curaddr.addr_type & !(ISFLOAT | ISLABEL | ISSTRCONST | 0x7f)) {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(FLOAT_TO_INT, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          break;
        }
        return destaddr;
      } else if(cexpr->vartype->tb & UINT) {
        destaddr.addr_type = cexpr->vartype->tb & 0x7f;
        destaddr.addr.iregnum = prog->iregcnt++;
        if(curaddr.addr_type & !(ISFLOAT | ISLABEL | ISSTRCONST | 0x7f)) {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(FLOAT_TO_INT, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          break;
        }
        return destaddr;
      } else if(cexpr->vartype->tb & FLOAT) {
        destaddr.addr_type = (cexpr->vartype->tb & 0x7f) | ISSIGNED | ISFLOAT;
        destaddr.addr.fregnum = prog->fregcnt++;
        if(curaddr.addr_type & !(ISFLOAT | ISLABEL | ISSTRCONST | 0x7f)) {
          dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          break;
        }
        return destaddr;
      }
      break;
    case TERNARY:
      //do more checking of other 
      initlbl.labelname = proglabel(prog);
      scndlbl.labelname = proglabel(prog);
      dapush(prog->ops, cmptype(daget(cexpr->params, 0), initlbl.labelname, 1, prog));
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      OPERATION* fixlater = ct_3ac_op0(NOP_3);
      dapush(prog->ops, fixlater);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, scndlbl));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, initlbl));
      otheraddr = linearitree(daget(cexpr->params, 2), prog);
      if(curaddr.addr_type & ISFLOAT) {
        if((otheraddr.addr_type & ISFLOAT) && ((curaddr.addr_type & 0x7f) < (otheraddr.addr_type & 0x7f)))
          destaddr.addr_type = (otheraddr.addr_type & 0x7f);
        else
          destaddr.addr_type = (curaddr.addr_type & 0x7f);
        destaddr.addr_type |= ISFLOAT | ISSIGNED;
        destaddr.addr.fregnum = prog->fregcnt++;
      } else if(otheraddr.addr_type & ISFLOAT) {
        destaddr.addr_type = ISFLOAT | ISSIGNED | (otheraddr.addr_type & 0x7f);
        destaddr.addr.fregnum = prog->fregcnt++;
      } else {
        if((curaddr.addr_type & 0x7f) < (otheraddr.addr_type & 0x7f))
          destaddr.addr_type = otheraddr.addr_type & 0x7f;
        else
          destaddr.addr_type = otheraddr.addr_type & 0x7f;
        destaddr.addr_type |= (curaddr.addr_type & otheraddr.addr_type & ISSIGNED);
        destaddr.addr.iregnum = prog->iregcnt++;
      }
      destaddr.addr = curaddr.addr;
      *fixlater = *ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr);
      dapush(prog->ops, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) scndlbl));
      return destaddr;
      //confirm 2 addrs have same type or are coercible

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
      //TODO: above shoud be an op with a target, which is the result. Check func return type?
  }
  fprintf(stderr, "Error: reduction of expression to 3 address code failed\n");
  return curaddr;
}

OPERATION* cmptype(EXPRESSION* cmpexpr, char* addr2jmp, char negate, PROGRAM* prog) {
  OPERATION* dest_op;
  FULLADDR destaddr;
  //check if new register is assigned to in cmpret, decrement?
  switch(cmpexpr->type) {
    case EQ: case NEQ: case GT: case LT: case GTE: case LTE:
      dest_op = cmpret_binary_3(cmp_osite(cmpexpr->type, negate), cmpexpr, prog);//figure out signedness here or elsewhere
      dest_op->dest_type = ISLABEL;
      dest_op->dest.labelname = addr2jmp;
      return dest_op;
    case L_NOT:
      destaddr = linearitree(daget(cmpexpr->params, 0), prog);
      return ct_3ac_op2(negate ? BNZ_3 : BEZ_3 , destaddr.addr_type, destaddr.addr, ISLABEL, (ADDRESS) addr2jmp);
    default:
      destaddr = linearitree(cmpexpr, prog);
      return ct_3ac_op2(negate ? BEZ_3 : BNZ_3 , destaddr.addr_type, destaddr.addr, ISLABEL, (ADDRESS) addr2jmp);
  }
}

//store some state about enclosing switch statement and its labeltable (how to represent?), about enclosing loop as well (for continue)
void solidstate(STATEMENT* cst, PROGRAM* prog) {
  FULLADDR ret_op;
  ADDRESS contlabel, brklabel;
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
    case WHILEL:
      contlabel.labelname = proglabel(prog);
      brklabel.labelname = proglabel(prog);
      dapush(prog->continuelabels, contlabel.labelname);
      dapush(prog->breaklabels, brklabel.labelname);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      dapush(prog->ops, cmptype(cst->cond, brklabel.labelname, 1, prog));
      solidstate(cst->body, prog);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, contlabel));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
    case DOWHILEL:
      contlabel.labelname = proglabel(prog);
      brklabel.labelname = proglabel(prog);
      dapush(prog->continuelabels, contlabel.labelname);
      dapush(prog->breaklabels, brklabel.labelname);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      solidstate(cst->body, prog);
      linearitree(cst->cond, prog);
      dapush(prog->ops, cmptype(cst->cond, contlabel.labelname, 0, prog));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
    case IFS:
      linearitree(cst->ifcond, prog);
      brklabel.labelname = proglabel(prog);
      dapush(prog->ops, cmptype(cst->cond, brklabel.labelname, 1, prog));
      solidstate(cst->thencond, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      return;
    case IFELSES:
      linearitree(cst->ifcond, prog);
      contlabel.labelname = proglabel(prog);
      brklabel.labelname = proglabel(prog);
      dapush(prog->ops, cmptype(cst->cond, contlabel.labelname, 1, prog));
      solidstate(cst->thencond, prog);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, brklabel));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      solidstate(cst->elsecond, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      return;
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
