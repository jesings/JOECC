#include <stdio.h>
#include <assert.h>
#include "compintern.h"
#include "treeduce.h"
#include "3ac.h"
//TODO: For loops with continue don't work I think
#define X(s) #s
const char* opcode_3ac_names[] = {
  OPS_3AC
};
#undef X

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

OPERATION* implicit_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog) {
  ADDRTYPE retaddr_type;
  ADDRESS retaddr;
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
      retaddr_type = addr0_type & ~ISCONST;
      if(addr0_type & ISCONST) {
        retaddr.fregnum = prog->fregcnt++;
      } else {
        retaddr = addr0;
      }
    } else {
      retaddr_type = addr0_type & 0xf;
      if(addr1_type & (0xf > retaddr_type))
        retaddr_type= addr1_type & 0xf;
      retaddr_type |= ISFLOAT | ISSIGNED; 
      if(addr0_type & ISCONST) {
        if(addr1_type & ISCONST) {
          retaddr.fregnum = prog->fregcnt++;
        } else {
          retaddr.fregnum = addr1.fregnum;
        }
      } else {
        retaddr.fregnum = addr0.fregnum;
      }
    }
    opmod = 2;
  } else if (addr1_type & ISFLOAT) {
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
    if(addr1_type & ISCONST) {
      retaddr.fregnum = prog->fregcnt++;
    } else {
      retaddr = addr1;
    }
  } else if ((addr0_type & ISSIGNED) || (addr1_type & ISSIGNED)) {
    opmod = 1;
    retaddr_type = addr0_type & 0xf;
    if(addr1_type & (0xf > retaddr_type))
      retaddr_type = addr1_type & 0xf;
    retaddr_type |= ISSIGNED;
    if(addr0_type & ISCONST) {
      if(addr1_type & ISCONST) {
        retaddr.iregnum = prog->iregcnt++;
      } else {
        retaddr = addr1;
      }
    } else {
      retaddr.iregnum = addr0.iregnum;
    }
  } else {
    opmod = 0;
    retaddr_type = addr0_type & 0xf;
    if(addr1_type & (0xf > retaddr_type))
      retaddr_type= addr1_type & 0xf;
    if(addr0_type & ISCONST) {
      if(addr1_type & ISCONST) {
        retaddr.iregnum = prog->iregcnt++;
      } else {
        retaddr = addr1;
      }
    } else {
      retaddr.iregnum = addr0.iregnum;
    }
  }
  return ct_3ac_op3(opcode_unsigned + opmod, addr0_type, addr0, addr1_type, addr1, retaddr_type, retaddr);
}

OPERATION* implicit_binary_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE arg2id = typex(daget(cexpr->params, 1));
  IDTYPE retid = typex(cexpr);
  FULLADDR desta;
  if(retid.pointerstack && retid.pointerstack->length) {
    desta.addr_type = ISPOINTER | 8;
    //perhaps save regnum here?
    desta.addr.iregnum = prog->iregcnt++;
  } else if(retid.tb & FLOATNUM) {
    op += 2;
    if(!(arg1id.tb & FLOATNUM)) {
      FULLADDR fad;
      fad.addr_type = ISFLOAT | (retid.tb & 0xf);
      fad.addr.fregnum = prog->fregcnt++;
      dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    } 
    if(!(arg2id.tb & FLOATNUM)) {
      FULLADDR fad;
      fad.addr_type = ISFLOAT | (retid.tb & 0xf);
      fad.addr.fregnum = prog->fregcnt++;
      dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    }
    desta.addr_type = ISFLOAT | (retid.tb & 0xf);
    //perhaps save regnum here?
    desta.addr.fregnum = prog->fregcnt++;
  } else if(retid.tb & UNSIGNEDNUM) {
    desta.addr_type = retid.tb & 0xf;
    //perhaps save regnum here?
    desta.addr.iregnum = prog->iregcnt++;
  } else {
    op += 1;
    desta.addr_type = (retid.tb & 0xf) | ISSIGNED;
    desta.addr.iregnum = prog->iregcnt++;
  }

  return ct_3ac_op3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, desta.addr_type, desta.addr);
  //return implicit_3ac_3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, prog);
}

OPERATION* implicit_unary_2(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE retid = typex(cexpr);
  FULLADDR desta;
  if(retid.pointerstack && retid.pointerstack->length) {
    desta.addr_type = ISPOINTER | 8;
    //perhaps save regnum here?
    desta.addr.iregnum = prog->iregcnt++;
  } else if(retid.tb & FLOATNUM) {
    op += 2;
    if(!(arg1id.tb & FLOATNUM)) {
      FULLADDR fad;
      fad.addr_type = ISFLOAT | (retid.tb & 0xf);
      fad.addr.fregnum = prog->fregcnt++;
      dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    } 
    desta.addr_type = ISFLOAT | (retid.tb & 0xf);
    //perhaps save regnum here?
    desta.addr.fregnum = prog->fregcnt++;
  } else if(retid.tb & UNSIGNEDNUM) {
    desta.addr_type = retid.tb & 0xf;
    //perhaps save regnum here?
    desta.addr.iregnum = prog->iregcnt++;
  } else {
    op += 1;
    desta.addr_type = (retid.tb & 0xf) | ISSIGNED;
    desta.addr.iregnum = prog->iregcnt++;
  }

  return ct_3ac_op2(op, a1.addr_type, a1.addr, desta.addr_type, desta.addr);
  //return implicit_3ac_3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, prog);
}

OPERATION* nocoerce_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                          ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog) {
  ADDRTYPE retaddr_type;
  char opmod;
  ADDRESS retaddr;

  if((addr0_type & ISFLOAT) && (addr1_type & ISFLOAT)) {
    opmod = 1;
    retaddr_type = addr0_type & 0xf;
    if(addr1_type & (0xf > retaddr_type))
      retaddr_type = addr1_type & 0xf;
    retaddr_type |= ISFLOAT | ISSIGNED;
    if(addr0_type & ISCONST) {
      if(addr1_type & ISCONST) {
        retaddr.fregnum = prog->fregcnt++;
      } else {
        retaddr.fregnum = addr1.fregnum;
      }
    } else {
      retaddr.fregnum = addr0.fregnum;
    }
  } else if((addr0_type & ISFLOAT) || (addr1_type & ISFLOAT)) {
    //TODO: error
  } else if ((addr0_type & ISSIGNED) || (addr1_type & ISSIGNED)) {
    opmod = 0;
    retaddr_type = addr0_type & 0xf;
    if(addr1_type & (0xf > retaddr_type))
      retaddr_type = addr1_type & 0xf;
    retaddr_type |= ISSIGNED;
    if(addr0_type & ISCONST) {
      if(addr1_type & ISCONST) {
        retaddr.iregnum = prog->iregcnt++;
      } else {
        retaddr = addr1;
      }
    } else {
      retaddr.iregnum = addr0.iregnum;
    }
  } else {
    opmod = 0;
    retaddr_type = addr0_type & 0xf;
    if(addr1_type & (0xf > retaddr_type))
      retaddr_type= addr1_type & 0xf;
    if(addr0_type & ISCONST) {
      if(addr1_type & ISCONST) {
        retaddr.iregnum = prog->iregcnt++;
      } else {
        retaddr.iregnum = addr1.iregnum;
      }
    } else {
      retaddr.iregnum = addr0.iregnum;
    }
  }
  
  return ct_3ac_op3(opcode_unsigned + opmod, addr0_type, addr0, addr1_type, addr1, retaddr_type, retaddr);
}

OPERATION* implicit_nocoerce_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  return nocoerce_3ac_3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, prog);
  //maybe this need not be seperate
}

FULLADDR implicit_shortcircuit_3(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, ADDRESS complete_val, ADDRESS shortcircuit_val, PROGRAM* prog) {
  ADDRESS doneaddr, afterdoneaddr;
  doneaddr.labelname = proglabel(prog);
  afterdoneaddr.labelname = proglabel(prog);
  FULLADDR addr2use;
  for(int i = 0; i < cexpr->params->length; i++) {
    addr2use = linearitree(daget(cexpr->params, i), prog);
    dapush(prog->ops, ct_3ac_op2(op_to_cmp, addr2use.addr_type, addr2use.addr, ISLABEL | ISCONST, doneaddr));
  }
  if(addr2use.addr_type & ISCONST || addr2use.addr_type & ISFLOAT) {
    addr2use.addr.iregnum = prog->iregcnt++; //do it more intelligently here
  }
  addr2use.addr_type = 1;//TODO: maybe make it signed?
  //perhaps we can make the last one a flat assignment rather than a jump and garbage?
  dapush(prog->ops, ct_3ac_op2(MOV_3, ISCONST, complete_val, addr2use.addr_type, addr2use.addr));
  dapush(prog->ops, ct_3ac_op1(JMP_3, ISLABEL | ISCONST, afterdoneaddr));
  dapush(prog->ops, ct_3ac_op1(LBL_3, ISLABEL | ISCONST, doneaddr));
  dapush(prog->ops, ct_3ac_op2(MOV_3, ISCONST, shortcircuit_val, addr2use.addr_type, addr2use.addr));
  dapush(prog->ops, ct_3ac_op1(LBL_3, ISLABEL | ISCONST, afterdoneaddr));
  return addr2use;
}

OPERATION* cmpret_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR otheraddr = linearitree(daget(cexpr->params, 1), prog);
  OPERATION* retop = implicit_3ac_3(opcode_unsigned, curaddr.addr_type, curaddr.addr, otheraddr.addr_type, otheraddr.addr, prog);
  if(retop->dest_type & ISFLOAT) {
    retop->dest_type = curaddr.addr_type & 0xf;
    if(retop->dest_type < (otheraddr.addr_type & 0xf)) {
      retop->dest_type = otheraddr.addr_type & 0xf;
    }
    retop->dest_type |= curaddr.addr_type & otheraddr.addr_type & ISSIGNED;
    if(curaddr.addr_type & ISFLOAT || curaddr.addr_type & ISCONST) {
      if(otheraddr.addr_type & ISFLOAT || otheraddr.addr_type & ISCONST) {
        retop->dest.iregnum = prog->iregcnt++;
      } else {
        retop->dest = otheraddr.addr;
      }
    } else {
      retop->dest = curaddr.addr;
    }
  }
  return retop;
}

OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  //check for no floats?
  enum opcode_3ac shlop = opcode_unsigned + (a1.addr_type & ISSIGNED ? 1 : 0);
  ADDRESS adr;
  if(a1.addr_type & ISCONST) {
    if(a2.addr_type & ISCONST) {
      adr.iregnum = prog->iregcnt++;
    } else {
      adr = a2.addr;
    }
  } else {
    adr = a1.addr;
  }
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
      if(cexpr->id->index == -1) {
        //global
        //do something with 
      } else {
        return *(FULLADDR*) fixedsearch(prog->fixedvars, cexpr->id->index);
      }
      break;
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
      //TODO: validate lots of types
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = (curaddr.addr_type & ~(ISCONST | 0xf)) | 1;
      //logical not only makes sense for ints
      otheraddr.addr.uintconst_64 = 0.0;
      destaddr.addr = curaddr.addr;
      dapush(prog->ops, ct_3ac_op3(EQ_U, curaddr.addr_type, curaddr.addr, curaddr.addr_type, otheraddr.addr,
                                   destaddr.addr_type, destaddr.addr));
      return destaddr;
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
      return op2ret(prog->ops, implicit_unary_2(ADDR_U, cexpr, prog));

    case DEREF: //Turn deref of addition, subtraction, into array index?
      //TODO: not sure if this is right
      //TODO: actually include lvalue
      return op2ret(prog->ops, implicit_unary_2(MFP_U, cexpr, prog));

    case ADD:
      return op2ret(prog->ops, implicit_binary_3(ADD_U, cexpr, prog));
    case SUB: 
      return op2ret(prog->ops, implicit_binary_3(SUB_U, cexpr, prog));
    case MULT:
      return op2ret(prog->ops, implicit_binary_3(MULT_U, cexpr, prog));
    case DIVI: 
      return op2ret(prog->ops, implicit_binary_3(DIV_U, cexpr, prog));

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
      return op2ret(prog->ops, implicit_binary_3(AND_U, cexpr, prog));
    case B_OR:
      return op2ret(prog->ops, implicit_binary_3(OR_U, cexpr, prog));
    case B_XOR:
      return op2ret(prog->ops, implicit_binary_3(XOR_U, cexpr, prog));

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
      destaddr.addr.uintconst_64 = curaddr.addr_type & 0xf;
      destaddr.addr_type = ISCONST;
      return destaddr;
    case CAST:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      if(cexpr->vartype->pointerstack && cexpr->vartype->pointerstack->length) {
        break;
        //move to unsigned int reg, make 64 bit, anything else?
      } else if(cexpr->vartype->tb & INT) {
        destaddr.addr_type = (cexpr->vartype->tb & 0xf) | ISSIGNED;
        destaddr.addr.iregnum = prog->iregcnt++;
        if(curaddr.addr_type & !(ISFLOAT | ISLABEL | ISSTRCONST | 0xf)) {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(FLOAT_TO_INT, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          break;
        }
        return destaddr;
      } else if(cexpr->vartype->tb & UINT) {
        destaddr.addr_type = cexpr->vartype->tb & 0xf;
        destaddr.addr.iregnum = prog->iregcnt++;
        if(curaddr.addr_type & !(ISFLOAT | ISLABEL | ISSTRCONST | 0xf)) {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(FLOAT_TO_INT, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          break;
        }
        return destaddr;
      } else if(cexpr->vartype->tb & FLOAT) {
        destaddr.addr_type = (cexpr->vartype->tb & 0xf) | ISSIGNED | ISFLOAT;
        destaddr.addr.fregnum = prog->fregcnt++;
        if(curaddr.addr_type & !(ISFLOAT | ISLABEL | ISSTRCONST | 0xf)) {
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
        if((otheraddr.addr_type & ISFLOAT) && ((curaddr.addr_type & 0xf) < (otheraddr.addr_type & 0xf)))
          destaddr.addr_type = (otheraddr.addr_type & 0xf);
        else
          destaddr.addr_type = (curaddr.addr_type & 0xf);
        destaddr.addr_type |= ISFLOAT | ISSIGNED;
        destaddr.addr.fregnum = prog->fregcnt++;
      } else if(otheraddr.addr_type & ISFLOAT) {
        destaddr.addr_type = ISFLOAT | ISSIGNED | (otheraddr.addr_type & 0xf);
        destaddr.addr.fregnum = prog->fregcnt++;
      } else {
        if((curaddr.addr_type & 0xf) < (otheraddr.addr_type & 0xf))
          destaddr.addr_type = otheraddr.addr_type & 0xf;
        else
          destaddr.addr_type = otheraddr.addr_type & 0xf;
        destaddr.addr_type |= (curaddr.addr_type & otheraddr.addr_type & ISSIGNED);
        destaddr.addr.iregnum = prog->iregcnt++;
      }
      destaddr.addr = curaddr.addr;
      *fixlater = *ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr);
      dapush(prog->ops, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) scndlbl));
      return destaddr;
      //confirm 2 addrs have same type or are coercible

    case ASSIGN:
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      //implicit type coercion needed
      dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;
    case PREINC:
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      //implicit type coercion needed
      dapush(prog->ops, ct_3ac_op2(INC_U, destaddr.addr_type, destaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;
    case PREDEC:
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      //implicit type coercion needed
      dapush(prog->ops, ct_3ac_op2(INC_U, destaddr.addr_type, destaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;
     case POSTINC: case POSTDEC:
       //scratch register returned, but type coercion stupid and unclear
       //confirm argument is lvalue?
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
       //confirm argument is lvalue?
       //type coercion stupid and unclear
      break;
    case NOP: case MEMBER:
      break;
    case SZOF:;
      IDTYPE idt = typex(daget(cexpr->params, 0));
      destaddr.addr_type = ISCONST | 8;
      if(idt.pointerstack && idt.pointerstack->length) {
        destaddr.addr.intconst_64 = 8;
      } else {
        if(idt.tb & STRUCTVAL) {
          destaddr.addr.intconst_64 = idt.structtype->size;
        } else if(idt.tb & UNIONVAL) {
          destaddr.addr.intconst_64 = idt.uniontype->size;
        } else {
          destaddr.addr.intconst_64 = idt.tb & 0xf;
        }
      }
      return destaddr;
    case FCALL: ;
      DYNARR* params = dactor(cexpr->params->length);
      EXPRESSION* fname = daget(cexpr->params, 0);
      for(int i = 1; i < cexpr->params->length; ++i) {
        curaddr = linearitree(daget(cexpr->params, i), prog);
        dapush(params, ct_3ac_op1(PARAM_3, curaddr.addr_type, curaddr.addr));
      }
      damerge(prog->ops, params);
      IDTYPE* frettype = cexpr->rettype;
      //struct type as well?
      if(frettype->pointerstack && frettype->pointerstack->length) {
        destaddr.addr_type = ISPOINTER | 8;
        destaddr.addr.iregnum = prog->iregcnt++;
      } else if(frettype->tb & FLOATNUM) {
        destaddr.addr_type = ISFLOAT | (frettype->tb & 0xf);
        destaddr.addr.fregnum = prog->fregcnt++;
      } else if(frettype->tb & UNSIGNEDNUM) {
        destaddr.addr_type = (frettype->tb & 0xf);
        destaddr.addr.iregnum = prog->iregcnt++;
      } else {
        destaddr.addr_type = ISSIGNED | (frettype->tb & 0xf);
        destaddr.addr.iregnum = prog->iregcnt++;
      }
      dapush(prog->ops, ct_3ac_op2(CALL_3, ISCONST | ISLABEL, (ADDRESS) fname->id->name, destaddr.addr_type, destaddr.addr));
      return destaddr;
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

void initializestate(INITIALIZER* i, PROGRAM* prog) {
  FULLADDR* newa = malloc(sizeof(FULLADDR));
  newa->addr_type = addrconv(i->decl->type);//addrtype should be determined from initializer type, helper function
  if(newa->addr_type & ISFLOAT) {
    newa->addr.iregnum = prog->iregcnt++;
  } else {
    newa->addr.fregnum = prog->fregcnt++;
  }
  dapush(prog->ops, ct_3ac_op1(INIT_3, newa->addr_type, newa->addr));
  if(i->expr) {
    FULLADDR lastemp = linearitree(i->expr, prog);
    if((lastemp.addr_type & ISFLOAT) && !(newa->addr_type & ISFLOAT)) {
      dapush(prog->ops, ct_3ac_op2(FLOAT_TO_INT, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    } else if(!(lastemp.addr_type & ISFLOAT) && (newa->addr_type & ISFLOAT)) {
      dapush(prog->ops, ct_3ac_op2(INT_TO_FLOAT, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    } else {
      //force float conversion in mov if necessary?
      dapush(prog->ops, ct_3ac_op2(MOV_3, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    }
  }
  fixedinsert(prog->fixedvars, i->decl->varid, newa);
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
      //check cases, if they're all within 1024 of each other, construct jump table, else make if else with jumps, current solution
      brklabel.labelname = proglabel(prog);
      FULLADDR fad = linearitree(cst->cond, prog);
      dapush(prog->breaklabels, brklabel.labelname);
      DYNARR* cll = cst->labeltable->da;
      HASHTABLE* htl = cst->labeltable->ht;
      for(int i = 0; i < cll->length; i++) {
        ADDRESS cadre, padre;
        cadre.intconst_64 = (long) daget(cll, i);
        padre.labelname = fixedsearch(htl, cadre.intconst_64);
        //maybe signed is unnecessary
        dapush(prog->ops, ct_3ac_op3(BEQ_I, fad.addr_type, fad.addr, ISCONST | ISSIGNED, cadre,
                                     ISCONST | ISLABEL, padre));
      }
      if(cst->defaultlbl) {
        ADDRESS deflbl;
        deflbl.labelname = cst->defaultlbl;
        dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, deflbl));
      } else {
        dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, brklabel));
      }
      solidstate(cst->body, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      break;
    case LABEL:
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) cst->glabel));
      return;
    case CMPND: 
      //probably more stack stuff will need to be done here?
      for(int i = 0; i < cst->stmtsandinits->length; i++) {
        SOI* s = (SOI*) daget(cst->stmtsandinits, i);
        if(s->isstmt) {
          solidstate(s->state, prog);
        } else {
          for(int j = 0; j < s->init->length; j++) {
            INITIALIZER* in = daget(s->init, j);
            initializestate(in, prog);
          }
        }
        
      }
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

void linefunc(FUNC* f) {
  PROGRAM* prog = calloc(sizeof(PROGRAM), 1);
  prog->ops = dactor(1024);
  prog->breaklabels = dactor(8);
  prog->continuelabels = dactor(8);
  prog->fixedvars = htctor();
  //initialize params
  for(int i = 0; i < f->params->da->length; i++) {
  }
  solidstate(f->body, prog);
}

static void printaddr(ADDRESS addr, ADDRTYPE addr_type) {
  if(addr_type & ISLABEL) {
    printf("<<%s>>", addr.labelname);
  } else if(addr_type & ISCONST) {
    if(addr_type & ISSTRCONST) 
      printf("WIP");
    else if(addr_type & ISFLOAT) 
      printf("%lf", addr.floatconst_64);
    else if(addr_type & ISSIGNED) 
      printf("%ld", addr.intconst_64);
    else
      printf("%lu", addr.intconst_64);
  } else {
    if(addr_type & ISFLOAT) 
      printf("freg%lu", addr.fregnum);
    else if(addr_type & ISSIGNED) 
      printf("ireg%lu", addr.iregnum);
  }
}

#define PRINTOP3(opsymb) do { \
    printf("\t"); \
    printaddr(op->addr0, op->addr0_type); \
    printf(" " #opsymb " "); \
    printaddr(op->addr1, op->addr1_type); \
    printf(" →  "); \
    printaddr(op->dest, op->dest_type); \
  } while(0)

#define PRINTOP2(opsymb) do { \
    printf("\t"); \
    printf(#opsymb " "); \
    printaddr(op->addr0, op->addr0_type); \
    printf(" →  "); \
    printaddr(op->dest, op->dest_type); \
  } while(0)

#define PRINTOP1() do { \
    printf("\t"); \
    printaddr(op->addr0, op->addr0_type); \
  } while(0)


void printprog(PROGRAM* prog) {
  DYNARR* pd = prog->ops;
  for(int i = 0; i < pd->length; i++) {
    OPERATION* op = daget(pd, i);
    printf("%s", opcode_3ac_names[op->opcode]);
    switch(op->opcode) {
      case NOP_3:
        break;
      case LBL_3: 
        printf("%s:", op->addr1.labelname);
        break;
      case ADD_U: case ADD_I: case ADD_F: 
        PRINTOP3(+);
        break;
      case SUB_U: case SUB_I: case SUB_F: 
        PRINTOP3(-);
        break;
      case MULT_U: case MULT_I: case MULT_F: 
        PRINTOP3(*);
        break;
      case DIV_U: case DIV_I: case DIV_F: 
        PRINTOP3(/);
        break;
      case MOD_U: case MOD_I: 
        PRINTOP3(%%);
        break;
      case SHL_U: case SHL_I: 
        PRINTOP3(>>);
        break;
      case SHR_U: case SHR_I: 
        PRINTOP3(<<);
        break;
      case AND_U: case AND_F: 
        PRINTOP3(&);
        break;
      case OR_U: case OR_F: 
        PRINTOP3(|);
        break;
      case XOR_U: case XOR_F: 
        PRINTOP3(^);
        break;
      case NOT_U: case NOT_F: 
        PRINTOP2(~);
        break;
      case INC_U: case INC_I: case INC_F: 
        PRINTOP2(++);
        break;
      case DEC_U: case DEC_I: case DEC_F: 
        PRINTOP2(--);
        break;
      case NEG_I: case NEG_F: 
        PRINTOP2(-);
        break;
      case ADDR_U: case ADDR_I: case ADDR_F: /*not sure if I is needed*/
        PRINTOP2(&);
        break;
      case EQ_U: case EQ_I: case EQ_F: 
        PRINTOP3(==);
        break;
      case NE_U: case NE_I: case NE_F: 
        PRINTOP3(!=);
        break;
      case GE_U: case GE_I: case GE_F: 
        PRINTOP3(>=);
        break;
      case LE_U: case LE_I: case LE_F: 
        PRINTOP3(<=);
        break;
      case GT_U: case GT_I: case GT_F: 
        PRINTOP3(>);
        break;
      case LT_U: case LT_I: case LT_F: 
        PRINTOP3(<);
        break;
      case BEQ_U: case BEQ_I: case BEQ_F: 
      case BNE_U: case BNE_I: case BNE_F: 
      case BGE_U: case BGE_I: case BGE_F: 
      case BLE_U: case BLE_I: case BLE_F: 
      case BGT_U: case BGT_I: case BGT_F: 
      case BLT_U: case BLT_I: case BLT_F: 
      case BNZ_3: case BEZ_3: 
      case JMP_3: 
        PRINTOP1();
        break;
      case MOV_3: 
        PRINTOP2( );
        break;
      case MTP_U: case MTP_I: case MTP_F:
      case MFP_U: case MFP_I: case MFP_F:
        PRINTOP2( ); //perhaps use deref later, not vital
        break;
      case PARAM_3: case CALL_3: case RETURN_3: 
        PRINTOP1();
        break;
      case FLOAT_TO_INT: case INT_TO_FLOAT: 
        PRINTOP2( ); //perhaps use cast later, not vital
        break;
      case ARRAY_INDEX:
      case ARRAY_OFFSET: 
      case INIT_3:
        break;
    }
    putchar('\n');
  }
  return;
}

char remove_nops(PROGRAM* prog) {
  DYNARR* da = prog->ops;
  OPERATION** adr = (OPERATION**) da->arr;
  int newlen = 0;
  for(int i = 0; i < da->length; i++) {
    OPERATION* cad = adr[i];
    if(cad->opcode != NOP_3) {
      adr[newlen] = cad;
      newlen++;
    }
  }
  int prevlen = da->length;
  da->length = newlen;
  return prevlen == newlen;
}
