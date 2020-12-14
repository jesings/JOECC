#include <assert.h>
#include "3ac.h"
#define X(s) #s
const char* opcode_3ac_names[] = {
  OPS_3AC
};
extern const char* name_EXPRTYPE[];
extern const char* name_STMTTYPE[];
#undef X
#define FILLIREG(addrvar, type) do { \
    (addrvar).addr.iregnum = prog->iregcnt++; \
    (addrvar).addr_type = (type); \
  } while(0)
#define FILLFREG(addrvar, type) do { \
    (addrvar).addr.fregnum = prog->fregcnt++; \
    (addrvar).addr_type = (type); \
  } while(0)
#define FILLGREG(addrvar, type) do { \
    if((type) & ISFLOAT) FILLFREG((addrvar), (type)); else FILLIREG((addrvar), (type));} while(0)

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

OPERATION* implicit_binary_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE arg2id = typex(daget(cexpr->params, 1));
  IDTYPE retid = typex(cexpr);
  FULLADDR desta;
  if(retid.pointerstack && retid.pointerstack->length) {
    if((arg1id.pointerstack && arg1id.pointerstack->length) &&
       !(arg2id.pointerstack && arg2id.pointerstack->length)) {
      a2 = ptarith(retid, a2, prog);
    } else if((arg2id.pointerstack && arg2id.pointerstack->length) && 
              !(arg1id.pointerstack && arg1id.pointerstack->length)) {
      a1 = ptarith(retid, a1, prog);
    }
    FILLIREG(desta, ISPOINTER | 8);
    //perhaps save regnum here?
  } else if(retid.tb & FLOATNUM) {
    op += 2;
    if(!(arg1id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLFREG(fad, ISFLOAT | (retid.tb & 0xf));
      dapush(prog->ops, ct_3ac_op2(I2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    } 
    if(!(arg2id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLFREG(fad, ISFLOAT | (retid.tb & 0xf));
      dapush(prog->ops, ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    }
    FILLFREG(desta, ISFLOAT | (retid.tb & 0xf));
  } else if(retid.tb & UNSIGNEDNUM) {
    FILLIREG(desta, retid.tb & 0xf);
  } else {
    op += 1;
    FILLIREG(desta, (retid.tb & 0xf) | ISSIGNED);
  }

  return ct_3ac_op3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, desta.addr_type, desta.addr);
}

FULLADDR cmpnd_assign(enum opcode_3ac op, EXPRESSION* destexpr, EXPRESSION* srcexpr, PROGRAM* prog) {
  IDTYPE destidt = typex(destexpr);
  IDTYPE srcidt = typex(srcexpr);
  FULLADDR srcaddr = linearitree(srcexpr, prog);
  FULLADDR destaddr = linearitree(destexpr, prog);
  //do some implicit binary stuff
  if(destidt.pointerstack && destidt.pointerstack->length) {
    if(srcidt.pointerstack && srcidt.pointerstack->length) {
      switch(op) {
        case ADD_U: case SUB_U: case AND_U: case OR_U: case XOR_U:
          break;
        case SHL_U: case SHR_U: case MOD_U: case MULT_U: case DIV_U:
          //pointer to integer without cast
        default:
          assert(0);
      }
    } else {
      switch(op) {
        case ADD_U: case SUB_U:
          srcaddr = ptarith(destidt, srcaddr, prog);
          break;
        default:
          //integer to pointer w/o cast
          assert(0);
      }
    }
  } else {
    assert(!(srcidt.pointerstack && srcidt.pointerstack->length));
    if(destidt.tb & ISFLOAT) {
      op += 2;
      if(!(srcidt.tb & ISFLOAT)) {
        FULLADDR fad;
        FILLFREG(fad, ISFLOAT | (srcidt.tb & 0xf));
        dapush(prog->ops, ct_3ac_op2(I2F, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        srcaddr = fad;
      }
    } else if(srcidt.tb & ISFLOAT) {
        FULLADDR fad;
        FILLFREG(fad, ISFLOAT | (srcidt.tb & 0xf));
        dapush(prog->ops, ct_3ac_op2(I2F, destaddr.addr_type, destaddr.addr, fad.addr_type, fad.addr));
        dapush(prog->ops, ct_3ac_op3(op, fad.addr_type, fad.addr, srcaddr.addr_type, 
                                     srcaddr.addr, fad.addr_type, fad.addr));
        dapush(prog->ops, ct_3ac_op2(F2I, fad.addr_type, fad.addr, destaddr.addr_type, destaddr.addr));
        return destaddr;
    } else {
        if(destidt.tb & ISSIGNED || srcidt.tb & ISSIGNED) 
          op += 1;
    }
  }
  dapush(prog->ops, ct_3ac_op3(op, destaddr.addr_type, destaddr.addr, srcaddr.addr_type, 
                               srcaddr.addr, destaddr.addr_type, destaddr.addr));
  return destaddr;
}

static FULLADDR prestep(char isinc, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR destaddr, otheraddr;
  IDTYPE rid = typex(cexpr);
  prog->lval = 1;
  prog->fderef = 0;
  destaddr = linearitree(daget(cexpr->params, 0), prog);
  char baseness = destaddr.addr_type & ISFLOAT ? 2 : 0;
  if(prog->fderef) {
    FILLGREG(otheraddr, destaddr.addr_type & ~ISLABEL);
    if(rid.pointerstack && rid.pointerstack->length) {
      FULLADDR curaddr;
      rid.pointerstack->length -= 1;
      curaddr.addr.uintconst_64= lentype(&rid);
      rid.pointerstack->length += 1;
      dapush(prog->ops, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, otheraddr.addr_type | ISDEREF , otheraddr.addr, ISCONST | 8, curaddr.addr, otheraddr.addr_type | ISDEREF, otheraddr.addr));
    } else {
      dapush(prog->ops, ct_3ac_op2((isinc ? INC_U : DEC_U) + baseness, otheraddr.addr_type | ISDEREF , otheraddr.addr, otheraddr.addr_type | ISDEREF, otheraddr.addr));
    }
  } else {
    if(rid.pointerstack && rid.pointerstack->length) {
      FULLADDR curaddr;
      rid.pointerstack->length -= 1;
      curaddr.addr.uintconst_64 = lentype(&rid);
      rid.pointerstack->length += 1;
      dapush(prog->ops, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, destaddr.addr_type, destaddr.addr, ISCONST | 8, curaddr.addr, destaddr.addr_type, destaddr.addr));
    } else {
      dapush(prog->ops, ct_3ac_op2((isinc ? INC_U : DEC_U) + baseness, destaddr.addr_type, destaddr.addr, destaddr.addr_type, destaddr.addr));
    }
  }
  return destaddr;
}
static FULLADDR poststep(char isinc, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR destaddr, otheraddr, actualaddr;
  IDTYPE rid = typex(cexpr);
  prog->lval = 1;
  prog->fderef = 0;
  destaddr = linearitree(daget(cexpr->params, 0), prog);
  char baseness = destaddr.addr_type & ISFLOAT ? 2 : 0;
  FILLGREG(actualaddr, destaddr.addr_type & ~ISLABEL);
  if(prog->fderef) {
    FILLGREG(otheraddr, destaddr.addr_type & ~ISLABEL);
    dapush(prog->ops, ct_3ac_op2(MOV_3, destaddr.addr_type | ISDEREF, destaddr.addr, otheraddr.addr_type, otheraddr.addr));
    dapush(prog->ops, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, actualaddr.addr_type, actualaddr.addr));
    if(rid.pointerstack && rid.pointerstack->length) {
      FULLADDR curaddr;
      rid.pointerstack->length -= 1;
      curaddr.addr.uintconst_64 = lentype(&rid);
      rid.pointerstack->length += 1;
      dapush(prog->ops, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, otheraddr.addr_type, otheraddr.addr, ISCONST | 8, curaddr.addr, destaddr.addr_type | ISDEREF, destaddr.addr));
    } else {
      dapush(prog->ops, ct_3ac_op2((isinc ? INC_U : DEC_U) + baseness, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type | ISDEREF, destaddr.addr));
    }
  } else {
    dapush(prog->ops, ct_3ac_op2(MOV_3, destaddr.addr_type, destaddr.addr, actualaddr.addr_type, actualaddr.addr));
    if(rid.pointerstack && rid.pointerstack->length) {
      FULLADDR curaddr;
      rid.pointerstack->length -= 1;
      curaddr.addr.uintconst_64 = lentype(&rid);
      rid.pointerstack->length += 1;
      dapush(prog->ops, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, destaddr.addr_type, destaddr.addr, ISCONST | 8, curaddr.addr, destaddr.addr_type, destaddr.addr));
    } else {
      dapush(prog->ops, ct_3ac_op2((isinc ? INC_U : DEC_U) + baseness, destaddr.addr_type, destaddr.addr, destaddr.addr_type, destaddr.addr));
    }
  }
  return actualaddr;
}

OPERATION* implicit_mtp_2(EXPRESSION* destexpr, EXPRESSION* fromexpr, FULLADDR a1, FULLADDR a2, PROGRAM* prog) {
  IDTYPE destidt = typex(destexpr);
  IDTYPE srcidt = typex(fromexpr);
  if(destidt.pointerstack && destidt.pointerstack->length) {
    assert(srcidt.pointerstack && srcidt.pointerstack->length);
  } else if(destidt.tb & FLOATNUM) {
    if(!(srcidt.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLFREG(fad, ISFLOAT | (destidt.tb & 0xf));
      return ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type | ISDEREF, fad.addr);
    }
  } else if(destidt.tb & UNSIGNEDNUM) {
    if(srcidt.tb & FLOATNUM) {
      FULLADDR fad;
      FILLIREG(fad, destidt.tb & 0xf);
      return ct_3ac_op2(F2I, a2.addr_type, a2.addr, fad.addr_type | ISDEREF, fad.addr);
    }
  } else {
    if(srcidt.tb & FLOATNUM) {
      FULLADDR fad;
      FILLIREG(fad, (destidt.tb & 0xf) | ISSIGNED);
      return ct_3ac_op2(F2I, a2.addr_type, a2.addr, fad.addr_type | ISDEREF, fad.addr);
    }
  }
  return ct_3ac_op2(MOV_3, a2.addr_type, a2.addr, a1.addr_type | ISDEREF, a1.addr);
}

OPERATION* implicit_unary_2(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE retid = typex(cexpr);
  FULLADDR desta;
  if(retid.pointerstack && retid.pointerstack->length) {
    FILLIREG(desta, ISPOINTER | 8);
    //perhaps save regnum here?
  } else if(retid.tb & FLOATNUM) {
    op += 2;
    if(!(arg1id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLFREG(fad, ISFLOAT | (retid.tb & 0xf));
      dapush(prog->ops, ct_3ac_op2(I2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    } 
    FILLFREG(desta, ISFLOAT | (retid.tb & 0xf));
  } else if(retid.tb & UNSIGNEDNUM) {
    FILLIREG(desta, retid.tb & 0xf);
  } else {
    op += 1;
    FILLIREG(desta, (retid.tb & 0xf) | ISSIGNED);
  }

  return ct_3ac_op2(op, a1.addr_type, a1.addr, desta.addr_type, desta.addr);
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
    addr2use.addr.iregnum = prog->iregcnt++;
  }
  addr2use.addr_type = 1;//maybe make it signed?
  dapush(prog->ops, ct_3ac_op2(MOV_3, ISCONST, complete_val, addr2use.addr_type, addr2use.addr));
  dapush(prog->ops, ct_3ac_op1(JMP_3, ISLABEL | ISCONST, afterdoneaddr));
  intsert(prog->labeloffsets, doneaddr.labelname, prog->ops->length);
  dapush(prog->ops, ct_3ac_op1(LBL_3, ISLABEL | ISCONST, doneaddr));
  dapush(prog->ops, ct_3ac_op2(MOV_3, ISCONST, shortcircuit_val, addr2use.addr_type, addr2use.addr));
  intsert(prog->labeloffsets, afterdoneaddr.labelname, prog->ops->length);
  dapush(prog->ops, ct_3ac_op1(LBL_3, ISLABEL | ISCONST, afterdoneaddr));
  return addr2use;
}

OPERATION* cmpret_binary_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE arg2id = typex(daget(cexpr->params, 1));
  IDTYPE retid = typex(cexpr);
  FULLADDR desta;
  FILLIREG(desta, (retid.tb & 0xf) | (retid.tb & UNSIGNEDNUM ? 0 : ISSIGNED));//unsigned
  if(arg1id.tb & FLOATNUM) {
    op += 2;
    if(!(arg2id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLFREG(fad, ISFLOAT | (arg1id.tb & 0xf));
      dapush(prog->ops, ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    }
  } else if(arg2id.tb & FLOATNUM) {
    op += 2;
    FULLADDR fad;
    FILLFREG(fad, ISFLOAT | (arg2id.tb & 0xf));
    dapush(prog->ops, ct_3ac_op2(I2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
    a2 = fad;
  } else if(!((arg1id.tb & UNSIGNEDNUM) || (arg2id.tb & UNSIGNEDNUM))) {
    op += 1;
  }

  return ct_3ac_op3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, desta.addr_type, desta.addr);
}

OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  //check for no floats?
  enum opcode_3ac shlop = opcode_unsigned + (a1.addr_type & ISSIGNED ? 1 : 0);
  FULLADDR adr;
  FILLIREG(adr, a1.addr_type & ~ISCONST);
  return ct_3ac_op3(shlop, a1.addr_type, a1.addr, a2.addr_type, a2.addr, adr.addr_type, adr.addr);
}

FULLADDR smemrec(EXPRESSION* cexpr, PROGRAM* prog, char lvalval) {
  FULLADDR sead = linearitree(daget(cexpr->params, 0), prog);
  IDTYPE seaty = typex(daget(cexpr->params, 0));
  IDTYPE retty = typex(cexpr);
  assert(((EXPRESSION*) daget(cexpr->params, 1))->type == MEMBER);
  char* memname = ((EXPRESSION*) daget(cexpr->params, 1))->member;
  assert(!seaty.pointerstack || seaty.pointerstack->length <= 1);
  assert(seaty.tb & (STRUCTVAL | UNIONVAL));
  if(seaty.tb & STRUCTVAL)
    feedstruct(seaty.structtype);
  else
    unionlen(seaty.uniontype);
  FULLADDR retaddr;
  ADDRESS offaddr;
  HASHTABLE* ofs = seaty.structtype->offsets;
  STRUCTFIELD* sf = search(ofs, memname);
  char pointerqual = sf->type->pointerstack && sf->type->pointerstack->length;
  offaddr.intconst_64 = sf->offset;
  if(!pointerqual && (sf->type->tb & (STRUCTVAL | UNIONVAL))) {
    FILLIREG(retaddr, ISPOINTER | 8);
    if(offaddr.intconst_64) {
      dapush(prog->ops, ct_3ac_op3(ADD_U, sead.addr_type, sead.addr, ISCONST, offaddr, retaddr.addr_type, retaddr.addr));
      //no fderef? special treatment for lvalval?
    } else {
      return sead; //no fderef? special treatment for lvalval?
    }
  } else {
    FULLADDR intermediate;
    if(offaddr.intconst_64) {
      FILLIREG(intermediate, ISPOINTER | 8);
      dapush(prog->ops, ct_3ac_op3(ADD_U, sead.addr_type, sead.addr, ISCONST, offaddr, intermediate.addr_type, intermediate.addr));
    } else {
      intermediate = sead;
    }
    if(lvalval) {
      prog->fderef = 1;
      return intermediate; //probably nothing different needs to be done with pointer or anything
    }
    FILLGREG(retaddr, addrconv(&retty));
    dapush(prog->ops, ct_3ac_op2(MOV_3, intermediate.addr_type | ISDEREF, intermediate.addr, retaddr.addr_type, retaddr.addr));
  }
  //need to handle lvalues
  return retaddr;
}

FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr, otheraddr, destaddr;
  ADDRESS initlbl, scndlbl;
  IDTYPE varty;
  char prevval = prog->lval;
  prog->lval = 0;

  switch(cexpr->type){
    case STRING:
      curaddr.addr_type = ISCONST | ISSTRCONST | ISPOINTER | 0x8;
      curaddr.addr.strconst = cexpr->strconst;
      return curaddr;
    case INT:
      curaddr.addr_type = ISCONST | ISSIGNED | 0x8;
      curaddr.addr.intconst_64 = cexpr->intconst;
      return curaddr;
    case UINT: 
      curaddr.addr_type = ISCONST | 0x8;
      curaddr.addr.uintconst_64 = cexpr->uintconst;
      return curaddr;
    case FLOAT:
      curaddr.addr_type = ISCONST | ISFLOAT | ISSIGNED | 0x8;
      curaddr.addr.floatconst_64 = cexpr->floatconst;
      return curaddr;
    case IDENT:
      if(cexpr->id->index < 0) {
        if(cexpr->id->index == -2) {
          destaddr.addr_type = 8;
        } else {
          destaddr.addr_type = addrconv(cexpr->id->type);
        }
        curaddr.addr_type = destaddr.addr_type | ISLABEL;
        curaddr.addr.labelname = cexpr->id->name;
        if(prevval | prog->fderef) {
          return curaddr;
        } else {
          FILLGREG(destaddr, destaddr.addr_type);
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
          return destaddr;
        }
      } else {
        return *(FULLADDR*) fixedsearch(prog->fixedvars, cexpr->id->index);
      }
    case ARRAY_LIT: ;
      struct declarator_part* ptrtop = dapeek(cexpr->rettype->pointerstack);
      FILLIREG(destaddr, ISPOINTER | 0x8);
      assert(ptrtop->type == ARRAYSPEC);
      curaddr.addr.uintconst_64 = ptrtop->arrlen;
      dapush(prog->ops, ct_3ac_op2(ALOC_3, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
      if(!ptrtop->arrmaxind) ptrtop->arrmaxind = cexpr->params->length;
      curaddr.addr.uintconst_64 = ptrtop->arrlen / ptrtop->arrmaxind;
      if(curaddr.addr.uintconst_64 < 0xf) {
        for(int i = 0; i < cexpr->params->length; i++) {
          EXPRESSION* dyne = daget(cexpr->params, i);
          otheraddr = linearitree(dyne, prog);
          curaddr.addr.uintconst_64 = i;
          dapush(prog->ops, ct_3ac_op3(ARRMOV, otheraddr.addr_type, otheraddr.addr, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else {
        ADDRESS a;
        a.uintconst_64 = curaddr.addr.uintconst_64;
        for(int i = 0; i < cexpr->params->length; i++) {
          EXPRESSION* dyne = daget(cexpr->params, i);
          otheraddr = linearitree(dyne, prog);
          dapush(prog->ops, ct_3ac_op3(ADD_U, destaddr.addr_type, destaddr.addr, ISCONST | 0x8, a, destaddr.addr_type, destaddr.addr));
          dapush(prog->ops, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type | ISDEREF, destaddr.addr));
        }
      }
      return destaddr;
    case STRUCT_LIT:
      FILLIREG(destaddr, ISPOINTER | 0x8);
      curaddr.addr.uintconst_64 = cexpr->rettype->structtype->size;
      dapush(prog->ops, ct_3ac_op2(ALOC_3, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
      for(int i = 0; i < cexpr->params->length; i++) {
        EXPRESSION* member = daget(cexpr->params, i);
        DECLARATION* decl = daget(cexpr->rettype->structtype->fields, i);
        STRUCTFIELD* sf = search(cexpr->rettype->structtype->offsets, decl->varname);
        curaddr = linearitree(member, prog);
        otheraddr.addr.uintconst_64 = sf->offset;
        dapush(prog->ops, ct_3ac_op3(MTP_OFF, curaddr.addr_type, curaddr.addr, ISCONST | 0x8, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;
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
      FILLIREG(destaddr, (curaddr.addr_type & ~(ISCONST | ISLABEL | 0xf)) | 1);
      //logical not only makes sense for ints
      otheraddr.addr.uintconst_64 = 0;
      dapush(prog->ops, ct_3ac_op3(EQ_U, curaddr.addr_type, curaddr.addr, (curaddr.addr_type & 0xf) | ISCONST, otheraddr.addr,
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
      varty = typex(daget(cexpr->params, 0));
      char hpoints = varty.pointerstack && varty.pointerstack->length;
      if(!(hpoints) && (varty.tb & STRUCTVAL)) {
        return linearitree(daget(cexpr->params, 0), prog);//addr should be a no-op for single pointers to structs
      }
      if(hpoints) {
        struct declarator_part* dclp = dapeek(varty.pointerstack);
        if(dclp->type == ARRAYSPEC)
          return linearitree(daget(cexpr->params, 0), prog);//addr should be a no-op for single pointers to arrays
      }
      return op2ret(prog->ops, implicit_unary_2(ADDR_U, cexpr, prog));

    case DEREF:
      varty = typex(daget(cexpr->params, 0));
      assert(varty.pointerstack && varty.pointerstack->length);
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      if(varty.pointerstack->length == 1 && (varty.tb & STRUCTVAL)) {
        if(prevval) {
          prog->fderef = 1;
        }
        return destaddr; //dereferencing single pointer to struct should be a no-op
      }
      struct declarator_part* dclp = dapeek(varty.pointerstack);
      if(dclp->type == ARRAYSPEC) {
        if(prevval) prog->fderef = 1;
        return destaddr;
      }
      if(prevval) {
        prog->fderef = 1;
        return destaddr;
      }
      varty = typex(cexpr);
      FILLGREG(curaddr, addrconv(&varty));
      return op2ret(prog->ops, ct_3ac_op2(MOV_3, destaddr.addr_type | ISDEREF, destaddr.addr, curaddr.addr_type, curaddr.addr));
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

    case MOD:
      varty = typex(daget(cexpr->params, 0));
      assert(!(varty.tb & FLOATNUM));
      return op2ret(prog->ops, cmpret_binary_3(MOD_U, cexpr, prog));

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

    case DOTOP: 
      varty = typex(daget(cexpr->params, 0));
      assert(!(varty.pointerstack && varty.pointerstack->length));
      return smemrec(cexpr, prog, prevval);
    case ARROW:
      varty = typex(daget(cexpr->params, 0));
      assert(varty.pointerstack && (varty.pointerstack->length == 1));
      return smemrec(cexpr, prog, prevval);
    case SZOFEXPR:
      varty = typex(cexpr);
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr.uintconst_64 = lentype(&varty);
      destaddr.addr_type = ISCONST;
      return destaddr;
    case CAST: //handle identity casts differently
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      if(cexpr->vartype->pointerstack && cexpr->vartype->pointerstack->length) {
        assert(!(curaddr.addr_type & ISFLOAT));
        FILLIREG(destaddr, 8 | ISPOINTER);
        dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else if(cexpr->vartype->tb & FLOATNUM) {
        FILLFREG(destaddr, (cexpr->vartype->tb & 0xf) | ISSIGNED | ISFLOAT);
        if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          dapush(prog->ops, ct_3ac_op2(I2F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else if(cexpr->vartype->tb & UNSIGNEDNUM) {
        FILLIREG(destaddr, cexpr->vartype->tb & 0xf);
        if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(F2I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else if(cexpr->vartype->tb & VOIDNUM) {
        return curaddr; //not sure how this should be handled
      } else if(cexpr->vartype->tb & UNIONVAL) { 
        UNION* castdest = cexpr->vartype->uniontype;
        FILLIREG(destaddr, ISPOINTER | 0x8);
        unionlen(castdest);
        otheraddr.addr.uintconst_64 = castdest->size;
        dapush(prog->ops, ct_3ac_op2(ALOC_3, ISCONST | 8, otheraddr.addr, destaddr.addr_type, destaddr.addr));
        IDTYPE srctype = typex(daget(cexpr->params, 0));
        for(int i = 0; i < castdest->fields->length; i++) {
          DECLARATION* dcl = daget(castdest->fields, i);
          if(!typecompat(dcl->type, &srctype)) continue;
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type | ISDEREF, curaddr.addr, destaddr.addr_type, destaddr.addr));
          return destaddr;
        }
        assert(0);
      } else if(cexpr->vartype->tb & 0xf) {
        FILLIREG(destaddr, (cexpr->vartype->tb & 0xf) | ISSIGNED);
        if(curaddr.addr_type & ISFLOAT) {
          dapush(prog->ops, ct_3ac_op2(F2I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else {
        //don't support casting structs and unions yet
        assert(0);
      }
      return destaddr;
    case TERNARY:
      //do more checking of other 
      initlbl.labelname = proglabel(prog);
      scndlbl.labelname = proglabel(prog);
      dapush(prog->ops, cmptype(daget(cexpr->params, 0), initlbl, 1, prog));
      IDTYPE t0t = typex(daget(cexpr->params, 0));
      IDTYPE t1t = typex(daget(cexpr->params, 1));
      IDTYPE t2t = typex(daget(cexpr->params, 2));
      IDTYPE t3t = typex(cexpr);
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      if(!(t1t.tb & FLOATNUM) && (t2t.tb & FLOATNUM)) {
        FULLADDR ad2;
        FILLFREG(ad2, (t0t.tb & 0xf) | ISFLOAT | ISSIGNED);
        dapush(prog->ops, ct_3ac_op2(I2F, curaddr.addr_type, curaddr.addr, ad2.addr_type, ad2.addr));
        curaddr = ad2;
      }
      //MOV_3 is an op2 but we don't have the second address yet so we leave it as a blank in an op1
      OPERATION* fixlater = ct_3ac_op1(MOV_3, curaddr.addr_type, curaddr.addr);
      dapush(prog->ops, fixlater);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, scndlbl));
      intsert(prog->labeloffsets, initlbl.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, initlbl));
      otheraddr = linearitree(daget(cexpr->params, 2), prog);
      if((t1t.tb & FLOATNUM) && !(t2t.tb & FLOATNUM)) {
        FULLADDR ad2;
        FILLFREG(ad2, (t0t.tb & 0xf) | ISFLOAT | ISSIGNED);
        dapush(prog->ops, ct_3ac_op2(I2F, otheraddr.addr_type, otheraddr.addr, ad2.addr_type, ad2.addr));
        otheraddr = ad2;
      }
      destaddr.addr_type = addrconv(&t3t);
      if(destaddr.addr_type & ISFLOAT) {
        destaddr.addr.fregnum = prog->fregcnt++;
      } else {
        destaddr.addr.iregnum = prog->iregcnt++;
      }
      fixlater->dest_type = destaddr.addr_type;
      fixlater->dest = destaddr.addr;
      dapush(prog->ops, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      intsert(prog->labeloffsets, scndlbl.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) scndlbl));
      return destaddr;
      //confirm 2 addrs have same type or are coercible

    case ASSIGN:
      varty = typex(cexpr);
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      prog->lval = 1;
      prog->fderef = 0;
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      if(!(varty.pointerstack && varty.pointerstack->length) && varty.tb & (STRUCTVAL | UNIONVAL)) {
        feedstruct(varty.structtype);
        otheraddr.addr.uintconst_64 = varty.structtype->size;
        dapush(prog->ops, ct_3ac_op3(COPY_3, curaddr.addr_type, curaddr.addr, ISCONST | 8, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      } else {
        if(prog->fderef) {
          dapush(prog->ops, implicit_mtp_2(daget(cexpr->params, 0), daget(cexpr->params, 1), destaddr, curaddr, prog));
        } else {
          //implicit type coercion needed
          dapush(prog->ops, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      }
      return destaddr;
    case PREINC:
      return prestep(1, cexpr, prog);
    case PREDEC:
      return prestep(0, cexpr, prog);
    case POSTINC:
      return poststep(1, cexpr, prog);
    case POSTDEC:
      return poststep(0, cexpr, prog);
       //confirm argument is lvalue?
    case ADDASSIGN:
      return cmpnd_assign(ADD_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case SUBASSIGN:
      return cmpnd_assign(SUB_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case DIVASSIGN: 
      return cmpnd_assign(DIV_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case SHLASSIGN:
      return cmpnd_assign(SHL_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case SHRASSIGN:
      return cmpnd_assign(SHR_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case ANDASSIGN:
      return cmpnd_assign(AND_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case XORASSIGN:
      return cmpnd_assign(XOR_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case ORASSIGN:
      return cmpnd_assign(OR_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case MULTASSIGN:
      return cmpnd_assign(MULT_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case MODASSIGN:
      return cmpnd_assign(MOD_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
       //confirm argument is lvalue?
       //type coercion stupid and unclear
    case NOP: case MEMBER:
      //unfilled, dummy register, never to be used unless the program is seriously malformed
      destaddr.addr_type = 0;
      destaddr.addr.intconst_64 = -1;
      return destaddr;
    case SZOF:
      varty = *cexpr->vartype;
      destaddr.addr_type = ISCONST | 8;
      if(varty.pointerstack && varty.pointerstack->length) {
        destaddr.addr.intconst_64 = 8;
      } else {
        if(varty.tb & STRUCTVAL) {
          feedstruct(varty.structtype);
          destaddr.addr.intconst_64 = varty.structtype->size;
        } else if(varty.tb & UNIONVAL) {
          unionlen(varty.uniontype);
          destaddr.addr.intconst_64 = varty.uniontype->size;
        } else {
          destaddr.addr.intconst_64 = varty.tb & 0xf;
        }
      }
      return destaddr;
    case FCALL: ;
      DYNARR* params = dactor(cexpr->params->length);
      EXPRESSION* fname = daget(cexpr->params, 0);
      for(int i = 1; i < cexpr->params->length; ++i) {
        //sequence point?
        curaddr = linearitree(daget(cexpr->params, i), prog);
        dapush(params, ct_3ac_op1(ARG_3, curaddr.addr_type, curaddr.addr));
      }
      prog->ops = damerge(prog->ops, params);
      IDTYPE* frettype = cexpr->rettype;
      //struct type as well?
      if(frettype->pointerstack && frettype->pointerstack->length) {
        FILLIREG(destaddr, ISPOINTER | 8);
      } else if(frettype->tb & FLOATNUM) {
        FILLIREG(destaddr, ISFLOAT | (frettype->tb & 0xf));
      } else if(frettype->tb & UNSIGNEDNUM) {
        FILLIREG(destaddr, frettype->tb & 0xf);
      } else {
        FILLIREG(destaddr, ISSIGNED | (frettype->tb & 0xf));
      }
      dapush(prog->ops, ct_3ac_op2(CALL_3, ISCONST | ISLABEL, (ADDRESS) fname->id->name, destaddr.addr_type, destaddr.addr));
      return destaddr;
  }
  fprintf(stderr, "Error: reduction of expression %s to 3 address code failed\n", name_EXPRTYPE[cexpr->type]);
  FILLIREG(curaddr, 0);
  return curaddr;
}

OPERATION* cmptype(EXPRESSION* cmpexpr, ADDRESS addr2jmp, char negate, PROGRAM* prog) {
  OPERATION* dest_op;
  FULLADDR destaddr;
  //check if new register is assigned to in cmpret, decrement?
  switch(cmpexpr->type) {
    case EQ: case NEQ: case GT: case LT: case GTE: case LTE:
      dest_op = cmpret_binary_3(cmp_osite(cmpexpr->type, negate), cmpexpr, prog);//figure out signedness here or elsewhere
      dest_op->dest_type = ISLABEL;
      dest_op->dest = addr2jmp;
      return dest_op;
    case L_NOT:
      destaddr = linearitree(daget(cmpexpr->params, 0), prog);
      return ct_3ac_op2(negate ? BNZ_3 : BEZ_3 , destaddr.addr_type, destaddr.addr, ISLABEL, addr2jmp);
    default:
      destaddr = linearitree(cmpexpr, prog);
      return ct_3ac_op2(negate ? BEZ_3 : BNZ_3 , destaddr.addr_type, destaddr.addr, ISLABEL, addr2jmp);
  }
}

void initializestate(INITIALIZER* i, PROGRAM* prog) {
  FULLADDR* newa = malloc(sizeof(FULLADDR));
  newa->addr_type = addrconv(i->decl->type);//addrtype should be determined from initializer type, helper function
  if(newa->addr_type & ISFLOAT) {
    newa->addr.fregnum = prog->fregcnt++;
  } else {
    newa->addr.iregnum = prog->iregcnt++;
  }
  dapush(prog->ops, ct_3ac_op1(INIT_3, newa->addr_type, newa->addr));
  if(i->expr) {
    FULLADDR lastemp = linearitree(i->expr, prog);
    if((lastemp.addr_type & ISFLOAT) && !(newa->addr_type & ISFLOAT)) {
      dapush(prog->ops, ct_3ac_op2(F2I, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    } else if(!(lastemp.addr_type & ISFLOAT) && (newa->addr_type & ISFLOAT)) {
      dapush(prog->ops, ct_3ac_op2(I2F, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    } else {
      //force float conversion in mov if necessary?
      dapush(prog->ops, ct_3ac_op2(MOV_3, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    }
  }
  fixedinsert(prog->fixedvars, i->decl->varid, newa);
}

void solidstate(STATEMENT* cst, PROGRAM* prog) {
  FULLADDR ret_op;
  ADDRESS contlabel, brklabel;
  switch(cst->type){
    case FRET:
      if(cst->expression) {
        ret_op = linearitree(cst->expression, prog);
        dapush(prog->ops, ct_3ac_op1(RET_3, ret_op.addr_type, ret_op.addr));
      } else {
        dapush(prog->ops, ct_3ac_op0(RET_0));
      }
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
      intsert(prog->labeloffsets, contlabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      dapush(prog->ops, cmptype(cst->cond, brklabel, 1, prog));
      solidstate(cst->body, prog);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, contlabel));
      intsert(prog->labeloffsets, brklabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
    case FORL:
      if(cst->forinit->isE) {
        if(cst->forinit->E->type != NOP) 
          linearitree(cst->forinit->E, prog);
      } else {
        for(int i = 0; i < cst->forinit->I->length; i++) {
          initializestate((INITIALIZER*) daget(cst->forinit->I, i), prog);
        }
      }
      contlabel.labelname = proglabel(prog);
      brklabel.labelname = proglabel(prog);
      ADDRESS toplabel;
      toplabel.labelname = proglabel(prog);
      dapush(prog->continuelabels, contlabel.labelname);
      dapush(prog->breaklabels, brklabel.labelname);
      intsert(prog->labeloffsets, contlabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, toplabel));
      dapush(prog->ops, cmptype(cst->forcond, brklabel, 1, prog));
      solidstate(cst->forbody, prog);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      linearitree(cst->increment, prog);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, toplabel));
      intsert(prog->labeloffsets, brklabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
    case DOWHILEL:
      contlabel.labelname = proglabel(prog);
      brklabel.labelname = proglabel(prog);
      dapush(prog->continuelabels, contlabel.labelname);
      dapush(prog->breaklabels, brklabel.labelname);
      intsert(prog->labeloffsets, contlabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      solidstate(cst->body, prog);
      linearitree(cst->cond, prog);
      dapush(prog->ops, cmptype(cst->cond, contlabel, 0, prog));
      intsert(prog->labeloffsets, brklabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      return;
    case IFS:
      brklabel.labelname = proglabel(prog);
      dapush(prog->ops, cmptype(cst->ifcond, brklabel, 1, prog));
      solidstate(cst->thencond, prog);
      intsert(prog->labeloffsets, brklabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      return;
    case IFELSES:
      contlabel.labelname = proglabel(prog);
      brklabel.labelname = proglabel(prog);
      dapush(prog->ops, cmptype(cst->ifcond, contlabel, 1, prog));
      solidstate(cst->thencond, prog);
      dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, brklabel));
      intsert(prog->labeloffsets, contlabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, contlabel));
      solidstate(cst->elsecond, prog);
      intsert(prog->labeloffsets, brklabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      return;
    case SWITCH:
      //check cases, if they're all within 1024 of each other, construct jump table, else make if else with jumps, current solution
      brklabel.labelname = proglabel(prog);
      FULLADDR fad = linearitree(cst->cond, prog);
      dapush(prog->breaklabels, brklabel.labelname);
      DYNARR* cll = cst->labeltable->da;
      HASHTABLE* htl = cst->labeltable->ht;
      for(int i = 0; i < cll->length; i++) {
        ADDRESS caseval, caselbl;
        caseval.intconst_64 = (long) daget(cll, i);
        caselbl.labelname = fixedsearch(htl, caseval.intconst_64);
        //maybe signed is unnecessary
        dapush(prog->ops, ct_3ac_op3(BEQ_I, fad.addr_type, fad.addr, ISCONST | ISSIGNED, caseval,
                                     ISCONST | ISLABEL, caselbl));
      }
      if(cst->defaultlbl) {
        ADDRESS deflbl;
        deflbl.labelname = cst->defaultlbl;
        dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, deflbl));
      } else {
        dapush(prog->ops, ct_3ac_op1(JMP_3, ISCONST | ISLABEL, brklabel));
      }
      solidstate(cst->body, prog);
      intsert(prog->labeloffsets, brklabel.labelname, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, brklabel));
      return;
    case LABEL:
      intsert(prog->labeloffsets, cst->glabel, prog->ops->length);
      dapush(prog->ops, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) strdup(cst->glabel)));
      return;
    case CMPND: 
      //probably more stack stuff will need to be done here?
      if(cst->stmtsandinits) {
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
  fprintf(stderr, "Error: reduction of statement %s to 3 address code failed\n", name_STMTTYPE[cst->type]);
}

PROGRAM* linefunc(FUNC* f) {
  PROGRAM* prog = calloc(sizeof(PROGRAM), 1);
  prog->ops = dactor(1024);
  prog->breaklabels = dactor(8);
  prog->continuelabels = dactor(8);
  prog->fixedvars = htctor();
  prog->labeloffsets = htctor();
  prog->lval = 0;
  //initialize params
  for(int i = 0; i < f->params->da->length; i++) {
    FULLADDR* newa = malloc(sizeof(FULLADDR));
    DECLARATION* pdec = pget(f->params, i);
    newa->addr_type = addrconv(pdec->type);
    if(newa->addr_type & ISFLOAT) {
      newa->addr.fregnum = prog->fregcnt++;
    } else {
      newa->addr.iregnum = prog->iregcnt++;
    }
    dapush(prog->ops, ct_3ac_op1(PARAM_3, newa->addr_type, newa->addr));
    if(!(pdec->type->pointerstack && pdec->type->pointerstack->length) && (pdec->type->tb & (STRUCTVAL | UNIONVAL) )) {
      ADDRESS tmpaddr, tmpaddr2;
      tmpaddr.intconst_64 = pdec->type->structtype->size;
      tmpaddr2.iregnum = prog->iregcnt++;
      dapush(prog->ops, ct_3ac_op2(ALOC_3, ISCONST | 8, tmpaddr, newa->addr_type, tmpaddr2));
      dapush(prog->ops, ct_3ac_op3(COPY_3, newa->addr_type, newa->addr, ISCONST, tmpaddr, newa->addr_type, tmpaddr2));
      dapush(prog->ops, ct_3ac_op2(MOV_3, newa->addr_type, tmpaddr2, newa->addr_type, newa->addr));
    } 
    fixedinsert(prog->fixedvars, pdec->varid, newa);
  }
  solidstate(f->body, prog);
  return prog;
}

static void printaddr(ADDRESS addr, ADDRTYPE addr_type) {
  if(addr_type & ISDEREF) putchar('(');
  if(addr_type & ISLABEL) {
    printf("{%s}", addr.labelname);
  } else if(addr_type & ISCONST) {
    if(addr_type & ISSTRCONST) {
      int l = strlen(addr.strconst);
      if(!l)
        printf("\"\"");
      else if(addr.strconst[l - 1] != '\n')
        printf("\"%s\"", addr.strconst);
      else
        printf("\"%.*s\"\\n", l - 1, addr.strconst);
    } else if(addr_type & ISFLOAT) 
      printf("%lf", addr.floatconst_64);
    else if(addr_type & ISSIGNED) 
      printf("%ld", addr.intconst_64);
    else
      printf("%lu", addr.intconst_64);
  } else {
    if(addr_type & ISFLOAT) 
      printf("freg%lu.%d", addr.fregnum, (addr_type & 0xf) * 8);
    else
      printf("ireg%lu.%d%c", addr.iregnum, (addr_type & 0xf) * 8, addr_type & ISSIGNED ? 's' : 'u');
  }
  if(addr_type & ISDEREF) putchar(')');
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
    printf("%s", #opsymb); \
    printaddr(op->addr0, op->addr0_type); \
    printf(" →  "); \
    printaddr(op->dest, op->dest_type); \
  } while(0)

#define PRINTOP1() do { \
    printf("\t"); \
    printaddr(op->addr0, op->addr0_type); \
  } while(0)


void printprog(PROGRAM* prog) {
  //maybe we want to color?
  DYNARR* pd = prog->ops;
  for(int i = 0; i < pd->length; i++) {
    OPERATION* op = daget(pd, i);
    printf("%s", opcode_3ac_names[op->opcode]);
    switch(op->opcode) {
      case NOP_3: case RET_0:
        break;
      case LBL_3: 
        printf("\t%s:", op->addr0.labelname);
        break;
      case COPY_3:
        PRINTOP3( );
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
        PRINTOP3(<<);
        break;
      case SHR_U: case SHR_I: 
        PRINTOP3(>>);
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
        PRINTOP3(==);
        break;
      case BNE_U: case BNE_I: case BNE_F: 
        PRINTOP3(!=);
        break;
      case BGE_U: case BGE_I: case BGE_F: 
        PRINTOP3(>=);
        break;
      case BLE_U: case BLE_I: case BLE_F: 
        PRINTOP3(<=);
        break;
      case BGT_U: case BGT_I: case BGT_F: 
        PRINTOP3(>);
        break;
      case BLT_U: case BLT_I: case BLT_F: 
        PRINTOP3(<);
        break;
      case BNZ_3: case BEZ_3: 
        PRINTOP2( );
        break;
      case JMP_3: 
        PRINTOP1( );
        break;
      case MOV_3: case ALOC_3:
        PRINTOP2( );
        break;
      case ARG_3: case PARAM_3: case RET_3:
        PRINTOP1( );
        break;
      case CALL_3:
      case F2I: case I2F:
        PRINTOP2( ); //perhaps use cast later, not vital
        break;
      case ARRIND:
      case ARROFF:
        printf("\t");
        printaddr(op->addr0, op->addr0_type);
        printf("[");
        printaddr(op->addr1, op->addr1_type);
        printf("] ");
        printf(" →  ");
        printaddr(op->dest, op->dest_type);
        break;
      case ARRMOV: //not quite right
        printf("\t");
        printaddr(op->addr0, op->addr0_type);
        printf(" →  ");
        printaddr(op->dest, op->dest_type);
        printf("[");
        printaddr(op->addr1, op->addr1_type);
        printf("] ");
        break;
      case MTP_OFF: //not quite right
        printf("\t");
        printaddr(op->addr0, op->addr0_type);
        printf(" →  ");
        printaddr(op->dest, op->dest_type);
        printf(" + ");
        printaddr(op->addr1, op->addr1_type);
        break;
      case INIT_3:
        PRINTOP1( );
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

static void freeop(void* o2) {
  OPERATION* op = o2;
  switch(op->opcode) {
    case LBL_3:
      free(op->addr0.labelname);
    default:
      break;
  }
  free(op);
}

void freeprog(PROGRAM* prog) {
  dadtorcfr(prog->ops, freeop);
  dadtor(prog->breaklabels);
  dadtor(prog->continuelabels);
  fhtdtorcfr(prog->fixedvars, free);
  htdtor(prog->labeloffsets);
  free(prog);
}

