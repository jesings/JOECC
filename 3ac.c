#include <assert.h>
#include <sys/stat.h>
#include "3ac.h"
#define X(s) #s,
const char* opcode_3ac_names[] = {
  OPS_3AC
};
extern const char* name_EXPRTYPE[];
extern const char* name_STMTTYPE[];
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

OPERATION* implicit_binary_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE arg2id = typex(daget(cexpr->params, 1));
  IDTYPE retid = typex(cexpr);
  FULLADDR desta;
  if(ispointer2(retid)) {
    if(ispointer2(arg1id) && !ispointer2(arg2id)) {
      a2 = ptarith(retid, a2, prog);
    } else if(ispointer2(arg2id) && !ispointer2(arg1id)) {
      a1 = ptarith(retid, a1, prog);
    }
    FILLREG(desta, ISPOINTER | 8);
  } else if(retid.tb & FLOATNUM) {
    op += 2;
    if(!(arg1id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(I2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    } else if((arg1id.tb & 0xf) != (retid.tb & 0xf)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(F2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    }
    if(!(arg2id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    } else if((arg2id.tb & 0xf) != (retid.tb & 0xf)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(F2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    }
    FILLREG(desta, ISFLOAT | (retid.tb & 0xf));
  } else if(retid.tb & UNSIGNEDNUM) {
    FILLREG(desta, retid.tb & 0xf);
  } else {
    op += 1;
    FILLREG(desta, (retid.tb & 0xf) | ISSIGNED);
  }

  return ct_3ac_op3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, desta.addr_type, desta.addr);
}

OPERATION* implicit_bitwise_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  FULLADDR desta;
  IDTYPE retid = typex(cexpr);
  assert(!(retid.tb & FLOATNUM));
  FILLREG(desta, retid.tb & 0xf);
  return ct_3ac_op3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, desta.addr_type, desta.addr);
}

FULLADDR cmpnd_assign(enum opcode_3ac op, EXPRESSION* destexpr, EXPRESSION* srcexpr, PROGRAM* prog) {
  IDTYPE destidt = typex(destexpr);
  IDTYPE srcidt = typex(srcexpr);
  FULLADDR srcaddr = linearitree(srcexpr, prog);
  FULLADDR destaddr = linearitree(destexpr, prog);
  //do some implicit binary stuff
  if(ispointer2(destidt)) {
    if(ispointer2(srcidt)) {
      switch(op) {
        case AND_U: case OR_U: case XOR_U:
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
    assert(!ispointer2(srcidt));
    if(destidt.tb & FLOATNUM) {
      op += 2;
      if(!(srcidt.tb & FLOATNUM)) {
        FULLADDR fad;
        FILLREG(fad, ISFLOAT | (destidt.tb & 0xf));
        opn(prog, ct_3ac_op2(I2F, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        srcaddr = fad;
      } else if((srcidt.tb & 0xf) != (destidt.tb & 0xf)) {
        FULLADDR fad;
        FILLREG(fad, ISFLOAT | (destidt.tb & 0xf));
        opn(prog, ct_3ac_op2(F2F, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        srcaddr = fad;
      }
    } else if(srcidt.tb & FLOATNUM) {
        FULLADDR fad, fad2;
        FILLREG(fad, ISFLOAT | (srcidt.tb & 0xf));
        FILLREG(fad2, ISFLOAT | (srcidt.tb & 0xf));
        opn(prog, ct_3ac_op2(I2F, destaddr.addr_type, destaddr.addr, fad.addr_type, fad.addr));
        opn(prog, ct_3ac_op3(op, fad.addr_type, fad.addr, srcaddr.addr_type, srcaddr.addr, fad2.addr_type, fad2.addr));
        opn(prog, ct_3ac_op2(F2I, fad2.addr_type, fad2.addr, destaddr.addr_type, destaddr.addr));
        return destaddr;
    } else {
        if(!(destidt.tb & UNSIGNEDNUM && srcidt.tb & UNSIGNEDNUM))
          op += 1;
    }
  }
  opn(prog, ct_3ac_op3(op, destaddr.addr_type, destaddr.addr, srcaddr.addr_type, srcaddr.addr, destaddr.addr_type, destaddr.addr));
  return destaddr;
}

static FULLADDR cmpnd_assign_addsub(enum opcode_3ac op, EXPRESSION* destexpr, EXPRESSION* srcexpr, PROGRAM* prog) {
  IDTYPE destidt = typex(destexpr);
  IDTYPE srcidt = typex(srcexpr);
  FULLADDR srcaddr = linearitree(srcexpr, prog);
  FULLADDR destaddr = linearitree(destexpr, prog);
  if(ispointer2(destidt)) {
    if(!ispointer2(srcidt)) {
      srcaddr = ptarith(destidt, srcaddr, prog);
    }
  } else {
    assert(!ispointer2(srcidt));
    if(destidt.tb & FLOATNUM) {
      op += 1;
      if(!(srcidt.tb & FLOATNUM)) {
        FULLADDR fad;
        FILLREG(fad, ISFLOAT | (destidt.tb & 0xf));
        opn(prog, ct_3ac_op2(I2F, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        srcaddr = fad;
      } else if((srcidt.tb & 0xf) != (destidt.tb & 0xf)) {
        FULLADDR fad;
        FILLREG(fad, ISFLOAT | (destidt.tb & 0xf));
        opn(prog, ct_3ac_op2(F2F, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        srcaddr = fad;
      }
    } else if(srcidt.tb & FLOATNUM) {
        FULLADDR fad, fad2;
        FILLREG(fad, ISFLOAT | (srcidt.tb & 0xf));
        FILLREG(fad2, ISFLOAT | (srcidt.tb & 0xf));
        opn(prog, ct_3ac_op2(I2F, destaddr.addr_type, destaddr.addr, fad.addr_type, fad.addr));
        opn(prog, ct_3ac_op3(op, fad.addr_type, fad.addr, srcaddr.addr_type, srcaddr.addr, fad2.addr_type, fad2.addr));
        opn(prog, ct_3ac_op2(F2I, fad2.addr_type, fad2.addr, destaddr.addr_type, destaddr.addr));
        return destaddr;
    }
  }
  opn(prog, ct_3ac_op3(op, destaddr.addr_type, destaddr.addr, srcaddr.addr_type, srcaddr.addr, destaddr.addr_type, destaddr.addr));
  return destaddr;
}
static FULLADDR prestep(char isinc, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR destaddr, curaddr;
  IDTYPE rid = typex(cexpr);
  destaddr = linearitree(daget(cexpr->params, 0), prog);
  char baseness = destaddr.addr_type & ISFLOAT ? 2 : 0;
  if(ispointer2(rid)) {
    struct declarator_part* dclp = dapeek(rid.pointerstack);
    assert(dclp->type != ARRAYSPEC && dclp->type != VLASPEC);
    rid.pointerstack->length -= 1;
    curaddr.addr.uintconst_64 = lentype(&rid);
    rid.pointerstack->length += 1;
  } else {
    curaddr.addr.uintconst_64 = 1;
  }
  opn(prog, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, destaddr.addr_type, destaddr.addr, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
  return destaddr;
}
static FULLADDR poststep(char isinc, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR destaddr, actualaddr, curaddr;
  IDTYPE rid = typex(cexpr);
  destaddr = linearitree(daget(cexpr->params, 0), prog);
  char baseness = destaddr.addr_type & ISFLOAT ? 2 : 0;
  FILLREG(actualaddr, destaddr.addr_type & GENREGMASK);
  opn(prog, ct_3ac_op2(MOV_3, destaddr.addr_type, destaddr.addr, actualaddr.addr_type, actualaddr.addr));
  if(ispointer2(rid)) {
    struct declarator_part* dclp = dapeek(rid.pointerstack);
    assert(dclp->type != ARRAYSPEC && dclp->type != VLASPEC);
    rid.pointerstack->length -= 1;
    curaddr.addr.uintconst_64 = lentype(&rid);
    rid.pointerstack->length += 1;
  } else {
    curaddr.addr.uintconst_64 = 1;
  }
  opn(prog, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, destaddr.addr_type, destaddr.addr, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
  return actualaddr;
}

OPERATION* implicit_mtp_2(EXPRESSION* destexpr, EXPRESSION* fromexpr, FULLADDR a1, FULLADDR a2, PROGRAM* prog) {
  IDTYPE destidt = typex(destexpr);
  IDTYPE srcidt = typex(fromexpr);
  if(ispointer2(destidt)) {
    if(!ispointer2(srcidt)) {
      assert((fromexpr->type == INT || fromexpr->type == UINT) && fromexpr->intconst == 0);
    }
  } else if(destidt.tb & FLOATNUM) {
    if(!(srcidt.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (destidt.tb & 0xf));
      return ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type | ISDEREF, fad.addr);
    } else if((srcidt.tb & 0xf) != (destidt.tb & 0xf)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (destidt.tb & 0xf));
      return ct_3ac_op2(F2F, a2.addr_type, a2.addr, fad.addr_type | ISDEREF, fad.addr);
    }
  } else if(destidt.tb & UNSIGNEDNUM) {
    if(srcidt.tb & FLOATNUM) {
      FULLADDR fad;
      FILLREG(fad, destidt.tb & 0xf);
      return ct_3ac_op2(F2I, a2.addr_type, a2.addr, fad.addr_type | ISDEREF, fad.addr);
    }
  } else {
    if(srcidt.tb & FLOATNUM) {
      FULLADDR fad;
      FILLREG(fad, (destidt.tb & 0xf) | ISSIGNED);
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
  if(ispointer2(retid)) {
    FILLREG(desta, ISPOINTER | 8);
  } else if(retid.tb & FLOATNUM) {
    op += 2;
    if(!(arg1id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(I2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    } else if((arg1id.tb & 0xf) != (retid.tb & 0xf)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(F2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    }
    FILLREG(desta, ISFLOAT | (retid.tb & 0xf));
  } else if(retid.tb & UNSIGNEDNUM) {
    FILLREG(desta, retid.tb & 0xf);
  } else {
    op += 1;
    FILLREG(desta, (retid.tb & 0xf) | ISSIGNED);
  }

  return ct_3ac_op2(op, a1.addr_type, a1.addr, desta.addr_type, desta.addr);
}

void implicit_shortcircuit_noret(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, BBLOCK* branchto, PROGRAM* prog) {
  FULLADDR addr2use;
  for(int i = 0; i < cexpr->params->length; i++) {
    addr2use = linearitree(daget(cexpr->params, i), prog);
    opn(prog, ct_3ac_op1(op_to_cmp, addr2use.addr_type, addr2use.addr));
    prog->curblock->branchblock = branchto;
    dapush(branchto->inedges, prog->curblock);
    prog->curblock = NULL;
  }
  prog->curblock = dapeek(prog->allblocks);
}

FULLADDR implicit_shortcircuit_3(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, ADDRESS complete_val, ADDRESS shortcircuit_val, PROGRAM* prog) {
  BBLOCK* failblock,* finalblock;
  finalblock = mpblk();
  failblock= mpblk();
  FULLADDR addr2use;
  for(int i = 0; i < cexpr->params->length; i++) {
    addr2use = linearitree(daget(cexpr->params, i), prog);
    opn(prog, ct_3ac_op1(op_to_cmp, addr2use.addr_type, addr2use.addr));
    prog->curblock->branchblock = failblock;
    dapush(failblock->inedges, prog->curblock);
    prog->curblock = NULL;
  }
  giveblock(prog, mpblk());
  addr2use.addr.iregnum = prog->regcnt++;
  addr2use.addr_type = 1;
  opn(prog, ct_3ac_op2(MOV_3, ISCONST | 1, complete_val, 1, addr2use.addr));
  prog->curblock->nextblock = finalblock;
  dapushc(finalblock->inedges, prog->curblock);
  giveblock(prog, failblock);
  opn(prog, ct_3ac_op2(MOV_3, ISCONST | 1, shortcircuit_val, 1, addr2use.addr));
  giveblock(prog, finalblock);
  return addr2use;
}

OPERATION* cmpret_binary_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  IDTYPE arg1id = typex(daget(cexpr->params, 0));
  IDTYPE arg2id = typex(daget(cexpr->params, 1));
  IDTYPE retid = typex(cexpr);
  if(arg1id.tb & FLOATNUM) {
    op += 2;
    if(!(arg2id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (arg1id.tb & 0xf));
      opn(prog, ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    } else if((arg1id.tb & 0xf) > (arg2id.tb & 0xf)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (arg1id.tb & 0xf));
      opn(prog, ct_3ac_op2(F2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
      a2 = fad;
    } else if((arg1id.tb & 0xf) < (arg2id.tb & 0xf)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (arg2id.tb & 0xf));
      opn(prog, ct_3ac_op2(F2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
      a1 = fad;
    }
  } else if(arg2id.tb & FLOATNUM) {
    op += 2;
    FULLADDR fad;
    FILLREG(fad, ISFLOAT | (arg2id.tb & 0xf));
    opn(prog, ct_3ac_op2(I2F, a1.addr_type, a1.addr, fad.addr_type, fad.addr));
    a2 = fad;
  } else if(!((arg1id.tb & UNSIGNEDNUM) || (arg2id.tb & UNSIGNEDNUM))) {
    op += 1;
  }

  FULLADDR desta;
  FILLREG(desta, (retid.tb & 0xf) | (retid.tb & UNSIGNEDNUM ? 0 : ISSIGNED));//unsigned
  return ct_3ac_op3(op, a1.addr_type, a1.addr, a2.addr_type, a2.addr, desta.addr_type, desta.addr);
}

OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR a1 = linearitree(daget(cexpr->params, 0), prog);
  FULLADDR a2 = linearitree(daget(cexpr->params, 1), prog);
  assert(!(a1.addr_type & ISFLOAT));
  assert(!(a2.addr_type & ISFLOAT));
  enum opcode_3ac shlop = opcode_unsigned + (a1.addr_type & ISSIGNED ? 1 : 0);
  FULLADDR adr;
  FILLREG(adr, a1.addr_type & GENREGMASK);
  return ct_3ac_op3(shlop, a1.addr_type, a1.addr, a2.addr_type, a2.addr, adr.addr_type, adr.addr);
}

FULLADDR smemrec(EXPRESSION* cexpr, PROGRAM* prog) {
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
  char pointerqual = ispointer(sf->type);
  offaddr.intconst_64 = sf->offset;
  if(!pointerqual && (sf->type->tb & (STRUCTVAL | UNIONVAL))) {
    if(offaddr.intconst_64) {
      FILLREG(retaddr, ISPOINTER | 8);
      opn(prog, ct_3ac_op3(ADD_U, sead.addr_type, sead.addr, ISCONST, offaddr, retaddr.addr_type, retaddr.addr));
    } else {
      if(sead.addr_type & ISDEREF) {
        FILLREG(retaddr, ISPOINTER | 8);
        opn(prog, ct_3ac_op2(MOV_3, sead.addr_type, sead.addr, retaddr.addr_type, retaddr.addr));
      } else {
        return sead;
      }
    }
  } else {
    FULLADDR intermediate;
    if(offaddr.intconst_64) {
      FILLREG(intermediate, ISPOINTER | 8);
      opn(prog, ct_3ac_op3(ADD_U, sead.addr_type, sead.addr, ISCONST, offaddr, intermediate.addr_type, intermediate.addr));
    } else {
      if(sead.addr_type & ISDEREF) {
        FILLREG(intermediate, ISPOINTER | 8);
        opn(prog, ct_3ac_op2(MOV_3, sead.addr_type, sead.addr, intermediate.addr_type, intermediate.addr));
      } else {
        sead.addr_type |= ISDEREF;
        return sead;
      }
    }
    intermediate.addr_type = addrconv(&retty) | ISDEREF;
    return intermediate; //probably nothing different needs to be done with pointer or anything
  }
  return retaddr;
}

static FULLADDR execvla(IDTYPE* idt, PROGRAM* prog) {
   FULLADDR curaddr, otheraddr, scratchaddr;
   struct declarator_part* dclp = dapeek(idt->pointerstack);
   curaddr = linearitree(dclp->vlaent, prog);
   struct declarator_part* subdclp;
   int psentry;
   for(psentry = idt->pointerstack->length - 1; psentry >= 0 && (subdclp = daget(idt->pointerstack, psentry))->type == VLASPEC; psentry--) {
     FILLREG(otheraddr, curaddr.addr_type & GENREGMASK);
     scratchaddr = linearitree(subdclp->vlaent, prog);
     opn(prog, ct_3ac_op3(MULT_U, curaddr.addr_type, curaddr.addr, scratchaddr.addr_type, scratchaddr.addr, otheraddr.addr_type, otheraddr.addr));
     curaddr = otheraddr;
   }
   int lenstore = idt->pointerstack->length;
   idt->pointerstack->length = psentry;
   scratchaddr.addr.uintconst_64 = lentype(idt);
   idt->pointerstack->length = lenstore;
   FILLREG(otheraddr, curaddr.addr_type & GENREGMASK);
   opn(prog, ct_3ac_op3(MULT_U, curaddr.addr_type, curaddr.addr, ISCONST | 0x8, scratchaddr.addr, otheraddr.addr_type, otheraddr.addr));
   dclp->addrun = otheraddr.addr.garbage; //may we need the non-top level pointerstack entries to be correct?
   dclp->addrty = otheraddr.addr_type;
   return otheraddr;
}

FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr, otheraddr, destaddr;
  IDTYPE varty;
  OPERATION* genop;

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
          curaddr.addr_type = 8 | ISLABEL;
        } else {
          curaddr.addr_type = addrconv(cexpr->id->type) | ISLABEL;
        }
        curaddr.addr.labelname = cexpr->id->name;
        return curaddr;
      } else {
        return *(FULLADDR*) daget(prog->dynvars, cexpr->id->index);
      }
    case ARRAY_LIT:
      FILLREG(destaddr, ISPOINTER | 0x8);
      struct declarator_part* ptrtop = dapeek(cexpr->rettype->pointerstack);
      --cexpr->rettype->pointerstack->length;
      ADDRTYPE memtype = addrconv(cexpr->rettype);
      ++cexpr->rettype->pointerstack->length;
      assert(ptrtop->type == ARRAYSPEC);
      //vlas may not be initialized
      curaddr.addr.uintconst_64 = ptrtop->arrlen;
      opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
      curaddr.addr.uintconst_64 = ptrtop->arrlen / ptrtop->arrmaxind;
      if(curaddr.addr.uintconst_64 < 0xf) {
        for(int i = 0; i < cexpr->params->length; i++) {
          EXPRESSION* dyne = daget(cexpr->params, i);
          otheraddr = linearitree(dyne, prog);
          curaddr.addr.uintconst_64 = i;
          if(memtype & ISFLOAT && !(otheraddr.addr_type & ISFLOAT)) {
            FULLADDR fad2;
            FILLREG(fad2, destaddr.addr_type & GENREGMASK);
            opn(prog, ct_3ac_op2(I2F, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
            otheraddr = fad2;
          } else if(!(memtype & ISFLOAT) && otheraddr.addr_type & ISFLOAT) {
            FULLADDR fad2;
            FILLREG(fad2, destaddr.addr_type & GENREGMASK);
            opn(prog, ct_3ac_op2(F2I, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
            otheraddr = fad2;
          } else if(memtype & ISFLOAT && (otheraddr.addr_type & ISFLOAT) && (memtype & 0xf) != (otheraddr.addr_type & 0xf)) {
            FULLADDR fad2;
            FILLREG(fad2, ISFLOAT | (memtype & 0xf));
            opn(prog, ct_3ac_op2(F2F, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
            otheraddr = fad2;
          }
          opn(prog, ct_3ac_op3(ARRMOV, otheraddr.addr_type, otheraddr.addr, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else {
        ADDRESS a;
        a.uintconst_64 = curaddr.addr.uintconst_64;
        for(int i = 0; i < cexpr->params->length; i++) {
          EXPRESSION* dyne = daget(cexpr->params, i);
          otheraddr = linearitree(dyne, prog);
          opn(prog, ct_3ac_op3(ADD_U, destaddr.addr_type, destaddr.addr, ISCONST | 0x8, a, destaddr.addr_type, destaddr.addr));
          if(destaddr.addr_type & ISFLOAT && !(otheraddr.addr_type & ISFLOAT)) {
            FULLADDR fad2;
            FILLREG(fad2, destaddr.addr_type & GENREGMASK);
            opn(prog, ct_3ac_op2(I2F, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
            otheraddr = fad2;
          } else if(!(destaddr.addr_type & ISFLOAT) && otheraddr.addr_type & ISFLOAT) {
            FULLADDR fad2;
            FILLREG(fad2, destaddr.addr_type & GENREGMASK);
            opn(prog, ct_3ac_op2(F2I, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
            otheraddr = fad2;
          } else if(destaddr.addr_type & ISFLOAT && (otheraddr.addr_type & ISFLOAT) && (destaddr.addr_type & 0xf) != (otheraddr.addr_type & 0xf)) {
            FULLADDR fad2;
            FILLREG(fad2, ISFLOAT | (destaddr.addr_type & 0xf));
            opn(prog, ct_3ac_op2(F2F, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
            otheraddr = fad2;
          }
          opn(prog, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type | ISDEREF, destaddr.addr));
        }
      }
      return destaddr;
    case STRUCT_LIT:
      FILLREG(destaddr, ISPOINTER | 0x8);
      curaddr.addr.uintconst_64 = cexpr->rettype->structtype->size;
      opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
      for(int i = 0; i < cexpr->params->length; i++) {
        EXPRESSION* member = daget(cexpr->params, i);
        DECLARATION* decl = daget(cexpr->rettype->structtype->fields, i);
        STRUCTFIELD* sf = search(cexpr->rettype->structtype->offsets, decl->varname);
        curaddr = linearitree(member, prog);
        otheraddr.addr.uintconst_64 = sf->offset;
        if(addrconv(sf->type) & ISFLOAT && !(curaddr.addr_type & ISFLOAT)) {
          FULLADDR fad2;
          FILLREG(fad2, destaddr.addr_type & GENREGMASK);
          opn(prog, ct_3ac_op2(I2F, curaddr.addr_type, curaddr.addr, fad2.addr_type, fad2.addr));
          curaddr = fad2;
        } else if(!(addrconv(sf->type) & ISFLOAT) && curaddr.addr_type & ISFLOAT) {
          FULLADDR fad2;
          FILLREG(fad2, destaddr.addr_type & GENREGMASK);
          opn(prog, ct_3ac_op2(F2I, curaddr.addr_type, curaddr.addr, fad2.addr_type, fad2.addr));
          curaddr = fad2;
        } else if(addrconv(sf->type) & ISFLOAT && curaddr.addr_type & ISFLOAT &&
                  ((sf->type->tb & 0xf) != (otheraddr.addr_type & 0xf))) {
          FULLADDR fad2;
          FILLREG(fad2, ISFLOAT | (sf->type->tb & 0xf));
          opn(prog, ct_3ac_op2(F2F, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
          otheraddr = fad2;
        }
        opn(prog, ct_3ac_op3(MTP_OFF, curaddr.addr_type, curaddr.addr, ISCONST | 0x8, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;
    case NEG:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & GENREGMASK;
      destaddr.addr.iregnum = prog->regcnt++;
      if(destaddr.addr_type & ISFLOAT) {
        opn(prog, ct_3ac_op2(NEG_F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else {
        opn(prog, ct_3ac_op2(NEG_I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;
    case L_NOT:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      assert(!(curaddr.addr_type & ISFLOAT));
      FILLREG(destaddr, (curaddr.addr_type & ISSIGNED) | 1);
      otheraddr.addr.uintconst_64 = 0;
      opn(prog, ct_3ac_op3(EQ_U, curaddr.addr_type, curaddr.addr, (curaddr.addr_type & 0xf) | ISCONST, otheraddr.addr,
                                   destaddr.addr_type, destaddr.addr));
      return destaddr;
    case B_NOT:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & GENREGMASK;
      assert(!(destaddr.addr_type & ISFLOAT));
      destaddr.addr.iregnum = prog->regcnt++;
      opn(prog, ct_3ac_op2(NOT_U, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      return destaddr;

    case ADDR:
      varty = typex(daget(cexpr->params, 0));
      char hpoints = ispointer2(varty);
      if(!(hpoints) && (varty.tb & STRUCTVAL)) {
        return linearitree(daget(cexpr->params, 0), prog);//addr should be a no-op for single pointers to structs
      }
      if(hpoints) {
        struct declarator_part* dclp = dapeek(varty.pointerstack);
        if(dclp->type == ARRAYSPEC || dclp->type == VLASPEC)
          return linearitree(daget(cexpr->params, 0), prog);//addr should be a no-op for single pointers to arrays
      }
      return op2ret(prog, implicit_unary_2(ADDR_3, cexpr, prog));

    case DEREF:
      varty = typex(daget(cexpr->params, 0));
      assert(ispointer2(varty));
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      if(varty.pointerstack->length == 1 && (varty.tb & STRUCTVAL)) {
        return destaddr; //dereferencing single pointer to struct should be a no-op
      }
      struct declarator_part* dclp = dapeek(varty.pointerstack);
      if(dclp->type == ARRAYSPEC || dclp->type == VLASPEC) {
        varty = typex(cexpr);
        if(ispointer2(varty)) {
          struct declarator_part* dclp2 = dapeek(varty.pointerstack);
          if(dclp2->type != ARRAYSPEC || dclp2->type == VLASPEC) {
            goto REALDEREF;
          }
        } else {
          goto REALDEREF;
        }
      } else {
        REALDEREF:
        if(destaddr.addr_type & ISDEREF) {
          FILLREG(otheraddr, destaddr.addr_type & GENREGMASK);
          opn(prog, ct_3ac_op2(MOV_3, destaddr.addr_type, destaddr.addr, otheraddr.addr_type, otheraddr.addr));
          destaddr = otheraddr;
        }
        destaddr.addr_type |= ISDEREF;
      }
      return destaddr;
    case ADD:
      genop = implicit_binary_3(GENERIC_U, cexpr, prog);
      DEGENERIC(genop, ADD);
      return op2ret(prog, genop);
    case SUB: 
      genop = implicit_binary_3(GENERIC_U, cexpr, prog);
      DEGENERIC(genop, SUB);
      return op2ret(prog, genop);
    case MULT:
      return op2ret(prog, implicit_binary_3(MULT_U, cexpr, prog));
    case DIVI: 
      return op2ret(prog, implicit_binary_3(DIV_U, cexpr, prog));

    case EQ:
      genop = cmpret_binary_3(GENERIC_U, cexpr, prog);
      DEGENERIC(genop, EQ);
      return op2ret(prog, genop);
    case NEQ: 
      genop = cmpret_binary_3(GENERIC_U, cexpr, prog);
      DEGENERIC(genop, NE);
      return op2ret(prog, genop);
    case GT:
      return op2ret(prog, cmpret_binary_3(GT_U, cexpr, prog));
    case LT:
      return op2ret(prog, cmpret_binary_3(LT_U, cexpr, prog));
    case GTE:
      return op2ret(prog, cmpret_binary_3(GE_U, cexpr, prog));
    case LTE:
      return op2ret(prog, cmpret_binary_3(LE_U, cexpr, prog));

    case MOD:
      varty = typex(daget(cexpr->params, 0));
      assert(!(varty.tb & FLOATNUM));
      return op2ret(prog, cmpret_binary_3(MOD_U, cexpr, prog));

    case L_AND:
      return implicit_shortcircuit_3(BEZ_3, cexpr, (ADDRESS) 1ul, (ADDRESS) 0ul, prog);
    case L_OR:
      return implicit_shortcircuit_3(BNZ_3, cexpr, (ADDRESS) 0ul, (ADDRESS) 1ul, prog);

    case B_AND:
      return op2ret(prog, implicit_bitwise_3(AND_U, cexpr, prog));
    case B_OR:
      return op2ret(prog, implicit_bitwise_3(OR_U, cexpr, prog));
    case B_XOR:
      return op2ret(prog, implicit_bitwise_3(XOR_U, cexpr, prog));

    case SHL:
      return op2ret(prog, binshift_3(SHL_U, cexpr, prog));
    case SHR:
      return op2ret(prog, binshift_3(SHR_U, cexpr, prog));
    case COMMA:
      for(int i = 0; i < cexpr->params->length - 1; i++) {
        linearitree(daget(cexpr->params, i), prog);
      }
      return linearitree(daget(cexpr->params, cexpr->params->length - 1), prog);

    case DOTOP: 
      varty = typex(daget(cexpr->params, 0));
      assert(!ispointer2(varty));
      return smemrec(cexpr, prog);
    case ARROW:
      varty = typex(daget(cexpr->params, 0));
      assert(varty.pointerstack && (varty.pointerstack->length == 1));
      return smemrec(cexpr, prog);
    case SZOFEXPR:
      varty = typex(cexpr);
      destaddr.addr.uintconst_64 = lentype(&varty);
      if(destaddr.addr.uintconst_64 == (unsigned long) -1) {
        struct declarator_part* dclp = dapeek(varty.pointerstack);
        destaddr.addr.garbage = dclp->addrun;
        destaddr.addr_type = dclp->addrty;
        linearitree(daget(cexpr->params, 0), prog);
      } else {
        destaddr.addr_type = ISCONST | 0x8;
      }
      return destaddr;
    case CAST: //handle identity casts differently
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      if(ispointer(cexpr->vartype)) {
        assert(!(curaddr.addr_type & ISFLOAT));
        FILLREG(destaddr, 8 | ISPOINTER);
        opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else if(cexpr->vartype->tb & FLOATNUM) {
        FILLREG(destaddr, (cexpr->vartype->tb & 0xf) | ISSIGNED | ISFLOAT);
        if(curaddr.addr_type & ISFLOAT) {
          opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          opn(prog, ct_3ac_op2(I2F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else if(cexpr->vartype->tb & UNSIGNEDNUM) {
        FILLREG(destaddr, cexpr->vartype->tb & 0xf);
        if(!(destaddr.addr_type)) destaddr.addr_type = curaddr.addr_type & 0xf;
        if(curaddr.addr_type & ISFLOAT) {
          opn(prog, ct_3ac_op2(F2I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else if(cexpr->vartype->tb & VOIDNUM) {
        return curaddr; //not sure how this should be handled
      } else if(cexpr->vartype->tb & UNIONVAL) { 
        USTRUCT* castdest = cexpr->vartype->uniontype;
        FILLREG(destaddr, ISPOINTER | 0x8);
        unionlen(castdest);
        otheraddr.addr.uintconst_64 = castdest->size;
        opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, otheraddr.addr, destaddr.addr_type, destaddr.addr));
        IDTYPE srctype = typex(daget(cexpr->params, 0));
        for(int i = 0; i < castdest->fields->length; i++) {
          DECLARATION* dcl = daget(castdest->fields, i);
          if(!typecompat(dcl->type, &srctype)) continue;
          opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type | ISDEREF, destaddr.addr));
          return destaddr;
        }
        assert(0);
      } else if(cexpr->vartype->tb & 0xf) {
        FILLREG(destaddr, (cexpr->vartype->tb & 0xf) | ISSIGNED);
        if(curaddr.addr_type & ISFLOAT) {
          opn(prog, ct_3ac_op2(F2I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        } else {
          opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else {
        //don't support casting structs yet
        assert(0);
      }
      return destaddr;
    case TERNARY: ;
      BBLOCK* joinblock = mpblk();
      BBLOCK* succblock = mpblk();
      BBLOCK* failblock = mpblk();
      cmptype(daget(cexpr->params, 0), failblock, succblock, prog);
      BBLOCK* topblock;
      prog->curblock = NULL;
      IDTYPE t0t = typex(daget(cexpr->params, 0));
      IDTYPE t1t = typex(daget(cexpr->params, 1));
      IDTYPE t2t = typex(daget(cexpr->params, 2));
      IDTYPE t3t = typex(cexpr);
      OPERATION* join = malloc(sizeof(OPERATION));
      join->opcode = PHI;
      join->addr0_type = ISCONST | GARBAGEVAL;
      join->addr0.joins = malloc(2 * sizeof(FULLADDR));
      giveblock(prog, succblock);
      FILLREG(destaddr, addrconv(&t3t));
      join->dest = destaddr.addr;
      join->dest_type = destaddr.addr_type;
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      if(!(t1t.tb & FLOATNUM) && (t2t.tb & FLOATNUM)) {
        FULLADDR ad2;
        FILLREG(ad2, (t0t.tb & 0xf) | ISFLOAT | ISSIGNED);
        opn(prog, ct_3ac_op2(I2F, curaddr.addr_type, curaddr.addr, ad2.addr_type, ad2.addr));
        curaddr = ad2;
      }
      if((t1t.tb & FLOATNUM) && ((destaddr.addr_type & 0xf) != (t1t.tb & 0xf))) {
        FULLADDR fad2;
        FILLREG(fad2, ISFLOAT | (destaddr.addr_type & 0xf));
        opn(prog, ct_3ac_op2(F2F, curaddr.addr_type, curaddr.addr, fad2.addr_type, fad2.addr));
        curaddr = fad2;
      }
      topblock = dapeek(prog->allblocks);
      if(!topblock->nextblock) {
        topblock->nextblock = joinblock;
        dapushc(joinblock->inedges, topblock);
      }
      FILLREG(otheraddr, addrconv(&t3t));
      join->addr0.joins[0] = otheraddr;
      opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, otheraddr.addr_type, otheraddr.addr));
      giveblock(prog, failblock);
      otheraddr = linearitree(daget(cexpr->params, 2), prog);
      if((t1t.tb & FLOATNUM) && !(t2t.tb & FLOATNUM)) {
        FULLADDR ad2;
        FILLREG(ad2, (t0t.tb & 0xf) | ISFLOAT | ISSIGNED);
        opn(prog, ct_3ac_op2(I2F, otheraddr.addr_type, otheraddr.addr, ad2.addr_type, ad2.addr));
        otheraddr = ad2;
      }
      if((t2t.tb & FLOATNUM) && ((destaddr.addr_type & 0xf) != (t2t.tb & 0xf))) {
        FULLADDR fad2;
        FILLREG(fad2, ISFLOAT | (destaddr.addr_type & 0xf));
        opn(prog, ct_3ac_op2(F2F, otheraddr.addr_type, otheraddr.addr, fad2.addr_type, fad2.addr));
        otheraddr = fad2;
      }
      assert((curaddr.addr_type & ISFLOAT) == (otheraddr.addr_type & ISFLOAT)); //confirm 2 addrs have same type or are coercible
      FILLREG(curaddr, addrconv(&t3t));
      join->addr0.joins[1] = curaddr;
      opn(prog, ct_3ac_op2(MOV_3, otheraddr.addr_type, otheraddr.addr, curaddr.addr_type, curaddr.addr));
      giveblock(prog, joinblock);
      opn(prog, join);
      return destaddr;
    case ASSIGN:
      varty = typex(cexpr);
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      destaddr = linearitree(daget(cexpr->params, 0), prog);
      if(!ispointer2(varty) && varty.tb & (STRUCTVAL | UNIONVAL)) {
        feedstruct(varty.structtype);
        otheraddr.addr.uintconst_64 = varty.structtype->size;
        opn(prog, ct_3ac_op3(COPY_3, curaddr.addr_type | ISDEREF, curaddr.addr, ISCONST | 8, otheraddr.addr, destaddr.addr_type | ISDEREF, destaddr.addr));
      } else {
        if(destaddr.addr_type & ISDEREF) {
          opn(prog, implicit_mtp_2(daget(cexpr->params, 0), daget(cexpr->params, 1), destaddr, curaddr, prog));
        } else {
          //implicit type coercion needed
          opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
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
    case ADDASSIGN:
      return cmpnd_assign_addsub(ADD_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
    case SUBASSIGN:
      return cmpnd_assign_addsub(SUB_U, daget(cexpr->params, 0), daget(cexpr->params, 1), prog);
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
    case NOP: case MEMBER:
      assert(0);
    case SZOF:
      destaddr.addr_type = ISCONST | 8;
      destaddr.addr.intconst_64 = lentype(cexpr->vartype);
      if(-1 == destaddr.addr.intconst_64)
        return execvla(cexpr->vartype, prog);
      return destaddr;
    case FCALL: ;
      if(cexpr->params->length > 1) {
        OPERATION* fparam,* lparam;
        curaddr = linearitree(daget(cexpr->params, 1), prog);
        lparam = fparam = ct_3ac_op1(ARG_3, curaddr.addr_type, curaddr.addr);
        for(int i = 2; i < cexpr->params->length; ++i) {
          curaddr = linearitree(daget(cexpr->params, i), prog);
          lparam->nextop = ct_3ac_op1(ARG_3, curaddr.addr_type, curaddr.addr);
          lparam = lparam->nextop;
        }
        opn(prog, fparam);
        prog->curblock->lastop = lparam;
      }
      IDTYPE* frettype = cexpr->rettype;
      if(ispointer(frettype)) {
        FILLREG(destaddr, ISPOINTER | 8);
      } else if(frettype->tb & (STRUCTVAL | UNIONVAL)) {
        FILLREG(destaddr, ISPOINTER | 8);
        FILLREG(otheraddr, ISPOINTER | 8);
        curaddr.addr.uintconst_64 = lentype(frettype);
        opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, curaddr.addr, otheraddr.addr_type, otheraddr.addr));
      } else if(frettype->tb & FLOATNUM) {
        FILLREG(destaddr, ISFLOAT | (frettype->tb & 0xf));
      } else if(frettype->tb & UNSIGNEDNUM) {
        FILLREG(destaddr, frettype->tb & 0xf);
      } else {
        FILLREG(destaddr, ISSIGNED | (frettype->tb & 0xf));
      }
      EXPRESSION* fname = daget(cexpr->params, 0);
      opn(prog, ct_3ac_op2(CALL_3, ISCONST | ISLABEL, (ADDRESS) fname->id->name, destaddr.addr_type, destaddr.addr));
      if(!ispointer(frettype) && frettype->tb & (STRUCTVAL | UNIONVAL))
        opn(prog, ct_3ac_op3(COPY_3, destaddr.addr_type, destaddr.addr, ISCONST | 8, curaddr.addr, otheraddr.addr_type, otheraddr.addr));
      //Note, this is not entirely to spec as the program is only supposed to copy the struct as a transparent final argument within the scope of the called function, not within the caller. This should work for now however.
      return destaddr;
  }
  fprintf(stderr, "Error: reduction of expression %s to 3 address code failed\n", name_EXPRTYPE[cexpr->type]);
  FILLREG(curaddr, 0);
  return curaddr;
}

void cmptype(EXPRESSION* cmpexpr, BBLOCK* failblock, BBLOCK* successblock, PROGRAM* prog) {
  OPERATION* dest_op;
  FULLADDR destaddr;
  char negate = 0;
  switch(cmpexpr->type) {
    case NEQ: case LT: case LTE:
      negate = 1;
      //fall through
    case EQ: case GT: case GTE:
      if(negate) {
        void* tmp = cmpexpr->params->arr[0];
        cmpexpr->params->arr[0] = cmpexpr->params->arr[1];
        cmpexpr->params->arr[1] = tmp;
      }
      switch(cmpexpr->type) {
        case EQ: case NEQ:
          dest_op = cmpret_binary_3(GENERIC_U, cmpexpr, prog);
          DEGENERIC(dest_op, BEQ);
          break;
        case GT: case LTE:
          dest_op = cmpret_binary_3(BGT_U, cmpexpr, prog);
          break;
        case GTE: case LT:
          dest_op = cmpret_binary_3(BGE_U, cmpexpr, prog);
          break;
        default: assert(0);//never should or will be reached
      }
      --prog->regcnt; //dealloc allocated register, ignore third operand
      opn(prog, dest_op);
      break;
     case L_AND:
       implicit_shortcircuit_noret(BEZ_3, cmpexpr, failblock, prog);
       prog->curblock->nextblock = successblock;
       dapush(successblock->inedges, prog->curblock);
       return;
     case L_OR:
       implicit_shortcircuit_noret(BNZ_3, cmpexpr, successblock, prog);
       prog->curblock->nextblock = failblock;
       dapush(failblock->inedges, prog->curblock);
       return;
     case L_NOT:
       cmptype(daget(cmpexpr->params, 0), successblock, failblock, prog);
       return;
     default:
       destaddr = linearitree(cmpexpr, prog);
       opn(prog, ct_3ac_op1(BEZ_3, destaddr.addr_type, destaddr.addr));
       break;
  }
  prog->curblock->nextblock = successblock;
  dapush(successblock->inedges, prog->curblock);
  prog->curblock->branchblock = failblock;
  dapush(failblock->inedges, prog->curblock);
  prog->curblock = NULL;
}

void initializestate(INITIALIZER* i, PROGRAM* prog) {
  FULLADDR* newa = malloc(sizeof(FULLADDR));
  newa->addr_type = addrconv(i->decl->type) | ISVAR;
  newa->addr.varnum = i->decl->varid;
  opn(prog, ct_3ac_op1(INIT_3, newa->addr_type, newa->addr));
  if(i->decl->type->tb & (STRUCTVAL | UNIONVAL)) {
    if(i->expr) {
      FULLADDR lastemp = linearitree(i->expr, prog);
      opn(prog, ct_3ac_op2(MOV_3, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
    } else {
      ADDRESS tmpaddr;
      feedstruct(i->decl->type->structtype);
      tmpaddr.intconst_64 = i->decl->type->structtype->size;
      opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, tmpaddr, newa->addr_type, newa->addr));
    }
  } else {
    if(ispointer(i->decl->type)) {
      struct declarator_part* dclp = dapeek(i->decl->type->pointerstack);
      if(dclp->type == ARRAYSPEC) {
        if(i->expr) {
          assert(i->expr->type == ARRAY_LIT);
          FULLADDR lastemp = linearitree(i->expr, prog);
          opn(prog, ct_3ac_op2(MOV_3, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
        } else {
          ADDRESS tmpaddr;
          tmpaddr.intconst_64 = 1;
          int origlen = i->decl->type->pointerstack->length;
          int index = i->decl->type->pointerstack->length - 1;
          for(; index >= 0; index--) {
            struct declarator_part* sdclp = daget(i->decl->type->pointerstack, index);
            if(sdclp->type != ARRAYSPEC) break;
            tmpaddr.intconst_64 *= sdclp->arrmaxind;
          }
          if(index < 0) index = 0;
          i->decl->type->pointerstack->length = index;
          tmpaddr.intconst_64 *= lentype(i->decl->type);
          i->decl->type->pointerstack->length = origlen;
          opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, tmpaddr, newa->addr_type, newa->addr));
        }
      } else if(dclp->type == VLASPEC) {
        assert(!i->expr); //vlas are not allowed to have expressions
        FULLADDR scratchaddr = execvla(i->decl->type, prog);
        dclp->addrun = scratchaddr.addr.garbage; //may we need the non-top level pointerstack entries to be correct?
        dclp->addrty = scratchaddr.addr_type;
        opn(prog, ct_3ac_op2(ALOC_3, scratchaddr.addr_type, scratchaddr.addr, newa->addr_type, newa->addr));
      } else {
        if(i->expr) {
          FULLADDR curaddr = linearitree(i->expr, prog);
          opn(prog, ct_3ac_op2(MOV_3, curaddr.addr_type, curaddr.addr, newa->addr_type, newa->addr));
        }
      }
    } else {
      if(i->expr) {
        FULLADDR lastemp = linearitree(i->expr, prog);
        if((lastemp.addr_type & ISFLOAT) && !(newa->addr_type & ISFLOAT)) {
          opn(prog, ct_3ac_op2(F2I, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
        } else if(!(lastemp.addr_type & ISFLOAT) && (newa->addr_type & ISFLOAT)) {
          opn(prog, ct_3ac_op2(I2F, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
        } else {
          if((lastemp.addr_type & ISFLOAT) && (newa->addr_type & ISFLOAT) && (lastemp.addr_type & 0xf) != (newa->addr_type & 0xf)) {
            opn(prog, ct_3ac_op2(F2F, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
          } else {
            opn(prog, ct_3ac_op2(MOV_3, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
          }
        }
      }
    }
  }
  assert(prog->dynvars->length == i->decl->varid);
  dapush(prog->dynvars, newa);
  dapush(prog->dynchars, i->decl->varname);
}

static void lbljmp(char* lblname, BBLOCK* block, BBLOCK** loc, PROGRAM* prog) {
  if(!(*loc = search(prog->labels, lblname))) {
    DYNARR* pushto,* inedges;
    if(!(pushto = search(prog->unfilledlabels, lblname))) {
      pushto = dactor(9);
      insert(prog->unfilledlabels, lblname, pushto);
      inedges = dactor(8);
      dapushc(pushto, inedges);
    } else {
      inedges = daget(pushto, 0);
    }
    dapush(inedges, block);
    dapush(pushto, loc);
    *loc = (void*) 1; //dummy, so nextblock can't get populated
  } else {
    dapush((*loc)->inedges, block);
  }
}

void solidstate(STATEMENT* cst, PROGRAM* prog) {
  FULLADDR ret_op;
  BBLOCK* topblock,* breakblock,* contblock,* otherblock;
  switch(cst->type){
    case FRET:
      if(cst->expression) {
        ret_op = linearitree(cst->expression, prog);
        opn(prog, ct_3ac_op1(RET_3, ret_op.addr_type, ret_op.addr));
      } else {
        ret_op.addr.uintconst_64 = 0;
        opn(prog, ct_3ac_op1(RET_3, GARBAGEVAL, ret_op.addr));
      }
      prog->curblock->nextblock = prog->finalblock;
      dapush(prog->finalblock->inedges, prog->curblock);
      prog->curblock = NULL;
      return;
    case LBREAK:
      if(prog->curblock) {
        breakblock = dapeek(prog->breaklabels);
        prog->curblock->nextblock = breakblock;
        dapush(breakblock->inedges, prog->curblock);
        prog->curblock = NULL;
      } //else dead code
      return;
    case LCONT:
      if(prog->curblock) {
        contblock = dapeek(prog->continuelabels);
        prog->curblock->nextblock = contblock;
        dapush(contblock->inedges, prog->curblock);
        prog->curblock = NULL;
      } //else dead code
      return;
    case JGOTO:
      if(!prog->curblock) ctblk(prog);
      lbljmp(cst->glabel, prog->curblock, &prog->curblock->nextblock, prog);
      prog->curblock = NULL;
      return;
    case WHILEL:
      breakblock = mpblk();
      contblock = mpblk();
      otherblock = mpblk();
      dapush(prog->breaklabels, breakblock);
      dapush(prog->continuelabels, contblock);
      giveblock(prog, contblock);
      cmptype(cst->cond, breakblock, otherblock, prog);
      prog->curblock = NULL;
      giveblock(prog, otherblock);
      solidstate(cst->body, prog);
      topblock = dapeek(prog->allblocks);
      if(!topblock->nextblock) {
        topblock->nextblock = contblock;
        dapush(contblock->inedges, topblock);
      }
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      giveblock(prog, breakblock);
      return;
    case FORL:
      breakblock = mpblk();
      contblock = mpblk();
      topblock = mpblk();
      if(cst->forinit->isE) {
        if(cst->forinit->E->type != NOP) 
          linearitree(cst->forinit->E, prog);
      } else {
        for(int i = 0; i < cst->forinit->I->length; i++) {
          initializestate((INITIALIZER*) daget(cst->forinit->I, i), prog);
        }
      }
      dapush(prog->continuelabels, contblock);
      dapush(prog->breaklabels, breakblock);
      giveblock(prog, topblock);
      otherblock = mpblk();
      if(cst->forcond->type != NOP)
        cmptype(cst->forcond, breakblock, otherblock, prog);
      giveblock(prog, otherblock);
      solidstate(cst->forbody, prog);
      giveblock(prog, contblock);
      if(cst->increment->type != NOP)
        linearitree(cst->increment, prog);
      prog->curblock->nextblock = topblock;
      dapush(topblock->inedges, prog->curblock);
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      giveblock(prog, breakblock);
      return;
    case DOWHILEL:
      breakblock = mpblk();
      contblock = mpblk();
      topblock = mpblk();
      dapush(prog->continuelabels, contblock);
      dapush(prog->breaklabels, breakblock);
      giveblock(prog, topblock);
      solidstate(cst->body, prog);
      linearitree(cst->cond, prog);
      giveblock(prog, contblock);
      cmptype(cst->cond, breakblock, topblock, prog);
      dapop(prog->continuelabels);
      dapop(prog->breaklabels);
      giveblock(prog, breakblock);
      return;
    case IFS:
      breakblock = mpblk();
      contblock = mpblk();
      cmptype(cst->ifcond, breakblock, contblock, prog);
      prog->curblock = NULL;
      giveblock(prog, contblock);
      solidstate(cst->thencond, prog);
      prog->curblock = NULL;
      giveblock(prog, breakblock);
      return;
    case IFELSES:
      breakblock = mpblk();
      contblock = mpblk();
      otherblock = mpblk();
      cmptype(cst->ifcond, contblock, otherblock, prog);
      prog->curblock = NULL;
      giveblock(prog, otherblock);
      solidstate(cst->thencond, prog);
      topblock = dapeek(prog->allblocks);
      if(!topblock->nextblock) {
        topblock->nextblock = breakblock;
        dapushc(breakblock->inedges, topblock);
      }
      giveblock(prog, contblock);
      solidstate(cst->elsecond, prog);
      giveblock(prog, breakblock);
      return;
    case SWITCH:
      breakblock = mpblk();
      FULLADDR fad = linearitree(cst->cond, prog);
      dapush(prog->breaklabels, breakblock);
      DYNARR* cll = cst->labeltable->da;
      HASHTABLE* htl = cst->labeltable->ht;
      for(int i = 0; i < cll->length; i++) {
        ADDRESS caseval, caselbl;
        caseval.intconst_64 = (long) daget(cll, i);
        caselbl.labelname = fixedsearch(htl, caseval.intconst_64);
        //maybe signed is unnecessary
        opn(prog, ct_3ac_op3(JEQ_I, fad.addr_type, fad.addr, ISCONST | ISSIGNED, caseval,
                                     ISCONST | ISLABEL, caselbl));
        lbljmp(caselbl.labelname, prog->curblock, &prog->curblock->branchblock, prog);
        prog->curblock = NULL;
      }
      if(cst->defaultlbl) {
        ctblk(prog);
        lbljmp(cst->defaultlbl, prog->curblock, &prog->curblock->nextblock, prog);
      } else {
        topblock = dapeek(prog->allblocks);
        if(!topblock->nextblock) {
          topblock->nextblock = breakblock;
          dapush(breakblock->inedges, topblock);
        }
      }
      prog->curblock = NULL;
      giveblock(prog, mpblk());
      solidstate(cst->body, prog);
      dapop(prog->breaklabels);
      prog->curblock = NULL;
      giveblock(prog, breakblock);
      return;
    case LABEL:
      prog->curblock = NULL;
      opn(prog, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) strdup(cst->glabel)));
      insert(prog->labels, cst->glabel, prog->curblock);
      DYNARR* toempty;
      if((toempty = search(prog->unfilledlabels, cst->glabel))) {
        for(int i = 1; i < toempty->length; i++) {
          *(void**) daget(toempty, i) = prog->curblock;
        }
        damerge(prog->curblock->inedges, daget(toempty, 0));
        dadtor(toempty);
        rmpair(prog->unfilledlabels, cst->glabel);
      }
      return;
    case CMPND: 
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
  prog->breaklabels = dactor(8);
  prog->continuelabels = dactor(8);
  prog->dynvars = dactor(256);//handle this better
  prog->dynchars = dactor(256);//handle this better
  prog->labels = htctor();
  prog->unfilledlabels = htctor();
  prog->allblocks = dactor(256);
  prog->finalblock = mpblk();
  prog->curblock = fctblk(prog);
  prog->pdone = 0;
  //initialize params
  opn(prog, ct_3ac_op1(LBL_3, ISCONST | ISLABEL, (ADDRESS) strdup(f->name)));
  for(int i = 0; i < f->params->length; i++) {
    FULLADDR* newa = malloc(sizeof(FULLADDR));
    DECLARATION* pdec = daget(f->params, i);
    newa->addr_type = addrconv(pdec->type) | ISVAR;
    newa->addr.varnum = pdec->varid;
    opn(prog, ct_3ac_op1(PARAM_3, newa->addr_type, newa->addr));
    if(!ispointer(pdec->type)) {
      if((pdec->type->tb & (STRUCTVAL | UNIONVAL) )) {
        ADDRESS tmpaddr, tmpaddr2;
        tmpaddr.intconst_64 = pdec->type->structtype->size;
        tmpaddr2.iregnum = prog->regcnt++;
        opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, tmpaddr, newa->addr_type & ~ISVAR, tmpaddr2));
        opn(prog, ct_3ac_op3(COPY_3, newa->addr_type | ISDEREF, newa->addr, ISCONST, tmpaddr, (newa->addr_type & ~ISVAR) | ISDEREF, tmpaddr2));
        opn(prog, ct_3ac_op2(MOV_3, newa->addr_type & ~ISVAR, tmpaddr2, newa->addr_type, newa->addr));
      }
    } else {
      struct declarator_part* dclp;
      if((dclp = dapeek(pdec->type->pointerstack))->type == VLASPEC) {

        FULLADDR scratchaddr = execvla(pdec->type, prog);
        dclp->addrun = scratchaddr.addr.garbage; //may we need the non-top level pointerstack entries to be correct?
        dclp->addrty = scratchaddr.addr_type;
        opn(prog, ct_3ac_op2(ALOC_3, scratchaddr.addr_type, scratchaddr.addr, newa->addr_type, newa->addr));
      }
    } 
    assert(prog->dynvars->length == pdec->varid);
    dapush(prog->dynvars, newa);
    dapush(prog->dynchars, pdec->varname);
  }
  solidstate(f->body, prog);
  BBLOCK* beforeexit = dapeek(prog->allblocks);
  if(!beforeexit->nextblock) {
    beforeexit->nextblock = prog->finalblock;
    dapush(prog->finalblock->inedges, beforeexit);
  }
  dapush(prog->allblocks, prog->finalblock);
  prog->closedblocks = NULL;
  return prog;
}

static void printaddr(ADDRESS addr, ADDRTYPE addr_type, char term, FILE* f, PROGRAM* prog) {
  if(addr_type & LASTUSE) fprintf(f, "@");
  if(addr_type & ISLABEL) {
    if(term) fprintf(f, RGBCOLOR(255,200,10));
    else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 255, 200, 10);
    if(addr_type & ISDEREF) fprintf(f, "({%s})", addr.labelname);
    else                    fprintf(f, "{%s}", addr.labelname);
    if(!(addr_type & ISCONST)) {
      if(addr_type & ISFLOAT) fprintf(f, ".%df", (addr_type & 0xf) * 8);
      else                    fprintf(f, ".%d%c", (addr_type & 0xf) * 8, addr_type & ISSIGNED ? 's' : 'u');
    }
    if(term) fprintf(f, CLEARCOLOR);
    else fprintf(f, "</FONT>");
  } else if(addr_type & ISCONST) {
    assert(!(addr_type & ISDEREF));
    if(addr_type & ISSTRCONST) {
      int l = strlen(addr.strconst);
      if(term) fprintf(f, RGBCOLOR(90,180,180));
      else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 90, 180, 180);
      if(!l) {
        fprintf(f, "\"\"");
      } else {
        if(term) {
          fprintf(f, "\"%s\"", addr.strconst);
        } else {
          char* strptr = addr.strconst;
          char* accepted;
          fprintf(f, "\"");
          while((accepted = strpbrk(strptr, "<>\n"))) {
            const char* whichtoprint;
            switch(*accepted) {
              case '>':
                whichtoprint = "&GT;";
                break;
              case '<':
                whichtoprint = "&LT;";
                break;
              case '\n':
                whichtoprint = "\\n";
                break;
            }
            *accepted = 0;
            fprintf(f, "%s%s", strptr, whichtoprint);
            strptr = accepted + 1;
          }
          fprintf(f, "%s", strptr);
          fprintf(f, "\"");
        }
      }
      if(term) fprintf(f, CLEARCOLOR);
      else fprintf(f, "</FONT>");
    } else {
      if(term) fprintf(f, RGBCOLOR(250,60,60));
      else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 250, 60, 60);
      if(addr_type & ISFLOAT) 
        fprintf(f, "%lf", addr.floatconst_64);
      else if(addr_type & ISSIGNED) 
        fprintf(f, "%ld", addr.intconst_64);
      else
        fprintf(f, "%lu", addr.intconst_64);
      if(term) fprintf(f, CLEARCOLOR);
      else fprintf(f, "</FONT>");
    }
  } else {
    int sz = (addr_type & 0xf) * 8;
    if(term) fprintf(f, RGBCOLOR(60,220,60));
    else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 60, 220, 60);
    if(addr_type & ISVAR && !(prog->pdone & GVN)) {
      char* adname = daget(prog->dynchars, addr.varnum);
      if(prog->pdone & SSA && !(addr_type & ADDRSVAR)) {
        if(addr_type & ISDEREF) fprintf(f, "(%s_%u)", adname, addr.ssaind);
        else                    fprintf(f, "%s_%u", adname, addr.ssaind);
      } else {
        if(addr_type & ISDEREF) fprintf(f, "(%s)", adname);
        else                    fprintf(f, "%s", adname);
      }
    } else {
      if(addr_type & ISDEREF) fprintf(f, "(reg%u)", addr.iregnum);
      else                    fprintf(f, "reg%u", addr.iregnum);
    }
    fprintf(f, ".%d%c", sz, addr_type & ISFLOAT ? 'f' : addr_type & ISSIGNED ? 's' : 'u');
    if(term) fprintf(f, CLEARCOLOR);
    else fprintf(f, "</FONT>");
  }
}

#define PRINTBROP2(opsymb) do { \
    printaddr(op->addr0, op->addr0_type, term, f, prog); \
    fprintf(f, " %s ", #opsymb); \
    printaddr(op->addr1, op->addr1_type, term, f, prog); \
  } while(0)

#define PRINTOP3(opsymb) do { \
    printaddr(op->addr0, op->addr0_type, term, f, prog); \
    fprintf(f, " " #opsymb " "); \
    printaddr(op->addr1, op->addr1_type, term, f, prog); \
    fprintf(f, " →  "); \
    printaddr(op->dest, op->dest_type, term, f, prog); \
  } while(0)

#define PRINTOP2(opsymb) do { \
    fprintf(f, "%s", #opsymb); \
    printaddr(op->addr0, op->addr0_type, term, f, prog); \
    fprintf(f, " →  "); \
    printaddr(op->dest, op->dest_type, term, f, prog); \
  } while(0)

#define PRINTOP1() do { \
    printaddr(op->addr0, op->addr0_type, term, f, prog); \
  } while(0)

static void printop(OPERATION* op, char term, BBLOCK* blk, FILE* f, PROGRAM* prog) {
  fprintf(f, "%s", opcode_3ac_names[op->opcode]);
  if(term) fprintf(f, "\t");
  else      fprintf(f, "&nbsp;");
  switch(op->opcode) {
    case NOP_3:
      break;
    case LBL_3: 
      if(term) fprintf(f, RGBCOLOR(200,200,120));
      else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 200, 200, 120);
      fprintf(f, "%s:", op->addr0.labelname);
      if(term) fprintf(f, CLEARCOLOR);
      else fprintf(f, "</FONT>");
      break;
    case COPY_3:
      PRINTOP3( );
      break;
    case ADD_U: case ADD_F: 
      PRINTOP3(+);
      break; 
    case SUB_U: case SUB_F:
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
      if(term) PRINTOP3(<<);
      else PRINTOP3(&lt;&lt;);
      break;
    case SHR_U: case SHR_I: 
      if(term) PRINTOP3(>>);
      else PRINTOP3(&gt;&gt;);
      break;
    case AND_U:
      if(term) PRINTOP3(&);
      else PRINTOP3(&amp;);
      break;
    case OR_U:
      PRINTOP3(|);
      break;
    case XOR_U:
      PRINTOP3(^);
      break;
    case NOT_U:
      PRINTOP2(~);
      break;
    case NEG_I: case NEG_F: 
      PRINTOP2(-);
      break;
    case ADDR_3: /*not sure if I is needed*/
      PRINTOP2(&);
      break;
    case EQ_U: case EQ_F: 
      PRINTOP3(==);
      break;
    case NE_U: case NE_F: 
      PRINTOP3(!=);
      break;
    case GE_U: case GE_I: case GE_F: 
      if(term) PRINTOP3(>=);
      else PRINTOP3(&gt;=);
      break;
    case LE_U: case LE_I: case LE_F: 
      if(term) PRINTOP3(<=);
      else PRINTOP3(&lt;=);
      break;
    case GT_U: case GT_I: case GT_F: 
      if(term) PRINTOP3(>);
      else PRINTOP3(&gt;);
      break;
    case LT_U: case LT_I: case LT_F: 
      if(term) PRINTOP3(<);
      else PRINTOP3(&lt;);
      break;
    case BEQ_U: case BEQ_F:
      PRINTBROP2(==);
      break;
    case BGE_U: case BGE_I: case BGE_F: 
      if(term) PRINTBROP2(>=);
      else PRINTBROP2(&gt;=);
      break;
    case BGT_U: case BGT_I: case BGT_F: 
      if(term) PRINTBROP2(>);
      else PRINTBROP2(&gt;);
      break;
    case BNZ_3: case BEZ_3: case ARG_3: case PARAM_3: 
    case INIT_3: case DEALOC:
      PRINTOP1( );
      break;
    case RET_3: 
      if(!(op->addr0_type & GARBAGEVAL)) PRINTOP1( );
      break;
    case JEQ_I:
      PRINTOP3(==);
      break;
    case CALL_3:
    case F2I: case I2F: case F2F:
    case MOV_3: case ALOC_3:
      PRINTOP2( );
      break;
    case ARROFF:
      printaddr(op->addr0, op->addr0_type, term, f, prog);
      fprintf(f, "[");
      printaddr(op->addr1, op->addr1_type, term, f, prog);
      fprintf(f, "] ");
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, term, f, prog);
      break;
    case ARRMOV:
      printaddr(op->addr0, op->addr0_type, term, f, prog);
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, term, f, prog);
      fprintf(f, "[");
      printaddr(op->addr1, op->addr1_type, term, f, prog);
      fprintf(f, "] ");
      break;
    case MTP_OFF:
      printaddr(op->addr0, op->addr0_type, term, f, prog);
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, term, f, prog);
      fprintf(f, " + ");
      printaddr(op->addr1, op->addr1_type, term, f, prog);
      break;
    case PHI:
      for(int i = 0; i < blk->inedges->length; i++) {
        printaddr(op->addr0.joins[i].addr, op->addr0.joins[i].addr_type, term, f, prog);
        fprintf(f, ", ");
      }
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, term, f, prog);
      break;
    case ASM:
      assert(0); //unimplemented
  }
}

void printprog(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(!blk->lastop) continue;
    for(OPERATION* op = blk->firstop; op != blk->lastop->nextop; op = op->nextop) {
      printop(op, 1, blk, stdout, prog);
      fputc('\n', stdout);
    }
  }
  return;
}

void treeprog(PROGRAM* prog, char* fname, const char* pass) {
  mkdir("functions", 0777);
  char filen[256];
  sprintf(filen, "functions/%s_%s.dot", fname, pass);
  FILE* f = fopen(filen, "w");
  fprintf(f, "strict digraph %s {\n", fname);
  fprintf(f, "rankdir=TB\nnode [shape=none]\n");
  fprintf(f, "graph [bgcolor=gray12]\n");
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->nextblock)
      fprintf(f, "\"%p\" -> \"%p\" [color=blue]\n", blk, blk->nextblock);
    if(blk->branchblock)
      fprintf(f, "\"%p\" -> \"%p\" [color=red]\n", blk, blk->branchblock);
    if(!blk->lastop) {
      fprintf(f, "\"%p\" [xlabel=\"%d, %d\" fontcolor=white]", blk, blk->domind, blk->df ? blk->df->length : 0);
      continue;
    }
    fprintf(f, "\"%p\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" BGCOLOR=\"#353632\"><TR><TD><FONT COLOR=\"#e3f2e6\">", blk);
    for(OPERATION* op = blk->firstop; op != blk->lastop->nextop; op = op->nextop) {
      printop(op, 0, blk, f, prog);
      fprintf(f, "<BR ALIGN=\"LEFT\"/>");
    }
    fprintf(f, "</FONT></TD></TR></TABLE>> fontcolor=white xlabel=\"%d, %d\"]\n", blk->domind, blk->df ? blk->df->length : 0);
  }
  fprintf(f, "\n}");
  fclose(f);
  return;
}

static void freeop(OPERATION* op, OPERATION* stop) {
  while(1) {
    switch(op->opcode) {
      case LBL_3:
        free(op->addr0.labelname);
        break;
      case PHI:
        free(op->addr0.joins);
        break;
      default:
        break;
    }
    if(op == stop) break;
    OPERATION* nope = op;
    op = op->nextop;
    free(nope);
  }
  free(op);
}

void freeblock(void* blk) {
  BBLOCK* blk2 = blk;
  dadtor(blk2->inedges);
  if(blk2->idominates) dadtor(blk2->idominates);
  if(blk2->df) dadtor(blk2->df);
  if(blk2->lastop) freeop(blk2->firstop, blk2->lastop);
  if(blk2->anticipability_out) free(blk2->anticipability_out);
  if(blk2->anticipability_in) free(blk2->anticipability_in);
  if(blk2->availability) free(blk2->availability);
  free(blk);
}

void freeprog(PROGRAM* prog) {
  dadtorcfr(prog->allblocks, freeblock);
  dadtor(prog->breaklabels);
  dadtor(prog->continuelabels);
  dadtorfr(prog->dynvars);
  dadtor(prog->dynchars);
  htdtor(prog->labels);
  assert(prog->unfilledlabels->keys == 0);
  htdtor(prog->unfilledlabels);
  if(prog->closedblocks) dadtor(prog->closedblocks);
  free(prog);
}
