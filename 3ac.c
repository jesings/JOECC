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
#define FILLREG(addrvar, type) do { \
    (addrvar).addr.iregnum = prog->iregcnt++; \
    (addrvar).addr_type = (type); \
  } while(0)

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
    } 
    if(!(arg2id.tb & FLOATNUM)) {
      FULLADDR fad;
      FILLREG(fad, ISFLOAT | (retid.tb & 0xf));
      opn(prog, ct_3ac_op2(I2F, a2.addr_type, a2.addr, fad.addr_type, fad.addr));
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
    assert(!ispointer2(srcidt));
    if(destidt.tb & FLOATNUM) {
      op += 2;
      if(!(srcidt.tb & FLOATNUM)) {
        FULLADDR fad;
        FILLREG(fad, ISFLOAT | (srcidt.tb & 0xf));
        opn(prog, ct_3ac_op2(I2F, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        srcaddr = fad;
      }
    } else if(srcidt.tb & FLOATNUM) {
        FULLADDR fad;
        FILLREG(fad, ISFLOAT | (srcidt.tb & 0xf));
        opn(prog, ct_3ac_op2(I2F, destaddr.addr_type, destaddr.addr, fad.addr_type, fad.addr));
        opn(prog, ct_3ac_op3(op, fad.addr_type, fad.addr, srcaddr.addr_type, srcaddr.addr, fad.addr_type, fad.addr));
        opn(prog, ct_3ac_op2(F2I, fad.addr_type, fad.addr, destaddr.addr_type, destaddr.addr));
        return destaddr;
    } else {
        if(!(destidt.tb & UNSIGNEDNUM && srcidt.tb & UNSIGNEDNUM))
          op += 1;
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
    rid.pointerstack->length -= 1;
    curaddr.addr.uintconst_64 = lentype(&rid);
    rid.pointerstack->length += 1;
  } else {
    curaddr.addr.uintconst_64 = 1;
  }
  opn(prog, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, destaddr.addr_type, destaddr.addr, ISCONST | 8, curaddr.addr, destaddr.addr_type, destaddr.addr));
  return destaddr;
}
static FULLADDR poststep(char isinc, EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR destaddr, actualaddr, curaddr;
  IDTYPE rid = typex(cexpr);
  destaddr = linearitree(daget(cexpr->params, 0), prog);
  char baseness = destaddr.addr_type & ISFLOAT ? 2 : 0;
  FILLREG(actualaddr, destaddr.addr_type & ~(ISCONST | ISLABEL | ISDEREF | ISVAR));
  opn(prog, ct_3ac_op2(MOV_3, destaddr.addr_type, destaddr.addr, actualaddr.addr_type, actualaddr.addr));
  if(ispointer2(rid)) {
    rid.pointerstack->length -= 1;
    curaddr.addr.uintconst_64 = lentype(&rid);
    rid.pointerstack->length += 1;
  } else {
    curaddr.addr.uintconst_64 = 1;
  }
  opn(prog, ct_3ac_op3((isinc ? ADD_U : SUB_U) + baseness, destaddr.addr_type, destaddr.addr, ISCONST | 8, curaddr.addr, destaddr.addr_type, destaddr.addr));
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
  prog->curblock->nextblock = finalblock;
  dapushc(finalblock->inedges, prog->curblock);
  giveblock(prog, failblock);
  giveblock(prog, finalblock);
  addr2use.addr.iregnum = prog->iregcnt++;
  addr2use.addr_type = 1;
  opn(prog, ct_3ac_op3(TPHI, ISCONST | 1, shortcircuit_val, ISCONST | 1, complete_val, 1, addr2use.addr));
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
  FILLREG(adr, a1.addr_type & ~(ISCONST | ISLABEL | ISDEREF | ISVAR));
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

FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog) {
  FULLADDR curaddr, otheraddr, destaddr;
  IDTYPE varty;

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
    case ARRAY_LIT: ;
      struct declarator_part* ptrtop = dapeek(cexpr->rettype->pointerstack);
      FILLREG(destaddr, ISPOINTER | 0x8);
      assert(ptrtop->type == ARRAYSPEC);
      curaddr.addr.uintconst_64 = ptrtop->arrlen;
      opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
      if(!ptrtop->arrmaxind) ptrtop->arrmaxind = cexpr->params->length;
      curaddr.addr.uintconst_64 = ptrtop->arrlen / ptrtop->arrmaxind;
      if(curaddr.addr.uintconst_64 < 0xf) {
        for(int i = 0; i < cexpr->params->length; i++) {
          EXPRESSION* dyne = daget(cexpr->params, i);
          otheraddr = linearitree(dyne, prog);
          curaddr.addr.uintconst_64 = i;
          opn(prog, ct_3ac_op3(ARRMOV, otheraddr.addr_type, otheraddr.addr, ISCONST | 0x8, curaddr.addr, destaddr.addr_type, destaddr.addr));
        }
      } else {
        ADDRESS a;
        a.uintconst_64 = curaddr.addr.uintconst_64;
        for(int i = 0; i < cexpr->params->length; i++) {
          EXPRESSION* dyne = daget(cexpr->params, i);
          otheraddr = linearitree(dyne, prog);
          opn(prog, ct_3ac_op3(ADD_U, destaddr.addr_type, destaddr.addr, ISCONST | 0x8, a, destaddr.addr_type, destaddr.addr));
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
        opn(prog, ct_3ac_op3(MTP_OFF, curaddr.addr_type, curaddr.addr, ISCONST | 0x8, otheraddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;
    case NEG:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & ~(ISCONST | ISLABEL | ISDEREF | ISVAR);
      destaddr.addr.iregnum = prog->iregcnt++;
      if(destaddr.addr_type & ISFLOAT) {
        opn(prog, ct_3ac_op2(NEG_F, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      } else {
        opn(prog, ct_3ac_op2(NEG_I, curaddr.addr_type, curaddr.addr, destaddr.addr_type, destaddr.addr));
      }
      return destaddr;
    case L_NOT:
      //TODO: validate lots of types
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      FILLREG(destaddr, (curaddr.addr_type & ISSIGNED) | 1);
      //logical not only makes sense for ints
      otheraddr.addr.uintconst_64 = 0;
      opn(prog, ct_3ac_op3(EQ_U, curaddr.addr_type, curaddr.addr, (curaddr.addr_type & 0xf) | ISCONST, otheraddr.addr,
                                   destaddr.addr_type, destaddr.addr));
      return destaddr;
    case B_NOT:
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr_type = curaddr.addr_type & ~(ISCONST | ISLABEL | ISDEREF | ISVAR);
      assert(!(destaddr.addr_type & ISFLOAT));
      destaddr.addr.iregnum = prog->iregcnt++;
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
        if(dclp->type == ARRAYSPEC)
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
      if(dclp->type == ARRAYSPEC) {
        varty = typex(cexpr);
        if(ispointer2(varty)) {
          struct declarator_part* dclp2 = dapeek(varty.pointerstack);
          if(dclp2->type != ARRAYSPEC) {
            goto REALDEREF;
          }
        } else {
          goto REALDEREF;
        }
      } else {
        REALDEREF:
        if(destaddr.addr_type & ISDEREF) {
          FILLREG(otheraddr, destaddr.addr_type & ~(ISCONST | ISLABEL | ISDEREF | ISVAR));
          opn(prog, ct_3ac_op2(MOV_3, destaddr.addr_type, destaddr.addr, otheraddr.addr_type, otheraddr.addr));
          destaddr = otheraddr;
        }
        destaddr.addr_type |= ISDEREF;
      }
      return destaddr;
    case ADD:
      return op2ret(prog, implicit_binary_3(ADD_U, cexpr, prog));
    case SUB: 
      return op2ret(prog, implicit_binary_3(SUB_U, cexpr, prog));
    case MULT:
      return op2ret(prog, implicit_binary_3(MULT_U, cexpr, prog));
    case DIVI: 
      return op2ret(prog, implicit_binary_3(DIV_U, cexpr, prog));

    case EQ:
      return op2ret(prog, cmpret_binary_3(EQ_U, cexpr, prog));
    case NEQ: 
      return op2ret(prog, cmpret_binary_3(NE_U, cexpr, prog));
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
      curaddr = linearitree(daget(cexpr->params, 0), prog);
      destaddr.addr.uintconst_64 = lentype(&varty);
      destaddr.addr_type = ISCONST;
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
      giveblock(prog, succblock);
      curaddr = linearitree(daget(cexpr->params, 1), prog);
      if(!(t1t.tb & FLOATNUM) && (t2t.tb & FLOATNUM)) {
        FULLADDR ad2;
        FILLREG(ad2, (t0t.tb & 0xf) | ISFLOAT | ISSIGNED);
        opn(prog, ct_3ac_op2(I2F, curaddr.addr_type, curaddr.addr, ad2.addr_type, ad2.addr));
        curaddr = ad2;
      }
      topblock = dapeek(prog->allblocks);
      if(!topblock->nextblock) {
        topblock->nextblock = joinblock;
        dapushc(joinblock->inedges, topblock);
      }
      giveblock(prog, failblock);
      otheraddr = linearitree(daget(cexpr->params, 2), prog);
      if((t1t.tb & FLOATNUM) && !(t2t.tb & FLOATNUM)) {
        FULLADDR ad2;
        FILLREG(ad2, (t0t.tb & 0xf) | ISFLOAT | ISSIGNED);
        opn(prog, ct_3ac_op2(I2F, otheraddr.addr_type, otheraddr.addr, ad2.addr_type, ad2.addr));
        otheraddr = ad2;
      }
      assert((curaddr.addr_type & ISFLOAT) == (otheraddr.addr_type & ISFLOAT)); //confirm 2 addrs have same type or are coercible
      destaddr.addr_type = addrconv(&t3t);
      destaddr.addr.iregnum = prog->iregcnt++;
      giveblock(prog, joinblock);
      opn(prog, ct_3ac_op3(TPHI, curaddr.addr_type, curaddr.addr, otheraddr.addr_type, otheraddr.addr, destaddr.addr_type, destaddr.addr));
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
       //type coercion stupid and unclear
    case NOP: case MEMBER:
      assert(0);
    case SZOF:
      varty = *cexpr->vartype;
      destaddr.addr_type = ISCONST | 8;
      if(ispointer2(varty)) {
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
      OPERATION* fparam,* lparam;
      EXPRESSION* fname = daget(cexpr->params, 0);
      if(cexpr->params->length > 1) {
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
      if(ispointer(frettype) || frettype->tb & (STRUCTVAL | UNIONVAL)) {
        FILLREG(destaddr, ISPOINTER | 8);
      } else if(frettype->tb & FLOATNUM) {
        FILLREG(destaddr, ISFLOAT | (frettype->tb & 0xf));
      } else if(frettype->tb & UNSIGNEDNUM) {
        FILLREG(destaddr, frettype->tb & 0xf);
      } else {
        FILLREG(destaddr, ISSIGNED | (frettype->tb & 0xf));
      }
      opn(prog, ct_3ac_op2(CALL_3, ISCONST | ISLABEL, (ADDRESS) fname->id->name, destaddr.addr_type, destaddr.addr));
      //TODO: copy out struct type
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
      dest_op = cmpret_binary_3(cmp_osite(cmpexpr->type), cmpexpr, prog);//figure out signedness here or elsewhere
      --prog->iregcnt; //dealloc allocated register, ignore third operand
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
          tmpaddr.intconst_64 = dclp->arrlen;
          opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, tmpaddr, newa->addr_type, newa->addr));
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
          opn(prog, ct_3ac_op2(MOV_3, lastemp.addr_type, lastemp.addr, newa->addr_type, newa->addr));
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
        opn(prog, ct_3ac_op0(RET_0));
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
    if(!ispointer(pdec->type) && (pdec->type->tb & (STRUCTVAL | UNIONVAL) )) {
      ADDRESS tmpaddr, tmpaddr2;
      tmpaddr.intconst_64 = pdec->type->structtype->size;
      tmpaddr2.iregnum = prog->iregcnt++;
      opn(prog, ct_3ac_op2(ALOC_3, ISCONST | 8, tmpaddr, newa->addr_type & ~ISVAR, tmpaddr2));
      opn(prog, ct_3ac_op3(COPY_3, newa->addr_type | ISDEREF, newa->addr, ISCONST, tmpaddr, (newa->addr_type & ~ISVAR) | ISDEREF, tmpaddr2));
      opn(prog, ct_3ac_op2(MOV_3, newa->addr_type & ~ISVAR, tmpaddr2, newa->addr_type, newa->addr));
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
  return prog;
}

static void printaddr(ADDRESS addr, ADDRTYPE addr_type, char color, FILE* f, PROGRAM* prog) {
  if(addr_type & ISLABEL) {
    if(color) fprintf(f, RGBCOLOR(255,200,10));
    else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 255, 200, 10);
    if(addr_type & ISDEREF) fprintf(f, "({%s})", addr.labelname);
    else                    fprintf(f, "{%s}", addr.labelname);
    if(!(addr_type & ISCONST)) {
      if(addr_type & ISFLOAT) fprintf(f, ".%df", (addr_type & 0xf) * 8);
      else                    fprintf(f, ".%d%c", (addr_type & 0xf) * 8, addr_type & ISSIGNED ? 's' : 'u');
    }
    if(color) fprintf(f, CLEARCOLOR);
    else fprintf(f, "</FONT>");
  } else if(addr_type & ISCONST) {
    assert(!(addr_type & ISDEREF));
    if(addr_type & ISSTRCONST) {
      int l = strlen(addr.strconst);
      if(color) fprintf(f, RGBCOLOR(90,180,180));
      else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 90, 180, 180);
      if(!l)
        fprintf(f, "\"\"");
      else if(addr.strconst[l - 1] != '\n')
        fprintf(f, "\"%s\"", addr.strconst);
      else
        fprintf(f, "\"%.*s\\n\"", l - 1, addr.strconst);
      if(color) fprintf(f, CLEARCOLOR);
      else fprintf(f, "</FONT>");
    } else {
      if(color) fprintf(f, RGBCOLOR(250,60,60));
      else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 250, 60, 60);
      if(addr_type & ISFLOAT) 
        fprintf(f, "%lf", addr.floatconst_64);
      else if(addr_type & ISSIGNED) 
        fprintf(f, "%ld", addr.intconst_64);
      else
        fprintf(f, "%lu", addr.intconst_64);
      if(color) fprintf(f, CLEARCOLOR);
      else fprintf(f, "</FONT>");
    }
  } else {
    int sz = (addr_type & 0xf) * 8;
    if(color) fprintf(f, RGBCOLOR(60,220,60));
    else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 60, 220, 60);
    if(addr_type & ISVAR) {
      char* adname = daget(prog->dynchars, addr.varnum);
      if(prog->pdone & SSA && !(addr_type & ADDRSVAR)) {
        if(addr_type & ISDEREF) fprintf(f, "(%s_%u)", adname, addr.ssaind);
        else                    fprintf(f, "%s_%u", adname, addr.ssaind);
      } else {
        if(addr_type & ISDEREF) fprintf(f, "(%s)", adname);
        else                    fprintf(f, "%s", adname);
      }
    } else {
      if(addr_type & ISDEREF) fprintf(f, "(ireg%u)", addr.iregnum);
      else                    fprintf(f, "%creg%u", addr_type & ISFLOAT ? 'f' : 'i', addr.iregnum);
    }
    fprintf(f, ".%d%c", sz, addr_type & ISFLOAT ? 'f' : addr_type & ISSIGNED ? 's' : 'u');
    if(color) fprintf(f, CLEARCOLOR);
    else fprintf(f, "</FONT>");
  }
}

#define PRINTBROP2(opsymb) do { \
    printaddr(op->addr0, op->addr0_type, color, f, prog); \
    fprintf(f, " %s ", #opsymb); \
    printaddr(op->addr1, op->addr1_type, color, f, prog); \
  } while(0)

#define PRINTOP3(opsymb) do { \
    printaddr(op->addr0, op->addr0_type, color, f, prog); \
    fprintf(f, " " #opsymb " "); \
    printaddr(op->addr1, op->addr1_type, color, f, prog); \
    fprintf(f, " →  "); \
    printaddr(op->dest, op->dest_type, color, f, prog); \
  } while(0)

#define PRINTOP2(opsymb) do { \
    fprintf(f, "%s", #opsymb); \
    printaddr(op->addr0, op->addr0_type, color, f, prog); \
    fprintf(f, " →  "); \
    printaddr(op->dest, op->dest_type, color, f, prog); \
  } while(0)

#define PRINTOP1() do { \
    printaddr(op->addr0, op->addr0_type, color, f, prog); \
  } while(0)

static void printop(OPERATION* op, char color, BBLOCK* blk, FILE* f, PROGRAM* prog) {
  fprintf(f, "%s", opcode_3ac_names[op->opcode]);
  if(color) fprintf(f, "\t");
  else      fprintf(f, "&nbsp;");
  switch(op->opcode) {
    case NOP_3: case RET_0:
      break;
    case LBL_3: 
      if(color) fprintf(f, RGBCOLOR(200,200,120));
      else fprintf(f, "<FONT COLOR=\"#%.2hhx%.2hhx%.2hhx\">", 200, 200, 120);
      fprintf(f, "%s:", op->addr0.labelname);
      if(color) fprintf(f, CLEARCOLOR);
      else fprintf(f, "</FONT>");
      break;
    case COPY_3: //TODO: make sure deref is safe
      PRINTOP3( );
      break;
    case ADD_U: case ADD_I: case ADD_F: 
      PRINTOP3(+);
      break; case SUB_U: case SUB_I: case SUB_F: 
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
      if(color) PRINTOP3(<<);
      else PRINTOP3(&lt;&lt;);
      break;
    case SHR_U: case SHR_I: 
      if(color) PRINTOP3(>>);
      else PRINTOP3(&gt;&gt;);
      break;
    case AND_U:
      if(color) PRINTOP3(&);
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
    case EQ_U: case EQ_I: case EQ_F: 
      PRINTOP3(==);
      break;
    case NE_U: case NE_I: case NE_F: 
      PRINTOP3(!=);
      break;
    case GE_U: case GE_I: case GE_F: 
      if(color) PRINTOP3(>=);
      else PRINTOP3(&gt;=);
      break;
    case LE_U: case LE_I: case LE_F: 
      if(color) PRINTOP3(<=);
      else PRINTOP3(&lt;=);
      break;
    case GT_U: case GT_I: case GT_F: 
      if(color) PRINTOP3(>);
      else PRINTOP3(&gt;);
      break;
    case LT_U: case LT_I: case LT_F: 
      if(color) PRINTOP3(<);
      else PRINTOP3(&lt;);
      break;
    case BEQ_U: case BEQ_I: case BEQ_F: 
      PRINTBROP2(==);
      break;
    case BGE_U: case BGE_I: case BGE_F: 
      if(color) PRINTBROP2(>=);
      else PRINTBROP2(&gt;=);
      break;
    case BGT_U: case BGT_I: case BGT_F: 
      if(color) PRINTBROP2(>);
      else PRINTBROP2(&gt;);
      break;
    case BNZ_3: case BEZ_3: case JMP_3:
    case ARG_3: case PARAM_3: case RET_3: case INIT_3:
      PRINTOP1( );
      break;
    case JEQ_I:
      PRINTOP3(==);
      break;
    case CALL_3:
    case F2I: case I2F:
    case MOV_3: case ALOC_3:
      PRINTOP2( );
      break;
    case ARROFF:
      printaddr(op->addr0, op->addr0_type, color, f, prog);
      fprintf(f, "[");
      printaddr(op->addr1, op->addr1_type, color, f, prog);
      fprintf(f, "] ");
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, color, f, prog);
      break;
    case ARRMOV:
      printaddr(op->addr0, op->addr0_type, color, f, prog);
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, color, f, prog);
      fprintf(f, "[");
      printaddr(op->addr1, op->addr1_type, color, f, prog);
      fprintf(f, "] ");
      break;
    case MTP_OFF:
      printaddr(op->addr0, op->addr0_type, color, f, prog);
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, color, f, prog);
      fprintf(f, " + ");
      printaddr(op->addr1, op->addr1_type, color, f, prog);
      break;
    case PHI:
      for(int i = 0; i < blk->inedges->length; i++) {
        printaddr(op->addr0.joins[i].addr, op->addr0.joins[i].addr_type, color, f, prog);
        fprintf(f, ", ");
      }
      fprintf(f, " →  ");
      printaddr(op->dest, op->dest_type, color, f, prog);
      break;
    case TPHI:
      PRINTOP3( or );
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
  free(prog);
}
