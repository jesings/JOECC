#include <stdint.h>
#include "joecc_assert.h"
#include "opt.h"
#include "ssa.h"

//marks a block as unreachable by setting its dominance index to a dummy value of -1,
//and eliminates all edges going from it
void domark(BBLOCK* blk) {
  blk->domind = -1;
  if(blk->nextblock) {
    dharma(blk->nextblock->inedges, blk);
    if(blk->nextblock->inedges == 0) domark(blk->nextblock);
  }
  if(blk->branchblock) {
    dharma(blk->branchblock->inedges, blk);
    if(blk->branchblock->inedges == 0) domark(blk->branchblock);
  }
}

//removes all blocks that have been marked as unreachable
void rmunreach(PROGRAM* prog) {
  DYNARR* oldall = prog->allblocks;
  DYNARR* newall = dactor(oldall->length);
  dapush(newall, daget(oldall, 0));
  for(int i = 1; i < oldall->length; i++) {
    BBLOCK* oldb = daget(oldall, i);
    if(oldb->domind == -1) {
      freeblock(oldb);
      if(oldb == prog->finalblock) prog->finalblock = NULL; //maybe not?
    } else {
      dapush(newall, oldb);
    }
  }
  prog->allblocks = newall;
  dadtor(oldall);
}
//http://www.cs.toronto.edu/~pekhimenko/courses/cscd70-w20/docs/Lecture 7 [Pointer Analysis] 03.09.2020.pdf
//http://ssabook.gforge.inria.fr/latest/book.pdf
//https://iitd-plos.github.io/col729/lec/loop_transformations.html

//folds in blocks that only have one predecessor and one successor, while trying to maintain simplified block form (i.e. without creating any critical edges)
void blockunblock(PROGRAM* prog) {
  for(int i = 1; i < prog->allblocks->length - 1; i++) {
    BBLOCK* curblock = daget(prog->allblocks, i);
    if(curblock->inedges->length == 1) {
      if(!curblock->branchblock && curblock->nextblock) {
        BBLOCK* prevblock = daget(curblock->inedges, 0);
        assert(prevblock != curblock);
        if(!prevblock->branchblock) {
          if(prevblock->lastop) {
            if(curblock->lastop) {
              prevblock->lastop->nextop = curblock->firstop;
              prevblock->lastop = curblock->lastop;
            }
          } else {
            if(curblock->lastop) {
              prevblock->firstop = curblock->firstop;
              prevblock->lastop = curblock->lastop;
            }
          }
          prevblock->nextblock = curblock->nextblock;
          curblock->lastop = NULL;
          prevblock->postdom = curblock->postdom;
          prevblock->postdomind = curblock->postdomind; //shouldn't be strictly necessary
          darpa(curblock->nextblock->inedges, curblock, prevblock);
          DYNARR* swaptmp = prevblock->idominates;
          prevblock->idominates = curblock->idominates;
          curblock->idominates = swaptmp;//to be freed by freeblock from marking
          curblock->nextblock = NULL;
          //dominance frontiers must be the same
          domark(curblock);
        }
      }
    }
  }
}

//This should be an extremely self explanatory function
char remove_nops(PROGRAM* prog) {
  DYNARR* da = prog->allblocks;
  for(int i = 0; i < da->length; i++) {
    BBLOCK* b = daget(da, i);
    if(!b->lastop) continue;
    OPERATION* op = b->firstop;
    while(op->opcode == NOP_3 && op != b->lastop) {
      OPERATION* prevop = op;
      op = op->nextop;
      free(prevop);
    } //first ops
    b->firstop = op;
    OPERATION* lastop;
    if(op == b->lastop) {
      lastop = NULL;
    } else {
      lastop = op;
      op = op->nextop;
      while(op != b->lastop) {
        if(op->opcode == NOP_3) {
          lastop->nextop = op->nextop;
          free(op);
          op = lastop;
        } else {
          lastop = op;
        }
        op = op->nextop;
      }
    }
    if(op->opcode == NOP_3) {
      free(op);
      b->lastop = lastop;
    } //last op
  }
  return 0;
}

//checks addr0 and addr1 of op for equality
static char feq(OPERATION* op) {
  if(op->addr0_type != op->addr1_type) return 0;
  if(op->addr0_type & ISLABEL) return !strcmp(op->addr0.labelname, op->addr1.labelname);
  if(op->addr0_type & ISVAR) return op->addr0.varnum == op->addr1.varnum;
  return op->addr0.regnum == op->addr1.regnum;
}

#define PRUNE(check, condition) \
   if(check) { \
     blk->lastop->opcode = NOP_3; \
     if(condition) { \
       dharma(blk->nextblock->inedges, blk); \
       blk->nextblock = blk->branchblock; \
     } else { \
       if(blk->branchblock == prog->finalblock) continue;  \
       dharma(blk->branchblock->inedges, blk); \
     } \
     goto cleantrue; \
   }

#define PRUNEFEQ(check, condition) \
  PRUNE(check, condition) \
  else if(feq(blk->lastop)) { \
    blk->lastop->opcode = NOP_3; \
    dharma(blk->branchblock->inedges, blk); \
    goto cleantrue; \
  }

#define PRUNEFEQBR(check, condition) \
  PRUNE(check, condition) \
  else if(feq(blk->lastop)) { \
    blk->lastop->opcode = NOP_3; \
    dharma(blk->nextblock->inedges, blk); \
    blk->nextblock = blk->branchblock; \
    goto cleantrue; \
  }

//largely boiler plate, just checks if branch evaluates, then gets rid of untaken branch
void prunebranch(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      switch(blk->lastop->opcode) {
        case BNZ_3:
          PRUNE(blk->lastop->addr0_type & ISCONST, blk->lastop->addr0.intconst_64 != 0)
          break;
        case BEZ_3:
          PRUNE(blk->lastop->addr0_type & ISCONST, blk->lastop->addr0.intconst_64 == 0)
          break;
        case BEQ_U:
          PRUNEFEQ(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                   blk->lastop->addr0.intconst_64 == blk->lastop->addr1.intconst_64)
          break;
        case BEQ_F:
          PRUNEFEQ(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                   blk->lastop->addr0.floatconst_64 == blk->lastop->addr1.floatconst_64)
          break;
        case BGT_U:
          PRUNEFEQBR(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                     blk->lastop->addr0.uintconst_64 > blk->lastop->addr1.uintconst_64)
          break;
        case BGT_I:
          PRUNEFEQBR(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                     blk->lastop->addr0.intconst_64 > blk->lastop->addr1.intconst_64)
          break;
        case BGT_F:
          PRUNEFEQBR(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                     blk->lastop->addr0.floatconst_64 > blk->lastop->addr1.floatconst_64)
          break;
        case BGE_U:
          PRUNEFEQ(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                   blk->lastop->addr0.uintconst_64 >= blk->lastop->addr1.uintconst_64)
          break;
        case BGE_I:
          PRUNEFEQ(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                   blk->lastop->addr0.intconst_64 >= blk->lastop->addr1.intconst_64)
          break;
        case BGE_F:
          PRUNEFEQ(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST,
                   blk->lastop->addr0.floatconst_64 >= blk->lastop->addr1.floatconst_64)
          break;
        default:
          break;
      }
    }
    continue;
cleantrue:
    blk->branchblock = NULL;
  }
}

//binary constant fold breakless
#define bincfbl(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = MOV_3; \
              op->addr0.constkind operator op->addr1.constkind; \
            }
//binary constant fold
#define bincf(constkind, operator) \
            bincfbl(constkind, operator) \
            break;
//binary equality fold
#define binef(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = MOV_3; \
              op->addr0.constkind = op->addr0.constkind operator op->addr1.constkind; \
            } \
            break;
//unary constant fold
#define uncf(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = MOV_3; \
              op->addr0.constkind = operator op->addr1.constkind; \
            } \
            break;
//branch constant fold
#define brcf(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = BNZ_3; \
              op->addr0.constkind = op->addr0.constkind operator op->addr1.constkind; \
            } \
            break;
//if the last operand is some value, evaluate to some passed in value (ident)
#define last1(constkind, floatness, ident) \
            if((op->addr1_type & (ISCONST | (floatness))) && (op->addr1.constkind == ident)) { \
              op->opcode = MOV_3; \
              break; \
            }
//if the first operand is some value, evaluate to some passed in value (ident)
#define first1(constkind, floatness, ident) \
            if((op->addr0_type & (ISCONST | (floatness))) && (op->addr0.constkind == ident)) { \
              op->addr0_type = op->addr1_type; \
              op->addr0 = op->addr1; \
              op->opcode = MOV_3; \
              break; \
            }
/*
#define last1z(constkind, floatness, ident) \
            if((op->addr1_type & (ISCONST | (floatness))) && (op->addr1.constkind == ident)) { \
              op->opcode = MOV_3; \
              break; \
            }
#define first1z(constkind, floatness, ident) \
            if((op->addr1_type & (ISCONST | (floatness))) && (op->addr1.constkind == ident)) { \
              op->opcode = MOV_3; \
              break; \
            }
*/

char constfold(PROGRAM* prog) {
  LOOPALLBLOCKS(
    switch(op->opcode) {
      default:
        break;
      case ADD_U:
        last1(intconst_64, 0, 0);
        first1(intconst_64, 0, 0);
        bincfbl(intconst_64, +=)
        else if(op->addr0.uintconst_64 == op->addr1.uintconst_64) {
          if((op->addr0_type & (ISDEREF | ISLABEL)) == (op->addr1_type & (ISDEREF | ISLABEL))) {
            op->opcode = SHL_U;
            op->addr1_type = ISCONST | 0x1;
            op->addr1.uintconst_64 = 0x1;
          }
        }
        break;
      case ADD_F:
        bincf(floatconst_64, +=);
      case SUB_U:
        if(feq(op)) {
          op->opcode = MOV_3;
          op->addr0.intconst_64 = 0;
          break;
        }
        last1(intconst_64, 0, 0);
        if((op->addr0_type & ISCONST) && (op->addr0.intconst_64 == 0)) {
          op->opcode = NEG_I;
          break;
        }
        bincf(intconst_64, -=);
      case SUB_F:
        bincf(floatconst_64, -=);
      case MULT_U:
        last1(uintconst_64, 0, 1);
        first1(uintconst_64, 0, 1);
        bincfbl(uintconst_64, *=)
        else if(op->addr0_type & ISCONST) {
          unsigned int pow2 = 0;
          unsigned long shifty = op->addr0.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            ++pow2;
            shifty >>= 1;
          }
          if(shifty == 0) break;
          op->addr0 = op->addr1;
          op->addr0_type = op->addr1_type;
          op->addr1_type = ISCONST | 0x1;
          op->addr1.uintconst_64 = pow2;
          op->opcode = SHL_U;
        } else if(op->addr1_type & ISCONST) {
          unsigned int pow2 = 0;
          unsigned long shifty = op->addr1.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            ++pow2;
            shifty >>= 1;
          }
          if(shifty == 0) break;
          op->addr1_type = ISCONST | 0x1;
          op->addr1.uintconst_64 = pow2;
          op->opcode = SHL_U;
        }
        break;
      case MULT_I:
        last1(intconst_64, 0, 1);
        first1(intconst_64, 0, 1);
        bincfbl(intconst_64, *=)
        else if(op->addr0_type & ISCONST) {
          unsigned int pow2 = 0;
          unsigned long shifty = op->addr0.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            ++pow2;
            shifty >>= 1;
          }
          if(shifty == 0) break;
          op->addr0 = op->addr1;
          op->addr0_type = op->addr1_type;
          op->addr1_type = ISCONST | 0x1;
          op->addr1.uintconst_64 = pow2;
          op->opcode = SHL_I;
        } else if(op->addr1_type & ISCONST) {
          unsigned int pow2 = 0;
          unsigned long shifty = op->addr1.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            ++pow2;
            shifty >>= 1;
          }
          if(shifty == 0) break;
          op->addr1_type = ISCONST | 0x1;
          op->addr1.uintconst_64 = pow2;
          op->opcode = SHL_I;
        }
        break;
      case MULT_F:
        bincf(floatconst_64, *=);
      case DIV_U:
        last1(uintconst_64, 0, 1);
        if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
          if(op->addr0.uintconst_64 == 0) break;
          op->opcode = MOV_3;
          op->addr0.uintconst_64 /= op->addr1.uintconst_64;
        } else if(op->addr1_type & ISCONST) {
          unsigned int pow2 = 0;
          unsigned long shifty = op->addr1.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            ++pow2;
            shifty >>= 1;
          }
          if(shifty == 0) break;
          op->addr1_type = ISCONST | 0x1;
          op->addr1.uintconst_64 = pow2;
          op->opcode = SHR_U;
        }
        break;
      case DIV_I:
        last1(intconst_64, 0, 1);
        if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
          if(op->addr0.intconst_64 == 0) break;
          if(op->addr0.intconst_64 == INT64_MIN && op->addr1.intconst_64 == -1) break;
          op->opcode = MOV_3;
          op->addr0.intconst_64 /= op->addr1.intconst_64;
        } else if(op->addr1_type & ISCONST) {
          unsigned int pow2 = 0;
          unsigned long shifty = op->addr1.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            ++pow2;
            shifty >>= 1;
          }
          if(shifty == 0) break;
          op->addr1_type = ISCONST | 0x1;
          op->addr1.uintconst_64 = pow2;
          op->opcode = SHR_I;
        }
        break;
      case DIV_F:
        bincf(floatconst_64, /=);
      case MOD_U:
        last1(uintconst_64, 0, 1);
        if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
          if(op->addr0.uintconst_64 == 0) break;
          op->opcode = MOV_3;
          op->addr0.uintconst_64 %= op->addr1.uintconst_64;
        } else if(op->addr1_type & ISCONST) {
          unsigned long shifty = op->addr1.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            shifty >>= 1;
          }
          if(shifty == 0) break;
          //if the modulus we are taking is a power of 2, finagle it
          op->addr1_type = ISCONST | 0x8;
          op->addr1.uintconst_64 -= 1;
          op->opcode = AND_U;
        }
        break;
      case MOD_I:
        last1(intconst_64, 0, 1);
        if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
          if(op->addr0.intconst_64 == 0) break;
          if(op->addr0.intconst_64 == INT64_MIN && op->addr1.intconst_64 == -1) break;
          op->opcode = MOV_3;
          op->addr0.intconst_64 %= op->addr1.intconst_64;
        } else if(op->addr1_type & ISCONST) {
          unsigned long shifty = op->addr1.intconst_64;
          while(shifty > 1) {
            if(shifty & 1) {
              shifty = 0;
              break;
            }
            shifty >>= 1;
          }
          if(shifty == 0) break;
          //if the modulus we are taking is a power of 2, finagle it
          op->addr1_type = ISCONST | 0x8;
          op->addr1.uintconst_64 -= 1;
          op->opcode = AND_U;
        }
        break;
      case AND_U:
        last1(uintconst_64, 0, 0xffffffffffffffffL);
        first1(uintconst_64, 0, 0xffffffffffffffffL);
        bincfbl(uintconst_64, &=)
        else if(feq(op)) {
          op->opcode = MOV_3;
        }
        break;
      case OR_U:
        last1(uintconst_64, 0, 0);
        first1(uintconst_64, 0, 0);
        bincfbl(uintconst_64, |=)
        else if(feq(op)) {
          op->opcode = MOV_3;
        }
        break;
      case XOR_U:
        last1(uintconst_64, 0, 0);
        first1(uintconst_64, 0, 0);
        bincfbl(uintconst_64, ^=)
        else if(feq(op)) {
          op->opcode = MOV_3;
          op->addr0_type = ISCONST | 0x8;
          op->addr0.uintconst_64 = 0;
        }
        break;
      case EQ_U:
        binef(intconst_64, ==);
      case EQ_F:
        binef(floatconst_64, ==);
      case NE_U:
        binef(intconst_64, !=);
      case NE_F:
        binef(floatconst_64, !=);
      case SHL_U:
        last1(uintconst_64, 0, 0);
        bincf(uintconst_64, <<=);
      case SHL_I:
        last1(intconst_64, 0, 0);
        bincf(intconst_64, <<=);
      case SHR_U:
        last1(uintconst_64, 0, 0);
        bincf(uintconst_64, >>=);
      case SHR_I:
        last1(intconst_64, 0, 0);
        bincf(intconst_64, >>=);
      case GE_U:
        binef(uintconst_64, >=);
      case GE_I:
        binef(intconst_64, >=);
      case GE_F:
        binef(floatconst_64, >=);
      case LE_U:
        binef(uintconst_64, <=);
      case LE_I:
        binef(intconst_64, <=);
      case LE_F:
        binef(floatconst_64, <=);
      case GT_U:
        binef(uintconst_64, >);
      case GT_I:
        binef(intconst_64, >);
      case GT_F:
        binef(floatconst_64, >);
      case LT_U:
        binef(uintconst_64, <);
      case LT_I:
        binef(intconst_64, <);
      case LT_F:
        binef(floatconst_64, <);
      case NOT_U:
        uncf(uintconst_64, ~);
      case NEG_I:
        uncf(intconst_64, -);
      case NEG_F:
        uncf(floatconst_64, -);
      case F2I:
        if(op->addr0_type & ISCONST) {
          op->opcode = MOV_3;
          op->addr0.intconst_64 = (long int) op->addr0.floatconst_64;
          op->addr0_type = op->dest_type | ISCONST;
          op->addr0_type &= ~ISFLOAT;
        }
        break;
      case I2F:
        if(op->addr0_type & ISCONST) {
          op->opcode = MOV_3;
          if(op->addr0_type & ISSIGNED)
            op->addr0.floatconst_64 = (double) op->addr0.intconst_64;
          else
            op->addr0.floatconst_64 = (double) op->addr0.uintconst_64;
          op->addr0_type |= ISSIGNED | ISFLOAT;
        }
        break;
      case BEQ_U:
        brcf(intconst_64, ==);
      case BEQ_F:
        brcf(floatconst_64, ==);
      case BGE_U:
        brcf(intconst_64, >=);
      case BGE_I:
        brcf(intconst_64, >=);
      case BGE_F:
        brcf(floatconst_64, >=);
      case BGT_U:
        brcf(uintconst_64, >);
      case BGT_I:
        brcf(intconst_64, >);
      case BGT_F:
        brcf(floatconst_64, >);
    }
  )
  return 0;
}

int countops(PROGRAM* prog) {
  int opcount = 0;
  LOOPALLBLOCKS(
    ++opcount;
  );
  return opcount;
}

/**
 * Splits blocks up so as to break up any critical edges. A critical edge is one coming from a block with more than one
 * outbound edge, and going to a block with more than one inbound edge. The resulting CFG is said to be in simplified form
**/
void splitcrit(PROGRAM* prog) {
  int blen = prog->allblocks->length;
  for(int i = 0; i < blen; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->branchblock) {//two or more successors
      if(blk->nextblock->inedges->length > 1) {//successor with two or more predecessors
        BBLOCK* blka = calloc(1, sizeof(BBLOCK));
        blka->nextblock = blk->nextblock;
        blk->nextblock = blka;
        dapush(prog->allblocks, blka);
        blka->inedges = dactor(1);
        dapushc(blka->inedges, blk);
        darpa(blka->nextblock->inedges, blk, blka);
      }
      if(blk->branchblock->inedges->length > 1) {//successor with two or more predecessors
        BBLOCK* blka = calloc(1, sizeof(BBLOCK));
        blka->nextblock = blk->branchblock;
        blk->branchblock = blka;
        dapush(prog->allblocks, blka);
        blka->inedges = dactor(1);
        dapushc(blka->inedges, blk);
        darpa(blka->nextblock->inedges, blk, blka);
      }
    }
  }
}

//Does not yet but will perform tail call elimination optimization
void tailcall(PROGRAM* prog) {
  //assume prog actually returns a value if this is being called
  for(int i = 0; i < prog->finalblock->inedges->length; i++) {
    BBLOCK* retb = daget(prog->finalblock->inedges, i);
    OPERATION* op = retb->firstop;
    if(op == retb->lastop) continue;
    while(op->nextop != retb->lastop) {
      op = op->nextop;
    }
    if(op->opcode == CALL_3) {
      if(op->nextop->opcode == RET_3 && (op->nextop->addr0_type & ~0xf) == (op->dest_type & ~0xf) && op->nextop->addr0.intconst_64 == op->dest.intconst_64) {
        if(op->addr0_type & ISLABEL && !strcmp(op->addr0.labelname, ((BBLOCK*) daget(prog->allblocks, 0))->firstop->addr0.labelname)) {
          //tail recursion!! more optimization potential?
          //insert block immediately after arguments with phi nodes for all the params
        }
        //free(op->nextop);
        //op->nextop = NULL;
        //op->opcode = JMP_3;
        //retb->lastop = op;
        //TODO: wait until interprocedural or codegen
      }
    }
  }
}

void collatealloc(PROGRAM* prog) {
  unsigned long totalalloc = 0;
  FULLADDR baseptr;
  FILLREG(baseptr, 8 | ISPOINTER);
  LOOPALLBLOCKS(
    if(op->opcode == ALOC_3) {
      if(op->addr0_type & ISCONST) {
        unsigned long tmpstore = op->addr0.uintconst_64;
        op->opcode = ADD_U;
        op->addr1_type = op->addr0_type;
        op->addr1.uintconst_64 = totalalloc;
        op->addr0_type = baseptr.addr_type;
        op->addr0 = baseptr.addr;
        totalalloc += tmpstore;
      } else {
        DYNARR* allocfrontier = dactor(8);
        dapushc(allocfrontier, blk);
        while(allocfrontier->length) {
          BBLOCK* inquestion = dapop(allocfrontier);
          if(inquestion->lastop && inquestion->lastop->opcode == DEALOC
             && inquestion->lastop->addr0.regnum == op->dest.regnum) continue;
          //removal of critical edges should remove the issue that arises if one branch leaves dominance but the other doesn't
          if(inquestion->nextblock == prog->finalblock) {
            OPERATION* preretop = inquestion->firstop;
            OPERATION** insertloc;
            if(preretop == inquestion->lastop) {
              insertloc = &inquestion->firstop;
            } else {
              while(preretop->nextop != inquestion->lastop) preretop = preretop->nextop;
              insertloc = &preretop->nextop;
            }
            OPERATION* deallocop = ct_3ac_op1(DEALOC, op->dest_type, op->dest);
            //repetition would be handled by PRE
            deallocop->nextop = *insertloc;
            *insertloc = deallocop;
          } else {
            if(inquestion->nextblock->dom == inquestion || fixedintersect(blk, inquestion->nextblock)) {
              if(fixedintersect(inquestion->nextblock, blk)) {
                //nothing to be dealloced if the loop is infinite!
                puts("Infinite loop detected");
                //is this sufficient for this purpose?
              } else {
                dapush(allocfrontier, inquestion->nextblock);
                if(inquestion->branchblock)
                  dapush(allocfrontier, inquestion->branchblock);
              }
            } else {
              OPERATION* deallocop = ct_3ac_op1(DEALOC, op->dest_type, op->dest);
              //repetition would be handled by PRE
              if(inquestion->lastop) {
                inquestion->lastop->nextop = deallocop;
              } else {
                inquestion->firstop = deallocop;
              }
              inquestion->lastop = deallocop;
            }
          }
        }
        dadtor(allocfrontier);
      }
    }
  )
  if(totalalloc > 0) {
    ADDRESS alloccnt;
    alloccnt.uintconst_64 = totalalloc;
    OPERATION* epsilop = ct_3ac_op2(ALOC_3, ISCONST | 8, alloccnt, baseptr.addr_type, baseptr.addr);

    BBLOCK* firstblock = (BBLOCK*) daget(prog->allblocks, 0);
    OPERATION* fbop;
    for(fbop = firstblock->firstop; fbop->nextop->opcode == PARAM_3; fbop = fbop->nextop) ;

    epsilop->nextop = fbop->nextop;
    if(fbop == firstblock->lastop) firstblock->lastop = epsilop;
    fbop->nextop = epsilop;
#ifndef NODEBUG
    printf("total allocated %lu\n", totalalloc);
#endif
  }
}

static int compar(const void* blk1, const void* blk2) {
  return (*(BBLOCK* const*) blk1)->domind > (*(BBLOCK* const*) blk2)->domind;
}

#define X(op) case op:
static void renumber_registers(BBLOCK* blk, IIHASHTABLE* ht) {
  LOOPOPS(
    OPARGCASES(
      if(!(op->addr0_type & (ISLABEL | ISCONST | GARBAGEVAL))) {
        int regnum = iisearch(ht, op->addr0.regnum);
        assert(regnum);
        op->addr0.regnum = regnum;
      }
      ,
      if(!(op->addr1_type & (ISLABEL | ISCONST | GARBAGEVAL))) {
        int regnum = iisearch(ht, op->addr1.regnum);
        assert(regnum);
        op->addr1.regnum = regnum;
      }
      ,
      if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL))) {
        if(iiqueryval(ht, op->dest.regnum)) {
          //it either must have been a deref or have come from a phi!!!
          int regnum = iisearch(ht, op->dest.regnum);
          assert(regnum);
          op->dest.regnum = regnum;
        } else {
          int newreg = ht->keys + 1;
          iiinsert(ht, op->dest.regnum, newreg);
          op->dest.regnum = newreg; //perhaps indicate here whether it's an int or a float? 2 separate hash tables?
        }
      }
      ,
      if(!(phijoinaddr->addr_type & (ISLABEL | ISCONST | GARBAGEVAL))) {
        if(iiqueryval(ht, phijoinaddr->addr.regnum)) {
          int regnum =  iisearch(ht, phijoinaddr->addr.regnum);
          assert(regnum);
          phijoinaddr->addr.regnum = regnum;
        } else {
          iiinsert(ht, phijoinaddr->addr.regnum, ht->keys + 1);
          phijoinaddr->addr.regnum = ht->keys; //perhaps indicate here whether it's an int or a float? 2 separate hash tables?
        }
      }
    )
  )
  if(blk->idominates)
    for(int i = 0; i < blk->idominates->length; i++)
      renumber_registers(daget(blk->idominates, i), ht);
}
#undef X

void renumber(PROGRAM* prog) {
  qsort(prog->allblocks->arr, prog->allblocks->length, sizeof(BBLOCK*), compar);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    blk->domind = i;
  }

  IIHASHTABLE* regtransht = iihtctor();
  renumber_registers(daget(prog->allblocks, 0), regtransht);
  prog->regcnt = regtransht->keys + 1;
  iihtdtor(regtransht);
  //change dynvars and dynchars?
}
