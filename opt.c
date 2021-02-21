#include <stdint.h>
#include "opt.h"
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

char markunreach(DYNARR* pb) {
  char marked = 0;
  char changed = 0;
  do {
    //ignore first block
    for(int i = 1; i < pb->length; i++) {
      BBLOCK* poss = daget(pb, i);
      if(poss->domind == -1) continue;
      if(poss->inedges->length == 0) {
        changed = 1;
        domark(poss);
      }
    }
  } while(changed-- && (marked = 1));
  return marked;
}
void rmunreach(PROGRAM* prog) {
  DYNARR* oldall = prog->allblocks;
  DYNARR* newall = dactor(oldall->length);
  dapush(newall, daget(oldall, 0));
  for(int i = 1; i < oldall->length; i++) {
    BBLOCK* oldb = daget(oldall, i);
    if(oldb->domind == -1) {
      freeblock(oldb);
      if(oldb == prog->finalblock) prog->finalblock = NULL;
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

OPERATION* exciseop(BBLOCK* blk, OPERATION* prevop, OPERATION* curop) {
  if(prevop == NULL) {
    if(curop == blk->lastop) {
      blk->lastop = NULL;
    } else {
      blk->firstop = curop->nextop;
    }
  } else if(curop == blk->lastop) {
    blk->lastop = prevop;
  } else {
    prevop->nextop = curop->nextop;
  }
  return curop;
}

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

static char feq(OPERATION* op) {
  if(op->addr0_type != op->addr1_type) return 0;
  if(op->addr0_type & ISLABEL) return !strcmp(op->addr0.labelname, op->addr1.labelname);
  if(op->addr0_type & ISVAR) return op->addr0.varnum == op->addr1.varnum;
  return op->addr0.iregnum == op->addr1.iregnum;
}

void prunebranch(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      switch(blk->lastop->opcode) {
        case BNZ_3:
          if(blk->lastop->addr0_type & ISCONST) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.intconst_64 != 0) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          }
          break;
        case BEZ_3:
          if(blk->lastop->addr0_type & ISCONST) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.intconst_64 == 0) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          }
          break;
        case BEQ_U: case BEQ_I:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.intconst_64 == blk->lastop->addr1.intconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->branchblock->inedges, blk);
            blk->branchblock = NULL;
          }
          break;
        case BEQ_F:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.floatconst_64 == blk->lastop->addr1.floatconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->branchblock->inedges, blk);
            blk->branchblock = NULL;
          }
          break;
        case BGT_U:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.uintconst_64 > blk->lastop->addr1.uintconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->nextblock->inedges, blk);
            blk->nextblock = blk->branchblock;
            blk->branchblock = NULL;
          }
          break;
        case BGT_I:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.intconst_64 > blk->lastop->addr1.intconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->nextblock->inedges, blk);
            blk->nextblock = blk->branchblock;
            blk->branchblock = NULL;
          }
          break;
        case BGT_F:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.floatconst_64 > blk->lastop->addr1.floatconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->nextblock->inedges, blk);
            blk->nextblock = blk->branchblock;
            blk->branchblock = NULL;
          }
          break;
        case BGE_U:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.uintconst_64 >= blk->lastop->addr1.uintconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->branchblock->inedges, blk);
            blk->branchblock = NULL;
          }
          break;
        case BGE_I:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.intconst_64 >= blk->lastop->addr1.intconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->branchblock->inedges, blk);
            blk->branchblock = NULL;
          }
          break;
        case BGE_F:
          if(blk->lastop->addr0_type & ISCONST && blk->lastop->addr1_type & ISCONST ) {
            blk->lastop->opcode = NOP_3;
            if(blk->lastop->addr0.floatconst_64 >= blk->lastop->addr1.floatconst_64) {
              dharma(blk->nextblock->inedges, blk);
              blk->nextblock = blk->branchblock;
            } else {
              dharma(blk->branchblock->inedges, blk);
            }
            blk->branchblock = NULL;
          } else if(feq(blk->lastop)) {
            blk->lastop->opcode = NOP_3;
            dharma(blk->branchblock->inedges, blk);
            blk->branchblock = NULL;
          }
          break;
        default:
          break;
      }
    }
  }
}

#define bincf(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = MOV_3; \
              op->addr0.constkind operator op->addr1.constkind; \
            } \
            break;
#define binef(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = MOV_3; \
              op->addr0.constkind = op->addr0.constkind operator op->addr1.constkind; \
            } \
            break;
#define uncf(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = MOV_3; \
              op->addr0.constkind = operator op->addr1.constkind; \
            } \
            break;
#define brcf(constkind, operator) \
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) { \
              op->opcode = BNZ_3; \
              op->addr0.constkind = op->addr0.constkind operator op->addr1.constkind; \
            } \
            break;
#define last1(constkind, floatness, ident) \
            if((op->addr1_type & (ISCONST | (floatness))) && (op->addr1.constkind == ident)) { \
              op->opcode = MOV_3; \
              break; \
            }
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
  DYNARR* blocks = prog->allblocks;
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* blk = daget(blocks, i);
    if(blk->lastop) {
      OPERATION* op = blk->firstop;
      while(1){
        switch(op->opcode) {
          default:
            break;
          case ADD_U: case ADD_I:
            last1(intconst_64, 0, 0);
            first1(intconst_64, 0, 0);
            bincf(intconst_64, +=);
          case ADD_F:
            bincf(floatconst_64, +=);
          case SUB_U: case SUB_I:
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
            bincf(uintconst_64, *=);
          case MULT_I:
            last1(intconst_64, 0, 1);
            first1(intconst_64, 0, 1);
            bincf(intconst_64, *=);
          case MULT_F:
            bincf(floatconst_64, *=);
          case DIV_U:
            last1(uintconst_64, 0, 1);
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.uintconst_64 == 0) break;
              op->opcode = MOV_3;
              op->addr0.uintconst_64 /= op->addr1.uintconst_64;
            }
            break;
          case DIV_I:
            last1(intconst_64, 0, 1);
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.intconst_64 == 0) break;
              if(op->addr0.intconst_64 == INT64_MIN && op->addr1.intconst_64 == -1) break;
              op->opcode = MOV_3;
              op->addr0.intconst_64 /= op->addr1.intconst_64;
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
            }
            break;
          case MOD_I:
            last1(intconst_64, 0, 1);
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.intconst_64 == 0) break;
              if(op->addr0.intconst_64 == INT64_MIN && op->addr1.intconst_64 == -1) break;
              op->opcode = MOV_3;
              op->addr0.intconst_64 %= op->addr1.intconst_64;
            }
            break;
          case AND_U:
            last1(uintconst_64, 0, 0xffffffffffffffffL);
            first1(uintconst_64, 0, 0xffffffffffffffffL);
            bincf(uintconst_64, &=);
          case OR_U:
            last1(uintconst_64, 0, 0);
            first1(uintconst_64, 0, 0);
            bincf(uintconst_64, |=);
          case XOR_U:
            last1(uintconst_64, 0, 0);
            first1(uintconst_64, 0, 0);
            bincf(uintconst_64, ^=);
          case EQ_U: case EQ_I:
            binef(intconst_64, ==);
          case EQ_F:
            binef(floatconst_64, ==);
          case NE_U: case NE_I:
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
              op->addr0.intconst_64 = (long) op->addr0.floatconst_64;
              op->addr0_type = op->dest_type | ISCONST;
            }
            break;
          case I2F:
            if(op->addr0_type & ISCONST) {
              op->opcode = MOV_3;
              if(op->addr0_type & ISSIGNED)
                op->addr0.floatconst_64 = op->addr0.intconst_64;
              else
                op->addr0.floatconst_64 = op->addr0.uintconst_64;
              op->addr0_type = op->dest_type | ISCONST;
            }
            break;
          case BEQ_U: case BEQ_I:
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
        if(blk->lastop == op) break;
        op = op->nextop;
      }
    }
  }
  return 0;
}

int countops(PROGRAM* prog) {
  int opcount = 0;
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      OPERATION* op = blk->firstop;
      while(op != blk->lastop) {
        ++opcount;
        op = op->nextop;
      }
      ++opcount;
    }
  }
  return opcount;
}

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

void tailcall(PROGRAM* prog) {
  //assume prog actually returns a value if this is being called
  for(int i = 0; i < prog->finalblock->inedges->length; i++) {
    BBLOCK* retb = daget(prog->finalblock->inedges, i);
    OPERATION* op = retb->firstop;
    if(op == retb->lastop) continue;
    while(1) {
      if(op->nextop == retb->lastop) break;
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
