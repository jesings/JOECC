#include <stdint.h>
#include "opt.h"
void domark(BBLOCK* blk) {
  blk->unreachable = 1;
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
      if(poss->unreachable) continue;
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
    if(oldb->unreachable) {
      freeblock(oldb);
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

char constfold(PROGRAM* prog) {
  DYNARR* blocks = prog->allblocks;
  //char modified = 0;
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* blk = daget(blocks, i);
    if(blk->lastop) {
      OPERATION* op = blk->firstop;
      while(1){
        switch(op->opcode) {
          default:
            break;
          case ADD_U: case ADD_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 += op->addr1.intconst_64;
            }
            break;
          case ADD_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 += op->addr1.floatconst_64;
            }
            break;
          case SUB_U: case SUB_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 -= op->addr1.intconst_64;
            }
            break;
          case SUB_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 -= op->addr1.floatconst_64;
            }
            break;
          case MULT_U: 
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 *= op->addr1.uintconst_64;
            }
            break;
          case MULT_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 *= op->addr1.intconst_64;
            }
            break;
          case MULT_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 *= op->addr1.floatconst_64;
            }
            break;
          case DIV_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.uintconst_64 == 0) break;
              op->opcode = MOV_3;
              op->addr0.uintconst_64 /= op->addr1.uintconst_64;
            }
            break;
          case DIV_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.intconst_64 == 0) break;
              if(op->addr0.intconst_64 == INT64_MIN && op->addr1.intconst_64 == -1) break;
              op->opcode = MOV_3;
              op->addr0.intconst_64 /= op->addr1.intconst_64;
            }
            break;
          case DIV_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 /= op->addr1.floatconst_64;
            }
            break;
          case MOD_U: 
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.uintconst_64 == 0) break;
              op->opcode = MOV_3;
              op->addr0.uintconst_64 %= op->addr1.uintconst_64;
            }
            break;
          case MOD_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              if(op->addr0.intconst_64 == 0) break;
              if(op->addr0.intconst_64 == INT64_MIN && op->addr1.intconst_64 == -1) break;
              op->opcode = MOV_3;
              op->addr0.intconst_64 %= op->addr1.intconst_64;
            }
            break;
          case AND_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 &= op->addr1.intconst_64;
            }
            break;
          case OR_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 |= op->addr1.intconst_64;
            }
            break;
          case XOR_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 ^= op->addr1.intconst_64;
            }
            break;
          case EQ_U: case EQ_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = op->addr1.intconst_64 == op->addr0.intconst_64;
            }
            break;
          case EQ_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = op->addr1.floatconst_64 == op->addr0.floatconst_64;
            }
            break;
          case NE_U: case NE_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = op->addr1.intconst_64 != op->addr0.intconst_64;
            }
            break;
          case NE_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = op->addr1.floatconst_64 != op->addr0.floatconst_64;
            }
            break;
          case SHL_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 <<= op->addr1.uintconst_64;
            }
            break;
          case SHL_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 <<= op->addr1.intconst_64;
            }
            break;
          case SHR_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 >>= op->addr1.uintconst_64;
            }
            break;
          case SHR_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 >>= op->addr1.intconst_64;
            }
            break;
          case GE_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 >= op->addr1.uintconst_64;
            }
            break;
          case GE_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 >= op->addr1.intconst_64;
            }
            break;
          case GE_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 >= op->addr1.floatconst_64;
            }
            break;
          case LE_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 <= op->addr1.uintconst_64;
            }
            break;
          case LE_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 <= op->addr1.intconst_64;
            }
            break;
          case LE_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 <= op->addr1.floatconst_64;
            }
            break;
          case GT_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 > op->addr1.uintconst_64;
            }
            break;
          case GT_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 > op->addr1.intconst_64;
            }
            break;
          case GT_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 > op->addr1.floatconst_64;
            }
            break;
          case LT_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 < op->addr1.uintconst_64;
            }
            break;
          case LT_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 < op->addr1.intconst_64;
            }
            break;
          case LT_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 < op->addr1.floatconst_64;
            }
            break;
          case NOT_U:
            if(op->addr0_type & ISCONST) {
              op->opcode = MOV_3;
              op->addr0.uintconst_64 = ~op->addr0.uintconst_64;
            }
            break;
          case NEG_I:
            if(op->addr0_type & ISCONST) {
              op->opcode = MOV_3;
              op->addr0.intconst_64 = -op->addr0.intconst_64;
            }
            break;
          case NEG_F:
            if(op->addr0_type & ISCONST) {
              op->opcode = MOV_3;
              op->addr0.floatconst_64 = -op->addr0.floatconst_64;
            }
            break;
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
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.intconst_64 = op->addr1.intconst_64 == op->addr0.intconst_64;
            }
            break;
          case BEQ_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.floatconst_64 = op->addr1.floatconst_64 == op->addr0.floatconst_64;
            }
            break;
          case BNE_U: case BNE_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.intconst_64 = op->addr1.intconst_64 != op->addr0.intconst_64;
            }
            break;
          case BNE_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.floatconst_64 = op->addr1.floatconst_64 != op->addr0.floatconst_64;
            }
            break;
          case BGE_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 >= op->addr1.uintconst_64;
            }
            break;
          case BGE_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 >= op->addr1.intconst_64;
            }
            break;
          case BGE_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 >= op->addr1.floatconst_64;
            }
            break;
          case BLE_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 <= op->addr1.uintconst_64;
            }
            break;
          case BLE_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 <= op->addr1.intconst_64;
            }
            break;
          case BLE_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 <= op->addr1.floatconst_64;
            }
            break;
          case BGT_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 > op->addr1.uintconst_64;
            }
            break;
          case BGT_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 > op->addr1.intconst_64;
            }
            break;
          case BGT_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 > op->addr1.floatconst_64;
            }
            break;
          case BLT_U:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.uintconst_64 = op->addr0.uintconst_64 < op->addr1.uintconst_64;
            }
            break;
          case BLT_I:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.intconst_64 = op->addr0.intconst_64 < op->addr1.intconst_64;
            }
            break;
          case BLT_F:
            if((op->addr0_type & ISCONST) && (op->addr1_type & ISCONST)) {
              op->opcode = BNZ_3;
              op->addr0.floatconst_64 = op->addr0.floatconst_64 < op->addr1.floatconst_64;
            }
            break;
        }
        if(blk->lastop == op) break;
        op = op->nextop;
      }
    }
  }
  return 0;
}
