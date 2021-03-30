#include <assert.h>
#include "3ac.h"

static void addrgen(FILE* of, ADDRTYPE adt, ADDRESS addr) {
  if(adt & ISCONST) {
    assert(!(adt & ISSTRCONST)); //handle strconsts beforehand (address)
    assert(!(adt & ISFLOAT)); //handle floatconsts beforehand (address)
  } else if(adt & ISDEREF) {
    if(adt & ISLABEL) {
      fprintf(of, "%s(%%rip)", addr.labelname);
    } else {
    }
  } else {
  }
}

static void opgen(FILE* outputfile, OPERATION* op) {
  switch(op->opcode) {
    default:
      assert(0);
  }
}

static void cgblock(FILE* outputfile, BBLOCK* blk) {
  if(blk->lastop) {
    OPERATION* op = blk->firstop;
    for(;op != blk->lastop; op = op->nextop) {
      opgen(outputfile, op);
    }
    opgen(outputfile, op);//last op
    if(blk->branchblock) {
      switch(op->opcode) {
        case BNZ_3:
          fprintf(outputfile, "jnz .L%d\n", blk->branchblock->work);
          break;
        case BEZ_3:
          fprintf(outputfile, "jz .L%d\n", blk->branchblock->work);
          break;
        case BEQ_U:
          fprintf(outputfile, "je .L%d\n", blk->branchblock->work);
          break;
        case BEQ_F:
          fprintf(outputfile, "je .L%d\n", blk->branchblock->work);
          break;
        case BGE_U:
          fprintf(outputfile, "jae .L%d\n", blk->branchblock->work);
          break;
        case BGE_I:
          fprintf(outputfile, "jge .L%d\n", blk->branchblock->work);
          break;
        case BGE_F:
          fprintf(outputfile, "jae .L%d\n", blk->branchblock->work);
          break;
        case BGT_U:
          fprintf(outputfile, "ja .L%d\n", blk->branchblock->work);
          break;
        case BGT_I:
          fprintf(outputfile, "jg .L%d\n", blk->branchblock->work);
          break;
        case BGT_F:
          fprintf(outputfile, "ja .L%d\n", blk->branchblock->work);
          break;
        default:
          assert(0);
      }
    }
  }
  if(blk->nextblock)
    fprintf(outputfile, "jmp .L%d\n", blk->nextblock->work);//changed to index
}

void codegen(FILE* outputfile, PROGRAM* prog) {
  int labelnum = prog->allblocks->length;
  //fprintf(outputfile, ".global %s\n", something);
  //fprintf(outputfile, ".extern %s\n", something);???
  fprintf(outputfile, ".data\n");
  //fprintf(outputfile, ".asciiz \"%s\"\n", something);
  fprintf(outputfile, ".text\n");
  for(int i = 0; i < prog->allblocks->length; i++) {
    fprintf(outputfile, ".L%d\n", i);
    cgblock(outputfile, daget(prog->allblocks, i));
  }
}
