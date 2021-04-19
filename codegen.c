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

static void cgblock(FILE* outputfile, BBLOCK* blk) {
  if(blk->lastop) {
    OPERATION* op = blk->firstop;
    int iparamno = 0;
    int fparamno = 0;
    do {
      switch(op->opcode) {
        case NOP_3:
          break;
        case LBL_3:
          fprintf(outputfile, "%s:\n", op->addr0.labelname);
          break;
        case PARAM_3:
          if(op->addr0_type & ISFLOAT) {
            switch(fparamno++) {
              case 0: case 1: case 2: case 3:
              case 4: case 5: case 6: case 7:
                break;
              default:
                break;
            }
          } else {
            switch(iparamno++) {
              case 0:
                break;
              case 1:
                break;
              case 2:
                break;
              case 3:
                break;
              case 4:
                break;
              case 5:
                break;
              default:
                break;
            }
          }
          break;
        case CALL_3:
          fparamno = 0;
          iparamno = 0;
          break;
        case BNZ_3: case BEZ_3:
          //test
          break;
        case BEQ_U: case BGE_U: case BGE_I:
        case BGT_U: case BGT_I:
          //cmp
          break;
        case BEQ_F: case BGE_F: case BGT_F:
          //ucomisd or ucomiss
          break;
        default:
          assert(0);
      }
      if(op == blk->lastop) break;
    } while((op = op->nextop));
    if(blk->branchblock) {
      if(blk->branchblock->work != blk->work + 1) {
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
      } else {
        switch(op->opcode) {
          case BNZ_3:
            fprintf(outputfile, "jz .L%d\n", blk->nextblock->work);
            break;
          case BEZ_3:
            fprintf(outputfile, "jnz .L%d\n", blk->nextblock->work);
            break;
          case BEQ_U:
            fprintf(outputfile, "jne .L%d\n", blk->nextblock->work);
            break;
          case BEQ_F:
            fprintf(outputfile, "jne .L%d\n", blk->nextblock->work);
            break;
          case BGE_U:
            fprintf(outputfile, "jb .L%d\n", blk->nextblock->work);
            break;
          case BGE_I:
            fprintf(outputfile, "jl .L%d\n", blk->nextblock->work);
            break;
          case BGE_F:
            fprintf(outputfile, "jb .L%d\n", blk->nextblock->work);
            break;
          case BGT_U:
            fprintf(outputfile, "jbe .L%d\n", blk->nextblock->work);
            break;
          case BGT_I:
            fprintf(outputfile, "jle .L%d\n", blk->nextblock->work);
            break;
          case BGT_F:
            fprintf(outputfile, "jbe .L%d\n", blk->nextblock->work);
            break;
          default:
            assert(0);
        }
        return;
      }
    }
  }
  if(blk->nextblock && blk->nextblock->work != blk->work + 1)
    fprintf(outputfile, "jmp .L%d\n", blk->nextblock->work);//changed to index
}

void codegen(FILE* outputfile, PROGRAM* prog) {
  int labelnum = prog->allblocks->length;
  for(int i = 0; i < prog->allblocks->length; i++) {
    fprintf(outputfile, ".L%d\n", i);
    cgblock(outputfile, daget(prog->allblocks, i));
  }
}

static void startgenfile(FILE* outputfile, struct lexctx* ctx) {
  //fprintf(outputfile, ".global %s\n", something);
  //functions that aren't static are globaled
  //fprintf(outputfile, ".extern %s\n", something);???
  //figure this out cleverly?
  fprintf(outputfile, ".data\n");
  for(int i = 0; i < ctx->externglobals->length; i++) {
    //process globals
  }
  for(int i = 0; i < ctx->globals->length; i++) {
    //process globals
  }
  //fprintf(outputfile, ".asciiz \"%s\"\n", something);
  fprintf(outputfile, ".text\n");
}
