#include "joecc_assert.h"
#include "3ac.h"
#include "codegen.h"

#define X(x) case x:

//suffixes: f for 4 byte float, d for eight byte double precision float
//suffixes: b for 1 byte, w for 2 byte, l for four byte, q for eight byte

//format is: opcode, explicit operand number, implicit operands, clobbers, whether input is dest
struct opinfo op2op[] = {
  [NOP_3] = {"nop", 0, 0, 0, 0},
  [LBL_3] = {"", 1, 0, 0, 0}, //not sure?
  [ADD_U] = {"add", 2, 0, 0, 1},
  [ADD_F] = {"vadds", 3, 0, 0, 0},
  [MULT_U] = {"mul", 1, REGBIT(DX), REGBIT(AX), 0}, //multiplies ax by operand, places result in dx:ax ax is low part
  [MULT_I] = {"imul", 2, 0, 0, 1},
  [MULT_F] = {"vmuls", 3, 0, 0, 0},
  [AND_U] = {"and", 2, 0, 0, 1},
  [OR_U] = {"or", 2, 0, 0, 1},
  [XOR_U] = {"xor", 2, 0, 0, 1},
  [EQ_U] = {"cmp", 1, 0, 0, 0}, //then do a setz
  [EQ_F] = {"comis", 1, 0, 0, 0}, //then do a setz
  [NE_U] = {"cmp", 2, 0, 0, 0}, //then do a setnz
  [NE_F] = {"comis", 2, 0, 0, 0}, //then do a setnz
  [SUB_U] = {"sub", 2, 0, 0, 1},
  [SUB_F] = {"vsubs", 3, 0, 0, 0},
  [DIV_U] = {"div", 1, REGBIT(DX), REGBIT(AX), 0}, //divides dx:ax by operand, places result in ax dx is remainder?
  [DIV_I] = {"idiv", 2, 0, 0, 1},
  [DIV_F] = {"vdivs", 3, 0, 0, 0},
  [MOD_U] = {"div", 1, REGBIT(AX), REGBIT(DX), 0}, //divides dx:ax by operand, places result in ax dx is remainder?
  [MOD_I] = {"idiv", 1, REGBIT(AX), REGBIT(DX), 0}, //single operand division, use cqo or cdq or cwd or cbw before, does same as above
  [SHL_U] = {"shl", 2, REGBIT(CX), 0, 1}, //if right hand side is immediate, CX need not be clobbered
  [SHL_I] = {"sal", 2, REGBIT(CX), 0, 1},
  [SHR_U] = {"shr", 2, REGBIT(CX), 0, 1},
  [SHR_I] = {"sar", 2, REGBIT(CX), 0, 1},
  [GE_U] = {"cmp", 2, 0, 0, 0}, //then do a setae
  [GE_I] = {"cmp", 2, 0, 0, 0}, //then do a setge
  [GE_F] = {"comis", 2, 0, 0, 0}, //then do a setae
  [LE_U] = {"cmp", 2, 0, 0, 0}, //then do a setbe
  [LE_I] = {"cmp", 2, 0, 0, 0}, //then do a setle
  [LE_F] = {"comis", 2, 0, 0, 0}, //then do a setbe
  [GT_U] = {"cmp", 2, 0, 0, 0}, //then do a seta
  [GT_I] = {"cmp", 2, 0, 0, 0}, //then do a setg
  [GT_F] = {"comis", 2, 0, 0, 0}, //then do a seta
  [LT_U] = {"cmp", 2, 0, 0, 0}, //then do a setb
  [LT_I] = {"cmp", 2, 0, 0, 0}, //then do a setl
  [LT_F] = {"comis", 2, 0, 0, 0}, //then do a setb
  [COPY_3] = {"repnz movs", 0, REGBIT(SI) | REGBIT(DI) | REGBIT(CX), 0, 0}, //clobbers si, di, move count into cx
  [ARRMOV] = {"mov", 2, 0, 0, 0}, //not sure, either way args are the same
  [MTP_OFF] = {"mov", 2, 0, 0, 0}, //not sure, either way args are the same
  [NOT_U] = {"not", 2, 0, 0, 0},
  [NEG_I] = {"neg", 2, 0, 0, 0},
  [NEG_F] = {"xorps", 2, 0, 0, 0}, //cmpeqd reg (packed op), reg/pslld $31 (packed op), reg/xorps reg, dest
  [F2I] = {"cvtsd2si", 2, 0, 0, 0}, //many choices from cvtss2sd, etc, either way args are the same
  [I2F] = {"cvtsi2sd", 2, 0, 0, 0}, //many choices from cvtsd2ss, etc, either way args are the same
  [F2F] = {"cvtss2sd", 2, 0, 0, 0}, //many choices from cvtsd2ss, etc, either way args are the same
  [ALOC_3] = {"sub", 2, 0, 0, 0}, //subtract from rsp
  [MOV_3] = {"mov", 2, 0, 0, 0}, //vmovsd movzx movsx etc, either way args are the same
  [BEQ_U] = {"cmp", 2, 0, 0, 0}, //cmp
  [BEQ_F] = {"comis", 2, 0, 0, 0}, //cmp
  [BGE_U] = {"cmp", 2, 0, 0, 0}, //cmp
  [BGE_I] = {"cmp", 2, 0, 0, 0}, //cmp
  [BGE_F] = {"comis", 2, 0, 0, 0}, //cmp
  [BGT_U] = {"cmp", 2, 0, 0, 0}, //cmp
  [BGT_I] = {"cmp", 2, 0, 0, 0}, //cmp
  [BGT_F] = {"comis", 2, 0, 0, 0}, //cmp
  [JEQ_I] = {"cmp", 2, 0, 0, 0}, //cmp
  [BNZ_3] = {"test", 2, 0, 0, 0}, //test
  [BEZ_3] = {"test", 2, 0, 0, 0}, //test
  [ARG_3] = {"mov", 2, 0, 0, 0}, //decide where/when to put, push too
  [RET_3] = {"ret", 0, 0, 0, 0}, //put it in rax
  [INIT_3] = {"", 0, 0, 0, 0}, //not sure
  [PARAM_3] = {"mov", 2, 0, 0, 0}, //not sure
  [CALL_3] = {"call", 1, REGBIT(DI) | REGBIT(SI) | REGBIT(DX) | REGBIT(CX)| REGBIT(R8)| REGBIT(R9) | REGBIT(R10), REGBIT(AX), 0}, //call, may clobber any/all of these
  [PHI] = {"", 0, 0, 0, 0}, //no phi nodes by this point
  [DEALOC] = {"sub", 2, 0, 0, 0},
  [ADDR_3] = {"lea", 2, 0, 0, 0}, //most likely nop?
  [ASM] = {"", 0, 0, 0, 0}, //figure it out
};

//assumptions we make before reg allocation and later codegen: only 1 deref'ed 
//arg in the expression string constants are factored out to further globals, as
//are float consts loads and stores are inserted before and after so that these 
//assumptions hold
void ldstrsep(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(!blk->operations || !blk->operations->length) continue;
    DYNARR* newops = dactor(blk->operations->length + 8);
    for(int opind = 0; opind < blk->operations->length; opind++) {
      OPERATION* op = blk->operations->arr[opind];
      char inplace;
      switch(op->opcode) {
        OPS_NOVAR_3ac OPS_1_3ac OPS_1_ASSIGN_3ac case COPY_3: case CALL_3: case DEALOC: case ASM:
          dapush(newops, op);
          break;
        case PHI: //all phis should be gone by this point
          dapush(newops, op);
          break;
        case ADDR_3:
          if(op->addr0_type & ISDEREF && op->dest_type & ISDEREF) {
            ADDRTYPE adrt = op->dest_type & GENREGMASK;
            ADDRESS adr;
            adr.regnum = prog->regcnt++;
            op->dest_type = adrt;
            op->dest = adr;
            dapush(newops, op);
            dapush(newops, ct_3ac_op2(MOV_3, adrt, adr, op->dest_type, op->dest));
          } else {
            dapush(newops, op);
          }
          break;
        OPS_3_3ac
        //shouldn't really work for float operations!
          inplace = 0;
          char addr0badness = (op->addr0_type & ISDEREF) ? 1 : (((op->addr0_type & (ISFLOAT | ISCONST)) == (ISFLOAT | ISCONST)) ? 2 : (op->addr0_type & ISSTRCONST ? 3 : 0));
          char addr1badness = (op->addr1_type & ISDEREF) ? 1 : (((op->addr1_type & (ISFLOAT | ISCONST)) == (ISFLOAT | ISCONST)) ? 2 : (op->addr1_type & ISSTRCONST ? 3 : 0));
          if(op->dest_type & ISDEREF) {
            if(addr0badness) {
              if((op->addr0_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
                 (op->addr0_type & ISLABEL ? strcmp(op->addr0.labelname, op->dest.labelname): op->addr0.regnum == op->dest.regnum)) {
                inplace = 1;
              } else {
                ADDRTYPE adrt = op->addr0_type & GENREGMASK & ~(ISCONST | ISSTRCONST);
                ADDRESS adr;
                adr.regnum = prog->regcnt++;
                dapush(newops, ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr));
                op->addr0_type = adrt;
                op->addr0 = adr;
              }
            }
            if(addr1badness) {
              if(!inplace && (op->addr1_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
                 (op->addr1_type & ISLABEL ? strcmp(op->addr1.labelname, op->dest.labelname): op->addr1.regnum == op->dest.regnum)) {
                inplace = 1;
              } else {
                ADDRTYPE adrt = op->addr1_type & GENREGMASK & ~(ISCONST | ISSTRCONST);
                ADDRESS adr;
                adr.regnum = prog->regcnt++;
                dapush(newops, ct_3ac_op2(MOV_3, op->addr1_type, op->addr1, adrt, adr));
                op->addr1_type = adrt;
                op->addr1 = adr;
              }
            }
          } else if(addr0badness && addr1badness) {
            //if deref then both badnesses must have been rectified
            //instead, for this case we arbitrarily choose addr 0
            ADDRTYPE adrt = op->addr0_type & GENREGMASK & ~(ISCONST | ISSTRCONST);
            ADDRESS adr;
            adr.regnum = prog->regcnt++;
            dapush(newops, ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr));
            op->addr0_type = adrt;
            op->addr0 = adr;
          }
          dapush(newops, op);
          break;
        case MTP_OFF: case ARRMOV:
          if(op->addr0_type & (ISDEREF | ISSTRCONST) || (op->addr0_type & (ISFLOAT | ISCONST)) == (ISFLOAT | ISCONST)) {
            ADDRTYPE adrt = op->addr0_type & GENREGMASK & ~(ISCONST | ISSTRCONST);
            ADDRESS adr;
            adr.regnum = prog->regcnt++;
            dapush(newops, ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr));
            op->addr0_type = adrt;
            op->addr0 = adr;
          }
          dapush(newops, op);
          break;
        OPS_2_3ac
        //shouldn't really work for F2F, NEG_F
          if(op->addr0_type & (ISDEREF | ISSTRCONST) || (op->addr0_type & (ISFLOAT | ISCONST)) == (ISFLOAT | ISCONST)) {
            if((op->addr0_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
               (op->addr0_type & ISLABEL ? strcmp(op->addr0.labelname, op->dest.labelname): op->addr0.regnum == op->dest.regnum)) {
              //do nothing?
            } else {
              ADDRTYPE adrt = op->addr0_type & GENREGMASK & ~(ISCONST | ISSTRCONST);
              ADDRESS adr;
              adr.regnum = prog->regcnt++;
              dapush(newops, ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr));
              op->addr0_type = adrt;
              op->addr0 = adr;
            }
          }
          dapush(newops, op);
          break;
        OPS_NODEST_3ac
          if(op->addr0_type & ISDEREF && op->addr1_type & ISDEREF) {
            ADDRTYPE adrt = op->addr0_type & GENREGMASK;
            ADDRESS adr;
            adr.regnum = prog->regcnt++;
            dapush(newops, ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr));
            op->addr0_type = adrt;
            op->addr0 = adr;
          }
          dapush(newops, op);
          break;
      }
    }
    dadtor(blk->operations);
    blk->operations = newops;
  }
}
#undef X


static void addrgen(FILE* of, QHASHTABLE* strconstht, ADDRTYPE adt, ADDRESS addr) {
  if(adt & ISCONST) {
    if(adt & ISSTRCONST) {
      char* labelstr = qsearch(strconstht, addr.strconst);
      if(!labelstr) {
          labelstr = malloc(16);
          snprintf(labelstr, 16, ".strlabel%d", strconstht->keys);
          qinsert(strconstht, addr.strconst, labelstr);
          //fprintf(of, "%s:\n.asciiz %s\n", labelstr, addr.strconst); do this at end with key/value pairs of strconstht
      }
      fprintf(of, "%s(%%rip)\n", labelstr);
      //stringconsty, but to some buffer, retrieve label as well?
    } else if(adt & ISFLOAT) {
      if((adt & 0xf) == 4) {
        //fprintf(outputfile, ".single %f\n", (float) addr.floatconst_64); but to some buffer, retrieve label as well?
      } else {
        //fprintf(outputfile, ".double %lf\n", addr.floatconst_64); but to some buffer, retrieve label as well?
      }
    } else {
      fprintf(of, "$%ld", addr.uintconst_64);
    }
  } else if(adt & ISDEREF) {
    if(adt & ISLABEL) {
      fprintf(of, "%s(%%rip)", addr.labelname);
    } else {
      //signal base pointer offset somehow?
      fprintf(of, "(%%%s)", ireg64[addr.regnum]);
    }
  } else {
    if(adt & ISFLOAT) {
      fprintf(of, "%%%s", freg128[addr.regnum]); //xmm register
    } else {
      const char* const* reginald;
      switch(adt & 0xf) {
        case 1:
          reginald = ireg8;
          break;
        case 2:
          reginald = ireg16;
          break;
        case 4:
          reginald = ireg32;
          break;
        case 8:
          reginald = ireg64;
          break;
        default:
          assert(0);
      }
      fprintf(of, "%%%s", reginald[addr.regnum]); //xmm register
    }
  }
}

static void cgblock(FILE* outputfile, char* fname, BBLOCK* blk) {
  if(blk->operations && blk->operations->length) {
    int iparamno = 0;
    int fparamno = 0;
    OPERATION* op;
    for(int opind = 0; opind < blk->operations->length; opind++) {
      op = blk->operations->arr[opind];
      switch(op->opcode) {
        case NOP_3:
          break;
        case LBL_3:
          fprintf(outputfile, "%s:\n", op->addr0.labelname);
          break;
        case PARAM_3:
          if(op->dest_type & ISFLOAT) {
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
          //assert(0);
          break;
      }
    };
    if(blk->branchblock) {
      if(blk->branchblock->work != blk->work + 1) {
        switch(op->opcode) {
          case BNZ_3:
            fprintf(outputfile, "jnz .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case BEZ_3:
            fprintf(outputfile, "jz .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case BEQ_U: case BEQ_F:
            fprintf(outputfile, "je .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case BGE_U: case BGE_F:
            fprintf(outputfile, "jae .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case BGE_I:
            fprintf(outputfile, "jge .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case BGT_U: case BGT_F:
            fprintf(outputfile, "ja .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case BGT_I:
            fprintf(outputfile, "jg .L%s.%d\n", fname, blk->branchblock->domind);
            break;
          case JEQ_I: break;
          default:
            assert(0);
        }
      } else {
        switch(op->opcode) {
          case BNZ_3:
            fprintf(outputfile, "jz .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          case BEZ_3:
            fprintf(outputfile, "jnz .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          case BEQ_U: case BEQ_F:
            fprintf(outputfile, "jne .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          case BGE_U: case BGE_F:
            fprintf(outputfile, "jb .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          case BGE_I:
            fprintf(outputfile, "jl .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          case BGT_U: case BGT_F:
            fprintf(outputfile, "jbe .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          case BGT_I:
            fprintf(outputfile, "jle .L%s.%d\n", fname, blk->nextblock->domind);
            break;
          default:
            assert(0);
        }
        return;
      }
    }
  }
  if(blk->nextblock && blk->nextblock->work != blk->work + 1)
    fprintf(outputfile, "jmp .L%s.%d\n", fname, blk->nextblock->domind);//changed to index
}

void genprogfile(FILE* outputfile, IDTYPE* rettype, char* funcname, PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    blk->visited = i;
  }
  //not if static or inline, do this
  if(!(rettype->tb & (STATICNUM | INLINED)))
    fprintf(outputfile, ".global %s\n", funcname);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    fprintf(outputfile, ".L%s.%d:\n", funcname, blk->domind);
    cgblock(outputfile, funcname, daget(prog->allblocks, i));
  }
}
static void stringconsty(FILE* outputfile, char* string) {
  fprintf(outputfile, ".asciz \"");
  for(int i = 0; string[i]; i++) {
    switch(string[i]) {
      case 0 ... 8: case 11: case 12: case 14 ... 31: case 127:
        fputs("\\%hho", outputfile);
        break;
      case '\n':
        fputs("\\n", outputfile);
        break;
      case '\r':
        fputs("\\r", outputfile);
        break;
      case '\t':
        fputs("\\t", outputfile);
        break;
      case '"':
        fputs("\\\"", outputfile);
        break;
      default:
        fputc(string[i], outputfile);
        break;
    }
  }
  fprintf(outputfile, "\"\n");
}
static void procinlit(FILE* outputfile, IDTYPE* ty, EXPRESSION* ex) {
  if(ispointer(ty)) {
    if(ex && ex->type == STRING) {
      stringconsty(outputfile, ex->strconst);
    } else if(((struct declarator_part*) dapeek(ty->pointerstack))->type == ARRAYSPEC) {
      if(ex) {
        assert(ex->type == ARRAY_LIT);
        //do elements one by one, we will put them all on different lines (except arrays of integers) for simplicity for now
        int maxi = ((struct declarator_part*) dapeek(ty->pointerstack))->arrmaxind;
        ty->pointerstack->length--;
        if(ty->pointerstack->length || ty->tb & (FLOATNUM | STRUCTVAL | UNIONVAL)) {
          for(int i = 0; i < maxi; i++){
            EXPRESSION* subex = i < ex->params->length ? daget(ex->params, i) : NULL;
            procinlit(outputfile, ty, subex);
          }
        } else {
          if(ty->tb & 8) {
            fprintf(outputfile, ".8byte ");
          } else if(ty->tb & 4) {
            fprintf(outputfile, ".4byte ");
          } else if(ty->tb & 2) {
            fprintf(outputfile, ".2byte ");
          } else if(ty->tb & 1) {
            fprintf(outputfile, ".byte ");
          }
          for(int i = 0; i < maxi; i++){
            EXPRESSION* subex = i < ex->params->length ? daget(ex->params, i) : NULL;
            if(ty->tb & 8) {
              fprintf(outputfile, "%ld", subex ? subex->intconst : 0);
            } else if(ty->tb & 4) {
              fprintf(outputfile, "%d", subex ? (int) subex->intconst : 0);
            } else if(ty->tb & 2) {
              fprintf(outputfile, "%hd", (short) (subex ? subex->intconst : 0));
            } else if(ty->tb & 1) {
              fprintf(outputfile, "%hhd", (char) (subex ? subex->intconst : 0));
            } else {
              assert(0);
            }
            if(i != maxi - 1) fprintf(outputfile, ",");
          }
          fprintf(outputfile, "\n");
        }
        ty->pointerstack->length++;
      } else {
        fprintf(outputfile, ".align 8\n");//overkill alignment
        fprintf(outputfile, ".zero %d\n", lentype(ty));//overkill alignment
      }
    } else {
      if(ex) {
        if(!(ex->type == INT || ex->type == UINT)) foldconst(ex);
        assert(ex->type == INT || ex->type == UINT);
        fprintf(outputfile, ".align 8\n");
        fprintf(outputfile, ".8byte %ld\n", ex->intconst);
      } else {
        fprintf(outputfile, ".align 8\n");
        fprintf(outputfile, ".8byte 0\n");
      }
    }
  } else if(ty->tb & (STRUCTVAL | UNIONVAL)) {
    if(ex) {
      for(int i = 0; i < ty->structtype->fields->length; i++) {
        DECLARATION* subdecl = daget(ty->structtype->fields, i);
        EXPRESSION* subex = i < ex->params->length ? daget(ex->params, i) : NULL;
        procinlit(outputfile, subdecl->type, subex);
      }
    } else {
      fprintf(outputfile, ".align 8\n");//overkill alignment
      if(lentype(ty))
        fprintf(outputfile, ".zero %d\n", lentype(ty));//overkill alignment
    }
  } else {
    if((ty->tb & 0xf) != 1)
      fprintf(outputfile, ".align %d\n", ty->tb & 0xf);
    if(ty->tb & FLOATNUM) {
      if(ex) {
        if(ex->type != FLOAT) foldconst(ex);
        assert(ex->type == FLOAT);
        if(ty->tb & 8) {
          fprintf(outputfile, ".double %lf\n", ex->floatconst);
        } else if(ty->tb & 4) {
          fprintf(outputfile, ".single %f\n", (float) ex->floatconst);
        } else {
          assert(0);
        }
      } else {
        fprintf(outputfile, ".zero %d\n", ty->tb & 0xf);
      }
    } else {
      if(ex) {
        if(!(ex->type == INT || ex->type == UINT)) foldconst(ex);
        assert(ex->type == INT || ex->type == UINT);
        if(ty->tb & 8) {
          fprintf(outputfile, ".8byte %ld\n", ex->intconst);
        } else if(ty->tb & 4) {
          fprintf(outputfile, ".4byte %d\n", (int) ex->intconst);
        } else if(ty->tb & 2) {
          fprintf(outputfile, ".2byte %hd\n", (short) ex->intconst);
        } else if(ty->tb & 1) {
          fprintf(outputfile, ".byte %hhd\n", (char) ex->intconst);
        }
      } else {
        fprintf(outputfile, ".zero %d\n", ty->tb & 0xf);
      }
    }
  }
}

//Start the generation of the assembly file, this outputs the extern functions and the global variables
//Eventually this will also output the string and float literals to be loaded
void startgenfile(FILE* outputfile, struct lexctx* lctx) {
  for(int i = 0; i < lctx->externglobals->length; i++) {
    INITIALIZER* in = daget(lctx->externglobals, i);
    if(!(in->decl->type->tb & STATICNUM)) fprintf(outputfile, ".extern %s\n", in->decl->varname);
    //not all of these are necessary
  }
  fprintf(outputfile, ".data\n");
  for(int i = 0; i < lctx->globals->length; i++) {
    INITIALIZER* in = daget(lctx->globals, i);
    if(ispointer(in->decl->type) && (((struct declarator_part*) dapeek(in->decl->type->pointerstack))->type == PARAMSSPEC || ((struct declarator_part*) dapeek(in->decl->type->pointerstack))->type == NAMELESS_PARAMSSPEC)) {
      if(!(in->decl->type->tb & STATICNUM)) {
        if(qqueryval(lctx->funcs, in->decl->varname)) {
          fprintf(outputfile, ".global %s\n", in->decl->varname);
        } else {
          fprintf(outputfile, ".extern %s\n", in->decl->varname);
        }
      }
    } else if(in->decl->type->tb & EXTERNNUM) {
      fprintf(outputfile, ".extern %s\n", in->decl->varname);
    } else {
      if(!(in->decl->type->tb & STATICNUM)) {
        fprintf(outputfile, ".global %s\n", in->decl->varname);
      }
      fprintf(outputfile, "%s:\n", in->decl->varname);
      procinlit(outputfile, in->decl->type, in->expr);
    }
  }
  //string literals
  //float literals
  fprintf(outputfile, ".text\n");
}
