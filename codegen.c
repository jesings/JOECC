#include <assert.h>
#include "3ac.h"
#include "codegen.h"

#define X(x) case x:
void ldstrsep(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(!blk->lastop) continue;
    OPERATION* op = blk->firstop;
    OPERATION** prevptr = &blk->firstop;
    while(1) {
      char inplace;
      switch(op->opcode) {
        OPS_NOVAR_3ac OPS_1_3ac OPS_1_ASSIGN_3ac case COPY_3: case ARROFF: case CALL_3: case DEALOC: case ASM:
          break;
        case PHI:
          if(op->dest_type & ISDEREF) {
            for(int i = 0; i < blk->inedges->length; i++) {
              if((op->addr0.joins[i].addr_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
                 (op->addr0.joins[i].addr_type & ISLABEL ? strcmp(op->addr0.joins[i].addr.labelname, op->dest.labelname): op->addr0.joins[i].addr.iregnum == op->dest.iregnum)) {
                //do nothing
              } else {
                BBLOCK* inblk = daget(blk->inedges, i);
                ADDRESS adr;
                adr.iregnum = prog->regcnt++;
                ADDRTYPE adrt = op->addr0_type & GENREGMASK;
                if(inblk->lastop) {
                  inblk->lastop = inblk->lastop->nextop = ct_3ac_op2(MOV_3, op->addr0.joins[i].addr_type, op->addr0.joins[i].addr, adrt, adr);
                } else {
                  inblk->firstop = inblk->lastop = ct_3ac_op2(MOV_3, op->addr0.joins[i].addr_type, op->addr0.joins[i].addr, adrt, adr);
                }
                op->addr0.joins[i].addr_type = adrt;
                op->addr0.joins[i].addr = adr;
              }
            }
          }
          break; //maybe handle phi
        case ADDR_3:
          if(op->addr0_type & ISDEREF && op->dest_type & ISDEREF) {
            ADDRESS adr;
            adr.iregnum = prog->regcnt++;
            ADDRTYPE adrt = op->dest_type & GENREGMASK;
            OPERATION* newop= ct_3ac_op2(MOV_3, adrt, adr, op->dest_type, op->dest);
            newop->nextop = op->nextop;
            op->nextop = newop;
            op->dest_type = adrt;
            op->dest = adr;
            if(blk->lastop == op) blk->lastop = op;
            op = newop; //prevent useless iteration
          }
          break;
        OPS_3_3ac
        //shouldn't really work for float operations!
          inplace = 0;
          if(op->addr0_type & ISDEREF && op->dest_type & ISDEREF) {
            if((op->addr0_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
               (op->addr0_type & ISLABEL ? strcmp(op->addr0.labelname, op->dest.labelname): op->addr0.iregnum == op->dest.iregnum)) {
              inplace = 1;
            } else {
              ADDRESS adr;
              adr.iregnum = prog->regcnt++;
              ADDRTYPE adrt = op->addr0_type & GENREGMASK;
              *prevptr = ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr);
              (*prevptr)->nextop = op;
              op->addr0_type = adrt;
              op->addr0 = adr;
            }
          }
          if(op->addr1_type & ISDEREF && op->dest_type & ISDEREF) {
            if(!inplace && (op->addr1_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
               (op->addr1_type & ISLABEL ? strcmp(op->addr1.labelname, op->dest.labelname): op->addr1.iregnum == op->dest.iregnum)) {
               //do nothing?
            } else {
              ADDRESS adr;
              adr.iregnum = prog->regcnt++;
              ADDRTYPE adrt = op->addr1_type & GENREGMASK;
              *prevptr = ct_3ac_op2(MOV_3, op->addr1_type, op->addr1, adrt, adr);
              (*prevptr)->nextop = op;
              op->addr1_type = adrt;
              op->addr1 = adr;
            }
          }
          break;
        case MTP_OFF: case ARRMOV:
          if(op->addr0_type & ISDEREF) {
            ADDRESS adr;
            adr.iregnum = prog->regcnt++;
            ADDRTYPE adrt = op->addr0_type & GENREGMASK;
            *prevptr = ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr);
            (*prevptr)->nextop = op;
            op->addr0_type = adrt;
            op->addr0 = adr;
          }
          break;
        OPS_2_3ac
        //shouldn't really work for F2F, NEG_F
          if(op->addr0_type & ISDEREF && op->dest_type & ISDEREF) {
            if((op->addr0_type & (ISLABEL | ISFLOAT)) == (op->dest_type & (ISLABEL | ISFLOAT)) &&
               (op->addr0_type & ISLABEL ? strcmp(op->addr0.labelname, op->dest.labelname): op->addr0.iregnum == op->dest.iregnum)) {
              //do nothing?
            } else {
              ADDRESS adr;
              adr.iregnum = prog->regcnt++;
              ADDRTYPE adrt = op->addr0_type & GENREGMASK;
              *prevptr = ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr);
              (*prevptr)->nextop = op;
              op->addr0_type = adrt;
              op->addr0 = adr;
            }
          }
          break;
        OPS_NODEST_3ac
          if(op->addr0_type & ISDEREF && op->addr1_type & ISDEREF) {
            ADDRESS adr;
            adr.iregnum = prog->regcnt++;
            ADDRTYPE adrt = op->addr0_type & GENREGMASK;
            *prevptr = ct_3ac_op2(MOV_3, op->addr0_type, op->addr0, adrt, adr);
            (*prevptr)->nextop = op;
            op->addr0_type = adrt;
            op->addr0 = adr;
          }
          break;
      }
      if(op == blk->lastop) break;
      prevptr = &op->nextop;
      op = op->nextop;
    }
  }
}
#undef X


//assumptions we make before codegen--aside from copy(?) expressions, addr,
//alloc (anything else?) , only 1 deref'ed arg in the expression
//string constants are factored out to further globals, as are float consts(maybe just for x86_64?)
//loads and stores are inserted before and after so that these assumptions hold
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
          case BEQ_U: case BEQ_F:
            fprintf(outputfile, "je .L%d\n", blk->branchblock->work);
            break;
          case BGE_U: case BGE_F:
            fprintf(outputfile, "jae .L%d\n", blk->branchblock->work);
            break;
          case BGE_I:
            fprintf(outputfile, "jge .L%d\n", blk->branchblock->work);
            break;
          case BGT_U: case BGT_F:
            fprintf(outputfile, "ja .L%d\n", blk->branchblock->work);
            break;
          case BGT_I:
            fprintf(outputfile, "jg .L%d\n", blk->branchblock->work);
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
          case BEQ_U: case BEQ_F:
            fprintf(outputfile, "jne .L%d\n", blk->nextblock->work);
            break;
          case BGE_U: case BGE_F:
            fprintf(outputfile, "jb .L%d\n", blk->nextblock->work);
            break;
          case BGE_I:
            fprintf(outputfile, "jl .L%d\n", blk->nextblock->work);
            break;
          case BGT_U: case BGT_F:
            fprintf(outputfile, "jbe .L%d\n", blk->nextblock->work);
            break;
          case BGT_I:
            fprintf(outputfile, "jle .L%d\n", blk->nextblock->work);
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

static void codegen(FILE* outputfile, PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    fprintf(outputfile, ".L%d\n", i);
    cgblock(outputfile, daget(prog->allblocks, i));
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
        if(ty->pointerstack->length || ty->tb == (FLOATNUM | STRUCTVAL | UNIONVAL)) {
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
              fprintf(outputfile, "%ld,", subex ? subex->intconst : 0);
            } else if(ty->tb & 4) {
              fprintf(outputfile, "%d,", subex ? (int) subex->intconst : 0);
            } else if(ty->tb & 2) {
              fprintf(outputfile, "%hd,", subex ? (short) subex->intconst : 0);
            } else if(ty->tb & 1) {
              fprintf(outputfile, "%hhd,", subex ? (char) subex->intconst : 0);
            }
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
        if(!(ex->type == INT || ex->type == UINT)) foldconst(&ex);
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
      fprintf(outputfile, ".zero %d\n", lentype(ty));//overkill alignment
    }
  } else {
    if((ty->tb & 0xf) != 1)
      fprintf(outputfile, ".align %d\n", ty->tb & 0xf);
    if(ty->tb & FLOATNUM) {
      if(ex) {
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
        if(search(lctx->funcs, in->decl->varname)) {
          fprintf(outputfile, ".global %s\n", in->decl->varname);
        } else {
          fprintf(outputfile, ".extern %s\n", in->decl->varname);
        }
      }
    } else if(in->decl->type->tb & EXTERNNUM) {
      fprintf(outputfile, ".extern %s\n", in->decl->varname);
    } else {
      fprintf(outputfile, ".global %s\n", in->decl->varname);
      fprintf(outputfile, "%s:\n", in->decl->varname);
      procinlit(outputfile, in->decl->type, in->expr);
    }
  }
  //string literals
  //float literals
  fprintf(outputfile, ".text\n");
}
