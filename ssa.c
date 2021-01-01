#include <assert.h>
#include "ssa.h"

static BBLOCK* intersect(BBLOCK* n1, BBLOCK* n2) {
  while(n1 != n2) {
    while(n1->domind < n2->domind) n1 = n1->dom;
    while(n2->domind < n1->domind) n2 = n2->dom;
  }
  return n1;
}

static void rpdt(BBLOCK* root, DYNARR* stack) {
  if(!root) return;
  if(root->visited) return;
  root->visited = 1;
  rpdt(root->nextblock, stack);
  rpdt(root->branchblock, stack);
  root->domind = stack->length;
  dapush(stack, root);
}

static void dfpdt(BBLOCK* root) {
  if(!root) return;
  if(root->visited) return;
  if(!(root->inedges && root->inedges->length)) return; //don't look at unreachable blocks
  root->visited = 1;
  if(root->idominates)
    for(int i = 0; i < root->idominates->length; i++)
      dfpdt(daget(root->idominates, i));
  root->df = dactor(8);
  if(root->nextblock && root->nextblock->dom != root) { //only excludes last node
    dapush(root->df, root->nextblock);
  }
  if(root->branchblock && root->branchblock->dom != root) {
    dapush(root->df, root->branchblock);
  }
  if(root->idominates) {
    for(int j = 0; j < root->idominates->length; j++) {
      BBLOCK* ib = daget(root->idominates, j);
      for(int k = 0; k < ib->df->length; k++) {
        BBLOCK* kb = daget(ib->df, k);
        if(kb->dom != root)
          dapush(root->df, kb);
      }
    }
  }
}

//TODO: implement lengauer tarjan: https://www.cl.cam.ac.uk/~mr10/lengtarj.pdf
void ctdtree(PROGRAM* prog) {
  DYNARR* blocks = prog->allblocks;
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->dom = NULL;
    dtn->visited = 0;
  }
  DYNARR* blockstack = dactor(blocks->length - 1);
  BBLOCK* first = daget(blocks, 0);
  first->dom = first;
  first->domind = blocks->length; //2 greater but we don't need a decrement
  rpdt(first->nextblock, blockstack);
  rpdt(first->branchblock, blockstack);
  //maybe reverse order now, rather than iterate backwards
  char changed = 1;
  while(changed) {
    changed = 0;
    //https://www.cs.rice.edu/~keith/EMBED/dom.pdf
    for(int i = blockstack->length - 1; i >= 0; i--) {
      BBLOCK* cb = daget(blockstack, i);
      BBLOCK* new_idom = NULL;
      for(int i = 0; i < cb->inedges->length; i++) {
        BBLOCK* pred = daget(cb->inedges, i);
        if(pred->dom) {
          if(new_idom) {
            new_idom = intersect(pred, new_idom);
          } else {
            new_idom = pred;
          }
        }
      }
      if(cb->dom != new_idom) 
        cb->dom = new_idom, changed = 1;
    }
  }
  //populate in parents
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    if(cb->dom) {
      if(!cb->dom->idominates)
        cb->dom->idominates = dactor(8);
      dapush(cb->dom->idominates, cb);
    }
    cb->visited = 0;
  }
  //dominator tree (immediate dominators) calculated
  dfpdt(first); //populate dominance frontiers dfs
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    cb->visited = 0;
    cb->work = 0;
  }
  //no need to handle globals
  DYNARR* varas = dactor(prog->dynvars->length);
  for(int i = 0; i < varas->maxlength; i++)
    dapushc(varas, dactor(16)); //initialize array for blocks that modify var
  //variable modification annotation, pass 1
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    if(cb->lastop) {
      OPERATION* op = cb->firstop;
      while(1) {
        switch(op->opcode) {
          case ADDR_3:
            if((op->addr0_type & (ISVAR | ISDEREF)) == ISVAR) {
              FULLADDR* fad = daget(prog->dynvars, op->addr0.varnum);
              fad->addr_type |= ADDRSVAR;
            }
            //fall through
          case ADD_U: case ADD_I: case ADD_F: case SUB_U: case SUB_I: case SUB_F:
          case MULT_U: case MULT_I: case MULT_F: case DIV_U: case DIV_I: case DIV_F:
          case MOD_U: case MOD_I:
          case SHL_U: case SHL_I: case SHR_U: case SHR_I:
          case AND_U: case AND_F: case OR_U: case OR_F: case XOR_U: case XOR_F:
          case INC_U: case INC_I: case INC_F: case DEC_U: case DEC_I: case DEC_F:
          case NEG_I: case NEG_F:
          case EQ_U: case EQ_I: case EQ_F: case NE_U: case NE_I: case NE_F:
          case GE_U: case GE_I: case GE_F: case LE_U: case LE_I: case LE_F:
          case GT_U: case GT_I: case GT_F: case LT_U: case LT_I: case LT_F:
          case MOV_3: case CALL_3: case ARROFF:
          case F2I: case I2F: case ALOC_3:
          //arrmov, mtp_off, copy_3 must have pointer dest
            if((op->dest_type & (ISVAR | ISDEREF | ADDRSVAR) == ISVAR) {
              DYNARR* dda = daget(varas, op->dest.varnum);
              if(!dda->length || dapeek(dda) != cb)
                dapush(dda, cb);
            }
          default:
            break; //no possible correct destination
        }
        if(op == cb->lastop) break;
        op = op->nextop;
      }
    }
  }
  //join node insertion, pass 2
  DYNARR* W = dactor(blocks->length);
  for(int i = 1; i <= prog->dynvars->length; i++) {
    FULLADDR* fadr = daget(prog->dynvars, i - 1);
    if(fadr->addr_type & ADDRSVAR) continue;
    DYNARR* blockassigns = daget(varas, i - 1);
    for(int j = 0; j < blockassigns->length; j++) {
      BBLOCK* block = daget(blockassigns, j);
      block->work = i;//do this better
      dapushc(W, block);
    }
    for(int j = 0; j < W->length; j++) {
      BBLOCK* block = daget(W, j);
      if(block->df) {
        for(int k = 0; k < block->df->length; k++) {
          BBLOCK* domblock = daget(block->df, k);
          if(domblock->visited != i) {
            ADDRESS jadr;
            jadr.joins = dactor(block->inedges->length == 2 ? 2 : 8);
            OPERATION* phi = ct_3ac_op2(PHI, ISCONST, jadr, fadr->addr_type, fadr->addr);
            phi->nextop = domblock->firstop;
            domblock->firstop = phi;
            domblock->visited = i;
            if(domblock->work != i) {
              domblock->work = i;
              dapushc(W, domblock);
            }
          }
        }
      }
    }
  }
  dadtor(blockstack);
  //variable renaming, pass 3
  dadtor(W);
  int* C = calloc(sizeof(int), varas->length);
  for(int i = 0; i < varas->length; i++) {
    DYNARR* da = daget(varas, i);
    da->length = 0;
  }
  dadtorcfr(varas, (void(*)(void*))dadtor);

}
