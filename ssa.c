#include <assert.h>
#include "ssa.h"
#define X(op) case op:

static BBLOCK* intersect(BBLOCK* n1, BBLOCK* n2) {
  while(n1 != n2) {
    while(n1->domind > n2->domind) n1 = n1->dom;
    while(n2->domind > n1->domind) n2 = n2->dom;
  }
  return n1;
}

static char fixedintersect(const BBLOCK* fb, BBLOCK* gb) {
  while(fb->domind < gb->domind) gb = gb->dom;
  return fb->domind == gb->domind;
}

static void rpdt(BBLOCK* root, void** aclist, int* ind) {
  if(!root) return;
  if(root->visited) return;
  root->visited = 1;
  rpdt(root->nextblock, aclist, ind);
  rpdt(root->branchblock, aclist, ind);
  root->domind = *ind;
  aclist[*ind - 1] = root;
  --*ind;
}

static void dfpdt(BBLOCK* root) {
  if(!root) return;
  if(root->visited) return;
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
        char flag = 1;
        for(int i = 0; i < root->df->length; i++) {
          //speed this up?
          if(daget(root->df, i) == kb) {
            flag = 0;
            break;
          }
        }
        if(flag && kb != root && kb->dom != root) {
          dapush(root->df, kb);
        }
      }
    }
  }
}

static void rrename(BBLOCK* block, int* C, DYNARR* S) {
  if(!block || block->visited) return;
  block->visited = 1;
  if(block->lastop) {
    OPERATION* op = block->firstop;
    while(1) {
      switch(op->opcode) {
        OPS_3_3ac OPS_3_PTRDEST_3ac
          if((op->addr1_type & (ADDRSVAR | ISVAR)) == ISVAR) 
            op->addr1.ssaind =  (long) dapeek((DYNARR*) daget(S, op->addr1.varnum));
          __attribute__((fallthrough));
        OPS_2_3ac case ADDR_3:
          if((op->addr0_type & (ADDRSVAR | ISVAR)) == ISVAR)
            op->addr0.ssaind =  (long) dapeek((DYNARR*) daget(S, op->addr0.varnum));
          __attribute__((fallthrough));
        case CALL_3: case PHI:
          if((op->dest_type & (ADDRSVAR | ISVAR)) == ISVAR) {
            if(op->dest_type & ISDEREF) {
              op->dest.ssaind =  (long) dapeek((DYNARR*) daget(S, op->dest.varnum));
            } else {
              op->dest.ssaind = C[op->dest.varnum];
              dapush((DYNARR*) daget(S, op->dest.varnum), (void*)(long) C[op->dest.varnum]);
              ++(C[op->dest.varnum]);
            }
          }
          break;
        OPS_NODEST_3ac
          if((op->addr1_type & (ADDRSVAR | ISVAR)) == ISVAR) 
            op->addr1.ssaind =  (long) dapeek((DYNARR*) daget(S, op->addr1.varnum));
          __attribute__((fallthrough));
        OPS_1_3ac
          if((op->addr0_type & (ADDRSVAR | ISVAR)) == ISVAR)
            op->addr0.ssaind =  (long) dapeek((DYNARR*) daget(S, op->addr0.varnum));
          break;
        OPS_1_ASSIGN_3ac
          if((op->addr0_type & (ADDRSVAR | ISVAR)) == ISVAR) {
            assert(!(op->addr0_type & ISDEREF));
            assert(!C[op->addr0.varnum]); //would work if not for join nodes after scope
            op->addr0.ssaind = 0;
            dapush((DYNARR*) daget(S, op->addr0.varnum), NULL);
            C[op->addr0.varnum] = 1;
          }
          break;
        OPS_NOVAR_3ac
          break;
      }
      if(op == block->lastop) break;
      op = op->nextop;
    }
  }
  //handle phis in children
  rrename(block->nextblock, C, S);
  rrename(block->branchblock, C, S);
  //reset stack
}

//TODO: implement lengauer tarjan: https://www.cl.cam.ac.uk/~mr10/lengtarj.pdf
void ctdtree(PROGRAM* prog) {
  DYNARR* blocks = prog->allblocks;
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->dom = NULL;
    dtn->visited = 0;
  }
  void** blocklist = calloc(sizeof(void*), (blocks->length - 1));
  BBLOCK* first = daget(blocks, 0);
  first->dom = first;
  first->domind = 0;
  int ind = blocks->length - 1;
  rpdt(first->nextblock, blocklist, &ind);
  rpdt(first->branchblock, blocklist, &ind);
  //maybe reverse order now, rather than iterate backwards
  char changed = 1;
  while(changed) {
    changed = 0;
    //https://www.cs.rice.edu/~keith/EMBED/dom.pdf
    for(int i = 0; i < blocks->length - 1; i++) {
      BBLOCK* cb = blocklist[i];
      if(!cb) continue;
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
  free(blocklist);
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
            __attribute__((fallthrough));
          OPS_3_3ac OPS_2_3ac case CALL_3:
          //arrmov, mtp_off, copy_3 must have pointer dest
            if((op->dest_type & (ISVAR | ISDEREF | ADDRSVAR)) == ISVAR) {
              DYNARR* dda = daget(varas, op->dest.varnum);
              if(!dda->length || dapeek(dda) != cb)
                dapush(dda, cb);
            }
            break;
          OPS_1_ASSIGN_3ac
            if(!(op->addr0_type & ADDRSVAR)) {
              DYNARR* dda = daget(varas, op->addr0.varnum);
              dapushc(dda, cb);
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
  int itercount = 0;
  for(int i = 0; i < prog->dynvars->length; i++) {
    ++itercount;
    FULLADDR* fadr = daget(prog->dynvars, i);
    if(fadr->addr_type & ADDRSVAR) continue;
    DYNARR* blockassigns = daget(varas, i);
    for(int j = 0; j < blockassigns->length; j++) {
      BBLOCK* block = daget(blockassigns, j);
      block->work = itercount;
      dapush(W, block);
    }
    BBLOCK* initblock = daget(blockassigns, 0);
    for(int j = 0; j < W->length; j++) {
      BBLOCK* block = daget(W, j);
      if(block->df) {
        for(int k = 0; k < block->df->length; k++) {
          BBLOCK* domblock = daget(block->df, k);
          if(domblock->visited < itercount && initblock != domblock && fixedintersect(initblock, domblock)) {
            ADDRESS jadr;
            jadr.joins = dactor(block->inedges->length);
            OPERATION* phi = ct_3ac_op2(PHI, ISCONST, jadr, fadr->addr_type, fadr->addr);
            phi->nextop = domblock->firstop;
            if(!domblock->lastop) domblock->lastop = phi;
            domblock->firstop = phi;
            domblock->visited = itercount;
            if(domblock->work < itercount) {
              domblock->work = itercount;
              dapushc(W, domblock);
            }
          }
        }
      }
    }
    W->length = 0;
  }
  dadtor(W);

  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    cb->visited = 0;
  }

  //variable renaming, pass 3
  int* C = calloc(sizeof(int), varas->length);
  for(int i = 0; i < varas->length; i++) {
    DYNARR* da = daget(varas, i);
    da->length = 0;
  }
  rrename(first, C, varas);

  dadtorcfr(varas, (void(*)(void*))dadtor);
  free(C);
  prog->pdone |= SSA;
}
#undef X
