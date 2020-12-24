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
  while(1) {
    char changed = 0;
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
    if(!changed) break;
    for(int i = 0; i < blocks->length; i++) {
      BBLOCK* cb = daget(blocks, i);
      cb->visited = 0;
    }
  }
  dadtor(blockstack);
}
