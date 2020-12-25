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
  DYNARR* W = dactor(prog->allblocks->length);
  for(int i = 0; i < prog->fixedvars->length; i++) {
    FULLADDR* fadr = daget(prog->fixedvars, i);
    //find blocks that assign to variable (could use regno?)
    //for each block, block->work = i + 1; and push block to W
    //for each block in W:
    //  for each block in the dominance frontier of block
    //    if domblock->visited != i + 1
    //      then place phi node, domblock->visited = i + 1
    //        if domblock->work != i + 1
    //          domblock->work = i + 1, push domblock to W
  }
  dadtor(W);
  dadtor(blockstack);
}
