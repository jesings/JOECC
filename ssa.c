#include <assert.h>
#include "ssa.h"
#include "opt.h"
#define X(op) case op:

static BBLOCK* intersect(BBLOCK* n1, BBLOCK* n2) {
  while(n1 != n2) {
    while(n1->domind > n2->domind) n1 = n1->dom;
    while(n2->domind > n1->domind) n2 = n2->dom;
  }
  return n1;
}

static BBLOCK* postintersect(BBLOCK* n1, BBLOCK* n2) {
  while(n1 != n2) {
    while(n1->postdomind > n2->postdomind) n1 = n1->postdom;
    while(n2->postdomind > n1->postdomind) n2 = n2->postdom;
  }
  return n1;
}

static char fixedintersect(const BBLOCK* fb, BBLOCK* gb) {
  while(fb->domind < gb->domind) gb = gb->dom;
  return fb->domind == gb->domind;
}

static void rpdt(BBLOCK* root, BBLOCK** aclist, int* ind) {
  if(!root) return;
  if(root->visited) return;
  root->visited = 1;
  rpdt(root->nextblock, aclist, ind);
  rpdt(root->branchblock, aclist, ind);
  root->domind = *ind;
  aclist[--(*ind)] = root;
}

static void rupdt(BBLOCK* root, BBLOCK** aclist, int* ind) {
  if(!root) return;
  if(root->visited) return;
  root->visited = 1;
  for(int i = 0; i < root->inedges->length; i++)
    rupdt(daget(root->inedges, i), aclist, ind);
  root->postdomind = *ind;
  aclist[--(*ind)] = root;
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

static void rrename(BBLOCK* block, int* C, DYNARR* S, PROGRAM* prog) {
  if(!block || block->visited) return;
  DYNARR* assigns = NULL;
  block->visited = 1;
  if(block->lastop) {
    assigns = dactor(32);
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
        case CALL_3: case PHI: case ALOC_3: /*must have constant input in alloc_3*/
          if((op->dest_type & (ADDRSVAR | ISVAR)) == ISVAR) {
            if(op->dest_type & ISDEREF) {
              op->dest.ssaind =  (long) dapeek((DYNARR*) daget(S, op->dest.varnum));
            } else {
              C[op->dest.varnum] = prog->iregcnt++;
              op->dest.ssaind = C[op->dest.varnum];
              dapush((DYNARR*) daget(S, op->dest.varnum), (void*)(long) C[op->dest.varnum]);
              dapush(assigns, (void*)(long) op->dest.varnum);
            }
          }
          break;
        OPS_NODEST_3ac case TPHI:
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
            assert(!C[op->addr0.varnum]);
            C[op->addr0.varnum] = prog->iregcnt++;
            op->addr0.ssaind = C[op->addr0.varnum];
            dapush((DYNARR*) daget(S, op->addr0.varnum), (void*)(long) C[op->addr0.varnum]);
            dapush(assigns, (void*)(long)op->addr0.varnum);
          }
          break;
        OPS_NOVAR_3ac
          break;
        case ASM:
          assert(0);//unimplemented
      }
      if(op == block->lastop) break;
      op = op->nextop;
    }
  }
  if(block->nextblock && block->nextblock->lastop) {
    int i = -1;
    while(daget(block->nextblock->inedges, ++i) != block) ;
    OPERATION* op = block->nextblock->firstop;
    while(op->opcode == PHI) {
      op->addr0.joins[i].addr.ssaind = (int) (long) dapeek((DYNARR*) daget(S, op->dest.varnum));
      op->addr0.joins[i].addr.varnum = op->dest.varnum;
      op->addr0.joins[i].addr_type = op->dest_type;
      if(op == block->nextblock->lastop) break;
      op = op->nextop;
    }
  }
  if(block->branchblock && block->branchblock->lastop) {
    int i = -1;
    while(daget(block->branchblock->inedges, ++i) != block) ;
    OPERATION* op = block->branchblock->firstop;
    while(op->opcode == PHI) {
      op->addr0.joins[i].addr.ssaind = (int) (long) dapeek((DYNARR*) daget(S, op->dest.varnum));
      op->addr0.joins[i].addr.varnum = op->dest.varnum;
      op->addr0.joins[i].addr_type = op->dest_type;
      if(op == block->branchblock->lastop) break;
      op = op->nextop;
    }
  }
  rrename(block->nextblock, C, S, prog);
  rrename(block->branchblock, C, S, prog);
  if(assigns) {
    for(int i = 0; i < assigns->length; i++) {
      long l = (long) daget(assigns, i);
      dapop((DYNARR*) daget(S, l));
    }
    dadtor(assigns);
  }
}

void ssa(PROGRAM* prog) {
  DYNARR* blocks = prog->allblocks;
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->visited = 0;
    dtn->domind = -1;
  }
  BBLOCK** blocklist = calloc(sizeof(BBLOCK*), blocks->length);
  BBLOCK* first = daget(blocks, 0);
  first->dom = first;
  first->domind = 1;
  int ind = blocks->length;
  rpdt(first->nextblock, blocklist, &ind);
  rpdt(first->branchblock, blocklist, &ind);
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    if(dtn->domind == -1) domark(dtn);
  }
  int oldlen = blocks->length;
  rmunreach(prog);
  blocks = prog->allblocks;

  char changed = 1;
  while(changed) {
    changed = 0;
    //https://www.cs.rice.edu/~keith/EMBED/dom.pdf
    for(int i = ind; i < oldlen; i++) {
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
      if(cb->dom != new_idom) {
        cb->dom = new_idom;
        changed = 1;
      }
    }
  }

  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->visited = 0;
    blocklist[i] = NULL;
  }

  ind = blocks->length;
  prog->finalblock->postdom = prog->finalblock;
  prog->finalblock->postdomind = 0;
  for(int i = 0; i < prog->finalblock->inedges->length; i++)
    rupdt(daget(prog->finalblock->inedges, i), blocklist, &ind);

  changed = 1;
  while(!changed) {
    changed = 0;
    for(int i = ind; i < blocks->length; i++) {
      BBLOCK* cb = blocklist[i];
      BBLOCK* new_pidom = NULL;
      if(cb->nextblock->postdom) {
        if(cb->branchblock && cb->branchblock->postdom)
          new_pidom = postintersect(cb->branchblock, cb->nextblock);
        else
          new_pidom = cb->nextblock;
      } else {
        if(cb->branchblock && cb->branchblock->postdom)
          new_pidom = cb->branchblock;
      }
      if(cb->postdom != new_pidom) {
        cb->postdom = new_pidom;
        changed = 1;
      }
    }
  }

  free(blocklist);
  //populate in parents
  first->visited = 0;
  for(int i = 1; i < blocks->length; i++) {//start at one so as not to let start block idominate itself
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
          OPS_3_3ac OPS_2_3ac case CALL_3: case ALOC_3:
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
          if(domblock->visited < itercount && initblock != domblock && fixedintersect(initblock, domblock) && domblock != prog->finalblock) {
            ADDRESS jadr;
            jadr.joins = malloc(domblock->inedges->length * sizeof(FULLADDR));
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
  rrename(first, C, varas, prog);

  dadtorcfr(varas, (void(*)(void*))dadtor);
  free(C);
  prog->pdone |= SSA;
}
//lengauer tarjan: https://www.cl.cam.ac.uk/~mr10/lengtarj.pdf

static EQNODE* cteqnode(EQONTAINER* eqcontainer, int hc) {
  EQNODE* retval = malloc(sizeof(EQNODE));
  retval->hasconst = hc;
  retval->equivs = dactor(8);
  retval->index = eqcontainer->nodes->length;
  retval->regno = -1;
  dapush(eqcontainer->nodes, retval);
  return retval;
}

static EQONTAINER* cteq(PROGRAM* prog) {
  EQONTAINER* retval = malloc(sizeof(EQONTAINER));
  retval->nodes = dactor(1024);
  retval->varnodes = calloc(sizeof(void*), prog->iregcnt);
  retval->intconsthash = htctor();
  retval->floatconsthash = htctor();
  retval->strconsthash = htctor();
  retval->opnodes = dactor(ADDR_3 + 1); //addr_3 is the last op
  retval->opnodes->length = ADDR_3 + 1;
  for(int i = 0; i <= ADDR_3; i++) {
    retval->opnodes->arr[i] = htctor();
  }
  return retval;
}

static EQITEM* cteqi(unsigned int regno) {
  EQITEM* retval = malloc(sizeof(EQITEM));
  retval->regno = regno;
  retval->op = PARAM_3;
  return retval;
}

static EQITEM* cteqib(enum opcode_3ac opc, EQNODE* eq1, EQNODE* eq2) {
  EQITEM* retval = malloc(sizeof(EQITEM));
  retval->op = opc;
  retval->arg0 = eq1;
  retval->arg1 = eq2;
  return retval;
}

static void freqnode(EQNODE* eqnode) {
  dadtorfr(eqnode->equivs);
  free(eqnode);
}

static void freeq(EQONTAINER* eq) {
  dadtorcfr(eq->nodes, (void(*)(void*)) freqnode);
  free(eq->varnodes);
  fhtdtor(eq->intconsthash);
  fhtdtor(eq->floatconsthash);
  htdtor(eq->strconsthash);
  dadtorcfr(eq->opnodes, (void(*)(void*)) fhtdtor);
  free(eq);
}

static EQNODE* nodefromaddr(EQONTAINER* eqcontainer, ADDRTYPE adt, ADDRESS adr, PROGRAM* prog) {
  EQNODE* cn;
  if(adt & ISCONST) {
    if(adt & ISSTRCONST) {
      cn = search(eqcontainer->strconsthash, adr.strconst);
      if(!cn) {
        cn = cteqnode(eqcontainer, STRCONST);
        cn->strconst = adr.strconst;
        insert(eqcontainer->strconsthash, adr.strconst, cn);
      }

    } else {
      char floatness = adt & ISFLOAT;
      cn = fixedsearch(floatness ? eqcontainer->floatconsthash : eqcontainer->intconsthash, adr.intconst_64);
      if(!cn) {
        cn = cteqnode(eqcontainer, floatness ? FLOATCONST : INTCONST);
        cn->intconst = adr.intconst_64;
        fixedinsert(floatness ? eqcontainer->floatconsthash : eqcontainer->intconsthash, adr.intconst_64, cn);
      }
    }
  } else {
    if(adt & (ISLABEL | ISDEREF)) {
      //ignore (for now) TODO: pointer analysis
      return NULL;
    }
    if(adt & ISVAR) {
      FULLADDR* adstore = daget(prog->dynvars, adr.varnum);
      if(adstore->addr_type & ADDRSVAR) return NULL;
    }
    cn = eqcontainer->varnodes[adr.iregnum];
    if(!cn) {
      cn = cteqnode(eqcontainer, NOCONST);
      dapush(cn->equivs, cteqi(adr.iregnum));
      eqcontainer->varnodes[adr.iregnum] = cn;
    }
  }
  return cn;
}

static void replaceop(BBLOCK* blk, EQONTAINER* eq, PROGRAM* prog, OPERATION* op) {
  BITFIELD bf = blk->availability;
  EQNODE* sen;
  switch(op->opcode) {
    OPS_3_3ac_NOCOM OPS_3_3ac_COM case TPHI:
      sen = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
          break;
        } else {
          bfset(bf, sen->index);
          sen->regno = op->dest.iregnum;
        }
      }
      __attribute__((fallthrough));
    OPS_NODEST_3ac OPS_3_PTRDEST_3ac
      sen = nodefromaddr(eq, op->addr1_type, op->addr1, prog);
      if(sen) {
        if(sen->hasconst != NOCONST) {
          op->addr1_type &= 0xf | ISSIGNED;
          op->addr1_type |= ISCONST;
          if(sen->hasconst == STRCONST) op->addr1_type |= ISSTRCONST;
          else if(sen->hasconst == FLOATCONST) op->addr1_type |= ISFLOAT;
          op->addr1.intconst_64 = sen->intconst; //could be anything
        } else {
          assert(bfget(bf, sen->index) || op->opcode == TPHI);
          op->addr1.iregnum = sen->regno;
        }
      }
      __attribute__((fallthrough));
    OPS_1_3ac
      sen = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(sen) {
        if(sen->hasconst != NOCONST) {
          op->addr0_type &= 0xf | ISSIGNED;
          op->addr0_type |= ISCONST;
          if(sen->hasconst == STRCONST) op->addr0_type |= ISSTRCONST;
          else if(sen->hasconst == FLOATCONST) op->addr0_type |= ISFLOAT;
          op->addr0.intconst_64 = sen->intconst; //could be anything
        } else {
          assert(bfget(bf, sen->index) || op->opcode == TPHI);
          op->addr0.iregnum = sen->regno;
        }
      }
      break;
    OPS_2_3ac_MUT case MOV_3: case ADDR_3:
      sen = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
          break;
        } else {
          bfset(bf, sen->index);
          sen->regno = op->dest.iregnum;
        }
      }
      sen = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(sen) {
        if(sen->hasconst != NOCONST) {
          op->addr0_type &= 0xf | ISSIGNED;
          op->addr0_type |= ISCONST;
          if(sen->hasconst == STRCONST) op->addr0_type |= ISSTRCONST;
          else if(sen->hasconst == FLOATCONST) op->addr0_type |= ISFLOAT;
          op->addr0.intconst_64 = sen->intconst; //could be anything
        } else {
          assert(bfget(bf, sen->index));
          op->addr0.iregnum = sen->regno;
        }
      }
      break;
    case CALL_3: case ALOC_3:
      sen = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
          fprintf(stderr, "Somehow function output is part of an equivalence class\n");
          assert(0);
        } else {
          bfset(bf, sen->index);
          sen->regno = op->dest.iregnum;
        }
      }
      break;
    case PHI:  ;
      FULLADDR* addrs = op->addr0.joins;
      void* sen_tinel = NULL;
      for(int k = 0; k < blk->inedges->length; k++) {
        sen = eq->varnodes[addrs[k].addr.ssaind];
        if(sen) {
          if(!sen_tinel) sen_tinel = sen;
          else if(sen_tinel != sen) sen_tinel = (void*) -1;
          if(sen->hasconst != NOCONST) {
            addrs[k].addr_type &= 0xf | ISSIGNED;
            addrs[k].addr_type |= ISCONST;
            if(sen->hasconst == STRCONST) addrs[k].addr_type |= ISSTRCONST;
            else if(sen->hasconst == FLOATCONST) addrs[k].addr_type |= ISFLOAT;
            addrs[k].addr.intconst_64 = sen->intconst; //could be anything
          } else {
            if(bfget(bf, sen->index)) { //cannot assert here
              addrs[k].addr.iregnum = sen->regno;
            }
          }
        } else {
          sen_tinel = (void*) -1;
        }
      }
      if(sen_tinel != (void*) -1) {
        op->opcode = NOP_3;
        free(op->addr0.joins);
      }
      sen = nodefromaddr(eq, op->dest_type, op->dest, prog);
      assert(sen->hasconst == NOCONST);
      assert(!bfget(bf, sen->index));
      bfset(bf, sen->index);
      sen->regno = op->dest.iregnum;
      break;
    OPS_1_ASSIGN_3ac
      sen = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
        } else {
          bfset(bf, sen->index);
          sen->regno = op->addr0.iregnum;
        }
      }
      break;
    OPS_NOVAR_3ac
      break;
    case ASM:
      assert(0); //unimplemented
  }
  
}

static void replacenode(BBLOCK* blk, EQONTAINER* eq, PROGRAM* prog) {
  BITFIELD bf = blk->availability;
  if(blk->lastop) {
    OPERATION* op = blk->firstop;
    while(1) {
      replaceop(blk, eq, prog, op);
      if(op == blk->lastop) break;
      op = op->nextop;
    }
  }
  if(blk->idominates) {
    for(int i = 0; i < blk->idominates->length; i++) {
      ((BBLOCK*) daget(blk->idominates, i))->availability = bfclone(bf, eq->nodes->length);
      replacenode(daget(blk->idominates, i), eq, prog);
    }
  }
}

void gvn(PROGRAM* prog) { //Constructs, populates Strong Equivalence DAG
  EQONTAINER* eqcontainer = cteq(prog);
  BBLOCK* first = daget(prog->allblocks, 0);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      OPERATION* op = blk->firstop;
      EQNODE* sen1,* sen2,* destsen;
      long combind;
      HASHTABLE* ophash;
      while(1) {
        switch(op->opcode) {
          OPS_NOVAR_3ac
            break; //nothing for nop, lbl, jmp, branching ops, or arg/ret
          OPS_3_3ac_NOCOM
            sen1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            sen2 = nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1 && sen2) {
                ophash = daget(eqcontainer->opnodes, op->opcode);
                combind = ((long) sen1->index << 32) + sen2->index;
                destsen = fixedsearch(ophash, combind);
                if(!destsen) {
                  destsen = cteqnode(eqcontainer, NOCONST);
                  dapush(destsen->equivs, cteqib(op->opcode, sen1, sen2));
                  fixedinsert(ophash, combind, destsen);
                }
              } else {
                destsen = cteqnode(eqcontainer, NOCONST);
              }
              dapush(destsen->equivs, cteqi(op->dest.iregnum));
              eqcontainer->varnodes[op->dest.iregnum] = destsen;
            }
            break;
          OPS_3_3ac_COM
            sen1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            sen2 = nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1 && sen2) {
                ophash = daget(eqcontainer->opnodes, op->opcode);
                combind = ((long) sen1->index << 32) + sen2->index;
                destsen = fixedsearch(ophash, combind);
                if(!destsen) {
                  destsen = cteqnode(eqcontainer, NOCONST);
                  dapush(destsen->equivs, cteqib(op->opcode, sen1, sen2));
                  fixedinsert(ophash, combind, destsen);
                  fixedinsert(ophash, ((long) sen2->index << 32) + sen1->index, destsen);
                }
              } else {
                destsen = cteqnode(eqcontainer, NOCONST);
              }
              dapush(destsen->equivs, cteqi(op->dest.iregnum));
              eqcontainer->varnodes[op->dest.iregnum] = destsen;
            }
            break;
          OPS_2_3ac_MUT
            sen1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1) {
                ophash = daget(eqcontainer->opnodes, op->opcode);
                destsen = fixedsearch(ophash, (long) sen1->index);
                if(!destsen) {
                  destsen = cteqnode(eqcontainer, NOCONST);
                  dapush(destsen->equivs, cteqib(op->opcode, sen1, NULL));
                  fixedinsert(ophash, (long) sen1->index, destsen);
                }
              } else {
                destsen = cteqnode(eqcontainer, NOCONST);
              }
              dapush(destsen->equivs, cteqi(op->dest.iregnum));
              eqcontainer->varnodes[op->dest.iregnum] = destsen;
            }
            break;
          case MOV_3:
            sen1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(!sen1) {
                sen1 = cteqnode(eqcontainer, NOCONST);
              }
              dapush(sen1->equivs, cteqi(op->dest.iregnum));
              eqcontainer->varnodes[op->dest.iregnum] = sen1;
            }
            break;
          case ADDR_3:
            //address should stay constant, so the value can be stored, as can the value of labels!
            sen1 = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
            if(!(op->dest_type & (ISDEREF | ISLABEL))) {
              //addrsvar is permissible
              if(sen1) {
                ophash = daget(eqcontainer->opnodes, op->opcode);
                destsen = fixedsearch(ophash, (long) sen1->index);
                if(!destsen) {
                  destsen = cteqnode(eqcontainer, NOCONST);
                  dapush(destsen->equivs, cteqib(op->opcode, sen1, NULL));
                  fixedinsert(ophash, (long) sen1->index, destsen);
                }
              } else {
                destsen = cteqnode(eqcontainer, NOCONST);
              }
              dapush(destsen->equivs, cteqi(op->dest.iregnum));
              eqcontainer->varnodes[op->dest.iregnum] = destsen;
            }
            break;
          case TPHI:
            sen1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            sen2 = nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1 && sen2) {
                combind = ((long) sen1->index << 32) + sen2->index;
                if(sen1 == sen2) {
                  destsen = sen1;
                } else {
                  ophash = daget(eqcontainer->opnodes, op->opcode);
                  destsen = fixedsearch(ophash, combind);
                  if(!destsen) {
                    destsen = fixedsearch(ophash, ((long) sen2->index << 32) + sen1->index);
                  }
                  if(!destsen) {
                    destsen = cteqnode(eqcontainer, NOCONST);
                    dapush(destsen->equivs, cteqib(op->opcode, sen1, sen2));
                    fixedinsert(ophash, combind, destsen);
                  }
                }
              } else {
                destsen = cteqnode(eqcontainer, NOCONST);
              }
              dapush(destsen->equivs, cteqi(op->dest.iregnum));
              eqcontainer->varnodes[op->dest.iregnum] = destsen;
            }
            nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
            break;
          case PHI: //TODO: get phi node handling loop at end
            nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
            break;
          OPS_3_PTRDEST_3ac OPS_NODEST_3ac
            nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
            __attribute__((fallthrough));
          OPS_1_3ac 
            nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            break;
          case CALL_3: //no pure functions for now
          case ALOC_3:
            nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
            break;
          OPS_1_ASSIGN_3ac
            nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
            break;
          case ASM:
            assert(0); //unimplemented
        }
        if(op == blk->lastop) break;
        op = op->nextop;
      }
    }
  }

  first->availability = bfalloc(eqcontainer->nodes->length);
  first->anticipability = bfalloc(eqcontainer->nodes->length);
  int rplistind = prog->allblocks->length;
  BBLOCK** rplist = malloc(prog->allblocks->length * sizeof(BBLOCK*));
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    blk->visited = 0;
  } //recalculate to tighten length
  rplist[0] = first;
  rpdt(first->nextblock, rplist, &rplistind);
  rpdt(first->branchblock, rplist, &rplistind);
  for(int i = 0; i < rplistind; i++) {
    //BBLOCK* blk = rplist[i];
  }
  replacenode(first, eqcontainer, prog);

  free(rplist);
  for(int i = 0; i < prog->allblocks->length; i++) {
    free(((BBLOCK*) daget(prog->allblocks, i))->availability);
    free(((BBLOCK*) daget(prog->allblocks, i))->anticipability);
  }

  freeq(eqcontainer);
}
//https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/gvn_sas04.pdf
//https://www.cs.purdue.edu/homes/hosking/papers/cc04.pdf
#undef X
