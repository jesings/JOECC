#include <assert.h>
#include "ssa.h"
#include "opt.h"
#define X(op) case op:

//finds the block that dominates both of these blocks and does 
//not dominate any other block that dominates both blocks (i.e. the lowest one)
BBLOCK* intersect(BBLOCK* n1, BBLOCK* n2) {
  while(n1 != n2) {
    while(n1->domind > n2->domind) n1 = n1->dom;
    while(n2->domind > n1->domind) n2 = n2->dom;
  }
  return n1;
}

//same as above but for postdominators
static BBLOCK* postintersect(BBLOCK* n1, BBLOCK* n2) {
  while(n1 != n2) {
    while(n1->postdomind > n2->postdomind) n1 = n1->postdom;
    while(n2->postdomind > n1->postdomind) n2 = n2->postdom;
  }
  return n1;
}

//Find if the fixed block dominates the other block
char fixedintersect(const BBLOCK* fb, BBLOCK* gb) {
  while(fb->domind < gb->domind) gb = gb->dom;
  return fb->domind == gb->domind;
}

//recursively populate dominator tree
static void rpdt(BBLOCK* root, BBLOCK** aclist, int* ind) {
  if(!root) return;
  if(root->visited) return;
  root->visited = 1;
  rpdt(root->nextblock, aclist, ind);
  rpdt(root->branchblock, aclist, ind);
  root->domind = *ind;
  aclist[--(*ind)] = root;
}

//recursively populate postdominator tree
static void rupdt(BBLOCK* root, BBLOCK** aclist, int* ind) {
  if(root->visited) return;
  root->visited = 1;
  for(int i = 0; i < root->inedges->length; i++)
    rupdt(daget(root->inedges, i), aclist, ind);
  root->postdomind = *ind;
  aclist[--(*ind)] = root;
}

//calculate dominance frontier, dominator tree
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
        if(flag && kb->dom != root) {
          dapush(root->df, kb);
        }
      }
    }
  }
}

//rename all registers in block based on SSA
static void rrename(BBLOCK* block, int* C, DYNARR* S, PROGRAM* prog) {
  if(!block || block->visited) return;
  DYNARR* assigns = NULL;
  block->visited = 1;
  if(block->lastop) {
    assigns = dactor(32);
    OPERATION* op = block->firstop;
    DYNARR* bdarr;
    while(1) {
      switch(op->opcode) {
        OPS_3_3ac OPS_3_PTRDEST_3ac
          if(op->addr1_type & ISVAR) {
            bdarr = daget(S, op->addr1.varnum);
            if(bdarr->length) //in case of addrsvar
              op->addr1.ssaind =  (long) dapeek(bdarr);
          }
          __attribute__((fallthrough));
        OPS_2_3ac case ADDR_3:
          if(op->addr0_type & ISVAR) {
            bdarr = daget(S, op->addr0.varnum);
            if(bdarr->length) 
              op->addr0.ssaind =  (long) dapeek(bdarr);
          }
          __attribute__((fallthrough));
        case CALL_3: case PHI: /*must have constant input in alloc_3*/
          if(op->dest_type & ISVAR) {
            bdarr = daget(S, op->dest.varnum);
            if(bdarr->length) {
              if(op->dest_type & ISDEREF) {
                op->dest.ssaind =  (long) dapeek(bdarr);
              } else {
                C[op->dest.varnum] = prog->regcnt++;
                op->dest.ssaind = C[op->dest.varnum];
                dapush(bdarr, (void*)(long) C[op->dest.varnum]);
                dapush(assigns, (void*)(long) op->dest.varnum);
              }
            }
          }
          break;
        OPS_NODEST_3ac
          if(op->addr1_type & ISVAR) {
            bdarr = daget(S, op->addr1.varnum);
            if(bdarr->length) //in case of addrsvar
              op->addr1.ssaind =  (long) dapeek(bdarr);
          }
          __attribute__((fallthrough));
        OPS_1_3ac
          if(op->addr0_type & GARBAGEVAL) break;
          __attribute__((fallthrough));
        case DEALOC:
          if(op->addr0_type & ISVAR) {
            bdarr = daget(S, op->addr0.varnum);
            if(bdarr->length) 
              op->addr0.ssaind =  (long) dapeek(bdarr);
          }
          break;
        OPS_1_ASSIGN_3ac
          if(op->addr0_type & ISVAR) {
            FULLADDR* fad = daget(prog->dynvars, op->addr0.varnum);
            if(!(fad->addr_type & ADDRSVAR)) {
              bdarr = daget(S, op->addr0.varnum);
              assert(!(op->addr0_type & ISDEREF));
              assert(!C[op->addr0.varnum]);
              C[op->addr0.varnum] = prog->regcnt++;
              op->addr0.ssaind = C[op->addr0.varnum];
              dapush(bdarr, (void*)(long) C[op->addr0.varnum]);
              dapush(assigns, (void*)(long)op->addr0.varnum);
            }
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
      if(!(op->addr0_type & GARBAGEVAL)) {
        DYNARR* bdarr = daget(S, op->dest.varnum);
        if(bdarr->length) {
          op->addr0.joins[i].addr.ssaind = (int) (long) dapeek(bdarr);
        }
        op->addr0.joins[i].addr.varnum = op->dest.varnum;
        op->addr0.joins[i].addr_type = op->dest_type;
      }
      if(op == block->nextblock->lastop) break;
      op = op->nextop;
    }
  }
  if(block->branchblock && block->branchblock->lastop) {
    int i = -1;
    while(daget(block->branchblock->inedges, ++i) != block) ;
    OPERATION* op = block->branchblock->firstop;
    while(op->opcode == PHI) {
      assert(!(op->addr0_type & GARBAGEVAL)); //no ternaries can join at a branch block
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
  BBLOCK** blocklist = calloc(sizeof(BBLOCK*), blocks->length + 1);
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
    dtn->postdomind = -1;
  }

  ind = blocks->length;
  if(!prog->finalblock) {
    prog->finalblock = mpblk(); //pseudo final block
    ind++;
    //dapush(prog->allblocks, prog->finalblock);
  }
  prog->finalblock->postdom = prog->finalblock;
  rupdt(prog->finalblock, blocklist, &ind);
  if(ind > 0) {
    for(int i = 0; i < blocks->length; i++) {
      BBLOCK* blk = daget(blocks, i);
      if(blk->postdomind == -1) {
        //check if it has a back-edge if so, add an inedge in finalblock then, recalculate postdom tree
        if((blk->nextblock && blk->nextblock->domind < blk->domind) ||
           (blk->branchblock && blk->branchblock->domind < blk->domind)) {
          dapush(prog->finalblock->inedges, blk);
        }
      }
      blk->visited = 0;
    }
    for(int i = 0; i < prog->finalblock->inedges->length; i++)
      rupdt(daget(prog->finalblock->inedges, i), blocklist, &ind);
    prog->finalblock->visited = 0;
  } else {
    for(int i = 0; i < blocks->length; i++) {
      BBLOCK* blk = daget(blocks, i);
      blk->visited = 0;
    }
  }
  changed = 1;
  while(changed) {
    changed = 0;
    for(int i = ind; i < blocks->length; i++) {
      BBLOCK* cb = blocklist[i];
      if(!cb) continue;
      BBLOCK* new_pidom = NULL;
      if(cb->nextblock) {
        if(!cb->branchblock) new_pidom = cb->nextblock;
        else {
          if(cb->nextblock->postdom) {
            if(cb->branchblock && cb->branchblock->postdom)
              new_pidom = postintersect(cb->branchblock, cb->nextblock);
            else
              new_pidom = cb->nextblock;
          } else {
            if(cb->branchblock && cb->branchblock->postdom)
              new_pidom = cb->branchblock;
          }
        }
      }
      if(new_pidom != NULL && cb->postdom != new_pidom) {
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
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    BBLOCK* pdblk = blk->postdom;
    if(!pdblk) {
      pdblk = prog->finalblock;
    }
    if(!pdblk->pidominates) pdblk->pidominates = dactor(8);
    dapush(pdblk->pidominates, blk);
  }
  if(prog->finalblock->pidominates) {
    dharma(prog->finalblock->pidominates, prog->finalblock);
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
          //ARRMOV, MTP_OFF, COPY_3 must have pointer dest
            if((op->dest_type & (ISVAR | ISDEREF | ADDRSVAR)) == ISVAR) {
              DYNARR* dda = daget(varas, op->dest.varnum);
              if(!dda->length || dapeek(dda) != cb)
                dapush(dda, cb);
            }
            break;
          OPS_1_ASSIGN_3ac
            if(!(op->addr0_type & ADDRSVAR)) {
              DYNARR* dda = daget(varas, op->addr0.varnum);
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
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    cb->visited = 0;
  }

  dadtorcfr(varas, (void(*)(void*))dadtor);
  free(C);
  prog->pdone |= SSA;
}
//lengauer tarjan: https://www.cl.cam.ac.uk/~mr10/lengtarj.pdf

static GVNNUM* ctgvnnum(EQONTAINER* eqcontainer, int hc, ADDRTYPE typ) {
  GVNNUM* retval = malloc(sizeof(GVNNUM));
  retval->hasconst = hc;
  retval->equivs = dactor(8);
  retval->index = eqcontainer->nodes->length;
  retval->commontype = typ & GENREGMASK & ~ISPOINTER;
  dapush(eqcontainer->nodes, retval);
  return retval;
}

static EQONTAINER* cteq(PROGRAM* prog) {
  EQONTAINER* retval = malloc(sizeof(EQONTAINER));
  retval->nodes = dactor(1024);
  ctgvnnum(retval, NOCONST, 0); //have a dummy value in the zero position
  retval->intconsthash = htctor();
  retval->floatconsthash = htctor();
  retval->strconsthash = htctor();
  retval->ophash = bightctor();
  return retval;
}

static void fregvnnum(GVNNUM* eqnode) {
  dadtorfr(eqnode->equivs);
  free(eqnode);
}

static void freeq(EQONTAINER* eq) {
  dadtorcfr(eq->nodes, (void(*)(void*)) fregvnnum);
  fhtdtor(eq->intconsthash);
  fhtdtor(eq->floatconsthash);
  htdtor(eq->strconsthash);
  bightdtor(eq->ophash);
  free(eq);
}

static VALUESTRUCT* genx(VALUESTRUCT* millenial) {
  VALUESTRUCT* boomer = malloc(sizeof(VALUESTRUCT));
  memcpy(boomer, millenial, sizeof(VALUESTRUCT));
  return boomer;
}

static GVNNUM* lookup_const(EQONTAINER* eq, ADDRTYPE adt, ADDRESS adr) {
    assert(adt & ISCONST);
    if(adt & ISSTRCONST) {
        return search(eq->strconsthash, adr.strconst);
    } else if(adt & ISFLOAT) {
        return fixedsearch(eq->floatconsthash, adr.intconst_64);
    } else {
        return fixedsearch(eq->intconsthash, adr.intconst_64);
    }
}

//find which equivalence node, if any, this address corresponds to, and create one if it corresponds to nothing extant
static GVNNUM* nodefromaddr(EQONTAINER* eqcontainer, ADDRTYPE adt, ADDRESS adr, PROGRAM* prog) {
  GVNNUM* cn;
  if(adt & ISCONST) {
    if(adt & ISSTRCONST) {
      cn = search(eqcontainer->strconsthash, adr.strconst);
      if(!cn) {
        cn = ctgvnnum(eqcontainer, STRCONST, adt);
        cn->strconst = adr.strconst;
        insert(eqcontainer->strconsthash, adr.strconst, cn);
      }

    } else {
      char floatness = adt & ISFLOAT;
      cn = fixedsearch(floatness ? eqcontainer->floatconsthash : eqcontainer->intconsthash, adr.intconst_64);
      if(!cn) {
        cn = ctgvnnum(eqcontainer, floatness ? FLOATCONST : INTCONST, adt);
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
    VALUESTRUCT exst = {INIT_3, supersize(adt), 0, adr.regnum, 0};
    cn = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(VALUESTRUCT));
    if(!cn) {
      cn = ctgvnnum(eqcontainer, NOCONST, adt);
      VALUESTRUCT* exn = ctvalstruct(INIT_3, supersize(adt), 0, adr.regnum, 0);
      bigfinsertfr(eqcontainer->ophash, (char*) exn, cn, sizeof(VALUESTRUCT));
      dapush(cn->equivs, genx(exn));
    }

    if(adt != 0) {
      assert((cn->commontype & ISFLOAT) == (adt & ISFLOAT)); //assert the types
      if((cn->commontype & 0xf) < (adt & 0xf)) {
        cn->commontype = adt & GENREGMASK;
      }
    }
  }
  return cn;
}

//replace operation via gvn
static void replaceop(BBLOCK* blk, EQONTAINER* eq, PROGRAM* prog, OPERATION* op) {
  HASHTABLE* leader = blk->leader;
  GVNNUM* val;
  switch(op->opcode) {
    OPS_3_3ac_NOCOM OPS_3_3ac_COM
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        if(val->hasconst != NOCONST) {
          op->opcode = NOP_3;
          break;
        }
        if(op->dest.regnum != (long) fixedsearch(leader, val->index)) {
          op->opcode = NOP_3;
          break;
        } else {
          //resize based on commontype?
        }
      }
      __attribute__((fallthrough));
    OPS_NODEST_3ac OPS_3_PTRDEST_3ac
      val = nodefromaddr(eq, op->addr1_type, op->addr1, prog);
      if(val) {
        if(val->hasconst != NOCONST) {
          op->addr1_type = (op->addr1_type & 0xf) | (val->commontype & ~0xf) | ISCONST;
          op->addr1.intconst_64 = val->intconst; //could be anything
        } else {
          op->addr1.regnum = (long) fixedsearch(leader, val->index);
        }
      }
      __attribute__((fallthrough));
    OPS_1_3ac
      if(op->addr0_type & GARBAGEVAL) break;
      val = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(val) {
        if(val->hasconst != NOCONST) {
          op->addr0_type = (op->addr0_type & 0xf) | (val->commontype & ~0xf) | ISCONST;
          op->addr0.intconst_64 = val->intconst; //could be anything
        } else {
          op->addr0.regnum = (long) fixedsearch(leader, val->index);
        }
      }
      break;
    case DEALOC:
      //don't really know how to handle this well
      break;

    OPS_2_3ac_MUT case MOV_3: case ADDR_3:
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        if(val->hasconst != NOCONST) {
          op->opcode = NOP_3;
          break;
        }
        if(op->dest.regnum != (long) fixedsearch(leader, val->index)) {
          op->opcode = NOP_3;
          break;
        } else {
          //resize based on commontype?
        }
      }
      val = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(val) {
        if(val->hasconst != NOCONST) {
          op->addr0_type = (op->addr0_type & 0xf) | (val->commontype & ~0xf) | ISCONST;
          op->addr0.intconst_64 = val->intconst; //could be anything
        } else {
          op->addr0.regnum = (long) fixedsearch(leader, val->index);
        }
      }
      break;
    case CALL_3:
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        assert(val->hasconst == NOCONST);
        assert(op->dest.regnum == (long) fixedsearch(leader, val->index));
      }
      break;
    OPS_1_ASSIGN_3ac
      val = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(val) {
        assert(val->hasconst == NOCONST);
        assert(op->addr0.regnum == (long) fixedsearch(leader, val->index));
      }
      break;
    case PHI: 
      for(int k = 0; k < blk->inedges->length; k++) {
        FULLADDR* fadrs = op->addr0.joins;
        val = nodefromaddr(eq, fadrs[k].addr_type, fadrs[k].addr, prog);
        if(val) {
          if(val->hasconst != NOCONST) {
            fadrs[k].addr_type = (fadrs[k].addr_type & 0xf) | (val->commontype & ~0xf) | ISCONST;
            fadrs[k].addr.intconst_64 = val->intconst; //could be anything
          } else {
            fadrs[k].addr.regnum = (long) fixedsearch(leader, val->index);
          }
        }
      }
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        assert(val->hasconst == NOCONST);
        assert(op->dest.regnum == (long) fixedsearch(leader, val->index));
      }
      break;
    OPS_NOVAR_3ac
      break;
    case ASM:
      assert(0); //unimplemented
  }
}

static void replacegvn(EQONTAINER* eq, PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      OPERATION* op = blk->firstop;
      while(1) {
        replaceop(blk, eq, prog, op);
        if(op == blk->lastop) break;
        op = op->nextop;
      }
    }
  }
}

//number values
static void gensall(PROGRAM* prog, EQONTAINER* eqcontainer, BBLOCK* blk) {
  blk->leader = fhtclone(blk->dom->leader);
  if(blk->lastop) {
    blk->tmp_gen = dinctor(32); //not sure what to do with this
    blk->exp_gen = htctor();
    blk->antileader_in = htctor();
    blk->antileader_out = htctor();
    OPERATION* op = blk->firstop;
    VALUESTRUCT exst = {INIT_3, 0, 0, 0, 0};
    do {
      GVNNUM* val1;
      GVNNUM* val2;
      GVNNUM* destval = NULL;
      GVNNUM* otherval = NULL;
      BIGHASHTABLE* ophash = eqcontainer->ophash;
      switch(op->opcode) {
        OPS_NOVAR_3ac
          break; //nothing for nop, lbl, jmp, branching ops, or arg/ret
        OPS_3_3ac_NOCOM
          val1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
          val2 = nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1 && val2) {
              VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), supersize(val2->commontype), val1->index, val2->index};
              destval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
              if(!destval) {
                destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
                dapush(destval->equivs, genx(&combind));
                bigfinsertfr(ophash, (char*) genx(&combind), destval, sizeof(VALUESTRUCT));
              } //destination type need not factor into largest use
            } else {
              destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0));
            bigfinsertfr(ophash, (char*) ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0), destval, sizeof(VALUESTRUCT));
          } else if(val1 && val2) {
            VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), supersize(val2->commontype), val1->index, val2->index};
            otherval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
            if(!otherval) {
              otherval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
              dapush(otherval->equivs, genx(&combind));
              bigfinsertfr(ophash, (char*) genx(&combind), otherval, sizeof(VALUESTRUCT));
            }
          }
          break;
        OPS_3_3ac_COM
          val1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
          val2 = nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1 && val2) {
              VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), supersize(val2->commontype), val1->index, val2->index};
              destval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
              if(!destval) {
                destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
                dapush(destval->equivs, genx(&combind));
                bigfinsertfr(ophash, (char*) genx(&combind), destval, sizeof(VALUESTRUCT));
                VALUESTRUCT* combind2 = ctvalstruct(op->opcode, supersize(val2->commontype), supersize(val1->commontype), val2->index, val1->index);
                bigfinsertfr(ophash, (char*) combind2, destval, sizeof(VALUESTRUCT));
              }
            } else {
              destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0));
            bigfinsertfr(ophash, (char*) ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0), destval, sizeof(VALUESTRUCT));
          } else if(val1 && val2) {
            VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), supersize(val2->commontype), val1->index, val2->index};
            otherval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
            if(!otherval) {
              otherval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
              dapush(otherval->equivs, genx(&combind));
              bigfinsertfr(ophash, (char*) genx(&combind), otherval, sizeof(VALUESTRUCT));
              VALUESTRUCT* combind2 = ctvalstruct(op->opcode, supersize(val2->commontype), supersize(val1->commontype), val2->index, val1->index);
              bigfinsertfr(ophash, (char*) combind2, otherval, sizeof(VALUESTRUCT));
            }
          }
          break;
        OPS_2_3ac_MUT
          val1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1) {
              VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), 0, val1->index, 0};
              destval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
              if(!destval) {
                destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
                dapush(destval->equivs, genx(&combind));
                bigfinsertfr(ophash, (char*) genx(&combind), destval, sizeof(VALUESTRUCT));
              }
            } else {
              destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0));
            bigfinsertfr(ophash, (char*) ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0), destval, sizeof(VALUESTRUCT));
          } else if(val1) {
            VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), 0, val1->index, 0};
            otherval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
            if(!otherval) {
              otherval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
              dapush(otherval->equivs, genx(&combind));
              bigfinsertfr(ophash, (char*) genx(&combind), otherval, sizeof(VALUESTRUCT));
            }
          }
          break;
        case MOV_3:
          val1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1) {
              destval = val1;
              //mov from low to high need not enlarge maximum size of commontype
            } else {
              destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, supersize(op->dest_type), 0, 0, op->dest.regnum, 0));
            bigfinsertfr(ophash, (char*) ctvalstruct(INIT_3, supersize(op->dest_type), 0, 0, op->dest.regnum, 0), destval, sizeof(VALUESTRUCT));
          }
          break;
        case ADDR_3:
          //address should stay constant, so the value can be stored, as can the value of labels!
          val1 = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
          if(!(op->dest_type & (ISDEREF | ISLABEL))) {
            //addrsvar is permissible
            if(val1) {
              VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), 0, val1->index, 0};
              destval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
              if(!destval) {
                destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
                dapush(destval->equivs, genx(&combind));
                bigfinsertfr(ophash, (char*) genx(&combind), destval, sizeof(VALUESTRUCT));
              }
            } else {
              destval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0));
            bigfinsertfr(ophash, (char*) ctvalstruct(INIT_3, supersize(op->dest_type), 0, op->dest.regnum, 0), destval, sizeof(VALUESTRUCT));
          } else if(val1) {
            VALUESTRUCT combind = {op->opcode, supersize(val1->commontype), 0, val1->index, 0};
            otherval = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
            if(!otherval) {
              otherval = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
              dapush(otherval->equivs, genx(&combind));
              bigfinsertfr(ophash, (char*) genx(&combind), otherval, sizeof(VALUESTRUCT));
            }
          }
          break;
        case PHI:
          destval = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
          break;
        case DEALOC:
          {
            VALUESTRUCT combind = {op->opcode, 0, 0, op->addr0.ssaind, 0};
            val1 = bigsearch(ophash, (char*) &combind, sizeof(VALUESTRUCT));
            if(!val1) {
              val2 = ctgvnnum(eqcontainer, NOCONST, op->dest_type);
              dapush(val2->equivs, ctvalstruct(DEALOC, supersize(val1->commontype), 0, val1->index, 0));
              bigfinsertfr(ophash, (char*) genx(&combind), val2, sizeof(VALUESTRUCT));
            }
          }
          break;
        OPS_3_PTRDEST_3ac OPS_NODEST_3ac
          nodefromaddr(eqcontainer, op->addr1_type, op->addr1, prog);
          __attribute__((fallthrough));
        OPS_1_3ac 
          if(op->addr0_type & GARBAGEVAL) break;
          nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
          break;
        case CALL_3: //no pure functions for now
          destval = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
          break;
        OPS_1_ASSIGN_3ac
          destval = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
          break;
        case ASM:
          assert(0); //unimplemented
      }
      char status = 0;
      GVNNUM* chosenval;
      //if in any case chosenval is assigned null, it must be an addrsvar
      switch(op->opcode) {
        OPS_3_3ac_NOCOM OPS_3_3ac_COM 
          if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(op->addr0_type & ISCONST) {
              status++;
            } else {
              exst.p1 = op->addr0.regnum;
              exst.size1 = supersize(op->addr0_type);
              chosenval = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(VALUESTRUCT));
              if(chosenval) {
                status++;
                //slightly inefficient, could query and insert in same traversal
                //however it might be worse to have to re-free the exprstr if not used
                if(!fixedqueryval(blk->exp_gen, chosenval->index)) {
                  fixedinsert(blk->exp_gen, chosenval->index, genx(&exst));
                }
              }
            }
          }
          if(!(op->addr1_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(op->addr1_type & ISCONST) {
              status++;
            } else {
              exst.p1 = op->addr1.regnum;
              exst.size1 = supersize(op->addr1_type);
              chosenval = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(VALUESTRUCT));
              if(chosenval) {
                status++;
                if(!fixedqueryval(blk->exp_gen, chosenval->index)) {
                  fixedinsert(blk->exp_gen, chosenval->index, genx(&exst));
                }
              }
            }
          }
          if(status == 2) {
            GVNNUM* n0 = nodefromaddr(eqcontainer, op->addr0_type & ~ISVAR, op->addr0, prog);
            GVNNUM* n1 = nodefromaddr(eqcontainer, op->addr1_type & ~ISVAR, op->addr1, prog);

            if(!(n0 && n1)) break;
            VALUESTRUCT refex = {op->opcode, n0->index, n1->index};
            chosenval = bigsearch(eqcontainer->ophash, (char*) &refex, sizeof(VALUESTRUCT));
            //printop(op, 1, blk, stdout, prog);
            //printf("\n %s %d %d\n", opcode_3ac_names[refex.o], refex.p1, refex.p2);
            //assert(chosenval);
            if(chosenval && !fixedqueryval(blk->exp_gen, chosenval->index)) {
              fixedinsert(blk->exp_gen, chosenval->index, genx(&refex));
            }
          } else {
            if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
              dipush(blk->tmp_gen, op->dest.regnum);
            }
          }
          break;
        OPS_2_3ac_MUT case MOV_3: 
          if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(op->addr0_type & ISCONST) {
              status++;
            } else {
              exst.p1 = op->addr0.regnum;
              exst.size1 = supersize(op->addr0_type);
              chosenval = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(VALUESTRUCT));
              if(chosenval) {
                status++;
                if(!fixedqueryval(blk->exp_gen, chosenval->index)) {
                  fixedinsert(blk->exp_gen, chosenval->index, genx(&exst));
                }
              }
            }
          }
          if(status == 1) {
            GVNNUM* n0 = nodefromaddr(eqcontainer, op->addr0_type & ~ISVAR, op->addr0, prog);
            if(!n0) break;
            VALUESTRUCT refex = {op->opcode, n0->index, 0};
            chosenval = bigsearch(eqcontainer->ophash, (char*) &refex, sizeof(VALUESTRUCT));
            if(chosenval && !fixedqueryval(blk->exp_gen, chosenval->index)) {
              fixedinsert(blk->exp_gen, chosenval->index, genx(&refex));
            }
          } else {
            if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
              dipush(blk->tmp_gen, op->dest.regnum);
            }
          }
          break;
          __attribute__((fallthrough));
        case PHI: case CALL_3:
          //we always treat these as black boxes
          if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            dipush(blk->tmp_gen, op->dest.regnum);
          }
          break;
        case ADDR_3:
          //kill of ADDR_3 only caused by kill of value it's taking the address of
          break;
        OPS_1_ASSIGN_3ac
          assert(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)));
          dipush(blk->tmp_gen, op->addr0.regnum);
          break;
        OPS_NODEST_3ac OPS_3_PTRDEST_3ac
          if(!(op->addr1_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(!(op->addr0_type & ISCONST)) {
              exst.p1 = op->addr1.regnum;
              exst.size1 = supersize(op->addr1_type);
              chosenval = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(VALUESTRUCT));
              if(chosenval && !fixedqueryval(blk->exp_gen, chosenval->index)) {
                fixedinsert(blk->exp_gen, chosenval->index, genx(&exst));
              }
            }
          }
          __attribute__((fallthrough));
        OPS_1_3ac
          if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(!(op->addr0_type & ISCONST)) {
              exst.p1 = op->addr0.regnum;
              exst.size1 = supersize(op->addr0_type);
              chosenval = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(VALUESTRUCT));
              if(chosenval && !fixedqueryval(blk->exp_gen, chosenval->index)) {
                fixedinsert(blk->exp_gen, chosenval->index, genx(&exst));
              }
            }
          }
          break;
        OPS_NOVAR_3ac case DEALOC:
          break;
        case ASM:
          assert(0); //unimplemented
      }
      if(destval && !fixedqueryval(blk->leader, destval->index)) {
        VALUESTRUCT* exs = dapeek(destval->equivs);
        assert(exs->o == INIT_3 || exs->o == PHI || exs->o == CALL_3 || exs->o == INIT_3 || exs->o == PARAM_3);
        fixedinsert(blk->leader, destval->index, (void*) (long) exs->p1);
      }
    } while(op != blk->lastop && (op = op->nextop));
  }
  if(blk->idominates)
    for(int i = 0; i < blk->idominates->length; i++)
      gensall(prog, eqcontainer, daget(blk->idominates, i));
}
//translate an expression across a phi, translation table pre-populated
static VALUESTRUCT* translate(PROGRAM* prog, EQONTAINER* eq, BBLOCK* blk, BBLOCK* blkn, VALUESTRUCT* prevex) {
  int translated;
  GVNNUM* val1;
  GVNNUM* val2;
  GVNNUM* pval1;
  GVNNUM* pval2;
  ADDRESS a1, a2;
  if(!blk->translator) return NULL;
  switch(prevex->o) {
    OPS_NOVAR_3ac OPS_3_PTRDEST_3ac case MOV_3:
    OPS_NODEST_3ac OPS_1_3ac case CALL_3: case PHI:
    case ASM:
      assert(0);
    case DEALOC:
      return NULL;
    OPS_3_3ac
      if((translated = (long) fixedsearch(blk->translator, prevex->p1))) {
        a1.regnum = prevex->p1;
        pval1 = nodefromaddr(eq, 0, a1, prog);

        a1.regnum = translated;
        val1 = nodefromaddr(eq, pval1->commontype, a1, prog);
      } else {
        a1.regnum = prevex->p1;
        val1 = nodefromaddr(eq, 0, a1, prog);
      }
      if((translated = (long) fixedsearch(blk->translator, prevex->p2))) {
        a2.regnum = prevex->p2;
        pval2 = nodefromaddr(eq, 0, a2, prog);

        a2.regnum = translated;
        val2 = nodefromaddr(eq, pval2->commontype, a2, prog);
      } else {
        a2.regnum = prevex->p2;
        val2 = nodefromaddr(eq, 0, a2, prog);
      }
      if(!(val1 && val2)) return NULL;
      return ctvalstruct(prevex->o, supersize(val1->commontype), supersize(val2->commontype), val1->index, val2->index);
    OPS_2_3ac_MUT
      if((translated = (long) fixedsearch(blk->translator, prevex->p1))) {
        a1.regnum = prevex->p1;
        pval1 = nodefromaddr(eq, 0, a1, prog);

        a1.regnum = translated;
        val1 = nodefromaddr(eq, pval1->commontype, a1, prog);
      } else {
        a1.regnum = prevex->p1;
        val1 = nodefromaddr(eq, 0, a1, prog);
      }
      if(!val1) return NULL;
      return ctvalstruct(prevex->o, supersize(val1->commontype), 0, val1->index, 0);
    OPS_1_ASSIGN_3ac case ADDR_3:
      if((translated = (long) fixedsearch(blk->translator, prevex->p1))) {
        a1.regnum = prevex->p1;
        pval1 = nodefromaddr(eq, 0, a1, prog);

        a1.regnum = translated;
        val1 = nodefromaddr(eq, pval1->commontype, a1, prog);
      } else {
        a1.regnum = prevex->p1;
        val1 = nodefromaddr(eq, 0, a1, prog);
      }
      if(!val1) return NULL;
      return ctvalstruct(INIT_3, supersize(val1->commontype), 0, va1.regnum, 0);
  }
  return NULL;
}
//populate the anticipability of values coming into and going out of a block for GVNPRE
static char antics(BBLOCK* blk, PROGRAM* prog, EQONTAINER* eq) {
  if(blk->nextblock && !blk->branchblock) {
    int index = 0;
    BBLOCK* blkn = blk->nextblock;
    for(; daget(blkn->inedges,index) != blk; index++) ;
    if(!blk->translator) { //translators will be properly populated the first time
      if(blkn->lastop && blkn->firstop->opcode == PHI) {
        OPERATION* op = blkn->firstop;
        blk->translator = htctor(); //would be more efficient to keep dag of references?
        blk->revtranslator = htctor(); //would be more efficient to keep dag of references?
        while(op->opcode == PHI) {
          int prephi = op->addr0.joins[index].addr.regnum;
          int postphi = op->dest.regnum;
          fixedinsertint(blk->translator, prephi, postphi);
          fixedinsertint(blk->revtranslator, postphi, prephi);
          if(op == blkn->lastop) break;
          op = op->nextop;
        }
      }
    }
  }
  if(!blk->pidominates) {
    return 0;
  }

  HASHTABLE* oldanticin = blk->antileader_in;
  HASHTABLE* oldanticout = blk->antileader_out;

  if(blk->nextblock && !blk->branchblock) {
    int index = 0;
    BBLOCK* blkn = blk->nextblock;
    for(; daget(blkn->inedges,index) != blk; index++) ;
    if(blkn->lastop && blkn->firstop->opcode == PHI) {
      blk->antileader_out = htctor();
      if(!blk->translator) { //translators will be properly populated the first time
        DYNARR* pairs2trans = htfpairs(blkn->antileader_in);
        for(int i = 0; i < pairs2trans->length; i++) {
          HASHPAIR* hp = daget(pairs2trans, i);
          VALUESTRUCT* translated = translate(prog, eq, blk, blkn, hp->value);
          int valnum;
          if(translated) {
            if(translated->o != INIT_3) {
              GVNNUM* destval = bigsearch(eq->ophash, (char*) translated, sizeof(VALUESTRUCT));
              if(!destval) {
                destval = ctgvnnum(eq, NOCONST, ((GVNNUM*) hp->value)->commontype);
                bigfinsertfr(eq->ophash, (char*) genx(translated), destval, sizeof(VALUESTRUCT));
                dapush(destval->equivs, genx(translated));
              }
              valnum = destval->index;
            } else {
              valnum = translated->p1;
            }
            void* storage;
            if((storage = fixedsearch(blk->antileader_out, valnum))) free(storage);//inefficient
            fixedinsert(blk->antileader_out, valnum, translated);
          }
        }
        dadtor(pairs2trans);
      }
    } else {
      blk->antileader_out = fhtcclone(blkn->antileader_in, (void*(*)(void*)) genx);
    }
  } else if(blk->branchblock) {
    //the first block isn't populated here if next block has phi
    if(blk->nextblock->antileader_in) {
      blk->antileader_out = fhtcclone(blk->nextblock->antileader_in, (void*(*)(void*)) genx);
      if(blk->branchblock->antileader_in) {
        DYNARR* otharr = htfpairs(blk->antileader_out);
        DYNINT* dinctarr = dinctor(otharr->length);
        for(int i = 0; i < otharr->length; i++) {
          HASHPAIR* hp = daget(otharr, i);
          dinctarr->arr[i] = hp->fixedkey;
        }

        for(int i = 0; i < otharr->length; i++) {
          int key = dinctarr->arr[i];
          void* val;
          if((val = fixedsearch(blk->branchblock->antileader_in, key))) {
            free(val);
            frmpair(blk->antileader_out, key);
          }
        }
        dadtor(otharr);
        didtor(dinctarr);
      }
    } else if(blk->branchblock->antileader_in) {
      blk->antileader_out = fhtcclone(blk->branchblock->antileader_in, (void*(*)(void*)) genx);
    } else {
      blk->antileader_out = htctor();
    }
    //and with branchblock
    //>1 succ
  } else {
    blk->antileader_out = htctor();
  }
  HASHTABLE* antiin_users = htctor(); //ht of dynarrs of expressions which would be killed by a kill of the value number
  blk->antileader_in = fhtcclone(blk->antileader_out, (void*(*)(void*)) genx);
  if(blk->exp_gen) {
    DYNARR* expairs = htfpairs(blk->exp_gen);
    for(int i = 0; i < expairs->length; i++) {
      HASHPAIR* hp = expairs->arr[i];
      VALUESTRUCT* exs = hp->value;
      ADDRESS a;
      GVNNUM* n3;
      switch((int) exs->o) {
        OPS_NOVAR_3ac OPS_3_PTRDEST_3ac case MOV_3:
        OPS_NODEST_3ac OPS_1_3ac case CALL_3: case PHI:
        case ASM: case CONST_3:
          assert(0);
        case DEALOC:
          continue;
        OPS_3_3ac
          n3 = bigsearch(eq->ophash, (char*) exs, sizeof(VALUESTRUCT));
          if(!fixedqueryval(blk->antileader_in, n3->index)) {
            fixedinsert(blk->antileader_in, n3->index, genx(exs));

            if(!fixedqueryval(antiin_users, exs->p1)) {
              fixedinsert(antiin_users, exs->p1, dinctor(4));
            }
            dipush(fixedsearch(antiin_users, exs->p1), n3->index);
            if(!fixedqueryval(antiin_users, exs->p2)) {
              fixedinsert(antiin_users, exs->p2, dinctor(4));
            }
            dipush(fixedsearch(antiin_users, exs->p2), n3->index);
          }
          break;
        OPS_2_3ac_MUT
          n3 = bigsearch(eq->ophash, (char*) exs, sizeof(VALUESTRUCT));
          if(!fixedqueryval(blk->antileader_in, n3->index)) {
            fixedinsert(blk->antileader_in, n3->index, genx(exs));
            if(!fixedqueryval(antiin_users, exs->p1)) {
              fixedinsert(antiin_users, exs->p1, dinctor(4));
            }
            dipush(fixedsearch(antiin_users, exs->p1), n3->index);
          }
          break;
        OPS_1_ASSIGN_3ac case ADDR_3:
            a.regnum = exs->p1;
            n3 = nodefromaddr(eq, 0, a, prog);
            if(!fixedqueryval(blk->antileader_in, n3->index))
              fixedinsert(blk->antileader_in, n3->index, genx(exs));
            break;
      }
    }
    dadtor(expairs);
  }
  if(blk->tmp_gen) {
    DYNINT* rmstack = dinctor(8);
    for(int i = 0; i < blk->tmp_gen->length; i++) {
      ADDRESS a;
      a.regnum = blk->tmp_gen->arr[i];
      GVNNUM* g = nodefromaddr(eq, 0, a, prog);

      if(fixedqueryval(blk->leader, g->index)) {
        long ex2rm = (long) fixedsearch(blk->leader, g->index);
        if(a.regnum == ex2rm) {
          dipush(rmstack, g->index);
          while(rmstack->length > 0) {
            int removalind = dipop(rmstack);
            void* fr = frmpair(blk->antileader_in, removalind);
            if(fr) {
              free(fr);
              DYNINT* tokill = frmpair(antiin_users, removalind);
              if(tokill) rmstack = dimerge(rmstack, tokill);
            }
          }
        }
      }
    }
    didtor(rmstack);
  }
  char changed = !(fhtequal(blk->antileader_out, oldanticout) && fhtequal(blk->antileader_in, oldanticin));
  if(oldanticin) fhtdtorcfr(oldanticin, free);
  if(oldanticout) fhtdtorcfr(oldanticout, free);
  fhtdtorcfr(antiin_users, (void(*)(void*)) didtor);
  for(int i = 0; i < blk->pidominates->length; i++)
    changed |= antics(daget(blk->pidominates, i), prog, eq);
  return changed;
}

static void printffht(DYNARR* da) {
  for(int i = 0; i < da->length; i++) {
    HASHPAIR* hp = daget(da, i);
    printf("{%ld: %ld}, ", hp->fixedkey, hp->ivalue);
  }
}

static void printfht(DYNARR* da) {
  for(int i = 0; i < da->length; i++) {
    HASHPAIR* hp = daget(da, i);
    VALUESTRUCT* ex = hp->value;
    printf("{%ld: [%s %d %d]}, ", hp->fixedkey, opcode_3ac_names[ex->o], ex->p1, ex->p2);
  }
}

static void printeq(EQONTAINER* eq, PROGRAM* prog) {
  puts("-------------------------------------------");
  for(int i = 1; i < eq->nodes->length; i++) {
    printf("node %d: ", i);
    GVNNUM* eqnode = daget(eq->nodes, i);
    if(eqnode->hasconst == INTCONST) printf("intconst %ld, ", eqnode->intconst);
    else if(eqnode->hasconst == FLOATCONST) printf("floatconst %lf, ", eqnode->floatconst);
    else if(eqnode->hasconst == STRCONST) printf("strconst \"%s\", ", eqnode->strconst);
    for(int j = 0; j < eqnode->equivs->length; j++) {
      VALUESTRUCT* exs = daget(eqnode->equivs, j);
      if(exs->o != INIT_3) {
        printf("[%s %d %d], ", opcode_3ac_names[exs->o], exs->p1, exs->p2);
      } else {
        printf("%d, ", exs->p1);
      }
    } 
    printf("\n");
  }
  puts("-------------------------------------------");
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    printf("BLOCK %d:\n", blk->domind);
    if(blk->exp_gen) {
      DYNARR* av = htfpairs(blk->exp_gen);
      printf("exp_gen: (");
      printfht(av);
      printf(") \n");
      dadtor(av);
    }
    if(blk->leader) {
      DYNARR* av = htfpairs(blk->leader);
      printf("leader: (");
      printffht(av);
      printf(") \n");
      dadtor(av);
    }
    if(blk->antileader_in) {
      DYNARR* av = htfpairs(blk->antileader_in);
      printf("antileader in: (");
      printfht(av);
      printf(") \n");
      dadtor(av);
    }
    if(blk->antileader_out) {
      DYNARR* av = htfpairs(blk->antileader_out);
      printf("antileader out: (");
      printfht(av);
      printf(") \n");
      dadtor(av);
    }
    if(blk->translator) {
      DYNARR* av = htfpairs(blk->translator);
      printf("translator (");
      printffht(av);
      printf(") \n");
      dadtor(av);
    }
  }
  puts("-------------------------------------------");
}

static void recdomins(BBLOCK* blk, long key, void* value) {
  fixedinsert(blk->leader, key, value);
  if(blk->idominates) {
    for(int i = 0; i < blk->idominates->length; i++) {
      recdomins((BBLOCK*) daget(blk->idominates, i), key, value);
    }
  }
}

//Hoist expressions to their earliest available program point, part of GVNPRE
static char hoist(PROGRAM* prog, EQONTAINER* eq) {
  char changed = 0;
  DYNARR* stubbornblocks = dactor(8);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->inedges->length > 1) {
      DYNARR* antilead = htfpairs(blk->antileader_in);
      for(int antiind = 0; antiind < antilead->length; antiind++) {
        HASHPAIR* antipair = daget(antilead, antiind);
        GVNNUM* antilnode = daget(eq->nodes, antipair->fixedkey);
        VALUESTRUCT* antil = antipair->value;
        if(antil->o == INIT_3) continue;
        for(int j = 0; j < blk->inedges->length; j++) {
            BBLOCK* oblk = daget(blk->inedges, j);
            if(oblk->leader && fixedqueryval(oblk->leader, antipair->fixedkey)) {
                dapush(stubbornblocks, oblk);
            }
        }
        if(stubbornblocks->length > 0 && stubbornblocks->length < blk->inedges->length) {
          int stubbornindex = 0;
          ADDRESS joins, reggie;
          joins.joins = malloc(blk->inedges->length * sizeof(FULLADDR));
          reggie.regnum = prog->regcnt++;
          OPERATION* phi = ct_3ac_op2(PHI, ISCONST, joins, antilnode->commontype, reggie);
          for(int j = 0; j < blk->inedges->length; j++) {
            BBLOCK* oblk = daget(blk->inedges, j);
            if(oblk == daget(stubbornblocks, stubbornindex)) {
              stubbornindex++;
              int stubbornval = (int) (long) fixedsearch(oblk->leader, antilnode->index);
              FULLADDR join;
              join.addr_type = antilnode->commontype;
              join.addr.regnum = stubbornval;
              joins.joins[j] = join;
            } else {
              VALUESTRUCT actionable = *antil;
              ADDRESS provisional;
              OPERATION* genop = malloc(sizeof(OPERATION));
              int leadreg;
              GVNNUM* operandnode;
              genop->opcode = actionable.o;
              genop->dest_type = antilnode->commontype;
              genop->dest.regnum = prog->regcnt++;
              switch(actionable.o) {
                default:
                  assert(0);
                case INIT_3:
                  free(genop);
                  joins.joins[j].addr_type = antilnode->commontype;
                  joins.joins[j].addr.regnum = actionable.p1;
                  continue;
                OPS_3_3ac
                  operandnode = daget(eq->nodes, actionable.p2);
                  if(operandnode->hasconst != NOCONST) {
                    genop->addr1.intconst_64 = operandnode->intconst;
                    if(operandnode->hasconst == INTCONST) {
                      genop->addr1_type = genop->dest_type | ISCONST;
                    } else if(operandnode->hasconst == FLOATCONST) {
                      genop->addr1_type = genop->dest_type | ISCONST | ISFLOAT; //float should be assumed
                    } else if(operandnode->hasconst == STRCONST) {
                      genop->addr1_type = genop->dest_type | ISSTRCONST | ISCONST;
                    } else {
                      assert(0);
                    }
                  } else {
                    int prevleader = (long) fixedsearch(blk->leader, actionable.p2);
                    provisional.regnum = oblk->revtranslator ? (long) fixedsearch(oblk->revtranslator, prevleader) : 0;
                    if(provisional.regnum) actionable.p2 = nodefromaddr(eq, operandnode->commontype, provisional, prog)->index;
                    int equivind = 0;
                    DYNARR* equivlist = ((GVNNUM*) daget(eq->nodes, actionable.p2))->equivs;
                    while(!(leadreg = (long) fixedsearch(oblk->leader, actionable.p2))) {
                      VALUESTRUCT* vs;
                      do {
                        assert(equivind < equivlist->length);
                        vs = daget(equivlist, equivind++);
                      } while(vs->o != INIT_3);
                      provisional.regnum = oblk->revtranslator ? (long) fixedsearch(oblk->revtranslator, vs->p1) : 0;
                      if(provisional.regnum) actionable.p2 = nodefromaddr(eq, operandnode->commontype, provisional, prog)->index;
                    }
                    genop->addr1_type = genop->dest_type;
                    genop->addr1.regnum = leadreg;
                  }
                __attribute__((fallthrough));
                OPS_2_3ac
                  operandnode = daget(eq->nodes, actionable.p1);
                  if(operandnode->hasconst != NOCONST) {
                    genop->addr0.intconst_64 = operandnode->intconst;
                    if(operandnode->hasconst == INTCONST) {
                      genop->addr0_type = genop->dest_type | ISCONST;
                    } else if(operandnode->hasconst == FLOATCONST) {
                      genop->addr0_type = genop->dest_type | ISCONST | ISFLOAT; //float should be assumed
                    } else if(operandnode->hasconst == STRCONST) {
                      genop->addr0_type = genop->dest_type | ISSTRCONST | ISCONST;
                    } else {
                      assert(0);
                    }
                  } else {
                    int prevleader = (long) fixedsearch(blk->leader, actionable.p1);
                    provisional.regnum = oblk->revtranslator ? (long) fixedsearch(oblk->revtranslator, prevleader) : 0;
                    if(provisional.regnum) actionable.p1 = nodefromaddr(eq, operandnode->commontype, provisional, prog)->index;
                    int equivind = 0;
                    DYNARR* equivlist = ((GVNNUM*) daget(eq->nodes, actionable.p1))->equivs;
                    while(!(leadreg = (long) fixedsearch(oblk->leader, actionable.p1))) {
                      VALUESTRUCT* vs;
                      do {
                        assert(equivind < equivlist->length);
                        vs = daget(equivlist, equivind++);
                      } while(vs->o != INIT_3);
                      provisional.regnum = oblk->revtranslator ? (long) fixedsearch(oblk->revtranslator, vs->p1) : 0;
                      if(provisional.regnum) actionable.p1 = nodefromaddr(eq, operandnode->commontype, provisional, prog)->index;
                    }
                    genop->addr0_type = genop->dest_type;
                    genop->addr0.regnum = leadreg;
                  }
                  break;
              }

              if(oblk->lastop) {
                oblk->lastop = oblk->lastop->nextop = genop;
              } else {
                oblk->firstop = oblk->lastop = genop;
              }

              GVNNUM* destlead = nodefromaddr(eq, genop->dest_type, genop->dest, prog);
              if(destlead->hasconst == NOCONST) {
                assert(!fixedqueryval(oblk->leader, destlead->index));

                recdomins(oblk, destlead->index, (void*) (long) genop->dest.regnum);
                if(!oblk->translator) oblk->translator = htctor();
                if(!oblk->revtranslator) oblk->revtranslator = htctor();
                fixedinsert(oblk->revtranslator, phi->dest.regnum, (void*) (long) genop->dest.regnum);
                fixedinsert(oblk->translator, genop->dest.regnum, (void*) (long) phi->dest.regnum);
              }

              //insert calculation of value here in predecessor block
              FULLADDR join = {genop->dest_type, genop->dest};
              joins.joins[j] = join;
            }
          }
          //insert phi at top of block
          if(blk->lastop) {
            phi->nextop = blk->firstop;
            blk->firstop = phi;
          } else {
            blk->firstop = blk->lastop = phi;
          }
          changed = 1;

          recdomins(blk, antipair->fixedkey, (void*) (long) phi->dest.regnum);

          bigfinsertfr(eq->ophash, (char*) ctvalstruct(INIT_3, supersize(phi->dest_type), phi->dest.regnum, 0), antilnode, sizeof(VALUESTRUCT));
          dapush(antilnode->equivs, ctvalstruct(INIT_3, supersize(phi->dest_type), 0, phi->dest.regnum, 0));

          void* prevval = frmpair(blk->antileader_in, antipair->fixedkey);
          if(prevval) free(prevval);
        }

        stubbornblocks->length = 0;
      }
      dadtor(antilead);
    }
  }
  dadtor(stubbornblocks);
  return changed;
}

//Run the GVNPRE algorithm on the code to eliminate recalculations of value and 
//partial redundancies (as well as factor out loop invariants)
void gvn(PROGRAM* prog) {
  BBLOCK* first = daget(prog->allblocks, 0);
  EQONTAINER* eqcontainer = cteq(prog);
  HASHTABLE* h1 = first->leader = htctor();
  gensall(prog, eqcontainer, first);
  free(h1);
  first->pidominates = dactor(0);
  while(antics(prog->finalblock, prog, eqcontainer)) ;
  //buildsets calculated
  while(hoist(prog, eqcontainer)) ;
  replacegvn(eqcontainer, prog);
  freeq(eqcontainer);
  prog->pdone |= GVN;
  return;
}
//https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/gvn_sas04.pdf
//https://www.cs.purdue.edu/homes/hosking/papers/cc04.pdf

//Convert code out of SSA form (currently we don't care to do so, and will register allocate right from SSA)
void ssaout(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      OPERATION* phiop = blk->firstop;
      while(phiop->opcode == PHI) {
        FULLADDR paraddr;
        paraddr.addr_type = phiop->dest_type & GENREGMASK;
        paraddr.addr = phiop->dest;
        paraddr.addr.ssaind++;
        for(int j = 0; j < blk->inedges->length; j++) {
           BBLOCK* predblock = daget(blk->inedges, j);
           FULLADDR fadradr = phiop->addr0.joins[j];
           if(predblock->lastop) {
             predblock->lastop = predblock->lastop->nextop = ct_3ac_op2(MOV_3, fadradr.addr_type, fadradr.addr, paraddr.addr_type, paraddr.addr);
           } else {
             predblock->firstop = predblock->lastop = ct_3ac_op2(MOV_3, fadradr.addr_type, fadradr.addr, paraddr.addr_type, paraddr.addr);
           }
        }
        free(phiop->addr0.joins);
        phiop->opcode = MOV_3;
        phiop->addr0_type = paraddr.addr_type;
        phiop->addr0 = paraddr.addr;
        if(phiop == blk->lastop) break;
        phiop = phiop->nextop;
      }
    }
  }
}

//Annotate the last use of a regnum in a block with the LASTUSE flag, this will allow us to figure out kills
void annotateuse(PROGRAM* prog) {
  DYNARR* pda = dactor(64);
  HASHTABLE* pht = htctor();
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(!blk->lastop) continue;
    OPERATION* op = blk->firstop;
    while(1) {
      //a deref is still a use
      switch(op->opcode) {
        OPS_3_3ac OPS_3_PTRDEST_3ac
          if(!(op->addr0_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr0.regnum, &op->addr0_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.regnum);
          }
          if(!(op->addr1_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr1.regnum, &op->addr1_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr1.regnum);
          }
          if(!(op->dest_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->dest.regnum, &op->dest_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->dest.regnum);
          }
          break;
        OPS_NODEST_3ac
          if(!(op->addr1_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr1.regnum, &op->addr1_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr1.regnum);
          }
          __attribute__((fallthrough));
        OPS_1_3ac OPS_1_ASSIGN_3ac case DEALOC:
          if(!(op->addr0_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr0.regnum, &op->addr0_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.regnum);
          }
          break;
        OPS_2_3ac case ADDR_3:
          if(!(op->addr0_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr0.regnum, &op->addr0_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.regnum);
          }
          __attribute__((fallthrough));
        case CALL_3:
          if(!(op->dest_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->dest.regnum, &op->dest_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->dest.regnum);
          }
          break;
        case PHI:
          for(int j = 0; j < blk->inedges->length; j++) {
            if(!(op->addr0.joins[j].addr_type & (ISCONST | ISLABEL))) {
              int oldk = pht->keys;
              fixedinsert(pht, op->addr0.joins[j].addr.regnum, &op->addr0.joins[j].addr_type);
              if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.joins[j].addr.regnum);
            }
          }
          if(!(op->dest_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->dest.regnum, &op->dest_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->dest.regnum);
          }
          break;
        OPS_NOVAR_3ac case ASM:
          break;
      }
      if(op == blk->lastop) break;
      op = op->nextop;
    }
    for(int j = 0; j < pda->length; j++) {
      unsigned int val = (unsigned long) pda->arr[j];
      ADDRTYPE* l = (ADDRTYPE*) fixedsearch(pht, val);
      *l |= LASTUSE;
      frmpair(pht, val);
    }
    pda->length = 0;
  }
  dadtor(pda);
  fhtdtor(pht);
}

#undef X
