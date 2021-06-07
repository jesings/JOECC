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
        if(flag && kb != root && kb->dom != root) {
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
        case CALL_3: case PHI: /*must have constant input in alloc_3*/
          if((op->dest_type & (ADDRSVAR | ISVAR)) == ISVAR) {
            if(op->dest_type & ISDEREF) {
              op->dest.ssaind =  (long) dapeek((DYNARR*) daget(S, op->dest.varnum));
            } else {
              C[op->dest.varnum] = prog->regcnt++;
              op->dest.ssaind = C[op->dest.varnum];
              dapush((DYNARR*) daget(S, op->dest.varnum), (void*)(long) C[op->dest.varnum]);
              dapush(assigns, (void*)(long) op->dest.varnum);
            }
          }
          break;
        OPS_NODEST_3ac
          if((op->addr1_type & (ADDRSVAR | ISVAR)) == ISVAR) 
            op->addr1.ssaind =  (long) dapeek((DYNARR*) daget(S, op->addr1.varnum));
          __attribute__((fallthrough));
        OPS_1_3ac
          if(op->addr0_type & GARBAGEVAL) break;
          __attribute__((fallthrough));
        case DEALOC:
          if((op->addr0_type & (ADDRSVAR | ISVAR)) == ISVAR)
            op->addr0.ssaind =  (long) dapeek((DYNARR*) daget(S, op->addr0.varnum));
          break;
        OPS_1_ASSIGN_3ac
          if((op->addr0_type & (ADDRSVAR | ISVAR)) == ISVAR) {
            assert(!(op->addr0_type & ISDEREF));
            assert(!C[op->addr0.varnum]);
            C[op->addr0.varnum] = prog->regcnt++;
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
      if(!(op->addr0_type & GARBAGEVAL)) {
        op->addr0.joins[i].addr.ssaind = (int) (long) dapeek((DYNARR*) daget(S, op->dest.varnum));
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
        //check if it has a back-edge
        //if so, add an inedge in finalblock
        //then, recalculate postdom tree
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

static GVNNUM* ctgvnnum(EQONTAINER* eqcontainer, int hc) {
  GVNNUM* retval = malloc(sizeof(GVNNUM));
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

//find which equivalence node, if any, this address corresponds to, and create one if it corresponds to nothing extant
static GVNNUM* nodefromaddr(EQONTAINER* eqcontainer, ADDRTYPE adt, ADDRESS adr, PROGRAM* prog) {
  GVNNUM* cn;
  if(adt & ISCONST) {
    if(adt & ISSTRCONST) {
      cn = search(eqcontainer->strconsthash, adr.strconst);
      if(!cn) {
        cn = ctgvnnum(eqcontainer, STRCONST);
        cn->strconst = adr.strconst;
        insert(eqcontainer->strconsthash, adr.strconst, cn);
      }

    } else {
      char floatness = adt & ISFLOAT;
      cn = fixedsearch(floatness ? eqcontainer->floatconsthash : eqcontainer->intconsthash, adr.intconst_64);
      if(!cn) {
        cn = ctgvnnum(eqcontainer, floatness ? FLOATCONST : INTCONST);
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
    EXPRSTR* exst = ex2string(adr.iregnum, 0, INIT_3);
    cn = bigsearch(eqcontainer->ophash, (char*) exst, sizeof(EXPRSTR));
    if(!cn) {
      cn = ctgvnnum(eqcontainer, NOCONST);
      EXPRSTR* exn = malloc(sizeof(EXPRSTR));
      memcpy(exn, exst, sizeof(EXPRSTR));
      bigfinsertfr(eqcontainer->ophash, (char*) exn, cn, sizeof(EXPRSTR));
      dapush(cn->equivs, exst);
    } else {
      free(exst);
    }
  }
  return cn;
}

//replace operation via gvn
static void replaceop(BBLOCK* blk, EQONTAINER* eq, PROGRAM* prog, OPERATION* op) {
  BITFIELD bf = bfalloc(8);//blk->availability_out; stupid placeholder compiler doesn't yell at me
  GVNNUM* sen;
  switch(op->opcode) {
    OPS_3_3ac_NOCOM OPS_3_3ac_COM
      sen = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
          break;
        } else {
          bfset(bf, sen->index);
          sen->regno = op->dest.iregnum;
          op->dest.gvnind = sen->index;
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
          assert(bfget(bf, sen->index));
          op->addr1.iregnum = sen->regno;
          op->addr1.gvnind = sen->index;
        }
      }
      __attribute__((fallthrough));
    OPS_1_3ac
      if(op->addr0_type & GARBAGEVAL) break;
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
          op->addr0.gvnind = sen->index;
        }
      }
      break;
    case DEALOC:
      {
      EXPRSTR ex2 = {op->opcode, op->addr0.ssaind, 0};
      sen = bigsearch(eq->ophash, (char*) &ex2, sizeof(EXPRSTR));
      if(sen) {
        if(bfget(bf, sen->index)) {
          op->opcode = NOP_3;
        } else {
          bfset(bf, sen->index);
        }
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
          op->dest.gvnind = sen->index;
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
          op->addr0.gvnind = sen->index;
        }
      }
      break;
    case CALL_3:
      sen = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
          fprintf(stderr, "Somehow function output is part of an equivalence class\n");
          assert(0);
        } else {
          bfset(bf, sen->index);
          sen->regno = op->dest.iregnum;
          op->dest.gvnind = sen->index;
        }
      }
      break;
    case PHI:  ;
      FULLADDR* addrs = op->addr0.joins;
      void* sen_tinel = NULL;
      EXPRSTR est = {INIT_3, 0, 0};
      for(int k = 0; k < blk->inedges->length; k++) {
        est.p1 = k;
        sen = bigsearch(eq->ophash, (char*) &est, sizeof(EXPRSTR));
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
              addrs[k].addr.gvnind = sen->index;
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
      op->dest.gvnind = sen->index;
      break;
    OPS_1_ASSIGN_3ac
      sen = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(sen) {
        if(sen->hasconst != NOCONST || bfget(bf, sen->index)) {
          op->opcode = NOP_3;
        } else {
          bfset(bf, sen->index);
          sen->regno = op->addr0.iregnum;
          op->addr0.gvnind = sen->index;
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
      replacenode(daget(blk->idominates, i), eq, prog);
    }
  }
}

static void gensall(PROGRAM* prog, EQONTAINER* eqcontainer, BBLOCK* blk) {
  if(blk->visited) return;
  blk->visited = 1;
  if(!blk->lastop) return;
  //blk->phi_gen = dinctor(8); //find length of phis maybe
  blk->tmp_gen = dinctor(32); //not sure what to do with this
  blk->exp_gen = htctor();
  blk->leader = fhtclone(blk->dom->leader);
  blk->antileader = fhtclone(blk->dom->antileader);
  OPERATION* op = blk->firstop;
  EXPRSTR exst = {INIT_3, 0, 0};
  do {
    GVNNUM* sen1;
    GVNNUM* sen2;
    GVNNUM* destsen = NULL;
    BIGHASHTABLE* ophash = eqcontainer->ophash;
    EXPRSTR* combind;
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
            combind = ex2string(sen1->index, sen2->index, op->opcode);
            destsen = bigsearch(ophash, (char*) combind, sizeof(EXPRSTR));
            if(!destsen) {
              destsen = ctgvnnum(eqcontainer, NOCONST);
              dapush(destsen->equivs, ex2string(sen1->index, sen2->index, op->opcode));
              bigfinsertfr(ophash, (char*) combind, destsen, sizeof(EXPRSTR));
            } else {
              free(combind);
            }
          } else {
            destsen = ctgvnnum(eqcontainer, NOCONST);
          }
          dapush(destsen->equivs, ex2string(op->dest.iregnum, 0, INIT_3));
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
            combind = ex2string(sen1->index, sen2->index, op->opcode);
            destsen = bigsearch(ophash, (char*) combind, sizeof(EXPRSTR));
            if(!destsen) {
              destsen = ctgvnnum(eqcontainer, NOCONST);
              dapush(destsen->equivs, ex2string(sen1->index, sen2->index, op->opcode));
              bigfinsertfr(ophash, (char*) combind, destsen, sizeof(EXPRSTR));
              EXPRSTR* combind2 = ex2string(sen2->index, sen1->index, op->opcode);
              bigfinsertfr(ophash, (char*) combind2, destsen, sizeof(EXPRSTR));
            } else {
              free(combind);
            }
          } else {
            destsen = ctgvnnum(eqcontainer, NOCONST);
          }
          dapush(destsen->equivs, ex2string(op->dest.iregnum, 0, INIT_3));
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
            combind = ex2string(sen1->index, 0, op->opcode);
            destsen = bigsearch(ophash, (char*) combind, sizeof(EXPRSTR));
            if(!destsen) {
              destsen = ctgvnnum(eqcontainer, NOCONST);
              dapush(destsen->equivs, ex2string(sen1->index, 0, op->opcode));
              bigfinsertfr(ophash, (char*) combind, destsen, sizeof(EXPRSTR));
            } else {
              free(combind);
            }
          } else {
            destsen = ctgvnnum(eqcontainer, NOCONST);
          }
          dapush(destsen->equivs, ex2string(op->dest.iregnum, 0, INIT_3));
        }
        break;
      case MOV_3:
        sen1 = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
        if(!(op->dest_type & (ISLABEL | ISDEREF))) {
          if(op->dest_type & ISVAR) {
            FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
            if(adstore->addr_type & ADDRSVAR) break;
          }
          destsen = sen1 ? sen1 : ctgvnnum(eqcontainer, NOCONST);
          dapush(destsen->equivs, ex2string(op->dest.iregnum, 0, INIT_3));
        }
        break;
      case ADDR_3:
        //address should stay constant, so the value can be stored, as can the value of labels!
        sen1 = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
        if(!(op->dest_type & (ISDEREF | ISLABEL))) {
          //addrsvar is permissible
          if(sen1) {
            combind = ex2string(sen1->index, 0, op->opcode);
            destsen = bigsearch(ophash, (char*) combind, sizeof(EXPRSTR));
            if(!destsen) {
              destsen = ctgvnnum(eqcontainer, NOCONST);
              dapush(destsen->equivs, ex2string(sen1->index, 0, op->opcode));
              bigfinsertfr(ophash, (char*) combind, destsen, sizeof(EXPRSTR));
            } else {
              free(combind);
            }
          } else {
            destsen = ctgvnnum(eqcontainer, NOCONST);
          }
          dapush(destsen->equivs, ex2string(op->dest.iregnum, 0, INIT_3));
        }
        break;
      case PHI:
        destsen = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
        break;
      case DEALOC:
        combind = ex2string(op->addr0.ssaind, 0, op->opcode);
        sen1 = bigsearch(ophash, (char*) combind, sizeof(EXPRSTR));
        if(!sen1) {
          sen2 = ctgvnnum(eqcontainer, NOCONST);
          dapush(sen2->equivs, ex2string(sen1->index, 0, op->opcode));
          bigfinsertfr(ophash, (char*) combind, sen2, sizeof(EXPRSTR));
        } else {
          free(combind);
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
        destsen = nodefromaddr(eqcontainer, op->dest_type, op->dest, prog);
        break;
      OPS_1_ASSIGN_3ac
        destsen = nodefromaddr(eqcontainer, op->addr0_type, op->addr0, prog);
        break;
      case ASM:
        assert(0); //unimplemented
    }
    char status = 0;
    GVNNUM* chosen;
    switch(op->opcode) {
      OPS_3_3ac_NOCOM OPS_3_3ac_COM 
        if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {
          exst.p1 = op->addr0.iregnum;
          chosen = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            //slightly inefficient, could query and insert in same traversal
            //however it might be worse to have to re-free the exprstr if not used
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &exst, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        if(!(op->addr1_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {
          exst.p1 = op->addr1.iregnum;
          chosen = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &exst, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)))
          dipush(blk->tmp_gen, op->dest.iregnum);
        if(status == 2) {
          EXPRSTR refex = {op->opcode, op->addr0.iregnum, op->addr1.iregnum};
          chosen = bigsearch(eqcontainer->ophash, (char*) &refex, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &refex, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        break;
      OPS_2_3ac_MUT case MOV_3: 
        if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {
          exst.p1 = op->addr0.iregnum;
          chosen = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &exst, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)))
          dipush(blk->tmp_gen, op->dest.iregnum);
        if(status == 1) {
          EXPRSTR refex = {op->opcode, op->addr0.iregnum, 0};
          chosen = bigsearch(eqcontainer->ophash, (char*) &refex, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &refex, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        break;
        __attribute__((fallthrough));
      case CALL_3: case ADDR_3:
        if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)))
          dipush(blk->tmp_gen, op->dest.iregnum);
        break;
      OPS_1_ASSIGN_3ac
        if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)))
          dipush(blk->tmp_gen, op->addr0.iregnum);
        break;
      case PHI:
        //for(int i = 0; i < blk->inedges->length; i++) {
        //  if(!(op->addr0.joins[i].addr_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {
        //    exst.p1 = op->addr0.joins[i].addr.iregnum;
        //    chosen = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(EXPRSTR));
        //    if(!fixedqueryval(blk->exp_gen, chosen->index)) {
        //      EXPRSTR* exc = malloc(sizeof(EXPRSTR));
        //      memcpy(exc, &exst, sizeof(EXPRSTR));
        //      fixedinsert(blk->exp_gen, chosen->index, exc);
        //    }
        //  }
        //}
        //if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)))
        //  dipush(blk->phi_gen, op->dest.iregnum); phi gen not realy necessary
        break;
      OPS_NODEST_3ac OPS_3_PTRDEST_3ac
        if(!(op->addr1_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {
          exst.p1 = op->addr1.iregnum;
          chosen = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &exst, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        __attribute__((fallthrough));
      OPS_1_3ac
        if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {
          exst.p1 = op->addr0.iregnum;
          chosen = bigsearch(eqcontainer->ophash, (char*) &exst, sizeof(EXPRSTR));
          if(!fixedqueryval(blk->exp_gen, chosen->index)) {
            EXPRSTR* exc = malloc(sizeof(EXPRSTR));
            memcpy(exc, &exst, sizeof(EXPRSTR));
            fixedinsert(blk->exp_gen, chosen->index, exc);
          }
        }
        break;
      OPS_NOVAR_3ac case DEALOC:
        break;
      case ASM:
        assert(0); //unimplemented
    }
    if(destsen && !fixedqueryval(blk->leader, destsen->index)) {
      EXPRSTR* exs = dapeek(destsen->equivs);
      //assert(exs->o == INIT_3 || exs->o == PHI || exs->o == CALL_3 || exs->o == INIT_3 || exs->o == PARAM_3);
      fixedinsert(blk->leader, destsen->index, (void*) (long) exs->p1);
    }
  } while(op != blk->lastop && (op = op->nextop));
  if(blk->idominates)
    for(int i = 0; i < blk->idominates->length; i++)
      gensall(prog, eqcontainer, daget(blk->idominates, i));
}
static void antics(BBLOCK* blk, PROGRAM* prog) {
  if(blk->visited) return;
  blk->visited = 1;

  blk->anticipability_out = bfalloc(prog->regcnt);
  memset(blk->anticipability_out, 1, (prog->regcnt + 7) >> 3);
  if(blk->nextblock && blk->nextblock->anticipability_in)
    for(unsigned int i = 0; i < (prog->regcnt + 7)  >> 3; i++) 
      blk->anticipability_out[i] &= blk->nextblock->anticipability_in[i];
  if(blk->branchblock && blk->branchblock->anticipability_in)
    for(unsigned int i = 0; i < (prog->regcnt + 7)  >> 3; i++)
      blk->anticipability_out[i] &= blk->branchblock->anticipability_in[i];

  blk->anticipability_in = bfclone(blk->anticipability_out, prog->regcnt);

  for(int i = 0; i < blk->inedges->length; i++) {
    antics(daget(blk->inedges, i), prog);
  }
}

void gvn(PROGRAM* prog) {
  BBLOCK* first = daget(prog->allblocks, 0);
  EQONTAINER* eqcontainer = cteq(prog);
  HASHTABLE* h1 = first->leader = htctor();
  HASHTABLE* h2 = first->antileader = htctor();
  gensall(prog, eqcontainer, first);
  free(h1); free(h2);
  freeq(eqcontainer);
  prog->pdone |= GVN;
  return;
  antics(prog->finalblock, prog);
  for(int i = 0; i < prog->allblocks->length; i++)
    ((BBLOCK*) prog->allblocks->arr[i])->visited = 0;
  prog->finalblock->visited = 0;
  int rplistind = prog->allblocks->length;
  BBLOCK** rplist = malloc(prog->allblocks->length * sizeof(BBLOCK*));
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    blk->visited = 0;
  } //recalculate to tighten length
  rplist[0] = first;
  rpdt(first->nextblock, rplist, &rplistind);
  rpdt(first->branchblock, rplist, &rplistind);

  //replacenode(first, eqcontainer, prog);
  //for(int i = 0; i < prog->allblocks->length; i++) {
  //  BBLOCK* blk = daget(prog->allblocks, i);
  //  blk->visited = 0;
  //} //recalculate to tighten length


  free(rplist);

  prog->regcnt = eqcontainer->nodes->length;
}
//https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/gvn_sas04.pdf
//https://www.cs.purdue.edu/homes/hosking/papers/cc04.pdf

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
            fixedinsert(pht, op->addr0.iregnum, &op->addr0_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.iregnum);
          }
          if(!(op->addr1_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr1.iregnum, &op->addr1_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr1.iregnum);
          }
          if(!(op->dest_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->dest.iregnum, &op->dest_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->dest.iregnum);
          }
          break;
        OPS_NODEST_3ac
          if(!(op->addr1_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr1.iregnum, &op->addr1_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr1.iregnum);
          }
          __attribute__((fallthrough));
        OPS_1_3ac OPS_1_ASSIGN_3ac case DEALOC:
          if(!(op->addr0_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr0.iregnum, &op->addr0_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.iregnum);
          }
          break;
        OPS_2_3ac case ADDR_3:
          if(!(op->addr0_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->addr0.iregnum, &op->addr0_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.iregnum);
          }
          __attribute__((fallthrough));
        case CALL_3:
          if(!(op->dest_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->dest.iregnum, &op->dest_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->dest.iregnum);
          }
          break;
        case PHI:
          for(int j = 0; j < blk->inedges->length; j++) {
            if(!(op->addr0.joins[j].addr_type & (ISCONST | ISLABEL))) {
              int oldk = pht->keys;
              fixedinsert(pht, op->addr0.joins[j].addr.iregnum, &op->addr0.joins[j].addr_type);
              if(oldk != pht->keys) dapush(pda, (void*) (long) op->addr0.joins[j].addr.iregnum);
            }
          }
          if(!(op->dest_type & (ISCONST | ISLABEL))) {
            int oldk = pht->keys;
            fixedinsert(pht, op->dest.iregnum, &op->dest_type);
            if(oldk != pht->keys) dapush(pda, (void*) (long) op->dest.iregnum);
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

void killreg(PROGRAM* prog) {
  DYNARR* live = dactor(16);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(!blk->lastop) continue;
    OPERATION* op = blk->firstop;
    //live is anticipable out + will be killed + born already - already killed = anticipabile in
    for(unsigned int j = 0; j < prog->regcnt; j++) {
      if(bfget(blk->anticipability_in, j)) dapush(live, (void*) (long) j);
    }

    while(0) { //we have issues with register numbering here
      //a deref is still a use
      //what about addrsvar??? other nongvn thingies??
      switch(op->opcode) {
        OPS_3_3ac OPS_3_PTRDEST_3ac
          if(op->addr1_type & LASTUSE && !bfget(blk->anticipability_out, op->addr1.gvnind)) {
          }
          __attribute__((fallthrough));
        OPS_2_3ac case ADDR_3:
          if(op->addr0_type & LASTUSE && !bfget(blk->anticipability_out, op->addr0.gvnind)) {
          }
          __attribute__((fallthrough));
        case CALL_3:
          if(op->dest_type & LASTUSE && !bfget(blk->anticipability_out, op->dest.gvnind)) {
          }
          if(~op->dest_type & (ISCONST | ISLABEL | ISDEREF)) { //birth
          }
          break;
        OPS_NODEST_3ac
          if(op->addr1_type & LASTUSE && !bfget(blk->anticipability_out, op->addr1.gvnind)) {
          }
          __attribute__((fallthrough));
        OPS_1_3ac OPS_1_ASSIGN_3ac case DEALOC:
          if(op->addr0_type & LASTUSE && !bfget(blk->anticipability_out, op->addr0.gvnind)) {
          }
          break;
        case PHI:
          for(int j = 0; j < blk->inedges->length; j++) {
            if(op->addr0.joins[j].addr_type & LASTUSE && !bfget(blk->anticipability_out, op->addr0.joins[j].addr.gvnind)) {
            }
          }
          if(op->dest_type & LASTUSE && !bfget(blk->anticipability_out, op->dest.gvnind)) {
          }
          if(~op->dest_type & (ISCONST | ISLABEL | ISDEREF)) { //birth
          }
          break;
        OPS_NOVAR_3ac case ASM:
          break;
      }
      if(op == blk->lastop) break;
      op = op->nextop;
    }
  }
  dadtor(live);
}

#undef X
