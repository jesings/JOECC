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

//TODO: implement lengauer tarjan: https://www.cl.cam.ac.uk/~mr10/lengtarj.pdf
void ctdtree(PROGRAM* prog) {
  DYNARR* blocks = prog->allblocks;
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->dom = NULL;
    dtn->visited = 0;
    dtn->domind = -1;
  }
  BBLOCK** blocklist = calloc(sizeof(BBLOCK*), (blocks->length - 1));
  BBLOCK* first = daget(blocks, 0);
  first->dom = first;
  first->domind = 0;
  int ind = blocks->length - 1;
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
    for(int i = ind; i < oldlen - 1; i++) {
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

static SEDNODE* ctsednode(SEDAG* sedag, int hc) {
  SEDNODE* retval = malloc(sizeof(SEDNODE));
  retval->hasconst = hc;
  retval->equivs = dactor(8);
  retval->index = sedag->nodes->length;
  dapush(sedag->nodes, retval);
  return retval;
}

static SEDAG* ctsedag(PROGRAM* prog) {
  SEDAG* retval = malloc(sizeof(SEDAG));
  retval->nodes = dactor(1024);
  retval->varnodes = dactor(prog->iregcnt);
  retval->varnodes->length = prog->iregcnt;
  memset(retval->varnodes->arr, 0, prog->iregcnt * sizeof(void*));
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

static SEDITEM* ctsedi(unsigned int regno) {
  SEDITEM* retval = malloc(sizeof(SEDITEM));
  retval->regno = regno;
  retval->op = PARAM_3;
  return retval;
}

static SEDITEM* ctsedib(enum opcode_3ac opc, SEDNODE* eq1, SEDNODE* eq2) {
  SEDITEM* retval = malloc(sizeof(SEDITEM));
  retval->op = opc;
  retval->arg0 = eq1;
  retval->arg1 = eq2;
  return retval;
}

static void freesednode(SEDNODE* sednode) {
  dadtorfr(sednode->equivs);
  free(sednode);
}

static void freesedag(SEDAG* sed) {
  dadtorcfr(sed->nodes, (void(*)(void*)) freesednode);
  dadtor(sed->varnodes);
  fhtdtor(sed->intconsthash);
  fhtdtor(sed->floatconsthash);
  htdtor(sed->strconsthash);
  dadtorcfr(sed->opnodes, (void(*)(void*)) fhtdtor);
  free(sed);
}

static SEDNODE* nodefromaddr(SEDAG* sedag, ADDRTYPE adt, ADDRESS adr, PROGRAM* prog) {
  SEDNODE* cn;
  if(adt & ISCONST) {
    if(adt & ISSTRCONST) {
      cn = search(sedag->strconsthash, adr.strconst);
      if(!cn) {
        cn = ctsednode(sedag, STRCONST);
        cn->strconst = adr.strconst;
        insert(sedag->strconsthash, adr.strconst, cn);
      }

    } else {
      char floatness = adt & ISFLOAT;
      cn = fixedsearch(floatness ? sedag->floatconsthash : sedag->intconsthash, adr.intconst_64);
      if(!cn) {
        cn = ctsednode(sedag, floatness ? FLOATCONST : INTCONST);
        cn->intconst = adr.intconst_64;
        fixedinsert(floatness ? sedag->floatconsthash : sedag->intconsthash, adr.intconst_64, cn);
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
    cn = daget(sedag->varnodes, adr.iregnum);
    if(!cn) {
      cn = ctsednode(sedag, NOCONST);
      dapush(cn->equivs, ctsedi(adr.iregnum));
      sedag->varnodes->arr[adr.iregnum] = cn;
    }
  }
  return cn;
}

static void replacenode(BBLOCK* blk, SEDAG* sed, BITFIELD bf, PROGRAM* prog) {
  SEDNODE* sen;
  if(blk->lastop) {
    OPERATION* op = blk->firstop;
    while(1) {
      switch(op->opcode) {
        OPS_3_3ac_NOCOM OPS_3_3ac_COM case TPHI: OPS_3_PTRDEST_3ac
          sen = nodefromaddr(sed, op->dest_type, op->dest, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->opcode = NOP_3;
            break;
          }
          __attribute__((fallthrough));
        OPS_NODEST_3ac
          sen = nodefromaddr(sed, op->addr1_type, op->addr1, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->addr1_type &= 0xf | ISSIGNED;
            op->addr1_type |= ISCONST;
            if(sen->hasconst == STRCONST) op->addr1_type |= ISSTRCONST;
            else if(sen->hasconst == FLOATCONST) op->addr1_type |= ISFLOAT;
            op->addr1.intconst_64 = sen->intconst; //could be anything
          }
          __attribute__((fallthrough));
        OPS_1_3ac
          sen = nodefromaddr(sed, op->addr0_type, op->addr0, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->addr0_type &= 0xf | ISSIGNED;
            op->addr0_type |= ISCONST;
            if(sen->hasconst == STRCONST) op->addr0_type |= ISSTRCONST;
            else if(sen->hasconst == FLOATCONST) op->addr0_type |= ISFLOAT;
            op->addr0.intconst_64 = sen->intconst; //could be anything
          }
          break;
        OPS_2_3ac_MUT case MOV_3: case ADDR_3:
          sen = nodefromaddr(sed, op->dest_type, op->dest, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->opcode = NOP_3;
            break;
          }
          sen = nodefromaddr(sed, op->addr0_type, op->addr0, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->addr0_type &= 0xf | ISSIGNED;
            op->addr0_type |= ISCONST;
            if(sen->hasconst == STRCONST) op->addr0_type |= ISSTRCONST;
            else if(sen->hasconst == FLOATCONST) op->addr0_type |= ISFLOAT;
            op->addr0.intconst_64 = sen->intconst; //could be anything
          }
          break;
        case CALL_3: case ALOC_3:
          sen = nodefromaddr(sed, op->dest_type, op->dest, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->opcode = NOP_3;
          }
          break;
        case PHI:  ;
          FULLADDR* addrs = op->addr0.joins;
          for(int k = 0; k < blk->inedges->length; k++) {
            sen = daget(sed->varnodes, addrs[k].addr.ssaind);
            if(sen && (sen->hasconst != NOCONST)) {
              addrs[k].addr_type &= 0xf | ISSIGNED;
              addrs[k].addr_type |= ISCONST;
              if(sen->hasconst == STRCONST) addrs[k].addr_type |= ISSTRCONST;
              else if(sen->hasconst == FLOATCONST) addrs[k].addr_type |= ISFLOAT;
              addrs[k].addr.intconst_64 = sen->intconst; //could be anything
            }
          }
          break;
        OPS_1_ASSIGN_3ac
          sen = nodefromaddr(sed, op->addr0_type, op->addr0, prog);
          if(sen && (sen->hasconst != NOCONST)) {
            op->opcode = NOP_3;
          }
          break;
        OPS_NOVAR_3ac
          break;
      }
      if(op == blk->lastop) break;
      op = op->nextop;
    }
  }
  if(blk->idominates) {
    for(int i = 1; i < blk->idominates->length; i++) {
      BITFIELD cbf = bfclone(bf, sed->nodes->length);
      replacenode(daget(blk->idominates, i), sed, bf, prog);
      free(cbf);
    }
  }
}

void popsedag(PROGRAM* prog) { //Constructs, populates Strong Equivalence DAG
  SEDAG* dagnabbit = ctsedag(prog);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->lastop) {
      OPERATION* op = blk->firstop;
      SEDNODE* sen1,* sen2,* destsen;
      long combind;
      HASHTABLE* ophash;
      while(1) {
        switch(op->opcode) {
          OPS_NOVAR_3ac
            break; //nothing for nop, lbl, jmp, branching ops, or arg/ret
          OPS_3_3ac_NOCOM
            sen1 = nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            sen2 = nodefromaddr(dagnabbit, op->addr1_type, op->addr1, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1 && sen2) {
                ophash = daget(dagnabbit->opnodes, op->opcode);
                combind = ((long) sen1->index << 32) + sen2->index;
                destsen = fixedsearch(ophash, combind);
                if(!destsen) {
                  destsen = ctsednode(dagnabbit, NOCONST);
                  dapush(destsen->equivs, ctsedib(op->opcode, sen1, sen2));
                  fixedinsert(ophash, combind, destsen);
                }
              } else {
                destsen = ctsednode(dagnabbit, NOCONST);
              }
              dapush(destsen->equivs, ctsedi(op->dest.iregnum));
              dagnabbit->varnodes->arr[op->dest.iregnum] = destsen;
            }
            break;
          OPS_3_3ac_COM
            sen1 = nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            sen2 = nodefromaddr(dagnabbit, op->addr1_type, op->addr1, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1 && sen2) {
                ophash = daget(dagnabbit->opnodes, op->opcode);
                combind = ((long) sen1->index << 32) + sen2->index;
                destsen = fixedsearch(ophash, combind);
                if(!destsen) {
                  destsen = ctsednode(dagnabbit, NOCONST);
                  dapush(destsen->equivs, ctsedib(op->opcode, sen1, sen2));
                  fixedinsert(ophash, combind, destsen);
                  fixedinsert(ophash, ((long) sen2->index << 32) + sen1->index, destsen);
                }
              } else {
                destsen = ctsednode(dagnabbit, NOCONST);
              }
              dapush(destsen->equivs, ctsedi(op->dest.iregnum));
              dagnabbit->varnodes->arr[op->dest.iregnum] = destsen;
            }
            break;
          OPS_2_3ac_MUT
            sen1 = nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(sen1) {
                ophash = daget(dagnabbit->opnodes, op->opcode);
                destsen = fixedsearch(ophash, (long) sen1->index);
                if(!destsen) {
                  destsen = ctsednode(dagnabbit, NOCONST);
                  dapush(destsen->equivs, ctsedib(op->opcode, sen1, NULL));
                  fixedinsert(ophash, (long) sen1->index, destsen);
                }
              } else {
                destsen = ctsednode(dagnabbit, NOCONST);
              }
              dapush(destsen->equivs, ctsedi(op->dest.iregnum));
              dagnabbit->varnodes->arr[op->dest.iregnum] = destsen;
            }
            break;
          case MOV_3:
            sen1 = nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            if(!(op->dest_type & (ISLABEL | ISDEREF))) {
              if(op->dest_type & ISVAR) {
                FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
                if(adstore->addr_type & ADDRSVAR) break;
              }
              if(!sen1) {
                sen1 = ctsednode(dagnabbit, NOCONST);
              }
              dapush(sen1->equivs, ctsedi(op->dest.iregnum));
              dagnabbit->varnodes->arr[op->dest.iregnum] = sen1;
            }
            break;
          case ADDR_3:
            //TODO: we don't care about label or nodest or even deref, but how to represent?
            nodefromaddr(dagnabbit, op->dest_type, op->dest, prog);
            break;
          case TPHI:
            sen1 = nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            sen2 = nodefromaddr(dagnabbit, op->addr1_type, op->addr1, prog);
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
                  ophash = daget(dagnabbit->opnodes, op->opcode);
                  destsen = fixedsearch(ophash, combind);
                  if(!destsen) {
                    destsen = fixedsearch(ophash, ((long) sen2->index << 32) + sen1->index);
                  }
                  if(!destsen) {
                    destsen = ctsednode(dagnabbit, NOCONST);
                    dapush(destsen->equivs, ctsedib(op->opcode, sen1, sen2));
                    fixedinsert(ophash, combind, destsen);
                  }
                }
              } else {
                destsen = ctsednode(dagnabbit, NOCONST);
              }
              dapush(destsen->equivs, ctsedi(op->dest.iregnum));
              dagnabbit->varnodes->arr[op->dest.iregnum] = destsen;
            }
            nodefromaddr(dagnabbit, op->dest_type, op->dest, prog);
            break;
          case PHI: //TODO: get phi node handling loop at end
            nodefromaddr(dagnabbit, op->dest_type, op->dest, prog);
            break;
          OPS_3_PTRDEST_3ac OPS_NODEST_3ac
            nodefromaddr(dagnabbit, op->addr1_type, op->addr1, prog);
            __attribute__((fallthrough));
          OPS_1_3ac 
            nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            break;
          case CALL_3: //no pure functions for now
          case ALOC_3:
            nodefromaddr(dagnabbit, op->dest_type, op->dest, prog);
            break;
          OPS_1_ASSIGN_3ac
            nodefromaddr(dagnabbit, op->addr0_type, op->addr0, prog);
            break;
        }
        if(op == blk->lastop) break;
        op = op->nextop;
      }
    }
  }

  for(int i = 0; i < dagnabbit->nodes->length; i++) {
    SEDNODE* sen = daget(dagnabbit->nodes, i);
    printf("%d: ", sen->index);
    for(int j = 0; j < sen->equivs->length; j++) {
      SEDITEM* sed = daget(sen->equivs, j);
      printf("%s", opcode_3ac_names[sed->op]);
      switch(sed->op) {
          OPS_NOVAR_3ac OPS_NODEST_3ac OPS_1_3ac OPS_3_PTRDEST_3ac
            assert(0);
          OPS_3_3ac_NOCOM OPS_3_3ac_COM case TPHI:
            printf("(%d, %d) ", sed->arg0->index, sed->arg1->index);
            break;
          OPS_2_3ac_MUT
            printf("(%d) ", sed->arg0->index);
            break;
          case MOV_3: case ADDR_3: case PHI: case CALL_3: case ALOC_3:
            assert(0);
          OPS_1_ASSIGN_3ac
            printf("[%u] ", sed->regno);
      }
      printf(", ");
    }
    switch(sen->hasconst) {
      case INTCONST:
        printf("intconst %ld ", sen->intconst);
        break;
      case FLOATCONST:
        printf("floatconst %lf ", sen->floatconst);
        break;
      case STRCONST:
        printf("strconst \"%s\"", sen->strconst);
        break;
      case NOCONST:
        printf("no const");
        break;
    }
    putchar('\n');
  }
  BITFIELD bf = bfalloc(dagnabbit->nodes->length);
  replacenode(daget(prog->allblocks, 0), dagnabbit, bf, prog);
  free(bf);

  freesedag(dagnabbit);
}
//https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/gvn_sas04.pdf
#undef X
