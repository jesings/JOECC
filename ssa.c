#include "joecc_assert.h"
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

  //recursively handle immediately dominated nodes
  if(root->idominates)
    for(int i = 0; i < root->idominates->length; i++)
      dfpdt(daget(root->idominates, i));

  root->df = dactor(8);
  //if the children aren't immediately dominated, they're in the dominance frontier
  if(root->nextblock && root->nextblock->dom != root) { //only excludes last node
    dapush(root->df, root->nextblock);
  }
  if(root->branchblock && root->branchblock->dom != root) {
    dapush(root->df, root->branchblock);
  }

  if(root->idominates) {
    //for each immediately dominated node of the root node, for each node in its dominance frontier, 
    //add to the root node's dominance frontier if its not already there
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
  DYNINT* assigns = NULL;
  block->visited = 1;
  if(block->operations && block->operations->length) {
    assigns = dictor(32);
    for(int opind = 0; opind < block->operations->length; opind++) {
      OPERATION* op = block->operations->arr[opind];
      DYNINT* bdarr;
      switch(op->opcode) {
        OPS_3_3ac OPS_3_PTRDEST_3ac
          //rename second input operand if it's a variable
          if(op->addr1_type & ISVAR) {
            bdarr = daget(S, op->addr1.varnum);
            if(bdarr->length) //in case of addrsvar
              op->addr1.ssaind = dipeek(bdarr);
          }
          __attribute__((fallthrough));
        OPS_2_3ac case ADDR_3:
          //rename first input operand if it's a variable
          if(op->addr0_type & ISVAR) {
            bdarr = daget(S, op->addr0.varnum);
            if(bdarr->length) 
              op->addr0.ssaind = dipeek(bdarr);
          }
          __attribute__((fallthrough));
        case CALL_3:/*must have constant input in alloc_3*/
          //rename destination operand if it's a variable. if it is not a dereference, generate a new name
          if(op->dest_type & ISVAR) {
            bdarr = daget(S, op->dest.varnum);
            if(bdarr->length) {
              if(op->dest_type & ISDEREF) {
                op->dest.ssaind = dipeek(bdarr);
              } else {
                C[op->dest.varnum] = prog->regcnt++;
                op->dest.ssaind = C[op->dest.varnum];
                dipush(bdarr, C[op->dest.varnum]);
                dipush(assigns, op->dest.varnum);
              }
            }
          }
          break;
        case PHI: //ternary phi, needs special case
          if(op->addr0_type & GARBAGEVAL) {
            for(int i = 0; i < block->inedges->length; i++) {
              if(op->addr0.joins[i].addr_type & ISVAR) {
                bdarr = daget(S, op->addr0.joins[i].addr.varnum);
                if(bdarr->length) //in case of addrsvar
                  op->addr0.joins[i].addr.ssaind = dipeek(bdarr);
              }
            }
          }
          if(op->dest_type & ISVAR) {
            bdarr = daget(S, op->dest.varnum);
            if(bdarr->length) {
              if(op->dest_type & ISDEREF) {
                op->dest.ssaind = dipeek(bdarr);
              } else {
                C[op->dest.varnum] = prog->regcnt++;
                op->dest.ssaind = C[op->dest.varnum];
                dipush(bdarr, C[op->dest.varnum]);
                dipush(assigns, op->dest.varnum);
              }
            }
          }
          break;
        OPS_NODEST_3ac
          //rename second input operand if it's a variable
          if(op->addr1_type & ISVAR) {
            bdarr = daget(S, op->addr1.varnum);
            if(bdarr->length) //in case of addrsvar
              op->addr1.ssaind = dipeek(bdarr);
          }
          __attribute__((fallthrough));
        OPS_1_3ac
          if(op->addr0_type & GARBAGEVAL) break;
          __attribute__((fallthrough));
        case DEALOC:
          //rename first input operand if it's a variable
          if(op->addr0_type & ISVAR) {
            bdarr = daget(S, op->addr0.varnum);
            if(bdarr->length) 
              op->addr0.ssaind = dipeek(bdarr);
          }
          break;
        OPS_1_ASSIGN_3ac
          //rename destination operand, if the variable is never dereferenced, do a bunch of asserts and rename
          if(op->dest_type & ISVAR) {
            FULLADDR* fad = daget(prog->dynvars, op->dest.varnum);
            if(!(fad->addr_type & ADDRSVAR)) {
              bdarr = daget(S, op->dest.varnum);
              assert(!(op->dest_type & ISDEREF));
              assert(!C[op->dest.varnum]);
              C[op->dest.varnum] = prog->regcnt++;
              op->dest.ssaind = C[op->dest.varnum];
              dipush(bdarr, C[op->dest.varnum]);
              dipush(assigns, op->dest.varnum);
            } else {
              op->dest_type |= ADDRSVAR;
            }
          }
          break;
        OPS_NOVAR_3ac
          break;
        case ASM:
          assert(0);//unimplemented
      }
    }
  }

  //for nextblock, rename the values within the PHI, based on this block's names
  if(block->nextblock && block->nextblock->operations && block->nextblock->operations->length) {
    int i = -1;
    while(daget(block->nextblock->inedges, ++i) != block) ;
    for(int opind = 0; opind < block->nextblock->operations->length; opind++) {
      OPERATION* op = block->nextblock->operations->arr[opind];
      if(op->opcode != PHI) 
        break;
      if(!(op->addr0_type & GARBAGEVAL)) {
        DYNINT* bdarr = daget(S, op->dest.varnum);
        if(bdarr->length) {
          op->addr0.joins[i].addr.ssaind = dipeek(bdarr);
        }
        op->addr0.joins[i].addr.varnum = op->dest.varnum;
        op->addr0.joins[i].addr_type = op->dest_type;
      }
    }
  }
  //for branch, rename the values within the PHI, based on this block's names
  if(block->branchblock && block->branchblock->operations && block->branchblock->operations->length) {
    int i = -1;
    while(daget(block->branchblock->inedges, ++i) != block) ;
    for(int opind = 0; opind < block->branchblock->operations->length; opind++) {
      OPERATION* op = block->branchblock->operations->arr[i];
      if(op->opcode != PHI)
          break;
      assert(!(op->addr0_type & GARBAGEVAL)); //no ternaries can join at a branch block
      op->addr0.joins[i].addr.ssaind = dipeek((DYNINT*) daget(S, op->dest.varnum));
      op->addr0.joins[i].addr.varnum = op->dest.varnum;
      op->addr0.joins[i].addr_type = op->dest_type;
    }
  }

  //Recurse on nextblock and branchblock
  rrename(block->nextblock, C, S, prog);
  rrename(block->branchblock, C, S, prog);

  //free dynint "assigns" and associated resources in S
  if(assigns) {
    for(int i = 0; i < assigns->length; i++) {
      int l = diget(assigns, i);
      dipop((DYNINT*) daget(S, l));
    }
    didtor(assigns);
  }
}

void ssa(PROGRAM* prog) {
  DYNARR* blocks = prog->allblocks;
  //reset flags in each block
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->visited = 0;
    dtn->domind = -1;
  }

  //construct utility lists
  BBLOCK** blocklist = calloc(sizeof(BBLOCK*), blocks->length + 1);
  BBLOCK* first = daget(blocks, 0);

  //populate dominator information for the first block in the CFG
  first->dom = first; //we set the first block's dominator to be itself for simplifying later algorithms
  first->domind = 1;

  //Recursively populate dominator tree for other blocks
  int ind = blocks->length;
  rpdt(first->nextblock, blocklist, &ind);
  rpdt(first->branchblock, blocklist, &ind);
  
  //reset flags in blocks, mark unreachable blocks for removal
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    if(dtn->domind == -1) {
      if(dtn->nextblock && dtn->nextblock->operations && dtn->nextblock->operations->length) {
        //we don't need to check that it's a join and we also don't need to rearrange PHIs, because there must be a maximum of one ternary phi per block
        OPERATION* possphi = dtn->nextblock->operations->arr[0];
        if(possphi->opcode == PHI) {
          DYNARR* possum = dtn->nextblock->inedges;
          //ternary phis must have 2 source operands
          assert(possum->length == 2);
          assert(possphi->addr0_type & GARBAGEVAL);

          //replace it with a mov from the operand of the not bogus block
          FULLADDR source = possphi->addr0.joins[daget(possum, 0) == dtn ? 1 : 0];
          free(possphi->addr0.joins);
          possphi->opcode = MOV_3;
          possphi->addr0_type = source.addr_type;
          possphi->addr0 = source.addr;
        }
      }
      domark(dtn);
    }
  }

  //remove unreachable blocks
  int oldlen = blocks->length;
  rmunreach(prog);
  blocks = prog->allblocks;

  //find immediate dominators for each block, keep looping until nothing has changed since last iter.
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
          //find the last shared predecessor of each parent block and the previously thought idominator
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

  //reset flags in each block
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* dtn = daget(blocks, i);
    dtn->visited = 0;
    blocklist[i] = NULL;
    dtn->postdomind = -1;
  }

  ind = blocks->length;
  //if we don't have a final block (we're in an infinite loop and the final block was freed)
  //Then make a dummy one for simplifying some other code.
  if(!prog->finalblock) {
    prog->finalblock = mpblk(); //pseudo final block
    ind++;
  }

  //now populate postdominator information
  prog->finalblock->postdom = prog->finalblock;
  //recursively update from finalblock
  rupdt(prog->finalblock, blocklist, &ind);

  //if we haven't processed all the blocks we used a pseudo-final block
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
    //recursively find postdominators for the edges with backedges that we found
    for(int i = 0; i < prog->finalblock->inedges->length; i++)
      rupdt(daget(prog->finalblock->inedges, i), blocklist, &ind);
    prog->finalblock->visited = 0;
  } else {
    for(int i = 0; i < blocks->length; i++) {
      BBLOCK* blk = daget(blocks, i);
      blk->visited = 0;
    }
  }

  //now actually populate the postdom field
  changed = 1;
  //recurse until none of the postdominators are incorrect
  while(changed) {
    changed = 0;
    for(int i = ind; i < blocks->length; i++) {
      BBLOCK* cb = blocklist[i];
      if(!cb) continue;
      BBLOCK* new_pidom = NULL;
      if(cb->nextblock) {
        if(!cb->branchblock) new_pidom = cb->nextblock;
        else {
          //same logic as normal dominator
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
  //Construct dominator tree structure by allocating lists of immediately dominated nodes for every nonleaf
  for(int i = 1; i < blocks->length; i++) {//start at one so as not to let start block idominate itself
    BBLOCK* cb = daget(blocks, i);
    if(cb->dom) {
      if(!cb->dom->idominates)
        cb->dom->idominates = dactor(8);
      dapush(cb->dom->idominates, cb);
    }
    cb->visited = 0;
  }

  //Construct postdominator tree structure by allocating lists of immediately postdominated nodes for every nonleaf
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    BBLOCK* pdblk = blk->postdom;
    if(!pdblk) {
      pdblk = prog->finalblock;
    }
    if(!pdblk->pidominates) pdblk->pidominates = dactor(8);
    dapush(pdblk->pidominates, blk);
  }

  //say finalblock doesn't postdominate itself
  if(prog->finalblock->pidominates) {
    daremove_swap(prog->finalblock->pidominates, prog->finalblock);
  }
  //dominator tree (immediate dominators) calculated
  dfpdt(first); //populate dominance frontiers dfs
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    cb->visited = 0;
    cb->work = 0;
  }
  //no need to handle globals
  DYNARR* var_modifying_blocks = dactor(prog->dynvars->length);
  for(int i = 0; i < var_modifying_blocks->maxlength; i++)
    dapushc(var_modifying_blocks, dactor(16)); //initialize array for blocks that modify var
  //variable modification annotation, pass 1
  LOOPALLBLOCKS(
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
          DYNARR* dda = daget(var_modifying_blocks, op->dest.varnum);
          if(!dda->length || dapeek(dda) != blk)
            dapush(dda, blk);
        }
        break;
      OPS_1_ASSIGN_3ac
        if(!(op->dest_type & ADDRSVAR)) {
          DYNARR* dda = daget(var_modifying_blocks, op->dest.varnum);
          dapush(dda, blk);
        }
      default:
        break; //no possible correct destination
    }
  )

  //join node insertion, pass 2
  DYNARR* W = dactor(blocks->length);
  int itercount = 0;

  //TODO: figure out what this is actually doing for commenting it right
  //for each ssa reg
  for(int i = 0; i < prog->dynvars->length; i++) {
    ++itercount;
    FULLADDR* fadr = daget(prog->dynvars, i);
    if(fadr->addr_type & ADDRSVAR) continue;

    //find which blocks modify it
    DYNARR* blockassigns = daget(var_modifying_blocks, i);
    for(int j = 0; j < blockassigns->length; j++) {
      BBLOCK* block = daget(blockassigns, j);
      //work here shows the most recent variable that this block modifies(?)
      block->work = itercount;
      dapush(W, block);
    }

    BBLOCK* initblock = daget(blockassigns, 0);
    //for each modifying block, with some other things pushed
    for(int j = 0; j < W->length; j++) {
      BBLOCK* block = daget(W, j);
      if(block->df) {
        //for each block in the modifying block's dominance frontier
        for(int k = 0; k < block->df->length; k++) {
          BBLOCK* domblock = daget(block->df, k);

          //if the block has not been visited yet, it's not its own dominance frontier value, and it is not the finalblock
          if(domblock->visited < itercount && initblock != domblock && fixedintersect(initblock, domblock) && domblock != prog->finalblock) {
            ADDRESS jadr;
            jadr.joins = malloc(domblock->inedges->length * sizeof(FULLADDR));
            //prepend phi to the block
            if(!domblock->operations)
              domblock->operations = dactor(4);
            OPERATION* phi = ct_3ac_op2(PHI, ISCONST, jadr, fadr->addr_type, fadr->addr);
            dainsertat(domblock->operations, 0, phi);
            domblock->visited = itercount;
            //if the block has not been edited yet for this variable, then put it back on the list??????????
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
  int* C = calloc(sizeof(int), var_modifying_blocks->length);
  for(int i = 0; i < var_modifying_blocks->length; i++) {
    DYNARR* da = daget(var_modifying_blocks, i);
    //convert each DYNARR* into a DYNINT*
    da->length = 0;
    da->maxlength *= sizeof(void*) / sizeof(int);
  }
  
  //now rename registers recursively
  rrename(first, C, var_modifying_blocks, prog);
  for(int i = 0; i < blocks->length; i++) {
    BBLOCK* cb = daget(blocks, i);
    cb->visited = 0;
  }

  dadtorcfr(var_modifying_blocks, (void(*)(void*))didtor);
  free(C);
  prog->pdone |= SSA;
}
//lengauer tarjan: https://www.cl.cam.ac.uk/~mr10/lengtarj.pdf

static GVNNUM* ctgvnnum(EQONTAINER* eq, int hc) {
  GVNNUM* retval = malloc(sizeof(GVNNUM));
  retval->hasconst = hc;
  retval->equivs = dactor(8);
  retval->index = eq->uniq_vals->length;
  dapush(eq->uniq_vals, retval);
  return retval;
}

static EQONTAINER* cteq(PROGRAM* prog) {
  EQONTAINER* retval = malloc(sizeof(EQONTAINER));
  retval->uniq_vals = dactor(1024);
  ctgvnnum(retval, NOCONST); //have a dummy value in the zero position
  retval->intconsthash = lvhtctor();
  retval->floatconsthash = fvchtctor(32);
  retval->strconsthash = qhtctor();
  retval->ophash = opchtctor(8192);
  return retval;
}

static void freegvnnum(GVNNUM* eqnode) {
  dadtorfr(eqnode->equivs);
  free(eqnode);
}

static void freeq(EQONTAINER* eq) {
  dadtorcfr(eq->uniq_vals, (void(*)(void*)) freegvnnum);
  lvhtdtor(eq->intconsthash);
  fvhtdtor(eq->floatconsthash);
  qhtdtor(eq->strconsthash);
  ophtdtor(eq->ophash);
  free(eq);
}

static VALUESTRUCT* valdup(VALUESTRUCT* original) {
  VALUESTRUCT* duplicate = malloc(sizeof(VALUESTRUCT));
  memcpy(duplicate, original, sizeof(VALUESTRUCT));
  return duplicate;
}

//find which equivalence node, if any, this address corresponds to, and create one if it corresponds to nothing extant
static GVNNUM* nodefromaddr(EQONTAINER* eq, ADDRTYPE adt, ADDRESS adr, PROGRAM* prog) {
  GVNNUM* cn;
  if(adt & ISCONST) {
    if(adt & ISSTRCONST) {
      cn = qsearch(eq->strconsthash, adr.strconst);
      if(!cn) {
        cn = ctgvnnum(eq, STRCONST);
        cn->strconst = adr.strconst;
        qinsert(eq->strconsthash, adr.strconst, cn);
      }

    } else {
      if(adt & ISFLOAT) {
          cn = fvsearch(eq->floatconsthash, adr.floatconst_64);
          if(!cn) {
              cn = ctgvnnum(eq, FLOATCONST);
              cn->floatconst = adr.floatconst_64;
              fvinsert(eq->floatconsthash, adr.floatconst_64, cn);
          }
      } else {
          cn = lvsearch(eq->intconsthash, adr.intconst_64);
          if(!cn) {
              cn = ctgvnnum(eq, INTCONST);
              cn->intconst = adr.intconst_64;;
              lvinsert(eq->intconsthash, adr.intconst_64, cn);
          }
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
    VALUESTRUCT valst = {INIT_3, adr.regnum, 0, supersize(adt), 0};
    cn = opsearch(eq->ophash, &valst);
    if(!cn) {
      cn = ctgvnnum(eq, NOCONST);
      VALUESTRUCT* exn = ctvalstruct(INIT_3, adr.regnum, 0, supersize(adt), 0);
      opinsert(eq->ophash, exn, cn);
      dapush(cn->equivs, exn);
    }
  }
  return cn;
}

static GVNNUM* supernodefromaddr(EQONTAINER* eq, char ty, ADDRESS adr, PROGRAM* prog) {
  return nodefromaddr(eq, downsize(ty), adr, prog);
}

static GVNNUM* derefwithnodefromaddr(EQONTAINER* eq, ADDRTYPE ty, ADDRESS adr, PROGRAM* prog) {
  if(ty & ISDEREF) {
    ADDRTYPE newty = 8 | (ty & (ISVAR | ISLABEL | ISPOINTER | GARBAGEVAL | LASTUSE));
    return nodefromaddr(eq, newty, adr, prog);
  } else {
    return nodefromaddr(eq, ty, adr, prog);
  }
}

//replace operation via gvn
static void replaceop(BBLOCK* blk, EQONTAINER* eq, PROGRAM* prog, OPERATION* op) {
  IIHASHTABLE* leader = blk->leader;
  GVNNUM* val;
  switch(op->opcode) {
    OPS_3_3ac OPS_3_PTRDEST_3ac
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        if(val->hasconst != NOCONST) {
          op->opcode = NOP_3;
          break;
        }
        if(op->dest.regnum != (unsigned) iisearch(leader, val->index)) {
          op->opcode = NOP_3;
          break;
        }
      } else {
        val = derefwithnodefromaddr(eq, op->dest_type, op->dest, prog);
        if(val && iiqueryval(leader, val->index)) {
          op->dest.regnum = iisearch(leader, val->index);
        }
      }
      __attribute__((fallthrough));
    OPS_NODEST_3ac
      val = derefwithnodefromaddr(eq, op->addr1_type, op->addr1, prog);
      if(val) {
        if(val->hasconst != NOCONST && !(op->addr1_type & ISDEREF)) {
          op->addr1_type = (op->addr1_type & GENREGMASK) | ISCONST;
          op->addr1.intconst_64 = val->intconst; //could be anything
        } else if(iiqueryval(leader, val->index)) {
          op->addr1.regnum = iisearch(leader, val->index);
        }
      }
      __attribute__((fallthrough));
    OPS_1_3ac
      if(op->addr0_type & GARBAGEVAL) break;
      val = derefwithnodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(val) {
        if(val->hasconst != NOCONST && !(op->addr0_type & ISDEREF)) {
          op->addr0_type = (op->addr0_type & GENREGMASK) | ISCONST;
          op->addr0.intconst_64 = val->intconst; //could be anything
        } else {
          if(iiqueryval(leader, val->index))
          op->addr0.regnum = iisearch(leader, val->index);
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
        if(op->dest.regnum != (unsigned) iisearch(leader, val->index)) {
          op->opcode = NOP_3;
          break;
        }
      } else {
        val = derefwithnodefromaddr(eq, op->dest_type, op->dest, prog);
        if(val && iiqueryval(leader, val->index)) {
          op->dest.regnum = iisearch(leader, val->index);
        }
      }
      val = derefwithnodefromaddr(eq, op->addr0_type, op->addr0, prog);
      if(val) {
        if(val->hasconst != NOCONST && !(op->addr0_type & ISDEREF)) {
          op->addr0_type = (op->addr0_type & GENREGMASK) | ISCONST;
          op->addr0.intconst_64 = val->intconst; //could be anything
        } else if(iiqueryval(leader, val->index)) {
            op->addr0.regnum = iisearch(leader, val->index);
        }
      }
      break;
    case CALL_3:
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        assert(val->hasconst == NOCONST);
        assert(op->dest.regnum == (unsigned) iisearch(leader, val->index));
      } else {
        val = derefwithnodefromaddr(eq, op->dest_type, op->dest, prog);
        if(val && iiqueryval(leader, val->index)) {
          op->dest.regnum = iisearch(leader, val->index);
        }
      }
      break;
    OPS_1_ASSIGN_3ac
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        assert(val->hasconst == NOCONST);
        assert(op->dest.regnum == (unsigned) iisearch(leader, val->index));
      } //no need to handle deref for i.e. PARAM_3, INIT_3
      break;
    case PHI: 
      for(int k = 0; k < blk->inedges->length; k++) {
        FULLADDR* fadrs = op->addr0.joins;
        val = derefwithnodefromaddr(eq, fadrs[k].addr_type, fadrs[k].addr, prog);
        IIHASHTABLE* predled = ((BBLOCK*) daget(blk->inedges, k))->leader;
        if(val) {
          if(val->hasconst != NOCONST && !(fadrs[k].addr_type & ISDEREF)) {
            fadrs[k].addr_type = (fadrs[k].addr_type & GENREGMASK) | ISCONST;
            fadrs[k].addr.intconst_64 = val->intconst; //could be anything
          } else if(iiqueryval(predled, val->index)) {
              fadrs[k].addr.regnum = iisearch(predled, val->index);
          }
        }
      }
      val = nodefromaddr(eq, op->dest_type, op->dest, prog);
      if(val) {
        assert(val->hasconst == NOCONST);
        assert(op->dest.regnum == (long) iisearch(leader, val->index));
      } else {
        //this should only probably be the case for ternary phis
        val = derefwithnodefromaddr(eq, op->dest_type, op->dest, prog);
        if(val && iiqueryval(leader, val->index)) {
          op->dest.regnum = iisearch(leader, val->index);
        }
      }
      break;
    OPS_NOVAR_3ac
      break;
    case ASM:
      assert(0); //unimplemented
  }
}

static void replacegvn(EQONTAINER* eq, PROGRAM* prog) {
  LOOPALLBLOCKS(
    replaceop(blk, eq, prog, op);
  )
}

static void debuggo(EQONTAINER* eq, PROGRAM* prog) {
  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    BBLOCK* blk = daget(prog->allblocks, blockind);
    GVNNUM* gn;
    printf("BBLOCK NUMBER %d\n", blk->domind);
    LOOPOPS(
      printf("%s ", opcode_3ac_names[op->opcode]);
      OPARGCASES(
        printaddr(op->addr0, op->addr0_type, 1, stdout, prog);
        gn = derefwithnodefromaddr(eq, op->addr0_type, op->addr0, prog);
        if(op->addr0_type & ISDEREF) {
          printf("([%d]),", gn ? gn->index : -1);
        } else {
          printf("[%d],", gn ? gn->index : -1);
        }
        ,
        printaddr(op->addr1, op->addr1_type, 1, stdout, prog);
        gn = derefwithnodefromaddr(eq, op->addr1_type, op->addr1, prog);
        if(op->addr0_type & ISDEREF) {
          printf("([%d]),", gn ? gn->index : -1);
        } else {
          printf("[%d],", gn ? gn->index : -1);
        }
        ,
        printaddr(op->dest, op->dest_type, 1, stdout, prog);
        gn = derefwithnodefromaddr(eq, op->dest_type, op->dest, prog);
        if(op->addr0_type & ISDEREF) {
          printf("([%d]),", gn ? gn->index : -1);
        } else {
          printf("[%d],", gn ? gn->index : -1);
        }
        ,
        printaddr(phijoinaddr->addr, phijoinaddr->addr_type, 1, stdout, prog);
        gn = derefwithnodefromaddr(eq, phijoinaddr->addr_type, phijoinaddr->addr, prog);
        if(op->addr0_type & ISDEREF) {
          printf("([%d]),", gn ? gn->index : -1);
        } else {
          printf("[%d],", gn ? gn->index : -1);
        }
      )
      putchar('\n');
    )
  }
}

//number values
static void gensall(PROGRAM* prog, EQONTAINER* eq, BBLOCK* blk) {
  blk->leader = iiclone(blk->dom->leader);
  blk->antileader_in = lvhtctor();
  blk->antileader_in_list = dictor(64);
  if(blk->operations && blk->operations->length) {
    blk->tmp_gen = dactor(32);
    blk->exp_gen = lvhtctor();
    blk->exp_gen_list = dictor(64);
    blk->antileader_out = lvhtctor();
    blk->antileader_out_list = dictor(64);
    VALUESTRUCT valst = {INIT_3, 0, 0, 0, 0};
    for(int opind = 0; opind < blk->operations->length; opind++) {
      OPERATION* op = blk->operations->arr[opind];
      GVNNUM* val1;
      GVNNUM* val2;
      GVNNUM* destval = NULL;
      GVNNUM* otherval = NULL;
      VALUESTRUCT finalval;
      OPHASHTABLE* ophash = eq->ophash;
      switch(op->opcode) {
        OPS_NOVAR_3ac
          break; //nothing for nop, lbl, jmp, branching ops, or arg/ret
        OPS_3_3ac_NOCOM
          val1 = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
          val2 = nodefromaddr(eq, op->addr1_type, op->addr1, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1 && val2) {
              VALUESTRUCT combind = {op->opcode, val1->index, val2->index, supersize(op->addr0_type), supersize(op->addr1_type)};
              destval = opsearch(ophash, &combind);
              if(!destval) {
                destval = ctgvnnum(eq, NOCONST);
                dapush(destval->equivs, valdup(&combind));
                opinsert(ophash, &combind, destval);
              }
            } else {
              destval = ctgvnnum(eq, NOCONST);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, op->dest.regnum, 0, supersize(op->dest_type), 0));
            finalval.o = INIT_3;
            finalval.p1 = op->dest.regnum;
            finalval.p2 = 0;
            finalval.size1 = supersize(op->dest_type);
            finalval.size2 = 0;
            opinsert(ophash, &finalval, destval);
          } else if(val1 && val2) {
            VALUESTRUCT combind = {op->opcode, val1->index, val2->index, supersize(op->addr0_type), supersize(op->addr1_type)};
            otherval = opsearch(ophash, &combind);
            if(!otherval) {
              otherval = ctgvnnum(eq, NOCONST);
              dapush(otherval->equivs, valdup(&combind));
              opinsert(ophash, &combind, otherval);
            }
          }
          break;
        OPS_3_3ac_COM
          val1 = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
          val2 = nodefromaddr(eq, op->addr1_type, op->addr1, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1 && val2) {
              VALUESTRUCT combind = {op->opcode, val1->index, val2->index, supersize(op->addr0_type), supersize(op->addr1_type)};
              destval = opsearch(ophash, &combind);
              if(!destval) {
                destval = ctgvnnum(eq, NOCONST);
                dapush(destval->equivs, valdup(&combind));
                opinsert(ophash, &combind, destval);
                VALUESTRUCT combind2 = {op->opcode, val2->index, val1->index, supersize(op->addr1_type), supersize(op->addr0_type)};
                opinsert(ophash, &combind2, destval);
              }
            } else {
              destval = ctgvnnum(eq, NOCONST);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, op->dest.regnum, 0, supersize(op->dest_type), 0));
            finalval.o = INIT_3;
            finalval.p1 = op->dest.regnum;
            finalval.p2 = 0;
            finalval.size1 = supersize(op->dest_type);
            finalval.size2 = 0;
            opinsert(ophash, &finalval, destval);
          } else if(val1 && val2) {
            VALUESTRUCT combind = {op->opcode, val1->index, val2->index, supersize(op->addr0_type), supersize(op->addr1_type)};
            otherval = opsearch(ophash, &combind);
            if(!otherval) {
              otherval = ctgvnnum(eq, NOCONST);
              dapush(otherval->equivs, valdup(&combind));
              opinsert(ophash, &combind, otherval);
              VALUESTRUCT combind2 = {op->opcode, val2->index, val1->index, supersize(op->addr1_type), supersize(op->addr0_type)};
              opinsert(ophash, &combind2, otherval);
            }
          }
          break;
        OPS_2_3ac_MUT //Do we really want to handle ALOC_3 this way?
          val1 = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            if(val1) {
              VALUESTRUCT combind = {op->opcode, val1->index, 0, supersize(op->addr0_type), 0};
              destval = opsearch(ophash, &combind);
              if(!destval) {
                destval = ctgvnnum(eq, NOCONST);
                dapush(destval->equivs, valdup(&combind));
                opinsert(ophash, &combind, destval);
              }
            } else {
              destval = ctgvnnum(eq, NOCONST);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, op->dest.regnum, 0, supersize(op->dest_type), 0));
            finalval.o = INIT_3;
            finalval.p1 = op->dest.regnum;
            finalval.p2 = 0;
            finalval.size1 = supersize(op->dest_type);
            finalval.size2 = 0;
            opinsert(ophash, &finalval, destval);
          } else if(val1) {
            VALUESTRUCT combind = {op->opcode, val1->index, 0, supersize(op->addr0_type), 0};
            otherval = opsearch(ophash, &combind);
            if(!otherval) {
              otherval = ctgvnnum(eq, NOCONST);
              dapush(otherval->equivs, valdup(&combind));
              opinsert(ophash, &combind, otherval);
            }
          }
          break;
        case MOV_3:
          val1 = nodefromaddr(eq, op->addr0_type, op->addr0, prog);
          if(!(op->dest_type & (ISLABEL | ISDEREF))) {
            if(op->dest_type & ISVAR) {
              FULLADDR* adstore = daget(prog->dynvars, op->dest.varnum);
              if(adstore->addr_type & ADDRSVAR) break;
            }
            //hmm this should be fine to keep this way as size can't increase at mov?
            if(val1) {
              destval = val1;
            } else {
              destval = ctgvnnum(eq, NOCONST);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, op->dest.regnum, 0, supersize(op->dest_type), 0));
            finalval.o = INIT_3;
            finalval.p1 = op->dest.regnum;
            finalval.p2 = 0;
            finalval.size1 = supersize(op->dest_type);
            finalval.size2 = 0;
            opinsert(ophash, &finalval, destval);
          }
          break;
        case ADDR_3:
          //address should stay constant, so the value can be stored, as can the value of labels!
          val1 = nodefromaddr(eq, op->dest_type, op->dest, prog);
          if(!(op->dest_type & (ISDEREF | ISLABEL))) {
            //addrsvar is permissible
            if(val1) {
              VALUESTRUCT combind = {op->opcode, val1->index, 0, supersize(op->addr0_type), 0};
              destval = opsearch(ophash, &combind);
              if(!destval) {
                destval = ctgvnnum(eq, NOCONST);
                dapush(destval->equivs, valdup(&combind));
                opinsert(ophash, &combind, destval);
              }
            } else {
              destval = ctgvnnum(eq, NOCONST);
            }
            dapush(destval->equivs, ctvalstruct(INIT_3, op->dest.regnum, 0, supersize(op->dest_type), 0));
            finalval.o = INIT_3;
            finalval.p1 = op->dest.regnum;
            finalval.p2 = 0;
            finalval.size1 = supersize(op->dest_type);
            finalval.size2 = 0;
            opinsert(ophash, &finalval, destval);
          } else if(val1) {
            VALUESTRUCT combind = {op->opcode, val1->index, 0, supersize(op->addr0_type), 0};
            otherval = opsearch(ophash, &combind);
            if(!otherval) {
              otherval = ctgvnnum(eq, NOCONST);
              dapush(otherval->equivs, valdup(&combind));
              opinsert(ophash, &combind, otherval);
            }
          }
          break;
        case PHI:
          destval = nodefromaddr(eq, op->dest_type, op->dest, prog);
          finalval = *(VALUESTRUCT*) daget(destval->equivs, 0);
          break;
        case DEALOC:
          {
            VALUESTRUCT combind = {DEALOC, op->addr0.ssaind, 0, 0, 0};
            val1 = opsearch(ophash, &combind);
            if(!val1) {
              val2 = ctgvnnum(eq, NOCONST);
              dapush(val2->equivs, ctvalstruct(DEALOC, op->addr0.ssaind, 0, 0, 0));
              opinsert(ophash, &combind, val2);
            }
          }
          break;
        OPS_3_PTRDEST_3ac OPS_NODEST_3ac
          nodefromaddr(eq, op->addr1_type, op->addr1, prog);
          __attribute__((fallthrough));
        OPS_1_3ac
          if(op->addr0_type & GARBAGEVAL) break;
          nodefromaddr(eq, op->addr0_type, op->addr0, prog);
          break;
        case CALL_3: //no pure functions for now
          destval = nodefromaddr(eq, op->dest_type, op->dest, prog);
          if(destval) finalval = *(VALUESTRUCT*) daget(destval->equivs, 0);
          break;
        OPS_1_ASSIGN_3ac
          destval = nodefromaddr(eq, op->dest_type, op->dest, prog);
          if(destval) finalval = *(VALUESTRUCT*) daget(destval->equivs, 0);
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
              valst.p1 = op->addr0.regnum;
              valst.size1 = supersize(op->addr0_type);
              chosenval = opsearch(eq->ophash, &valst);
              if(chosenval) {
                status++;
                //slightly inefficient, could query and insert in same traversal
                //however it might be worse to have to re-free the value struct if not used
                if(!lvqueryval(blk->exp_gen, chosenval->index)) {
                  lvinsert(blk->exp_gen, chosenval->index, valdup(&valst));
                  dipush(blk->exp_gen_list, chosenval->index);
                }
              }
            }
          }
          if(!(op->addr1_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(op->addr1_type & ISCONST) {
              status++;
            } else {
              valst.p1 = op->addr1.regnum;
              valst.size1 = supersize(op->addr1_type);
              chosenval = opsearch(eq->ophash, &valst);
              if(chosenval) {
                status++;
                if(!lvqueryval(blk->exp_gen, chosenval->index)) {
                  lvinsert(blk->exp_gen, chosenval->index, valdup(&valst));
                  dipush(blk->exp_gen_list, chosenval->index);
                }
              }
            }
          }
          if(status == 2) {
            GVNNUM* n0 = nodefromaddr(eq, op->addr0_type & ~ISVAR, op->addr0, prog);
            GVNNUM* n1 = nodefromaddr(eq, op->addr1_type & ~ISVAR, op->addr1, prog);

            if(!(n0 && n1)) break;
            VALUESTRUCT refex = {op->opcode, n0->index, n1->index, supersize(op->addr0_type), supersize(op->addr1_type)};
            chosenval = opsearch(eq->ophash, &refex);

            if(chosenval && !lvqueryval(blk->exp_gen, chosenval->index)) {
              lvinsert(blk->exp_gen, chosenval->index, valdup(&refex));
              dipush(blk->exp_gen_list, chosenval->index);
            }
          } else {
            if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
              long fullval = ((long) supersize(op->dest_type)) << 32 | op->dest.regnum;
              dapush(blk->tmp_gen, (void*) fullval);
            }
          }
          break;
        OPS_2_3ac_MUT case MOV_3: 
          if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(op->addr0_type & ISCONST) {
              status++;
            } else {
              valst.p1 = op->addr0.regnum;
              valst.size1 = supersize(op->addr0_type);
              chosenval = opsearch(eq->ophash, &valst);
              if(chosenval) {
                status++;
                if(!lvqueryval(blk->exp_gen, chosenval->index)) {
                  lvinsert(blk->exp_gen, chosenval->index, valdup(&valst));
                  dipush(blk->exp_gen_list, chosenval->index);
                }
              }
            }
          }
          if(status == 1) {
            GVNNUM* n0 = nodefromaddr(eq, op->addr0_type & ~ISVAR, op->addr0, prog);
            if(!n0) break;
            VALUESTRUCT refex = {op->opcode, n0->index, 0, supersize(n0->index), 0};
            chosenval = opsearch(eq->ophash, &refex);
            if(chosenval && !lvqueryval(blk->exp_gen, chosenval->index)) {
              lvinsert(blk->exp_gen, chosenval->index, valdup(&refex));
              dipush(blk->exp_gen_list, chosenval->index);
            }
          } else {
            if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
              long fullval = ((long) supersize(op->dest_type)) << 32 | op->dest.regnum;
              dapush(blk->tmp_gen, (void*) fullval);
            }
          }
          break;
          __attribute__((fallthrough));
        case PHI: case CALL_3:
          //we always treat these as black boxes
          if(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            long fullval = ((long) supersize(op->dest_type)) << 32 | op->dest.regnum;
            dapush(blk->tmp_gen, (void*) fullval);
          }
          break;
        case ADDR_3:
          //kill of ADDR_3 only caused by kill of value it's taking the address of
          break;
        OPS_1_ASSIGN_3ac
          assert(!(op->dest_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST)));
          long fullval = ((long) supersize(op->dest_type)) << 32 | op->dest.regnum;
          dapush(blk->tmp_gen, (void*) fullval);
          break;
        OPS_NODEST_3ac OPS_3_PTRDEST_3ac
          if(!(op->addr1_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(!(op->addr0_type & ISCONST)) {
              valst.p1 = op->addr1.regnum;
              valst.size1 = supersize(op->addr1_type);
              chosenval = opsearch(eq->ophash, &valst);
              if(chosenval && !lvqueryval(blk->exp_gen, chosenval->index)) {
                lvinsert(blk->exp_gen, chosenval->index, valdup(&valst));
                dipush(blk->exp_gen_list, chosenval->index);
              }
            }
          }
          __attribute__((fallthrough));
        OPS_1_3ac
          if(!(op->addr0_type & (ISDEREF | GARBAGEVAL | ISLABEL))) {
            if(!(op->addr0_type & ISCONST)) {
              valst.p1 = op->addr0.regnum;
              valst.size1 = supersize(op->addr0_type);
              chosenval = opsearch(eq->ophash, &valst);
              if(chosenval && !lvqueryval(blk->exp_gen, chosenval->index)) {
                lvinsert(blk->exp_gen, chosenval->index, valdup(&valst));
                dipush(blk->exp_gen_list, chosenval->index);
              }
            }
          }
          break;
        OPS_NOVAR_3ac case DEALOC:
          break;
        case ASM:
          assert(0); //unimplemented
      }
      if(destval && !iiqueryval(blk->leader, destval->index)) {
        assert(finalval.o == INIT_3);
        iiinsert(blk->leader, destval->index, finalval.p1);
      }
    }
  }
  if(blk->idominates)
    for(int i = 0; i < blk->idominates->length; i++)
      gensall(prog, eq, daget(blk->idominates, i));
}
//translate an expression across a phi, translation table pre-populated
static VALUESTRUCT* translate(PROGRAM* prog, EQONTAINER* eq, BBLOCK* blk, BBLOCK* blkn, VALUESTRUCT* prevex) {
  int translated;
  GVNNUM* val1;
  GVNNUM* val2;
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
      if((translated = iisearch(blk->translator, prevex->p1))) {
        a1.regnum = translated;
        val1 = supernodefromaddr(eq, prevex->size1, a1, prog);
      } else {
        a1.regnum = prevex->p1;
        val1 = supernodefromaddr(eq, prevex->size1, a1, prog);
      }
      if((translated = iisearch(blk->translator, prevex->p2))) {
        a2.regnum = translated;
        val2 = supernodefromaddr(eq, prevex->size2, a2, prog);
      } else {
        a2.regnum = prevex->p2;
        val2 = supernodefromaddr(eq, prevex->size2, a2, prog);
      }
      if(!(val1 && val2)) return NULL;
      return ctvalstruct(prevex->o, val1->index, val2->index, prevex->size1, prevex->size2);
    OPS_2_3ac_MUT
      if((translated = iisearch(blk->translator, prevex->p1))) {
        a1.regnum = translated;
        val1 = supernodefromaddr(eq, prevex->size1, a1, prog);
      } else {
        a1.regnum = prevex->p1;
        val1 = supernodefromaddr(eq, prevex->size1, a1, prog);
      }
      if(!val1) return NULL;
      return ctvalstruct(prevex->o, val1->index, 0, prevex->size1, 0);
    OPS_1_ASSIGN_3ac case ADDR_3:
      if((translated = iisearch(blk->translator, prevex->p1))) {
        a1.regnum = translated;
        val1 = supernodefromaddr(eq, prevex->size1, a1, prog);
      } else {
        a1.regnum = prevex->p1;
        val1 = supernodefromaddr(eq, prevex->size1, a1, prog);
      }
      if(!val1) return NULL;
      return ctvalstruct(INIT_3, a1.regnum, 0, prevex->size1, 0);
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
      if(blkn->operations && blkn->operations->length) {
        OPERATION* op = blkn->operations->arr[0];
        if(op->opcode == PHI) {
          blk->translator = iihtctor(); //would be more efficient to keep dag of references?
          blk->revtranslator = iihtctor(); //would be more efficient to keep dag of references?
          for(int opind = 0; opind < blkn->operations->length; opind++) {
            OPERATION* op = blkn->operations->arr[opind];
            if(op->opcode != PHI) break;
            int prephi = op->addr0.joins[index].addr.regnum;
            int postphi = op->dest.regnum;
            iiinsert(blk->translator, prephi, postphi);
            iiinsert(blk->revtranslator, postphi, prephi);
          }
        }
      }
    }
  }
  if(!blk->pidominates) {
    return 0;
  }

  LVHASHTABLE* oldanticin = blk->antileader_in;
  LVHASHTABLE* oldanticout = blk->antileader_out;
  DYNINT* oldanticinlist = blk->antileader_in_list;
  DYNINT* oldanticoutlist = blk->antileader_out_list;

  if(blk->nextblock && !blk->branchblock) {
    int index = 0;
    BBLOCK* blkn = blk->nextblock;
    for(; daget(blkn->inedges,index) != blk; index++) ;
    if(blkn->operations && blkn->operations->length && 
            ((OPERATION*) blkn->operations->arr[0])->opcode == PHI) {
      blk->antileader_out = lvhtctor();
      blk->antileader_out_list = dictor(64);
      if(!blk->translator) { //translators will be properly populated the first time
        for(int i = 0; i < blkn->antileader_in_list->length; i++) {
          void* value = lvsearch(blkn->antileader_in, blkn->antileader_in_list->arr[i]);
          VALUESTRUCT* translated = translate(prog, eq, blk, blkn, value);
          int valnum;
          if(translated) {
            if(translated->o != INIT_3) {
              GVNNUM* destval = opsearch(eq->ophash, translated);
              if(!destval) {
                destval = ctgvnnum(eq, NOCONST);
                opinsert(eq->ophash, translated, destval);
                dapush(destval->equivs, valdup(translated));
              }
              valnum = destval->index;
            } else {
              valnum = translated->p1;
            }
            void* storage;
            if((storage = lvsearch(blk->antileader_out, valnum))) {
                free(storage);
            } else {
                dipush(blk->antileader_out_list, valnum); //only add to list if it wasn't already there
            }
            lvinsert(blk->antileader_out, valnum, translated);
          }
        }
      }
    } else {
      blk->antileader_out = lvhtcclone(blkn->antileader_in, (void*(*)(void*)) valdup);
      blk->antileader_out_list = diclone(blkn->antileader_in_list);
    }
  } else if(blk->branchblock) {
    //the first block isn't populated here if next block has phi
    if(blk->nextblock->antileader_in) {
      blk->antileader_out = lvhtcclone(blk->nextblock->antileader_in, (void*(*)(void*)) valdup);
      blk->antileader_out_list = diclone(blk->nextblock->antileader_in_list);
      if(blk->branchblock->antileader_in) {
        for(int i = 0; i < blk->antileader_out_list->length; i++) {
          int key = blk->antileader_out_list->arr[i];
          void* val;
          if((val = lvsearch(blk->branchblock->antileader_in, key))) {
            free(val);
            lvrmpair(blk->antileader_out, key);
            blk->antileader_out_list->arr[i] = -1;
          }
        }

        int newlen = 0;
        int aoindex;
        for(aoindex = 0; aoindex < blk->antileader_out_list->length; aoindex++)  {
            if(blk->antileader_out_list->arr[aoindex] == -1)
                continue;
            blk->antileader_out_list->arr[newlen++] = blk->antileader_out_list->arr[aoindex];
        }
        blk->antileader_out_list->length = newlen;
      }
    } else if(blk->branchblock->antileader_in) {
      blk->antileader_out = lvhtcclone(blk->branchblock->antileader_in, (void*(*)(void*)) valdup);
      blk->antileader_out_list = diclone(blk->branchblock->antileader_in_list);
    } else {
      blk->antileader_out = lvhtctor();
      blk->antileader_out_list = dictor(32);
    }
    //and with branchblock
    //>1 succ
  } else {
    blk->antileader_out = lvhtctor();
    blk->antileader_out_list = dictor(32);
  }
  LVHASHTABLE* antiin_users = lvchtctor(32); //ht of dynarrs of expressions which would be killed by a kill of the value number
  blk->antileader_in = lvhtcclone(blk->antileader_out, (void*(*)(void*)) valdup);
  blk->antileader_in_list = diclone(blk->antileader_out_list);
  if(blk->exp_gen) {
    for(int i = 0; i < blk->exp_gen_list->length; i++) {
      VALUESTRUCT* exs = lvsearch(blk->exp_gen, blk->exp_gen_list->arr[i]);
      ADDRESS a;
      GVNNUM* n3;
      switch((int) exs->o) {
        OPS_NOVAR_3ac OPS_3_PTRDEST_3ac case MOV_3:
        OPS_NODEST_3ac OPS_1_3ac case CALL_3: case PHI:
        case ASM:
          assert(0);
        case DEALOC:
          continue;
        OPS_3_3ac
          n3 = opsearch(eq->ophash, exs);
          if(!lvqueryval(blk->antileader_in, n3->index)) {
            lvinsert(blk->antileader_in, n3->index, valdup(exs));
            dipush(blk->antileader_in_list, n3->index);
          }
          break;
        OPS_2_3ac_MUT
          n3 = opsearch(eq->ophash, exs);
          if(!lvqueryval(blk->antileader_in, n3->index)) {
            lvinsert(blk->antileader_in, n3->index, valdup(exs));
            dipush(blk->antileader_in_list, n3->index);
          }
          break;
        OPS_1_ASSIGN_3ac case ADDR_3:
          a.regnum = exs->p1;
          n3 = supernodefromaddr(eq, exs->size1, a, prog);
          if(!lvqueryval(blk->antileader_in, n3->index)) {
            lvinsert(blk->antileader_in, n3->index, valdup(exs));
            dipush(blk->antileader_in_list, n3->index);
          }
          break;
      }
    }
  }

  if(blk->antileader_in->keys != 0) {
    for(int i = 0; i < blk->antileader_in_list->length; i++) {
      VALUESTRUCT* exs = lvsearch(blk->antileader_in, blk->antileader_in_list->arr[i]);
      GVNNUM* n3 = opsearch(eq->ophash, exs);
      switch(exs->o) {
        default:
          assert(0);
        case DEALOC:
          continue;
        OPS_3_3ac
          if(!lvqueryval(antiin_users, exs->p1)) {
            lvinsert(antiin_users, exs->p1, dictor(4));
          }
          dipush(lvsearch(antiin_users, exs->p1), n3->index);
          if(!lvqueryval(antiin_users, exs->p2)) {
            lvinsert(antiin_users, exs->p2, dictor(4));
          }
          dipush(lvsearch(antiin_users, exs->p2), n3->index);
          break;
        OPS_2_3ac
        OPS_1_ASSIGN_3ac case ADDR_3: //these also should be killed?
          if(!lvqueryval(antiin_users, exs->p1)) {
            lvinsert(antiin_users, exs->p1, dictor(4));
          }
          dipush(lvsearch(antiin_users, exs->p1), n3->index);
          break;
      }
    }
  }

  if(blk->tmp_gen) {
    DYNINT* rmstack = dictor(8);
    for(int i = 0; i < blk->tmp_gen->length; i++) {
      long fullval = (long) blk->tmp_gen->arr[i];
      ADDRESS a;
      a.regnum = (unsigned int) fullval;
      GVNNUM* g = nodefromaddr(eq, downsize(fullval >> 32), a, prog);

      if(iiqueryval(blk->leader, g->index)) {
        long ex2rm = iisearch(blk->leader, g->index);
        if(a.regnum == ex2rm) {
          dipush(rmstack, g->index);
          while(rmstack->length > 0) {
            int removalind = dipop(rmstack);
            lvrmpaircfr(blk->antileader_in, removalind, free);
            //remove instance of removalind! more efficient way to do this?
            for(int i = 0; i < blk->antileader_in_list->length; i++) {
                if(blk->antileader_in_list->arr[i] == removalind) {
                    blk->antileader_in_list->arr[i] = -1;
                    break;
                }
            }
            DYNINT* tokill = lvsearch(antiin_users, removalind);
            if(tokill) rmstack = dimerge(rmstack, tokill);
            lvrmpair(antiin_users, removalind);
          }
        }
      }
    }

    int aiindex;
    int newlen = 0;
    for(aiindex = 0; aiindex < blk->antileader_in_list->length; aiindex++)  {
        if(blk->antileader_in_list->arr[aiindex] == -1)
            continue;
        blk->antileader_in_list->arr[newlen++] = blk->antileader_in_list->arr[aiindex];
    }
    blk->antileader_in_list->length = newlen;

    didtor(rmstack);
  }
  char changed = !oldanticout || blk->antileader_out->keys != oldanticout->keys || !oldanticin || blk->antileader_in->keys != oldanticin->keys;
  if(oldanticin) lvchtdtor(oldanticin, free);
  if(oldanticout) lvchtdtor(oldanticout, free);
  if(oldanticinlist) didtor(oldanticinlist);
  if(oldanticoutlist) didtor(oldanticoutlist);
  lvchtdtor(antiin_users, (void(*)(void*)) didtor);
  for(int i = 0; i < blk->pidominates->length; i++)
    changed |= antics(daget(blk->pidominates, i), prog, eq);
  return changed;
}

static void recdomins(BBLOCK* blk, int key, int value) {
  iiinsert(blk->leader, key, value);
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

      //we try to find which blocks have antileader sets that have some items present as leaders
      //in predecessor nodes, and others not present, these are as per the paper candidates for value generation
      for(int antiind = 0; antiind < blk->antileader_in_list->length; antiind++) {
        int antiint = blk->antileader_in_list->arr[antiind];
        GVNNUM* antilnode = daget(eq->uniq_vals, antiint);
        VALUESTRUCT* antil = lvsearch(blk->antileader_in, antiint);
        if(antil->o == INIT_3) continue;
        for(int j = 0; j < blk->inedges->length; j++) {
            BBLOCK* oblk = daget(blk->inedges, j);
            if(oblk->leader && iiqueryval(oblk->leader, antiint)) {
                dapush(stubbornblocks, oblk);
            }
        }

        if(stubbornblocks->length > 0 && stubbornblocks->length < blk->inedges->length) {
          int stubbornindex = 0;
          ADDRESS joins, reggie;
          joins.joins = malloc(blk->inedges->length * sizeof(FULLADDR));
          reggie.regnum = prog->regcnt++;
          VALUESTRUCT *antilantileader;
          //We find one authoritative type that this value should have
          for(int k = 0; ;k++) {
              antilantileader = daget(antilnode->equivs, k);
              assert(k < antilnode->equivs->length);
              if(antilantileader->o == INIT_3) break;
          }

          //create the PHI join node, to be populated later
          OPERATION* phi = ct_3ac_op2(PHI, ISCONST, joins, downsize(antilantileader->size1), reggie);

          //for each predecessor block, i.e. each value to be joined within the PHI node
          for(int j = 0; j < blk->inedges->length; j++) {
            BBLOCK* oblk = daget(blk->inedges, j);

            //if it's got the value already represented, just insert the value into the PHI
            if(stubbornindex < stubbornblocks->length && 
               oblk == daget(stubbornblocks, stubbornindex)) {
              stubbornindex++;
              assert(iiqueryval(oblk->leader, antiint));
              int stubbornval = iisearch(oblk->leader, antiint);
              joins.joins[j].addr_type = phi->dest_type;
              joins.joins[j].addr.regnum = stubbornval;
            } else {
            //if the value is not already represented, we need to hoist the operation used to generate it!
              VALUESTRUCT actionable = *antil;
              VALUESTRUCT genvalue; //holds the generated value, p2 is zeroed out for the case of op1
              genvalue.o = actionable.o;
              genvalue.p2 = 0;
              genvalue.size2 = 0;
              ADDRESS provisional;
              OPERATION* genop = malloc(sizeof(OPERATION));
              int leadreg;
              GVNNUM* operandnode;
              genop->opcode = actionable.o;
              genop->dest_type = phi->dest_type;
              genop->dest.regnum = prog->regcnt++;
              //we switch on the operation
              switch(actionable.o) {
                default:
                  assert(0);
                case INIT_3:
                  free(genop);
                  //somehow the variable was present when we thought it wasn't? Just insert the value like normal
                  joins.joins[j].addr_type = phi->dest_type;
                  joins.joins[j].addr.regnum = actionable.p1;
                  continue;
                OPS_3_3ac
                  //for an op3 we first look at the second source operand
                  operandnode = daget(eq->uniq_vals, actionable.p2);
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
                    int prevleader = iisearch(blk->leader, actionable.p2);
                    int origp2 = actionable.p2;
                    provisional.regnum = oblk->revtranslator ? iisearch(oblk->revtranslator, prevleader) : 0;
                    if(provisional.regnum) actionable.p2 = nodefromaddr(eq, phi->dest_type, provisional, prog)->index;
                    while(1) {
                      char contflag = 0;
                      int beginp2 = actionable.p2;
                      int equivind = 0;
                      DYNARR* equivlist = ((GVNNUM*) daget(eq->uniq_vals, actionable.p2))->equivs;
                      while(!(leadreg = iisearch(oblk->leader, actionable.p2))) {
                        VALUESTRUCT* vs;
                        do {
                          if(equivind >= equivlist->length) {
                              assert(beginp2 != origp2);
                              beginp2 = origp2;
                              actionable.p2 = beginp2;
                              contflag = 1;
                              break;
                          }
                          vs = daget(equivlist, equivind++);
                        } while(vs->o != INIT_3);
                        if(contflag) break;
                        provisional.regnum = oblk->revtranslator ? iisearch(oblk->revtranslator, vs->p1) : 0;
                        if(provisional.regnum) actionable.p2 = nodefromaddr(eq, phi->dest_type, provisional, prog)->index;
                      }
                      if(!contflag) break;
                    }
                    genop->addr1_type = genop->dest_type;
                    genvalue.p2 = genop->addr1.regnum = leadreg;
                    genvalue.size2 = supersize(genop->dest_type);
                  }
                __attribute__((fallthrough));
                OPS_2_3ac
                  //for an op2 and an op3 we look at the first source operand now
                  operandnode = daget(eq->uniq_vals, actionable.p1);
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
                    int prevleader = iisearch(blk->leader, actionable.p1);
                    int origp1 = actionable.p1;
                    provisional.regnum = oblk->revtranslator ? iisearch(oblk->revtranslator, prevleader): 0;
                    if(provisional.regnum) actionable.p1 = nodefromaddr(eq, phi->dest_type, provisional, prog)->index;
                    while(1) {
                      char contflag = 0;
                      int beginp1 = actionable.p1;
                      int equivind = 0;
                      DYNARR* equivlist = ((GVNNUM*) daget(eq->uniq_vals, actionable.p1))->equivs;
                      while(!(leadreg = iisearch(oblk->leader, actionable.p1))) {
                        VALUESTRUCT* vs;
                        //Done: This used to be subject to a bug wherein we too eagerly translated the values across phi.
                        //This also used to not assign the new generated calculations  to the same value. These bugs have been fixed.
                        do {
                          if(equivind >= equivlist->length) {
                              assert(beginp1 != origp1);
                              beginp1 = origp1;
                              actionable.p1 = beginp1;
                              contflag = 1;
                              break;
                          }
                          vs = daget(equivlist, equivind++);
                        } while(vs->o != INIT_3);
                        if(contflag) break;
                        provisional.regnum = oblk->revtranslator ? iisearch(oblk->revtranslator, vs->p1) : 0;
                        if(provisional.regnum) actionable.p1 = nodefromaddr(eq, phi->dest_type, provisional, prog)->index;
                      }
                      if(!contflag) break;
                    }
                    genop->addr0_type = genop->dest_type;
                    genvalue.p1 = genop->addr0.regnum = leadreg;
                    genvalue.size1 = supersize(genop->dest_type);
                  }
                  break;
              }

              if(!oblk->operations) {
                oblk->operations = dactor(4);
              }
              dapush(oblk->operations, genop);

              if(antilnode->hasconst == NOCONST) {
                assert(!iiqueryval(oblk->leader, antiint));

                if(!oblk->translator) oblk->translator = iihtctor();
                if(!oblk->revtranslator) oblk->revtranslator = iihtctor();
                iiinsert(oblk->revtranslator, phi->dest.regnum, genop->dest.regnum);
                iiinsert(oblk->translator, genop->dest.regnum, phi->dest.regnum);
                iiinsert(oblk->leader, antiint, genop->dest.regnum);
                VALUESTRUCT generated_value = {INIT_3, genop->dest.regnum, 0, supersize(genop->dest_type), 0};
                opinsert(eq->ophash, &generated_value, antilnode);
                opinsert(eq->ophash, &genvalue, antilnode);
              }

              //insert calculation of value here in predecessor block
              FULLADDR join = {genop->dest_type, genop->dest};
              joins.joins[j] = join;
            }
          }
          //insert phi at top of block
          if(!blk->operations) {
            blk->operations = dactor(4);
          } 
          dainsertat(blk->operations, 0, phi);
          changed = 1;

          recdomins(blk, antiint, phi->dest.regnum);
          VALUESTRUCT* joinedval = ctvalstruct(INIT_3, phi->dest.regnum, 0, supersize(phi->dest_type), 0);
          opinsert(eq->ophash, joinedval, antilnode);
          dapush(antilnode->equivs, joinedval);

          lvrmpaircfr(blk->antileader_in, antiint, free);
          blk->antileader_in_list->arr[antiind] = -1;
        }

        stubbornblocks->length = 0;
      }

      int newlen = 0;
      int aiindex;
      for(aiindex = 0; aiindex < blk->antileader_in_list->length; aiindex++) {
          if(blk->antileader_in_list->arr[aiindex] == -1)
              continue;
          blk->antileader_in_list->arr[newlen++] = blk->antileader_in_list->arr[aiindex];
      }
      blk->antileader_in_list->length = newlen;
    }
  }
  dadtor(stubbornblocks);
  return changed;
}

//Run the GVNPRE algorithm on the code to eliminate recalculations of value and 
//partial redundancies (as well as factor out loop invariants)
void gvn(PROGRAM* prog) {
  BBLOCK* first = daget(prog->allblocks, 0);
  EQONTAINER* eq = cteq(prog);
  IIHASHTABLE* h1 = first->leader = iihtctor();
  gensall(prog, eq, first);
  iihtdtor(h1);
  first->pidominates = dactor(0);
  while(antics(prog->finalblock, prog, eq)) ;
  //buildsets calculated
  while(hoist(prog, eq)) ;
  replacegvn(eq, prog);
  freeq(eq);
  prog->pdone |= GVN;
  return;
}
//https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/gvn_sas04.pdf
//https://www.cs.purdue.edu/homes/hosking/papers/cc04.pdf

//Convert code out of SSA form (currently we don't care to do so, and will register allocate right from SSA)
void ssaout(PROGRAM* prog) {
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    if(blk->operations && blk->operations->length) {
      for(int opind = 0; opind < blk->operations->length; opind++) {
        OPERATION* phiop = blk->operations->arr[opind];
        if(phiop->opcode != PHI)
          break;
        FULLADDR paraddr;
        paraddr.addr_type = phiop->dest_type & GENREGMASK;
        paraddr.addr = phiop->dest;
        paraddr.addr.ssaind++;
        for(int j = 0; j < blk->inedges->length; j++) {
           BBLOCK* predblock = daget(blk->inedges, j);
           FULLADDR fadradr = phiop->addr0.joins[j];
           if(!predblock->operations) {
             predblock->operations = dactor(4);
           }
           dapush(predblock->operations, ct_3ac_op2(MOV_3, fadradr.addr_type, fadradr.addr, paraddr.addr_type, paraddr.addr));
        }
        free(phiop->addr0.joins);
        phiop->opcode = MOV_3;
        phiop->addr0_type = paraddr.addr_type;
        phiop->addr0 = paraddr.addr;
      }
    }
  }
}
#undef X
