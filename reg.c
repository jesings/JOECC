#include "reg.h"
#include "codegen.h"
#define X(op) case op:

enum reguse callreg[6] = {DI, SI, DX, CX, R8, R9};

const char* ireg64[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
const char* ireg32[] = {"eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
const char* ireg16[] = {"ax", "bx", "cx", "dx", "di", "si", "bp", "sp", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"};
const char* ireg8[] = {"al", "bl", "cl", "dl", "dil", "sil", "bpl", "spl", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"};
const char* freg128[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
const char* freg256[] = {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};

static void printvarbs(int dim, int count, IHASHSET** varbs) {
  for(int i = 0; i < count; i++) {
    if(varbs[i]) {
      printf("block %d has vars: ", i);
      DYNINT* varberinos = isetelems(varbs[i]);
      for(int j = 0; j < varberinos->length; j++) {
        printf("%d, ", varberinos->arr[j]);//i, j or j, i doesn't matter because it's symmetric
      }
      didtor(varberinos);
      putchar('\n');
    }
  }
}
static void printusedefs(int count, DYNARR** usedefchains) {
  for(int i = 0; i < count; i++) {
    DYNARR* chain = usedefchains[i];
    if(!chain)
      continue;
    printf("variable %d use/def chain: ", i);
    for(int j = 0; j < chain->length; j++) {
      printf("%d -> ", ((BBLOCK*) chain->arr[j])->domind);
    }
    putchar('\n');
  }
}

//Returns whether a reg is live at the very beginning of this block, before any operations have been executed yet
static char islive_in(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, IHASHSET** varbs, int varnum) {
  //if it's not the definition block, it's live_in provided it's live
  //if it is the definition block, it's only live in if it's joined
  //this is not true for addrsvar, variables where we conservatively say it's live
  //if and only if it's live in a predecessor of the defining block
  if(usedefchains[varnum] && daget(usedefchains[varnum], 0) == blk) {
    int numpred = blk->inedges->length;
    if(usedefchains[varnum]->length == -1) {
      for(int i = 0; i < numpred; i++) {
        BBLOCK* predblk = daget(blk->inedges, i);
        if(isetcontains(varbs[predblk->domind], varnum))
          return 1;
      }
    } else {
      assert(blk->operations && blk->operations->length);//if it's the defining block there must be an op
      for(int opind = 0; opind < blk->operations->length; opind++) {
        OPERATION* op = blk->operations->arr[opind];
        for(int i = 0; i < numpred; i++) {
          FULLADDR f = op->addr0.joins[i];
          //deref is not necessary because derefs should never be used in phis
          if(!(f.addr_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {//i.e. it must be a register
            if(f.addr.regnum == (unsigned) varnum)
              return 1;
          }
        }
      }
    }
    return 0;
  }

  //fall through here if it's not the definition block
  return isetcontains(varbs[blk->domind], varnum);
}

//Returns whether a reg is live at the very end of a block, after all operations have been executed
static char islive_out(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, IHASHSET** varbs, int varnum) {
  //here we simply check whether the block is live in any successor
  char nextcheck = varbs[blk->nextblock->domind] && isetcontains(varbs[blk->nextblock->domind], varnum);
  if(nextcheck) return 1;
  if(blk->branchblock) {
    //no possible join in the next block in this case because of how we've broken the CFG into simplified form
    return varbs[blk->branchblock->domind] && isetcontains(varbs[blk->branchblock->domind], varnum);
  } else {
    if(!blk->nextblock->operations) return 0;
    int joindex = -1;
    while(daget(blk->nextblock->inedges, ++joindex) != blk) ;
    for(int i = 0; i < blk->nextblock->operations->length; i++) {
      OPERATION* op = daget(blk->nextblock->operations, i);
      if(op->opcode != PHI) break;
      FULLADDR ad = op->addr0.joins[joindex];
      if(!(ad.addr_type & (ISCONST | ISLABEL)) && ad.addr.regnum == (unsigned) varnum) return 1;
    }
    return 0;
  }
}

void lastuse(PROGRAM* prog, DYNARR** chains, IHASHSET** varbs);

//populate a bitfield of blocks dominated by a given block, to prevent recalculation and make liveness easier
static void bfdtreepopulate(BBLOCK* blk, IHASHSET* bf) {
    isetinsert(bf, blk->domind);
    if(blk->idominates)
      for(int i = 0; i < blk->idominates->length; i++)
        bfdtreepopulate(daget(blk->idominates, i), bf);
}

//populate all blocks in the dominance subtree of the definition where the variable is live
//In order to do this, for each of the uses in the use def chain, for each of the variables
//find recursively all predecessor blocks that lie within the dominance tree
static void updompop(BBLOCK* liveblk, IHASHSET* domtreeset, IHASHSET** varbs, int regnum) {
  if(!isetcontains(domtreeset, liveblk->domind)) return;
  IHASHSET* curset = varbs[liveblk->domind];
  if(!curset) curset = varbs[liveblk->domind] = isetctor(64);
  if(isetcontains(curset, regnum)) return;
  isetinsert(curset, regnum);
  for(int i = 0; i < liveblk->inedges->length; i++)
      updompop(daget(liveblk->inedges, i), domtreeset, varbs, regnum);
}

//Populates liveness information for a PROGRAM, determining which variables have livenesses that overlap and register allocating
static void liveness_populate(PROGRAM* prog, DYNARR**chains, IHASHSET** varbs) {
  //allocate a bitfield for each block which should be NULL to start with and will be filled lazily with the dominance tree of that block
  int pal = prog->allblocks->length;
  IHASHSET** domtreeset = calloc(pal, sizeof(IHASHSET*));
  for(unsigned int i = 1; i < prog->regcnt; i++) {
    DYNARR* localchain = chains[i];
    if(!localchain) continue;
    BBLOCK* defblk = daget(localchain, 0);
    assert(defblk);
    IHASHSET* domset = domtreeset[defblk->domind];
    if(domset == NULL) {
      domset = domtreeset[defblk->domind] = isetctor(64);
      bfdtreepopulate(defblk, domset);
    }
    if(localchain->length != -1L) {
      //normal variable
      for(int j = 1; j < localchain->length; j++) {
        //updompop differently for phis? how to mark as phi
        updompop(daget(localchain, j), domset, varbs, i);
      }
    } else {
      //addrsvar case! consider it live from every point in its dominator tree
      DYNINT* blocklives = isetelems(domset);
      for(int j = 0; j < blocklives->length; j++) {
        IHASHSET* ih = varbs[blocklives->arr[j]];
        if(!ih) ih = varbs[blocklives->arr[j]] = isetctor(64);
        isetinsert(ih, i);
      }
      didtor(blocklives);
    }
    //for both of these cases, maybe we want to exclude the defblk from the live_in set?
  }

  for(int i = 0; i < pal; i++)
      if(domtreeset[i]) isetdtor(domtreeset[i]);
  free(domtreeset);
}

static BITFIELD genadjmatrix(PROGRAM* prog, DYNARR** chains, IHASHSET** varbs, short* clobberers) {
  BITFIELD adjmatrix = bfalloc(prog->regcnt * prog->regcnt);

  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    BBLOCK* blk = daget(prog->allblocks, blockind);
    DYNINT* blockers;
    if(varbs[blockind]) {
      blockers = isetelems(varbs[blockind]);
      //mark off the variables live at the start of a block
      for(int iind = 0; iind < blockers->length; iind++) {
        int i = blockers->arr[iind]; //this is a block where it is live
        for(int jind = iind+1; jind < blockers->length; jind++) {
          int j = blockers->arr[jind];
          bfset(adjmatrix, prog->regcnt * i + j);
          bfset(adjmatrix, prog->regcnt * j + i);
        }
      }
    } else {
      blockers = dictor(8);
    }
    if(blk->operations) {
      for(int opind = 0; opind < blk->operations->length; opind++) {
        OPERATION* op = daget(blk->operations, opind);
        short allclobs = op2op[op->opcode].fixedclobbers | op2op[op->opcode].retloc;
        OPARGCASES(
          if(op->addr0_type & LASTUSE) {
            diremove_swap(blockers, op->addr0.regnum);
          }
          ,
          if(op->addr1_type & LASTUSE) {
            diremove_swap(blockers, op->addr0.regnum);
          }
          ,
          if(op->dest_type & LASTUSE) {
            diremove_swap(blockers, op->addr0.regnum);
          } else if(!(op->dest_type & (ISLABEL | ISDEREF | ADDRSVAR))) {
            for(int i = 0; i < blockers->length; i++) {
              bfset(adjmatrix, prog->regcnt * blockers->arr[i] + op->dest.regnum);
              bfset(adjmatrix, prog->regcnt * op->dest.regnum + blockers->arr[i]);
            }
            dipush(blockers, op->dest.regnum);
          }
          ,
          (void) phijoinaddr;
        )
        if(allclobs) {
          for(int i = 0; i < blockers->length; i++) {
            clobberers[blockers->arr[i]] |= allclobs;
          }
        }
      }
    }
    didtor(blockers);
  }

  return adjmatrix;
}

void liveness(PROGRAM* prog) {
  //first we should try to rename variables downward
  DYNARR** usedefchains = calloc(prog->regcnt, sizeof(DYNARR*));
  //first calculate the use-def chains
  LOOPALLBLOCKS(
    OPARGCASES(
      if(!(op->addr0_type & (ISLABEL | ISCONST | GARBAGEVAL))) {
        unsigned int reg = op->addr0.regnum;
        DYNARR* chain = usedefchains[reg];
        assert(reg < prog->regcnt);
        assert(chain); //forward use that is not a phi
        if(chain->length != -1) {//if it's not an addrsvar
          if(dapeek(chain) != blk)
            dapush(chain, blk); //prevent adjacent duplicates
        }
      },
      if(!(op->addr1_type & (ISLABEL | ISCONST))) {
        unsigned int reg = op->addr1.regnum;
        DYNARR* chain = usedefchains[reg];
        assert(reg < prog->regcnt);
        assert(chain); //forward use that is not a phi
        if(chain->length != -1) {//if it's not an addrsvar
          if(dapeek(chain) != blk)
            dapush(chain, blk); //prevent adjacent duplicates
        }
      },
      if(!(op->dest_type & ISLABEL)) {
        DYNARR* chain;
        unsigned int reg = op->dest.regnum;
        assert(reg < prog->regcnt);

        if(!(op->dest_type & ISDEREF)) {
          if(op->dest_type & ADDRSVAR) {
            assert(!usedefchains[reg]);
            chain = usedefchains[reg] = dactor(1);
            usedefchains[reg]->length = -1;
            usedefchains[reg]->arr[0] = blk;
          } else {
            chain = usedefchains[reg];
            if(chain) {
              if(chain->length == -1) break;
              assert(daget(chain, 0) == NULL);
            } else {
              chain = usedefchains[reg] = dactor(8);
              chain->length = 1;
            }
            chain->arr[0] = blk;
          }
        } else {
          chain = usedefchains[reg];
          assert(chain); //forward use that is not a phi
          if(chain->length != -1) {//if it's not an addrsvar
            if(dapeek(chain) != blk)
              dapush(chain, blk); //prevent adjacent duplicates
          }
        }
      },
      //phis count as uses in the previous block!
      if(!(phijoinaddr->addr_type & (ISLABEL | ISCONST))) {
        DYNARR* chain;
        BBLOCK* prebblock = daget(blk->inedges, phiindex);
        unsigned int reg = phijoinaddr->addr.regnum;
        assert(reg < prog->regcnt);
        if((chain = usedefchains[reg])) {
          if(chain->length != -1) {//if it's not an addrsvar
            for(int i = chain->length - 1; i >= 0; i--) {
              BBLOCK* ith = daget(chain, i);
              if(ith == NULL) {
                assert(i == 0);
                dainsertat(chain, 1, blk);
                break;
              }
              if(ith->domind < prebblock->domind) {
                dainsertat(chain, i + 1, blk);
                break;
              } else if (ith == prebblock) {
                break; //prevent duplicates
              }
            }
          }
        } else {
          chain = usedefchains[reg] = dactor(8);
          chain->length = 2;
          chain->arr[0] = NULL;
          chain->arr[1] = prebblock;
        }
      }
    )
  )

  //remove all defines with no uses
  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    BBLOCK* blk = daget(prog->allblocks, blockind);
    LOOPOPS(
      OPARGCASES(
        , ,
        //TODO: look through LHS and for each addr, check if a use needs to be removed...
        if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL))) {
          DYNARR* chain = usedefchains[op->dest.regnum];
          if(chain->length == 1) {
            if(op->opcode == PHI)
              free(op->addr0.joins);
            op->opcode = NOP_3;
          }
          dadtor(usedefchains[op->dest.regnum]);
          usedefchains[op->dest.regnum] = NULL;
        }
        ,
        (void) phijoinaddr;
      )
    )
  }

  //vabs is an array of hash sets, indexed by block numbers, of variables live on entry to that block
  IHASHSET** varbs = calloc(prog->allblocks->length, sizeof(IHASHSET*));

  liveness_populate(prog, usedefchains, varbs);

  lastuse(prog, usedefchains, varbs);

  short* clobberers = malloc(sizeof(short) * prog->regcnt);
  BITFIELD adjmatrix = genadjmatrix(prog, usedefchains, varbs, clobberers);
  //printusedefs(prog->regcnt, usedefchains);
  //printvarbs(prog->regcnt, prog->allblocks->length, varbs);
  //printadjmatrix(prog->regcnt, adjmatrix);

  for(int i = 0; i < prog->allblocks->length; i++) {
      if(varbs[i])
          isetdtor(varbs[i]);
  }
  //generate liveness matrix
  regalloc(prog, adjmatrix);
  free(varbs);
  free(adjmatrix);
  free(clobberers);

  for(unsigned int i = 0; i < prog->regcnt; i++)
    if(usedefchains[i]) dadtor(usedefchains[i]);
  free(usedefchains);
}

//Annotates uses of each reg in each operation if that variable is not used at any program point that can be reached after that
//This excludes when the very next use would be the definition of that reg except for the case where that definition is a phi statement
//which takes in the reg itself as the parameter for a reachable predecessor block.
//These liveness checks should be pretty cheap as in most cases they are 1 or 2 lookups
void lastuse(PROGRAM* prog, DYNARR** chains, IHASHSET** varbs) {
  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    BBLOCK* blk = daget(prog->allblocks, blockind);
    LVHASHTABLE* reglasts = lvchtctor(32);
    LOOPOPS(
      OPARGCASES(
        if(!(op->addr0_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, varbs, op->addr0.regnum))
          lvinsert(reglasts, op->addr0.regnum, &op->addr0_type);
        ,
        if(!(op->addr1_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, varbs, op->addr1.regnum))
          lvinsert(reglasts, op->addr1.regnum, &op->addr1_type);
        ,
        if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, varbs, op->dest.regnum))
          lvinsert(reglasts, op->dest.regnum, &op->dest_type); //if this is the case we'll free! then maybe we need to consider what happens if we entirely get rid of a variable?
        ,
        if(!(phijoinaddr->addr_type & (ISLABEL | ISCONST)) && !islive_out(prog, blk, chains, varbs, phijoinaddr->addr.regnum))
          lvinsert(reglasts, phijoinaddr->addr.regnum, &phijoinaddr->addr_type);
      )
    )
    DYNARR* da = lvhtpairs(reglasts);
    for(int i = 0; i < da->length; i++) {
      LVHASHPAIR* hp = daget(da, i);
      if(chains[hp->key]->length != -1) { //if it's not an addrsvar
        ADDRTYPE* ad = hp->value;
        *ad |= LASTUSE;
      }
    }
    dadtor(da);
    lvhtdtor(reglasts);
  }
}

//handle long doubles same as doubles
//https://compilers.cs.uni-saarland.de/projects/ssara/hack_ssara_ssa09.pdf
//https://www.rw.cdl.uni-saarland.de/people/grund/private/papers/cgo08-liveness.pdf

static int comparcollisions(const void* v1, const void* v2, void* cb) {
  int i1 = *(const int*) v1;
  int i2 = *(const int*) v2;
  int* comparbank = cb;
  if(comparbank[i1] > comparbank[i2]) {
      return 1;
  } else if(comparbank[i1] == comparbank[i2]) {
      return 0;
  } else {
      return -1;
  }
}

void regalloc(PROGRAM* prog, BITFIELD adjmatrix) {
  //start out by coloring based on fewest collisions?!
  //attempt color
  //spill if fail--where/how to spill, with belady's algorithm?
  //how coalesce and retry?
  char colors[prog->regcnt];
  int orderedcollisions[prog->regcnt];
  memset(colors, -1, prog->regcnt);
  memset(orderedcollisions, 0, prog->regcnt * sizeof(int));
  for(unsigned int i = 0; i < prog->regcnt; i++) {
    for(unsigned int j = 0; j < prog->regcnt; j++) {
      if(bfget(adjmatrix, i * prog->regcnt + j)) {
          orderedcollisions[i] += 1;
      }
    }
  }
  int colliders[prog->regcnt];
  for(unsigned int i = 0; i < prog->regcnt; i++) {
    colliders[i] = i;
  }
  qsort_r(colliders, prog->regcnt, sizeof(int), &comparcollisions, colliders);

  char colfail = 0;
  //now try to color!
  for(unsigned int i = 0; i < prog->regcnt; i++) {
    short posscolors = 0;
    for(unsigned int j = 0; j < prog->regcnt; j++) {
      if(bfget(adjmatrix, i * prog->regcnt + j)) {
        if(colors[j] != -1)
          posscolors |= 1 << colors[j];
      }
    }
    short col = 1;
    for(char colb = 0; colb < 16; colb++) {
      if(!(col & posscolors)) {
        colors[i] = colb;
        break;
      }
      col <<= 1;
    }
    if(col == 0) {
        colfail = 1;
        break;
    }
  }
  if(colfail) {
      printf("coloring failed!");
  } else {
      printf("coloring succeeded!");
  }
}

void printadjmatrix(int dim, BITFIELD adjmatrix) {
  for(int i = 0; i < dim; i++) {
    for(int j = 0; j < dim; j++) {
      putchar(bfget(adjmatrix, i*dim + j) ? ' ' : 'X');//i, j or j, i doesn't matter because it's symmetric
    }
    putchar('\n');
  }
}
#undef X
