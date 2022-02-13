#include "reg.h"
#define X(op) case op:

enum reguse callreg[6] = {DI, SI, DX, CX, R8, R9};


const char* ireg64[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
const char* ireg32[] = {"eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
const char* ireg16[] = {"ax", "bx", "cx", "dx", "di", "si", "bp", "sp", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"};
const char* ireg8[] = {"al", "bl", "cl", "dl", "dil", "sil", "bpl", "spl", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"};
const char* freg128[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
const char* freg256[] = {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};

//Returns whether a reg is live at the very beginning of this block, before any operations have been executed yet
static char islive_in(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, BITFIELD* varbs, int varnum) {
  //if it's not the definition block, it's live_in provided it's live
  //if it is the definition block, it's only live in if it's joined
  //this is not true for addrsvar, variables where we conservatively say it's live
  //if and only if it's live in a predecessor of the defining block
  if(usedefchains[varnum] && daget(usedefchains[varnum], 0) == blk) {
    int numpred = blk->inedges->length;
    if(usedefchains[varnum]->length == -1) {
      for(int i = 0; i < numpred; i++) {
        BBLOCK* predblk = daget(blk->inedges, i);
        if(bfget(varbs[varnum], predblk->domind))
          return 1;
      }
    } else {
      assert(blk->lastop);//if it's the defining block there must be an op
      OPERATION* op = blk->firstop;
      while(op->opcode == PHI) {
        for(int i = 0; i < numpred; i++) {
          FULLADDR f = op->addr0.joins[i];
          //deref is not necessary because derefs should never be used in phis
          if(!(f.addr_type & (ISDEREF | GARBAGEVAL | ISLABEL | ISCONST))) {//i.e. it must be a register
            if(f.addr.regnum == (unsigned) varnum)
              return 1;
          }
        }
        if(op == blk->lastop) break;
        op = op->nextop;
      }
    }
    return 0;
  }
  return bfget(varbs[varnum], blk->domind);
}

//Returns whether a reg is live at the very end of a block, after all operations have been executed
static char islive_out(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, BITFIELD* varbs, int varnum) {
  //here we simply check whether the block is live in any successor
  return bfget(varbs[varnum], blk->nextblock->domind) || (blk->branchblock && bfget(varbs[varnum], blk->branchblock->domind));
}

void lastuse(PROGRAM* prog, DYNARR** chains, BITFIELD* varbs);

//populate a bitfield of blocks dominated by a given block, to prevent recalculation and make liveness easier
static void bfdtreepopulate(BBLOCK* blk, BITFIELD bf) {
    bfset(bf, blk->domind);
    if(blk->idominates)
      for(int i = 0; i < blk->idominates->length; i++)
        bfdtreepopulate(daget(blk->idominates, i), bf);
}

//populate all blocks in the dominance subtree of the definition where the variable is live
//In order to do this, for each of the uses in the use def chain, for each of the variables
//find recursively all predecessor blocks that lie within the dominance tree
static void updompop(BBLOCK* liveblk, BITFIELD domtreeset, BITFIELD topop) {
  //I THINK only one of these cases is necessary?
  if(!bfget(domtreeset, liveblk->domind)) return;
  if(!bfget(topop, liveblk->domind)) return;
  bfset(topop, liveblk->domind);
  for(int i = 0; i < liveblk->inedges->length; i++)
      updompop(daget(liveblk->inedges, i), domtreeset, topop);
}

//Populates liveness information for a PROGRAM, determining which variables have livenesses that overlap and register allocating
static void liveness_populate(PROGRAM* prog, DYNARR**chains, BITFIELD* varbs) {
  //allocate a bitfield for each block which should be NULL to start with and will be filled lazily with the dominance tree of that block
  BITFIELD* domtreeset = calloc(sizeof(BITFIELD), prog->allblocks->length);
  for(unsigned int i = 0; i < prog->regcnt; i++) {
    DYNARR* localchain = chains[i];
    if(!localchain) continue;
    BBLOCK* defblk = daget(localchain, 0);
    assert(defblk);
    BITFIELD dombf = domtreeset[defblk->domind];
    if(dombf == NULL) {
      dombf = domtreeset[defblk->domind] = bfalloc(prog->allblocks->length);
      bfdtreepopulate(defblk, dombf);
    }
    if(localchain->length != -1L) {
      //normal variable
      BITFIELD varb = bfalloc(prog->allblocks->length);
      for(int j = 1; j < localchain->length; j++) {
        updompop(daget(localchain, j), dombf, varb);
      }
      varbs[i] = varb;
    } else {
      //addrsvar case! consider it live from every point in its dominator tree
      varbs[i] = bfclone(dombf, prog->allblocks->length);
    }
    //for both of these cases, maybe we want to exclude the defblk from the live_in set?
  }

  for(int i = 0; i < prog->allblocks->length; i++)
      if(domtreeset[i]) free(domtreeset[i]);
  free(domtreeset);
}

static void adjmatrixset(BITFIELD bf, int dim, int reg1, int reg2) {
  bfset(bf, reg1 * dim + reg2);
  bfset(bf, reg2 * dim + reg1);
}

static BITFIELD genadjmatrix(PROGRAM* prog, DYNARR** chains, BITFIELD* varbs) {
  BITFIELD adjmatrix = bfalloc(prog->regcnt * prog->regcnt);
  //for now let's handle things inefficiently and say if they're live at all in the same block it counts
  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    for(unsigned int i = 0; i < prog->regcnt; i++) {
      if(varbs[i] && bfget(varbs[i], blockind)) {
        for(unsigned int j = i + 1; j < prog->regcnt; j++) {
          if(varbs[j] && bfget(varbs[j], blockind)) {
              adjmatrixset(adjmatrix, prog->regcnt, i, j);
          }
        }
      }
    }
  }
  return adjmatrix;
}

void liveness(PROGRAM* prog) {
  //first we should try to rename variables downward
  DYNARR** usedefchains = calloc(prog->regcnt, sizeof(DYNARR*));
  //first calculate the use-def chains
  LOOPALLBLOCKS(
    OPARGCASES(
      if(!(op->addr0_type & (ISDEREF | ISLABEL | ISCONST | GARBAGEVAL))) {
        DYNARR* chain;
        unsigned int reg = op->addr0.regnum;
        assert(reg < prog->regcnt);
        assert((chain = usedefchains[reg])); //forward use that is not a phi
        if(chain->length != -1) {//if it's not an addrsvar
          if(dapeek(chain) != blk)
            dapush(chain, blk); //prevent adjacent duplicates
        }
      },
      if(!(op->addr1_type & (ISDEREF | ISLABEL | ISCONST))) {
        DYNARR* chain;
        unsigned int reg = op->addr1.regnum;
        assert(reg < prog->regcnt);
        assert((chain = usedefchains[reg])); //forward use that is not a phi
        if(chain->length != -1) {//if it's not an addrsvar
          if(dapeek(chain) != blk)
            dapush(chain, blk); //prevent adjacent duplicates
        }
      },
      if(!(op->dest_type & (ISDEREF | ISLABEL))) {
        DYNARR* chain;
        unsigned int reg = op->dest.regnum;
        assert(reg < prog->regcnt);

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
      },
      if(!(phijoinaddr->addr_type & (ISDEREF | ISLABEL | ISCONST))) {
        DYNARR* chain;
        unsigned int reg = phijoinaddr->addr.regnum;
        assert(reg < prog->regcnt);
        if((chain = usedefchains[reg])) {
          if(chain->length != -1) {//if it's not an addrsvar
            if(dapeek(chain) != blk) {
              dapush(chain, blk); //prevent adjacent duplicates
            }
          }
        } else {
          chain = usedefchains[reg] = dactor(8);
          chain->length = 2;
          chain->arr[0] = NULL;
          chain->arr[1] = blk;
        }
      }
    )
  )

  BITFIELD* varbs = calloc(sizeof(BITFIELD), prog->regcnt);
  liveness_populate(prog, usedefchains, varbs);

  lastuse(prog, usedefchains, varbs);

  BITFIELD adjmatrix = genadjmatrix(prog, usedefchains, varbs);

  for(unsigned int i = 0; i < prog->regcnt; i++) {
      assert(varbs[i] || !usedefchains[i]);
      free(varbs[i]);
  }
  //generate liveness matrix
  free(varbs);
  free(adjmatrix);


  //LOOPALLBLOCKS(
  //  OPARGCASES(
  //    ,
  //    ,
  //    if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL | ISDEREF)) && op->dest_type & LASTUSE &&  op->opcode != CALL_3) {
  //      if(op->opcode == PHI) free(op->addr0.joins);
  //      op->opcode = NOP_3;
  //    }
  //    ,
  //    (void) phijoinaddr;
  //  )
  //)

  //liveadjmatrix(prog, usedefchains);

  for(unsigned int i = 0; i < prog->regcnt; i++)
    if(usedefchains[i]) dadtor(usedefchains[i]);
  free(usedefchains);
}

//(currently nonfunctional) Annotates uses of each reg in each operation if that variable is not used at any program point that can be reached after that
//This excludes when the very next use would be the definition of that reg except for the case where that definition is a phi statement
//which takes in the reg itself as the parameter for a reachable predecessor block.
void lastuse(PROGRAM* prog, DYNARR** chains, BITFIELD* varbs) {
  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    BBLOCK* blk = daget(prog->allblocks, blockind);
    HASHTABLE* reglasts = htctor();
    LOOPOPS(
      OPARGCASES(
        if(!(op->addr0_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, varbs, op->addr0.regnum))
          fixedinsert(reglasts, op->addr0.regnum, &op->addr0_type);
        ,
        if(!(op->addr1_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, varbs, op->addr1.regnum))
          fixedinsert(reglasts, op->addr1.regnum, &op->addr1_type);
        ,
        if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, varbs, op->dest.regnum))
          fixedinsert(reglasts, op->dest.regnum, &op->dest_type); //uh oh
        ,
        if(!(phijoinaddr->addr_type & (ISLABEL | ISCONST)) && !islive_out(prog, blk, chains, varbs, phijoinaddr->addr.regnum))
          fixedinsert(reglasts, phijoinaddr->addr.regnum, &phijoinaddr->addr_type); //uh oh
      )
    )
    DYNARR* da = htfpairs(reglasts);
    for(int i = 0; i < da->length; i++) {
      HASHPAIR* hp = daget(da, i);
      *((ADDRTYPE*) hp->value) |= LASTUSE;
    }
    dadtor(da);
    fhtdtor(reglasts);
  }
}

//handle long doubles same as doubles
//https://compilers.cs.uni-saarland.de/projects/ssara/hack_ssara_ssa09.pdf
//https://www.rw.cdl.uni-saarland.de/people/grund/private/papers/cgo08-liveness.pdf

void regalloc(PROGRAM* prog, BITFIELD adjmatrix) {
  //start out by coloring based on fewest collisions?!
  //attempt color
  //spill if fail--where/how to spill, with belady's algorithm?
  //how coalesce and retry?
}
#undef X
