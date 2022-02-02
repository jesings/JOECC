#include "reg.h"
#define X(op) case op:

enum reguse callreg[6] = {DI, SI, DX, CX, R8, R9};


const char* ireg64[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
const char* ireg32[] = {"eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
const char* ireg16[] = {"ax", "bx", "cx", "dx", "di", "si", "bp", "sp", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"};
const char* ireg8[] = {"al", "bl", "cl", "dl", "dil", "sil", "bpl", "spl", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"};
const char* freg128[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
const char* freg256[] = {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};
static char islive_in(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, int varnum) {
  return 0;
}
static char islive_out(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, int varnum) {
  return 0;
}

void lastuse(PROGRAM* prog, DYNARR** chains);

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

static void liveness_populate(PROGRAM* prog, DYNARR**chains, BITFIELD* varbs) {
    //allocate a bitfield for each block which should be NULL to start with and will be filled lazily with the dominance tree of that block
    BITFIELD* domtreeset = calloc(sizeof(BITFIELD), prog->allblocks->length);
    for(unsigned int i = 0; i < prog->regcnt; i++) {
      DYNARR* localchain = chains[i];
      if(!localchain) continue;
      assert(localchain != (void*) -1L); //forward use that is never resolved!
      BBLOCK* defblk = daget(localchain, 0);
      assert(defblk);
      BITFIELD dombf = domtreeset[defblk->domind];
      if(dombf == NULL) 
       dombf = domtreeset[defblk->domind] = bfalloc(prog->allblocks->length);
      bfdtreepopulate(defblk, dombf);
      BITFIELD varb = bfalloc(prog->allblocks->length);
      for(int j = 1; j < localchain->length; j++) {
        updompop(daget(localchain, j), dombf, varb);
      }
      varbs[i] = varb;
    }

    for(int i = 0; i < prog->allblocks->length; i++)
        if(domtreeset[i]) free(domtreeset[i]);
    free(domtreeset);
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
        if(chain != (DYNARR*) -1) {
          if(dapeek(chain) != blk)
            dapush(chain, blk); //prevent adjacent duplicates
        }
      },
      if(!(op->addr1_type & (ISDEREF | ISLABEL | ISCONST))) {
        DYNARR* chain;
        unsigned int reg = op->addr1.regnum;
        assert(reg < prog->regcnt);
        assert((chain = usedefchains[reg])); //forward use that is not a phi
        if(chain != (DYNARR*) -1) {
          if(dapeek(chain) != blk)
            dapush(chain, blk); //prevent adjacent duplicates
        }
      },
      if(!(op->dest_type & (ISDEREF | ISLABEL))) {
        DYNARR* chain;
        unsigned int reg = op->dest.regnum;
        assert(reg < prog->regcnt);

        if(op->dest_type & ADDRSVAR) {
          usedefchains[reg] = (DYNARR*) -1;
        } else {
          chain = usedefchains[reg];
          if(chain != (DYNARR*) -1) {
            if(chain) {
              assert(daget(chain, 0) == NULL);
            } else {
              chain = usedefchains[reg] = dactor(8);
              chain->length = 1;
            }
            chain->arr[0] = blk;
          }
        }
      },
      if(!(phijoinaddr->addr_type & (ISDEREF | ISLABEL | ISCONST))) {
        DYNARR* chain;
        unsigned int reg = phijoinaddr->addr.regnum;
        assert(reg < prog->regcnt);
        if((chain = usedefchains[reg])) {
          if(chain != (DYNARR*) -1) {
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

  //in order to calculate the liveness, the thing we probably want to do is...
  //recursively check if predecessors of any use node are sdominated by the def node, 
  //perhaps lazily generate bitfields for the dominance information?
  //we take in the prog and the use def chains, and then we output...
  //A dynarr of the liveness bitfields for all the variables
  //Easy to check for live-in just do not set on def node, even if there is a use there
  BITFIELD* varbs = calloc(sizeof(BITFIELD), prog->regcnt);
  liveness_populate(prog, usedefchains, varbs);
  for(unsigned int i = 0; i < prog->regcnt; i++) {
      assert(varbs[i] || !usedefchains[i]);
      free(varbs[i]);
  }
  free(varbs);

  //lastuse(prog, usedefchains);

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
    if(usedefchains[i] && usedefchains[i] != (DYNARR*) -1) dadtor(usedefchains[i]);
  free(usedefchains);
}

void lastuse(PROGRAM* prog, DYNARR** chains) {
  for(int blockind = 0; blockind < prog->allblocks->length; blockind++) {
    BBLOCK* blk = daget(prog->allblocks, blockind);
    HASHTABLE* reglasts = htctor();
    LOOPOPS(
      OPARGCASES(
        if(!(op->addr0_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, op->addr0.regnum))
          fixedinsert(reglasts, op->addr0.regnum, &op->addr0_type);
        ,
        if(!(op->addr1_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, op->addr1.regnum))
          fixedinsert(reglasts, op->addr1.regnum, &op->addr1_type);
        ,
        if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL)) && !islive_out(prog, blk, chains, op->dest.regnum))
          fixedinsert(reglasts, op->dest.regnum, &op->dest_type); //uh oh
        ,
        if(!(phijoinaddr->addr_type & (ISLABEL | ISCONST)) && !islive_out(prog, blk, chains, phijoinaddr->addr.regnum))
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

static void adjmatrixset(BITFIELD bf, int dim, int reg1, int reg2) {
  bfset(bf, reg1 * dim + reg2);
  bfset(bf, reg2 * dim + reg1);
}

//We need to treat multiple consecutive phi statements as if they copy at the same time for the sake of liveness, register allocation
//this is because there's much more flexibility if we do this
BITFIELD liveadjmatrix(PROGRAM* prog, DYNARR** usedefs) {
  int dim = prog->regcnt;
  BITFIELD bf = bfalloc(dim * dim);

  for(int blockindex = 0; blockindex < prog->allblocks->length; blockindex++) {
    BITFIELD curbf = bfalloc(dim);
    BBLOCK* blk = daget(prog->allblocks, blockindex);
    for(int i = 0; i < dim; i++) {
      if(islive_in(prog, blk, usedefs, i)) {
        bfset(curbf, i);
      }
      //each new declaration add collision
    }
  }
  return bf;
}

//handle long doubles same as doubles
//https://compilers.cs.uni-saarland.de/projects/ssara/hack_ssara_ssa09.pdf
//https://www.rw.cdl.uni-saarland.de/people/grund/private/papers/cgo08-liveness.pdf

void regalloc(PROGRAM* prog) {
  //attempt color
  //spill if fail--where/how to spill, with belady's algorithm?
  //how coalesce and retry?
}
#undef X
