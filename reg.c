#include "reg.h"
#define X(op) case op:

enum reguse callreg[6] = {DI, SI, DX, CX, R8, R9};


const char* ireg64[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
const char* ireg32[] = {"eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
const char* ireg16[] = {"ax", "bx", "cx", "dx", "di", "si", "bp", "sp", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"};
const char* ireg8[] = {"al", "bl", "cl", "dl", "dil", "sil", "bpl", "spl", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"};
const char* freg128[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
const char* freg256[] = {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};

static void rrblk(BBLOCK* blk, int blkcnt) {
  if(blk->visited) return;
  blk->visited = 1;
  if(blk->nextblock && blk->nextblock->domind > blk->domind) {
    rrblk(blk->nextblock, blkcnt);
    blk->simply_reachable = bfclone(blk->nextblock->simply_reachable, blkcnt);
  }
  if(blk->branchblock && blk->branchblock->domind > blk->domind) {
    rrblk(blk->branchblock, blkcnt);
    if(!blk->simply_reachable) {
      blk->simply_reachable = bfclone(blk->branchblock->simply_reachable, blkcnt);
    } else {
      for(int i = 0; i < (blkcnt + 7) >> 3; i++)
        blk->simply_reachable[i] |= blk->branchblock->simply_reachable[i];
    }
  }
  if(!blk->simply_reachable) blk->simply_reachable = bfalloc(blkcnt);
  bfset(blk->simply_reachable, blk->domind);
}
static void calcbackblocks(BBLOCK* blk, PROGRAM* prog) {
  blk->back_reachable = bfalloc(prog->allblocks->length + 1);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* possbackblock = daget(prog->allblocks, i);
    if(possbackblock != blk) continue;
    if(bfget(blk->simply_reachable, possbackblock->domind) && !bfget(possbackblock->simply_reachable, blk->domind)) {
      bfset(blk->back_reachable, possbackblock->domind);
    }
  }
}

static void reducedreachable(PROGRAM* prog) {
  BBLOCK* b = daget(prog->allblocks, 0);
  rrblk(b, prog->allblocks->length + 1);
  for(int i = 0; i < prog->allblocks->length; i++) {
    BBLOCK* blk = daget(prog->allblocks, i);
    blk->visited = 0;
    calcbackblocks(blk, prog);
  }
}

static char isreachable(BBLOCK* src, BBLOCK* dest, BITFIELD bf) {
  if(bfget(bf, src->domind))
    return 0;
  //DFS because I'm lazy
  bfset(bf, src->domind);
  if(src == dest) 
    return 1;
  return (src->nextblock && isreachable(src->nextblock, dest, bf)) || (src->branchblock && isreachable(src->branchblock, dest, bf));
}

static char islive_in(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, int varnum) {
  if(!usedefchains[varnum] || usedefchains[varnum] == (void*) -1 || !usedefchains[varnum]->arr[0]) return 0;
  BBLOCK* defblock = usedefchains[varnum]->arr[0];
  BITFIELD visited = bfalloc(prog->allblocks->length);
  for(int index = 1; index < usedefchains[varnum]->length; index++) {
    BBLOCK* otherblock = daget(usedefchains[varnum], index);
    bfzero(visited, prog->allblocks->length);
    bfset(visited, defblock->domind);
    if(isreachable(blk, otherblock, visited)) {
      free(visited);
      return 1;
    }
  }
  free(visited);
  return 0;
}
static char islive_out(PROGRAM* prog, BBLOCK* blk, DYNARR** usedefchains, int varnum) {
  if(!usedefchains[varnum] || usedefchains[varnum] == (void*) -1 || !usedefchains[varnum]->arr[0]) return 0;
  if(blk->nextblock && islive_in(prog, blk->nextblock, usedefchains, varnum))
    return 1;
  if(blk->branchblock && islive_in(prog, blk->branchblock, usedefchains, varnum))
    return 1;
  return 0;
}

void lastuse(PROGRAM* prog, DYNARR** chains);

void liveness(PROGRAM* prog) {
  DYNARR** usedefchains = calloc(prog->regcnt, sizeof(DYNARR*)); //could be reduced by renaming registers downwards first
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
  reducedreachable(prog);

  lastuse(prog, usedefchains);

  LOOPALLBLOCKS(
    OPARGCASES(
      ,
      ,
      if(!(op->dest_type & (ISLABEL | ISCONST | GARBAGEVAL | ISDEREF)) && op->dest_type & LASTUSE &&  op->opcode != CALL_3) {
        if(op->opcode == PHI) free(op->addr0.joins);
        op->opcode = NOP_3;
      }
      ,
      (void) phijoinaddr;
    )
  )

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

//treat multiple consecutive phi statements as if they copy at the same time for the sake of liveness, register allocation
//this is because there's much more flexibility if we do this
BITFIELD liveadjmatrix(PROGRAM* prog) {
  int dim = prog->regcnt;
  BITFIELD bf = bfalloc(dim * dim);

  for(int blockindex = 0; blockindex < prog->allblocks->length; blockindex++) {
    BBLOCK* blk = daget(prog->allblocks, blockindex);
    for(unsigned int bfiterval = 0; bfiterval < prog->regcnt; bfiterval++) {
      if(bfget(bf, bfiterval)) {
      }
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
