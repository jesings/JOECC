#ifndef _3AC_H
#define _3AC_H
#include <stdio.h>
#include "compintern.h"
#include "treeduce.h"

#define OPS_NOVAR_3ac \
  X(NOP_3) /*op0*/ X(LBL_3) /*op1 (special)*/ X(RET_0) /*op0*/
#define OPS_3_3ac_COM \
  X(ADD_U) X(ADD_F) X(MULT_U) X(MULT_I) X(MULT_F) \
  X(AND_U) X(OR_U) X(XOR_U) \
  X(EQ_U) X(EQ_F) X(NE_U) X(NE_F)
#define OPS_3_3ac_NOCOM \
  X(SUB_U) X(SUB_F) X(DIV_U) X(DIV_I) X(DIV_F) \
  X(MOD_U) X(MOD_I) \
  X(SHL_U) X(SHL_I) X(SHR_U) X(SHR_I) \
  X(GE_U) X(GE_I) X(GE_F) X(LE_U) X(LE_I) X(LE_F) \
  X(GT_U) X(GT_I) X(GT_F) X(LT_U) X(LT_I) X(LT_F)
#define OPS_3_3ac OPS_3_3ac_COM OPS_3_3ac_NOCOM
#define OPS_3_PTRDEST_3ac \
  X(COPY_3) /*source (pointer) length destination (pointer) */\
  X(ARROFF) /*source (uintconst or reg) index dest (index size implicit from result) */\
  X(ARRMOV) /*source (uintconst or reg) index dest (index size implicit from result) */\
  X(MTP_OFF)
#define OPS_2_3ac_MUT \
  X(NOT_U) X(NEG_I) X(NEG_F) \
  X(F2I) X(I2F) X(F2F) X(ALOC_3)
#define OPS_2_3ac OPS_2_3ac_MUT X(MOV_3)
#define OPS_NODEST_3ac \
  X(BEQ_U) X(BEQ_F) \
  X(BGE_U) X(BGE_I) X(BGE_F) \
  X(BGT_U) X(BGT_I) X(BGT_F) \
  X(JEQ_I) /*op3 (dest label)*/
#define OPS_1_3ac \
  X(BNZ_3) X(BEZ_3) \
  X(ARG_3) X(RET_3) 
#define OPS_1_ASSIGN_3ac \
  X(INIT_3) X(PARAM_3) /*both op1 declare variable*/

#define OPS_3AC OPS_NOVAR_3ac OPS_3_3ac OPS_3_PTRDEST_3ac OPS_2_3ac \
        OPS_NODEST_3ac OPS_1_ASSIGN_3ac OPS_1_3ac X(CALL_3) X(PHI) X(TPHI) \
        X(DEALOC) X(ADDR_3) X(ASM) /*call, phi, asm, and addr are special cases*/ \

#define X(s) s,
enum opcode_3ac {
  OPS_3AC
};
#undef X
#define GENERIC_U ASM+1
#define GENERIC_I ASM+2
#define GENERIC_F ASM+3
extern const char* opcode_3ac_names[];

enum passes {
  SSA = 1,
  GVN = 2,
};

struct op;
struct bblock;
struct fulladdr;

typedef union {
  struct {
    unsigned int varnum;
    union {
      unsigned int ssaind;
      unsigned int iregnum; //enforce same storage space
    };
  };
  unsigned long uintconst_64; //unsigned int 64 bit or pointer
  long intconst_64;
  double floatconst_64;
  char* strconst;
  unsigned long* arrayconst;
  char* labelname;
  struct fulladdr* joins;
  void* garbage;
} ADDRESS;

typedef enum {
  //bottom 4 bits used for size
  ISCONST = 0x10,
  ISSIGNED = 0x20,
  ISFLOAT = 0x40,
  ISLABEL = 0x80,
  ISSTRCONST = 0x100,
  ISPOINTER = 0x200,
  ISDEREF = 0x400,
  ISVAR = 0x800,
  ADDRSVAR = 0x1000,
  GARBAGEVAL = 0x2000,
} ADDRTYPE;

typedef struct op {
  enum opcode_3ac opcode;
  //int oprank;
  ADDRTYPE addr0_type;
  ADDRESS addr0;
  ADDRTYPE addr1_type;
  ADDRESS addr1;
  ADDRTYPE dest_type;
  ADDRESS dest;
  struct op* nextop;
} OPERATION;

typedef struct fulladdr {
  ADDRTYPE addr_type;
  ADDRESS addr;
} FULLADDR;

typedef struct bblock {
  OPERATION* firstop;
  OPERATION* lastop;
  DYNARR* inedges;
  struct bblock* nextblock;
  struct bblock* branchblock;

  DYNARR* idominates;
  DYNARR* df;
  struct bblock* dom; //immediate dominator
  struct bblock* postdom; //immediate postdominator
  int domind, postdomind;
  int visited;
  int work;

  char* anticipability_in; //bitfield
  char* anticipability_out; //bitfield
  char* availability; //bitfield
} BBLOCK;

typedef struct {
  unsigned long iregcnt;
  DYNARR* breaklabels;
  DYNARR* continuelabels;
  DYNARR* allblocks;
  DYNARR* dynvars;
  DYNARR* dynchars;
  HASHTABLE* labels;
  HASHTABLE* unfilledlabels;
  BBLOCK* curblock;
  BBLOCK* finalblock;
  int* tmpstore;
  enum passes pdone;
} PROGRAM;

OPERATION* ct_3ac_op0(enum opcode_3ac opcode);
OPERATION* ct_3ac_op1(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0);
OPERATION* ct_3ac_op2(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0, ADDRTYPE dest_type, ADDRESS dest);
OPERATION* ct_3ac_op3(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, ADDRTYPE dest_type, ADDRESS dest);
FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog);
FULLADDR smemrec(EXPRESSION* cexpr, PROGRAM* prog);
void initializestate(INITIALIZER* i, PROGRAM* prog);
void cmptype(EXPRESSION* cmpexpr, BBLOCK* failblock, BBLOCK* successblock, PROGRAM* prog);
void solidstate(STATEMENT* cst, PROGRAM* prog);
FULLADDR cmpnd_assign(enum opcode_3ac op, EXPRESSION* destexpr, EXPRESSION* srcexpr, PROGRAM* prog);
OPERATION* implicit_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                          ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog);
OPERATION* implicit_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_bitwise_3(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_unary_2(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_mtp_2(EXPRESSION* destexpr, EXPRESSION* fromexpr, FULLADDR a1, FULLADDR a2, PROGRAM* prog);
void implicit_shortcircuit_noret(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, BBLOCK* branchto, PROGRAM* prog);
FULLADDR implicit_shortcircuit_3(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, 
                                 ADDRESS complete_val, ADDRESS shortcircuit_val, PROGRAM* prog);
OPERATION* cmpret_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
PROGRAM* linefunc(FUNC* f);
void printprog(PROGRAM* prog);
void treeprog(PROGRAM* prog, char* fname, const char* pass);
void freeblock(void* blk);
void freeprog(PROGRAM* prog);

static inline ADDRTYPE addrconv(IDTYPE* idt) {
  ADDRTYPE adt;
  if(ispointer(idt)) {
    adt = ISPOINTER | 0x8;
  } else if(idt->tb & (STRUCTVAL | UNIONVAL)) {
    adt = ISPOINTER | 0x8;
  } else if(idt->tb & FLOATNUM) {
    adt = ISFLOAT | (idt->tb & 0xf);
  } else if(idt->tb & UNSIGNEDNUM) {
    adt = (idt->tb & 0xf);
  } else {
    adt = ISSIGNED | (idt->tb & 0xf);
  }
  return adt;
}

static inline BBLOCK* mpblk(void) {
  BBLOCK* pblk = calloc(1, sizeof(BBLOCK));
  pblk->inedges = dactor(8);
  return pblk;
}

static inline BBLOCK* fctblk(PROGRAM* prog) {
  BBLOCK* retval = mpblk();
  dapush(prog->allblocks, retval);
  prog->curblock = retval;
  return retval;
}


static inline BBLOCK* ctblk(PROGRAM* prog) {
  BBLOCK* retval = mpblk();
  BBLOCK* curblock = dapeek(prog->allblocks);
  dapush(prog->allblocks, retval);
  if(!curblock->nextblock) {
    curblock->nextblock = retval;
    dapush(retval->inedges, curblock);
  }
  prog->curblock = retval;
  return retval;
}

static inline void opn(PROGRAM* prog, OPERATION* op) {
  if(prog->curblock) {
    if(prog->curblock->lastop) {
      prog->curblock->lastop->nextop = op;
    } else {
      prog->curblock->firstop = op;
    }
    prog->curblock->lastop = op;
  } else {
    BBLOCK* blk = ctblk(prog);
    blk->firstop = op;
    blk->lastop = op;
  }
}

static inline void giveblock(PROGRAM* prog, BBLOCK* pblk) {
  BBLOCK* curblock = dapeek(prog->allblocks);
  dapush(prog->allblocks, pblk);
  if(!curblock->nextblock) {
    curblock->nextblock = pblk;
    dapush(pblk->inedges, curblock);
  }
  prog->curblock = pblk;
}

static inline FULLADDR op2addr(OPERATION* op) {
  FULLADDR fa = {op->dest_type, op->dest};
  return fa;
}
static inline FULLADDR op2ret(PROGRAM* prog, OPERATION* op) {
  opn(prog, op);
  FULLADDR fa = {op->dest_type, op->dest};
  return fa;
}

static inline int lentype(IDTYPE* idt) {
  if(ispointer(idt)) {
    struct declarator_part* pointtop = dapeek(idt->pointerstack);
    if(pointtop->type == VLASPEC) {
      return -1;
    } else if(pointtop->type != ARRAYSPEC) {
      return 0x8;
    } else {
      return pointtop->arrlen;
    }
  } else if(idt->tb & (STRUCTVAL | UNIONVAL)) {
    return idt->structtype->size;
  }
  return idt->tb & 0xf;
}

static inline FULLADDR ptarith(IDTYPE retidt, FULLADDR fadt, PROGRAM* prog) {
  FULLADDR destad;
  ADDRESS sz;
  retidt.pointerstack->length -= 1;
  sz.uintconst_64 = lentype(&retidt);
  retidt.pointerstack->length += 1;
  if(sz.uintconst_64 == 1) return fadt;
  destad.addr.iregnum = prog->iregcnt++;
  destad.addr_type = 8 | ISPOINTER;

  switch(sz.uintconst_64) {
    case 2:
      sz.uintconst_64 = 1;
      opn(prog, ct_3ac_op3(SHL_U, fadt.addr_type, fadt.addr, ISCONST | 1, sz, destad.addr_type, destad.addr));
      break;
    case 4:
      sz.uintconst_64 = 2;
      opn(prog, ct_3ac_op3(SHL_U, fadt.addr_type, fadt.addr, ISCONST | 1, sz, destad.addr_type, destad.addr));
      break;
    case 8:
      sz.uintconst_64 = 3;
      opn(prog, ct_3ac_op3(SHL_U, fadt.addr_type, fadt.addr, ISCONST | 1, sz, destad.addr_type, destad.addr));
      break;
    default:
      opn(prog, ct_3ac_op3(MULT_U, fadt.addr_type, fadt.addr, ISCONST | 4, sz, destad.addr_type, destad.addr));
      break;
  }
  return destad;
}
#define RGBCOLOR(R, G, B) "\033[38;2;" #R ";" #G ";" #B "m"
#define CLEARCOLOR "\033[39;49m"
#define GENREGMASK ~(ISCONST | ISLABEL | ISDEREF | ISVAR)
#define DEGENERIC(operation, opt) \
    if((operation)->opcode == GENERIC_U) (operation)->opcode = opt ## _U; \
    else if((operation)->opcode == GENERIC_I) (operation)->opcode = opt ## _U; /*for generic'ed, signed and unsigned are the same*/\
    else if((operation)->opcode == GENERIC_F) (operation)->opcode = opt ## _F;
#endif
#define FILLREG(addrvar, type) do { \
    (addrvar).addr.iregnum = prog->iregcnt++; \
    (addrvar).addr_type = (type); \
  } while(0)
