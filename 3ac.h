#ifndef _3AC_H
#define _3AC_H
#include <stdio.h>
#include "compintern.h"
#include "treeduce.h"

#define OPS_3AC \
  X(NOP_3), X(LBL_3), \
  X(ADD_U), X(ADD_I), X(ADD_F), \
  X(SUB_U), X(SUB_I), X(SUB_F), \
  X(MULT_U), X(MULT_I), X(MULT_F), \
  X(DIV_U), X(DIV_I), X(DIV_F), \
  X(MOD_U), X(MOD_I), \
  X(SHL_U), X(SHL_I), \
  X(SHR_U), X(SHR_I), \
  X(AND_U), X(AND_F), \
  X(OR_U), X(OR_F), \
  X(XOR_U), X(XOR_F), \
  X(NOT_U), X(NOT_F), \
  X(INC_U), X(INC_I), X(INC_F), \
  X(DEC_U), X(DEC_I), X(DEC_F), \
  X(NEG_I), X(NEG_F), \
  X(EQ_U), X(EQ_I), X(EQ_F), \
  X(NE_U), X(NE_I), X(NE_F), \
  X(GE_U), X(GE_I), X(GE_F), \
  X(LE_U), X(LE_I), X(LE_F), \
  X(GT_U), X(GT_I), X(GT_F), \
  X(LT_U), X(LT_I), X(LT_F), \
  X(BEQ_U), X(BEQ_I), X(BEQ_F), \
  X(BNE_U), X(BNE_I), X(BNE_F), \
  X(BGE_U), X(BGE_I), X(BGE_F), \
  X(BLE_U), X(BLE_I), X(BLE_F), \
  X(BGT_U), X(BGT_I), X(BGT_F), \
  X(BLT_U), X(BLT_I), X(BLT_F), \
  X(BRNCH), \
  X(JEQ_I), \
  X(JOIN_3), \
  X(ADDR_3), \
  X(BNZ_3), X(BEZ_3), \
  X(JMP_3), \
  X(MOV_3), \
  X(MTP_OFF), \
  X(ARG_3), /*Do this for each param for CALL, must be done immediately before CALL */\
  X(CALL_3), X(RET_3), X(RET_0),\
  X(F2I), X(I2F), \
  X(COPY_3), /*source (pointer), length, destination (pointer) */\
  X(ARRIND), /*source (uintconst_64 or regnum), index, dest (index size impicit from result size) */\
  X(ARROFF), /*source (uintconst_64 or regnum), index, dest (index size impicit from result size) */\
  X(ARRMOV), /*source (uintconst_64 or regnum), index, dest (index size impicit from result size) */\
  X(ALOC_3), /*Allocate stack memory for struct, op2, takes size, and outputs start index*/\
  X(INIT_3), /*we need to add to symbol table on encountering it, 2 params: size, regnum */\
  X(PARAM_3), /*we need to add to symbol table on encountering it, 2 params: size, regnum */

#define X(s) s
enum opcode_3ac {
  OPS_3AC
};
#undef X

struct op;

typedef union {
  unsigned long iregnum; //integer register
  unsigned long fregnum; //floating point register
  unsigned long uintconst_64; //unsigned int 64 bit or pointer
  long intconst_64;
  double floatconst_64;
  char* strconst;
  unsigned long* arrayconst;
  char* labelname;
  struct op* branchop;
  DYNARR* joins;
} ADDRESS;

//Extra information for SSA?

typedef enum {
  //bottom 4 bits used for size
  ISCONST = 0x10,
  ISSIGNED = 0x20,
  ISFLOAT = 0x40,
  ISLABEL = 0x80,
  ISSTRCONST = 0x100,
  ISPOINTER = 0x200,
  ISDEREF = 0x400,
} ADDRTYPE;

typedef struct op {
  enum opcode_3ac opcode;
  ADDRTYPE addr0_type;
  ADDRESS addr0;
  ADDRTYPE addr1_type;
  ADDRESS addr1;
  ADDRTYPE dest_type;
  ADDRESS dest;
  struct op* nextop;
} OPERATION;

typedef struct {
  ADDRTYPE addr_type;
  ADDRESS addr;
} FULLADDR;

typedef struct {
  OPERATION* firstop;
  OPERATION* lastop;
  int labelcnt;
  unsigned long iregcnt;
  unsigned long fregcnt;
  DYNARR* breaklabels;
  DYNARR* continuelabels;
  HASHTABLE* fixedvars;
  HASHTABLE* labels;
} PROGRAM;


OPERATION* ct_3ac_op0(enum opcode_3ac opcode);
OPERATION* ct_3ac_op1(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0);
OPERATION* ct_3ac_op2(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0, ADDRTYPE dest_type, ADDRESS dest);
OPERATION* ct_3ac_op3(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, ADDRTYPE dest_type, ADDRESS dest);
FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog);
FULLADDR smemrec(EXPRESSION* cexpr, PROGRAM* prog);
void initializestate(INITIALIZER* i, PROGRAM* prog);
OPERATION* cmptype(EXPRESSION* cmpexpr, OPERATION* op2brnch, char negate, PROGRAM* prog);
void solidstate(STATEMENT* cst, PROGRAM* prog);
FULLADDR cmpnd_assign(enum opcode_3ac op, EXPRESSION* destexpr, EXPRESSION* srcexpr, PROGRAM* prog);
OPERATION* implicit_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                          ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog);
OPERATION* implicit_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_unary_2(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_mtp_2(EXPRESSION* destexpr, EXPRESSION* fromexpr, FULLADDR a1, FULLADDR a2, PROGRAM* prog);
FULLADDR implicit_shortcircuit_3(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, 
                                 ADDRESS complete_val, ADDRESS shortcircuit_val, PROGRAM* prog);
OPERATION* cmpret_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
PROGRAM* linefunc(FUNC* f);
void printprog(PROGRAM* prog);

static inline char* proglabel(PROGRAM* prog) {
  char* c = malloc(8);
  snprintf(c, 8, ".L%d", (prog->labelcnt)++);
  return c;
}
static inline FULLADDR op2addr(OPERATION* op) {
  FULLADDR fa = {op->dest_type, op->dest};
  return fa;
}
static inline FULLADDR op2ret(PROGRAM* prog, OPERATION* op) {
  prog->lastop = prog->lastop->nextop = op;
  FULLADDR fa = {op->dest_type, op->dest};
  return fa;
}
static inline enum opcode_3ac cmp_osite(EXPRTYPE coptype, char negate) {
  switch(coptype) {
    case EQ:
      return negate ? BNE_U : BEQ_U;
    case NEQ:
      return negate ? BEQ_U : BNE_U;
    case GT:
      return negate ? BLE_U : BGT_U;
    case LT:
      return negate ? BGE_U : BLT_U;
    case GTE:
      return negate ? BLT_U : BGE_U;
    case LTE:
      return negate ? BGT_U : BLE_U;
    case L_NOT:
      return negate ? BNZ_3 : BEZ_3;
    default:
      return negate ? BEZ_3 : BNZ_3;
  }
}
char remove_nops(PROGRAM* prog);
void freeprog(PROGRAM* prog);

static inline ADDRTYPE addrconv(IDTYPE* idt) {
  ADDRTYPE adt;
  if(idt->pointerstack && idt->pointerstack->length) {
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
      prog->lastop = prog->lastop->nextop = ct_3ac_op3(SHL_U, fadt.addr_type, fadt.addr, ISCONST | 1, sz, destad.addr_type, destad.addr);
      break;
    case 4:
      sz.uintconst_64 = 2;
      prog->lastop = prog->lastop->nextop = ct_3ac_op3(SHL_U, fadt.addr_type, fadt.addr, ISCONST | 1, sz, destad.addr_type, destad.addr);
      break;
    case 8:
      sz.uintconst_64 = 3;
      prog->lastop = prog->lastop->nextop = ct_3ac_op3(SHL_U, fadt.addr_type, fadt.addr, ISCONST | 1, sz, destad.addr_type, destad.addr);
      break;
    default:
      prog->lastop = prog->lastop->nextop = ct_3ac_op3(MULT_U, fadt.addr_type, fadt.addr, ISCONST | 4, sz, destad.addr_type, destad.addr);
      break;
  }
  return destad;
}

static inline void opn(PROGRAM* prog, OPERATION* op) {
  prog->lastop->nextop = op;
  prog->lastop = op;
}

#define RGBCOLOR(R, G, B) "\033[38;2;" #R ";" #G ";" #B "m"
#define CLEARCOLOR "\033[39;49m"
#endif
