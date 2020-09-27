#ifndef _3AC_H
#define _3AC_H

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
  X(ADDR_U), X(ADDR_I), X(ADDR_F), /*not sure if I is needed*/\
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
  X(BNZ_3), X(BEZ_3), \
  X(JMP_3), \
  X(MOV_3), \
  X(MTP_U), X(MTP_I), X(MTP_F), /*move to pointer*/\
  X(MFP_U), X(MFP_I), X(MFP_F), /*move from pointer*/\
  X(PARAM_3), /*Do this for each param for CALL, must be done immediately before CALL */\
  X(CALL_3), X(RETURN_3), \
  X(FLOAT_TO_INT), X(INT_TO_FLOAT), \
  X(ARRAY_INDEX), /* pointer (uintconst_64 or regnum), index, result (index size impicit from result size) */\
  X(ARRAY_OFFSET), /*  pointer (uintconst_64 or regnum), index, result (index size impicit from result size) */\
  X(INIT_3), /*we need to add to symbol table on encountering it, 2 params: size, regnum */

#define X(s) s
enum opcode_3ac {
  OPS_3AC
};
#undef X

struct ptaddress;

typedef union {
  unsigned long iregnum; //integer register
  unsigned long fregnum; //floating point register
  unsigned long uintconst_64; //unsigned int 64 bit or pointer
  long intconst_64;
  double floatconst_64;
  char* strconst;
  unsigned long* arrayconst;
  char* labelname;
  struct ptaddress* ptaddr;
} ADDRESS;

//Extra information for SSA?

typedef enum {
  //bottom 4 bits used for size
  ISCONST = 0x10, //if not set, it's a register
  ISSIGNED = 0x20, //if not set it's an unsigned int
  ISFLOAT = 0x40, //if not set it's an int
  ISLABEL = 0x80, //if not set it's not a label
  ISSTRCONST = 0x100, //if not set it's not a string
  ISPOINTER = 0x200, //needs regnum and pointer type?
  //isstruct not necessary because of how we type struct exprs
  //array constants not necessary to handle here
} ADDRTYPE;

typedef struct {
  enum opcode_3ac opcode;
  ADDRTYPE addr0_type;
  ADDRESS addr0;
  ADDRTYPE addr1_type;
  ADDRESS addr1;
  ADDRTYPE dest_type;
  ADDRESS dest;
} OPERATION;

typedef struct {
  ADDRTYPE addr_type;
  ADDRESS addr;
} FULLADDR;

typedef struct {
  DYNARR* ops;
  int labelcnt;
  unsigned long iregcnt;
  unsigned long fregcnt;
  DYNARR* breaklabels;
  DYNARR* continuelabels;
  HASHTABLE* fixedvars;
} PROGRAM;


OPERATION* ct_3ac_op0(enum opcode_3ac opcode);
OPERATION* ct_3ac_op1(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0);
OPERATION* ct_3ac_op2(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0, ADDRTYPE dest_type, ADDRESS dest);
OPERATION* ct_3ac_op3(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, ADDRTYPE dest_type, ADDRESS dest);
FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* cmptype(EXPRESSION* cmpexpr, char* addr2jmp, char negate, PROGRAM* prog);
void solidstate(STATEMENT* cst, PROGRAM* prog);
OPERATION* implicit_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                          ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog);
OPERATION* nocoerce_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                          ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog);
OPERATION* implicit_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_unary_2(enum opcode_3ac op, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* implicit_nocoerce_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
FULLADDR implicit_shortcircuit_3(enum opcode_3ac op_to_cmp, EXPRESSION* cexpr, 
                                 ADDRESS complete_val, ADDRESS shortcircuit_val, PROGRAM* prog);
OPERATION* cmpret_binary_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
OPERATION* binshift_3(enum opcode_3ac opcode_unsigned, EXPRESSION* cexpr, PROGRAM* prog);
void printprog(PROGRAM* prog);

inline char* proglabel(PROGRAM* prog) {
  char* c = malloc(8);
  snprintf(c, 8, ".L%d", (prog->labelcnt)++);
  return c;
}
inline FULLADDR op2addr(OPERATION* op) {
  FULLADDR fa = {op->dest_type, op->dest};
  return fa;
}
inline FULLADDR op2ret(DYNARR* da, OPERATION* op) {
  dapush(da, op);
  FULLADDR fa = {op->dest_type, op->dest};
  return fa;
}
inline enum opcode_3ac cmp_osite(EXPRTYPE coptype, char negate) {
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

#endif
