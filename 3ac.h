#ifndef _3AC_H
#define _3AC_H

#define OPS_3AC \
  X(NOP_3), X(LBL_3), \
  X(ADD_U), X(ADD_I), X(ADD_F), \
  X(SUB_U), X(SUB_I), X(SUB_F), \
  X(MULT_U), X(MULT_I), X(MULT_F), \
  X(DIV_U), X(DIV_I), X(DIV_F), \
  X(MOD_U), X(MOD_I), \
  X(SHL_U), X(SHR_U), \
  X(SHL_I), X(SHR_I), \
  X(AND_U), X(AND_F), \
  X(OR_U), X(OR_F), \
  X(XOR_U), X(XOR_F), \
  X(NOT_U), X(NOT_F), \
  X(INC_U), X(INC_I), \
  X(DEC_U), X(DEC_I), \
  X(NEG_I), X(NEG_F), \
  X(ADDR_U), X(ADDR_I), X(ADDR_F), \
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
  X(MOV_TO_PTR), X(MOV_FROM_PTR), \
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
#define X(s) #s
const char* opcode_3ac_names[] = {
  OPS_3AC
};
#undef X

typedef union {
  unsigned long iregnum; //integer register
  unsigned long fregnum; //floating point register
  unsigned long uintconst_64; //unsigned int 64 bit or pointer
  long intconst_64;
  double floatconst_64;
  char* strconst;
  unsigned long* arrayconst;
  char* labelname;
} ADDRESS;

//Extra information for SSA?

typedef enum {
  //bottom 6 bits used for size
  ISCONST = 0x40, //if not set, it's a register
  ISSIGNED = 0x80, //if not set it's an unsigned int
  ISFLOAT = 0x100, //if not set it's an int
  ISLABEL = 0x200, //if not set it's not a label
  ISSTRCONST = 0x300, //if not set it's not a string
  //array constants not handled yet
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
} PROGRAM;


OPERATION* ct_3ac_op0(enum opcode_3ac opcode);
OPERATION* ct_3ac_op1(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0);
OPERATION* ct_3ac_op2(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0, ADDRTYPE dest_type, ADDRESS dest);
OPERATION* ct_3ac_op3(enum opcode_3ac opcode, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, ADDRTYPE dest_type, ADDRESS dest);
FULLADDR linearitree(EXPRESSION* cexpr, PROGRAM* prog);
ADDRTYPE cmptype(EXPRESSION* cmpexpr);
void solidstate(STATEMENT* cst, PROGRAM* prog);
FULLADDR implicit_3ac_3(enum opcode_3ac opcode_unsigned, ADDRTYPE addr0_type, ADDRESS addr0,
                      ADDRTYPE addr1_type, ADDRESS addr1, PROGRAM* prog);

inline char* proglabel(PROGRAM* prog) {
  char* c = malloc(8);
  snprintf(c, 8, ".L%d", (prog->labelcnt)++);
  return c;
}

#endif

