#ifndef 3AC_H
#define 3AC_H

#define OPS_3AC */\
  X(NOP_3), */\
  X(ADD_U), X(ADD_I), X(ADD_F), */\
  X(SUB_U), X(SUB_I), X(SUB_F), */\
  X(MULT_U), X(MULT_I), X(MULT_F), */\
  X(DIV_U), X(DIV_I), X(DIV_F), */\
  X(MOD_U), X(MOD_I), */\
  X(SHL_U), X(SHR_U), */\
  X(SHL_I), X(SHR_I), */\
  X(AND_U), X(AND_F), */\
  X(OR_U), X(OR_F), */\
  X(XOR_U), X(XOR_F), */\
  X(NOT_U), X(NOT_F), */\
  X(INC_U), X(INC_I), */\
  X(DEC_U), X(DEC_I), */\
  X(NEG_I), X(NEG_F), */\
  X(ADDR_U), X(ADDR_I), X(ADDR_F), */\
  X(BEQ_U), X(BEQ_I), X(BEQ_F), */\
  X(BNE_U), X(BNE_I), X(BNE_F), */\
  X(BGE_U), X(BGE_I), X(BGE_F), */\
  X(BLE_U), X(BLE_I), X(BLE_F), */\
  X(BGT_U), X(BGT_I), X(BGT_F), */\
  X(BLT_U), X(BLT_I), X(BLT_F), */\
  X(JMP_3), */\
  X(MOV_3), */\
  X(MOV_TO_PTR), X(MOV_FROM_PTR), */\
  X(PARAM_3), /*Do this for each param for CALL, must be done immediately before CALL */\
  X(CALL_3), \
  X(FLOAT_TO_INT), X(INT_TO_FLOAT), \
  X(ARRAY_INDEX), /* pointer (uintconst_64 or regnum), index, result (index size impicit from result size) */\
  X(ARRAY_OFFSET), /*  pointer (uintconst_64 or regnum), index, result (index size impicit from result size) */\
  X(INIT_3), /*we need to add to symbol table on encountering it, 2 params: size, regnum */

#define X(s) s
enum opcode_3ac {
  OPS_3AC;
};
#undef X

typedef union {
  unsigned long iregnum;
  unsigned long fregnum;
  unsigned long uintconst_64;
  long intconst_64;
  unsigned int uintconst_32;
  int intconst_32;
  unsigned short uintconst_16;
  short intconst_16;
  unsigned char uintconst_8;
  char intconst_8;
  long double floatconst_80;
  double floatconst_64;
  float floatconst_32;
  char* strconst;
  unsigned long* arrayconst;
  char* labelname;
} ADDRESS;

//Extra information for SSA?

enum addrtype {
  //bottom 6 bits used for size
  ISCONST = 0x40, //if not set, it's a register
  ISFLOAT = 0x80, //if not set it's an int
  ISLABEL = 0x100, //if not set it's not a label
  //string and array constants not handled yet
};

typedef struct {
  enum opcode_3ac opcode;
  enum addrtype addr0_type;
  ADDRESS addr0;
  enum addrtype addr1_type;
  ADDRESS addr1;
  enum addrtype addr2_type;
  ADDRESS addr2;
} OPERATION;

#endif

