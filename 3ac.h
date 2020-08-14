#ifndef 3AC_H
#define 3AC_H

enum opcode_3ac {
  NOP_3,
  ADD_U, ADD_I, ADD_F,
  SUB_U, SUB_I, SUB_F,
  MULT_U, MULT_I, MULT_F,
  DIV_U, DIV_I, DIV_F,
  MOD_U, MOD_I,
  SHL_U, SHR_U,
  SHL_I, SHR_I,
  AND_U, AND_F,
  OR_U, OR_F,
  XOR_U, XOR_F,
  NOT_U, NOT_F,
  INC_U, INC_I,
  DEC_U, DEC_I,
  NEG_I, NEG_F,
  ADDR_U, ADDR_I, ADDR_F,
  BEQ_U, BEQ_I, BEQ_F,
  BNE_U, BNE_I, BNE_F,
  BGE_U, BGE_I, BGE_F,
  BLE_U, BLE_I, BLE_F,
  BGT_U, BGT_I, BGT_F,
  BLT_U, BLT_I, BLT_F,
  JMP_3,
  MOV_3,
  MOV_TO_PTR, MOV_FROM_PTR,
  PARAM_3, //Do this for each param for CALL, must be done immediately before CALL
  CALL_3,
  FLOAT_TO_INT, INT_TO_FLOAT,
  ARRAY_INDEX, // pointer (uintconst_64 or regnum), index, result (index size impicit from result size)
  ARRAY_OFFSET, // pointer (uintconst_64 or regnum), index, result (index size impicit from result size)
  INIT_3, //we need to add to symbol table on encountering it, 2 params: size, regnum
};

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

enum ADDRTYPE {
  //bottom 6 bits used for size
  ISCONST = 0x40, //if not set, it's a register
  ISFLOAT = 0x80, //if not set it's an int
  ISLABEL = 0x100, //if not set it's not a label
  //string and array constants not handled yet
}

typedef struct {
  enum opcode_3ac opcode;
  //type and size;
  ADDRESS addr0;
  //type and size;
  ADDRESS addr1;
  //type and size;
  ADDRESS addr2;
} OPERATION;

#endif

