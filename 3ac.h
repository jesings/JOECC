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
  AND_U, AND_F,
  OR_U, OR_F,
  XOR_U, XOR_F,
  NOT_U, NOT_F,
  BEQ_U, BEQ_I, BEQ_F,
  BNE_U, BNE_I, BNE_F,
  BGE_U, BGE_I, BGE_F,
  BLE_U, BLE_I, BLE_F,
  BGT_U, BGT_I, BGT_F,
  BLT_U, BLT_I, BLT_F,
  JMP_3,
};

typedef enum {
  unsigned long regnum;
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
  char* labelname;
  char* constname;
} ADDRESS;

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

