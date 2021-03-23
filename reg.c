#include "3ac.h"

char* ireg64[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
char* ireg32[] = {"eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
char* ireg16[] = {"ax", "bx", "cx", "dx", "di", "si", "bp", "sp", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"};
char* ireg8[] = {"al", "bl", "cl", "dl", "dil", "sil", "bpl", "spl", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"};
char* freg128[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
char* freg256[] = {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};

struct opinfo {
  char* opname;
  int numargs;
  //clobbers?
  //anything else?
};

struct opinfo op2op[] = {
  [NOP_3] = {"nop", 0},
  [LBL_3] = {"", 1}, //not sure?
  [RET_0] = {"ret", 0}, //not sure?
  [ADD_U] = {"add", 2},
  [ADD_F] = {"vadds", 3},
  [MULT_U] = {"mul", 1}, //multiplies ax by operand, places result in dx:ax ax is low part
  [MULT_I] = {"imul", 2},
  [MULT_F] = {"vmuls", 3},
  [AND_U] = {"and", 2},
  [OR_U] = {"or", 2},
  [XOR_U] = {"xor", 2},
  [EQ_U] = {"setz", 1}, //precede this by a cmp
  [EQ_F] = {"setz", 1}, //precede this by a cmp
  [NEQ_U] = {"setnz", 2}, //precede this by a 
  [NEQ_F] = {"setnz", 2}, //then a cmovz
  [SUB_U] = {"sub", 3},
  [SUB_F] = {"vsubs", 3},
  [DIV_U] = {"div", 1}, //divides dx:ax by operand, places result in ax dx is remainder?
  [DIV_I] = {"idiv", 2},
  [DIV_F] = {"vdivs", 3},
  [MOD_U] = {"div", 1}, //divides dx:ax by operand, places result in ax dx is remainder?
  [MOD_I] = {"idiv", 1}, //single operand division, use cqo or cdq or cwd or cbw before, does same as above
  [SHL_U] = {"shl", 2},
  [SHL_I] = {"sal", 2},
  [SHR_U] = {"shr", 2},
  [SHR_I] = {"sar", 2},
  [GE_U] = {"setae", 1}, //precede this by a cmp
  [GE_I] = {"setge", 1}, //precede this by a cmp
  [GE_F] = {"setae", 1}, //precede this by a cmp
  [LE_U] = {"setbe", 1}, //precede this by a cmp
  [LE_I] = {"setle", 1}, //precede this by a cmp
  [LE_F] = {"setbe", 1}, //precede this by a cmp
  [GT_U] = {"seta", 1}, //precede this by a cmp
  [GT_I] = {"setg", 1}, //precede this by a cmp
  [GT_F] = {"seta", 1}, //precede this by a cmp
  [LT_U] = {"setb", 1}, //precede this by a cmp
  [LT_I] = {"setl", 1}, //precede this by a cmp
  [LT_F] = {"setb", 1}, //precede this by a cmp
  [COPY_3] = {"repnz movs", 0}, //clobbers si, di, move count into cx
  [ARROFF] = {"lea", 2}, //not sure?
  [ARRMOV] = {"mov", 2}, //not sure?
  [MTP_OFF] = {"mov", 2}, //not sure?
  [NOT_U] = {"not", 2},
  [NEG_I] = {"neg", 2},
  [NEG_F] = {"xorps", 2}, //cmpeqd reg (packed op), reg/pslld $31 (packed op), reg/xorps reg, dest
  [F2I] = {"cvtsd2si", 2}, //many choices from cvtss2sd, etc.
  [I2F] = {"cvtsi2sd", 2}, //many choices from cvtsd2ss, etc.
  [ALOC_3] = {"sub", 2}, //subtract from rsp
  [MOV_3] = {"mov", 2}, //vmovsd etc.
};
//handle long doubles same as doubles
//

int regalloc() {
}
