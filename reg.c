#include "3ac.h"

enum reguse {
  DI = 1, 
  SI = 2, 
  DX = 4,
  CX = 8,
  AX = 0x10,
  R8 = 0x20,
  R9 = 0x40,
  R10 = 0x80,
  R11 = 0x100,
  R12 = 0x200,
  R13 = 0x400,
  R14 = 0x800,
  R15 = 0x1000,
  BX = 0x2000,
};

#define callermask BX | R12 | R13 | R14 | R15

enum reguse callreg[6] = {DI, SI, DX, CX, R8, R9};

char* ireg64[] = {"rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
char* ireg32[] = {"eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
char* ireg16[] = {"ax", "bx", "cx", "dx", "di", "si", "bp", "sp", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"};
char* ireg8[] = {"al", "bl", "cl", "dl", "dil", "sil", "bpl", "spl", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"};
char* freg128[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
char* freg256[] = {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};

struct opinfo {
  char* opname;
  int numargs;
  int fixedclobbers;
  enum reguse retloc;
  char reqreg;
};

struct opinfo op2op[] = {
  [NOP_3] = {"nop", 0, 0, 0, 0},
  [LBL_3] = {"", 1, 0, 0, 0}, //not sure?
  [ADD_U] = {"add", 2, 0, 0, 1},
  [ADD_F] = {"vadds", 3, 0, 0, 0},
  [MULT_U] = {"mul", 1, 0, DX, AX}, //multiplies ax by operand, places result in dx:ax ax is low part
  [MULT_I] = {"imul", 2, 0, 0, 1},
  [MULT_F] = {"vmuls", 3, 0, 0, 0},
  [AND_U] = {"and", 2, 0, 0, 1},
  [OR_U] = {"or", 2, 0, 0, 1},
  [XOR_U] = {"xor", 2, 0, 0, 1},
  [EQ_U] = {"setz", 1, 0, 0, 0}, //precede this by a cmp
  [EQ_F] = {"setz", 1, 0, 0, 0}, //precede this by a cmp
  [NE_U] = {"setnz", 2, 0, 0, 0}, //precede this by a cmp
  [NE_F] = {"setnz", 2, 0, 0, 0}, //precede this by a cmp
  [SUB_U] = {"sub", 2, 0, 0, 1},
  [SUB_F] = {"vsubs", 3, 0, 0, 0},
  [DIV_U] = {"div", 1, DX, AX, 0}, //divides dx:ax by operand, places result in ax dx is remainder?
  [DIV_I] = {"idiv", 2, 0, 0, 1},
  [DIV_F] = {"vdivs", 3, 0, 0, 0},
  [MOD_U] = {"div", 1, AX, DX, 0}, //divides dx:ax by operand, places result in ax dx is remainder?
  [MOD_I] = {"idiv", 1, AX, DX, 0}, //single operand division, use cqo or cdq or cwd or cbw before, does same as above
  [SHL_U] = {"shl", 2, 0, 0, 1},
  [SHL_I] = {"sal", 2, 0, 0, 1},
  [SHR_U] = {"shr", 2, 0, 0, 1},
  [SHR_I] = {"sar", 2, 0, 0, 1},
  [GE_U] = {"setae", 1, 0, 0, 0}, //precede this by a cmp
  [GE_I] = {"setge", 1, 0, 0, 0}, //precede this by a cmp
  [GE_F] = {"setae", 1, 0, 0, 0}, //precede this by a cmp
  [LE_U] = {"setbe", 1, 0, 0, 0}, //precede this by a cmp
  [LE_I] = {"setle", 1, 0, 0, 0}, //precede this by a cmp
  [LE_F] = {"setbe", 1, 0, 0, 0}, //precede this by a cmp
  [GT_U] = {"seta", 1, 0, 0, 0}, //precede this by a cmp
  [GT_I] = {"setg", 1, 0, 0, 0}, //precede this by a cmp
  [GT_F] = {"seta", 1, 0, 0, 0}, //precede this by a cmp
  [LT_U] = {"setb", 1, 0, 0, 0}, //precede this by a cmp
  [LT_I] = {"setl", 1, 0, 0, 0}, //precede this by a cmp
  [LT_F] = {"setb", 1, 0, 0, 0}, //precede this by a cmp
  [COPY_3] = {"repnz movs", 0, SI | DI | CX, 0, 0}, //clobbers si, di, move count into cx
  [ARROFF] = {"lea", 2, 0, 0, 0}, //not sure?
  [ARRMOV] = {"mov", 2, 0, 0, 0}, //not sure?
  [MTP_OFF] = {"mov", 2, 0, 0, 0}, //not sure?
  [NOT_U] = {"not", 2, 0, 0, 0},
  [NEG_I] = {"neg", 2, 0, 0, 0},
  [NEG_F] = {"xorps", 2, 0, 0, 0}, //cmpeqd reg (packed op), reg/pslld $31 (packed op), reg/xorps reg, dest
  [F2I] = {"cvtsd2si", 2, 0, 0, 0}, //many choices from cvtss2sd, etc.
  [I2F] = {"cvtsi2sd", 2, 0, 0, 0}, //many choices from cvtsd2ss, etc.
  [F2F] = {"cvtss2sd", 2, 0, 0, 0}, //many choices from cvtsd2ss, etc.
  [ALOC_3] = {"sub", 2, 0, 0, 0}, //subtract from rsp
  [MOV_3] = {"mov", 2, 0, 0, 0}, //vmovsd etc.
  [BEQ_U] = {"jeq", 2, 0, 0, 0}, //cmp
  [BEQ_F] = {"jeq", 2, 0, 0, 0}, //cmp
  [BGE_U] = {"jae", 2, 0, 0, 0}, //cmp
  [BGE_I] = {"jge", 2, 0, 0, 0}, //cmp
  [BGE_F] = {"jae", 2, 0, 0, 0}, //cmp
  [BGT_U] = {"ja", 2, 0, 0, 0}, //cmp
  [BGT_I] = {"jg", 2, 0, 0, 0}, //cmp
  [BGT_F] = {"ja", 2, 0, 0, 0}, //cmp
  [JEQ_I] = {"jeq", 2, 0, 0, 0}, //cmp
  [BNZ_3] = {"jnz", 2, 0, 0, 0}, //test
  [BEZ_3] = {"jez", 2, 0, 0, 0}, //test
  [ARG_3] = {"mov", 2, 0, 0, 0}, //decide where/when to put, push too
  [RET_3] = {"ret", 0, 0, 0, 0}, //put it in rax
  [INIT_3] = {"", 0, 0, 0, 0}, //not sure
  [PARAM_3] = {"mov", 2, 0, 0, 0}, //not sure
  [CALL_3] = {"call", 1, DI | SI | DX | CX | R8 | R9 | R10 , AX, 0}, //call, may 
  [PHI] = {"", 0, 0, 0, 0}, //no phi nodes by this point
  [DEALOC] = {"sub", 2, 0, 0, 0},
  [ADDR_3] = {"lea", 2, 0, 0, 0}, //most likely nop?
  [ASM] = {"", 0, 0, 0, 0}, //figure it out
};
//handle long doubles same as doubles
//

int regalloc() {
}
