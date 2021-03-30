#include "3ac.h"

static void addrgen(FILE* of, ADDRTYPE adt, ADDRESS addr) {
  if(adt & ISCONST) {
    if(adt & STRCONST) {
    } else {
    }
  } else if(adt & ISDEREF) {
  } else {
  }
}

static void cgblock(FILE* outputfile, BBLOCK* blk) {
}

void codegen(FILE* outputfile, PROGRAM* prog) {
  int labelnum = prog->allblocks->length;
  //fprintf(outputfile, ".global %s\n", something);
  //fprintf(outputfile, ".extern %s\n", something);???
  fprintf(outputfile, ".data\n");
  //fprintf(outputfile, ".asciiz \"%s\"\n", something);
  fprintf(outputfile, ".text\n");
  for(int i = 0; i < prog->allblocks->length; i++) {
    cgblock(outputfile, daget(prog->allblocks, i));
  }
}
