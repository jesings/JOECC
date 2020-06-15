#include <stdio.h>
#include "compintern.h"

#define X(name) #name
char* name_EXPRTYPE[] = {
  EXPRTYPELIST
};
char* name_STMTTYPE[] = {
  STMTTYPE_LIST
};
char* name_DECLPART_TYPE[] = {
  DECLPART_TYPE
};
char* name_MEMBERTYPE[] = {
  MEMBERTYPELIST
};
#undef X

#define COLOR(r, g, b) "\e[ 38;2;" #r ";" #g ";" #b "m"
#define HCOLOR(hex) COLOR((hex & 0xff0000) >> 16, (hex & 0xff00) >> 8,  hex & 0xff)
struct color {
  char r, g, b;
};
struct color rainbow[] = {{148, 0, 211}, {75, 0, 130}, {0, 0, 255}, {0, 255, 0}, {255, 255, 0}, {255, 127, 0}, {255, 0 , 0}};
char rainbowpos = 0;

char* name_TYPEBITS(TYPEBITS tb) {
  char* vals = malloc(1024);
  short index = 0;

  for(int sh = 0x1; sh < 0x8000; sh <<= 1) {
    switch(tb & sh) {
      case 0x1: memcpy(&vals[index], "8-BIT ", 6); index += 6; break;
      case 0x2: memcpy(&vals[index], "16-BIT ", 7); index += 7; break;
      case 0x4: memcpy(&vals[index], "32-BIT ", 7); index += 7; break;
      case 0x8: memcpy(&vals[index], "64-BIT ", 7); index += 7; break;
      case 0x10: memcpy(&vals[index], "FLOATNUM ", 9); index += 9; break;
      case 0x20: memcpy(&vals[index], "UNSIGNEDNUM ", 12); index += 12; break;
      case 0x40: memcpy(&vals[index], "CONSTNUM ", 9); index += 9; break;
      case 0x80: memcpy(&vals[index], "VOLATILENUM ", 12); index += 12; break;
      case 0x100: memcpy(&vals[index], "STATICNUM ", 10); index += 10; break;
      case 0x200: memcpy(&vals[index], "EXTERNNUM ", 10); index += 10; break;
      case 0x400: memcpy(&vals[index], "PARAMNUM ", 9); index += 9; break;
      case 0x800: memcpy(&vals[index], "VOIDNUM ", 8); index += 8; break;
      case 0x1000: memcpy(&vals[index], "ENUMVAL ", 8); index += 8; break;
      case 0x2000: memcpy(&vals[index], "STRUCTVAL ", 10); index += 10; break;
      case 0x4000: memcpy(&vals[index], "UNIONVAL ", 9); index += 9; break;
    }
  }
  vals[index] = 0;
  return vals;
}

char* treeid(IDENTIFIERINFO* id) {
  //TODO
}

char* treetype(IDTYPE* type) {
  //TODO
}

char* treexpr(EXPRESSION* expr) {
  DYNSTR* dstrdly = strctor(malloc(2048), 0, 2048);
  struct color ecolor = rainbow[rainbowpos = (rainbowpos + 1) % 7];
  char* docolor = COLOR(ecolor->r, ecolor->g, ecolor->b);
  dscat(dstrdly, docolor, strlen(docolor));
  dsccat(dstrdly, '(');
  dscat(dstrdly, name_EXPRTYPE[expr->type], strlen(name_EXPRTYPE[expr->type]));
  dsccat(dstrdly, ' ');
  char buf[128];
  switch(expr->type) {
    case NOP:
      dscat(dstrdly, "NOTHING", 8);
      break;
    case STRING:
      dscat(dstrdly, expr->strconst, strlen(expr->strconst)); //don't care abt performance
      break;
    case INT:
      sprintf(buf, "%ld", expr->intconst);
      dscat(dstrdly, buf, strlen(buf));
      break;
    case UINT:
      sprintf(buf, "%lu", expr->uintconst);
      dscat(dstrdly, buf, strlen(buf));
      break;
    case FLOAT:
      sprintf(buf, "%lf", expr->floatconst);
      dscat(dstrdly, buf, strlen(buf));
      break;
    case IDENT: ;
      char* oofstr = treeid(expr->id);
      dscat(dstrdly, oofstr, strlen(oofstr));
      break;
    case ARRAY_LIT:
      //TODO: make this better
      for(int i = 0; i < expr->dynvals->length; i++) {
        long* longman = daget(expr->dynvals, i);
        sprintf(buf, "%lu", *longman);
        dscat(dstrdly, buf, strlen(buf));
      }
      break;
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE:
    case MULT: case DIVI: case MOD: case L_AND: case L_OR: case L_NOT: case B_AND:
    case B_OR: case B_XOR: case SHL: case SHR: case DOTOP: case ARROW: case ASSIGN:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
    case COMMA: ;
      char* e1 = treexpr(expr->param1);
      char* e2 = treexpr(expr->param2);
      dscat(dstrdly, e1, strlen(e1));
      dscat(dstrdly, docolor, strlen(docolor));
      dsccat(dstrdly, '$');
      dscat(dstrdly, e2, strlen(e2));
      dscat(dstrdly, docolor, strlen(docolor));
      break;
    case NEG: case PREINC: case POSTINC: case PREDEC: case POSTDEC: case ADDR: 
    case DEREF: case SZOFEXPR: ;
      char* e = treexpr(expr->unaryparam);
      dscat(dstrdly, e, strlen(e));
      dscat(dstrdly, docolor, strlen(docolor));
      break;
    case SZOF: ;
      char* c = treetype(expr->typesz);
      dscat(dstrdly, c, strlen(c));
      dscat(dstrdly, docolor, strlen(docolor));
      break;
    case CAST: ;
      char* cte = treexpr(expr->castexpr);
      dscat(dstrdly, cte, strlen(cte));
      dscat(dstrdly, docolor, strlen(docolor));
      dscat(dstrdly, " TO ", 5);
      char* ctt = treetype(expr->typesz);
      dscat(dstrdly, ctt, strlen(ctt));
      dscat(dstrdly, docolor, strlen(docolor));
      break;
    case FCALL:
      dscat(dstrdly, expr->ftocall->id->name, strlen(expr->ftocall->id->name));
      dscat(dstrdly, " PARAMS ", 9);
      for(int i = 0; i < expr->params->length; i++) {
        char* toapp = treexpr(daget(expr->params, i));
        dscat(dstrdly, toapp, strlen(toapp));
        dscat(dstrdly, docolor, strlen(docolor));
        dsccat(dstrdly, '$');
      }
      dscat(dstrdly, docolor, strlen(docolor));
      break;
    case TERNARY: ;
      char* tern = treexpr(expr->ifexpr);
      dscat(dstrdly, tern, strlen(tern));
      dscat(dstrdly, docolor, strlen(docolor));
      dscat(dstrdly, " THEN ", 7);
      tern = treexpr(expr->thenexpr);
      dscat(dstrdly, tern, strlen(tern));
      dscat(dstrdly, docolor, strlen(docolor));
      tern = treexpr(expr->elseexpr);
      dscat(dstrdly, tern, strlen(tern));
      dscat(dstrdly, " ELSE ", 7);
      dscat(dstrdly, docolor, strlen(docolor));
      break;
  }
  dsccat(dstrdly, ')');

}

void treefunc(FUNC* func) {
  printf("%s %s (", func->name);
  for(int i = 0; i < func->params->length; i++) {
  }

}
