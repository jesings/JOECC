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

#define COLOR(r, g, b) "\033[38;2;" #r ";" #g ";" #b "m"
#define HCOLOR(hex) COLOR((hex & 0xff0000) >> 16, (hex & 0xff00) >> 8,  hex & 0xff)
char* rainbow[] = {COLOR(148, 0, 211), COLOR(180, 0, 180), COLOR(0, 0, 255), COLOR(0, 255, 0), COLOR(255, 255, 0), COLOR(255, 127, 0), COLOR(255, 0 , 0)};
char rainbowpos = 0;

char* pdecl(DECLARATION* decl);

char* structree(STRUCT* container) {
  DYNSTR* dstrdly = strctor(malloc(1024), 0, 1024);
  dscat(dstrdly, "FIELDS: ", 8);
  for(int i = 0; i < container->fields->length; i++) {
    DECLARATION* field = daget(container->fields, i);
    char* fieldstr = pdecl(field);
    dscat(dstrdly, fieldstr, strlen(fieldstr));
    dscat(dstrdly, " $F$ ", 5);
  }
  dscat(dstrdly, "$FIELDSOVER$", 12);
  dsccat(dstrdly, 0);
  char* rv = dstrdly->strptr;
  free(dstrdly);
  return rv;
}

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

char* treetype(IDTYPE* type) {
  DYNSTR* dstrdly = strctor(malloc(512), 0, 512);
  if(type->pointerstack)
    for(int i = 0; i < type->pointerstack->length; i++)
      dscat(dstrdly, "POINTER TO ", 11);
  if(type->tb & ENUMVAL) {
    dscat(dstrdly, "ENUM ", 5);
    if(type->enumtype && type->enumtype->name)
      dscat(dstrdly, type->enumtype->name, strlen(type->enumtype->name));
    else
      dscat(dstrdly, "ANONYMOUS ", 10);
  } else if(type->tb & STRUCTVAL) {
    dscat(dstrdly, "STRUCT ", 7);
    if(type->structtype && type->structtype->name)
      dscat(dstrdly, type->structtype->name, strlen(type->structtype->name));
    else {
      dscat(dstrdly, "ANONYMOUS ", 10);
      char* isc = structree(type->structtype);
      dscat(dstrdly, isc, strlen(isc));
    }
  } else if(type->tb & UNIONVAL){ 
    dscat(dstrdly, "UNION ", 6);
    if(type->uniontype && type->uniontype->name)
      dscat(dstrdly, type->uniontype->name, strlen(type->uniontype->name));
    else {
      dscat(dstrdly, "ANONYMOUS ", 10);
      char* isc = structree(type->structtype);
      dscat(dstrdly, isc, strlen(isc));
    }
  } else {
    char* istb = name_TYPEBITS(type->tb);
    dscat(dstrdly, istb, strlen(istb));
  }
  dsccat(dstrdly, 0);
  char* rv = dstrdly->strptr;
  free(dstrdly);
  return rv;
}

char* treeid(IDENTIFIERINFO* id) {
  DYNSTR* dstrdly = strctor(malloc(2048), 0, 2048);
  char* typespec = treetype(id->type);
  dscat(dstrdly, typespec, strlen(typespec));
  free(typespec);
  char* whitecolor = COLOR(30, 30, 30);
  dscat(dstrdly, whitecolor, strlen(whitecolor));
  dscat(dstrdly, id->name, strlen(id->name));
  dsccat(dstrdly, 0);
  char* strptr = dstrdly->strptr;
  free(dstrdly);
  return strptr;
}

char* treexpr(EXPRESSION* expr) {
  DYNSTR* dstrdly = strctor(malloc(2048), 0, 2048);
  char* docolor = rainbow[rainbowpos = (rainbowpos + 1) % 7];
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
      //char* oofstr = treeid(expr->id);
      //dscat(dstrdly, oofstr, strlen(oofstr));
      dscat(dstrdly, expr->ident, strlen(expr->ident));
      //free(oofstr);
      break;
    case ARRAY_LIT:
      //TODO: perhaps we want more complicated array handling
      for(int i = 0; i < expr->dynvals->length; i++) {
        EXPRESSION* longman = daget(expr->dynvals, i);
        sprintf(buf, "%lu,", longman->intconst);
        dscat(dstrdly, buf, strlen(buf));
      }
      break;
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE:
    case MULT: case DIVI: case MOD: case L_AND: case L_OR: case B_AND: case COMMA:
    case B_OR: case B_XOR: case SHL: case SHR: case DOTOP: case ARROW: case ASSIGN:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      ;
      char* e1 = treexpr(expr->param1);
      char* e2 = treexpr(expr->param2);
      dscat(dstrdly, e1, strlen(e1));
      dscat(dstrdly, docolor, strlen(docolor));
      dsccat(dstrdly, '$');
      dscat(dstrdly, e2, strlen(e2));
      free(e1);
      free(e2);
      break;
    case NEG: case PREINC: case POSTINC: case PREDEC: case POSTDEC: case ADDR: 
    case DEREF: case SZOFEXPR: case L_NOT: ;
      char* e = treexpr(expr->unaryparam);
      dscat(dstrdly, e, strlen(e));
      free(e);
      break;
    case SZOF: ;
      char* c = treetype(expr->typesz);
      dscat(dstrdly, c, strlen(c));
      free(c);
      break;
    case CAST: ;
      char* cte = treexpr(expr->castexpr);
      dscat(dstrdly, cte, strlen(cte));
      dscat(dstrdly, docolor, strlen(docolor));
      dscat(dstrdly, " TO ", 4);
      free(cte);
      char* ctt = treetype(expr->typesz);
      dscat(dstrdly, ctt, strlen(ctt));
      free(ctt);
      break;
    case FCALL:
      dscat(dstrdly, expr->ftocall->ident, strlen(expr->ftocall->ident));
      dscat(dstrdly, " PARAMS ", 8);
      for(int i = 0; i < expr->params->length; i++) {
        char* toapp = treexpr(daget(expr->params, i));
        dscat(dstrdly, toapp, strlen(toapp));
        dscat(dstrdly, docolor, strlen(docolor));
        dsccat(dstrdly, '$');
        free(toapp);
      }
      dscat(dstrdly, docolor, strlen(docolor));
      break;
    case TERNARY: ;
      char* tern = treexpr(expr->ifexpr);
      dscat(dstrdly, tern, strlen(tern));
      dscat(dstrdly, docolor, strlen(docolor));
      free(tern);
      dscat(dstrdly, " THEN ", 6);
      tern = treexpr(expr->thenexpr);
      dscat(dstrdly, tern, strlen(tern));
      dscat(dstrdly, docolor, strlen(docolor));
      free(tern);
      dscat(dstrdly, " ELSE ", 6);
      tern = treexpr(expr->elseexpr);
      dscat(dstrdly, tern, strlen(tern));
      free(tern);
      break;
  }
  dscat(dstrdly, docolor, strlen(docolor));
  dsccat(dstrdly, ')');
  dsccat(dstrdly, 0);
  char* strptr = dstrdly->strptr;
  free(dstrdly);
  return strptr;
}

char* pdecl(DECLARATION* decl) {
  DYNSTR* dstrdly = strctor(malloc(2048), 0, 2048);
  char* docolor = COLOR(255, 255, 255);
  dscat(dstrdly, docolor, strlen(docolor));
  char* tt = treetype(decl->type);
  dscat(dstrdly, tt, strlen(tt));
  free(tt);
  dscat(dstrdly, docolor, strlen(docolor));
  dscat(dstrdly, decl->varname, strlen(decl->varname));
  dsccat(dstrdly, 0);
  char* rv = dstrdly->strptr;
  free(dstrdly);
  return rv;
}

char* prinit(DYNARR* dinit) {
  DYNSTR* dstrdly = strctor(malloc(2048), 0, 2048);
  dsccat(dstrdly, '\n');
  for(int i = 0; i < dinit->length; i++) {
    INITIALIZER* init = daget(dinit, i);
    char* docolor = COLOR(255, 255, 255);
    dscat(dstrdly, docolor, strlen(docolor));
    char* decl = pdecl(init->decl);
    dscat(dstrdly, decl, strlen(decl));
    free(decl);
    dscat(dstrdly, docolor, strlen(docolor));
    if(init->expr) {
      dscat(dstrdly, docolor, strlen(docolor));
      dscat(dstrdly, " ASSIGNS ", 9);
      char* expr = treexpr(init->expr);
      dscat(dstrdly, expr, strlen(expr));
      free(expr);
    }
  }
  dsccat(dstrdly, 0);
  char* rv = dstrdly->strptr;
  free(dstrdly);
  return rv;
}

char* statemeant(STATEMENT* stmt) {
  char* docolor = rainbow[rainbowpos = (rainbowpos + 1) % 7];
  DYNSTR* dstrdly = strctor(malloc(2048), 0, 2048);
  dscat(dstrdly, docolor, strlen(docolor));
  dscat(dstrdly, name_STMTTYPE[stmt->type], strlen(name_STMTTYPE[stmt->type]));
  dsccat(dstrdly, ' ');
  switch(stmt->type) {
    case FORL:
      dscat(dstrdly, "=> ", 3);
      char* eoistr;
      if(stmt->init->isE)
        eoistr = treexpr(stmt->init->E);
      else
        eoistr = prinit(stmt->init->I);
      dscat(dstrdly, eoistr, strlen(eoistr));
      dscat(dstrdly, docolor, strlen(docolor));
      free(eoistr);
      dscat(dstrdly, "\nCOND=> ", 8);
      eoistr = treexpr(stmt->forcond);
      dscat(dstrdly, eoistr, strlen(eoistr));
      dscat(dstrdly, docolor, strlen(docolor));
      free(eoistr);
      dscat(dstrdly, "\nAFTER=> ", 9);
      eoistr = treexpr(stmt->increment);
      dscat(dstrdly, eoistr, strlen(eoistr));
      dscat(dstrdly, docolor, strlen(docolor));
      free(eoistr);
      dscat(dstrdly, "\nOVER=> ", 8);
      char* forbodystr = statemeant(stmt->forbody);
      dscat(dstrdly, forbodystr, strlen(forbodystr));
      free(forbodystr);
      break;
    case JGOTO: case LABEL:
      dscat(dstrdly, stmt->glabel, strlen(stmt->glabel));
      break;
    case IFS: case IFELSES:
      dscat(dstrdly, "\nCOND=> ", 8);
      char* ifcondstr = treexpr(stmt->ifcond);
      dscat(dstrdly, ifcondstr, strlen(ifcondstr));
      dscat(dstrdly, docolor, strlen(docolor));
      free(ifcondstr);
      dscat(dstrdly, "\nTHEN ", 6);
      char* ifbodystr = statemeant(stmt->thencond);
      dscat(dstrdly, ifbodystr, strlen(ifbodystr));
      dscat(dstrdly, docolor, strlen(docolor));
      free(ifbodystr);
      if(stmt->elsecond) {
        dscat(dstrdly, "\nELSE ", 6);
        ifbodystr = statemeant(stmt->elsecond);
        dscat(dstrdly, ifbodystr, strlen(ifbodystr));
        free(ifbodystr);
      }
      break;
    case WHILEL: case DOWHILEL: case SWITCH:
      dscat(dstrdly, "\nCOND=> ", 8);
      char* condstr = treexpr(stmt->cond);
      dscat(dstrdly, condstr, strlen(condstr));
      dscat(dstrdly, docolor, strlen(docolor));
      free(condstr);
      dscat(dstrdly, "\nOVER=> ", 8);
      char* bodystr = statemeant(stmt->body);
      dscat(dstrdly, bodystr, strlen(bodystr));
      free(bodystr);
      break;
    case LBREAK: case LCONT: case DEFAULT:
      break;
    case FRET: case EXPR: case CASE:
      dscat(dstrdly, "ON ", 3);
      char* expor = treexpr(stmt->expression);
      dscat(dstrdly, expor, strlen(expor));
      free(expor);
      break;
    case CMPND:
      dsccat(dstrdly, '{');
      for(int i = 0; i < stmt->stmtsandinits->length; i++) {
        dscat(dstrdly, "\n=> ", 4);
        SOI* soi = daget(stmt->stmtsandinits, i);
        char* lineptr;
        if(soi->isstmt) {
          lineptr = statemeant(soi->state);
        } else {
          lineptr = prinit(soi->init);
        }
        dscat(dstrdly, lineptr, strlen(lineptr));
        free(lineptr);
        dscat(dstrdly, docolor, strlen(docolor));
      }
      dsccat(dstrdly, '}');
      break;
    case NOPSTMT:
      break;
  }
  dsccat(dstrdly, 0);
  char* rv = dstrdly->strptr;
  free(dstrdly);
  return rv;
}

void treefunc(FUNC* func) {
  char* docolor = COLOR(100, 255, 100);
  printf("%s%s %s (\n", docolor, treetype(func->retrn), func->name);
  for(int i = 0; i < func->params->length; i++) {
    char* strecl = pdecl(daget(func->params, i));
    puts(strecl);
    free(strecl);
    printf("$F$");
  }
  printf("%s", docolor);
  puts("\nPARAMS OVER");
  puts("{");
  char* internbody = statemeant(func->body);
  puts(internbody);
  free(internbody);
  printf("%s", docolor);
  puts("}");
  puts("FUNCEND");
  puts("------------------------------------------------------------------------------");
}
