#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include "compintern.h"
int funcfile;
int nodenumber;

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

int pdecl(DECLARATION* decl);
int treexpr(EXPRESSION* expr);
int structree(STRUCT* container);
int enumtree(ENUM* container);
int uniontree(UNION* container);

int structree(STRUCT* container) {
  int structnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"STRUCT %s\"];\n", structnode, container->name ? container->name : "ANONYMOUS");
  for(int i = 0; i < container->fields->length; i++) {
    DECLARATION* field = daget(container->fields, i);
    if(field->varname) {
      dprintf(funcfile, "n%d -> n%d;\n", structnode, pdecl(field));
    } else if(field->type->tb & ANONMEMB) {
      int (*memptr) (void*);
      if(field->type->tb & STRUCTVAL) {
        memptr = (int (*) (void*)) structree;
      } else if(field->type->tb & UNIONVAL) {
        memptr = (int (*) (void*)) uniontree;
      } else {
        exit(-1);
      }
      dprintf(funcfile, "n%d -> n%d;\n", structnode, memptr(field->type->structtype));
    } else  {
      exit(-1);
    }
  }
  return structnode;
}

int enumtree(ENUM* container) {
  int enumnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"STRUCT %s\"];\n", enumnode, container->name ? container->name : "ANONYMOUS");
  for(int i = 0; i < container->fields->length; i++) {
    ENUMFIELD* field = daget(container->fields, i);
    int nnn = nodenumber++;
    dprintf(funcfile, "n%d [label=\"%s\"];\n", nnn, field->name);
    dprintf(funcfile, "n%d -> n%d [color=red];\n", enumnode, nnn);
    dprintf(funcfile, "n%d -> n%d [color=green];\n", enumnode, treexpr(field->value));
  }
  return enumnode;
}

int uniontree(UNION* container) {
  int unionnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"UNION %s\"];\n", unionnode, container->name ? container->name : "ANONYMOUS");
  for(int i = 0; i < container->fields->length; i++) {
    DECLARATION* field = daget(container->fields, i);
    if(field->varname) {
      dprintf(funcfile, "n%d -> n%d;\n", unionnode, pdecl(field));
    } else if(field->type->tb & ANONMEMB) {
      int (*memptr) (void*);
      if(field->type->tb & STRUCTVAL) {
        memptr = (int (*) (void*)) structree;
      } else if(field->type->tb & UNIONVAL) {
        memptr = (int (*) (void*)) uniontree;
      } else {
        exit(-1);
      }
      dprintf(funcfile, "n%d -> n%d;\n", unionnode, memptr(field->type->structtype));
    } else  {
      exit(-1);
    }
  }
  return unionnode;
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

int treetype(IDTYPE* type) {
  int typenode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"TYPE\"];\n", typenode);
  int subtnode = nodenumber++;
  if(type->tb & ENUMVAL) {
    dprintf(funcfile, "n%d [label=\"ENUM %s\"];\n", subtnode, type->enumtype->name ? type->enumtype->name : "ANONYMOUS");
    if(!type->enumtype->name)
      /*dprintf(funcfile, "n%d -> n%d;\n", subtnode, enumtree(type->enumtype))*/;
  } else if(type->tb & STRUCTVAL) {
    dprintf(funcfile, "n%d [label=\"STRUCT %s\"];\n", subtnode, type->structtype->name ? type->structtype->name : "ANONYMOUS");
    if(!type->structtype->name)
      /*dprintf(funcfile, "n%d -> n%d;\n", subtnode, structree(type->structtype))*/;
  } else if(type->tb & UNIONVAL){ 
    dprintf(funcfile, "n%d [label=\"UNION %s\"];\n", subtnode, type->uniontype->name ? type->uniontype->name : "ANONYMOUS");
    if(!type->uniontype->name)
      /*dprintf(funcfile, "n%d -> n%d;\n", subtnode, uniontree(type->uniontype))*/;
  } else {
    char* ntb =  name_TYPEBITS(type->tb);
    dprintf(funcfile, "n%d [label=\"%s\"];\n", subtnode, ntb);
    free(ntb);
  }
  if(type->pointerstack && type->pointerstack->length)
    dprintf(funcfile, "n%d -> n%d [xlabel=\"%dx *\"];\n", typenode, subtnode, type->pointerstack->length);
  else
    dprintf(funcfile, "n%d -> n%d;\n", typenode, subtnode);
  return typenode;
}

int treeid(IDENTIFIERINFO* id) {
  int idnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"%s\"];\n", idnode, id->name);
  dprintf(funcfile, "n%d -> n%d;\n", idnode, treetype(id->type));
  return idnode;
}

int treexpr(EXPRESSION* expr) {
  int exnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"%s\"];\n", exnode, name_EXPRTYPE[expr->type]);
  int secondnodeary;
  switch(expr->type) {
    case NOP:
      break;
    case STRING:
      secondnodeary = nodenumber++;
      dprintf(funcfile, "n%d [label=\"%s\"];\n", secondnodeary, expr->strconst);
      dprintf(funcfile, "n%d -> n%d;\n", exnode, secondnodeary);
      break;
    case INT:
      secondnodeary = nodenumber++;
      dprintf(funcfile, "n%d [label=\"%ld\"];\n", secondnodeary, expr->intconst);
      dprintf(funcfile, "n%d -> n%d;\n", exnode, secondnodeary);
      break;
    case UINT:
      secondnodeary = nodenumber++;
      dprintf(funcfile, "n%d [label=\"%lu\"];\n", secondnodeary, expr->uintconst);
      dprintf(funcfile, "n%d -> n%d;\n", exnode, secondnodeary);
      break;
    case FLOAT:
      secondnodeary = nodenumber++;
      dprintf(funcfile, "n%d [label=\"%lf\"];\n", secondnodeary, expr->floatconst);
      dprintf(funcfile, "n%d -> n%d;\n", exnode, secondnodeary);
      break;
    case IDENT:
      //int ooftype = treeid(expr->id);
      secondnodeary = nodenumber++;
      dprintf(funcfile, "n%d [label=\"%s\"];\n", secondnodeary, expr->id->name);
      dprintf(funcfile, "n%d -> n%d;\n", exnode, secondnodeary);
      break;
    case MEMBER:
      secondnodeary = nodenumber++;
      dprintf(funcfile, "n%d [label=\"%s\"];\n", secondnodeary, expr->member);
      dprintf(funcfile, "n%d -> n%d;\n", exnode, secondnodeary);
      break;
    case ARRAY_LIT:
      for(int i = 0; i < expr->dynvals->length; i++) {
        EXPRESSION* longman = daget(expr->dynvals, i);
        dprintf(funcfile, "n%d -> n%d;\n", exnode, treexpr(longman));
      }
      break;
    case ADD: case SUB: case EQ: case NEQ: case GT: case LT: case GTE: case LTE:
    case MULT: case DIVI: case MOD: case L_AND: case L_OR: case B_AND: case COMMA:
    case B_OR: case B_XOR: case SHL: case SHR: case DOTOP: case ARROW: case ASSIGN:
    case ADDASSIGN: case SUBASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN:
    case XORASSIGN: case ORASSIGN: case DIVASSIGN: case MULTASSIGN: case MODASSIGN:
      for(int i = 0; i < expr->params->length; i++)
        dprintf(funcfile, "n%d -> n%d;\n", exnode, treexpr(expr->params->arr[i]));
      break;
    case NEG: case PREINC: case POSTINC: case PREDEC: case POSTDEC: case ADDR: 
    case DEREF: case SZOFEXPR: case L_NOT: case B_NOT:
      dprintf(funcfile, "n%d -> n%d;\n", exnode, treexpr(expr->params->arr[0]));
      break;
    case SZOF:
      dprintf(funcfile, "n%d -> n%d;\n", exnode, treetype(expr->vartype));
      break;
    case CAST:
      dprintf(funcfile, "n%d -> n%d [color=red];\n", exnode, treexpr(expr->params->arr[0]));
      dprintf(funcfile, "n%d -> n%d [color=green];\n", exnode, treetype(expr->vartype));
      break;
    case FCALL:
      dprintf(funcfile, "n%d -> n%d [color=red];\n", exnode, treexpr(expr->params->arr[0]));
      for(int i = 1; i < expr->params->length; i++) {
        dprintf(funcfile, "n%d -> n%d [color=green] [label=\"%d\"];\n", exnode, treexpr(expr->params->arr[i]), i);
      }
      break;
    case TERNARY:
      dprintf(funcfile, "n%d -> n%d [color=red];\n", exnode, treexpr(expr->params->arr[0]));
      dprintf(funcfile, "n%d -> n%d [color=green];\n", exnode, treexpr(expr->params->arr[1]));
      dprintf(funcfile, "n%d -> n%d [color=blue];\n", exnode, treexpr(expr->params->arr[2]));
      break;
  }
  return exnode;
}

int pdecl(DECLARATION* decl) {
  int declnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"%s\"];\n", declnode, "DECLARATION"); 
  int dnamenode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"%s\"];\n", dnamenode, decl->varname); 
  dprintf(funcfile, "n%d -> n%d [color=red];\n", declnode, treetype(decl->type));
  dprintf(funcfile, "n%d -> n%d [color=green];\n", declnode, dnamenode); 
  return declnode;
}

int prinit(DYNARR* dinit) {
  int printnode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"%s\"];\n", printnode, "INITIALIZER"); 
  for(int i = 0; i < dinit->length; i++) {
    INITIALIZER* init = daget(dinit, i);
    int decl = pdecl(init->decl);
    dprintf(funcfile, "n%d -> n%d [color=red];\n", printnode, decl); 
    if(init->expr) {
      int expr = treexpr(init->expr);
      dprintf(funcfile, "n%d -> n%d [color=green];\n", printnode, expr); 
    }
  }
  return printnode;
}

int statemeant(STATEMENT* stmt) {
  int statenode = nodenumber++;
  dprintf(funcfile, "n%d [label=\"%s\"];\n", statenode, name_STMTTYPE[stmt->type]); 
  switch(stmt->type) {
    case JGOTO: case LABEL: ;
      int lnn = nodenumber++;
      dprintf(funcfile, "n%d[label=\"%s\"];\n", lnn, stmt->glabel); 
      dprintf(funcfile, "n%d -> n%d;\n", statenode, lnn); 
      break; 
    case IFELSES:
        dprintf(funcfile, "n%d -> n%d [color=blue];\n", statenode, statemeant(stmt->elsecond)); 
    case IFS:
      dprintf(funcfile, "n%d -> n%d [color=red];\n", statenode, treexpr(stmt->ifcond)); 
      dprintf(funcfile, "n%d -> n%d [color=green];\n", statenode, statemeant(stmt->thencond)); 
      break;
    case SWITCH: ;//not final, should be working better
      PARALLEL* pl = stmt->labeltable;
      for(int i = 0; i < pl->da->length; i++) {
        int scnn = nodenumber++;
        char* lname = pl->da->arr[i];
        dprintf(funcfile, "n%d[label=\"%s\"];\n", scnn, lname);
        dprintf(funcfile, "n%d -> n%d [color=blue];\n", statenode, scnn); 
        dprintf(funcfile, "n%d -> n%d;\n", scnn, treexpr(psearch(pl, lname))); 
      }
    case WHILEL: case DOWHILEL: 
      dprintf(funcfile, "n%d -> n%d [color=red];\n", statenode, treexpr(stmt->cond)); 
      dprintf(funcfile, "n%d -> n%d [color=green];\n", statenode, statemeant(stmt->body)); 
      break;
    case LBREAK: case LCONT: case DEFAULT:
      break;
    case FRET: 
      if(!stmt->expression)
        break;
    case EXPR: case CASE:
      dprintf(funcfile, "n%d -> n%d;\n", statenode, treexpr(stmt->expression)); 
      break;
    case CMPND:
      if(stmt->stmtsandinits) {
        for(int i = 0; i < stmt->stmtsandinits->length; i++) {
          SOI* soi = daget(stmt->stmtsandinits, i);
          int soiopt = soi->isstmt ? statemeant(soi->state) : prinit(soi->init);
          dprintf(funcfile, "n%d -> n%d;\n", statenode, soiopt); 
        }
      }
      break;
    case NOPSTMT:
      break;
    default:
      exit(-1);
  }
  return statenode;
}

void treefunc(FUNC* func) {
  nodenumber = 0;
  char filename[256];
  sprintf(filename, "%s.dot", func->name);
  funcfile = creat(filename, 0666);
  int fnn = nodenumber++;
  dprintf(funcfile, "digraph %s {\ngraph [rankdir=LR];\nnode [shape=box];\ngraph [splines=ortho, nodesep=1];", func->name);
  dprintf(funcfile, "n%d [label=\"%s\"];\n", fnn, func->name); 
  int typenoden = treetype(func->retrn);
  dprintf(funcfile, "n%d -> n%d;\n", fnn, typenoden);
  for(int i = 0; i < func->params->da->length; i++) {
    DECLARATION* declsrc= search(func->params->ht, daget(func->params->da, i));
    if(!declsrc && (i == func->params->da->length - 1)) {
      dprintf(funcfile, "n%d [label=\"...\"];\n", nodenumber++); 
      dprintf(funcfile, "n%d -> n%d;\n", fnn, nodenumber - 1);
    } else {
      int parnum = pdecl(declsrc);
      dprintf(funcfile, "n%d -> n%d;\n", fnn, parnum);
    }
  }
  //params above, separate from body---by shape?
  int internode = statemeant(func->body);
  dprintf(funcfile, "n%d -> n%d;\n", fnn, internode);
  dprintf(funcfile, "}\n");
  close(funcfile);
}
