#include "compintern.h"

STRUCT* structor(char* name, DYNARR* fields) {
    STRUCT* retval = malloc(sizeof(STRUCT));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

UNION* unionctor(char* name, DYNARR* fields) {
    UNION* retval = malloc(sizeof(UNION));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

ENUM* enumctor(char* name, DYNARR* fields) {
    ENUM* retval = malloc(sizeof(ENUM));
    retval->name = name;
    retval->fields = fields;
    return retval;
}

EXPRESSION* cloneexpr(EXPRESSION* orig) {
  EXPRESSION* clone = malloc(sizeof(EXPRESSION));
  memcpy(clone, orig, sizeof(EXPRESSION));
  return clone;
}

EXPRESSION* ct_unary_expr(EXPRTYPE t, EXPRESSION* param) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = t;
  retval->unaryparam = param;
  return retval;
}

EXPRESSION* ct_sztype(IDTYPE* whichtype) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = SZOF;
  retval->typesz = whichtype;
  return retval;
}

EXPRESSION* ct_binary_expr(EXPRTYPE t, EXPRESSION* param1, EXPRESSION* param2) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));

  retval->type = t;
  retval->param1 = param1;
  retval->param2 = param2;
  return retval;
}

EXPRESSION* ct_cast_expr(IDTYPE* type, EXPRESSION* expr ) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = CAST;
  retval->castexpr = expr;
  retval->casttype = type;
  return retval;
}

EXPRESSION* ct_ternary_expr(EXPRESSION* param1, EXPRESSION* param2, EXPRESSION* param3) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = TERNARY;
  retval->ifexpr = param1;
  retval->thenexpr = param2;
  retval->elseexpr = param2;
  return retval;
}

EXPRESSION* ct_fcall_expr(EXPRESSION* func, int num, EXPRESSION* params) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = FCALL;
  retval->ftocall = func;
  retval->numparams = num;
  retval->params = params;
  return retval;
}

EXPRESSION* ct_strconst_expr(char* str) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = STRING;
  retval->strconst = strdup(str);//may or may not need to duplicate
  return retval;
}

EXPRESSION* ct_wstrconst_expr(wchar_t* str) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = WSTRING;
  retval->wstrconst = wcsdup(str);//may or may not need to duplicate
  return retval;
}

EXPRESSION* ct_intconst_expr(long num) { 
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->intconst = num;
  return retval;
}

EXPRESSION* ct_uintconst_expr(unsigned long num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = INT;
  retval->uintconst = num;
  return retval;
}

EXPRESSION* ct_floatconst_expr(double num) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));\
  retval->type = FLOAT;
  retval->floatconst = num;
  return retval;
}

EXPRESSION* ct_ident_expr(/*IDENTIFIERINFO* id*/ char* ident) {
  EXPRESSION* retval = malloc(sizeof(EXPRESSION));
  retval->type = IDENT;
  retval->ident = ident;
  return retval;
}

DECLARATOR* mkdeclarator(char* name) {
  DECLARATOR* retval = calloc(1,sizeof(DECLARATOR));
  retval->idname = name;
  retval->declparts = dactor(4);
  return retval;
}

INITIALIZER* geninit(DECLARATOR* decl, EXPRESSION* expr) {
  INITIALIZER* retval = malloc(sizeof(INITIALIZER));
  retval->decl = decl;
  retval->expr = expr;
  return retval;
}

SOI* sois(struct stmt* state) {
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 1;
  retval->state = state;
  return retval;
}

SOI* soii(DYNARR* init) {
  SOI* retval = malloc(sizeof(SOI));
  retval->isstmt = 0;
  retval->init = init;
  return retval;
}

STATEMENT* mkexprstmt(enum stmttype type, EXPRESSION* express) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->expression = express;
  return retval;
}

STATEMENT* mkgotostmt(char* gotoloc) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = JGOTO;
  retval->glabel = gotoloc;
  return retval;
}

STATEMENT* mkforstmt(EXPRESSION* e1, EXPRESSION* e2, EXPRESSION* e3, STATEMENT* bdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = FORL;
  retval->init = e1;
  retval->forcond = e2;
  retval->increment = e3;
  retval->forbody = bdy;
  return retval;
}

STATEMENT* mklsstmt(enum stmttype type, EXPRESSION* condition, STATEMENT* bdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = type;
  retval->cond = condition;
  retval->body = bdy;
  return retval;
}

STATEMENT* mkifstmt(EXPRESSION* condition, STATEMENT* ifbdy, STATEMENT* elsebdy) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = IFS;
  retval->ifcond = condition;
  retval->thencond = ifbdy;
  retval->elsecond = elsebdy;
  return retval;
}

STATEMENT* mkcmpndstmt(DYNARR* stmtsandinits) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CMPND;
  retval->stmtsandinits = stmtsandinits;
  return retval;
}

STATEMENT* mklblstmt(char* identifier) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = LABEL;
  retval->glabel = identifier;
  return retval;
}

STATEMENT* mkcasestmt(EXPRESSION* casexpr/*, STATEMENT* stmt*/) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = CASE;
  retval->cond = casexpr;
  //retval->body = stmt;
  return retval;
}

STATEMENT* mkdefaultstmt(STATEMENT* stmt) {
  STATEMENT* retval = malloc(sizeof(STATEMENT));
  retval->type = DEFAULT;
  retval->substatement = stmt;
  return retval;
}

ENUMFIELD* genenumfield(char* name, EXPRESSION* value) {
  ENUMFIELD* retval = malloc(sizeof(ENUMFIELD));
  retval->name = name;
  //confirm is expression statement
  //confirm is expr that will resolve into something uintconstifiable that is known at compile time
  retval->value = value;//this is worthless TODO: FIX
  return retval;
}

struct declarator_part* mkdeclpart(enum declpart_info typ, void* d) {
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = typ;
  retval->garbage = d;
  return retval;
}

struct declarator_part* mkdeclptr(TYPEBITS d) {
  struct declarator_part* retval = malloc(sizeof(struct declarator_part));
  retval->type = POINTERSPEC;
  retval->ptrspec = d;
  return retval;
}

DECLARATION* mkdeclaration(char* name) {
  DECLARATION* retval = malloc(sizeof(DECLARATION));
  retval->varname = name;
  //retval->type = dactor(4);
  return retval;
}

EXPRESSION* exprfromdecl(char* name, IDTYPE* id) {
  EXPRESSION* outer = malloc(sizeof(EXPRESSION));
  outer->type = IDENT;
  //outer->id = id;
  DYNARR* ptrs = id->pointerstack;
  if(ptrs) {
    for(int i = 0; i<ptrs->length; i++) {
      //TODO
    }
  }
  return outer;
}

FUNC* ct_function(char* name, STATEMENT* body, DYNARR* params, IDTYPE* retrn) {
  FUNC* func = malloc(sizeof(FUNC));
  func->name = name;
  func->body = body;
  func->params = params;
  func->retrn = retrn;
  return func;
}

SCOPE* mkscope(SCOPE* parent) {
  SCOPE* child = malloc(sizeof(SCOPE));
  child->identifiers = htclone(parent->identifiers);
  return child;
}

void scopepush(struct lexctx* ctx) {
  SCOPE* child = mkscope(dapeek(ctx->scopes));
  dapush(ctx->scopes, child);
}

void add2scope(SCOPE* scope, char* memname, enum membertype mtype, void* memberval) {
  SCOPEMEMBER sm = malloc(sizeof(SCOPEMEMBER));
  sm->mtype = mtype;
  sm->garbage = memberval;
  insert(SCOPE->members, memname, sm);
}

TOPBLOCK* gtb(char isfunc, void* assign) {
  TOPBLOCK* retval = malloc(sizeof(TOPBLOCK));
  retval->isfunc = isfunc;
  retval->garbage = assign;
  return retval;
}
