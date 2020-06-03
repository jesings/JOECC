#ifndef PREPARSE_H
#define PREPARSE_H
#include "hash.h"
#include "dynarr.h"
int preparse();
enum ifdefstate {
  IFANDTRUE, IFANDFALSE, ELSEANDTRUE, ELSEANDFALSE
};
struct ppstate {
  DYNARR* ifdefstack;
  HASHTABLE* defines;
};
#endif

