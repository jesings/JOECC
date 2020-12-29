#ifndef DSTORE_H
#define DSTORE_H
#include "dynarr.h"
#include "dynstr.h"
#include "hash.h"
void initstores(void);
DYNSTR* gsmallds(int dssize);
DYNSTR* glargeds(int dssize);
DYNARR* gsmallda(int dasize);
DYNARR* glargeda(int dasize);
HASHTABLE* ght(void);
void deinitstores(void);
void rds(DYNSTR* ds);
void rda(DYNARR* da);
void rht(HASHTABLE* ht);
#endif
