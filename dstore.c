#include <pthread.h>
#include "dstore.h"

DYNARR* smalldsstore,* largedsstore;
DYNARR* smalldastore,* largedastore;
DYNARR* htstore;
pthread_mutex_t sss, lss, sas, las, hts;

//TODO: if more than 32, free next ones
void initstores(void) {
  smalldsstore = dactor(32);
  largedsstore = dactor(32);
  smalldastore = dactor(32);
  smalldsstore = dactor(32);
  htstore = dactor(32);
  pthread_mutex_init(&sss, NULL);
  pthread_mutex_init(&lss, NULL);
  pthread_mutex_init(&sas, NULL);
  pthread_mutex_init(&las, NULL);
  pthread_mutex_init(&hts, NULL);
}

DYNSTR* gsmallds(int dssize) {
  pthread_mutex_lock(&sss);
  DYNSTR* retval = smalldsstore->length ? dapop(smalldsstore) : strctor(malloc(dssize), 0, dssize);
  pthread_mutex_unlock(&sss);
  return retval;
}

DYNSTR* glargeds(int dssize) {
  pthread_mutex_lock(&lss);
  DYNSTR* retval = largedsstore->length ? dapop(largedsstore) : strctor(malloc(dssize), 0, dssize);
  pthread_mutex_unlock(&lss);
  return retval;
}

DYNARR* gsmallda(int dasize) {
  pthread_mutex_lock(&sas);
  DYNARR* retval = smalldastore->length ? dapop(smalldastore) : dactor(dasize);
  pthread_mutex_unlock(&sas);
  return retval;
}

DYNARR* glargeda(int dasize) {
  pthread_mutex_lock(&las);
  DYNARR* retval = largedastore->length ? dapop(largedastore) : dactor(dasize);
  pthread_mutex_unlock(&las);
  return retval;
}

HASHTABLE* ght(void) {
  pthread_mutex_lock(&hts);
  HASHTABLE* retval = htstore->length ? dapop(htstore) : htctor();
  pthread_mutex_unlock(&hts);
  return retval;
}

void deinitstores(void) {
  dadtorcfr(smalldsstore, (void(*)(void*))strdtor);
  dadtorcfr(largedsstore, (void(*)(void*))strdtor);
  dadtorcfr(smalldastore, (void(*)(void*))dadtor);
  dadtorcfr(largedastore, (void(*)(void*))dadtor);
  dadtorcfr(htstore, free);
}

void rds(DYNSTR* ds) {
  if(ds->maxlenstr < 3) {
    strdtor(ds);
  } else {
    ds->lenstr = 0;
    if(ds->maxlenstr >= 128) {
      pthread_mutex_lock(&sss);
      dapush(smalldsstore, ds);
      pthread_mutex_unlock(&sss);
    } else {
      pthread_mutex_lock(&lss);
      dapush(largedsstore, ds);
      pthread_mutex_unlock(&lss);
    }
  }
}

void rda(DYNARR* da) {
  if(da->maxlength < 3) {
    dadtor(da);
  } else {
    da->length = 0;
    if(da->maxlength >= 128) {
      pthread_mutex_lock(&sas);
      dapush(smalldastore, da);
      pthread_mutex_unlock(&sas);
    } else {
      pthread_mutex_lock(&las);
      dapush(largedastore, da);
      pthread_mutex_unlock(&las);
    }
  }
}
void rht(HASHTABLE* ht) {
  //wipe, zero(lazily?) ht
}
