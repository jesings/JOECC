#include "opt.h"
char rmunreach(PROGRAM* prog) {
  DYNARR* oldall = prog->allblocks;
  DYNARR* newall = dactor(oldall->length);
  dapush(newall, daget(oldall, 0));
  char modified = 0;
  for(int i = 1; i < oldall->length; i++) {
    BBLOCK* oldb = daget(oldall, i);
    if(oldb->inedges->length == 0) {
      freeblock(oldb);
      modified = 1;
    } else {
      dapush(newall, oldb);
    }
  }
  prog->allblocks = newall;
  dadtor(oldall);
  return modified;
}
//http://www.cs.toronto.edu/~pekhimenko/courses/cscd70-w20/docs/Lecture 7 [Pointer Analysis] 03.09.2020.pdf
//http://ssabook.gforge.inria.fr/latest/book.pdf
//https://iitd-plos.github.io/col729/lec/loop_transformations.html
