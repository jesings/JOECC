#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "preparse.h"
#define SKIPWHITE(var) while(isspace(*var)) ++(var)
#define UNTILWHITE(var) while(!isspace(*var)) ++(var)
#define SKIPWHITEB(var, var2) while(isspace(*var) && var >= var2) --(var)
#define IFDEFGOOD(var) if(var->length > 0 && *(enum ifdefstate*) dapeek(var) != IFANDTRUE && *(enum ifdefstate*) dapeek(var) != ELSEANDFALSE) return 0
struct ppstate pps;
FILE* curstream = NULL;
int ppline(char* line, size_t linelen) {
  char* linenew = malloc(linelen + 1);
  memcpy(linenew, line, linelen + 1);
  size_t guysread = 256;
  char* lineptr = malloc(guysread);
  while(1) {
    char* bline = linenew + linelen - 1;
    SKIPWHITEB(bline, linenew);
    if(*bline == '\\') {
      size_t bread = getline(&lineptr, &guysread, curstream);
      if(bread == -1) {
        free(lineptr);
        break;//throw some garbage error idk
      }
      linelen = (bline - linenew) + bread;
      linenew = realloc(linenew, linelen + 1);
      memcpy(bline, lineptr, bread + 1);
    } else {
      break;
    }
  }
  if(lineptr) free(lineptr);
  line = linenew;
  SKIPWHITE(line);
  if(*line == '\0') return 0;
  if(*line == '#') {
    ++line;
    SKIPWHITE(line);
    char* beginword = line;
    UNTILWHITE(line);
    *line = '\0';
    ++line;
    //no elif, defined, 
    if(strcmp(beginword, "include") == 0) {
      IFDEFGOOD(pps.ifdefstack);

      SKIPWHITE(line);
      char* incfile = line;
      UNTILWHITE(line);
      char delim;
      if('<' == *incfile && '>' == *(line - 1)) {
        delim = '<';
      } else if('"' == *incfile && '"' == *(line - 1)) {
        delim = '"';
      } else {
        delim = 0;
        //malformed include
      }
      *(line - 1) = '\0';
      incfile++;
      FILE* incfiledes;
      if((incfiledes = fopen(incfile, "r")) != NULL) {
        FILE* tmp = curstream;
        curstream = incfiledes;
        preparse();
        curstream = tmp;
      }
    } else if (strcmp(beginword, "define") == 0) {
      IFDEFGOOD(pps.ifdefstack);
      SKIPWHITE(line);
      char* todef= line;
      UNTILWHITE(line);      
      *line = '\0';
      line++;
      //handle function like macros
      if(search(pps.defines, todef) != NULL) {//probably not necessary
      } else {
        //line[linelen - 1] = '\0';
        insert(pps.defines, todef, line);//confirm newline is stripped, etc
      }
    } else if (strcmp(beginword, "ifdef") == 0){
      //if already in a bad ifdef state, add a dummy to the stack
      SKIPWHITE(line);
      char* todef= line;
      UNTILWHITE(line);      
      *line = '\0';
      line++;
      enum ifdefstate* ids = malloc(sizeof(enum ifdefstate));
      if(search(pps.defines, todef) != NULL) {
        *ids = IFANDTRUE;
      } else {
        *ids = IFANDFALSE;
      }
      dapush(pps.ifdefstack, ids);
    } else if (strcmp(beginword, "ifndef") == 0){
      SKIPWHITE(line);
      char* todef= line;
      UNTILWHITE(line);      
      *line = '\0';
      line++;
      enum ifdefstate* ids = malloc(sizeof(enum ifdefstate));
      if(search(pps.defines, todef) == NULL) {
        *ids = IFANDTRUE;
      } else {
        *ids = IFANDFALSE;
      }
      dapush(pps.ifdefstack, ids);
    } else if (strcmp(beginword, "else") == 0){
      IFDEFGOOD(pps.ifdefstack);//dummy values are useless, this else doesn't change stack depth
      if(pps.ifdefstack->length > 0) {
        enum ifdefstate* destate = dapeek(pps.ifdefstack);
        if(*destate == IFANDTRUE) {
          *destate = ELSEANDTRUE;
        } else if(*destate == IFANDFALSE) {
          *destate = ELSEANDFALSE;
        } else {
          //error else doesn't mean anything
        }
      } else {
        //error, else doesn't mean anything
      }
    } else if (strcmp(beginword, "endif") == 0){
      //even if it is dummy value, we're still good
      if(pps.ifdefstack->length > 0) {
        dapop(pps.ifdefstack);
      } else {
        //error, endif doesn't mean anything
      }
    } else if (strcmp(beginword, "undef") == 0){
      IFDEFGOOD(pps.ifdefstack);
      SKIPWHITE(line);
      char* undef2 = line;
      UNTILWHITE(line);
      *line = '\0';
      line++;
      rmpair(pps.defines, undef2);
    }

  } else {
    IFDEFGOOD(pps.ifdefstack);
    puts(line);
  }
}
int preparse() {
  //read from stdin, output to stdout
  if(curstream == NULL)
    curstream = stdin;
  pps.ifdefstack = dactor(32);
  pps.defines = htctor();
  size_t len = 256;
  char* lineptr = malloc(len);
  size_t countread;
  while((countread = getline(&lineptr, &len, curstream)) != -1) {
    ppline(lineptr, countread);
  }
  free(lineptr);
  return 0;
}
