#include <string.h>
#include <stdlib.h>
#include <stdio.h>

char charconv(char** text);
int main(){
    char* s1 = "\\a";
    char* s2 = "\\n";
    char* s3 = "\\40";
    char* s4 = "\\101";
    char* s5 = "\\040";
    char* s6 = "\\x41";
    char* s7 = "\\xA";
    char* s8 = "H";
    char* s9 = "i";
    char* s10 = "!";
    char* s11 = "\\x0a";
    putchar(charconv(&s1));
    putchar(charconv(&s2));
    putchar(charconv(&s3));
    putchar(charconv(&s4));
    putchar(charconv(&s5));
    putchar(charconv(&s6));
    putchar(charconv(&s7));
    putchar(charconv(&s8));
    putchar(charconv(&s9));
    putchar(charconv(&s10));
    putchar(charconv(&s11));
    return 0;
}

char charconv(char** text){
  if(**text=='\\'){
    ++(*text);
    switch(*((*text)++)){
      case 'a':
        return '\a';
      case 'b':
        return '\b';
      case 'e': case 'E':
        return '\e';
      case 'f':
        return '\f';
      case 'n':
        return '\n';
      case 'r':
        return '\r';
      case 't':
        return '\t';
      case 'v':
        return '\v';
      case '\'':
        return '\'';
      case '"':
        return '"';
      case '\\':
        return '\\';
      case '?':
        return '?';
      case 'x': ;
        char* endofseq;
        unsigned long l = strtoul(*text, &endofseq, 16);
        if(l > 256){
          printf("WARNING: hex number too large to fit in char.");//Warning checking/ verbosity here?
          l = 0;
        }
        *text = (char) endofseq;
        return l;
      case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
        --(*text);
        char* endofoctseq;
        unsigned short s = (unsigned short) strtoul(*text, &endofoctseq, 8);
        if(endofoctseq - *text > 3{
          printf("ERROR: invalid char in string literal or char constant encountered.\n");//Warning checking/ verbosity here?
          s = 0;
        }
        char* malloc(3)
        *text = (char*) endofoctseq;
        return s;
      default:
        printf("WARNING: unkown escape sequence \\%c.\n", **text);//Warning checking/ verbosity here?
        return **text;
    }
  }
  return *((*text)++);
}
