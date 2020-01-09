#include <string.h>

long int charconv(char* text){
  char* convhead = text+1;
  char* writehead = text;
  while(*convhead!='"'){
    if(*convhead=='\\'){
      ++convhead;
      switch(*convhead){
        case 'a':
          *writehead = '\a';
          break;
        case 'b':
          *writehead = '\b';
          break;
        case 'f':
          *writehead = '\f';
          break;
        case 'n':
          *writehead = '\n';
          break;
        case 'r':
          *writehead = '\r';
          break;
        case 't':
          *writehead = '\t';
          break;
        case 'v':
          *writehead = '\v';
          break;
        case '\'':
          *writehead = '\'';
          break;
        case '"':
          *writehead = '"';
          break;
        case '\\':
          *writehead = '\\';
          break;
        case '?':
          *writehead = '?';
          break;
        case 'x':
          char tempval = 0, iters = 0;
          while(iters<2 && (*(convhead+1)-'0'<10 || tolower(*(convhead+1)-'f'<7))){
            ++convhead;
            tempval<<=4;
            tempval+=tolower(*convhead)!=*convhead?tolower(*convhead)-'a'+10:*convhead-'0';
          }
          if(!iters){
              //error about escape sequences
          }
          *writehead = tempval;
          break;
        case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7: case 8:
          //octal shit
        default:
          printf("WARNING: unkown escape sequence \\");//Warning checking/ verbosity here?
          putchar(*convhead);
          putchar('\n');
          *writehead = *convhead;
    }
    else
      *writehead = *convhead;
    ++convhead;
    ++writehead;
  }
  *writehead = NULL;
  return text;
}
