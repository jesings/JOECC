#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"

#define CLEAR "\033[H\033[J"

void display_block(PROGRAM* prog, BBLOCK* blk) {
  //assert stdin/stdout both terminals??
  struct termios orig_attrs;
  struct termios new_attrs;
  tcgetattr(STDIN_FILENO, &orig_attrs);
  new_attrs = orig_attrs;
  new_attrs.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL | IEXTEN); //stop cooking the output!
  new_attrs.c_cc[VMIN] = 1; //reads go character by character
  new_attrs.c_cc[VTIME] = 0; //never timeout on the read

  tcsetattr(STDIN_FILENO, TCSANOW, &new_attrs);
  char readchar;
  while(read(STDIN_FILENO, &readchar, 1) > 0) {
    puts(CLEAR);
    //figure out raw arrow keys?
    switch(readchar) {
      case '\x1b':
        //escape character
	goto cleanup;
      case 'w':
        break;
      case 'a':
        break;
      case 's':
        break;
      case 'd':
        break;
      default:
	    break;
    }

    //print block... shortened?
    //print liveness information
    //print dominance information
    //print location information
  }

cleanup:
  tcsetattr(STDIN_FILENO, TCSANOW, &orig_attrs);
}
