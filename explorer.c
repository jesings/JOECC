#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include "compintern.h"
#include "printree.h"
#include "3ac.h"

#define CLEAR "\033[H\033[J"

void display_block(PROGRAM* prog, BBLOCK* blk) {
  puts(CLEAR);

}
