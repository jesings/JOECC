#include <stdio.h>
#include "compintern.h"

#define COLOR(r, g, b) "\e[ 38;2;" #r ";" #g ";" #b "m"
#define HCOLOR(hex) COLOR((hex & 0xff0000) >> 16, (hex & 0xff00) >> 8,  hex & 0xff)

char* name_TYPEBITS(TYPEBITS tb) {
  char* vals[16];


  for(int sh = 0x1; sh < 0x8000; sh <<= 1) {
    switch(tb & sh) {
      case 0x1: rvs[index++] = "8-BIT"; break;
      case 0x2: rvs[index++] = "16-BIT"; break;
      case 0x4: rvs[index++] = "32-BIT"; break;
      case 0x8: rvs[index++] = "64-BIT"; break;
      case 0x10: rvs[index++] = "FLOATNUM"; break;
      case 0x20: rvs[index++] = "UNSIGNEDNUM"; break;
      case 0x40: rvs[index++] = "CONSTNUM"; break;
      case 0x80: rvs[index++] = "VOLATILENUM"; break;
      case 0x100: rvs[index++] = "STATICNUM"; break;
      case 0x200: rvs[index++] = "EXTERNNUM"; break;
      case 0x400: rvs[index++] = "PARAMNUM"; break;
      case 0x800: rvs[index++] = "VOIDNUM"; break;
      case 0x1000: rvs[index++] = "ENUMVAL"; break;
      case 0x2000: rvs[index++] = "STRUCTVAL"; break;
      case 0x4000: rvs[index++] = "UNIONVAL"; break;
    }
  }
}

void treefunc(FUNCTION* func) {
  printf("%s %s (", func->name);
  for(int i = 0; i < func->params->length; i++) {
  }

}
