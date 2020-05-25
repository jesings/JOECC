#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>
#include "conv.h"

//int main(){
//    char* s1 = "\\q";
//    char* s2 = "\\n";
//    char* s3 = "\\40";
//    char* s4 = "\\101";
//    char* s5 = "\\040";
//    char* s6 = "\\x41";
//    char* s7 = "\\xA";
//    char* s8 = "H";
//    char* s9 = "i";
//    char* s10 = "!";
//    char* s11 = "\\x0a";
//    putchar(charconv(&s1));
//    putchar(charconv(&s2));
//    putchar(charconv(&s3));
//    putchar(charconv(&s4));
//    putchar(charconv(&s5));
//    putchar(charconv(&s6));
//    putchar(charconv(&s7));
//    putchar(charconv(&s8));
//    putchar(charconv(&s9));
//    putchar(charconv(&s10));
//    putchar(charconv(&s11));
//    char* ws1 = "\\q";
//    char* ws2 = "\\n";
//    char* ws3 = "\\40";
//    char* ws4 = "\\101";
//    char* ws5 = "\\040";
//    char* ws6 = "\\x41";
//    char* ws7 = "\\xA";
//    char* ws8 = "H";
//    char* ws9 = "i";
//    char* ws10 = "!";
//    char* ws11 = "\\x0a";
//    putwchar(wcharconv(&ws1));
//    putwchar(wcharconv(&ws2));
//    putwchar(wcharconv(&ws3));
//    putwchar(wcharconv(&ws4));
//    putwchar(wcharconv(&ws5));
//    putwchar(wcharconv(&ws6));
//    putwchar(wcharconv(&ws7));
//    putwchar(wcharconv(&ws8));
//    putwchar(wcharconv(&ws9));
//    putwchar(wcharconv(&ws10));
//    putwchar(wcharconv(&ws11));
//
//    char* actualstring = "\"what the actual \n\t hell is going on ????\n\"";
//    puts(strconv(actualstring));
//    return 0;
//}

char octconv(char** text){
    char* begin = *text;
    unsigned short accuum = 0;
    while(*text - begin < 3) {
        switch(**text){
            case '0'...'7': 
                accuum <<= 3;
                accuum += **text - '0';
                break;
            default:
                return accuum;
        }
        ++(*text);
    }
    if(accuum > 255) {
        puts("warning: octal escape sequence out of range");
        return (char) accuum;
    }
    return accuum;
}
char hexconv(char** text){
    char* begin = *text;
    unsigned long accuum = 0;
    while(1) {
        char offset = 0;
        switch(**text){
            case '0'...'9': 
                offset = '0'; break;
            case 'a'...'f': 
                offset = 'a' - 10; break;
            case 'A'...'F':
                offset = 'A' - 10; break;
            default:
                if(begin == *text) {
                    puts("error: \\x used with no following hex digits");
                    return 'x';
                }
                return accuum;
        }
        //deal with out of range
        if(accuum < (1 << (8 * sizeof(char)))) {
            accuum <<= 4;
            accuum += **text - offset;
        }
        ++(*text);
    }
    if(accuum >= (1 << sizeof(char))) {
        puts("warning: hex escape sequence out of range");
        return (char) accuum;
    }
    return accuum;
}
wchar_t whexconv(char** text){
    char* begin = *text;
    unsigned long accuum = 0;
    while(1) {
        char offset = 0;
        switch(**text){
            case '0'...'9': 
                offset = '0'; break;
            case 'a'...'f': 
                offset = 'a' - 10; break;
            case 'A'...'F':
                offset = 'A' - 10; break;
            default:
                if(begin == *text) {
                    puts("error: \\x used with no following hex digits");
                    return 'x';
                }
                return accuum;
        }
        //deal with out of range
        if(accuum < (1L << (8 * sizeof(wchar_t)))) {
            accuum <<= 4;
            accuum += **text - offset;
        }
        ++(*text);
    }
    if(accuum >= (1 << sizeof(wchar_t))) {
        puts("warning: hex escape sequence out of range");
    }
    return accuum;
}

char charconv(char** text){
    if(**text=='\\'){
        ++(*text);
        switch(*((*text)++)){
            case 'a':
                return '\a';
            case 'b':
                return '\b';
            case 'e': /*case 'E':*/
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
                return hexconv(text);
            case '0' ... '7':
                --*text;
                return octconv(text);
            default:
                printf("WARNING: unkown escape sequence \\%c.\n", *(*text - 1));//Warning checking/ verbosity here?
                return *(*text - 1);
        }
    }
    return *((*text)++);
}

char* strconv(char* text) {
    unsigned int index = 0;
    char* newstr = malloc(strlen(text) + 1);
    char* tstart = text;
    ++text;
    while(*text != '\"') {
        newstr[index++] = charconv(&text);
    }
    newstr[index] = 0;
    //free(tstart);
    return newstr;
}

wchar_t wcharconv(char** text){
    if(**text=='\\'){
        ++(*text);
        switch(*((*text)++)){
            case 'a':
                return L'\a';
            case 'b':
                return L'\b';
            case 'e': /*case 'E':*/
                return L'\e';
            case 'f':
                return L'\f';
            case 'n':
                return L'\n';
            case 'r':
                return L'\r';
            case 't':
                return L'\t';
            case 'v':
                return L'\v';
            case '\'':
                return L'\'';
            case '"':
                return L'"';
            case '\\':
                return L'\\';
            case '?':
                return L'?';
            case 'x': ;
                return whexconv(text);
            case '0' ... '7':
                --*text;
                return (wchar_t) octconv(text);
            default:
                printf("WARNING: unkown escape sequence \\%c.\n", *(*text - 1));//Warning checking/ verbosity here?
                return *(*text - 1);
        }
    }
    return *((*text)++);
}

wchar_t* wstrconv(char* text) {
    unsigned int index = 0;
    wchar_t* newstr = malloc((strlen(text) + 1) * sizeof(wchar_t));
    char* tstart = text;
    while(*text != '\"')
        newstr[index++] = charconv(&text);
    newstr[index] = 0;
    //free(tstart);
    return newstr;
}
