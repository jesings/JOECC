#include <stdio.h>
#include <stdlib.h>
#include "hash.h"
#include "dynarr.h"

int main() {

    //HASHMAP TESTS
 
    HASHTABLE* myhash = htctor();
    long longman[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    insert(myhash, "asdf", &longman[2]);
    insert(myhash, "foobar", &longman[3]);
    insert(myhash, "frederick", &longman[1]);
    insert(myhash, "", &longman[8]);
    insert(myhash, "e", &longman[6]);
    insert(myhash, "supposition", &longman[0]);

    void* testval = search(myhash, "nope");
    if(testval)
        return -1;
    testval = search(myhash, "foobar");
    printf("foobar is %ld\n", *(long*) testval);
    testval = search(myhash, "frederick");
    printf("frederick is %ld\n", *(long*) testval);
    insert(myhash, "foobar", &longman[8]);
    testval = search(myhash, "foobar");
    printf("foobar is %ld\n", *(long*) testval);

    HASHTABLE* urhash = htclone(myhash);
    
    testval = search(urhash, "supposition");
    printf("supposition is %ld\n", *(long*) testval);

    rmpair(myhash, "asdf");
    rmpair(myhash, "foobar");
    rmpair(myhash, "frederick");
    rmpair(myhash, "");
    rmpair(myhash, "e");
    rmpair(myhash, "supposition");
    htdtor(urhash);
    urhash = htclone(myhash);
    for(int i = 0; i < 4097; i++){
        char buf[8];
        sprintf(buf, "%d\0", i);
        long* e = malloc(8);
        *e = (long) (i + 2);
        insert(urhash, buf, e);
    }

    for(int i = 0; i < 4097; i++){
        char buf[8];
        sprintf(buf, "%d\0", i);
        void* v = search(urhash, buf);
        if(i == 4096)
            printf("%s goes to %ld\n", buf, *(long*) v);
    }
    htdtorfr(myhash);
    htdtorfr(urhash);
    
    //DYNARR TESTS

    DYNARR* da = dactor(2);
    for(int i = 0; i < 29; i++){
        char* buf = malloc(8);
        sprintf(buf, "%d\0", i);
        dapush(da, buf);
    }
    for(int i = 0; i < 8; i++){
        char* buf = dapop(da);
        puts(buf);
        free(buf);
    }

    DYNARR* dasecond = dactor(2);
    for(int i = 5000; i < 5005; i++){
        char* buf = malloc(8);
        sprintf(buf, "%d\0", i);
        dapush(dasecond, buf);
    }
    DYNARR* mrjd = damerge(da, dasecond);
    for(int i = 0; i < 10; i++){
        char* buf = dapop(mrjd);
        puts(buf);
        free(buf);
    }

    puts(daget(mrjd, 2));
    puts(dapeek(mrjd));
    puts(dapeek(mrjd));

    dadtorfr(mrjd);
}
