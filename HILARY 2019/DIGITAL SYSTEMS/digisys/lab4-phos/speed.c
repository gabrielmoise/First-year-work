#include "phos.h"

void put_string(char *s) {
  for (char *p = s; *p != '\0'; p++)
        serial_putc(*p);
}


void produce_task(int n){
  for (int i = 1; i<=10; i++) put_string("*");
}

void timer

void init(void) {
    serial_init();
    start(USER+0, "Characters", produce_task, 0, STACK);
}
