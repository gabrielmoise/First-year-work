#include "phos.h"
#include "lib.h"
#include <string.h>

#define MAY 10
#define FARAGE 11

static const char *slogan[] = {
    "no deal is better than a bad deal\n",
    "BREXIT MEANS BREXIT!\n"
};

void put_string(char *s) {
  for (char *p = s; *p != '\0'; p++)
        serial_putc(*p);
}

void speaker (int n){
  message m;

  while (1)
  {
    // Farage waits for May to finish the message
    if (n == 1) receive (ANY, &m);
    put_string(slogan[n]);
    int n1 = 1-n;
    // The message is over
    send(USER+n1, &m);
    // May wait for Farage to finish the message
    if (n == 0) receive (ANY, &m);

  }
}

void init(void) {
    serial_init();
    start(USER+0, "May", speaker, 0, STACK);
    start(USER+1, "Farage", speaker, 1, STACK);
}
