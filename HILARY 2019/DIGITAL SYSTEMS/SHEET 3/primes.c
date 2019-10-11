#include "hardware.h"
#include "lib.h"
#include <stdarg.h>

#define INTERRUPT

#ifdef INTERRUPT
#define NBUF 1

static volatile int txidle;       // Whether UART is idle
static volatile int bufcnt = 0;   // Number of chars in buffer
//static unsigned bufin = 0;        // Index of first free slot
static unsigned bufout = 0;       // Index of first occupied slot
static volatile char txbuf[NBUF]; // The buffer
#else
static int txinit;                // UART ready to transmit first char
#endif

/* serial_init -- set up UART connection to host */
void serial_init(void) {
    UART_ENABLE = 0;

    GPIO_DIRSET = BIT(USB_TX);
    GPIO_DIRCLR = BIT(USB_RX);
    SET_FIELD(GPIO_PINCNF[USB_TX], GPIO_PINCNF_PULL, GPIO_Pullup);
    SET_FIELD(GPIO_PINCNF[USB_RX], GPIO_PINCNF_PULL, GPIO_Pullup);

    UART_BAUDRATE = UART_BAUD_9600;     // 9600 baud
    UART_CONFIG = 0;                    // format 8N1
    UART_PSELTXD = USB_TX;              // choose pins
    UART_PSELRXD = USB_RX;
    UART_ENABLE = UART_Enabled;
    UART_STARTTX = 1;
    UART_STARTRX = 1;
    UART_RXDRDY = 0;
    UART_TXDRDY = 0;

#ifdef INTERRUPT
    // Interrupt for transmit only
    UART_INTENSET = BIT(UART_INT_TXDRDY);
    set_priority(UART_IRQ, 3);
    enable_irq(UART_IRQ);
    txidle = 1;
#else
    txinit = 1;
#endif
}

#ifdef INTERRUPT

void uart_handler(void) {
    if (UART_TXDRDY) {
        UART_TXDRDY = 0;
        if (bufcnt == 0)
            txidle = 1;
        else {
            UART_TXD = txbuf[bufout];
            bufcnt --;
            bufout = (bufout+1) % NBUF;
        }
    }
}

/* serial_putc -- send output character */
void serial_putc(char ch) {
    while (bufcnt == NBUF) pause();
    intr_disable();
    if (txidle) {
        intr_enable();
        txidle = 0;
        UART_TXD = ch;

    } else {
        txbuf[(bufout+bufcnt)%NBUF] = ch;
        bufcnt++;
    }
    intr_enable();
}

#else

/* serial_putc -- send output character */
void serial_putc(char ch) {
    if (! txinit) {
        while (! UART_TXDRDY) { /* do nothing */ }
    }
    txinit = 0;
    UART_TXDRDY = 0;
    UART_TXD = ch;
}

#endif

void serial_printf(char *fmt, ...) {
    va_list va;
    va_start(va, fmt);
    do_print(serial_putc, fmt, va);
    va_end(va);
}

int modulo(int a, int b) {
    int r = a;
    while (r >= b) r -= b;
    return r;
}

int prime(int n) {
    for (int k = 2; k * k <= n; k++) {
        if (modulo(n, k) == 0)
            return 0;
    }

    return 1;
}

#define MASK 0x0000fff0
#define LITE 0x00005fbf

void start_timer(void) {
    GPIO_DIRSET = MASK;
    GPIO_OUT = LITE;
}

void stop_timer(void) {
    GPIO_OUT = 0;
}

void rng_init(void)
{
  RNG_STOP = 1;
  RNG_INTENSET = 1;
  RNG_START = 1;
  enable_irq(RNG_IRQ);
}

static volatile unsigned randArray [1000];
static unsigned nr = 0;

void rng_handler(void)
{
  if (RNG_VALRDY == 1)
  {
    randArray[nr%1000] = RNG_VALUE;
    nr = (nr + 1)%1000;
    RNG_VALRDY = 0;
  }
}

unsigned randint(void)
{
  if(nr>=4)
    return ((randArray[nr-1] << 24) + (randArray[nr-2] << 16) + (randArray[nr-3] << 8) + randArray[nr-4]);
  else while (nr<4) pause();
  return 0;
}

static volatile unsigned die [6];

unsigned roll(void)
{
  unsigned check;
  check = randint() ;
  if (check>=4){
  check = check % 6;
  //if (check == 0) check = 6;
  return check;}
  else return roll();
}

void init(void) {
    int count = 0;
    nr = 0; // nr is declared globally
    serial_init();
    rng_init();
    start_timer();
    while (count<60000)
    {
      if (nr>4){
        unsigned value;
        value = randint();
        if (value >=4) {value = value % 6;
        die[value] ++; // the vector that counts the number of appearances for 1..6
        nr = nr - 4;
        count++;
       }
      }
      else pause();
    }
    int i;
    for (i=1;i<=5;i++) serial_printf("%d was rolled %u times\r\n",i ,die[i]);
    serial_printf("6 was rolled %u times\r\n",die[0]);
    stop_timer();
}
