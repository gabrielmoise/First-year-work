#include "hardware.h"
#include "lib.h"
#include <stdarg.h>

#define INTERRUPT

#define NBUF 64

/* serial_init -- set up UART connection to host */
void serial_init(void)
{
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

    RNG_VALRDY = 1;
    UART_INTENSET = BIT(UART_INT_TXDRDY);
    set_priority(UART_IRQ, 3);
    enable_irq(UART_IRQ);
}

unsigned randint (void)
{

}

void serial_putc(char ch) {

}

void serial_printf(char *fmt, ...) {
    va_list va;
    va_start(va, fmt);
    do_print(serial_putc, fmt, va);
    va_end(va);
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

void init(void) {
    int count = 0;

    serial_init();
    start_timer();



    stop_timer();
}
