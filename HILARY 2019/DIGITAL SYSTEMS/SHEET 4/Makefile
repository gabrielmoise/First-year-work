# lab4-phos/Makefile
# Copyright (c) 2018 J. M. Spivey

all: heart.hex race.hex echo.hex today.hex proc.hex #speed.hex

CC = "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-gcc"
AS = "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-as"
AR = "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-ar"

CPU = -mcpu=cortex-m0 -mthumb
CFLAGS = -O -g -Wall -ffreestanding
# The -ffreestanding suppresses warnings that functions like exit()
# don't have types that match the built-in definitions.

# Use one interrupt handler for most interrupts
startup.o: CFLAGS += -DHANDLER=phos_interrupt

phos.a: serial.o timer.o phos.o mpx-m0.o lib.o startup.o
	$(AR) cr $@ $^

%.elf: %.o phos.a
	$(CC) $(CPU) -TNRF51822.ld -nostdlib \
	    $^ -lc -lgcc --specs=nano.specs -o $@ -Wl,-Map,$*.map
	C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-size $@

%.o: %.c
	$(CC) $(CPU) $(CFLAGS) -c $< -o $@ 

%.o: %.s
	$(AS) $(CPU) $< -o $@

%.hex: %.elf
	C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-objcopy -O ihex $< $@

clean:
	rm -f *.hex *.elf *.map *.o phos.a

# Don't delete intermediate files
.SECONDARY:

