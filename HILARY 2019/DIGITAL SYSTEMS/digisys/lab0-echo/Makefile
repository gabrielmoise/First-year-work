# lab0-echo/Makefile
# Copyright (c) 2018, J. M. Spivey

all: echo.hex

CPU = -mcpu=cortex-m0 -mthumb
CFLAGS = -O -g -Wall
CC = C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-gcc

%.elf: %.o startup.o
	$(CC) $(CPU) $(CFLAGS) -TNRF51822.ld -nostdlib \
	    $^ -lgcc -o $@ -Wl,-Map,$*.map 
	C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-size $@

%.o: %.c
	$(CC) $(CPU) $(CFLAGS) -c $< -o $@ 

%.hex: %.elf
	C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-objcopy -O ihex $< $@

clean:
	rm -f *.hex *.elf *.map *.o 

# Don't delete intermediate files
.SECONDARY:


