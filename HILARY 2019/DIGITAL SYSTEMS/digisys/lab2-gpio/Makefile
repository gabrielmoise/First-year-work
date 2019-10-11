# lab2-asm/Makefile
# Copyright (c) 2018, J. M. Spivey

all: heart-intr.hex

CPU = -mcpu=cortex-m0 -mthumb
CFLAGS = -O -g -Wall
CC = "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-gcc"
AS = "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-as"
LD = "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-ld"

# The easy thing to do is invoke gcc for linking.  But it's nice to invoke
# ld directly at first, to show who is really doing the work.  For this,
# we need to find the location of the library libgcc.a in a semi-portable
# way.

# First find the initial element of the gcc library search path.
GCCLIB := "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\lib\gcc\arm-none-eabi\7.3.1"

# Then try a couple of likely-sounding subdirectories for the library.
# Trickily we use the 'wildcard' function here to test for the existence of
# a file, and take the first result with the 'word' function.
LIBGCC := "C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\arm-none-eabi\lib\thumb\v6-m\libgcc.a"

%.elf: %.o heart-intr.o lib.o startup.o
	$(LD) $^ $(LIBGCC) -o $@ -Map $*.map -T NRF51822.ld
	C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-size $@

# In an emergency, just use $(GCCLIB)/thumb/v6-m/libgcc.a

%.o: %.c
	$(CC) $(CPU) $(CFLAGS) -c $< -o $@ 

%.o: %.s
	$(AS) $(CPU) $< -o $@

%.hex: %.elf
	C:\Program Files (x86)\GNU Tools ARM Embedded\7 2018-q2-update\bin\arm-none-eabi-objcopy -O ihex $< $@

clean:
	rm -f *.hex *.elf *.map *.o 

# Don't delete intermediate files
.SECONDARY:

# Alternative rules to build a binary for use with qemu-arm

QEMU-CPU = -march=armv6

q-%.elf: %-q.o test-q.o
	$(QEMU-GCC) $(QEMU-CPU) $^ -static -o $@

%-q.o: %.s
	$(QEMU-AS) $(QEMU-CPU) -mthumb $< -o $@

test-q.o: test.c
	$(QEMU-GCC) $(QEMU-CPU) -c $< -o $@

QEMU-GCC = arm-linux-gnueabihf-gcc -marm
QEMU-AS = arm-linux-gnueabihf-as
