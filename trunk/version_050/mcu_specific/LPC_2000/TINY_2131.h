@---------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

@ Permission is hereby granted, free of charge, to any person obtaining
@ a copy of this software and associated documentation files (the "Software"),
@ to deal in the Software without restriction, including without limitation
@ the rights to use, copy, modify, merge, publish, distribute, sublicense,
@ and/or sell copies of the Software, and to permit persons to whom the
@ Software is furnished to do so, subject to the following conditions:
@
@ The above copyright notice and this permission notice shall be included
@ in all copies or substantial portions of the Software.
@
@ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
@ OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
@ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
@ THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
@ OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
@ ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
@ OTHER DEALINGS IN THE SOFTWARE.
@
@---------------------------------------------------------------------------------------------------

@-------------------------------------------------------------------------------------
@ NewMicros Tiny 2131 / LPC 2131
@ ------- FAMILY -----------
LPC_2000	= 1			@ NXP LPC2000 family MCU
@ --------- LEDs -----------
LEDPINSEL	= PINSEL2		@ board LED control is on PINSEL2
LEDIO		= io1_base		@ board LED on/off pins are on IO1PINs
REDLED		= 0x00200000		@ P1.21
YELLED		= 0x00400000		@ P1.22
GRNLED		= 0x00800000		@ P1.23
@ --------- FREQs -----------
CLOCK_FREQ	= 0xEA60		@ LPC2131 -- 60000 kHz = clock frequency
PLL_PM_parms	= 0x25			@ LPC2131 (10MHz) -- PLL dividor 2, multiplier 6 -> 60MHZ
UART0_DIV_L	= 0x87			@ lower byte of divisor for 9600 baud if pclk = 60 mhz
UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 60 mhz
@ --------- RAM ------------
RAMBOTTOM	= 0x40000000
RAMTOP		= 0x40002000		@ LPC 2131 (8 kB)
@ --------- BUFFERS --------
RBF_size	= 0x0400		@ READBUFFER size for tag as bytevector (1KB including 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x00006000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00007000		@ address of page after last page of FLASH used for files (4kB)
F_PAGE_SIZE	= 256			@ size of pages used for files
@-------------------------------------------------------------------------------------
