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
@ Olimex LPC-P1343 / NXP LPC1343
@ ------- FAMILY -----------
LPC_13xx	= 1			@ NXP LPC13xx family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= iocon_pio		@ board LED control is on IOCON_PIOn
LEDIO		= io3_base		@ board LED on/off pins are on IO1PINs
REDLED		= 0x01			@ P3.0
YELLED		= 0x02			@ P3.1
GRNLED		= 0x04			@ P3.2
@ --------- FREQs -----------
CLOCK_FREQ	= 0x011940		@ 72000 kHz = clock frequency (72 MHz)
PLL_PM_parms	= 0x25			@ 12 MHz Xtal -- PLL divs: out=4, feedback=6  -> 288 MHZ, 72MHz
UART0_DIV_L	= 0xd5			@ lower byte of divisor for 9600 baud if pclk = 72 MHz
UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 72 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x10000000		@ Main on-chip RAM
RAMTOP		= 0x10002000		@ LPC 1343 (8 kB)
@ --------- BUFFERS --------
RBF_size	= 0x0400		@ READBUFFER  size for tag as bytevector (1KB including 4-byte tag)
WBF_size	= 0x0400		@ WRITEBUFFER size for tag as bytevector (1KB incl. 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x00006000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00007000		@ address of page after last page of FLASH used for files (4kB)
F_PAGE_SIZE	= 256			@ size of pages used for files
@-------------------------------------------------------------------------------------
