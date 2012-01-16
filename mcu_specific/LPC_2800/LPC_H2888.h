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
@ Olimex H2888 / LPC 2888
@ ------- FAMILY -----------
LPC_2800	= 1			@ NXP LPC2880 family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDIO		= io2_base		@ PINS_2
REDLED		= 0x02
YELLED		= REDLED
GRNLED		= REDLED
@ --------- FREQs -----------
.ifdef native_usb
  UART0_DIV_L	= 0x39			@ lower byte of divisor for 9600 baud if pclk = 48 mhz (for usb)
  UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 48 mhz (for usb)
.else
  UART0_DIV_L	= 0x87			@ lower byte of divisor for 9600 baud if pclk = 60 mhz
  UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 60 mhz
.endif
@ --------- RAM ------------
RAMBOTTOM	= 0x00000000		@ off-chip SDRAM, remapped by MMU
RAMTOP		= 0x02000000		@ 32MB off-chip SDRAM
@ --------- BUFFERS --------
BUFFER_START	= RAMBOTTOM + 0x020000	@ 128kb into SDRAM (allows for scheme machine code)
RBF_size	= 0x020000		@ READBUFFER size for tag as bytevector (128KB including 4-byte tag)
WBF_size	= 0x020000		@ WRITEBUFFER size for tag as bytevector (128KB incl. 4-byte tag)
heapbottom	= RAMBOTTOM + 0x100000	@ 1MB into SDRAM
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x20010000		@ address of 1st page of external FLASH (for files)
F_END_PAGE	= 0x201F0000		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 512			@ size of pages used for files
@-------------------------------------------------------------------------------------
