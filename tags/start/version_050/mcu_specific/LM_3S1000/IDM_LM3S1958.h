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
@ Luminary Micro 3.5in LCD Intelligent Display Module (IDM-L35), LM3S1958, Cortex-M3
@ ------- FAMILY -----------
LM_3S1000	= 1			@ LMI LM3S1000 family MCU
@ --------- LEDs -----------
LEDPINSEL	= ioportg_base		@ LED IO function control is on I/O Port G
LEDIO		= ioportg_base		@ LED on/off control is on I/O Port G
REDLED		= GRNLED		@ aliased to grnled
YELLED		= GRNLED		@ aliased to grnled
GRNLED		= 0x04			@ PG2 (the only board LED except for status-type)
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= ssi1_base		@ SD card is on SSI1
sd_spi_gpio	= ioporte_base		@ SD card / SSI0 is on I/O port E pins (PE.0,2,3=CLK,MISO,MOSI)
sd_cs_gpio	= ioporte_base		@ SD card chip-select is on I/O port E pins
sd_cs		= 1 << (1 + 2)		@ SD card chip-select is on PE1 pin (the 1 in (1 + 2))
@ --------- FREQs -----------
UART0_IDIV	= 325			@ integer divisor for 9600 baud at 50 MHz
UART0_FDIV	= 33			@ fractional divisor for 9600 baud at 50 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
RAMTOP		= 0x20010000		@  (64kB)
@ --------- BUFFERS --------
RBF_size	= 0x0800		@ READBUFFER size for tag as bytevector (2KB including 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x00010000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x0003fC00		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 256			@ size of pages used for files
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x00010800		@ 66KB into flash (page after 2 x 1 KB file pages, after code)
LIB_TOP_PAGE	= 0x00040000		@ end of flash
@-------------------------------------------------------------------------------------