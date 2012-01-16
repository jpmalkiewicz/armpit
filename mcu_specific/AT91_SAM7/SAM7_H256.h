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
@
@ Contributions:
@
@     This file includes contributions by Robbie Dinn, marked <RDC>
@
@-------------------------------------------------------------------------------------

@-------------------------------------------------------------------------------------
@ Olimex SAM7-H256 / AT91-SAM7S256
@ ------- FAMILY -----------
AT91_SAM7	= 1			@ ATMEL AT91SAM7 family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= pioa_base		@ write 1 here to make pin a gpio
LEDIO		= pioa_base
REDLED		= GRNLED		@ PA8
YELLED		= GRNLED		@ PA8
GRNLED		= (1 << 8)		@ PA8
@ ---flash init (boot) override---
flash_int_gpio	= pioa_base		@ 				       	<RDC>
flash_init_pin	= 0x08			@ PA3 is boot file override pin        	<RDC>
@ -------UART0 pin assignements ---------
uart0_gpio	= pioa_base		@ 					<RDC>
uart0_pins	= 0x60			@ 				       	<RDC>
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0
sd_spi_gpio	= pioa_base		@ SD card / SPI0 is on IO port A (PA.11,12,13,14=SS,MISO,MOSI,CLK)
sd_cs_gpio	= pioa_base		@ SD card chip-select is on IO port A
sd_cs		= 1 << 10		@ SD card chip-select is on pin PA.10 (the 10 in 1<< 10)
@ --------- FREQs -----------
PLL_parms	= 0x15153FFA		@ XTal=18.432MHz, PLL mul=1301+1, wt=63, div=250 -> 96MHz, USB=96/2MHz
UART0_DIV	= 313			@ uart0 frequency divisor for 9600 bauds at 48 MHz
UART1_DIV	= 313			@ uart1 frequency divisor for 9600 bauds at 48 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x00200000		@
RAMTOP		= 0x00210000		@ AT91-SAM7S256 (64kB)
@ --------- BUFFERS --------
RBF_size	= 0x0800		@ READBUFFER size for tag as bytevector (2KB including 4-byte tag)
WBF_size	= 0x0800		@ READBUFFER size for tag as bytevector (2KB including 4-byte tag)
@ --------- FLASH ----------
F_START_PAGE	= 0x00010000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x0003F000		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 256			@ size of pages used for files
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x00012000		@ 72KB into flash (page after 2 x 4 KB file pages, after code)
LIB_TOP_PAGE	= 0x00040000		@ end of flash
@-------------------------------------------------------------------------------------
