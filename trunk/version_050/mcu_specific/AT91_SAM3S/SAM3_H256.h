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
@ Olimex SAM3-H256 / AT91-SAM3S4B
@ ------- FAMILY -----------
AT91_SAM3S	= 1			@ ATMEL AT91SAM3S family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= pioa_base		@ write 1 here to make pin a gpio
LEDIO		= pioa_base
REDLED		= GRNLED		@ PA8
YELLED		= GRNLED		@ PA8
GRNLED		= (1 << 8)		@ PA8
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0
sd_spi_gpio	= pioa_base		@ SD card / SPI0 is on IO port A (PA.11,12,13,14=SS,MISO,MOSI,CLK)
@ --------- FREQs -----------
PLL_parmsA	= 0x201F3F03		@ XTal=12 MHz, PLLA x32/3 -> 64x2 MHz
PLL_parmsB	= 0x000F3F02		@ XTal=12 MHz, PLLB x16/2 -> 48x2 MHz for USB
UART0_DIV	= 417			@ usart0 frequency divisor for 9600 bauds at 64 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000		@
RAMTOP		= 0x2000C000		@ AT91-SAM3S4B (48kB)
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
