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
@ Olimex STR-H711 / STR711
@ ------- FAMILY -----------
STR_7xx 	= 1			@ ST STR7xx family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= ioport1_base		@ LED IO function control is on IOPORT1
LEDIO		= ioport1_base		@ LED on/off control is on IOPORT1
REDLED		= GRNLED		@ P1.8 aliased to grnled (the only board led except for power)
YELLED		= GRNLED		@ P1.8 aliased to grnled (the only board led except for power)
GRNLED		= 0x0100		@ P1.8 (i.e. bit 8 of IOPORT1)
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi1_base		@ SD card is on BSPI1
sd_spi_gpio	= ioport0_base		@ SD card / BSPI1 is on IO port 0 (P0.4,5,6=S1.MISO,S1.MOSI,S1.SCLK)
sd_cs_gpio	= ioport1_base		@ SD card chip-select is on IO port 1
sd_cs		= 1 << 9		@ SD card chip-select is on pin P1.9 (the 9 in 1<< 9)
@ --------- FREQs -----------
PLL_parms	= 0x20			@ PLL1 mult=24, divi=1 -> 48MHZ with 4MHz crystal and div 2 on x-tal
UART0_DIV	= 0x138			@ divisor for 9600 baud at PCLK1 = 48 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
.ifndef	debug_usb
RAMTOP		= 0x20010000		@  (64kB)
.else
RAMTOP		= 0x20008000		@ top of STM32F107VC 64 KB SRAM
.endif
@ --------- BUFFERS --------
RBF_size	= 0x0800		@ READBUFFER size for tag as bytevector (2KB including 4-byte tag)
WBF_size	= 0x0800		@ WRITEBUFFER size for tag as bytevector (2KB incl. 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x00010000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00030000		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 256			@ size of pages used for files
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x00030000		@ 192KB into flash (page after 2 x 64 KB file pages, after code)
LIB_TOP_PAGE	= 0x00040000		@ end of flash
@-------------------------------------------------------------------------------------
