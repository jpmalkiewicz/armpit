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
@ Olimex STM32-H107 / STM32F107VCT6 Cortex-M3
@ ------- FAMILY -----------
STM32x		= 1			@ ST STM32x family MCU
connectivity_ln = 1			@ STM32F107 is part of Connectivity Line (USB OTG, Ethernet)
@ ------ Native USB --------
@native_usb	= 1			@ comment this line out to assemble without usb support
@debug_usb 	= 1			@ Note: USB is not currently functional on this board
@ --------- LEDs -----------
LEDPINSEL	= ioportc_base		@ LED IO function control is on I/O Port C
LEDIO		= ioportc_base		@ LED on/off control is on I/O Port C
REDLED		= YELLED		@ aliased to yelled
YELLED		= 1 << 7		@ PC.7 -- STAT2 yellow LED on board
GRNLED		= 1 << 6		@ PC.6 -- STAT1 green  LED on board
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi1_base		@ SD card is on SPI1
sd_spi_gpio	= ioporta_base		@ SD card / SPI1 is on IO port A (PA.4,5,6,7=SS,SCK,MISO,MOSI)
sd_cs_gpio	= ioporta_base		@ SD card chip-select is on IO port A
sd_cs		= 1 << 8		@ SD card chip-select is on pin PA.8 (the 8 in 1<< 8)
@ --------- FREQs -----------
Clock_parms	= 0x001d2402		@ USB->48MHz, PLL x9->72MHz->AHB, 36MHz->ADC->APB1->APB2
Clock_parms2	= 0x00018644		@ PLL2 x1/5->PLL src, PLL3 x10/5->50MHz, PLL2 x8/5->40MHz
UART0_DIV	= 0x0ea6		@ divisor for 9600 baud at APB2 Clock = 36 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
.ifndef	debug_usb
RAMTOP		= 0x20010000		@ top of STM32F107VC 64 KB SRAM
.else
RAMTOP		= 0x20008000		@ 32 KB SRAM (32 KB used to store USB interrupt info)
.endif
@ --------- BUFFERS --------
RBF_size	= 0x0800		@ READBUFFER  size for tag as bytevector (2KB including 4-byte tag)
WBF_size	= 0x0800		@ WRITEBUFFER size for tag as bytevector (2KB including 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x08010000		@ address of 1st page of FLASH for files (page/sector 32)
F_END_PAGE	= 0x0803f800		@ address of page after last page of FLASH used for files (pg/sct 127)
F_PAGE_SIZE	= 256			@ size of pages used for files (armpit scheme page size)
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x08011000		@ 68KB into flash (page after 2 x 2 KB file sectors, after code)
LIB_TOP_PAGE	= 0x08040000		@ end of flash (256 KB)
@-------------------------------------------------------------------------------------
