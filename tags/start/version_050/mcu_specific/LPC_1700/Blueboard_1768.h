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
@ ngxtechnologies Blueboard LPC1768-H / NXP LPC1768
@ ------- FAMILY -----------
LPC_17xx	= 1			@ NXP LPC17xx family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= PINSEL3		@ board LED control is on PINSEL3
LEDIO		= io1_base		@ board LED on/off pins are on IO1PINs
REDLED		= GRNLED		@ aliased
YELLED		= GRNLED		@ aliased
GRNLED		= 0x20000000		@ P1.29
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0 (legacy SPI)
sd_spi_gpio	= io0_base		@ SD card / SPI0 is on gpio0 (P0.15,17,18=SCK0,MISO0,MOSI0)
sd_cs_gpio	= io0_base		@ SD card chip-select is on gpio0
sd_cs		= 1 << 16		@ SD card chip-select is on pin P0.16 (the 16 in 1 << 16)
@ --------- FREQs -----------
CLOCK_FREQ	= 0x017700		@ 96000 kHz = clock frequency (96 MHz)
PLL_PM_parms	= 0x0B			@ 12 MHz Xtal -- PLL dividor 1, multiplier 12 -> 288 MHZ
UART0_DIV_L	= 0x38			@ lower byte of divisor for 9600 baud if pclk = 48 MHz
UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 48 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x10000000		@ Main on-chip RAM
RAMTOP		= 0x10008000		@ LPC 1768 (32 kB)
@ --------- BUFFERS --------
BUFFER_START	= 0x2007C000		@ AHB RAM Bank 0, 16KB, 0x2007C000 to 0x2007FFFF
RBF_size	= 0x1000		@ READBUFFER size for tag as bytevector (4KB including 4-byte tag)
WBF_size	= 0x0C00		@ WRITEBUFFER size for tag as bytevector (3KB incl. 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
@ Note: some LPC1768 hardware revisions have the flash sector at 0x70000 not write-able
@       even though IAP does not return an error (0x78000 remains write-able though).
@       This is undocumented but discussed in electronix.ru forums (Dec. 6, 2010 to Mar. 3, 2011)
@       and apparently confirmed by NXP (chips from week 11 to 34 of 2010, possibly marked: rescreen).
@       Select F_END_PAGE and LIB_TOP_PAGE accordingly below.
F_START_PAGE	= 0x00010000		@ address of 1st page of FLASH (for files)
@F_END_PAGE	= 0x00078000		@ address of page after last page of FLASH used for files
F_END_PAGE	= 0x00068000		@ 0x70000 is not accessible on this NXP hardware revision
F_PAGE_SIZE	= 256			@ size of pages used for files
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x00020000		@ 128KB into flash (page after 2 x 32 KB file pages, after code)
@LIB_TOP_PAGE	= 0x00080000		@ after last flash page
LIB_TOP_PAGE	= 0x00070000		@ after last flash page, 0x70000 not accessible on this NXP hardware
@-------------------------------------------------------------------------------------
