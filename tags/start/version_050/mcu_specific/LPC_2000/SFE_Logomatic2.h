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
@ SparkFun Electronics Logomatic V2.0 / LPC 2148
@ ------- FAMILY -----------
LPC_2000	= 1			@ NXP LPC2000 family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= PINSEL0		@ board LED control is on PINSEL0
LEDIO		= io0_base		@ board LED on/off pins are on IO0PINs
REDLED		= 0x00000800		@ P0.11 -- STAT1
YELLED		= REDLED		@ aliased to red led
GRNLED		= 0x00000004		@ P0.2  -- STAT0
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0
sd_spi_gpio	= io0_base		@ SD card / SPI0 is on gpio0 (P0.4,5,6=SCK0,MISO0,MOSI0)
sd_cs_gpio	= io0_base		@ SD card chip-select is on gpio0
sd_cs		= 1 << 7		@ SD card chip-select is on pin P0.7 (the 7 in 1<< 7)
@ --------- FREQs -----------
CLOCK_FREQ	= 0xEA60		@ LPC2148 -- 60000 kHz = clock frequency
PLL_PM_parms	= 0x24			@ LPC2148 (12MHz) -- PLL dividor 2, multiplier 5 -> 60MHZ
UART0_DIV_L	= 0x87			@ lower byte of divisor for 9600 baud if pclk = 60 mhz
UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 60 mhz
PLL1_PM_parms	= 0x23			@ LPC2148 (12MHz) -- PLL1 USB dividor 2, multiplier 4 -> 48MHZ
@ --------- RAM ------------
RAMBOTTOM	= 0x40000000		@ Main non-USB RAM
RAMTOP		= 0x40008000		@ LPC 2148 (32 kB)
@ --------- BUFFERS --------
BUFFER_START	= 0x7FD00000		@ USB DMA RAM (8kb) 0x7FD00000 to 0x7FD02000
RBF_size	= 0x1000		@ READBUFFER size for tag as bytevector (4KB including 4-byte tag)
WBF_size	= 0x0C00		@ WRITEBUFFER size for tag as bytevector (3KB incl. 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x00010000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00070000		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 256			@ size of pages used for files
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x00020000		@ 128KB into flash (page after 2 x 32 KB file pages, after code)
LIB_TOP_PAGE	= 0x0007D000		@ start of boot block
@-------------------------------------------------------------------------------------
