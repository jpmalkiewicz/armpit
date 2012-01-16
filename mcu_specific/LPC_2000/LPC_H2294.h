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
@ Olimex H2294 / LPC 2294
@ ------- FAMILY -----------
LPC_2000	= 1			@ NXP LPC2000 family MCU
@ --------- LEDs -----------
LEDPINSEL	= PINSEL1		@ LPC 2294 LED
LEDIO		= io0_base		@ LEDs are on IO0PINs
REDLED		= 0x60000000		@ P0.30 (and P0.29 to support 30-bit internal representation)
YELLED		= REDLED		@ P0.30
GRNLED		= REDLED		@ P0.30
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0
sd_spi_gpio	= io0_base		@ SD card / SPI0 is on gpio0 (P0.4,5,6=SCK0,MISO0,MOSI0)
spi_old_silicon	= 1			@ SPI0 needs P0.7 configured as SSEL0 and tied to 3.3 Volts
sd_cs_gpio	= io0_base		@ SD card chip-select is on gpio0
sd_cs		= 1 << 20		@ SD card chip-select is on pin P0.20 (the 20 in 1<< 20)
@ --------- FREQs -----------
CLOCK_FREQ	= 0xE666		@ LPC2294 -- 58982 kHz = clock frequency
PLL_PM_parms	= 0x23			@ LPC2294 (14.7456MHz) -- PLL dividor 2, multiplier 4 -> 58.9824MHZ
UART0_DIV_L	= 0x80			@ lower byte of divisor for 9600 baud if pclk = 58.9824 mhz
UART0_DIV_H	= 0x01			@ upper byte of divisor for 9600 baud if pclk = 58.9824 mhz
@ --------- RAM ------------		  1MB off-chip RAM
RAMBOTTOM	= 0x81000000		@ LPC 2294 (1MB off-chip RAM)
RAMTOP		= 0x81100000		@ LPC 2294 (1MB off-chip RAM)
@ --------- BUFFERS --------
BUFFER_START	= 0x40000000		@ 16kb on-chip RAM
RBF_size	= 0x3f00		@ READBUFFER size for tag as bytevector (16128 Bytes incl. 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x80010000		@ address of 1st 64KB page of external FLASH (for files)
F_END_PAGE	= 0x803F0000		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 512			@ size of pages used for files
LIB_BOTTOM_PAGE	= 0x00010000		@ 64KB into on-chip flash (page after code)
LIB_TOP_PAGE	= 0x0003E000		@ start of boot block
@-------------------------------------------------------------------------------------
