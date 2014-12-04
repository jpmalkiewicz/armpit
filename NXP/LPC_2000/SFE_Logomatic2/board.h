/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2014 Hubert Montas

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
@-----------------------------------------------------------------------------*/

@-------10--------20--------30--------40--------50--------60--------70--------80

/* =======  BOARD =========== */
SFE_Logomatic2	= 1		@ SparkFun Electronics Logomatic V2.0 / LPC 2148

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD-card file system
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC i/f (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= PINSEL0	@ board LED control is on PINSEL0
LEDIO		= io0_base	@ board LED on/off pins are on IO0PINs
rled_pin	= 11		@ P0.11 -- STAT1
yled_pin	= rled_pin	@ aliased
gled_pin	= 2		@ P0.2  -- STAT0
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0
sd_spi_gpio	= io0_base	@ SD card/SPI0 gpio0 P0.4,5,6=SCK0,MISO0,MOSI0
sd_cs_gpio	= io0_base	@ SD card chip-select is on gpio0
sd_cs_pin	= 7		@ SD card chip-select pin P0.7
@ --------- FREQs -----------
CLOCK_FREQ	= 0xEA60	@ LPC2148 -- 60000 kHz = clock frequency
PLL_PM_parms	= 0x24		@ LPC2148 (12MHz), PLL      div 2, mul 5 = 60MHZ
@UART0_DIV_L	= 0x87		@ lower byte divisor for 9600 baud if pclk=60MHz
@UART0_DIV_H	= 0x01		@ upper byte divisor for 9600 baud if pclk=60MHz
UART0_DIV_L	= 32		@ lower byte of div for 115200 baud, pclk =60MHz
UART0_DIV_H	= 0		@ upper byte of div for 115200 baud, pclk =60MHz
PLL1_PM_parms	= 0x23		@ LPC2148 (12MHz), PLL1 USB div 2, mul 4 = 48MHZ
@ --------- RAM ------------
RAMBOTTOM	= 0x40000000	@ Main non-USB RAM
RAMTOP		= 0x40008000	@ LPC 2148 (32 kB)
@ --------- BUFFERS --------
.ifndef native_usb
  RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
.else
  BUFFER_START	= 0x7FD00000+4	@ USB DMA RAM (8kb) 0x7FD00000 to 0x7FD02000
  RBF_size	= 0x1000	@ READBUFFER size for tag as bytevector (4KB)
  WBF_size	= 0x0C00	@ WRITEBUFFER size for tag as bytevector (3KB)
.endif
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap14x32KB	= 1		@ LPC2148 flash, 8x4KB + 14x32KB + 5x4KB sectors
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00070000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00020000	@ 128KB into flash, after 2x32KB file pages+code
LIB_TOP_PAGE	= 0x0007D000	@ start of boot block

@-------10--------20--------30--------40--------50--------60--------70--------80





