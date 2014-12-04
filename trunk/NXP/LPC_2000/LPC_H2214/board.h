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
LPC_H2214	= 1		@ Olimex H2214 / LPC 2214

/* ======= OPTIONS ========== */
onboard_SDFT	= 1		@ include SD-card file system
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= PINSEL1	@ LPC 2214 LED
LEDIO		= io0_base	@ LEDs are on IO0PINs
rled_pin	= 30		@ P0.30
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0
sd_spi_gpio	= io0_base	@ SD card/SPI0, gpio0, P0.4,5,6=SCK0,MISO0,MOSI0
spi_old_silicon	= 1		@ SPI0 needs P0.7 conf as SSEL0 + tied to 3.3V
sd_cs_gpio	= io0_base	@ SD card chip-select is on gpio0
sd_cs_pin	= 20		@ SD card chip-select P0.20
@ --------- FREQs -----------
CLOCK_FREQ	= 0xE666	@ LPC2214 -- 58982 kHz = clock frequency
PLL_PM_parms	= 0x23		@ LPC2214 (14.7456MHz) PLL div 2, mul 4 = 59MHz
@UART0_DIV_L	= 0x80		@ lower byte of div for   9600 baud, pclk =59MHz
@UART0_DIV_H	= 0x01		@ upper byte of div for   9600 baud, pclk =59MHz
UART0_DIV_L	= 32		@ lower byte of div for 115200 baud, pclk =59MHz
UART0_DIV_H	= 0		@ upper byte of div for 115200 baud, pclk =59MHz
@ --------- RAM ------------	  1MB off-chip RAM
RAMBOTTOM	= 0x81000000	@ LPC 2214 (1MB off-chip RAM)
RAMTOP		= 0x81100000	@ LPC 2214 (1MB off-chip RAM)
@ --------- BUFFERS --------
BUFFER_START	= 0x40000000+4	@ 16kB on-chip RAM
RBF_size	= 0x3f00	@ READBUFFER size for bytevector tag 16128 Bytes
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
ext_flash_micron =1		@ external flash is Micron
extflmap15x64KB	= 1		@ Micron MX26LV800BTC 1x16+2x8+1x32+15x64KB sect
F_START_PAGE	= 0x80010000	@ 1st 64KB page of external FLASH (for files)
F_END_PAGE	= 0x800F0000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 512		@ size of pages used for files / lib
iap_on_lib_only	= 1		@ IAP writes libs to on-chip flash, not files
flashmap_2x64KB	= 1		@ LPC2214 on-chip, 8x8KB+2x64kB+8x8kB sectors
IAP_ARGS_ADRS	= 0x40003C00	@ address for IAP args (15KB into on-chip RAM)
LIB_BOTTOM_PAGE	= 0x00010000	@ 64KB into flash (page after code)
LIB_TOP_PAGE	= 0x0003E000	@ start of boot block

@-------10--------20--------30--------40--------50--------60--------70--------80




