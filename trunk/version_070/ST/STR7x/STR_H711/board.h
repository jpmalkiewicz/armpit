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
STR_H711 	= 1		@ Olimex STR-H711 / STR711

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
@onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ SDHC cards on SPI/MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioport1_base	@ LED IO function control is on IOPORT1
LEDIO		= ioport1_base	@ LED on/off control is on IOPORT1
rled_pin	= 8		@ P1.8 (i.e. bit 8 of IOPORT1)
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioport0_base	@ override button is on IOPORT0
BOOTOVERRID_BUT	= 3		@ override button is on P0.3
@ --------- UART ------------
UART_PINSEL	= ioport0_base	@ UART pin config register = IOPORT0
uart_rx		= 8		@ uart Rx pin = P0.8
uart_tx		= 9		@ uart Tx pin = P0.9
@UART0_DIV	= 312		@ divisor for   9600 baud at PCLK1 = 48 MHz
UART0_DIV	= 26		@ divisor for 115200 baud at PCLK1 = 48 MHz
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi1_base	@ SD card is on BSPI1
sd_spi_gpio	= ioport0_base	@ SD card IO port 0 P04,5,6=S1MISO,S1MOSI,S1SCLK
sd_cs_gpio	= ioport1_base	@ SD card chip-select is on IO port 1
sd_cs_pin	= 9		@ SD card chip-select is pin P1.9
@ --------- FREQs -----------
PLL_parms	= 0x20		@ PLL1 mult=24, divi=1->48MHZ, 4MHz XTal, div 2
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
RAMTOP		= 0x20010000	@  (64KB)
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmapA_4x8KB	= 1		@ STR711: 4x8KB, 1x32KB, 3x64KB, Bank 0 sectors
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00030000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00030000	@ 192KB into flash, after 2x64KB file pages+code
LIB_TOP_PAGE	= 0x00040000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80




