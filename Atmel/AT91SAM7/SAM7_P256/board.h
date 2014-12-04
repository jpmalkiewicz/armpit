/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2011-2014 Robbie Dinn

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
SAM7_P256	= 1		@ Olimex SAM7-P256 / AT91-SAM7S256

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= pioa_base	@ write 1 here to make pin a gpio
LEDIO		= pioa_base
led_per_id	= 2		@ PIO A peripheral ID = 2
rled_pin	= 18		@ PA18
yled_pin	= 17		@ PA17
gled_pin	= 15		@ PA15
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= pioa_base	@ Boot-override is on I/O Port A
but_per_id	= 2		@ PIO A peripheral ID = 2
BOOTOVERRID_BUT	= 19		@ Boot-override button is PA19 (Button 1)
@ ---------- UART -----------
uart_gpio	= pioa_base	@ uart0 pins on PA			<RDC>
uart_per_id	= 6		@ uart0 peripheral ID
uart_rx		= 5		@ uart0 Rx pin = PA5
uart_tx		= 6		@ uart0 Tx pin = PA6
@uart_div	= 313		@ uart0 freq divisor for   9600 bauds at 48MHz
uart_div	= 26		@ uart0 freq divisor for 115200 bauds at 48MHz
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0
sd_spi_gpio	= pioa_base	@ SD card port A PA11,12,13,14=SS,MISO,MOSI,CLK
sd_spi_per_id	= 5		@ SPI0 is peripheral 5
sd_nss		= 11		@ SD SPI NSS  pin = PA.11
sd_miso		= 12		@ SD SPI MISO pin = PA.12
sd_mosi		= 13		@ SD SPI MOSI pin = PA.13
sd_clk		= 14		@ SD SPI CLK  pin = PA.14
sd_cs_gpio	= pioa_base	@ SD card chip-select is on IO port A
sd_cs_pin	= 10		@ SD card chip-select is PA.10
sd_spi_slo	= 120		@ SPI low  speed SCBR:  48 MHz/120 = 400 KHz
sd_spi_fst	= 3		@ SPI high speed SCBR:  48 MHz/3  =   16 MHz
@ --------- FREQs -----------
PLL_parms	= 0x15153FFA	@ XTal=18.4MHz,PLLmul=1302,wt=63,div=250->96MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x00200000	@
RAMTOP		= 0x00210000	@ AT91-SAM7S256 (64kB)
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
@ --------- FLASH ----------
flashmap_64x4KB	= 1		@ AT91SAM7: 64 x 4kB sectors
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x0003F000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00012000	@ 72KB into flash, after 2x4KB file pages+code
LIB_TOP_PAGE	= 0x00040000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80





