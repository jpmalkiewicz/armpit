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
SAM9_L9261	= 1		@ Olimex SAM9-L9261 / AT91-SAM9261

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD-card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= pioa_base	@ write 1 here to make pin a gpio
LEDIO		= pioa_base
led_per_id	= 2		@ PIO A peripheral ID = 2
rled_pin	= 13		@ PA13, low  = on, (green LED, LED1)
yled_pin	= 23		@ PA23, high = on  (LED3)
gled_pin	= 14		@ PA14, low  = on  (LED2)
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= pioa_base	@ Boot-override (BP3) is on I/O Port A    <RDC>
but_per_id	= 2		@ PIO A peripheral ID = 2
BOOTOVERRID_BUT	= 27		@ Boot-override button is on PA27 (BP3)   <RDC>
@ ---------- UART -----------
@ (Note: BootROM enables DBGU uart at 115200, 8N1)
uart_gpio	= pioa_base	@ uart0/DBGU pins are on Port A		<RDC>
uart_rx		= 9		@ uart0/DBGU Rx pin = PA9
uart_tx		= 10		@ uart0/DBGU Tx pin = PA10
@uart_div	= 651		@ uart0/DBGU div for   9600 bauds at 100MHz
uart_div	= 54		@ uart0/DBGU div for 115200 bauds at 100MHz
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card is on SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0
sd_spi_gpio	= pioa_base	@ SD card port A PA0,1,2=MISO,MOSI,CLK
sd_spi_per_id	= 12		@ SPI0 peripheral ID = 12
sd_miso		= 0		@ SD SPI MISO pin = PA.0
sd_mosi		= 1		@ SD SPI MOSI pin = PA.1
sd_clk		= 2		@ SD SPI CLK  pin = PA.2
sd_nss		= sd_clk	@ dummy sd_nss for sd_cfg
sd_cs_gpio	= pioa_base	@ SD card chip-select is on IO port A
sd_cs_pin	= 6		@ SD card chip-select is PA.6
sd_spi_slo	= 250		@ SPI low  speed SCBR: 100 MHz/250 = 400 KHz
sd_spi_fst	= 6		@ SPI high speed SCBR: 100 MHz/6  =   16 MHz
@ --------- FREQs -----------
PLLA_parms	= 0x204bbf07	@ PLLA XTal=18.4MHz,PLLmul=76,wt=63,div=7,200MHz
				@      Periphs at CPU/2=100MHz (PMC_MCKR reg)
				@ PLLB preconf by BootROM, 96.1MHz -> USB=48MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000	@ Bottom of off-chip SDRAM
RAMTOP		= 0x23F00000	@ 63MB (top 1MB of 64MB used to shadow FLASH)
@ --------- BUFFERS --------
BUFFER_START	= 0x020000+4	@ 128KB into remapped on-chip RAM (160KB tot sz)
RBF_size	= 0x2000	@ READBUFFER size for tag as bytevector (8KB)
WBF_size	= 0x2000	@ READBUFFER size for tag as bytevector (8KB)
@ --------- FLASH ----------
flash_is_extern	= 1		@ FLASH is not on MCU but external
flashmapEx_16MB	= 1		@ K9F4G08UOA: 128x128KB RAM shdw top 16MB FLASH
F_START_PAGE	= RAMTOP	@ 1st page of FLASH (for files)=shadow RAM start
F_END_PAGE	= 0x23FE0000	@ page (128 KB block) after last file page
F_PAGE_SIZE	= 2048		@ size of pages used for files

@-------10--------20--------30--------40--------50--------60--------70--------80





