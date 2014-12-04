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
SAMG53_XPLD	= 1		@ Atmel SAMG53-Xplained / ATSAMG53N19
_text_link_address_ = 0x00400000
excep_vector_adr = 0x00400000
CODE_OFFSET	= 0x00400000
@nrml_lambda_lkp	= 1

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
enable_MPU	= 1		@ use the MPU
@onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC (no normal SD)

/* ===== CONFIGURATION ====== */
@ ----- LEDs / BUTTON -------
LEDPINSEL	= pioa_base	@ where to write 1 to make pin a gpio
LEDIO		= pioa_base
led_per_id	= 11		@ PIO A peripheral ID = 11
rled_pin	= 16		@ PA16 (actually yellow)
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= pioa_base	@ Boot-override (BP2) is on I/O Port A
but_per_id	= 11		@ PIO A peripheral ID = 11
BOOTOVERRID_BUT	= 2		@ Boot-override button is on pin PA2 (SW0)
@ ---------- UART -----------
uart_not_usart	= 1		@ use uart (not usart) as scheme uart
uart_gpio	= pioa_base	@ uart0 is on PA pins
uart_per_id	=  8		@ uart0  peripheral ID = 8
uart_rx		=  9		@ uart0  Rx pin = PA9
uart_tx		= 10		@ uart0  Tx pin = PA10
uart_div	= 26		@ uart0 frequency div for 115200 bauds at 48 MHz
@ --------- SD card ---------
sd_is_on_spi	= 1		@ if used, SD card will be on SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0
sd_spi_gpio	= pioa_base	@ SD card port A PA11,12,13,14=SS,MISO,MOSI,CLK
sd_spi_per_id	= 21		@ SPI0 is peripheral 21
sd_nss		= 11		@ SD SPI NSS  pin = PA.11
sd_miso		= 12		@ SD SPI MISO pin = PA.12
sd_mosi		= 13		@ SD SPI MOSI pin = PA.13
sd_clk		= 14		@ SD SPI CLK  pin = PA.14
sd_spi_slo	= 120		@ SPI low  speed SCBR: 48 MHz/120 = 400 KHz
sd_spi_fst	= 10		@ SPI high speed SCBR: 48 MHz/10  = 4.8 MHz
@ --------- FREQs -----------
PLL_parmsA	= (1464<<16)|0x3F01 @ PLLA: (1464+1)*32.768KHz = 48 MHz
FLSH_WTSTA 	= 3		@ flash wait states for 48 MHz: 3 =>4 cycles r/w
SYSTICK_RELOAD	= 48*10000 - 1	@ systick reload for 10ms interrupts at 48 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000	@ bottom of RAM
@RAMTOP		= 0x20008000	@ ATSAMG53-N19 (32KB)
/*  need to power-up next 2x32KB RAM to get 96KB -- nite, already powered */
RAMTOP		= 0x20018000	@ ATSAMG53-N19 (96KB)
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER size for tag as bytevector (2KB)
@WBF_size	= 0x0800	@ READBUFFER size for tag as bytevector (2KB)
@ --------- FLASH ----------
/* not sure if flash gets remapped to 0x00000000 or stays at 0x00400000 */
/* also, 1st 64KB block is 2x8KB + 1x48KB (if important/useful) */

flashmapg8x64KB	= 1		@ AT91-SAM4S16C: 8 x 64KB sectors
F_START_PAGE	= 0x00410000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00470000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 512		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00430000	@ 192KB into flash (after 2x64KB fil sects+code)
LIB_TOP_PAGE	= 0x00480000	@ end of flash
/*
flashmap_8x64KB	= 1		@ AT91-SAM4S16C: 8 x 64KB sectors
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00070000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 512		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00030000	@ 192KB into flash (after 2x64KB fil sects+code)
LIB_TOP_PAGE	= 0x00080000	@ end of flash
*/
@-------10--------20--------30--------40--------50--------60--------70--------80




