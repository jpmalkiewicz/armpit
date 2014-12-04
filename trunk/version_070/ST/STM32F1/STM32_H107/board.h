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
STM32_H107	= 1		@ Olimex STM32-H107 / STM32F107VCT6 Cortex-M3
connectivity_ln = 1		@ STM32F107 is part of Connectivity Line (USB)

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ SDHC cards only on SPI/MMC i/f (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioportc_base	@ LED IO function control is on I/O Port C
LEDIO		= ioportc_base	@ LED on/off control is on I/O Port C
led_pinmode	= 3		@ LED pin mode (gpio out,2,3=psh-pul,7=opn-drn)
rled_pin	= 7		@ PC.7 -- STAT2 yellow LED on board
yled_pin	= rled_pin	@ aliased
gled_pin	= 6		@ PC.6 -- STAT1 green  LED on board
LEDBUT_APB2P	= (1<<2)|(1<<3)|(1<<4)|(1<<0) @ power Ports A,B,C,AFIO (A=2,B=3)
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioporta_base	@ Boot-override is on I/O Port A
BOOTOVERRID_BUT	= 0		@ Boot-override button is PA0
BOOTOVERRID_INV = 1		@ invert boot-override (normally low)
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi1_base	@ SD card is on SPI1
sd_spi_gpio	= ioporta_base	@ SD card SPI1 port A PA4,5,6,7=SS,SCK,MISO,MOSI
sd_cs_gpio	= ioporta_base	@ SD card chip-select is on IO port A
sd_cs_pin	= 8		@ SD card chip-select is pin PA.8
@ --------- FREQs -----------
Clock_parms	= 0x001d2402	@ USB=48MHz,PLLx9=72MHz AHB, 36MHz=ADC=APB1=APB2
Clock_parms2	= 0x00018644	@ PLL2x1/5=PLLsrc,PLL3x10/5=50MHz,PLL2x8/5=40MHz
@UART0_DIV	= 3750		@ divisor for   9600 baud at APB2 Clock = 36 MHz
UART0_DIV	= 312		@ divisor for 115200 baud at APB2 Clock = 36 MHz
SYSTICK_RELOAD	= 72*10000 - 1	@ systick reload for 10ms interrupts at 72 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
RAMTOP		= 0x20010000	@ top of STM32F107VC 64 KB SRAM
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap128x2KB	= 1		@ STM32F107VCT6: 128 x 2KB FLASH sectors
F_START_PAGE	= 0x08010000	@ address of 1st page of FLASH for files
F_END_PAGE	= 0x0803f800	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x08011000	@ 68KB into flash, after 2x2KB file sectors+code
LIB_TOP_PAGE	= 0x08040000	@ end of flash (256 KB)

@-------10--------20--------30--------40--------50--------60--------70--------80





