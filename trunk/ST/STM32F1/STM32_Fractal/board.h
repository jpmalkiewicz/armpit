/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2012-2014 Tzirechnoy

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
STM32_Fractal	= 1		@ Fractal MCU32-1.12 Board / STM32F103RBT6 Cx-M3
STM32_H103	= 1		@ similar to Olimex STM32-H103

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
always_init_usb	= 1		@ init USB even if not plugged in
manual_usb_reset = 1		@ include manual USB disconnect code
@onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ SDHC cards only on SPI/MMC i/f (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioportc_base	@ LED IO function control is on I/O Port C
LEDIO		= ioportc_base	@ LED on/off control is on I/O Port C
led_pinmode	= 7		@ LED pin mode (gpio out,2,3=psh-pul,7=opn-drn)
rled_pin	= 12		@ PC12 (the only board LED except for power)
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
LEDBUT_APB2P	= (1<<4)	@ bit to power Port C (A=2, B=3, ...)
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioporta_base	@ Boot-override is on I/O Port A
BOOTOVERRID_BUT	= 0		@ Boot-override button is PA0
@ --------- USB -------------
vusb_prt	= ioportc_base	@ USB-P power pin is on port C, pin PC4
vusb_pin	= 4		@ USB-P power pin is PC4
usb_conn_prt	= ioportc_base	@ USB soft connect is on port C, pin PC11
usb_conn_pin	= 11		@ USB soft connect pin is PC11
@ --------- SD card ---------
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi1_base	@ SD card is on SPI1
sd_spi_gpio	= ioporta_base	@ SD card IO port A PA4,5,6,7=SS,SCK,MISO,MOSI
sd_cs_gpio	= ioporta_base	@ SD card chip-select is on IO port A
sd_cs_pin	= 8		@ SD card chip-select is pin PA.8
@ --------- FREQs -----------
Clock_parms	= 0x001d2402	@ USB=48MHz,HSEPLLmult9=72MHz,AHB=36MHz=ADC=APBn
@UART0_DIV	= 3750		@ divisor for   9600 baud at APB2 Clock = 36 MHz
UART0_DIV	= 312		@ divisor for 115200 baud at APB2 Clock = 36 MHz
SYSTICK_RELOAD	= 72*10000 - 1	@ systick reload for 10ms interrupts at 72 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
RAMTOP		= 0x20005000	@ top of STM32 20 KB SRAM
@ --------- BUFFERS --------
RBF_size	= 0x0600	@ READBUFFER  size for tag as bytevector (1.5KB)
WBF_size	= 0x0600	@ WRITEBUFFER size for tag as bytevector (1.5KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap128x1KB	= 1		@ STM32F103RBT6: 128 x 1KB FLASH sectors
F_START_PAGE	= 0x08010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x0801fC00	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x08010800	@ 66KB into flash (after 2x1KB file pages+code)
LIB_TOP_PAGE	= 0x08020000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80





