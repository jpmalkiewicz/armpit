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
EK_TM4C1294	= 1		@ TI EK TM4C1294XL Launchpad C-M4F TM4C1294NCPDT

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
enable_MPU	= 1		@ use the MPU
native_usb	= 1		@ include usb support
@onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ SDHC cards only on SPI/MCI i/f (no normal SD)

/* ===== CONFIGURATION ====== */
@ ----- LEDs / BUTTON -------
GPIO_on_AHB	= 1		@ GPIO register addresses are on AHB
LEDPINSEL	= ioportn_base	@ LED IO function control is on I/O Port N
LEDIO		= ioportn_base	@ LED on/off control is on I/O Port N
rled_pin	= 1		@ PN1 line to D1 (a green LED really)
yled_pin	= rled_pin	@ aliased
gled_pin	= 0		@ PN0 line to D2

BOOTOVERRID_PRT	= ioportj_base	@ Boot-override (SW1) is on I/O Port J
BOOTOVERRID_BUT	= 0		@ Boot-override is PJ0
ENABLE_PORTS	= (1<<8)|(1<<12) @ Power up ports J & N (A=0, B=1, ..) LEDs/BUTN
@ --------- SD card ---------
sd_is_on_spi	= 1
sd_spi		= ssi0_base	@ SD card is on SSI0
sd_spi_gpio	= ioporta_base	@ SD card SSI0 I/O port A PA2,4,5=CLK,MISO,MOSI
sd_cs_gpio	= ioporta_base	@ SD card chip-select is on I/O port A pins
sd_cs_pin	= 3		@ SD card chip-select is PA3 pin
@ --------- FREQs -----------
UART0_IDIV	= 65		@ integer    divisor for 115200 baud at 120 MHz
UART0_FDIV	=  7		@ fractional divisor for 115200 baud at 120 MHz
SYSTICK_RELOAD	= 120*10000 - 1	@ systick reload for 10ms interrupts at 120 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000	@ start of on-chip RAM
RAMTOP		= 0x20040000	@ (256 kB)
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap64x16KB = 1		@ TM4C1294, 64 x 16 KB sectors in 4 banks
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x000f8000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00018000	@ 96 KB in flash (after 2x16 KB file pages+code)
LIB_TOP_PAGE	= 0x00100000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80



