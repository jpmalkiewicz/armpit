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
CS_E9302	= 1		@ Olimex CS-E9302 / EP9302

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
@enable_MPU	= 1		@ use the MPU (via MMU) [BUGGY, not recommended]
onboard_SDFT	= 1		@ include SD-card code
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDIO		= ioE_base	@ address of register control for LED pins
rled_pin	= 1		@ bit 1
yled_pin	= rled_pin	@ aliased
gled_pin	= 0		@ bit 0
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioH_base	@ Boot-override GPIO H pin 4 = BUT button
BOOTOVERRID_BUT	= 4		@ Boot-override button is on gioh pin 4
@ --------- SD card --------- 
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0 (aka SPI1)
sd_cs_gpio	= ioF_base	@ SD card chip-select is on gpio_F
sd_cs_pin	= 3		@ SD card chip-select gpio_F.3
@ --------- FREQs -----------
PLL_PM_parms	= 0x02b49907	@ PLL1 parms for 166MHz
@UART0_DIV_L	= 0x2F		@ lower byte of div for   9600 baud, pclk=166MHz
@UART0_DIV_H	= 0x00		@ upper byte of div for   9600 baud, pclk=166MHz
UART0_DIV_L	= 3		@ lower byte of div for 115200 baud, pclk=166MHz
UART0_DIV_H	= 0		@ upper byte of div for 115200 baud, pclk=166MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x00000000	@ SDRAM, SROMLL=1, 4x8MB remapped to contig 32MB
RAMTOP		= 0x02000000	@ 32MB into SDRAM
@ --------- BUFFERS --------
BUFFER_START	= RAMBOTTOM+0x020000+4	@ 128kb into SDRAM (for scheme code)
RBF_size	= 0x10000	@ READBUFFER size for tag as bytevector (64KB)
heapbottom	= RAMBOTTOM + 0x100000	@ 1MB into SDRAM
@ --------- FLASH ----------
F_START_PAGE	= 0x60020000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x60FE0000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 512		@ size of pages used for files

@-------10--------20--------30--------40--------50--------60--------70--------80





