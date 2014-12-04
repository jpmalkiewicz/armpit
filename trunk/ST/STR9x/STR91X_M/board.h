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
STR91X_M 	= 1		@ InSem STR91X_M / STR911

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioport0_base	@ LED IO function control is on IOPORT0
LEDIO		= ioport0_base	@ LED on/off control is on IOPORT0
rled_pin	= 0		@ P0.0 (bit 0 of IOPORT0), but board has no LED
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioport0_base	@ override button is on IOPORT0
BOOTOVERRID_BUT	= 3		@ override button is on P0.3
@ --------- FREQs -----------
PLL_parms	= 0x2C019	@ PLL -> 96 MHz with 25 MHz crystal
@UART0_DIV	= 312		@ integer    div for   9600 baud, BRCLK = 48 MHz
@UART0_DIV2	= 32		@ fractional div for   9600 baud, BRCLK = 48 MHz
UART0_DIV	= 26		@ integer    div for 115200 baud, BRCLK = 48 MHz
UART0_DIV2	= 0		@ fractional div for 115200 baud, BRCLK = 48 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x04000000
RAMTOP		= 0x04018000	@  (96kB)
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap_8x64KB	= 1		@ STR911: 8 x 64KB, Bank 0 FLASH sectors
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00070000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00030000	@ 192KB into flash, after 2x64KB file pages+code
LIB_TOP_PAGE	= 0x00080000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80




