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
LPC2478_STK	= 1		@ Olimex LPC2478_STK / LPC 2478

/* ======= OPTIONS ========== */
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD-card file system
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= PINSEL2	@ USB_UP_LED2 (Host)
LEDIO		= io1_base	@ LED is on IO1PINs
rled_pin	= 13		@ P1.13
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= 0x3fffc054	@ Boot-override is on I/O Port 2 (fio2pin adrs)
BOOTOVERRID_BUT	= 19		@ Boot-override button is P2.19
@ --------- SD card ---------
sd_is_on_mci	= 1		@ SD card uses MCI interface
sd_mci		= mci_base	@ SD card is on mci port
@ --------- FREQs -----------
CLOCK_FREQ	= 0x011940	@ 72000 kHz = clock frequency
PLL_PM_parms	= 0x0B		@ 12 MHz Xtal PLL div 1, mul 12 -> 288MHZ
@UART0_DIV_L	= 0xD5		@ lower byte of div for   9600 baud, pclk =72MHz
@UART0_DIV_H	= 0x01		@ upper byte of div for   9600 baud, pclk =72MHz
UART0_DIV_L	= 39		@ lower byte of div for 115200 baud, pclk =72MHz
UART0_DIV_H	= 0		@ upper byte of div for 115200 baud, pclk =72MHz
@ --------- RAM ------------
RAMBOTTOM	= 0xA0000000
RAMTOP		= 0xA4000000	@ LPC 2478-STK external SDRAM (64MB)
@ --------- BUFFERS --------
BUFFER_START	= 0x40000000+4	@ LPC 2478 internal SRAM (64 kB)
RBF_size	= 0x7000	@ READBUFFER size for tag as bytevector  (28KB)
WBF_size	= 0x7000	@ WRITEBUFFER size for tag as bytevector (28KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap14x32KB	= 1		@ LPC2478 flash, 8x4KB + 14x32KB + 5x4KB sectors
IAP_ARGS_ADRS	= 0x4000F000	@ address for IAP arguments (60KB in onchip RAM)
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00070000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00020000	@ 128KB into flash, after 2x32KB file pages+code
LIB_TOP_PAGE	= 0x0007D000	@ start of boot block

@-------10--------20--------30--------40--------50--------60--------70--------80




