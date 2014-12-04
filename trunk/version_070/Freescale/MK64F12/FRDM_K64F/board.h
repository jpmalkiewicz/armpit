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
FRDM_K64F	= 1		@ Freescale FRDM-K64F Cortex-M4F MK64FN1M0VLL12

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC i/f (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
invert_LED	= 1		@ Hi turns LED off, Lo turns LED on
LEDIO		= ioportb_base	@ LED on/off control is on I/O Port B
rled_pin	= 22		@ PB22 line to red  side of RGB LED
yled_pin	= rled_pin	@ aliased
gled_pin	= 21		@ PB21 line to blue side of RGB LED
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioportc_base	@ Boot-override (SW2) is on I/O Port C
BOOTOVERRID_BUT	= 6		@ Boot-override is PC6
@ --------- SD card ---------
sd_is_on_mci	= 1		@ SD card uses MCI (sdhc) interface
sd_mci		= sdhc0_base	@ SD card is on sdhc0 port
@ SPI option (may be buggy)
@sd_is_on_spi	= 1		@ SD card uses SPI interface
@sd_spi		= spi1_base	@ SD card is on SPI1
@sd_spi_gpio	= ioporte_base	@ SD card/SPI0 gpioE PTE1,2,3=SIN,SCK,SOUT
@sd_cs_gpio	= ioporte_base	@ SD card chip-select is on gpioE
@sd_cs_pin	= 4		@ SD card chip-select pin is PTE4
@sd_cs_wait	= 5 << 17	@ SD wait countdown on CS change
@spi_clr_status = 1		@ write contents of STAT reg to clear it
@ --------- FREQs -----------
UART0_IDIV	= 65		@ integer    divisor for 115200 baud at 120 MHz
UART0_FDIV	=  3		@ fractional divisor for 115200 baud at 120 MHz
SYSTICK_RELOAD	= 120*10000 - 1	@ systick reload for 10ms interrupts at 120 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x1fff0000	@ start of on-chip RAM
.ifndef native_usb
  RAMTOP	= 0x20030000	@ (256 kB)
.else
  RAMTOP	= 0x20030000 - 1024 @ 256 kB minus 1024 for USB buffers & align.
.endif
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmap128x4KB = 1		@ MK64FN1M0LL12, 128x4KB sectors of BANK0, p.105
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x0007f000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00012000	@ 96 KB in flash (after 2x16 KB file pages+code)
LIB_TOP_PAGE	= 0x00080000	@ end of flash
/* alternative map for BANK 1 * /
flashmap128x4KB_BANK_1 = 1	@ MK64FN1M0LL12, 128x4KB sectors of BANK1, p.105
F_START_PAGE	= 0x00080000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x000ff000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00082000	@ 96 KB in flash (after 2x16 KB file pages+code)
LIB_TOP_PAGE	= 0x00100000	@ end of flash
*/
@-------10--------20--------30--------40--------50--------60--------70--------80



