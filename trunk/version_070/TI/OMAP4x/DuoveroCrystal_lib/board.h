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
GMX_DUOVERO_CRYSTAL = 1		@ Gumstix Duovero Crystal OMAP4430 1GHz 1GB RAM

/* ======= OPTIONS ========== */

standalone_lib	= 1
_text_link_address_	= 0x0000c000	@ .text and .data run from 48KB into RAM

enable_a9_mpcore= 1		@ uncomment to run 2 cores
MP_numcores	= 2		@ number of cores (for: enable_a9_mpcore)
hardware_FPU	= 1		@ comment out to use soft float (no FPU/VFP use)
native_usb	= 1		@ comment out to exclude usb support
live_SD 	= 1		@ live SD version (no flash on this board)
onboard_SDFT	= 1		@ comment out to exclude SD card file subsystem
@enable_MPU	= 1		@ comment this out to not use the MPU (via MMU)
stop_and_copy	= 1		@ stop-and-copy gc (reduces boot mem below 48KB)
top_16MB_frmbuf	= 1		@ set top 16MB of SDRAM as non-cached space
sdhc_on_sd_mmc	= 1		@ use SDHC cards on MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ ---- LEDs (Parlor) -------
LEDIO		= 0x48059000 	@ GPIO Port 4, control Parlor board LED on/off
/*
REDLED		= 1 << 26	@ bit 26, GPIO 122 (Parlor blue LED)
YELLED		= REDLED	@ aliased to red led
GRNLED		= REDLED	@ aliased to red led
*/
rled_pin	= 26		@ bit 26, GPIO 122 (Parlor blue LED)
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
@ --- Live SD / SD Card ----
sd_is_on_mci	= 1		@ SD card uses MCI interface
sd_mci		= mmc1_base	@ SD card is on mmc1
@ --------- FREQs -----------
UART_DIVL	= 0x1A		@ low  div for 115200 baud, uart clk = 48 MHz
UART_DIVH	= 0x00		@ high div for 115200 baud, uart clk = 48 MHz
@ --------- RAM ------------
SDRAMBOTTOM	= 0x80000000	@ Bottom of SDRAM          on board
SDRAMTOP	= 0xc0000000	@ Top    of SDRAM (1024MB) on board
RAMBOTTOM	= SDRAMBOTTOM	@ Bottom of SDRAM          for cpu0
RAMTOP		= 0xa0000000	@ Top    of SDRAM  (512MB) for cpu0
RAMBOTTOM_1	= 0xa0000000	@ Bottom of SDRAM          for cpu1
RAMTOP_1	= SDRAMTOP	@ Top    of SDRAM (1024MB) for cpu1
@ --------- BUFFERS --------
heapbottom	= RAMBOTTOM   + (1<<20)	    @   1MB into 1st 1/2 SDRAM for cpu0
BUFFER_START	= RAMBOTTOM   + (1<<17) + 4 @ 128KB into 1st 1/2 SDRAM for cpu0
I2C0ADR		= i2c0_base + i2c_address   @ for cpu0

heapbottom_1	= RAMBOTTOM_1 + (1<<20)	    @   1MB into 2nd 1/2 SDRAM for cpu1
BUFFER_START_1	= RAMBOTTOM_1 + (1<<17) + 4 @ 128KB into 2nd 1/2 SDRAM for cpu1
I2C0ADR_1	= i2c0_base + i2c_address_1 @ for cpu1

RBF_size	= 0x10000	@ READBUFFER  size for tag as bytevector (64 KB)
WBF_size	= 0x10000	@ WRITEBUFFER size for tag as bytevector (64 KB)

@-------10--------20--------30--------40--------50--------60--------70--------80




