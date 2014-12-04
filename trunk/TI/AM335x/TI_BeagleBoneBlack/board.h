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
BeagleBoneBlack	= 1		@ TI Beagle Bone Black AM3359, 1 GHz, 512 MB RAM

/* ======= OPTIONS ========== */
live_SD 	= 1		@ live SD version (no flash on this board)
hardware_FPU	= 1		@ use the FPU (for floats)
@enable_MPU	= 1		@ use the MPU (via MMU)
native_usb	= 1		@ include usb support
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ -------- LEDs ------------
LEDIO		= 0x4804C100	@ GPIO Port 1
rled_pin	= 21		@ bit 21, GPIO1 21
yled_pin	= 22		@ bit 22, GPIO1 22
gled_pin	= 23		@ bit 23, GPIO1 23
@ --- Live SD / SD Card ----
sd_is_on_mci	= 1		@ SD card uses MCI interface
sd_mci		= mmc0_base	@ SD card is on mmchs0
@ --------- FREQs -----------
@ Note: reduce freq_mpu and vdd_mpu if too hot or unstable (choice pairs below)
freq_mpu	= 900		@ CM_CLKSEL_DPLL_MPU <- 900 MHz, Nitro
vdd_mpu		= 0x11		@ TPS65217c DCDC2 = 1.325V, AM3359 Nitro  900MHz
@freq_mpu	= 800		@ CM_CLKSEL_DPLL_MPU <- 800 MHz, Turbo
@vdd_mpu	= 0x0f		@ TPS65217c DCDC2 = 1.275V, AM3359 Turbo  800MHz
@freq_mpu	= 700		@ CM_CLKSEL_DPLL_MPU <- 700 MHz, opp120
@vdd_mpu	= 0x0c		@ TPS65217c DCDC2 = 1.200V, AM3359 opp120 700MHz
@freq_mpu	= 600		@ CM_CLKSEL_DPLL_MPU <- 600 MHz, opp100
@vdd_mpu	= 0x08		@ TPS65217c DCDC2 = 1.100V, AM3359 opp100 600MHz
UART_DIVL	= 26		@ low  div for 115200 baud, uart clk = 48 MHz
UART_DIVH	= 0		@ high div for 115200 baud, uart clk = 48 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x80000000	@ Bottom of SDRAM
RAMTOP		= 0xa0000000	@ 512MB
@ --------- BUFFERS --------
BUFFER_START	= RAMBOTTOM+0x020000+4	@ 128kb into SDRAM
RBF_size	= 0x10000	@ READBUFFER  size for tag as bytevector (64 KB)
WBF_size	= 0x10000	@ WRITEBUFFER size for tag as bytevector (64 KB)
heapbottom	= RAMBOTTOM + 0x100000	@ 1MB into SDRAM
I2C0ADR		= i2c0_base + i2c_address

@-------10--------20--------30--------40--------50--------60--------70--------80




