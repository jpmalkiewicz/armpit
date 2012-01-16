@---------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

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
@---------------------------------------------------------------------------------------------------

@-------------------------------------------------------------------------------------
@ Olimex CS-E9302 / EP9302
@ ------- FAMILY -----------
EP_93xx		= 1			@ CIRRUS EP93xx family MCU
@ ---------- FPU -----------
hardware_FPU	= 1			@ comment this to use soft float (no FPU use)
FPU_is_maverick = 1			@ FPU is Maverick Crunch (rather than default VFP/NEON)
@ --------- LEDs -----------
LEDIO		= 0x80840020		@ address of register controlling board LED on/off pins
REDLED		= 0x02			@ bit 1
YELLED		= 0x02			@ aliased to red led
GRNLED		= 0x01			@ bit 0
@ --------- SD card --------- 
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0 (aka SPI1)
sd_cs_gpio	= ioF_base		@ SD card chip-select is on gpio_F
sd_cs		= 1 << 3		@ SD card chip-select is on pin gpio_F.3 (the 1 in << 3)
@ --------- FREQs -----------
PLL_PM_parms	= 0x02b49907		@ PLL1 parms for 166MHz
UART0_DIV_L	= 0x2F			@ lower byte of divisor for 9600 baud if pclk = 166 mhz
UART0_DIV_H	= 0x00			@ upper byte of divisor for 9600 baud if pclk = 166 mhz
@ --------- RAM ------------
RAMBOTTOM	= 0x00000000		@ SDRAM with SROMLL=1, 4 segs of 8 MB remapped to continuous 32MB
RAMTOP		= 0x02000000		@ 32MB into SDRAM
@ --------- BUFFERS --------
BUFFER_START	= RAMBOTTOM + 0x020000	@ 128kb into SDRAM (allows for scheme machine code)
RBF_size	= 0x10000		@ READBUFFER size for tag as bytevector (64 KB incl. 4-byte tag)
heapbottom	= RAMBOTTOM + 0x100000	@ 1MB into SDRAM
@ --------- FLASH ----------
F_START_PAGE	= 0x60020000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x60FE0000		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 512			@ size of pages used for files
@-------------------------------------------------------------------------------------
