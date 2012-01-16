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
@ Olimex SAM9-L9261 / AT91-SAM9261
@ ------- FAMILY -----------
AT91_SAM9	= 1			@ ATMEL AT91SAM9 family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= pioa_base		@ write 1 here to make pin a gpio
LEDIO		= pioa_base
REDLED		= (1 << 13)		@ PA13, low  = on, (actually green LED, LED1 on board)
YELLED		= (1 << 23)		@ PA23, high = on  (LED3)
GRNLED		= (1 << 14)		@ PA14, low  = on  (LED2)
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi0_base		@ SD card is on SPI0
sd_spi_gpio	= pioa_base		@ SD card / SPI0 is on IO port A (PA.0,1,2=MISO,MOSI,CLK)
sd_cs_gpio	= pioa_base		@ SD card chip-select is on IO port A
sd_cs		= 1 << 6		@ SD card chip-select is on pin PA.6 (the 6 in 1<< 6)
@ --------- FREQs -----------
PLLA_parms	= 0x204bbf07		@ PLLA: XTal=18.432 MHz, PLL mul=75+1, wt=63, div=7 -> CPU = 200.1 MHz
					@       Peripherals run at CPU / 2 = 100 MHz (PMC_MCKR register)
					@ PLLB: preconfigured by BootROM for 96.1 MHz -> USB = 48 MHz
UART0_DIV	= 651			@ uart0 (DBGU uart) frequency divisor for 9600 bauds at 100 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000		@ Bottom of off-chip SDRAM
RAMTOP		= 0x23F00000		@ 63 MB (use top 1 MB of 64 MB of SDRAM to shadow FLASH)
@ --------- BUFFERS --------
BUFFER_START	= 0x020000		@ 128 KB into re-mapped on-chip RAM (160 KB total size)
RBF_size	= 0x2000		@ READBUFFER size for tag as bytevector (8KB including 4-byte tag)
WBF_size	= 0x2000		@ READBUFFER size for tag as bytevector (8KB including 4-byte tag)
@ --------- FLASH ----------
F_START_PAGE	= RAMTOP		@ address of 1st page of FLASH (for files) = shadow RAM start
F_END_PAGE	= 0x23FE0000		@ address of page (128 KB block) after last page used for files
F_PAGE_SIZE	= 2048			@ size of pages used for files
@-------------------------------------------------------------------------------------
