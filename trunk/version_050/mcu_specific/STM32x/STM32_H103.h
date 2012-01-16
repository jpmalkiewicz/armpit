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
@ Olimex STM32-H103 / STM32F103RBT6 Cortex-M3
@ ------- FAMILY -----------
STM32x		= 1			@ ST STM32x family MCU
@ ------ Native USB --------
native_usb	= 1			@ comment this line out to assemble without usb support
@ --------- LEDs -----------
LEDPINSEL	= ioportc_base		@ LED IO function control is on I/O Port C
LEDIO		= ioportc_base		@ LED on/off control is on I/O Port C
REDLED		= GRNLED		@ aliased to grnled
YELLED		= GRNLED		@ aliased to grnled
GRNLED		= 0x1000		@ PC12 (the only board LED except for power)
@ --------- SD card ---------
onboard_SDFT	= 1
sd_is_on_spi	= 1
sd_spi		= spi1_base		@ SD card is on SPI1
sd_spi_gpio	= ioporta_base		@ SD card / SPI1 is on IO port A (PA.4,5,6,7=SS,SCK,MISO,MOSI)
sd_cs_gpio	= ioporta_base		@ SD card chip-select is on IO port A
sd_cs		= 1 << 8		@ SD card chip-select is on pin PA.8 (the 8 in 1<< 8)
@ --------- FREQs -----------
Clock_parms	= 0x001d2402		@ USB->48MHz, HSE->PLL mult 9->72MHz->AHB, 36MHz->ADC->APB1->APB2
UART0_DIV	= 0x0ea6		@ divisor for 9600 baud at APB2 Clock = 36 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
@debug_usb = 1
.ifndef	debug_usb
RAMTOP		= 0x20005000		@ top of STM32 20 KB SRAM
.else
RAMTOP		= 0x20004000		@ 16 KB SRAM (4 KB used to store USB interrupt info)
.endif
@ --------- BUFFERS --------
RBF_size	= 0x0600		@ READBUFFER size for tag as bytevector (1.5KB including 4-byte tag)
WBF_size	= 0x0600		@ WRITEBUFFER size for tag as bytevector (1.5KB including 4-byte tag)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
F_START_PAGE	= 0x08010000		@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x0801fC00		@ address of page after last page of FLASH used for files
F_PAGE_SIZE	= 256			@ size of pages used for files
SHARED_LIB_FILE	= 1			@ library and file space are both in on-chip flash
LIB_BOTTOM_PAGE	= 0x08010800		@ 66KB into flash (page after 2 x 1 KB file pages, after code)
LIB_TOP_PAGE	= 0x08020000		@ end of flash
@-------------------------------------------------------------------------------------
