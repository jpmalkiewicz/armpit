/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2012-2014 Hubert Montas

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
STM32F429_Disco	= 1		@ STM32F429 Discovery board, M4F, STM32F429ZIT6U
connectivity_ln = 1		@ USB is similar to connectivity line (OTG)

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
heap_in_SDRAM	= 1		@ use SDRAM (8 MB) for heap
native_usb	= 1		@ include usb support
USB_is_OTG_HS	= 1		@ usb uses the OTG_HS hardware of the chip
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ SDHC cards on SPI/MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioportg_base	@ LED IO function control is on I/O Port G
LEDIO		= ioportg_base	@ LED on/off control is on I/O Port G
rled_pin	= 14		@ PG.14 -- LD4 red    LED on board
yled_pin	= rled_pin	@ aliased
gled_pin	= 13		@ PG.13 -- LD3 green  LED on board
LEDBUT_AHB1P	= (1<<0)|(1<<6)	@ bits for Ports A and G (A=0, B=1, ...)
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioporta_base	@ PIO port A where PA0 user button is
BOOTOVERRID_BUT	= 0		@ Button on pin 0, port A
@ --------- UART ------------
UART_PINSEL	= ioporta_base	@ UART pin config register for USART1 = Port A
@UARLOPN		= 9		@ USART1 Tx/Rx is PA9/PA10, lowest one is 9
uart_tx		=  9		@ uart Tx pin = PA9
uart_rx		= 10		@ uart Rx pin = PA10
UART0_DIV	= (0x5b<<4 | 1)	@ divisor for 115200 baud at APB2 Clock = 84 MHz
@ --------- SD card ---------
@ PINS: PE2=CLK,PE3=CS(GPIO),PE4=NSS[CONNECT to 3V],PE5=MISO,PE6=MOSI
@ Note: all pins must be either within 0-7 range or within 8-15 range.
@ Note: if errors occur on long files adjust hi speed at sd_fst: in file_sd_hw.s
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi4_base	@ SD card is on SPI4 (AF5)
sd_spi_APB_bit	= 13		@ SPI4 enable bit in RCC_APB2ENR
sd_spi_APB_ofst	= 0x44		@ offset to RCC_APB2ENR in RCC
sd_spi_gpio	= ioporte_base	@ SD card SPI4 port E PE2,5,6=SCK,MISO,MOSI,4=SS
sd_cs_gpio	= sd_spi_gpio	@ SD card chip-select on same port as SPI pins
sd_gpio_AHB_bit	= 4		@ Port E enable bit in RCC_AHB1ENR (A=0,B=1,...)
sd_clk		= 2		@ SD SPI CLK  pin = PE.2
sd_nss		= 4		@ SD SPI NSS  pin = PE.4
sd_miso		= 5		@ SD SPI MISO pin = PE.5
sd_mosi		= 6		@ SD SPI MOSI pin = PE.6
sd_cs_pin	= 3		@ SD card chip-select pin PE.3
@ --------- FREQs -----------
@ Note: MPU can go to 180 MHz -- here, 168 MHz config is used
Clock_parms	= 4 | (168 << 6) | (0 << 16) | (1 << 22) | (7 << 24)
Prescl_parms	= (0 << 4) | (5 << 10) | (4 << 13) | (8 << 16)
SYSTICK_RELOAD	= 168*10000 - 1	@ systick reload for 10ms interrupts at  168 MHz
FLSH_WAIT_ST	= 5		@ 5 wait states for flash at up to 180 MHz
@ --------- RAM ------------
.ifndef heap_in_SDRAM
  RAMBOTTOM	= 0x20000000
  RAMTOP	= 0x20030000	@ top of STM32F407VC on-chip 192 KB SRAM
.else
  RAMBOTTOM	= 0xd0000000	@ bottom of SDRAM Bank 2 (SDNE1 CE, FMC Bank 6)
  RAMTOP	= 0xd0800000	@ top of ISSI SDRAM IS42S16400J-7TL (8 MB)
  MAIN_STACK 	= 0x20030000	@ top of STM32F407VC on-chip 192 KB SRAM
.endif
@ --------- BUFFERS --------
.ifndef heap_in_SDRAM
  RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
  WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
.else
  RBF_size	= 0x8000	@ READBUFFER  size for tag as bytevector (32KB)
  WBF_size	= 0x8000	@ WRITEBUFFER size for tag as bytevector (32KB)
.endif
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
@ Note: we're using the first bank only (there's another 1MB bank)
flashmapA4x16KB	= 1		@ STM32F4 sectors: 4x16KB, 1x64KB, 7x128KB
F_START_PAGE	= 0x08020000	@ address of 1st page of FLASH for files
F_END_PAGE	= 0x080E0000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x08060000	@ above files (after 2x128KB file sectors+code)
LIB_TOP_PAGE	= 0x08100000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80



