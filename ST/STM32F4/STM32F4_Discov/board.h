/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2012-2014 Petr Cermak

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
STM32F4_Discov	= 1		@ STM32F4 Discovery board, Cortex-M4F
connectivity_ln = 1		@ USB is similar to connectivity line (OTG)

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
native_usb	= 1		@ include usb support
harvard_split	= 1		@ store .data section in CCM
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ SDHC cards on SPI/MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioportd_base	@ LED IO function control is on I/O Port D
LEDIO		= ioportd_base	@ LED on/off control is on I/O Port D
rled_pin	= 14		@ PD.14
yled_pin	= 13		@ PD.13 -- STAT2 yellow LED on board
gled_pin	= 12		@ PD.12 -- STAT1 green  LED on board
LEDBUT_AHB1P	= (1<<0)|(1<<3)	@ bits for Ports A and D (A=0, B=1, ...)
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioporta_base	@ PIO port A where PA0 user button is
BOOTOVERRID_BUT	= 0		@ Button on pin 0, port A
BOOTOVERRID_INV = 1		@ invert boot-override (normally low)
@ --------- UART ------------
UART_PINSEL	= ioportb_base	@ UART pin config register for USART1 = Port B
@UARLOPN		= 6		@ USART1 Tx/Rx is PB6/PB7, lowest one is 6
uart_tx		= 6		@ uart Tx pin = PB6
uart_rx		= 7		@ uart Rx pin = PB7
UART0_DIV	= (0x5b<<4 | 1)	@ divisor for 115200 baud at APB2 Clock = 84 MHz
@ --------- SD card ---------
@ PINS: PB11=CS(GPIO),PB12=NSS[CONNECT to 3V],PB13=CLK,PB14=MISO,PB15=MOSI
@ Note: all pins must be either within 0-7 range or within 8-15 range.
@ Note: if errors occur on long files adjust hi speed at sd_fst: in file_sd_hw.s
sd_is_on_spi	= 1		@ SD card uses SPI interface
sd_spi		= spi2_base	@ SD card is on SPI2 (AF5)
sd_spi_APB_bit	= 14		@ SPI2 enable bit in RCC_APB1ENR
sd_spi_APB_ofst	= 0x40		@ offset to RCC_APB1ENR in RCC
sd_spi_gpio	= ioportb_base	@ SD card SPI2 portB PB13-15=SCK,MISO,MOSI,12=SS
sd_cs_gpio	= sd_spi_gpio	@ SD card chip-select on same port as SPI pins
sd_gpio_AHB_bit	= 1		@ Port B enable bit in RCC_AHB1ENR (A=0,B=1,...)
sd_clk		= 13		@ SD SPI CLK  pin = PB.13
sd_nss		= 12		@ SD SPI NSS  pin = PB.12
sd_miso		= 14		@ SD SPI MISO pin = PB.14
sd_mosi		= 15		@ SD SPI MOSI pin = PB.15
sd_cs_pin	= 11		@ SD card chip-select pin PB.11
@ --------- FREQs -----------
Clock_parms	= 4 | (168 << 6) | (0 << 16) | (1 << 22) | (7 << 24)
Prescl_parms	= (0 << 4) | (5 << 10) | (4 << 13) | (8 << 16)
SYSTICK_RELOAD	= 168*10000 - 1	@ systick reload for 10ms interrupts at  168 MHz
FLSH_WAIT_ST	= 6		@ 6 wait states for flash at 168 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
RAMTOP		= 0x2001C000	@ top of STM32F407VC 112 KB SRAM
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmapA4x16KB	= 1		@ STM32F4 sectors: 4x16KB, 1x64KB, 7x128KB
F_START_PAGE	= 0x08020000	@ address of 1st page of FLASH for files
F_END_PAGE	= 0x080E0000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x08060000	@ above files (after 2x128KB file sectors+code)
LIB_TOP_PAGE	= 0x08100000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80



