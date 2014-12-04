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
SAM4S_XPLD	= 1		@ Atmel/Embest SAM4S-Xplained / AT91-SAM4S16C

/* ======= OPTIONS ========== */
@run_at_84_MHz	= 1		@ run at 84 MHz (default is 120MHz)
enable_MPU	= 1		@ use the MPU
external_ram	= 1		@ enable ISSI external RAM: NCS0,1
native_usb	= 1		@ include usb support
@onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on SPI/MMC (no normal SD)

/* ===== CONFIGURATION ====== */
@ ----- LEDs / BUTTON -------
LEDPINSEL	= pioc_base	@ where to write 1 to make pin a gpio
LEDIO		= pioc_base
led_per_id	= 13		@ PIO C peripheral ID = 13
rled_pin	= 10		@ PC10 (actually yellow)
yled_pin	= 17		@ PC17 (actually yellow)
gled_pin	= rled_pin	@ aliased
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= pioa_base	@ Boot-override (BP2) is on I/O Port A
but_per_id	= 11		@ PIO A peripheral ID = 11
BOOTOVERRID_BUT	= 5		@ Boot-override button is on pin PA5
@ ---------- UART -----------
switch_uart01	= 1		@ use uart1 (or usart1) as scheme uart0
uart_not_usart	= 1		@ use uart  (not usart) as scheme uarts
uart_gpio	= piob_base	@ uart1 (scheme uart0) is on PB pins
uart_per_id	= 9		@ uart1 peripheral ID = 9
uart_rx		= 2		@ uart1 Rx pin = PB2
uart_tx		= 3		@ uart1 Tx pin = PB3
@uart_gpio	= pioa_base	@ alt -- uart0, usart0, usart1 are on PA pins
@uart_per_id	=  8		@ alt -- uart0  peripheral ID = 8
@uart_rx	=  9		@ alt -- uart0  Rx pin = PA9
@uart_tx	= 10		@ alt -- uart0  Tx pin = PA10
@uart_per_id	= 14		@ alt -- usart0 peripheral ID = 14
@uart_rx	=  5		@ alt -- usart0 Rx pin = PA5
@uart_tx	=  6		@ alt -- usart0 Tx pin = PA6
@uart_per_id	= 15		@ alt -- usart1 peripheral ID = 15
@uart_rx	= 21		@ alt -- usart1 Rx pin = PA21
@uart_tx	= 22		@ alt -- usart1 Tx pin = PA22
.ifndef run_at_84_MHz
  uart_div	= 65		@ uart0 frequency div for 115200 bauds at 120MHz
.else
  uart_div	= 46		@ uart0 frequency div for 115200 bauds at  84MHz
.endif
@ --------- SD card ---------
sd_is_on_spi	= 1		@ if used, SD card will be on SPI interface
sd_spi		= spi0_base	@ SD card is on SPI0
sd_spi_gpio	= pioa_base	@ SD card port A PA11,12,13,14=SS,MISO,MOSI,CLK
sd_spi_per_id	= 21		@ SPI0 is peripheral 21
sd_nss		= 11		@ SD SPI NSS  pin = PA.11
sd_miso		= 12		@ SD SPI MISO pin = PA.12
sd_mosi		= 13		@ SD SPI MOSI pin = PA.13
sd_clk		= 14		@ SD SPI CLK  pin = PA.14
.ifndef run_at_84_MHz
  sd_spi_slo	= 255		@ SPI low  speed SCBR: 120 MHz/255 = 470 KHz
  sd_spi_fst	= 10		@ SPI high speed SCBR: 120 MHz/10  =  12 MHz
.else
  sd_spi_slo	= 255		@ SPI low  speed SCBR:  84 MHz/255 = 329 KHz
  sd_spi_fst	= 10		@ SPI high speed SCBR:  84 MHz/10  = 8.4 MHz
.endif
@ --------- FREQs -----------
.ifndef run_at_84_MHz
 PLL_parmsA	= 0x20273F02	@ XTal=12 MHz, PLLA x40/2 -> 120x2 MHz
 PLL_parmsB	= 0x000F3F02	@ XTal=12 MHz, PLLB x16/2 ->  48x2 MHz for USB
 FLSH_WTSTA 	= 5		@ flash wait states for 120MHz: 5 =>6 cycles r/w
 SYSTICK_RELOAD	= 120*10000 - 1	@ systick reload for 10ms interrupts at 120 MHz
 SMC_SETUP	= 0x00000100	@ ISSI 66WV51216DBLL, 55ns r/w setup clock ticks
 SMC_PULSE	= 0x07070607	@ ISSI 66WV51216DBLL, 55ns r/w pulse clock ticks
 SMC_CYCLE	= 0x00070007	@ ISSI 66WV51216DBLL, 55ns r/w cycle clock ticks
.else
 PLL_parmsA	= 0x201b3f02	@ XTal=12 MHz, PLLA x28/2 ->  84x2 MHz
 PLL_parmsB	= 0x000f3f02	@ XTal=12 MHz, PLLB x16/2 ->  48x2 MHz for USB
 FLSH_WTSTA 	= 3		@ flash wait states for  84MHz: 3 =>4 cycles r/w
 SYSTICK_RELOAD	= 84*10000 - 1	@ systick reload for 10ms interrupts at  84 MHz
 SMC_SETUP	= 0x00000100	@ ISSI 66WV51216DBLL, 55ns r/w setup clock ticks
 SMC_PULSE	= 0x05050405	@ ISSI 66WV51216DBLL, 55ns r/w pulse clock ticks
 SMC_CYCLE	= 0x00050005	@ ISSI 66WV51216DBLL, 55ns r/w cycle clock ticks
.endif
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000	@ bottom of RAM
RAMTOP		= 0x20020000	@ AT91-SAM4S16C (128KB)
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER size for tag as bytevector (2KB)
WBF_size	= 0x0800	@ READBUFFER size for tag as bytevector (2KB)
@ --------- FLASH ----------
flashmap_8x64KB	= 1		@ AT91-SAM4S16C: 8 x 64KB sectors
F_START_PAGE	= 0x00010000	@ address of 1st page of FLASH (for files)
F_END_PAGE	= 0x00070000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 512		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x00030000	@ 192KB into flash (after 2x64KB fil sects+code)
LIB_TOP_PAGE	= 0x00080000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80




