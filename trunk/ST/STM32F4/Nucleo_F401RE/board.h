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
Nucleo_F401RE	= 1		@ STM32 Nucleo-F401RE board, CM4F, STM32F401RET6
connectivity_ln = 1		@ USB is similar to connectivity line (OTG)

/* ======= OPTIONS ========== */
hardware_FPU	= 1		@ use the FPU (for floats)
@enable_MPU	= 1		@ use the MPU

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= ioporta_base	@ LED IO function control is on I/O Port A
LEDIO		= ioporta_base	@ LED on/off control is on I/O Port A
rled_pin	= 5		@ PA.5 (green)
yled_pin	= rled_pin	@ aliased
gled_pin	= rled_pin	@ aliased
LEDBUT_AHB1P	= (1<<0)|(1<<2)	@ bits for Ports A and C (A=0, B=1, ...)
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PRT	= ioportc_base	@ PIO port C where PC13 user button is
BOOTOVERRID_BUT	= 13		@ Button on pin 13, port C
@BOOTOVERRID_INV = 1		@ invert boot-override (normally low)
@ --------- UART ------------
swap_default_usart = 1		@ usart2 is default uart (rather than usart1)
UART_PINSEL	= ioporta_base	@ UART pin config register for USART2 = Port A
@UARLOPN		= 2		@ USART2 Tx/Rx is PA2/PA3, lowest one is 2
uart_tx		= 2		@ uart Tx pin = PA2
uart_rx		= 3		@ uart Rx pin = PA3
UART0_DIV	= (0x2d<<4 | 5)	@ div for 115200 baud at USART2 APB1 Clock=42MHz
@ --------- SD card ---------
@ --------- FREQs -----------
HSI_is_PLL_src	= 1		@ High-Speed Internal Osc. (16MHz) is PLL source
Clock_parms	= 8 | (168 << 6) | (1 << 16) | (0 << 22) | (7 << 24)
Prescl_parms	= (0 << 4) | (4 << 10) | (0 << 13) | (0 << 16)
SYSTICK_RELOAD	= 84*10000 - 1	@ systick reload for 10ms interrupts at   84 MHz
FLSH_WAIT_ST	= 4		@ 4 wait states for flash at 84 MHz
@ --------- RAM ------------
RAMBOTTOM	= 0x20000000
RAMTOP		= 0x20018000	@ top of STM32F401RET6 96 KB SRAM
@ --------- BUFFERS --------
RBF_size	= 0x0800	@ READBUFFER  size for tag as bytevector (2KB)
@WBF_size	= 0x0800	@ WRITEBUFFER size for tag as bytevector (2KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
flashmapB4x16KB	= 1		@ STM32F401xE sectors: 4x16KB, 1x64KB, 3x128KB
F_START_PAGE	= 0x08020000	@ address of 1st page of FLASH for files
F_END_PAGE	= 0x08060000	@ page after last page of FLASH used for files
F_PAGE_SIZE	= 256		@ size of pages used for files
SHARED_LIB_FILE	= 1		@ library and file space share on-chip flash
LIB_BOTTOM_PAGE	= 0x08060000	@ above files (after 2x128KB file sectors+code)
LIB_TOP_PAGE	= 0x08080000	@ end of flash

@-------10--------20--------30--------40--------50--------60--------70--------80



