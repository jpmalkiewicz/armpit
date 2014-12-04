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

/*----------------------------------------------------------------------------*\
|										|
|			1. Device Family Constants				|
|										|
|			(followed by device family macros)			|
|										|
\*----------------------------------------------------------------------------*/

@ family
LM_4Fxxx	= 1			@ LM4Fxxx family of MCUs

@ architecture
cortex		= 1
.cpu		cortex-m4
.fpu		fpv4-sp-d16

@ type of gpio set/clear
@has_combined_set_clear	= 1		@ MCU has combined GPIO SET / CLEAR reg
stellaris_gpio	= 1			@ Stellaris-type gpio set/clear

@ interrupts
.ifndef EK_TM4C1294
  num_interrupts = 139
.else
  num_interrupts = 114
.endif
uart0_int_num	= 5
uart1_int_num	= 6
timer0_int_num	= 19			@ also int 20, not treated
timer1_int_num	= 21			@ also int 22, not treated
i2c0_int_num	= 8
i2c1_int_num	= 37
.ifndef EK_TM4C1294
  usb_int_num	= 44			@ USB-0
.else
  usb_int_num	= 42			@ USB-0 MAC
.endif

timer0_int	= 1 << (timer0_int_num % 32)
timer1_int	= 1 << (timer1_int_num % 32)
i2c0_int	= 1 << (i2c0_int_num   % 32)
i2c1_int	= 1 << (i2c1_int_num   % 32)
uart0_int	= 1 << (uart0_int_num  % 32)
uart1_int	= 1 << (uart1_int_num  % 32)
usb_int		= 1 << (usb_int_num    % 32)

int_status	= int_stat__0__31	@ where to find the timer interrupts
scminten__0__31	= timer0_int | timer1_int | i2c0_int | uart0_int | uart1_int
scminten_32__63	= i2c1_int   | usb_int
scminten_64__95	= 0
scminten_96_127	= 0
scminten128_159	= 0

@ gpio

.ifndef GPIO_on_AHB
ioporta_base	= 0x40004000		@ I/O Port A base address -- APB
ioportb_base	= 0x40005000		@ I/O Port B base address -- APB
ioportc_base	= 0x40006000		@ I/O Port C base address -- APB
ioportd_base	= 0x40007000		@ I/O Port D base address -- APB
ioporte_base	= 0x40024000		@ I/O Port E base address -- APB
ioportf_base	= 0x40025000		@ I/O Port F base address -- APB
ioportg_base	= 0x40026000		@ I/O Port G base address -- APB
ioporth_base	= 0x40027000		@ I/O Port H base address -- APB
.else
ioporta_base	= 0x40058000		@ I/O Port A base address -- AHB
ioportb_base	= 0x40059000		@ I/O Port B base address -- AHB
ioportc_base	= 0x4005A000		@ I/O Port C base address -- AHB
ioportd_base	= 0x4005B000		@ I/O Port D base address -- AHB
ioporte_base	= 0x4005C000		@ I/O Port E base address -- AHB
ioportf_base	= 0x4005D000		@ I/O Port F base address -- AHB
ioportg_base	= 0x4005E000		@ I/O Port G base address -- AHB
ioporth_base	= 0x4005F000		@ I/O Port H base address -- AHB
.endif
ioportj_base	= 0x40060000		@ I/O Port J base address -- AHB
ioportk_base	= 0x40061000		@ I/O Port K base address -- AHB
ioportl_base	= 0x40062000		@ I/O Port L base address -- AHB
ioportm_base	= 0x40063000		@ I/O Port M base address -- AHB
ioportn_base	= 0x40064000		@ I/O Port N base address -- AHB
ioportp_base	= 0x40065000		@ I/O Port P base address -- AHB
io_set		= 0x03fc		@ all bits count (read-modify-write mod)
io_clear	= 0x03fc		@ all bits count (read-modify-write mod)
io_state	= 0x03fc		@ all bits count (read-modify-write mod)

@ uarts	
uart0_base	= 0x4000C000		@ UART0
uart1_base	= 0x4000D000		@ UART1
uart_rhr	= 0x00			@ UARTDR
uart_thr	= 0x00			@ UARTDR
uart_status	= 0x18			@ UARTFR
uart_txrdy	= 0x80			@ Tx FIFO Empty
uart_istat	= 0x40			@ UARTMIS <- int may clr in uart too
uart_ier	= 0x04			@ uart interrupt enable  reg offset
uart_idr	= uart_ier		@ uart interrupt disable reg offset
uart_iRx_ena	= 1			@ uart value to enable  interrupts
uart_iRx_dis	= 0			@ uart value to disable interrupts

@ spi
ssi0_base	= 0x40008000
ssi1_base	= 0x40009000
spi_rhr		= 0x08
spi_thr		= 0x08
spi_status	= 0x0c
spi_rxrdy	= 0x04
spi_txrdy	= 0x02

@ adc
adc0_base	= 0x40038000

@ pwm
pwm0_base	= 0x40028000

@ i2c -- NOT DONE !!!	
i2c0_base	= 0x40020000		@ I2C0 Master
i2c1_base	= 0x40021000		@ I2C1 Master
i2c_address	= 0x0800		@ Slave-Own-Address offset from Master
i2c_rhr		= 0
i2c_thr		= 0
i2c_irm_rcv	= 0
i2c_irs_rcv	= 0

@ timers
timer0_base	= 0x40030000		@ TIMER 0
timer1_base	= 0x40031000		@ TIMER 1
timer2_base	= 0x40032000		@ TIMER 2
timer3_base	= 0x40033000		@ TIMER 3
timer_ctrl	= 0x0C			@ GPTMCTL, GPTM Control
timer_istat	= 0x20			@ GPTMMIS, GPTM Masked Interrupt Status
timer_iset	= 0x24			@ GPTMICR, GPTM Interrupt Clear

@ power
sys_base	= 0x400FE000		@ System Control -- RCC base address
rcc_base	= 0x400FE000		@ System Control -- RCC base address
rcc		= 0x60
rcc2		= 0x70
rcgc_base	= rcc_base + 0x0600	@ Peripheral Specific RCGC register base
pr_base		= rcc_base + 0x0a00	@ Peripheral Specific PR   Periph-Ready

@ flash	
flashcr_base	= 0x400FD000		@ FLASH control registers base

@ usb
usb_base	= 0x40050000		@ base address
usb_busreset 	= 0x04			@ bit 2 in USBIS
usb_suspend  	= 0x00
usb_ibulkout	= 0x126			@ status of Bulk OUT EP -- USBTXCSRL2
usb_ibulkin	= 0x132			@ status of Bulk IN  EP -- USBTXCSRL3
usb_txrdy	= 1			@ Tx ready bit in usb_iBulk_IN
usb_istat_dv 	= 0x00			@ USBFADDR w/USBTXIS in upper half-word
usb_istat_dv2 	= 0x0a			@ USBIS
usb_idv_mask 	= 0x07
usb_ctl_stat  	= 0x0102		@ USBCSRL0
usb_iep_mask 	= 0x1f
usbCO_ibit	= 0x01			@ 
usbCI_ibit	= 0x02			@ 
usbBO_ibit	= 0x04			@ 
usbBI_ibit	= 0x08			@ 
usbCO_setupbit	= 0x10			@ bit 4 (SETUP_END) in USBCSRL0
usb_daddr 	= 0x00
UsbControlOutEP	= 0			@ 
UsbControlInEP	= 0			@ 
UsbBulkOutEP	= 2			@ 
UsbBulkInEP	= 3			@ 
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for desc at end file)


/*----------------------------------------------------------------------------*\
|										|
|			2. Device Family Macros					|
|										|
\*----------------------------------------------------------------------------*/

.macro	clearUartInt	
	@ clear interrupt in uart with base address in rva
	@ nothing to do on this MCU
.endm

.macro	clearTimerInt	
	@ clear interrupt in timer peripheral block with base address in rva
	read	rvc, rva, #timer_istat	@ at91sam7
	write	rvc, rva, #timer_iset	@ lpc2000
	set	rvc, 0			@ rvc <- 0
	write	rvc, rva, #timer_iset	@ str711, STM32
.endm

.macro	usbldr dest, src, ofst
	read8	\dest, \src, #\ofst
.endm

.macro	usbstr dest, src, ofst
	write8	\dest, \src, #\ofst
.endm

.macro	usbstrne dest, src, ofst
	write8ne \dest, \src, #\ofst
.endm



