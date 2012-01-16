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

@ architecture
cortex		= 1

@ type of gpio set/clear
has_combined_set_clear	= 1		@ MCU has combined GPIO SET and CLEAR register

@ interrupts
num_interrupts	= 64
uart0_int_num	= 5
uart1_int_num	= 6
timer0_int_num	= 19			@ also int 20, not treated
timer1_int_num	= 21			@ also int 22, not treated
i2c0_int_num	= 8
i2c1_int_num	= 37
int_base	= 0xe000e300		@ interrupt status base address
int_statu1	= 0x00			@ for interrupts  0 to 31 -- TMR0, TMR1, I2C0, UART0, UART1
int_statu2	= 0x04			@ for interrupts 32 to 63 -- I2C1
int_status	= int_statu1		@ where to find the timer interrupts
timer0_int	= 0x00180000		@ bit 19-20 = Timer0 from statu1 (INT 19-20)
timer1_int	= 0x00600000		@ bit 21-22 = Timer1 from statu1 (INT 21-22)
i2c0_int	= 0x00000100		@ bit  8 = I2C0 from statu1 (INT 8)
i2c1_int	= 0x20			@ bit  5 = I2C1 from statu2 (INT 37)
uart0_int	= 0x20			@ bit  5 = UART0 from statu1 (INT 5)
uart1_int	= 0x40			@ bit  6 = UART1 from statu1 (INT 6)
int_en_base	= 0xe000e100
int_enabl1	= 0x00
int_enabl2	= 0x04
int_disab1	= 0x80
int_disab2	= 0x84
scheme_ints_en1	= timer0_int + timer1_int + i2c0_int + uart0_int + uart1_int
scheme_ints_en2	= i2c1_int

@ Cortex-M3 SysTick Timer
systick_base	= 0xe000e000
tick_ctrl	= 0x10
tick_load	= 0x14
tick_val	= 0x18

@ gpio
ioporta_base	= 0x40004000		@ I/O Port A base address
ioportb_base	= 0x40005000		@ I/O Port B base address
ioportc_base	= 0x40006000		@ I/O Port C base address
ioportd_base	= 0x40007000		@ I/O Port D base address
ioporte_base	= 0x40024000		@ I/O Port E base address
ioportf_base	= 0x40025000		@ I/O Port F base address
ioportg_base	= 0x40026000		@ I/O Port G base address
ioporth_base	= 0x40027000		@ I/O Port H base address
io_set		= 0x03fc		@ all bits count (read-modify-write mode)
io_clear	= 0x03fc		@ all bits count (read-modify-write mode)
io_state	= 0x03fc		@ all bits count (read-modify-write mode)

@ uarts	
uart0_base	= 0x4000C000		@ UART0
uart1_base	= 0x4000D000		@ UART1
uart_rhr	= 0x00			@ UARTDR
uart_thr	= 0x00			@ UARTDR
uart_status	= 0x18			@ UARTFR
uart_txrdy	= 0x80			@ Tx FIFO Empty
uart_istat	= 0x40			@ UARTMIS <-* intrrpt may need clrng in uart too (at 0x44) uarisr

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
sys_base	= 0x400FE000		@ System Control -- RCC base address (power, clocks and reset)
rcc		= 0x60
rcc2		= 0x70
rcc_base	= 0x400FE000		@ System Control -- RCC base address (power, clocks and reset)

@ flash	
flashcr_base	= 0x400FD000		@ FLASH control registers base

@ usb
usb_base	= 0x00			@ not on-chip, just needed by inisc0
