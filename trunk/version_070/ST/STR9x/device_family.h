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
STR_9xx 	= 1			@ ST STR9xx family MCU

@ architecture
.cpu		arm9tdmi

@ writing to on-chip flash (done from RAM, reserve space for flsRAM of initio.s)
EXTRA_FILE_RAM	= 0x90

@ type of gpio set/clear
has_combined_set_clear	= 1		@ MCU has combined GPIO SET/CLEAR reg

@ interrupts
irq_direct_branch	= 1		@ branch to genisr on int (vs VIC)
num_interrupts	= 32
uart0_int_num	= 16
uart1_int_num	= 17
timer0_int_num	=  4
timer1_int_num	=  5
i2c0_int_num	= 19			@ 
i2c1_int_num	= 20			@ 
usb_int_num     =  9			@ USB Low Pri
int_voffset	= -0x0ff0
int_base	= 0xFFFFF000		@ VIC 0
int_base2	= 0xFC000000		@ VIC 1
int_status	= 0x00			@ 
int_enable	= 0x10			@ 
uart0_int	= 1 << (uart0_int_num - 16)	 
uart1_int	= 1 << (uart1_int_num - 16)	 
timer0_int	= 1 << timer0_int_num		 
timer1_int	= 1 << timer1_int_num		 
i2c0_int	= 1 << (i2c0_int_num - 16)	 
i2c1_int	= 1 << (i2c1_int_num - 16)	 
usb_int		= 1 << usb_int_num		 
scheme_ints_enb	= timer0_int | timer1_int | usb_int
scheme_ints_en2	= i2c0_int | i2c1_int | uart0_int | uart1_int

@ gpio -- 
ioport0_base	= 0x58006000
ioport1_base	= 0x58007000
ioport2_base	= 0x58008000
ioport3_base	= 0x58009000
ioport4_base	= 0x5800A000
ioport5_base	= 0x5800B000
ioport6_base	= 0x5800C000
ioport7_base	= 0x5800D000
ioport8_base	= 0x5800E000
ioport9_base	= 0x5800F000
io_set		= 0x03FC
io_clear	= 0x03FC
io_state	= 0x03FC
io_dir		= 0x0400

@ uarts -- 
uart0_base	= 0x5C004000		@ uart0
uart1_base	= 0x5C005000		@ uart1
uart_rhr	= 0x00			@ offset to uart rhr register
uart_thr	= 0x00			@ offset to uart thr register
uart_istat	= 0x40
uart_status	= 0x18			@ offset to uart status register
uart_txrdy	= 0x80			@ bit indicating uart THR empty
uart_iclear	= 0x44
@uart_ienab	= 0x38
@uart_iRx_ena	= 0x10
@uart_iRx_dis	= 0x00
uart_ier	= 0x38			@ interrupt enable  reg offset
uart_idr	= uart_ier		@ interrupt disable reg offset
uart_iRx_ena	= 0x10			@ value to enable  interrupts
uart_iRx_dis	= 0			@ value to disable interrupts

@ timers -- 
timer0_base	= 0X58002000		@ TIM0_ICAR
timer1_base	= 0X58003000		@ TIM1_ICAR
timer2_base	= 0X58004000		@ TIM2_ICAR
timer3_base	= 0X58005000		@ TIM3_ICAR
timer_istat	= 0x1C			@ TIMn_SR
timer_iset	= 0x1c
timer_ctrl	= 0x14			@ TIMn_CR1

@ rtc
rtc0_base	= 0x5C001000		@ rtc

@ spi
spi0_base	= 0x5C007000		@ ssp0
spi1_base	= 0x5C008000		@ ssp1
spi_rhr		= 0x08
spi_thr		= 0x08
spi_status	= 0x0c
spi_rxrdy	= 0x04
spi_txrdy	= 0x01

@ adc
adc0_base	= 0x5C00a000		@ adc

@ i2c -- 
i2c0_base	= 0x5C00C000		@ I2C0
i2c1_base	= 0x5C00D000		@ I2C1
i2c_cr		= 0x00			@ I2Cn_CR
i2c_stat1	= 0x04			@ I2Cn_SR1
i2c_stat2	= 0x08			@ I2Cn_SR2
i2c_address	= 0x10			@ I2Cn_OAR1
i2c_rhr		= 0x18			@ I2Cn_DR
i2c_thr		= 0x18			@ I2Cn_DR
i2c_irm_rcv	= 0x9A			@ EV7	- SR1, EVF BSY BTF MSL, #0x9A
i2c_irs_rcv	= 0x98			@ EV2	- SR1, EVF BSY BTF, #0x98

@ usb -- 
usb_hw_buffer	= 0x70000000		@ USB RAM
usb_cntreg_32b	= 1			@ USB count reg needs to presrv upr bits
usb_base	= 0x70000800		@ USB Registers (after USB RAM)
usb_istat_dv	= 0x44			@ USB_ISTR
usb_daddr	= 0x4C
usb_iep_mask	= 0x8000		@ EndPoint Interrupts mask
usb_idv_mask	= 0x1C00		@ Dev int mask -- WakeUp, Suspend, Reset
usb_busreset	= 0x0400		@ 
usb_suspend	= 0x1000		@ (used to wake up device)
usb_txrdy	= 0x10			@ EP NAKing or disabled (rdy for new Tx)
usb_ibulkin	= 0x0C			@ offset of USB_EP3R
usb_itxendp	= 0xFF00		@ every device int cleared at usbEPx
usb_iclear_dv	= 0x44			@ USB_ISTR
usbCO_setupbit	= 0x0800		@ EP stat bit if last tfer was SETUP
UsbControlOutEP	= 0x00			@ Control OUT Endpoint
UsbControlInEP	= 0x00			@ Control IN  Endpoint (same as ctl out)
UsbBulkOutEP	= 0x02			@ Bulk OUT EP
UsbBulkInEP	= 0x03			@ Bulk IN  EP 3
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for descriptor)
usbCO_ibit	= 0x010000		@ bit indicating int for Control OUT EP
usbCI_ibit	= 0x020000		@ bit indicating int for Control IN  EP
usbBO_ibit	= 0x100000		@ bit indicating int for Bulk    OUT EP
usbBI_ibit	= 0x800000		@ bit indicating int for Bulk    IN  EP

sys_ctrl	= 0x5C002000		@ SCU base

/*----------------------------------------------------------------------------*\
|										|
|			2. Device Family Macros					|
|										|
\*----------------------------------------------------------------------------*/

.macro	enable_VIC_IRQ
	@ enable interrupts
	write	scheme_ints_enb, int_base, #int_enable	@ enable scheme ints
	@ STR_9xx	@ enable VIC1 ints (in addition to VIC0 enabled above)
	write	scheme_ints_en2, int_base2, #int_enable	@ enable ints (16-31)
.endm

.macro	enterisr
	@ interrupt service routine entry
	@ on exit:	rvb <- interrupt number (of interrupt to process)
	@ on exit:	sp  <- fre, cnt, rva, rvb, rvc, lnk_usr, pc_usr, spsr
	sub	lnk, lnk, #4		@ Adjust lnk to point to return 
	stmdb	sp!, {lnk}		@ store lnk_irq (pc_usr) on irq stack
	mrs	lnk, spsr		@ lnk  <- spsr
	tst	lnk, #IRQ_disable	@ were interrupts disabled?
	ldmiane	sp!, {pc}^		@ If so, just return immediately
	@ save some registers on stack
	stmib	sp,  {lnk}		@ str spsr on irq stk (above lnk_irq)
	stmdb	sp,  {fre, cnt, rva, rvb, rvc, lnk}^	@ str 5 regs on irq stk
	sub	sp,  sp, #24		@ sp  <- adj. stk ptr (next free cell)
	set	rvb, int_base		@ rvb <- address of VIC IRQ Status reg
	@  STR_9xx dual 16-int VICs
	read	rvc, rvb, #int_status	@ rvc <- asserted interrupts ( 0-15)
	set	rvb, 0
	eq	rvc, #0
	seteq	rvb, int_base2
	readeq	rvc, rvb, #int_status	@ rvc <- asserted interrupts (33-64)
	seteq	rvb, 16
	tst	rvc, #0xff		@ is int in 0-7 or 16-23?
	addeq	rvb, rvb, #8		@	if not, rvb <- int num + 8
	lsreq	rvc, rvc, #8		@	if not, rvc <- ints, shifted
	tst	rvc, #0x0f		@ is int in 0-3, 8-11, 16-19 or 24-27?
	addeq	rvb, rvb, #4		@	if not, rvb <- int num + 4
	lsreq	rvc, rvc, #4		@	if not, rvc <- ints, shifted
	tst	rvc, #0x03		@ is int in 0-1,4-5,8-9,12-13,...,28-29?
	addeq	rvb, rvb, #2		@	if not, rvb <- int num + 2
	lsreq	rvc, rvc, #2		@	if not, rvc <- ints, shifted
	tst	rvc, #0x01		@ is int even?
	addeq	rvb, rvb, #1		@	if not, rvb <- updated int num
.endm

.macro	clearUartInt
	@ clear interrupt in uart with base address in rva
	read	cnt, rva, #uart_istat	@ cnt <- int stat (clr UART int/LPC2000)
	write	cnt, rva, #uart_iclear
.endm

.macro	clearTimerInt
	@ clear interrupt in timer peripheral block with base address in rva
	set	rvc, 0			@ rvc <- 0
	write	rvc, rva, #timer_ctrl	@ stop timer
	write	rvc, rva, #0x10		@ reset count
	write	rvc, rva, #timer_iset	@ clear flags
.endm

.macro	clearVicInt
	@ clear interrupt in interrupt vector (if needed)
	@ modifies:	rva, rvc
.endm
	
.macro	exitisr
	@ exitisr for non-cortex
	ldr	rvc, [sp,  #28]
	msr	spsr_cxsf, rvc		@ restore spsr
	ldmia	sp, {fre, cnt, rva, rvb, rvc, lnk}^	@ Restore registers
	add	sp, sp, #24
	ldmia	sp!, {lnk}
	movs	pc,  lnk		@ Return
.endm

.macro	isrexit
	@ second version - different from exitisr for STR7 and AT91SAM7 only
	exitisr
.endm



