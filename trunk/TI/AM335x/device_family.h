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
OMAP_35xx	= 1			@ TI OMAP 35xx MCUs (OMAP3530, DM3730)
AM335x		= 1			@ AM335x type (eg. for USB cfg)
include_startup = 1			@ device startup in startup.s file

@ architecture
cortex_a8	= 1
.cpu		cortex-a8
.fpu		neon

@ MMU
MPU_is_in_MMU	= 1			@ MPU functionality is in MMU block
.ifndef TTB_address
  TTB_address	= 0x80010000
.endif

@ code addresses
boot_ram_size		= 0x1b400	@ public ram size for boot, 109KB
_boot_section_address_	= 0x402F0400	@ where startup code will run from
_text_section_address_	= _boot_section_address_ + 0x0400 @ cod strt for cpy2RAM

@ interrupts
irq_direct_branch	= 1		@ branch to genisr diectly on interrupt

num_interrupts	= 128
usb_int_num	= 18			@ USB0, usbint0
uart0_int_num	= 72			@ uart0
uart1_int_num	= 72			@ uart0
timer0_int_num	= 68			@ DMtimer2
timer1_int_num	= 69			@ DMtimer3
i2c0_int_num	= 70			@ i2c0
i2c1_int_num	= 71			@ i2c1
int_base	= 0x48200000		@ interrupt controller base adrs (L4_PER)
int_clear	= 0x48			@ INTCPS_CONTROL
int_clear_vals  = 0x01			@ IRQ_AGR_MASK for INTCPS_CONTROL, NEW IRQ

@ clocks
WKUP_CM_base	= 0x44E00400
PER_CM_base	= 0x44E00000		@ 

@ System Control Module
SCM_base	= 0x44E10000

@ Memory Control

@ gpio
io0_base	= 0x44E07100		@ GPIO0 (L4_WKUP)
io1_base	= 0x4804C100		@ GPIO1 (L4_PER)
io2_base	= 0x481AC100		@ GPIO2 (L4_PER)
io3_base	= 0x481AE100		@ GPIO3 (L4_PER)
io_dir		= 0x34			@ GPIO_OE (1 is input, 0 is output)
io_set		= 0x94			@ SETDATAOUT
io_clear	= 0x90			@ CLEARDATAOUT	
io_state	= 0x38			@ DATAIN
io_out_state	= 0x3c			@ DATAOUT

@ uarts
uart0_base	= 0x44E09000		@ UART0 base address (default) (L4_WKUP)
uart1_base	= uart0_base		@ aliased to uart0
uart_rhr	= 0x00			@ RHR
uart_thr	= 0x00			@ THR
uart_istat	= 0x08			@ IIR
uart_status	= 0x14			@ LSR
uart_txrdy	= 0x20			@ Tx Buffer Empty
uart_ier	= 0x04			@ uart interrupt enable  reg offset
uart_idr	= uart_ier		@ uart interrupt disable reg offset
uart_iRx_ena	= 1			@ uart value to enable  interrupts
uart_iRx_dis	= 0			@ uart value to disable interrupts

@ spi
spi1_base	= 0x48030100		@ MCSPI0
spi2_base	= 0x481A0100		@ MCSPI1 
spi_rhr		= 0x3c			@ channel 0 (SPI0-1 are multi-channel)
spi_thr		= 0x38			@ channel 0 (SPI0-1 are multi-channel)
spi_status	= 0x18			@ channel 0 (SPI0-1 are multi-channel)
spi_rxrdy	= 0x04			@ 
spi_txrdy	= 0x01			@ 

@ timers
@ armpit scheme internal names
timer0_base	= 0X48040000		@ DMTIMER2
timer1_base	= 0X48042000		@ DMTIMER3
@ normal timer assignment -- eg. for user
timer2_base	= 0X48040000		@ DMTIMER2
timer3_base	= 0X48042000		@ DMTIMER3
timer_istat	= 0x28			@ IRQSTATUS
timer_iset	= 0x28			@ IRQSTATUS
@timer_iset	= 0x30			@ IRQENABLE_CLR
timer_ctrl	= 0x38			@ TCLR

@ MMC
mmc0_base	= 0X48060000 + 0x100	@ MMCHS0 registers base address

@ i2c --  partially done
@ used in polling mode, not interrupt, to communicate with TPS65217C
i2c0_base	= 0x44e0b000		@ I2C0
i2c1_base	= i2c0_base		@ I2C0
i2c_sysc	= 0x10			@ _SYSC
i2c_status	= 0x24			@ _IRQSTATUS_RAW
i2c_syss	= 0x90			@ _SYSS
i2c_buf		= 0x94			@ _BUF
i2c_cnt		= 0x98			@ _CNT
i2c_rhr		= 0x9c			@ _DATA
i2c_thr		= 0x9c			@ _DATA
i2c_cset	= 0xa4			@ _CON
i2c_address	= 0xa8			@ own address
i2c_sadr	= 0xac			@ _SA (target slave address)
i2c_psc		= 0xb0			@ _PSC
i2c_scll	= 0xb4			@ _SCLL
i2c_sclh	= 0xb8			@ _SCLH
/* used somewhere?  */
@i2c_data	= 0x00			@ not used
@i2c_cclear	= 0x00			@ 
i2c_irm_rcv	= 0x08			@ RxRdy
i2c_irs_rcv	= 0x08			@ RxRdy
reset_i2c_to_end_transfers = 1		@ special workaround

@ usb --  (resembles S3C24xx, except for byte, half-word access and offsets)
@has_HS_USB      = 1                     @ controller supports High-Speed op
USB_FSHS_MODE   = BUFFER_START + 4 + 0x0C @ recycling I2C0ADR (mcu-id is in i2c)
usb_base	= 0x47401400 		@ USB0 Core base address (Mentor regs)
usb_istat_dv 	= 0x00
usb_istat_dv2 	= 0x0a
usb_iep_mask 	= 0x1f
usb_idv_mask 	= 0x07
usb_busreset 	= 0x04
usb_suspend  	= 0x00
usb_itxendp  	= 0x00  		@ dummy
@ usb_iclear_dv = usb_istat_dv2 @ (bloody thing is read-only! -- not used)
usb_index_reg 	= 0x0e
usb_ctl_stat  	= 0x12
usb_ibulkin	= usb_ctl_stat		@ Bulk IN EP ctrl/stat after EPselect
usb_ibulkout  	= 0x16
usb_txrdy	= 0x01			@ bit 0, for Bulk IN EP
usbCO_ibit	= 0x01			@ 
usbCI_ibit	= 0x02			@ 
usbBO_ibit	= 0x04			@ 
usbBI_ibit	= 0x08			@ 
usbCO_setupbit	= 0x10			@ bit 4 (SETUP_END) in EP0_CSR
usb_rcvd_cnt 	= 0x18
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

.macro	enable_VIC_IRQ
	@ enable interrupts
	@ nothing to do on this MCU
	swi	run_normal		@ set Thread mode, unpriv, IRQ (user)
.endm

.macro	enterisr
	@ enterisr for non-cortex-m3
	sub	lnk, lnk, #4		@ Adjust lnk to point to return 
	stmdb	sp!, {lnk}		@ store lnk_irq (pc_usr) on irq stack
	mrs	lnk, spsr		@ lnk  <- spsr
	tst	lnk, #IRQ_disable	@ were interrupts disabled?
	ldmiane	sp!, {pc}^		@ If so, just return immediately
	@ save some registers on stack
	stmib	sp,  {lnk}		@ store spsr on irq stack (above pc_usr)
	stmdb	sp,  {fre, cnt, rva, rvb, rvc, lnk}^ @ 5 regs+lnk_usr on stack
	sub	sp,  sp, #24		@ sp  <- adjstd stck ptr (nxt free cell)
	set	rvb, int_base		@ rvb <- address of VIC IRQ Status reg
	@ OMAP_35xx
	read	rvb, rvb, #0x40		@ rvb <- asserted int, INTCPS_SIR_IRQ
	and	rvb, rvb, #0x7f
.endm

.macro	clearUartInt
	@ clear interrupt in uart with base address in rva
	read	cnt, rva, #uart_istat	@ cnt <- interrupt status (clears UART)
.endm

.macro	clearTimerInt
	@ clear interrupt in timer peripheral block with base address in rva
	read	rvc, rva, #timer_istat	@ at91sam7
	write	rvc, rva, #timer_iset	@ lpc2000
	set	rvc, 0			@ rvc <- 0
	write	rvc, rva, #timer_iset	@ str711, STM32
.endm

.macro	clearVicInt
	@ clear interrupt in interrupt vector (if needed)
	@ modifies:	rva, rvc
	set	rvc, int_clear_vals
	write	rvc, int_base, #int_clear
	dsb				@ data sync barrier (ensure int clr)
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

.macro	usbldr dest, src, ofst
	read16	\dest, \src, #\ofst
.endm

.macro	usbstr dest, src, ofst
	write16	\dest, \src, #\ofst
.endm

.macro	usbstrne dest, src, ofst
	write16ne \dest, \src, #\ofst
.endm



