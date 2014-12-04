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

/* architecture */
cortex		= 1
.cpu		cortex-m4
.fpu		fpv4-sp-d16

/* writing to on-chip flash (done from RAM, reserve space for flsRAM) */
EXTRA_FILE_RAM	= 0x60

/* interrupts */
num_interrupts	= 86
uart0_int_num	= 31
uart1_int_num	= 33
timer0_int_num	= 42
timer1_int_num	= 43
i2c0_int_num	= 24
i2c1_int_num	= 25
usb_int_num	= 53

timer0_int	= 1 << (timer0_int_num % 32)
timer1_int	= 1 << (timer1_int_num % 32)
i2c0_int	= 1 << (i2c0_int_num   % 32)
i2c1_int	= 1 << (i2c1_int_num   % 32)
uart0_int	= 1 << (uart0_int_num  % 32)
uart1_int	= 1 << (uart1_int_num  % 32)
usb_int		= 1 << (usb_int_num    % 32)

int_status	= int_stat_32__63	@ where to find the timer interrupts
scminten__0__31	= uart0_int | i2c0_int   | i2c1_int  
scminten_32__63	= uart1_int | timer0_int | timer1_int | usb_int
scminten_64__95	= 0

/* gpio */
ioporta_pcr	= 0x40049000		@ I/O Port A base address PCR
ioportb_pcr	= 0x4004a000		@ I/O Port B base address PCR
ioportc_pcr	= 0x4004b000		@ I/O Port C base address PCR
ioportd_pcr	= 0x4004c000		@ I/O Port D base address PCR
ioporte_pcr	= 0x4004d000		@ I/O Port E base address PCR
ioporta_base	= 0x400ff000		@ I/O Port A base address PDOR
ioportb_base	= 0x400ff040		@ I/O Port B base address PDOR
ioportc_base	= 0x400ff080		@ I/O Port C base address PDOR
ioportd_base	= 0x400ff0c0		@ I/O Port D base address PDOR
ioporte_base	= 0x400ff100		@ I/O Port E base address PDOR
io_set		= 0x04			@ PSOR
io_clear	= 0x08			@ PCOR
io_state	= 0x10			@ PDIR
io_dir		= 0x14			@ PDDIR

/* uarts */
uart0_base	= 0x4006a000		@ UART 0
uart1_base	= 0x4006b000		@ UART 1
uart_byte_only	= 1			@ uart register access is bytewise only
uart_istat_1st	= 1			@ read istat (S1) reg before data byte
uart_rhr	= 0x07			@ UARTn_D -- byte access only
uart_thr	= 0x07			@ UARTn_D -- byte access only
uart_status	= 0x04			@ UARTn_S1
uart_txrdy	= 0x80			@ TDRE -- Tx Data Reg Empty Flag
uart_istat	= 0x04			@ UARTn_S1
uart_ier	= 0x03			@ uart interrupt enable  reg offset
uart_idr	= 0x03			@ uart interrupt disable reg offset
uart_iRx_ena	= 0x2c			@ uart value to enable  interrupts
uart_iRx_dis	= 0x0c			@ uart value to disable interrupts

/* spi */
spi0_base	= 0x4002c000		@ SPI 0
spi1_base	= 0x4002d000		@ SPI 1
spi_rhr		= 0x38			@ SPIx_POPR
spi_thr		= 0x34			@ SPIx_PUSHR
spi_status	= 0x2c			@ SPIx_SR
spi_rxrdy	= 1<<17			@ Rx FIFO not empty, RFDF bit
spi_txrdy	= 1<<25			@ Tx FIFO not full,  TFFF bit

/* SD/MMC -- similar IP to TI OMAP */
sdhc0_base	= 0x400b1000		@ eSDHC
mmc_arg		= 0x08			@ SDHC_CMDARG
mmc_cmd		= 0x0c			@ SDHC_XFERTYP

/* adc */
adc0_base	= 0x4003b000		@ ADC 0
adc1_base	= 0x400bb000		@ ADC 1

/* i2c -- mostly NOT DONE */
i2c0_base	= 0x40066000		@ I2C0 Master
i2c1_base	= 0x40067000		@ I2C1 Master
i2c_address	= 0x00			@ I2Cn_A1
i2c_rhr		= 0
i2c_thr		= 0
i2c_irm_rcv	= 0
i2c_irs_rcv	= 0

/* timers */
timer0_base	= 0x40038000		@ Flex TIMER 0
timer1_base	= 0x40039000		@ Flex TIMER 1
timer2_base	= 0x4003a000		@ Flex TIMER 2
@ *** TODO 
@timer_ctrl	= 0x0C			@ GPTMCTL, GPTM Control
timer_istat	= 0x00			@ FTMn_SC (global, not channel based)
timer_iset	= 0x00			@ FTMn_SC (global, not channel based)

/* usb -- similar IP to MicroChip */
usb_base	= 0x40072000		@ base address - USB OTG FS/LS
usb_byte_only	= 1			@ usb register access is bytewise only
BDT_address	= RAMTOP		@ buffer descriptor address (queues)
usb_istat_dv	= 0x80			@ USB0_ISTAT
usb_iep_mask	= 0x08			@ Token Done bit (bit 3)
usb_idv_mask	= 0x01			@ Reset bit (bit 0)
usb_busreset	= 0x01			@ Reset bit (bit 0)
usb_itxendp	= 0x00			@ no TxPcktEnd bit to clear
usb_iclear_dv	= 0x80			@ USB0_ISTAT (wrt 1 to clr reset  int)
usb_iclear_dvep	= 0x80			@ USB0_ISTAT (wrt 1 to clr tokdne int)
usbCO_ibit	= 1<<0			@ EP 0 OUT bit for dispatch
usbCI_ibit	= (1<<0)<<8		@ EP 0 IN  bit for dispatch
usbBO_ibit	= 1<<2			@ EP 2 OUT bit for dispatch
usbBI_ibit	= (1<<3)<<8		@ EP 3 IN  bit for dispatch
UsbControlOutEP	= 0			@ 
UsbControlInEP	= 0			@ 
UsbBulkOutEP	= 2			@ 
UsbBulkInEP	= 3			@ 
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for desc at end file)
usbCO_setupbit	= 1<<5			@ SETUP bit in USB0_CTL
usb_daddr 	= 0x98			@ USB0_ADDR

/* power */
sim_base	= 0x40047000		@ System Integration Module - SIM
smc_base	= 0x4007e000		@ System Mode Control       - SMC
pmc_base	= 0x4007d000		@ Power Mangement Control   - PMC
rcm_base	= 0x4007f000		@ Reset Control Module      - RCM
mcg_base	= 0x40064000		@ Multi-purpose clock generator - MCG
osc_base	= 0x40065000		@ System Oscillator - OSC
scgc_base	= 0x40048028		@ SCGC1 (add 4, 8 ... for SCGC2, 3 ...)

/* flash */
flash_ctl_base	= 0x4001f000		@ FLASH memory controller base
flashcr_base	= 0x40020000		@ FLASH memory base (for write/erase)


/*----------------------------------------------------------------------------*\
|										|
|			2. Device Family Macros					|
|										|
\*----------------------------------------------------------------------------*/

.macro	clearUartInt	
	@ clear interrupt in uart with base address in rva
	@ nothing to do on this MCU
	@ (treated with read_istat_1st flag)
.endm

.macro	clearTimerInt	
	@ clear interrupt in timer peripheral block with base address in rva
	read	rvc, rva, #timer_istat	@ at91sam7
	set	rvc, 0
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






