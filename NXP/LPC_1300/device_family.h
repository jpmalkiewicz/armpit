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
LPC_13xx	= 1			@ NXP LPC13xx family MCU

@ architecture
cortex		= 1
.cpu		cortex-m3

@ type of gpio set/clear
has_combined_set_clear	= 1		@ MCU has combined GPIO SET / CLEAR reg

@ interrupts
num_interrupts	= 64
uart0_int_num	= 46
uart1_int_num	= uart0_int_num
timer0_int_num	= 41			@ 
timer1_int_num	= 42			@ 
i2c0_int_num	= 40
i2c1_int_num	= i2c0_int_num
usb_int_num	= 47			@ also: 48 = usb FRQ (not treated)

uart0_int	= 1 << (uart0_int_num  % 32)
uart1_int	= 1 << (uart1_int_num  % 32)
timer0_int	= 1 << (timer0_int_num % 32)
timer1_int	= 1 << (timer1_int_num % 32)
i2c0_int	= 1 << (i2c0_int_num   % 32)
i2c1_int	= 1 << (i2c1_int_num   % 32)
usb_int		= 1 << (usb_int_num    % 32)

int_status	= int_stat_32__63	@ where to find the timer interrupts

scminten__0__31 = 0
scminten_32__63 = timer0_int|timer1_int|i2c0_int|i2c1_int|uart0_int|uart1_int|usb_int

@ gpio
io0_base 	= 0x50000000 		@ gpio0
io1_base 	= 0x50010000 		@ gpio1
io2_base 	= 0x50020000 		@ gpio2
io3_base 	= 0x50030000 		@ gpio3
io_set		= 0x0ffc		@ IODATA bits 0-9 (bit 10-11 not cnsdrd)
io_clear	= 0x0ffc		@ IODATA
io_state	= 0x0ffc		@ IODATA
io_dir		= 0x8000		@ dir for LED pins (may not be general)
iocon_pio	= 0x40044000

@ uarts	
uart0_base	= 0x40008000		@ UART0
uart1_base	= uart0_base		@ UART1 = UART0
uart_rhr	= 0x00			@ RBR
uart_thr	= 0x00			@ THR
uart_ier	= 0x04			@ IER
uart_istat	= 0x08
uart_status	= 0x14			@ offset to uart status register
uart_txrdy	= 0x20			@ bit indicating uart THR empty
uart_idr	= uart_ier		@ interrupt disable reg offset
uart_iRx_ena	= 1			@ value to enable  interrupts
uart_iRx_dis	= 0			@ value to disable interrupts

@ adc
adc0_base	= 0x4001c000		@ ADC

@ spi
spi0_base	= 0x40040000		@ SSP

@ i2c -- NOT DONE !!!	
i2c_fastmodplus	= 0x4002C07C  @ I2C fast mode plus config register
i2c0_base	= 0x40000000		@ I2C0
i2c1_base	= i2c0_base		@ I2C1 = I2C0
i2c_cset	= 0x00
i2c_status	= 0x04
i2c_rhr		= 0x08
i2c_thr		= 0x08
i2c_data	= 0x08
i2c_address	= 0x0C
i2c_cclear	= 0x18
i2c_irm_rcv	= 0x50			@ ok on cortex? (this is from LPC2000)
i2c_irs_rcv	= 0x80			@ ok on cortex? (this is from LPC2000)

@ timers
timer0_base	= 0x4000C000		@ TIMER 0, 16-bits: CT16B0
timer1_base	= 0x40010000		@ TIMER 1, 16-bits: CT16B1
timer_istat	= 0x00			@ 
timer_iset	= 0x00			@ 
timer_ctrl	= 0x04			@ 
timer2_base	= 0x40014000		@ TIMER 0, 32-bits: CT32B0
timer3_base	= 0x40018000		@ TIMER 1, 32-bits: CT32B1

@ usb
usb_base	= 0x40020000	       	@ USB base
usb_istat_dv	= 0x0000		@ offset to USBDevIntSt
usb_iep_mask	= 0x01fe		@ mask in USBDevIntSt for EP int EP0-7
usb_idv_mask	= 0x0200	       	@ mask in USBDevIntSt for dev stat int
usb_itxendp	= 0x2000	       	@ mask in USBDevIntSt for Tx end pkt int
usb_icd_full	= 0x0800	       	@ mask in USBDevIntSt for CD_FULL
usb_icc_empty	= 0x0400	       	@ mask in USBDevIntSt for CC_EMPTY
usb_iclear_dv	= 0x08			@ offset to USBDevIntClr
usb_iset_dv	= 0x0c			@ offset to USBDevIntSet
usb_cmd_code	= 0x10			@ offset to USBCmdCode  -- USB Cmnd Code
usb_cmd_data	= 0x14			@ offset to USBCmdData  -- USB Cmnd Data
usb_busreset	= 0x10			@ bus reset bit from cmd 0xFE0500 FE0200
usb_suspend	= 0x08			@ suspend bit   from cmd 0xFE0500 FE0200
usb_rxdata	= 0x18			@ USBRxData   -- USB Receive Data
usb_txdata	= 0x1C			@ USBTxData   -- USB Transmit Data
usb_rxplen	= 0x20			@ USBRxPLen   -- USB Receive Packet Len
usb_txplen	= 0x24			@ USBTxPLen   -- USB Transmit Packet Len
usb_ctrl	= 0x28			@ USBCtrl     -- USB Control
UsbControlOutEP	= 0x00			@ Control IN Endpoint (phys 0, log 0)
UsbControlInEP	= 0x80			@ Control IN Endpoint (phys 1, log 0)
UsbBulkOutEP	= 0x02			@ Bulk OUT EP (phys = 4, log = 2)
UsbBulkInEP	= 0x82			@ Bulk IN  EP (phys = 5, log = 2)
usbBulkINDescr	= 0x82			@ Bulk IN is EP 2 (for desc at end file)
usbCO_ibit	= 0x02			@ bit indic int for Control OUT Endpoint
usbCI_ibit	= 0x04			@ bit indic int for Control IN  Endpoint
usbBO_ibit	= 0x20			@ bit indic int for Bulk    OUT Endpoint
usbBI_ibit	= 0x40			@ bit indic int for Bulk    IN  Endpoint
usbCO_setupbit	= 0x04			@ bit indic last tfer was SETUP packet
usb_ibulkin	= 0x00			@ status of Bulk IN EP = USBDevIntSt
usb_txrdy	= 0x2000	       	@ Txrdy bit in usb_iBulk_IN=TxPacket_end

@ power
sys_ctrl       	= 0x40048000		@ SCS base
PLOCKbit	= 0			@ bit 0 = PLL LOCK bit in PLLSTAT

@ flash
flash_tim	= 0x4003C010
IAP_ENTRY	= 0x1FFF1FF1		@ IAP routine entry point in boot sector


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
	set	rvc, 0
	write	rvc, rva, #timer_iset	@ str711, STM32
.endm



