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

@ type of gpio set/clear
has_combined_set_clear	= 1		@ MCU has combined GPIO SET and CLEAR register

@ interrupts
irq_direct_branch	= 1		@ branch to genisr diectly on interrupt (instead of VIC)
num_interrupts	= 32
uart0_int_num	= 28			@ uart0
uart1_int_num	= 15			@ uart2
timer0_int_num	= 10			@ timer0
timer1_int_num	= 11			@ timer1
i2c0_int_num	= 27			@ IIC
i2c1_int_num	= 27			@ IIC (aliased, only one channel)
usb_int_num	= 25			@ USB Device
int_base	= 0x4a000000		@ interrupt controller base address
int_status	= 0x10			@ INTPND
int_clear	= 0x00			@ SRCPND
int_enable	= 0x08			@ 
uart0_int	= 0x10000000		@ bit 28 = UART0 general
uart1_int	= 0x00008000		@ bit 15 = UART2 general
timer0_int	= 0x00000400		@ bit 10 = Timer 0
timer1_int	= 0x00000800		@ bit 11 = Timer 1
i2c0_int	= 0x08000000		@ bit 27 -- I2C		<- not enabled in this version
i2c1_int	= i2c0_int		@ bit 27 -- aliased
usb_int 	= 0x02000000		@ bit 25 -- USB device
scheme_ints_enb	= uart0_int | timer0_int | timer1_int | usb_int

@ gpio
ioA_base	= 0x56000000		@ GPFCON -- port A
ioB_base	= 0x56000010		@ GPGCON -- port B
ioC_base	= 0x56000020		@ GPHCON -- port C
ioD_base	= 0x56000030		@ GPGCON -- port D
ioE_base	= 0x56000040		@ GPHCON -- port E
ioF_base	= 0x56000050		@ GPFCON -- port F
ioG_base	= 0x56000060		@ GPGCON -- port G
ioH_base	= 0x56000070		@ GPHCON -- port H
io0_base	= ioF_base		@ GPFCON -- port F -- LED port
io1_base	= ioG_base		@ GPGCON -- port G -- boot inhibit -- used in FlashInitCheck
io_dir		= 0x00			@ GPxCON
io_set		= 0x04			@ GPxDAT
io_clear	= 0x04			@ GPxDAT	
io_state	= 0x04			@ GPxDAT	

@ uarts (page 312+)
uart0_base	= 0x50000000		@ UART0
uart1_base	= 0x50008000		@ UART2 (is brought out to board pins)
uart_rhr	= 0x24			@ URXHn
uart_thr	= 0x20			@ UTXHn
uart_istat	= 0x14			@ UERSTATn (uart error -- used as dummy)
uart_status	= 0x10			@ UTRSTATn
uart_txrdy	= 0x02			@ Tx Buffer Empty	

@ spi
spi0_base	= 0x59000000
spi1_base	= 0x59000020
spi_rhr		= 0x14
spi_thr		= 0x10
spi_status	= 0x04
spi_rxrdy	= 0x01
spi_txrdy	= 0x01

@ rtc
rtc0_base	= 0x57000040

@ adc
adc0_base	= 0x58000000

@ timers
timer0_base	= 0X51000000		@ TCFG0
timer1_base	= 0X51000000		@ TCFG0
timer_istat	= 0x0C			@ ?
timer_iset	= 0x0C			@ ?
timer_ctrl	= 0x08			@ TCON

@ i2c --  NOT DONE !!!!!!
i2c0_base	= 0x54000000		@ IICCON
i2c1_base	= 0x54000000		@ IICCON (one channel only)
i2c_cset	= 0x00			@ IICCON
i2c_status	= 0x04			@ IICSTAT
i2c_rhr		= 0x0c			@ IICDS
i2c_thr		= 0x0c			@ IICDS
i2c_data	= 0x0c			@ IICDS
i2c_address	= 0x08			@ IICADR
i2c_cclear	= 0x18			@ ?
i2c_irm_rcv	= 0x50			@ ?
i2c_irs_rcv	= 0x80			@ ?

@ usb
usb_base	= 0x52000140		@ NOT DONE !!!!!
usb_daddr	= 0x00			@ FUNC_ADDR_REG
usb_istat_dv	= 0x08			@ EP_INT_REG
usb_iep_mask	= 0x1f			@ 
usb_istat_ep	= 0x08			@ EP_INT_REG
usb_istat_dv2	= 0x18			@ USB_INT_REG
usb_idv_mask	= 0x07			@ 
usb_busreset	= 0x04			@ 
usb_suspend	= 0x01			@ 
usbCO_ibit	= 0x01			@ 
usbCI_ibit	= 0x02			@ 
usbBO_ibit	= 0x04			@ 
usbBI_ibit	= 0x08			@ 
usb_index_reg	= 0x38			@ INDEX_REG
usb_ctl_stat	= 0x44			@ EP0_CSR
usbCO_setupbit	= 0x10			@ bit 4 (SETUP_END) in EP0_CSR
usb_rcvd_cnt	= 0x58			@ OUT_FIFO_CNT1_REG
usb_itxendp	= 0x00			@ 
usb_iclear_dv	= 0x18			@ USB_INT_REG
usb_iclear_ep	= 0x08			@ EP_INT_REG
UsbControlOutEP	= 0			@ 
UsbControlInEP	= 0			@ 
UsbBulkOutEP	= 2			@ 
UsbBulkInEP	= 3			@ 
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for descriptor at end of file)

@ power and clocks
sys_ctrl	= 0x4c000000
