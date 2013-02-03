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

@ interrupts
irq_direct_branch	= 1		@ branch to genisr diectly on interrupt (instead of VIC)
num_interrupts	= 32	
uart0_int_num	= 12
uart1_int_num	= 12
timer0_int_num	= 5
timer1_int_num	= 6
i2c0_int_num	= 13
i2c1_int_num	= 13
int_base	= 0x80300000		@ INT_PRIOMASK0
int_status	= 0x0200		@ INT_PENDING
int_clear_vals	= 0x010000		@ base address of IRQ IVT, 64kb into SDRAM
int_clear	= 0x0100		@ INT_VECTOR0
uart0_int	= 0x1000		@ bit 12
uart1_int	= 0x0000		@ NA
timer0_int	= 0x0020		@ bit 5
timer1_int	= 0x0040		@ bit 6
i2c0_int	= 0x2000		@ bit 13
i2c1_int	= 0x0000		@ NA
scheme_ints_enb	= timer0_int + timer1_int + i2c0_int + i2c1_int
int_enable	= 0x00			@ offset to nothing (see interaction of lpc28xx_encons and cons)
int_disable	= 0x00			@ offset to nothing (unused)

@ gpio
io0_base	= 0x80003000		@ PINS_0
io1_base	= 0x80003040		@ PINS_1
io2_base	= 0x80003080		@ PINS_2
io3_base	= 0x800030c0		@ PINS_3
io4_base	= 0x80003100		@ PINS_4
io5_base	= 0x80003140		@ PINS_5
io6_base	= 0x80003180		@ PINS_6
io7_base	= 0x800031c0		@ PINS_7
io_set		= 0x14			@ Mode0S_n
io_clear	= 0x18			@ Mode0C_n
io_state	= 0x00			@ PINS_n

@ uarts
uart0_base	= 0x80101000
uart1_base	= 0x80101000
uart_thr	= 0x00
uart_rhr	= 0x00
uart_ier	= 0x04			@ IER
uart_istat	= 0x08			@ IIR
uart_status	= 0x14			@ LSR
uart_txrdy	= 0x20			@ bit 5 of LSR == THRE (empty THR)	

@ rtc
rtc0_base	= 0X80002000		@ rtc

@ mci
mci_base	= 0X80100000		@ mci

@ lcd
lcd_base	= 0X80103000		@ lcd

@ adc
adc0_base	= 0X80002400		@ adc

@ dma
gdma_base	= 0X80103800

@ timers
timer0_base	= 0X80020000		@ T0Load
timer1_base	= 0X80020400		@ T1Load
timer_istat	= 0x0C			@ TnClear
timer_iset	= 0x0C			@ TnClear
timer_ctrl	= 0x08			@ TnControl

@ i2c  -- NOT COMPLETE
i2c0_base	= 0x80020800		@ I2RX/I2TX
i2c1_base	= i2c0_base
i2c_rhr		= 0x00
i2c_thr		= 0x00
i2c_data	= 0x00
i2c_address	= 0x14			@ offset to I2ADR
i2c_status	= 0x04
i2c_cset	= 0x00
i2c_cclear	= 0x18
i2c_irm_rcv	= 0x50
i2c_irs_rcv	= 0x80

@ usb
has_HS_USB      = 1                     @ this controller supports High-Speed operation
USB_FSHS_MODE   = 0x80041078            @ USBScratch used to store HS vs FS state (16 low bits)
usb_clken	= 0x80005050		@ USBClckEN
usb_base	= 0x80041000		@ USBDevAdr
usb_dev_adr	= 0x00			@ USBDevAdr
usb_istat_dv	= 0x94			@ USBIntStat
usb_iclear_dv	= 0xAC			@ USBIntClr
usb_istat_ep	= 0x98			@ USBEIntStat
usb_iclear_ep	= 0xA0			@ USBEIntClr
usb_iep_mask	= 0x80			@ mask for endpoint interrupt EP0SETUP
usb_busreset	= 0x01			@ bus reset bit
usb_suspend	= 0x38			@ suspend/resume, change to HS bits
usb_idv_mask	= 0x39			@ mask for device status interrupt
usb_maxpsize	= 0x04			@ USBMaxPSize -- USB MaxPacketSize
usb_txplen	= 0x1C			@ USBDCnt     -- USB Transmit Packet Length
usb_rxplen	= 0x1C			@ USBDCnt     -- USB Receive Packet Length
usb_rxdata	= 0x20			@ USBData     -- USB Receive Data
usb_txdata	= 0x20			@ USBData     -- USB Transmit Data
usbCO_ibit	= 0x01			@ bit indicating interrupt for Control OUT Endpoint
usbCI_ibit	= 0x02			@ bit indicating interrupt for Control IN  Endpoint
usbBO_ibit	= 0x10			@ bit indicating interrupt for Bulk    OUT Endpoint
usbBI_ibit	= 0x20			@ bit indicating interrupt for Bulk    IN  Endpoint
usb_epind	= 0x2C			@ USBEIDX    -- USB Endpoint Index
usb_reep	= 0x08			@ USBEType   -- USB Realize Endpoint / enpoint type
UsbControlOutEP	= 0x00			@ Control OUT Endpoint (phys 0, log 0)
UsbControlInEP	= 0x01			@ Control IN Endpoint (phys 1, log 0)
UsbBulkOutEP	= 0x04			@ Bulk OUT EP (phys = 4, log = 2)
UsbBulkInEP	= 0x05			@ Bulk IN  EP (phys = 5, log = 2)
usbBulkINDescr	= 0x82			@ Bulk IN is EP 2 (for descriptor at end of file)
usb_itxendp	= 0x00			@ Tx end of packet interrupt bit -- not used on this device
usbCO_setupbit	= 0x00			@ EP status bit indicating last transfer was a SETUP packet - not used
usb_ctrl	= 0x28			@ USBECtrl    -- USB Endpoint Control
usb_txrdy	= 0x20			@ Tx ready bit in usb_iBulk_IN

@ system control -- few power offsets, eg. USBClkEN (see above)
sys_ctrl	= 0x80005000
