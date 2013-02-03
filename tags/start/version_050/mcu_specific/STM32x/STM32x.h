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

@ interrupts
.ifndef	connectivity_ln
  num_interrupts  = 64
.else
  num_interrupts  = 68
.endif

.ifndef swap_default_usart
  uart0_int_num	= 37			@ USART1
  uart1_int_num	= 38			@ USART2
.else
  uart0_int_num	= 38			@ USART2
  uart1_int_num	= 37			@ USART1
.endif

timer0_int_num	= 28			@ Timer 2
timer1_int_num	= 29			@ Timer 3
i2c0_int_num	= 31
i2c1_int_num	= 33
.ifndef	connectivity_ln
  usb_int_num	= 20
.else
  usb_int_num	= 67			@ USB OTG FS
.endif
int_base	= 0xe000e300		@ interrupt status base address
int_statu1	= 0x00			@ for interrupts  0 to 31 -- TMR1, TMR2, I2C1, USB on regular line
int_statu2	= 0x04			@ for interrupts 32 to 63 -- I2C2, UART1, UART2 
int_statu3	= 0x08			@ for interrupts 64 to 95 -- USB OTG on connectivity line
int_status	= int_statu1		@ where to find the timer interrupts
int_en_base	= 0xe000e100
int_enabl1	= 0x00
int_enabl2	= 0x04
int_enabl3	= 0x08
int_disab1	= 0x80
int_disab2	= 0x84
int_disab3	= 0x88
uart0_int	= 1 << (uart0_int_num - 32)	@ bit  5 from statu2 (INT 37)
uart1_int	= 1 << (uart1_int_num - 32)	@ bit  6 from statu2 (INT 38)
timer0_int	= 1 << timer0_int_num		@ bit 28
timer1_int	= 1 << timer1_int_num		@ bit 29
i2c0_int	= 1 << i2c0_int_num		@ bit 31
i2c1_int	= 1 << (i2c1_int_num - 32)	@ bit  1 from statu2 (INT 33)
.ifndef	connectivity_ln
  usb_int	= 1 << usb_int_num		@ bit 20 -- USB.LPIRQ
.else
  usb_int	= 1 << (usb_int_num - 64)	@ bit  3 from statu3 (INT 67) USB OTG FS
.endif
.ifndef	connectivity_ln
  scheme_ints_en1 = timer0_int | timer1_int | i2c0_int | usb_int
.else
  scheme_ints_en1 = timer0_int | timer1_int | i2c0_int
.endif
scheme_ints_en2	= i2c1_int | uart0_int | uart1_int
scheme_ints_en3	= usb_int			@ for connectivity line (68 interrupts)

@ Cortex-M3 SysTick Timer
systick_base	= 0xe000e000
tick_ctrl	= 0x10
tick_load	= 0x14
tick_val	= 0x18

@ gpio
ioporta_base	= 0x40010800		@ I/O Port A base address
ioportb_base	= 0x40010c00		@ I/O Port B base address
ioportc_base	= 0x40011000		@ I/O Port C base address
ioportd_base	= 0x40011400		@ I/O Port D base address
ioporte_base	= 0x40011800		@ I/O Port E base address
ioportf_base	= 0x40011c00		@ I/O Port F base address
ioportg_base	= 0x40012000		@ I/O Port G base address
io_set		= 0x10			@ GPIOx_BSRR
io_clear	= 0x14			@ GPIOx_BRR
io_state	= 0x08

@ uarts	
.ifndef swap_default_usart
  uart0_base	= 0x40013800		@ USART1
  uart1_base	= 0x40004400		@ USART2
.else
  uart0_base	= 0x40004400		@ USART2
  uart1_base	= 0x40013800		@ USART1
.endif
uart_rhr	= 0x04			@ USART_DR
uart_thr	= 0x04			@ USART_DR
uart_status	= 0x00			@ USART_SR
uart_txrdy	= 0x80			@ Tx FIFO Empty
uart_istat	= 0x00			@ USART_SR

@ i2c
i2c0_base	= 0x40005400		@ I2C1
i2c1_base	= 0x40005800		@ I2C2
i2c_cr1		= 0x00			@ I2C_CR1
i2c_cr2		= 0x04			@ I2C_CR2
i2c_address	= 0x08			@ I2C_OAR1
i2c_rhr		= 0x10			@ I2C_DR
i2c_thr		= 0x10			@ I2C_DR
i2c_stat1	= 0x14			@ I2C_STAT1
i2c_stat2	= 0x18			@ I2C_STAT2
i2c_ccr		= 0x1c			@ I2C_CCR
i2c_trise	= 0x20			@ I2C_TRISE
i2c_irm_rcv	= 0x60			@ EV7 - SR1: RxNE, with MSL=1 (from SR2 at bit 5 in SR1)
i2c_irs_rcv	= 0x40			@ EV2 - SR1: RxNE, with MSL=0 (from SR2 at bit 5 in SR1)


@ SPI
spi1_base	= 0x40013000		@ SPI1 is the first SPI (there is no SPI0)
spi2_base	= 0x40003800
spi3_base	= 0x40003c00
spi_rhr		= 0x0c
spi_thr		= 0x0c
spi_status	= 0x08
spi_rxrdy	= 0x01
spi_txrdy	= 0x02

@ ADC
adc1_base	= 0x40012400
adc2_base	= 0x40012800
adc3_base	= 0x40013c00

@ SDIO
sdio_base	= 0x40018000

@ FSMC
fsmc_base	= 0xa0000000

@ AFIO
afio_base	= 0x40010000

@ timers
timer0_base	= 0x40000000		@ TIMER 2
timer1_base	= 0x40000400		@ TIMER 3
timer4_base	= 0x40000800		@ TIMER 4
timer5_base	= 0x40000c00		@ TIMER 5
timer6_base	= 0x40001000		@ TIMER 6
timer7_base	= 0x40001400		@ TIMER 7
timer_ctrl	= 0x00			@ TIMER CR1, used to enable/disable timer
timer_istat	= 0x10			@ TIMER SR
timer_iset	= timer_istat		@ TIMER SR,  used to clear update interrupt

@ USB

.ifndef	connectivity_ln

usb_base	= 0x40005C00		@ USB base address
usb_istat_dv	= 0x44			@ USB_ISTR
usb_daddr	= 0x4C
usb_iep_mask	= 0x8000		@ EndPoint Interrupts mask
usb_idv_mask	= 0x1C00		@ Device interrupts mask -- WakeUp, Suspend, Reset
usb_busreset	= 0x0400		@ 
usb_suspend	= 0x1000		@ (used to wake up device)
usb_txrdy	= 0x10			@ EP NAKing or disabled (ready for new Tx data) vs has data/stalled
usb_ibulkin	= 0x0C			@ offset of USB_EP3R
usb_itxendp	= 0xFF00		@ every device interrupt cleared at usbEPx
usb_iclear_dv	= 0x44			@ USB_ISTR

usbCO_setupbit	= 0x0800		@ EP status bit indicating last transfer was SETUP packet in USB_EP0R
UsbControlOutEP	= 0x00			@ Control OUT Endpoint
UsbControlInEP	= 0x00			@ Control IN  Endpoint (same as control out)

UsbBulkOutEP	= 0x02			@ Bulk OUT EP
UsbBulkInEP	= 0x03			@ Bulk IN  EP 3
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for descriptor at end of file)
usbCO_ibit	= 0x010000		@ bit indicating interrupt for Control OUT Endpoint
usbCI_ibit	= 0x020000		@ bit indicating interrupt for Control IN  Endpoint
usbBO_ibit	= 0x100000		@ bit indicating interrupt for Bulk    OUT Endpoint
usbBI_ibit	= 0x800000		@ bit indicating interrupt for Bulk    IN  Endpoint

.else

@
@ connectivity line USB OTG FS does not work in this version (eg. STM32_H107)
@

usb_base	= 0x50000000		@ USB OTG FS base address
usb_istat_dv	= 0x14			@ USB_FS_GINTSTS
usb_iep_mask2	= 0x0C0000		@ EndPoint Interrupts mask (IN, OUT) in GINTSTS
usb_iep_mask	= 0x10			@ RxLVL in GINTSTS
@usb_iep_mask	= 0x30			@ NPTXFE and RxLVL in GINTSTS
usb_idv_mask	= 0x003800		@ Device interrupts mask -- EnumDone, Suspend, Reset
usb_busreset	= 0x1000		@ 
usb_suspend	= 0x0800		@ (used to wake up device) but this is suspend (backwards?)
@@usb_itxendp	= 0x0C0000		@ every device interrupt cleared at usbEPx
usb_itxendp	= 0x00			@ every device interrupt cleared at usbEPx
usb_iclear_dv	= 0x14			@ USB_FS_GINTSTS
usb_daddr	= 0x0800 		@ OTG_FS_DCFG
usb_txrdy	= 0x0ff0 		@ more than 16 words available in FIFO
usb_ibulkin	= 0x78 	 	  	@ OTG_FS_DTXFSTX3 (0x900 offset added in code) 
usbCO_setupbit	= 0x08			@ EP status bit indicating last transfer was a SETUP packet
UsbControlOutEP	= 0x00			@ Control OUT Endpoint
UsbControlInEP	= 0x00			@ Control IN  Endpoint (same as control out)
UsbBulkOutEP	= 0x02			@ Bulk OUT EP 2
UsbBulkInEP	= 0x03			@ Bulk IN  EP 3
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for descriptor at end of file)
usbCO_ibit	= 0x010000		@ bit indicating interrupt for Control OUT Endpoint
usbCI_ibit	= 0x000001		@ bit indicating interrupt for Control IN  Endpoint
usbBO_ibit	= 0x040000		@ bit indicating interrupt for Bulk    OUT Endpoint
usbBI_ibit	= 0x000008		@ bit indicating interrupt for Bulk    IN  Endpoint

.endif

@ power
rcc_base	= 0x40021000		@ RCC base address (power, clocks and reset)

@ flash	
flashcr_base	= 0x40022000		@ FLASH control registers base
