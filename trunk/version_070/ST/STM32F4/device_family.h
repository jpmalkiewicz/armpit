/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2012-2014 Petr Cermak

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
STM32F4		= 1			@ ST STM32F4 family MCU

@ architecture
cortex		= 1
.cpu		cortex-m4
.fpu		fpv4-sp-d16

.ifdef harvard_split
  _data_link_address_	= 0x10000000	@ CCM start address for .data section
  grey_area_base	= 0x10004000	@ mark-and-sweep grey/black areas in CCM
  BUFFER_START		= 0x10008000+4	@ i/o buffers in CCM
.endif

@ type of gpio
has_combined_set_clear	= 1		@ MCU has combined GPIO SET + CLEAR reg

@ interrupts
num_interrupts  = 82
@num_interrupts  = 82			@ 85 interrupts on STM32F401RET6 -- last few not treated

.ifndef swap_default_usart
  uart0_int_num	= 37			@ USART1
  uart1_int_num	= 38			@ USART2
.else
  uart0_int_num	= 38			@ USART2
  uart1_int_num	= 37			@ USART1
.endif

timer0_int_num	= 28			@ Timer 2
timer1_int_num	= 50			@ Timer 5
i2c0_int_num	= 31
i2c1_int_num	= 33
.ifndef USB_is_OTG_HS
  usb_int_num	= 67			@ USB OTG FS
.else
  usb_int_num	= 77			@ USB OTG HS
.endif

int_status	= int_stat__0__31	@ where to find the timer interrupts

timer0_int	= 1 << (timer0_int_num % 32)
timer1_int	= 1 << (timer1_int_num % 32)
i2c0_int	= 1 << (i2c0_int_num   % 32)
i2c1_int	= 1 << (i2c1_int_num   % 32)
uart0_int	= 1 << (uart0_int_num  % 32)
uart1_int	= 1 << (uart1_int_num  % 32)
usb_int		= 1 << (usb_int_num    % 32)

scminten__0__31	= timer0_int | i2c0_int
scminten_32__63	= i2c1_int   | uart0_int  | uart1_int | timer1_int
scminten_64__95	= usb_int

@ gpio
ioporta_base	= 0x40020000		@ I/O Port A base address
ioportb_base	= 0x40020400		@ I/O Port B base address
ioportc_base	= 0x40020800		@ I/O Port C base address
ioportd_base	= 0x40020C00		@ I/O Port D base address
ioporte_base	= 0x40021000		@ I/O Port E base address
ioportf_base	= 0x40021400		@ I/O Port F base address
ioportg_base	= 0x40021800		@ I/O Port G base address
ioporth_base	= 0x40021C00		@ I/O Port H base address
ioporti_base	= 0x40022000		@ I/O Port I base address
ioportj_base	= 0x40022400		@ I/O Port J base address
ioportk_base	= 0x40022800		@ I/O Port K base address
io_set		= 0x14			@ GPIOx_ODR
io_clear	= 0x14			@ GPIOx_ODR
io_state	= 0x10

@ uarts
.ifndef swap_default_usart
  uart0_base	= 0x40011000		@ USART1
  uart1_base	= 0x40004400		@ USART2
.else
  uart0_base	= 0x40004400		@ USART2
  uart1_base	= 0x40011000		@ USART1
.endif
uart_rhr	= 0x04			@ USART_DR
uart_thr	= 0x04			@ USART_DR
uart_status	= 0x00			@ USART_SR
uart_txrdy	= 0x80			@ Tx FIFO Empty
uart_istat	= 0x00			@ USART_SR
uart_ier	= 0x0c			@ interrupt enable  reg offset
uart_idr	= uart_ier		@ interrupt disable reg offset
uart_iRx_ena	= 0x202c		@ value to enable  interrupts
uart_iRx_dis	= 0x200c		@ value to disable interrupts

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
i2c_irm_rcv	= 0x60			@ EV7 - SR1: RxNE, w/MSL=1 SR2 bit 5 SR1
i2c_irs_rcv	= 0x40			@ EV2 - SR1: RxNE, w/MSL=0 SR2 bit 5 SR1


@ SPI
spi1_base	= 0x40013000		@ SPI1 is the first SPI (no SPI0)
spi2_base	= 0x40003800
spi3_base	= 0x40003c00
spi4_base	= 0x40013400
spi5_base	= 0x40015000
spi6_base	= 0x40015400
spi_rhr		= 0x0c
spi_thr		= 0x0c
spi_status	= 0x08
spi_rxrdy	= 0x01
spi_txrdy	= 0x02

@ ADC
adc1_base	= 0x40012000
adc2_base	= 0x40012100
adc3_base	= 0x40012200

@ SDIO
sdio_base	= 0x40012C00

@ FSMC (STM32F407)
fsmc_base	= 0xa0000000

@ AFIO
afm_base	= 0x40010000

@ POWER
pwr_base	= 0x40007000

@ LCD
lcd_base	= 0x40016800

@ FMC -- external memory (STM32F429)
fmc_base	= fsmc_base

@ timers
timer0_base	= 0x40000000		@ TIMER 2
timer1_base	= 0x40000c00		@ TIMER 5
timer1_base_a	= 0x40010000		@ TIMER 1
timer2_base	= 0x40000000		@ TIMER 2
timer3_base	= 0x40000400		@ TIMER 3
timer4_base	= 0x40000800		@ TIMER 4
timer5_base	= 0x40000c00		@ TIMER 5
timer6_base	= 0x40001000		@ TIMER 6
timer7_base	= 0x40001400		@ TIMER 7
timer8_base	= 0x40010400		@ TIMER 8
timer9_base	= 0x40014000		@ TIMER 9
timer10_base	= 0x40014400		@ TIMER 10
timer11_base	= 0x40014800		@ TIMER 11
timer12_base	= 0x40001800		@ TIMER 12
timer13_base	= 0x40001C00		@ TIMER 13
timer14_base	= 0x40002000		@ TIMER 14
timer_ctrl	= 0x00			@ TIMER CR1, used to enab/disab timer
timer_istat	= 0x10			@ TIMER SR
timer_iset	= timer_istat		@ TIMER SR,  used to clear update int

@ usb
.ifndef USB_is_OTG_HS
  usb_base	= 0x50000000		@ USB OTG FS base address
.else
  usb_base	= 0x40040000		@ USB OTG HS base address
.endif
usb_istat_dv	= 0x14			@ USB_FS_GINTSTS
usb_iep_mask	= 0x0C0010		@ RxLVL and EP IN/OUT in GINTSTS
usb_idv_mask	= 0x003800		@ Dev ints mask - EnumDone,Suspend,Reset
usb_busreset	= 0x1000		@ 
usb_suspend	= 0x0800		@ wake up dev but is suspend backwards?
usb_itxendp	= 0x00			@ every dev interrupt cleared at usbEPx
usb_iclear_dv	= 0x14			@ USB_FS_GINTSTS
usb_daddr	= 0x0800 		@ OTG_FS_DCFG
usb_txrdy	= (1 << 31) 		@ EP3 IN free (not enabled)
usb_ibulkin	= 0x960 	 	@ OTG_FS_DIEPCTL3
usbCO_setupbit	= 0x08			@ EP stat bit indic last tfer was SETUP
UsbControlOutEP	= 0x00			@ Control OUT Endpoint
UsbControlInEP	= 0x00			@ Control IN  Endpoint (id control out)
UsbBulkOutEP	= 0x02			@ Bulk OUT EP 2
UsbBulkInEP	= 0x03			@ Bulk IN  EP 3
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for desc at end file)
usbCO_ibit	= 0x010000		@ bit indic int for Control OUT Endpoint
usbCI_ibit	= 0x000001		@ bit indic int for Control IN  Endpoint
usbBO_ibit	= 0x040000		@ bit indic int for Bulk    OUT Endpoint
usbBI_ibit	= 0x000008		@ bit indic int for Bulk    IN  Endpoint


@ power
rcc_base	= 0x40023800		@ RCC base address (power, clocks and reset)

@ flash
flashcr_base	= 0x40023C00		@ FLASH control register base (FLASH_ACR)
flash_busy	= 16			@ FLASH busy bit in FLASH_SR
flash_ersbysnb	= 1			@ flash erased by sector num (not page)
flash_set16bprg	= 1			@ flash programming needs 16-bit setting

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



