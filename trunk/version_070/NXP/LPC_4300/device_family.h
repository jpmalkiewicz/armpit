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
LPC_4300	= 1			@ NXP LPC4300 family MCU

@ architecture
cortex		= 1
.cpu		cortex-m4
.fpu		fpv4-sp-d16

@ interrupts
num_interrupts	= 53
cpo_int_num	= 1			@ M0 Tx event
uart0_int_num	= 24
uart1_int_num	= 25
timer0_int_num	= 12			@ 
timer1_int_num	= 13			@ 
i2c0_int_num	= 18
i2c1_int_num	= 19
.ifndef use_usb1
  usb_int_num	=  8			@ USB-0
.else
  usb_int_num	=  9			@ USB-1
.endif

int_status	= int_stat__0__31	@ where to find the timer interrupts

cpo_int		= 1 << (cpo_int_num    % 32)
uart0_int	= 1 << (uart0_int_num  % 32)
uart1_int	= 1 << (uart1_int_num  % 32)
timer0_int	= 1 << (timer0_int_num % 32)
timer1_int	= 1 << (timer1_int_num % 32)
i2c0_int	= 1 << (i2c0_int_num   % 32)
i2c1_int	= 1 << (i2c1_int_num   % 32)
usb_int		= 1 << (usb_int_num    % 32)

scminten__0__31 = cpo_int|timer0_int|timer1_int|i2c0_int|i2c1_int|uart0_int|uart1_int|usb_int
scminten_32__63 = 0

@ on-chip memory banks
RAM_bank_1	= 0x10000000
RAM_bank_2	= 0x10080000
RAM_bank_3	= 0x20000000
RAM_bank_4	= 0x20008000
RAM_bank_5	= 0x2000c000

@ size of boot ram
.ifndef LPC4357_Xplorer
  boot_ram_size = 0x010000		@ 64KB boot RAM
.else
  boot_ram_size = 0x008000		@ 32KB boot RAM
.endif

@ emc
emc_base	= 0x40005000		@ External Memory Controller

@ gpio
io0_base 	= 0x400F6000 		@ gpio0 dir
io1_base 	= 0x400F6004 		@ gpio1 dir
io2_base 	= 0x400F6008 		@ gpio2 dir
io3_base 	= 0x400F600C 		@ gpio3 dir
io4_base 	= 0x400F6010 		@ gpio4 dir
io5_base 	= 0x400F6014 		@ gpio5 dir
io6_base 	= 0x400F6018 		@ gpio6 dir
io7_base 	= 0x400F601C 		@ gpio7 dir
io_set		= 0x0200		@ SET
io_dir		= 0x0000		@ DIR
io_clear	= 0x0280		@ CLR
io_state	= 0x0100		@ PIN -- read state of INPUT  pin
@io_state	= 0x0200		@ SET -- use this to read OUTPUT pin?
io_toggle	= 0x0300		@ NOT

@ pin configuration
SCU_SFSP0_n	= 0x40086000		@ SFSP0_0 to SFSP0_1  base address
SCU_SFSP1_n	= 0x40086080		@ SFSP1_0 to SFSP1_20 base address
SCU_SFSP2_n	= 0x40086100		@ SFSP2_0 to SFSP2_13 base address
SCU_SFSP3_n	= 0x40086180		@ SFSP3_0 to SFSP3_8  base address
SCU_SFSP4_n	= 0x40086200		@ SFSP4_0 to SFSP4_10 base address
SCU_SFSP5_n	= 0x40086280		@ SFSP5_0 to SFSP5_7  base address
SCU_SFSP6_n	= 0x40086300		@ SFSP6_0 to SFSP6_12 base address
SCU_SFSP7_n	= 0x40086380		@ SFSP7_0 to SFSP7_7  base address
SCU_SFSP8_n	= 0x40086400		@ SFSP8_0 to SFSP8_8  base address
SCU_SFSP9_n	= 0x40086480		@ SFSP9_0 to SFSP9_6  base address
SCU_SFSPA_n	= 0x40086500		@ SFSPA_0 to SFSPA_4  base address
SCU_SFSPB_n	= 0x40086580		@ SFSPB_0 to SFSPB_6  base address
SCU_SFSPC_n	= 0x40086600		@ SFSPC_0 to SFSPC_14 base address
SCU_SFSPD_n	= 0x40086680		@ SFSPD_0 to SFSPD_16 base address
SCU_SFSPE_n	= 0x40086700		@ SFSPE_0 to SFSPE_15 base address
SCU_SFSPF_n	= 0x40086780		@ SFSPF_0 to SFSPF_11 base address
SCU_SFSCLKn	= 0x40086C00		@ SFSCLK0 to SFSCLK3  base address
SCU_SFSUSB	= 0x40086C80		@ SFSUSB  base address

@ uarts	
uart0_base	= 0x40081000		@ UART0
uart1_base	= 0x40082000		@ UART1
uart_rhr	= 0x00			@ RBR
uart_thr	= 0x00			@ THR
uart_ier	= 0x04			@ IER
uart_istat	= 0x08
uart_status	= 0x14			@ offset to uart status register
uart_txrdy	= 0x20			@ bit indicating uart THR empty
uart_idr	= uart_ier		@ interrupt disable reg offset
uart_iRx_ena	= 1			@ value to enable  interrupts
uart_iRx_dis	= 0			@ value to disable interrupts

@ i2c -- NOT YET IMPLEMENTED!!!	
i2c0_base	= 0x400A1000		@ I2C0
i2c1_base	= 0x400E0000		@ I2C1
i2c_cset	= 0x00
i2c_status	= 0x04
i2c_rhr		= 0x08
i2c_thr		= 0x08
i2c_data	= 0x08
i2c_address	= 0x0C
i2c_cclear	= 0x18
i2c_irm_rcv	= 0x50			@ ok on cortex? (this is from LPC2000)
i2c_irs_rcv	= 0x80			@ ok on cortex? (this is from LPC2000)

@ SPIFI
spifi_base	= 0x40003000		@ SPIFI (Ch.16, 18 May 2011 User Manual)
spifi_ctrl	= 0x00			@ SPIFICTRL
spifi_cmd	= 0x04			@ SPIFICMD
spifi_addr	= 0x08			@ SPIFIADDR
spifi_idat	= 0x0c			@ SPIFIDATINTM
spifi_adid	= 0x10			@ SPIFIADDRINTM
spifi_dat	= 0x14			@ SPIFIDAT
spifi_mcmd	= 0x18			@ SPIFIMEMCMD
spifi_stat	= 0x1c			@ SPIFISTAT

@ SPI
spi0_base	= 0x40100000		@ SPI0 (legacy SPI)
spi1_base	= spi0_base		@ aliased
spi_cr		= 0x00
spi_ccr		= 0x10
spi_rhr		= 0x08
spi_thr		= 0x08
spi_status	= 0x04
spi_rxrdy	= 0x80
spi_cs_gpio	= io5_base
spi_cs_pin	= 1 << 11

@ timers
timer0_base	= 0x40084000		@ TIMER 0
timer1_base	= 0x40085000		@ TIMER 1
timer_istat	= 0x00			@ ok on cortex? (this is from LPC2000)
timer_iset	= 0x00			@ ok on cortex? (this is from LPC2000)
timer_ctrl	= 0x04			@ ok on cortex? (this is from LPC2000)

@ SD/MMC
mmc_base	= 0x40004000
mmc_arg		= 0x28
mmc_cmd		= 0x2c

@ rtc
rtc0_base	= 0x40046000

@ adc
adc0_base	= 0x400E3000
adc1_base	= 0x400E4000

@ pwm
pwm1_base	= 0x400A0000

@ lcd (LPC4357)
lcd_base	= 0x40008000

@ usb
usb0_base	= 0x40006100		@ USB-0 base
usb1_base	= 0x40007100		@ USB-1 base
.ifndef use_usb1
  usb_base	= usb0_base		@ USB-0 base
.else
  usb_base	= usb1_base		@ USB-1 base
.endif
@has_HS_USB	= 1			@ <- KEEP COMMENTED (HS not functional)
USB_FSHS_MODE   = usb_queue_heads+0x38	@ where to store HS/FS state
					@ (in-between 64-byte-aligned 48-byte queue heads)
QH_CO		= usb_queue_heads+0x000	@ queue head         for Control OUT EP
QH_CI		= usb_queue_heads+0x040	@ queue head         for Control IN  EP
QH_BO		= usb_queue_heads+0x100	@ queue head         for Bulk    OUT EP
QH_BI		= usb_queue_heads+0x140	@ queue head         for Bulk    IN  EP
dTD_CO		= usb_queue_heads+0x180	@ data Rx descriptor for Control OUT EP
dTD_CI		= usb_queue_heads+0x1a0	@ data Tx descriptor for Control IN  EP
dTD_BO		= usb_queue_heads+0x1c0	@ data Rx descriptor for Bulk    OUT EP
dTD_BI		= usb_queue_heads+0x1e0	@ data Tx descriptor for Bulk    IN  EP
BUFFER8_CO	= usb_queue_heads+0x030	@  8-byte Rx buffer  for Control OUT EP
BUFFER64_BO	= usb_queue_heads+0x080	@ 64-byte Rx buffer  for Bulk    OUT EP
BUFFER64_BI	= usb_queue_heads+0x0c0	@ 64-byte Tx buffer  for Bulk    IN  EP

usb_istat_dv	= 0x44			@ USBDevIntSt
usb_iep_mask	= 0x01			@ mask for endpoint interrupt
usb_iclear_dv	= usb_istat_dv		@ USBDevIntClr
usb_idv_mask	= (1 << 6)		@ mask for device status interrupt
usb_busreset	= (1 << 6)		@ bus reset bit
usb_itxendp	= 0			@ Tx end of packet interrupt bit
usb_suspend	= 0			@ suspend bit
usb_istat_ep	= 0xbc			@ USBEpIntSt
usb_iclear_ep	= usb_istat_ep		@ USBEpIntClr
usbCO_ibit	= (1 <<  0)		@ bit indic int for Control OUT Endpoint
usbCI_ibit	= (1 << 16)		@ bit indic int for Control IN  Endpoint
usbBO_ibit	= (1 <<  2)		@ bit indic int for Bulk    OUT Endpoint
usbBI_ibit	= (1 << 18)		@ bit indic int for Bulk    IN  Endpoint
usbBulkINDescr	= 0x82			@ Bulk IN is EP 2 (for desc at end file)
usbCO_setupbit	= (1 << 0)		@ EP stat bit indic last tfer was SETUP
UsbControlOutEP	= 0x00			@ Control IN Endpoint (phys 0, log 0)
UsbControlInEP	= 0x01			@ Control IN Endpoint (phys 1, log 0)
UsbBulkOutEP	= 0x04			@ Bulk OUT EP (phys = 4, log = 2)
UsbBulkInEP	= 0x05			@ Bulk IN  EP (phys = 5, log = 2)
usb_ibulkin	= 0xb8			@ to find status of Bulk IN EP (primed?)
usb_txrdy	= (1 << 18)		@ Tx rdy bit in Bulk_IN (EP Tx primed)

@ M0 - M4 Inter-Process Communication (IPC)
cpo_tx_status	= 0x40043130		@ M4TXEVENT register address
cpo_rxint_clear	= 0x40043400		@ M0TXEVENT register address

@ system
sys_config	= 0x40043000		@ CREG (M4-M0 mem remap, ETB RAM cfg,..)

@ clocks
CGU_base	= 0x40050000

@ flash -- Note: IAP uses 32 bytes at top of RAM bank 2 (not RAM bank 1)
@                it also uses 256 bytes of stack space (vs 128 for other LPCs)
INIT_IAP_CMD49	= 1			@ IAP write/erase needs extra CMD 49
flash_tim	= 0x8000923A		@ FLASHCFGA value for 204MHz, p.87 (read on running chip)
@flash_tim	= 0x8000523A		@ FLASHCFGA value for 120MHz, p.87
@flash_tim	= 0x8000903A		@ FLASHCFGA value for 204MHz, p.87
@flash_tim	= 0x8000503A		@ FLASHCFGA value for 120MHz, p.87
IAP_ENTRY_PTR	= 0x10400100		@ IAP entry pointer in ROM,   p.1230
@IAP_ENTRY	= 0x10408581		@ IAP entry read at ROM pointer, on-chip

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



