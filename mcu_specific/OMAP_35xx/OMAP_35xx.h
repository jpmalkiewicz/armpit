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

@ architecture/model
cortex_a8	= 1

@ interrupts
irq_direct_branch	= 1		@ branch to genisr diectly on interrupt (instead of VIC)
num_interrupts	= 96
uart0_int_num	= 74			@ uart3
uart1_int_num	= 74			@ uart3
timer0_int_num	= 37			@ timer1
timer1_int_num	= 38			@ timer2
i2c0_int_num	= 56			@ i2c1
i2c1_int_num	= 57			@ i2c2
int_base	= 0x48200000		@ interrupt controller base address
int_status	= 0xb8			@ pending irq (32-63)
int_base2	= 0x48200020		@ base to get irq 64-95
int_clear	= 0x48			@ INTCPS_CONTROL
int_clear_vals  = 0x01			@ IRQ_AGR_MASK for INTCPS_CONTROL

@ clocks
CORE_CM_base	= 0x48004a00
WKUP_CM_base	= 0x48004C00
PER_CM_base	= 0x48005000		@ gpio2-6, wdt2, uart3, tmr2-9, McBSP2-4, 
F_clck		= 0x00			@ functional clock
I_clck		= 0x10			@ interface  clock

@ System Control Module
SCM_base	= 0x48002000

@ Memory Control
GPMC_base	= 0x6E000000

@ gpio
io1_base	= 0x48310000		@ GPIO1 -- USER button on TI_Beagle, LED on Thumbo (Gumstix)
io2_base	= 0x49050000		@ GPIO2 -- 
io3_base	= 0x49052000		@ GPIO3 -- 
io4_base	= 0x49054000		@ GPIO4 -- 
io5_base	= 0x49056000		@ GPIO5 -- LED port on TI_Beagle
io6_base	= 0x49058000		@ GPIO6 -- 
io_dir		= 0x34			@ GPIO_OE (1 is input, 0 is output)
io_set		= 0x94			@ SETDATAOUT
io_clear	= 0x90			@ CLEARDATAOUT	
io_state	= 0x38			@ DATAIN
io_out_state	= 0x3c			@ DATAOUT

@ uarts (page 69+)
uart0_base	= 0x49020000		@ UART3 base address (default uart)
uart1_base	= uart0_base		@ aliased to uart3
uart_rhr	= 0x00			@ RHR
uart_thr	= 0x00			@ THR
uart_istat	= 0x08			@ IIR
uart_status	= 0x14			@ LSR
uart_txrdy	= 0x20			@ Tx Buffer Empty	

@ spi
spi1_base	= 0x48098000		@ MCSPI1
spi2_base	= 0x4809a000		@ MCSPI2
spi3_base	= 0x480b8000		@ MCSPI3
spi4_base	= 0x480ba000		@ MCSPI4
spi_rhr		= 0x3c			@ channel 0 (SPI1-4 are multi-channel)
spi_thr		= 0x38			@ channel 0 (SPI1-4 are multi-channel)
spi_status	= 0x18			@ channel 0 (SPI1-4 are multi-channel)
spi_rxrdy	= 0x04			@ channel 0 (SPI1-4 are multi-channel)
spi_txrdy	= 0x01			@ channel 0 (SPI1-4 are multi-channel)

@ timers
@ armpit scheme internal names
timer0_base	= 0X48318000		@ GPTIMER1
timer1_base	= 0X49032000		@ GPTIMER2
@ normal timer assignment -- for user (except timer1)
timer2_base	= 0X49032000		@ GPTIMER2
timer3_base	= 0X49034000		@ GPTIMER3
timer4_base	= 0X49036000		@ GPTIMER4
timer5_base	= 0X49038000		@ GPTIMER5
timer6_base	= 0X4903a000		@ GPTIMER6
timer7_base	= 0X4903c000		@ GPTIMER7
timer8_base	= 0X4903e000		@ GPTIMER8
timer9_base	= 0X49040000		@ GPTIMER9
timer10_base	= 0X48086000		@ GPTIMER10
timer11_base	= 0X48088000		@ GPTIMER11
timer_istat	= 0x18			@ TISR
timer_iset	= 0x18			@ TISR
timer_ctrl	= 0x24			@ TCLR

@ MMC
mmc1_base	= 0X4809c000		@ MMC1 base address

@ i2c --  partially done
@ used in polling mode, not interrupt, to communicate with TWL4030/TPS65950
i2c0_base	= 0x48070000		@ I2C1
i2c1_base	= 0x48070000		@ I2C1
i2c_address	= 0x28			@ own address 0 _OA0
i2c_cset	= 0x24			@ _CON
i2c_status	= 0x08			@ _STAT
i2c_rhr		= 0x1c			@ _DATA
i2c_thr		= 0x1c			@ _DATA

i2c_sadr	= 0x2c			@ _SA (target slave address)
i2c_cnt		= 0x18			@ _CNT

@i2c_data	= 0x00			@ not used
@i2c_cclear	= 0x00			@ 
i2c_irm_rcv	= 0x08			@ RxRdy
i2c_irs_rcv	= 0x08			@ RxRdy

@ usb --  (resembles S3C24xx, except for byte, half-word access and corresponding offsets)
@has_HS_USB      = 1                     @ this controller supports High-Speed operation
USB_FSHS_MODE   = BUFFER_START + 4 + 0x0C @ recycling I2C0ADR (unused as mcu-id is in i2c)
usb_base	= 0x480ab000		@ NOT DONE !!!!!
usb_istat_dv 	= 0x00
usb_istat_dv2 	= 0x0a
usb_iep_mask 	= 0x001f0000
usb_idv_mask 	= 0x07
usb_busreset 	= 0x04
usb_suspend  	= 0x00
usb_itxendp  	= 0x00  @ dummy
@ usb_iclear_dv = usb_istat_dv2 @ (bloody thing is apparently read-only! -- not used)
usb_index_reg 	= 0x0e
usb_ctl_stat  	= 0x12
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
usbBulkINDescr	= 0x83			@ Bulk IN is EP 3 (for descriptor at end of file)
