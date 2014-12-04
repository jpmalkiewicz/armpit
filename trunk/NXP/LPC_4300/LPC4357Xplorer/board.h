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

@-------10--------20--------30--------40--------50--------60--------70--------80

/* =======  BOARD =========== */
LPC4357_Xplorer	= 1		@ NGX LPC4357-Xplorer++ / NXP LPC4357

/* ======= OPTIONS ========== */

/**/
run_in_bank2	= 1
_data_section_address_	= 0x00006f00
_data_link_address_	= 0x10006f00
/**/

run_at_120mhz	= 1		@ run MPU at 120 MHz (default is 204 MHz)
CORE_ONLY	= 1
integer_only	= 1
exclude_r6rs_fx = 1		@ (fx req by (lcd draw) lib but code then 2 big)
nrml_lambda_lkp	= 1
stop_and_copy	= 1
hardware_FPU	= 1		@ use the FPU (for floats)
onboard_SDFT	= 1		@ include SD card file subsystem

/**/
upload_via_DFU	= 1		@ upload to SPIFI via DFU (vs JTAG)
/**/

enable_cpo	= 1		@ enable coprocessor offload
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= SCU_SFSP9_n	@ LED pin func ctl P9_6(L11)-GPIO4[11]
LEDIO		= io4_base	@ LED on/off pins are on GPIO4[n]
rled_pin	= 11		@ LED on GPIO4[11]
yled_pin	= 11		@ aliased
gled_pin	= 11		@ aliased
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PSL	= SCU_SFSP8_n	@ Boot-override button pin func ctl P8_0
BOOTOVERRID_PRT	= io4_base	@ Boot-override is on I/O Port 4, GPIO4[0]=P8_0
BOOTOVERRID_BUT	= 0		@ Boot-override button is GPIO4[0] = P8_0
@ --------- UART -----------
UART_PINSEL	= SCU_SFSPF_n	@ uart0 pins:PF_10(A3)=TXD,PF_11(A2)=RXD
UART_tx_pin	= 10		@ PF_10 is Tx
UART_rx_pin	= 11		@ PF_11 is Rx
UART_pinmode	= 1		@ set uart pins to mode 1 = uart
UART0_DIV_L	= 65		@ divisor low  byte, 115200 baud, pclk = 120 MHz
UART0_DIV_H	= 0		@ divisor high byte, 115200 baud, pclk = 120 MHz
@ ------ Native USB --------
usb_queue_heads	= 0x2000f800	@ USB queue heads stored in 3rd RAM bank
@ --------- SD card ---------
sd_is_on_mci	= 1		@ SD card is on sd/mmc interface
sd_mci		= mmc_base	@ base address of sd/mmc interface
sd_psl_d0_cmd	= SCU_SFSPC_n	@ mci pin func ctl reg for D0-D3 and CMD = PC
sd_mod_d0_cmd	= 7		@ mci pin mode for D0-D3 and CMD = 7
sd_pin_d0	= 4		@ mci pin for D0  =  4 (D0-D3 = PC_4-PC_7)
sd_pin_cmd	= 10		@ mci pin for CMD = 10 (PC_10)
sd_psl_clk	= SCU_SFSPC_n	@ mci pin func ctl reg for CLK = PC
sd_mod_clk	= 7		@ mci pin mode for CLK = 7
sd_pin_clk	= 0		@ mci pin for CLK = 0 (PC_0)
@ --------- FREQs -----------
.ifndef	run_at_120mhz
  SYSTICK_RELOAD = 204*10000-1	@ systick reload for 10ms interrupts at 204 MHz
.else
  SYSTICK_RELOAD = 120*10000-1	@ systick reload for 10ms interrupts at 120 MHz
.endif
@ --------- RAM ------------
MAIN_STACK	= 0x10008000	@ LPC 4357 main stack at top of bank 1
RAMBOTTOM	= 0x28000000	@ heap in off-chip SDRAM
RAMTOP		= 0x2a000000	@ LPC 4357 Xplorer++ (32 MB off-chip SDRAM)
@ ------- COPROCESSOR ------
cpo_rx_buffer	= 0x2000fc00
cpo_tx_buffer	= 0x2000fd00
cpo_rx_msg	= 0x2000fe00		@ M0->M4 msg, 1=USB rx, 2=gc mark done
cpo_tx_msg	= 0x2000ff00		@ M4->M0 msg, 1=USB tx, 2/3=strt/stp gc
@ --------- BUFFERS --------
BUFFER_START	= 0x10000000+4	@ buffers at start of on-chip RAM bank 1
RBF_size	= 0x1000	@ READBUFFER  size for tag as bytevector (4KB)
WBF_size	= 0x0C00	@ WRITEBUFFER size for tag as bytevector (3KB)
I2C0ADR		= i2c0_base + i2c_address
@ --------- FLASH ----------
ext_flash_only	= 1		@ external flash only (no on-chip flash)
ext_flash_spifi	= 1		@ external flash is SPIFI
spifimap64x64KB	= 1		@ SPANSION S25FL032P, 64 x 64KB sectors
F_START_PAGE	= 0x14010000	@ SPIFI (above 64KB stored code)
F_END_PAGE	= 0x143F0000	@ top of 4 MB SPIFI, minus top 64KB crnch sector
F_PAGE_SIZE	= 256		@ size of pages used for files

@-------10--------20--------30--------40--------50--------60--------70--------80




