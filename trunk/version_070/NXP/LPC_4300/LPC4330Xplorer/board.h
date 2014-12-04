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
LPC4330_Xplorer	= 1		@ NGX LPC4330-Xplorer / NXP LPC4330

/* ======= OPTIONS ========== */
upload_via_DFU	= 1		@ upload to SPIFI via DFU
run_in_bank2	= 1		@ run core from RAM bank 2 (heap in bank 1)
hardware_FPU	= 1		@ use the FPU (for floats)
enable_MPU	= 1		@ use the MPU
native_usb	= 1		@ include usb support
@use_usb1	= 1		@ use USB1 hardware (vs USB0)
enable_cpo	= 1		@ enable coprocessor offload (cortex-M0)
cpo_default_io	= 1		@ set default io port to cpo
onboard_SDFT	= 1		@ include SD card file subsystem
@sdhc_on_sd_mmc	= 1		@ use SDHC cards on MMC interface (no normal SD)

/* ===== CONFIGURATION ====== */
@ --------- LEDs -----------
LEDPINSEL	= SCU_SFSP2_n	@ LED pin func ctl P2_11/12(A9/B9)-GPIO1[11/12]
LEDIO		= io1_base	@ LED on/off pins are on GPIO1[n]
rled_pin	= 11		@ Blue  LED on GPIO1[11]
yled_pin	= 12		@ aliased
gled_pin	= 12		@ Green LED on GPIO1[12]
@ ----- BOOT OVERRIDE -------
BOOTOVERRID_PSL	= SCU_SFSP2_n	@ Boot-override button pin func ctl P2_7
BOOTOVERRID_PRT	= io0_base	@ Boot-override is on I/O Port 0, GPIO0[7]=P2_7
BOOTOVERRID_BUT	= 7		@ Boot-override button is GPIO0[7] = P2_7
@ --------- UART -----------
UART_PINSEL	= SCU_SFSP6_n	@ uart0 pins: P6_4(F6)=TXD,P6_5(F9)=RXD
UART_tx_pin	= 4		@ P6_4 is Tx
UART_rx_pin	= 5		@ P6_5 is Rx
UART_pinmode	= 2		@ set uart pins to mode 2 = uart
UART0_DIV_L	= 65		@ divisor low  byte, 115200 baud, pclk = 120 MHz
UART0_DIV_H	= 0		@ divisor high byte, 115200 baud, pclk = 120 MHz
@ ------ Native USB --------
usb_queue_heads	= 0x2000f800	@ USB queue heads stored in 3rd RAM bank
@ --------- SD card ---------
sd_is_on_mci	= 1		@ SD card is on sd/mmc interface
sd_mci		= mmc_base	@ base address of sd/mmc interface
sd_psl_d0_cmd	= SCU_SFSP1_n	@ mci pin func ctl reg for D0-D3 and CMD = P1
sd_mod_d0_cmd	= 7		@ mci pin mode for D0-D3 and CMD
sd_pin_cmd	= 6		@ mci pin for CMD = 6 (P1_6)
sd_pin_d0	= 9		@ mci pin for D0  = 9 (D0-D3 = P1_9-P1_12)
sd_psl_clk	= SCU_SFSCLKn	@ mci pin func ctl reg for CLK = SFSCLKn
sd_mod_clk	= 4		@ mci pin mode for CLK = 4, CLK2
sd_pin_clk	= 2		@ mci pin for CLK = 2 (CLK2 in SFSCLKn)
@ --------- FREQs -----------
SYSTICK_RELOAD	= 204*10000 - 1	@ systick reload for 10ms interrupts at  204 MHz
@ --------- RAM ------------
.ifdef run_in_bank2
  RAMBOTTOM	= 0x10000000	@ heap in on-chip RAM, bank 1 (code in bank 2)
  RAMTOP	= 0x10020000	@ LPC 4330 (128 kB)
.else
  RAMBOTTOM	= 0x10080000	@ heap in on-chip RAM, bank 2 (code in bank 1)
  RAMTOP	= 0x10092000	@ LPC 4330 (72 kB)
.endif
@ ------- COPROCESSOR ------
cpo_rx_buffer	= 0x2000fc00
cpo_tx_buffer	= 0x2000fd00
cpo_rx_msg	= 0x2000fe00		@ M0->M4 msg, 1=USB rx, 2=gc mark done
cpo_tx_msg	= 0x2000ff00		@ M4->M0 msg, 1=USB tx, 2/3=strt/stp gc
@ --------- BUFFERS --------
.ifdef run_in_bank2
  BUFFER_START	= 0x10090000+4	@ buffers in on-chip RAM bank 2, after 64KB code
.else
  BUFFER_START	= 0x10010000+4	@ buffers in on-chip RAM bank 1, after 64KB code
.endif
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
@ comment-out lines below to store libs in off-chip SDRAM
@SHARED_LIB_FILE = 1		@ library and file space are both in SPIFI
@LIB_BOTTOM_PAGE = 0x14030000	@ 192KB into SPIFI (after 3x64KB file sects+code
@LIB_TOP_PAGE	= 0x14400000	@ top of SPIFI (4 MB)

@-------10--------20--------30--------40--------50--------60--------70--------80




