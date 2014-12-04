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

	/* LPC 2800 */

.ifdef LPC_2800

  .include "LPC_2800/usb_hw_local.s"

.endif

	/* LPC 4300 */

.ifdef LPC_4300

  .include "LPC_4300/usb_hw_local.s"

.endif

	/* Other MCUs */

.ifndef usbhwReset

/*------------------------------------------------------------------------------

		preliminary items

------------------------------------------------------------------------------*/

	/* helper macro */
  .macro wrtcmd val
	set	rvb, \val
	bl	wrtcmd
  .endm

	/* helper macro */
  .macro rdcmd val
	set	rvb, \val
	bl	rdcmd
  .endm

	/* define usb_icd_full_bit if needed */
.ifdef usb_icd_full
  .ifndef usb_icd_full_bit
	.set bit_pos, 0
	find_bit_pos_for usb_icd_full
	.if bit_pos < 32
	  .set usb_icd_full_bit, bit_pos
	.endif
  .endif
.endif

	/* define usb_icc_empty_bit if needed */
.ifdef usb_icc_empty
  .ifndef usb_icc_empty_bit
	.set bit_pos, 0
	find_bit_pos_for usb_icc_empty
	.if bit_pos < 32
	  .set usb_icc_empty_bit, bit_pos
	.endif
  .endif
.endif


/*------------------------------------------------------------------------------

		hardware configuration

------------------------------------------------------------------------------*/

_func_
usbcfg:	@ configure usb power and pins
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl	= clock selection, enabling, ...
	@ ret:	via lnk
	@ power-up USB RAM
  .ifndef LPC_13xx
	rgcpbt	env, #0xc4, 31, 1	@ PCONP <- power USB RAM, clock, etc...
  .endif
	@ 1. set USB parameters ------------------------------------------------
	write	fre,  USB_CHUNK, #0x00	@ zero bytes remain to send at startup
  	@ 4. configure USB clocks and power ------------------------------------
  .ifdef LPC_2000
    .ifndef USBClkCtrl
	@  turn on PLL1 at 48 MHz for USB clock (see 4-8)
	write	PLL1_PM_parms, 0xE01FC000, #0xa4 @ PLL1CFG <- div/mult 48MHZ USB
	write	sv1,  rva, #0xa0	@ PLL1CON <- enable PLL1
	write	0xaa, rva, #0xac	@ PLL1FEED <- feed PLL1
	write	0x55, rva, #0xac	@ PLL1FEED <- feed PLL1
	rgwfbt	rva, #0xa8, 10, 1	@ wait for PLL locked
	write	sv3,  rva, #0xa0	@ PLL1CON <- connect PLL1
	write	0xaa, rva, #0xac	@ PLL1FEED <- feed PLL1
	write	0x55, rva, #0xac	@ PLL1FEED <- feed PLL1
    .endif
  .endif
  .ifdef USBClkCtrl
	@  turn on USB device clock (see 9-2-1)  (4. using port1 for dev = dflt)
	write	0x12, USBClkCtrl, #0x00	@ enable USB clock and AHB clock
	rgwfbm	rva, #4, 0x12
  .endif
  .ifdef LPC_13xx
	@ configure and enable the USB PLL and USB clock
	write	sv1,  env, #0x48	@ USBPLLCLKSEL <- sys osc is PLL input
	write	fre,  env, #0x4C	@ USBPLLCLKUEN <- enable  clk sel update
	write	sv1,  env, #0x4C	@ USBPLLCLKUEN <- perform clk sel update
	write	0x23, env, #0x10	@ USBPLLCTRL   <- PLL 192/4=48MHz/4=xtal
	rgcpbt	env, #0x0238, 8, 0	@ PDRUNCFG     <- ~#x0100 = power up PLL
	rgwfbt	env, #0x14, PLOCKbit, 1	@ wait for USB PLL locked
	write	fre,  env, #0xC0	@ USBCLKSEL <- USB PLL is USB clk input
	write	fre,  env, #0xC4	@ USBCLKUEN <- enable  clk sel update
	write	sv1,  env, #0xC4	@ USBCLKUEN <- perform clk sel update
	@ power up the USB subsystem
	rgcpbt	env, #0x0238, 10, 0	@ PDRUNCFG     <- ~#x0400 = pwr PAD/PHY
	rgcpbt	env, #0x80,   14, 1	@ SYSAHBCLKCTRL <- power up USB REG
	write	sv1, env, #0xC8		@ USBCLKDIV <- enab USB clk, divisor = 1
  .endif
	@ 5. disable all USB interrupts ----------------------------------------
  .ifndef LPC_13xx
	write	fre, USBIntSt, #0x00
	write	fre, usb_base, #0x04	@ USBDevIntEn
	write	fre, rva,      #0x34	@ USBEpIntEn
  .else
	write	0x01ff, usb_base, #usb_iclear_dv
	write	fre, rva, #0x04		@ disable control 0,1 and EP 4,5 ints
  .endif
	@  6. Configure pins ---------------------------------------------------
  .ifdef LPC_2000
    .ifndef USBClkCtrl
	@ Set PINSEL1 to enable USB VBUS and the soft connect/good link LED
	@  VBUS=15:14(P0.23)->01, UP_LED=31:30(P0.31)->01, CONNECT=31:30->10
	rgrmw	PINSEL1, #0, 0xC000C000, 0x4000	@ VBUS: unplug=>CON_CH DEV_STAT
	rgcpbf	io0_base, #io_dir, 29,32,0b111	@ P0.29-31 <-gpio out,USB psdcon
    .endif
  .endif
  .ifdef USBClkCtrl
	@ enable USB D-, D+, USB_UP_LED1, USB-Connect (GPIO)
	rgcpbf	PINSEL1, #0x00, 26, 30, 0b0101	@ P0.29,30 = USB1(device) D-,D+
	rgcpbf	PINSEL3, #0x00, 4,6,0b01 @ set P1.18 to USB_UP_LED
	rgcpbf	rva,     #0x40, 4,8,0x2	 @ PINMODE3 <- P1.18 USB_UP_LED no pull
    .ifdef LPC2478_STK
	@ configure USB_CONNECT P1.19 as GPIO out
	rgrmw	rva, #0x00, 0xc0, xxx	@ P1.18 USB_UP_LED, P1.19 GPIO USB_CONN
	rgcpbt	rva, #0x40, 7, 1	@ PINMODE0 <- disable pull-up/down
	rgcpbt	io1_base, #io_dir, 19,1	@ P1.19 <- gpio out (USB pseudo-connect)
    .else @ LPC_17xx
	@ configure USB_CONNECT P2.9 as GPIO out
	rgcpbf	PINSEL4, #0x40,18,20,0b10 @ PINMODE4 <- P2.9 USB CONNECT no pull
	rgcpbt	io2_base, #io_dir, 9, 1	  @ P2.9 gpio out (USB pseudo-connect)
    .endif
  .endif
  .ifdef LPC_13xx
	write	sv1, iocon_pio, #0x2c	@ P0.3 <- VBUS function
	write	sv1, rva,       #0x4c	@ P0.6 <- CONNECT function
  .endif
	@ 7. configure EP0 and interrupts --------------------------------------
  .ifndef LPC_13xx
	@  Set Endpoint index & MaxPacketSize regs for EP0,EP1, & wait until
	@  EP_RLZED bit in the Dev int status reg is set so EP0/1 are realized.
	write	fre,  usb_base, #0x48	@ USBEpInd     -- USBEpIndEP_INDEX <- 0
	write	0x08, rva,      #0x4c	@ USBMaxPSize  -- MAXPACKET_SIZE <- 8
	rgwfbt	rva, #0x00, 8, 1	@ USBDevIntSt  -- wait for EP_RLZED_INT
	write	1<<8, rva,      #0x08	@ USBDevIntClr -- clear EP_RLZD_INT
	write	sv1,  rva,      #0x48	@ USBEpInd     -- EP_INDEX <- 1
	write	0x08, rva,      #0x4c	@ USBMaxPSize  -- MAXPACKET_SIZE <-8
	rgwfbt	rva, #0x00, 8, 1	@ USBDevIntSt  -- wait for EP_RLZED_INT
	write	1<<8, rva,      #0x08	@ USBDevIntClr -- clear EP_RLZD_INT
	@  Clear, then Enable, all Endpoint interrupts
	write	-1,   rva,      #0x38	@ USBEpIntClr  -- EP_INT_CLR=0xFFFFFFFF
	write	rvb,  rva,      #0x34	@ USBEpIntEn   -- EP_INT_EN =0xFFFFFFFF
	@  Clr Dev Ints, then Enable DEV_STAT, EP_SLOW, EP_FAST, FRAME
	write	rvb,  rva,      #0x08	@ USBDevIntClr -- DEV_INT_CLR=0xFFFFFFFF
	write	0x0c, rva,      #0x04	@ USBDevIntEn <- DEV_STAT, EP_SLOW int
	@  Install USB int handler in VIC table and enab USB interrut in VIC.
    .ifdef LPC2478_STK
	ldr	rvb, =genisr
	write	rvb, int_base, #0x0158	@ VICVectAddr22 <- IRQ handler = genisr
    .endif

  .endif
	@ 8. dflt USB adrs=0x0 & Set Adrs in protoc engin (twice cf.manual) ----
	set	rvc, lnk		@ rvc <- lnk, saved against wrtcmd
	wrtcmd	0xD00500		@ set-address cmd (0x0500 = wrt command)
	wrtcmd	0x800100		@ dev enab adrs zero (0x80) (0x0100=wrt)
	wrtcmd	0xD00500		@ set-address cmd (0x0500 = wrt command)
	wrtcmd	0x800100		@ dev enab adrs zero (0x80) (0x0100=wrt)
	@ 9. Set CON bit to 1 to make SoftConnect_N active
	wrtcmd	0xFE0500		@ execute get/set device status command
	wrtcmd	0x010100		@ set device status to connected (0x01)
	@ 10. Set AP_Clk high so that USB clock does not disconnect on suspend
	wrtcmd	0xF30500		@ execute get/set mode command
	wrtcmd	0x010100		@ set mode to "no suspend" (0x01)
	@ connect/enable USB interrupts
  .ifndef LPC_13xx
	write	1<<31,  USBIntSt, #0x00	@ activate USB ints (connect to VIC)
  .else
	write	0x0266, usb_base, #0x04	@ enable control 0,1 and EP 4,5 ints
  .endif
	@ return
	set	lnk, rvc		@ lnk <- restored
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwDeviceStatus: @ return device status in rvb
	write	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt
	set	sv4, lnk
	wrtcmd	0xFE0500		@ get the device status
	rdcmd	0xFE0200		@ read the device status to rvb
	set	pc,  sv4

_func_
usbhwReset:
 .ifndef usb_reep
	write	0xffff, usb_base, #usb_iclear_dv @ clr USB ints (esp. txpcktend)
	write	0x266,  rva,      #0x04	@ enable interrupts
 .endif
	set	pc,  lnk

_func_
usbhwRemoteWakeUp:
	set	sv4, lnk
	wrtcmd	0xFE0500		@ get ready to set the device status
	wrtcmd	0x010100		@ set dev stat to 0x01 (remote wakeup)
	set	pc,  sv4

/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwEndpointStatus: @ return endpoint status in sv3
 .ifdef usb_reep
	read	sv2, usb_base, #usb_istat_ep		@ sv2 <- EP Int Status
	write	sv2, rva,      #usb_iclear_ep 		@ clear the interrupt
	rgwfbt	rva, #usb_istat_dv, usb_icd_full_bit, 1	@ wait for data rdy
	read	sv3, rva,      #usb_cmd_data  		@ sv3 <- cmd dat/EP stat
	set	pc,  lnk
 .else
	@ modifies sv2, sv3, sv4, sv5, rva, rvb, env
	set	sv4, lnk
	set	sv2, sv1		@ sv2 <- endpoint interrupts
	and	rvb, sv1, #0x01fe
	write	rvb, usb_base, #usb_iclear_dv @ clear USB interrupt
	set	sv5, UsbControlOutEP
	tst	sv2, #usbCO_ibit	@ is interrupt for Control OUT EP ?
	itT	eq
	seteq	sv5, UsbControlInEP
	tsteq	sv2, #usbCI_ibit	@ is interrupt for Control IN EP ?
	itT	eq
	seteq	sv5, UsbBulkOutEP
	tsteq	sv2, #usbBO_ibit	@ is interrupt for Bulk Out EP ?
	itT	eq
	seteq	sv5, UsbBulkInEP
	tsteq	sv2, #usbBI_ibit	@ is interrupt for Bulk IN EP ?
	it	eq
	seteq	pc, sv4
	bl	usbhwEPSet		@ rvb <- EP frmtd for cmnd wrt (phys ep)
	orr	rvb, rvb, #0x400000	@ rvb <- full cmnd to set sel EP/clr int
	bic	env, rvb, #0x500	@ env <- phys endpoint, shifted, saved
	bl	wrtcmd			@ select endpoint
	orr	rvb, env, #0x200	@ rvb <- command to read status
	bl	rdcmd			@ rvb <- cmd/data (read status)
	set	sv3, rvb
	set	pc,  sv4
 .endif

/* BULK IN Enpoint Interrupt Response */

_func_
usbhwBIe: @ clear the txendpkt interrupt
	and	env, sv1, #usb_itxendp
	write	env, usb_base, #usb_iclear_dv @ clear USB interrupt reg
	set	pc,  lnk

/* BULK OUT Enpoint Interrupt Response */

_func_
usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb
	@ returns via:	lnk
	read	rvb, usb_base, #usb_ibulkin @ rvb <- USBDevIntSt
	tst	rvb, #usb_txrdy		@ is Bulk IN EP already ready to Tx
	itT	ne
	writene	sv1, rva, #usb_iclear_dv @ clear USB interrupt
	setne	pc,  lnk		@ 	if so,  return (good to go)
	set	cnt, 0			@ cnt <- 0 (0 bytes to init write)
	set	env, UsbBulkInEP	@ env <- Bulk IN EP (phys = 5, log = 2)
	b	wrtEPU

/* CONTROL IN Enpoint Interrupt Response */


/* CONTROL OUT Enpoint Interrupt Response */

_func_
usbhwSetAddress: @ Set Device to Address in sv5
	wrtcmd	0xD00500		@ execute Set Address command
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	lsr	rvb, rvb, #16		@ rvb <- address = val(16)
	orr	rvb, rvb, #0x80		@ rvb <- address ored with Dev Enab 0x80
	lsl	rvb, rvb, #16		@ rvb <- address/enable shifted
	orr	rvb, rvb, #0x0100	@ rvb <- command to set address (part 2)
	bl	wrtcmd			@ Set the address
	b	usbSIx			@ jump to Status IN Phase and exit

_func_
usbhwConfigure: @ Configure the device
	@ Realize the Interrupt In Endpoint (phys 3, log 1, aka 0x81)
	set	sv4, lnk
 .ifdef usb_reep
	set	sv5, 3
	bl	usbhwReEP
 .endif
	wrtcmd	0x430500		@ set endpoint status ...
	wrtcmd	0x100			@ ... to 0
	@ Realize the BULK OUT Endpoint (phys 4, log 2, aka 0x02)
 .ifdef usb_reep
	set	sv5, 4
	bl	usbhwReEP
 .endif
	wrtcmd	0x440500		@ set endpoint status ...
	wrtcmd	0x100			@ ... to 0
	@ Realize the BULK IN Endpoint (phys 5, log 2, aka 0x82)
 .ifdef usb_reep
	set	sv5, 5
	bl	usbhwReEP
 .endif
	wrtcmd	0x450500		@ set endpoint status ...
	wrtcmd	0x100			@ ... to 0
	@ configure device
	wrtcmd	0xD80500		@ set device configuration status ...
	wrtcmd	0x010100		@ ... to 1
	set	pc,  sv4

 .ifdef usb_reep
usbhwReEP: @ realize endpoint in sv5 (raw int)
	@ modifies:	rva, rvb
	ash	rvb, 1, sv5
	read	rva, usb_base,#usb_reep	@ rva <- current content of ReEP reg
	orr	rvb, rva, rvb		@ bit for EP 3
	write	rvb, usb_base,#usb_reep	@ OR EP with current val of realized reg
	write	sv5, rva,    #usb_epind	@ Load EP index Reg with physical EP num
	cmp	sv5, #4
	itE	mi
	setmi	rvb, 0x08		@ rvb <-  8 bytes=max pkt siz for EP3
	setpl	rvb, 0x64		@ rvb <- 64 bytes=max pkt siz for EP4,5
	write	rvb, rva, #usb_maxpsize	@ load the max packet size Register
	rgwfbt	rva, #usb_istat_dv, 8,1	@ wait for EP Reallized bit set
	write	0x100, rva, #usb_iclear_dv @ Clear the EP Realized bit
	set	pc,  lnk

 .endif

_func_
usbhwDeconfigure: @ Deconfigure the device
	set	sv4, lnk		@ sv4 <- lnk, saved
	wrtcmd	0xD80500		@ set device configuration status ...
	set	rvb, 0x100		@ rvb <- data for command = 0
	set	lnk, sv4		@ lnk <- lnk, restored
	b	wrtcmd			@ ... to 0, return via lnk

/* Status IN/OUT responses */


/* Enpoint stalling, unstalling */

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	wrtcmd	0x410500		@ set EP status
	wrtcmd	0x010100		@ ... , EP_ST == 1);
	b	usbEPx

_func_
usbhwStallEP: @ Stall EP in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rvb <- EP format for cmnd writing phys
	orr	rvb, rvb, #0x400000	@ rvb <- full command to set stat of EP
	bl	wrtcmd			@ set status of EP ...
	wrtcmd	0x010100		@ ... to 1 = stalled
	set	pc,  sv4

_func_
usbhwUnstallEP:	@ Unstall the EndPoint in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rvb <- EP format for cmnd writing phys
	orr	rvb, rvb, #0x400000	@ rvb <- full command to set stat of EP
	bl	wrtcmd			@ set status of EP ...
	wrtcmd	0x000100		@ ... to 0 = not stalled	
	set	pc,  sv4

/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

/* 12/05/2014 modification (LPC-P1343 doesn't enumerate otherwise) */
.ifdef	LPC_13xx
  .ltorg
.endif
/*
.ifdef	LPC_13xx
  @ adjustment for LPC 1300 such that rdEP starts on 2-byte boundary
  .balign	16
  .space	2
.endif
*/

_func_
rdEP:	@ (eg. section 9.13) uses rva, rvb, env, dts, cnt, returns cnt = count
	@ env <- EPNum, dts <- buffer
	@ set read_enable bit, and endpoint to use in control register
	and	rvb, env, #0x0F		@ rvb <- logical endpoint number
	lsl	rvb, rvb, #2		@ rvb <- logical endpoint number, shftd
	orr	rvb, rvb, #0x01		@ rvb <- log ep num shftd ored w/read_en
	write	rvb, usb_base,#usb_ctrl	@ enable reading from endpoint buffer
	nop
	nop
	nop
 .ifndef LPC_13xx
	rgwfbt	rva, #usb_rxplen, 11, 1, cnt @ wait for packet ready (0x800)
 .else
	read	cnt, rva, #usb_rxplen	@ cnt <- contents of Receive Length reg
 .endif
	@ verify that packet is valid
	tst	cnt, #0x400		@ is packet valid ?
	itT	eq
	seteq	cnt, -1			@	if not, set count to -1
	seteq	pc,  lnk		@	if not, return w/count{cnt} = -1
	@ get count from packet
	set	rvb, 0x3FF		@ rvb <- mask to get number of bytes
	and	cnt, cnt, rvb		@ cnt <- number of bytes read
	@ read data
rdEP_1:	read	rvb, rva, #usb_ctrl
	tst	rvb, #0x01		@ is read_enable still asserted ?
	beq	rdEP_2			@	if not, exit read loop
	read	rvb, rva, #usb_rxdata	@ rvb <- word of data read
	write	rvb, dts, #0		@ store it in buffer
	add	dts, dts, #4		@ dts <- updated data storage address
	b	rdEP_1
rdEP_2:	set	dts, lnk		@ dts <- saved lnk
	@ send select endpoint to protocol engine
	set	sv5, env
	bl	usbhwEPSet		@ rvb <- EP format for cmnd writing phys
	bl	wrtcmd			@ select the endpoint
	@ issue clear buffer command (0xF2)
	wrtcmd	0xF20500		@ clear the endpoint's receive buffer
	set	pc,  dts		@ return

_func_
wrtEP:	@ (eg. section 9.14) uses rva, rvb, env, dts, cnt
	@ env <- EPNum, dts <- buffer, cnt <- cnt
	@ set write_enable bit, and endpoint to use in control register
	and	rvb, env, #0x0F		@ rvb <- logical endpoint number
	lsl	rvb, rvb, #2		@ rvb <- logical endpoint number shifted
	orr	rvb, rvb, #0x02		@ rvb <- log ep shftd ored w/write_enab
	write	rvb, usb_base,#usb_ctrl	@ enable writing to endpoint buffer
	nop
	nop
	nop
	write	cnt, rva, #usb_txplen	@ set the number of bytes to be sent
	nop
	nop
	nop
	@ write data packet to send
wrtEP1:	read	rvb, rva, #usb_ctrl
	tst	rvb, #0x02		@ is write_enable still asserted ?
	beq	wrtEP2			@	if not, exit write loop
 .ifdef cortex
	read	rvb, dts, #0		@ rvb <- next data word
 .else
	read8	rvb, dts, #3		@ rvb <- byte 3 of next data word
	read8	sv5, dts, #2		@ sv5 <- byte 2 of next data word
	orr	rvb, sv5, rvb, lsl #8	@ rvb <- bytes 3 and 2 combined
	read8	sv5, dts, #1		@ rvb <- byte 1 of next data word
	orr	rvb, sv5, rvb, lsl #8	@ rvb <- bytes 3, 2 and 1 combined
	read8	sv5, dts, #0		@ rvb <- byte 0 of next data word
	orr	rvb, sv5, rvb, lsl #8	@ rvb <- full data word
 .endif
	write	rvb, rva, #usb_txdata	@ write data to Transmit register
	add	dts, dts, #4		@ dts <- updated data source address
	b	wrtEP1
wrtEP2:	set	dts, lnk		@ dts <- saved lnk
	@ send select endpoint command to protocol engine
	set	sv5, env
	bl	usbhwEPSet		@ rvb <- EP format for cmnd write phys
	bl	wrtcmd			@ select the endpoint
	@ issue validate buffer command (0xFA)
	wrtcmd	0xFA0500		@ validate the EP's transmit buffer
	set	pc,  dts		@ return

_func_
wrtEPU:	@ (eg. section 9.14)
	@ env <- EPNum, dts <- buffer, cnt <- cnt
	set	sv4, lnk
	bl	wrtEP
	bic	sv1, sv1, #usb_itxendp	@ exclude Txendpkt bit from int clearing
	write	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt
	set	pc,  sv4

/* helper functions */

_func_
usbhwEPSet: @ get EP into proper format for writing cmd to engine	
	@ on entry:	sv5 <- EP
	@ on exit:	rvb <- EP formated for command writing
	@ modifies:	rvb
	@ returns via:	lnk
	and	rvb, sv5, #0x0F			@ rvb <- EP logical number
	lsl	rvb, rvb, #1			@ rvb <- EP physical number (if even)
	tst	sv5, #0x80			@ is this an IN enpoint (odd) ?
	it	ne
	addne	rvb, rvb, #1			@	if so,  rvb <- EP physical index
	lsl	rvb, rvb, #16			@ rvb <- command shifted
	orr	rvb, rvb, #0x0500		@ rvb <- full command to set status of EP
	set	pc,  lnk

_func_
wrtcmd:	@ write command/data from rvb to USB protocol engine (uses sv5, rva, rvb)
	@ modifies:	sv5, rva, rvb
	set	sv5, usb_icc_empty|usb_icd_full	@ sv5 <- CCEMTY and CDFULL bits
	write	sv5, usb_base, #usb_iclear_dv	@ Clear CCEMTY & CDFULL bits
	write	rvb, rva,      #usb_cmd_code
	rgwfbt	rva, #usb_istat_dv, usb_icc_empty_bit,1	@ wait for cmd procssd
	write	usb_icc_empty, rva, #usb_iclear_dv	@ clear CCEMTY bit
	set	pc,  lnk			@ return

_func_
rdcmd:	@ read command data, cmd in rvb, result in rvb (uses sv5, rva, rvb)
	@ always follows a wrtcmd (never used alone)
	@ modifies:	sv5, rva, rvb
	write	rvb, usb_base, #usb_cmd_code	@ CMD_CODE -> protocol engine
	rgwfbt	rva, #usb_istat_dv, usb_icd_full_bit, 1	@ wait for cmd dat rdy
	read	rvb, rva,      #usb_cmd_data	@ rvb <- command data
	set	sv5, usb_icd_full		@ sv5 <- CDFULL
	write	sv5, rva,      #usb_iclear_dv	@ clear the CDFULL bit
	set	pc,  lnk			@ return

/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	read	rvc, usb_base, #usb_ibulkin	@ rvb <- USB Int Stat
	tst	rvc, #usb_txrdy			@ is bulkin EP already rdy to Tx
	itT	eq
	seteq	rvc, usbBI_ibit		@ 	if not, rvc <- EP2 Tx Int bit
	writeeq	rvc, rva, #usb_iset_dv	@ 	if not, USBEPIntSet <- Bulk IN
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

.ltorg

.endif	@ .ifndef usbhwReset



