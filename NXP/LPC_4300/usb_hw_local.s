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

/*------------------------------------------------------------------------------

		hardware configuration

------------------------------------------------------------------------------*/

_func_
usbcfg:	@ configure usb power and pins
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ ret:	via lnk
  .ifndef use_usb1
	@ set USB0 clock to PLL0USB (480 MHz) and power-up PHY
	write	(7<<24)|(1<<11), CGU_base,#0x60	@ BASE_USB0_CLK <- PLL0USB,autob
	rgcpbt	sys_config, #0x04, 5, 0		@ CREG0 <- USB0 PHY enabled
  .else
	@ set USB1 clock to IDIVB (60 MHz) and cfg VBUS pin + Full-Speed PHY
	write	(0x0d<<24)|(1<<11),CGU_base,#0x68 @ BASE_USB1_CLK <- IDIVB,autob
	write	0x52, SCU_SFSP2_n, #0x14 	  @ P2_5 pin <- VBUS func
	write	0x12, SCU_SFSUSB,  #0x00 	  @ ena USB1 ntrn FS PHY,nrmlPWR
  .endif
	@ initialize USB variables
	write	fre, USB_CHUNK, #0x00	@ zero bytes remain to send at startup
  .ifdef has_HS_USB
	write	fre, USB_FSHS_MODE, #0	@ indicate that USB starts in FS mode
  .endif
	@ reset
	write	sv2, usb_base, #0x40	@ USB_CMD  <- reset
	rgwfbt	rva, #0x40, 1, 0	@ wait for reset done
	@ configure device mode, full-speed (possibly HS)
  .ifndef use_usb1
    .ifndef has_HS_USB
	write	1<<24,    rva, #0x84	@ USB_PORTSC1 <- full-speed only (no HS)
    .endif
	write	0x08,     rva, #0xa4	@ OTGSC    <- device mode pull-down
	write	sv2,      rva, #0xa8	@ USBMODE  <- device mode
  .else
	write	0xc1<<24, rva, #0x84	@ USB_PORTSC1 <- FS PHY, no HS chirp
	write	sv2,      rva, #0xa8	@ USBMODE     <- device mode
  .endif
	@ initialize Queue Heads
	write	usb_queue_heads,rva,#0x58 @ EPLISTADDRESS <- adrs of Queue Heads
	write	sv2,        rva, #0xa8	@ USBMODE  <- Device mode, with lockouts
	write	0x00800080, rva, #0xc0	@ EPCTRL0  <- enab EP 0 Rx, Tx, control
	write	0x20088000, QH_CO, #0	@ QH0 OUT <- set capabilities
	write	fre,        rva,   #4	@ QH0 OUT <- set current dTD
	write	sv1,        rva,   #8	@ QH0 OUT <- set tail
	write	0x20088000, QH_CI, #0	@ QH0 IN  <- set capabilities
	write	fre,        rva,   #4	@ QH0 IN  <- set current dTD
	write	sv1,        rva,   #8	@ QH0 IN  <- set tail
	@ initialize USB device controller
	write	0x41, usb_base, #0x48	@ USB_INTR <- enable usb int, reset int
	write	sv1,  rva,      #0x40	@ USB_CMD  <- enable usb (run)
	@ return
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwDeviceStatus: @ return device status in rvb
	@ on exit:	rvb <- device status
	@ modifies:	rva, rvb
	@ side-effect:	clears usb interrupts (global, not endpoint)
	write	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt
	set	rvb, sv1
	set	pc,  lnk

_func_
usbhwReset:
	read	rvb, usb_base, #0xac	@ rvb <- ENDPTSETUPSTAT
	write	rvb, rva,      #0xac	@ clear EP setups
	read	rvb, rva,      #0xbc	@ rvb <- ENDPTCOMPLETE
	write	rvb, rva,      #0xbc	@ clear EP complete
urstw0:	read	rvb, rva,      #0xb0	@ rvb <- ENDPTPRIME
	eq	rvb, #0
	bne	urstw0
	mvn	rvb, rvb
	write	rvb, rva, #0xb4		@ ENDPTFLUSH <- flush endpoints
	rgwfbt	rva, #usb_istat_dv, 2,1 @ wait for port change detected
	write	0x04, rva, #usb_iclear_dv
	@
	@ we can check attach speed (FS vs HS) at #0x84 USB_PORTSC1 (p. 461)
	@ and then update USB_FSHS_MODE accordingly. but here, we run in full-speed
	@ only (set in LPC_4300_init_io.s).
	@
  .ifdef has_HS_USB
	read	rvb, rva, #0x84		@ rvb <- USB_PORTSC1
	tst	rvb, #(1 << 27)		@ running in HS mode?
	it	ne
	writene	1, USB_FSHS_MODE, #0	@ 	if so,  indicate that USB is now in HS mode
  .endif
	b	hwprCO


/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwEndpointStatus: @ return endpoint status in sv3
	read	sv2, usb_base, #usb_istat_ep	@ sv2 <- Endpoint Interrupt Status (eisr)
	read	sv3, rva,      #0xac		@ sv3 <- EPSetupSTAT
	@ clear selected interrupts
	write	sv2, rva,      #usb_iclear_ep	@ clear the interrupt
	tst	sv3, #1
	it	ne
	orrne	sv2, sv2, #usbCO_ibit
	@ clear control IN interrupt if there's a bulk in/out interrupt
	tst	sv2, #4
	it	eq
	tsteq	sv2, #(4 << 16)
	it	ne
	bicne	sv2, sv2, #(1 << 16)
	set	rva, usb_base
	set	pc,  lnk

/* BULK IN Enpoint Interrupt Response */

_func_
usbhwBIe: @ clear the txendpkt interrupt
	and	env, sv1, #usb_itxendp
	write	env, usb_base, #usb_iclear_dv @ clear USB interrupt register
	set	pc,  lnk

/* BULK OUT Enpoint Interrupt Response */

_func_
usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb
	@ returns via:	lnk
	read	rvb, usb_base, #usb_ibulkin
	tst	rvb, #usb_txrdy		@ txpktrdy
	bne	usbixt			@ exit
	write	dTD_BI, QH_BI,  #0x08	@ store address of dTD in QH next dTD
	write	0,      rva,    #0x0c	@ clear QH STAT
	write	1,      dTD_BI, #0x00	@ set dTD as list tail
	write	(1<<15)|0x80, rva, #4	@ store ctl info in dTD, act, dat0,int
	write	4<<16, usb_base, #0xb0	@ ENDPTPRIME <- prime endpoint
	rgwfbt	rva, #0xb8, 18, 1	@ wait for bit to be set in ENDPTSTATUS
	b	usbixt			@ exit

/* CONTROL IN Enpoint Interrupt Response */


/* CONTROL OUT Enpoint Interrupt Response */

_func_
usbhwSetup: @ Control OUT Interrupt, Setup Phase
	read	rvb, usb_queue_heads, #0x28
	write	rvb, dts,      #0x00
	read	rvb, rva,      #0x2c
	write	rvb, dts,      #0x04
	write	sv3, usb_base, #0xac	@ ENDPTSetupSTAT <- ack/clr setup rcvd
uhwsw0:	read	rvb, rva, #0xac
	eq	rvb, #0
	bne	uhwsw0
	write	(1<<16)|1, rva, #0xb4	@ flush pending cntrl IN/OUT tfr, if any
uhwsw1:	read	rvb, rva, #0xb8
	tst	rvb, #1
	it	eq
	tsteq	rvb, #(1<<16)
	bne	uhwsw1
	set	pc,  lnk

_func_
usbhwDGD: @ 9.4.3 Get Descriptor of Device Standard request
	bl	wrtEP
	bl	hwprCO			@ prime the Control OUT EP
	b	usbSOx

_func_
usbhwSetAddress: @ Set Device to Address in SETUP buffer
	@ modifies:	rva, rvb
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	lsr	rvb, rvb, #16		@ rvb <- address = val(16)	
	lsl	rvb, rvb, #25
	orr	rvb, rvb, #(1 << 24)
	write	rvb, usb_base, #0x54
	b	usbSIx			@ jump to Status IN Phase and exit

_func_
usbhwConfigure: @ Configure the device
	@ modifies:	rva, rvb, rvc, sv4, sv5
	@ Realize the Bulk Out/In Endpoint (phys. 4, 5, log. 2)
	write	(1<<23)|(2<<18)|(1<<7)|(2<<2),usb_base,#0xc8 @ EPCTRL2<-enab EP2
	write	0x20400000, QH_BO, #0	@ QH2 OUT <- no ZLT, 64-byte max packet
	set	rvc, QH_BI
	write	rvb,        rvc, #0x00	@ QH2 IN  <- no ZLT, 64-byte max packet
	write	0,          rva, #0x04	@ QH2 OUT <- set current dTD
	write	rvb,        rvc, #0x04	@ QH2 IN  <- set current dTD
	write	1,          rva, #0x08	@ QH2 OUT <- set tail
	write	rvb,        rvc, #0x08	@ QH2 IN  <- set tail
	@ prime EP0
	@ on entry:	sv5 <- QHn  address for endpoint
	@ on entry:	rva <- dTDn address for endpoint
	b	hwprBO			@ prime Bulk OUT EP, return via lnk

_func_
usbhwDeconfigure: @ Deconfigure the device
	@ modifies:	rva, rvb
	@ side-effects:	uart interrupts, default i/o port, read buffer
	@ disable the Interrupt In and Bulk Out/In EP 2 (phys. 3,4,5 log. 1,2)
	write	(0<<23)|(2<<18)|(0<<7)|(2<<2),usb_base,#0xc8 @ EPCTRL1<-disa EP2
	set	pc,  lnk

/* Status IN/OUT responses */


/* Enpoint stalling, unstalling */

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	write	0x810081, usb_base, #0xc0
	b	usbEPx


/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

_func_
rdEP:	@ (eg. section 9.13) uses rva, rvb, env, dts, cnt, returns cnt = count
	@ on entry:	env <- EPNum
	@ on entry:	dts <- buffer
	@ on exit:	cnt <- number of bytes read
	@ modifies:	rva, rvb, sv5, cnt
	eq	env, #UsbBulkOutEP
	itTEE	eq
	seteq	sv5, QH_BO
	seteq	rva, dTD_BO
	setne	sv5, QH_CO
	setne	rva, dTD_CO
	read	rvb, sv5, #0x00
	read	cnt, rva, #0x04
	lsr	rvb, rvb, #16
	and	rvb, rvb, #0xff
	lsr	cnt, cnt, #16
	and	cnt, cnt, #0xff
	sub	cnt, rvb, cnt
	eq	env, #UsbBulkOutEP
	itE	eq
	seteq	rva, BUFFER64_BO
	setne	rva, BUFFER8_CO
	set	sv5, 0
rdEP_1:	@ read packet into data buffer
	read8	rvb, rva, sv5		@ rvb <- next data byte
	write8	rvb, dts, sv5		@ write data to buffer
	add	sv5, sv5, #1		@ dts <- updated data source address
	cmp	sv5, cnt		@ done?
	bmi	rdEP_1			@	if not, jump back to continue
	eq	env, #UsbBulkOutEP
	bne	hwprCO

_func_
hwprBO: @ prime Bulk OUT endpoint for read
	set	sv5, QH_BO
	set	rva, dTD_BO
	b	hwprimeEPnrd

_func_
hwprCO: @ prime Control Out endpoint for read
	set	sv5, QH_CO
	set	rva, dTD_CO
hwprimeEPnrd: @ continue and [internal entry]
	@ on entry:	sv5 <- QHn  address for endpoint
	@ on entry:	rva <- dTDn address for endpoint
	write	rva, sv5, #0x08		@ store address of dTD in QH next dTD
	write	0,   sv5, #0x0c		@ clear QH STAT
	write	1,   rva, #0x00		@ set dTD as list tail
	read	rvb, sv5, #0x00
	and	rvb, rvb, #0xff0000	@ data count (max for EP)
	orr	rvb, rvb, #(1 << 15)	@ set interrupt on completion for dTD
	orr	rvb, rvb, #0x80		@ dTD status = active
	write	rvb, rva, #0x04		@ store control info in dTD
	set	rvb, QH_BO
	eq	sv5, rvb
	itE	eq
	seteq	rvb, BUFFER64_BO
	setne	rvb, BUFFER8_CO
	write	rvb, rva, #0x08		@ store buffer ptr 0
	set	rvb, QH_BO
	eq	sv5, rvb
	itE	eq
	seteq	rvb, 4
	setne	rvb, 1
	write	rvb, usb_base, #0xb0	@ ENDPTPRIME <- prime endpoint
	@ may want to wait for bit to be set in ENDPTSTATUS here
rdEP_2:	read	sv5, rva, #0xb8
	tst	sv5, rvb
	beq	rdEP_2
	set	pc,  lnk

_func_
wrtEPU:	@ (eg. section 9.14)
	@ on entry:	env <- EPNum
	@ on entry:	dts <- buffer
	@ on entry:	cnt <- cnt
	@ modifies:	rva, sv1, sv4
	set	sv4, lnk
	bl	wrtEP
	bic	sv1, sv1, #usb_itxendp	   @ exclude Txendpkt bit from interrupt clearing
	write	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt
	set	pc,  sv4

_func_
wrtEP:	@ (eg. section 9.14) uses rva, rvb, env, dts, cnt
	@ on entry:	env <- EPNum
	@ on entry:	dts <- data start address (buffer)
	@ on entry:	cnt <- number of bytes to write to USB
	@ set write_enable bit, and endpoint to use in control register
	and	env, env, #0x0F
	eq	env, #1
	beq	wrtEw1
	@ for bulk EP (eg. echo of rcvd chars) wait for prior tranfer to be complete
	rgwfbt	usb_base, #usb_ibulkin, usb_txrdy_bit, 0
wrtEw1:	@ continue
	eq	env, #1
	itTEE	eq
	seteq	sv5, QH_CI
	seteq	rva, dTD_CI
	setne	sv5, QH_BI
	setne	rva, dTD_BI
	write	rva, sv5, #0x08		@ store address of dTD in QH next dTD
	write	0,   sv5, #0x0c		@ clear QH STAT
	write	1,   rva, #0x00		@ set dTD as list tail
	lsl	rvb, cnt, #16		@ set data count for dTD
	eq	cnt, #0			@ transferring zero bytes?
	it	ne
	orrne	rvb, rvb, #(1 << 15)	@ 	if not, set interrupt on completion for dTD
	orr	rvb, rvb, #0x80		@ dTD status = active
	write	rvb, rva, #0x04		@ store control info in dTD
	eq	env, #1
	itE	eq
	seteq	rvb, BUFFER8_CO
	setne	rvb, BUFFER64_BI
	write	rvb, rva, #0x08		@ store buffer ptr 0
	set	rva, rvb
	set	sv5, 0
	@ write data packet to send
wrtEP1:	read8	rvb, dts, sv5		@ rvb <- next data word
	write8	rvb, rva, sv5		@ write data to Transmit buffer
	add	sv5, sv5, #1		@ dts <- updated data source address
	cmp	sv5, cnt
	bmi	wrtEP1
	set	rvb, 1
	lsr	sv5, env, #1
	lsl	rvb, rvb, sv5
	lsl	rvb, rvb, #16
	write	rvb, usb_base, #0xb0	@ ENDPTPRIME <- prime endpoint
	@ may want to wait for bit to be set in ENDPTSTATUS here
wrtEP2:	read	sv5, rva, #0xb8
	tst	sv5, rvb
	beq	wrtEP2
	set	pc,  lnk

/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	read	rvc, usb_base, #usb_ibulkin
	tst	rvc, #usb_txrdy		@ is EP already txpktrdy?
	bne	usbhwrcxt
	set	rvc, rvb		@ rvc <- rvb, saved
	write	dTD_BI, QH_BI,   #0x08	@ store address of dTD in QH next dTD
	write	0,      rva,     #0x0c	@ clear QH STAT
	write	1,      dTD_BI,  #0x00	@ set dTD as list tail
	write	(1<<15)|0x80,rva,#0x04	@ dTD <- stat=act,dat cnt=0,int on cmplt
	write	4<<16, usb_base, #0xb0	@ ENDPTPRIME <- prime endpoint
	rgwfbt	rva, #0xb8, 18, 1	@ wait for bit to be set in ENDPTSTATUS
	set	rvb, rvc		@ rvb <- restored
usbhwrcxt: @ finish up
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return



