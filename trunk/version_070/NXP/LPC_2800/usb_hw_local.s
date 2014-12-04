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
	@ in:	sv1-3	<- 1-3 (raw int)
	@ ret:	via lnk
	@ disable USB clock for now (enable it later)
	write	fre, usb_clken, #0x00	@ disable USB Clock
	@ operational parameters
	write	fre, USB_CHUNK, #0x00
  .ifdef has_HS_USB
	write	fre, USB_FSHS_MODE, #0	@ indicate USB is not yet in HS mode
  .endif
	@ see if USB is plugged in (if not, exit USB setup)
	read	rvb, io7_base, #io_state
	tst	rvb, #0x01
	seteq	pc,  lnk
	@ continue configuration
	write	sv1, usb_clken, #0x00	@ enable USB Clock
  .ifndef has_HS_USB
	write	1<<4, usb_base, #0x84	@ USBTMode <- set FS only operation
  .endif
	write	fre,  usb_base, #0x00	@ USB Device Address <- 0
	write	0xfc,   rva,    #0x10	@ USB Int cfg <- ACK,STALL NYET,some NAK
	write	0x20,   rva, #usb_epind	@ USB EP  INDEX <- select EP 0 SETUP
	write	0xff,   rva,    #0xac	@ USB Dev Inter clear <- clear dev ints
	write	0xffff, rva,    #0xa0	@ USB EP Int clr  <- clr EP ints, Tx/Rx
	write	rvb,    rva,    #0x90	@ USB EP Int enab <- enab bus reset int
	write	0xaa37, rva,    #0x7c	@ unlock USB registers
	write	0xa1,   rva,    #0x8c	@ USB Dev Int enab <- enab bus reset int
	write	0x1C010001, int_base+0x0400, #0x68 @ INT_REQ26 <-   IRQ, prior=1
	write	rvb,    rva,    #0x6c	@ INT_REQ27 <- USB int enab IRQ, prior=1
	write	rvb,    rva,    #0x70	@ INT_REQ28 <- USB int enab IRQ, prior=1
	write	rvb,    rva,    #0x74	@ INT_REQ29 <- USB int enab IRQ, prior=1
	write	0x80, usb_base, #0x00	@ USB Device Address <- 0, enabled
	write	0x89,   rva,    #0x0c	@ Mode <- clk alws on,ints enab,softconn
	@ return
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/

usbhwgetDevEPint: @ special interrupt entry processing for this MCU
	@ on entry:	rva <- USB base address
	@ on exit:	sv1 <- interrupt status w/r EP and/or Device
	read	sv1, rva, #usb_istat_dv
	tst	sv1, #usb_iep_mask	@ is this an Enpoint (Slow) Interrupt?	
	setne	sv3, usbCO_setupbit
	bne	usbCOi			@	if so, jump to process it
	tst	sv1, #usb_idv_mask	@ is this a Device Status Interrupt?
	bne	usbDSi			@	if so, jump to process it
	b	usbEPi			@ jump to process an enpoint interrupt

usbhwReset:
	write	0xaa37, 0x8004107c, #0
  .ifdef has_HS_USB
	write	0, USB_FSHS_MODE, #0	@ indicate that USB is not in HS mode
  .endif
	write	0x20, usb_base, #usb_epind @ USB EP  INDEX <- select EP 0 SETUP
	write	0x08, rva, #usb_reep	@ USB EP  Type  <- control, enabled
	write	0x00, rva, #usb_epind	@ USB EP  INDEX <- select EP 0 SETUP
	write	0x08, rva, #usb_reep	@ USB EP  Type  <- control, enabled
	write	0x01, rva, #usb_epind	@ USB EP  INDEX <- select EP 0 SETUP
	write	0x08, rva, #usb_reep	@ USB EP  Type  <- control, enabled
	write	0x54, rva, #0x10	@ USB Int cfg <- ACK,STALL NYET,some NAK
	write	0xb9, rva, #0x8c	@ USB Dev Int Enab <- rst,susp,rsm,setup
	write	0x03, rva, #0x90	@ USB EP  Int Enable  <- EP 0 Rx and Tx
	write	0x80, rva, #0x00	@ USB Device Address <- 0,device enabled
	set	pc,  lnk

.ifdef has_HS_USB

usbhwRemoteWakeUp:
	@ process change to HS, suspend, resume (do nothing on suspend/resume)
	tst	rvb, #0x20		@ is it a FS to HS interrupt?
	seteq	pc,  lnk		@	if not, return
	write	1, USB_FSHS_MODE, #0x00	@ indic that USB has switched to HS mode
	set	pc,  lnk

.endif
	
/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

usbhwEndpointStatus: @ return endpoint status in r3
	read	sv2, usb_base, #usb_istat_ep  @ sv2 <- Endpoint Interrupt Status
	write	sv2, rva,      #usb_iclear_ep @ clear the interrupt
	set	sv3, 0			   @ sv3 <- 0 (setup treated at isr top)
	set	pc,  lnk

/* BULK IN Enpoint Interrupt Response */


/* BULK OUT Enpoint Interrupt Response */

_func_
usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb
	@ returns via:	lnk
	b	usbBIi			@ jump to write OUT data to IN EP

/* CONTROL IN Enpoint Interrupt Response */


/* CONTROL OUT Enpoint Interrupt Response */

usbhwSetup: @ Control OUT Interrupt, Setup Phase
	@ dts <- buffer
	write	0x20, usb_base, #usb_epind @ USB EP INDEX <- select EP0
	read	cnt, rva, #usb_rxplen	@ cnt <- number of bytes to read
	set	rvb, 0x3FF		@ rvb <- mask to get number of bytes
	and	cnt, cnt, rvb		@ cnt <- number of bytes to read
	read	rvb, rva, #usb_rxdata	@ rvb <- next word read
	write	rvb, dts, #0		@ store next word in bfr
	read	rvb, rva, #usb_rxdata	@ rvb <- next word read
	write	rvb, dts, #4		@ store next word in bfr
	read	rvb, dts, #4
	lsr	rvb, rvb, #16
	eq	rvb, #0
	seteq	env, UsbControlInEP	@ env  <- Control IN EndPoint
	writeeq	env, rva, #usb_epind	@ USB EP  INDEX <- select EP in env
	writeeq	2,   rva, #usb_ctrl	@	if so,  USBECtrl <- Stat IN Phs
	seteq	pc,  lnk
	read	rvb, dts, #0
	tst	rvb, #0x80
	seteq	env, UsbControlOutEP	@	if so,  env  <- Control OUT EP
	setne	env, UsbControlInEP	@	if not, env  <- Control IN  EP
	write	env, rva, #usb_epind	@ USB EP  INDEX <- select EP in env
	rgcpbt	rva, #usb_ctrl, 2, 1	@ USBECtrl <- initiate DATA phase
	set	pc,  lnk		@ return

usbhwDGD: @ get Descriptor of Device
	bl	wrtEP
	b	usbEPx			@ SOx

usbhwSetAddress: @ Set Device to Address in rvb
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	lsr	rvb, rvb, #16		@ rvb <- address = val(16)
	orr	rvb, rvb, #0x80		@ rvb <- adrs ored with Dev Enab (0x80)
	write	rvb, usb_base, #usb_dev_adr
	b	usbSIx			@ jump to Status IN Phase and exit

usbhwConfigure: @ Configure the device
	@ de-realize target endpoints
	set	sv4, lnk
	set	rva, usb_base
	set	sv5, 0x05040302		@ sv5 <- EP realize: 2<-0,3<-0,4<-0,5<-0
	bl	usbhwReEPs		@ de-realize EPs
	@ Realize the Interrupt Out Endpoint (phys 2, log 1, aka 0x01)
	write	0x02, rva, #usb_epind	@ Load EP idx Reg with physical EP num
	write	0x08, rva,#usb_maxpsize	@ set the max packet size
	@ Realize the Interrupt In Endpoint (phys 3, log 1, aka 0x81)
	write	0x03, rva, #usb_epind	@ Load EP idx Reg with physical EP num
	write	0x08, rva,#usb_maxpsize	@ set the max packet size
	@ Realize the BULK OUT Endpoint (phys 4, log 2, aka 0x02)
	write	0x04, rva, #usb_epind	@ Load EP idx Reg with physical EP num
  .ifndef has_HS_USB
	set	rvb, 64
  .else
	set	rvb, USB_FSHS_MODE
	read	rvb, rvb, #0
	eq	rvb, #0
	seteq	rvb, 64
	setne	rvb, 512
  .endif
	write	rvb, rva, #usb_maxpsize	@ load the max packet size Register
	@ Realize the BULK IN Endpoint (phys 5, log 2, aka 0x02)
	write	0x05, rva, #usb_epind	@ Load EP idx Reg with physical EP num
  .ifndef has_HS_USB
	set	rvb, 64
  .else
	set	rvb, USB_FSHS_MODE
	read	rvb, rvb, #0
	eq	rvb, #0
	seteq	rvb, 64
	setne	rvb, 512
  .endif
	str	rvb, [rva, #usb_maxpsize] @ load the max packet size Register
	@ Realize the Interrupt Out Endpoint (phys 2, log 1, aka 0x01)
	@ Realize the Interrupt In Endpoint (phys 3, log 1, aka 0x81)
	@ Realize the BULK OUT Endpoint (phys 4, log 2, aka 0x02)
	@ Realize the BULK IN Endpoint (phys 5, log 2, aka 0x02)
	set	sv5, 0xa5a4b3b2		@ sv5 <- realize:2<x0B,3<x0B,4<x0A,5<x0A
	bl	usbhwReEPs		@ realize EPs
	@ enable interrupts for physical EP 3, 4 and 5 (EP 2 is not used)
	write	0x44, rva, #0x10	@ USB Int config <- NYET/ACK/ACKSTLL int
	write	0x3B, rva, #0x90	@ EP Int Enab <- EP0,1,4,5 Rx,Tx,EP3 Tx
	set	pc,  sv4

usbhwDeconfigure: @ Deconfigure the device
	@ enable interrupts for physical EP 0 only (disab those for EP 3,4,5)
	write	0x03, usb_base, #0x90	@ USB EP  Int Enab <- EP 0 Rx, Tx only
	@ de-realize target endpoints
	set	sv5, 0x05040302		@ sv5 <- EP realize: 2<-0,3<-0,4<-0,5<-0
	b	usbhwReEPs		@ jump to de-realize EPs, return via lnk

usbhwReEPs: @ realize/de-realize EPs
	@ on entry:	rva <- usb_base
	@ on entry:	sv5 <- EPs and cfg vals, eg. #xv5v4v3v2 (4-bit per EP)
	and	rvb, sv5, #0x0F
	write	rvb, rva, #usb_epind	@ Load EP idx Reg with physical EP num
	lsr	sv5, sv5, #4
	and	rvb, sv5, #0x0F
	write	rvb, rva, #usb_reep	@ set EP type to 0, no type, disabled
	lsrs	sv5, sv5, #4
	bne	usbhwReEPs
	set	pc,  lnk

/* Status IN/OUT responses */

usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	b	usbEPx

/* Enpoint stalling, unstalling */

usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	write	1, usb_base, #usb_epind	@ USB EP INDEX <- select EP to stall
	rgcpbt	rva, #usb_ctrl, 0, 1
	b	usbEPx

usbhwStallEP: @ Stall EP in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rva <- usb_base, rvb <- EP control dat
	orr	rvb, rvb, #0x01
	write	rvb, rva, #usb_ctrl
	set	pc,  sv4

usbhwUnstallEP:	@ Unstall the EndPoint in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rva <- usb_base, rvb <- EP control dat
	bic	rvb, rvb, #0x01
	write	rvb, rva, #usb_ctrl
	set	pc,  sv4

/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

rdEP:	@ uses sv5, rva, rvb, env, dts, cnt, returns cnt = count
	@ env <- EPNum, dts <- buffer
	write	env,usb_base,#usb_epind	@ USB EP  INDEX <- select EP in env
	eq	env, #UsbControlOutEP	@ were we reading from  EP 0 SETUP EP?
	readeq	rvb, rva, #usb_ctrl	@	if so,  rvb <- USBECtrl
	orreq	rvb, rvb, #4
	writeeq	rvb, rva, #usb_ctrl	@	if so,  USBECtrl <-init DATA phs
	read	cnt, rva, #usb_rxplen	@ r11 <- num bytes to read
	set	rvb, 0x3FF		@ rvb <- mask to get number of bytes
	and	cnt, cnt, rvb		@ r11 <- number of bytes to read
	add	sv5, cnt, #3		@ r5  <- number of bytes to read + 3
	lsr	sv5, sv5, #2		@ r5  <- number of words to read
	@ read data
rdEP_1:	eq	sv5, #0			@ done reading?
	readne	rvb, rva, #usb_rxdata	@	if not, rvb <- next word read
	writene	rvb, dts, #0x00		@	if not, store next word in bfr
	addne	dts, dts, #4		@	if not, r10 <- updtd storg adrs
	subne	sv5, sv5, #1		@	if not, r5  <- upd num wrd to rd
	bne	rdEP_1			@	if not, jump to keep reading
	@ return
	set	pc,  lnk		@ return

	
wrtEP:	@ uses sv5, rva, rvb, env, dts, cnt
	@ env <- EPNum, dts <- buffer, cnt <- cnt
	@ set endpoint to use in control register
	write	env,usb_base,#usb_epind	@ USB EP  INDEX <- select EP in env
	eq	env, #UsbControlInEP	@ writing to EP0 Control IN EndPoint?
	bne	wrtEPq			@	if not, jump to continue
	read	rvb, rva, #usb_ctrl	@ rvb <- USBECtrl
	tst	rvb, #0x02		@ going to status out phase (data done)?
	eqeq	cnt, #0			@	if so, are we writing 0 bytes?
	beq	wrtEPS			@	if so,  jump to Status OUT
	tst	rvb, #0x02		@ data phase not done?
	orreq	rvb, rvb, #4		@	if so,  yes, we're done
	writeeq	rvb, rva, #usb_ctrl	@	if so,  USBECtrl <- Stat IN phas
wrtEPq:	@ continue/wait
	tst	env, #0x0E		@ writing to EP0?
	beq	wrtEP0			@	if so,  skip bufr rdy chk
	rgwfbt	rva, #usb_ctrl, 5, 0	@ wait for buffer not full
wrtEP0:	@ keep going
	write	cnt, rva, #usb_txplen	@ set number of bytes to write
	add	sv5, cnt, #3		@ sv5  <- number of bytes to write + 3
	lsr	sv5, sv5, #2		@ sv5  <- number of words to write
	@ write data
wrtEP1:	eq	sv5, #0
	it	eq
	seteq	pc,  lnk		@ return
	read8	rvb, dts, #0
	read8	cnt, dts, #1
	orr	rvb, rvb, cnt, lsl #8
	read8	cnt, dts, #2
	orr	rvb, rvb, cnt, lsl #16
	read8	cnt, dts, #3
	orr	rvb, rvb, cnt, lsl #24
	write	rvb, rva, #usb_txdata	@ write word to USB
	add	dts, dts, #4
	sub	sv5, sv5, #1		@ sv5  <- how many words remain to wrt?
	b	wrtEP1

wrtEPS:	@ get ready for status OUT phase
	write	UsbControlOutEP, rva, #usb_epind @ USB EP INDEX <- select EP
	write	2, rva, #usb_ctrl	 @ USBECtrl <- initiate Status phase
	write	0xff,USB_SETUP_BUFFER,#0 @ rvb  <- reqtyp(8),request(8),val(16)
	set	rva, usb_base
	set	pc,  lnk		@ return	

wrtEPU:	@ (eg. section 9.14)
	@ env <- EPNum, dts <- buffer, cnt <- count
	set	sv4, lnk
	bl	wrtEP
	write	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt
	set	pc,  sv4

/* helper functions */

usbhwEPSet: @ get control data for EP in sv5
	@ on entry:	sv5 <- EP
	@ on exit:	rva <- usb_base
	@ on exit:	rvb <- EP control data
	and	rvb, sv5, #0x0F		@ rvb <- EP logical number
	lsl	rvb, rvb, #1		@ rvb <- EP physical number (if even)
	tst	sv5, #0x80
	addne	rvb, rvb, #1		@ rvb <- EP physical index
	write	rvb,usb_base,#usb_epind	@ USB EP INDEX <- select EP
	read	rvb, rva,    #usb_ctrl
	set	pc,  lnk


/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	set	rvc, UsbBulkInEP
	write	rvc, usb_base, #usb_epind @ USB EP  INDEX <- select EP 5
	read	rvc, rva, #usb_ctrl
	tst	rvc, #usb_txrdy		@ is buffer ready (not full)?
	itT	eq
	seteq	rvc, usbBI_ibit		@ 	if not, rvc <- EP2 Tx Int bit
	writeeq	rvc, rva, #usb_iset_ep	@ 	if not, USBEPIntSet <- Bulk IN
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return



