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
	@ in:	env	<- scgc_base = SIM_SCGC, periph clken
	@ ret:	via lnk
	@ initialize USBOTG FS
	@ configure clock and enable peripheral
	@ cfg clks (SIM_SOPT2 p.296) module (SCGC4 p.312) and USB div (48MHz)
	swi	run_prvlgd
	write	(4<<1)|1, env, #0x20		@ CLKDIV2 <-USBDIV=4, USBFRAC=1
	rgrmw	sim_base+0x1000, #0x4, 5<<16	@ _SOPT2  <-ena MCGPLLCLK 120MHz
	rgcpbt	env, #0x0c, 18, 1		@ SCGC4   <-ena clock to USBOTG
	swi	run_normal
	@ initialize USB variables and reset controller
	write	fre,  USB_CHUNK, #0x00	@ zero bytes remain to send at startup
	write	fre,  USB_ADRS,  #0x00	@ USB address not received yet
	write8	0x80, usb_base,  #0x10c	@ USB0_USBTRC0 <- reset USB controller
	@ set buffer addresses in BDT RAM space (also allows time for reset)
	ldr	rva, =BDT_address	@ rva <- BDT_address
hwbdt0:	add	rvb, rva, #128
	write	rvb, rva, #0x04		@ BDT <- EP0 OUT/IN ping/pong bfr adrs
	add	rva, rva, #8
	tst	rva, #(1<<6)
	beq	hwbdt0
	add	rvb, rva, #128
	write	rvb, rva, #0x04		@ BDT <- EP2 OUT ping buffer address
	add	rvb, rvb, #64
	write	rvb, rva, #0x0c		@ BDT <- EP2 OUT pong buffer address
	add	rvb, rvb, #64
	write	rvb, rva, #0x34		@ BDT <- EP3 IN  ping buffer address
	add	rvb, rvb, #64
	write	rvb, rva, #0x3c		@ BDT <- EP3 IN  ping buffer address
	@ enable EP0
	write8	0x0d, usb_base, #0xc0	@ USB0_ENDPT0 <- enable Rx,Tx,HSHK,SETUP
	write8	 fre, rva, #usb_daddr	@ USB0_ADDR <- set address to 0
	@ set buffer descriptor table (BDT) address in USB controller
	write8	BDT_address>>8, rva, #0x9c @  _BDTPAGE1 <- bits  15:9 of buf adr
	lsr	rvb, rvb, #8
	write8	rvb, rva, #0xb0		@ USB0_BDTPAGE2 <- bits 23:16 of buf adr
	lsr	rvb, rvb, #8
	write8	rvb, rva, #0xb4		@ USB0_BDTPAGE3 <- bits 31:24 of buf adr
	@ enable controller (set to JSTATE before enab to avoid pseudo-reset)
	write8	0x10, rva, #0x108	@ USB0_CONTROL <- enable DP pullup
	write8	fre,  rva, #0x100	@ USB0_USBCTRL <- release suspend & PD
	write8	sv1,  rva, #0x94	@ USB0_CTL     <- enab USB, device mode
	write8	0x89, rva, #0x84	@ USB0_INTEN   <- enab TOKDNE,RESET ints
	@ return
	set	pc,  lnk		@ return


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwDeviceStatus: @ used here to unstall a possibly stalled EP0
	r8cpbt	usb_base, #0xc0, 1, 0	@ USB0_ENDPT0 <- enable Rx,Tx,HSHK,SETUP
	set	pc,  lnk

_func_
usbhwReset:
	write8	3,      usb_base, #0x94	@ USB0_CTL <- USB enabled (SETUP cleard)
	write8	0x0d,   rva,      #0xc0	@ USB0_ENDPT0 <- enable Rx,Tx,HSHK,SETUP
	write	0x80080,BDT_address, #0	@ BDT <- EP0 OUT, Rx ping rdy, 8byt
	write	rvb,    rva,         #8	@ BDT <- EP0 OUT, Rx pong rdy, 8byt
	write	0,      rva,        #16	@ BDT <- EP0  IN, not rdy for Tx, ping
	write	rvb,    rva,        #24	@ BDT <- EP0  IN, not rdy for Tx, pong
	write8	rvb,usb_base,#usb_daddr	@ USB0_ADDR <- set address to 0
	write8	1,      rva,      #0x94	@ USB0_CTL <- USB enabled (SETUP cleard)
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwEndpointStatus:
	read8	rvc, usb_base, #0x090	@ rvc <- USB0_STAT
	rgbf	rvb, rvc, 4, 8		@ rvb <- Enpoint (0-7)
	ash	sv2, 1, rvb		@ sv2 <- Enpoint interrupt indicator
	tst	rvc, #(1<<3)		@ is this an IN EP?
	it	ne
	lslne	sv2, sv2, #8		@	if so,  sv2 <- adj for IN EP
	read8	sv3, rva, #0x94		@ sv3 <- USB0_CTL, where SETUP bit is
	set	pc,  lnk		@ return


/* BULK IN Enpoint Interrupt Response */

/* BULK OUT Enpoint Interrupt Response */

_func_
usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb
	@ returns via:	usbEPx
	read	rvb, BDT_address+96,#16	@ rva <- EP2-IN, BDT entry, ping side
	tst	rvb, #0x80		@ EP2 Tx ready on ping side?
	it	eq
	writeeq	0x80, rva, #16
	read	rvb,  rva, #24		@ rva <- EP2-IN, BDT entry, pong side
	tst	rvb, #0x80		@ EP2 Tx ready on pong side?
	it	eq
	writeeq	0xc0, rva, #24		@	if not, release EP to SIE (rdy)
	b	usbEPx


/* CONTROL IN Enpoint Interrupt Response */

_func_
usbhwCIw: @ prepare data for Control IN EP
	eq	cnt, #0			@ no data to send?
	beq	usbhwCIw0		@	if so,  jump to check if set adr
	bl	wrtEP			@ write data (prepare EP)
	b	usbEPx			@ return
usbhwCIw0: @ check if setting new address
	read	rvb, USB_ADRS, #0x00	@ rvb <- content of USB_ADRS
	eq	rvb, #0			@ is content zero?
	beq	usbEPx			@	if so,  return
	write8	rvb,usb_base,#usb_daddr	@ USB0_ADDR <- set new device address
	write	0,   USB_ADRS, #0x00	@ USB_ADRS <- clear back to zero
	b	usbEPx			@ return

/* CONTROL OUT Enpoint Interrupt Response */

_func_
usbhwSetAddress: @ Set Device to Address in SETUP buffer
	@ modifies:	rva, rvb, rvc
	@ get address
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	rgbf	sv4, rvb, 16, 32	@ sv4 <- address = val(16)
	write	sv4, USB_ADRS, #0x00
	b	usbSIx

_func_
usbhwConfigure:
	write8	3,    usb_base, #0x94
	write8	0x15, rva,      #0xc4	@ USB0_ENDPT1 <- enable Tx,HSHK
	write8	rvb,  rva,      #0xcc	@ USB0_ENDPT3 <- enable Tx,HSHK
	write8	0x19, rva,      #0xc8	@ USB0_ENDPT2 <- enable Rx,HSHK
	set	rvc,  BDT_address
	write	0,    rvc,    #(32+16)	@ BDT <- EP1  IN, not rdy for Tx, ping
	write	rvb,  rvc,    #(32+24)	@ BDT <- EP1  IN, not rdy for Tx, pong
	write	rvb,  rvc,    #(96+16)	@ BDT <- EP3  IN, not rdy for Tx, ping
	write	rvb,  rvc,    #(96+24)	@ BDT <- EP3  IN, not rdy for Tx, pong
	write	0x400080, rvc, #(64+0)	@ BDT <- EP2 OUT, ready for Rx, ping
	write	rvb,  rvc,     #(64+8)	@ BDT <- EP2 OUT, ready for Rx, pong
	write8	1,    rva,      #0x94
	set	pc,  lnk		@ return


/* Status IN/OUT responses */

_func_
usbhwSIX: @ status IN exit
	write	0xff,USB_SETUP_BUFFER,#0
	write	0,   USB_CHUNK,    #0	@ cnt <- 0 bytes remain to be sent
	write8	3,   usb_base,  #0x94
	write8	0,   BDT_address, #16	@ clear ping BDT entry (for DATA1)
	write8	rvb, rva,         #24	@ clear ping BDT entry (for DATA1)
	write8	1,   usb_base,  #0x94
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	set	dts, USB_DATA		@ dts <- buffer
	set	cnt, 0x00		@ cnt <- 0 bytes to send
	bl	wrtEP			@ write 0 bytes to EP
	b	usbEPx


/* Enpoint stalling, unstalling */

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	b	usbEPx


/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

_func_
rdEP:	@ copy data from EP in USB0_STAT to location in dts, return cnt
	r8bf	rvb, usb_base,#0x90,0,8	@ rvb <- USB0_STAT = EP,Tx/Rx,odd/even
	set	rvc, BDT_address
	add	rvb, rvc, rvb, lsl #1	@ rvb <- transfer info address in BDT
	read	sv4, rvb, #0		@ sv4 <- [CNT,OWN,PID] from BDT, for EP
	rgbf	cnt, sv4, 16, 26	@ cnt <- number of bytes received
	@ in no-echo case (READBUF num-chars is float) avoid inf isr re-entry
	tst	sv4, #0x80
	it	ne
	setne	cnt, 0
	@ get data from packet
	eq	cnt, #0			@ zero data bytes to extract?
	beq	usbhw2			@	if so,  jump to continue
	read	rvc, rvb, #4		@ rvc <- input buffer address from BDT
	set	sv4, cnt
usbhw1:	@ loop to read data
	subs	sv4, sv4, #1
	read8	sv5, rvc, sv4
	write8	sv5, dts, sv4
	bne	usbhw1
usbhw2:	@ release EP to SIE
	set	rvc, 0x80		@ rvc <- OWN bit = 1 (SIE owner)
	eq	env, #UsbBulkOutEP	@ reading from bulk OUT?
	itE	eq
	orreq	rvc, rvc, #(64<<16)	@	if so,  rvc <- 64 bytes
	orrne	rvc, rvc, #( 8<<16)	@	if not, rvc <-  8 bytes
	write	rvc, rvb, #0		@ release EP to SIE
	@ check if SETUP bit needs clearing, if not, return
	read8	rvc, rva, #0x94		@ rvb <- USB0_CTL
	tst	rvc, #usbCO_setupbit	@ did we read-in a SETUP packet?
	it	eq
	seteq	pc,  lnk		@	if not, return
	@ reset ping/pong, clear SETUP bit in USB0_CTL, reset EP0 IN BDT, return
	orr	rvc, rvc, #(1<<1)
	write8	rvc, rva, #0x94		@ USB0_CTL <- ODDRST, reset to ping/even
	set	rvc, BDT_address
	write	0,   rvc,   #16		@ EP0 IN ping BDT <- not ready
	write	rvb, rvc,   #24		@ EP0 IN pong BDT <- not ready
	write8	1,   rva, #0x94		@ USB0_CTL <- enabled, SETUP bit cleared
	set	pc,  lnk		@ return


_func_
wrtEP:	@ copy cnt bytes of data in dts to buffer of EP in env & give EP to SIE
	set	rva, usb_base
	lsl	sv4, cnt, #16		@ sv4 <- data count, shifted, saved
	set	rvc, BDT_address
	add	rvc, rvc, env, lsl #5
	@ dispatch on EP type
	eq	env, #UsbControlInEP
	bne	usbhw5
	@ EP0 -- control
	read8	rvb, rva, #0x90		@ rvb <- USB0_STAT = EP,Tx/Rx,odd/even
	tst	rvb, #(1<<3)
	itT	ne
	mvnne	rvb, rvb
	tstne	rvb, #(1<<2)
	it	ne
	addne	rvc, rvc, #8		@	if so,  rvc <- use pong side
	it	eq
	orreq	sv4, sv4, #(1 << 6)	@ 	if not, sv4 <- dat cnt & DATA1
	b	usbhw6
usbhw5:	@ Bulk IN EP
	read	rvb, rvc, #16		@ rvb <- ping BDT entry
	tst	rvb, #0x80		@ is ping-side busy?
	itT	ne
	readne	sv5, rvc, #24		@	if so,  sv5 <- pong BDT entry
	tstne	sv5, #0x80		@	if so,  is pong-side also busy?
	bne	usbhw5			@	if so,  jump back to wait
	tst	rvb, #0x80		@ is ping-side in use?
	itT	ne
	addne	rvc, rvc, #8		@	if so,  rvc <- use pong side
	orrne	sv4, sv4, #(1 << 6)	@ 	if not, sv4 <- dat cnt & DATA1
usbhw6:	@ common, continue
	eq	cnt, #0
	beq	usbhw8
	read	rvb, rvc, #20		@ rvb <- Tx EP buffer address
usbhw7:	@ loop to write data
	subs	cnt, cnt, #1
	read8	sv5, dts, cnt
	write8	sv5, rvb, cnt
	bne	usbhw7
usbhw8:	@ release EP to SIE and return
	orr	rvb, sv4, #0x80		@ rvb <- data count and OWN = 1
	write	rvb, rvc, #16
	set	pc,  lnk		@ return


/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	set	rvc, BDT_address+96
	read	rva, rvc, #16		@ rva <- EP2-IN, BDT entry, ping side
	tst	rva, #0x80		@ EP2 Tx ready on ping side?
	itT	eq
	seteq	rva, 0x80		@	if not, rva <- release EP to SIE
	writeeq	rva, rvc, #16		@	if not, release EP to SIE (rdy)
	read	rva, rvc, #24		@ rva <- EP2-IN, BDT entry, pong side
	tst	rva, #0x80		@ EP2 Tx ready on pong side?
	itT	eq
	seteq	rva, 0xc0		@	if not, rva <- release EP to SIE
	writeeq	rva, rvc, #24		@	if not, release EP to SIE (rdy)
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return



