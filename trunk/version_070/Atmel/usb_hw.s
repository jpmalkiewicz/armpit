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
@
@ Contributions:
@
@     This file includes contributions by Robbie Dinn, marked <RDC>
@
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------

		hardware configuration

	Note:	on AT91SAM9, internal 1.5 K pull-up, bit 30 in USB_PUCR,
	        #xffffee34, is set by BootROM

------------------------------------------------------------------------------*/

_func_
usbcfg:	@ configure usb power and pins
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- PMC_base
	@ ret:	via lnk
	@ initialization of USB device controller
	write	fre, USB_CHUNK, #0x00	@ zero bytes remaining to snd at startup
  .ifdef AT91_SAM7
	ldr	rvb, =genisr
	write	rvb, AIC_SVR11, #0	@ set genisr as USB isr
  .endif
  .ifdef cortex
	write	sv1, env, #0x38		@ PMC_USB   <- PLLB output as USB clock
  .endif
	write	(1<<7), env, #0x00		 @ PMC_SCER  <- enable USB clock
  .if usb_per_id < 32
	write	1<<(usb_per_id%32), env, #0x0010 @ PMC_PCER  <- enab USB periph
  .else
	write	1<<(usb_per_id%32), env, #0x0100 @ PMC_PCER1 <- enab USB periph
  .endif
	@ clear interrupts and configure control endpoint
	write	0xffff, usb_base, #0x20	@ UDP_ICR   <- clear USB interrupts
	write	0x8000, rva, #0x30	@ UDP_CSR0  <- rvb  <- enab Control EP
	write	0x0100, rva, #0x08	@ UDP_FADDR <- enab tfer on address 0
	write	0xFF0F, rva, #0x10	@ UDP_IER   <- enable USB ints (0-3)
  .ifndef cortex
	write	   fre, rva, #0x74	@ UDP_TXVC  <- enable transceiver
  .else
	write	 0x200, rva, #0x74	@ UDP_TXVC  <- ena trans+connect 1.5Kint
  .endif
  .ifdef SAM7_P256
	@ OLIMEX SAM_P256 requires USB pullups to be set under software control.
	write	0x10100, pioa_base, #0	@ PIO_PER  <- PA8, PA16		<RDC>
	write	    rvb, rva, #io_dir	@ PIO_OER  <- PA8, PA16	output	<RDC>
	write	    rvb, rva, #0x64	@ PIO_PUDR <- PA8, PA16 pull-up	<RDC>
	write	    rvb, rva, #io_set	@ PIO_SODR <- PA8, PA16	set (hi)<RDC>
	write	0x10000, rva, #io_clear	@ PIO_CODR <- PA16 clear (lo)	<RDC>
  .endif @ SAM7_P256
	@ return
	set	pc,  lnk		@ return


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/


usbhwReset: @ reset usb hardware
	set	rva, usb_base
usbDS0:	write	0x8000, rva, #usb_csr0		@ rvb <- enable, Control EP
	read	rvb,    rva, #usb_csr0
	eq	rvb, #0x8000
	bne	usbDS0
	write	0x0F,   rva, #usb_ier		@ enable USB interrupts (0-3)
	write	0xFFFF, rva, #usb_iclear_dv	@ clear USB interrupts
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

usbhwEndpointStatus: @ get status of EP whose interrupt is in sv2 into sv3
	read	sv2, usb_base, #usb_istat_ep	@ sv2 <- EP Int Status (eisr)
	write	sv2, rva,      #usb_iclear_ep	@ clear the interrupt
	and	rvb, sv2, #0x0F
	cmp	rvb, #2
	it	pl
	addpl	rvb, rvb, #0x04
	and	rvb, rvb, #0x0C
	add	rva, rva, rvb
	read	sv3, rva, #usb_csr0
	set	pc,  lnk

/* BULK IN Enpoint Interrupt Response */

usbhwBIe: @ Bulk IN EP isr entry: clear the txcomp int
	read	env, usb_base, #usb_ibulkin
	bic	env, env, #0x01			@ txcomp
	write	env, rva,      #usb_ibulkin
	set	pc,  lnk

_func_
usbhwBIw:
	write	sv1, usb_base, #usb_iclear_dv	@ clr USB int reg (DEV_INT_CLR)	
	b	wrtEPU				@ write data from dts to host

usbhwBIx: @ Bulk IN EP isr exit (special)
	b	usbixt


/* BULK OUT Enpoint Interrupt Response */

usbhwBOe: @ Bulk OUT EP isr entry: clear int
	write	sv1, usb_base, #usb_iclear_dv	@ clr USB int reg (DEV_INT_CLR)
	set	pc,  lnk

_func_
usbhwBOw: @ initiate input data echo (if needed) through Bulk IN EP
	@ modifies:	rva, rvb
	@ returns via:	lnk
	read	rvb, usb_base, #usb_ibulkin @ rvb <- UDP_CSR3, BulkIN EP3 stactl
	tst	rvb, #0x01		@ is txcomp set?
	it	eq
	tsteq	rvb, #usb_txrdy		@	if not, is txpktrdy set?
	itT	eq
	orreq	rvb, rvb, #usb_txrdy	@	if not, rvb <- Txpktrdy bit
	writeeq	rvb, rva, #usb_ibulkin	@	if not, USB sta/ctl <- pkt rdy
	beq	usbhwBOw		@	if not, jmp back to verify bit
	set	pc,  lnk		@ return (transfer via usbBIi)
	

/* CONTROL IN Enpoint Interrupt Response */

usbhwCIw: @ Control IN interrupt response
	@ write data to Cntl In EP (wrt cnt bytes starting at dts to ep in env)
	eq	cnt, #0
	beq	usbCIZ
	write	sv1, usb_base, #usb_iclear_dv	@ clr USB int reg (DEV_INT_CLR)
	b	wrtEPU	
usbCIZ: read	rvb, usb_base, #usb_csr0
usbCIY:	bic	rvb, #0xFF
	write	rvb, rva,      #usb_csr0
	read	sv4, rva,      #usb_csr0
	tst	sv4, #0xFF
	bne	usbCIY
	write	sv1, rva,      #usb_iclear_dv	@ clr USB int reg (DEV_INT_CLR)
	set	pc,  lnk

/* CONTROL OUT Enpoint Interrupt Response */

usbhwSetup: @ Control OUT Interrupt, Setup Phase
	@ (eg. section 9.13) uses rva, rvb, env, dts, cnt, returns cnt = count
	@ env <- EPNum, dts <- buffer
	set	rva, usb_base
	set	env, 0
rdSTP1:	eq	env, #8
	itTT	ne
	read8ne	 rvb, rva, #usb_fdr0
	write8ne rvb, dts, env
	addne	env, env, #1
	bne	rdSTP1
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	tst	rvb, #0x80		@ dir frm dev to host (bit 7 in reqtyp)?
	beq	usbST5
usbST1:	rgcpbt	usb_base, #usb_csr0, 7, 1
	read	sv4, rva, #usb_csr0
	eq	sv4, rvb
	bne	usbST1
	b	usbST3
usbST5:	rgcpbt	usb_base, #usb_csr0, 7, 0
	read	sv4, rva, #usb_csr0
	eq	sv4, rvb
	bne	usbST5
usbST3:	rgcpbt	rva, #usb_csr0, 2, 0
	read	sv4, rva, #usb_csr0
	eq	sv4, rvb
	bne	usbST3
	set	pc,  lnk

usbhwDGD: @ send specified device descriptor
	eq	cnt, #0
	beq	usbhwSOx
	@ clear current interrupt
	write	sv1, usb_base, #usb_iclear_dv @ clr USB int reg (DEV_INT_CLR)
	bl	wrtEPU			@ write packet wo/wait for or clr txcomp
	write	0xFF, USB_SETUP_BUFFER, #0
	b	usbixt
usbhwSOx: @ Prepare setup buffer for Status OUT Phase 
	write	0x00, USB_SETUP_BUFFER, #0
	set	env, UsbControlOutEP	@ env <- Control OUT EndPoint
	set	dts, USB_DATA		@ dts <- buffer
	bl	rdEP
	b	usbEPx

usbhwSetAddress: @ Set Device to Address in sv5
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	set	dts, USB_DATA		@ dts <- buffer
	set	cnt, 0x00		@ cnt <- 0 bytes to send
	bl	wrtEP			@ write 0 bytes to EP
	read	sv5,USB_SETUP_BUFFER,#0	@ sv5 <- reqtyp(8), request(8), val(16)
	lsr	sv5, sv5, #16		@ sv5 <- address = val(16)
	orr	rvb, sv5, #0x100	@ rvb <- adrs ored w/Device Enab (0x100)
	write	rvb,usb_base,#usb_faddr	@ set address
	write	1,  rva,  #usb_glbstate	@ set FADDEN bit
	b	usbEPx

usbhwConfigure: @ Configure the device
	@ configure USB
hwcnf1:	write	0x8700, usb_base, #usb_csr1	@ enable, interrupt IN endpoint
	read	sv4,    rva,      #usb_csr1
	eq	sv4, rvb
	bne	hwcnf1
hwcnf2:	write	0x8200, rva, #usb_csr2		@ enable, bulk OUT endpoint
	read	sv4,    rva, #usb_csr2
	eq	sv4, rvb
	bne	hwcnf2
hwcnf3:	write	0x8600, rva, #usb_ibulkin	@ enable bulk IN endpoint
	read	sv4,    rva, #usb_ibulkin
	eq	sv4, rvb
	bne	hwcnf3
	write	2,      rva, #usb_glbstate	@ set CONFG bit
	set	pc,  lnk

usbhwDeconfigure: @ Deconfigure the device
	rgcpbt	usb_base, #usb_glbstate, 1, 0	@ clear CONFG bit
	set	pc,  lnk

/* Status IN/OUT responses */

usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	b	usbCIi


/* Enpoint stalling, unstalling */

usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	read	rvb, usb_base, #usb_csr0
	tst	rvb, #0x20
	itT	eq
	orreq	rvb, rvb, #0x20	
	writeeq	rvb, rva, #usb_csr0	@ forcestall
	beq	usbStall
	rgwfbt	rva, #usb_csr0, 3, 1	@ wait for stallsent
hwstl1:	read	rvb, rva, #usb_csr0
	tst	rvb, #0x28
	itT	ne
	bicne	rvb, #0x28		@ forcestall, stallsent
	writene	rvb, rva, #usb_csr0
	bne	hwstl1
	b	usbEPx

/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write

------------------------------------------------------------------------------*/

_func_
rdEP:	@ (eg. section 9.13)
	@ on entry:	env <- EPNum, dts <- buffer
	@ uses rva, rvb, env, dts, cnt, returns cnt = count
	set	rva, usb_base		@ rva <- USB base register
	add	rva, rva, env, LSL #2	@ rva <- Stat/Ctl register for EP
	read	rvb, rva, #usb_csr0	@ rvb <- EP status
	tst	rvb, #0x42		@ is rxdatabk0 or rxdatabk1 set?
	beq	rdEP			@	if not, jump back to wait
	lsr	cnt, rvb, #16		@ cnt <- bytecount
	set	env, 0
rdEP1:	eq	env, cnt
	itTT	ne
	read8ne	 rvb, rva, #usb_fdr0
	write8ne rvb, dts, env
	addne	env, env, #1
	bne	rdEP1
	set	rvb, usb_base		@ rvb <- USB base register
	sub	env, rva, rvb
	read	rvb, rva, #usb_csr0
	tst	rvb, #0x02
	it	ne
	setne	env, 0x02
	tst	rvb, #0x40
	it	ne
	orrne	env, env, #0x40
	tst	rvb, #0x01
	it	ne
	orrne	env, env, #0x01
rdEP6:	read	rvb, rva, #usb_csr0
	tst	rvb, env
	itT	ne
	bicne	rvb, rvb, env		@ rxdatabk0, rxdatabk1, txcomp
	writene	rvb, rva, #usb_csr0
	bne	rdEP6
	set	pc,  lnk

_func_
wrtEP:	@ (eg. section 9.14)
	@ on entry:	env <- EPNum, dts <- buffer, cnt <- cnt
	@ uses rva, rvb, env, dts, cnt
	set	rva, usb_base		@ rva <- USB base register
	add	rva, rva, env, LSL #2	@ rva <- Stat/Ctrl register for EP
	rgwfbt	rva, #usb_csr0, usb_txrdy_bit, 0 @ wait for txpktrdy cleared
	set	env, 0			@ env <- 0 = write char offset
wrtEP1:	eq	env, cnt		@ done writing?
	itTT	ne
	read8ne	 rvb, dts, env		@ 	if not, rvb <- next data word
	write8ne rvb, rva, #usb_fdr0	@	if not, wrt data to Transmit reg
	addne	env, env, #1		@ 	if not, env <- updtd dat src adr
	bne	wrtEP1			@	if not, jump back to write chars
wrtEP6:	read	rvb, rva, #usb_csr0	@ rvb <- content of Stat/Ctrl for EP
	tst	rvb, #usb_txrdy		@ is txpktrdy set?
	itT	eq
	orreq	rvb, rvb, #usb_txrdy	@	if so,  rvb <- sta/ctl w/txpkrdy
	writeeq	rvb, rva, #usb_csr0	@	if so,  set txpktrdy
	beq	wrtEP6			@	if so,  jump back to check stat
	rgwfbt	rva, #usb_csr0, 0, 1	@ wait for txcomp set (Tx done)
wrtEP7:	read	rvb, rva, #usb_csr0	@ rvb <- content of Stat/Ctrl for EP
	tst	rvb, #0x01		@ is txcomp set?
	itT	ne
	bicne	rvb, rvb, #0x01		@ 	if so,  rvb <- sta/ctl wo/txcomp
	writene	rvb, rva, #usb_csr0	@	if so,  clear txcomp in sta/ctl
	bne	wrtEP7			@	if so,  jump back to check stat
	set	pc,  lnk		@ return

_func_
wrtEPU:	@ (eg. section 9.14)
	@ on entry:	env <- EPNum, dts <- buffer, cnt <- cnt
	@ uses rva, rvb, env, dts, cnt
	set	rva, usb_base		@ rva <- USB base register
	add	rva, rva, env,  LSL #2	@ rva <- Stat/Ctrl register for EP
	read	rvb, rva, #usb_csr0	@ rvb <- content of Stat/Ctrl
	tst	rvb, #0x01		@ is txcomp set?
	itT	ne
	bicne	rvb, rvb, #0x01		@ 	if so,  rvb <- clr txcmp sta/ctl
	writene	rvb, rva, #usb_csr0	@	if so,  clear txcomp in register
	bne	wrtEPU			@	if so,  jump back to check stat
	set	env,  0			@ env <- 0 = initial char offset
wrtEU3:	eq	env,  cnt		@ done writing chars?
	itTT	ne
	read8ne	 rvb, dts, env		@ 	if not, rvb <- next data byte
	write8ne rvb, rva, #usb_fdr0	@ 	if not, wrt dat to Transmit reg
	addne	env, env,  #1		@ 	if not, env <- updtd dat src adr
	bne	wrtEU3			@	if not, jump back to write chars
wrtEU4:	read	rvb, rva, #usb_csr0
	tst	rvb, #usb_txrdy
	itT	eq
	orreq	rvb, rvb, #usb_txrdy	@ txpktrdy
	writeeq	rvb, rva, #usb_csr0
	beq	wrtEU4
	set	pc,  lnk

/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	read	rvc, usb_base, #usb_ibulkin @ rvc <- UDP_CSR3, BlkIN EP3 sta/ctl
	tst	rvc, #0x01		@ is txcomp set?
	it	eq
	tsteq	rvc, #usb_txrdy		@	if not, is txpktrdy set?
	itT	eq
	orreq	rvc, rvc, #usb_txrdy	@	if not, rvb <- sta/ctl w/Txpkrdy
	writeeq	rvc, rva, #usb_ibulkin	@	if not, USB stat/ctrl <- pkt rdy
	beq	usbhwrc			@	if not, jump to verify bit set
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return



