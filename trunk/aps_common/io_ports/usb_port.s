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
@ III.C. COMMON COMPONENTS OF USB I/O and ISR
@
@-----------------------------------------------------------------------------*/

	/* usb port, environment+obarray binding and port model		------*/
	BNDREG	"usb", usb_base
	BNDVAR	"USB", vusb
	VCTR	vusb, reg_usb, usbipr, usbopr

	/* usb input and output port-vectors	------------------------------*/
	VCTR	usbipr, i1, val_npofxt, val_chrrdc, val_chrrdy, val_chpred, true, val_uargc0, val_uargc1, val_uargc2
	VCTR	usbopr, i2, val_npofxt, val_uarwrc, val_chpwrt, val_usbptc


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* usb putc sub-sub-function
	   write raw ascii value in rvb to USB write buffer */
	PRIMIT	usbptc, ufun, 2
	@ on entry:	sv1 <- scheme char or string to write out
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vec) = full out port
	@ on entry:	sv3 <- saved lnk from caller of caller
	@ on entry:	sv4 <- port address
	@ on entry:	sv5 <- saved lnk from caller
	@ on entry:	rvb <- ascii char to write + offset of char in string
	@ preserves:	sv1, sv2, sv3, sv4, sv5, rvb
	@ modifies:	rva, rvc
	@ returns via lnk (through usbhwrc)
	vecref	rva, glv, 5		@ rva <- main buffer
	vecref	rva, rva, WRITE_BF_offset @ rva <- address of WRITEBUFFER
	swi	run_no_irq		@ disable interrupts (user mode)
	vecref	rvc, rva, 0		@ rvc <- num chars in wrt bffr (sch int)
	add	rvc, rvc, #4		@ rvc <- buffer tag w/number of chars+1
	vecset	rva, 0, rvc		@ store updated number of chars
	add	rvc, rva, rvc, lsr #2	@ rvc <- buffer address for char minus 3
	write8	rvb, rvc, #3		@ store new character in buffer
	b	usbhwrc			@ init usb write, re-enab ints, return

/*------------------------------------------------------------------------------
@ USB ISR
@-----------------------------------------------------------------------------*/

	/* interrupt service routine for USB, branched from genisr */
	PRIMIT	usbisr, ufun, 0
	stmdb	sp!, {sv1-sv5, env, dts, glv}	@ store remnng user regs on stk
	@ get and process interrupt
  .ifndef usbhwgetDevEPint
    .ifndef usb_byte_only
	read	sv1, usb_base, #usb_istat_dv @ sv1 <- Device Interrupt Status
    .else
	read8	sv1, usb_base, #usb_istat_dv @ sv1 <- Device Interrupt Status
    .endif
  .else
	set	rva, usb_base
	bl	usbhwgetDevEPint	@ sv1 <- USB int status (EP and/or dev)
  .endif
	set	rvb, usb_iep_mask
	tst	sv1, rvb		@ is this an Enpoint (Slow) Interrupt?
	bne	usbEPi			@	if so, jump to process it
  .ifdef usbhwgetDevint
	bl	usbhwgetDevint		@ sv1 <- USB device (only) int status
  .endif
	tst	sv1, #usb_idv_mask	@ is this a Device Status Interrupt ?
	beq	usbDSx			@ 	if not, jump to exit
usbDSi: @ Process a Device Status Interrupt [internal entry]
  .ifndef usbhwDeviceStatus
	set	rvb, sv1		@ rvb <- device status
  .else
	bl	usbhwDeviceStatus	@ rvb <- device status
  .endif
	tst	rvb, #usb_busreset	@ did we device receive a bus reset?
	beq	usbSUS			@	if not, jump to see if suspend
	bl	usbhwReset
	write	0, USB_CHUNK, #0x00	@ set remaining bytes to send to zero
	b	usbDSx			@ jump to exit
usbSUS:	
  .ifdef usbhwRemoteWakeUp
	tst	rvb, #usb_suspend	@ did device receive a state change?
	it	ne
	blne	usbhwRemoteWakeUp	@	if so,  jump to process it
  .endif
usbDSx: @ clear Device Status Interrupt and exit
  .ifdef usb_iclear_dv			@ MCUs w/interrupt not cleared on read
	bic	sv1, sv1, #usb_itxendp	@ exclude Txendpkt bit from int clearing
    .ifndef usb_byte_only
	write	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt register
    .else
	write8	sv1, usb_base, #usb_iclear_dv @ clear USB interrupt register
    .endif
  .endif
usbixt:	@ exit usb ISR
	@ restore registers and return
	ldmia	sp!, {sv1-sv5, env, dts, glv} @ restore user mode regs from stk
	b	adr__isx		@ jump to exit isr (simple)

usbEPi:	@ Process an endpoint (slow) interrupt
	bl	usbhwEndpointStatus	@ sv2, sv3 <- EP stat (sv3 w/SETUP inf)
	tst	sv2, #usbCO_ibit	@ is interrupt for Control OUT EP ?
	bne	usbCOi			@	if so, jump to process EP0 int
	tst	sv2, #usbCI_ibit	@ is interrupt for Control IN EP ?
	bne	usbCIi			@	if so, jump to process EP1 int
	tst	sv2, #usbBO_ibit	@ is interrupt for Bulk Out EP ?
	bne	usbBOi			@	if so, jump to process EP4 int
	tst	sv2, #usbBI_ibit	@ is interrupt for Bulk IN EP ?
	bne	usbBIi			@	if so, jump to process EP5 int
usbEPx:	@ clear endpoint interrupt and exit
  .ifdef usb_iclear_dv			@ MCUs w/interrupt not cleared on read
	bic	sv1, sv1, #usb_itxendp	@ exclude Txendpkt bit from int clearing
    .ifndef usb_byte_only
	write	sv1, usb_base, #usb_iclear_dvep @ clear USB EP/DV interrupt reg
    .else
	write8	sv1, usb_base, #usb_iclear_dvep @ clear USB EP/DV interrupt reg
    .endif
  .endif
	b	usbixt

usbBOi:	/* Process interrupt for Bulk OUT endpoint */
	@ read data from usb FIFO
  .ifdef usbhwBOe
	bl	usbhwBOe		@ special entry: clr Rx/Bk int
  .endif
	set	env, UsbBulkOutEP	@ env <- Bulk OUT EP (phys = 4, log = 2)
	set	dts, USB_DATA		@ dts <- address of bulk data buffer
	bl	rdEP			@ cnt <- updated num bytes (read data)
	cmp	cnt, #0			@ is data count negative (error)?
	bmi	usbStall		@	if so,  stall
	eq	cnt, #0			@ was no data read?
	beq	usbEPx			@	if so,  exit isr
	@ copy data to read and write buffers
	set	dts, USB_DATA		@ dts <- address of bulk data buffer
	getBUF	env, rva		@ env <- BUFFER_START_n, rva <- temp
	vecref	sv4, env, WRITE_BF_offset @ sv4 <- address of WRITEBUFFER
	vecref	rvc, sv4, 0		@ rvc <- num chars in writ buf (sch int)
	lsr	rvc, rvc, #2
	add	sv4, sv4, #4		@ sv4 <- address of 1st char in WRITEBUF
	vcrfi	env, env, READ_BF_offset @ env <- address of READBUFFER
	vcrfi	rva, env, 0		@ rva <- num chars in buffer (sch int)
	add	env, env, #4		@ env <- address of 1st char in READBUF
	set	sv5, 0
	tst	rva, #i0
	lsr	rva, rva, #2
	beq	usbnec
usb4_0:	cmp	sv5, cnt		@ done gettng chars frm USB blk dat buf?
	bpl	usb4_1			@	if so,  jump to finish up
	ldrb	rvb, [dts, sv5]		@ rvb <- char from USB bulk data buffer
	add	sv5, sv5, #1		@ sv5 <- offset nxt char USB blk dat buf
	eq	rvb, #3
	beq	usbbrk
	eq	rvb, #'\n		@ is byte a newline (lf)
	beq	usb4_0			@	if so,  jump to process chars
	write8	rvb, env, rva		@ store character in READBUFFER
	write8	rvb, sv4, rvc		@ store character in WRITEBUFFER
	add	rva, rva, #1		@ rva <- offset of next char in READBUF
	add	rvc, rvc, #1		@ rva <- offset of next char in WRITEBUF
	eq	rvb, #'\b		@ was byte a backspace?
	bne	usb4_0			@	if not, jump to process chars
usb4_3:	@ process a backspace character
	subs	rva, rva, #2
	it	mi
	setmi	rva, 0
	write8	' , sv4, rvc		@ store space in WRITEBUFFER
	add	rvc, rvc, #1		@ rva <- offset of next char in WRITEBUF
	write8	'\b, sv4, rvc		@ store backspace in WRITEBUFFER
	add	rvc, rvc, #1		@ rva <- offset of next char in WRITEBUF
	b	usb4_0
usb4_1:	@ finish up
	raw2int	rvb, rva
	str	rvb, [env, #-4]		@ update READBUFFER tag
	raw2int	rvb, rvc
	str	rvb, [sv4, #-4]		@ update WRITEBUFFER tag
	bl	usbhwBOw		@ echo the characters read
	b	usbixt			@ exit

usbnec:	@ process chars with no special treatment and no echo
	ldrb	rvb, [dts, sv5]		@ rvb <- char from USB bulk data buffer
	add	sv5, sv5, #1		@ sv5 <- offset nxt char USB blk dat buf
	strb	rvb, [env, rva]		@ store character in READBUFFER
	add	rva, rva, #1		@ rva <- offset of next char in READBUF
	cmp	sv5, cnt		@ done gettng chars frm USB blk dat buf?
	bmi	usbnec			@	if not, jump to get more chars
	lsl	rvb, rva, #2
	orr	rvb, rvb, #f0
	str	rvb, [env, #-4]		@ update READBUFFER tag
	b	usbixt			@ exit

usbbrk:	@ process reception of a break (ctrl-c)
  .ifdef usb_iclear_dv			@ MCUs w/interrupt not cleared on read
	bic	sv1, sv1, #usb_itxendp	@ exclude Txendpkt bit from int clearing
    .ifndef usb_byte_only
	write	sv1, usb_base, #usb_iclear_dvep @ clear USB EP/DV interrupt reg
    .else
	write8	sv1, usb_base, #usb_iclear_dvep @ clear USB EP/DV interrupt reg
    .endif
  .endif
	sub	rva, env, #4 		@ rva <- address of READBUFFER
	ldmia	sp!, {sv1-sv5, env, dts, glv}	@ restor usr mode regs frm stack
	set	rvc, i0
	vecset	rva, 0, rvc
	mvn	rvb, rvb
	add	rvb, rvb, #1
	b	genis0

usbBIi:	/* Process interrupt for Bulk IN endpoint */
  .ifdef usbhwBIe
	bl	usbhwBIe		@ special entry: clr txpktend/txcomp int
  .endif
	set	env, UsbBulkInEP	@ env <- Bulk IN EP (phys = 5, log = 2)
	getBUF	dts, cnt		@ dts <- BUFFER_START_n, cnt <- temp
	vecref	dts, dts, WRITE_BF_offset @ dts <- address of WRITEBUFFER	
	vecref	cnt, dts, 0		@ cnt <- num chars in writ buf (sch int)
	cmp	cnt, #5
	bmi	usbEPx
	add	dts, dts, #4		@ dts <- adrs of 1st char in WRITEBUFFER
	lsr	cnt, cnt, #2
  .ifndef has_HS_USB
	set	rvb, 64
  .else
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	itE	eq
	seteq	rvb, 64
	setne	rvb, 512
  .endif
	cmp	cnt, rvb
	it	pl
	setpl	cnt, rvb
  .ifndef usbhwBIw
	bl	wrtEP			@ write buffer to Bulk IN EP (normal)
  .else
	bl	usbhwBIw		@ write buffer to Bulk IN EP (special)
  .endif
	@ update the write buffer
	getBUF	env, rva		@ env <- BUFFER_START_n, rva <- temp
	vecref	env, env, WRITE_BF_offset @ env <- address of WRITEBUFFER
	vecref	rva, env, 0		@ rva <- num chars in writ buf (sch int)
	lsr	sv5, rva, #2
  .ifndef has_HS_USB
	set	rvb, 64
  .else
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	itE	eq
	seteq	rvb, 64
	setne	rvb, 512
  .endif
	cmp	sv5, rvb
	it	pl
	setpl	sv5, rvb
	rsb	rvb, sv5, rva, LSR #2	@ rvb <- num byts to remain in writ buf
	lsl	rvc, rvb, #2
	orr	rvc, rvc, #i0
	str	rvc, [env]
	add	env, env, #4
	set	sv3, 0			@ sv3 <- number of bytes moved = 0
usb5_0:	cmp	sv3, rvb		@ have we moved all bytes ?
  .ifndef usbhwBIx
	bpl	usbEPx			@	if so,  exit (normal)
  .else
	bpl	usbhwBIx		@	if so,  exit (special)
  .endif
	ldrb	sv4, [env, sv5]		@ sv4 <- byte
	strb	sv4, [env, sv3]		@ store it at earlier index in write buf
	add	sv3, sv3, #1		@ sv3 <- updated number of moved bytes
	add	sv5, sv5, #1		@ sv5 <- updtd offset to next byt to mov
	b	usb5_0

usbCIi:	/* Process interrupt for Control IN Endpoint */
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	read	dts, USB_CHUNK, #4	@ dts <- start address of data to send
	read	cnt, rva,       #0	@ cnt <- how many bytes remain to send
  .ifndef has_HS_USB
	set	rvb, 8
  .else
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	itE	eq
	seteq	rvb, 8
	setne	rvb, 64
  .endif
	cmp	cnt, rvb		@ want to send more than 8/64 bytes?
	itTE	pl
	subpl	sv5, cnt, rvb		@	if so,  sv5 <- remainng num byts
	setpl	cnt, rvb		@	if so,  cnt <- 8/64=num byts snd
	setmi	sv5, 0			@	if not, sv5 <- 0=num byts remain
	str	sv5, [rva]		@ store that in USB_CHUNK
	itE	pl
	addpl	sv5, dts, cnt		@	if so,  sv5 <- adrs remainng dat
	setmi	sv5, dts		@	if not, sv5 <- prior address
	str	sv5, [rva, #4]		@ store that in USB_CHUNK
  .ifndef usbhwCIw
	bl	wrtEPU			@ write buffer to EP0 IN (normal)
  .else
	bl	usbhwCIw		@ write buffer to EP0 IN (special)
  .endif
	b	usbixt			@ jump to exit

usbCOi:	/* Process interrupt for Control OUT Endpoint */
	tst	sv3, #usbCO_setupbit	@ is last rcvd pkt for EP0 a Setup pkt?
	beq	usbDSP			@	if not, jump to DATA/STATUS phas
	write	0, USB_CHUNK, #0x00	@ set remaining bytes to send to zero
	set	env, UsbControlOutEP	@ env <- Control OUT EndPoint
	set	dts, USB_SETUP_BUFFER	@ dts <- Setup
  .ifndef usbhwSetup
	bl	rdEP			@ read SETUP pkt into buffer (normal)
  .else
	bl	usbhwSetup		@ read SETUP pkt into buffer (special)
  .endif
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	read	sv5,rva,             #4	@ sv5 <- index(16), length(16)
	lsrs	cnt, sv5, #16		@ cnt <- num byts to tfer, is it zero?
	beq	usbRQS			@	if so,  jump to process request
	tst	rvb, #0x80		@ is dir from dev to host?
	beq	usbEPx			@	if not, exit
	b	usbRQS			@	if so,  jump to process request

usbDSP:	@ Data/Status Phase
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	eq	rvb, #0xFF		@ is data OUT phase complete?
  .ifndef usbhwStatusOut
	beq	usbSOp			@	if so,  jump to Status OUT phase
  .else
	beq	usbhwStatusOut		@	if so,  jump to Status OUT phase
  .endif
	@ Data OUT Phase
	read	sv5, rva, #4		@ sv5 <- index(16), length(16)
	lsrs	cnt, sv5, #16		@ cnt <- num byts to tfer, is it zero?
	beq	usbEPx			@	if so,  jump to return
	@ continue to usbRQS

usbRQS:	@ process EP0 request
	@ here:		rva <- USB_SETUP_BUFFER
	@ here:		rvb <- reqtyp(8l), request(8h), val(16H)
	@ here:		sv5 <- index(16L), length(16H)
	@ here:		cnt <- num bytes to transfer (length)
	set	rvc, 0xFF7F		@ rvc <- mask for Standard Requests
	and	rvc, rvb, rvc		@ rvc <- possible Standard request
	lsr	rvb, rvb, #16		@ rvb <- value of request
	and	sv5, sv5, #0xFF		@ sv5 <- EP logical number (from index)
	@ process possible Standard Requests
	eq	rvc, #0x0500		@ Set Address of Device Standard req?
	beq	usbhwSetAddress		@	if so,  jump to set the address
	eq	rvc, #0x0600		@ Get Descriptor of Device Standard req?
	beq	usbDGD			@	if so,  jump to process that
	eq	rvc, #0x0900		@ Set Configuration of Device Stndr req?
	beq	usbDSC			@	if so,  jump to process that
	@ process possible Class Requests
	set	sv3, 0x2021
	eq	rvc, sv3		@ Set Line Coding Class request?
	beq	usbCSL			@	if so,  jump to process that
	set	sv3, 0x2221
	eq	rvc, sv3		@ Set Control Line State Class req?
	beq	usbSIx			@	if so,  jump to Status IN Phase
	b	usbStall		@ Stall on unknown request

usbCSL:	@ 6.2.12 (CDC) Set Line Coding Class request
	set	env, UsbControlOutEP	@ env <- Control OUT EndPoint
	set	dts, USB_DATA
	bl	rdEP			@ read int data into buffer (dts)
	cmp	cnt, #0
	bmi	usbStall
	b	usbSIx			@ jump to Status IN Phase and exit

usbDGD:	@ 9.4.3 Get Descriptor of Device Standard request
	@ on entry:	rva <- USB_SETUP_BUFFER address
	@ on entry:	rvb <- descr index(8l), descr type(8h) (val of request)
	@ on entry:	sv5 <- EP logical number (from index)
	@ on entry:	cnt <- num bytes to transfer (length of request)
	and	sv5, rvb, #0xFF		@ sv5 <- descriptor index
	lsr	rvb, rvb, #8		@ rvb <- dscrptr typ (1-dev,2-cfg,3-str,
					@	4-if,5-ep,6-dq,7-osp,8-pow)
  .ifndef has_HS_USB
	ldr	dts, =USB_DeviceDesc	@	if so,  dts  <- adrs of dev desc
  .else
	ldr	rva, =USB_FSHS_MODE
	ldr	rva, [rva]
	eq	rva, #0
	itE	eq
	ldreq	dts, =USB_DeviceDesc	@	if so,  dts  <- FS dev desc adrs
	ldrne	dts, =USB_HS_DeviceDesc	@	if not, dts  <- HS dev desc adrs
  .endif
	set	rva, 0			@ rva <- index = 0
	set	env, 0			@ env <- offset to next desc (init=0)
usbS61:	add	dts, dts, env		@ dts <- address of next descriptor
	ldrb	env, [dts]		@ env <- size of descriptor
	eq	env, #0			@ have we reached end of descrip table?
	beq	usbStall		@	if so, return Stall nothng found
	ldrb	rvc, [dts, #1]		@ rvc <- item at pos 1 in desc (typ)
	eq	rvb, rvc		@ is type = descriptor type ?
	bne	usbS61			@	if not, go to scan rest of descs
	eq	sv5, rva		@ is index = Descriptor index?
	it	ne
	addne	rva, rva, #1		@	if not, rva <- index + 1
	bne	usbS61
	@ send cnt bytes of the descriptor
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
  .ifndef has_HS_USB
	set	rvb, 8
  .else
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	itE	eq
	seteq	rvb, 8
	setne	rvb, 64
  .endif
	add	rvb, rvb, #1	
	cmp	cnt, rvb		@ is packet size > 8 (max packet size)
	sub	rvb, rvb, #1
	itE	pl
	subpl	sv5, cnt, rvb		@	if so,  sv5 <- nmbr bytes remain
	setmi	sv5, 0			@	if not, sv5 <- 0
	write	sv5, USB_CHUNK, #0	@ store that in USB_CHUNK
	itTE	pl
	setpl	cnt, rvb		@	if so,  cnt <- nmbr bytes to Tx
	addpl	sv5, dts, cnt		@	if so,  sv5 <- adrs of next chnk
	setmi	sv5, dts		@	if not, sv5 <- prior address
	write	sv5, rva, #4		@ store that in USB_CHUNK
  .ifndef usbhwDGD
	bl	wrtEP
	b	usbSOx
  .else
	b	usbhwDGD
  .endif

usbDSC:	@ 9.4.7 Set Configuration of Device Standard request
	@ on entry:	rva <- USB_SETUP_BUFFER address
	@ on entry:	rvb <- configuration to set (value of request)
	@ on entry:	sv5 <- EP logical number (from request index)
	and	sv5, rvb, #0xFF		@ sv5 <- config = lower byt of setup val
	eq	sv5, #0			@ is configuration zero ?
	bne	usbS90
	@ de-configure the device
  .ifdef usbhwDeconfigure
	bl	usbhwDeconfigure	@ perform hw-specific usb deconf
  .endif
	@ clear the readbuffer
	getBUF	rva, rvb		@ rva <- BUFFER_START_n, rvb <- temp
	vecref	rva, rva, READ_BF_offset
	vecset	rva, 0, i0
	@ set uart to generate Rx interrupts
	@ (except in MP core where uart is on a different MCU than USB)
  .ifndef enable_a9_mpcore
    .ifndef uart_byte_only
	write	uart_iRx_ena, uart0_base, #uart_ier @ UART0_IER <- enab Rx int
    .else
	write8	uart_iRx_ena, uart0_base, #uart_ier @ UART0_IER <- enab Rx int
    .endif
  .endif
	@ set default i/o port to uart
	@ Note: challenge/issue/hazard/bug -- if this occurs while gc is in
	@	progress, glv will not be valid ... (needs fixing)
	ldr	rvb, =vuart0
	vecset	glv, 4, rvb		@ set default i/o port model to uart0
	set	rvb, 0			@ rvb <- 0, USB status = deconfigured
	b	usbS91			@ jump to finish up
usbS90:	eq	sv5, #1			@ is the selected configuration #1 ?
	bne	usbStall		@	if not, exit with Stall bad conf
	@ stop uart from generating Rx interrupts (noise on shared READBUFFER)
	@ (except in MP core where uart is on a different MCU than USB)
  .ifndef enable_a9_mpcore
    .ifndef uart_byte_only
	write	uart_iRx_dis, uart0_base, #uart_idr @ UART_IDR <- disab Rx int
    .else
	write8	uart_iRx_dis, uart0_base, #uart_idr @ UART_IDR <- disab Rx int
    .endif
  .endif
	@ clear the readbuffer
	getBUF	rva, rvb		@ rva <- BUFFER_START_n, rvb <- temp
	vecref	rva, rva, READ_BF_offset
	vecset	rva, 0, i0
	@ configure the device
	bl	usbhwConfigure		@ perform hw-specific usb config
	@ set default i/o port to usb
	ldr	rvb, =vusb
	vcsti	glv, 4, rvb		@ default input/output port model
	set	rvb, 1			@ rvb <- 1, USB status = configured
usbS91:	@ finish up
	b	usbSIx			@ jump to Status IN Phase and exit

.ifndef usbhwStatusOut
usbSOp:	@ Control OUT Interrupt, Status OUT Phase (default)
	set	env, UsbControlOutEP	@ env <- Control OUT EndPoint
	set	dts, USB_DATA		@ dts <- buffer
	set	cnt, 0			@ cnt <- 0 bytes to read
	bl	rdEP			@ read 0 bytes from EP
	b	usbEPx
.endif

usbSOx:	@ Prepare setup buffer for Status OUT Phase
	write	0xff, USB_SETUP_BUFFER, #0
	b	usbEPx

usbSIx:	@ USB Status IN  exit -- write null packet to Control EP
  .ifndef usbhwSIX
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	set	dts, USB_DATA		@ dts <- buffer
	set	cnt, 0x00		@ cnt <- 0 bytes to send
	bl	wrtEP			@ write 0 bytes to EP
	b	usbEPx
  .else
	b	usbhwSIX		@ jump to hw-specific routine
  .endif


/*------------------------------------------------------------------------------
@
@  USB descriptors and configuration
@
@-----------------------------------------------------------------------------*/

	startBVU8 	USB_DeviceDesc

@ Device Descriptor:
@ ------------------
@	bLength		bDescriptorType bcdUSB(L,H)
@       18 bytes	1 = device	usb 1.10
@.byte   0x12,		0x01,		0x10,	0x01
.byte   0x12,		0x01,		0x00,	0x02

@	bDeviceClass	bDeviceSubClass bDeviceProtocol bMaxPacketSize0
@	2 = CDC		0 = none	0 = std USB	8 bytes
.byte	0x02,		0x00,		0x00,		0x08

@	idVendor(L, H)	idProduct(L, H)	bcdDevice(L,H)
@					release 1.0
.byte	0xFF,	0xFF,	0x05,	0x00,	0x00,	0x01

@	iManufacturer	iProduct	iSerialNumber	bNumConfigurations
@	is in string 1	is in string 2	is in string 3	1 config only
.byte	0x01,		0x02,		0x03,		0x01

@ Configuration Descriptor:
@ -------------------------
@	bLength		bDescriptorType wTotalLength	bNumInterfaces
@	9 bytes		2 = config	100 bytes (L,H)	2 interfaces
.byte	0x09,		0x02,		0x43,	0x00,	0x02

@	bConfigValue	iConfiguration	bmAttributes	bMaxPower
@	config #1	0 = no string	0xC0 = usbpwr	250 x 2 mA
.byte	0x01,		0x00,		0xC0,		0xFA


@ Interface 0 Setting 0 CDC ACM Interface Descriptor:
@ ---------------------------------------------------
@	bLength		bDescriptorType bIntrfcNumber	bAlternateSetting
@	9 bytes		4 = interf	interface 0	setting 0
.byte	0x09,		0x04,		0x00,		0x00

@	bNumEndpoints	bIntrfcClss	bIntrfcSbClss	bIntrfcPrtcl	
@	uses 1 endpnt	2 = CDC		2 = ACM		1 = Hayes modem	
.byte	0x01,		0x02,		0x02,		0x01		

@	iIntrfc
@	0 = no string
.byte	0x00


@ Header Functional Descriptor (CDC):
@ -----------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bcdDCD (L,H)
@	5 bytes		CS_INTERFACE	0 = Header	1.10
.byte	0x05,		0x24,		0x00,		0x10,	0x01

@ ACM Functional Descriptor (CDC):
@ --------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bmCapabilities
@	4 bytes		CS_INTERFACE	2 = ACM	
@.byte	0x04,		0x24,		0x02,		0x02
.byte	0x04,		0x24,		0x02,		0x00

@ Union Functional Descriptor (CDC):
@ ----------------------------------
@	bFunctionLength	bDescriptorType bDescriptorSbTp	
@	5 bytes		CS_INTERFACE	6 = Union	
.byte	0x05,		0x24,		0x06		

@	bMasterInterfce	bSlaveInterface0
@	Interface 0	Interface 1
.byte	0x00,		0x01

@ Call Management Functional Descriptor (CDC):
@ --------------------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bmCapabilities
@	5 bytes		CS_INTERFACE	1 = Call Mgmnt	1 = mgmt on CDC
@.byte	0x05,		0x24,		0x01,		0x01
.byte	0x05,		0x24,		0x01,		0x00

@	bDataInterface
@	interface 1 used for mgmnt
.byte	0x01

@ Endpoint 1 (Interrupt In, notification) Descriptor:
@ ---------------------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP1, IN
.byte	0x07,		0x05,		0x81

@	bmAttributes	wMaxPacketSize	bInterval
@	3 = interrupt	8 bytes	(L,H)	polling interval
@.byte	0x03,		0x08,	0x00,	0x0A
.byte	0x03,		0x08,	0x00,	0x10

@ Interface 1 Setting 0 CDC Data Class Interface Descriptor:
@ ----------------------------------------------------------
@	bLength		bDescriptorType bIntrfcNumber	bAlternateSetting
@	9 bytes		4 = interf	interface 1	setting 0
.byte	0x09,		0x04,		0x01,		0x00

@	bNumEndpoints	bIntrfcClss	bIntrfcSbClss	bIntrfcPrtcl
@	uses 2 endpnts	10 = CDC Data	0 = default	0 = no specific
.byte	0x02,		0x0A,		0x00,		0x00

@	iIntrfc
@	0 = no string
.byte	0x00

@ Endpoint 2 (bulk data OUT, phys=5):
@ ------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP2, OUT
.byte	0x07,		0x05,		0x02

@	bmAttributes	wMaxPacketSize	bInterval
@	2 = bulk	64 bytes (L,H)	bulk EP never NAKs
.byte	0x02,		0x40,	0x00,	0x00

@ Bulk IN Endpoint, LPC2000: 2 (bulk data IN, phys=5), AT91SAM7: 3
@ ----------------------------------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP2, IN
.byte	0x07,		0x05,		usbBulkINDescr

@	bmAttributes	wMaxPacketSize	bInterval
@	2 = bulk	64 bytes (L,H)	bulk EP never NAKs
.byte	0x02,		0x40,	0x00,	0x00

@ String Descriptor 0 (language ID):
@ ----------------------------------
@	bLength		bDescriptorType language ID
@	4 bytes		3 = string	English US (L,H)
.byte	0x04,		0x03,		0x09,	0x04

@ String Descriptor 1: Manufacturer
@ ---------------------------------
@	bLength		bDescriptorType
@	14 bytes	3 = string
.byte	0x0E,		0x03

@	String contents
@	A	r	m	p	i	t
.hword	0x41,	0x72,	0x6D,	0x70,	0x69,	0x74

@ String Descriptor 2: Product
@ ----------------------------
@	bLength		bDescriptorType
@	14 bytes	3 = string
.byte	0x0E,		0x03

@	String contents
@	S	c	h	e	m	e
.hword	0x53,	0x63,	0x68,	0x65,	0x6D,	0x65

@ String Descriptor 3: Version
@ ----------------------------
@	bLength		bDescriptorType
@	8 bytes	3 = string
.byte	0x08,		0x03

@	String contents
@	0	7	0
.hword	0x30,	0x37,	0x30

@ Terminating zero:
@ -----------------
.byte	0x00


	ENDsized

/*------------------------------------------------------------------------------
@
@  USB descriptors and configuration for High-Speed (HS) Mode
@
@-----------------------------------------------------------------------------*/
	
.ifdef	has_HS_USB

	startBVU8	USB_HS_DeviceDesc

@ Device Descriptor:
@ ------------------
@	bLength		bDescriptorType bcdUSB(L,H)
@       18 bytes	1 = device	usb 1.10
@.byte   0x12,		0x01,		0x10,	0x01
.byte   0x12,		0x01,		0x00,	0x02

@	bDeviceClass	bDeviceSubClass bDeviceProtocol bMaxPacketSize0
@	2 = CDC		0 = none	0 = std USB	64 bytes
.byte	0x02,		0x00,		0x00,		0x40

@	idVendor(L, H)	idProduct(L, H)	bcdDevice(L,H)
@					release 1.0
.byte	0xFF,	0xFF,	0x05,	0x00,	0x00,	0x01

@	iManufacturer	iProduct	iSerialNumber	bNumConfigurations
@	is in string 1	is in string 2	is in string 3	1 config only
.byte	0x01,		0x02,		0x03,		0x01

@ Configuration Descriptor:
@ -------------------------
@	bLength		bDescriptorType wTotalLength	bNumInterfaces
@	9 bytes		2 = config	100 bytes (L,H)	2 interfaces
.byte	0x09,		0x02,		0x43,	0x00,	0x02

@	bConfigValue	iConfiguration	bmAttributes	bMaxPower
@	config #1	0 = no string	0xC0 = usbpwr	250 x 2 mA
.byte	0x01,		0x00,		0xC0,		0xFA


@ Interface 0 Setting 0 CDC ACM Interface Descriptor:
@ ---------------------------------------------------
@	bLength		bDescriptorType bIntrfcNumber	bAlternateSetting
@	9 bytes		4 = interf	interface 0	setting 0
.byte	0x09,		0x04,		0x00,		0x00

@	bNumEndpoints	bIntrfcClss	bIntrfcSbClss	bIntrfcPrtcl	
@	uses 1 endpnt	2 = CDC		2 = ACM		1 = Hayes modem	
.byte	0x01,		0x02,		0x02,		0x01		

@	iIntrfc
@	0 = no string
.byte	0x00


@ Header Functional Descriptor (CDC):
@ -----------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bcdDCD (L,H)
@	5 bytes		CS_INTERFACE	0 = Header	1.10
.byte	0x05,		0x24,		0x00,		0x10,	0x01

@ Call Management Functional Descriptor (CDC):
@ --------------------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bmCapabilities
@	5 bytes		CS_INTERFACE	1 = Call Mgmnt	1 = mgmt on CDC
.byte	0x05,		0x24,		0x01,		0x01

@	bDataInterface
@	interface 1 used for mgmnt
.byte	0x01

@ ACM Functional Descriptor (CDC):
@ --------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bmCapabilities
@	4 bytes		CS_INTERFACE	2 = ACM	
.byte	0x04,		0x24,		0x02,		0x02

@ Union Functional Descriptor (CDC):
@ ----------------------------------
@	bFunctionLength	bDescriptorType bDescriptorSbTp	
@	5 bytes		CS_INTERFACE	6 = Union	
.byte	0x05,		0x24,		0x06		

@	bMasterInterfce	bSlaveInterface0
@	Interface 0	Interface 1
.byte	0x00,		0x01

@ Endpoint 1 (Interrupt In, notification) Descriptor:
@ ---------------------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP1, IN
.byte	0x07,		0x05,		0x81

@	bmAttributes	wMaxPacketSize	bInterval
@	3 = interrupt	8 bytes	(L,H)	polling interval
@.byte	0x03,		0x08,	0x00,	0x0A
.byte	0x03,		0x08,	0x00,	0x10

@ Interface 1 Setting 0 CDC Data Class Interface Descriptor:
@ ----------------------------------------------------------
@	bLength		bDescriptorType bIntrfcNumber	bAlternateSetting
@	9 bytes		4 = interf	interface 1	setting 0
.byte	0x09,		0x04,		0x01,		0x00

@	bNumEndpoints	bIntrfcClss	bIntrfcSbClss	bIntrfcPrtcl
@	uses 2 endpnts	10 = CDC Data	0 = default	0 = no specific
.byte	0x02,		0x0A,		0x00,		0x00

@	iIntrfc
@	0 = no string
.byte	0x00

@ Endpoint 2 (bulk data OUT, phys=5):
@ ------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP2, OUT
.byte	0x07,		0x05,		0x02

@	bmAttributes	wMaxPacketSize	bInterval
@	2 = bulk	512 bytes (L,H)	bulk EP never NAKs
.byte	0x02,		0x00,	0x02,	0x00

@ Bulk IN Endpoint, LPC2000: 2 (bulk data IN, phys=5), AT91SAM7: 3
@ ----------------------------------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP2, IN
.byte	0x07,		0x05,		usbBulkINDescr

@	bmAttributes	wMaxPacketSize	bInterval
@	2 = bulk	512 bytes (L,H)	bulk EP never NAKs
.byte	0x02,		0x00,	0x02,	0x00

@ String Descriptor 0 (language ID):
@ ----------------------------------
@	bLength		bDescriptorType language ID
@	4 bytes		3 = string	English US (L,H)
.byte	0x04,		0x03,		0x09,	0x04

@ String Descriptor 1: Manufacturer
@ ---------------------------------
@	bLength		bDescriptorType
@	14 bytes	3 = string
.byte	0x0E,		0x03

@	String contents
@	A	r	m	p	i	t
.hword	0x41,	0x72,	0x6D,	0x70,	0x69,	0x74

@ String Descriptor 2: Product
@ ----------------------------
@	bLength		bDescriptorType
@	14 bytes	3 = string
.byte	0x0E,		0x03

@	String contents
@	S	c	h	e	m	e
.hword	0x53,	0x63,	0x68,	0x65,	0x6D,	0x65

@ String Descriptor 3: Version
@ ----------------------------
@	bLength		bDescriptorType
@	8 bytes	3 = string
.byte	0x08,		0x03

@	String contents
@	0	7	0
.hword	0x30,	0x37,	0x30

@ Terminating zero:
@ -----------------
.byte	0x00

	ENDsized

.endif		@ HS descriptors


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



