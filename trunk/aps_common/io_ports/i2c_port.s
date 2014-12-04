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
@	I2C INPUT/OUTPUT PORT and ISR
@
@-------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input SUPPORT 4  - i2c input  port:	i2cipr, pi2rdy, pi2red
@  II.A.6.6.3. output SUPPORT 4 - i2c output port:	i2copr, pi2wrt
@-----------------------------------------------------------------------------*/

	/* i2c0 port, environment+obarray binding and port model -------------*/
	BNDREG	"i2c0",	i2c0_base
	BNDVAR	"I2C0", vi2c0
	VCTR	vi2c0, reg_i2c0, i2cipr, i2copr

	/* i2c1 port, environment+obarray binding and port model -------------*/
	BNDREG	"i2c1",	i2c1_base
	BNDVAR	"I2C1", vi2c1
	VCTR	vi2c1, reg_i2c1, i2cipr, i2copr

	/* i2c input and output port-vectors	------------------------------*/
	VCTR	i2cipr, i1, val_npofxt, val_npofxt, val_i2crdy, val_i2cred
	VCTR	i2copr, i2, val_npofxt, val_lnkfxt, val_i2cwrt


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* return via lnk */
	PRIMIT	lnkfxt, ufun, 0
	set	pc, lnk

	/* char-ready? function for i2c input port */
	PRIMIT	i2crdy, ufun, 2
	@ on entry:	sv1 <- ((<port> <reg> <n>) . port-vector) = full input port
	@ on exit:	sv1 <- #t/#f indicating port char-ready status
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt
	car	sv1, sv1		@ sv1 <- full input port
	snoc	sv1, sv2, sv1		@ sv1 <- port,		sv2 <- (<reg> <n> ...)
	lsr	rva, sv1, #2		@ rva <- port base address without upper two bits
	lsl	sv1, rva, #4		@ sv1 <- full port base address
	ldr	rvb, =i2c0_base
	eq	rvb, sv1		@ is port i2c0?
	itE	eq
	ldreq	sv5, =I2C0BUFFER	@	if so,  sv5 <- address of i2c0buffer
	ldrne	sv5, =I2C1BUFFER	@	if not, sv5 <- address of i2c1buffer
	tbrfi	rvb, sv5, 1		@ rvb <- data ready status from i2cbuffer[4]
	eq	rvb, #t			@ is i2c data ready?
	b	adr_boolxt

.ifndef	exclude_read_write
		

	/* read function for i2c input port */
	PRIMIT	i2cred, ufun, 2
	@ on entry:	sv1 <- ((<port> <reg> <n>) . port-vector) = full input port
	@ on exit:	sv1 <- object read
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt
	car	sv1, sv1		@ sv1 <- full input port
	snoc	sv1, sv2, sv1		@ sv1 <- port,		sv2 <- (<reg> <n> ...)
	nullp	sv2			@ is (<reg> ...) unspecified (reading as slave)?
	itE	eq
	seteq	sv3, sv2		@	if so,  sv3 <- '() (n is not specified either)
	snocne	sv2, sv3, sv2		@	if not, sv2 <- <reg>,		sv3 <- (<n> ...)
	lsr	rva, sv1, #2		@ rva <- port base address without upper two bits
	lsl	sv1, rva, #4		@ sv1 <- full port base address
	ldr	rvb, =i2c0_base
	eq	rvb, sv1		@ is port i2c0?
	itE	eq
	ldreq	sv5, =I2C0BUFFER	@	if so,  sv5 <- address of i2c0buffer
	ldrne	sv5, =I2C1BUFFER	@	if not, sv5 <- address of i2c1buffer
	nullp	sv2			@ are we reading as slave (i.e. <reg> = '())?
	beq	i2crde			@	if so, jump to process that
	@ reading as master, first write-out mcu-id and registers
i2crd0:	@ wait for i2c channel to be free
	swi	run_no_irq		@ disable interrupts
	tbrfi	rvb, sv5, 0		@ rvb <- status of i2c channel
	eq	rvb, #f			@ is i2c channel not busy?
	beq	i2crd1			@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	i2crd0			@ jump to keep waiting
i2crd1:	@ write target mcu address and num of additional adrs bytes (if any) in i2c buffer[0,4]
	vcrfi	rvb, sv2, 0		@ rvb <- mcu-id (1st element of reg-vector)	
	tbsti	rvb, sv5, 0		@ store i2c write address in i2cbuffer[0]
	veclen	rvb, sv2		@ rvb <- additional number of address bytes + 1 (scheme int)
	sub	rvb, rvb, #4		@ rvb <- additional number of address bytes (scheme int)
	tbsti	rvb, sv5, 1		@ str addtnl num of adrs byts in i2cbfr[4] (dat not rdy)
	eq	rvb, #i0		@ no registers specified?
	beq	i2crd3			@	if so,  (no reg) jump to initiate read transfer	
	@ write additional address bytes (if any) in i2c buffer[8] (backwards)
	set	rva, #0x11		@ rva <-  4, offset to byte in reg-vector (scheme int)
	add	sv4, rvb, #0x20		@ sv4 <- additional number of address bytes (scheme int)
i2crd2:	eq	sv4, #0x21		@ are we done writing additional address bytes?
	beq	i2crd4
	wrdref	rvb, sv2, rva		@ 	if not, rvb <- nxt adrs byt frm reg-vec (sch int)
	lsr	rvb, rvb, #2		@	if not, rvb <- next address byte (raw int)
	sub	sv4, sv4, #4		@	if not, sv4 <- adrs byt ofst in i2cbfr[8] (sch int)
	bytsetu	sv5, sv4, rvb		@	if not, store address byte in i2cbuffer[8+offset]
	add	rva, rva, #0x10		@	if not, rva <- nxt ofst 2 adrs byt in rgvc (sch int)
	b	i2crd2			@	if not, jump to store next additional address byte
i2crd4:	@ initiate i2c write of registers
	set	rva, sv1		@ rva <- port address
	bl	hwi2cr			@ initiate i2c read, as master, (wrt intrnl adrs regs)
i2crd3:	@ start/re-start data transfer (for reading)
	tbrfi	rvb, sv5, 0		@ rvb <- mcu write address from i2cbuffer[0]
	eor	rvb, rvb, #3		@ rvb <- mcu read address as scheme pseudo float)
	tbsti	rvb, sv5, 0		@ store i2c read or write address in i2cbuffer[0]
	@ set number of bytes to receive
	nullp	sv3			@ is there an unspecified number of bytes to receive?
	itEE	eq
	seteq	rvb, sv3		@	if so,  rvb <- '()
	carne	sv3, sv3		@	if not, sv3 <- n
	lsrne	rvb, sv3, #2		@	if not, rvb <- number of bytes to receive (raw int)
	tbsti	rvb, sv5, 3		@ store number of bytes to receive in i2c buffer[12]
	swi	run_normal		@ re-enable interrupts
	set	rva, sv1		@ rva <- port address
	bl	hwi2ni			@ initiate transfer
i2crde:	@ wait for i2c data to be ready
	swi	run_no_irq		@ disable interrupts
	tbrfi	rvb, sv5, 1		@ rvb <- data ready status from i2cbuffer[4]
	eq	rvb, #t			@ is i2c data ready?
	beq	i2crdf			@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	i2crde			@ jump to keep waiting
i2crdf:	@ get data
	ldr	rvb, =i2c0_base		@ rvb <- i2c0 base address
	eq	rvb, sv1		@ is port i2c0?
	set	rvb, #null		@ rvb <- '()
	itTEE	eq
	vcrfieq sv1, glv, 2		@	if so,  sv1 <- object received in glv for i2c0
	vcstieq glv, 2, rvb		@	if so,  store '() in glv (for gc)
	vcrfine sv1, glv, 3		@	if not, sv1 <- object received in glv for i2c1
	vcstine glv, 3, rvb		@	if not, store '() in glv (for gc)
	tbrfi	rva, sv5, 4		@ rva <- number of bytes received
	cmp	rva, #5			@ were more than 4 bytes received?
	bpl	i2crxt			@	if so,  jump to exit
	eq	rva, #4			@ were exactly 4 bytes received?
	it	eq
	tbrfieq sv1, sv5, 2		@	if so,  sv1 <- object received
	beq	i2crxt			@	if so,  jump to exit
	add	rvb, rva, #7		@ rvb <- address of last byte received (i2cbuffer[8 to 10])
	ldrb	rva, [sv5, #8]		@ rva <- MSB byte from i2cbuffer[8]
	strb	rva, [sv5, #11]		@ store MSB in i2cbuffer[11]
	ldrb	rva, [sv5, rvb]		@ rva <- last byte received (LSB)
	strb	rva, [sv5, #8]		@ store LSB in i2cbuffer[8]
	ldrb	rva, [sv5, #11]		@ rva <- MSB back from i2cbuffer[11]
	strb	rva, [sv5, rvb]		@ store MSB at position of last byte received
	ldr	rva, [sv5, #8]		@ rva <- raw data with extraneous upper byte
	bic	rva, rva, #0xFF000000	@ rva <- data
	lsl	rva, rva, #2		@ rva <- data, shifted
	orr	sv1, rva, #int_tag	@ sv1 <- data (scheme int)
i2crxt:	@ exit
	set	rva, #f
	tbsti	rva, sv5, 1		@ store #f (data not ready)   in i2cbuffer[4]
	tbsti	rva, sv5, 0		@ store #f (channel not busy) in i2cbuffer[0]
	swi	run_normal		@ re-enable interrupts
	pntrp	sv1			@ did we receive a packed item?
	it	eq
	seteq	sv2, #null		@	if so,  sv2 <- '() == unpack to heap
	beq	adr_unpack		@	if so,  sv1 <- unpacked-object, return via cnt
	set	pc,  cnt

.endif

.ifndef	exclude_read_write


	/* i2c output port write sub-function
	   write scheme item in sv1 to i2c port in sv2 */
	PRIMIT	i2cwrt, ufun, 2
	@ on entry:	sv1 <- object
	@ on entry:	sv2 <- ((port target n ...) . port-vector) = full output port
	pntrp	sv1
	bne	puti2r
	@ pack object in r1 before sending it out through i2c
	sav_rc	sv2			@ dts <- (full-output-port cnt ...)
	list	sv1, sv1		@ sv1 <- (object)
	call	adr_pack
	restor	sv2, cnt		@ sv2 <- full output port, cnt <- cnt, dts <- (...)
puti2r:	@ continue
	car	sv2, sv2		@ sv2 <- (port target n ...)
	snoc	sv2, sv3, sv2		@ sv2 <- port,		sv3 <- (target n ...)
	lsr	rva, sv2, #2		@ rva <- port base address without upper two bits
	lsl	sv2, rva, #4		@ sv2 <- full port base address
	ldr	rvb, =i2c0_base		@ rvb <- address of i2c base port #0
	eq	rvb, sv2		@ is port i2c0?
	itE	eq
	ldreq	sv5, =I2C0BUFFER	@	if so,  sv5 <- address of i2c0buffer
	ldrne	sv5, =I2C1BUFFER	@	if not, sv5 <- address of i2c1buffer
i2cwr0:	@ wait for i2c channel to be free
	swi	run_no_irq		@ disable interrupts
	tbrfi	rvb, sv5, 0		@ rvb <- status of i2c channel
	eq	rvb, #f			@ is i2c channel not busy?
	beq	i2cwr1			@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	i2cwr0			@ jump to keep waiting
i2cwr1:	@ write object to send in global vector
	ldr	rvb, =i2c0_base
	eq	rvb, sv2		@ is port i2c0?
	itE	eq
	vcstieq glv, 2, sv1		@	if so,  store object in glv, for i2c0, to support gc
	vcstine glv, 3, sv1		@	if not, store object in glv, for i2c1, to support gc
	@ write target mcu address and num additional adrs bytes (if any) in i2c buffer[0,4]
	nullp	sv3			@ is (<reg> ...) unspecified (writing as slave)?
	itE	eq
	seteq	sv4, sv3		@	if so,  sv4 <- '() (n is not specified either)
	snocne	sv3, sv4, sv3		@	if not, sv3 <- target,	sv4 <- (n ...)
	nullp	sv3			@ are we writing as slave (i.e. <reg> = '())?
	itE	eq
	seteq	rvb,  sv3		@	if so,  rvb <- '()
	vcrfine rvb, sv3, 0		@	if not, rvb <- mcu-id (1st element of reg-vector)
	tbsti	rvb, sv5, 0		@ str mcu-id/null in i2cbfr[0] (indcts chnnl now bsy)
	itE	eq
	seteq	rvb, #i0		@	if so,  rvb <- 0 (sch int) addtnl num adrs byts
	veclenne rvb, sv3		@	if not, rvb <- addtnl num adrs byts + 1 (sch int)
	it	ne
	subne	rvb, rvb, #4		@	if not, rvb <- addtnl num adrs byts (sch int)
	tbsti	rvb, sv5, 1		@ str addtnl num of adrs byts in i2cbfr[4] (dat not rdy)
	@ write additional address bytes (if any) in i2c buffer[8] (backwards)
	set	sv1, #0x11		@ sv1 <-  4, offset to byte in reg-vector (scheme int)
	add	rva, rvb,  #0x20	@ rva <- additional number of address bytes (scheme int)
i2cwr2:	eq	rva, #0x21		@ are we done writing additional address bytes?
	beq	i2cwr4
	wrdref	rvb, sv3, sv1		@ 	if not, rvb <- nxt adrs byt frm reg-vec (sch int)
	lsr	rvb, rvb, #2		@	if not, rvb <- next address byte (raw int)
	sub	rva, rva, #4		@	if not, rva <- adrs byt ofst in i2cbfr[8] (sch int)
	bytsetu	sv5, rva, rvb		@	if not, store address byte in i2cbuffer[8+offset]
	add	sv1, sv1, #0x10		@	if not, sv1 <- nxt ofst 2 adrs byt in rgvc (sch int)
	b	i2cwr2			@	if not, jump to store next additional address byte
i2cwr4:	@ write number of bytes to send in i2c buffer[12]
	nullp	sv4			@ is number of bytes to send, <n>, unspecified?
	itT	ne
	carne	sv4, sv4		@	if not, sv4 <- n
	lsrne	rvb, sv4, #2		@	if not, rvb <- number of bytes to send (raw int)
	bne	i2cwr3			@	if not,  jump to store it in i2c buffer[12]
	ldr	rvb, =i2c0_base		@ rvb <- i2c0 port address
	eq	rvb, sv2		@ is port i2c0?
	itE	eq
	vcrfieq sv1, glv, 2		@	if so,  sv1 <- object in glv for i2c0
	vcrfine sv1, glv, 3		@	if not, sv1 <- object in glv for i2c1
	pntrp	sv1			@ is object a pointer?
	itE	ne
	setne	rvb,  #4		@	if not, rvb <- 4 bytes to send (raw int)
	vecleneq rvb, sv1		@	if so,  rvb <- number of bytes in obj (sch int)
	itT	eq
	lsreq	rvb, rvb, #2		@	if so,  rvb <- number of bytes in object (raw int)
	addeq	rvb, rvb, #4		@	if so,  rvb <- number of bytes to send (raw int)
i2cwr3:	tbsti	rvb, sv5, 3		@ store number of bytes to send in i2c buffer[12]
	@ initiate i2c write (if writing as master) and exit
	swi	run_normal		@ re-enable interrupts
	tbrfi	rvb, sv5, 0		@ rvb <- mcu-id/null
	nullp	rvb			@ are we writing as slave?
	itT	ne
	setne	rva, sv2		@	if not, rva <- port address (raw)
	blne	hwi2ni			@	if not, jump to hw-spcfc wrt-as-mstr init routn
	b	adr_npofxt

.endif


/*------------------------------------------------------------------------------
@  II.I.7. I2C ISR:			pi2isr
@-----------------------------------------------------------------------------*/


	/* I2C ISR -- not completed for this new version */
	PRIMIT	i2cisr, ufun, 0
	@
	@ If gc was interrupted, glv may not be valid
	@    => need to test for memory reservation state early on
	@
	@
	@ see if memory transaction or gc was interrupted
	str	rvb, [sp,  #-12]
	ldmia	sp!, {fre}
	tst	fre, #0x02		@ was memory reserved?
	it	eq
	bleq	adr__ism		@	if so,  jump to restart or go-through
	stmdb	sp!, {fre}
	ldr	rvb, [sp,  #-12]
	@ see if interrupt requires zmaloc
	eq	rvb, #i2c0_int_num
	itE	eq
	ldreq	rva, =i2c0_base		@	if so,  rva <- I2C0 base address
	ldrne	rva, =i2c1_base		@	if not, rva <- I2C1 base address
	bl	hwi2st			@ rvb <- i2c status	(rva remains i2c base address)
	ldr	lnk, =i2c0_base
	eq	rva, lnk
	itE	eq
	ldreq	lnk, =I2C0BUFFER
	ldrne	lnk, =I2C1BUFFER
	eq	rvb, #i2c_irm_rcv	@ is status: Receiving as Master w/new dat rcvd?
	it	ne
	eqne	rvb, #i2c_irs_rcv	@	if not, is stat Rcvng as Slv w/new dat rcvd?
	itT	eq
	tbrfieq lnk, lnk, 4		@	if so, lnk <- num dat byts rcvd bfr this one
	eqeq	lnk, #7			@	if so, were 7 bytes received before this one?
	beq	i2c_maloc		@		if so, jump to alloc Heap mem for obj
	@ interrupt doesn't require zmaloc, keep going
	stmdb	sp!, {sv1-sv5}		@ store user mode registers on irq stack
	@ identify i2c channel
	set	sv3, rva		@ sv3 <- i2c[0/1] relevant base address for this interrupt
	ldr	rva, =i2c0_base
	eq	sv3, rva
	itTEE	eq
	seteq	sv1, #12		@	if so,  sv1 <- 12 == offset in glv for I2C0 object
	ldreq	sv2, =I2C0BUFFER	@	if so,  sv2 <- i2c0 buffer address
	setne	sv1, #16		@	if not, sv1 <- 16 == offset in glv for I2C1 object
	ldrne	sv2, =I2C1BUFFER	@	if not, sv2 <- i2c1 buffer address
	bl	i2c_hw_branch
	b	i2cxit

i2c_wm_ini:	@ Writing as Master -- slave has acknowledged address (I2STAT = 0x18)
	bl	gldon
	set	rva, #0			@ rva <- 0 = number of bytes sent
	tbsti	rva, sv2,  4		@ store number of bytes sent (0)
	b	i2c_wm_put
	
i2c_wm_put:	@ Writing as Master -- slave ready to receive data  (I2STAT = 0x28)
	bl	i2putp			@ prologue:	write additional address bytes if needed
	bl	i2putc			@ write data
	bl	i2pute			@ epilogue:	set completion status if needed
	b	i2cxit			@ exit isr
	
i2c_wm_end:	@ Writing as Master -- completed, exit (I2STAT = 0x58)
	set	rvb, #f			@ rvb <- #f
	tbsti	rvb, sv2,  1		@ set data ready to #f
	str	rvb, [glv, sv1]		@ set source object to #f in glv (released for gc)
	bl	hwi2we			@ set channel busy status/stop bit at end of write as master
	bl	gldoff
	b	i2cxit			@ exit isr

i2c_rm_ini: @ Reading as Master -- slave has acknwldgd adrs (I2STAT=0x40, nak if need just 1 byt)
	bl	yldon
	set	rva, #i0		@ rva <- 0 (scheme int)
	tbsti	rva, sv2,  2		@ store 0 as data received so far
	set	rva, #0			@ rva <- 0
	tbsti	rva, sv2,  4		@ store 0 as number of bytes received
	b	i2rmxt

i2c_rm_get: @ Reading as Master, new byte received
	bl	i2getc
i2rmxt: @ exit when reading as master
	tbrfi	rva, sv2,  4		@ rva <- number of bytes read so far
	tbrfi	rvb, sv2,  3		@ rvb <- number of bytes to read
	nullp	rvb			@ undefined number of bytes to read?
	bne	i2rmx1			@	if not, jump to see if only 1 byte left to read
	eq	rva, #1			@ have we received just one byte?
	bne	i2cxit			@	if not, exit
	ldrb	rva, [sv2, #8]		@ rva <- byte received
	and	rva, rva, #0xFF		@ rva <- lower 8 bits of byte received (whole byte really)
	eq	rva, #bytevector_tag	@ are we reading a sized object?
	itT	ne
	setne	rva, #4			@	if not, rva <- 4 (number of bytes to read)
	tbstine rva, sv2,  3		@	if not, store 4 as number of bytes to read
	b	i2cxit			@ exit

i2rmx1:	@ set stop bit/other ending parms, if needed
	sub	rvb, rvb, rva		@ rvb <- number of bytes remaining to read
	eq	rvb, #1			@ only one byte left?
	it	eq
	bleq	i2cstp			@	if so,  jump to end or prepare to end transfer
	b	i2cxit

i2c_rm_end:	@ Reading as Master -- last byte received (I2STAT = 0x58)
	bl	i2getc
	bl	hwi2re			@ set stop bit if needed
	b	i2c_rs_end

i2c_ws_ini:	@ Writing as Slave -- address recognized as mine (I2STAT = 0xA8)
	bl	gldon
	tbrfi	rva, sv2, 0		@ rva <- channel-busy status
	eq	rva, #f			@ is channel free?
	itTTT	eq
	seteq	rva, #i0		@	if so,  rva <- 0 (scheme int)
	tbstieq rva, sv2, 0		@	if so,  str 0 (sch int) as channel-busy
	tbstieq rva, sv2, 1		@	if so,  str 0 (sch int) as dat-not-rdy/#adrs-byts
	ldreq	rva, =eof_char		@	if so,  rva <- eof-character
	itTT	eq
	streq	rva, [glv, sv1]		@	if so,  store eof-character as object to send
	seteq	rva, #4			@	if so,  rva <- 4 (raw int) = number of bytes to send
	tbstieq rva, sv2, 3		@	if so,  store 4 as number of bytes to send
	set	rva, #0x00		@ rva <- 0 = number of bytes sent
	tbsti	rva, sv2, 4		@ store number of bytes sent (0)
	b	i2c_ws_put

i2c_ws_put:	@ Writing as Slave -- master requests byte (I2STAT = 0xB8)
	bl	i2putc
	b	i2cxit			@ exit isr

i2c_ws_end:	@ Writing as Slave -- NAK received from master - i.e. done (I2STAT = 0xC0)
	set	rvb, #f			@ rvb <- #f
	tbsti	rvb, sv2, 0		@ set channel-busy to #f
	bl	gldoff
	b	i2cxit

i2c_rs_ini:	@ Receiving as Slave -- address recognized as mine (I2STAT = 0x60)
	bl	yldon
	set	rva, #0			@ rva <- 0
	tbsti	rva, sv2, 2		@ store 0 as data received so far
	tbsti	rva, sv2, 4		@ store 0 as number of bytes received
	b	i2cxit

i2c_rs_get:	@ Receiving as Slave -- new data received (I2STAT = 0x80)
	bl	i2getc
	b	i2cxit

i2c_rs_end:	@ Receiving as Slave -- STOP or re-START received (I2STAT = 0xA0)
	set	rva, #f			@ rva <- #f
	tbsti	rva, sv2, 0		@ set busy status to #f in i2cbuffer[0]
	set	rva, #t			@ rva <- #t
	tbsti	rva, sv2, 1		@ set data ready to #t in i2cbuffer[4]
	bl	yldoff
	b	i2cxit

i2putc:	@ write to i2c, from buffer or glv
	tbrfi	rvb, sv2, 4		@ rvb <- number of bytes sent
	add	rva, rvb, #1		@ rva <- updated number of bytes sent
	tbsti	rva, sv2, 4		@ store updated number of bytes sent
	tbrfi	rva, sv2, 3		@ rva <- number of bytes to send
	cmp	rva, #4			@ are we sending less than 4 bytes?
	itTTT	mi
	submi	rva, rva, rvb		@	if so,  rva <- offset of byte to send + 1
	submi	rva, rva, #1		@	if so,  rva <- offset of byte to send
	lslmi	rva, rva, #3		@	if so,  rva <- num bits 2 shft to get trgt byt
	ldrmi	rvb, [glv, sv1]		@	if so,  rvb <- scheme int containing bytes to send
	itTT	mi
	lsrmi	rvb, rvb, #2		@	if so,  rvb <- raw int containing bytes to send
	lsrmi	rvb, rvb, rva		@	if so,  rvb <- raw int, shftd 2 get byt in bits 0-7
	andmi	rva, rvb, #0xFF		@	if so,  rva <- byte to send
	bmi	i2put1			@	if so,  jump to send byte
	cmp	rva, #5			@ are we sending more than 4 bytes?
	itE	mi
	addmi	rva, glv, sv1		@	if not, rva <- address of object in glv
	ldrpl	rva, [glv, sv1]		@	if so,  rva <- address of object from glv
	ldrb	rva, [rva, rvb]		@ rva <- next byte of object
i2put1:	strb	rva, [sv3, #i2c_thr]	@ put next data byte in I2C data register
	set	pc,  lnk

i2getc:	@ get character from i2c data register and store it in i2cbuffer or glv
	tbrfi	rvb, sv2, 4		@ rvb <- number of data bytes received before this one
	add	rva, rvb, #1		@ rva <- updated number of bytes received
	tbsti	rva, sv2, 4		@ store updated number of received bytes in i2cbuffer[16]
	cmp	rvb, #8			@ were 8 bytes already received?
	itE	mi
	addmi	rva, sv2, #8		@	if not, rva <- storage start address in i2cbuffer
	ldrpl	rva, [glv, sv1]		@	if so,  rva <- storage start address in glv
	add	rvb, rvb, rva		@ rvb <- address at which to store new byte
	ldrb	rva, [sv3, #i2c_rhr]	@ rva <- received byte from I2C data register
	strb	rva, [rvb]		@ store the byte in memory	
	set	pc,  lnk

i2c_maloc: @ Receiving as Master or Slave -- 7 bytes received
	@ store new byte, allocate Heap memory for object
	@ and update number of bytes to read
	ldmia	sp!, {fre}
	stmdb	sp,  {rva}		@ save i2c[0/1] base address on stack
	ldr	lnk, =i2c0_base
	eq	rva, lnk		@ is it an I2C0 interrupt?
	itE	eq
	ldreq	rvb, =I2C0BUFFER
	ldrne	rvb, =I2C1BUFFER
	ldrb	rva, [rva, #i2c_rhr]	@ rva <- received byte from I2C data register
	strb	rva, [rvb, #15]		@ store the byte in memory			
	tbrfi	rva, rvb, 4		@ rva <- number of data bytes received before this one
	add	rva, rva, #1		@ rva <- updated number of bytes received
	tbsti	rva, rvb, 4		@ store updated number of received bytes in i2cbuffer[16]
	ldr	rva, [rvb, #8]		@ rva <- tag of sized object
	lsr	rva, rva, #8		@ rva <- number of data bytes in sized-object (raw int)	
	add	rvb, rva, #4		@ rvb <- number of bytes to allocate, with header
	bl	adr__alo		@ rva <- address of sized object
	add	fre, rva, rvb		@ fre <- address of next free cell (level 2 reserved)
	orr	fre, fre, #0x02		@ fre <- de-reserved
	ldmdb	sp,  {rvb}		@ rvb <- i2c[0/1] base address restored from stack
	stmdb	sp!, {fre}
	stmdb	sp!, {sv1-sv5}		@ store user mode registers on irq stack
	set	sv3, rvb		@ sv3 <- i2c[0/1] base address
	ldr	rvb, =i2c0_base
	eq	sv3, rvb
	itTEE	eq
	streq	rva, [glv, #12]		@	if so,  store address of sized object in glv
	ldreq	sv2, =I2C0BUFFER	@	if so,  sv2 <- i2c0 buffer address
	strne	rva, [glv, #16]		@	if not, store address of sized object in glv
	ldrne	sv2, =I2C1BUFFER	@	if not, sv2 <- i2c1 buffer address
	@ next 2 lines shouldn't be necessary anymore if i2cmlc threshold is lowered to 4 bytes
	tbrfi	rvb, sv2, 3		@ rvb <- 1st data word of object
	str	rvb, [rva,  #4]		@ store it into sized object
	tbrfi	rvb, sv2, 2		@ rvb <- tag of sized object
	str	rvb, [rva]		@ store tag into sized object
	lsr	rvb, rvb, #8		@ rvb <- number of data bytes in sized-object (raw int)
	add	rvb, rvb, #4		@ rvb <- number of bytes in object, with header
	tbsti	rvb, sv2, 3		@ store num byts to read (total, raw) in i2cbfr[12]
	b	i2cxit

i2cxit:	@ standard i2c exit
	bl	hwi2cs			@ clear SI, if needed
	@ restore registers and return
	ldmia	sp!, {sv1-sv5}
	b	adr__isx		@ jump to exit isr (simple)

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



