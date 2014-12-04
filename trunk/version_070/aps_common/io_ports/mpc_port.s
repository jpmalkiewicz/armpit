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
@	MPC (MP Core) INPUT/OUTPUT PORT and ISR
@
@-------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input SUPPORT 4  - mpc input  port:	mpcipr, mpcrdy, mpcred
@  II.A.6.6.3. output SUPPORT 4 - mpc output port:	mpcopr, mpcwrt
@-----------------------------------------------------------------------------*/

	/* mpc port, environment+obarray binding and port model -------------*/
@!!!!!	BNDREG	"i2c0",	i2c0_base
	BNDVAR	"MPC", vmpc
	VCTR	vmpc, reg_i2c0, mpcipr, mpcopr

	/* mpc input and output port-vectors	------------------------------*/
	VCTR	mpcipr, i1, val_npofxt, val_npofxt, val_mpcrdy, val_mpcred
	VCTR	mpcopr, i2, val_npofxt, val_lnkfxt, val_mpcwrt


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* return via lnk */
	PRIMIT	lnkfxt, ufun, 0
	set	pc, lnk

	/* char-ready? function for mpc input port */
	PRIMIT	mpcrdy, ufun, 2
	@ on entry:	sv1 <- ((<port> <reg> <n>) . port-vector) = full input port
	@ on exit:	sv1 <- #t/#f indicating port char-ready status
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt

	vcrfi	rva, glv, 5		@ rva <- main buffer
/*
	swi	run_prvlgd		@ privil, no irq, for mrc p15
	mrc	p15, 0, rva, c0, c0, 5	@ rva <- Multiproc Affinity reg, MPIDR
	swi	run_normal              @ enable interrupts (user mode)
	and	rva, rva, #3
	ldr	rvc, =MP_mat
	add	rva, rvc, rva, lsl 5
	ldr	rva, [rva, #0x10]	@ rva <- BUFFER_START_n
*/
	add	sv5, rva, #(I2C0_BF_offset << 2)
	tbrfi	rvb, sv5, 1		@ rvb <- data ready status from i2cbuffer[4]
	eq	rvb, #t			@ is i2c data ready?
	b	boolxt


.ifndef	exclude_read_write
		

	/* read function for mpc input port */
	PRIMIT	mpcred, ufun, 2
	@ on entry:	sv1 <- ((<port> <reg> <n>) . port-vector) = full input port
	@ on exit:	sv1 <- object read
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt

	vcrfi	rva, glv, 5		@ rva <- main buffer
	add	sv5, rva, #(I2C0_BF_offset << 2)

mpcrde:	@ wait for mpc data to be ready
	swi	run_no_irq		@ disable interrupts
	tbrfi	rvb, sv5, 1		@ rvb <- data ready status from i2cbuffer[4]
	eq	rvb, #t			@ is mpc data ready?
	beq	mpcrdf			@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	mpcrde			@ jump to keep waiting
mpcrdf:	@ get data
	set	rvb, #null		@ rvb <- '()
	vcrfi	sv1, glv, 2		@ sv1 <- object received in glv
	vcsti	glv, 2, rvb		@ store '() in glv (for gc)

	tbrfi	rva, sv5, 4		@ rva <- number of bytes received
	eq	rva, #4			@ were exactly 4 bytes received?
	it	eq
	tbrfieq sv1, sv5, 2		@	if so,  sv1 <- object received
	@ exit
	set	rva, #f
	tbsti	rva, sv5, 1		@ store #f (data not ready)   in i2cbuffer[4]
	tbsti	rva, sv5, 0		@ store #f (channel not busy) in i2cbuffer[0]
	swi	run_normal		@ re-enable interrupts
	set	sv2, #null		@ sv2 <- '() eg. to unpack to heap
	pntrp	sv1			@ did we receive a packed item?
	beq	adr_unpack		@	if so,  sv1 <- unpacked-object, return via cnt
	set	pc,  cnt

.endif

.ifndef	exclude_read_write


	/* mpc output port write sub-function
	   write scheme item in sv1 to mpc port in sv2 */
	PRIMIT	mpcwrt, ufun, 2
	@ on entry:	sv1 <- object
	@ on entry:	sv2 <- ((port target n ...) . port-vector) = full output port
	pntrp	sv1
	bne	putmpr
	@ pack object in r1 before sending it out through mpc
	sav_rc	sv2			@ dts <- (full-output-port cnt ...)
	list	sv1, sv1		@ sv1 <- (object)
	call	adr_pack
	restor	sv2, cnt		@ sv2 <- full output port, cnt <- cnt, dts <- (...)
putmpr:	@ continue

	vcrfi	rva, glv, 5		@ rva <- main buffer
	add	sv5, rva, #(I2C1_BF_offset << 2)

mpcwr0:	@ wait for i2c channel to be free
	swi	run_no_irq		@ disable interrupts
	tbrfi	rvb, sv5, 0		@ rvb <- status of mpc channel
	eq	rvb, #f			@ is mpc channel not busy?
	beq	mpcwr1			@	if so, jump to continue
	swi	run_normal		@ re-enable interrupts
	b	mpcwr0			@ jump to keep waiting
mpcwr1:	@ write object to send in global vector
	vcsti	glv, 3, sv1		@ store object in glv, for mpc, to support gc

	@ write target mcu address in i2c buffer[0,4]
	set	rvb, #null
	tbsti	rvb, sv5, 0		@ store () in i2cbuffer[0] channel busy
	set	rvb, #i0		@ rvb <- 0 (scheme int)
	tbsti	rvb, sv5, 1		@ store 0 in i2cbuffer[4] data not ready

	@ write number of bytes to send in i2c buffer[12]

	vcrfi	sv1, glv, 3		@ sv1 <- object in glv for mpc

	pntrp	sv1			@ is object a pointer?
	itE	ne
	setne	rvb,  #4		@	if not, rvb <- 4 bytes to send (raw int)
	vecleneq rvb, sv1		@	if so,  rvb <- number of bytes in object (scheme int)
	itT	eq
	lsreq	rvb, rvb, #2		@	if so,  rvb <- number of bytes in object (raw int)
	addeq	rvb, rvb, #4		@	if so,  rvb <- number of bytes to send (raw int)
	tbsti	rvb, sv5, 3		@ store number of bytes to send in i2c buffer[12]
	@ initiate mpc write and exit
	swi	run_normal		@ re-enable interrupts
	bl	hwmpni			@ jump to hw-specific mpc-write init routine
	b	adr_npofxt

.endif


/*------------------------------------------------------------------------------
@  II.I.7. MPC ISR:			mpcisr
@-----------------------------------------------------------------------------*/


	/* MPC ISR -- not completed for this new version */
	PRIMIT	mpcisr, ufun, 0
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
	bleq	adr__ism		@	if so,  jump to deal with that (restart or go-through)
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
	eq	rvb, #i2c_irm_rcv	@ is status: Receiving as Master with new data received?
	it	ne
	eqne	rvb, #i2c_irs_rcv	@	if not, is status Receiving as Slave with new dat received?
	itT	eq
	tbrfieq lnk, lnk, 4		@	if so, lnk <- number of data bytes received before this one
	eqeq	lnk, #7			@	if so, were 7 bytes received before this one?
	beq	i2c_maloc		@		if so, jump to allocate Heap memory for object
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

i2c_rm_ini: @ Reading as Master -- slave has acknowldgd address (I2STAT = 0x40, set nak if need just 1 byt)
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
	bne	i2rmx1			@	if not, jump to see if there's only 1 byte left to read
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
	tbstieq rva, sv2, 0		@	if so,  store 0 (scheme int) as channel-busy
	tbstieq rva, sv2, 1		@	if so,  store 0 (scheme int) as data-not-ready/#addrss-byts
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
	lslmi	rva, rva, #3		@	if so,  rva <- number of bits to shift to get target byte
	ldrmi	rvb, [glv, sv1]		@	if so,  rvb <- scheme int containing bytes to send
	itTT	mi
	lsrmi	rvb, rvb, #2		@	if so,  rvb <- raw int containing bytes to send
	lsrmi	rvb, rvb, rva		@	if so,  rvb <- raw int, shifted to get byte in bits 0-7
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
	tbsti	rvb, sv2, 3		@ store number of bytes to read (total, raw) in i2cbuffer[12]
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





