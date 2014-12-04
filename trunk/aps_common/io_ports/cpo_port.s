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
@ III.C. COMMON COMPONENTS OF COPROCESSOR I/O and ISR
@
@-----------------------------------------------------------------------------*/

	/* cpo port, environment+obarray binding and port model		------*/
	BNDVAR	"CPO", vcpo
	VCTR	vcpo, i0, cpoipr, cpoopr

	/* cpo input and output port-vectors	------------------------------*/
	VCTR	cpoipr, i1, val_npofxt, val_chrrdc, val_chrrdy, val_chpred, true, val_uargc0, val_uargc1, val_uargc2
	VCTR	cpoopr, i2, val_npofxt, val_uarwrc, val_chpwrt, val_cpoptc


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* cpo putc sub-sub-function */
	PRIMIT	cpoptc, ufun, 2
	@ modifies:	rva, rvc
	swi	run_no_irq		@ disable interrupts (user mode)
	ldr	rva, =cpo_tx_buffer
	ldrb	rvc, [rva, #2]		@ rvc <- write pointer (offset)
	strb	rvb, [rva, rvc]
	add	rvc, rvc, #1		@ rvc <- updated write pointer (offset)
	eq	rvc, #0x0100		@ is write pointer above buffer?
	it	eq
	seteq	rvc, 4			@	if so,  rvc <- reset write ptr
pcppt0:	@ wait for cpo-tx buffer to not be full (if it is)
	ldrb	rva, [rva, #0]		@ rvb <- read  pointer (offset)
	eq	rva, rvc		@ is buffer full?
	ldr	rva, =cpo_tx_buffer	@ rva <- coprocessor Tx buffer, reloaded
	beq	pcppt0			@	if so,  jump back to wait
	strb	rvc, [rva, #2]		@ store updated write pointer
	b	cpohwrc			@ init cpo write, re-enab ints, return


/*------------------------------------------------------------------------------
@ COPROCESSOR ISR
@-----------------------------------------------------------------------------*/

	/* interrupt service routine for cpo, branched from genisr */
	PRIMIT	cpoisr, ufun, 0
	stmdb	sp!, {sv1-sv5, env, dts, glv}	@ store remnng user regs on stk
	@ get event from CPO and acknowledge it back to CPO
	ldr	rvb, =cpo_rx_msg
	ldr	rva, [rvb]
	orr	sv1, rva, #0x80
	ldr	rvb, =cpo_tx_msg
	str	sv1, [rvb]
	@ branch
	eq	rva, #1
	beq	cpo_rx
cpoixt:	@ exit: wait for handshake from M0 (prevents isr re-entry on jitter)
	ldr	rva, =cpo_rx_msg
	ldr	rvb, [rva]
	cmp	rvb, #0
	bne	cpoixt
	@ exit: clear ack set on entry (prevents deadlock)
	ldr	rva, =cpo_tx_msg
	set	rvb, 0
	str	rvb, [rva]
	@ exit: clear interrupt
	bl	cpohwclri
	@ exit: restore registers and return from interrupt
	ldmia	sp!, {sv1-sv5, env, dts, glv} @ restore user mode regs from stk
	b	adr__isx		@ jump to exit isr (simple)

cpo_rx:	@ event 1: data Rx
	ldr	dts, =cpo_rx_buffer
	ldrb	sv5, [dts, #0]		@ sv5 <- Rx read  pointer (offset)
	ldrb	sv2, [dts, #2]		@ sv2 <- Rx write pointer (offset)
	eq	sv5, sv2		@ nothing to read?
	beq	cpoixt
  .ifndef enable_a9_mpcore
	ldr	env, =BUFFER_START	@ env <- start address of buffers
  .else
	mrc	p15, 0, env, c0, c0, 5	@ env <- Multiproc Affinity reg, MPIDR
	and	env, env, #3		@ env <- cpu_ID
	ldr	rva, =MP_mat		@ rva <- MP address table start
	add	env, rva, env, lsl 5	@ env <- MP table address for cpu_ID[n]
	ldr	env, [env, #0x10]	@ env <- BUFFER_START_n
  .endif
	vcrfi	env, env, READ_BF_offset @ env <- address of READBUFFER
	vcrfi	rva, env, 0		@ rva <- num chars in buffer (sch int)
	add	env, env, #4		@ env <- address of 1st char in READBUF
	tst	rva, #i0
	lsr	rva, rva, #2
	beq	cponec
	ldr	sv4, =cpo_tx_buffer
	ldrb	glv, [sv4, #0]		@ glv <- Tx read  pointer (offset)
	ldrb	rvc, [sv4, #2]		@ rvc <- Tx write pointer (offset)
cporxl:	@ loop to copy data to write buffer and Tx buffer
	ldrb	rvb, [dts, sv5]		@ rvb <- char from Rx buffer
	add	sv5, sv5, #1		@ sv5 <- updated Rx read pointer
	eq	sv5, #0x0100
	it	eq
	seteq	sv5, 4
	eq	rvb, #3
	beq	cpobrk
	eq	rvb, #'\n		@ is byte a newline (lf)
	beq	cporxl			@	if so,  jump to process chars
	strb	rvb, [env, rva]
	add	rva, rva, #1		@ rva <- offset of next char in READBUF
	add	sv3, rvc, #1		@ sv3 <- updated Tx write pointer
	eq	sv3, #0x0100
	it	eq
	seteq	sv3, 4
	eq	sv3, glv		@ is Tx buffer full?
	itT	ne
	strbne	rvb, [sv4, rvc]		@	if not, write char to Tx buffer
	setne	rvc, sv3		@ 	if not, rvc <- updated Tx ptr
	eq	rvb, #'\b		@ was byte a backspace?
	beq	cpobks			@	if so,  jump to process backspc
cporxe:	@ re-entry after backspace
	eq	sv5, sv2		@ Rx buffer now empty?
	bne	cporxl			@	if not, jump to get more chars
	@ update write pointer in Tx buffer
	strb	rvc, [sv4, #2]
	@ update read pointer in Rx buffer
	strb	sv5, [dts, #0]
	@ update offset in READBUFFER
	lsl	rvb, rva, #2
	orr	rvb, rvb, #i0
	str	rvb, [env, #-4]		@ update READBUFFER tag
	@ wait on message completion from coprocessor
	bl	cpohwrdywt
	@ set write message in tx-msg memory cell
	ldr	rva, =cpo_tx_msg
	set	rvc, 1			@ rvc <- 1, new message = write
	str	rvc, [rva]		@ set message in Tx-MSG
	@ signal the write to coprocessor
	bl	cpohwsig
cpopt2:	@ wait for acknowledge
	ldr	rva, =cpo_rx_msg
	ldr	rvc, [rva]		@ set message in Tx-MSG
	eq	rvc, #0x81
	bne	cpopt2
	@ clear tx-msg (handshake) (prevent M0 isr re-entry on interrupt jitter)
	ldr	rva, =cpo_tx_msg
	set	rvc, 0			@ rvc <- 0, new message = do nothing
	str	rvc, [rva]		@ set message in Tx-MSG
	@ return
	b	cpoixt			@ exit

cpobks:	@ process a backspace character
	subs	rva, rva, #2
	it	mi
	setmi	rva, 0
	eq	sv3, glv		@ is Tx buffer full?
	beq	cporxe
	add	sv3, rvc, #1		@ sv3 <- updated Tx write pointer
	eq	sv3, #0x0100
	it	eq
	seteq	sv3, 4
	eq	sv3, glv		@ is Tx buffer full?
	beq	cporxe
	set	rvb, ' 		@ rvb <- space (raw ASCII)
	strb	rvb, [sv4, rvc]		@ write char to Tx buffer
	set	rvc, sv3		@ rvc <- updated Tx ptr
	add	sv3, rvc, #1		@ sv3 <- updated Tx write pointer
	eq	sv3, #0x0100
	it	eq
	seteq	sv3, 4
	eq	sv3, glv		@ is Tx buffer full?
	beq	cporxe
	set	rvb, '\b		@ rvb <- backspace
	strb	rvb, [sv4, rvc]		@ write char to Tx buffer
	set	rvc, sv3		@ rvc <- updated Tx ptr
	b	cporxe

cponec:	@ process chars with no special treatment and no echo
	ldrb	rvb, [dts, sv5]		@ rvb <- char from Rx buffer
	add	sv5, sv5, #1		@ sv5 <- updated Rx read pointer
	eq	sv5, #0x0100
	it	eq
	seteq	sv5, 4
	strb	rvb, [env, rva]		@ store character in READBUFFER
	add	rva, rva, #1		@ rva <- offset of next char in READBUF
	eq	sv5, sv2		@ Rx buffer now empty?
	bne	cponec			@	if not, jump to get more chars
	@ update read pointer in Rx buffer
	strb	sv5, [dts, #0]
	@ update offset in READBUFFER
	lsl	rvb, rva, #2
	orr	rvb, rvb, #f0
	str	rvb, [env, #-4]		@ update READBUFFER tag
	@ exit
	b	cpoixt			@ exit

cpobrk:	@ process a break (ctrl-c)
	@ exit: wait for handshake from M0
	ldr	rva, =cpo_rx_msg
	ldr	rvb, [rva]
	cmp	rvb, #0
	bne	cpobrk
	@ exit: clear ack set on entry (prevents deadlock)
	ldr	rva, =cpo_tx_msg
	set	rvb, 0
	str	rvb, [rva]
	@ flush cpo buffer
	ldr	rva, =cpo_rx_buffer
	ldrb	rvc, [rva, #2]		@ rvc <- write pointer (offset)
	strb	rvc, [rva, #0]		@ set read ptr to write ptr (flush)
	@ clear interrupt
	bl	cpohwbrki
	@ reset READBUFFER
	getBUF	rva, rvc		@ rva <- BUFFER_START_n, rvc <- temp
	vcrfi	rva, rva, READ_BF_offset @ rva <- address of readbuffer
	vcrfi	rvc, rva, 0		@ rvc <- number of chars in readbuffer
	and	rvc, rvc, #3		@ rvc <- num chars tag (echo vs non-echo)
	vcsti	rva, 0, rvc		@ set number of chars in readbuffer to 0
	@ return to genisr with interrupt -3
	ldmia	sp!, {sv1-sv5, env, dts, glv}	@ restor usr mode regs frm stack
	mvn	rvb, #3			@ rvb <-  3 (ascii ctrl-c) inverted
	add	rvb, rvb, #1		@ rvb <- -3 (to indicate ctrl-c)
	b	genis0



