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


	SMBL	"port", port

.balign	4

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV ports

	.include "memory_hw.s"		@ MEM    port hardware specific routines
	.include "memory_port.s"	@ MEM    port env binding and functions
	.include "uart_hw.s"		@ UAR0/1 port hardware specific routines
	.include "uart_port.s"		@ UAR0/1 port env binding and functions
.ifndef live_SD
	.include "file_flash_hw.s"	@ FILE   port hardware specific routines
	.include "file_flash_port.s"	@ FILE   port env binding and functions
.endif
.ifdef	onboard_SDFT
	.include "file_sd_hw.s"		@ SDFT   port hardware specific routines
	.include "file_sd_port.s"	@ SDFT   port env binding and functions
.endif
.ifdef	native_usb
	.include "usb_hw.s"		@ USB    port hardware specific routines
	.include "usb_port.s"		@ USB    port env binding and functions
.endif
.ifdef	include_i2c
	.include "i2c_hw.s"		@ I2C0/1 port hardware specific routines
	.include "i2c_port.s"		@ I2C0/1 port env binding and functions
.endif
.ifdef	enable_cpo
	.include "cpo_hw.s"		@ CPO    port hardware specific routines
	.include "cpo_port.s"		@ CPO    port env binding and functions
.endif


.text

.balign	4

/*------------------------------------------------------------------------------
@
@	Code Entry Points:
@
@		ioprfe		sets io port for (load ...) ...
@		ioprfn		sets io port in pre-entry for (read-char ...) ..
@
@	Rationale
@		Scheme io functions can take null, an integer, a shifted base
@		address, and other items as io port specifier. For example
@		(read), (read 1), (read #x4003000), (read #x4003000 #x04), 
@		(read #x4003000 #(20)).
@		The ioprfe and ioprfn functions convert these port specifiers
@		into the full input or output port needed by the port functions
@		defined in port vectors (code jump tables).
@		A null port specifier is first substituted by a call to
@		(current-input/output-port) which can return a (non-null)
@		port specifier or a full port.
@		Then, a port specifier that is a small integer becomes a
@		file port, larger integers become fixed address ports (uart,
@		i2c, usb), two integers become memory ports (address and offset)
@		and an integer followed by a vector becomes an i2c port.
@		The ioprfe and ioprfn functions can also call a port function
@		on the resulting full port.
@
@-----------------------------------------------------------------------------*/

	/* set default input port (do not modify sv2, sv3, lnk, cnt) */
	PRIMIT	ioprfe, ufun, 0
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved (even if Thumb2)
	b	adr_ioprfn

	/* set default input port (do not modify sv2, sv3, lnk, cnt) */
	PRIMIT	ioprfn, ufun, 0
	@ on entry:	sv1 <- () or (<port> <reg> ...) or full input  port
	@ on entry:	sv2 <- () or (<port> <reg> ...) or full output port
	@ on entry:	sv4 <- #t/#f or index of port function to exec on exit
	@ on entry:	sv5 <- value to keep (eg. lnk of function to return to)
	@ on exit:	sv1 <- ((port <reg> ...) . port-vec) = full input  port
	@ on exit:	sv2 <- ((port <reg> ...) . port-vec) = full output port
	and	rva, sv4, #0x7f		@ rva <- sv4, masked to find if #f
	eq	rva, #f			@ return 0 on problem?
	itE	eq
	seteq	sv3, i0			@	if so,  sv3 <- 0
	setne	sv3, null		@	if not, sv3 <- ()
	orr	sv3, sv3, sv4, lsl #8	@ sv3 <- 0 or () LSB with sv4 shifted up
	tst	sv4, #0x80		@ dealing with output port?
	itEE	eq
	ldreq	sv4, =var_curoutport	@	if so,  sv4 <- current-output-port
	setne	sv2, sv1		@	if not, sv2 <- cpoy of input port
	ldrne	sv4, =var_curinport	@	if not, sv4 <- current-input-port
	@ on entry:	sv2 <- port or null
	@ on entry:	sv3 <- ()=error out on problem or 0=return 0 on problem
	@ on entry:	sv4 <- curinport_var or curoutport_var
	@ on entry:	sv5 <- lnk of caller
	@ on exit:	sv1 <- ((port <reg> ...) . port-vec) = full input  port
	@ on exit:	sv2 <- ((port <reg> ...) . port-vec) = full output port
	@ on exit:	sv4 <- port model if port is not a file handle
	@ on exit:	rvb <- port type indicator
	@ preserves:	sv1, sv5 and sv3 (sv3 must be immediate, not pointer)
	nullp	sv2			@ is a port specified?
	bne	getpr1			@	if so,  skip current-output-port
	save	sv1, sv3, sv5		@ dts <- (obj lnk ...)
	sav_ec	
	list	sv1, sv4		@ sv1 <- (current-output-port)	
	call	eval			@ sv1 <- prt or (prt <reg> <n> .) ...
	set	sv2, sv1		@ sv2 <- prt or (prt <reg> <n> .) ...
	restor	env, cnt
	restor	sv1, sv3, sv5		@ sv1 <- obj, sv5 <- lnk, dts <- (...)
	pntrp	sv2			@ is sv2 a pointer?
	beq	getpr2			@	if so,  jump to continue
	list	sv2, sv2		@ sv2 <- (port)
	b	getpr2			@ jump to continue
getpr1:	@ build port-vector if needed
	car	sv4, sv2		@ sv4 <- prt or ((prt <rg> <n> .).prtvc)
	pntrp	sv4			@ is this a pointer?
	it	eq
	seteq	sv2, sv4		@	if so,  sv2 <- full port
getpr2:	@ continue building port-vector if needed
	snoc	rvb, sv4, sv2		@ rvb <-p|(p<r><n>.),sv4 <-(<r><n>.)|pvc
	pntrp	rvb			@ is this a pointer?
	beq	ioprfd			@	if so, return w/full-port in sv2
	intgrp	rvb
	bne	getprr
	@ sv2 is (integer ...) check if reg was specified
	pntrp	sv4
	itE	eq
	careq	rva, sv4		@ rva <- reg
	setne	rva, null
	intgrp	rva			@ is reg an int?
	beq	getpr5
	@ possible file port (character port, no reg)
	cmp	rvb, #0x4000		@ is port below highst fil prt bas adrs?
	bmi	getpr4
getpr5:	@ memory port or ISR (on glv)
	vcrfi	sv4, glv, 13		@ sv4 <- built-in env vec
	vcrfi	sv4, sv4, 4		@ sv4 <- built-in ports sub-env
	intgrp	rva			@ is reg an int?
	it	eq
	vcrfieq	sv4, sv4, 0		@	if so,  sv4 <- MEM port model
	beq	ioprfd			@	if so, jump to build full port
	@ sv2 is either (fixed-address) or (fixed-address #(...))
	ldr	rva, [sv4, #-4]		@ rva <- built-in port env vector tag
	lsr	rva, rva, #8		@ rva <- size of port env vec (raw int)
getpr3:	@ fixed-address ports (uart0/1,i2c0/1,usb) - scan port model bindings
	subs	rva, rva, #1
	bmi	getprr
	ldr	sv4, [sv4, rva, lsl #2]	@ sv4 <- port model (UAR0/1,I2C0/1,USB)
	vcrfi	rvc, sv4, 0		@ rvc <- port address
	eq	rvb, rvc		@ is this the port we want?
	beq	ioprfd			@	if so, jump to build full port
	vcrfi	sv4, glv, 13		@ sv4 <- built-in env vec
	vcrfi	sv4, sv4, 4		@ sv4 <- built-in ports sub-env
	b	getpr3
getpr4:	@ file port -- sv2 is (integer) with integer <= #x1000
	set	sv2, rvb
	set	rvc, sv3
	bl	ffhofl
	set	sv3, rvc
	nullp	sv4
	it	eq
	seteq	sv2, rvb
	beq	getprr
	set	sv2, sv4
	set	rvb, sv2
ioprfd:	@ continue
	@ sv2 <- port address or full port
	@ sv4 <- port model or full port
	@ rvb <- port address or non-gced pointer (used as indicator)
	lsr	sv3, sv3, #8		@ sv3 <- #t/#f or prt fun idx & io indic
	pntrp	rvb			@ was a full port specified on input?
	beq	iprfxt			@	if so,  jmp to ret w/ful-prt sv1
	tst	sv3, #0x80		@ is this an output port?
	itE	eq
	vcrfieq	sv4, sv4, 2		@ 	if so,  sv4 <- out prtvec of mdl
	vcrfine	sv4, sv4, 1		@ 	if not, sv4 <- in  prtvec of mdl
	cons	sv2, sv2, sv4		@ sv2 <- (port adrs . port-vec) =ful prt
iprfxt:	@ common exit
	tst	sv3, #0x80		@ is this an input port?
	it	ne
	setne	sv1, sv2		@ 	if so,  sv1 <- prt mdl | ful prt
	and	rvc, sv3, #0x03
	eq	rvc, #3
	itT	eq
	orreq	lnk, sv5, #lnkbit0	@ lnk <- made odd if Thumb2
	seteq	pc,  lnk
	tst	sv3, #0x02		@ is func to exec an int (eg.rd not pk)?
	beq	iprfx1			@	if so,  jump to continue
	snoc	sv2, sv5, sv2		@ sv1 <- (port ...),	sv5 <- port-vec
	snoc	sv2, sv4, sv2		@ sv1 <- port,		sv4 <- (...)
	eor	sv2, sv2, #0x03		@ sv1 <- port as pseudo float (for peek)
	bcons	sv2, sv2, sv4, sv5	@ sv1 <- ((port ...) . port-vector)	
	tst	sv3, #0x80		@ is this an input port?
	it	ne
	setne	sv1, sv2		@ 	if so,  sv1 <- prt mdl | ful prt
iprfx1:	@ finish up
	int2raw	rvc, sv3
	ldr	lnk, =adr_npofxt	@ lnk <- exit via npofxt (wrt-chr/nwln)
	b	prtfun			@ jump to perform port function

_func_
getprr:	@ report error if needed
	and	rva, sv3, #0xff
	eq	rva, #i0
	itT	ne
	setne	sv1, sv2
	ldrne	sv4, =port
	bne	error4
	set	sv1, i0
	set	sv2, sv1
	set	rvb, 0
	orr	lnk, sv5, #lnkbit0	@ lnk <- made odd if Thumb2
	set	pc,  lnk


/*------------------------------------------------------------------------------
@
@	Code Entry Points:
@
@		getc0		prepare to get char(s) from input port
@		getc1		get char from input port
@		getc2		update after getting char(s) from input port
@		prtwrc		call port write-char/write-string function
@		prthwp		call port putc function
@		prtfun		call a port-vector function
@
/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input  SUPPORT 1:		iprtfn
@  II.A.6.6.3. output SUPPORT 1:		oprtfn, prtwrc
@-----------------------------------------------------------------------------*/


.macro	prtfun	fun, return:vararg
	set	rvc, \fun
	.ifb	\return
	  b	prtfun
	.else
	  bl	prtfun
	.endif
.endm
	
_func_
getc0:	@ prepare to get one or more chars from input-port in sv1
	@ preserve all but rva, rvb
	prtfun	0x26
	
_func_
getc1:	@ get one char from input-port in sv1
	@ preserve all but rva, rvb
	prtfun	0x27
	
_func_
getc2:	@ what to do after getting one or more chars from input-port in sv1
	@ preserve all but rva, rvb
	prtfun	0x28
	
_func_
prtwrc:	@ port write-char / write-string function
	@ write scheme char or string sv1 to port sv2 using port's write-char
	@ on entry:	sv1 <- character
	@ on entry:	sv2 <- full output port
	prtfun	0x02

_func_
prthwp:	@ port hw-putc function
	@ on entry:	sv1 <- character
	prtfun	0x04

_func_	
prtfun:	@ call a port-vector function
	@ on entry:	sv1 or sv2 <- ((port <reg> <n> ...) . port-vec) =ful prt
	@ on entry:	rvc <- index of function to call in output port-vector
	@ modifies:	rva, possibly rvc (if input-port funtion)
	@ [internal entry]
	@ Note:	port-vector's address must be in non-heap (i.e. fixed) memory
	@	otherwise if gc occurs (through multitasking or interrupt that
	@	does zmaloc) then rva could become invalid in-between the
	@	cdr(ne/eq) and ldr lines below.
	tst	rvc, #0x20		@ is this an input-port function?
	itTE	ne
	bicne	rvc, rvc, #0x20		@	if so,  rvc <- offset to func
	cdrne	rva, sv1		@	if so,  rva <- input  port-vec
	cdreq	rva, sv2		@	if not, rva <- output port-vec
	ldr	rva, [rva, rvc, lsl #2]
	pntrp	rva
	it	ne
	lsrne	rva, rva, #16

  .ifdef CODE_OFFSET
  	it	ne
	addne	rva, rva, #CODE_OFFSET
  .endif

	orr	rva, rva, #lnkbit0	@ rva <- adrs chosn func cod odd if T2
	set	pc,  rva		@ jump to chosen func code (ret via lnk)


/*------------------------------------------------------------------------------
@
@	CHARACTER INPUT/OUTPUT PORT -- common port-vector functions
@
@	Ports:	file, uart, usb
@
@	Code Entry Points:
@
@		pchrdc		read-char/peek-char
@		pchrdy		char-ready?
@		pputs		write-string
@
@-------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input  SUPPORT 3 - character input  port:	pchrdc,	pchrdy
@-----------------------------------------------------------------------------*/


	/* read-char / peek-char for character input port (uart, file, usb) */
	PRIMIT	chrrdc, ufun, 2
	@ on entry:	sv1 <- ((port reg <n>) . port-vector) = full input port
	@ on exit:	sv1 <- char read
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt
	bl	getc0			@ sv4 <- prior char pos/fil desc (getc1)
					@ sv2 <- 0 or pos/descr copy
	eq	sv2, #i0		@ port can't be read?
	it	eq
	ldreq	sv3, =eof_char		@	if so,  sv3 <- eof
	beq	redchx			@	if so,  jump to exit with eof
	caar	sv3, sv1		@ sv3 <- port
	tst	sv3, #0x01		@ is port a float (peek- vs read-)?
	it	eq
	seteq	sv4, sv2		@	if so,  sv4 <- fil-desc copy
	bl	getc1			@ rvb <- chr read,rvc=rvb,sv4<-updtd pos
	eq	rvb, #eof		@ is char an eof?
	bne	redch1			@	if not,  jump to updt pos, exit
	cdr	rvc, sv1		@ rvc <- port-vector
	vcrfi	rvc, rvc, 5		@ rvc <- value of port go-through-on-eof
	eq	rvc, #f			@ is eof acceptable?
	it	eq
	ldreq	sv3, =eof_char		@	if so,  sv3 <- eof
	beq	redchx			@	if so,  jump to exit
	b	adr_chrrdc		@ jump back to re-read buffer/file

redch1:	@ update input-port's buffer if needed
	caar	sv3, sv1		@ sv3 <- port
	tst	sv3, #0x01		@ is port a float (peek- vs read-char)?
	raw2chr	sv3, rvb		@ sv3 <- char (scheme char)
	beq	redchx			@	if so,  jump to skip bfr update
	save	sv3			@ dts <- (char ...)
	bl	getc2			@ sv1 <- string read, bfr/desc updated
	restor	sv3			@ sv3 <- char,		dts <- (...)
redchx:	@ finish up
	set	sv1, sv3		@ sv1 <- char (scheme char)
	set	pc,  cnt		@ return


	/* char-ready? for character input port (common to uart, file, usb) */
	PRIMIT	chrrdy, ufun, 2
	@ on entry:	sv1 <- ((<port> <reg> <n>) . port-vector) = full in port
	@ on exit:	sv1 <- #t/#f indicating port char-ready status
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt
	bl	getc0			@ sv4 <- prior chr pos/fil desc (getc1)
					@ sv2 <- 0 or pos/descr copy
	eq	sv2, #i0		@ port can't be read?
	beq	adr_notfxt		@	if so,  exit with #f
	set	sv4, sv2		@ sv4 <- position/descriptor-copy
	bl	getc1			@ rvb <- chr read,rvc=rvb,sv4<-updtd pos
	eq	rvb, #eof		@ is char an eof?
	bne	adr_notfxt		@	if not, jump to exit with #t
	cdr	rvc, sv1		@ rvc <- port-vector
	vcrfi	rvc, rvc, 5		@ rvc <- value of port go-through-on-eof
	eq	rvc, #f			@ is eof acceptable?
	b	adr_boolxt		@ exit with #t/#f based on result


/*------------------------------------------------------------------------------
@
@	FILE INPUT/OUTPUT PORT -- COMMON FUNCTIONS (common to flash and sd)
@
@	Ports:	file
@
@	Code Entry Points:
@
@		pflwrc		write-char
@
@-----------------------------------------------------------------------------*/

	/* file write-char / write-string sub-function (file flash and sd ports) */
	PRIMIT	filwrc, ufun, 2
	@ on entry:	sv1 <- scheme char or string to write out
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vec) = full out port
	@ on exit:	sv4 <- updated file descriptor
	@ modifies:	sv3, sv4, sv5, rva, rvb, rvc
	@ write char in sv1 to FLASH file in sv2
	set	sv5, sv2		@ sv5 <- out port savd (ffhofl mods sv2)
	caar	sv2, sv2		@ sv2 <- file-ID (for ffhofl)
	tst	sv2, #f0		@ is file-ID an int?
	it	ne
	eorne	sv2, sv2, #0x03		@	if not, sv2 <- fileID for ffhofl
	bic	rvc, lnk, #lnkbit0	@ rvc <- lnk, saved against flok, ffhofl
	bl	flok			@ acquire file system lock
	bl	ffhofl			@ sv4 <- desc,sv2<-pre-lst,sv3<-post-lst
	set	sv2, sv5		@ sv2 <- full output port, restored
	set	sv3, rvc		@ sv3 <- saved lnk (pputs mods rvc)
	nullp	sv4			@ file open?
	it	ne
	cadarne	sv4, sv4		@	if so,  sv4 <- #(fnam pg ofst())
	itT	ne
	vcrfine sv5, sv4, 3		@	if so,  sv5 <- (<buffer>)
	nullpne	sv5			@	if so,  is file opened for out?
	beq	pflwrx			@		if not, jump to exit
	pntrp	sv1			@ is object to write a string?
	beq	pflwrs			@	if so, jump to write string
	chr2raw	rvb, sv1		@ rvb <- raw ascii char
	bl	prthwp			@ jump to write character
	b	pflwrx			@ jump to finish up
pflwrs:	bl	pputs			@ jump to write string
pflwrx:	@ finish up
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	b	funlok			@ release file system and return via lnk
	
_func_	
pputs:	@ port write string function
	@ (common to uart, file, usb, via puawrc (uart & usb) and pflwrc (file))
	@ write string in sv1 to port in sv2
	@ on entry:	sv1 <- string
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vec) = full out port
	@ on entry:	sv3 <- saved lnk from caller
	@ on entry:	sv4 <- file descriptor / port position desciptor
	@ on exit:	sv4 <- updated file descriptor / port position desciptor
	@ preserves:	sv1, sv2, sv3, sv4 (if not file)
	@ modifies:	sv4 (if file), sv5, rva, rvb, rvc
	caar	sv5, sv2		@ sv5 <- port
	set	rvc, i0			@ rvc <- 1st char offset (scheme int)
	tst	sv5, #0x02		@ is port a float (display vs write)?
	it	ne
	eorne	rvc, rvc, #0x03		@	if so,  rvc <- 1st char offset
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved against oprtfn
pputs0:	strlen	rva, sv1		@ rva <- number of chars (scheme int)
	cmp	rvc, rva		@ done writing?
	itT	pl
	orrpl	lnk, sv5, #lnkbit0
	setpl	pc,  lnk
	bytref	rvb, sv1, rvc		@ rvb <- raw ascii char
	tst	rvc, #0x02		@ doing display (rather than write)?
	bne	pputs1			@	if so,  jump to write char
	eq	rvb, #'"		@ writing a " (dble-quote), withn strng?
	it	ne
	eqne	rvb, #'\\		@	if not, writing \, withn strng?
	bne	pputs1
	set	rvb, '\\
	orr	rvb, rvb, rvc, lsl #8	@ rvb <- ascii chr + savd src strng ofst
	bl	prthwp			@ jump to write character
	lsr	rvc, rvb, #8		@ rvc <- offset of char, restored
	bytref	rvb, sv1, rvc		@ rvb <- raw ascii char, re-acquired
pputs1:	@ write char to port	
	orr	rvb, rvb, rvc, lsl #8	@ rvb <- ascii chr + savd src strng ofst
	bl	prthwp			@ jump to write character
	lsr	rvc, rvb, #8		@ rvc <- offset of char, restored
	add	rvc, rvc,  #4		@ rvc <- offset of nxt char (scheme int)
	b	pputs0			@ jump to keep writing

/*------------------------------------------------------------------------------
@
@	FILE INPUT/OUTPUT PORT -- UTILITY FUNCTIONS (common to flash and sd)
@
@	Ports:	file
@
@	Code Entry Points:
@
@		flok		acquire file system lock
@		funlok		release file system lock
@		ffhofl		return descriptor of file on open file list
@		foflup		update open-file list
@
@-----------------------------------------------------------------------------*/


_func_	
flok:	@ acquire the file system lock
	eor	fre, fre, #0x03		@ fre <- ...bbb01	(reserv level 1)
	set	rvb, BUFFER_START
flokwt:	vcrfi	rva, rvb, FILE_LOCK
	eq	rva, #0
	bne	flokwt
	bic	rva, fre, #0x03
	vcsti	rvb, FILE_LOCK, fre	@ reserve the file system, [commit rsrv]
  .ifdef cortex_a9
	dmb
  .endif
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [restart crt]
	set	pc,  lnk		@ return
	
_func_	
funlok:	@ release the file system lock
	set	rva, 0			@ rva <- 0, files unlocked indicator
	set	rvb, BUFFER_START
  .ifdef cortex_a9
	dmb
  .endif
	vcsti	rvb, FILE_LOCK, rva
  .ifdef cortex_a9
	dsb
  .endif
	set	pc,  lnk		@ return


_func_	
ffhofl:	@ return the file descriptor of a file that is on the open file list
	@ also return pointer to file list
	@ on entry:	sv2 <- file ID/handle
	@ on exit:	sv4 <- file descriptor or null if file not on list
	@ on exit:	sv2 <- open-file-list link to curr desc (for fil close)
	@ on exit:	sv3 <- open-file-list after current desc (for fil close)
	@ modifies:	sv2, sv3, sv4, rva, rvb
	set	rvb, sv2
	set	sv2, null
	vecref	sv3, glv, 6		@ sv3 <- list of open files frm glbl vec
ffhof0:	nullp	sv3			@ done scanning open file list?
	itT	eq
	seteq	sv4, sv3		@	if so,  sv4 <- '()
	seteq	pc,  lnk		@	if so,  jump to exit
	car	sv4, sv3		@ sv4 <- open file descr
	caar	sv4, sv4		@ sv4 <- handle
	eq	sv4, rvb		@ is this the handle to acquire?
	itT	ne
	setne	sv2, sv3		@	if not, sv2 <-ptr to prv opn fil
	cdrne	sv3, sv3		@ 	if not, sv3 <-remnng opn fil lst
	bne	ffhof0			@	if not, scan remndr opn fil lst
	car	sv4, sv3		@ sv4 <- open file descr
	cdr	sv3, sv3		@ sv3 <- remaining open files list
	set	pc,  lnk		@ return sv4-descr,sv2-prels,sv3-postls
	

_func_
foflup:	@ update open-file list (if source page address is on that list)
	@ on entry:	sv4 <-	special output file descriptor:
	@			#(calr-tmp-stor  dest-pg  caller-tmp-stor calr-tmp-stor  src-pg)
	@ modifes:	sv2, rva, rvb
	@ returns via:	lnk
	@ Note:		this routine switches mode to run_no_irq and back to run_normal
	@ side-effects:	the open-file list (glv, 6) is updated with page address dest-page
	@		wherever it contains page-address src-page.
	swi	run_no_irq		@ disable interrupts (user mode)
	vcrfi	rva, glv, 6		@ rva <- open-files list
foflu0:	
	nullp	rva			@ done updating addresses?
	beq	foflu1			@	if so,  jump to continue
	car	rvb, rva		@ rvb <- 1st file descriptor
	@
	@ possible bug:
	@
	@ with possible external file systems and their open file descriptors stored
	@ on open-file list, it may be necessary to check first that this descriptor
	@ is for an on-chip file (before getting/checking the page address) here.
	@
	@ workaround:	do not use when ext-file-port files are open (a bit too limiting though)
	@
	vcrfi	rvb, rvb, 1		@ rvb <- page address from descriptor
	vcrfi	sv2, sv4, 4		@ sv2 <- source page address
	eq	sv2, rvb		@ does this page descriptor include this page address?
	itTT	eq
	careq	rvb, rva		@	if so,  rvb <- file descriptor
	vcrfieq sv2, sv4, 1		@	if so,  sv2 <- trgt adrs in extra flsh=new pg adrs
	vcstieq rvb, 1, sv2		@	if so,  store new page address in file descriptor
	cdr	rva, rva		@ rva <- rest of open-files list
	b	foflu0			@ jump to continue updtng page adrs in open-files list
foflu1:	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return
	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



