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
@	UART INPUT/OUTPUT PORT and ISR
@
@-------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input SUPPORT 4 - uart/usb input port:	uaripr, puagc0,1,2
@  II.A.6.6.3. output SUPPORT 4 - uart output port:	uaropr, puawrc, puaptc
@-----------------------------------------------------------------------------*/


	/* uart0 port, environment+obarray binding and port model	------*/
	BNDREG	"uar0",	uart0_base
	BNDVAR	"UAR0", vuart0
	VCTR	vuart0, reg_uar0, uaripr, uaropr

	/* uart1 port, environment+obarray binding and port model	------*/
	BNDREG	"uar1",	uart1_base
	BNDVAR	"UAR1", vuart1
	VCTR	vuart1, reg_uar1, uaripr, uaropr

	/* uart input and output port-vectors	------------------------------*/
	VCTR	uaripr, i1, val_npofxt, val_chrrdc, val_chrrdy, val_chpred, true, val_uargc0, val_uargc1, val_uargc2
	VCTR	uaropr, i2, val_npofxt, val_uarwrc, val_chpwrt, val_uarptc

/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* uart read-helper init function
	   prepare to get one or more chars from uart */
	PRIMIT	uargc0, ufun, 1
	@ on entry:	sv1 <- ((port <reg> <n>) . port-vector) = full input port
	@ on exit:	sv2 <- position of last char read from buffer, copy
	@ on exit:	sv4 <- position of last char read from buffer
	@ preserves:	sv1, sv3, sv5, rva, rvb, rvc
	@ modifies:	sv2, sv4
	cdar	sv2, sv1
	pntrp	sv2
	itEE	eq
	careq	sv2, sv2
	vecrefne sv2, glv, 5		@ sv2 <- main buffer
	vecrefne sv2, sv2, READ_BF_offset
	vecref	sv2, sv2, 0		@ sv2 <- num chars in bfr (int/flt)
	and	sv2, sv2, #3
	add	sv2, sv2, #0x0C		@ sv2 <- ofst to befr 1st chr in READBFR
	set	sv4, sv2		@ sv4 <- ofst to befr 1st chr in READBFR
	set	pc,  lnk		@ return

	/* uart read-helper getc function
	   get next char from uart into rvb, update char position in sv4 */
	PRIMIT	uargc1, ufun, 4
	@ on entry:	sv1 <- ((port <reg> <n>) . port-vec) = full input port
	@ on entry:	sv2 <- value to preserve (eg. ofst to before 1st chr)
	@ on entry:	sv4 <- ofst to befr 1st char in READBFR or copy (pk-chr)
	@ on entry:	sv5 <- value to preserve (eg. lnk of caller)
	@ on entry:	rvc <- value to preserve (eg. previous char read)
	@ on exit:	rvb <- ascii char read or eof (raw ascii char)
	@ on exit:	rvc <- entry val of rvb (pchred prev chr, frdxp4 pos)
	@ on exit:	sv4 <- updated char offset in READBUFFER (or its copy)
	@ preserves:	sv1, sv2, sv5, rva
	@ modifies:	sv3, sv4, rvb, rvc
	set	rvc, rvb		@ rvc <- entry val of rvb, set for exit
	cdar	sv3, sv1
	pntrp	sv3
	itEE	eq
	careq	sv3, sv3
	vecrefne sv3, glv, 5
	vecrefne sv3, sv3, READ_BF_offset
	vecref	rvb, sv3, 0		@ rvb <- num chars in bfr (scheme int)
	add	rvb, rvb, #0x10		@ rvb <- offest to after last bfr char
	add	sv4, sv4, #0x04		@ sv4 <- offset of char to read
	cmp	sv4, rvb		@ is ofst of chr to rd past end of bfr?
	itE	pl
	setpl	rvb, eof		@	if so,  rvb <- eof
	bytrefmi rvb, sv3, sv4		@	if not, rvb <- chr from read bfr
	set	pc,  lnk		@ return	

	/* uart read-helper function finish-up
	   extract string and crunch buffer */
	PRIMIT	uargc2, ufun, 5
	@ on entry:	sv1 <- ((port <reg> <n>) . port-vec) = full input port
	@ on entry:	sv2 <- offset to before 1st char in READBUFFER
	@ on entry:	sv4 <- offset of last char read
	@ on exit:	sv1 <- string to be parsed or eof-char
	@ preserves:	sv5
	@ modifies:	sv1, sv2, sv3, sv4, rva, rvb, rvc
	bic	sv3, lnk, #lnkbit0	@ sv5 <- lnk, saved (made even if T2)
	save	sv4, sv3, sv5		@ dts <- (end lnk sv5 ...)
	cdar	sv3, sv1		@ sv3 <- (port's read buffer) or '()
	pntrp	sv3			@ does port have read buffer?
	itEE	eq
	careq	sv1, sv3		@	if so,  sv1 <- port's read buffr
	vecrefne sv1, glv, 5
	vecrefne sv1, sv1, READ_BF_offset
	car	sv3, dts		@ sv3 <- end -- offset to last char
	save	sv1			@ dts <- (port's-read-bfr end lnk sv5 .)
	add	sv2, sv2, #4		@ sv2 <- start -- offset to 1st char
	bl	subcpy			@ sv1 <- expression -- i.e. substring
	restor	sv3			@ sv3 <- rd-bfr rstrd,dts<-(end lk sv5.)
	restor	sv4			@ sv4 <- strt chrs to mov,dts<-(lk sv5.)
	save	sv1			@ dts <- (expr-to-parse lnk sv5 ...)
	set	sv1, sv3		@ sv1 <- port's-read-buffer
	add	sv2, sv4, #4		@ sv2 <- start offset, incl header & cr
	vecref	sv3, sv1, 0		@ rvb <- number of chars in buffer
	add	sv3, sv3, #0x10		@ rvb <- offest to after last buffr char
	set	sv4, sv1		@ sv4 <- destination
	set	rvb, 4
	swi	run_no_irq		@ disable interrupts (user mode)
cpmem:	@ copy from start to end
	cmp	sv2, sv3		@ are we done copying?
	it	mi
	bytrefmi rva, sv1, sv2		@ rva <- raw byte from source
	itTT	mi
	strbmi	rva, [sv4, rvb]		@ store it in target
	addmi	sv2, sv2, #4		@ sv2 <- updated source end offset
	addmi	rvb, rvb, #1		@ rvb <- updated target end address
	bmi	cpmem			@ jump to continue copying bytes
	sub	rvb, rvb, #4		@ rvb <- number of chars copied
	lsl	rvb, rvb, #2		@ rvb <- number of chars, shifted
	vecref	sv5, sv1, 0		@ sv5 <- num chars in buffer (int/flt)
	and	sv5, sv5, #3
	orr	rvb, rvb, sv5		@ rvb <- number of chars (scheme int)
	vecset	sv1, 0, rvb		@ store that in buffer
	swi	run_normal		@ re-enable interrupts (user mode)
	restor	sv1, sv5		@ sv1 <- exp to pars,sv5<-lk,dts<-(sv5.)
	orr	lnk, sv5, #lnkbit0	@ lnk <- restored
	restor	sv5			@ sv5 <- restored, dts <- (...)
	set	pc,  lnk		@ return

	/* uart write-char / write-string sub-function */
	PRIMIT	uarwrc, ufun, 2
	@ on entry:	sv1 <- scheme char or string to write out
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vec) = full out port
	@ modifies:	sv4, rva, rvb, rvc
	caar	sv4, sv2		@ sv4 <- port
	lsr	rva, sv4, #2		@ rva <- port base adrs without upr 2bt
	lsl	sv4, rva, #4		@ sv4 <- full port base address
	pntrp	sv1			@ are we writing a string?
	beq	pputs			@	if so,  write string ret via lnk
	chr2raw	rvb, sv1		@ rvb <- raw ASCII character from sv1
	b	prthwp			@ jump to write character (ret via lnk)

	/* uart putc sub-sub-function
	   write raw ascii value in rvb to UART specified by base address in sv4 */
	PRIMIT	uarptc, ufun, 2
	@ on entry:	sv1 <- scheme char or string to write out
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vec) = full out port
	@ on entry:	sv3 <- saved lnk from caller of caller
	@ on entry:	sv4 <- port address
	@ on entry:	sv5 <- saved lnk from caller
	@ on entry:	rvb <- ascii char to write + offset in string (if strng)
	@ preserves:	sv1, sv2, sv3, sv4, sv5, rvb, rvc
	@ modifies:	rva
	swi	run_no_irq		@ disable interrupts (user mode)
	r8wfbt	sv4, #uart_status, uart_txrdy_bit, 1, rva @ wait for uart ready
	write8	rvb, sv4, #uart_thr	@ Write content of rvb out to UART's THR
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk

/*------------------------------------------------------------------------------
@ 	UART ISR
@-----------------------------------------------------------------------------*/

	/* interrupt service routine for uart, branched from genisr */
	PRIMIT	uarisr, ufun, 0
	eq	rvb, #uart0_int_num	@ is interrupt for UART0?
	itE	eq			@	if-then (Thumb-2)
	seteq	rva, uart0_base		@	if so,  rva <- UART0 base adrs
	setne	rva, uart1_base		@	if not, rva <- UART1 base adrs
  .ifdef uart_istat_1st
	read8	fre, rva, #uart_istat	@ fre <- interrupt status -- clears flag
  .endif
	read8	fre, rva, #uart_rhr	@ fre <- Byte from UART
	getBUF	rvb, cnt		@ rvb <- BUFFER_START_n, cnt <- temp
	vecref	rvb, rvb, READ_BF_offset
	ldr	cnt, [rvb, #-4]
	lsr	cnt, cnt, #6
	sub	cnt, cnt, #0x10
	vecref	rvc, rvb, 0
	tst	rvc, #i0		@ are we in normal mode?
	beq	uarnec			@	if not, jump to special mode
	eq	fre, #3			@ is byte a ctrl-c?
	beq	uarbrk			@	if so,  exit via break int
	eq	fre, #'\n		@ is byte a lf?
	beq	uarskp			@	if so,  ignore it (jump to exit)
	eq	fre, #'\b		@ is byte a backspace?
	beq	uarbks			@	if so,  jump to process it
	cmp	rvc, cnt		@ readbuffer full?
	bpl	uarskp			@	if so,  jump to exit
	add	cnt, rvb, rvc, lsr #2	@ cnt <- char offset (after num-chars)
	write8	fre, cnt, #4		@ store byte in buffer
	add	rvc, rvc, #4		@ rvc <- offset of next char in buffer
	bl	iuaptc			@ echo character
uarixt:	@ finish up
	vecset	rvb, 0, rvc		@ store number of chars in bfr, sch int
uarskp:	@ clear interrupt in uart[0/1] and interrupt vector
	clearUartInt			@ clear interrupt in uart[0/1]
	b	adr__isx		@ jump to exit isr (simple)

_func_
uarbks:	@ process a backspace
	lsrs	cnt, rvc, #2		@ is num chars in buffer zero?
	it	ne
	subne	rvc, rvc, #4		@	if not, rvc <- offst of nxt chr
	bl	iuaptc			@ write backspace out to UART
	set	fre, ' 			@ fre <- space (raw ASCII)
	bl	iuaptc			@ write space out to UART
	set	fre, '\b		@ fre <- backspace
	bl	iuaptc			@ write backspace out to UART
	b	uarixt

_func_
uarnec:	@ process char with no special treatment and no echo
	eor	cnt, cnt, #3
	cmp	rvc, cnt		@ readbuffer full?
	bpl	uarskp			@	if so,  jump to exit
	add	cnt, rvb, rvc, lsr #2	@ cnt <- char offset (after num-chars)
	write8	fre, cnt, #4		@ store byte in buffer
	add	rvc, rvc, #4		@ rvc <- offset of next char in buffer
	b	uarixt

_func_
iuaptc:	@ write raw ascii value in fre (r0) to UART specified in rva (r2)
	@ on entry:	fre <- raw char to write out
	@ on entry:	rva <- uart base address
	@ modifies:	cnt
	r8wfbt    rva, #uart_status, uart_txrdy_bit, 1, cnt @ wait for uart rdy
  .ifndef uart_byte_only
	write	fre, rva, #uart_thr	@ Write content of fre out to UART's THR
  .else
	write8	fre, rva, #uart_thr	@ Write content of fre out to UART's THR
  .endif
	set	pc,  lnk		@ return

_func_
uarbrk:	@ process a break (ctrl-c)
	clearUartInt			@ clear interrupt in uart[0/1]
	getBUF	rva, rvb		@ rva <- BUFFER_START_n, rvb <- temp
	vecref	rva, rva, READ_BF_offset @ rva <- address of readbuffer
	vecset	rva, 0, i0		@ set number of chars in readbuffer to 0
	mvn	rvb, fre		@ rvb <-  3 (ascii ctrl-c) inverted
	add	rvb, rvb, #1		@ rvb <- -3 (to indicate ctrl-c)
	set	pc,  lnk		@ return to genisr

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



