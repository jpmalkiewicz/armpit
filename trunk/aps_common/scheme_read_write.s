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

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV read_write

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. Input:			read
@  II.A.6.6.3. Output:			write, display
@-----------------------------------------------------------------------------*/

	PRIMIT	"read",    efun, 0, oioprfn, (0x24<<2)|i0
	PRIMIT	"write",   efun, 1, oioprfn, (0x03<<2)|i0
	PRIMIT	"display", efun, 1, oioprfn, (0x03<<2)|f0

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.4. System interface:	OPTIONAL:	load (used by boot)
@-----------------------------------------------------------------------------*/

	/* (load filename <port-model>) */
	PRIMIT	"load", pfun, 2
	@ in:	sv1 <- filename
	@ in:	sv2 <- <port-model> or null
	sav__c				@ dts <- (cnt ...)
	call	adr_open_infile		@ sv1 <- port
	list	sv1, sv1		@ sv1 <- (port), for setipr
	set	sv4, 0x80 | true
	bl	adr_ioprfe		@ sv1 <- full input-port
	save	sv1			@ dts <- (port cnt ...)
load1:	car	sv1, dts		@ sv1 <- port
	set	sv4, (0x24<<2)|i0
	call	adr_ioprfn		@ sv1 <- expr read from file
	ldr	sv2, =eof_char		@ sv2 <- end-of-file character
	eq	sv1, sv2		@ end-of file found?
	beq	loadxt			@	if so,  jump to close file and exit
	vcrfi	env, glv, 7		@ env from glv (***NEW***)
	call	eval			@ sv1 <- result, from evaluating sv1 in default environment
	b	load1			@ jump back to read and evaluate next expression
loadxt:	@ exit
	restor	sv1			@ sv1 <- port, dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt
	b	adr_close_inprt		@ close the input port, return npo via prtcli

/*------------------------------------------------------------------------------
@
@	CHARACTER INPUT/OUTPUT PORT -- COMMON FUNCTIONS
@
@-------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input  SUPPORT 3 - character input  port:	pchred
@  II.A.6.6.3. output SUPPORT 3 - character output port:	pchwrt, rwrite
@-----------------------------------------------------------------------------*/


	/* read function for character input port (common to uart, file, usb) */
	PRIMIT	chpred, ufun, 2
	@ in:	sv1 <- ((<port> <reg> <n>) . port-vector) = full input port
	@ out:	sv1 <- parsed scheme expression
	@ modifies:	sv1, sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt (through parse and mxpnd if needed)
	bl	getc0			@ sv4 <- prev chr pos or fil desc (for getc1)
					@ sv2 <- 0 or pos/desc cpy
	eq	sv2, #i0		@ port can be read?
	beq	readxt			@	if not, jump to return eof
	@ port can be read, proceed
	set	sv5, 0x15		@ sv5 <- par-cnt=0 (b16:31), commnt=#f (b4), par-inc=1 (b2)
	set	rvb, 0x0		@ rvb <- npo, pseudo-previous character
rdexp0:	@ wait for a full datum or eof
	bl	getc1			@ rvb <- raw chr read, rvc<-prev chr, sv4<-updtd pos/descr
	eq	rvb, #eof		@ is byte the end-of-file byte?
	beq	rdexp1			@	if so,  jump to process possible early end-of-input
	eq	rvb, #'\r		@ is char a carriage return?
	it	ne
	eqne	rvb, #'\n		@	if not, is char a line feed?
	itE	eq
	orreq	sv5, sv5, #0x10		@	if so,  set comment indicator to #f (set b4 in sv5)
	eqne	rvb, #' 		@	if not, is char a space?
	it	eq
	eqeq	sv5, #0x15		@	if so, is par-cnt=0, par-inc=1, cmnt=#f (sch int)?
	beq	rdexp2			@			if so,  jump to extrct & parse expr
	eq	rvb, #'\r		@ is char a carriage return?
	it	ne
	eqne	rvb, #'\n		@	if not, is char a line feed?
	beq	rdexp0			@		if so,  jump to keep scanning characters
	tst	sv5, #0x10		@ are we within a comment?
	beq	rdexp0			@	if so,  jump to keep scanning characters
	eq	rvc, #'\\		@ is previous character a \?
	it	eq
	seteq	rvb, 0x00		@	if so,  rvb <- npo (clear rvb in case of \\)
	beq	rdexp0			@	if so,  jump to process next chars
	and	rva, sv5, #0x04		@ rva <- paren-inc
	eq	rvb, #'(		@ is character a ( ?
	it	eq
	addeq	sv5, sv5, rva, lsl #6	@	if so,  sv5 <- paren-count increased by paren-inc
	beq	rdexp0			@	if so,  jump to keep scanning characters
	eq	rvb, #')		@ is character a ) ?
	it	eq
	subeq	sv5, sv5, rva, lsl #6	@	if so,  sv5 <- paren-count decreased by paren-inc
	beq	rdexp4			@ 	if so,  jump to check if acceptable end of expr
	eq	rvb, #'"		@ is character a " ?
	it	eq
	eoreq	sv5, sv5, #0x04		@	if so,  sv5 <- paren-inc toggled (between 0 and 1)
	beq	rdexp4			@	if so,  jump to check if acceptable end of expr	
	eq	rvb, #';		@ is char a semi-colon?
	bne	rdexp0			@	if not, jump to keep scanning characters
	tst	sv5, #0x04		@ are we outside of a string (i.e. paren-inc=1)?
	it	ne
	bicne	sv5, sv5, #0x10		@		if so,  set cmnt ind to #t (clr b4 in sv5)
	b	rdexp0			@ jump to keep scanning characters
rdexp1: @ eof encountered -- return expr or eof or re-read buffer
	cdr	rva, sv1		@ rva <- port-vector
	vcrfi	rva, rva, 5		@ rva <- value of 'wait-for-cr' from port
	eq	rva, #t			@ wait for cr?
	beq	adr_chpred		@	if so,  jump back to re-read buffer from start
	bic	rva, sv5, #0x10		@ rva <- paren-count and paren-inc (comment indic clrd)
	eq	rva, #0x05		@ did we get a full expr (par-cnt=0 & par-inc=1, sch int)?
	beq	rdexp3			@	if so,  jump to extract and parse
	b	readxt			@ jump to return eof
	
rdexp4:	@ closing parenthesis or double-quote encountered
	@ check if this is acceptable as end of expression
	eq	sv5, #0x15		@ is par-cnt=0, par-inc=1, comment=#f (scheme int)?
	bne	rdexp0
	cdr	rva, sv1		@ rva <- port-vector
	vcrfi	rva, rva, 5		@ rva <- value of 'wait-for-cr' from port
	eq	rva, #t			@ wait for cr?
	beq	rdexp0
	b	rdexp3

rdexp2:	@ full datum identified, ends in space or cr -- return expr or wait for cr
	cdr	rva, sv1		@ rva <- port-vector
	vcrfi	rva, rva, 5		@ rva <- value of 'wait-for-cr' from port
	eq	rva, #t			@ wait for cr?
	it	eq
	eqeq	rvb, #' 		@	if so,  was last char a space?
	beq	rdexp0			@		if so,  jmp to cont read from prt (til cr)
rdexp3: @ extract expression from buffer/file based on sv2 and sv4 (pos/descriptor)
	bl	getc2			@ sv1 <- string read-in, side-effect: buffer/desr updtd
	pntrp	sv1			@ was a string acquired?
	bne	readxt			@	if not, jump to exit with eof
  .ifndef r3rs
	sav__c				@ dts <- (cnt ...)	
	call	adr_parse		@ sv1 <- parsed expression
    .ifdef exclude_lib_mod
	restor	cnt			@ cnt <- cnt, dts <- (...)
	b	adr_expand		@ sv1 <- parsed expr with expanded macros, return via cnt
    .else
	pairp	sv1
	itTT	eq
	snoceq	sv2, sv4, sv1
	ldreq	sv3, =var_library
	eqeq	sv2, sv3
	it	ne
	restorne cnt			@ cnt <- cnt, dts <- (...)
	bne	adr_expand		@ sv1 <- parsed expr with expanded macros, return via cnt
	save	env
	car	sv3, sv4
	vcsti	glv, 14, sv3
	cdr	env, sv3
	call	adr_expand
	set	sv2, null
	vcsti	glv, 14, sv2
	vcsti	glv, 15, sv2
	restor	env, cnt
	set	pc,  cnt
    .endif
  .else
	b	adr_parse		@ sv1 <- parsed expression, return via cnt
  .endif
	
readxt:	@ return eof
	ldr	sv1, =eof_char		@ sv1 <- eof
	set	pc,  cnt		@ return


	/* character output port write sub-function (common to uart, file, usb) */
	PRIMIT	chpwrt, ufun, 2
	@ in:	sv1 <- object
	@ in:	sv2 <- ((port offset ...) . port-vector) = full output port
	sav__c				@ dts <- (cnt ...)
	call	rwrite
	restor	cnt
	b	adr_npofxt
	
_func_	
rwrite:	@ [internal entry] -- recursive entry point
	@ in:	sv1 <- object
	@ in:	sv2 <- ((port offset ...) . port-vector) = full output port
	@ write external representation of object in sv1 to port in sv2
	@ return via cnt
	bl	typsv1			@ rva <- type tag of obj1 (sv1)
	set	lnk, cnt
	eq	rva, #i0
	it	ne
	eqne	rva, #f0
	it	ne
	eqne	rva, #rational_tag
	it	ne
	eqne	rva, #complex_tag
	beq	wrtnum
	eq	rva, #char_tag
	beq	wrtcha
	eq	rva, #variable_tag
	beq	wrtvar
	eq	rva, #vector_tag
	beq	wrtvec
	eq	rva, #string_tag
	beq	wrtstr
	eq	rva, #bytevector_tag
	beq	wrtvu8
	eq	rva, #list_tag
	beq	wrtlst
	eq	rva, #symbol_tag
	itT	ne
	ldrne	sv1, =null__
	eqne	rva, #null
	itT	ne
	ldrne	sv1, =true__
	eqne	rva, #t
	itT	ne
	ldrne	sv1, =false_
	eqne	rva, #f
	itT	ne
	ldrne	sv1, =proc__
	eqne	rva, #procedure
	beq	prtwrc
	b	adr__err

_func_	
wrtcha:	@ write-out a character
	eq	sv1, #npo		@ is char the non-printing object?
	it	eq
	seteq	pc,  cnt
	caar	sv4, sv2		@ rvc <- port address
	tst	sv4, #int_tag		@ is port-address an int? (i.e. doing write, not display)
	beq	prtwrc			@	if so,  write the char out and return via lnk
	save	sv1			@ dts <- (char port ...)
	set	sv1, pound_char		@ sv1 <-  pound, #, (scheme char)
	bl	prtwrc			@ write pound out
	set	sv1, backslash_char	@ sv1 <-  backslash, \, (scheme char)
	bl	prtwrc			@ write backslash out
	restor	sv1			@ sv1 <- char,  dts <- (port ...)
	set	lnk, cnt
	b	prtwrc			@ write the space out and return via lnk/cnt

_func_	
wrtvar:	@ write-out a variable
	sav_rc	sv2
	adr	cnt, r2wstx
	b	adr_sym2str

_func_	
wrtnum:	@ write-out a number
	sav_rc	sv2
	set	sv2, null
	adr	cnt, r2wstx
	vcrfi	rva, glv, 16		@ rva <- pre-ntry fun tbl	
	ldr	rva, [rva, #(onumstr << 2)]
	orr	rva, rva, #lnkbit0
	set	pc,  rva		@ jump to num2str, return to r2wstx

_func_	
wrtstr:	@ write-out a string
	save	sv1			@ dts <- (string port ...)
	caar	sv1, sv2		@ rvc <- port address
	tst	sv1, #int_tag		@ is port-address an int? (i.e. doing write, not display)
	itT	ne
	setne	sv1, dbl_quote_char	@ sv1 <- double quote (scheme char)
	blne	prtwrc			@	if so,  write double quote out
	restor	sv1			@ sv1 <- string,		dts <- (port ...)
	bl	prtwrc			@ write string out
	caar	sv1, sv2		@ rvc <- port address
	tst	sv1, #int_tag		@ is port-address an int? (i.e. doing write, not display)
	itT	ne
	setne	sv1, dbl_quote_char	@ sv1 <- double quote (scheme char)
	blne	prtwrc			@	if so,  write double quote out
	set	pc,  cnt

_func_	
wrtvec:	@ write-out a vector
	sav_rc	sv1			@ dts <- (vector cnt ...)
	set	sv1, pound_char		@ sv1 <- pound (scheme char)
	bl	prtwrc			@ write pound out
	set	sv1, open_par_char	@ sv1 <- open parenthesis (scheme char)
	bl	prtwrc			@ write open parenthesis out
	set	sv1, i0			@ sv1 <- 0 = offset (scheme int)
wrtvlp:	save	sv1			@ dts <- (offset vector cnt ...)
	set	rvc, sv1		@ rvc <- item offset (scheme int)
	cadr	sv1, dts		@ sv1 <- vector
	veclen	rva, sv1		@ rva <- vector length (scheme int)	
	cmp	rvc, rva
	bpl	wrtvdn
	cmp	rvc, #i1
	it	pl
	blpl	wrtspc
	car	sv1, dts
	bic	rvc, sv1, #1
	cadr	sv1, dts
	ldr	sv1, [sv1, rvc]
	call	rwrite
	restor	sv1
	add	sv1, sv1, #4
	b	wrtvlp

_func_	
wrtvu8:	@ write-out a bytevector
	sav_rc	sv1			@ dts <- (bytevector cnt ...)
	ldr	sv1, =vu8str
	bl	prtwrc			@ write string out
	set	sv1, open_par_char	@ sv1 <- open parenthesis (scheme char)
	bl	prtwrc			@ write open parenthesis out
	set	sv1, i0			@ sv1 <- 0 = item offset (scheme int)
wrtv8l:	save	sv1			@ dts <- (offset bytevector cnt ...)
	set	rvc, sv1		@ rvc <- item offset (scheme int)
	cadr	sv1, dts		@ sv1 <- bytevector
	vu8len	rva, sv1
	cmp	rvc, rva
	bpl	wrtvdn
	cmp	rvc, #i1
	it	pl
	blpl	wrtspc
	car	sv1, dts
	int2raw	rvc, sv1
	cadr	sv1, dts
	ldrb	rvc, [sv1, rvc]
	raw2int	sv1, rvc
	call	rwrite
	restor	sv1
	add	sv1, sv1, #4
	b	wrtv8l
wrtvdn:	@ done
	restor	rva, rvb, cnt
	b	wrtclp
		
_func_	
wrtlst:	@ write the contents of a list
	save	sv1			@ dts<- (list ...)
	set	sv1, open_par_char	@ sv1 <- open parenthesis (scheme char)
	bl	prtwrc			@ write open parenthesis out
wrtcac:	@ write the car and then the cdr of the list
	@ dts <- (list ...)
	caar	sv1, dts		@ sv1 <- car-of-list
	sav__c				@ dts <- (cnt ...)
	call	rwrite
	restor	cnt
	@ write the cdr of a proper or improper list
	@ dts <- (list ...)
	cdar	sv1, dts
	nullp	sv1
	it	ne
	blne	wrtspc
	restor	sv1			@ sv1 <- list,		dts <- (port ...)
	cdr	sv1, sv1		@ sv1 <- cdr-of-list
	nullp	sv1			@ is cdr-of-list null?
	beq	wrtclp			@	if so,  jump to write closing parenthesis
	save	sv1			@ dts <- (cdr-of-list port ...)
	pairp	sv1
	beq	wrtcac			@	if so,  jump to write cdr as a proper list
wrtipl:	@ write the cdr of an improper list
	@ dts <- (cdr-of-list ...)
	set	sv1, dot_char		@ sv1 <- dot (scheme char)
	bl	prtwrc			@ write dot out
	bl	wrtspc
	restor	sv1			@ sv1 <- cdr-of-list,		dts <- (...)
	sav__c				@ dts <- (cnt ...)
	call	rwrite
	restor	cnt
wrtclp:	@ write backspace, closing parenthesis, space, then exit
	@ dts <- (port ...)
	set	sv1, close_par_char	@ sv1 <- close parenthesis (scheme char)
	set	lnk, cnt
	b	prtwrc			@ write the space out and return via lnk/cnt

_func_
r2wstx:	@ restore sv2 and cnt, then write string in sv1 and return via cnt
	restor	sv2, cnt
	set	lnk, cnt
	b	prtwrc			@ write the space out and return via lnk/cnt

_func_
wrtspc:	@ write space and return
	set	sv1, space_char		@ sv1 <- space (scheme char)
	b	prtwrc			@ write the space out and return via lnk

	SMBL	"()",	   null__
	SMBL	"#t",	   true__
	SMBL	"#f",	   false_
	SMBL	"#<proc>", proc__
	SMBL	"#vu8",	   vu8str
	SMBL	"?exp",	   badexp
	SMBL	"ap> ",	   prmpt_


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*------------------------------------------------------------------------------
@  II.B.6.     Standard Procedures
@  II.B.6.6.   Input and Output
@  II.B.6.6.2. input:				read [parse]
@-----------------------------------------------------------------------------*/

	/* (parse expr) */
	PRIMIT	"parse", pfun, 1
	@ in:	sv1 <- expression as string
	@ out:	sv1 <- parsed scheme expression (ready for eval)
	set	sv4, 0			@ sv1 <- 0 (bottom of parse stack)
	save	sv1, sv4		@ dts <- (expr-string 0 ...)
	set	sv5, i0			@ sv5 <- offset to 1st char (scheme int)	
parse0:	@ parse expression on stack
	@ sv5 <- offset of previous char
	@ dts <- (expr <result> 0 ...)
	car	sv4, dts		@ sv4 <- expr
	strlen	sv3, sv4		@ sv3 <- number of chars in expr (scheme int)
	cmp	sv5, sv3		@ did we process all chars?
	bpl	parsxt			@	if so,  jump to exit
	@ skip over leading backspace, tab, lf, cr, space
	bytref	rva, sv4, sv5		@ rva <- char
	cmp	rva, #'!		@ is char a control character (eg. tab, lf, cr, space ...)
	it	mi
	addmi	sv5, sv5, #4		@ 	if so,  sv5 <- ofst of nxt char to get frm bfr
	bmi	parse0			@	if so,  jump to keep looping over chars
	@ check for beginning of comment
	eq	rva, #'; 		@ is char a semi-colon? (begining of comment)
	beq	prsskp
	@ check for a start of list, quoted or quasiquoted expression
	set	sv1, 0x60		@ sv1 <- indicator for open parenthesis
	eq	rva, #'(		@ is char an open parenthesis?
	itT	ne
	setne	sv1, 0x20		@	if not, sv1 <- "quote" indicator
	eqne	rva, #''		@	if not, is char a quote?
	itT	ne
	setne	sv1, 0x30		@	if not, sv1 <- "backquote"=backquote_char indicator
	eqne	rva, #'`		@	if not, is char a backquote?
	beq	prsrcr
	@ check for the cdr of an improper list
	add	sv1, sv5, #4		@ sv1 <- offset to next char
	bytref	rvb, sv4, sv1		@ rvb <- next char
	eq	rva, #'.		@ is char a dot?
	it	eq
	eqeq	rvb, #' 		@	if so,  is next char a space? (improper list)
	it	eq
	ldreq	sv1, =coreoba		@	if so,  sv1 <- "cons" (indicator for improper list)
	beq	prsrcr
	@ check for an unquoted or unquoted-spliced expr (uses rvb <- next char, extracted above)
	eq	rva, #',		@ is char a comma?
	beq	prsuqt
	@ check for an end of list
	eq	rva, #')		@ is char a close parenthesis?
	beq	prsclp			@ 	if so, jump to recover list from stack
	@ default: parse a token
	sav__c				@ dts <- (cnt expr <result> 0 ...)
	call	toksch			@ sv1 <- token conv to scheme, or list->vector indic
	restor	cnt			@ cnt <- cnt,  dts <- (expr <result> 0 ...)
  .ifndef exclude_lib_mod
	ldr	rva, =var_library
	eq	sv1, rva
	itT	ne
	ldrne	rvb, =var_export
	eqne	sv1, rvb
	itT	ne
	ldrne	rvc, =var_import
	eqne	sv1, rvc
	beq	parslb
  .endif
	ldr	rva, =tokvec		@ rva <- [indicator that list needs conversion to vector]
	eq	sv1, rva		@ need to perform list->vector?
	itT	ne
	ldrne	rva, =tokvu8		@ rva <- [indicator that list needs conversion to vector]
	eqne	sv1, rva		@ need to perform list->vector?
	bne	parse5			@	if not, jump to keep parsing
	set	sv2, null
	tuck	sv2, sv3
	ldr	rva, =tokvec		@ rva <- [indicator that list needs conversion to vector]
	eq	sv1, rva		@ need to perform list->vector?
	itE	eq
	seteq	sv1, 0x10		@ sv1 <- indicator for list->vector
	setne	sv1, 0x70		@ sv1 <- indicator for u8-list->bytevector
parse5:	tuck	sv1, sv2		@ dts <- (expr token ... 0 ...), sv2=tmp
parsqt:	@ check if there's an operation underneath result
	@ in:	dts <- (expr result operation-or-0 ...)
	cdr	sv2, dts		@ sv2 <- (result operation-or-0 ...)
	cdr	sv3, sv2		@ sv3 <- (operation-or-0 ...)
	car	sv1, sv3		@ sv1 <- operation or 0
	eq	sv1, #0			@ is there no operation to perform on result?
	it	eq
	addeq	sv5, sv5, #4		@ 	if so,  sv5 <- ofst nxt chr to get frm bfr
	beq	parse0			@	if so,  jump to parse expression following list
	@ check if operation is quote, quasiquote, unquote or unquote-splicing
	ldr	rva, =var_quote		@ sv4 <- "quote" scheme symbol-id
	eq	sv1, #0x20		@ is "quote" underneath?
	itT	ne
	ldrne	rva, =var_quasiquote	@	if not, sv4 <- "backquote" scheme symbol-id
	eqne	sv1, #0x30		@	if not, is "backquote" underneath?
	itT	ne
	ldrne	rva, =var_unquote	@	if not, sv4 <- "unquote" scheme symbol-id
	eqne	sv1, #0x40		@	if not, is "unquote" underneath?
	itT	ne
	ldrne	rva, =var_unqtsplc	@	if not, sv4 <- "unquote-splicing" scheme symbol-id
	eqne	sv1, #0x50		@	if not, is "unquote-splicing" underneath?
	beq	parsqu			@	if so,  jump to perform quotation/unquotation
	@ check if operation is to convert list to vector (pound underneath result)
	eq	sv1, #0x10		@ convert list to vector?
	it	ne
	eqne	sv1, #0x70		@	if not, convert list to bytevector?
	it	ne
	addne	sv5, sv5, #4		@ 	if so,  sv5 <- ofst nxt chr to get frm bfr
	bne	parse0			@	if not,  jump to parse expression following list
	set	sv4, sv1
	@ perform list->vector or list->bytevector
	@ and clear # and () from stack, then branch back to parse0
	car	sv1, sv2		@ sv1 <- result (vector as list)
	cdr	sv2, sv3		@ sv2 <- (() ...)
	car	sv3, dts		@ sv1 <- expr
	cdr	dts, sv2		@ dts <- (...)
	car	sv2, dts		@ sv2 <- quote or unquote or something else
	save	sv5, sv3		@ dts <- (char-offset expr ...)
	sav_rc	sv4			@ dts <- (vec-type-indicator cnt char-offset expr ...)
	eq	sv2, #0x30		@ is "backquote" underneath?
	bne	parsqS			@	if not, jump to keep going
	call	adr_quasiquote		@ sv1 <- quasiquote-expanded expression (vector as list)
parsqS:	@ get list length
	set	sv5, sv1		@ sv5 <- result (as list) saved
	set	sv2, i0			@ sv2 <- 0, initial length of list
parsqT:	nullp	sv1			@ done with list?
	itT	ne
	cdrne	sv1, sv1		@	if not, sv1 <- rest of list
	addne	sv2, sv2, #4		@	if not, sv2 <- upated list size
	bne	parsqT			@	if not, jump to keep counting
	@ convert list to vector or bytevector
	set	sv1, sv2		@ sv1 <- list length (scheme int)
	set	sv2, null		@ sv2 <- '() = fill for vector
	restor	sv4			@ sv4 <- vec-type-indic, dts <- (cnt chr-ofst expr ...)
	eq	sv4, #0x70		@ rebuilding a bytevector?
	beq	prmkv8			@	if so,  jump to that case
	@ convert list to vector and return to parsing
	call	adr_make_vec		@ sv1 <- new, cleared vector of size sv1
	set	sv2, i0			@ sv2 <- position of 1st vector item
lstve1:	nullp	sv5			@ done copying?
	beq	lstvxt			@	if so,  jump to exit
	snoc	sv4, sv5, sv5		@ sv4 <- next item,  sv5 <- remaining items
	vecset	sv1, sv2, sv4		@ store item in vector
	incr	sv2, sv2		@ sv2 <- position of next vector item
	b	lstve1			@ jump to continue copying from list to vector
prmkv8:	@ convert list to bytevector and return to parsing
	call	makvu8			@ sv1 <- new, cleared bytevector of size sv1
	set	rvc, 0
prmv80:	nullp	sv5
	beq	lstvxt
	snoc	sv4, sv5, sv5		@ sv4 <- next item,  sv5 <- remaining items
	int2raw	rvb, sv4
	strb	rvb, [sv1, rvc]
	add	rvc, rvc, #1
	b	prmv80
lstvxt:	@ return when done
	restor	cnt, sv5		@ cnt <- cnt, sv5 <- char-offset, dts <- (expr ...)
	b	parse5			@ jump to continue parsing

.ifndef exclude_lib_mod

parslb:	@ special considerations for (library ...) or (export ...) or (import ...)
	cadr	sv3, dts		@ sv3 <- possible list indicator on stack
	eq	sv3, #0x60		@ is library, export, import at beginning of list?
	bne	parse5			@	if not, jump back for normal processing
	eq	sv1, rvb		@ is it (export ...)?
	it	eq
	vecseteq glv, 15, i1		@ 	if so,  set library-parse-export-mode-5 in glv
	beq	parse5			@	if so,  jump to continue parsing  
	eq	sv1, rvc		@ is it (import ...)?
	beq	parsir			@	if so,  jump to that case
	@ (library ...)
	@ build new built-in env vector for this library, and set it in glv for post-parsing ops
	vcsti	glv, 15, i5		@ set library-parse-mode-13 in glv for zrslb
	bl	zrslb			@ sv3 <- extended initial env
	save	sv3
	vcsti	glv, 15, i3		@ set library-parse-mode-13 in glv for zrslb & tokend
	bl	zrslb			@ sv3 <- extended initial obarray
	restor	sv4
	cons	sv3, sv3, sv4
	vcsti	glv, 14, sv3
	b	parse5

parsir:	@ (import ...)
	vcrfi	sv3, glv, 15		@ sv3 <- current parse mode
	nullp	sv3			@ nothing special mode (i.e. not inside (library ...))?
	it	eq
	vecseteq glv, 15, i2		@	if so,  set parse-mode flag for tokend
	beq	parse5			@	if so,  jump to continue parsing
	car	sv4, dts		@ sv4 <- expr
	savrec	sv1			@ dts <- (import_var env cnt expr <result> 0 ...)
	save	sv5			@ dts <- (char-ofst imprt_var env cnt expr <rslt> 0 ...)
	set	sv1, sv4		@ sv1 <- expr
	set	sv2, sv5		@ sv2 <- char-offset
prsir1:	@ find lib name in input string
	add	sv2, sv2, #4		@ sv2 <- char-offset, updated for next char
	bytref	rva, sv1, sv2		@ rva <- char
	eq	rva, #')		@ is char a close parenthesis (end of (import ...))?
	beq	prsir9			@	if so,  done importing, jump to finish up
	eq	rva, #'(		@ is char an open parenthesis (start of lib name)?
	bne	prsir1			@	if not, jump to keep looking for open par
	set	sv3, sv2		@ sv3 <- start offset
	@*********** may need removal if name not copied correctly *************
	add	sv2, sv2, #4		@ sv2 <- start offset for lib-name copy
	set	rvb, 1			@ rvb <- 1 = paren count
prsir2:	add	sv3, sv3, #4		@ sv3 <- updated char offset
	bytref	rva, sv1, sv3		@ rva <- char
	eq	rva, #'(		@ is char an open parenthesis?
	it	eq
	addeq	rvb, rvb, #1		@	if so,  rvb <- updated open-par count
	eq	rva, #')		@ is char a close parenthesis?
	it	eq
	subseq	rvb, rvb, #1		@	if so,  rvb <- updated par count, is it zero?
	bne	prsir2			@	if not, jump to keep looking for end of lib name
	@ find lib in lib-flash and store its exports in this-lib's-env
	save	sv1, sv3		@ dts <- (exp lb-nm-end chr-of imprt_vr nv cnt e <res> 0 .)
	bl	subcpy			@ sv1 <- lib to import (name string)
	bl	lbimp			@ rvb <- lib exprt ofst | 1, sv5/sv4 <-lib exprt sub-nv/oba
	eq	rvb, #1			@ library found?
	itTTT	ne
	vcrfine	sv1, glv, 14		@	if so,  sv1 <- (this-lib-oba . this-lib-env)
	snocne	sv1, sv2, sv1
	strne	sv4, [sv1, rvb]		@	if so,  store lib's export sub-oba in this-lib-oba
	strne	sv5, [sv2, rvb]		@	if so,  store lib's export sub-oba in this-lib-oba

	@ continue to process next lib in input string
	restor	sv1, sv2		@ sv1 <- exp, sv2<-lb-nm-nd,dts<-(cof imv nv cn e <r> 0 .)
	b	prsir1			@ jump to process next lib

prsir9:	@ done importing, resume parsing
	restor	sv5, sv1		@ sv5 <- char-off, sv1<-imp_var,dts<-(nv cnt exp <re> 0 .)
	restor	env, cnt		@ env <- env, cnt <- cnt, dts <- (expr <result> 0 ...)
	@ note:	here, we skip re-parsing of import form and, essentially, set it to (import) for
	@	execution (i.e., no args). Alternative would be to set glv, 15 to #9.
	sub	sv5, sv2, #4		@ sv5 <- ofst to before closing paren in (import ...)
	b	parse5			@ jump to continue parsing

.endif	@ do not exclude_lib_mod


parsqu:	@ process quoted expression (cons quote in front of it)
	set	sv1, rva		@ sv1 <- scheme symbol-id for the type of quote
	snoc	sv2, sv3, sv2		@ sv2 <- result,		sv3 <- (operation ...)
	list	sv2, sv2		@ sv2 <- (result)
	cdr	sv3, sv3
	bcons	sv2, sv1, sv2, sv3	@ sv2 <- (updated-result ...)	
	car	sv1, dts		@ sv1 <- expr
	cons	dts, sv1, sv2		@ dts <- (expr updated-result ... 0 ...)
	b	parsqt			@ jump to process possible operation underneath new-result
	
prsskp:	@ skip over a comment
	add	sv5, sv5, #4		@ sv5 <- offset to next character
	cmp	sv5, sv3		@ processed all chars?  (NEW -- to remove data abort issue)
	bpl	parsxt			@	if so,  jmp to exit (NEW -- remov data abrt issue)
	bytref	rva, sv4, sv5		@ rva  <- char
	eq	rva, #'\r		@ is char a cr? (end of comment)
	it	ne
	eqne	rva, #'\n		@	if not, is char a line feed?
	bne	prsskp			@	if not, jump to keep scanning comment chars
	add	sv5, sv5, #4		@ sv5 <- offset of next char to get from buffer
	b	parse0			@ jump to parse expression following comment

prsuqt:	@ unquote or unquote-splicing
	eq	rvb, #'@		@ is next char a @?
	itTE	eq
	addeq	sv5, sv5, #4		@	if so,  sv5 <- offset to @
	seteq	sv1, 0x50		@	if so,  sv1 <- "unquote-splicing" indicator
	setne	sv1, 0x40		@	if not, sv1 <- "unquote" indicator
prsrcr:	@ tuck indicator into stack and recurse on the parsing
	tuck	sv1, sv2		@ dts <- (expr "cons" ... 0 ..), sv2=tmp
	add	sv5, sv5, #4		@ sv5 <- offset of next char to get from buffer
	b	parse0			@ jump to parse expression following dot of improper list
	
prsclp:	@ recover a parsed list from stack
	restor	sv4			@ sv4 <- expr,  dts <- ((itn itn-1 ... it2 it1) ... 0 ...)
	eq	sv4, #0			@ no expression on stack?
	it	eq
	seteq	sv1, 0
	beq	prserr			@	if so,  jump to parse error
	set	sv2, null		@ sv2 <- ()
	set	sv3, 0x60		@ sv3 <- open parenthesis = end of list-on-stack indicator
dstls0:	restor	sv1			@ sv1 <- stck item, itn,  ds<-(itn-1 .. it2 it1 "(" . 0 .)
	eq	sv1, #0			@ parse stack exhausted with no opening parenthesis?
	beq	prserr			@	if so,  jump to parse error
	eq	sv1, sv3		@ is stack item a "(" ?
	beq	dstls1			@	if so, we're done with the stack
	ldr	rva, =coreoba		@ rva <- "cons" = improper list
	eq	sv1, rva		@ is stack item "cons"
	it	eq
	careq	sv2, sv2		@	if so,  sv2 <- 1st item of current cdr
	beq	dstls0			@	if so, jump to continue processing stack
	cons	sv2, sv1, sv2		@ sv2 <- (itn itn+1 ...)
	b	dstls0			@ jump to continue processing stack

dstls1:	@ finish up

  .ifndef exclude_lib_mod
	pntrp	sv2			@ non-null list result?
	bne	dstlxt			@	if not, jump to return
	car	rvc, sv2		@ rvc <- item at start of list
	set	rva, null
	ldr	rvb, =var_library
	eq	rvc, rvb		@ did we recover a (library ...) list?
	itTTT	eq
	vcrfieq	sv3, glv, 14
	cdreq	sv1, sv2
	setcareq sv1, sv3
	vcstieq	glv, 14, rva
	itTT	ne
	setne	rva, i0
	ldrne	rvb, =var_export
	eqne	rvc, rvb		@	if not, did we recover a (export ...) list?
	beq	dstls2			@	if so,  jump to update lib-parse-export-mode in glv
	ldr	rvb, =var_import
	eq	rvc, rvb		@	if not, did we recover a (import ...) list?
	bne	dstlxt
	vcrfi	rvb, glv, 15
	eq	rvb, #i2		@ are we in nothing-special-mode?
	it	eq
	seteq	rva, null		@	if so,  rva <- () to clear parse mode
dstls2:	@ update parse mode
	vcsti	glv, 15, rva		@	if so,  clear library-parse-export-mode in glv
  .endif  @ do not exclude_lib_mod

dstlxt:	@ return
	save	sv4, sv2		@ dts <- (expr (it1 ... itn) ... 0 ...)
	b	parsqt			@ jump to process possib op under recov lst

parsxt:	@ recover result from stack (expr <result> 0 ...) and exit
	cdr	dts, dts		@ (<result> 0 ...)
	restor	sv1			@ sv1 <- result or 0,  dts <- (<0> ...)
	eq	sv1, #0			@ did parsing return nothing?
	itTE	eq
	seteq	sv1, npo		@	if so,  sv1 <- non-printing character
	seteq	rva, 0
	restorne rva			@	if not, dts <- (...)
	eq	rva, #0
	bne	prserr
	set	pc,  cnt

prserr:	@ parse error, for example no opening parenthesis for a closing parenthesis
	@ Note: parser doesn't catch the opposite case (eg. "(hello") and leaves residue
	@ on stack -- need to clear stack using ctrl-c in this case.
	ldr	sv1, =badexp
	b	adr__err


toksch:	@ convert string token to scheme internal representation
	@ in:	sv4 <- buffer
	@ in:	sv5 <- offset to start of token
	@ in:	dts <- (cnt expr ... 0 ...)
	bytref	rva, sv4, sv5		@ rva <- char
	@ check if there's a single char to convert
	strlen	sv3, sv4		@ sv3 <- number of chars in expr (scheme int)
	sub	sv3, sv3, #4		@ sv3 <- offset to last char (scheme int)
	cmp	sv5, sv3		@ single char?
	bpl	tok1ch			@	if so,  jump to process single char cases
	@ conversion for case where 1st two chars of token may determine type
	add	sv1, sv5, #4		@ sv1 <- offset to next char
	bytref	rvb, sv4, sv1		@ rvb <- next char
	@ test for double-quote-something => string
	eq	rva, #'"		@ is char a double quote "? (start of string)
	beq	tokstr			@	if so, jump to convert token to string
	@ test for pound-something => number, vector, char, #t/#f or symbol
	eq	rva, #'#		@ is char a #  --  pound?
	beq	tokpnd			@	if so,  jump to convert accordingly
	@ test for minus-dot => number
	eq	rva, #'-		@ is char a minus sign "-"?
	itT	eq
	seteq	rva, rvb
	eqeq	rvb, #'.		@	if so,  is following char a dot?
	beq	toknum
	@ test for dot-space => cdr of improper list
	eq	rva, #'.		@ is char a dot?
	itT	eq
	seteq	rva, rvb
	eqeq	rvb, #0x20		@	if so,  is following char a space?
	beq	tokils			@	if so,  jump to conv token to cdr of imprpr lst
tok1ch:	@ conversion for case where a single char (1st char of token) determines type
	cmp	rva, #'0		@ is char less than 0  --  digit 0?
	bmi	toksym			@	if so,  jump to see if it is #something
	cmp	rva, #':		@ is char smaller than 9+1  --  digit 9, + 1?
	bmi	toknum			@	if so,  jump to convert token to number
toksym:	@ convert token to a symbol
	ldr	sv1, =adr_str2sym	@ sv1 <- [string->symbol alternate entry]
	b	tokprc			@ jump to convert token to symbol (via string)

tokpnd:	@ analyze token that starts with # (pound)
	eq	rvb, #'\\		@ is next char a \?
	beq	tokchr			@	if so,  jump to convert token to char
	eq	rvb, #'(		@ is next char an open parenthesis?
	beq	tokvec			@	if so,  jump to convert token to vector
	eq	rvb, #'v		@ is next char a v (eg. #vu8())?
	it	eq
	bleq	tokvu8			@	if so,  jump to possibly conv token to bytevector
	set	sv2, false		@ sv2 <- #f (default for tokcbe)
	eq	rvb, #'t		@ is next char a t?
	itE	eq
	seteq	sv2, true		@	if so,  sv2 <- #t (for tokcbe)
	eqne	rvb, #'f		@	if not, is next char a f?
	beq	tokcbe			@	if so,  jump to convert to #t/#f
	bic	rvb, rvb, #0x20		@ rvb <- char following pound, without case (lower->upper)
	eq	rvb, #'I		@ is it an I or i? => inexact number
	it	ne
	eqne	rvb, #'B		@	if not, is it a  B or b? => binary number
	it	ne
	eqne	rvb, #'O		@	if not, is it an O or o? => octal number
	it	ne
	eqne	rvb, #'X		@	if not, is it an X or x? => hexadecimal number
	it	ne
	eqne	rvb, #'E		@	if not, is it an E or e? => exact number
	it	ne
	eqne	rvb, #'D		@	if not, is it a  D or d? => decimal number
	bne	toksym			@	if not, jump to convert to symbol
toknum:	@ convert a token to a number
	vcrfi	rva, glv, 16		@ rva <- pre-ntry fun tbl	
	ldr	rva, [rva, #(ostrnum << 2)]
	orr	sv1, rva, #lnkbit0
	b	tokprc			@ jump to convert token to number (via string)
	
tokvec:	@ schemize a vector (via list, parse-quote and list->vector)
	ldr	sv1, =tokvec		@ sv1 <- [list->vector indicator]
	set	pc,  cnt		@ return (caller will check sv1 and proceed accordingly)

tokvu8:	@ schemize a bytevector, possibly, (via list and u8-list->bytevector)
	@ called by bl to be able to return to parsing if token is a symbol (eg. #vwxyz)
	add	sv2, sv5, #16		@ sv2 <- offset to open parenthesis if #vu8()
	cmp	sv2, sv3		@ enough chars for #vu8()?
	it	pl
	setpl	pc,  lnk		@	if not, return to parsing
	bytref	rvc, sv4, sv2		@ rvc <- last char to check
	eq	rvc, #'(		@ is it an open parenthesis?
	itT	eq
	subeq	sv2, sv2, #4		@	if so,  sv2 <- offset of previous char
	bytrefeq rvc, sv4, sv2		@	if so,  rvc <- previous char
	it	eq
	eqeq	rvc, #'8		@	if so,  is it 8?
	itT	eq
	subeq	sv2, sv2, #4		@	if so,  sv2 <- offset of previous char
	bytrefeq rvc, sv4, sv2		@	if so,  rvc <- previous char
	it	eq
	eqeq	rvc, #'u		@	if so,  is it u?
	it	ne
	setne	pc,  lnk		@	if not, return to parsing
	add	sv5, sv5, #12		@ sv5 <- updated char start (offset of 8, before open par.)
	ldr	sv1, =tokvu8		@ sv1 <- [u8-list->bytevector indicator]
	set	pc,  cnt		@ return (caller will check sv1 and proceed accordingly)

tokchr:	@ convert a token to a char
	add	sv5, sv1, #4		@ sv5 <- offset to character
	bytref	rva, sv4, sv5		@ rva <- char
	strlen	rvb, sv4
	sub	rvb, rvb, #4
	cmp	sv5, rvb
	bpl	tokchs
	add	rvb, sv1, #8		@ rvb <- offset to next character
	bytref	rvb, sv4, rvb		@ rvb <- next char
	bic	rvb, rvb, #0x20		@ rvb <- next char, without case
	eq	rvb, #'P		@ is next chr P? (for \sPace, \space, doesn't chk all chrs)
	it	eq
	seteq	rva, '\ 		@	if so,  rva <- ascii space
	eq	rvb, #'E		@ is next char an E? (for \nEwline, \newline)
	it	eq
	seteq	rva, '\r		@	if so,  rva <- ascii carriage return
tokchs:	raw2chr	sv2, rva		@ sv2 <- char as scheme char (saved against tokend)
	add	sv5, sv5, #4		@ sv5 <- offset to next character
tokcbe:	@ finish up for char or #t/#f
	bl	tokend			@ sv1 <- offset of char after end of char token
	sub	sv5, sv1, #4		@ sv5 <- offset of last char in char token
	set	sv1, sv2		@ sv1 <- char as scheme char
	set	pc,  cnt
	
tokstr:	@ convert token to a string
	save	sv4			@ dts <- (buffer ...)
tokst0:	@ look for closing double-quote
	bytref	rva, sv4, sv1		@ rva <- char
	eq	rva, #'\\		@ is char a \?
	it	eq
	addeq	sv1, sv1, #8		@	if so,  sv1 <- ofst of chr aftr nxt (skip next)
	beq	tokst0			@	if so,  jump to look for closing double-quote
	eq	rva, #'"		@ is char a double quote "? (end of string)
	it	ne
	addne	sv1, sv1, #4		@	if not, sv1 <- offest of next char
	bne	tokst0			@	if not, continue scanning for closing double-quote
	@ extract string
	save	sv1			@ dts <- (closing-quote-offset buffer cnt expr ...)
	set	sv1, sv4		@ sv1 <- buffer containing string
	add	sv2, sv5, #4		@ sv2 <- ofst 1st char of string (after opening dbleqot)
	car	sv3, dts		@ sv3 <- closing-quote-offset
	bl	subcpy			@ sv1 <- string characters (substring)
	@ strip 1st \ in \\ and \doublequote
	set	sv4, sv1		@ sv4 <- destination for memcpy
	strlen	sv3, sv1		@ sv3 <- number of chars in string
	set	sv5, i0			@ sv5 <- start offset = 4 (scheme int)
	ldr	rvc, [sv1, #-4]		@ rvc <- string tag, to count removed \
tokst1:	cmp	sv5, sv3		@ at end of string?
	bpl	tokst2			@	if so,  jump to finish up
	bytref	rvb, sv1, sv5		@ rvb <- previous char
	add	sv5, sv5, #4		@ sv5 <- offset of current char
	eq	rvb, #'\\		@ was previous char a \?
	bne	tokst1			@	if not, jump to keep scanning string
	set	sv2, sv5		@ sv2 <- source start offset
	sub	rvb, sv2, #4		@ rvb <- target start offset (scheme int)
	int2raw	rvb, rvb		@ rvb <- target start offset (raw int)
memcpy:	@ Copy a block of memory, one byte at a time
	@ copy from start to end
	cmp	sv2, sv3		@ are we done copying?
	it	mi
	bytrefmi rva, sv1, sv2		@ rva <- raw byte from source
	itTT	mi
	strbmi	rva, [sv4, rvb]		@ store it in target
	addmi	sv2, sv2, #4		@ sv2 <- updated source end offset
	addmi	rvb, rvb, #1		@ rvb <- updated target end address
	bmi	memcpy			@ jump to continue copying bytes
	sub	rvc, rvc, #0x0100	@ rvc <- updated string size tag
	sub	sv3, sv3, #4		@ sv3 <- updated string length (scheme int)
	b	tokst1			@ jump to keep stripping \
tokst2:	@ finish up token->string conversion
	str	rvc, [sv1, #-4]		@ update the result string tag
	restor	sv5, sv4		@ sv5 <- quote-ofst, sv4 <- buffer, dts <- (cnt expr ...)
	set	pc,  cnt

tokils:	@ schemize the cdr of an improper list
	add	sv5, sv5, #8		@ sv5 <- offset of character after next
	sav__c				@ dts <- (cnt ...)
	call	toksch			@ sv1 <- token
	restor	cnt, sv3, sv4		@ cnt <- cnt, sv3 <- upper-cnt,
					@ sv4 <- expr, dts <- (itn itn-1 .. it1 "(" ..)
tokscc:	bytref	rva, sv4, sv5		@ rva <- char -- scan for closing parenthesis
	eq	rva, #')		@ is char a closing par?
	it	ne
	addne	sv5, sv5, #4		@	if not, sv5 <- offset to next character
	bne	tokscc			@	if not, may want to chk for null to avoid inf loop
	restor	sv2			@ sv2 <- itn,	dts <- (itn-1 .. it2 it1 "(" ..)
dstil0:	restor	sv1			@ sv1 <- stack item, itn-1, dts<-(itn-2 .. it2 it1 "(" ..)
	ldr	rva, =open_par_char
	eq	sv1, rva		@ is sv1 = "(" ?
	beq	dstil1			@	if so, we're done with the stack
	cons	sv2, sv1, sv2		@ sv2 <- (itn itn+1 ...)
	b	dstil0			@ jump to process remainder of stack
dstil1:	set	sv1, sv2		@ sv1 <- result
	save	sv3, sv4		@ dts <- (upper-cnt expr ... 0 ...)
	set	pc,  cnt


_func_	
tokend:	@ return in sv1 the position of the character after the end
	@ of the read buffer token that starts at sv5
	@ in:	sv4 <- address of read buffer
	@ in:	sv5 <- start offset of token
	@ out:	sv1 <- end offset of token
	@ modifies:	sv1, sv3, rva
	set	sv1, sv5		@ sv1 <- offset to 1st char
	strlen	sv3, sv4		@ sv3 <- number of chars in expr (scheme int)
  .ifndef exclude_lib_mod
	vcrfi	rva, glv, 15
	eq	rva, #i2		@ parsing (import ...) lib names outside lib (eg. rep)?
	it	ne
	eqne	rva, #i3		@	if not, parsing this-lib's name in (lib ...)?
	beq	token1			@	if so,  use special tokend
  .endif
token0:	cmp	sv1, sv3		@ at end of expression?
	it	pl
	setpl	pc,  lnk		@	if so,  return
	bytref	rva, sv4, sv1		@ rva <- char
	eq	rva, #'\t		@ is char a tab?
	it	ne
	eqne	rva, #'\r		@	if not, is char a cr?
	it	ne
	eqne	rva, #'\n		@	if not, is char a line feed?
	it	ne
	eqne	rva, #'\ 		@	if not, is char a space?
	it	ne
	eqne	rva, #')		@	if not, is char a closing parenthesis?
	it	ne
	eqne	rva, #'(		@	if not, is char an opening parenthesis?
	it	ne
	@ The two lines below are contributed by Christophe Scholl of the Brussels Free University
	eqne	rva, #';		@	if not, is char a semi-colon (start of comment)?
	it	ne
	addne	sv1, sv1, #4		@	if not, sv1 <- offset to next char as scheme int
	bne	token0			@	if not done, jump to continue counting characters
	set	pc,  lnk		@ return

.ifndef exclude_lib_mod

token1:	@ end of token for (possibly compound) library name
	@ used for:	(library (lib name) ...) and (import (lib name1) (lib name2) ...)
	@ Note:		for (import ...) this code is used only outside of a (library ...) clause
	@		(see prsir1: for code used within a library clause).
	set	sv3, 5			@ sv3 <- initial parenthesis count (scheme int)
token2:	bytref	rva, sv4, sv1		@ rva <- char
	eq	rva, #'(		@ is char an open parenthesis?
	it	eq
	addeq	sv3, sv3, #4		@	if so,  sv3 <- updated parenthesis count
	eq	rva, #')		@ is char a close parenthesis?
	itT	eq
	subeq	sv3, sv3, #4		@	if so,  sv3 <- updated parenthesis count
	eqeq	sv3, #i0		@	if so,  is parenthesis count zero?
	it	ne
	addne	sv1, sv1, #4		@	if not, sv1 <- offset to next char
	bne	token2			@	if not, jump to look for end of compound token
	vcrfi	rva, glv, 15		@ rva <- current parse mode
	eq	rva, #13		@ are we parsing (library (lib name) ...)
	itT	eq
	seteq	rva, i0			@	if so,  rva <- 0 (scheme int)
	vcstieq	glv, 15, rva		@	if so,  set lib-parse-mode in glv (normal parse)
	set	pc,  lnk		@ return

.endif
	
tokprc:	@ convert token to string and then convert it according to func in sv1.
	@ in:	sv1 <- func
	@ in:	sv4 <- read buffer
	@ in:	sv5 <- start offset
	@ out:	sv1 <- result
	@ out:	sv5 <- updated offset
	set	sv2, sv1		@ sv2 <- func, saved against tokend
	bl	tokend			@ sv1 <- offset to char after end of token
	sav_rc	sv1			@ dts <- (end-offset cnt expr ...)
	swap	sv2, sv5, sv3		@ sv2 <- start offset,  sv5 <- func
	set	sv3, sv1		@ sv3 <- offset to char after end of token
	set	sv1, sv4		@ sv1 <- expr
	bl	subcpy			@ sv1 <- token-string
	set	sv2, null		@ sv2 <- '(), for string->number, fmt = '()
	calla	sv5			@ sv1 <- result of applying func in sv3 to token-str in sv1
	restor	sv5, cnt		@ sv5 <- end-offset, cnt <- cnt, dts <- (expr ...)
	sub	sv5, sv5, #4		@ sv5 <- offset to last char of substringed symbol item
	set	pc,  cnt

	
/*------------------------------------------------------------------------------
@  II.B.6.     Standard Procedures
@  II.B.6.6.   Input and Output
@  II.B.6.6.3. output:				prompt
@-----------------------------------------------------------------------------*/

	/* (prompt) */
	PRIMIT	"prompt", pfun, 0
	ldr	sv1, =prmpt_
	set	sv2, null
	set	sv4, (0x03<<2)|i0
	b	adr_ioprfn		@ write-out the prompt
	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.3.s.	support:		expand, match, substitute
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::*/

.ifndef r3rs

	/* var6 -- special role in macros */
	BNDVAR	"var6",  true

	/* (expand expr) */
	PRIMIT	"expand", sntx, 1
	@ expand the macros in expr
	@ in:	sv1 <- expr
	@ out:	sv1 <- expr with expanded macros
	@ macro expansion
	list	sv4, sv1		@ sv4 <- (expr) = parent
	sav_rc	sv4			@ dts <- (parent cnt ...)
mxpnds:	set	sv5, null		@ sv5 <- '() = initial macro-list
	car	sv4, dts		@ sv4 <- parent
	call	mxpndl			@ sv5 <- updated macro-list (if macros are in expr)
	nullp	sv5			@ any macros in expr?
	bne	mxpndq			@	if so,  jump to substitute and side-effect expr
	restor	sv4, cnt		@ sv4 <- parent, cnt <- cnt, dts <- (...)
	car	sv1, sv4		@ sv1 <- substituted expr
	set	pc,  cnt		@ return

mxpndl:	@ build a list of macros in an expression
	@ in:	sv4 <- parent expr (list)
	@ in:	sv5 <- current macro list
	@ out:	sv5 <- updated macro list
	sav__c				@ dts <- (cnt ...)
mxpnd5:	@ loop over items in parent list
	macrop	sv4			@ is it a macro?
	itE	eq
	vcrfieq	sv4, sv4, 1		@	if so,  sv1 <- macro body
	pairpne	sv4			@	if not, is it a list?
	itT	ne
	restorne cnt
	setne	pc, cnt
	call	mxpnd0			@ sv5 <- updated macro-list (if macros are in expr)
	cdr	sv4, sv4		@ sv4 <- rest of parent-list
	b	mxpnd5			@ jump back to keep finding macros in parent

mxpnd0:	@ expand a list or macro
	car	sv1, sv4		@ sv1 <- first item in parent list
	macrop	sv1			@ is it a macro?
	it	eq
	vcrfieq	sv1, sv1, 1		@	if so,  sv1 <- macro body
	beq	mxpnd1			@	if so,  jump to explore macro body
	pairp	sv1			@ is it a list?
	it	ne
	setne	pc,  cnt		@	if not, return
	car	rvc, sv1		@ rvc <- first item in list
	varp	rvc			@ is it a variable?
	bne	mxpnd1			@	if not, jump to explore the list
	ldr	rva,=var_syntax_rules	@ rva <- variable-ID of syntax-rules
	eq	rva, rvc		@ is var/synt = syntax-rules?
	it	eq
	seteq	pc,  cnt		@	if so,  return (don't expnd macrs insde sntx-rls)
	ldr	rva,=var_quote		@ rva <- variable-ID of quote
	ldr	rvb,=var_quasiquote	@ rvb <- variable-ID of quasiquote
	eq	rva, rvc		@ is var/synt = quote?
	it	ne
	eqne	rvb, rvc		@	if not, is var/synt = quasiquote?
	it	eq
	seteq	pc,  cnt		@	if so,  return (don't xpnd macr inside quot/quasiq)
	@ expr starts with var, see if it is bound to macro (if so extend mcaro-list)
	@ and continue to explore expr
	@ sv1 <- expr
	@ sv4 <- parent-list = ((var ...) ...)
	@ sv5 <- macro-list
	caar	sv1, sv4		@ sv1 <- var
	set	sv2, sv5		@ sv2 <- macro-list, saved against bndchk
	bl	bndchk			@ sv3 <- binding value
	car	sv1, sv4		@ sv1 <- expr, restored for later
	set	sv3, sv5		@ sv3 <- binding value
	set	sv5, sv2		@ sv5 <- macro-list, restored
	macrop	sv3			@ is var bound to macro?
	bne	mxpnd1			@	if not, jump to continue
	set	sv1, sv4		@ sv1 <- parent-list
	bcons	sv5, sv1, sv3, sv5	@ sv5 <- ((parent-list . macro) . previous-macro-list)
	car	sv1, sv4		@ sv1 <- expr, restored
mxpnd1:	@ expr is a macro (body) or list, explore it
	@ sv4 <- parent
	@ sv1 <- expr
	sav_rc	sv4			@ dts <- (sv4 cnt ...)
	set	sv4, sv1		@ sv4 <- new parent
	call	mxpndl			@ jump to explore sv1
	restor	sv4, cnt		@ sv4 <- sv4, cnt <- cnt, dts <- (...)
	set	pc,  cnt		@ return

mxpndq:	@ find match for expr in macro rules and jump to substitute
	@ sv5 <- macro-list, dts <- ((expr) cnt ...)
	nullp	sv5			@ no macros in list?
	beq	mxpnds			@ 	if so,  jump to re-scan expression for macros
	snoc	sv4, sv5, sv5		@ sv4 <- (parent . macro), sv5 <- rest-macro-list
	snoc	sv2, sv3, sv4		@ sv2 <- parent, sv3 <- macro
	@ sv3 <- macro,  sv2 <- parent, dts <- (macro-list (expr) cnt ...)
	ldmia	sv3, {sv1, sv3}		@ sv1 <- literals, sv3 <- (rule1 rule2 ...)
	save	sv2, sv1, sv5		@ dts <- (parent literals macro-list (expr) cnt ...)
	set	sv1, sv3		@ sv1 <- (rule1 rule2 ...)
evlmlp:	@ macro match loop
	@ sv1 <- (rule1 rule2 ...)
	@ dts <- (parent literals macro-list (expr) cnt ...)
	caar	sv4, sv1		@ sv4 <- pattern1
	snoc	sv3, sv5, dts		@ sv3 <- parent, sv5 <- (literals macro-lst (expr) cnt ...)
	car	sv3, sv3		@ sv3 <- expr
	car	sv2, sv5		@ sv2 <- literals
	save	sv1			@ dts <- ((rul1 rul2 ..) prnt lits macr-lst (expr) cnt ..)
	set	sv5, null		@ sv5 <- '() = initial bindings
	call	match			@ sv1 <- bindings-or-#f
	eq	sv1, #f			@ match found?
	bne	evlsbs			@	if so,  jump to substitute
	restor	sv1			@ sv1 <- (rl1 rl2 .), dts <- (prnt lits maclst (exp) cnt .)
	cdr	sv1, sv1		@ sv1 <- (rule2 ...)
	nullp	sv1			@ more rules to try?
	bne	evlmlp			@	if so,  jump to keep testing them
	@ error in expand
	b	adr__err

evlsbs:	@ macro substitute
	@ sv1 <- bindings, dts <- ((rule1 rule2 ...) parent literals macro-list (expr) cnt ...)
	set	sv4, sv1		@ sv4 <- bindings
	restor	sv3			@ sv3 <- (mtchn-rl othr-rl .),dts<-(prn lts mcls (e) cnt .)
	cadar	sv3, sv3		@ sv3 <- expr-for-subst
	call	substi			@ sv1 <- substttd-expr (tmplt=sv3, bndngs=sv4, (env)=sv2)
	restor	sv4			@ sv4 <- parent, dts <- (literals macro-lst (expr) cnt ...)
	setcar	sv4, sv1		@ side-effect parent with substituted expression
	cdr	dts, dts		@ dts <- (macro-list (expr) cnt ...)
	restor	sv5			@ sv5 <- macro-list, dts <- ((expr) cnt ...)
	b	mxpndq			@ jump back to match & subst macros in rest-of-macro-list

	/* (match expr pat bndngs lits) */
	PRIMIT	"match", sntx, 4
	@ in:	sv1 <- expr
	@ in:	sv2 <- pat
	@ in:	sv3 <- bndings
	@ in:	sv4 <- lits
	@ out:	sv1 <- updated bindings or #f
	set	sv5, sv3		@ sv5 <- bindings
	set	sv3, sv1		@ sv3 <- expr
	swap	sv2, sv4, sv1		@ sv2 <- literals,		sv4 <- pattern
match:	@ [internal entry]
	@ in:	sv2 <- literals
	@ in:	sv3 <- expression
	@ in:	sv4 <- pattern
	@ in:	sv5 <- bindings
	eq	sv5, #f			@ did cars not match?
	beq	matcfx			@	if so,  jump to exit with #f
	nullp	sv4			@ has pattern ended?
	it	eq
	nullpeq	sv3			@	if so,  has expression ended too?
	beq	matcxt			@	if so,  return with bindings
	nullp	sv4			@ has pattern ended?
	beq	matcfx			@	if so,  jump to exit with #f
	varp	sv4			@ is pattern a variable?
	beq	matcvr			@	if so,  jump to match expr to var-pattern
	eq	sv3, sv4		@ is expr = pattern?
	beq	matcxt			@	if so,  return with bindings
	pntrp	sv4			@ is pattern a pointer?
	bne	matcfx			@	if not, return with #f
	nullp	sv3			@ is expression null?
	it	ne
	pntrpne	sv3			@	if not, is expression a pointer?
	bne	matcfx			@	if not, return with #f
	sizdp	sv4
	beq	matcob			@	if so,  jump to see if pat and expr are equal?
	ratcpx	sv4
	beq	matcob			@	if so,  jump to see if pat and expr are equal?
	@ is pattern a list like (a_pattern "...")?
	cdr	sv1, sv4		@ sv1 <- cdr of pattern
	pairp	sv1			@ is cdr of ptrn a list? (i.e ptrn may be (a_ptrn ...))
	bne	matcls			@	if not, jump to match proper or improper lists
	car	sv1, sv1		@ sv1 <- cadr of pattern
	and	rva, sv1, #0xff		@ rva <- type of cadr of pattern
	eq	rva, #variable_tag	@ is cadr of pattern a variable (eg. "...")?
	bne	matcls			@	if not, jump to match proper or improper lists
	cadr	sv1, sv4		@ sv1 <- cadr of pattern
	ldr	rva, =var_ellipsis
	eq	sv1, rva		@ is cadr of pattern "..."?
	beq	matcel			@	if so,  jump to match expr to pattern-with-ellipsis
matcls:	@ pattern is a list and is not (a_pattern "...")
	pairp	sv3			@ is expression a pair?
	bne	matcfx			@	if not, exit with #f
	@ both expr and pattern are lists (match them to one another)
	cdr	sv1, sv3		@ sv1 <- rest-of-expr
	save	sv1, sv2		@ dts <- (rest-of-expr literals ...)
	cdr	sv1, sv4		@ sv1 <- rest-pattern
	car	sv3, sv3		@ sv3 <- 1st-expr
	car	sv4, sv4		@ sv4 <- 1st pattern
	sav_rc	sv1			@ dts <- (rest-pattern cnt rest-of-expr literals ...)
	call	match			@ sv1 <- bindings-or-#f
	restor	sv4, cnt		@ sv4 <- rest-ptrn, cnt <- cnt, dts <- (rst-expr lits ...)
	restor	sv3, sv2		@ sv3 <- rest-expr, sv2 <- literals, dts <- (...)
	set	sv5, sv1		@ sv5 <- bindings-or-#f
	b	match
matcob:	@ is the pattern/value (sv4) equal to the expression (sv3)?
	set	sv1, sv3		@ sv2 <- expr
	set	sv2, sv4		@ sv2 <- pattern
	sav_rc	sv5			@ dts <- (bindings cnt ...)
	call	adr_equal		@ sv1 <- #t/#f
	restor	sv5, cnt		@ cnt <- cnt, sv5 <- bindings, dts <- (...)
	eq	sv1, #t			@ were binding and pattern equal?
	bne	matcfx			@	if not, return with #f
	b	matcxt			@ return with bindings
matcvr:	@ pattern (sv4) is a variable
	@ 1) is the pattern-var (sv4) in the list of literals (sv2)?
	nullp	sv2			@ done with literals?
	it	eq
	seteq	sv1, sv5		@	if so,  sv1 <- bindings
	beq	matcv1			@	if so,  jump to look for pattern-var in bindings
	snoc	sv1, sv2, sv2		@ sv1 <- 1st literal,		sv2 <- remaining literals
	eq	sv1, sv4		@ is pattern-var the same as literal?
	bne	matcvr			@	if not, jump to continue looking up literals
	eq	sv3, sv4		@ is expr = pattern-var (a literal)
	bne	matcfx			@	if not, return with #f
	b	matcxt			@ return with bindings
matcv1:	@ 2) is the pattern-var (sv4) in the list of bindings (sv1)?
	nullp	sv1			@ are we done with bindings list?
	beq	matcv2			@	if so,  jump to make a new binding
	caar	sv2, sv1		@ sv2 <- 1st binding's var
	eq	sv2, sv4		@ is pattern-var = 1st binding's var?
	it	ne
	cdrne	sv1, sv1		@	if not, sv1 <- remaining bndngs
	bne	matcv1			@	if not, jump to keep looking through bindings
	cdar	sv4, sv1		@ sv4 <- pattern-var's value (i.e. binding value)
	b	matcob			@ jump to test expr against pattern-var's value
matcv2:	@ 3) pattern-var (sv4) is not bound,
	@    make a new binding between pattern-var (sv4) and expr (sv3)
	set	sv1, sv4		@ sv1 <- pattern-var
	bcons	sv5, sv1, sv3, sv5	@ sv5 <- ((pat-var . expr) . bindings-list)
	b	matcxt			@ return with updated bindings
matcel:	@ pattern is a list like (a_pattern "...")
	save	sv2			@ dts <- (literals ...)
	car	sv4, sv4		@ sv4 <- a_pat
	save	sv3, sv5, sv4		@ dts <- (expr bindings a_pat literals ...)
	set	sv5, null		@ sv5 <- () = new-bindings
matc_J:	restor	sv3			@ sv3 <- rest-expr,	dts <- (bnds a_pat lits ...)
	nullp	sv3			@ no more expr to match to (a_pat "...") ?
	beq	matc_K			@	if so,  jump to continue
	cdr	sv1, sv3		@ sv1 <- rest-expr
	car	sv3, sv3		@ sv3 <- expr1
	cdr	sv4, dts		@ sv4 <- (a_pat lits ...)
	save	sv5, cnt, sv1		@ dts <- (new-binds rest-expr bindings a_pat literals ...)
	snoc	sv4, sv5, sv4		@ sv4 <- a_pat,			sv5 <- (lits ...)
	car	sv2, sv5		@ sv2 <- lits
	set	sv5, null		@ sv5 <- () = initial bindings
	call	match			@ sv1 <- bnds-or-#f
	restor	sv5, cnt		@ sv5 <- new-bnds,cnt<-cnt,dts<-(rst-ex bnds a_pat lits ..)
	eq	sv1, #f			@ was match found?
	beq	matcfx			@	if not, exit with #f
	set	sv4, sv1		@ sv4 <- bindings
matc_G:	@ splice bindings into bindings list
	nullp	sv4			@ no more bindings to splice in?
	beq	matc_J			@	if so,  jump to match rest-expr to a_pat
	caar	sv2, sv4		@ sv2 <- 1st binding's variable
	set	sv3, sv5		@ sv3 <- new-bnds
matc_E:	nullp	sv3			@ no more new bindings?
	beq	matc_H			@	if so,  jump to cons binding on new-bnds
	caar	sv1, sv3		@ sv1 <- 1st new binding's variable
	eq	sv1, sv2		@ are vars the same?
	it	ne
	cdrne	sv3, sv3		@	if not, sv3 <- rest of new-bnds
	bne	matc_E			@	if not, jump to keep scanning new-bnds
	cdar	sv2, sv3		@ sv2 <- 1st new binding's value
	cdar	sv1, sv4		@ sv1 <- 1st binding's value
	cons	sv2, sv1, sv2		@ sv2 <- (1st binding's value . (1st new binding's value))
	car	sv1, sv3		@ sv1 <- 1st new binding
	setcdr	sv1, sv2		@ store updtd bndng in cdr of 1st new bndng (side-eff sv1)
	cdr	sv4, sv4		@ sv4 <- rest of bindings to splice
	b	matc_G			@ jump to splice in remaining bindings
matc_H:	car	sv1, sv4		@ sv1 <- (var1 . val1)
	snoc	sv1, sv3, sv1		@ sv1 <- var1, sv3 <- val1
	list	sv3, sv3		@ sv3 <- (val1)
	bcons	sv5, sv1, sv3, sv5	@ sv5 <- ((var1 . (val1)) . new-bnds)
	cdr	sv4, sv4		@ sv4 <- rest of bindings to splice
	b	matc_G			@ jump to splice in remaining bindings
matc_K:	restor	sv1			@ sv1 <- bnds,		dts <- (a_pat lits ...)
	cddr	dts, dts		@ dts <- (...)
matc_Z:	nullp	sv5			@ no more new bindings to cons to bnds?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	set	sv2, sv1		@ sv2 <- updated-bnds
	snoc	sv1, sv5, sv5		@ sv1 <- 1st new binding,   sv5 <- rest of new bindings
	cons	sv1, sv1, sv2		@ sv1 <- (new-binding . updated-bnds)
	b	matc_Z
matcex:	@ pop stack then exit with #f
	cddr	dts, dts
	cddr	dts, dts
matcfx:	@ exit with #f
	set	sv1, false		@ sv1 <- #f
	set	pc,  cnt		@ return with #f
matcxt:	@ exit with bindings
	set	sv1, sv5		@ sv1 <- bindings
	set	pc,  cnt		@ return with bindings

	/* (substitute bindings template) */
	PRIMIT	"substitute", sntx, 2
	@ in:	sv1 <- bindings
	@ in:	sv2 <- template
	@ out:	sv1 <- substituted expr
	set	sv4, sv1		@ sv4 <- bindings
	set	sv3, sv2		@ sv3 <- template
substi:	@ [internal entry]
	@ in:	sv3 <- template
	@ in:	sv4 <- bindings
	@ in:	dts <- (...)
	@ out:	sv1 <- substituted-template
	varp	sv3			@ is template a variable or syntax?
	beq	subs_v			@	if so,  jump to substitute a var
	@ template is not a var, is it a pointer, vector or string?	
	pairp	sv3			@ is template a list?
	itT	ne
	setne	sv1, sv3		@	if not, sv1 <- template
	setne	pc,  cnt		@	if not, return template
	@ template is a list: reverse it then subst car, subst cdr
	set	sv1, null		@ sv1 <- ()
subs_3:	set	sv2, sv1		@ sv2 <- reversed template
	snoc	sv1, sv3, sv3		@ sv1 <- 1st item,		sv3 <- rest of template
	cons	sv1, sv1, sv2		@ sv1 <- (item . reversed-template)
	pntrp	sv3			@ is cdr a pointer?
	beq	subs_3			@	if so,  continue processing template
	nullp	sv3			@ was end-of-list null (i.e template was proper list)?
	itT	eq
	seteq	sv3, sv1		@	if so,  sv3 <- reversed template
	seteq	sv1, null		@	if so,  sv1 <- ()
	beq	subs_Q			@	if so,  
	@ process a template that is an improper list
	sav_rc	sv1			@ dts <- (cnt remaining (<env>) ...)
	call	substi			@ sv1 <- rslt of subst (sv3<-tmplt,sv4<-bndngs,sv2<(env))
	restor	sv3, cnt		@ sv3 <- remaining items,  dts <- ((<env>) ...)
subs_4:	nullp	sv3			@ nothing remaining to substitute?
	it	eq
	seteq	pc, cnt			@	if so,  return
	snoc	sv3, sv5, sv3		@ sv3 <- next item to subst, sv5 <- remaining list to subst
	save	sv5, sv1		@ dts <- (remaining result (<env>) ...)
	car	sv1, dts		@ sv1 <- remaining list to subst
	sav__c				@ dts <- (cnt remaining result (<env>) ...)
	call	substi			@ sv1 <- item (tmplt is sv3, bndngs is sv4, (env) is sv2)
	restor	cnt			@ cnt <- cnt, dts <- (remaining result (<env>) ...)
	restor	sv3, sv2		@ sv3 <- rmnng itms in lst, sv2<-reslt, dts<-((<env>) ..)
	cons	sv1, sv1, sv2		@ sv1 <- updated result = (item . result)
subs_Q:	@ substitute the proper-list part of template, does it end with "..."?
	nullp	sv3			@ nothing remaining to substitute?
	it	eq
	seteq	pc, cnt			@	if so,  return
	car	rva, sv3		@ rva <- next var in template
	ldr	rvb, =var_ellipsis		@ rvb <- var "..."
	eq	rva, rvb		@ is next var = "..."?
	bne	subs_4			@	if not, jump back to keep prcssng rglr tmpl lst
	@ template does end with "...", substitute appropriately
	cdr	sv5, sv3		@ sv5 <- reversed template starting with var/tmplt
	car	sv3, sv5		@ sv3 <- var/tmplt that was followed by "..."
	save	sv5, cnt, sv1		@ dts <- (rvrsd-tmplt cnt result (<env>) ...)
	car	sv1, dts		@ sv1 <- reversed template starting with var/tmplt
	call	substi			@ sv1 <- item (tmplt is sv3, bndngs is sv4, (env) is sv2)
	set	sv5, sv1		@ sv5 <- item
	restor	sv3, cnt, sv1		@ sv3 <- rest-tmplt, cnt<-cnt, sv1<-rslt, dts<-((<env>) .)
	@ here, we'd post-check if [sv3] is a var or list and proceed accordingly
	car	sv2, sv3		@ sv2 <- pattern before substitution
	eq	sv2, sv5		@ is subst-pattern = pattern?
	it	eq
	cdreq	sv3, sv3		@	if so,  sv3 <- rest of rest tmplt
	beq	subs_4			@	if so,  no bndngs fnd, jmp bck to proc rst of pat
	pntrp	sv2			@ is var following "..." a list?
	beq	subs_L			@	if so,  jump to post-process that
	cdr	sv3, sv3		@ sv3 <- rest of rest tmplt
subs_F:	nullp	sv5			@ done with pattern?
	it	eq
	nullpeq	sv3			@	if so,  done with template?
	it	eq
	seteq	pc, cnt			@	if so,  return
	nullp	sv5			@ done with pattern (but not template)?
	beq	subs_Q			@ 	if so,  jump to that case
	set	sv2, sv1		@ sv2 <- result
	snoc	sv1, sv5, sv5		@ sv1 <- itm 1 of sbst-var/tmpl, sv5<-rst of sbst_var/tmpl
	cons	sv1, sv1, sv2		@ sv1 <- updated result
	b	subs_F			@ jump back to process rest of pattern and template
subs_L:	@ substitute many things into a list of symbols	--  eg. (do 'step' var . step) ...
	snoc	sv2, sv3, sv3		@ sv2 <- tmplt for multi-item,	sv3 <-rest of rest-tmplt
	save	sv3			@ dts <- (rest-of-rest-tmplt (<env>) ...)
	save	sv1, sv2, sv4		@ dts <- (rslt tmplt-mult bnds rst-of-rst-tmplt (<env>) .)
subsl0:	set	sv4, sv5		@ sv4 <- current substituted multi-template
	set	sv2, null		@ sv2 <- result = ()
	set	sv3, null		@ sv3 <- updated subst-template = ()
	cadr	sv5, dts		@ sv5 <- template-mult
subsl1:	car	sv1, sv4		@ sv1 <- 1st item of subst-template
	pairp	sv1
	bne	subsl2			@	if not, jump to splice it in
	car	sv1, sv1		@ sv1 <- 1st of multi-item  -- if not str, vec, sym
	cons	sv2, sv1, sv2		@ sv2 <- (1st-multi-item . other-items)
	eq	sv3, #0xd7
	beq	subsl6
	cdar	sv1, sv4		@ sv1 <- rest of 1st multi-item of subst template
	nullp	sv1			@ have we reached deepest end of multi-item?
	it	eq
	seteq	sv3, 0xd7		@	if so,  sv3 <- bltn tag as mark
	beq	subsl6
	cons	sv3, sv1, sv3
	b	subsl6
subsl2:	cons	sv2, sv1, sv2		@ sv2 <- updated result = (1st-multi-item . other-items)
	eq	sv3, #0xd7
	beq	subsl6
	car	sv1, sv4		@	if not, sv1 <- 1st item of subst-tmplt (restored)
	cons	sv3, sv1, sv3
subsl6:	cdr	sv5, sv5		@ sv5 <- rest of template mult
	cdr	sv4, sv4		@ sv4 <- rest of substituted template
	pntrp	sv5			@ is rest template-mult a pointer?
	beq	subsl1			@	if so,  jump to continue processing template
	nullp	sv5			@ done processing the multi-templt (and a propr lst)?
	itE	eq
	seteq	sv4, sv5		@	if so,  sv4 <- ()
	snocne	sv4, sv5, sv4		@	if not, sv4<-dtd it,sv5<-dtd it fr updtd sbst tmpl
subsl3: @ reverse the updated result
	nullp	sv2			@ done?
	beq	subsl4			@	if so,  jump to continue
	snoc	sv1, sv2, sv2		@ sv1 <- 1st item is result, sv2 <- rest of result
	cons	sv4, sv1, sv4		@ sv4 <- updated reversed result list
	b	subsl3			@ jump back to keep reversing
subsl4:	@ glue updated result on stack or exit with it
	set	sv1, sv4
	restor	sv2			@ sv2 <- frmr-upd-rslt,dts<-(tmp-mlt bnds rst-tmp (<nv>) .)
	cons	sv1, sv1, sv2		@ sv1 <- new updated result
	eq	sv3, #0xd7
	itTT	eq
	cdreq	dts, dts		@ 	if so,  dts <- (bindings rest-tmplt (<env>) ...)
	snoceq	sv4, dts, dts		@ 	if so,  sv4 <- bnds, dts <- (rst-tmplt (<env>) ...)
	snoceq	sv3, dts, dts		@ 	if so,  sv3<-rst-tmplt,aftr sbst ex,dts<-((<nv>) .)
	beq	subs_Q
	save	sv1			@ dts <- (new-updt-rslt tmp-mlt bnds rst-tmp (<env>) ...)
subsl5: @ reverse the updated substituted template
	nullp	sv3
	beq	subsl0
	snoc	sv1, sv3, sv3
	cons	sv5, sv1, sv5
	b	subsl5

subs_v:	@ template is a variable or syntax
	@ in:	sv3 <- var/synt (template)
	@ in:	sv4 <- substitution bindings list
	@ out:	sv1 <- something or other
	set	sv1, sv3
	set	sv5, sv4		@ sv5 <- bindings
subs_1:	nullp	sv5			@ no more bindings?
	beq	subsz0			@	if so,  jump to deal with possibly unbound var/synt
	caar	sv3, sv5		@ sv3 <- 1st binding's var
	eq	sv1, sv3		@ is var = 1st binding's var?
	it	ne
	cdrne	sv5, sv5		@	if not, sv5 <- remaining bndngs
	bne	subs_1			@	if not, keep looking through bindings
	cdar	sv1, sv5		@ sv1 <- binding value
	set	pc,  cnt		@ return binding value

subsz0:	@ template is var/synt not in bindings
	@ sv1 <- var/synt
	ldr	rva, =var_var6
	eq	sv1, rva		@ is var/synt = var6 (i.e. possibly unbound)?
	beq	subszV
	tst	sv1, #0xFF000000	@ is it a built-in var/synt?
	it	eq
	seteq	pc,  cnt
subszU:	@ is var/synt in environment?
	bl	bndchk	
	nullp	sv3			@ any binding found?
	it	ne
	setne	pc,  cnt		@	if so,  exit with var/synt
subszV:	save	sv4, sv1, cnt
subsz3:	call	adr_sym2str		@ sv1 <- external representation (string) of var/synt
	ldr	rva, [sv1, #-4]		@ rva <- full string tag
	asr	rva, rva, #8		@ rva <- string length
	sub	rva, rva, #1		@ rva <- offset to last char
	ldrb	rvb, [sv1, rva]		@ rvb <- last char of string
	add	rvb, rvb, #1		@ rvb <- last char of string + 1 (eg. x becomes y)
	strb	rvb, [sv1, rva]		@ side-effect string with new last char
	call	adr_str2sym		@ sv1 <- var/synt, sv3 <- null if new interned sym
	@ is var/synt in environment?
	nullp	sv3
	it	ne
	blne	bndchk
	nullp	sv3
	bne	subsz3
	car	sv5, dts		@ sv5 <- bindings
	@ is var/synt in cdr of a binding
subsz8:	nullp	sv5			@ no more bindings?
	beq	subszA			@	if so,  jump to exit
	snoc	sv2, sv5, sv5		@ sv1 <- 1st binding,  sv5 <- next binding
	cdr	sv2, sv2		@ sv2 <- val(s) of binding
	macrop	sv2
	it	eq
	vcrfieq	sv2, sv2, 1
	pairp	sv2
	it	ne
	bne	subsz8			@	if not, jump to keep searching
subsz9:	nullp	sv2
	beq	subsz8
	car	sv3, sv2
	eq	sv1, sv3
	beq	subsz3
	cdr	sv2, sv2
	b	subsz9
subszA:	@ exit
	set	sv5, sv1
	restor	sv4, sv3, cnt
	bcons	sv4, sv3, sv5, sv4
	set	pc,  cnt
	

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


.endif		@ for .ifndef r3rs


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



