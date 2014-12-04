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

/*

	switches used:
		cortex, hardware_FPU, FPU_is_maverick

*/

	SMBL	"0.0",  zerof_
	SMBL	"inf.", inf___
	SMBL	"nan.", nan___

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV numbers

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.2.	Numbers
@	6.2.5	Numerical operations:	number?, complex?, real?, rational?,
@					integer?,
@					exact?, inexact?
@					=, <, >, <=, >=, +, *, -, /,
@					quotient, remainder, modulo,
@					numerator, denominator
@					floor, ceiling, truncate, round,
@					exact->inexact, inexact->exact
@					zero?, positive?, negative?, odd?,
@					even?, max, min, abs, 
@					gcd, lcm, rationalize
@	6.2.6	Numerical input output:	number->string, string->number
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		flsfxt, trufxt, boolxt, corerr, notfxt
@					save, save3, cons, sav_rc, zmaloc
@
@	Modified by (switches):		CORE_ONLY, cortex
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (number? obj)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	BNDVAR	"number?", number, tb_numcpx

	/* (complex? obj)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"complex?", cpx, 1, onumgto, null, flsfxt, trufxt, trufxt, trufxt, trufxt

	/* (real? obj)			~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	BNDVAR	"real?", real, tb_numrat

	/* (rational? obj)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"rational?", rat, 1, onumgto, null, flsfxt, trufxt, trufxt, trufxt, flsfxt

	/* (integer? obj)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"integer?", int, 1, onumgto, null, flsfxt, trufxt, fltint, flsfxt, flsfxt
	@ in:	sv1 <- obj
	@ out:	sv1 <- #t/#f

.ifdef	hardware_FPU

	/* integer? for float */
	PRIMIT	fltint, ufun, 0
	bic	rva, sv1, #0x80000000	@ rva <- number, without sign
	cmp	rva, #0x4a000000	@ does number have exponent >= 148 (no fractional part)?
	bpl	adr_trufxt		@	if so,  exit with #t
	bic	rva, sv1, #0x03
  .ifndef FPU_is_maverick
	vmov	s0, rva
	vcvt.s32.f32	s0, s0
	vcvt.f32.s32	s0, s0
	vmov	rvb, s0
  .else
	cfmvsr	mvf0, rva
	cfcvts32 mvfx0, mvf0
	cfcvt32s mvf0, mvfx0
	cfmvrs	rvb, mvf0
  .endif
	eq	rva, rvb		@ are numbers equal?
	b	adr_boolxt		@ return with #t/#f based on test result
.else

	/* integer? for float */
	PRIMIT	fltint, ufun, 0
	bic	rva, sv1, #0x80000000	@ rva <- number, without sign
	cmp	rva, #0x4a000000	@ does number have exponent >= 148 (no fractional part)?
	bpl	adr_trufxt		@	if so,  exit with #t
	set	sv4, sv1
	bl	fltmte			@ sv1 <- mantissa,  rva <- exponent
	rsb	rvb, rva, #148		@ rvb <- right shft needed to get int part of num (raw int)
	int2raw	rva, sv1		@ rva <- mantissa (raw)
	bl	iround
	raw2int	sv1, rva
	bl	i12flt
	eq	sv1, sv4		@ are numbers equal?
	b	adr_boolxt		@ return with #t/#f based on test result
.endif

	/* (exact? obj)			~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"exact?", exa, 1, onumgto, null, flsfxt, trufxt, flsfxt, trufxt, flsfxt

	/* (inexact? obj)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"inexact?", inx, 1, onumgto, null, flsfxt, flsfxt, trufxt, flsfxt, trufxt

	/* (= num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBFi	eq, 0, oprdnml, t, _err, ifleq, ifleq, rcpeq, rcpeq; .ascii "="; ENDi
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- #t/#f

	/* = for int and flt */
	PRIMIT	ifleq, ufun, 0
	eq	sv1, sv2
	it	ne
	setne	sv1, false		@	if not, sv1 <- #f
	set	pc,  lnk		@ return with #f or value in sv1
	
	/* = for rat and cpx */
	PRIMIT	rcpeq, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	bl	eqrtcx
	orr	lnk, sv3, #lnkbit0
	it	ne
	setne	sv1, false		@	if not, sv1 <- #f
	set	pc,  lnk		@ return with #f or value in sv1

_func_
eqrtcx:	@ raise eq flag on equality of rat/cpx
	@ modifies:	rva-rvc
	rawsplt	rva, rvc, sv1		@ rva <- word 1, rvc <- word 2 of x1, possible rat/cpx
	ldr	rvb, [sv2, #-4]		@ rvb <- word 1 of x2, possible rat/cpx
	eq	rva, rvb		@ are word 1 of x1 and x2 the same?
	itT	eq
	ldreq	rvb, [sv2]		@	if so,  rvb <- word 2 of x2
	eqeq	rvc, rvb		@	if so,  are word 2 of x1 and x2 the same?
	set	pc,  lnk		@ return with flag

	/* (< num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"<", lt, 0, oprdnml, t, _err, intlt, fltlt, ratlt, _err
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- #t/#f

	/* < for integer */
	PRIMIT	intlt, ufun, 0
	cmp	sv1, sv2		@ is x1 < x2 ?
	bmi	cmptru			@	if so,  jump to exit with x2
cmpfls:	set	sv1, false		@ sv1 <- #f
	set	pc,  lnk		@ exit reduction with #f

	/* < for float */
	PRIMIT	fltlt, ufun, 0
	anynan	sv1, sv2
	beq	cmpfls			@	if so,  exit reduction with #f
	postv	sv1			@ is x1 positive?
	it	ne
	postvne	sv2			@	if so,  is x2 negative?
	bne	adr_intgt		@	if so,  jump to test for that
	postv	sv1			@ is x1 positive or 0?
	bne	cmptru			@	if not, exit with num2
	postv	sv2			@ is x2 positive or 0?
	bne	cmpfls			@	if not, exit with #f
	@ continue to ltint
	b	adr_intlt

	/* < for rational */
	PRIMIT	ratlt, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ rvc <- lnk, saved (and made even if Thumb2)
	bl	eqrtcx
	orr	lnk, sv3, #lnkbit0
	beq	cmpfls
	save	sv2, sv3
ltgtxt:	@ < > for rat (common exit)
	bl	adr_ratmns
	restor	sv2, sv3
	pntrp	sv1
	itTE	eq
	ldreq	rva, [sv1]
	lsleq	rva, rva, #30
	setne	rva, sv1
	postv	rva
	itE	eq
	seteq	sv1, false
	setne	sv1, sv2
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk

	/* (> num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	">", gt, 0, oprdnml, t, _err, intgt, fltgt, ratgt, _err
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- #t/#f

	/* > for integer */
	PRIMIT	intgt, ufun, 0
	cmp	sv2, sv1		@ is x1 >= x2 ?
	bpl	cmpfls			@	if not, jump to exit with #f
cmptru:	set	sv1, sv2		@ sv1 <- x2 (latest number)
	set	pc,  lnk		@ exit with num2

	/* > for float */
	PRIMIT	fltgt, ufun, 0
	anynan	sv1, sv2		@ is either x1 or x2 nan?
	beq	cmpfls			@	if so,  exit reduction with #f
	postv	sv1			@ is x1 positive?
	it	ne
	postvne	sv2			@	if not, is x2 positive?
	bne	adr_intlt		@	if not, jump to test for that
	postv	sv2			@ is x2 positive or 0?
	bne	cmptru			@	if not, exit with num2
	postv	sv1			@ is x1 positive or 0?
	bne	cmpfls			@	if not, exit with #f
	@ continue to gtint
	b	adr_intgt

	/* > for rational */
	PRIMIT	ratgt, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ rvc <- lnk, saved (and made even if Thumb2)
	bl	eqrtcx
	orr	lnk, sv3, #lnkbit0
	beq	cmpfls
	save	sv2, sv3
	swap	sv1, sv2, sv3
	b	ltgtxt

	/* (<= num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBFi	le, 0, oprdnml, t, _err, intle, fltle, ratle, _err ; .ascii "<=" ; ENDi
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- #t/#f

	/* <= for integer */
	PRIMIT	intle, ufun, 0
	eq	sv1, sv2
	bne	adr_intlt
	set	pc,  lnk
			
	/* <= for float */
	PRIMIT	fltle, ufun, 0
	eq	sv1, sv2
	bne	adr_fltlt
	set	pc,  lnk

	/* <= for rational */
	PRIMIT	ratle, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	bl	eqrtcx
	orr	lnk, sv3, #lnkbit0
	bne	adr_ratlt
	set	pc, lnk

	/* (>= num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBFi	ge, 0, oprdnml, t, _err, intge, fltge, ratge, _err ; .ascii ">=" ; ENDi
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- #t/#f

	/* >= for integer */
	PRIMIT	intge, ufun, 0
	eq	sv1, sv2
	bne	adr_intgt
	set	pc,  lnk
			
	/* >= for float */
	PRIMIT	fltge, ufun, 0
	eq	sv1, sv2
	bne	adr_fltgt
	set	pc,  lnk

	/* >= for rational */
	PRIMIT	ratge, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	bl	eqrtcx
	orr	lnk, sv3, #lnkbit0
	bne	adr_ratgt
	set	pc, lnk

	/* (+ num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"+", pls, 0, ordcnml, i0, _err
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- result (sum)

	/* + for int */
	PRIMIT	intpls, ufun, 0
	int2raw	rva, sv1		@ rva <- x1 (raw int)
	int2raw	rvb, sv2		@ rva <- x2 (raw int)
	add	rvc, rva, rvb
	raw2int	sv1, rvc
	ands	rva, rvc, #0xE0000000
	it	ne
	eqne	rva, #0xE0000000
	it	eq
	seteq	pc,  lnk
.ifdef	hardware_FPU
  .ifndef FPU_is_maverick
	vmov	s0,  rvc
	vcvt.f32.s32	s0, s0
	vmov	rva, s0
  .else
	cfmv64lr mvdx0, rvc
	cfcvt32s mvf0, mvfx0
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	orr	sv1, rva, #f0
	set	pc,  lnk
.else
	bic	rvc, rvc, #3
	orr	sv1, rvc, #int_tag
	set	rva, 150
	b	mteflt
.endif

.ifdef	hardware_FPU

	/* + for float */
	PRIMIT	fltpls, ufun, 0
	bic	rva, sv1, #0x03
	bic	rvb, sv2, #0x03
  .ifndef FPU_is_maverick
	vmov	s0, s1, rva, rvb
	vadd.f32 s0, s0, s1	
	vmov	rva, s0	
  .else
	cfmvsr	mvf0, rva
	cfmvsr	mvf1, rvb
	cfadds	mvf0, mvf0, mvf1
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	orr	sv1, rva, #f0
	set	pc,  lnk

.else	@ no hardware FPU

	/* + for float */
	PRIMIT	fltpls, ufun, 0
	anynan	sv1, sv2
	beq	nanlxt
	fltmte	rva, sv1		@ sv1 <- x1's signed mantissa,  rva <- x1's exponent
	fltmte	rvb, sv2		@ sv2 <- x2's signed mantissa,  rvb <- x2's exponent
	eq	rva, #0xff
	it	ne
	eqne	rvb, #0xff
	beq	plsspc
	cmp	rva, rvb		@ is x2's exponent > x1's exponent?
	itE	pl
	subpl	rvb, rva, rvb		@	if so,   rvb <- difference in exponents (>= 0)
	swapmi	sv1, sv2, rvc		@ sv1 <- num w/lrgst exp, sv2 <- num w/smllst exp (sv4=tmp)
	itT	mi
	submi	rvb, rvb, rva		@	if not,  rvb <- difference in exponents (>= 0)
	addmi	rva, rva, rvb		@	if not,  rva <- largest exponent
	asr	rvb, sv2, rvb		@ rvb <- small mntss shftd to large one (raw int)
	bic	rvb, rvb, #0x03		@ rva <- small mntss shftd to large one, tag bits clrd
	orr	sv2, rvb, #int_tag	@ sv1 <- small mntss shftd to large one (scheme int)
	add	sv1, sv1, sv2		@ sv1 <- sum of mantissas (pseudo scheme float)
	eor	sv1, sv1, #3		@ sv1 <- sum of mantissas (scheme int)
	b	mteflt			@ sv1 <- sum (sch float) frm sv1 & rva, exit w/rslt via lnk
plsspc:	@ special addition of scheme floats with +/-inf
	eq	rva, rvb
	beq	plssp2
	eq	rva, #0xff
	itE	eq
	seteq	sv2, sv1
	setne	sv1, sv2
plssp2:	@	
	eor	rva, sv1, sv2		@ rva <- xor sv1 sv2 (sign of operands indicator)
	postv	rva			@ is rva positive (both sv1 and sv2 have same sign)?
	bne	nanlxt			@	if not, exit with nan
	set	sv1, scheme_inf		@ sv1 <- inf
	postv	sv2			@ is result negative?
	it	ne
	ngfltne	sv1, sv1		@	if so,  sv1 <- -inf
	set	pc,  lnk		@ return with +/-inf or nan

.endif	@ yes/no hardware FPU
	
	/* + for rational */
	PRIMIT	ratpls, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv5
	save	sv1, sv2, sv3		@ dts <- (rat1 rat2 lnk ...)
	denom	sv1, sv1		@ sv1 <- denom-rat1
	izerop	sv1
	beq	plsra0
	denom	sv2, sv2		@ sv2 <- denom-rat2
	izerop	sv2
	beq	plsra2
	bl	igcd			@ sv1 <- gcd of denom-rat1 and denom-rat2
	set	sv3, sv1		@ sv3 <- gcd of denom-rat1 and denom-rat2, saved
	car	sv1, dts		@ sv1 <- rat1
	denom	sv1, sv1		@ sv1 <- denom-rat1
	set	sv2, sv3		@ sv2 <- gcd of denom-rat1 and denom-rat2
	bl	idivid			@ sv1 <- denom-rat1 / gcd
	set	sv2, sv3		@ sv2 <- gcd of denom-rat1 and denom-rat2
	set	sv3, sv1		@ sv3 <- denom-rat1 / gcd, saved
	cadr	sv1, dts		@ sv1 <- rat2
	denom	sv1, sv1		@ sv1 <- denom-rat2
	bl	idivid			@ sv1 <- denom-rat2 / gcd
	restor	sv2			@ sv2 <- rat1,	dts <- (rat2 lnk ...)
	numerat	sv2, sv2		@ sv2 <- numer-rat1
	bl	adr_intprd		@ sv1 <- numer-rat1 * denom-rat2 / gcd
	car	sv2, dts		@ sv2 <- rat2
	save	sv1			@ dts <- (num-rat1*den-rat2/gcd rat2 lnk ...)
	numerat	sv1, sv2		@ sv1 <- numer-rat2
	set	sv2, sv3		@ sv2 <- denom-rat1 / gcd
	bl	adr_intprd		@ sv1 <- numer-rat2 * denom-rat1 / gcd
	restor	sv2			@ sv2 <- num-rat1*den-rat2/gcd,	dts <- (rat2 lnk ...)
	save	sv3			@ dts <- (den-rat1/gcd rat2 lnk ...)
	bl	unipls
	set	sv3, sv1
	restor	sv1, sv2
	denom	sv2, sv2
	bl	adr_intprd
	set	sv2, sv1
	set	sv1, sv3
_func_
plsra4:	@ common completion
	adr	lnk, plsra3
	b	unidiv

plsra0:	@ sv1 is n/0, what about sv2?
	set	rva, sv2
	restor	sv1, sv2
	izerop	rva
	bne	plsra3
	bl	eqrtcx
	beq	plsra3
	set	sv1, i0
	set	sv2, i0
	b	plsra4
plsra2:	@ sv2 is n/0, but sv1 is normal => return sv2 (in sv1)
	restor	sv1, sv1
_func_
plsra3:	@ return (also for prdrat, mnsrat, divrat)	
	restor	sv3, sv5
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk	
	
	/* + for complex */
	PRIMIT	cpxpls, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3
	imag	sv1, sv1
	imag	sv2, sv2
	bl	adr_fltpls
	restor	sv2, sv3
	save	sv1
	real	sv1, sv3
	real	sv2, sv2
	bl	adr_fltpls
	restor	sv2, sv3
	orr	lnk, sv3, #lnkbit0
	b	makcpx


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

	/* (* num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"*", prd, 0, ordcnml, i1, _err
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- result (product)

	/* * (product) for integers */
	PRIMIT	intprd, ufun, 0
	int2raw	rvc, sv1		@ rva <- x2 (raw int)
	int2raw	rva, sv2		@ rva <- x1 (raw int)
	smull	rva, rvb, rvc, rva	@ rva <- x1 (raw int) * x2 (raw int), rvc <- pssbl ovrflw
	raw2int	sv1, rva
	lsl	rvb, rvb, #3
	orrs	rvb, rvb, rva, lsr #29
	it	ne
	mvnsne	rvc, rvb
	it	eq
	seteq	pc,  lnk
	@ integer product overflow, convert to float
	lsl	rva, rva, #3
	set	rvc, rvb
	postv	rvc
	it	ne
	mvnne	rvc, rvc
	
.ifndef	hardware_FPU	@ exclude cortex-a8
  .ifndef cortex	@ left shift on ARMv4T
	set	sv2, i0
prdcf0:	postv	rvc
	itT	eq
	addeq	sv2, sv2, #4
	lsleq	rvc, rvc, #1
	beq	prdcf0
	int2raw	rvc, sv2
  .else
	clz	rvc, rvc
  .endif
.else
  .ifndef FPU_is_maverick
	clz	rvc, rvc
  .else
	set	sv2, i0
prdcf0:	postv	rvc
	itT	eq
	addeq	sv2, sv2, #4
	lsleq	rvc, rvc, #1
	beq	prdcf0
	int2raw	rvc, sv2
  .endif
.endif
	@ common completion	
	sub	rvc, rvc, #1
	lsl	rvb, rvb, rvc
	rsb	rvc, rvc, #32
	lsr	rva, rva, rvc
	orr	rvb, rvb, rva
	bic	rvb, rvb, #3
	orr	sv1, rvb, #int_tag
	add	rva, rvc, #147
	b	mteflt

.ifdef	hardware_FPU

	/* * (product) for floats */
	PRIMIT	fltprd, ufun, 0
	bic	rva, sv1, #0x03
	bic	rvb, sv2, #0x03
  .ifndef FPU_is_maverick
	vmov	s0, s1, rva, rvb
	vmul.f32 s0, s0, s1	
	vmov	rva, s0	
  .else
	cfmvsr	mvf0, rva
	cfmvsr	mvf1, rvb
	cfmuls	mvf0, mvf0, mvf1
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	eq	rva, #(1 << 31)		@ is result -0.0?
	it	eq
	seteq	rva, 0			@	if so,  rva <- 0.0
	orr	sv1, rva, #f0
	set	pc,  lnk

.else	@ no hardware FPU

	/* * (product) for floats */
	PRIMIT	fltprd, ufun, 0
	anynan	sv1, sv2		@ is either sv1 or sv2 = nan?
	beq	nanlxt			@	if so,  exit with nan
	fltmte	rva, sv1		@ sv1 <- signed mantissa of x1, rva <- biased expon of x1
	fltmte	rvb, sv2		@ sv2 <- signed mantissa of x2, rvb <- biased expon of x2
	eq	rva, #0xff
	it	ne
	eqne	rvb, #0xff
	beq	prdspc
	add	rvb, rvb, rva
	sub	rvb, rvb, #133		@ rvb <- biased exponent of result	
	int2raw	rva, sv1
	int2raw	rvc, sv2
	raw2int	sv2, rvb
	smull	rva, rvb, rvc, rva	@ rvb  <- product of x1 and x2 mantissas
	lsl	rvb, rvb, #17
	orr	rvb, rvb, rva, lsr #15
	lsl	rva, rva, #17
prdfl1:	@
	ands	rvc, rvb, #0x30000000
	it	ne
	eqne	rvc, #0x30000000
	bne	prdfl2
	lsl	rvb, rvb, #1
	tst	rva, #0x80000000
	it	ne
	orrne	rvb, rvb, #1
	lsls	rva, rva, #1
	sub	sv2, sv2, #4
	bne	prdfl1	
prdfl2:	raw2int	sv1, rvb
	int2raw	rva, sv2
	b	mteflt			@ sv1 <- flt frm sv1 sgnd mant & rva bsd exp, ret via lnk
prdspc:	@ special product of scheme floats with +/-inf
	eq	rva, #0
	it	eq
	eqeq	sv1, #i0
	beq	nanlxt			@ (* 0 inf) -> nan
	eq	rvb, #0
	it	eq
	eqeq	sv2, #i0
	beq	nanlxt			@ (* inf 0) -> nan
	eor	rva, sv1, sv2		@ rva <- item with sign of result in MSb
	and	rva, rva, #0x80000000	@ rva <- sign of result
	set	sv1, scheme_inf		@ sv1 <- inf
	orr	sv1, sv1, rva		@ sv1 <- signed inf
	set	pc,  lnk		@ exit with +/-inf
	
.endif	@ yes/no hardware FPU


	/* * (product) for rationals */
	PRIMIT	ratprd, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv5
	save	sv1, sv2, sv3
	numerat	sv1, sv1		@ sv1 <- denominator of r1
	denom	sv2, sv2
	bl	adr_intdiv
	restor	sv2, sv3
	save	sv1
	denom	sv2, sv2
	numerat	sv1, sv3
	bl	adr_intdiv
	restor	sv2
	bl	uninum
	pntrp	sv1
	it	ne
	setne	sv3, 5
	bne	prdrxt
	spltrat	sv3, sv2, sv2		@ sv3 <- numerator of r2, sv2 <- denominator of r2
	save	sv3
	spltrat	sv3, sv1, sv1		@ sv3 <- numerator of r1, sv1 <- denominator of r1
	bl	adr_intprd		@ sv1 <- product of denominators
	set	sv2, sv3
	set	sv3, sv1
	restor	sv1
prdrxt:	@
	bl	adr_intprd		@ sv1 <- product of numerators
	set	sv2, sv3		@ sv2 <- product of denominators
	adr	lnk, plsra3
	b	unidiv

	/* * (product) for complex numbers */
	PRIMIT	cpxprd, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3
	real	sv1, sv1		@ sv1 <- real part of z1
	imag	sv2, sv2		@ sv2 <- imag part of z2
	bl	adr_fltprd		@ sv1 <- first real-x-imag product
	snoc	sv2, sv3, dts		@ sv2 <- z1, sv3 <- (z2 lnk ...)
	save	sv1			@ dts <- (partial-imag-prod z1 z2 lnk ...)
	car	sv1, sv3		@ sv1 <- z2
	real	sv1, sv1		@ sv1 <- real part of z2
	imag	sv2, sv2		@ sv2 <- imag part of z1
	bl	adr_fltprd		@ sv1 <- second real-x-imag product
	restor	sv2			@ sv2 <- first  real-x-imag product, dts <- (z1 z2 lnk ...)
	bl	adr_fltpls		@ sv1 <- imag part of product
	snoc	sv2, sv3, dts		@ sv2 <- z1, sv3 <- (z2 lnk ...)
	save	sv1			@ dts <- (imag-prod z1 z2 lnk ...)
	car	sv1, sv3		@ sv1 <- z2
	real	sv1, sv1		@ sv1 <- real part of z2
	real	sv2, sv2		@ sv2 <- real part of z1
	bl	adr_fltprd		@ sv1 <- product of real parts
	cdr	sv3, dts		@ sv3 <- (z1 z2 lnk ...)
	snoc	sv2, sv3, sv3		@ sv2 <- z1, sv3 <- (z2 lnk ...)
	save	sv1			@ dts <- (prod-real-parts imag-prod z1 z2 lnk ...)
	car	sv1, sv3		@ sv1 <- z2
	imag	sv1, sv1		@ sv1 <- imag part of z2
	imag	sv2, sv2		@ sv2 <- imag part of z1
	bl	adr_fltprd		@ sv1 <- product of imag parts
	ngflt	sv1, sv1		@ sv1 <- minus product of imag parts
	restor	sv2			@ sv2 <- prod real parts, dts <- (imag-prod z1 z2 lnk ...)
	bl	adr_fltpls		@ sv1 <- real part of product
	restor	sv2			@ sv2 <- imag part of product, dts <- (z1 z2 lnk ...)
	cddr	dts, dts		@ dts <- (lnk ...)
	restor	sv3			@ sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	b	makcpx			@ jump to build complex, return via lnk

	/* (- num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"-", mns, 0, ordcnml, i0, _err
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- result (diference)

	/* - for integers */
	PRIMIT	intmns, ufun, 0
	ngint	sv2, sv2
	b	adr_intpls

	/* - for floats */
	PRIMIT	fltmns, ufun, 0
	anynan	sv1, sv2
	beq	nanlxt
	ngflt	sv2, sv2
	b	adr_fltpls

	/* - for rationals */
	PRIMIT	ratmns, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv3, sv5
	spltrat	sv1, sv2, sv2
	ngint	sv1, sv1
	bl	adr_intdiv
	set	sv2, sv1
	restor	sv1
	ldr	lnk, =plsra3
	b	unipls

	/* - for complex numbers */
	PRIMIT	cpxmns, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv3
	spltcpx	sv1, sv2, sv2
	ngflt	sv1, sv1
	ngflt	sv2, sv2
	bl	makcpx
	set	sv2, sv1
	restor	sv1, sv3
	orr	lnk, sv3, #lnkbit0
	b	adr_cpxpls			@ continue to plus12 to add num1 with -num2

	/* (/ num1 num2 ...)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"/", div, 0, ordcnml, i1, _err
	@ in:	sv1 <- (num1 num2 ...)
	@ out:	sv1 <- result (division)

.ifdef	hardware_FPU

  .ifndef FPU_is_maverick

	/* / for floats */
	PRIMIT	fltdiv, ufun, 0
	@ modifies:	sv1, rva, rvb
	bic	rva, sv1, #0x03
	bic	rvb, sv2, #0x03
	vmov	s0, s1, rva, rvb
	vdiv.f32 s0, s0, s1	
	vmov	rva, s0	
	bic	rva, rva, #0x03
	eq	rva, #(1 << 31)		@ is result -0.0?
	it	eq
	seteq	rva, 0			@	if so,  rva <- 0.0
	orr	sv1, rva, #f0
	set	pc,  lnk

  .else	@ Maverick Crunch FPU does not do division
	@ so, code below is same as with no hardware FPU

	/* / for floats */
	PRIMIT	fltdiv, ufun, 0
	@ modifies:	sv1, sv2, rva, rvb, rvc
	anynan	sv1, sv2
	beq	nanlxt
	eq	sv1, #f0
	it	ne
	eqne	sv2, #f0
	beq	divzro
	@ division of scheme floats
	set	rva, 0xFF000000		@ rva <- mask for float exponent
	and	rvb, rva, sv1, LSL #1	@ rvb <- exponent
	eq	rva, rvb
	itT	ne
	andne	rvb, rva, sv2, LSL #1	@ rvb <- exponent
	eqne	rva, rvb
	beq	divspc
	@ regular division of scheme floats
	fltmte	rva, sv1		@ sv1 <- dividand's mntss, rva <- dividand's biased expon
	fltmte	rvb, sv2		@ sv2 <- divisor's mntss, rvb <- divisor's biased exponent
	add	rva, rva, #148		@ rva <- dividand's biased exponent + 127 + 21
	sub	rva, rva, rvb		@ rva <- initial result's biased exponent (raw int)
divnrm:	@ normal division of scheme floats (no zeros involved)
	and	rvc, sv1, #0x80000000
	postv	sv1			@ is dividand positive?
	itT	ne
	eorne	rvc, rvc, #int_tag	@	if not, sv5 <- lr xor int_tag => negative result
	ngintne	sv1, sv1		@	if not, sv1 <- -dividand (scheme int)
	postv	sv2			@ is divisor positive?
	itT	ne
	eorne	rvc, rvc, #int_tag	@	if not, sv5 <- lr xor sign (int_tag or zero)
	ngintne	sv2, sv2		@	if not, sv2 <- -divisor (scheme int)
	@ shift divisor left as much as possible
dvdls2:	@
	tst	sv2, #0x40000000	@ is bit 30 of divisor = 1?
	itTT	eq
	addeq	rva, rva, #1		@	if not, rva <- exponent minus 1
	lsleq	sv2, sv2, #1		@	if not, sv2 <- divisor shifted left (psd sch float)
	eoreq	sv2, sv2, #0x03		@	if not, sv2 <- divisor shifted left (scheme int)
	beq	dvdls2			@	if not, jump to keep shifting
	int2raw	rvb, sv1		@ rvb <- initial dividand (raw int)
	set	sv1, i0			@ sv1 <- initial result = 0 (scheme int)
	@  sv1 <- result mantissa, sv4 <- biased exponent of result (scheme int)
dvddls:	@ shift dividand (rvb) to be greater or equal to divisor (sv2)
	cmp	rvb, sv2, LSR #2	@ is dividand >= divisor (raw int)
	bpl	dvddvd			@	if so,  jump to continue
	lsl	rvb, rvb, #1		@ rvb <- dividand shifted left
	sub	rva, rva, #1		@ rva <- add 1 to shift
	lsl	sv1, sv1, #1		@ sv1 <- result shifted left (pseudo scheme float)
	eor	sv1, sv1, #3		@ sv1 <- result shifted left (scheme int)
	tst	sv1, #0x40000000	@ is result saturated?
	beq	dvddls			@	if not, jump to continue shifting dividand
dvddvd:	subs	rvb, rvb, sv2, LSR #2	@ rvb <- updated dividand = shifted dividand - divisor
	it	pl
	addpl	sv1, sv1, #4		@	if positive dividand, sv1 <- result + 1 (sch int)
	beq	dvddon			@	if remainder is 0, jump to finish up  
	tst	sv1, #0x40000000	@ is result saturated?
	beq	dvddls			@	if not, jump to continue dividing
dvddon:	@ update result sign if necessary
	tst	rvc, #int_tag		@ should result be negative?
	it	ne
	ngintne	sv1, sv1		@	if so,  sv1 <-  -sv1
	b	mteflt			@ sv1 <- result as float, return via lr
divzro:	@ division of 0 by dividand or of divisor by 0
	eq	sv1, sv2		@ are divisor and dividand both zero?
	beq	nanlxt
	zerop	sv1			@ is dividand = 0 or 0.0?
	itTT	ne
	andne	rva, sv1, #0x80000000	@	if not, rva <- sign of dividand
	setne	sv1, scheme_inf		@	if not, sv1 <- inf
	orrne	sv1, sv1, rva		@	if not, sv1 <- +/-inf (same sign as dividand)
	set	pc,  lnk		@ return with 0, 0.0 or inf
divspc:	@ special division of scheme floats with nan or +/-inf
	lsl	rvb, sv1, #1		@ rvb <- x1 without sign (shifted out)
	eq	rvb, sv2, LSL #1	@ is x1 (unsigned) = x2 (unsigned) ( +-inf  /  +-inf )?
	beq	nanlxt
	isinf	sv1
	it	ne
	setne	sv1, f0			@	if not, sv1 <- 0.0	(value / +-inf)
	set	pc,  lnk		@ return with +/-inf or 0.0
	
  .endif	@ FPU_is_maverick

.else	@ no hardware FPU

	/* / for floats */
	PRIMIT	fltdiv, ufun, 0
	@ modifies:	sv1, sv2, rva, rvb, rvc
	anynan	sv1, sv2
	beq	nanlxt
	eq	sv1, #f0
	it	ne
	eqne	sv2, #f0
	beq	divzro
	@ division of scheme floats
	set	rva, 0xFF000000		@ rva <- mask for float exponent
	and	rvb, rva, sv1, LSL #1	@ rvb <- exponent
	eq	rva, rvb
	itT	ne
	andne	rvb, rva, sv2, LSL #1	@ rvb <- exponent
	eqne	rva, rvb
	beq	divspc
	@ regular division of scheme floats
	fltmte	rva, sv1		@ sv1 <- dividand's mntss, rva <- dividand's biased expon
	fltmte	rvb, sv2		@ sv2 <- divisor's mntss,   rvb <- divisor's biased expon
	add	rva, rva, #148		@ rva <- dividand's biased exponent + 127 + 21
	sub	rva, rva, rvb		@ rva <- initial result's biased exponent (raw int)
divnrm:	@ normal division of scheme floats (no zeros involved)
	and	rvc, sv1, #0x80000000
	postv	sv1			@ is dividand positive?
	itT	ne
	eorne	rvc, rvc, #int_tag	@	if not, sv5 <- lr xor int_tag => negative result
	ngintne	sv1, sv1		@	if not, sv1 <- -dividand (scheme int)
	postv	sv2			@ is divisor positive?
	itT	ne
	eorne	rvc, rvc, #int_tag	@	if not, sv5 <- lr xor sign (int_tag or zero)
	ngintne	sv2, sv2		@	if not, sv2 <- -divisor (scheme int)
	@ shift divisor left as much as possible
dvdls2:	@
	tst	sv2, #0x40000000	@ is bit 30 of divisor = 1?
	itTT	eq
	addeq	rva, rva, #1		@	if not, rva <- exponent minus 1
	lsleq	sv2, sv2, #1		@	if not, sv2 <- divisor shftd left (psdo sch float)
	eoreq	sv2, sv2, #0x03		@	if not, sv2 <- divisor shifted left (scheme int)
	beq	dvdls2			@	if not, jump to keep shifting
	int2raw	rvb, sv1		@ rvb <- initial dividand (raw int)
	set	sv1, i0			@ sv1 <- initial result = 0 (scheme int)
	@  sv1 <- result mantissa, sv4 <- biased exponent of result (scheme int)
dvddls:	@ shift dividand (rvb) to be greater or equal to divisor (sv2)
	cmp	rvb, sv2, LSR #2	@ is dividand >= divisor (raw int)
	bpl	dvddvd			@	if so,  jump to continue
	lsl	rvb, rvb, #1		@ rvb <- dividand shifted left
	sub	rva, rva, #1		@ rva <- add 1 to shift
	lsl	sv1, sv1, #1		@ sv1 <- result shifted left (pseudo scheme float)
	eor	sv1, sv1, #3		@ sv1 <- result shifted left (scheme int)
	tst	sv1, #0x40000000	@ is result saturated?
	beq	dvddls			@	if not, jump to continue shifting dividand
dvddvd:	subs	rvb, rvb, sv2, LSR #2	@ rvb <- updated dividand = shifted dividand - divisor
	it	pl
	addpl	sv1, sv1, #4		@	if positive dividand, sv1 <- result + 1 (sch int)
	beq	dvddon			@	if remainder is 0, jump to finish up  
	tst	sv1, #0x40000000	@ is result saturated?
	beq	dvddls			@	if not, jump to continue dividing
dvddon:	@ update result sign if necessary
	tst	rvc, #int_tag		@ should result be negative?
	it	ne
	ngintne	sv1, sv1		@	if so,  sv1 <-  -sv1
	b	mteflt			@ sv1 <- result as float, return via lr
divzro:	@ division of 0 by dividand or of divisor by 0
	eq	sv1, sv2		@ are divisor and dividand both zero?
	beq	nanlxt
	zerop	sv1			@ is dividand = 0 or 0.0?
	itTT	ne
	andne	rva, sv1, #0x80000000	@	if not, rva <- sign of dividand
	setne	sv1, scheme_inf		@	if not, sv1 <- inf
	orrne	sv1, sv1, rva		@	if not, sv1 <- +/-inf (same sign as dividand)
	set	pc,  lnk		@ return with 0, 0.0 or inf
divspc:	@ special division of scheme floats with nan or +/-inf
	lsl	rvb, sv1, #1		@ rvb <- x1 without sign (shifted out)
	eq	rvb, sv2, LSL #1	@ is x1 (unsigned) = x2 (unsigned) ( +-inf  /  +-inf )?
	beq	nanlxt
	isinf	sv1
	it	ne
	setne	sv1, f0			@	if not, sv1 <- 0.0	(value / +-inf)
	set	pc,  lnk		@ return with +/-inf or 0.0
	
.endif	@ yes/no hardware FPU

	/* / for rationals */
	PRIMIT	ratdiv, ufun, 0
	@ should do a cross gcd12 between num/denom
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv3, sv5
	spltrat	sv2, sv1, sv2		@ sv2 <- numerator of r2, sv1 <- denominator of r2
	bl	adr_intdiv
	restor	sv2
	ldr	lnk, =plsra3
	b	uniprd

	/* / for complex numbers */
	PRIMIT	cpxdiv, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3
	real	sv1, sv2		@ sv1 <- real part of z2
	set	sv2, sv1		@ sv2 <- real part of z2
	bl	adr_fltprd		@ sv1 <- (real-z2)^2
	snoc	sv2, sv3, dts		@ sv2 <- z1, sv3 <- (z2 lnk ...)
	save	sv1			@ dts <- ((real-z2)^2 z1 z2 lnk ...)
	car	sv1, sv3		@ sv1 <- z2
	imag	sv1, sv1		@ sv1 <- imag part of z2
	set	sv2, sv1		@ sv2 <- imag part of z2
	bl	adr_fltprd		@ sv1 <- (imag-z2)^2
	restor	sv2			@ sv2 <- (real-z2)^2, dts <- (z1 z2 lnk ...)
	bl	adr_fltpls		@ sv1 <- magnitude(z2)^2
	set	sv2, sv1		@ sv2 <- magnitude(z2)^2
	cadr	sv1, dts		@ sv1 <- z2
	save	sv2			@ dts <- (magnitude(z2)^2 z1 z2 lnk ...)
	imag	sv1, sv1		@ sv1 <- imag part of z2
	bl	adr_fltdiv		@ sv1 <- imag part of z2 / magnitude(z2)^2
	restor	sv2			@ sv2 <- magnitude(z2)^2, dts <- (z1 z2 lnk ...)
	cadr	sv3, dts		@ sv3 <- z2
	save	sv1			@ dts <- (imag-z2/mag(z2)^2 z1 z2 lnk ...)
	real	sv1, sv3		@ sv1 <- real part of z2
	bl	adr_fltdiv		@ sv1 <- real part of z2 / magnitude(z2)^2
	restor	sv2			@ sv2 <- imag part of z2/magnt(z2)^2, dts <- (z1 z2 lnk .)
	ngflt	sv2, sv2		@ sv2 <- minus imag part of z2 / magnitude(z2)^2
	bl	makcpx			@ sv1 <- z2* / |z2|
	restor	sv2			@ sv2 <- z1, dts <- (z2 lnk ...)
	cdr	dts, dts		@ dts <- (lnk ...)
	restor	sv3			@ sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	b	adr_cpxprd		@ jump to multiply z1 by z2* / |z2|, return via lnk
	
	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

	/* (quotient int1 int2)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"quotient", quo, 2, ounijpe, null, _err, intquo, fltquo, ratquo, _err
	@ in:	sv1 <- num1
	@ in:	sv2 <- num2
	@ out:	sv1 <- result

	/* quotient for integers */
	PRIMIT	intquo, ufun, 0
	set	lnk, cnt		@ lnk <- return address
	izerop	sv2
	beq	adr_intdiv
	b	idivid			@ sv1 <- quotient, and return

	/* quotient for floats */
	PRIMIT	fltquo, ufun, 0
	bl	adr_fltdiv
	lsr	rvb, sv1, #23
	and	rvb, rvb, #0xff
	eq	rvb, #0xff
	it	ne
	blne	itrunc
	set	sv2, f0			@ sv2 <- 0.0
	set	lnk, cnt		@ lnk <- return address
	b	uninum

	/* quotient for rationals */
	PRIMIT	ratquo, ufun, 0
	bl	adr_ratdiv
	intgrp	sv1
	it	eq
	seteq	pc,  cnt
	spltrat	sv1, sv2, sv1
	b	adr_intquo

	/* (remainder int1 int2)	~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"remainder", rem, 2, ounijpe, null, _err, intrem, fltrem, ratrem, _err
	@ in:	sv1 <- int1
	@ in:	sv2 <- int2
	@ out:	sv1 <- result

	/* remainder for integers */
	PRIMIT	intrem, ufun, 0
	zerop	sv2
	it	eq
	seteq	pc,  cnt
	bl	idivid			@ sv1 <- quotient, sv2 <- remainder
	set	sv1, sv2		@ sv1 <- remainder
	set	pc,  cnt		@ return

	/* remainder for floats (floats are truncated before getting remainder) */
	PRIMIT	fltrem, ufun, 0
	save	sv2, sv1
	bl	adr_fltdiv
	bl	itrunc
	bl	i12flt
	restor	sv2
	bl	adr_fltprd
	ngflt	sv1, sv1
	restor	sv2
	set	lnk, cnt
	b	adr_fltpls

	/* remainder for rationals */
	PRIMIT	ratrem, ufun, 0
	@ (need to check if ratcdn returned some floats)
	spltrat	rvc, sv3, sv2
	izerop	sv3
	it	eq
	eqeq	rvc, sv3
	itT	eq
	seteq	sv1, sv2
	seteq	pc,  cnt
	spltrat	rvc, sv3, sv1
	izerop	sv3
	it	eq
	eqeq	rvc, sv3
	it	eq
	seteq	pc,  cnt
	izerop	sv3
	it	eq
	seteq	pc,  cnt
	bl	ratcdn			@ sv1 <- nn1, sv2 <- nn2, sv3 <- lcm
	bl	idivid			@ sv1 <- quotient, sv2 <- remainder
	set	sv1, sv2		@ sv1 <- remainder
	set	sv2, sv3		@ sv2 <- lcm
	set	lnk, cnt
	b	adr_intdiv
	
_func_
ratcdn:	@ set rationals to common denominator
	@ in:	sv1 <- rational
	@ in:	sv2 <- rational
	@ out:	sv1 <- nn1
	@ out:	sv2 <- nn2
	@ out:	sv3 <- lcm
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3		@ dts <- (p1/q1 p2/q2 lnk ...)
	denom	sv1, sv1		@ sv1 <- q1
	denom	sv2, sv2		@ sv2 <- q2
	bl	adr_intgcd		@ sv1 <- gcd(q1,q2)
	set	sv3, dts
	save	sv1			@ dts <- (gcd(q1,q2) p1/q1 p2/q2 lnk ...)
	set	sv2, sv1
	cadr	sv1, sv3
	denom	sv1, sv1		@ sv1 <- q2
	bl	idivid			@ sv1 <- q2/gcd(q1,q2)
	car	sv3, sv3
	numerat	sv2, sv3		@ sv2 <- p1
	bl	adr_intprd		@ sv1 <- p1 q2 / gcd(q1,q2) = nn1
	restor	sv2			@ sv2 <- gcd(q1,q2), dts <- (p1/q1 p2/q2 lnk ...)
	set	sv3, dts		@ dts <- (p1/q1 p2/q2 lnk ...)
	save	sv1			@ dts <- (nn1 p1/q1 p2/q2 lnk ...)
	snoc	sv1, sv3, sv3		@ sv1 <- p1/q1, sv3 <- (p2/q2 lnk ...)
	denom	sv1, sv1		@ sv1 <- q1
	bl	idivid			@ sv1 <- q1/gcd(q1,q2)
	save	sv1			@ dts <- (q1/gcd(q1,q2) nn1 p1/q1 p2/q2 lnk ...)
	car	sv2, sv3		@ sv2 <- p2/q2
	numerat	sv2, sv2		@ sv2 <- p2
	bl	adr_intprd		@ sv1 <- p2 q1 / gcd(q1,q2) = nn2
	restor	sv2			@ sv2 <- q1/gcd(q1,q2), dts <- (nn1 p1/q1 p2/q2 lnk ...)
	cdr	sv3, dts		@ sv3 <- (p1/q1 p2/q2 lnk ...)
	save	sv1			@ dts <- (nn2 nn1 p1/q1 p2/q2 lnk ...)
	cadr	sv1, sv3		@ sv1 <- p2/q2
	denom	sv1, sv1		@ sv1 <- q2
	bl	adr_intprd		@ sv1 <- q1 q2 / gcd(q1,q2) = lcm
	set	sv3, sv1		@ sv3 <- lcm
	restor	sv2, sv1		@ sv2 <- nn2, sv1 <- nn1, dts <- (p1/q1 p2/q2 lnk ...)
	cddr	dts, dts		@ dts <- (lnk ...)
	restor	rva			@ rva <- lnk, dts <- (...)
	orr	lnk, rva, #lnkbit0
	set	pc,  lnk

	/* gcd for integer */
	PRIMIT	intgcd, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv3
	set	sv3, sv1
	set	sv1, i0
gcdien:	@ [internal entry]
	save	sv1
	adr	lnk, gcdlop
_func_	
gcdlop:	@ gcd loop
	izerop	sv2
	itT	ne
	setne	sv1, sv3
	setne	sv3, sv2		@ sv4 <- int2 (saved against idivid -- will become int1)
	bne	idivid			@ sv1 <- quotient, sv2 <- remainder -- will become int2
	@ gcd12 exit
	iabs	sv1, sv3
	restor	sv2, sv3
	ldr	sv5, =unirtb
	b	unijrt

	/* (modulo int1 int2)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"modulo", pfun, 2
	@ in:	sv1 <- int1
	@ in:	sv2 <- int2
	@ out:	sv1 <- result
	@ mods:	sv1-sv5, rva, rvb
	zerop	sv2
	beq	adr__err
	ldr	sv5, =tb_nummod		@ rvc <- operator table
	b	unijmp			@ jump to proper

	/* jump table for modulo */
	NUMJTB	mod, _err, intmod, fltmod, ratmod, _err

	/* modulo for int */
	PRIMIT	intmod, ufun, 0
	set	sv4, sv2		@ sv4 <- divisor, saved
	bl	idivid			@ sv1 <- quotient, sv2 <- remainder
	postv	sv1			@ is quotient positive?
	itE	eq
	seteq	sv1, sv2		@	if so,  sv1 <- remainder
	plusne	sv1, sv2, sv4		@	if not, sv1 <- remainder + divisor
	set	pc,  cnt

	/* modulo for float */
	PRIMIT	fltmod, ufun, 0
	save	sv2, sv1
	bl	adr_fltdiv
	bl	itrunc
	bl	i12flt
	car	sv2, dts
	save	sv1
	bl	adr_fltprd
	ngflt	sv1, sv1
	caddr	sv2, dts
	bl	adr_fltpls
	restor	sv3, sv2
	cdr	dts, dts
	postv	sv2
	it	eq
	seteq	pc,  cnt
	set	lnk, cnt
	b	adr_fltpls
	
	/* modulo for rational */
	PRIMIT	ratmod, ufun, 0
	@ (need to check if ratcdn returned some floats)
	bl	ratcdn			@ sv1 <- nn1, sv2 <- nn2, sv3 <- lcm
	set	sv4, sv2		@ sv4 <- divisor, saved
	bl	idivid			@ sv1 <- quotient, sv2 <- remainder
	postv	sv1			@ is quotient positive?
	itE	eq
	seteq	sv1, sv2		@	if so,  sv1 <- remainder
	plusne	sv1, sv2, sv4		@	if not, sv1 <- remainder + divisor
	set	sv2, sv3		@ sv2 <- lcm
	set	lnk, cnt
	b	adr_intdiv

	/* (numerator q)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"numerator", nmr, 1, onumgto, null, _err, return, fltnmr, ratnmr, _err
	@ in:	sv1 <- q
	@ out:	sv1 <- numerator of q

	/* numerator for float */
	PRIMIT	fltnmr, ufun, 0
	bl	flt2ndn
	set	lnk, cnt
	b	i12flt
	
	/* numerator for rational */
	PRIMIT	ratnmr, ufun, 0
	numerat	sv1, sv1
	set	pc,  cnt

	/* (denominator q)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"denominator", dnm, 1, onumgto, null, _err, intdnm, fltdnm, ratdnm, _err
	@ in:	sv1 <- q
	@ out:	sv1 <- denominator of q

	/* denominator for integer */
	PRIMIT	intdnm, ufun, 0
	set	sv1, i1
	set	pc,  cnt

	/* denominator for float */
	PRIMIT	fltdnm, ufun, 0
	bl	flt2ndn		@ sv1 <- numerator of flt, sv2 <- denomin of flt
	set	sv1, sv2
	set	lnk, cnt
	b	i12flt
	
	/* denominator for rational */
	PRIMIT	ratdnm, ufun, 0
	denom	sv1, sv1
	set	pc,  cnt

	/* jump table for entry into floor, ceiling, truncate, round */
	NUMJTB	fctr, _err, return, fltfctr, ratfctr, _err

	/* (floor number)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"floor", pfun, 1
	@ in:	sv1 <- number
	@ out:	sv1 <- result
	@ mods:	sv1-sv5, rva, rvb
	ldr	rvc, =tb_numfctr
	pntrp	sv1
	itE	ne
	adrne	lnk, adr_fltflr
	adreq	lnk, adr_ratflr
	b	numjmp			@ rva <- number,  rvb <- right shift, sv2 <- type/rem

.ifdef	hardware_FPU

	/* common function entry for floor, ceiling, truncate, round with float arg */
	PRIMIT	fltfctr, ufun, 0
	bic	rva, sv1, #0x80000000	@ rva <- number, without sign
	cmp	rva, #0x4a000000	@ does number have exponent >= 148 (no fractional part)?
	it	pl
	setpl	pc,  cnt		@	if so,  exit with original number
	bic	rva, sv1, #0x03
  .ifndef FPU_is_maverick
	vmov	s0, rva
  .else
	cfmvsr	mvf0, rva
  .endif
	set	pc,  lnk

.else

	/* common function entry for floor, ceiling, truncate, round with float arg */
	PRIMIT	fltfctr, ufun, 0
	set	sv2, sv1		@ sv2 <- number, saved against fltmte
	set	rvb, lnk		@ rvb <- lnk,    saved against fltmte
	bl	fltmte			@ sv1 <- mantissa,  rva <- exponent
	cmp	rva, #148		@ is exponent too large (float has no fractional part)
	itT	pl
	setpl	sv1, sv2		@	if so,  sv1 <- original number (restored)
	setpl	pc,  cnt		@	if so,  exit with original number
	set	lnk, rvb		@ lnk <- restored
	rsb	rvb, rva, #148		@ rvb <- right shft needed to get int part of num (raw int)
	int2raw	rva, sv1		@ rva <- mantissa (raw)
	set	pc,  lnk		@ return

.endif
	
.ifdef	hardware_FPU

	/* floor completion for float */
	PRIMIT	fltflr, ufun, 0
  .ifndef FPU_is_maverick
	vmrs	rvb, fpscr
	bic	rvb, rvb, #0x00c00000	@ clear rounding mode
	orr	rvb, rvb, #0x00800000	@ rounding mode = towards -inf (i.e. floor)
	vmsr	fpscr, rvb
	vcvtr.s32.f32	s0, s0
_func_
fctfxt:	@ normal completion for float, common to floor, ceiling, truncate, round
	vmrs	rvb, fpscr
	orr	rvb, rvb, #0x00c00000	@ rounding mode = towards zero (i.e. truncate = default)
	vmsr	fpscr, rvb
	vcvt.f32.s32	s0, s0
	vmov	rva, s0	
  .else
	cfmv32sc mvdx1, dspsc		@ mvfx1 <- rounding mode from DSPSC
	cfmvr64l rvb, mvdx1		@ rvb   <- rounding mode
	orr	rvb, rvb, #0x0c00	@ rounding mode = towards -inf (i.e. floor)
	cfmv64lr mvdx1, rvb		@ mvfx1 <- new rounding mode
	cfmvsc32 dspsc, mvdx1		@ set rounding mode in DSPSC
	cfcvts32 mvfx0, mvf0		@ mvfx0 <- number rounded
_func_
fctfxt:	@ normal completion for float, common to floor, ceiling, truncate, round
	cfmv32sc mvdx1, dspsc		@ mvfx1 <- rounding mode from DSPSC
	cfmvr64l rvb, mvdx1		@ rvb   <- rounding mode
	bic	rvb, rvb, #0x0c00	@ clear rounding mode
	orr	rvb, rvb, #0x0400	@ rounding mode = towards zero (i.e. truncate = default)
	cfmv64lr mvdx1, rvb		@ mvfx1 <- new rounding mode
	cfmvsc32 dspsc, mvdx1		@ set rounding mode in DSPSC
	cfcvt32s mvf0, mvfx0
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	orr	sv1, rva, #f0
	set	pc,  cnt

.else

	/* floor completion for float */
	PRIMIT	fltflr, ufun, 0
	asr	rva, rva, rvb		@ rva <- number shifted to integer
_func_
fctfxt:	@ normal completion for float, common to floor, ceiling, truncate, round
	raw2int	sv1, rva
	set	lnk, cnt
	b	i12flt

.endif

	/* common function entry for floor, ceiling, truncate, round with rational arg */
	PRIMIT	ratfctr, ufun, 0
	spltrat	sv3, sv5, sv1		@ sv3 <- nmrtr, sv5<-dnmntr (for round)
	izerop	sv5
	it	eq
	seteq	pc,  cnt
	set	sv1, sv3
	set	sv2, sv5
	b	idivid			@ sv1 <- qotnt, sv2<-remndr, ret via lnk
	
	/* floor completion for rational */
	PRIMIT	ratflr, ufun, 0
	postv	sv2
	it	ne
	subne	sv1, sv1, #4
	set	pc,  cnt

	/* (ceiling number)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"ceiling", pfun, 1
	@ in:	sv1 <- number
	@ out:	sv1 <- result
	@ mods:	sv1-sv5, rva, rvb
	ldr	rvc, =tb_numfctr
	pntrp	sv1
	itE	ne
	adrne	lnk, adr_fltcel
	adreq	lnk, adr_ratcel
	b	numjmp			@ rva <- num, rvb<-rshft, sv2<-typ/rem
	
.ifdef	hardware_FPU

	/* ceiling completion for float */
	PRIMIT	fltcel, ufun, 0
  .ifndef FPU_is_maverick
	vmrs	rvb, fpscr
	bic	rvb, rvb, #0x00c00000	@ clear rounding mode
	orr	rvb, rvb, #0x00400000	@ rounding mode = towards +inf (i.e. ceiling)
	vmsr	fpscr, rvb
	vcvtr.s32.f32	s0, s0
	b	fctfxt
  .else
	cfmv32sc mvdx1, dspsc		@ mvdx1 <- rounding mode from DSPSC
	cfmvr64l rvb, mvdx1		@ rvb   <- rounding mode
	bic	rvb, rvb, #0x0c00	@ clear rounding mode
	orr	rvb, rvb, #0x0800	@ rounding mode = towards +inf (i.e. ceiling)
	cfmv64lr mvdx1, rvb		@ mvdx1 <- new rounding mode
	cfmvsc32 dspsc, mvdx1		@ set rounding mode in DSPSC
	cfcvts32 mvfx0, mvf0		@ mvfx0 <- ceiling of number
	b	fctfxt
  .endif
.else

	/* ceiling completion for float */
	PRIMIT	fltcel, ufun, 0
	rsb	rva, rva, #0		@ rva <- negated number
	asr	rva, rva, rvb		@ rva <- number (negated) shifted to integer
	rsb	rva, rva, #0		@ rva <- number, de-negated = result
	b	fctfxt
.endif


	/* ceiling completion for rational */
	PRIMIT	ratcel, ufun, 0
	postv	sv2
	it	eq
	addeq	sv1, sv1, #4
	set	pc,  cnt

	/* (truncate number)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"truncate", pfun, 1
	@ in:	sv1 <- number
	@ out:	sv1 <- result
	@ mods:	sv1-sv5, rva, rvb
	ldr	rvc, =tb_numfctr
	pntrp	sv1
	itE	ne
	adrne	lnk, adr_flttrc
	seteq	lnk, cnt
	b	numjmp			@ rva <-num, rvb <-rshft, sv2 <-typ/rem
	
.ifdef	hardware_FPU

	/* truncate completion for float */
	PRIMIT	flttrc, ufun, 0
  .ifndef FPU_is_maverick
	vcvt.s32.f32	s0, s0
	b	fctfxt
  .else
	cfcvts32 mvfx0, mvf0		@ mvfx0 <- number truncated
	b	fctfxt
  .endif

.else

	/* truncate completion for float */
	PRIMIT	flttrc, ufun, 0
	cmp	rva, #0			@ is number negative?
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- positive number
	asr	rva, rva, rvb		@ rva <- number, shifted to integer
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- int, w/proper sgn
	b	fctfxt
.endif

	/* (round number)		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"round", pfun, 1
	@ in:	sv1 <- number
	@ out:	sv1 <- result
	@ mods:	sv1-sv5, rva, rvb
	ldr	rvc, =tb_numfctr
	pntrp	sv1
	itE	ne
	adrne	lnk, adr_fltrnd
	adreq	lnk, adr_ratrnd
	b	numjmp			@ rva <-num, rvb <-rshft, sv2 <-typ/rem
	
.ifdef	hardware_FPU

	/* round completion for float */
	PRIMIT	fltrnd, ufun, 0
  .ifndef FPU_is_maverick
	vmrs	rvb, fpscr
	bic	rvb, rvb, #0x00c00000	@ clear rounding mode (= round)
	vmsr	fpscr, rvb
	vcvtr.s32.f32	s0, s0
	b	fctfxt
  .else
	cfmv32sc mvdx1, dspsc		@ mvdx1 <- rounding mode from DSPSC
	cfmvr64l rvb, mvdx1		@ rvb   <- rounding mode
	bic	rvb, rvb, #0x0c00	@ clear rounding mode (= round)
	cfmv64lr mvdx1, rvb		@ mvdx1 <- new rounding mode
	cfmvsc32 dspsc, mvdx1		@ set rounding mode in DSPSC
	cfcvts32 mvfx0, mvf0		@ mvfx0 <- number rounded
	b	fctfxt
  .endif

.else

	/* round completion for float */
	PRIMIT	fltrnd, ufun, 0
	adr	lnk, fctfxt
	b	iround
.endif
		

	/* round completion for rational */
	PRIMIT	ratrnd, ufun, 0
	lsl	sv2, sv2, #1		@ sv2 <- remainder * 2 (pseudo float)
	eor	sv2, sv2, #3		@ sv2 <- remainder * 2 (scheme int)
	postv	sv1
	it	ne
	ngintne	sv2, sv2
	cmp	sv5, sv2
	bmi	rndraa
	eq	sv5, sv2		@ is 2*remainder = dividand?
	it	ne
	setne	pc,  cnt		@	if not, return
	tst	sv1, #4			@ is result even?
	it	eq
	seteq	pc,  cnt		@	if so,  return
rndraa:	@ adjust result up or down
	postv	sv1
	itE	eq
	addeq	sv1, sv1, #4
	subne	sv1, sv1, #4
	set	pc,  cnt
		
_func_
iround:	@ helper
	@ returns an int
	@ in:	sv1 <- mantissa
	@ in:	rva <- number
	@ in:	rvb <- exponent (negative)
	@ modifies:	sv2, rva, rvb
	cmp	rva, #0			@ is number negative?
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- -rva (make number positive)
	rsb	rvb, rvb, #32		@ rvb <- left shift needed to get fraction
	lsl	rva, rva, rvb		@ rva <- fraction, shifted all the way left
	rsb	rvb, rvb, #32		@ rvb <- right shift needed to get whole number
	eq	rva, #0x80000000	@ is fraction exactly 0.5 ?	
	beq	round2			@	if so,  jump to process that case
	tst	rva, #0x80000000	@ is fraction > 0.5
	itE	eq
	seteq	sv2, 0			@	if not, sv2 <- 0 (raw = o.k., not RAM pointer)
	setne	sv2, int_tag		@	if so,  sv2 <- 1 (int_tag)
	asr	rva, sv1, #2		@ rva <- re-get number from mantissa (raw int)
	cmp	rva, #0			@ is number negative?
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- -rva, make number positive
.ifndef	cortex
	add	rva, sv2, rva, ASR rvb	@ rva <- integer plus zero or one
.else
	asr	rva, rva, rvb		@ rva <- integer plus zero or one
	add	rva, sv2, rva		@ rva <- integer plus zero or one
.endif
round1:	
	postv	sv1			@ was mantissa positive?
	it	ne
	rsbne	rva, rva, #0		@	if not, rva <- integer, restored to proper sign
	set	pc,  lnk
round2:	asr	rva, sv1, #2		@ rva <- re-get number from mantissa (raw int)
	cmp	rva, #0			@ is number negative?
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- -rva, make number positive	
	asr	rva, rva, rvb		@ rva <- integer part of number
	tst	rva, #1			@ is number odd?
	it	ne
	addne	rva, rva, #1		@	if so,  rva <- number made even
	b	round1			@ jump to finish up

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.balign	4
	
taylor:	@ Taylor series computation
	@ in:	sv3 <- address of Taylor series coefficients	(floats)
	@ in:	sv4 <- argument					(float)
	@ out:	sv1 <- coeff + arg*(coeff + arg*(coeff + ...	(float)
	@ modifies:	sv1-sv3, rva-rvc
	set	sv1, f0			@ sv1 <- 0.0 = initial-result
tayllp:	@ loop
	set	sv2, sv4
	bl	adr_fltprd		@ sv1 <- fractional-arg * prev-rslt
	car	sv2, sv3		@ sv2 <- Taylor series coefficient
	bl	adr_fltpls		@ sv1 <- new-rslt=coef+frac-arg*prv-rslt
	incr	sv3, sv3		@ sv3 <- adrs of nxt Taylor series coef
	car	sv2, sv3		@ sv2 <- next coefficient
	nullp	sv2			@ is next coefficient null?
	bne	tayllp			@	if not, jmp to add trm to series
	set	pc,  cnt

.balign	4
	
_func_	
spcflt:	@ special values treatment for flt
	@ in:	sv1 <- value to check
	@ in:	sv5 <- special values jump table
	eq	sv1, #f0
	it	eq
	ldreq	pc, [sv5]		@ jump to special case for arg = 0.0
	lsr	rva, sv1, #23
	and	rva, rva, #0xff
	eq	rva, #0xff
	it	ne
	setne	pc,  lnk
	tst	sv1, #4
	bne	adr_nanfxt
	postv	sv1
	it	eq
	ldreq	pc, [sv5, #4]		@ jump to special case for arg = +inf
	ldr	pc, [sv5, #8]		@ jump to special case for arg = -inf

	/* (exp number)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"exp", 1, onumgto, null, nanfxt
	@  89 or greater gives inf, -102 or smaller gives 0.0

	/* special returns table for exp */
	startBVU8 expspc
	.word	adr_f1fxt		@ 1.0 <- (exp 0)
	.word	adr_infxt		@ inf <- (exp inf)
	.word	adr_f0fxt		@ 0.0 <- (exp -inf)
	ENDsized

	/* Taylor Series Coefficients (tsc) for exponential */
	startBVU8 exptsc
.word	0x32D7322A	@ 0011-0010-1101-0111-0011-0010-0010-1010 (/ 39916800)	
.word	0x3493F27E	@ 0011-0100-1001-0011-1111-0010-0111-1110 (/ 3628800)
.word	0x3638EF1E	@ 0011-0110-0011-1000-1110-1111-0001-1110 (/ 362880)
.word	0x37D00D02	@ 0011-0111-1101-0000-0000-1101-0000-0010 (/ 40320)
.word	0x39500D02	@ 0011-1001-0101-0000-0000-1101-0000-0010 (/ 5040)
.word	0x3AB60B62	@ 0011-1010-1011-0110-0000-1011-0110-0010 (/ 720)
.word	0x3C08888A	@ 0011-1100-0000-1000-1000-1000-1000-1010 (/ 120
.word	0x3D2AAAAA	@ 0011-1101-0010-1010-1010-1010-1010-1010 (/ 24)
.word	0x3E2AAAAA	@ 0011-1110-0010-1010-1010-1010-1010-1010 (/ 6)
.word	0x3F000002	@ 0011-1111-0000-0000-0000-0000-0000-0010 (/ 2)
.word	scheme_one	@ 0011-1111-1000-0000-0000-0000-0000-0010 (/ 1)
.word	scheme_one	@ 0011-1111-1000-0000-0000-0000-0000-0010 (1)
.word	scheme_null	@ null == end
	ENDsized

	/* exp for int */
	PRIMIT	intexp, ufun, 0
	postv	sv1			@ is number positive?
	itE	eq
	seteq	sv3, scheme_e		@	if so,  sv3 <- e   (sch float)
	setne	sv3, scheme_em1		@	if not, sv3 <- 1/e (sch float)
	iabs	sv5, sv1		@ sv5 <- n
	set	sv1, scheme_one
	adr	lnk, expilp
expilp:	@ loop
	eq	sv5, #i0		@ is n zero?
	it	eq
	seteq	pc,  cnt		@	if so,  exit with result
	sub	sv5, sv5, #4
	set	sv2, sv3
	b	adr_fltprd		@ sv1 <- result = prev rslt * (e or 1/e)

	/* exp for float */
	PRIMIT	fltexp, ufun, 0
	ldr	sv5, =expspc		@ sv5 <- special values jump table
	bl	spcflt			@ exit with val if sv1 = 0, +/-inf, nan
	set	sv4, sv1		@ sv4 <- number (saved)
	bl	fltmte			@ sv1 <- mantissa,  rva <- exponent
	cmp	rva, #148		@ is number too large for a scheme int
	it	mi
	bmi	expcnt			@	if not, jump to continue
	postv	sv4			@ is truncated float positive?
	beq	adr_infxt
	b	adr_f0fxt
expcnt:	@ keep going
	int2raw	rvb, sv1		@ rvb <- number
	rsb	rva, rva, #148		@ rvb <- rshift to get int part of num
	rsb	rvb, rvb, #0		@ rvb <- positive mantissa
	asr	rva, rvb, rva		@ rva <- number, shifted to integer
	rsb	rva, rva, #0		@ rva <- original mantissa
	raw2int	sv5, rva		@ sv5 <- n == ceiling of num (sch int)
	ngint	sv1, sv5
	set	sv2, sv4
	bl	i12flt
	bl	adr_fltpls		@ sv1 <- fraction = num - ceil-of-num
	set	sv4, sv1
	set	sv1, sv5	
	sav__c				@ dts <- (cnt ...)
	call	adr_intexp		@ sv1 <- exp(n) (scheme float)
	set	sv5, sv1		@ sv5 <- exp(n)
	ldr	sv3, =exptsc		@ sv3 <- address of series coefs
	call	taylor			@ sv1 <- res=coef+arg*(coef+arg*(coeff+.
	restor	cnt
	set	sv2, sv5
	set	lnk, cnt
	b	adr_fltprd
	
	/* exp for rational */
	PRIMIT	ratexp, ufun, 0
	bl	ir12fl			@ sv1 <- sch float frm int or rat in sv1
	@ continue to expflt
	b	adr_fltexp

	/* exp for complex */
	PRIMIT	cpxexp, ufun, 0
	spltcpx	sv1, sv2, sv1
	sav_rc	sv2			@ dts <- (y cnt ...)
	call	adr_fltexp		@ sv1 <- exp(x)
	car	sv2, dts		@ sv2 <- y
	save	sv1			@ dts <- (exp(x) y cnt ...)
	set	sv1, sv2		@ sv1 <- y
	call	adr_fltsin		@ sv1 <- sin(y)
	cadr	sv2, dts		@ sv2 <- y
	save	sv1			@ dts <- (sin(y) exp(x) y cnt ...)
	set	sv1, sv2		@ sv1 <- y
	call	adr_fltcos		@ sv1 <- cos(y)
	restor	sv2			@ sv2 <- sin(y), dts <- (exp(x) y cnt .)
	bl	makcpx			@ sv1 <- cos(y) + i sin(y)
	restor	sv2			@ sv2 <- exp(x), dts <- (y cnt ...)
	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,	 dts <- (...)
	set	lnk, cnt		@ lnk <- cnt
	b	uniprd

	/* (log number)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"log", 1, onumgto, null, nanfxt, intlog, fltlog, intlog, cpxlog

	/* special returns table for log */
	startBVU8 logspc
	.word	adr_ninfxt		@     -inf <- (log 0)
	.word	adr_infxt		@      inf <- (log inf)
	.word	adr_ipifxt		@ inf+pi.i <- (log -inf)
	ENDsized

	/* Taylor Series Coefficients (tsc) for log function */
	startBVU8 logtsc
	.word	0x3D579436		@ 0011-1101-0101-0111-1001-0100-0011-0110 (/ 19)
	.word	0xBD638E3A		@ 1011-1101-0110-0011-1000-1110-0011-1010 (/ -18)
	.word	0x3D70F0F2		@ 0011-1101-0111-0000-1111-0000-1111-0010 (/ 17)
	.word	0xBD800002		@ 1011-1101-1000-0000-0000-0000-0000-0010 (/ -16)
	.word	0x3D88888A		@ 0011-1101-1000-1000-1000-1000-1000-1010 (/ 15)
	.word	0xBD924926		@ 1011-1101-1001-0010-0100-1001-0010-0110 (/ -14)
	.word	0x3D9D89DA		@ 0011-1101-1001-1101-1000-1001-1101-1010 (/ 13)
	.word	0xBDAAAAAA		@ 1011-1101-1010-1010-1010-1010-1010-1010 (/ -12)
	.word	0x3DBA2E8A		@ 0011-1101-1011-1010-0010-1110-1000-1010 (/ 11)
	.word	0xBDCCCCCE		@ 1011-1101-1100-1100-1100-1100-1100-1110 (/ -10)
	.word	0x3DE38E3A		@ 0011-1101-1110-0011-1000-1110-0011-1010 (/ 9)
	.word	0xBE000002		@ 1011-1110-0000-0000-0000-0000-0000-0010 (/ -8)
	.word	0x3E124926		@ 0011-1110-0001-0010-0100-1001-0010-0110 (/ 7)
	.word	0xBE2AAAAA		@ 1011-1110-0010-1010-1010-1010-1010-1010 (/ -6)
	.word	0x3E4CCCCE		@ 0011-1110-0100-1100-1100-1100-1100-1110 (/ 5)
	.word	0xBE800002		@ 1011-1110-1000-0000-0000-0000-0000-0010 (/ -4)
	.word	0x3EAAAAAA		@ 0011-1110-1010-1010-1010-1010-1010-1010 (/ 3)
	.word	0xBF000002		@ 1011-1111-0000-0000-0000-0000-0000-0010 (/ -2)
	.word	scheme_one		@ 0011-1111-1000-0000-0000-0000-0000-0010 (/ 1.0)
	.word	float_tag		@ 0000-0000-0000-0000-0000-0000-0000-0010 = 0.0
	.word	scheme_null		@ null == end
	ENDsized

	/* exit with -inf */
	PRIMIT	ninfxt, ufun, 0
	set	sv1, scheme_inf		@ sv1 <- inf (scheme float)
	ngflt	sv1, sv1
	set	pc,  cnt

	/* exit with inf + pi.i */
	PRIMIT	ipifxt, ufun, 0
	set	sv1, scheme_inf		@ sv1 <- inf (scheme float)
	b	lognxt

	/* log for integer and rational */
	PRIMIT	intlog, ufun, 0
	bl	ir12fl			@ sv1 <- sch float frm int or rat in sv1
	b	adr_fltlog

	/* log for float */
	PRIMIT	fltlog, ufun, 0
	ldr	sv5, =logspc		@ sv5 <- special values jump table
	bl	spcflt			@ exit w/value if sv1 = 0, +/-inf or nan
	postv	sv1			@ is number >= 0?
	bne	logneg			@	if not, jump to log of negtv num
	set	sv2, scheme_one		@ sv2 <- 1.0
	set	rva, 0x7F800000		@ rva <- mask for float's exponent
	and	rvb, sv1, rva		@ rvb <- num's base 2 bsd expo, in pos
	bic	sv1, sv1, rva		@ sv1 <- number without exponent (float)
	lsr	rvb, rvb, #23		@ rvb <- num's base 2 bsd expo (raw int)
	eq	rvb, #0			@ is expo zero? (num is denormalized?)
	bne	logcnt			@	if not,  jump to continue
	eor	sv1, sv1, #0x03		@ sv1 <- number as pseudo-scheme-int
	set	rva, 31			@ rva <- 31 (pos of first potential msb)
intms0:	lsr	rvb, sv1, rva
	tst	sv1, rvb		@ is bit rva a 1 (test against int_tag)?
	it	eq
	subeq	rva, rva, #1		@ rva <- (rva - 1) = next possible msb
	beq	intms0			@ jump to test that bit
	rsb	rvb, rva, #23		@ rvb <- shift to pseud-normalize number
	bic	rva, sv1, #int_tag	@ rva <- number without tag
	lsl	rva, rva, rvb		@ rva <- num shftd to pseudo-nrmlzd form
	bic	rva, rva, #0x00800000	@ rva <- pseud-nrmlzd num without 1. bit
	orr	sv1, rva, #float_tag	@ sv1 <- pseud-nrmlzd num (scheme float)
	rsb	rvb, rvb, #1		@ rvb  <- num's bsd expo (neg, raw int)
logcnt:	sub	rvb, rvb, #127		@ rvb <- power-of+2 of number (raw int)
	lsl	rvb, rvb, #2		@ rvb <- power-of+2, shifted
	orr	sv3, rvb, #int_tag	@ sv3 <- power-of+2 (scheme int)
	orr	sv1, sv1, sv2		@ sv1 <- num w/bsd expo=127, 1.0<=to<2.0
	tst	sv1, #0x00400000	@ is number >= 1.5?
	itT	ne
	bicne	sv1, sv1, #0x00800000	@	if so,  sv1 <- num rscld 0.75->1
	addne	sv3, sv3, #4		@	if so,  sv3 <- pow2 for expo 126
	ngflt	sv2, sv2		@ sv2 <- -1.0
	bl	adr_fltpls		@ sv1 <- adjusted-number-minus-1
	set	sv4, sv1
	set	sv1, sv3		@ sv1 <- power-of+2 (scheme int)
	set	sv2, scheme_log_two	@ sv2 <- (log 2)
	bl	uniprd
	set	sv5, sv1
	ldr	sv3, =logtsc		@ sv3 <- address of Taylor series coeffs
	sav__c				@ dts <- (cnt ...)
	call	taylor			@ sv1 <- res=coef+arg*(coef+arg*(coef+..
	restor	cnt
	set	sv2, sv5
	set	lnk, cnt
	b	adr_fltpls

logneg:	@ log of negative flt
	sav__c				@ dts <- (cnt ...)
	ngflt	sv1, sv1
	call	adr_fltlog
	restor	cnt
lognxt:	@ common exit for case with negative arg
	set	sv2, scheme_pi
	set	lnk, cnt
	b	makcpx

	/* log for complex */
	PRIMIT	cpxlog, ufun, 0
	sav_rc	sv1			@ dts <- (z cnt ...)
	call	adr_cpxang		@ sv1 <- angle(z)
	restor	sv2			@ sv2 <- z,	dts <- (cnt ...)
	save	sv1			@ dts <- (angle(z) cnt ...)
	set	sv1, sv2		@ sv1 <- z
	call	adr_cpxmag		@ sv1 <- magnitude(z)
	call	adr_fltlog		@ sv1 <- log(magnitude(z))
	restor	sv2, cnt		@ sv2 <- angle(z), cnt <-cnt, dts <-(..)
	set	lnk, cnt		@ lnk <- cnt
	b	makcpx			@ sv1 <- log(magnitude(z)) + i angle(z)

	/* (sin angle)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"sin", 1, onumgto, null, nanfxt, intsin, fltsin, intsin, cpxsin

	/* special returns table for sin */
	startBVU8 sinspc
	.word	adr_f0fxt		@ 0.0 <- (sin 0)
	.word	adr_nanfxt		@ nan <- (sin inf)
	.word	adr_nanfxt		@ nan <- (sin -inf)
	ENDsized

	/* Taylor Series Coefficients (tsc) for sine function */
	startBVU8 sintsc
	.word	0x2F30922E		@ 0010-1111-0011-0000-1001-0010-0010-1110 (/ 13!)	
	.word	0xB2D7322A		@ 1011-0010-1101-0111-0011-0010-0010-1010 (/ -39916800)	
	.word	0x3638EF1E		@ 0011-0110-0011-1000-1110-1111-0001-1110 (/ 362880)
	.word	0xB9500D02		@ 1011-1001-0101-0000-0000-1101-0000-0010 (/ -5040)
	.word	0x3C08888A		@ 0011-1100-0000-1000-1000-1000-1000-1010 (/ 120
	.word	0xBE2AAAAA		@ 1011-1110-0010-1010-1010-1010-1010-1010 (/ -6)
	.word	scheme_one		@ 0011-1111-1000-0000-0000-0000-0000-0010 (/ 1.0)
	.word	scheme_null		@ null == end
	ENDsized

	/* sin for integer and rational */
	PRIMIT	intsin, ufun, 0
	bl	ir12fl			@ sv1 <- sch float frm int or rat in sv1
	b	adr_fltsin
	
	/* sin for float */
	PRIMIT	fltsin, ufun, 0
	ldr	sv5, =sinspc		@ sv5 <- special values jump table
	bl	spcflt			@ exit with value if sv1 = 0,+/-inf,nan
	set	sv3, f0			@ sv3 <- +0.0 = sign of result (sch flt)
	postv	sv1			@ is angle positive?
	itT	ne
	ngfltne	sv1, sv1		@	if not, sv1 <- positive angle
	ngfltne	sv3, sv3		@	if not, sv3 <- -0., ngtv sgn res
	set	sv5, scheme_two_pi	@ sv5 <- 2pi
	cmp	sv5, sv1		@ is angle <= 2pi ?
	bpl	sin1			@	if so,  jump to continue
	set	sv5, sv1
	set	sv2, scheme_two_pi	@ sv2 <- 2pi
	bl	adr_fltdiv		@ sv1 <- angle/2pi
	set	sv2, sv1		@ sv2 <- angle/2pi (saved)
	bl	fltmte			@ sv1 <- mantissa,  rva <- exponent
	cmp	rva, #148		@ is angle/2pi too large for scheme int
	bpl	adr_nanfxt
	rsb	rva, rva, #150		@ rva <- rshift to get int part of num
	asr	rva, sv1, rva		@ rva <- number, shifted to integer
	raw2int	sv1, rva		@ sv1 <- n == truncated(angle/2pi)
	bl	i12flt
	set	sv2, scheme_two_pi	@ sv2 <- 2pi
	bl	adr_fltprd		@ sv1 <- angle as n * 2pi
	ngflt	sv2, sv1		@ sv2 <- minus angle as n * 2pi
	set	sv1, sv5	
	bl	adr_fltpls		@ sv1 <- angle remapped == angle - n*2pi
sin1:	set	sv5, scheme_pi		@ sv5 <- pi
	cmp	sv5, sv1		@ is angle <= pi ?
	itTTT	mi
	ngfltmi	sv1, sv1		@	if not, sv1 <- -angle
	ngfltmi	sv3, sv3		@	if not, sv3 <- -sign
	setmi	sv2, scheme_two_pi	@	if not, sv2 <- 2pi
	blmi	adr_fltpls		@	if not, sv1 <- (- 2pi angle)
	set	sv5, scheme_half_pi	@ sv5 <- pi/2
	cmp	sv5, sv1		@ is angle <= pi/2 ?
	itTT	mi
	ngfltmi	sv1, sv1		@	if not, sv1 <- -angle
	setmi	sv2, scheme_pi		@	if not, sv2 <- pi
	blmi	adr_fltpls		@	if not, sv1 <- (- pi angle)
	@ calculate sine of angle between 0 and pi/2
	bic	rva, sv3, #float_tag	@ rva <- sign, raw
	orr	sv1, sv1, rva		@ sv1 <- signed-angle
	set	sv5, sv1
	set	sv2, sv1		@ sv2 <- signed-angle
	bl	adr_fltprd		@ sv1 <- angle-squared	
	set	sv4, sv1
	ldr	sv3, =sintsc		@ sv3 <- address of Taylor series coeffs
	sav__c				@ dts <- (cnt ...)
	call	taylor			@ sv1 <- res=coef+arg*(coef+arg*(coef+..
	restor	cnt
	@ multiply result by signed-angle and exit
	set	sv2, sv5
	set	lnk, cnt
	b	adr_fltprd

	/* sin for complex */
	PRIMIT	cpxsin, ufun, 0
	sav_rc	sv1			@ dts <- (z cnt ...)
	real	sv1, sv1		@ sv1 <- x
	call	adr_fltcos		@ sv1 <- cos(x)
	car	sv2, dts		@ sv2 <- z
	save	sv1			@ dts <- (cos(x) z cnt ...)
	real	sv1, sv2		@ sv1 <- x
	call	adr_fltsin		@ sv1 <- sin(x)
	cadr	sv2, dts		@ sv2 <- z
	save	sv1			@ dts <- (sin(x) cos(x) z cnt ...)
	imag	sv1, sv2		@ sv1 <- y
	call	adr_fltexp		@ sv1 <- exp(y)
	caddr	sv2, dts		@ sv2 <- z
	save	sv1			@ dts <- (exp(y) sin(x) cos(x) z cnt ..)
	imag	sv1, sv2		@ sv1 <- y
	ngflt	sv1, sv1		@ sv1 <- -y
	call	adr_fltexp		@ sv1 <- exp(-y)
	restor	sv2			@ sv2 <- exp(y), dts<-(snx csx z cnt ..)
	set	sv4, sv1		@ sv4 <- exp(-y)
	set	sv5, sv2		@ sv5 <- exp(y)
	bl	adr_fltpls		@ sv1 <- exp(y) + exp(-y)
	ngflt	sv2, sv4		@ sv2 <- -exp(-y)
	set	sv4, sv1		@ sv4 <- exp(y) + exp(-y)
	set	sv1, sv5		@ sv1 <- exp(y)
	bl	adr_fltpls		@ sv1 <- exp(y) - exp(-y)
	restor	sv5, sv2		@ sv5 <- sin(x), sv2<-csx,dts<-(z cnt .)
	bl	adr_fltprd		@ sv1 <- [exp(y) - exp(-y)] * cos(x)
	swap	sv1, sv5, sv3		@ sv1 <- sin(x), sv5<-[expy-exp(-y)]*csx
	set	sv2, sv4		@ sv2 <- exp(y) + exp(-y)
	bl	adr_fltprd		@ sv1 <- [exp(y) + exp(-y)] * sin(x)
	set	sv2, sv5		@ sv2 <- [exp(y) - exp(-y)] * cos(x)
	bl	makcpx			@ sv1 <- [exy+ex(-y)]*sx+i[exy-e(-y)]*cx
	set	sv2, 0x09		@ sv2 <- 2 (scheme int)
	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,		dts <- (...)
	set	lnk, cnt		@ lnk <- cnt
	b	unidiv

	/* (cos angle)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"cos", 1, onumgto, null, nanfxt, intcos, fltcos, intcos, cpxcos

	/* cos for integer and rational */
	PRIMIT	intcos, ufun, 0
	bl	ir12fl			@ sv1 <- sch float frm int or rat in sv1
	b	adr_fltcos

	/* cos for float */
	PRIMIT	fltcos, ufun, 0
	set	sv2, scheme_half_pi	@ sv2 <- pi/2 (scheme float)
	adr	lnk, adr_fltsin
	b	adr_fltpls

	/* cos for complex */
	PRIMIT	cpxcos, ufun, 0
	set	sv2, scheme_half_pi	@ sv2 <- pi/2 (scheme float)
	bl	unipls
	floatp	sv1
	beq	adr_fltsin		@	if so,  cos <- sin(angle+pi/2)
	b	adr_cpxsin		@ branch to calc cos as sin(angle+pi/2)

	/* (tan angle)    		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"tan", pfun, 1
	sav_rc	sv1			@ dts <- (angle cnt ...)
	ldr	rvc, =tb_numcos
	call	numjmp			@ sv1 <- cos(angle)
	set	sv2, sv1		@ sv2 <- cos(angle)
	restor	sv1			@ sv1 <- angle, dts <- (cnt ...)
	save	sv2			@ dts <- (cos(angle) cnt ...)
	ldr	rvc, =tb_numsin
	call	numjmp			@ sv1 <- sin(angle)
	restor	sv2, cnt		@ sv2 <- cos(angle), cnt<-cnt, dts<-(..)
	set	lnk, cnt
	b	unidiv
	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg
	
	/* (asin number)   A.S. 4.4.46	~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"asin", asn, 1, onumgto, null, nanfxt, intasn, fltasn, intasn, cpxasn

	/* special returns table for asin */
	startBVU8 asnspc
	.word	adr_f0fxt		@ 0.0 <- (asin 0)
	.word	adr_nanfxt		@ nan <- (asin inf)
	.word	adr_nanfxt		@ nan <- (asin -inf)
	ENDsized

	/* Taylor Series Coefficients (tsc) for arcsine function */
	startBVU8 asntsc
	.word	0xBAA57A2A		@ 1011-1010-1010-0101-0111-1010-0010-1010  -0.0012624911
	.word	0x3BDA90C6		@ 0011-1011-1101-1010-1001-0000-1100-0110   0.0066700901
	.word	0xBC8BFC66		@ 1011-1100-1000-1011-1111-1100-0110-0110  -0.0170881256
	.word	0x3CFD10FA		@ 0011-1100-1111-1101-0001-0000-1111-1010   0.0308918810
	.word	0xBD4D8392		@ 1011-1101-0100-1101-1000-0011-1001-0010  -0.0501743046
	.word	0x3DB63A9E		@ 0011-1101-1011-0110-0011-1010-1001-1110   0.0889789874
	.word	0xBE5BBFCA		@ 1011-1110-0101-1011-1011-1111-1100-1010  -0.2145988016
	.word	scheme_half_pi		@ pi/2
	.word	scheme_null		@ null == end
	ENDsized

	/* asin for integer and rational */
	PRIMIT	intasn, ufun, 0
	bl	ir12fl			@ sv1 <- sch float frm int or rat in sv1
	b	adr_fltasn
	
	/* asin for float */
	PRIMIT	fltasn, ufun, 0
	ldr	sv5, =asnspc		@ sv5 <- special values jump table
	bl	spcflt			@ exit with value if sv1 = 0,+/-inf,nan
	fabs	rva, sv1
	set	rvb, 0x3D5F6376
	cmp	rva, rvb
	bmi	asnfsm
	set	sv4, sv1
	set	sv1, scheme_one		@ sv3 <- 1 (sign as scheme float)
	fabs	sv3, sv4
	cmp	sv1, sv3
	itTT	mi
	setmi	sv1, sv4
	adrmi	lnk, adr_cpxasn
	setmi	sv2, f0
	bmi	flt2cpx
	postv	sv4			@ is number positive?
	itT	ne
	ngfltne	sv4, sv4		@	if not, sv3 <- minus number
	ngfltne	sv1, sv1		@	if not, sv1 <- -1, sign
	set	sv5, sv1
	sav__c				@ dts <- (cnt ...)
	ldr	sv3, =asntsc		@ sv3 <- address of Taylor series coeffs
	call	taylor			@ sv1 <- res=coef+arg*(coef+arg*(coef+..
	@ finish up
	restor	cnt
	set	sv3, sv4
	set	sv4, sv1
	ngflt	sv1, sv3		@ sv1 <- minus abs-number
	set	sv2, scheme_one		@ sv2 <- 1.0 (scheme float)
	bl	adr_fltpls		@ sv1 <- 1.0 - abs-number
	save	sv4, sv5, cnt	
	call	adr_fltsqr		@ sv1 <- sqrt(1-x)
	restor	sv2
	bl	uniprd
	set	sv2, sv1		@ sv2 <- result * sqrt(1-x)
	set	sv1, scheme_half_pi	@ sv1 <- pi/2
	bl	unimns			@ sv1 <- pi/2 - result * sqrt(1-x)
	restor	sv2, cnt
	set	lnk, cnt
	b	uniprd			@ sv1 <- sign*[pi/2-result*sqrt(1-x)]
		
asnfsm:	@ asin for small arg (< 0.0545382) -- formerly (< 1/32)
	set	sv2, sv1
	set	sv4, sv1
	bl	adr_fltprd
	set	sv2, 0x19		@ sv2 <- 6 (scheme int)
	bl	unidiv
	set	sv2, scheme_one
	bl	adr_fltpls
	set	sv2, sv4
	set	lnk, cnt
	b	adr_fltprd		@ sv1 <- x*(1+x^2/6)

	/* asin for complex */
	PRIMIT	cpxasn, ufun, 0
	@
	@ for accuracy:
	@	if real part is negative:	
	@	1) negate the whole z
	@	2) compute asin
	@	3) negate the whole complex result
	@ (else, i z + sqrt(1 - z^2) gives huge roundoff err, eg. when z=-1000)
	@
	sav_rc	sv1			@ dts <- (z cnt ...)
	set	sv2, sv1		@ sv2 <- z
	bl	adr_cpxprd			@ sv1 <- z^2
	set	sv2, sv1		@ sv2 <- z^2
	set	sv1, i1			@ sv1 <- 1 (scheme int)
	bl	unimns			@ sv1 <- 1 - z^2
	ldr	sv5, =tb_numsqr
	call	unijmp			@ sv1 <- sqrt(1 - z^2)
	restor	sv2			@ sv2 <- z,		dts <- (cnt ...)
	bl	uninum
	spltcpx	sv4, sv3, sv1		@ sv4 <- r(sq(1-z^2)), sv3<-i(sq(1-z^2))
	spltcpx	sv2, sv1, sv2		@ sv2 <- x,  sv1 <- y
	fabs	rva, sv3		@ rva <- |imag(sqrt(1 - z^2))|
	fabs	rvb, sv4		@ rva <- |real(sqrt(1 - z^2))|
	cmp	rva, rvb		@ is |im(sq(1-z^2))| > |re(sq(1-z^2))|
	bpl	asncp0
	@ if sv4 and sv1 have diff sgns, normal case, neg sv1, else neg sv2
	eor	rva, sv1, sv4
	postv	rva
	itTEE	ne
	ngfltne	sv1, sv1		@ sv1 <- -y
	setne	sv5, true
	ngflteq	sv2, sv2		@ sv2 <- -x
	seteq	sv5, false
	b	asncp1
asncp0:	@ if sv3 and sv2 have same sign, normal case, negat sv1, else negat sv2
	eor	rva, sv2, sv3
	postv	rva
	itTEE	ne
	ngfltne	sv2, sv2		@ sv2 <- -x
	setne	sv5, false
	ngflteq	sv1, sv1		@ sv1 <- -y
	seteq	sv5, true
asncp1:	@ continue
	fabs	rva, sv1
	eq	rva, #f0
	it	eq
	seteq	sv1, f0
	fabs	rva, sv2
	eq	rva, #f0
	it	eq
	seteq	sv2, f0
	swap	sv1, sv3, rva		@ sv1 <- imag(sqrt(1-z^2)), sv3 <-(-/+)y
	bl	adr_fltpls		@ sv1 <- (+/-) x + imag( sqrt(1 - z^2) )
	swap	sv1, sv3, rva		@ sv1 <- (-/+) y, sv3<-+/-x+i(sq(1-z^2))
	set	sv2, sv4		@ sv2 <- real( sqrt(1 - z^2) )
	bl	adr_fltpls		@ sv1 <- (-/+) y + real( sqrt(1 - z^2) )
	set	sv2, sv3		@ sv2 <- (+/-) x + imag( sqrt(1 - z^2) )
	bl	makcpx			@ sv1 <- (+/-) i z + sqrt(1 - z^2)
	save	sv5			@ dts <- (sign cnt ...)
	ldr	rvc, =tb_numlog
	call	numjmp			@ sv1 <- log((+/-) i z + sqrt(1 - z^2))
	restor	sv5, cnt		@ sv5 <- sign, cnt <- cnt, dts <- (...)
	floatp	sv1
	beq	asncp2
	spltcpx	sv2, sv1, sv1		@ sv2 <- real(log((+/-)iz+sqrt(1-z^2)))
					@ sv1 <- imag(log((+/-)iz+sqrt(1-z^2)))
	eq	sv5, #t
	itE	eq
	ngflteq	sv2, sv2		@	if so,  sv2<--r(l(+-iz+s(1-z2)))
	ngfltne	sv1, sv1		@	if not, sv1<--i(l(+-iz+s(1-z2)))
	fabs	sv3, sv2
	eq	sv3, #f0
	it	eq
	seteq	sv2, f0
	set	lnk, cnt		@ lnk <- cnt
	b	makcpx			@ sv1 <- log( i z + sqrt(1 - z^2) ) / i
asncp2:	@ purely imaginary argument
	eq	sv5, #t
	itE	eq
	ngflteq	sv2, sv1		@	if so,  sv2 <- -lg(iz+sq(1-z^2))
	setne	sv2, sv1		@	if not, sv2 <-  lg(iz+sq(1-z^2))
	set	sv1, f0			@ sv1 <- 0.0
	set	lnk, cnt		@ lnk <- cnt
	b	makcpx			@ sv1 <- log( i z + sqrt(1 - z^2) ) / i

	/* (acos z)   			~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"acos", pfun, 1
	sav__c				@ dts <- (cnt ...)
	ldr	rvc, =tb_numasn
	call	numjmp
	restor	cnt
	set	sv2, sv1
	set	sv1, scheme_half_pi	@ sv1 <- pi/2
	set	lnk, cnt
	b	unimns

	/* (atan y <x>)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"atan", atn, 2, onumgto, null, nanfxt, intatn, fltatn, intatn, cpxatn

	/* special returns table for atan */
	startBVU8 atnspc
	.word	adr_pifxt		@    pi <- (atan 0), (0.0) dealt intrnly
	.word	adr_p2fxt		@  pi/2 <- (atan inf)
	.word	adr_np2fxt		@ -pi/2 <- (atan -inf)
	ENDsized

	/* Taylor Series Coefficients (tsc) for arctangent function */
	startBVU8 atntsc
	.word	0x3B3BD74A		@ 0011-1011-0011-1011-1101-0111-0100-1010   0.0028662257
	.word	0xBC846E02		@ 1011-1100-1000-0100-0110-1110-0000-0010  -0.0161657367
	.word	0x3D2FC1FE		@ 0011-1101-0010-1111-1100-0001-1111-1110   0.0429096138
	.word	0xBD9A3176		@ 1011-1101-1001-1010-0011-0001-0111-0110  -0.0752896400
	.word	0x3DDA3D82		@ 0011-1101-1101-1010-0011-1101-1000-0010   0.1065626393
	.word	0xBE117FC6		@ 1011-1110-0001-0001-0111-1111-1100-0110  -0.1420889944
	.word	0x3E4CBBE6		@ 0011-1110-0100-1100-1011-1011-1110-0110   0.1999355085
	.word	0xBEAAAA6A		@ 1011-1110-1010-1010-1010-1010-0110-1010  -0.3333314528
	.word	scheme_one		@ one
	.word	scheme_null		@ null == end
	ENDsized

	/* exit with pi */
	PRIMIT	pifxt, ufun, 0
	set	sv1, scheme_pi		@ sv1 <- pi
	set	pc,  cnt
	
	/* exit with pi/2 */
	PRIMIT	p2fxt, ufun, 0
	set	sv1, scheme_half_pi	@ sv1 <- pi/2
	set	pc,  cnt

	/* exit with -pi/2 */
	PRIMIT	np2fxt, ufun, 0
	set	sv1, scheme_half_pi	@ sv1 <- pi/2
	ngflt	sv1, sv1
	set	pc,  cnt

	/* atan for integer and rational */
	PRIMIT	intatn, ufun, 0	
	bl	ir12fl			@ sv1 <- sch float frm int or rat in sv1
	b	adr_fltatn

	/* atan for float */
	PRIMIT	fltatn, ufun, 0	
	nullp	sv2
	it	eq
	seteq	sv2, scheme_one		@	if not, sv2 <- 1.0 (sch float)
	nmbrp	sv2			@ is sv2 a number?
	bne	adr_nanfxt
	bl	uninum			@ sv1 <- y (sch flt), sv2 <- x (sch flt)
	floatp	sv2			@ is sv2 a float (eg. rather than cpx)?
	bne	adr__err
	set	sv3, f0
	postv	sv1
	it	ne
	orrne	sv3, sv3, #4
	postv	sv2
	it	ne
	orrne	sv3, sv3, #8
	bl	adr_fltdiv		@ sv1 <- arg == y / x
	eq	sv1, #f0		@ is arg = 0.0?
	it	eq
	tsteq	sv3, #8			@	if so,  was x positive?
	it	eq
	seteq	pc,  cnt		@	if so,  exit with 0.0
	ldr	sv5, =atnspc		@ sv5 <- special values jump table
	bl	spcflt			@ exit with value if sv1 = 0,+/-inf,nan
	fabs	sv1, sv1		@ sv1 <- absolute value of arg
	set	sv2, scheme_one		@ sv2 <- 1.0
	cmp	sv2, sv1		@ is arg > 1.0 ?
	itT	mi
	orrmi	sv3, sv3, #16		@	if so,  sv3 <- qdrnt w/invr flag
	swapmi	sv1, sv2, sv4		@	if so,  sv1<-1.,sv2<-arg,sv4=tmp
	it	mi
	blmi	adr_fltdiv		@	if so,  sv1 <- 1.0/arg
	set	sv5, sv3		@ sv5 <- quadrant	(saved)
	set	sv4, sv1		@ sv4 <- arg		(saved)
	set	sv2, sv1		@ sv2 <- arg
	bl	adr_fltprd		@ sv1 <- arg**2
	save	sv4, sv5, cnt
	set	sv4, sv1
	ldr	sv3, =atntsc		@ sv3 <- address of Taylor series coeffs
	call	taylor			@ sv1 <- res=coef+arg*(coef+arg*(coef+..
	restor	sv2, sv3, cnt
	@ post-processing
	bl	adr_fltprd		@ sv1 <- angle = arg*result
	tst	sv3, #16		@ was tangent inverted?
	itTT	ne
	ngfltne	sv1, sv1		@	if so,  sv1 <- -angle
	setne	sv2, scheme_half_pi	@	if so,  sv2 <- pi/2
	blne	adr_fltpls		@	if so,  sv1 <- pi/2 - angle
	tst	sv3, #8			@ is x negative?
	itTT	ne
	orrne	sv1, sv1, #0x80000000	@	if so,  sv1 <- -angle
	setne	sv2, scheme_pi		@	if so,  sv2 <- pi
	blne	adr_fltpls		@	if so,  sv1 <- pi - angle
	tst	sv3, #4			@ is y negative?
	it	ne
	ngfltne	sv1, sv1		@	if so,  sv1 <- -angle
	set	pc,  cnt

	/* atan for complex */
	PRIMIT	cpxatn, ufun, 0	
	sav_rc	sv1			@ dts <- (z cnt ...)
	imag	sv2, sv1		@ sv2 <- y
	set	sv1, scheme_one		@ sv1 <- 1.0
	bl	adr_fltpls		@ sv1 <- 1 + y
	car	sv2, dts		@ sv2 <- z
	real	sv2, sv2		@ sv2 <- x
	eq	sv2, #f0		@ <- don't negat real part if zero, it rslts in bad cpx
					@    both makcpx and ngflt could/should account for this
	it	ne
	ngfltne	sv2, sv2		@	if not, sv2 <- -x
	bl	makcpx			@ sv1 <- 1 - i z = 1 + y - i x
	ldr	rvc, =tb_numlog
	call	numjmp			@ sv1 <- log(1 - i z)
	restor	sv4			@ sv4 <- z,		dts <- (cnt ...)
	save	sv1			@ dts <- (log(1 - i z) cnt ...)
	imag	sv2, sv4		@ sv2 <- y
	set	sv1, scheme_one		@ sv1 <- 1.0
	bl	adr_fltmns		@ sv1 <- 1 - y
	real	sv2, sv4		@ sv2 <- x
	bl	makcpx			@ sv1 <-  1 + i z = 1 - y + i x
	ldr	rvc, =tb_numlog
	call	numjmp			@ sv1 <- log(1 + i z)
	restor	sv2			@ sv2 <- log(1 - i z),	dts <- (cnt ...)
	bl	unimns			@ sv1 <- log(1 + i z) - log(1 - i z)	
	set	sv2, i2			@ sv2 <- 2 (scheme int)
	bl	uninum
	restor	cnt			@ cnt <- cnt,		dts <- (...)
	floatp	sv1
	beq	atncp2
	bl	adr_cpxdiv
	spltcpx	sv2, sv1, sv1
	ngflt	sv2, sv2
	set	lnk, cnt
	b	makcpx
atncp2:	@ purely imaginary argument
	bl	adr_fltdiv
	ngflt	sv2, sv1
	set	sv1, f0
	set	lnk, cnt
	b	makcpx

	/* (sqrt number)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"sqrt", sqr, 1, onumgto, null, nanfxt, intsqr, fltsqr, intsqr, cpxsqr

	/* special returns table for sqrt */
	startBVU8 sqrspc
	.word	adr_f0fxt		@     0.0 <- (sqrt 0)
	.word	adr_infxt		@     inf <- (sqrt inf)
	.word	adr_ziifxt		@ 0+inf.i <- (sqrt -inf)
	ENDsized

	/* exit with 0.0 + inf.i */
	PRIMIT	ziifxt, ufun, 0
	set	sv1, f0
	set	sv2, scheme_inf		@ sv2 <- inf (scheme float)
	set	lnk, cnt
	b	makcpx

	/* sqrt for integer and rational */
	PRIMIT	intsqr, ufun, 0
	bl	ir12fl			@ sv1 <- scheme float from scheme int or rational in sv1
	b	adr_fltsqr

	/* sqrt for float */
	PRIMIT	fltsqr, ufun, 0
	ldr	sv5, =sqrspc		@ sv5 <- special values jump table
	bl	spcflt			@ exit with value if sv1 = 0, +/-inf or nan
	@ continue to sqrcpx
	b	adr_cpxsqr
	
	/* sqrt for complex */
	PRIMIT	cpxsqr, ufun, 0
	sav_rc	sv1			@ dts <- (z cnt ...)
	ldr	rvc, =tb_numlog
	call	numjmp			@ sv1 <- log(z)
	set	sv2, scheme_0p5		@ sv2 <- 0.5
	bl	uniprd			@ sv1 <- 0.5*log(z)
	ldr	rvc, =tb_numexp
	call	numjmp			@ sv1 <- sqrt(z)	
	set	sv4, sv1		@ sv4 <- sqrt(z)
	set	sv2, sv1		@ sv2 <- sqrt(z)
	restor	sv1, cnt		@ sv1 <- z, cnt <- cnt, dts <- (...)
	@ refinement
	bl	unidiv			@ sv1 <- z / sqrt(z)
	set	sv2, sv4		@ sv2 <- sqrt(z)
	bl	unipls			@ sv1 <- sqrt(z) + z / sqrt(z)
	set	sv2, scheme_two		@ sv2 <- 2.0
	set	lnk, cnt
	b	unidiv			@ sv1 <- [sqrt(z) + z / sqrt(z)] / 2

	/* (expt z1 z2)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"expt", pfun, 2
	intgrp	sv2
	beq	exptnt
	nmbrp	sv2
	bne	adr__err
	isnan	sv2
	beq	adr__err
	set	sv3, scheme_one		@ sv3 <- 1.0
	eq	sv1, sv3		@ is z1 = 1.0?
	it	ne
	eqne	sv1, #5			@	if not, is z1 = 1?
	it	ne
	eqne	sv2, sv3		@	if not, is z2 = 1.0?
	it	ne
	eqne	sv2, #5			@	if not, is z1 = 1?
	it	eq
	seteq	pc,  cnt		@	if so,  exit with z1
	anyzro	sv1, sv2		@ is z1 or z2 = 0 or 0.0?
	beq	exptzr			@	if so,  jump to that case
	anyinf	sv1, sv2		@ is z1 or z2 = +/- inf?
	beq	exptnn			@	if so,  jump to that case
exptrg:	@
	@ to do:
	@ check if z2 is an integer when z1 < 0, else, z1 < 0 => nan through log (complex num)
	@ --> split z1 into integer & fractional part & process expt accordingly, skipping log if
	@ fractional part is zero
	@
	sav_rc	sv2			@ dts <- (z2 cnt ...)
	ldr	rvc, =tb_numlog
	call	numjmp			@ sv1 <- log(z1)
	restor	sv2, cnt		@ sv2 <- z2, cnt <- cnt, dts <- (...)
	bl	uniprd			@ sv1 <- z2*log(z1),		returns via lnk
	ldr	sv5, =tb_numexp
	b	unijmp			@ sv1 <- exp(z2*log(z1)),	returns via cnt

exptzr:	@ expt with z1 or z2 = 0
	zerop	sv2			@ is z2 = 0?
	itT	eq
	seteq	sv1, sv3		@	if so,  sv1 <- 1.0
	seteq	pc,  cnt		@	if so,  exit with 1.0
	postv	sv2			@ is z2 positive?
	bne	adr_infxt	
	set	pc,  cnt		@ exit with 0, 0.0 or inf
exptnn:	@ expt with z1 or z2 = nan
	postv	sv2			@ is z2 positive?
	beq	exptpn			@	if so,  jump to process positive exponent
	cmp	sv1, sv3		@ is z1 > 1?
	bpl	adr_f0fxt
	postv	sv1			@ is z1 > 0?
	beq	adr_infxt
	b	adr_nanfxt
exptpn:	cmp	sv1, sv3		@ is z1 > 1?
	bpl	adr_infxt
	postv	sv1			@ is z1 >= 0?
	beq	adr_f0fxt
	b	adr_nanfxt

exptnt:	@ expt when z2 is an integer
	set	sv4, sv2
	postv	sv4
	beq	exptn0
	iabs	sv4, sv4
	set	sv2, sv1
	set	sv1, scheme_one
	bl	unidiv
exptn0:	
	save	sv1
	set	sv1, 5
exptn1:	@ loop
	eq	sv4, #i0
	itT	eq
	cdreq	dts, dts
	seteq	pc,  cnt
	sub	sv4, sv4, #4
	car	sv2, dts
	adr	lnk, exptn1
	b	uniprd


/*------------------------------------------------------------------------------
@ dump literal constants (up to 4K of code before and after this point)
@-----------------------------------------------------------------------------*/
.ltorg

	/* (make-rectangular x1 x2)   	~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"make-rectangular", mrc, 2, onumgto, null, _err, intmrc, fltmrc, intmrc, _err

	/* make-rectangular for integer and rational */
	PRIMIT	intmrc, ufun, 0
	bl	ir12fl			@ sv1 <- scheme float from scheme int or rational in sv1
	b	adr_fltmrc
	
	/* make-rectangular for float */
	PRIMIT	fltmrc, ufun, 0
	bl	uninum
	floatp	sv1
	bne	adr__err
	set	lnk, cnt
	b	makcpx

	/* (make-polar x3 x4)   	~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"make-polar", mpo, 2, onumgto, null, _err, intmpo, fltmpo, intmpo, _err
	@ in:	sv1 <- x3		(scheme float)
	@ in:	sv2 <- x4		(scheme float)
	@ out:	sv1 <- z = x3*e^(x4 i)	(scheme complex)

	/* make-polar for integer and rational */
	PRIMIT	intmpo, ufun, 0
	bl	ir12fl			@ sv1 <- scheme float from scheme int or rational in sv1
	@ continue to mpoflt
	b	adr_fltmpo

	/* make-polar for float */
	PRIMIT	fltmpo, ufun, 0
	bl	uninum
	floatp	sv1
	bne	adr__err
	save	sv2, sv1, cnt		@ dts <- (x4 x3 cnt ...)
	set	sv1, sv2		@ sv1 <- x4
	call	adr_fltsin		@ sv1 <- sin(x4)
	restor	sv2			@ sv2 <- x4
	save	sv1			@ dts <- (sin(x4) x3 cnt ...)
	set	sv1, sv2		@ sv1 <- x4
	call	adr_fltcos		@ sv1 <- cos(x4)
	restor	sv2			@ sv2 <- sin(x4),	dts <- (x3 cnt ...)
	bl	makcpx			@ sv1 <- cos(x4) + i sin(x4)
	set	sv2, sv1		@ sv2 <- cos(x4) + i sin(x4)
	restor	sv1, cnt		@ sv1 <- x3, cnt <- cnt, dts <- (...)
	set	lnk, cnt		@ lnk <- cnt
	b	uniprd

	/* (real-part z)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"real-part", rpt, 1, onumgto, null, _err, return, return, return, cpxrpt
	@ in:	sv1 <- z		(scheme complex)
	@ out:	sv1 <- real part of z	(scheme float)

	/* real-part for complex */
	PRIMIT	cpxrpt, ufun, 0
	real	sv1, sv1
	set	pc,  cnt

	/* (imag-part z)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"imag-part", img, 1, onumgto, null, _err, i0fxt, f0fxt, i0fxt, cpximg
	@ in:	sv1 <- z		(scheme complex)
	@ out:	sv1 <- imag part of z	(scheme float)

	/* imag-part for complex */
	PRIMIT	cpximg, ufun, 0
	imag	sv1, sv1
	set	pc,  cnt

	/* (magnitude z)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"magnitude", mgntd, 1, onumgto, null, _err, intabs, fltabs, ratabs, cpxmag
	@ in:	sv1 <- z		(scheme complex)
	@ out:	sv1 <- magnitude of z	(scheme float)

	/* abs and magnitude for integer */
	PRIMIT	intabs, ufun, 0
	iabs	sv1, sv1
	set	pc,  cnt
	
	/* abs and magnitude for float */
	PRIMIT	fltabs, ufun, 0
	fabs	sv1, sv1
	set	pc,  cnt

	/* abs and magnitude for rational */
	PRIMIT	ratabs, ufun, 0
	spltrat	sv1, sv2, sv1		@ sv1 <- numerator, sv2 <- denominator
	iabs	sv1, sv1
	set	lnk, cnt
	b	adr_intdiv

	/* magnitude for complex */
	PRIMIT	cpxmag, ufun, 0
	spltcpx	sv1, sv4, sv1		@ sv1 <- real part, sv4 <- imag part
	set	sv2, sv1		@ sv2 <- x
	bl	adr_fltprd		@ sv1 <- x^2
	set	sv5, sv1		@ sv5 <- x^2, saved
	set	sv1, sv4		@ sv1 <- y
	set	sv2, sv1		@ sv2 <- y
	bl	adr_fltprd		@ sv1 <- y^2
	set	sv2, sv5		@ sv2 <- x^2
	bl	adr_fltpls		@ sv1 <- magnitude(z)^2
	b	adr_fltsqr		@ sv1 <- magnitude(z), return via cnt

	/* (angle z)   			~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"angle", ang, 1, onumgto, null, nanfxt, intang, intang, ratang, cpxang
	@ in:	sv1 <- z		(scheme complex)
	@ out:	sv1 <- angle of z	(scheme float)

	/* angle for integer and float */
	PRIMIT	intang, ufun, 0
	postv	sv1
	itE	eq
	seteq	sv1, f0
	setne	sv1, scheme_pi
	set	pc,  cnt

	/* angle for rational */
	PRIMIT	ratang, ufun, 0
	numerat	sv1, sv1
	b	adr_intang

	/* angle for complex */
	PRIMIT	cpxang, ufun, 0
	spltcpx	sv2, sv1, sv1		@ sv2 <- real part, sv1 <- imag part
	b	adr_fltatn

/*------------------------------------------------------------------------------
@ dump literal constants (up to 4K of code before and after this point)
@-----------------------------------------------------------------------------*/
.ltorg

	/* (exact->inexact z)   	~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"exact->inexact", exact2inex, pfun, 1
	@ in:	sv1 <- z
	@ out:	sv1 <- inexact version of z
	@ mods:	sv1, rva
	set	sv2, f0			@ sv2 <- float zero
	set	lnk, cnt
	b	uninum
	
	/* (inexact->exact z)   	~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"inexact->exact", i2e, 1, onumgto, null, _err, return, flti2e, return, _err
	@ in:	sv1 <- z
	@ out:	sv1 <- exact version of z

	/* inexact->exact for flt */
	PRIMIT	flti2e, ufun, 0
	bl	flt2ndn			@ sv1 <- numerator (int), sv2 <- denominator (int)
	set	lnk, cnt
	b	adr_intdiv
	
/*------------------------------------------------------------------------------
@ dump literal constants (up to 4K of code before and after this point)
@-----------------------------------------------------------------------------*/
.ltorg

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.2.   Numbers
@  II.A.6.2.5  Numerical operations SUPPORT:	uninum, fltmte, mteflt
@-----------------------------------------------------------------------------*/

	/* exit with inf */
	PRIMIT	infxt, ufun, 0
	set	sv1, scheme_inf
	set	pc,  cnt		@ return

	/* exit with 0 */
	PRIMIT	i0fxt, ufun, 0
	set	sv1, i0
	set	pc,  cnt		@ return

	/* exit with 0.0 */
	PRIMIT	f0fxt, ufun, 0
	set	sv1, f0
	set	pc,  cnt		@ return

	/* exit with 1.0 */
	PRIMIT	f1fxt, ufun, 0
	set	sv1, scheme_one
	set	pc,  cnt		@ return

	/* exit with nan */
	PRIMIT	nanfxt, ufun, 0
	set	sv1, scheme_nan
	set	pc,  cnt
	
_func_
nanlxt:	@ exit with nan, via lnk
	set	sv1, scheme_nan
	set	pc,  lnk


	/* jump to function in address table based on object type (for paptbl:) */
	PRIMIT	numgto, ufun, 0
	set	rvc, sv5
_func_	
numjmp:	@ jump to function in address table based on object type
	@ in:	sv1 <-	object (hopefully a number)
	@ in:	rvc <-	table start address
	@ in:		offsets: 0, 4, 8, 12, 16 for nan, int, flt, rat, cpx
	ands	rva, sv1, #3
	it	ne
	eqne	rva, #3
	it	ne
	ldrne	pc,  [rvc, rva, lsl #2]
	@ rat/cpx or nan
	and	rvb, sv1, #0x07
	eq	rvb, #0x04
	itTT	eq
	ldrbeq	rvb, [sv1, #-4]
	andeq	rva, rvb, #0x07
	eqeq	rva, #3			@ rat/cpx?
	itTT	eq
	andeq	rva, rvb, #0x0C
	addeq	rva, rvc, rva, lsr #1
	ldreq	pc,  [rva, #12]
	ldr	pc,  [rvc]		@ error, pointer is not rat/cpx


	/* reduce the list of numbers in sv1 using pred in sv5 (oprts on sv1, sv2)
	   and dflt rslt in sv4 */
	PRIMIT	prdnml, ufun, 0
	sav__c				@ dts <- (cnt ...)
	call	adr_rdcnml		@ sv1 <- reduction-result
	restor	cnt			@ cnt <- cnt, dts <- (...)
	isnan	sv1			@ is reduction-result nan?
	it	ne
	eqne	sv1, #f			@	if not, is reduction-result = #f?
	b	adr_notfxt		@ return with #f/#t based on test result

	/* reduce the list of numbers in sv1 using operator in sv5
	   (oprts on sv1, sv2) and dflt rslt in sv4 */
	PRIMIT	rdcnml, ufun, 0
	@ in:	sv1 <- (arg1 ...)
	@ in:	sv4 <- default-result
	@ in:	sv5 <- operator
	@ out:	sv1 <- result
	nullp	sv1			@ are there no arguments?
	itT	ne
	cdrne	sv2, sv1		@	if not, sv2 <- (num2 ...)
	nullpne	sv2			@	if not, is there only one argument?
	itT	ne
	carne	sv4, sv1		@	if not, sv4 <- num1 = updated startup value
	setne	sv1, sv2		@	if not, sv1 <- (num2 ...) = updated list of numbers
	it	eq
	eqeq	sv4, #t			@	if so,  was starting value #t?
	itT	eq
	seteq	sv1, sv4		@		if so,  sv1 <- #t
	seteq	pc,  cnt		@		if so,  exit with #t
	set	sv2, sv1		@ sv2 <- () if 0 args, (num1) if 1 arg, (num2 .) if > 1 arg
	set	sv1, sv4		@ sv1 <- default result if <= 1 arg or num1 if > 1 arg
	set	sv4, sv2		@ sv4 <- () if 0 args, (num1) if 1 arg, (num2 .) if > 1 arg
	nmbrp	sv1
	bne	adr_nanfxt		@	if not, exit with nan
	adr	lnk, rdcncn		@ lnk <- return from operator
_func_	
rdcncn:	@ reduction loop
	@ in:	sv1 <- current/default result
	@ in:	sv4 <- list of numbers
	@ in:	sv5 <- jump table of function to apply to pairs of numbers
	@ preserves:	sv5    (function called needs to preserve sv4, sv5)
	nullp	sv4			@ is (num1 num2 ...) null?
	it	ne
	eqne	sv1, #f			@	if not, is sv1 = #f
	it	eq
	seteq	pc,  cnt		@	if so,  exit with result in sv1
	snoc	sv2, sv4, sv4		@ sv2 <- num1,	sv4 <- (num2 ...)
	b	unijmp

	/* build rational from scheme ints in sv1 (numer) and sv2 (denom) */
	PRIMIT	intdiv, ufun, 0
	@ in:	sv1 <- numerator   (scheme int)
	@ in:	sv2 <- denominator (scheme int)
	@ out:	sv1 <- rational, or int if sv2 = 1
	@ modifies:	sv1-sv3, rva-rvc
	@ check if denominator is zero
	izerop	sv2			@ is denominator = 0?
	bne	makra0			@	if not, jump to normal case
	@ denominator is zero:	0/0 or n/0
	izerop	sv1			@ is numerator = 0?
	beq	int2rat			@	if so,  jump to build rat
	postv	sv1			@ is numerator positive?
	set	sv1, i1			@ sv1 <- 1 (scheme int)
	it	ne
	ngintne	sv1, sv1		@	if not, sv1 <- -1 (scheme int)
	b	int2rat			@ jump to build rat
makra0:	@ non-zero denominator	
	@ check if denominator is negative:	m/-n  ->  -m/n
	postv	sv2			@ is denominator positive?
	it	ne
	ngintne	sv1, sv1		@	if not, sv1 <- minus numerator
	it	ne
	ngintne	sv2, sv2		@	if not, sv2 <- minus denominator
	@ check simple cases:	 0/n and n/1
	eq	sv1, #i0		@ is numerator = 0?
	it	ne
	eqne	sv2, #i1		@	if not, is denominator = 1?
	it	eq
	seteq	pc,  lnk		@	if either, return with numerator
	@ simplify the fraction
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv2, sv1, sv3
	bl	igcd			@ sv1 <- greatest common dvsr of nmrtr & dnmntr
	set	sv3, sv1		@ sv3 <- gcd, saved
	set	sv2, sv1		@ sv2 <- gcd
	restor	sv1			@ sv1 <- denominator
	bl	idivid			@ sv1 <- simplified denominator
	set	sv2, sv3		@ sv2 <- gcd
	set	sv3, sv1		@ sv3 <- simplified denominator
	restor	sv1			@ sv1 <- numerator
	bl	idivid			@ sv1 <- simplified numerator
	set	sv2, sv3		@ sv2 <- simplified denominator
	restor	sv3
	orr	lnk, sv3, #lnkbit0
	eq	sv2, #i1		@ is simplified fraction denominator = 1?
	it	eq
	seteq	pc,  lnk		@	if so,  return with simplified numerator
_func_
int2rat: @ int -> rat
	@ in:	sv1 <- integer
	@ in:	sv2 <- integer (1, as scheme int, when called by uninum)
	@ out:	sv1 <- rational
	@ modifies:	sv1, sv3, rva, rvb, rvc
	int2rat	sv1, sv1, sv2
	set	pc,  lnk	


.balign	4
_func_
makcpx:	@ build complex from scheme floats in sv1 (real) and sv2 (imag)
	@ in:	sv1 <- real part      (scheme float)
	@ in:	sv2 <- imaginary part (scheme float)
	@ out:	sv1 <- complex, or float if sv2 = 0.0
	eq	sv2, #f0
	it	eq
	seteq	pc,  lnk
_func_
flt2cpx: @ float -> cpx
	@ in:	sv1 <- float
	@ in:	sv2 <- float (0, as scheme float, when called by uninum)
	@ out:	sv1 <- complex
	@ modifies:	sv1, sv3, rva, rvb, rvc
	flt2cpx	sv1, sv1, sv2
	set	pc,  lnk	


_func_
igcd:	@ in:	sv1 <- scheme int
	@ in:	sv2 <- scheme int
	@ out:	sv1 <- gcd of sv1 and sv2 (scheme int)
	@ modifies:	sv1-sv3, rva-rvc
	bic	sv3, lnk, #lnkbit0	@ sv5 <- lnk, saved (and made even if Thumb2)
	save	sv3
	set	sv3, sv1
	adr	lnk, igcdl
_func_
igcdl:	@ igcd loop
	izerop	sv2
	itT	ne
	setne	sv1, sv3
	setne	sv3, sv2		@ sv3 <- int2 (saved against idivid -- will become int1)
	bne	idivid			@ sv1 <- quotient, sv2 <- remainder -- will become int2
	@ igcd exit
	iabs	sv1, sv3
	restor	sv3
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk
	

_func_
flt2ndn:	@ common entry for numerator and denominator
	@ used also in inexact->exact, rationalize
	@ in:	sv1 <- float
	@ out:	sv1 <- float's numerator   (scheme int)
	@ out:	sv2 <- float's denominator (scheme int)
	@ modifies:	sv1, sv2, sv3, rva, rvb, rvc
	@ check for 0.0, nan and inf
	eq	sv1, #f0
	itTT	eq
	seteq	sv1, i0
	seteq	sv2, i1
	seteq	pc,  lnk
	isnan	sv1
	itTT	eq
	seteq	sv1, i0
	seteq	sv2, i0
	seteq	pc,  lnk
	isinf	sv1
	bne	nmdne0
	postv	sv1			@ is number positive?
	set	sv1, i1			@ sv1 <- 1 (scheme int)
	it	ne
	ngintne	sv1, sv1		@	if not, sv1 <- -1 (scheme int)
	set	sv2, i0
	set	pc,  lnk
nmdne0:	@ normal cases	
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	bl	fltmte			@ sv1 <- mantissa, rva <- biased exponent
	cmp	rva, #156
	bmi	nmdne1
	eq	rva, #156
	itT	eq
	lsleq	rvb, sv1, #9
	eqeq	rvb, #0x0200
	bne	adr__err
	postv	sv1
	itTEE	eq
	mvneq	rvb, #0
	biceq	rvb, rvb, #0xE0000000
	setne	rvb, 0xE0000000
	orrne	rvb, rvb, #1
	raw2int	sv1, rvb
	set	sv2, i1
	b	nmdnxt
nmdne1:	@
	cmp	rva, #148
	bmi	nmdne2
	sub	rva, rva, #148
	bic	rvb, sv1, #3
	lsl	rvb, rvb, rva
	orr	sv1, rvb, #int_tag
	set	sv2, i1
	b	nmdnxt
nmdne2:	@
	cmp	rva, #127
	bmi	nmdne4
	rsb	rva, rva, #148
	set	rvb, 1
	lsl	rvb, rvb, rva
	raw2int	sv2, rvb
	save	sv2, sv1, sv3		@ dts <- (denom numer lnk ...)
	bl	igcd
	set	sv2, sv1
	restor	sv1			@ sv1 <- denom, dts <- (numer lnk ...)
	save	sv2			@ dts <- (gcd numer lnk ...)
	bl	idivid			@ sv1 <- denom/gcd
	set	sv3, sv1
	restor	sv2, sv1		@ sv2 <- gcd, sv1 <- numer, dts <- (lnk ...)
	save	sv3			@ dts <- (denom/gcd lnk ...)
	bl	idivid			@ sv1 <- numer/gcd
	restor	sv2, sv3		@ sv2 <- denom/gcd, sv3 <- lnk, dts <- (...)
	b	nmdnxt
nmdne4:	@
	bl	mteflt
	set	sv2, sv1
	set	sv1, scheme_one
	save	sv3
	bl	adr_fltdiv
	bl	flt2ndn
	swap	sv1, sv2, sv3
	restor	sv3
	postv	sv2
	it	ne
	ngintne	sv1, sv1
	it	ne
	ngintne	sv2, sv2
nmdnxt:	@ exit	
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk

_func_	
unipls:	@ uniformize numbers in sv1, sv2, then do: plus
	ldr	sv5, =tb_numpls
	b	unijmp

_func_	
unimns:	@ uniformize numbers in sv1, sv2, then do: minus
	ldr	sv5, =tb_nummns
	b	unijmp

_func_	
uniprd:	@ uniformize numbers in sv1, sv2, then do: prod
	ldr	sv5, =tb_numprd
	b	unijmp

_func_	
unidiv:	@ uniformize numbers in sv1, sv2, then do: divi
	ldr	sv5, =tb_numdiv
	b	unijmp

	/* pre-entry function for unijmp */
	PRIMIT	unijpe, ufun, 0
	b	unijmp

_func_	
uninum:	@ [entry]
	ldr	sv5, =unirtb
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, to avoid a jump
_func_
unijrt:	@ [return address]
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
_func_
unijmp:	@ [entry]
	@ makes sv1 (x1) and sv2 (x2) either both scheme ints or both scheme floats
	@ modifies:	sv1-sv3, rva-rvc
	ands	rva, sv1, #3
	beq	unij1p
	eq	rva, #3
	beq	unijer
	ands	rvb, sv2, #3
	beq	unij2p
	eq	rvb, #3
	beq	unijer
	@ x1 is int/flt, x2 is int/flt
	eq	rva, rvb
	it	eq
	ldreq	pc,  [sv5, rva, lsl #2]
	orr	rva, rva, #0x01
	lsl	rva, rva, #1
	b	unij4p
unij1p:	@ x1 is rat/cpx
	ldrb	rva, [sv1, #-4]
	and	rva, rva, #0x0f
	and	rvc, rva, #0x07
	eq	rvc, #3
	bne	unijer
	ands	rvb, sv2, #3
	bne	unij3p
	@ x1 is rat/cpx, x2 is rat/cpx
	ldrb	rvb, [sv2, #-4]
	and	rvb, rvb, #0x0f
	and	rvc, rvb, #0x07
	eq	rvc, #3
	bne	unijer
	eq	rva, rvb
	itTTT	eq
	andeq	rva, rva, #0x0c
	lsreq	rva, rva, #1
	addeq	rva, rva, #12
	ldreq	pc,  [sv5, rva]
	eor	rva, rva, #0x08
	lsr	rva, rva, #1
	b	unij4p
unij2p:	@ x1 is int/flt, x2 is rat/cpx
	ldrb	rvb, [sv2, #-4]
	and	rvb, rvb, #0x0f
	and	rvc, rvb, #0x07
	eq	rvc, #3
	bne	unijer
	add	rvc, rva, rvb, lsr #2
	sub	rva, rvc, #1
	b	unij4p
unij3p:	@ x1 is rat/cpx, x2 is int/flt
	eq	rvb, #3
	beq	unijer
	add	rvc, rvb, rva, lsr #2
	add	rva, rvc, #3
unij4p:	@ continue
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	ldr	lnk, =unijrt
	ldr	rvc, =unijtb
	ldr	pc,  [rvc, rva, lsl #2]

unijer:	@ non-number encountered
	ldr	pc,  [sv5]


	/* jump table for numeric type conversions */
	startBVU8 unijtb
	.word	i12rat, r22flt, i12flt, f12cpx
	.word	i22rat, r12flt, i22flt, f22cpx
	ENDsized
	
	/* return table (turns unijmp into uninum) */
	startBVU8 unirtb
	.word	adr__err
	.word	lnklnk
	.word	lnklnk
	.word	lnklnk
	.word	lnklnk
	ENDsized


_func_
lnklnk:	@ return via lnk	
	set	pc, lnk

.ifdef	hardware_FPU

_func_
ir12fl:	@ convert scheme int or rat in sv1 to a float (return via lnk)
	@ input:	sv1	scheme int or rational
	@ output:	sv1	scheme float
	@ modifies:	sv1, rva, rvb
	pntrp	sv1
	beq	r12flt
	@ continue to i12flt
	
_func_
i12flt:	@ convert scheme int in sv1 to a float
	@ input:	sv1	scheme int
	@ output:	sv1	scheme float
	@ modifies:	sv1, rva
	int2raw	rva, sv1
  .ifndef FPU_is_maverick
	vmov	s0, rva
	vcvt.f32.s32	s0,  s0
	vmov	rva, s0
  .else
	cfmv64lr mvdx0, rva
	cfcvt32s mvf0, mvfx0
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	orr	sv1, rva, #f0
	set	pc,  lnk
	
_func_
i22flt:	@ convert scheme int in sv2 to a float
	@ input:	sv2	scheme int
	@ output:	sv2	scheme float
	@ modifies:	sv2, rva
	int2raw	rva, sv2
  .ifndef FPU_is_maverick
	vmov	s0, rva
	vcvt.f32.s32	s0,  s0
	vmov	rva, s0
  .else
	cfmv64lr mvdx0, rva
	cfcvt32s mvf0, mvfx0
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	orr	sv2, rva, #f0
	set	pc,  lnk
	
_func_
r12flt: @ convert rational in sv1 to float
	@ in:	sv1 <- rational
	@ out:	sv1 <- float
	@ modifies:	sv1, rva, rvb

  .ifndef FPU_is_maverick

	rawsplt	rva, rvb, sv1
	lsr	rva, rva, #2
	bic	rva, rva, #3
	orr	rva, rva, rvb, lsl #30
	bic	rvb, rvb, #3
	vmov	s0, s1, rva, rvb
	vcvt.f32.s32	s0, s0
	vcvt.f32.s32	s1, s1
	vdiv.f32	s0, s0, s1	
	vmov	rva, s0	
	bic	rva, rva, #0x03
	orr	sv1, rva, #f0
	set	pc,  lnk

  .else		@ FPU_is_maverick

	@ maverick crunch has no division, use code from non_FPU version
	cfmv64lr mvdx0, rvc		@ use FPU reg to save rvc
	cfmv64lr mvdx1, lnk		@ use FPU reg to save lnk
	save	sv2
	bl	rat2fl
	restor	sv2
	cfmvr64l rvc, mvdx0
	cfmvr64l lnk, mvdx1
	set	pc,  lnk
	
_func_
rat2fl: @ helper function
	@ modifies:	sv1, sv2, rva, rvb, rvc
	rawsplt	rva, rvb, sv1
	lsr	rva, rva, #2
	bic	rva, rva, #3
	orr	rva, rva, rvb, lsl #30
	bic	rvb, rvb, #3
	orr	sv1, rva, #int_tag
	orr	sv2, rvb, #int_tag
	izerop	sv2
	beq	divzro
	set	rva, 148		@ rva <- biased exponent of dividand
	b	divnrm			@ sv1 <- sv1/sv2 as float, returns via lnk

  .endif	@ FPU_is_maverick
	
_func_
r22flt: @ convert rational in sv2 to float
	@ in:	sv2 <- rational
	@ out:	sv2 <- float
	@ modifies:	sv2, rva, rvb
	swap	sv1, sv2, rva
	bl	r12flt
	swap	sv1, sv2, rva
	b	unijrt

.else	@ no hardware FPU
	
_func_
ir12fl:	@ convert scheme int or rat in sv1 to a float (return via lnk)
	@ input:	sv1	scheme int or rational
	@ output:	sv1	scheme float
	@ modifies:	sv1, sv4, sv5, rva, rvb, rvc
	pntrp	sv1
	beq	r12fln
	@ continue to i12flt
	
_func_
i12flt:	@ convert scheme int in sv1 to a float
	@ input:	sv1	scheme int	signed 'in-place' mantissa
	@ output:	sv1	scheme float
	@ modifies:	sv1, rva, rvb, rvc
	set	rva, 148
	b	mteflt
	
_func_
i22flt:	@ convert scheme int in sv2 to a float
	@ input:	sv1	scheme int	signed 'in-place' mantissa
	@ output:	sv1	scheme float
	@ modifies:	sv1, rva, rvb, rvc
	swap	sv1, sv2, rvc
	set	rva, 148
	bl	mteflt
	swap	sv1, sv2, rvc
	b	unijrt
	
_func_
r12flt: @ convert rational in sv1 to float
	@ in:	sv1 <- rational
	@ out:	sv1 <- float
	@ modifies:	sv1, rva, rvb, rvc
	save	sv2
	bl	rat2fl
	restor	sv2
	b	unijrt
	
_func_
r12fln: @ convert rational in sv1 to float
	@ in:	sv1 <- rational
	@ out:	sv1 <- float
	@ modifies:	sv1, sv4, sv5, rva, rvb, rvc
	@ returns via:	lnk
	set	sv4, sv2
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved (and made even if Thumb2)
	bl	rat2fl
	orr	lnk, sv5, #lnkbit0	@ lnk <- lnk, restored
	set	sv2, sv4
	set	pc,  lnk
	
_func_
r22flt: @ convert rational in sv2 to float
	@ in:	sv2 <- rational
	@ out:	sv2 <- float
	@ modifies:	sv2, rva, rvb, rvc
	swap	sv1, sv2, rvc
	save	sv2
	bl	rat2fl
	restor	sv2
	swap	sv1, sv2, rvc
	b	unijrt

_func_
rat2fl: @ helper function
	@ modifies:	sv1, sv2, rva, rvb, rvc
	rawsplt	rva, rvb, sv1
	lsr	rva, rva, #2
	bic	rva, rva, #3
	orr	rva, rva, rvb, lsl #30
	bic	rvb, rvb, #3
	orr	sv1, rva, #int_tag
	orr	sv2, rvb, #int_tag
	izerop	sv2
	beq	divzro
	set	rva, 148		@ rva <- biased exponent of dividand
	b	divnrm			@ sv1 <- sv1/sv2 as float, returns via lnk

.endif	@ yes/no hardware FPU
		
_func_
i12rat:	@ convert scheme int in sv1 to a rational
	@ input:	sv1	scheme int
	@ output:	sv1	scheme rational
	@ modifies:	sv1, rva, rvb, rvc
	save	sv2, sv3
	set	sv2, i1			@ sv2 <- 1 as scheme int
	bl	int2rat
	restor	sv2, sv3
	b	unijrt
	
_func_
i22rat:	@ convert scheme int in sv2 to a rational
	@ input:	sv2	scheme int
	@ output:	sv2	scheme rational
	@ modifies:	sv1, rva, rvb, rvc
	save	sv1, sv3
	set	sv1, sv2
	set	sv2, i1			@ sv2 <- 1 as scheme int
	bl	int2rat
	set	sv2, sv1
	restor	sv1, sv3
	b	unijrt
	
_func_
f12cpx:	@ convert scheme float in sv1 to a complex
	@ input:	sv1	scheme int
	@ output:	sv1	scheme rational
	@ modifies:	sv1, rva, rvb, rvc
	save	sv2, sv3
	set	sv2, f0			@ sv2 <- 0 as scheme float
	bl	flt2cpx
	restor	sv2, sv3
	b	unijrt
	
_func_
f22cpx:	@ convert scheme float in sv2 to a complex
	@ input:	sv2	scheme int
	@ output:	sv2	scheme rational
	@ modifies:	sv1, rva, rvb, rvc
	save	sv1, sv3
	set	sv1, sv2
	set	sv2, f0			@ sv2 <- 0 as scheme float
	bl	flt2cpx
	set	sv2, sv1
	restor	sv1, sv3
	b	unijrt

_func_
fltmte:	@ convert float to 'in-place' mantissa and biased exponent
	@ in:	sv1 <- number					(scheme float)
	@ out:	sv1 <- signed 'in-place' mantissa of sv1	(scheme int)
	@ out:	rva <- biased exponent of sv1			(raw int)
	@ modifies:	sv1, rva
	fltmte	rva, sv1
	set	pc,  lnk		@ return

.ifdef	hardware_FPU

_func_
mteflt:	@ convert 'in-place' mantissa and biased exponent to float
	@ input:	sv1	scheme int	signed 'in-place' mantissa
	@ input:	rva	raw int		biased exponent
	@ output:	sv1	scheme float
	@ modifies:	sv1, rva, rvb
	int2raw	rvb, sv1
  .ifndef FPU_is_maverick
	vmov	s0,  rvb
	vcvt.f32.s32	s0,  s0
	sub	rva, rva, #21
	lsl	rva, rva, #23
	vmov	s1,  rva
	vmul.f32	s0,  s0,  s1
	vmov	rva, s0
  .else
	cfmv64lr mvdx0, rvb
	cfcvt32s mvf0, mvfx0
	sub	rva, rva, #21
	lsl	rva, rva, #23
	cfmvsr	mvf1, rva
	cfmuls	mvf0, mvf0, mvf1
	cfmvrs	rva, mvf0
  .endif
	bic	rva, rva, #0x03
	orr	sv1, rva, #f0
	set	pc,  lnk
	
.else	@ no hardware FPU
		
_func_
mteflt:	@ convert 'in-place' mantissa and biased exponent to float
	@ input:	sv1	scheme int	signed 'in-place' mantissa
	@ input:	rva	raw int		biased exponent
	@ output:	sv1	scheme float
	@ modifies:	sv1, rva, rvb, rvc
	eq	sv1, #i0		@ is number zero ?
	beq	mtefzr
	set	rvc, f0			@ sv4 <- 0.0, initial result (scheme float)
	cmp	sv1, #0			@ is mantissa negative?
	itT	mi
	ngfltmi	rvc, rvc		@	if so,  sv4 <- -0.0, initial result (scheme float)
	ngintmi	sv1, sv1		@	if so,  sv1 <- mantissa (positive)
	set	rvb, rva		@ rvb <- biased exponent (saved)
.ifndef	cortex
	set	rva, 31			@ rva <- 31 (position of first potential msb)
fltmsb:	tst	sv1, sv1, LSR rva	@ is bit rva a 1 (tested against int_tag) ?
	itT	eq
	subeq	rva, rva, #1		@	if not, rva <- (rva - 1) = next possible msb
	beq	fltmsb			@	if not, jump to test that bit
.else
	clz	rva, sv1		@ rva <- number of leading zeroes in sv1
	rsb	rva, rva, #31		@ rva <- msb of sv1
.endif
	sub	rva, rva, #23		@ rva <- how much to shift mantissa to the right
	add	rvb, rvb, rva		@ rvb <- updated biased exponent
	cmp	rvb, #255		@ is updated biased exponent >= 255 ?
	bpl	mtefnf
	cmp	rvb, #1			@ is updated biased exponent less than 1 ?
	bmi	mtefdn			@	if so,  jump to prepare for denormalized number
mtefr0:	@ continue (in normal or denormalized case)
	orr	rvc, rvc, rvb, LSL #23	@ sv4 <- result sign + exponent + float tag
.ifndef	cortex
	cmp	rva, #0			@ are we shifting mantissa to the right?
	itTTT	pl
	lsrpl	rva, sv1, rva		@	if so,  rva <- shifted unsigned mantissa
	addpl	rvb, rva, #2		@	if so,  rvb <- shftd usgn mnts rndd up (>=0.5->1)
	lsrpl	rva, rvb, #24		@	if so,  rva <- 1 if rndng incrsd pwr of 2 of res
	lsrpl	rvb, rvb, rva		@	if so,  rvb <- mnt shftd if rndng incrsd pow of 2
	itT	pl
	addpl	rvc, rvc, rva, LSL #23	@	if so,  sv4 <- bsd exp incrsd if rndng incrsd pow
	bicpl	rvb, rvb, #3		@	if so,  rvb <- shifted unsigned mantissa wo/tag
	itTT	mi
	rsbmi	rva, rva, #0		@	if not, rva <- how much to shift mntss left
	bicmi	rvb, sv1, #3		@	if not, rvb <- unsigned mantissa without tag
	lslmi	rvb, rvb, rva		@	if not, rvb <- shifted unsgnd mantissa wo/tag
.else
	cmp	rva, #0			@ are we shifting mantissa to the right?
	bmi	mtefl4
	@ shifting to the right
	set	rvb, rva
	set	rva, sv1
mtefl0:	eq	rvb, #0
	itT	ne	
	lsrne	rva, rva, #1		@	if so,  rva <- shifted unsigned mantissa
	subne	rvb, rvb, #1
	bne	mtefl0
	add	rvb, rva, #2		@ rvb <- shifted unsgnd mantissa rounded up (>= 0.5 -> 1)
	lsr	rva, rvb, #24		@ rva <- 1 if rounding increased power of 2 of result
	add	rvc, rvc, rva, LSL #23	@ sv4 <- biased expon. increased if rndng incrsd pwr of 2
mtefl1:	eq	rva, #0
	itT	ne
	lsrne	rvb, rvb, #1		@	if so,  rvb <- mnt shftd if rndng incrsd pwr of 2
	subne	rva, rva, #1
	bne	mtefl1
	bic	rvb, rvb, #3		@ rvb <- shifted unsigned mantissa without tag
	b	mtefl6
mtefl4:	@ shifting to the left
	rsb	rva, rva, #0		@ rva <- how much to shift mantissa to the left
	bic	rvb, sv1, #3		@ rvb <- unsigned mantissa without tag
mtefl5:	eq	rva, #0
	itT	ne
	lslne	rvb, rvb, #1		@ rvb <- shifted unsigned mantissa without tag
	subne	rva, rva, #1
	bne	mtefl5
.endif
mtefl6:	@ finish up
	set	rva, 0xff		@ rva <- exponent mask
	tst	rvc, rva, lsl #23	@ is exp 0 (dnrmlzd) ? [excld dnrm (poss rnd up) frm 1 clr]
	it	ne
	bicne	rvb, rvb, #0x00800000	@	if not, rvb <- shftd unsgnd mantissa wo/tag and 1.
	orr	sv1, rvc, rvb		@ sv1 <- float
	set	pc,  lnk		@ return
mtefzr:	@ return zero
	set	sv1, f0			@ sv1 <- 0.0
	set	pc,  lnk		@ exit with 0.0	
mtefnf:	@ return inf
	set	rvb, 255		@ rvb <- 255
	orr	sv1, rvc, rvb, LSL #23	@ sv1 <- +/- inf
	set	pc,  lnk		@ return with +/- inf
mtefdn:	@ prepare for denormalized number
	add	rva, rva, #1		@ rva <- required right shift + 1 (for denormalization)
	sub	rva, rva, rvb		@ rva <- right shift needed to get exponent of zero
	set	rvb, 0			@ rvb <- zero (denormalized exponent)
	b	mtefr0

.endif	@ yes/no hardware FPU
		
	
.ifndef	cortex	@ integer division on ARMv4T (arm7tdmi, arm920t) and ARMv7A (cortex-a8)

idivid:	@ integer division:
	@ in:	sv1 <- dividand		(scheme int)
	@ in:	sv2 <- divisor		(scheme int)
	@ out:	sv1 <- quotient		(scheme int)
	@ out:	sv2 <- remainder	(scheme int)
	@ modifies:	sv1, sv2, rva, rvb, rvc
	set	rvc, 0
	postv	sv1
	itT	ne
	eorne	rvc, rvc, #0x14
	ngintne	sv1, sv1
	postv	sv2
	itT	ne
	eorne	rvc, rvc, #0x18
	ngintne	sv2, sv2	
	int2raw	rva, sv1		@ rva <- int1 (raw int)
	int2raw	rvb, sv2		@ rvb <- int2 (raw int)
pdivsh:	lsl	rvb, rvb, #1		@ rvb <- divisor, shifted leftwards as necessary
	cmp	rva, rvb		@ is dividand >= divisor ?
	bpl	pdivsh			@	if so, jump to keep shifting divisor, leftward
	lsr	rvb, rvb, #1		@ rvb <- shifted divisor/2
	raw2int	sv1, rvb		@ sv1 <- divisor as scheme integer
	set	rvb, rva		@ rvb <- dividand (will be remainder)
	set	rva, 0			@ rva <- 0 (initial quotient, raw int)
pdivcn:	lsl	rva, rva, #1		@ rva <- updated quotient
	cmp	rvb, sv1, LSR #2	@ is dividand >= divisor?
	itT	pl
	subpl	rvb, rvb, sv1, LSR #2	@	if so,  rvb <- dividand - divisor
	addpl	rva, rva, #1		@	if so,  rva <- quotient + 1
	eq	sv1, sv2		@ is divisor = original divisor (i.e. done)?
	itT	ne
	addne	sv1, sv1, #1		@	if not, sv1 <- divisor as pseudo float
	lsrne	sv1, sv1, #1		@	if not, sv1 <- divisor shftd right by 1 (sch int)
	bne	pdivcn			@	if not, jump to continue dividing
	raw2int	sv1, rva		@ sv1 <- quotient (scheme int)
	raw2int	sv2, rvb		@ sv2 <- remainder (scheme int)
	tst	rvc, #0x04		@ should remainder be positive?
	it	ne
	ngintne	sv2, sv2		@	if not, sv2 <- remainder (negative)
	tst	rvc, #0x10		@ should quotient be positive?
	it	ne
	ngintne	sv1, sv1		@	if not, rva <- quotient (negative)
	set	pc,  lnk		@ return

.else	@ integer division on ARMv7M (cortex-m3 -- not available of ARMv7A, cortex-a8)

_func_
idivid:	@ integer division:	 rva{quot} rvb{rem} <- rva / rvb 
	@ modifies:	sv1, sv2, rva, rvb
	@ rva and rvb must be from 30-bit scheme ints (i.e. 31 or 32 bit ints don't work)
	int2raw	rva, sv1		@ rva <- int1 (raw int)
	int2raw	rvb, sv2		@ rvb <- int2 (raw int)
	sdiv	rva, rva, rvb
	mul	rvb, rvb, rva
	rsb	rvb, rvb, sv1, ASR #2
	raw2int	sv1, rva		@ sv1 <- quotient (scheme int)
	raw2int	sv2, rvb		@ sv2 <- remainder (scheme int)
	set	pc,  lnk
	
.endif	@ ifndef cortex

	
.ifdef	hardware_FPU

_func_
itrunc:	@ truncate number in sv1
	@ modifies:	sv1, rva
	@ note:		may need to check for "overflow" (number too large)
	bic	rva, sv1, #0x80000000	@ rva <- number, without sign
	cmp	rva, #0x59000000	@ does number have exponent >= 178 (no integer equivalent)?
	bpl	adr__err		@	if so,  itrunc error
	bic	rva, sv1, #0x03
  .ifndef FPU_is_maverick
	vmov	s0, rva
	vcvt.s32.f32	s0, s0
	vmov	rva, s0
  .else
	cfmvsr	mvf0, rva
	cfcvts32 mvfx0, mvf0
	cfmvr64l rva, mvdx0
  .endif
	raw2int	sv1, rva
	set	pc,  lnk

.else	@ no hardware FPU

_func_
itrunc:	@ truncate number in sv1
	@ modifies:	sv1, rva, rvb
	set	rvb, lnk
	bl	fltmte			@ sv1 <- mantissa,  rva <- exponent
	set	lnk, rvb
	cmp	rva, #148
	bpl	itrun2
	rsb	rvb, rva, #148		@ rvb <- right shft needed to get int part of num (raw int)
	asr	rva, sv1, #2		@ rva <- mantissa (raw)
	cmp	rva, #0			@ is number negative?
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- positive number
	asr	rva, rva, rvb		@ rva <- number, shifted to integer
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- integer, restored to proper sign
	raw2int	sv1, rva
	set	pc,  lnk
itrun2:	@ left shift
	sub	rvb, rva, #148
	cmp	rvb, #30
	bpl	itrerr
	asr	rva, sv1, #2		@ rva <- mantissa (raw)
	cmp	rva, #0			@ is number negative?
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- positive number
	lsl	rva, rva, rvb		@ rva <- number, shifted to integer
	it	mi
	rsbmi	rva, rva, #0		@	if so,  rva <- integer, restored to proper sign
	asr	rvb, rva, rvb
	lsl	rvb, rvb, #2
	orr	rvb, rvb, #int_tag
	eq	rvb, sv1
	it	ne
	asrne	rva, sv1, #2
	bne	itrerr
	raw2int	sv1, rva
	set	pc,  lnk
itrerr:	@ itrunc error
	raw2int	sv1, rva
	b	adr__err
	
.endif	@ yes/no hardware FPU


/*------------------------------------------------------------------------------
@ dump literal constants (up to 4K of code before and after this point)
@-----------------------------------------------------------------------------*/
.ltorg

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.2.   Numbers
@  II.A.6.2.6. Numerical input and output CORE:	number->string, string->number
@-----------------------------------------------------------------------------*/

	/* (number->string number <radix>)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"number->string", numstr, pfun, 2
	@ in:	sv1 <- number, sv2 <- <radix>
	@ out:	sv1 <- string
	pntrp	sv1			@ is number a rational or complex?
	beq	numsts			@	if so,  jump to process that case
numstn:	@ normal conversion
	set	sv5, sv1		@ sv5 <- number (saved against malloc, pcons, pshdat)
	set	sv3, 0			@ sv3 <- 0
	nullp	sv2			@ no radix given?
	it	eq
	seteq	sv2, 0x29		@	if so, sv2 <- radix=10 (scheme int)
	set	sv1, 0x31		@ sv4 <- max of 12 digits (aligned) for dec int/float (+/-)
	eq	sv2, #0x29		@ is radix 10?
	itTTT	ne
	setne	sv1, 0x81		@	if not, sv1 <- 32 digs+4 byt hdr for bin (sch int)
	setne	sv3, 0x05		@	if not, sv3 <- shift = 1 (scheme int)
	setne	sv4, 0x05		@	if not, sv4 <- 1-bit mask (scheme int)
	eqne	sv2, #0x09		@	if not, is radix 2?
	itTTT	ne
	setne	sv1, 0x31		@	if not, sv1 <- 12 digs+4 byt hdr for oct (sch int)
	setne	sv3, 0x0D		@	if not, sv3 <- shift = 3 (scheme int)
	setne	sv4, 0x1D		@	if not, sv4 <- 3-bit mask (scheme int)
	eqne	sv2, #0x21		@	if not, is radix 8?
	itTTT	ne
	setne	sv1, 0x21		@	if not, sv1 <- 8 digs + 4 byt hdr for hex (sch int)
	setne	sv3, 0x11		@	if not, sv3 <- shift = 4 (scheme int)
	setne	sv4, 0x3D		@	if not, sv4 <- 4-bit mask (scheme int)
	eqne	sv2, #0x41		@	if not, is radix 16?
	set	sv2, sv1		@ sv2 <- size of string to allocate
	straloc	sv1, sv2		@ sv1 <- address of digits-string
	int2raw	rva, sv5		@ rva <- number (eg. raw int)
	eq	sv3, #0			@ is radix decimal?
	beq	numst1			@	if so,  jump to decimal conversion
	@ scheme 32-bit item -> binary, octal or hexadecimal digits, as string
	strlen	sv5, sv1		@ sv5 <- number of digits (scheme int)
	sub	sv5, sv5, #0x04		@ sv5 <- offset to last digit (scheme int)
numst0:	and	rvb, rva, sv4, LSR #2	@ rvb <- masked bits
	cmp	rvb, #0x0A		@ are they above 9?
	itE	pl
	addpl	rvb, rvb, #0x37		@	if so,  rvb <- raw ASCII char A to F
	addmi	rvb, rvb, #0x30		@	if not, rvb <- raw ASCII char 0 to 9
	bytset	sv1, sv5, rvb		@ store raw ASCII char into digits string
	eq	sv5, #i0		@ are we done?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	lsr	rvb, sv3, #2		@ rvb <- shift as raw int
	lsr	rva, rva, rvb		@ rva <- raw number, shifted
	sub	sv5, sv5, #4		@ sv4 <- offset of previous digit
	b	numst0			@ jump to continue conversion

numst1:	@ convert to base 10 string (integer or float)
	cmp	rva, #0			@ see if number is negative
	itE	pl
	setpl	rvb, ' 			@	if not, rvb <- ascii space
	setmi	rvb, '-			@	if so,  rvb <- ascii - (minus)
	tst	sv5, #int_tag		@ is number an integer?
	set	sv5, i0			@ sv5 <- offset to sign as scheme int
	bytset	sv1, sv5, rvb		@ store sign into digits array
	beq	numst4			@	if not, jump to float conversion
	@ integer to base 10 string conversion -- extract digits
	set	sv5, 0x31		@ sv5 <- offset to (after) 12th digit as scheme int
	cmp	rva, #0			@ see if number is negative
	it	mi
	rsbmi	rva, rva, #0		@	if so,  make number positive
numst2:	@
	bl	dg2asc
	sub	sv5, sv5, #4		@ sv5 <- offset to digit
	bytset	sv1, sv5, rvb		@ store digit into digits array
	eq	rva, #0			@ is quotient zero?
	bne	numst2			@	if not, jump to keep extracting digits
	set	sv2, i0			@ sv2 <- offset to 1st digit or sign as scheme int
	bytref	rva, sv1, sv2		@ rva <-  sign digit
	eq	rva, #0x2D		@ is sign "-" ?
	it	eq
	addeq	sv2, sv2, #4		@	if so,  adjust offset to 1st digit
	rsb	rva, sv5, #0x31		@ rva <- number of digits * 4
	orr	sv3, rva, #0x01		@ sv3 <- number of digits as scheme int
	it	eq
	addeq	sv3, sv3, #4		@	adjust number of chars if number is negative
	add	sv3, rva, sv2		@ sv3 <- offset to position of last digit
	set	sv4, sv1		@ sv4 <- char source = destination
numst3:	@ integer to base 10 string conversion -- move digits to begining of string
	eq	sv2, sv3		@ are we done?
	beq	numstx
	bytref	rva, sv4, sv5		@ rva <- next digit
	bytset	sv1, sv2, rva		@ store digit sequentially back into array
	add	sv2, sv2, #4		@ sv2 <- next destination offset
	add	sv5, sv5, #4		@ sv5 <- next source offset
	b	numst3
numstx:	@ finish up
	lsl	rva, sv3, #6
	orr	rva, rva, #string_tag
	str	rva, [sv1, #-4]		@ store number of digits in array
	set	pc,  cnt

numst4:	@ scheme float -> digits that make up float, as symbol
	@ mantissa (21 or even 23 bits) has max of 7 digits + sign
	@ exponent has max of 2 digits + sign
	@ total is 12 digits, including e or E
	lsl	rva, rva, #2		@ rva <- float as raw value
	cmp	rva, #0			@ see if float is negative
	it	mi
	addmi	sv5, sv5, #4		@	if negat, update strg ofst to account for "-" sign
	@ begin extracting number, watch for +/-0.0, +/-inf, +/-nan
	fabs	rva, rva		@ rva <- raw float, cleared sign
	eq	rva, #0			@ is float zero?
	itT	eq
	ldreq	sv4, =zerof_
	beq	nflstQ
	lsl	rva, rva, #1		@ rva <- exponent and mantissa, shifted left
	and	rvb, rva, #0xFF000000	@ rvb <- biased binary exponent, shifted all the way left
	eq	rvb, #0xFF000000	@ is exponent 255? => nan or inf
	bne	nflstA			@	if not, jump to process regular number
	eq	rva, #0xFF000000	@ is exponent 255 and mantissa zero? (i.e. Inf)
	itE	eq
	ldreq	sv4, =inf___		@	if so,  sv4 <- inf as symbol characters
	ldrne	sv4, =nan___		@	if not, sv4 <- nan as symbol characters
nflstQ:	set	sv2, sv5
	set	sv5, i0
	strlen	rva, sv4
	bic	rva, rva, #3
	add	sv3, sv2, rva
	b	numst3

nflstA:	lsr	rvb, rvb, #24		@ rvb <- biased binary exponent
	bic	rva, rva, #0xFF000000	@ rva <- mantissa (pre-shifted left by 1)
	orr	rva, rva, #0x01000000	@ rva <- mantissa with 1.x
	lsl	rva, rva, #7		@ rva <- full mantissa shifted all the way left
	eq	rvb, #0			@ is biased binary exponent zero?
	itEE	ne
	subne	rvc, rvb, #127		@	if not, rvb <- unbiased exponent
	subeq	rvc, rvb, #126		@	if so,  rvb <- unbiased exponent (denormalized)
	biceq	rva, rva, #0x80000000	@	if so,  rva <- mantissa 0.x (denormalized)
	@ convert exponent to decimal
	set	sv2, int_tag		@ sv2 <- decimal exponent (scheme int)
	cmp	rvc, #30		@ is bin exp >= 30?
	bpl	nflstp			@	if so,  jump to process exponent greater than 31
nflstn:	bl	lftshf			@ rva <- rva shifted left, rvb <- # shift steps
	rsb	rvc, rvb, rvc		@ rvb <- updated binary exponent (raw)
	lsr	rva, rva, #4		@ rva <- rva / 16
	set	rvb, 4
	add	rvc, rvb, rvc		@ rvb <- updated binary exponent (raw)
	lsl	rvb, rva, #3
	add	rva, rvb, rva, LSL #1	@ rva <- 10*rva
	sub	sv2, sv2, #4
	cmp	rvc, #30		@ is bin exp >= 30?
	bmi	nflstn			@	if not, keep going
nflstp:	bl	idiv10			@ rva <- rva/10  (number/10) (rvb used)
	add	sv2, sv2, #4
	bl	lftshf			@ rva <- rva shifted left, rvb <- # shift steps
	rsb	rvc, rvb, rvc		@ rvb <- updated binary exponent (raw)
	cmp	rvc, #30		@ is bin exp >= 30?
	bpl	nflstp
	rsb	rvb, rvc, #31
	lsr	rva, rva, rvb		@ rva <- number (with no extraneous bin exp)
	@ rva = number (unsigned, sign is in sv1 already), sv2 = dec expon (sch int, signed)
	set	sv4, sv2		@ sv4 <- decimal exponent
	set	sv5, 0x31		@ sv5 <- offset to (after) 12th digit as scheme int
nflst0:	@
	bl	dg2asc			@ rvb <- remainder as digit
	sub	sv5, sv5, #4		@ sv5 <- offset to digit
	bytset	sv1, sv5, rvb		@ store digit into digits array
	eq	rva, #0			@ is quotient zero?
	bne	nflst0
	set	sv2, i0			@ sv2 <- offset to 1st digit or sign as scheme int
	bytref	rva, sv1, sv2		@ rva <-  sign digit
	eq	rva, #0x2D		@ is sign "-" ?
	it	eq
	addeq	sv2, sv2, #4		@	if so, adjust offset to 1st digit
	rsb	rva, sv5, #0x31		@ rva <- number of digits * 4
	@ update exponent for future position of dot
	lsr	rvb, rva, #2		@ rvb <- number of digits, raw int
	sub	rvb, rvb, #1		@ rvb <- how much to increase expon by (for upcoming dot)
	add	rvb, rvb, sv4, ASR #2	@ rvb <- updated decimal exponent (raw int)
	raw2int	sv4, rvb
	@ shift digits back to start of array then round number to 6 digits, if necessary
	cmp	rva, #28
	itTE	pl
	setpl	rva, 28			@	rva <- maximum number of digits to keep: 7 (* 4)
	setpl	rvb, true		@	rvb <- true that we need to round result
	setmi	rvb, false		@	rvb <- no, we don't need to round
	add	sv3, rva, sv2		@ sv3 <- offset to position of last digit + 1
nflst1:	eq	sv2, sv3		@ was that the last digit?
	itT	eq
	subeq	sv2, sv2, #4		@	if so,  sv2 <- offset to position of last digit
	subeq	sv3, sv3, #4		@	if so,  sv3 <- offset to position of last digit
	beq	nflst2			@	if so,  jump to continue
	bytref	rva, sv1, sv5		@ rva <- next digit
	bytset	sv1, sv2, rva		@ store digit sequentially back into array
	add	sv2, sv2, #4		@ sv2 <- next destination offset
	add	sv5, sv5, #4		@ sv5 <- next source offset
	b	nflst1
nflst2:	eq	rvb, #t			@ should number be rounded?
	bne	nflst3			@	if not, jump to continue
	cmp	rva, #0x35		@ is last digit >= 5?
	it	mi
	submi	sv3, sv3, #4		@	if not, eliminate it (adj ofst to end of digits)
	bmi	nflst3			@	if not, jump to continue
	@ round number to 6 digits (if necessary)
nflst4:	eq	sv2, #i0		@ were we at last digit?
	itT	ne
	subne	sv2, sv2, #4		@	if not, sv2 <- offset of previous digit
	bytrefne rvb, sv1, sv2		@ 	if not, rvb <- digit
	it	ne
	eqne	rvb, #0x2D		@ 	if not, is this digit a minus sign?
	it	eq
	seteq	sv2, sv3		@	if so,  sv2 <- ofst of last dig (prep to splice 1)
	beq	nflst5			@	if so,  jump to add a leading 1
	eq	rvb, #0x39		@ is previous digit a 9?
	itE	eq
	seteq	rvb, 0x30		@	rvb <- (if so) make previous digit zero
	addne	rvb, rvb, #1		@	rvb <- (if not) add 1 to previous digit
	bytset	sv1, sv2, rvb		@ rvb <- updated previous digit
	beq	nflst4			@	if carry-over, redo loop with previous digit
	sub	sv3, sv3, #4		@ eliminate last digit
	b	nflst3			@ jump to continue
nflst5:	@ splice a 1 before the other digits (due to carry-over), update exponent
	eq	sv2, #i0		@ were we at first digit?
	itT	ne
	subne	sv2, sv2, #4		@	if not, sv2 <- offset of previous digit
	bytrefne rvb, sv1, sv2		@ 	if not, rvb <- previous digit
	itT	ne
	addne	sv2, sv2, #4		@	if not, sv2 <- offset, restored
	eqne	rvb, #0x2D		@ 	if not, is previous digit a minus sign?
	it	eq
	seteq	rvb, 0x31		@	if so,  rvb <- (if time to splice) ASCII 1
	bytset	sv1, sv2, rvb		@ store digit there
	it	ne
	subne	sv2, sv2, #4		@	if not, sv2 <- offset of previous digit
	bne	nflst5			@	if not, jump to keep scanning
	asr	rvb, sv4, #2		@ rvb <- exponent (raw int)
	add	rvb, rvb, #1		@ rvb <- exponent increased by 1 (raw int)
	raw2int	sv4, rvb
nflst3:	@ put decimal dot in
	add	sv3, sv3, #4		@ sv3 <- offset to new end of digits (with dot)
	set	sv2, sv3		@ sv2 <- offset to new end of digits (with dot)
nflst6:	eq	sv2, #i0		@ were we at first digit?
	itT	ne
	subne	sv2, sv2, #4		@	if not, sv2 <- offset of previous digit
	bytrefne rvb, sv1, sv2		@ 	if not, rvb <- previous digit
	itT	ne
	addne	sv2, sv2, #4		@	if not, sv2 <- offset, restored
	eqne	rvb, #0x2D		@ 	if not, is previous digit a minus sign?
	itT	eq
	addeq	sv2, sv2, #4		@	if so,  sv2 <- offset to where dot goes
	seteq	rvb, 0x2E		@	if so,  rvb <- (if time to splice) ASCII dot
	bytset	sv1, sv2, rvb		@ store dot there
	it	ne
	subne	sv2, sv2, #4		@	if not, sv2 <- offset of previous digit
	bne	nflst6			@	if not, jump to keep scanning
	@ erase trailing zeros, and maybe the dot too
nflst7:	bytref	rvb, sv1, sv3		@ rvb <-  last digit
	eq	rvb, #0x30		@ is it a zero?
	it	eq
	subeq	sv3, sv3, #4		@	if so,  delete it
	beq	nflst7			@	if so,  branch back to next digit
	eq	sv4, #i0		@ is exponent zero?
	beq	nflsxt			@	if so, exit
	set	rvb, 0x65		@ rvb <- ASCII e
	add	sv3, sv3, #4		@ sv3 <- updated offset to last digit
	bytset	sv1, sv3, rvb		@ store e there
	int2raw	rva, sv4
	cmp	rva, #0			@ is exponent positive?
	itTTT	mi
	rsbmi	rva, rva, #0		@	if not, make it positive
	setmi	rvb, 0x2D		@	if not, rvb <- ASCII minus
	addmi	sv3, sv3, #4		@	if not, sv3 <- updated offset to last digit
	bytsetmi sv1, sv3, rvb		@	if not, store minus there
	add	sv3, sv3, #4		@ sv3 <- offset to where to store exponent digits
	bl	dg2asc			@ rvb <- remainder as digit
	eq	rva, #0			@ is quotient zero?
	it	ne
	addne	sv3, sv3, #4		@	if not, sv3 <- position for exponent
	bytset	sv1, sv3, rvb		@ store exponent there
	beq	nflsxt			@	if so, (if quotient is zero) exit
	bl	dg2asc			@ rvb <- remainder as digit
	sub	sv5, sv3, #4		@ sv5 <- position of where to put second digit of exponent
	bytset	sv1, sv5, rvb		@ store exponent there
nflsxt:	add	sv3, sv3, #4		@ sv3 <- number of digits (scheme int)
	set	rva, string_tag
	orr	sv3, rva, sv3, LSL #6
	str	sv3, [sv1, #-4]		@ store number of digits in array
	set	pc,  cnt

dg2asc:	@ digit to ascii
	set	rvc, lnk
	raw2int	sv2, rva
	bl	idiv10			@ rva <- rva / 10 = quotient
	lsl	rvb, rva, #1		@ rvb <- quotient * 2
	add	rvb, rvb, rva, LSL #3	@ rvb <- quotient * 10
	rsb	rvb, rvb, sv2, LSR #2	@ rvb <- orig num mns 10*quotient = rmndr (rightmost dig)
	add	rvb, rvb, #0x30		@ rvb <- remainder as ascii digit
	set	pc,  rvc

numsts:	@ convert rational or complex to string
	sav_rc	sv1
	rawsplt	rva, rvc, sv1
	and	rvb, rva, #0x0f
	lsr	rva, rva, #2
	bic	rva, rva, #3
	orr	rva, rva, rvc, lsl #30
	eq	rvb, #rational_tag
	itE	eq
	orreq	sv1, rva, #int_tag
	orrne	sv1, rva, #float_tag
	set	sv2, null
	call	numstn			@ sv1 <- numerator/real-part-string
	car	sv2, dts		@ sv2 <- number
	save	sv1			@ dts <- (numerator/real-part-string number cnt ...)
	rawsplt	rva, rvc, sv2
	bic	rvc, rvc, #3
	tst	rva, #0x08		@ is this a rational?
	itE	eq
	orreq	sv1, rvc, #int_tag
	orrne	sv1, rvc, #float_tag
	set	sv2, null
	call	numstn			@ sv1 <- denominator/imaginary-part-string
	restor	sv2, sv3, cnt		@ sv2 <- nmrtr/real-part-str,sv3<-num,cnt<-cnt,dts<-(.)
	strlen	rva, sv1
	strlen	rvb, sv2
	add	rvc, rva, rvb
	add	sv4, rvc, #3
	rawsplt	rva, rvb, sv3
	eor	rvb, rvb, #0x80000000
	tst	rva, #0x08
	itT	ne
	tstne	rvb, #0x80000000
	addne	sv4, sv4, #4
	straloc	sv5, sv4
	set	sv4, i0
	strlen	rvb, sv2
numrc0:	bytref	rva, sv2, sv4
	bytset	sv5, sv4, rva
	add	sv4, sv4, #4
	eq	sv4, rvb
	bne	numrc0
	ldr	rvc, [sv3, #-4]
	ldr	rvb, [sv3]
	eor	rvb, rvb, #0x80000000
	tst	rvc, #0x08		@ is this a rational?
	itT	eq
	seteq	rva, '/
	bytseteq sv5, sv4, rva
	itEEE	eq
	addeq	sv4, sv4, #4
	tstne	rvb, #0x80000000
	setne	rva, '+
	bytsetne sv5, sv4, rva
	it	ne
	addne	sv4, sv4, #4
	set	sv2, i0
	strlen	rva, sv1
numrc1:	bytref	rvb, sv1, sv2
	bytset	sv5, sv4, rvb
	add	sv2, sv2, #4
	add	sv4, sv4, #4
	eq	sv2, rva
	bne	numrc1
	ldr	rvc, [sv3, #-4]
	tst	rvc, #0x08		@ is this a rational?
	itT	ne
	setne	rvb, 'i
	bytsetne sv5, sv4, rvb
	set	sv1, sv5
	set	pc,  cnt

	/* (string->number string <fmt>)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	PRIMIT	"string->number", strnum, pfun, 2
	@ in:	sv1 <- string, sv2 <- <fmt>
	strlen	sv3, sv1		@ sv3 <- number of digits (scheme int)
	set	sv5, i0			@ sv5 <- offset of 1st char
strnen:	@ [internal re-entry]
	eq	sv5, sv3		@ string has no characters?
	beq	adr_i0fxt
	bytref	rva, sv1, sv5		@ rva <- 1st char of string
	eq	rva, #'#		@ is it a #?
	beq	strn_0			@	if so,  jump to use it to decide format
	eq	sv2, #0x29		@ is fmt = 10 (decimal format)?
	it	ne
	nullpne	sv2			@	or was no format specified?
	beq	strn10			@	if so,  jump to decimal conversion
	set	sv4, i1			@ sv4 <- 1 (shift, sch int), assume <fmt> is binary
	eq	sv2, #0x09		@ is fmt = 2, really?
	itT	ne
	setne	sv4, i4			@	if not, sv4 <- 4 (shft, sch int),  <fmt> is hex
	eqne	sv2, #0x41		@	if not, is fmt = 16, really?
	it	ne
	setne	sv4, i3			@	if not, sv4 <- 3 (shift, sch int),  <fmt> is octal
	b	strn_1			@ jump to binary, otal, hexadecimal conversion
strn_0:	@ identify format based on leading #d, #b, #o, #x, #e, #i
	add	sv5, sv5, #4		@ sv5 <- offset to digits after #
	bytref	rva, sv1, sv5		@ rva <- char of string, after #
	add	sv5, sv5, #4		@ sv5 <- offset to digits after #x
	orr	rva, rva, #0x20		@ rva <- 2nd char of string, lower case
	eq	rva, #'d		@ is 2nd char a d?  --  D?
	beq	strn10			@	if so,  jump to decimal conversion
	set	sv4, i1			@ sv4 <- 1 (shift, scheme int), assume <fmt> is binary
	eq	rva, #'b		@ is 2nd char a b?  --  b?
	itT	ne
	setne	sv4, i3			@	if not, sv4 <- 3 (shift, sch int),  <fmt> is octal
	eqne	rva, #'o		@	if not, is 2nd char a o?  --  O?
	itT	ne
	setne	sv4, i4			@	if not, sv4 <- 4 (shift, sch int), <fmt> is hex
	eqne	rva, #'x		@	if not, is 2nd char a x?
	bne	strnei			@	if not, jump to #e/#i case
strn_1:	@ keep going with binary, octal or hexadecimal (exact) conversion
	set	rvb, 0			@ rvb <- stores result	
	bytref	rvc, sv1, sv5
	eq	rvc, #'-		@ is it a minus?
	it	eq
	addeq	sv5, sv5, #4
strn_2:	@ binary, octal and hexadecimal conversion
	eq	sv5, sv3		@ are we done with string?
	beq	strn_3
	lsr	rva, sv4, #2		@ rva <- shift
	lsl	rvb, rvb, rva		@ rvb <- prior result, shifted
	bytref	rva, sv1, sv5		@ rva <- char
	tst	rva, #0x40		@ is char a->f or A->F?
	it	ne
	addne	rva, rva, #0x09		@	if so,  adjust it
	and	rva, rva, #0x0F		@ rva <- decimal value of char (raw int)
	add	rvb, rva, rvb		@ rvb <- updated result (raw int)
	add	sv5, sv5, #4		@ sv5 <- offset of next digit (scheme int)
	b	strn_2			@ jump to continue conversion
strn_3:	@ binary, octal and hexadecimal conversion
	raw2int	sv1, rvb		@ sv1 <- value as scheme int
	eq	rvc, #'-
	it	eq
	nginteq	sv1, sv1
	set	pc,  cnt
	
strn10:	@ decimal conversion to integer or float, overflows to nan
	@ in:	sv1 <- string
	@ in:	sv3 <- offset to after last digit
	@ in:	sv5 <- offset to first digit
	sub	sv2, sv3, #4		@ sv2 <- offset of last char
	bytref	rva, sv1, sv2		@ rva <- char
	eq	rva, #'i
	beq	strcpx
	set	sv2, sv5
strnlp:	bytref	rva, sv1, sv2		@ rva <- char
	eq	rva, #'/
	beq	strrat
	add	sv2, sv2, #4
	eq	sv2, sv3
	bne	strnlp
	set	lnk, cnt
_func_	
strnrm:	@ normal conversion of string to int or float
	@ in:	sv1 <- string
	@ in:	sv3 <- offset to after last digit
	@ in:	sv5 <- offset to first digit
	set	rvb, 0
	set	sv4, null
	bytref	rva, sv1, sv5
	eq	rva, #'-		@ is it a minus?
	itEE	eq
	seteq	rvc, false
	setne	rvc, true
	subne	sv5, sv5, #4		@	if not,  sv5 <- offset of before first digit
strn11:	add	sv5, sv5, #4		@ sv5 <- offset of next digit
	eq	sv5, sv3		@ are we done with string?
	beq	strn12			@	if so,  jump to finish up
	bytref	rva, sv1, sv5		@ rva <- char
	eq	rva, #'.		@ is char #\. ?
	it	eq
	seteq	sv4, sv5		@	if so,  sv4 <- offset of dot
	beq	strn11			@	if so,  jump to keep processing number
	eq	rva, #'#
	itT	eq
	seteq	sv4, sv5		@	if so,  sv4 <- offset of #
	seteq	rva, '0
	cmp	rva, #'0		@ is char >= #\0 ?
	bmi	strn12			@	if not, jump to finish up or convert exponent
	cmp	rva, #':		@ and is it <= #\9 ?
	bpl	strn12			@	if not, jump to finish up or convert exponent
	and	rva, rva, #0x0F		@ rva <- integer from digit (raw int)
	lsl	rvb, rvb, #1		@ rvb <- previous result * 2 (raw int)
	adds	rvb, rvb, rvb, LSL #2	@ rvb <- previous result * 10 (raw int)
	bcs	adr__err
	adds	rvb, rvb, rva		@ rvb <- updated integer result (raw int)
	bcs	adr__err
	eq	rvc, #f
	it	eq
	eqeq	rvb, #0x20000000
	beq	strn11
	tst	rvb, #0xE0000000
	bne	adr__err
	b	strn11			@ jump to continue processing
strn12:	@
	eq	rvc, #f
	it	eq
	rsbeq	rvb, rvb, #0		@ if so,  rvb <- negative number (raw int)
	raw2int	sv2, rvb		@ sv2 <- number (scheme int)
	eq	sv5, sv3		@ are we at end of string?
	it	eq
	nullpeq	sv4			@	and was no dot or exponent encountered?
	itT	eq
	seteq	sv1, sv2		@	if so,  sv1 <- number (an integer)
	seteq	pc,  lnk
	@ float conversion, sv2 is integer part (scheme int), sv4 is position of dot or null
	nullp	sv4			@ was a dot encountered?
	it	eq
	seteq	rvb, 0			@	if not, rvb <- 0 (exponent due to dot)
	itTTT	ne
	subne	rvb, sv5, sv4		@	if so,  rvb <- current position - dot position
	lsrne	rvb, rvb, #2		@	if so,  rvb <- exponent + 1 (raw int)
	subne	rvb, rvb, #1		@	if so,  rvb <- exponent due to dot (positive)
	rsbne	rvb, rvb, #0		@	if so,  rvb <- expon due to dot (prprly ngtv)
	eq	sv5, sv3		@ was string fully processed? (no e or E part)
	beq	strn16			@	if so,  skip processing of e/E part
	lsl	rvb, rvb, #8		@ rvb <- exponent due to dot, shifted
	add	sv5, sv5, #4		@ sv5 <- index of 1st digit of e/E exponent
	bytref	rva, sv1, sv5		@ rva <- 1st digit of e/E exponent
	eq	rva, #'-		@ is it a minus?
	itEE	eq
	orreq	sv4, rvb, #f		@	if minus, sv4 <- dot exponent packed with #f
	orrne	sv4, rvb, #t		@	if plus,  sv4 <- dot exponent packed with #t
	subne	sv5, sv5, #4		@	if plus,  sv5 <- offset of before first digit
	set	rvb, 0			@ rvb <- 0 (starting vaue of e/E exponent)
strn14:	add	sv5, sv5, #4		@ sv5 <- offset of next digit
	eq	sv5, sv3		@ are we done with string?
	beq	strn15			@	if so, jump to finish up
	bytref	rva, sv1, sv5		@ rva <- char
	and	rva, rva, #0x0F		@ rva <- integer from digit (raw int)
	lsl	rvb, rvb, #1		@ rvb <- previous result * 2 (raw int)
	add	rvb, rvb, rvb, LSL #2	@ rvb <- previous result * 10 (raw int)
	add	rvb, rvb, rva		@ rvb <- updated integer result (raw int)
	b	strn14			@ jump to continue processing
strn15:	and	sv3, sv4, #0xFF
	eq	sv3, #f
	it	eq
	rsbeq	rvb, rvb, #0
	add	rvb, rvb, sv4, ASR #8
strn16:	@ unpack sv2 to rva (num) sv1 (sign in float) and rvb to rvb (num) + branch accord to sign
	set	sv1, f0			@ sv1 <- blank scheme float (zero)
	int2raw	rva, sv2		@ rva <- number as raw int
	cmp	rva, #0			@ is number negative?
	itT	mi
	orrmi	sv1, sv1, #0x80000000	@	if so, place minus sign in float
	rsbmi	rva, rva, #0		@	if so, make number positive
	eq	rva, #0			@ is number zero?
	it	eq
	seteq	pc,  lnk		@	if so, exit with +/- 0.0
	set	rvc, lnk		@ rvc <- lnk saved against lftshf, idiv10
	set	sv4, i0			@ sv4 <- 0 (scheme int)
	set	sv5, sv4
	cmp	rvb, #0			@ is decimal exponent negative?
	it	mi
	rsbmi	rvb, rvb, #0		@	if so, make it positive
	orr	sv2, sv4, rvb, LSL #2	@ sv2 <- decimal exponent as scheme int
	bmi	tokfln			@	if dec exp is negat, branch to proc negat expon
tokflp:	bl	lftshf			@ rva <- number shifted left, rvb <- # shift steps
	rsb	rvb, rvb, sv5, ASR #2	@ rvb <- updated binary exponent (raw int)
	orr	sv5, sv4, rvb, LSL #2	@ sv5 <- bin exp decreased by shift (scheme int)
	eq	sv2, sv4		@ are we done? (decimal exponent = 0 as scheme int)
	beq	tokfxz			@	if so, exit
	@ divide number by 16, update binary exponent
	lsr	rva, rva, #4		@ rva <- number / 16
	set	rvb, 4			@ rvb <- 4
	add	rvb, rvb, sv5, ASR #2	@ rvb <- updated binary exponent (raw int)
	orr	sv5, sv4, rvb, LSL #2	@ sv5 <- bin exponent increased by 4 (scheme int)
	@ multiply number by 10 and update decimal exponent (sv2)
	sub	sv2, sv2, #4		@ sv2 <- decimal exponent minus 1 (scheme int)
	lsl	rvb, rva, #3		@ rvb <- number * 8
	add	rva, rvb, rva, LSL #1	@ rva <- number * 10
	b	tokflp
tokfln:	bl	lftshf			@ rva <- number shifted left, rvb <- # shift steps
	rsb	rvb, rvb, sv5, ASR #2	@ rvb <- updated binary exponent
	orr	sv5, sv4, rvb, LSL #2	@ sv5 <- bin exp decreased by shift (scheme int)
	eq	sv2, sv4		@ are we done? (decimal exponent = 0 as scheme int)
	beq	tokfxz			@	if so, exit
	@ divide number by 10 and update decimal exponent (sv2)
	sub	sv2, sv2, #4		@ sv2 <- decimal exponent ("neg") minus 1 (scheme int)
	bl	idiv10			@ rva <- number / 10
	b	tokfln
tokfxz: @
	set	lnk, rvc		@ lnk <- lnk, restored	
	lsr	rva, rva, #3
	set	rvb, 3
	add	rvb, rvb, sv5, ASR #2
	postv	sv1			@ is number positive?
	it	ne
	rsbne	rva, rva, #0		@	if not, negativize it
	orr	sv1, sv4, rva, LSL #2	@ sv1 <- number (scheme int)
	add	rva, rvb, #148		@ 127 + 21
	b	mteflt

strrat:	@ string -> rational
	@ in:	 sv2 <- offset of /
	save	sv1, sv2, sv3
	set	sv3, sv2
	bl	strnrm			@ sv1 <- numerator-or-real-part
	tst	sv1, #float_tag		@ is numerator a scheme int?
	bne	adr__err		@	if not, jump to report error
	set	sv4, sv1		@ sv4 <- numerator-or-real-part
	restor	sv1, sv5, sv3
	save	sv4			@ dts <- (numerator-or-real-part ...)
	add	sv5, sv5, #4
	eq	sv5, sv3
	it	eq
	seteq	sv1, 5
	pntrp	sv1
	it	eq
	bleq	strnrm			@ sv1 <- denominator-or-imaginary part
	tst	sv1, #float_tag		@ is denominator a scheme int?
	bne	adr__err		@	if not, jump to report error
	restor	sv4			@ sv4 <- numerator-or-real-part, dts <- (...)
	set	sv2, sv1		@ sv2 <- denominator
	set	sv1, sv4		@ sv1 <- numerator
	set	lnk, cnt
	b	adr_intdiv

strcpx:	@ string -> complex
	sub	sv3, sv3, #4
	set	sv2, sv3
strcx1:	@ loop to find +/- between real and imag parts
	sub	sv2, sv2, #4
	eq	sv2, sv5
	beq	strcx2			@ no real part
	bytref	rva, sv1, sv2		@ rva <- char
	eq	rva, #'+
	it	ne
	eqne	rva, #'-
	bne	strcx1
	eq	sv2, sv5
	beq	strcx2
	sub	rvc, sv2, #4
	bytref	rva, sv1, rvc		@ rva <- prior char
	eq	rva, #'e
	it	ne
	eqne	rva, #'E
	beq	strcx1
	@ extract parts
	save	sv1, sv2, sv3
	set	sv3, sv2
	bl	strnrm			@ sv1 <- real-part
	set	sv2, f0
	bl	uninum
	set	sv4, sv1		@ sv4 <- real-part
	restor	sv1, sv5, sv3
	save	sv4			@ dts <- (real-part ...)
	bytref	rva, sv1, sv5
	eq	rva, #'-
	it	ne
	addne	sv5, sv5, #4
	bl	strnrm			@ sv1 <- imaginary part
	restor	sv2			@ sv2 <- real-part, dts <- (...)
strrc1:	@ finish up
	swap	sv1, sv2, sv3		@ sv1 <- real-part, sv2 <- imag-part, sv3 <- temp
	bl	uninum
	set	lnk, cnt
	b	makcpx
strcx2:	@ complex with no real part
	bl	strnrm			@ sv1 <- imaginary part
	set	sv2, f0			@ sv2 <- real-part (zero)
	b	strrc1
	
strnei:	@ number starting with #e or #|, or #i or #~
	@ on entry, rva <- char following #
	eq	rva, #'e
	it	ne
	eqne	rva, #'|
	itE	eq
	seteq	sv4, true
	setne	sv4, false
	sav_rc	sv4
	call	strnen
	restor	sv4, cnt
	eq	sv4, #t
	it	eq
	ldreq	rvc, =tb_numi2e
	beq	numjmp
	set	sv2, f0			@ sv2 <- float zero
	set	lnk, cnt
	b	uninum

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.2.   Numbers
@  II.A.6.2.6. Numerical input and output SUPPORT:	lftshf, idiv10
@-----------------------------------------------------------------------------*/

.ifndef	hardware_FPU	@ exclude cortex-a8
  .ifndef cortex	@ left shift on ARMv4T

lftshf:	@ shift rva left until it starts with a 1, rvb has number of steps used to shift
	set	rvb, 0
lftsh0:	cmp	rva, #0
	it	mi
	setmi	pc,  lnk
	lsl	rva, rva, #1
	add	rvb, rvb, #1
	eq	rvb, #32
	it	eq
	seteq	pc,  lnk
	b	lftsh0

  .else	@ left shift on ARMv7M (cortex-m3)

_func_	
lftshf:	@ shift rva left until it starts with a 1, rvb has number of steps used to shift
	clz	rvb, rva
	lsl	rva, rva, rvb
	set	pc,  lnk

  .endif

.else	@ left shift on devices with FPU (eg. ARMv7A (cortex-a8) or EP9302 = ARMv4T)

  .ifndef FPU_is_maverick
	
_func_	
lftshf:	@ shift rva left until it starts with a 1, rvb has number of steps used to shift
	clz	rvb, rva
	lsl	rva, rva, rvb
	set	pc,  lnk

  .else
	
lftshf:	@ shift rva left until it starts with a 1, rvb has number of steps used to shift
	set	rvb, 0
lftsh0:	cmp	rva, #0
	it	mi
	setmi	pc,  lnk
	lsl	rva, rva, #1
	add	rvb, rvb, #1
	eq	rvb, #32
	it	eq
	seteq	pc,  lnk
	b	lftsh0

  .endif
	
.endif

.ifndef cortex	@ integer division by 10 on ARMv4T and ARMv7A (cortex-a8)
	
idiv10:	@ positive integer division by 10:	rva <- rva / 10 (rvb used also)
	lsr	rvb, rva, #1		@ rvb <- num/2^1
	add	rvb, rvb, rva, LSR #2	@ rvb <- num/2^(1+2)
	add	rvb, rvb, rvb, LSR #4	@ rvb <- num/2^(1+2+5+6)
	add	rvb, rvb, rvb, LSR #8	@ rvb <- num/2^(1+2+5+6+9+10+13+14)
	add	rvb, rvb, rvb, LSR #16	@ rvb <- num/2^(1+2+5+6+9+10+13+14+17+18+21+22+25+26+29+30)
	lsr	rvb, rvb, #3		@ rvb <-num/2^(4+5+8+9+12+13+16+17+20+21+24+25+28+29+32+33)
	sub	rva, rva, rvb, LSL #3	@ rva <- approximation error: rva -  8*rvb
	sub	rva, rva, rvb, LSL #1	@ rva <- approximation error: rva - 10*rvb (value: 0 to 9?)
	add	rva, rva, #6		@ rva <- aproximation error + 6 (0110)
	add	rva, rvb, rva, LSR #4	@ rva <- rva / 10 == rvb + (approximation error + 6)/16
	set	pc,  lnk		@ return (result is in rva)

.else	@ integer division by 10 on ARMv7M (cortex-m3)
	
_func_	
idiv10:	@ positive integer division by 10:	rva <- rva / 10 (rvb used also)
	set	rvb, 10
	udiv	rva, rva, rvb
	set	pc,  lnk

.endif


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


.ifndef CORE_ONLY

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@			GENERAL NUMBERS LIBRARY functions
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.2.	Numbers
@	6.2.5	Numerical operations:	zero?, positive?, negative?, odd?, even?
@					max, min, abs,gcd, lcm, rationalize
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		flsfxt, trufxt, boolxt, corerr, notfxt
@					save, save3, cons, sav_rc, zmaloc
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/


	/* (zero? obj) */
	PRIMIT	"zero?", zero, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- #t/#f
	@ mods:	sv1
	zerop	sv1
	b	adr_boolxt

	/* (positive? obj)                ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"positive?", pst, 1, onumgto, null, flsfxt, intpst, intpst, ratpst, flsfxt

	/* positive? for integer and float */
	PRIMIT	intpst, ufun, 0
	zerop	sv1
	beq	adr_notfxt
	postv	sv1			@ is number positive?
	b	adr_boolxt		@ exit with #t/#f based on result

	/* positive? for rational */
	PRIMIT	ratpst, ufun, 0
	numerat	sv1, sv1
	b	adr_intpst

	/* (negative? obj)                ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"negative?", ngt, 1, onumgto, null, flsfxt, intngt, intngt, ratngt, flsfxt

	/* negative? for integer and float */
	PRIMIT	intngt, ufun, 0
	postv	sv1			@ is number positive?
	b	adr_notfxt		@ exit with #f/#t based on result

	/* negative? for rational */
	PRIMIT	ratngt, ufun, 0
	numerat	sv1, sv1
	b	adr_intngt

	/* (odd? obj) */
	PRIMIT	"odd?", odd, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- #t/#f
	@ mods:	sv1, rva
	intgrp	sv1			@ is sv1 an integer?
	bne	adr_boolxt		@	if not, exit with #f
	tst	sv1, #0x04		@ is sv1 even?
	b	adr_notfxt		@ return with not #t/#f

	/* (even? obj) */
	PRIMIT	"even?", even, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- #t/#f
	@ mods:	sv1, rva
	intgrp	sv1			@ is sv1 an integer?
	it	eq
	tsteq	sv1, #0x04		@	if so,  is it even?
	b	adr_boolxt		@ return with #t/#f

	/* entry for max, min, gcd, lcm (general numbers version) */
	PRIMIT	mmglen, ufun, 0
	@ in:	sv1 <- (num1 num2 ...)
	@ in:	sv5 <- binary operator table = maxtb, mintb, gcdtb or lcmtb
	@ out:	sv4 <- startup value for rdcnml
	nullp	sv1			@ are there no arguments?
	itE	eq
	seteq	sv4, sv1		@	if so,  sv4 <- '()
	carne	sv4, sv1		@	if not, sv4 <- num1
	b	adr_rdcnml		@ jump to reduce arg-list using operator and default value

	/* (max num1 num2 ...)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"max", 0, ommglen, null, _err, intmax, fltmax, ratmax, _err

	/* max for integer */
	PRIMIT	intmax, ufun, 0
	cmp	sv1, sv2		@ is x1 >= x2 ?
	it	mi
	setmi	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	set	pc,  lnk		@ return with largest number in sv1

	/* max for float */
	PRIMIT	fltmax, ufun, 0
	anynan	sv1, sv2
	beq	nanlxt
	postv	sv1			@ is x1 positive?
	it	ne
	postvne	sv2			@	if so,  is x2 negative?
	bne	adr_intmin		@	if so,  jump to compare that (both floats negative)
	postv	sv1			@ is x1 positive or 0?
	itE	ne
	setne	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	postveq	sv2			@	if so,  is x2 positive or 0?
	it	ne
	setne	pc,  lnk		@	if not, (either is ngtv) exit w/non-ngtv one
	@ continue to maxint
	b	adr_intmax

	/* max for rational */
	PRIMIT	ratmax, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3
	denom	sv1, sv1
	numerat	sv2, sv2
	bl	adr_intprd
	set	sv3, sv1
	snoc	sv1, sv2, dts
	car	sv2, sv2
	numerat	sv1, sv1
	denom	sv2, sv2
	bl	adr_intprd
	set	sv2, sv3
	ldr	rvc, =tb_numgt
mxmnrt:	@ max/min common completion for rat
	bl	numjmp	
	eq	sv1, #f
	restor	sv1, sv2, sv3
	orr	lnk, sv3, #lnkbit0
	it	eq
	seteq	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	denom	sv2, sv1
	eq	sv2, #5
	it	eq
	nmrtreq	sv1, sv1
	set	pc,  lnk		@ return with largest number in sv1

	/* (min num1 num2 ...)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"min", 0, ommglen, null, _err, intmin, fltmin, ratmin, _err

	/* min for integer */
	PRIMIT	intmin, ufun, 0
	cmp	sv1, sv2		@ is x1 >= x2 ?
	it	pl
	setpl	sv1, sv2		@	if not, sv1 <- x2 (smallest number)
	set	pc,  lnk		@ return with smallest number in sv1
	
	/* min for float */
	PRIMIT	fltmin, ufun, 0
	anynan	sv1, sv2
	beq	nanlxt
	postv	sv1			@ is x1 negative?
	it	ne
	postvne	sv2			@	if so,  is x2 negative?
	bne	adr_intmax		@	if so,  jump to compare that (both floats negative)
	postv	sv2			@ is x2 positive or 0?
	itE	ne
	setne	sv1, sv2		@	if not, sv1 <- x2 (smallest number)
	postveq	sv1			@	if so,  is x1 positive or 0?
	it	ne
	setne	pc,  lnk		@	if not, (either is ngtv) exit w/ngtv one
	@ continue to minint
	b	adr_intmin

	/* min for rational */
	PRIMIT	ratmin, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3
	denom	sv1, sv1
	numerat	sv2, sv2
	bl	adr_intprd
	set	sv3, sv1
	snoc	sv1, sv2, dts
	car	sv2, sv2
	numerat	sv1, sv1
	denom	sv2, sv2
	bl	adr_intprd
	set	sv2, sv3
	ldr	rvc, =tb_numlt
	b	mxmnrt			@ jump to common completion of max/min for rat

	/* (abs number)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"abs", 1, onumgto, null, _err, intabs, fltabs, ratabs, _err

	/* (gcd n1 ...)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"gcd", 0, ommglen, null, _err, intgcd, fltgcd, ratgcd, _err

	/* gcd for float */
	PRIMIT	fltgcd, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv5 <- lnk, saved (and made even if Thumb2)
	save	sv3
	bl	itrunc
	set	sv3, sv1
	set	sv1, sv2
	bl	itrunc
	set	sv2, sv1
	set	sv1, f0
	b	gcdien

	/* gcd for rational */
	PRIMIT	ratgcd, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3		@ dts <- (rat1 rat2 lnk ...)
	denom	sv1, sv1
	denom	sv2, sv2
	bl	adr_intlcm		@ sv1 <- lcm of denom1 and denom2 (scheme int)
	restor	sv2, sv3		@ sv2 <- rat1, sv3 <- rat2, dts <- (lnk ...)
	save	sv1			@ dts <- (denom-lcm lnk ...)
	numerat	sv1, sv2
	numerat	sv2, sv3
	bl	adr_intgcd		@ sv1 <- numerat-gcd
	restor	sv2, sv3		@ sv2 <- denom-lcm, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	b	adr_intdiv		@ jump to make rational result

	/* (lcm n1 ...)  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"lcm", 0, ommglen, null, _err, intlcm, fltlcm, ratlcm, _err

	/* lcm for integer */
	PRIMIT	intlcm, ufun, 0
	eq	sv1, #i0
	it	ne
	eqne	sv2, #i0
	itT	eq
	seteq	sv1, i0
	seteq	pc,  lnk
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	iabs	sv1, sv1
	iabs	sv2, sv2
	save	sv1, sv2, sv3		@ dts <- (int1 int2 lnk ...)
	bl	adr_intgcd		@ sv1 <- gcd of int1 and int2 (scheme int)
	set	sv2, sv1
	restor	sv1			@ sv1 <- int1,		dts <- (int2 lnk ...)
	bl	idivid			@ sv1 <- n1 / gcd (scheme int)
	restor	sv2, sv3		@ sv2 <- int2, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0
	b	adr_intprd

	/* lcm for float */
	PRIMIT	fltlcm, ufun, 0
	eq	sv1, #f0
	it	ne
	eqne	sv2, #f0
	itT	eq
	seteq	sv1, f0
	seteq	pc,  lnk
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv3
	bl	itrunc
	swap	sv1, sv2, sv3
	bl	itrunc
	bl	adr_intlcm
	restor	sv3			@ sv3 <- lnk,		dts <- (...)
	orr	lnk, sv3, #lnkbit0
	b	i12flt

	/* lcm for rational */
	PRIMIT	ratlcm, ufun, 0
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv1, sv2, sv3		@ dts <- (rat1 rat2 lnk ...)
	denom	sv1, sv1
	denom	sv2, sv2
	bl	adr_intgcd		@ sv1 <- gcd of denom1 and denom2 (scheme int)
	restor	sv2, sv3		@ sv2 <- rat1, sv3 <- rat2, dts <- (lnk ...)
	save	sv1			@ dts <- (denom-gcd lnk ...)
	numerat	sv1, sv2
	numerat	sv2, sv3
	bl	adr_intlcm		@ sv1 <- numerat-lcm
	restor	sv2, sv3		@ sv2 <- denom-gcd, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	izerop	sv2
	it	eq
	seteq	pc,  lnk
	b	adr_intdiv		@ jump to make rational result

	/* (rationalize x y)   		~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ */
	NUMTBF	"rationalize", rtz, 2, ounijpe, null, _err, intrtz, fltrtz, ratrtz, _err
	@ in:	sv1 <- x
	@ in:	sv2 <- y
	@ out:	sv1 <- ratio

	/* special returns table for rationalize, based on y, tested first (but after x == nan) */
	startBVU8 rtyspc
	.word	adr_return		@   x <- (rationalize x 0)
	.word	adr_f0fxt		@ 0.0 <- (rationalize x inf)
	.word	adr_f0fxt		@ 0.0 <- (rationalize x -inf)
	ENDsized

	/* special returns table for rationalize, based on x, tested after rtyspc */
	startBVU8 rtxspc
	.word	adr_return		@    0 <- (rationalize 0 y)
	.word	adr_return		@  inf <- (rationalize inf y)
	.word	adr_return		@ -inf <- (rationalize -inf y)
	ENDsized

	/* rationalize for int */
	PRIMIT	intrtz, ufun, 0
	iabs	sv2, sv2
	set	sv5, sv1
	iabs	sv1, sv1
	set	sv4, sv2
	bl	adr_intpls
	set	sv2, sv4
	set	sv4, sv1
	set	sv1, sv5
	iabs	sv1, sv1
	bl	adr_intmns
	eor	rva, sv1, sv4
	postv	rva
	it	ne
	setne	sv1, i0
rtzrxi:	@ exit for int
	postv	sv5
	it	ne
	ngintne	sv1, sv1
	set	pc,  cnt

	/* rationalize for float */
	PRIMIT	fltrtz, ufun, 0
	isnan	sv1
	it	eq
	seteq	pc,  cnt
	@ rationalize with x and y as flt
	set	sv4, sv1	@ sv4 <- x
	fabs	sv1, sv2	@ sv1 <- |y|
	ldr	sv5, =rtyspc
	bl	spcflt
	set	sv1, sv4	@ sv1 <- x
	ldr	sv5, =rtxspc
	bl	spcflt
	@ exit with 0.0 if range is larger than val (i.e. spans 0)
	fabs	sv1, sv2	@ sv1 <- |y|
	fabs	sv3, sv4	@ sv3 <- |x|
	cmp	sv1, sv3
	bpl	adr_f0fxt
	@ find top and bottom of interval (and sort, in abs val, in case top/bottom are unsorted)
	set	sv2, sv4	@ sv2 <- x
	set	sv3, sv1	@ sv3 <- |y|
	bl	adr_fltpls	@ sv1 <- top-signed = x + |y|
	set	sv2, sv3	@ sv2 <- |y|
	set	sv3, sv1	@ sv3 <- top-signed
	set	sv1, sv4	@ sv1 <- x
	bl	adr_fltmns	@ sv1 <- bottom-signed = x - |y|
	sav_rc	sv1		@ dts <- (bottom-signed cnt ...)
	fabs	sv1, sv1
	fabs	sv2, sv3
	set	sv4, null
	cmp	sv1, sv2
	bmi	rtzflp
	swap	sv1, sv2, rva
rtzflp:	@ loop
	save	sv1, sv2, sv4	@ dts <- (bottom top coeffs top-signed cnt ...)
	call	adr_fltdnm
	set	sv3, sv1	@ sv3 <- denom-bottom	
	restor	sv1, sv2, sv4	@ sv1 <- bttm, sv2 <- top, sv4 <- coeffs, dts <- (top-sgnd cnt ...)
	set	sv5, sv1	@ sv5 <- bottom
	set	rvc, scheme_one
	eq	sv3, rvc
	beq	rtzfx1
	bl	itrunc
	set	sv3, sv1	@ sv3 <- truncated bottom
	set	sv1, sv2
	bl	itrunc		@ sv1 <- truncated top
	eq	sv1, sv3
	bne	rtzfx0
	cons	sv4, sv3, sv4	@ sv4 <- updated coeffs list
	@ sv5 <- bottom, sv2 <- top, sv4 <- coefs list
	set	sv1, sv2
	bl	flt2ndn		@ sv1 <- numer-top, sv2 <- denom-top
	set	sv3, sv2	@ sv3 <- denom-top
	bl	idivid		@ sv2 <- remainder-top
	set	sv1, sv3	@ sv1 <- denom-top
	bl	i12flt
	save	sv4
	set	sv4, sv5	@ sv4 <- bottom
	bl	unidiv		@ sv1 <- denom-top/remainder-top
	swap	sv1, sv4, sv3	@ sv1 <- bottom, sv4 <- denom-top/remainder-top
	bl	flt2ndn		@ sv1 <- numer-bottom, sv2 <- denom-bottom
	set	sv3, sv2	@ sv3 <- denom-bottom
	bl	idivid		@ sv2 <- remainder-bottom
	set	sv1, sv3	@ sv1 <- denom-bottom
	bl	i12flt
	bl	unidiv		@ sv1 <- denom-bottom/remainder-bottom
	set	sv2, sv1	@ sv2 <- denom-bottom/remainder-bottom
	set	sv1, sv4	@ sv1 <- denom-top/remainder-top
	restor	sv4		@ sv4 <- coefs list
	b	rtzflp
rtzfx0:	@ build 1+truncated bottom
	set	sv2, scheme_one
	set	sv1, sv3	@ sv1 <- truncated bottom
	bl	i12flt
	bl	adr_fltpls
rtzfx1:	@ sv1 <- bottom or 1+truncated bottom
	@ sv4 <- coeffs list
	nullp	sv4
	beq	rtzfxt
	set	sv2, sv1
	set	sv1, scheme_one
	bl	adr_fltdiv
	snoc	sv2, sv4, sv4
	adr	lnk, rtzfx1
	b	unipls
	
rtzfxt:	@ exit
	restor	sv5, cnt
	postv	sv5
	it	ne
	ngfltne	sv1, sv1
	set	pc,  cnt

	/* rationalize for rational */
	PRIMIT	ratrtz, ufun, 0
	@ rationalize with x and y as rat
	@ return nan if sv1 == 0/0
	rawsplt	rva, rvb, sv1
	eq	rva, #3
	it	eq
	eqeq	rvb, #0
	it	eq
	seteq	pc,  cnt
	@ return nan if sv2 == 0/0
	rawsplt	rva, rvb, sv2
	eq	rva, #3
	it	eq
	eqeq	rvb, #0
	itT	eq
	seteq	sv1, sv2
	seteq	pc,  cnt
	@ return 0 if sv2 == 1/0
	eq	rva, #0x13
	it	eq
	eqeq	rvb, #0
	beq	adr_i0fxt
	@ return 0 if sv2 == -1/0
	mvn	rvc, rva
	eq	rvc, #0x0c
	it	eq
	eqeq	rvb, #3
	beq	adr_i0fxt
	@ return sv1 if sv2 == 0/1
	eq	rva, #3
	it	eq
	eqeq	rvb, #4
	it	eq
	seteq	pc,  cnt
	@ return sv1 if sv1 == +/- 1/0
	ldr	rvb, [sv1]
	bic	rvb, rvb, #3
	eq	rvb, #0
	it	eq
	seteq	pc,  cnt	
	@ check if |x| <= |y| if so, return 0
	numerat	sv5, sv1
	sav_rc	sv5		@ dts <- (x-numerat-signed cnt ...)
	set	sv4, sv1	@ sv4 <- x
	set	sv1, sv2	@ sv1 <- y
	call	adr_ratabs	@ sv1 <- |y|
	set	sv5, sv1	@ sv5 <- |y|
	set	sv1, sv4	@ sv1 <- x
	call	adr_ratabs	@ sv1 <- |x|
	set	sv4, sv1	@ sv4 <- |x|
	set	sv2, sv5	@ sv2 <- |y|
	save	sv5		@ dts <- (|y| x-numerat-signed cnt ...)
	bl	unipls		@ sv1 <- top
	restor	sv2		@ sv2 <- |y|,	dts <- (x-numerat-signed cnt ...)
	save	sv1		@ dts <- (top x-numerat-signed cnt ...)
	set	sv1, sv4	@ sv1 <- |x|
	bl	unimns		@ sv1 <- bottom
	restor	sv2		@ sv2 <- top,	dts <- (x-numerat-signed cnt ...)
	bl	uninum		@ sv1 and sv2 both int or rat	
	intgrp	sv1
	bne	rtlzr0
	eor	rva, sv1, sv2
	postv	rva
	beq	rtlzr1
rtzaxt:	@ exit with 0	
	restor	sv5, cnt
	b	adr_i0fxt
	
rtlzr0:	@
	numerat	sv3, sv1
	numerat	sv4, sv2
	eor	rva, sv3, sv4
	postv	rva
	bne	rtzaxt
rtlzr1:
	set	sv4, null
rtzrlp:	@ loop
	@ sv1 <- bottom, sv2 <- top
	intgrp	sv1
	beq	rtzrx1
	denom	sv3, sv1
	eq	sv3, #5
	it	eq
	nmrtreq	sv1, sv3
	beq	rtzrx1
	set	sv3, sv1	@ sv3 <- bottom
	spltrat	sv1, sv2, sv2	@ sv1 <- top-numerator, sv2 <- top-denominator
	save	sv2		@ dts <- (top-denominator x-signed cnt ...)
	bl	idivid		@ sv1 <- top-quotient, sv2 <- top-remainder
	set	sv5, sv2	@ sv5 <- top-remainder
	set	sv2, sv3	@ sv2 <- bottom
	set	sv3, sv1	@ sv3 <- top-quotient
	spltrat	sv1, sv2, sv2	@ sv1 <- bottom-numerator, sv2 <- bottom-denominator
	save	sv2		@ dts <- (bottom-denominator top-denominator x-signed cnt ...)
	bl	idivid		@ sv1 <- bottom-quotient, sv2 <- bottom-remainder
	eq	sv1, sv3
	bne	rtzrx0
	cons	sv4, sv1, sv4	@ sv4 <- updated coeffs list
	@ sv2 <- bottom-remainder, sv5 <- top-remainder
	restor	sv1		@ sv1 <- bottom-denominator, dts <- (top-dnmntr x-sgnd cnt ...)
	bl	adr_intdiv	@ sv1 <- bottom-denominator/bottom-remainder
	set	sv2, sv5	@ sv2 <- top-remainder
	set	sv5, sv1	@ sv5 <- bottom-denominator/bottom-remainder
	restor	sv1		@ sv1 <- top-denominator, dts <- (x-signed cnt ...)
	bl	adr_intdiv	@ sv1 <- top-denominator / top-remainder
	set	sv2, sv5	@ sv2 <- bottom-denominator / bottom-remainder
	adr	lnk, rtzrlp
	b	uninum
	
rtzrx0:	@ build 1+bottom-quotient
	cddr	dts, dts
	add	sv1, sv1, #4
rtzrx1:	@ sv1 <- bottom or 1+truncated bottom
	@ sv4 <- coeffs list
	nullp	sv4
	beq	rtzrxt
	set	sv2, sv1
	set	sv1, i1
	bl	unidiv
	snoc	sv2, sv4, sv4
	adr	lnk, rtzrx1
	b	unipls
	
rtzrxt:	@ exit
	restor	sv5, cnt
	intgrp	sv1
	beq	rtzrxi
	spltrat	sv1, sv2, sv1
	postv	sv5
	it	ne
	ngintne	sv1, sv1
	set	lnk, cnt
	b	adr_intdiv

.endif

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



