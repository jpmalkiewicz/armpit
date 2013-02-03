@---------------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

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
@---------------------------------------------------------------------------------------------------------

@---------------------------------------------------------------------------------------------------------
@
@  0.G.	  ASSEMBLER MACROS for SCHEME
@
@---------------------------------------------------------------------------------------------------------

.macro _func_
  .ifdef cortex
	.thumb_func
  .endif
.endm

.macro	SYMSIZE n
	.word	(\n << 8) | symbol_tag
.endm

.macro	VECSIZE n
	.word	((\n) << 8) | vector_tag
.endm

.macro	VU8SIZE n
	.word	((\n) << 8) | bytevector_tag
.endm

.macro	SYNTAX narg
	.word	(1 << 11) | (\narg << 8) | proc
.endm
	
.macro	ESYNTAX initsv4, fentry, narg
	.word	((\initsv4) << 24) | (\fentry << 16) | (1 << 11) | (\narg << 8) | proc
.endm
	
.macro	PFUNC narg
	.word	(\narg << 8) | proc
.endm
	
.macro	EPFUNC initsv4, fentry, narg
	.word	((\initsv4) << 24) | (\fentry << 16) | (\narg << 8) | proc
.endm

.macro	r2i rawint
	((\rawint << 2) | i0)
.endm
	
@
@ 4.1.6. Assignments
@

.macro set var, expr
	mov	\var,  \expr
.endm

.macro seteq var, expr
	moveq	\var,  \expr
.endm

.macro setne var, expr
	movne	\var,  \expr
.endm

.macro setmi var, expr
	movmi	\var,  \expr
.endm

.macro setpl var, expr
	movpl	\var,  \expr
.endm

.macro setls var, expr
	movls	\var,  \expr
.endm

.macro sethi var, expr
	movhi	\var,  \expr
.endm

@
@ 6.1. Equivalence predicates
@

.macro eq obj1, obj2
	teq	\obj1,  \obj2
.endm

.macro eqeq obj1, obj2
	teqeq	\obj1,  \obj2
.endm

.macro eqne obj1, obj2
	teqne	\obj1,  \obj2
.endm

@
@ 6.2.5. Numerical operations (including addendum)
@

.macro incr dest, source
	add	\dest,  \source, #4
.endm

.macro increq dest, source
	addeq	\dest,  \source, #4
.endm

.macro incrne dest, source
	addne	\dest,  \source, #4
.endm

.macro decr dest, source
	sub	\dest,  \source, #4
.endm

.macro decrne dest, source
	subne	\dest,  \source, #4
.endm

.macro intgrp obj
	@ raise eq flag if obj is an integer
	@ uses rva
	and	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eq	rva, #int_tag		@ is object an integer?
.endm

.macro intgrpeq obj
	@ raise eq flag if obj is an integer
	@ uses rva
	itT	eq
	andeq	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eqeq	rva, #int_tag		@ is object an integer?
.endm

.macro intgrpne obj
	@ raise eq flag if obj is an integer
	@ uses rva
	itT	ne
	andne	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eqne	rva, #int_tag		@ is object an integer?
.endm

.macro floatp obj
	@ raise eq flag if obj is a float
	@ uses rva
	and	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eq	rva, #float_tag		@ is object a float?
.endm

.macro ratiop obj
	@ raise eq flag if obj is a rational
	@ uses rva
	pntrp	\obj
	itTT	eq
	careq	rva, \obj
	andeq	rva, rva,  #0x0F	@ rva <- four-bit tag of object
	eqeq	rva, #rational_tag	@ is object a rational?
.endm

.macro cmplxp obj
	@ raise eq flag if obj is a complex
	@ uses rva
	pntrp	\obj
	itTT	eq
	careq	rva, \obj
	andeq	rva, rva,  #0x0F	@ rva <- four-bit tag of object
	eqeq	rva, #complex_tag	@ is object a complex?
.endm

.macro zero obj
	eq	\obj, #i0
	it	ne
	eqne	\obj, #f0
.endm

.macro zerop obj
	eq	\obj, #i0
	it	ne
	eqne	\obj, #f0
.endm

.macro zeropne obj
	eqne	\obj, #i0
	it	ne
	eqne	\obj, #f0
.endm

.macro anyzro obj1, obj2
	eq	\obj1, #i0
	it	ne
	eqne	\obj1, #f0
	it	ne
	eqne	\obj2, #i0
	it	ne
	eqne	\obj2, #f0
.endm

.macro isnan obj
	ldr	rva, =scheme_nan	@ rva <- nan
	eq	\obj, rva		@ is obj = nan ?
.endm
	
.macro anynan obj1, obj2
	ldr	rvc, =scheme_nan	@ rvc <- nan
	eq	\obj1, rvc		@ is obj1 = nan ?
	it	ne
	eqne	\obj2, rvc		@	if not, is obj2 = nan ?
.endm

.macro isinf obj
	ldr	rva, =scheme_inf	@ rva <- inf
	fabs	rvb, \obj		@ rvb <- obj without sign
	eq	rvb, rva		@ is x1 = +/-inf ?
.endm
	
.macro anyinf obj1, obj2
	ldr	rva, =scheme_inf	@ rva <- inf
	bic	rvb, \obj1, #0x80000000	@ rvb <- x1 without sign
	eq	rvb, rva		@ is x1 = +/-inf ?
	itT	ne
	bicne	rvb, \obj2, #0x80000000	@ rvb <- x1 without sign
	eqne	rvb, rva		@ is x1 = +/-inf ?
.endm
	
.macro plus dest, val1, val2
	add	\dest, \val1, \val2
	eor	\dest, #0x03
.endm

.macro plusne dest, val1, val2
	addne	\dest, \val1, \val2
	it	ne
	eorne	\dest, #0x03
.endm

.macro ngflt dest, val
	eor	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngflteq dest, val
	eoreq	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngfltne dest, val
	eorne	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngfltmi dest, val
	eormi	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngint dest, val
	mvn	\dest, \val
	add	\dest, \dest, #3
.endm

.macro nginteq dest, val
	mvneq	\dest, \val
	it	eq
	addeq	\dest, \dest, #3
.endm

.macro ngintne dest, val
	mvnne	\dest, \val
	it	ne
	addne	\dest, \dest, #3
.endm

.macro ngintmi dest, val
	mvnmi	\dest, \val
	it	mi
	addmi	\dest, \dest, #3
.endm

.macro postv num
	tst	\num, #0x80000000
.endm

.macro postveq num
	tsteq	\num, #0x80000000
.endm

.macro postvne num
	tstne	\num, #0x80000000
.endm

.macro iabs dest, val
	postv	\val
	itEE	eq
	seteq	\dest, \val
	mvnne	\dest, \val
	addne	\dest, \dest, #3
.endm

.macro fabs dest, val
	bic	\dest, \val, #0x80000000	@ \dest <- unsigned \val (scheme float)	
.endm

.macro numerat dest, num
	@ returns the numerator (scheme int) of a rational
	@ uses:	 rva, rvb
	snoc	rva, rvb, \num		@
	lsr	rva, rva, #2
	orr	rva, rva, rvb, lsl #30
	orr	\dest, rva, #int_tag
.endm
	
.macro nmrtreq dest, num
	@ returns the numerator (scheme int) of a rational
	@ uses:	 rva, rvb
	snoceq	rva, rvb, \num		@
	itTT	eq
	lsreq	rva, rva, #2
	orreq	rva, rva, rvb, lsl #30
	orreq	\dest, rva, #int_tag
.endm
	
.macro denom dest, num
	@ returns the denominator (scheme int) of a rational
	@ uses:	 rva
	cdr	rva, \num
	bic	rva, rva, #3
	orr	\dest, rva, #int_tag
.endm

.macro spltrat nmr, dnm, rat
	@ returns the numerator (scheme int) and denominator (scheme int) of a rational
	@ uses:	 rva, rvb
	snoc	rva, rvb, \rat		@
	lsr	rva, rva, #2
	orr	rva, rva, rvb, lsl #30	
	bic	rvb, rvb, #3
	orr	\nmr, rva, #int_tag
	orr	\dnm, rvb, #int_tag
.endm
	
.macro real dest, num
	@ returns the real part (scheme float) of a complex
	@ uses:	 rva, rvb
	snoc	rva, rvb, \num
	lsr	rva, rva, #2
	orr	\dest, rva, rvb, lsl #30
.endm
	
.macro imag dest, num
	@ returns the imaginary (scheme float) of a complex
	@ uses:	 rva
	cdr	rva, \num
	bic	rva, rva, #3
	orr	\dest, rva, #float_tag
.endm

.macro spltcpx real, imag, cpx
	@ returns the real part (scheme float) and imaginary part (scheme float) of a complex
	@ uses:	 rva, rvb
	snoc	rva, rvb, \cpx
	lsr	rva, rva, #2
	orr	\real, rva, rvb, lsl #30
	bic	rvb, rvb, #3
	orr	\imag, rvb, #float_tag
.endm
	
.macro ngnum dest, num
	tst	\num, #int_tag		@ is val a integer?
	itE	eq
	ngflteq	\dest, \num
	ngintne	\dest, \num
.endm

.macro fltmte exp, num
	@ convert float to 'in-place' mantissa and biased exponent
	@ on entry:	num <- float (scheme float)
	@ on exit:	num <- signed 'in-place' mantissa of input num (scheme int)
	@ on exit:	exp <- biased exponent of input num (raw int)
	@ modifies:	num, exp
	lsr	\exp, \num, #23			@ \exp  <- exponent and sign (raw)
	bic	\num, \num, \exp, lsl #23	@ \num <- mantis of \num (pseudo scheme float)
	eor	\num, \num, #0x03		@ \num <- mantissa of \num (scheme int)
	tst	\exp, #0xff			@ is exponent zero ?
	itEE	ne
	orrne	\num, \num, #0x00800000		@	if not, \num <- mantissa with 1. of normalization
	lsleq	\num, \num, #1			@	if so,  \num <- mantis shftd lft (psd scheme float)
	eoreq	\num, \num, #0x03		@	if so,  \num <- mantissa shifted left (scheme int)
	tst	\exp, #0x0100			@ is number positive?
	itTT	ne
	bicne	\exp, \exp, #0x0100		@	if not, \exp <- expon without sign of num (raw int)
	mvnne	\num, \num			@	if not, negate mantissa
	addne	\num, \num, #3			@	if not, negate mantissa
.endm

@
@ 6.3.2. Pairs and lists
@

.macro isave reg
	@ inlined version of save macro (below)
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is an 8-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {\reg, dts}	@ rva <- address of next free cell, and store reg, dts in free cell
	sub	dts, rva, #8		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro save reg
  .ifndef inline_cons
	bl	save			@ dts <- updated scheme stack with free car or 1st cell
	setcar	dts, \reg		@ update car of the updated dts
  .else
	isave	\reg
  .endif
.endm

.macro save2 reg1, reg2
	lcons	dts, \reg1, \reg2, dts
.endm

.macro save3 reg1, reg2, reg3
	llcons	dts, \reg1, \reg2, \reg3, dts
.endm

.macro restor reg
	ldmia	dts, {\reg, dts}
.endm

.macro restoreq reg
	ldmiaeq	dts, {\reg, dts}
.endm

.macro restorne reg
	ldmiane	dts, {\reg, dts}
.endm

.macro restorpl reg
	ldmiapl	dts, {\reg, dts}
.endm

.macro restormi reg
	ldmiami	dts, {\reg, dts}
.endm

.macro restor2 reg1, reg2
	ldmia	dts, {\reg1, dts}
	ldmia	dts, {\reg2, dts}
.endm

.macro restor2ne reg1, reg2
	ldmiane	dts, {\reg1, dts}
	ldmiane	dts, {\reg2, dts}
.endm

.macro restor3 reg1, reg2, reg3
	ldmia	dts, {\reg1, dts}
	ldmia	dts, {\reg2, dts}
	ldmia	dts, {\reg3, dts}
.endm

.macro icons dest, car, cdr
	@ inlined version
	@ dest <- (cons car cdr)
	@
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is an 8-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {\car, \cdr}	@ rva <- addr of next free cell, + store car-cdr in prior free cell
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro cons dest, car, cdr
	@
	@ dest <- (cons car cdr)
	@
	@ Note 0: car and cdr must be in increasing order, eg. sv1 and sv5
	@ Note 0+ rva, rvb and rvc cannot be dest, car or cdr
	@ Note 1: This is conditionally restartable when fre is at reservation level 1 (after bl cons)
	@ ------- condition is:	if
	@				lr_irq = [*restart critical instruction*]
	@			then
	@				do not restart
	@				execute this instr (save has been committed) and add 4 to lr_irq
	@			else
	@				 restart at cons: label
	@ Note 2: Code restart is handled in ISR that wishes to allocate memory
	@ Note 3: All level 1 code restart exceptions are based on same [*restart critical instruction*]
	@
  .ifndef inline_cons
	bl	cons			@ rva <- addr of fre cel (gc if ndd), rvb <- 8, fre-ptr rsrvd lvl 1
	stmia	rva!, {\car, \cdr}	@ rva <- addr of next free cell, + store car-cdr in prior free cell
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	icons	\dest, \car, \cdr
  .endif
.endm

.macro ibcons dest, bcar, bcdr, rest
	@ inlined version of bcons
	@ dest <- ((bcar . bcdr) . rest)
   .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
 	stmia	rva!, {\bcar,\bcdr,rvc}
	stmia	rva!, {\rest}
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro bcons dest, bcar, bcdr, rest
	@ dest <- ((bcar . bcdr) . rest)
  .ifndef inline_cons
	bl	cons2
	stmia	rva!, {\bcar,\bcdr,rvc}
	stmia	rva!, {\rest}
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	ibcons	\dest, \bcar, \bcdr, \rest
  .endif
.endm

.macro lcons dest, car, cdr, cddr
	@ dest <- (car . (cdr . cddr))
  .ifndef inline_cons
	bl	cons2
  .else
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
  .endif
	stmia	rva!, {\cdr,\cddr}
	stmia	rva!, {\car,rvc}
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro illcons dest, car, cdr, cddr, cdddr
	@ inlined version of llcons (below)
	@ dest <- (car . (cdr . (cddr . cdddr)))
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc24			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvb, rva
	add	rvc, rva, #8
	stmia	rva!, {\cddr,\cdddr}
	stmia	rva!, {\cdr}
	stmia	rva!, {rvb,\car,rvc}
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro llcons dest, car, cdr, cddr, cdddr
	@ dest <- (car . (cdr . (cddr . cdddr)))
  .ifndef inline_cons
	bl	cons3
	stmia	rva!, {\cddr,\cdddr}
	stmia	rva!, {\cdr}
	stmia	rva!, {rvb,\car,rvc}
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	illcons	\dest, \car, \cdr, \cddr, \cdddr
  .endif
.endm

.macro sav__c
	@ dts <- (cnt . dts)
  .ifndef inline_cons
	bl	sav__c
  .else
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop				@ <- lnk points here or at next instruction depending on alignment
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {cnt, dts}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .endif
.endm

.macro isav_ec
	@ inlined version of sav_ec (below)
	@ dts <- (env cnt . dts)
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	set	rvb, dts
	stmia	rva!, {cnt, rvb, env, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro sav_ec
	@ dts <- (env cnt . dts)
  .ifndef inline_cons
	bl	sav_ec
  .else
	isav_ec
  .endif
.endm

.macro sav_rc reg
	@ dts <- (reg cnt . dts)
  .ifndef inline_cons
	bl	sav_rc
	setcar	dts, \reg
  .else
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts}
	stmia	rva!, {\reg, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .endif
.endm

.macro savrec reg
	@ dts <- (reg env cnt . dts)
  .ifndef inline_cons
	bl	savrec
	setcar	dts, \reg
  .else
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc24			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	set	rvb, dts
	stmia	rva!, {cnt, rvb, env, rvc}
	sub	rvc, rva, #8
	stmia	rva!, {\reg, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .endif
.endm

.macro ilist dest, obj
	@ inlined version of list macro (below)
	@ dest <- (obj)  -- i.e. obj consed with #null
	@
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is an 8-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, #null
	stmia	rva!, {\obj, rvc}	@ rva <- addr of next free cell, + store car-cdr in prior free cell
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit list destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro list dest, obj
	@
	@ dest <- (obj)  -- i.e. obj consed with #null
	@
  .ifndef inline_cons
	bl	cons			@ rva <- addr of fre cel (gc if ndd), rvb <- 8, fre-ptr rsrvd lvl 1
	stmia	rva!, {\obj, rvc}	@ rva <- addr of next free cell, + store car-cdr in prior free cell
	sub	\dest, rva, #8		@ \dest <- address of cons cell, [*commit list destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	ilist	\dest, \obj
  .endif
.endm


.macro car dest, pair
	ldr	\dest,  [\pair]
.endm

.macro careq dest, pair
	ldreq	\dest,  [\pair]
.endm

.macro carne dest, pair
	ldrne	\dest,  [\pair]
.endm

.macro carmi dest, pair
	ldrmi	\dest,  [\pair]
.endm

.macro carpl dest, pair
	ldrpl	\dest,  [\pair]
.endm

.macro cdr dest, pair
	ldr	\dest,  [\pair, #4]
.endm

.macro cdreq dest, pair
	ldreq	\dest,  [\pair, #4]
.endm

.macro cdrne dest, pair
	ldrne	\dest,  [\pair, #4]
.endm

.macro cdrmi dest, pair
	ldrmi	\dest,  [\pair, #4]
.endm

.macro cdrhi dest, pair
	ldrhi	\dest,  [\pair, #4]
.endm

.macro cdrpl dest, pair
	ldrpl	\dest,  [\pair, #4]
.endm

.macro setcar pair, obj
	str	\obj,  [\pair]
.endm

.macro setcareq pair, obj
	streq	\obj,  [\pair]
.endm

.macro setcarne pair, obj
	strne	\obj,  [\pair]
.endm

.macro setcarmi pair, obj
	strmi	\obj,  [\pair]
.endm

.macro setcdr pair, obj
	str	\obj,  [\pair, #4]
.endm

.macro setcdreq pair, obj
	streq	\obj,  [\pair, #4]
.endm

.macro setcdrne pair, obj
	strne	\obj,  [\pair, #4]
.endm

.macro setcdrhi pair, obj
	strhi	\obj,  [\pair, #4]
.endm

.macro caar dest, pair
	car	\dest,  \pair
	car	\dest,  \dest
.endm

.macro caareq dest, pair
	careq	\dest,  \pair
	careq	\dest,  \dest
.endm

.macro caarne dest, pair
	carne	\dest,  \pair
	carne	\dest,  \dest
.endm

.macro cadr dest, pair
	cdr	\dest,  \pair
	car	\dest,  \dest
.endm

.macro cadrne dest, pair
	cdrne	\dest,  \pair
	it	ne
	carne	\dest,  \dest
.endm

.macro cdar dest, pair
	car	\dest,  \pair
	cdr	\dest,  \dest
.endm

.macro cdarne dest, pair
	carne	\dest,  \pair
	it	ne
	cdrne	\dest,  \dest
.endm

.macro cdarpl dest, pair
	carpl	\dest,  \pair
	cdrpl	\dest,  \dest
.endm

.macro cddr dest, pair
	cdr	\dest,  \pair
	cdr	\dest,  \dest
.endm

.macro caaar dest, pair
	car	\dest,  \pair
	car	\dest,  \dest
	car	\dest,  \dest
.endm

.macro cadar dest, pair
	car	\dest,  \pair
	cdr	\dest,  \dest
	car	\dest,  \dest
.endm

.macro cadarne dest, pair
	carne	\dest,  \pair
	itT	ne
	cdrne	\dest,  \dest
	carne	\dest,  \dest
.endm

.macro caddr dest, pair
	cdr	\dest,  \pair
	cdr	\dest,  \dest
	car	\dest,  \dest
.endm

.macro caddrne dest, pair
	cdrne	\dest,  \pair
	itT	ne
	cdrne	\dest,  \dest
	carne	\dest,  \dest
.endm

.macro cdaar dest, pair
	car	\dest,  \pair
	car	\dest,  \dest
	cdr	\dest,  \dest
.endm

.macro cdddr dest, pair
	cdr	\dest,  \pair
	cdr	\dest,  \dest
	cdr	\dest,  \dest
.endm

@
@ 6.3.5. Strings
@

.macro straloc dest, size
	@
	@ dest <- (make-string size)
	@
	@ dest and size must be different registers and not rva, rvb
	@
	@ align the number of bytes to allocate
	lsr	rvb, \size, #2		@ rvb <- #bytes to allocate for data
	add	rvb, rvb, #4		@ rvb <- #bytes to allocate + header's 4
	@ allocate the aligned object
	bl	zmaloc			@ rva <- addr of object (symbol-taggd), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	\dest, rva, rvb		@ \dest <- address of string (symbl), [*commit string destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@ update the object's tag for actual size and type (string)
	lsl	rva, \size, #6
	orr	rva, rva, #string_tag
	str	rva, [\dest]		@ update string tag
.endm

.macro strlen dest, string
	ldr	rvb, [\string]
	lsr	\dest, rvb, #6
.endm

.macro strref char, string, position
	add	\position,  \position, #16
.ifndef	cortex
	ldrb	rva, [\string,  \position, ASR #2]
.else
	asr	rva, \position, #2
	ldrb	rva, [\string, rva]
.endif
	lsl	rva, rva, #8
	orr	\char, rva, #char_tag
	sub	\position,  \position, #16
.endm

.macro strrefi char, string, position
	ldrb	rva, [\string,  #4+\position]
	lsl	rva, rva, #8
	orr	\char, rva, #char_tag
.endm

.macro strfi char, string, position
	ldrb	rva, [\string,  #4+\position]
	raw2chr	\char, rva
.endm

.macro strfieq char, string, position
	ldrbeq	rva, [\string,  #4+\position]
	it	eq
	raw2chreq \char, rva
.endm

.macro strset char, string, position
	@ modifies rva, rvc
	add	rvc, \position, #16
	lsr	rva, \char, #8
  .ifndef cortex
	strb	rva, [\string, rvc, ASR #2]
  .else
	asr	rvc, rvc, #2
	strb	rva, [\string, rvc]
  .endif
.endm

@
@ 6.3.6. Vectors
@

.macro veclen dest, vector
	ldr	rvb, [\vector]
	lsr	\dest, rvb, #6
.endm

.macro vecleneq dest, vector
	ldreq	rvb, [\vector]
	it	eq
	lsreq	\dest, rvb, #6
.endm

.macro veclenne dest, vector
	ldrne	rvb, [\vector]
	it	ne
	lsrne	\dest, rvb, #6
.endm

.macro vcrfi dest, vector, position
	ldr	\dest,  [\vector,  #4+4*\position]	
.endm

.macro vcrfieq dest, vector, position
	ldreq	\dest,  [\vector,  #4+4*\position]	
.endm

.macro vcrfine dest, vector, position
	ldrne	\dest,  [\vector,  #4+4*\position]	
.endm

.macro vcrfimi dest, vector, position
	ldrmi	\dest,  [\vector,  #4+4*\position]	
.endm

.macro vcrfipl dest, vector, position
	ldrpl	\dest,  [\vector,  #4+4*\position]	
.endm

.macro vcrfihi dest, vector, position
	ldrhi	\dest,  [\vector,  #4+4*\position]	
.endm

.macro vcsti vector, position, obj
	str	\obj,  [\vector,  #4+4*\position]	
.endm

.macro vcstieq vector, position, obj
	streq	\obj,  [\vector,  #4+4*\position]	
.endm

.macro vcstine vector, position, obj
	strne	\obj,  [\vector,  #4+4*\position]	
.endm

.macro vcstipl vector, position, obj
	strpl	\obj,  [\vector,  #4+4*\position]	
.endm

.macro vcstihi vector, position, obj
	strhi	\obj,  [\vector,  #4+4*\position]	
.endm

.macro vcstimi vector, position, obj
	strmi	\obj,  [\vector,  #4+4*\position]	
.endm

@
@ bytevectors
@
			
.macro vu8aloc dest, size
	@
	@ dest <- (make-bytevector size)
	@
	@ dest and size must be different registers and not rva, rvb
	@
	@ align the number of bytes to allocate
	lsr	rvb, \size, #2		@ rvb <- #bytes to allocate for data
	add	rvb, rvb, #4		@ rvb <- #bytes to allocate + header's 4
	@ allocate the aligned object
	bl	zmaloc			@ rva <- addr of object (symbol-taggd), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	\dest, rva, rvb		@ \dest <- address of string (symbl), [*commit string destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@ update the object's tag for actual size and type (bytevector)
	lsl	rva, \size, #6
	orr	rva, rva, #bytevector_tag
	str	rva, [\dest]		@ update tag
.endm

.macro vu8len dest, vu8
	ldr	rvb, [\vu8]
	lsr	\dest, rvb, #6
.endm

.macro vu8ref octet, vu8, position
	add	\position,  \position, #16
.ifndef	cortex
	ldrb	rva, [\vu8,  \position, ASR #2]
.else
	asr	rva, \position, #2
	ldrb	rva, [\vu8, rva]
.endif
	lsl	rva, rva, #2
	orr	\octet, rva, #i0
	sub	\position,  \position, #16
.endm

.macro vu8set vu8, position, octet
	@ modifies rva, rvc
	add	rvc, \position, #16
	lsr	rva, \octet, #2
  .ifndef cortex
	strb	rva, [\vu8, rvc, ASR #2]
  .else
	asr	rvc, rvc, #2
	strb	rva, [\vu8, rvc]
  .endif
.endm

@
@ word (table) and byte references
@	
	
.macro tbrfi reg, table, position
	ldr	\reg,  [\table,  #4*\position]	
.endm

.macro tbrfieq reg, table, position
	ldreq	\reg,  [\table,  #4*\position]	
.endm

.macro tbrfine reg, table, position
	ldrne	\reg,  [\table,  #4*\position]	
.endm

.macro tbrfimi reg, table, position
	ldrmi	\reg,  [\table,  #4*\position]	
.endm

.macro tbsti reg, table, position
	str	\reg,  [\table,  #4*\position]	
.endm

.macro tbstieq reg, table, position
	streq	\reg,  [\table,  #4*\position]	
.endm

.macro tbstine reg, table, position
	strne	\reg,  [\table,  #4*\position]	
.endm

.macro bytref reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrb	\reg,  [\array,  \position, ASR #2 ]
.else
	asr	\reg, \position, #2
	ldrb	\reg,  [\array, \reg]
.endif
.endm

.macro bytrefeq reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrbeq	\reg,  [\array,  \position, ASR #2 ]
.else
	asreq	\reg, \position, #2
	it	eq
	ldrbeq	\reg,  [\array, \reg]
.endif
.endm

.macro bytrefne reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrbne	\reg,  [\array,  \position, ASR #2 ]
.else
	asrne	\reg, \position, #2
	it	ne
	ldrbne	\reg,  [\array, \reg]
.endif
.endm

.macro bytrefmi reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrbmi	\reg,  [\array,  \position, ASR #2 ]
.else
	asrmi	\reg, \position, #2
	it	mi
	ldrbmi	\reg,  [\array, \reg]
.endif
.endm

.macro bytset reg, array, position	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strb	\reg,  [\array,  \position, ASR #2 ]
  .else
	asr	rvc, \position, #2
	strb	\reg, [\array, rvc]
  .endif
.endm

.macro bytseteq reg, array, position	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strbeq	\reg,  [\array,  \position, ASR #2 ]
  .else
	asreq	rvc, \position, #2
	it	eq
	strbeq	\reg, [\array, rvc]
  .endif
.endm

.macro bytsetne reg, array, position	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strbne	\reg,  [\array,  \position, ASR #2 ]
  .else
	asrne	rvc, \position, #2
	it	ne
	strbne	\reg, [\array, rvc]
  .endif
.endm

.macro bytsetmi reg, array, position	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strbmi	\reg,  [\array,  \position, ASR #2 ]
  .else
	asrmi	rvc, \position, #2
	it	mi
	strbmi	\reg, [\array, rvc]
  .endif
.endm

.macro bytsetu reg, array, position	@ the 3 registers should be different
	@ used only within irq-disabled code zones
	@ or if position is in rva-rvc
	@ (does not disable/enable interrupts on cortex)
.ifndef	cortex
	strb	\reg,  [\array,  \position, ASR #2 ]
.else
	asr	\position, \position, #2	@ - not gc safe !!!! (needs no irq or pos in rva-rvc)
	strb	\reg,  [\array, \position]	@ - not gc safe !!!! (needs no irq or pos in rva-rvc)
	lsl	\position, \position, #2	@ - not gc safe !!!! (needs no irq or pos in rva-rvc)
	orr	\position, \position, #int_tag	@ - not gc safe !!!! (needs no irq or pos in rva-rvc)
.endif
.endm

.macro wrdref reg, array, position	
	@ the 3 registers should be different
	@ reg (first reg) should be rva-rvc (raw value obtained)
  .ifndef cortex
	ldr	\reg,  [\array,  \position, ASR #2 ]
  .else
	asr	\reg, \position, #2	@ reg could be not gc safe here (if not rva or rvb)
	ldr	\reg,  [\array, \reg]
  .endif
.endm

.macro wrdst reg, array, position	
	@ array and position should be different registers,
	@ array should not be rvc
	@ modifies rvc on cortex-m3
  .ifndef cortex
	str	\reg,  [\array,  \position, ASR #2 ]
  .else
	asr	rvc, \position, #2
	str	\reg,  [\array, rvc]
  .endif
.endm


@
@ Addendum: Pair splitting
@

.macro snoc car, cdr, pair
	ldmia	\pair, {\car, \cdr}
.endm

.macro snoceq car, cdr, pair
	ldmiaeq	\pair, {\car, \cdr}
.endm

.macro snocne car, cdr, pair
	ldmiane	\pair, {\car, \cdr}
.endm

.macro snocpl car, cdr, pair
	ldmiapl	\pair, {\car, \cdr}
.endm

@
@ Addendum: Type analysis
@

.macro charp reg
	and	rva, \reg, #0xFF
	eq	rva, #char_tag
.endm

.macro nmbrp reg
	@ raise eq flag if reg contains a rational, complex, int or float
	@ uses rva
	pntrp	\reg
	itTT	eq
	careq	rva, \reg
	andeq	rva, rva, #0x07		@ rva <- two-bit tag of object in num
	eqeq	rva, #0x03		@ is object a rational or complex?
	itT	ne
	andne	rva, \reg,  #0x03	@ rva <- two-bit tag of object in num
	eqne	rva, #int_tag		@ is object an integer?
	it	ne
	eqne	rva, #float_tag		@	if not, is object a float?
.endm

.macro ratcpx reg
	@ raise eq flag if reg contains a rational or complex
	@ uses rva
	pntrp	\reg
	itTT	eq
	careq	rva, \reg
	andeq	rva, rva, #0x07		@ rva <- two-bit tag of object in num
	eqeq	rva, #0x03		@ is object a rational or complex?
.endm

.macro varp reg
	@ raise eq flag if reg contains a variable or syntax item
	@ uses rva
	and	rva, \reg, #0xDF
	eq	rva, #0x8F
.endm

.macro tagdp reg
	@ raise eq flag if reg points to a tagged item (string, vector, procedure, continuation, ...)
	@ uses rva
	pntrp	\reg
	itTT	eq
	careq	rva, \reg
	andeq	rva, rva, #0x47
	eqeq	rva, #0x47
.endm

.macro sizdp reg
	@ raise eq flag if reg points to a tagged-sized item (string, symbol, vector, ...)
	@ uses rva
	pntrp	\reg
	itTT	eq
	careq	rva, \reg
	andeq	rva, rva, #0xCF
	eqeq	rva, #0x4F
.endm

.macro vctrp reg
	@ raise eq flag if reg points to a vector
	@ uses rva
	pntrp	\reg
	itTT	eq
	careq	rva, \reg
	andeq	rva, rva, #0xFF
	eqeq	rva, #vector_tag
.endm

.macro pntrp reg
	@ raise eq flag if reg contains a pointer
	tst	\reg,  #0x03
.endm

.macro pntrpeq reg
	tsteq	\reg,  #0x03
.endm

.macro pntrpne reg
	tstne	\reg,  #0x03
.endm

.macro macrop reg
	car	rva, \reg
	eq	rva, #macro		@ is tag in reg a macro tag?
.endm

.macro execp reg
	pntrp	\reg
	itTT	eq
	careq	rva, \reg
	andeq	rva, rva, #0xCF
	eqeq	rva, #0xCF		@ is fun a primitive, procedure or continuation?
.endm

.macro qpair0 reg
	@ raise eq flag if reg is an atom, sized item or rat/cpx
	@ modifies:	 rva
	pntrp	\reg
	itTE	eq
	careq	rva, \reg
	andeq	rva, rva, #0xcf
	setne	rva, #0x4f
	eq	rva, #0x4f		@ is it a sized item?
	itT	ne
	andne	rva, rva, #0x07
	eqne	rva, #0x03		@	if not, is it a rat/cpx?
.endm

.macro qpair2 reg
	@ raise eq flag if reg is an atom, sized item, rat/cpx, syntax or assembled code
	@ modifies:	 rva, rvb
	pntrp	\reg			@ is it a pointer?
	itTE	eq
	careq	rvb, \reg		@	if so,  rvb <- tag or item
	andeq	rva, rvb, #0xcf		@	if so,  rva <- tag masked for sized-item vs proc
	setne	rva, #0x4f		@	if not, rva <- vector-tag (to raise eq flag below)
	eq	rva, #0xcf		@ is it a procedure?
	itTTT	eq
	mvneq	rvb, rvb		@	if so,  rvb <- inverted tag
	andeq	rvb, rvb, #0x4000	@	if so,  rvb <- lambda bit, isolated (inverted)
	lsreq	rvb, rvb, #7		@	if so,  rvb <- lambda bit, shifted
	biceq	rva, rva, rvb		@	if so,  rva <- tag identifying assembled code vs lambda
	eq	rva, #0x4f		@ is it a sized item, non-pointer, or assembled code?
	itT	ne
	andne	rva, rva, #0x07		@	if not, rva <- 3-bit tag
	eqne	rva, #0x03		@	if not, is it a rat/cpx?
.endm

.macro tuckd datast, reg, datast2
	restor	sv2			@ sv2 <- item-at-top-of-stack, dts <- (...)
	save2	sv2, sv1		@ dts <- (item-at-top-of-stack new-item ...)
.endm

.macro tuck reg, tmp
	@ on entry:	dts <- (item1 item2 ...)
	@ on exit:	dts <- (item1 reg item2 ...)
	@ on exit:	tmp <- item1
	restor	\tmp			@ tmp <- item1, dts <- (item2 ...)
	save2	\tmp, \reg		@ dts <- (item1 reg item2 ...)
.endm

@
@ EVAL
@

.macro	evalsv1
	and	rva, sv1, #0xff
	eq	rva, #variable_tag
	it	eq
	bleq	sbevlv
	pntrp	rva
	itTE	eq
	careq	rva, sv1
	andeq	rva, rva, #0x47
	setne	rva, #0x47
	eq	rva, #0x47
	it	ne
	blne	sbevll	
.endm

@
@ Addendum: Calling and branching to scheme functions or labels
@

.macro	call label
.ifndef	cortex
	set	cnt, pc			@ cnt <- instruction after next
	b	\label
.else
	add	cnt, pc, #4		@ cnt <- instruction after next (16-bit instruction)
	b	\label			@ (16 or 32-bit instruction)
	nop				@ <- cnt points here, or at next instruction for 32-bit branch
	nop				@ <- cnt points here, or at next instruction for 16-bit branch
.endif	
.endm

.macro	calla reg
.ifndef	cortex
	set	cnt, pc			@ cnt <- instruction after next
	set	pc,  \reg
.else
	add	cnt, pc, #4		@ cnt <- instruction after next (16-bit instruction)
	set	pc,  \reg		@ (16-bit instruction) (note: *add cnt, pc, #2* is 32 bit)
	nop
	nop				@ <- cnt points here, or at next instruction
.endif	
.endm

.macro	nullp reg
	eq	\reg, #null
.endm

.macro	nullpeq reg
	eqeq	\reg, #null
.endm

.macro	nullpne reg
	eqne	\reg, #null
.endm

.macro	izerop reg
	eq	\reg, #i0
.endm

.macro	vecref res, vec, pos
	bic	rva, \pos, #0x03
	add	rva, rva, #4
	ldr	\res, [\vec, rva]
.endm
	
.macro	vecset vec, pos, val
	@ modifies rva
	bic	rva, \pos, #0x03
	add	rva, rva, #4
	str	\val, [\vec, rva]
.endm
	
.macro	int2raw raw, int
	asr	\raw, \int, #2
.endm

.macro	int2raweq raw, int
	asreq	\raw, \int, #2
.endm

.macro	int2rawne raw, int
	asrne	\raw, \int, #2
.endm

.macro	int2rawmi raw, int
	asrmi	\raw, \int, #2
.endm

.macro	raw2int int, raw  @ <- target (int) and source (raw) must be different regs
	set	\int, #int_tag
	orr	\int, \int, \raw, LSL #2
.endm
	
.macro	raw2inteq int, raw
	seteq	\int, #int_tag
	orreq	\int, \int, \raw, LSL #2
.endm
	
.macro	raw2intne int, raw
	setne	\int, #int_tag
	orrne	\int, \int, \raw, LSL #2
.endm
	
.macro	raw2intmi int, raw
	setmi	\int, #int_tag
	it	mi
	orrmi	\int, \int, \raw, LSL #2
.endm
	
.macro	chr2raw raw, chr
	lsr	\raw, \chr, #8
.endm

.macro	raw2chr chr, raw
	set	\chr, #char_tag
	orr	\chr, \chr, \raw, LSL #8
.endm
	
.macro	raw2chreq chr, raw
	seteq	\chr, #char_tag
	it	eq
	orreq	\chr, \chr, \raw, LSL #8
.endm
	
@ ------------------------------------------------------------
@ swap macros
@ ------------------------------------------------------------
	
.macro swap reg1, reg2, temp
	set	\temp,  \reg1
	set	\reg1,  \reg2
	set	\reg2,  \temp
.endm

.macro swapmi reg1, reg2, temp
	setmi	\temp,  \reg1
	itT	mi
	setmi	\reg1,  \reg2
	setmi	\reg2,  \temp
.endm

@ ------------------------------------------------------------
@ enabling IRQ in VIC, ISR entry, clearing, exit compatibility
@ ------------------------------------------------------------

.macro	enable_VIC_IRQ

.ifndef	cortex	@ arm7tdmi, arm920t
  .ifndef LPC_2800	@ NOT lpc 2800
    .ifndef S3C24xx
      .ifndef OMAP_35xx
	ldr	rva, =int_base		@ rva <- address of interrupt enable register
	ldr	rvb, =scheme_ints_enb	@ rvb <- scheme interrupts
	str	rvb, [rva, #int_enable]	@ enable scheme interrupts
      .endif
    .endif
  .endif
  .ifdef STR_9xx	@ enable VIC1 interrupts (in addition to VIC0 enabled above)
	ldr	rva, =int_base2		@ rva <- address of interrupt enable register (16-31)
	ldr	rvb, =scheme_ints_en2	@ rvb <- scheme interrupts (16-31)
	str	rvb, [rva, #int_enable]	@ enable scheme interrupts (16-31)
  .endif
  .ifdef S3C24xx
	set	rvb, #0x00
	mvn	rvb, rvb
	ldr	rva, =scheme_ints_enb	@ rvb <- scheme interrupts
	bic	rvb, rvb, rva
	ldr	rva, =int_base
	str	rvb, [rva, #int_enable]	@ enable uart 0, and timer 0, 1 and usb ints in INTMSK
  .endif
  .ifdef LPC_2800
	ldr	rva, =0x80300400	@ rva <- Interrupt Request Registers base address INT_REQ
	ldr	rvb, =0x1C010001	@ rvb <- interrupt enable
	str	rvb, [rva, #0x14]	@ INT_REQ5  <- Timer 0 zero Count interrupt enabled as IRQ
	str	rvb, [rva, #0x18]	@ INT_REQ6  <- Timer 1 zero Count interrupt enabled as IRQ
	str	rvb, [rva, #0x34]	@ INT_REQ13 <- I2C interrupt enabled as IRQ
  .endif
.else	@ cortex-m3
	ldr	rva, =int_en_base	@ rva <- address of interrupt enable register
	ldr	sv3, =BUFFER_START
	vcrfi	rvb, sv3, CTX_EI_offset	@ rvb <- enabled scheme interrupts  0-31
	str	rvb, [rva, #int_enabl1]	@ enable scheme interrupts  0-31
	vcrfi	rvb, sv3, CTX_EI_offset+4 @ rvb <- enabled scheme interrupts  32-63
	str	rvb, [rva, #int_enabl2]	  @ enable scheme interrupts
  .if num_interrupts > 64
	vcrfi	rvb, sv3, CTX_EI_offset+8 @ rvb <- enabled scheme interrupts  64-95
	str	rvb, [rva, #int_enabl3]	  @ enable scheme interrupts
  .endif
.endif	@ cortex
	
.endm

.macro	enterisr
.ifndef	cortex	
	@ enterisr for non-cortex-m3
	sub	lnk, lnk, #4		@ Adjust lnk to point to return 
	stmdb	sp!, {lnk}		@ store lnk_irq (pc_usr) on irq stack
	mrs	lnk, spsr		@ lnk  <- spsr
	tst	lnk, #IRQ_disable	@ were interrupts disabled?
	ldmiane	sp!, {pc}^		@ If so, just return immediately
	@ save some registers on stack
	stmib	sp,  {lnk}		@ store spsr on irq stack (above lnk_irq/pc_usr)
	stmdb	sp,  {fre, cnt, rva, rvb, rvc, lnk}^	@ store 5 regs and lnk_usr on irq stack
	sub	sp,  sp, #24		@ sp  <- adjusted stack pointer (next free cell)
	ldr	rvb, =int_base		@ rvb <- address of VIC IRQ Status register
  .ifdef STR_7xx
	ldr	rvc, [rvb, #0x18]	@ rvc <- EIC_IVR (indicates start of interrupt processing)
	ldr	rvb, [rvb, #0x04]	@ rvb <- asserted interrupt, EIC_CICR
  .endif
  .ifdef AT91_SAM7
	ldr	rvc, [rvb, #0x00]	@ rvc <- AIC_IVR (indicates start of interrupt processing)
	ldr	rvb, [rvb, #8]		@ rvb <- asserted interrupt, AIC_ISR
  .endif
  .ifdef AT91_SAM9
	ldr	rvc, [rvb, #0x00]	@ rvc <- AIC_IVR (indicates start of interrupt processing)
	ldr	rvb, [rvb, #8]		@ rvb <- asserted interrupt, AIC_ISR
  .endif
  .ifdef S3C24xx
	ldr	rvb, [rvb, #0x14]	@ rvb <- asserted interrupt, INTOFFSET1
  .endif
  .ifdef OMAP_35xx
	ldr	rvb, [rvb, #0x40]	@ rvb <- asserted interrupt, INTCPS_SIR_IRQ
	and	rvb, rvb, #0x7f
  .endif
  .ifdef LPC_2800
	@ LPC-2800
	ldr	rvc, [rvb]		@ rvc <- [int_priomask0]
	ldr	rvc, [rvb, #0x0100]	@ rvc <- [int_vector0]
	ldr	rvb, [rvb, #int_status]	@ rvb <- asserted interrupts
	lsr	rvb, rvc, #3		@ rvb <- interrupt number + address bits
	and	rvb, rvb, #0x1f		@ rvb <- interrupt number
  .endif
  .ifdef LPC_2000
	ldr	rvc, [rvb, #int_status]	@ rvb <- asserted interrupts
	set	rvb, #0
	lsrs	rva, rvc, #16
	addne	rvb, rvb, #16
	setne	rvc, rva
	lsrs	rva, rvc, #8
	addne	rvb, rvb, #8
	setne	rvc, rva
	lsrs	rva, rvc, #4
	addne	rvb, rvb, #4
	setne	rvc, rva
	lsrs	rva, rvc, #2
	addne	rvb, rvb, #2
	setne	rvc, rva
	add	rvb, rvb, rvc, lsr #1
  .endif
  .ifdef  STR_9xx	@ dual 16-int VICs
	ldr	rvc, [rvb, #int_status]	@ rvc <- asserted interrupts ( 0-15)
	set	rvb, #0
	eq	rvc, #0
	ldreq	rvb, =int_base2
	ldreq	rvc, [rvb, #int_status]	@ rvc <- asserted interrupts (33-64)
	seteq	rvb, #16
	tst	rvc, #0xff		@ is int in 0-7 or 16-23?
	addeq	rvb, rvb, #8		@	if not, rvb <- int num + 8
	lsreq	rvc, rvc, #8		@	if not, rvc <- ints, shifted
	tst	rvc, #0x0f		@ is int in 0-3, 8-11, 16-19 or 24-27?
	addeq	rvb, rvb, #4		@	if not, rvb <- int num + 4
	lsreq	rvc, rvc, #4		@	if not, rvc <- ints, shifted
	tst	rvc, #0x03		@ is int in 0-1, 4-5, 8-9, 12-13, 16-17, 20-21, 24-25 or 28-29?
	addeq	rvb, rvb, #2		@	if not, rvb <- int num + 2
	lsreq	rvc, rvc, #2		@	if not, rvc <- ints, shifted
	tst	rvc, #0x01		@ is int even?
	addeq	rvb, rvb, #1		@	if not, rvb <- updated interrupt number
  .endif
  .ifdef  EP_93xx	@ dual 32-int VICs
	ldr	rvc, [rvb, #int_status]	@ rvc <- asserted interrupts (1-32)
	set	rvb, #0
	eq	rvc, #0
	ldreq	rvb, =int_base2
	ldreq	rvc, [rvb, #int_status]	@ rvc <- asserted interrupts (33-64)
	seteq	rvb, #32
	ldr	rva, =0xffff		@ rva <- initial mask
	tst	rvc, rva		@ is int num between 0 and 15?
	addeq	rvb, rvb, #16		@	if not, rvb <- int num + 16
	lsreq	rvc, rvc, #16		@	if not, rvc <- ints, shifted
	tst	rvc, #0xff		@ is int in 0-7 or 16-23?
	addeq	rvb, rvb, #8		@	if not, rvb <- int num + 8
	lsreq	rvc, rvc, #8		@	if not, rvc <- ints, shifted
	tst	rvc, #0x0f		@ is int in 0-3, 8-11, 16-19 or 24-27?
	addeq	rvb, rvb, #4		@	if not, rvb <- int num + 4
	lsreq	rvc, rvc, #4		@	if not, rvc <- ints, shifted
	tst	rvc, #0x03		@ is int in 0-1, 4-5, 8-9, 12-13, 16-17, 20-21, 24-25 or 28-29?
	addeq	rvb, rvb, #2		@	if not, rvb <- int num + 2
	lsreq	rvc, rvc, #2		@	if not, rvc <- ints, shifted
	tst	rvc, #0x01		@ is int even?
	addeq	rvb, rvb, #1		@	if not, rvb <- updated interrupt number
  .endif
.else	
	@ enterisr for cortex-m3
	mrs	sp,  psp		@ sp  <- psp stack
	@ *** Workaround for Cortex-M3 errata bug #382859, Category 2, present in r0p0, fixed in r1p0
	@ *** affects LM3S1968 (needed for multitasking)
	ldr	rvb, [sp, #28]		@ rvb <- saved xPSR
	ldr	rva, =0x0600000c	@ rva <- bit mask to identify if interruptd instruction was ldm/stm
	tst	rvb, rva		@ was interruted instruction ldm/stm?
	itT	eq
	biceq	rvb, rvb, #0xf0		@	if so,  rvb <- xPSR set to restart (not continue) ldm/stm
	streq	rvb, [sp, #28]		@	if so,  store xPSR back on stack
	@ *** end of workaround
	ldr	rvc, =0xe000ed00
	ldr	rvb, [rvc, #4]
	set	rvc, #0xff
	orr	rvc, rvc, #0x0100
	and	rvb, rvb, rvc
	sub	rvb, rvb, #16
.endif
	
.endm

	
.macro	clearUartInt	@ clear interrupt in uart with base address in rva
  .ifndef cortex
    .ifndef S3C24xx
      .ifndef STR_9xx
	ldr	cnt, [rva, #uart_istat]	@ cnt <- interrupt status (clears UART interrupt on LPC2000)
      .endif
    .endif
    .ifdef STR_9xx
	ldr	cnt, [rva, #uart_istat]	@ cnt <- interrupt status (clears UART interrupt on LPC2000)
	str	cnt, [rva, #uart_iclear]
    .endif
    .ifdef S3C24xx	
	ldr	rvc, [rva, #0x14]	@ cnt <- error status (clears potential overrun and frame errors)
	ldr	rva, =int_base		@ clear interrupt
	ldr	rvc, [rva, #0x18]	@ get sub-int in SUBSRCPND
	str	rvc, [rva, #0x18]	@ clear Rx sub-int in SUBSRCPND
    .endif
  .else @ cortex
    .ifdef LPC_17xx
	ldr	cnt, [rva, #uart_istat]	@ cnt <- interrupt status (clears UART interrupt on LPC17xx)
    .endif
    .ifdef AT91_SAM3S
	ldr	cnt, [rva, #uart_istat]	@ cnt <- interrupt status (clears UART interrupt)
    .endif
  .endif
.endm

.macro	clearTimerInt	@ clear interrupt in timer peripheral block with base address in rva
  .ifndef LPC_2800
    .ifndef S3C24xx	
     .ifndef OMAP_35xx	
      .ifndef STR_9xx
	ldr	rvc, [rva, #timer_istat]@ at91sam7
	str	rvc, [rva, #timer_iset]	@ lpc2000
	set	rvc, #0			@ rvc <- 0
	str	rvc, [rva, #timer_iset]	@ str711, STM32
       .endif
      .endif
    .endif
    .ifdef OMAP_35xx	
	ldr	rvc, [rva, #timer_istat]@ at91sam7
	str	rvc, [rva, #timer_iset]	@ lpc2000
	set	rvc, #0			@ rvc <- 0
	str	rvc, [rva, #timer_iset]	@ str711, STM32
    .endif
    .ifdef STR_7xx
	str	rvc, [rva, #timer_ctrl]	@ stop timer
    .endif
    .ifdef STR_9xx
	set	rvc, #0			@ rvc <- 0
	str	rvc, [rva, #timer_ctrl]	@ stop timer
	str	rvc, [rva, #0x10]	@ reset count
	str	rvc, [rva, #timer_iset]	@ clear flags
    .endif
  .else	@ LPC_2800
	set	rvc, #0			@ rvc <- 0
	str	rvc, [rva, #timer_ctrl]	@ stop the timer
	str	rvc, [rva]		@ clear the load value
	str	rvc, [rva, #timer_iset]	@ clear the interrupt
  .endif
.endm

.macro	clearVicInt	@ clear interrupt in interrupt vector (if needed)
  .ifndef cortex
    .ifndef S3C24xx	
      .ifndef OMAP_35xx	
        .ifndef STR_7xx
          .ifndef STR_9xx
	ldr	rva, =int_base		@ 
	ldr	rvc, =int_clear_vals
	str	rvc, [rva, #int_clear]	@ clear interrupt
	  .endif
        .endif
      .endif
    .endif
    .ifdef STR_7xx
	ldr	rva, =int_base		@ 
	ldr	rvc, [rva, #0x04]
	set	rva, #1
	lsl	rvc, rva, rvc
	ldr	rva, =int_base		@ 
	str	rvc, [rva, #int_clear]	@ clear interrupt
    .endif
    .ifdef STR_9xx
    .endif
    .ifdef S3C24xx	
	ldr	rva, =int_base		@ 
	ldr	rvc, [rva, #int_status]	@ rvc <- asserted interrupt bit
	str	rvc, [rva, #int_clear]	@ clear int in SRCPND
	str	rvc, [rva, #int_status]	@ clear int in INTPND
    .endif
    .ifdef OMAP_35xx	
	ldr	rva, =int_base		@ 
	ldr	rvc, =int_clear_vals
	str	rvc, [rva, #int_clear]
	dsb				@ data sync barrier (ensure interrupt cleared before proceeding)
    .endif
  .endif
.endm
	
.macro	exitisr
  .ifndef cortex
	@ exitisr for non-cortex
	ldr	rvc, [sp,  #28]
	msr	spsr_cxsf, rvc		@ restore spsr
    .ifndef STR_7xx
      .ifndef  AT91_SAM7
	ldmia	sp, {fre, cnt, rva, rvb, rvc, lnk}^	@ Restore registers
	add	sp, sp, #24
	ldmia	sp!, {lnk}
	movs	pc,  lnk		@ Return
      .endif
    .endif
    .ifdef STR_7xx
	ldmia	sp!, {fre, cnt, rva, rvb, rvc, lnk, pc}^ @ Restore regs (STR7 USB doesn't like other meth)
    .endif
    .ifdef  AT91_SAM7
	ldmia	sp!, {fre, cnt, rva, rvb, rvc, lnk, pc}^ @ Restore regs (USB meddles with SPI otherwise)
    .endif
  .else	
	@ exitisr for cortex
	ldr	pc,  =0xfffffffd	@ return to thread mode, use process stack
  .endif
.endm

.macro	isrexit
	@ second version - different from exitisr for STR7 and AT91SAM7 only
  .ifndef cortex	
	@ isr exit for non- cortex
	ldr	rvc, [sp,  #28]
	msr	spsr_cxsf, rvc		@ restore spsr
	ldmia	sp, {fre, cnt, rva, rvb, rvc, lnk}^	@ Restore registers
	add	sp, sp, #24
	ldmia	sp!, {lnk}
	movs	pc,  lnk		@ Return
  .else	
	@ isr exit for cortex
	ldr	pc,  =0xfffffffd	@ return to thread mode, use process stack
  .endif
.endm

