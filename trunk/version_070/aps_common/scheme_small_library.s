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

	STARTSUBOBAENV library

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				R3RS
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.1.	conditionals:			and, or
@	4.2.2.	binding constructs:		let, let*, letrec
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/


	/* (and exp1 exp2 ...) */
	PRIMIT	"and", sntx, 0
	@ in:	sv1 <- (exp1 exp2 ...),		sv5<- env
	@ out:	sv1 <- result
	set	sv4, #t			@ sv4 <- #t, default value with no args
	b	andor			@ jump to common and/or process loop

	/* (or exp1 exp2 ...) */
	PRIMIT	"or", sntx, 0
	@ in:	sv1 <- (exp1 exp2 ...),		sv5<- env
	@ out:	sv1 <- result
	set	sv4, #f			@ sv4 <- #f, default value with no args
andor:	@ [internal entry] common loop for and/or
	nullp	sv1			@ no arguments?
	itT	eq
	seteq	sv1, sv4		@	if so,  sv1 <- default result
	seteq	pc,  cnt		@	if so,  return with default result
	set	sv3, sv1		@ sv3 <- (exp1 exp2 ...)
	savrec	sv4			@ dts <- (dflt env cnt ...)
andorL:	@ evaluate body of function, one expression at a time
	@ sv3 <- body = (exp1 exp2 ...),  dts <- (env cnt ...)
	snoc	sv1, sv3, sv3		@ sv1 <- exp1,			sv2 <- (exp2 ...)
	nullp	sv3			@ are we at last expression?
	beq	andorT			@	if so,  jump to process it as a tail call
	cadr	env, dts		@ env <- env
	save	sv3			@ dts <- ((exp2 ...) env cnt ...)
	call	eval			@ sv1 <- val1, from evaluating sv1 in default environment
	restor	sv3			@ sv3 <- (exp1 exp2 ...),	dts <- (env cnt ...)
	car	sv4, dts		@ sv4 <- default val (#t for AND, #f for OR)
	eq	sv1, sv4		@ is result = default (AND with #t or OR with #f)
	beq	andorL			@	if so,  jump to keep looping
	eq	sv1, #f			@ is result #f?
	it	ne
	eqne	sv4, #f			@	if not, is default #f (i.e. OR)?
	bne	andorL			@		if neither, keep looping (AND with NOT #f)
andoxt:	@ immediate exit
	cdr	dts, dts		@ dts <- (env cnt ...)
	restor	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	set	pc, cnt			@ return
andorT:	@ tail-call for last expression in function body
	cdr	dts, dts		@ dts <- (env cnt...)
	restor	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	b	eval			@ jump to evaluate tail

	/* (let <name> bindings-list exp1 exp2 ...) */
	PRIMIT	"let", sntx, 1
	@ in:	sv1 <- name or bindings-list
	@ in:	sv2 <- (bindings-list exp1 exp2 ...) or (exp1 exp2 ...)
	@ out:	sv1 <- result
	save	sv1			@ dts <- (<name>-or-b-lst ...)
	varp	sv1			@ is this a named-let?
	it	eq
	snoceq	sv1, sv2, sv2		@	if so,  sv1 <- b-lst,	sv2 <- (exp1 exp2 ...)
	save	sv2			@ dts <- ((exp1 exp2 ...) <name>-or-b-lst ...)
	set	sv5, #null		@ sv5 <- '()
	list	sv2, sv5		@ sv2 <- (() . var-list-tail)
	list	sv5, sv5		@ sv5 <- (() . uval-list-tail)
	save	sv2, sv5		@ dts <- ((().vrlst (().uvlls) (e1 e2 ..) <nam>-or-bls ..)
	set	sv3, sv1		@ sv3 <- bindings-list
let0:	@ build lists of init vars and init uvals
	nullp	sv3			@ is bindings-list done?
	beq	let1			@	if so, jump to continue
	snoc	sv1, sv3, sv3		@ sv1 <- binding1,		sv3 <- rest of bindings-list
	snoc	sv1, sv4, sv1		@ sv1 <- var1,			sv4 <- (uval1)
	list	sv1, sv1		@ sv1 <- (var1)
	setcdr	sv2, sv1		@ store (var1) at tail of sv2
	set	sv2, sv1		@ sv2 <- new var list tail
	car	sv4, sv4		@ sv4 <- uval1
	list	sv4, sv4		@ sv4 <- (uval1)
	setcdr	sv5, sv4		@ store (uval1) at tail of sv5
	set	sv5, sv4		@ sv5 <- new uval list tail
	b	let0			@ jump to continue building var and uval lists
let1:	@ extract built-lists and expr list from stack
	restor	sv2, sv5		@ sv2 <- (().vrls), sv5<-(().uvlls), dts<-((e1.) <nm>|bls .)
	cdr	sv2, sv2		@ sv2 <- var-list
	cdr	sv5, sv5		@ sv5 <- uval-list
	restor	sv3			@ sv3 <- (exp1 exp2 ...), dts <-  (<name>-or- b-lst ...)
	car	sv4, dts		@ sv4 <- <name> or b-lst
	varp	sv4			@ is this a named-let?
	it	ne
	bne	let2			@	if not, jump to continue
	list	sv4, sv4		@ sv1 <- (name) = upcoming binding for name
	list	sv1, sv4		@ sv1 <- ((name)) = upcoming binding frame for name	
	cons	env, sv1, env		@ env <- updated environment for named-let
let2:	@ build lambda and jump to eval
	set	sv1, #procedure		@ sv1 <- procedure tag
	orr	sv1, sv1, #0x4000	@ sv1 <- full proc tag
	tagwenv	sv1, sv1, sv2, sv3	@ sv1 <- proc == [proc_tag vars-list (body) env]
	restor	sv2			@ sv2 <- <name> or b-lst,	dts <- (...)
	varp	sv2			@ is this a named-let?
	it	eq
	setcdreq sv4, sv1		@	if so,  store binding for named-let in environment
	cons	sv1, sv1, sv5		@ sv1 <- ((proc env (var1 ...) (exp1 exp2 ...)) uval1 ...)
	b	eval			@ jump to evaluate the lambda

	/* (let* bindings-list exp1 exp2 ...) */
	PRIMIT	"let*", lets, sntx, 1
	@ in:	sv1 <- bindings-list
	@ in:	sv2 <- (exp1 exp2 ...)
	@ out:	sv1 <- result
	sav_rc	sv2			@ dts <- ((exp1 ...) cnt ...)
lets0:	@ extend environment with binding for each init-var
	nullp	sv1			@ is bindings-list done?
	beq	lets1			@	if so, jump to continue
	snoc	sv1, sv3, sv1		@ sv1 <- binding1,		sv3 <- rest of bindings-list
	save	sv3			@ dts <- (rest-of-b-lst (exp1 ...) cnt ...)
	snoc	sv1, sv3, sv1		@ sv1 <- var1,			sv3 <- (uval1)
	list	sv1, sv1		@ sv1 <- (var1) = upcoming binding for var1
	list	sv1, sv1		@ sv1 <- ((var1)) = upcoming frame for var1
	cons	env, sv1, env		@ env <- environment for evaluation
	save	env			@ dts <- (env rest-of-b-lst (exp1 ...) cnt ...)
	car	sv1, sv3		@ sv1 <- uval1
	call	eval			@ sv1 <- val1
	restor	env			@ env <- env,	dts <- (rst-b-lst (exp1 ..) env ..)
	caar	sv2, env		@ sv2 <- (var1) = null binding for var1
	setcdr	sv2, sv1		@ store val1 in (var1) binding
	restor	sv1			@ sv1 <- rest-of-b-lst,		dts <- ((exp1 ...) cnt ...)
	b	lets0			@ jump to continue evaluating and binding the inits
lets1:	@ evaluate body in environment extended with let-bindings
	restor	sv1, cnt		@ sv1 <- (exp1 ...), cnt <- cnt, dts <- (...)
	b	sqnce

	/* (letrec bindings-list exp1 exp2 ...) */
	PRIMIT	"letrec", sntx, 1
	@ in:	sv1 <- bindings-list
	@ in:	sv2 <- (exp1 exp2 ...)
	@ out:	sv1 <- result
	save	sv1, sv2, cnt		@ dts <- (let-bindings-lst (exp1 ...) cnt ...)
	@ build environment frame for let-vars
	set	sv3, sv1		@ sv3 <- let-bindings-list
	set	sv4, sv1		@ sv4 <- pseudo-val-list (used for initialization)
	call	mkfrm			@ env <- let-env = (new-frame . env)
	@ prepare to evaluate binding vals for let-vars
	car	sv3, dts		@ sv3 <- let-bindings-list
	save	env			@ dts <- (let-env let-bindings-list (exp1 ...) cnt ...)
	set	sv4, #null		@ sv4 <- initial list of vals for let-vars
letr2:	@ evaluate let-vals
	nullp	sv3			@ is bindings-list done?
	beq	letr3			@	if so, jump to continue
	car	env, dts		@ env <- let-env
	snoc	sv1, sv3, sv3		@ sv1 <- binding1,		sv3 <- rest of bindings-list
	cadr	sv1, sv1		@ sv1 <- uval1
	save	sv4, sv3		@ dts <- (val-lst rst-b-lst let-env (exp1 ...) env cnt ...)
	call	eval			@ sv1 <- val1
	restor	sv4, sv3		@ sv4 <- oldvls, sv3<-rstbls,dts<-(ltnv ltbds (e1 .) cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- (val1 ...) = updated vals list	
	b	letr2			@ jump to continue building init vals list
letr3:	@ keep going
	restor	env			@ env <- let-env,	dts <- (let-bndngs-ls (e1 .) cnt .)
	restor	sv3, sv1, cnt		@ sv3 <- let-bndngs-ls, sv1 <-(e1 .), cnt <-cnt, dts <-(.)
	@ reverse vals list
	set	sv5, #null
ltr3_1:	nullp	sv4
	beq	ltr3_3
	snoc	sv2, sv4, sv4
	cons	sv5, sv2, sv5
	b	ltr3_1
ltr3_3:	@ bind vals to vars
	nullp	sv3			@ is let-bindings-list null?
	beq	sqnce
	snoc	sv2, sv3, sv3		@ sv2 <- (var uinit)
	car	rva, sv2		@ rva <- var
	car	sv4, env		@ sv4 <- current frame
ltr3_4:	@ bind var to val
	snoc	sv2, sv4, sv4		@ sv2 <- 1st binding in frame
	car	rvb, sv2		@ rvb <- 1st var in frame
	eq	rvb, rva		@ does var to add go farther in frame?
	bne	ltr3_4
	snoc	sv4, sv5, sv5
	setcdr	sv2, sv4
	b	ltr3_3


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



