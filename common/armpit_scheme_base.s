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

.balign	4

	@-------.-------.-------.-------.-------+
basenv:	@	4.1. sub-environment		|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_basenv - basenv - 4) >> 2

quote_env:	.word	quote,	pquote		@ quote			4.1.2 literal expressions
lambda_env:	.word	lambda,	plmbda		@ lambda		4.1.4 procedures
if_env:		.word	if,	pif		@ if			4.1.5 conditionals
set_env:	.word	set,	pset		@ set!			4.1.6 assignments

	@-------.-------.-------.-------.-------+
	@	4.2. sub-environment		|
	@-------.-------.-------.-------.-------+

begin_env:	.word	begin,	pbegin		@ begin			4.2.3 sequencing
unquote_env:	.word	unquote, scheme_true	@ unquote		4.2.6 quasiquotation
unqtsplc_env:	.word	unqtsplc, scheme_true	@ unquote-splicing
quasiquote_env:	.word	sqsqot,	pqsqot		@ quasiquote

	@-------.-------.-------.-------.-------+
	@	4.3. sub-environment		|
	@-------.-------.-------.-------.-------+

.ifndef r3rs

		.word	letsyn,	splet		@ let-syntax		4.3.1. bind. const. synt. keywords
		.word	ltrsyn,	spletr		@ letrec-syntax
synt_rules_env:	.word	synrls,	sntxrl		@ syntax-rules		4.3.2. pattern language
ellipsis_env:	.word	ellipsis, scheme_true	@ ...			4.3.c. constants
underscore_env:	.word	underscore, scheme_true	@ _
		.word	expand,	pmxpnd		@ expand		4.3.s.	support
		.word	smatch,	pmatch		@ match
		.word	substt,	psubst		@ substitute

.endif
	
	@-------.-------.-------.-------.-------+
	@	5. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	def,	pdefin		@ define		5.2 definitions
		.word	defsyn,	pdefin		@ define-syntax		5.3 syntax definition

	@-------.-------.-------.-------.-------+
	@	6.1. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	seqv,	peq		@ eq?
		.word	seq,	peq		@ eqv?
		.word	sequal,	pequal		@ equal?
	
	@-------.-------.-------.-------.-------+
	@	6.2. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	snumbe,	number		@ number?		6.2.5 numerical operations
		.word	seqn,	eqn		@ =
		.word	slt,	lt		@ <
		.word	sgt,	gt		@ >
		.word	sle,	le		@ <=
		.word	sge,	ge		@ >=
		.word	splus,	plus		@ +
		.word	sprodu,	produc		@ *
		.word	sminus,	minus		@ -
		.word	sdivis,	divisi		@ /
		.word	squoti,	quotie		@ quotient
		.word	sremai,	remain		@ remainder
		.word	smodul,	modulo		@ modulo
		.word	snmstr,	pnmstr		@ number->string	6.2.6 numerical input output
		.word	sstnum,	pstnum		@ string->number

.ifndef	integer_only

		.word	scmplx,	cmplx		@ complex?		6.2.5 numerical operations
		.word	sreal,	real		@ real?
		.word	sratio,	ration		@ rational?
		.word	sinteg,	integr		@ integer?
		.word	sexact,	exact		@ exact?
		.word	sinxct,	inxact		@ inexact?
		.word	snmrtr,	nmrtr		@ numerator
		.word	sdnmnt,	dnmntr		@ denominator
		.word	sfloor,	floor		@ floor
		.word	sceili,	ceilin		@ ceiling
		.word	strunc,	trunca		@ truncate
		.word	sround,	round		@ round
		.word	sexp,	exp		@ exp
		.word	slog,	log		@ log
		.word	ssin,	sin		@ sin
		.word	scos,	cos		@ cos
		.word	stan,	tan		@ tan
		.word	sasin,	asin		@ asin
		.word	sacos,	acos		@ acos
		.word	satan,	atan		@ atan
		.word	ssqrt,	sqrt		@ sqrt
		.word	sexpt,	expt		@ expt
		.word	smkrec,	makrec		@ make-rectangular
		.word	smkpol,	makpol		@ make-polar
		.word	srlpt,	realpt		@ real-part
		.word	simgpt,	imagpt		@ imag-part
		.word	smagni,	magnit		@ magnitude
		.word	sangle,	angle		@ angle
		.word	sex2in,	ex2inx		@ exact->inexact
		.word	sin2ex,	inx2ex		@ inexact->exact
	
.endif	@ .ifndef integer_only

	@-------.-------.-------.-------.-------+
	@	6.3.2. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	pair,	ppair		@ pair?			6.3.2 pairs and lists
cons_env:	.word	scons,	pcons		@ cons
		.word	car,	pcar		@ car
		.word	cdr,	pcdr		@ cdr
		.word	setcar,	pstcar		@ set-car!
		.word	setcdr,	pstcdr		@ set-cdr!

	@-------.-------.-------.-------.-------+
	@	6.3.3. Sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	symbol,	psymbl		@ symbol?		6.3.3 symbols
		.word	ssmstr,	psmstr		@ symbol->string
		.word	sstsym,	pstsym		@ string->symbol

	@-------.-------.-------.-------.-------+
	@	6.3.4. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	char,	pchar		@ char?			6.3.4 characters
		.word	chareq,	pcheq		@ char=?
		.word	charlt,	pchlt		@ char<?
		.word	chargt,	pchgt		@ char>?
		.word	charle,	pchle		@ char<=?
		.word	charge,	pchge		@ char>=?
		.word	chrint,	pchint		@ char->integer
		.word	intchr,	pintch		@ integer->char

	@-------.-------.-------.-------.-------+
	@	6.3.5. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	sqstng,	qstrng		@ string?		6.3.5 strings
		.word	smakst,	makstr		@ make-string
		.word	sstlen,	strlen		@ string-length
		.word	sstref,	strref		@ string-ref
		.word	sstset,	strset		@ string-set!

	@-------.-------.-------.-------.-------+
	@	6.3.6. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	sqvect,	qvecto		@ vector?		6.3.6 vectors
		.word	smkvec,	pmkvec		@ make-vector
		.word	svclen,	veclen		@ vector-length
		.word	svcref,	vecref		@ vector-ref
		.word	svcset,	vecset		@ vector-set!

	@-------.-------.-------.-------.-------+
	@	6.4. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	sprocd,	proced		@ procedure?		6.4 control features
		.word	sapply,	papply		@ apply
		.word	scllcc,	callcc		@ call/cc
		.word	scwcc,	callcc		@ call-with-current-continuation

.ifndef r3rs

		.word	svalus,	values		@ values
		.word	scllwv,	callwv		@ call-with-values
		.word	sdnwnd,	dynwnd		@ dynamic-wind

.endif
	
	@-------.-------.-------.-------.-------+
	@	6.5. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	seval,	peval		@ eval			6.5 eval
		.word	sscenv,	screnv		@ scheme-report-environment
		.word	snlenv,	nulenv		@ null-environment
		.word	sinenv,	pinenv		@ interaction-environment

	@-------.-------.-------.-------.-------+
	@	6.6. sub-environment		|
	@-------.-------.-------.-------.-------+
	
		.word	sinpor,	inport		@ input-port?		6.6.1 ports
		.word	soutpr,	outprt		@ output-port?
curinport_env:	.word	scripr,	criprt		@ current-input-port
curoutport_env:	.word	scropr,	croprt		@ current-output-port
		.word	sopnif,	opnifl		@ open-input-file
		.word	sopnof,	opnofl		@ open-output-file
		.word	sclsip,	clsipr		@ close-input-port
		.word	sclsop,	clsopr		@ close-output-port
		.word	sredch,	redchr		@ read-char		6.6.2 input
		.word	spekch,	pekchr		@ peek-char
		.word	seofob,	eofobj		@ eof-object?
		.word	schrdy,	chrrdy		@ char-ready?
		.word	swrtch,	wrtchr		@ write-char		6.6.3 output

	@-------.-------.-------.-------.-------+
	@	addendum sub-environment	|
	@-------.-------.-------.-------.-------+

		.word	sunloc,	unlock		@ unlock
		.word	sfiles,	files		@ files
.ifndef	live_SD	
		.word	serase,	erase		@ erase			files
		.word	sfpgwr,	fpgwrt		@ fpgw
.endif
.ifdef	onboard_SDFT
		.word	ssdini,	psdini		@ sd-init
.endif

end_of_basenv:	@ end of basenv
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======@
@												@
@		CONSTANTS									@
@												@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======@

.macro	make_var_from_basenv var_name, var_env
	\var_name = ((\var_env - basenv + 4) << 13) | ((base_env - scmenv) << 6) | variable_tag
.endm

	@-------.-------.-------.-------.-------+
@-------@	4.1. Constants			|
	@-------.-------.-------.-------.-------+

	make_var_from_basenv	lambda_var,	lambda_env
	make_var_from_basenv	set_var,	set_env
	make_var_from_basenv	quote_var,	quote_env
	make_var_from_basenv	if_var,		if_env

	@-------.-------.-------.-------.-------+
@-------@	4.2. Constants			|
	@-------.-------.-------.-------.-------+

	make_var_from_basenv	begin_var,	begin_env
	make_var_from_basenv	unquote_var,	unquote_env
	make_var_from_basenv	unqtsplc_var,	unqtsplc_env
	make_var_from_basenv	quasiquote_var,	quasiquote_env

	@-------.-------.-------.-------.-------+
@-------@	4.3. Constants			|
	@-------.-------.-------.-------.-------+
	
.ifndef r3rs

	make_var_from_basenv	synt_rules_var,	synt_rules_env
	make_var_from_basenv	ellipsis_var,	ellipsis_env
	make_var_from_basenv	underscore_var,	underscore_env

.endif
	
	@-------.-------.-------.-------.-------+
@-------@	6.6. Constants			|
	@-------.-------.-------.-------.-------+

	make_var_from_basenv	curinport_var,	curinport_env
	make_var_from_basenv	curoutport_var,	curoutport_env

	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg


@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	4.	Expressions
@	4.1.	Primitive expression types:	quote, lambda, if, set!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			savrec, eval, lcons, bndchk, npofxt
@
@	Modified by (switches):			(none)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
.balign	4
	
quote:	SYMSIZE	5
	.ascii	"quote"
	.balign 4
	
pquote:	@ (quote expr)
	@ on entry:	sv1 <- expr
	@ on exit:	sv1 <- result
	@ pre-entry:	return returns directly via cnt
	ESYNTAX	0, oreturn, 1		@  syntax, init-sv4 = none, fentry = return, narg = 1

.balign	4
	
lambda:	SYMSIZE	6
	.ascii	"lambda"
	.balign 4
	
plmbda:	@ (lambda vars-list body)
	@ on entry:	sv1 <- (vars-list body)
	@ on exit:	sv1 <- (procedure env vars-list body)
	SYNTAX	0			@ syntax type, listed input arg
	set	sv2, sv1		@ sv2 <- (vars-list body)
	set	sv1, env		@ sv1 <- env
	set	sv3, #procedure		@ sv3 <- partial proc tag
	orr	sv3, sv3, #0x4000	@ sv3 <- full proc tag
	lcons	sv1, sv3, sv1, sv2	@ sv1 <- proc == (proc_tag env vars-list body)
	set	pc,  cnt		@ return
	
.balign	4
	
if:	SYMSIZE	2
	.ascii	"if"
	.balign 4
	
pif:	@ (if pred true-exp <false-exp>)
	@ on entry:	sv1 <- pred
	@ on entry:	sv2 <- (true-exp <false-exp>)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax type, one input arg	
	savrec	sv2			@ dts <- ((true-exp <false-exp>) env cnt ...)
	call	eval			@ sv1 <-  #t/#f, from evaluating sv1 in default environment
	eq	sv1, #f			@ is predicate false?
	restor3	sv1, env, cnt		@ sv1 <- (t-exp <f-exp>), env <- env, cnt <- cnt, dts <- (...)
	it	eq
	pntrpeq	sv1
	it	eq
	cdreq	sv1, sv1		@	if so,  sv1 <- (<false-exp>)
	nullp	sv1			@ is there an expression to evaluate?
	it	eq
	seteq	pc,  cnt		@	if not, return with '()
	car	sv1, sv1		@ sv1 <- true-or-false-exp
	b	eval			@ jump to evaluate true-or-false-exp

.balign	4
	
set:	SYMSIZE	4
	.ascii	"set!"
	.balign 4

pset:	@ (set! var exp)
	@ on entry:	sv1 <- var
	@ on entry:	sv2 <- exp
	@ on exit:	sv1 <- '()
	SYNTAX	2			@ syntax type, 2 input args
	savrec	sv1			@ dts <- (var env cnt ...)
	set	sv1, sv2		@ sv1 <- exp
	call	eval			@ sv1 <- val of exp, from evaluating sv1 in default environment
	set	sv4, sv1		@ sv4 <- val of exp saved against bndchk
	restor3	sv1, env, cnt		@ sv1 <- var  -- for bndenv, env <- env, cnt <- cnt, dts <- (...)
	bl	bndchk			@ sv1 <- bndng=(var . val) or val or null, sv5 <- indicator
	eq	sv3, sv5		@ is sv1 immediate (no binding found or var is built-in)?
	beq	corerr
	setcdr	sv3, sv4		@ binding == (key . val), set cdr of binding to val
	b	npofxt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.3.	sequencing:					begin
@	4.2.6.	quasiquotation:					unquote, unquote-splicing, quasiquote
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			
@							
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:
@
@	4.2.3.	sequencing:		begin
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:

.balign	4
	
begin:	SYMSIZE	5
	.ascii	"begin"
	.balign 4

pbegin:	@ (begin exp1 exp2 ...)
	@ on entry:	sv1 <- (exp1 exp2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	0			@ syntax type, listed input args	
	b	sqnce			@ jump to sequence (located within apply:)


@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:
@
@	4.2.6.	quasiquotation:		unquote, unquote-splicing, quasiquote
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:

.balign	4
	
sqsqot:	SYMSIZE	10
	.ascii	"quasiquote"
	.balign 4

unquote:
	SYMSIZE	7
	.ascii	"unquote"
	.balign 4

unqtsplc:
	SYMSIZE	16
	.ascii	"unquote-splicing"
	.balign 4

pqsqot:	@ (quasiquote expr-list)
	@ on entry:	sv1 <- expr-list
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax type, one input arg
qsqot:	@ [internal entry]
	bl	typsv1			@ rva <- type tag of expr1 (sv1)
	eq	rva, #list_tag		@ is expr1 a list?
	it	ne
	setne	pc, cnt			@	if not, return
	set	sv5, #null		@ sv5 <- '()
	list	sv5, sv5		@ sv5 <- (() . expr-list-result) = tail-ref
	save3	env, sv5, cnt		@ dts <- (env (() . expr-list-result) cnt ...)
	set	sv3, sv1		@ sv3 <- expr-list
qsqt0:	@ build unquoted/spliced copy of expr-list
	nullp	sv3			@ done processing expr-list?
	beq	qsqtxt			@	if so,  jump to exit
	car	sv1, sv3		@ sv1 <- expr1
	ldr	sv4, =unquote_var	@ sv4 <- unquote
	eq	sv1, sv4		@ is expr1 an improper-list unquote expression?
	beq	qsqt3			@	if so,  jump to process that
	pntrp	sv1			@ is expr1 a pointer (i.e. a non-immediate)?
	bne	qsqt2			@	if not, jump to glue it in expr-list-result
	tagdp	sv1			@ is expr1 a tagged item (proc, string, ...)
	beq	qsqt2			@	if so,  jump to glue it in expr-list-result
	ratcpx	sv1			@ is expr1 a rational or complex
	beq	qsqt2			@	if so,  jump to glue it in expr-list-result
	car	sv2, sv1		@ sv2 <- 1st item of expr1 (expr1 is a list)
	ldr	sv4, =unquote_var	@ sv4 <- unquote
	eq	sv2, sv4		@ is expr1 a unquote expression?
	itT	ne
	ldrne	sv4, =unqtsplc_var	@	if not, sv4 <- unquote-splicing
	eqne	sv2, sv4		@	if not, is expr1 a unquote-splicing expression?
	bne	qsqt4			@	if not, jump to recursively unquote/splice list in sv1
	@ evaluate item that is to be unquoted/spliced
	cadr	sv1, sv1		@ sv1 <- item to evaluate
	car	env, dts		@ env <- env
	save2	sv5, sv3		@ dts <- (tail-ref expr-list env (() . expr-list-result) cnt ...)
	call	eval			@ sv1 <- expr1-result
	restor2	sv5, sv3		@ sv5 <- tl-rf, sv3 <- exp-ls, dts <- (env (() . exp-lsrslt) cnt .)
	caar	sv2, sv3		@ sv2 <- unquote-or-unquote-splicing
	ldr	sv4, =unquote_var	@ sv4 <- unquote
	eq	sv2, sv4		@ was expr1 just unquote-ed (rather than uq-splice)?
	beq	qsqt2			@	if so,  jump to glue it in expr-list-result
	@ splice elements of list in sv1 into expr-result-list
	nullp	sv1			@ is expr1-result null?
	it	eq
	cdreq	sv3, sv3		@	if so,  sv3 <- rest-of-expr-list
	beq	qsqt0			@	if so,  jump to process rest-of-expr-list
	set	sv2, sv1		@ sv2 <- expr1-results-list
qsqt1:	snoc	sv1, sv2, sv2		@ sv1 <- expr1-result1,	sv2 <- expr1-rest-of-results
	nullp	sv2			@ is rest of results null?
	beq	qsqt2			@	if so,  jump to glue last item in expr-result-list
	list	sv1, sv1		@ sv1 <- (expr1-result)
	setcdr	sv5, sv1		@ store (expr1-result) at tail of sv5
	set	sv5, sv1		@ sv5 <- new expr-list-result tail
	b	qsqt1			@ jump to continue splicing in the expr1-items
qsqt4:	@ rescursively unquote inside a list
	save3	sv5, sv3, cnt		@ dts <- (tail-ref expr-list cnt env (() . expr-lst-result) cnt ..)
	call	qsqot			@ sv1 <- unquoted-spliced expr-list
	restor3	sv5, sv3, cnt		@ sv5 <- tl-rf, sv3 <- exp-ls, dts <- (env (() . exp-lsrslt) cnt .)
qsqt2:	@ glue item in expr-result-list
	cdr	sv3, sv3		@ sv3 <- rest-of-expr-list
	list	sv1, sv1		@ sv1 <- (expr1-result)
	setcdr	sv5, sv1		@ store (expr1-result) at tail of sv5
	set	sv5, sv1		@ sv5 <- new expr-list-result tail
	b	qsqt0
qsqt3:	@ evaluate and glue an improper-list unquote
	cadr	sv1, sv3		@ sv1 <- item to evaluate
	car	env, dts		@ env <- env
	save	sv5			@ dts <- (tail-ref env (() . expr-list-result) cnt ...)
	call	eval			@ sv1 <- expr1-result
	restor	sv5			@ sv5 <- tail-ref,	dts <- (env (() . exp-lst-rslt) cnt ..)
	setcdr	sv5, sv1		@ store expr1-result at tail of sv5
qsqtxt:	@ extract result from stack and exit
	restor3	env, sv1, cnt		@ env <- env, sv1 <- (().ex-ls-rs), cnt <- cnt, dts <- (...)
	cdr	sv1, sv1		@ sv1 <- expr-list-result
	set	pc, cnt


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	4.	Expressions
@	4.3.	Macros
@	4.3.1.	binding constructs for syntactic keywords:	let-syntax, letrec-syntax
@	4.3.2.	Pattern language:				syntax-rules
@	4.3.c.	constants:					..., _
@	4.3.s.	support:					expand, match, substitute
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			savrec, eval, cons, save3, mkfrm, save, mxpnd
@						sqnce, save2
@						sav_rc, list, sav__c, synt_rules_synt, quote_synt
@						quasiquote_synt, bndchk, bcons, save3, save,
@						error4, ellipsis_var, save2, sav_rc, bcons, save3,
@						list, cons, bndchk
@		6.3.3.	Symbols:		symstr, strsym (gensym)
@							
@
@	Modified by (switches):			CORE_ONLY
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
.ifndef r3rs

.balign	4
	
letsyn:	SYMSIZE	10
	.ascii	"let-syntax"
	.balign 4

splet:	@ (let-syntax bindings-list exp1 exp2 ...)
	@ on entry:	sv1 <- bindings-list
	@ on entry:	sv2 <- (exp1 exp2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax type, one input arg
	savrec	sv2			@ dts <- ((exp1 exp2 ...) env cnt ...)
	set	sv3, #null		@ sv3 <- '() = initial vars-list
	set	sv4, #null		@ sv4 <- '() = initial vals-list
slet0:	@ build lists of init vars and init uvals
	nullp	sv1			@ is bindings-list done?
	beq	slet1			@	if so, jump to continue
	snoc	sv1, sv5, sv1		@ sv1 <- 1st-binding, sv5 <- rest-of-bindings-list
	snoc	sv1, sv2, sv1		@ sv1 <- var1, sv2 <- (uval1)
	cons	sv3, sv1, sv3		@ sv3 <- updated vars-list
	cadr	env, dts		@ env <- env, restored
	save3	sv5, sv3, sv4		@ dts <- (rst-bnds-lst new-vrs-lst vls-ls (exp1 exp2 ..) env cnt .)
	car	sv1, sv2		@ sv1 <- uval1
	call	eval			@ sv1 <- val1
	restor3	sv5, sv3, sv4		@ sv5 <- rst-bnds, sv3 <- new-vrs-l
					@ sv4 <- vls-lst, dts <- ((e1 e2 .) env cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- new-vals-list
	set	sv1, sv5		@ sv1 <- rest-bindings-list
	b	slet0
slet1:	@ extract built-lists and expr list from stack
	call	mkfrm			@ env <- extended environment
	restor	sv2			@ sv2 <- (exp1 exp2 ...), dts <- (env cnt ...)
	save	env			@ dts <- (extended-env env cnt ...)
	set	sv3, #null
slet2:	@
	nullp	sv2
	beq	slet3
	snoc	sv1, sv4, sv2		@ sv1 <- exp1, sv2 <- (exp2 ...)
	car	env, dts		@ env <- extended-env
	save2	sv3, sv4		@ dts <- (extended-exprs-list (exp2 ...) extended-env env dts ...)
	call	mxpnd			@ sv1 <- extended expression1
	restor2	sv3, sv4		@ sv3 <- extnd-exprs-list, sv4 <- (exp2 ...)
					@ dts <- (extnd-env env dts ...)
	cons	sv3, sv1, sv3		@ sv3 <- updated expanded-exprs-list
	set	sv2, sv4		@ sv2 <- (exp2 ...)
	b	slet2
slet3:	@ reverse the expanded list
	nullp	sv3
	beq	slet4
	snoc	sv1, sv3, sv3
	cons	sv2, sv1, sv2
	b	slet3
slet4:	@ prepare to evaluate expanded expressions
	restor3	env, env, cnt
	set	sv1, sv2		@ sv1 <- expanded-exprs-list
	b	sqnce
	
.balign	4

ltrsyn:	SYMSIZE	13
	.ascii	"letrec-syntax"
	.balign 4

spletr:	@ (letrec-syntax bindings-list exp1 exp2 ...)
	@ on entry:	sv1 <- bindings-list
	@ on entry:	sv2 <- (exp1 exp2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax type, one input arg
	save3	sv1, sv2, cnt		@ dts <- (let-bindings-lst (exp1 ...) cnt ...)
	@ build environment frame for let-vars
	set	sv3, sv1		@ sv3 <- let-bindings-list
	set	sv4, sv1		@ sv4 <- pseudo-val-list (used for initialization)
	call	mkfrm			@ env <- let-env = (new-frame . env)
	@ prepare to evaluate binding vals for let-vars
	car	sv3, dts		@ sv3 <- let-bindings-list
	save	env			@ dts <- (let-env let-bindings-list (exp1 ...) cnt ...)
	set	sv4, #null		@ sv4 <- initial list of vals for let-vars
sletr2:	@ evaluate let-vals
	nullp	sv3			@ is bindings-list done?
	beq	sletr3			@	if so, jump to continue
	car	env, dts		@ env <- let-env
	snoc	sv1, sv3, sv3		@ sv1 <- binding1,		sv3 <- rest of bindings-list
	cadr	sv1, sv1		@ sv1 <- uval1
	save2	sv4, sv3		@ dts <- (val-lst rst-b-lst let-env (exp1 ...) env cnt ...)
	call	eval			@ sv1 <- val1
	restor2	sv4, sv3		@ sv4 <-old-vlst,sv3 <-rst-blst,dts <-(ltenv ltbnds (exp1 .) cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- (val1 ...) = updated vals list	
	b	sletr2			@ jump to continue building init vals list
sletr3:	@ keep going
	restor	env			@ env <- let-env,	dts <- (let-bindings-list (exp1 ..) cnt ..)
	restor3	sv3, sv1, cnt		@ sv3 <- let-bndngs-lst, sv1 <- (exp1 ..), cnt <- cnt, dts <- (..)
	@ reverse vals list
	set	sv5, #null
sltr31:	nullp	sv4
	beq	sltr33
	snoc	sv2, sv4, sv4
	cons	sv5, sv2, sv5
	b	sltr31
sltr33:	@ bind vals to vars
	nullp	sv3			@ is let-bindings-list null?
	beq	sltr35
	snoc	sv2, sv3, sv3		@ sv2 <- (var uinit)
	car	rva, sv2		@ rva <- var
	car	sv4, env		@ sv4 <- current frame
sltr34:	@ bind var to val
	snoc	sv2, sv4, sv4		@ sv2 <- 1st binding in frame
	car	rvb, sv2		@ rvb <- 1st var in frame
	eq	rvb, rva		@ does var to add go farther in frame?
	bne	sltr34
	snoc	sv4, sv5, sv5
	setcdr	sv2, sv4
	b	sltr33
sltr35:	
	savrec	env			@ dts <- (extended-env env cnt ...)
	set	sv2, sv1
	b	slet2			@ jump to expand (exp1 ...) and then sequence over exprs
	
	
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:
@
@	4.3.2.	Pattern language:	syntax-rules
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:

.balign	4
	
synrls:	SYMSIZE	12
	.ascii	"syntax-rules"
	.balign 4

sntxrl:	@ (syntax-rules literals rule1 ...)
	@ on entry:	sv1 <- (literals rule1 ...)
	@ on exit:	sv1 <- (macro literals rule1 ...)
	SYNTAX	0			@ syntax type, listed input args
	set	sv3, sv1
	set	sv1, #macro
	cons	sv1, sv1, sv3		@ sv1 <- (macro env literals rule1 ...)
	set	pc,  cnt

@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:
@
@	4.3.s.	support:		expand, match, substitute
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:

.balign	4

expand:	SYMSIZE	6
	.ascii	"expand"
	.balign 4

ellipsis: @ ... (variable)
	SYMSIZE	3
	.ascii	"..."
	.balign 4
	
underscore: @ _ (variable)
	SYMSIZE	1
	.ascii	"_"
	.balign 4

pmxpnd:	@ (expand expr)
	@ expand the macros in expr
	@ on entry:	sv1 <- expr
	@ on exit:	sv1 <- expr with expanded macros
	SYNTAX	1			@ syntax type, one input arg
mxpnd:	@ [internal entry]
	@ macro expansion
	list	sv4, sv1		@ sv4 <- (expr) = parent
	sav_rc	sv4			@ dts <- (parent cnt ...)
mxpnds:	set	sv5, #null		@ sv5 <- '() = initial macro-list
	car	sv4, dts		@ sv4 <- parent
	call	mxpndl			@ sv5 <- updated macro-list (if macros are in expr)
	nullp	sv5			@ any macros in expr?
	bne	mxpndq			@	if so,  jump to substitute and side-effect expr
	restor2	sv4, cnt		@ sv4 <- parent, cnt <- cnt, dts <- (...)
	car	sv1, sv4		@ sv1 <- substituted expr
	set	pc,  cnt		@ return

mxpndl:	@ sv4 <- parent list
	sav__c				@ dts <- (cnt ...)
mxpnd5:	@ sv4 <- parent list
	qpair0	sv4			@ is it an atom, var/synt, sized item or rat/cpx?
	beq	mxpnd6
	call	mxpnd0			@ sv5 <- updated macro-list (if macros are in expr)
	cdr	sv4, sv4		@ sv4 <- rest of parent
	nullp	sv4			@ done?
	bne	mxpnd5			@	if not, jump to keep finding macros in parent
mxpnd6:	@ return
	restor	cnt			@ cnt <- cnt, dts <- (...)
	set	pc, cnt			@ return
mxpnd0:	car	sv1, sv4
	bl	typsv1
	eq	rva, #macro		@ is it a macro?
	it	ne
	eqne	rva, #list_tag		@	if not, is it a list?
	it	ne
	setne	pc,  cnt		@	if not, return
	car	rvc, sv1
	varp	rvc
	bne	mxpnd1
	ldr	rva,=synt_rules_var	@ rva <- variable-ID of syntax-rules
	eq	rva, rvc		@ is var/synt = syntax-rules?
	it	eq
	seteq	pc,  cnt		@	if so,  return (don't expand macros inside syntax-rules)
	ldr	rva,=quote_var		@ rva <- variable-ID of quote
	ldr	rvb,=quasiquote_var	@ rvb <- variable-ID of quasiquote
	eq	rva, rvc		@ is var/synt = quote or quasiquote?
	it	ne
	eqne	rvb, rvc
	it	eq
	seteq	pc,  cnt		@	if so,  return (don't expand macros inside quote/quasiq)
	sav__c				@ dts <- (cnt ...)
	call	mxpnr0
	restor	cnt
mxpnd1:	@
	sav_rc	sv4			@ dts <- (sv4 cnt ...)
	set	sv4, sv1		@ sv4 <- new parent
	call	mxpndl			@ explore sv1
	restor2	sv4, cnt
	set	pc,  cnt
mxpnr0:	@ list starts with a variable, see if it is bound to a macro
	@ sv1 <- expr, sv4 <- parent, sv5 <- macro-list
	caar	sv1, sv4		@ sv1 <- var
	set	sv2, sv5		@ sv2 <- macro-list, saved against bndchk
	bl	bndchk			@ sv1 <- binding
	car	sv1, sv4		@ sv1 <- expr
	set	sv3, sv5		@ sv3 <- bndchk return indicator
	set	sv5, sv2		@ sv5 <- macro-list, restored
	pntrp	sv3
	it	ne
	setne	pc,  cnt
	macrop	sv3
	it	ne
	setne	pc,  cnt
	set	sv1, sv4
	bcons	sv5, sv1, sv3, sv5
	car	sv1, sv4
	set	pc,  cnt
mxpndq:	@ sv5 <- macro-list, dts <- ((expr) cnt ...)
	nullp	sv5
	beq	mxpnds			@ jump to re-scan expression for macros
	snoc	sv4, sv5, sv5		@ sv4 <- (parent . macro), sv5 <- rest-macro-list
	snoc	sv2, sv3, sv4		@ sv2 <- parent, sv3 <- macro
	@ sv3 <- macro,  sv2 <- parent, dts <- (macro-list (expr) cnt ...)
	cdr	sv3, sv3		@ sv1 <- (lits rule1 rule2 ...)
	snoc	sv1, sv3, sv3		@ sv1 <- literals,		sv3 <- (rule1 rule2 ...)
	save3	sv2, sv1, sv5		@ dts <- (parent literals macro-list (expr) cnt ...)
	set	sv1, sv3		@ sv1 <- (rule1 rule2 ...)
evlmlp:	@ macro match loop
	@ sv1 <- (rule1 rule2 ...)
	@ dts <- (parent literals macro-list (expr) cnt ...)
	caar	sv4, sv1		@ sv4 <- pattern1
	snoc	sv3, sv5, dts		@ sv3 <- parent,	sv5 <- (literals macro-list (expr) cnt ...)
	car	sv3, sv3		@ sv3 <- expr
	car	sv2, sv5		@ sv2 <- literals
	save	sv1			@ dts <- ((rule1 rule2 ...) parent lits macro-list (expr) cnt ...)
	set	sv5, #null		@ sv5 <- '() = initial bindings
	call	match			@ sv1 <- bindings-or-#f
	eq	sv1, #f			@ match found?
	bne	evlsbs			@	if so,  jump to substitute
	restor	sv1			@ sv1 <- (rule1 rule2 ..), dts <- (prnt lits maclst (exp) cnt ..)
	cdr	sv1, sv1		@ sv1 <- (rule2 ...)
	nullp	sv1			@ more rules to try?
	bne	evlmlp			@	if so,  jump to keep testing them
	@ error in expand
	ldr	sv4, =expand
	b	error4
evlsbs:	@ macro substitute
	@ sv1 <- bindings, dts <- ((rule1 rule2 ...) parent literals macro-list (expr) cnt ...)
	set	sv4, sv1		@ sv4 <- bindings
	restor	sv3			@ sv3 <- (mtchn-rul othr-rul .), dts<-(prnt lts  maclst (ex) cnt .)
	cadar	sv3, sv3		@ sv3 <- expr-for-subst
	call	substi			@ sv1 <- substituted-expr (template=sv3, bindings=sv4, (env)=sv2)
	restor	sv4			@ sv4 <- parent, dts <- (literals macro-list (expr) cnt ...)
	setcar	sv4, sv1		@ side-effect parent with substituted expression
	cdr	dts, dts		@ dts <- (macro-list (expr) cnt ...)
	restor	sv5			@ sv5 <- macro-list, dts <- ((expr) cnt ...)
	b	mxpndq

.balign	4
	
smatch:	SYMSIZE	5
	.ascii	"match"
	.balign 4

pmatch:	@ (match expr pat bndngs lits)
	@ on entry:	sv1 <- expr
	@ on entry:	sv2 <- pat
	@ on entry:	sv3 <- bndings
	@ on entry:	sv4 <- lits
	@ on exit:	sv1 <- updated bindings or #f
	SYNTAX	4			@ syntax type, 4 input args
	set	sv5, sv3		@ sv5 <- bindings
	set	sv3, sv1		@ sv3 <- expr
	swap	sv2, sv4, sv1		@ sv2 <- literals,		sv4 <- pattern
match:	@ [internal entry]
	@ on entry:	sv2 <- literals
	@ on entry:	sv3 <- expression
	@ on entry:	sv4 <- pattern
	@ on entry:	sv5 <- bindings
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
	pntrp	sv1			@ is cdr of pattern a list? (i.e pattern may be (a_pattern ...))
	bne	matcls			@	if not, jump to match proper or improper lists
	car	sv1, sv1		@ sv1 <- cadr of pattern
	and	rva, sv1, #0xff		@ rva <- type of cadr of pattern
	eq	rva, #variable_tag	@ is cadr of pattern a variable (eg. "...")?
	bne	matcls			@	if not, jump to match proper or improper lists
	cadr	sv1, sv4		@ sv1 <- cadr of pattern
	ldr	rva, =ellipsis_var
	eq	sv1, rva
	beq	matcel			@	if so,  jump to match expr to pattern-with-ellipsis
matcls:	@ pattern is a list and is not (a_pattern "...")
	pntrp	sv3			@ is expression a pointer?	
	bne	matcfx			@	if not, exit with #f
	@ both expr and pattern are lists (match them to one another)
	cdr	sv1, sv3		@ sv1 <- rest-of-expr
	save2	sv1, sv2		@ dts <- (rest-of-expr literals ...)
	cdr	sv1, sv4		@ sv1 <- rest-pattern
	car	sv3, sv3		@ sv3 <- 1st-expr
	car	sv4, sv4		@ sv4 <- 1st pattern
	sav_rc	sv1			@ dts <- (rest-pattern cnt rest-of-expr literals ...)
	call	match			@ sv1 <- bindings-or-#f
	restor2	sv4, cnt		@ sv4 <- rest-pattern, cnt <- cnt, dts <- (rest-expr literals ...)
	restor2	sv3, sv2		@ sv3 <- rest-expr, sv2 <- literals, dts <- (...)
	set	sv5, sv1		@ sv5 <- bindings-or-#f
	b	match
matcob:	@ is the pattern/value (sv4) equal to the expression (sv3)?
	set	sv1, sv3		@ sv2 <- expr
	set	sv2, sv4		@ sv2 <- pattern
	sav_rc	sv5			@ dts <- (bindings cnt ...)
	call	equal			@ sv1 <- #t/#f
	restor2	sv5, cnt		@ cnt <- cnt, sv5 <- bindings, dts <- (...)
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
	bcons	sv5, sv1, sv3, sv5
	b	matcxt			@ return with updated bindings
matcel:	@ pattern is a list like (a_pattern "...")
	save	sv2			@ dts <- (literals ...)
	car	sv4, sv4		@ sv4 <- a_pat
	save3	sv3, sv5, sv4		@ dts <- (expr bindings a_pat literals ...)
	set	sv5, #null		@ sv5 <- () = new-bindings
matc_J:	restor	sv3			@ sv3 <- rest-expr,		dts <- (bnds a_pat lits ...)
	nullp	sv3			@ no more expr to match to (a_pat "...") ?
	beq	matc_K
	cdr	sv1, sv3		@ sv1 <- rest-expr
	car	sv3, sv3		@ sv3 <- expr1
	cdr	sv4, dts		@ sv4 <- (a_pat lits ...)
	save3	sv5, cnt, sv1		@ dts <- (new-binds rest-expr bindings a_pat literals ...)
	snoc	sv4, sv5, sv4		@ sv4 <- a_pat,			sv5 <- (lits ...)
	car	sv2, sv5		@ sv2 <- lits
	set	sv5, #null		@ sv5 <- () = initial bindings
	call	match			@ sv1 <- bnds-or-#f
	restor2	sv5, cnt		@ sv5 <- new-bnds, cnt <- cnt, dts <- (rst-exp bnds a_pat lits ...)
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
	setcdr	sv1, sv2		@ store updated binding in cdr of 1st new bindng (side-effects sv1)
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
	seteq	pc,  cnt
	set	sv2, sv1		@ sv2 <- updated-bnds
	snoc	sv1, sv5, sv5		@ sv1 <- 1st new binding,   sv5 <- rest of new bindings
	cons	sv1, sv1, sv2		@ sv1 <- (new-binding . updated-bnds)
	b	matc_Z
matcex:	@ pop stack then exit with #f
	cddr	dts, dts
	cddr	dts, dts
matcfx:	@ exit with #f
	set	sv1, #f			@ sv1 <- #f
	set	pc,  cnt		@ return with #f
matcxt:	@ exit with bindings
	set	sv1, sv5		@ sv1 <- bindings
	set	pc,  cnt		@ return with bindings


.balign	4
	
substt:	SYMSIZE	10
	.ascii	"substitute"
	.balign 4

psubst:	@ (substitute bindings template)
	@ on entry:	sv1 <- bindings
	@ on entry:	sv2 <- template
	@ on exit:	sv1 <- substituted expr
	SYNTAX	2			@ syntax type, two input args
	set	sv4, sv1		@ sv4 <- bindings
	set	sv3, sv2		@ sv3 <- template
substi:	@ [internal entry]
	@ on entry:	sv3 <- template
	@ on entry:	sv4 <- bindings
	@ on entry:	dts <- (...)
	@ on exit:	sv1 <- substituted-template
	varp	sv3			@ is template a variable or syntax?
	beq	subs_v
	@ template is not a var, is it a pointer, vector or string?	
	qpair0	sv3			@ is template an atom, var/synt, sized item or rat/cpx?
	itT	eq
	seteq	sv1, sv3		@	if so,  sv1 <- template
	seteq	pc,  cnt		@	if so,  return template
	@ template is a list: reverse it then subst car, subst cdr
	set	sv1, #null		@ sv1 <- ()
subs_3:	set	sv2, sv1		@ sv2 <- reversed template
	snoc	sv1, sv3, sv3		@ sv1 <- 1st item,		sv3 <- rest of template
	cons	sv1, sv1, sv2		@ sv1 <- (item . reversed-template)
	pntrp	sv3			@ is cdr a pointer?
	beq	subs_3			@	if so,  continue processing template
	nullp	sv3			@ was end-of-list null (i.e template was proper list)?
	@ template is a proper list, does it end with "..."?
	itT	eq
	seteq	sv3, sv1		@	if so,  sv3 <- reversed template
	seteq	sv1, #null		@	if so,  sv1 <- ()
	beq	subs_Q			@	if so,  
	@ process a template that is an improper list
	sav_rc	sv1			@ dts <- (cnt remaining (<env>) ...)
	call	substi			@ sv1 <- result of subst (sv3<-template, sv4<-bindngs, sv2 <-(env))
	restor2	sv3, cnt		@ sv3 <- remaining items,  dts <- ((<env>) ...)
subs_4:	nullp	sv3			@ nothing remaining to substitute?
	it	eq
	seteq	pc, cnt
	snoc	sv3, sv5, sv3		@ sv3 <- next item to subst, sv5 <- remaining list to subst
	save2	sv5, sv1		@ dts <- (remaining result (<env>) ...)
	car	sv1, dts		@ sv1 <- remaining list to subst
	sav__c				@ dts <- (cnt ...)
	call	substi			@ sv1 <- item (template is sv3, bindings is sv4, (env) is sv2)
	restor	cnt			@ cnt <- cnt
subs11:	restor2	sv3, sv2		@ sv3 <- remaining items in list, sv2 <- reslt, dts <- ((<env>) ..)
	cons	sv1, sv1, sv2		@ sv1 <- updated result = (item . result)
subs_Q:	nullp	sv3			@ nothing remaining to substitute?
	it	eq
	seteq	pc, cnt
	car	rva, sv3
	ldr	rvb, =ellipsis_var
	eq	rva, rvb
	bne	subs_4			@	if not, jump to process regular template list
	@ template does end with "...", substitute appropriately
	cdr	sv5, sv3		@ sv5 <- reversed template starting with var/tmplt
	car	sv3, sv5		@ sv3 <- var/tmplt that was followed by "..."
	save3	sv5, cnt, sv1		@ dts <- (rvrsd-tmplt cnt result (<env>) ...)
	car	sv1, dts		@ sv1 <- reversed template starting with var/tmplt
	call	substi			@ sv1 <- item (template is sv3, bindings is sv4, (env) is sv2)
	set	sv5, sv1		@ sv5 <- item
	restor3	sv3, cnt, sv1		@ sv3 <- rest-tmplt, cnt <- cnt, sv1 <- reslt, dts <- ((<env>) ...)
	@ here, we'd post-check if [sv3] is a var or list and proceed accordingly
	car	sv2, sv3		@ sv2 <- pattern before substitution
	eq	sv2, sv5		@ is subst-pattern = pattern?
	it	eq
	cdreq	sv3, sv3		@	if so,  sv3 <- rest of rest tmplt
	beq	subs_4			@	if so,  no bindings found, jump to process rest of pattern
	pntrp	sv2			@ is var following "..." a list?
	beq	subs_L			@	if so,  jump to post-process that
	cdr	sv3, sv3		@ sv3 <- rest of rest tmplt
subs_F:	nullp	sv5
	it	eq
	nullpeq	sv3
	it	eq
	seteq	pc, cnt
	nullp	sv5
	beq	subs_Q
	set	sv2, sv1		@ sv2 <- result
	snoc	sv1, sv5, sv5		@ sv1 <- 1st item of subst-var/tmplt, sv5 <- rst of subst_var/tmplt
	cons	sv1, sv1, sv2
	b	subs_F
subs_L:	@ substitute many things into a list of symbols	--  eg. (do 'step' var . step) ...
	snoc	sv2, sv3, sv3		@ sv2 <- template for multi-item,	sv3 <-rest of rest-tmplt
	save	sv3			@ dts <- (rest-of-rest-tmplt (<env>) ...)
	save3	sv1, sv2, sv4		@ dts <- (result template-mult bndngs rest-of-rst-tmplt (<env>) ..)
subsl0:	set	sv4, sv5		@ sv4 <- current substituted multi-template
	set	sv2, #null		@ sv2 <- result = ()
	set	sv3, #null		@ sv3 <- updated subst-template = ()
	cadr	sv5, dts		@ sv5 <- template-mult
subsl1:	car	sv1, sv4		@ sv1 <- 1st item of subst-template
	bl	typsv1			@ rva <- type tag of sv1
	eq	rva, #macro		@ is it a macro?
	it	ne
	eqne	rva, #list_tag		@	if not, is it a list?
	it	ne
	eqne	rva, #procedure		@	if not, is it a proc?
	it	ne
	bne	subsl2			@	if not, jump to splice it in
	car	sv1, sv1		@ sv1 <- 1st of multi-item  -- if not str, vec, sym
	cons	sv2, sv1, sv2		@ sv2 <- (1st-multi-item . other-items)
	eq	sv3, #macro
	beq	subsl6
	cdar	sv1, sv4		@ sv1 <- rest of 1st multi-item of subst template
	nullp	sv1			@ have we reached deepest end of multi-item?
	it	eq
	seteq	sv3, #macro		@	if so,  sv3 <- macro_tag
	beq	subsl6
	cons	sv3, sv1, sv3
	b	subsl6
subsl2:	cons	sv2, sv1, sv2		@ sv2 <- updated result = (1st-multi-item . other-items)
	eq	sv3, #macro
	beq	subsl6
	car	sv1, sv4		@	if not, sv1 <- 1st item of subst-template (restored)
	cons	sv3, sv1, sv3
subsl6:	cdr	sv5, sv5		@ sv5 <- rest of template mult
	cdr	sv4, sv4		@ sv4 <- rest of substituted template
	pntrp	sv5			@ is rest template-mult a pointer?
	beq	subsl1			@	if so,  jump to continue processing template
	nullp	sv5			@ are we done processing the multi-templt (and it was a propr lst)?
	itE	eq
	seteq	sv4, sv5		@	if so,  sv4 <- ()
	snocne	sv4, sv5, sv4		@	if not, sv4<-dottd itm, sv5<-dottd itm fr updtd subst tmplt
subsl3: @ reverse the updated result
	nullp	sv2
	beq	subsl4
	snoc	sv1, sv2, sv2
	cons	sv4, sv1, sv4
	b	subsl3
subsl4:	@ glue updated result on stack or exit with it
	set	sv1, sv4
	restor	sv2			@ sv2 <- frmr-updtd-rslt, dts<-(tmplt-mlt bnds rst-tmplt (<nv>) ..)
	cons	sv1, sv1, sv2		@ sv1 <- new updated result
	eq	sv3, #macro
	itTT	eq
	cdreq	dts, dts		@ 	if so,  dts <- (bindings rest-tmplt (<env>) ...)
	snoceq	sv4, dts, dts		@ 	if so,  sv4 <- bindings,  dts <- (rest-tmplt (<env>) ...)
	snoceq	sv3, dts, dts		@ 	if so,  sv3<-rst-tmplt, after subst expr, dts<-((<env>) ..)
	beq	subs_Q
	save	sv1			@ dts <- (new-updt-result tmplt-mult bndngs rst-tmplt (<env>) ...)
subsl5: @ reverse the updated substituted template
	nullp	sv3
	beq	subsl0
	snoc	sv1, sv3, sv3
	cons	sv5, sv1, sv5
	b	subsl5

subs_v:	@ template is a variable or syntax
	@ on entry:	sv3 <- var/synt (template)
	@ on entry:	sv4 <- substitution bindings list
	@ on exit:	sv1 <- something or other
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
  .ifndef CORE_ONLY	@ Level 2 library functions' variable var6 check
	ldr	rva, =var6_var
	eq	sv1, rva		@ is var/synt = var6 (i.e. possibly unbound)?
	beq	subszV
  .endif
	tst	sv1, #0xFF000000	@ is it a built-in var/synt?
	it	eq
	seteq	pc,  cnt
subszU:	@ is var/synt in environment?
	bl	bndchk	
	nullp	sv3			@ any binding found?
	it	ne
	setne	pc,  cnt		@	if so,  exit with var/synt
subszV:	save3	sv4, sv1, cnt
subsz3:	call	symstr
	ldr	rva, [sv1]
	asr	rva, rva, #8
	add	rva, rva, #3
	ldrb	rvb, [sv1, rva]
	add	rvb, rvb, #1
	strb	rvb, [sv1, rva]
	call	strsym			@ sv1 <- var/synt, sv3 <- null if new interned sym
	@ is var/synt in environment?
	nullp	sv3
	it	ne
	blne	bndchk
	nullp	sv3
	bne	subsz3
	car	sv5, dts		@ sv5 <- bindings
	@ is var/synt in cdr of a binding
subsz8:	nullp	sv5			@ no more bindings?
	beq	subszA
	snoc	sv2, sv5, sv5		@ sv1 <- 1st binding,  sv5 <- next binding
	cdr	sv2, sv2		@ sv2 <- val(s) of binding
	bl	typsv2			@ rvb <- type tag of sv2
	eq	rvb, #macro		@ is it a macro?
	it	ne
	eqne	rvb, #list_tag		@	if not, is it a list?
	it	ne
	bne	subsz8			@	if not, jump back to keep searching
subsz9:	nullp	sv2
	beq	subsz8
	car	sv3, sv2
	eq	sv1, sv3
	beq	subsz3
	cdr	sv2, sv2
	b	subsz9
subszA:	set	sv5, sv1
	restor3	sv4, sv3, cnt
	bcons	sv4, sv3, sv5, sv4
	set	pc,  cnt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

.endif	@	ifndef r3rs
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	5.	Program Structure
@	5.2.	Definitions:		define
@	5.3.	syntax definitions:	define-syntax
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			lcons, lambda_synt, sav__c, save, eval, npofxt
@
@	Modified by (switches):			(none)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
.balign	4
	
def:	SYMSIZE	6
	.ascii	"define"
	.balign 4
	
defsyn:	SYMSIZE	13
	.ascii	"define-syntax"
	.balign 4

pdefin:	@ (define var exp) and (define-syntax var rules)
	@ on entry:	sv1 <- var or (var arg1 ...)
	@ on entry:	sv2 <- (exp)
	@ on exit:	sv1 <- '()
	@ preserves:	none
	SYNTAX	1			@ syntax type, one input arg (others listed)
	set	sv4, sv2		@ sv4 <- (exp)
	pntrp	sv1			@ is var a pointer?
	it	ne
	carne	sv4, sv4		@	if not, sv4 <- exp
	bne	defva0			@	if not, jump to continue
	snoc	sv1, sv2, sv1		@ sv1 <- var,	sv2 <- (arg1 ...)
	ldr	sv3, =lambda_var
	lcons	sv4, sv3, sv2, sv4	@ sv4 <- (lambda (args) exp)
defva0:	@ sv1 <- var, sv4 <- exp
	vcrfi	sv3, glv, 14
	nullp	sv3
	bne	deflib
	sav__c				@ dts <- (cnt ...)
	call	vrnsrt			@ sv2 <- binding-for-var
	save	sv2			@ dts <- (binding-for-var cnt ...)
	set	sv1, sv4		@ sv1 <- exp
	call	eval			@ sv1 <- val of exp
	restor2	sv2, cnt		@ sv2 <- binding-for-var, cnt <- cnt, dts <- (...)
	setcdr	sv2, sv1
	b	npofxt
deflib:	@ define into a lib
	savrec	sv1			@ dts <- (var env cnt ...)
	set	sv1, sv4		@ sv1 <- exp
	set	env, sv3
	call	eval			@ sv1 <- val of exp
	restor	sv2			@ sv2 <- var
	restor2	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	vcrfi	sv3, glv, 14
	tst	sv2, #0xFF000000
	bne	npofxt
	and	rva, sv2, #0x7f00	@ rva <- offset in built-in env, shifted
.ifndef	cortex
	ldr	sv3, [sv3, rva, lsr #6]	@ sv3 <- built-in sub-env vector
.else
	lsr	rva, rva, #6		@ rva <- offset in built-in env
	ldr	sv3, [sv3, rva]		@ sv3 <- built-in sub-env vector
.endif
	bic	rva, sv2, #0x6000
	lsr	rva, rva, #13
	str	sv1, [sv3, rva]		@ sv1 <- symbol's printed representation (address)
	b	npofxt
	

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.1.	Equivalence predicates:		eq?, eqv?, equal?
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt, notfxt, save3, stsyeq, sav_rc, cons, save
@
@	Modified by (switches):			(none)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
.balign	4
	
seqv:	SYMSIZE	4
	.ascii	"eqv?"
	.balign 4

seq:	SYMSIZE	3
	.ascii	"eq?"
	.balign 4

peq:	@ (eq? obj1 obj2) and (eqv? obj1 obj2)
	@ on entry:	sv1 <- obj1
	@ on entry:	sv2 <- obj2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ function type, two input args
eq:	@ [internal entry]
	eq	sv1, sv2
	b	boolxt			@ (T/F ...) and pop return stack

.balign	4
	
sequal:	SYMSIZE	6
	.ascii	"equal?"
	.balign 4

pequal:	@ (equal? obj1 obj2)
	@ on entry:	sv1 <- obj1,	sv2 <-  obj2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ function type, two input args
equal:	@ [internal entry]
	eq	sv1, sv2		@ is obj1 = obj2?
	beq	trufxt			@	if so,  exit with #t
	bl	typsv1			@ rva <- type tag of obj1 (sv1)
	bl	typsv2			@ rvb <- type tag of obj2 (sv2)
	eq	rva, rvb
	bne	flsfxt
	eq	rva, #list_tag
	beq	equlst
	eq	rva, #vector_tag
	beq	veceq_
	eq	rva, #rational_tag
	it	ne
	eqne	rva, #complex_tag
	beq	equrcx
	eq	rva, #string_tag
	it	ne
	eqne	rva, #symbol_tag
	it	ne
	eqne	rva, #bytevector_tag
	bne	flsfxt
	ldr	lnk, =boolxt		@ set return link to boolxt
	ldr	pc,  =stsyeq		@ jump to test strings/bytvctrs (long jump for cortex)

_func_
equlst:	@ are lists in sv1 and sv2 equal?
	snoc	sv1, sv4, sv1		@ sv1 <- (car obj1),		sv4 <- (cdr obj1)
	snoc	sv2, sv5, sv2		@ sv2 <- (car obj2),		sv5 <- (cdr obj2)
	save3	sv4, sv5, cnt		@ dts <- ((cdr obj1) (cdr obj2) cnt ...)
	call	equal			@ sv1 <- #t/#f, from (equal sv1 sv2)
	eq	sv1, #t			@ did cars match?
	restor3	sv1, sv2, cnt		@ sv1 <- (cdr obj1),  sv2 <- (cdr obj2),  cnt <- cnt, dts <- (...)
	bne	boolxt			@	if not, exit with #f
	b	equal			@ jump to test cdrs
	
_func_
veceq_:	@ check equality of vectors in sv1 and sv2
	veclen	sv5, sv1		@ sv5 <- size of obj1 (scheme int)
	veclen	sv4, sv2		@ sv5 <- size of obj2 (scheme int)
	eq	sv4, sv5
	bne	flsfxt
	cons	sv1, sv1, sv2		@ sv1 <- (vec1 . vec2)
	sav_rc	sv1			@ dts <- ((vec1 .  vec2) cnt ...)
veceq0:	@ are vectors on stack ((vec1 . vec2) ...) equal (sv5 starts as offset of last item)?
	izerop	sv5			@ done comparing vectors?
	beq	veceqx			@	if so,  jump to exit
	car	sv4, dts		@ sv4 <- (vec1 . vec2)
	snoc	sv1, sv2, sv4		@ sv1 <- vec1,  sv2 <- vec2
	decr	sv5, sv5		@ sv5 <- offset of item to compare
	vecref	sv1, sv1, sv5		@ sv1 <- item from vec2
	vecref	sv2, sv2, sv5		@ sv2 <- item from vec1
	save	sv5			@ dts <- (current-offset (vec1 vec2) cnt ...)
	call	equal			@ sv1 <- #t/#f, from (equal sv1 sv2)
	restor	sv5			@ sv5 <- current-offset,	dts <- ((vec1 vec2) cnt ...)
	eq	sv1,  #t		@ were items the same?
	beq	veceq0			@	if so,  jump to compare next items
veceqx:	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,			dts <- (...)
	b	boolxt			@ jump to exit with #t/#f

_func_
equrcx:	@ are sv1 and sv2 equal rat/cpx?
	snoc	rva, rvc, sv1		@ rva <- word 1, rvc <- word 2 of x1, possible rat/cpx
	car	rvb, sv2		@ rvb <- word 1 of x2, possible rat/cpx
	eq	rva, rvb		@ are word 1 of x1 and x2 the same?
	itT	eq
	cdreq	rvb, sv2		@	if so,  rvb <- word 2 of x2
	eqeq	rvc, rvb		@	if so,  are word 2 of x1 and x2 the same?
	b	boolxt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				INTEGER ONLY
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.2.	Numbers
@	6.2.5	Numerical operations:	number?, =, <, >, <=, >=, +, *, -, /,
@					quotient, remainder, modulo
@	6.2.6	Numerical input output:	number->string, string->number
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.ifdef	integer_only
  .include "common/armpit_scheme_base_6.2.Integers.s"
.endif

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				GENERAL NUMBERS
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
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
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.ifndef	integer_only
  .include "common/armpit_scheme_base_6.2.Numbers.s"
.endif

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.2.	Pairs and list:		pair?, cons, car, cdr, set-car!, set-cdr!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt, notfxt, error4, npofxt, flsfxt,
@						i0fxt, infxt, list, save, sav_rc, heapbottom
@		6.1.	Equivalence predicates:	equal
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4
	
_func_
pairen:	@ checking that sv1 is a pair (or not) for paptbl (entry to function)
	orr	lnk, sv5, #lnkbit0	@ lnk <- made odd if Thumb2
_func_	
ckpair:
	qpair2	sv1			@ is it an atom, var/synt, rat/cpx, sized item, syntax or raw code?
	set	pc, lnk			@ return
	
.balign	4
	
pair:	SYMSIZE	5
	.ascii	"pair?"
	.balign 4

ppair:	@ (pair? obj)
	@ on entry:	sv1 <-obj
	@ on exit:	sv1 <- #t/#f
	@ (1 << 8):		one input arg
	@ (0 << 12):		primitive (not lambda, not compiled)
	@ (opairen << 16):	starts with jump to pairen
	@ (0 << 24):		no sv4 startup
	@ pre-entry:	pairen sets ne flag if obj (sv1) is a pair
	EPFUNC	0, opairen, 1		@ primitive, init-sv4 = none, fentry = pairen, narg = 1
	b	notfxt			@ return with #f/#t
	
.balign	4
	
scons:	SYMSIZE	4
	.ascii	"cons"
	.balign 4

pcons:	@ (cons item1 item2)
	@ on entry:	sv1 <- item1
	@ on entry:	sv2 <- item2
	@ on exit:	sv1 <- (item1 . item2)
	PFUNC	2			@ primitive function, two input args
	cons	sv1, sv1, sv2		@ sv1 <- (item1 . item2)
	set	pc,  cnt		@ return with (item1 . item2)
	
.balign	4
	
car:	SYMSIZE	3
	.ascii	"car"
	.balign 4

pcar:	@ (car list)
	@ on entry:	sv1 <- list
	@ on exit:	sv1 <- car of list
	@ (1 << 8):		one input arg
	@ (0 << 12):		primitive (not lambda, not compiled)
	@ (opairen << 16):	starts with jump to pairen
	@ (0 << 24):		no sv4 startup
	@ pre-entry:	pairen sets ne flag if obj (sv1) is a pair
	EPFUNC	0, opairen, 1		@ primitive, init-sv4 = none, fentry = pairen, narg = 1
	itT	ne
	carne	sv1, sv1
	setne	pc,  cnt
	ldr	sv4, =car
	b	error4			@ jump to signal error

.balign	4
	
cdr:	SYMSIZE	3
	.ascii	"cdr"
	.balign 4

pcdr:	@ (cdr list)
	@ on entry:	sv1 <- list
	@ on exit:	sv1 <- cdr of list
	@ (1 << 8):		one input arg
	@ (0 << 12):		primitive (not lambda, not compiled)
	@ (opairen << 16):	starts with jump to pairen
	@ (0 << 24):		no sv4 startup
	@ pre-entry:	pairen sets ne flag if list (sv1) is a pair
	EPFUNC	0, opairen, 1		@ primitive, init-sv4 = none, fentry = pairen, narg = 1
	itT	ne
	cdrne	sv1, sv1
	setne	pc,  cnt
	ldr	sv4, =cdr
	b	error4			@ jump to signal error

.balign	4
	
setcar:	SYMSIZE	8
	.ascii	"set-car!"
	.balign 4

pstcar:	@ (set-car! pair obj)
	@ on entry:	sv1 <- pair
	@ on entry:	sv2 <- obj
	@ on exit:	sv1 <- npo
	@ (2 << 8):		one input arg
	@ (0 << 12):		primitive (not lambda, not compiled)
	@ (opairen << 16):	starts with jump to pairen
	@ (0 << 24):		no sv4 startup
	@ pre-entry:	pairen sets ne flag if pair (sv1) is a pair
	EPFUNC	0, opairen, 2		@ primitive, init-sv4 = none, fentry = pairen, narg = 2
	itT	ne
	setcarne sv1, sv2		@ store obj in car of pair (side-effect)
	bne	npofxt			@ return with npo
	ldr	sv4, =setcar
	b	error4			@ jump to signal error
	
.balign	4
	
setcdr:	SYMSIZE	8
	.ascii	"set-cdr!"
	.balign 4

pstcdr:	@ (set-cdr! pair obj)
	@ on entry:	sv1 <- pair
	@ on entry:	sv2 <- obj
	@ on exit:	sv1 <- npo
	@ (2 << 8):		one input arg
	@ (0 << 12):		primitive (not lambda, not compiled)
	@ (opairen << 16):	starts with jump to pairen
	@ (0 << 24):		no sv4 startup
	@ pre-entry:	pairen sets ne flag if pair (sv1) is a pair
	EPFUNC	0, opairen, 2		@ primitive, init-sv4 = none, fentry = pairen, narg = 2
	itT	ne
	setcdrne sv1, sv2		@ store obj in car of pair (side-effect)
	bne	npofxt			@ return with npo
	ldr	sv4, =setcdr
	b	error4			@ jump to signal error

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.3.	Symbols:		symbol?, symbol->string, string->symbol
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt, scmenv, subcpy, stsyeq, run_no_irq,
@						run_normal, I2C0ADR, clngc, error4
@
@	Modified by (switches):			cortex, oba_above_heap, mark_and_sweep
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
.balign	4
	
symbol:	SYMSIZE	7
	.ascii	"symbol?"
	.balign 4

psymbl:	@ (symbol? obj)
	@ on entry:	 sv1 <- obj
	@ on exit:	 sv1 <- #t/#f
	@ (1 << 8):		one input arg
	@ (0 << 12):		primitive (not lambda, not compiled)
	@ (otypchk << 16):	starts with jump to typchk
	@ (ivar << 24):		startup value for sv4: ivar (variable type for check) as scheme int
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	ivar, otypchk, 1	@ primitive, init-sv4 = var-tag, fentry = typchk, narg = 1

.balign	4
	
ssmstr:	SYMSIZE	14
	.ascii	"symbol->string"
	.balign 4

psmstr:	@ (symbol->string symbol)
	@ on entry:	 sv1 <- symbol-id
	@ on exit:	 sv1 <- string
	PFUNC	1			@ primitive function, one input arg
symstr:	@ [internal entry]
	vcrfi	sv3, glv, 14
	nullp	sv3
	bne	symst1
        vcrfi	sv3, glv, 8		@ sv3 <- obarray
symst0:	@ scan current obarray for symbol
	nullp	sv3			@ is current obarray exhausted?
	beq	symst1			@	if so,  jump to scan built-in obarray
	cdar	sv2, sv3		@ sv2 <- obarray binding symbol-id
	eq	sv1, sv2		@ are symbol-ids the same?
	itT	eq
	caareq	sv1, sv3		@	if so,  sv1 <- symbol's printed representation (address)
	beq	symst2			@	if so,  jump to exit with symbol as string
	cdr	sv3, sv3		@ sv3 <- rest of obarray
	b	symst0			@ continue scanning obarray
symst1:	@ symstr for built-in symbol
	tst	sv1, #0xFF000000	@
	bne	symst3
	vcrfi	sv2, glv, 14		@ sv1 <- lib built-in env vector (in case we're within parse)
	nullp	sv2			@ are we in parse mode?
	it	eq
	vcrfieq	sv2, glv, 13		@	if not, sv1 <- built-in env vector
	and	rva, sv1, #0x7f00	@ rva <- offset in built-in env, shifted
	lsr	rva, rva, #6		@ rva <- offset in built-in env
	veclen	rvb, sv2
	cmp	rva, rvb
	bpl	symst3
	ldr	sv2, [sv2, rva]		@ sv2 <- built-in sub-env vector
	sub	rvb, sv1, #0x8000	@ rvb <- offset in built-in sub-env, shifted
	bic	rva, rvb, #0x6000
	lsr	rva, rva, #13		@ rvb <- offset in built-in sub-env
	veclen	rvb, sv2
	cmp	rva, rvb
	bpl	symst3
	ldr	sv1, [sv2, rva]		@ sv1 <- symbol's printed representation (address)
symst2:	@ finish up: copy printed rep into string and return
	set	sv2, #0x11		@ sv2 <- position of 1st char, adjusted for header (scheme int)
	strlen	sv3, sv1		@ sv3 <- position after last char (scheme int)
	add	sv3, sv3, #16		@ sv3 <- position after last char, adjusted for header (scheme int)
	set	lnk, cnt
	b	subcpy

symst3:	@ undefined symbol
	adr	sv1, undef_		@ sv1 <- undefined symbol
	b	symst2
	
.balign	4
	
undef_:	SYMSIZE	4
	.ascii	"#<?>"
	.balign 4

.balign	4
	
sstsym:	SYMSIZE	14
	.ascii	"string->symbol"
	.balign 4

pstsym:	@ (string->symbol string)
	@ on entry:	 sv1 <- string
	@ on exit:	 sv1 <- symbol-id
	@ on exit:	 sv3 <- result indicator: null if new symbol was interned, #t otherwise
	PFUNC	1			@ primitive function, one input arg
strsym:	@ [internal entry]
	vcrfi	sv4, glv, 14		@ sv4 <- library-built-in env vector
	nullp	sv4			@ are we in normal (non-parse) mode?
	it	eq
	vcrfieq	sv4, glv, 13		@	if so, sv4 <- built-in env vector
	veclen	sv5, sv4		@ sv5 <- size of built-in env (scheme int)
strsy0:	@
	bic	rvb, sv5, #1		@ rvb <- sub-env index
	ldr	sv4, [sv4, rvb]		@ sv4 <- sub-env
	veclen	rvc, sv4		@ rvc <- size of built-in sub-env (scheme int)
	add	rvc, rvc, #3
strsyL:	subs	rvc, rvc, #8
	bmi	strsyM
	ldr	sv2, [sv4, rvc]
	bl	stsyeq
	bne	strsyL
	set	sv3, #t	
strsy7:	@ construct built-in symbol id and exit
	set	rvb, #variable_tag
	bic	rva, sv5, #1
	orr	sv1, rvb, rva, lsl #6
	add	rvc, rvc, #4
	orr	sv1, sv1, rvc, lsl #13
	set	pc,  cnt
strsyM:	vcrfi	sv4, glv, 14		@ sv4 <- library-built-in env vector
	sub	sv5, sv5, #4
	eq	sv5, #i0
	it	eq
	nullpeq	sv4
	beq	strsyu
	eq	sv5, #i0
	beq	strsy6
	nullp	sv4			@ are we in normal (non-parse) mode?
	it	eq
	vcrfieq	sv4, glv, 13		@	if so, sv4 <- built-in env vector
	beq	strsy0
	vcrfi	rva, glv, 15		@	if not, sv4 <- library-parse/parse-export-mode
	eq	rva, #5			@ are we in library-parse-export-mode?
	bne	strsy0
strsy6:	@ intern string as new symbol in private/public lib-env
	@ sv1 is input string
	@ sv5 is sub-env position - 1 (scheme int)
	add	sv5, sv5, #4
	set	sv2, #0x11		@ sv2 <- position of 1st char, adjusted for header (scheme int)
	strlen	sv3, sv1		@ sv3 <- position after last char (scheme int)
	add	sv3, sv3, #16		@ sv3 <- position after last char, adjusted for header (scheme int)
	bl	subcpy			@ sv1 <- copy-of-string	
	tbrfi	rva, sv1, 0
	bic	rva, rva, #0xFF
	orr	rva, rva, #symbol_tag
	tbsti	rva, sv1, 0		@ sv1 <- string-as-symbol
	@ make a new vector of size rvc+
	@ copy from sv4 to that
	@ add symbol and #t as binding
	vcrfi	sv4, glv, 14		@ sv4 <- library-built-in env vector
	bic	rvb, sv5, #1		@ rvb <- offset to sub-env
	ldr	sv4, [sv4, rvb]		@ sv4 <- sub-env
	veclen	sv3, sv4		@ sv3 <- size of sub-env (scheme int)
	add	sv3, sv3, #8
	bic	rvb, sv3, #0x03
	add	rvb, rvb, #4
	bl	zmaloc
	lsl	rvc, sv3, #6
	orr	rvc, rvc, #vector_tag
	str	rvc, [rva]
	lsr	rvc, rvc, #6
	bic	rvc, rvc, #0x03
	set	sv2, #t
	str	sv2, [rva, rvc]
	sub	rvc, rvc, #4
	str	sv1, [rva, rvc]
strsyO:	subs	rvc, rvc, #4
	itT	ne
	ldrne	sv1, [sv4, rvc]
	strne	sv1, [rva, rvc]
	bne	strsyO
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv3, rva, rvb		@ sv1 <- address of object (symbol), [*commit vector destination*]
	orr	fre, rva, #0x02		@ fre <- updated and de-reserved, [*restart critical instruction*]
	@ store that back at proper offset in glv, 14
	vcrfi	sv4, glv, 14		@ sv4 <- library-built-in env vector
	bic	rvb, sv5, #1		@ rvb <- offset to sub-env
	str	sv3, [sv4, rvb]		@ store updated sub-env into lib-env
	@ set rvc properly then jump to construct built-in symbol id and exit
	veclen	rvc, sv3
	bic	rvc, rvc, #0x03
	sub	rvc, rvc, #4
	set	sv3, #null
	b	strsy7			@ jump to construct built-in symbol id and exit

strsyu:	@ look for symbol in user obarray
	swi	run_no_irq              @ disable interrupts (user mode)
        vcrfi	sv4, glv, 8             @ sv4 <- obarray
strsy1:	@ scan current obarray (in sv4)
	nullp	sv4			@ symbol not found in obarray?
	beq	strsy2			@	if so,  jump to build new symbol and id
	car	sv5, sv4		@ sv5 <- 1st binding in obarray
	car	sv2, sv5		@ sv2 <- symbol of 1st binding
	bl	stsyeq			@ is obarray symbol = string searched for?
	it	ne
	cdrne	sv4, sv4		@	if not, sv4 <- rest of obarray
	bne	strsy1			@	if not, jump to keep scanning obarray
	cdr	sv1, sv5		@ sv1 <- id{var/synt}
	swi	run_normal              @ enable interrupts (user mode)
	set	sv3, #t
	set	pc,  cnt
strsy2:	@ make a new symbol and id for string
.ifndef	oba_above_heap
	set	sv2, #0x11		@ sv2 <- position of 1st char, adjusted for header (scheme int)
	strlen	sv3, sv1		@ sv3 <- position after last char (scheme int)
	add	sv3, sv3, #16		@ sv3 <- position after last char, adjusted for header (scheme int)
	bl	subcpy			@ sv1 <- copy-of-string	
	tbrfi	rva, sv1, 0
	bic	rva, rva, #0xFF
	orr	rva, rva, #symbol_tag
	tbsti	rva, sv1, 0		@ sv1 <- string-as-symbol
        vcrfi	sv4, glv, 8             @ sv4 <- obarray
	set	rvc, #variable_tag
	nullp	sv4			@ is obarray empty?
	itTT	eq
	ldreq	rva, =I2C0ADR		@	if so,  rva <- address of mcu-id
	ldrbeq	rva, [rva]		@	if so,  rva <- mcu-id from i2c0adr (raw int)
	orreq	sv2, rvc, rva, LSL #23	@	if so,  sv2 <- blank symbol id
	itT	ne
	snocne	sv2, sv5, sv4
	cdrne	sv2, sv2
	add	sv2, sv2, #0x0100
	bcons	sv1, sv1, sv2, sv4	@ sv1 <- updated obarray = ((symbol . new-obj-id) . old-obarray)
        vcsti	glv, 8, sv1             @ update obarray in global vector
	cdar	sv1, sv1		@ sv1 <- new obarray symid
	swi	run_normal              @ enable interrupts (user mode)
	set	sv3, #t
	set	pc,  cnt		@ return new obj id
.else
	bl	gc			@ rva <- amount of free memory bytes, perform garbage collection
  .ifndef mark_and_sweep
	vcrfi	rvb, glv, 9		@ rvb <- heaptop0 -- from global vector
	cmp	fre, rvb		@ is upper heap in use?
	it	pl
	blpl	gc			@	if so,  rva <- amount of free memory bytes, perform gc
	lsl	rva, rva, #1		@ rva <- number of free bytes *2 (free bytes availbl in both heaps)
  .endif
	strlen	sv4, sv1		@ sv4 <- number of chars in string (scheme int)
	lsr	rvb, sv4, #2		@ rvb <- number of bytes of object
	add	rvb, rvb, #20
	add	rvb, rvb, #0x1f		@ rvb <- number of bytes of obj + 31 (15 to align, 16 as spacer)
	bic	rvb, rvb, #0x0f		@ rvb <- number of bytes needed for obj + spacer (16-byte aligned)
	cmp	rvb, rva		@ is enough room available?
	bpl	stsyer			@	if not, jump to exit with error
  .ifndef mark_and_sweep
	@ update heaptop0 and heaptop1
	vcrfi	rva, glv, 10		@ rva <- heaptop1 -- from global vector
	sub	rva, rva, rvb		@ rva <- heaptop1 minus size of code
	vcsti	glv, 10, rva		@ store it back in global vector as heaptop1
	vcrfi	rvc, glv, 9		@ rvc <- heaptop0 -- from global vector
	sub	rvc, rvc, rvb, lsr #1	@ rvc <- heaptop0 minus size of code / 2
	vcsti	glv, 9, rvc		@ store heaptop0 back in global vector
	vcsti	glv, 1, rvc		@ store it back in global vector as heaptop
  .else
	@ update heaptop
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, rvb		@ rva <- heaptop minus size of code
	vcsti	glv, 1, rva		@ store it back in global vector as heaptop
  .endif
	@ upate obarray
	add	rva, rva, #11		@ rva <- start target address
	add	rvc, rva, #8
	str	rvc, [rva]
	add	rvc, rva, #16
	str	rvc, [rva, #8]
        vcrfi	sv4, glv, 8             @ sv4 <- obarray
	str	sv4, [rva, #4]
	vcsti	glv, 8, rva
	set	rvb, rva		@ rvb <- target address, saved against inRAMp
	set	rvc, #variable_tag
	nullp	sv4			@ is obarray empty?
	itTT	eq
	ldreq	rva, =I2C0ADR		@	if so,  rva <- address of mcu-id
	ldrbeq	rva, [rva]		@	if so,  rva <- mcu-id from i2c0adr (raw int)
	orreq	sv2, rvc, rva, LSL #23	@	if so,  sv2 <- blank symbol id
	itT	ne
	snocne	sv2, sv5, sv4
	cdrne	sv2, sv2
	add	sv2, sv2, #0x0100
	set	rva, rvb		@ rva <- target address, restored
	str	sv2, [rva, #12]
	car	rvc, sv1
	bic	rvc, rvc, #0xff
	orr	rvc, rvc, #symbol_tag
	str	rvc, [rva, #16]
	lsr	rvc, rvc, #10
	add	rvc, rvc, #1
	add	rvb, sv1, #4
	add	rva, rva, #20
stsylp:	subs	rvc, rvc, #1
	ldr	sv1, [rvb, rvc, lsl #2]
	str	sv1, [rva, rvc, lsl #2]
	bne	stsylp
	set	sv1, sv2	
	swi	run_normal              @ enable interrupts (user mode)
	set	sv3, #t
	set	pc,  cnt
stsyer:	@ signal error (out of memory)
	ldr	sv4, =pstsym
	swi	run_normal
	b	error4
.endif

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.4.	Characters:		char?, char=?, char<?, char>?, char<=?,
@					char>=?, char->integer, integer->char
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4
	
char:	SYMSIZE	5
	.ascii	"char?"
	.balign 4

pchar:	@ (char? obj):	
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	ichr, otypchk, 1	@  primitive, init-sv4 = chr-tag, fentry = typchk, narg = 1

.balign	4
	
chareq:	SYMSIZE	6
	.ascii	"char=?"
	.balign 4

pcheq:	@ (char=? char1 char2):	
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	.word	proc | 0x0200
ichreq:	@ [internal entry]
	eq	sv1, sv2
	b	boolxt

.balign	4
	
charlt:	SYMSIZE	6
	.ascii	"char<?"
	.balign 4

pchlt:	@ (char<? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	.word	proc | 0x0200
ichrlt:	@ [internal entry]
	cmp	sv1, sv2
	b	chrgt0

.balign	4
	
chargt:	SYMSIZE	6
	.ascii	"char>?"
	.balign 4

pchgt:	@ (char>? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	.word	proc | 0x0200
ichrgt:	@ [internal entry]
	cmp	sv2, sv1
chrgt0:	itE	mi
	setmi	sv1, #t
	setpl	sv1, #f
	set	pc,  cnt

.balign	4
	
charle:	SYMSIZE	7
	.ascii	"char<=?"
	.balign 4

pchle:	@ (char<=? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	.word	proc | 0x0200
ichrle:	@ [internal entry]
	cmp	sv2, sv1
	b	chrge0

.balign	4
	
charge:	SYMSIZE	7
	.ascii	"char>=?"
	.balign 4

pchge:	@ (char>=? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	.word	proc | 0x0200
ichrge:	@ [internal entry]
	cmp	sv1, sv2
chrge0:	itE	pl
	setpl	sv1, #t
	setmi	sv1, #f
	set	pc,  cnt

.balign	4
	
chrint:	SYMSIZE	13
	.ascii	"char->integer"
	.balign 4

pchint:	@ (char->integer char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- int
	.word	proc | 0x0100
ichint:	@ [internal entry]
	chr2raw	rva, sv1
	raw2int	sv1, rva
	set	pc,  cnt

.balign	4
	
intchr:	SYMSIZE	13
	.ascii	"integer->char"
	.balign 4

pintch:	@ (integer->char int)
	@ on entry:	sv1 <- int
	@ on exit:	sv1 <- char
	.word	proc | 0x0100
	int2raw	rva, sv1
	raw2chr	sv1, rva
	set	pc,  cnt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.5.	Strings:		string?, make-string, string-length,
@					string-ref, string-set!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt, zmaloc, subcpy
@		6.1.	Equivalence predicates:	equal
@		6.3.4.	Characters:		ichceq, ichrlt, ichrgt, ichrle, ichrge, ichclt,
@						ichcgt, ichcle, ichcge
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4
	
sqstng:	SYMSIZE	7
	.ascii	"string?"
	.balign 4

qstrng:	@ (string? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	istr, otypchk, 1	@ primitive, init-sv4 = str-tag, fentry = typchk, narg = 1

.balign	4
	
smakst:	SYMSIZE	11
	.ascii	"make-string"
	.balign 4

makstr:	@ (make-string k <char>)
	@ on entry:	sv1 <- k
	@ on entry:	sv2 <- char or '()
	@ on exit:	sv1 <- string
	PFUNC	2			@ primitive function, two input args
	set	sv3, sv1		@ sv3 <- size of string to allocate
	straloc	sv1, sv3		@ sv1 <- allocated string of size sv1
	nullp	sv2			@ was fill specified?
	itE	eq
	seteq	rvb, #0			@	if not, rvb <- 0
	lsrne	rvb, sv2, #8
fill8:	@ [internal entry]
	and	rvb, rvb, #0xff
	orr	rvb, rvb, rvb, lsl #8
	orr	rvb, rvb, rvb, lsl #16
	int2raw	rva, sv3
	add	rva, rva, #7
	bic	rva, rva, #3
fill8l:	subs	rva, rva, #4
	itT	ne
	strne	rvb, [sv1, rva]
	bne	fill8l
	set	pc,  cnt

.balign	4
	
sstlen:	SYMSIZE	13
	.ascii	"string-length"
	.balign 4

strlen:	@ (string-length string)
	@ on entry:	sv1 <- string
	@ on exit:	sv1 <- int
	PFUNC	1			@ primitive function, one input arg
	strlen	sv1, sv1		@ sv1 <- string length (scheme int)
	set	pc,  cnt

.balign	4
	
sstref:	SYMSIZE	10
	.ascii	"string-ref"
	.balign 4

strref:	@ (string-ref string k)
	@ on entry:	sv1 <- string
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- char
	PFUNC	2			@ primitive function, two input args
	strref	sv1, sv1, sv2		@ sv1 <- character (scheme char)
	set	pc,  cnt

.balign	4
	
sstset:	SYMSIZE	11
	.ascii	"string-set!"
	.balign 4

strset:	@ (string-set! string k char)
	@ on entry:	sv1 <- string
	@ on entry:	sv2 <- k
	@ on entry:	sv3 <- char
	@ on exit:	sv1 <- string
	PFUNC	3			@ primitive function, three input args
	strset	sv3, sv1, sv2		@
	set	pc,  cnt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.6.	Vectors:		vector?, make-vector, vector-length,
@					vector-ref, vector-set!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt, npofxt, zmaloc, cons
@		6.3.2.	Pairs and lists:	length
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4

sqvect:	SYMSIZE	7
	.ascii	"vector?"
	.balign 4

qvecto:	@ (vector? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	ivec, otypchk, 1	@ primitive, init-sv4 = vec-tag, fentry = typchk, narg = 1

.balign	4
	
smkvec:	SYMSIZE	11
	.ascii	"make-vector"
	.balign 4

pmkvec:	@ (make-vector size <fill>)
	@ on entry:	sv1 <- size
	@ on entry:	sv2 <- (<fill>)
	@ on exit:	sv1 <- vector == #(fill fill ...)
	@ preserves:	sv4-sv5
	PFUNC	1			@ primitive function, one input arg
makvec:	@ [internal entry]
	@ allocate memory for object
	set	sv3, sv1		@ sv3 <- size (saved)
	bic	rvb, sv1, #3		@ rvb <- #bytes to allocate for data
	add	rvb, rvb, #4		@ rvb <- #bytes to allocate + header's 4
	bl	zmaloc			@ rva <- addr of object (symbol-taggd), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- address of object (symbol), [*commit vector destination*]
	orr	fre, rva, #0x02		@ fre <- updated and de-reserved, [*restart critical instruction*]
	@ clear contents of object (for gc)
	set	rva, #null		@ rva <- '(), initial neutral value
makve0:	subs	rvb, rvb, #4		@ rvb <- offset of next item, is it zero?
	it	ne
	strne	rva, [sv1, rvb]		@	if not, store item in vector
	bne	makve0			@	if not, jump to keep storing neutral val in vector
	@ update tag to vector
	lsl	rva, sv3, #6		@ rva <- vector size, shifted in place
	orr	rva, rva, #vector_tag	@ rva <- sized vector tag
	str	rva, [sv1]		@ set sized vector tag in vector
	@ store fill items in vector
	pntrp	sv2			@ is fill item non-null?
	it	eq
	careq	sv2, sv2		@	if so,  sv2 <- fill item (else sv2 <- scheme_null)
vecfil:	@ [internal entry]
	veclen	sv4, sv1		@ sv4 <- number of items (scheme int)
vecfi0:	izerop	sv4			@ done filling vector?
	it	eq
	seteq	pc,  cnt		@	if so,  exit
	decr	sv4, sv4		@ sv4 <- position of next vector item
	vecset	sv1, sv4, sv2		@ store fill item in vector
	b	vecfi0			@ jump to continue

.balign	4
	
svclen:	SYMSIZE	13
	.ascii	"vector-length"
	.balign 4

veclen:	@ (vector-length vector)
	@ on entry:	sv1 <- vector
	@ on exit:	sv1 <- int
	PFUNC	1			@ primitive function, one input arg
	veclen	sv1, sv1		@ sv1 <- vector length (scheme int)
	set	pc,  cnt

.balign	4
	
svcref:	SYMSIZE	10
	.ascii	"vector-ref"
	.balign 4

vecref:	@ (vector-ref vector k)
	@ on entry:	sv1 <- vector
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- item from vector
	PFUNC	2			@ primitive function, two input args
	vecref	sv1, sv1, sv2
	set	pc,  cnt

.balign	4
	
svcset:	SYMSIZE	11
	.ascii	"vector-set!"
	.balign 4

vecset:	@ (vector-set! vector k item)
	@ on entry:	sv1 <- vector
	@ on entry:	sv2 <- k
	@ on entry:	sv3 <- item
	@ on exit:	sv1 <- #npo
	PFUNC	3			@ primitive function, three input args
	vecset	sv1, sv2, sv3
	b	npofxt			@ return with npo

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.4.	control features:	procedure?, apply, call/cc,
@					call-with-current-continuation, values,
@					call-with-values, dynamic-wind
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			boolxt, save, list, apply, save3, bndchk, winders_var
@						sav_rc, eval, savrec, bcons, cons
@
@	Modified by (switches):			CORE_ONLY
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4
	
sprocd:	SYMSIZE	10
	.ascii	"procedure?"
	.balign 4

proced:	@ (procedure? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	iprc, otypchk, 1	@ primitive, init-sv4 = proc-tag, fentry = typchk, narg = 1
	
.balign	4
	
sapply:	SYMSIZE	5
	.ascii	"apply"
	.balign 4

papply:	@ (apply fun arg1 ... args)
	@ on entry:	sv1 <- fun
	@ on entry:	sv2 <- argl
	@ on exit:	sv1 <- result
	PFUNC	1			@ primitive function, one input arg
	set	sv4, sv2		@ sv4 <- (arg1 arg2 ... args)
	set	sv3, #null		@ sv3 <- '()
	list	sv5, sv3		@ sv5 <- (()) = tail-ref
	save	sv5			@ dts <- ((()) ...)
paply0:	snoc	sv3, sv4, sv4		@ sv3 <- arg1,		sv4 <- (arg2 ... args)
	nullp	sv4
	beq	paply1
	list	sv3, sv3		@ sv3 <- (arg1)
	setcdr	sv5, sv3		@ dts <- ((() arg1) ...)
	set	sv5, sv3		@ sv5 <- new tail-ref
	b	paply0
paply1:	setcdr	sv5, sv3		@ dts <- ((() arg1 ... . args) ...)
	restor	sv2			@ sv2 <- (() arg1 ... . args),		dts <- (...)
	cdr	sv2, sv2		@ sv2 <- (arg1 ... . args)
	b	apply

.balign	4

scllcc:	SYMSIZE	7
	.ascii	"call/cc"
	.balign 4

scwcc:	SYMSIZE	30
	.ascii	"call-with-current-continuation"
	.balign 4

callcc:	@ (call/cc procedure)
	@ on entry:	sv1 <- procedure
	@ on exit:	sv1 <- result of applying procedure on current continuation
	PFUNC	1			@ primitive function, one input arg
icalcc:	@ [internal entry]
	lcons	sv4, cnt, env, dts	@ sv4 <- (cnt env ...)	
	set	sv2, sv1
	ldr	sv1, =winders_var
	bl	bndchk
	pntrp	sv3
	itE	eq
	seteq	sv1, sv5
	setne	sv1, #null
	set	sv3, #procedure		@ sv3 <- procedure tag
	orr	sv3, sv3, #0x8000	@ sv2 <- continuation tag
	lcons	sv3, sv3, sv1, sv4	@ sv3 <- (#continuation winders-list cnt env ...) = continuation
	set	sv1, sv2
	list	sv2, sv3		@ sv2 <- (continuation)
	b	apply

.ifndef r3rs

.balign	4
	
svalus:	SYMSIZE	6
	.ascii	"values"
	.balign 4

values:	@ (values obj ...)
	@ on entry:	sv1 <- (obj ...)
	@ on exit:	sv1 <- (obj ...)
	@ pre-entry:	return returns directly via cnt
	EPFUNC	0, oreturn, 0		@ primitive, init-sv4 = none, fentry = return, narg = 0
	
.balign	4
	
scllwv:	SYMSIZE	16
	.ascii	"call-with-values"
	.balign 4

callwv:	@ (call-with-values producer consumer)
	@ on entry:	sv1 <- producer
	@ on entry:	sv2 <- consumer
	PFUNC	2			@ primitive function, two input args
	list	sv1, sv1		@ sv1 <- (producer) = expr
	sav_rc	sv2			@ dts <- (consumer cnt ...)
	call	eval
	set	sv2, sv1		@ sv2 <- values
	restor2	sv1, cnt		@ sv1 <- consumer, cnt <- cnt, dts <- (...)
	pntrp	sv2			@ are values in a list?
	it	ne
	nullpne	sv2			@	if not, is values = '()?
	beq	apply			@	if so,  jump to apply
	list	sv2, sv2		@ sv2 <- (value)
	b	apply			@ jump to apply

.balign	4

sdnwnd:	SYMSIZE	12
	.ascii	"dynamic-wind"
	.balign 4

dynwnd:	@ (dynamic-wind before thunk after)
	@ on entry:	sv1 <- before
	@ on entry:	sv2 <- thunk
	@ on entry:	sv3 <- after
	PFUNC	3			@ primitive function, three input args
	savrec	sv2			@ dts <- (thunk env cnt ...)
	bcons	dts, sv1, sv3, dts	@ dts <- ((before . after) thunk env cnt ...)
	set	sv2, #null
	call	apply			@ sv1 <- result of applying before to '()
	caddr	env, dts		@ env <- saved env
	ldr	sv1, =winders_var
	set	sv4, sv1
	bl	bndchk			@ sv1 <- binding for _winders or #t
	set	sv1, sv3
	restor2	sv3, sv2		@ sv3 <- (before . after), sv2 <- thunk, dts <- (env cnt ...)
	eq	sv1, #t
	bne	dynwn2
	@ here, we could (define _winders '()) into current env
	@ if _winders had not been already placed in (interaction-environment)
	@ but _winders is there so there's nothing to do here.
	@ So, report an error in case something went wrong somewhere.
	b	corerr
dynwn2:	@ store
	cons	sv3, sv3, sv5
	setcdr	sv1, sv3
	set	sv1, sv2
	set	sv2, #null
	call	apply
	car	env, dts
	save	sv1			@ dts <- (result env cnt ...)
	ldr	sv1, =winders_var
	bl	bndchk			@ sv1 <- binding for _winders
	snoc	sv2, sv5, sv5
	setcdr	sv3, sv5
	cdr	sv1, sv2		@ sv1 <- after
	set	sv2, #null
	call	apply
	restor3	sv1, env, cnt
	set	pc,  cnt

.endif
	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.5.	eval:			eval, scheme-report-environment,
@					null-environment, interaction-environment
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:			core:	eval, error4
@
@	Modified by (switches):		none
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4

seval:	SYMSIZE	4
	.ascii	"eval"
	.balign 4

peval:	@ (eval expr env)
	@ on entry:	sv1 <- expr
	@ on entry:	sv2 <- env
	@ on exit:	sv1 <- result
	PFUNC	2			@ primitive function, two input args
	@ peval can set env to sv2 but won't unset it upon exit
	@ so it might be possible that some ensuing statement would use this new env (?)
	@ note:	 it's not possible if eval is within a begin but could potentially occur in rep
	@ between the eval and the write (eg. it could temporarily modify the current-input-port)
	@ a way to obviate this would be to save env and cnt then set new cnt to restore these vars
	@ (not done here)
	nullp	sv2
	it	ne
	setne	env, sv2
	b	eval

.balign	4
	
sscenv:	SYMSIZE	25
	.ascii	"scheme-report-environment"
	.balign 4

screnv:	@ (scheme-report-environment version)
	@ on entry:	sv1 <- version
	@ on exit:	sv1 <- environment
	PFUNC	1			@ primitive function, one input arg
	ldr	sv4, =sscenv		@ sv4 <- address of this function
srnenv:	@ [internal entry]	
.ifdef r3rs
	eq	sv1, #0x0d		@ is version = 3 (scheme int)?
.else
	eq	sv1, #0x15		@ is version = 5 (scheme int)?
.endif
	itT	eq
	seteq	sv1, #null		@	if so,  sv1 <- empty user enviroment
	seteq	pc,  cnt		@	if so,  return
	b	error4

.balign	4
	
snlenv:	SYMSIZE	16
	.ascii	"null-environment"
	.balign 4

nulenv:	@ (null-environment version)
	@ on entry:	sv1 <- version
	@ on exit:	sv1 <- environment
	PFUNC	1			@ primitive function, one input arg
	ldr	sv4, =snlenv		@ sv4 <- address of this function
	b	srnenv

.balign	4
	
sinenv:	SYMSIZE	23
	.ascii	"interaction-environment"
	.balign 4

pinenv:	@ (interaction-environment)
	@ on entry:	sv1 <- '()
	@ on exit:	sv1 <- environment
	PFUNC	0			@ primitive function, no input arg
	vcrfi	sv1, glv, 7		@ sv1 <- env -- global (*** NEW ***)
	set	pc,  cnt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	6.	Standard Procedures
@	6.6.	Input and Output
@	6.6.1.	ports:			input-port?, output-port?,
@					current-input-port, current-output-port,
@					open-input-file, open-output-file,
@					close-input-port, close-output-port
@	6.6.2.	input:			read-char, peek-char,
@					eof-object?, char-ready?
@	6.6.3.	output:			write-char
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			eval, apply, save, cons, list, error4, boolxt, npofxt
@						subcpy, memcpy	
@						eof_char,dbl_quote_char,pound_char,backslash_char,
@						open_par_char,dot_char,space_char,backspace_char,
@						close_par_char
@		4.2.	Derived expressions:	qsqt	
@		4.3.	Macros:			mxpnd
@		6.2.	Numbers			strnum	
@		6.3.3.	Symbols:		strsym
@		6.3.6.	Vectors:		makvec
@	
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=

.balign	4
	
sinpor:	SYMSIZE	11
	.ascii	"input-port?"
	.balign 4

inport:	@ (input-port? obj ...)
	@ on entry:	sv1 <- (obj ...)
	@ defined only for file ports
	EPFUNC	0x80|f, oioprfn, 0	@ primitive, init-sv4 = #f, fentry = ioprfn, narg = listed
					@ sv1 <- input port
	set	sv2, #5
	b	ioprtp

.balign	4
	
soutpr:	SYMSIZE	12
	.ascii	"output-port?"
	.balign 4

outprt:	@ (output-port? obj ...)
	@ on entry:	sv1 <- (obj ...)
	PFUNC	0			@ primitive function, listed input args
	set	sv2, sv1		@ sv2 <- (obj ...)
	set	sv4, #f
	bl	ioprfe			@ sv2 <- possible output port	
	set	sv1, sv2
	set	sv2, #9
ioprtp:	@ [internal entry]
	pntrp	sv1			@ was a port obtained?
	itTT	eq
	cdreq	sv1, sv1
	vcrfieq	sv1, sv1, 0
	eqeq	sv1, sv2
	b	boolxt			@ return with #t/#f based on whether port was obtained
	
.balign	4
	
scripr:	SYMSIZE	18
	.ascii	"current-input-port"
	.balign 4

criprt:	@ (current-input-port)
	@ returns the top-level current-input port (the default input port)
	@ user redefines (current-input-port) in user environment to modify 'default' input port
	@ but this works at top level only since local env is not carried into read/write
	@ (they're proc, not syntax)
	@ on entry:	sv1 <- ()
	PFUNC	0			@ primitive function, listed input args
	vcrfi	sv2, glv, 4
	vcrfi	sv3, sv2, 1
	b	cr_prt

.balign	4
	
scropr:	SYMSIZE	19
	.ascii	"current-output-port"
	.balign 4

croprt:	@ (current-output-port)
	@ on entry:	sv1 <- ()
	PFUNC	0			@ primitive function, listed input args
	vcrfi	sv2, glv, 4
	vcrfi	sv3, sv2, 2
cr_prt:	@ [internal entry]
	vcrfi	sv1, sv2, 0
	list	sv1, sv1
	cons	sv1, sv1, sv3
	set	pc,  cnt
	
.balign	4
	
sopnif:	SYMSIZE	15
	.ascii	"open-input-file"
	.balign 4

opnifl:	@ (open-input-file filename <port-model>)
	@ on entry:	sv1 <- filename
	@ on entry:	sv2 <- <port-model> or null
	PFUNC	2			@ primitive function, two input args
opnife:	@ [internal entry]
	set	sv3, sv1		@ sv3 <- file name
	nullp	sv2			@ was a port model given?
	it	eq
	ldreq	sv2, =vfile		@	if not, sv3 <- FILE port model (default)
	vcrfi	sv2, sv2, 1		@ sv2 <- input  port vector
	set	sv1, #null		@ sv1 <- null
	cons	sv1, sv1, sv2		@ sv1 <- (null . input-port-vector) for prtifi
	set	sv2, sv1		@ sv2 <- (null . input-port-vector) for prtifi (symmetry)
	bl	flok			@ acquire file system lock
	@ get file info, assess if it exists (if not, exit with error)
	set	rvc, #0x2a		@ rvc <- offset to file info function in port-vector
	bl	prtfun			@ sv2 <- file info vectr, sv1 <- (null . input-or-output-port-vect)
	vcrfi	rva, sv2, 1		@ rva <- file not found indicator
	eq	rva, #f0		@ file not found?
	it	eq
	seteq	sv1, sv3
	beq	opnfer
	cdr	sv1, sv1		@ sv1 <- input-port-vector
opnfcm:	@ [internal entry] open-input/output-file common exit
	@ on entry:	sv1 <- input/output-port-vector
	@ on entry:	sv2 <- #(fname page offset <buffer>)
	@ find a new handle for the file
	set	sv5, sv2		@ sv5 <- file info vector (saved against ffhofl)
	set	sv2, #5
opnif8:	bl	ffhofl			@ sv4 <- descr, sv2 <- file pre-sublist, sv3 <- post-sublist
	nullp	sv4
	it	ne
	addne	sv2, rvb, #4
	bne	opnif8
	set	sv4, rvb		@ sv4 <- file handle (scheme int)
	@ add file port in open file list
	set	rvb, #32
	bl	zmaloc
	add	rvc, rva, #8
	stmia	rva!, {rvc}
	vcrfi	rvc, glv, 6		@ rvc <- open-file-list
	stmia	rva!, {rvc}
	add	rvc, rva, #8
	stmia	rva!, {rvc}
	add	rvc, rva, #12
	stmia	rva!, {sv1, sv4, rvc}
	set	rvc, #null
	stmia	rva!, {sv5, rvc}
	sub	sv2, rva, rvb		@ sv2 <- updated open file list [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	vcsti	glv, 6, sv2		@ update open-file-list on glv
	set	sv1, sv4		@ sv1 <- file handle (scheme int), for exit
	set	lnk, cnt
	b	funlok
opnfer:	@ open file error
	bl	funlok			@ release file system lock
	ldr	sv4, =sfile
	b	error4
		
.balign	4
	
sopnof:	SYMSIZE	16
	.ascii	"open-output-file"
	.balign 4

opnofl:	@ (open-output-file filename <port-model>)
	@ on entry:	sv1 <- filename
	@ on entry:	sv2 <- <port-model> or null
	PFUNC	2			@ primitive function, two input args
opnofe:	@ [internal entry]
	nullp	sv2			@ was a port model given?
	itE	eq
	ldreq	sv5, =vfile		@	if not, sv5 <- FILE port model (default)
	setne	sv5, sv2		@	if so,  sv5 <- port-model
	bl	flok			@ acquire file system lock
	@ see if file is already open (error out if it is)
	vcrfi	sv4, glv, 6		@ sv4 <- list of open files
opnof0:	nullp	sv4			@ done scanning open file list?
	beq	opnof1			@	if so,  jump to continue
	snoc	sv3, sv4, sv4		@ sv3 <- file port, sv4 <- rest of open-file list
	cadar	sv3, sv3		@ sv3 <- #(fname page offset <buffer>)
	vcrfi	sv2, sv3, 0		@ sv2 <- fname
	bl	stsyeq			@ are file names the same?
	beq	opnfer
	b	opnof0			@ jump to keep scanning open file list
opnof1:	@ get file info and add buffer to it
	set	sv3, sv1		@ sv3 <- file name
	vcrfi	sv5, sv5, 2		@ sv5 <- output  port vector
	set	sv1, #null		@ sv1 <- null
	cons	sv1, sv1, sv5		@ sv1 <- (null . output-port-vector) for prtofi
	set	sv2, sv1		@ sv2 <- (null . output-port-vector) for prtofi (symmetry)
	set	rvc, #0x06		@ rvc <- 6 = offset to file-info function in port-vector
	bl	prtfun			@ sv2 <- file info vector, sv1 <- (null . input-or-output-port-vec)
	cdr	sv3, sv1		@ sv3 <- output-port-vector
	vcrfi	rvb, sv3, 7		@ rvb <- buffer size (scheme int)
	lsr	rvb, rvb, #2
	add	rvb, rvb, #8
	bl	zmaloc			@ rva <- start address of allocated area
	set	rvc, #i0		@ rvc <- 0 (scheme int)
	str	rvc, [rva, #4]		@ set 0 as file ID, for potential file erasure in prtfun
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv3, rva, rvb		@ sv3 <- buffer (bytevector)	[*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer,	[*restart critical instruction*]
	vcsti	sv2, 3, sv3		@ store buffer in file info vector
	@ port-specific file-info initialization, including erasing an existing file
	set	sv4, sv2		@ sv4 <- #(fname page offset buffer), for filers (via prtfun)
	set	sv2, sv1		@ sv2 <- (null . output-port-vector), for prtfun
	set	rvc, #0x07		@ rvc <- 7 = offset to filers function in port-vector
	bl	prtfun			@ file pseudo-erased, file-info initialized
	set	sv2, sv4		@ sv2 <- #(fname page offset buffer)
	cdr	sv1, sv1		@ output-port-vector
	b	opnfcm
	
.balign	4

sclsip:	SYMSIZE	16
	.ascii	"close-input-port"
	.balign 4

clsipr:	@ (close-input-port port ...)
	@ on entry:	sv1 <- (port ...)
	EPFUNC	0x80|t, oioprfn, 0	@ primitive, init-sv4 = #t, fentry = ioprfn, narg = listed
					@ sv1 <-  ((handle ...) . port-vector) = full input port
clsipe:	@ [internal entry] for load, call-with-input-file
	car	sv2, sv1		@ sv2 <- file info list (handle ...) (for oflfrm)
	bl	oflfrm			@ remove file from open-file-list
	set	rvc, #0x22		@ rvb <- offset to file-close function in port-vector
	b	prtfun			@ close the input port, return via npofxt and cnt

.balign	4
	
sclsop:	SYMSIZE	17
	.ascii	"close-output-port"
	.balign 4

clsopr:	@ (close-output-port port <mode>)
	@ on entry:	sv1 <- port
	@ on entry:	sv2 <- (<mode>), if non-null close as input port (i.e. forget write-on-close)
	PFUNC	0			@ primitive function, listed input args
clsopt:	@ [internal entry]
	swap	sv1, sv2, sv4		@ sv1 <- (<mode>),	sv2 <- port
	list	sv2, sv2		@ sv2 <- (port)
	set	sv4, #t
	bl	ioprfe			@ sv2 <- full output-port (could be the null output-port)
	set	sv5, sv2
	car	sv2, sv2		@ sv2 <- file info list (handle ...) (for oflfrm)
	bl	oflfrm			@ remove file from open-file-list
	set	sv2, sv5
	set	rvc, #0x02		@ rvc <- 2, offset to file-close function in port-vector
	b	prtfun			@ close the output port, return via npofxt and cnt

oflfrm:	@ remove a file from the open file list
	@ preserves:	sv1, sv5
	bic	rvc, lnk, #lnkbit0	@ rvc <- lnk, saved against flok, fclose (and even if Thumb2)
	car	sv2, sv2		@ sv2 <- file handle (for ffhofl)
	bl	flok			@ acquire file system lock
	bl	ffhofl			@ sv4 <- descr, sv2 <- file prior-sublist, sv3 <- post-sublist
	nullp	sv2			@ is prior open-file-list = '()?
	itE	eq
	vcstieq glv, 6, sv3		@	if so,  store remaining open file list in global vector
	setcdrne  sv2, sv3		@	if not, store rmnng open file list as cdr of prior open lst
	orr	lnk, rvc, #lnkbit0	@ lnk <- lnk, restored
	b	funlok			@ release file system lock and return to caller via lnk
	
@---------------------------------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input:			CORE:		read-char, peek-char,
@							eof-object?, char-ready?
@---------------------------------------------------------------------------------------------------------

.balign	4
	
sredch:	SYMSIZE	9
	.ascii	"read-char"
	.balign 4

redchr: @ (read-char <port> <reg> <n>)
	@ on entry:	sv1 <- (<port> <reg> <n>)
	@ on entry:	sv3 <- (), => setipr will error out on port problem
	@ on exit:	sv1 <- char read
	EPFUNC	(0x23<<2)|i0, oioprfn, 0 @ primitive, init-sv4 = 3, fentry = ioprfn, narg = listed

.balign	4
	
spekch:	SYMSIZE	9
	.ascii	"peek-char"
	.balign 4

pekchr: @ (peek-char <port> <reg> <n>)
	@ on entry:	sv1 <- (<port> <reg> <n>)
	@ on entry:	sv3 <- (), => setipr will error out on port problem
	@ on exit:	sv1 <- char read
	EPFUNC	(0x23<<2)|f0, oioprfn, 0 @ primitive, init-sv4 = 3 (~flt), fentry = ioprfn, narg = listed

.balign	4
	
seofob:	SYMSIZE	11
	.ascii	"eof-object?"
	.balign 4
	
eofobj:	@ (eof-object? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	PFUNC	1			@ primitive function, one input arg
	ldr	sv2, =eof_char
	eq	sv1, sv2
	b	boolxt

.balign	4
	
schrdy:	SYMSIZE	11
	.ascii	"char-ready?"
	.balign 4

chrrdy: @ (char-ready? <port> <reg> <n>)
	@ on entry:	sv1 <- (<port> <reg> <n>)
	@ on entry:	sv3 <- (), => setipr will error out on port problem
	@ on exit:	sv1 <- #t/#f
	EPFUNC	(0x24<<2)|i0, oioprfn, 0 @ primitive, init-sv4 = 4, fentry = ioprfn, narg = listed

@---------------------------------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.3. output:			CORE:		write-char
@---------------------------------------------------------------------------------------------------------

.balign	4
	
swrtch:	SYMSIZE	10
	.ascii	"write-char"
	.balign 4

wrtchr: @ (write-char char <port> <reg> <n> ...)
	@ on entry:	sv1 <- char
	@ on entry:	sv2 <- (<port> <reg> <n> ...) or (((port <reg> <n> ...) . port-vector))
	@ on exit:	sv1 <- npo
	EPFUNC	(0x03<<2)|i0, oioprfn, 1 @ primitive, init-sv4 = 3, fentry = ioprfn, narg = 1


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg


@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	7.	Addendum:					erase, files
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			lcons, lambda_synt, sav__c, save, eval, npofxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

	
@---------------------------------------------------------------------------------------------------------
@  II.D.3. file system:				erase, files
@---------------------------------------------------------------------------------------------------------


.balign	4

.ifndef	live_SD
	
serase:	SYMSIZE	5
	.ascii	"erase"
	.balign 4
	
erase:	@------	function --------------
	@ scheme call:	(erase <sector>|<-1>)
	@ erase all file flash or just one sector within file flash or all lib flash
	@ on entry:	sv1 <- (<sector>|<-1>)
	@------	type ------------------
	PFUNC	0			@ primitive function, listed input args
	@------	code ------------------
erasee:	@ [internal entry]
	bl	flok			@ acquire file system lock
	nullp	sv1			@ erasing all file flash?
	bne	erase4
	@ erase all file flash
	ldr	sv2, =F_START_PAGE	@ sv2 <- start address of flash used for files
	vcrfi	sv3, glv, 11		@ sv3 <- address of end of file flash (crunch space)
	bic	sv3, sv3, #i0
	bl	pgsctr			@ rva <- sector number (raw int), of flash page in sv2
	raw2int	sv5, rva
	ldr	sv4, =flashsectors	@ sv4 <- address of flash sector table
eraslp:	bic	rva, sv5, #i0
	ldr	sv2, [sv4, rva]		@ sv2 <- start address of flash sector
	add	sv5, sv5, #4		@ sv5 <- next flash sector number	
	bl	ersfla			@ erase flash sector that starts at sv2
	eq	sv2, sv3
	bne	eraslp
	b	unlokx
erase4:	@ erase all lib flash or a single sector of file or lib flash
	adr	lnk, unlokx		@ lnk <- return address when erasing a single file/lib sector
	car	sv1, sv1		@ sv1 <- sector or -1 (to erase all lib flash)
  .ifndef LIB_TOP_PAGE
	tst	sv1, #0x80000000	@ erasing lib flash?
	bne	unlokx			@	if so,  jump to exit
	int2raw	rva, sv1		@ rva <- sector (raw int)
	lsl	sv2, rva, #4		@ sv2 <- flash page to erase (full address)
	b	ersfla			@ jump to erase single file flash sector
  .else
	tst	sv1, #0x80000000	@ erasing lib flash?
	bne	erslib			@	if so,  jump to that case
	int2raw	rva, sv1		@ rva <- sector (raw int)
	lsl	sv2, rva, #4		@ sv2 <- flash page to erase (full address)
    .ifndef SHARED_LIB_FILE
	ldr	sv3, =LIB_TOP_PAGE	@ sv3 <- end address of lib flash (eg. boot sector)
	lsr	rvb, sv3, #4		@ rvb <- end address of lib flash (shifted for unsigned comparison)
	cmp	rva, rvb		@ erasing lib flash?
	bmi	libers			@	if so,  jump to erase single lib flash sector
    .endif
	b	ersfla			@ jump to erase single file flash sector
erslib:	@ erase lib flash
	ldr	rvb, =scmenv
	vcsti	glv, 13, rvb		@ set scmenv (no libs) as built-in environment
	vcrfi	sv2, glv, 12		@ sv2 <- address of 1st lib page in flash
	nullp	sv2			@ nothing to erase?
	beq	unlokx			@	if so,  jump to exit
	bl	lbsctr			@ rva <- start sector number for erasure (raw int)
	raw2int	sv5, rva		@ sv5 <- start sector number for erasure (scheme int)
	@ all lib flash is erased if sv1 = -536870912 (a kind of safety if libs are not a valid list)
	lsr	rva, sv1, #2
	eq	rva, #0x20000000
	it	eq
	seteq	sv2, #null
	beq	erslb1
erslb0:	@ find end page for erasure
	cdr	sv2, sv2
	nullp	sv2
	beq	erslb1
	add	sv1, sv1, #4
	eq	sv1, #i0
	bne	erslb0
erslb1:	@ update glv and account for case where all lib flash will be erased
	vcsti	glv, 12, sv2
	nullp	sv2
	it	eq
	ldreq	sv2, =LIB_TOP_PAGE
	bl	lbsctr			@ rva <- sector number of page with lib to keep (raw int)
	raw2int	sv4, rva		@ sv4 <- sector number of page with lib to keep (scheme int)
	set	sv3, sv2		@ sv3 <- start page of lib to keep, or LIB_TOP_PAGE (saved)
	@ erase lib sectors with no lib to keep
	adr	lnk, erslb3
_func_
erslb3:	eq	sv5, sv4
	beq	erslb5
	bic	rva, sv5, #i0		@ rva <- flash sector number * 4
	ldr	rvc, =lib_sectors	@ rvc <- address of lib flash sector table
	ldr	sv2, [rvc, rva]		@ sv2 <- start address of flash sector
	add	sv5, sv5, #4		@ sv5 <- next flash sector number
	b	libers			@ erase lib flash sector that starts at sv2
erslb5:	@ deal with sector with libs to keep, if any
	ldr	rvc, =LIB_TOP_PAGE
	eq	sv3, rvc		@ all lib flash was erased?
	beq	unlokx			@	if so,  jump to exit
	bic	rva, sv4, #i0		@ rva <- flash sector number * 4 of sector with lib to keep
	ldr	rvc, =lib_sectors	@ rvc <- address of lib flash sector table
	ldr	sv2, [rvc, rva]		@ sv2 <- start address of flash sector with lib to keep
	eq	sv2, sv3		@ do libs to keep start at start of sector?
	beq	unlokx			@	if so,  jump to exit
	@ prepare for partial sector erasure (build pseudo file descriptor, find pages to copy)
	set	sv5, sv4		@ sv5 <- sector number of page with lib to keep (scheme int)
	set	sv2, sv3		@ sv2 <- start page of lib to keep
	bl	mkfdsc			@ sv4 <- blank output file descriptor (with buffer)
	int2raw	rvc, sv5		@ rvc <- sector number of page with lib to keep (raw int)
erslb6:	add	sv2, sv2, #F_PAGE_SIZE
	bl	lbsctr			@ rva <- sector number of page in sv2 (raw int)
	eq	rva, rvc		@ is this page in same sector as lib to keep?
	beq	erslb6
	@ copy from lib to crunch space
	set	rva, sv3
	set	rvb, sv2
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (destination, pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space (destination)
	vcsti	sv4, 2, rva		@ store src start page address in caller-tmp-storage of sv4
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ erase lib sector
	set	sv2, sv3		@ sv2 <- start page of lib to keep
	bl	libers
	@ copy from crunch space back to lib
	vcrfi	rva, glv, 11		@ rva <- address of crunch space (source start, pseudo scheme int)
	bic	rva, rva, #i0		@ rva <- address of crunch space (source start)
	vcrfi	rvb, sv4, 1		@ rvb <- address of end of extra FLASH target (source end)
	vcrfi	rvc, sv4, 2		@ rvc <- start address of former source = destination for copy
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ erase crunch sector
	bl	fcsdel
	@ return
	b	unlokx

  .endif  @ LIB_TOP_PAGE

.endif	@ live_SD
	
.balign	4

.ifndef	live_SD
	
sfpgwr:	SYMSIZE	4
	.ascii	"fpgw"
	.balign 4

fpgwrt:	@------	function --------------
	@ (fpgw pseudo-file-descriptor file-flash-page)
	@ write the contents of bytevector to specified file flash page
	@ on entry:	sv1 <- pseudo-file-descriptor with data to write at index 3
	@ on entry:	sv2 <- destination file-flash-page (scheme int)
	@------	type ------------------
	PFUNC	2			@ primitive function, two input args
	@------	code ------------------
	set	sv4, sv1		@ sv4 <- pseudo-file-descriptor
	bl	flok			@ acquire file system lock
	int2raw	rva, sv2		@
	lsl	sv2, rva, #4
	adr	lnk, unlokx		@ lnk <- return address for file/lib flash page write function
  .ifdef LIB_TOP_PAGE
    .ifndef SHARED_LIB_FILE
	ldr	sv3, =LIB_TOP_PAGE	@ sv3 <- end address of lib flash (eg. boot sector)
	lsr	rvb, sv3, #4		@ rvb <- end address of lib flash (shifted for unsigned comparison)
	cmp	rva, rvb		@ writing to lib flash?
	it	mi
	bmi	libwrt			@	if so,  jump to commit lib-flash write (returns to unlokx)
    .endif
  .endif	
	b	wrtfla			@ commit write to file flash (returns to unlokx)

.endif	@ live_SD
	
.balign	4
	
sunloc:	SYMSIZE	6
	.ascii	"unlock"
	.balign 4
	
unlock:	@------	function --------------
	@ scheme call:	(unlock)
	@ release the file flash lock -- used to recover from file read/write crash
	@------	type ------------------
	PFUNC	0			@ primitive function, listed input args
	@------	code ------------------
_func_
unlokx:	@ [internal entry]
	bl	funlok			@ release file lock
	b	trufxt
	
.balign	4
	
sfiles:	SYMSIZE	5
	.ascii	"files"
	.balign 4
	
files:	@------	function --------------
	@ scheme call:	(files <port-model>)
	@ list names of all files in flash or specified port
	@ note:	 if interrupt occurs and file gets deleted just before its name is consed,
	@	 it may still be listed
	@ on entry:	sv1 <- port-model or ()
	@------	type ------------------
	PFUNC	1			@ primitive function, one input arg
	@------	code ------------------
	nullp	sv1			@ was a port model given?
	itE	eq
	ldreq	sv2, =vfile		@	if not, sv2 <- FILE port model (default)
	setne	sv2, sv1		@	if so,  sv2 <- port model
	vcrfi	sv2, sv2, 1		@ sv2 <- input  port vector
	set	sv1, #null		@ sv1 <- null
	cons	sv1, sv1, sv2		@ sv1 <- (null . input-port-vector) for prtifi
	set	rvc, #0x2b		@ rvc <- offset to file-list function in port-vector
	b	prtfun			@ sv1 <- list of file names, return via cnt


.ifdef	onboard_SDFT

.balign	4
	
ssdini:	SYMSIZE	7
	.ascii	"sd-init"
	.balign 4
	
psdini:	@------	function --------------
	PFUNC	0			@ primitive function, no input args
	b	sd_ini
	
.endif


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

