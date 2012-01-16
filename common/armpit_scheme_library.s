@---------------------------------------------------------------------------------------------------
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
@---------------------------------------------------------------------------------------------------

	
.balign	4

	@-------.-------.-------.-------.-------+
libenv:	@	4.2. sub-environment		|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_libenv - libenv - 4) >> 2

cond_env:	.word	cond,	pcond		@ cond		4.2.1 conditionals
case_env:	.word	case,	pcase		@ case
and_env:	.word	and,	pand		@ and
or_env:		.word	or,	por		@ or
let_env:	.word	let,	plet		@ let		4.2.2 binding constructs
lets_env:	.word	lets,	plets		@ let*
letrec_env:	.word	letr,	pletr		@ letrec
do_env:		.word	do,	pdo		@ do		4.2.4 iteration
		.word	delay,	pdelay		@ delay		4.2.5 delayed evaluation
mkprms_env:	.word	mkp,	mkprms		@ _mkp			(make-promise)
else_env:	.word	else,	scheme_true	@ else		4.2.c constants
implies_env:	.word	implies, scheme_true	@ =>
var0_env:	.word	var0,	scheme_true	@ var0
		.word	var1,	scheme_true	@ var1
		.word	var2,	scheme_true	@ var2
		.word	var3,	scheme_true	@ var3
		.word	var4,	scheme_true	@ var4
		.word	var5,	scheme_true	@ var5
		.word	var6,	scheme_true	@ var6

	@-------.-------.-------.-------.-------+
	@	6.3.1. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	not,	pnot		@ not
		.word	booln,	pbooln		@ boolean?

	@-------.-------.-------.-------.-------+
	@	6.2. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	szero,	zero		@ zero?
		.word	sposit,	positi		@ positive?
		.word	snegat,	negati		@ negative?
		.word	sodd,	odd		@ odd?
		.word	seven,	even		@ even?
		.word	smax,	max		@ max
		.word	smin,	min		@ min
		.word	sabs,	abs		@ abs
		.word	sgcd,	gcd		@ gcd
		.word	slcm,	lcm		@ lcm

.ifndef	integer_only
	
		.word	srtnlz,	rtnlz		@ rationalize

.endif
	
	@-------.-------.-------.-------.-------+
	@	6.3.2. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	caar,	pcaar		@ caar
		.word	cadr,	pcadr		@ cadr
		.word	cdar,	pcdar		@ cdar
		.word	cddr,	pcddr		@ cddr
		.word	caaar,	pcaaar		@ caaar
		.word	caadr,	pcaadr		@ caadr
		.word	cadar,	pcadar		@ cadar
		.word	caddr,	pcaddr		@ caddr
		.word	cdaar,	pcdaar		@ cdaar
		.word	cdadr,	pcdadr		@ cdadr
		.word	cddar,	pcddar		@ cddar
		.word	cdddr,	pcdddr		@ cdddr
		.word	caaaar, paaaar		@ caaaar
		.word	caaadr, paaadr		@ caaadr
		.word	caadar, paadar		@ caadar
		.word	caaddr, paaddr		@ caaddr
		.word	cadaar, padaar		@ cadaar
		.word	cadadr, padadr		@ cadadr
		.word	caddar, paddar		@ caddar
		.word	cadddr, padddr		@ cadddr
		.word	cdaaar, pdaaar		@ cdaaar
		.word	cdaadr, pdaadr		@ cdaadr
		.word	cdadar, pdadar		@ cdadar
		.word	cdaddr, pdaddr		@ cdaddr
		.word	cddaar, pddaar		@ cddaar
		.word	cddadr, pddadr		@ cddadr
		.word	cdddar, pdddar		@ cdddar
		.word	cddddr, pddddr		@ cddddr
		.word	snull,	pnull		@ null?
		.word	listp,	plistp		@ list?
		.word	list,	plist		@ list
		.word	slngth,	plngth		@ length
		.word	append,	pappnd		@ append
		.word	srevrs,	prevrs		@ reverse
		.word	lstail,	plstal		@ list-tail
		.word	lstref,	plstrf		@ list-ref
memv_env:	.word	smemv,	pmemv		@ memv
		.word	memq,	pmemv		@ memq
		.word	member,	pmembr		@ member
		.word	sassq,	passq		@ assq
		.word	sassv,	passq		@ assv
		.word	assoc,	passoc		@ assoc

	@-------.-------.-------.-------.-------+
	@	6.3.4. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	chrceq,	pchceq		@ char-ci=?
		.word	chrclt,	pchclt		@ char-ci<?
		.word	chrcgt,	pchcgt		@ char-ci>?
		.word	chrcle,	pchcle		@ char-ci<=?
		.word	chrcge,	pchcge		@ char-ci>=?
		.word	chralp,	pchalp		@ char-alphabetic?
		.word	chrnum,	pchnum		@ char-numeric?
		.word	chrspa,	pchspa		@ char-whitespace?
		.word	chrupq,	pchupq		@ char-upper-case?
		.word	chrloq,	pchloq		@ char-lower-case?
		.word	chrupc,	pchupc		@ char-upcase
		.word	chrdnc,	pchdnc		@ char-downcase

	@-------.-------.-------.-------.-------+
	@	6.3.5. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	spstng,	pstrng		@ string
		.word	sstequ,	strequ		@ string=?
		.word	sstceq,	strceq		@ string-ci=?
		.word	sstlt,	strlt		@ string<?
		.word	sstgt,	strgt		@ string>?
		.word	sstle,	strle		@ string<=?
		.word	sstge,	strge		@ string>=?
		.word	sstclt,	strclt		@ string-ci<?
		.word	sstcgt,	strcgt		@ string-ci>?
		.word	sstcle,	strcle		@ string-ci<=?
		.word	sstcge,	strcge		@ string-ci>=?
		.word	ssubst,	substr		@ substring
		.word	sstapp,	strapp		@ string-append
		.word	sstlst,	strlst		@ string->list
		.word	slstst,	lststr		@ list->string
		.word	sstcpy,	strcpy		@ string-copy
		.word	sstfil,	strfil		@ string-fill

	@-------.-------.-------.-------.-------+
	@	6.3.6. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	svctor,	vector		@ vector
		.word	svclst,	pvclst		@ vector->list
		.word	slsvec,	plsvec		@ list->vector
		.word	svcfll,	pvcfll		@ vector-fill

	@-------.-------.-------.-------.-------+
	@	6.4. sub-environment		|
	@-------.-------.-------.-------.-------+

		.word	smap,	map		@ map
		.word	sfreac,	foreac		@ for-each
		.word	sforce,	force		@ force
	
	@-------.-------.-------.-------.-------+
	@	6.6. sub-environment		|
	@-------.-------.-------.-------.-------+
	
		.word	scwipf,	cwinpf		@ call-with-input-file	6.6.1 ports
		.word	scwopf,	cwoutf		@ call-with-output-file
		.word	snewln,	pnewln		@ newline

	@-------.-------.-------.-------.-------+
	@	system utility  sub-environment	|
	@-------.-------.-------.-------.-------+

		.word	s_dfnd, p_dfnd		@ defined?
		.word	s_link, p_link		@ link
		.word	s_upah, p_upah		@ unpack-above-heap
		.word	s_libs, p_libs		@ libs
	
.ifdef	LIB_TOP_PAGE
	
		.word	s_erlb, p_erlb		@ erase-libs
		.word	s_uplb, p_uplb		@ unpack-to-lib
.else

		.word	s_uplb, p_upah		@ unpack-to-lib

.endif

end_of_libenv:	@ end of libenv

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======@
@												@
@		CONSTANTS									@
@												@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======@
	
.macro	make_var_from_libenv var_name, var_env
	\var_name = ((\var_env - libenv + 4) << 13) | ((lib_env - scmenv) << 6) | variable_tag
.endm
	
	@-------.-------.-------.-------.-------+
@-------@	4.2. Constants			|
	@-------.-------.-------.-------.-------+

	make_var_from_libenv	else_var,	else_env
	make_var_from_libenv	implies_var,	implies_env
	make_var_from_libenv	cond_var,	cond_env
	make_var_from_libenv	case_var,	case_env
	make_var_from_libenv	and_var,	and_env
	make_var_from_libenv	or_var,		or_env
	make_var_from_libenv	let_var,	let_env
	make_var_from_libenv	lets_var,	lets_env
	make_var_from_libenv	letrec_var,	letrec_env
	make_var_from_libenv	do_var,		do_env
	make_var_from_libenv	mkpromise_var,	mkprms_env
	make_var_from_libenv	var0_var,	var0_env
	make_var_from_libenv	var1_var,	(var0_env +  8)
	make_var_from_libenv	var2_var,	(var0_env + 16)
	make_var_from_libenv	var3_var,	(var0_env + 24)
	make_var_from_libenv	var4_var,	(var0_env + 32)
	make_var_from_libenv	var5_var,	(var0_env + 40)
	make_var_from_libenv	var6_var,	(var0_env + 48)
	
	@-------.-------.-------.-------.-------+
@-------@	6.3.2. Constants		|
	@-------.-------.-------.-------.-------+

	make_var_from_libenv	memv_var,	memv_env


@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				R5RS (see R3RS below)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.1.	conditionals:					cond, case, and, or
@	4.2.2.	binding constructs:				let, let*, letrec
@	4.2.3.	sequencing:					begin
@	4.2.4.	iteration:					do
@	4.2.5.	delayed evaluation:				delay
@	4.2.c.	constants:					else, =>, var0, var1, var2, var3, var4,
@								var5, var6
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.ifndef r3rs

.balign	4

cond:	SYMSIZE	4
	.ascii	"cond"
	.balign 4

pcond:	@ (cond clause1 clause2 ...)
	@(define-syntax cond
	@  (syntax-rules (else =>)
	@    ((_ (else result1 ...))
	@     (begin result1 ...))
	@    ((_ (test => result))
	@     (let ((temp test)) (if temp (result temp))))
	@    ((_ (test => result) clause1 ...)
	@     (let ((temp test)) (if temp (result temp) (cond clause1 ...))))
	@    ((_ (test)) test)
	@    ((_ (test) clause1 ...)
	@     (let ((temp test)) (if temp temp (cond clause1 ...))))
	@    ((_ (test result1 ...))
	@     (if test (begin result1 ...)))
	@    ((_ (test result1 ...) clause1 ...)
	@     (if test (begin result1 ...) (cond clause1 ...)))))
	.word	macro,		. + 4
	.word	. + 8,		. + 12
	.word	implies_var,	else_null
	.word	cond_a,	. + 4,	cond_b,	. + 4,	cond_c,	. + 4,	cond_d,	. + 4
	.word	cond_e,	. + 4,	cond_f,	. + 4,	cond_g,	scheme_null
cond_a:	@ pattern:	(_ (else result1 ...))
	@ template:	(begin result1 ...)
	@		(_ (else var2 ...))
	@		(begin var2 ...)
	@ var2 <-> result1
	.word	condaP,	. + 4, condaT, scheme_null
condaP:	@ (_ (else result1 ...))
	.word	underscore_var,	. + 4
	.word	. + 8,		scheme_null
	.word	else_var,	var2_ellipse
condaT:	@ (begin result1 ...)
	.word	begin_var,	var2_ellipse
cond_b:	@ pattern:	(_ (test => result))
	@ template:	(let ((temp test)) (if temp (result temp)))
	@		(_ (var1 => var2))
	@		(let ((var0 var1)) (if var0 (var2 var0)))
	@ var0 <-> temp
	@ var1 <-> test
	@ var2 <-> result
	.word	condbP,	. + 4, condbT, scheme_null
condbP:	@ (_ (test => result))
	.word	underscore_var,	. + 4
	.word	condcP + 16,	scheme_null
condbT:	@ (let ((temp test)) (if temp (result temp)))
	.word	let_var,	. + 4
	.word	condcT + 16,	. + 4
	.word	. + 8,		scheme_null
	.word	if_var,	. + 4
	.word	var0_var,	. + 4
	.word	condcT + 56,	scheme_null
cond_c:	@ pattern:	(_ (test => result) clause1 ...)
	@ template:	(let ((temp test)) (if temp (result temp) (cond clause1 ...)))
	@		(_ (var1 => var2) var3 ...)
	@		(let ((var0 var1)) (if var0 (var2 var0) (cond var3 ...)))
	@ var0 <-> temp
	@ var1 <-> test
	@ var2 <-> result
	@ var3 <-> clause1
	.word	condcP,	. + 4, condcT, scheme_null
condcP:	@ (_ (test => result) clause1 ...)
	.word	underscore_var,	. + 4
	.word	. + 8,		var3_ellipse
	.word	var1_var,	. + 4
	.word	implies_var,	. + 4
	.word	var2_var,	scheme_null
condcT:	@ (let ((temp test)) (if temp (result temp) (cond clause1 ...)))
	.word	let_var,	. + 4
	.word	. + 8,		. + 12
	.word	var0_1_null,	scheme_null
	.word	. + 8,		scheme_null
	.word	if_var,	. + 4
	.word	var0_var,	. + 4
	.word	. + 8,		. + 20
	.word	var2_var,	. + 4
	.word	var0_var,	scheme_null
	.word	. + 8,		scheme_null
	.word	cond_var,	var3_ellipse
cond_d:	@ pattern:	(_ (test))
	@ template:	test
	@		(_ (var1))
	@		var1
	@ var1 <-> test
	.word	conddP,	. + 4, var1_var, scheme_null
conddP:	@ (_ (test))
	.word	underscore_var,	. + 4
	.word	var1_null,	scheme_null
cond_e:	@ pattern:	(_ (test) clause1 ...)
	@ template:	(let ((temp test)) (if temp temp (cond clause1 ...)))
	@		(_ (var1) var3 ...)
	@		(let ((var0 var1)) (if var0 var0 (cond var3 ...)))
	@ var0 <-> temp
	@ var1 <-> test
	@ var3 <-> clause1
	.word	condeP,	. + 4, condeT, scheme_null
condeP:	@ (_ (test) clause1 ...)
	.word	underscore_var,	. + 4
	.word	var1_null,	var3_ellipse
condeT:	@ (let ((temp test)) (if temp temp (cond clause1 ...)))
	.word	let_var,	. + 4
	.word	condcT + 16,	. + 4
	.word	. + 8,		scheme_null
	.word	if_var,	. + 4
	.word	var0_var,	. + 4
	.word	var0_var,	condcT + 72
cond_f:	@ pattern:	(_ (test result1 ...))
	@ template:	(if test (begin result1 ...))
	@		(_ (var1 var2 ...))
	@		(if var1 (begin var2 ..))
	@ var1 <-> test
	@ var2 <-> result1
	.word	condfP,	. + 4, condfT, scheme_null
condfP:	@ (_ (test result1 ...))
	.word	underscore_var,	. + 4
	.word	var1_2_ellipse,	scheme_null
condfT:	@ (if test (begin result1 ...))
	.word	if_var,	. + 4
	.word	var1_var,	cond_a + 8
cond_g:	@ pattern:	(_ (test result1 ...) clause1 ...)
	@ template:	(if test (begin result1 ...) (cond clause1 ...))
	@		(_ (var1 var2 ...) var3 ...)
	@		(if var1 (begin var2 ...) (cond var3 ...))
	@ var1 <-> test
	@ var2 <-> result1
	@ var3 <-> clause1
	.word	condgP,	. + 4, condgT, scheme_null
condgP:	@ (_ (test result1 ...) clause1 ...)
	.word	underscore_var,	. + 4
	.word	var1_2_ellipse,	var3_ellipse
condgT:	@ (if test (begin result1 ...) (cond clause1 ...))
	.word	if_var,	. + 4
	.word	var1_var,	. + 4
	.word	condaT,		condcT + 72

.balign	4
	
case:	SYMSIZE	4
	.ascii	"case"
	.balign 4

pcase:	@ (case key clause1 clause2 ...)
	@(define-syntax case
	@  (syntax-rules (else)
	@    ((_ (key ...) clauses ...)
	@     (let ((atom-key (key ...))) (case atom-key clauses ...)))
	@    ((_ key (else result1 ...))
	@     (begin result1 ...))
	@    ((_ key ((atom ...) result1 ...))
	@     (if (memv key (quote (atom ...))) (begin result1 ...)))
	@    ((_ key ((atoms ...) result1 ...) clause1 ...)
	@     (if (memv key (quote (atoms ...))) (begin result1 ...) (case key clause1 ...)))))
	.word	macro,		. + 4
	.word	else_null,	. + 4
	.word	case_a,	. + 4,	case_b,	. + 4,	case_c,	. + 4,	case_d,	scheme_null
case_a:	@ pattern:	(_ (key ...) clauses ...)
	@ template:	(let ((atom-key (key ...))) (case atom-key clauses ...))
	@		(_ (var0 ...) var2 ...)
	@ ->->->		(let (var1 (var0 ...)) (case var1 var2 ...))
	@ var0 <-> key
	@ var1 <-> atom-key
	@ var2 <-> clauses
	.word	caseaP,	. + 4, caseaT, scheme_null
caseaP:	@ (_ (key ...) clauses ...)
	.word	underscore_var,	. + 4
	.word	var0_ellipse,	var2_ellipse
caseaT:	@ (let ((atom-key (key ...))) (case atom-key clauses ...))
	.word	let_var,	. + 4
	.word	. + 8,		. + 28
	.word	. + 8,		scheme_null
	.word	var1_var,	. + 4
	.word	var0_ellipse,	scheme_null
	.word	. + 8,		scheme_null
	.word	case_var,	var1_2_ellipse
case_b:	@ pattern:	(_ key (else result1 ...))
	@ template:	(begin result1 ...)
	@		(_ var0 (else var3 ...))
	@		(begin var3 ...)
	@ var0 <-> key
	@ var3 <-> result1
	.word	casebP,	. + 4, casebT, scheme_null
casebP:	@ (_ key (else result1 ...))
	.word	underscore_var,	. + 4
	.word	var0_var,	. + 4
	.word	. + 8,		scheme_null
	.word	else_var,	var3_ellipse
casebT:	@ (begin result1 ...)
	.word	begin_var,	var3_ellipse
case_c:	@ pattern:	(_ key ((atom ...) result1 ...))
	@ template:	(if (memv key (quote (atom ...))) (begin result1 ...))
	@ ->->->key	(_ var1 ((var1 ...) var3 ...))
	@		(if (memv var0 (quote (var1 ...))) (begin var3 ...)))
	@ var0 <-> atom
	@ var1 <-> key
	@ var3 <-> result1
	.word	casecP,	. + 4, casecT, scheme_null
casecP:	@ (_ key ((atom ...) result1 ...))
	.word	underscore_var,	. + 4
	.word	var1_var,	. + 4
	.word	. + 8,		scheme_null
	.word	var0_ellipse,	var3_ellipse
casecT:	@ (if (memv key (quote (atom ...))) (begin result1 ...))
	.word	if_var,	. + 4
	.word	. + 8,		case_b + 8
	.word	memv_var,	. + 4
	.word	var1_var,	. + 4
	.word	. + 8,		scheme_null
	.word	quote_var,	. + 4
	.word	var0_ellipse,	scheme_null
case_d:	@ pattern:	(_ key ((atoms ...) result1 ...) clause1 ...)
	@ template:	(if (memv key (quote (atoms ...))) (begin result1 ...) (case key clause1 ...))
	@ ->-> atoms	(_ var1 ((var1 ...) var3 ...) var2 ...)
	@ ->-> key	(if (memv var0 (quote (var1 ...))) (begin var3 ...) (case var1 var2 ...)))) 
	@ var0 <-> atoms
	@ var1 <-> key
	@ var2 <-> clause1
	@ var3 <-> result1
	.word	casedP,	. + 4, casedT, scheme_null
casedP:	@ (_ key ((atoms ...) result1 ...) clause1 ...)
	.word	underscore_var,	. + 4
	.word	var1_var,	. + 4
	.word	casecP + 24,	var2_ellipse
casedT:	@ (if (memv key (quote (atoms ...))) (begin result1 ...) (case key clause1 ...))
	.word	if_var,	. + 4
	.word	casecT + 16,	. + 4
	.word	casebT,		caseaT + 40
	
			
.balign	4

and:	SYMSIZE	3
	.ascii	"and"
	.balign 4

pand:	@ (and exp1 exp2 ...)
	@ (define-syntax and
	@   (syntax-rules ()
	@     ((_) #t)
	@     ((_ test) test)
	@     ((_ test1 test2 ...)
	@      (if test1 (and test2 ...) #f))))
	.word	macro,			. + 4
	.word	scheme_null,		. + 4
	.word	and_a,	. + 4,	and_b,	. + 4,	and_c,	scheme_null
and_a:	@ ((_) #t)
	.word	underscore_null,	true_null	@ ((_) #t)
and_b:	@ ((_ test) test)
	.word	und_var1_null,		var1_null	@ ((_ test) test)
and_c:	@ ((_ test1 test2 ...)
	@  (if test1 (and test2 ...) #f))
	.word	und_var12_ell,		. + 4		@ ((_ test1 test2 ...)
	.word	. +  8,			scheme_null	@  (
	.word	if_var,		. + 4		@   if
	.word	var1_var,		. + 4		@     test1
	.word	. + 8,			false_null	@                       #f)
	.word	and_var,		var2_ellipse	@       (and test2 ...)

			
.balign	4

or:	SYMSIZE	2
	.ascii	"or"
	.balign 4

por:	@ (or exp1 exp2 ...)
	@(define-syntax or
	@  (syntax-rules ()
	@    ((_) #f )
	@    ((_ test) test)
	@    ((_ test1 test2 ...)
	@     (let ((x test1))
	@       (if x x (or test2 ...))))))
	.word	macro,			. + 4
	.word	scheme_null,		. + 4
	.word	or_a,	. + 4,	and_b,	. + 4,	or_c,	scheme_null	@ case b same as and
or_a:	@ ((_) #f )
	.word	underscore_null,	false_null	@ ((_) #f)
or_c:	@ ((_ test1 test2 ...)
	@  (let ((x test1))
	@    (if x x (or test2 ...))))))
	.word	und_var12_ell,		. + 4		@ ((_ test1 test2 ...)
	.word	. + 8,			scheme_null	@  (
	.word	let_var,		. + 4		@   let
	.word	. + 8,			. + 12		@     (
	.word	var0_1_null,		scheme_null	@      (x test1))
	.word	. + 8,			scheme_null	@   (                    )
	.word	if_var,		. + 4		@   if
	.word	var0_var,		. + 4		@     x
	.word	var0_var,		. + 4		@     x
	.word	. + 8,			scheme_null	@                       )
	.word	or_var,			var2_ellipse	@       (or test2 ...)

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.2.	binding constructs:	let, let*, letrec
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

.balign	4

let:	SYMSIZE	3
	.ascii	"let"
	.balign 4

plet:	@ (let <name> bindings-list exp1 exp2 ...)
	@ (define-syntax let
	@   (syntax-rules ()
	@     ((_ ((name val) ...) body1 ...)
	@      ((lambda (name ...) body1 ...)
	@       val ...))
	@     ((_ tag ((name val) ...) body1 ...)
	@      ((letrec
	@ 	 ((tag (lambda (name ...) body1 ...)))
	@	 tag)
	@       val ...))))
	.word	macro,		. + 4
	.word	scheme_null,	. + 4
	.word	let_a, . + 4, let_b, scheme_null
let_a:	@ pattern:	(_ ((name val) ...) body1 ...)
	@ template:	((lambda (name ...) body1 ...) val ...)
	@ var0 <-> name
	@ var1 <-> val
	@ var2 <-> body1
	.word	let_aP,	. + 4, let_aT, scheme_null
let_aP:	@ pattern:	(_ ((name val) ...) body1 ...)
	.word	underscore_var,	letcm1
let_aT:	@ template:	((lambda (name ...) body1 ...) val ...)
	.word	letcm2,		var1_ellipse
let_b:	@ pattern:	(_ tag ((name val) ...) body1 ...)
	@ template:	((letrec
	@		   ((tag (lambda (name ...) body1 ...)))
	@		  tag)
	@	         val ...)
	@ var0 <-> name
	@ var1 <-> val
	@ var2 <-> body1
	@ var3 <-> tag
	.word	let_bP,	. + 4, let_bT, scheme_null
let_bP:	.word	underscore_var,	. + 4
	.word	var3_var,	letcm1
let_bT:	.word	. + 8,		var1_ellipse
	.word	letrec_var,	. + 4
	.word	. + 8,		var3_null
	.word	. + 8,		scheme_null
	.word	var3_var,	. + 4
	.word	letcm2,		scheme_null
	
letcm1:	@ (((name val) ...) body1 ...)
	@ var0 <-> name
	@ var1 <-> val
	@ var2 <-> body1
	.word	. + 8,		var2_ellipse
	.word	var0_1_null,	ellipse_null

letcm2:	@ (lambda (name ...) body1 ...)
	@ var0 <-> name
	@ var2 <-> body1
	.word	lambda_var,	. + 4
	.word	var0_ellipse,	var2_ellipse

	
.balign	4

lets:	SYMSIZE	4
	.ascii	"let*"
	.balign 4

plets:	@ (let* bindings-list exp1 exp2 ...)
	@ (define-syntax let*
	@   (syntax-rules ()
	@     ((_ ()  body1 ...)
	@      (let ()  body1 ...))
	@     ((_ (binding1) body1 ...)
	@      (let (binding1) body1 ...))
	@     ((_ (binding1 binding2 ...) body1 ...)
	@      (let (binding1)
	@        (let* (binding2 ...) body1 ...)))))
	.word	macro,		. + 4
	.word	scheme_null,	. + 4
	.word	lets_a,	. + 4,	lets_b,	. + 4,	lets_c,	scheme_null
lets_a:	@ pattern:	(_ ()  body1 ...)
	@ template:	(let ()  body1 ...)
	@ var2 <-> body1
	.word	letsaP,	. + 4, letsaT, scheme_null
letsaP:	@ (_ ()  body1 ...)
	.word	underscore_var,	. + 4
	.word	scheme_null,	var2_ellipse
letsaT:	@ (let ()  body1 ...)
	.word	let_var,	letsaP + 8
lets_b:	@ pattern:	(_ (binding1) body1 ...)
	@ template:	(let (binding1) body1 ...)
	@ var1 <-> binding1
	@ var2 <-> body1
	.word	letsbP,	. + 4, letsbT, scheme_null
letsbP:	@ (_ (binding1) body1 ...)
	.word	underscore_var,	. + 4
	.word	var1_null,	var2_ellipse
letsbT:	@ (let (binding1) body1 ...)
	.word	let_var,	letsbP + 8
lets_c:	@ pattern:	(_ (binding1 binding2 ...) body1 ...)
	@ template:	(let (binding1)
	@		 (let* (binding2 ...) body1 ...))
	@ var0 <-> body1
	@ var1 <-> binding1
	@ var2 <-> binding2
	.word	letscP,	. + 4, letscT, scheme_null
letscP:	@ (_ (binding1 binding2 ...) body1 ...)
	.word	underscore_var,	. + 4
	.word	var1_2_ellipse,	var0_ellipse
letscT:	@ (let (binding1) (let* (binding2 ...) body1 ...))
	.word	let_var,	. + 4
	.word	var1_null,	. + 4
	.word	. + 8,		scheme_null
	.word	lets_var,	. + 4
	.word	var2_ellipse,	var0_ellipse

	
.balign	4
	
letr:	SYMSIZE	6
	.ascii	"letrec"
	.balign 4

pletr:	@ (letrec bindings-list exp1 exp2 ...)
	@ (define-syntax letrec
	@   (syntax-rules ()
	@     ((_ ((var1 init1) ...) body ...)
	@      (letrec #t (var1 ...) () ((var1 init1) ...) body ...))
	@     ((_ #t () (temp1 ...) ((var1 init1) ...) body ...)
	@      (let ((var1 #t) ...)
	@        (let ((temp1 init1) ...) (set! var1 temp1) ... body ...)))
	@     ((_ #t (x . y) temp ((var1 init1) ...) body ...)
	@      (letrec #t y (newtemp . temp) ((var1 init1) ...) body ...))))
	.word	macro,		. + 4
	.word	scheme_null,	. + 4
	.word	letr_a, . + 4, letr_b, . + 4,	letr_c, scheme_null
letr_a:	@ pattern:	(_ ((var1 init1) ...) body ...)
	@ template:	(letrec #t (var1 ...) () ((var1 init1) ...) body ...)
	@		(_ ((var0 var1) ...) var2 ...)
	@		(letrec #t (var0 ...) () ((var0 var1) ...) var2 ...)
	@ var0 <-> var1
	@ var1 <-> init1
	@ var2 <-> body
	.word	let_aP,	. + 4, letraT, scheme_null	@ pattern same as let_a
letraT:	@ (letrec #t (var1 ...) () ((var1 init1) ...) body ...)
	.word	letrec_var,	. + 4
	.word	scheme_true,	. + 4
	.word	var0_ellipse,	. + 4
	.word	scheme_null,	letcm1
letr_b:	@ pattern:	(_ #t () (temp1 ...) ((var0 init1) ...) body ...)
	@ template:	(let ((var0 #t) ...)
	@	          (let ((temp1 init1) ...) (set! var0 temp1) ... body ...))
	@		(_ #t () (var3 ...) ((var0 var1) ...) var2 ...)
	@		(let ((var0 #t) ...)
	@		  (let ((var3 var1) ...) (set! var0 var3) ... var2 ...))
	@ var0 <-> var0
	@ var1 <-> init1
	@ var2 <-> body
	@ var3 <-> temp1
	.word	letrbP,	. + 4, letrbT, scheme_null
letrbP:	@ (_ #t () (temp1 ...) ((var0 init1) ...) body ...)
	.word	underscore_var,	. + 4
	.word	scheme_true,	. + 4
	.word	scheme_null,	. + 4
	.word	var3_ellipse,	letcm1
letrbT:	@ (let ((var0 #t) ...)
	@   (let ((temp1 init1) ...) (set! var0 temp1) ... body ...))
	.word	let_var,	. + 4
	.word	. + 8,		. + 20
	.word	. + 8,		ellipse_null
	.word	var0_var,	true_null
	.word	. + 8,		scheme_null
	.word	let_var,	. + 4
	.word	. + 8,		. + 20
	.word	. + 8,		ellipse_null
	.word	var3_var,	var1_null
	.word	. + 8,		. + 20
	.word	set_var,	. + 4
	.word	var0_var,	var3_null
	.word	ellipsis_var,	var2_ellipse
letr_c:	@ pattern:	(_ #t (x . y) temp ((var0 init1) ...) body ...)
	@ template:	(letrec #t y (newtemp . temp) ((var0 init1) ...) body ...)
	@		(_ #t (var4 . var5) var3 ((var0 var1) ...) var2 ...)
	@		(letrec #t var5 (var6 . var3) ((var0 var1) ...) var2 ...)
	@ var0 <-> var0
	@ var1 <-> init1
	@ var2 <-> body
	@ var3 <-> temp
	@ var4 <-> x
	@ var5 <-> y
	@ var6 <-> newtemp
	.word	letrcP,	. + 4, letrcT, scheme_null
letrcP:	@ (_ #t (x . y) temp ((var0 init1) ...) body ...)
	.word	underscore_var,	. + 4
	.word	scheme_true,	. + 4
	.word	. + 8,		. + 12
	.word	var4_var,	var5_var
	.word	var3_var,	letcm1
letrcT:	@ (letrec #t y (newtemp . temp) ((var0 init1) ...) body ...)
	.word	letrec_var,	. + 4
	.word	scheme_true,	. + 4
	.word	var5_var,	. + 4
	.word	. + 8,		letcm1
	.word	var6_var,	var3_var

@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.4.	iteration:		do
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

.balign	4
	
do:	SYMSIZE	2
	.ascii	"do"
	.balign 4

pdo:	@ (do ((var1 init1 <step1>) ...) (test expr1 expr2 ...) command1 command2 ...)
@(define-syntax do
@  (syntax-rules ()
@    ((_ ((var init . step) ...) (test expr ...))
@     (do ((var init . step) ...) (test expr ...) #f))
@    ((_ ((var init . step) ...) (test expr ...) command ...)
@     (letrec
@         ((loop
@           (lambda (var ...)
@             (if test
@                 (begin (if #f  #f) expr ...)
@                 (begin command ... (loop (do #t var . step) ...))))))
@       (loop init ...)))
@    ((_ #t x) x)
@    ((_ #t x y) y)))
	.word	macro,		. + 4
	.word	scheme_null,	. + 4
	.word	do_a,	. + 4,	do_b,	. + 4,	do_c,	. + 4,	do_d,	scheme_null
do_a:	@ pattern:	(_ ((var init . step) ...) (test expr ...))
	@ template:	(do ((var init . step) ...) (test expr ...) #f)
	@		(_ ((var0 var3 . var4) ...) (var1 var2 ...))
	@ -> list		 (do ((var0 var3 . var4) ...) (var1 var2 ...) #f)
	@ var0 <-> var
	@ var1 <-> test
	@ var2 <-> expr
	@ var3 <-> init
	@ var4 <-> step
	.word	do_aP,	. + 4, do_aT, scheme_null
do_aP:	@ (_ ((var init . step) ...) (test expr ...))
	.word	underscore_var,	. + 4
	.word	. + 8,		. + 28
	.word	. + 8,		ellipse_null
	.word	var0_var,	. + 4
	.word	var3_var,	var4_var
	.word	var1_2_ellipse,	scheme_null
do_aT:	@ (do ((var init . step) ...) (test expr ...) #f)
	.word	do_var,		. + 4
	.word	do_aP + 16,	. + 4
	.word	var1_2_ellipse,	false_null
do_b:	@ pattern:	(_ ((var init . step) ...) (test expr ...) command ...)
	@ template:	(letrec
	@	         ((loop
	@	           (lambda (var ...)
	@	             (if test
	@	                 (begin (if #f  #f) expr ...)
	@	                 (begin command ... (loop (do #t var . step) ...))))))
	@	         (loop init ...))
	@ -> do, list	(do ((var0 var3 . var4) ...) ((var1 var2 ...)) var5 ...)
	@		(letrec
	@		  ((var6
	@		    (lambda (var0 ...)
	@		      (if var1
	@			 (begin (if #f #f) var2 ...)
	@			 (begin var5 ... (var6 (do #t var0 . var4) ...))))))
	@		 (var6 var3 ...))
	@ var0 <-> var
	@ var1 <-> test
	@ var2 <-> expr
	@ var3 <-> init
	@ var4 <-> step
	@ var5 <-> command
	@ var6 <-> loop
	.word	do_bP,	. + 4, do_bT, scheme_null
do_bP:	@ (_ ((var init . step) ...) (test expr ...) command ...)
	.word	underscore_var,	. + 4
	.word	do_aP + 16,	. + 4
	.word	var1_2_ellipse,	. + 4
	.word	var5_var,	ellipse_null
do_bT:	@ (letrec
	@   ((loop (lambda (var ...)
	@	      (if test
	@	         (begin (if #f #f) expr ...)
	@	         (begin command ... (loop (do #t var . step) ...))))))
	@  (loop init ...))
	.word	letrec_var,	. + 4
	.word	. + 24,		. + 4
	.word	. + 8,		scheme_null
	.word	var6_var,	var3_ellipse
	.word	. + 8,		scheme_null
	.word	var6_var,	. + 4
	.word	. + 8,		scheme_null
	.word	lambda_var,	. + 4
	.word	var0_ellipse,	. + 4
	.word	. + 8,		scheme_null
	.word	if_var,	. + 4
	.word	var1_var,	. + 4
	.word	. + 8,		. + 36
	.word	begin_var,	. + 4
	.word	. + 8,		var2_ellipse
	.word	if_var,	. + 4
	.word	scheme_false,	false_null
	.word	. + 8,		scheme_null
	.word	begin_var,	. + 4
	.word	var5_var,	. + 4
	.word	ellipsis_var,	. + 4
	.word	. + 8,		scheme_null
	.word	var6_var,	. + 4
	.word	. + 8,		ellipse_null
	.word	do_var,		. + 4
	.word	scheme_true,	. + 4
	.word	var0_var,	var4_var
do_c:	@ pattern:	(_ #t x)
	@ template:	x
	@		(_ #t var1)
	@		 var1
	@ var1 <-> x
	.word	do_cP,	. + 4, var1_var, scheme_null
do_cP:	@ (_ #t x)
	.word	underscore_var,	. + 4
	.word	scheme_true,	var1_null
do_d:	@ pattern:	(_ #t x y)
	@ template:	y
	@		(_ #t var0 var1)
	@		var1
	@ var0 <-> x
	@ var1 <-> y
	.word	do_dP,	. + 4, var1_var, scheme_null
do_dP:	@ (_ #t x y)
	.word	underscore_var,	. + 4
	.word	scheme_true,	var0_1_null

@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.5.	delayed evaluation:	delay
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

.balign	4

delay:	SYMSIZE	5
	.ascii	"delay"
	.balign 4
	
pdelay:	@ (delay expr)
	@(define-syntax delay
	@  (syntax-rules ()
	@    ((_ expr)
	@     (_mkp (lambda () expr)))))
	.word	macro,		. + 4
	.word	scheme_null,	. + 4
	.word	dlay_a, scheme_null
dlay_a:	@ pattern:	(_ expr)
	@ template:	(_mkp (lambda () expr))
	@ var1 <-> expr
	.word	dly_aP,	. + 4, dly_aT, scheme_null
dly_aP:	@ pattern:	(_ expr)
	.word	underscore_var,	var1_null
dly_aT:	@ template:	(_mkp (lambda () expr))
	.word	mkpromise_var,	. + 4
	.word	. + 8,		scheme_null
	.word	lambda_var,	. + 4
	.word	scheme_null,	var1_null

mkp:	SYMSIZE	4
	.ascii	"_mkp"
	.balign 4

mkprms:	@ (_mkp proc)
	@ on entry:	sv1 <- proc = (lambda env () expr)
	@ on exit:	sv1 <- (promise env () expr)
	PFUNC	1			@ primitive function, one input arg
	@ return a lambda with extended env
	@
	@ well, probably better off returning a compiled proc, branching to assembly code
	@ (maybe even two of those?)
	@
	@ var0 <- #f, var1 <- proc
	@ build var list
	ldr	sv2, =var2_var
	list	sv3, sv2
	ldr	sv2, =var1_var
	cons	sv3, sv2, sv3
	ldr	sv2, =var0_var
	cons	sv3, sv2, sv3		@ sv3 <- (var0 var1 var2)
	@ build val list
	list	sv4, sv1
	set	sv2, #f	
	cons	sv4, sv2, sv4
	cons	sv4, sv2, sv4		@ sv4 <- (#f #f proc)
	@ extend environment
	sav_ec				@ dts <- (env cnt ...)
	call	mkfrm
	set	sv1, env
	restor2	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	@ build promise (compiled thunk with extended env)
	set	sv2, #null
	ldr	sv3, =prmcod
	cons	sv2, sv2, sv3
	cons	sv2, sv1, sv2
	set	sv1, #procedure
	orr	sv1, sv1, #0xC000
	cons	sv1, sv1, sv2
	set	pc,  cnt

.balign	4

prmcod:	@ code for make-promise
	ldr	sv1, =var0_var
	bl	bndchk
	eq	sv5, #t
	bne	prmco1
	ldr	sv1, =var1_var
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco1:	
	sav_ec
	ldr	sv1, =var2_var
	bl	bndchk
	set	sv1, sv5
	set	sv2, #null
	call	apply
	set	sv2, sv1
	restor2	env, cnt
	ldr	sv1, =var0_var
	bl	bndchk
	eq	sv5, #t
	bne	prmco2
	ldr	sv1, =var1_var
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco2:	@ set result in promise env
	ldr	sv1, =var0_var
	bl	bndchk
	set	sv4, #t
	setcdr	sv3, sv4
	ldr	sv1, =var1_var
	bl	bndchk
	setcdr	sv3, sv2
	set	sv1, sv2
	set	pc,  cnt
		
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.c.	constants:		else, =>, var0, var1, var2, var3, var4, var5, var6
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

.balign	4

else:	SYMSIZE	4
	.ascii	"else"
	.balign 4

implies: SYMSIZE 2
	.ascii	"=>"
	.balign 4

var0:	SYMSIZE	4
	.ascii	"var0"
	.balign 4

var1:	SYMSIZE	4
	.ascii	"var1"
	.balign 4

var2:	SYMSIZE	4
	.ascii	"var2"
	.balign 4

var3:	SYMSIZE	4
	.ascii	"var3"
	.balign 4

var4:	SYMSIZE	4
	.ascii	"var4"
	.balign 4

var5:	SYMSIZE	4
	.ascii	"var5"
	.balign 4

var6:	SYMSIZE	4
	.ascii	"var6"
	.balign 4

var0_1_null:	.word	var0_var,	var1_null	@ (var0 var1)
und_var1_null:	.word	underscore_var,	var1_null	@ (_ var1)
und_var12_ell:	.word	underscore_var,	var1_2_ellipse	@ (_ var1 var2 ...)
var1_2_ellipse:	.word	var1_var,	var2_ellipse	@ (var1 var2 ...)
var0_ellipse:	.word	var0_var,	ellipse_null	@ (var0 ...)
var1_ellipse:	.word	var1_var,	ellipse_null	@ (var1 ...)
var2_ellipse:	.word	var2_var,	ellipse_null	@ (var2 ...)
var3_ellipse:	.word	var3_var,	ellipse_null	@ (var3 ...)
ellipse_null:	.word	ellipsis_var,	scheme_null	@ ( ...)
else_null:	.word	else_var,	scheme_null	@ (else)	
underscore_null: .word	underscore_var,	scheme_null	@ (_)
var1_null:	.word	var1_var,	scheme_null	@ (var1)
var3_null:	.word	var3_var,	scheme_null	@ (var3)
true_null:	.word	scheme_true,	scheme_null	@ (#t)
false_null:	.word	scheme_false,	scheme_null	@ (#f)

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

.endif		@ .ifndef r3rs
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				R3RS (see R5RS above)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.1.	conditionals:					cond, case, and, or
@	4.2.2.	binding constructs:				let, let*, letrec
@	4.2.3.	sequencing:					begin
@	4.2.4.	iteration:					do
@	4.2.5.	delayed evaluation:				delay
@	4.2.c.	constants:					else, =>, var0, var1, var2,
@								var3, var4, var5, var6
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.ifdef r3rs

.balign	4

cond:	SYMSIZE	4
	.ascii	"cond"
	.balign 4

pcond:	@ (cond clause1 clause2 ...)
	@ on entry:	sv1 <- (clause1 clause2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	0			@ syntax, listed input args
	sav_ec				@ dts <- (env cnt ...)
	set	sv4, sv1		@ sv4 <- (clause1 clause2 ...)
cond0:	@ evaluate tests in sequence and branch to expr as appropriate
	nullp	sv4			@ no more clauses?
	beq	condxt			@	if so,  jump to exit with '() or #f
	caar	sv1, sv4		@ sv1 <- test1-or-else from clause1
	ldr	sv3, =else_var		@ sv3 <- else
	eq	sv1, sv3		@ is sv1 = else?
	beq	cond1			@	if so,  jump to evaluate expressions1
	car	env, dts		@ env <- env
	save	sv4			@ dts <- ((clause1 clause2 ...) env cnt ...)
	call	eval			@ sv1 <- test1-val
	restor	sv4			@ sv4 <- (clause1 clause2 ...),	dts <- (env cnt ...)
	eq	sv1, #f			@ did test return #f?
	it	eq
	cdreq	sv4, sv4		@	if so,  sv4 <- (clause2 ...)
	beq	cond0			@	if so,  jump to continue evaluating tests
cond1:	@ evaluate expressions for #t test or else clause
	cdar	sv4, sv4		@ sv4 <- (<=>> expr1 expr2 ...)
	car	sv2, sv4		@ sv2 <- =>-or-expr1
	ldr	sv5, =implies_var	@ sv5 <- =>
	eq	sv2, sv5		@ is sv2 = =>?
	beq	cond2			@	if so,  jump to process that case
	set	sv2, #null		@ sv2 <- '()
	car	sv3, dts		@ sv3 <- env
	set	sv1, #procedure		@ sv1 <- procedure_tag
	orr	sv1, sv1, #0x4000	@ sv1 <- full proc tag
	llcons	sv1, sv1, sv3, sv2, sv4	@ sv1 <- proc == (lambda env () exp1 ...)
	restor2	env, cnt		@ env <- original env, cnt <- cnt, dts <- (...)
	b	apply
cond2:	@ evaluate the case with =>
	car	env, dts		@ env <- env
	list	sv1, sv1		@ sv2 <- (test-val)
	save	sv1			@ dts <- ((test-val) env cnt ...)
	cadr	sv1, sv4		@ sv1 <- procedure-of-1-arg
	call	eval			@ sv1 <- proc
	restor	sv2			@ sv2 <- (test-val) = argl
	restor2	env, cnt		@ env <- original env, cnt <- cnt, dts <- (...)
	b	apply
condxt:	@ exit without match
	restor2	env, cnt		@ env <- original env, cnt <- cnt, dts <- (...)
	set	pc, cnt

.balign	4
	
case:	SYMSIZE	4
	.ascii	"case"
	.balign 4

pcase:	@ (case key clause1 clause2 ...)
	@ on entry:	sv1 <- key
	@ on entry:	sv2 <- (clause1 clause2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax, one input arg, rest listed
	savrec	sv2			@ dts <- ((clause1 clause2 ...) env cnt ...)
	call	eval			@ sv1 <- key-val
	restor	sv4			@ sv4 <- (clause1 clause2 ...)
	save	sv1			@ dts <- (key-val env cnt ...)
	ldr	sv5, =else_var		@ sv5 <- else
case0:	@ look for key in clauses' datum
	nullp	sv4			@ done with clauses?
	beq	casext			@	if so,  jump to exit
	caar	sv2, sv4		@ sv2 <- datum1-or-else from clause1
	eq	sv2, sv5		@ is sv2 = else?
	beq	case1			@	if so,  jump to evaluate expressions1
	call	memv			@ sv1 <- sublist or #f (look for key-val sv1 in datum sv2)
	eq	sv1, #f			@ key not in datum?
	itT	eq
	careq	sv1, dts		@	if so,  sv1 <- key-val, restored
	cdreq	sv4, sv4		@	if so,  sv4 <- rest of datum-list
	beq	case0			@	if so,  jump to continue scanning datum-list
case1:	@ evaluate expressions
	restor3	sv1, env, cnt		@ sv1 <- dummy, env <- original env, cnt <- cnt, dts <- (...)
	cdar	sv1, sv4		@ sv1 <- (expr1 expr2 ...)
	b	sqnce
casext:	@ exit without match
	restor3	sv1, env, cnt		@ sv1 <- dummy, env <- original env, cnt <- cnt, dts <- (...)
	set	sv1, #f
	set	pc, cnt
	
.balign	4

and:	SYMSIZE	3
	.ascii	"and"
	.balign 4

pand:	@ (and exp1 exp2 ...)
	@ on entry:	sv1 <- (exp1 exp2 ...),		sv5<- env
	@ on exit:	sv1 <- result
	SYNTAX	0			@ syntax, listed input args
	set	sv4, #t			@ sv4 <- #t, default value with no args
	b	andor			@ jump to common and/or process loop

.balign	4

or:	SYMSIZE	2
	.ascii	"or"
	.balign 4

por:	@ (or exp1 exp2 ...)
	@ on entry:	sv1 <- (exp1 exp2 ...),		sv5<- env
	@ on exit:	sv1 <- result
	SYNTAX	0			@ syntax, listed input args
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
	bne	andorL			@		if neither, jump to keep looping (AND with NOT #f)
andoxt:	@ immediate exit
	cdr	dts, dts		@ dts <- (env cnt ...)
	restor2	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	set	pc, cnt			@ return
andorT:	@ tail-call for last expression in function body
	cdr	dts, dts		@ dts <- (env cnt...)
	restor2	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	b	eval			@ jump to evaluate tail

.balign	4

let:	SYMSIZE	3
	.ascii	"let"
	.balign 4

plet:	@ (let <name> bindings-list exp1 exp2 ...)
	@ on entry:	sv1 <- name or bindings-list
	@ on entry:	sv2 <- (bindings-list exp1 exp2 ...) or (exp1 exp2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax, one input arg, rest listed
	save	sv1			@ dts <- (<name>-or-b-lst ...)
	varp	sv1			@ is this a named-let?
	it	eq
	snoceq	sv1, sv2, sv2		@	if so,  sv1 <- b-lst,	sv2 <- (exp1 exp2 ...)
	save	sv2			@ dts <- ((exp1 exp2 ...) <name>-or-b-lst ...)
	set	sv5, #null		@ sv5 <- '()
	list	sv2, sv5		@ sv2 <- (() . var-list-tail)
	list	sv5, sv5		@ sv5 <- (() . uval-list-tail)
	save2	sv2, sv5		@ dts <- ((() . vrlst) (() . uvllst) (ex1 ex2 ..) <nam>-or-blst ..)
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
	restor2	sv2, sv5		@ sv2 <- (() . vrlst), sv5<-(().uvllst), dts<-((ex1.) <nam>|blst .)
	cdr	sv2, sv2		@ sv2 <- var-list
	cdr	sv5, sv5		@ sv5 <- uval-list
	restor	sv3			@ sv3 <- (exp1 exp2 ...),	dts <-  (<name>-or- b-lst ...)
	cons	sv3, sv2, sv3		@ sv3 <- ((var1 ...) (exp1 exp2 ...))
	restor	sv2			@ sv2 <- <name> or b-lst,	dts <- (...)
	varp	sv2			@ is this a named-let?
	it	ne
	bne	let2			@	if not, jump to continue
	list	sv4, sv2		@ sv4 <- (name) = upcoming binding for name
	list	sv1, sv4		@ sv1 <- ((name)) = upcoming binding frame for name	
	cons	env, sv1, env		@ env <- updated environment for named-let
let2:	@ build lambda and jump to eval
	set	sv1, env		@ sv1 <- env
	cons	sv3, sv1, sv3		@ sv3 <- (env (var1 ...) (exp1 exp2 ...))
	set	sv1, #procedure		@ sv1 <- procedure tag
	orr	sv1, sv1, #0x4000	@ sv1 <- full proc tag
	cons	sv1, sv1, sv3		@ sv1 <- (procedure env (var1 ...) (exp1 exp2 ...)) = lambda-expr
	varp	sv2			@ is this a named-let?
	it	eq
	setcdreq sv4, sv1		@	if so,  store binding for named-let in environment
	cons	sv1, sv1, sv5		@ sv1 <- ((procedure env (var1 ...) (exp1 exp2 ...)) uval1 ...)
	b	eval			@ jump to evaluate the lambda
	
.balign	4

lets:	SYMSIZE	4
	.ascii	"let*"
	.balign 4

plets:	@ (let* bindings-list exp1 exp2 ...)
	@ on entry:	sv1 <- bindings-list
	@ on entry:	sv2 <- (exp1 exp2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax, one input arg, rest listed
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
	restor	env			@ env <- env,			dts <- (rst-b-lst (exp1 ..) env ..)
	caar	sv2, env		@ sv2 <- (var1) = null binding for var1
	setcdr	sv2, sv1		@ store val1 in (var1) binding
	restor	sv1			@ sv1 <- rest-of-b-lst,		dts <- ((exp1 ...) cnt ...)
	b	lets0			@ jump to continue evaluating and binding the inits
lets1:	@ evaluate body in environment extended with let-bindings
	restor2	sv1, cnt		@ sv1 <- (exp1 ...), cnt <- cnt, dts <- (...)
	b	sqnce
			
.balign	4

letr:	SYMSIZE	6
	.ascii	"letrec"
	.balign 4

pletr:	@ (letrec bindings-list exp1 exp2 ...)
	@ on entry:	sv1 <- bindings-list
	@ on entry:	sv2 <- (exp1 exp2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	1			@ syntax, one input arg, rest listed
	save3	sv1, sv2, cnt		@ dts <- (let-bindings-lst (exp1 ...) cnt ...)
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
	save2	sv4, sv3		@ dts <- (val-lst rst-b-lst let-env (exp1 ...) env cnt ...)
	call	eval			@ sv1 <- val1
	restor2	sv4, sv3		@ sv4 <- oldvlst, sv3 <- rstblst, dts <- (ltnv ltbds (ex1 .) cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- (val1 ...) = updated vals list	
	b	letr2			@ jump to continue building init vals list
letr3:	@ keep going
	restor	env			@ env <- let-env,	dts <- (let-bindings-list (exp1 ..) cnt ..)
	restor3	sv3, sv1, cnt		@ sv3 <- let-bindings-lst, sv1 <- (ex1 ..), cnt <- cnt, dts <- (..)
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
	
.balign	4
	
do:	SYMSIZE	2
	.ascii	"do"
	.balign 4

pdo:	@ (do ((var1 init1 <step1>) ...) (test expr1 expr2 ...) command1 command2 ...)
	@ on entry:	sv1 <- ((var1 init1 <step1>) ...)
	@ on entry:	sv2 <- (test expr1 expr2 ...)
	@ on entry:	sv3 <- (command1 command2 ...)
	@ on exit:	sv1 <- result
	SYNTAX	2			@ syntax, two input args
	save3	sv2, sv3, cnt		@ dts <- ((test expr1 ...) (command1 ...) cnt ...)
	save2	env, sv1		@ dts <- (env inits-list (test expr1 ...) (command1 ...) cnt ...)
	set	sv4, #null		@ sv4 <- '() = initial init-vals-list
do0:	@ build list of evaluated inits into sv4
	nullp	sv1			@ done with inits list?
	beq	do1			@	if so, jump to continue
	car	env, dts		@ env <- env
	snoc	sv1, sv2, sv1		@ sv1 <- (var1 init1 <step1>),	sv2 <- ((var2 init2 <step2>) ...)
	cadr	sv1, sv1		@ sv1 <- init1
	save2	sv4, sv2		@ dts <- (init-vals-list ((var2 init2 <step2>) ...) env ...)
	call	eval			@ sv1 <- init1-val
	restor	sv4			@ sv4 <- init-vals-list, dts <- (((var2 init2 <step2>) ...) env ..)
	cons	sv4, sv1, sv4		@ sv4 <- updated init-vals-list
	restor	sv1			@ sv1 <- ((vr2 in2 <stp2>) ..),	dts <- (env inits-list ...)
	b	do0			@ jump to continue evaluating inits
do1:	@ reverse vals list
	set	sv2, sv4
	set	sv4, #null
do1_b:	nullp	sv2
	beq	do1_c
	snoc	sv1, sv2, sv2
	cons	sv4, sv1, sv4
	b	do1_b
do1_c:	@ build environment frame for do-vars
	cadr	sv3, dts		@ sv3 <- inits-list
	car	env, dts		@ env <- saved env
	call	mkfrm			@ env <- do-env = (new-frame . env)
do5:	@ evaluate the test
	cddr	sv1, dts		@ sv1 <- ((test expr1 ...) (command1 ...) env cnt ...)
	caar	sv1, sv1		@ sv1 <- test
	save	env			@ dts <- (do-env env inits-list ...)
	call	eval			@ sv1 <- value of test
	eq	sv1, #f			@ done with do?
	bne	doexit			@	if so,  jump to exit
	@ evaluate the commands
	car	env, dts		@ env <- do-env
	cdddr	sv1, dts		@ sv1 <- ((test expr1 ...) (command1 ...) cnt ...)
	cadr	sv1, sv1		@ sv1 <- (command1 ...)
	call	sqnce
	@ evaluate the steps
	cddr	sv1, dts		@ sv1 <- (inits-list ...)
	car	sv1, sv1		@ sv1 <- ((var1 init1 <step1>) (var2 init2 <step2>) ...)
	set	sv4, #null		@ sv4 <- '() = initial init-vals-list
do6:	@ build list of evaluated steps into sv4 then jump back to iterate
	nullp	sv1			@ done with steps list?
	it	eq
	cdreq	dts, dts		@	if so,  dts <- (env inits-lst (test exp1 .) (cmd1 .) cnt .)
	beq	do1			@	if so,  jump to next iteration
	car	env, dts		@ env <- do-env
	snoc	sv1, sv2, sv1		@ sv1 <- (var1 init1 <step1>),	sv2 <- ((var2 init2 <step2>) ...)
	save2	sv4, sv2		@ dts <- (step-vals-list ((var2 init2 <step2>) ...) do-env ...)
	cddr	sv2, sv1		@ sv2 <- (<step>)
	nullp	sv2			@ no step?
	itE	eq
	careq	sv1, sv1		@	if so,  sv1 <- var1
	carne	sv1, sv2		@	if not, sv1 <- step1
	call	eval			@ sv1 <- step1-val
	restor	sv4			@ sv4 <- step-vals-list, dts <- (((var2 ini2 <stp2>) ..) do-env ..)
	cons	sv4, sv1, sv4		@ sv4 <- updated step-vals-list
	restor	sv1			@ sv1 <- ((vr2 in2 <stp2>) ..),	dts <- (do-env env inits-list ...)
	b	do6			@ jump to continue evaluating steps
doexit:	@ exit the do -- evaluate the expressions
	restor3	sv1, env, sv2		@ sv1 <- donv, env <- nv, sv2<-dmy, dts<-((tst e1 .) (cm1 .) cnt .)
	restor3	sv5, sv2, cnt		@ sv5 <- (test expr1 ...), sv2 <- dummy, cnt <- cnt, dts <- (...)
	set	sv2, #null		@ sv2 <- '()
	cdr	sv5, sv5		@ sv5 <- (expr1 ...)
	set	sv3, #procedure		@ sv3 <- procedure_tag
	orr	sv3, sv3, #0x4000	@ sv3 <- full proc tag	
	llcons	sv1, sv3, sv1, sv2, sv5
	b	apply			@ jump to evaluate expression sequence
	
.balign	4

delay:	SYMSIZE	5
	.ascii	"delay"
	.balign 4
	
pdelay:	@ (delay expr)
	@ on entry:	sv1 <- (expr)
	@ on exit:	sv1 <- (promise env () expr)
	SYNTAX	0			@ syntax, listed input args
	set	sv2, sv1		@ sv2 <- (expr)
	set	sv1, #null		@ sv1 <- '()
	set	sv3, #procedure		@ sv3 <- partial proc tag
	orr	sv3, sv3, #0x4000	@ sv3 <- full proc tag
	llcons	sv1, sv3, env, sv1, sv2	@ sv1 <- (lambda env () expr)
	b	mkprme
	
.balign	4

mkp:	SYMSIZE	4
	.ascii	"_mkp"
	.balign 4

mkprms:	@ (_mkp proc)
	@ on entry:	sv1 <- proc = (lambda env () expr)
	@ on exit:	sv1 <- (promise env () expr)
	PFUNC	1			@ one input var | variable type
mkprme:	@ [internal entry]
	@ return a lambda with extended env
	@
	@ well, probably better off returning a compiled proc, branching to assembly code
	@ (maybe even two of those?)
	@
	@ var0 <- #f, var1 <- proc
	@ build var list
	ldr	sv2, =var0_var
	ldr	sv3, =var1_var
	ldr	sv4, =var2_var
	set	sv5, #null
	llcons	sv3, sv2, sv3, sv4, sv5	@ sv3 <- (var0 var1 var2)
	@ build val list
	set	sv2, #f	
	llcons	sv4, sv2, sv2, sv1, sv5	@ sv4 <- (#f #f proc)
	@ extend environment
	sav_ec				@ dts <- (env cnt ...)
	call	mkfrm
	set	sv1, env
	restor2	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	@ build promise (compiled thunk with extended env)
	set	sv2, #null
	ldr	sv3, =prmcod
	set	sv4, #procedure
	orr	sv4, sv4, #0xC000
	llcons	sv1, sv4, sv1, sv2, sv3	@ sv1 <- promise = (promise-tag prm-env () prm-cod)
	set	pc,  cnt

.balign	4

prmcod:	@ code for make-promise
	ldr	sv1, =var0_var
	bl	bndchk
	eq	sv5, #t
	bne	prmco1
	ldr	sv1, =var1_var
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco1:	
	sav_ec
	ldr	sv1, =var2_var
	bl	bndchk
	set	sv1, sv5
	set	sv2, #null
	call	apply
	set	sv2, sv1
	restor2	env, cnt
	ldr	sv1, =var0_var
	bl	bndchk
	eq	sv5, #t
	bne	prmco2
	ldr	sv1, =var1_var
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco2:	@ set result in promise env
	ldr	sv1, =var0_var
	bl	bndchk
	set	sv4, #t
	setcdr	sv3, sv4
	ldr	sv1, =var1_var
	bl	bndchk
	setcdr	sv3, sv2
	set	sv1, sv2
	set	pc,  cnt


@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.c.	constants:		else, =>, var0, var1, var2, var3, var4, var5, var6
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

.balign	4

else:	SYMSIZE	4
	.ascii	"else"
	.balign 4

implies: SYMSIZE 2
	.ascii	"=>"
	.balign 4

var0:	SYMSIZE	4
	.ascii	"var0"
	.balign 4

var1:	SYMSIZE	4
	.ascii	"var1"
	.balign 4

var2:	SYMSIZE	4
	.ascii	"var2"
	.balign 4

var3:	SYMSIZE	4
	.ascii	"var3"
	.balign 4

var4:	SYMSIZE	4
	.ascii	"var4"
	.balign 4

var5:	SYMSIZE	4
	.ascii	"var5"
	.balign 4

var6:	SYMSIZE	4
	.ascii	"var6"
	.balign 4

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

.endif		@ .ifdef r3rs
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@			INTEGER ONLY (see general numbers further down)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.2.	Numbers
@	6.2.5	Numerical operations:	zero?, positive?, negative?, odd?, even?,
@					max, min, abs, gcd, lcm
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			flsfxt, trufxt, boolxt, corerr, notfxt
@						save, save3, cons, sav_rc, zmaloc (straloc)	
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.ifdef	integer_only

.balign	4

szero:	SYMSIZE	5
	.ascii	"zero?"
	.balign 4

zero:	@ (zero? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1
	PFUNC	1
	izerop	sv1
	b	boolxt

.balign	4
	
sposit:	SYMSIZE	9
	.ascii	"positive?"
	.balign 4
	
positi:	@ (positive? obj)
	@ on entry:	sv1 <- obj	
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1, rva
	PFUNC	1
	izerop	sv1
	beq	notfxt
	postv	sv1			@ is number positive?
	b	boolxt			@ exit with #t/#f based on result

.balign	4
	
snegat:	SYMSIZE	9
	.ascii	"negative?"
	.balign 4

negati:	@ (negative? obj)
	@ on entry:	sv1 <- obj	
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1, rva
	PFUNC	1
	postv	sv1			@ is number positive?
	b	notfxt			@ exit with #f/#t based on result

.balign	4
	
sodd:	SYMSIZE	4
	.ascii	"odd?"
	.balign 4
	
odd:	@ (odd? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1, rva
	PFUNC	1
	intgrp	sv1			@ is sv1 an integer?
	bne	boolxt			@	if not, exit with #f
	tst	sv1, #0x04		@ is sv1 even?
	b	notfxt			@ return with not #t/#f
	
.balign	4
	
seven:	SYMSIZE	5
	.ascii	"even?"
	.balign 4
	
even:	@ (even? obj)
	@ on entry:	sv1 <- obj	
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1, rva
	PFUNC	1
	intgrp	sv1			@ is sv1 an integer?
	it	eq
	tsteq	sv1, #0x04		@	if so,  is it even?
	b	boolxt			@ return with #t/#f

.balign	4
	
mmglen:	@ entry for max, min, gcd, lcm (integer only version)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on entry:	sv5 <- binary operator = maxint, minint, gcdint or lcmint
	@ on exit:	sv4 <- startup value for rdcnml
	nullp	sv1			@ are there no arguments?
	itE	eq
	seteq	sv4, sv1		@	if so,  sv4 <- '()
	carne	sv4, sv1		@	if not, sv4 <- num1
	b	rdcnml			@ jump to reduce arg-list using operator and default value

.balign	4
	
smax:	SYMSIZE	3
	.ascii	"max"
	.balign 4

max:	@ (max num1 num2 ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on exit:	sv1 <- max of (num1 num2 ...)
	@ modifies:	sv1-sv5, rva-rvc
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	cmp	sv1, sv2		@ is x1 >= x2 ?
	it	mi
	setmi	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	set	pc,  lnk		@ return with largest number in sv1

.balign	4
	
smin:	SYMSIZE	3
	.ascii	"min"
	.balign 4
	
min:	@ (min num1 num2 ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	cmp	sv1, sv2		@ is x1 >= x2 ?
	it	pl
	setpl	sv1, sv2		@	if not, sv1 <- x2 (smallest number)
	set	pc,  lnk		@ return with smallest number in sv1

.balign	4
	
sabs:	SYMSIZE	3
	.ascii	"abs"
	.balign 4
	
abs:	@ (abs number)
	@ on entry:	sv1 <- number
	PFUNC	1
	iabs	sv1, sv1
	set	pc,  cnt

.balign	4
	
sgcd:	SYMSIZE	3
	.ascii	"gcd"
	.balign 4
	
gcd:	@ (gcd n1 ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
_func_
gcdint:	@ gcd for int
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv3
	set	sv3, sv1
	set	sv1, #i0
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
	restor2	sv2, sv3
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk

.balign	4
	
slcm:	SYMSIZE	3
	.ascii	"lcm"
	.balign 4
	
lcm:	@ (lcm n1 ...)			  ((n1 ...) ...) -> (int ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	eq	sv1, #i0
	it	ne
	eqne	sv2, #i0
	itT	eq
	seteq	sv1, #i0
	seteq	pc,  lnk
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	iabs	sv1, sv1
	iabs	sv2, sv2
	save3	sv1, sv2, sv3		@ dts <- (int1 int2 lnk ...)
	bl	gcdint			@ sv1 <- gcd of int1 and int2 (scheme int)
	set	sv2, sv1
	restor	sv1			@ sv1 <- int1,		dts <- (int2 lnk ...)
	bl	idivid			@ sv1 <- n1 / gcd (scheme int)
	restor2	sv2, sv3		@ sv2 <- int2, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0
	b	prdint
	
.endif

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@			GENERAL NUMBERS (see also integers only further up)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.2.	Numbers
@	6.2.5	Numerical operations:	zero?, positive?, negative?, odd?, even?,
@					max, min, abs,gcd, lcm, rationalize
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			flsfxt, trufxt, boolxt, corerr, notfxt
@						save, save3, cons, sav_rc, zmaloc (straloc)	
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.ifndef	integer_only

.balign	4
	
szero:	SYMSIZE	5
	.ascii	"zero?"
	.balign 4

zero:	@ (zero? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1
	PFUNC	1
	zerop	sv1
	b	boolxt

.balign	4
	
sposit:	SYMSIZE	9
	.ascii	"positive?"
	.balign 4
	
positi:	@ (positive? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	EPFUNC	0, onumgto, 1		@ primitive, init-sv4 = none, fentry = numgto, narg = 1
	@ jump table for positive?
	.word	flsfxt
	.word	pstint
	.word	pstflt
	.word	pstrat
	.word	flsfxt

_func_	
pstrat:	@ positive? for rat
	numerat	sv1, sv1
_func_	
pstint:	@ positive? for int
_func_	
pstflt:	@ positive? for flt
	zerop	sv1
	beq	notfxt
	postv	sv1			@ is number positive?
	b	boolxt			@ exit with #t/#f based on result

.balign	4
	
snegat:	SYMSIZE	9
	.ascii	"negative?"
	.balign 4

negati:	@ (negative? obj)
	@ on entry:	sv1 <- obj	
	@ on exit:	sv1 <- #t/#f
	EPFUNC	0, onumgto, 1		@ primitive, init-sv4 = none, fentry = numgto, narg = 1
	@ jump table for negative?
	.word	flsfxt
	.word	ngtint
	.word	ngtflt
	.word	ngtrat
	.word	flsfxt

_func_	
ngtrat:	@ negative? for rat
	numerat	sv1, sv1
_func_	
ngtint:	@ negative? for int
_func_	
ngtflt:	@ negative? for flt
	postv	sv1			@ is number positive?
	b	notfxt			@ exit with #f/#t based on result

.balign	4
	
sodd:	SYMSIZE	4
	.ascii	"odd?"
	.balign 4
	
odd:	@ (odd? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1, rva
	PFUNC	1
	intgrp	sv1			@ is sv1 an integer?
	bne	boolxt			@	if not, exit with #f
	tst	sv1, #0x04		@ is sv1 even?
	b	notfxt			@ return with not #t/#f
	
.balign	4
	
seven:	SYMSIZE	5
	.ascii	"even?"
	.balign 4
	
even:	@ (even? obj)
	@ on entry:	sv1 <- obj	
	@ on exit:	sv1 <- #t/#f
	@ modifies:	sv1, rva
	PFUNC	1
	intgrp	sv1			@ is sv1 an integer?
	it	eq
	tsteq	sv1, #0x04		@	if so,  is it even?
	b	boolxt			@ return with #t/#f

.balign	4
	
mmglen:	@ entry for max, min, gcd, lcm (general numbers version)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on entry:	sv5 <- binary operator table = maxtb, mintb, gcdtb or lcmtb
	@ on exit:	sv4 <- startup value for rdcnml
	nullp	sv1			@ are there no arguments?
	itE	eq
	seteq	sv4, sv1		@	if so,  sv4 <- '()
	carne	sv4, sv1		@	if not, sv4 <- num1
	b	rdcnml			@ jump to reduce arg-list using operator and default value

.balign	4
	
smax:	SYMSIZE	3
	.ascii	"max"
	.balign 4

max:	@ (max num1 num2 ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on exit:	sv1 <- max of (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	@ jump table for max
	.word	corerr
	.word	maxint
	.word	maxflt
	.word	maxrat
	.word	corerr

_func_	
maxflt:	@ max for flt
	anynan	sv1, sv2
	beq	nanlxt
	postv	sv1			@ is x1 positive?
	it	ne
	postvne	sv2			@	if so,  is x2 negative?
	bne	minint			@	if so,  jump to compare that (both floats negative)
	postv	sv1			@ is x1 positive or 0?
	itE	ne
	setne	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	postveq	sv2			@	if so,  is x2 positive or 0?
	it	ne
	setne	pc,  lnk		@	if not, (either is negative) exit with the non-negative one
	@ continue to maxint

_func_	
maxint:	@ max for int
	cmp	sv1, sv2		@ is x1 >= x2 ?
	it	mi
	setmi	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	set	pc,  lnk		@ return with largest number in sv1

_func_	
maxrat:	@ max for rat
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save3	sv1, sv2, sv3
	denom	sv1, sv1
	numerat	sv2, sv2
	bl	prdint
	set	sv3, sv1
	snoc	sv1, sv2, dts
	car	sv2, sv2
	numerat	sv1, sv1
	denom	sv2, sv2
	bl	prdint
	set	sv2, sv3
	ldr	rvc, =gttb
mxmnrt:	@ max/min common completion for rat
	bl	numjmp	
	eq	sv1, #f
	restor3	sv1, sv2, sv3
	orr	lnk, sv3, #lnkbit0
	it	eq
	seteq	sv1, sv2		@	if not, sv1 <- x2 (largest number)
	denom	sv2, sv1
	eq	sv2, #5
	it	eq
	nmrtreq	sv1, sv1
	set	pc,  lnk		@ return with largest number in sv1

.balign	4
	
smin:	SYMSIZE	3
	.ascii	"min"
	.balign 4
	
min:	@ (min num1 num2 ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on exit:	sv1 <- min of (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	@ jump table for min
	.word	corerr
	.word	minint
	.word	minflt
	.word	minrat
	.word	corerr

_func_	
minflt:	@ min for flt
	anynan	sv1, sv2
	beq	nanlxt
	postv	sv1			@ is x1 negative?
	it	ne
	postvne	sv2			@	if so,  is x2 negative?
	bne	maxint			@	if so,  jump to compare that (both floats negative)
	postv	sv2			@ is x2 positive or 0?
	itE	ne
	setne	sv1, sv2		@	if not, sv1 <- x2 (smallest number)
	postveq	sv1			@	if so,  is x1 positive or 0?
	it	ne
	setne	pc,  lnk		@	if not, (either is negative) exit with the negative one
	@ continue to minint

_func_	
minint:	@ min for int
	cmp	sv1, sv2		@ is x1 >= x2 ?
	it	pl
	setpl	sv1, sv2		@	if not, sv1 <- x2 (smallest number)
	set	pc,  lnk		@ return with smallest number in sv1
	
_func_	
minrat:	@ min for rat
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save3	sv1, sv2, sv3
	denom	sv1, sv1
	numerat	sv2, sv2
	bl	prdint
	set	sv3, sv1
	snoc	sv1, sv2, dts
	car	sv2, sv2
	numerat	sv1, sv1
	denom	sv2, sv2
	bl	prdint
	set	sv2, sv3
	ldr	rvc, =lttb
	b	mxmnrt			@ jump to common completion of max/min for rat
	
.balign	4
	
sabs:	SYMSIZE	3
	.ascii	"abs"
	.balign 4
	
abs:	@ (abs number)
	@ on entry:	sv1 <- number
	@ on exit:	sv1 <- absolute value of number
	EPFUNC	0, onumgto, 1		@ primitive, init-sv4 = none, fentry = numgto, narg = 1
	.word	corerr
	.word	absint
	.word	absflt
	.word	absrat
	.word	corerr

.balign	4
	
sgcd:	SYMSIZE	3
	.ascii	"gcd"
	.balign 4
	
gcd:	@ (gcd n1 ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on exit:	sv1 <- gcd of (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	.word	corerr
	.word	gcdint
	.word	gcdflt
	.word	gcdrat
	.word	corerr

_func_
gcdflt:	@ gcd for flt
	bic	sv3, lnk, #lnkbit0	@ sv5 <- lnk, saved (and made even if Thumb2)
	save	sv3
	bl	itrunc
	set	sv3, sv1
	set	sv1, sv2
	bl	itrunc
	set	sv2, sv1
	set	sv1, #f0
	b	gcdien

_func_
gcdrat:	@ gcd for rat
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save3	sv1, sv2, sv3		@ dts <- (rat1 rat2 lnk ...)
	denom	sv1, sv1
	denom	sv2, sv2
	bl	lcmint			@ sv1 <- lcm of denom1 and denom2 (scheme int)
	restor2	sv2, sv3		@ sv2 <- rat1, sv3 <- rat2, dts <- (lnk ...)
	save	sv1			@ dts <- (denom-lcm lnk ...)
	numerat	sv1, sv2
	numerat	sv2, sv3
	bl	gcdint			@ sv1 <- numerat-gcd
	restor2	sv2, sv3		@ sv2 <- denom-lcm, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	b	makrat			@ jump to make rational result

.balign	4

slcm:	SYMSIZE	3
	.ascii	"lcm"
	.balign 4
	
lcm:	@ (lcm n1 ...)			  ((n1 ...) ...) -> (int ...)
	@ on entry:	sv1 <- (num1 num2 ...)
	@ on exit:	sv1 <- lcm of (num1 num2 ...)
	EPFUNC	0, ommglen, 0		@ primitive, init-sv4 = none, fentry = mmglen, narg = listed
	.word	corerr
	.word	lcmint
	.word	lcmflt
	.word	lcmrat
	.word	corerr

_func_
lcmint:	@ lcm for int
	eq	sv1, #i0
	it	ne
	eqne	sv2, #i0
	itT	eq
	seteq	sv1, #i0
	seteq	pc,  lnk
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	iabs	sv1, sv1
	iabs	sv2, sv2
	save3	sv1, sv2, sv3		@ dts <- (int1 int2 lnk ...)
	bl	gcdint			@ sv1 <- gcd of int1 and int2 (scheme int)
	set	sv2, sv1
	restor	sv1			@ sv1 <- int1,		dts <- (int2 lnk ...)
	bl	idivid			@ sv1 <- n1 / gcd (scheme int)
	restor2	sv2, sv3		@ sv2 <- int2, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0
	b	prdint

_func_
lcmflt:	@ lcm for flt
	eq	sv1, #f0
	it	ne
	eqne	sv2, #f0
	itT	eq
	seteq	sv1, #f0
	seteq	pc,  lnk
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv3
	bl	itrunc
	swap	sv1, sv2, sv3
	bl	itrunc
	bl	lcmint
	restor	sv3			@ sv3 <- lnk,		dts <- (...)
	orr	lnk, sv3, #lnkbit0
	b	i12flt

_func_
lcmrat:	@ lcm for rat
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save3	sv1, sv2, sv3		@ dts <- (rat1 rat2 lnk ...)
	denom	sv1, sv1
	denom	sv2, sv2
	bl	gcdint			@ sv1 <- gcd of denom1 and denom2 (scheme int)
	restor2	sv2, sv3		@ sv2 <- rat1, sv3 <- rat2, dts <- (lnk ...)
	save	sv1			@ dts <- (denom-gcd lnk ...)
	numerat	sv1, sv2
	numerat	sv2, sv3
	bl	lcmint			@ sv1 <- numerat-lcm
	restor2	sv2, sv3		@ sv2 <- denom-gcd, sv3 <- lnk, dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	izerop	sv2
	it	eq
	seteq	pc,  lnk
	b	makrat			@ jump to make rational result

.balign	4
	
srtnlz:	SYMSIZE	11
	.ascii	"rationalize"
	.balign 4

rtnlz:	@ (rationalize x y)
	@ on entry:	sv1 <- x
	@ on entry:	sv2 <- y
	@ on exit:	sv1 <- ratio
	EPFUNC	0, ounijmp, 2		@ primitive, init-sv4 = none, fentry = unijmp, narg = 2
	.word	corerr
	.word	rtzint
	.word	rtzflt
	.word	rtzrat
	.word	corerr
rtyspc:	@ special returns table for rationalize, based on y, tested first (but after x == nan)
	.word	return			@   x <- (rationalize x 0)
	.word	f0fxt			@ 0.0 <- (rationalize x inf)
	.word	f0fxt			@ 0.0 <- (rationalize x -inf)
rtxspc:	@ special returns table for rationalize, based on x, tested after rtyspc
	.word	return			@    0 <- (rationalize 0 y)
	.word	return			@  inf <- (rationalize inf y)
	.word	return			@ -inf <- (rationalize -inf y)

_func_
rtzint:	@ rationalize for int
	iabs	sv2, sv2
	set	sv5, sv1
	iabs	sv1, sv1
	set	sv4, sv2
	bl	plsint
	set	sv2, sv4
	set	sv4, sv1
	set	sv1, sv5
	iabs	sv1, sv1
	bl	mnsint
	eor	rva, sv1, sv4
	postv	rva
	it	ne
	setne	sv1, #i0
rtzrxi:	@ exit for int
	postv	sv5
	it	ne
	ngintne	sv1, sv1
	set	pc,  cnt
	
_func_
rtzflt:	@ rationalize for flt
	isnan	sv1
	it	eq
	seteq	pc,  cnt
	@ rationalize with x and y as flt
	set	sv4, sv1	@ sv4 <- x
	fabs	sv1, sv2	@ sv1 <- |y|
	adr	sv5, rtyspc
	bl	spcflt
	set	sv1, sv4	@ sv1 <- x
	adr	sv5, rtxspc
	bl	spcflt
	@ exit with 0.0 if range is larger than val (i.e. spans 0)
	fabs	sv1, sv2	@ sv1 <- |y|
	fabs	sv3, sv4	@ sv3 <- |x|
	cmp	sv1, sv3
	bpl	f0fxt
	@ find top and bottom of interval (and sort, in absolute value, in case top/bottom are unsorted)
	set	sv2, sv4	@ sv2 <- x
	set	sv3, sv1	@ sv3 <- |y|
	bl	plsflt		@ sv1 <- top-signed = x + |y|
	set	sv2, sv3	@ sv2 <- |y|
	set	sv3, sv1	@ sv3 <- top-signed
	set	sv1, sv4	@ sv1 <- x
	bl	mnsflt		@ sv1 <- bottom-signed = x - |y|
	sav_rc	sv1		@ dts <- (bottom-signed cnt ...)
	fabs	sv1, sv1
	fabs	sv2, sv3
	set	sv4, #null
	cmp	sv1, sv2
	bmi	rtzflp
	swap	sv1, sv2, rva
rtzflp:	@ loop
	save3	sv1, sv2, sv4	@ dts <- (bottom top coeffs top-signed cnt ...)
	call	dnmflt
	set	sv3, sv1	@ sv3 <- denom-bottom	
	restor3	sv1, sv2, sv4	@ sv1 <- bottom, sv2 <- top, sv4 <- coeffs, dts <- (top-signed cnt ...)
	set	sv5, sv1	@ sv5 <- bottom
	ldr	rvc, =scheme_one
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
	ldr	sv2, =scheme_one
	set	sv1, sv3	@ sv1 <- truncated bottom
	bl	i12flt
	bl	plsflt
rtzfx1:	@ sv1 <- bottom or 1+truncated bottom
	@ sv4 <- coeffs list
	nullp	sv4
	beq	rtzfxt
	set	sv2, sv1
	ldr	sv1, =scheme_one
	bl	divflt
	snoc	sv2, sv4, sv4
	adr	lnk, rtzfx1
	b	unipls
	
rtzfxt:	@ exit
	restor2	sv5, cnt
	postv	sv5
	it	ne
	ngfltne	sv1, sv1
	set	pc,  cnt

_func_
rtzrat:	@ rationalize for rat
	@ rationalize with x and y as rat
	@ return nan if sv1 == 0/0
	snoc	rva, rvb, sv1
	eq	rva, #3
	it	eq
	eqeq	rvb, #0
	it	eq
	seteq	pc,  cnt
	@ return nan if sv2 == 0/0
	snoc	rva, rvb, sv2
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
	beq	i0fxt
	@ return 0 if sv2 == -1/0
	mvn	rvc, rva
	eq	rvc, #0x0c
	it	eq
	eqeq	rvb, #3
	beq	i0fxt
	@ return sv1 if sv2 == 0/1
	eq	rva, #3
	it	eq
	eqeq	rvb, #4
	it	eq
	seteq	pc,  cnt
	@ return sv1 if sv1 == +/- 1/0
	cdr	rvb, sv1
	bic	rvb, rvb, #3
	eq	rvb, #0
	it	eq
	seteq	pc,  cnt	
	@ check if |x| <= |y| if so, return 0
	numerat	sv5, sv1
	sav_rc	sv5		@ dts <- (x-numerat-signed cnt ...)
	set	sv4, sv1	@ sv4 <- x
	set	sv1, sv2	@ sv1 <- y
	call	absrat		@ sv1 <- |y|
	set	sv5, sv1	@ sv5 <- |y|
	set	sv1, sv4	@ sv1 <- x
	call	absrat		@ sv1 <- |x|
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
	restor2	sv5, cnt
	b	i0fxt
	
rtlzr0:	@
	numerat	sv3, sv1
	numerat	sv4, sv2
	eor	rva, sv3, sv4
	postv	rva
	bne	rtzaxt
rtlzr1:		
	set	sv4, #null
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
	restor	sv1		@ sv1 <- bottom-denominator, dts <- (top-denominator x-signed cnt ...)
	bl	makrat		@ sv1 <- bottom-denominator/bottom-remainder
	set	sv2, sv5	@ sv2 <- top-remainder
	set	sv5, sv1	@ sv5 <- bottom-denominator/bottom-remainder
	restor	sv1		@ sv1 <- top-denominator, dts <- (x-signed cnt ...)
	bl	makrat		@ sv1 <- top-denominator / top-remainder
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
	set	sv1, #5
	bl	unidiv
	snoc	sv2, sv4, sv4
	adr	lnk, rtzrx1
	b	unipls
	
rtzrxt:	@ exit
	restor2	sv5, cnt
	intgrp	sv1
	beq	rtzrxi
	spltrat	sv1, sv2, sv1
	postv	sv5
	it	ne
	ngintne	sv1, sv1
	set	lnk, cnt
	b	makrat

.endif

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.1.	booleans:		not, boolean?
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			boolxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.balign	4
	
not:	SYMSIZE	3
	.ascii	"not"
	.balign 4

pnot:	@ (not obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t if obj = #f, else #f
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	inot, otypchk, 1	@  primitive, init-sv4 = #f, fentry = typchk, narg = 1

.balign	4
	
booln:	SYMSIZE	8
	.ascii	"boolean?"
	.balign 4

pbooln:	@ (boolean? obj)
	@ on entry:	sv1 <-obj
	@ on exit:	sv1 <- #t if obj = #t or #f, else #f
	PFUNC	1			@ primitive function, one input arg
	eq	sv1, #t
	it	ne
	eqne	sv1, #f
	b	boolxt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.2.	Pairs and list:		caar, cadr, ..., cdddar, cddddr,
@					null?, list?, list, length, append, reverse,
@					list-tail, list-ref, memq, memv, member,
@					assq, assv, assoc
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.balign	4

cxxxxr:	@ (cxxxxr obj)
	@ on entry:	sv1 <- list
	@ on entry:	sv4 <- car/cdr code (scheme int)
	@ on entry:	sv5 <- symbol of function after the one that called cxxxxr (for error4)
	@
	@ code in sv4 is 8-bits, lsb 0 means car, lsb 1 means cdr,
	@ code is shifted right at each step and process ends when code = 1.
	@
	lsr	rvb, sv4, #2		@ rvb <- car/cdr code (raw)
jxxxxr:	@ (cxxxxr list)
	@ uses code in rvb to determine car/cdr
	eq	rvb, #1			@ done with car/cdr code?
	it	eq
	seteq	pc,  cnt		@	if so,  exit
	qpair0	sv1			@ is it an atom, var/synt, rat/cpx or sized item?
	beq	cxxerr
	tst	rvb, #1			@ is bit 0 of rva a one?
	itE	eq
	careq	sv1, sv1		@	if not, sv1 <- (car list)
	cdrne	sv1, sv1		@	if so,  sv1 <- (cdr list)
	lsr	rvb, rvb, #1		@ rva <- shifted car/cdr code
	b	jxxxxr			@ jump to continue
cxxerr:	@ error
	tst	sv4, #0x60
	itE	ne
	subne	sv4, sv5, #16
	subeq	sv4, sv5, #12
	b	error4
	
.balign	4
	
caar:	SYMSIZE	4
	.ascii	"caar"
	.balign 4

pcaar:	EPFUNC	(0x04<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cadr:	SYMSIZE	4
	.ascii	"cadr"
	.balign 4

pcadr:	EPFUNC	(0x05<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cdar:	SYMSIZE	4
	.ascii	"cdar"
	.balign 4

pcdar:	EPFUNC	(0x06<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cddr:	SYMSIZE	4
	.ascii	"cddr"
	.balign 4

pcddr:	EPFUNC	(0x07<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
caaar:	SYMSIZE	5
	.ascii	"caaar"
	.balign 4

pcaaar:	EPFUNC	(0x08<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
caadr:	SYMSIZE	5
	.ascii	"caadr"
	.balign 4

pcaadr:	EPFUNC	(0x09<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cadar:	SYMSIZE	5
	.ascii	"cadar"
	.balign 4

pcadar:	EPFUNC	(0x0A<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
caddr:	SYMSIZE	5
	.ascii	"caddr"
	.balign 4

pcaddr:	EPFUNC	(0x0B<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cdaar:	SYMSIZE	5
	.ascii	"cdaar"
	.balign 4

pcdaar:	EPFUNC	(0x0C<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cdadr:	SYMSIZE	5
	.ascii	"cdadr"
	.balign 4

pcdadr:	EPFUNC	(0x0D<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cddar:	SYMSIZE	5
	.ascii	"cddar"
	.balign 4

pcddar:	EPFUNC	(0x0E<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cdddr:	SYMSIZE	5
	.ascii	"cdddr"
	.balign 4

pcdddr:	EPFUNC	(0x0F<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

caaaar:	SYMSIZE	6
	.ascii	"caaaar"
	.balign 4

paaaar:	EPFUNC	(0x10<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
caaadr:	SYMSIZE	6
	.ascii	"caaadr"
	.balign 4

paaadr:	EPFUNC	(0x11<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

caadar:	SYMSIZE	6
	.ascii	"caadar"
	.balign 4

paadar:	EPFUNC	(0x12<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

caaddr:	SYMSIZE	6
	.ascii	"caaddr"
	.balign 4

paaddr:	EPFUNC	(0x13<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cadaar:	SYMSIZE	6
	.ascii	"cadaar"
	.balign 4

padaar:	EPFUNC	(0x14<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cadadr:	SYMSIZE	6
	.ascii	"cadadr"
	.balign 4

padadr:	EPFUNC	(0x15<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

caddar:	SYMSIZE	6
	.ascii	"caddar"
	.balign 4

paddar:	EPFUNC	(0x16<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cadddr:	SYMSIZE	6
	.ascii	"cadddr"
	.balign 4

padddr:	EPFUNC	(0x17<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cdaaar:	SYMSIZE	6
	.ascii	"cdaaar"
	.balign 4

pdaaar:	EPFUNC	(0x18<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cdaadr:	SYMSIZE	6
	.ascii	"cdaadr"
	.balign 4

pdaadr:	EPFUNC	(0x19<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cdadar:	SYMSIZE	6
	.ascii	"cdadar"
	.balign 4

pdadar:	EPFUNC	(0x1A<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1

cdaddr:	SYMSIZE	6
	.ascii	"cdaddr"
	.balign 4

pdaddr:	EPFUNC	(0x1B<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cddaar:	SYMSIZE	6
	.ascii	"cddaar"
	.balign 4

pddaar:	EPFUNC	(0x1C<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cddadr:	SYMSIZE	6
	.ascii	"cddadr"
	.balign 4

pddadr:	EPFUNC	(0x1D<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cdddar:	SYMSIZE	6
	.ascii	"cdddar"
	.balign 4

pdddar:	EPFUNC	(0x1E<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1
	
cddddr:	SYMSIZE	6
	.ascii	"cddddr"
	.balign 4

pddddr:	EPFUNC	(0x1F<<2)|i0, ocxxxxr, 1	@  prim., sv4 = code, fentry = cxxxxr, narg = 1


.balign	4
	
snull:	SYMSIZE	5
	.ascii	"null?"
	.balign 4

pnull:	@ (null? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	inul, otypchk, 1	@ primitive, init-sv4 = '(), fentry = typchk, narg = 1

.balign	4
	
listp:	SYMSIZE	5
	.ascii	"list?"
	.balign 4

plistp:	@ (list? obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- #t/#f
	PFUNC	1			@ primitive function, one input arg
	ldr	rvc, =heapbottom	@ rvc <- heap bottom address
	ldr	rva, =heaptop1		@ rva <- heap top address
	sub	rvc, rva, rvc		@ rvc <- max number of bytes to search
	lsr	rvc, rvc, #3		@ rvc <- max number of cons cells to search
	set	sv3, sv1		@ sv3 <- start of list -- for cyclic list
qlist0:	nullp	sv1			@ is list '()?
	beq	boolxt			@	if so,  return #t
	bl	ckpair			@ raise eq flag on atom, sized item, rat/cpx, syntax, assembld code
	beq	notfxt			@	if so,  return #f
	cdr	sv1, sv1		@ sv1 <- (cdr obj)
	subs	rvc, rvc, #1		@ rvc <- remaining max number of cons cells to search, is it zero?
	beq	flsfxt			@	if so,  return #f
	b	qlist0			@ jump to keep going through potential list

.balign	4
	
list:	SYMSIZE	4
	.ascii	"list"
	.balign 4

plist:	@ (list item1 item2 ...)
	@ on entry:	sv1 <- (item1 item2 ...)
	@ on exit:	sv1 <- (item1 item2 ...)
	@ pre-entry:	return returns directly via cnt
	EPFUNC	0, oreturn, 0		@  primitive, init-sv4 = none, fentry = return, narg = 0

.balign	4
	
slngth:	SYMSIZE	6
	.ascii	"length"
	.balign 4

plngth:	@ (length list)
	@ on entry:	sv1 <- list
	@ on exit:	sv1 <- length of list (scheme int)
	PFUNC	1			@ primitive function, one input arg
length:	@ [internal entry]
	ldr	rvc, =heapbottom	@ rvc <- heap bottom address
	ldr	rva, =heaptop1		@ rva <- heap top address
	sub	rvc, rva, rvc		@ rvc <- max number of bytes to search
	lsr	rvc, rvc, #3		@ rvc <- max number of cons cells to search
	set	sv2, #i0		@ sv2 <- initial list length = 0 (scheme int)
lengt0:	nullp	sv1			@ at end of list?
	itT	eq
	seteq	sv1, sv2		@	if so,  sv1 <- length
	seteq	pc,  cnt		@	if so,  return with length
	bl	ckpair			@ raise eq flag on atom, sized item, rat/cpx, syntax, assembld code
	beq	flsfxt			@	if so,  return #f
	cdr	sv1, sv1		@ sv1 <- (cdr obj)
	incr	sv2, sv2		@ sv2 <- updated list length
	subs	rvc, rvc, #1		@ rvc <- remaining max number of cons cells to search, is it zero?
	beq	flsfxt			@	if so,  return #f
	b	lengt0			@ jump back to keep going

.balign	4
	
append:	SYMSIZE	6
	.ascii	"append"
	.balign 4

pappnd:	@ (append list1 list2 ...)
	@ on entry:	sv1 <- (list1 list2 ...)
	@ on exit:	sv1 <- appended lists
	PFUNC	0			@ primitive function, listed input args
	set	sv4, sv1		@ sv4 <- (list1 list2 ...)
	set	sv1, #null		@ sv1 <- '()
	list	sv5, sv1		@ sv5 <- (null . null) = (() tail-ref)
	save	sv5			@ dts <- ((() tail-ref) ...)
appen0:	nullp	sv4			@ done?
	beq	appext			@	if so,  jump to exit
	snoc	sv3, sv4, sv4		@ sv3 <- list1,  sv4 <- (list2 ...)
	nullp	sv4			@ is rest-of lists null?
	it	eq
	setcdreq sv5, sv3		@	if so,  store last list as cdr of appended result
	beq	appext			@	if so,  jump to exit
appen2:	nullp	sv3			@ is list1 null?
	beq	appen0			@	if so,  branch to process next list
	snoc	sv1, sv3, sv3		@ sv1 <- car1 == car of list1,  sv3 <- rest of list1
	list	sv1, sv1		@ sv1 <- (car1)
	setcdr	sv5, sv1		@ set (car1) as cdr of result
	set	sv5, sv1		@ sv5 <- result's tail
	b	appen2			@ jump to continue appending
appext:	restor	sv1			@ sv1 <- (() . result),		dts <- (...)
	cdr	sv1, sv1		@ sv1 <- result
	set	pc,  cnt

.balign	4
	
srevrs:	SYMSIZE	7
	.ascii	"reverse"
	.balign 4

prevrs:	@ (reverse list)
	@ on entry:	sv1 <- list
	@ on exit:	sv1 <- reversed list
	PFUNC	1			@ primitive function, one input arg
revers:	@ [internal entry]
	set	sv2, sv1		@ sv2 <- list
	set	sv1, #null		@ sv1 <- '() = initial reversed list
	bl	typsv2			@ rvb <- type tag of sv2
_func_
rvrls0:	eq	rvb, #list_tag		@ item is a list?
	it	ne
	setne	pc,  cnt		@	if not, return reversed list in sv1
	set	sv3, sv1		@ sv2 <- current reversed list
	snoc	sv1, sv2, sv2		@ sv1 <- car of rest of list,		sv4  <- cdr of rest of list
	cons	sv1, sv1, sv3		@ sv1 <- (car . reversed list) = updated reversed list
	adr	lnk, rvrls0		@ lnk <- return address for typsv2 below
	b	typsv2			@ rvb <- type tag of rest of list (return via lnk to rvrls0)

.balign	4
	
lstail:	SYMSIZE	9
	.ascii	"list-tail"
	.balign 4

plstal:	@ (list-tail list k)
	@ on entry:	sv1 <- list
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- tail of list or ()
	PFUNC	2			@ function type, two input args
	set	sv3, #f
	b	lstre0

.balign	4
	
lstref:	SYMSIZE	8
	.ascii	"list-ref"
	.balign 4

plstrf:	@ (list-ref list k)
	@ on entry:	sv1 <- list
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- k-th item from list or ()
	PFUNC	2			@ function type, two input args
	set	sv3, #t
lstre0:	nullp	sv1			@ is list empty?
	it	eq
	seteq	pc,  cnt		@	if so,  exit with null as result
	izerop	sv2			@ is k zero?
	itT	ne
	cdrne	sv1, sv1		@ sv1 <- (cdr list)
	decrne	sv2, sv2		@ sv2 <- k - 1 (scheme int)
	bne	lstre0			@ jump to continue looking for ref
	eq	sv3, #t
	it	eq
	careq	sv1, sv1
	set	pc,  cnt

.balign	4
	
memq:	SYMSIZE	4
	.ascii	"memq"
	.balign 4

smemv:	SYMSIZE	4
	.ascii	"memv"
	.balign 4

pmemv:	@ (memv obj list)
	@ on entry:	sv1 <- obj
	@ on entry:	sv2 <- list
	@ on exit:	sv1 <- sub-list or #f
	@ modifies:	sv1, sv2, sv3
	PFUNC	2			@ function type, two input args
memv:	@ [internal entry]
	swap	sv1, sv2, sv3
memv0:	nullp	sv1			@ is list null?
	beq	notfxt
	car	sv3, sv1		@ sv3 <- (car list)
	eq	sv2, sv3		@ is car list = obj?
	it	eq
	seteq	pc,  cnt
	cdr	sv1, sv1		@ sv1 <- (cdr list)
	b	memv0			@ jump to continue looking for obj

.balign	4
	
member:	SYMSIZE	6
	.ascii	"member"
	.balign 4

pmembr:	@ (member obj list)
	@ on entry:	sv1 <- obj
	@ on entry:	sv2 <- list
	@ on exit:	sv1 <- sub-list or #f
	PFUNC	2			@ function type, two input args
	sav_rc	sv1			@ dts <- (obj cnt ...)
membe0:	nullp	sv2			@ is list null?
	it	eq
	seteq	sv1, #f			@	if so,  sv1 <- #f
	beq	membxt			@	if so,  jump to exit with #f
	car	sv1, dts		@ sv1 <- obj
	save	sv2			@ dts <- (list obj cnt ...)
	car	sv2, sv2		@ sv2 <- arg = (car list)
	call	equal			@ sv1 <- #t/#f, from (equal sv1 sv2)
	restor	sv2			@ sv2 <- list,		dts <- (obj cnt ...)
	eq	sv1, #t			@ was object found?
	it	eq
	seteq	sv1, sv2
	beq	membxt			@	if so,  jump to exit with rest of list
	cdr	sv2, sv2		@ sv1 <- (cdr list) = restlist
	b	membe0			@ jump to continue looking for obj
membxt:	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,		dts <- (...)
	set	pc,  cnt

.balign	4
	
sassq:	SYMSIZE	4
	.ascii	"assq"
	.balign 4

sassv:	SYMSIZE	4
	.ascii	"assv"
	.balign 4

passq:	@ (assq obj alist)
	@ on entry:	sv1 <- obj
	@ on entry:	sv2 <- alist
	@ on exit:	sv1 <- binding-or-#f
	@ on exit:	sv2 <- null-or-rest-of-alist
	@ on exit:	sv3 <- obj
	@ preserves:	sv4-sv5
	PFUNC	2			@ function type, two input args
	set	sv3, sv1		@ sv3 <- obj = key
assq0:	@ loop
	nullp	sv2			@ is binding-list null?
	beq	notfxt			@	if so,  exit with #f
	snoc	sv1, sv2, sv2		@ sv1 <- 1st binding,		sv2 <- rest of binding-list
	car	sv4, sv1		@ sv4 <- bkey of 1st binding (bkey . bval) in binding-list
	eq	sv3, sv4		@ is bkey = key ?
	bne	assq0			@	if not, jump to keep searching
	set	pc,  cnt		@ return with binding in sv1

.balign	4

assoc:	SYMSIZE	5
	.ascii	"assoc"
	.balign 4

passoc:	@ (assoc key binding-list)
	@ on entry:	sv1 <- key
	@ on entry:	sv2 <- binding-list
	@ on exit:	sv1 <- binding-or-#f
	PFUNC	2			@ function type, two input args
	sav_rc	sv1			@ dts <- (key cnt ...)
assoc0:	@ sv1 <- binding-list, dts <- (key ...)
	nullp	sv2			@ is binding-list null?
	it	eq
	seteq	sv1, #f			@	if so,  sv1 <- #f
	beq	assoxt			@	if so,  jump to exit with #f
	car	sv1, dts		@ sv1 <- key
	save	sv2			@ dts <- (binding-list key cnt ...)
	caar	sv2, sv2		@ sv2 <- key
	call	equal			@ sv1 <- #t/#f, from (equal sv1 sv2)
	restor	sv2			@ sv2 <- binding-list,	dts <- (key cnt ...)
	eq	sv1, #t			@ was a binding found?
	it	ne
	cdrne	sv2, sv2		@	if not, sv2 <- rest-of-binding-list
	bne	assoc0			@	if not, jump to continue searching
	car	sv1, sv2		@ sv1 <- winning binding
assoxt:	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,		dts <- (...)
	set	pc,  cnt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.4.	Characters:		char-ci=?, char-ci<?, char-ci>?, char-ci<=?,
@					char-ci>=?, char-alphabetic?, char-numeric?,
@					char-whitespace?, char-upper-case?, char-lower-case?,
@					char-upcase, char-downcase
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.balign	4

chrceq:	SYMSIZE	9
	.ascii	"char-ci=?"
	.balign 4

pchceq:	@ (char-ci=? char1 char2):	
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2
ichceq:	@ [internal entry]
	ldr	lnk, =ichreq
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char=?

.balign	4

chrclt:	SYMSIZE	9
	.ascii	"char-ci<?"
	.balign 4

pchclt:	@ (char-ci<? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2
ichclt:	@ [internal entry]
	ldr	lnk, =ichrlt
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char<?

.balign	4
	
chrcgt:	SYMSIZE	9
	.ascii	"char-ci>?"
	.balign 4

pchcgt:	@ (char-ci>? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2
ichcgt:	@ [internal entry]
	ldr	lnk, =ichrgt
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char>?
	
.balign	4
	
chrcle:	SYMSIZE	10
	.ascii	"char-ci<=?"
	.balign 4

pchcle:	@ (char-ci<=? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2
ichcle:	@ [internal entry]
	ldr	lnk, =ichrle
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char<=?

.balign	4
	
chrcge:	SYMSIZE	10
	.ascii	"char-ci>=?"
	.balign 4

pchcge:	@ (char-ci>=? char1 char2)
	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2
ichcge:	@ [internal entry]
	ldr	lnk, =ichrge
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char>=?

.balign	4
	
chralp:	SYMSIZE	16
	.ascii	"char-alphabetic?"
	.balign 4

pchalp:	@ (char-alphabetic? char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- #t/#f
	PFUNC	1
	bl	tolocs			@ rvc <- #t/#f based on whether sv1 is upper case
	eq	rvc, #t			@ is char upper case?
	it	ne
	blne	toupcs			@	if not,  rvc <- #t/#f based on whether sv1 is lower case
	set	sv1, rvc		@ sv1 <- #t/#f result (#t if upper case or lower case)
	set	pc,  cnt		@ return

.balign	4
	
chrnum:	SYMSIZE	13
	.ascii	"char-numeric?"
	.balign 4

pchnum:	@ (char-numeric? char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- #t/#f
	PFUNC	1
	set	rvb, #'9		@ rvb <- ascii char 9
	chr2raw	rva, sv1		@ rva <- raw char
	cmp	rva, #'0		@ is char >= ascii char 0?
	it	pl
	cmppl	rvb, rva		@	if so,  is char <= ascii char 9?
	itE	pl
	setpl	sv1, #t			@		if so,  sv1 <- #t
	setmi	sv1, #f			@		if not, sv1 <- #f
	set	pc,  cnt		@ return with #t/#f

.balign	4
	
chrspa:	SYMSIZE	16
	.ascii	"char-whitespace?"
	.balign 4
	
pchspa:	@ (char-whitespace? char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- #t/#f
	PFUNC	1
	chr2raw	rvb, sv1
	eq	rvb, #'\r		@ is char a carriage return?
	it	ne
	eqne	rvb, #'  		@	if not, is char a space?
	it	ne
	eqne	rvb, #'			@	if not, is char a tab?
	it	ne
	eqne	rvb, #'\n		@	if not, is char a lf?
	b	boolxt			@ return with #t/#f

.balign	4
	
chrupq:	SYMSIZE	16
	.ascii	"char-upper-case?"
	.balign 4

pchupq:	@ (char-upper-case? char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- #t/#f
	PFUNC	1
	bl	tolocs			@ rvc <- #t/#f based on whether sv1 is upper case
	set	sv1, rvc		@ sv1 <- result
	set	pc,  cnt		@ return

.balign	4
	
chrloq:	SYMSIZE	16
	.ascii	"char-lower-case?"
	.balign 4

pchloq:	@ (char-lower-case? char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- #t/#f
	PFUNC	1
	bl	toupcs			@ rvc <- #t/#f based on whether sv1 is lower case
	set	sv1, rvc		@ sv1 <- result
	set	pc,  cnt		@ return

.balign	4
	
chrupc:	SYMSIZE	11
	.ascii	"char-upcase"
	.balign 4

pchupc:	@ (char-upcase char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- char in upper case
	PFUNC	1
	set	lnk, cnt
	b	toupcs

.balign	4
	
chrdnc:	SYMSIZE	13
	.ascii	"char-downcase"
	.balign 4

pchdnc:	@ (char-downcase char)
	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- char in lower case
	PFUNC	1
	set	lnk, cnt
	b	tolocs
	
.balign	4
	
toupcs:	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- upper case version of char
	@ on exit:	rvc <- #t/#f based on whether char was lower case or not
	set	rvb, #'z
	chr2raw	rva, sv1
	cmp	rva, #'a
	it	pl
	cmppl	rvb, rva
	itTE	pl
	bicpl	sv1, sv1, #0x2000	@	if so,  sv1 <- upper case version of char
	setpl	rvc, #t
	setmi	rvc, #f
	set	pc,  lnk

.balign	4
	
_func_
toloc2:	@ on entry:	sv1 <- char1
	@ on entry:	sv2 <- char2
	@ on exit:	sv1 <- lower case version of char1
	@ on exit:	sv2 <- lower case version of char2
	@ on exit:	rvc <- #t/#f based on whether char1 was upper case or not
	set	rvb, #'Z
	chr2raw	rva, sv2
	cmp	rva, #'A
	it	pl
	cmppl	rvb, rva
	it	pl
	orrpl	sv2, sv2, #0x2000
_func_
tolocs:	@ on entry:	sv1 <- char
	@ on exit:	sv1 <- lower case version of char
	@ on exit:	rvc <- #t/#f based on whether char was upper case or not
	set	rvb, #'Z
	chr2raw	rva, sv1
	cmp	rva, #'A
	it	pl
	cmppl	rvb, rva
	itTE	pl
	orrpl	sv1, sv1, #0x2000
	setpl	rvc, #t
	setmi	rvc, #f
	set	pc,  lnk

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.5.	Strings:		string, string=?,
@					string-ci=?, string<?, string>?, string<=?
@					string>=?, string-ci<?, string-ci>?,
@					string-ci<=?, string-ci>=?,
@					substring, string-append,
@					string->list, list->string,string-copy, string-fill
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.balign	4
	
spstng:	SYMSIZE	6
	.ascii	"string"
	.balign 4

pstrng:	@ (string char1 char2 ...)
	@ on entry:	sv1 <- (char1 char2 ...)
	@ on exit:	sv1 <- string
	PFUNC	0			@ primitive function, listed input args
string:	@ [internal entry]
	set	sv5, sv1		@ sv5 <- list (saved)
	set	sv2, #i0		@ sv2 <- initial list length = 0 (scheme int)
strin0:	nullp	sv1
	itT	ne
	cdrne	sv1, sv1
	incrne	sv2, sv2
	bne	strin0
	straloc	sv1, sv2		@ sv1 <- allocated string of size sv2
	set	sv3, #i0		@ sv3 <- offset to start address of items (scheme int)
strin1:	cmp	sv3, sv2		@ done copying chars?
	it	pl
	setpl	pc,  cnt		@	if so,  return string
	snoc	sv4, sv5, sv5		@ sv4 <- 1st char (scheme char),  sv5 <- remaining chars list
	strset	sv4, sv1, sv3
	incr	sv3, sv3
	b	strin1

.balign	4
	
sstequ:	SYMSIZE	8
	.ascii	"string=?"
	.balign 4

strequ:	@ (string=? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichreq		@ sv5 <- address of comparison routine
streq0:	@ [ internal entry]
	strlen	sv3, sv1		@ sv3 <- length of string1
	strlen	sv4, sv2		@ sv4 <- length of string2
	eq	sv3, sv4		@ are string lengths equal?
	bne	boolxt			@	if not, return with #f
strcmp:	@ [internal entry] string comparison based on function in sv5
	save3	sv1, sv2, cnt		@ dts <- (string1 string2 cnt ...)
	strlen	sv3, sv1		@ sv3 <- length of string1
	strlen	sv4, sv2		@ sv4 <- length of string2
	cmp	sv4, sv3		@ is string2 shorter than string1?
	it	mi
	setmi	sv3, sv4		@	if so,  sv3 <- shortest string length
	save	sv3			@ dts <- (length string1 string2 cnt ...)
	set	sv1, #t			@ sv1 <- #t = initial result
	set	sv4, #i0		@ sv4 <- 0, start char offset (scheme int)
	ldr	cnt, =strcrt		@ cnt <- comparison return address
strclp:	@ loop to compare chars
	snoc	sv2, sv3, dts		@ sv2 <- count, sv3 <- (string1 string2 cnt ...)
	eq	sv2, sv4		@ done comparing?
	beq	strcxt			@	if so,  jump to exit
	snoc	sv1, sv2, sv3		@ sv1 <- string1, sv2 <- (string2 cnt ...)
	car	sv2, sv2		@ sv2 <- string2
	strref	sv1, sv1, sv4		@ sv1 <- char1, from string1
	strref	sv2, sv2, sv4		@ sv2 <- char2, from string2
	set	pc,  sv5		@ sv1 <- #t/#f from jump to comparison routine
strcrt:	eq	sv1, #f			@ did test fail?
	beq	strcxt			@	if so,  jump to exit
	add	sv4, sv4, #4		@ sv4 <- offset f next char
	b	strclp			@ jump to keep comparing chars
strcxt:	@ exit
	restor	sv2			@ sv2 <- length, dts <- (string1 string2 cnt ...)
	restor3	sv2, sv3, cnt		@ sv2 <- string1, sv3 <- string2, cnt <- cnt, dts <-(...)
	set	pc,  cnt		@ return

.balign	4
	
sstceq:	SYMSIZE	11
	.ascii	"string-ci=?"
	.balign 4

strceq:	@ (string-ci=? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichceq		@ sv5 <- address of comparison routine
	b	streq0

.balign	4
	
sstlt:	SYMSIZE	8
	.ascii	"string<?"
	.balign 4

strlt:	@ (string<? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichrlt		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstgt:	SYMSIZE	8
	.ascii	"string>?"
	.balign 4

strgt:	@ (string>? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichrgt		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstle:	SYMSIZE	9
	.ascii	"string<=?"
	.balign 4

strle:	@ (string<=? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichrle		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstge:	SYMSIZE	9
	.ascii	"string>=?"
	.balign 4

strge:	@ (string>=? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichrge		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstclt:	SYMSIZE	11
	.ascii	"string-ci<?"
	.balign 4

strclt:	@ (string-ci<? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichclt		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstcgt:	SYMSIZE	11
	.ascii	"string-ci>?"
	.balign 4

strcgt:	@ (string-ci>? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichcgt		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstcle:	SYMSIZE	12
	.ascii	"string-ci<=?"
	.balign 4

strcle:	@ (string-ci<=? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichcle		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
sstcge:	SYMSIZE	12
	.ascii	"string-ci>=?"
	.balign 4

strcge:	@ (string-ci>=? string1 string2)
	@ on entry:	sv1 <- string1
	@ on entry:	sv2 <- string2
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	ldr	sv5, =ichcge		@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

.balign	4
	
ssubst:	SYMSIZE	9
	.ascii	"substring"
	.balign 4

substr:	@ (substring string start end)
	@ on entry:	sv1 <- list
	@ on entry:	sv2 <- start
	@ on entry:	sv3 <- end
	@ on exit:	sv1 <- string
	PFUNC	3			@ primitive function, three input args
sbstrg:	@ [internal entry]
	add	sv2, sv2, #16
	add	sv3, sv3, #16
	set	lnk, cnt
	b	subcpy

.balign	4
	
sstapp:	SYMSIZE	13
	.ascii	"string-append"
	.balign 4

strapp:	@ (string-append st1 st2 ...)
	@ on entry:	sv1 <- (st1 st2 ...)
	@ on exit:	sv1 <- string
	PFUNC	0			@ primitive function, listed input args
	set	sv5, sv1		@ sv5 <- (st1 st2 ...)
	set	sv4, sv1		@ sv4 <- (st1 st2 ...)
	@ count total number of chars in strings to be appended
	set	sv2, #i0		@ sv2 <- initial size
strap0:	nullp	sv4			@ done counting chars?
	beq	strap1			@	if so,  jump to allocate new string
	snoc	sv1, sv4, sv4		@ sv1 <- st1,  sv4 <- (st2 ...)
	strlen	sv3, sv1		@ sv3 <- number of characters in st1
	plus	sv2, sv2, sv3		@ sv2 <- updated character count
	b	strap0			@ jump to keep counting chars
strap1:	@ allocate memory for new string
	straloc	sv1, sv2		@ sv1 <- newly-allocated target string
	@ append strings into new string
	set	sv4, #i0		@ sv4 <- offset to start address of items (scheme int)
strap2:	nullp	sv5			@ done with all strings?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	snoc	sv3, sv5, sv5		@ sv3 <- source string,  sv5 <- rest-of-source-string-list
	strlen	sv2, sv3		@ sv2 <- number of characters in source string
	set	rvb, #i0		@ rvb <- offset to start address in source
strap3:	eq	rvb, sv2		@ done with this string?
	beq	strap2			@	if so,  jump to process next string
	strref	rva, sv3, rvb		@ rva <- source raw ASCII char
	strset	rva, sv1, sv4		@ store it in target string
	incr	rvb, rvb		@ rvb <- updated offset to source char
	incr	sv4, sv4		@ sv4 <- updated offset to destination char
	b	strap3			@ jump to keep copying

.balign	4
	
sstlst:	SYMSIZE	12
	.ascii	"string->list"
	.balign 4

strlst:	@ (string->list string)
	@ on entry:	sv1 <- string
	@ on exit:	sv1 <- list
	PFUNC	1			@ primitive function, one input arg
	set	sv3, sv1		@ sv3 <- string
	strlen	sv4, sv3		@ sv4 <- number of characters in string
	set	sv1, #null		@ sv1 <- '()
strls0:	izerop	sv4			@ done making list?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	set	sv2, sv1		@ sv2 <- previous list of characters (for cons)
	decr	sv4, sv4		@ sv4 <- offset of next character
	strref	sv1, sv3, sv4		@ sv1 <- next character
	cons	sv1, sv1, sv2		@ sv1 <- (char ...) = updated list of characters
	b	strls0			@ jump to process rest of string

.balign	4
	
slstst:	SYMSIZE	12
	.ascii	"list->string"
	.balign 4

lststr:	@ (list->string list)
	@ on entry:	sv1 <- list
	@ on exit:	sv1 <- string
	PFUNC	1			@ primitive function, one input arg
	b	string

.balign	4
	
sstcpy:	SYMSIZE	11
	.ascii	"string-copy"
	.balign 4

strcpy:	@ (string-copy string)
	@ on entry:	sv1 <- string
	@ on exit:	sv1 <- string
	PFUNC	1			@ primitive function, one input arg
	set	sv2, #i0		@ sv2 <- position of 1st char (scheme int)
	strlen	sv3, sv1		@ sv3 <- position after last char (scheme int)
	b	sbstrg

.balign	4
	
sstfil:	SYMSIZE	12
	.ascii	"string-fill!"
	.balign 4

strfil:	@ (string-fill! string char)
	@ on entry:	sv1 <- string
	@ on entry:	sv2 <- char
	@ on exit:	sv1 <- string
	PFUNC	2			@ primitive function, two input args
	strlen	sv3, sv1		@ sv3 <- string length (scheme int)
	chr2raw	rvb, sv2		@ rvb <- ascii char
	b	fill8			@ perform fill and return

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.6.	Vectors:		vector, vector->list, list->vector, vector-fill
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.balign	4
	
svctor:	SYMSIZE	6
	.ascii	"vector"
	.balign 4

vector:	@ (vector item1 item2 ...)
	@ on entry:	sv1 <- (item1 item2 ...)
	@ on exit:	sv1 <- vector == #(item1 item2 ...)
	PFUNC	0			@ primitive function, listed input args
	b	lstvec

.balign	4
	
svclst:	SYMSIZE	12
	.ascii	"vector->list"
	.balign 4

pvclst:	@ (vector->list vector)
	@ on entry:	sv1 <- vector
	@ on exit:	sv1 <- list
	@ preserves:	sv2 (for wrtvec:)
	PFUNC	1			@ primitive function, one input arg
	set	sv4, sv1		@ sv4 <- vector
	veclen	sv5, sv4		@ sv5 <- number of items, (scheme int)
	set	sv1, #null		@ sv1 <- '() = initial result list
vecls0:	izerop	sv5			@ no more vector items?
	it	eq
	seteq	pc,  cnt		@	if so,  exit
	set	sv3, sv1		@ sv3 <- current result list
	decr	sv5, sv5		@ sv5 <- position of next item from vector
	vecref	sv1, sv4, sv5		@ sv1 <- item from vector
	cons	sv1, sv1, sv3		@ sv1 <- updated result list
	b	vecls0			@ jump to continue
	
.balign	4
	
slsvec:	SYMSIZE	12
	.ascii	"list->vector"
	.balign 4

plsvec:	@ (list->vector list) --- used by parsqt
	@ on entry:	sv1 <- list   ==  (item1 item2 ...)
	@ on exit:	sv1 <- vector == #(item1 item2 ...)
	@ preserves:	none
	PFUNC	1			@ primitive function, one input arg
lstvec:	@ [internal entry]
	set	sv5, sv1		@ sv5 <- items list (saved)
	sav__c				@ dts <- (cnt ...)
	set	sv2, sv1
	set	sv1, #i0
lstvln:	nullp	sv2
	itT	ne
	cdrne	sv2, sv2
	addne	sv1, sv1, #4
	bne	lstvln	
	set	sv2, #null		@ sv2 <- '() = fill for vector
	call	makvec			@ sv1 <- new, cleared vector of size sv1
	restor	cnt
	set	sv2, #i0		@ sv2 <- position of 1st vector item
lstve0:	nullp	sv5			@ done copying?
	it	eq
	seteq	pc,  cnt		@	if so,  exit
	snoc	sv4, sv5, sv5		@ sv4 <- next item,  sv5 <- remaining items
	vecset	sv1, sv2, sv4		@ store item in vector
	incr	sv2, sv2		@ sv2 <- position of next vector item
	b	lstve0			@ jump to continue copying from list to vector

.balign	4
	
svcfll:	SYMSIZE	12
	.ascii	"vector-fill!"
	.balign 4
	
pvcfll:	@ (vector-fill! vector fill)
	@ on entry:	sv1 <- vector
	@ on entry:	sv2 <- fill
	@ on exit:	sv1 <- vector == #(fill fill ...)
	PFUNC	2			@ primitive function, two input args
	b	vecfil

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg
	
	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.4.	control features:	map, for-each,	force
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.balign	4

smap:	SYMSIZE	3
	.ascii	"map"
	.balign 4

map:	@ (map fun list1 list2 ...)
	@ on entry:	sv1 <- fun
	@ on entry:	sv2 <- (list1 list2 ...)
	PFUNC	1			@ primitive function, one input arg
	set	sv3, #null
mapfor:	@ common loop for map/for-each
	save3	sv3, sv1, cnt		@ dts <- (()=val-list-or-#npo fun cnt ...)
	set	sv1, sv2		@ sv1 <- (list1 list2 ...)
_func_
mapf0:	nullp	sv1			@ is list of lists null?
	itT	ne
	carne	sv2, sv1		@	if not, sv1 <- list1
	nullpne	sv2			@	if not, is list1 null?
	beq	mapfxt			@	if so,  exit (lol or list is null)
	set	sv4, sv1		@ sv4 <- (list1 list2 ...) -- saved against cars
	set	sv3, sv4		@ sv3 <- (list1 list2 ...) -- for cars
	call	cars			@ sv1 <- (item1 item2 ...) -- cars of lists
	set	sv2, sv1		@ sv2 <- (item1 item2 ...)
	cadr	sv1, dts		@ sv1 <- fun
	save	sv4			@ dts <- ((list1 list2 ...) val-list fun cnt ...)
	call	apply			@ sv1 <- new-val, from applying fun in sv1 to (itm1 itm2 ...) = sv2
	restor	sv3			@ sv3 <- (list1 list2 ...),	dts <- (val-lst-or-#npo fun cnt ..)
	adr	cnt, mapf0		@ cnt <- mapf0 (return for cdrs)
	car	sv2, dts		@ sv2 <- val-list-or-#npo
	eq	sv2, #npo		@ doing for-each?
	beq	cdrs			@	if so,  sv1 <- (cdr-list1 cdr-list2 ...), and jump to mapf0
	cdr	dts, dts		@ dts <- (fun cnt ...)
	cons	sv1, sv1, sv2		@ sv1 <- new-val-list
	save	sv1			@ sv1 <- (new-val-list fun cnt ...)
	b	cdrs			@ sv1 <- (cdr-list1 cdr-list2 ...), and jump to mapf0
mapfxt:	@ exit
	restor3	sv1, sv2, cnt		@ sv1 <- val-list-or-#npo, sv2 <- dummy, cnt <- cnt, dts <- (...)
	eq	sv1, #npo		@ doing for-each?
	it	eq
	seteq	pc, cnt			@	if so,  return with npo
	b	revers			@ reverse val-list in sv1 and exit via cnt

.balign	4
	
sfreac:	SYMSIZE	8
	.ascii	"for-each"
	.balign 4
	
foreac:	@ (for-each fun list1 list2 ...)
	@ on entry:	sv1 <- fun
	@ on entry:	sv2 <- (list1 list2 ...)
	PFUNC	1			@ primitive function, one input arg
	set	sv3, #npo
	b	mapfor

.balign	4

sforce:	SYMSIZE	5
	.ascii	"force"
	.balign 4

force:	@ (force promise)
	@ on entry:	sv1 <- promise
	PFUNC	1			@ primitive function, one input arg
	set	sv2, #null		@ sv2 <- '()
	b	apply

@-------------------------------------------------------------------------------------
@  II.H.6.     Standard Procedures
@  II.H.6.4.   control features SUPPORT:		cars, cdrs
@-------------------------------------------------------------------------------------

cars:	@ return a list of cars	of the lists in sv3
	set	sv1, #null		@ sv1 <- '() = initial result
itcars:	nullp	sv3			@ done with lists?
	beq	irevrs			@	if so,  jump to reverse list and return via cnt
	set	sv2, sv1		@ sv2 <- (...) = current result
	car	sv1, sv3		@ sv1 <- arg1
	pntrp	sv1			@ is sv1 a pointer?
	it	eq
	careq	sv1, sv1		@	if so,  sv1 <- car-arg1
	cons	sv1, sv1, sv2		@ sv1 <- (car-arg1 ...)
	cdr	sv3, sv3		@ sv3 <- (arg2 arg3 ...)
	b	itcars			@ jump to continue consing cars
	
cdrs:	@ return a list of cdrs	of the lists in sv3
	set	sv1, #null		@ sv1 <- '() = initial result
itcdrs:	nullp	sv3			@ done with lists?
	beq	irevrs			@	if so,  jump to reverse list and return via cnt
	set	sv2, sv1		@ sv2 <- (...) = current result
	car	sv1, sv3		@ sv1 <- arg1
	pntrp	sv1			@ is arg1 a pointer?
	it	eq
	cdreq	sv1, sv1		@	if so,  sv1 <- cdr-arg1
	cons	sv1, sv1, sv2		@ sv1 <- (cdr-arg ...)
	cdr	sv3, sv3		@ sv3 <- (arg2 arg3 ...)
	b	itcdrs			@ jump to continue consing cdrs

irevrs:	@ reverse the list in sv1
	@ sv1  -> sv1  (iso-memory, uses sv2, sv3)
	set	sv2, #null		@ sv2 <- '() = initial result
	nullp	sv1			@ is input list null?
	it	eq
	seteq	pc,  cnt		@	if so,  return
precnt:	cdr	sv3, sv1		@ sv3 <- cdr of input list
	setcdr	sv1, sv2		@ sv2 -> cdr of input list
	nullp	sv3			@ done with input list?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	set	sv2, sv1		@ sv2 <- input list
	set	sv1, sv3		@ sv1 <- cdr of input list
	b	precnt

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg

@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.6.	Input and Output
@	6.6.1.	ports:			call-with-input-file,
@					call-with-output-file,
@	6.6.3.	output:			newline
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======

.balign	4
	
scwipf:	SYMSIZE	20
	.ascii	"call-with-input-file"
	.balign 4

cwinpf:	@ (call-with-input-file string <port-model> proc)
	@ on entry:	sv1 <- string
	@ on entry:	sv2 <- port-model or proc
	@ on entry:	sv3 <- proc or ()
	PFUNC	3			@ primitive function, three input args
	nullp	sv3			@ is port-model provided?
	itT	eq
	seteq	sv3, sv2		@	if not, sv3 <- proc
	seteq	sv2, #null		@	if not, sv2 <- () (no port)
	save3	sv1, sv3, cnt		@ dts <- (string proc cnt ...)
	call	opnife			@ sv1 <- file handle or 0
	set	sv2, sv1		@ sv2 <- file handle or 0
	restor2	sv3, sv1		@ sv3 <- string, sv1 <- proc, dts <- (cnt ...)
	eq	sv2, #i0		@ unable to open?
	beq	cwifer			@	if so,  jump to report error
	list	sv2, sv2		@ sv2 <- (file-handle)
	save	sv2			@ dts <- ((file-handle) cnt ...)
	call	apply			@ sv1 <- result of calling proc on port
	restor2	sv1, cnt		@ sv1 <- (file-handle), cnt <- cnt, dts <- (...)
	set	sv4, #(0x80 | t)
	bl	ioprfe
	b	clsipe			@ jump to close input port, return via cnt
	
cwifer:	@ error in call-with-input-file
	set	sv1, sv3		@ sv2 <- argument (fun or argl) that caused the error
	ldr	sv4, =scwipf		@ sv1 <- address of function with error
	b	error4

.balign	4
	
scwopf:	SYMSIZE	21
	.ascii	"call-with-output-file"
	.balign 4

cwoutf:	@ (call-with-output-file string <port-model> proc)
	@ on entry:	sv1 <- string
	@ on entry:	sv2 <- port-model or proc
	@ on entry:	sv3 <- proc or ()
	PFUNC	3			@ primitive function, three input args
	nullp	sv3			@ is port-model provided?
	itT	eq
	seteq	sv3, sv2		@	if not, sv3 <- proc
	seteq	sv2, #null		@	if not, sv2 <- () (no port)
	save3	sv1, sv3, cnt		@ dts <- (string proc cnt ...)
	call	opnofe			@ sv1 <- file handle or 0
	set	sv2, sv1		@ sv2 <- file handle or 0
	restor2	sv3, sv1		@ sv3 <- string, sv1 <- proc, dts <- (cnt ...)
	eq	sv2, #i0		@ unable to open?
	beq	cwofer			@	if so,  jump to report error
	list	sv2, sv2		@ sv2 <- (file-handle)
	save	sv2			@ dts <- ((file-handle) cnt ...)
	call	apply			@ sv1 <- result of calling proc on port
	restor2	sv1, cnt		@ sv1 <- (file-handle), cnt <- cnt, dts <- (...)
	set	sv2, #null		@ sv2 <- '() = normal close mode
	b	clsopt			@ jump to close output port, return via cnt
cwofer:	@ error in call-with-output-file
	set	sv1, sv3		@ sv2 <- argument (fun or argl) that caused the error
	ldr	sv4, =scwopf		@ sv1 <- address of function with error
	b	error4

.balign	4
	
snewln:	SYMSIZE	7
	.ascii	"newline"
	.balign 4

pnewln:	@ (newline <port> <reg> <n> ...)
	@ on entry:	sv1 <- (<port> <reg> <n> ...) or (((port <reg> <n> ...) . port-vector))
	@ on exit:	sv1 <- npo
	PFUNC	0			@ primitive function, listed input args
	set	sv2, sv1		@ sv2 <- (<port> <reg> <n> ...)
	set	rvb, #'\r
	raw2chr	sv1, rvb
	set	sv4, #((0x03<<2)|i0)
	b	ioprfn			@ jump to write the newline (cr)

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
.ltorg


@-------.-------.-------.-------.-------+
@	system utility  sub-environment	|
@-------.-------.-------.-------.-------+

	
.balign	4

s_dfnd:	SYMSIZE	8
	.ascii	"defined?"
	.balign 4
	
p_dfnd:	@ (defined? var)
	@ on entry:	sv1 <- var
	@ on exit:	sv1 <- #t/#f
	PFUNC	1			@ primitive function, one input arg
	bl	bndchk
	nullp	sv3
	b	notfxt
	
.balign	4
	
s_link:	SYMSIZE	4
	.ascii	"link"
	.balign 4
	
p_link:	@ (link cvec)
	@ on entry:	sv1 <- cvec = #(code-bytevector *asm-comp-link* *compile-syms* *long-jumps*)
	@ on exit:	sv1 <- linked code bytevector
	PFUNC	1			@ primitive function, one input arg
_func_
link:	@ [internal entry]
	vcrfi	sv2, sv1, 0		@ sv2 <- code-bytevector
	save3	sv2, sv1, cnt		@ dts <- (code-bytevector cvec cnt ...)
	@ link the symbols used by the compiled code
	vcrfi	sv5, sv1, 1		@ sv5 <- *asm-comp-link*
p_ln00:	@ loop over compiler symbols to link
	nullp	sv5			@ done linking compiler symbols?
	beq	p_lnlj			@	if so,  jump to link long jumps
	save	sv5			@ dts <- (symbols-to-link code-bytevector cvec cnt ...) 
	car	sv4, sv5		@ sv4 <- position of first symbol to link (pos . key)
	cdr	sv4, sv4		@ sv4 <- symbol key in *compile-syms* a-list
	caddr	sv1, dts		@ sv1 <- cvec = #(code-bv *as-lnk* *comp-syms* *lng-jmps*)
	vcrfi	sv3, sv1, 2		@ sv3 <- *compile-syms*
p_ln01:	@ search loop for symbol name
	nullp	sv3			@ done scanning *compile-syms*?
	beq	corerr			@	if so,  report error (key must be in a-list)
	snoc	sv2, sv3, sv3		@ sv2 <- (key1 . symname1), sv3 <- rest of *compile-syms*
	car	rva, sv2		@ rva <- key1
	eq	rva, sv4		@ key found?
	bne	p_ln01			@	if not, jump to check next item in *compile-syms*
	cdr	sv1, sv2		@ sv1 <- symbol-name (string)
	call	strsym			@ sv1 <- symbol (from name)
	restor	sv5			@ sv5 <- *asm-comp-link*, dts <- (code-bytevector cvec cnt ...)
	snoc	sv4, sv5, sv5		@ sv4 <- 1st sym to lnk (pos . key), sv5 <- rst of *asm-comp-link*
	car	sv4, sv4		@ sv4 <- position of symbol in code (scheme int)
	int2raw	rva, sv4		@ rva <- position of symbol in code (raw int)
	add	rva, rva, #4		@ rva <- position of symbol including bytevector header
	car	sv2, dts		@ sv2 <- code bytevector
	str	sv1, [sv2, rva]		@ store symbol in code bytevector
	b	p_ln00			@ jump to link other symbols
p_lnlj:	@ link the long jumps
	cadr	sv1, dts		@ sv1 <- cvec
	vcrfi	sv5, sv1, 3		@ sv5 <- *long-jumps*
p_ln04:	@ loop
	nullp	sv5			@ done linking long jumps?
	beq	p_lnxt			@	if so,  jump to exit
	save	sv5			@ dts <- (long-jumps code-bytevector cvec cnt ...)
	cdar	sv1, sv5		@ sv1 <- first long jump target name (string)
	call	strsym			@ sv1 <- first long jump target name (symbol)
@	vcrfi	env, glv, 7		@ env <- interaction env (if needed, also do save/restore at top)
	bl	bndchk			@ sv5 <- long jump preamble address (i.e. cdr of binding)
	add	rva, sv5, #4		@ rva <- function code start address
	restor	sv5			@ sv5 <- long jumps, dts <- (code-bytevector cvec cnt ...)
	snoc	sv4, sv5, sv5		@ sv4 <- first long jump, sv5 <- rest of long jumps
	car	sv4, sv4		@ sv4 <- position of first long jump in code (scheme int)
	int2raw	rvb, sv4		@ rvb <- position of first long jump in code (raw int)
	add	rvb, rvb, #4		@ rvb <- position of first long jump including bytevector header
	car	sv1, dts		@ sv1 <- code bytevector
	str	rva, [sv1, rvb]		@ store ong jump in code bytevector
	b	p_ln04			@ jump to link other long jumps
p_lnxt:	@ done, return
	restor3	sv1, sv2, cnt		@ sv1 <- code bytevector, sv2 <- cvec (dummy), cnt <- cnt
	set	pc,  cnt		@ return
	
@ (define (link cvec)
@   (let ((code (vector-ref cvec 0)))
@     ;; link the symbols used by the compiled code
@     (map
@      (lambda (lvar)
@	(let ((n (car lvar))
@	      (s (string->symbol (cdr (assq (cdr lvar) (vector-ref cvec 2))))))
@	  (bytevector-u16-native-set!
@	   code n (bitwise-ior (bitwise-arithmetic-shift s 2) #x0f)) ; synt/var
@	  (bytevector-u16-native-set! code (+ n 2) (bitwise-arithmetic-shift s -14))))
@      (vector-ref cvec 1))
@     ;; link the long jumps
@     (map
@      (lambda (ljmp)
@	(bytevector-copy!
@	 (address-of (eval (string->symbol (cdr ljmp)) (interaction-environment)) 4)
@	 0 code (car ljmp) 4))
@      (vector-ref cvec 3))
@     code))

	
.balign	4
	
s_upah:	SYMSIZE	17
	.ascii	"unpack-above-heap"
	.balign 4
	
p_upah:	@ (unpack-above-heap obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- result
	PFUNC	1			@ primitive function, one input arg
	set	sv2, #i1
_func_
p_upae:	@ [internal entry]
	vctrp	sv1
	bne	unpack
	sav_rc	sv2
	call	link
	restor2	sv2, cnt
	b	unpack

.balign	4
	
s_libs:	SYMSIZE	4
	.ascii	"libs"
	.balign 4
	
p_libs:	@ (libs)
	@ on exit:	sv1 <- list of library names
	PFUNC	0			@ primitive function, no input args
	vcrfi	sv5, glv, 12
	set	sv1, #null
p_lib0:	@ loop
	nullp	sv5
	it	eq
	seteq	pc,  cnt
	set	sv2, sv1
	snoc	sv4, sv5, sv5
	vcrfi	sv1, sv4, 0
	vcrfi	sv1, sv1, 0
	list	sv1, sv1
	cons	sv1, sv1, sv2
	b	p_lib0

.ifdef	LIB_TOP_PAGE
	
.balign	4
	
s_erlb:	SYMSIZE	10
	.ascii	"erase-libs"
	.balign 4
	
p_erlb:	@ (erase-libs)
	@ on exit:	sv1 <- list of library names
	PFUNC	0			@ primitive function, no input args
	set	sv1, #i0
	orr	sv1, sv1, #0x80000000
	list	sv1, sv1
	b	erasee
	
.balign	4
	
s_uplb:	SYMSIZE	13
	.ascii	"unpack-to-lib"
	.balign 4
	
p_uplb:	@ (unpack-to-lib obj)
	@ on entry:	sv1 <- obj
	@ on exit:	sv1 <- result
	PFUNC	1			@ primitive function, one input arg
	set	sv2, #f0	@ sv2 <- 0.0	(scheme float)
	mvn	sv2, sv2	@ sv2 <- -1	(scheme int)
	b	p_upae

.else	@ no LIB_TOP_PAGE (libs above heap)

.balign	4
	
s_uplb:	SYMSIZE	13
	.ascii	"unpack-to-lib"
	.balign 4
	
.endif	@ .ifdef LIB_TOP_PAGE

