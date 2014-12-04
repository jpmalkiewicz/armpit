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
		r3rs, integer_only, exclude_lib_mod, LIB_TOP_PAGE

*/

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV library

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				R5RS (see R3RS below)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.1.	conditionals:			cond, case, and, or
@	4.2.2.	binding constructs:		let, let*, letrec
@	4.2.3.	sequencing:			begin
@	4.2.4.	iteration:			do
@	4.2.5.	delayed evaluation:		delay
@	4.2.c.	constants:			else, =>, var0, var1, var2,
@						var3, var4, var5
@						(var6 is bound in read_write.s)
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	BNDVAR	"var0",  true
	BNDVAR	"var1",  true
	BNDVAR	"var2",  true
	BNDVAR	"var3",  true
	BNDVAR	"var4",  true
	BNDVAR	"var5",  true
	BNDVAR	"else",  true
	BNDVARi	implies, true ; .ascii "=>" ; ENDi

.ifndef r3rs

		@ (cond clause1 clause2 ...)
		@
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

		/* (cond clause1 clause2 ...) */
		PMACRO	"cond", implies_else, cond_body

cond_body:	LISTn	cond_a,	cond_b,	cond_c,	cond_d,	cond_e,	cond_f,	cond_g

cond_a:		LISTn	condaP, begin_res1_etc
condaP:		@ (_ (else result1 ...))
		LISTn	var_underscore, condaP2
condaP2:	CONSn	var_else, var2_ellipse

cond_b:		LISTn	condbP, condbT
condbP:		@ (_ (test => result))
		LISTn	var_underscore, test_impl_res
condbT:		@ (let ((temp test)) (if temp (result temp)))
		LISTn	var_let, temp_test, condbT2
condbT2:	LISTn	var_if, var_var0, result_temp

cond_c:		LISTn	condcP, condcT
condcP:		@ (_ (test => result) clause1 ...)
		CONSn	var_underscore, test_impl_res, var3_ellipse
condcT:		@ (let ((temp test)) (if temp (result temp) (cond clause1 ...)))
		LISTn	var_let, temp_test, condcT3
condcT3:	IF	var_var0, result_temp, cond_claus_etc

cond_d:		LISTn	conddP, var_var1
conddP:		@ (_ (test))
		LISTn	var_underscore, var1_null

cond_e:		LISTn	condeP, condeT
condeP:		@ (_ (test) clause1 ...)
		CONSn	var_underscore, var1_null, var3_ellipse
condeT:		@ (let ((temp test)) (if temp temp (cond clause1 ...)))
		LISTn	var_let, temp_test, condeT2
condeT2:	IF	var_var0, var_var0, cond_claus_etc

cond_f:		LISTn	condfP, condfT
condfP:		@ (_ (test result1 ...))
		LISTn	var_underscore, var1_2_ellipse
condfT:		@ (if test (begin result1 ...))
		LISTn	var_if, var_var1, begin_res1_etc

cond_g:		LISTn	condgP, condgT
condgP:		@ (_ (test result1 ...) clause1 ...)
		CONSn	var_underscore, var1_2_ellipse, var3_ellipse
condgT:		@ (if test (begin result1 ...) (cond clause1 ...))
		IF	var_var1, begin_res1_etc, cond_claus_etc

begin_res1_etc:	CONSn	var_begin,	var2_ellipse	@ (begin result1 ...)
test_impl_res:	LISTn	var_var1, var_implies, var_var2	@ (test => result)
temp_test:	LISTn	var0_1_null			@ ((temp test))
result_temp:	LISTn	var_var2, var_var0		@ (result temp)
cond_claus_etc:	CONSn	var_cond, var3_ellipse		@ (cond clause1 ...)


		@ (case key clause1 clause2 ...)
		@
		@(define-syntax case
		@  (syntax-rules (else)
		@    ((_ (key ...) clauses ...)
		@     (let ((atom-key (key ...))) (case atom-key clauses ...)))
		@    ((_ key (else result1 ...))
		@     (begin result1 ...))
		@    ((_ key ((atom ...) result1 ...))
		@     (if (memv key (quote (atom ...))) (begin result1 ...)))
		@    ((_ key ((atoms ...) result1 ...) clause1 ...)
		@     (if (memv key (quote (atoms ...))) (begin result1 ...)
		@       (case key clause1 ...)))))

		/* (case key clause1 clause2 ...) */
		PMACRO	"case", else_null, case_body

case_body:	LISTn	case_a,	case_b,	case_c,	case_d

case_a:		LISTn	caseaP, caseaT
caseaP:		@ (_ (key ...) clauses ...)
		CONSn	var_underscore, var0_ellipse, var2_ellipse
caseaT:		@ (let ((atom-key (key ...))) (case atom-key clauses ...))
		LISTn	var_let, caseaT2, case_key_clause
caseaT2:	LISTn	caseaT3
caseaT3:	LISTn	var_var1, var0_ellipse

case_b:		LISTn	casebP, begin_result1
casebP:		@ (_ key (else result1 ...))
		LISTn 	var_underscore, var_var0, casebP2
casebP2:	CONSn	var_else, var3_ellipse

case_c:		LISTn	casecP, casecT
casecP:		@ (_ key ((atom ...) result1 ...))
		LISTn	var_underscore,	var_var1, var0_ell_var3_ell
casecT:		@ (if (memv key (quote (atom ...))) (begin result1 ...))
		LISTn	var_if, memv_key_etc, begin_result1

case_d:		LISTn	casedP, casedT
casedP:		@ (_ key ((atoms ...) result1 ...) clause1 ...)
		CONSn	var_underscore,	var_var1, var0_ell_var3_ell, var2_ellipse
casedT:		@ (if (memv key (quote (atoms ...))) (begin result1 ...) (case key clause1 ...))
		IF	memv_key_etc, begin_result1, case_key_clause
	

case_key_clause: @ (case <atom>-key clauses ...)
		CONSn	var_case, var1_2_ellipse

begin_result1:	@ (begin result1 ...)
		CONSn	var_begin, var3_ellipse

memv_key_etc:	@ (memv key (quote (atom ...)))
		LISTn	var_memv, var_var1, casecT3
casecT3:	LISTn	var_quote, var0_ellipse

var0_ell_var3_ell: @ ((atom ...) result1 ...)
		CONSn	var0_ellipse,	var3_ellipse


		@ (and exp1 exp2 ...)
		@
		@ (define-syntax and
		@   (syntax-rules ()
		@     ((_) #t)
		@     ((_ test) test)
		@     ((_ test1 test2 ...)
		@      (if test1 (and test2 ...) #f))))

		/* (and exp1 exp2 ...) */
		PMACRO	"and", null, and1_body

and1_body:	LISTn	and1_b1, and1_b2, and1_b3
and1_b1:	LISTn	underscore_null, true		@ ((_) #t)
and1_b2:	LISTn	und_var1_null,	var_var1	@ ((_ test) test)
and1_b3:	LISTn	und_var12_ell,	and1_b31	@ ((_ tst1 tst2 ...) _)
and1_b31:	IF	var_var1, and1_b311, false	@     (if test1 ___ #f)
and1_b311:	AND_	var_var2, var_ellipsis		@       (and test2 ...)


		@ (or exp1 exp2 ...)
		@
		@(define-syntax or
		@  (syntax-rules ()
		@    ((_) #f )
		@    ((_ test) test)
		@    ((_ test1 test2 ...)
		@     (let ((x test1))
		@       (if x x (or test2 ...))))))

		/* (or exp1 exp2 ...) */
		PMACRO	"or", null, or1_body

or1_body:	LISTn	or1_b1, and1_b2, or1_b3		@ or1_b2 id to and1_b2
or1_b1:		LISTn	underscore_null, false		@ ((_) #f)
or1_b3:		LISTn	und_var12_ell,	or1_b31		@ ((_ tst1 tst2 ...) _)
or1_b31:	LET1	or1_b311, or1_b312		@  (let ______  _____ )
or1_b311:	LISTn	var0_1_null			@   ((x test1))
or1_b312:	IF	var_var0, var_var0, or1_b313	@        (if x x ____ )
or1_b313:	OR_	var_var2, var_ellipsis		@	     (or test2)


/*::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.::::::
@
@	4.2.2.	binding constructs:	let, let*, letrec
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.::::*/

		@ (let <name> bindings-list exp1 exp2 ...)
		@
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

		/* (let <name> bindings-list exp1 exp2 ...) */
		PMACRO	"let", null, let_body

let_body:	LISTn	let_t10, let_t20
let_t10:	LISTn	let_t10_pat, let_t10_tpl
let_t10_pat:	CONSn	var_underscore, letcm1		@ (_ ((nm vl) ..) bd .)
let_t10_tpl:	CONSn	letcm2, var1_ellipse		@ 

let_t20:	LISTn	let_t20_pat, let_t20_tpl
let_t20_pat:	CONSn	var_underscore, var_var3, letcm1
let_t20_tpl:	CONSn	let_t21_tpl, var1_ellipse
let_t21_tpl:	LISTn	var_letrec, let_t22_tpl, var_var3
let_t22_tpl:	LISTn	let_t23_tpl
let_t23_tpl:	LISTn	var_var3, letcm2

letcm1:		@ (((name val) ...) body1 ...)
		CONSn	. + 8,		var2_ellipse
		CONSn	var0_1_null,	ellipse_null

letcm2:		@ (lambda (name ...) body1 ...)
		CONSn	var_lambda, var0_ellipse, var2_ellipse


		@ (let* bindings-list exp1 exp2 ...)
		@
		@ (define-syntax let*
		@   (syntax-rules ()
		@     ((_ ()  body1 ...)
		@      (let ()  body1 ...))
		@     ((_ (binding1) body1 ...)
		@      (let (binding1) body1 ...))
		@     ((_ (binding1 binding2 ...) body1 ...)
		@      (let (binding1)
		@        (let* (binding2 ...) body1 ...)))))

		/* (let* bindings-list exp1 exp2 ...) */
		PMACRO	"let*", lets, null, lets_body

lets_body:	LISTn	lets_t10, lets_t20, lets_t30

lets_t10:	LISTn	lets_t10_pat, lets_t10_tpl
lets_t10_pat:	@ (_ ()  body1 ...)
		CONSn	var_underscore, null, var2_ellipse
lets_t10_tpl:	@ (let ()  body1 ...)
		CONSn	var_let, null, var2_ellipse

lets_t20:	LISTn	lets_t20_pat, lets_t20_tpl
lets_t20_pat:	@ (_ (binding1) body1 ...)
		CONSn	var_underscore, var1_null, var2_ellipse
lets_t20_tpl:	@ (let (binding1) body1 ...)
		CONSn	var_let, var1_null, var2_ellipse

lets_t30:	LISTn	lets_t30_pat, lets_t30_tpl
lets_t30_pat:	@ (_ (binding1 binding2 ...) body1 ...)
		CONSn	var_underscore, var1_2_ellipse, var0_ellipse
lets_t30_tpl:	@ (let (binding1) (let* (binding2 ...) body1 ...))
		LISTn	var_let, var1_null, lets_t31_tpl
lets_t31_tpl:	CONSn	var_lets, var2_ellipse, var0_ellipse


		@ (letrec bindings-list exp1 exp2 ...)
		@
		@ (define-syntax letrec
		@   (syntax-rules ()
		@     ((_ ((var1 init1) ...) body ...)
		@      (letrec #t (var1 ...) () ((var1 init1) ...) body ...))
		@     ((_ #t () (temp1 ...) ((var1 init1) ...) body ...)
		@      (let ((var1 #t) ...)
		@        (let ((temp1 init1) ...) (set! var1 temp1) ... body ...)))
		@     ((_ #t (x . y) temp ((var1 init1) ...) body ...)
		@      (letrec #t y (newtemp . temp) ((var1 init1) ...) body ...))))

		/* (letrec bindings-list exp1 exp2 ...) */
		PMACRO	"letrec", null, letr_body

letr_body:	LISTn	letr_t10, letr_t20, letr_t30

letr_t10:	LISTn	let_t10_pat, letr_t10_tpl		@ pattern same as let_t10
letr_t10_tpl:	@ (letrec #t (var1 ...) () ((var1 init1) ...) body ...)
		CONSn	var_letrec, true, var0_ellipse, null, letcm1

letr_t20:	LISTn	letr_t20_pat, letr_t20_tpl
letr_t20_pat:	@ (_ #t () (temp1 ...) ((var0 init1) ...) body ...)
		CONSn	var_underscore,	true, null, var3_ellipse, letcm1
letr_t20_tpl:	@ (let ((var0 #t) ...)
		@   (let ((temp1 init1) ...) (set! var0 temp1) ... body ...))
		LISTn	var_let, letr_t21_tpl, letr_t23_tpl
letr_t21_tpl:	CONSn	. + 8, ellipse_null
		CONSn	var_var0, true_null
letr_t23_tpl:	CONSn	var_let, letr_t25_tpl, letr_t27_tpl, var_ellipsis, var2_ellipse
letr_t25_tpl:	CONSn	. + 8, ellipse_null
		CONSn	var_var3, var1_null
letr_t27_tpl:	CONSn	var_set, var_var0, var3_null

letr_t30:	LISTn	letr_t30_pat, letr_t30_tpl
letr_t30_pat:	@ (_ #t (x . y) temp ((var0 init1) ...) body ...)
		CONSn	var_underscore, true, letr_t31_pat, var_var3, letcm1
letr_t31_pat:	CONSn	var_var4,	var_var5
letr_t30_tpl:	@ (letrec #t y (newtemp . temp) ((var0 init1) ...) body ...)
		CONSn	var_letrec, true, var_var5, letr_t31_tpl, letcm1
letr_t31_tpl:	CONSn	var_var6,	var_var3

/*::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.::::::
@
@	4.2.4.	iteration:		do
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.::::*/

		@ (do ((var1 init1 <step1>) ...) (test expr1 expr2 ...) command1 command2 ...)
		@
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

		/* (do ((var1 init1 <step1>) ...) (test exp1 exp2 ...) cmd1 cmd2 ...) */
		PMACRO	"do", null, do_body

do_body:	LISTn	do_b10, do_b20, do_b40, do_b50

do_b10:		LISTn	do_b10_pat, do_b10_tpl
do_b10_pat:	@ (_ ((var init . step) ...) (test expr ...))
		LISTn	var_underscore,	var03d4_ell, var1_2_ellipse
do_b10_tpl:	@ (do ((var init . step) ...) (test expr ...) #f)
		CONSn	var_do, var03d4_ell, var1_2_ellipse, false_null

do_b20:		LISTn	do_b20_pat, do_b20_tpl
do_b20_pat:	@ (_ ((var init . step) ...) (test expr ...) command ...)
		CONSn	var_underscore,	var03d4_ell, var1_2_ellipse, var_var5, ellipse_null
do_b20_tpl:	@ (letrec
		@   ((loop (lambda (var ...)
		@	      (if test
		@	         (begin (if #f #f) expr ...)
		@	         (begin command ... (loop (do #t var . step) ...))))))
		@  (loop init ...))
		LISTn	var_letrec, do_b22_tpl, do_b21_tpl
do_b21_tpl:	CONSn	var_var6,	var3_ellipse
do_b22_tpl:	LISTn	do_b23_tpl
do_b23_tpl:	LISTn	var_var6, do_b24_tpl
do_b24_tpl:	LAMBDA1	var0_ellipse, do_b25_tpl
do_b25_tpl:	IF	var_var1, do_b26_tpl, do_b28_tpl
do_b26_tpl:	CONSn	var_begin, do_b27_tpl, var2_ellipse
do_b27_tpl:	CONSn	var_if, false, false_null
do_b28_tpl:	LISTn	var_begin, var_var5, var_ellipsis, do_b29_tpl
do_b29_tpl:	CONSn	var_var6, do_b30_tpl, ellipse_null
do_b30_tpl:	CONSn	var_do, true, var_var0, var_var4

do_b40:		@ (_ #t x) -> x
		LISTn	do_b40_pat, var_var1
do_b40_pat:	CONSn	var_underscore, true, var1_null

do_b50:		@ (_ #t x y) -> y
		LISTn	do_b50_pat, var_var1
do_b50_pat:	CONSn	var_underscore, true, var0_1_null

var03d4_ell:	CONSn	var03d4, ellipse_null		@ ((var0 var3 . var4) .)
var03d4:	CONSn	var_var0, var_var3, var_var4	@ (var0 var3 . var4)

/*::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.::::::
@
@	4.2.5.	delayed evaluation:	delay
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.::::*/

		@ (delay expr)
		@
		@(define-syntax delay
		@  (syntax-rules ()
		@    ((_ expr)
		@     (_mkp (lambda () expr)))))

		/* (delay expr) */
		PMACRO	"delay", null, delay1_body

delay1_body:	LISTn	delay1_b1
delay1_b1:	LISTn	und_var1_null, delay1_b12	@ ((_ expr) ___ )
delay1_b12:	LISTn	var__mkp, delay1_b13		@   (_mkp ___ )
delay1_b13:	LAMBDA1	null, var_var1			@     (lambda () expr)


/*::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.c.	common completion lists
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::*/

.balign	8

var0_1_null:	CONSn	var_var0,	var1_null	@ (var0 var1)
und_var1_null:	CONSn	var_underscore,	var1_null	@ (_ var1)
und_var12_ell:	CONSn	var_underscore,	var1_2_ellipse	@ (_ var1 var2 ...)
var1_2_ellipse:	CONSn	var_var1,	var2_ellipse	@ (var1 var2 ...)
var0_ellipse:	CONSn	var_var0,	ellipse_null	@ (var0 ...)
var1_ellipse:	CONSn	var_var1,	ellipse_null	@ (var1 ...)
var2_ellipse:	CONSn	var_var2,	ellipse_null	@ (var2 ...)
var3_ellipse:	CONSn	var_var3,	ellipse_null	@ (var3 ...)
implies_else:	CONSn	var_implies,	else_null	@ (=> else)
else_null:	LISTn	var_else			@ (else)
ellipse_null:	LISTn	var_ellipsis			@ ( ...)
underscore_null: LISTn	var_underscore			@ (_)
var1_null:	LISTn	var_var1			@ (var1)
var3_null:	LISTn	var_var3			@ (var3)
true_null:	LISTn	scheme_true			@ (#t)
false_null:	LISTn	scheme_false			@ (#f)


/*::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.d.	more code
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::*/

	/* (_mkp proc) */
	PRIMIT	"_mkp", pfun, 1
	@ in:	sv1 <- proc = (lambda env () expr)
	@ out:	sv1 <- (promise env () expr)
	@ return a lambda with extended env
	@
	@ well, probably better off returning a compiled proc, branching to assembly code
	@ (maybe even two of those?)
	@
	@ var0 <- #f, var1 <- proc
	@ build var list
	ldr	sv2, =var_var2
	list	sv3, sv2
	ldr	sv2, =var_var1
	cons	sv3, sv2, sv3
	ldr	sv2, =var_var0
	cons	sv3, sv2, sv3		@ sv3 <- (var0 var1 var2)
	@ build val list
	list	sv4, sv1
	set	sv2, false
	cons	sv4, sv2, sv4
	cons	sv4, sv2, sv4		@ sv4 <- (#f #f proc)
	@ extend environment
	sav_ec				@ dts <- (env cnt ...)
	call	mkfrm
	@ build promise (compiled thunk with extended env)
	set	sv2, null
	ldr	sv3, =adr_prmcod
	set	sv1, procedure
	orr	sv1, sv1, #0xC000
	tagwenv	sv1, sv1, sv2, sv3	@ sv1 <- promise == [proc_tag () prmcod prom-env]
	restor	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	set	pc,  cnt


	/* code for make-promise (tagged,nonbound,direct prim (eg cmpld cod)) */
	PRIMIT	prmcod, ufun, 0, 0, 0
	ldr	sv1, =var_var0
	bl	bndchk
	eq	sv5, #t
	bne	prmco1
	ldr	sv1, =var_var1
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco1:	sav_ec
	ldr	sv1, =var_var2
	bl	bndchk
	set	sv1, sv5
	set	sv2, null
	call	adr__apl
	set	sv2, sv1
	restor	env, cnt
	ldr	sv1, =var_var0
	bl	bndchk
	eq	sv5, #t
	bne	prmco2
	ldr	sv1, =var_var1
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco2:	@ set result in promise env
	ldr	sv1, =var_var0
	bl	bndchk
	set	sv4, true
	setcdr	sv3, sv4
	ldr	sv1, =var_var1
	bl	bndchk
	setcdr	sv3, sv2
	set	sv1, sv2
	set	pc,  cnt
		

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.endif		@ .ifndef r3rs
	
/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@				R3RS (see R5RS above)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.1.	conditionals:			cond, case, and, or
@	4.2.2.	binding constructs:		let, let*, letrec
@	4.2.3.	sequencing:			begin
@	4.2.4.	iteration:			do
@	4.2.5.	delayed evaluation:		delay
@	4.2.c.	constants:			else, =>, var0, var1, var2,
@						var3, var4, var5
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

.ifdef r3rs

	/* (cond clause1 clause2 ...) */
	PRIMIT	"cond", sntx, 0
	@ in:	sv1 <- (clause1 clause2 ...)
	@ out:	sv1 <- result
	sav_ec				@ dts <- (env cnt ...)
	set	sv4, sv1		@ sv4 <- (clause1 clause2 ...)
cond0:	@ evaluate tests in sequence and branch to expr as appropriate
	nullp	sv4			@ no more clauses?
	beq	condxt			@	if so,  jump to exit with '() or #f
	caar	sv1, sv4		@ sv1 <- test1-or-else from clause1
	ldr	sv3, =var_else		@ sv3 <- else
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
	ldr	sv5, =var_implies	@ sv5 <- =>
	eq	sv2, sv5		@ is sv2 = =>?
	beq	cond2			@	if so,  jump to process that case
	set	sv2, null		@ sv2 <- '()
	restor	env			@ env <- original env, dts <- (cnt ...)
	set	sv1, procedure		@ sv1 <- procedure_tag
	orr	sv1, sv1, #0x4000	@ sv1 <- full proc tag
	tagwenv	sv1, sv1, sv2, sv4	@ sv1 <- proc == [proc_tag vars-list (body) env]
	restor	cnt			@ cnt <- cnt, dts <- (...)
	b	adr__apl
cond2:	@ evaluate the case with =>
	car	env, dts		@ env <- env
	list	sv1, sv1		@ sv2 <- (test-val)
	save	sv1			@ dts <- ((test-val) env cnt ...)
	cadr	sv1, sv4		@ sv1 <- procedure-of-1-arg
	call	eval			@ sv1 <- proc
	restor	sv2			@ sv2 <- (test-val) = argl
	restor	env, cnt		@ env <- original env, cnt <- cnt, dts <- (...)
	b	adr__apl
condxt:	@ exit without match
	restor	env, cnt		@ env <- original env, cnt <- cnt, dts <- (...)
	set	pc, cnt

	/* (case key clause1 clause2 ...) */
	PRIMIT	"case", sntx, 1
	@ in:	sv1 <- key
	@ in:	sv2 <- (clause1 clause2 ...)
	@ out:	sv1 <- result
	savrec	sv2			@ dts <- ((clause1 clause2 ...) env cnt ...)
	call	eval			@ sv1 <- key-val
	restor	sv4			@ sv4 <- (clause1 clause2 ...)
	save	sv1			@ dts <- (key-val env cnt ...)
	ldr	sv5, =var_else		@ sv5 <- else
case0:	@ look for key in clauses' datum
	nullp	sv4			@ done with clauses?
	beq	casext			@	if so,  jump to exit
	caar	sv2, sv4		@ sv2 <- datum1-or-else from clause1
	eq	sv2, sv5		@ is sv2 = else?
	beq	case1			@	if so,  jump to evaluate expressions1
	call	pmemq			@ sv1 <- sublist or #f (look for key-val sv1 in datum sv2)
	eq	sv1, #f			@ key not in datum?
	itT	eq
	careq	sv1, dts		@	if so,  sv1 <- key-val, restored
	cdreq	sv4, sv4		@	if so,  sv4 <- rest of datum-list
	beq	case0			@	if so,  jump to continue scanning datum-list
case1:	@ evaluate expressions
	restor	sv1, env, cnt		@ sv1 <- dummy, env <- orig env, cnt <- cnt, dts <- (...)
	cdar	sv1, sv4		@ sv1 <- (expr1 expr2 ...)
	b	sqnce
casext:	@ exit without match
	restor	sv1, env, cnt		@ sv1 <- dummy, env <- orig env, cnt <- cnt, dts <- (...)
	set	sv1, false
	set	pc, cnt

	/* (and exp1 exp2 ...) */
	PRIMIT	"and", sntx, 0
	@ in:	sv1 <- (exp1 exp2 ...),		sv5<- env
	@ out:	sv1 <- result
	set	sv4, true		@ sv4 <- #t, default value with no args
	b	andor			@ jump to common and/or process loop

	/* (or exp1 exp2 ...) */
	PRIMIT	"or", sntx, 0
	@ in:	sv1 <- (exp1 exp2 ...),		sv5<- env
	@ out:	sv1 <- result
	set	sv4, false			@ sv4 <- #f, default value with no args
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
	bne	andorL			@		if neither, jump to loop (AND with NOT #f)
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
	set	sv5, null		@ sv5 <- '()
	list	sv2, sv5		@ sv2 <- (() . var-list-tail)
	list	sv5, sv5		@ sv5 <- (() . uval-list-tail)
	save	sv2, sv5		@ dts <- ((() . vrls) (() . uvllst) (e1 e2 ..) <nm>/bls ..)
	set	sv3, sv1		@ sv3 <- bindings-list
let0:	@ build lists of init vars and init uvals
	nullp	sv3			@ is bindings-list done?
	beq	let1			@	if so, jump to continue
	snoc	sv1, sv3, sv3		@ sv1 <- binding1,	sv3 <- rest of bindings-list
	snoc	sv1, sv4, sv1		@ sv1 <- var1,		sv4 <- (uval1)
	list	sv1, sv1		@ sv1 <- (var1)
	setcdr	sv2, sv1		@ store (var1) at tail of sv2
	set	sv2, sv1		@ sv2 <- new var list tail
	car	sv4, sv4		@ sv4 <- uval1
	list	sv4, sv4		@ sv4 <- (uval1)
	setcdr	sv5, sv4		@ store (uval1) at tail of sv5
	set	sv5, sv4		@ sv5 <- new uval list tail
	b	let0			@ jump to continue building var and uval lists
let1:	@ extract built-lists and expr list from stack
	restor	sv2, sv5		@ sv2 <- (().vrls),sv5<-(().uvlls),dts<-((ex1.) <nm>|bls .)
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
	set	sv1, procedure		@ sv1 <- procedure tag
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
	snoc	sv1, sv3, sv1		@ sv1 <- binding1,	sv3 <- rest of bindings-list
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
	set	sv4, null		@ sv4 <- initial list of vals for let-vars
letr2:	@ evaluate let-vals
	nullp	sv3			@ is bindings-list done?
	beq	letr3			@	if so, jump to continue
	car	env, dts		@ env <- let-env
	snoc	sv1, sv3, sv3		@ sv1 <- binding1,	sv3 <- rest of bindings-list
	cadr	sv1, sv1		@ sv1 <- uval1
	save	sv4, sv3		@ dts <- (val-lst rst-b-lst let-env (exp1 ...) env cnt ...)
	call	eval			@ sv1 <- val1
	restor	sv4, sv3		@ sv4 <- oldvls,sv3<-rstblst,dts<-(ltnv ltbds (e1 .) cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- (val1 ...) = updated vals list	
	b	letr2			@ jump to continue building init vals list
letr3:	@ keep going
	restor	env			@ env <- let-env, dts <- (let-bndngs-lst (exp1 ..) cnt ..)
	restor	sv3, sv1, cnt		@ sv3 <- let-bndngs-lst, sv1 <- (e1 .),cnt<-cnt,dts<-(..)
	@ reverse vals list
	set	sv5, null
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

	/* (do ((var1 init1 <step1>) ...) (test exp1 exp2 ...) cmd1 cmd2 ...) */
	PRIMIT	"do", sntx, 2
	@ in:	sv1 <- ((var1 init1 <step1>) ...)
	@ in:	sv2 <- (test expr1 expr2 ...)
	@ in:	sv3 <- (command1 command2 ...)
	@ out:	sv1 <- result
	save	sv2, sv3, cnt		@ dts <- ((test expr1 ...) (command1 ...) cnt ...)
	save	env, sv1		@ dts <- (env inits-list (test expr1 ..) (cmd1 ..) cnt ..)
	set	sv4, null		@ sv4 <- '() = initial init-vals-list
do0:	@ build list of evaluated inits into sv4
	nullp	sv1			@ done with inits list?
	beq	do1			@	if so, jump to continue
	car	env, dts		@ env <- env
	snoc	sv1, sv2, sv1		@ sv1 <- (vr1 ini1 <stp1>), sv2 <- ((vr2 ini2 <stp2>) ..)
	cadr	sv1, sv1		@ sv1 <- init1
	save	sv4, sv2		@ dts <- (init-vals-lst ((vr2 ini2 <stp2>) ..) env ...)
	call	eval			@ sv1 <- init1-val
	restor	sv4			@ sv4 <- init-vals-lst, dts<-(((vr2 ini2 <stp2>) .) env .)
	cons	sv4, sv1, sv4		@ sv4 <- updated init-vals-list
	restor	sv1			@ sv1 <- ((vr2 in2 <stp2>) ..),	dts <- (env inits-list ...)
	b	do0			@ jump to continue evaluating inits
do1:	@ reverse vals list
	set	sv2, sv4
	set	sv4, null
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
	set	sv4, null		@ sv4 <- '() = initial init-vals-list
do6:	@ build list of evaluated steps into sv4 then jump back to iterate
	nullp	sv1			@ done with steps list?
	it	eq
	cdreq	dts, dts		@	if so,  dts<-(nv ini-ls (tst e1 .) (cmd1 .) cnt .)
	beq	do1			@	if so,  jump to next iteration
	car	env, dts		@ env <- do-env
	snoc	sv1, sv2, sv1		@ sv1 <- (vr1 ini1 <stp1>), sv2 <- ((vr2 ini2 <stp2>) ...)
	save	sv4, sv2		@ dts <- (step-vals-lst ((vr2 ini2 <stp2>) ...) do-env ...)
	cddr	sv2, sv1		@ sv2 <- (<step>)
	nullp	sv2			@ no step?
	itE	eq
	careq	sv1, sv1		@	if so,  sv1 <- var1
	carne	sv1, sv2		@	if not, sv1 <- step1
	call	eval			@ sv1 <- step1-val
	restor	sv4			@ sv4 <- stp-vls-ls, dts<-(((vr2 ini2 <stp2>) .) do-env .)
	cons	sv4, sv1, sv4		@ sv4 <- updated step-vals-list
	restor	sv1			@ sv1 <- ((vr2 in2 <stp2>) .),	dts<-(do-env env ini-lst .)
	b	do6			@ jump to continue evaluating steps
doexit:	@ exit the do -- evaluate the expressions
	restor	env, sv4, sv2		@ env <- donv,sv4<-nv,sv2<-dmy,dts<-((ts e1 .) (c1 .) cnt.)
	restor	sv3, sv2, cnt		@ sv3 <- (tst exp1 ..), sv2 <- dmy, cnt <- cnt, dts <- (..)
	set	sv2, null		@ sv2 <- '()
	cdr	sv3, sv3		@ sv3 <- (expr1 ...)
	set	sv1, procedure		@ sv1 <- procedure_tag
	orr	sv1, sv1, #0x4000	@ sv1 <- full proc tag	
	tagwenv	sv1, sv1, sv2, sv3	@ sv1 <- proc == [proc_tag vars-list (body) do-env]
	set	env, sv4		@ env <- env, restored
	b	adr__apl		@ jump to evaluate expression sequence

	/* (delay expr) */
	PRIMIT	"delay", sntx, 0
	@ in:	sv1 <- (expr)
	@ out:	sv1 <- (promise env () expr)
	set	sv2, sv1		@ sv2 <- (expr)
	set	sv1, null		@ sv1 <- '()
	set	sv3, procedure		@ sv3 <- partial proc tag
	orr	sv3, sv3, #0x4000	@ sv3 <- full proc tag
	tagwenv	sv1, sv3, sv1, sv2	@ sv1 <- proc == [proc_tag () expr env]
	b	adr__mkp

	/* (_mkp proc) */
	PRIMIT	"_mkp", pfun, 1
	@ in:	sv1 <- proc = (lambda env () expr)
	@ out:	sv1 <- (promise env () expr)
	@ return a lambda with extended env
	@
	@ well, probably better off returning a compiled proc, branching to assembly code
	@ (maybe even two of those?)
	@
	@ var0 <- #f, var1 <- proc
	@ build var list
	ldr	sv2, =var_var0
	ldr	sv3, =var_var1
	ldr	sv4, =var_var2
	set	sv5, null
	llcons	sv3, sv2, sv3, sv4, sv5	@ sv3 <- (var0 var1 var2)
	@ build val list
	set	sv2, false
	llcons	sv4, sv2, sv2, sv1, sv5	@ sv4 <- (#f #f proc)
	@ extend environment
	sav_ec				@ dts <- (env cnt ...)
	call	mkfrm
	@ build promise (compiled thunk with extended env)
	set	sv2, null
	ldr	sv3, =adr_prmcod
	set	sv4, procedure
	orr	sv4, sv4, #0xC000
	tagwenv	sv1, sv4, sv2, sv3	@ sv1 <- promise == [promise_tag () prm-cod prm-env]
	restor	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	set	pc,  cnt
	

	/* code for make-promise (tagged,nonbound,direct prim (eg cmpld cod)) */
	PRIMIT	prmcod, ufun, 0, 0, 0
	ldr	sv1, =var_var0
	bl	bndchk
	eq	sv5, #t
	bne	prmco1
	ldr	sv1, =var_var1
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco1:	sav_ec
	ldr	sv1, =var_var2
	bl	bndchk
	set	sv1, sv5
	set	sv2, null
	call	adr__apl
	set	sv2, sv1
	restor	env, cnt
	ldr	sv1, =var_var0
	bl	bndchk
	eq	sv5, #t
	bne	prmco2
	ldr	sv1, =var_var1
	bl	bndchk
	set	sv1, sv5
	set	pc,  cnt
prmco2:	@ set result in promise env
	ldr	sv1, =var_var0
	bl	bndchk
	set	sv4, true
	setcdr	sv3, sv4
	ldr	sv1, =var_var1
	bl	bndchk
	setcdr	sv3, sv2
	set	sv1, sv2
	set	pc,  cnt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.endif		@ .ifdef r3rs


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.1.	booleans:		not, boolean?
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (not obj) */
	PRIMIT	"not", efun, 1, otypchk, inot

	/* (boolean? obj) */
	PRIMIT	"boolean?", boolean, pfun, 1
	@ in:	sv1 <-obj
	@ out:	sv1 <- #t if obj = #t or #f, else #f
	eq	sv1, #t
	it	ne
	eqne	sv1, #f
	b	adr_boolxt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.2.	Pairs and list:		caar, cadr, ..., cdddar, cddddr,
@					null?, list?, list, length, append, reverse,
@					list-tail, list-ref, memq, memv, member,
@					assq, assv, assoc
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (caar list) to (cddddr list) */
	PRIMIT	  "caar", efun, 1, ocxxxxr, (0x04<<2)|i0
	PRIMIT	  "cadr", efun, 1, ocxxxxr, (0x05<<2)|i0
	PRIMIT	  "cdar", efun, 1, ocxxxxr, (0x06<<2)|i0
	PRIMIT	  "cddr", efun, 1, ocxxxxr, (0x07<<2)|i0
	PRIMIT	 "caaar", efun, 1, ocxxxxr, (0x08<<2)|i0
	PRIMIT	 "caadr", efun, 1, ocxxxxr, (0x09<<2)|i0
	PRIMIT	 "cadar", efun, 1, ocxxxxr, (0x0A<<2)|i0
	PRIMIT	 "caddr", efun, 1, ocxxxxr, (0x0B<<2)|i0
	PRIMIT	 "cdaar", efun, 1, ocxxxxr, (0x0C<<2)|i0
	PRIMIT	 "cdadr", efun, 1, ocxxxxr, (0x0D<<2)|i0
	PRIMIT	 "cddar", efun, 1, ocxxxxr, (0x0E<<2)|i0
	PRIMIT	 "cdddr", efun, 1, ocxxxxr, (0x0F<<2)|i0
	PRIMIT	"caaaar", efun, 1, ocxxxxr, (0x10<<2)|i0
	PRIMIT	"caaadr", efun, 1, ocxxxxr, (0x11<<2)|i0
	PRIMIT	"caadar", efun, 1, ocxxxxr, (0x12<<2)|i0
	PRIMIT	"caaddr", efun, 1, ocxxxxr, (0x13<<2)|i0
	PRIMIT	"cadaar", efun, 1, ocxxxxr, (0x14<<2)|i0
	PRIMIT	"cadadr", efun, 1, ocxxxxr, (0x15<<2)|i0
	PRIMIT	"caddar", efun, 1, ocxxxxr, (0x16<<2)|i0
	PRIMIT	"cadddr", efun, 1, ocxxxxr, (0x17<<2)|i0
	PRIMIT	"cdaaar", efun, 1, ocxxxxr, (0x18<<2)|i0
	PRIMIT	"cdaadr", efun, 1, ocxxxxr, (0x19<<2)|i0
	PRIMIT	"cdadar", efun, 1, ocxxxxr, (0x1A<<2)|i0
	PRIMIT	"cdaddr", efun, 1, ocxxxxr, (0x1B<<2)|i0
	PRIMIT	"cddaar", efun, 1, ocxxxxr, (0x1C<<2)|i0
	PRIMIT	"cddadr", efun, 1, ocxxxxr, (0x1D<<2)|i0
	PRIMIT	"cdddar", efun, 1, ocxxxxr, (0x1E<<2)|i0
	PRIMIT	"cddddr", efun, 1, ocxxxxr, (0x1F<<2)|i0

	/* (cxxxxr obj) */
	PRIMIT	cxxxxr, ufun, 0
	@ in:	sv1 <- list
	@ in:	sv4 <- car/cdr code (scheme int)
	@ in:	sv5 <- symbol of function after the one that called cxxxxr (for error4)
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
	pairp	sv1			@ is it a pair?
	bne	adr__err
	tst	rvb, #1			@ is bit 0 of rva a one?
	itE	eq
	careq	sv1, sv1		@	if not, sv1 <- (car list)
	cdrne	sv1, sv1		@	if so,  sv1 <- (cdr list)
	lsr	rvb, rvb, #1		@ rva <- shifted car/cdr code
	b	jxxxxr			@ jump to continue

	/* (null? obj) */
	PRIMIT	"null?", null, efun, 1, otypchk, inul

	/* (list? obj) */
	PRIMIT	"list?", list, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- #t/#f
	ldr	rvc, =heapbottom	@ rvc <- heap bottom address
	ldr	rva, =heaptop1		@ rva <- heap top address
	sub	rvc, rva, rvc		@ rvc <- max number of bytes to search
	lsr	rvc, rvc, #3		@ rvc <- max number of cons cells to search
	set	sv3, sv1		@ sv3 <- start of list -- for cyclic list
qlist0:	nullp	sv1			@ is list '()?
	beq	adr_boolxt		@	if so,  return #t
	pairp	sv1			@ is item a pair?
	bne	adr_flsfxt		@	if not, return #f
	cdr	sv1, sv1		@ sv1 <- (cdr obj)
	subs	rvc, rvc, #1		@ rvc <- remnng mx num cons cells to search, is it zero?
	beq	adr_flsfxt		@	if so,  return #f
	b	qlist0			@ jump to keep going through potential list

	/* (list item1 item2 ...) */
	PRIMIT	"list", mklist, efun, 0, oreturn, null

	/* (length list) */
	PRIMIT	"length", pfun, 1
	@ in:	sv1 <- list
	@ out:	sv1 <- length of list (scheme int)
	ldr	rvc, =heapbottom	@ rvc <- heap bottom address
	ldr	rva, =heaptop1		@ rva <- heap top address
	sub	rvc, rva, rvc		@ rvc <- max number of bytes to search
	lsr	rvc, rvc, #3		@ rvc <- max number of cons cells to search
	set	sv2, i0			@ sv2 <- initial list length = 0 (scheme int)
lengt0:	nullp	sv1			@ at end of list?
	itT	eq
	seteq	sv1, sv2		@	if so,  sv1 <- length
	seteq	pc,  cnt		@	if so,  return with length
	pairp	sv1			@ is object a pair?
	bne	adr_flsfxt		@	if not, return #f
	cdr	sv1, sv1		@ sv1 <- (cdr obj)
	incr	sv2, sv2		@ sv2 <- updated list length
	subs	rvc, rvc, #1		@ rvc <- remaining max num cons cells to srch, is it zero?
	beq	adr_flsfxt		@	if so,  return #f
	b	lengt0			@ jump back to keep going

	/* (append list1 list2 ...) */
	PRIMIT	"append", pfun, 0
	@ in:	sv1 <- (list1 list2 ...)
	@ out:	sv1 <- appended lists
	set	sv4, sv1		@ sv4 <- (list1 list2 ...)
	set	sv1, null		@ sv1 <- '()
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

	/* (reverse list) */
	PRIMIT	"reverse", pfun, 1
	@ in:	sv1 <- list
	@ out:	sv1 <- reversed list
	set	sv2, sv1		@ sv2 <- list
	set	sv1, null		@ sv1 <- '() = initial reversed list
_func_
rvrls0:	pairp	sv2			@ item is a list?
	it	ne
	setne	pc,  cnt		@	if not, return reversed list in sv1
	set	sv3, sv1		@ sv2 <- current reversed list
	snoc	sv1, sv2, sv2		@ sv1 <- car of rest of list, sv4  <- cdr of rest of list
	cons	sv1, sv1, sv3		@ sv1 <- (car . reversed list) = updated reversed list
	b	rvrls0

	/* (list-tail list k) */
	PRIMIT	"list-tail", list_tail, pfun, 2
	@ in:	sv1 <- list
	@ in:	sv2 <- k
	@ out:	sv1 <- tail of list or ()
	set	sv3, false
	b	lstre0

	/* (list-ref list k) */
	PRIMIT	"list-ref", list_ref, pfun, 2
	@ in:	sv1 <- list
	@ in:	sv2 <- k
	@ out:	sv1 <- k-th item from list or ()
	set	sv3, true
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

	/* (memq obj list) */
	PRIMIT	"memq", pfun, 2
	@ in:	sv1 <- obj
	@ in:	sv2 <- list
	@ out:	sv1 <- sub-list or #f
	@ mods:	sv1, sv2, sv3
	swap	sv1, sv2, sv3
memv0:	nullp	sv1			@ is list null?
	beq	adr_notfxt
	car	sv3, sv1		@ sv3 <- (car list)
	eq	sv2, sv3		@ is car list = obj?
	it	eq
	seteq	pc,  cnt
	cdr	sv1, sv1		@ sv1 <- (cdr list)
	b	memv0			@ jump to continue looking for obj

	/* (memv obj list) */
	BNDVAR	"memv", val_memq

	/* (member obj list) */
	PRIMIT	"member", pfun, 2
	@ in:	sv1 <- obj
	@ in:	sv2 <- list
	@ out:	sv1 <- sub-list or #f
	sav_rc	sv1			@ dts <- (obj cnt ...)
membe0:	nullp	sv2			@ is list null?
	it	eq
	seteq	sv1, false		@	if so,  sv1 <- #f
	beq	membxt			@	if so,  jump to exit with #f
	car	sv1, dts		@ sv1 <- obj
	save	sv2			@ dts <- (list obj cnt ...)
	car	sv2, sv2		@ sv2 <- arg = (car list)
	call	adr_equal		@ sv1 <- #t/#f, from (equal sv1 sv2)
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

	/* (assq obj alist) */
	PRIMIT	"assq", pfun, 2
	@ in:	sv1 <- obj
	@ in:	sv2 <- alist
	@ out:	sv1 <- binding-or-#f
	@ out:	sv2 <- null-or-rest-of-alist
	@ out:	sv3 <- obj
	@ keep:	sv4-sv5
	set	sv3, sv1		@ sv3 <- obj = key
assq0:	@ loop
	nullp	sv2			@ is binding-list null?
	beq	adr_notfxt		@	if so,  exit with #f
	snoc	sv1, sv2, sv2		@ sv1 <- 1st binding,		sv2 <- rest of binding-list
	car	sv4, sv1		@ sv4 <- bkey of 1st binding (bkey . bval) in binding-list
	eq	sv3, sv4		@ is bkey = key ?
	bne	assq0			@	if not, jump to keep searching
	set	pc,  cnt		@ return with binding in sv1

	/* (assv obj alist) */
	BNDVAR	"assv", val_assq

	/* (assoc key binding-list) */
	PRIMIT	"assoc", pfun, 2
	@ in:	sv1 <- key
	@ in:	sv2 <- binding-list
	@ out:	sv1 <- binding-or-#f
	sav_rc	sv1			@ dts <- (key cnt ...)
assoc0:	@ sv1 <- binding-list, dts <- (key ...)
	nullp	sv2			@ is binding-list null?
	it	eq
	seteq	sv1, false		@	if so,  sv1 <- #f
	beq	assoxt			@	if so,  jump to exit with #f
	car	sv1, dts		@ sv1 <- key
	save	sv2			@ dts <- (binding-list key cnt ...)
	caar	sv2, sv2		@ sv2 <- key
	call	adr_equal		@ sv1 <- #t/#f, from (equal sv1 sv2)
	restor	sv2			@ sv2 <- binding-list,	dts <- (key cnt ...)
	eq	sv1, #t			@ was a binding found?
	it	ne
	cdrne	sv2, sv2		@	if not, sv2 <- rest-of-binding-list
	bne	assoc0			@	if not, jump to continue searching
	car	sv1, sv2		@ sv1 <- winning binding
assoxt:	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,		dts <- (...)
	set	pc,  cnt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.4.	Characters:		char-ci=?, char-ci<?, char-ci>?,
@					char-ci<=?, char-ci>=?, char-alphabetic?
@					char-numeric?, char-whitespace?,
@					char-upper-case?, char-lower-case?,
@					char-upcase, char-downcase
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (char-ci=?  char1 char2) */
	PRIMITi	chrceq, pfun, 2 ; .ascii "char-ci=?" ; ENDi
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	ldr	lnk, =adr_chareq
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char=?

	/* (char-ci<?  char1 char2) */
	PRIMIT	"char-ci<?", chrclt, pfun, 2
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	ldr	lnk, =adr_charlt
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char<?

	/* (char-ci>?  char1 char2) */
	PRIMIT	"char-ci>?", chrcgt, pfun, 2
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	ldr	lnk, =adr_chargt
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char>?

	/* (char-ci<=? char1 char2) */
	PRIMITi	chrcle, pfun, 2 ; .ascii "char-ci<=?" ; ENDi
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	ldr	lnk, =adr_charle
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char<=?

	/* (char-ci>=? char1 char2) */
	PRIMITi	chrcge, pfun, 2 ; .ascii "char-ci>=?" ; ENDi
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	ldr	lnk, =adr_charge
	b	toloc2			@ sv1, sv2 <- lower case char1, char2 and jump to char>=?

	/* (char-alphabetic? char) */
	PRIMIT	"char-alphabetic?", chralp, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- #t/#f
	bl	tolocs			@ rvc <- #t/#f based on whether sv1 is upper case
	eq	rvc, #t			@ is char upper case?
	it	ne
	blne	toupcs			@	if not,  rvc <- #t/#f if sv1 is up/lo case
	set	sv1, rvc		@ sv1 <- #t/#f result (#t if upper case or lower case)
	set	pc,  cnt		@ return

	/* (char-numeric?    char) */
	PRIMIT	"char-numeric?", chrnum, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- #t/#f
	set	rvb, '9			@ rvb <- ascii char 9
	chr2raw	rva, sv1		@ rva <- raw char
	cmp	rva, #'0		@ is char >= ascii char 0?
	it	pl
	cmppl	rvb, rva		@	if so,  is char <= ascii char 9?
	itE	pl
	setpl	sv1, true		@		if so,  sv1 <- #t
	setmi	sv1, false		@		if not, sv1 <- #f
	set	pc,  cnt		@ return with #t/#f
	
	/* (char-whitespace? char) */
	PRIMIT	"char-whitespace?", chrspace, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- #t/#f
	chr2raw	rvb, sv1
	eq	rvb, #'\r		@ is char a carriage return?
	it	ne
	eqne	rvb, #'  		@	if not, is char a space?
	it	ne
	eqne	rvb, #'			@	if not, is char a tab?
	it	ne
	eqne	rvb, #'\n		@	if not, is char a lf?
	b	adr_boolxt		@ return with #t/#f

	/* (char-upper-case? char) */
	PRIMIT	"char-upper-case?", chrupc, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- #t/#f
	bl	tolocs			@ rvc <- #t/#f based on whether sv1 is upper case
	set	sv1, rvc		@ sv1 <- result
	set	pc,  cnt		@ return

	/* (char-lower-case? char) */
	PRIMIT	"char-lower-case?", chrloc, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- #t/#f
	bl	toupcs			@ rvc <- #t/#f based on whether sv1 is lower case
	set	sv1, rvc		@ sv1 <- result
	set	pc,  cnt		@ return

	/* (char-upcase      char) */
	PRIMIT	"char-upcase", chrup, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- char in upper case
	set	lnk, cnt
	b	toupcs

	/* (char-downcase    char) */
	PRIMIT	"char-downcase", chrdown, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- char in lower case
	set	lnk, cnt
	b	tolocs
	
.balign	4
	
toupcs:	@ in:	sv1 <- char
	@ out:	sv1 <- upper case version of char
	@ out:	rvc <- #t/#f based on whether char was lower case or not
	set	rvb, 'z
	chr2raw	rva, sv1
	cmp	rva, #'a
	it	pl
	cmppl	rvb, rva
	itTE	pl
	bicpl	sv1, sv1, #0x2000	@	if so,  sv1 <- upper case version of char
	setpl	rvc, true
	setmi	rvc, false
	set	pc,  lnk

.balign	4
	
_func_
toloc2:	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- lower case version of char1
	@ out:	sv2 <- lower case version of char2
	@ out:	rvc <- #t/#f based on whether char1 was upper case or not
	set	rvb, 'Z
	chr2raw	rva, sv2
	cmp	rva, #'A
	it	pl
	cmppl	rvb, rva
	it	pl
	orrpl	sv2, sv2, #0x2000
_func_
tolocs:	@ in:	sv1 <- char
	@ out:	sv1 <- lower case version of char
	@ out:	rvc <- #t/#f based on whether char was upper case or not
	set	rvb, 'Z
	chr2raw	rva, sv1
	cmp	rva, #'A
	it	pl
	cmppl	rvb, rva
	itTE	pl
	orrpl	sv1, sv1, #0x2000
	setpl	rvc, true
	setmi	rvc, false
	set	pc,  lnk

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.5.	Strings:		string, string=?,
@					string-ci=?, string<?, string>?,
@					string<=?, string>=?, 
@					string-ci<?, string-ci>?,
@					string-ci<=?, string-ci>=?,
@					substring, string-append,
@					string->list, list->string,string-copy,
@					string-fill
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (string char1 char2 ...) */
	PRIMIT	"string", mkstring, pfun, 0
	@ in:	sv1 <- (char1 char2 ...)
	@ out:	sv1 <- string
	set	sv5, sv1		@ sv5 <- char list (saved)
	set	sv2, i0			@ sv2 <- initial list length = 0 (scheme int)
strin0:	nullp	sv1
	itT	ne
	cdrne	sv1, sv1
	incrne	sv2, sv2
	bne	strin0
	straloc	sv1, sv2		@ sv1 <- allocated string of size sv2
	set	sv3, i0			@ sv3 <- offset to start address of items (scheme int)
strin1:	cmp	sv3, sv2		@ done copying chars?
	it	pl
	setpl	pc,  cnt		@	if so,  return string
	snoc	sv4, sv5, sv5		@ sv4 <- 1st char (sch char),  sv5 <- remaining chars list
	strset	sv1, sv3, sv4
	incr	sv3, sv3
	b	strin1

	/* (string=? string1 string2) */
	PRIMITi	streq, pfun, 2 ; .ascii "string=?" ; ENDi
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chareq	@ sv5 <- address of comparison routine
streq0:	@ [ internal entry]
	strlen	sv3, sv1		@ sv3 <- length of string1
	strlen	sv4, sv2		@ sv4 <- length of string2
	eq	sv3, sv4		@ are string lengths equal?
	bne	adr_boolxt		@	if not, return with #f
strcmp:	@ [internal entry] string comparison based on function in sv5
	save	sv1, sv2, cnt		@ dts <- (string1 string2 cnt ...)
	strlen	sv3, sv1		@ sv3 <- length of string1
	strlen	sv4, sv2		@ sv4 <- length of string2
	cmp	sv4, sv3		@ is string2 shorter than string1?
	it	mi
	setmi	sv3, sv4		@	if so,  sv3 <- shortest string length
	save	sv3			@ dts <- (length string1 string2 cnt ...)
	set	sv1, true			@ sv1 <- #t = initial result
	set	sv4, i0			@ sv4 <- 0, start char offset (scheme int)
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
_func_	
strcrt:	eq	sv1, #f			@ did test fail?
	beq	strcxt			@	if so,  jump to exit
	add	sv4, sv4, #4		@ sv4 <- offset f next char
	b	strclp			@ jump to keep comparing chars
strcxt:	@ exit
	restor	sv2			@ sv2 <- length, dts <- (string1 string2 cnt ...)
	restor	sv2, sv3, cnt		@ sv2 <- string1, sv3 <- string2, cnt <- cnt, dts <-(...)
	set	pc,  cnt		@ return

	/* (string-ci=? string1 string2) */
	PRIMITi	strceq, pfun, 2 ; .ascii "string-ci=?" ; ENDi
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chrceq	@ sv5 <- address of comparison routine
	b	streq0

	/* (string<? string1 string2) */
	PRIMIT	"string<?", strlt, pfun, 2
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_charlt	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string>? string1 string2) */
	PRIMIT	"string>?", strgt, pfun, 2
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chargt	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string<=? string1 string2) */
	PRIMITi	strle, pfun, 2 ; .ascii "string<=?" ; ENDi
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_charle	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string>=? string1 string2) */
	PRIMITi	strge, pfun, 2 ; .ascii "string>=?" ; ENDi
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_charge	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string-ci<? string1 string2) */
	PRIMIT	"string-ci<?", strclt, pfun, 2
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chrclt	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string-ci>? string1 string2) */
	PRIMIT	"string-ci>?", strcgt, pfun, 2
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chrcgt	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string-ci<=? string1 string2) */
	PRIMITi	strcle, pfun, 2 ; .ascii "string-ci<=?" ; ENDi
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chrcle	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (string-ci>=? string1 string2) */
	PRIMITi	strcge, pfun, 2 ; .ascii "string-ci>=?" ; ENDi
	@ in:	sv1 <- string1
	@ in:	sv2 <- string2
	@ out:	sv1 <- #t/#f
	ldr	sv5, =adr_chrcge	@ sv5 <- address of comparison routine
	b	strcmp			@ jump to compare strings and return with #t/#f

	/* (substring string start end) */
	PRIMIT	"substring", pfun, 3
	@ in:	sv1 <- list
	@ in:	sv2 <- start
	@ in:	sv3 <- end
	@ out:	sv1 <- string
	set	lnk, cnt
	b	subcpy

	/* (string-append st1 st2 ...) */
	PRIMIT	"string-append", str_append, pfun, 0
	@ in:	sv1 <- (st1 st2 ...)
	@ out:	sv1 <- string
	set	sv5, sv1		@ sv5 <- (st1 st2 ...)
	set	sv4, sv1		@ sv4 <- (st1 st2 ...)
	@ count total number of chars in strings to be appended
	set	sv2, i0			@ sv2 <- initial size
strap0:	nullp	sv4			@ done counting chars?
	beq	strap1			@	if so,  jump to allocate new string
	snoc	sv1, sv4, sv4		@ sv1 <- st1,  sv4 <- (st2 ...)
	strlen	sv3, sv1		@ sv3 <- number of characters in st1
	plus	sv2, sv2, sv3		@ sv2 <- updated character count
	b	strap0			@ jump to keep counting chars
strap1:	@ allocate memory for new string
	straloc	sv1, sv2		@ sv1 <- newly-allocated target string
	@ append strings into new string
	set	sv4, i0			@ sv4 <- offset to start address of items (scheme int)
strap2:	nullp	sv5			@ done with all strings?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	snoc	sv3, sv5, sv5		@ sv3 <- source string,  sv5 <- rest-of-source-string-list
	strlen	sv2, sv3		@ sv2 <- number of characters in source string
	set	rvb, i0			@ rvb <- offset to start address in source
strap3:	eq	rvb, sv2		@ done with this string?
	beq	strap2			@	if so,  jump to process next string
	strref	rva, sv3, rvb		@ rva <- source raw ASCII char
	strset	sv1, sv4, rva		@ store it in target string
	incr	rvb, rvb		@ rvb <- updated offset to source char
	incr	sv4, sv4		@ sv4 <- updated offset to destination char
	b	strap3			@ jump to keep copying

	/* (string->list string) */
	PRIMIT	"string->list", str2lst, pfun, 1
	@ in:	sv1 <- string
	@ out:	sv1 <- list
	set	sv3, sv1		@ sv3 <- string
	strlen	sv4, sv3		@ sv4 <- number of characters in string
	set	sv1, null		@ sv1 <- '()
strls0:	izerop	sv4			@ done making list?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	set	sv2, sv1		@ sv2 <- previous list of characters (for cons)
	decr	sv4, sv4		@ sv4 <- offset of next character
	strref	sv1, sv3, sv4		@ sv1 <- next character
	cons	sv1, sv1, sv2		@ sv1 <- (char ...) = updated list of characters
	b	strls0			@ jump to process rest of string

	/* (list->string list) */
	PRIMIT	"list->string", lst2str, pfun, 1
	@ in:	sv1 <- list
	@ out:	sv1 <- string
	b	adr_mkstring

	/* (string-copy string) */
	PRIMIT	"string-copy", str_copy, pfun, 1
	@ in:	sv1 <- string
	@ out:	sv1 <- string
	set	sv2, i0			@ sv2 <- position of 1st char (scheme int)
	strlen	sv3, sv1		@ sv3 <- position after last char (scheme int)
	b	adr_substring

	/* (string-fill! string char) */
	PRIMIT	"string-fill!", str_fill, pfun, 2
	@ in:	sv1 <- string
	@ in:	sv2 <- char
	@ out:	sv1 <- string
	strlen	sv3, sv1		@ sv3 <- string length (scheme int)
	chr2raw	rvb, sv2		@ rvb <- ascii char
	b	fill8			@ perform fill and return

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.6.	Vectors:		vector, vector->list, list->vector,
@					vector-fill
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (vector item1 item2 ...) */
	PRIMIT	"vector", mkvector, pfun, 0
	@ in:	sv1 <- (item1 item2 ...)
	@ out:	sv1 <- vector == #(item1 item2 ...)
	b	adr_lst2vec

	/* (vector->list vector) */
	PRIMIT	"vector->list", vec2lst, pfun, 1
	@ in:	sv1 <- vector
	@ out:	sv1 <- list
	@ keep:	sv2 (for wrtvec:)
	set	sv4, sv1		@ sv4 <- vector
	veclen	sv5, sv4		@ sv5 <- number of items, (scheme int)
	set	sv1, null		@ sv1 <- '() = initial result list
vecls0:	izerop	sv5			@ no more vector items?
	it	eq
	seteq	pc,  cnt		@	if so,  exit
	set	sv3, sv1		@ sv3 <- current result list
	decr	sv5, sv5		@ sv5 <- position of next item from vector
	vecref	sv1, sv4, sv5		@ sv1 <- item from vector
	cons	sv1, sv1, sv3		@ sv1 <- updated result list
	b	vecls0			@ jump to continue

	/* (list->vector list) */
	PRIMIT	"list->vector", lst2vec, pfun, 1
	@ in:	sv1 <- list   ==  (item1 item2 ...)
	@ out:	sv1 <- vector == #(item1 item2 ...)
	@ keep:	none
	set	sv5, sv1		@ sv5 <- items list (saved)
	sav__c				@ dts <- (cnt ...)
	set	sv2, sv1
	set	sv1, i0
lstvln:	nullp	sv2
	itT	ne
	cdrne	sv2, sv2
	addne	sv1, sv1, #4
	bne	lstvln
	set	sv2, null		@ sv2 <- '() = fill for vector
	call	adr_make_vec		@ sv1 <- new, cleared vector of size sv1
	restor	cnt
	set	sv2, i0			@ sv2 <- position of 1st vector item
lstve0:	nullp	sv5			@ done copying?
	it	eq
	seteq	pc,  cnt		@	if so,  exit
	snoc	sv4, sv5, sv5		@ sv4 <- next item,  sv5 <- remaining items
	vecset	sv1, sv2, sv4		@ store item in vector
	incr	sv2, sv2		@ sv2 <- position of next vector item
	b	lstve0			@ jump to continue copying from list to vector

	/* (vector-fill! vector fill) */
	PRIMIT	"vector-fill!", vec_fill, pfun, 2
	@ in:	sv1 <- vector
	@ in:	sv2 <- fill
	@ out:	sv1 <- vector == #(fill fill ...)
	b	vecfil

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.4.	control features:	map, for-each,	force
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/


	/* (map fun list1 list2 ...) */
	PRIMIT	"map", pfun, 1
	@ in:	sv1 <- fun
	@ in:	sv2 <- (list1 list2 ...)
	set	sv3, null
mapfor:	@ common loop for map/for-each
	save	sv3, sv1, cnt		@ dts <- (()=val-list-or-#npo fun cnt ...)
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
	call	adr__apl		@ sv1 <- new-val, frm fun in sv1 apl to (it1 it2 .) = sv2
	restor	sv3			@ sv3 <- (ls1 ls2 ..), dts <- (val-lst-or-#npo fun cnt ..)
	adr	cnt, mapf0		@ cnt <- mapf0 (return for cdrs)
	car	sv2, dts		@ sv2 <- val-list-or-#npo
	eq	sv2, #npo		@ doing for-each?
	beq	cdrs			@	if so,  sv1 <- (cdr-ls1 cdr-ls2 .) & jmp to mapf0
	cdr	dts, dts		@ dts <- (fun cnt ...)
	cons	sv1, sv1, sv2		@ sv1 <- new-val-list
	save	sv1			@ sv1 <- (new-val-list fun cnt ...)
	b	cdrs			@ sv1 <- (cdr-list1 cdr-list2 ...) & jmp to mapf0
mapfxt:	@ exit
	restor	sv1, sv2, cnt		@ sv1 <- val-ls-or-#npo, sv2 <-dmy, cnt <-cnt, dts <-(..)
	eq	sv1, #npo		@ doing for-each?
	it	eq
	seteq	pc, cnt			@	if so,  return with npo
	b	adr_reverse		@ reverse val-list in sv1 and exit via cnt

	/* (for-each fun list1 list2 ...) */
	PRIMIT	"for-each", for_each, pfun, 1
	@ in:	sv1 <- fun
	@ in:	sv2 <- (list1 list2 ...)
	set	sv3, npo
	b	mapfor

	/* (force promise) */
	PRIMIT	"force", pfun, 1
	@ in:	sv1 <- promise
	set	sv2, null		@ sv2 <- '()
	b	adr__apl

/*------------------------------------------------------------------------------
@  II.H.6.     Standard Procedures
@  II.H.6.4.   control features SUPPORT:		cars, cdrs
@-----------------------------------------------------------------------------*/

.balign	4

cars:	@ return a list of cars	of the lists in sv3
	set	sv1, null		@ sv1 <- '() = initial result
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
	set	sv1, null		@ sv1 <- '() = initial result
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
	set	sv2, null		@ sv2 <- '() = initial result
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

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.6.	Input and Output
@	6.6.1.	ports:			call-with-input-file,
@					call-with-output-file,
@	6.6.3.	output:			newline
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (call-with-input-file string <port-model> proc) */
	PRIMIT	"call-with-input-file", cwif, pfun, 3
	@ in:	sv1 <- string
	@ in:	sv2 <- port-model or proc
	@ in:	sv3 <- proc or ()
	nullp	sv3			@ is port-model provided?
	itT	eq
	seteq	sv3, sv2		@	if not, sv3 <- proc
	seteq	sv2, null		@	if not, sv2 <- () (no port)
	save	sv1, sv3, cnt		@ dts <- (string proc cnt ...)
	call	adr_open_infile		@ sv1 <- file handle or 0
	set	sv2, sv1		@ sv2 <- file handle or 0
	restor	sv3, sv1		@ sv3 <- string, sv1 <- proc, dts <- (cnt ...)
	eq	sv2, #i0		@ unable to open?
	beq	cwifer			@	if so,  jump to report error
	list	sv2, sv2		@ sv2 <- (file-handle)
	save	sv2			@ dts <- ((file-handle) cnt ...)
	call	adr__apl		@ sv1 <- result of calling proc on port
	restor	sv1, cnt		@ sv1 <- (file-handle), cnt <- cnt, dts <- (...)
	set	sv4, 0x80 | true
	bl	adr_ioprfe
	b	adr_close_inprt		@ jump to close input port, return via cnt

cwifer:	@ error in call-with-input-file
	set	sv1, sv3		@ sv2 <- argument (fun or argl) that caused the error
	b	adr__err

	/* (call-with-output-file string <port-model> proc) */
	PRIMIT	"call-with-output-file", cwof, pfun, 3
	@ in:	sv1 <- string
	@ in:	sv2 <- port-model or proc
	@ in:	sv3 <- proc or ()
	nullp	sv3			@ is port-model provided?
	itT	eq
	seteq	sv3, sv2		@	if not, sv3 <- proc
	seteq	sv2, null		@	if not, sv2 <- () (no port)
	save	sv1, sv3, cnt		@ dts <- (string proc cnt ...)
	call	adr_open_outfile	@ sv1 <- file handle or 0
	set	sv2, sv1		@ sv2 <- file handle or 0
	restor	sv3, sv1		@ sv3 <- string, sv1 <- proc, dts <- (cnt ...)
	eq	sv2, #i0		@ unable to open?
	beq	cwofer			@	if so,  jump to report error
	list	sv2, sv2		@ sv2 <- (file-handle)
	save	sv2			@ dts <- ((file-handle) cnt ...)
	call	adr__apl		@ sv1 <- result of calling proc on port
	restor	sv1, cnt		@ sv1 <- (file-handle), cnt <- cnt, dts <- (...)
	set	sv2, null		@ sv2 <- '() = normal close mode
	b	adr_close_outprt	@ jump to close output port, return via cnt
cwofer:	@ error in call-with-output-file
	set	sv1, sv3		@ sv2 <- argument (fun or argl) that caused the error
	b	adr__err

	/* (newline <port> <reg> <n> ...) */
	PRIMIT	"newline", pfun, 0
	@ in:	sv1 <- (<port> <reg> <n> ...) or (((port <reg> <n> ...) . port-vector))
	@ out:	sv1 <- npo
	set	sv2, sv1		@ sv2 <- (<port> <reg> <n> ...)
	set	rvb, '\r
	raw2chr	sv1, rvb
	set	sv4, (0x02<<2)|i0
	b	adr_ioprfn		@ jump to write the newline (cr)


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/*------.-------.-------.-------.-------+
@	system utility  sub-environment	|
@-------.-------.-------.-------.------*/


	/* (defined? var . env) */
	PRIMIT	"defined?", defined, pfun, 1
	@ in:	sv1 <- var
	@ in:	sv2 <- (<env>)
	@ out:	sv1 <- #t/#f
	nullp	sv2
	itE	eq
	seteq	sv5, env
	carne	sv5, sv2
	set	rvb, sv1		@ rvb <- var
	set	rvc, null
	bl	bndene
	nullp	sv3
	b	adr_notfxt

	/* (lookup var . env) */
	PRIMIT	"lookup", pfun, 1
	@ in:	sv1 <- var
	@ in:	sv2 <- (<env>)
	@ out:	sv1 <- value of var in env
	nullp	sv2
	itE	eq
	seteq	sv5, env
	carne	sv5, sv2
	set	rvb, sv1		@ rvb <- var
	set	rvc, cnt
	b	bndene

	/* (link cvec) */
	PRIMIT	"link", pfun, 1
	@ in:	sv1 <- cvec = #(code-bytevector *asm-comp-link* *compile-syms* *long-jumps*)
	@ out:	sv1 <- linked code bytevector
	vcrfi	sv2, sv1, 0		@ sv2 <- code-bytevector
	save	sv2, sv1, cnt		@ dts <- (code-bytevector cvec cnt ...)
	@ link the symbols used by the compiled code
	vcrfi	sv5, sv1, 1		@ sv5 <- *asm-comp-link*
p_ln00:	@ loop over compiler symbols to link
	nullp	sv5			@ done linking compiler symbols?
	beq	p_lnlj			@	if so,  jump to link long jumps
	save	sv5			@ dts <- (syms-to-lnk cod-bv cvec cnt .)
	car	sv4, sv5		@ sv4 <- pos 1st sym to lnk (pos . key)
	cdr	sv4, sv4		@ sv4 <- sym key in *compile-syms* alist
	caddr	sv1, dts		@ sv1 <- cvec=#(cod *lnk* *syms* *jmps*)
	vcrfi	sv3, sv1, 2		@ sv3 <- *compile-syms*
p_ln01:	@ search loop for symbol name
	nullp	sv3			@ done scanning *compile-syms*?
	beq	adr__err		@	if so,  report error
	snoc	sv2, sv3, sv3		@ sv2 <- (k1.symnm1), sv3 <- rest *syms*
	car	rva, sv2		@ rva <- key1
	eq	rva, sv4		@ key found?
	bne	p_ln01			@	if not, jmp to chck next *syms*
	cdr	sv1, sv2		@ sv1 <- symbol-name (string)
	call	adr_str2sym		@ sv1 <- symbol (from name)
	restor	sv5			@ sv5 <- *link*, dts <- (cod cvec cnt .)
	snoc	sv4, sv5, sv5		@ sv4 <- sym to lnk (pos.key), sv5 <-rst
	car	sv4, sv4		@ sv4 <- pos of symbol in code (scm int)
	int2raw	rva, sv4		@ rva <- pos of symbol in code (raw int)
	car	sv2, dts		@ sv2 <- code bytevector
	str	sv1, [sv2, rva]		@ store symbol in code bytevector
	b	p_ln00			@ jump to link other symbols
p_lnlj:	@ link the long jumps
	cadr	sv1, dts		@ sv1 <- cvec
	vcrfi	sv5, sv1, 3		@ sv5 <- *long-jumps*
p_ln04:	@ loop
	nullp	sv5			@ done linking long jumps?
	beq	p_lnxt			@	if so,  jump to exit
	save	sv5			@ dts <- (long-jmps cod-bv cvec cnt ...)
	cdar	sv1, sv5		@ sv1 <- 1st lng jmp targt name (string)
	call	adr_str2sym		@ sv1 <- 1st lng jmp targt name (symbol)
	bl	bndchk			@ sv5 <- lng jmp addrs / direct fun tag

@ 050	set	rva, sv5		@ rva <- function code start address
	/* start of update for 070 */
	pntrp	sv5
	itE	eq
	seteq	rva, sv5		@ 	if so,  rva <- fun cod strt adrs
	lsrne	rva, sv5, #16		@ 	if not, rva <- fun cod strt adrs
	/* end of update for 070 */

	restor	sv5			@ sv5 <- lng jmps, dts <- (cod cvec ...)
	snoc	sv4, sv5, sv5		@ sv4 <- 1st lng jmp, sv5 <- rst lng jmp
	car	sv4, sv4		@ sv4 <- pos 1st lng jmp (scheme int)	
	int2raw	rvb, sv4		@ rvb <- pos 1st lng jmp (raw int)
	car	sv1, dts		@ sv1 <- code bytevector
	str	rva, [sv1, rvb]		@ store long jump in code bytevector
	b	p_ln04			@ jump to link other long jumps

p_lnxt:	@ done, return
	restor	sv1, sv2, cnt		@ sv1 <- cod-bv, sv2 <- cvec, cnt <- cnt
	set	pc,  cnt		@ return

/* ------070 linker, in scheme -------------------------------------------------
 (define (link cvec)
   (let ((code (vector-ref cvec 0)))
     ;; link the symbols used by the compiled code
     (map
      (lambda (lvar)
	(let ((n (car lvar))
	      (s (string->symbol (cdr (assq (cdr lvar) (vector-ref cvec 2))))))
	  (bytevector-u16-native-set! code n       (| (<< s 2) #x0f)) ; synt/var
	  (bytevector-u16-native-set! code (+ n 2) (<< s -14))))
      (vector-ref cvec 1))
     ;; link the long jumps
     (map
      (lambda (ljmp)
	(let* ((s (eval (string->symbol (cdr ljmp)) (interaction-environment)))
	       (a (@ s 14 32))
	       (b (if (not (number? a))
	              (address-of s 0) 
	              (let ((v #vu8(0 0 0 0))) (bytevector-u16-native-set! v 0 a) v))))
	(bytevector-copy! b 0 code (car ljmp) 4)))
      (vector-ref cvec 3))
     code))
----------------------------------------------------------------------------- */

/* ------050 linker, in scheme -------------------------------------------------
 (define (link cvec)
   (let ((code (vector-ref cvec 0)))
     ;; link the symbols used by the compiled code
     (map
      (lambda (lvar)
	(let ((n (car lvar))
	      (s (string->symbol (cdr (assq (cdr lvar) (vector-ref cvec 2))))))
	  (bytevector-u16-native-set!
	   code n (bitwise-ior (bitwise-arithmetic-shift s 2) #x0f)) ; synt/var
	  (bytevector-u16-native-set! code (+ n 2) (bitwise-arithmetic-shift s -14))))
      (vector-ref cvec 1))
     ;; link the long jumps
     (map
      (lambda (ljmp)
	(bytevector-copy!
	 (address-of (eval (string->symbol (cdr ljmp)) (interaction-environment)) 4)
	 0 code (car ljmp) 4))
      (vector-ref cvec 3))
     code))
----------------------------------------------------------------------------- */


	/* (unpack-above-heap obj) */
	PRIMIT	"unpack-above-heap", upah, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- result
	set	sv2, i1
_func_
p_upae:	@ [internal entry]
	vctrp	sv1
	bne	adr_unpack
	sav_rc	sv2
	call	adr_link
	restor	sv2, cnt
	b	adr_unpack

.ifndef exclude_lib_mod

	/* (libs) */
	PRIMIT	"libs", pfun, 0
	@ out:	sv1 <- list of library names
	vcrfi	sv5, glv, 12		@ sv5 <- libraries
	set	sv1, null
p_lib0:	nullp	sv5			@ reached end of libraries?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	save	sv1
	car	sv4, sv5		@ sv4 <- first lib
	car	sv2, sv4
	vcrfi	sv1, sv2, 0		@ sv1 <- first lib's priv sub-oba (sym)
	set	sv2, i1
	ldrb	rvb, [sv1, #0]
	sub	rvb, rvb, #47
	raw2int	sv3, rvb
	bl	subcpy
	restor	sv2
	cons	sv1, sv1, sv2
	cdr	sv5, sv5
	b	p_lib0			@	if not, jump to scan restof libs


  .ifdef LIB_TOP_PAGE

	/* (erase-libs) */
	PRIMIT	"erase-libs", erase_libs, pfun, 0
	@ out:	sv1 <- list of library names
	set	sv1, i0
	orr	sv1, sv1, #0x80000000
	list	sv1, sv1
	b	adr_erase

	/* (unpack-to-lib obj) */
	PRIMIT	"unpack-to-lib", uptl, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- result
	set	sv2, f0			@ sv2 <- 0.0	(scheme float)
	mvn	sv2, sv2		@ sv2 <- -1	(scheme int)
	b	p_upae

  .endif	@ .ifdef LIB_TOP_PAGE

.endif	@ do not exclude_lib_mod


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



