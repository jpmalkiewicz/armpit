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
		LIB_TOP_PAGE, SHARED_LIB_FILE, live_SD
		fast_eval_apply, fast_lambda_lkp, mark_and_sweep
		rw_funcs_in_scm, cortex, r3rs
		exclude_lib_mod, oba_above_heap, enable_a9_mpcore
		onboard_SDFT

*/

	SMBL	"#<?>", undef_


/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV base

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.1.	Primitive expression types:	quote, lambda, if, set!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		savrec, eval, lcons, bndchk, npofxt
@
@	Modified by (switches):		(none)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (quote expr) */
	PRIMIT	"quote", esntx, 1, oreturn, 0

	/* (lambda vars-list body) */
	PRIMIT	"lambda", sntx, 1
	@ in:	sv1 <- vars-list
	@ in:	sv2 <- (body)
	@ out:	sv1 <- [procedure env vars-list body]
	set	sv3, procedure		@ sv3 <- partial proc tag
	orr	sv3, sv3, #0x4000	@ sv3 <- full proc tag
  .ifdef fast_eval_apply
	@ count number of input vars and, if 1-3 & a proper list, set it in tag
	set	sv4, sv1
	set	rvb, 0
lmbdal:	pairp	sv4
	itT	eq
  	addeq	rvb, rvb, #0x0100
  	cdreq	sv4, sv4
  	beq	lmbdal
  	nullp	sv4
  	it	ne
	setne	rvb, 0
  	cmp	rvb, #0x0400
  	it	pl
	setpl	rvb, 0
	orr	sv3, sv3, rvb		@ sv3 <- full proc tag w/num vars if 1-3
  .endif
	tagwenv	sv1, sv3, sv1, sv2	@ sv1 <- proc = [tag vrs-lst (bod) env]
  .ifndef fast_lambda_lkp
	set	pc,  cnt		@ return
  .else
	@ do not do fast lookup inside libraries
	vcrfi	rva, glv, 14		@ rva <- lib-env or null
	nullp	rva			@ are we in normal (non-lib) mode?
	it	ne
	setne	pc,  cnt		@	if not, return
	@ do not do fast lookup if body is not in heap (would require body-copy)
	vcrfi	sv4, sv1, 1		@ sv4 <- body
	vcrfi	rva, glv, 18		@ rva <- cpu(n) heapbottom, pseudo-int
	bic	rva, rva, #3
	cmp	sv4, rva		@ is sv4 > heapbottom ?
	itT	pl
    .ifndef mark_and_sweep
	vcrfipl	rva, glv, 10		@	if so,  rva <- heaptop1
    .else
	vcrfipl	rva, glv, 1		@	if so,  rva <- heaptop
    .endif
	cmppl	rva, sv4		@	if so,  is RAMPTOP > sv4 ?
	it	mi
	setmi	pc,  cnt		@	if not, return
	sav_rc	sv1			@ dts <- (proc cnt ...)
	set	sv2, dts
	call	flblkp
flblxt:	@ exit
	restor	sv1, cnt
	set	pc,  cnt

_func_
flblkr:	@ continue to look for variables in body
	cdr	sv4, sv4
_func_
flblkp:	@ [entry] look for variables in body
	pairp	sv4
	it	ne
	setne	pc,  cnt
	car	sv3, sv4		@ sv3 <- 1st expr
	varp	sv3			@ is this whole expr just a variable?
	beq	flblkV			@	if so,  jump to that case
	pairp	sv3			@ is the expr a list (i.e. application)?
	bne	flblkr			@	if not, jump to process nxt expr
	car	sv1, sv3		@ sv1 <- car of expr
	varp	sv1			@ is car of expr a var?
	beq	flblkC			@	if so,  deal w/1st var in a list
	pairp	sv1
	bne	adr__err		@	if not, jump to error
	sav_rc	sv4
	set	sv4, sv3
	call	flblkp
	restor	sv4, cnt
	b	flblkr

_func_
flblkC:	@ expression (in sv3) is a list that starts with a var (in sv1)
	tst	sv1, #0xff000000
	beq	flblkq
flblkd:	@ go through contents of expr-list
	sav_rc	sv4
	set	sv4, sv3
	set	sv3, sv1
	call	flblkV
	restor	sv4, cnt
	b	flblkr

flblkq:	@ check for special built-in vars
	@ check to see if it is (xlkp ...) or (define) in which case don't
	@ go further with analysis
	eq	sv1, #variable_tag
	itT	ne
	ldrne	rva, =var_define
	eqne	sv1, rva
	it	eq
	seteq	dts, sv2
	beq	flblxt
	@ check to see if it is (lambda ...), (set! ...), (quote ...), ...
	ldr	rva, =var_lambda
	eq	sv1, rva
	itT	ne
	ldrne	rva, =var_set
	eqne	sv1, rva
	itT	ne
	ldrne	rva, =var_quote
	eqne	sv1, rva
	itT	ne
	ldrne	rva, =var_quasiquote
	eqne	sv1, rva
	beq	flblkz
	ldr	rva, =var_unquote
	eq	sv1, rva
	itT	ne
	ldrne	rva, =var_unqtsplc
	eqne	sv1, rva
	beq	flblkr
	b	flblkd

_func_
flblkz:	@ sv3 is (lambda ...), (set! ...), (quote ...) or (quasiquote ...)
	@ sv1 is lambda, set!, quote or quasiquote
	vecref	sv5, glv, 13		@ sv3 <- built-in env vector
	rgbf	rvc, sv1, 8, 15
	read	sv5, sv5, rvc, lsl #2	@ sv3 <- built-in sub-env vector
	lsr	rvc, sv1, #16		@ rvb <- offset in built-in sub-env
	ldr	sv5, [sv5, rvc, lsl #2]	@ sv3 <- symbol's binding value, or adrs
	cons	sv5, sv1, sv5
	set	sv1, variable_tag
	cons	sv5, sv1, sv5
	setcar	sv3, sv5
	b	flblkr

_func_
flblkV:	@ expression (in sv3) is just a variable, like x or version
	@ in (lambda (x) x) or (lambda (y) (write y) version)
	set	sv1, variable_tag
	cons	sv3, sv1, sv3
	setcar	sv4, sv3
	b	flblkr
	
  .endif	@ fast_lambda_lkp

	/* (if pred true-exp <false-exp>) */
	PRIMIT	"if", sntx, 1
	@ in:	sv1 <- pred
	@ in:	sv2 <- (true-exp <false-exp>)
	@ out:	sv1 <- result
	savrec	sv2			@ dts <- ((t-exp <f-exp>) env cnt ...)
	call	eval			@ sv1 <-  #t/#f frm eval sv1 in dflt env
	eq	sv1, #f			@ is predicate false?
	restor	sv1, env, cnt		@ sv1 <- (t-ex <f-ex>),env<-e,cnt<-c,dts
	it	eq
	pntrpeq	sv1
	it	eq
	cdreq	sv1, sv1		@	if so,  sv1 <- (<false-exp>)
	nullp	sv1			@ is there an expression to evaluate?
	it	eq
	seteq	pc,  cnt		@	if not, return with '()
	car	sv1, sv1		@ sv1 <- true-or-false-exp
	b	eval			@ jump to evaluate true-or-false-exp

	/* (set! var exp) */
	PRIMIT	"set!", set, sntx, 2
	@ in:	sv1 <- var
	@ in:	sv2 <- exp
	@ out:	sv1 <- '()
	savrec	sv1			@ dts <- (var env cnt ...)
	set	sv1, sv2		@ sv1 <- exp
	call	eval			@ sv1 <- val of exp, eval in dflt env
	set	sv4, sv1		@ sv4 <- val of exp saved against bndchk
	restor	sv1, env, cnt		@ sv1 <- var for bndenv,env<-e,cnt<-c,..
	bl	bndchk			@ sv1 <- bndng=(var.val),val or (), sv5
	eq	sv3, sv5		@ is sv1 immediate (no bndng or blt-in)?
	beq	adr__err		@	if so,  error (can't set)
	setcdr	sv3, sv4		@ bndng=(key . val),set cdr bndng to val
	b	adr_npofxt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.2.	Derived expression types
@	4.2.3.	sequencing:		begin
@	4.2.6.	quasiquotation:		unquote, unquote-splicing, quasiquote
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			
@							
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.3.	sequencing:		begin
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

	/* (begin . expr-list) */
	BNDVAR	"begin", (((sqnce - start_of_code)|lnkbit0) << 16)|(0 << 8)|sntx

@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.2.6.	quasiquotation:		unquote, unquote-splicing, quasiquote
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

	BNDVAR	"unquote", true
	BNDVAR	"unquote-splicing", unqtsplc, true

	/* (quasiquote expr-list) */
	PRIMIT	"quasiquote", sntx, 1
	@ in:	sv1 <- expr-list
	@ out:	sv1 <- result
	bl	typsv1			@ rva <- type tag of expr1 (sv1)
	eq	rva, #list_tag		@ is expr1 a list?
	it	ne
	setne	pc, cnt			@	if not, return
	set	sv5, null		@ sv5 <- '()
	list	sv5, sv5		@ sv5 <- (() . expr-list-result) = tail-ref
	save	env, sv5, cnt		@ dts <- (env (() . expr-list-result) cnt ...)
	set	sv3, sv1		@ sv3 <- expr-list
qsqt0:	@ build unquoted/spliced copy of expr-list
	nullp	sv3			@ done processing expr-list?
	beq	qsqtxt			@	if so,  jump to exit
	car	sv1, sv3		@ sv1 <- expr1
	ldr	sv4, =var_unquote	@ sv4 <- unquote
	eq	sv1, sv4		@ is expr1 an improper-list unquote expression?
	beq	qsqt3			@	if so,  jump to process that
	pntrp	sv1			@ is expr1 a pointer (i.e. a non-immediate)?
	bne	qsqt2			@	if not, jump to glue it in expr-list-result
	tagdp	sv1			@ is expr1 a tagged item (proc, string, ...)
	beq	qsqt2			@	if so,  jump to glue it in expr-list-result
	ratcpx	sv1			@ is expr1 a rational or complex
	beq	qsqt2			@	if so,  jump to glue it in expr-list-result
	car	sv2, sv1		@ sv2 <- 1st item of expr1 (expr1 is a list)
	ldr	sv4, =var_unquote	@ sv4 <- unquote
	eq	sv2, sv4		@ is expr1 a unquote expression?
	itT	ne
	ldrne	sv4, =var_unqtsplc	@	if not, sv4 <- unquote-splicing
	eqne	sv2, sv4		@	if not, is expr1 a unquote-splicing expression?
	bne	qsqt4			@	if not, jump to recursively unqot/splc lst in sv1
	@ evaluate item that is to be unquoted/spliced
	cadr	sv1, sv1		@ sv1 <- item to evaluate
	car	env, dts		@ env <- env
	save	sv5, sv3		@ dts <- (tail-rf expr-lst env (() . expr-lst-rslt) cnt .)
	call	eval			@ sv1 <- expr1-result
	restor	sv5, sv3		@ sv5 <- tl-rf, sv3<-exp-ls,dts<-(nv (().exp-lsrslt) cnt .)
	caar	sv2, sv3		@ sv2 <- unquote-or-unquote-splicing
	ldr	sv4, =var_unquote	@ sv4 <- unquote
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
	save	sv5, sv3, cnt		@ dts <- (tail-rf exp-lst cnt nv (() . exp-lst-res) cnt .)
	call	adr_quasiquote		@ sv1 <- unquoted-spliced expr-list
	restor	sv5, sv3, cnt		@ sv5 <- tl-rf,sv3<-exp-ls,dts<-(env (().exp-lsrslt) cnt .)
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
	restor	sv5			@ sv5 <- tail-ref, dts<-(env (().exp-lst-rslt) cnt ..)
	setcdr	sv5, sv1		@ store expr1-result at tail of sv5
qsqtxt:	@ extract result from stack and exit
	restor	env, sv1, cnt		@ env <- env, sv1 <- (().ex-ls-rs), cnt<-cnt, dts<-(...)
	cdr	sv1, sv1		@ sv1 <- expr-list-result
	set	pc, cnt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	4.	Expressions
@	4.3.	Macros
@	4.3.1.	binding constructs for syntactic keywords:
@					let-syntax, letrec-syntax
@	4.3.2.	Pattern language:	syntax-rules
@	4.3.c.	constants:		..., _
@	4.3.s.	support:		expand, match, substitute
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		savrec, eval, cons, save3, mkfrm, save,
@					mxpnd, sqnce, save2, sav_rc, list,
@					sav__c, synt_rules_synt, quote_synt,
@					quasiquote_synt, bndchk, bcons, save3,
@					save, error4, ellipsis, save2,
@					sav_rc, bcons, save3, list, cons, bndchk
@		6.3.3.	Symbols:	symstr, strsym (gensym)
@							
@
@	Modified by (switches):		CORE_ONLY
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/
	
.ifndef r3rs

	/* (let-syntax bindings-list exp1 exp2 ...) */
	PRIMIT	"let-syntax", let_syntax, sntx, 1
	@ in:	sv1 <- bindings-list
	@ in:	sv2 <- (exp1 exp2 ...)
	@ out:	sv1 <- result
	savrec	sv2			@ dts <- ((exp1 exp2 ...) env cnt ...)
	set	sv3, null		@ sv3 <- '() = initial vars-list
	set	sv4, null		@ sv4 <- '() = initial vals-list
slet0:	@ build lists of init vars and init uvals
	nullp	sv1			@ is bindings-list done?
	beq	slet1			@	if so, jump to continue
	snoc	sv1, sv5, sv1		@ sv1 <- 1st-binding, sv5 <- rest-of-bindings-list
	snoc	sv1, sv2, sv1		@ sv1 <- var1, sv2 <- (uval1)
	cons	sv3, sv1, sv3		@ sv3 <- updated vars-list
	cadr	env, dts		@ env <- env, restored
	save	sv5, sv3, sv4		@ dts <- (rst-bnd-ls nw-vrs-lst vls-ls (e1 e2 .) nv cnt .)
	car	sv1, sv2		@ sv1 <- uval1
	call	eval			@ sv1 <- val1
	restor	sv5, sv3, sv4		@ sv5 <- rst-bnds, sv3 <- new-vrs-l
					@ sv4 <- vls-lst, dts <- ((e1 e2 .) env cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- new-vals-list
	set	sv1, sv5		@ sv1 <- rest-bindings-list
	b	slet0
slet1:	@ extract built-lists and expr list from stack
	call	mkfrm			@ env <- extended environment
	restor	sv2			@ sv2 <- (exp1 exp2 ...), dts <- (env cnt ...)
	save	env			@ dts <- (extended-env env cnt ...)
	set	sv3, null
slet2:	@
	nullp	sv2
	beq	slet3
	snoc	sv1, sv4, sv2		@ sv1 <- exp1, sv2 <- (exp2 ...)
	car	env, dts		@ env <- extended-env
	save	sv3, sv4		@ dts <- (extndd-exps-lst (exp2 .) extndd-env env dts ..)
  .ifndef rw_funcs_in_scm
	call	adr_expand		@ sv1 <- extended expression1
  .else
	list	sv2, sv1
	ldr	sv1, =val_expand
	call	adr__apl		@ sv1 <- parsed expr with expanded macros, return via cnt
  .endif
	restor	sv3, sv4		@ sv3 <- extnd-exprs-list, sv4 <- (exp2 ...)
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
	restor	env, env, cnt
	set	sv1, sv2		@ sv1 <- expanded-exprs-list
	b	sqnce

	/* (letrec-syntax bindings-list exp1 exp2 ...) */
	PRIMIT	"letrec-syntax", letrec_syntax, sntx, 1
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
sletr2:	@ evaluate let-vals
	nullp	sv3			@ is bindings-list done?
	beq	sletr3			@	if so, jump to continue
	car	env, dts		@ env <- let-env
	snoc	sv1, sv3, sv3		@ sv1 <- binding1,	sv3 <- rest of bindings-list
	cadr	sv1, sv1		@ sv1 <- uval1
	save	sv4, sv3		@ dts <- (val-lst rst-b-lst let-env (exp1 ...) env cnt ...)
	call	eval			@ sv1 <- val1
	restor	sv4, sv3		@ sv4 <-old-vls,sv3 <-rst-bls,dts<-(ltnv ltbn (e1 .) cnt .)
	cons	sv4, sv1, sv4		@ sv4 <- (val1 ...) = updated vals list	
	b	sletr2			@ jump to continue building init vals list
sletr3:	@ keep going
	restor	env			@ env <- let-env, dts <- (let-bndngs-lst (exp1 ..) cnt ..)
	restor	sv3, sv1, cnt		@ sv3 <- let-bndngs-lst,sv1<-(exp1 .), cnt<-cnt, dts<-(..)
	@ reverse vals list
	set	sv5, null
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
	
	
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::
@
@	4.3.2.	Pattern language:	syntax-rules
@
@:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::.:::::::

	BNDVAR	"...", ellipsis,   true
	BNDVAR	"_",   underscore, true

	/* (syntax-rules literals rule1 ...) */
	PRIMIT	"syntax-rules", syntax_rules, sntx, 0
	@ in:	sv1 <- (literals rule1 ...)
	@ out:	sv1 <- (macro literals rule1 ...)
	snoc	sv2, sv3, sv1		@ sv2 <- ltrls, sv3 <- (rule1 rule2 ...)
	set	sv1, (1<<11)|proc
	tagwnul	sv1, sv1, sv2, sv3	@ sv1 <- [macro_tag ltrls (rule1 ..) ()]
	set	pc,  cnt


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.endif	@	ifndef r3rs

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	5.	Program Structure
@	5.2.	Definitions:		define
@	5.3.	syntax definitions:	define-syntax
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		lcons, lambda_synt, sav__c, save, eval,
@					npofxt
@
@	Modified by (switches):		(none)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/


	/* (define-syntax var rules) */
	BNDVAR	"define-syntax", define_syntax, val_define

	/* (define var exp) */
	PRIMIT	"define", sntx, 1
	@ in:	sv1 <- var or (var arg1 ...)
	@ in:	sv2 <- (exp)
	@ out:	sv1 <- '()
	@ keep:	none
	set	sv4, sv2		@ sv4 <- (exp)
	pntrp	sv1			@ is var a pointer?
	it	ne
	carne	sv4, sv4		@	if not, sv4 <- exp
	bne	defva0			@	if not, jump to continue
	snoc	sv1, sv2, sv1		@ sv1 <- var,	sv2 <- (arg1 ...)
	ldr	sv3, =var_lambda
	lcons	sv4, sv3, sv2, sv4	@ sv4 <- (lambda (args) exp)
defva0:	@ sv1 <- var, sv4 <- exp
  .ifndef exclude_lib_mod
	vcrfi	sv3, glv, 14
	nullp	sv3
	bne	deflib
  .endif
	sav__c				@ dts <- (cnt ...)
	call	adr__dfv		@ sv2 <- binding-for-var
	restor	cnt			@ cnt <- cnt, dts <- (...)
	set	sv1, sv4		@ sv1 <- exp
	evalsv1				@ sv1 <- val of exp
	setcdr	sv2, sv1
	b	adr_npofxt

.ifndef exclude_lib_mod

deflib:	@ define into a lib
	set	sv2, sv1		@ sv2 <- var, saved against eval
	set	sv1, sv4		@ sv1 <- exp
	set	sv4, env		@ sv4 <- env, saved against eval
	cdr	env, sv3
	evalsv1				@ sv1 <- val of exp
	set	env, sv4		@ env <- env, restored
	vecref	sv3, glv, 14
	cdr	sv3, sv3
	tst	sv2, #0xFF000000
	bne	adr_npofxt
	rgbf	rva, sv2, 8, 15		@ rva <- offset in built-in env
	read	sv3, sv3, rva, lsl #2	@ sv3 <- built-in sub-env vector
	lsr	rva, sv2, #16
	str	sv1, [sv3, rva, lsl #2]	@ set symbol's value
	b	adr_npofxt

.endif

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.1.	Equivalence predicates:	eq?, eqv?, equal?
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt, notfxt, save3, stsyeq, sav_rc,
@					cons, save
@
@	Modified by (switches):		(none)
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (eq? obj1 obj2) */
	PRIMIT	"eq?", eq, pfun, 2
	@ in:	sv1 <- obj1
	@ in:	sv2 <- obj2
	@ out:	sv1 <- #t/#f
	eq	sv1, sv2
	b	adr_boolxt		@ (T/F ...) and pop return stack

	/* (eqv? obj1 obj2) */
	BNDVAR	"eqv?", eqv, val_eq

	/* (equal? obj1 obj2) */
	PRIMIT	"equal?", equal, pfun, 2
	@ in:	sv1 <- obj1
	@ in:	sv2 <-  obj2
	@ out:	sv1 <- #t/#f
	eq	sv1, sv2		@ is obj1 = obj2?
	beq	adr_trufxt		@	if so,  exit with #t
	pairp	sv1
	beq	equlst
	pairp	sv2
	beq	adr_flsfxt
	pntrp	sv1
	it	eq
	pntrpeq	sv2
	bne	adr_flsfxt
	ldr	rva, [sv1, #-4]
	ldr	rvb, [sv2, #-4]
	eq	rva, rvb
	bne	adr_flsfxt
	tst	rva, #0x04
	bne	equszd
	ldr	rva, [sv1]		@ rva <- word 1 of x1, possible rat/cpx
	ldr	rvb, [sv2]		@ rvb <- word 1 of x2, possible rat/cpx
	eq	rva, rvb		@ are word 1 of x1 and x2 the same?
	b	adr_boolxt
	
equszd:	@ sized
	and	rva, rva, #0xff
	eq	rva, #vector_tag
	itT	ne
	ldrne	lnk, =adr_boolxt	@ set return link to boolxt
	ldrne	pc,  =stsyeq		@ jump to test strings/bytvctrs (long jump for cortex)
	@ check equality of vectors in sv1 and sv2
	veclen	sv5, sv1		@ sv5 <- size of obj1 (scheme int)
	cons	sv1, sv1, sv2		@ sv1 <- (vec1 . vec2)
	sav_rc	sv1			@ dts <- ((vec1 .  vec2) cnt ...)
veceq0:	@ are vectors on stack ((vec1 . vec2) ...) equal (sv5 starts as offset after last item)?
	izerop	sv5			@ done comparing vectors?
	beq	veceqx			@	if so,  jump to exit
	car	sv4, dts		@ sv4 <- (vec1 . vec2)
	snoc	sv1, sv2, sv4		@ sv1 <- vec1,  sv2 <- vec2
	decr	sv5, sv5		@ sv5 <- offset of item to compare
	vecref	sv1, sv1, sv5		@ sv1 <- item from vec2
	vecref	sv2, sv2, sv5		@ sv2 <- item from vec1
	save	sv5			@ dts <- (current-offset (vec1 vec2) cnt ...)
	call	adr_equal		@ sv1 <- #t/#f, from (equal sv1 sv2)
	restor	sv5			@ sv5 <- current-offset, dts <- ((vec1 vec2) cnt ...)
	eq	sv1,  #t		@ were items the same?
	beq	veceq0			@	if so,  jump to compare next items
veceqx:	cdr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,			dts <- (...)
	b	adr_boolxt		@ jump to exit with #t/#f

_func_
equlst:	@ are lists in sv1 and sv2 equal?
	pairp	sv2
	bne	adr_flsfxt
	snoc	sv1, sv4, sv1		@ sv1 <- (car obj1),		sv4 <- (cdr obj1)
	snoc	sv2, sv5, sv2		@ sv2 <- (car obj2),		sv5 <- (cdr obj2)
	save	sv4, sv5, cnt		@ dts <- ((cdr obj1) (cdr obj2) cnt ...)
	call	adr_equal		@ sv1 <- #t/#f, from (equal sv1 sv2)
	eq	sv1, #t			@ did cars match?
	restor	sv1, sv2, cnt		@ sv1 <- (cdr obj1), sv2<-(cdr obj2), cnt<-cnt, dts<-(...)
	bne	adr_boolxt		@	if not, exit with #f
	b	adr_equal		@ jump to test cdrs


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.2.	Pairs and list:		pair?, cons, car, cdr, set-car!, set-cdr!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt, notfxt, error4, npofxt, flsfxt,
@					i0fxt, infxt, list, save, sav_rc,
@					heapbottom
@		6.1.	Equivalence predicates:	
@					equal
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (pair? obj) */
	PRIMIT	"pair?", pair, pfun, 1
	@ in:	sv1 <-obj
	@ out:	sv1 <- #t/#f
	pairp	sv1
	b	adr_boolxt		@ return with #f/#t

	/* (cons item1 item2) */
	PRIMIT	"cons", pfun, 2
	@ in:	sv1 <- item1
	@ in:	sv2 <- item2
	@ out:	sv1 <- (item1 . item2)
	cons	sv1, sv1, sv2		@ sv1 <- (item1 . item2)
	set	pc,  cnt		@ return with (item1 . item2)

	/* (car list) */
	PRIMIT	"car", pfun, 1
	@ in:	sv1 <- list
	@ out:	sv1 <- car of list
	pairp	sv1
	itT	eq
	careq	sv1, sv1
	seteq	pc,  cnt
	b	adr__err		@ jump to signal error

	/* (cdr list) */
	PRIMIT	"cdr", pfun, 1
	@ in:	sv1 <- list
	@ out:	sv1 <- cdr of list
	pairp	sv1
	itT	eq
	cdreq	sv1, sv1
	seteq	pc,  cnt
	b	adr__err		@ jump to signal error

	/* (set-car! pair obj) */
	PRIMIT	"set-car!", setcar, pfun, 2
	@ in:	sv1 <- pair
	@ in:	sv2 <- obj
	@ out:	sv1 <- npo
	pairp	sv1
	itT	eq
	setcareq sv1, sv2		@ store obj in car of pair (side-effect)
	beq	adr_npofxt		@ return with npo
	b	adr__err		@ jump to signal error

	/* (set-cdr! pair obj) */
	PRIMIT	"set-cdr!", setcdr, pfun, 2
	@ in:	sv1 <- pair
	@ in:	sv2 <- obj
	@ out:	sv1 <- npo
	pairp	sv1
	itT	eq
	setcdreq sv1, sv2		@ store obj in car of pair (side-effect)
	beq	adr_npofxt		@ return with npo
	b	adr__err		@ jump to signal error

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.3.	Symbols:		symbol?, symbol->string, string->symbol
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt, scmenv, subcpy, stsyeq,
@					run_no_irq,
@					run_normal, I2C0ADR, clngc, error4
@
@	Modified by (switches):		cortex, oba_above_heap, mark_and_sweep
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (ers? object) (external representation of a symbol?) */
	PRIMIT	"ers?", ers, efun, 1, otypchk, iers

	/* (symbol? object) */
	PRIMIT	"symbol?", symbol, efun, 1, otypchk, ivar

	/* (symbol->string symbol) */
	PRIMIT	"symbol->string", sym2str, pfun, 1
	@ in:	 sv1 <- symbol-id
	@ out:	 sv1 <- string
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
	caareq	sv1, sv3		@	if so,  sv1 <- symbol's printed rep (address)
	beq	symst2			@	if so,  jump to exit with symbol as string
	cdr	sv3, sv3		@ sv3 <- rest of obarray
	b	symst0			@ continue scanning obarray

symst1:	@ symstr for built-in symbol (or library)
	tst	sv1, #0xFF000000	@
	bne	symst3
	vcrfi	sv2, glv, 14		@ sv1 <- lib built-in env vec (in case we're within parse)
	pairp	sv2
	itE	eq
	careq	sv2, sv2
	vcrfine	sv2, glv, 17		@	if not, sv1 <- built-in obarray
	and	rva, sv1, #0x7f00	@ rva <- offset in built-in env, shifted
	lsr	rva, rva, #6		@ rva <- offset in built-in env
	veclen	rvb, sv2
	bic	rvb, rvb, #3
	cmp	rva, rvb
	bpl	symst3

	@ *!*!*!*!*!*!*!*!*!* START-NEW

	@ go to var's index in compressed obarray
	lsr	rvc, sv1, #16		@ rvc <- index in sub-oba's string/symbol
	ldr	sv1, [sv2, rva]		@ sv1 <- built-in sub-oba string/symbol
	strlen	sv3, sv1
	eq	sv3, #i0
	beq	symst3
	set	rvb, 0
symst6:	ldrb	rva, [sv1, rvb]
	sub	rva, rva, #48
	add	rvb, rvb, #1
	eq	rvc, #0			@ done?
	beq	symst7
	add	rvb, rvb, rva
	cmp	rvb, sv3, lsr #2
	bpl	symst3
	sub	rvc, rvc, #1
	b	symst6
	
symst7:	@ extract symbol
	add	rva, rva, rvb
	raw2int	sv2, rvb
	raw2int	sv3, rva
	set	lnk, cnt
	b	subcpy

	@ *!*!*!*!*!*!*!*!*!* END-NEW

symst3:	@ undefined symbol
	ldr	sv1, =undef_		@ sv1 <- undefined symbol
symst2:	@ finish up: copy printed rep into string and return
	set	sv2, i0			@ sv2 <- pos of 1st char, adjusted for header (scheme int)
	strlen	sv3, sv1		@ sv3 <- pos after last char (scheme int)
	set	lnk, cnt
	b	subcpy


	/* (string->symbol string) */
	PRIMIT	"string->symbol", str2sym, pfun, 1
	@ in:	 sv1 <- string
	@ in:	 sv1 <- symbol-id
	@ out:	 sv3 <- result indicator: null if new sym was interned, else #t
	vcrfi	sv4, glv, 14		@ sv4 <- library-built-in env vector
	pairp	sv4
	itE	eq
	careq	sv4, sv4
	vecrefne sv4, glv, 17		@	if not, sv4 <- built-in obarray
	veclen	sv5, sv4		@ sv5 <- size of built-in oba (sch int)
strsy0:	@
	sub	sv5, sv5, #4

	@ *!*!*!*!*!*!*!*!*!* START-NEW

	@ search a compressed sub-obarray
	@ preserves:	sv1, sv5
	bic	rvb, sv5, #1		@ rvb <- sub-oba index
	read	sv4, sv4, rvb		@ sv4 <- sub-oba string/symbol
	strlen	sv2, sv1
	strlen	sv3, sv4
	eq	sv3, #i0
	beq	strsyM
	bic	rva, sv5, #3
	set	sv5, variable_tag
	orr	sv5, sv5, rva, lsl #6
	set	rvc, 0
strsy4:	@ loop
	read8	rva, sv4, rvc
	sub	rva, rva, #48
	add	rvc, rvc, #1
	add	rvc, rvc, rva
	eq	rva, sv2, lsr #2
	bne	strsy3
strsy9:	@ symbol and string have the same length, check if same chars too
	eq	rva, #0
	beq	strsy8
	sub	rva, rva, #1
	sub	rvc, rvc, #1
	read8	rvb, sv1, rva
	orr	sv2, sv2, rvb, lsl #24
	read8	rvb, sv4, rvc
	eq	rvb, sv2, lsr #24
	bic	sv2, sv2, #0xff000000
	beq	strsy9
	sub	rvc, rvc, rva
	add	rvc, rvc, sv2, lsr #2
strsy3:	@ go to next symbol in sub-oba
	cmp	rvc, sv3, lsr #2
	it	mi
	addmi	sv5, sv5, #(1 << 16)
	bmi	strsy4
	and	rva, sv5, #0xff00
	lsr	rva, rva, #6
	orr	sv5, rva, #i0
	b	strsyM

strsy8:	@ symbol found, build ID and exit
	@ (see also strsy7)
	set	sv3, true		@ non-library
	set	sv1, sv5
	set	pc,  cnt

	@ *!*!*!*!*!*!*!*!*!* END-NEW

strsyM:	vcrfi	sv4, glv, 14		@ sv4 <- library-built-in env vector
	eq	sv5, #i0		@ did we just scan the last (0th) sub-env
	it	eq
	nullpeq	sv4			@	if so,  are we in normal parse mode (non-lib)
	beq	strsyu			@	if so,  jump to scan user obarray
	eq	sv5, #i0		@ just scand last (0th) sub-env (if so, we're in lib mode)
	beq	strsy6			@	if so,  jump to intern symbol in lib-env
	nullp	sv4			@ are we in normal (non-parse) mode?
	it	eq
	vcrfieq	sv4, glv, 17		@	if so,  sv4 <- built-in obarray
	beq	strsy0			@	if so,  jump back to scan next sub-env
	vcrfi	rva, glv, 15		@ rva <- library-parse/parse-export-mode
	eq	rva, #i1		@ are we in library-parse-export-mode?
	it	ne
	carne	sv4, sv4
	bne	strsy0			@	if not, jump back to scan next sub-env

strsy6:	@ intern string as new symbol in private/public lib-env
	@ sv1 <- input string
	@ sv5 <- sub-env position - 1 (scheme int)
	@ sv4 <- (lib-oba . lib-env)
  .ifdef exclude_lib_mod
	b	adr__err
  .else
	@ extend lib-obarray
	car	sv3, sv4		@ sv3 <- this-lib-oba
	bic	rvb, sv5, #1		@ rvb <- offset to sub-oba
	ldr	sv3, [sv3, rvb]		@ sv3 <- sub-oba
	strlen	rva, sv3
	strlen	rvb, sv1		@ rva <- position after last char (scheme int)
	add	rvb, rvb, #3
	add	sv2, rva, rvb
	lsr	rvb, sv2, #2
	bl	adr__alo
	lsl	rvc, sv2, #6
	orr	rvc, rvc, #symbol_tag
	str	rvc, [rva, #-4]
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv2, rva, rvb		@ sv2 <- adrs of obj (sym), [*commit vector destination*]
	orr	fre, rva, #0x02		@ fre <- updtd & de-rsrvd, [*restart critical instruction*]
	@ copy lib-obarray symbols
	strlen	rvc, sv3
	lsr	rvb, rvc, #2
strs60:	subs	rvb, rvb, #1
	itT	pl
	ldrbpl	rva, [sv3, rvb]
	strbpl	rva, [sv2, rvb]
	bpl	strs60
	@ add new symbol
	lsr	rvb, rvc, #2
	ldr	rvc, [sv1, #-4]
	lsr	rvc, rvc, #8
	add	rva, rvc, #48
	strb	rva, [sv2, rvb]
	add	rvb, rvb, rvc
strs61:	subs	rvc, rvc, #1
	ldrb	rva, [sv1, rvc]
	strb	rva, [sv2, rvb]
	sub	rvb, rvb, #1
	bne	strs61
	@ update lib-sub-obarray in glv, 14
	car	sv3, sv4		@ sv3 <- this-lib-oba
	bic	rvb, sv5, #1		@ rvb <- offset to sub-oba
	str	sv2, [sv3, rvb]		@ set lib sub-oba in lib-obarray
	@ extend lib-sub-env, copy from prior
	cdr	sv3, sv4		@ sv3 <- this-lib-env
	bic	rvb, sv5, #1		@ rvb <- offset to sub-env
	ldr	sv3, [sv3, rvb]		@ sv3 <- lib sub-env
	veclen	sv2, sv3
	add	sv2, sv2, #4
	bic	rvb, sv2, #3
	bl	adr__alo
	lsl	rvc, sv2, #6
	orr	rvc, rvc, #vector_tag
	str	rvc, [rva, #-4]
	bic	rvc, sv2, #3
	sub	rvc, rvc, #4
	set	sv1, t
	str	sv1, [rva, rvc]
strs62:	subs	rvc, rvc, #4
	itT	pl
	ldrpl	sv1, [sv3, rvc]
	strpl	sv1, [rva, rvc]
	bpl	strs62
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv2, rva, rvb		@ sv2 <- adrs of obj (sym), [*commit vector destination*]
	orr	fre, rva, #0x02		@ fre <- updtd & de-rsrvd, [*restart critical instruction*]
	@ update lib-sub-env in glv, 14
	cdr	sv3, sv4		@ sv3 <- this-lib-env
	bic	rvb, sv5, #1		@ rvb <- offset to sub-env
	str	sv2, [sv3, rvb]		@ set lib sub-env in lib-env
	@ set rvc properly then jump to construct built-in symbol id and exit
	veclen	rvc, sv2
	bic	rvc, rvc, #0x03
	sub	rvc, rvc, #4
	set	sv3, null		@ sv3 <- () == new symbol interned flag
	@ construct built-in symbol id and exit 
	set	rvb, variable_tag
	bic	rva, sv5, #1
	orr	sv1, rvb, rva, lsl #6
	orr	sv1, sv1, rvc, lsl #14
	set	pc,  cnt

  .endif @	exclude_lib_mod

strsyu:	@ look for symbol in user obarray
	swi	run_prvlgd		@ privil, no irq, for hptp2mpu, mrc p15
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
strsxt:	@ return
	swi	run_normal              @ enable interrupts (user mode)
	set	sv3, true
	set	pc,  cnt

strsy2:	@ make a new symbol and id for string
	strlen	sv3, sv1		@ sv3 <- position after last char (scheme int)
  .ifndef oba_above_heap
	set	sv2, i0			@ sv2 <- pos of 1st char, adjstd for header (sch int)
	bl	subcpy			@ sv1 <- copy-of-string
  .else
	add	sv3, sv3, #(20<<2)	@ sv3 <- num chars + space for cons cell
	bl	ahpalo			@ rva <- obj insertion point or #f
	eq	rva, #f			@ above-heap alloc successful?
	beq	stsyer			@	if not, jump to exit with error
  .endif
  .ifndef enable_a9_mpcore
	set	rvb, I2C0ADR		@	if so,  rvb <- address of mcu-id
  .else
	mrc	p15, 0, rvb, c0, c0, 5	@ rva <- Multiproc Affinity reg, MPIDR
	and	rvb, rvb, #3
	ldr	rvc, =MP_mat
	add	rvb, rvc, rvb, lsl 5
	read	rvb, rvb, #0x1c		@ rvb <- cpun mcu-id adrs from I2C0ADR_n
  .endif

/* ---------------------------------- 

    here, we may set var IDs in descending order

---------------------------------- */

        vcrfi	sv4, glv, 8             @ sv4 <- current obarray
	/* ascending var-ID order * /
	set	rvc, #variable_tag	@ rvc <- empty var tag
	nullp	sv4			@ is obarray empty?
	itTE	eq
	ldrbeq	rvb, [rvb]		@	if so,  rvb <- mcu-id from i2c0adr (raw int)
	orreq	sv2, rvc, rvb, LSL #23	@	if so,  sv2 <- blank symbol id
	cdarne	sv2, sv4		@	if not, sv2 <- sym-id of top var
	add	sv2, sv2, #0x0100	@ sv2 <- new var-ID
	/**/
	/* descending var-ID order */
	nullp	sv4			@ is obarray empty?
	itTTT	eq
	seteq	rvc, variable_tag	@ rvc <- empty var tag
	orreq	rvc, rvc, #0x0000ff00
	orreq	rvc, rvc, #0x00ff0000
	ldrbeq	rvb, [rvb]		@	if so,  rvb <- mcu-id from i2c0adr (raw int)
	itE	eq
	orreq	sv2, rvc, rvb, lsl #23	@	if so,  sv2 <- blank symbol id
	cdarne	sv2, sv4		@	if not, sv2 <- sym-id of top var
	it	ne
	subne	sv2, sv2, #0x0100	@ sv2 <- new var-ID
	/**/
	ldr	rvc, [sv1, #-4]		@ rvc <- new symbol's size tag
	bic	rvc, rvc, #0xff		@ rvc <- new symbol's tag without type info
	orr	rvc, rvc, #symbol_tag	@ rvc <- new symbol's tag with type set to symbol
  .ifndef oba_above_heap
	str	rvc, [sv1, #-4]		@ sv1 <- string-as-symbol (updated tag)
	bcons	sv1, sv1, sv2, sv4	@ sv1 <- updt oba = ((sym.newid).oldoba)
  .else
	set	sv5, sv1
	set	sv1, rva
	add	rvb, rva, #8		@ rvb <- address of cell for new binding
	stmia	rva!, {rvb,sv4}
	add	rvb, rva, #12		@ rvb <- address of symbol-copy (4B ali)
	stmia	rva!, {rvb,sv2,rvc}
	lsr	rvc, rvc, #8		@ rvc <- number of bytes in symbol's name
	add	rvc, rvc, #3		@ rvc <- number of bytes in symbol's name + 3 for alignment
	bic	rvc, rvc, #3		@ rvc <- number of bytes in symbol's name, word aligned
stsylp:	subs	rvc, rvc, #4		@ rvc <- offset of next word to copy, is it the last one?
	ldr	rvb, [sv5, rvc]		@ rvb <- word from symbol
	str	rvb, [rva, rvc]		@ store it in destination
	bne	stsylp			@ 	if not, jump back to keep copying
  .endif
	vcsti	glv, 8, sv1             @ update obarray in global vector
	cdar	sv1, sv1		@ sv1 <- new obarray symid
	b	strsxt			@ return (enable ints, etc...)

  .ifdef oba_above_heap
stsyer:	@ signal error (out of memory)
	swi	run_normal
	b	adr__err	
  .endif

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.4.	Characters:		char?, char=?, char<?, char>?, char<=?,
@					char>=?, char->integer, integer->char
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (char? object) */
	PRIMIT	"char?", char, efun, 1, otypchk, ichr

	/* (char=? char1 char2) */
	PRIMITi	chareq, pfun, 2 ; .ascii "char=?" ; ENDi
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	eq	sv1, sv2
	b	adr_boolxt

	/* (char<? char1 char2) */
	PRIMIT	"char<?", charlt, pfun, 2
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	cmp	sv1, sv2
	b	chrgt0

	/* (char>? char1 char2) */
	PRIMIT	"char>?", chargt, pfun, 2
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	cmp	sv2, sv1
chrgt0:	itE	mi
	setmi	sv1, true
	setpl	sv1, false
	set	pc,  cnt

	/* (char<=? char1 char2) */
	PRIMITi	charle, pfun, 2 ; .ascii "char<=?" ; ENDi
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	cmp	sv2, sv1
	b	chrge0

	/* (char>=? char1 char2) */
	PRIMITi	charge, pfun, 2 ; .ascii "char>=?" ; ENDi
	@ in:	sv1 <- char1
	@ in:	sv2 <- char2
	@ out:	sv1 <- #t/#f
	cmp	sv1, sv2
chrge0:	itE	pl
	setpl	sv1, true
	setmi	sv1, false
	set	pc,  cnt

	/* (char->integer char) */
	PRIMIT	"char->integer", char2int, pfun, 1
	@ in:	sv1 <- char
	@ out:	sv1 <- int
	chr2raw	rva, sv1
	raw2int	sv1, rva
	set	pc,  cnt

	/* (integer->char int) */
	PRIMIT	"integer->char", int2char, pfun, 1
	@ in:	sv1 <- int
	@ out:	sv1 <- char
	int2raw	rva, sv1
	raw2chr	sv1, rva
	set	pc,  cnt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.5.	Strings:		string?, make-string, string-length,
@					string-ref, string-set!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt, zmaloc, subcpy
@		6.1.	Equivalence predicates:
@					equal
@		6.3.4.	Characters:	ichceq, ichrlt, ichrgt, ichrle,
@					ichrge, ichclt,
@					ichcgt, ichcle, ichcge
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (string? object) */
	PRIMIT	"string?", string, efun, 1, otypchk, istr

	/* (make-string k <char>) */
	PRIMIT	"make-string", make_string, pfun, 2
	@ in:	sv1 <- k
	@ in:	sv2 <- char or '()
	@ out:	sv1 <- string
	set	sv3, sv1		@ sv3 <- size of string to allocate
	straloc	sv1, sv3		@ sv1 <- allocated string of size sv1
	nullp	sv2			@ was fill specified?
	itE	eq
	seteq	rvb, 0			@	if not, rvb <- 0
	lsrne	rvb, sv2, #8
fill8:	@ [internal entry]
	and	rvb, rvb, #0xff
	orr	rvb, rvb, rvb, lsl #8
	orr	rvb, rvb, rvb, lsl #16
	int2raw	rva, sv3
	add	rva, rva, #3
	bic	rva, rva, #3
fill8l:	subs	rva, rva, #4
	itT	pl
	strpl	rvb, [sv1, rva]
	bpl	fill8l
	set	pc,  cnt

	/* (string-length string) */
	PRIMIT	"string-length", string_len, pfun, 1
	@ in:	sv1 <- string
	@ out:	sv1 <- int
	strlen	sv1, sv1		@ sv1 <- string length (scheme int)
	set	pc,  cnt

	/* (string-ref string k) */
	PRIMIT	"string-ref", string_ref, pfun, 2
	@ in:	sv1 <- string
	@ in:	sv2 <- k
	@ out:	sv1 <- char
	strref	sv1, sv1, sv2		@ sv1 <- character (scheme char)
	set	pc,  cnt

	/* (string-set! string k char) */
	PRIMIT	"string-set!", string_set, pfun, 3
	@ in:	sv1 <- string
	@ in:	sv2 <- k
	@ in:	sv3 <- char
	@ out:	sv1 <- string
	strset	sv1, sv2, sv3		@ sv1 <- string updated with char
	set	pc,  cnt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.3.	Other Data Types
@	6.3.6.	Vectors:		vector?, make-vector, vector-length,
@					vector-ref, vector-set!
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt, npofxt, zmaloc, cons
@		6.3.2.	Pairs and lists:
@					length
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (vector? object) */
	PRIMIT	"vector?", vector, efun, 1, otypchk, ivec

	/* (make-vector size <fill>) */
	PRIMIT	"make-vector", make_vec, pfun, 1
	@ in:	sv1 <- size
	@ in:	sv2 <- (<fill>)
	@ out:	sv1 <- vector == #(fill fill ...)
	@ keep:	sv4-sv5
	@ allocate memory for object
	set	sv3, sv1		@ sv3 <- size (saved)
	bic	rvb, sv1, #3		@ rvb <- #bytes to allocate for data
	bl	adr__alo		@ rva <- obj adrs (sym-taggd), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- obj adrs (sym), [*commit vector dest*]
	orr	fre, rva, #0x02		@ fre <- updated & de-reserved, [*restart crit instr*]
	@ clear contents of object (for gc)
	set	rva, null		@ rva <- '(), initial neutral value
makve0:	subs	rvb, rvb, #4		@ rvb <- offset of next item, is it zero or positive?
	it	pl
	strpl	rva, [sv1, rvb]		@	if so,  store item in vector
	bpl	makve0			@	if so,  jump to keep storing neutral val in vector
	@ update tag to vector
	lsl	rva, sv3, #6		@ rva <- vector size, shifted in place
	orr	rva, rva, #vector_tag	@ rva <- sized vector tag
	str	rva, [sv1, #-4]		@ set sized vector tag in vector
	@ store fill items in vector
	pntrp	sv2			@ is fill item non-null?
	it	eq
	careq	sv2, sv2		@	if so,  sv2 <- fill item (else sv2 <- scheme_null)
vecfil:	@ [internal entry]
	veclen	sv4, sv1		@ sv4 <- number of items (scheme int)
	bic	rvb, sv4, #0x03
vecfi0:	subs	rvb, rvb, #4		@ done filling vector?
	it	pl
	strpl	sv2, [sv1, rvb]		@ store fill item in vector
	bpl	vecfi0			@ jump to continue
	set	pc,  cnt		@ exit

	/* (vector-length vector) */
	PRIMIT	"vector-length", vector_len, pfun, 1
	@ in:	sv1 <- vector
	@ out:	sv1 <- int
	veclen	sv1, sv1		@ sv1 <- vector length (scheme int)
	set	pc,  cnt

	/* (vector-ref vector k) */
	PRIMIT	"vector-ref", vector_ref, pfun, 2
	@ in:	sv1 <- vector
	@ in:	sv2 <- k
	@ out:	sv1 <- item from vector
	vecref	sv1, sv1, sv2
	set	pc,  cnt

	/* (vector-set! vector k item) */
	PRIMIT	"vector-set!", vector_set, pfun, 3
	@ in:	sv1 <- vector
	@ in:	sv2 <- k
	@ in:	sv3 <- item
	@ out:	sv1 <- #npo
	vecset	sv1, sv2, sv3
	b	adr_npofxt		@ return with npo

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.4.	control features:	procedure?, apply, call/cc,
@					call-with-current-continuation, values,
@					call-with-values, dynamic-wind
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt, save, list, apply, save3,
@					bndchk, winders_var,
@					sav_rc, eval, savrec, bcons, cons
@
@	Modified by (switches):		CORE_ONLY
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (procedure? object) */
	PRIMIT	"procedure?", procedure, pfun, 1
	@ in:	sv1 <- object
	@ out:	sv1 <- #t/#f
	execp	sv1
	b	adr_boolxt

	/* (syntax? object) */
	PRIMIT	"syntax?", syntax, pfun, 1
	@ in:	sv1 <- object
	@ out:	sv1 <- #t/#f
	syntaxp	sv1
	b	adr_boolxt

	/* (macro? object) */
	PRIMIT	"macro?", macro, pfun, 1
	@ in:	sv1 <- object
	@ out:	sv1 <- #t/#f
	macrop	sv1
	b	adr_boolxt

	/* (apply fun arg1 ... args) */
	PRIMIT	"apply", pfun, 1
	@ in:	sv1 <- fun
	@ in:	sv2 <- argl
	@ out:	sv1 <- result
	set	sv4, sv2		@ sv4 <- (arg1 arg2 ... args)
	set	sv3, null		@ sv3 <- '()
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
	b	adr__apl

	/* (call-with-current-continuation procedure) */
	BNDVAR	"call-with-current-continuation", cwcc, val_callcc

	/* (call/cc procedure) */
	PRIMIT	"call/cc", callcc, pfun, 1
	@ in:	sv1 <- procedure
	@ out:	sv1 <- result of applying procedure on current continuation
	set	sv2, sv1		@ sv2 <- procedure, saved against bndchk
	ldr	sv1, =var__winders
	bl	bndchk			@ sv3 <- binding for winders, or null
	pntrp	sv3			@ was a binding found?
	itE	eq
	seteq	sv1, sv5		@	if so,  sv1 <- binding value = winders-list
	setne	sv1, null		@	if not, sv1 <- null
	set	sv3, (1<<15)|procedure	@ sv3 <- continuation tag
	cons	sv4, cnt, dts		@ sv4 <- (cnt ...)
	tagwenv	sv3, sv3, sv1, sv4	@ sv3 <- [#cont winders-lst (cnt ...) env] = continuation
	set	sv1, sv2		@ sv1 <- procedure
	list	sv2, sv3		@ sv2 <- (continuation)
	b	adr__apl

.ifndef r3rs

	/* (values obj ...) */
	PRIMIT	"values", efun, 0, oreturn, null

	/* (call-with-values producer consumer) */
	PRIMIT	"call-with-values", callwvalues, pfun, 2
	@ in:	sv1 <- producer
	@ in:	sv2 <- consumer
	list	sv1, sv1		@ sv1 <- (producer) = expr
	sav_rc	sv2			@ dts <- (consumer cnt ...)
	call	eval			@ sv1 <- values from producer
	set	sv2, sv1		@ sv2 <- values
	restor	sv1, cnt		@ sv1 <- consumer, cnt <- cnt, dts <- (...)
	pntrp	sv2			@ are values in a list?
	it	ne
	nullpne	sv2			@	if not, is values = '()?
	beq	adr__apl		@	if so,  jump to apply consumer to values
	list	sv2, sv2		@ sv2 <- (value)
	b	adr__apl		@ jump to apply consumer to value

	/* (dynamic-wind before thunk after) */
	PRIMIT	"dynamic-wind", dynamicwind, pfun, 3
	@ in:	sv1 <- before
	@ in:	sv2 <- thunk
	@ in:	sv3 <- after
	savrec	sv2			@ dts <- (thunk env cnt ...)
	bcons	dts, sv1, sv3, dts	@ dts <- ((before . after) thunk env cnt ...)
	set	sv2, null		@ sv2 <- ()
	call	adr__apl		@ sv1 <- result of applying before to '()
	caddr	env, dts		@ env <- saved env
	ldr	sv1, =var__winders	@ sv1 <- winders_var for bndchk
	set	sv4, sv1		@ sv4 <- winders_var, saved for later
	bl	bndchk			@ sv3 <- bndng for _winders or #t, sv5 <- val of _winders
	set	sv1, sv3		@ sv1 <- binding for _winders or #t
	restor	sv3, sv2		@ sv3 <- (bfr . aftr), sv2 <- thnk, dts <- (env cnt ...)
	eq	sv1, #t			@ winders found?
	bne	dynwn2			@	if so, jump to continue
	@ here, we could (define _winders '()) into current env
	@ if _winders had not been already placed in (interaction-environment)
	@ but _winders is there so there's nothing to do here.
	@ So, report an error in case something went wrong somewhere.
	b	adr__err
dynwn2:	@ store
	cons	sv3, sv3, sv5		@ sv3 <- ((before . after) . _winders)
	setcdr	sv1, sv3		@ sv1 <- (wndrs_vr . ((befr . aftr) . _wndrs)) updt bndng
	set	sv1, sv2		@ sv1 <- thunk
	set	sv2, null		@ sv2 <- ()
	call	adr__apl		@ sv1 <- result of applying dyn-wind thunk to ()
	car	env, dts		@ env <- env, restored
	save	sv1			@ dts <- (result env cnt ...)
	ldr	sv1, =var__winders
	bl	bndchk			@ sv3 <- binding for _winders, sv5 <- _winders list
	snoc	sv2, sv5, sv5		@ sv2 <- first winder, sv5 <- rest-of-winders
	setcdr	sv3, sv5		@ sv3 <- ((before . after) . rest-of-winders)
	cdr	sv1, sv2		@ sv1 <- after
	set	sv2, null		@ sv2 <- ()
	call	adr__apl		@ sv1 <- result of applying after to ()
	restor	sv1, env, cnt		@ sv1 <- result, env <- env, cnt <- cnt, dts <- (...)
	set	pc,  cnt		@ return

.endif

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	6.	Standard Procedures
@	6.5.	eval:			eval, scheme-report-environment,
@					null-environment,
@					interaction-environment
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:			core:	eval, error4
@
@	Modified by (switches):		none
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (eval expr env) */
	PRIMIT	"eval", pfun, 2
	@ in:	sv1 <- expr
	@ in:	sv2 <- env
	@ out:	sv1 <- result
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

	/* (scheme-report-environment version) */
	PRIMIT	"scheme-report-environment", report_env, pfun, 1
	@ in:	sv1 <- version
	@ out:	sv1 <- environment
  .ifdef r3rs
	eq	sv1, #0x0d		@ is version = 3 (scheme int)?
  .else
	eq	sv1, #0x15		@ is version = 5 (scheme int)?
  .endif
	itT	eq
	seteq	sv1, null		@	if so,  sv1 <- empty user enviroment
	seteq	pc,  cnt		@	if so,  return
	b	adr__err		@ jump to signal error

	/* (null-environment version) */
	PRIMIT	"null-environment", null_env, pfun, 1
	@ in:	sv1 <- version
	@ out:	sv1 <- environment
	b	adr_report_env

	/* (interaction-environment) */
	PRIMIT	"interaction-environment", intera_env, pfun, 0
	@ in:	sv1 <- '()
	@ out:	sv1 <- environment
	vecref	sv1, glv, 7		@ sv1 <- env -- global
	set	pc,  cnt

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
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
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@		core:			eval, apply, save, cons, list, error4,
@					boolxt, npofxt, subcpy, memcpy	
@					eof_char,dbl_quote_char,pound_char,
@					backslash_char,open_par_char,dot_char,
@					space_char,backspace_char,close_par_char
@	4.2.	Derived expressions:	qsqt	
@	4.3.	Macros:			mxpnd
@	6.2.	Numbers			strnum	
@	6.3.3.	Symbols:		strsym
@	6.3.6.	Vectors:		makvec
@	
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	/* (input-port? obj ...) */
	PRIMIT	"input-port?", input_port, pfun, 0, oioprfn, 0x80|f
	@ in:	sv1 <- (obj ...)
	@ defined only for file ports
	set	sv2, i1
	b	ioprtp

	/* (output-port? obj ...) */
	PRIMIT	"output-port?", output_port, pfun, 0
	@ in:	sv1 <- (obj ...)
	set	sv2, sv1		@ sv2 <- (obj ...)
	set	sv4, false
	bl	adr_ioprfe		@ sv2 <- possible output port	
	set	sv1, sv2
	set	sv2, i2
ioprtp:	@ [internal entry]
	pntrp	sv1			@ was a port obtained?
	itTT	eq
	cdreq	sv1, sv1
	vecrefeq sv1, sv1, 0
	eqeq	sv1, sv2
	b	adr_boolxt		@ ret with #t/#f if port obtained

	/* (current-input-port) */
	PRIMIT	"current-input-port", curinport, pfun, 0
	@ returns the top-level current-input port (the default input port)
	@ user redefines (current-input-port) in user environment to modify 'default' input port
	@ but this works at top level only since local env is not carried into read/write
	@ (they're proc, not syntax)
	@ in:	sv1 <- ()
	vecref	sv2, glv, 4
	vecref	sv3, sv2, 1
	b	cr_prt

	/* (current-output-port) */
	PRIMIT	"current-output-port", curoutport, pfun, 0
	@ in:	sv1 <- ()
	vecref	sv2, glv, 4
	vecref	sv3, sv2, 2
cr_prt:	@ [internal entry]
	vecref	sv1, sv2, 0
	list	sv1, sv1
	cons	sv1, sv1, sv3
	set	pc,  cnt

	/* (open-input-file filename <port-model>) */
	PRIMIT	"open-input-file", open_infile, pfun, 2
	@ in:	sv1 <- filename
	@ in:	sv2 <- <port-model> or null
	set	sv3, sv1		@ sv3 <- file name
	nullp	sv2			@ was a port model given?
	it	eq
	ldreq	sv2, =vfile		@	if not, sv3 <- FILE port model (default)
	vecref	sv2, sv2, 1		@ sv2 <- input  port vector
	set	sv1, null		@ sv1 <- null
	cons	sv1, sv1, sv2		@ sv1 <- (null . input-port-vector) for prtifi
	set	sv2, sv1		@ sv2 <- (null . input-port-vector) for prtifi (symmetry)
	bl	flok			@ acquire file system lock
	@ get file info, assess if it exists (if not, exit with error)
	prtfun	0x29, lnk		@ sv2 <- fil info vec, sv1 <- (null . in/out-port-vec)
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
	set	sv2, i1
opnif8:	bl	ffhofl			@ sv4 <- descr, sv2 <- file pre-sblst, sv3 <- post-sblst
	nullp	sv4
	it	ne
	addne	sv2, rvb, #4
	bne	opnif8
	set	sv4, rvb		@ sv4 <- file handle (scheme int)
	@ add file port in open file list
	set	rvb, 28
	bl	adr__alo
	add	rvc, rva, #4
	str	rvc, [rva, #-4]
	vcrfi	rvc, glv, 6		@ rvc <- open-file-list
	stmia	rva!, {rvc}
	add	rvc, rva, #8
	stmia	rva!, {rvc}
	add	rvc, rva, #12
	stmia	rva!, {sv1, sv4, rvc}
	set	rvc, null
	stmia	rva!, {sv5, rvc}	@ fre <- (((hndl #(fnm pg ofst bfr 0)) io-port-vec) . ofl)
	sub	sv2, rva, #32		@ sv2 <- updated open file list [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	vcsti	glv, 6, sv2		@ update open-file-list on glv
	set	sv1, sv4		@ sv1 <- file handle (scheme int), for exit
	set	lnk, cnt
	b	funlok
opnfer:	@ open file error
	bl	funlok			@ release file system lock
	b	adr__err

	/* (open-output-file filename <port-model>) */
	PRIMIT	"open-output-file", open_outfile, pfun, 2
	@ in:	sv1 <- filename
	@ in:	sv2 <- <port-model> or null
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
	vecref	sv2, sv3, 0		@ sv2 <- fname
	bl	stsyeq			@ are file names the same?
	beq	opnfer
	b	opnof0			@ jump to keep scanning open file list
opnof1:	@ get file info and add buffer to it
	set	sv3, sv1		@ sv3 <- file name
	vecref	sv5, sv5, 2		@ sv5 <- output  port vector
	set	sv1, null		@ sv1 <- null
	cons	sv1, sv1, sv5		@ sv1 <- (null . output-port-vector) for prtofi
	set	sv2, sv1		@ sv2 <- (null . output-port-vector) for prtofi (symmetry)
	prtfun	0x05, lnk
	cdr	sv3, sv1		@ sv3 <- output-port-vector
	vcrfi	rvb, sv3, 7		@ rvb <- buffer size (scheme int)
	lsr	rvb, rvb, #2
	add	rvb, rvb, #4
	bl	adr__alo		@ rva <- start address of allocated area
	set	rvc, i0			@ rvc <- 0 (scheme int)
	str	rvc, [rva]		@ set 0 as file ID, for potential file erasure in prtfun
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv3, rva, rvb		@ sv3 <- buffer (bytevector)	[*commit dest*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer,	[*restart crit instr*]
	vcsti	sv2, 3, sv3		@ store buffer in file info vector
	@ port-specific file-info initialization, including erasing an existing file
	set	sv4, sv2		@ sv4 <- #(fnm pg ofst bfr), for filers (via prtfun)
	set	sv2, sv1		@ sv2 <- (null . output-port-vector), for prtfun
	prtfun	0x06, lnk
	set	sv2, sv4		@ sv2 <- #(fname page offset buffer)
	cdr	sv1, sv1		@ output-port-vector
	b	opnfcm

	/* (close-input-port port ...) */
	PRIMIT	"close-input-port", close_inprt, pfun, 0, oioprfn, 0x80|t
	@ in:	sv1 <- (port ...)
	car	sv2, sv1		@ sv2 <- file info list (handle ...) (for oflfrm)
	bl	oflfrm			@ remove file from open-file-list
	prtfun	0x21

	/* (close-output-port port <mode>) */
	PRIMIT	"close-output-port", close_outprt, pfun, 0
	@ in:	sv1 <- port
	@ in:	sv2 <- (<mode>), if non-null close as input port (i.e. forget write-on-close)
	swap	sv1, sv2, sv4		@ sv1 <- (<mode>),	sv2 <- port
	list	sv2, sv2		@ sv2 <- (port)
	set	sv4, true
	bl	adr_ioprfe		@ sv2 <- full output-port (could be the null output-port)
	set	sv5, sv2
	car	sv2, sv2		@ sv2 <- file info list (handle ...) (for oflfrm)
	bl	oflfrm			@ remove file from open-file-list
	set	sv2, sv5
	prtfun	0x01

oflfrm:	@ remove a file from the open file list
	@ preserves:	sv1, sv5
	bic	rvc, lnk, #lnkbit0	@ rvc <- lnk, saved against flok, fclose (and even if T2)
	car	sv2, sv2		@ sv2 <- file handle (for ffhofl)
	bl	flok			@ acquire file system lock
	bl	ffhofl			@ sv4 <- descr, sv2 <- fil prior-sblst, sv3 <- post-sblst
	nullp	sv2			@ is prior open-file-list = '()?
	itE	eq
	vecseteq glv, 6, sv3		@	if so,  store rmnng open fil lst in global vec
	setcdrne  sv2, sv3		@	if not, str rmnng opn fil lst as cdr of open lst
	orr	lnk, rvc, #lnkbit0	@ lnk <- lnk, restored
	b	funlok			@ release file system lock and return to caller via lnk

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input:			CORE:	read-char, peek-char,
@						eof-object?, char-ready?
@-----------------------------------------------------------------------------*/

	/* (read-char <port> <reg> <n>) */
	PRIMIT	"read-char", read_char, efun, 0, oioprfn, (0x22<<2)|i0

	/* (peek-char <port> <reg> <n>) */
	PRIMIT	"peek-char", peek_char, efun, 0, oioprfn, (0x22<<2)|f0

	/* (eof-object? object) */
	PRIMIT	"eof-object?", eof_object, pfun, 1
	@ in:	sv1 <- obj
	@ out:	sv1 <- #t/#f
	set	sv2, eof_char
	eq	sv1, sv2
	b	adr_boolxt

	/* (char-ready? <port> <reg> <n>) */
	PRIMIT	"char-ready?", char_ready, efun, 0, oioprfn, (0x23<<2)|i0

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.3. output:			CORE:		write-char
@-----------------------------------------------------------------------------*/

	/* (write-char char <port> <reg> <n> ...) */
	PRIMIT	"write-char", write_char, efun, 1, oioprfn, (0x02<<2)|i0


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	7.	Addendum:		erase, files
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		lcons, lambda_synt, sav__c, save, eval,
@					npofxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

	
/*------------------------------------------------------------------------------
@  II.D.3. file system:				erase, files
@-----------------------------------------------------------------------------*/


.ifndef	live_SD

	/* (erase <sector>|<-1>) */
	PRIMIT	"erase", pfun, 0
	@ erase all file flash or just one sector within file flash or all lib flash
	@ in:	sv1 <- (<sector>|<-1>)
	bl	flok			@ acquire file system lock
	nullp	sv1			@ erasing all file flash?
	bne	erase4
	@ erase all file flash
	set	sv2, F_START_PAGE	@ sv2 <- start address of flash used for files
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
	adr	lnk, unlokx		@ lnk <- return addrs when erasing a single file/lib sector
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
	lsl	sv2, rva, #4		@ sv2 <- flash page to erase (full adrs)
    .ifndef SHARED_LIB_FILE
	ldr	sv3, =LIB_TOP_PAGE	@ sv3 <- end addrs of lib flash (eg. boot sector)
	lsr	rvb, sv3, #4		@ rvb <- end addrs of lib flash (shftd for unsigned comp)
	cmp	rva, rvb		@ erasing lib flash?
	bmi	libers			@	if so,  jump to erase single lib flash sector
    .endif
	b	ersfla			@ jump to erase single file flash sector
erslib:	@ erase lib flash
	ldr	rvb, =env_scheme
	vcsti	glv, 13, rvb		@ set scmenv (no libs) as built-in env
	vcrfi	sv2, glv, 12		@ sv2 <- addrs of 1st lib page in flash
	nullp	sv2			@ nothing to erase?
	beq	unlokx			@	if so,  jump to exit
	bl	lbsctr			@ rva <- start sect num for erase (raw int)
	raw2int	sv5, rva		@ sv5 <- start sect num for erase (scheme int)
	@ all lib flash is erased if sv1 = -536870912
	@ (a kind of safety if libs are not a valid list)
	lsr	rva, sv1, #2
	eq	rva, #0x20000000
	it	eq
	seteq	sv2, null
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
	bl	lbsctr			@ rva <- sect num of pg w/lib to keep (raw int)
	raw2int	sv4, rva		@ sv4 <- sect num of pg w/lib to keep (scheme int)
	set	sv3, sv2		@ sv3 <- start pg of lib to keep, or LIB_TOP_PAGE (saved)
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
	set	sv5, sv4		@ sv5 <- sector number of page with lib to keep (sch int)
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
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (dest, pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space (destination)
	vcsti	sv4, 2, rva		@ store src start page address in caller-tmp-storage of sv4
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ erase lib sector
	set	sv2, sv3		@ sv2 <- start page of lib to keep
	bl	libers
	@ copy from crunch space back to lib
	vcrfi	rva, glv, 11		@ rva <- crunch space addrs (src start, pseudo scheme int)
	bic	rva, rva, #i0		@ rva <- addrs of crunch space (source start)
	vcrfi	rvb, sv4, 1		@ rvb <- address of end of extra FLASH target (source end)
	vcrfi	rvc, sv4, 2		@ rvc <- start adrs of former src = dest for cpy
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ erase crunch sector
	bl	fcsdel
	@ return
	b	unlokx

  .endif  @ LIB_TOP_PAGE

.endif	@ live_SD
	
.ifndef	live_SD


	/* (fpgw pseudo-file-descriptor file-flash-page) */
	PRIMIT	"fpgw", pfun, 2
	@ write the contents of bytevector to specified file flash page
	@ in:	sv1 <- pseudo-file-descriptor with data to write at index 3
	@ in:	sv2 <- destination file-flash-page (scheme int)
	set	sv4, sv1		@ sv4 <- pseudo-file-descriptor
	bl	flok			@ acquire file system lock
	int2raw	rva, sv2		@
	lsl	sv2, rva, #4
	adr	lnk, unlokx		@ lnk <- return adrs for file/lib flash pg wrt func
  .ifdef LIB_TOP_PAGE
    .ifndef SHARED_LIB_FILE
	ldr	sv3, =LIB_TOP_PAGE	@ sv3 <- end adrs of lib flash (eg. boot sector)
	lsr	rvb, sv3, #4		@ rvb <- end adrs of lib flash (shftd for unsig comp)
	cmp	rva, rvb		@ writing to lib flash?
	it	mi
	bmi	libwrt			@	if so,  jmp to commit lib-flash wrt (ret to unlock)
    .endif
  .endif	
	b	wrtfla			@ commit write to file flash (returns to unlock)

.endif	@ live_SD


	/* (unlock) */
	PRIMIT	"unlock", pfun, 0
	@ release the file flash lock -- used to recover from file read/write crash
_func_
unlokx:	@ [internal entry]
	bl	funlok			@ release file lock
	b	adr_trufxt

	/* (files <port-model>) */
	PRIMIT	"files", pfun, 1
	@ list names of all files in flash or specified port
	@ note:	 if interrupt occurs and file gets deleted just before its name is consed,
	@	 it may still be listed
	@ in:	sv1 <- port-model or ()
	nullp	sv1			@ was a port model given?
	itE	eq
	ldreq	sv2, =vfile		@	if not, sv2 <- FILE port model (default)
	setne	sv2, sv1		@	if so,  sv2 <- port model
	vcrfi	sv2, sv2, 1		@ sv2 <- input  port vector
	set	sv1, null		@ sv1 <- null
	cons	sv1, sv1, sv2		@ sv1 <- (null . input-port-vector) for prtifi
	prtfun	0x2a			@ sv1 <- list of file names, return via cnt

.ifdef	onboard_SDFT

	/* (sd-init) */
	PRIMIT	"sd-init", sd_init, pfun, 0
	b	sd_ini

.endif


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */




