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

	SMBL	"_ISR",  pISR
	SMBL	"core",  core_
@	SMBL	"070",   versn_
	SMBL	"07r",   versn_
	SMBL	"throw", sthrow

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV core


	BNDVAR	"_winders", true
	BNDVAR	"_prg",     true
	BNDVAR	"version",  versn_

/* ========================================================================== */
	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	armpit scheme core:					
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			
@							
@
@	Modified by (switches):		LIB_TOP_PAGE, SHARED_LIB_FILE, cortex,
@					top_level_btree, inline_cons
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/
	
/*------------------------------------------------------------------------------
@
@ I.D.	MEMORY ALLOCATION AND MANAGEMENT
@
@ I.D.1. garbage collection - Stop and Copy:	gc, gc_bgn
@ I.D.2. garbage collection - Mark and Sweep:	gc, gc_bgn
@ I.D.3. memory allocation:			zmaloc, cons, list, save
@ I.D.4. memory comparison:			stsyeq
@ I.D.5. memory copying:			memcpy, szdcpy, subcpy
@ I.D.6. common function exits:			npofxt, notfxt, boolxt
@
@-----------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------
@ I.D.4. memory comparison:			stsyeq
@-----------------------------------------------------------------------------*/

_func_
stsyeq:	@ compare strings/symbols/bytevectors in sv1 and sv2, return with eq
	@ or ne flag raised.
	@ type is not compared (only size and bytes are compared)
	@ in:	sv1 <- string or symbol or bytevector
	@ in:	sv2 <- string or symbol or bytevector
	@ modifies:	sv3, rva, rvb
	@ returns via:	lnk
	strlen	rva, sv1		@ rva <- size of string/symbol/bv in sv1
	strlen	sv3, sv2		@ sv3 <- size of string/symbol/bv in sv2
	eq	rva, sv3		@ do strngs/symbls/bvs have the sam siz?
	it	ne
	setne	pc,  lnk		@	if not, return w/ne flag raised
stsye0:	eq	sv3, #i0
	it	eq
	seteq	pc,  lnk
	sub	sv3, sv3, #4		@ sv3 <- offst of nxt byt to comp, done?
	bytref	rvb, sv2, sv3		@ rvb <- byte frm strng/symbol/bv in sv2
	bytref	rva, sv1, sv3		@ rva <- byte frm strng/symbol/bv in sv1
	eq	rva, rvb		@ are bytes the same?
	beq	stsye0			@	if so,  jump to compare nxt byte
	set	pc,  lnk		@ return with ne flag raised

/*------------------------------------------------------------------------------
@ I.D.5. memory copying:			subcpy
@-----------------------------------------------------------------------------*/

_func_
subcpy:	@ copy a sized object in sv1, from offset sv2 to sv3
	@ return copy in sv1 (non-gceable, string tagged)
	@ in:	sv1 <- source
	@ in:	sv2 <- start offset
	@ in:	sv3 <- end offset
	@ out:	sv1 <- copy of src from start to end offset, schm string
	@ modifies:	sv1, sv2, sv4, rva, rvb, rvc
	@ returns via:	lnk
	set	sv4, sv1		@ sv4 <- source, saved
	@ calculate number of bytes needed
	sub	rvb, sv3, sv2		@ rvb <- number of chars to copy * 4
	lsr	rvb, rvb, #2		@ rvb <- number of chars to copy
	@ allocate memory
	bic	sv1, lnk, #lnkbit0	@ sv1 <- lnk, saved (even if Thumb2)
	bl	adr__alo		@ rva <- target object
	str	sv1, [rva]		@ store lnk in target object, temprrly
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- adrs of target (symbol), [*commit target dest*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer,        [*restart crit instr*]
	ldr	rva, [sv1]		@ rva <- lnk, almost restored
	orr	lnk, rva, #lnkbit0	@ lnk <- lnk, restored
	@ update object header to string of proper size
	set	rva, string_tag		@ rva <- full string tag
	sub	rvb, sv3, sv2		@ rvb <- number of chars in target * 4
	orr	rva, rva, rvb, LSL #6	@ rva <- full string tag w/size of data
	str	rva, [sv1, #-4]		@ set word #0 of target obj to strng tag
	@ copy bytes from source to target
	set	rvb, 0			@ rvb <- 0, ofst in trgt to chr aftr hdr
	@******* not 100% clear whether sv2 should start at 4b or 8b boundary **
subcp0:	cmp	sv2, sv3		@ are we done copying?
	it	pl
	setpl	pc,  lnk		@	if so, return
	bytref	rva, sv4, sv2		@ rva <- raw byte from source
	strb	rva, [sv1, rvb]		@ store it in target
	add	sv2, sv2, #4		@ sv2 <- updated source end offset
	add	rvb, rvb, #1		@ rvb <- updated target end address
	b	subcp0			@ jump to continue copying bytes


/*------------------------------------------------------------------------------
@ I.D.8. type checking
@-----------------------------------------------------------------------------*/

	/* return #t/#f based on whether type tag of object in sv1
	   matches sv4 */
	PRIMIT	typchk, ufun, 0
	adr	lnk, tpckxt
_func_
typsv1:	@ return in rva the type tag of sv1
	@ modifies:	rva, rvc
	ands	rva, sv1, #0x03
	beq	tpsv1p
	eq	rva, #3
	it	eq
	andeq	rva, sv1, #0xff
	set	pc,  lnk
tpsv1p:	@ pointer
	pairp	sv1
	itT	eq
	seteq	rva, 0xFF
	seteq	pc,  lnk
	ldrb	rva, [sv1, #-4]
	tst	rva, #0x04
	it	eq
	andeq	rva, rva, #0x0f
	set	pc,  lnk

_func_
tpckxt:	@ exit for type tag checking (typchk)
	ldr	rvc, =typtbl
	add	rvc, rvc, sv4, lsr #2
	ldrb	rvc, [rvc]
	eq	rva, rvc
	b	adr_boolxt


	/* type classification table for typchk */
	startBVU8	typtbl
	TYPTBWORD	iint, i0		@ number? (integer_only)
	TYPTBWORD	ichr, npo		@ char?
	TYPTBWORD	inul, null		@ null?
	TYPTBWORD	iprc, proc		@ procedure?
	TYPTBWORD	inot, false		@ not
	TYPTBWORD	ivar, variable_tag	@ symbol?
	TYPTBWORD	istr, string_tag	@ string?
	TYPTBWORD	ivec, vector_tag	@ vector?
	TYPTBWORD	ivu8, bytevector_tag	@ bytevector?
	TYPTBWORD	iers, symbol_tag	@ ers? (extern rep of a symbol?)
	ENDsized

/*------------------------------------------------------------------------------
@ I.D.4. common function exits
@-----------------------------------------------------------------------------*/

	/* normal exit (used by numeric function jump tables) */
	PRIMIT	"return", ufun, 0
	set	pc,  cnt

	/* exit with #t */
	PRIMIT	trufxt, ufun, 0
	set	sv1, true
	set	pc,  cnt

	/* exit with #f */
	PRIMIT	flsfxt, ufun, 0
	set	sv1, false
	set	pc,  cnt

	/* exit with non-printing object */
	PRIMIT	npofxt, ufun, 0
	set	sv1, npo
	set	pc,  cnt

	/* boolean function exit, based on test result, eq -> #t, ne -> #f */
	PRIMIT	boolxt, ufun, 0
	itE	eq
	seteq	sv1, true
	setne	sv1, false
	set	pc,  cnt

	/* exit with #t/#f in opposition to test result, ne -> #t, eq -> #f */
	PRIMIT	notfxt, ufun, 0
	itE	eq
	seteq	sv1, false
	setne	sv1, true
	set	pc,  cnt
	
/*----------------------------------------------------------------------------*\

  I.C.   HARDWARE-INDEPENDENT INTERRUPT ROUTINES (ISRs)

  I.C.1. Generic / Timer 0/1  ISR
  I.C.2. UART 0/1 ISR
  I.C.3. TIMER 0/1 clearing partial ISR (branched to from genisr)
  I.C.4. Exit from isr via break

\*----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@  I.C.1. Generic ISR (branches to uart isr, i2c isr or processes timer ints)
@-----------------------------------------------------------------------------*/

_func_
genisr:	@ generic isr routine, branches to specific routines or scheme callback
	enterisr			@ rvb <- interrupt status
					@ stk <- fre,cnt,rvabc,lnkusr,pcusr,spsr
  .ifndef enable_a9_mpcore
	set	rva, BUFFER_START	@ rva <- start address of buffers
  .else
	mrc	p15, 0, rva, c0, c0, 5	@ rva <- Multiproc Affinity reg, MPIDR
	and	rva, rva, #3
	ldr	rvc, =MP_mat
	add	rva, rvc, rva, lsl 5
	ldr	rva, [rva, #0x10]	@ rva <- BUFFER_START_n
  .endif
	vcrfi	rva, rva, ISR_V_offset
	ldr	rvc, [rva, rvb, lsl #2]
	execp	rvc
	bne	adr__isx
	adr	lnk, genis0	
	pntrp	rvc
	it	ne
	lsrne	rvc, rvc, #16

  .ifdef CODE_OFFSET
  	it	ne
	addne	rvc, rvc, #CODE_OFFSET	@	if so,  rvc <- adjstd start adr
  .endif

	set	pc,  rvc
genis0:	@ [return from machine code ISR, entry frm Systick handler on Cortex-M3]
	raw2int	rvc, rvb
	str	rvc, [sp,  #-12]
	@ see if memory transaction or gc was interrupted
	ldmia	sp!, {fre}
	tst	fre, #0x02		@ was memory reserved?
	it	eq
	bleq	adr__ism		@	if so,  restart or go-through
	@ exit if there is no scheme isr
	vcrfi	rvb, glv, 0		@ rvb <- possible scheme callback
	execp	rvb			@ is it run-able?
	it	ne
	stmdbne	sp!, {fre}
	bne	genier
	@ save the context
  .ifndef hardware_FPU
	set	rvb, 84			@ rvb <- num data bytes to allocate
  .else
    .ifndef FPU_is_maverick
	set	rvb, 100		@ rvb <- num data bytes to allocate
    .else
	set	rvb, 108		@ rvb <- num data bytes to allocate
    .endif
  .endif
	bl	adr__alo		@ rva <- address of allocated memory
	bic	fre, fre, #0x03
	add	cnt, fre, #60
	add	rva, fre, #8
	add	rvc, fre, #28
	stmia	fre!, {cnt, rva, rvc}
	ldr	cnt, =stkbtm		@ cnt <- scheme stack-bottom
	ldr	rva, [sp,  #-16]	@ rva <- int num restored from under stk
	set	rvc, null		@ rvc <- '()
	stmia	fre!, {cnt, rva, rvc}
	@ build vector for gc-eable part of context
	set	rva, vector_tag
	orr	rva, rva, #0x0700
	stmia	fre!, {rva, sv1-sv5, env, dts} @ sv1-5,env,cnt,dts->gc-cntxt vec
	@ adjust resume-stack start address
	sub	dts, fre, #56
	@ build bytevector for non-gc-eable part of context
	ldmia	sp!, {sv1-sv5, env, rvc} @ cnt,rva/b/c,lnkusr,lnkirq,psrusr<-stk
	set	rva, bytevector_tag
	sub	rvb, rvb, #56
	orr	rva, rva, rvb, lsl #8	@ rva <- 28,44 or 52 bytes in non-gc vec
	stmia	fre!, {rva, sv1-sv5, env, rvc} @ cnt,rvabc,lnkusr/irq,psru->ngcv
  .ifdef hardware_FPU
    .ifndef FPU_is_maverick
	vstmia	fre!, {s0, s1}
	vmrs	rva, fpscr
	stmia	fre!, {rva, rvb}
  .else
	cfmv32sc mvdx4, dspsc		@ mvdx4 <- contents of DSPSC
	cfmvr64l sv1, mvdx4		@ sv1   <- id
	cfmvrdl	sv2, mvd0		@ sv2   <- lo 32 bits of maverick reg 0
	cfmvrdh	sv3, mvd0		@ sv3   <- hi 32 bits of maverick reg 0
	cfmvrdl	sv4, mvd1		@ sv4   <- lo 32 bits of maverick reg 1
	cfmvrdh	sv5, mvd1		@ sv5   <- hi 32 bits of maverick reg 1
	stmia	fre!, {sv1-sv5, rvc}
    .endif
  .endif
	orr	fre, fre, #0x02		@ fre <- de-reserved
	@ load isr
	vcrfi	sv1, glv, 0		@ sv1 <- scheme callback
	clearVicInt			@ clear interrupt in interrupt vector
	@ prepare scheme stacks and environment
	@ sv1 <- handler
	vcrfi	env, sv1, 2		@ env <- env from handler
	add	sv2, dts, #16		@ sv2 <- (interrupt-number) = arg-list
	set	sv3, null		@ (or car sv2 for int num if needed)
	ldr	sv4, =reset0		@ lnk <- reset0, if lnk used/savd on stk
	orr	sv4, sv4, #lnkbit0
	ldr	sv5, =adr__apl
	adr	cnt, rsrctx
	set	rvc, normal_run_mode	@ rvc <- normal run mode
	stmdb	sp!, {fre,cnt,sv1-sv5,rvc}	@ put values on irq_stack
	set	sv4, null		@ sv4 <- '() for cortx if sv4 ends w/b11
	b	rsrxit			@ jump to common isr exit

_func_
rsrctx:	@ restore context (non-gc-context gc-context ...)
	swi	isr_no_irq		@ switch to IRQ mode without interrupts
	restor	cnt			@ cnt <- non-gc-cntxt,dts <- (gc-cntxt.)
	car	rva, dts		@ rva <- gc-context
	@ restore a saved context and resume
	@ rva <- non-gc-context vector,  cnt <- gc-context vector
	ldmia	cnt!, {sv1-sv5, env, dts}
  .ifndef cortex
	add	sp,  sp,  #4
  .endif
	stmdb	sp!, {fre, sv1-sv5, env, dts}
  .ifdef hardware_FPU
    .ifndef FPU_is_maverick
	vldmia	cnt!, {s0, s1}
	ldmia	cnt!, {rvb}
	vmsr	fpscr, rvb
    .else
	ldmia	cnt!, {sv1-sv5}
	cfmv64lr mvdx4, sv1		@ mvfx4 <- saved contents of DSPSC
	cfmvsc32 dspsc, mvdx4		@ restore DSPSC in maverick co-processor
	cfmvdlr	mvd0, sv2		@ restore lo 32 bits of maverick reg 0
	cfmvdhr	mvd0, sv3		@ restore hi 32 bits of maverick reg 0
	cfmvdlr	mvd1, sv4		@ restore lo 32 bits of maverick reg 1
	cfmvdhr	mvd1, sv5		@ restore hi 32 bits of maverick reg 1
    .endif
  .endif
	ldmia	rva, {sv1-sv5, env, dts} @ restr sv1-5,env,cnt,dts frm cntxt vec
rsrxit:	@ [internal entry] common isr exit
	isrexit

genier:	@ return from ISR with error
	vcrfi	env, glv, 7		@ env from glv (***NEW***)
	ldr	sv1, [sp, #-12]
	ldr	sv4, =pISR
	ldr	rvc, =error4
	str	rvc, [sp,  #20]
	str	rvc, [sp,  #24]
	set	rvc, normal_run_mode
	str	rvc, [sp,  #28]
	b	adr__isx		@ jump to common isr exit

	/* _isx */
	PRIMIT	"_isx", sntx, 0
	@ simple exit from interrupt	
	clearVicInt			@ clear interrupt in interrupt vector
	exitisr				@ exit from isr

	/* _ism */
	PRIMIT	"_ism", sntx, 0
	@ _ism   isr memory allocation (check reservation ...)
	@ prepare to allocate memory during interrupt when memory was reserved by user process
	@ in:	sp  <- [cnt rva rvb rvc lnk_usr lnk_irq psr_usr]
	@ in:	fre <- value of fre when interrupt arose
	ldr	rvb, [sp,  #20]		@ rvb <- lnk_irq (pc_usr, adrs of post-int instr)
	bic	rvb, rvb, #0x01		@ rvb <- lnk_irq (pc_usr) clrd Thumb bit to load properly
	tst	fre, #0x01		@ was memory reserved at level 1 (i.e zmaloc)?
	beq	gniml2			@	if not, jump to process level 2 memory reservation
	@ process level 1 memory reservation (zmaloc interruption)
	ldr	rvb, [rvb]		@ rvb <- interrupted instruction
	adr	rvc, icrit1		@ rvc <- address of critical instruction
	ldr	rvc, [rvc]		@ rvc <- critical instruction
	eq	rvb, rvc		@ was the critical instruction interrupted?
	bne	zmrstr			@	if not, jump to set user proc up for maloc restart
	@ process level 1 critical instruction interruption
	ldr	rva, [sp,  #4]		@ get saved rva frm stk (in case cortex ISRs tail-chained)
icrit1:	orr	fre, rva, #0x02		@ mem crit instr for level 1 (update and free mem ptr)
	ldr	rvb, [sp,  #20]		@ rva <- lnk_irq (pc_usr)
	@ this, below, is ok since crit is 32-bit instruction, even on cortex
	add	rvb, rvb, #4		@ rva <- next instr for user proc to exec (crit done)
	str	rvb, [sp,  #20]		@ set next instruction for interrupted process
	set	pc,  lnk		@ return

zmrstr:	@ set user process up for 'bl zmaloc' restart when resumed
	ldr	rvb, [sp,  #16]		@ rva <- lnk_usr (return adrs (lr) of interrupted proc)
	@ this, below, works since bl is 32-bit instruction even on cortex
	bic	rvb, rvb, #0x01
	sub	rvb, rvb, #4		@ rva <- rstrt adrs of cal to cns/sv/zmlc in intrptd proc
	str	rvb, [sp,  #20]		@ set lnk_irq (pc_usr) to be the restart address
.ifdef	cortex
	@ reset the xPSR/EPSR to prevent restarting invalidated ldm/stm instructions
	@ alternatively, clear just bits 26:25 and 15:10
	set	rvb, normal_run_mode	@ r1 <- dflt xPSR to restore w/bit 24 (Thumb mode) set
	str	rvb, [sp,  #24]
.endif
	eor	fre, fre, #0x03		@ fre <- free-pointer, de-reserved
	set	pc,  lnk		@ return


gniml2:	@ process level 2 memory reservation (interrupted cons/save ... or gc)
	@ in:	sp  <- [cnt rva rvb rvc lnk_usr lnk_irq psr_usr]
	@ in:	rvb <- address of interrupted instruction
	@ in:	fre <- value of fre when interrupt arose
  .ifdef enable_MPU
	ldr	rva, =adr__gc
	cmp	rva, rvb
	itT	mi
	ldrmi	rva, =gc_end
	cmpmi	rvb, rva
	bpl	gniml3
  .endif 	@ enable_MPU
	stmdb	sp!, {fre}		@ store fre ptr back on stack (complt stk for return)
	ldr	cnt, [sp,  #20]		@ cnt  <- lnk_usr
	str	cnt, [sp,  #-4]		@ store it under stack
	str	lnk, [sp,  #-8]		@ store lnk_irq under stack
	adr	rva, gnimrt		@ rva  <- return address (lnk_usr) for restarted gc
	add	rva, rva, #4		@ rva  <- lnk_usr updated for gc-return method
.ifndef cortex
	str	rva, [sp,  #20]		@ store lnk_usr on stack
	ldr	rva, [sp,  #28]		@ rva  <- psr_usr
	orr	rva, rva,  #IRQ_disable	@ rva  <- user psr with IRQ disabled
	msr	spsr_cxsf, rva		@ spsr <- cpsr_usr with interrupts disabled
	mrs	rva, cpsr
	str	rva, [sp, #28]
	ldmia	sp,  {fre, cnt, rva, rvb, rvc, lnk}^	@ Return
	add	sp,  sp, #24
	ldmia	sp!, {lnk}
	movs	pc,  lnk
gnimrt:	mrs	lnk, cpsr		@ lnk <- usr_psr
	bic	lnk, lnk, #IRQ_disable	@ lnk <- usr_psr with interrupts enabled
	swi	isr_no_irq		@ switch to IRQ mode without interrupts
	sub	sp,  sp,  #4
	stmdb	sp!, {fre, cnt, rva, rvb, rvc, lnk}	@ store 5 regs and dummy on irq stack
	ldr	cnt, [sp,  #28]		@ rva <- IRQ psr
	msr	cpsr_cxsf, cnt		@ restore IRQ psr
	add	cnt, sp,  #24
	stmib	cnt, {lnk}^		@ store cpsr_usr
.else
	orr	rva, rva,  #lnkbit0	@ adjust lnk_usr for Thumb mode
	str	rva, [sp,  #20]		@ store lnk_usr on stack
	@ resume gc in thread mode with no IRQ
	set	rvb, run_no_irq
	b	svcigc
	nop
	nop
	nop
	nop
_func_
gnimrt:	nop
	nop
	nop
	nop
	nop
	swi	isr_no_irq		@ switch to IRQ mode without interrupts
	sub	sp,  sp,  #32		@ set stack ptr back to get saved items
	@ enable scheme interrupts
	set	cnt, int_enab_base
	intupd	cnt
.endif
	ldr	lnk, [sp,  #-8]		@ lnk <- lnk_irq restored
	ldr	cnt, [sp,  #-4]		@ cnt  <- lnk_usr
	str	cnt, [sp,  #20]		@ store it in stack
	bic	cnt, cnt, #0x01
	sub	cnt, cnt, #4		@ cnt  <- restart adrs of intrptd zmaloc
	str	cnt, [sp,  #24]		@ store it in stack
	ldmia	sp!, {fre}
	set	pc,  lnk

  .ifdef enable_MPU

gniml3:	@ complete an interrupted cons/save ... 
	@ or set user process up to restart it when resumed
	@ in:	sp  <- [cnt rva rvb rvc lnk_usr lnk_irq psr_usr]
	@ in:	fre <- value of fre when interrupt arose
	@ in:	rvb <- address of interrupted instruction
	ldr	rva, [rvb]		@ rva <- interrupted instruction
	adr	rvc, icrit0		@ rvc <- address of critical instruction
	ldr	rvc, [rvc]		@ rvc <- critical instruction
	eq	rva, rvc		@ was the critical instruction interrupted?
	bne	alrstr			@	if not, jmp to set usr proc for cns/sv rstrt
	@ process level 2 critical instruction interruption
icrit0:	orr	fre, fre, #0x02		@ mem crit instr for level 2 (de-reserve mem ptr)
	ldr	rvb, [sp,  #20]		@ rva <- lnk_irq (pc_usr)
	@ this, below, is ok since crit is 32-bit instruction, even on cortex
	add	rvb, rvb, #4		@ rva <- next instr for user proc to exec (crit done)
	str	rvb, [sp,  #20]		@ set next instruction for interrupted process
	set	pc,  lnk		@ return

alrstr:	@ set user process up to restart cons/save ... when resumed
	@ in:	sp  <- [cnt rva rvb rvc lnk_usr lnk_irq psr_usr]
	@ in:	fre <- value of fre when interrupt arose
	@ in:	rvb <- address of interrupted instruction
  .ifndef cortex
	set	rva, 0xE3C00003		@ rva <- bic fre, fre, #3 ARM mach code
  .else
	set	rva, 0x0003F020		@ rva <- bic fre, fre, #3 T2  mach code
  .endif
alsrch:	@ search code backwards for memory reservation instruction
  .ifndef cortex
	sub	rvb, rvb, #4
  .else
	sub	rvb, rvb, #2
  .endif
	ldr	cnt, [rvb]
	eq	cnt, rva
	bne	alsrch
  .ifdef cortex
	set	rvc, normal_run_mode	@ rvc <- dflt xPSR to restore w/bit 24 (Thumb mode) set
	str	rvc, [sp,  #24]
  .endif
	str	rvb, [sp,  #20]		@ set lnk_irq (pc_usr) to restart at memory reservation
	bic	fre, fre, #7		@ fre <- 8-byte aligned (eg. for ibcons)
	orr	fre, fre, #0x02		@ fre <- free-pointer, de-reserved
	set	pc,  lnk		@ return

  .endif @ enable_MPU

/*------------------------------------------------------------------------------
@  I.C.3. TIMER 0/1 clearing partial ISR (branched to from genisr)
@-----------------------------------------------------------------------------*/

	/* TIMER clearing partial ISR */
	PRIMIT	tmrisr, ufun, 0
	@
	@ do not modify rvb, lnk, sp in such partial ISR
	@
	eq	rvb, #timer0_int_num	@ is interrupt for TIMER0?
	itE	eq			@	if-then (Thumb-2)
	seteq	rva, timer0_base	@	if so,  rva <- base adrs of TMR0
	setne	rva, timer1_base	@	if not, rva <- base adrs of TMR1
	clearTimerInt			@ clear int in timer peripheral block
	set	pc,  lnk		@ return to genisr


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*------------------------------------------------------------------------------
@ I.D.1. garbage collection - Stop and Copy:	gc, gc_bgn
@-----------------------------------------------------------------------------*/

	/* (gc) */
	PRIMIT	"gc", pfun, 0
	bl	gc			@ rva <- free mem bytes (raw int), do gc
	lsr	rva, rva, #3
	raw2int	sv1, rva		@ sv1 <- free cons cells (scheme int)
	set	pc,  cnt

_func_
gc:	@ perform garbage collection
	@ out:	rva <- number of bytes of free memory
	@ out:	rvb <- 0 = number of bytes allocated
	set	rvb, 0			@ rvb <- 0 = number of bytes to allocate
	add	lnk, lnk, #4		@ lnk <- retrn addr adj for gc_bgn exit
	b	adr__gc


.ifndef mark_and_sweep
	
	/* (_gc) */
	PRIMIT	"_gc", sntx, 0
	@ entry for cons/save/zmaloc and genism
	@ in:	rvb <- how many bytes were to be allocated when gc was triggered
	@ in:	lnk <- caller return address (gc returns to lnk - 4 = restart, except for 'bl gc')
	@ out:	rva <- number of bytes of free memory
	@ out:	rvb <- (unmodified)
	@ reserve memory
	bic	fre, fre, #0x03		@ reserve memory, level 2
	@ get heapbottom
	vcrfi	rvc, glv, 18		@ rvc <- cpu(n) heapbottom, pseudo-int
	@ if fre > heaptop0 do gc down otherwise do gc up
	vcrfi	rva, glv, 9		@ rva <- heaptop0 as pseudo scheme int
	bic	rva, rva, #i0		@ rva <- heaptop0 (address)
	cmp	rva, fre		@ was the top heap in use?
	itE	mi
	bicmi	fre, rvc, #3		@	if so,  fre <- bottom of low heap (heap bottom)
  .ifndef enable_MPU
	setpl	fre, rva		@	if not, fre <- bottom of top heap (top of low heap)
  .else
	addpl	fre, rva, #64		@	if not, fre <- bottom of top heap (top of low + 64)
  .endif
	set	rva, cnt
	set	cnt, (9<<8)|vector_tag
	stmia	fre!, {cnt, rva, sv1-sv5, env, dts, glv}
	@ prepare registers/constants for gc
	set	sv2,  broken_heart
	bic	sv1, rvc, #3		@ sv1 <- heapbottom (bottom of low heap) - to test pointers
	vcrfi	env, glv, 10		@ env <- heaptop1 (top of top heap)
	sub	env, env, #2		@ env <- top of top heap, adjusted -- to test pointers
	sub	dts, fre, #4
	sub	glv, fre, #40
	sub	rvc, fre, #36
gcloop:	add	glv, glv, #4		@ update scan-gc
	cmp	glv, fre		@ done scanning memory?
	it	pl
	bpl	gcexit			@	if so,  jump to exit
	ldr	cnt, [glv]		@ cnt  <- (car scan-gc){old gc}
	pntrp	cnt			@ could item be a pointer?
	beq	gcmov			@	if so,  jump to move the object it points to
	cmp	dts, glv		@ is sized-object-tag within a gc-eable sized object?
	bpl	gcloop			@	if so,  skip it (only ptrs have to be treated)
	and	sv4, cnt, #0xff		@ sv4 <- user proc/cont/macro id bits
	eq	sv4, #proc		@ is item a user proc/cont/macro?
	it	eq
	tsteq	cnt, #(1 << 12)		@	if so,  is it indirect?
	it	eq
	addeq	dts, glv, #12
	beq	gcloop
	and	sv4, cnt, #0x07		@ sv4 <- rat/cpx id bits
	eq	sv4, #0x03		@ is item a rat/cpx?
	it	eq
	addeq	glv, glv, #4
	and	sv4, cnt, #0xCF		@ sv4  <- sized-object-tag indicator bits of item
	eq	sv4, #0x4F		@ is item a sized-object-tag?
	bne	gcloop			@	if not, jump to copy next item	
	tst	cnt, #0x30
	itT	eq
	addeq	dts, glv, cnt, LSR #6	@	if so,  dts <- position of end of object + 1
	biceq	dts, dts, #3		@	if so,  dts <- position of end of object
	itTT	ne
	addne	glv, glv, cnt, LSR #8	@	if not, glv <- position of next object, unaligned
	addne	glv, glv, #3		@	if not, glv <- nxt obj pos, unalign + 3 (to align)
	bicne	glv, glv, #3		@	if not, glv <- nxt obj pos, aligned (to skip curr.)
	b	gcloop

gcmov:	cmp	cnt, sv1		@ is object address >= heapbottom ?
	it	pl
	cmppl	env, cnt		@		if so,  is object address <= heaptop ?
	bmi	gcloop			@		if not, jump to cont scan (obj not in heap)
	bic	sv5, cnt, #7		@ sv5 <- aligned object address
	snoc	sv3, sv4, sv5		@ sv3 <- car/tag of object, sv4 <- cdr/item1 of object
	eq	sv3, sv2		@ is sv3 a broken-heart?
	it	eq
	streq	sv4, [glv]		@	if so,  store updated object address in new heap
	beq	gcloop			@	if so,  jump to cont scan (object already moved)
	stmia	fre!, {sv3, sv4}	@ store car/tag-cdr/item1 in new heap
	pairp	cnt
	itE	eq
	subeq	sv4, fre, #8
	subne	sv4, fre, #4
	stmia	sv5!, {sv2, sv4}	@ store broken-heart and obj's new heap adrs in old heap
	str	sv4, [glv]		@ store object's new heap address in new heap
	beq	gcloop			@	if so,  jump to continue scan (move completed)
	set	cnt, sv5
	and	sv5, sv3, #0xff
	eq	sv5, #proc		@ is object a proc/cont/macro (size=4W)?
	it	eq
	tsteq	sv3, #(1 << 12)		@	if so,  is it indirect?
	itT	eq
	ldmiaeq	cnt!, {sv3, sv4}	@	if so,  cnt <- next two words of object
	stmiaeq	fre!, {sv3, sv4}	@	if so,  store them in new heap
	beq	gcloop			@ 	if so,  jump to process next object
	and	sv5, sv3, #0x07
	eq	sv5, #0x03		@ is object a rat/cpx (size=2W)?
	beq	gcloop			@ 	if so,  jump to process next object
	lsrs	sv5, sv3, #8		@ is sized item an empty string, vector or bytevector?
	itT	eq
	streq	sv2, [fre, #-4]		@	if so,  broken-heart to new heap
	beq	gcloop			@	if so,  return
	tst	sv3, #0x30		@ did cnt point to a gc-eable sized obj?
	itT	ne
	addne	sv5, sv5, #0x03		@	if not, sv5 <- size + 3 bytes
	lsrne	sv5, sv5, #2		@	if not, sv5 <- num words to move
	tst	sv5, #0x01		@ is object 8-byte aligned?
	itTT	eq
	subeq	sv5, sv5, #1		@	if not, sv5 <- obj xtra wrd idx
	streq	sv2, [cnt, sv5, lsl #2]	@	if not, broken-heart to old heap
	addeq	sv5, sv5, #2		@	if not, sv5 <- 8B aligned size
	add	sv5, sv5, #1
gclp01:	subs	sv5, sv5, #2		@ sv5 <- remnng num words to move, is 0?
	itT	ne
	ldmiane	cnt!, {sv3, sv4}	@	if not, cnt <- nxt 2 wrds of obj
	stmiane	fre!, {sv3, sv4}	@	if not, store them in new heap
	bne	gclp01			@	if not, jump to keep copying obj
	b	gcloop			@ jump to process next object

gcexit:	@ restore registers
	ldmia	rvc, {cnt, sv1-sv5, env, dts, glv}
	@ reset if memory is still exhausted after gc
	vcrfi	rva, glv, 9		@ rva <- heaptop0 -- from global vector
	cmp	fre, rva		@ is code using the bottom heap?
	it	pl
	vcrfipl	rva, glv, 10		@	if not, rva <- heaptop1
	vcsti	glv, 1, rva		@ store current heap top in global vec
	bic	rva, rva, #i0
	sub	rva, rva, fre		@ rva <- free heap space bytes (raw int)
	cmp	rva, rvb		@ are enough bytes free?
	it	mi
	ldrmi	pc,  =reset0		@	if not, reset, mem is exhausted
	@ exit
	orr	fre, fre, #0x02		@ fre <- ...bbb10 (back to de-reserved)
	bic	lnk, lnk, #lnkbit0
	sub	lnk, lnk, #4		@ return (restart alloc that triggrd gc)
	orr	lnk, lnk, #lnkbit0
	set	pc,  lnk

gc_end:	@ end-of gc code address marker

.else	@ mark_and_sweep gc

	/* (_gc) */
	PRIMIT	"_gc", sntx, 0
	@-----------------------------------------------------------------------
	@ entry for cons/save/zmaloc and genism
	@ in:	rvb <- how many bytes were to be allocated when gc was triggered
	@ in:	lnk <- return address of caller (gc retrns to lnk - 4 = restrt, excpt for 'bl gc')
	@ out:	rva <- number of bytes of free memory
	@ out:	rvb <- (unmodified)
	@ modifies:	rva, rvc, others are gc-updated or preserved/restored
	@-----------------------------------------------------------------------
	vcrfi	rva, glv, 18		@ rva <- cpu(n) heapbottom, pseudo-int
	bic	fre, rva, #3		@ fre <- heapbottom, resrv mem, level 2
	set	rva, cnt
	set	cnt, (9<<8)|vector_tag
	stmia	fre, {cnt, rva, sv1-sv5, env, dts, glv}
	vcrfi	env, glv, 1		@ env <- heaptop -- from glv, pseudo-int
  .ifndef grey_area_base
	vcrfi	sv2, glv, 10		@ sv2 <- grey area address, pseudo-int
	bic	sv2, sv2, #i0		@ sv2 <- address of grey area
  .else
	set	sv2, grey_area_base
  .endif
	@ clear grey and black sets
	@ in:	sv2 <- grey set
	@ in:	dts <- black set
	@ modifies:	sv3, sv4
	add	sv3, sv2, #(grey_set_size << 3)	@ dts <- address of black area
	set	sv4, 0
	set	sv5, 0
gcm1_c:	stmdb	sv3!, {sv4, sv5}
	eq	sv3, sv2
	bne	gcm1_c
	@-----------------------------------------------------------------------
	@ mark phase
	@ in:	fre <- heapbottom
	@ in:	env <- heaptop
	@ in:	sv2 <- grey  area address
	@ out:	black set is marked
	@ modifies:	rvc, cnt, sv1, sv3, sv4, sv5, glv
	@ exit via:	jump to sweep phase 1 (gcswp1)
	@-------------------------------------------------------------------------
	add	sv1, fre, #4		@ sv1 <- start of mark scan (vec of regs)
	set	sv5, null		@ sv5 <- top parent
	b	gcm1_l
gcm1_b:	@ parent is black or grey-black
	@ in:	sv1 <- child
	@ in:	sv5 <- parent
	@ in:	sv4 <- grey  set word for parent
	@ in:	glv <- black set word for parent
	@ in:	cnt <- positioned bit for testing if parent is in grey set
  .ifdef mcu_has_swp
	swp	sv3, sv1, [sv5]
  .else
	ldr	sv3, [sv5]
	str	sv1, [sv5]
  .endif
	tst	sv4, cnt		@ is parent black-only?
	it	ne
	subne	sv5, sv5, #8
	bne	gcm1_x			@ 	if not, jump to that: obj in vec
	and	rva, sv1, #0xff
	eq	rva, #proc
	it	eq
	tsteq	sv1, #(1 << 12)		@	if so,  is it indirect?
	it	ne
	eqne	rva, #vector_tag
	it	eq
	addeq	sv5, sv5, #4
	set	sv1, sv5		@ sv1 <- child of parent of parent (i.e. parent)
	set	sv5, sv3		@ sv5 <- parent of parent, restored as parent
gcm1up:	@ return to parent
	@ in:	sv1 <- child
	@ in:	sv5 <- parent
	@ in:	fre <- heapbottom
	@ in:	env <- heaptop
	@ in:	sv2 <- grey  area address
	@ in:	dts <- black area address
	@ modifies:	rvc, cnt, sv1, sv3, sv4, sv5, glv
	nullp	sv5			@ done scanning?
	beq	gcswp1			@	if so,  jump to sweep phase 1
	@ if parent is grey only, mark it black, go to 2nd child
	@ if parent is black only, go to its parent
	@ if parent is grey+black (eg. vec), keep marching through its children
	sub	sv3, sv5, fre		@ sv3 <- offset to parent
	rgbf	rvc, sv3, 3, 8		@ rvc <- bit pos for object's start adrs
	ash	cnt, 1, rvc		@ cnt <- positioned bit for object
	bic	sv3, sv3, #0xff
	add	sv3, sv2, sv3, lsr #5
	ldmia	sv3, {sv4, glv}		@ sv4, glv <- word from grey, black set
	@ is parent white or grey-only?
	tst	glv, cnt		@ is parent black or maybe grey-black?
	bne	gcm1_b			@	if so, jump to that case
	eor	sv4, sv4, cnt		@ sv4 <- grey  word w/parent bit flipped
	eor	glv, glv, cnt		@ glv <- black word w/parent bit flipped
	stmia	sv3, {sv4, glv}		@ update parent marks in grey/black sets
	set	rvc, sv1		@ rvc <- previously visited child
	ldmia	sv5, {sv1, sv3}		@ sv1 <- nxt chld 2 vst, sv3 <- grndprnt
	stmia	sv5, {sv3, rvc}		@ store grndprnt & prev child in parent
	pntrp	sv1			@ is next child a pointer?
	bne	gcm1up			@	if not, jump to return to parent
	
gcm1_l:	@ mark loop	[main entry]
	@ in:	sv1 <- child
	@ in:	sv5 <- parent
	@ in:	fre <- heapbottom
	@ in:	env <- heaptop
	@ in:	sv2 <- grey  area address
	@ sv1 is pointer to child
	@ is child in heap?
	cmp	sv1, fre		@ is child address >= heapbottom ?
	it	pl
	cmppl	env, sv1		@	if so,  is child address < heaptop ?
	bmi	gcm1up			@	if not, jump up to continue processing of parent
	@ is child already marked?
	sub	sv3, sv1, fre		@ sv3 <- offset to child
	rgbf	rvc, sv3, 3, 8		@ rvc <- bit pos for object's start adrs
	ash	cnt, 1, rvc		@ cnt <- positioned bit for object
	bic	sv3, sv3, #0xff
	add	sv3, sv2, sv3, lsr #5
	ldmia	sv3, {sv4, glv}
	tst	sv4, cnt		@ is child in grey set?
	it	eq
	tsteq	glv, cnt		@	if not, is child in black set?
	bne	gcm1up			@	if so,  jump up to continue processing of parent
	@ is child a pair?
	pairp	sv1
	bne	gcmnpp			@	if not, jump to mark for non-pair pointer
	orr	sv4, sv4, cnt		@ sv4 <- grey set word updated with child cons cell bit
	str	sv4, [sv3]		@ store updated mark in grey set
gcmve2:	@ [internal re-entry for vectors]
	set	sv3, sv5		@ sv3 <- parent
	set	sv5, sv1		@ sv5 <- child
gcm1_x:	@ [internal re-entry when parent is mixed grey-black (vector)]
	ldr	sv1, [sv5, #4]		@ sv1 <- cdr of child (becomes child)
	str	sv3, [sv5, #4]		@ store parent in cdr of child (DSW)
	pntrp	sv1			@ is child a pointer?
	bne	gcm1up			@	if not, jump to return to parent
	b	gcm1_l			@ jump down to mark first child

gcmnpp:	@ mark a non-pair pointer (rat/cpx, procedure, sized object)
	@ in:	sv1 <- object pointer (bit 2 set)
	@ in:	sv3 <- offset of word in grey/black set
	@ in:	sv4 <- word from grey set
	@ in:	sv5 <- parent
	@ in:	fre <- heapbottom
	@ in:	glv <- word from black set
	@ in:	cnt <- bit mask for object position in word from grey/black set
	@ in:	dts <- black set
	@ modifies:	rva, rvc, cnt, glv, sv3, sv4
	ldr	rva, [sv1, #-4]		@ rva <- tag of child
	and	rvc, rva, #0x07		@ rvc <- tag masked for rat/cpx identification
	eq	rvc, #0x03		@ is child a rat/cpx?
	it	eq
	seteq	rva, 0			@	if so,  rva <- 0, for return to gcm1up (done below)
	@ =*=*=*=*= nothing special to do here it seems
	tst	rva, #0x80		@ is child a proc?
	it	ne
	setne	rvc, 3
	bne	gcmvec
	lsrs	rvc, rva, #8		@ rvc <- object length (raw int), is it zero (or rat/cpx)?
	itT	eq
	orreq	glv, glv, cnt		@	if so,  glv <- old black bits + object's bits
	streq	glv, [sv3, #4]		@	if so,  add object bits to black set
	beq	gcm1up			@	if so,  jump up to continue processing of parent
	tst	rva, #0x30		@ is child a vector?
	bne	gcmngc			@	if not, jump to that case

gcmvec:	@ child is a vector
	@ in:	sv1 <- object pointer (vector)
	@ in:	rvc <- vector length (raw int)
	@ in:	sv3 <- offset of word in grey/black set
	@ in:	sv4 <- word from grey set
	@ in:	sv5 <- parent
	@ in:	fre <- heapbottom
	@ in:	glv <- word from black set
	@ in:	cnt <- bit mask for object position in word from grey/black set
	@ in:	dts <- black set
	@ modifies:	rva, rvc, cnt, glv, sv1, sv3, sv4
	orr	sv4, sv4, cnt		@ sv4 <- grset wrd updtd w/chld vec strt
	str	sv4, [sv3]		@ store updated mark in grey set
	bic	rva, rvc, #1
	sub	sv1, sv1, #4
	add	sv1, sv1, rva, lsl #2	@ sv1 <- adrs of last 8-byte cell of vec
	tst	rvc, #1			@ is vec-length odd (rather than even)?
	bne	gcmve2
	sub	sv3, sv1, fre
	rgbf	rvc, sv3, 3, 8		@ rvc <- bit pos for object's start adrs
	ash	cnt, 1, rvc		@ cnt <- positioned bit for object
	bic	sv3, sv3, #0xff
	add	sv3, sv2, sv3, lsr #5
	ldmia	sv3, {sv4, glv}		@ sv4, glv <- word from grey, black set
	orr	sv4, sv4, cnt		@ glv <- old black bits + object's bits
	orr	glv, glv, cnt		@ glv <- old black bits + object's bits
	stmia	sv3, {sv4, glv}		@ mark cell in both sets
	set	sv3, sv5		@ sv3 <- parent
	set	sv5, sv1		@ sv5 <- child
  .ifdef mcu_has_swp
	swp	sv1, sv3, [sv5]
  .else
	ldr	sv1, [sv5]
	str	sv3, [sv5]
  .endif
	pntrp	sv1			@ is child a pointer?
	bne	gcm1up			@	if not, jump to return to parent
	b	gcm1_l			@ jump down to mark first child

gcmngc:	@ child is non-gceable (string, symbol, bytevector)
	sub	sv4, sv1, fre		@ sv4 <- offset to child
	lsr	sv4, sv4, #3		@ sv4 <- offset to child's 8-byte cell
	and	rvc, sv4, #0x1f		@ rvc <- bit position for object's start address
	mvn	cnt, #0			@ cnt <- 0b1111...1111
	lsl	cnt, cnt, rvc		@ cnt <- bit-mask for object's 8-byte cells
	add	rva, rva, #0x0300	@ rva <- size tag + 3 bytes
	lsr	rva, rva, #11		@ rva <- (size-in-8bytes - 1) of sized item
	add	rva, rva, rvc		@ rva <- bit position of object's end address
	add	rva, rva, #1
gcm1_3:	@ set sized object's bits in black area
	cmp	rva, #32		@ do multiple words remain to be marked?
	itTT	mi
	mvnmi	sv4, #0			@	if not, sv4 <- full mask
	lslmi	sv4, sv4, rva		@	if not, sv4 <- bit-mask for end of remaining cells
	eormi	cnt, cnt, sv4		@	if not, cnt <- bit-mask for remaining cells
	ldr	glv, [sv3, #4]		@ glv <- grey bits from black set
	orr	glv, glv, cnt		@ glv <- old black bits + object's bits
	str	glv, [sv3, #4]		@ add object bits to black set
	bmi	gcm1up			@	if so,  jump up to continue processing of parent
	mvn	cnt, #0			@ cnt <- full mask
	add	sv3, sv3, #8		@ sv3 <- offset to next word to mark / 4
	sub	rva, rva, #32		@ rva <- updated number of 8-byte cells to mark
	b	gcm1_3			@ jump back to continue marking black set
	
gcswp1:	@-------------------------------------------------------------------------
	@ sweep phase 1: build free cell list
	@ in:	fre <- heapbottom
	@ in:	env <- heaptop
	@ in:	sv2 <- grey area
	@ out:	sv2 <- free cells and cumulative free space list
	@ modifies:	rva, rvc, sv1, sv3-sv5, dts, holes in heap
	@
	@ modifies:	cnt, glv (new)
	@
	@ exits via:	continues to sweep phase 2
	@-------------------------------------------------------------------------
	@ is top-cell of heap in use?
	bic	sv1, env, #i0		@ sv1 <- heaptop (adrs frm psdo sch int)
	sub	sv1, sv1, #8		@ sv1 <- start adrs of last-word-in-heap
	sub	sv3, sv1, fre		@ sv3 <- offset to last-word-in-heap
	rgbf	rvc, sv3, 3, 8		@ rvc <- bit pos for object's start adrs
	ash	cnt, 1, rvc		@ cnt <- positioned bit for object
	bic	sv3, sv3, #0xff
	add	sv3, sv2, sv3, lsr #5
	ldr	sv4, [sv3, #4]		@ sv4 <- black set word for last-word-in-heap
	tst	sv4, cnt		@ is last-word-in-heap a free cell?
	itTTT	ne
	mvnne	cnt, #0
	lslne	cnt, cnt, rvc
	orrne	sv4, sv4, cnt
	strne	sv4, [sv3, #4]		@ update black set word for last-word-in-heap
	add	sv3, sv3, #8
	set	dts, sv2
	set	sv2, null		@ sv2 <- top of free cell list
	itTE	eq
	seteq	rvc, 0			@	if so,  rvc <- 0 -- start looking for bttm of hole
	biceq	rva, env, #i0		@	if so,  rva <- heaptop (adrs from pseudo sch int)
	mvnne	rvc, #0			@	if not, rvc <- -1 --start looking for top of hole
	@ find top/bottom of holes by scanning black set words
gcs1_0:	subs	glv, sv3, dts
	beq	gcs1_c			@	if so,  jump to exit
	sub	sv3, sv3, #8
	ldr	sv4, [sv3, #4]		@ sv4 <- word from black set
	eors	sv4, sv4, rvc		@ sv4 <- possibly inverted word from black set
	beq	gcs1_0			@	if not, jump back to scan next black-set-word
	@ find top/bottom of hole based on bits in black-set-word
	add	sv1, fre, glv, lsl #5	@ sv1 <- base address of black-set-word
	set	sv5, 0x80000000		@ sv5 <- bit mask, at top bit position
gcs1_1:	eq	sv5, #0			@ black-set-word fully scanned?
	beq	gcs1_0			@	if so,  jump back to scan next black-set-word
	sub	sv1, sv1, #8		@ sv1 <- address of item at bit in sv5
	tst	sv4, sv5		@ is item a top/bottom of hole?
	lsr	sv5, sv5, #1		@ sv5 <- mask bit shifted for next item
	beq	gcs1_1			@	if not, jump back to test next bit
	mvn	sv4, sv4		@ sv4 <- black-set-word invrtd (to find btm/top of hole)
	mvns	rvc, rvc
	it	eq
	seteq	rva, sv1		@	if so,  rva <- address of top of this hole
	beq	gcs1_1			@	if so,  jump back to look for bottom of this hole
	set	rvc, sv2
	add	sv2, sv1, #8		@ rvc <- updated start of free-cell list
	sub	rva, rva, sv1		@ rva <- size of hole (in bytes)
	stmia	sv2, {rva, rvc}		@ store size-of-hole in hole
	mvn	rvc, #0			@ rvc <- -1 = we will now look for top of next hole
	b	gcs1_1			@ jump back to look for top of next hole

gcs1_c:	@ set hole sizes to cumulative sizes in free cell list
	@ in:	sv2 <- free cell list with hole sizes
	@ out:	sv2 <- free cell list with cumulative hole sizes
	@ modifies:	rva, sv4, sv5
	set	sv5, sv2		@ sv5 <- address of first hole (start of holes-list)
	set	rva, 0			@ rva <- 0, initial total hole size
gcs1_d:	@ loop
	nullp	sv5			@ done scanning holes?
	itTTT	ne
	ldrne	sv4, [sv5]		@	if not, sv4 <- size of this hole
	addne	rva, rva, sv4		@	if not, rva <- updated cumulative hole size
	strne	rva, [sv5]		@	if not, store cumulative hole size in hole
	ldrne	sv5, [sv5, #4]		@	if not, sv5 <- address of next hole
	bne	gcs1_d			@	if not, jump to continue adding hole sizes

	@-------------------------------------------------------------------------
	@ sweep phase 2: update pointers for upcoming crunch (compaction)
	@ in:	sv2 <- free cell list with cumulative hole sizes
	@ in:	fre <- heapbottom
	@ in:	env <- heaptop
	@ modifies:	rva, rvc, sv1, sv3-sv5, dts, content of heap (pointers)
	@ exits via:	jump to sweep phase 3 (gcs3_0) for compaction
	@-------------------------------------------------------------------------
	nullp	sv2			@ no free cells?
	beq	gcs3_0			@	if so,  jump to sweep phase 3
	set	sv3, sv2		@ sv3 <- end   of 1st black area (start of 1st hole)
	sub	sv4, fre, #4		@ sv4 <- adrs of 32-bit cell before 1st one to be visited
	set	sv5, sv4		@ sv5 <- end adrs of prior vector (semi-dummy but needed)
	set	rvc, 0			@ rvc <- 0 = cumulative free area up to this point
	b	gcs2_1
	
gcs2_4:	@ go to next black area
	@ in:	sv3 <- address of last scanned memory cell + 1 word (start of hole)
	@ in:	env <- heaptop
	@ in:	rvc <- cumulative free area below last scan
	@ modifies:	rva, rvc, sv3, sv4
	bic	rva, env, #i0		@ rva <- heaptop (raw int)
	eq	sv3, rva		@ done scanning memory?
	beq	gcs3_0			@	if so,  jump forward to crunch mem (sweep phase 3)
	ldr	rva, [sv3]		@ rva <- free area below upcoming scan
	sub	rvc, rva, rvc		@ rvc <- hole size = difference in free areas
	add	sv4, sv3, rvc		@ sv4 <- start address for next chunk to scan
	sub	sv4, sv4, #4		@ sv4 <- adrs of 32-bit cell before 1st one to visit next
	set	rvc, rva		@ rvc <- free area below upcoming scan
	ldr	sv3, [sv3, #4]		@ sv3 <- address of next hole
	nullp	sv3			@ no more holes?
	it	eq
	biceq	sv3, env, #i0		@	if so,  sv3 <- heaptop
gcs2_1:	@ loop
	add	sv4, sv4, #4		@ sv4 <- address of 32-bit cell to visit
	cmp	sv4, sv3		@ are we done with this black area?
	bpl	gcs2_4			@	if so,  jump to process next black area
	ldr	sv1, [sv4]		@ sv1 <- content of 32-bit memory cell
	pntrp	sv1			@ is it a pointer?
	beq	gcs2_2			@	if so,  jump to process it
	cmp	sv5, sv4		@ is item within a sized-object?
	bpl	gcs2_1			@	if so,  skip it (only ptrs have to be treated)
	and	rva, sv1, #0x07		@ rva <- content masked for rat/cpx identification
	eq	rva, #0x03		@ is it a rat/cpx?
	it	eq
	addeq	sv4, sv4, #4		@	if so,  sv4 <- skip one 32-bit cell
	and	rva, sv1, #0xff		@ rva <- content masked for macro/procedure identification
	eq	rva, #proc		@ is it a macro/cont/proc (size = 4 W)?
	it	eq
	tsteq	sv1, #(1 << 12)		@	if so,  is it indirect?
	it	eq
	addeq	sv5, sv4, #12		@	if so,  sv5 <- position of last word in object
	and	rva, sv1, #0xcf		@ rva <- content masked for sized item identification
	eq	rva, #0x4f		@ is content a sized item tag?
	bne	gcs2_1			@	if not, jump back to process next 32-bit cell
	tst	sv1, #0x30		@ is it a vector?
	itEE	eq
	addeq	sv5, sv4, sv1, LSR #6	@	if so,  sv5 <- pos last word of obj, plus  1 byte
	addne	sv5, sv4, sv1, LSR #8	@	if not, sv5 <- pos last byte of obj, minus 3 bytes
	addne	sv5, sv5, #3		@	if not, sv5 <- position of last byte of object
	bic	sv5, sv5, #7		@ sv5 <- position of last 8-byte cell of object
	add	sv5, sv5, #4		@ sv5 <- position of last word allocated to object
	it	ne
	setne	sv4, sv5		@	if not, sv4 <- pos last word of obj (to skip it)
	b	gcs2_1			@ jump back to process next 32-bit cell

gcs2_2:	@ update pointer in sv1 to the address it will point to after compaction
	@ in:	sv1 <- pointer
	@ in:	sv2 <- free cell list
	@ in:	sv4 <- address where sv1 is stored
	@ in:	env <- heaptop
	@ modifies:	rva, cnt, sv1, [sv4]
	cmp	sv1, sv2		@ does pointer point above first hole?
	it	pl
	cmppl	env, sv1		@	if so,  does it point below heaptop?
	bmi	gcs2_1			@	if so,  jump back to process next 32-bit cell
	@ find amount of free space below pointer's pointed address
	set	cnt, sv2		@ cnt <- start of first hole
gcs2_3:	@ loop
	ldr	rva, [cnt, #4]		@ rva <- address of test hole
	nullp	rva			@ done with holes?
	it	eq
	seteq	rva, env		@	if so,  rva <- heaptop
	cmp	sv1, rva		@ does pointer point above test hole?
	it	pl
	setpl	cnt, rva		@ 	if so,  cnt <- next test hole
	bpl	gcs2_3			@	if so,  jump back to continue search
	ldr	rva, [cnt]		@ rva <- amount of free space below pointer's pointed adrs
	sub	sv1, sv1, rva		@ sv1 <- pointer's pointed address after upcoming crunch
	str	sv1, [sv4]		@ store new address (update pointer)
	b	gcs2_1			@ jump back to scan memory

gcs3_0:	@-------------------------------------------------------------------------
	@ sweep phase 3: crunch (garbage compaction)
	@ in:	sv2 <- free cell list
	@ in:	env <- heaptop
	@ out:	sv3 <- first free cell in compacted heap
	@ modifies:	rva, rvc, sv1-sv5, content of heap
	@-------------------------------------------------------------------------
	nullp	sv2			@ no free cells?
	itE	eq
	biceq	sv3, env, #i0		@	if so,  sv3 <- heaptop (heap full)
	setne	sv3, sv2		@	if not, sv3 <- adrs start of infill area (1st hole)
	set	rva, 0			@ rva <- 0 = cumulative size of free cells below this point
gcs3_1:	@ loop over holes
	nullp	sv2			@ no more free cells?
	beq	gcexit			@	if so,  jump to exit
	set	rvc, rva		@ rvc <- previous cumulative free cell size
	ldr	rva, [sv2]		@ rva <- cumulative free cell size
	sub	rvc, rva, rvc		@ rvc <- size of this hole
	add	sv1, sv2, rvc		@ sv1 <- adrs of start of next black area to be moved down
	ldr	sv2, [sv2, #4]		@ sv2 <- address of free area after next black area
	nullp	sv2			@ is this the last free area?
	itE	eq
	biceq	rvc, env, #i0		@	if so,  rvc <- end of black area = heaptop
	setne	rvc, sv2		@	if not, rvc <- end black area = start of next hole
gcs3_4:	@ loop to move data down, filling hole
	cmp	sv1, rvc		@ done filling hole?
	itT	mi
	ldmiami	sv1!, {sv4, sv5}	@	if not, sv4, sv5 <- 8-byts frm src, incr sv1 (src)
	stmiami	sv3!, {sv4, sv5}	@	if not, store 8-bytes in target, incr sv3 (dest)
	bmi	gcs3_4			@	if not, jump to keep filling hole
	b	gcs3_1			@ jump back to fill next hole

gcexit:	@-------------------------------------------------------------------------
	@ restore registers and exit
	@ in:	sv3 <- first free cell in compacted heap
	@ in:	env <- heaptop
	@ in:	fre <- heapbottom
	@ out:	cnt, sv1-sv5, env, dts, glv <- restored registers
	@ out:	fre <- first free cell in compacted heap, de-reserved
	@ out:	rva <- number of free bytes in heap
	@ modifies:	rva, rvc, fre, cnt, sv1-sv5, env, dts, glv
	@-------------------------------------------------------------------------
	bic	rvc, env, #i0		@ rvc <- end of black area = heaptop
	add	rva, fre, #4		@ rva <- heapbottom + 4 (from fre), to restore regs
	set	fre, sv3		@ fre <- first free cell in compacted heap
	ldmia	rva, {cnt, sv1-sv5, env, dts, glv} @ restore regs
	@ reset if memory is still exhausted after gc
	sub	rva, rvc, fre		@ rva <- free heap space bytes (raw int)
	cmp	rva, rvb		@ are enough bytes free?
	it	mi
	ldrmi	pc,  =reset0		@	if not, jump to reset, memory is exhausted
	orr	fre, fre, #0x02		@ fre <- ...bbb10		(back to de-reserved)
	bic	lnk, lnk, #lnkbit0
	sub	lnk, lnk, #4		@ return (restart allocation that triggered gc)
	orr	lnk, lnk, #lnkbit0
	set	pc,  lnk

gc_end:	@ end-of gc code address marker

.ltorg

.endif	@ mark-and-sweep


/*------------------------------------------------------------------------------
@ I.D.3. memory allocation:			zmaloc, cons, list, save
@-----------------------------------------------------------------------------*/

	/* _alo */
	PRIMIT	"_alo", sntx, 0
	@ allocate rvb (raw int, aligned) bytes of memory to a sized object (bytevector)
	@ allocated block is 8-byte aligned
	@ in:	rvb <- number of bytes to allocate (raw int)
	@ in:	fre <- current free cell address (normally reserved)
	@ out:	rva <- allocated address (tagged as bytevector, aligned at 8-byte + 4)
	@ out:	rvb <- number of data bytes in reserved area (8-byte aligned - 4)
	@ out:	fre <- address of start of allocated zone (level 1 reserved, i.e. ...bbb01)
	@ mods:	rva, rvb, rvc, fre
	@ ret:	lnk
	@ unconditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	add	rvb, rvb, #11		@ rvb <- num bytes to alloc + 4 for tag + 7 to align
	bic	rvb, rvb, #7		@ rvb <- num bytes to allocate, 8-byte aligned
	sub	rvb, rvb, #4		@ rvb <- num data bytes in zone to allocate
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	sub	rva, rva, fre		@ rva <- free heap space bytes (raw int)
	cmp	rva, rvb		@ are enough bytes free?
	bmi	adr__gc			@	if not, jump to perform gc and restart zmaloc
	lsl	rva, rvb, #8		@ rva <- number of bytes in object, shifted
	orr	rva, rva, #bytevector_tag @ rva <- full tag for memory area
	str	rva, [fre, #-1]		@ set word #1 of sized object to bytevector tag
	add	rva, fre, #0x03		@ rva <- address of zmaloc-ed bytevector (data area)
	set	pc,  lnk

	/* (_mkc vars-list code-address) */
	PRIMIT	"_mkc", sntx, 2
	@ in:	sv1 <- address of proc's vars-list
	@ in:	sv2 <- address where proc's code starts
	@ out:	sv1 <- compiled proc
	set	sv3, procedure
	orr	sv3, sv3, #0xC000
	tagwenv	sv1, sv3, sv1, sv2
	set	pc,  cnt

  .ifndef enable_MPU
    .ifdef inline_cons
alogc8:	set	rvb, 8			@ rvb <- number of bytes to allocate (8)
	b	adr__gc			@ jump to perform gc and restart cons
    
algc16:	set	rvb, 16			@ rvb <- number of bytes to allocate (16)
	b	adr__gc			@ jump to perform gc and restart cons

algc24:	set	rvb, 24			@ rvb <- number of bytes to allocate (24)
	b	adr__gc			@ jump to perform gc and restart cons

    .else

_func_
cons:	@ primitive cons:	called by cons and list macros
	@ in:	fre <- current free cell address (normally reserved)
	@ out:	rva <- allocated address (content is tagged as bytevector)
	@ out:	rvb <- unmodified or 8 (raw int) if gc was triggered
	@ out:	rvc <- '() (for cdr of cons cell, if called from list macro)
	@ out:	fre <- allocated address (level 1 reserved, i.e. ...bbb01)
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ unconditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	@ assuming current heaptop is stored in glv (updated by gc and/or install)
	@ assuming heap size is always 8-byte aligned
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	cmp	rva, fre		@ is an 8-byte cell available?
	itTT	hi
	bichi	rva, fre, #0x03		@	if so,  rva <- address of allocated memory
	sethi	rvc, null		@	if so,  rvc <- '() (for list macro)
	sethi	pc,  lnk		@	if so,  return to complete cons macro
alogc8:	set	rvb, 8			@ rvb <- number of bytes to allocate (8)
	b	adr__gc			@ jump to perform gc and restart cons

_func_
save:	@ primitive save:	called by save macro
	@ in:	fre <- current free cell address (not reserved)
	@ in:	dts <- current scheme stack
	@ out:	rva <- next free address
	@ out:	rvb <- unmodified or 8 (raw int) if gc was triggered
	@ out:	fre <- next free address (de-reserved, i.e. ...bbb10)
	@ out:	dts <- updated scheme stack with free car
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ conditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	@ assuming current heaptop is stored in glv (updated by gc and/or install)
	@ assuming heap size is always 8-byte aligned (heaptop always adjusted by 16-bytes)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	cmp	rva, fre		@ is an 8-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {fre, dts}	@ rva <- adrs of nxt free cell + str dmy in prior fre cell
	sub	dts, rva, #8		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
cons2:	@ primitive cons2:	called by bcons and lcons macros (called by save2 macro)
	@ in:	fre <- current free cell address (not reserved)
	@ out:	rva <- allocated address
	@ out:	rvc <- allocated address
	@ out:	fre <- current free cell address (reserved level 1)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	itTT	hi
	bichi	rva, fre, #0x03		@ rva <- address of allocated memory
	sethi	rvc, rva
	sethi	pc,  lnk		@ return to complete save macro
algc16:	set	rvb, 16			@ rvb <- number of bytes to allocate (16)
	b	adr__gc			@ jump to perform gc and restart cons

_func_
cons3:	@ primitive cons3:	called by llcons macro (called by save3 macro)
	@ in:	fre <- current free cell address (not reserved)
	@ out:	rva <- allocated address
	@ out:	rvb <- allocated address
	@ out:	rvc <- allocated address + 8 (start of next cons cell)
	@ out:	fre <- current free cell address (reserved level 1)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 24-byte cell available?
	itTTT	hi
	bichi	rva, fre, #0x03		@ rva <- address of allocated memory
	sethi	rvb, rva
	addhi	rvc, rva, #8
	sethi	pc,  lnk		@ return to complete save macro
algc24:	set	rvb, 24			@ rvb <- number of bytes to allocate (24)
	b	adr__gc			@ jump to perform gc and restart cons

_func_
sav__c:	@ primitive sav__c:	save cnt on stack
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	alogc8
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {cnt, dts}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
sav_ec:	@ primitive sav_ec:	save env and cnt on stack
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	set	rvb, dts
	stmia	rva!, {cnt, rvb, env, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
sav_rc:	@ primitive sav_rc:	save cnt on stack with room for register at top (reg cnt -> dts)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts, glv, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
savrec:	@ primitive savrec:	save env,cnt on stack, w/room for reg at top (reg env cnt -> dts)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 24-byte cell available?
	bls	algc24
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts}
	add	rvb, rva, #8
	stmia	rva!, {cnt,rvb,env,rvc}
	sub	dts, rva, #16		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

    .endif	@ ifndef inline_cons

  .endif	@ no enable_MPU


/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.5.   eval:				eval, interaction-environment
@-----------------------------------------------------------------------------*/


_func_
eval:	@ [internal entry]
	@ in:	sv1 <- exp == self-eval-obj or var/synt or application or macro to expand
	varp	sv1
	beq	adr__lkp
	pairp	sv1
	it	ne
	setne	pc,  cnt
  .ifdef fast_lambda_lkp
	snoc	rva, sv5, sv1
	eq	rva, #variable_tag
	beq	xlkp0
  .endif
	@ continue to evalls
_func_
evalls:	@ sv1 <- exp == (ufun uarg1 uarg2 ...) where ufun could be a macro
	@ evaluate ufun, uarg1 ... and branch to apply, unless fun is a macro
	snoc	sv1, sv2, sv1		@ sv1 <- ufun, sv2 <- (ufun uarg1 uarg2 ...)
	evalsv1				@ sv1 <- fun	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	and	rva, sv1, #0x07		@ rva <- fun masked for 4-bit pointer id
	eq	rva, #0x04		@ is this a user function?
	itE	eq
	ldreq	rva, [sv1, #-4]		@	if so,  rva <- user fun tag
	setne	rva, sv1		@	if not, rva <- built-in fun tag
	and	rvb, rva, #0xff		@ rvb <- tag masked for function id
	eq	rvb, #0xdf		@ is it a prim, proc, cont, synt, macro?
	bne	adr__err		@	if not, jump to report error
	tst	rva, #0x0800		@ is fun a built-in syntax (or macro)?
	bne	synapp			@	if so,  jump to that case
	nullp	sv2			@ no args (prim, proc, cont)?
	beq	apply0			@	if so,  jump to apply
	@ evaluate the arguments of a compound expression, then jump to apply
	and	rva, rva, #0x0700	@ rva <- number of vars, shifted
	orr	sv4, rva, #i0
	save	sv1			@ dts <- (fun ...)
  .ifdef fast_eval_apply
	ands	rvb, sv4, #0x0700	@ rvb <- number of vars, is it zero?
	itT	ne
	mvnne	rvc, rvb		@	if not, rvc <- inverted number of vars
	tstne	rvc, #0x0400		@	if not, are there 1 to 3 vars?
	beq	evlnrm			@	if not, jump to use normal process
	snoc	sv4, sv5, sv2
	nullp	sv5
	it	eq
	eqeq	rvb, #0x0100
	beq	evlan1
	pairp	sv5
	itT	eq
	snoceq	sv3, sv5, sv5
	nullpeq	sv5
	it	eq
	eqeq	rvb, #0x0200
	beq	evlan2
	pairp	sv5
	itT	eq
	snoceq	sv1, sv5, sv5
	nullpeq	sv5
	it	eq
	eqeq	rvb, #0x0300
	beq	evlan3
evlnrm:	@ continue with normal evaluation

  .endif

	set	sv4, null		@ sv4 <- initial, reversed, evaluated args-list
evlarg:	@ sv2 <- (uarg1 uarg2 ...), sv4 <- (fun . result), dts <- ((fun . result) ...)
	snoc	sv1, sv2, sv2		@ sv1 <- uarg1,	sv2 <- (uarg2 ...)
	evalsv1				@ sv1 <- arg1	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	cons	sv4, sv1, sv4		@ sv4 <- updated, reversed, evaluated args-list
	nullp	sv2			@ no uarg left?
	bne	evlarg			@	if not, jump to continue processing uargs
	@ de-reverse evaluated args-list
	set	sv2, null		@ sv2 <- initial evaluated args-list
evlrag:	snoc	sv1, sv4, sv4
	cons	sv2, sv1, sv2
	nullp	sv4
	bne	evlrag
	restor	sv1			@ sv1 <- fun,	dts <- (...)
	pntrp	sv1
	itE	eq
	ldreq	rva, [sv1, #-4]
	setne	rva, sv1
	adr	rvc, apptbl
	rgbf	rvb, rva, 14, 16
	ldr	pc,  [rvc, rvb, lsl #2]

_func_
sbevll: @ evaluate a list
	@ in:	sv1 <- list to evaluate (application)
	@ out:	sv1 <- value of applying car of list to cdr
	@ preserves:	sv2, sv4, env, cnt
	@ returns via:	lnk
  .ifdef fast_lambda_lkp
	snoc	rva, sv5, sv1
	eq	rva, #variable_tag
	beq	xlkp
  .endif
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk made even if Thumb2
	vctrcr4	sv3, sv2, sv3, sv4, env	@ sv3 <- #(cnt sv2 lnk sv4 env)
	save	sv3			@ dts <- (#(cnt sv2 lnk sv4 env) ...)
	call	evalls
	restor	sv3			@ sv3 <- #(cnt sv2 lnk sv4 env), dts <- (...)
	ldmia	sv3, {cnt, sv2, sv3, sv4, env}
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk

  .ifdef fast_lambda_lkp

xlkp0:	@ entry 0 (returns via cnt)
	set	lnk, cnt
xlkp:	@ entry 1 (returns via lnk)
	varp	sv5
	beq	xlkpv
	pairp	sv5
	bne	adr__err
	snoc	rva, sv1, sv5
	tst	rva, #0x02
	it	ne
	setne	pc,  lnk
	set	sv3, env
	lsrs	rvb, rva, #16
xlkpl0:	itT	ne
	cdrne	sv3, sv3
	subsne	rvb, rvb, #1
	bne	xlkpl0
	car	sv3, sv3
	lsl	rvb, rva, #16
	lsrs	rvb, rvb, #18
xlkpl1:	itT	ne
	cdrne	sv3, sv3
	subsne	rvb, rvb, #1
	bne	xlkpl1
	set	sv5, sv1
xlkpl3:	car	sv1, sv3
	snoc	rva, sv1, sv1
	eq	rva, sv5
	it	eq
	seteq	pc,  lnk
	cdr	sv3, sv3
	b	xlkpl3

xlkpv:	@ look in user env for var in sv5
	@ in:	sv1 <- (xlkp . var)
	@ in:	sv5 <- var
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk made even if Thumb2
	save	sv1, sv3
	set	sv3, sv5
	@ in:	sv3 <- var to look for
	@ out:	sv1 <- binding for var
	@ out:	sv5 <- sub-env (not built-in) where bndng for var was found, or null
	@ out:	rva <- var to look for
	@ out:	rvb <- pos of var's bndng in env, coded as [frm-pos bndng-pos] (sch int)
	@ modifies:	rva, rvb, rvc, sv1, sv3, sv5
	set	rva, sv3
	set	rvb, i0
	set	sv5, env
bndlk0:	nullp	sv5
	beq	xlkpv0
	car	sv3, sv5
	bic	rvb, rvb, #0xff00
	bic	rvb, rvb, #0x00fc
bndlk1:	nullp	sv3
	itT	eq
	addeq	rvb, rvb, #0x010000
	cdreq	sv5, sv5
	beq	bndlk0
	car	sv1, sv3
	car	rvc, sv1
	cmp	rva, rvc
	beq	xlkpv0
	itT	mi
	addmi	rvb, rvb, #0x010000
	cdrmi	sv5, sv5
	bmi	bndlk0
	add	rvb, rvb, #4
	cdr	sv3, sv3
	b	bndlk1
	
xlkpv0:	@ continue
	nullp	sv5
	@ Note: if libraries are implemented, then may have to look through
	@ 	them here, rather than error out.
	beq	xlkpv2
	vcrfi	sv3, glv, 7		@ sv1 <- top-level env
	eq	sv3, sv5
	it	eq
	seteq	sv5, sv1
	beq	xlkpv1
	car	sv3, dts
	cdr	sv5, sv3
	set	sv3, rvb
	cons	sv5, sv3, sv5
xlkpv1:	@ continue
	restor	sv3
	setcdr	sv3, sv5
	cdr	sv1, sv1
	restor	sv3
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk

xlkpv2:	@ built-in var or unknown var
	set	sv1, rva
	tst	sv1, #0xff000000
	bne	adr__err
	vecref	sv3, glv, 13		@ sv3 <- built-in env vector
	rgbf	rvc, sv1, 8, 15
	ldr	sv3, [sv3, rvc, lsl #2]	@ sv3 <- built-in sub-env vector
	lsr	rvc, sv1, #16		@ rvb <- offset in built-in sub-env
	ldr	sv3, [sv3, rvc, lsl #2]	@ sv3 <- symbol's binding value (value or address)
	cons	sv1, sv1, sv3
	set	sv5, sv1
	b	xlkpv1

  .endif 	@ fast_lambda_lkp

/*------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.4.   control features:		apply
@-----------------------------------------------------------------------------*/

	/* (_apl function args-list) */
	PRIMIT	"_apl", sntx, 2
	execp	sv1			@ is fun a prim, proc or cont?
	bne	adr__err		@	if not, jump to apply error
	pntrp	sv1
	itE	eq
	ldreq	rva, [sv1, #-4]
	setne	rva, sv1
apply0:	@ [internal entry 2]
	adr	rvc, apptbl
	rgbf	rvb, rva, 14, 16
	ldr	pc,  [rvc, rvb, lsl #2]

.balign 4

apptbl:	@ apply jump table
	.word	prmapp
	.word	cmpapp
	.word	cntapp
	.word	cplapp


synapp:	@ syntax-apply (error-out on user macro)
	@ in:	sv1 <- primitive (syntax or user-macro)
	@ in:	sv2 <- arg-list
	@ in:	rva <- 32-bit tag of primitive
	pntrp	sv1			@ is sv1 a user macro?
	beq	adr__err		@	if so,  error out
	@ continue to prmapp
_func_
prmapp:	@ primitive-apply
	@ in:	sv1 <- primitive
	@ in:	sv2 <- arg-list
	@ in:	rva <- 32-bit tag of primitive
	and	rvb, rva, #0x0700	@ rvb <- num input args (raw int shftd)
	set	rvc, rva
	tst	rvc, #0x1000		@ is this a direct function tag?
	itE	eq
	seteq	rva, sv1		@	if not, rva <- prim code addres
	lsrne	rva, rva, #16		@	if so,  rva <- code adr or fent
	tst	rvc, #0x2000		@ indir fun or cod-strt adr in upr 16b?
	itT	eq
	seteq	sv4, null		@	if so,  sv4 <- null
	seteq	sv5, null		@	if so,  sv5 <- null (last-arg)

  .ifdef CODE_OFFSET
  	it	eq
	addeq	rva, rva, #CODE_OFFSET	@	if so,  rva <- adjstd start adr
  .endif

	itTT	ne
	andne	sv4, rva, #0xff		@	if not, sv4 <- startup value
	setne	sv5, sv1		@	if not, sv5 <- pre-ntry ret adr
	vcrfine	rvc, glv, 16		@	if not, rva <- pre-ntry fun tbl
	itT	ne
	lsrne	rva, rva, #8		@	if not, rvc <- pre-ntry fun idx
	ldrne	rva, [rvc, rva, lsl #2]	@	if not, rva <- pre-ntry fun adr
	set	sv1, sv2		@ sv1 <- argument list
	set	sv2, null		@ sv2 <- '()
	set	sv3, sv2		@ sv3 <- '()
	eq	rvb, #0x0000		@ does prim have 0 (or var num) args?
	it	ne
	nullpne	sv1			@	if not, is arg-list null?
	it	eq
	seteq	pc,  rva		@	if so,  jump to prim
	@ un-list proc's input arg-list into regs sv1 to sv5 and branch to proc
	snoc	sv1, sv2, sv1		@ sv1 <- arg1, sv2 <- (<a2> - <a5> ...)
	eq	rvb, #0x0100		@ does primitive have 1 input arg?
	it	ne
	nullpne	sv2			@	if not, is rest of arglst null?
	it	eq
	seteq	pc,  rva		@	if so,  jump to prim
	snoc	sv2, sv3, sv2		@ sv2 <- arg2, sv3 <- (<a3><a4><a5> ..)
	eq	rvb, #0x0200		@ does primitive have 2 input args?
	it	ne
	nullpne	sv3			@	if not, is rest of arglst null?
	it	eq
	seteq	pc,  rva		@	if so,  jump to prim
	snoc	sv3, sv4, sv3		@ sv3 <- arg3,	sv4 <- (<ar4> <ar5> ..)
	eq	rvb, #0x0300		@ does primitive have 3 input args?
	it	ne
	nullpne	sv4			@	if not, is rest of arglst null?
	it	ne
	snocne	sv4, sv5, sv4		@ 	if not, sv4 <-a4, sv5 <-(<a5>.)
	set	pc,  rva		@ jump to prim w/3-4 args (sv5 is rest)

  .ifdef fast_eval_apply

evlan3:	@ evaluate 3 args for function with 3 vars
	@ dts <- (fun ...)
	@ sv4 <- uarg1
	@ sv3 <- uarg2
	@ sv1 <- uarg3
	@ sv5 <- ()
	set	sv2, sv3		@ sv2 <- uarg2
	evalsv1				@ sv1 <- arg3	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	set	sv3, sv2		@ sv3 <- uarg2
	set	sv2, sv1		@ sv2 <- arg3
evlan2:	@ evaluate 2 args for function with 2 vars (or continue for 3 args / 3 vars)
	@ dts <- (fun ...)
	@ sv2 <- <arg3>
	@ sv3 <- uarg2
	@ sv4 <- uarg1
	@ sv5 <- ()
	set	sv1, sv3		@ sv1 <- uarg2
	evalsv1				@ sv1 <- arg2	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	set	sv3, sv2		@ sv3 <- <arg3>
	set	sv2, sv1		@ sv2 <- arg2
evlan1:	@ evaluate 1 arg for function with 1 var (or continue for 2-3 args / 2-3 vars)
	@ dts <- (fun ...)
	@ sv2 <- <arg2>
	@ sv3 <- <arg3>
	@ sv4 <- uarg1
	@ sv5 <- ()
	set	sv1, sv4		@ sv1 <- uarg1
	set	sv4, sv3		@ sv4 <- <arg3>
	evalsv1				@ sv1 <- arg1	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	set	sv3, sv4		@ sv3 <- <arg3>
	car	sv5, dts		@ sv5 <- function
	pntrp	sv5
	itE	eq
	ldreq	rva, [sv5, #-4]		@ rva <- function's tag
	setne	rva, sv5
	ands	rvc, rva, #0xc000	@ is fun a primitive?
	bne	evlapc			@	if not, jump to compound/compiled process
	@ fun is a primitive
	cdr	dts, dts		@ dts <- (...)
	and	rvb, rva, #0x0700	@ rvb <- num input args of prmtv (raw int, shftd)
	cmp	rvb, #0x0200
	it	mi
	setmi	sv2, null
	it	ls
	setls	sv3, null
	set	rvc, rva
	tst	rvc, #0x1000		@ is this a direct function tag?
	itE	eq
	seteq	rva, sv5		@	if not, 
	lsrne	rva, rva, #16		@	if so,  
	tst	rvc, #0x2000		@ ind fun or code-start adr in upr 16b?
	itTT	eq
	seteq	sv4, null		@	if not, sv4 <- null
	seteq	sv5, null		@	if so,  sv5 <- null == last-arg possibly

  .ifdef CODE_OFFSET
	addeq	rva, rva, #CODE_OFFSET	@	if so,  rva <- adjstd start adr
  	it	eq
  .endif

	seteq	pc,  rva
	and	sv4, rva, #0xff		@	if so,  sv4 <- startup value
	vcrfi	rvc, glv, 16		@	if not, rva <- address of pre-entry function table
	lsr	rva, rva, #8		@	if not, rvc <- pre-entry func idx shftd in place
	ldr	pc,  [rvc, rva, lsl #2]	@	if not, rva <- pre-entry func's code start adrs

evlapc:	@ compound or compiled procedure with 1-3 vars and 1-3 args
	@ sv1 <- arg1
	@ sv2 <- <arg2>
	@ sv3 <- <arg3>
	@ sv5 <- procedure
	@ dts <- (procedure ...)
	ldr	sv5, [sv5]		@ sv5 <- procedure's vars-list = (var1 <var2> <var3>)
	set	sv4, sv1		@ sv4 <- arg1
	snoc	sv1, sv5, sv5		@ sv1 <- var1, sv5 <- (<var2> <var3>)
	cons	sv4, sv1, sv4		@ sv4 <- (var1 . arg1)
	nullp	sv5			@ done?
	beq	evlap1			@	if so,  jump to finish-up for 1 arg
	snoc	sv1, sv5, sv5		@ sv1 <- var2, sv5 <- (<var3>)
	cons	sv2, sv1, sv2		@ sv2 <- (var2 . arg2)
	car	rva, sv4		@ rva <- var1
	cmp	rva, sv1		@ does var1 come before var2?
	itTT	mi
	setmi	sv1, sv4		@	if so,  sv1 <- (var1 . arg1)
	setmi	sv4, sv2		@	if so,  sv4 <- (var2 . arg2)
	setmi	sv2, sv1		@	if so,  sv2 <- (var1 . arg1)
	list	sv4, sv4		@ sv4 <- ((var1/2 . arg1/2))
	cons	sv4, sv2, sv4		@ sv4 <- ((var2/1 . arg2/1) (var1/2 . arg1/2))
	nullp	sv5			@ done?
	beq	evlapx			@	if so,  jump to finish-up for 2-3 args
	car	sv1, sv5		@ sv1 <- var3
	cons	sv3, sv1, sv3		@ sv3 <- (var3 . arg3)
	caar	sv2, sv4		@ sv2 <- var2/1
	cmp	sv1, sv2		@ does var3 come before var2/1?
	bpl	evlap3
	cons	sv4, sv3, sv4		@ sv4 <- ((vr3 . arg3) (vr2/1 . arg2/1) (vr1/2 . arg1/2))
	b	evlapx			@ jump to finish-up for 2-3 args
evlap3:	@ insert var3
	cdr	sv5, sv4		@ sv5 <- ((var1/2 . arg1/2))
	caar	sv2, sv5		@ sv2 <- var1/2
	cmp	sv1, sv2		@ does var3 come before var1/2?
	itEE	mi
	setmi	sv2, sv4
	setpl	sv2, sv5
	setpl	sv5, null
	cons	sv3, sv3, sv5
	setcdr	sv2, sv3
	b	evlapx			@ jump to finish-up for 2-3 args

evlap1:	@ finish-up for 1 var-arg
	list	sv4, sv4		@ sv4 <- ((var1 . arg1))
evlapx:	@ finish-up
	@ sv4 <- new binding frame
	@ dts <- (procedure ...)
	restor	sv3			@ sv3 <- procedure, dts <- (...)
	ldmia	sv3, {rva, sv1, env}	@ rva <-vrlst, sv1 <-adr|bdy, env <-env
	cons	env, sv4, env		@ env <- (new-frame . env)
	pairp	sv1			@ is fun compound (i.e. a list)?
	beq	sqnce			@	if so,  jump to seq over body
	set	pc,  sv1		@ jump to execute its (compiled) code
	
  .endif	@  fast_eval_apply

	
_func_
cmpapp:	@ compound-apply
_func_
cplapp:	@ compiled proc apply
	@ in:	sv1 <- listed-proc or compiled-proc
	@ in:	sv2 <- arg-list
	@ in:	rva <- 32-bit tag of primitive
	set	sv4, sv2		@ sv4 <- val-list
	ldmia	sv1, {sv3, sv5, env}	@ sv3 <- vars-list, sv5 <- address-or-body, env <- env
	sav_rc	sv5			@ dts <- (address-or-body cnt ...)
	call	mkfrm			@ env <- (new-frame  . env)
	restor	sv1, cnt		@ sv1 <- address-or-body, cnt <- cnt, dts <- (...)
	pairp	sv1			@ is fun compound (i.e. a list)?
	it	ne
	setne	pc,  sv1		@	if not, jump to execute its (compiled) code
sqnce:	@ [internal entry] (eg. for begin)
	nullp	sv1			@ no expressions to evaluate?
	it	eq
	seteq	pc,  cnt		@	if so,  return with null
	set	sv2, sv1		@ sv2 <- (exp1 exp2 ...)
_func_
sqnce0:	@ evaluate body of function, one expression at a time
	@ sv2 <- body = (exp1 exp2 ...),  dts <- (env cnt ...)
	snoc	sv1, sv2, sv2		@ sv1 <- exp1,			sv2 <- (exp2 ...)
	nullp	sv2			@ are we at last expression?
	beq	eval			@	if so,  jump to process it as a tail call
	adr	lnk, sqnce0
	pairp	sv1
	beq	sbevll
	varp	sv1
	beq	sbevlv
	b	sqnce0

_func_
cntapp:	@ continuation-apply
	@ in:	sv1 <- continuation
	@ in:	sv2 <- return-value-inside-a-list
	@ in:	rva <- 32-bit tag of primitive
	set	sv4, sv1		@ sv4 <- continuation, saved against bndchk
	ldr	sv1, =var__winders
	bl	bndchk			@ sv1 <- (_winders . winders) or #t
	pntrp	sv3			@ was a binding found?
	itE	eq
	seteq	sv1, sv5		@	if so,  sv1 <- winders
	setne	sv1, null		@	if not, sv1 <- '()
	vecref	sv5, sv4, 0		@ sv5 <- continuation winders list
	eq	sv1, sv5		@ are current winders the same as those stored in cont?
	bne	cntapw			@	 if not, jump to wind/unwind
cntaxt:	@ winders are unwound or inexistent, continue with applying continuation	
	@ sv4 <- [cnt_tag winders (cnt ...) env],  sv2 <- return-value-inside-a-list
	snoc	sv1, sv3, sv2		@ sv1 <- (car ret-val-list), sv3 <- (cdr ret-val-list)
	nullp	sv3			@ are we returning a single value?
	it	ne
	setne	sv1, sv2		@	if not, sv1 <- ret-val-list
	ldmia	sv4, {rva, sv3, env}	@ rva <- winders-list, sv3 <- (cnt ...), env <- env of cont
	snoc	cnt, dts, sv3		@ cnt <- cnt, dts <- (...)
	set	pc,  cnt		@ resume the continuation

.ifndef	r3rs

cntapw:	@
	@ inspired by:	
	@	http://paste.lisp.org/display/49837
	@	http://scheme.com/tspl3/control.html#./control:s38
	@	http://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html
	@
	@ if we get here, the env is assumed to have a binding for _winders
	@ in other words, bndchk is assumed to have returned a binding (now in sv5)
	@ if this is not the case, some code change will be needed in the case
	@ where we unwind the befores (that case seems impossible as we should
	@ re-enter the dynamic context only if this context was set up earlier, thereby
	@ defining a binding for _winders).
	@ maybe an error should be signalled for this case?
	@ in:	sv1 <- current winders list
	@ in:	sv2 <- return-value-inside-a-list
	@ in:	sv4 <- continuation
	@ in:	sv5 <- continuation winders list
	save	sv2			@ dts <- (return-value-inside-a-list ...)
	save	sv3, env, sv4		@ dts <- ((_wnd.wnd) env [cnt wnd (ct .) nv] (ret-val) .)
	set	sv3, sv5
	@ identify shortests winders list
	set	sv5, true		@ sv5 <- #t, assume wndrs,sv1, shorter than cnt-wndrs,sv3
	set	sv2, sv1
	set	sv4, sv3
cntap0:	nullp	sv2
	beq	cntap1
	nullp	sv4
	it	eq
	seteq	sv5, false
	beq	cntap1
	cdr	sv2, sv2
	cdr	sv4, sv4
	b	cntap0
cntap1:	@ sv5 <- #t => unwind the befores (entering dynamic context)
	@ sv5 <- #f => unwind the afters (leaving dynamic context)
	eq	sv5, #t
	beq	cntap4
	@ unwind the afters in winders (sv1)
	save	sv3			@ dts <- (cnt-wnd (_wnd.wnd) env (cnt env .) (ret-val) .)
cntap2:	snoc	sv3, sv4, dts		@ sv3 <- cnt-wnds,sv4<-((_wnds.wnds) nv (cnt nv .) (rv) .)
	snoc	sv4, sv5, sv4		@ sv4 <- (_wnd.wnd), sv5<-(env (cnt env ..) (ret-val) ..)
	cdr	sv1, sv4		@ sv1 <- winders
	eq	sv1, sv3
	beq	cntap3
	snoc	sv1, sv2, sv1		@ sv1 <- 1st winder = (bef . aft),  sv2 <- rmnng winders
	setcdr	sv4, sv2		@ update _winders binding to remaining winders
	car	env, sv5
	cdr	sv1, sv1		@ sv1 <- after
	set	sv2, null
	call	adr__apl
	b	cntap2
cntap3:	cdr	dts, sv5		@ dts <- ((cnt env ...) (ret-val) ...)
	restor	sv4, sv2		@ sv4 <- [cnt wnd (ct .) nv], sv2 <- ret-val-inside-a-list
	b	cntaxt
cntap4:	@ unwind the before in cnt-winders (sv3)
	set	sv4, null
cntap5:	eq	sv1, sv3
	beq	cntap6
	cons	sv4, sv3, sv4
	cdr	sv3, sv3
	b	cntap5
cntap6:	nullp	sv4
	beq	cntap7
	cadr	env, dts		@ env <- env
	save	sv4			@ dts <- (lst-wndr-lsts (_wnd.wnd) env (cnt env .) (rv) .)
	caaar	sv1, sv4		@ sv1 <- before of 1st winder of 1st-list-of-winders
	set	sv2, null
	call	adr__apl
	restor	sv4			@ sv4 <- lst-wnd, dts <- ((_wnd.wnd) nv (cnt nv .) (rv) .)
	snoc	sv3, sv4, sv4		@ sv3 <- 1st-list-of-winders, sv4 <- rst-of-lst-of-winders
	car	sv2, dts		@ sv2 <- (_winders . wndrs)
	setcdr	sv2, sv3
	b	cntap6
cntap7:	cddr	dts, dts		@ dts <- ((cnt env ...) (ret-val) ...)
	restor	sv4, sv2		@ sv4 <- [cnt wnd (ct .) nv], sv2 <- ret-val-inside-a-list
	b	cntaxt
	
.else	@ no dynamic-wind in core
	
cntapw:	@
	b	adr__err

.endif	@ r3rs vs r5rs
	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*------------------------------------------------------------------------------
@  II.A.4.     Expressions
@  II.A.4.1.   Primitive expression types
@  II.A.4.1.1. variable reference:		bndenv (intrnl), mkfrm (intrnl)
@-----------------------------------------------------------------------------*/

	
mkfrm:	@ [internal only]
	@ extend env with a binding frame between vars in sv3 and vals in sv4 
	@ in:	sv3 <- vars-list = (var1 var2 ...) or ((var1 ...) (var2 ...) ...)
	@ in:	sv4 <- val-list
	@ in:	env <- current environment
	@ out:	env <- environment extended with new binding frame
	@ modifies:	rva-rvc, sv1-sv5, env, lnk
	set	sv5, null
	icons	env, sv5, env		@ env <- (() . env) = pre-extended environment
	nullp	sv3			@ is var-list null?
	it	eq
	seteq	pc, cnt
mkfrm0:	@ [internal only]
	@ build new environment frame
	set	sv1, sv3		@ sv1 <- var-list
	pntrp	sv3			@ is var-list a pointer?
	it	eq
	nullpeq	sv4			@	if so,  is val-list null?
	beq	adr__err		@	if so,  jump to report error
	pntrp	sv3			@ is var-list a pointer?
	itTEE	ne
	setne	sv2, sv4		@	if not, sv2 <- val-list
	setne	sv3, null		@	if not, sv3 <- null = remaining vars
	snoceq	sv1, sv3, sv3		@	if so,  sv1 <- var1, sv3 <- (var2 ...)
	snoceq	sv2, sv4, sv4		@	if so,  sv2 <- val1, sv4 <- (val2 ...)
	pntrp	sv1			@ is var1 a pointer?
	it	eq
	careq	sv1, sv1		@	if so,  sv1 <- var1 (from pointer)
	ibcons	sv2, sv1, sv2, sv3	@ sv2 <- ((var1 . val1) var2 ...)
	@ scan frame for insertion point for new binding
	set	rva, sv1		@ rva <- var1
	car	sv3, env		@ sv3 <- binding frame
	set	sv5, null
	nullp	sv3			@ is binding frame empty?
	beq	mkfrm2			@	if so,  jump to add new binding
mkfrm1:	car	sv1, sv3		@ sv1 <- 1st binding
	car	rvb, sv1		@ rvb <- var of 1st binding
	cmp	rva, rvb		@ is var1 < var of 1st binding?
	bmi	mkfrm2			@	if so,  jump to add new binding
	set	sv5, sv3		@ sv5 <- bndngs lst, starting with last nspctd bndng
	cdr	sv3, sv3		@ sv3 <- rest of bindings list
	nullp	sv3			@ no more bindings?
	bne	mkfrm1			@	if not, jump to continue scanning bindings
mkfrm2:	@ insert new binding at insertion point in frame
	cdr	sv1, sv2		@ sv1 <- (var2 ...)
	setcdr	sv2, sv3		@ sv2 <- ((var1 . val1) . rest of binding frame)
	nullp	sv5			@ any bindings before this one?
	itE	eq
	setcareq env, sv2		@	if not, env <- (((vr1.vl1) . rst-bndng-frm) . env)
	setcdrne sv5, sv2		@	if so,  env <- ((bndg1 .. (vr1.vl1).rst-bn-fr).env)
	set	sv3, sv1		@ sv3 <- (var2 ...) = remaining vars to deal with
	nullp	sv3			@ any more vars?
	bne	mkfrm0			@	if so,  jump to keep inserting bndngs in frame
	set	pc,  cnt		@ return

_func_
bndchk:	@ [internal entry]	called using bl from (set! ...), (defined? ...), (expand ...)
	@ in:	sv1 <- var
	@ in:	env <- env
	@ out:	sv3 <- binding or null
	@ out:	sv5 <- binding value
	@ modifies:	sv3, sv5, rva, rvb, rvc
	@ returns via:	lnk
	set	rvc, null
	b	bndche

	/* (_lkp var) */
	PRIMIT	"_lkp", sntx, 1
	@ in:	sv1 <- var-id
	@ out:	sv1 <- value  (or var-id if entering via bndche)
	@ out:	sv3 <- binding
	@ out:	sv5 <- value
	@ keep:	sv2, sv4
	set	lnk, cnt
_func_
sbevlv:	@ [internal entry]
	set	rvc, lnk
_func_
bndche:	@ [internal entry]	
	set	sv5, env		@ sv5 <- env (user env or library-env)
	set	rvb, sv1		@ rvb <- var
_func_
bndene:	@ look through frames
	pairp	sv5			@ done with user env (or env = lib-env)?
	bne	bndenb			@	if so,  jump to scan builtin env
bndenr:	snoc	sv3, sv5, sv5		@ sv3 <- 1st frame, sv5 <- (frame2 ...)
	pntrp	sv3
	bne	bndene
  .ifdef top_level_btree
	pairp	sv3
	bne	bnden0
  .endif
bndenl:	@ look through bindings in frame
	snoc	sv1, sv3, sv3		@ sv1 <- 1st binding
	car	rva, sv1		@ rva <- bkey , key of 1st binding
	cmp	rvb, rva
	bmi	bndene
	it	eq
	cdreq	sv5, sv1
	beq	bndext
	pairp	sv3
	beq	bndenl			@	if not, jump to keep searching
	pairp	sv5
	beq	bndenr			@	if not, jump to keep searching
bndenb:	@ get binding from built-in environment
	tst	rvb, #0xFF000000
	bne	bndnon
	nullp	sv5			@ was env a library-env?
	itE	eq
	vcrfieq	sv1, glv, 13		@ 	if not, sv1 <- built-in env vec
	setne	sv1, sv5		@	if so,  sv1 <- given library env
	rgbf	rva, rvb, 8, 15
	ldr	sv1, [sv1, rva, lsl #2]	@ sv1 <- built-in sub-env vector
	lsr	rva, rvb, #16		@ rvb <- offset in built-in sub-env
	ldr	sv1, [sv1, rva, lsl #2]	@ sv1 <- symbol's binding value (value or address)
	set	sv5, sv1
bndext:	@ exit
	set	sv3, sv1		@ sv3 <- bndng or nul if user-var, val if primitive-var
	set	sv1, rvb		@ sv1 <- var-id, restored
	nullp	rvc			@ exiting from bndchk?
	it	eq
	seteq	pc,  lnk		@	if so,  ret sv3 <- bndng|nul|val,sv5 <- val|nul
	nullp	sv3			@ binding found?
	itTT	ne
	setne	sv1, sv5		@ sv1 <- val
	setne	rva, null		@ rva <- '(), for return to eval (evalsv1 macro)
	setne	pc,  rvc
	b	adr__err		@	if not, report error

bndnon:	@ binding not found for a user-var
	set	sv1, null
	set	sv5, null
	b	bndext

	/* (_dfv var) */
	PRIMIT	"_dfv", sntx, 1
	@ insert a variable binding in an env
	@ in:	sv1 <- var
	@ out:	sv2 <- binding for var
	@ keep:	sv4
	car	sv3, env		@ sv1 <- frame1 of env
	set	sv5, sv1		@ sv5 <- var
	nullp	sv3			@ frame1 exhausted?
	beq	defva6			@	if so,  jump to add new binding
	/* ----------  updated version  ---------- */
  .ifdef top_level_btree
	@ check type of environment frame
	pairp	sv3
	bne	defva1
  .endif
defva5:	@ env frame is list (ordered)
	@ search for a binding for var in frame1
	car	sv2, sv3		@ sv2 <- 1st binding
	car	rva, sv2		@ rva <- var-id from binding
	cmp	sv1, rva		@ var found or less than next var?
	it	eq
	seteq	pc,  cnt		@	if found,  return binding in sv2
	bmi	defva6			@ 	if less than next, jump to insert
	set	sv5, sv3		@ sv5 <- previous rest-of-frame1
	cdr	sv3, sv3		@ sv1 <- new rest of frame1
	nullp	sv3			@ frame1 exhausted?
	bne	defva5			@	if not, jump to keep scanning frame1
defva6:	@ build new binding and insert into frame1
	list	sv2, sv1		@ sv2 <- (var) == new binding for var
	cons	sv3, sv2, sv3		@ sv3 <- ((var) . rest-of-frame1)
	eq	sv5, sv1		@ new binding is head of frame?
	itE	eq
	setcareq env, sv3		@	if so,  update frame1 in env
	setcdrne sv5, sv3		@	if not, cnnct start-of frm1 to updtd rst-of frm1
	set	pc,  cnt
	/* ----------  end of updated version  ---------- */


/*------------------------------------------------------------------------------
@
@  Balanced Binary Tree (ordered) environment frame
@
@-----------------------------------------------------------------------------*/

.ifdef top_level_btree

bnden0:	@ look for var in btree
	vcrfi	sv1, sv3, 0		@ sv1 <- 1st binding
	car	rva, sv1		@ rva <- bkey , key of 1st binding
	cmp	rvb, rva		@ binding found (or < or >)?
	it	eq
	cdreq	sv5, sv1		@	if so,  sv5 <- value of binding
	beq	bndext			@	if so,  jump to normal exit
	it	mi
	vcrfimi	sv3, sv3, 1		@	if  <,  sv3 <- left child
	it	hi
	vcrfihi	sv3, sv3, 2		@	if  >,  sv3 <- right child
	nullp	sv3			@ is child null?
	bne	bnden0			@	if not, jump to continue searching
	b	bndene			@ jump back to resume search

defva1:	@ search for a binding for var in btree (env frame1)
	@ and create one if needed (+ re-balance the btree)
	vcrfi	sv2, sv3, 0		@ sv2 <- 1st binding
	car	sv5, sv2		@ sv5 <- var-id from binding
	cmp	sv1, sv5
	it	eq
	seteq	pc,  cnt
	set	sv2, sv3		@ sv2 <- binding (will be parent)
	it	mi
	vcrfimi	sv3, sv2, 1		@ sv3 <- left  child binding in frame
	it	hi
	vcrfihi	sv3, sv2, 2		@ sv3 <- right child binding in frame
	nullp	sv3			@ frame1 exhausted?
	bne	defva1			@	if not, jump to continue searching
	@ build new binding and insert into frame1
	save	sv4, cnt
	set	sv4, null
	list	sv1, sv1		@ sv1 <- (var) == new binding
	vctr3	sv3, sv1, sv3, sv4	@ sv3 <- #(binding () ())
	car	sv1, sv1
	cmp	sv1, sv5
	it	mi
	vcstimi	sv2, 1, sv3
	it	hi
	vcstihi	sv2, 2, sv3
	vcrfi	sv2, sv3, 0		@ sv2 <- binding
	@ call the btree balancing function 
	save	sv2
	car	sv1, env
	call	adr__bal
	setcar	env, sv1
	restor	sv2, sv4, cnt
	set	pc,  cnt

	/* (_bal btree) */
	PRIMIT	"_bal", pfun, 1
	@ balance an ordered binary tree
	@ in:	sv1 <- ordered-btree
	@ out:	sv1 <- balanced copy of ordered-btree
	@ mods:	rva-rvc, sv1-sv5
	set	sv3, sv1		@ sv3 <- source-frame (btree)
	set	sv2, null
	set	sv4, null
	set	sv5, null
	vctr3	sv1, sv2, sv4, sv5	@ sv1 <- #(() () ())
	save	sv4, sv1		@ dts <- (end-indicator root-of-frame-flat-copy ...)
	set	sv5, #i0		@ sv5 <- 0 = number of bindings in frame (scheme int)
	@ copy-flatten btree
bala_0:	nullp	sv3
	beq	bala_4
bala_1:	vcrfi	sv4, sv3, 1		@ sv4 <- source-frame.left-child
	nullp	sv4
	beq	bala_2
	save	sv3			@ dts <- (source-frame ...)
	set	sv3, sv4		@ sv3 <- source-frame = source-frame.left-child
	b	bala_1
bala_4:	@
	restor	sv3
	nullp	sv3
	beq	bala_d
	set	sv4, null
bala_2:	add	sv5, sv5, #4
	vcrfi	sv2, sv3, 0		@ sv2 <- source-frame.binding
	vctr3	sv2, sv2, sv4, sv5	@ sv2 <- #(() () sv5)
	vcsti	sv2, 2, sv4		@ sv2 <- #(() () ())
	vcsti	sv1, 2, sv2	@ dest-frame.right-child = copy
	set	sv1, sv2	@ sv1 <- dest-frame = copy
	vcrfi	sv3, sv3, 2	@ sv3 <- source-frame = source-frame.right-child
	b	bala_0
bala_d:	@ vine-to-tree:	balance the flat btree-copy
	int2raw	rva, sv5	@ rva <- number of bindings in env (raw int)
	restor	sv5		@ sv5 <- root-of-btree-flat-copy
	cmp	rva, 2
	bmi	balan3
balan1:	asr	rva, rva, 1	@ rva <- size             = size div 2
	set	rvb, rva	@ rvb <- count            = size
	set	sv1, sv5
balan2:	@ for-loop
	set	sv4, sv1
	vcrfi	sv2, sv1, 2	@ sv2 <- child            = scanner.right
	vcrfi	sv1, sv2, 2	@ sv1 <- new-scanner      = child.right
	vcsti	sv4, 2, sv1
	vcrfi	sv4, sv1, 1	@ sv4 <- new-scanner.left
	vcsti	sv2, 2, sv4	@        child.right      = new-scanner.left
	vcsti	sv1, 1, sv2	@        new-scanner.left = child
	subs	rvb, rvb, 1	@ rvb <- count            = count - 1, is it zero?
	bne	balan2
	cmp	rva, 2
	bpl	balan1
balan3:	@ done
	vcrfi	sv1, sv5, 2
	set	pc,  cnt

.endif

/*------------------------------------------------------------------------------
@
@  error handling
@
@-----------------------------------------------------------------------------*/

	/* (_catch error) */
	PRIMIT	"_catch", pfun, 0
	@ in:	sv1 <- error
	@ out:	never exits
	@ keep:	none
	vecref	env, glv, 7
	set	sv2, sv1
	set	sv5, null
	vecset	glv, 14, sv5
	vecset	glv, 15, sv5
	b	errore

	/* (throw func-name-string invalid-arg) */
	PRIMIT	"throw", pfun, 2
	@ in:	sv1 <- function name string
	@ in:	sv2 <- arg that caused error
	set	sv4, sv1
	set	sv1, sv2
_func_
error4:	@ [internal entry]
	@ in:	sv1 <- arg that caused error
	@ in:	sv4 <- function name string
	ldr	dts, =stkbtm		@ dts <- (...)
	ldr	sv3, =sthrow		@ sv3 <- "throw"
	set	sv5, null
	llcons	sv1, sv4, sv3, sv1, sv5	@ sv1 <- (funcnamestring "throw" invalid-arg/type)
	ldr	sv2, =var_quote
	lcons	sv2, sv2, sv1, sv5
errore:	@ [intenal entry 2] (eg. from catch)
	ldr	sv1, =var__catch
	lcons	sv1, sv1, sv2, sv5
	@
	@ throw could also send dts in arg list to _catch so _catch could give a trace of what
	@ went wrong and/or attempt restart? (would require more than just stack though
	@ -- more like the full context	stored during interrupt - that is feasible indeed but
	@    not done here)
	@
	b	eval

	/* (_err arg) */
	PRIMIT	"_err", pfun, 1
	@ generic error (eg. bndenv, mkfrm)
	@ in:	sv1 <- function name string	
	ldr	sv4, =core_		@ sv1 <- address of "core" (sv2 is the wrong type)
	b	error4


/*------------------------------------------------------------------------------
@ I.D.4. version
@-----------------------------------------------------------------------------*/

	/* (_GLV) */
	PRIMIT	"_GLV", pfun, 0
	@ return the built-in env
	@ or sets built-in env to input arg
	set	sv1, glv
	set	pc,  cnt

/*------------------------------------------------------------------------------
@
@  II.I. SCHEME FUNCTIONS AND MACROS:	ADDENDUM -- Level 2
@  II.I.1. definitions:				defined?
@  II.I.2. file system:				unlock
@  II.I.3. pack/unpack:				packed?, unpack, pack
@
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@  II.I.3. pack/unpack:		packed-data-length, packed?, unpack, pack
@-----------------------------------------------------------------------------*/

	/* (address-of obj ofst) */
	PRIMIT	"address-of", padrof, pfun, 2
	@ in:	sv1 <- obj
	@ in:	sv2 <- ofst
	mkvu84	sv3			@ sv3 <- #vu8(space-for-4-items)
	pntrp	sv1
	itEE	eq
	seteq	rvc, sv1
	lsrne	rvc, sv1, #2
	lslne	rvc, rvc, #4
	add	rvc, rvc, sv2, lsr #2
	str	rvc, [sv3]
	set	sv1, sv3
	set	pc,  cnt

	/* (packed-data-set! bv pos val) */
	PRIMIT	"packed-data-set!", pkdtst, pfun, 3
	@ in:	sv1 <- packed-object
	@ in:	sv2 <- position from end of data portion of packed-object
	@ in:	sv3 <- item to store at end of packed object
	ldr	rvb, [sv1, #-4]		@ sv1 <- bytevector length (scheme int)
	lsr	rvb, rvb, #8		@ rvb <- number of bytes in packed object (raw int)
	bl	pkdtsz			@ rvb <- number of data bytes in packed object (raw int)
	bic	rva, sv2, #0x03
	add	rvb, rvb, rva
	pntrp	sv3
	itE	eq
	ldreq	rva, [sv3]
	asrne	rva, sv3, #2
	str	rva, [sv1, rvb]
	b	adr_npofxt

_func_
pkdtsz:	@ return the size of code in a packed object
	@ in:	rvb <- bytevector length
	@ out:	rvb <- size of code in packed bytevector
	@ modifies:	rva, rvb, rvc
	bic	rvc, rvb, #3		@ rvc <- initial guess for number of data bytes
pkdts0:	add	rva, rvc, #31		@ rva <- trial total bytevector size adjusted
	add	rva, rvc, rva, lsr #5	@ rva <- trial total bytevector size (final)
	eq	rva, rvb		@ is data-section size correct?
	itT	ne
	subne	rvc, rvc, #4		@	if not, rvc <- next data size guess
	bne	pkdts0			@	if not, jump back to test next guess
	set	rvb, rvc		@ rvb <- number of data bytes
	set	pc,  lnk
	
_func_
pkisof:	@ raise ne flag if item from code vector is an offset
	@ in:	sv3 <- offset in source
	@ in:	sv4 <- source packed byetvector
	@ in:	sv5 <- offset to bitfield end in source packed bytevector
	@ modifies:	rva, rvb
	lsr	rvb, sv3, #4
	and	rvb, rvb, #0x07
	set	rva, 1
	lsl	rvb, rva, rvb		@ rvb <- offset determination mask
	int2raw	rva, sv5
	sub	rva, rva, sv3, lsr #7	
	ldrb	rva, [sv4, rva]
	tst	rva, rvb
	set	pc,  lnk

	/* (unpack packed-object destination) */
	PRIMIT	"unpack", pfun, 2
	@ in:	sv1 <- packed-object
	@ in:	sv2 <- null for heap, positive for above heap, negative for flash
	vu8len	sv5, sv1		@ sv5 <- number of bytes, as scheme int
	cmp	sv5, #0x15		@ is num bytes less than 5? (i.e. object is an imm)
	itT	mi
	vcrfimi sv1, sv1, 0		@	if so,  sv1 <- item stored in object
	setmi	pc,  cnt		@	if so,  return sv1
	@ get number of data bytes in packed object (excluding bitfield)
	int2raw	rvb, sv5		@ rvb <- number of bytes in packed object (raw int)
	bl	pkdtsz			@ rvb <- number of data bytes in packed object (raw int)
	raw2int	sv3, rvb		@ sv3 <- number of data bytes in packed object (scheme int)
	sub	sv5, sv5, #4		@ sv5 <- offset to bitfield in packed object (scheme int)
	@ dispatch on destination
  .ifdef LIB_TOP_PAGE
	tst	sv2, #0x80000000	@ unpacking to flash?
	bne	unpafl			@	if so,  jump to that case
  .endif
	nullp	sv2			@ unpacking to heap?
	bne	unpaca			@	if not, jmp to unpack above-heap
	@ unpacking to heap, allocate memory for destination
	bl	adr__alo		@ rva <- obj adrs (vu8-tagged), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- adrs of next free cell
	sub	sv4, rva, rvb		@ sv4 <- adrs of dest (bytevector), [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
unpac0:	@ set stack for unpacking
	save	sv1, sv4		@ dts <- (source-bv dest-address ...)
	sub	sv1, sv3, #16		@ sv1 <- offset in target
unpac1:	@ unpack the data to RAM or flash
	car	sv4, dts		@ sv4 <- source bytevector
	sub	sv3, sv3, #16
  .ifndef cortex
	ldr	rvc, [sv4, sv3, lsr #2]	@ rvc <- current word of code from bytevector
  .else
	lsr	rvc, sv3, #2
	ldr	rvc, [sv4, rvc]		@ rvc <- current word of code from bytevector
  .endif
	bl	pkisof			@ is item from source bv an offset (raise ne flag if so)?
	cadr	sv4, dts		@ sv4 <- dest start-address (eg. lib-code-start-in-flash)
	beq	unpac2
	add	rvc, rvc, sv4		@ rvc <- target address of offset
	eq	sv1, sv3		@ unpacking to RAM?
	it	eq
	subeq	rvc, rvc, #4		@ 	if so,  rvc <- adjstd target ofst (tag overwritten)
unpac2:	@ continue
  .ifdef LIB_TOP_PAGE
	eq	sv1, sv3		@ unpacking to RAM (heap or non-heap)?
	it	ne
	caddrne	sv4, dts		@	if not, sv4 <- pseudo-file descriptor
	it	ne
	vcrfine	sv4, sv4, 3		@	if not, sv4 <- target buffer for writing to flash
  .endif
	sub	sv1, sv1, #16
  .ifndef cortex
	str	rvc, [sv4, sv1, asr #2]	@ store item in destination
  .else
	asr	rvb, sv1, #2
	str	rvc, [sv4, rvb]		@ store item in destination
  .endif
	add	sv1, sv1, #16
  .ifdef LIB_TOP_PAGE
	eq	sv1, sv3		@ unpacking to RAM (heap or non-heap)?
	beq	unpac3			@	if so,  jump to continue
	eq	sv1, #0x11		@ page complete?
	bne	unpac3			@	if not, jump to continue
	caddr	sv4, dts
	bl	libwrt
	sub	sv2, sv2, #F_PAGE_SIZE
	set	rvc, F_PAGE_SIZE
	raw2int	sv1, rvc
	add	sv1, sv1, #16
unpac3:	@ continue
  .endif
	@ check if we're done unpacking
	eq	sv3, #i0		@ done unpacking?
	it	ne
	subne	sv1, sv1, #16		@	if not, sv1 <- ofst in which to str nxt code wrd
	bne	unpac1			@	if not, jump back to continue unpacking	
	@ finish up (adjust sv4 if top unpacked item was a pair or list)
	restor	sv4, sv4
  .ifdef LIB_TOP_PAGE
	orr	sv4, sv4, #0x04		@ sv4 <- adjusted sized item start (needed by flash target)
  .endif	
	ldr	rva, [sv4, #-4]
	@ =*=*=*=*= nothing to do here it seems
	and	rvb, rva, #0x47
	eq	rvb, #0x47
	itT	ne
	andne	rvb, rva, #0x07
	eqne	rvb, #0x03
	it	ne
	subne	sv4, sv4, #4
  .ifdef LIB_TOP_PAGE
	eq	sv1, sv3
	itT	ne
	cdrne	dts, dts
	blne	funlok
  .endif
unpaxt:	set	sv1, sv4
	set	pc,  cnt		@ return

_func_
unpaca:	@ unpacking to above-heap RAM
	@ in:	sv3 <- number of data bytes in object (scheme int)
	@ in:	rvb <- number of data bytes in object (raw int)
	swi	run_prvlgd		@ privil, no irq, for hptp2mpu, mrc p15
	bl	ahpalo			@ rva <- obj insertion point or #f
	eq	rva, #f			@ above-heap alloc successful?
	it	eq
	seteq	sv4, false		@	if not, sv4 <- #f
	beq	unpax0			@	if not, jump to exit with false
	add	sv4, rva, #4		@ sv4 <- address of object
	int2raw	rva, sv3
	lsl	rva, rva, #8
	orr	rva, rva, #bytevector_tag
	str	rva, [sv4, #-4]
	swi	run_normal		@ enable interrupts (user mode)
	b	unpac0

unpax0:	@ return to normal mode and exit
	swi	run_normal
	b	unpaxt

_func_
ahpalo:	@ allocate space above the heap, move MPU barrier if needed
	@ should be entered in privileged, no IRQ mode (privileged user mode)
	@ in:	sv3 <- number of bytes of object to allocate
	@ out:	rva <- object insertion point (8B ali) or #f
	@ modifies:	rva, rvb, rvc, sv2

	bic	sv2, lnk, #lnkbit0	@ sv2 <- lnk, saved against gc, hptp2mpu

  .ifdef enable_MPU
    .ifdef MPU_is_in_MMU
      @ check if object insertion can be done without moving 1MB barrier and gc

	int2raw	rvb, sv3		@ rvb <- number of bytes of data
	add	rvb, rvb, #0x4f		@ rvb <- num bytes + 79 (63 ali 16 spc)
	bic	rvb, rvb, #0x3f		@ rvb <- num bytes + spacer (64B ali)
	vcrfi	rvc, glv, 9		@ rvc <- prior object insertion point
	sub	rvc, rvc, rvb		@ rvc <- insrtn pnt of object (schint)
	sub	rvc, rvc, #(1<<20)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	cmp	rvc, rva		@ will inserted obj cross 1MB barrier?
	bpl	ahpalc			@	if not, jump to skip gc

    .endif
  .endif

	bl	gc			@ rva <- num free mem bytes, perform gc
  .ifndef mark_and_sweep
	vcrfi	rvb, glv, 9		@ rvb <- heaptop0 -- from global vector
	cmp	fre, rvb		@ is upper heap in use?
	it	pl
	blpl	gc			@	if so,  rva <- free mem bytes/gc
	lsl	rva, rva, #1		@ rva <- free bytes *2 (both heaps)
  .endif
	int2raw	rvb, sv3		@ rvb <- number of bytes of data
  .ifndef cortex_a8
    .ifndef enable_MPU
	add	rvb, rvb, #0x1f		@ rvb <- num obj byts+31 (15 ali 16 spc)
	bic	rvb, rvb, #0x0f		@ rvb <- num bytes need + spacer 16B ali
    .else
      .ifndef mark_and_sweep
	add	rvb, rvb, #0x4f		@ rvb <- num obj byts+79 (63 ali 16 spc)
	bic	rvb, rvb, #0x3f		@ rvb <- num bytes need + spacer 64B ali
      .else
	add	rvb, rvb, #0x2f		@ rvb <- num obj byts+47 (31 ali 16 spc)
	bic	rvb, rvb, #0x1f		@ rvb <- num bytes need + spacer 32B ali
      .endif
    .endif
  .else	@ cortex-a8
	add	rvb, rvb, #0x4f		@ rvb <- num bytes + 79 (63 ali 16 spc)
	bic	rvb, rvb, #0x3f		@ rvb <- num bytes + spacer 64B ali
	bic	rva, rva, #0x3E		@ rva <- adj for 64B L2 cache line len
  .endif

	orr	lnk, sv2, #lnkbit0	@ lnk <- lnk, restored
	cmp	rvb, rva		@ is enough room available?
	itT	pl
	setpl	rva, false		@	if not, rva <- #f
	setpl	pc,  lnk		@	if not, exit with false

  .ifndef mark_and_sweep
	@ update heaptop0 and heaptop1
	vcrfi	rva, glv, 10		@ rva <- heaptop1 -- from global vector
	sub	rva, rva, rvb		@ rva <- heaptop1 minus size of code
    .ifdef cortex_a8
	bic	rva, rva, #0x3E		@ rva <- adj for 64-byte L2 cache line
    .endif
	vcsti	glv, 10, rva		@ store it in global vector as heaptop1
	vcrfi	rvc, glv, 9		@ rvc <- heaptop0 -- from global vector
	sub	rvc, rvc, rvb, lsr #1	@ rvc <- heaptop0 minus size of code / 2
    .ifdef cortex_a8
	bic	rvc, rvc, #0x1E		@ rvc <- adj for 64-byte L2 cache line
    .endif
	vcsti	glv, 9, rvc		@ store heaptop0 back in global vector
	vcsti	glv, 1, rvc		@ store it in global vector as heaptop
  .else
	@ update heaptop
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
    .ifdef enable_MPU
      .ifdef MPU_is_in_MMU

ahpalc:	@ we assume that no packed object is larger than 1MB
@	cmp	rva, #(2<<20)		@ is 2 or 3 MB avail? -- BUG -- need to consid heapbottom
@	bmi	stsyer			@	if not, jump to exit with error
	vcrfi	rvc, glv, 9		@ rvc <- prior object insertion point
	sub	rvc, rvc, rvb		@ rvc <- insrtn pnt of object (schint)
	vcsti	glv, 9, rvc		@ store object insertion point in glv
	sub	rvc, rvc, #(1<<20)
	cmp	rvc, rva		@ will inserted obj cross 1MB barrier?
	submi	rva, rva, #(1<<20)	@	if so,  rva <- new barrier
	add	rva, rva, rvb

      .endif
    .endif
	sub	rva, rva, rvb		@ rva <- heaptop minus size of code
    .ifdef cortex_a8
	bic	rva, rva, #0x3E		@ rva <- adj for 64-byte L2 cache line
    .endif
	vcsti	glv, 1, rva		@ store it in global vector as heaptop
  .endif
  .ifdef enable_MPU
	bl	hptp2mpu		@ update MPU with new heaptop(s)
  .endif
  .ifndef enable_MPU
	add	rva, rva, #7		@ rva <- object insertion point
  .else
      .ifdef MPU_is_in_MMU
	vcrfi	rva, glv, 9		@ rva <- object insertion pnt (schint)
      .endif
	add	rva, rva, #39		@ rva <- object insertion point
  .endif

	orr	lnk, sv2, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk		@ return


.ifdef	LIB_TOP_PAGE

unpafl:	@ unpacking to flash
	set	rva, F_PAGE_SIZE	@ rvb <- flash page size (to align num bytes needed)
	sub	rva, rva, #1		@ rvb <- flash page size - 1 (to align)
	add	rvb, rvb, rva		@ rva <- number of bytes needed + alignment value
	bic	rva, rvb, rva		@ rva <- number of bytes needed aligned to flash page size
	raw2int	sv4, rva		@ sv4 <- number of bytes needed, saved against flok
	bl	flok			@ acquire file system lock
	vcrfi	rva, glv, 12		@ rva <- flash lib start page from glv
	nullp	rva			@ no flash lib yet?
	it	eq
	seteq	rva, LIB_TOP_PAGE	@	if so,  rva <- adress of top of flash lib
	sub	rvc, rva, sv4, lsr #2	@ rvc <- possible new flash lib start
	set	rvb, LIB_BOTTOM_PAGE	@ rvb <- flash lib bottom page
	cmp	rvc, rvb		@ is enough free flash space available?
	bmi	unpfer			@	if not, jump to report error
  .ifdef SHARED_LIB_FILE
	vcrfi	sv2, glv, 11		@ sv2 <- file flash end page (crunch start, pseudo sch int)
	bic	sv2, sv2, #i0		@ sv2 <- file flash crunch space address
	bl	pgsctr			@ rva <- sector of crunch space
	set	sv2, rvc		@ sv2 <- possible new flash lib start, for pgsctr
	set	rvc, rva		@ rvc <- sector of crunch space, saved against pgsctr
	bl	pgsctr			@ rva <- sector of possible new flash lib start
	set	rvb, rva		@ rvb <- sector of possible new flash lib start, for adlber
	cmp	rvc, rvb		@ will new flash lib encroach into crunch space?
	bmi	unpaf1			@	if not, jump to continue
	@ new flash lib will overlap old crunch space, check that crunch space can be moved down
	sub	rvb, rvb, #1		@ rvb <- possible new crunch sector
	ldr	rva, =lib_sectors	@ rva <- lib sectors
	ldr	rva, [rva, rvb, lsl #2]	@ rva <- page start address of possible new crunch sector
	ldr	rvc, [rva]		@ rvc <- content of 1st word of sector
	mvns	rvc, rvc		@ is this sector free?
	itT	eq
	orreq	rva, rva, #i0		@	if so,  rva <- new crnch spc adrs (pseudo sch int)
	vcstieq	glv, 11, rva		@	if so,  set new fil flsh end pg(crnch start) in glv
	beq	unpaf1			@	if so,  jump to keep going
	@ check if enough flash (erased files) can be reclaimed, if so, do it and restart unpack
	vcrfi	rvb, glv, 11		@ rvb <- current crunch space address (pseudo scheme int)
	bic	rvb, rvb, #i0		@ rvb <- current crunch space address
	sub	rvc, rvb, rva		@ rvc <- space needed by reclamation process
	set	sv2, F_START_PAGE	@ sv2 <- address of flash start page for files
unpag0:	@ add-up reclaimable space up to needed amount or end of file flash
	cmp	sv2, rvb		@ is page >= file end page?
	bpl	unpfer			@	if so,  jump to error (not enough space to reclaim)
	tbrfi	rva, sv2, 0		@ rva <- potential file ID for page
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of next FLASH page to check
	and	rva, rva, #0xff		@ rva <- lower byte of 1st word
	eq	rva, #0xfd		@ is this a valid file (not deleted)?
	beq	unpag0			@	if so,  jump to continue scanning file flash
	subs	rvc, rvc, #F_PAGE_SIZE	@ rvc <- remaining space req if page crunchd, is it enough?
	bne	unpag0			@	if not, jump to continue scanning file flash
	@ sufficient space can be reclaimed, perform crunch
	save	cnt
	call	adr_fsc
	restor	cnt
	@ restart the unpack process
	set	sv2, 5
	ngint	sv2, sv2
	b	adr_unpack
	
unpaf1:	@ enough flash space is available, keep going
	set	rvc, sv2		@ rvc <- new flash lib start, restored
  .endif @ SHARED_LIB_FILE
	set	sv2, rvc		@ sv2 <- new flash lib start page (write target)
	bl	mkfdsc			@ sv4 <- blank output file descr (with buffer and 5 items)
	save	sv1, sv2, sv4		@ dts <- (source-bv lib-code-start-in-flash file-descr ...)
	vcrfi	sv4, glv, 12		@ sv4 <- old flash-lib start address from glv
	nullp	sv4
	it	eq
	seteq	sv4, LIB_TOP_PAGE
	vcsti	glv, 12, sv2		@ set new flash-lib start address into glv
	sub	sv2, sv4, #F_PAGE_SIZE	@ sv2 <- last flash page in target
	set	rvb, F_PAGE_SIZE
	sub	rvb, rvb, #1
	lsl	rvb, rvb, #2
	orr	rvb, rvb, #i0
	and	sv1, sv3, rvb		@ sv1 <- offset of last word (realtive to F_PAGE_SIZE)
	eq	sv1, #i0		@ unpacking an exact multiple of F_PAGE_SIZE?
	it	eq
	addeq	sv1, rvb, #4		@	if so,  sv1 <- adjusted last word offset
	b	unpac1			@ jump to unpack
	
.endif	@ LIB_TOP_PAGE

unpfer:	@ error: not enough free space
	set	sv4, false
	bl	funlok
	b	unpaxt


.ifndef	exclude_pack

	/* (pack object) */
	PRIMIT	"pack", pfun, 0
	@ in:	sv1 <- (object)
	@ vars:	sv3 <- target offset,  sv4 <- source list,  sv5 <- relocation list
	save	sv1, sv1		@ dts <- ((object) (object) ...)
	set	sv3, i0			@ sv3 <- 0, size of thing to move, as scheme_int
	set	sv5, null		@ sv5 <- empty relocation list
pack_1:	restor	sv4			@ sv4 <- (adr1 adr2 .) = src obj lst, dts <- ((obj) .)
	nullp	sv4			@ is source object list null?
	beq	pack_8			@	if so,  jump to build position-independent object
	cdr	sv1, sv4		@ sv1 <- (addr2 ...) = rest of source object list
	save	sv1			@ dts <- ((addr2 ...) (object) ...)
	car	sv4, sv4		@ sv4 <- addr1 = 1st item in source object list
pack_2:	@
	bl	pkckfr			@ sv2 <- '() if item needs reloc and isn't in reloc list
	nullp	sv2			@ is relocation needed?
	bne	pack_1			@	if not, jump to process next item
	@ add relocation frame to relocation list
	set	sv1, sv4		@ sv1 <- source_address
	@ see if sv4 is sized object
	pairp	sv4			@ is object a list?
	itE	eq
	seteq	sv2, sv3
	addne	sv2, sv3, #16
	bcons	sv5, sv1, sv2, sv5	@ sv5 <- updtd reloc lst ((src_adrs . trgt_ofst) reloc-lst)
	pairp	sv4			@ is object a list?
	bne	pack_5			@	if not, jump to process a sized object
	@ sv4 is a list, prepare to process car and cdr
	car	sv1, sv4		@ sv1 <- (car object)
	restor	sv2			@ sv2 <- (addr2 ...),		dts <- ((object) ...)
	bcons	dts, sv1, sv2, dts	@ dts <- (((car obj) addr2 ...) ...)
	add	sv3, sv3, #32		@ sv3 <- updtd num bytes to cpy & dest offst (+8, sch int)
	cdr	sv4, sv4		@ sv4 <- (cdr lst)
	b	pack_2			@ jump to process cdr

pack_5:	@ sv4 is a sized object
	ldr	rvc, [sv4, #-4]		@ sv4 <- full tag of sized object
	tst	rvc, #0x04		@ is object a rat/cpx?
	it	eq
	addeq	sv3, sv3, #32		@	if so,  sv3 <- updtd sz of obj, in bytes (sch int)
	beq	pack_1			@	if so,  jump to process next item
	@ =*=*=*=*=*= nothing to do here it seems
	tst	rvc, #0x80		@ is object a procedure?
	itTE	ne
	setne	rvc, 0
	setne	rva, 3
	lsreq	rva, rvc, #8		@ 	if not, rva <- sz of item in byts or wrds (raw int)
	tst	rvc, #0x30		@ is object gc-eable
	it	eq
	lsleq	rva, rva, #2		@	if so,  rva <- number of bytes in object
	add	rvb, rva, #11		@ rvb <- num of bytes to alloc + header + 7-bytes to align
	bic	rvb, rvb, #7		@ rvb <- num of bytes to alloc, with header, 8-byte aligned
	add	sv3, sv3, rvb, LSL #2	@ sv3 <- updated size of object, in bytes, as scheme int
	bne	pack_1			@	if not,  return (non gc-eable)
	@ item is gc-eable sized obj, scan for pntrs & store them in source list for reloc
	raw2int	sv1, rva
pack_7:	
	sub	sv1, sv1, #16
	cmp	sv1, #i0		@ done?
	bmi	pack_1			@	if so,  jump to process remaining objects
	wrdref	rva, sv4, sv1		@ rva <- array item
	pntrp	rva			@ is item a pointer?
	bne	pack_7			@	if not, jump to process next array item
	restor	sv2			@ sv2 <- (addr2 ...),  dts <- ((object) ...)
	set	rvb, 12			@ rvb <- 12, number of data bytes to allocate
	bl	adr__alo		@ rva <- allocated block
	add	rva, rva, #4		@ rva <- address of new cons
	stmdb	rva, {rva, dts}		@ fre - 1 <- ( ... dts )
	wrdref	rva, sv4, sv1		@ rva <- array item (pointer == address)
	str	rva, [fre, #7]		@ fre - 1 <- ( ( address ...) dts )
	str	sv2, [fre, #11]		@ fre - 1 <- ( ( address addr2 ... ) dts )
	add	rva, fre, #15		@ rva <- address of next free cell
	sub	dts, rva, #16		@ dts <- address of new dts, [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	b	pack_7			@ jump to keep processing vector items
	
_func_	
pkckfr:	@ see if item in sv4 is relocated/needs relocation
	@ in:	sv4 <- item to check
	@ out:	sv2 <- #i0 == no need to relocate / not relocated,
	@ out:	sv2 <- '() == item not on reloc list (needs relocation)
	@ out:	sv2 <- other == frame from reloc list
	@ modifies:	sv1, sv2, rva
	set	sv2, i0			@ sv2 <- indicator value (scheme 0)	
	pntrp	sv4			@ is item (sv4) a pointer?
	it	ne
	setne	pc,  lnk		@	if not, return
	vcrfi	rva, glv, 18		@ rva <- cpu(n) heapbottom, pseudo-int
	bic	rva, rva, #3
	cmp	sv4, rva		@ is sv4 > heapbottom ?
	itT	pl
  .ifndef mark_and_sweep
	vcrfipl	rva, glv, 10		@	if so,  rva <- heaptop1, from global vector
  .else
	vcrfipl	rva, glv, 1		@	if so,  rva <- heaptop -- from global vector 
  .endif
	cmppl	rva, sv4		@	if so,  is RAMPTOP > sv4 ?
	it	mi
	setmi	pc,  lnk		@	if not, return
	@ check relocation list
	set	sv2, sv5		@ sv2 <- relocation list
pkckfl:	nullp	sv2			@ done scanning relocation list?
	it	eq
	seteq	pc,  lnk		@	if so,  return
	caar	sv1, sv2		@ sv1 <- next relocation list source address
	eq	sv1, sv4		@ was source object address already relocated?
	it	eq
	seteq	pc,  lnk		@	if so,  return
	cdr	sv2, sv2		@ sv2 <- rest of relocation list
	b	pkckfl			@ jump to keep searching for source object address (sv4)

pack_8:	@ build the pic from the relocation list and object
	@ sv3 <- object total size
	@ sv5 <- reloc list ((src_addrss . trgt_offst) ...)
	@ dts <- ((object) ...)
	restor	sv4			@ sv4 <- (obj) being packed (eg. if not a list), dts <- (.)
	int2raw	rvb, sv3		@ rvb <- number of data bytes in target object (raw int)
	add	rva, rvb, #31
	add	rvb, rvb, rva, lsr #5
	add	rvb, rvb, #12		@ rvb <- number of bytes to allocate
	bl	adr__alo		@ rva <- allocated block of rvb bytes
	str	dts, [fre, #3]		@ fre - 1  <- (... dts )
	int2raw	rva, sv3		@ rva <- number of data bytes in target object (raw int)
	nullp	sv5			@ was nothing relocated?
	itEE	eq
	addeq	rva, rva, #4		@	if so,  rva <- adjstd num byts allocd (for 1 word)
	addne	rvc, rva, #31
	addne	rva, rva, rvc, lsr #5
	lsl	rva, rva, #8		@ rva <- number of bytes, shifted in place
	orr	rva, rva, #bytevector_tag @ rva <- full (sized) tag for pic
	str	rva, [fre, #7]		@ store tag in target
	add	rva, fre, #11		@ rva <- address of object
	str	rva, [fre, #-1]		@ fre - 1 <- (target_relocated_object ...) = new dts
	bic	rva, fre, #0x01		@ rva <- target address
	add	rvc, rvb, #4
	add	rva, rva, rvc		@ rva <- address of next free cell
	sub	dts, rva, rvc		@ dts <- address of new dts, [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	car	sv1, dts		@ sv1 <- empty packed object
	@ clear out object contents (especially bitfield)
	ldr	rva, [sv1, #-4]
	lsr	rva, rva, #8
	add	rva, rva, #3
	bic	rva, rva, #3
	set	rvb, 0
pack0l:	subs	rva, rva, #4
	str	rvb, [sv1, rva]
	bne	pack0l
	@ deal with immediates
	nullp	sv5			@ was nothing relocated?
	itT	eq
	careq	sv4, sv4		@	if so,  sv4 <- object to package
	vcstieq sv1, 0, sv4		@	if so,  store object in pic
	@ sv3 will scan over the relocation list
	set	sv3, sv5		@ sv3 <- relocation list
pack_9:	nullp	sv3			@ are we done with relocation list?
	itT	eq
	restoreq sv1			@ 	if so,  sv1 <- packed object,	dts <- (...)
	seteq	pc,  cnt		@ 	if so,  return
	caar	sv4, sv3		@ sv4 <- source
	pairp	sv4
	bne	packsr
	@ relocate the car
	car	sv4, sv4		@ sv4 <- source car
	bl	pkckfr			@ sv2 <- target offset frame
	set	rvb, 0
	bl	packlp			@ sv1 <- address of target, sv4 <- object or scheme offset
	@ relocate the cdr
	cdaar	sv4, sv3		@ sv4 <- source cdr
	bl	pkckfr			@ sv2 <- target offset frame
	set	rvb, 4
	bl	packlp			@ sv1 <- address of target, sv4 <- object or scheme offset
	cdr	sv3, sv3		@ sv3 <- rest of relocation list
	b	pack_9			@ jump to continue processing relocation list

_func_	
packlp:	@ in:	sv3 <- rest of reloc list, starting with current item
	@ in:	sv4 <- item
	@ in:	rvb <- secondary offset in destination (beyond sv2)
	@ in:	sv5 <- relocation list
	@ out:	sv1 <- address of target object
	@ out:	sv2 <- offset in target
	@ out:	sv4 <- item (if immediate) or its tagged offset (if pair/sized)
	@ modifies:	sv1, sv2, sv4, rva, rvc
	eq	sv2, #i0		@ is sv4 non-relocated?
	it	ne
	cdarne	sv4, sv2		@	if not, sv4 <- ofst to relocated object (sch int)
	car	sv1, dts		@ sv1 <- address of sized object
	cdar	sv2, sv3		@ sv2 <- target_offset (scheme_int)
	add	sv2, sv2, rvb, LSL #2	@ sv2 <- target ofst updated for item position (sch int)
	bne	packl2
	@ store it and return
	wrdst	sv1, sv2, sv4		@ store item
	set	pc, lnk			@ return
packl2:	@ add offset in bitfield and target
	lsr	rvc, sv2, #4
	and	rvc, rvc, #0x07
	set	rva, 1
	lsl	rvc, rva, rvc
	ldr	rva, [sv1, #-4]
	lsr	rva, rva, #8
	sub	rva, rva, #1
	sub	rva, rva, sv2, lsr #7
	ldrb	rva, [sv1, rva]
	orr	rvc, rva, rvc
	ldr	rva, [sv1, #-4]
	lsr	rva, rva, #8
	sub	rva, rva, #1
	sub	rva, rva, sv2, lsr #7
	strb	rvc, [sv1, rva]
	int2raw	rva, sv4
	wrdst	sv1, sv2, rva		@ store item
	set	pc, lnk			@ return

packsr:	@ sized object or rat/cpx (dispatch)
	ldr	rvc, [sv4, #-4]		@ rvc <- item's full tag
	tst	rvc, #0x04		@ is item a rat/cpx?
	it	eq
	seteq	rvb, 4			@	if so,  rvb <- 4, raw offset of object end
	beq	packrc			@	if so,  copy-relocate that rat/cpx item
	@ =*=*=*=*=*=  nothing to do here it seems
	tst	rvc, #0x80		@ is object a procedure?
	it	ne
	setne	rvb, 12
	bne	packs1
	tst	rvc, #0x30		@ is object gc-eable
	beq	packsz
	@ copy non-gceable sized item from source to target
	@ in:	sv3 <- rest of reloc list, starting with current item
	@ in:	rvc <- object tag of item
	@ in:	sv5 <- relocation list
	@ out:	sv3 <- rest of reloc list, starting after current item
	@ modifies:	sv1-sv4, rva, rvb
	lsr	rvb, rvc, #8		@ rvb <- number of data bytes in object
	add	rvb, rvb, #3		@ rvb <- number of data bytes in object, + 3
	bic	rvb, rvb, #0x03		@ rvb <- num bytes in obj, aligned = end ofst in src (raw)
_func_	
packrc:	@ [internal entry] for rat/cpx
	car	sv1, dts		@ sv1 <- address of target sized object
	snoc	sv2, sv3, sv3		@ sv2 <- (src_adrs . trgt_ofst), sv3 <- rest of reloc list
	snoc	sv4, rvc, sv2		@ sv4 <- src_address,	rvc <- trgt_offset, as scheme_int
	add	sv2, rvc, rvb, LSL #2	@ sv2 <- end offset in target (scheme int)
packn1:	subs	rvb, rvb, #4		@ rvb <- remaining number of words to copy, are we done?
	sub	sv2, sv2, #16		@ sv2 <- updated target offset
	ldr	rva, [sv4, rvb]		@ rva <- word from source
	wrdst	sv1, sv2, rva		@ store it in target
	bpl	packn1			@ 	if not, (not done) jump to keep copying
	b	pack_9			@ return

_func_	
packsz:	@ copy gceable sized item, update sv3, don't change sv5
	@ in:	sv3 <- rest of reloc list, starting with current item
	@ in:	rvc <- object tag of item
	@ in:	sv5 <- relocation list
	@ out:	sv3 <- rest of reloc list, starting after current item
	@ modifies:	sv1-sv4, rva, rvb
	lsr	rvb, rvc, #6		@ rvb <- number of items in object (scheme int)
	bic	rvb, rvb, #0x03		@ rvb <- number of data bytes in object (raw int)
packs1:	sub	rvb, rvb, #4		@ rvb <- next source offset
	caar	sv2, sv3		@ sv2 <- source object address
	ldr	sv4, [sv2, rvb]		@ sv4 <- item from sized object
	bl	pkckfr			@ sv2 <- target offset
	bl	packlp			@ sv1 <- address of target, sv4 <- object or scheme offset
	cmp	rvb, #0			@ done copying data?
	bpl	packs1			@ 	if not, jump to keep copying
	cdr	sv3, sv3		@ sv3 <- rest of reloc list
	b	pack_9			@ return

.else	@ exclude_pack

pack:	@ (pack object)	 -- not implemented (when exclude_pack flag is set)
	@ in:	sv1 <- (object)
	@ sv3 <- target offset,  sv4 <- source list,  sv5 <- relocation list
	b	adr_flsfxt
	
.endif

/*------------------------------------------------------------------------------
@ I.D.4. library
@-----------------------------------------------------------------------------*/

.ifndef exclude_lib_mod

	/* (library (lib name) expressions) */
	PRIMIT	"library", sntx, 1
	@ in:	sv1 <- (lib name)
	@ in:	sv2 <- (expressions), eg. ((export ...) (import ...) . body)
	@ in:	glv, 14 <- library environment, set by parser
	@ out:	sv1 <- result
	vcrfi	sv4, glv, 12		@ sv4 <- list of libraries on MCU
	save	sv4, cnt		@ save libs (unpack-to-lib mods glv,12)
	vcsti	glv, 14, sv1
	@ set library index into library's private sub-env
	cdr	sv3, sv1
	vcrfi	sv4, sv3, 0		@ sv4 <- library's private sub-env
	veclen	sv5, sv3		@ sv5 <- library env's size (= lib index + 1)
	sub	sv5, sv5, #4		@ sv3 <- library index
	vcsti	sv4, 0, sv5
	@ evaluate expressions in library, within lib-env
plibr0:	nullp	sv2			@ done with expressions?
	beq	plibr1			@	if so,  jump to continue
	snoc	sv1, sv2, sv2		@ sv1 <- 1st lib-expr, sv2 <- remaining lib-expr
	vcrfi	sv3, glv, 14		@ sv3 <- lib (oba . env)
	cdr	env, sv3		@ env <- lib env
	save	sv2			@ dts <- (remaining-lib-expr lib-env library cnt ...)
	call	eval			@ sv1 <- rslt, frm eval of sv1 in dflt env
	restor	sv2			@ sv2 <- remaining-lib-expr, dts <- (lib-env lib cnt ...)
	b	plibr0			@ jump to continue evaluating lib-expressions
plibr1:	@
	vcrfi	sv1, glv, 14		@ sv1 <- (oba . env) of new lib
	set	rva, null		@ rva <- '()
	vcsti	glv, 14, rva		@ clear lib oba (set by parser) from glv
	restor	sv2			@ sv2 <- list of libraries on MCU
	cons	sv1, sv1, sv2		@ sv1 <- (lib . list-of-libs) = new libs
	list	sv1, sv1		@ sv1 <- (new list of libs), for pack
	call	adr_pack		@ sv1 <- new list of libraries, packed
.ifdef	LIB_TOP_PAGE
	set	sv2, i1			@ sv2 <-  1 (scheme int)
	ngint	sv2, sv2		@ sv2 <- -1 (scm int) = unpack to flash
.else
	set	sv2, i0			@ sv2 <-  0 (scm int) = unpack aboveheap
.endif
	call	adr_unpack		@ sv1 <- new list of MCU libs, unpacked
	vcsti	glv, 12, sv1		@ set new list of libs in global vector
	restor	cnt			@ cnt <- cnt,  dts <- (...)
	b	adr_npofxt		@ return with npo

	/* (export expr) */
	PRIMIT	"export", sntx, 1
	@ in:	sv1 <- expr
	@ in:	glv, 15 <- possible parse-mode flag
	@ out:	sv1 <- result
	set	pc,  cnt		@ return

	/* (import (lib1) (lib2) ...) */
	PRIMIT	"import", sntx, 0
	@ in:	sv1 <- ((lib1) (lib2) ...)
	@ in:	glv, 14 <- null (normal) or library environment set by parser
	@ in:	glv, 15 <- possible parse-mode flag
	@ out:	sv1 <- result
	vcrfi	sv5, glv, 14		@ sv5 <- mode indicator / library env (from parser)
	nullp	sv5			@ are we in normal (vs parse or (library ...)) mode?
	it	ne
	setne	pc,  cnt		@	if not, return
	@ reset built-in env if no libraries are specified
	nullp	sv1			@ no libraries specified (i.e. reset built-in env)?
	itTTT	eq
	ldreq	rvb, =oba_scheme	@	if so,  rvb <- initial built-in obarray
	vcstieq	glv, 17, rvb		@	if so,  set initial built-in obarray in glv
	ldreq	rvb, =env_scheme	@	if so,  rvb <- initial built-in scmenv
	vcstieq	glv, 13, rvb		@	if so,  set initial built-in environment in glv
	beq	adr_npofxt		@	if so,  return
	@ build new built-in oba
	vecset	glv, 15, true		@ set parse-mode flag (also means cumul import for zrslb)
	bl	zrslb			@ sv3 <- extended copy of built-in obarray
	sav_rc	sv3			@ dts <- (built-in-oba cnt ...)
	@ build new built-in env
	vecset	glv, 15, null		@ clear parse-mode flag (also means cumul import for zrslb)
	bl	zrslb			@ sv3 <- extended copy of built-in environment
	save	sv3			@ dts <- (built-in-env built-in-oba cnt ...)
	@ import the libraries
	set	sv2, sv1		@ sv2 <- ((lib1) (lib2) ...)
pimpo0:	@
	snoc	sv1, sv2, sv2		@ sv1 <- (lib1),  sv2 <- ((lib2) ...)
	save	sv2			@ dts <- (((lib2) ...) built-in-env cnt ...)
	car	sv1, sv1		@ sv1 <- library name's symbol ID
	call	adr_sym2str		@ sv1 <- library name as string
	bl	lbimp			@ rvb <- , sv5 <- lib-export-env, sv4 <- lib-export-oba
	restor	sv2			@ sv2 <- ((lib2) ...),  dts <- (built-in-env cnt ...)
	eq	rvb, #1			@ library found?
	itTTT	ne
	snocne	sv1, sv3, dts
	carne	sv3, sv3
	strne	sv5, [sv1, rvb]		@	if so,  store lib's export sub-env in built-in env
	strne	sv4, [sv3, rvb]		@	if so,  store lib's export sub-env in built-in env
	nullp	sv2			@ more libraries to import?
	bne	pimpo0			@	if so,  jump to import next library
	@ finish up
	restor	sv1, sv2, cnt
	vcsti	glv, 13, sv1
	vcsti	glv, 17, sv2
	b	adr_npofxt

_func_
zrslb:	@ construct a built-in env large enough to import libraries into it
	@ in:	glv, 15 <- mode flag: null => additive (import), #13/i3 => scmenv only (parslb)
	@ out:	sv3 <- new built-in env
	@ modifies:	sv2, sv3, sv4, rva, rvb, rvc
	@ returns via:	lnk
	bic	sv2, lnk, #lnkbit0	@ sv2 <- lnk, saved (and made even if Thumb2)
	ldr	sv4, =env_scheme	@ sv4 <- initial built-in env
	vcrfi	sv3, glv, 12		@ sv3 <- libraries
	@ find size needed for extended env including libs
	veclen	rvb, sv4		@ rvb <- size of initial built-in env
zrslb0:	nullp	sv3			@ done scanning libs?
	itT	ne
	cdrne	sv3, sv3		@	if not, sv3 <- rest of libraries
	addne	rvb, rvb, #4		@	if not, rvb <- updated size of built-in env
	bne	zrslb0			@	if not, jump to continue sizing new built-in env
	add	sv3, rvb, #4		@ sv3 <- 
	@ construct built-in env large enough to include libs (copy scmenv into it)
	bic	rvb, sv3, #0x03		@ rvb <- number of items in new env
	bl	adr__alo		@ rva <- address of allocated memory
	lsl	rvc, sv3, #6		@ rvc <- size of new env, shifted
	orr	rvc, rvc, #vector_tag	@ rvc <- tag for new env
	str	rvc, [rva, #-4]		@ store tag in allocated area
	@ initialize new built-in env to empty vectors
	bic	rvc, sv3, #0x03
	ldr	sv4, =empty_vector	@ sv4 <- empty vector
zrslb1:	subs	rvc, rvc, #4		@ rvc <- offset to sub-env position, are we done?
	itT	pl
	strpl	sv4, [rva, rvc]		@	if not, store empty-vector in new env
	bpl	zrslb1			@	if not, jump to keep initializing
	@ complete the allocation
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv3, rva, rvb		@ sv3 <- adrs of obj (sym), [*commit vector destination*]
	orr	fre, rva, #0x02		@ fre <- updtd de-rsrvd, [*restart critical instruction*]
	orr	lnk, sv2, #lnkbit0	@ lnk <- lnk, restored
	@ copy existing built-in env into new built-in env
	vcrfi	sv4, glv, 15		@ sv4 <- () =additive, #13/i3 =scmenv only
	nullp	sv4			@ copy whole built-in env?
	it	eq
	vcrfieq	sv4, glv, 13		@	if so,  sv4 <- built-in env
	eq	sv4, #t			@ copy whole built-in obarray?
	it	eq
	vcrfieq	sv4, glv, 17		@	if so,  sv4 <- built-in oba
	eq	sv4, #i3
	it	eq
	ldreq	sv4, =oba_scheme	@	if so,  sv4 <- initial obarray
	eq	sv4, #i5
	it	eq
	ldreq	sv4, =env_scheme	@	if so,  sv4 <- initial scmenv
	strlen	rvc, sv4		@ rvc <- size of built-in env/oba to copy
	bic	rvc, rvc, #0x03		@ rvc <- ofst of lst itm to cpy, is 0?
zrslb2:	subs	rvc, rvc, #4		@ rvc <- ofst of nxt item to cpy, is 0?
	ldr	sv2, [sv4, rvc]		@ sv2 <- item from source built-in env 
	str	sv2, [sv3, rvc]		@ store item in new built-in env
	bne	zrslb2			@	if not, jump to keep copying
	set	pc,  lnk		@ return w/extended built-in-env in sv3
	
_func_
lbimp:	@ find library for import
	@ in:	sv1 <- library name (symbol)
	@ out:	rvb <- lib's exprt ofst in main env (raw int), 1 if none
	@ out:	sv5 <- lib's export sub-environment
	@ modifies:	sv2-sv5, rva-rvc
	bic	sv3, lnk, #lnkbit0
	save	sv1, sv3
	vcrfi	sv5, glv, 12		@ sv5 <- libraries
lbimp0:	nullp	sv5			@ reached end of libraries?
	it	eq
	seteq	rvb, 1			@	if so,  rvb <- 1, lib not found
	beq	lbimpx			@	if so,  jump to exit
	car	sv4, sv5		@ sv4 <- first lib
	car	sv2, sv4
	vcrfi	sv1, sv2, 0		@ sv1 <- first lib's priv sub-oba (sym)
	set	sv2, i1
	ldrb	rvb, [sv1, #0]
	sub	rvb, rvb, #47
	raw2int	sv3, rvb
	bl	subcpy
	car	sv2, dts
	bl	stsyeq			@ is this the lib we're looking for?
	it	ne
	cdrne	sv5, sv5		@	if not, sv5 <- rest of libs
	bne	lbimp0			@	if not, jump to scan restof libs
	@ get lib's index in built-in env and lib's export env
	car	sv4, sv5
	snoc	sv3, sv4, sv4		@ sv3 <- lib-oba, sv4 <- lib-env
	vcrfi	sv2, sv4, 0
	vcrfi	sv2, sv2, 0		@ sv2 <- import-lib-idx
	bic	rvb, sv2, #0x03
	ldr	sv5, [sv4, rvb]		@ sv5 <- import-lib's export sub-env
	ldr	sv4, [sv3, rvb]		@ sv4 <- import-lib's export sub-oba
lbimpx:	@ exit
	restor	sv1, sv3
	orr	lnk, sv3, #lnkbit0
	set	pc,  lnk

.else	@ do exclude_lib_mod


.endif	@ do or dont exclude_lib_mod


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



