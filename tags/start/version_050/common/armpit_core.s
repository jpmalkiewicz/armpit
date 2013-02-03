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
corenv:	@	core sub-environment		|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_corenv - corenv - 4) >> 2
	
winders_env:	.word	wndrs,	scheme_true	@ _winders
catch_env:	.word	scatch,	catch		@ _catch
program_env:	.word	sprgrm,	scheme_true	@ _prg
@ Addendum: ISR vector, Read Buffer, uart input-port vect (read-only), uart output-port vect (read-only)
		.word	pGLV,	_GLV		@ _GLV
@ Addendum: procedures for linking external code / installing non-moving objects into RAM
		.word	s_lkp,	_vrlkp		@ _lkp
		.word	s_mkc,	_mkcpl		@ _mkc
		.word	s_apl,	_apply		@ _apl
		.word	s_dfv,	_dfnsb		@ _dfv
		.word	s_alo,	_maloc		@ _alo
		.word	s_cons,	_cons		@ _cns
		.word	s_save,	_save		@ _sav
		.word	sisrxt,	pisrxt		@ _isx   isr exit
		.word	sgnism,	pgnism		@ _ism   isr memory allocation (check reservation ...)
@ Addendum: error, gc
		.word	sthrow,	throw		@ throw
		.word	sgc,	pgc		@ gc
		.word	s_gc,	_gc		@ _gc
		.word	s_err,	_err		@ _err
		.word	sversi,	versn_		@ version
@ Addendum: library, pack
		.word	sadrof,	padrof		@ address-of
		.word	spkdts,	pkdtst		@ packed-data-set!
		.word	sunpak,	punpak		@ unpack
		.word	spack,	ppack		@ pack
	
library_env:	.word	slibra,	plibra		@ library
export_env:	.word	sexpor,	pexpor		@ export
import_env:	.word	simpor,	pimpor		@ import
	
.ifndef	live_SD
		.word	sfcln,	pfcln		@ fsc (file space cleaning)
.endif

@ Addendum: balancing the top-level b-tree

.ifdef top_level_btree
		.word	sbalan,	balanc		@ _bal
.endif

end_of_corenv:	@ end of corenv
	
	@-------.-------.-------.-------.-------+
@-------@	core Constants			|
	@-------.-------.-------.-------.-------+

.macro	make_var_from_corenv var_name, var_env
	\var_name = ((\var_env - corenv + 4) << 13) | ((core_env - scmenv) << 6) | variable_tag
.endm

	make_var_from_corenv	winders_var,	winders_env
	make_var_from_corenv	catch_var,	catch_env
	make_var_from_corenv	program_var,	program_env
	make_var_from_corenv	library_var,	library_env
	make_var_from_corenv	export_var,	export_env
	make_var_from_corenv	import_var,	import_env
	
	@-------.-------.-------.-------.-------+
@-------@	Core Function Names		|
	@-------.-------.-------.-------.-------+
	
.balign	4

sprgrm:	SYMSIZE	4
	.ascii	"_prg"
	.balign 4

wndrs:	SYMSIZE	8
	.ascii	"_winders"
	.balign 4

	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg


@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	armpit scheme core:					
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
@
@	Requires:
@			core:			
@							
@
@	Modified by (switches):			LIB_TOP_PAGE, SHARED_LIB_FILE, cortex,
@						top_level_btree, inline_cons
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=
	
@---------------------------------------------------------------------------------------------------------
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
@---------------------------------------------------------------------------------------------------------

	
@---------------------------------------------------------------------------------------------------------
@ I.D.4. memory comparison:			stsyeq
@---------------------------------------------------------------------------------------------------------

_func_
stsyeq:	@ compare strings/symbols/bytevectors in sv1 and sv2, return with eq or ne flag raised
	@ type is not compared (only size and bytes are compared)
	@ on entry:	sv1 <- string or symbol or bytevector
	@ on entry:	sv2 <- string or symbol or bytevector
	@ modifies:	sv3, rva, rvb
	@ returns via:	lnk
	strlen	rva, sv1		@ rva <- size of string/symbol/bv in sv1
	strlen	rvb, sv2		@ rvb <- size of string/symbol/bv in sv2
	eq	rva, rvb		@ do strings/symbols/bvs have the same size?
	it	ne
	setne	pc,  lnk		@	if not, return with ne flag raised
	add	sv3, rva, #16		@ sv3 <- offset of byte after last (scheme int)
stsye0:	sub	sv3, sv3, #4		@ sv3 <- offset of next byte to compare (scheme int)
	eq	sv3, #13		@ are we done comparing?
	it	eq
	seteq	pc,  lnk		@	if so,  return with eq flag raised
	bytref	rvb, sv2, sv3		@ rvb <- byte from string/symbol/bv in sv2
	bytref	rva, sv1, sv3		@ rva <- byte from string/symbol/bv in sv1
	eq	rva, rvb		@ are bytes the same?
	beq	stsye0			@	if so,  jump to compare next byte
	set	pc,  lnk		@ return with ne flag raised

@---------------------------------------------------------------------------------------------------------
@ I.D.5. memory copying:			subcpy
@---------------------------------------------------------------------------------------------------------

_func_
subcpy:	@ copy a sized object in sv1, from offset sv2 to sv3
	@ return copy in sv1 (non-gceable, string tagged)
	@ on entry:	sv1 <- source
	@ on entry:	sv2 <- start offset
	@ on entry:	sv3 <- end offset
	@ on exit:	sv1 <- copy of source from start to end offset, as scheme string
	@ modifies:	sv1, sv2, sv4, rva, rvb, rvc
	@ returns via:	lnk
	set	sv4, sv1		@ sv4 <- source, saved
	@ calculate number of bytes needed
	sub	rvb, sv3, sv2		@ rvb <- number of chars to copy * 4
	lsr	rvb, rvb, #2		@ rvb <- number of chars to copy
	add	rvb, rvb, #4		@ rvb <- size of target object, with header (raw int)	
	@ allocate memory
	bic	sv1, lnk, #lnkbit0	@ sv1 <- lnk, saved (and made even if Thumb2)
	bl	zmaloc			@ rva <- target object
	str	sv1, [rva, #4]		@ store lnk in target object, temporarily
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- address of target (symbol), [*commit target destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer,        [*restart critical instruction*]
	ldr	rva, [sv1, #4]		@ rva <- lnk, almost restored
	orr	lnk, rva, #lnkbit0	@ lnk <- lnk, restored
	@ update object header to string of proper size
	set	rva, #string_tag	@ rva <- full string tag
	sub	rvb, sv3, sv2		@ rvb <- number of chars in target * 4
	orr	rva, rva, rvb, LSL #6	@ rva <- full string tag with size of data
	str	rva, [sv1]		@ set word #0 of target object to string tag
	@ copy bytes from source to target
	set	rvb, #4			@ rvb <- 4, offset in target to char after header
subcp0:	cmp	sv2, sv3		@ are we done copying?
	it	pl
	setpl	pc,  lnk		@	if so, return
	bytref	rva, sv4, sv2		@ rva <- raw byte from source
	strb	rva, [sv1, rvb]		@ store it in target
	add	sv2, sv2, #4		@ sv2 <- updated source end offset
	add	rvb, rvb, #1		@ rvb <- updated target end address
	b	subcp0			@ jump to continue copying bytes

	
@---------------------------------------------------------------------------------------------------------
@
@  I.C.   HARDWARE-INDEPENDENT INTERRUPT ROUTINES (ISRs)
@
@  I.C.1. Generic / Timer 0/1  ISR
@  I.C.2. UART 0/1 ISR
@  I.C.3. TIMER 0/1 clearing partial ISR (branched to from genisr)
@  I.C.4. Exit from isr via break
@
@---------------------------------------------------------------------------------------------------------

@---------------------------------------------------------------------------------------------------------
@  I.C.1. Generic ISR (branches to uart isr, i2c isr or processes timer interrupts)
@---------------------------------------------------------------------------------------------------------

_func_
genisr:	@ generic isr routine, branches to specific routines or scheme callback
	enterisr			@ rvb <- interrupt status
					@ put fre,cnt,rva,rvb,rvc,lnk_usr,pc_usr,spsr on stack	
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, ISR_V_offset
	add	rvc, rvb, #1
	ldr	rvc, [rva, rvc, lsl #2]
	execp	rvc
	bne	gnisxt
	adr	lnk, genis0
	add	rvc, rvc, #4
	set	pc,  rvc
genis0:	@ [return from machine code ISR and entry from Systick handler on Cortex-M3]
	raw2int	rvc, rvb
	str	rvc, [sp,  #-12]
	@ see if memory transaction or gc was interrupted
	ldmia	sp!, {fre}
	tst	fre, #0x02		@ was memory reserved?
	it	eq
	bleq	genism			@	if so,  jump to deal with that (restart or go-through)
	@ exit if there is no scheme isr
	vcrfi	rva, glv, 0		@ rva <- possible scheme callback
	execp	rva			@ is it run-able?
	it	ne
	stmdbne	sp!, {fre}
	bne	genier
	@ build vector for gc-eable part of context
.ifdef	hardware_FPU
  .ifndef FPU_is_maverick
	set	rvb, #104		@ rvb <- number of bytes to allocate (items + headers)
  .else
	set	rvb, #112		@ rvb <- number of bytes to allocate (items + headers)
  .endif
.else
	set	rvb, #88		@ rvb <- number of bytes to allocate (8 items + header)
.endif
	bl	zmaloc			@ rva <- address of allocated memory
	add	fre, rva, rvb		@ fre <- address of next free cell (level 2 reserved)
	orr	fre, fre, #0x02		@ fre <- de-reserved
	set	cnt, rva		@ cnt <- address of non gc-context objects
.ifdef	hardware_FPU
  .ifndef FPU_is_maverick
	add	rvb, cnt, #48		@ rvb <- address of gc-context objects
  .else
	add	rvb, cnt, #56		@ rvb <- address of gc-context objects
  .endif
.else
	add	rvb, cnt, #32		@ rvb <- address of gc-context objects
.endif
	set	rva, #vector_tag
	orr	rva, rva, #0x0700
	stmia	rvb, {rva, sv1-sv5, env, dts}	@ save sv1-sv5, env, cnt, dts in gc-context vector
	@ build vector for non-gc-eable part of context
	set	rva, #bytevector_tag
.ifdef	hardware_FPU
  .ifndef FPU_is_maverick
	orr	rva, rva, #0x2c00	@ rva <- 44 = 11 items*4 bytes/item, num. bytes in non-gc vect
	fmrx	sv1, fpscr
	stmia	cnt!, {rva, sv1}
	vstmia	cnt!, {s0, s1}
  .else
	orr	rva, rva, #0x3400	@ rva <- 52 = 13 items*4 bytes/item, num. bytes in non-gc vect
	cfmv32sc mvdx4, dspsc		@ mvdx4 <- contents of DSPSC
	cfmvr64l sv1, mvdx4		@ sv1   <- id
	stmia	cnt!, {rva, sv1}
	cfmvrdl	rva, mvd0		@ rva <- lo 32 bits of maverick register 0
	cfmvrdh	sv1, mvd0		@ sv1 <- hi 32 bits of maverick register 0
	stmia	cnt!, {rva, sv1}
	cfmvrdl	rva, mvd1		@ rva <- lo 32 bits of maverick register 1
	cfmvrdh	sv1, mvd1		@ sv1 <- hi 32 bits of maverick register 1
	stmia	cnt!, {rva, sv1}
  .endif
.else
	orr	rva, rva, #0x1c00	@ rva <- 28 = 7 items*4 bytes/item, num. bytes in non-gc vect
.endif
	ldmia	sp!, {sv1-sv5, env, dts} @ get cnt, rva, rvb, rvc, lnk_usr, lnk_irq and psr_usr from stack
	stmia	cnt, {rva, sv1-sv5, env, dts}	@ store them back in non gc-context vector
	@ load isr
	vcrfi	sv1, glv, 0		@ sv1 <- scheme callback
	ldr	sv3, [sp,  #-44]	@ sv3 <- interrupt number restored from under stack (scheme int)
	clearVicInt			@ clear interrupt in interrupt vector
	@ prepare scheme stacks and environment
	@ cnt <- non-gc-context vector,  rvb <- gc-context vector
	@ sv1 <- handler
	add	dts, cnt, #64		@ dts <- address of upcoming data-stack
	add	rva, dts, #8		@ rva <- address of rest of upcoming data-stack
	ldr	sv2, =stkbtm		@ sv2 <- stack-bottom
	set	sv4, #null		@ sv4 <- '()
.ifdef	hardware_FPU
  .ifndef FPU_is_maverick
	sub	cnt, cnt, #16
  .else
	sub	cnt, cnt, #24
  .endif
.endif
	stmia	dts, {cnt,rva,rvb,sv2,sv3,sv4}	@ dts <- (non-gc-context gc-context stack-bottom)
	cadr	env, sv1		@ env <- env from handler
	add	sv2, dts, #16		@ sv2 <- (interrupt-number) = arg-list
	ldr	sv4, =reset0		@ lnk <- reset0, in case lnk is used or saved on stack
	orr	sv4, sv4, #lnkbit0
	ldr	sv5, =apply
	adr	cnt, rsrctx
	set	rvc, #normal_run_mode	@ rvc <- normal run mode
	stmdb	sp!, {fre,cnt,sv1-sv5,rvc}	@ put values on irq_stack
	set	sv4, #null		@ sv4 <- '(), for cortex, in case sv4 now ends in b11
	b	rsrxit			@ jump to common isr exit

_func_
rsrctx:	@ restore context (non-gc-context gc-context ...)
	swi	isr_no_irq		@ switch to IRQ mode without interrupts
	restor	cnt			@ cnt <- non-gc-context,	dts <- (gc-context ...)
	car	rvb, dts		@ rvb <- gc-context
	@ restore a saved context and resume
	@ rva <- non-gc-context vector,  cnt <- gc-context vector
.ifdef	hardware_FPU
  .ifndef FPU_is_maverick
	ldmia	cnt!, {rva, sv1}
	vldmia	cnt!, {s0, s1}
	fmxr	fpscr, sv1
  .else
	ldmia	cnt!, {rva, sv1}
	cfmv64lr mvdx4, sv1		@ mvfx4 <- saved contents of DSPSC
	cfmvsc32 dspsc, mvdx4		@ restore DSPSC in maverick co-processor
	ldmia	cnt!, {rva, sv1}
	cfmvdlr	mvd0, rva		@ restore lo 32 bits of maverick reg 0
	cfmvdhr	mvd0, sv1		@ restore hi 32 bits of maverick reg 0
	ldmia	cnt!, {rva, sv1}
	cfmvdlr	mvd1, rva		@ restore lo 32 bits of maverick reg 1
	cfmvdhr	mvd1, sv1		@ restore hi 32 bits of maverick reg 1
  .endif
.endif
	ldmia	cnt, {rva, sv1-sv5, env, dts}
.ifndef cortex
	add	sp,  sp,  #4
.endif
	stmdb	sp!, {fre, sv1-sv5, env, dts}
	ldmia	rvb, {rva, sv1-sv5, env, dts}	@ restore sv1-sv5, env, cnt, dts from context vector
rsrxit:	@ [internal entry] common isr exit
	isrexit

genier:	@ return from ISR with error
	vcrfi	env, glv, 7		@ env from glv (***NEW***)
	ldr	sv1, [sp, #-12]
	adr	sv4, pISR
	ldr	rvc, =error4
	str	rvc, [sp,  #20]
	str	rvc, [sp,  #24]
	set	rvc, #normal_run_mode
	str	rvc, [sp,  #28]
	b	gnisxt			@ jump to common isr exit

.balign 4

pISR:	SYMSIZE	4
	.ascii	"_ISR"
	.balign 4

.balign	4

sisrxt:	SYMSIZE	4
	.ascii	"_isx"
	.balign 4
		
pisrxt:	@ (_isx)
	SYNTAX	0			@ primitive syntax, no input args
_func_
gnisxt:	@ simple exit from interrupt	
	clearVicInt			@ clear interrupt in interrupt vector (if needed)
	exitisr				@ exit from isr

.balign	4

sgnism:	SYMSIZE	4
	.ascii	"_ism"
	.balign 4
		
pgnism:	@ (_ism)
	SYNTAX	0			@ primitive syntax, no input args
_func_
genism: @ prepare to allocate memory during interrupt when memory was reserved by user process
	@ stack is currently:	[cnt,rva,rvb,rvc,lnk_usr,lnk_irq,psr_usr]
	tst	fre, #0x01		@ was memory reserved at level 1?
	beq	gniml2			@	if not, jump to process level 2 memory reservation
	@ process level 1 memory reservation (non-gc critial interruption)
	ldr	rvb, [sp,  #20]		@ rva <- lnk_irq (pc_usr, address of post-interrupt instruction)
	bic	rvb, rvb, #0x01		@ rva <- lnk_irq (pc_usr) with cleared Thumb bit to load properly
	ldr	rvb, [rvb]		@ rva <- interrupted instruction
	adr	rvc, icrit1		@ rvb <- address of critical instruction
	ldr	rvc, [rvc]		@ rvb <- critical instruction
	eq	rvb, rvc		@ was the critical instruction interrupted?
	bne	grstrt			@	if not, jump to set user process up for maloc restart
	@ process level 1 critical instruction interruption
	ldr	rva, [sp,  #4]		@ get saved rva from stack (in case, cortex, ISRs are tail-chained)
icrit1:	orr	fre, rva, #0x02		@ memory critical instruction for level 1 (update and free mem ptr)
	ldr	rvb, [sp,  #20]		@ rva <- lnk_irq (pc_usr)
	@ this, below, is ok since crit is 32-bit instruction, even on cortex
	add	rvb, rvb, #4		@ rva <- next instruction for user process to execute (crit done)
	str	rvb, [sp,  #20]		@ set next instruction for interrupted process
	set	pc,  lnk		@ return
grstrt:	@ set user process up for 'bl cons/save/zmaloc' restart when resumed
	ldr	rvb, [sp,  #16]		@ rva <- lnk_usr (return address (lr) of interrupted process)
	@ this, below, works since bl is 32-bit instruction even on cortex
	bic	rvb, rvb, #0x01
	sub	rvb, rvb, #4		@ rva <- addr (restart) of call to cons/save/zmaloc in intrptd proc
	str	rvb, [sp,  #20]		@ set lnk_irq (pc_usr) to be the restart address
.ifdef	cortex
	@ reset the xPSR/EPSR to prevent restarting invalidated ldm/stm instructions
	@ alternatively, clear just bits 26:25 and 15:10
	set	rvb, #0x01000000	@ r1 <- default xPSR to be restored with bit 24 (Thumb mode) set
	str	rvb, [sp,  #24]
.endif
	eor	fre, fre, #0x03		@ fre <- free-pointer, de-reserved
	set	pc,  lnk		@ return
gniml2:	@ process level 2 memory reservation (interrupted gc)
	stmdb	sp!, {fre}		@ store fre pointer back on stack (to complete stack for return)
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
	@ disable scheme interrupts
	ldr	rva, =BUFFER_START
	vcrfi	fre, rva, CTX_EI_offset	@ fre <- enabled scheme interrupts  0-31 (raw)	
	ldr	cnt, =int_en_base
	str	fre, [cnt, #int_disab1]
	vcrfi	fre, rva, CTX_EI_offset+4 @ fre <- enabled scheme interrupts 32-63 (raw)
	str	fre, [cnt, #int_disab2]
  .if num_interrupts > 64
	vcrfi	fre, rva, CTX_EI_offset+8 @ fre <- enabled scheme interrupts 64-95 (raw)
	str	fre, [cnt, #int_disab3]
  .endif
	@ jump to resume gc
	ldr	pc,  =0xfffffffd	@ return to thread mode, use process stack, finish-up gc
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
	sub	sp,  sp,  #32		@ set stack pointer back to get saved items
	@ enable scheme interrupts
	ldr	rva, =BUFFER_START
	vcrfi	fre, rva, CTX_EI_offset	@ fre <- enabled scheme interrupts  0-31 (raw)	
	ldr	cnt, =int_en_base
	str	fre, [cnt, #int_enabl1]
	vcrfi	fre, rva, CTX_EI_offset+4 @ fre <- enabled scheme interrupts 32-63 (raw)	
	str	fre, [cnt, #int_enabl2]
  .if num_interrupts > 64
	vcrfi	fre, rva, CTX_EI_offset+8 @ fre <- enabled scheme interrupts 64-95 (raw)
	str	fre, [cnt, #int_enabl3]
  .endif
.endif
	ldr	lnk, [sp,  #-8]		@ lnk <- lnk_irq restored
	ldr	cnt, [sp,  #-4]		@ cnt  <- lnk_usr
	str	cnt, [sp,  #20]		@ store it in stack
	bic	cnt, cnt, #0x01
	sub	cnt, cnt, #4		@ cnt  <- addr (restart) of call to cns/sav/zmaloc in intrptd proc
	str	cnt, [sp,  #24]		@ store it in stack
	ldmia	sp!, {fre}
	set	pc,  lnk


@---------------------------------------------------------------------------------------------------------
@  I.C.3. TIMER 0/1 clearing partial ISR (branched to from genisr)
@---------------------------------------------------------------------------------------------------------

.balign	4
	
ptmisr:	@ TIMER clearing partial ISR
	PFUNC	0
	@
	@ do not modify rvb, lnk, sp in such partial ISR
	@
	eq	rvb, #timer0_int_num	@ is interrupt for TIMER0?
	itE	eq			@	if-then (Thumb-2)
	ldreq	rva, =timer0_base	@	if so,  rva <- base address of TIMER0
	ldrne	rva, =timer1_base	@	if not, rva <- base address of TIMER1
	clearTimerInt			@ clear interrupt in timer peripheral block
	set	pc,  lnk		@ return to genisr
	
	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@---------------------------------------------------------------------------------------------------------
@ I.D.1. garbage collection - Stop and Copy:	gc, gc_bgn
@---------------------------------------------------------------------------------------------------------

.balign	4
	
sgc:	SYMSIZE	2
	.ascii	"gc"
	.balign 4

pgc:	@ (gc)
	PFUNC	0
	bl	gc			@ rva <- amount of free mem bytes (raw int), perform garb collect
	raw2int	sv1, rva		@ sv1 <- amount of free mem bytes (scheme int)
	set	pc,  cnt

.balign	4

_func_
gc:	@ perform garbage collection
	@ on exit:	rva <- number of bytes of free memory
	@ on exit:	rvb <- 0 = number of bytes allocated
	set	rvb, #0			@ rvb <- 0 = number of bytes to allocate
	add	lnk, lnk, #4		@ lnk <- retrn addr adj for gc_bgn exit (bl gc resumd, not restrtd)
	b	gc_bgn
	
.balign	4

s_gc:	SYMSIZE	3
	.ascii	"_gc"
	.balign 4

_gc:	@ (_gc)
	SYNTAX	0

.ifndef mark_and_sweep
	
_func_
gc_bgn:	@ [internal entry] for cons/save/zmaloc and genism
	@ on entry:	rvb <- how many bytes were to be allocated when gc was triggered
	@ on entry:	lnk <- caller return address (gc returns to lnk - 4 = restart, except for 'bl gc')
	@ on exit:	rva <- number of bytes of free memory
	@ on exit:	rvb <- (unmodified)
	@ reserve memory
	bic	fre, fre, #0x03		@ reserve memory, level 2
	@ if fre > heaptop0 do gc down otherwise do gc up
	vcrfi	rva, glv, 9		@ rva <- heaptop0 -- from global vector (*** NEW ***)
	bic	rva, rva, #i0
	cmp	rva, fre
	itE	mi
	ldrmi	fre, =heapbottom
	setpl	fre, rva
	stmia	fre!, {cnt, sv1-sv5, env, dts, glv}
	@ prepare registers/constants for gc
	set	env,  #broken_heart
	stmia	fre!, {env}		@ 8-byte adjustment // maybe lnk could be stmia-ed instead
	ldr	sv1, =heapbottom
	vcrfi	sv2, glv, 10		@ sv2  <- heaptop1 -- from global vector (*** NEW ***)
	sub	sv2, sv2, #2
	sub	dts, fre, #4
	sub	glv, fre, #44
	sub	rvc, fre, #40
gcloop:	add	glv, glv, #4		@ update scan-gc
	cmp	glv, fre
	it	pl
	bpl	gcexit
	ldr	cnt, [glv]		@ cnt  <- (car scan-gc){old gc}
	pntrp	cnt			@ could item be a pointer?
	beq	gcmov			@	if so,  jump to move the object it points to
	and	sv4, cnt, #0x07
	eq	sv4, #0x03
	it	eq
	addeq	glv, glv, #4
	and	sv4, cnt, #0xCF		@ sv4  <- sized-object-tag indicator bits of item
	eq	sv4, #0x4F		@ is item a sized-object-tag?
	bne	gcloop			@	if not, jump to copy next item	
	cmp	dts, glv		@ is sized-object-tag within a gc-eable sized object?
	bpl	gcloop			@	if so,  skip it (only ptrs have to be treated)
	tst	cnt, #0x30
	itT	eq
	addeq	dts, glv, cnt, LSR #6	@	if so,  dts <- position of end of object + 1
	biceq	dts, dts, #3		@	if so,  dts <- position of end of object
	itTT	ne
	addne	glv, glv, cnt, LSR #8	@	if not, glv <- position of next object, unaligned
	addne	glv, glv, #3		@	if not, glv <- pos of next object, unaligned + 3 (to align)
	bicne	glv, glv, #3		@	if not, glv <- pos of next object, aligned (to skip curr.)
	b	gcloop
gcmov:	cmp	cnt, sv1		@ is cnt >= heapbottom ?
	it	pl
	cmppl	sv2, cnt		@		if so,  is heaptop1 >= cnt ?
	bmi	gcloop			@		if not, return (object is not in heap)
	snoc	sv3, sv4, cnt		@ sv3 <- car, sv4 <- cdr of object
	eq	sv3, env		@ is sv3 a broken-heart?
	it	eq
	streq	sv4, [glv]		@	if so,  store updated object address in new heap
	beq	gcloop			@	if so,  return (cnt contains new address)
	stmia	cnt!, {env}		@ store broken-heart in old heap
	stmia	cnt!, {fre}		@ store object's new heap address in old heap
	str	fre, [glv]		@ store object's new heap address in new heap
	stmia	fre!, {sv3, sv4}	@ store car-cdr in new heap
	and	sv5, sv3, #0xCF		@ check tag of (car cnt)
	eq	sv5, #0x4F		@ did cnt point to a sized item?
	bne	gcloop			@	if not, return
	lsrs	sv5, sv3, #8		@ is sized item an empty string, vector or bytevector?
	itT	eq
	streq	env, [fre, #-4]		@	if so,  store broken-heart in obj's extra wrd in new heap
	beq	gcloop			@	if so,  return
	tst	sv3, #0x30		@ did cnt point to a "gc-eable" sized item?
	itT	ne
	addne	sv5, sv5, #0x03		@	if not, sv5 <- tag with 3 bytes added to size
	lsrne	sv5, sv5, #2		@	if not, sv5 <- number of words to move (i.e. word aligned)
	tst	sv5, #0x01		@ is object 8-byte aligned?
	itTT	eq
	subeq	sv5, sv5, #1		@	if not, sv5 <- index of object's extra word
	streq	env, [cnt, sv5, lsl #2]	@	if not, store broken-heart in obj's extra wrd in old heap
	addeq	sv5, sv5, #2		@	if not, sv5 <- 8-byte aligned size
	add	sv5, sv5, #1
gclp01:	subs	sv5, sv5, #2		@ sv5 <- remaining number of words to move, is it 0?
	itT	ne
	ldmiane	cnt!, {sv3, sv4}	@	if not, cnt <- next two words of object
	stmiane	fre!, {sv3, sv4}	@	if not, store them in new heap
	bne	gclp01			@	if not, jump to keep copying object contents
	b	gcloop			@ jump to process next object
gcexit:	@ restore registers
	ldmia	rvc, {cnt, sv1-sv5, env, dts, glv}
	@ reset if memory is still exhausted after gc
	vcrfi	rva, glv, 9		@ rva <- heaptop0 -- from global vector (*** NEW ***)
	cmp	fre, rva		@ is code using the bottom heap?
	it	pl
	vcrfipl	rva, glv, 10		@	if not, rva <- heaptop1 -- from global vector (*** NEW ***)
	vcsti	glv, 1, rva		@ store current heap top in global vector (*** NEW ***)
	bic	rva, rva, #i0
	sub	rva, rva, fre		@ rva <- free heap space bytes (raw int)
	cmp	rva, rvb		@ are enough bytes free?
	it	mi
	ldrmi	pc,  =reset0		@	if not, jump to reset, memory is exhausted	
	@ exit
	orr	fre, fre, #0x02		@ fre <- ...bbb10			(back to de-reserved)
	bic	lnk, lnk, #lnkbit0
	sub	lnk, lnk, #4		@ return (restart allocation that triggered gc)
	set	pc,  lnk

	
.else	@ mark_and_sweep gc

_func_
gc_bgn:	@ [internal entry] for cons/save/zmaloc and genism
	@ on entry:	rvb <- how many bytes were to be allocated when gc was triggered
	@ on entry:	lnk <- return address of caller (gc retrns to lnk - 4 = restrt, excpt for 'bl gc')
	@ on exit:	rva <- number of bytes of free memory
	@ on exit:	rvb <- (unmodified)
	@ reserve memory
	ldr	fre, =heapbottom	@ reserve memory, level 2
	set	rva, cnt
	set	cnt, #vector_tag
	orr	cnt, cnt, #0x0900 
	stmia	fre, {cnt, rva, sv1-sv5, env, dts, glv}
	vcrfi	env, glv, 1		@ env <- heaptop -- from global vector, as pseudo-scheme-int
	vcrfi	sv2, glv, 10		@ sv2 <- grey area address, from global vect, as pseudo-scheme-int
	bic	sv2, sv2, #i0		@ sv2 <- address of grey area
	add	dts, sv2, #grey_size_bytes	@ dts <- address of black area
gcmrk1:	@ clear grey and black sets
	set	sv3, #grey_set_size	@ sv3 <- 256 = space for 64KB
	set	sv4, #0
gcm1_c:	subs	sv3, sv3, #1
	str	sv4, [sv2, sv3, lsl #2]
	str	sv4, [dts, sv3, lsl #2]
	bne	gcm1_c
	@ mark
	@ rvc <- heaptop
	@ sv2 <- grey  area address
	@ dts <- black area address
	set	sv1, fre		@ sv1 <- start of mark scan
	set	sv5, #null		@ sv5 <- top parent
gcm1_l:	@ mark loop
	pntrp	sv1
	beq	gcm1_p
gcm1up:	@ return to parent
	nullp	sv5			@ done scanning?
	beq	gcswp1			@	if so,  jump to sweep phase 1
	@ if parent is grey only, mark it black, go to 2nd child
	@ if parent is black only, go to its parent
	@ if parent is grey+black, keep marching through its children
	sub	sv3, sv5, fre
	lsr	sv3, sv3, #3
	and	rvc, sv3, #0x1f		@ rvc <- bit position for object's start address
	set	cnt, #1			@ cnt <- bit in position 0
	lsl	cnt, cnt, rvc		@ cnt <- positioned bit for object
	lsr	sv3, sv3, #5		@ sv3 <- offset of word in grey/black area
	ldr	sv4, [sv2, sv3, lsl #2]	@ sv4 <- word from grey set
	ldr	glv, [dts, sv3, lsl #2]	@ glv <- word from black set
	@ is parent white or grey-only?
	tst	glv, cnt
	bne	gcm1_b
	eor	sv4, sv4, cnt		@ sv4 <- word from grey set with parent mark bit flipped
	str	sv4, [sv2, sv3, lsl #2]	@ unmark parent from grey set (mark if re-entering via gcm1wh)
	eor	glv, glv, cnt		@ glv <- word from black set with parent mark bit flipped
	str	glv, [dts, sv3, lsl #2]	@ mark parent in black set
	ldr	sv3, [sv5, #4]		@ sv3 <- parent of parent
	str	sv1, [sv5, #4]		@ store previously visited child back in parent
	ldr	sv1, [sv5]		@ sv1 <- next child to visit
	str	sv3, [sv5]		@ store parent of parent in parent
	b	gcm1_l			@ jump laterally to mark next child
gcm1_b:	@ parent is black or grey-black
	ldr	sv3, [sv5]		@ sv3 <- parent of parent
	str	sv1, [sv5]		@ store previously visited child back in parent
	tst	sv4, cnt		@ is parent black-only?
	itT	eq
	seteq	sv1, sv5		@	if so,  sv1 <- child of parent of parent (i.e. parent)
	seteq	sv5, sv3		@	if so,  sv5 <- parent of parent, restored as parent
	beq	gcm1up			@	if so,  jump up to continue processing of parent
	sub	sv5, sv5, #8
	ldr	sv1, [sv5, #4]
	str	sv3, [sv5, #4]
	b	gcm1_l			@ jump laterally to mark next child
	
gcm1_p:	@ sv1 is pointer to child
	@ is child in heap?
	cmp	sv1, fre		@ is child address >= heapbottom ?
	it	pl
	cmppl	env, sv1		@	if so,  is child address < heaptop ?
	bmi	gcm1up			@	if not, jump up to continue processing of parent
	@ is child already marked?
	sub	sv3, sv1, fre
	lsr	sv3, sv3, #3
	and	rvc, sv3, #0x1f		@ rvc <- bit position for object's start address
	set	cnt, #1			@ cnt <- bit in position 0
	lsl	cnt, cnt, rvc		@ cnt <- positioned bit for object
	lsr	sv3, sv3, #5		@ sv3 <- offset of word in grey/black area
	ldr	sv4, [sv2, sv3, lsl #2]	@ sv4 <- word from grey set
	tst	sv4, cnt		@ is child in grey set?
	itT	eq
	ldreq	glv, [dts, sv3, lsl #2]	@	if not, glv <- word from black set
	tsteq	glv, cnt		@	if not, is child in black set?
	bne	gcm1up			@	if so,  jump up to continue processing of parent
	@ is child a rat/cpx?
	ldr	rva, [sv1]		@ rva <- possible tag of child
	and	rvc, rva, #0x07		@ rvc <- possible tag masked for rat/cpx identification
	eq	rvc, #0x03		@ is child a rat/cpx?
	itT	eq
	orreq	glv, glv, cnt		@	if so,  glv <- black set word updatd with child rat/cpx bit
	streq	glv, [dts, sv3, lsl #2]	@	if so,  store updated mark in black set
	beq	gcm1up			@	if so,  jump up to continue processing of parent
	@ is child a sized object?
	and	rvc, rva, #0xCF		@ rvc <- possible tag masked for sized object identification
	eq	rvc, #0x4F		@ is child a sized-object?
	beq	gcmszd			@	if so,  jump to mark a sized object
	@ child is cons cell
	orr	sv4, sv4, cnt		@ sv4 <- grey set word updated with child cons cell bit
	str	sv4, [sv2, sv3, lsl #2]	@ store updated mark in grey set
gcmve2:	@ [internal re-entry for vectors]
	ldr	sv3, [sv1, #4]		@ sv3 <- cdr of child
	str	sv5, [sv1, #4]		@ store parent in cdr of child (DSW)
	set	sv5, sv1		@ sv5 <- child (becomes parent)
	set	sv1, sv3		@ sv1 <- cdr of child (becomes child)
	b	gcm1_l			@ jump down to mark first child

gcmszd:	@ mark a sized object
	lsrs	rvc, rva, #8		@ rvc <- object length, is this a zero-length object?
	itT	eq
	orreq	glv, glv, cnt		@	if so,  glv <- old black bits + object's bits
	streq	glv, [dts, sv3, lsl #2]	@	if so,  add object bits to black set
	beq	gcm1up			@	if so,  jump up to continue processing of parent
	tst	rva, #0x30		@ is child a vector?
	beq	gcmvec			@	if so,  jump to that case
	@ child is non-gceable (string, symbol, bytevector)
	sub	sv4, sv1, fre
	lsr	sv4, sv4, #3
	and	rvc, sv4, #0x1f		@ rvc <- bit position for object's start address
	mvn	cnt, #0
	lsl	cnt, cnt, rvc	
	add	rva, rva, #0x0300
	lsr	rva, rva, #11		@ rva <- (size-in-8bytes - 1) of sized item
	add	rva, rva, rvc		@ rva <- bit position of object's end address
	add	rva, rva, #1
gcm1_3:	@ set sized object's bits in black area
	cmp	rva, #32		@ do multiple words have to be marked?
	itTT	mi
	mvnmi	sv4, #0
	lslmi	sv4, sv4, rva
	eormi	cnt, cnt, sv4
	ldr	glv, [dts, sv3, lsl #2]	@ glv <- grey bits from black set
	orr	glv, glv, cnt		@ glv <- old black bits + object's bits
	str	glv, [dts, sv3, lsl #2]	@ add object bits to black set
	bmi	gcm1up			@	if so,  jump up to continue processing of parent
	mvn	cnt, #0
	add	sv3, sv3, #1		@ sv3 <- offset to next word to mark / 4
	sub	rva, rva, #32
	b	gcm1_3
gcmvec:	@ child is a vector
	orr	sv4, sv4, cnt		@ sv4 <- grey set word updated with child vector start bit
	str	sv4, [sv2, sv3, lsl #2]	@ store updated mark in grey set
	bic	rva, rvc, #1
	add	sv1, sv1, rva, lsl #2
	tst	rvc, #1
	bne	gcmve2
	sub	sv3, sv1, fre
	lsr	sv3, sv3, #3
	and	rvc, sv3, #0x1f		@ rvc <- bit position for object's start address
	set	cnt, #1			@ cnt <- bit in position 0
	lsl	cnt, cnt, rvc		@ cnt <- positioned bit for object
	lsr	sv3, sv3, #5		@ sv3 <- offset of word in grey/black area
	ldr	sv4, [sv2, sv3, lsl #2]	@ sv4 <- word from grey set
	ldr	glv, [dts, sv3, lsl #2]	@ glv <- grey bits from black set
	orr	sv4, sv4, cnt		@ glv <- old black bits + object's bits
	str	sv4, [sv2, sv3, lsl #2]	@ sv4 <- word from grey set
	orr	glv, glv, cnt		@ glv <- old black bits + object's bits
	str	glv, [dts, sv3, lsl #2]	@ add object bits to black set
	ldr	sv3, [sv1]
	str	sv5, [sv1]
	set	sv5, sv1
	set	sv1, sv3
	b	gcm1_l			@ jump down to mark first child
		

gcswp1:	@ sweep phase 1: build free cell list
	set	sv2, #null		@ sv2 <- top of free cell list
	set	sv3, #grey_set_size	@ sv3 <- offset to after-end of black set
	sub	sv3, sv3, #1
	ldr	sv4, [dts, sv3, lsl #2]	@ sv4 <- last word from black set
	tst	sv4, #0x80000000	@ does black set end with free cell?
	itTE	eq
	seteq	rvc, #1			@	if so,  rvc <- 1 -- we start looking for bottom of hole
	biceq	rva, env, #i0		@	if so,  rva <- heaptop (address from pseudo scheme int)
	setne	rvc, #0			@	if not, rvc <- 0 -- we start looking for top of hole
	@ find top/bottom of holes by scanning black set words
	set	sv3, #grey_set_size
gcs1_0:	eq	sv3, #0			@ done scanning for end of free space?
	beq	gcs1_c			@	if so,  jump to exit
	sub	sv3, sv3, #1
	ldr	sv4, [dts, sv3, lsl #2]	@ sv4 <- word from black set
	eq	rvc, #0			@ are we looking for top of hole?
	it	eq
	mvneq	sv4, sv4		@	if so,  sv4 <- inverted word from black set
	eq	sv4, #0			@ does black-set-word contain hole top/bottom?
	beq	gcs1_0			@	if not, jump back to scan next black-set-word
	@ find top/bottom of hole based on bits in black-set-word
	add	sv1, fre, sv3, lsl #8	@ sv1 <- base address of black-set-word
	add	sv1, sv1, #256		@ sv1 <- address of next item (256 bytes per black-set-word)
	set	sv5, #0x80000000	@ sv5 <- bit mask, at top bit position
gcs1_1:	eq	sv5, #0			@ black-set-word fully scanned?
	beq	gcs1_0			@	if so,  jump back to scan next black-set-word
	sub	sv1, sv1, #8		@ sv1 <- address of item at bit in sv5
	tst	sv4, sv5		@ is item a top/bottom of hole?
	lsr	sv5, sv5, #1		@ sv5 <- mask bit shifted for next item
	beq	gcs1_1			@	if not, jump back to test next bit
	mvn	sv4, sv4		@ sv4 <- black-set-word inverted (to find bottom/top of hole)
	eq	rvc, #0			@ were we looking for top of hole?
	itT	eq
	seteq	rva, sv1		@	if so,  rva <- address of top of this hole
	seteq	rvc, #1			@	if so,  rvc <- 1 = we will now look for bottom of this hole
	beq	gcs1_1			@	if so,  jump back to look for bottom of this hole
	str	sv2, [sv1, #12]		@ store free-cell list in hole
	add	sv2, sv1, #8		@ sv2 <- updated start of free-cell list
	sub	rva, rva, sv1		@ rva <- size of hole (in bytes)
	str	rva, [sv1, #8]		@ store size-of-hole in hole
	set	rvc, #0			@ rvc <- 0 = we will now look for top of next hole
	b	gcs1_1			@ jump back to look for top of next hole
	
gcs1_c:	@ set hole sizes to cumulative sizes in free cell list
	set	sv5, sv2
	set	rva, #0
gcs1_d:	@ loop
	nullp	sv5
	itTTT	ne
	ldrne	sv4, [sv5]
	addne	rva, rva, sv4
	strne	rva, [sv5]
	ldrne	sv5, [sv5, #4]
	bne	gcs1_d

	@ sweep phase 2: update pointers for upcoming crunch
	nullp	sv2
	beq	gcs3_0
	set	sv3, sv2		@ sv3 <- end   of 1st black area (start of 1st hole)
	set	sv4, fre		@ sv4 <- start of 1st black area
	sub	sv4, sv4, #4		@ sv4 <- address of 32-bit cell before 1st one to be visited
	set	sv5, sv4		@ sv5 <- end address of prior vector (semi-dummy but needed)
	set	rvc, #0			@ rvc <- 0 = cumulative free area up to this point
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
	and	rva, sv1, #0xCF		@ rva <- content masked for sized item identification
	eq	rva, #0x4F		@ is content a sized item tag?
	bne	gcs2_1			@	if not, jump back to process next 32-bit cell
	tst	sv1, #0x30		@ is it a vector?
	itEE	eq
	addeq	sv5, sv4, sv1, LSR #6	@	if so,  sv5 <- position of end of object + 1
	addne	sv5, sv4, sv1, LSR #8	@	if not, sv4 <- position of next object, unaligned
	addne	sv5, sv5, #3		@	if not, sv4 <- pos of next object, unaligned + 3 (to align)
	bic	sv5, sv5, #7		@	if so,  sv5 <- position of end of object
	add	sv5, sv5, #4
	it	ne
	setne	sv4, sv5
	b	gcs2_1			@ jump back to process next 32-bit cell

gcs2_2:	@ deal with pointer
	cmp	sv1, sv2
	it	pl
	cmppl	env, sv1
	bmi	gcs2_1
	set	dts, sv2
gcs2_3:	@ loop
	ldr	rva, [dts, #4]
	nullp	rva
	it	eq
	seteq	rva, env
	cmp	sv1, rva
	it	pl
	setpl	dts, rva
	bpl	gcs2_3
	ldr	rva, [dts]
	sub	sv1, sv1, rva
	str	sv1, [sv4]
	b	gcs2_1
	
gcs2_4:	@ go to next black area
	bic	rva, env, #i0
	eq	sv3, rva
	beq	gcs3_0
	ldr	rva, [sv3]
	sub	rvc, rva, rvc
	add	sv4, sv3, rvc
	sub	sv4, sv4, #4
	set	rvc, rva
	ldr	sv3, [sv3, #4]
	nullp	sv3
	it	eq
	biceq	sv3, env, #i0
	b	gcs2_1
	
gcs3_0:	@ sweep phase 3: crunch
	nullp	sv2			@ no free cells?
	itE	eq
	biceq	sv3, env, #i0		@	if so,  sv3 <- heaptop (heap full)
	setne	sv3, sv2		@	if not, sv3 <- address of start of infillng area (1st hole)
	set	rva, #0			@ rva <- 0 = cumulative size of free cells below this point
gcs3_1:	@ loop over holes
	nullp	sv2			@ no more free cells?
	beq	gcexit			@	if so,  jump to exit
	set	rvc, rva		@ rvc <- previous cumulative free cell size
	ldr	rva, [sv2]		@ rva <- cumulative free cell size
	sub	rvc, rva, rvc		@ rvc <- size of this hole
	add	sv1, sv2, rvc		@ sv1 <- address of start of next black area to be moved down
	ldr	sv2, [sv2, #4]		@ sv2 <- address of free area after next black area
	nullp	sv2			@ is this the last free area?
	itE	eq
	biceq	rvc, env, #i0		@	if so,  rvc <- end of black area = heaptop
	setne	rvc, sv2		@	if not, rvc <- end of black area = start of next hole
gcs3_4:	@ loop to move data down, filling hole
	cmp	sv1, rvc
	itT	mi
	ldmiami	sv1!, {sv4, sv5}
	stmiami	sv3!, {sv4, sv5}
	bmi	gcs3_4
	b	gcs3_1

gcexit:	@ restore registers
	add	rva, fre, #4		@ rva <- heapbottom + 4 (from fre), to restore regs
	set	fre, sv3
	ldmia	rva, {cnt, sv1-sv5, env, dts, glv} @ restore regs
	@ reset if memory is still exhausted after gc
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	bic	rva, rva, #i0
	sub	rva, rva, fre		@ rva <- free heap space bytes (raw int)
	cmp	rva, rvb		@ are enough bytes free?
	it	mi
	ldrmi	pc,  =reset0		@	if not, jump to reset, memory is exhausted
	@ exit
	orr	fre, fre, #0x02		@ fre <- ...bbb10		(back to de-reserved)
	bic	lnk, lnk, #lnkbit0
	sub	lnk, lnk, #4		@ return (restart allocation that triggered gc)
	set	pc,  lnk

.ltorg
	
.endif	@ mark-and-sweep


@---------------------------------------------------------------------------------------------------------
@ I.D.3. memory allocation:			zmaloc, cons, list, save
@---------------------------------------------------------------------------------------------------------

.balign	4

s_alo:	SYMSIZE	4
	.ascii	"_alo"
	.balign 4

_maloc:	@ (_alo)
	SYNTAX	0			@ primitive syntax, no input args
_func_
zmaloc:	@ allocate rvb (raw int, aligned) bytes of memory to a sized object (bytevector)
	@ allocated block is 8-byte aligned
	@ on entry:	rvb <- number of bytes to allocate (raw int)
	@ on entry:	fre <- current free cell address (normally reserved)
	@ on exit:	rva <- allocated address (content is tagged as bytevector)
	@ on exit:	rvb <- modified if needed for 8-byte alignment
	@ on exit:	fre <- allocated address (level 1 reserved, i.e. ...bbb01)
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ unconditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	add	rvb, rvb, #7		@ rvb <- number of bytes to allocate, + 7
	bic	rvb, rvb, #7		@ rvb <- number of bytes to allocate, 8-byte aligned
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, fre		@ rva <- free heap space bytes (raw int)
	cmp	rva, rvb		@ are enough bytes free?
	bmi	gc_bgn			@	if not, jump to perform gc and restart zmaloc
	sub	rva, rvb, #4		@ rva <- number of bytes in object
	lsl	rva, rva, #8		@ rva <- number of bytes in object, shifted
	orr	rva, rva, #bytevector_tag @ rva <- full tag for memory area
	str	rva, [fre, #-1]		@ set word #1 of sized object to bytevector tag
	bic	rva, fre, #0x01		@ rva <- address of zmaloc cell
	set	pc,  lnk

.balign	4

s_cons:	SYMSIZE	4
	.ascii	"_cns"
	.balign 4

_cons:	@ (_cns)
	SYNTAX	0			@ primitive syntax, no input args
_func_
cons:	@ primitive cons:	called by cons and list macros
	@ on entry:	fre <- current free cell address (normally reserved)
	@ on exit:	rva <- allocated address (content is tagged as bytevector)
	@ on exit:	rvb <- unmodified or 8 (raw int) if gc was triggered
	@ on exit:	rvc <- '() (for cdr of cons cell, if called from list macro)
	@ on exit:	fre <- allocated address (level 1 reserved, i.e. ...bbb01)
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ unconditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	@ assuming current heaptop is stored in glv (updated by gc and/or install)
	@ assuming heap size is always 8-byte aligned
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is an 8-byte cell available?
	itTT	hi
	bichi	rva, fre, #0x03		@	if so,  rva <- address of allocated memory
	sethi	rvc, #null		@	if so,  rvc <- '() (for list macro)
	sethi	pc,  lnk		@	if so,  return to complete cons macro	
alogc8:	set	rvb, #8			@ rvb <- number of bytes to allocate (8)
	b	gc_bgn			@ jump to perform gc and restart cons
	
.balign	4

s_save:	SYMSIZE	4
	.ascii	"_sav"
	.balign 4

_save:	@ (_sav)
	SYNTAX	0			@ primitive syntax, no input args
_func_
save:	@ primitive save:	called by save macro
	@ on entry:	fre <- current free cell address (not reserved)
	@ on entry:	dts <- current scheme stack
	@ on exit:	rva <- next free address
	@ on exit:	rvb <- unmodified or 8 (raw int) if gc was triggered
	@ on exit:	fre <- next free address (de-reserved, i.e. ...bbb10)
	@ on exit:	dts <- updated scheme stack with free car
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ conditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	@ assuming current heaptop is stored in glv (updated by gc and/or install)
	@ assuming heap size is always 8-byte aligned (heaptop always adjusted by 16-bytes)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is an 8-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {fre, dts}	@ rva <- address of next free cell, + store dummy in prior fre cell
	sub	dts, rva, #8		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

.ifndef	inline_cons

.balign	4

_func_
cons2:	@ primitive cons2:	called by bcons and lcons macros (called by save2 macro)
	@ on entry:	fre <- current free cell address (not reserved)
	@ on exit:	rva <- allocated address
	@ on exit:	rvc <- allocated address
	@ on exit:	fre <- current free cell address (reserved level 1)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	itTT	hi
	bichi	rva, fre, #0x03		@ rva <- address of allocated memory
	sethi	rvc, rva
	sethi	pc,  lnk		@ return to complete save macro
algc16:	set	rvb, #16		@ rvb <- number of bytes to allocate (16)
	b	gc_bgn			@ jump to perform gc and restart cons

.balign	4

_func_
cons3:	@ primitive cons3:	called by llcons macro (called by save3 macro)
	@ on entry:	fre <- current free cell address (not reserved)
	@ on exit:	rva <- allocated address
	@ on exit:	rvb <- allocated address
	@ on exit:	rvc <- allocated address + 8 (start of next cons cell)
	@ on exit:	fre <- current free cell address (reserved level 1)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 24-byte cell available?
	itTTT	hi
	bichi	rva, fre, #0x03		@ rva <- address of allocated memory
	sethi	rvb, rva
	addhi	rvc, rva, #8
	sethi	pc,  lnk		@ return to complete save macro
algc24:	set	rvb, #24		@ rvb <- number of bytes to allocate (24)
	b	gc_bgn			@ jump to perform gc and restart cons

.balign	4

_func_
sav__c:	@ primitive sav__c:	save cnt on stack
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	alogc8
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {cnt, dts}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

.balign	4

_func_
sav_ec:	@ primitive sav_ec:	save env and cnt on stack
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
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

.balign	4

_func_
sav_rc:	@ primitive sav_rc:	save cnt on stack with room for register at top (reg cnt -> dts)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts, glv, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

.balign	4

_func_
savrec:	@ primitive savrec:	save env and cnt on stack, with room for reg at top (reg env cnt -> dts)
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc24
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts}
	add	rvb, rva, #8
	stmia	rva!, {cnt,rvb,env,rvc}
	sub	dts, rva, #16		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

.else	@ inline_cons

.balign	4

algc16:	set	rvb, #16		@ rvb <- number of bytes to allocate (16)
	b	gc_bgn			@ jump to perform gc and restart cons

.balign	4

algc24:	set	rvb, #24		@ rvb <- number of bytes to allocate (24)
	b	gc_bgn			@ jump to perform gc and restart cons

.endif	@ ifndef inline_cons
	
.balign	4

s_mkc:	SYMSIZE	4
	.ascii	"_mkc"
	.balign 4

_mkcpl:	@ (_mkc)
	SYNTAX	2			@ primitive syntax, two input args
	@ on entry:	sv1 <- address of proc's vars-list
	@ on entry:	sv2 <- address where proc's code starts
	@ on exit:	sv1 <- compiled proc
	set	rvb, #24
	bl	zmaloc
	set	rvc, rva
	stmia	rva!, {sv1, sv2, env, rvc}
	set	rvc, #procedure
	orr	rvc, rvc, #0xC000
	stmia	rva!, {rvc}
	sub	rvc, rva, #12
	stmia	rva!, {rvc}
	sub	sv1, rva, #8		@ sv1 <- address of destination (symbol), [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  cnt

@---------------------------------------------------------------------------------------------------------
@ I.D.4. common function exits
@---------------------------------------------------------------------------------------------------------

.balign	4

trufun:	@ internal function returning #t, used in ports
	PFUNC	0
_func_
trufxt:	@ exit with #t
	set	sv1, #t
	set	pc,  cnt

_func_
boolxt:	@ boolean function exit, based on test result, eq -> #t, ne -> #f
	itE	eq
	seteq	sv1, #t
	setne	sv1, #f
	set	pc,  cnt
	
_func_
flsfxt:	@ exit with #f
	eq	sv1, sv1
_func_
notfxt:	@ exit with #t/#f in opposition to test result, ne -> #t, eq -> #f
	itE	eq
	seteq	sv1, #f
	setne	sv1, #t
	set	pc,  cnt
	
.balign	4

npofun:	@ internal function returning npo, used in ports
	PFUNC	0
_func_
npofxt:	@ exit with non-printing-object
	set	sv1, #npo
	set	pc,  cnt

.balign	4
	
_func_
return:	@ exit (used by numeric function jump tables)
	set	pc,  cnt
	
.balign	4
	
lnkret:	@ internal function, just returning, used in ports
	@ return
	PFUNC	0
_func_
lnkfxt:	@ return via lnk	
	set	pc, lnk

	
@---------------------------------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.5.   eval:				eval, interaction-environment
@---------------------------------------------------------------------------------------------------------

	
_func_
typchk:	@ return #t/#f based on whether type tag of object in sv1
	@ matches sv4
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
	ldrb	rva, [sv1]
	and	rvc, rva, #0x07
	eq	rvc, #0x03
	itEE	eq
	andeq	rva, rva, #0x0f
	andne	rvc, rva, #0x47
	eqne	rvc, #0x47
	it	ne
	setne	rva, #0xFF
	set	pc,  lnk
_func_
tpckxt:	@ exit for type tag checking (typchk)
	adr	rvc, typtbl
	add	rvc, rvc, sv4, lsr #2
	ldrb	rvc, [rvc]
	eq	rva, rvc
	b	boolxt

typtbl:	@ type classification table for typchk
	
aint:	.byte	i0		@ number? (integer_only)
achr:	.byte	npo		@ char?
anul:	.byte	null		@ null?
aprc:	.byte	proc		@ procedure?
anot:	.byte	f		@ not
avar:	.byte	variable_tag	@ symbol?
astr:	.byte	string_tag	@ string?
avec:	.byte	vector_tag	@ vector?
avu8:	.byte	bytevector_tag	@ bytevector?

	@ type classification constants for EPFUNC-otypchk

	iint	= ((aint - typtbl) << 2) | i0
	ichr	= ((achr - typtbl) << 2) | i0
	inul	= ((anul - typtbl) << 2) | i0
	iprc	= ((aprc - typtbl) << 2) | i0
	inot	= ((anot - typtbl) << 2) | i0
	ivar	= ((avar - typtbl) << 2) | i0
	istr	= ((astr - typtbl) << 2) | i0
	ivec	= ((avec - typtbl) << 2) | i0
	ivu8	= ((avu8 - typtbl) << 2) | i0
	
.balign	4
		
_func_
typsv2:	@ return in rvb the type tag of sv2
	@ modifies:	rvb, rvc
	ands	rvb, sv2, #0x03
	beq	tpsv2p
	eq	rvb, #3
	it	eq
	andeq	rvb, sv2, #0xff
	set	pc,  lnk
tpsv2p:	@ pointer
	ldrb	rvb, [sv2]
	and	rvc, rvb, #0x07
	eq	rvc, #0x03
	itEE	eq
	andeq	rvb, rvb, #0x0f
	andne	rvc, rvb, #0x47
	eqne	rvc, #0x47
	it	ne
	setne	rvb, #0xFF
	set	pc,  lnk

.balign	4

_func_
evlmac:	@ evaluate a macro
.ifndef r3rs
	set	sv1, sv2
	sav_ec
	call	mxpnd			@ sv1 <- parsed expr with expanded macros
	restor2	env, cnt		@ env <- env, cnt <- cnt, dts <- (...)
	b	eval
.else
	b	corerr
.endif

.balign	4

_func_
eval:	@ [internal entry]
	@ on entry:	sv1 <- exp == self-eval-obj or var/synt or application or macro to expand
	and	rva, sv1, #0xff
	eq	rva, #variable_tag
	beq	evlvar
	pntrp	rva
	it	ne
	setne	pc,  cnt
	car	rva, sv1
	and	rva, rva, #0x47
	eq	rva, #0x47
	it	eq
	seteq	pc,  cnt
	@ continue to evalls
_func_
evalls:	@ sv1 <- exp == (ufun uarg1 uarg2 ...) where ufun could be a macro
	@ evaluate ufun, uarg1 ... and branch to apply, unless fun is a macro
	and	rva, rva, #0x07		@ rva <- rat/cpx tag extraction
	eq	rva, #3			@ is pointed item a rat/cpx?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	set	sv2, sv1		@ sv2 <- (ufun uarg1 uarg2 ...)
	car	sv1, sv2		@ sv1 <- ufun
	evalsv1				@ sv1 <- fun	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	pntrp	sv1
	itTT	eq
	careq	rva, sv1
	andeq	rvb, rva, #0xF7
	eqeq	rvb, #0xD7		@ is fun a primitive, procedure, continuation or macro?
	bne	corerr			@	if not, jump to report error
	bic	rvb, rva, #0x0700	@ rva <- type tag without #input-args (if prim)
	eq	rvb, #macro
	beq	evlmac
	cdr	sv2, sv2
	nullp	sv2			@ no args?
	beq	apply0			@	if so,  jump to apply
	tst	rva, #0x0800		@ is fun of syntax type?
	bne	apply0			@	if so,  jump to apply (don't evaluate args)
	@ evaluate the arguments of a compound expression, then jump to apply

.ifdef	small_eval_apply

	save	sv1			@ dts <- (fun ...)
	set	sv4, #null		@ sv4 <- initial, reversed, evaluated args-list
evlarg:	@ sv2 <- (uarg1 uarg2 ...), sv4 <- (fun . result), dts <- ((fun . result) ...)
	snoc	sv1, sv2, sv2		@ sv1 <- uarg1,	sv2 <- (uarg2 ...)
	evalsv1				@ sv1 <- arg1	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	cons	sv4, sv1, sv4		@ sv4 <- updated, reversed, evaluated args-list
	nullp	sv2			@ no uarg left?
	bne	evlarg			@	if not, jump to continue processing uargs
	@ de-reverse evaluated args-list
	set	sv2, #null		@ sv2 <- initial evaluated args-list
evlrag:	snoc	sv1, sv4, sv4
	cons	sv2, sv1, sv2
	nullp	sv4
	bne	evlrag
	restor	sv1			@ sv1 <- fun,	dts <- (...)
	car	rva, sv1
	b	apply0

_func_
sbevll: @ evaluate a list (or rat/cpx)
  .ifndef integer_only
	and	rva, rva, #0x07		@ rva <- rat/cpx tag extraction
	eq	rva, #3			@ is pointed item a rat/cpx?
	it	eq
	seteq	pc,  lnk		@	if so,  return
  .endif
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and even if Thumb2)
	sav_ec
	save3	sv2, sv3, sv4		@ dts <- (uarg-list lnk result env cnt ...)
	call	evalls
	restor3	sv2, sv3, sv4
	restor2	env, cnt
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk

.else	@ non-small eval/apply

	@ count number of args provided
	set	sv5, sv2		@ sv5 <- (uarg1 uarg2 ...)
	set	rvb, #0			@ rvb <- initial arg count
evarct:	pntrp	sv5
	itT	eq
	addeq	rvb, rvb, #1
	cdreq	sv5, sv5
	beq	evarct
	@ check that args-list is proper (error out if not)
	nullp	sv5
	it	ne
	setne	sv1, sv2
	bne	corerr
	@ jump to different case if more than 3 args or fun is a continuation
	cmp	rvb, #4
	bpl	evargn
	and	rvc, rva, #0xf000
	eq	rvc, #0x8000		@ is fun a continuation?
	beq	evargn			@	if so,  use slow process (args will be listed)
	@ check if primitive, if not, count number of vars and check for improper vars-list
	eq	rvc, #0			@ is fun a primitive?
	itT	eq
	andeq	rva, rva, #0x0700	@ rvb <- number of input args of primitive (raw int, shifted)
	lsreq	rva, rva, #8
	beq	evlrg0
	caddr	sv3, sv1		@ sv3 <- var-list
	set	rva, #0
evapct:	pntrp	sv3
	itT	eq
	addeq	rva, rva, #1
	cdreq	sv3, sv3
	beq	evapct
	@ if improper vars-list, jump to general case
	nullp	sv3
	bne	evargn
evlrg0:	@ if unequal number of vars vs args, jump to general case
	eq	rva, rvb
	bne	evargn
	@ case with proper vars-list,
	@ less than 4 vars,
	@ equal number of args,
	@ and not a continuation
	@ sv1 <- fun
	@ sv2 <- (uarg1 ...)
	set	sv4, sv1		@ sv4 <- fun, saved against eval (esp. eval-var)
	snoc	sv1, sv2, sv2		@ sv1 <- uarg1,	sv2 <- (<uarg2> <uarg3>) saved against eval
	evalsv1				@ sv1 <- arg1	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	nullp	sv2			@ no uarg2?
	beq	evlrg6			@	if so,  jump to go to function
	set	sv3, sv1		@ sv3 <- arg1
	snoc	sv1, sv5, sv2		@ sv1 <- uarg2,	sv5 <- (<uarg3>)
	nullp	sv5			@ any uarg3?
	bne	evlrg3			@	if so,  jump to eval of uarg2 and uarg3
	set	sv2, sv3		@ sv2 <- arg1, saved against eval
	evalsv1				@ sv1 <- arg2	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	set	sv3, sv1		@ sv3 <- arg2
	set	sv1, sv2		@ sv1 <- arg1
	set	sv2, sv3		@ sv2 <- arg2
evlrg6:	@ continue for 1 or 2 args	
	set	sv3, #null		@ sv3 <- '()
evlrg7:	@ continue for 3 args
	car	rvb, sv4
	tst	rvb, #0xf000		@ is fun compiled or compound?
	bne	evrgcp			@	if so,  jump to process that case
	@ execute primitive with args in sv1-sv3
	ands	rvc, rvb, #0x00ff0000
	itTEE	eq
	addeq	rva, sv4, #4
	seteq	sv5, #null
	addne	sv5, sv4, #4
	vcrfine	rva, glv, 16
  .ifndef cortex
	it	ne
	ldrne	rva, [rva, rvc, lsr #14]
  .else
	itT	ne
	lsrne	rvc, rvc, #14
	ldrne	rva, [rva, rvc]
  .endif
	tst	rvb, #0xff000000
	itE	eq
	seteq	sv4, #null
	lsrne	sv4, rvb, #24
	set	pc,  rva

evlrg3:	@ 3 input args, process uarg2 and uarg3
	@ on entry:	sv1 <- uarg2
	@ on entry:	sv3 <- arg1
	@ on entry:	sv4 <- fun
	@ on entry:	sv5 <- (uarg3)
	@ on exit:	sv1 <- arg1
	@ on exit:	sv2 <- arg2
	@ on exit:	sv3 <- arg3
	@ on exit:	sv4 <- fun	(unmodified)
	isave	sv3			@ dts <- (arg1 ...)
	car	sv2, sv5		@ sv2 <- uarg3, saved against eval
	evalsv1				@ sv1 <- arg2	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	set	sv3, sv1		@ sv3 <- arg2
	set	sv1, sv2		@ sv1 <- uarg3
	set	sv2, sv3		@ sv2 <- arg2, saved against eval
	evalsv1				@ sv1 <- arg3	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	set	sv3, sv1		@ sv3 <- arg3
	restor	sv1			@ sv1 <- arg1
	b	evlrg7

evargn:	@ more than 3 args,
	@ or case where number of args differs from number of vars,
	@ or continuation,
	@ either way, there's at least one arg here.
	@ on entry:	rvb <- number of args
	@ on entry:	sv1 <- fun
	@ on entry:	sv2 <- (uarg1 uarg2 ...)
	lsl	rvb, rvb, #3
	add	rvb, rvb, #16
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	add	rva, rva, #8
	sub	rva, rva, rvb
	cmp	rva, fre		@ is a 16-byte cell available?
	itT	ls
	movls	sv3, #null
	movls	sv4, #null
	bls	gc_bgn
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	add	rvc, rva, #8
	stmia	rva!, {rvc}
	stmia	rva!, {dts}
	add	sv5, fre, rvb
	sub	sv5, sv5, #4
evarbl:	add	rvc, rva, #8
	stmia	rva!, {sv1, rvc}
	cmp	rva, sv5
	bls	evarbl
	set	rvc, #null
	str	rvc, [rva, #-4]
	sub	dts, rva, rvb		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	car	sv4, dts		@ sv4 <- (fun arg1_space arg2_space ...)
evlarg:	@ sv2 <- (uarg1 uarg2 ...), sv4 <- (fun arg1_space ...), dts <- ((fun ar1_sp ar2_sp ...) ...)
	snoc	sv1, sv2, sv2		@ sv1 <- uarg1,	sv2 <- (uarg2 ...)
	evalsv1				@ sv1 <- arg1	k(sv2,sv4,env,cnt,dts), x(sv1,sv3,sv5)
	cdr	sv4, sv4		@ sv4 <- (arg1_space arg2_space ...)
	setcar	sv4, sv1		@ sv4 <- (arg1 arg2_space ...)
	nullp	sv2			@ no uarg left?
	bne	evlarg			@	if not, jump to continue processing uargs
	restor	sv1			@ sv1 <- (fun arg1 arg2 ...)
	snoc	sv1, sv2, sv1		@ sv1 <- fun,	sv2 <- (arg1 arg2 ...)
	@ copy the args-list (in case of restart from continuation -- may be important for
	@ listed args / optional args).
	set	sv5, sv2
	set	rvb, #0
evlar0:	pntrp	sv5
	itT	eq
	addeq	rvb, rvb, #8
	cdreq	sv5, sv5
	beq	evlar0
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	add	rva, rva, #8
	sub	rva, rva, rvb
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	gc_bgn
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	sv4, sv2
evlar1:	add	rvc, rva, #8
	snoc	sv3, sv4, sv4
	stmia	rva!, {sv3, rvc}
	nullp	sv4
	bne	evlar1
	set	sv3, #null
	str	sv3, [rva, #-4]
	sub	sv2, rva, rvb		@ sv2 <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	car	rva, sv1
	b	apply0

_func_
sbevll: @ evaluate a list	
	and	rva, rva, #0x07	@ rva <- rat/cpx tag extraction
	eq	rva, #3		@ is pointed item a rat/cpx?
	it	eq
	seteq	pc,  lnk	@	if so,  return
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved against flok, fclose (and even if Thumb2)	
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #24
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc32
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	add	rvb, rva, #8
	set	rvc, #vector_tag
	orr	rvc, rvc, #0x500
	stmia	rva!, {rvb, dts, rvc}
	stmia	rva!, {cnt, sv2-sv4, env}
	sub	dts, rva, #32		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]	
	call	evalls
	snoc	sv2, dts, dts
    .ifndef cortex
	ldmib	sv2, {cnt, sv2-sv4, env}
    .else
	ldmia	sv2, {cnt, rva, sv2-sv4, env}
	set	cnt, rva
    .endif
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk

algc32:	set	rvb, #32		@ rvb <- number of bytes to allocate (32)
	b	gc_bgn			@ jump to perform gc and restart cons


.endif	@ small / non-small eval/apply


@---------------------------------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.4.   control features:		apply
@---------------------------------------------------------------------------------------------------------


.balign	4

s_apl:	SYMSIZE	4
	.ascii	"_apl"
	.balign 4
	
_apply:	@ (_apl)
	SYNTAX	2			@ primtive syntax, two input args
_func_
apply:	@ [internal entry 1]
	execp	sv1			@ is fun a prim, proc or cont?
	bne	corerr			@	if not, jump to apply error	
	car	rva, sv1
apply0:	@ [internal entry 2]
	and	rvb, rva, #0xf000
	adr	rvc, apptbl
.ifndef	cortex
	ldr	pc,  [rvc, rvb, lsr #12]
.else
	lsr	rvb, rvb, #12
	ldr	pc,  [rvc, rvb]
.endif

.balign 4
	
apptbl:	@ apply jump table
	.word	prmapp
	.word	cmpapp
	.word	cntapp
	.word	cplapp

_func_
prmapp:	@ primitive-apply
	@ on entry:	sv1 <- primitive
	@ on entry:	sv2 <- arg-list
	@ on entry:	rva <- 32-bit tag of primitive
	and	rvb, rva, #0x0700	@ rvb <- number of input args of primitive (raw int, shifted)
	tst	rva, #0xff000000
	itE	eq
	seteq	sv4, #null
	lsrne	sv4, rva, #24
	ands	rvc, rva, #0x00ff0000
	itTEE	eq
	addeq	rva, sv1, #4
	seteq	sv5, #null
	addne	sv5, sv1, #4
	vcrfine	rva, glv, 16
.ifndef	cortex
	it	ne
	ldrne	rva, [rva, rvc, lsr #14]
.else
	itT	ne
	lsrne	rvc, rvc, #14
	ldrne	rva, [rva, rvc]
.endif
	set	sv1, sv2		@ sv1 <- argument list
	set	sv2, #null		@ sv2 <- '()
	set	sv3, sv2		@ sv3 <- '()
	eq	rvb, #0x0000		@ does primitive have 0 (or a variable number of) args?
	it	ne
	nullpne	sv1			@	if not, is arg-list null?
	it	eq
	seteq	pc,  rva		@	if so,  jump to prim's label with sv1 = '() or '(arg1 ...)
	@ un-list proc's input argument list into registers sv1 to sv5 and branch to proc
	snoc	sv1, sv2, sv1		@ sv1 <- arg1,		sv2 <- (<arg2> <arg3> <arg4> <arg5> ...)
	eq	rvb, #0x0100		@ does primitive have 1 input argument?
	it	ne
	nullpne	sv2			@	if not, is the rest of the arg-list null?
	it	eq
	seteq	pc,  rva		@	if so,  jump to prim's label wit sv1=arg1, sv2=()|(arg2 ..)
	snoc	sv2, sv3, sv2		@ sv2 <- arg2,		sv3 <- (<arg3> <arg4> <arg5> ...)
	eq	rvb, #0x0200		@ does primitive have 2 input arguments?
	it	ne
	nullpne	sv3			@	if not, is the rest of the arg-list null?
	it	eq
	seteq	pc,  rva		@	if so,  jmp to prim's lbl wit sv1=a1,sv2=a2,sv3=()|(a3 ..)
	snoc	sv3, sv4, sv3		@ sv3 <- arg3,		sv4 <- (<arg4> <arg5> ...)
	eq	rvb, #0x0300		@ does primitive have 3 input arguments?
	it	ne
	nullpne	sv4			@	if not, is the rest of the arg-list null?
	it	ne
	snocne	sv4, sv5, sv4		@ sv4 <- arg4,		sv5 <- (<arg5> ...)
	set	pc,  rva		@ jmp to prim's label wit 3 or 4 args (sv5 is rest)

.ifdef	small_eval_apply

.balign	4
	
_func_
cmpapp:	@ compound-apply
_func_
cplapp:	@ compiled proc apply
	@ on entry:	sv1 <- listed-proc or compiled-proc
	@ on entry:	sv2 <- arg-list
	@ on entry:	rva <- 32-bit tag of primitive
	lsr	sv5, rva, #13		@ sv5 <- type, listed = 2, compiled = 6 (float-tagged)
	cdr	sv1, sv1		@ sv1 <- (env vars-list . address-or-body)
	snoc	sv3, sv4, sv1		@ sv3 <- env, sv4 <- (vars-list . address-or-body)
	set	sv1, #null		@ sv1 <- '()
	cons	env, sv1, sv3		@ env <- (() env) = pre-extended proc env
	snoc	sv3, sv4, sv4		@ sv3 <- vars-list, sv4 <- address-or-body
	nullp	sv3			@ no vars?
	it	eq
	eqeq	sv5, #6			@	if so,  is it a compiled proc?
	it	eq
	seteq	pc,  sv4		@	if so,  jump to execute its code
	set	sv1, sv4		@ sv1 <- address-or-body
	nullp	sv3			@ no vars?
	beq	sqnce			@	if so,  jump to execute it
	save3	sv1, sv5, cnt		@ dts <- (address-or-body type cnt ...)
	set	sv4, sv2		@ sv4 <- val-list
	call	mkfrm0			@ env <- (new-frame  . env)
	restor3	sv1, sv5, cnt		@ sv1 <- address-or-body, sv5 <- type, cnt <- cnt, dts <- (...)
	eq	sv5, #6			@ is fun a compiled_proc?
	it	eq
	seteq	pc,  sv1		@	if so,  jump to execute its code
	
.else	@ non-small eval/apply
	
.balign	4
	
_func_
cmpapp:	@ compound-apply
_func_
cplapp:	@ compiled proc apply
	@ compound or compiled func with more than 3 args,
	@ or with unequal number of args and vars
	@ on entry:	sv1 <- listed-proc or compiled-proc == (proc-tag env vars-list . address-or-body)
	@ on entry:	sv2 <- val-list
	@ on entry:	rva <- 32-bit tag of primitive
	caddr	sv3, sv1		@ sv3 <- var-list
	set	rvb, #8
cpapct:	pntrp	sv3
	itT	eq
	addeq	rvb, rvb, #16
	cdreq	sv3, sv3
	beq	cpapct
	nullp	sv3
	it	ne
	addne	rvb, rvb, #16
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	add	rva, rva, #8
	sub	rva, rva, rvb
	cmp	rva, fre		@ is enough memory available?
	itT	ls
	movls	sv4, #null
	movls	sv5, #null
	bls	gc_bgn
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	eq	rvb, #8
	itE	eq
	seteq	rvc, #null
	addne	rvc, rva, #16
	stmia	rva!, {rvc}
	cdr	sv3, sv1		@ sv3 <- (env vars-list . address-or-body)
	snoc	sv3, sv4, sv3		@ sv3 <- env,	sv4 <- (vars-list . address-or-body)
	stmia	rva!, {sv3}
	beq	cpapad
	car	sv4, sv4		@ sv4 <- var-list
	set	env, sv2		@ env <- val-list
cpapa0:	pntrp	sv4
	it	eq
	nullpeq	env
	beq	cpaper
	set	rvc, rva
	pntrp	sv4
	itTEE	eq
	snoceq	sv3, sv4, sv4
	snoceq	sv5, env, env
	setne	sv3, sv4
	setne	sv5, env
	pntrp	sv3
	it	eq
	careq	sv3, sv3
	stmia	rva!, {sv3, sv5, rvc}
	str	sv4, [rva]
	sub	rvc, rva, #20
	cmp	rvc, fre
	bls	cpapac
cpapab:	@ bubble through pointers for sorted insertion
	ldr	sv4, [rvc]
	ldr	sv5, [sv4]
	cmp	sv3, sv5
	bpl	cpapac
	ldr	sv5, [rvc, #16]
	str	sv4, [rvc, #16]
	str	sv5, [rvc]
	sub	rvc, rvc, #16
	cmp	rvc, fre
	bhi	cpapab
cpapac:	@ insertion complete, keep going
	ldr	sv4, [rva]
	add	rvc, rva, #12
	stmia	rva!, {rvc}
	add	rvc, fre, rvb
	sub	rvc, rvc, #4
	cmp	rva, rvc
	bls	cpapa0
	set	rvc, #null
	str	rvc, [rva, #-4]
cpapad:	@ finish up allocation
	sub	env, rva, rvb		@ env <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@ sv1 <- (proc-tag env vars-list . address-or-body)
	@ env <- extended env of closure
	snoc	rva, sv1, sv1
	cddr	sv1, sv1
	lsr	rva, rva, #13		@ sv5 <- type, listed = 2, compiled = 6 (float-tagged)
	eq	rva, #6			@ is fun a compiled_proc?
	it	eq
	seteq	pc,  sv1		@	if so,  jump to execute its code
	b	sqnce

cpaper:	@ error while extending environment
	@ the val-list is too short
	cadr	env, sv1
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	sv1, sv2		@ sv1 <- val-list
	b	corerr

evrgcp:	@ compound or compiled func with 1-3 args and 1-3 vars
	@ on entry:	sv1 <- arg1
	@ on entry:	sv2 <- arg2 | null
	@ on entry:	sv3 <- arg3 | null
	@ on entry:	sv4 <- func
	set	sv5, sv1		@ sv5 <- arg1
    .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return address
    .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- memory transaction return address, adjustd for Thumb2 mode
    .endif
	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	cdr	sv1, sv4
	cadr	rvc, sv1		@ rvc <- var-list
	set	rvb, #8
ceapct:	pntrp	rvc
	itT	eq
	addeq	rvb, rvb, #16
	cdreq	rvc, rvc
	beq	ceapct
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	add	rva, rva, #8
	sub	rva, rva, rvb
	cmp	rva, fre		@ is enough memory available?
	bls	gc_bgn
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	snoc	rvb, sv1, sv1		@ rvb <- env, sv1 <- (vars-list . address-or-body)
	set	rvc, rvb
	add	rvb, rva, #16
	stmia	rva!, {rvb, rvc}	
	car	env, sv1		@ env <- var-list
	set	rvc, rva
	snoc	rvb, env, env		@ rvb <- var1
	stmia	rva!, {rvb, sv5, rvc}
	nullp	env
	it	eq
	seteq	rvb, #24
	beq	ceapac
	@ insert var2
	snoc	sv1, env, env		@ sv1 <- var3, env <- null
	cmp	rvb, sv1
	itTE	pl
	addpl	rvb, rva, #4
	strpl	rvb, [rva, #-4]
	addmi	rvc, rva, #4	
	add	rvb, rva, #12
	stmia	rva!, {rvb, sv1, sv2, rvc}
	nullp	env
	it	eq
	seteq	rvb, #40
	beq	ceapac
	@ transition
	ldr	rvb, [rvc]		@ rvb <- var with largest ID so far  (var1 or var2)
	@ insert var3 -- step 1
	snoc	sv1, env, env		@ sv1 <- var3, env <- null
	cmp	rvb, sv1
	itTE	pl
	addpl	rvb, rva, #4
	strpl	rvb, [rva, #-4]
	addmi	rvc, rva, #4	
	add	rvb, rva, #12
	stmia	rva!, {rvb, sv1, sv3, rvc}
	bmi	ceapab
	@ insert var3 -- step 2
	ldr	rvc, [rva, #-36]
	ldr	rvb, [rvc]
	cmp	rvb, sv1
	itTT	pl
	ldrpl	rvb, [rva, #-20]
	strpl	rvc, [rva, #-20]
	strpl	rvb, [rva, #-36]	
ceapab:	@ continue
	set	rvb, #56
ceapac:	@ store null at end
	stmia	rva!, {env}
ceapad:	@ finish up allocation
	sub	env, rva, rvb		@ env <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	sv1, sv4
	set	sv2, #null
	set	sv3, sv2
	set	sv4, sv3
	set	sv5, sv4
	@ sv1 <- (proc-tag env vars-list . address-or-body)
	@ env <- extended env of closure
	snoc	rva, sv1, sv1
	cddr	sv1, sv1
	lsr	rva, rva, #13		@ sv5 <- type, listed = 2, compiled = 6 (float-tagged)
	eq	rva, #6			@ is fun a compiled_proc?
	it	eq
	seteq	pc,  sv1		@	if so,  jump to execute its code

.endif	@ small/non-small eval/apply

sqnce:	@ [continuation from compound apply, above]
	@ [internal entry] (eg. for begin)
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
	and	rva, sv1, #0xff
	eq	rva, #variable_tag
	beq	sbevlv
	pntrp	rva
	bne	sqnce0
	car	rva, sv1
	and	rva, rva, #0x47
	eq	rva, #0x47
	beq	sqnce0
	b	sbevll

_func_
cntapp:	@ continuation-apply
	@ on entry:	sv1 <- continuation
	@ on entry:	sv2 <- return-value-inside-a-list
	@ on entry:	rva <- 32-bit tag of primitive
	set	sv4, sv1		@ sv4 <- continuation, saved against bndchk
	ldr	sv1, =winders_var
	bl	bndchk			@ sv1 <- (_winders . winders) or #t
	pntrp	sv3
	itE	eq
	seteq	sv1, sv5		@	if so,  sv1 <- winders
	setne	sv1, #null		@	if not, sv1 <- '()
	set	sv5, sv3
	cdr	sv4, sv4		@ sv4 <- (cnt-winders cnt env ...)
	snoc	sv3, sv4, sv4		@ sv3 <- cnt-winders, sv4 <- (cnt env ...)
	eq	sv1, sv3
	bne	cntapw
cntaxt:	@ winders are unwound or inexistent, continue with applying continuation	
	@ sv4 <- (cnt env  ...),  sv2 <- return-value-inside-a-list
	set	dts, sv4		@ dts <- (cnt env  ...)
	set	sv1, sv2		@ sv1 <- ret-val-list
	cdr	sv3, sv1		@ sv3 <- (cdr ret-val-list)
	nullp	sv3			@ are we returning a single value?
	it	eq
	careq	sv1, sv1		@	if so,  sv2 <- ret-val (single return value)
	restor2	cnt, env		@ cnt <- cnt, env <- env, dts <- (...)
	set	pc,  cnt

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
	save	sv2			@ dts <- (return-value-inside-a-list ...)
	save3	sv5, env, sv4		@ dts <- ((_winders . winders) env (cnt env ...) (ret-val) ...)
	@ indentify shortests winders list
	set	sv5, #t			@ sv5 <- #t, assume winders (sv1) is shorter than cnt-winders (sv3)
	set	sv2, sv1
	set	sv4, sv3
cntap0:	nullp	sv2
	beq	cntap1
	nullp	sv4
	it	eq
	seteq	sv5, #f
	beq	cntap1
	cdr	sv2, sv2
	cdr	sv4, sv4
	b	cntap0
cntap1:	@ sv5 <- #t => unwind the befores (entering dynamic context)
	@ sv5 <- #f => unwind the afters (leaving dynamic context)
	eq	sv5, #t
	beq	cntap4
	@ unwind the afters in winders (sv1)
	save	sv3			@ dts <- (cnt-wndrs (_wndrs . wndrs) env (cnt env ..) (ret-val) ..)
cntap2:	snoc	sv3, sv4, dts		@ sv3 <- cnt-wndrs, sv4 <- ((_wnds . wnds) env (cnt env .) (rv) .)
	snoc	sv4, sv5, sv4		@ sv4 <- (_wndrs . wndrs), sv5 <- (env (cnt env ...) (ret-val) ...)
	cdr	sv1, sv4		@ sv1 <- winders
	eq	sv1, sv3
	beq	cntap3
	snoc	sv1, sv2, sv1		@ sv1 <- 1st winder = (before . after),  sv2 <- remaining winders
	setcdr	sv4, sv2		@ update _winders binding to remaining winders
	car	env, sv5
	cdr	sv1, sv1		@ sv1 <- after
	set	sv2, #null
	call	apply
	b	cntap2
cntap3:	cdr	dts, sv5		@ dts <- ((cnt env ...) (ret-val) ...)
	restor2	sv4, sv2		@ sv4 <- (cnt env ...), sv2 <- return-value-inside-a-list
	b	cntaxt
cntap4:	@ unwind the before in cnt-winders (sv3)
	set	sv4, #null
cntap5:	eq	sv1, sv3
	beq	cntap6
	cons	sv4, sv3, sv4
	cdr	sv3, sv3
	b	cntap5
cntap6:	nullp	sv4
	beq	cntap7
	cadr	env, dts		@ env <- env
	save	sv4			@ dts <- (lst-of-wndr-lsts (_wndrs . wndrs) env (cnt env .) (rv) .)
	caaar	sv1, sv4		@ sv1 <- before of 1st winder of 1st-list-of-winders
	set	sv2, #null
	call	apply
	restor	sv4			@ sv4 <- lst-wndrs, dts <- ((_wnds . wnds) env (cnt env .) (rv) .)
	snoc	sv3, sv4, sv4		@ sv3 <- 1st-list-of-winders, sv4 <- rest-of-list-of-winders
	car	sv2, dts		@ sv2 <- (_winders . wndrs)
	setcdr	sv2, sv3
	b	cntap6
cntap7:	cddr	dts, dts		@ dts <- ((cnt env ...) (ret-val) ...)
	restor2	sv4, sv2		@ sv4 <- (cnt env ...), sv2 <- return-value-inside-a-list
	b	cntaxt
	
.else	@ no dynamic-wind in core
	
cntapw:	@
	b	corerr

.endif	@ r3rs vs r5rs
	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

@---------------------------------------------------------------------------------------------------------
@  II.A.4.     Expressions
@  II.A.4.1.   Primitive expression types
@  II.A.4.1.1. variable reference:		bndenv (internal), mkfrm (internal)
@---------------------------------------------------------------------------------------------------------

	
mkfrm:	@ [internal only]
	@ extend env with a binding frame between vars in sv3 and vals in sv4 
	@ on entry:	sv3 <- vars-list = (var1 var2 ...) or ((var1 ...) (var2 ...) ...)
	@ on entry:	sv4 <- val-list
	@ on entry:	env <- current environment
	@ on exit:	env <- environment extended with new binding frame
	@ modifies:	rva-rvc, sv1-sv5, env, lnk
	set	sv5, #null
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
	beq	corerr			@	if so,  jump to report error
	pntrp	sv3			@ is var-list a pointer?
	itTEE	ne
	setne	sv2, sv4		@	if not, sv2 <- val-list
	setne	sv3, #null		@	if not, sv3 <- null = remaining vars
	snoceq	sv1, sv3, sv3		@	if so,  sv1 <- var1, sv3 <- (var2 ...)
	snoceq	sv2, sv4, sv4		@	if so,  sv2 <- val1, sv4 <- (val2 ...)
	pntrp	sv1			@ is var1 a pointer?
	it	eq
	careq	sv1, sv1		@	if so,  sv1 <- var1 (from pointer)
	ibcons	sv2, sv1, sv2, sv3	@ sv2 <- ((var1 . val1) var2 ...)
	@ scan frame for insertion point for new binding
	set	rva, sv1		@ rva <- var1
	car	sv3, env		@ sv3 <- binding frame
	set	sv5, #null
	nullp	sv3			@ is binding frame empty?
	beq	mkfrm2			@	if so,  jump to add new binding
mkfrm1:	car	sv1, sv3		@ sv1 <- 1st binding
	car	rvb, sv1		@ rvb <- var of 1st binding
	cmp	rva, rvb		@ is var1 < var of 1st binding?
	bmi	mkfrm2			@	if so,  jump to add new binding
	set	sv5, sv3		@ sv5 <- bindings list, starting with last inspected binding
	cdr	sv3, sv3		@ sv3 <- rest of bindings list
	nullp	sv3			@ no more bindings?
	bne	mkfrm1			@	if not, jump to continue scanning bindings
mkfrm2:	@ insert new binding at insertion point in frame
	cdr	sv1, sv2		@ sv1 <- (var2 ...)
	setcdr	sv2, sv3		@ sv2 <- ((var1 . val1) . rest of binding frame)
	nullp	sv5			@ any bindings before this one?
	itE	eq
	setcareq env, sv2		@	if not, env <- (((vr1 . vl1) . rst-bndng-frame) . env)
	setcdrne sv5, sv2		@	if so,  env <- ((bndg1 ... (vr1 . vl1) . rst-bn-fr) . env)
	set	sv3, sv1		@ sv3 <- (var2 ...) = remaining vars to deal with
	nullp	sv3			@ any more vars?
	bne	mkfrm0			@	if so,  jump to continue inserting bindings into frame
	set	pc,  cnt		@ return

.balign	4

_func_
bndchk:	@ [internal entry]	called using bl from (set! ...), (defined? ...), (expand ...)
	@ on entry:	sv1 <- var
	@ on entry:	env <- env
	@ on exit:	sv3 <- binding or null
	@ on exit:	sv5 <- binding value
	@ modifies:	sv3, sv5, rva, rvb, rvc
	@ returns via:	lnk
	set	rvc, #null
	b	bndche

.balign	4
	
s_lkp:	SYMSIZE	4
	.ascii	"_lkp"
	.balign 4

_vrlkp:	@ scheme call:	(_lkp var)
	@ on entry:	sv1 <- var-id
	@ on exit:	sv1 <- value  (or var-id if entering via bndche)
	@ on exit:	sv3 <- binding
	@ on exit:	sv5 <- value
	@ preserves:	sv2, sv4
	SYNTAX	1			@ primitive syntax, one input arg	
_func_
evlvar:	@ [internal entry]
	set	lnk, cnt
_func_
sbevlv:	@ [internal entry]
	set	rvc, lnk
_func_
bndche:	@ [internal entry]	
	set	sv5, env		@ sv5 <- env
	set	rvb, sv1		@ rvb <- var
bndene:	pntrp	sv5			@ done with user environment?
	bne	bnden1			@	if so,  jump to look in built-in environment
bndenr:	car	rva, sv5
	and	rva, rva, #0xff
	eq	rva, #vector_tag
	beq	bnden1
	snoc	sv3, sv5, sv5		@ sv1 <- 1st frame, sv5 <- (frame2 ...)
	nullp	sv3			@ is binding-list null?
	beq	bndene			@	if so,  jump to look for var in remaining frames
	car	rva, sv3
	pntrp	rva
.ifdef top_level_btree
	bne	bnden0
.else
	bne	corerr
.endif
bndenl:	@ env frame is list (ordered)
	snoc	sv1, sv3, sv3		@ sv1 <- 1st binding
	car	rva, sv1		@ rva <- bkey , key of 1st binding
	cmp	rvb, rva
	bmi	bndene
	it	eq
	cdreq	sv5, sv1
	beq	bndext
	nullp	sv3			@ is binding-list null?
	bne	bndenl			@	if not, jump to continue searching
	nullp	sv5			@ is environment empty?
	bne	bndenr			@	if not, jump to look for var in remaining frames
.ifdef top_level_btree
	b	bnden1
bnden0:	@ env frame is btree (ordered)
	vcrfi	sv1, sv3, 0		@ sv1 <- 1st binding
	car	rva, sv1		@ rva <- bkey , key of 1st binding
	cmp	rvb, rva
	it	eq
	cdreq	sv5, sv1
	beq	bndext
	it	mi
	vcrfimi	sv3, sv3, 1
	it	hi
	vcrfihi	sv3, sv3, 2
	nullp	sv3			@ is binding-list null?
	bne	bnden0			@	if not, jump to continue searching
	nullp	sv5			@ is environment empty?
	bne	bndenr			@	if not, jump to look for var in remaining frames
.endif	
bnden1:	@ get binding from built-in environment
	tst	rvb, #0xFF000000
	itT	ne
	setne	sv1, #null
	setne	sv5, #null
	bne	bndext
	and	rva, rvb, #0x7f00	@ rva <- offset in built-in env, shifted
	nullp	sv5
	itE	eq
	vcrfieq	sv1, glv, 13		@ sv1 <- built-in env vector
	setne	sv1, sv5	
.ifndef	cortex
	ldr	sv1, [sv1, rva, lsr #6]	@ sv1 <- built-in sub-env vector
	bic	rva, rvb, #0x6000
	ldr	sv1, [sv1, rva, lsr #13] @ sv1 <- symbol's printed representation (address)
.else
	lsr	rva, rva, #6		@ rva <- offset in built-in env
	ldr	sv1, [sv1, rva]		@ sv1 <- built-in sub-env vector
	bic	rva, rvb, #0x6000
	lsr	rva, rva, #13		@ rvb <- offset in built-in sub-env
	ldr	sv1, [sv1, rva]		@ sv1 <- symbol's binding value (value or address)
.endif
	set	sv5, sv1
bndext:	@ exit
	set	sv3, sv1		@ sv3 <- binding or null if user-var, value if primitive-var
	set	sv1, rvb		@ sv1 <- var-id, restored
	nullp	rvc			@ exiting from bndchk?
	it	eq
	seteq	pc,  lnk		@	if so,  return sv3 <- bndng | nul | val, sv5 <- val | nul
	nullp	sv3			@ binding found?
	beq	corerr			@	if not, report error
	set	sv1, sv5		@ sv1 <- val
	set	rva, #null		@ rva <- '(), for return to eval (evalsv1 macro)
	set	pc,  rvc

.balign	4

s_dfv:	SYMSIZE	4
	.ascii	"_dfv"
	.balign 4
	
_dfnsb:	@
	@ on entry:	sv1 <- var or (var . val)
	@ on exit:	sv2 <- binding for var
	@ preserves:	sv4
	SYNTAX	1			@ primitive syntax, one input arg
_func_
vrnsrt:	@ [internal entry]
	@ on entry:	sv1 <- var
	@ on exit:	sv2 <- binding for var
	@ preserves:	sv4
	car	sv3, env		@ sv1 <- frame1 of env
	set	sv5, sv1		@ sv5 <- var
	nullp	sv3			@ frame1 exhausted?
	beq	defva6			@	if so,  jump to add new binding
	pntrp	sv1
	itE	eq
	careq	rvb, sv1
	setne	rvb, sv1
	@ check type of environment frame
	car	rva, sv3
	pntrp	rva
.ifdef top_level_btree
	bne	defva1
.else
	bne	corerr
.endif
defva5:	@ env frame is list (ordered)
	@ search for a binding for var in frame1
	car	sv2, sv3		@ sv2 <- 1st binding
	car	rva, sv2		@ rva <- var-id from binding
	cmp	rvb, rva		@ var found?
	it	eq
	seteq	pc,  cnt		@	if so,  return
	bmi	defva6
	set	sv5, sv3		@ sv5 <- previous rest-of-frame1
	cdr	sv3, sv3		@ sv1 <- new rest of frame1
	nullp	sv3			@ frame1 exhausted?
	bne	defva5			@	if not, jump to keep scanning frame1
defva6:	@ build new binding and insert into frame1
	pntrp	sv1
	it	eq
	seteq	sv2, sv1
	beq	defva7
	list	sv2, sv1		@ sv1 <- (var)
defva7:	@ continue
	cons	sv3, sv2, sv3		@ sv3 <- ((var) . rest-of-frame1)
	eq	sv5, sv1
	itE	eq
	setcareq env, sv3		@	if so,  update frame1 in env
	setcdrne sv5, sv3		@	if not, connect start-of frame1 to updated rest-of frame1
	set	pc,  cnt
	
.ifdef top_level_btree
	
.balign	4

defva1:	@ end frame is btree (ordered)
	@ search for a binding for var in frame1
	vcrfi	sv2, sv3, 0
	car	sv5, sv2		@ sv5 <- var-id from binding
	cmp	rvb, sv5
	it	eq
	seteq	pc,  cnt
	set	sv2, sv3		@ sv2 <- binding (will be parent)
	it	mi
	vcrfimi	sv3, sv2, 1		@ sv1 <- left  child binding in frame
	it	hi
	vcrfihi	sv3, sv2, 2		@ sv1 <- right child binding in frame
	nullp	sv3			@ frame1 exhausted?
	bne	defva1
	pntrp	sv1
	beq	defva3
	list	sv1, sv1
defva3:	@ build new binding and insert into frame1
	adr	lnk, defval		@ lnk <- transaction return address + 4
defval:	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- adjusted heap top
	cmp	rva, fre		@ are three 8-byte cons-cells available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvb, #vector_tag	@ rvb <- address of allocated memory, saved
	orr	rvb, rvb, #0x300
	stmia	rva!, {rvb, sv1}	@ store vector-tag, (var . <val>) in free cell
	set	rvb, #null		@ rvc <- '()
	stmia	rva!, {rvb, sv3}	@ store () () in free cell
	sub	sv3, rva, #16		@ sv1 <- address of save cell,	[*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer,	[*restart critical instruction*]
	car	sv1, sv1
	cmp	sv1, sv5
	it	mi
	vcstimi	sv2, 1, sv3
	it	hi
	vcstihi	sv2, 2, sv3
	vcrfi	sv2, sv3, 0		@ sv2 <- binding
	@ call the btree balancing function 
	save3	sv2, sv4, cnt
	car	sv1, env
	call	_bal
	setcar	env, sv1
	restor3	sv2, sv4, cnt
	set	pc,  cnt

.balign	4

sbalan:	SYMSIZE	4
	.ascii	"_bal"
	.balign 4

balanc:	@ (_bal btree)
	@ balance an ordered binary tree
	PFUNC	1			@ primitive function, one input arg
	@ on entry:	sv1 <- ordered-btree
	@ on exit:	sv1 <- balanced copy of ordered-btree
	@ modifies:	rva-rvc, sv1-sv5
_bal:	@ [internal entry]
	set	sv3, sv1		@ sv3 <- source-frame (btree)
	set	sv2, #null
	set	sv4, #null
	set	sv5, #null
	adr	lnk, balanr		@ lnk <- transaction return address + 4
balanr:	eor	fre, fre, #0x03		@ fre <- ...bbb01			(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8		@ rva <- adjusted heap top
	cmp	rva, fre		@ are three 8-byte cons-cells available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvb, #vector_tag	@ rvb <- address of allocated memory, saved
	orr	rvb, rvb, #0x300
	stmia	rva!, {rvb,sv2,sv4,sv5}	@ store vector-tag, var, () in free cell
	sub	sv1, rva, #16		@ sv1 <- address of save cell,	[*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer,	[*restart critical instruction*]
	save2	sv4, sv1	@ dts <- (end-indicator root-of-frame-flat-copy ...)
	set	sv5, #i0	@ sv5 <- 0 = number of bindings in frame (scheme int)
	@ copy-flatten btree
bala_0:	nullp	sv3
	beq	bala_4
bala_1:	vcrfi	sv4, sv3, 1	@ sv4 <- source-frame.left-child
	nullp	sv4
	beq	bala_2
	save	sv3		@ dts <- (source-frame ...)
	set	sv3, sv4	@ sv3 <- source-frame = source-frame.left-child
	b	bala_1
bala_4:	@
	restor	sv3
	nullp	sv3
	beq	bala_d
	set	sv4, #null
bala_2:	add	sv5, sv5, #4
	vcrfi	sv2, sv3, 0	@ sv2 <- source-frame.binding
	adr	lnk, bala_3	@ lnk <- transaction return address + 4
bala_3:	eor	fre, fre, #0x03	@ fre <- ...bbb01				(reservation level 1)
	vcrfi	rva, glv, 1	@ rva <- heaptop -- from global vector (*** NEW ***)
	sub	rva, rva, #8	@ rva <- adjusted heap top
	cmp	rva, fre	@ are three 8-byte cons-cells available?
	bls	algc16		@	if not,  jump to perform gc
	bic	rva, fre, #0x03	@ rva <- address of allocated memory
	set	rvb, #vector_tag @ rvb <- address of allocated memory, saved
	orr	rvb, rvb, #0x300
	set	rvc, #null
	stmia	rva!, {rvb, sv2, sv4, rvc}	@ store vector-tag, var, () in free cell
	sub	sv2, rva, #16	@ sv1 <- address of save cell,	[*commit save destination*]
	orr	fre, rva, #0x02	@ de-reserve free-pointer,	[*restart critical instruction*]
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

.endif	@ 

@---------------------------------------------------------------------------------------------------------
@
@  error handling
@
@---------------------------------------------------------------------------------------------------------

.balign	4

scatch:	SYMSIZE	6
	.ascii	"_catch"
	.balign 4

catch:	@ (catch error)
	@ on entry:	sv1 <- error
	@ on exit:	never exits
	@ preserves:	none
	PFUNC	0			@ primitive function, listed input args
	vcrfi	env, glv, 7
	set	sv2, sv1
	set	sv5, #null
	vcsti	glv, 14, sv5
	vcsti	glv, 15, sv5
	b	errore

.balign 4
	
sthrow:	SYMSIZE	5
	.ascii	"throw"
	.balign 4

throw:	@ (throw func-name-string invalid-arg)
	PFUNC	2			@ primitive function, two input args
	@ on entry:	sv1 <- function name string
	@ on entry:	sv2 <- arg that caused error
	set	sv4, sv1
	set	sv1, sv2
_func_
error4:	@ [internal entry]
	@ on entry:	sv1 <- arg that caused error
	@ on entry:	sv4 <- function name string
	adr	dts, stkbtm		@ dts <- (...)
	ldr	sv3, =sthrow		@ sv3 <- "error:"
	set	sv5, #null
	llcons	sv1, sv4, sv3, sv1, sv5	@ sv1 <- (funcnamestring "error:" invalid-arg/type)
	ldr	sv2, =quote_var
	lcons	sv2, sv2, sv1, sv5
errore:	@ [intenal entry 2] (eg. from catch)
	ldr	sv1, =catch_var
	lcons	sv1, sv1, sv2, sv5
	@
	@ throw could also send dts in arg list to _catch so _catch could give a trace of what
	@ went wrong and/or attempt restart? (would require more than just stack though
	@ -- more like the full context	stored during interrupt - that is feasible indeed but
	@    not done here)
	@
	b	eval

.balign 4

s_err:	SYMSIZE	4
	.ascii	"_err"
	.balign 4

_err:	@ (throw func-name-string invalid-arg)
	PFUNC	1			@ primitive function, one input arg
	@ on entry:	sv1 <- function name string	
_func_
corerr:	@ generic error (eg. bndenv, mkfrm)
	adr	sv4, core_		@ sv1 <- address of "core" (sv2 is the wrong type)
	b	error4

.balign 4

core_:	SYMSIZE	4
	.ascii	"core"
	.balign 4

stkbtm:	.word	null
	.word	stkbtm

@---------------------------------------------------------------------------------------------------------
@ I.D.4. version
@---------------------------------------------------------------------------------------------------------

.balign 4

sversi:	SYMSIZE	7
	.ascii	"version"
	.balign 4
	
versn_:	SYMSIZE	3
	.ascii	"050"
	.balign 4

pGLV:	SYMSIZE	4
	.ascii	"_GLV"
	.balign 4

_GLV:	@ (_GLV)
	@ return the built-in env
	@ or sets built-in env to input arg
	PFUNC	0			@ primitivr function, no input arg
	set	sv1, glv
	set	pc,  cnt

@---------------------------------------------------------------------------------------------------------
@
@  II.I. SCHEME FUNCTIONS AND MACROS:	ADDENDUM -- Level 2
@  II.I.1. definitions:				defined?
@  II.I.2. file system:				unlock
@  II.I.3. pack/unpack:				packed?, unpack, pack
@
@---------------------------------------------------------------------------------------------------------

@---------------------------------------------------------------------------------------------------------
@  II.I.3. pack/unpack:				packed-data-length, packed?, unpack, pack
@---------------------------------------------------------------------------------------------------------

.balign	4
	
sadrof:	SYMSIZE	10
	.ascii	"address-of"
	.balign 4

padrof:	@ (address-of obj ofst)
	@ on entry:	sv1 <- obj
	@ on entry:	sv2 <- ofst
	PFUNC	2			@ primitive function, two input args
	set	rvb, #8
	bl	zmaloc
	pntrp	sv1
	itEE	eq
	seteq	rvc, sv1
	lsrne	rvc, sv1, #2
	lslne	rvc, rvc, #4
	add	rvc, rvc, sv2, lsr #2
	str	rvc, [rva, #4]
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- address of string (symbol), [*commit string destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  cnt
	
.balign	4

spkdts:	SYMSIZE	16
	.ascii	"packed-data-set!"
	.balign 4

pkdtst:	@ (packed-data-set! bv pos val)
	@ on entry:	sv1 <- packed-object
	@ on entry:	sv2 <- position from end of data portion of packed-object
	@ on entry:	sv3 <- item to store at end of packed object
	PFUNC	3			@ primitive function, three input args
	ldr	rvb, [sv1]		@ sv1 <- bytevector length (scheme int)
	lsr	rvb, rvb, #8		@ rvb <- number of bytes in packed object (raw int)
	bl	pkdtsz			@ rvb <- number of data bytes in packed object (raw int)
	bic	rva, sv2, #0x03
	add	rvb, rvb, rva
	add	rvb, rvb, #4
	pntrp	sv3
	itE	eq
	ldreq	rva, [sv3, #4]
	asrne	rva, sv3, #2
	str	rva, [sv1, rvb]
	b	npofxt

.balign	4

_func_
pkdtsz:	@ return the size of code in a packed object
	@ on entry:	rvb <- bytevector length
	@ on exit:	rvb <- size of code in packed bytevector
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
	
.balign	4
	
_func_
pkisof:	@ raise ne flag if item from code vector is an offset
	@ on entry:	sv3 <- offset in source
	@ on entry:	sv4 <- source packed byetvector
	@ on entry:	sv5 <- offset to bitfield end in source packed bytevector
	@ modifies:	rva, rvb
	lsr	rvb, sv3, #4
	and	rvb, rvb, #0x07
	set	rva, #1
	lsl	rvb, rva, rvb		@ rvb <- offset determination mask
	int2raw	rva, sv5
	sub	rva, rva, sv3, lsr #7	
	ldrb	rva, [sv4, rva]
	tst	rva, rvb
	set	pc,  lnk

.balign	4

sunpak:	SYMSIZE	6
	.ascii	"unpack"
	.balign 4

punpak:	@ (unpack packed-object destination)
	@ on entry:	sv1 <- packed-object
	@ on entry:	sv2 <- null for heap, positive for above heap, negative for flash
	PFUNC	2			@ primitive function, two input args
unpack:	@ [internal entry]
	vu8len	sv5, sv1		@ sv5 <- number of bytes, as scheme int
	cmp	sv5, #0x15		@ is number of bytes less than 5? (i.e. object is an immediate)
	itT	mi
	vcrfimi sv1, sv1, 0		@	if so,  sv1 <- item stored in object
	setmi	pc,  cnt		@	if so,  return sv1
	@ get number of data bytes in packed object (excluding bitfield)
	int2raw	rvb, sv5		@ rvb <- number of bytes in packed object (raw int)
	bl	pkdtsz			@ rvb <- number of data bytes in packed object (raw int)
	raw2int	sv3, rvb		@ sv3 <- number of data bytes in packed object (scheme int)
	add	sv5, sv5, #0x0c		@ sv5 <- offset to bitfield in packed object (scheme int)
	@ dispatch on destination
	nullp	sv2			@ unpacking to heap?
	bne	unpacf			@	if not, jump to test for above-heap or flash
	@ unpacking to heap, allocate memory for destination
	bl	zmaloc			@ rva <- addr of obj (vu8-tagged), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv4, rva, rvb		@ sv4 <- address of dest (bytevector), [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	b	unpac0			@ jump to unpack
unpacf:	@ unpacking to above-heap RAM or to flash
.ifdef	LIB_TOP_PAGE
	tst	sv2, #0x80000000	@ unpacking to flash?
	bne	unpafl			@	if so,  jump to that case
.endif
	@ unpacking to above-heap RAM
	swi	run_no_irq		@ disable interrupts (user mode)
	bl	gc			@ rva <- amount of free memory bytes, perform gc
  .ifndef mark_and_sweep
	vcrfi	rvb, glv, 9		@ rvb <- heaptop0 -- from global vector
	cmp	fre, rvb		@ is upper heap in use?
	it	pl
	blpl	gc			@	if so,  rva <- amount of free memory bytes, perform gc
	lsl	rva, rva, #1		@ rva <- number of free bytes *2 (free bytes in both heaps)
  .endif
	int2raw	rvb, sv3		@ rvb <- number of bytes of data
  .ifndef cortex_a8
	add	rvb, rvb, #0x1f		@ rvb <- number of bytes + 31 (15 for alignment, 16 as spacer)
	bic	rvb, rvb, #0x0f		@ rvb <- number of bytes + spacer (16-byte aligned)
  .else
	add	rvb, rvb, #0x4f		@ rvb <- number of bytes + 79 (63 for alignment, 16 as spacer)
	bic	rvb, rvb, #0x3f		@ rvb <- number of bytes + spacer (64-byte aligned
	bic	rva, rva, #0x3E		@ rva <- adjusted for 64-byte L2 cache line length
  .endif
	cmp	rvb, rva		@ is enough room available?
	it	pl
	setpl	sv4, #f			@	if not, sv4 <- #f
	bpl	unpaxt			@	if not, jump to exit with false
  .ifndef mark_and_sweep
	@ update heaptop0 and heaptop1
	vcrfi	rva, glv, 10		@ rva <- heaptop1 -- from global vector
	sub	rva, rva, rvb		@ rva <- heaptop1 minus size of code
    .ifdef cortex_a8
	bic	rva, rva, #0x3E		@ rva <- adjusted for 64-byte L2 cache line
    .endif
	vcsti	glv, 10, rva		@ store it back in global vector as heaptop1
	vcrfi	rvc, glv, 9		@ rvc <- heaptop0 -- from global vector
	sub	rvc, rvc, rvb, lsr #1	@ rvc <- heaptop0 minus size of code / 2
    .ifdef cortex_a8
	bic	rvc, rvc, #0x1E		@ rvc <- adjusted for 64-byte L2 cache line
    .endif
	vcsti	glv, 9, rvc		@ store heaptop0 back in global vector
	vcsti	glv, 1, rvc		@ store it back in global vector as heaptop
  .else
	@ update heaptop
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, rvb		@ rva <- heaptop minus size of code
    .ifdef cortex_a8
	bic	rva, rva, #0x3E		@ rva <- adjusted for 64-byte L2 cache line
    .endif
	vcsti	glv, 1, rva		@ store it back in global vector as heaptop
  .endif
	add	sv4, rva, #15		@ sv4 <- address of object
	swi	run_normal		@ enable interrupts (user mode)
unpac0:	@ set stack for unpacking
	save2	sv1, sv4		@ dts <- (source-bv dest-address ...)
	sub	sv1, sv3, #16		@ sv1 <- offset in target
	b	unpac1			@ jump to unpack
.ifdef	LIB_TOP_PAGE
unpafl:	@ unpacking to flash
	set	rva, #F_PAGE_SIZE	@ rvb <- flash page size (to align num bytes needed)
	sub	rva, rva, #1		@ rvb <- flash page size - 1 (to align)
	add	rvb, rvb, rva		@ rva <- number of bytes needed + alignment value
	bic	rva, rvb, rva		@ rva <- number of bytes needed aligned to flash page size
	raw2int	sv4, rva		@ sv4 <- number of bytes needed, saved against flok
	bl	flok			@ acquire file system lock
	vcrfi	rva, glv, 12		@ rva <- flash lib start page from glv
	nullp	rva			@ no flash lib yet?
	it	eq
	ldreq	rva, =LIB_TOP_PAGE	@	if so,  rva <- adress of top of flash lib
	sub	rvc, rva, sv4, lsr #2	@ rvc <- possible new flash lib start
	ldr	rvb, =LIB_BOTTOM_PAGE	@ rvb <- flash lib bottom page
	cmp	rvc, rvb		@ is enough free flash space available?
	bmi	unpfer			@	if not, jump to report error
  .ifdef SHARED_LIB_FILE
	vcrfi	sv2, glv, 11		@ sv2 <- file flash end page (crunch start, pseudo scheme int)
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
	orreq	rva, rva, #i0		@	if so,  rva <- new crunch space address (pseudo scheme int)
	vcstieq	glv, 11, rva		@	if so,  set new file flash end page (crunch start) into glv
	beq	unpaf1			@	if so,  jump to keep going
	@ check if sufficient flash (erased files) can be reclaimed, if so, do it and restart unpack
	vcrfi	rvb, glv, 11		@ rvb <- current crunch space address (pseudo scheme int)
	bic	rvb, rvb, #i0		@ rvb <- current crunch space address
	sub	rvc, rvb, rva		@ rvc <- space needed by reclamation process	
	ldr	sv2, =F_START_PAGE	@ sv2 <- address of flash start page for files
unpag0:	@ add-up reclaimable space up to needed amount or end of file flash
	cmp	sv2, rvb		@ is page >= file end page?
	bpl	unpfer			@	if so,  jump to report error (not enough space to reclaim)
	tbrfi	rva, sv2, 0		@ rva <- potential file ID for page
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of next FLASH page to check
	and	rva, rva, #0xff		@ rva <- lower byte of 1st word
	eq	rva, #0xfd		@ is this a valid file (not deleted)?
	beq	unpag0			@	if so,  jump to continue scanning file flash
	subs	rvc, rvc, #F_PAGE_SIZE	@ rvc <- remaining space required if page is crunchd, is it enough?
	bne	unpag0			@	if not, jump to continue scanning file flash
	@ sufficient space can be reclaimed, perform crunch
	save	cnt
	call	ffcln
	restor	cnt
	@ restart the unpack process
	set	sv2, #5
	ngint	sv2, sv2
	b	unpack
	
unpaf1:	@ enough flash space is available, keep going
	set	rvc, sv2		@ rvc <- new flash lib start, restored
  .endif @ SHARED_LIB_FILE
	set	sv2, rvc		@ sv2 <- new flash lib start page (write target)
	bl	mkfdsc			@ sv4 <- blank output file descriptor (with buffer and 5 items)
	save3	sv1, sv2, sv4		@ dts <- (source-bv lib-code-start-in-flash file-descr ...)
	vcrfi	sv4, glv, 12		@ sv4 <- old flash-lib start address from glv
	nullp	sv4
	it	eq
	ldreq	sv4, =LIB_TOP_PAGE
	vcsti	glv, 12, sv2		@ set new flash-lib start address into glv
	sub	sv2, sv4, #F_PAGE_SIZE	@ sv2 <- last flash page in target
	set	rvb, #F_PAGE_SIZE
	sub	rvb, rvb, #1
	lsl	rvb, rvb, #2
	orr	rvb, rvb, #i0
	and	sv1, sv3, rvb		@ sv1 <- offset of last word (realtive to F_PAGE_SIZE)
	eq	sv1, #i0		@ unpacking an exact multiple of F_PAGE_SIZE?
	it	eq
	addeq	sv1, rvb, #4		@	if so,  sv1 <- adjusted last word offset
	
.endif	@ LIB_TOP_PAGE
unpac1:	@ unpack the data to RAM or flash
	car	sv4, dts		@ sv4 <- source bytevector
.ifndef	cortex
	ldr	rvc, [sv4, sv3, lsr #2]	@ rvc <- current word of code from bytevector
.else
	lsr	rvc, sv3, #2
	ldr	rvc, [sv4, rvc]		@ rvc <- current word of code from bytevector
.endif
	sub	sv3, sv3, #16
	bl	pkisof			@ is item from source bv an offset (raise ne flag if so)?
	cadr	sv4, dts		@ sv4 <- lib-code-start-in-flash	
	it	ne
	addne	rvc, rvc, sv4		@	if so,  rvc <- target address of offset
.ifdef	LIB_TOP_PAGE
	eq	sv1, sv3		@ unpacking to RAM (heap or non-heap)?
	it	ne
	caddrne	sv4, dts
	it	ne
	vcrfine	sv4, sv4, 3		@	if not, sv4 <- target buffer for writing to flash
.endif
.ifndef	cortex
	str	rvc, [sv4, sv1, lsr #2]	@ store code word in target buffer for flash write
.else
	lsr	rvb, sv1, #2
	str	rvc, [sv4, rvb]		@ store item in destination
.endif
.ifdef	LIB_TOP_PAGE
	eq	sv1, sv3		@ unpacking to RAM (heap or non-heap)?
	beq	unpac2			@	if not, perform flash write if needed
	eq	sv1, #0x11
	bne	unpac2
	caddr	sv4, dts
	bl	libwrt
	sub	sv2, sv2, #F_PAGE_SIZE
	set	rvc, #F_PAGE_SIZE
	add	rvc, rvc, #4
	lsl	rvc, rvc, #2
	orr	sv1, rvc, #i0
.endif
unpac2:	@ check if we're done unpacking
	eq	sv3, #i0		@ done unpacking?
	it	ne
	subne	sv1, sv1, #16		@	if not, sv1 <- offset in which to store next code word
	bne	unpac1			@	if not, jump back to continue unpacking
	@ finish up
	restor2	sv4, sv4
.ifdef	LIB_TOP_PAGE
	eq	sv1, sv3
	itT	ne
	cdrne	dts, dts
	blne	funlok
.endif
unpaxt:	set	sv1, sv4
	set	pc,  cnt		@ return
	
unpfer:	@ error: not enough free space
	set	sv4, #f
	bl	funlok
	b	unpaxt


.ifndef	exclude_pack
	
.balign	4

spack:	SYMSIZE	4
	.ascii	"pack"
	.balign 4

ppack:	@ (pack object)	 -- used by i2c
	@ on entry:	sv1 <- (object)
	@ sv3 <- target offset,  sv4 <- source list,  sv5 <- relocation list
	PFUNC	0			@ primitive function, listed input args
pack:	@ [internal entry]
	save2	sv1, sv1		@ dts <- ((object) (object) ...)
	set	sv3, #i0		@ sv3 <- 0, size of thing to move, as scheme_int
	set	sv5, #null		@ sv5 <- empty relocation list
pack_1:	restor	sv4			@ sv4 <- (addr1 addr2 ...) = src obj lst, dts <- ((object) ...)
	nullp	sv4			@ is source object list null?
	beq	pack_8			@	if so,  jump to build position-independent object
	cdr	sv1, sv4		@ sv1 <- (addr2 ...) = rest of source object list
	save	sv1			@ dts <- ((addr2 ...) (object) ...)
	car	sv4, sv4		@ sv4 <- addr1 = 1st item in source object list
pack_2:	@
	bl	pkckfr			@ sv2 <- '() if item needs relocation and isn't in reloc list
	nullp	sv2			@ is relocation needed?
	bne	pack_1			@	if not, jump to process next item
	@ add relocation frame to relocation list
	set	sv1, sv4		@ sv1 <- source_address
	bcons	sv5, sv1, sv3, sv5	@ sv5 <- updtd reloc list ((src_addrss . trgt_offst) reloc-lst)
	@ see if sv4 is sized object
	sizdp	sv4			@ is sv4 a sized object?
	beq	pack_5			@	if so,  jump to do default
	ratcpx	sv4			@ is sv4 a rat/cpx?
	itT	eq
	addeq	sv3, sv3, #32		@	if so,  sv3 <- updtd size of obj, in bytes (scheme int)
	beq	pack_1			@	if so,  jump to process next item
	@ sv4 is a list, prepare to process car and cdr
	car	sv1, sv4		@ sv1 <- (car object)
	restor	sv2			@ sv2 <- (addr2 ...),		dts <- ((object) ...)
	bcons	dts, sv1, sv2, dts	@ dts <- (((car obj) addr2 ...) ...)
	add	sv3, sv3, #32		@ sv3 <- updtd num bytes to cpy & dest offst (+8, scheme int)
	cdr	sv4, sv4		@ sv4 <- (cdr lst)
	b	pack_2			@ jump to process cdr
	
pack_5:	@ sv4 is a sized object
	car	rvc, sv4		@ sv1 <- (car object)
	lsr	rva, rvc, #8		@ rva <- size of item, in bytes or words (raw int)
	tst	rvc, #0x30		@ is object gc-eable
	it	eq
	lsleq	rva, rva, #2		@	if so,  rva <- number of bytes in object
	add	rvb, rva, #11		@ rvb <- num of bytes to allocate + header + 7-bytes to align
	bic	rvb, rvb, #7		@ rvb <- num of bytes to allocate, with header, 8-byte aligned
	add	sv3, sv3, rvb, LSL #2	@ sv3 <- updated size of object, in bytes, as scheme int
	bne	pack_1			@	if not,  return (non gc-eable)
	@ item is gc-eable sized object, scan for pointers and store them in source list for relocation
	raw2int	sv1, rva
pack_7:	@ sub	sv1, sv1, #16
	cmp	sv1, #0x11		@ done?
	bmi	pack_1			@	if so,  jump to process remaining objects
	wrdref	rva, sv4, sv1		@ rva <- array item
	pntrp	rva			@ is item a pointer?
	bne	pack_6			@	if not, jump to process next array item
	restor	sv2			@ sv2 <- (addr2 ...),  dts <- ((object) ...)
	set	rvb, #16		@ rvb <- 16, number of bytes to allocate
	bl	zmaloc			@ rva <- allocated block
	add	rva, rva, #8		@ rva <- address of new cons
	stmdb	rva, {rva, dts}		@ fre - 1 <- ( ... dts )
	wrdref	rva, sv4, sv1		@ rva <- array item (pointer == address)
	str	rva, [fre, #7]		@ fre - 1 <- ( ( address ...) dts )
	str	sv2, [fre, #11]		@ fre - 1 <- ( ( address addr2 ... ) dts )
	add	rva, fre, #15		@ rva <- address of next free cell
	sub	dts, rva, rvb		@ dts <- address of new dts, [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
pack_6:	sub	sv1, sv1, #16		@ sv1 <- offset of next vector item
	b	pack_7			@ jump to keep processing vector items
	
_func_	
pkckfr:	@ see if item in sv4 is relocated/needs relocation
	@ on entry:	sv4 <- item to check
	@ on exit:	sv2 <- #i0 == no need to relocate / not relocated,
	@ on exit:	sv2 <- '() == item not on reloc list (needs relocation)
	@ on exit:	sv2 <- other == frame from reloc list
	@ modifies:	sv1, sv2, rva
	set	sv2, #i0		@ sv2 <- indicator value (scheme 0)	
	pntrp	sv4			@ is item (sv4) a pointer?
	it	ne
	setne	pc,  lnk		@	if not, return
	ldr	rva, =heapbottom	@ rva <- heapbottom
	cmp	sv4, rva		@ is sv4 > heapbottom ?
	itT	pl
  .ifndef mark_and_sweep
	vcrfipl	rva, glv, 10		@	if so,  rva <- heaptop1, from global vector
  .else
	vcrfipl	rva, glv, 1		@	if so,  rva <- heaptop -- from global vector (*** NEW ***)
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
	restor	sv4			@ sv4 <- (object) being packed (eg. if not a list), dts <- (.)
	int2raw	rvb, sv3		@ rvb <- number of data bytes in target object (raw int)
	add	rva, rvb, #31
	add	rvb, rvb, rva, lsr #5
	add	rvb, rvb, #16		@ rvb <- number of bytes to allocate
	bl	zmaloc			@ rva <- allocated block of rvb bytes
	str	dts, [fre, #3]		@ fre - 1  <- (... dts )
	int2raw	rva, sv3		@ rva <- number of data bytes in target object (raw int)
	nullp	sv5			@ was nothing relocated?
	itEE	eq
	addeq	rva, rva, #4		@	if so,  rva <- adjstd num of bytes allocd (for 1 word)
	addne	rvc, rva, #31
	addne	rva, rva, rvc, lsr #5
	lsl	rva, rva, #8		@ rva <- number of bytes, shifted in place
	orr	rva, rva, #bytevector_tag @ rva <- full (sized) tag for pic
	str	rva, [fre, #7]		@ store tag in target
	add	rva, fre, #7		@ rva <- address of object
	str	rva, [fre, #-1]		@ fre - 1 <- (target_relocated_object ...) = new dts
	bic	rva, fre, #0x01		@ rva <- target address
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	dts, rva, rvb		@ dts <- address of new dts, [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	car	sv1, dts		@ sv1 <- empty packed object
	@ clear out object contents (especially bitfield)
	car	rva, sv1
	lsr	rva, rva, #8
	add	rva, rva, #7
	bic	rva, rva, #3
	set	rvb, #0
pack0l:	subs	rva, rva, #4
	it	ne
	strne	rvb, [sv1, rva]
	bne	pack0l
	@ deal with immediates
	nullp	sv5			@ was nothing relocated?
	itT	eq
	careq	sv4, sv4		@	if so,  sv4 <- object to package
	vcstieq sv1, 0, sv4		@	if so,  store object in pic
	@ sv3 will scan over the relocation list
	set	sv3, sv5		@ sv3 <- relocation list
pack_9:	nullp	sv3			@ are we done with relocation list?
	beq	packxt			@	if so,  jump to finish up
	@ relocate the car
	caar	sv4, sv3		@ sv4 <- source
	car	rvc, sv4		@ rvc <- source car
	and	rva, rvc, #0xFF		@ rva <- source tag, masked for vector/string... identification
	eq	rva, #vector_tag	@ is it a gceable sized item?
	it	eq
	ldreq	pc, =packsz		@	if so,  copy-relocate that sized item
	and	rva, rva, #0xCF		@ rva <- source tag, masked for vector/string... identification
	eq	rva, #0x4F		@ is it a non-gceable sized item?
	beq	packng			@	if so,  copy-relocate that sized item
	and	rva, rva, #0x07		@ rva <- source tag, masked for rat/cpx identification
	eq	rva, #0x03		@ is it a rat/cpx item?
	itT	eq
	seteq	rvb, #4			@	if so,  rvb <- 4, raw offset of object end
	beq	packrc			@	if so,  copy-relocate that rat/cpx item
	car	sv4, sv4		@ sv4 <- source car
	bl	pkckfr			@ sv2 <- target offset frame
	set	rvb, #0
	bl	packlp			@ sv1 <- address of target, sv4 <- object or scheme offset
	@ relocate the cdr
	cdaar	sv4, sv3		@ sv4 <- source cdr
	bl	pkckfr			@ sv2 <- target offset frame
	set	rvb, #4
	bl	packlp			@ sv1 <- address of target, sv4 <- object or scheme offset
	cdr	sv3, sv3		@ sv3 <- rest of relocation list
	b	pack_9			@ jump to continue processing relocation list
packxt:	restor	sv1			@ sv1 <- packed object,		dts <- (...)
	set	pc,  cnt		@ return

_func_	
packlp:	@ on entry:	sv3 <- rest of reloc list, starting with current item
	@ on entry:	sv4 <- item
	@ on entry:	rvb <- secondary offset in destination (beyond sv2)
	@ on entry:	sv5 <- relocation list
	@ on exit:	sv1 <- address of target object
	@ on exit:	sv2 <- offset in target
	@ on exit:	sv4 <- item (if immediate) or its tagged offset (if pair/sized)
	@ modifies:	sv1, sv2, sv4, rva, rvc
	eq	sv2, #i0		@ is sv4 non-relocated?
	it	ne
	cdarne	sv4, sv2		@	if not, sv4 <- offset to relocated object (scheme int)
	car	sv1, dts		@ sv1 <- address of sized object
	cdar	sv2, sv3		@ sv2 <- target_offset (scheme_int)
	add	sv2, sv2, rvb, LSL #2	@ sv2 <- target offset updated for item position (scheme int)
	bne	packl2
	@ store it and return
	add	sv2, sv2, #16		@ sv2 <- target_offset + 4 bytes (for header), as scheme int
	wrdst	sv4, sv1, sv2		@ store item
	set	pc, lnk			@ return
packl2:	@ add offset in bitfield and target
	lsr	rvc, sv2, #4
	and	rvc, rvc, #0x07
	set	rva, #1
	lsl	rvc, rva, rvc
	car	rva, sv1
	lsr	rva, rva, #8
	add	rva, rva, #3
	sub	rva, rva, sv2, lsr #7
	ldrb	rva, [sv1, rva]
	orr	rvc, rva, rvc
	car	rva, sv1
	lsr	rva, rva, #8
	add	rva, rva, #3
	sub	rva, rva, sv2, lsr #7
	strb	rvc, [sv1, rva]
	add	sv2, sv2, #16		@ sv2 <- target_offset + 4 bytes (for header), as scheme int
	
@	int2raw	rvc, sv4
@	wrdst	rvc, sv1, sv2		@ store item
	int2raw	rva, sv4
	wrdst	rva, sv1, sv2		@ store item
	
	set	pc, lnk			@ return

_func_	
packng:	@ copy non-gceable sized item from source to target
	@ on entry:	sv3 <- rest of reloc list, starting with current item
	@ on entry:	rvc <- object tag of item
	@ on entry:	sv5 <- relocation list
	@ on exit:	sv3 <- rest of reloc list, starting after current item
	@ modifies:	sv1-sv4, rva, rvb
	lsr	rvb, rvc, #8		@ rvb <- number of data bytes in object
	add	rvb, rvb, #3		@ rvb <- number of data bytes in object, + 3
	bic	rvb, rvb, #0x03		@ rvb <- num bytes in obj, aligned = end offset in src (raw)
_func_	
packrc:	@ [internal entry] for rat/cpx
	car	sv1, dts		@ sv1 <- address of target sized object
	snoc	sv2, sv3, sv3		@ sv2 <- (src_address . trgt_offset), sv3 <- rest of reloc list
	snoc	sv4, rvc, sv2		@ sv4 <- src_address,	rvc <- trgt_offset, as scheme_int
	add	sv2, rvc, rvb, LSL #2	@ sv2 <- end offset in target (scheme int)
	add	sv2, sv2, #16		@ sv2 <- end ofst in trgt (scheme int) (incl hdr of non-gc obj)
packn1:	cmp	rvb, #0			@ done copying?
	bmi	pack_9			@	if so,  return
	ldr	rva, [sv4, rvb]		@ rva <- word from source
	wrdst	rva, sv1, sv2		@ store it in target
	sub	rvb, rvb, #4		@ rvb <- remaining number of words to copy
	sub	sv2, sv2, #16		@ sv2 <- updated target offset
	b	packn1			@ jump to keep copying

_func_	
packsz:	@ copy gceable sized item, update sv3, don't change sv5
	@ on entry:	sv3 <- rest of reloc list, starting with current item
	@ on entry:	rvc <- object tag of item
	@ on entry:	sv5 <- relocation list
	@ on exit:	sv3 <- rest of reloc list, starting after current item
	@ modifies:	sv1-sv4, rva, rvb
	lsr	rvb, rvc, #6		@ rvb <- number of items in object (scheme int)
	bic	rvb, rvb, #0x03		@ rvb <- number of data bytes in object (raw int)
packs1:	cmp	rvb, #0			@ done copying data?
	it	mi
	cdrmi	sv3, sv3		@	if so,  sv3 <- rest of reloc list
	bmi	pack_9			@	if so,  return
	caar	sv2, sv3		@ sv2 <- source object address
	ldr	sv4, [sv2, rvb]		@ sv4 <- item from sized object
	bl	pkckfr			@ sv2 <- target offset
	bl	packlp			@ sv1 <- address of target, sv4 <- object or scheme offset
	sub	rvb, rvb, #4		@ rvb <- next source offset
	b	packs1			@ jump to keep copying

.else	@ exclude_pack
	
.balign	4

spack:	SYMSIZE	4
	.ascii	"pack"
	.balign 4

ppack:	@ (pack object)	 -- not implemented (when exclude_pack flag is set)
	@ on entry:	sv1 <- (object)
	@ sv3 <- target offset,  sv4 <- source list,  sv5 <- relocation list
	PFUNC	0			@ primitive function, listed input args
pack:	@ [internal entry]
	b	flsfxt
	
.endif

@---------------------------------------------------------------------------------------------------------
@ I.D.4. library
@---------------------------------------------------------------------------------------------------------

.balign	4

slibra:	SYMSIZE	7
	.ascii	"library"
	.balign 4
	
plibra:	@ (library (lib name) expressions)
	@ on entry:	sv1 <- (lib name)
	@ on entry:	sv2 <- (expressions), eg. ((export ...) (import ...) . body)
	@ on entry:	glv, 14 <- library environment, set by parser
	@ on exit:	sv1 <- result
	SYNTAX	1			@ primitive syntax, one input arg
	vcrfi	sv5, glv, 14		@ sv5 <- library env (from parser)
	nullp	sv5			@ no lib-env?
	beq	corerr			@	if so,  jump to report error
	@ set library index into library's private sub-env
	vcrfi	sv1, sv5, 0		@ sv1 <- library's private sub-env
	veclen	sv3, sv5		@ sv3 <- library env's size (= lib index + 1)
	sub	sv3, sv3, #4		@ sv3 <- library index
	vcsti	sv1, 1, sv3
	vcrfi	sv4, glv, 12		@ sv4 <- list of libraries on MCU
	vcrfi	sv3, glv, 13
	vcsti	glv, 13, sv5		@ set built-in env to lib-env
	save3	sv4, sv3, cnt		@ dts <- (prev-lib prev-bie cnt ...)
	@ evaluate expressions in library, within lib-env
plibr0:	nullp	sv2			@ done with expressions?
	beq	plibr1			@	if so,  jump to continue
	snoc	sv1, sv2, sv2		@ sv1 <- 1st lib-expression, sv2 <- remaining lib-expressions
	vcrfi	env, glv, 13		@ env <- lib-env (couldn't this just be #nul?? or smtng wit _catch)
	save	sv2			@ dts <- (remaining-lib-expr lib-env library cnt ...)
	call	eval			@ sv1 <- result, from evaluating sv1 in default environment
	restor	sv2			@ sv2 <- remaining-lib-expr, dts <- (lib-env library cnt ...)
	b	plibr0			@ jump to continue evaluating lib-expressions
plibr1:	@
	vcrfi	sv1, glv, 13
	restor	sv2			@ sv2 <- list of libraries on MCU,  dts <- (cnt ...)
	restor	sv3
	vcsti	glv, 13, sv3
	cons	sv1, sv1, sv2		@ sv1 <- (library . list-of-libraries) = new list of libraries
	list	sv1, sv1		@ sv1 <- (new list of libraries), for pack
	call	pack			@ sv1 <- new list of libraries, packed (bytevector)
.ifdef	LIB_TOP_PAGE
	set	sv2, #5			@ sv2 <-  1 (scheme int)
	ngint	sv2, sv2		@ sv2 <- -1 (scheme int) = unpack to flash indicator
.else
	set	sv2, #i0		@ sv2 <-  0 (scheme int) = unpack to above-heap indicator
.endif
	call	unpack			@ sv1 <- new list of libraries on MCU, unpacked to flash or RAM
	vcsti	glv, 12, sv1		@ set new list of libraries in global vector
	set	rva, #null		@ rva <- '()
	vcsti	glv, 14, rva		@ clear library env (set by parser) from global vector
	restor	cnt			@ cnt <- cnt,  dts <- (...)
	b	npofxt			@ return with npo

.balign	4

sexpor:	SYMSIZE	6
	.ascii	"export"
	.balign 4

pexpor:	@ (export expr)
	@ on entry:	sv1 <- expr
	@ on entry:	glv, 15 <- possible parse-mode flag
	@ on exit:	sv1 <- result
	SYNTAX	1			@ primitive syntax, one input arg
	set	rva, #null		@ rva <- '()
	vcsti	glv, 15, rva		@ clear the residual parse-mode flag, if any
	set	pc,  cnt		@ return

.balign	4

simpor:	SYMSIZE	6
	.ascii	"import"
	.balign 4

pimpor:	@ (import (lib1) (lib2) ...)
	@ on entry:	sv1 <- ((lib1) (lib2) ...)
	@ on entry:	glv, 14 <- null (normal) or library environment set by parser
	@ on entry:	glv, 15 <- possible parse-mode flag
	@ on exit:	sv1 <- result
	SYNTAX	0			@ primitive syntax, listed input args
	vcrfi	sv5, glv, 14		@ sv5 <- mode indicator / library env (from parser)
	nullp	sv5			@ are we in normal (vs parse or (library ...)) mode?
	it	ne
	setne	pc,  cnt		@	if not, return
	@ reset built-in env if no libraries are specified
	nullp	sv1			@ no libraries specified (i.e. reset built-in env)?
	itT	eq
	ldreq	rvb, =scmenv		@	if so,  rvb <- initial built-in scmenv
	vcstieq	glv, 13, rvb		@	if so,  set initial built-in environment in glv
	beq	npofxt			@	if so,  return
	@ build new built-in env
	set	rva, #null		@ rva <- '() = cumulative import
	vcsti	glv, 15, rva		@ clear parse-mode flag (also means cumul import for zrslb)
	bl	zrslb			@ sv3 <- extended copy of built-in environment
	sav_rc	sv3			@ dts <- (built-in-env cnt ...)
	@ import the libraries
	set	sv2, sv1		@ sv2 <- ((lib1) (lib2) ...)
pimpo0:	@
	snoc	sv1, sv2, sv2		@ sv1 <- (lib1),  sv2 <- ((lib2) ...)
	save	sv2			@ dts <- (((lib2) ...) built-in-env cnt ...)
	car	sv1, sv1		@ sv1 <- library name's symbol ID
	call	symstr			@ sv1 <- library name as string
	bl	lbimp			@ rvb <- , sv5 <- 
	eq	rvb, #1			@ library found?
	it	ne
	cadrne	sv4, dts		@	if so,  sv4 <- built-in environment
	it	ne
	strne	sv5, [sv4, rvb]		@	if so,  store lib's export sub-env in built-in env
	restor	sv2			@ sv2 <- ((lib2) ...),  dts <- (built-in-env cnt ...)
	nullp	sv2			@ more libraries to import?
	bne	pimpo0			@	if so,  jump to import next library
	@ finish up
	restor	sv1
	restor	cnt
	vcsti	glv, 13, sv1
	b	npofxt

_func_
zrslb:	@ construct a built-in env large enough to import libraries into it
	@ on entry:	glv, 15 <- mode flag: null => additive (import), #13 => scmenv only (parslb)
	@ on exit:	sv3 <- new built-in env
	@ modifies:	sv2, sv3, sv4, rva, rvb, rvc
	@ returns via:	lnk
	bic	sv2, lnk, #lnkbit0	@ sv2 <- lnk, saved (and made even if Thumb2)
	ldr	sv4, =scmenv		@ sv4 <- initial built-in env
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
	add	rvb, rvb, #4		@ rvb <- number of bytes to allocate
	bl	zmaloc			@ rva <- address of allocated memory
	lsl	rvc, sv3, #6		@ rvc <- size of new env, shifted
	orr	rvc, rvc, #vector_tag	@ rvc <- tag for new env
	str	rvc, [rva]		@ store tag in allocated area
	@ initialize new built-in env to empty vectors
	lsr	rvc, rvc, #6
	bic	rvc, rvc, #0x03
	ldr	sv4, =empty_vector	@ sv4 <- empty vector
	eq	rvc, #0			@ done initializing new built-in env to empty-vector?
zrslb1:	itT	ne
	strne	sv4, [rva, rvc]		@	if not, store empty-vector in new env
	subsne	rvc, rvc, #4		@	if not, rvc <- new sub-env position, updated
	bne	zrslb1			@	if not, jump to keep initializing
	@ complete the allocation
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv3, rva, rvb		@ sv3 <- address of object (symbol), [*commit vector destination*]
	orr	fre, rva, #0x02		@ fre <- updated and de-reserved, [*restart critical instruction*]
	orr	lnk, sv2, #lnkbit0	@ lnk <- lnk, restored
	@ copy existing built-in env into new built-in env
	vcrfi	sv4, glv, 15		@ sv4 <- null for additive build, #13 for scmenv only
	nullp	sv4			@ copy whole current built-in env (rather than just scmenv)?
	itE	eq
	vcrfieq	sv4, glv, 13		@	if so,  sv4 <- built-in env
	ldrne	sv4, =scmenv		@	if not, sv4 <- scmenv
	ldr	rvc, [sv4]		@ rvc <- built-in env tag
	lsr	rvc, rvc, #6		@ rvc <- size of built-in env to copy (scheme int)
	bics	rvc, rvc, #0x03		@ rvc <- offset of last item to copy from built-in env, is it zero?
zrslb2:	itTTT	ne
	ldrne	sv2, [sv4, rvc]		@	if not, sv2 <- item from source built-in env 
	strne	sv2, [sv3, rvc]		@	if not, store item in new built-in env
	subsne	rvc, rvc, #4		@	if not, rvc <- offset of next item to copy, is it zero?
	bne	zrslb2			@	if not, jump to keep copying
	set	pc,  lnk		@ return with extended built-in-env in sv3
	
_func_
lbimp:	@ find library for import
	@ on entry:	sv1 <- library name (symbol)
	@ on exit:	rvb <- library's export offset in main env (raw int) or 1 if not found
	@ on exit:	sv5 <- library's export sub-environment
	@ modifies:	sv2-sv5, rva-rvc
	set	rvc, lnk		@ rvc <- lnk, saved
	vcrfi	sv5, glv, 12		@ sv5 <- libraries
lbimp0:	nullp	sv5			@ reached end of libraries?
	it	eq
	seteq	rvb, #1			@	if so,  rvb <- 1 (raw int) lib not found indicator
	beq	lbimpx			@	if so,  jump to exit
	snoc	sv4, sv5, sv5		@ sv4 <- first lib,  sv5 <- rest of libs
	vcrfi	sv2, sv4, 0
	vcrfi	sv2, sv2, 0		@ sv2 <- first lib's name (symbol)
	bl	stsyeq			@ is this the lib we're looking for?
	bne	lbimp0			@	if not, jump back to scan remaining libraries
	@ get lib's index in built-in env and lib's export env
	vcrfi	sv2, sv4, 0
	vcrfi	sv2, sv2, 1		@ sv2 <- import-lib-idx
	bic	rvb, sv2, #0x03
	add	rvb, rvb, #4
	ldr	sv5, [sv4, rvb]		@ sv5 <- import-lib's export sub-env	
lbimpx:	@ exit
	set	pc,  rvc

@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

.balign 4

.ifndef	live_SD
	
sfcln:	SYMSIZE	3
	.ascii	"fsc"
	.balign 4

pfcln:	@ (fsc)
	@ file space cleaning
	@ perform actual erasing of deleted flash files and pack them all
	@ at the bottom of file flash space, opening up potential space for
	@ flash libraries on shared-library systems
	PFUNC	0			@ primitive function, no input arg
ffcln:	@ [internal entry]
	@ allocate memory for file descriptor and buffer
	bl	mkfdsc			@ sv4 <- blank output file descriptor (with buffer)
ffcln0:	@ find 1st page with pseudo-erased file (return if none)
	bl	ff1del			@ sv2 <- 1st page with pseudo deleted file, rva <- 0 if none
	eq	rva, #0			@ done cleaning-up?
	beq	npofxt			@	if so,  return
	@ copy sector of 1st pseudo-deleted page to extra flash
	bl	pgsctr			@ rva <- sector of page with pseudo deleted file (address in sv2)
	add	rvc, rva, #1		@ rvc <- sector after that of deleted page
	ldr	rvb, =flashsectors	@ rvb <- address of FLASH sector table
	ldr	rva, [rvb, rva, LSL #2]	@ rva <- address of start page of source sector
	ldr	rvb, [rvb, rvc, LSL #2]	@ rvb <- address of start page of next sector (end page)
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (destination, pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space (destination)
	vcsti	sv4, 2, rva		@ store src start page address in caller-tmp-storage of sv4
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ bring that back into file flash (without deleted files)
	vcrfi	rva, glv, 11		@ rva <- address of crunch space (source start, pseudo scheme int)
	bic	rva, rva, #i0		@ rva <- address of crunch space (source start)
	vcrfi	rvb, sv4, 1		@ rvb <- address of end of extra FLASH target (source end)
	vcrfi	rvc, sv4, 2		@ rvc <- start address of former source = destination for copy
	bl	flshcp			@ perform flash copy (sv4 updated)
	vcrfi	sv2, sv4, 1		@ sv2 <- address after end of copied pages = 1st free flash page
	@ fill-up
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space
ffcln1:	sub	rvc, rvc, #F_PAGE_SIZE	@ rvc <- possible source page address
	cmp	sv2, rvc		@ have we exhausted possible source pages?
	bpl	ffcln0			@	if so,  jump back to search for more deleted pages
	ldr	rva, [rvc]		@ rva <- 1st word of possible source page
	and	rva, rva, #0xff		@ rva <- lower byte of 1st word
	eq	rva, #0xfd		@ is this a valid file source page (not deleted, not free)?
	bne	ffcln1			@	if not, jump to continue fill-up
	@ copy source page to erased destination page
	vcsti	sv4, 4, rvc		@ store destination page (erased) in extendd pseudo-file-descriptor
	bl	fpgcpy			@ copy file data from source page to destination page
	bl	foflup			@ update open-file list (if source page address is on that list)
	@ perform pseudo-deletion of source page
	vcrfi	sv2, sv4, 3		@ sv2 <- buffer, from pseudo-file-descriptor
	set	rva, #i0		@ rva <- 0 (scheme int), used for pseudo-deletion of file
	vcsti	sv2, 0, rva		@ store 0 in buffer (as file ID => pseudo-deleted file)
	vcrfi	sv2, sv4, 4		@ sv2 <- prior source page
	bl	wrtfla			@ overwrite prior source page to ID 0 (pseudo-deletion)
	@ update destination page
	vcrfi	sv2, sv4, 1		@ sv2 <- prior destination page
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- possible new destination page
	ldr	rva, [sv2]		@ rva <- 1st word of possible new destination page
	mvns	rva, rva		@ is this possible new page cleared to write?
	bne	ffcln0			@	if not, jump back to search for more deleted pages
	vcsti	sv4, 1, sv2		@ store update destination in pseudo file descriptor
	b	ffcln1			@ jump to continue fill-up

.endif	@ live_SD	







	