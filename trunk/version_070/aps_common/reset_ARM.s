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

/*------------------------------------------------------------------------------
@
@	ARMv4T, ARMv5TEJ, ARMv7A Common Code (expressed as macros)
@
@-----------------------------------------------------------------------------*/

.macro	simple_random_gen
	/* (RNG seed) */
	/* adapted from:
	Park, S.K. and K.W. Miller, 1988. Random Number Generators: Good ones
	are Hard to Find. Communications of the ACM, 31(10):1192-1201. 
	check: seed = #vu8(1 0 0 0) gives #vu8(17 89 52 62) for 10,000th val 
	         (this is 1043618065 in decimal, #x3e345911 in hex).
	The base equation is:  seed =  a * seed mod m,  a = 7^5, m = 2^31 - 1 */
	PRIMIT	"RNG", pfun, 1
	@ in:	sv1 <- seed stored in a bytevector
	@ out:	sv1 <- updated seed stored in input bytevector
	vcrfi	rvb, sv1, 0		@ rvb <- seed
	set	sv2, (127773<<2)|i0	@ sv2 <- 127773 divisor (scheme int)
	int2raw	rvc, sv2		@ rvc <- 127773 divisor (raw int)
rngdsh:	lsl	rvc, rvc, #1		@ rvc <- divisor, shifted left as needed
	cmp	rvc, rvb		@ is divisor <= dividend?
	bls	rngdsh			@	if so,  jump to shift divisor
	set	rva, 0			@ rva <- 0 (initial quotient, raw int)
rngdcn:	lsr	rvc, rvc, #1		@ rvc <- divisor/2
	lsl	rva, rva, #1		@ rva <- updated quotient
	cmp	rvb, rvc		@ is dividend >= divisor?
	subpl	rvb, rvb, rvc		@	if so,  rvb <- divid-divis = rem
	addpl	rva, rva, #1		@	if so,  rva <- quotient + 1
	eq	rvc, sv2, lsr #2	@ is divisor = original divisor (done)?
	bne	rngdcn			@	if not, jump to keep dividing
	set	rvc, 16807		@ rvc <- a = 7^5
	mul	rvb, rvc, rvb		@ rvb <- a * (seed % q)
	set	rvc, 2836		@ rvc <- r = m % a
	mul	rva, rvc, rva		@ rva <- r * (seed / q)
	subs	rva, rvb, rva		@ rva <- a * (seed % q) - r * (seed / q)
	addls	rva, rva, #2147483647	@	rva <- rva + m    (m = 2^31 - 1)
	vcsti	sv1, 0, rva		@ set result (rva) into sv1
	set	pc,  cnt
.endm

/*------------------------------------------------------------------------------
@  I.B.1. ARMv4T, ARMv5TEJ, ARMv7A
@-----------------------------------------------------------------------------*/


_start:	@ reset startup (address 0x00 normally)

.ifndef	rst_direct_branch

	ldr	pc,  =reset		@ reset
	ldr	pc,  =inserr		@ undefined instruction handler
	ldr	pc,  =swi_hndlr		@ software interrupt
	ldr	pc,  =prferr		@ prefetch abort handler
	ldr	pc,  =daterr		@ data abort handler
	.space 4
  .ifndef irq_direct_branch
        ldr	pc,  [pc, #int_voffset]	@ IRQ:	jump to isr stord in VICVectAddr
  .else
        ldr	pc,  =genisr		@ IRQ:	jump to genisr
  .endif
	subs	pc,  lr, #4		@ FIQ:	return

.else	@ (eg. AT91-SAM9)

	b	reset			@ reset
	b	inserr			@ undefined instruction handler
	b	swi_hndlr		@ software interrupt
	b	prferr			@ prefetch abort handler
	b	daterr			@ data abort handler
	.space 4			@ uplodr may wrt val here lpc21isp,SAMBA
        b	genisr			@ IRQ:	jump to genisr
	b	fiqisr			@ FIQ:	branch to FIQ return
fiqisr:	subs	pc,  lr, #4		@ FIQ return
	
.endif

	SMBL	"inst", snster
	SMBL	"pref", sprfer
	SMBL	"data", sdater


inserr:	@ undefined instruction handler
	set	rvb, 1<<gled_pin
	ldr	sv4, =snster
	set	sv1, lnk
	b	errcmn

prferr:	@ prefetch abort handler
	set	rvb, 1<<rled_pin
	ldr	sv4, =sprfer
	sub	sv1, lnk,  #4
	b	errcmn
	
daterr:	@ data abort handler

.ifdef	enable_MPU

	mrc	p15, 0, rva, c5, c0, 0	@ rva <- content of data fault stat reg
	and	rva, rva, #0x0d		@ rva <- fault masked for permissns chk
	eq	rva, #0x0d		@ was this an access permission fault?
	bne	dater9			@	if not, jump to std process
	@ de-reserve memory in case an int (eg. timer) arose while handling this
	bic	fre, fre, #7		@ rvb <- 8-byte aligned (eg. for ibcons)
	orr	fre, fre, #0x02
	@ find start address of memory allocation sequence
	ldr	rva, =0xE3C00003	@ rva <- bic fre, fre, #3 ARM mach code
	sub	rvb, lnk,  #8		@ rvb <- address of faulted storage inst
cisrc_:	sub	rvb, rvb, #4
	ldr	rvc, [rvb]
	eq	rvc, rva
	bne	cisrc_
	@ set context up to perform gc and return to aborted storage instr.
	add	rvb, rvb, #4		@ rvb <- gc return lnk (adj for gc ret)
	stmdb	sp, {rvb}		@ store gc-return lnk on sp_abt
	ldmdb	sp, {lnk}^		@ set lnk-usr to gc-return lnk
	set	rvc, normal_run_mode	@ rvc <- normal run mode
	msr	spsr_cxsf, rvc		@ set spsr for gc
	set	rvb, 24			@ rvb <- 24 default/max num bytes needed
	ldr	lnk, =adr__gc		@ lnk <- address of gc routine	
	@ return to gc (then faulted data instr via lnk-usr)
	movs	pc,  lnk		@ return

dater9:	@ data abort is not barrier crossing
	@ continue to turn YELLED on
.endif
	set	rvb, 1<<yled_pin
	ldr	sv4, =sdater
	sub	sv1, lnk,  #8
errcmn:	@ [internal entry]
	bl	ledon
	orr	sv1, sv1, #int_tag
	ldr	lnk, =error4
	bic	rvb, fre, #0x03
	orr	fre, rvb, #0x02
	movs	pc,  lnk

.ifdef	enable_MPU

hptp2mpu: @ update MMU access permissions for new heaptop
        @ Caution:	use only with mark_and_sweep gc
	@ called via:	bl, entered right after "swi run_prvlgd" so dsb/isb
	@						not needed on entry
	@ called with:	mcu in privileged mode (set before call)
	@ modifies:	rvb, rvc
	@ returns via:	lnk
	@ 1- update TTB entries in RAM
	vcrfi	rvb, glv, 1		@ rvb <- new heaptop from global vector
	ldr	rvc, =TTB_address	@ rvc <- TTB start address in RAM
	ldr	rvb, [rvc, rvb, LSR #18] @ rvb <- section descriptor from TTB
	tst	rvb, #0x0400		@ is heaptop section already read-only?
	seteq	pc,  lnk		@	if so,  return
	bic	rvb, rvb, #0x0400	@ rvb <- sect desc set to usr rd-only
	str	rvb, [rvc, rvb, LSR #18] @ store read-only sect desc in TTB
	vcrfi	rvb, glv, 1		@ rvb <- new heaptop from global vector
	add	rvb, rvb, #0x00100000	@ rvb <- 1 MB section after new heaptop
	ldr	rvb, [rvc, rvb, LSR #18] @ rvb <- section descriptor from TTB
	orr	rvb, rvb, #0x0400	@ rvb <- sect desc set to usr read/wrt
	str	rvb, [rvc, rvb, LSR #18] @ store read/write sect desc in TTB
	@ 2- force reload into TLB
  .ifdef cortex_a8
  	@ on cortex-a8, do dsb before invalidating TLB (barrier litmus test)
	dsb				@ complete outstanding data accesses
  .endif
	set	rvb, 0
	mcr	p15, 0, rvb, c8, c6, 0	@ invalidate the Data TLB/force reload
	@
	@ an isb/dsb may be needed here??? (according to barrier litmus: yes,
	@ do dsb before invalidating and dsb+isb after)
	@
  .ifdef cortex_a8
  	@ on cortex-a8, do dsb and isb after invalidating TLB (barrier litmus)
	dsb				@ complete outstanding data accesses
	isb				@ complete outstanding instructions
  .endif
	set	pc,  lnk		@ return
.endif


swi_hndlr: @ switch from user mode to specified mode
	@ (including switching interrupts on/off in user mode)
	ldr	r13, [lnk, #-4]		@ r13  <- swi instruction, incl. its arg
	bic	r13, r13, #0xff000000	@ r13  <- new mode = argument of swi
	msr	spsr_c, r13		@ set into spsr
	movs	pc,  lnk		@ return

reset0:	@ soft reset when scheme heap is exhausted
	swi	isr_normal		@ switch to IRQ mode with interrupts
	set	sp,  RAMTOP - 4		@ set stack pointer for IRQ mode
	msr	cpsr_c, #normal_run_mode @ switch to user mode with interrupts
	set	sp,  RAMTOP - 92	@ set stack pointer for system mode
	bl	rldon			@ turn on red (or other) led
	bl	gldoff			@ turn on green (or other) led
	write	  2, timer0_base, #timer_ctrl	@ reset and stop/disable Timer
	read	rvb, rva,         #timer_istat
	write	rvb, rva,         #timer_iset
	write	  2, timer1_base, #timer_ctrl	@ reset and stop/disable Timer
	read	rvb, rva,         #timer_istat
	write	rvb, rva,         #timer_iset
	b	scinit			@ jump to initialize scheme and boot

reset:	/* set stacks for various MCU modes */
  .ifdef STR_9xx
	mrc	p15, 0, cnt, c1, c0, 0
	orr	cnt, cnt, #0x08
	mcr	p15, 0, cnt, c1, c0, 0	@ enable buffered writes to AMBA AHB
	set	cnt, 0x40000
	mcr	p15, 1, cnt, c15, c1, 0	@ order TCM instr (cf.errata, Flash mem)
  .endif
  .ifdef cortex_a8
	/* set interrupt vector base address (cortex_a8, cortex_a9) */
	ldr	rvb, =_startcode	@ start of scheme code (from build_link)
	mcr	p15, 0, rvb, c12, c0, 0	@ VBAR <- set Vector Base Address Reg
  .endif
  .ifndef enable_a9_mpcore
	set	r3, RAMTOP		@ r3 <- RAMPTOP
  .else
	mrc	p15, 0, rvc, c0, c0, 5	@ rvc <- Multiproc Affinity reg, MPIDR
	and	rvc, rvc, #3		@ rvc <- cpu id (cpu[n])
	ldr	r3,  =MP_mat
	ldr	r3, [r3, rvc, lsl 5]	@ r3 <- RAMPTOP_n
  .endif
	sub	fre, r3, #4
	msr	CPSR_c,  #0x1F		@ switch to system mode with interrupts
	set	sp, fre			@ set system mode stack pointer
	msr	CPSR_c,  #0x1B		@ switch to undef instr mode with ints
	set	sp, fre			@ set undef instr mode stack pointer
	msr	CPSR_c,  #0x17		@ switch to abort mode with interrupts
	set	sp, fre			@ set abort mode stack pointer
	msr	CPSR_c,  #0x13		@ switch to supervisor mode with ints
	set	sp, fre			@ set service mode stack pointer
	msr	CPSR_c,  #isr_normal	@ switch to IRQ mode with interrupts
	set	sp, fre			@ set IRQ mode stack pointer
	msr	CPSR_c,  #0x11		@ switch to FIQ mode with interrupts
	set	sp, fre			@ set FIQ mode stack pointer
	msr	cpsr_c, #normal_run_mode @ switch to user mode with interrupts
	sub	sp,  r3, #92		@ set user mode stack pointer
	/* initialize hardware and scheme system */
	b	hwinit



