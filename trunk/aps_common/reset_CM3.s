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
@	Cortex-M3 and Cortex-M4 Common Constants
@
@-----------------------------------------------------------------------------*/

@ interrupt status and enabling/disabling
int_base	= 0xe000e300		@ interrupt status base address
int_stat__0__31	= 0x00			@ interrupts   0 to  31
int_stat_32__63	= 0x04			@ interrupts  32 to  63
int_stat_64__95	= 0x08			@ interrupts  64 to  95
int_stat_96_127	= 0x0C			@ interrupts  96 to 127
int_stat128_159	= 0x10			@ interrupts 128 to 159
int_enab_base	= 0xe000e100		@ interrupt enable  base
int_disa_base	= 0xe000e180		@ interrupt disable base

@ mpu
mpu_base	= 0xe000ed90		@ MPU_TYPE register

@ Cortex-M3 SysTick Timer
systick_base	= 0xe000e000
tick_ctrl	= 0x10
tick_load	= 0x14
tick_val	= 0x18

/*------------------------------------------------------------------------------
@
@	Cortex-M3 and Cortex-M4 Common Code (expressed as macros)
@
@-----------------------------------------------------------------------------*/

.macro	enable_VIC_IRQ
	@ enable interrupts
	swi	run_normal		@ Thread mode, unprivileged, with IRQ
.endm

.macro	intupd	base
	@ enable/disable interrupts
	@ in:	base <- base address of int_enable/int_disable register
	@ mods:	fre, rva
	set	rva, BUFFER_START
	.set	tmp, 0
	.rept	1+(num_interrupts-1)/32
	  vecref fre, rva, CTX_EI_offset+tmp	@ fre <- ena/disa scheme ints
	  str	 fre, [\base, #(tmp<<2)]
	  .set	 tmp, tmp+1
	.endr
.endm

.macro	enterisr
	@ interrupt service routine entry
	@ on exit:	rvb <- interrupt number (of interrupt to process)
	@ on exit:	sp  <- process stack
	mrs	rvb, psp		@ rvb <- psp stack
	set	sp,  rvb		@ sp  <- psp_stack
	@ *** Workaround for Cortex-M3 errata bug #382859, 
	@ *** Category 2, present in r0p0, fixed in r1p0
	@ *** affects LM3S1968 (needed for multitasking)
	ldr	rvb, [sp, #28]		@ rvb <- saved xPSR
	set	rva, 0x0600000c		@ rva <- mask to id int inst as ldm/stm
	tst	rvb, rva		@ was interrupted instruction ldm/stm?
	itT	eq
	biceq	rvb, rvb, #0xf0		@	if so,  rvb <- xPSR to restart
	streq	rvb, [sp, #28]		@	if so,  store xPSR back on stack
	@ *** end of workaround
	set	rvc, 0xe000ed00
	ldr	rvb, [rvc, #4]		@ rvb <- interrupt number
	set	rvc, 0xff		@ rvc <- mask
	orr	rvc, rvc, #0x0100	@ rvc <- updated mask
	and	rvb, rvb, rvc		@ rvb <- masked interrupt number
	sub	rvb, rvb, #16		@ rvb <- adjusted interrupt number
.endm

.macro	clearVicInt	
	@ clear interrupt in interrupt vector (if needed)
	@ nothing to do on cortex-m3/m4
.endm

.macro	exitisr
	@ return from interrupt
	set	pc,  0xfffffffd		@ return to thread mode, process stack
.endm

.macro	isrexit
	@ return from interrupt
	set	pc,  0xfffffffd		@ return to thread mode, process stack
.endm

.macro	stackalign_4
	/* allow 4-byte stack alignment (clear STKALIGN in CCR) in hw_init.s */
	swi	run_prvlgd		@ Thread mode,privileged,no IRQ
	write	fre, 0xe000ed14, #0x00
	swi	run_no_irq		@ Thread mode,unprivileged,no IRQ (user)
.endm

.macro	systick_init
	/* initialize Cortex-M3 SysTick Timer in hw_init.s */
	swi	run_prvlgd		@ Thread mode,privileged,no IRQ
	write	SYSTICK_RELOAD, systick_base, #tick_load @ SYSTICK-RELOAD <-10ms
	write	fre, rva, #tick_val	@ SYSTICK-VALUE   <- 0
	write	sv5, rva, #tick_ctrl	@ SYSTICK-CONTROL <- 5=enab,noint,cpuclk
	swi	run_no_irq		@ Thread mode,unprivileged,no IRQ (user)
.endm

.macro	systick_stop
	/* scheme function (tic-stop) for system_0.s  */
	PRIMIT	"tic-stop", tstop, pfun, 0
	@ stop the systick timer
	@ out:	sv1 <- npo
	swi	run_prvlgd		@ Thread mode,privileged,no IRQ
	write	0, systick_base, #tick_ctrl
	swi	run_normal		@ Thread mode,unprivileged (user)
	b	adr_npofxt
.endm

.macro	systick_start
	/* scheme function (tic-start bool) for system_0.s */
	PRIMIT	"tic-start", tstrt, pfun, 1
	@ start the systick timer (without interrupt generation if bool = #f)
	@ in:	sv1 <- #f (no interrupts) or anything else (interrupts)
	@ out:	sv1 <- npo
	swi	run_prvlgd		@ Thread mode,privileged,no IRQ
	write	0,   systick_base, #tick_ctrl
	write	rvb, rva, #tick_val
	eq	sv1, #f
	itE	eq
	seteq	rvb, 0x05
	setne	rvb, 0x07
	write	rvb, rva, #tick_ctrl
	swi	run_normal		@ Thread mode,unprivileged (user)
	b	adr_npofxt
.endm

.macro	systick_read
	/* scheme function (tic-read) for system_0.s  */
	PRIMIT	"tic-read", tkred, pfun, 0
	@ read current value of the systick timer
	@ out:	sv1 <- value from systick timer
	swi	run_prvlgd		@ Thread mode,privileged,no IRQ
	read	rvb, systick_base, #tick_val
	raw2int	sv1, rvb
	swi	run_normal		@ Thread mode,unprivileged (user)
	set	pc,  cnt
.endm

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
	vecref	rva, sv1, 0
	set	rvc, rva
	set	rvb, 127773		@ rvc <- q = m / a
	sdiv	rva, rva, rvb		@ rva <- seed / q
	mul	rvb, rvb, rva
	sub	rvb, rvc, rvb		@ rvb <- seed % q
	set	rvc, 16807		@ rvc <- a = 7^5
	mul	rvb, rvb, rvc		@ rvb <- a * (seed % q)
	set	rvc, 2836		@ rvc <- r = m % a
	mul	rva, rva, rvc		@ rva <- r * (seed / q)
	subs	rva, rvb, rva		@ rva <- a * (seed % q) - r * (seed / q)
	itT	ls
	setls	rvb, 2147483647		@ 	rvb <- m = 2^31 - 1
	addls	rva, rva, rvb		@	rva <- rva + m
	vecset	sv1, 0, rva		@ set result (rva) into sv1
	set	pc,  cnt
.endm

@-------------------------------------------------------------------------------
@  I.B.2. Cortex
@-------------------------------------------------------------------------------

.ifndef	run_from_ram

_start:	.word	MAIN_STACK			@ 0x00 - Main Stack base address
	.word	reset				@ 0x01 - Reset isr
	.word	nmi_hndlr			@ 0x02 - Non Maskable Int isr
	.word	fault_hndlr			@ 0x03 - Hard Fault isr
	.word	mpu_hndlr			@ 0x04 - MPU isr
	.word	busf_hndlr			@ 0x05 - Bus Fault isr
	.word	usef_hndlr			@ 0x06 - Usage Fault isr
	.word	0x00				@ 0x07 - reserved (LPC checksum)
	.word	0x00				@ 0x08 - reserved
	.word	0x00				@ 0x09 - reserved
	.word	0x00				@ 0x0A - reserved
	.word	svc_hndlr			@ 0x0B - software int handler
	.word	debug_hndlr			@ 0x0C - debug monitor
	.word	0x00				@ 0x0D - reserved
	.word	pends_hndlr			@ 0x0E - pendable service reqst
	.word	tick_hndlr			@ 0x0F - SYS Tick handler
	.rept	(num_interrupts + 31) & 0x01e0
	  .word	genisr				@ INT 0 -> num_interrupts
	.endr

.endif	@ run_from_ram

.ifdef	enable_MPU

_func_
mpu_hndlr:
	@ perform gc on MPU FAULT
	mrs	fre, psp		@ fre <- psp stack
	@ find start address of memory allocation sequence
	set	rva, 0x0003F020		@ rva <- mach code of: bic fre, fre, #3
	ldr	rvb, [fre, #24]		@ rvb <- address of faulted storage inst
	bic	rvb, rvb, #0x01		@ rvb <- address with cleared Thumb bit
cisrc_:	sub	rvb, rvb, #2
	ldr	cnt, [rvb]
	eq	cnt, rva
	bne	cisrc_
	add	cnt, rvb, #4		@ cnt <- gc return lnk (adj for gc ret)
	@ de-reserve memory in case an int (eg. timer) arose while handling this
	rgcpbf	fre, #0x00, 0, 3, 2	@ align to 8-byte (eg. for ibcons)
	@ set stack up to perform gc
	set	rvb, 24			@ rvb <- 24 default/max num bytes needed
	str	rvb, [fre, #12]		@ set number of bytes needed from gc
	ldr	rva, =adr__gc		@ rva <- address of gc routine
	set	rvb, normal_run_mode	@ rvb <- normal run mode
	add	fre, fre, #20
	stmia	fre, {cnt, rva, rvb}	@ set svd lnk_usr,pc_usr,run_mode for gc
mpuhxt:	@ exit
	bx	lnk			@ jump to gc, thread mode, process stack

.else

_func_
mpu_hndlr:
	@ continue to nmi_hndler, etc... (default fault handlers)
.endif

_func_
fault_hndlr:
_func_
nmi_hndlr:
_func_
busf_hndlr:
_func_
usef_hndlr:
_func_
debug_hndlr:
_func_
pends_hndlr:
	b	pends_hndlr

_func_
tick_hndlr:
	mrs	rvc, psp		@ rvc <- psp stack
	set	sp,  rvc		@ sp  <- psp stack, for genis0
	@ *** Workaround for Cortex-M3 errata bug #382859, Category 2,
	@ *** present in r0p0, fixed in r1p0,
	@ *** affects LM3S1968 (needed for multitasking)
	ldr	rvb, [sp, #28]		@ rvb <- saved xPSR
	set	rva, 0x0600000c		@ rva <- msk to id if ldm/stm intrptd
	tst	rvb, rva		@ was interruted instruction ldm/stm?
	itT	eq
	biceq	rvb, rvb, #0xf0		@	if so,  rvb <- xPSR to restart
	streq	rvb, [sp, #28]		@	if so,  store xPSR back on stack
	@ *** end of workaround
	read	rvb, systick_base, #tick_ctrl	@ clear the tick flag
	write	0x05, rva, #tick_ctrl	@ disab systick int generation (if set)
	set	rvb, 0xff		@ rvb <- 255 = interrupt num for systick
	b	genis0

_func_
svc_hndlr:
	mrs	rva, psp		@ rva <- psp stack
	ldr	rvc, [rva, #24]		@ rvc <- saved lnk_irq (pc_usr) frm stk
	ldrh	rvb, [rvc, #-2]		@ rvb <- svc instruction, incl its arg
	and	rvb, rvb, #0xff		@ rvb <- argument of svc
	eq	rvb, #isr_no_irq	@ stay in irq mode and continue?
	itTT	eq
	seteq	sp,  rva		@ 	if so,  sp  <- psp stack
	addeq	sp,  sp, #32		@ 	if so,  sp  <- psp stack, updatd
	seteq	pc,  rvc		@	if so,  return, in IRQ mode
svcigc:	@ [internal entry from genism]
	@ enable/disable scheme interrupts
	eq	rvb, #run_normal	@ enable interrupts?
	itE	eq
	seteq	cnt, int_enab_base
	setne	cnt, int_disa_base
	intupd	cnt
	mrs	rva, control		@ rva  <- content of Processor Cntrl reg
	eq	rvb, #run_prvlgd	@ set thread mode to privileged, no irq?
	itE	eq
	biceq	rva, rva, #0x01		@ 	if so,  rva  <- prvlgd Thread bt
	orrne	rva, rva, #0x01		@ 	if not, rva  <- unprvlgd Thrd bt
	msr	control, rva		@ set Thread mode to privil/unprivileged
	set	pc,  0xfffffffd		@ return to thread mode, w/process stack

.ifdef	enable_MPU

_func_
hptp2mpu: @ update MPU for new heaptop(s)
	@ called via:	bl, entered right after "swi run_prvlgd" so dsb/isb
	@						not needed on entry
	@ called with:	mcu in privileged mode (set before call)
	@ modifies:	rvb, rvc
	@ returns via:	lnk
	set	rvc, mpu_base		@ rvc      <- address of MPU_TYPE
	write	3,          rvc, #0x08	@ MPU_RNR  <- set region to 3
	write16	0x0008,     rvc, #0x10	@ MPU_RASR <- set region size (disabled)
  .ifndef mark_and_sweep
	vecref	rvb, glv, 9		@ rvc <- heaptop0 -- from global vector
  .else
	vecref	rvb, glv, 1		@ rva <- heaptop -- from global vector
  .endif
	bic	rvb, rvb, #i0		@ rvb      <- heaptop region (32B align)
	write	rvb,        rvc, #0x0c	@ MPU_RBAR <- set region start address
	write	0x02040009, rvc, #0x10	@ MPU_RASR <- nrm hndl rw,usr ro,shr,32B

  .ifndef mark_and_sweep
	write	4,          rvc, #0x08	@ MPU_RNR  <- set region to 4
	write16	0x0008,     rvc, #0x10	@ MPU_RASR <- set region size (disabled)
	vecref	rvb, glv, 10		@ rvb      <- heaptop1 from global vec
	bic	rvb, rvb, #i0		@ rvb      <- heaptop region (32B align)
	write	rvb,        rvc, #0x0c	@ MPU_RBAR <- set region start address
	write	0x02040009, rvc, #0x10	@ MPU_RASR <- nrm hndl rw,usr ro,shr,32B
  .endif
	dsb				@ complete outstanding data accesses
	isb				@ complete outstanding instructions
	set	pc,  lnk		@ return
.endif

_func_
isrreset: @ soft reset when heap is exhausted and system is in IRQ Handler mode
	mrs	rva, control		@ rva  <- content of Processor Cntrl reg
	bic	rva, rva, #0x01		@ rva  <- bit for privilegd Thread mode
	msr	control, rva		@ set Thread mode to privlgd (for reset)
	mrs	rvc, psp		@ rvc <- psp stack
	ldr	rvb, =reset		@ rvb <- address of reset routine
	str	rvb, [rvc, #24]		@ set saved pc_usr (lnk_irq) for reset
	set	rvb, normal_run_mode	@ rvb <- normal run mode
	str	rvb, [rvc, #28]		@ set normal run mode in saved xPSR
	set	pc,  0xfffffffd		@ jump to reset in privileged thread mod

_func_
reset0:	@ soft reset when scheme heap is exhausted
	bl	rldon			@ turns on/off red   (or other) led
	bl	gldoff			@ turns off/on green (or other) led
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ
	@ continue to reset (below)

_func_
reset:	@ enable MPU if desired (done in privileged mode)
  .ifdef enable_MPU
	@ for cortex-M4 MPU and memory map
	write	5,     mpu_base, #0x04	@ MPU_CTRL <- enable MPU w/dflt map
	write	0x10,       rva, #0x0c	@ MPU_RBAR <- region 0 adrs=0x00, valid
	write	0x0306003f, rva, #0x10	@ MPU_RASR <- norml mem rw,shar,cach,4GB
	write	0x11,       rva, #0x0c	@ MPU_RBAR <- region 1 adrs=0x00, valid
	write	0x13051b3f, rva, #0x10	@ MPU_RASR <- dev xn,rw,share,4GB w/hols
	write	0xe0000012, rva, #0x0c	@ MPU_RBAR <- region 2 adrs=0xe0000000
	write	0x13040027, rva, #0x10	@ MPU_RASR <- strng-ordrd xn,rw,shar,1MB
	write	1<<16, 0xe000ed24, #0	@ SHCSR <- enab MemManage fault handling
  .endif
  .ifdef hardware_FPU
	/* enable FPU if desired (done in privileged mode, enables all access) */
	rgrmw	0xe000ed88, #0, 0xf<<20	@ CPACR <- enab CP10,11,FPU (bits 20-23)
	dsb				@ wait for store (bit 20-23) to complete
	isb				@ wait for instr to complete (FPU enbld)
	vmrs	rva, fpscr		@ rva    <- contents of FPSCR
	orr	rva, rva, #0x00c00000	@ rva    <- round towards zero (trunc)
	vmsr	fpscr, rva		@ FPSCR <- updated rounding mode
	write	0, 0xe000ef34, #0	@ FPCCR <- no FPU reg stacking on int
	dsb				@ wait for store to complete
	isb				@ wait for instruction to complete
  .endif
  .ifdef excep_vector_adr
	/* remap exception vector address */
  	write	excep_vector_adr, 0xE000ED08, #0 @ VTOR <- exception vectors adr
  .endif
	/* configure Process Stack, select it and drop to User mode */
	ldr	rva, =MAIN_STACK - 88	@ rva  <- address of Process Stack
	msr	psp, rva		@ Set Process stack address
	mrs	rva, control		@ rva  <- content of Processor Cntrl reg
	orr	rva, rva, #0x02		@ rva  <- code to use Process stack
  .ifdef hardware_FPU
	bic	rva, rva, #0x04		@ rva  <- clear FPCA bit (make sure)
  .endif
	orr	rva, rva, #0x01		@ rva  <- bit for User (unpriv) mode
	msr	control, rva		@ drop to unprivlgd mode, Process stack
	/* initialize hardware and scheme system */
	b	hwinit



