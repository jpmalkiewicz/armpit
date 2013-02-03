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


@-------------------------------------------------------------------------------------
@  I.B.2. Cortex
@-------------------------------------------------------------------------------------

_start:
	.word	RAMTOP				@ 0x00 - Main Stack base address
	.word	reset				@ 0x01 - Reset isr
	.word	nmi_hndlr			@ 0x02 - Non Maskable Interrupt isr
	.word	fault_hndlr			@ 0x03 - Hard Fault isr
	.word	mpu_hndlr			@ 0x04 - MPU isr
	.word	busf_hndlr			@ 0x05 - Bus Fault isr
	.word	usef_hndlr			@ 0x06 - Usage Fault isr
	.word	0x00				@ 0x07 - reserved (check_sum on LPC)
	.word	0x00				@ 0x08 - reserved
	.word	0x00				@ 0x09 - reserved
	.word	0x00				@ 0x0A - reserved
	.word	svc_hndlr			@ 0x0B - software interrupt handler
	.word	debug_hndlr			@ 0x0C - debug monitor
	.word	0x00				@ 0x0D - reserved
	.word	pends_hndlr			@ 0x0E - pendable service request
	.word	tick_hndlr			@ 0x0F - SYS Tick handler
	.word	genisr, genisr, genisr, genisr	@ 0x10-0x13 -> INT 00-03
	.word	genisr, genisr, genisr, genisr	@ 0x14-0x17 -> INT 04-07
	.word	genisr, genisr, genisr, genisr	@ 0x18-0x1B -> INT 08-11
	.word	genisr, genisr, genisr, genisr	@ 0x1C-0x1F -> INT 12-15
	.word	genisr, genisr, genisr, genisr	@ 0x20-0x23 -> INT 16-19
	.word	genisr, genisr, genisr, genisr	@ 0x24-0x27 -> INT 20-23
	.word	genisr, genisr, genisr, genisr	@ 0x28-0x2B -> INT 24-27
	.word	genisr, genisr, genisr, genisr	@ 0x2C-0x2F -> INT 28-31
	.word	genisr, genisr, genisr, genisr	@ 0x30-0x33 -> INT 32-33
	.word	genisr, genisr, genisr, genisr	@ 0x34-0x37 -> INT 36-39
	.word	genisr, genisr, genisr, genisr	@ 0x38-0x3B -> INT 40-43
	.word	genisr, genisr, genisr, genisr	@ 0x3C-0x3F -> INT 44-47
	.word	genisr, genisr, genisr, genisr	@ 0x40-0x43 -> INT 48-51
	.word	genisr, genisr, genisr, genisr	@ 0x44-0x47 -> INT 52-55
	.word	genisr, genisr, genisr, genisr	@ 0x48-0x4B -> INT 56-59
	.word	genisr, genisr, genisr, genisr	@ 0x4C-0x4F -> INT 60-63
.if num_interrupts > 64
	.word	genisr, genisr, genisr, genisr	@ 0x50-0x53 -> INT 64-67, eg. STM32 connectivity line
	.word	genisr, genisr, genisr, genisr	@ 0x54-0x57 -> INT 68-71
	.word	genisr, genisr, genisr, genisr	@ 0x58-0x5B -> INT 72-75
	.word	genisr, genisr, genisr, genisr	@ 0x5C-0x5F -> INT 76-79
	.word	genisr, genisr, genisr, genisr	@ 0x60-0x63 -> INT 80-83
	.word	genisr, genisr, genisr, genisr	@ 0x64-0x67 -> INT 84-87
	.word	genisr, genisr, genisr, genisr	@ 0x68-0x6B -> INT 88-91
	.word	genisr, genisr, genisr, genisr	@ 0x6C-0x6F -> INT 92-95
.endif

_func_
nmi_hndlr:
_func_
fault_hndlr:
_func_
mpu_hndlr:
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
	mrs	sp,  psp		@ sp  <- psp stack
	@ *** Workaround for Cortex-M3 errata bug #382859, Category 2, present in r0p0, fixed in r1p0
	@ *** affects LM3S1968 (needed for multitasking)
	ldr	rvb, [sp, #28]		@ rvb <- saved xPSR
	ldr	rva, =0x0600000c	@ rva <- bit mask to identfy if interrupted instruction was ldm/stm
	tst	rvb, rva		@ was interruted instruction ldm/stm?
	itT	eq
	biceq	rvb, rvb, #0xf0		@	if so,  rvb <- xPSR set to restart (not continue) ldm/stm
	streq	rvb, [sp, #28]		@	if so,  store xPSR back on stack
	@ *** end of workaround
	ldr	rva, =systick_base
	ldr	rvb, [rva, #tick_ctrl]	@ clear the tick flag
	set	rvb, #0x05
	str	rvb, [rva, #tick_ctrl]	@ disable systick interrupt generation (if set)
	set	rvb, #64		@ rvb <-  64 = interrupt number for systick
	b	genis0

_func_
svc_hndlr:
	mrs	sp,  psp		@ sp  <- psp stack
	ldr	r12, [sp,  #24]		@ r1  <- saved lnk_irq (pc_usr) from stack
	ldrh	r2,  [r12, #-2]		@ r2  <- svc instruction, including its argument
	and	r3,  r2,  #0xff		@ r3  <- argument of svc
	eq	r3,  #isr_no_irq	@ stay in irq mode and continue?
	itT	eq
	addeq	sp,  sp, #32
	seteq	pc,  r12		@	if so,  return, while in IRQ mode
	ldr	r1,  =int_en_base
	eq	r3,  #run_normal	@ enable interrupts?
	it	ne
	addne	r1,  r1, #int_disab1
	@ enable/disable scheme interrupts
	ldr	r2,  =BUFFER_START
	vcrfi	r0,  r2, CTX_EI_offset	 @ r0 <- enabled scheme interrupts  0-31 (raw)
	str	r0,  [r1, #0x00]
	vcrfi	r0,  r2, CTX_EI_offset+4 @ r0 <- enabled scheme interrupts 32-63 (raw)
	str	r0,  [r1, #0x04]
.if num_interrupts > 64
	vcrfi	r0,  r2, CTX_EI_offset+8 @ r0 <- enabled scheme interrupts 64-95 (raw)
	str	r0,  [r1, #0x08]
.endif
	ldr	pc,  =0xfffffffd	@ return to thread mode, use process stack

_func_
reset0:	@ soft reset when scheme heap is exhausted
	ldr	r0,  =RAMTOP - 88	@ r0  <- address of Process Stack
	msr	psp, r0			@ Set Process stack address
	mrs	r0,  control		@ r0  <- contents of Processor Control register
	orr	r0,  r0, #0x02		@ r0  <- code to use Process stack
	bic	r0,  r0, #0x01		@ r0  <- code to drop to User (unprivileged) mode
	msr	control, r0		@ drop to User mode and use Process stack
	bl	rldon			@ turns on red (or other) led on LPC boards
	bl	gldoff			@ turns on green led on AT91SAM7 board
	b	scinit			@ jump to initialize scheme and boot

_func_
reset:	@ configure Process Stack, select it and drop to User mode
	ldr	r0,  =RAMTOP - 88	@ r0  <- address of Process Stack
	msr	psp, r0			@ Set Process stack address
	mrs	r0,  control		@ r0  <- contents of Processor Control register
	orr	r0,  r0, #0x02		@ r0  <- code to use Process stack
	bic	r0,  r0, #0x01		@ r0  <- code to drop to User (unprivileged) mode
	msr	control, r0		@ drop to User mode and use Process stack
	
