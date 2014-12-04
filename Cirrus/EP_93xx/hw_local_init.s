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


_func_
hw_cfg:	/* configure clocks, interupts, FPU (post-startup) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	env	<- int_base = VIC1 base address
	@ out:	dts	<- VIC2 base address
	@ ret:	via lnk

	/* pre-set common address(es) */
	set	env, int_base		@ env <- VIC1 base address
	add	dts, env, #0x010000	@ dts <- VIC2 base address

	/* configure interrupts */
	ldr	rvb, =genisr
	write	fre, env, #0x0c		@ VIC1IntSelect   <- all ints are IRQ
	write	rvb, env, #0x34		@ VIC1VectDefAddr <- default ISR
	write	fre, dts, #0x0c		@ VIC2IntSelect   <- all ints are IRQ
	write	rvb, dts, #0x34		@ VIC2VectDefAddr <- default ISR

	/* configure FPU */
  .ifdef hardware_FPU
	@ power-up the Maverick Crunch co-processor
	write	0xaa, 0x80930000, #0xc0	@ sysSWLock <- unlock DeviceCfg register
	write	1<<23, rva,       #0x80	@ DeviceCfg enab/pwr-up FPU
	wait	8			@ wait a bit
	@ set default rounding mode to truncate
	cfmv32sc mvdx0, dspsc		@ mvdx0 <- rounding mode from DSPSC
	cfmvr64l rvb, mvdx0		@ rvb   <- rounding mode
	bic	rvb, rvb, #0x0c00	@ clear rounding mode
	orr	rvb, rvb, #0x0400	@ rounding mode = towards 0 (truncate)
	cfmv64lr mvdx0, rvb		@ mvdx0 <- new rounding mode
	cfmvsc32 dspsc, mvdx0		@ set rounding mode in DSPSC
  .endif

	/* return */
	set	pc,  lnk
	


