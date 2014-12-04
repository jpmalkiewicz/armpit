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
hw_cfg:	@ configure clocks, sdram, flash
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	env	<- PMC_base
	@ ret:	via lnk

	/* pre-set common address(es) */
	set	env, PMC_base		@ env <- PMC_base

	/* initialize wait states, oscillator, PLL and main clock */
	write	(FLSH_WTSTA<<8), EEFC_base, #0	@ EEFC_FMR  <- wait states
	write	    0x8000, 0x400E1454, #0x00	@ WDTC_MR   <- disable watchdog
	write	0x0137FF01,        env, #0x20	@ PMC_MOR   <- enable main osc
	rgwfbt	env, #0x68, 0, 1		@ wait for PMC_SR bit 0 (MOSCS)
	write	PLL_parmsA, env, #0x28	@ PMC_PLLAR <- 128 MHz
	write	PLL_parmsB, env, #0x2c	@ PMC_PLLBR <-  96 MHz
	rgwfbt	env, #0x68, 2, 1	@ wait for bit 2 set in PMC_SR
	rgwfbt	env, #0x68, 1, 1	@ wait for bit 1 set (LOCK) in PMC_SR
	write	0x3000, env, #0x30	@ PMC_MCKR  <- PLLA div = PLLB div = 2
	rgwfbt	env, #0x68, 3, 1	@ wait for bit 3 (MCKRDY) set in PMC_SR
	write	0x3002, env, #0x30	@ PMC_MCKR  <- system clock = PLLA/2
	rgwfbt	env, #0x68, 3, 1	@ wait for bit 3 (MCKRDY) set in PMC_SR
	write	0xA5000401, 0x400E1408, #0x00	@ RSTC_RMR  <- enab reset button

	/* return */
	set	pc,  lnk



