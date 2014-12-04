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
hw_cfg:	/* configure clocks, sdram, flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	env	<- PMC_base
	@ ret:	via lnk

	/* pre-set common address(es) */
	set	env, PMC_base		@ env <- PMC_base

	/* initialize PLLA and main clock */
	@ (Note: BootROM pre-sets PLLB to 96/48MHz for USB, disables watchdog
	@        and enables reset button)
	write	PLLA_parms, env, #0x28	@ PMC_PLLAR <- 200.1 MHz for CPU
	rgwfbt	env, #0x68, 1, 1	@ wait for PLLA-locked bit set in PMC_SR
	write	0x102, env, #0x30	@ PMC_MCKR  <- mstr clk=PLLA/2 = 100MHz
	rgwfbt	env, #0x68, 3, 1	@ wait for mstr ck rdy bit set in PMC_SR

	/* return */
	set	pc,  lnk



