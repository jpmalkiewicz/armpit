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
@ Contributions:
@
@     This file includes contributions by Robbie Dinn, marked <RDC>
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
	write	0x480100, 0xFFFFFF60, #0 @ MC_FMR    <- flsh wt stt=1,Rd-2,Wrt-3
	write	  0x8000, 0xFFFFFD44, #0 @ WDTC_WDMR <- disable watchdog timer
	write	   0xFF01, env, #0x20	@ PMC_MOR    <- StrtCnt=6[*8]1.5ms,enab
	rgwfbt	env, #0x68, 0, 1	@ wait for PMC_MOSCS bit set in PMC_SR
	write	PLL_parms, env, #0x2c	@ PMC_PLLR   <- 96 MHz
	rgwfbt	env, #0x68, 2, 1	@ wait for PMC_LOCK bit set in PMC_SR
	write	      sv4, env, #0x30	@ PMC_MCKR   <- sys clck prescaler=1/2
	rgwfbt	env, #0x68, 3, 1	@ wait for PMC_MCKRDY bit set in PMC_SR
	write	     0x07, env, #0x30	@ PMC_MCKR   <- sys clck<-PLL/2
	rgwfbt	env, #0x68, 3, 1	@ wait for PMC_MCKRDY bit set in PMC_SR
	write	0xA5000401, 0xFFFFFD08, #0 @ RSTC_RMR <- enable reset button 1ms

	/* initialize interrupts vector for UART0,1, twi (i2c), timer0,1 */
	ldr	rvb, =spuisr
	write	rvb, AIC_SPU,  #0x00	@ AIC_SPU   <- spuisr spurious int hndlr
	write	rvb, AIC_SVR0, #0x00 	@ AIC_SVR0  <- spuisr as fiq handler
	write	rvb, rva, #0x04		@ AIC_SVR1  <- spuisr as sys irq handler
	ldr	rvb, =genisr
	write	rvb, rva, #0x18		@ AIC_SVR6  <- set genisr as uart0  isr
	write	rvb, rva, #0x1c		@ AIC_SVR7  <- set genisr as uart1  isr
	write	rvb, rva, #0x24		@ AIC_SVR9  <- set genisr as twi    isr
	write	rvb, rva, #0x30		@ AIC_SVR12 <- set genisr as timer0 isr
	write	rvb, rva, #0x34		@ AIC_SVR13 <- set genisr as timer1 isr

	/* initialization complete -- return */
	set	pc,  lnk

	
	/* Spurius interrupt handler (if needed) */
spuisr:	sub	lnk, lnk, #4		@ Adjust lnk to point to return 
	stmdb	sp!, {rva, rvb, lnk}	@ store lnk_irq on irq stack
	read	rvb, int_base, #int_status
	write	rvb, rva, #int_iccr
	write	0x00, rva, #int_clear
	ldmia	sp!, {rva, rvb, pc}^	@ return



