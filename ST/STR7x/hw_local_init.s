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
hw_cfg:	/* configure clocks, isrs, ... */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	STR7	env	<- rcc_base = RCC base adrs
	@ out:	STM32x	env	<- rcc_base = RCC base adrs
	@ out:	STM32F4	env	<- rcc_base = RCC base adrs
	@ out:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk

	/* pre-set common registers */
	set	env, rcc_base		@ env <- RCC base adrs

	/* initialize voltage and clocks */
	@ wait for voltage regulator to stabilize
	rgwfbt	env, #0x54, 12, 1	@ wait for VREG OK bit set in PCU_PWRCR
	@ initialize the phase-locked loop (PLL1) to get 48MHZ operation
	write	PLL_parms, env, #0x18	@ RCCU_PLL1CR    <- PLL_parms
	write	      fre, env, #0x44	@ RCCU PCU_PDIVR <- APB2,1 clks=mstr=48M
	rgwfbt	env, #0x08, 1, 1	@ wait for PLL1-Locked bit set in _CFR
	write	   0x8009, env, #0x08	@ RCCU_CFR <- connect PLL1 (x-tal div2)

	/* initialize interrupts */
	ldr	rvb, =genisr
	write	rvb, EIC_base, #0x18	@ EIC_IVR	<- default IRQ handler
	lsl	rvb, rvb, #16
	orr	rvb, rvb, #0x07		@ all IRQs have priority of 7
	write	rvb, rva, #0x60		@ TIMER0 ISR = EIC_SIR0
	write	rvb, rva, #0x84		@ UART0 ISR  = EIC_SIR9
	write	rvb, rva, #0x88		@ UART1 ISR  = EIC_SIR10
	write	rvb, rva, #0x9c		@ I2C0 ISR   = EIC_SIR15
	write	rvb, rva, #0xa0		@ I2C1 ISR   = EIC_SIR16
	write	rvb, rva, #0xac		@ TIMER1 ISR = EIC_SIR19
	write	rvb, rva, #0x7c		@ I2C0 ISR   = EIC_SIR7
	write	rvb, rva, #0x80		@ I2C1 ISR   = EIC_SIR8
	write	scheme_ints_enb, rva, #0x20 @ EIC_IER	<- enab scheme,uart ints
	write	int_clear_vals,  rva, #0x40 @ EIC_IPR	<- clr pndng ints in EIC
	write	sv1, rva, #0x00		    @ EIC_ICR	<- enable EIC IRQ output

	/* return */
	set	pc,  lnk



