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
	@ out:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk

	/* pre-set common addresses */
	set	env, sys_ctrl		@ env <- clock selection, enabling, ...

	/* set FLASHCFG (formerly MAMTIM) */
	rgcpbf	flash_tim, #0x00, 0,2,2	@ 3 clocks for flash read (up to 72 MHz)

	/* configure clocks */
	rgcpbt	env, #0x0238, 5, 0	@ PDRUNCFG     <- ~#x20 = pwr 12MHz Xtal
	@ configure and enable the PLL
	write	sv1, env, #0x40		@ SYSPLLCLKSEL <- sys osc = input to PLL
	write	fre, env, #0x44		@ SYSPLLCLKUEN <- enable  clk sel update
	write	sv1, env, #0x44		@ SYSPLLCLKUEN <- perform clk sel update
	write	PLL_PM_parms, env, #0x8	@ SYSPLLCTRL   <- 288/4=72MHz/(5+1)=xtal
	rgcpbt	env, #0x0238, 7, 0	@ PDRUNCFG     <- ~#x80 = power up PLL
	rgwfbt	env, #0x0c, PLOCKbit,1	@ wait for PLL locked bit in PLL status
	@ set main clock to PLL output (72 MHz)
	write	sv3, env, #0x70		@ MAINCLKSEL   <- sys pll as main clock
	write	fre, env, #0x74		@ MAINCLKUEN   <- enable  clk sel update
	write	sv1, env, #0x74		@ MAINCLKUEN   <- perform clk sel update

	/* return */
	set	pc,  lnk



