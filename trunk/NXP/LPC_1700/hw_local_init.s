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
	write	0x403A, env, #0x00	@ 5 clocks for flash read (up to 100MHz)

	/* configure clocks */
	@ enable the main oscillator
	write	0x20, sys_ctrl+0x100, #0xa0 @ SCS <- enab main osc. 12MHz Xtal
	rgwfbt	rva, #0xa0, 6, 1	    @ wait for osc stable bit set
	@ select clocks and dividers
	write	sv1,  env, #0x80	@ PLLCON  <-    1 == enab+disconnect PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL
	write	fre,  env, #0x80	@ PLLCON  <-    0 == disa+disconnect PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL
	write	sv1,  rva, #0x0c	@ CLKSRCSEL <- 1, select Main Osc
	write	sv2,  rva, #0x04	@ CCLKCFG   <- 2, CPU = 288MHz/3 = 96MHz
	write	sv5,  rva, #0x08	@ USBCLKCFG <- 5, USB = 288MHz/6 = 48MHz
	write	0xaaaaaaaa, rva, #0xa8	@ PCLKSEL1 <- periph clk 48 MHz (CPU/2)
	write	rvb,        rva, #0xac	@ PCLKSEL2 <- periph clk 48 MHz (CPU/2)
	@ configure the PLL
	write	PLL_PM_parms, env,#0x84	@ PLLCFG  <- PLL_PM_parms
	write	sv1,  env, #0x80	@ PLLCON  <-    1 == enable PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL
	rgwfbt	env,  #0x88, PLOCKbit,1	@ wait for PLL locked bit
	write	sv3,  env, #0x80	@ PLLCON  <-    3 == connect PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL

	/* return */
	set	pc,  lnk



