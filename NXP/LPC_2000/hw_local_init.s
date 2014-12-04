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

/*
	Note: to initialize the Memory Accelerator Module (MAM) for 60MHZ op --
	according to the Errata published online, disabling MAM can cause
	problems with SRAM reading while running a program from Flash
	recommended settings are: MAMCR = 0x2 and MAMTIM = 0x1 (<20MHZ)
	0x2 (<40MHZ) 0x3 (>40MHZ) or MAMCR = 0x0 and MAMTIM = 0x1.
	To set MAMTIM first set MAMCR to 0 then back to non-zero value
*/

_func_
hw_cfg:	/* configure clocks, sdram, flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk

	/* pre-set common addresses */
	set	env, sys_ctrl		@ env <- clock selection, enabling, ...

	/* set MAMTIM (on-chip flash access timing) */
	write	fre, env, #0x00		@ MAMCR  <- 0
  .ifndef LPC2478_STK
	write	sv3, env, #0x04		@ MAMTIM <- 3 for PLL at 60 MHZ
	write	sv2, env, #0x00		@ MAMCR  <- 2 to enable MAMTIM
	write	sv1, env, #0x0100	@ VPBDIV  <- peripheral clock divisor
  .else
	write	sv4, env, #0x04		@ MAMTIM <- 4 for PLL at 72 MHZ
	write	sv2, env, #0x00		@ MAMCR  <- 2 to enable MAMTIM
  .endif

	/* configure clocks */
  .ifdef LPC2478_STK
	@ enable the main oscillator
	write	0x20, sys_ctrl+0x0100, #0xa0 @ enable main osc
	rgwfbt	rva, #0xa0, 6, 1	     @ wait for osc rdy bit
	@ select clocks and dividers
	write	sv1,  env, #0x80	@ PLLCON  <-    1 == enable+discnnct PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL
	write	fre,  env, #0x80	@ PLLCON  <-    0 == disbl+discnnctd PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL
	write	sv1,  rva, #0x0c	@ CLKSRCSEL <- 1, select Main Osc
	write	sv3,  rva, #0x04	@ CCLKCFG   <- 3, CPU = 288MHz/4 = 72MHz
	write	sv5,  rva, #0x08	@ USBCLKCFG <- 5, USB = 288MHz/6 = 48MHz
	write	0x55555555, rva, #0xa8	@ PCLKSEL1 <- peripherals at 72 MHz
	write	rvb,  rva, #0xac	@ PCLKSEL2 <- peripherals at 72 MHz
  .endif
	@ configure the PLL
	write	PLL_PM_parms, env, #0x84	@ PLLCFG  <- PLL_PM_parms
	write	sv1,  env, #0x80	@ PLLCON  <-    1 == enable PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL
	rgwfbt	env, #0x88, PLOCKbit, 1	@ wait for PLL lock bit
	write	sv3,  env, #0x80	@ PLLCON  <-    3 == connect PLL
	write	0xaa, env, #0x8c	@ PLLFEED <- 0xaa == feed PLL
	write	0x55, env, #0x8c	@ PLLFEED <- 0x55 == feed PLL

	/* initialize interrupts */
	write	fre, int_base, #0x0c	@ VICIntSelect   <- all interrupts = IRQ
  .ifndef LPC2478_STK
	ldr	rvb, =genisr
	write	rvb, rva, #0x34		@ VICDefVectAddr <- Default IRQ handler
  .else
	ldr	rvb, =genisr
	write	rvb, int_base+0x100, #0x10 @ VectAddr4  <- TIMER0 IRQ isr=genisr
	write	rvb, rva, #0x14		@ VICVectAddr5  <- TIMER1 IRQ isr=genisr
	write	rvb, rva, #0x18		@ VICVectAddr6  <- UART0  IRQ isr=genisr
	write	rvb, rva, #0x1c		@ VICVectAddr7  <- UART1  IRQ isr=genisr
	write	rvb, rva, #0x24		@ VICVectAddr9  <- I2C0   IRQ isr=genisr
	write	rvb, rva, #0x4c		@ VICVectAddr19 <- I2C1   IRQ isr=genisr
  .endif

	/* return */
	set	pc,  lnk



