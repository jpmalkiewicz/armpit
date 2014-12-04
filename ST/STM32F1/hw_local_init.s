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
@     This file includes contributions by tzirechnoy, marked <TZC>
@
@     This file includes contributions by Ruslan Popov, marked <RPC>
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

	/* initialize clocks */
	rgcpbt	env, #0x00, 16, 1	@ RCC_CR    <- set HSEON
	rgwfbt	env, #0x00, 17, 1	@ wait for HSERdy bit set in RCC_CR
	write	0x32, flashcr_base, #0	@ FLASH_ACR <- bfr,2 wt stats for 72MHz
  .ifdef connectivity_ln
	write	Clock_parms2, env,#0x2c	@ RCC_CFGR2 <- PLL3x2=50MHz,PLL2x8/5=40M
	rgcpbt	env, #0x00, 26, 1	@ RCC_CR    <- turn PLL2 on
	rgwfbt	env, #0x00, 27, 1	@ wait for PLL2-locked bit set in RCC_CR
	rgcpbt	env, #0x00, 28, 1	@ RCC_CR    <- turn PLL3 on
	rgwfbt	env, #0x00, 29, 1	@ wait for PLL3-locked bit set in RCC_CR
  .endif
	write	Clock_parms, env, #0x04	@ RCC_CFGR  <- USB=48,AHB=72,APB12=36MHz
	rgcpbt	env, #0x00, 24, 1	@ RCC_CR    <- turn PLL on
	rgwfbt	env, #0x04,  3, 1	@ for PLL-connected bit set in RCC_CFGR

	/* initialize APB1 and APB2 Peripheral Clock Power */
	rgrmw	env, #0x18, 0x00005E7D	@ RCC_APB2ENR <- AFIO,ABCDE,AD12,TM1,SP1
	rgrmw	env, #0x1c, 0x00624007	@ RCC_APB1ENR <- TM234,SP2,UAR2,3,I2C1,2

	/* return */
	set	pc,  lnk



