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
	set	env, sys_ctrl		@ env <- scu register base (0x5C002000)

	/* intialize and enable flash banks 0 and 1 */
	write	    sv4, 0x54000000, #0	@ FMI_BBSR   <- boot bnk sz=2^4*32=512KB
	write	    fre, rva, #0x0C	@ FMI_BBADR  <- boot bank address = 0
	write	    sv2, rva, #0x04	@ FMI_NBBSR  <- non-bt bnk sz=2^2*8=32KB
	write	0x20000, rva, #0x10	@ FMI_NBBADR <- non-bt bnk adrs=#x80000
	write	   0x18, rva, #0x18	@ FMI_CR     <- enable flash banks 0 & 1

	/* configure flash for high-freq, 2-wait-states (need 16-bit str) */
	write16	0x60, 0x80000, #0x00	@ prepare to write flash config
	write16	 sv3, 0x83000, #0x40	@ cfg bnks 0-1, 2 rd wait-state, hi freq

	/* set RAM to 96KB (default is 32KB) */
	write	0x0197, env, #0x34	@ SCU_SCR0 <- set RAM size to 96KB

	/* configure PLL for 96MHz, and enable it */
	write	PLL_parms|(1<<19), env, #4 @ SCU_PLLCONF <- PLL 2x192/4=96MHz
	rgwfbt	env, #0x08, 0, 1	@ wait for PLL locked bit set
	write	0x0480, env, #0x00	@ SCU_CLKCNTR <- clks=96MHz,USB=PCLK=48M

	/* power-up VIC */
	rgrmw	env, #0x14, 0x20	@ SCU_PCGR0 <- enable VIC clock
	rgrmw	env, #0x1C, 0x20	@ SCU_PRR0  <- de-assert VIC reset

	/* power-up TIM 0-1, UART 0-1, I2C 0-1, GPIO 0-9 */
	write	0xFFC0D9, env, #0x18	@ SCU_PCGR1 <- enab TIM,UART,I2C,GPIO
	write	     rvb, env, #0x20	@ SCU_PRR0  <- unreset TIM,UART,I2C,GPIO

	/* return */
	set	pc,  lnk



