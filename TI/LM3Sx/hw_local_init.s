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
hw_cfg:	/* configure clocks, isrs */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ out:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ out:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ out:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ out:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ out:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ out:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ out:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ out:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ out:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk

	/* pre-set common registers */
	set	env, rcgc_base		@ env <- peripheral RCGC base adrs
	set	dts, sys_base		@ dts <- System Control -- RCC base adrs

	/* initialize clocks */
  .ifndef TI_EvalBot
	rgcpbf	dts, #rcc,   6, 10, 0xe		@ RCC  <- XTAL=0xE =  8MHz Xtal
	rgcpbt	dts, #rcc,   0,  0		@ RCC  <- MOSCDIS=0,ena main osc
	rgcpbf	dts, #rcc,  23, 27, 0x3		@ RCC  <- SYSDIV=3,dv4,PLL=50MHz
	rgcpbt	dts, #rcc,  22,  1		@ RCC  <- USESYSDIV=1,ena fdiv 4
	rgcpbf	dts, #rcc,   4,  6, 0x0		@ RCC  <- OSCSRC=0,clk=main osc
	rgcpbt	dts, #rcc,  13,  0		@ RCC  <- PWRDN=0, power up PLL
	rgwfbt	dts, #0x50,  6,  1		@ wait for PLL Trdy bit in RIS
	rgcpbt	dts, #rcc,  11,  0		@ RCC  <- BYPASS=0, connect PLL
  .else	@ TI_EvalBot
	rgcpbf	dts, #rcc,   6, 11, 0x15	@ RCC  <- XTAL=0x15 = 16MHz Xtal
	rgcpbt	dts, #rcc,   0,  0		@ RCC  <- MOSCDIS=0,ena main osc
	rgcpbf	dts, #rcc2, 22, 32, 0x304	@ RCC2 <- SYSDIV2=5 w/LSB2,80MHz
	rgcpbt	dts, #rcc,  22,  1		@ RCC  <- USESYSDIV=1,ena f div
	rgcpbf	dts, #rcc2,  4,  7, 0x00	@ RCC2 <- OSCSRC2=0,ck=main osc
	rgcpbf	dts, #rcc2, 13, 15, 0x00	@ RCC2 <- PWRDN2=0,pwr PLL & USB
	rgwfbf	dts, #0x50,  6,  8, 0x03	@ wait for PLL & USB Trdy bits
	rgcpbt	dts, #rcc2, 11,  0		@ RCC2 <- BYPASS2=0, connect PLL
  .endif

	/* initialize some clocks */
	rgrmw	env, #0x04, 0x035003	@ RCGC1 <- enab clk UAR0,1,I2C0,1,Tmr0,1

	/* return */
	set	pc,  lnk



