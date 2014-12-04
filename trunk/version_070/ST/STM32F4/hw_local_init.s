/*------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 070
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2012-2014 Petr Cermak

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

	/* allow 4-byte stack (clear STKALIGN in CCR) */
	stackalign_4			@ cortex-m3/m4 macro

	/* Disable all interrupts */
	write	fre, env, #0x0C		@ RCC_CIR   <- 0

	/* initialize high-speed external (HSE) oscillator clock */
  .ifndef HSI_is_PLL_src
	rgcpbt	env, #0x00, 16, 1	@ RCC_CR    <- set HSEON
	rgwfbt	env, #0x00, 17, 1	@ wait for HSERdy bit set in RCC_CR
  .endif

	/* Select regulator voltage output Scale 1 mode, Sys freq to 168MHz */
	rgcpbt	env, #0x40, 28, 1	@ RCC_APB1ENR <- set PWREN bit
	write	1<<14, pwr_base, #0	@ PWR_CR      <- PWR_CR_VOS

	/* FLASH_ACR <- enable buffer, set wait states */
	write	(7<<8)|FLSH_WAIT_ST, flashcr_base, #0	@ FLASH_ACR <- wtst,cch
	rgwfbf	rva, #0x00, 0, 3, FLSH_WAIT_ST	@ wait for wait states

	/* configure clocks */
	write	Clock_parms, env, #0x04	@ Configure the main PLL
	rgrmw	env, #0x08,Prescl_parms	@ Configure Prescaler
	rgcpbt	env, #0x00, 24, 1	@ RCC_CR <- turn PLL on
	rgwfbt	env, #0x00, 25, 1	@ wait for PLL conn bit set in RCC_CR
	rgcpbf	env, #0x08,  0, 2, 2	@ RCC_CR <- PLL as System clock on
	rgwfbt	env, #0x08,  3, 1	@ wait for PLL Rdy bit set in RCC_CFGR

	/* copy .data section to CCM */
  .ifdef harvard_split
	ldr	dts, =_data_section_address_	@ start of source
	ldr	glv, =_startdata	@ start of dest (from build_link file)
	ldr	rvc, =_enddata		@ end of dest (from build_link file)
	add	rvc, rvc, #4
datcp0:	ldmia	dts!, {rva-rvb}
	stmia	glv!, {rva-rvb}
	cmp	glv, rvc
	bmi	datcp0
  .endif

	/* return */
	set	pc,  lnk



