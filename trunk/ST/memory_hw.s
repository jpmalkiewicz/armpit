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
memcfg:	/* configure memory (esp. external SDRAM) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk

  .ifdef STM32F429_Disco

	@ ISSI IS42S16400J-7TL (8 MB), 16-bit bus, SDNE1 Chip-sel (SDRAM Bank 2)
	@ power-up FMC and pin ports
	rgrmw	env, #0x30, 0x7f	@ RCC_AHB1ENR  <- enable Ports B to G
	rgrmw	env, #0x38, sv1		@ RCC_AHB3ENR  <- enable FMC
	@ configure pins: AF12, push-pull (dflt), fast speed, no pull-u/d (dflt)
	set	rvc, lnk		@ rvc <- lnk, saved
	@ 1 - port B:  PB5 = cke, PB6 = nei
	ldr	rva, =ioportb_base	@ rva <- GPIOB_MODER address
	mask	glv, 1,  2, 5,6		@ glv <- PB5, PB6 = AF (cke1,nei) / fast
	mask	dts, 2, 12, 5,6		@ dts <- PB5, PB6 = AF12 (cke1,nei)
	set	cnt, 0			@ cnt <- 0, no high pins (8-15)
	bl	fmcmoa
	@ 2 - port C:  PC0 = nwe
	mask	glv, 1,  2, 0		@ glv <- PC0 = AF (nwe)/ fast
	mask	dts, 2, 12, 0		@ dts <- PC0 = AF12 (nwe)
	set	cnt, 0			@ cnt <- 0, no high pins (8-15)
	bl	fmcmoa
	@ 3 - port D:  PD0,1 = D2,3, PD8-10 = D13-15, PD14,15 = D0,1
	mask	glv, 1,  2, 0,1,8,9,10,14,15	@ glv <- PDn   = Dx / fast
	mask	dts, 2, 12, 0,1		 	@ dts <- PD0,1        = AF12
	mask	cnt, 2, 12, 8,9,10,14,15 	@ cnt <- PD8-10,14,15 = AF12
	bl	fmcmoa
	@ 4 - port E:  PE0,1 = NBL0,1, PE7-15 = D2,3,13-15,0,1
	mask	glv, 1,  2, 0,1,7,8,9,10,11,12,13,14,15 @ glv <- PEn = NBLn,Dx
	mask	dts, 2, 12, 0,1,7		 	@ dts <- PE0,1,7 = AF12
	mask	cnt, 2, 12, 8,9,10,11,12,13,14,15 	@ cnt <- PE8-15  = AF12
	bl	fmcmoa
	@ 5 - port F:  PF0-5 = A0-5, PF11 = NRAS, PF12-15 = A6-9
	mask	glv, 1,  2, 0,1,2,3,4,5,11,12,13,14,15	@ glv <- PFn = Ax,NRAS
	mask	dts, 2, 12, 0,1,2,3,4,5			@ dts <- PF0-5   = AF12
	mask	cnt, 2, 12, 11,12,13,14,15 		@ cnt <- PF11-15 = AF12
	bl	fmcmoa
	@ 6 - port G:  PG0,1 = A10,11, PG4,5 = BA0,1, PG8 = CLK, PG15 = NCAS
	mask	glv, 1,  2, 0,1,4,5,8,15	@ glv <- PGn = Ax,Dx,BA,CLK,NCAS
	mask	dts, 2, 12, 0,1,4,5		@ dts <- PG0,1,4,5 = AF12
	mask	cnt, 2, 12, 8,15 		@ cnt <- PG8,15    = AF12
	bl	fmcmoa
	@ configure SDRAM interface and timing
	write	0x3954, fmc_base+0x140, #0 @ _SDCR1 <- CAS2,brst,4bks,16b,12r,8c
	write	       rvb, rva, #0x04	@ FMC_SDCR2 <- clk/2,CAS,bits,row,col
	write	0x01115351, rva, #0x08	@ FMC_SDTR1 <- trc=63n,tras=42n,txsr=70n
	write	       rvb, rva, #0x0c	@ FMC_SDTR2 <- trcd=15n,trp=15n,twr=2c
	@ initialize SDRAM
	bl	fmc_wt			@ wait for FMC ready
	write	0x09, rva, #0x10	@ FMC_SDCMR <- enable clock cfg, Mode 0
	bl	fmc_wt			@ wait for FMC ready
	write	0x0a, rva, #0x10	@ FMC_SDCMR <- PALL
	bl	fmc_wt			@ wait for FMC ready
	write	0xeb, rva, #0x10	@ FMC_SDCMR <- autorefresh 8
	bl	fmc_wt			@ wait for FMC ready
	write	((0x221<<9)|0x0c),rva,#0x10 @_SDCMR <- CAS2,seq rd brst 2,32 bit
	write	(1280<<1), rva, #0x14	@ FMC_SDRTR <- 64ms/2^12row*168/2MHz-20
	bl	fmc_wt			@ wait for FMC ready
	b	fmc_dn			@ jump to finish up

_func_
fmcmoa:	@ set FMC_MODER <- glv, FMC_OSPEEDR <- glv, FMC_AFRL,AFRH <- dts,cnt
	rgrmw	rva, #0x00, glv		@ GPIOn_MODER   <- AF   for SDRAM pins
	rgrmw	rva, #0x08, glv		@ GPIOn_OSPEEDR <- fast for SDRAM pins
	rgrmw	rva, #0x20, dts		@ GPIOn_AFRL    <- AF val lo  pins, 0-7
	rgrmw	rva, #0x24, cnt		@ GPIOn_AFRH    <- AF val hi  pins, 8-15
	add	rva, rva, #0x0400	@ rva <- next GPIOx_MODER address
	set	pc, lnk

_func_
fmc_wt:	@ wait for FMC ready
	rgwfbt	rva, #0x18, 5, 0	@ wait on FMC_SDSR FMC-ready bit clear
	set	pc,  lnk

fmc_dn:	@ done, finish up
	set	lnk, rvc

  .endif @  STM32F429_Disco

	/* return */
	set	pc,  lnk



