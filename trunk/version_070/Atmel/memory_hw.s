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
memcfg:	/* configure external SDRAM */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- PMC_base
	@ ret:	via lnk

  .ifdef SAM4S_XPLD
    .ifdef external_ram
	@ configure PC0-8, PC11, PC14-15, PC18-31 to MEM function (peripheral A)
	mask	rvc, 0, 1, 9,10,12,13,16,17	@ rvc <- 1-bit inverted pin mask
	mvn	rvc, rvc			@ rvc <- 1-bit MEM pin mask
	rgrmw	pioc_base, #0x04, rvc		@ PIOn_PDR     <- Disab GPIO
	rgrmw	      rva, #0x70, rvc, xxx	@ PIOn_ABCDSR1 <- MEM func
	rgrmw	      rva, #0x74, rvc 		@ PIOn_ABCDSR2 <- MEM func
	@ configure PA0-1 (A17-18) & PA18-20 (A14-16) to MEM func (peripheral C)
	mask	rvc, 0, 1, 0,1,18,19,20		@ rvc <- 1-bit MEM pin mask
	rgrmw	pioa_base, #0x04, rvc		@ PIOn_PDR     <- Disab GPIO
	rgrmw	      rva, #0x70, rvc, xxx	@ PIOn_ABCDSR1 <- MEM func
	rgrmw	      rva, #0x74, rvc		@ PIOn_ABCDSR2 <- MEM func
	@ power-up the static memory controller
	write	(1<<10), env, 0x10		@ PMC_PCER0 <- Enab SMC clk/pwr
	@ give 50 us for ISSI chip to self-initialize
	wait	0x1800
	@ set SMC timing for SRAM chip (ISSI 66WV51216DBLL, 55ns)
	write	SMC_SETUP, SMC_base, #0	@ SMC_SETUP0 <- setup time
	write	      rvb, rva, #0x10	@ SMC_SETUP1 <- setup time (idem)
	write	SMC_PULSE, rva, #0x04	@ SMC_PULSE0 <- { rd = 7x8ns = 56ns
	write	      rvb, rva, #0x14	@ SMC_PULSE1 <- { wr = 6|8x8ns = 48|64ns
	write	SMC_CYCLE, rva, #0x08	@ SMC_CYCLE0 <- r=7x8=56ns, w=8x8=64ns
	write	      rvb, rva, #0x18	@ SMC_CYCLE1 <- id.
	write	      sv1, rva, #0x0c	@ SMC_MODE0  <- 1, rd:NRD & wr:NCS (LB#)
	write	      rvb, rva, #0x1c	@ SMC_MODE1  <- 1, rd:NRD & wr:NCS (UB#)
    .endif @ external_ram
  .endif   @ SAM4S_XPLD

  .ifdef SAM9_L9261

    .macro ramcmd reg
	write	\reg, rva, #0x00	@ SDRAMC_MR <- cmd
	write	 fre, cnt, #0x00	@ issue command
    .endm

	@ 2 x Samsung K4S561632J-UC75, 32-bit data bus, CAS 2, 100 MHz
	@ (see also LPC2478-STK, LPC-H2888)
	mask	rvb, 0, 1, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
	write	rvb, pioc_base, #0x04	@ PIOC_PDR  <- disab pio func on PC16-31
	write	rvb,       rva, #0x60	@ PIOC_PUDR <- disab pull-up for PC16-31
	write	rvb,       rva, #0x70	@ PIOC_ASR  <- PerA SDRAM D16-31,PC16-31
	set	rvc, 0x102
	rgrmw	0xffffee00, #0x30, rvc	@ EBI_CSA  <- CS1A SDRAM, no p/up, D0-15
	write	0x75227159, 0xffffea00, #0x08 	@ SDRAMC_CR <- 100MHz SDRAM cfg
						@ txsr=7(tras+trp),tras=5(45ns),
						@ trcd=2(20ns),trp=2(20ns),trc=
						@ 7(65ns),twr=1(trdl=1@100MHz),
						@ 32bt,cas2,4bk,13row,9col
	wait	0x010000		@ wait a bit
	set	cnt, 0x20000000		@ cnt <- SDRAM base adrs (eg. RAMBOTTOM)
	ramcmd	sv1			@ SDRAMC_MR <- 1, NOP command
	ramcmd	sv2			@ SDRAMC_MR <- 2, Precharge-All command
	set	rvb, 8
ramlp1:	@ auto-refresh loop
	ramcmd	sv4			@ SDRAMC_MR <- 4, Auto-Referesh command
	subs	rvb, rvb, #1
	bne	ramlp1
	ramcmd	sv3			@ SDRAMC_MR <- 3, Load-Mode-Register cmd
	ramcmd	fre			@ SDRAMC_MR <- 0, Normal-Mode command
	@ set refresh rate to 768x10ns = 7.68us < 64ms/8K = 7.81us
	write	0x0300, rva, #0x04	@ SDRAMC_TR <- refresh at 7.68us
  .endif @ SAM9_L9261

	/* return */
	set	pc,  lnk



