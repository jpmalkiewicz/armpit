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


	/* ---------- invalidate L1 caches, TLB, disable MMU ---------- */
	set	rvb, 0
	mcr	p15, 0, rvb, c8, c7, 0	@ invalidate instruction and data TLBs
	mcr	p15, 0, rvb, c7, c5, 0	@ invalidate instruction caches
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- cntnts of cntrl reg, CP15 reg.1
	bic	rvb, rvb, #0x01
	mcr	p15, 0, rvb, c1, c0, 0	@ disable MMU in CP15 register 1

	/* ---------- enable EMIF module ---------- */
	write	2, PER_CM_base, #0x28		@ CM_PER_EMIF_CLKCTRL <-ena EMIF

	/* ---------- configure DDR PLL (400 MHz) ---------- */
	rgcpbf	WKUP_CM_base, #0x94, 0, 3, 4	@ CM_CLKMODE_DPLL_DDR <- bypass
	rgwfbt	rva, #0x34, 8, 1		@ wait for bypass bit in _IDLEST
	rgcpbf	rva, #0x40, 0, 19, (800<<8)|23	@ CLKSEL_DPLL_DDR <- M=800,N=23
	rgcpbf	rva, #0xa0, 0,  5, 0x02		@ CM_DIV_M2_DPLL_DDR <- M2=2
	rgrmw	rva, #0x94, 0x07		@ CM_CLKMODE_DPLL_DDR <- lock
	rgwfbt	rva, #0x34, 0, 1		@ wait for lock bit in CM_IDLEST

	/* ---------- enable VTP ---------- */
	write	0,   SCM_base+0x0E00, #0x0c	@ VTP_CTRL_REG <- 0, cleared
	write	6,   rva,             #0x0c	@ VTP_CTRL_REG <- 6, filter bits
	rgcpbt	rva, #0x0c, 6, 1		@ VTP_CTRL_REG <- add bit 6
	rgcpbt	rva, #0x0c, 0, 0		@ VTP_CTRL_REG <- CLRZ bit = 0
	rgcpbt	rva, #0x0c, 0, 1		@ VTP_CTRL_REG <- CLRZ bit = 1
	rgwfbt	rva, #0x0c, 5, 1		@ wait on ready bit

	/* ---------- configure SDRAM ---------- */
	@ cfg SDRAM PHY (PHY_Config_CMD, PHY_Config_DATA) (* is _REG_PHY_ below)
	write	0x80, 0x44E12000, #0x1C		@ CMD0*CTRL_SLAVE_RATIO_0 <-0x80
	write	rvb,  rva, #0x50		@ CMD1*CTRL_SLAVE_RATIO_0 <-0x80
	write	rvb,  rva, #0x84		@ CMD2*CTRL_SLAVE_RATIO_0 <-0x80
	write	0x00, rva, #0x2C		@ CMD0*INVERT_CLKOUT_0 	  <- 0
	write	rvb,  rva, #0x60		@ CMD1*INVERT_CLKOUT_0 	  <- 0
	write	rvb,  rva, #0x94		@ CMD2*INVERT_CLKOUT_0 	  <- 0
	add	rvc, rva, #0xA4
	write	0x3A, rva, #0xC8		@ DATA0*RD_DQS_SLAV_RTIO_0<-0x3A
	write	rvb,  rvc, #0xC8		@ DATA1*RD_DQS_SLAV_RTIO_0<-0x3A
	write	0x46, rva, #0xDC		@ DATA0*WR_DQS_SLAV_RTIO_0<-0x46
	write	rvb,  rvc, #0xDC		@ DATA1*WR_DQS_SLAV_RTIO_0<-0x46
	write	0x97, rva, #0x0108		@ DATA0*FIFO_WE_SLV_RTIO_0<-0x97
	write	rvb,  rvc, #0x0108		@ DATA1*FIFO_WE_SLV_RTIO_0<-0x97
	write	0x7C, rva, #0x0120		@ DATA0*WR_DATA_SLV_RTIO_0<-0x7C
	write	rvb,  rvc, #0x0120		@ DATA1*WR_DATA_SLV_RTIO_0<-0x7C
	@ configure IOCTRL
	write	0x18B, SCM_base+0x1400, #0x04	@ DDR_CMD0_IOCTRL  <- 0x018b
	write	rvb, rva, #0x08			@ DDR_CMD1_IOCTRL  <- 0x018b
	write	rvb, rva, #0x0C			@ DDR_CMD2_IOCTRL  <- 0x018b
	write	rvb, rva, #0x40			@ DDR_DATA0_IOCTRL <- 0x018b
	write	rvb, rva, #0x44			@ DDR_DATA1_IOCTRL <- 0x018b
	@ set mode to DDR3 and configure CKE for EMIF/DDR_PHY control
	rgcpbt	SCM_base+0x0E00, #0x04, 28, 0	@ DDR_IO_CTRL <- DDR3 mode
	rgcpbt	SCM_base+0x1300, #0x1C,  0, 1	@ DDR_CKE_CTRL <- EMIF/DDR_PHY
	@ configure timing
	write	7,   0x4C000000, #0xE4		@ EMIF_DDR_PHY_CTRL_1_REG
	write	rvb,        rva, #0xE8		@ EMIF_DDR_PHY_CTRL_1_SHDW_REG
	write	rvb,        rva, #0xEC		@ EMIF_DDR_PHY_CTRL_2_REG
	write	0x0AAAD4DB, rva, #0x18		@ EMIF_SDRAM_TIM_1_REG
	write	rvb,        rva, #0x1C		@ EMIF_SDRAM_TIM_1_SHDW_REG
	write	0x266B7FDA, rva, #0x20		@ EMIF_SDRAM_TIM_2_REG
	write	rvb,        rva, #0x24		@ EMIF_SDRAM_TIM_2_SHDW_REG
	write	0x501F867F, rva, #0x28		@ EMIF_SDRAM_TIM_3_REG
	write	rvb,        rva, #0x2C		@ EMIF_SDRAM_TIM_3_SHDW_REG
	write	0x0C30,     rva, #0x10		@ EMIF_SDRAM_REF_CTRL_REG
	write	rvb,        rva, #0x14		@ EMIF_SDRAM_REF_CTRL_SHDW_REG
	write	0x50074BE4, rva, #0xC8		@ EMIF_ZQ_CONFIG_REG
	write	0x61C05332, rva, #0x08		@ EMIF_SDRAM_CONFIG_REG

	/* ---------- copy scheme code to SDRAM ---------- */
	ldr	sv5, = _text_section_address_	@ start of source
	set	env, RAMBOTTOM			@ start of destination
	ldr	dts, = _boot_section_address_	@ start of boot
	add	dts, dts, #boot_ram_size	@ end of source
codcp0:	ldmia	sv5!, {fre-sv4}
	stmia	env!, {fre-sv4}
	cmp	sv5, dts
	bmi	codcp0

	/* ---------- initialize TTB (Translation Table) ---------- */
	@ Default Memory space (not cacheable, not buffered)
	set	rvc, 0x0C02		@ rvc <- r/w,dmn 0,no cach/buf,1MB sect.
	set	rva, TTB_address	@ rva <- address of start of TTB
	set	rvb, rvc		@ rvb <- section 0 descriptor
ttbst0:	str	rvb, [rva, rvb, LSR #18] @ store direct mapped sect descr in TTB
	add	rvb, rvb, #(1<<20)
	eq	rvb, rvc
	bne	ttbst0
	@ continue TTB init, for Scheme core and SDRAM (cacheable, buffered)
	set	rvc, 0x0C0E		@ rvc <- r/w,dmn 0,cache/buf,1MB sect.
	@ on-chip RAM direct mapped
	ldr	rvb, =(_boot_section_address_ & 0xfff00000)
	orr	rvb, rvb, rvc
	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
	add	rvb, rvb, #(1<<20)
	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
	@ SDRAM (cacheable, buffered)
	orr	rvb, rvc, #RAMBOTTOM
	write	rvb, rva, #0		@ virt adr 0 = phys adr rambttm for code
ttbst1:	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
	add	rvb, rvb, #(1<<20)
	lsr	rvc, rvb, #20
	eors	rvc, rvc, #(RAMTOP>>20)	@ 512MB
	bne	ttbst1

	/* ---------- set domain access control and TTB base ---------- */
	set	rvb, 0x01		@ rvb <- dmn 0 clnt accs prms (A&P bits)
	mcr	p15, 0, rvb, c3, c0, 0	@ set domain access into CP15 register 3
	orr	rva, rva, #0x18		@ TTB 0 out cach,wrt-bck,no alloc on wrt
	mcr	p15, 0, rva, c2, c0, 0	@ CP15 reg 2 <- TTB 0 adrs
	mcr	p15, 0, rva, c2, c0, 1	@ CP15 reg 2 <- TTB 1 adrs (id TTB 0)

	/* ---------- enable L1-I, L1-D caches, MMU ---------- */
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- cont of ctrl reg (CP15 reg 1)
	bic	rvb, rvb, #0x0002	@ rvb <- alignment bit cleared (b1)
	orr	rvb, rvb, #0x1800	@ rvb <- Icach ena b12,flow pred ena b11
	orr	rvb, rvb, #0x0005	@ rvb <- Dcach ena b2, MMU ena b0
	mcr	p15, 0, rvb, c1, c0, 0	@ CP15 reg 1 <- L1 cache/MMU enable
	isb				@ wait for instructions to complete
	dsb				@ wait for data transfere to complete

	/* ---------- enable L2 cache and NEON/VFP if needed ---------- */
.ifndef	hardware_FPU
	@ enable L2 cache
	set	rvb, 0x52		@ L2 enable, speculative, Cp15 inval
	mcr	p15, 0, rvb, c1, c0, 1	@ CP15 aux ctrl <- L2 ena,spcl,CP15 invl
.else
	@ enable L2 cache and NEON/VFP
	set	rvb, 0x72		@ L2 ena NEON Cache ena specul,CP15 invl
	mcr	p15, 0, rvb, c1, c0, 1	@ set sel in CP15 aux control register
	set	rvb, 0xf00000		@ enable VFP and NEON coprocessors
	mcr	p15, 0, rvb, c1, c0, 2	@ set sel in CP15 coproc access ctl reg
	set	r1, 0
	isb				@ instruction memory barrier
	set	rvb, 0x40000000
	fmxr	fpexc, rvb		@ enable VFP/NEON (bit 30 in FPEXC)
	vmrs	rvb, fpscr
	orr	rvb, rvb, #0x00c00000	@ rounding mode = towards 0 (truncate)
	vmsr	fpscr, rvb
.endif

	/* ---------- jump to remainder of initialization ---------- */
	ldr	pc, =_start



