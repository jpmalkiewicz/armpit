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

.ifdef	live_SD

	/* ---------- disable and invalidate L2 cache ---------- */
	mrc	p15, 0, fre, c1, c0, 1	@ fre <- aux ctrl reg cp15 reg 1 content
	bic	fre, fre, #0x02
	set	rvc, 3			@ r12 <- wrt fre to aux ctrl reg smc idx
	smc	#0			@ secure monitor call to disab L2 cache
	@ (initialization pdf, p. 20)
	set	rvc, 1			@ r12 <- clear cache smi call
	smc	#0			@ secure monitor call to invalidate L2

	/* ---------- invalidate L1 caches, TLB, disable MMU ---------- */
	set	rvb, 0
	mcr	p15, 0, rvb, c8, c7, 0	@ invalidate instruction and data TLBs
	mcr	p15, 0, rvb, c7, c5, 0	@ invalidate instruction caches
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- contents of ctrl reg cp15 reg 1
	bic	rvb, rvb, #0x01
	mcr	p15, 0, rvb, c1, c0, 0	@ disable MMU in CP15 register 1

	/* ---------- open-up the L3 interconnect firewall (p. 74) ---------- */
	@ SDRAM Memory Scheduler (SMS)
	write	-1, 0x6c000000, #0x48	@ SMS_RG_ATT0 <- full access (p. 149)
	@ RT, GPMC, OCM RAM
	set	rvb, 0x563E
	set	rvc, 0xFFFF
	set	sv1, 0
	write	rvb, 0x68010000, #0x50	@ RT      L3_PM_READ_PERM,     region 0
	write	rvb, rva, #0x58		@ RT      L3_PM_WRITE_PERM,    region 0
	write	rvc, rva, #0x68		@ RT      L3_PM_REQ_INFO_PERM, region 1
	write	sv1, rva, #0x80		@ RT      L3_PM_ADDR_MATCH,    region 1
	add	rva, rva, #0x2400	@ rva <- L3_PM_GPMC base
	write	rvc, rva, #0x48		@ GPMC    L3_PM_REQ_INFO_PERM, region 0
	write	rvb, rva, #0x50		@ GPMC    L3_PM_READ_PERM,     region 0
	write	rvb, rva, #0x58		@ GPMC    L3_PM_WRITE_PERM,    region 0
	add	rva, rva, #0x0400	@ rva <- L3_PM_OCM_RAM base
	write	rvc, rva, #0x48		@ OCM RAM L3_PM_REQ_INFO_PERM, region 0
	write	rvb, rva, #0x50		@ OCM RAM L3_PM_READ_PERM,     region 0
	write	rvb, rva, #0x58		@ OCM RAM L3_PM_WRITE_PERM,    region 0
	write	sv1, rva, #0xa0		@ OCM RAM L3_PM_ADDR_MATCH,    region 2

	/* ---------- turn clock functions/interfaces on or off ---------- */
	@ MMC1,SCM,SDRC in CORE domain, GPIO1 in WAKE-UP, GPIO5 in PER domain
	write	1<<24, CORE_CM_base, #F_clck	@ cm_fclken1_core <-MMC1 Fclk
	write	 0x42, rva,          #I_clck	@ cm_iclken1_core <-SCM,SDRC Ick
	write	 1<<3, WKUP_CM_base, #F_clck	@ cm_fclken_wkup  <- GPIO1 Fclk
	write	  rvb, rva,          #I_clck	@ cm_iclken_wkup  <- GPIO1 Iclk
	write	1<<16, PER_CM_base,  #F_clck	@ cm_fclken_per   <- GPIO5 Fclk
	write	  rvb, rva,          #I_clck	@ cm_iclken_per   <- GPIO5 Iclk

	/* ---------- initialize clocks in prcm ---------- */
	write	3, 0x48306d00, #0x40	     	@ _clksel <- OSC_SYS_CLK=26MHz
	rgrmw	0x48307200, #0x70, 0xc3, 0x80	@ _clksrc_ctrl <-SYS_CK=OSCCK/2

	/* ---------- configure DPLL3: CORE DPLL (for SDRAM) ---------- */
	rgcpbf	0x48004d00, #0x00, 0, 3, 0x06	@ cm_clken_pll <- bypass/stop
	rgwfbt	rva, #0x20, 0, 0		@ wait for bypass/stop bit
	write	PLL3_parms, rva, #0x40		@ cm_*_pll <- M2,M,N=2xL3f,4xL4f
  .ifndef TI_Beagle_XM
	rgcpbf	rva, #0x00, 0, 8, 0x37		@ cm_clken_pll <- lock (fsel=3)
  .else
	rgcpbf	rva, #0x00, 0, 4, 0x07		@ cm_clken_pll <- lock (no fsel)
  .endif
  	rgwfbt	rva, #0x20, 0, 1		@ wair for lock bit

	/* ---------- configure SDRAM ---------- */
	@ configure sdrc pads for POP RAM
	r16rmw	SCM_base+0x0200,#0x62,0x107,xxx @ cke0 <- output, pullup, mode 0
  .ifdef configure_CS1
	r16rmw	rva, #0x64, 0x107, xxx		@ cke1 <- output, pullup, mode 0
  .endif
	@ configure sdrc controller for external POP ram
	write	2, 0x6d000000, #0x10		@ reset sdrc
	rgwfbt	rva, #0x14, 0, 1		@ wait for rest bit
	write	0, rva, #0x10			@ de-assert reset (sysconfig)
	write	0x0100, rva, #0x44		@ set sharing mode
  .ifndef configure_CS1
	@ SDRC_MCFG: RAS=13/14,CAS=10,128/256MB/bnk,rw-bnk-cl,32b,mobeDD,DpPwrDn
	write	SDRC_MCFG, rva, #0x80		@ sdrc_mcfg_0 <- DDR cfg
	write	SDRC_ACTIM_A, rva,#0x9c		@ sdrc_actim_ctrla_0
	set	rvc, SDRC_ACTIM_B
	write	rvc, rva, #0xa0			@ sdrc_actim_ctrlb_0
	@ SDRC_RFR_CTRL: 1294 or 1560 (#x4dc,#x5e6+50)->7.8us/6,5ns(166/200 MHz)
	write	SDRC_RFR_CTRL,rva,#0xa4		@ sdrc_rfr_ctrl_0 <- refresh ctl
	write	0x81, rva, #0x70		@ sdrc_power    <- power POP sys
	write	0x00, rva, #0xa8		@ sdrc_manual_0 <- NOP command
	wait	0x040000			@ wait a bit
	@ issue SDRAM commands (JEDEC)
	write	  1, rva, #0xa8			@ sdrc_manual_0 <- precharge
	write	  2, rva, #0xa8			@ sdrc_manual_0 <- auto-refresh
	write	rvb, rva, #0xa8			@ sdrc_manual_0 <- auto-refresh
	write	0x32, rva, #0x84		@ sdrc_mr_0     <- CAS 3 Burst 4
  .else @ configure_CS1
	write	configure_CS1>>7,rva,#0x40 	@ sdrc_cs_cfg <- CS1 start
	@ SDRC_MCFG: RAS=13/14,CAS=10,128/256MB/bnk,rw-bnk-cl,32b,mobeDD,DpPwrDn
	write	SDRC_MCFG, rva, #0x80		@ sdrc_mcfg_0 <- DDR cfg
	write	rvb, rva, #0xb0			@ sdrc_mcfg_1 <- DDR cfg
	write	SDRC_ACTIM_A, rva,#0x9c		@ sdrc_actim_ctrla_0
	set	rvc, SDRC_ACTIM_B
	write	rvc, rva, #0xa0			@ sdrc_actim_ctrlb_0
	write	rvb, rva, #0xc4			@ sdrc_actim_ctrla_1
	write	rvc, rva, #0xc8			@ sdrc_actim_ctrlb_1
	@ SDRC_RFR_CTRL: 1294 or 1560 (#x4dc,#x5e6+50)->7.8us/6,5ns(166/200 MHz)
	write	SDRC_RFR_CTRL,rva,#0xa4		@ sdrc_rfr_ctrl_0 <- refresh ctl
	write	rvb, rva, #0xd4			@ sdrc_rfr_ctrl_1 <- refresh ctl
	write	0x81, rva, #0x70		@ sdrc_power    <- power POP sys
	write	0x00, rva, #0xa8		@ sdrc_manual_0 <- NOP command
	write	 rvb, rva, #0xd8		@ sdrc_manual_1 <- NOP command
	wait	0x040000			@ wait a bit
	@ issue SDRAM commands (JEDEC)
	write	  1, rva, #0xa8			@ sdrc_manual_0 <- precharge
	write	rvb, rva, #0xd8			@ sdrc_manual_1 <- precharge
	write	  2, rva, #0xa8			@ sdrc_manual_0 <- auto-refresh
	write	rvb, rva, #0xd8			@ sdrc_manual_1 <- auto-refresh
	write	rvb, rva, #0xa8			@ sdrc_manual_0 <- auto-refresh
	write	rvb, rva, #0xd8			@ sdrc_manual_1 <- auto-refresh
	write	0x32, rva, #0x84		@ sdrc_mr_0     <- CAS 3 Burst 4
	write	 rvb, rva, #0xb4		@ sdrc_mr_1     <- CAS 3 Burst 4
  .endif @ configure_CS1
	@ configure dlla
  .ifndef TI_Beagle_XM
	write	0x0a, rva, #0x60		@ sdrc_dlla_ctrl <- dlla,phase72
  .else
	write	0x08, rva, #0x60		@ sdrc_dlla_ctrl <- dlla,phase90
  .endif
	wait	0x080000			@ wait a bit

.endif	@ live_SD

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
ttbst0:	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
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
	eors	rvc, rvc, #(RAMTOP>>20)	@ 96MB/128MB/512MB
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
	dsb				@ wait for data transfer to complete

	/* ---------- enable L2 cache and NEON/VFP if needed ---------- */
.ifndef	hardware_FPU
	@ enable L2 cache
	set	rvb, 0x52		@ L2 enable, speculative, Cp15 inval
	mcr	p15, 0, rvb, c1, c0, 1	@ CP15 auxctlreg <- L2 ena,spcl,CP15invl
.else
	@ enable L2 cache and NEON/VFP
	set	rvb, 0x72		@ L2 ena,NEON Cache ena,specul,CP15 invl
	mcr	p15, 0, rvb, c1, c0, 1	@ set sel in CP15 aux control register
	set	rvb, 0xf00000		@ enable VFP and NEON coprocessors
	mcr	p15, 0, rvb, c1, c0, 2	@ set sel in CP15 copro access ctl reg
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



