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

/* ==========================================================================
Out of reset and ROM:
  NSACR = 0x00020C00	(mrc p15, 0, rva, c1, c1, 2; NonSecure Access Ctrl Reg)
  => TL   bit set: can allocate lockable TLB entries in non-secure state
     CP11 bit set: can access coprocessor 11 in non-secure state
     CP10 bit set: can access coprocessor 10 in non-secure state
  ACTLR = 0x00000041	(mrc p15, 0, rva, c1, c0, 1; Auxiliary Control Register)
  => SMP bit set: take part in coherency
     FW  bit set: broadcast cache and TLB maintenance ops
 ============================================================================ */

	/* jump to cpu0 or cpun (n > 0) startup */
	b	cpu0go			@ jump to cpu0 startup
	b	cpungo			@ jump to cpun (n > 0) startup

cpungo: /* cpun (n > 0) startup */
	mrc	p15, 0, rvc, c0, c0, 5	@ rvc <- Multiproc Affinity reg, MPIDR
	tst	rvc, #3			@ self-identify as cpu1?
	beq	cpungo			@	if not, jump back to retry
	b	cpu_go			@ jump to finish up

cpu0go: /* cpu0 startup */
	/* disable L2 cache (PL310) -- Section 27.5 Services for HLOS Support */
	set	fre, 0			@ fre <- disable L2 cache
	set	rvc, 0x102		@ r12 <- wrt fre/r0 to L2 cache ctrl reg
	smc	#1			@ secure monitor call: disable L2 cache
	dsb
	rgwfbt	0x48242100, #0, 0, 0	@ wait for PL310 disabled (Control reg)
	isb
	set	fre, 0			@ fre <- start address
	set	cnt, -1			@ cnt <- size
	set	rvc, 0x101		@ r12 <- clean L2 from fre to cnt
	smc	#1			@ secure monitor call: clean L2 cache
	dsb

	/* enable L2 cache (PL310) -- Section 27.5 Services for HLOS Support */
	set	fre, 1			@ fre <- enable L2 cache
	set	rvc, 0x102		@ r12 <- wrt fre/r0 to L2 cache ctrl reg
	smc	#1			@ secure monitor call: enable L2 cache
	dsb
	rgwfbt	0x48242100, #0, 0, 1	@ wait for PL310 enabled (Control reg)
	isb

	/* ---------- set common values ---------- */
	set	fre, 0
	set	sv1, 1
	set	sv2, 2
	set	sv3, 3
	set	sv4, 4

	/* ---------- invalidate L1 caches, TLB, disable MMU ---------- */
	mcr	p15, 0, fre, c8, c7, 0	@ invalidate instruction and data TLBs
	mcr	p15, 0, fre, c7, c5, 0	@ invalidate instruction caches
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- contents of ctrl reg cp15 reg 1
	bic	rvb, rvb, #0x01
	mcr	p15, 0, rvb, c1, c0, 0	@ disable MMU in CP15 register 1
	isb
	dsb

	/* enable the snoop control unit */
	write	fre, 0x48240000, #0x00

	/* set system clock to 38.4 MHz */
	write	0x07, 0x4A306110, #0x00	@ CM_SYS_CLKSEL <- 38.4 MHz

	/* ---------- configure CORE DPLL and CORE, L3 & L4 clocks ---------- */
	@ set CORE, L3 and L4 clock sources (opp100, p.410, p.798)
	write	0x0110, 0x4A004100, #0	@ CM_CLKSEL_CORE<-CORX2,L3=COR/2,L4=L3/2
	write	sv4,    rva, #0x20	@ CM_CLKMODE_DPLL_CORE <- bypass
	rgwfbt	rva, #0x24, 8, 1	@ wait for bypass in CM_IDLEST_DPLL_CORE
	write	fre,    rva, #0x28	@ CM_AUTOIDLE_DPLL_CORE <- no idle
	write	0x7d05, rva, #0x2c	@ CM_CLKSEL_DPLL_CORE <- M=0x7d, N=0x05
	write	0x0101, rva, #0x30	@ CM_DIV_M2_DPLL_CORE <- M2=0x01,no idle
	write	0x0105, rva, #0x34	@ CM_DIV_M3_DPLL_CORE <- M3=0x05,no idle
	write	0x0108, rva, #0x38	@ CM_DIV_M4_DPLL_CORE <- M4=0x08,no idle
	write	0x0104, rva, #0x3c	@ CM_DIV_M5_DPLL_CORE <- M5=0x04,no idle
	write	0x0106, rva, #0x40	@ CM_DIV_M6_DPLL_CORE <- M6=0x06,no idle
	write	0x0105, rva, #0x44	@ CM_DIV_M7_DPLL_CORE <- M7=0x05,no idle

	/* ---------- configure SDRAM ---------- */
	@ MICRON FBGA/POP:D9QQG = MT42L128M64D2LL-25 WT
	@ enable clocks (around p. 927)
	write	 sv2, 0x4A008A00,#0x100	@ CM_MEMIF_CLKSTCTRL        <- enable
	write	 sv1, rva, #0x0130	@ CM_MEMIF_EMIF_1_CLKCTRL   <- enable
	write	 sv1, rva, #0x0138	@ CM_MEMIF_EMIF_2_CLKCTRL   <- enable
	write	1<<8, rva, #0x0140	@ CM_MEMIF_DLL_CLKCTRL      <- ena altck
	@ configure LPDDR2 pads, p. 4236
	write	0x7c7c7c7c,0x4A100638,#0 @CONTROL_LPDDR2IO1_0 <- slew=1,drv12,pd
	write	rvb, rva, #0x10		@ CONTROL_LPDDR2IO2_0 <- slew=1,drv12,pd
	write	rvb, rva, #0x04		@ CONTROL_LPDDR2IO1_1 <- slew=1,drv12,pd
	write	rvb, rva, #0x14		@ CONTROL_LPDDR2IO2_1 <- slew=1,drv12,pd
	bic	rvb, rvb, #(0x3 << 17)	@ rvb <- Errata ID i736: no-pu/pd
	write	rvb, rva, #0x08		@ CONTROL_LPDDR2IO1_2 <- slew=1,drv12
	write	rvb, rva, #0x18		@ CONTROL_LPDDR2IO1_2 <- slew=1,drv12
	write	0xa0888c00, rva, #0x0c	@ CONTROL_LPDDR2IO1_3<-dis vrf_ca,tap0/1
	write	rvb, rva, #0x1c		@ CONTROL_LPDDR2IO2_3<-dis vrf_ca,tap0/1
	@ override EFUSE_2 values, Errata ID: i684
	write	0x004e4000,0x4A100704,#0 @ CONTROL_EFUSE_2 <- i684
	@ configure DMM, for EMIF1, no interleaving (CS0) p.3259
	write	0x80640300,0x4E000000,#0x4c @ DMM_LISA_MAP_3<-1GB,EMIF12,128Btlv
	orr	rvb, rvb, #(1<<30)	@ rvb <- 1GB, above existing RAM
	write	rvb, rva, #0x48		@ DMM_LISA_MAP_2            <- val
	write	fre, rva, #0x44		@ DMM_LISA_MAP_1            <- unmapped
	write	fre, rva, #0x40		@ DMM_LISA_MAP_0            <- unmapped
	@ configure EMIF1 and EMIF2 - CS0 
	ldr	rva, =0x4C000000	@ rva <- EMIF1 base	
emifcf:	@ configure EMIFn (address in rva) - CS0
	write	fre, rva, #0x38		@ EMIF_PWR_MGMT_CTRL        <- no mgmt
	write	fre, rva, #0x3c		@ EMIF_PWR_MGMT_CTRL_SHDW   <- id
	@ LPDDR2-S4,CL6,14rws,8bnks,10col
	write	0x80001ab2, rva, #0x08	@ EMIF_SDRAM_CONFIG <- val (p.3326)
	write	0x849FF408, rva, #0xe4	@ EMIF_DDR_PHY_CTRL_1       <- val
	write	rvb,        rva, #0xe8	@ EMIF_DDR_PHY_CTRL_1_SHDW  <- val
	write	fre,        rva, #0x50	@ EMIF_LPDDR2_MODE_REG_CFG  <- MR0 INIT
	rgwfbt	rva, #0x40, 0, 0	@ wait for bit EMIF_LPDDR2_MODE_REG_DATA
	write	  10,       rva, #0x50	@ EMIF_LPDDR2_MODE_REG_CFG  <- MR10 ZQ
	write	0xff,       rva, #0x40	@ EMIF_LPDDR2_MODE_REG_DATA <- calibrate
	wait	1<<20			@ wait a bit (countdown from 1M to 0)
	write	sv1,        rva, #0x50	@ EMIF_LPDDR2_MODE_REG_CFG  <- MR1 BURST
	write	0x83,       rva, #0x40	@ EMIF_LPDDR2_MODE_REG_DATA <- nWR6,BL8
	write	sv2,        rva, #0x50	@ EMIF_LPDDR2_MODE_REG_CFG  <- MR2 LATEN
	write	sv4,        rva, #0x40	@ EMIF_LPDDR2_MODE_REG_DATA <- RL=6,WL=3
	write	0x40000010, rva, #0x50	@ EMIF_LPDDR2_MODE_REG_CFG  <- MR16
	write	fre,        rva, #0x40	@ EMIF_LPDDR2_MODE_REG_DATA <- enab bnks
	write	0x000501FF, rva, #0x98	@ EMIF_READ_IDLE_CTRL       <- len=5,max
	write	rvb,        rva, #0x9c	@ EMIF_READ_IDLE_CTRL_SHDW  <- val
	@ WTR3,DDR4,RC63,RAS17,WR6,RCD7,RD8, CKE=3,RTP=3,XSRD=XSNR=88,XP=3
	write	0x0ecb065a, rva, #0x18	@ EMIF_SDRAM_TIM_1          <- val
	write	rvb,        rva, #0x1c	@ EMIF_SDRAM_TIM_1_SHDW     <- val
	write	0x205715d2, rva, #0x20	@ EMIF_SDRAM_TIM_2          <- val
	write	rvb,        rva, #0x24	@ EMIF_SDRAM_TIM_2_SHDW     <- val
	@ RASmx17,RFC84,DQSCKmx3,ZQCS36,CKESR6, refresh each 3.9us, at 400MHz
	write	0x00b1c53f, rva, #0x28	@ EMIF_SDRAM_TIM_3          <- val
	write	rvb,        rva, #0x2c	@ EMIF_SDRAM_TIM_3_SHDW     <- val
	write	0x00000618, rva, #0x10	@ EMIF_SDRAM_REF_CTRL       <- val
	write	rvb,        rva, #0x14	@ EMIF_SDRAM_REF_CTRL_SHDW  <- val
	write	0x500b3215, rva, #0xc8	@ EMIF_ZQ_CONFIG            <- CS0 ZQ
	@ configure the second EMIF or continue
	set	rvb, rva		@ rvb <- EMIFn (the one just configured)
	set	rva, 0x4D000000		@ rva <- EMIF2 base
	eq	rva, rvb		@ was EMIF2 just configured?
	bne	emifcf			@	if not, jump to configure EMIF2
	@ lock the core PLL now that SDRAM is initialized
	write	0xf0d,0x4A004100,#0x160	@ CM_SHADOW_FREQ_CONFIG1 <- M2=0x01
	rgwfbt	rva, #0x160, 0, 0	@ wait for bitclr CM_SHADOW_FREQ_CONFIG1
	rgwfbt	rva,  #0x24, 0, 1	@ wait for lock in CM_IDLEST_DPLL_CORE
	@ no more DLL override
	write	fre, 0x4A004100, #0x10	@ CM_DLL_CTRL <- no override
	wait	1<<20			@ wait a bit (countdown from 1M to 0)
	@ wait for EMIF PHY ready, then reset PHYs -- Errata ID: i615, i682
	rgwfbt	0x4C000000, #0x04, 2, 1	@ wait for EMIF1 ready in EMIF_STATUS
	rgcpbt	rva, #0x60, 10, 1	@ EMIF1 <- reset
	rgwfbt	0x4D000000, #0x04, 2, 1	@ wait for EMIF2 ready in EMIF_STATUS
	rgcpbt	rva, #0x60, 10, 1	@ EMIF2 <- reset
	@ set data mode on CS0 and MEMIF Clock Domain -> HW_AUTO
	write	fre, 0x80000000, #0x00	@ data mode on CS0
	write	sv3, 0x4A008A00, #0x100	@ CM_MEMIF_CLKSTCTRL <- enable

	/* ---------- copy scheme code to SDRAM ---------- */
	ldr	sv5, = _text_section_address_	@ start of source
	set	env, SDRAMBOTTOM		@ start of destination
	ldr	dts, = _boot_section_address_	@ start of boot
	add	dts, dts, #boot_ram_size	@ end of source
codcp0:	ldmia	sv5!, {fre-sv4}
	stmia	env!, {fre-sv4}
	cmp	sv5, dts
	bmi	codcp0

	/* ---------- initialize TTB (Translation Table) ---------- */
	@ Default Memory space (not cache, not buffer, shared)
	set	rvc, 0x0C02		@ rvc <- r/w,dmn 0,no cach/buf,1MB,share
	set	rva, TTB_address	@ rva <- address of start of TTB
	set	rvb, rvc		@ rvb <- section 0 descriptor
@ttbst0:	str	rvb, [rva, rvb, LSR #18] @ store direct mapped sect descr in TTB
ttbst0:	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
	add	rvb, rvb, #(1<<20)
	eq	rvb, rvc
	bne	ttbst0
	@ continue TTB init, for Scheme core and SDRAM (cache, buffer, shared)
	set	rvc, 0x010C0E		@ rvc <- r/w,dmn 0,cache/buf,1MB,share
	@ on-chip RAM direct mapped
	ldr	rvb, =(_boot_section_address_ & 0xfff00000)
	orr	rvb, rvb, rvc
	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
	@ SDRAM (cacheable, buffered)
	orr	rvb, rvc, #SDRAMBOTTOM
	write	rvb, rva, #0		@ virt adr 0 = phys adr rambttm for code
ttbst1:	write	rvb, rva, rvb, LSR #18	@ store direct mapped sect descr in TTB
	add	rvb, rvb, #(1<<20)
	lsr	rvc, rvb, #20
	eors	rvc, rvc, #(SDRAMTOP>>20) @ 1024MB
	bne	ttbst1

cpu_go:	@ startup code common to all cpus

	/* ---------- set domain access control and TTB base ---------- */
	set	rva, TTB_address	@ rva <- address of start of TTB
	set	rvb, 0x01		@ rvb <- dmn 0 client acc perms,A&P bits
	mcr	p15, 0, rvb, c3, c0, 0	@ set domain access into CP15 register 3
	orr	rva, rva, #0x1a		@ TTB 0 outr cach wrt-bk,no alo wrt,shr
	mcr	p15, 0, rva, c2, c0, 0	@ set TTB 0 base adrs in CP15 reg 2
	mcr	p15, 0, rva, c2, c0, 1	@ set TTB 1 base adrs in CP15 reg 2 (id)

	/* ---------- enable L1-I, L1-D caches, MMU ---------- */
	mrc	p15, 0, rvb, c1, c0, 0	@ rvb <- sys ctrl reg (cp15 reg. 1)
	bic	rvb, rvb, #0x0002	@ rvb <- alignment bit cleared (b1)
	orr	rvb, rvb, #0x1800	@ rvb <- Icache ena-b12, flopred ena-b11
	orr	rvb, rvb, #0x0005	@ rvb <- Dcache ena-b2,  MMU ena-b0
	mcr	p15, 0, rvb, c1, c0, 0	@ SCTLR <- cach/MMU ena in sys ctrl reg
	isb				@ wait for instructions to complete
	dsb				@ wait for data transfere to complete

	/* ---------- enable VFP, NEON ---------- */
	set	rvb, 0xf<<20		@ rvb   <- enab VFP, NEON (cp10, cp11)
	mcr	p15, 0, rvb, c1, c0, 2	@ CPACR <- enab in coproc accss ctrl reg
	isb				@ instruction memory barrier
	set	rvb, 1<<30
	fmxr	fpexc, rvb		@ enable VFP/NEON (bit 30 in FPEXC)
	vmrs	rvb, fpscr
	orr	rvb, rvb, #0x00c00000	@ rounding mode = towards 0 (truncate)
	vmsr	fpscr, rvb

	/* ---------- jump to remainder of initialization ---------- */
	ldr	pc, =_start



