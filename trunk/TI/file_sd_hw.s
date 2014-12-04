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
@	SD card low-level interface, SPI and MMC/MCI
@
@	Targets:
@
@		LM_3Sxxxx, LM_4Fxxx
@		OMAP_35xx, OMAP_44xx, AM335x
@
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@
@	SPI Interface (Cortex-M3, Cortex-M4)
@
@	Code Entry Points:
@
@		sd_cfg		configure hardware
@		sd_fst		configure and set high speed
@		sd_slo		set slow speed
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_spi

_func_
sd_cfg:	@ configure spi power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- rcgc_base = peripheral RCGC base adrs
	@ ret:	via lnk (through sd_slo)
	@ identify whether SSI0 or SSI1 is used (set eq/ne flag)
	@   SSI0: PA.2,4,5 (port A unlckd in UART init)
	@         CS on gpio A or D (eg. PD0 on EVB_LM3S6965)
	@   SSI1: PE.0,2,3, CS on gpio E (eg. PE1 on IDM_LM3S1958)
  .ifdef LM_3S1000
	@ enable SSI and GPIO clocks
    .ifndef sd_is_on_ssi1
	rgcpbt	env, #0x04, 4, 1	@ RCGC1    <- enab clk for SSI0
	rgrmw	env, #0x08, 0x09	@ RCGC2    <- enab clk for Port A & D
    .else
	rgcpbt	env, #0x04, 5, 1	@ RCGC1    <- enab clk for SSI1
	rgcpbt	env, #0x08, 4, 1	@ RCGC2    <- enab clk for Port E
    .endif
	wait	4			@ wait a bit
  .endif
  .ifdef LM_4Fxxx
	@ enable SSI and GPIO clocks
	write	sv1, env, #0x1c		@ RCGCSSI  <- enab clk for SSI0
	rgwfbt	glv, #0x1c, 0, 1	@ wait for periph clk bit set in PR_SSI
  .endif
	@ configure chip-select pin and de-select card
	rgcpbt	sd_cs_gpio, #0x400, sd_cs_pin,1 @ GPIODIR  <- PA3|PE1 = output
	add	rvc, rva, #0x500	 	@ rvc <- sd_cs_gpio + 0x500
	rgcpbt	rvc, #0x1c, sd_cs_pin, 1 	@ GPIODEN  <- PA3|PE1 = digital
	rgcpbt	rvc, #0x10, sd_cs_pin, 1 	@ GPIOPUR  <- PA3|PE1 = pull-up
	write	0xff, rva, #((1<<sd_cs_pin)<<2)	@ de-select SD card (PE1 high)
	@ configure SSI pins
  .ifndef sd_is_on_ssi1
	rgrmw	sd_spi_gpio+0x500, #0x1c, 0x34	@ GPIODEN   <- PA2,4,5 digital
	rgrmw	rva, #0x10, 0x34		@ GPIOPUR   <- PA2,4,5 wk pulup
	sub	rva, rva, #0x0100
	rgrmw	rva, #0x20, 0x34		@ GPIOAFSEL <- PA2,4,5 SSI
  .else
	rgrmw	sd_spi_gpio+0x500, #0x1c, 0xd	@ GPIODEN   <- PE0,2,3 digital
	rgrmw	rva, #0x10, 0x0d		@ GPIOPUR   <- PE0,2,3 wk pulup
	sub	rva, rva, #0x0100
	rgrmw	rva, #0x20, 0x0d		@ GPIOAFSEL <- PE0,2,3 SSI
  .endif
  .ifdef EK_TM4C1294
	rgrmw	rva, #0x2c, 0x220200		@ GPIOPCTL <- PA2,4,5 = SSI func
  .endif
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_	
sd_fst:	@ configure spi speed (high), phase, polarity
	write	  0x00, sd_spi, #0x04	@ SSI0CR1  <- disable SSI1
	write	0x0507, rva, #0x00	@ SSI0CR0  <- PHA/POL=0,SPImod,SCR 3,fst
	write	  0x02, rva, #0x10	@ SSI0CPSR <- set prescale to 2
	write	   rvb, rva, #0x04	@ SSI0CR1  <- enable SSI1
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	write	  0x00, sd_spi, #0x04	@ SSI0CR1  <- disable SSI1
	write	0x9607, rva, #0x00	@ SSI0CR0  <- PHA/POL=0,SPImod,SCR 3,slo
	write	  0x02, rva, #0x10	@ SSI0CPSR <- set prescale to 2
	write	   rvb, rva, #0x04	@ SSI0CR1  <- enable SSI1
	set	pc,  lnk


.endif	@ .ifdef sd_is_on_spi

/*------------------------------------------------------------------------------
@
@	MMC/MCI Interface (Cortex-A8, Cortex-A9)
@
@	Code Entry Points:
@
@		sd_cfg		configure MMC/MCI
@		_sgbrs		get data block from SD card
@		_spbrs		write data block to SD card
@		sd_fst		set high speed
@		sd_slo		set slow speed
@		sd_cmd		send command
@		sdpcmd		send command during initialization
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_mci

sd_cfg:	/* configure MMC/MCI for SD */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ in:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ in:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ in:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ in:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ in:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ in:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ in:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk
    .ifdef AM335x	/* AM335x */
	@ enable MMC0 module
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ
	write	sv2, env, #0x3c		@ CM_PER_MMC0_CLKCTRL   <- enable MMC0
	swi	run_no_irq		@ set Thread mode, unprvlgd, no IRQ, usr
    .else
      .ifndef cortex_a9	/* OMAP3 */
	@ ensure MMC1 clocks are enabled (use: dts<-CM_CORE)
	rgcpbt	dts, #F_clck, 24, 1	@ cm_fclken_core <- enable MMC1 Fclk
	rgcpbt	dts, #I_clck, 24, 1	@ cm_iclken_core <- enable MMC1 Iclk
      .else 		/* OMAP4 */
	@ power-up hs-mmc1 with 96MHz clock (use: env <- L3INIT_CM2 base)
	write	0x01000002, env, #0x28	@ CM_L3INIT_HSMMC1_CLKCTRL <- enab 96MHz
	@ disable VMMC_AUTO_OFF in TWL6030 MMCCTRL reg (OMAP4 Errata ID: i705)
	set	sv5, lnk		@ sv5 <- lnk, saved
	ldr	rva, =i2c1_base		@ rva <- I2C1 base address
	hwi2wr	0x00ee48, i2		@ VMMC_AUTO_OFF(#xee) <- disable(#x00)
	@ restore pre-set common values
	set	lnk, sv5		@ lnk <- link, restored
	set	sv3, 3			@ sv3 <- 3,    restored
	set	sv5, 5			@ sv5 <- 5,    restored
      .endif
    .endif
	@ return
	set	pc,  lnk


_sgbrs:	@ sd block read transfer start/restart
	@ in:	rvc <- block or byte address of block to read (raw int)
	@ in:	sv3 <- buffer to store block data (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ out:	sv3 <- updated buffer
	@ mods:	sv3, sv5, rva, rvb, rvc
	bl	sd_pre
	@ 11|+24<cmd17,3|23:20<nrml,dat,idxchk,a|19:16<crc,48b,rsp,1|4<rd
	set	rvb, 0x113a0010
	bl	sd_cmd
	eq	rva, #0
	beq	_sgbrs
sgb_wt:	@ wait for data (loop)
	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	bne	_sgbrs
	tst	rvb, #0x20
	beq	sgb_wt
	orr	rvb, rvb, #0x20
	write	rvb, rva, #0x30
	@ get data
	set	rvc, 0
sgb_cp:	@ data-read loop
	read	rvb, rva, #0x20		@ rvb <- MMCHS_DATA
	write	rvb, sv3, rvc
	add	rvc, rvc, #4
	eq	rvc, #512
	bne	sgb_cp
	rgcpbt	rva, #0x30, 1, 1	@ MMCHS_STAT <- updated
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk


_spbrs:	@ sd block write transfer start/restart
	@ in:	rvc <- block or byte address of block to write (raw int)
	@ in:	sv3 <- buffer w/block data to wrt to sd (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ mods:	sv5, rva, rvb, rvc
	bl	sd_pre
	@ 18|+24<cmd24,3|23:20<nrml,dat,idxchk,a|19:16<crc,48b,rsp,0|4<wr
	set	rvb, 0x183a0000
	bl	sd_cmd
	eq	rva, #0
	beq	_spbrs
	rgwfbt	rva, #0x24, 10, 1	@ wait for card ready
	@ put data
	set	rvc, 0
spb_cp:	@ data-write loop
	read	rvb, sv3, rvc
	write	rvb, rva, #0x20		@ MMCHS_DATA <- data word
	add	rvc, rvc, #4
	eq	rvc, #512
	bne	spb_cp
spb_wt:	@ wait for end of write
	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	readne	rvc, rva, #0x08		@ 	if so,  rvc <- MMCHS_ARG
	bne	_spbrs			@	if so,  jump back to restart
	tst	rvb, #0x02
	beq	spb_wt
	orr	rvb, rvb, #0x02
	write	rvb, rva, #0x30		@ MMCHS_STAT <- clear state
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk


sd_pre:	@ mci-prep subroutine
	read	rvb, sd_mci+0x100,#0x24	@ rvb <- MMCHS_PSTATE
	tst	rvb, #0x07
	beq	sd_pr0
	@ reset MMC data/command lines
	rgcpbt	rva, #0x2c, 25, 1	@ MMCHS_SYSCTL <- update
	rgcpbt	rva, #0x2c, 26, 1
  .ifdef cortex_a9
sd_pr1:	@ wait for reset complete
	read	rvb, rva, #0x2c
	tst	rvb, #(3 << 25)
	bne	sd_pr1
  .endif
sd_pr0:	@ clear previous state
	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	write	rvb, rva, #0x30
	@ set data length
	write	512, rva, #0x04		@ MMCHS_BLK <- 512
	set	pc,  lnk

		
sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	@ on entry: rvc <- arg
	write	rvc, sd_mci+0x100,#0x08	@ set arg in MMCHS_ARG
	write	rvb, rva, #0x0c		@ MMCHS_CMD <- cmd
sd_cm0:	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	setne	rva, 0
	setne	pc,  lnk
	tst	rvb, #0x01
	beq	sd_cm0
	@ clear state, get response, wait for write-ready
	write	0x01, rva, #0x30
	read	rvb,  rva, #0x10	@ rvb <- MMCHS_RSP10
	set	pc,  lnk

sd_slo:	@ configure mci speed (low = 400 KHz), 1-bit bus, clock enabled
	@ (perform reset first)
	@ modifies:	rva, rvb
	rgcpbt	sd_mci, #0x10, 0, 1	@ reset mci peripheral
	rgwfbt	rva,    #0x14, 0, 1	@ wait for reset
	add	rva, rva, #0x100	@ rva <- mci base adrs+0x100 for ldr/str
	rgcpbt	rva, #0x2c, 24, 1	@ reset mci peripheral
	rgwfbt	rva, #0x2c, 24, 0	@ wait for reset
	rgcpbf	rva, #0x28, 9, 12, 6	@ 3-Volt operation
	rgcpbf	rva, #0x2c, 6,20,0x3a58	@ set timeout cntr, 160 KHz clk (#x258)
	rgcpbt	rva, #0x2c, 0, 1	@ MMCHS_SYSCTL <- clock enabled
	rgwfbt	rva, #0x2c, 1, 1	@ wait for clock stable
	rgcpbt	rva, #0x2c, 2, 1	@ MMCHS_SYSCTL <- card clocked
	rgcpbt	rva, #0x28, 8, 1	@ power-up the bus
	write	0x307f0033, rva, #0x34	@ enable events
	@ send initialization stream (2 x CMD0, with init-stream bit set in CON)
	sub	rva, rva, #0x100	@ rva <- mci base address
	rgcpbt	rva, #0x2c, 1, 1	@ set init-stream bit
	add	rva, rva, #0x100	@ rva <- mci base adrs+0x100 for ldr/str
	write	0,   rva, #0x0c		@ MMCHS_CMD <- CMD0
	rgwfbt	rva, #0x30, 0, 1	@ wait for command complete
	write	rvb, rva, #0x30		@ clear command-complete bit
	write	0,   rva, #0x0c		@ MMCHS_CMD <- CMD0
	rgwfbt	rva, #0x30, 0, 1	@ wait for command complete
	write	rvb, rva, #0x30		@ clear command-complete bit
	sub	rva, rva, #0x100	@ rva <- mci base address
	rgcpbt	rva, #0x2c, 1, 0	@ clear init-stream bit
  .ifdef change_sd_clock_after_init_stream
	add	rva, rva, #0x100	@ rva <- mci base adrs+0x100 for ldr/str
	rgcpbt	rva, #0x2c, 2, 0	@ MMCHS_SYSCTL <- card not clocked
	rgcpbt	rva, #0x2c, 0, 0	@ MMCHS_SYSCTL <- clock disabled
	@ change clock freq
	bic	rvb, rvb, #0x0ff00
	bic	rvb, rvb, #0x000c0
	orr	rvb, rvb, #0x03c00	@ 400 KHz
	write	rvb, rva, #0x2c		@ MMCHS_SYSCTL <- new speed
	rgcpbt	rva, #0x2c, 0, 1	@ MMCHS_SYSCTL <- clock enabled
	rgwfbt	rva, #0x2c, 1, 1	@ wait for clock stable
	rgcpbt	rva, #0x2c, 2, 1	@ MMCHS_SYSCTL <- card clocked
  .endif
	set	pc,  lnk

sd_fst:	@ configure mci speed (high = 19 MHz), wide bus, clock enabled
	@ modifies:	rva, rvb
	rgcpbt	sd_mci+0x100, #0x2c,2,0	@ MMCHS_SYSCTL <- card not clocked
	rgcpbt	rva, #0x2c, 0, 0	@ MMCHS_SYSCTL <- clock disabled
	@ change clock freq
	bic	rvb, rvb, #0x0ff00
	bic	rvb, rvb, #0x000c0
	orr	rvb, rvb, #0x00200	@ 12 MHz
	write	rvb, rva, #0x2c		@ MMCHS_SYSCTL <- new speed
	@ enable clock
	rgcpbt	rva, #0x2c, 0, 1	@ MMCHS_SYSCTL <- clock enabled
	rgwfbt	rva, #0x2c, 1, 1	@ wait for clock stable
	rgcpbt	rva, #0x2c, 2, 1	@ MMCHS_SYSCTL <- card clocked
	rgcpbt	rva, #0x28, 1, 1	@ MMCHS_HCTL <- 4-bit bus
	set	pc,  lnk
	
sdpcmd:	@ function to write a command to SD/MMC card during initialization
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	read	rvb, sd_mci+0x100,#0x30	@ rvb <- MMCHS_STAT
	write	rvb, rva, #0x30		@ clear stat flags
	write	rvc, rva, #0x08		@ MMCHS_ARG <- arg
	int2raw	rvb, sv4
	and	rvb, rvb, #0xff
	lsl	rvb, rvb, #24
	eq	sv4, #i0
	beq	sdpcm0
	eq	sv4, #((41 << 2) | i0)
	orreq	rvb, rvb, #(0x02 << 16)
	beq	sdpcm0
	tst	sv4, #0x10000000
	orreq	rvb, rvb, #(0x1a << 16)
	orrne	rvb, rvb, #(0x09 << 16)
	eq	sv4, #((0x06 << 2) | i0)
	eqne	sv4, #((0x07 << 2) | i0)
	orreq	rvb, rvb, #(0x1b << 16)
sdpcm0:	@ continue
	write	rvb, rva, #0x0c		@ MMCHS_CMD <- cmd
sdpcmb:	@ wait for cmd complete
	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	tsteq	rvb, #0x01
	beq	sdpcmb
	@ wait for transfer complete if needed
	eq	sv4, #((0x06 << 2) | i0)
	eqne	sv4, #((0x07 << 2) | i0)
	bne	sdpcm1
sdpcmt:	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	tsteq	rvb, #0x02
	beq	sdpcmt
sdpcm1:	@ continue
	wait	0x5000000		@ wait a bit more
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #0x0d		@ CMD3?
	bne	sdpcmc
	read	rvb, rva, #0x30		@ rvb <- MMCHS_STAT
	tst	rvb, #0x8000
	setne	rvb, 0
	setne	pc,  lnk
sdpcmc:	@ continue
	@ clear state, get response
	write	0x01, rva, #0x30
	read	rvb,  rva, #0x10	@ rvb <- MMCHS_RSP10
	@ return
	set	pc,  lnk

.endif	@ .ifdef sd_is_on_mci


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



