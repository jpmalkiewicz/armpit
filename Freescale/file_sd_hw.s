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
@	SD card low-level interface, SPI and MMC/MCI (SDHC)
@
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@
@	SPI Interface
@
@	Code Entry Points:
@
@		sd_cfg		configure hardware
@		sd_fst		set high speed
@		sd_slo		set slow speed
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_spi

_func_
sd_cfg:	@ configure spi power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- scgc_base = SIM_SCGC, periph clken
	@ ret:	via lnk (through sd_slo)
	@ PTE 1-3 = SPI1, PTE 4 = gpio CS
	@ enable module clock (SCGC6 p.316) and pins (gpio port E, SCGC5 p.314)
	swi	run_prvlgd
	rgcpbt	env, #0x14, 13, 1	@ SCGC6      <- enable clock to SPI1
	rgcpbt	env, #0x10, 13, 1	@ SCGC5      <- enable clock to PORT E
	swi	run_normal
	@ configure pin function and deselect card (p.246,280,284) (p.1761)
	write	0x200, ioporte_pcr, #(2<<2) @  _PCR2 <- PTE2=SPI1 SCK,nopud,fast
	write	0x703, rva, #(1<<2)	@ PORTE_PCR1 <- PTE1=SPI1 SIN, pup, fast
	write	rvb,   rva, #(3<<2)	@ PORTE_PCR3 <- PTE3=SPI1 SOUT,pup, fast
	write	0x100, rva, #(4<<2)	@ PORTE_PCR4 <- PTE4=GPIO(CS),nopud,fast
	rgrmw	sd_cs_gpio, #io_dir, 1<<sd_cs_pin   @ GPIOE_PDDR <- CS(PTE4)=out
	write	1<<sd_cs_pin, rva, #io_set	    @ GPIOE_PSOR <- desel SD(hi)
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_
sd_fst:	@ configure spi speed (high), phase, polarity
	write	  0x01, sd_spi, #0x00	@ SPIx_MCR   <- halt
	write	(7<<27)|4, rva, #0x0c	@ SPIx_CTAR0 <- 120/2/16~4MHz,POL/PHA=0
	write	(1<<31)|(3<<12), rva,#0	@ SPIx_MCR   <- run, master, no FIFO
	set	pc,  lnk

_func_
sd_slo:	@ configure spi speed (low), phase, polarity
	write	  0x01, sd_spi, #0x00	@ SPIx_MCR   <- halt
	write	(7<<27)|9, rva, #0x0c	@ SPIx_CTAR0 <- 120/2/512~120K,POL/PHA=0
	write	(1<<31)|(3<<12), rva,#0	@ SPIx_MCR   <- run, master, no FIFO
	set	pc,  lnk

.endif @ sd_is_on_spi

/*------------------------------------------------------------------------------
@
@	MMC/MCI Interface
@
@	Code Entry Points:
@
@		sd_cfg		configure hardware
@		_sgbrs		get data block from SD card
@		_spbrs		write data block to SD card
@		sd_fst		set high speed
@		sd_slo		set slow speed
@		sd_cmd		send command
@		sdpcmd		send command during initialization
@
@-------------------------------------------------------------------------------
@
@	Interface is similar to TI OMAP
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_mci

_func_
sd_cfg:	@ configure MMC/MCI power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- scgc_base = SIM_SCGC, periph clken
	@ ret:	via lnk
	@ initialize SDHC, pins PTE 0-5 = SDHC0
	@ cfg clocks (SIM_SOPT2,p.296) module (SCGC3 p.310) pins (SCGC5 p.314)
	swi	run_prvlgd
	rgrmw	sim_base+0x1000, #4, 0b10<<28	@ _SOPT2 <- SDHC=OSCERCLK 50MHz
	rgcpbt	env, #0x08, 17, 1	@ SCGC3      <- enable SDHC module clock
	rgcpbt	env, #0x10, 13, 1	@ SCGC5      <- enable PORT E clock
	swi	run_normal
	@ configure pin function (PORTE_PCR, p.246,280,284)
	write	0x400, ioporte_pcr, #(0<<2) @  _PCR0 <- SDHC0 D1,  nopud, fast
	write	rvb,   rva, #(2<<2)	@ PORTE_PCR2 <- SDHC0 DCLK,nopud, fast
	write	rvb,   rva, #(4<<2)	@ PORTE_PCR4 <- SDHC0 D3,  nopud, fast
	write	rvb,   rva, #(5<<2)	@ PORTE_PCR5 <- SDHC0 D2,  nopud, fast
	write	0x403, rva, #(1<<2)	@ PORTE_PCR1 <- SDHC0 D0,  pullup,fast
	write	rvb,   rva, #(3<<2)	@ PORTE_PCR3 <- SDHC0 CMD, pullup,fast
	@ return
	set	pc,  lnk		@ return


_sgbrs:	@ sd block read transfer start/restart
	@ in:	rvc <- block or byte address of block to read (raw int)
	@ in:	sv3 <- buffer to store block data (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ out:	sv3 <- updated buffer
	@ modifies:  sv3, sv5, rva, rvb, rvc
	bl	sd_pre
	@ do CMD17: rd 1 blk, dat prsnt, cmd idx chk, CRC chk,48b rsp,crd-to-hst
	set	rvb, (17<<24)|(3<<20)|(0xa<<16)|(1<<4)	@ rvb <- CMD17 & options
	bl	sd_cmd			@ SDHC_XFERTYP <- cmd (rvb) & arg (rvc)
	eq	rva, #0
	beq	_sgbrs
sgb_wt:	@ wait for data (loop)
	read	rvb, rva, #0x30		@ rvb <- SDHC_IRQSTAT
	tst	rvb, #(0x7f<<16)	@ any error?
	bne	_sgbrs			@	if so,  jump to restart transfer
	tst	rvb, #(1<<5)		@ Buffer Read Ready (BRR)?
	beq	sgb_wt			@	if not, jump back to wait
	orr	rvb, rvb, #(1<<5)
	write	rvb, rva, #0x30		@ SDHC_IRQSTAT <- clear BRR bit
	@ get data
	set	rvc, 0
sgb_cp:	@ data-read loop
	read	rvb, rva, #0x20		@ rvb <- SDHC_DATPORT data word from SD
	write	rvb, sv3, rvc		@ store data word in bytvector buffer
	add	rvc, rvc, #4
	eq	rvc, #512		@ done?
	bne	sgb_cp			@	if not, jump back to get data
	rgcpbt	rva, #0x30, 1, 1	@ SDHC_IRQSTAT <- clear TC bit
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk


_spbrs:	@ sd block write transfer start/restart
	@ in:	rvc <- block or byte address of block to write (raw int)
	@ in:	sv3 <- buffer w/block data to wrt to sd (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ mods:	sv5, rva, rvb, rvc
	bl	sd_pre
	@ do CMD24: wrt sngl blk, dat prsnt, cmd idx chk, CRC chk, 48-bit rspns
	set	rvb, (24<<24)|(3<<20)|(0xa<<16)	@ rvb <- CMD24 & options
	bl	sd_cmd			@ SDHC_XFERTYP <- cmd (rvb) & arg (rvc)
	eq	rva, #0
	beq	_spbrs
	rgwfbt	rva, #0x24, 10, 1	@ wait for card ready (BWEN/PRSSTAT)
	@ put data
	set	rvc, 0
spb_cp:	@ data-write loop
	read	rvb, sv3, rvc		@ rvb <- data word from bytevec buffer
	write	rvb, rva, #0x20		@ SDHC_DATPORT <- data word to SD-card
	add	rvc, rvc, #4
	eq	rvc, #512		@ done?
	bne	spb_cp			@	if not, jump back to write more
spb_wt:	@ wait for end of write
	read	rvb, rva, #0x30		@ rvb <- SDHC_IRQSTAT
	tst	rvb, #(0x7f<<16)	@ any error?
	it	ne
	readne	rvc, rva, #0x08		@ 	if so,  rvc <- CMDARG, restored
	bne	_spbrs			@	if so,  jump to restart transfer
	tst	rvb, #0x02		@ Transfer Complete (TC bit set)?
	beq	spb_wt			@	if not, jump back to wait
	orr	rvb, rvb, #0x02		@ rvb <- TC bit
	write	rvb, rva, #0x30		@ SDHC_IRQSTAT <- clear state
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk


_func_
sd_pre:	@ mci-prep subroutine
	read	rvb, sd_mci, #0x24	@ rvb <- SDHC_PRSSTAT
	tst	rvb, #0x07		@ DAT and CMD lines idle?
	beq	sd_pr0			@	if so,  jump to continue
	rgcpbt	rva, #0x2c, 25, 1	@ SDHC_SYSCTL <- reset CMD line
	rgcpbt	rva, #0x2c, 26, 1	@ SDHC_SYSCTL <- reset DAT line
	rgwfbf	rva, #0x2c, 25, 27, 0	@ wait for reset done
	rgcpbt	rva, #0x28, 1, 1	@ SDHC_PROCTL <- set 4-bit bus mode
	write	0x007f0033, rva, #0x34	@ SDHC_IRQSTATEN <- DTO,DCE,DEB,etc...
	write	512, rva, #0x04		@ SDHC_BLKATTR <- 512
	write	(128<<16)|128,rva,#0x44	@ SDHC_WML <- 128 words for read & write
sd_pr0:	@ clear previous state
	read	rvb, rva, #0x30		@ rvb <- SDHC_IRQSTAT
	write	rvb, rva, #0x30		@ SDHC_IRQSTAT <- clear state
	set	pc,  lnk


_func_
sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	@ on entry: rvc <- arg
	write	rvc, sd_mci, #0x08	@ SDHC_CMDARG  <- arg
	write	rvb, rva,    #0x0c	@ SDHC_XFERTYP <- cmd
sd_cm0:	read	rvb, rva,    #0x30	@ rvb <- SDHC_IRQSTAT
	tst	rvb, #(0x7f<<16)	@ any error?
	itT	ne
	setne	rva, 0			@	if so,  rva <- 0 (error indic)
	setne	pc,  lnk		@	if so,  return
	tst	rvb, #0x01		@ Command Complete (CC bit set)?
	beq	sd_cm0			@	if not, jump back to wait
	@ clear state, get response, wait for write-ready
	write	1,   rva, #0x30		@ SDHC_IRQSTAT <- clear CC
	read	rvb, rva, #0x10		@ rvb <- SDHC_CMDRSP0 (CMD response 0/1)
	set	pc,  lnk

_func_
sdpcmd:	@ function to write a command to SD/MMC card during initialization
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	@ commands treated are: CMD 0,2,3,6,7,9,16,41,55
	read	rvb, sd_mci, #0x30	@ rvb <- SDHC_IRQSTAT
	write	rvb, rva,    #0x30	@ SDHC_IRQSTAT <- clear status flags
	write	rvc, rva,    #0x08	@ SDHC_CMDARG <- arg
	int2raw	rvb, sv4
	and	rvb, rvb, #0xff
	lsl	rvb, rvb, #24
	eq	sv4, #i0		@ is this CMD0 ?
	beq	sdpcm0			@	if so,  jump to continue
	eq	sv4, #((41 << 2) | i0)	@ is this CMD41 ?
	it	eq
	orreq	rvb, rvb, #(0x02 << 16)	@	if so,  rvb <- 48-bit response
	beq	sdpcm0			@	if so,  jump to continue
	tst	sv4, #(1<<28)		@ want a long response ?
	itE	eq
	orreq	rvb, rvb, #(0x1a << 16)	@	if not, rvb <- idx&CRC ck,48-bit
	orrne	rvb, rvb, #(0x09 << 16)	@	if so,  rvb <- CRC chk,  136-bit
	eq	sv4, #((0x06 << 2)|i0)	@ is this CMD6 ?
	it	ne
	eqne	sv4, #((0x07 << 2)|i0)	@	if not, is this CMD7 ?
	it	eq
	orreq	rvb, rvb, #(0x1b << 16)	@	if so,  rvb <- idx&CRC,48bit,bsy
sdpcm0:	@ continue
	write	rvb, rva, #0x0c		@ SDHC_XFERTYP <- cmd
sdpcmb:	@ wait for cmd complete
	read	rvb, rva, #0x30		@ rvb <- SDHC_IRQSTAT
	tst	rvb, #(0x7f<<16)	@ no error?
	it	eq
	tsteq	rvb, #0x01		@	if so,  is CMD complete (CC)?
	beq	sdpcmb			@	if not, jump back to wait
	@ wait for transfer complete if needed
	eq	sv4, #((0x06 << 2)|i0)	@ was this CMD6 ?
	it	ne
	eqne	sv4, #((0x07 << 2)|i0)	@	if not, is this CMD7 ?
	bne	sdpcm1			@	if not, jump to continue
sdpcmt:	@ loop
	add	sv4, sv4, #(1<<16)
	read	rvb, rva, #0x30		@ rvb <- SDHC_IRQSTAT
	tst	rvb, #(0x7f<<16)	@ no error?
	it	eq
	tsteq	rvb, #0x02		@	if so,  is transfer complete?
	it	eq
	tsteq	sv4, #(1<<23)
	beq	sdpcmt			@	if not, jump back to wait
	bic	sv4, sv4, #(0xff << 16)
sdpcm1:	@ continue
	wait	1<<22
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #0x0d		@ was this CMD3?
	bne	sdpcmc			@	if not, jump to continue
	read	rvb, rva, #0x30		@ rvb <- SDHC_IRQSTAT
	tst	rvb, #(0x7f<<16)	@ any error?
	itT	ne
	setne	rvb, 0			@	if so,  rvb <- 0, error indic
	setne	pc,  lnk		@	if so,  return
sdpcmc:	@ continue
	@ clear state, get response
	write	1,   rva, #0x30		@ SDHC_IRQSTAT <- clear CC
	read	rvb, rva, #0x10		@ rvb <- SDHC_CMDRSP0 (CMD response 0/1)
	@ return
	set	pc,  lnk

_func_
sd_slo:	@ configure mci speed (low = 400 KHz), 1-bit bus, clock enabled
	@ (perform reset first)
	@ modifies:	rva, rvb
	@ Note: SYSCTL differs slightly from TI OMAP (address and bits)
	@ disconnect clock-out to card
	rgcpbt	sd_mci, #0x2c, 3, 0	@ SDHC_SYSCTL <- card clock discon
	rgcpbt	rva, #0x2c, 24, 1	@ SDHC_SYSCTL <- reset all (RSTA)
	rgwfbt	rva, #0x2c, 24, 0	@ wait for reset cmplt in SDHC_SYSCTL
	rgcpbt	rva, #0x28, 3, 0	@ SDHC_PROCTL <- set 1-bit bus mode
	rgcpbf	rva, #0x2c, 4,20,0xe400	@ SDHC_SYSCTL <- tmout=2^27, f=390KHz
	rgwfbt	rva, #0x24, 3, 1	@ wait on clk stbl, SDSTB/MMCHS_PRSSTAT
	rgrmw	rva, #0x2c, 0x0f	@ SDHC_SYSCTL <- SDCLKEN,PEREN,HCKEN,...
	write	0x007f0033, rva, #0x34	@ SDHC_IRQSTATEN <- CC,TC,DTO,DCE,DEB...
	rgcpbt	rva, #0x2c, 27, 1	@ SDHC_SYSCTL <- set init-stream bit
	write	0,   rva, #0x0c		@ SDHC_XFERTYP <- CMD0
	rgwfbt	rva, #0x30, 0, 1	@ wait for cmd cmplt(CC) in SDHC_IRQSTAT
	write	rvb, rva, #0x30		@ SDHC_IRQSTAT <- clear CC bit
	write	0,   rva, #0x0c		@ SDHC_XFERTYP <- CMD0
	rgwfbt	rva, #0x30, 0, 1	@ wait for Command Complete (CC)
	write	1,   rva, #0x30		@ SDHC_IRQSTAT <- clear CC bit
	rgcpbt	rva, #0x2c, 27, 0	@ SDHC_SYSCTL <- clr init-stream bit
	set	pc,  lnk

_func_
sd_fst:	@ configure mci speed (high = 19 MHz), wide bus, clock enabled
	@ modifies:	rva, rvb
	@ Note: SYSCTL differs slightly from TI OMAP (address and bits)
	rgcpbt	sd_mci, #0x2c, 3, 0	@ SDHC_SYSCTL <- card not clocked
	rgcpbf	rva, #0x2c, 4, 16, 0x20	@ SDHC_SYSCTL <- speed = 12MHz (50MHz/4)
	rgwfbt	rva, #0x24, 3, 1	@ wait for clk stbl, SDSTB/MMCHS_PRSSTAT
	rgrmw	rva, #0x2c, 0x0f	@ SDHC_SYSCTL <- card clocked
	write	512, rva, #0x04		@ SDHC_BLKATTR <- 512
	rgcpbt	rva, #0x28, 1, 1	@ SDHC_PROCTL <- set 4-bit bus mode
	write	(128<<16)|128,rva,#0x44	@ SDHC_WML <- 128 words for read & write
	set	pc,  lnk

.endif 	@ sd_is_on_mci



