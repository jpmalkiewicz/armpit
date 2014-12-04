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
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@
@	SPI Interface
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
	@ in:	env	<- sys_ctrl	= clock selection, enabling, ...
	@ ret:	via lnk (through sd_slo)
	@ SPI0 interface pins:
	@ LPC_2000: P0.4,  P0.5,  P0.6  used for SCK0, MISO0, MOSI0
	@ LPC_17xx: P0.15, P0.17, P0.18 used for SCK0, MISO0, MOSI0
	@ sd_cs (gpio) used for CS
	@ configure chip-select pin as gpio out, and de-select sd card
	rgcpbt	sd_cs_gpio, #io_dir, sd_cs_pin, 1 @ sd_cs_gpio <- SD CS pin=out
	write	1<<sd_cs_pin, rva, #io_set	  @ set SD CS hi = deselect card
  .ifdef LPC_2000
	@ configure other spi pins: P0.4,5,6 cfg via pinsel0 as SPI (cfg = #b01)
	rgcpbf	PINSEL0, #0x00, 8, 14, 0b010101
    .ifdef spi_old_silicon @ (TINY_2106, LPC_H2214, LPC_H2294)
	@ also config SSEL0 (P0.7) and tie it to 3.3 Volt manually (with a wire)
	rgcpbf	PINSEL0, #0x00, 14, 16, 0b01
    .endif
  .endif
  .ifdef LPC_17xx
	@ config other spi pins: P0.15,17,18 via pinsel0/1, SPI legacy, cfg=#b11
	rgcpbf	PINSEL0, #0x00, 30, 32, 0b11	@ P0.15    <- #b11 for sck
	rgcpbf	PINSEL1, #0x00,  2,  6, 0b1111	@ P0.17,18 <- #b11 for miso,mosi
  .endif
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_	
sd_fst:	@ configure spi speed (high), phase, polarity
	write	8,    sd_spi, #0x0c	@ s0spccr clk <- 48-60 MHz/8 = 6-8 MHz
	write	0x20, rva,    #0x00	@ s0spcr <- master, 8-bit, POL=PHA=0
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	write	150,  sd_spi, #0x0c	@ s0spccr clk <- 48-60 MHz/150 ~= 400KHz
	write	0x20, rva,    #0x00	@ s0spcr <- master, 8-bit, POL=PHA=0
	set	pc,  lnk

.endif @ sd_is_on_spi

/*------------------------------------------------------------------------------
@
@	MMC/MCI Interface (targets: LPC2478, LPC4300)
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
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_mci

	/* LPC 4300 */

  .ifdef LPC_4300

    .include "LPC_4300/file_sd_hw_local.s"

  .endif

	/* LPC2478 or (possibly) Other MCUs with dma */

  .ifndef _sgbrs

_func_
sd_cfg:	@ configure spi power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl	= clock selection, enabling, ...
	@ ret:	via lnk
	@ MCI interface with DMA, as implemented on LPC-2478-STK
	@ Pins P1.2, P1.3, P1.5, P1.6, P1.7, P1.11, P1.12 <- MCI function
	@ power-up and enable gpdma
	rgcpbt	env, #0xc4, 29, 1	@ power-up GPDMA (PCONP bit 29)
	write	sv1, gdma_base, #0x30	@ enable gpdma, little-endian
	write	0x02006f, bdma_base, #0	@ init dma buffer with bytevector tag
	@ power-up and configure mci peripheral
	rgcpbt	env, #0xc4, 28, 1	@ power-up SD/MMC block (PCONP bit 28)
	rgrmw	env, #0x1ac, 0x03000000	@ PCLK_MCI <- 9MHz, PCLKSEL1 24:25
	rgcpbt	env, #0x1a0, 3, 1	@ MCIPWR phase active high (SCS bit 3)
	@ configure mci pins
	rgrmw	PINSEL2, #0, 0x03c0fcf0, 0x0280a8a0 @ P1.2,3,5-7,11,12 <- MCI
	rgrmw	rva,  #0x40, 0x03c0fcf0, xxx	    @ P1.2,3,5-7,11,12 <- mode 0
	@ power-up and power-on mci peripheral function
	write	sv2,   mci_base, #0x00	@ set MCI to power-up phase
	write	0x116, rva,      #0x04	@ 400KHz (200KHz?) MCI CLK, narrow bus
	write	sv3, rva, #0x00		@ set MCI to power-on phase
	rgwfbm	rva, #0x00, 3
	@ return
	set	pc,  lnk

_sgbrs:	@ sd block read transfer start/restart
	@ in:	rvc <- block or byte address of block to read (raw int)
	@ in:	sv3 <- buffer to store block data (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ out:	sv3 <- updated buffer
	@ mods:	sv3, sv5, rva, rvb, rvc
	@ dmactl  (#x0849 . #x2200) ; #x08492200
	@ dmacfg  (#x0001 . #x3009) ; #x013009 (dma chan lock) (#x003009 unlock)
	@ clear dma channel  0 via DMACC0Config
	write	0, gdma_base, #0x110
	@ prepare for read-block
	bl	sd_pre		@ prepare mci
	set	rvb, 17
	bl	sd_cmd
	eq	rva, #0
	bne	_sgbrs
	@ configure gpdma
	write	sd_mci+0x80,gdma_base,#0x100 @ DMACC0SrcAddr  <- MCIFIFO
	write	bdma_base+4,rva,      #0x104 @ DMACC0DestAddr <- dma buffer + 4
	write	0x08492200, rva,      #0x10c @ DMACC0Control <- 512B,8x32,incdst
	@ enable dma transfer
	write	0x013009, rva, #0x110	@ DMACC0Config <- lok,MCI2mem,MCIctl,ena
	@ MCIDataCtl <- 512B, block, dma ,from card
	write	0x9b, sd_mci, #0x2c
	@ wait for DataBlockEnd
	set	rvc, 0x400
	adr	lnk, _sgbrs
sgb_wt:	@ wait loop
	read	rvb, rva, #0x34		@ rvb <- stat
	tst	rvb, #0x3f		@ error?
	bne	sd_cm1			@	if so,  jump to restart
	tst	rvb, rvc
	beq	sgb_wt
	@ copy data into sv3
	set	rva, bdma_base
	set	rvc, 512
sgb_cp:	@ copy loop
	read	rvb, rva, rvc
	subs	rvc, rvc, #4
	write	rvb, sv3, rvc
	bne	sgb_cp
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_spbrs:	@ sd block write transfer start/restart
	@ in:	rvc <- block or byte address of block to write (raw int)
	@ in:	sv3 <- buffer w/block data to wrt to sd (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ mods:	sv5, rva, rvb, rvc
	@ dmactl  (#x0449 . #x2200) ; #x04492200
	@ dmacfg  (#x0001 . #x2901) ; #x012901 (dma chan lock) (#x002901 unlock)
	@ clear dma channel  0 via DMACC0Config
	write	0, gdma_base, #0x110
	@ prepare for write-block
	bl	sd_pre			@ prepare mci
	write	rvc, sd_mci, #0x08	@ set arg in MCIargument
	@ copy data from sv3 to dma buffer
	set	rva, bdma_base
	set	rvc, 0
spb_cp:	@ copy loop
	read	rvb, sv3, rvc
	add	rvc, rvc, #4
	write	rvb, rva, rvc
	eq	rvc, #512
	bne	spb_cp
	@ send cmd 24 (write single block)
	read	rvc, sd_mci, #0x08	@ rvc <- MCIargument, restored
	set	rvb, 24
	bl	sd_cmd
	eq	rva, #0
	bne	_spbrs
	@ configure gpdma and enable transfer
	write	sd_mci+0x80,gdma_base,#0x104 @ DMACC0DestAddr <- MCIFIFO
	write	bdma_base+4,rva, #0x100	@ DMACC0SrcAddr <- dma buffer + 4
	write	0x4492200,  rva, #0x10c	@ DMACC0Control <- 512B, 8x32, incr. src
	write	0x012901,   rva, #0x110	@ DMACC0Config <- lok,mem2MCI,MCIctl,ena
	@ MCIDataCtl <- 512B, block, dma, to card
	write	0x99, sd_mci, #0x2c
	@ wait for DataBlockEnd
	set	rvc, 0x400
	adr	lnk, _spbrs
spb_wt:	@ wait loop
	read	rvb, rva, #0x34		@ rvb <- MCI stat
	tst	rvb, #0x3f		@ error?
	bne	sd_cm1			@ 	if so,  jump to restart
	tst	rvb, rvc
	beq	spb_wt
	read	rvc, rva, #0x14		@ rvc <- response0
spb_ts:	@ wait for card in ready-tran state
	bl	sd_pre			@ prepare mci
	set	rvc, 0			@ rvc <- arg
	set	rvb, 13			@ rvb <- cmd13 (card status)
	bl	sd_cmd
	eq	rva, #0
	eqne	rvb, #9
	bne	spb_ts
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk
	
sd_pre:	@ mci-prep subroutine
	write	0,      sd_mci, #0x0c	@ clear previous MCI command
	write	0x07ff, rva,    #0x38	@ clear MCI Stat flags
	write	1<<27,  rva,    #0x24	@ set timeout to > 1e8 in MCIDataTimer
	write	512,    rva,    #0x28	@ set MCIDataLength to 512
	set	pc,  lnk

sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	@ on entry: rvc <- arg
	write	rvc, sd_mci, #0x08	@ set arg in MCIargument
	orr	rvb, rvb, #0x0440
	write	rvb, rva,    #0x0c
sd_cm0:	@ comand wait loop
	read	rvb, rva, #0x34		@ rvb <- MCI stat
	tst	rvb, #0x04		@ cmd timeout?
	bne	sd_cm1
	tst	rvb, #0x40
	beq	sd_cm0
	@ get response
	rgbf	rvb, rva, #0x14, 8, 12	@ rvb <- response (bit-field 8-12)
	eq	rvb, #9			@ was cmd rcvd while card ready and in tran state?
	seteq	rva, 0
	seteq	pc,  lnk
sd_cm1:	@ wait then restart transfer
	@ [also: internal entry]
	wait	1<<18
	rgbf	rvb, sd_mci, #0x14,8,12	@ rvb <- response (bit-field 8-12)
	read	rvc, rva, #0x08		@ rvc <- arg restored (eg. for restart)
	set	pc,  lnk

_func_	
sd_slo:	@ configure mci speed (low = 400 KHz), 1-bit bus, clock enabled
	write	0x116, sd_mci, #0x04	@ 400KHz, 1-bit bus, CLK enabled
	set	pc,  lnk

_func_	
sd_fst:	@ configure mci speed (high = 9 MHz), wide bus, clock enabled
	write	0xd16, sd_mci, #0x04	@ 9 MHz (bypass), wide bus, CLK enabled
	set	pc,  lnk

sdpcmd:	@ function to write a command to SD/MMC card
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	write	0, sd_mci, #0x0c	@ clear previous cmd
	write	0x7ff, rva, #0x38	@ clear stat flags
	write	rvc,   rva, #0x08	@ set arg in MCIargument
	int2raw	rvb, sv4
	and	rvb, rvb, #0xff
	orr	rvb, rvb, #0x0400	@ bit to enable CPSM
	eq	sv4, #i0
	orrne	rvb, rvb, #0x40		@ bit to wait for response
	tst	sv4, #0x10000000
	orrne	rvb, rvb, #0x80		@ bit for long (vs short) response
	write	rvb, rva, #0x0c		@ send cmd
sdpcmb:	@ wait for mci not busy
	read	rvb, rva, #0x34
	tst	rvb, #0x3800
	bne	sdpcmb
	wait	0x100000		@ wait a bit
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #0x0d		@ CMD3?
	bne	sdpcmc
	read	rvb, rva, #0x34
	eq	rvb, #0x40
	setne	rvb, 0
	setne	pc,  lnk
sdpcmc:	@ continue
	rgbf	rvb, rva, #0x34, 0, 11
	write	rvb, rva, #0x38		@ clear status register
	read	rvb, rva, #0x14		@ rvb <- response0
	set	pc,  lnk

  .endif	@ .ifndef _sgb (LPC2478)

.endif @ sd_is_on_mci

.ltorg



