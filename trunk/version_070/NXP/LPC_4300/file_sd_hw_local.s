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
@	SD card low-level interface, MMC/MCI
@
@-----------------------------------------------------------------------------*/

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
@-----------------------------------------------------------------------------*/

_func_
sd_cfg:	@ configure MMC/MCI power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ ret:	via lnk
	@ configure SD_MMC pins
	write	0xf0|sd_mod_d0_cmd,sd_psl_d0_cmd,#(sd_pin_cmd<<2) @ SDCMD <-mode
	write	rvb, rva, #((sd_pin_d0+0)<<2)	@ SD_D0 <- set mode
	write	rvb, rva, #((sd_pin_d0+1)<<2)	@ SD_D1 <- set mode
	write	rvb, rva, #((sd_pin_d0+2)<<2)	@ SD_D2 <- set mode
	write	rvb, rva, #((sd_pin_d0+3)<<2)	@ SD_D3 <- set mode
	write	0xb0|sd_mod_clk,sd_psl_clk,#(sd_pin_clk<<2)	  @ SDCLK <-mode
	@ set SD clock parameters into peripheral
	write	fre, sd_mci, #0x08	@ CLKDIV0 <- 0 (only setting available?)
	write	fre, rva,    #0x10	@ CLKSRC  <- clock source is DIV0 (id.)
	write	sv1, rva,    #0x10	@ CLKENA  <- clock enable, off when idle
	write	(1<<21)|(1<<31), rva, #0x2c	@ CMD     <- update clock
	rgwfbt	rva, #0x2c, 31, 0		@ wait for cmd loaded in CUI
	@ return
	set	pc,  lnk


_func_
_sgbrs:	@ sd block read transfer start/restart
	@ in:	rvc <- block or byte address of block to read (raw int)
	@ in:	sv3 <- buffer to store block data (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ out:	sv3 <- updated buffer
	@ mods:	sv3, sv5, rva, rvb, rvc
	bl	sd_pre			@ prep mci (clr stat bits, set byte cnt)
	@ send cmd 17 (read single block)
	set	rvb, 17			@ rvb <- read single block command
	orr	rvb, rvb, #(1 << 9)	@ rvb <- dat expctd, read sngl block mod
	bl	sd_cmd			@ set command
	@ check for error, if so, restart
	eq	rva, 0			@ command set successfully?
	bne	_sgbrs			@	if not, jmp to retry
	@ get and save data
	set	rva, sd_mci		@ rva <- mci address
	set	rvc, 0			@ rvc <- initial buffer offset/data cnt
	adr	lnk, _sgbrs		@ lnk <- address for retry (via sd_cm1)
sgb_gd:	@ get-data loop
	read	rvb, rva, #0x44		@ rvb <- RINTSTS
	bic	rvb, rvb, #0x3c		@ rvb <- status w/cleared non-error bits
	bic	rvb, rvb, #(1 << 10)	@ rvb <- status w/cleared HTO starve bit
	bic	rvb, rvb, #(1 << 14)	@ rvb <- status w/cleared acmd done bit
	eq	rvb, #0			@ any non-expected errors?
	bne	sd_cm1			@	if so,  jump to restart
	rgcpbt	rva, #0x44, 3, 0	@ clear set status bits, except DTO
	tst	rvb, #0x0420		@ is data available?
	bne	sgb_g0			@	if so,  jump to read it
	read	rvb, rva, #0x48		@ rvb <- STATUS
	lsr	rvb, rvb, #17		@ rvb <- status shifted
	and	rvb, rvb, #0x3f		@ rvb <- number of words in FIFO
	eq	rvb, #0			@ FIFO empty?
	beq	sgb_gd			@	if so,  jump to keep waiting
sgb_g0:	@ read a data word and update count
	read	rvb, rva, #0x0100	@ rvb <- word from FIFO
	write	rvb, sv3, rvc		@ store word in buffer
	add	rvc, rvc, #4		@	if not, rvc <- updated count
	eq	rvc, #512		@ done reading data?
	bne	sgb_gd			@	if not, jump to read next word
	rgwfbt	rva, #0x44, 3, 1	@ wait for DTO (transfer done) bit
	write	0x08, rva, #0x44	@ RINTSTS <- clear DTO bit
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk


_func_
_spbrs:	@ sd block write transfer start/restart
	@ in:	rvc <- block or byte address of block to write (raw int)
	@ in:	sv3 <- buffer w/block data to wrt to sd (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ mods:	sv5, rva, rvb, rvc
	bl	sd_pre			@ prep mci (clr stat bits, set byte cnt)
	write	rvc, sd_mci, #0x28	@ set arg in CMDARG (saved)
	@ pre-load the FIFO, if needed (32 words)
	read	rvb, sd_mci, #0x48	@ rvb <- STATUS
	lsr	rvb, rvb, #17		@ rvb <- status shifted
	and	rvb, rvb, #0x3f		@ rvb <- number of words in FIFO
	lsl	rvc, rvb, #2		@ rvc <- number of bytes in FIFO
spb_w0:	@ loop
	cmp	rvc, #128
	it	mi
	readmi	rvb, sv3, rvc
	it	mi
	writemi	rvb, rva, #0x0100
	it	mi
	addmi	rvc, rvc, #4
	bmi	spb_w0
	@ send cmd 24 (write single block)
	read	rvc, sd_mci, #0x28	@ rvc <- arg from CMDARG (restored)
	set	rvb, (3<<9)|24		@ rvb <- dat expctd, wrt sngl block mode
	bl	sd_cmd
	@ check for error, if so, restart
	eq	rva, #0			@ command set successfully?
	bne	_spbrs			@	if not, jmp to retry
	@ write data
	set	rva, sd_mci		@ rva <- mci address
	set	rvc, 128		@ rvc <- bufr ofst/dat cnt (aft preload)
	adr	lnk, _spbrs		@ lnk <- address for retry (via sd_cm1)
spb_wd:	@ write-data loop
	read	rvb, rva, #0x44		@ rvb <- RINTSTS
	bic	rvb, rvb, #0x3c		@ rvb <- status w/cleared non-error bits
	bic	rvb, rvb, #(1 << 10)	@ rvb <- status w/cleared HTO starve bit
	bic	rvb, rvb, #(1 << 14)	@ rvb <- status w/cleared acmd done bit
	eq	rvb, #0			@ any non-expected errors?
	bne	sd_cm1			@	if so,  jump to restart
	rgcpbt	rva, #0x44, 3, 0	@ RINTSTS <- clear stat bits except DTO
	tst	rvb, #0x0410		@ is FIFO TxRdy or data starved?
	beq	spb_wd			@	if not, jump to keep waiting
	read	rvb, sv3, rvc		@ rvb <- data word from buffer
	write	rvb, rva, #0x0100	@ write data word to FIFO
	add	rvc, rvc, #4		@ rvc <- updated offset/count
	eq	rvc, #512		@ done writing data?
	bne	spb_wd			@	if not, jmp to keep writing data
	rgwfbt	rva, #0x44, 3, 1	@ wait for DTO (transfer done) bit
	write	0x08, rva, #0x44	@ RINTSTS <- clear DTO bit
	read	rvc,  rva, #0x30	@ rvc <- response0
spb_ts:	@ wait for card in ready-tran state
	bl	sd_pre			@ prep mci (clr stat bits, set byte cnt)
	set	rvc, 0			@ rvc <- command argument
	set	rvb, 13			@ rvb <- 13 (command to read status)
	bl	sd_cmd			@ set command
	eq	rva, #0			@ command set properly?
	it	ne
	eqne	rvb, #9			@	if not, is card rdy-tran state?
	bne	spb_ts			@	if not, jump to keep waiting
	@ return
	wait	0x4e0000		@ wait approx. 50ms
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_func_	
sd_pre:	@ mci-prep subroutine
	read	rvb, sd_mci, #0x44	@ rvb <- RINTSTS
	write	rvb, rva,    #0x44	@ RINTSTS <- clear set status bits
	write	512, rva,    #0x20	@ set BYTCNT to 512
	set	pc,  lnk


_func_
sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	@ on entry: rvc <- arg
	write	rvc, sd_mci, #0x28	@ set arg in CMDARG
	orr	rvb, rvb, #(1 << 31)	@ rvb <- bit to start command
	orr	rvb, rvb, #0x40		@ rvb <- bit to wait for response
	write	rvb, rva, #0x2c		@ CMD <- cmd
sd_cm0:	@ wait for cmd to be loaded (or HLE error)
	read	rvb, rva, #0x44		@ rvb <- RINTSTS
	tst	rvb, #(1 << 12)		@ hardware lock error (HLE)?
	bne	sd_cm1			@ 	if so,  wait+restart (via lnk)
	read	rvb, rva, #0x2c		@ rvb <- CMD
	tst	rvb, #(1 << 31)		@ cmd loaded?
	bne	sd_cm0			@	if not, jump to keep waiting
	set	rva, 1<<28
sd_cm3:	@ wait for cmd done or error
	subs	rva, rva, #1
	beq	sd_cm1
	set	rvb, sd_mci		@ rva <- mci address	
	read	rvb, rvb, #0x44		@ rvb <- RINTSTS
	tst	rvb, #(1 << 2)		@ cmd done?
	it	eq
	tsteq	rvb, #(1 << 12)		@ 	if not, hw lock error (HLE)?
	it	eq
	tsteq	rvb, #(1 << 14)		@ 	if not, acmd done?
	beq	sd_cm3			@	if not, jump to keep waiting
	write	rvb, sd_mci, #0x44	@ RINTSTS <- clear set status bits
	@ get response
	read	rvb, rva, #0x30		@ rvb <- response0
	lsr	rvb, rvb, #8		@ rvb <- response0 shifted
	and	rvb, rvb, #0x0f		@ rvb <- card status from response0
	eq	rvb, #9			@ cmd rcvd w/card ready & in tran state?
	itT	eq
	seteq	rva, 0			@	if so,  rva <- 0, i.e. all good
	seteq	pc,  lnk		@	if so,  return
sd_cm1:	@ wait then restart transfer
	@ [also: internal entry]
	wait	1<<18			@ wait a bit
	read	rvb, sd_mci, #0x30	@ rvb <- response0
	lsr	rvb, rvb, #8		@ rvb <- response0 shifted
	and	rvb, rvb, #0x0f		@ rvb <- card status from response0
	read	rvc, rva, #0x28		@ rvc <- CMDARG restored for restart
	set	pc,  lnk		@ jump back to restart (based on lnk)

_func_	
sd_slo:	@ configure mci speed to low = 400 KHz, 1-bit bus, clock enabled
	@ connect IDIVE, 400 KHz, for SD-card (sd-slo), to XTal (12 MHz)
	write	(6<<24)|(1<<11)|(29<<2), CGU_base, #0x58 @ IDIVE_CTRL <- XTal/30
	read	rvb, sd_mci, #0x44	  @ rvb     <- RINTSTS
	write	rvb, rva,    #0x44	  @ RINTSTS <- clear set status bits
	write	0,   rva,    #0x18	  @ CTYPE   <- 1-bit bus
	write	(1<<31)|(1<<15),rva,#0x2c @ CMD     <- init 80-clk stream bit
	rgwfbt	rva, #0x2c, 31, 0	  @ wait for cmd loaded
	set	pc,  lnk

_func_	
sd_fst:	@ configure mci speed to high, wide bus, clock enabled
	@ connect IDIVE, 20 MHz, for SD-card (sd-fst), to IDIVA (120 MHz)
	write	(0xc<<24)|(1<<11)|(11<<2),CGU_base,#0x58 @ IDIVE_CTRL <-IDIVA/12
	read	rvb, sd_mci, #0x44	@ rvb <- RINTSTS
	write	rvb, rva,    #0x44	@ RINTSTS <- clear set status bits
	write	1, rva, #0x18		@ CTYPE   <- 4-bit bus
	write	512, rva, #0x1c		@ BLKSIZ <- 512 bytes per data block
	write	(3<<16)|16, rva, #0x4c	@ FIFOTH <- 16-byte Tx, 3-byte Rx
	set	pc,  lnk

_func_	
sdpcmd:	@ function to write a command to SD/MMC card
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	read	rvb, sd_mci, #0x44	@ rvb <- RINTSTS
	write	rvb, rva,    #0x44	@ RINTSTS <- clear set status bits
	write	rvc, rva,    #0x28	@ CMDARG <- arg
	int2raw	rvb, sv4		@ rvb <- CMD (raw int)
	and	rvb, rvb, #0xff		@ rvb <- CMD (8 bits)
	orr	rvb, rvb, #(1 << 31)	@ rvb <- CMD with bit to start command
	eq	sv4, #i0		@ is it CMD0?
	it	ne
	orrne	rvb, rvb, #0x40		@ 	if not, rvb <- CMD & wait respns
	tst	sv4, #0x10000000	@ long response?
	it	ne
	orrne	rvb, rvb, #0x80		@ 	if so,  rvb <- CMD & long respns
	write	rvb, rva, #0x2c		@ CMD <- cmd
sdpcma:	@ wait for cmd to be loaded or error
	read	rvb, rva, #0x44		@ rvb <- RINTSTS
	tst	rvb, #(1 << 12)		@ hardware lock error (HLE)?
	bne	sdpcme			@	if so,  jump to exit
	read	rvb, rva, #0x2c		@ rvb <- CMD
	tst	rvb, #(1 << 31)		@ cmd loaded?
	bne	sdpcma			@	if not, jump to keep waiting
sdpcmb:	@ wait for cmd done or error
	read	rvb, rva, #0x44		@ rvb <- RINTSTS
	tst	rvb, #(1 << 2)		@ cmd done?
	it	eq
	tsteq	rvb, #(1 << 12)		@ 	if not, hw lock error (HLE)?
	it	eq
	tsteq	rvb, #(1 << 14)		@ 	if not, acmd done?
	beq	sdpcmb			@	if not, jump to keep waiting
sdpcme:	@ keep going (branched from HLE detected)
	wait	0x100000		@ wait a bit
	read	rvb, rva, #0x44		@ rvb <- RINTSTS
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #((3 << 2) | i0)	@ was this a CMD3?
	bne	sdpcmc			@	if not, jump to normal exit
	bic	rvb, rvb, #0x3c		@ rvb <- status w/cleared non-error bits
	bic	rvb, rvb, #(1 << 14)	@ rvb <- status w/cleared acmd done bit
	eq	rvb, #0			@ any non-expected errors?
	itT	ne
	setne	rvb, 0			@	if so,  rvb <- 0 (not succesful)
	setne	pc,  lnk		@	if so,  return
sdpcmc:	@ normal exit
	read	rvb, rva, #0x30		@ rvb <- response0
	set	pc,  lnk



