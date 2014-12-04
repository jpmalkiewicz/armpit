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
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input SUPPORT 5  - file input  port:	filipr, pflcli, pflgc0,
@							pflgc1, pflgc2
@  II.A.6.6.3. output SUPPORT 5 - file output port:	filopr, pflclo, pflwrc,
@							pflptc, pputs
@-----------------------------------------------------------------------------*/

.ifdef live_SD

	/* (main) file sd port, environment+obarray binding and port model ---*/
	BNDVAR	"FILE", vfile
	VCTR	vfile, i0, sdfipr, sdfopr

.endif

	/* sd-card port, environment+obarray binding and port model 	------*/
	BNDVAR	"SDFT", vsdft
	VCTR	vsdft, i0, sdfipr, sdfopr

	/* sd-card input and output port-vectors	----------------------*/
	VCTR	sdfipr, i1, val_npofxt, val_chrrdc, val_chrrdy, val_chpred, false, val_sdfgc0, val_sdfgc1, val_sdfgc2, val_sdfnfo, val_sdflst, (512 << 2) | i0
	VCTR	sdfopr, i2, val_sdfclo, val_filwrc, val_chpwrt, val_sdfptc, val_sdfnfo, val_sdfers, (512 << 2) | i0


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/


.ifdef	sd_is_on_spi

_func_	
sd_ini:	@ initialize communication with SD card
	@ out:	sv1 <- #t/#f for initialization success/failure
	@ mods:	sv1, sv3, sv4, sv5, rva, rvb, rvc
	bl	sd_dsl
	bl	sd_slo
	set	rvc, 10
sd_in0:	@ initial pulse train
	bl	sd_get
	subs	rvc, rvc, #1
	bne	sd_in0
	bl	sd_sel
	set	sv1, false		@ sv1 <- #f, default init result (fail)
	@ CMD0: go to idle state
	set	sv4, i0
	set	rvc, 0
	bl	sd_cmd
	@ CMD8: get interface cond (mandatory for SDHC)
	set	sv4, i8
	set	rvc, 0x1aa
	bl	sd_cmd
	@ flush CMD8 response (esp. if received)
	bl	sd_get			@ rvb <- CMD8 R7 response byte [31:24]
	bl	sd_get			@ rvb <- CMD8 R7 response byte [23:16]
	bl	sd_get			@ rvb <- CMD8 R7 response byte [15:8]
	bl	sd_get			@ rvb <- CMD8 R7 response byte  [7:0]
	set	sv3, i0
sd_in1:	@ ACMD41: send op cond (connect)
	set	sv4, (55<<2)|i0
	set	rvc, 0
	bl	sd_cmd
	set	sv4, (41<<2)|i0
	set	rvc, 1<<30		@ rvc <- arg for SDHC card (HCS bit set)
	bl	sd_cmd
	eq	rvb, #0
	beq	sd_in2
	add	sv3, sv3, #4
	cmp	sv3, #0x8000
	bmi	sd_in1
sd_in2:	@ continue
	eq	rvb, #0
	bne	sd_in3
	@ CMD58: get OCR to check card capacity
	set	sv4, (58<<2)|i0
	set	rvc, 0
	bl	sd_cmd
	eq	rvb, #0
	bne	sd_in3
	bl	sd_get			@ rvb <- CMD58 R3 response byte [31:24]
	set	rvc, rvb		@ rvc <- CMD58 R3 resp [31:24], saved
	bl	sd_get			@ rvb <- CMD58 R3 response byte [23:16]
	bl	sd_get			@ rvb <- CMD58 R3 response byte [15:8]
	bl	sd_get			@ rvb <- CMD58 R3 response byte  [7:0]
	tst	rvc, #(1<<6)		@ HCS bit set?
  .ifndef sdhc_on_sd_mmc
	bne	sd_in3			@	if so,  exit with #f (SDHC)
  .else
	beq	sd_in3			@	if not, exit with #f (not SDHC)
  .endif

	@ CMD16: set block length to 512
	set	sv4, i16
	set	rvc, 512
	bl	sd_cmd
	set	sv1, true		@ sv1 <- #t, card init success
sd_in3:	@ done
	bl	sd_dsl
	bl	sd_fst
	set	pc,  cnt


_func_	
sd_cmd:	@ send cmd to SD card
	@ in:	sv4 <- cmd (scheme int)
	@ in:	rvc <- arg (raw int)
	@ out:	rvb <- value returned by cmd
	@ mods:	sv5, rva, rvb
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved
	bl	sd_get
	int2raw	rvb, sv4
	orr	rvb, rvb, #0x40
	bl	sd_put			@ send cmd
	lsr	rvb, rvc, #24
	bl	sd_put			@ send arg
	lsr	rvb, rvc, #16
	bl	sd_put			@ send arg
	lsr	rvb, rvc, #8
	bl	sd_put			@ send arg
	set	rvb, rvc
	bl	sd_put			@ send arg
	set	rvb, 0xff 		@ rvb <- pseudo-crc (default)
	eq	sv4, #i0		@ cmd = 0?
	it	eq
	seteq	rvb, 0x95		@	if so,  rvb <- CRC for CMD 0
	eq	sv4, #i8		@ cmd = 8?
	it	eq
	seteq	rvb, 0x87		@	if so,  rvb <- CRC for CMD 8
	bl	sd_put			@ send crc/pseudo-crc
	@ wait for non #xff response
	set	rvc, 1
sd_cm1:	@ wait loop
	bl	sd_get
	eq	rvb, #0xff
	bne	sd_cm2
	add	rvc, rvc, #1
	eq	rvc, #10
	bne	sd_cm1
sd_cm2:	@ done, return
	orr	lnk, sv5, #lnkbit0	@ lnk <- restored
	set	pc,  lnk

_func_	
_sgb:	@ [internal only]
	@ sd-get-block internal func
	@ in:	rvc <- block number to be read (scheme int)
	@ in:	sv3 <- buffer in which to store block data (scheme bytevec)
	@ out:	sv3 <- updated buffer
	@ mods:	sv3, sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0
	str	sv5, [sv3, #0]		@ save saved-lnk into buffer
	lsr	rvc, rvc, #2		@ rvc <- block number (raw int)
  .ifndef sdhc_on_sd_mmc
	lsl	rvc, rvc, #9		@ rvc <- block's byte number (raw int)
  .endif
_sgbrs:	@ [continue/restart]
	bl	sd_fst			@ configure the spi interface
	bl	sd_sel			@ select sd
	@ put-cmd / restart
	@ 1- get
	bl	sd_get
	@ 2- write read-single-block cmd for block in rvc (CMD17)
	set	rvb, 0x40|17
	bl	sd_put
	lsr	rvb, rvc, #24
	bl	sd_put
	lsr	rvb, rvc, #16
	bl	sd_put
	lsr	rvb, rvc, #8
	bl	sd_put
	set	rvb, rvc
	bl	sd_put
	set	sv5, i0			@ sv5 <- schem int 0
	orr	sv5, sv5, #(1<<12)	@ sv5 <- 1024 max wait count (sch int)
sgrdwt:	@ 3- wait for read-ready
	sub	sv5, sv5, #4		@ sv5 <- max wait count, updated
	bl	sd_get
	eq	rvb, #0xfe		@ read-ready?
	it	ne
	eqne	sv5, #i0		@	if not, count not exceeded?
	bne	sgrdwt			@	if not, jump back to wait
	eq	rvb, #0xfe		@ read-ready?
	bne	_sgb_r			@	if not, jump to restart
	ldr	sv5, [sv3, #0]		@ sv5 <- saved-lnk restored from bfr
	@ get and save data (512 bytes)
	set	rvc, 0
sggtlp:	@ loop
	bl	sd_get
	strb	rvb, [sv3, rvc]
	add	rvc, rvc, #1
	eq	rvc, #0x0200
	bne	sggtlp
	@ get crc
	bl	sd_get
	bl	sd_get
	@ return
	bl	sd_dsl			@ de-select sd
	orr	lnk, sv5, #lnkbit0	@ lnk <- restored
	set	pc,  lnk

_sgb_r:	@ restart _sgb
	bl	sd_dsl			@ de-select sd
	b	_sgbrs

_func_	
_spb:	@ [internal only]
	@ sd-put-block internal func
	@ in:	rvc <- block number to write (scheme int)
	@ in:	sv3 <- buffer with block data to write to sd (schm bytevec)
	@ mods:	sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0
	lsr	rvc, rvc, #2		@ rvc <- block number (raw int)
  .ifndef sdhc_on_sd_mmc
	lsl	rvc, rvc, #9		@ rvc <- block's byte number (raw int)
  .endif
_spbrs:	@ [continue/restart]
	swi	run_no_irq
	bl	sd_fst			@ configure the spi interface
	bl	sd_sel			@ select sd
	@ put-cmd
	@ 1- get
	bl	sd_get
	@ 2- write write-single-block cmd for block in rvc (CMD24)
	set	rvb, 0x40|24
	bl	sd_put
	lsr	rvb, rvc, #24
	bl	sd_put
	lsr	rvb, rvc, #16
	bl	sd_put
	lsr	rvb, rvc, #8
	bl	sd_put
	set	rvb, rvc
	bl	sd_put
	set	rvb, 0xff 		@ put pseudo-crc
	bl	sd_put
	@ 3- wait for read-ready or non #xff response (desired: 0)
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct1
	bl	sd_get
spbct1:	@ check on zero read
	eq	rvb, #0
	bne	_spb_r
spbct2:	@ wait for #xff response following 0 response
	bl	sd_get
	eq	rvb, #0xff
	bne	spbct2
	@ write out the data
	set	rvb, 0xfe 		@ start token
	bl	sd_put
	set	rvc, 0
spbwlp:	@ write loop
	ldrb	rvb, [sv3, rvc]
	bl	sd_put
	add	rvc, rvc, #1
	eq	rvc, #0x0200
	bne	spbwlp
	@ write out pseudo-crc
	set	rvb, 0xff
	bl	sd_put
	set	rvb, 0xff
	bl	sd_put
	@ wait for write completion
	bl	sd_get
	set	rvc, 1
spbwt2:	@ wait loop
	bl	sd_get
	eq	rvb, #0
	bne	spbdon
	add	rvc, rvc, #1
	cmp	rvc, #(1 << 24)
	bmi	spbwt2
spbdon:	@ done
	bl	sd_get
	@ return
	bl	sd_dsl			@ de-select sd
	swi	run_normal
	orr	lnk, sv5, #lnkbit0	@ lnk <- restored
	set	pc,  lnk

_spb_r:	@ restart _spb
	bl	sd_dsl			@ de-select sd
	swi	run_normal
	b	_spbrs

_func_
sd_sel:	@ select SD-card subroutine
  .ifdef sd_cs
	set	rvb, sd_cs
	apnclr	sd_cs_gpio, rvb
    .ifdef sd_cs_wait
	wait	#sd_cs_wait	@ wait a bit (10 ms) to prevent SD comm errors
    .endif
  .endif
  .ifdef sd_cs_pin
	set	rvb, 1<<sd_cs_pin
	apnclr	sd_cs_gpio, rvb
    .ifdef sd_cs_wait
	wait	#sd_cs_wait	@ wait a bit (10 ms) to prevent SD comm errors
    .endif
  .endif
	set	pc,  lnk

_func_	
sd_dsl:	@ de-select SD-card subroutine
  .ifdef sd_cs
	set	rvb, sd_cs
	apnset	sd_cs_gpio, rvb
    .ifdef sd_cs_wait
	wait	#sd_cs_wait	@ wait a bit (10 ms) to prevent SD comm errors
    .endif
  .endif
  .ifdef sd_cs_pin
	set	rvb, 1<<sd_cs_pin
	apnset	sd_cs_gpio, rvb
    .ifdef sd_cs_wait
	wait	#sd_cs_wait	@ wait a bit (10 ms) to prevent SD comm errors
    .endif
  .endif
	set	pc,  lnk


_func_	
sd_get:	@ _sgb get sub-routine
	@ mods:	rva, rvb
	set	rvb, 0xff
_func_	
sd_put:	@ _sgb put sub-routine
	@ in:	rvb <- byte to write to sd-spi
	@ mods:	rva, rvb
  .ifdef spi_txrdy
	ldr	rva, =sd_spi
	ldr	rva, [rva, #spi_status]	@ ssta
	tst	rva, #spi_txrdy
	beq	sd_put
  .endif
	ldr	rva, =sd_spi
	and	rvb, rvb, #0xff
  .ifdef spi_datshft
	lsl	rvb, rvb, #spi_datshft
  .endif
	str	rvb, [rva, #spi_thr]	@ sdtx (sdat)
sd_gpw:	@ wait
	ldr	rvb, [rva, #spi_status]	@ ssta
	tst	rvb, #spi_rxrdy		@ sdrr
	beq	sd_gpw
  .ifdef spi_clr_status
	str	rvb, [rva, #spi_status]	@ clear status
  .endif
	ldr	rvb, [rva, #spi_rhr]	@ sdrx (sdat)
  .ifdef spi_datshft
	lsr	rvb, rvb, #spi_datshft
  .endif
	and	rvb, rvb, #0xff
	set	pc, lnk


.endif	@ sd_is_on_spi


.ifdef sd_is_on_mci


_func_
sd_ini:	@ initialize SD card function (repeat manually on #f)
	@ on exit:	sv1 <- #t/#f for initialization success/failure
	@ modifies:	sv1, sv3, sv4, sv5, rva, rvb, rvc
	bl	sd_slo			@ 400KHz, 1-bit bus, CLK enabled
	@ go to idle state: CMD0, arg=0
	set	sv4, i0			@ CMD0
	set	rvc, 0
	bl	sdpcmd			@ go to idle state
	@ get interface cond: CMD8, arg=0x1aa (mandatory for SDHC)
	set	sv4, i8
	ldr	rvc, =0x1aa
	bl	sdpcmd			@ CMD8, get if-cond, expect #x1aa
	set	sv5, i3			@ sv5 <- max trials for ACMD41 (sch int)
sd_in1:	@ set op-cond (ACMD41)
	@ CMD55, arg=0
	set	sv4, (55<<2)|i0
	set	rvc, 0
	bl	sdpcmd			@ CMD55
	@ get op-cond: CMD41, arg=0x40ff8000
	set	sv4, (41<<2)|i0
	set	rvc, 0x40ff8000
	bl	sdpcmd			@ CMD41, get op-cond, expect #xff800
	tst	rvb, #(1 << 31)		@ card idle?
	bne	sd_in2			@	if so,  jump to continue
	sub	sv5, sv5, #4		@ sv5 <- remaining trials
	eq	sv5, #i0		@ done with trials?
	beq	adr_flsfxt		@	if so,  exit with #f
	b	sd_in1			@ jump back to retry ACMD41
sd_in2:	@ card idle, continue (check SD vs SDHC w/r assembly mode)
	tst	rvb, #(1 << 30)		@ SDHC card?
  .ifdef sdhc_on_sd_mmc
	beq	adr_flsfxt		@	if not, exit w/#f (SDHC aps)
  .else
	bne	adr_flsfxt		@	if so,  exit w/#f (std SD aps)
  .endif
	@ set CID: CMD2, arg=0, long response
	set	sv4, i2
	orr	sv4, sv4, #(1<<28)
	set	rvc, 0
	bl	sdpcmd			@ CMD2, set CID (negative for long resp)
	@ get address: CMD3, arg=0
	set	sv4, i3
	set	rvc, 0
	bl	sdpcmd			@ CMD3, get address
	@ if get-address didn't work, exit with #f
	eq	rvb, #0			@ get-address failure?
	beq	adr_flsfxt		@	if so,  exit with #f
	@ save cRCA in sv1, sv2
	bic	rvc, rvb, #3
	orr	sv2, rvc, #i0
	raw2int	sv1, rvb
	@ send CSD: CMD9, arg=cRCA, long response
	set	sv4, i9
	orr	sv4, sv4, #(1<<28)
	bic	rvc, sv2, #3
	orr	rvc, rvc, sv1, lsr #2
	bl	sdpcmd			@ CMD9, send CSD (negativ for long resp)
	@ select card: CMD7, arg=cRCA
	set	sv4, i7
	bic	rvc, sv2, #3
	orr	rvc, rvc, sv1, lsr #2
	bl	sdpcmd			@ CMD7, select card, expect #x700
	@ switch to fast mode, wide bus
	bl	sd_fst			@ set MCI to 9-25 MHz,wide bus,CLK enab
	@ CMD55, arg=cRCA
	set	sv4, (55 << 2) | i0
	bic	rvc, sv2, #3
	orr	rvc, rvc, sv1, lsr #2
	bl	sdpcmd			@ CMD55, short response, expect #x920
	@ set bus to 4-bits: CMD6, arg=0x02
	set	sv4, i6
	set	rvc, 0x02
	bl	sdpcmd			@ CMD6, set bus size to 4b, expect #x920
	@ set block length to 512: CMD16, arg=512
	set	sv4, i16
	set	rvc, 512
	bl	sdpcmd			@ CMD16, set 512B blk len, expect #x900
	@ build bytevector for cRCA (return value)
	mkvu84	sv4			@ sv4 <- #vu8(space-for-4-items)
	bic	rvc, sv2, #3
	orr	rvc, rvc, sv1, lsr #2
	str	rvc, [sv4]
	set	sv1, sv4
	@ return (sv1 contains cRCA as bytevector)
	set	pc,  cnt

_func_
_sgb:	@ [internal only]
	@ sd-get-block internal func
	@ in:	rvc <- block number to be read (scheme int)
	@ in:	sv3 <- buffer to store block data (scheme bytevector)
	@ out:	sv3 <- updated buffer
	@ mods:	sv3, sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0	@ sv5 <- return address, saved
	lsr	rvc, rvc, #2		@ rvc <- block number (raw int)
  .ifndef sdhc_on_sd_mmc
	lsl	rvc, rvc, #9		@ rvc <- block's byte number (raw int)
  .endif
	b	_sgbrs			@ jump to hw specific routine

_func_
_spb:	@ [internal only]
	@ sd-put-block internal func
	@ in:	rvc <- block number to be write (scheme int)
	@ in:	sv3 <- buffer w/block data to wrt to sd (scheme bytevector)
	@ mods:	sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0	@ sv5 <- return address, saved
	lsr	rvc, rvc, #2		@ rvc <- block number (raw int)
  .ifndef sdhc_on_sd_mmc
	lsl	rvc, rvc, #9		@ rvc <- block's byte number (raw int)
  .endif
	b	_spbrs			@ jump to hw specific routine


.endif

_func_	
_snf:	@ _snf [internal only]
	@ sd-get-info internal func
	@ on entry:	sv1 <- (null . input-port-vector)
	@ on entry:	sv3 <- file name or other thing to store in info vector
	@ on exit:	sv2 <- preliminary file/fat16 info vector:
	@			#(sv3{eg. fil nm} _0_ root-dir-blk clstr2-blk _0_    _0_ 
	@			 blks-per-clstr  _0_ fat-blk      root-dir-blk clstr2-blk)
	@ on exit:	sv3 <- bytevector to use as data buffer (512 bytes)
	@ modifies:	sv2, sv3, sv5, rva, rvb, rvc
	@ returns via lnk
	bic	sv5, lnk, #lnkbit0
	@ allocate space for info vector
	set	rvb, 44
	bl	adr__alo
	set	rvc, 11<<8
	orr	rvc, rvc, #vector_tag	@ rvc <- vector tag
	str	rvc, [rva, #-4]
	set	rvc, rvb
	set	sv2, i0
_snfil:	subs	rvc, rvc, #4
	it	pl
	strpl	sv2, [rva, rvc]
	bpl	_snfil
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv2, rva, rvb		@ sv2 <- allocated block [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	vcsti	sv2, 0, sv3		@ set file name (original) into info vector
	vcsti	sv2, 4, sv4		@ save sv4 (eg. lnk of caller) in info vector temp space
	vcsti	sv2, 5, sv5		@ save lnk in info vector temp space
	@ allocate buffer
	set	rvb, 0x81
	lsl	rvb, rvb, #2
	bl	adr__alo
	set	rvc, 0x020000
	orr	rvc, rvc, #bytevector_tag	@ rvc <- bytevector tag
	str	rvc, [rva, #-4]
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv3, rva, rvb		@ sv3 <- allocated block [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@	get block 0
	set	rvc, i0
	bl	_sgb			@ sv3 <- block 0 data
	@ extract partition 0 block from data buffer
	set	rvc, 227
	lsl	rvc, rvc, #1
	ldrh	rva, [sv3, rvc]
	add	rvc, rvc, #2
	ldrh	rvb, [sv3, rvc]
	orr	rva, rva, rvb, lsl #16
	@ get partition 0 block
	lsl	rvc, rva, #2
	orr	sv4, rvc, #i0		@ sv4 <- partition 0 block number
	set	rvc, sv4
	bl	_sgb			@ sv3 <- partition 0 block data
	@ extract fat block, then root dir block, then cluster 2 block
	ldrb	rvc, [sv3, #13]
	lsl	rvc, rvc, #2
	orr	rvc, rvc, #i0 		@ rvc <-  blocks per cluster (scheme int)
	vcsti	sv2, 6, rvc 		@ save blocks per cluster in info vector
	ldrh	rva, [sv3, #14]
	add	sv4, sv4, rva, lsl #2 	@ sv4 <- fat blk = parttn 0 blk + # rsrvd sects (sch int)
	vcsti	sv2, 8, sv4 		@ save fat block in info vector temp space
	ldrb	rva, [sv3, #16]
	ldrh	rvb, [sv3, #22]
	mul	rva, rvb, rva
	add	rvb, sv4, rva, lsl #2 	@ rvb <- root dir blk=fat blk+fat cpies*sect/fat (sch int)
	ldrb	rva, [sv3, #17]
	ldrb	rvc, [sv3, #18]
	orr	rva, rva, rvc, lsl #8
	lsr	rva, rva, #4
	add	rva, rvb, rva, lsl #2 	@ rva <-  clstr 2 blk=root + #root entrs/16 (sch int)
	vcsti	sv2,  2, rvb
	vcsti	sv2,  3, rva
	vcsti	sv2,  9, rvb
	vcsti	sv2, 10, rva
	@ return
	vcrfi	sv4, sv2, 4		@ sv4 <- sv4, restored
	vcrfi	sv5, sv2, 5		@ sv5 <- lnk, restored
	set	rva, i0
	vcsti	sv2, 4, rva
	vcsti	sv2, 5, rva
	orr	lnk, sv5, #lnkbit0
	set	pc, lnk

	/* file-list:  list the files on the device
	   for files in top directory of FAT16.
	   file names are in 8.3 format (eg. "test.scm"), i.e. no long filenames. */
	PRIMIT	sdflst, ufun, 0
	@ on entry:	sv1 <- (null . input-port-vector)
	@ on exit:	sv1 <- list of file names
	@ modifies:	sv2, sv3, sv4, sv5, rva, rvb, rvc
	@ returns via cnt
	@ get sd-card parameters
	bl	_snf	 	@ sv2 <- #(fil nm  _0_ root-dir-blk clstr2-blk _0_    _0_ 
	@				  blks-per-clstr  _0_ fat-blk  root-dir-blk clstr2-blk)
	@			  sv3 <- buffer (512 bytes) (bytevector)
	@ build the file list
	set	sv4, null
flslp0:	@ scan over directory blocks
	vcrfi	rvc, sv2, 2		@ rvc <- current block (from dir block to cluster 2 block)
	vcrfi	rva, sv2, 10		@ rva <- cluster 2 block (scheme int)
	eq	rva, rvc		@ are we at end of directory?
	beq	flsdon			@ 	if so,  jump to finish up
	add	rva, rvc, #4
	vcsti	sv2, 2, rva		@ store next directory block in info vector
	bl	_sgb			@ sv3 <- dir block data
	set	rvc, -32		@ rvc <- offset to before first directory entry
flslp1:	@ process a directory block (512 bytes)
	add	rvc, rvc, #32		@ rvc <- offset of next directory entry
	tst	rvc, #0x0200		@ are we at end of this directory block?
	it	ne
	bne	flslp0			@ 	if so,  jump back to process next directory block
	ldrb	rva, [sv3, rvc]
	eq	rva, #0			@ is this the last entry in directory?
	it	eq
	beq	flsdon			@ 	if so,  jump to finish up
	@ check for deleted file or long file name (don't list those)
	eq	rva, #0xe5		@ is this a deleted file?
	itTT	ne
	addne	rva, rvc, #11		@	if not, rva <- offset to attributes field
	ldrbne	rva, [sv3, rva]		@	if not, rva <- file attributes
	eqne	rva, #0x0f		@	if not, is this a long file-name?
	beq	flslp1			@ 	if so,  jump back to process next directory entry
	@ dir entry found, get its file name into file-name list (sv4)
	orr	sv5, rvc, #i0		@ rvc <- dir entry offset (saved against _alo)
	set	rvb, 28			@ rvb <- 28 bytes to alloc (cons cell + str w/ <= 16 chrs)
	bl	adr__alo
	add	rvc, rva, #8
	str	rvc, [rva, #-4]
	set	rvc, 0x1000
	orr	rvc, rvc, #string_tag
	stmia	rva, {sv4, rvc}
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv4, rva, #32		@ sv4 <- allocated block [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	bic	rvc, sv5, #i0		@ rvc <- dir entry offset (restored)
	@ copy file name into new string
	car	sv1, sv4		@ sv1 <- new string for file name
	set	rvb, 0			@ rvb <- initial char offset in target string
flslp2:	@ loop over characters
	and	rva, rvc, #0x0f		@ rva <- char offset in target string, including header
	eq	rva, #11		@ done copying file name?
	beq	flslp4			@	if so,  jump to adjust file name size
	eq	rva, #0x08		@ are we at start of file name extension?
	bne	flslp3			@	if not, jump to keep going
	ldrb	rva, [sv3, rvc]		@ rva <- first char of extension
	eq	rva, #32		@ is first char of extension a space?
	beq	flslp4			@	if so,  no extension, jump to adjust file name size
	set	rva, '\.		@ rva <- dot
	strb	rva, [sv1, rvb]		@ store dot in file name in target string
	add	rvb, rvb, #1		@ rvb <- updated char offset in target string
flslp3:	@ keep going
	ldrb	rva, [sv3, rvc]		@ rva <- char from file name in directory entry
	eq	rva, #32		@ is char a space?
	itT	ne
	strbne	rva, [sv1, rvb]		@	if not, store char in string
	addne	rvb, rvb, #1		@	if not, rvb <- updated char offset in target string
	add	rvc, rvc, #1		@ rvc <- offset of next file name char in directory entry
	b	flslp2			@ jump back to continue copying file name chars
flslp4:	@ update file name (size, sub-dir) and go to next file
	bic	rvc, rvc, #0x0f		@ rvc <- offset to attributes part of file name
	orr	rvc, rvc, #0x0b		@ rvc <- offset to attributes part of file name
	ldrb	rva, [sv3, rvc]		@ rva <- attributes byte
	ands	rva, rva, #0x10		@ rva <- sub-dir bit, is this a sub-dir?
	beq	flslp5			@	if not, jump to continue
	set	rva, '\/		@ rva <- slash character
	strb	rva, [sv1, rvb]		@ store sub-dir indicator (/) in file name string
	add	rvb, rvb, #1		@ rvb <- updated char offset in target string
flslp5:	@ keep going
	lsl	rvb, rvb, #8		@ rvb <- number of chars shifted
	orr	rvb, rvb, #string_tag	@ rvb <- full tag for file name string
	str	rvb, [sv1, #-4]		@ update file name string size tag
	bic	rvc, rvc, #0x0f		@ rvc <- start offset of current directory entry
	b	flslp1			@ jump back to process next directory entry
flsdon:	@ finish up
	set	sv2, null
	set	sv3, sv2
	set	sv1, sv4
	@ return
	set	pc, cnt

_func_	
_nft:	@ get file name into fat16 format
	@ [internal only]
	@ on entry: sv4 <- file name string
	@ on exit:  sv1 <- file name string in FAT16 format
	@ modifies: sv1, sv5, rva, rvb, rvc
	bic	sv5, lnk, #lnkbit0 	@ sv5 <- lnk saved against toupcs
	set	rvb, 12
	bl	adr__alo
	set	rvc, 11 << 8
	orr	rvc, rvc, #string_tag
	str	rvc, [rva, #-4]
	set	rvc, 32 		@ rvc <- ascii space
	orr	rvc, rvc, rvc, lsl #8	@ rvc <- two  ascii spaces 	16-bits)
	orr	rvc, rvc, rvc, lsl #16	@ rvc <- four ascii spaces (32-bits)
	str	rvc, [rva]
	str	rvc, [rva, #4]
	str	rvc, [rva, #8]
	add	rva, rva, rvb		@ rva <- adrs of next fre cell lvl 2 rsv
	sub	sv1, rva, rvb		@ sv1 <- allocated block  [commit dest]
	orr	fre, rva, #0x02		@ de-reserve free-pointer [restart crit]
	ldr	rva, [sv4, #-4]
	lsr	rva, rva, #8
	cmp	rva, #13
	bpl	noequiv
	set	rvc, -1			@ rvc <- ofst to befor 1st chr in filnam
befordot: @ process file name
	bl	fnfguc			@ rvb <- new chr frm src fnam,rvc <-ofst
	eq	rvb, #'.		@ is char a dot?
	beq	afterdot		@	if so,  jump to process file ext
	eq	rvc, #8			@ are we at 9th char with no dot found?
	beq	noequiv			@	if so,  jump to no equivalence
	strb	rvb, [sv1, rvc]		@ store char in target
	b	befordot		@ jump back to keep processing file name
afterdot: @ process file extension
	sub	rvb, rva, rvc		@ rvb <- num chars in extension + 1
	cmp	rvb, #5			@ are there 4 or more chrs in extension?
	bpl	noequiv			@	if so,  jump to no equiv case
	bl	fnfguc			@ rvb <- 1st ext chr in uprcase, if alph
	strb	rvb, [sv1, #8]		@ store char in target
	bl	fnfguc			@ rvb <- 2nd ext chr in uprcase, if alph
	strb	rvb, [sv1, #9]		@ store char in target
	bl	fnfguc			@ rvb <- 3rd ext chr in uprcase, if alph
	strb	rvb, [sv1, #10]		@ store char in target
fnexit:	@ normal exit
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk
noequiv: @ exit with no equivalence
	set	sv1, i0
	b	fnexit

_func_
fnfguc:	@ get next char from source and convert to upper case, or exit if done
	@ on entry:	sv4 <- source file name string
	@ on entry:	rva <- length of file name string
	@ on entry:	rvc <- offset of previous char in file name
	@ on exit:	rvb <- char from file name, in upper case if alphabetic
	@ on exit:	rvc <- offset to returned char in file name
	@ modifies:	rvb, rvc
     	add	rvc, rvc, #1		@ rvc <- offset of char to get
     	cmp	rvc, rva		@ done getting chars?
	bpl	fnexit			@	if so,  jump to normal exit
     	ldrb	rvb, [sv4, rvc]		@ rvb <- char from source string
	@ convert to upper case, if alphabetic
	cmp	rvb, #'a
	it	mi
	setmi	pc,  lnk
	cmp	rvb, #'{
	it	mi
	bicmi	rvb, rvb, #32
	set	pc,  lnk

_func_	
_ffd:	@ find file in root directory
	@ [internal only]
	@ on entry: sv1 <- FAT16 8.3 file name string
	@ on entry: sv2 <- info vector, [2]=root-dir-block, [10]=cluster2-block
	@ on exit:  sv2 <- info vector, [2]=dir blk aftr that cntnng fil nm (if fnd) / aftr trgt
	@ on exit:  sv3 <- 512B dir block data buffer containing file name (if found)
	@ on exit:  rvc <- ofst of fil entry in dir blk + 15 (w/hdr/tag) (if fnd) or 0 (not fnd)
	@ side-effects: contents of sv2 index 2
	@ modifies: sv3, sv4, sv5, rva, rvb, rvc
	bic	sv4, lnk, #lnkbit0 	@ sv4 <- lnk saved against _sgb
ffdlp0:	
     	vcrfi	rvc, sv2, 2		@ rvc <- directory block to examine next
     	vcrfi	rva, sv2, 10		@ rva <- cluster2-block (end of directory)
     	eq	rva, rvc		@ directory exhausted?
	it	eq
	seteq	rvc, 0x0200		@	if so,  rvc <- indicator for no room left in dir
	beq	ffdfnf			@ 	if so,  jump to exit (file not found)
     	add	rva, rvc, #4		@ rva <- next directory block to examine
     	vcsti	sv2, 2, rva		@ store next block to examine in info vector
     	bl	_sgb			@ sv3 <- dir block data (512 bytes)
	set	rvc, -32		@ rvc <- offset to before first directory entry
ffdlp1:	
	add	rvc, rvc, #32		@ rvc <- offset of next directory entry
	tst	rvc, #0x0200		@ at end of dir-data block?
	bne	ffdlp0			@	if so,  jump back to process next block
	ldrb	rva, [sv3, rvc]		@ rva <- 1st character of dir entry
	eq	rva, #0			@ at end of directory?
	beq	ffdfnf			@ 	if so,  jump to exit (file not found)
	@ check for deleted file, long file name or directory (don't scan those)
	eq	rva, #0xe5		@ is this a deleted file?
	itTT	ne
	addne	rva, rvc, #11		@	if not, rva <- offset to attributes field
	ldrbne	rva, [sv3, rva]		@	if not, rva <- file attributes
	eqne	rva, #0x0f		@	if not, is this a long file-name?
	itT	ne
	andne	rva, rva, #0x10		@	if not, rva <- sub-dir bit
	eqne	rva, #0x10		@	if not, is this a sub-directory?
     	beq	ffdlp1			@	if so,  jump back to test next dir entry	
ffdlp2:	@ compare file name chars
	and	rvb, rvc, #0x0f		@ rvb <- ofst into fil nm being srchd for (incl. hdr)
	eq	rvb, #11		@ checked all file name chars and they match?
	beq	ffdxit			@ 	if so,  jump to exit (file found)
	ldrb	rva, [sv3, rvc]		@ rva <- character of dir entry
	ldrb	rvb, [sv1, rvb]		@ rvb <- character of target file name
	eq	rva, rvb		@ are chars the same?
	it	eq
	addeq	rvc, rvc, #1		@ 	if so,  rvc <- offset of next char
	beq	ffdlp2			@ 	if so,  jump back to compare next chars
	bic	rvc, rvc, #0x0f		@ rvc <- start offset of current dir entry
	b	ffdlp1			@ jump back to test next dir entry
ffdfnf:	@ exit for file not found
	set	rvb, rvc		@ rvb <- ofst in sv3 of fre dir entry / #x0200 if dir ful
	set	rvc, 0			@ rvc <- 0 == file not found indicator
ffdxit:	@ normal exit
     	orr	lnk, sv4, #lnkbit0
     	set	pc,  lnk


	/* find file and get its info
	   for a file in top directory of FAT.
	   file name is in 8.3 format (eg. "test.scm"), i.e. no long filenames.
	   called with file system locked (chain: FNFO <- prtifi | prtofi <- opnife | opnofe) */
	PRIMIT	sdfnfo, ufun, 3
	@ on entry:	sv1 <- (null . input-or-output-port-vector)
	@ on entry:	sv2 <- (null . input-or-output-port-vector) (symmetry of inbound call)
	@ on entry:	sv3 <- file name string
	@ on exit:	sv2 <- #(fname byt-in-fil|#f0 fil-sz bfr|#f0 byt-in-blk blk-in-clstr
	@			 blks-per-clstr fil-clstr-lst fat-blk root-dir-blk clstr2-blk)
	@ on exit:	sv3 <- file name string in FAT16 format (caps with spaces)
	@ modifies:	sv2, sv4, sv5, rva, rvb, rvc
	@ returns via lnk
	bic	sv4, lnk, #lnkbit0 	@ sv4 <- lnk saved against _snf
	bl	_snf		 	@ sv2 <- #(fil nm  _0_ root-dir-blk clstr2-blk _0_  _0_ 
					@	  blks/clstr _0_ fat-blk root-dir-blk clstr2-blk)
					@ sv3 <- buffer (512 bytes)	(bytevector)
	vcsti	sv2, 4, sv1		@ sav (null . port model) in inf vec tmp
	vcsti	sv2, 5, sv4		@ sav lnk in info vector temp space
	set	rva, null
	vcsti	sv2, 7, rva 		@ set file cluster list to '()
	@ convert file name to fat
	vcrfi	sv4, sv2, 0 		@ sv4 <- file name string
	bl	_nft 			@ sv1 <- FAT16-frmt fnam 8.3, 0 no equiv
	eq	sv1, #i0
	it	eq
	seteq	rva, f0
	beq	fnfxt
	@ find file
	bl	_ffd			@ sv2 <- updtd inf vec [2]=blk aftr fnam
					@ sv3 <- dir block data
					@ rvc <- fil ntry ofst in dr blk +15 or0
	eq	rvc, #0			@ file not found?
	it	eq
	seteq	rva, f0
	beq	fnfxt			@	if so,  go to file not found xit
	@ process the file-found case (fill-in the info vector)
	bic	rvc, rvc, #0x0f
	add	rvc, rvc, #28
	ldr	rva, [sv3, rvc]
	lsl	rva, rva, #2
	orr	rva, rva, #i0 		@ rva <- file size (scheme int)
	vcsti	sv2, 2, rva 		@ store file size in info vector
	@ build file blocks list (start blocks of file clusters)
	set	sv4, null
     	sub	rvc, rvc, #2
fnflop:	@ loop
     	ldrh	rva, [sv3, rvc]		@ rva <- start cluster of file (raw int)
	set	rvb, 0xff
	set	rvc, 0xf7
     	orr	rvb, rvc, rvb, lsl #8	@ rvb <- 0xfff7 = last cluster indicator
     	cmp	rvb, rva
     	bmi	fnfdon
     	sub	rvc, rva, #2
     	vcrfi	rvb, sv2, 6 		@ rvb <- number of blocks per cluster
     	lsr	rvb, rvb, #2
     	mul	rvc, rvb, rvc
     	vcrfi	rvb, sv2, 10		@ rvb <- cluster 2 block
     	add	sv1, rvb, rvc, lsl #2
     	lsl	rva, rva, #2
     	orr	sv5, rva, #i0
     	cons	sv4, sv1, sv4
	set	sv1, sv5
     	lsr	rvc, sv1, #10
     	vcrfi	rvb, sv2, 8 		@ rvb <- fat block
	add	rvc, rvb, rvc, lsl #2
	bl	_sgb 			@ sv3 <- data from next block of fat
	and	rvc, sv1, #0x03fc
	lsr	rvc, rvc, #1
     	b	fnflop
fnfdon:	@ reverse the block list
	set	sv5, null
fnfrvl:	@ loop
     	nullp	sv4			@ done reversing block list?
     	beq	fnfxt0			@ 	if so,  jump to finish up
     	snoc	sv1, sv4, sv4
     	cons	sv5, sv1, sv5
	b	fnfrvl
fnfxt0:	@ finish up
     	vcsti	sv2, 7, sv5 		@ stor fil clstr strt blk lst in inf vec
	@ get 1st data block of file and store it in info vector
	car	rvc, sv5
	bl	_sgb 			@ sv3 <- dat frm dat block of fil, 512B
	set	rva, i0
fnfxt:	@ return
     	vcsti	sv2, 1, rva		@ byte-in-file <- 0 found, 0.0 not found
     	vcsti	sv2, 3, sv3 		@ store data buffer in info vector
     	vcrfi	sv3, sv2, 0		@ sv3 <- file name, restored
     	vcrfi	sv1, sv2, 4		@ sv1 <- (null . port model), restored
     	vcrfi	sv5, sv2, 5		@ sv5 <- lnk, restored
	set	rva, i0
     	vcsti	sv2, 4, rva		@ set byte-in-block    to 0
     	vcsti	sv2, 5, rva		@ set block-in-cluster to 0
     	orr	lnk, sv5, #lnkbit0
	set	pc, lnk


	/* _SG0 [internal only]
	   file read-helper init function
	   prepare to get one or more chars from file */
	PRIMIT	sdfgc0, ufun, 1
	@ on entry:	sv1 <- ((fid #(fname page ofst ())) . port-vec)
	@ on exit:	sv2 <- #(fid page ofst) = fil start-stat partial copy
	@ on exit:	sv4 <- pointer to #(fname page offset ()) in full port
	@ preserves:	sv1, sv5
	@ modifies:	sv2, sv3, sv4, rva, rvb, rvc
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk (saved against zmaloc)
	cadar	sv4, sv1		@ sv4 <- #(fnam bytinfil fsz bfr bytinblk blkinclstr
					@	   blksperclstr fclstrlst tmp tmp tmp)
	set	rvb, 44			@ rvb <- 44 = data bytes to cpy fil desc
	bl	adr__alo
	set	rvc, rvb
qfl0lp:	@
	subs	rvc, rvc, #4
	ldr	sv2, [sv4, rvc]
	str	sv2, [rva, rvc]
	bpl	qfl0lp
	add	rva, rva, rvb		@ rva <- adrs of nxt fre cell, lvl 2 rsv
	sub	sv2, rva, rvb		@ sv2 <- copd desc inf blk [commit dest]
	orr	fre, rva, #0x02		@ de-reserve free-pointer [restart crit]
	orr	lnk, sv3, #lnkbit0	@ lnk <- restored
	set	pc,  lnk


	/* _SG1 [internal only]
	   file read-helper getc function */
	PRIMIT	sdfgc1, ufun, 4
	@ on entry:	sv1 <- ((port <reg> <n>) . port-vec) = full in port
	@ on entry:	sv2 <- value to preserve (eg. file desc partial copy)
	@ on entry:	sv4 <- file desc or its partial cpy (eg. for peek-)
	@ on entry:	sv5 <- value to preserve (eg. lnk of caller)
	@ on entry:	rvc <- value to preserve (eg. previous char read)
	@ on exit:	rvb <- ascii char read or eof (raw ascii char)
	@ on exit:	sv4 <- updated file descriptor (or its partial copy)
	@ preserves:	sv1, sv2, sv5, rvc (no zmaloc, no cons, no save)
	@ modifies:	sv3, sv4, rva, rvb
	@ check if file was read to end
     	vcrfi	rva, sv4, 1 		@ rva <- byte in file (scheme int)
     	vcrfi	rvb, sv4, 2 		@ rvb <- size of file (scheme int)
     	cmp	rva, rvb 		@ file read to end?
	itT	pl
	setpl	rvb, eof		@	if so,  rvb <- eof
	setpl	pc,  lnk		@	if so,  return
	@ update byte in file
     	add	rva, rva, #4
     	vcsti	sv4, 1, rva
	@ check if a new block needs loading
     	vcrfi	rva, sv4, 4 		@ rva <- byte in block (scheme int)
     	lsr	rvb, rva, #4 		@ rva <- byte in block / 4 (raw int)
     	cmp	rvb, #0x80 		@ not yet at end of 512-byte block?
     	bmi	qfgtc2			@      if so,  jump to get char
	@ prepare to load next block of file from SD card
     	vcsti	sv4, 8, sv5
     	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, almost saved
     	vcsti	sv4, 9, sv5
     	lsl	rvc, rvc, #2
     	orr	rvc, rvc, #i0
     	vcsti	sv4, 10, rvc
     	vcrfi	rvc, sv4, 5 		@ rvc <- block in cluster (scheme int)
     	add	rvc, rvc, #4 		@ rvc <- next block in cluster (sch int)
     	vcrfi	rvb, sv4, 6 		@ rvb <- blocks per cluster (scheme int)
     	cmp	rvc, rvb 		@ still within cluster?
     	bmi	qfgtc1			@      if so,  jump to keep going
     	vcrfi	sv3, sv4, 7 		@ sv3 <- cluster list
     	cdr	sv3, sv3 		@ sv3 <- rest of cluster list
     	vcsti	sv4, 7, sv3 		@ set updated cluster list in info vec
	set	rvc, i0 		@ rvc <- 0, block in cluster
qfgtc1:	@ load next block of file from SD card
     	vcsti	sv4, 5, rvc 		@ save updated block in cluster
     	vcrfi	sv3, sv4, 7 		@ sv3 <- cluster list
     	car	rvb, sv3 		@ rvb <- start block of current cluster
     	bic	rvc, rvc, #0x03
     	add	rvc, rvb, rvc		@ rvc <- new block to be read
     	vcrfi	sv3, sv4, 3 		@ sv3 <- buffer
     	bl	_sgb 			@ get new block data into buffer
     	vcrfi	rvc, sv4, 10
     	lsr	rvc, rvc, #2
     	vcrfi	sv5, sv4, 9
     	orr	lnk, sv5, #lnkbit0
     	vcrfi	sv5, sv4, 8
	set	rva, i0
     	vcsti	sv4, 4, rva 		@ set byte in block to 0
qfgtc2:	@ get char and update descriptor
     	vcrfi	sv3, sv4, 3 		@ sv3 <- buffer
     	vcrfi	rva, sv4, 4 		@ rva <- byte in block (scheme int)
     	int2raw rvb, rva		@ rvb <- byte in block (raw int)
     	ldrb	rvb, [sv3, rvb] 	@ rvb <- raw ascii char from buffer
     	add	rva, rva, #4
     	vcsti	sv4, 4, rva 		@ save next byte in block
	@ exit
	set	rva, i0
     	vcsti	sv4,  8, rva
     	vcsti	sv4,  9, rva
     	vcsti	sv4, 10, rva
	set	pc,  lnk

	/* _SG2 [internal only]
	   file read-helper function finish-up
	   extract string and update file descriptor function */
	PRIMIT	sdfgc2, ufun, 5
	@ on entry:	sv1 <- ((port <reg> <n>) . port-vector) = full input port
	@ on entry:	sv2 <- initial file descriptor partial copy (start of string in flash)
	@ on entry:	sv4 <- updated file descriptor (end of string in flash)
	@ on exit:	sv1 <- string to be parsed or eof-char
	@ preserves:	sv5
	@ modifies:	sv1, sv2, sv3, sv4, rva, rvb, rvc
	@ identify number of bytes to get
     	vcrfi	rva, sv2, 1		@ rva <- start byte in file (scheme int)
     	vcrfi	rvb, sv4, 1		@ rvb <- end byte in file (scheme int)
     	subs	rvb, rvb, rva		@ rvb <- number of chars to read * 4 (raw int), is it zero?
	itTT	eq
	seteq	rvb, eof<<8
	orreq	sv1, rvb, #char_tag
	seteq	pc,  lnk
	vcsti	sv4, 8, sv5		@ save sv5 in info vector
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, almost saved
	vcsti	sv4, 9, sv5		@ save lnk (via sv5) in info vector
	orr	sv1, rvb, #i0		@ sv1 <- number of chars to read, saved (scheme int)
	vcsti	sv4, 1, rva		@ set start byte back into file descr (to restart read)
	vcrfi	rva, sv2, 4		@ rva <- start byte in block  (scheme int)
	vcsti	sv4, 4, rva		@ set start byte in block back in descriptor
	vcrfi	rva, sv2, 5		@ rva <- starting block in cluster (scheme int)
	vcrfi	rvc, sv4, 5		@ rvc <- ending block in cluster (scheme int)
	vcsti	sv4, 5 ,rva		@ set starting block in cluster back in descriptor
	eq	rva, rvc		@ are starting and ending blocks the same? (set flag)
	vcrfi	sv3, sv4, 7		@ sv3 <- ending cluster list
	car	rvc, sv3		@ rvc <- ending cluster
	vcrfi	sv3, sv2, 7		@ sv3 <- starting cluster list
	car	rva, sv3		@ rva <- starting cluster
	vcsti	sv4, 7, sv3		@ set starting cluster list back in descriptor
	it	eq
	eqeq	rva, rvc		@      if so,  start = end clstr? (if start/end blks are)
	beq	qrdxp1			@      if so,  jump to keep going
	@ re-get starting block data (rva is starting cluster)
	bic	rva, rva, #0x03
	vcrfi	rvc, sv4, 5		@ rvc <- starting block in cluster (scheme int)
     	add	rvc, rvc, rva		@ rvc <- block to be read
     	vcrfi	sv3, sv4, 3		@ sv3 <- buffer
     	bl	_sgb
	@ keep going (rvb is number of bytes to get)
qrdxp1:	
	int2raw	rvb, sv1		@ rvb <- number of chars to read (raw int)
	@ allocate target string
	bl	adr__alo
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv2, rva, rvb		@ sv2 <- free string for chars read [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	rva, string_tag		@ rva <- full string tag for zero chars
	orr	rva, rva, sv1, lsl #6	@ rva <- full string tag with number of chars to get
	str	rva, [sv2, #-4]		@ store it in reserved memory block
	@ get characters from file into target string
	vcrfi	sv1, sv4, 9		@ sv1 <- lnk svd agnst _SG1 (_SG1 mods tmp spce in sv4)
	vcrfi	sv5, sv4, 8		@ sv5 <- sv5, restored
	set	rvc, 0			@ rvc <- offest to 1st char
qrdxp4:	
	bl	adr_sdfgc1		@ rvb <- raw char frm SD, sv4 <- pg & ofst updatd as needd
	strb	rvb, [sv2, rvc]		@ store it in target string
	strlen	rva, sv2
	add	rvc, rvc, #1		@ rvc <- offset of next char
	cmp	rvc, rva, lsr #2	@ done getting chars?
	bmi	qrdxp4			@	if not, jump to continue
	@ exit
	orr	lnk, sv1, #lnkbit0	@ lnk <- lnk, restored
	set	sv1, sv2		@ sv1 <- extracted string
	set	pc, lnk

	/* _fcl == pflclo
	   file close-output-port sub-function */
	PRIMIT	sdfclo, ufun, 2
	@ on entry:	sv1 <- (<mode>), if non-null, close as inp fil (forget write-on-close)
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vector) = full output port
	@ on entry:	sv4 <- descriptor of file being closed, from open file list (from ffhofl)
	nullp	sv1			@ perform write-on-close?
  	bne	adr_npofxt		@	if not, jump to exit
  	nullp	sv4			@ was file in open file list?
	beq	adr_npofxt		@	if not, jump to exit
	cadar	sv4, sv4		@ sv4 <- #(fname page offset ())
	bl	flok			@ acquire file system lock
  	vcrfi	rva, sv4, 4		@ rva <- byte-in-block
  	eq	rva, #i0		@ partial block to write?
  	beq	fatwrt			@ 	if not, jump to update fat and dir entry
  	bl	_fwr			@ write last, partial, block of file
	@ update file-size for partial block
  	vcrfi	rvb, sv4, 1		@ rvb <- byte-in-file (scheme int)
	sub	rvb, rvb, #(1 << 11)	@ rvb <- file-size minus block size (scheme int)
  	vcrfi	rva, sv4, 4		@ rva <- byte-in-block
  	bic	rva, rva, #0x03
  	add	rvb, rvb, rva		@ rvb <- file-size (scheme int)
  	vcsti	sv4, 1, rvb		@ store file size in file info vector
fatwrt:	@ write fat link sequence in fat -- _spb
  	set	sv2, sv4		@ sv2 <- fil inf vec, for upcmng ops (eg. _nft, for _ffd)
  	vcrfi	sv4, sv2, 7		@ sv4 <- list of file clstr start blks (start with last)
  	nullp	sv4			@ no cluster list in this file (empty file)?
  	beq	dirwrt			@ 	if so,  jump to update directory entries
  	vcrfi	sv3, sv2, 3		@ sv3 <- 512B buffer (for _sgb, _spb)
	@ process last cluster
  	bl	getfatblock		@ sv3 <- fat block containing file cluster at start of sv4
					@ sv1 <- last cluster number (scheme int)
  	lsr	rva, sv1, #2		@ rva <- last cluster number (raw int)
	set	rvb, 0xff<<8		@ rvb <- last cluster of file indicator (partial)
	orr	rvb, rvb, #0xff		@ rvb <- last cluster of file indicator (complete)
  	and	rvc, rva, #0xff
  .ifdef cortex
  	strh	rvb, [sv3, rvc, lsl #1]	@ str nxt clstr lnk in fat bfr at prv clstr ntry (in rva)
  .else
	lsl	rvc, rvc, #1
  	strh	rvb, [sv3, rvc]		@ str nxt clstr lnk in fat bfr at prv clstr ntry (in rva)
  .endif
fatclop:	@ loop over other clusters
  	cdr	sv5, sv4		@ sv5 <- rest of cluster list
  	nullp	sv5			@ done processing cluster list?
  	beq	fatspb			@ 	if so,  jump to write updated fat block to sd-card
  	car	rvb, sv5		@ rvb <- start blk of prev clstr of fil (wrkng bckwrds)
  	bl	blk2cl			@ rva <- clstr numb (raw int) of blk in rvb (sch int)
  	lsr	rvb, sv1, #10		@ rvb <- blk ofst of fat clstr being updtd (raw int)
  	lsr	rvc, rva, #8		@ rvc <- blk ofst of fat clstr of prev clstr (raw int)
  	eq	rvb, rvc		@ are clusters in same fat block?
  	bne	fatspb			@ 	if not, jump to write updated fat block to sd-card
  	lsr	rvb, sv1, #2		@ rvb <- next cluster number (relative to previous cluster)
  	lsl	rvc, rva, #2		@ 
  	orr	sv1, rvc, #i0		@ sv1 <- next cluster (for next round) (scheme int)
  	and	rvc, rva, #0xff
  .ifdef cortex
  	strh	rvb, [sv3, rvc, lsl #1]	@ str nxt clstr lnk in fat bfr at prev clstr ntry (in rva)
  .else
	lsl	rvc, rvc, #1
  	strh	rvb, [sv3, rvc]		@ str nxt clstr lnk in fat bfr at prev clstr ntry (in rva)
  .endif
  	set	sv4, sv5
  	b	fatclop
fatspb:	@ write updated fat block to sd-card
	lsr	rva, sv1, #10		@ rva <- block of fat currently being updated (raw int)
	vcrfi	rvc, sv2, 8
  	add	rvc, rvc, rva, lsl #2	@ rva <- block of fat currently being updated (scheme int)
  	bl	_spb			@ write updated fat block to sd-card
	@ check if done, and, if not, start processing next needed cluster block of fat
  	cdr	sv5, sv4		@ sv5 <- rest of cluster list
  	nullp	sv5			@ done processing cluster list?
  	beq	fatwrt4			@ 	if so,  jump to continue
  	vcsti	sv2, 7, sv1		@ store next cluster in info vector
  	set	sv4, sv5
  	bl	getfatblock		@ sv3 <- fat block containing file cluster at start of sv4
					@ sv1 <- cluster number (scheme int)
  	lsr	rva, sv1, #2		@ rva <- cluster number (raw int)
  	vcrfi	rvb, sv2, 7		@ rvb <- next cluster (scheme int)
  	lsr	rvb, rvb, #2		@ rvb <- next cluster (raw int)
  	and	rvc, rva, #0xff
  .ifdef cortex
  	strh	rvb, [sv3, rvc, lsl #1]	@ str nxt clstr lnk in fat bfr at prev clstr ntry (in rva)
  .else
	lsl	rvc, rvc, #1
  	strh	rvb, [sv3, rvc]		@ str nxt clstr lnk in fat bfr at prev clstr ntry (in rva)
  .endif
  	b	fatclop			@ jump back to process this fat cluster block

	
_func_
getfatblock:	@ subroutine getfatblock
  	bic	rvc, lnk, #lnkbit0 	@ rvc <- lnk, saved
  	car	rvb, sv4
  	bl	blk2cl			@ rva <- clstr num (raw int) of blk in rvb (sch int)
  	orr	lnk, rvc, #lnkbit0 	@ lnk <- restored
  	lsl	rvb, rva, #2
  	orr	sv1, rvb, #i0
  	lsr	rva, sv1, #10
  	vcrfi	rvc, sv2, 8		@ rvb <- fat block
  	add	rvc, rvc, rva, lsl #2
  	b	_sgb			@ sv3 <- fat block containing file cluster, returns via lnk

_func_
blk2cl:	@ subroutine to get cluster number from block number
	@ rva <- cluster number (raw int) of block in rvb (scheme int)
  	vcrfi	rva, sv2, 10		@ rva <- start block of cluster 2 (scheme int)
  	sub	rva, rvb, rva		@ rva <- start block*4 relative to cluster 2 (raw int)
  	lsr	rva, rva, #2		@ rva <- start block relative to cluster 2 (raw int)
  	vcrfi	rvb, sv2, 6		@ rvb <- blocks per cluster (scheme int)
  	lsr	rvb, rvb, #2
blk2sh:	@ loop
	lsrs	rvb, rvb, #1
	it	ne
	lsrne	rva, rva, #1
	bne	blk2sh
	add	rva, rva, #2		@ rva <- start cluster of file (raw int)
	set	pc,  lnk

fatwrt4: @ continue
  	vcsti	sv2, 7, sv4		@ store last cluster (as list) in info vector
dirwrt:	@ write directory entry in dir
  	vcrfi	sv4, sv2, 0		@ sv4 <- file name
  	bl	_nft 			@ sv1 <- FAT16-format fil nm (8.3) or 0 if no equiv
  	vcrfi	sv3, sv2, 3
  	vcrfi	rva, sv2, 9		@ rva <- root-dir block
  	vcsti	sv2, 2, rva		@ store root-dir-block at offset 2 (for _ffd)
  	bl	_ffd			@ sv2 <- updtd info vec [2]=dir blk aftr that for fil nm
					@ sv3 <- dir block data
					@ rvc <- fil ntry ofst in dir blk+15 (rawint)|0(filnotfnd)
					@ rvb <- file not fnd: fre ntry ofst in dir blk +hdr,raw
					@	 or #x0200 (dir full)
  	eq	rvb, #0x0200		@ dir full?
	it	ne
 	setne	rvc, rvb		@	if not, rvc <- fre ntry ofst in dir blk +hdr,raw
	bne	namwrt			@	if not, jump to continue
	bl	qfndfl			@ find a deleted file in dir to reclaim
	eq	rvc, #0x0200		@ no deleted file to reclaim?
	beq	opnfer			@	if so,  jump to report error
	bl	qfprg			@ purge delted file and its cluster list
	b	dirwrt			@ jump back to wrt dir entry to flsh, using reclmd spce
namwrt:	@ copy file name into dir buffer
	and	rvb, rvc, #0x0f
	eq	rvb, #11
	itTT	ne
	ldrbne	rva, [sv1, rvb]
	strbne	rva, [sv3, rvc]
	addne	rvc, rvc, #1
	bne	namwrt
	@ copy file size into sv3 (dir block buffer)
	bic	rvc, rvc, #0x0f
	add	rvc, rvc, #28		@ rvc <- offset of file size in dir buffer (incl. header)
	vcrfi	rva, sv2, 1		@ rva <- file size (scheme int)
	lsr	rva, rva, #2		@ rva <- file size (raw int)
	str	rva, [sv3, rvc]		@ store file size in dir buffer
	@ copy 1st cluster of file into sv3 (dir block buffer)
	vcrfi	sv4, sv2, 7		@ sv4 <- start blk of frst file clstr (listed sch int)
	nullp	sv4
	it	eq
	seteq	rva, 0
	beq	strwrt
	car	rvb, sv4		@ rvb <- start block of first file cluster (sch int)
	bl	blk2cl			@ rva <- clstr num (raw int) of blk in rvb (sch int)
strwrt:	@ perform write
	sub	rvc, rvc, #2		@ rvc <- offset of file start cluster in dir buffer
	strh	rva, [sv3, rvc]		@ store start cluster of file in dir data buffer
	@ write updated dir to sd-card
	vcrfi	rvc, sv2, 2		@ rvc <- target dir block + 1 (scheme int)
	sub	rvc, rvc, #4		@ rvc <- target dir block (scheme int)
	bl	_spb			@ write updated dir block to sd-card
pflclx:	@ unlock the file system and exit
	bl	funlok
	b	adr_npofxt

_func_
qfndfl:	@ find a deleted file in the root dir
	@ deleted dir-entries for normal file names are ok
	@ deleted entries for long file names are ok but start cluster will be 0 (file size 0)
	@ deleted sub-directory entries are skipped
  	vcrfi	rvc, sv2, 9
  	vcsti	sv2, 2, rvc		@ store dir start block in info vector, for scan
qfndf0:	@ loop over directory blocks
     	vcrfi	rvc, sv2, 2		@ rvc <- directory block to examine next
     	vcrfi	rva, sv2, 10		@ rva <- cluster2-block (end of directory)
     	eq	rva, rvc		@ directory exhausted?
	itT	eq
	seteq	rvc, 0x0200
	seteq	pc,  lnk
     	add	rva, rvc, #4		@ rva <- next directory block to examine
     	vcsti	sv2, 2, rva		@ store next block to examine in info vector
	bic	sv4, lnk, #lnkbit0 	@ sv4 <- lnk, saved
     	bl	_sgb			@ sv3 <- dir block data (512 bytes)
	orr	lnk, sv4, #lnkbit0
	set	rvc, 0
qfndf1:	@ loop over directory entries in block
     	ldrb	rva, [sv3, rvc]		@ rva <- 1st character of dir entry
	eq	rva, #0xe5		@ is this a deleted file?
	bne	qfndf5			@	if not, jump to loop back
	add	rva, rvc, #11		@ rva <- offset to file attributes
	ldrb	rva, [sv3, rva]		@ rva <- file attributes
	tst	rva, #0x10		@ is it a deleted sub-directory?
	it	eq
	seteq	pc,  lnk		@	if not, return (deleted file found)
qfndf5:	@ loop back
     	add	rvc, rvc, #32		@ rvc <- start offset of next dir entry
     	tst	rvc, #0x0200		@ at end of dir-data block?
     	bne	qfndf0			@	if so,  jump back to process next block
     	b	qfndf1			@ jump back to test next dir entry

_func_
qfprg:	@ purge a file's directory entry and its cluster list in fat
	@ on exit:	sv4 <- 0 (scheme int) if no clusters were purged
	add	rva, rvc, #26
	ldrh	rva, [sv3, rva]
	raw2int	sv4, rva		@ sv4 <- deleted file's start cluster (scheme int)
	vcrfi	rva, sv2, 2
	vcsti	sv2, 2, sv4
	set	sv4, rva		@ sv4 <- deleted file's directory block + 1 (scheme int)
	add	rvb, rvc, #32
	set	rva, 0
qfprg0:	@ erase file directory entry in buffer
	str	rva, [sv3, rvc]
	add	rvc, rvc, #4
	eq	rvc, rvb
	bne	qfprg0
	@ update directory block in flash
	sub	rvc, sv4, #4
	bic	sv4, lnk, #lnkbit0 	@ sv4 <- lnk, saved
     	bl	_spb			@ write updated block to file
	orr	lnk, sv4, #lnkbit0
	@ update fat (based on start cluster in sv2[2])
	vcrfi	sv4, sv2, 2		@ sv4 <- deleted file's start cluster (scheme int)
	eq	sv4, #i0		@ was this an empty file?
	beq	qfprgx			@	if so,  jump to skip cluster clearing in fat
	@ clear file clusters from fat
	bic	sv5, lnk, #lnkbit0 	@ sv5 <- lnk, nearly saved
	vcsti	sv2, 2, sv5		@ save lnk in info vector
qfprcl:	@ acquire fat block for cluster
	lsr	rvc, sv4, #10		@ rvc <- file cluster offset in fat (raw int)
     	vcrfi	rvb, sv2, 8 		@ rvb <- fat block (scheme int)
	add	rvc, rvb, rvc, lsl #2	@ rvc <- fat block where file cluster is (for _sgb)
     	bl	_sgb 			@ sv3 <- data from fat block containing cluster in sv4
	lsr	rvc, sv4, #10		@ rvc <- file cluster offset in fat (raw int)
     	vcrfi	rvb, sv2, 8 		@ rvb <- fat block (scheme int)
	add	sv5, rvb, rvc, lsl #2	@ sv5 <- fat blk where fil clstr is (svd for upcmng _spb)
qfprc0:	@ loop over file clusters to clear within this fat block
	int2raw	rvc, sv4
     	and	rvc, rvc, #0xff
	lsl	rvc, rvc, #1
	ldrh	rva, [sv3, rvc]		@ rva <- next cluster of file (raw int)
	set	rvb, 0
	strh	rvb, [sv3, rvc]		@ clear next cluster of file, in buffer
	lsr	rvc, sv4, #10		@ rvc <- previous cluster's offset in fat
	raw2int	sv4, rva		@ sv4 <- next cluster of file (scheme int)
     	ldr	rvb, =0xfff7		@ rvb <- last cluster indicator
     	cmp	rva, rvb		@ is this the last cluster?
	bpl	qfprc1			@	if so,  jump to write updated fat block to card
	lsr	rvb, sv4, #10		@ rvb <- next cluster's offset in fat
	eq	rvb, rvc		@ is next cluster in same fat block as previous cluster?
	beq	qfprc0			@	if so,  jump back to process next cluster
qfprc1:	@ write updated (cleared) fat block to flash
	set	rvc, sv5		@ rvc <- fat block where file cluster(s) is (for _spb)
	bl	_spb			@ write updated fat block to flash
	int2raw	rva, sv4		@ rva <- cluster to visit next (raw int)
     	ldr	rvb, =0xfff7		@ rvb <- last cluster indicator
     	cmp	rva, rvb		@ done visiting clusters?
	bmi	qfprcl			@	if not, jump back to clear next clusters
	@ restore link
	vcrfi	sv5, sv2, 2		@ sv5 <- lnk, nearly restored
	orr	lnk, sv5, #lnkbit0	@ lnk <- lnk, restored
qfprgx:	@ return
	set	pc,  lnk


_func_
_ffr:	@ _ffr
	@ find a free cluster in which to write a file's data buffer
	@ called with file system locked (chain: _ffr <- _fwr <- pflptc <- {pputs} <-  pflwrc)
	@ modifies:	sv2, sv3, sv4[2], sv5, rva-rvc
	bic	sv2, lnk, #lnkbit0 	@ sv2 <- lnk, saved
	@ allocate buffer for fat reading
	set	rvb, 0x81
	lsl	rvb, rvb, #2
  	bl	adr__alo
	set	rvc, 0x020000
	orr	rvc, rvc, #bytevector_tag @ rvc <- bytevector tag
	str	rvc, [rva, #-4]
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv3, rva, rvb		@ sv3 <- allocated block [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	orr	lnk, sv2, #lnkbit0 	@ lnk <- restored
	@ find a free cluster and add it to file cluster list
	vcrfi	rvc, sv4, 8		@ rvc <- FAT block
	vcsti	sv4, 2, rvc		@ store fat block in info vector, for scan
fatblockloop:	
	vcrfi	rvc, sv4, 2
	vcrfi	rva, sv4, 9
	cmp	rvc, rva		@ scanned the whole fat?
	itT	pl
	setpl	rvc, 0			@ 	if so,  rvc <- 0 (no free cluster)
	setpl	pc,  lnk		@ 	if so,  return with no free cluster indicator
	add	rva, rvc, #4
	vcsti	sv4, 2, rva
	bic	sv2, lnk, #lnkbit0 	@ sv2 <- lnk, saved
	bl	_sgb			@ sv3 <- contents of fat block
	orr	lnk, sv2, #lnkbit0 	@ lnk <- restored
	set	rvb, 0x0200
fatloop: @
	subs	rvb, rvb, #2
	bmi	fatblockloop
	ldrh	rva, [sv3, rvb]
	eq	rva, #0
	bne	fatloop
	@ calculate start block for cluster in this fat entry
	vcrfi	rvc, sv4, 2
	sub	rvc, rvc, #4		@ rvc <- fat block with free cluster (scheme int)
	vcrfi	rva, sv4, 8		@ rva <- fat block (scheme int)
	sub	rvc, rvc, rva		@ rvc <- cluster*8 (raw int)
	lsl	rvc, rvc, #6		@ rvc <- cluster*512 (raw int)
	add	rvc, rvc, rvb, lsr #1	@ rvc <- possible free cluster (raw int)
	sub	rvc, rvc, #2		@ rvc <- possib fre clstr, relative to cluster 2 (raw int)
	vcrfi	rva, sv4, 6		@ rva <- blocks-per-cluster (scheme int)
	lsr	rva, rva, #2
	mul	rvc, rva, rvc		@ rvc <- strt blk of poss. fre clstr, rel clstr 2 (raw int)
	vcrfi	rva, sv4, 10		@ rva <- cluster-2 block (scheme int)
	add	rvc, rva, rvc, lsl #2	@ rvc <- start block of poss. free cluster (scheme int)
	@ check for this cluster's start block on the open file list
	vcrfi	sv2, glv, 6		@ sv2 <- open file list
filelistloop:	
	nullp	sv2			@ done scanning open file list
	it	eq
	seteq	pc,  lnk		@ 	if so,  return w/start-block of free clstr in rvc
	caar	sv5, sv2		@ sv5 <- (handle #(fname ...))
	cadr	sv5, sv5		@ sv5 <- #(fname ...)
	ldr	rva, [sv5, #-4]		@ sv5 <- tag of file info vector
	lsr	rva, rva, #8		@ rva <- size of file info vector (raw int)
	eq	rva, #11		@ is this a sd-file info vector (size = 11)?
	bne	nextfile		@	if not, jump to scan rest of open file list
	@ check open sd-file's cluster list
	vcrfi	sv5, sv5, 7		@ sv5 <- file's cluster list
fileclusterloop:
	nullp	sv5			@ done scanning cluster list?
	beq	nextfile		@	if so,  jump to scan rest of open file list
	car	rva, sv5		@ rva <- cluster (in use) from cluster list
	eq	rva, rvc		@ is this the possible free cluster (i.e. cluster in use)?
	beq	fatloop			@	if so,  jump back to try a different free cluster
	cdr	sv5, sv5		@ sv5 <- rest of cluster list for this file
	b	fileclusterloop		@ jump back to scan rest of file's cluster list
nextfile:	
	cdr	sv2, sv2		@ sv2 <- rest of open file list
	b	filelistloop		@ jump back to scan rest of open file list


_func_	
_fwr:	@ _fwr == fwrfla
	@ write buffer to flash, 
	@ called with file system locked (chain: _fwr <- pflptc <- {pputs} <- pflwrc)
	@ on entry:	sv4 <- file descriptor
	@ on entry:	rvb <- char to be written to file next (to be saved, restored)
	@ on entry:	rvc <- lnk of caller (to be saved, restored)
	@ uses:		sv2, rva, rvb
	@ uses:		68 bytes of user-stack space
	@ identify file ID and block number for buffer
	stmdb	sp!, {rvb, rvc, lnk}	@ store scheme registers onto stack
	save	sv2, sv3, sv5
	vcrfi	rva, sv4, 5		@ rva <- block-in-cluster (scheme int)
	eq	rva, #i0
	bne	sdwr5
sdwr2:	@ find a free cluster (if any) for file data
	bl	_ffr			@ rvc <- start blk of free clstr on SD card, 0 if none
	eq	rvc, 0			@ free space available?
	bne	sdwr4			@	if so,  jump to continue
	swap	sv2, sv4, sv5
	bl	qfndfl			@ find a deleted file in dir to reclaim
	eq	rvc, #0x0200		@ no deleted file to reclaim?
	beq	sdwr3			@	if so,  jump to return with no room indicator
	bl	qfprg			@ purge delted file and its cluster list
	swap	sv2, sv4, sv5
	b	sdwr2
sdwr3:	@ absolutely no space left
	swap	sv2, sv4, sv5
	set	rva, 0			@ rva <- 0 (no room left indicator)
	b	sdwrxt			@ jump to exit		
sdwr4:	@ free cluster found, add it to the file's cluster list in info vector
	set	sv3, rvc
	vcrfi	sv5, sv4, 7
	cons	sv5, sv3, sv5
	vcsti	sv4, 7, sv5
sdwr5:	@ write data to block in file cluster
	vcrfi	sv3, sv4, 7 		@ sv3 <- cluster list
	car	rvb, sv3 		@ rvb <- start block of current cluster (scheme int)
	vcrfi	rvc, sv4, 5		@ rvc <- block-in-cluster (scheme int)
	bic	rvc, rvc, #0x03
	add	rvc, rvb, rvc		@ rvc <- destination block for write (scheme int)
	vcrfi	sv3, sv4, 3		@ sv3 <- buffer to write
	bl	_spb			@ write buffer to sd-card [modifies sv3, sv5, rva-rvc]
	@ update block-in-cluster
	vcrfi	rva, sv4, 5		@ rva <- block-in-cluster (scheme int)
	add	rva, rva, #4
	vcrfi	rvb, sv4, 6		@ rvb <- blocks per cluster (scheme int)
	cmp	rva, rvb
	it	pl
	setpl	rva, i0
	vcsti	sv4, 5, rva		@ store updated block-in-cluster in info vector
	@ update byte-in-file
	vcrfi	rva, sv4, 1		@ rva <- byte-in-file (scheme int)
	add	rva, rva, #(1 << 11)
	vcsti	sv4, 1, rva
sdwrxt:	@ exit
  	restor	sv2 sv3 sv5
  	ldmia	sp!, {rvb, rvc, lnk}
  	set	pc,  lnk


	/* _ptc == pflptc
	   file putc sub-sub-function
	   called with file system locked (chain: pflptc <- {pputs} <- pflwrc) */
	PRIMIT	sdfptc, ufun, 2
	@ on entry:	sv1 <- scheme char or string to write out
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vec) = full out port
	@ on entry:	sv3 <- saved lnk from caller of caller
	@ on entry:	sv4 <- file descriptor
	@ on entry:	sv5 <- saved lnk from caller
	@ on entry:	rvb <- ascii char to write + ofst of char in string
	@ on exit:	sv4 <- updated file descriptor
	@ preserves:	sv1, sv2, sv3, sv5, rvb
	@ modifies:	sv4, rva, rvc
	@ does buffer need to be written to flash?
  	vcrfi	rva, sv4, 4		@ rva <- byte-in-block (scheme int)
  	asr	rvc, rva, #2		@ rvc <- byte-in-block (raw int)
  	cmp	rvc, #512		@ byte-in-block larger than page size?
  	bmi	bfrupd
  	bic	rvc, lnk, #lnkbit0	@ rvc <- lnk, saved against fwrfla
  	bl	_fwr
  	orr	lnk, rvc, #lnkbit0	@ lnk <- restored 
  	eq	rva, #0			@ did write fail (if _fwr was called)?
	it	eq
	seteq	pc,  lnk		@	if so,  return
	set	rva, i0			@ rva <- byte-in-block for char in rvb
bfrupd:	@ write char in rvb to buffer in file descriptor sv4
	add	rvc, rva, #4		@ rvc <- updated byte-in-block
	vcsti	sv4, 4, rvc		@ store updatd byte-in-block in fil desc
	set	rvc, sv5		@ rvc <- saved lnk from caller, free sv5
	vcrfi	sv5, sv4, 3		@ sv5 <- buffer (scheme vec, gc-eable)
	lsr	rva, rva, #2		@ rva <- offset, incl. header (raw int)
	strb	rvb, [sv5, rva]		@ store character in buffer
	set	sv5, rvc		@ sv5 <- lnk from caller, restored
	set	pc,  lnk		@ return


	/* _fer == filers
	   erase an existing file before writing to it (pseudo-erasure where old
	   file is invalidated) and, prepare output buffer contents
	   called with file system locked (chain: _fer <- prtfrs <- opnofe) */
	PRIMIT	sdfers, ufun, 3
	@ on entry:	sv1 <- (null . input-or-output-port-vec)
	@ on entry:	sv2 <- (null . input-or-output-port-vec) (sym inbnd cal)
	@ on entry:	sv4 <- #(fname           byte-in-file|#f0  file-size
	@			buffer 	         byte-in-block     blk-in-clstr
	@			blocks-per-clstr file-clstr-lst    fat-block      
	@			root-dir-block   cluster2-block)
	@ on entry:	sv3 <- buffer (empty) {also referenced by sv4}
	@ modifies:	sv2, sv3, sv5, rva, rvb, rvc
	@ updates:	sv4 (page updated to 0, scheme int)
	@ returns via lnk
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved (made even if T2)
  	vcrfi	rvb, sv4, 1		@ rvb <- #i0=file found, #f0=not found
  	eq	rvb, #f0		@ file not found?
  	beq	flersx			@	if so,  jump to continue
	@ erase an exiting file
  	save	sv4, sv1, sv5		@ save registers against _nft and _ffd
  	vcrfi	sv4, sv4, 0		@ sv4 <- file name (for _nft)
  	bl	_nft 			@ sv1 <- FAT16-formatted file name (8.3)
  	car	sv2, dts		@ sv2 <- file info vector (for _ffd)
  	vcrfi	rva, sv2, 9		@ rva <- root-dir block
  	vcsti	sv2, 2, rva		@ store root-dir-blk at ofst 2, for _ffd
  	bl	_ffd			@ sv2 <- updtd info vec [2]=dir blk aftr
	@				  sv3 <- dir block data
	@       	 	 	  rvc <- ofst of file in dir blk (+ 15)
	bic	rvb, rvc, #0x0f
	set	rva, 0xe5
	strb	rva, [sv3, rvb]
	vcrfi	rvc, sv2, 2		@ rvc <- dir blk after fnam to erase
	sub	rvc, rvc, #4		@ rvc <- dir blk w/fnam to erase
	bl	_spb			@ overwrite dir block with file name
	restor	sv4, sv1, sv5		@ restore saved registers
	@ initialize file info vector and return
flersx:	@
	set	rva, null
	vcsti	sv4, 7, rva 		@ set file cluster list to '()
	set	rva, i0
	vcsti	sv4, 1, rva		@ set byte-in-file     to 0
	vcsti	sv4, 2, rva		@ set file size        to 0
	vcsti	sv4, 4, rva		@ set byte-in-block    to 0
	vcsti	sv4, 5, rva		@ set block-in-cluster to 0
	orr	lnk, sv5, #lnkbit0	@ lnk <- restored
	set	pc,  lnk		@ return


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



