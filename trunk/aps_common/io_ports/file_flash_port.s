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

	/* (main) file flash port, environment+obarray binding and port model */
	BNDVAR	"FILE", vfile
	VCTR	vfile, i0, filipr, filopr

	/* file input and output port-vectors	------------------------------*/
	VCTR	filipr, i1, val_npofxt, val_chrrdc, val_chrrdy, val_chpred, false, val_filgc0, val_filgc1, val_filgc2, val_filnfo, val_fillst, i0
	VCTR	filopr, i2, val_filclo, val_filwrc, val_chpwrt, val_filptc, val_filnfo, val_filers, (F_PAGE_SIZE << 2) | i0


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* file read-helper init function
	   prepare to get one or more chars from file */
	PRIMIT	filgc0, ufun, 1
	@ on entry:	sv1 <- ((fid #(fname page offset ())) . port-vector) = full input port
	@ on exit:	sv2 <- #(fid pg ofst) = fil strt-stat partl cpy or 0 if fil can't be read
	@ on exit:	sv4 <- pointer to #(fname page offset ()) in full input port
	@ preserves:	sv1, sv5
	@ modifies:	sv2, sv3, sv4, rva, rvb, rvc
	bic	rvc, lnk, #lnkbit0	@ rvc <- lnk (saved against flok, ffhofl)
	caar	sv2, sv1		@ sv2 <- file handle (i.e. port) (for ffhofl)
	tst	sv2, #0x01		@ is port a float (doing peek-char rather than read-char)?
	it	eq
	eoreq	sv2, sv2, #0x03
	bl	flok			@ acquire file system lock
	bl	ffhofl			@ sv4 <- full-port or (), sv2<-pre-sblst, sv3<-post-sblst
	orr	lnk, rvc, #lnkbit0	@ lnk <- restored
	@ verify that file is open and has flash presence (could be open, but not yet on flash)
	nullp	sv4			@ file open?
	itTT	ne
	carne	sv4, sv4		@	if so,  sv4 <- (fid #(fname page offset ()))
	snocne	sv2, sv4, sv4		@	if so,  sv2 <- fid, sv4 <- (#(fname page offset ()))
	carne	sv4, sv4		@	if so,  sv4 <- #(fname page offset ())
	itT	ne
	vcrfine rva, sv4, 1		@	if so,  rva <- page
	eqne	rva, #i0		@	if so,  is pg 0? (fl not yet wrttn 2 FLSH/rd 2 end)
	it	eq
	seteq	sv2, i0			@	if so,  sv2 <- 0, indic fil cant be read (sch int)
	beq	funlok			@	if so,  release file system lock, return via lnk
	@ copy sv4 contents into sv2
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk (saved against zmaloc)
	set	rvb, 12			@ rvb <- 16 = bytes needed to copy file descriptor
	bl	adr__alo		@ rva <- address of memory block = descriptor copy
	vcsti	rva, 0, sv2		@ store handle in copy block
	vcrfi	rvc, sv4, 1		@ rvc <- page from file descriptor
	vcsti	rva, 1, rvc		@ store page in copy block
	vcrfi	rvc, sv4, 2		@ rvc <- offset from file descriptor
	vcsti	rva, 2, rvc		@ store offset in copy block
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv2, rva, rvb		@ sv2 <- #vu8(fid fpag offst) copied positn [*commit dest*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	orr	lnk, sv3, #lnkbit0	@ lnk <- restored
	b	funlok			@ release file system lock, return via lnk


	/* file read-helper getc function */
	PRIMIT	filgc1, ufun, 4
	@ on entry:	sv1 <- ((fid #(fname page offset ())) . port-vector) = full input port
	@ on entry:	sv2 <- #(fid page offset) == file start-status partial copy
	@ on entry:	sv4 <- pointer to #(fname page offset ()) in full input port
	@ on entry:	sv5 <- value to preserve (eg. lnk of caller)
	@ on entry:	rvc <- value to preserve (eg. previous char read)
	@ on exit:	rvb <- ascii char read or eof (raw ascii char)
	@ on exit:	rvc <- entry value of rvb (eg. prev char in pchred, position in frdxp4)
	@ on exit:	sv4 <- updated file descriptor (or its partial copy)
	@ preserves:	sv1, sv2, sv5
	@ modifies:	sv3, sv4, rva, rvb, rvc
	set	rvc, rvb		@ rvc <- entry value of rvb, set for exit
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk (saved against flok)
	bl	flok			@ acquire file system lock
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk (restored)
	@ verify that file has flash presence
	vcrfi	sv3, sv4, 1		@ sv3 <- page address
	eq	sv3, #i0		@ is page zero? (file not yet written to FLASH)
	beq	fgetcx			@	if so,  jump to exit with eof-char
	vcrfi	rva, sv4, 2		@ rva <- offset
	asr	rva, rva, #2		@ rva <- offset (raw int)
	cmp	rva, #F_PAGE_SIZE	@ is offset larger than page size?
	bmi	fgetc2			@	if not, jump to continue
	@ find address of next page of file
	ldr	sv3, =F_START_PAGE	@ sv3 <- address of flash start page for files
fgetc1:	vcrfi	rva, glv, 11		@ rva <- address of end of file flash (crunch space)
	bic	rva, rva, #i0
	cmp	sv3, rva		@ is page >= end page?
	bpl	fgetcx			@	if so,  jump to exit with eof-char
	tbrfi	rva, sv3, 0		@ rva <- potential file ID on FLASH
	vcrfi	rvb, sv4, 1		@ rvb <- original page address
	tbrfi	rvb, rvb, 0		@ rvb <- file ID
	eq	rvb, rva		@ is it the ID we're looking for?
	itTTT	eq
	tbrfieq rva, sv3, 1		@	if so,  rva <- file block number
	vcrfieq rvb, sv4, 1		@	if so,  rvb <- original page address
	tbrfieq rvb, rvb, 1		@	if so,  rvb <- original block number
	increq	rvb, rvb		@	if so,  rvb <- next block
	it	eq
	eqeq	rvb, rva		@	if so,  is it the block number we're looking for?
	it	ne
	addne	sv3, sv3, #F_PAGE_SIZE	@	if not, sv3 <- start address of next page
	bne	fgetc1			@	if not, jump to scan next page
	@ update file descriptor
	vcsti	sv4, 1, sv3		@ store new page in file descriptor
	set	rva, 49			@ rva <- offset, byte 12 (scheme int)
	vcsti	sv4, 2, rva		@ store new offset in file descriptor
fgetc2:	@ read from page in descriptor sv4 and update descriptor
	vcrfi	sv3, sv4, 1		@ sv3 <- start address of page
	tbrfi	rvb, sv3, 2		@ rvb <- offset of last char in file
	vcrfi	rva, sv4, 2		@ rva <- offset to read from
	cmp	rva, rvb		@ is offset >= offset of last char in file?
	bpl	fgetcx			@	if so,  jump to exit with eof-char
	bytref	rvb, sv3, rva		@ rvb <- byte from FLASH
	eq	rvb, #0xFF		@ is byte the end-of-file byte?
	beq	fgetcx			@	if so,  jump to exit with eof-char
	incr	rva, rva		@ rva <- offset to next char
	vcsti	sv4, 2, rva		@ store updated offset in file descriptor
fgetc3:
	set	rva, 0			@ rva <- 0, files unlocked indicator
	set	sv3, BUFFER_START	@ sv3 <- address of main system buffer
	vcsti	sv3, FILE_LOCK, rva	@ set file system to unlocked state	
	set	pc,  lnk
fgetcx:	@ exit with eof
	set	rvb, eof		@ rvb <- eof
	b	fgetc3

	/* file read-helper function finish-up
	   extract string and update file descriptor function */
	PRIMIT	filgc2, ufun, 5
	@ on entry:	sv1 <- ((fid #(fname page offset ())) . port-vector) = full input port
	@ on entry:	sv2 <- #(fid page offset) == file start-status partial copy
	@ on entry:	sv4 <- pointer to #(fname page offset ()) in full input port
	@ on exit:	sv1 <- string to be parsed or eof-char
	@ preserves:	sv5
	@ modifies:	sv1, sv2, sv3, sv4, rva, rvb, rvc
	@ use sv1 = start of what to read and sv4 = end of what to read to find num chars to read
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
	save	sv3			@ dts <- (lnk ...)
	bl	flok			@ acquire file system lock
	vcrfi	sv3, sv2, 2		@ sv3 <- start offset of file read
	vcrfi	rvc, sv4, 2		@ sv5 <- end offset of file read
	vcsti	sv4, 2, sv3		@ set start offset back into file descr (to restart read)
	vcrfi	rva, sv2, 1		@ rva <- start page address of file read
	vcrfi	rvb, sv4, 1		@ rvb <- end page address of file read
	vcsti	sv4, 1, rva		@ set start page back into file descriptor (to restart read)
	tbrfi	rva, rva, 1		@ rva <- file read start block number
	tbrfi	rvb, rvb, 1		@ rvb <- file read end block number
	sub	rvb, rvb, rva		@ rvb <- number of blocks spanned by read * 4
	asr	rvb, rvb, #2		@ rvb <- number of blocks spanned by read
	set	rva, F_PAGE_SIZE	@ rva <- file page size
	sub	rva, rva, #12		@ rva <- file page size minus header (for blocks > 0)
	muls	rvb, rva, rvb		@ rvb <- num chars for pages spanned by read, is it zero?
	itEEE	eq
	subeq	rvb, rvc, sv3		@	if so,  rvb <- number of chars to read * 4
	subne	rvb, rvb, sv3, ASR #2	@	if not, rvb <- num chars to rd adjstd for strt ofst
	addne	rvb, rvb, rvc, ASR #2	@	if not, rvb <- num chars to rd adjstd for end ofst
	lslne	rvb, rvb, #2		@	if not, rvb <- number of chars to read * 4
	eq	rvb, #0			@ is number of chars zero (i.e. end of file)?
	it	eq
	ldreq	sv1, =eof_char		@	if so,  sv1 <- eof_char
	beq	frdxxt			@	if so,  exit with eof_char
	orr	sv3, rvb, #i0		@ sv3 <- num chars to read, saved against zmaloc (sch int)
	asr	rvb, rvb, #2		@ rvb <- number of chars to read (raw int)
	@ allocate target string
	bl	adr__alo		@ rva <- address of reserved memory block
	add	rva, rva, rvb		@ rva <- address of next free cell (level 2 reserved)
	sub	sv2, rva, rvb		@ sv2 <- free string for chars read [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	rva, string_tag		@ rva <- full string tag for zero chars
	orr	rva, rva, sv3, LSL #6	@ rva <- full string tag with number of chars to get
	str	rva, [sv2, #-4]		@ store it in reserved memory block
	@ get characters from file into target string
	bl	funlok			@ release file system lock, for pflgc1
	set	rvb, i0			@ sv3 <- offest to 1st char (scheme int)
frdxp4:	bl	getc1			@ rvb <- chr frm flsh, rvc <- rvb, sv4 <- upd pg,ofst as ndd
	bytsetu	sv2, rvc, rvb		@ store it in target string
	strlen	rva, sv2		@ rva <- number of chars to get (scheme int) (modifies rvb)
	add	rvb, rvc, #4		@ rvb <- offset of next char
	cmp	rvb, rva		@ done getting chars?
	it	mi
	bmi	frdxp4			@	if not, jump to continue
	bl	flok			@ acquire file sys lock, to own lock when exit releases it
	set	sv1, sv2		@ sv1 <- extracted string
frdxxt:	@ finish up
	restor	sv3			@ sv3 <- lnk,				dts <- (...)
	orr	lnk, sv3, #lnkbit0	@ lnk <- restored
	b	funlok			@ release file system lock, return via lnk

	/* file info -- return in sv2 information about the file whose name string is in sv3
	   (best with scheme interrupts off) */
	PRIMIT	filnfo, ufun, 3
	@ on entry:	sv1 <- (null . input-or-output-port-vector)
	@ on entry:	sv2 <- (null . input-or-output-port-vector) (symmetry of inbound call)
	@ on entry:	sv3 <- file name string
	@ on exit:	sv2 <- #(fname page offset ()) or 0.0 if file not found
	@ modifies:	sv2, sv4, sv5, rva, rvb, rvc
	@ returns via lnk
	set	sv1, sv3		@ sv1 <- file name string (for stsyeq), and saved
	set	sv4, sv2		@ sv4 <- (null . input-or-output-port-vector), saved
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved (and made even if Thumb2)
	ldr	sv2, =F_START_PAGE	@ sv2 <- address of flash start page for files
finfo0:	@ loop
	vcrfi	rva, glv, 11		@ rva <- address of end of file flash (crunch space)
	bic	rva, rva, #i0
	cmp	sv2, rva		@ is page >= end page?
	it	pl
	setpl	sv2, f0			@	if so,  sv2 <- 0.0 (file not found)
	bpl	finfo2
	tbrfi	rva, sv2, 0		@ rva <- potential file ID
	and	rva, rva, #0xff
	eq	rva, #0xfd		@ is this an active file page?
	bne	finfo1			@	if not, jump to scan next page
	tbrfi	rva, sv2, 1		@ rva <- file block number
	eq	rva, #i0		@ is it block num 0? (i.e. the start page of a file)
	bne	finfo1			@	if not, jump to scan next page
	add	sv2, sv2, #20		@ sv2 <- address of file name in page	
	bl	stsyeq			@ are file names equal?
	sub	sv2, sv2, #20		@ sv2 <- address of start of page
finfo1:	itT	ne
	addne	sv2, sv2, #F_PAGE_SIZE	@	if so,  sv2 <- next page
	bne	finfo0			@	if so,  jump to scan next page
finfo2:	@ finish up
	set	rvb, 20			@ rvb <- 28 = bytes needed to update open file list
	bl	adr__alo		@ rva <- address of memory block = updated open file list
	set	rvc, vector_tag		@ rva <- null vector tag
	orr	rvc, rvc, #0x0400	@ rva <- full vector tag with size (4 items)
	str	rvc, [rva, #-4]		@ fre+7 <- #( ___  ___  ___  ___ )
	ldr	rvc, [sv1, #-4]		@ rvc <- file name string tag
	lsr	rvc, rvc, #6		@ rvc <- #bytes in file name (scheme int)
	add	rvc, rvc, #0x5C		@ rvc <- #byts in fil nm + 3 + ofst to dat (scheme int)
	bic	rvc, rvc, #0x0C		@ rvc <- offset to data in file, word aligned (scheme int)
	stmia	rva!, {sv1, sv2, rvc}	@ fre <- #(fname page offset ___  ___ )
	set	rvc, null
	stmia	rva!, {rvc}		@ fre <- #(fname page-address data-offset () ___ )
	stmia	rva!, {rvc}		@ fre <- #(fname page-address data-offset () ())
	sub	sv2, rva, rvb		@ sv2 <- updated open file list [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@ return
	set	sv3, sv1		@ sv3 <- file name, restored
	set	sv1, sv4		@ sv1 <- (null . input-or-output-port-vector), restored
	orr	lnk, sv5, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk		@ return

	/* return a list of files on this device (flash) */
	PRIMIT	fillst, ufun, 1
	@ on entry:	sv1 <- (null . input-port-vector)
	@ on exit:	sv1 <- list of file names
	@ returns via cnt
	set	sv1, null		@ sv1 <- () = initial list cdr
	set	sv3, F_START_PAGE	@ sv3 <- address of flash start page for files
	sub	sv3, sv3, #F_PAGE_SIZE	@ sv3 <- page before first file page
	vcrfi	sv4, glv, 11		@ sv4 <- address of end of file flash (crunch space)
	bic	sv4, sv4, #i0
files0:	@ loop over pages of file flash
	add	sv3, sv3, #F_PAGE_SIZE	@ sv3 <- next page
	cmp	sv3, sv4		@ is page >= end page?
	it	pl
	setpl	pc,  cnt		@	if so,  return with list of file names
	tbrfi	rva, sv3, 0		@ rva <- potential file ID
	and	rva, rva, #0xff		@ rva <- lower byte of potential ID
	eq	rva, #0xfd		@ rva <- is this a valid ID (non-erased file)?
	bne	files0			@	if not, jump to scan next page
	tbrfi	rva, sv3, 1		@ rva <- file block number
	eq	rva, #i0		@ is it block number 0? (i.e. the start page of a file)
	bne	files0			@	if not, jump to scan next page
	set	sv2, sv1
	add	sv1, sv3, #20		@	if so,  sv1 <- file-name == file name adrs in pg
	cons	sv1, sv1, sv2		@	if so,  sv1 <- (file-name ...)
	b	files0			@ jump to scan next page

	/* file close-output-port sub-function */
	PRIMIT	filclo, ufun, 2
	@ on entry:	sv1 <- (<mode>), if non-null, close as input file (i.e. no write-on-close)
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vector) = full output port
	@ on entry:	sv4 <- descriptor of file being closed, from open file list (from ffhofl)
	nullp	sv1			@ perform write-on-close?
	bne	adr_npofxt		@	if not, jump to exit
	nullp	sv4			@ was file in open file list?
	beq	adr_npofxt		@	if not, jump to exit
	cadar	sv4, sv4		@ sv4 <- #(fname page offset ())
	set	sv3, i0			@ sv3 <- 0 (scheme int) for fwrfla/gc protection
	bl	flok
	bl	fwrfla			@ write file buffer (from sv4) to flash
	bl	funlok			@ release file system lock
	b	adr_npofxt		@ return with npo

	/* file putc sub-sub-function */
	PRIMIT	filptc, ufun, 2
	@ on entry:	sv1 <- scheme char or string to write out
	@ on entry:	sv2 <- ((port <reg> <n> ...) . port-vector) = full output port
	@ on entry:	sv3 <- saved lnk from caller of caller
	@ on entry:	sv4 <- file descriptor
	@ on entry:	sv5 <- saved lnk from caller
	@ on entry:	rvb <- ascii char to write to file + offset of char in string (if string)
	@ on exit:	sv4 <- updated file descriptor
	@ preserves:	sv1, sv2, sv3, sv5, rvb
	@ modifies:	sv4, rva, rvc
	bic	rvc, lnk, #lnkbit0	@ rvc <- lnk, saved against fwrfla (and made even if Thumb2)
	@ does buffer need to be written to flash?
	vcrfi	rva, sv4, 2		@ rva <- offset
	asr	rva, rva, #2		@ rva <- offset (raw int)
	cmp	rva, #F_PAGE_SIZE	@ is offset larger than page size?
	it	pl
	blpl	fwrfla			@	if so,  sv4 <- upd desc, rva <- stat,wrt bfr to flsh
	orr	lnk, rvc, #lnkbit0	@ lnk <- restored (as Thumb)
	eq	rva, #0			@ did write fail?
	it	eq
	seteq	pc,  lnk		@	if so,  return
	@ write char in rvb to buffer in file descriptor sv4
	set	rvc, sv5		@ rvc <- saved lnk frm caller (frees sv5 for bffr below)
	vcrfi	rva, sv4, 2		@ rva <- offset
	vcrfi	sv5, sv4, 3		@ sv5 <- buffer (scheme vector, gc-eable)
	bytsetu	sv5, rva, rvb		@ store character in buffer	
	incr	rva, rva		@ rva <- updated offset
	vcsti	sv4, 2, rva		@ store updated offset in file descriptor
	vcsti	sv5, 2, rva		@ store updated offset in buffer
	set	sv5, rvc		@ sv5 <- saved lnk from caller, restored
	set	pc,  lnk		@ return

_func_	
fwrfla:	@ write buffer to flash, 
	@ on entry:	sv4 <- file descriptor
	@ on entry:	rvb <- char to be written to file next (to be saved, restored)
	@ on entry:	rvc <- lnk of caller (to be saved, restored)
	@ uses:		sv2, rva, rvb
	@ uses:		68 bytes of user-stack space
	@ identify file ID and block number for buffer
	stmfd	sp!, {rvb, rvc, lnk}	@ store scheme registers onto stack
	save	sv2
	vcrfi	sv2, sv4, 1		@ sv2 <- page address
	eq	sv2, #i0		@ does buffer contain page zero of file? (i.e. a new file)
	itTTT	ne
	tbrfine rvb, sv2, 1		@	if not, rvb <- block number of previous file page
	addne	rvb, rvb, #4		@	if not, rvb <- block number for buffer/new page
	tbrfine rva, sv2, 0		@	if not, rva <- ID of file
	bne	fwrfl1			@	if not, jump to continue
	@ find a new ID for file
	set	rva, 0xfd		@ rva <- #xfd, trial fil Id (#b0...0 1111 1101, scheme int)
	set	sv2, F_START_PAGE	@ sv2 <- address of flash start page for files
fwrfl0:	
	vcrfi	rvb, glv, 11		@ rvb <- address of end of file flash (crunch space)
	bic	rvb, rvb, #i0
	cmp	sv2, rvb		@ is page >= end page? (i.e. file ID is new)
	itT	pl
	setpl	rvb, i0			@	if so,  rvb <- 0, block number for new file
	bpl	fwrfl1			@	if so,  jump to continue with ID in rva
	tbrfi	rvb, sv2, 0		@ rvb <- potential file ID from flash
	eq	rva, rvb		@ is flash file ID equal to the tentative file ID?
	itTE	eq
	addeq	rva, rva, #0x100	@	if so,  rva <- new tentative file ID
	ldreq	sv2, =F_START_PAGE	@	if so,  sv2 <- adrs of start pg to scan for file ID
	addne	sv2, sv2, #F_PAGE_SIZE	@	if not, sv2 <- address of next page
	b	fwrfl0			@ jump to scan next page
fwrfl1:	@ store file ID and block number in buffer
	vcrfi	sv2, sv4, 3		@ sv2 <- buffer
	vcsti	sv2, 0, rva		@ store file ID in buffer
	vcsti	sv2, 1, rvb		@ store block number in buffer
	@ identify a free page in which to write buffer
	ldr	sv2, =F_START_PAGE	@ sv2 <- address of flash start page for files
fwrfl2:	
	vcrfi	rva, glv, 11		@ rva <- address of end of file flash (crunch space)
	bic	rva, rva, #i0
	cmp	sv2, rva		@ is page >= end page?
	bpl	fwrcrn			@	if so, jump to crunch files in flash (ret to fwrflc)
	tbrfi	rva, sv2, 0		@ rva <- potential free page in flash
	mvns	rva, rva		@ rva <- not(rva) & set eq flag if zero, i.e. page is free
	it	ne
	addne	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of next page
	bne	fwrfl2			@ jump to scan next page
fwrflc:	@ commit write to flash (destination page is in sv2)
	bl	wrtfla
	@ update file descriptor
	vcsti	sv4, 1, sv2		@ store (prev)page in file descriptor
	set	sv2, 49			@ sv2 <- (new)offset (12 as scheme int)
	vcsti	sv4, 2, sv2		@ store (new)offset in file descriptor
	@ erase crunch space if needed and return
	bl	fcsdel			@ erase crunch space if needed (cleanup)
	set	rva, 0xff		@ rva <- non-zero status = write o.k.
fwrfxt:	@ exit
	restor	sv2
	ldmfd	sp!, {rvb, rvc, pc}	@ return

	
_func_	
fwrcrn:	@ file crunching during file write
	@ on entry:	sv2 <- F_START_PAGE
	@ on entry:	sv4 <- file descriptor of file to write to flash after crunch
	@ on entry:	sp  <- (rvb, rvc, pc)
	@ modifies:	sv2, sv4, rva, rvb, rvc, lnk, dts
	@ returns to:	fwrflc (not lnk, not cnt)
	bl	ff1del			@ sv2 <- 1st page with pseudo deleted file, rva <- 0 if none
	eq	rva, #0			@ is flash completely full? (no pseudo-deleted page)
	beq	fwrfxt			@	if so,  ret w/rva=0 (raw int) - flash 100% full
	@ crunch files in flash (to free space for buffer)
	save	sv4			@ save sv4 (file descr vect of file to write after crunch)
	@ allocate memory for file descriptor and buffer
	bl	mkfdsc			@ sv4 <- blank output file descriptor (with buffer)
	bl	pgsctr			@ rva <- sector of deleted page whose address is in sv2
	add	rvc, rva, #1		@ rvc <- sector after that of deleted page
	@ copy from source FLASH page(s) to target FLASH page(s) (if src wasn't deltd)
	ldr	rvb, =flashsectors	@ rvb <- address of FLASH sector table
	ldr	rva, [rvb, rva, LSL #2]	@ rva <- address of start page of source sector
	ldr	rvb, [rvb, rvc, LSL #2]	@ rvb <- address of start page of next sector (end page)
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (dest, pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space (destination)
	vcsti	sv4, 2, rva		@ store src start page address in caller-tmp-storage of sv4
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ prepare to copy back from extra FLASH to file FLASH, or commit write if done crunching
	vcrfi	rva, glv, 11		@ rva <- address of crunch space (src start, pseudo sch int)
	bic	rva, rva, #i0		@ rva <- address of crunch space (source start)
	vcrfi	rvb, sv4, 1		@ rvb <- address of end of extra FLASH target (source end)
	vcrfi	rvc, sv4, 2		@ rvc <- start address of former source = dest for copy
	bl	flshcp			@ perform flash copy (sv4 updated)
	vcrfi	sv2, sv4, 1		@ sv2 <- address after end of copied pages (target)
	restor	sv4			@ restore sv4 (file descriptor)
	b	fwrflc			@ jump back to commit write to FLASH

	/* erase an existing file before writing to it (pseudo-erase,
	   old file invalidated) and, prepare output buffer contents */
	PRIMIT	filers, ufun, 3
	@ on entry:	sv1 <- (null . input-or-output-port-vector)
	@ on entry:	sv2 <- (null . in-or-out-port-vec) (sym inbound call)
	@ on entry:	sv4 <- #(name page offset buffer)
	@ modifies:	sv2, sv3, sv5, rva, rvb, rvc
	@ updates:	sv4 (page updated to 0, scheme int)
	@ returns via lnk
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved (made even if T2)
	vcrfi	sv2, sv4, 1		@ sv2 <- strt-pg of fil, #f0 if not fnd
	eq	sv2, #f0		@ file not found?
	beq	flers1			@	if so,  jump to continue
	tbrfi	sv3, sv2, 0		@ sv3 <- fil ID, from strt pg of old fil
	ldr	sv2, =F_START_PAGE	@ sv2 <- adrs of flash strt pg for fils
flers0:	@ loop
	vcrfi	rvb, glv, 11		@ rvb <- address of end of file flash
	bic	rvb, rvb, #i0		@ rvb <- address of flash crunch sector
	cmp	sv2, rvb		@ is page >= end page?
	bpl	flers1			@	if so,  jump to continue (done)
	tbrfi	rva, sv2, 0		@ rva <- potential file ID for page
	eq	sv3, rva		@ is page's fil ID that of fil to erase?
	it	eq
.ifndef	wrtflr
	bleq	wrtfla			@	if so,  overwrite page to ID 0
.else
	bleq	wrtflr			@	if so,  overwrite page to ID 0
.endif
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of next page
	b	flers0			@ jump to continue scanning pages
flers1:	@ initialize info and buffer
	orr	lnk, sv5, #lnkbit0	@ lnk <- restored
	vcrfi	sv5, sv4, 0		@ sv5 <- file name
	vcrfi	sv3, sv4, 3		@ sv3 <- buffer
	set	rva, i0			@ rva <- 0 (scheme int)
	vcsti	sv4, 1, rva		@ page <- 0 (scheme int)
	vcsti	sv3, 0, rva		@ buffer <- 'ID=0
	vcsti	sv3, 1, rva		@ buffer <- 'ID=0,block-number=0
	vcrfi	rvb, sv4, 2		@ rvb <- offset
	vcsti	sv3, 2, rvb		@ buffer <- 'ID=0,block-number=0,offset
	@ copy file name into buffer
flers2:	sub	rvb, rvb, #16		@ rvb <- ofst nxt wrd of fnam strng cpy
	sub	rvc, rvb, #80		@ rvc <- ofst in src strng, 20B blw bfr
	wrdref	rva, sv5, rvc		@ rva <- word from file name string
	wrdst	sv3, rvb, rva		@ store it in bfr (mods rvc)
	eq	rvb, #65		@ done copying file name string (+ tag)?
	bne	flers2			@	if not, jump to keep copying
	set	pc,  lnk		@ return with address in sv2

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*------------------------------------------------------------------------------
@   III.A.2. FILE system crunching
@
@	File system crunch for small memory and regular MCUs
@-----------------------------------------------------------------------------*/


_func_	
pgsctr:	@ on entry:	sv2{r5} <- start address of flash page of which sector is sought
	@ on exit:	rva{r2} <- flash sector number in which page is found (raw int)
	@ modifies:	rva{r2}, rvb{r3}
	set	rva, 0
pgsct0:	ldr	rvb, =flashsectors
	ldr	rvb, [rvb, rva, LSL #2]
	cmp	sv2, rvb
	itT	mi
	submi	rva, rva, #1
	setmi	pc,  lnk		@ return
	add	rva, rva, #1
	b	pgsct0

.ifdef	LIB_TOP_PAGE
		
_func_	
lbsctr:	@ on entry:	sv2{r5} <- start address of flash page of which lib sector is sought
	@ on exit:	rva{r2} <- flash sector number in which page is found (raw int)
	@ modifies:	rvb{r3}
	set	rva, 0
lbsct0:	ldr	rvb, =lib_sectors
	ldr	rvb, [rvb, rva, LSL #2]
	cmp	sv2, rvb
	itT	mi
	submi	rva, rva, #1
	setmi	pc,  lnk		@ return
	add	rva, rva, #1
	b	lbsct0

.endif

	
_func_
mkfdsc:	@ build a new (output) file descriptor
	@ on exit:	sv4 <- new extended file descriptor (5 items vs 4 in normal descriptor)
	@ modifies:	sv4, rva, rvb, rvc
	@ returns via:	lnk
	bic	sv4, lnk, #lnkbit0	@ sv4 <- lnk, saved (and made even if Thumb2)
	set	rvb, F_PAGE_SIZE	@ rvb <- flash page size (bytes)
	add	rvb, rvb, #28		@ rvb <- size of blk to alloc for file descr + buffer
	bl	adr__alo		@ rva <- address of free block
	set	rva, 0x0500		@ rva <- number of items in vector (5, shifted)
	orr	rva, rva, #vector_tag	@ rva <- vector tag for file descriptor
	str	rva, [fre, #-1]		@ store vector tag in file descriptor
	set	rva, i0			@ rva <- 0 (scheme int)
	str	sv4, [fre, #3]		@ store lnk (sv4) in file descr (for gc and to restore lnk)
	str	rva, [fre, #7]		@ store 0 in file descriptor (for gc)
	str	rva, [fre, #11]		@ store 0 in file descriptor (for gc)
	str	rva, [fre, #19]		@ store 0 in file descriptor (for gc)
	add	rva, fre, #27		@ rva <- address of buffer (8-byte aligned)
	str	rva, [fre, #15]		@ store buffer address in file descriptor
	set	rva, F_PAGE_SIZE<<8 	@ rva <- flash page size (bytes)
	orr	rva, rva, #bytevector_tag @ rva <- bytevector tag for buffer
	str	rva, [fre, #23]		@ store tag in buffer
	add	rva, fre, #3		@ rva <- address of start of file descriptor + buffer
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv4, rva, rvb		@ sv4 <- file descriptor [* commit destination*]
	orr	fre, rva, #0x02		@ fre <- next free cell [* de-reserve memory*]
	vcrfi	lnk, sv4, 0
	set	rva, i0
	vcsti	sv4, 0, rva
	orr	lnk, lnk, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk		@ return

_func_
ff1del:	@ find 1st flash page with pseudo-erased file
	@ on exit:	sv2 <- 1st flash page with pseudo-erased file
	@ on exit:	rva <- 0 if no pseudo-erased file found
	@ modifes:	sv2, rva, rvb, rvc
	@ returns via:	lnk
	ldr	sv2, =F_START_PAGE
	sub	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of previous page
	set	rva, 0			@ rva <- 0 (raw int), default return value indic flash full
	vcrfi	rvb, glv, 11		@ rvb <- address of end of file flash (crunch space)
	bic	rvb, rvb, #i0
fcrnlp:	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of next page
	cmp	sv2, rvb		@  is page >= end page (flash is completely full)?
	it	pl
	setpl	pc,  lnk		@	if so,  return rva=0 (raw int) - no pseudo-del file
	tbrfi	rvc, sv2, 0		@ rvc <- word 0 from potential deleted page in flash
	mvns	rvc, rvc
	beq	fcrnlp
	mvn	rvc, rvc
	and	rvc, rvc, #0xff		@ rvc <- byte 0 of potential deleted page
	eq	rvc, #0xfd		@ is this page in use?
	beq	fcrnlp			@	if so,  jump to scan next page
	set	rva, 1			@ rva <- 1 (indicates a pseudo-deleted page was found)
	set	pc,  lnk		@ return
	
_func_
fcsdel:	@ erase crunch space if needed
	@ modifies:	sv2, rva
	vcrfi	sv2, glv, 11		@ sv2 <- flash crunch space address (pseudo scheme int)
	bic	sv2, sv2, #i0		@ sv2 <- flash crunch space address
	ldr	rva, [sv2]		@ rva <- 1st word in crunch space
	mvns	rva, rva		@ rva <- inverted 1st word, is it zero (erased)?
	bne	ersfla			@	if not, jmp to erase extra flsh sect, ret via lnk
	set	pc,  lnk		@ return

_func_
flshcp:	@ copy frm src FLASH page(s) to dest FLASH page(s) (if src wasn't deltd or is lib)
	@ on entry:	rva <- source      start page
	@ on entry:	rvb <- source      end   page
	@ on entry:	rvc <- destination start page
	@ on entry:	sv4 <-	special output file descriptor:
	@			#(______  _________ caller-tmp-storage  buffer  ________)
	@ on exit:	sv4 <-	updated special output file descriptor:
	@			#(src-end-pg  dest-end-pg   cllr-tmp-strg  bfr  src-end-pg)
	@ modifies:	sv2, sv4, rva, rvb, rvc
	@ returns via:	lnk
	@ Note:		sec of 1st dest pg is ersd if 1st wrd in 1st dest pg is not #xffffffff
	@ Note:		erased source pages (corresponding to erased files) are not copied
	@ Note:		copy is from:	src-start-page <= flash-page < src-end-page
	@ Note:		this routine switches mode to run_no_irq and back to run_normal
	@ side-effects:	the open-file list (glv, 6) is updated with page destination addresses
	@		if a copied page is in the open-file list.
	vcsti	sv4, 4, rva		@ store source      start page in pseudo-file-descriptor
	vcsti	sv4, 0, rvb		@ store source      end   page in pseudo-file-descriptor
	vcsti	sv4, 1, rvc		@ store destination start page in pseudo-file-descriptor
	set	sv2, rvc		@ sv2 <- destination start page
	set	rvc, lnk		@ rvc <- lnk, saved
	ldr	rva, [sv2]		@ rva <- 1st word in destination start page
	mvns	rva, rva		@ is destination start page erased?
	beq	fcrnc1			@	if so,  jump to skip destination sector erasure
.ifndef	LIB_TOP_PAGE
	bl	ersfla			@ jump to erase destination sector
.else
  .ifdef SHARED_LIB_FILE
	bl	ersfla			@ jump to erase destination sector
  .else
	adr	lnk, fcrnc1
	ldr	rvb, =LIB_BOTTOM_PAGE
	cmp	sv2, rvb
	ldr	rvb, =LIB_TOP_PAGE
	cmppl	rvb, sv2
	bmi	ersfla
	b	libers
  .endif
.endif
	@ at this point: sv4 <-	special output file descriptor:
	@			#(src-end-pg  dest-start-pg caller-tmp-storage  bfr  src-start-pg)
_func_
fcrnc1:	@ copy from src FLASH pg in [sv4, 4] to target FLASH page in [sv4, 1] (if src wasn't deltd)
	vcrfi	sv2, sv4, 4		@ sv2 <- source page address
	vcrfi	rvb, sv4, 0		@ rvb <- address after end page
	cmp	sv2, rvb		@ done writing?
	it	pl
	setpl	pc,  rvc		@	if so,  return
	
.ifndef	LIB_TOP_PAGE
	tbrfi	rva, sv2, 0		@ rva <- 1st word of FLASH page
	and	rva, rva, #0xff		@ rva <- lower byte of 1st word
	eq	rva, #0xfd		@ is this a valid file (not deleted)?
	bne	fcrnc5			@	if not, (deleted page) skip copying to extra FLASH
.else
	vcrfi	rvb, glv, 12		@ rvb <- possible lib space bottom address
	nullp	rvb			@ any lib to check against?
	beq	flshc3			@	if not, jump to check for valid file
	@ check if source is a lib page
	cmp	sv2, rvb
	ldr	rva, =LIB_TOP_PAGE
	it	pl
	cmppl	rva, sv2
	bpl	flshc4			@	if so,  skip checking for file validity
	@ check if destination is a lib page
	vcrfi	rva, sv4, 1
	cmp	rva, rvb
	ldr	rvb, =LIB_TOP_PAGE
	it	pl
	cmppl	rvb, rva
	bpl	flshc4			@	if so,  skip checking for file validity

flshc3:	@ copying from file space, check if we're copying a valid file page (i.e. non-deleted)
	tbrfi	rva, sv2, 0		@ rva <- 1st word of FLASH page
	and	rva, rva, #0xff		@ rva <- lower byte of 1st word
	eq	rva, #0xfd		@ is this a valid file (not deleted)?
	bne	fcrnc5			@	if not, (deleted page) skip copying to extra FLASH
flshc4:	@ continue
.endif

	bl	fpgcpy			@ copy data from source FLASH page to destination FLASH page
	bl	foflup			@ update open-file list (if src page adrs is on that list)
	@ update target FLASH page address
	vcrfi	sv2, sv4, 1		@ sv2 <- target FLASH page adddress
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- adrs of nxt FLASH page in target sect (nxt target)
	vcsti	sv4, 1, sv2		@ store it back in RAM file descriptor
fcrnc5:	@ update source FLASH page address
	vcrfi	sv2, sv4, 4		@ sv2 <- source FLASH page address
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- address of next FLASH page in src sector (nxt src)
	vcsti	sv4, 4, sv2		@ store it back in non-heap RAM
	b	fcrnc1			@ jump to copy next page to extra FLASH

_func_
fpgcpy:	@ copy a flash page from src to dest (both in FLASH)
	@ on entry:	sv4 <-	special output file descriptor:
	@			#(caller-tmp-storage  dest-page  caller-tmp-storage  bfr  src-page)
	@ modifes:	sv2, rva, rvb
	@ returns via:	lnk
	vcrfi	sv2, sv4, 3		@ sv2 <- buffer's address
	set	rvb, 0			@ rvb <- 0, initial offset
fpgcp0:	cmp	rvb, #F_PAGE_SIZE	@ are we done copying?
	itTTT	mi
	vcrfimi	rva, sv4, 4		@	if not, rva <- source page address
	ldrmi	rva, [rva, rvb]		@	if not, rva <- data word from FLASH source
	strmi	rva, [sv2, rvb]		@	if not, store data word into target RAM buffer
	addmi	rvb, rvb, #4		@	if not, rvb <- offset in target
	bmi	fpgcp0			@	if not, jump to keep copying
	@ write non-heap RAM to target FLASH page
	vcrfi	sv2, sv4, 1		@ sv2 <- target FLASH page address
.ifndef	LIB_TOP_PAGE
	b	wrtfla			@ write data to target FLASH, return to caller via lnk
.else
  .ifdef SHARED_LIB_FILE
	b	wrtfla			@ write data to target FLASH, return to caller via lnk
  .else
	ldr	rvb, =LIB_BOTTOM_PAGE
	cmp	sv2, rvb
	ldr	rvb, =LIB_TOP_PAGE
	cmppl	rvb, sv2
	bmi	wrtfla
	b	libwrt
  .endif
.endif

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

	/* (fsc) */
	PRIMIT	"fsc", pfun, 0
	@ file space cleaning
	@ perform actual erasing of deleted flash files and pack them all
	@ at the bottom of file flash space, opening up potential space for
	@ flash libraries on shared-library systems
	@ allocate memory for file descriptor and buffer
	bl	mkfdsc			@ sv4 <- blank output file descriptor (with buffer)
ffcln0:	@ find 1st page with pseudo-erased file (return if none)
	bl	ff1del			@ sv2 <- 1st page with pseudo deleted file, rva <- 0 if none
	eq	rva, #0			@ done cleaning-up?
	beq	adr_npofxt		@	if so,  return
	@ copy sector of 1st pseudo-deleted page to extra flash
	bl	pgsctr			@ rva <- sector of page with pseudo del file (adrs in sv2)
	add	rvc, rva, #1		@ rvc <- sector after that of deleted page
	ldr	rvb, =flashsectors	@ rvb <- address of FLASH sector table
	ldr	rva, [rvb, rva, LSL #2]	@ rva <- address of start page of source sector
	ldr	rvb, [rvb, rvc, LSL #2]	@ rvb <- address of start page of next sector (end page)
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (dest, pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space (destination)
	vcsti	sv4, 2, rva		@ store src start page address in caller-tmp-storage of sv4
	bl	flshcp			@ perform flash copy (sv4 updated)
	@ bring that back into file flash (without deleted files)
	vcrfi	rva, glv, 11		@ rva <- address of crunch space (src start, pseudo sch int)
	bic	rva, rva, #i0		@ rva <- address of crunch space (source start)
	vcrfi	rvb, sv4, 1		@ rvb <- address of end of extra FLASH target (source end)
	vcrfi	rvc, sv4, 2		@ rvc <- start address of former source = dest for copy
	bl	flshcp			@ perform flash copy (sv4 updated)
	vcrfi	sv2, sv4, 1		@ sv2 <- address after end of copied pages = 1st free page
	@ fill-up
	vcrfi	rvc, glv, 11		@ rvc <- address of crunch space (pseudo scheme int)
	bic	rvc, rvc, #i0		@ rvc <- address of crunch space
ffcln1:	sub	rvc, rvc, #F_PAGE_SIZE	@ rvc <- possible source page address
	cmp	sv2, rvc		@ have we exhausted possible source pages?
	bpl	ffcln0			@	if so,  jump back to search for more deleted pages
	ldr	rva, [rvc]		@ rva <- 1st word of possible source page
	and	rva, rva, #0xff		@ rva <- lower byte of 1st word
	eq	rva, #0xfd		@ is this a valid file source page (not deleted, not free)?
	bne	ffcln1			@	if not, jump to continue fill-up
	@ copy source page to erased destination page
	vcsti	sv4, 4, rvc		@ store dest page (erased) in extendd pseudo-file-descr
	bl	fpgcpy			@ copy file data from source page to destination page
	bl	foflup			@ update open-file list (if src page adrs is on that list)
	@ perform pseudo-deletion of source page
	vcrfi	sv2, sv4, 3		@ sv2 <- buffer, from pseudo-file-descriptor
	set	rva, i0			@ rva <- 0 (scheme int), used for pseudo-deletion of file
	vcsti	sv2, 0, rva		@ store 0 in buffer (as file ID => pseudo-deleted file)
	vcrfi	sv2, sv4, 4		@ sv2 <- prior source page
@ 06/26/2014 -- it seems that wrtflr (eg. STM32) should be used here, if present
@	bl	wrtfla			@ overwrite prior source page to ID 0 (pseudo-deletion)
.ifndef	wrtflr
	bl	wrtfla			@ overwrite prior source page to ID 0 (pseudo-deletion)
.else
	bl	wrtflr			@ overwrite prior source page to ID 0 (pseudo-deletion)
.endif
	@ update destination page
	vcrfi	sv2, sv4, 1		@ sv2 <- prior destination page
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- possible new destination page
	ldr	rva, [sv2]		@ rva <- 1st word of possible new destination page
	mvns	rva, rva		@ is this possible new page cleared to write?
	bne	ffcln0			@	if not, jump back to search for more deleted pages
	vcsti	sv4, 1, sv2		@ store update destination in pseudo file descriptor
	b	ffcln1			@ jump to continue fill-up

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg



