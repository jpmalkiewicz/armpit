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
@	Code Entry Points:
@
@		flscfg		configure/initialize flash or flashing code
@		wrtfla		write file    page to flash
@		ersfla		erase file    flash sector
@		libwrt		write library page to flash (if libs in flash)
@		libers		erase library flash sector  (if libs in flash)
@
@	Data Addresses:
@
@		flashsectors	file    flash sector map
@		lib_sectors	library flash sector map    (if libs in flash)
@
@-----------------------------------------------------------------------------*/

/* ------------------------------------------------------------ */
/* 	on-chip flash 						*/
/* ------------------------------------------------------------ */

.ifndef	flash_is_extern

_func_
flscfg:	/* copy FLASH writing code to RAM */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- PMC_base
	@ ret:	via lnk
	ldr	rva, =flsRAM		@ rva <- start address of flashing code
	ldr	rvb, =flsRND		@ rvb <- end address of flashing code
	set	rvc, heaptop1		@ rvc <- RAM target address
	add	rvc, rvc, #4
hwiwt6:	read	cnt, rva, #0		@ cnt <- next flashing code instruction
	write	cnt, rvc, #0		@ store it in free RAM
	cmp	rva, rvb		@ done copying the flashing code?
	itT	mi
	addmi	rva, rva, #4		@	if not, rva <- next source adrs
	addmi	rvc, rvc, #4		@	if not, rvc <- next target adrs
	bmi	hwiwt6			@	if not, keep copying code to RAM
	@ return
	set	pc,  lnk

	/* define flash-writing code to be copied to RAM */
.data	0

.balign	4

  .ifndef cortex @ code for ARM7TDMI

flsRAM:	@ code to be copied to RAM, at fre+, such that exec is from RAM while
	@ FLASH is being written. i.e. (1) ldr lnk, =xyz (2) set pc, fre to init
	set	rvb, 0
	mvn	rvb, rvb
	bic	rvb, rvb, #0xff
	write	rva, rvb, #0x64		@ perform FLASH write
	@ wait for completion or timeout
	set	rva, 0			@ rva <- 0, for timeout
	set	rvb, 0
	mvn	rvb, rvb
	bic	rvb, rvb, #0xff
	read	rvb, rvb, #0x68		@ get status
	tst	rvb, #0x01		@ FRDY?
	addeq	rva, rva, #1
	tsteq	rva, #0x800000		@ rva <- timeout, approx 1 sec
	subeq	pc,  pc,  #36		@	if not, jump back to get status
	set	pc,  lnk		@ return
flsRND: @ end of ram code

  .else	@ code for cortex-M3/M4

flsRAM:	@ code to be copied to RAM, at fre+, such that execution is from RAM while
	@ FLASH is being written. i.e. do: (1) ldr lnk, =xyz (2) set pc, fre to initiate
	@ EEFC address = 0x400E0A00
	set	rvb, 0x40000000
	orr	rvb, rvb, #0x0E0000
	orr	rvb, rvb, #0x000A00
	write	rva, rvb, #0x04		@ perform FLASH write
	@ wait for completion or timeout
	nop				@ landing pad
	nop				@ landing pad
	nop				@ landing pad
	nop				@ landing pad
	nop				@ landing pad
	nop				@ landing pad
	nop				@ landing pad
	read	rva, rvb, #0x08		@ get status
	tst	rva, #0x01		@ FRDY?
	itT	eq
	subeq	rva, pc, #20		@	if not, jump back to get status
	seteq	pc,  rva
	set	pc,  lnk		@ return
flsRND: @ end of ram code

  .endif

.balign	4	@ make sure data section remains 4-byte aligned

.text		@ return to text section for remainder of code


_func_
wrtfla:	/* write to flash, sv4=r7 is file descriptor, sv2=r5 is page address */
_func_
libwrt:	/* write to on-chip lib flash (lib shares on-chip file flash) */
  .ifdef cortex
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
  .endif
	set	rvc, sv3		@ rvc <- sv3, saved
  .ifdef FLSH_WRTWS			@ if wait-states need to be changed
	write	FLSH_WRTWS<<8,EEFC_base,#0 @ EEFC_FMR  <- wait states=6 (errata)
  .endif
	@ copy buffer data from file descriptor (sv4) (RAM) to AT91SAM7 FLASH buffer (sv2)
	vecref	sv3, sv4, 3		@ rvb <- file data source buffer
	set	rvb, F_PAGE_SIZE	@ rvb <- last source offset
wrtfl0:	subs	rvb, rvb, #4		@ last word to copy?
	read	rva, sv3, rvb		@ rva <- word from data buffer
	write	rva, sv2, rvb		@ store word into flash buffer
	bne	wrtfl0			@	if not, jump to keep copying data to flash buffer
  .ifndef cortex
	write 2, int_base, #0x38	@ disconnect AIC
  .endif
	@ commit buffer to FLASH using code in RAM
  .if F_PAGE_SIZE == 256
	lsr	rvb, sv2, #8		@ sv1 <- target FLASH page (assumes 256 bytes page size)
  .else
	lsr	rvb, sv2, #9		@ sv1 <- target FLASH page (512B pg sz)
  .endif
	set	rva, 0x5A000001		@ rva <- flash write command (page zero)
	orr	rva, rva, rvb, LSL #8	@ rva <- flash write command for page in sv1
	set	rvb, heaptop1
	add	rvb, rvb, #4
  .ifndef cortex
	swi	isr_no_irq
  .endif
	adr	lnk, wrtfxt		@ lnk <- return address for after FLASH command
	set	pc,  rvb		@ jump to FLASH write routine in RAM
	
_func_
wrtfxt:	@ finish up
  .ifdef FLSH_WTSTA			@ if wait-states need to be changed
	write	FLSH_WTSTA<<8,EEFC_base,#0 @ EEFC_FMR  <- wait states = 3
  .endif
	set	sv3, rvc		@ sv3 <- restored
  .ifdef cortex
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
  .endif
	swi	run_normal		@ enable interrupts (user mode)
  .ifdef cortex
	set	pc,  lnk		@ return
  .else
	write	0, int_base, #0x38	@ reconnect AIC
	@ wait a bit (recovery?)
	wait	0x6000			@ wait, approx 1 ms
	@ check for errors (it that's possible)
	set	rvb, -1
	bic	rvb, rvb, #0xff
	read	rva, rvb, #0x68		@ rva <- status
	tst	rva, #0x01		@ FRDY?
	beq	wrterr
	set	pc,  lnk		@ return

wrterr:	@ write error other than 1/0
	raw2int	sv1, rva
	ldmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ restore scheme regs from stack
	b	adr__err
  .endif


_func_
ersfla:	/* erase flash sector that contains page address in sv2 */
_func_
libers:	/* erase on-chip lib flash sector (lib shares on-chip file flash) */
	@ copy #xffffffff to AT91SAM7 FLASH buffer (sv2)
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
  .ifdef FLSH_WRTWS			@ if wait-states need to be changed
	write	FLSH_WRTWS<<8,EEFC_base,#0 @ EEFC_FMR  <- wait states = 6,errata
  .endif
	set	rvc, sv2		@ rvc <- sv2, saved
	bl	pgsctr			@ rva <- sector number (raw int), of flash page in sv2
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	read	sv2, rvb, rva, LSL #2	@ sv2 <- start address of flash sector
  .ifndef flash_erase_sct		@ if not erasing by sector
ersfl1:	set	rva, 0			@ sv3 <- 0 = start offset
	mvn	rvb, rva		@ sv4 <- erase flash data = 0xFFFFFFFF
ersfl0:	cmp	rva, #F_PAGE_SIZE	@ done writing to flash buffer?
	itT	mi
	writemi	rvb, sv2, rva		@	if not, store next word into flash buffer
	addmi	rva, rva, #4		@	if not, sv3 <- next word offset
	bmi	ersfl0			@	if not, jump to keep copying data to flash buffer
  .endif
	@ commit buffer to FLASH using code in RAM
  .if F_PAGE_SIZE == 256
	lsr	rvb, sv2, #8		@ sv1 <- target FLASH page (assumes 256 bytes page size)
  .else
	lsr	rvb, sv2, #9		@ sv1 <- target FLASH page (512B pg sz)
  .endif
	set	rva, flash_erase_cmd	@ rva <- flash erase cmd (page 0)
	orr	rva, rva, rvb, LSL #8	@ rva <- flash erase cmd for page in sv1
	set	rvb, heaptop1
	add	rvb, rvb, #4
	adr	lnk, ersfxt		@ lnk <- return address for after FLASH command
	set	pc,  rvb		@ jump to FLASH write routine in RAM
_func_
ersfxt:	@ finish up or jump to erase next page of sector
  .ifndef flash_erase_sct		@ if not erasing by sector
	add	sv2, sv2, #F_PAGE_SIZE	@ sv2 <- next page address
	set	rvb, 0x0FFF
	ands	rvb, rvb, sv2		@ done erasing sector? (4kb = 16 pages of 256 bytes)
	bne	ersfl1			@	if not, jump back to erase more pages
  .endif
	@ exit
  .ifdef FLSH_WTSTA			@ if wait-states need to be changed
	write	FLSH_WTSTA<<8,EEFC_base,#0 @ EEFC_FMR  <- wait states = 3,normal
  .endif
	set	sv2, rvc		@ sv2 <- restored
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

.endif @ on-chip flash

/* ------------------------------------------------------------ */
/* 	code for external flash 				*/
/* ------------------------------------------------------------ */

.ifdef	flash_is_extern

_func_
flscfg:	/* configure external flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- PMC_base
	@ ret:	via lnk
  .ifdef SAM9_L9261
	@ initialize FLASH
	@ 1 x K9F4G08U0A-PCB0 (cf. k9xxg08uxa.pdf, ATMEL doc6255.pdf)
	@ PC0/NANDOE #RE, PC1/NANDWE #WE, PC14 #CE, PC15 Rdy/#Bsy,
	@ A21/NANDCLE CLE, A22/NANDALE ALE, D0-7 i/o
	write	1<<4,  env,       #0x10	@ PMC_PCER <- Enable clock/pwr for PIOC
	write	3,     pioc_base, #0x04	@ PIOC_PDR <- Disable GPIO for PC0, PC1
	write	rvb,   rva,       #0x70	@ PIOC_ASR <- NAND OE-WE Periph A, PC0-1
	write	1<<14, rva,       #0x10	@ PIOC_OER  <- CS (PC14) as gpio output
	write	rvb,   rva,     #io_set	@ PIOC_SODR <- CS (PC14) hi (desel NAND)
	rgcpbt	0xffffee00, #0x30, 3, 1	@ EBI_CSA  <- alloc CS3A SmartMedia/NAND
	set	rva, 0xffffec00		@ rva <- SMC base address
	write	0x01010101, rva, #0x30	@ SMC_SETUP_3 <- 2 clk RE/WE setup 12ns
	write	0x03030303, rva, #0x34	@ SMC_PULSE_3 <- 2 clk RE/WE pulse 12ns
	write	0x05050505, rva, #0x38	@ SMC_CYCLE_3 <- 5 clk Setup+Pulse+Hold
	write	0x020003,   rva, #0x3c	@ SMC_MODE_3  <- RE/WE ctl, 8-bit, 3 clk
	@ select FLASH and wait for ready
	write	1<<14, pioc_base, #io_clear
	rgwfbt	rva, #io_state, 15, 1
	/* copy file FLASH contents into shadow RAM */
	set	cnt, F_START_PAGE
cpyfl0:	@ configure flash for read operation with RAM address in cnt
	write8	0x00, 0x40200000, #0	@ set page read command in FLASH
	eor	rva,  rva, #(3 << 21)	@ rva <- adrs-wrt adrs (CS3) for FLASH
	write8	0x00, rva, #0		@ start byt in pg adrs lo=Col Adr.1 = 0
	write8	rvb,  rva, #0		@ start byt in pg adrs hi,4bit,Col Adr.2
	lsr	rvb,  cnt, #11
	and	rvb,  rvb, #0xff
	write8	rvb,  rva, #0		@ pg-in-blk (b0-5) & adr lo (Row Adr.1)
	lsr	rvb,  cnt, #19
	and	rvb,  rvb, #0x1f
	write8	rvb,  rva, #0		@ set block address middle (Row Adr. 2)
	write8	0x00, rva, #0		@ blk adrs hi (4 bits, Row Adr. 3)
	eor	rva,  rva, #(3 << 21)	@ rva <- cmd-write adrs (CS3) for FLASH
	write8	0x30, rva, #0		@ set page read confirm in FLASH
	bic	rva,  rva, #(3 << 21)	@ rva <- data-r/w adrs (CS3) for FLASH
	@ read data
	set	rvc, pioc_base		@ rvc <- Rdy/~Bsy status gpio
cpyfl1:	rgwfbt	rvc, #io_state, 15, 1
	read	rvb, rva, #0
	write	rvb, cnt, #0
	add	cnt, cnt, #4
	tst	cnt, #0xff
	tsteq	cnt, #0x0700
	bne	cpyfl1
	@
	@ The copying done here takes 90 seconds to get 16 MB from flash to RAM.
	@ That's a long time to wait for startup.
	@ Currently, 128 blocks x 64 pages per block (each page is 2 KB) are read.
	@ So the rate is 21 us per 32-bit word, 11 ms per page, 700 ms per block, 190 KB per second.
	@ I would have expected 100 us per page at most (factor of 100 faster).
	@ Maybe we could skip reading of pages in blocks whose 1st page starts
	@ with #xffffffff (i.e. nothing written to that block yet).
	@ Or, skip to next block on any page that starts with #xffffffff.
	@ The shadow RAM would still need to be initialized however (for the
	@ remaining pages in the skipped block).
	@ This RAM initialization might be done before cpyfl0.
	@ Also, less than 16 MB of flash could be used.
	@
	@ Maybe reading the Ready/Busy pin for each word slows things down a lot?
	@
	@ Another issue is that we don't check for bad flash blocks.
	@ Samsung documentation indicates that bad blocks are possible even
	@ in a brand new FLASH chip of K9F4... type.
	@ Possibly, for a bad block, the shadow RAM would be written with all 0's
	@ or some other indicator to avoid using those pages for new files.
	@ Also, bad blocks should probably not be erased in flash (this removes
	@ the invalid block indicators -- it's too late for my chip).
	@ It is not 100% clear how fsc/file-crunch would deal with that though
	@ (if it is ever needed with 16 MB).
	@
	@ The user may be advised to use the SD-card, rather than NAND flash to
	@ store user files.
	@ Meanwhile, Armpit Scheme's (erase) affects only the lower 16 MB of NAND
	@ flash (out of 512 MB) so the possible erasure of invalid blocks (or their
	@ info) affects only the bottom of the chip (and block 0 is guaranteed good).
	@
	@ check for end of file flash
	set	rvb, F_END_PAGE
	eq	rvb, cnt
	bne	cpyfl0
	@ de-select FLASH
	write	1<<14, pioc_base, #io_set
  .endif @   SAM9_L9261

  	/* return */
	set	pc,  lnk


wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme regs onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ store scm regs on stk
	@ copy bufr dat frm file descr (sv4) (RAM) to RAM and FLASH buffer (sv2)
	vecref	sv3, sv4, 3		@ sv3 <- buffer address
	@ select FLASH
	write	1<<14, pioc_base, #io_clear
	@ configure flash for program operation
	set	sv1,  0x40200000	@ sv1 <- command wrt adrs CS3 for FLASH
	write8	0x80, sv1, #0		@ set page program command in FLASH
	eor	sv1,  sv1, #(3 << 21)	@ sv1 <- address wrt adrs CS3 for FLASH
	write8	0x00, sv1, #0		@ set start byt-in-pg adrs lo=ColAdr1=0
	write8	rvb,  sv1, #0		@ set start byt-in-pg adrs hi=ColAdr2=0
	lsr	rvb,  sv2, #11
	and	rvb,  rvb, #0xff
	write8	rvb,  sv1, #0		@ set pg-in-blk b0-5, blk adr lo RowAdr1
	lsr	rvb,  sv2, #19
	and	rvb,  rvb, #0x1f
	write8	rvb,  sv1, #0		@ set blk adrs mid (Row Adr. 2) in FLASH
	write8	0x00, sv1, #0		@ set blk adrs hi 4bit Row Adr3 in FLASH
	eor	sv1,  sv1, #(1 << 22)	@ sv1 <- data wrt adrs (CS3) for FLASH
	@ write data
	set	rvc, 0
	set	rva, pioc_base		@ rva <- Rdy/~Bsy status gpio
wrtfl1:	rgwfbt	rva, #io_state, 15, 1
	read8	rvb, sv3, rvc
	write8	rvb, sv2, rvc
	write8	rvb, sv1, #0
	add	rvc, rvc, #1
	eq	rvc, #F_PAGE_SIZE
	bne	wrtfl1
	@ complete write, check status
	orr	sv1, sv1, #(1 << 21)	@ sv1 <- cmd wrt adrs (CS3) for FLASH
	write8	0x10, sv1, #0		@ set confirm command in gpmc
	bl	flstwt
	@ de-select FLASH
	write	1<<14, pioc_base, #io_set
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme regs
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

flstwt:	@ get flash status
	@ on entry:	sv1 <- command write address (CS3) for FLASH
	@ on entry:	sv1 <- data read/write address (CS3) for FLASH
	@ modifies:	rvb, sv1
	@ wait for flash ready
	set	rvb, pioc_base		@ rvb <- Rdy/~Bsy status gpio
	read	rvb, rvb, #io_state
	tst	rvb, #(1 << 15)
	beq	flstwt
	write8	0x70, sv1, #0		@ set get status command in FLASH
	bic	sv1, sv1, #(1 << 21)	@ sv1 <- dat rd/wrt adrs (CS3) for FLASH
	read8	rvb, sv1, #0		@ rvb <- flash status
	set	pc,  lnk

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme regs onto stack
	stmfd	sp!, {sv1}			@ store scheme regs onto stack
	@ select FLASH
	write	1<<14, pioc_base, #io_clear
	@ prepare for flash block-erase operation
	set	sv1, 0x40200000		@ sv1 <- cmd wrt adrs (CS3) for FLASH
	write8	0x60, sv1, #0		@ set page program command in FLASH
	eor	sv1, sv1, #(3 << 21)	@ sv1 <- adrs wrt adrs (CS3) for FLASH
	lsr	rvb, sv2, #11
	and	rvb, rvb, #0xff
	write8	rvb,  sv1, #0		@ set pg-in-blk b0-5, blk adr lo Ro Adr1
	lsr	rvb, sv2, #19
	and	rvb, rvb, #0x1f
	write8	rvb,  sv1, #0		@ set blk adrs mid (Row Adr 2) in FLASH
	write8	0x00, sv1, #0		@ set blk adrs hi 4bit Row Adr3 in FLASH
	@ comfirm erase
	eor	sv1, sv1, #(3 << 21)	@ sv1 <- command wrt adrs CS3 for FLASH
	write8	0xd0, sv1, #0		@ set confirm command in gpmc
	@ erase corresponding shadow RAM
	set	rvc, 0
	mvn	rvb, rvc
ersfl1:	write	rvb, sv2, rvc		@ store #xffffffff in RAM
	add	rvc, rvc, #4
	eq	rvc, #0x20000		@ 128kB / block
	bne	ersfl1
	@ check flash status/wait for flash ready
	bl	flstwt
	@ de-select FLASH
	write	1<<14, pioc_base, #io_set
	@ finish up
	ldmfd	sp!, {sv1}		@ restore scheme regs from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme regs from stack
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

.endif

.ltorg	@ dump literal constants here => up to 4K of code before and after this

	/* Flash Sector Maps */

.data	0
.balign	4

/* 8 x 64KB sectors of AT91-SAM4S16C (could also erase 8KB sectors it seems) */
.ifdef flashmap_8x64KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	 0x00000000, 8, 64, 0x0ffffffc
.endif

/* @ 2x8KB, 1x48KB, 7x64KB sectors of ATSAMG53N19 (could also erase 8KB sect) */
.ifdef flashmapg8x64KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	 0x00400000, 2, 8, 1, 48, 7, 64, 0x0ffffffc
.endif

/* 64 x 4kB sectors of AT91SAM7 and AT91SAM3S */
.ifdef flashmap_64x4KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 64, 4, 0x0FFFFFFC
.endif

/* @ 128 x 128KB RAM Blocks shadow file FLASH (top 16 MB), Samsung K9F4G08UOA */
.ifdef flashmapEx_16MB
  flashsectors:	FLASH_SECTORS	0x23000000, 128, 128
.endif

.text



