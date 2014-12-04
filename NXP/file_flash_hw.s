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
@		flscfg		configure flash (esp. off-chip)
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

/* --------------------------------------------------------------------- */
/* 	routines for Micron off-chip flash, for files			 */
/* --------------------------------------------------------------------- */

.ifdef ext_flash_micron	@ code to use Micron MX26LV800BTC external FLASH
			@ (eg. LPC-H2214)

_func_
flscfg:	/* configure external Micron flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk
	@ initialize external memory pins (bus) and parameters
  .ifdef LPC_H2214
  	@ Micron MX26LV800BTC
	@ Note: address and data lines configured in memcfg (sdram config)
	write	0x10003481,0xFFE00000,#0 @ BCG0 <- 
					@ offchip bank 0,FLASH,MX26LV800BTC,
					@ R55ns/W55ns,0x80000000->0x80FFFFFF
					@ 01/b29:28->MW=16bit,110/b15:11->WST2=6
					@ (write=9cycles),1/b10->RBLE=1,100/b9:5
					@ ->WST1=4 (read=7cycles), 0001/b3:0 
					@ ->IDCY=2 (R/W bus wait)
					@ note: RW55ns chip should be WST1/2=3/3
  .endif
	set	pc,  lnk		@ return, nothing to do

_func_
wrtfla:	/* write to flash, sv2 = target page address, sv4 = file descriptor */
	@ uses 56 bytes of user-stack space
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store regs onto stack
	@ copy buffer data from file descriptor {sv4=r7} (RAM) to FLASH {sv2=r5}
	vecref	sv3, sv4, 3		@ sv3 <- bfr adrs (src start)
	add	rva, sv2, #F_PAGE_SIZE	@ rva <- end target address
	@ perform write, 2 half-words at a time
	set	sv1, 0x80000aaa		@ sv1 <- flash command address 1
	set	sv5, 0x80000554		@ sv5 <- flash command address 2
wrtfl0:	read16	rvc,  sv2, #0x00	@ rvc <- FLASH cell prior dat, hlf-wrd 1
	write16	0xaa, sv1, #0x00	@ write command 1 to flash
	write16	0x55, sv5, #0x00	@ write command 2 to flash
	write16	0xa0, sv1, #0x00	@ write command 3 to flash
	read16	rvb,  sv3, #0x00	@ rvb <- first half-word from buffer
	and	rvb,  rvb, rvc		@ rvb <- new data ANDed with prior data
	write16	rvb,  sv2, #0x00	@ write ANDed data to FLASH cell
wrtfw0:	read16	rvc,  sv2, #0x00	@ rvc <- FLASH cell data, half-word 1
	eq	rvc, rvb		@ has flash content been updated?
	bne	wrtfw0			@	if not, jump to keep waiting
	read16	rvc,  sv2, #0x02	@ rvc <- FLASH cell prior dat, hlf-wrd 2
	write16	0xaa, sv1, #0x00	@ write command 1 to flash
	write16	0x55, sv5, #0x00	@ write command 2 to flash
	write16	0xa0, sv1, #0x00	@ write command 3 to flash
	read16	rvb,  sv3, #0x02	@ rvb <- second half-word from buffer
	and	rvb,  rvb, rvc		@ rvb <- new data ANDed with prior data
	write16	rvb,  sv2, #0x02	@ write ANDed data to FLASH cell
wrtfw1:	read16	rvc,  sv2, #0x02	@ rvc <- FLASH cell data, half-word 2
	eq	rvc, rvb		@ has flash content been updated?
	bne	wrtfw1			@	if not, jump to keep waiting
	add	sv3, sv3, #4		@ sv3 <- next source word address
	add	sv2, sv2, #4		@ sv2 <- next destination address
	cmp	sv2, rva		@ done writing?
	bmi	wrtfl0			@	if not, jump to keep writing
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restor regs from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

_func_
ersfla:	/* erase flash sector that contains page address in sv2 */
	@ uses 56 bytes of user-stack space
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store schm regs to stk
	@ prepare flash sector for write
	bl	pgsctr			@ rva <- sect num (raw) frm pg in sv2
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	read	rvc, rvb, rva, LSL #2	@ rvc <- start address of block to erase
	set	sv1, 0x80000aaa		@ sv1 <- flash command address 1
	set	sv5, 0x80000554		@ sv5 <- flash command address 2
	write16	0xaa, sv1, #0x00	@ write command 1 to flash
	write16	0x55, sv5, #0x00	@ write command 2 to flash
	write16	0x80, sv1, #0x00	@ write command 3 to flash
	write16	0xaa, sv1, #0x00	@ write command 1 to flash
	write16	0x55, sv5, #0x00	@ write command 2 to flash
	write16	0x30, rvc, #0x00	@ write command 4 to flash, eras blk rvc
	set	rvb,  0xffff		@ rvb <- #xffff = erased half-word mask
erswt0:	read16	rva,  rvc, #0x00	@ rva <- 1st half-wrod of erased block
	eq	rva, rvb		@ is it erased?
	bne	erswt0			@	if not, jump to keep waiting
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restor regs from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

.endif	@ ifdef ext_flash_micron

/* --------------------------------------------------------------------- */
/*	routines for INTEL off-chip flash, for files			 */
/* --------------------------------------------------------------------- */

.ifdef ext_flash_intel	@ code to use INTEL JS28F320C3-BD70 external FLASH
			@ (eg. LPC-H2294, LPC-H2888)

_func_
flscfg:	/* configure external Micron flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk
	@ initialize external memory pins (bus) and parameters
  .ifdef LPC_H2294
	@ INTEL JS28F320C3-BD70
	@ Note: address and data lines configured in memcfg (sdram config)
	write	0x10003481,0xFFE00000,#0 @ BCG0 <- bank 0,FLASH,JS28F320C3-BD70
					@ R70ns/W70ns, 0x80000000-0x80FFFFFF
					@ 01/b29:28->MW=16bit,110/b15:11->WST2=6
					@ (wrt=9cyc),1/b10->RBLE=1,
					@ 100/b9:5  ->WST1=4 (read=7cyc),
					@ 0001/b3:0 ->IDCY=2 (R/W wait)
  .endif
	@ unlock all file flash
	@ mods:	rva, rvb, rvc, glv
	set	glv, lnk		@ glv <- lnk, saved
	set	sv2, F_START_PAGE	@ sv2 <- start address of file flash
	bl	pgsctr			@ rva <- sector num, from pg adrs in sv2
unlok0:	@ loop over flash blocks to be unlocked
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	read	sv2, rvb, rva, LSL #2	@ sv2 <- start address of flash sector
	@ unlock block that starts at sv2
	read	rvc,  rvb, #0x00	@ rvc <- FLASH start address
	write16	0x60, sv2, #0x00	@ initiate block unlock
	write16	0xd0, sv2, #0x00	@ confirm block unlock
	@ wait for FLASH device to be ready
	write16	0x90, rvc, #0x00	@ initiate ID and status read
unlok1:	read16	rvb,  sv2, #0x04	@ rvb <- block status
	tst	rvb, #0x03		@ is block unlocked?
	bne	unlok1			@	if not, jump to keep waiting
	set	rvb, F_END_PAGE		@ rvb <- end address of file flash
	cmp	sv2, rvb		@ done unlocking?
	addmi	rva, rva, #1		@	if not, rva <- next sector num
	bmi	unlok0			@	if not, jump to unlok nxt sctr
	@ Return to FLASH Read Array mode and exit
	write16	0xff, rvc, #0x00	@ set FLASH to read array mode
	@ done, exit
	set	lnk, glv		@ lnk <- lnk, restored
	set	sv2, 2			@ sv2 <-   2, restored
	set	pc,  lnk		@ return

_func_
wrtfla:	/* write to flash, sv2 = page address, sv4 = file descriptor */
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store schm regs on stk
	@ copy buffer data from file descriptor{sv4} (RAM) to FLASH buffer {sv2}
	vecref	sv3, sv4, 3		@ sv3 <- buffer address	
	add	sv5, sv2, #F_PAGE_SIZE	@ sv5 <- end target address
wrtfl0:	bl	pgsctr			@ rva <- sect num (raw) frm pg in sv2
	ldr	rvb, =flashsectors	@ rvb <- address of flash address table
	read	rva, rvb, rva, LSL #2	@ rva <- start adrs of target flash blk
	@ write lower 2 bytes of word
	read16	rvc,  sv3, #0x00	@ rvc <- lower half of word to write
	write16	0x40, sv2, #0x00	@ start half word write
	write16	rvc,  sv2, #0x00	@ confirm half word write
	r16wfbt	rva, #0x00, 7, 1	@ wait for FLASH ready
	@ write upper two bytes of word
	read16	rvc,  sv3, #0x02	@ rvc <- upper half word to write
	write16	0x40, sv2, #0x02	@ start half word write
	write16	rvc,  sv2, #0x02	@ confirm half word write
	r16wfbt	rva, #0x00, 7, 1	@ wait for FLASH ready
	@ jump to keep writing or finish up
	add	sv3, sv3, #4		@ sv3 <- address of next source word
	add	sv2, sv2, #4		@ sv2 <- target address of next word
	cmp	sv2, sv5		@ done writing page?
	bmi	wrtfl0			@	if not, jump to keep writing
	@ Return to FLASH Read Array mode
	write16	0xff, rva, #0x00	@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restor regs frm stack
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

_func_
ersfla:	/* erase flash sector that contains page address in sv2 */
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store schm regs on stk
	@ prepare flash sector for write
	bl	pgsctr			@ rva <- sect num (raw) frm pg in sv2
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	read	rva, rvb, rva, LSL #2	@ rva <- address of flash block start
	@ erase block whose address starts at sv2
	write16	0x20, rva, #0x00	@ initiate erase block
	write16	0xd0, rva, #0x00	@ confirm erase block
	@ wait for FLASH device to be ready
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	read	rva, rvb, #0x00		@ rva <- FLASH start address
	rgwfbt	rva, #0x00, 7, 1	@ wait for FLASH ready
	@ Return to FLASH Read Array mode
	write16	0xff, rva, #0x00	@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restor regs from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

.endif	@ ifdef ext_flash_intel

/* --------------------------------------------------------------------- */
/*	routines for SPIFI off-chip flash, for libs and files		 */
/* --------------------------------------------------------------------- */

.ifdef ext_flash_spifi	@ code to use SPANSION S25FL032P SPIFI external FLASH
			@ (eg. LPC-4300)

	/* helper macro */
.macro	spfcmd cmd
	set	rvb, \cmd
	bl	spfcmd			@ SPFICMD <- cmd in rvb
.endm

_func_
flscfg:	/* configure flash, if needed */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl, clock sele, enabling [*not set on LPC4300*]
	@ ret:	via lnk
	@ initialize SPIFI and write code there if initial DFU upload
	write	0xe3, SCU_SFSP3_n,#0x20	@ P3_8 <- SPIFI CS,   pull-up,fst,inbuf
	write	0xf3, rva,        #0x0c	@ P3_3 <- SPIFI SCK,  no pu/d,fst,inbuf
	write	rvb,  rva,        #0x10	@ P3_4 <- SPIFI SIO3, no pu/d,fst,inbuf
	write	rvb,  rva,        #0x14	@ P3_5 <- SPIFI SIO2, no pu/d,fst,inbuf
	write	rvb,  rva,        #0x18	@ P3_6 <- SPIFI MISO, no pu/d,fst,inbuf
	write	rvb,  rva,        #0x1c	@ P3_7 <- SPIFI MOSI, no pu/d,fst,inbuf
	write	0x1fff15,spifi_base,#spifi_ctrl	@ SPIFICTRL <- FLASH size = 4MB
	set	sv5, lnk
  .ifdef upload_via_DFU
	ldr	rva, =upload_flag	@ rva <- address of code upload flag
	read	rvb, rva, #0x00		@ rvb <- flag value
	eq	rvb, #0			@ is this the initial upload
	it	eq
	bleq	cp2spf			@	if so,  jump to cpy cod to SPIFI
  .endif
	@ initiate read mode on SPIFI
	set	rva, spifi_base
	bl	spfrdm
	set	lnk, sv5		@ lnk <- restored
	set	sv5, 5			@ sv5 <- restored
	@ return
	set	pc,  lnk

_func_
wrtfla:	/* write to file flash */
.ifdef	SHARED_LIB_FILE
_func_
libwrt:	/* write to on-chip lib flash (if lib shares on-chip file flash) */
.endif
	@ on entry:	sv2 <- target flash page address
	@ on entry:	sv4 <- file descriptor with data to write
	@ preserves:	all
	stmfd	sp!, {rva, rvb, sv2, rvc, lnk} @ store scheme regs onto stack
	bl	spfcmm			@ enter SPIFI command mode	
	bl	spfwre			@ write-enable the SPIFI
	bic	sv2, sv2, #0xff
	write	sv2, rva, #spifi_addr	@ SPFIADDR <- set destination address
	write	0x02808100, rva, #spifi_cmd @ SPFICMD  <- wrt 256 bytes to dest
	vecref	sv2, sv4, 3		@ sv2 <- address of buffer
	set	rvc, 0
wrtflp:	@ loop
	read	rvb, sv2, rvc
	write	rvb, rva, #spifi_dat
	add	rvc, rvc, #4
	eq	rvc, #F_PAGE_SIZE
	bne	wrtflp
	bl	spfcwt			@ wait for completion (i.e. cmd sent)
	spfcmd	0x5204000		@ SPFICMD <- rd flsh stat (CMD=rvb=#x05)
	read8	rvb, rva, #spifi_dat
	bl	spfrdm			@ return to SPIFI read mode
	ldmfd	sp!, {rva, rvb, sv2, rvc, lnk}	@ restore scheme regs from stack
	set	pc,  lnk		@ return

_func_
ersfla:	/* erase flash sector that contains page address in sv2 */
.ifdef	SHARED_LIB_FILE
_func_
libers:	/* erase on-chip lib flash sector (if lib shares on-chip file flash) */
.endif
	@ on entry:	sv2 <- target flash page address (whole sector erased)
	@ preserves:	all
	stmfd	sp!, {rva, rvb, sv2, rvc, lnk} @ store scheme regs onto stack
	bl	spfcmm			@ enter SPIFI command mode
	bl	spfwre			@ write-enable the SPIFI
	bic	sv2, sv2, #0x00ff
	bic	sv2, sv2, #0xff00
	write	sv2, rva, #spifi_addr	@ SPFIADDR <- set dest sector address
	spfcmd	0xd8800000		@ SPFICMD <- erase full sector (SE)
	spfcmd	0x5204000		@ SPFICMD <- rd flsh stat,CMD=5,tilWIP=0
	read8	rvb, rva, #spifi_dat
	bl	spfrdm			@ return SPIFI to read mode
	ldmfd	sp!, {rva, rvb, sv2, rvc, lnk}	@ restore scheme regs from stack
	set	pc,  lnk		@ return

_func_
spfcmm:	/* enter SPIFI command mode */
	@ out:	rva <- SPIFI_base
	@ mods:	rvb, rvc
	set	rva, spifi_base
	set	rvc, lnk
	wait	0x10000			@ wait a bit (for CS)
	spfcmd	0x5204000		@ SPIFICMD <- rd stat,CMD=5,'til WIP=0
	read8	rvb, rva, #spifi_dat
	set	lnk, rvc
	set	pc,  lnk		@ return
	
_func_
spfrdm:	/* enter SPIFI read mode */
	@ on entry:	rva <- SPIFI_base
	@ modifies:	rvb, rvc
	set	rvc, lnk
	write	0,          rva, #spifi_addr	@ SPIFIADDR     <- set adrs to 0
	write	0xa5a5a5a5, rva, #spifi_idat	@ SPIFIDATINTM  <- rd mod,CS#asr
	write	0xeb133fff, rva, #spifi_mcmd	@ SPIFIDATINTM  <- set read mode
	set	lnk, rvc
	set	pc,  lnk

_func_
spfwre:	/* write-enable the SPIFI */
	@ on entry:	rva <- SPIFI_base
	@ modifies:	rvb, rvc
	set	rvc, lnk
	spfcmd	0x06200000		@ SPIFICMD <- write enable, CMD=rvb=#x06
	spfcmd	0x05204009		@ SPIFICMD <- rd stat,CMD=5,'til WREN=1
	read8	rvb, rva, #spifi_dat
	set	lnk, rvc
	set	pc,  lnk

_func_
spfcmd:	/* perform a SPIFI command, wait for completion */
	@ on entry:	rva <- SPIFI_base
	@ on entry:	rvb <- command to execute
	@ modifies:	rvb
	write	rvb, rva, #spifi_cmd	@ SPIFICMD      <- command
_func_
spfcwt:	@ wait on command complete
	@ [internal entry] (also)
	rgwfbt	rva, #spifi_stat, 1, 0
	set	pc,  lnk

	/* code to copy running program to SPIFI */
  .ifdef upload_via_DFU

.balign	4
upload_flag:
	.word	0x00000000		@ initial code upload flag

_func_	
cp2spf:	@ copy code from RAM to SPIFI (updt init-upload flag & add boot header)
	@ image header will be: #x1a, #x3f, #x7f, #x00, 8 * #x00, 4 * #xff
	@ 64 KB will be written to SPIFI (16-byte header + 64KB-16bytes code)
	@ connection to SPIFI is via SSP0 interface (regular SPI, not QUAD)
	@ FLASH chip is Spansion: S25FL032P
	@ on entry:	rva <- address of code upload flag (RAM)
	@ on entry:	rvb <- 0x00000000 = initial code upload flag
	mvn	rvb, rvb		@ rvb <- invert upload flag (futur boot)
	write	rvb, rva, #0x00		@ str invert flag in code-RAM (for copy)
	set	sv4, lnk
	@ erase 64KB sector at 0x000000
	set	sv2, 0
	bl	ersfla
	@ copy the header and code
	bl	spfcmm			@ enter SPIFI command mode	
	set	sv2, 0			@ sv2 <- destination page address
cp2spl:	@ loop over addresses
	@ issue Page Program (PP) command
	set	rva, spifi_base
	bl	spfwre			@ write-enable the SPIFI
	write	sv2, rva, #spifi_addr	@ SPFIADDR     <- set dest address
	write	0x02808100, rva, #spifi_cmd @ SPFICMD  <- wrt 256B to dest (PP)
	@ check if header needs inserting here (page 0)
	eq	sv2, #0
	bne	cp2spr
	@ copy the header
    .if boot_ram_size > 0x8000
	write	0x7f3f1a,rva,#spifi_dat	@ 64 KB minus 512 Bytes image size
    .else
	write	0x403f1a,rva,#spifi_dat	@ 32 KB image size
    .endif
	write	0,   rva, #spifi_dat
	write	rvb, rva, #spifi_dat
	mvn	rvb, rvb
	write	rvb, rva, #spifi_dat

	set	sv2, 16
cp2spr:	@ loop over bytes in page
	sub	rvc, sv2, #16
	read	rvb, rvc, #0x00
	write	rvb, rva, #spifi_dat
	add	sv2, sv2, #4
	tst	sv2, #0xff
	bne	cp2spr
	bl	spfcwt			@ wait for completion (i.e. cmd sent)
	spfcmd	0x5204000		@ SPFICMD <- flsh stat CMD=5 until WIP=0
	read8	rvb, rva, #spifi_dat
	@ page is done, check if whole code+header is copied
	eq	sv2, #boot_ram_size	@ done copying (32/64KB)?
	bne	cp2spl
	@ done -- turn green LED on, blue LED off, and return
	set	lnk, sv4
	set	sv2, 2
	set	sv3, 3
	set	sv4, 4
	set	pc,  lnk

  .endif

.endif	@ ifdef ext_flash_spifi


/* --------------------------------------------------------------------- */
/* 	routines for on-chip flash, for libs and optionally files	 */
/*	(keep this after external flash routines so that the test	 */
/*	 for flscfg does not fail)					 */
/* --------------------------------------------------------------------- */

.ifndef ext_flash_only

  .ifndef flash_bank_used
	@ set target (default/dummy) flash bank if not previously defined
	flash_bank_used = 0
  .endif

  .ifndef flscfg	@ fall-through in case no flscfg is defined

_func_
flscfg:	/* configure internal flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk
	@ return
	set	pc,  lnk		@ nothing to do here
	
  .endif

  .ifndef iap_on_lib_only		@ if IAP is used for both files and libs
_func_
wrtfla:	/* write to file flash */
  .endif
_func_
libwrt:	/* write to on-chip lib flash (lib shares on-chip file flash) */
	@ in:	sv2 <- target flash page address
	@ in:	sv4 <- file descriptor with data to write
	@ keep:	all
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	set	rvb, 20			@ rvb <- 20 = space for 5 IAP args words
	bl	adr__alo		@ rva <- adrs of free memory
	bic	fre, fre, #0x03		@ fre <- adrs of free cell for IAP args
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ str scm regs on stack
    .ifdef INIT_IAP_CMD49
	@ 0- initialize IAP (CMD 49)
      .ifdef IAP_ARGS_ADRS
	set	fre, IAP_ARGS_ADRS	@ fre <- adrs for IAP args (on-chip RAM)
      .endif
	set	cnt, fre		@ cnt <- (r1) IAP result table (id args)
	set	sv1, 49			@ sv1 <- IAP cmd 49 initialize IAP
	stmia	fre, {sv1}		@ write IAP arguments
	bl	go_iap			@ run IAP
    .endif
	@ 1- prepare flash sector for write/erase (CMD 50)
    .ifdef IAP_ARGS_ADRS
	set	fre, IAP_ARGS_ADRS	@ fre <- adrs for IAP args (on-chip RAM)
    .endif
    .ifndef iap_on_lib_only		@ if IAP is used for both files and libs
	bl	pgsctr			@ rva <- sect num (raw) frm sv2 pg adrs
    .else
	bl	lbsctr			@ rva <- sect num (raw) frm sv2 pg adrs
    .endif
	set	cnt, fre		@ cnt <- (r1) IAP result table (id args)
	set	sv1, 50			@ sv1 <- IAP cmd 50 prepare sect for wrt
	set	sv2, rva		@ sv2 <- start sector
	set	sv3, rva		@ sv3 <- end sector
	set	sv4, flash_bank_used
	stmia	fre, {sv1-sv4}		@ write IAP arguments
	bl	go_iap			@ run IAP
	@ 2- copy RAM flash to FLASH (CMD 51)
	ldmfd	sp,  {fre,cnt,sv1-sv5}	@ restore sv2=pg adrs & sv4=file descr
    .ifdef IAP_ARGS_ADRS
	set	fre, IAP_ARGS_ADRS	@ fre <- (r0) IAP args adrs, on-chip RAM
    .endif
	set	cnt, fre		@ cnt <- (r1) IAP result table (id args)
	set	sv1, 51			@ sv1 <- IAP cmd 51 copy RAM to FLASH
	vcrfi	sv3, sv4, 3		@ sv3 <- address of file data buffer
    .ifdef IAP_ARGS_ADRS
	@ copy file data buffer to on-chip RAM
	add	sv4, fre, #24
	set	sv5, F_PAGE_SIZE
wrtflp:	subs	sv5, sv5, #4
	read	rvc, sv3, sv5
	write	rvc, sv4, sv5
	bne	wrtflp
	set	sv3, sv4		@ sv3 <- adrs of copied data to write
    .endif
	set	sv4, F_PAGE_SIZE	@ sv4 <- number of bytes to write
	set	sv5, CLOCK_FREQ		@ sv5 <- clock frequency
	stmia	fre, {sv1-sv5}		@ str IAP args (cmd,pg,src,numbyts,freq)
	bl	go_iap			@ run IAP
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ rstr scm regs frm stak
	ldmfd	sp!, {rva, rvb, rvc, lnk}		@ rstr scm regs frm stak
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

  .ifndef iap_on_lib_only		@ if IAP is used for both files and libs
_func_
ersfla:	/* erase flash sector that contains page address in sv2 */
  .endif
_func_
libers:	/* erase on-chip lib flash sector (lib shares on-chip file flash) */
	@ in:	sv2 <- target flash page address (whole sector erased)
	@ keep:	all
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	set	rvb, 24			@ rvb <- 24 = space for 5 IAP args words
	bl	adr__alo		@ rva <- adrs of free memory
	bic	fre, fre, #0x03		@ fre <- adrs of free cell for IAP args
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ str scm regs on stack
    .ifdef INIT_IAP_CMD49
	@ 0- initialize IAP (CMD 49)
      .ifdef IAP_ARGS_ADRS
	set	fre, IAP_ARGS_ADRS	@ fre <- adrs for IAP args (on-chip RAM)
      .endif
	set	cnt, fre		@ cnt <- (r1) IAP result table (id args)
	set	sv1, 49			@ sv1 <- IAP cmd 49 initialize IAP
	stmia	fre, {sv1}		@ write IAP arguments
	bl	go_iap			@ run IAP
    .endif
	@ 1- prepare flash sector for write/erase (CMD 50)
    .ifdef IAP_ARGS_ADRS
	set	fre, IAP_ARGS_ADRS	@ fre <- adrs for IAP args (on-chip RAM)
    .endif
    .ifndef iap_on_lib_only		@ if IAP is used for both files and libs
	bl	pgsctr			@ rva <- sect num (raw) frm sv2 pg adrs
    .else
	bl	lbsctr			@ rva <- sect num (raw) frm sv2 pg adrs
    .endif
	set	cnt, fre		@ cnt <- (r1) IAP result table (id args)
	set	sv1, 50			@ sv1 <- IAP cmd 50 prepare sect for wrt
	set	sv2, rva		@ sv2 <- start sector
	set	sv3, rva		@ sv3 <- end sector
	set	sv4, flash_bank_used
	stmia	fre, {sv1-sv4}		@ write IAP arguments
	bl	go_iap			@ run IAP
	@ 2- erase flash sector (CMD 52)
	ldmfd	sp,  {fre, cnt, sv1-sv2} @ restore pg adrs in sv2 {r5} frm stack
    .ifdef IAP_ARGS_ADRS
	set	fre, IAP_ARGS_ADRS	@ fre <- (r0) IAP args adrs, on-chip RAM
    .endif
    .ifndef iap_on_lib_only		@ if IAP is used for both files and libs
	bl	pgsctr			@ rva <- sect num (raw) frm sv2 pg adrs
    .else
	bl	lbsctr			@ rva <- sect num (raw) frm sv2 pg adrs
    .endif
	set	cnt, fre		@ cnt  <- (r1) IAP result tbl (id args)
	set	sv1, 52			@ sv1 <- IAP cmd 52 erase FLASH sectors
	set	sv2, rva		@ sv2 <- start sector
	set	sv3, rva		@ sv3 <- end sector
	set	sv4, CLOCK_FREQ		@ sv4 <- clock frequency
	set	sv5, flash_bank_used	@ sv5 <- target flash bank
	stmia	fre, {sv1-sv5}		@ write IAP arguments
	bl	go_iap			@ run IAP
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ rstr scm regs frm stck
	ldmfd	sp!, {rva, rvb, rvc, lnk}		@ rstr scm regs frm stck
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

go_iap:	@ jump to IAP routine
  .ifndef IAP_ENTRY_PTR
	set	rvc, IAP_ENTRY		@ rvc <- address of IAP routine
  .else
	set	rvc, IAP_ENTRY_PTR
	read	rvc, rvc, #0x00		@ rvc <- address of IAP routine
  .endif
	bx	rvc			@ jump to perform IAP


.endif	@ on-chip flash routines


.ltorg	@ dump literal constants here => up to 4K of code before and after this


/*------------------------------------------------------------------------------
@
@    FLASH Sector Maps
@
@-----------------------------------------------------------------------------*/

.data	0
.balign	4

/* LPC2131, LPC2103, LPC1343, 8 x 4KB flash sectors */
.ifdef flashmap_8x4KB
  flashsectors:	FLASH_SECTORS	0x00000000, 8, 4, 0x0FFFFFFC
.endif	@ .ifdef flashmap_8x4KB

/* LPC2106, 16 x 8KB FLASH sectors */
.ifdef flashmap_16x8KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 16, 8, 0x0FFFFFFC
.endif	@ .ifdef flashmap_16x8KB
	
/* LPC2138, LPC2148, LPC2158, LPC2478, 8x4KB, 14x32KB, 5x4KB FLASH sectors */
.ifdef flashmap14x32KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 8, 4, 14, 32, 5, 4, 0x0FFFFFFC
.endif	@ .ifdef flashmap14x32KB

/* LPC1768 flash, 16 x 4KB, 14 x 32KB sectors */
.ifdef flashmap14_32KB
  @ Note: on some LPC1768 hardware (week 11 to 34, 2010) the sector starting
  @       at 0x70000 cannot be written.
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 16, 4, 14, 32, 0x0FFFFFFC
.endif	@ .ifdef flashmap14_32KB

/* LPC2214, LPC2294, 8 x 8KB + 2 x 64kB + 8 x 8 kB on-chip FLASH sectors */
.ifdef flashmap_2x64KB
  .ifndef iap_on_lib_only
    flashsectors:	@ files on on-chip flash
  .endif
  lib_sectors:	FLASH_SECTORS	0x00000000, 8, 8, 2, 64, 8, 8, 0x0FFFFFFC
.endif	@ .ifdef flashmap_2x64KB

/* LPC4357, Bank A, 8 x 8KB + 7 x 64kB on-chip FLASH sectors */
.ifdef flashmapA7x64KB
  .ifdef SHARED_LIB_FILE
    lib_sectors:	@ lib shares on-chip file flash
  .endif
  flashsectors:	FLASH_SECTORS	0x1A000000, 8, 8, 7, 64, 0x1A08ffff
.endif @ .ifdef flashmapA7x64KB

/* Micron MX26LV800BTC, 1 x 16KB, 2 x 8KB, 1 x 32KB, 15 x 64KB, LPC-H2214 */
.ifdef extflmap15x64KB
  flashsectors:	FLASH_SECTORS	0x80000000, 1, 16, 2, 8, 1, 32, 15, 64
.endif	@ .ifdef extflmap15x64KB

/* Intel JS28F160C3-BD70, 8 x 8KB + 31 x 64KB FLASH sectors, LPC-H2888 */
.ifdef extflmap31x64KB
  flashsectors:	FLASH_SECTORS	0x20000000, 8, 8, 31, 64
.endif

/* Intel JS28F320C3-BD70 flash, 8 x 8KB + 63 x 64KB sectors (sim. LPC-H2294) */
.ifdef extflmap63x64KB
  flashsectors:	FLASH_SECTORS	0x80000000, 8, 8, 63, 64
.endif	@ .ifdef extflmap63x64KB

/* SPANSION S25FL032P SPIFI FLASH, 64 x 64KB sectors */
.ifdef spifimap64x64KB
  .ifdef SHARED_LIB_FILE
    lib_sectors:	@ lib shares on-chip file flash
  .endif
  flashsectors:	FLASH_SECTORS	0x14000000, 64, 64, 0x14ffffff
.endif	@ .ifdef spifimap64x64KB

.text



