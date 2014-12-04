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
@		flscfg		configure flash
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

	/* STR711 */

.ifdef STR_7xx

_func_
flscfg:	/* configure flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk
	@ copy FLASH writing code to RAM
	ldr	rva, =flsRAM		@ rva <- start address of flashing code
	ldr	rvb, =flsRND		@ rvb <- end address of flashing code
	set	rvc, heaptop1		@ rvc <- RAM target address
	add	rvc, rvc, #4
hwiwt6:	read	cnt, rva, #0		@ cnt <- next flashing code instruction
	write	cnt, rvc, #0		@ store it in free RAM
	cmp	rva, rvb		@ done copying the flashing code?
	itT	mi
	addmi	rva, rva, #4		@	if not, rva  <- next source adrs
	addmi	rvc, rvc, #4		@	if not, rvc <- next target adrs
	bne	hwiwt6			@	if not, jump to keep copying
	@ return
	set	pc,  lnk


.data	0

.balign	4


flsRAM:	@ code to be copied to RAM, at fre+, such that exec is from RAM while
	@ FLASH is written. i.e. do: (1) ldr lnk, =xyz (2) set pc, fre to init
	set	rvb, 0x100000		@ rvb <- flash_base = FLASH_CR0
	write	rva, rvb, #0		@ start write/erase operation
	set	rva, 0x100000		@ rva <- timeout to susp, approx 200 ms
	@ wait loop
	set	rvb, 0x100000		@ rvb <- flash_base = FLASH_CR0
	eq	rva, #0
	subsne	rva, rva, #1		@ rva <- timeout updated, is it zero?
	readeq	rvb, rvb, #0
	tsteq	rvb, #0x10
	orreq	rva, rvb, #0x40000000
	set	rvb, 0x100000		@ rvb <- flash_base = FLASH_CR0
	writeeq	rva, rvb, #0		@	if so,  initiate suspend of op
	read	rvb, rvb, #0		@ rvb <- stat of flsh bnks frm FLASH_CR0
	tst	rvb, #0x12		@ is bank 0 busy? (LOCK & BSYA0, chckbl)
	subne	pc,  pc, #48		@	if so,  jump to keep waiting
	@ wait a bit if WPG/DWPG/SER is stuck
	set	rva, 0x100000		@ rva <- timeout to susp, approx 100 ms
	set	rvb, 0x100000		@ rvb <- flash_base = FLASH_CR0
	read	rvb, rvb, #0
	subs	rva, rva, #1
	tstne	rvb, #0x38000000
	subne	pc,  pc, #24		@	if so,  jump to keep waiting
	@ exit
	set	pc,  lnk		@ return
flsRND: @ end of ram code
	

.text


wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	@ uses 56 bytes of user-stack space
	stmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ store scheme regs onto stack
	@ copy bfr data from file descriptor (sv4) (RAM) to STR7 FLASH bfr sv2
	vecref	sv3, sv4, 3		@ sv3 <- buffer address
	set	rvc, 0
wrtfl0:	swi	run_normal		@ enable interrupts (user mode)
	write	1,   EIC_base, #0x00
	read	rvb, 0x100000, #0x14
	tst	rvb, #0xf6
	bne	wrterr
	write	0,   rva,      #0x14
	read	rvb, rva,      #0x00	@ rva <- status of flash banks FLASH_CR0
	tst	rvb, #0x70000000	@ was resume bit set or WPG/DWPG stuck?
	@ wait if resuming/recovering
	beq	wrtfl1
	wait	0x2000000		@ rvb <- wait, approx 1 s
wrtfl1:	@ continue
	read	rvb, rva,      #0x00	@ rvb <- status of flash banks FLASH_CR0
	tst	rvb, #0x70000000	@ was resume bit set or WPG/DWPG stuck?
	readne	rvb, rva,      #0x10	@ rva <-  troublesome adrs in FLASH_AR
	bne	wrterr
	eq	rvc, #F_PAGE_SIZE	@ done writing page?
	beq	wrtfxt			@	if so,  jump to finish up
	add	rvb, sv2, rvc
	write	rvb, rva,      #0x10	@ store adrs at which to wrt in FLASH_AR
	read	rvb, sv3,      rvc	@ rvb, rvc <- next source data word
	write	rvb, rva,      #0x08	@ store data words in FLASH_DR0-1
	add	rvc, rvc, #4
	read	rvb, sv3,      rvc	@ rvb, rvc <- next two source data words
	write	rvb, rva,      #0x0C	@ rvb, rvc <- next two source data words
	add	rvc, rvc, #4
rsmwrf:	@ continue/resume double-word write
	write	0x10000000, rva, #0x00	@ CR0 <- DWPG cmnd bit (wrt double word)
	write	0,   EIC_base, #0x00
	set	rva, 0x90000000		@ rva <- DWPG & WMS cmnd, wrt dble word
	set	rvb, heaptop1+4		@ rvb <- adrs of flash write code in RAM
	swi	isr_no_irq
	adr	lnk, wrtfl0		@ lnk <- return adrs for aftr FLASH cmnd
	set	pc,  rvb		@ jump to FLASH write routine in RAM
wrtfxt:	@ finish up
	set	rva, 0			@ rva <- 0
	write	rva, rvb, #0x14		@ clr 1/0 ovrwrtng err bits in FLASH_ER
	@ wait a bit (recovery?)
	wait	0x6000			@ wait, approx 1 ms
	ldmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ restore scheme regs from stack
	set	pc,  lnk		@ return

wrterr:	@ write error other than 1/0
	raw2int	sv1, rvb
	ldmfd	sp!, {rva, rvb, sv3, rvc, lnk}	@ restore scheme regs from stack
	b	adr__err

	
ersfla:	@ erase flash sector that contains page address in sv2
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	@ prepare flash sector for erase
	write	1<27, 0x100000, #0x00	@ CR0 <- SER command bit (sector erase)
	bl	pgsctr			@ rva <- sector num (raw int),frm pg sv2
	ash	rvb, 1, rva		@ rva <- bit indic which sector to erase
	write	rvb,  0x100000, #0x04	@ set which sector to erase in FLASH_CR1
rsmerf:	@ continue/resume flash erase
	set	rva, 0x88000000		@ rva <- SER & WMS cmnd bits,start erase
	set	rvb, heaptop1+4
	swi	isr_no_irq
	adr	lnk, ersfxt		@ lnk <- return adrs for aftr FLASH cmnd
	set	pc,  rvb		@ jump to FLASH erase routine in RAM
ersfxt:	@ finish up
	swi	run_normal		@ enable interrupts (user mode)
	read	rvb, 0x100000, #0x04	@ FLASH_CR0
	eq	rvb, #0
	writeeq	rvb, rva,      #0x00
	read	rvb, rva,      #0x00
	tst	rvb, #0x48000000
	bicne	rvb, rvb, #0x40000000	@ rva <- SER command bit (sector erase)
	writene	rvb, rva,      #0x00	@ set command bit (r6) in CR0 (rvb)
	setne	rvb, 0x08000000		@ rva <- SER command bit (sector erase)
	writene	rvb, rva,      #0x00	@ set command bit (r6) in CR0 (rvb)
	bne	rsmerf
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	set	pc,  lnk		@ return

.endif

	/* STR91X_M */

.ifdef STR_9xx

_func_
flscfg:	/* configure flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk
	@ unlock the file flash sectors
	set	sv5, lnk		@ sv5 <- lnk, saved
	ldr	cnt, =flsULK		@ cnt <- start address of flashing code
	ldr	rvb, =flsULE		@ rvb <- end address of flashing code
	bl	cp2RAM			@ copy flash unlock code to RAM
	set	sv2, F_START_PAGE	@ sv2 <- start address of file flash
	bl	pgsctr			@ rva <- sect num (raw), frm pg in sv2
	set	rvc, F_END_PAGE		@ rvc <- end address of file flash
	ldr	glv, =flashsectors	@ glv <- address of flash sector table
	adr	lnk, unlkrt
	set	rvb, heaptop1+4
	set	pc,  rvb		@ jump to unlock FLASH routine in RAM
unlkrt:	@ copy FLASH writing/erasing code to RAM
	ldr	cnt, =flsRAM		@ cnt <- start address of flashing code
	ldr	rvb, =flsRND		@ rvb <- end address of flashing code
	bl	cp2RAM			@ copy flash writing code to RAM
	@ restore modified registers
	set	lnk, sv5
	set	sv1, 1
	set	sv2, 2
	set	sv5, 5
	@ return
	set	 pc,  lnk

cp2RAM:	@ copy code block delimited by cnt and rvb to RAM
	@ on entry:	cnt <- addresss of start of code block
	@ on entry:	rvb <- addresss of end   of code block
	@ modifies:	rvc, cnt, dts
	@ returns via:	lnk
	set	rvc, heaptop1+4		@ rvc <- RAM target address
hwiwt6:	read	dts, cnt, #0		@ dts <- next flashing code instruction
	write	dts, rvc, #0		@ store it in free RAM
	eq	cnt, rvb		@ done copying the flashing code?
	addne	cnt, cnt, #4		@	if not, cnt <- next source address
	addne	rvc, rvc, #4		@	if not, rvc <- next target address
	bne	hwiwt6			@	if not, jump to keep copying code to RAM
	set	pc,  lnk

.data	0

.balign 4

flsULK:	@ unlock flash code (to be executed from RAM)
	@ on entry:	glv <- address of flash sector table
	@ on entry:	rva <- sector number of 1st file flash sector
	@ on entry:	rvb <- address of this code in RAM
	@ on entry:	rvc <- end address of file flash
	@ modifies:	sv1, rva, cnt, dts
	@ returns via:	lnk
	@ loop over flash blocks to be unlocked
	read	cnt, glv, rva, LSL #2		@ cnt <- start address of flash sector
	@ unlock sector that starts at cnt
	read	dts, glv, #0			@ env <- FLASH start address
	set	sv1, 0x60			@ sv1 <- CFI unlock block command code
	write16	sv1, cnt, #0			@ initiate block unlock
	set	sv1, 0xd0			@ sv1 <- CFI confirm unlock command code
	write16	sv1, cnt, #0			@ confirm block unlock
	@ wait for FLASH device to be ready
	set	sv1, 0x90			@ sv1 <- CFI read device ID command code
	write16	sv1, dts, #0			@ initiate ID and status read
	read16	sv1, cnt, #5			@ sv1 <- block prot status, STR91xFAxx6,7
	cmp	cnt, rvc			@ done unlocking?
	addmi	rva, rva, #1			@	if not, rva <- next sector number
	setmi	pc, rvb 			@	if not, jump to unlock next sector
	@ Return to FLASH Read Array mode and exit
	set	sv1, 0xff			@ sv1 <- CFI Read Array command code
	write16	sv1, dts, #0			@ set FLASH to read array mode
	set	pc,  lnk
flsULE:	@ end of unlock code


flsRAM:	@ flash writing/erasing code to be copied to RAM
	@ on entry:	rva <- flash writing (#x40) or erasing (#x20) code
	@ on entry:	rvb <- start address of target flash sector
	@ on entry:	rvc <- address of flash start (if erase)
	@ on entry:	sv2 <- destination flash page start address (if write)
	@ on entry:	sv3 <- data buffer to be written (if write)
	@ on entry:	sv5 <- end target address (if write)
	@ on entry:	cnt <- this routine's start address in RAM
	@ modifies:	sv2, sv3, rva, rvc, cnt (if write)
	@ returns via:	lnk
	eq	rva, #0x20		@ doing erase?
	adrne	cnt, flwren		@	if not, cnt <- loop adddress for write
	setne	pc, cnt			@	if not, jump to write
	@ sector erase routine: erase block whose address starts at sv2
	write16	rva, rvb, #0		@ initiate erase block
	set	rva, 0xd0		@ rva <- CFI confirm erase command code
	write16	rva, rvb, #0		@ confirm erase block
	@ wait for FLASH device to be ready
	read16	rva, rvb, #0		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc, pc, #16		@	if not, jump back to keep waiting
	@ Return to FLASH Read Array mode and return
	set	rva, 0xff		@ rva <- CFI Read Array command code
	write16	rva, rvb, #0		@ set FLASH to read array mode
	set	pc,  lnk		@ return
flwren:	@ file writing routine
	@ write lower 2 bytes of word
	read16	rvc, sv3, #0		@ rvc <- lower half of word to write
	set	rva, 0x40		@ rva <- CFI word program command code
	write16	rva, sv2, #0		@ start half word write
	write16	rvc, sv2, #0		@ confirm half word write
	@ wait for FLASH device to be ready
	read16	rva, rvb, #0		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc, pc, #16		@	if not, jump to keep waiting
	@ write upper two bytes of word
	read16	rvc, sv3, #2		@ rvc <- upper half word to write
	set	rva, 0x40		@ rva <- CFI word program command code
	write16	rva, sv2, #0		@ start half word write
	write16	rvc, sv2, #2		@ confirm half word write
	@ wait for FLASH device to be ready
	read16	rva, rvb, #0		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc, pc, #16		@	if not, jump to keep waiting
	@ jump to keep writing or finish up
	add	sv3, sv3, #4		@ sv3 <- address of next source word
	add	sv2, sv2, #4		@ sv2 <- target address of next word
	cmp	sv2, sv5		@ done writing page?
	setmi	pc, cnt			@	if not, jump to keep writing
	@ Return to FLASH Read Array mode and return
	set	rva, 0xff		@ rva <- CFI Read Array command code
	write16	rva, rvb, #0		@ set FLASH to read array mode
	set	pc,  lnk		@ return
flsRND: @ end of ram code
	

.text


wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store scheme registers onto stack
	@ copy buffer data from file descriptor{sv4} (RAM) to FLASH buffer {sv2}
	vecref	sv3, sv4, 3			@ sv3 <- buffer address
	add	sv5, sv2, #F_PAGE_SIZE		@ sv5 <- end target address
	bl	pgsctr				@ rva <- sect num (raw) frm pg adrs in sv2
	ldr	rvb, =flashsectors		@ rvb <- address of flash address table
	read	rvb, rvb, rva, LSL #2		@ rvb <- start address of target flash sector
	adr	lnk, flwrrt
	set	rva, 0x40			@ rva <- CFI word program command code
	set	cnt, heaptop1+4
	set	pc,  cnt			@ jump to FLASH-write routine in RAM
flwrrt:	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

ersfla:	@ erase flash sector that contains page address in sv2
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	swi	run_no_irq				@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ store scheme registers onto stack
	@ prepare flash sector for write
	bl	pgsctr				@ rva <- sect num (raw) frm pg adrs in sv2
	ldr	rvb, =flashsectors		@ rvb <- address of flash sector table
	read	rvb, rvb, rva, LSL #2		@ rvb <- address of flash sector start
	adr	lnk, flerrt
	set	rva, 0x20			@ rva <- CFI erase sector command code
	set	cnt, heaptop1+4
	set	pc,  cnt			@ jump to FLASH-write routine in RAM
flerrt:	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

.endif

	/* STM32F1, STM32F4 */

.ifdef cortex

_func_
flscfg:	/* configure flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk
	@ unlock flash
	write	0x45670123, flashcr_base, #0x04	@ FLASH_KEYR <- KEY1 unlok flash
	write	0xcdef89ab,          rva, #0x04	@ FLASH_KEYR <- KEY2 unlok flash
	@ return
	set	pc,  lnk

_func_	
wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
_func_
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	set	rvc, F_PAGE_SIZE
_func_
wrtfle:	@ [internal entry] (for wrtflr = file pseudo-erase)
	stmfd	sp!, {sv3}		@ store scheme registers onto stack
  .ifdef flash_set16bprg
	write	0x101,flashcr_base,#0x10 @ FLASH_CR <- 16-bit, PSIZE & PG bits
  .else
	write	0x001,flashcr_base,#0x10 @ FLASH_CR <- PG bit
  .endif
	vecref	sv3, sv4, 3		@ sv3 <- buffer address from file desc
	@ check for file pseudo-erasure
	eq	rvc, #0
	it	eq
	write16eq rvc, sv2, #0		@ write half-word to flash
	beq	wrtfl1
wrtfl0:	@ write #F_PAGE_SIZE bytes to flash
	sub	rvc, rvc, #2
	read16	rvb, sv3, rvc		@ rvb <- half-word to write, from buffer
	write16	rvb, sv2, rvc		@ write half-word to flash
wrtfl1:	@ wait for flash ready
	rgwfbt	rva, #0x0c,flash_busy,0	@ wait for flash not busy in FLASH_SR
	eq	rvc, #0			@ done?
	bne	wrtfl0			@	if not, jump to keep writing
	@ exit
	write	0, rva, #0x10		@ FLASH_CR <- clear contents
	ldmfd	sp!, {sv3}		@ restore scheme registers from stack
	set	pc,  lnk		@ return

_func_	
wrtflr:	@ pseudo-erase a file flash page, sv2 = page address, sv4 = file desc
	@ Note:	overwriting a flash cell w/anything other than 0x00 can produce
	@	errors on this MCU. For this reason, #0 is used here (vs #i0).
	set	rvc, 0
	b	wrtfle

_func_	
ersfla:	@ erase flash sector that contains page address in sv2
_func_	
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
  .ifndef flash_ersbysnb
	write	2, flashcr_base, #0x10	@ set page erase command in FLASH_CR
	write	sv2,        rva, #0x14	@ set page to erase in FLASH_AR
	write	0x42,       rva, #0x10	@ FLASH_CR <- erase, STRT+PER,stalls cpu
  .else
	set	rvc, lnk		@ rvc <- lnk, saved against pgsctr
	bl	pgsctr			@ rva <- sector num (raw int), from page adr in r5 {sv2}
	set	lnk, rvc		@ lnk <- lnk, restored
	lsl	rvb, rva, #3		@ rvb <- bits 6:3, SNB (sector number)
	orr	rvb, rvb, #(1 << 16)	@ rvb <- bit 16, Start erase
	orr	rvb, rvb, #0x02		@ rvb <- bit 1,  PER (page erase)
	write	rvb, flashcr_base,#0x10	@ FLASH_CR <- erase, PER  (stalls cpu)
  .endif
	rgwfbt	rva, #0x0c,flash_busy,0	@ wait for flash not busy in FLASH_SR
	write	0,   rva, #0x10		@ FLASH_CR <- clear contents
	set	pc,  lnk		@ return

.endif	@ .ifdef cortex


.ltorg


/*------------------------------------------------------------------------------
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@
------------------------------------------------------------------------------*/

.data	0
.balign	4

/* 4 x 8KB, 1 x 32KB, 3 x 64KB, Bank 0 FLASH sectors of STR711 */
.ifdef flashmapA_4x8KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 4, 8, 1, 32, 3, 64, 0x0FFFFFFC
.endif

/* 8 x 64KB, Bank 0 FLASH sectors of STR911 */
.ifdef flashmap_8x64KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 8, 64, 0x0FFFFFFC
.endif

/* 4 x 16KB, 1 x 64KB, 7 x 128 KB, FLASH sectors of STM32F4 */
.ifdef flashmapA4x16KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x08000000, 4, 16, 1, 64, 7, 128, 0x08800000
.endif

/* 4 x 16KB, 1 x 64KB, 3 x 128 KB, FLASH sectors of STM32F401xE */
.ifdef flashmapB4x16KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x08000000, 4, 16, 1, 64, 3, 128, 0x08100000
.endif

/* 64 x 1KB FLASH sectors of STM32F103C8T6*/
.ifdef flashmap_64x1KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x08000000, 64, 1, 0x08800000
.endif

/* 128 x 1KB FLASH sectors of STM32F103RBT6 */
.ifdef flashmap128x1KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x08000000, 128, 1, 0x08800000
.endif

/* 128 x 2KB FLASH sectors of STM32F107VCT6 */
.ifdef flashmap128x2KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x08000000, 128, 2, 0x08800000
.endif

/* 256 x 2KB FLASH sectors of STM32F103ZET6 */
.ifdef flashmap256x2KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x08000000, 256, 2, 0x08800000
.endif

.text



