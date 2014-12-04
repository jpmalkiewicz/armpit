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

_func_
flscfg:	/* configure flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- scgc_base  =  SIM_SCGC base adrs periph clken
	@ ret:	via lnk
	@ copy FLASH writing code to RAM
	ldr	rva, =flsRAM		@ sv1 <- start address of flashing code
	ldr	rvb, =flsRND		@ sv5 <- end address of flashing code
	set	rvc, heaptop1		@ sv3 <- RAM target address
	add	rvc, rvc, #4
hwiwt6:	read	cnt, rva, #0		@ rva <- next flashing code instruction
	write	cnt, rvc, #0		@ store it in free RAM
	cmp	rva, rvb		@ done copying the flashing code?
	itT	mi
	addmi	rva, rva, #4		@	if not, sv1 <- next source adrs
	addmi	rvc, rvc, #4		@	if not, sv1 <- next target adrs
	bmi	hwiwt6			@	if not, keep copying code to RAM
	@ return
	set	pc,  lnk


_func_	
wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
_func_	
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {sv3, lnk}		@ store scheme registers onto stack
	vecref	sv3, sv4, 3		@ sv3 <- buffer address from file descr
	set	rva, 0x14000000		@ rva <- programming accel RAM address
	set	rvc, 0
wrtfl0:	@ write #F_PAGE_SIZE bytes to prog accel RAM
	read	rvb, sv3, rvc		@ rvb <- word to write, from buffer
	write	rvb, rva, rvc		@ write word to prog accel RAM
	add	rvc, rvc, #4
	eq	rvc, #F_PAGE_SIZE	@ done?
	bne	wrtfl0			@	if not, jump to keep writing
	@ set flash CMD and parameters in FTFE_FCCOBn
	bic	rvb, sv2, #(0xff << 24)	@ rvb <- page address
	orr	rvb, rvb, #(11 << 24)	@ rvb <- pg adrs and write-8bytes cmd
	write	rvb, flashcr_base, #4	@ FTFE_FCCOB_0_3 <- cmd and adrs, p.707
	write	F_PAGE_SIZE<<12, rva,#8	@ FTFE_FCCOB_4_7 <- num 16 bytes to wrt

wrtfxx:	@ jump to RAM to execute command
	set	rvc, heaptop1+4
	adr	lnk, wrtfxt		@ lnk <- return adrs for after command
	set	pc,  rvc		@ jump to FLASH write routine in RAM
_func_
wrtfxt:	@ finish up
	@ clear error bits, if any
	bic	rvb, rvb, #0x8f
	write8	rvb, rva, #0x00
	@ exit
	ldmfd	sp!, {sv3, lnk}		@ restore scheme registers from stack
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

_func_	
wrtflr:	@ pseudo-erase a file flash page, sv2 = page address, sv4 = file desc
	@ Note:	Reference manual cautions abundantly about how writing 0 over 0
	@	over-stresses the flash so here we write mostly #xff except for
	@	file id which is pseudo-erased from #xfd to #x00 using #x02 mask
	/* Most reliable is erasing file tag from #xfd to #x0d with #xf0 mask.
	   However, if this doesn't stand the test of time then a way to
	   error-out gracefully will be needed here. */
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {sv3, lnk}		@ store scheme registers onto stack
	@ set flash CMD and parameters in FTFE_FCCOBn
	bic	rvb, sv2, #(0xff << 24)	@ rvb <- page address
	orr	rvb, rvb, #(7 << 24)	@ rvb <- pg adrs and write-8bytes cmd
	write	rvb, flashcr_base, #0x4	@ FTFE_FCCOB_0_3 <- cmd and adrs, p.701
	mvn	rvb, #0
	write	rvb, rva,          #0xc	@ FTFE_FCCOB_8_c <- data (all 1)
	bic	rvb, rvb, #0xf0		/* seems reliable  */
	write	rvb, rva, #0x08		@ FTFE_FCCOB_4_7 <- data (#xfffffff3)
	b	wrtfxx

_func_	
ersfla:	@ erase flash sector that contains page address in sv2
_func_	
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {sv3, lnk}		@ store scheme registers onto stack
	@ set flash CMD and parameters in FTFE_FCCOBn
	bic	rvb, sv2, #(0xff << 24)	@ rvb <- page address
	orr	rvb, rvb, #(9 << 24)	@ rvb <- pg adrs and erase sector cmd
	write	rvb, flashcr_base, #0x4	@ FTFE_FCCOB_0_3 <- cmd and adrs, p.704
	b	wrtfxx			@ jump to perform command


/*------------------------------------------------------------------------------
@ FLAH write/erase completion code
@-----------------------------------------------------------------------------*/

.data	0
.balign	4

flsRAM:	@ code to be copied to RAM, at fre+, such that execution is from RAM while
	@ FLASH is being written. i.e. do: (1) ldr lnk, =xyz (2) set pc, fre to initiate
	@ initiate command
	write8	0x80, rva, #0x00
	@ wait for command start
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	@ wait for completion
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	nop				@ landing pad / set cmd delay
	read8	rvb, rva, #0x00
	tst	rvb, #0x80
	itT	eq
	subeq	rvc, pc, #20		@	if not, jump back to get status
	seteq	pc,  rvc
	set	pc,  lnk		@ return
flsRND: @ end of ram code

.balign	4
.text

	/* Flash Sector Maps */

.data	0
.balign	4

/* 128 x 4 KB FLASH sectors of BANK1 in MK64FN1M0LL12, p.105
   (using BANK1 for lib and files, BANK0 is program code)     */
.ifdef flashmap128x4KB_BANK_1
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00080000, 128, 4, 0x00200000
.endif

/* 128 x 4 KB FLASH sectors of BANK0 in MK64FN1M0LL12, p.105 */
.ifdef flashmap128x4KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 128, 4, 0x00200000
.endif


.text



