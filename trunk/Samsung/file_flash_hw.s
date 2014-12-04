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
flscfg:	/* configure flash -- nothing to do (done in startup.s) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- [not set]
	@ ret:	via lnk
	@ return
	set	pc,  lnk


wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ scheme regs to stack
	@ copy write-flash-code to boot SRAM
	ldr	sv1, =wflRAM		@ sv1 <- start address of flashing code
	ldr	sv5, =wflEND		@ sv5 <- end address of flashing code
	set	sv3, 0x40000000		@ sv3 <- target adrs for flashing code
	bl	cpflcd
	@ prepare to copy bfr dat from file desc (sv4) (RAM) to FLASH bfr (sv2)
	vcrfi	sv3, sv4, 3		@ sv3 <- buffer address	
	add	sv4, sv2, #F_PAGE_SIZE	@ sv4 <- end target address
wrtfl0:	bl	pgsctr			@ rva <- sctr num raw int, frm pag in r5
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	ldr	sv1, [rvb, rva, LSL #2]	@ sv1 <- adrs of flash page block start
	@ jump to SRAM code
	set	lnk, pc			@ lnk <- return address
	set	pc,  0x40000000		@ jump to SRAM
	@ more data to write?
	cmp	sv2, sv4		@ done writing?
	bmi	wrtfl0			@	if not, jump to wrt dat to flash
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ rstr schm regs frm stk
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme regs onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ scheme regs to stack
	@ copy erase-flash-code to boot SRAM
	ldr	sv1, =rflRAM		@ sv1 <- start address of flashing code
	ldr	sv5, =rflEND		@ sv5 <- end address of flashing code
	set	sv3, 0x40000000		@ sv3 <- boot SRAM start target address
	bl	cpflcd
	@ prepare flash sector for write
	bl	pgsctr			@ rva <- sctr num raw int, frm pag adrs
	ldr	rvb, =flashsectors	@ rvb <- address of flash sector table
	ldr	sv1, [rvb]		@ sv1 <- start address of whole flash
	ldr	sv2, [rvb, rva, LSL #2]	@ sv2 <- address of flash block start
	@ jump to SRAM code
	set	lnk, pc			@ lnk <- return address
	set	pc,  0x40000000		@ jump to SRAM
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ rstr schm regs frm stk
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return


cpflcd:	@ copy FLASH code to RAM
	ldr	rva, [sv1]		@ rva <- next flashing code instruction
	str	rva, [sv3]		@ store it in free RAM
	eq	sv1, sv5		@ done copying the flashing code?
	addne	sv1, sv1, #4		@	if not, sv1 <- next source adrs
	addne	sv3, sv3, #4		@	if not, sv1 <- next target adrs
	bne	cpflcd			@	if not, jump to keep copying
	set	pc,  lnk		@ return


/* -----------------------------------------------------------------------------
@
@ 1- Initialization from FLASH, writing to and erasing FLASH
@
----------------------------------------------------------------------------- */

.data	0


wflRAM:	@ code to be copied to S3C24xx boot SRAM to run from SRAM while
	@ writing file FLASH	
	@ initiate write-buffer to FLASH
	set	rva, 0xe8		@ rva <- CFI write-buffer command code
	strh	rva, [sv1]		@ initiate write-buffer
	ldrh	rva, [sv1]		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc,  pc, #24		@	if not, jump to keep waiting
	@ set count and transfer data to FLASH write-buffer
	set	rva, 0x1f		@ rva <- 32 bytes to write
	strh	rva, [sv1]		@ set number of bytes to write in CFI
	ldmia	sv3!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ next 8 src dat words
	stmia	sv2,  {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store words in FLASH
	stmia	sv2!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data AGAIN (cfi)
	@ commit write-buffer to FLASH
	set	rva, 0xd0		@ rva <- CFI confirm wrt-bfr cmnd code
	strh	rva, [sv1]		@ confirm write-buffer command
	ldrh	rva, [sv1]		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc,  pc,  #16		@	if not, jump to keep waiting
	set	rva, 0x50		@ rva <- CFI Clear Stat Reg command code
	strh	rva, [sv1]		@ clear the status register
	set	rva, 0xff		@ rva <- CFI Read Array command code
	strh	rva, [sv1]		@ set FLASH to read array mode
	set	pc,  lnk		@ return
wflEND:	@ end of SRAM code
		

rflRAM:	@ code to be copied to S3C24xx boot SRAM to run from SRAM while
	@ erasing file FLASH
	@ unlock block to be erased (unlocks all blocks really it seems)
	set	rva, 0x60		@ rva <- CFI unlock block command code
	strh	rva, [sv1]		@ initiate block unlock
	set	rva, 0xd0		@ rva <- CFI confirm unlock command code
	strh	rva, [sv1]		@ confirm block unlock
	ldrh	rva, [sv2]		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc,  pc,  #16		@	if not, jump to keep waiting
	set	rva, 0x50		@ rva <- CFI Clear Stat Reg command code
	strh	rva, [sv1]		@ clear the status register
	@ erase block whose address starts at sv2
	set	rva, 0x20		@ rva <- CFI erase block command code
	strh	rva, [sv2]		@ initiate erase block
	set	rva, 0xd0		@ rva <- CFI confirm erase command code
	strh	rva, [sv2]		@ confirm erase block
	ldrh	rva, [sv2]		@ rva <- FLASH device status
	tst	rva, #0x80		@ is FLASH ready?
	subeq	pc,  pc,  #16		@	if not, jump to keep waiting
	set	rva, 0x50		@ rva <- CFI Clear Stat Reg command code
	strh	rva, [sv1]		@ clear the status register
	set	rva, 0xff		@ rva <- CFI Read Array command code
	strh	rva, [sv1]		@ set FLASH to read array mode
	set	pc,  lnk		@ return
rflEND:	@ end of RAM code


/* 128 x 128KB FLASH sectors of Intel JS28F128 J3D75 on TCT Hammer board */
.ifdef TCT_Hammer
  flashsectors:	FLASH_SECTORS	0x00000000, 128, 128
.endif	@ .ifdef TCT_Hammer

.text



