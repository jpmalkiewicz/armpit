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
	@ in:	env	<- int_base = VIC1 base address
	@ in:	dts	<- VIC2 base address
	@ ret:	via lnk
	@ return
	set	pc,  lnk


wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv} @ store scheme registers onto stack
	@ copy buffer data from file descriptor (sv4) (RAM) to FLASH buffer (sv2)
	vcrfi	sv3, sv4, 3			@ sv3 <- buffer address	
	add	sv4, sv2, #F_PAGE_SIZE		@ sv4 <- end target address
wrtfl0:	bl	pgsctr				@ rva <- sect num (raw int) frm pg adrs in r5
	ldr	rvb, =flashsectors		@ rvb <- address of flash sector table
	ldr	sv1, [rvb, rva, LSL #2]		@ sv1 <- address of flash page block start
	@ initiate write-buffer to FLASH
flwrw1:	set	rva, 0xe8			@ rva <- CFI write-buffer command code
	strh	rva, [sv1]			@ initiate write-buffer
	ldrh	rva, [sv1]			@ rva <- FLASH device status
	tst	rva, #0x80			@ is FLASH ready?
	beq	flwrw1				@	if not, jump to keep waiting
	@ set count and transfer data to FLASH write-buffer
	write16	0x1f, sv1, #0			@ set number of bytes to write in CFI controller
	ldmia	sv3!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ get next eight source data words
	stmia	sv2,  {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data words in FLASH write-buffer
	stmia	sv2!, {fre,cnt,rva,rvb,sv5,env,dts,glv}	@ store data AGAIN (cfi expects 16x2 writes)
	@ commit write-buffer to FLASH
	write16	0xd0, sv1, #0			@ confirm write-buffer command
	r16wfbt	sv1, #0, 7, 1			@ wait for FLASH ready
	write16	0x50, sv1, #0			@ clear the status register
	cmp	sv2, sv4			@ done writing?
	bmi	wrtfl0				@	if not, jump to keep writing data to flash
	write16	0xff, sv1, #0			@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk}	@ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv} @ store scheme registers onto stack
	@ prepare flash sector for write
	bl	pgsctr				@ rva <- sect num (raw int) frm pg adrs in sv2
	ldr	rvb, =flashsectors		@ rvb <- address of flash sector table
	ldr	sv1, [rvb]			@ sv1 <- start address of whole FLASH (controller)
	ldr	sv2, [rvb, rva, LSL #2]		@ sv2 <- address of flash block start
	@ unlock block to be erased (unlocks all blocks really it seems)
	write16	0x60, sv1, #0			@ initiate block unlock
	write16	0xd0, sv1, #0			@ confirm block unlock
	r16wfbt	sv1, #0, 7, 1			@ wait for FLASH ready
	write16	0x50, sv1, #0			@ clear the status register
	@ erase block whose address starts at sv2
	write16	0x20, sv2, #0			@ initiate erase block
	write16	0xd0, sv2, #0			@ confirm erase block
	r16wfbt	sv1, #0, 7, 1			@ wait for FLASH ready
	write16	0x50, sv1, #0			@ clear the status register
	write16	0xff, sv1, #0			@ set FLASH to read array mode
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk}	@ restore scheme registers from stack
	orr	fre, fre, #0x02			@ fre <- fre-ptr de-reserved
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return


	/* Flash Sector Maps */

.data	0
.balign	4

/* 128 x 128KB FLASH sectors of Intel JS28F128 J3D75 on CS-EP9302 board */
.ifdef CS_E9302
  flashsectors:	FLASH_SECTORS	0x60000000, 128, 128
.endif	@ .ifdef CS_E9302
	
.text



