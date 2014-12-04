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

	/* Cortex-M3, Cortex-M4 */

.ifdef cortex

_func_
flscfg:	/* configure flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ in:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ in:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ in:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ in:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ in:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ in:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ in:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk
	@ nothing to do: return
	set	pc, lnk

_func_	
wrtfla:	@ write to flash, sv2 = page address, sv4 = file descriptor
_func_	
libwrt:	@ write to on-chip lib flash (lib shares on-chip file flash)
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, sv3, sv5}	@ store scheme registers onto stack
	set	rva, flashcr_base		@ rva <- flash registers base address
	vecref	sv3, sv4, 3			@ sv3 <- buffer address from file descriptor
	set	sv5, 0				@ sv5 <- 0, start offset for read/write
wrtfl0:	@ write #F_PAGE_SIZE bytes to flash
	read	rvb, sv3, sv5			@ rvb <- word to write, from buffer
	write	rvb, rva, #0x04			@ write word to flash data buffer (FMD)
	add	rvb, sv2, sv5			@ rvb <- destination address in FLASH
	write	rvb, rva, #0x00			@ write destination address to flash register (FMA)
	write	0xA4420001, rva, #0x08		@ FMC <- flash write key with write bit
	rgwfbt	rva, #0x08, 0, 0		@ wait for write bit not asserted
	add	sv5, sv5, #4			@ sv5 <- offset of next word
	eq	sv5, #F_PAGE_SIZE		@ done?
	bne	wrtfl0				@	if not, jump to keep writing
	@ exit
	ldmfd	sp!, {rva, rvb, sv3, sv5}	@ restore scheme registers from stack
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return

_func_	
ersfla:	@ erase flash sector that contains page address in sv2
_func_	
libers:	@ erase on-chip lib flash sector (lib shares on-chip file flash)
	swi	run_no_irq			@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb}			@ store scheme registers onto stack
	write	sv2, flashcr_base, #0x00	@ set page to erase in FMA register (Flash address)
	write	0xA4420002, rva, #0x08		@ start erasure via FMC (Flash Control)
	rgwfbt	rva, #0x08, 1, 0		@ wait for erase bit not asserted
	ldmfd	sp!, {rva, rvb}			@ restore scheme registers from stack
	swi	run_normal			@ enable interrupts (user mode)
  .ifdef flashmap_64x4KB			@ LM3S9B92 / TI_EvalBot (for errata)
	add	sv2, sv2, #0x0400		@ sv2 <- start of next page in 4KB block
	tst	sv2, #0x0c00			@ done erasing 4 x 1KB pages?
	bne	ersfla				@	if not, jump to erase next 1KB page
	sub	sv2, sv2, #0x1000		@ sv2 <- restore original start page address
  .endif
  .ifdef flashmap128x2KB			@ LM_4Fxxx (for errata)
	@ erase a second 1KB page (to take care of Rev. A1 vs A2 silicon errata)
	add	sv2, sv2, #0x0400		@ sv2 <- start of next page in 2KB block
	tst	sv2, #0x0400			@ done erasing 2 x 1KB pages?
	bne	ersfla				@	if not, jump to erase next 1KB page
	sub	sv2, sv2, #0x0800		@ sv2 <- restore original start page address
  .endif
	@ return
	set	pc,  lnk			@ return

.endif

	/* Cortex-A8 */

.ifdef cortex_a8

_func_
flscfg:	/* configure flash (eg. TI_Beagle POP) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ in:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ in:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ in:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ in:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ in:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ in:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ in:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk
	@ initialize POP flash
	@ unlock flash
	set	sv1, GPMC_base
	set	sv2, F_START_PAGE
	set	sv3, F_END_PAGE
	set	sv5, lnk		@ sv5 <- lnk, saved
	write	0x230023, sv1, #0x7c	@ set read command in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash start destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	write	rvb, sv1, #0x80		@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	write	rva, sv1, #0x80		@ set block/page(high) address in gpmc
	write	0x240024, sv1, #0x7c	@ set read command in gpmc
	eor	rva, sv3, #0x88000000	@ rva <- flash end destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	write	rvb, sv1, #0x80		@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	write	rva, sv1, #0x80		@ set block/page(high) address in gpmc
	bl	flstwt
	@ copy file NAND Flash into Shadow SDRAM (32MB)
	set	sv1, GPMC_base
	set	sv2, F_START_PAGE
	set	sv3, F_END_PAGE
flcp_1:	write	0x00, sv1, #0x7c	@ set read command in gpmc
	write	rvb,  sv1, #0x80	@ set byt strt offst(low) adrs=0 in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	write	rvb, sv1, #0x80		@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	write	rva, sv1, #0x80		@ set block/page(high) address in gpmc
	write	0x300030, sv1, #0x7c	@ set confirm command in gpmc
	bl	flstwt
	write	0x00, sv1, #0x7c	@ set read command in gpmc
	set	rvc, 0
flcp_2:	read	rvb, sv1, #0x84		@ rvb <- data from Flash (16-bits)
	write	rvb, sv2, rvc		@ store it in RAM
	add	rvc, rvc, #4
	eq	rvc, #F_PAGE_SIZE
	bne	flcp_2
	add	sv2, sv2, #F_PAGE_SIZE
	eq	sv2, sv3
	bne	flcp_1
	@ finish-up: restore modified default values
	set	lnk, sv5
	set	sv1, 1
	set	sv2, 2
	set	sv3, 3
	set	sv5, 5
	@ return
	set	pc,  lnk
		
flstwt:	@ wait for flash ready (subroutine)
	@ on entry:	sv1 <- GPMC_base
	@ modifies:	rvb
	write	0x700070, sv1, #0x7c	@ set get status command in gpmc
flstw0:	read	rvb, sv1, #0x84		@ rvb <- flash status
	and	rvb, rvb, #0x60
	eq	rvb, #0x60
	bne	flstw0
	set	pc,  lnk


wrtfla:	@ write to flash, sv2 is page address, sv4 is file descriptor
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme registers onto stack
	stmfd	sp!, {fre, cnt, sv1-sv5,  env, dts, glv} @ scheme regs to stack
	@ copy buffer data from file descrptr (sv4) (RAM) to RAM+FLASH bfr (sv2)
	set	sv1, GPMC_base
	vecref	sv3, sv4, 3		@ sv3 <- buffer address
	@ configure gpmc
	write	0x800080, sv1, #0x7c	@ set write command in gpmc
	write	0x00,     sv1, #0x80	@ set byt strt offst(low) adrs=0 in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	write	rvb, sv1, #0x80		@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	write	rva, sv1, #0x80		@ set block/page(high) address in gpmc
	@ write data
	set	rvc, 0
wrtfl1:	rgwfbt	sv1, #0x54, 0, 1	@ wait for write buffer status
	read	rvb, sv3, rvc
	write	rvb, sv2, rvc
	write	rvb, sv1, #0x84
	add	rvc, rvc, #4
	eq	rvc, #F_PAGE_SIZE
	bne	wrtfl1
	@ complete write, check status
	write	0x10, sv1, #0x7c	@ set confirm command in gpmc
	bl	flstwt
	@ finish up
	ldmfd	sp!, {fre, cnt, sv1-sv5, env, dts, glv}	@ get schm regs frm stck
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

ersfla:	@ erase flash sector that contains page address in sv2
	swi	run_no_irq		@ disable interrupts (user mode)
	stmfd	sp!, {rva, rvb, rvc, lnk} @ store scheme regs onto stack
	stmfd	sp!, {sv1}		@ store scheme regs onto stack
	@ perform FLash block erase
	set	sv1, GPMC_base
	write	0x600060, sv1, #0x7c	@ set block erase command in gpmc
	eor	rva, sv2, #0x88000000	@ rva <- flash destination address
	lsr	rva, rva, #11		@ rva <- destination, shifted
	bic	rvb, rva, #0xff0000	@ rva <- destination, low 16 bits
	and	rvc, rvb, #0xff00
	bic	rvb, rvb, #0xff00
	orr	rvb, rvb, rvc, lsl #8
	write	rvb, sv1, #0x80		@ set block/page(low) address in gpmc
	lsr	rva, rva, #16		@ rva <- destination, shifted
	write	rva,  sv1, #0x80	@ set block/page(high) address in gpmc
	write	0xd0, sv1, #0x7c	@ set confirm command in gpmc
	@ erase corresponding shadow RAM
	set	rvc, 0
	mvn	rvb, rvc
ersfl1:	write	rvb, sv2, rvc		@ store #xffffffff in RAM
	add	rvc, rvc, #4
	eq	rvc, #0x20000		@ 128kB / block
	bne	ersfl1
	@ check flash status/wait for flash ready
	bl	flstwt
	@ finish up
	ldmfd	sp!, {sv1}		@ restore scheme registers from stack
	ldmfd	sp!, {rva, rvb, rvc, lnk} @ restore scheme registers from stack
	orr	fre, fre, #0x02		@ fre <- fre-ptr de-reserved
	swi	run_normal		@ enable interrupts (user mode)
	set	pc,  lnk		@ return

.endif

	/* Flash Sector Maps */

.data	0
.balign	4

/* 64 x 4 KB FLASH sectors of LM3S9B92 (use 4 KB sectors for erase, p.307) */
.ifdef flashmap_64x4KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 64, 4, 0x00050000
.endif

/* 128 x 2 KB FLASH sectors of LM4F232 and LM4F120 (2KB for Rev A1-A2 errata) */
.ifdef flashmap128x2KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 128, 2, 0x00050000
.endif

/* 256 x 1KB FLASH sectors of LM3S1958, LM3S1968, LM3S6965 */
.ifdef flashmap256x1KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 256, 1, 0x00050000
.endif

/* 64 x 16 KB FLASH sectors of TM4C1294 (4x256 KB banks, interleaved) */
.ifdef flashmap64x16KB
  lib_sectors:	@ lib shares on-chip file flash
  flashsectors:	FLASH_SECTORS	0x00000000, 64, 16, 0x00200000
.endif

/* 256 x 128KB RAM Blocks shadowing file FLASH (top 32MB) Micron MT29F2G16ABC */
.ifdef TI_Beagle
  flashsectors:	FLASH_SECTORS	0x86000000, 256, 128
.endif	@ .ifdef TI_Beagle

.text



