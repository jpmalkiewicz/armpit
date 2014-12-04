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
@  Hardware Initialization ALL ARCHITECTURES
@
@-----------------------------------------------------------------------------*/

	.include "hw_local_init.s"	@ clocks, isrs, sdram ... init routines
	.include "gpio_hw.s"		@ gpio configuration for LED, button(s)
	.include "mcu_id_hw.s"		@ mcu ID configuration

_func_
hwinit:	/* pre-set common values */
	set	fre, 0			@ fre <- 0 (used by stackalign_4 macro)
	set	sv1, 1
	set	sv2, 2
	set	sv3, 3
	set	sv4, 4
	set	sv5, 5			@ sv5 <- 5 (used by systick_init macro)

	/* configure hardware */
	bl	hw_cfg			@ config clks, tmng... (hw_local_init.s)
	bl	memcfg			@ config SDRAM (memory_hw.s)
  .ifndef live_SD
	bl	flscfg			@ config flash (file_flash_hw.s)
  .endif
  .ifdef cortex
	systick_init			@ cortex-m3/m4 macro
  .endif
	bl	id_cfg			@ config mcu ID (mcu_id_hw.s)
	bl	piocfg			@ config gpio (gpio_hw.s)
	bl	uarcfg			@ config uart (uart_hw.s)
  .ifdef onboard_SDFT
	bl	sd_cfg			@ configure SD interface (file_sd_hw.s)
  .endif
  .ifdef native_usb
	bl	usbcfg			@ configure USB interface (usb_hw.s)
  .endif
  .ifdef enable_cpo
	bl	cpocfg			@ configure co-processor (cpo_hw.s)
  .endif
	/* ---------- configure cpu1 startup ---------- */
  .ifdef enable_a9_mpcore
	write	0x0c, 0x48281800, #0x00	@ AUX_CORE_BOOT_0 <- cpu1 ready to start
	write	0x40300004, rva,  #0x04	@ AUX_CORE_BOOT_1 <- cpu1 code start adr
	@ start cpu1 (do sev)
	dsb
	sev
  .endif

	/* continue to scheme initialization*/

/*------------------------------------------------------------------------------
@
@  Scheme Initialization ALL ARCHITECTURES
@
@-----------------------------------------------------------------------------*/

scinit:	@ initialize the scheme system
	set	sv1, null
  .ifndef enable_a9_mpcore
	set	dts, heapbottom		@ initialize free-pointer {fre}
  	set	sv5, heaptop0
  	set	rvc, heaptop1
	set	sv3, BUFFER_START
	set	sv2, READBUFFER
    .ifdef WRITEBUFFER
	set	sv4, WRITEBUFFER
    .endif
  .else
	swi	run_prvlgd
	mrc	p15, 0, rva, c0, c0, 5	@ rva <- Multiproc Affinity reg, MPIDR
	swi	run_no_irq		@ set Thread mode, unprivileged, no IRQ
	and	rva, rva, #3
	ldr	sv2, =MP_mat
	add	rva, sv2, rva, lsl 5
	ldr	dts, [rva, #0x04]	@ heapbottom_n
  	ldr	sv5, [rva, #0x08]	@ heaptop0_n
  	ldr	rvc, [rva, #0x0c]	@ heaptop1_n
	ldr	sv3, [rva, #0x10]	@ BUFFER_START_n
	ldr	sv2, [rva, #0x14]	@ READBUFFER_n
    .ifdef WRITEBUFFER
	ldr	sv4, [rva, #0x18]	@ WRITEBUFFER_n
    .endif
  .endif
	set	fre, dts
  .ifdef mark_and_sweep
	add	fre, fre, #40		@ fre <- for 8B-aligned reg sav in gc
  .endif

	/* initialize fixed (main system) buffer (sv3) */
	write	BUF_bv_tag, sv3, #-4
	vecset	sv3, FILE_LOCK, 0	@ set file lock to free state

	@ ISR vector initialization
	ldr	rva, =ISR_vector
	vecset	sv3, ISR_V_offset, rva

	/* prepare the readbuffer (sv2) */
	write	RBF_bv_tag, sv2, #-4
	vecset	sv2, 0, i0
	vecset	sv3, READ_BF_offset, sv2 @ set readbuffer address in main buffer

	/* prepare the writebuffer (sv4, if defined) */
  .ifndef WRITEBUFFER
	vecset	sv3, WRITE_BF_offset, sv1 @ set writebfr adrs to () in main bfr
  .else
	write	WBF_bv_tag, sv4, #-4
	vecset	sv4, 0, i0
	vecset	sv3, WRITE_BF_offset, sv4 @ set writebuffer address in main bfr
  .endif

	@ i2c initialization
	vecset	sv3, I2C0_BF_offset+0, false	@ set busy status to #f for I2C0
	vecset	sv3, I2C0_BF_offset+1, rvb	@ set data ready  to #f for I2C0
	vecset	sv3, I2C1_BF_offset+0, rvb	@ set busy status to #f for I2C1
	vecset	sv3, I2C1_BF_offset+1, rvb	@ set data ready  to #f for I2C1

  .ifdef cortex	@ store interrupts to enable/disable on swi modes in main buffer
	vecset	sv3, CTX_EI_offset+0, scminten__0__31	@ set int   0-31  in bfr
    .if num_interrupts > 32
	vecset	sv3, CTX_EI_offset+1, scminten_32__63	@ set int  32-63  in bfr
    .endif
    .if num_interrupts > 64
	vecset	sv3, CTX_EI_offset+2, scminten_64__95	@ set int  64-95  in bfr
    .endif
    .if num_interrupts > 96
	vecset	sv3, CTX_EI_offset+3, scminten_96_127	@ set int  96-127 in bfr
    .endif
    .if num_interrupts > 128
	vecset	sv3, CTX_EI_offset+4, scminten128_159	@ set int 128-159 in bfr
    .endif
  .endif

	@
	@ build new handler/utility global vector
	@
	add	glv, fre, #4		@ glv <- adrs of hndlr vec=1st fre-cel+4
	add	fre, fre, #80
	write	(19<<8)|vector_tag, glv, #-4
	vecset	glv, 0, sv1		@ null, callbacks
	vecset	glv, 2, sv1		@ null, i2c0 data
	vecset	glv, 3, sv1		@ null, i2c1 data

	@ default io ports initialization
  .ifndef cpo_default_io
	ldr	sv2, =vuart0
  .else
	ldr	sv2, =vcpo
  .endif
	vecset	glv, 4, sv2		@ default input/output port model
	@ store MAIN buffer in GLV
	vecset	glv, 5, sv3		@ put MAIN buffer in GLV
	@ open-file-list initialization
	vecset	glv, 6, sv1		@ null, initial open file list	
	@ initialize obarray to '()
	vecset	glv, 8, sv1		@ obarray

	/* initialize heaptop, heaptop-0/1 */
	set	rva, sv5		@ rva <- heaptop0
.ifdef	enable_MPU
  .ifdef mark_and_sweep
        @ adjust for 32-byte memory barrier in Cortex-M3/M4 (& general MPU use)
	sub	rva, rva, #32		@ rva <- heaptop dcrsd below grey set
    .ifdef MPU_is_in_MMU
        @ adjust for 1MB memory barrier section of TTB in ARM9, Cortex-A8
	lsr	rva, rva, #20
	lsl	rva, rva, #20
	sub	rva, rva, #(1 << 20)
    .endif
  .endif
.endif
	orr	rva, rva, #i0
	vecset	glv, 1, rva		@ heaptop = heaptop0
	vecset	glv, 9, rva		@ heaptop0 (stp-cpy) or maybe not used
	set	rva, rvc		@ rva <- heaptop1
.ifdef mark_and_sweep
	add	rva, rva, #EXTRA_FILE_RAM @ rva <- grey set start address
.endif
	orr	rva, rva, #i0
	vecset	glv, 10, rva		@ heaptop1 (stop-copy) or grey-set adrs
	@ re-set glv, 9 to be insertion point for above-heap objects, if needed
.ifdef	enable_MPU
  .ifdef mark_and_sweep
    .ifdef MPU_is_in_MMU
	set	rva, rvc		@ rva <- heaptop1
	sub	rva, rva, #32		@ rva <- heaptop dcrsd below grey set
	orr	rva, rva, #i0
	vecset	glv,  9, rva		@ insertion point of above-heap objects
    .endif
  .endif
.endif

	@ store built-in environment in global vector
	ldr	rvb, =env_scheme
	vecset	glv, 13, rvb		@ set built-in environment in glv
	vecset	glv, 14, sv1		@ set lib-building/parse mode1 to null
	vecset	glv, 15, sv1		@ set lib-building/parse mode2 to null
	@ initialize primitive pre-function entry table
	ldr	rva, =paptbl
	vecset	glv, 16, rva
	ldr	rva, =oba_scheme
	vecset	glv, 17, rva
	orr	rva, dts, #i0		@ rva <- cpu(n) heapbottom, pseudo-int
	vecset	glv, 18, rva
	@ initialize environment
.ifndef top_level_btree
	@ non-btree	
	@ env <- (((_winders . ()) (_prg . boot-or-not) (_catch . #<prim>)))
	set	env, fre
	add	rva, fre, #8
	stmia	fre!, {rva, sv1}
	add	rvb, fre, #8
	add	sv2, fre, #16
	ldr	sv3, =var__winders
	set	sv4, null
	add	sv5, fre, #40
	add	rvc, fre, #32
	stmia	fre!, {rvb, sv2-sv5, rvc}
	ldr	rvb, =var__catch
	ldr	sv2, =val__catch	@ dpfunc
	set	sv3, fre
	ldr	sv5, =var__prg
	set	rvc, null
	stmia	fre!, {rvb, sv2-sv5, rvc}
.else	@ btree (ordered, balanced)
	@ env <- (#((_prg.boot) #((_winders) () ()) #((_catch.#<prim>) () ())))
	set	env, fre
	add	rva, fre, #60
	stmia	fre!, {rva, sv1}
	set	rvb, (3<<8)|vector_tag
	add	sv2, fre, #16
	set	sv3, null
	set	sv4, null
	ldr	sv5, =var__catch
	ldr	rvc, =val__catch
	stmia	fre!, {rvb,sv2-sv5,rvc}
	add	sv2, fre, #16
	ldr	sv5, =var__winders
	set	rvc, null
	stmia	fre!, {rvb,sv2-sv5,rvc}
	add	sv2, fre, #16
	sub	sv3, fre, #20
	sub	sv4, fre, #44
	ldr	sv5, =var__prg
	stmia	fre!, {rvb,sv2-sv5,rvc}
.endif

	@ check on boot override
  .ifndef FlashInitCheck
    .ifndef BOOTOVERRID_PRT
	set	sv5, true		@ sv5 <- #t = do load "boot" file
    .else
	read	rva, BOOTOVERRID_PRT, #io_state
	tst	rva, #(1<<BOOTOVERRID_BUT) @ is boot override btn low?
	itE	eq
      .ifndef BOOTOVERRID_INV
	seteq	sv5, false		@	if so,  sv5 <- #f=dont load boot
	setne	sv5, true		@	if not, sv5 <- #t=do load boot
      .else
	seteq	sv5, true		@	if so,  sv5 <- #t=do   load boot
	setne	sv5, false		@	if not, sv5 <- #f=dont load boot
      .endif
    .endif
  .else
	bl	FlashInitCheck		@ rva <- status of boot override pin
	eq	rva, #0			@ is pin low?
	itE	eq
	seteq	sv5, false		@	if so,  sv5 <- #f=dont load boot
	setne	sv5, true		@	if not, sv5 <- #t=do load boot
  .endif
	str	sv5, [fre, #-4]		@ bind boot-or-not with _prg in env
	vcsti	glv, 7, env		@ set env into glv
	@ initialize flash file and library limits
	@ and store them in GLV
.ifndef	LIB_TOP_PAGE
  .ifndef live_SD
	ldr	sv3, =F_END_PAGE
	orr	sv3, sv3, #i0
  .else
	set	sv3, i0
  .endif
	vecset	glv, 11, sv3		@ set file flash end page in glv
	vecset	glv, 12, sv1		@ set lib start page to '() in glv
.else	
	@ update file flash limits for possible flash library
	ldr	sv2, =LIB_TOP_PAGE
	ldr	sv3, =F_END_PAGE
	set	sv4, sv2
flbchk:	sub	sv4, sv4, F_PAGE_SIZE
	ldr	rva, [sv4]
	mvns	rva, rva		@ is flash cell empty (#xffffffff)?
	bne	flbchk
	add	sv4, sv4, F_PAGE_SIZE
	eq	sv4, sv2
	it	eq
	seteq	sv4, null
	vecset	glv, 12, sv4		@ set lib start page in glv
  .ifdef  SHARED_LIB_FILE
	beq	flbskp
	set	sv2, sv4		@ sv2 <- lib start page adrs for pgsctr
	bl	pgsctr			@ rva <- lib start sector (raw int)
	set	rvc, rva		@ rvc <- lib start sectr (raw int, savd)
	set	sv2, sv3
	bl	pgsctr			@ rva <- fil crunch space sctr (raw int)
	cmp	rva, rvc
	itTT	pl
	subpl	rva, rvc, #1
	ldrpl	rvc, =flashsectors
	ldrpl	sv3, [rvc, rva, lsl #2]
flbskp:	
  .endif @ SHARED_LIB_FILE
	orr	sv3, sv3, #i0
	vecset	glv, 11, sv3		@ set file flash end page in glv
.endif

.ifdef	enable_MPU
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ
	bl	hptp2mpu		@ update MPU for heaptop(s)
	swi	run_no_irq		@ set Thread mode, unprivileged, no IRQ
.endif
	/* initialize scheme stack to cycle over null stack-bottom */
	ldr	dts, =stkbtm
	/* de-reserve memory */
	orr	fre, fre, #0x02
	@ enable IRQ
	enable_VIC_IRQ
	/* probably useless here: unlock file system (MP core) -- may need new sd-init too? */
	bl	funlok

.ifndef	exclude_read_write
	@ start resident program (REP)
	ldr	sv1, =prgstr
  .ifndef rw_funcs_in_scm
	call	adr_parse
  .else
	list	sv2, sv1
	ldr	sv1, =pprs1	
	save	env
	call	apply
	restor	env
  .endif
	b	eval
.endif

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.ifndef	exclude_read_write

STARTSMBL prgstr
.ascii		"(begin "
.ascii		  "(write (call/cc (lambda (_prg) (set! _catch _prg)))) "
.ascii		  "(if (eq? _prg #t) "
.ascii		      "(load ((lambda () (set! _prg #f) \"boot\")))) "
.ascii		  "(define (_prg) "
.ascii		    "(write-char #\\newline) "
.ascii		    "(prompt) "
.ascii		    "(write (eval (read) (interaction-environment))) "
.ascii		    "(_prg)) "
.ascii		  "(_prg))"
ENDsized

.endif

/*------------------------------------------------------------------------------
@  Turning LEDs on/off
@
@	There are two types of LED on/off approaches:
@	  The default is where the MCU has separate SET and CLEAR registers
@	  otherwise, the has_combined_set_clear flag needs to be set,
@         for single register MCUs
@-----------------------------------------------------------------------------*/

_func_	
yldon:	set	rvb, 1<<yled_pin
	b	ledon
_func_	
gldon:	set	rvb, 1<<gled_pin
	b	ledon
_func_	
rldon:	set	rvb, 1<<rled_pin

_func_	
ledon:
  .ifndef invert_LED
	apnset	LEDIO, rvb
  .else
	apnclr	LEDIO, rvb
  .endif
	set	pc,  lnk

_func_
yldoff:	set	rvb, 1<<yled_pin
	b	ledoff
_func_	
gldoff:	set	rvb, 1<<gled_pin
	b	ledoff
_func_	
rldoff:	set	rvb, 1<<rled_pin

_func_	
ledoff:
  .ifndef invert_LED
	apnclr	LEDIO, rvb
  .else
	apnset	LEDIO, rvb
  .endif
	set	pc,  lnk



