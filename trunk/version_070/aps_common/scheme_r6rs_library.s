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

/* ==========================================================================

	start of sub-environment and sub-obarray

	(don't forget ENDSUBOBAENV at end of file)

========================================================================== */

	STARTSUBOBAENV r6rs_library

/*------------------------------------------------------------------------------
@  II.I.2. bytevectors
@-----------------------------------------------------------------------------*/

	/* (bytevector? obj) */
	PRIMIT	"bytevector?", bytevector, efun, 1, otypchk, ivu8

	/* (make-bytevector k <fill>) */
	PRIMIT	"make-bytevector", make_bv, pfun, 2
	@ in:	sv1 <- k
	@ in:	sv2 <- <fill>
	@ out:	sv1 <- bytevector
	@ keep:	sv2, sv4, sv5
makvu8:	@ [internal entry]
	set	sv3, sv1		@ sv3 <- size of bytevector to allocate
	vu8aloc	sv1, sv3		@ sv1 <- allocated bytevector of size sv1
	nullp	sv2			@ was fill specified?
	itE	eq
	seteq	rvb, 0			@	if not, rvb <- 0
	lsrne	rvb, sv2, #2	
	b	fill8

	/* (bytevector-length bytevector) */
	PRIMIT	"bytevector-length", bv_len, pfun, 1
	@ in:	sv1 <- bytevector
	@ out:	sv1 <- int
	vu8len	sv1, sv1		@ sv1 <- bytevector length (scheme int)
	set	pc,  cnt

	/* (bytevector-copy! src src-start dest dest-start k) */
	PRIMIT	"bytevector-copy!", bv_copy, pfun, 4
	@ in:	sv1 <- src
	@ in:	sv2 <- src-start
	@ in:	sv3 <- dest
	@ in:	sv4 <- dest-start
	@ in:	sv5 <- (k)
	car	sv5, sv5
	add	rvc, sv2, sv5
	eor	rvc, rvc, #0x03
	cmp	sv2, sv4
	itT	mi
	addmi	sv4, sv4, sv5
	eormi	sv4, sv4, #0x03
	bmi	vu8cpd
vu8cpu:	@ copy up
	cmp	sv2, rvc
	bpl	adr_npofxt
  .ifndef cortex
	ldrb	rva, [sv1, sv2, lsr #2]
	strb	rva, [sv3, sv4, lsr #2]
  .else
	int2raw	rvb, sv2
	ldrb	rva, [sv1, rvb]
	int2raw	rvb, sv4
	strb	rva, [sv3, rvb]
  .endif
	add	sv2, sv2, #4
	add	sv4, sv4, #4
	b	vu8cpu
vu8cpd:	@ copy down
	sub	rvc, rvc, #4
	sub	sv4, sv4, #4
	cmp	rvc, sv2
	bmi	adr_npofxt
  .ifndef cortex
	ldrb	rva, [sv1, rvc, lsr #2]
	strb	rva, [sv3, sv4, lsr #2]
  .else
	int2raw	rvb, rvc
	ldrb	rva, [sv1, rvb]
	int2raw	rvb, sv4
	strb	rva, [sv3, rvb]
  .endif
	b	vu8cpd

	/* (bytevector-u8-ref bytevector k)
	   to (bytevector-s32-native-set! bytevector k s30-item) */
	PRIMIT	"bytevector-u8-ref",          bv_u8_ref,  efun, 2, ovu8ren, i0
	PRIMIT	"bytevector-u16-native-ref",  bv_u16_ref, efun, 2, ovu8ren, i1
	PRIMIT	"bytevector-s32-native-ref",  bv_s32_ref, efun, 2, ovu8ren, i2
	PRIMIT	"bytevector-u8-set!",         bv_u8_set,  efun, 2, ovu8ren, i32
	PRIMIT	"bytevector-u16-native-set!", bv_u16_set, efun, 2, ovu8ren, i33
	PRIMIT	"bytevector-s32-native-set!", bv_s32_set, efun, 2, ovu8ren, i34

	/* common entry for bytevector-xx-ref/set! (from paptbl:) */
	PRIMIT	vu8ren, ufun, 0
	@ (bytevector-u8-ref  bytevector k)			when sv4 =   i0
	@ (bytevector-u8-set! bytevector k octet)		when sv4 =  i32
	@ (bytevector-u16-native-ref  bytevector k)		when sv4 =   i1
	@ (bytevector-u16-native-set! bytevector k u16-item)	when sv4 =  i33
	@ (bytevector-s32-native-ref  bytevector k)		when sv4 =   i2
	@ (bytevector-s32-native-set! bytevector k s30-item)	when sv4 =  i34
	@ common entry for bytevector-xx-ref/set! (from paptbl:)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- 2nd input arg (eg. position, k)
	@ on entry:	sv3 <- 3rd input arg listd (because direct entry)
	@ on entry:	sv5 <- remainder of function
	@ on exit:	rvb <- position (scheme int)
	@ on exit:	rvc <- raw octet (if sv3 is octet)
	int2raw	rvb, sv2
	tst	sv4, #0x80
	bne	vu8set
	tst	sv4, #0x08
	itE	eq
	ldrheq	rvb, [sv1, rvb]		@ rvb <- octet (raw int)
	ldrne	rvb, [sv1, rvb]		@ rvb <- octet (raw int)
	tst	sv4, #0x0c
	it	eq
	andeq	rvb, rvb, #0xff
	raw2int	sv1, rvb		@ sv1 <- octet (scheme int)
	set	pc,  cnt

vu8set:	@ bytevector-xx-set!
	car	sv3, sv3		@ sv3 <- val
	int2raw	rvc, sv3
	eq	sv4, #i32
	it	eq
	strbeq	rvc, [sv1, rvb]		@ update content of bytevector
	beq	adr_npofxt		@ return with npo
	tst	sv4, #0x08
	itE	eq
	strheq	rvc, [sv1, rvb]		@ update content of bytevector
	strne	rvc, [sv1, rvb]		@ update content of bytevector
	b	adr_npofxt		@ return with npo

/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	11.4.	Exact Bitwise Operations:	bitwise-ior, bitwise-xor,
@						bitwise-and, bitwise-not,
@						bitwise-arithmetic-shift,
@						bitwise-bit-set?,
@						bitwise-copy-bit,
@						bitwise-bit-field,
@						bitwise-copy-bit-field
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		lcons, lambda_synt, sav__c, save, eval,
@					npofxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/


	/* bindings (shortcuts) for ARMSchembler */
	BNDVARi	bior, adr_bw_ior; .ascii "|"; ENDi	@ (| int1 int2 ...)
	BNDVARi	bxor, adr_bw_xor; .ascii "^"; ENDi	@ (^ int1 int2 ...)
	BNDVARi	band, adr_bw_and; .ascii "&"; ENDi	@ (& int1 int2 ...)
	BNDVARi	bnot, adr_bw_not; .ascii "~"; ENDi	@ (~ ei1)
	BNDVARi	bash, adr_bw_ash; .ascii "<<"; ENDi	@ (<< ei1 ei2)
	BNDVARi	bfld, adr_bw_bit_field; .ascii "@"; ENDi @ (@ ei1 ei2 ei3)

	/* (logior/logxor/logand i1 i2 ...) */
	BNDVAR	"logior", adr_bw_ior	@ (logior int1 int2 ...)
	BNDVAR	"logxor", adr_bw_xor	@ (logxor int1 int2 ...)
	BNDVAR	"logand", adr_bw_and	@ (logand int1 int2 ...)

	/* (lognot ei1) */
	BNDVAR	"lognot", adr_bw_not

	/* (ash ei1 ei2) */
	BNDVAR	"ash", adr_bw_ash

	/* (bitwise-ior int1 int2 ...) */
	PRIMIT	"bitwise-ior", bw_ior, pfun, 0, obwloop, i0
	@ in:	sv1 <- (int1 int2 ...)
	orr	rva, rva, rvb
	set	pc,  lnk

	/* (bitwise-xor int1 int2 ...) */
	PRIMIT	"bitwise-xor", bw_xor, pfun, 0, obwloop, i0
	@ in:	sv1 <- (int1 int2 ...)
	eor	rva, rva, rvb
	set	pc,  lnk

	/* (bitwise-and int1 int2 ...) */
	PRIMIT	"bitwise-and", bw_and, pfun, 0, obwloop, 0xfd
	@ in:	sv1 <- (int1 int2 ...)
	and	rva, rva, rvb
	set	pc,  lnk

	/* (bitwise-not ei1) */
	PRIMIT	"bitwise-not", bw_not, pfun, 1, obwloop, null
	@ in:	sv1 <- ei1	(scheme int or #vu8)
	mvn	rva, rva		@ rva <- inverted bits of rva
	b	bwexit			@ jump to common exit

	/* (bitwise-arithmetic-shift ei1 ei2) */
	PRIMIT	"bitwise-arithmetic-shift", bw_ash, pfun, 2, obwfent, null
	@ in:	sv1 <- ei1 = object to shift				(scheme int or #vu8)
	@ in:	sv2 <- ei2 = +l/-r-shift				(scheme int)
	cmp	rvb, #0			@ is shift positive?
	itEE	pl
	lslpl	rva, rva,rvb		@	if so,  rva <- raw int shifted left
	rsbmi	rvb, rvb, #0		@	if not, rvb <- minus shift
	asrmi	rva, rva, rvb		@	if not, rva <- raw int shifted right
	b	bwexit			@ jump to common exit

	/* (bitwise-if ei1 ei2 ei3) */
	PRIMIT	"bitwise-if", bw_if, pfun, 3, obwfent, null
	@ in:	sv1 <- ei1 = test-bits item			(scheme int or #vu8)
	@ in:	sv2 <- ei2 = bit-field to fill in if ei1 has 1	(scheme int or #vu8)
	@ in:	sv3 <- ei3 = bit-field to fill in if ei1 has 0	(scheme int or #vu8)
	@ out:	sv1 <- item with bits from ei2 and ei2 based on mask in ei1 (int or #vu8)
	and	rvb, rva, rvb		@ rvb <- ei2 bits masked by ei1
	mvn	rva, rva		@ rva <- inverted test-bit mask
	and	rvc, rva, rvc		@ rvc <- ei3 bits masked by not ei1
	orr	rva, rvb, rvc		@ rva <- result (raw int)
	b	bwexit			@ jump to common exit

	/* (bitwise-bit-set? ei1 ei2) */
	PRIMIT	"bitwise-bit-set?", bw_biset, pfun, 2
	@ in:	sv1 <- ei1 = item to test		(scheme int or #vu8)
	@ in:	sv2 <- ei2 = bit position to test	(scheme int)
	@ out:	sv1 <- #t/#f
	pntrp	sv1
	itE	eq
	ldreq	rva, [sv1]
	int2rawne rva, sv1
	int2raw	rvb, sv2
	asr	rva, rva, rvb
	tst	rva, #1
	b	adr_notfxt			@ jump to exit with #f/#t

	/* (bitwise-copy-bit ei1 ei2 ei3) */
	PRIMIT	"bitwise-copy-bit", bw_copy_bit, pfun, 3, obwfent, null
	@ in:	sv1 <- ei1 = item in which to set/clear bit		(scheme int or #vu8)
	@ in:	sv2 <- ei2 = bit position to set/clear			(scheme int)
	@ in:	sv3 <- ei3 = 1 to set bit, 0 to clear			(scheme int)
	@ out:	sv1 <- copy of ei1, with bit at ei2 set or cleared
	set	rvc, 1
	lsl	rvb, rvc, rvb
	eq	sv3, #i0
	itE	eq
	biceq	rva, rva, rvb
	orrne	rva, rva, rvb
	b	bwexit			@ jump to common exit

	/* (bitwise-bit-field ei1 ei2 ei3) */
	PRIMIT	"bitwise-bit-field", bw_bit_field, pfun, 3, obwfent, null
	@ in:	sv1 <- ei1, item from which to get bit-field		(scheme int or #vu8)
	@ in:	sv2 <- ei2, start position of bits to get		(scheme int)
	@ in:	sv3 <- ei3, end   position of bits to get		(scheme int)
	@ out:	sv1 <- bits from start to end (excl.) of ei1, shifted to bit 0
	set	rvb, -1
	lsl	rvb, rvb, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv2
	lsr	rva, rva, rvb
	b	bwexit			@ jump to common exit

	/* (bitwise-copy-bit-field ei1 ei2 ei3 ei4) */
	PRIMIT	"bitwise-copy-bit-field", bw_copy_bf, pfun, 4 
	@ in:	sv1 <- ei1 = item in which to insert bit-field		(scheme int or #vu8)
	@ in:	sv2 <- ei2 = start position of bits to insert		(scheme int)
	@ in:	sv3 <- ei3 = end   position of bits to insert		(scheme int)
	@ in:	sv4 <- ei4 = bit-field to insert into ei1		(scheme int or #vu8)
	@ out:	sv1 <- ei1 with bits of ei4 inserted at ei2 to ei3 (#i0 or #vu8)
	swap	sv2, sv4, sv5		@ sv2 <- ei4 = bit-field, sv4 <- ei2 = start (sv5 is temp)
	bl	bwfen			@ sv1 <- ei1 (eg. bv-cpy), rva-rvc<-ei1,ei4,ei3 conts (raw)
	set	rvb, -1			@ rvb <- #xffffffff
	lsl	rva, rvb, rvc		@ rva <- bit mask:	end-bit    -> bit-31
	int2raw	rvc, sv4		@ rvc <- start bit position
	lsl	rvb, rvb, rvc		@ rvb <- bit mask:	start-bit -> bit-31
	bic	rvb, rvb, rva		@ rvb <- bit mask:	start-bit -> end-bit (excluded)
	pntrp	sv2			@ is ei4 (bit-field) a bytevector?
	itE	eq
	ldreq	rva, [sv2]		@	if so,  rva <- ei4 bit-field data (raw int)
	asrne	rva, sv2, #2		@	if not, rva <- ei4 bit-field data (raw int)
	lsl	rva, rva, rvc		@ rva <- bit-field, shifted in place to start-bit
	and	rva, rva, rvb		@ rva <- bit-field, trimmed to start-end size
	pntrp	sv1			@ is ei1 (source item) a bytevector?
	itE	eq
	ldreq	rvc, [sv1]		@	if so,  rvc <- ei1 source item data (raw int)
	asrne	rvc, sv1, #2		@	if not, rvc <- ei1 source item data (raw int)
	bic	rvc, rvc, rvb		@ rvc <- source item data with bit-field cleared
	orr	rva, rva, rvc		@ rva <- source data with bit-field filled in
	b	bwexit			@ jump to common exit

/*------------------------------------------------------------------------
@
@	11.4 Exact Bitwise Operations:	Common entry and exit
@
@-----------------------------------------------------------------------*/
	
	/* bitwise logical operations loop */
	PRIMIT	bwloop, ufun, 0
	set	sv3, sv1
	lsl	rva, sv4, #24
	asr	sv1, rva, #24
	set	sv4, sv5
	@ on entry:	sv1 <- start value for result (scheme int)
	@ on entry:	sv3 <- list of input data
	@ on entry:	sv4 <- function to apply to data
	int2raw	rva, sv1		@ rva <- initial result (raw int)
_func_	@ loop over input data list
bwlop1:	nullp	sv3
	beq	bwexit
	snoc	sv2, sv3, sv3
	bic	rvc, sv1, sv2		@ rvc <- cross-type identifier for sv1 and sv2
	tst	rvc, #1			@ is sv1 a scheme int and sv2 a bytevector?
	itTT	ne
	raw2intne sv1, rva		@	if so,  sv1 <- current result (scheme int)
	blne	bwsv1b			@	if so,  sv1 <- bytevector allocated for result
	pntrp	sv2
	itE	eq
	ldreq	rvb, [sv2]
	asrne	rvb, sv2, #2
	adr	lnk, bwlop1
	set	pc,  sv4
	
_func_
bwexit:	@ [common exit]
	pntrp	sv1
	itTE	ne
	raw2intne sv1, rva		@ sv1 <- shifted int
	streq	rva, [sv1]
	set	pc,  cnt

	/* common function entry for paptbl */
	PRIMIT	bwfent, ufun, 0
	orr	lnk, sv5, #lnkbit0	@ lnk <- set from sv5 = func code adrs to exec
_func_
bwfen:	@ common function entry
	pntrp	sv1			@ is ei1 a bytevector?
	it	ne
	pntrpne	sv2			@	if not, is ei2 a bytevector?
	it	ne
	pntrpne	sv3			@	if not, is ei3 a bytevector?
	itTTT	ne
	asrne	rva, sv1, #2		@	if not, rva <- ei1 data (raw int)
	asrne	rvb, sv2, #2		@	if not, rvb <- ei2 data (raw int)
	asrne	rvc, sv3, #2		@	if not, rvc <- ei3 data (raw int)
	setne	pc,  lnk		@	if not, return
_func_	@ upgrade sv1 to bytevector
bwsv1b:	@ [internal entry]

.ifdef enable_MPU

	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved
	bic	fre, fre, #0x03		@ upd <- reserve memory
	set	rva, 0x0400
	orr	rva, rva, #bytevector_tag
	pntrp	sv1
	itE	eq
	ldreq	rvc, [sv1]
	asrne	rvc, sv1, #2		@ rvc <- ei1 data (raw int)
	stmia	fre!, {rva, rvc}	@ fre <- addr of next free cell
	sub	sv1, fre, #4		@ sv1 <- address of cons cell, [*commit destination*]
	orr	fre, fre, #0x02		@ de-reserve memory, [*restart critical instruction*]
	orr	lnk, sv5, #lnkbit0	@ lnk <- lnk, restored
.else
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved
	set	rvb, 4			@ rvb <- number of bytes to allocate
	bl	adr__alo		@ rva <- addr of object (symbol-tagged)
	pntrp	sv1
	itE	eq
	ldreq	rvc, [sv1]
	asrne	rvc, sv1, #2		@ rvc <- ei1 data (raw int)
	str	rvc, [rva]		@ store ei1 data in bytevector
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- address of bytevec, [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	orr	lnk, sv5, #lnkbit0	@ lnk <- lnk, restored
.endif
	ldr	rva, [sv1]		@ rva <- ei1 data (raw int)
	pntrp	sv2			@ is ei2 a bytevector?
	itE	eq
	ldreq	rvb, [sv2]		@	if so,  rvb <- ei2 data (raw int)
	asrne	rvb, sv2, #2		@	if not, rvb <- ei2 data (raw int)
	pntrp	sv3			@ is ei3 a bytevector?
	itE	eq
	ldreq	rvc, [sv3]		@	if so,  rvc <- ei3 data (raw int)
	asrne	rvc, sv3, #2		@	if not, rvc <- ei3 data (raw int)
	set	pc,  lnk		@ return


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Addendum Register Bitwise Operations:		register-copy-bit,
@							register-copy-bit-field
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/


	/* entry for register-oriented functions, for paptbl */
	PRIMIT	regent, ufun, 0
	orr	lnk, sv5, #lnkbit0	@ lnk <- from sv5 = fun cod adrs to exec
_func_
reg_en:	@ [internal entry] using bl
	int2raw	rva, sv1
	lsl	rva, rva, #4
	int2raw	rvb, sv2
	set	pc,  lnk

	/* (register-copy-bit reg ofst ei2 ei3) */
	PRIMIT	"register-copy-bit", reg_copy_bit, pfun, 3, oregent, null
	@ in:	sv1 <- reg  = register base address for set/clear bit (schint)
	@ in:	sv2 <- ofst = register offset for set/clear bit	  (scheme int)
	@ in:	sv3 <- ei2  = bit position to set/clear		  (scheme int)
	@ in:	sv4 <- (ei3) where ei3 = 1 to set bit, 0 to clear (scheme int)
	@ out:	sv1 <- npo
	@ pre:	regent sets rva to reg address (sv1) and rvb to offset (raw sv2)
	pntrp	sv4
	it	eq
	careq	sv4, sv4		@ sv4 <- ei3	
_func_
rcpbit:	@ [internal entry]
	add	rva, rva, rvb
	int2raw	rvb, sv3
	ash	rvc, 1, rvb
	ldr	rvb, [rva]
	eq	sv4, #i0
	itE	eq
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	str	rvb, [rva]
	b	adr_npofxt

	/* (register-copy-bit-field reg ofst ei2 ei3 ei4) */
	PRIMIT	"register-copy-bit-field", reg_copy_bf, pfun, 4 
	@ in:	sv1 <- reg   = register base address for set/clear bit	(schint)
	@ in:	sv2 <- ofst  = register offset for set/clear bit (scheme int)
	@ in:	sv3 <- ei2   = start position of bits to insert	 (scheme int)
	@ in:	sv4 <- ei3   = end   position of bits to insert	 (scheme int)
	@ in:	sv5 <- (ei4) = bit-field to insert into reg+ofst (schint,listed)
	@ out:	sv1 <- npo
	int2raw	rvc, sv4
	set	rvb, -1			@ rvb <- #xffffffff
	lsl	rva, rvb, rvc		@ rva <- bit mask:  end-bit   -> bit-31
	int2raw	rvc, sv3		@ rvc <- start bit position
	lsl	rvb, rvb, rvc		@ rvb <- bit mask:  start-bit -> bit-31
	bic	rvc, rvb, rva		@ rvc <- bit mask:  start-bit -> end-bit
	bl	reg_en			@ rva <- reg adrs, rvb <- raw ofst
	ldr	rvb, [rva, rvb]
	bic	rva, rvb, rvc
	car	sv5, sv5
	int2raw	rvb, sv3
	cmp	rvb, #2
	itTEE	pl
	subpl	rvb, rvb, #2
	lslpl	rvb, sv5, rvb
	rsbmi	rvb, rvb, #2
	lsrmi	rvb, sv5, rvb
	and	rvc, rvb, rvc
	orr	rvc, rva, rvc
	bl	reg_en			@ rva <- reg adrs, rvb <- raw ofst
	str	rvc, [rva, rvb]
	b	adr_npofxt

	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	11.2.	Fixnum Operations:	fx=?, fx>?, fx<?, fx>=?, fx<=?
@					fxmax, fxmin, fx+, fx*, fx-, fx/
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:		boolxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/
	
.ifdef	include_r6rs_fx

	/* (fx=?,fx>?,fx<?,fx>=?,fx<=?,fxmax,fxmin,fx+,fx-,fx*,fx/ int1 int2) */
	PRIMITi	fxeq, efun, 2, ofxchk2, i0 ; .ascii "fx=?"  ; ENDi
	PRIMITi	fxge, efun, 2, ofxchk2, i3 ; .ascii "fx>=?" ; ENDi
	PRIMITi	fxle, efun, 2, ofxchk2, i4 ; .ascii "fx<=?" ; ENDi
	PRIMIT	"fx>?",  fxgt, efun, 2, ofxchk2, i1
	PRIMIT	"fx<?",  fxlt, efun, 2, ofxchk2, i2
	PRIMIT	"fxmax", fxmx, efun, 2, ofxchk2, i5
	PRIMIT	"fxmin", fxmn, efun, 2, ofxchk2, i6
	PRIMIT	"fx+",   fxpl, efun, 2, ofxchk2, i7
	PRIMIT	"fx-",   fxmi, efun, 2, ofxchk2, i8
	PRIMIT	"fx*",   fxpr, efun, 2, ofxchk2, i9
	PRIMIT	"fx/",   fxdv, efun, 2, ofxchk2, i10

	/* common entry for binary fixnum functions */
	PRIMIT	fxchk2, ufun, 0
	@ (type-checking)
	and	rva, sv1, #0x03
	eq	rva, #i0
	itT	eq
	andeq	rvb, sv2, #0x03
	eqeq	rvb, #i0
	bne	fxerr
	bic	rva, sv4, #3
	ldr	sv5, =fxtb
	ldr	pc, [sv5, rva]
	
fxerr:	@ error out
	eq	rva, #i0
	it	eq
	seteq	sv1, sv2
	b	adr__err

	/* jump table for fixnum functions */
	startBVU8 fxtb
	.word	pfxeq
	.word	pfxgt
	.word	pfxlt
	.word	pfxge
	.word	pfxle
	.word	pfxmax
	.word	pfxmin
	.word	pfxpls
	.word	pfxmns
	.word	pfxprd
	.word	pfxdiv
	ENDsized

_func_
pfxeq:	@ (fx=? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	eq	sv1, sv2
	b	adr_boolxt

_func_
pfxgt:	@ (fx>? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	eq	sv1, sv2
	beq	adr_notfxt
	@ continue to pfxge

_func_
pfxge:	@ (fx>=? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	cmp	sv1, sv2
	itE	pl
	setpl	sv1, true
	setmi	sv1, false
	set	pc,  cnt

_func_
pfxlt:	@ (fx<? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	eq	sv1, sv2
	beq	adr_notfxt
	@ continue to pfxle

_func_
pfxle:	@ (fx<=? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	cmp	sv2, sv1
	itE	pl
	setpl	sv1, true
	setmi	sv1, false
	set	pc,  cnt

_func_
pfxmax:	@ (fxmax int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	cmp	sv1, sv2
	it	mi
	setmi	sv1, sv2
	set	pc,  cnt

_func_
pfxmin:	@ (fxmin int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	cmp	sv2, sv1
	it	mi
	setmi	sv1, sv2
	set	pc,  cnt

_func_
pfxmns:	@ (fx- int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	ngint	sv2, sv2
	@ continue to pfxpls

_func_
pfxpls:	@ (fx+ int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	int2raw	rva, sv1		@ rva <- x1 (raw int)
	int2raw	rvb, sv2		@ rva <- x2 (raw int)
	add	rvc, rva, rvb
	raw2int	sv1, rvc
	ands	rva, rvc, #0xE0000000
	it	ne
	eqne	rva, #0xE0000000
	it	eq
	seteq	pc,  cnt
	@ integer sum overflow, error out
	b	adr__err

_func_
pfxprd:	@ (fx* int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	int2raw	rvc, sv1		@ rva <- x2 (raw int)
	int2raw	rva, sv2		@ rva <- x1 (raw int)
	smull	rva, rvb, rvc, rva	@ rva <- x1 (raw int) * x2 (raw int), rvc <- pssbl ovrflw
	raw2int	sv1, rva
	lsl	rvb, rvb, #3
	orrs	rvb, rvb, rva, lsr #29
	it	ne
	mvnsne	rvc, rvb
	it	eq
	seteq	pc,  cnt
	@ integer product overflow, error out
	b	adr__err

_func_
pfxdiv:	@ (fx/ int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	set	lnk, cnt
	b	idivid

	
/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.endif	@	include_r6rs_fx


/*======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	testing.	itak:				
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=====*/

.ifndef small_memory

	/* (itak x y z) */
	PRIMIT	"itak", pfun, 3
	@ in:	sv1 <- x
	@ in:	sv2 <- y
	@ in:	sv3 <- z
	cmp	sv2, sv1		@ done?
	itT	pl
	setpl	sv1, sv3		@    if so,  sv1 <- z, result
	setpl	pc,  cnt		@    if so,  return
	sav__c				@ dts <- (cnt ...)
	save	sv1, sv2, sv3		@ dts <- (x y z cnt ...)
	sub	sv1, sv1, #4		@ sv1 <- (- x 1)
	call	adr_itak		@ sv1 <- xnew = (itak sv1 sv2 sv3)
	snoc	sv3, sv4, dts		@ sv3 <- x,    sv4 <- (y z cnt ...)
	snoc	sv4, sv5, sv4		@ sv4 <- y,    sv5 <- (z cnt ...)
	car	sv2, sv5		@ sv2 <- z
	save	sv1			@ dts <- (xnew x y z cnt ...)
	sub	sv1, sv4, #4		@ sv1 <- (- y 1)
	call	adr_itak		@ sv1 <- ynew = (itak sv1 sv2 sv3)
	cdr	sv4, dts		@ sv4 <- (x y z cnt ...)
	snoc	sv2, sv4, sv4		@ sv2 <- x,    sv4 <- (y z cnt ...)
	snoc	sv3, sv4, sv4		@ sv3 <- y,    sv4 <- (z cnt ...)
	car	sv4, sv4		@ sv4 <- z
	save	sv1			@ dts <- (ynew xnew x y z cnt ...)
	sub	sv1, sv4, #4		@ sv1 <- (- z 1)
	call	adr_itak		@ sv1 <- znew = (itak sv1 sv2 sv3)
	set	sv3, sv1		@ sv3 <- znew
	restor	sv2, sv1		@ sv1 <- xnew, sv2 <- ynew, dts <- (x y z cnt ...)
	cdddr	dts, dts		@ dts <- (cnt ...)
	restor	cnt			@ cnt <- cnt,   dts <- (...)
	b	adr_itak		@ jump to compute (itak sv1 sv2 sv3)

.endif


/* -------------------------------------------------------------------------- */

ENDSUBOBAENV

/* -------------------------------------------------------------------------- */



