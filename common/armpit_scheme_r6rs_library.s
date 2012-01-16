@---------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

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
@---------------------------------------------------------------------------------------------------

.balign	4

	@-------.-------.-------.-------.-------+
r6lenv:	@	r6rs lib sub-environment	|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_r6lenv - r6lenv - 4) >> 2

	@-------.-------.-------.-------.-------+
	@	2. sub-environment		|
	@-------.-------.-------.-------.-------+
	
		.word	svu8p,	vu8p		@ bytevector?		bytevectors
		.word	smkvu8,	pmkvu8		@ make-bytevector
		.word	svu8ln,	vu8len		@ bytevector-length
		.word	svu8cp,	vu8cpy		@ bytevector-copy!
		.word	svu8rf,	vu8ref		@ bytevector-u8-ref
		.word	svu8st,	vu8set		@ bytevector-u8-set!
		.word	svu6rf,	vu6ref		@ bytevector-u16-native-ref
		.word	svu6st,	vu6set		@ bytevector-u16-native-set!
		.word	svu3rf,	vu3ref		@ bytevector-s32-native-ref
		.word	svu3st,	vu3set		@ bytevector-s32-native-set!
	
	@-------.-------.-------.-------.-------+
	@	11.4. sub-environment		|
	@-------.-------.-------.-------.-------+
	
		.word	slgior,	logior		@ bitwise-ior		bitwise operations
		.word	slgxor,	logxor		@ bitwise-xor
		.word	slgand,	logand		@ bitwise-and
		.word	slgnot,	lognot		@ bitwise-not
		.word	sbwash,	pbwash		@ bitwise-arithmetic-shift
		.word	sbwif,	pbwif		@ bitwise-if
		.word	sbwbst,	pbwbst		@ bitwise-bit-set?
		.word	sbwcpb, pbwcpb		@ bitwise-copy-bit
		.word	sbwbfl, pbwbfl		@ bitwise-bit-field
		.word	sbwbcf, pbwbcf		@ bitwise-copy-bit-field
	
	@-------.-------.-------.-------.-------+
	@	Addendum sub-environment	|
	@-------.-------.-------.-------.-------+
	
		.word	srcpbt, prcpbt		@ register-copy-bit
		.word	srcpbf, prcpbf		@ register-copy-bit-field

	@-------.-------.-------.-------.-------+
	@	11.2. sub-environment		|
	@-------.-------.-------.-------.-------+

.ifdef	include_r6rs_fx

		.word	sfxeq,	pfxeq		@ fx=?			fixnum operations
		.word	sfxgt,	pfxgt		@ fx>?
		.word	sfxlt,	pfxlt		@ fx<?
		.word	sfxge,	pfxge		@ fx>=?
		.word	sfxle,	pfxle		@ fx<=?	
		.word	sfxmax,	pfxmax		@ fxmax	
		.word	sfxmin,	pfxmin		@ fxmin	
		.word	sfxpls,	pfxpls		@ fx+	
		.word	sfxmns,	pfxmns		@ fx-	
		.word	sfxprd,	pfxprd		@ fx*	
		.word	sfxdiv,	pfxdiv		@ fx/	
.endif

	@-------.-------.-------.-------.-------+
	@	11.3. sub-environment		|
	@-------.-------.-------.-------.-------+

.ifdef	include_r6rs_fl

		.word	sfleq,	pfleq		@ fl=?			flonum operations
		.word	sflgt,	pflgt		@ fl>?
		.word	sfllt,	pfllt		@ fl<?
		.word	sflge,	pflge		@ fl>=?
		.word	sflle,	pflle		@ fl<=?
		.word	sflmax,	pflmax		@ flmax
		.word	sflmin,	pflmin		@ flmin
		.word	sflpls,	pflpls		@ fl+
		.word	sflmns,	pflmns		@ fl-
		.word	sflprd,	pflprd		@ fl*
		.word	sfldiv,	pfldiv		@ fl/

.endif

end_of_r6lenv:	@ end of r6rs library env vector
	

@---------------------------------------------------------------------------------------------------------
@  II.I.2. bytevectors
@---------------------------------------------------------------------------------------------------------

.balign	4
	
svu8p:	SYMSIZE	11
	.ascii	"bytevector?"
	.balign 4

vu8p:	@ (bytevector? obj)
	@ on entry:	sv1 <- obj
	@ pre-entry:	typchk returns via cnt with #t if obj (sv1) tag matches init-sv4, else #f
	EPFUNC	ivu8, otypchk, 1	@ primitive, init-sv4 = vu8-tag, fentry = typchk, narg = 1

.balign	4
	
smkvu8:	SYMSIZE	15
	.ascii	"make-bytevector"
	.balign 4
	
pmkvu8:	@ (make-bytevector k <fill>)
	@ on entry:	sv1 <- k
	@ on entry:	sv2 <- <fill>
	@ on exit:	sv1 <- bytevector
	@ preserves:	sv2, sv4, sv5
	PFUNC	2			@ primitive function, two input args
makvu8:	@ [internal entry]
	set	sv3, sv1		@ sv3 <- size of bytevector to allocate
	vu8aloc	sv1, sv3		@ sv1 <- allocated bytevector of size sv1
	nullp	sv2			@ was fill specified?
	itE	eq
	seteq	rvb, #0			@	if not, rvb <- 0
	lsrne	rvb, sv2, #2	
	b	fill8

.balign	4
	
svu8ln:	SYMSIZE	17
	.ascii	"bytevector-length"
	.balign 4
	
vu8len:	@ (bytevector-length bytevector)
	@ on entry:	sv1 <- bytevector
	@ on exit:	sv1 <- int
	PFUNC	1			@ primitive function, one input arg
	vu8len	sv1, sv1		@ sv1 <- bytevector length (scheme int)
	set	pc,  cnt

.balign	4

svu8cp:	SYMSIZE	16
	.ascii	"bytevector-copy!"
	.balign 4

vu8cpy:	@ (bytevector-copy! src src-start dest dest-start k)
	@ on entry:	sv1 <- src
	@ on entry:	sv2 <- src-start
	@ on entry:	sv3 <- dest
	@ on entry:	sv4 <- dest-start
	@ on entry:	sv5 <- (k)
	PFUNC	4			@ primitive function, four input args
	car	sv5, sv5
	add	sv2, sv2, #16
	add	sv4, sv4, #16
	add	rvc, sv2, sv5
	eor	rvc, rvc, #0x03
	cmp	sv2, sv4
	itT	mi
	addmi	sv4, sv4, sv5
	eormi	sv4, sv4, #0x03
	bmi	vu8cpd
vu8cpu:	@ copy up
	cmp	sv2, rvc
	bpl	npofxt
.ifndef	cortex
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
	bmi	npofxt
.ifndef	cortex
	ldrb	rva, [sv1, rvc, lsr #2]
	strb	rva, [sv3, sv4, lsr #2]
.else
	int2raw	rvb, rvc
	ldrb	rva, [sv1, rvb]
	int2raw	rvb, sv4
	strb	rva, [sv3, rvb]
.endif
	b	vu8cpd

.balign	4

vu8ren:	@ common entry for bytevector-xx-ref/set! (from paptbl:)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- 2nd input arg (eg. position, k)
	@ on entry:	sv3 <- 3rd input arg (eg. octet, scheme int, or null)
	@ on entry:	sv5 <- remainder of function
	@ on exit:	rvb <- position (scheme int)
	@ on exit:	rvc <- raw octet (if sv3 is octet)
	int2raw	rvb, sv2
	add	rvb, rvb, #4
	int2raw	rvc, sv3
	set	pc,  sv5

.balign	4
	
svu8rf:	SYMSIZE	17
	.ascii	"bytevector-u8-ref"
	.balign 4
	
vu8ref:	@ (bytevector-u8-ref bytevector k)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- octet
	@ pre-entry:	vu8ren sets rvb to raw offset (raw sv2 + 4) and rvc to raw sv3
	EPFUNC	0, ovu8ren, 2		@  primitive, init-sv4 = none, fentry = vu8ren, narg = 2
	ldrb	rvb, [sv1, rvb]		@ rvb <- octet (raw int)
vu8rxt:	@ [internal entry] common exit for bytevector-xx-ref
	raw2int	sv1, rvb		@ sv1 <- octet (scheme int)
	set	pc,  cnt

.balign	4
	
svu8st:	SYMSIZE	18
	.ascii	"bytevector-u8-set!"
	.balign 4
	
vu8set:	@ (bytevector-u8-set! bytevector k octet)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- k
	@ on entry:	sv3 <- octet (scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	vu8ren sets rvb to raw offset (raw sv2 + 4) and rvc to raw sv3
	EPFUNC	0, ovu8ren, 3		@  primitive, init-sv4 = none, fentry = vu8ren, narg = 3
	strb	rvc, [sv1, rvb]		@ update content of bytevector
	b	npofxt			@ return with npo

.balign	4

svu6rf:	SYMSIZE	25
	.ascii	"bytevector-u16-native-ref"
	.balign 4

vu6ref:	@ (bytevector-u16-native-ref bytevector k)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- u16 item (scheme int)
	@ pre-entry:	vu8ren sets rvb to raw offset (raw sv2 + 4) and rvc to raw sv3
	EPFUNC	0, ovu8ren, 2		@  primitive, init-sv4 = none, fentry = vu8ren, narg = 2
	ldrh	rvb, [sv1, rvb]		@ rvb <- u16 item (raw int)
	b	vu8rxt

.balign	4
	
svu6st:	SYMSIZE	26
	.ascii	"bytevector-u16-native-set!"
	.balign 4
	
vu6set:	@ (bytevector-u16-native-set! bytevector k u16-item)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- k
	@ on entry:	sv3 <- u16 item (scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	vu8ren sets rvb to raw offset (raw sv2 + 4) and rvc to raw sv3
	EPFUNC	0, ovu8ren, 3		@  primitive, init-sv4 = none, fentry = vu8ren, narg = 3
	strh	rvc, [sv1, rvb]		@ update content of bytevector
	b	npofxt			@ return with npo

.balign	4

svu3rf:	SYMSIZE	25
	.ascii	"bytevector-s32-native-ref"
	.balign 4

vu3ref:	@ (bytevector-s32-native-ref bytevector k)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- k
	@ on exit:	sv1 <- s30-item (scheme int)
	@ pre-entry:	vu8ren sets rvb to raw offset (raw sv2 + 4) and rvc to raw sv3
	EPFUNC	0, ovu8ren, 2		@  primitive, init-sv4 = none, fentry = vu8ren, narg = 2
	ldr	rvb, [sv1, rvb]		@ rvb <- s30-item (raw int)
	b	vu8rxt

.balign	4
	
svu3st:	SYMSIZE	26
	.ascii	"bytevector-s32-native-set!"
	.balign 4

vu3set:	@ (bytevector-s32-native-set! bytevector k s30-item)
	@ on entry:	sv1 <- bytevector
	@ on entry:	sv2 <- k
	@ on entry:	sv3 <- s30-item (scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	vu8ren sets rvb to raw offset (raw sv2 + 4) and rvc to raw sv3
	EPFUNC	0, ovu8ren, 3		@  primitive, init-sv4 = none, fentry = vu8ren, narg = 3
	str	rvc, [sv1, rvb]		@ update content of bytevector
	b	npofxt			@ return with npo


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg


@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	11.4.	Exact Bitwise Operations:			bitwise-ior, bitwise-xor,
@								bitwise-and, bitwise-not,
@								bitwise-arithmetic-shift,
@								bitwise-bit-set?,
@								bitwise-copy-bit,
@								bitwise-bit-field,
@								bitwise-copy-bit-field
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			lcons, lambda_synt, sav__c, save, eval, npofxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.balign	4
	
slgior:	SYMSIZE	11
	.ascii	"bitwise-ior"
	.balign 4

logior:	@ (logior int1 int2 ...)
	@ on entry:	sv1 <- (int1 int2 ...)
	EPFUNC	i0, obwloop, 0		@ primitive, init-sv4 = 0, fentry = bwloop, narg = listed
	orr	rva, rva, rvb
	set	pc,  lnk

.balign	4
	
slgxor:	SYMSIZE	11
	.ascii	"bitwise-xor"
	.balign 4
	
logxor:	@ (logxor int1 int2 ...)
	@ on entry:	sv1 <- (int1 int2 ...)
	EPFUNC	i0, obwloop, 0		@ primitive, init-sv4 = 0, fentry = bwloop, narg = listed
	eor	rva, rva, rvb
	set	pc,  lnk
	
.balign	4
	
slgand:	SYMSIZE	11
	.ascii	"bitwise-and"
	.balign 4

logand:	@ (logand int1 int2 ...)
	@ on entry:	sv1 <- (int1 int2 ...)
	EPFUNC	0xfd, obwloop, 0	@ primitive, init-sv4 = -1, fentry = bwloop, narg = listed
	and	rva, rva, rvb
	set	pc,  lnk
	
.balign	4

slgnot:	SYMSIZE	11
	.ascii	"bitwise-not"
	.balign 4

lognot:	@ (lognot ei1)
	@ on entry:	sv1 <- ei1	(scheme int or #vu8)
	EPFUNC	0, obwloop, 1		@ primitive, init-sv4 = none, fentry = bwloop, narg = 1
	mvn	rva, rva		@ rva <- inverted bits of rva
	b	bwexit			@ jump to common exit
	
.balign	4
	
sbwash:	SYMSIZE	24
	.ascii	"bitwise-arithmetic-shift"
	.balign 4
	
pbwash:	@ (bitwise-arithmetic-shift ei1 ei2)
	@ on entry:	sv1 <- ei1 = object to shift				(scheme int or #vu8)
	@ on entry:	sv2 <- ei2 = +l/-r-shift				(scheme int)
	EPFUNC	0, obwfent, 2		@ primitive, init-sv4 = none, fentry = bwfent, narg = 2
	cmp	rvb, #0			@ is shift positive?
	itEE	pl
	lslpl	rva, rva,rvb		@	if so,  rva <- raw int shifted left
	rsbmi	rvb, rvb, #0		@	if not, rvb <- minus shift
	asrmi	rva, rva, rvb		@	if not, rva <- raw int shifted right
	b	bwexit			@ jump to common exit

.balign	4

sbwif:	SYMSIZE	10
	.ascii	"bitwise-if"
	.balign 4

pbwif:	@ (bitwise-if ei1 ei2 ei2)
	@ on entry:	sv1 <- ei1 = test-bits item				(scheme int or #vu8)
	@ on entry:	sv2 <- ei2 = bit-field to fill in where ei1 has 1	(scheme int or #vu8)
	@ on entry:	sv3 <- ei3 = bit-field to fill in where ei1 has 0	(scheme int or #vu8)
	@ on exit:	sv1 <- item with bits from ei2 and ei2 based on mask in ei1 (int or #vu8)
	EPFUNC	0, obwfent, 3		@ primitive, init-sv4 = none, fentry = bwfent, narg = 3
	and	rvb, rva, rvb		@ rvb <- ei2 bits masked by ei1
	mvn	rva, rva		@ rva <- inverted test-bit mask
	and	rvc, rva, rvc		@ rvc <- ei3 bits masked by not ei1
	orr	rva, rvb, rvc		@ rva <- result (raw int)
	b	bwexit			@ jump to common exit
	
.balign	4

sbwbst:	SYMSIZE	16
	.ascii	"bitwise-bit-set?"
	.balign 4
	
pbwbst:	@ (bitwise-bit-set? ei1 ei2)
	@ on entry:	sv1 <- ei1 = item to test				(scheme int or #vu8)
	@ on entry:	sv2 <- ei2 = bit position to test			(scheme int)
	@ on exit:	sv1 <- #t/#f
	PFUNC	2			@ primitive function, two input args
	pntrp	sv1
	itE	eq
	ldreq	rva, [sv1, #4]
	int2rawne rva, sv1
	int2raw	rvb, sv2
	asr	rva, rva, rvb
	tst	rva, #1
	b	notfxt			@ jump to exit with #f/#t
	
.balign	4

sbwcpb:	SYMSIZE	16
	.ascii	"bitwise-copy-bit"
	.balign 4

pbwcpb:	@ (bitwise-copy-bit ei1 ei2 ei3)
	@ on entry:	sv1 <- ei1 = item in which to set/clear bit		(scheme int or #vu8)
	@ on entry:	sv2 <- ei2 = bit position to set/clear			(scheme int)
	@ on entry:	sv3 <- ei3 = 1 to set bit, 0 to clear			(scheme int)
	@ on exit:	sv1 <- copy of ei1, with bit at ei2 set or cleared
	EPFUNC	0, obwfent, 3		@ primitive, init-sv4 = none, fentry = bwfent, narg = 3
	set	rvc, #1
	lsl	rvb, rvc, rvb
	eq	sv3, #i0
	itE	eq
	biceq	rva, rva, rvb
	orrne	rva, rva, rvb
	b	bwexit			@ jump to common exit
		
.balign	4

sbwbfl:	SYMSIZE	17
	.ascii	"bitwise-bit-field"
	.balign 4

pbwbfl:	@ (bitwise-bit-field ei1 ei2 ei3)
	@ on entry:	sv1 <- ei1, item from which to get bit-field		(scheme int or #vu8)
	@ on entry:	sv2 <- ei2, start position of bits to get		(scheme int)
	@ on entry:	sv3 <- ei3, end   position of bits to get		(scheme int)
	@ on exit:	sv1 <- bits from start to end (excl.) of ei1, shifted to bit 0
	EPFUNC	0, obwfent, 3		@ primitive, init-sv4 = none, fentry = bwfent, narg = 3
	set	rvb, #-1
	lsl	rvb, rvb, rvc
	bic	rva, rva, rvb
	int2raw	rvb, sv2
	lsr	rva, rva, rvb
	b	bwexit			@ jump to common exit
		
.balign	4

sbwbcf:	SYMSIZE	22
	.ascii	"bitwise-copy-bit-field"
	.balign 4

pbwbcf:	@ (bitwise-copy-bit-field ei1 ei2 ei3 ei4)
	@ on entry:	sv1 <- ei1 = item in which to insert bit-field		(scheme int or #vu8)
	@ on entry:	sv2 <- ei2 = start position of bits to insert		(scheme int)
	@ on entry:	sv3 <- ei3 = end   position of bits to insert		(scheme int)
	@ on entry:	sv4 <- ei4 = bit-field to insert into ei1		(scheme int or #vu8)
	@ on exit:	sv1 <- ei1 with bits of ei4 inserted at ei2 to ei3 (#i0 or #vu8)
	PFUNC	4			@ primitive function, four input args
	swap	sv2, sv4, sv5		@ sv2 <- ei4 = bit-field, sv4 <- ei2 = start (sv5 used as temp)
	bl	bwfen			@ sv1 <- ei1 (eg. bv-copy), rva-rvc <- ei1,ei4,ei3 contents (raw)
	set	rvb, #-1		@ rvb <- #xffffffff
	lsl	rva, rvb, rvc		@ rva <- bit mask:	end-bit    -> bit-31
	int2raw	rvc, sv4		@ rvc <- start bit position
	lsl	rvb, rvb, rvc		@ rvb <- bit mask:	start-bit -> bit-31
	bic	rvb, rvb, rva		@ rvb <- bit mask:	start-bit -> end-bit (excluded)
	pntrp	sv2			@ is ei4 (bit-field) a bytevector?
	itE	eq
	ldreq	rva, [sv2, #4]		@	if so,  rva <- ei4 bit-field data (raw int)
	asrne	rva, sv2, #2		@	if not, rva <- ei4 bit-field data (raw int)
	lsl	rva, rva, rvc		@ rva <- bit-field, shifted in place to start-bit
	and	rva, rva, rvb		@ rva <- bit-field, trimmed to start-end size
	pntrp	sv1			@ is ei1 (source item) a bytevector?
	itE	eq
	ldreq	rvc, [sv1, #4]		@	if so,  rvc <- ei1 source item data (raw int)
	asrne	rvc, sv1, #2		@	if not, rvc <- ei1 source item data (raw int)
	bic	rvc, rvc, rvb		@ rvc <- source item data with bit-field cleared
	orr	rva, rva, rvc		@ rva <- source data with bit-field filled in
	b	bwexit			@ jump to common exit
	
@-------------------------------------------------------------------------
@
@	11.4 Exact Bitwise Operations:	Common entry and exit
@
@-------------------------------------------------------------------------
	
.balign	4
	
_func_
bwloop:	@ bitwise logical operations loop
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
	ldreq	rvb, [sv2, #4]
	asrne	rvb, sv2, #2
	adr	lnk, bwlop1
	set	pc,  sv4
	
_func_
bwexit:	@ [common exit]
	pntrp	sv1
	itTE	ne
	raw2intne sv1, rva		@ sv1 <- shifted int
	streq	rva, [sv1, #4]
	set	pc,  cnt

_func_
bwfent:	@ common function entry for paptbl
	orr	lnk, sv5, #lnkbit0	@ lnk <- set from sv5 = function code address to execute	
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
	bic	sv5, lnk, #lnkbit0	@ sv5 <- lnk, saved
	set	rvb, #8			@ rvb <- number of bytes to allocate
	bl	zmaloc			@ rva <- addr of object (symbol-tagged)
	pntrp	sv1
	itE	eq
	ldreq	rvc, [sv1, #4]
	asrne	rvc, sv1, #2		@ rvc <- ei1 data (raw int)
	str	rvc, [rva, #4]		@ store ei1 data in bytevector
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	sv1, rva, rvb		@ sv1 <- new bytevector			  [*commit destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer	[*restart critical instruction*]
	ldr	rva, [sv1, #4]		@ rva <- ei1 data (raw int)
	pntrp	sv2			@ is ei2 a bytevector?
	itE	eq
	ldreq	rvb, [sv2, #4]		@	if so,  rvb <- ei2 data (raw int)
	asrne	rvb, sv2, #2		@	if not, rvb <- ei2 data (raw int)
	pntrp	sv3			@ is ei3 a bytevector?
	itE	eq
	ldreq	rvc, [sv3, #4]		@	if so,  rvc <- ei3 data (raw int)
	asrne	rvc, sv3, #2		@	if not, rvc <- ei3 data (raw int)
	orr	lnk, sv5, #lnkbit0	@ lnk <- lnk, restored
	set	pc,  lnk		@ return


@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg


@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Addendum Register Bitwise Operations:			bitwise-copy-bit,
@								bitwise-copy-bit-field
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======


_func_
regent:	@ entry for register-oriented functions, for paptbl
	orr	lnk, sv5, #lnkbit0	@ lnk <- set from sv5 = function code address to execute
_func_
reg_en:	@ [internal entry] using bl
	int2raw	rva, sv1
	lsl	rva, rva, #4
	int2raw	rvb, sv2
	set	pc,  lnk	

.balign	4
	
srcpbt:	SYMSIZE	17
	.ascii	"register-copy-bit"
	.balign 4
	
prcpbt:	@ (register-copy-bit reg ofst ei2 ei3)
	@ on entry:	sv1 <- reg  = register base address for set/clear bit	(scheme int)
	@ on entry:	sv2 <- ofst = register offset for set/clear bit		(scheme int)
	@ on entry:	sv3 <- ei2  = bit position to set/clear			(scheme int)
	@ on entry:	sv4 <- (ei3) where ei3 = 1 to set bit, 0 to clear	(scheme int)
	@ on exit:	sv1 <- npo
	@ pre-entry:	regent sets rva to register address (sv1) and rvb to offset (raw sv2)
	EPFUNC	0, oregent, 3		@ primitive, init-sv4 = none, fentry = regent, narg = 3
	pntrp	sv4
	it	eq
	careq	sv4, sv4		@ sv4 <- ei3	
_func_
rcpbit:	@ [internal entry]
	add	rva, rva, rvb
	int2raw	rvb, sv3
	set	rvc, #1
	lsl	rvc, rvc, rvb
	ldr	rvb, [rva]
	eq	sv4, #i0
	itE	eq
	biceq	rvb, rvb, rvc
	orrne	rvb, rvb, rvc
	str	rvb, [rva]
	b	npofxt
	
.balign	4
	
srcpbf:	SYMSIZE	23
	.ascii	"register-copy-bit-field"
	.balign 4

prcpbf:	@ (register-copy-bit-field reg ofst ei2 ei3 ei4)
	@ on entry:	sv1 <- reg   = register base address for set/clear bit	(scheme int)
	@ on entry:	sv2 <- ofst  = register offset for set/clear bit	(scheme int)
	@ on entry:	sv3 <- ei2   = start position of bits to insert		(scheme int)
	@ on entry:	sv4 <- ei3   = end   position of bits to insert		(scheme int)
	@ on entry:	sv5 <- (ei4) = bit-field to insert into reg+ofst	(scheme int, listed)
	@ on exit:	sv1 <- npo
	PFUNC	4			@ primitive function, four input args
	int2raw	rvc, sv4
	set	rvb, #-1		@ rvb <- #xffffffff
	lsl	rva, rvb, rvc		@ rva <- bit mask:	end-bit    -> bit-31
	int2raw	rvc, sv3		@ rvc <- start bit position
	lsl	rvb, rvb, rvc		@ rvb <- bit mask:	start-bit -> bit-31
	bic	rvc, rvb, rva		@ rvc <- bit mask:	start-bit -> end-bit (excluded)
	bl	reg_en			@ rva <- reg address, rvb <- raw offset, from sv1-sv2
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
	bl	reg_en			@ rva <- reg address, rvb <- raw offset, from sv1-sv2
	str	rvc, [rva, rvb]
	b	npofxt

	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

	
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	11.2.	Fixnum Operations:				fx=?, fx>?, fx<?, fx>=?, fx<=?
@								fxmax, fxmin, fx+, fx*, fx-, fx/
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
@
@	Requires:
@			core:			boolxt
@
@	Modified by (switches):			
@
@=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======-=======
	
.ifdef	include_r6rs_fx

.balign	4
	
sfxeq:	SYMSIZE	4
	.ascii	"fx=?"
	.balign 4
	
pfxeq:	@ (fx=? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	eq	sv1, sv2
	b	boolxt

.balign	4
	
sfxgt:	SYMSIZE	4
	.ascii	"fx>?"
	.balign 4
	
pfxgt:	@ (fx>? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	eq	sv1, sv2
	bne	fxge
	b	notfxt
			
.balign	4
	
sfxlt:	SYMSIZE	4
	.ascii	"fx<?"
	.balign 4
	
pfxlt:	@ (fx<? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	eq	sv1, sv2
	bne	fxle
	b	notfxt
			
.balign	4

sfxge:	SYMSIZE	5
	.ascii	"fx>=?"
	.balign 4
	
pfxge:	@ (fx>=? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
_func_
fxge:	@ fx>=?
	cmp	sv1, sv2
	itE	pl
	setpl	sv1, #t
	setmi	sv1, #f
	set	pc,  cnt
			
.balign	4

sfxle:	SYMSIZE	5
	.ascii	"fx<=?"
	.balign 4
	
pfxle:	@ (fx<=? int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
_func_
fxle:	@ fx<=?
	cmp	sv2, sv1
	itE	pl
	setpl	sv1, #t
	setmi	sv1, #f
	set	pc,  cnt
			
.balign	4

sfxmax:	SYMSIZE	5
	.ascii	"fxmax"
	.balign 4
	
pfxmax:	@ (fxmax int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	cmp	sv1, sv2
	it	mi
	setmi	sv1, sv2
	set	pc,  cnt
			
.balign	4

sfxmin:	SYMSIZE	5
	.ascii	"fxmin"
	.balign 4
	
pfxmin:	@ (fxmin int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	cmp	sv2, sv1
	it	mi
	setmi	sv1, sv2
	set	pc,  cnt
			
.balign	4

sfxpls:	SYMSIZE	3
	.ascii	"fx+"
	.balign 4
	
pfxpls:	@ (fx+ int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
_func_
fxpls:	@ fx+
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
	b	corerr

.balign	4

sfxprd:	SYMSIZE	3
	.ascii	"fx*"
	.balign 4
	
pfxprd:	@ (fx* int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	int2raw	rvc, sv1		@ rva <- x2 (raw int)
	int2raw	rva, sv2		@ rva <- x1 (raw int)
	smull	rva, rvb, rvc, rva	@ rva <- x1 (raw int) * x2 (raw int), rvc <- possible overflow
	raw2int	sv1, rva
	lsl	rvb, rvb, #3
	orrs	rvb, rvb, rva, lsr #29
	it	ne
	mvnsne	rvc, rvb
	it	eq
	seteq	pc,  cnt
	@ integer product overflow, error out
	b	corerr

.balign	4

sfxmns:	SYMSIZE	3
	.ascii	"fx-"
	.balign 4
	
pfxmns:	@ (fx- int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	ngint	sv2, sv2
	b	fxpls

.balign	4

sfxdiv:	SYMSIZE	3
	.ascii	"fx/"
	.balign 4
	
pfxdiv:	@ (fx/ int1 int2)
	@ on entry:	sv1 <- int1
	@ on entry:	sv2 <- int2
	EPFUNC	0, ofxchk2, 2			@ primitive, init-sv4 = none, fentry = fxchk2, narg = 2
	set	lnk, cnt
	b	idivid


fxchk2:	@ common entry for binary fixnum functions
	@ (type-checking)
	and	rva, sv1, #0x03
	eq	rva, #i0
	itT	eq
	andeq	rvb, sv2, #0x03
	eqeq	rvb, #i0
	it	eq
	seteq	pc, sv5
	eq	rva, #i0
	it	eq
	seteq	sv1, sv2
	b	corerr
	
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~
.ltorg

.endif	@	include_r6rs_fx
