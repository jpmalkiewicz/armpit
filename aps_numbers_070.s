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

main_code_runs_from_zero	= 1	@ for DPFUNC and DSYNTAX in as_macros.s

/*------------------------------------------------------------------------------
@  Specify board- and device-specific constants and macros
@-----------------------------------------------------------------------------*/

.include "board.h"			@ Parameters Specific to each Board
.include "device_family.h"		@ HARDWARE CONSTANTS for each MCU Family
	
/*------------------------------------------------------------------------------
@ Assembler Constants and Macros
@-----------------------------------------------------------------------------*/

.include "as_constants.s"		@ assembly constants for scheme
.include "as_macros.s"			@ assembly macros for scheme

/*------------------------------------------------------------------------------
@ Various assembler/linker parameters
@-----------------------------------------------------------------------------*/

.syntax unified				@ enable Thumb-2 mode

.global _start
.global reset
.global _code_
.global	_text_link_address_		@ code address when system is running
.global _text_section_address_		@ code address in FLASH
.global	_data_link_address_		@ data address when system is running
.global _data_section_address_		@ data address in FLASH
.global _boot_section_address_		@ startup code address in FLASH (if any)
.global	end_of_armpit			@ armpit end = coprocessor code start
.global	start_of_code
.global	end_of_code
.global	start_of_data
.global	end_of_data

/*------------------------------------------------------------------------------
@ Code instructions section (.text) (start)
@-----------------------------------------------------------------------------*/

.text					@ code (_start label) link at 0x00
start_of_code:

/*------------------------------------------------------------------------------
@ data section (start)
@-----------------------------------------------------------------------------*/

.data
start_of_data:

/*------------------------------------------------------------------------------
@ Reset and Initialization (in text section)
@-----------------------------------------------------------------------------*/

.text

var_suffix_numbers =  (3 << 8) | variable_tag

@ function linkages from paptbl (must match offsets in aps_nnn.s)
otypchk =  1
oreturn =  2
oioprfn =  3
oprdnml	=  4
ordcnml	=  5
ounijpe =  6
onumgto =  7
ommglen =  8
ocxxxxr =  9
ovu8ren = 10
obwloop = 11
obwfent = 12
oregent = 13
ofxchk2 = 14
onumstr = 15
ostrnum = 16
o_alo	= 17
o_gc	= 18
o_err	= 19

.text
.word	_text_section_address_, _text_link_address_	@ .text section address
.word	_data_section_address_, _data_link_address_	@ .data section address
.word	oba_numbers,		env_numbers		@ address of obarray and environment in lib
.word	var_suffix_numbers, 	0
.word	oprdnml, 		adr_prdnml		@ paptbl index and address for link
.word	ordcnml, 		adr_rdcnml
.word	ounijpe, 		adr_unijpe
.word	onumgto, 		adr_numgto
.ifndef CORE_ONLY
  .word	ommglen, 		adr_mmglen
.endif
.word	onumstr, 		adr_numstr
.word	ostrnum, 		adr_strnum
.word	0, 			0			@ end of paptbl entries list

.include "scheme_base_6.2.Numbers.s"

.text

/*------------------------------------------------------------------------------
@ functions duplicated from or linked to main system
@-----------------------------------------------------------------------------*/

	/* link to memory allocation function: _alo */
	PRIMIT	_alo, ufun, 0
	set	rvc, o_alo
	b	adr_ptbjmp

	/* link to garbage collection function: _gc */
	PRIMIT	_gc, ufun, 0
	set	rvc, o_gc
	b	adr_ptbjmp

	/* link to error reporting function: _err */
	PRIMIT	_err, ufun, 0
	set	rvc, o_err
	b	adr_ptbjmp

	/* jump to a function in the pre-entry/linkage function table */
	PRIMIT	ptbjmp, ufun, 0
	@ in:	rvc <- function's offset in table
	@ mods:	rva
	vcrfi	rva, glv, 16		@ rva <- pre-entry function table
	ldr	rva, [rva, rvc, lsl #2]
	orr	rva, rva, #lnkbit0
	set	pc,  rva

	/* exit (used by numeric function jump tables) */
	PRIMIT	"return", ufun, 0
	set	pc,  cnt

	/* exit with #t */
	PRIMIT	trufxt, ufun, 0
	set	sv1, true
	set	pc,  cnt

	/* exit with #f */
	PRIMIT	flsfxt, ufun, 0
	set	sv1, false
	set	pc,  cnt

	/* exit with non-printing object */
	PRIMIT	npofxt, ufun, 0
	set	sv1, npo
	set	pc,  cnt

	/* boolean function exit, based on test result, eq -> #t, ne -> #f */
	PRIMIT	boolxt, ufun, 0
	itE	eq
	seteq	sv1, true
	setne	sv1, false
	set	pc,  cnt

	/* exit with #t/#f in opposition to test result, ne -> #t, eq -> #f */
	PRIMIT	notfxt, ufun, 0
	itE	eq
	seteq	sv1, false
	setne	sv1, true
	set	pc,  cnt

.ifndef enable_MPU

_func_
cons:	@ primitive cons:	called by cons and list macros
	@ in:	fre <- current free cell address (normally reserved)
	@ out:	rva <- allocated address (content is tagged as bytevector)
	@ out:	rvb <- unmodified or 8 (raw int) if gc was triggered
	@ out:	rvc <- '() (for cdr of cons cell, if called from list macro)
	@ out:	fre <- allocated address (level 1 reserved, i.e. ...bbb01)
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ unconditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	@ assuming current heaptop is stored in glv (updated by gc and/or install)
	@ assuming heap size is always 8-byte aligned
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	cmp	rva, fre		@ is an 8-byte cell available?
	itTT	hi
	bichi	rva, fre, #0x03		@	if so,  rva <- address of allocated memory
	sethi	rvc, null		@	if so,  rvc <- '() (for list macro)
	sethi	pc,  lnk		@	if so,  return to complete cons macro
alogc8:	set	rvb, 8			@ rvb <- number of bytes to allocate (8)
	b	adr__gc			@ jump to perform gc and restart cons
	
_func_
save:	@ primitive save:	called by save macro
	@ in:	fre <- current free cell address (not reserved)
	@ in:	dts <- current scheme stack
	@ out:	rva <- next free address
	@ out:	rvb <- unmodified or 8 (raw int) if gc was triggered
	@ out:	fre <- next free address (de-reserved, i.e. ...bbb10)
	@ out:	dts <- updated scheme stack with free car
	@ modifies:	rva, rvb, rvc, fre
	@ returns via:	lnk
	@ conditionally restartable when fre is at reservation level 1
	@ special considerations needed if fre is at reservation level 0
	@ assuming current heaptop is stored in glv (updated by gc and/or install)
	@ assuming heap size is always 8-byte aligned (heaptop always adjusted by 16-bytes)
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	cmp	rva, fre		@ is an 8-byte cell available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {fre, dts}	@ rva <- adrs of nxt fre cel, + str dmy in prior fre cell
	sub	dts, rva, #8		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
cons2:	@ primitive cons2:	called by bcons and lcons macros (called by save2 macro)
	@ in:	fre <- current free cell address (not reserved)
	@ out:	rva <- allocated address
	@ out:	rvc <- allocated address
	@ out:	fre <- current free cell address (reserved level 1)
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	itTT	hi
	bichi	rva, fre, #0x03		@ rva <- address of allocated memory
	sethi	rvc, rva
	sethi	pc,  lnk		@ return to complete save macro
algc16:	set	rvb, 16			@ rvb <- number of bytes to allocate (16)
	b	adr__gc			@ jump to perform gc and restart cons

_func_
cons3:	@ primitive cons3:	called by llcons macro (called by save3 macro)
	@ in:	fre <- current free cell address (not reserved)
	@ out:	rva <- allocated address
	@ out:	rvb <- allocated address
	@ out:	rvc <- allocated address + 8 (start of next cons cell)
	@ out:	fre <- current free cell address (reserved level 1)
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector 
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 24-byte cell available?
	itTTT	hi
	bichi	rva, fre, #0x03		@ rva <- address of allocated memory
	sethi	rvb, rva
	addhi	rvc, rva, #8
	sethi	pc,  lnk		@ return to complete save macro
algc24:	set	rvb, 24			@ rvb <- number of bytes to allocate (24)
	b	adr__gc			@ jump to perform gc and restart cons

_func_
sav__c:	@ primitive sav__c:	save cnt on stack
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	alogc8
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {cnt, dts}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
sav_ec:	@ primitive sav_ec:	save env and cnt on stack
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	set	rvb, dts
	stmia	rva!, {cnt, rvb, env, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
sav_rc:	@ primitive sav_rc:	save cnt on stack with room for register at top (reg cnt -> dts)
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #8		@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
	bls	algc16
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts, glv, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

_func_
savrec:	@ primitive savrec: save env and cnt on stack, with room for reg at top (reg env cnt -> dts)
	eor	fre, fre, #0x03		@ fre <- ...bbb01		(reservation level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #16		@ rva <- comparison address
	cmp	rva, fre		@ is a 24-byte cell available?
	bls	algc24
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts}
	add	rvb, rva, #8
	stmia	rva!, {cnt,rvb,env,rvc}
	sub	dts, rva, #16		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	set	pc,  lnk		@ return to complete save macro

.endif	@ no enable_MPU


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

.text

.ifdef cortex_a8
  .balign 64		@ align the end_of_code tag for cortex-a8/a9 cache line
.endif

end_of_code:

/*------------------------------------------------------------------------------
@ data section (ending)
@-----------------------------------------------------------------------------*/

.data	0
.data	1
.data	2

.balign	4	@ align the end_of_data tag

.ifdef cortex_a8
  .balign 64		@ align the end_of_code tag for cortex-a8/a9 cache line
.endif

end_of_data:


.end



