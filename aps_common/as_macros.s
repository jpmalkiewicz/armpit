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

/*----------------------------------------------------------------------------*\
|
|  switches used:
|
|	cortex, enable_MPU, inline_cons
|
\*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*\
|
|  0.G.	  ASSEMBLER MACROS for SCHEME
|
\*----------------------------------------------------------------------------*/

.macro print_numcores num
    .print "#PU: \num cores"
.endm

.macro echo_options
  .ifdef cortex_a9
    .ifdef enable_a9_mpcore
      .print " MP: multiple cores"
    .else
      .print " MP: single core"
    .endif
  .endif
  .ifdef r3rs
    .print "SCM: R3RS"
  .else
    .print "SCM: R5RS"
  .endif
  .ifdef small_memory
    .print "MEM: small"
  .else
    .print "MEM: normal"
  .endif
  .ifdef CORE_ONLY
    .ifdef include_small_lib
      .print "LIB: small"
    .else
      .print "LIB: no"
    .endif
  .else
    .print "LIB: normal"
  .endif
  .ifdef mark_and_sweep
    .ifdef enable_MPU
      .print " GC: mark & sweep, MPU"
    .else
      .print " GC: mark & sweep"
    .endif
  .else
    .ifdef enable_MPU
      .print " GC: stop & copy, MPU = NO, NO, NO (bad combo)!"
    .else
      .print " GC: stop & copy"
    .endif
  .endif
  .ifdef integer_only
    .ifdef hardware_FPU
      .print "FLT: no, but FPU is on"
    .else
      .print "FLT: no"
    .endif
  .else
    .ifdef hardware_FPU
      .print "FLT: FPU"
    .else
      .print "FLT: soft"
    .endif
  .endif
  .ifdef native_usb
    .print "USB: device"
  .else
    .print "USB: no"
  .endif
  .ifdef onboard_SDFT
    .ifdef sdhc_on_sd_mmc
      .print " SD: SDHC only"
    .else
      .print " SD: not SDHC"
    .endif
  .else
      .print " SD: no"
  .endif
  .ifdef enable_cpo
    .ifdef cpo_default_io
      .print "CPO: default io port"
    .else
      .print "CPO: enabled"
    .endif
  .endif
.endm

.macro check_options
  .ifdef stop_and_copy
    .ifdef enable_MPU
	.print	"Incompatible options: enable_MPU requires mark_and_sweep!"
    .endif
  .endif
  .ifdef stop_and_copy
    .ifdef mark_and_sweep
	.print	"Incompatible options: mark_and_sweep and stop_and_copy!"
    .endif
  .endif
  .ifdef top_level_btree
    .ifndef nrml_lambda_lkp
	.print	"Incompatible options: top_level_btree requires nrml_lambda_lkp!"
    .endif
  .endif
  .ifdef live_SD
    .ifndef onboard_SDFT
	.print	"Incompatible options: live_SD requires onboard_SDFT!"
    .endif
  .endif
.endm

/* Thumb-2 function tagging for bl */

.macro _func_
  .ifdef cortex
	.thumb_func
  .endif
.endm

/* pre-entry function table data items */

.macro	PAPTBWORD item
 .if	\item == 0
	.word	\item
 .else
	o\item	= ( . - paptbl) >> 2
	.word	adr_\item
 .endif
.endm

/* type table data items */

.macro	TYPTBWORD	label, item
	\label	= (( . - typtbl) << 2) | i0
	.byte	\item
.endm

/* main obarray and environment */

.macro	STARTOBAENV name
	.data	2
	.balign	8
	.word	(((8f - 6f) >> 2) << 8) | vector_tag
	6:
	oba_\name = .
	.set	top_obarray, oba_\name
	.set	oba_var_suffix,  variable_tag
	.word	empty_vector
	.data	1
	.balign	8
	.word	(((9f - 7f) >> 2) << 8) | vector_tag
	7:
	env_\name = .
	.word	empty_vector
.endm

.macro	OBAENVWORD name
	.data	2
	.set	oba_var_suffix, oba_var_suffix + (1 << 8)
	var_suffix_\name = oba_var_suffix
	.word	oba_\name
	.data	1
	.word	env_\name
.endm

.macro	ENDOBAENV
	.data	2
	8:		@ end of scmoba
	.data	1
	9:		@ end of scmenv
	.text
	.balign	4
.endm

/* sub- obarray and environment */

.macro	STARTSUBOBAENV	name
	.data	2
	.balign	8
	.word	((4f - 2f) << 8) | symbol_tag
	2:
	oba_\name = .
	.set	this_oba_var_suffix, var_suffix_\name
	.set	obarray_varpos,  0
	.data	1
	.balign	8
	.word	(((5f - 3f) >> 2) << 8) | vector_tag
	3:
	env_\name = .
	.text
	.balign	4
.endm

.macro	ENDSUBOBAENV
	.data	2
	4:		@ end of core_oba
	.data	1
	5:		@ end of core_env
	.text
	.balign	4
.endm

/* isr vector */

.macro	ISRVECTOR
	STARTVCTR ISR_vector
	.set	isr_position, 0
	.set	usb_isr_found, 0
	.rept	num_interrupts
	splice_isr isr_position, uart0_int_num,  uarisr, uart1_int_num,  uarisr
	.if item_is_not_isr_flag
	  splice_isr isr_position, timer0_int_num, tmrisr, timer1_int_num,tmrisr
	.endif
	.ifdef include_i2c
	  .if item_is_not_isr_flag
	    splice_isr isr_position, i2c0_int_num, i2cisr, i2c1_int_num, i2cisr
	  .endif
	.endif
	.ifdef	enable_cpo
	  .if item_is_not_isr_flag
	    splice_isr isr_position, cpo_int_num, cpoisr
	  .endif
	.endif
	.ifdef native_usb
	  .if item_is_not_isr_flag
	    splice_isr isr_position, usb_int_num, usbisr, usb_int_num_alt,usbisr
	    .ifdef usb_int_num_alt2
	      splice_isr isr_position, usb_int_num_alt2, usbisr
	      splice_isr isr_position, usb_int_num_alt3, usbisr
	    .endif
	    .ifeq item_is_not_isr_flag
	      .set usb_isr_found, 1
	    .endif
	  .endif
	.endif
	.if item_is_not_isr_flag
          .word	i0
        .endif
	.set	isr_position, isr_position + 1
	.endr
	.ifdef native_usb
	  .ifeq usb_isr_found
	    .warning "ISR: USB isr not found"
	  .endif
	.endif
	ENDsized
.endm

.macro	splice_isr pos, arg1, arg2, args:vararg
	.set	item_is_not_isr_flag, 1
   .if \pos == \arg1
        .print "ISR: \arg1 \t<- \arg2"
        .word	val_\arg2
	.set	item_is_not_isr_flag, 0
  .else
    .ifnb \args
	splice_isr \pos, \args
    .endif
  .endif
.endm

.macro	FLASH_SECTORS start, num, size, rest:vararg
	.set	flash_sector_address, \start
	.rept	\num
	  .word	flash_sector_address
	  .set	flash_sector_address, flash_sector_address + (\size<<10)
	.endr
	.ifb	\rest
	  .word	flash_sector_address
	.else
	  .set 	flash_sectors_end, 0
	  FLASH_SECTORS_endchk \rest
	  .if	flash_sectors_end
	    .word flash_sector_address
	    .word \rest
	  .else
	    FLASH_SECTORS flash_sector_address, \rest
	  .endif
	.endif
.endm

.macro	FLASH_SECTORS_endchk arg1, arg2:vararg
	.ifb	\arg2
	  .set flash_sectors_end, 1
	.endif
.endm


.macro	FLASH_SECTORS_OLD num, size, start, last:vararg
	.ifnes	"\start", "continue"
	  .set	flash_sector_address, \start
	.endif
	.rept	\num
	  .word	flash_sector_address
	  .set	flash_sector_address, flash_sector_address + (\size<<10)
	.endr
	.ifnb	\last
	  .word	flash_sector_address
	  .ifnes "\last", "end"
	    .word \last
	  .endif
	.endif
.endm


/* scheme symbol */

.macro	SMBL strng, address
	STARTSMBL \address
	.ascii	"\strng"
	ENDsized
.endm

.macro	STARTSMBL address
	.data	0
	.balign	8
	.word	((13f - \address) << 8) | symbol_tag
	\address = .
.endm

.macro	ENDsized
	13:
	.balign	4
	.text
.endm

/* scheme vector */

.macro	VCTR	target, items:vararg
	STARTVCTR \target
  .ifnes "\items", ""
	VCTR_item \items
  .endif
	ENDsized
.endm

.macro	STARTVCTR target
	.data	0
	.balign	8
	.word	((13f - \target) << 6) | vector_tag
	\target	= .
.endm

.macro	VCTR_item item1, items:vararg
	.word	\item1
  .ifnes "\items", ""
	VCTR_item \items
  .endif
.endm

/* scheme bytevector */

.macro	STARTBVU8 address
	.data	0
	.balign	8
	.word	((13f - \address) << 8) | bytevector_tag
	\address = .
.endm


/* bind name to value in sub-environment */

.macro	BNDVAR	name, arg1, arg2:vararg
  .ifeqs "\arg2", ""
	BNDVARi \name, \arg1
  .else
	BNDVARi \arg1, \arg2
  .endif
	.ascii "\name"
	ENDi
.endm

.macro	BNDVARi name, value
  .ifdef var_\name
	.print	"aps WARNING: var_\name duplicate (system_0.s?) -- re-defined!"
  .endif
	var_\name = (obarray_varpos << 16) | this_oba_var_suffix
	.set	obarray_varpos, obarray_varpos + 1
	.data	1
	.word	\value
	.data	2
	.byte	(11f - .) + 47
.endm

.macro	ENDi
	11:		@ end of ascii var name for size calculation
	.text
.endm

/* bind name to shifted register address in sub-environment */

.macro	BNDREG	name, reg
	reg_\name = (\reg >> 2) | i0
	BNDVAR	\name, reg_\name
.endm

/* bind name to primitive in sub-environment, unless ufun type */

pfun	= (1 << 12) | proc
ufun	= (1 << 12) | proc
sntx	= (1 << 12) | (1 << 11) | proc
efun	= (1 << 12) | proc		@ pre-entry-only primitive (no other code)
esntx	= (1 << 12) | (1 << 11) | proc	@ pre-entry-only syntax prim (no other code)

.macro	direct_entry_val  name, type, narg
    .ifndef main_code_runs_from_zero
	val_\name = ((adr_\name-start_of_code) << 16)|(\narg << 8)|\type | (lnkbit0 << 16)
    .else
	val_\name = (((adr_\name-start_of_code)+_text_link_address_)<<16)|(\narg<<8)|\type| (lnkbit0<<16)
    .endif

.endm

.macro	pre_entry_val  name, type, narg, fentry, initsv4
    .ifeqs "\type", "ufun"
	val_\name = (\narg<<8)|\type	@ tagged,nonbound,direct prim (eg cmpld)
    .else
	val_\name = (\fentry << 24)|((\initsv4)<<16)|(1<<13)|(\narg<<8)|\type
    .endif
.endm

.macro	PRIMITi name, type, narg, fentry:vararg
	@ 3 or 5 input arguments (3 if fentry == "", 5 otherwise)
	@ (3 input args for direct entry, 5 for pre-entry function)
  .ifeqs "\fentry", ""
	direct_entry_val  \name, \type, \narg
	.text
	.balign 4
	_func_
	adr_\name = . + lnkbit0		@ <- may not work with _func_, alt is: | lnkbit0
    .ifnes "\type","ufun"
	BNDVARi \name, val_\name
    .endif
  .else
	pre_entry_val	\name, \type, \narg, \fentry
    .ifeqs "\type", "efun"
	BNDVARi \name, val_\name
    .else
      .ifeqs "\type", "esntx"
	BNDVARi \name, val_\name
      .else
	.text
	.balign	8
	.word	val_\name
	_func_
	adr_\name = .
        .ifnes "\type","ufun"
	  BNDVARi \name, adr_\name
        .endif
      .endif
    .endif
  .endif
.endm

.macro	PRIMIT name, arg1, arg2, arg3:vararg
	@ name is primitive's name string (for scheme)
	@ arg1 is either type (arg3 == "") or re-name symbol
	@ arg2 is either narg (arg3 == "") or type
	@ arg3 is either "" or {narg} or {fentry,initsv4} or {narg,fentry,initsv4}
  .ifeqs "\arg3", ""
  	@ PRIMIT called with 3 args: non-renamed direct primitive
  	@ (eg. "lambda", sntx, 1)
	PRIMITi \name, \arg1, \arg2
  .else
  	@ PRIMIT called with 4-6 args
	PRIMIT_aux46 \name, \arg1, \arg2, \arg3
  .endif
  .ifnes "\arg1","ufun"
	.ascii "\name"
  .endif
	ENDi
.endm

.macro	PRIMIT_aux46 name, arg1, arg2, arg3, arg4:vararg
  .ifeqs "\arg4", ""
  	@ PRIMIT called with 4 args: renamed direct primitive
  	@ (eg. "symbol->string", symstr, pfun, 1)
	PRIMITi \arg1, \arg2, \arg3
  .else
  	@ PRIMIT called with 5 or 6 input args: pre-entry primitive
	PRIMIT_aux56 \name, \arg1, \arg2, \arg3, \arg4
  .endif
.endm

.macro	PRIMIT_aux56 name, arg1, arg2, arg3, arg4, arg5:vararg
  .ifeqs "\arg5", ""
  	@ PRIMIT called with 4 args: non-renamed pre-entry primitive
  	@ (eg. "quote", sntx, 1, oreturn, 0)
	PRIMITi \name, \arg1, \arg2, \arg3, \arg4
  .else
  	@ PRIMIT called with 5 args: renamed pre-entry primitive
  	@ (eg. "symbol?", symbol, pfun, 1, otypchk, ivar)
	PRIMITi \arg1, \arg2, \arg3, \arg4, \arg5
  .endif
.endm

/* bind name to numeric function with table-dispatch */

.macro	NUMTBF	name, arg2, arg3, arg4, arg5, rest:vararg
 .ifeqs	"\rest",""
	NUMTBFi	\name, \arg2, \arg3, \arg4, \arg5	@ not renamed, short
 .else
	NUMTBF_aux610	\name, \arg2, \arg3, \arg4, \arg5, \rest
 .endif
	.ascii "\name"
	ENDi
.endm

.macro	NUMTBF_aux610 name, arg2, arg3, arg4, arg5, arg6, rest:vararg
 .ifeqs	"\rest",""
	NUMTBFi	\arg2, \arg3, \arg4, \arg5, \arg6	@ renamed, short
 .else
	NUMTBF_aux910	\name, \arg2, \arg3, \arg4, \arg5, \arg6, \rest
 .endif
.endm

.macro	NUMTBF_aux910 name, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, rest:vararg
 .ifeqs	"\rest",""
	NUMTBFi	\name,\arg2,\arg3,\arg4,\arg5,\arg6,\arg7,\arg8,\arg9	@ not renamed, long
 .else
	NUMTBFi	\arg2,\arg3,\arg4,\arg5,\arg6,\arg7,\arg8,\arg9,\rest	@ renamed, long
 .endif
.endm

.macro	NUMTBFi name, narg, fentry, initsv4, fnan, funcs:vararg
	.data	0
	.balign	8
	.word	((\initsv4) << 16)|(\fentry << 24)|(1 << 13)|(\narg << 8)|pfun

	NUMJTB	\name, \fnan, \funcs
	BNDVARi num\name, tb_num\name
.endm

.macro	NUMJTB	name, fnan, funcs:vararg
	.data	0
	.balign	4
	tb_num\name = .
	.word	adr_\fnan
 .ifeqs	"\funcs",""
	.word	adr_int\name, adr_flt\name, adr_rat\name, adr_cpx\name
 .else
 	NUMTBFi_funcs \funcs
 .endif
	.text
.endm

.macro	NUMTBFi_funcs fint, fflt, frat, fcpx
	.word	adr_\fint, adr_\fflt, adr_\frat, adr_\fcpx
.endm


/* bind name to macro */

.macro	PMACRO	name, arg1, arg2, arg3:vararg
  .ifeqs "\arg3", ""
	PMACRO_aux \name, \arg1, \arg2
  .else
	PMACRO_aux \arg1, \arg2, \arg3
  .endif
	.data	2
	.byte	(11f - .) + 47
	.ascii "\name"
11:	@ end of string for size calculation
	.data	0
.endm

.macro	PMACRO_aux name, literals, body
	var_\name = (obarray_varpos << 16) | this_oba_var_suffix
	.set	obarray_varpos, obarray_varpos + 1
	.data	0
	.balign	8
	.word	(1 << 11) | proc
	adr_\name = .
	.word	\literals, \body, null
	.data	1
	.word	adr_\name
.endm

/* other things */

.macro	qLAMBDA	target, narg, vars, body, env
	@ lambda available at user-level (public)
	.data	0
	.balign	8
	.word	(1 << 14) | (\narg << 8) | proc
	\target	= .
	.word	\vars, \body, \env
	.data	1
	.word	\target
	.data	0
.endm

.macro	qLAMBD1	target, narg, vars, body, env
	@ lambda available at user-level (public)
	@ body is a single statement (include the LIST1_ in macro)
	.data	0
	.balign	8
	.word	(1 << 14) | (\narg << 8) | proc
	\target	= .
	.word	\vars, .+8 , \env, \body, null
	.data	1
	.word	\target
	.data	0
.endm

.macro	aLAMBDA	target, narg, vars, body, env
	@ lambda that is not exported to user-level (private)
	.data	0
	.balign	8
	.word	(1 << 14) | (\narg << 8) | proc
	\target	= .
	.word	\vars, \body, \env
.endm

.macro	aLAMBD1	target, narg, vars, body, env
	@ lambda that is not exported to user-level (private)
	@ body is a single statement (include the LIST1_ in macro)
	.data	0
	.balign	8
	.word	(1 << 14) | (\narg << 8) | proc
	\target	= .
	.word	\vars, .+8 , \env, \body, null
.endm

.macro	r2i rawint
	((\rawint << 2) | i0)
.endm

/*------------------------------------------------------------------------------
	4.1.6. Assignments
------------------------------------------------------------------------------*/

.macro	ash dest, val, pos
	@ in:	dest <- destination reg (must be non-gc-eable if val is imm)
	@ in:	val  <- value to shift, reg or imm
	@ in:	pos  <- amount of shift, reg
	check_if_reg \val
	.if item_is_reg_flag
	  lsl	\dest, \val, \pos
	.else
	  set	\dest, \val
	  lsl	\dest, \dest, \pos
	.endif
.endm


.macro set dest, obj
	set_aux	\dest, \obj
.endm

.macro seteq dest, obj
	set_aux	\dest, \obj, eq
.endm

.macro setne dest, obj
	set_aux	\dest, \obj, ne
.endm

.macro setmi dest, obj
	set_aux	\dest, \obj, mi
.endm

.macro setpl dest, obj
	set_aux	\dest, \obj, pl
.endm

.macro setls dest, obj
	set_aux	\dest, \obj, ls
.endm

.macro sethi dest, obj
	set_aux	\dest, \obj, hi
.endm

.macro set_aux dest, obj, cond:vararg
	check_if_reg \obj
	.if item_is_reg_flag
	  mov\cond \dest, \obj
	.else
	  check_if_smallimm (\obj)
	  .if item_is_smallimm_flag
	    mov\cond \dest, #(\obj)
	  .else
	    ldr\cond \dest, =(\obj)
	  .endif
	.endif
.endm


/*------------------------------------------------------------------------------
	6.1. Equivalence predicates
------------------------------------------------------------------------------*/


.macro eq obj1, obj2, shift
	eq_aux	\obj1, \obj2, \shift
.endm

.macro eqeq obj1, obj2, shift
	eq_aux	\obj1, \obj2, \shift, eq
.endm

.macro eqne obj1, obj2, shift
	eq_aux	\obj1, \obj2, \shift, ne
.endm

.macro	eq_aux	obj1, obj2, shift, cond
	.ifb \shift
	  teq\cond \obj1, \obj2
	.else
	  teq\cond \obj1, \obj2, \shift
	.endif
.endm

/*------------------------------------------------------------------------------
	6.2.5. Numerical operations (including addendum)
------------------------------------------------------------------------------*/

.macro incr dest, source
	add	\dest,  \source, #4
.endm

.macro increq dest, source
	addeq	\dest,  \source, #4
.endm

.macro incrne dest, source
	addne	\dest,  \source, #4
.endm

.macro decr dest, source
	sub	\dest,  \source, #4
.endm

.macro decrne dest, source
	subne	\dest,  \source, #4
.endm

.macro intgrp obj
	@ raise eq flag if obj is an integer
	@ uses rva
	and	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eq	rva, #int_tag		@ is object an integer?
.endm

.macro intgrpeq obj
	@ raise eq flag if obj is an integer
	@ uses rva
	itT	eq
	andeq	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eqeq	rva, #int_tag		@ is object an integer?
.endm

.macro intgrpne obj
	@ raise eq flag if obj is an integer
	@ modifies:	rva
	itT	ne
	andne	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eqne	rva, #int_tag		@ is object an integer?
.endm

.macro floatp obj
	@ raise eq flag if obj is a float
	@ modifies:	rva
	and	rva, \obj,  #0x03	@ rva <- two-bit tag of object in num
	eq	rva, #float_tag		@ is object a float?
.endm

.macro ratiop obj
	@ raise eq flag if obj is a rational
	@ modifies:	rva
	and	rva, \obj, #0x07
	eq	rva, #0x04
	itTT	eq
	ldreq	rva, [\obj, #-4]
	andeq	rva, rva,  #0x0F	@ rva <- four-bit tag of object
	eqeq	rva, #rational_tag	@ is object a rational?
.endm

.macro cmplxp obj
	@ raise eq flag if obj is a complex
	@ modifies:	rva
	and	rva, \obj, #0x07
	eq	rva, #0x04
	itTT	eq
	ldreq	rva, [\obj, #-4]
	andeq	rva, rva,  #0x0F	@ rva <- four-bit tag of object
	eqeq	rva, #complex_tag	@ is object a complex?
.endm

.macro zero obj
	eq	\obj, #i0
	it	ne
	eqne	\obj, #f0
.endm

.macro zerop obj
	eq	\obj, #i0
	it	ne
	eqne	\obj, #f0
.endm

.macro zeropne obj
	eqne	\obj, #i0
	it	ne
	eqne	\obj, #f0
.endm

.macro anyzro obj1, obj2
	eq	\obj1, #i0
	it	ne
	eqne	\obj1, #f0
	it	ne
	eqne	\obj2, #i0
	it	ne
	eqne	\obj2, #f0
.endm

.macro isnan obj
	ldr	rva, =scheme_nan	@ rva <- nan
	eq	\obj, rva		@ is obj = nan ?
.endm
	
.macro anynan obj1, obj2
	ldr	rvc, =scheme_nan	@ rvc <- nan
	eq	\obj1, rvc		@ is obj1 = nan ?
	it	ne
	eqne	\obj2, rvc		@	if not, is obj2 = nan ?
.endm

.macro isinf obj
	ldr	rva, =scheme_inf	@ rva <- inf
	fabs	rvb, \obj		@ rvb <- obj without sign
	eq	rvb, rva		@ is x1 = +/-inf ?
.endm
	
.macro anyinf obj1, obj2
	ldr	rva, =scheme_inf	@ rva <- inf
	bic	rvb, \obj1, #0x80000000	@ rvb <- x1 without sign
	eq	rvb, rva		@ is x1 = +/-inf ?
	itT	ne
	bicne	rvb, \obj2, #0x80000000	@ rvb <- x1 without sign
	eqne	rvb, rva		@ is x1 = +/-inf ?
.endm
	
.macro plus dest, val1, val2
	add	\dest, \val1, \val2
	eor	\dest, #0x03
.endm

.macro plusne dest, val1, val2
	addne	\dest, \val1, \val2
	it	ne
	eorne	\dest, #0x03
.endm

.macro ngflt dest, val
	eor	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngflteq dest, val
	eoreq	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngfltne dest, val
	eorne	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngfltmi dest, val
	eormi	\dest, \val, #0x80000000	@ dest <- -val
.endm

.macro ngint dest, val
	mvn	\dest, \val
	add	\dest, \dest, #3
.endm

.macro nginteq dest, val
	mvneq	\dest, \val
	it	eq
	addeq	\dest, \dest, #3
.endm

.macro ngintne dest, val
	mvnne	\dest, \val
	it	ne
	addne	\dest, \dest, #3
.endm

.macro ngintmi dest, val
	mvnmi	\dest, \val
	it	mi
	addmi	\dest, \dest, #3
.endm

.macro postv num
	tst	\num, #0x80000000
.endm

.macro postveq num
	tsteq	\num, #0x80000000
.endm

.macro postvne num
	tstne	\num, #0x80000000
.endm

.macro iabs dest, val
	postv	\val
	itEE	eq
	seteq	\dest, \val
	mvnne	\dest, \val
	addne	\dest, \dest, #3
.endm

.macro fabs dest, val
	bic	\dest, \val, #0x80000000	@ \dest <- unsigned \val (scheme float)	
.endm

.macro rawsplt wrd1, wrd2, ratcpx
	@ returns the two parts (raw) of a  rat/cpx
	@ wrd1 and wrd2 registers should be ascending
  .ifndef cortex
	ldmda	\ratcpx, {\wrd1, \wrd2}		@ <- this may be backwards (wrd1 vs wrd2)
  .else
	ldr	\wrd1, [\ratcpx, #-4]
	ldr	\wrd2, [\ratcpx]
  .endif
.endm
	
.macro rwspleq wrd1, wrd2, ratcpx
	@ returns the two parts (raw) of a  rat/cpx
	@ wrd1 and wrd2 registers should be ascending
  .ifndef cortex
	ldmdaeq	\ratcpx, {\wrd1, \wrd2}		@ <- this may be backwards (wrd1 vs wrd2)
  .else
	ldreq	\wrd1, [\ratcpx, #-4]
	it	eq
	ldreq	\wrd2, [\ratcpx]
  .endif
.endm
	
.macro numerat dest, rat
	@ returns the numerator (scheme int) of a rational
	@ modifies:	rva, rvb
	rawsplt	rva, rvb, \rat
	lsr	rva, rva, #2
	orr	rva, rva, rvb, lsl #30
	orr	\dest, rva, #int_tag
.endm
	
.macro nmrtreq dest, rat
	@ returns the numerator (scheme int) of a rational
	@ modifies:	rva, rvb
	rwspleq rva, rvb, \rat
	itTT	eq
	lsreq	rva, rva, #2
	orreq	rva, rva, rvb, lsl #30
	orreq	\dest, rva, #int_tag
.endm
	
.macro denom dest, rat
	@ returns the denominator (scheme int) of a rational
	@ modifies:	rva
	ldr	rva, [\rat]
	bic	rva, rva, #3
	orr	\dest, rva, #int_tag
.endm

.macro spltrat nmr, dnm, rat
	@ returns the numerator (scheme int) and denominator (scheme int) of a rational
	@ modifies:	rva, rvb
	rawsplt	rva, rvb, \rat
	lsr	rva, rva, #2
	orr	rva, rva, rvb, lsl #30	
	bic	rvb, rvb, #3
	orr	\nmr, rva, #int_tag
	orr	\dnm, rvb, #int_tag
.endm

.macro real dest, cpx
	@ returns the real part (scheme float) of a complex
	@ modifies:	rva, rvb
	rawsplt	rva, rvb, \cpx
	lsr	rva, rva, #2
	orr	\dest, rva, rvb, lsl #30
.endm

.macro imag dest, cpx
	@ returns the imaginary (scheme float) of a complex
	@ modifies:	rva
	ldr	rva, [\cpx]
	bic	rva, rva, #3
	orr	\dest, rva, #float_tag
.endm

.macro spltcpx real, imag, cpx
	@ returns the real part (scheme float) and imaginary part (scheme float) of a complex
	@ modifies:	rva, rvb
	rawsplt	rva, rvb, \cpx
	lsr	rva, rva, #2
	orr	\real, rva, rvb, lsl #30
	bic	rvb, rvb, #3
	orr	\imag, rvb, #float_tag
.endm
	
.macro ngnum dest, num
	tst	\num, #int_tag		@ is val a integer?
	itE	eq
	ngflteq	\dest, \num
	ngintne	\dest, \num
.endm

.macro fltmte exp, num
	@ convert float to 'in-place' mantissa and biased exponent
	@ on entry:	num <- float (scheme float)
	@ on exit:	num <- signed 'in-place' mantissa of input num (scheme int)
	@ on exit:	exp <- biased exponent of input num (raw int)
	@ modifies:	num, exp
	lsr	\exp, \num, #23		@ \exp  <- exponent and sign (raw)
	bic	\num, \num,\exp,lsl #23	@ \num <- mantis of \num (pseudo scheme float)
	eor	\num, \num, #0x03	@ \num <- mantissa of \num (scheme int)
	tst	\exp, #0xff		@ is exponent zero ?
	itEE	ne
	orrne	\num, \num, #(1<<23)	@	if not, \num <- mantissa with 1. of normalization
	lsleq	\num, \num, #1		@	if so,  \num <- mantis shftd lft (psd scheme float)
	eoreq	\num, \num, #0x03	@	if so,  \num <- mantissa shifted left (scheme int)
	tst	\exp, #0x0100		@ is number positive?
	itTT	ne
	bicne	\exp, \exp, #0x0100	@	if not, \exp <- expon without sign of num (raw int)
	mvnne	\num, \num		@	if not, negate mantissa
	addne	\num, \num, #3		@	if not, negate mantissa
.endm

/*------------------------------------------------------------------------------
	6.3.2. Pairs and lists
------------------------------------------------------------------------------*/


.macro memsetlnk
  .ifndef cortex
	sub	lnk, pc,  #4		@ lnk <- memory transaction return adrs
  .else
	set	lnk, pc
	nop
	orr	lnk, lnk,  #lnkbit0	@ lnk <- mem trnsct ret adrs Thumb2 mode
  .endif
.endm

.macro memfrchk8
	eor	fre, fre, #0x03		@ fre <- ...bbb01	(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	cmp	rva, fre		@ is a 16-byte cell available?
.endm

.macro memfrck bytes
	eor	fre, fre, #0x03		@ fre <- ...bbb01	(reserv level 1)
	vcrfi	rva, glv, 1		@ rva <- heaptop -- from global vector
	sub	rva, rva, #(\bytes - 8)	@ rva <- comparison address
	cmp	rva, fre		@ is a 16-byte cell available?
.endm

.macro cons1 upd, dest, car, cdr
	@ upd is rva without MPU or fre with MPU
	bic	\upd, fre, #0x03	@ upd  <- fre cel adrs and, w/MPU, (reservation level 2)
	stmia	\upd!, {\car, \cdr}	@ upd  <- nxt fre cel adrs + str car, cdr in free cell
	sub	\dest, \upd, #8		@ dest <- address of save cell, [*commit save destination*]
	orr	fre, \upd, #0x02	@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro icons dest, car, cdr
	@ inlined cons or cons with MPU enabled
	@ dest <- (car . cdr)
  .ifdef enable_MPU
  	cons1	fre, \dest, \car, \cdr	@ reserve, store, commit and de-reserve
  .else
	memsetlnk			@ lnk <- memory trans return adrs (adjustd for T2 mode)
	memfrchk8			@ reserve memory, sufficient space available?
	bls	alogc8			@	if not,  jump to perform gc
  	cons1	rva, \dest, \car, \cdr	@ store, commit and de-reserve
  .endif
.endm

.macro cons dest, car, cdr
	@ generic cons
	@ dest <- (car . cdr)
  .ifdef enable_MPU
	icons	\dest, \car, \cdr
  .else
    .ifdef inline_cons
	icons	\dest, \car, \cdr
    .else
	bl	cons		   @ rva <- fre cel adrs (gc if ndd), rvb <- 8, fre-ptr rsrvd lvl 1
	stmia	rva!, {\car, \cdr} @ rva <- nxt fre cel adrs + store car-cdr in prior free cell
	sub	\dest, rva, #8	   @ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02	   @ de-reserve free-pointer, [*restart critical instruction*]
    .endif
  .endif
.endm

.macro list1 upd, dest, obj
	bic	\upd, fre, #0x03   @ upd <- address of allocated memory
	set	rvc, null
	stmia	\upd!, {\obj, rvc} @ upd <- adrs of nxt fre cel + str car-cdr in prior fre cel
	sub	\dest, \upd, #8	   @ \dest <- address of cons cell, [*commit list destination*]
	orr	fre, \upd, #0x02   @ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro ilist dest, obj
	@ inlined list or list with MPU
	@ dest <- (obj)  -- i.e. obj consed with #null
	@ modifies:	rvc
  .ifdef enable_MPU
  	list1	fre, \dest, \obj @ reserve, store, commit and de-reserve
  .else
	memsetlnk		 @ lnk <- memory transaction return address (adjustd for T2 mode)
	memfrchk8		 @ reserve memory, sufficient space available?
	bls	alogc8		 @	if not,  jump to perform gc
  	list1	rva, \dest, \obj @ store, commit and de-reserve
  .endif
.endm

.macro list dest, obj
	@ generic list
	@ dest <- (obj)  -- i.e. obj consed with #null
  .ifdef enable_MPU
	ilist	\dest, \obj
  .else
    .ifdef inline_cons
	ilist	\dest, \obj
    .else
	bl	cons		  @ rva <- fre cel adrs (gc if ndd), rvb <- 8, fre-ptr rsrvd lvl 1
	stmia	rva!, {\obj, rvc} @ rva <- nxt fre cel adrs, + store car-cdr in prior free cell
	sub	\dest, rva, #8	  @ \dest <- address of cons cell, [*commit list destination*]
	orr	fre, rva, #0x02	  @ de-reserve free-pointer, [*restart critical instruction*]
    .endif
  .endif
.endm


.macro bcons1 upd, dest, bcar, bcdr, rest
	@ upd is rva without MPU or fre with MPU
	bic	\upd, fre, #0x03 @ rva <- address of allocated memory
	set	rvc, \upd
 	stmia	\upd!, {\bcar,\bcdr,rvc}
	stmia	\upd!, {\rest}
	sub	\dest, \upd, #8	 @ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, \upd, #0x02 @ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro ibcons dest, bcar, bcdr, rest
  .ifdef enable_MPU
  	bcons1	fre, \dest, \bcar, \bcdr, \rest	@ reserve, store, commit and de-reserve
  .else
	memsetlnk				@ lnk <- mem trans ret adrs (adjustd for T2 mode)
	memfrck	16				@ reserve memory, sufficient space available?
	bls	algc16				@	if not,  jump to perform gc
  	bcons1	rva, \dest, \bcar, \bcdr, \rest	@ store, commit and de-reserve
  .endif
.endm

.macro bcons dest, bcar, bcdr, rest
	@ generic cons-binding
	@ dest <- ((bcar . bcdr) . rest)
  .ifdef enable_MPU
	ibcons	\dest, \bcar, \bcdr, \rest
  .else
    .ifdef inline_cons
	ibcons	\dest, \bcar, \bcdr, \rest
    .else
	bl	cons2
	stmia	rva!, {\bcar,\bcdr,rvc}
	stmia	rva!, {\rest}
	sub	\dest, rva, #8		@ \dest <- adrs of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
    .endif
  .endif
.endm

.macro cons2 upd, dest, car, cdr, cddr
	bic	\upd, fre, #0x03	@ rva <- address of allocated memory
	set	rvc, \upd
 	stmia	\upd!, {\cdr,\cddr}
	stmia	\upd!, {\car,rvc}
	sub	\dest, \upd, #8		@ \dest <- adrs of cons cell, [*commit cons destination*]
	orr	fre, \upd, #0x02	@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro lcons dest, car, cdr, cddr
	@ generic lcons: cons 2 items onto cddr
	@ dest <- (car . (cdr . cddr))
  .ifdef enable_MPU
	cons2	fre, \dest, \car, \cdr, \cddr
  .else
    .ifdef inline_cons
	memsetlnk			@ lnk <- mem trans ret adrs (adjustd for T2 mode)
	memfrck	16			@ reserve memory, sufficient space available?
	bls	algc16			@	if not,  jump to perform gc
	cons2	rva, \dest, \car, \cdr, \cddr
    .else
	bl	cons2			@ rva and rvc <- allocated address
	stmia	rva!, {\cdr,\cddr}
	stmia	rva!, {\car,rvc}
	sub	\dest, rva, #8		@ \dest <- cons cell adrs, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
    .endif
  .endif
.endm

.macro cons3 upd, dest, car, cdr, cddr, cdddr
	@ dest <- (car . (cdr . (cddr . cdddr)))
	bic	\upd, fre, #0x03	@ rva <- address of allocated memory
	set	rvc, \upd
	stmia	\upd!, {\cddr, \cdddr}
	stmia	\upd!, {\cdr, rvc}
	sub	rvc, \upd, #8
	stmia	\upd!, {\car, rvc}
	sub	\dest, \upd, #8		@ \dest <- cons cell adrs, [*commit cons destination*]
	orr	fre, \upd, #0x02	@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro illcons dest, car, cdr, cddr, cdddr
	@ llcons with inline cons or enable_MPU
	@ dest <- (car . (cdr . (cddr . cdddr)))
  .ifdef enable_MPU
	cons3	fre, \dest, \car, \cdr, \cddr, \cdddr	@ reserve, store, commit and de-reserve
  .else
	memsetlnk				@ lnk <- mem trans ret adrs (adjustd for T2 mode)
	memfrck	24				@ reserve memory, sufficient space available?
	bls	algc24				@	if not,  jump to perform gc
	cons3	rva, \dest, \car, \cdr, \cddr, \cdddr	@ store, commit and de-reserve
  .endif
.endm

.macro llcons dest, car, cdr, cddr, cdddr
	@ generic llcons: cons 3 items onto cdddr
	@ dest <- (car . (cdr . (cddr . cdddr)))
  .ifdef enable_MPU
	illcons	\dest, \car, \cdr, \cddr, \cdddr
  .else
    .ifdef inline_cons
	illcons	\dest, \car, \cdr, \cddr, \cdddr
    .else
	bl	cons3		@ rva and rvb <- allocated address, rvc <- alloc + 8
	stmia	rva!, {\cddr,\cdddr}
	stmia	rva!, {\cdr}
	stmia	rva!, {rvb,\car,rvc}
	sub	\dest, rva, #8	@ \dest <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02	@ de-reserve free-pointer, [*restart critical instruction*]
    .endif
  .endif
.endm

.macro isave reg
	@ inlined save or save with MPU
	@ dts <- (reg . dts)
  .ifdef enable_MPU
  	cons1	fre, dts, \reg, dts
  .else
	memsetlnk
	memfrchk8
	bls	alogc8			@	if not,  jump to perform gc
  	cons1	rva, dts, \reg, dts
  .endif
.endm

.macro	save reg1, reg2:vararg
  .ifeqs "\reg2", ""
	@ dts <- (reg1 . dts)
    .ifdef enable_MPU
	isave	\reg1
    .else
      .ifdef inline_cons
	isave	\reg1
      .else
	bl	save			@ dts <- updated scheme stack with free car or 1st cell
	setcar	dts, \reg1		@ update car of the updated dts
      .endif
    .endif
  .else
  	save23	\reg1, \reg2
  .endif
.endm

.macro	save23 reg1, reg2, reg3:vararg
  .ifeqs "\reg3", ""
	@ dts <- (reg1 reg2 . dts)
	lcons	dts, \reg1, \reg2, dts
  .else
	@ dts <- (reg1 reg2 reg3 . dts)
	llcons	dts, \reg1, \reg2, \reg3, dts
  .endif
.endm

.macro restor reg1, reg2:vararg
	ldmia	dts, {\reg1, dts}
  .ifnes "\reg2", ""
	restor	\reg2
  .endif
.endm

.macro restoreq reg1, reg2:vararg
	ldmiaeq	dts, {\reg1, dts}
  .ifnes "\reg2", ""
	it eq
	restoreq \reg2
  .endif
.endm

.macro restorne reg1, reg2:vararg
	ldmiane	dts, {\reg1, dts}
  .ifnes "\reg2", ""
	it ne
	restorne \reg2
  .endif
.endm

.macro tagwenv dest, tag, obj1, obj2
	@ dest <- [tag obj1 obj2 env]
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ rva <- address of allocated memory
  	set	rva, \tag		@ rva <- tag
	stmia	fre!, {rva, \obj1}
	stmia	fre!, {\obj2, env}
	sub	\dest, fre, #12		@ \dest <- cons cell adrs, [*commit cons destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	memsetlnk			@ lnk <- memory trans return adrs (adjustd for T2 mode)
	memfrck	16			@ reserve memory, sufficient space available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {\tag}
	stmia	rva!, {\obj1, \obj2, env}
	sub	\dest, rva, #12		@ \dest <- cons cell adrs, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .endif
.endm

.macro tagwnul dest, tag, obj1, obj2
	@ dest <- [tag obj1 obj2 ()]
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ rva <- address of allocated memory
  	set	rva, \tag		@ rva <- tag
	stmia	fre!, {rva, \obj1}
	set	rvc, null
	stmia	fre!, {\obj2, rvc}
	sub	\dest, fre, #12		@ \dest <- cons cell adrs, [*commit cons destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	memsetlnk			@ lnk <- mem trans ret adrs (adjustd for T2 mode)
	memfrck	16			@ reserve memory, sufficient space available?
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {\tag}
	set	rvc, null
	stmia	rva!, {\obj1, \obj2, rvc}
	sub	\dest, rva, #12		@ \dest <- cons cell adrs, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .endif
.endm


.macro sav__c
	@ dts <- (cnt . dts)
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ rva <- address of allocated memory
	stmia	fre!, {cnt, dts}
	sub	dts, fre, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
    .ifdef inline_cons
	memsetlnk			@ lnk <- memory trans return adrs (adjustd for T2 mode)
	memfrchk8			@ reserve memory, sufficient space available?
	bls	alogc8			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	stmia	rva!, {cnt, dts}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
    .else
	bl	sav__c
    .endif
  .endif
.endm

.macro isav_ec
	@ inlined version of sav_ec (below)
	@ dts <- (env cnt . dts)
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, fre
	stmia	fre!, {cnt, dts}
	stmia	fre!, {env, rvc}
	sub	dts, fre, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	memsetlnk			@ lnk <- memory trans return adrs, (adjustd for T2 mode)
	memfrck	16			@ fre <- ...bbb01 (level 1 rsrvd) + set ls flags if gc ndd
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	set	rvb, dts
	stmia	rva!, {cnt, rvb, env, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .endif
.endm

.macro sav_ec
	@ dts <- (env cnt . dts)
  .ifdef enable_MPU
	isav_ec
  .else
    .ifdef inline_cons
	isav_ec
    .else
	bl	sav_ec
    .endif
  .endif
.endm

.macro sav_rc reg
	@ dts <- (reg cnt . dts)
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, fre
	stmia	fre!, {cnt, dts}
	stmia	fre!, {\reg, rvc}
	sub	dts, fre, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
    .ifdef inline_cons
	memsetlnk			@ lnk <- memory trans return adrs, (adjustd for T2 mode)
	memfrck	16			@ fre <- ...bbb01 (level 1 rsrvd) + set ls flags if gc ndd
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	stmia	rva!, {cnt, dts}
	stmia	rva!, {\reg, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
    .else
	bl	sav_rc
	setcar	dts, \reg
    .endif
  .endif
.endm

.macro savrec reg
	@ dts <- (reg env cnt . dts)
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, fre
	stmia	fre!, {cnt, dts}
	stmia	fre!, {env, rvc}
	sub	rvc, fre, #8
	stmia	fre!, {\reg, rvc}
	sub	dts, fre, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
    .ifdef inline_cons
	memsetlnk			@ lnk <- memory trans return adrs, (adjustd for T2 mode)
	memfrck	24			@ fre <- ...bbb01 (level 1 rsrvd) + set ls flags if gc ndd
	bls	algc24			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- address of allocated memory
	set	rvc, rva
	set	rvb, dts
	stmia	rva!, {cnt, rvb, env, rvc}
	sub	rvc, rva, #8
	stmia	rva!, {\reg, rvc}
	sub	dts, rva, #8		@ dts <- address of cons cell, [*commit cons destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
    .else
	bl	savrec
	setcar	dts, \reg
    .endif
  .endif
.endm

.macro vctrcr4 dest, reg1, reg2, reg3, reg4
	@ dest <- #(cnt reg1 reg2 reg3 reg4)
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ fre <- ...bbb00	(reserv level 2)
	set	rva, vector_tag
	orr	rva, rva, #0x500
	set	rvb, cnt
	stmia	fre!, {rva,rvb,\reg1,\reg2,\reg3,\reg4}	@ rva <- adr nxt fre cel,str reg+dts in cel
	sub	\dest, fre, #20		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	memsetlnk			@ lnk <- memory trans return adrs, (adjustd for T2 mode)
	memfrck	24			@ fre <- ...bbb01 (level 1 rsrvd) & set ls flags if gc ndd
	bls	algc24			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- free cell address
	set	rvc, vector_tag
	orr	rvc, rvc, #0x500
	stmia	rva!, {rvc}
	stmia	rva!, {cnt,\reg1,\reg2,\reg3,\reg4}	@ rva <- adr nxt fre cel,str reg+dts in cel
	sub	\dest, rva, #20		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]

  .endif
.endm

.macro vctr3 dest, reg1, reg2, reg3
	@ dest <- #(reg1 reg2 reg3)
  .ifdef enable_MPU
	bic	fre, fre, #0x03		@ fre <- ...bbb00		(reservation level 2)
	set	rva, vector_tag
	orr	rva, rva, #0x300
	stmia	fre!, {rva,\reg1,\reg2,\reg3}	@ fre <- adrs nxt fre cel, stor reg, dts in fre cel
	sub	\dest, fre, #12		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, fre, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
  .else
	memsetlnk			@ lnk <- memory trans return adrs, (adjustd for T2 mode)
	memfrck	16			@ fre <- ...bbb01 (level 1 rsrvd) & set ls flags if gc ndd
	bls	algc16			@	if not,  jump to perform gc
	bic	rva, fre, #0x03		@ rva <- free cell address
	set	rvc, vector_tag
	orr	rvc, rvc, #0x300
	stmia	rva!, {rvc}
	stmia	rva!, {\reg1,\reg2,\reg3}	@ rva <- adrs nxt fre cel, stor reg, dts in fre cel
	sub	\dest, rva, #12		@ dts <- address of save cell, [*commit save destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]

  .endif
.endm


.macro car dest, pair
	ldr	\dest,  [\pair]
.endm

.macro careq dest, pair
	ldreq	\dest,  [\pair]
.endm

.macro carne dest, pair
	ldrne	\dest,  [\pair]
.endm

.macro carmi dest, pair
	ldrmi	\dest,  [\pair]
.endm

.macro carpl dest, pair
	ldrpl	\dest,  [\pair]
.endm

.macro cdr dest, pair
	ldr	\dest,  [\pair, #4]
.endm

.macro cdreq dest, pair
	ldreq	\dest,  [\pair, #4]
.endm

.macro cdrne dest, pair
	ldrne	\dest,  [\pair, #4]
.endm

.macro cdrmi dest, pair
	ldrmi	\dest,  [\pair, #4]
.endm

.macro cdrhi dest, pair
	ldrhi	\dest,  [\pair, #4]
.endm

.macro cdrpl dest, pair
	ldrpl	\dest,  [\pair, #4]
.endm

.macro setcar pair, obj
	str	\obj,  [\pair]
.endm

.macro setcareq pair, obj
	streq	\obj,  [\pair]
.endm

.macro setcarne pair, obj
	strne	\obj,  [\pair]
.endm

.macro setcarmi pair, obj
	strmi	\obj,  [\pair]
.endm

.macro setcdr pair, obj
	str	\obj,  [\pair, #4]
.endm

.macro setcdreq pair, obj
	streq	\obj,  [\pair, #4]
.endm

.macro setcdrne pair, obj
	strne	\obj,  [\pair, #4]
.endm

.macro setcdrhi pair, obj
	strhi	\obj,  [\pair, #4]
.endm

.macro setcdrpl pair, obj
	strpl	\obj,  [\pair, #4]
.endm

.macro caar dest, pair
	car	\dest,  \pair
	car	\dest,  \dest
.endm

.macro caareq dest, pair
	careq	\dest,  \pair
	careq	\dest,  \dest
.endm

.macro caarne dest, pair
	carne	\dest,  \pair
	carne	\dest,  \dest
.endm

.macro cadr dest, pair
	cdr	\dest,  \pair
	car	\dest,  \dest
.endm

.macro cadrne dest, pair
	cdrne	\dest,  \pair
	it	ne
	carne	\dest,  \dest
.endm

.macro cdar dest, pair
	car	\dest,  \pair
	cdr	\dest,  \dest
.endm

.macro cdarne dest, pair
	carne	\dest,  \pair
	it	ne
	cdrne	\dest,  \dest
.endm

.macro cdarpl dest, pair
	carpl	\dest,  \pair
	cdrpl	\dest,  \dest
.endm

.macro cddr dest, pair
	cdr	\dest,  \pair
	cdr	\dest,  \dest
.endm

.macro caaar dest, pair
	car	\dest,  \pair
	car	\dest,  \dest
	car	\dest,  \dest
.endm

.macro cadar dest, pair
	car	\dest,  \pair
	cdr	\dest,  \dest
	car	\dest,  \dest
.endm

.macro cadarne dest, pair
	carne	\dest,  \pair
	itT	ne
	cdrne	\dest,  \dest
	carne	\dest,  \dest
.endm

.macro caddr dest, pair
	cdr	\dest,  \pair
	cdr	\dest,  \dest
	car	\dest,  \dest
.endm

.macro caddrne dest, pair
	cdrne	\dest,  \pair
	itT	ne
	cdrne	\dest,  \dest
	carne	\dest,  \dest
.endm

.macro cdaar dest, pair
	car	\dest,  \pair
	car	\dest,  \dest
	cdr	\dest,  \dest
.endm

.macro cdddr dest, pair
	cdr	\dest,  \pair
	cdr	\dest,  \dest
	cdr	\dest,  \dest
.endm

/*------------------------------------------------------------------------------
	6.3.5. Strings
------------------------------------------------------------------------------*/

.macro straloc dest, size
	@ dest <- (make-string size)
	@ dest and size must be different registers and not rva, rvb
	int2raw	rvb, \size		@ rvb <- #bytes to allocate for data
	@ allocate the aligned object
	bl	adr__alo		@ rva <- obj adrs (sym-taggd), fre <- addr (rsrvd level 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	\dest, rva, rvb		@ \dest <- strng adrs (sym), [*commit string destination*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instruction*]
	@ update the object's tag for actual size and type (string)
	lsl	rva, \size, #6
	orr	rva, rva, #string_tag
	str	rva, [\dest, #-4]	@ update string tag
.endm

.macro strlen dest, string
	ldr	rvb, [\string, #-4]
	lsr	\dest, rvb, #6
.endm

.macro strref char, string, position
	@ modifies:	rva
  .ifndef	cortex
	ldrb	rva, [\string,  \position, ASR #2]
  .else
	asr	rva, \position, #2
	ldrb	rva, [\string, rva]
  .endif
	lsl	rva, rva, #8
	orr	\char, rva, #char_tag
.endm


.macro strset string, position, char
	@ modifies:	rva, rvc (rvc on cortex only)
	chr2raw	rva, \char
  .ifndef cortex
	strb	rva, [\string, \position, ASR #2]
  .else
	asr	rvc, \position, #2
	strb	rva, [\string, rvc]
  .endif
.endm

/*------------------------------------------------------------------------------
	6.3.6. Vectors
------------------------------------------------------------------------------*/

.macro CTX_IT cond:vararg
	.ifdef cortex
	  .ifnb \cond
	    it \cond
	  .endif
	.endif
.endm

.macro veclen dest, vector
	ldr	rvb, [\vector, #-4]
	lsr	\dest, rvb, #6
.endm

.macro vecleneq dest, vector
	ldreq	rvb, [\vector, #-4]
	CTX_IT	eq
	lsreq	\dest, rvb, #6
.endm

.macro veclenne dest, vector
	ldrne	rvb, [\vector, #-4]
	CTX_IT	ne
	lsrne	\dest, rvb, #6
.endm

.macro	vecref res, vec, pos
	vecref_aux \res, \vec, \pos
.endm

.macro	vecrefeq res, vec, pos
	vecref_aux \res, \vec, \pos, eq
.endm

.macro	vecrefne res, vec, pos
	vecref_aux \res, \vec, \pos, ne
.endm

.macro	vecrefmi res, vec, pos
	vecref_aux \res, \vec, \pos, mi
.endm

.macro	vecrefpl res, vec, pos
	vecref_aux \res, \vec, \pos, pl
.endm

.macro	vecrefhi res, vec, pos
	vecref_aux \res, \vec, \pos, hi
.endm

.macro	vecref_aux res, vec, pos, cond:vararg
	@ in:	vec <- vector,	 reg		(scheme vector)
	@ in:	pos <- position, reg or imm 	(scheme int)
	@ out:	res <- item from vector, reg
	@ mods:	rva 	(if pos is reg)
	check_if_reg \pos
	.if item_is_reg_flag
	  bic\cond rva, \pos, #0x03
	  CTX_IT \cond
	  ldr\cond \res, [\vec, rva]
	.else
	  ldr\cond \res, [\vec, #((\pos) << 2)]
	.endif
.endm

.macro vcrfi dest, vector, position
	vecref	\dest,  \vector, \position
.endm

.macro vcrfieq dest, vector, position
	vecrefeq \dest,  \vector, \position
.endm

.macro vcrfine dest, vector, position
	vecrefne \dest,  \vector, \position
.endm

.macro vcrfimi dest, vector, position
	vecrefmi \dest,  \vector, \position
.endm

.macro vcrfipl dest, vector, position
	vecrefpl \dest,  \vector, \position
.endm

.macro vcrfihi dest, vector, position
	vecrefhi \dest,  \vector, \position
.endm

.macro	vecset	vec, pos, val
	vecset_aux \vec, \pos, \val
.endm

.macro	vecseteq vec, pos, val
	vecset_aux \vec, \pos, \val, eq
.endm

.macro	vecsetne vec, pos, val
	vecset_aux \vec, \pos, \val, ne
.endm

.macro	vecsetpl vec, pos, val
	vecset_aux \vec, \pos, \val, pl
.endm

.macro	vecsetmi vec, pos, val
	vecset_aux \vec, \pos, \val, mi
.endm

.macro	vecsethi vec, pos, val
	vecset_aux \vec, \pos, \val, hi
.endm

.macro	vecset_aux vec, pos, val, cond:vararg
	@ in:	vec <- vector, 	 reg 		(scheme vector)
	@ in:	pos <- position, reg or imm 	(scheme int)
	@ in:	val <- item to store in vector, reg or imm
	@ mods:	rva	if pos is imm
	@ mods:	rvb	if val is imm (small or large)
	check_if_reg \val
	.if item_is_reg_flag
	  check_if_reg \pos
	  .if item_is_reg_flag
	    bic\cond rva, \pos, #0x03
	    CTX_IT \cond
	    str\cond \val, [\vec, rva]
	  .else
	    str\cond \val, [\vec, #((\pos) << 2)]
	  .endif
	.else
	  set\cond rvb, \val
	  CTX_IT \cond
	  vecset_aux \vec, \pos, rvb, \cond
	.endif
.endm

.macro vcsti vector, position, obj
	vecset	\vector, \position, \obj
.endm

.macro vcstieq vector, position, obj
	vecseteq	\vector, \position, \obj
.endm

.macro vcstine vector, position, obj
	vecsetne	\vector, \position, \obj
.endm

.macro vcstipl vector, position, obj
	vecsetpl	\vector, \position, \obj
.endm

.macro vcstimi vector, position, obj
	vecsetmi	\vector, \position, \obj
.endm

.macro vcstihi vector, position, obj
	vecsethi	\vector, \position, \obj
.endm


/*------------------------------------------------------------------------------
	bytevectors
------------------------------------------------------------------------------*/

.macro	mkvu841 upd, dest
	@ dest <- #vu8(space-for-4-items)
	bic	\upd, fre, #0x03	@ upd <- reserve memory
	set	rvc, 0x0400
	orr	rvc, rvc, #bytevector_tag
	stmia	\upd!, {rvc, lnk}	@ fre <- addr of next free cell
	sub	\dest, \upd, #4		@ \dest <- address of cons cell, [*commit destination*]
	orr	fre, \upd, #0x02	@ de-reserve memory, [*restart critical instruction*]
.endm

.macro	mkvu84 dest
	@ dest <- #vu8(space-for-4-items)
  .ifdef enable_MPU
  	mkvu841	fre, \dest		@ reserve, store, commit and de-reserve
  .else
	memsetlnk			@ lnk <- memory transaction return adrs (adj for T2 mode)
	memfrchk8			@ reserve memory, sufficient space available?
	bls	alogc8			@	if not,  jump to perform gc
  	mkvu841	rva, \dest		@ store, commit and de-reserve
  .endif
.endm


.macro vu8aloc dest, size
	@
	@ dest <- (make-bytevector size)
	@
	@ dest and size must be different registers and not rva, rvb
	@
	@ align the number of bytes to allocate
	int2raw	rvb, \size		@ rvb <- #bytes to allocate for data
	@ allocate the aligned object
	bl	adr__alo		@ rva <- adrs of obj (smbl-tag), fre <- adrs (rsrvd lvl 1)
	add	rva, rva, rvb		@ rva <- address of next free cell
	sub	\dest, rva, rvb		@ \dest <- adrs of strng (symbl), [*commit string dest*]
	orr	fre, rva, #0x02		@ de-reserve free-pointer, [*restart critical instr*]
	@ update the object's tag for actual size and type (bytevector)
	lsl	rva, \size, #6
	orr	rva, rva, #bytevector_tag
	str	rva, [\dest, #-4]	@ update tag
.endm

.macro vu8len dest, vu8
	@ on entry:	vu8  (reg) <- bytevector
	@ on exit:	dest (reg) <- bytevector length (scheme int)
	@ modifies:	rvb
	ldr	rvb, [\vu8, #-4]
	lsr	\dest, rvb, #6
.endm

.macro vu8ref octet, vu8, position
	@ on entry:	vu8      (reg) <- bytevector
	@ on entry:	position (reg) <- index (scheme int)
	@ on exit:	octet    (reg) <- item from bytevector (scheme int)
	@ modifies:	rva
.ifndef	cortex
	ldrb	rva, [\vu8,  \position, ASR #2]
.else
	asr	rva, \position, #2
	ldrb	rva, [\vu8, rva]
.endif
	lsl	rva, rva, #2
	orr	\octet, rva, #i0
.endm

.macro vu8set vu8, position, octet
	@ on entry:	vu8      (reg) <- bytevector
	@ on entry:	position (reg) <- index (scheme int)
	@ on entry:	octet    (reg) <- item to store in bytevector (scheme int)
	@ modifies rva, rvc
	int2raw	rva, \octet
  .ifndef cortex
	strb	rva, [\vu8, \position, ASR #2]
  .else
	int2raw	rvc, \position
	strb	rva, [\vu8, rvc]
  .endif
.endm

/*------------------------------------------------------------------------------
	word (table) and byte references
------------------------------------------------------------------------------*/

.macro tbrfi reg, table, position
	ldr	\reg,  [\table,  #4*\position]	
.endm

.macro tbrfieq reg, table, position
	ldreq	\reg,  [\table,  #4*\position]	
.endm

.macro tbrfine reg, table, position
	ldrne	\reg,  [\table,  #4*\position]	
.endm

.macro tbrfimi reg, table, position
	ldrmi	\reg,  [\table,  #4*\position]	
.endm

.macro tbsti reg, table, position
	str	\reg,  [\table,  #4*\position]	
.endm

.macro tbstieq reg, table, position
	streq	\reg,  [\table,  #4*\position]	
.endm

.macro tbstine reg, table, position
	strne	\reg,  [\table,  #4*\position]	
.endm

.macro bytref reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrb	\reg,  [\array,  \position, ASR #2 ]
.else
	asr	\reg, \position, #2
	ldrb	\reg,  [\array, \reg]
.endif
.endm

.macro bytrefeq reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrbeq	\reg,  [\array,  \position, ASR #2 ]
.else
	asreq	\reg, \position, #2
	it	eq
	ldrbeq	\reg,  [\array, \reg]
.endif
.endm

.macro bytrefne reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrbne	\reg,  [\array,  \position, ASR #2 ]
.else
	asrne	\reg, \position, #2
	it	ne
	ldrbne	\reg,  [\array, \reg]
.endif
.endm

.macro bytrefmi reg, array, position	@ the 3 registers should be different
.ifndef	cortex
	ldrbmi	\reg,  [\array,  \position, ASR #2 ]
.else
	asrmi	\reg, \position, #2
	it	mi
	ldrbmi	\reg,  [\array, \reg]
.endif
.endm

.macro bytset array, position, reg	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strb	\reg,  [\array,  \position, ASR #2 ]
  .else
	asr	rvc, \position, #2
	strb	\reg, [\array, rvc]
  .endif
.endm

.macro bytseteq array, position, reg	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strbeq	\reg,  [\array,  \position, ASR #2 ]
  .else
	asreq	rvc, \position, #2
	it	eq
	strbeq	\reg, [\array, rvc]
  .endif
.endm

.macro bytsetne array, position, reg	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strbne	\reg,  [\array,  \position, ASR #2 ]
  .else
	asrne	rvc, \position, #2
	it	ne
	strbne	\reg, [\array, rvc]
  .endif
.endm

.macro bytsetmi array, position, reg	@ the 3 registers should be different
	@ modifies rvc on cortex-m3
  .ifndef cortex
	strbmi	\reg,  [\array,  \position, ASR #2 ]
  .else
	asrmi	rvc, \position, #2
	it	mi
	strbmi	\reg, [\array, rvc]
  .endif
.endm

.macro bytsetu array, position, reg	@ the 3 registers should be different
	@ used only within irq-disabled code zones
	@ or if position is in rva-rvc
	@ (does not disable/enable interrupts on cortex)
.ifndef	cortex
	strb	\reg,  [\array,  \position, ASR #2 ]
.else
	asr	\position, \position, #2	@ not gc safe !! (needs no irq or pos in rva-rvc)
	strb	\reg,  [\array, \position]	@ not gc safe !! (needs no irq or pos in rva-rvc)
	lsl	\position, \position, #2	@ not gc safe !! (needs no irq or pos in rva-rvc)
	orr	\position, \position, #int_tag	@ not gc safe !! (needs no irq or pos in rva-rvc)
.endif
.endm

.macro wrdref reg, array, position	
	@ the 3 registers should be different
	@ reg (first reg) should be rva-rvc (raw value obtained)
  .ifndef cortex
	ldr	\reg,  [\array,  \position, ASR #2 ]
  .else
	asr	\reg, \position, #2	@ reg could be not gc safe here (if not rva or rvb)
	ldr	\reg,  [\array, \reg]
  .endif
.endm

.macro wrdst array, position, reg
	@ array and position should be different registers,
	@ array should not be rvc
	@ modifies rvc on cortex-m3
  .ifndef cortex
	str	\reg,  [\array,  \position, ASR #2 ]
  .else
	asr	rvc, \position, #2
	str	\reg,  [\array, rvc]
  .endif
.endm


/*------------------------------------------------------------------------------
	Addendum: Pair splitting
------------------------------------------------------------------------------*/

.macro snoc car, cdr, pair
	ldmia	\pair, {\car, \cdr}
.endm

.macro snoceq car, cdr, pair
	ldmiaeq	\pair, {\car, \cdr}
.endm

.macro snocne car, cdr, pair
	ldmiane	\pair, {\car, \cdr}
.endm

.macro snocpl car, cdr, pair
	ldmiapl	\pair, {\car, \cdr}
.endm

/*------------------------------------------------------------------------------
	Addendum: Type analysis
------------------------------------------------------------------------------*/

.macro fl2cp1 upd, cpx, real, imag
	@ modifies:	sv3, fre, rva, rvb, rvc
	bic	\upd, fre, #0x03	@ fre <- reserve memory
	bic	rvc, \imag, #3
	orr	rvc, rvc, \real, lsr #30
	bic	rvb, \real, #3
	lsl	rvb, rvb, #2
	orr	rvb, rvb, #complex_tag
	stmia	\upd!, {rvb, rvc}
	sub	\cpx, \upd, #4		@ cpx <- address of rational, [*commit destination*]
	orr	fre, \upd, #0x02	@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro flt2cpx cpx, real, imag
	@ modifies:	sv3, fre, rva, rvb, rvc
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
  .ifdef enable_MPU
  	fl2cp1	fre, \cpx, \real, \imag	@ reserve, store, commit and de-reserve
  .else
	memsetlnk			@ lnk <- mem trans return address (adjustd for T2 mode)
	memfrchk8			@ reserve memory, sufficient space available?
	bls	alogc8			@	if not,  jump to perform gc
  	fl2cp1	rva, \cpx, \real, \imag	@ store, commit and de-reserve
  .endif
	orr	lnk, sv3, #lnkbit0
.endm

.macro in2ra1 upd, rat, num, den
	@ modifies:	fre, rva, rvb, rvc
	bic	\upd, fre, #0x03	@ fre <- reserve memory
	bic	rvc, \den, #3
	orr	rvc, rvc, \num, lsr #30
	bic	rvb, \num, #3
	lsl	rvb, rvb, #2
	orr	rvb, rvb, #rational_tag
	stmia	\upd!, {rvb, rvc}
	sub	\rat, \upd, #4		@ rat <- address of rational, [*commit destination*]
	orr	fre, \upd, #0x02	@ de-reserve free-pointer, [*restart critical instruction*]
.endm

.macro int2rat rat, num, den
	@ modifies:	fre, rva, rvb, rvc
	bic	sv3, lnk, #lnkbit0	@ sv3 <- lnk, saved (and made even if Thumb2)
  .ifdef enable_MPU
  	in2ra1	fre, \rat, \num, \den	@ reserve, store, commit and de-reserve
  .else
	memsetlnk			@ lnk <- mem trans return address (adjustd for T2 mode)
	memfrchk8			@ reserve memory, sufficient space available?
	bls	alogc8			@	if not,  jump to perform gc
  	in2ra1	rva, \rat, \num, \den	@ store, commit and de-reserve
  .endif
	orr	lnk, sv3, #lnkbit0
.endm

.macro charp reg
	and	rva, \reg, #0xFF
	eq	rva, #char_tag
.endm

.macro ratcpx reg
	@ raise eq flag if reg contains a rational or complex
	@ uses rva
	and	rva, \reg, #0x07
	eq	rva, #0x04
	itTT	eq
	ldreq	rva, [\reg, #-4]
	andeq	rva, rva, #0x07		@ rva <- two-bit tag of object in num
	eqeq	rva, #0x03		@ is object a rational or complex?
.endm

.macro nmbrp reg
	@ raise eq flag if reg contains a rational, complex, int or float
	@ uses rva
	ratcpx	\reg			@ is object in reg a rat/cpx?
	itT	ne
	andne	rva, \reg,  #0x03	@ 	if not, rva <- two-bit tag of object in reg
	eqne	rva, #int_tag		@ 	if not, is object an integer?
	it	ne
	eqne	rva, #float_tag		@	if not, is object a float?
.endm

.macro varp reg
	@ raise eq flag if reg contains a variable or syntax item
	@ uses rva
	and	rva, \reg, #0xFF
	eq	rva, #variable_tag
.endm

.macro tagdp reg
	@ raise eq flag if reg points to a tagged item (str, vec, proc, cont, ...)
	@ uses rva
	and	rva, \reg, #0x07
	eq	rva, #0x04
	itTT	eq
	ldreq	rva, [\reg, #-4]
	andeq	rva, rva, #0x47
	eqeq	rva, #0x47
.endm

.macro sizdp reg
	@ raise eq flag if reg points to a tagged-sized item (string, symbol, vector, ...)
	@ uses rva
	and	rva, \reg, #0x07
	eq	rva, #0x04
	itTT	eq
	ldreq	rva, [\reg, #-4]
	andeq	rva, rva, #0xCF
	eqeq	rva, #0x4F
.endm

.macro vctrp reg
	@ raise eq flag if reg points to a vector
	@ uses rva
	and	rva, \reg, #0x07
	eq	rva, #0x04
	itT	eq
	ldrbeq	rva, [\reg, #-4]
	eqeq	rva, #vector_tag
.endm

.macro pntrp reg
	@ raise eq flag if reg contains a pointer
	tst	\reg,  #0x03
.endm

.macro pntrpeq reg
	tsteq	\reg,  #0x03
.endm

.macro pntrpne reg
	tstne	\reg,  #0x03
.endm

.macro syntaxp reg
	@ modifies:	rva
	eor	rva, \reg, #(1<<11)	@ invert the syntax bit
	tst	rva, #(1<<11)
	itT	eq
	andeq	rva, rva, #0xff
	eqeq	rva, #proc
.endm

.macro macrop reg
	@ modifies:	rva
	and	rva, \reg, #0x07
	eq	rva, #0x04
	itTT	eq
	ldreq	rva, [\reg, #-4]
	eoreq	rva, rva, #0x0800	@ is tag in reg a macro tag?
	eqeq	rva, #proc
.endm

.macro execp reg
	@ raises eq flag if \reg is normal prim, proc or cont (not direct prim)
	@ modifies:	rva
	and	rva, \reg, #0x07
	eq	rva, #0x04
	itE	eq
	ldreq	rva, [\reg, #-4]
	setne	rva, \reg
	tst	rva, #0x0800		@ exlude syntax and macros
	itT	eq
	andeq	rva, rva, #0xff
	eqeq	rva, #proc
.endm

.macro pairp reg
	tst	\reg, #0x07
.endm

.macro pairpeq reg
	tsteq	\reg, #0x07
.endm

.macro pairpne reg
	tstne	\reg, #0x07
.endm

.macro tuck reg, tmp
	@ on entry:	dts <- (item1 item2 ...)
	@ on exit:	dts <- (item1 reg item2 ...)
	@ on exit:	tmp <- item1
	restor	\tmp			@ tmp <- item1, dts <- (item2 ...)
	save	\tmp, \reg		@ dts <- (item1 reg item2 ...)
.endm

/*------------------------------------------------------------------------------
	EVAL
------------------------------------------------------------------------------*/

.macro	evalsv1
	varp	sv1			@ is object a variable (rva <- 8-bit tag)?
	it	eq
	bleq	sbevlv			@	if so,  sv1 <- value of var, rva <- non-pair
	pairp	rva			@ is object in sv1 (with 8-bit tag in rva) a pair?
	it	eq
	bleq	sbevll			@	if so,  sv1 <- value of list in sv1
.endm

/*------------------------------------------------------------------------------
	Addendum: Calling and branching to scheme functions or labels
------------------------------------------------------------------------------*/

.macro	call label
  .ifndef cortex
	set	cnt, pc			@ cnt <- instruction after next
	b	\label
  .else
	add	cnt, pc, #4		@ cnt <- instruction after next (16-bit instruction)
	b	\label			@ (16 or 32-bit instruction)
	nop				@ <- cnt points here, or at next instr for 32-bit branch
	nop				@ <- cnt points here, or at next instr for 16-bit branch
  .endif	
.endm

.macro	calla reg
  .ifndef cortex
	set	cnt, pc			@ cnt <- instruction after next
	set	pc,  \reg
  .else
	add	cnt, pc, #4		@ cnt <- instruction after next (16-bit instruction)
	set	pc,  \reg		@ (16-bit instruction) (note: *add cnt, pc, #2* is 32 bit)
	nop
	nop				@ <- cnt points here, or at next instruction
  .endif	
.endm

.macro	nullp reg
	eq	\reg, #null
.endm

.macro	nullpeq reg
	eqeq	\reg, #null
.endm

.macro	nullpne reg
	eqne	\reg, #null
.endm

.macro	izerop reg
	eq	\reg, #i0
.endm

.macro	int2raw raw, int
	asr	\raw, \int, #2
.endm

.macro	int2raweq raw, int
	asreq	\raw, \int, #2
.endm

.macro	int2rawne raw, int
	asrne	\raw, \int, #2
.endm

.macro	int2rawmi raw, int
	asrmi	\raw, \int, #2
.endm


.macro	raw2int int, raw  @ <- target (int) and source (raw) must be different regs
	set	\int, int_tag
	orr	\int, \int, \raw, LSL #2
.endm
	
.macro	raw2inteq int, raw
	seteq	\int, int_tag
	orreq	\int, \int, \raw, LSL #2
.endm
	
.macro	raw2intne int, raw
	setne	\int, int_tag
	orrne	\int, \int, \raw, LSL #2
.endm
	
.macro	raw2intmi int, raw
	setmi	\int, int_tag
	it	mi
	orrmi	\int, \int, \raw, LSL #2
.endm
	


.macro	chr2raw raw, chr
	lsr	\raw, \chr, #8
.endm


.macro	raw2chr chr, raw
	set	\chr, char_tag
	orr	\chr, \chr, \raw, LSL #8
.endm
	
.macro	raw2chreq chr, raw
	seteq	\chr, char_tag
	it	eq
	orreq	\chr, \chr, \raw, LSL #8
.endm

/*------------------------------------------------------------------------------
	swap macros
------------------------------------------------------------------------------*/

.macro swap reg1, reg2, temp
	set	\temp,  \reg1
	set	\reg1,  \reg2
	set	\reg2,  \temp
.endm

.macro swapmi reg1, reg2, temp
	setmi	\temp,  \reg1
	itT	mi
	setmi	\reg1,  \reg2
	setmi	\reg2,  \temp
.endm

/* ============================================================================

   macros for writing s-expressions in ..._data.s

============================================================================ */

.macro	CONSn	obj1, obj2, obj3:vararg
  .ifeqs "\obj3", ""
	.word	\obj1, \obj2
  .else
	.word	\obj1, . + 4
	CONSn	\obj2, \obj3
  .endif
.endm

.macro	LISTn	obj:vararg
	CONSn	\obj, null
.endm

/* ----------------------------------------------------- */

.macro	IF test, t_exp, f_exp
	@ (if test t_exp f_exp)
	LISTn	var_if, \test, \t_exp, \f_exp
.endm

.macro	dIF test, t_exp, f_exp
	@ (if test t_exp f_exp)
	LISTn	val_if, \test, \t_exp, \f_exp
.endm

.macro	QUOTE obj
	@ (quote obj)
	LISTn	var_quote, \obj
.endm

.macro	DEFINED obj, env
	@ (defined? obj env)
	LISTn	var_defined, \obj, \env
.endm

.macro	LOOKUP obj, env
	@ (lookup obj env)
	LISTn	var_lookup, \obj, \env
.endm

.macro	NULL obj
	@ (null? obj)
	LISTn	var_null, \obj
.endm

.macro	PAIR obj
	@ (pair? obj)
	LISTn	var_pair, \obj
.endm

.macro	dPAIR obj
	@ (pair? obj)
	LISTn	val_pair, \obj
.endm

.macro	SETCAR_ pair, obj
	@ (set-car! pair obj)
	LISTn	var_setcar, \pair, \obj
.endm

.macro	SETCDR_ pair, obj
	@ (set-cdr! pair obj)
	LISTn	var_setcdr, \pair, \obj
.endm

.macro	CAR_ obj
	@ (car obj)
	LISTn	var_car, \obj
.endm

.macro	CDR_ obj
	LISTn	var_cdr, \obj
.endm

.macro	dCAR_ obj
	@ (car obj)
	LISTn	val_car, \obj
.endm

.macro	dCDR_ obj
	LISTn	val_cdr, \obj
.endm


.macro	CAAR_ lst
	@ (caar lst)
	LISTn	var_caar, \lst
.endm

.macro	CADR_ lst
	@ (cadr lst)
	LISTn	var_cadr, \lst
.endm

.macro	CDAR_ obj
	LISTn	var_cdar, \obj
.endm

.macro	CDDR_ obj
	LISTn	var_cddr, \obj
.endm

.macro	CAAAR_ obj
	LISTn	var_caaar, \obj
.endm

.macro	CAADR_ obj
	LISTn	var_caadr, \obj
.endm

.macro	CADAR_ obj
	LISTn	var_cadar, \obj
.endm

.macro	CADDR_ obj
	LISTn	var_caddr, \obj
.endm

.macro	CDAAR_ obj
	LISTn	var_cdaar, \obj
.endm

.macro	CDADR_ obj
	LISTn	var_cdadr, \obj
.endm

.macro	CDDAR_ obj
	LISTn	var_cddar, \obj
.endm

.macro	CDDDR_ obj
	LISTn	var_cdddr, \obj
.endm

.macro	CONS_ obj1, obj2
	LISTn	var_cons, \obj1, \obj2
.endm

.macro	dCONS_ obj1, obj2
	LISTn	val_cons, \obj1, \obj2
.endm

.macro	APPEND1 lst1, lst2
	LISTn	var_append, \lst1, \lst2
.endm

.macro	MAP fun, obj
	LISTn	var_map, \fun, \obj
.endm

.macro	MEMQ obj, lst
	LISTn	var_memq, \obj, \lst
.endm

.macro	MEMBER obj, lst
	LISTn	var_member, \obj, \lst
.endm

.macro	ASSQ obj, alst
	LISTn	var_assq, \obj, \alst
.endm

.macro	ASSOC obj, alst
	LISTn	var_assoc, \obj, \alst
.endm

.macro	EQ_ obj1, obj2
	LISTn	var_eq, \obj1, \obj2
.endm

.macro	dEQ_ obj1, obj2
	LISTn	val_eq, \obj1, \obj2
.endm

.macro	EQUAL obj1, obj2
	LISTn	var_equal, \obj1, \obj2
.endm

.macro	dEQUAL obj1, obj2
	LISTn	val_equal, \obj1, \obj2
.endm

.macro	THROW obj1, obj2
	LISTn	var_throw, \obj1, \obj2
.endm

/* ----------------------------------------------------- */

.macro	CHAR obj
	@ (char? obj)
	LISTn	var_char, \obj
.endm

.macro	CHAREQ chr1, chr2
	@ (char? obj)
	LISTn	var_chareq, \chr1, \chr2
.endm

.macro	CHARLT chr1, chr2
	@ (char? obj)
	LISTn	var_charlt, \chr1, \chr2
.endm

.macro	CHARALP chr
	@ (char-alphabetic? chr)
	LISTn	var_chralp, \chr
.endm

.macro	CHARNUM chr
	@ (char-numeric? chr)
	LISTn	var_chrnum, \chr
.endm

.macro	WRTCHAR chr, port
	@ (write-char chr port)
	LISTn	var_write_char, \chr, \port
.endm

/* ----------------------------------------------------- */

.macro	SYMBOL obj
	@ (symbol? obj)
	LISTn	var_symbol, \obj
.endm

.macro	ERS obj
	@ (ers? obj)
	LISTn	var_ers, \obj
.endm

.macro	SYM2STR sym
	@ (symbol->string symbol)
	LISTn	var_sym2str, \sym
.endm

/* ----------------------------------------------------- */

.macro	NUMBER obj
	@ (number? obj)
	LISTn	var_number, \obj
.endm

.macro	EXACT obj
	@ (exact? obj)
	LISTn	var_exact, \obj
.endm

.macro	INEXACT obj
	@ (inexact? obj)
	LISTn	var_inexact, \obj
.endm

.macro	INTEGER obj
	@ (integer? obj)
	LISTn	var_integer, \obj
.endm

.macro	REAL_ obj
	@ (real? obj)
	LISTn	var_real, \obj
.endm

.macro	NUM2STR num
	@ (number->string number)
	LISTn	var_num2str, \num
.endm

.macro	STR2NUM str
	LISTn	var_str2num, \str
.endm

.macro	EQNUM num1, num2
	LISTn	var_eqnum, \num1, \num2
.endm

.macro	LTNUM num1, num2
	LISTn	var_ltnum, \num1, \num2
.endm

.macro	GTNUM num1, num2
	LISTn	var_gtnum, \num1, \num2
.endm

.macro	LENUM num1, num2
	LISTn	var_lenum, \num1, \num2
.endm

.macro	GENUM num1, num2
	LISTn	var_genum, \num1, \num2
.endm

.macro	PLUS_ num1, num2
	LISTn	var_plus, \num1, \num2
.endm

.macro	MINUS_ num1, num2
	LISTn	var_minus, \num1, \num2
.endm

/* ----------------------------------------------------- */

.macro	PROCED obj
	@ (procedure? obj)
	LISTn	var_procedure, \obj
.endm

.macro	SYNTAX obj
	@ (syntax? obj)
	LISTn	var_syntax, \obj
.endm

.macro	MACRO_ obj
	@ (macro? obj)
	LISTn	var_macro, \obj
.endm

.macro	LAMBDA1 args, expr
	@ (lambda var(s) expr)
	LISTn	var_lambda, \args, \expr
.endm

.macro	LAMBDAn args, body
	@ (lambda var(s) . body-list)
	CONSn	var_lambda, \args, \expr
.endm

.macro	EVAL_ var, env
	LISTn	var_eval, \var, \env
.endm

.macro	LET1 bnds, expr
	@ (let bnds expr)
	LISTn	var_let, \bnds, \expr
.endm

/* ----------------------------------------------------- */

.macro	VECTOR obj
	@ (vector? obj)
	LISTn	var_vector, \obj
.endm

.macro	VECLEN_ vec
	LISTn	var_vector_len, \vec
.endm

.macro	MAKVEC size, init
	LISTn	var_make_vec, \size, \init
.endm

.macro	VECREF_ vec, k
	LISTn	var_vector_ref, \vec, \k
.endm

.macro	VECSET_ vec, k, obj
	LISTn	var_vector_set, \vec, \k, \obj
.endm

/* ----------------------------------------------------- */

.macro	STRING obj
	@ (string? obj)
	LISTn	var_string, \obj
.endm

.macro	STR2SYM str
	LISTn	var_str2sym, \str
.endm

.macro	STRLEN_ str
	LISTn	var_string_len, \str
.endm

.macro	STRREF_ str, k
	LISTn	var_string_ref, \str, \k
.endm

.macro	STRAPND str1, str2
	LISTn	var_str_append, \str1, \str2
.endm

.macro	SUBSTR_ str, start, end
	LISTn	var_substring, \str, \start, \end
.endm

/* ----------------------------------------------------- */

.macro	BVECTOR obj
	@ (bytevector? obj)
	LISTn	var_bytevector, \obj
.endm

.macro	BVCLEN_ bvec
	LISTn	var_bv_len, \bvec
.endm

.macro	MAKBV size
	LISTn	var_make_bv, \size
.endm

.macro	BWAND obj1, obj2
	LISTn	var_bw_and, \obj1, \obj2
.endm

/* ----------------------------------------------------- */

.macro	AND_ obj1, obj2
	LISTn	var_and, \obj1, \obj2
.endm

.macro	OR_ obj1, obj2
	LISTn	var_or, \obj1, \obj2
.endm

/*------------------------------------------------------------------------------
	Addendum: getting the BUFFER_START address from within ISR
	          including in MP core case
------------------------------------------------------------------------------*/

.macro	getBUF dest, temp
  .ifndef enable_a9_mpcore
	ldr	\dest, =BUFFER_START	@ dest <- start address of buffers
  .else
	mrc	p15,0,\dest,c0,c0,5	@ dest <- Multiproc Affinity reg, MPIDR
	and	\dest, \dest, #3	@ dest <- cpu_ID
	ldr	\temp, =MP_mat		@ temp <- MP address table start
	add	\dest,\temp,\dest,lsl 5	@ dest <- MP table address for cpu_ID[n]
	ldr	\dest, [\dest, #0x10]	@ dest <- BUFFER_START_n
  .endif
.endm

/*------------------------------------------------------------------------------
	Addendum: hardware macros -- pin setting and clearing
------------------------------------------------------------------------------*/


debug_macros = 0
@debug_macros = 1

	/* identifying registers and small immediates */

.macro	check_if_smallimm imm
	.set	item_is_smallimm_flag, 0
	.set	small_imm_mask, 0xff
  .if \imm < 0
	check_if_smallimm_aux -\imm
  .else
	check_if_smallimm_aux \imm
  .endif
  .if debug_macros
    .if item_is_smallimm_flag
	.print "\imm is small imm"
    .else
	.print "\imm is not small imm"
    .endif
  .endif
.endm

.macro	check_if_smallimm_aux imm
  .if \imm == \imm & small_imm_mask
	.set	item_is_smallimm_flag, 1
  .else
    .ifeq small_imm_mask == 0xff000000
	.set	small_imm_mask, (small_imm_mask << 2)
	check_if_smallimm_aux \imm
    .endif
  .endif
.endm

.macro	check_if_member item, arg1, args:vararg
	.set	item_is_member_flag, 0
   .ifeqs "\item","\arg1"
	.set	item_is_member_flag, 1
  .else
    .ifnb \args
	check_if_member \item, \args
    .endif
  .endif
.endm

.macro check_if_reg item
	.set	item_is_reg_flag, 0
	check_if_member \item, fre,cnt,rva,rvb,rvc,sv1,sv2,sv3,sv4,sv5,env,dts,glv,lnk,pc
	.set	item_is_reg_flag, item_is_member_flag
  .if debug_macros
    .if item_is_reg_flag
	.print "\item is reg"
    .else
	.print "\item is not reg"
    .endif
  .endif
.endm

	/* wait for countdown */

.macro	wait	arg1, arg2:vararg
  .ifb \arg2
	set	rvb, \arg1
100:	subs	rvb, rvb, #1
	bne	100b
  .else
	set	\arg1, \arg2
100:	subs	\arg1, \arg1, #1
	bne	100b
  .endif
.endm

	/* wait for bit in register */

.macro	rgwfbt	reg, ofst, pos, val, temp:vararg
	rgwfbt_aux ldr,  \reg, \ofst, \pos, \val, \temp
.endm

.macro	r8wfbt	reg, ofst, pos, val, temp:vararg
	rgwfbt_aux ldrb, \reg, \ofst, \pos, \val, \temp
.endm

.macro	r16wfbt	reg, ofst, pos, val, temp:vararg
	rgwfbt_aux ldrh, \reg, \ofst, \pos, \val, \temp
.endm

.macro	rgwfbt_aux rfun, reg, ofst, pos, val, temp:vararg
	check_if_reg \reg
	.ifeq item_is_reg_flag
	  ldr	rva, =\reg
	.endif
100:	@ wait loop
	.ifb \temp
	  .if item_is_reg_flag
	    \rfun rvb, [\reg, \ofst]
	  .else
	    \rfun rvb, [rva, \ofst]
	  .endif
	  tst	rvb, #(1 << \pos)
	.else
	  .if item_is_reg_flag
	    \rfun \temp, [\reg, \ofst]
	  .else
	    \rfun \temp, [rva, \ofst]
	  .endif
	  tst	\temp, #(1 << \pos)
	.endif
	.if \val
	  beq	100b
	.else
	  bne	100b
	.endif
        .if debug_macros
	  .if \val
	    .print "waiting for bit == 1"
	  .else
	    .print "waiting for bit == 0"
	  .endif
	.endif
.endm

	/* wait for bit-field in register */

.macro	rgwfbf	reg, ofst, start, end, val
	rgwfbf_aux ldr,  \reg, \ofst, \start, \end, \val
.endm

.macro	r8wfbf	reg, ofst, start, end, val
	rgwfbf_aux ldrh, \reg, \ofst, \start, \end, \val
.endm

.macro	r16wfbf	reg, ofst, start, end, val
	rgwfbf_aux ldrb, \reg, \ofst, \start, \end, \val
.endm

.macro	rgwfbf_aux rfun, reg, ofst, start, end, val
	check_if_reg \reg
	.ifeq item_is_reg_flag
	  ldr	rva, =\reg
	.endif
100:	@  loop
	.if item_is_reg_flag
	  \rfun	rvb, [\reg, \ofst]
	.else
	  \rfun	rvb, [rva, \ofst]
	.endif
	.if (\end-\start) < 9
	  and	rvb, rvb, #((-1<<(\start)) ^ (-1<<(\end)))
	  eq	rvb, #((\val)<<(\start))
	.else
	  ldr	rvc, =((-1<<(\start)) ^ (-1<<(\end)))
	  and	rvb, rvb, rvc
	  check_if_smallimm (\val)
	  .if item_is_smallimm_flag
	    eq	rvb, #((\val)<<(\start))
	  .else
	    ldr	rvc, =((\val)<<(\start))
	    eq	rvb, rvc
	  .endif
	.endif
	bne	100b
.endm

	/* wait for bit-mask in register */

.macro	rgwfbm	reg, ofst, bitmask
	rgwfbm_aux ldr,  \reg, \ofst, \bitmask
.endm

.macro	r8wfbm	reg, ofst, bitmask
	rgwfbm_aux ldrb, \reg, \ofst, \bitmask
.endm

.macro	r16wfbm reg, ofst, bitmask
	rgwfbm_aux ldrh, \reg, \ofst, \bitmask
.endm

.macro	rgwfbm_aux rfun, reg, ofst, bitmask
	check_if_reg \reg
	.ifeq item_is_reg_flag
	  ldr	rva, =\reg
	.endif
100:	@ wait loop
	.if item_is_reg_flag
	  \rfun	rvb, [\reg, \ofst]
	.else
	  \rfun	rvb, [rva, \ofst]
	.endif

	check_if_reg \bitmask
	.if item_is_reg_flag
	  and	rvb, rvb, \bitmask
	  eq	rvb, \bitmask
	.else
	  check_if_smallimm (\bitmask)
	  .if item_is_smallimm_flag
	    and	rvb, rvb, #(\bitmask)
	    eq	rvb, #(\bitmask)
	  .else
	    ldr	rvc, =\bitmask
	    and	rvb, rvb, rvc
	    eq	rvb, rvc
	  .endif
	.endif
	bne	100b
        .if debug_macros
	  .print "waiting for bits == \bitmask"
	.endif
.endm

	/* read from register */

.macro	read	dest, reg, ofst, shift
	read_aux ldr,  \dest, \reg, \ofst, \shift
.endm

.macro	readeq	dest, reg, ofst, shift
	read_aux ldr,  \dest, \reg, \ofst, \shift, eq
.endm

.macro	readne	dest, reg, ofst, shift
	read_aux ldr,  \dest, \reg, \ofst, \shift, ne
.endm

.macro	readmi	dest, reg, ofst, shift
	read_aux ldr,  \dest, \reg, \ofst, \shift, mi
.endm

.macro	read8	dest, reg, ofst, shift
	read_aux ldrb, \dest, \reg, \ofst, \shift
.endm

.macro	read8eq	dest, reg, ofst, shift
	read_aux ldrb, \dest, \reg, \ofst, \shift, eq
.endm

.macro	read8ne	dest, reg, ofst, shift
	read_aux ldrb, \dest, \reg, \ofst, \shift, ne
.endm

.macro	read16	dest, reg, ofst, shift
	read_aux ldrh, \dest, \reg, \ofst, \shift
.endm

.macro	read16eq dest, reg, ofst, shift
	read_aux ldrh, \dest, \reg, \ofst, \shift, eq
.endm

.macro	read16ne dest, reg, ofst, shift
	read_aux ldrh, \dest, \reg, \ofst, \shift, ne
.endm

.macro	read_aux rfun, dest, src, ofst, shift, cond:vararg
	@ in:	rfun	<- read function	(ldr, ldrh or ldrb)
	@ in:	dest	<- destination		(register)
	@ in:	src	<- source		(register or address)
	@ in:	ofst	<- destination offset	(register or immediate)
	check_if_reg \src
	.if item_is_reg_flag
	  .set	src_is_reg_flag, 1
	.else
	  ldr\cond rva, =\src
	  CTX_IT \cond
	  .set	src_is_reg_flag, 0
	.endif
	.if src_is_reg_flag
	  .ifb \shift
	    \rfun\cond \dest, [\src, \ofst]
	  .else
	    \rfun\cond \dest, [\src, \ofst, \shift]
	  .endif
	.else
	  .ifb \shift
	    \rfun\cond \dest, [rva, \ofst]
	  .else
	    \rfun\cond \dest, [rva, \ofst, \shift]
	  .endif
	.endif
.endm


	/* write to register */

.macro	write	val, reg, ofst, shift
	write_aux str,  \val, \reg, \ofst, \shift
.endm

.macro	writeeq	val, reg, ofst, shift
	write_aux str,  \val, \reg, \ofst, \shift, eq
.endm

.macro	writene	val, reg, ofst, shift
	write_aux str,  \val, \reg, \ofst, \shift, ne
.endm

.macro	writepl	val, reg, ofst, shift
	write_aux str,  \val, \reg, \ofst, \shift, pl
.endm

.macro	writemi	val, reg, ofst, shift
	write_aux str,  \val, \reg, \ofst, \shift, mi
.endm

.macro	write8	val, reg, ofst, shift
	write_aux strb, \val, \reg, \ofst, \shift
.endm

.macro	write8eq val, reg, ofst, shift
	write_aux strb, \val, \reg, \ofst, \shift, eq
.endm

.macro	write8ne val, reg, ofst, shift
	write_aux strb, \val, \reg, \ofst, \shift, ne
.endm

.macro	write16	val, reg, ofst, shift
	write_aux strh, \val, \reg, \ofst, \shift
.endm

.macro	write16eq val, reg, ofst, shift
	write_aux strh, \val, \reg, \ofst, \shift, eq
.endm

.macro	write16ne val, reg, ofst, shift
	write_aux strh, \val, \reg, \ofst, \shift, ne
.endm

.macro	write_aux wfun, item, dest, ofst, shift, cond:vararg
	@ in:	wfun	<- write function	(str, strh or strb)
	@ in:	item	<- item to write	(register or immediate)
	@ in:	dest	<- destination		(register or address)
	@ in:	ofst	<- destination offset	(register or immediate)
	check_if_reg \dest
	.if item_is_reg_flag
	  .set	dest_is_reg_flag, 1
	.else
	  ldr\cond rva, =\dest
	  CTX_IT \cond
	  .set	dest_is_reg_flag, 0
	.endif
	check_if_reg \item
	.if item_is_reg_flag
	  .ifb \shift
	    .if dest_is_reg_flag
	      \wfun\cond \item, [\dest, \ofst]
	    .else
	      \wfun\cond \item, [rva, \ofst]
	    .endif
	  .else
	    .if dest_is_reg_flag
	      \wfun\cond \item, [\dest, \ofst, \shift]
	    .else
	      \wfun\cond \item, [rva, \ofst, \shift]
	    .endif
	  .endif
	.else
	  set\cond rvb, \item
	  CTX_IT \cond
	  .ifb \shift
	    .if dest_is_reg_flag
	      \wfun\cond rvb, [\dest, \ofst]
	    .else
	      \wfun\cond rvb, [rva, \ofst]
	    .endif
	  .else
	    .if dest_is_reg_flag
	      \wfun\cond rvb, [\dest, \ofst, \shift]
	    .else
	      \wfun\cond rvb, [rva, \ofst, \shift]
	    .endif
	  .endif
	.endif
.endm


	/* register copy-bit */

.macro	rgcpbt	reg, ofst, pos, val
	rgcpbt_aux ldr,  str,  \reg, \ofst, \pos, \val
.endm

.macro	r8cpbt	reg, ofst, pos, val
	rgcpbt_aux ldrb, strb, \reg, \ofst, \pos, \val
.endm

.macro	r16cpbt	reg, ofst, pos, val
	rgcpbt_aux ldrh, strh, \reg, \ofst, \pos, \val
.endm

.macro	rgcpbt_aux rfun, wfun, reg, ofst, pos, val
	check_if_reg \reg
	.if item_is_reg_flag
	  \rfun	rvb, [\reg, \ofst]
	.else
	  ldr	rva, =\reg
	  \rfun	rvb, [rva, \ofst]
	.endif
	.if \val == 0
	  bic	rvb, rvb, #(1<<(\pos))
	.else
	  orr	rvb, rvb, #(1<<(\pos))
	.endif
	.if item_is_reg_flag
	  \wfun	rvb, [\reg, \ofst]
	.else
	  \wfun	rvb, [rva, \ofst]
	.endif
.endm

	/* register copy-bit-field */

.macro	rgcpbf	reg, ofst, start, end, val
	rgcpbf_aux ldr,  str,  \reg, \ofst, \start, \end, \val
.endm

.macro	r8cpbf	reg, ofst, start, end, val
	rgcpbf_aux ldrb, strb, \reg, \ofst, \start, \end, \val
.endm

.macro	r16cpbf	reg, ofst, start, end, val
	rgcpbf_aux ldrh, strh, \reg, \ofst, \start, \end, \val
.endm

.macro	rgcpbf_aux rfun, wfun, reg, ofst, start, end, val
	check_if_reg \reg
	.if item_is_reg_flag
	  \rfun	rvb, [\reg, \ofst]
	.else
	  ldr	rva, =\reg
	  \rfun	rvb, [rva, \ofst]
	.endif
	.if (\end-\start) < 9
	  bic	rvb, rvb, #((-1<<(\start)) ^ (-1<<(\end)))
	  orr	rvb, rvb, #((\val)<<(\start))
	.else
	  ldr	rvc, =((-1<<(\start)) ^ (-1<<(\end)))
	  bic	rvb, rvb, rvc
	  ldr	rvc, =((\val)<<(\start))
	  orr	rvb, rvb, rvc
	.endif
	.if item_is_reg_flag
	  \wfun	rvb, [\reg, \ofst]
	.else
	  \wfun	rvb, [rva, \ofst]
	.endif
.endm

	/* register bit-field */

.macro	rgbf	dest, reg, arg1, arg2, arg3
	.ifnb \arg3
	  rgbf_aux ldr, \dest, \reg, \arg1, \arg2, \arg3
	.else
	  .ifndef cortex
	    .if (\arg2-\arg1) < 9
	      .if \arg2 < 32
	        and \dest, \reg, #((-1<<(\arg1)) ^ (-1<<(\arg2)))
	        .if \arg1 > 0
	          lsr \dest, \dest, #(\arg1)
	        .endif
	      .else
	        lsr \dest, \reg, #(\arg1)
	      .endif
	    .else
	      .if \arg2 < 32
	        lsl \dest, \reg, #(32-\arg2)
	      .endif
	      lsr \dest, \dest, #(\arg1+32-\arg2)
	    .endif
	  .else
	    ubfx \dest, \reg, #(\arg1), #(\arg2-\arg1)
	  .endif
	.endif
.endm

.macro	r8bf	dest, reg, ofst, start, end
	rgbf_aux ldrb, \dest, \reg, \ofst, \start, \end
.endm

.macro	r16bf	dest, reg, ofst, start, end
	rgbf_aux ldrh, \dest, \reg, \ofst, \start, \end
.endm

.macro	rgbf_aux rfun, dest, reg, ofst, start, end
	check_if_reg \reg
	.if item_is_reg_flag
	  \rfun \dest, [\reg, \ofst]
	.else
	  ldr	rva, =\reg
	  \rfun \dest, [rva, \ofst]
	.endif
	.ifndef cortex
	  .if (\end-\start) < 9
	    .if \end < 32
	      and \dest, \dest, #((-1<<(\start)) ^ (-1<<(\end)))
	    .endif
	    .if \start > 0
	      lsr \dest, \dest, #(\start)
	    .endif
	  .else
	    .if \end < 32
	      lsl \dest, \dest, #(32-\end)
	    .endif
	    lsr \dest, \dest, #(\start+32-\end)
	  .endif
	.else
	  ubfx \dest, \dest, #(\start), #(\end-\start)
	.endif
.endm

	/* register read-modify-write */

.macro	rgrmw	reg, ofst, mask1, mask2:vararg
	rgrmw_aux ldr,  str,  \reg, \ofst, \mask1, \mask2
.endm

.macro	r8rmw	reg, ofst, mask1, mask2:vararg
	rgrmw_aux ldrb, strb, \reg, \ofst, \mask1, \mask2
.endm

.macro	r16rmw	reg, ofst, mask1, mask2:vararg
	rgrmw_aux ldrh, strh, \reg, \ofst, \mask1, \mask2
.endm

.macro	rgrmw_aux rfun, wfun, reg, ofst, mask1, mask2:vararg
	check_if_reg \reg
	.if item_is_reg_flag
	  \rfun	rvb, [\reg, \ofst]
	.else
	  ldr	rva, =\reg
	  \rfun	rvb, [rva, \ofst]
	.endif
	.ifnb \mask2
	    rgrmw_op_aux bic, \mask1
	.endif
	.ifnes "\mask2", "xxx"
	  .ifb \mask2
	    rgrmw_op_aux orr, \mask1
	  .else
	    rgrmw_op_aux orr, \mask2
	  .endif
	.endif
	check_if_reg \reg
	.if item_is_reg_flag
	  \wfun	rvb, [\reg, \ofst]
	.else
	  \wfun	rvb, [rva, \ofst]
	.endif
.endm

.macro	rgrmw_op_aux op, mask
	check_if_reg \mask
	.if item_is_reg_flag
	  \op	rvb, rvb, \mask
	.else
	  check_if_smallimm (\mask)
	  .if item_is_smallimm_flag
	    \op rvb, rvb, #(\mask)
	  .else
	    ldr rvc, =\mask
	    \op rvb, rvb, rvc
	  .endif
	.endif
.endm

	/* build bit-mask */

.macro	mask	dest, logsz, mask, pos:vararg
	maski	\dest, \logsz, \mask, (32/(1<<\logsz)), 0, \pos
.endm

.macro	maski	dst, lgsz, val, pdiv, rslt, pos1, pos:vararg
  .ifeqs "\pos", ""
	ldr	\dst, =\rslt | (\val << ((\pos1 % \pdiv) << \lgsz))
  .else
	maski	\dst,\lgsz,\val,\pdiv,\rslt|(\val<<((\pos1%\pdiv)<<\lgsz)),\pos
  .endif
.endm

	/* set or clear pin */

.macro	apnset gpioadr, pinbitreg
	@
	@  example:	set	rvb, #(1 << 2)	@ for pin 2
	@		apnset	LEDIO, rvb	@ set pin 2 high on LEDIO
	@ modifies:	rva, input-reg (eg. rvb)
	@
  .ifdef stellaris_gpio
	ldr	rva, =\gpioadr
	add	rva, rva, \pinbitreg, lsl #2
	set	\pinbitreg, 0xff
	str     \pinbitreg, [rva]
  .else
    .ifdef has_combined_set_clear
	ldr	rva, =\gpioadr
	ldr     rva, [rva, #io_set]
	orr	\pinbitreg, \pinbitreg, rva
    .endif
	ldr	rva, =\gpioadr
	str     \pinbitreg, [rva, #io_set]
  .endif
.endm


.macro	apnclr gpioadr, pinbitreg
	@
	@  example:	set	rvb, #(1 << 2)	@ for pin 2
	@		apnclr	LEDIO, rvb	@ set pin 2 low on LEDIO
	@ modifies:	rva, input-reg (eg. rvb)
	@
  .ifdef stellaris_gpio
	ldr	rva, =\gpioadr
	add	rva, rva, \pinbitreg, lsl #2
	set	\pinbitreg, 0x00
	str     \pinbitreg, [rva]
  .else
    .ifdef has_combined_set_clear
	ldr	rva, =\gpioadr
	ldr     rva, [rva, #io_clear]
	bic	\pinbitreg, \pinbitreg, rva
    .endif
	ldr	rva, =\gpioadr
	str     \pinbitreg, [rva, #io_clear]
  .endif
.endm



