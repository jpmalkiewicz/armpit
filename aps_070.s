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

/*==============================================================================
@
@ Contributions:
@
@     This file includes contributions by Robbie Dinn, marked <RDC>
@     This file includes contributions by Petr Cermak, marked <PCC>
@
@=============================================================================*/

@-------------------------------------------------------------------------------
@  Select MCU-ID (for variables and i2c) (use even address, min=2, max=254)
@-------------------------------------------------------------------------------

@mcu_id		= 2			@ set i2c address of   1 (i.e.   2 / 2)
mcu_id		= 200			@ set i2c address of 100 (i.e. 200 / 2)
@mcu_id		= 254			@ set i2c address of 127 (i.e. 254 / 2)

@-------------------------------------------------------------------------------
@  Specify board- and device-specific constants and macros
@-------------------------------------------------------------------------------

.include "board.h"			@ Parameters Specific to each Board
.include "device_family.h"		@ HARDWARE CONSTANTS for each MCU Family

  @-------------------------------------------------------------------------------
  @  Common assembly options
  @-------------------------------------------------------------------------------

.ifndef	stop_and_copy
  mark_and_sweep	= 1		@ use mark-and-sweep garbage collection
.endif
  oba_above_heap	= 1		@ store obarray above the heap
  include_system0	= 1		@ include system-0 (gpio bindings ...)
 
.ifndef	small_memory

  @-------------------------------------------------------------------------------
  @  Assembly options for most MCUs
  @  Check that .bin file is below 64 KB after re-assembly (approx. 60KB
  @  for live_SD versions of TI_Beagle(_XM) and GMX_OVERO_TIDE).
  @-------------------------------------------------------------------------------

  .ifndef exclude_r6rs_fx
    include_r6rs_fx	= 1		@ include R6RS fx+, fx-, ...
  .endif
  fast_eval_apply 	= 1		@ use faster eval/apply for 1-3 var/args
  .ifndef nrml_lambda_lkp
    fast_lambda_lkp 	= 1		@ use fast var lookup inside closures
  .endif

.else

  @-----------------------------------------------------------------------------
  @  Assembly options for small memory MCUs:	NXP 2131, 2103, 1343
  @  Check that .bin file is below 24 KB after re-assembly.
  @-----------------------------------------------------------------------------
  CORE_ONLY		= 1		@ remove armpit_scheme_library.s
  integer_only		= 1		@ remove float, complex, rationals
  r3rs			= 1		@ restrict language to R3RS scheme

.endif

@-------------------------------------------------------------------------------
@  Additional assembly options for most MCUs
@  For regular MCUs: uncomment if desired.
@  Note: these options are not used in distribution but could be useful.
@  Check that .bin file is below 24KB/60KB/64KB after re-assembly.
@-------------------------------------------------------------------------------

@inline_cons	= 1	@ uncomment to use inlined cons and save functions
@top_level_btree	= 1	@ uncomment to use a btree for top-level env
@include_i2c	= 1	@ uncomment to include the I2C subsystem (if available)
@exclude_read_write = 1	@ uncomment to exclude read (parse...), write, display
	
@-------------------------------------------------------------------------------
@
@   PROGRAM CODE:
@
@	Assembler Constants
@	Assembler Macros
@	Reset and Initialization code
@	MCU-Dependent Initialization and I/O Routines
@	Turning LEDs on/off
@	Scheme environment, functions and ports
@	Startup Code for LPC_2800, EP_93xx, S3C24xx, OMAP_35xx -- .data section
@
@-------------------------------------------------------------------------------
	
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
@ check compatibility of selected options (minimal)
@-----------------------------------------------------------------------------*/

echo_options
check_options

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
@ MP Core parameters table (in data section)
@-----------------------------------------------------------------------------*/

.ifdef	enable_a9_mpcore

MP_mat:
	.word	RAMTOP,		heapbottom,	heaptop0,	heaptop1
	.word	BUFFER_START, 	READBUFFER, 	WRITEBUFFER,	I2C0ADR
  .if MP_numcores > 1
	.word	RAMTOP_1,	heapbottom_1,	heaptop0_1,	heaptop1_1
	.word	BUFFER_START_1,	READBUFFER_1,	WRITEBUFFER_1,	I2C0ADR_1
  .endif
  .if MP_numcores > 2
	.word	RAMTOP_2,	heapbottom_2,	heaptop0_2,	heaptop1_2
	.word	BUFFER_START_2,	READBUFFER_2,	WRITEBUFFER_2,	I2C0ADR_2
  .endif
  .if MP_numcores > 3
	.word	RAMTOP_3,	heapbottom_3,	heaptop0_3,	heaptop1_3
	.word	BUFFER_START_3,	READBUFFER_3,	WRITEBUFFER_3,	I2C0ADR_3
  .endif

.endif

/*------------------------------------------------------------------------------
@ ISR vector
@-----------------------------------------------------------------------------*/

	ISRVECTOR

/*------------------------------------------------------------------------------
@ Reset and Initialization (in text section)
@-----------------------------------------------------------------------------*/

.text

.ifndef	cortex
  .include "reset_ARM.s"		@ int vects+resets for arm7tdmi,cortexa8
.else
  .include "reset_CM3.s"		@ int vects+resets for cortex-m3/-m4
.endif
.include "scheme_init.s"		@ scheme init code, init glv, enab int


/*------------------------------------------------------------------------------
@ Scheme code
@-----------------------------------------------------------------------------*/

.data	0

.balign 8

stkbtm:	.word	null
	.word	stkbtm

VCTR	empty_vector			@ armpit_as_constants.s, absent sub-envs


	/* --------------------------------------------------------------------\
	| 		pre-entry function table for primitives		       |
	\-------------------------------------------------------------------- */

.ifdef integer_only
	adr_unijpe = adr__err
	adr_numgto = adr__err
.endif
.ifdef CORE_ONLY
	adr_mmglen = adr__err
	adr_cxxxxr = adr__err
.endif
.ifndef CORE_ONLY
  .ifdef lib_funcs_in_scm
	adr_mmglen = adr__err
	adr_cxxxxr = adr__err
  .endif
.endif
.ifndef include_r6rs_fx
	adr_fxchk2 = adr__err
.endif

	/* pre-entry function table */
	startBVU8	paptbl
	@ pre-entry functions
	PAPTBWORD	0x00		@  0 dummy (offst 0 = no pre-ntry in apply)
	PAPTBWORD	typchk		@  1 scheme_core.s
	PAPTBWORD	return		@  2 scheme_core.s
	PAPTBWORD	ioprfn		@  3 io_ports/ports.s
	PAPTBWORD	prdnml		@  4 scheme_base_6.2.Integers.s & Numbers.s
	PAPTBWORD	rdcnml		@  5 scheme_base_6.2.Integers.s & Numbers.s
	PAPTBWORD	unijpe		@  6 scheme_base_6.2.Numbers.s
	PAPTBWORD	numgto		@  7 scheme_base_6.2.Numbers.s
	PAPTBWORD	mmglen		@  8 scheme_base_6.2.Numbers.s
	PAPTBWORD	cxxxxr		@  9 scheme_library.s
	PAPTBWORD	vu8ren		@ 10 scheme_r6rs_library.s
	PAPTBWORD	bwloop		@ 11 scheme_r6rs_library.s
	PAPTBWORD	bwfent		@ 12 scheme_r6rs_library.s
	PAPTBWORD	regent		@ 13 scheme_r6rs_library.s
	PAPTBWORD	fxchk2		@ 14 scheme_r6rs_library.s
	@ additional linkages for loadable numbers.s
	PAPTBWORD	numstr		@ 15 scheme_base_6.2.Integers.s & Numbers.s
	PAPTBWORD	strnum		@ 16 scheme_base_6.2.Integers.s & Numbers.s
	PAPTBWORD	_alo		@ 17 scheme_core.s
	PAPTBWORD	_gc		@ 18 scheme_core.s
	PAPTBWORD	_err		@ 19 scheme_core.s
	ENDsized

	/* --------------------------------------------------------------------\
	|		built-in environment				       |
	\-------------------------------------------------------------------- */

.ifdef exclude_read_write
	oba_read_write = empty_vector
	env_read_write = empty_vector
.endif
.ifdef CORE_ONLY
  .ifndef include_small_lib
	oba_library = empty_vector
	env_library = empty_vector
  .endif
.endif
.ifndef include_system0
	oba_system_0 = empty_vector
	env_system_0 = empty_vector
.endif

	STARTOBAENV	scheme
	OBAENVWORD	core		@ 1
	OBAENVWORD	base		@ 2
	OBAENVWORD	numbers		@ 3
	OBAENVWORD	ports		@ 4
	OBAENVWORD	read_write	@ 5
	OBAENVWORD	library		@ 6
	OBAENVWORD	r6rs_library	@ 7
	OBAENVWORD	system_0	@ 8
	ENDOBAENV

	coreoba = ((var_suffix_core >> 8) << 2) + oba_scheme

	/* --------------------------------------------------------------------\
	|		post-initialization code			       |
	\-------------------------------------------------------------------- */

.include "ports.s"

.include "scheme_core.s"
.include "scheme_base.s"

.ifdef	integer_only
  .include "scheme_base_6.2.Integers.s"
.else
  .include "scheme_base_6.2.Numbers.s"
.endif

.ifndef	exclude_read_write
  .ifndef read_write_funcs_in_scheme
    .include "scheme_read_write.s"
  .else
    .include "scheme_read_write_S.s"
  .endif
.endif

.ifndef	CORE_ONLY
  .ifndef lib_funcs_in_scm
    .include "scheme_library.s"
  .else
    .include "scheme_library_S.s"
  .endif
.else
  .ifdef include_small_lib
    .include "scheme_small_library.s"
  .endif
.endif

.include "scheme_r6rs_library.s"

.ifdef include_system0
  .include "system_0.s"
.endif

.text

.ifdef FRDM_K64F
  .balign 1024		@ align the end_of_code tag for Freescale
.endif

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


/*------------------------------------------------------------------------------
@ boot section (if MCU needs separate startup)
@-----------------------------------------------------------------------------*/

.section boot_section, "ax"

.ifdef	include_startup
  .include "startup.s"		@ code for LPC_2800, EP_93xx, S3C24xx, OMAP_35xx
.endif


.end



