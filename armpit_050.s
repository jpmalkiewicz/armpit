@---------------------------------------------------------------------------------------------------------
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
@---------------------------------------------------------------------------------------------------------

@=========================================================================================================
@
@ Contributions:
@
@     This file includes contributions by Robbie Dinn, marked <RDC>
@
@=========================================================================================================

@---------------------------------------------------------------------------------------------------------
@
@   Configure Assembly Parameters:
@
@   1- Select Board
@   2- Select MCU-ID (for variables and i2c)
@   3- Select assembly options for most MCUs
@   4- Select additional assembly options for most MCUs
@   5- Select assembly options for small memory MCUs:	NXP 2131, 2104, 1343
@   6- Select additional assembly options for any MCU
@
@---------------------------------------------------------------------------------------------------------

@--------------------------------------------------------------------------------
@  1 - Select Board:
@--------------------------------------------------------------------------------
@
@ --- ARM7TDMI ---
@
@TINY_2106	= 1	@ uncomment for NewMicros	Tiny-2106	board (lpc2000)
@TINY_2131	= 1	@ uncomment for NewMicros	Tiny-2131	board (lpc2000)
@TINY_2138	= 1	@ uncomment for NewMicros	Tiny-2138	board (lpc2000)
@SFE_Logomatic1	= 1	@ uncomment for SparkFun	Logomatic V1	board (lpc2000)
@SFE_Logomatic2	= 1	@ uncomment for SparkFun	Logomatic V2	board (lpc2000)
@LPC_H2103	= 1	@ uncomment for Olimex		LPC-H2103	board (lpc2000)
@LPC_H2148	= 1	@ uncomment for Olimex		LPC-H2148	board (lpc2000)
@LCDDemo_2158	= 1	@ uncomment for FutureDesign	LCDDemo-2158	board (lpc2000)
@LPC_H2214	= 1	@ uncomment for Olimex		LPC-H2214	board (lpc2000)
@LPC_H2294	= 1	@ uncomment for Olimex		LPC-H2294	board (lpc2000)
@LPC2478_STK	= 1	@ uncomment for Olimex		LPC2478-STK	board (lpc2000)
@LPC_H2888	= 1	@ uncomment for Olimex		LPC-H2888	board (lpc2800)
@SAM7_H256	= 1	@ uncomment for Olimex		SAM7-H256	board (at91sam7s)
@SAM7_P256	= 1	@ uncomment for Olimex		SAM7-P256	board (at91sam7s)	<RDC>
@SAM7_1184	= 1	@ uncomment for MicroBus	118404		board (at91sam7x)	<RDC>
@STR_H711	= 1	@ uncomment for Olimex		STR-H711	board (str7xx)
@
@ --- ARM9TDMI ---
@
@STR91X_M	= 1	@ uncomment for InSem-Lodin  	STR91X-M	board (str9xx)
@CS_E9302	= 1	@ uncomment for Olimex		CS-EP9302	board (ep93xx)
@TCT_Hammer	= 1	@ uncomment for TinCanTools	Hammer		board (s3c2410)
@SAM9_L9261	= 1	@ uncomment for Olimex		SAM9-L9261	board (at91sam9261)
@
@ --- Cortex-M3 ---
@
@LPC_P1343	= 1	@ uncomment for Olimex		LPC-P1343	board (lpc1300)
@Blueboard_1768	= 1	@ uncomment for ngxelectronics	Blueboard-1768H	board (lpc1700)
@SAM3_H256	= 1	@ uncomment for Olimex		SAM3-H256	board (at91sam3s)
@STM32_DT_Board	= 1	@ uncomment for InSem-Lodin	STM32-DOT-BOARD	board (stm32x)
@STM32_H103	= 1	@ uncomment for Olimex		STM32-H103	board (stm32x)
@STM32_H107	= 1	@ uncomment for Olimex		STM32-H107	board (stm32x)
@STM32_LCD	= 1	@ uncomment for Olimex		STM32-LCD	board (stm32x)
@EVB_LM3S1968	= 1	@ uncomment for TI-LuminaryM	LM3S1968-EVB	board (lm3s1000)
@IDM_LM3S1958	= 1	@ uncomment for TI-LuminaryM	IDM-L35		board (lm3s1000)
@EVB_LM3S6965	= 1	@ uncomment for TI-LuminaryM	LM3S6965-EVB	board (lm3s1000)
@TI_EvalBot	= 1	@ uncomment for TI-LuminaryM	EvalbotLM3S9B92	board (lm3s1000)
@
@ --- Cortex-A8 ---
@
@TI_Beagle	= 1	@ uncomment for TI		Beagle rev B7	board (omap35xx)
@TI_Beagle_XM	= 1	@ uncomment for TI		Beagle XM rev C	board (dm37xx / omap36xx)
GMX_OVERO_TIDE	= 1	@ uncomment for Gumstix		Overo Tide	board (omap35xx)


@--------------------------------------------------------------------------------
@  2- Select MCU-ID (for variables and i2c) (use even address, min=2, max=254)
@--------------------------------------------------------------------------------

@mcu_id		= 2	@ uncomment to get i2c address of   1 (i.e.   2 / 2)
mcu_id		= 200	@ uncomment to get i2c address of 100 (i.e. 200 / 2)

@--------------------------------------------------------------------------------
@  3- Assembly options for most MCUs
@	For regular MCUs: uncomment these and comment-out options in 5- below.
@	Except:	for Beagle and Overo, do comment out include_i2c (for size).
@	-- check that .bin file is below 64 KB after re-assembly (or approx. 60KB
@	   for live_SD versions of TI_Beagle(_XM) and GMX_OVERO_TIDE).
@--------------------------------------------------------------------------------

mark_and_sweep	= 1	@ uncomment to use mark-and-sweep garbage collection
oba_above_heap	= 1	@ uncomment to store obarray above the heap
include_r6rs_fx	= 1	@ uncomment to include R6RS fx+, fx-, ...
include_system0	= 1	@ uncomment to include system-0 (gpio bindings ...)
@include_i2c	= 1	@ uncomment to include the I2C subsystem (if available)

@--------------------------------------------------------------------------------
@  4- Additional assembly options for most MCUs
@	For regular MCUs: uncomment if desired.
@	Note:	these options are not used in distribution but could be useful.
@	-- check that .bin file is below 64 KB after re-assembly.
@--------------------------------------------------------------------------------

@inline_cons	= 1	@ uncomment to use inlined cons and save functions
@top_level_btree	= 1	@ uncomment to use a btree for top-level env

@--------------------------------------------------------------------------------
@  5- Assembly options for small memory MCUs:	NXP 2131, 2103, 1343
@	For small memory MCUs: uncomment these and comment-out opts in 3- above.
@	-- check that .bin file is below 24 KB after re-assembly.
@--------------------------------------------------------------------------------

@mark_and_sweep	= 1	@ uncomment to use mark-and-sweep garbage collection
@CORE_ONLY	= 1	@ uncomment to remove armpit_scheme_library.s
@integer_only	= 1	@ uncomment to remove float, complex and rational support
@small_eval_apply = 1	@ uncomment to use smaller evaluator
@exclude_pack	= 1	@ uncomment to exclude the pack function
@r3rs		= 1	@ uncomment to restrict language to R3RS scheme
@include_system0	= 1	@ uncomment to include system-0 (gpio bindings ...)

@--------------------------------------------------------------------------------
@  6- Additional assembly options for any MCU
@	For any MCU: uncomment if desired.
@	Note: the system is not useful when the option below is selected
@	(removes the rep loop - a new internal startup program is then needed).	
@--------------------------------------------------------------------------------

@exclude_read_write = 1	@ uncomment to exclude read (parse...), write and display
	
@--------------------------------------------------------------------------------
@
@  0.A.	  Initialization and Operation Parameters Specific to each Board / MCU
@
@--------------------------------------------------------------------------------

.ifdef TINY_2106
  .include "mcu_specific/LPC_2000/TINY_2106.h"
.endif
.ifdef TINY_2131
  .include "mcu_specific/LPC_2000/TINY_2131.h"
.endif
.ifdef TINY_2138
  .include "mcu_specific/LPC_2000/TINY_2138.h"
.endif
.ifdef SFE_Logomatic1
  .include "mcu_specific/LPC_2000/SFE_Logomatic1.h"
.endif
.ifdef SFE_Logomatic2
  .include "mcu_specific/LPC_2000/SFE_Logomatic2.h"
.endif
.ifdef LPC_H2103
  .include "mcu_specific/LPC_2000/LPC_H2103.h"
.endif
.ifdef LPC_H2148
  .include "mcu_specific/LPC_2000/LPC_H2148.h"
.endif
.ifdef LCDDemo_2158
  .include "mcu_specific/LPC_2000/LCDDemo_2158.h"
.endif
.ifdef LPC_H2214
  .include "mcu_specific/LPC_2000/LPC_H2214.h"
.endif
.ifdef LPC_H2294
  .include "mcu_specific/LPC_2000/LPC_H2294.h"
.endif
.ifdef LPC2478_STK
  .include "mcu_specific/LPC_2000/LPC2478_STK.h"
.endif
.ifdef LPC_H2888
  .include "mcu_specific/LPC_2800/LPC_H2888.h"
.endif
.ifdef LPC_P1343
  .include "mcu_specific/LPC_1300/LPC_P1343.h"
.endif
.ifdef Blueboard_1768
  .include "mcu_specific/LPC_1700/Blueboard_1768.h"
.endif
.ifdef SAM7_H256
  .include "mcu_specific/AT91_SAM7/SAM7_H256.h"
.endif
.ifdef SAM7_P256
  .include "mcu_specific/AT91_SAM7/SAM7_P256.h"		@ 				       	<RDC>
.endif
.ifdef SAM7_1184
  .include "mcu_specific/AT91_SAM7/SAM7_1184.h"		@ 				       	<RDC>
.endif
.ifdef SAM3_H256
  .include "mcu_specific/AT91_SAM3S/SAM3_H256.h"
.endif
.ifdef SAM9_L9261
  .include "mcu_specific/AT91_SAM9/SAM9_L9261.h"
.endif
.ifdef STR_H711
  .include "mcu_specific/STR_7xx/STR_H711.h"
.endif
.ifdef STR91X_M
  .include "mcu_specific/STR_9xx/STR91X_M.h"
.endif
.ifdef STM32_H103
  .include "mcu_specific/STM32x/STM32_H103.h"
.endif
.ifdef STM32_H107
  .include "mcu_specific/STM32x/STM32_H107.h"
.endif
.ifdef STM32_DT_Board
  .include "mcu_specific/STM32x/STM32_DT_Board.h"
.endif
.ifdef STM32_LCD
  .include "mcu_specific/STM32x/STM32_LCD.h"
.endif
.ifdef CS_E9302
  .include "mcu_specific/EP_93xx/CS_E9302.h"
.endif
.ifdef TCT_Hammer
  .include "mcu_specific/S3C24xx/TCT_Hammer.h"
.endif
.ifdef EVB_LM3S1968
  .include "mcu_specific/LM_3S1000/EVB_LM3S1968.h"
.endif
.ifdef EVB_LM3S6965
  .include "mcu_specific/LM_3S1000/EVB_LM3S6965.h"
.endif
.ifdef IDM_LM3S1958
  .include "mcu_specific/LM_3S1000/IDM_LM3S1958.h"
.endif
.ifdef TI_EvalBot
  .include "mcu_specific/LM_3S1000/TI_EvalBot.h"
.endif
.ifdef TI_Beagle
  .include "mcu_specific/OMAP_35xx/TI_Beagle.h"
.endif
.ifdef TI_Beagle_XM
  .include "mcu_specific/OMAP_35xx/TI_Beagle_XM.h"
.endif
.ifdef GMX_OVERO_TIDE
  .include "mcu_specific/OMAP_35xx/GMX_Overo_Tide.h"
.endif

@--------------------------------------------------------------------------------
@
@  0.C.	  HARDWARE CONSTANTS for each MCU Family
@
@--------------------------------------------------------------------------------

.ifdef	LPC_2000
  .include "mcu_specific/LPC_2000/LPC_2000.h"
.endif
.ifdef	LPC_2800
  .include "mcu_specific/LPC_2800/LPC_2800.h"
.endif
.ifdef	LPC_13xx
  .include "mcu_specific/LPC_1300/LPC_13xx.h"
.endif
.ifdef	LPC_17xx
  .include "mcu_specific/LPC_1700/LPC_17xx.h"
.endif
.ifdef	AT91_SAM3S
  .include "mcu_specific/AT91_SAM3S/AT91_SAM3S.h"
.endif
.ifdef	AT91_SAM7
  .include "mcu_specific/AT91_SAM7/AT91_SAM7.h"
.endif
.ifdef	AT91_SAM9
  .include "mcu_specific/AT91_SAM9/AT91_SAM9.h"
.endif
.ifdef	STR_7xx
  .include "mcu_specific/STR_7xx/STR_7xx.h"
.endif
.ifdef	STR_9xx
  .include "mcu_specific/STR_9xx/STR_9xx.h"
.endif
.ifdef	STM32x
  .include "mcu_specific/STM32x/STM32x.h"
.endif
.ifdef	EP_93xx
  .include "mcu_specific/EP_93xx/EP_93xx.h"
.endif
.ifdef	S3C24xx
  .include "mcu_specific/S3C24xx/S3C24xx.h"
.endif
.ifdef	LM_3S1000
  .include "mcu_specific/LM_3S1000/LM_3S1000.h"
.endif
.ifdef OMAP_35xx
  .include "mcu_specific/OMAP_35xx/OMAP_35xx.h"
.endif

@---------------------------------------------------------------------------------------------------------
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
@---------------------------------------------------------------------------------------------------------
	
@-------------------------------------------------------------------------------------
@ Assembler Constants and Macros
@-------------------------------------------------------------------------------------
	
.include "common/armpit_as_constants.s"	@ armpit assembly constants for scheme
.include "common/armpit_as_macros.s"	@ armpit assembly macros for scheme

@-------------------------------------------------------------------------------------
@ Reset and Initialization
@-------------------------------------------------------------------------------------
	
.text				@ code (text section, _start label) must be linked to start at 0x00

.syntax unified			@ enable Thumb-2 mode

.global _start
.global	_text_link_address_	@ scheme code and reset address when system is running (link address)
.global _text_section_address_	@ scheme code and reset address in FLASH
.global _data_section_address_	@ boot code address in FLASH for LPC2888, ARM920T and Cortex
	
.ifndef	cortex
   .include "common/armpit_reset_ARM.s"	@ interrupt vectors and resets for arm7tdmi, arm920t, cortex-a8
.else
   .include "common/armpit_reset_CM3.s"	@ interrupt vectors and resets for cortex-m3
.endif

.include "common/armpit_init.s"		@ initialization code (initialize glv, enable interrupts)
	
@-------------------------------------------------------------------------------------
@ MCU-Dependent Initialization and I/O Routines
@-------------------------------------------------------------------------------------

.ifdef	LPC_2000
  .include "mcu_specific/LPC_2000/LPC_2000_init_io.s"
.endif
.ifdef	LPC_2800
  .include "mcu_specific/LPC_2800/LPC_2800_init_io.s"
.endif
.ifdef	LPC_13xx
  .include "mcu_specific/LPC_1300/LPC_13xx_init_io.s"
.endif
.ifdef	LPC_17xx
  .include "mcu_specific/LPC_1700/LPC_17xx_init_io.s"
.endif
.ifdef	AT91_SAM3S
  .include "mcu_specific/AT91_SAM3S/AT91_SAM3S_init_io.s"
.endif
.ifdef	AT91_SAM7
  .include "mcu_specific/AT91_SAM7/AT91_SAM7_init_io.s"
.endif
.ifdef	AT91_SAM9
  .include "mcu_specific/AT91_SAM9/AT91_SAM9_init_io.s"
.endif
.ifdef	STR_7xx
  .include "mcu_specific/STR_7xx/STR_7xx_init_io.s"
.endif
.ifdef	STR_9xx
  .include "mcu_specific/STR_9xx/STR_9xx_init_io.s"
.endif
.ifdef	STM32x
  .include "mcu_specific/STM32x/STM32x_init_io.s"
.endif
.ifdef	EP_93xx
  .include "mcu_specific/EP_93xx/EP_93xx_init_io.s"
.endif
.ifdef	LM_3S1000
  .include "mcu_specific/LM_3S1000/LM_3S1000_init_io.s"
.endif
.ifdef	S3C24xx
  .include "mcu_specific/S3C24xx/S3C24xx_init_io.s"
.endif
.ifdef	OMAP_35xx
  .include "mcu_specific/OMAP_35xx/OMAP_35xx_init_io.s"
.endif

@-------------------------------------------------------------------------------------
@  Turning LEDs on/off
@
@	There are two types of LED on/off approaches:
@	  The default is where the MCU has separate SET and CLEAR registers
@	  otherwise, the has_combined_set_clear flag needs to be set, for single register MCUs
@-------------------------------------------------------------------------------------

_func_	
yldon:	ldr	rvb, =YELLED
	b	ledon
_func_	
gldon:	ldr	rvb, =GRNLED
	b	ledon
_func_	
rldon:	ldr	rvb, =REDLED
_func_	
ledon:
.ifdef has_combined_set_clear
	ldr	rva, =LEDIO
	ldr     rva, [rva, #io_set]
	orr	rvb, rvb, rva
.endif
	ldr	rva, =LEDIO
	str     rvb, [rva, #io_set]
	set	pc,  lnk
	
_func_	
yldoff:	ldr	rvb, =YELLED
	b	ledoff
_func_	
gldoff:	ldr	rvb, =GRNLED
	b	ledoff
_func_	
rldoff:	ldr	rvb, =REDLED
_func_	
ledoff:
.ifdef has_combined_set_clear
	ldr	rva, =LEDIO
	ldr     rva, [rva, #io_clear]
	bic	rvb, rva, rvb
.endif
	ldr	rva, =LEDIO
	str     rvb, [rva, #io_clear]
	set	pc,  lnk

@-------------------------------------------------------------------------------------
@ Scheme environment, functions and ports
@-------------------------------------------------------------------------------------

.balign	4

	@-------.-------.-------.-------.-------+
scmenv:	@	built-in environment		|
	@-------.-------.-------.-------.-------+

		VECSIZE	(end_of_scmenv - scmenv - 4) >> 2

		.word	empty_vector	@ private sub-env (nothing at top-level, used in libraries)
core_env:	.word	corenv		@ 0.0.	Core
base_env:	.word	basenv		@ 4.1.	Primitive expression types
port_env:	.word	prtenv		@	Ports [keep me at index 2 or port functions will fail]
rdwr_env:	.word	rw_env		@ 4.1.	Primitive expression types
lib_env:	.word	libenv		@ 4.2.	Derived expression types
r6lb_env:	.word	r6lenv		@	R6RS library (2. bytevectors, 11.4 bitwise ops, ...)
sys0_env:	.word	s0_env		@	system 0

end_of_scmenv:	@ end of scmenv

	@-------.-------.-------.-------.-------+
@-------@	empty sub-environment vector	|
	@-------.-------.-------.-------.-------+

empty_vector:	VECSIZE	0		@ set in armpit_as_constants.s for non-existent sub-envs

	@-------.-------.-------.-------.-------.-------+
paptbl:	@	primitive pre-entry function table	|
	@-------.-------.-------.-------.-------.-------+
	@ secondary apply table for primitives that start with a jump
	@ pre-function indices (for EPFUNC) are computed below the table.

		VU8SIZE	(end_of_paptbl - paptbl - 4)

atypchk:	.word	typchk		@ armpit_core.s
areturn:	.word	return		@ armpit_core.s
aioprfn:	.word	ioprfn		@ arpit_ports.s
apairen:	.word	pairen		@ armpit_scheme_base.s
aprdnml:	.word	prdnml		@ armpit_scheme_base_6.2.Integers.s & Numbers.s
ardcnml:	.word	rdcnml		@ armpit_scheme_base_6.2.Integers.s & Numbers.s
aunijmp:	.word	unijmp		@ armpit_scheme_base_6.2.Numbers.s
anumgto:	.word	numgto		@ armpit_scheme_base_6.2.Numbers.s
ammglen:	.word	mmglen		@ armpit_scheme_base_6.2.Numbers.s
acxxxxr:	.word	cxxxxr		@ armpit_scheme_library.s
avu8ren:	.word	vu8ren		@ armpit_scheme_r6rs_library.s
abwloop:	.word	bwloop		@ armpit_scheme_r6rs_library.s
abwfent:	.word	bwfent		@ armpit_scheme_r6rs_library.s
aregent:	.word	regent		@ armpit_scheme_r6rs_library.s
afxchk2:	.word	fxchk2		@ armpit_scheme_r6rs_library.s

end_of_paptbl:	@ end of paptbl

	@-------.-------.-------.-------.-------.-------+
@-------@	pre-entry function table indices	|
	@-------.-------.-------.-------.-------.-------+

	otypchk	= (atypchk - paptbl) >> 2
	oreturn = (areturn - paptbl) >> 2
	oioprfn = (aioprfn - paptbl) >> 2
	opairen	= (apairen - paptbl) >> 2
	oprdnml	= (aprdnml - paptbl) >> 2
	ordcnml	= (ardcnml - paptbl) >> 2
	ounijmp	= (aunijmp - paptbl) >> 2
	onumgto	= (anumgto - paptbl) >> 2
	ommglen	= (ammglen - paptbl) >> 2
	ocxxxxr	= (acxxxxr - paptbl) >> 2
	ovu8ren	= (avu8ren - paptbl) >> 2
	obwloop	= (abwloop - paptbl) >> 2
	obwfent	= (abwfent - paptbl) >> 2
	oregent = (aregent - paptbl) >> 2
	ofxchk2	= (afxchk2 - paptbl) >> 2
	
.ltorg
	
	@-------.-------.-------.-------.-------+
@-------@	Functions			|
	@-------.-------.-------.-------.-------+

.include "common/armpit_core.s"

.include "common/armpit_scheme_base.s"

.include "common/armpit_ports.s"

.ifndef	exclude_read_write
  .include "common/armpit_scheme_read_write.s"
.endif

.ifndef	CORE_ONLY
  .include "common/armpit_scheme_library.s"
.endif

.include "common/armpit_scheme_r6rs_library.s"

.ifdef include_system0

  .ifdef	LPC_2000
    .include "mcu_specific/LPC_2000/LPC_2000_system_0.s"
  .endif
  .ifdef	LPC_2800
    .include "mcu_specific/LPC_2800/LPC_2800_system_0.s"
  .endif
  .ifdef	LPC_13xx
  .include "mcu_specific/LPC_1300/LPC_13xx_system_0.s"
  .endif
  .ifdef	LPC_17xx
    .include "mcu_specific/LPC_1700/LPC_17xx_system_0.s"
  .endif
  .ifdef	AT91_SAM3S
    .include "mcu_specific/AT91_SAM3S/AT91_SAM3S_system_0.s"
  .endif
  .ifdef	AT91_SAM7
    .include "mcu_specific/AT91_SAM7/AT91_SAM7_system_0.s"
  .endif
  .ifdef	AT91_SAM9
    .include "mcu_specific/AT91_SAM9/AT91_SAM9_system_0.s"
  .endif
  .ifdef	STR_7xx
    .include "mcu_specific/STR_7xx/STR_7xx_system_0.s"
  .endif
  .ifdef	STR_9xx
    .include "mcu_specific/STR_9xx/STR_9xx_system_0.s"
  .endif
  .ifdef	STM32x
    .include "mcu_specific/STM32x/STM32x_system_0.s"
  .endif
  .ifdef	EP_93xx
    .include "mcu_specific/EP_93xx/EP_93xx_system_0.s"
  .endif
  .ifdef	LM_3S1000
    .include "mcu_specific/LM_3S1000/LM_3S1000_system_0.s"
  .endif
  .ifdef	S3C24xx
    .include "mcu_specific/S3C24xx/S3C24xx_system_0.s"
  .endif
  .ifdef	OMAP_35xx
    .include "mcu_specific/OMAP_35xx/OMAP_35xx_system_0.s"
  .endif
	
.endif

@-------------------------------------------------------------------------------------
@
@ III.D. MCU-DEPENDENT COMPONENTS OF USB I/O and ISR
@
@-------------------------------------------------------------------------------------

.ifdef	native_usb

  .ifdef	LPC_2000
    .include "mcu_specific/LPC_2000/LPC_2000_usb.s"
  .endif
  .ifdef	LPC_2800
    .include "mcu_specific/LPC_2800/LPC_2800_usb.s"
  .endif
  .ifdef	LPC_13xx
    .include "mcu_specific/LPC_1300/LPC_13xx_usb.s"
  .endif
  .ifdef	LPC_17xx
    .include "mcu_specific/LPC_1700/LPC_17xx_usb.s"
  .endif
  .ifdef	AT91_SAM3S
    .include "mcu_specific/AT91_SAM3S/AT91_SAM3S_usb.s"
  .endif
  .ifdef	AT91_SAM7
    .include "mcu_specific/AT91_SAM7/AT91_SAM7_usb.s"
  .endif
  .ifdef	AT91_SAM9
    .include "mcu_specific/AT91_SAM9/AT91_SAM9_usb.s"
  .endif
  .ifdef	STR_7xx
    .include "mcu_specific/STR_7xx/STR_7xx_usb.s"
  .endif
  .ifdef	STR_9xx
    .include "mcu_specific/STR_9xx/STR_9xx_usb.s"
  .endif
  .ifdef	STM32x
    .include "mcu_specific/STM32x/STM32x_usb.s"
  .endif
  .ifdef	EP_93xx
  .endif
  .ifdef	S3C24xx
    .include "mcu_specific/S3C24xx/S3C24xx_usb.s"
  .endif
  .ifdef	OMAP_35xx
    .include "mcu_specific/OMAP_35xx/OMAP_35xx_usb.s"
  .endif

.endif


@-------------------------------------------------------------------------------------	
@ Startup Code for LPC_2800, EP_93xx, S3C24xx, OMAP_35xx -- .data section
@-------------------------------------------------------------------------------------

.data

.ifdef	LPC_2800
	.include "mcu_specific/LPC_2800/LPC_2800_startup.s"
.endif
.ifdef	EP_93xx
	.include "mcu_specific/EP_93xx/EP_93xx_startup.s"
.endif
.ifdef	S3C24xx
	.include "mcu_specific/S3C24xx/S3C24xx_startup.s"
.endif
.ifdef	OMAP_35xx
	.include "mcu_specific/OMAP_35xx/OMAP_35xx_startup.s"
.endif

@---------------------------------------------------------------------------------------------------------
@ END
@---------------------------------------------------------------------------------------------------------
	
.end

