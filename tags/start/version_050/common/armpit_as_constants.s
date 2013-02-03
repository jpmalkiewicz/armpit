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
@  0.E.	  Scheme Tags and Constants
@
@---------------------------------------------------------------------------------------------------------

@ tags and constants:
@ a) Immediate tags
@ a1) numbers & pointers
pointer_tag	= 0x00
int_tag		= 0x01			@ integer tag
float_tag	= 0x02			@ float tag
@ a2) var/synt, broken-heart, offset-tag
broken_heart	= 0x9F			@ 
variable_tag	= 0xAF
@ a3) ground items
scheme_null	= 0x0F
scheme_true	= 0x1F
scheme_false	= 0x2F
char_tag	= 0x3F
@ b) non-Immediate tags
@ b1) sized -- non-gceable
symbol_tag	= 0x7F			@ #x30 mask used in gc to identify non gc-eable sized-item
string_tag	= 0x5F			@ 
bytevector_tag	= 0x6F			@ 
@ b2) sized -- gceable
vector_tag	= 0x4F			@ 
@ b3) listed -- non-procedure
macro		= 0xD7
@ b4) listed -- procedure
procedure	= 0xDF
@ c) 8-byte (4-bit tag)
rational_tag	= 0x03
complex_tag	= 0x0B

@ list tag reported by typchk, typsv1, typsv2
@ (lists are not type tagged in the internal representation)
list_tag	= 0xFF
	
@ abbreviations
t	= scheme_true
f	= scheme_false
null	= scheme_null
i0	= int_tag
i1	= (1 << 2) | i0
i2	= (2 << 2) | i0
f0	= float_tag
npo	= char_tag
proc	= procedure

@ floating point constants
scheme_inf	= 0x7F800002
scheme_nan	= 0x7F800006
scheme_pi	= 0x40490FDA		@ 0100-0000-0100-1001-0000-1111-1101-1010 pi     as float
scheme_two_pi	= 0x40C90FDA		@ 0100-0000-1100-1001-0000-1111-1101-1010 2*pi   as float
scheme_half_pi	= 0x3FC90FDA		@ 0011-1111-1100-1001-0000-1111-1101-1010 pi/2   as float
scheme_log_two	= 0x3F317216		@ 0011-1111-0011-0001-0111-0010-0001-0110 (ln 2) as float
scheme_two	= 0x40000002		@ 0100-0000-0000-0000-0000-0000-0000-0010 two    as float
scheme_one	= 0x3F800002		@ 0011-1111-1000-0000-0000-0000-0000-0010 one    as float
scheme_1p5	= 0x3FC00002		@ 0011-1111-1100-0000-0000-0000-0000-0010 1.5    as float
scheme_0p5	= 0x3F000002		@ 0011-1111-0000-0000-0000-0000-0000-0010 0.5    as float
scheme_e	= 0x402DF856		@ 0100-0000-0010-1101-1111-1000-0101-0110 e      as float
scheme_em1	= 0x3EBC5AB6		@ 0011-1110-1011-1100-0101-1010-1011-0110 1/e    as float

@ punctuation
backspace_char	= (0x08 << 8) | char_tag	@     backspace
tab_char	= (0x09 << 8) | char_tag	@	tab
lf_char		= (0x0A << 8) | char_tag	@     lf
cr_char		= (0x0D << 8) | char_tag	@     cr
newline_char	= (0x0D << 8) | char_tag	@     cr
space_char	= (0x20 << 8) | char_tag	@     space
dbl_quote_char	= (0x22 << 8) | char_tag	@  "  double quote "
pound_char	= (0x23 << 8) | char_tag	@  #  pound
open_par_char	= (0x28 << 8) | char_tag	@  (  open parenthesis
close_par_char	= (0x29 << 8) | char_tag	@  )  close parenthesis
dot_char	= (0x2E << 8) | char_tag	@  .  dot
at_char		= (0x40 << 8) | char_tag	@  @  at char
backslash_char	= (0x5C << 8) | char_tag	@  \  backslash
eof_char	= (0x03 << 8) | char_tag	@  ctrl-D eof char (really EOT)

@------------------------------------------------------------------------------
@	
@	dummy sub-environments and pre-entry function labels for
@	items not included in assembly based on configuration switches
@	(used at labels scmenv: and/or paptbl: in armpit_050.s).
@
@------------------------------------------------------------------------------

@ define dummy labels for sub-environments that are not included
@ based on configuration options.

.ifdef	exclude_read_write
  rw_env = empty_vector
.endif

.ifdef	CORE_ONLY
  libenv = empty_vector
.endif

.ifndef	include_system0
  s0_env = empty_vector
.endif


@ define dummy labels for common pre-entry functions that are not included
@ based on configuration options.

.ifdef	integer_only
  unijmp = corerr
  numgto = corerr
.endif

.ifdef CORE_ONLY
  mmglen = corerr
  cxxxxr = corerr
.endif

.ifndef include_r6rs_fx
  fxchk2 = corerr
.endif
	
@------------------------------------------------------------------------------
@	
@	dummy ISR labels for cases where USB or I2C are
@	not included in assembly based on configuration switches
@	(used in ISR vector in mcu_specific/family/family_init_io.s).
@
@------------------------------------------------------------------------------

.ifndef	include_i2c
	pi2isr = i0
.endif

.ifndef	native_usb
	usbisr = i0
.endif

	
@---------------------------------------------------------------------------------------------------------
@
@  0.F.	  REGISTER RENAMING for SCHEME
@
@---------------------------------------------------------------------------------------------------------

fre	.req r0		@ free pointer
cnt	.req r1		@ continuation (return address)
rva	.req r2		@ raw value a
rvb	.req r3		@ raw value b
sv1	.req r4		@ scheme value 1
sv2	.req r5		@ scheme value 2
sv3	.req r6		@ scheme value 3
sv4	.req r7		@ scheme value 4
sv5	.req r8		@ scheme value 5
env	.req r9		@ default environment
dts	.req r10	@ data stack
glv	.req r11	@ global vector
rvc	.req r12	@ raw value c
lnk	.req r14	@ jump link -- lr

@---------------------------------------------------------------------------------------------------------
@
@  0.B.   Common Definitions (heap size, buffers, code address)
@
@---------------------------------------------------------------------------------------------------------

ALLLED		= REDLED | YELLED | GRNLED

@
@ Note:	for stop-and-copy gc, each half-heap's size must be a multiple of 8 bytes.
@	Thus, heaptop1 minus heapbottom must be a multiple of 16 bytes.
@	Given that RAMTOP and RAMBOTTOM are 16-byte aligned, the requirement is
@	met if stack_size, READBUFFER, RBF_size, WRITEBUFFER, WBF_size,
@	and EXTRA_FILE_RAM are all 16-byte aligned, and/or made so by choosing
@	appropriate 8 and/or 16 byte spacers below.
@

.ifdef	LPC_2000
  stack_size	= 288				@ additional stack space needed by IAP on LPC-2000
.endif
.ifndef	stack_size
    stack_size	= 160
.endif

.ifndef	BUFFER_START
  .ifndef STR_7xx
    .ifndef  AT91_SAM7
      .ifndef cortex
BUFFER_START	= RAMBOTTOM + 0x04		@ size is 0x7c including tag
READBUFFER	= RAMBOTTOM + 0x80		@ must start on a 16-byte boundary
      .else
	.if num_interrupts > 64
BUFFER_START	= RAMBOTTOM			@ size is 0x80 including tag
READBUFFER	= RAMBOTTOM + 0x80		@ must start on a 16-byte boundary
	.else
BUFFER_START	= RAMBOTTOM + 0x04		@ size is 0x7c including tag
READBUFFER	= RAMBOTTOM + 0x80		@ must start on a 16-byte boundary
        .endif
      .endif
    .endif
  .endif
  .ifdef STR_7xx	@ account for sp slippage on stm/ldm
BUFFER_START	= RAMBOTTOM + 0x00A4		@ size is 0x7c including tag
READBUFFER	= RAMBOTTOM + 0x0120		@ must start on a 16-byte boundary
  .endif
  .ifdef  AT91_SAM7	@ account for sp slippage on stm/ldm
BUFFER_START	= RAMBOTTOM + 0x00A4		@ size is 0x7c including tag
READBUFFER	= RAMBOTTOM + 0x0120		@ must start on a 16-byte boundary
  .endif
  .ifndef	WBF_size
    heapbottom	= READBUFFER  + RBF_size + 16	@ heap is above main and read buffers, 16-byte aligned
  .else
    WRITEBUFFER = READBUFFER  + RBF_size + 8	@ writebuffer is above readbuffer (+ tag and position)
    heapbottom	= WRITEBUFFER + WBF_size + 8	@ heap is above main, read and write buffers
  .endif
.else
READBUFFER	= BUFFER_START + 0x80		@ must start on a 16-byte boundary
  .ifdef	WBF_size
    WRITEBUFFER = READBUFFER  + RBF_size + 8	@ writebuffer is above readbuffer (+ tag and position)
  .endif
  .ifndef	heapbottom
    heapbottom	= RAMBOTTOM
  .endif
.endif

RBF_bv_tag	= ((RBF_size + 4) << 8) | bytevector_tag
.ifdef	WBF_size
WBF_bv_tag	= ((WBF_size + 4) << 8) | bytevector_tag
.endif


@ --------- HEAP -----------
.ifdef	STR_9xx
  EXTRA_FILE_RAM	= 0x90			@ RAM space to copy flash-writing code
.endif
.ifdef	STR_7xx
  EXTRA_FILE_RAM	= 0x60			@ RAM space to copy flash-writing code
.endif
.ifdef	AT91_SAM7
  EXTRA_FILE_RAM	= 0x60			@ RAM space to copy flash-writing code
.endif
.ifdef	AT91_SAM3S
  EXTRA_FILE_RAM	= 0x60			@ RAM space to copy flash-writing code
.endif
.ifndef EXTRA_FILE_RAM
  EXTRA_FILE_RAM	= 0
.endif
.ifndef mark_and_sweep
  heaptop1	=  RAMTOP - stack_size - EXTRA_FILE_RAM
  heaptop0	= (RAMTOP - stack_size - EXTRA_FILE_RAM - heapbottom) >> 1 + heapbottom
.else
  grey_set_size	= (RAMTOP - RAMBOTTOM) >> 8	@ grey and black set sizes, in words -- 8-bytes per bit
  grey_size_bytes	= grey_set_size << 2	@ grey and black set sizes, in bytes -- 8-bytes per bit
  heaptop0	= RAMTOP - stack_size - EXTRA_FILE_RAM - grey_set_size << 3
  heaptop1	= heaptop0
.endif


@ --------- BUFFERS -----------
FILE_LOCK	= 0
ISR_V_offset	= 1
READ_BF_offset	= 2
@ space for I2C0ADR at offset = 3, BUFFER_START + 4 + 0x0C, if needed (eg. for AT91SAM7, EP9302)
I2C0_BF_offset	= 4
I2C1_BF_offset	= 9
WRITE_BF_offset	= 14
USB_CONF_offset	= 15
USB_DATA_offset	= 16
USB_CHNK_offset	= 18
USB_LC_offset	= 20
USB_ST_BF_offset= 22
USB_BK_DT_offset= 24
USB_ZERO_offset	= 26
CTX_EI_offset	= 28
@ writebuffer is now set in MCU specific .h files

F_LOCK		= BUFFER_START + 4 + FILE_LOCK   << 2
.ifndef	I2C0ADR
  I2C0ADR	= BUFFER_START + 4 + 0x0C
.endif
I2C0BUFFER	= BUFFER_START + 4 + I2C0_BF_offset   << 2
I2C1BUFFER	= BUFFER_START + 4 + I2C1_BF_offset   << 2
USB_CONF	= BUFFER_START + 4 + USB_CONF_offset  << 2
USB_DATA	= BUFFER_START + 4 + USB_DATA_offset  << 2
USB_CHUNK	= BUFFER_START + 4 + USB_CHNK_offset  << 2
USB_LineCoding	= BUFFER_START + 4 + USB_LC_offset    << 2
USB_SETUP_BUFFER= BUFFER_START + 4 + USB_ST_BF_offset << 2
USB_BULK_DATA	= BUFFER_START + 4 + USB_BK_DT_offset << 2
USB_ZERO	= BUFFER_START + 4 + USB_ZERO_offset  << 2
	
	
@ ----- CODE ADDRESSES --------
@ Specify where to store scheme+reset code (.text section)
@ and boot code (.data section) in MCU FLASH
.ifdef	LPC_H2888
	_data_section_address_	= 0x10400000
	_text_section_address_	= _data_section_address_ + 0x0400
.endif
.ifdef	CS_E9302
	_data_section_address_	= 0x60000000
	_text_section_address_	= _data_section_address_ + 0x0400
.endif
.ifdef	TCT_Hammer
	_data_section_address_	= 0x00000000
	_text_section_address_	= _data_section_address_ + 0x0400
.endif
.ifdef	TI_Beagle
	_data_section_address_	= 0x40200000
  .ifndef live_SD
	_text_section_address_	= _data_section_address_ + 0x0400
  .else
	_text_section_address_	= _data_section_address_ + 0x0480
  .endif
	_text_link_address_	= 0x80000000
.endif
.ifdef	TI_Beagle_XM
	_data_section_address_	= 0x40200000
	_text_section_address_	= _data_section_address_ + 0x0480
	_text_link_address_	= 0x80000000
.endif
.ifdef	GMX_OVERO_TIDE
	_data_section_address_	= 0x40200000
	_text_section_address_	= _data_section_address_ + 0x0480
	_text_link_address_	= 0x80000000
.endif
@ ************* this (below) was not in prior version -- is it needed? ***************
.ifdef	AT91_SAM7
	_text_section_address_	= 0x00100000		@ 				       	<RDC>
	_data_section_address_	= 0x00110000		@ 				       	<RDC>
.endif
.ifndef	_text_section_address_
	_text_section_address_	= 0x00000000
	_data_section_address_	= 0x00010000
.endif
.ifndef	_text_link_address_
	_text_link_address_	= 0x00000000
.endif
	
@---------------------------------------------------------------------------------------------------------


@ ARM MCU interrupts definitions (ARCHv4T, not Cortex)

IRQ_disable	= 0x80
FIQ_disable	= 0x40

@---------------------------------------------------------------------------------------------------------
@
@  0.D.	  RUNNING MODES
@
@---------------------------------------------------------------------------------------------------------
@---------------------------------------------------------------------------------------------------------
@
@ Note:	 lnkbit0 is needed, on cortex, by operations where lnk is stored in a scheme register,
@ and needs to lose (bic) its lsb (which is 1 for THUMB mode) so that it becomes gc-safe.
@ Similarly, restoring lnk from a scheme register needs it to be orred with this lnkbit0.
@
@---------------------------------------------------------------------------------------------------------

.ifndef cortex
	run_normal	= 0x10		@ user mode with    interrupts
	normal_run_mode	= 0x10
	run_no_irq	= 0x90		@ user mode without interrupts
	isr_normal	= 0x12		@ IRQ  mode with    interrupts
	isr_no_irq	= 0x92		@ IRQ  mode without interrupts
	lnkbit0		= 0		@ used to keep lnk in ARM mode
.else
	run_normal	= 0x10
	normal_run_mode	= 0x01000000	@ xPSR with bit 24 (Thumb mode) set
	run_no_irq	= 0x90		@ user mode without interrupts
	isr_normal	= 0x12		@ IRQ  mode with    interrupts (not used)
	isr_no_irq	= 0x92		@ IRQ  mode without interrupts
	lnkbit0		= 1		@ used to keep lnk in THUMB mode
.endif

