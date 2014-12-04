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


_func_
hw_cfg:	/* configure clocks, sdram, flash */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ out:	env	<- [not set on LPC_4300]
	@ ret:	via lnk

	/* allow 4-byte stack (clear STKALIGN in CCR) */
	stackalign_4			@ cortex-m3/m4 macro

	/* check if code is running in RAM bank 2 (if option says it should) */
  .ifdef run_in_bank2
	read	rvb, sys_config, #0x100	@ rvb <- M4MEMMAP
	set	cnt, RAM_bank_2
	eq	rvb, cnt		@ is code already running from bank 2?
	beq	cdcskp			@	if so,  skip code copy & remap
	@ copy code to RAM bank 2
	set	rvc, 0
cdclop:	@ code copy loop (32KB/64KB)
	read	rvb, rvc, #0x00
	write	rvb, cnt, rvc
	add	rvc, rvc, #4
	eq	rvc, #boot_ram_size	@ end code copy (32KB/64KB)?
	bne	cdclop
	@ remap bank 2 to address 0x00000000
	write	cnt, rva, #0x100	@ M4MEMMAP <- RAM bank 2 remapped to 0
cdcskp:	@ keep going
  .endif

	/* set FLASHCFG (formerly MAMTIM) */
  .ifndef ext_flash_only
    .ifndef run_at_120mhz
	rgcpbf	sys_config+0x120, #0x00, 12, 16, 9 @ FLASHCFGA <- 9+1 cyc 204MHz
	rgcpbf	rva,              #0x04, 12, 16, 9 @ FLASHCFGB <- 9+1 cyc 204MHz
    .else
	rgcpbf	sys_config+0x120, #0x00, 12, 16, 5 @ FLASHCFGA <- 5+1 cyc 120MHz
	rgcpbf	rva,              #0x04, 12, 16, 5 @ FLASHCFGB <- 5+1 cyc 120MHz
    .endif
  .endif

	/* configure clocks */
	@ enable XTal (12 MHz) and PLL1 (204 MHz = 17*12/2)
	write	fre, CGU_base, #0x18		    @ XTAL_OSC_CTRL <- enab Xtal
	write	(6<<24)|(16<<16)|(1<<7), rva, #0x44 @ PLL1_CTRL <- cfg, bypass
	rgwfbt	rva, #0x40, 0, 1		    @ wait for PLL1 locked
	@ enable PLL0USB (480 MHz)
	rgrmw	rva, #0x20, 0x10, 0x03	@ PLL0USB_CTRL   <- PLL0USB disabled
	write	0x06167ffa, rva, #0x24	@ PLL0USB_MDIV   <- 480 MHz from XTal
	write	0x00302062, rva, #0x28	@ PLL0USB_NP_DIV <- 480 MHz from XTal
	write	0x06000808, rva, #0x20	@ PLL0USB_CTRL   <- enab,XTalsrc,autoblk
	rgwfbt	rva, #0x1c, 0, 1	@ wait for PLL0USB locked
	rgcpbt	rva, #0x20, 4, 1	@ PLL0USB_CTRL   <- PLL0USB output clock
	@ connect other clocks
	write	(7<<24)|(1<<11)|(3<<2),   rva,#0x48 @ IDIVA <- PLL0USB/4  120MHz
	write	0xc<<24,                  rva,#0x9c @ UART0 <- IDIVA      120MHz
	write	(0xc<<24)|(1<<11)|(1<<2), rva,#0x4c @ IDIVB <- IDIVA/2     60MHz
	write	(0xc<<24)|(1<<11)|(2<<2), rva,#0x50 @ IDIVC <- IDIVA/3     40MHz
	write	(0xe<<24)|(1<<11),        rva,#0x70 @ SPIFI <- IDIVC       40MHz
	write	(0x6<<24)|(1<<11)|(29<<2),rva,#0x58 @ IDIVE <- XTal/30    400KHz
	write	(0x10<<24)|(1 << 11),     rva,#0x90 @ SDIO  <- IDIVE      400KHz
  .ifndef run_at_120mhz
	write	0x09<<24,                 rva,#0x6c @ M4    <- PLL1       204MHz
  .else
	write	0x0c<<24,                 rva,#0x6c @ M4    <- IDIVA      120MHz
  .endif

	/* return */
	set	pc,  lnk



