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
piocfg:	/* configure GPIO for LED(s) and button(s) */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk
	/* initialize gpio pins for LEDs */
  .ifdef LPC_13xx
	rgcpbt	env, #0x80, 16, 1	@ SYSAHBCLKCTRL <- power up IOCON
  .endif
  .ifdef LPC_17xx
	write	fre, LEDPINSEL, #0x00	@ GPIO function
  .endif
  .ifdef LPC_2000
	write	fre, LEDPINSEL, #0x00	@ GPIO function
  .endif
  .ifdef LPC_4300
	write	0x10, LEDPINSEL, #(rled_pin<<2)	@ red    LED pin, no p-u/d
	write	rvb,  rva,       #(yled_pin<<2)	@ yellow LED pin, no p-u/d
	write	rvb,  rva,       #(gled_pin<<2)	@ green  LED pin, no p-u/d
  .endif
	write	ALLLED, LEDIO+io_dir,#0	@ make all LED pins output pins

	/* initialize button gpio pins */
  .ifdef LPC_4300
	write	0x50, BOOTOVERRID_PSL, #(BOOTOVERRID_BUT<<2) @ btn pin <- GPIOin
  .endif

	/* return */
	set	pc,  lnk


	/* FlashInitCheck function (specialized for some MCUs) */
.ifdef LPC_2000
  .ifndef LCDDemo_2158
    .ifndef LPC2478_STK
FlashInitCheck: @ return status of init enable/override gpio pin (P0.3) in rva
	read	sv1, PINSEL0, #0x00	@ sv1 <- P0.3 config
	bic	rvb, sv1, #0x00C0
	write	rvb, rva,     #0x00	@ set P0.3 to gpio
	read	sv2, io0_base, #io_dir	@ sv2 <- P0.3 direction
	bic	rvb, sv2, #0x08
	write	rvb, rva,      #io_dir	@ set P0.3 as input
	read	rvc, rva, #0x00		@ rvc <- values of all P0.X
	write	sv2, rva, #io_dir	@ set P0.3 back to prior dir
	write	sv1, PINSEL0, #0x00	@ set P0.3 back to prior setting
	and	rva, rvc, #8		@ rva <- status of P0.3 only (ret value)
	set	sv1, null		@ sv1 <- '() (cleared for exit)
	set	sv2, sv1		@ sv2 <- '() (cleared for exit)
	set	pc,  lnk
    .endif
  .endif
.endif



