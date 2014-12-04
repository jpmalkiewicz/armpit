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
piocfg:	/* configure gpio */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- scgc_base  =  SIM_SCGC base adrs periph clken
	@ ret:	via lnk
	@ enable pin clocks (SCGC5 p.314)
	swi	run_prvlgd
	rgcpbf	env, #0x10, 10, 12, 3	     @ SCGC5       <- enab PORTB & C clk
	swi	run_normal
	@ configure LED pin function (gpio, out) (p.250,277,282,1757,1761)
	write	0x130, ioportb_pcr, #(21<<2) @ PORTB_PCR21 <- blue RGB,hi-str,OD
	write	rvb,   rva,         #(22<<2) @ PORTB_PCR22 <- red  RGB,hi-str,OD
	write	ALLLED,LEDIO,       #io_dir  @ make all LED pins an output
	write	rvb,   rva,         #io_set  @ turn LEDs off
	@ config boot-override button pin func (gpio in) (p.250,277,282,1757)
	write	0x100, ioportc_pcr, #(6<<2)  @ PORTC_PCR06 <- cfg pin SW2 button
	@ return
	set	pc,  lnk



