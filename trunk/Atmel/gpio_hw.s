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
	@ in:	env	<- PMC_base
	@ ret:	via lnk
	/* power-up peripheral (gpio) module */
	mask	rvb, 0, 1, led_per_id, but_per_id @ rvb <- 1-bit led/but pwr msk
	write	rvb, env, #0x10		@ PMC_PCER0 <- enab LED,BUTN pio
	/* configure pin function */
	mask	rvb, 0, 1, rled_pin,yled_pin,gled_pin @ rvb <- 1-bit led pin msk
	write	rvb, LEDPINSEL, #0x00	@ PIOn_PER  <- led pins pio func
	write	rvb,     LEDIO, #io_dir	@ PIOn_OER  <- led pins out dir
	@ return
	set	pc,  lnk



