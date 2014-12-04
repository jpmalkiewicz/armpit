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
uarcfg:	/* configure uart */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- [not set]
	@ ret:	via lnk
	@ initialize UART0 for bauds, 8N1 operation
	write	0x0c, 0x56000070, #0x08	@ GPHCON  <- disab pulup on GPH 2,3
	write	0xa0,        rva, #0x00	@ enab uart0 Tx Rx for pins GPH 2,3
	write	sv3,  uart0_base, #0x00	@ ULCON0 <- 8, N, 1
	write	fre,         rva, #0x08	@ UFCON0 <- no FIFO
	write	UART0_DIV,   rva, #0x28	@ UBRDIV0 <- 329 (divisor for baud)
	write	0x45,        rva, #0x04	@ UCON0 <- enable Tx, Rx, error int
	@ return
	set	pc,  lnk



