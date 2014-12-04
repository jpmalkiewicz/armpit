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
	@ in:	env	<- scgc_base  =  SIM_SCGC base adrs periph clken
	@ ret:	via lnk
	@ enable module clock (port B pins clock enabled earlier) (p.312)
	swi	run_prvlgd
	rgcpbt	env, #0x0c, 10, 1		@ SCGC4       <- enab clk UART0
	swi	run_normal
	@ configure pin function (on PORTB_PCR, p.250,277,282,1757)
	write	0x300, ioportb_pcr, #(16<<2)	@ PORTB_PCR16 <- cfg pin UART Rx
	write	rvb,   rva,         #(17<<2)	@ PORTB_PCR17 <- cfg pin UART Tx
	@ configure uart (default is 8N1 in UARTn_C1, p.1544)
	write8	UART0_IDIV, uart0_base, #0x01	@ UARTn_BDL   <- baud rate, int
	write8	UART0_FDIV, rva,        #0x0a	@ UARTn_C4    <- baud rate, frac
	write8	0x2c,       rva,        #0x03	@ UARTn_C2    <- ena Tx,Rx,Rxint
	@ return
	set	pc,  lnk



