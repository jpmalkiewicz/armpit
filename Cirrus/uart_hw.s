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
	@ in:	env	<- int_base = VIC1 base address
	@ in:	dts	<- VIC2 base address
	@ ret:	via lnk
	@ initialize UART0 for bauds, 8N1 operation
	read	rvc,  0x80930000, #0x80	@ rvc <- DeviceCfg
	orr	rvc, rvc, #(1<<18)	@ enable UART0 (aka uart1)
	write	0xaa, rva,        #0xc0	@ sysSWLock <- unlock DeviceCfg register
	write	rvc,  rva,        #0x80	@ DeviceCfg enab/pwr-up UART
	write	UART0_DIV_L, uart0_base, #0x10	@ Uart1LinCtrlLow  <- lo div
	write	UART0_DIV_H, rva,        #0x0c	@ Uart1LinCtrlMid  <- hi div
	write	0x60,        rva,        #0x08	@ Uart1LinCtrlHigh <- 8N1,noFIFO
	write	fre,         rva,        #0x1c	@ Uart1IntIDIntClr <- clear ints
	write	0x11,        rva,        #0x14	@ Uart1Ctrl <- enab uart1,Rx int
	write	1<<23,       env,        #0x10	@ VIC1IntEnable <-bt 23=RXintR1
	write	1<<20,       dts,        #0x10	@ VIC2IntEnable <-bt 52=ovrl int
	@ return
	set	pc,  lnk



