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
	@ in:	env	<- sys_ctrl, clock selection, enabling, ...
	@ ret:	via lnk
	@ initialize UART0 for chosen baud rate and 8N1 operation
  .ifdef LPC_13xx
	write	sv1, iocon_pio, #0xa4	@ set UART_RXD func on P1.6 (no pul-u/d)
	write	sv1, rva,       #0xa8	@ set UART_TXD func on P1.7 (no pul-u/d)
	rgcpbt	env, #0x80, 12, 1 	@ SYSAHBCLKCTRL <- pwr UART0
	write	sv1, env,       #0x98	@ UARTCLKDIV    <- pwr UART0 Clk (div=1)
  .endif
  .ifdef LPC_17xx
	rgcpbf	PINSEL0, #0x40,4,8,0xa	@ PINMODE0 <- disable pull-up/down
	rgcpbf	rva,     #0x00,4,8,0x5	@ PINSEL0  <- Enab UART0 pins P0.2,P0.3
  .endif
  .ifdef LPC_2000
    .ifndef LPC2478_STK
	rgcpbf	PINSEL0, #0x00,0,4,0x5	@ PINSEL0  <- Enab UART0 pins P0.0,P0.1
    .else
	rgcpbf	PINSEL0, #0x40,4,8,0xa	@ PINMODE0 <- disable pull-up/down
	rgcpbf	rva,     #0x00,4,8,0x5	@ PINSEL0  <- Enab UART0 pins P0.2,P0.3
    .endif
  .endif
  .ifdef LPC_4300
  	@ disconnect uart0 from P2_0, P2_1 (if needed)
	read	rvb, SCU_SFSP2_n, #0x00	@ rvb <- P2_0 config
	and	rvb, rvb, #3		@ rvb <- P2_0 mode
	eq	rvb, #1			@ mode = uart0 Tx?
	it	eq
	writeeq	fre, rva, #0x00		@ 	if so,  discon uart0 from P2_0
	read	rvb, rva, #0x04		@ rvb <- P2_1 config
	and	rvb, rvb, #3		@ rvb <- P2_1 mode
	eq	rvb, #1			@ mode = uart0 Rx?
	it	eq
	writeeq	fre, rva, #0x04		@ 	if so,  discon uart0 from P2_1
	@ configure uart0 pins 
	write	0x10|UART_pinmode, UART_PINSEL, #(UART_tx_pin<<2) @ cfg TXD pin
	write	0x50|UART_pinmode, rva,         #(UART_rx_pin<<2) @ cfg RXD pin
  .endif
	@ configure uart speed, 8N1 format
	write	sv1, uart0_base,  #0x08	@ U0FCR <- Enab Rx trigger-lvl = 1 char
	write	0x80,        rva, #0x0c	@ U0LCR <- Enab divisor latch
	write	UART0_DIV_L, rva, #0x00	@ U0DLL <- lwr byt of div for baud rate
	write	UART0_DIV_H, rva, #0x04	@ U0DLM <- upr byt of div for baud rate
	write	sv3,         rva, #0x0c	@ U0LCR <- Disab div latch, set 8N1
	write	sv1,         rva, #0x04	@ U0IER <- Enab RDA int
	@ return
	set	pc,  lnk



