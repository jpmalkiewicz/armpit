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
	@ in:	env	<- PMC_base
	@ ret:	via lnk
	@ initialize UART/USART for bauds, 8N1 operation
	/* configure pin function */
	mask	rvb, 0, 0b1, uart_rx, uart_tx	@ rvb      <- 1-bit uart pin msk
	write	rvb, uart_gpio, #0x04	@ PIOn_PDR         <- uart pin disab PIO
	set	rvc, rvb		@ rvc              <- 1-bit uart pin msk
  .ifndef cortex
	rgrmw	rva, #0x70, rvc		@ PIOn_ASR         <- uart func
  .else
	rgrmw	rva, #0x70, rvc, xxx	@ PIOn_ABCDSR1 	   <- uart func
  .endif
	rgrmw	rva, #0x74, rvc, xxx	@ PIOn_BSR/ABCDSR2 <- uart func
	/* power-up peripheral (uart/usart) module */
  .ifdef uart_per_id
	write	(1<<uart_per_id), env, #0x10	@ PMC_PCER0 <- enab uart clk/pwr
  .endif
	/* configure peripheral module */
	write	uart_div, uart0_base, #0x20	@ UAR0_BRGR <- bauds=CLK/DIVx16
  .ifndef uart_not_usart
	write	     fre, rva, #0x28		@ UAR0_TTGR <- disab time guard
	write	  0x08C0, rva, #0x04		@ UAR0_MR   <- 8N1, oversamp
  .else
	write	  0x0800, rva, #0x04		@ UAR0_MR   <- 8N1, oversamp
  .endif
  .ifdef AT91_SAM9
	write	      -1, rva, #0x0c		@ disab all uart ints
  .endif
  .ifndef cortex
	write	  0x0202, rva, #0x0120		@ UAR0_PTCR <- disab DMA
  .endif
	write	     sv1, rva, #0x08		@ UAR0_IER  <- enab RxRDY int
	write	  0x0050, rva, #0x00		@ UAR0_CR   <- enab uart RX/TX
	/* return */
	set	pc,  lnk



