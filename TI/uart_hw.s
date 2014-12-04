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
	@ in:	cortex	env	<- rcgc_base = peripheral RCGC base adrs
	@ in:	AM335x	env	<- PER_CM_base  = CM_PER  base address
	@ in:	AM335x	dts	<- SCM_base     = Control Module base (L4_WKUP)
	@ in:	AM335x	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP3	env	<- PER_CM_base  = CM_PER  base address
	@ in:	OMAP3	dts	<- CORE_CM_base	= CM_CORE base address
	@ in:	OMAP3	glv	<- WKUP_CM_base	= CM_WKUP base address
	@ in:	OMAP4	env	<- L3INIT_CM2_base = L3INIT_CM2 base
	@ in:	OMAP4	dts	<- L4PER_CM2_base  = L4PER_CM2 base
	@ in:	OMAP4	glv	<- SCM_PADCONF     = SYSCTRL_PADCONF_CORE
	@ ret:	via lnk

	/* cortex-M3/M4: LM3S, LM4F */
  .ifdef cortex	@ LM3S, LM4F
     .ifdef LM_3S1000	/* LM3S */
	@ initialize UART0 for chosen baud rate and 8N1 operation
	rgcpbt	env, #0x08, 0, 1	@ RCGC2    <- 0x01,bit 0,clk Port A
	write	0x1ACCE551, ioporta_base+0x500, #0x20	@ GPIOLOCK <- PORT A, unlock AFSEL
	ldr	rvc, =ioporta_base+0x400
	write	sv3, rvc, #0x20		@ GPIOAFSEL<- UART0 func for pins p.169
	write	sv3, rva, #0x1c		@ GPIODEN  <- UART0 pins active, p.169
    .endif
    .ifdef LM_4Fxxx	/* LM4F */
	rgcpbt	env, #0x08, 0, 1	@ RCGCGPIO <- enable prt A,PA0-Rx,PA1-Tx
	rgwfbt	glv, #0x08, 0, 1	@ wait for periph ready bit in PR_GPIO
	write	0x4C4F434B, ioporta_base+0x500, #0x20	@ GPIOLOCK <- PORT A, unlock AFSEL
	ldr	rvc, =ioporta_base+0x400
	write	sv3, rvc, #0x20		@ GPIOAFSEL<- UART0 func for pins, p.169
      .ifdef EK_TM4C1294
	write	0x11, rva, #0x2c	@ GPIOPCTL <- UART Port function
      .endif
	write	sv3, rva, #0x1c		@ GPIODEN  <- UART0 activ, non tri-state
	write	sv3, env, #0x18		@ RCGCUART <- enable clk for sel uart(s)
	rgwfbf	glv, #0x18, 0, 2, 3	@ wait for periph ready bits in PR_UART
    .endif
	@ configure uart mode
	rgcpbt	uart0_base, #0x30, 0, 0	@ UARTCTL  <- disable UART0
	write	UART0_IDIV, rva, #0x24	@ UARTIBRD
	write	UART0_FDIV, rva, #0x28	@ UARTFBRD
	write	0x60, rva, #0x2c	@ UARTLCRH <- 8,N,1, no fifo
	write	0x10, rva, #0x38	@ UARTIM   <- allow Rx interrupt to vic
	rgrmw	rva, #0x30, 0x0301	@ UARTCTL  <- enable UART0, Tx, Rx
  .endif @ cortex (LM3S, LM4F)

	/* cortex-A8/A9: AM335x, OMAP3, OMAP4 */
  .ifdef cortex_a8
    .ifdef AM335x	/* AM335x */
	@ enable UART0 module
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ
	write	sv2, glv, #0xB4		@ CM_WKUP_UART0_CLKCTRL <- enable UART0
	@ configure UART0 pins (aka uart0) (Tx E16, Rx E15, Mode 0)
	add	rva, dts, #0x0970	@ rva <- conf_uart0 base address
	write	0x30, rva, #0x00	@ UART0_RXD <- mode 0, pull-up, Rx
	write	0x10, rva, #0x04	@ UART0_TXD <- mode 0, pull-up, no Rx
	swi	run_no_irq		@ set Thread mode, unprvlgd, no IRQ, usr
    .else
      .ifndef cortex_a9	/* OMAP3 */
	@ enable clocks for uart3 (use: env<-CM_PER)
	rgcpbt	env, #F_clck, 11, 1	@ rvb <- set uart3 Fclk, bit 11
	rgcpbt	env, #I_clck, 11, 1	@ cm_iclken_per <- enable uart3 Iclk
	@ configure uart3 pins
	rgcpbf	SCM_base+0x100, #0x9c, 16, 32, 0x0100 @ CONTROL_PADCONF_UART3_RX
	rgcpbf	rva, #0xa0,  0,  9, 0x0000
      .else 		/* OMAP4 */
	@ power-up uart3 and cfg UART3 Rx/Tx pins G27,G28 mode 0
	@ (use: dts <- L4PER_CM2 base, glv <- SYSCTRL_PADCONF_CORE)
	write	sv2, dts, #0x0150	@ CM_L4PER_UART3_CLKCTRL <- enable
	write	0x00180518, glv, #0x144	@ PAD0_UART3_RX_IRRX_PAD1_UART3_TX_IRTX
      .endif
    .endif
	@ configure uart mode
	write	0x07, uart0_base, #0x20	@ MDR1 <- disable uart operation
	write	0x80, rva, #0x0c	@ LCR  <- enable divisor latch
	write	UART_DIVL, rva, #0x00	@ DLL  <- 26 (low  div for 115200 baud)
	write	UART_DIVH, rva, #0x04	@ DLH  <-  0 (high div for 115200 baud)
	write	sv3, rva, #0x0c		@ LCR  <- disable div latch set 8N1 frmt
	write	fre, rva, #0x20		@ MDR1 <- enable uart op (16x mode)
	write	sv1, rva, #0x04		@ IER  <- enable Rx interrupt
  .endif @ cortex_a8/a9

	/* return */
	set	pc,  lnk



