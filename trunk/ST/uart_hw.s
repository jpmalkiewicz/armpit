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
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk
  .ifdef STR_7xx
	/* initialization of UART0 for 9600 8N1 operation */
	@ TX <- P0.9 (port 0, bit 9, PC0,1,2 <- 1,1,1 = Push-Pull output)
	@ RX <- P0.8 (port 0, bit 8, PC0,1,2 <- 1,0,0 = TTL input)
	write	UART0_DIV, uart0_base, #0x00	@ UART0_BR     <- baud rate div
	write	   0x0589, rva, #0x0c		@ UART0_CR     <- enab UART0,8N1
	write	      fre, rva, #0x20		@ UART0_TxRSTR <- reset Tx FIFO
	write	      fre, rva, #0x24		@ UART0_RxRSTR <- reset Rx FIFO
	@ select UART function
	mask	rvc, 0, 1, uart_rx, uart_tx	@ rvc <- 0b1 mask for rx & tx
	mask	cnt, 0, 1, uart_tx		@ cnt <- 0b1 mask for tx only
	rgrmw	UART_PINSEL, #0x00, rvc		@ PC0 <- Rx TTL in,Tx psh/pl out
	rgrmw	        rva, #0x04, rvc, cnt	@ PC1 <- Rx TTL in,Tx psh/pl out
	rgrmw	        rva, #0x08, rvc, cnt	@ PC2 <- Rx TTL in,Tx psh/pl out
	@ enable UART receive interrupts
	write	0x01, uart0_base, #0x10	@ UART0_IE	<- enab RxBfNotEmpty Int
  .endif
  .ifdef STR_9xx
	/* configure UART0 pins (P3.0 = Rx, P3.1 = Tx) */
	write	  sv1, env, #0x70		@ SCU_GPIOIN3  <- P3.0 = in
	write	 0x08, env, #0x50		@ SCU_GPIOOUT3 <- P3.1 = AF2out
	write	  sv2, ioport3_base, #io_dir	@ GPIO3_DIR    <- P3.1 = output
	@ configure UART0 peripheral
	write	UART0_DIV, uart0_base, #0x24	@ UART_IBRD <- bauds BRCLK=48MHz
	write	UART0_DIV2, rva, #0x28		@ UART_FBRD <- bauds BRCLK=48MHz
	write	      0x60, rva, #0x2C		@ UART_LCR  <- 8,N,1 mode
	write	    0x0300, rva, #0x30		@ UART_CR   <- enab Tx, Rx
	write	      0x10, rva, #0x38		@ UART_IMSC <- enab Rx interrupt
	rgrmw	rva, #0x30, sv1			@ UART_CR   <- enab Tx, Rx, uart
  .endif
  .ifdef STM32x
	/* initialize USART1 for 9600 8N1 operation */
    .ifndef connectivity_ln
	rgrmw	env, #0x18, 0x4004		@ RCC_APB2ENR <- USART1 & Port A
	rgcpbf	ioporta_base, #0x04, 4, 8, 0xb	@ GPIOA_CRH   <- Tx pspl,Rx in
    .else
      .ifndef swap_default_usart
	@ remap: USART1 -> PB, I2C1 -> PB, TIM1 -> PE, TIM4 -> PD, CAN1 -> PD
	mask	rvc, 0, 1, 1,2,6,7,12,13,14 	@ rvc <- remap mask
	rgrmw	afio_base, #0x04, rvc		@ AFIO_MAPR <- TxRx=PB6-7,tm4=..
	rgcpbt	env, #0x18, 14, 1		@ RCC_APB2ENR <- enable USART1
	rgcpbf	ioportb_base, #0, 24, 32, 0x8b	@ _CRL <- TxAFopp/Rxin
      .else @ uart swap
	rgcpbt	env, #0x1c, 17, 1		@ RCC_APB1ENR <- enable USART2
	rgcpbf	ioporta_base, #0, 8, 16, 0x8b   @ _CRL <- TxAFopp/Rxin
      .endif
    .endif
	write	UART0_DIV, uart0_base, #0x08	@ USART_BRR <- 115200 bauds
	write	0x202c, rva, #0x0c		@ USART_CR1 <- 8N1, Rx int
  .endif
  .ifdef STM32F4
	/* initialize USARTn for 115200 8N1 operation */
	@ 1- Power clock for port B (if needed)
    .ifdef STM32F4_Discov
	rgcpbt	env, #0x30, 1, 1		@ RCC_AHB1ENR <- enab port B clk
    .endif
	@ 2- GPIOn_CRL   <- USARTn Tx/Rx, cfg AF out, push-pull & input
	mask	rvc, 1, 0b11, uart_tx, uart_rx	@ rvc      <- pin clear-mask
	mask	cnt, 1, 0b10, uart_tx, uart_rx	@ cnt      <- pin set-mask
	rgrmw	UART_PINSEL, #0x00, rvc, cnt	@ _MODER   <- alt function mode
	lsr	cnt, cnt, #1			@ cnt      <- updated set-mask
	rgrmw	rva, #0x08, rvc, cnt		@ _OSPEEDR <- speed at 25MHz
	mask	cnt, 1, 0b01, uart_tx		@ cnt      <- pin set-mask
	rgrmw	rva, #0x0c, rvc, cnt		@ Tx-PullUp Rx-NoPull
	mask	rvc, 2, 0xf, uart_tx, uart_rx	@ rvc      <- pin clear-mask
	mask	cnt, 2, 0x7, uart_tx, uart_rx	@ cnt      <- pin set-mask, AF7
	rgrmw	rva, #(0x20+((uart_tx/8)<<2)), rvc, cnt	@ _CRL/H <- AF7
	@ Power clock for USART
    .ifndef swap_default_usart
	rgcpbt	env, #0x44,  4, 1		@ RCC_APB2ENR <- power usart1
    .else
	rgcpbt	env, #0x40, 17, 1		@ RCC_APB1ENR <- power usart2
    .endif
	wait	4				@ wait a bit
	@ set speed and mode
	write	UART0_DIV, uart0_base, #0x08	@ USART_BRR <- 115200 bauds
	write	0xa02c, rva, #0x0c		@ USART_CR1 <- 8N1,Rx int,ovr8
  .endif @ STM32F4
	/* return */
	set	pc,  lnk



