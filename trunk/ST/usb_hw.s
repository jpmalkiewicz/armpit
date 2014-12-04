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

/*------------------------------------------------------------------------------
@
@		Regular Line USB Device (Connectivity USB OTG further down)
@
@-----------------------------------------------------------------------------*/

.ifndef connectivity_ln

/*------------------------------------------------------------------------------

		hardware configuration

------------------------------------------------------------------------------*/

_func_
usbcfg:	@ configure usb power and pins
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- rcc_base = RCC base address
	@ ret:	via lnk
  .ifndef always_init_usb		@					<TZC>
	@ check if USB is powered, otherwise, return
    .ifdef vusb_prt
	read	rvb, vusb_prt, #0x08	@ rvb <- vusb (gpio) port pins data
	tst	rvb, #(1 << vusb_pin)	@ is VUSB on?
	it	eq
	seteq	pc,  lnk		@	if not, return
    .endif
  .endif
	@ enable USB clock
  .ifdef cortex
	rgcpbt	env, #0x1c, 23, 1	@ RCC_APB1ENR <- enable clock for USB
  .else
    .ifdef STR_9xx
	rgcpbt	env, #0x14,  9, 1	@ SCU_PCGR0 <- enable USB
	rgcpbt	env, #0x1C,  9, 1	@ SCU_PRR0  <- de-assert USB reset
	rgcpbt	env, #0x14, 10, 1	@ SCU_PCGR0 <- enable USB 48MHz clock
    .else @ STR_7xx
	write	  0x10, env, #0x1c	@ RCCU_PER   <- enable USB kernel
	write	0x0150, env, #0x4c	@ PCU_PLL2CR <- PLL2,12x4MHz,in3-5MHzHCK
	rgwfbt	env, #0x4c, 15, 1	@ wait for PLL2-Locked bit in PCU_PLL2CR
	write	0x01D0, env, #0x4C	@ PCU_PLL2CR <- connect PLL2
	@ configure interrupts
	ldr	rvb, =genisr
	lsl	rvb, rvb, #16
	orr	rvb, rvb, #0x07		@ all IRQs have priority of 7
	write	rvb, EIC_base, #0x78	@ USB HP ISR = EIC_SIR6, priority 7
	write	rvb,      rva, #0xc8	@ USB LP ISR = EIC_SIR26 priority 7
    .endif
  .endif
	@ initialization of USB device controller
	write	fre, USB_CHUNK, #0x00	@ zero bytes remaining to snd at startup
  .ifdef manual_usb_reset
	@ USB disconnect simulation with hard soldered 1.5k pull-up on D+	<TZC>
	@ output 0 to PA12 for some time					<TZC>
	write	sv3, usb_base, #0x40	@ USB_CNT   <- powerdown/reset		<TZC>
	read	dts, ioporta_base, #4	@ dts       <- GPIOA_CRH pin cfg, saved	<TZC>
	rgcpbf	rva, #0x04, 16, 20, 1	@ GPIOA_CRH <- set PA12 to push-pull out<TZC>
	write	1<<12, rva, #io_clear	@ GPIOA_BRR <- set PA12 low		<TZC>
	wait	1<<17			@ wait (eg. 3ms, ***FIXME*** how long?)	<TZC>
	write	dts, rva, #0x04		@ GPIOA_CRH <- set PA12 back to orig cfg<TZC>
  .endif
	@ continue with USB configuration, mode, endpoints
	write	   sv1, usb_base, #0x40	@ USB_CNTR   -> exit power down mode
	wait	0x0100			@ wait between exit pwrdn and exit reset
	write	   fre, rva, #0x40	@ USB_CNTR   -> exit reset mode
	write	   fre, rva, #0x40	@ USB_CNTR   -> exit reset mode, be sure
	write	   fre, rva, #0x44	@ USB_ISTR   -> clr spurious pndng ints
	write	0x9C00, rva, #0x40	@ USB_CNTR   -> int ctr,wakup,susp,reset
	@ configure EP buffers
	write	fre, rva, #0x50		@ BTABLE    -> bfr alloc tbl strt ofst=0
  .ifdef cortex
	write	0x80, usb_base+0x400,#0	@ ADR0_TX   <- EP0 snd bfr ofst
  .else
	write	0x80, usb_base-0x800,#0	@ ADR0_TX   <- EP0 snd bfr ofst
  .endif
  .ifndef STR_9xx
	write	  0x88, rva, #0x08	@ ADR0_RX   <- EP0 rcv bfr ofst
	write	  0x90, rva, #0x10	@ ADR1_TX   <- EP1 snd bfr ofst
	write	  0x98, rva, #0x18	@ ADR1_RX   <- EP1 rcv bfr ofst
	write	   fre, rva, #0x04	@ COUNT0_TX <- 0 bytes to transmit
	write	   fre, rva, #0x14	@ COUNT1_TX <- 0 bytes to transmit
	write	0x1000, rva, #0x0c	@ COUNT0_RX <- blksz= 2B,bfsz= 8B,0B rcv
	write	   rvb, rva, #0x1c	@ COUNT1_RX <- blksz= 2B,bfsz= 8B,0B rcv
	write	  0xa0, rva, #0x20	@ ADR2_TX   <- EP2 snd bfr ofst
	write	  0xe0, rva, #0x28	@ ADR2_RX   <- EP2 rcv bfr ofst
	write	0x01a0, rva, #0x30	@ ADR3_TX   <- EP3 snd bfr ofst
	write	0x01e0, rva, #0x38	@ ADR3_RX   <- EP3 rcv bfr ofst
	write	   fre, rva, #0x24	@ COUNT2_TX <- 0 bytes to transmit
	write	   fre, rva, #0x34	@ COUNT3_TX <- 0 bytes to transmit
	write	0x8400, rva, #0x2c	@ COUNT2_RX <- blksz=32B,bfsz=64B,0B rcv
	write	   rvb, rva, #0x3c	@ COUNT3_RX <- blksz=32B,bfsz=64B,0B rcv
  .else @ STR_9xx
	write	0x01100100, rva, #0x00	@ ADR0_TX   <- EP0 snd/rcv bfr ofst
	write	0x01300120, rva, #0x08	@ ADR1_TX   <- EP0 snd/rcv bfr ofst
	write	0x01c00140, rva, #0x10	@ ADR2_TX   <- EP0 snd/rcv bfr ofst
	write	0x02c00240, rva, #0x18	@ ADR3_TX   <- EP0 snd/rcv bfr ofst
	write	0x10000000, rva, #0x04	@ COUNT0_TX <- 0 byt to Tx, 8 byt to Rx
	write	       rvb, rva, #0x0c	@ COUNT1_TX <- 0 byt to Tx, 8 byt to Rx
	write	0x84000000, rva, #0x14	@ COUNT2_TX <- 0 byt to Tx, 64 byt to Rx
	write	       rvb, rva, #0x1c	@ COUNT3_TX <- 0 byt to Tx, 64 byt to Rx
  .endif
	@ if needed, block below could probably be moved to after buffer alloc
	@ table initialization or, branch-link to a wait loop
	write	0x3230, usb_base, #0x00	@ USB_EP0R      -> cfg EP0 as control EP
	write	  0x80,      rva, #0x4c	@ USB_DADDR	-> enable USB, address 0
	@ signify to USB host that device is attached on USB bus
  .ifdef usb_conn_prt
	mask	rvc, 2, 0xf, usb_conn_pin			@ rvc <- clr-msk
	mask	cnt, 2, 0x7, usb_conn_pin			@ cnt <- set-msk
	rgrmw	usb_conn_prt, #((usb_conn_pin/8)<<2), rvc, cnt	@ _CRL/H<-OD out
	write	1<<usb_conn_pin, rva, #io_clear			@ connect USB
  .endif
	@ return
	set	pc,  lnk		@ return


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/

usbhwReset:
	read	rvb, usb_base, #0x00
	eor	rvb, rvb, #0x3000
	eor	rvb, rvb, #0x0030
	bic	rvb, rvb, #0xC000
	bic	rvb, rvb, #0x00C0
	orr	rvb, rvb, #0x0200
	write	rvb, rva, #0x00		@ USB_EP0R <- enable EP 0 as control EP
	write	0x80, rva, #usb_daddr	@ enable USB at address 0
	set	pc,  lnk

usbhwRemoteWakeUp: @ suspend/wakeup
	read	rvb, usb_base, #0x40	@ rvb <- contents of USB_CNTR
	eor	rvb, rvb, #0x08		@ rvb <- USB_CNTR with FSUSP bit toggled
	write	rvb, rva,      #0x40
	set	pc,  lnk

/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

usbhwEndpointStatus: @ get status of EP into sv2 and sv3 (sv1 is device int)
	and	rvb, sv1, #0x0F		@ rvb <- endpoint part of istr
	read	sv3,usb_base,rvb,LSL #2	@ sv3 <- content of USB_EPnR (EP Stat)
	eq	rvb, #0			@ is interrupt for EP0?
	it	eq
	seteq	rvb, 1<<16		@	if so,  rvb <- indic bit for EP0
	eq	rvb, #1			@ is interrupt for EP1?
	it	eq
	seteq	rvb, 1<<18		@	if so,  rvb <- indic bit for EP1
	eq	rvb, #2			@ is interrupt for EP2?
	it	eq
	seteq	rvb, 1<<20		@	if so,  rvb <- indic bit for EP2
	eq	rvb, #3			@ is interrupt for EP3?
	it	eq
	seteq	rvb, 1<<22		@	if so,  rvb <- indic bit for EP3
	tst	sv1, #0x10		@ is this an IN transfer?
	it	eq
	lsleq	rvb, rvb, #1		@	if so,  adjust indic bit
	set	sv2, rvb
	set	pc,  lnk

/* BULK IN Enpoint Interrupt Response */

usbhwBIe: @ clear the txcomp interrupt
	set	env, UsbBulkInEP
	read	rvb, usb_base, env, LSL #2	@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	write	rvb, rva,      env, LSL #2	@ USB_EPnR <- Clr CTR-TX int
	set	pc,  lnk


usbhwBIw: @ write to Bulk IN EP
	read	rvc, usb_base, #usb_ibulkin	@ rvc <- USB_EP3R
	tst	rvc, #usb_txrdy		 	@ is EP3 active (or stalled)?
	bne	usbhwBIw		 	@	if so,  jmp back to wait
	b	wrtEPU

/* BULK OUT Enpoint Interrupt Response */

usbhwBOe: @ Bulk OUT EP int entry
	bic	sv1, sv1, #usb_itxendp		@ excld Txendpkt bit frm int clr
	write	sv1, usb_base, #usb_iclear_dv	@ clear USB interrupt register
	set	pc,  lnk

usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb
	@ returns via:	lnk
	read	rvb, usb_base, #usb_ibulkin	@ rvb <- contents of USB_EP3R
	tst	rvb, #usb_txrdy		 	@ is EP3 idle or disabled?
	it	ne
	setne	pc,  lnk		 	@	if not, ret (good 2 go)
  .ifndef usb_cntreg_32b
	write	0, usb_hw_buffer, #0x34	 	@ USB_COUNT3_TX <- 0 byt to snd
  .else
	rgrmw	usb_hw_buffer, #0x1c,0xffff,xxx	@ USB_COUNT3_TX <- 0 byt to snd
  .endif
	read	rvb, usb_base, #usb_ibulkin @ rvb <- contents of USB_EP3R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	orr	rvb, rvb, #0x0080
	write	rvb, rva, #usb_ibulkin	@ USB_EP3R <- EP data VALID, tx rdy
	b	usbhwBOw

/* CONTROL IN Enpoint Interrupt Response */


/* CONTROL OUT Enpoint Interrupt Response */

usbhwSetAddress: @ Set Device to Address in sv5
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	@ disable the correct transfer interrupt mask (CTRM) in USB_CNTR
	rgrmw	usb_base, #0x40, 0x9c00, xxx
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	set	dts, USB_DATA		@ dts <- buffer
	set	cnt, 0x00		@ cnt <- 0 bytes to send
	bl	wrtEP			@ write 0 bytes to EP
	rgwfbt	rva, #0x00, 7, 1	@ wait for CTR_TX set in USB_EP0R
	rgrmw	rva, #0, 0xf1f0, 0x8000
	read	rvb, rva, #0		@ rvb <- contents of USB_EP0R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	write	rvb, rva, #0		@ USB_EPnR <- EP data VALID, tx when rdy
	@ re-enable the correct transfer interrupt mask (CTRM) in USB_CNTR
	rgrmw	usb_base, #0x40, 0x9c00
	read	sv5,USB_SETUP_BUFFER,#0	@ sv5 <- reqtyp(8), request(8), val(16)
	lsr	sv5, sv5, #16		@ sv5 <- address = val(16)
	orr	rvb, sv5, #0x80		@ rvb <- address ored w/Dev Enab (0x80)
	write	rvb, usb_base, #0x4C	@ set address
	b	usbEPx

usbhwConfigure: @ Configure the device
	@ configure USB
	write	0x0621, usb_base, #0x04	@ enable EP1 -- Interrupt IN
	write	0x3002, rva,      #0x08	@ enable EP2 -- Bulk OUT
	write	0x0023, rva,      #0x0C	@ enable EP3 -- Bulk IN
	set	pc,  lnk

/* Status IN/OUT responses */


/* Enpoint stalling, unstalling */

usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	read	rvb, usb_base, #0	@ rvb <- USB_EP0R
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0010
	orr	rvb, rvb, #0x8000
	write	rvb, rva,      #0	@ USB_EPnR <- EP data VALID, tx when rdy
	b	usbEPx

/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	and	env, env, #0x0F
	read	rvb, usb_base, env, LSL #2	@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF000
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x0080
	write	rvb, rva,      env, LSL #2	@ USB_EP0R <- Clear CTR-RX int
	set	rva, usb_hw_buffer		@ rva <- usb hw buffer start
  .ifndef usb_cntreg_32b
	add	rva, rva, env, LSL #4	@ rva <- start of pkt bfr table for EP0
	read	cnt, rva, #12		@ cnt <- num bytes rcvd (USB_COUNTn_RX)
	bic	cnt, cnt, #0xFC00	@ cnt <- number of bytes received
	read	rvb, rva, #8		@ rvb <- offset to USB_ADRn_RX data / 2
	set	rva, usb_hw_buffer	@ rva <- usb hardware buffer start
	add	rva, rva, rvb, LSL #1	@ rva <- start adrs of Rx pkt mem for EP
	set	rvb, 0
usbSEZ:	cmp	rvb, cnt
	bpl	usbSEX
	read	rvc, rva, rvb, LSL #1
	write16	rvc, dts, rvb
	add	rvb, rvb, #2
	b	usbSEZ
  .else
	add	rva, rva, env, LSL #3	@ rva <- start of pkt bfr table for EP0
	read	cnt, rva, #4		@ cnt <- num bytes rcvd (USB_COUNTn_RX)
	lsr	cnt, cnt, #16
	bic	cnt, cnt, #0xFC00	@ cnt <- number of bytes received
	read	rvb, rva, #0		@ rvb <- offset to USB_ADRn_RX data
	lsr	rvb, rvb, #16
	set	rva, usb_hw_buffer	@ rva <- usb hardware buffer start
	add	rva, rva, rvb		@ rva <- start adrs of Rx pkt mem for EP
	set	rvb, 0
usbSEZ:	cmp	rvb, cnt
	bpl	usbSEX
	read	rvc, rva, rvb
	write	rvc, dts, rvb
	add	rvb, rvb, #4
	b	usbSEZ
  .endif
usbSEX:	write	rvb, usb_base, env, LSL #2	@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xC000
	bic	rvb, rvb, #0x00F0
	eor	rvb, rvb, #0x3000
	orr	rvb, rvb, #0x0080
	orr	rvb, rvb, #0x8000
	write	rvb, rva,      env, LSL #2	@ USB_EP0R <- dat VALID, rcv rdy
	set	pc,  lnk

wrtEP:	@ write data to Control or Bulk In Endpoint
wrtEPU:	@ write data to Control or Bulk In Endpoint
	and	env, env, #0x0F
	read	rvb, usb_base, env, LSL #2	@ rvb <- USB_EPnR
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00F0
	orr	rvb, rvb, #0x8000
	write	rvb, rva,      env, LSL #2	@ USB_EPnR <- Clear CTR-TX int
	set	rva, usb_hw_buffer		@ rva <- usb hw buffer start
  .ifndef usb_cntreg_32b
	add	rva, rva, env, LSL #4	@ rva <- start of pkt bfr table for EP
wrtEPW:	write	0,   rva, #4		@ store num byte to snd in USB_COUNTn_TX
	read	rvb, rva, #4		@ store num byte to snd in USB_COUNTn_TX
	eq	rvb, #0
	bne	wrtEPW
	write	cnt, rva, #4		@ store num byte to snd in USB_COUNTn_TX
	read	rvb, rva, #0		@ rvb <- offset to USB_ADRn_TX data / 2
	set	rva, usb_hw_buffer	@ rva <- usb hardware buffer start
	add	rva, rva, rvb, LSL #1	@ rva <- start adrs of Tx pkt mem for EP
	set	rvb, 0
wrtEPX:	cmp	rvb, cnt
	bpl	wrtEPY
	read8	rvc, dts, rvb
	add	rvb, rvb, #1
	read8	sv5, dts, rvb
	sub	rvb, rvb, #1
	orr	rvc, rvc, sv5, lsl #8
	write	rvc, rva, rvb, LSL #1
	add	rvb, rvb, #2
	b	wrtEPX
  .else
	add	rva, rva, env, LSL #3	@ rva <- start of pkt bfr table for EP
	read	rvb, rva, #4
	bic	rvb, rvb, #0xff00
	bic	rvb, rvb, #0x00ff
	orr	rvb, rvb, cnt
	write	rvb, rva, #4		@ store num byte to snd in USB_COUNTn_TX
	read	rvb, rva, #0		@ rvb <- offset to USB_ADRn_TX data
	bic	rvb, rvb, #0xff000000
	bic	rvb, rvb, #0x00ff0000
	set	rva, usb_hw_buffer	@ rva <- usb hardware buffer start
	add	rva, rva, rvb		@ rva <- start adrs of Tx pkt mem for EP
	set	rvb, 0
wrtEPX:	cmp	rvb, cnt
	bpl	wrtEPY
	add	rvb, rvb, #3
	read8	rvc, dts, rvb		@ rvb <- byte 3 of next data word
	sub	rvb, rvb, #1
	read8	sv5, dts, rvb		@ sv5 <- byte 2 of next data word
	orr	rvc, sv5, rvc, lsl #8	@ rvb <- bytes 3 and 2 combined
	sub	rvb, rvb, #1
	read8	sv5, dts, rvb		@ rvb <- byte 1 of next data word
	orr	rvc, sv5, rvc, lsl #8	@ rvb <- bytes 3, 2 and 1 combined
	sub	rvb, rvb, #1
	read8	sv5, dts, rvb		@ rvb <- byte 0 of next data word
	orr	rvc, sv5, rvc, lsl #8	@ rvb <- full data word
	write	rvc, rva, rvb
	add	rvb, rvb, #4
	b	wrtEPX
  .endif
wrtEPY:	read	rvb, usb_base, env, LSL #2	@ rvb <- USB_EPnR
	and	rvc, rvb, #0x30			@ rvc <- STAT_TX bits
	eq	rvc, #0x30			@ is EPn active?
	it	eq
	seteq	pc,  lnk			@	if so, return (ok 2 go)
	bic	rvb, rvb, #0xF100
	bic	rvb, rvb, #0x00C0
	eor	rvb, rvb, #0x0030
	orr	rvb, rvb, #0x8000
	orr	rvb, rvb, #0x0080
	write	rvb, rva,      env, LSL #2	@ USB_EPnR <- EP dat VALID,txrdy
	b	wrtEPY

/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	read	rvc, usb_base, #usb_ibulkin	@ rvb <- contents of USB_EP3R
	tst	rvc, #usb_txrdy			@ is EP3 idle (or disabled)?
	bne	usbhwrc0			@	if not, return, ok 2 go
	set	rvc, rvb			@ rvc <- rvb, saved
  .ifndef usb_cntreg_32b
	write	0, usb_hw_buffer, #0x34		@ USB_COUNT3_TX <- 0 byts to snd
  .else
	rgrmw	usb_hw_buffer,#0x1c,0xffff,xxx	@ USB_COUNT3_TX <- 0 byts to snd
  .endif
	read	rvc, usb_base, #usb_ibulkin	@ rvc <- contents of USB_EP3R
	bic	rvc, rvc, #0xF100
	bic	rvc, rvc, #0x00C0
	eor	rvc, rvc, #0x0030
	orr	rvc, rvc, #0x8000
	orr	rvc, rvc, #0x0080
	write	rvc, rva, #usb_ibulkin		@ USB_EP3R <- EP dat VALID,txrdy
	set	rvb, rvc			@ rvb <- rvb, restored
	b	usbhwrc
usbhwrc0:
	swi	run_normal			@ enable interrupts (user mode)
	set	pc,  lnk			@ return


.endif	@ .ifndef connectivity_ln

/*------------------------------------------------------------------------------
@
@		Connectivity Line USB OTG (set as Device)
@
@-----------------------------------------------------------------------------*/

.ifdef connectivity_ln

/*------------------------------------------------------------------------------

		hardware configuration

------------------------------------------------------------------------------*/

_func_
usbcfg:	@ configure usb power and pins
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- rcc_base = RCC base address
	@ ret:	via lnk
  .ifndef always_init_usb			@				<TZC>
    .ifdef STM32_H107
	@ check if USB is powered (PA9 OTG_VBUS power pin), otherwise, return
	read	rvb, ioporta_base, #0x08
	tst	rvb, #(1 << 9)
	it	eq
	seteq	pc,  lnk
    .endif
    .ifdef STM32F4_Discov
	@ check if USB is powered (PA9 OTG_VBUS power pin), otherwise, return
	read	rvb, ioporta_base, #io_state
	tst	rvb, #(1 << 9)
	it	eq
	seteq	pc,  lnk
    .endif
    .ifdef STM32F429_Disco
	@ Power clock for port B
	rgcpbt	env, #0x30, 1, 1
	@ check if USB is powered (PB13 OTG_VBUS pwr pin), otherwise, return
	read	rvb, ioportb_base, #io_state
	tst	rvb, #(1 << 13)
	it	eq
	seteq	pc,  lnk
    .endif
  .endif	@ .ifndef always_init_usb
  .ifndef STM32F4
	@ enable USB clock
	rgcpbt	env, #0x14, 12, 1		@ RCC_AHBENR  <- enab USB OTG FS
  .else
	@ configure alternate function (USB OTG FS/HS) for DM and DP
    .ifdef STM32F4_Discov
	USBLOPN	= 11				@ DM and DP are pins PA 11-12
    .endif
    .ifdef STM32F429_Disco
	USBLOPN	= 14				@ DM and DP are pins PB 14-15
    .endif
	set	rvc, 0b1111 << (USBLOPN<<1)	@ rvc <- clear-mask
	set	cnt, 0b1010 << (USBLOPN<<1)	@ cnt <- set-mask
	rgrmw	rva, #0x00, rvc, cnt		@ GPIOn_MODER  <- AF for DM,DP
	rgrmw	rva, #0x08, rvc, cnt		@ GPIOn_SPEEDR <- 50 MHz
	set	rvc, 0xff<<((USBLOPN-8)<<2)	@ rvc <- AFn  clear-mask
    .ifndef USB_is_OTG_HS
	set	cnt, 0xaa<<((USBLOPN-8)<<2)	@ cnt <- AF10 set-mask
    .else
	set	cnt, 0xcc<<((USBLOPN-8)<<2)	@ cnt <- AF12 set-mask
    .endif
	rgrmw	rva, #0x24, rvc, cnt		@ GPIOn_AFRH<-AF10/AF12 OTGFS
	@ enable USB OTG FS/HS clock
    .ifndef USB_is_OTG_HS
	rgcpbt	env, #0x34,  7, 1		@ RCC_AHB2ENR <- enab USB OTG FS
    .else
	rgcpbt	env, #0x30, 29, 1		@ RCC_AHB1ENR <- enab USB OTG HS
    .endif
  .endif
	@ initialize USB device controller
	write	fre, USB_CHUNK, #0x00		@ zero bytes to send at startup
	@ configure USB device, mode, interrupts
  .ifndef USB_is_OTG_HS
	write	0x40002487, usb_base, #0x0c 	@ GUSBCFG <- dev,trdt=9:72MHz
  .else
	write	0x400024c7, usb_base, #0x0c	@ GUSBCFG <- dev,FS PHY,trdt=9
  .endif
	write	    0x81, rva, #0x08		@ GAHBCFG <- glbl ints,Txlvl=0
	write	0x041010, rva, #0x18		@ GINTMSK <- RXFLVL,IN,RESET int
	add	rva, rva, #0x0800
	rgrmw	rva, #0x00, sv3			@ OTG_FS_DCFG <- FS dev, adrs=0
	sub	rva, rva, #0x0800
	write	0x090000, rva, #0x38		@ OTG_FS_GCCFG <- pwr, VBus sens

	@ return
	set	pc,  lnk			@ return


/*------------------------------------------------------------------------------

		response to device interrupts 

------------------------------------------------------------------------------*/


_func_
usbhwReset:
	write	0x400040,usb_base,#0x24	@ GRXFSIZ  <- 64w Rx FIFO
	add	rvb, rvb, #0x40
	write	rvb, rva, #0x28		@ DIEPTXF0 <- 64w EP0 Tx FIFO, start= 64
	add	rva, rva, #0x0100	@ rva <- USB IN FIFO regs base address
	add	rvb, rvb, #0x40
	write	rvb, rva, #0x04		@ DIEPTXF1 <- 64w FIFO 1, start = 128
	add	rvb, rvb, #0x40
	write	rvb, rva, #0x08		@ DIEPTXF2 <- 64w FIFO 2, start = 192
	add	rvb, rvb, #0x40
	write	rvb, rva, #0x0c		@ DIEPTXF3 <- 64w FIFO 3, start = 256
	add	rvb, rvb, #0x40
	write	rvb, rva, #0x00		@ HPTXFSIZ <- 64w Host FIFO,start=256+64
	sub	rva, rva, #0x0100	@ rva <- USB base address (restored)
	write	0x0420, rva, #0x10	@ GRSTCTL <- flush all Tx Fifos
	rgwfbt	rva, #0x10, 31, 1	@ wait for FIFO flushed
	write	0x0010, rva, #0x10	@ GRSTCTL <- flush Rx Fifo
	rgwfbt	rva, #0x10, 31, 1	@ wait for FIFO flushed
	rgwfbt	rva, #0x10,  4, 0	@ wait for FIFO flushed, test 2
	add	rva, rva, #0x0800	@ rva <- USB device regs base address
	rgrmw	rva, #0, 0x07f0, 0x03	@ DCFG     <- Dev mod full spd, addrss 0
	write	0,      rva, #0x34	@ DIEPEMPMSK <- mask TxFIfo empty ints
	write	1,      rva, #0x1c	@ DAINTMSK <- unmask EP0 IN ints
	write	0x00,   rva, #0x10	@ DIEPMSK  <- mask global EP IN ints
	write	rvb,    rva, #0x14	@ DOEPMSK  <- mask global EP OUT ints
	write	(1<<29)|(1<<19)|8,rva,#0x0310 @ DOEPTSIZ0 <- EP0 OUT 3 setup pkt
	write	(1<<31)|(1<<27)|3,rva,#0x100 @ DIEPCTL0  <- EP0 IN 8 byte packet
	write	(1<<31)|(1<<26)|3,rva,#0x300 @ DOEPCTL0  <- enab EP0 OUT for Rx
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwEndpointStatus: @ get status of EP into sv2 and sv3 (sv1 is device interrupt from usb_istr)
	@ on entry:	sv1 <- GINTSTS
	@ on exit:	sv2 <- DAINT
	@ on exit:	sv3 <- DIEPINTx / DOEPINTx
	@ side-effect:	clears EP interrupt
	@ modifies:	rva, rvb, sv2, sv3
	tst	sv1, #0x1000		@ is interrupt a reset, mixed w/EP ints?
	bne	usbDSi			@	if so,  jump back to dev int
	read	sv2,usb_base+0x800,#0x18 @ sv2 <- DAINT (EP interrupts)
	read	rvb,rva,           #0x1c @ rvb <- DAINTMSK (allowed EP ints)
	and	sv2, sv2, rvb		@ sv2 <- allowed, active EP ints
	eq	sv2, #0			@ any DAINT signalled?
	beq	usbhwEPstd		@	if not, jump to Bulk Rx or Ctrl
	read	rvb, rva, #0x08		@ rvb <- DSTS (must read on DAINT int)
	add	rva, rva, #0x0100	@ rva <- USB base address + 0x900
	tst	sv2, #usbCO_ibit	@ is interrupt for Control OUT EP 0?
	it	ne
	addne	rva, rva, #0x0200	@ 	if so,  rva <- USB base + 0xb00
	bne	usbhwEPstc		@	if so,  jump to finish up
	tst	sv2, #usbBO_ibit	@ is interrupt for Bulk Out EP 2?
	it	ne
	addne	rva, rva, #0x0240	@ 	if so,  rva <- USB base + 0xb40
	bne	usbhwEPstc		@	if so,  jump to finish up
	tst	sv2, #usbBI_ibit	@ is interrupt for Bulk IN EP 3?
	it	ne
	addne	rva, rva, #0x60		@ 	if so,  rva <- USB base + 0x960
usbhwEPstc: @ continue
	read	sv3, rva, #0x08		@ sv3 <- DIEPINTx/DOEPINTx EP int stat
	write	sv3, rva, #0x08		@ clear EP int
	write	0,usb_base+0x800,#0x10	@ DIEPMSK <- re-mask all EP IN ints
	set	pc,  lnk

usbhwEPstd: @ non-DAINT Bulk Rx or Control Rx/Tx interrupt
	read	rvb, usb_base, #0x1c	@ rvb <- GRXSTSR (peek Rx FIFO)
	tst	sv1, #0x10		@ is this a Rx interrupt (vs Tx)
	itE	eq
	addeq	rva, rva, #0x0900	@ 	if not, rva <- IN  EP base addrs
	addne	rva, rva, #0x0b00	@	if so,  rva <- OUT EP base addrs
	tst	rvb, #3			@ int is for control EP?
	itTEE	eq
	readeq	sv2, rva, #0x08		@ 	if so,  sv2 <- DIEPINT0/DOEPINT0
	writeeq	sv2, rva, #0x08		@	if so,  clear control int
	readne	sv2, rva, #0x48		@ 	if not, sv2 <- DOEPINT2 Bulk OUT
	writene	sv2, rva, #0x48		@	if not, clear Bulk OUT int
	tst	sv1, #0x10		@ is this a Rx interrupt (vs Tx)
	bne	usbhwEPstdRx
	b	usbEPx			@ exit on Control IN int

usbhwEPstdRx: @ non-DAINT Control OUT or Bulk OUT interrupt
	tst	rvb, #0x0f		@ EP0 ?
	itE	eq
	seteq	sv2, usbCO_ibit		@	if so,  sv2 <- Ctrl OUT indic
	setne	sv2, usbBO_ibit		@	if not, sv2 <- Bulk OUT indic
	lsr	rvb, rvb, #17		@ rvb <- packet info, shifted
	and	rvb, rvb, #0x0f		@ rvb <- packet info, masked
	set	sv3, 0			@ sv3 <- 0 (non-SETUP packet indicator)
	eq	rvb, #0x06		@ SETUP received?
	itE	eq
	seteq	sv3, usbCO_setupbit	@	if so,  sv3 <- set SETUP bit
	eqne	rvb, #0x02		@ 	if not, OUT packet received?
	it	eq
	seteq	pc,  lnk		@	if so,  return for normal proc
	@ other (global OUT NAK, OUT complete, SETUP complete, reserved)
	set	sv2, 0			@ sv2 <- 0 (cleared, nothing to do)
	read	rvb, usb_base, #0x20	@ rvb <- GRXSTSP (pop Rx FIFO)
	set	pc,  lnk		@ return for isr EP exit

/* BULK IN Enpoint Interrupt Response */


/* BULK OUT Enpoint Interrupt Response */

_func_
usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb, rvc
	@ returns via:	lnk
	rgcpbt	usb_base+0x800, #16,0,1	@ DIEPMSK <- unmask TxComp int
	add	rva, rva, #0x0100	@ rva <- IN EP regs base address
	read	rvc, rva, #0x60		@ rvc <- DIEPCTL3
	tst	rvc, #(1 << 31)		@ is EP enabled (waiting to send)?
	it	ne
	setne	pc,  lnk		@	if so,  return	
	write	1<<19, rva, #0x70	@ DIEPTSIZ3 <- zero bytes to snd for pkt
	rgrmw	rva, #0x60,(1<<15)|(1<<31)|(1<<26) @ DIEPCTL3  <- enab EP for Tx
	set	pc,  lnk		@ return

/* CONTROL IN Enpoint Interrupt Response */


/* CONTROL OUT Enpoint Interrupt Response */

_func_
usbhwSetup: @ Control OUT Interrupt, Setup Phase
	set	sv3, lnk
	bl	rdEP
	set	lnk, sv3
	write	0, usb_base+0x800, #4	@ DCTL <- clr glbl OUT NAK, set prg done
	set	pc,  lnk		@ setup packet read in hwstatus above

_func_
usbhwSetAddress: @ Set Device to Address in sv5
	@ USB Status IN  exit -- write null packet to   EP 1 (phys, aka 0x80)
	@ disable the correct transfer interrupt mask (CTRM) in USB_CNTR
	read	sv5,USB_SETUP_BUFFER,#0	@ sv5 <- reqtyp(8), request(8), val(16)
	lsr	sv5, sv5, #16		@ sv5 <- address = val(16)
	lsl	sv5, sv5, #4
	read	rvb,usb_base,#usb_daddr	@ rvb <- address reg content
	bic	rvb, rvb, #0x07f0
	orr	rvb, rvb, sv5
	write	rvb, rva,    #usb_daddr	@ set address
	b	usbSIx

_func_
usbhwConfigure: @ Configure the device
	@ configure USB
	set	rva, usb_base+0x0900	@ rva <- IN EP regs base address
	write	(1<<22)|(3<<18)|(1<<15)|8, rva,#0x20 @ DIEPCTL1 <- FIFO 1,Int IN
	write	(2<<22)|(2<<18)|(1<<15)|64,rva,#0x60 @ DIEPCTL3 <- FIFO 3,BulkIN
	add	rva, rva, #0x0200	@ rva <- OUT EP regs base address
	write	64, rva, #0x50		@ DOEPTSIZ2 <- EP2 pktsz=64byts,exp 1pkt
	write	(1<<31)|(1<<28)|(2<<18)|(1<<15)|64,rva,#0x40 @ DOEPCTL2 <- activ
	rgrmw	usb_base+0x800,#0x1c,0x4000a @ DAINTMSK <- unmsk int EP 1i,2o,3i
	write	0xa0, usb_base, #0x10	@ GRSTCTL <- flush selected Tx Fifo(s)
	set	pc,  lnk

/* Status IN/OUT responses */


/* Enpoint stalling, unstalling */

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	rgcpbt	usb_base, #0x0900, 21,1	@ DIEPCTL0 <- send stall
	b	usbEPx

/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

_func_
rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	and	env, env, #0x0F
	read	rvb, usb_base, #0x20	@ rvb <- GRXSTSP (pop Rx FIFO)
	lsr	cnt, rvb, #4
	and	cnt, cnt, #0xff		@ cnt <- received byte count (up to 256)
	set	rvb, 0			@ rvb <- initial offset in dest buffer
usbSEZ:	cmp	rvb, cnt		@ done getting data?
	bpl	usbSEX
	read	rvc, rva, #0x20		@ rvc <- word from Rx FIFO
	write	rvc, dts, rvb		@ store it in buffer
	add	rvb, rvb, #4		@ rvb <- updated destination offset
	b	usbSEZ
usbSEX:	read	rvb, rva, #0x14		@ rvb <- GINTSTS
	tst	rvb, #0x10
	it	ne
	readne	rvb, rva, #0x20		@ rvb <- GRXSTSP (pop Rx FIFO) -- clear
	bne	usbSEX
	add	rva, rva, #0x0b00	@ rva <- DOEPCTL0 EP0 OUT (base address)
	add	rva, rva, env, LSL #5	@ rva <- DOEPCTLx EP0 OUT (base address)
	eq	env, #0
	itTE	eq
	seteq	rvb, 1<<29
	orreq	rvb, rvb, #8
	setne	rvb, 64
	write	rvb, rva, #0x10		@ DOEPTSIZx <- num bytes to read for pkt
	eq	env, #0
	itEE	eq
	seteq	rvb, 3
	setne	rvb, 64
	orrne	rvb, rvb, #(2 << 18)
	orr	rvb, rvb, #(1 << 15)
	orr	rvb, rvb, #(1 << 31)
	orr	rvb, rvb, #(1 << 26)	@ rvb <- clear NAK generation
	write	rvb, rva, #0		@ DOEPCTLx  <- enable EP for Rx
	set	pc,  lnk

_func_
wrtEP:	@ write data to Control In Endpoint (cnt bytes from dts to EP in env)
wrtEPU:	@ write data to Control In Endpoint (cnt bytes from dts to EP in env)
	and	env, env, #0x0F
	set	rva, usb_base+0x900	@ rva <- IN EP regs base address
	add	rva, rva, env, LSL #5	@ rva <- DIEPCTLx address for EP
	eq	env, #0
	beq	wrtEw1
	rgwfbt	rva, #0x00, 31, 0 	@ wait for EP active in DIEPCTLx
wrtEw1:	orr	rvb, cnt, #(1<<19)
	write	rvb, rva, #0x10		@ DIEPTSIZx <- num bytes for pkt
	rgcpbt	rva, #0x08, 0, 1	@ DIEPINTx  <- clear Tx interrupt on EP
	read	rvb, rva, #0		@ rvb <- DIEPCTLx
	orr	rvb, rvb, #(1 << 15)	@ rvb <- EP active bit
	orr	rvb, rvb, #(1 << 31)	@ rvb <- enable EP bit
	orr	rvb, rvb, #(1 << 26)	@ rvb <- clear NAK generation
	write	rvb, rva, #0		@ DIEPCTLx  <- enable EP for Tx
	set	rva, usb_base+0x1000	@ rva <- address of EP0 FIFO
	add	rva, rva, env, lsl #12	@ rva <- address of EPx FIFO
	set	rvb, 0
wrtEPX:	cmp	rvb, cnt
	bpl	wrtEPY
	read	rvc, dts, rvb
	write	rvc, rva, #0
	add	rvb, rvb, #4
	b	wrtEPX
wrtEPY:	eq	env, #0
	it	ne
	setne	pc,  lnk
	@ EP0
	eq	cnt, #0
	it	eq
	seteq	pc,  lnk
	read	rvb, USB_CHUNK, #0	@ rvb <- how many bytes remain to send
	eq	rvb, #0			@ end of transfer?
	read	rvb,usb_base+0x800,#0x34 @ rvb <- DIEPEMPMSK
	itE	eq
	biceq	rvb, rvb, #1		@ 	if so,  rvb <- mask   EP0 bit
	orrne	rvb, rvb, #1		@	if not, rvb <- unmask EP0 bit
	write	rvb, rva, #0x34		@ DIEPEMPMSK <- un/mask EP0 Tx empty int
	set	pc,  lnk

/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	read	rvc, usb_base+0x800, #0x10   @ rvc <- DIEPMSK
	orr	rvc, rvc, #0x01		     @ rvc <- DIEPMSK + bit for TxComp
	write	rvc, rva,            #0x10   @ DIEPMSK <- unmask TxComp int
	read	rvc, usb_base+0x900, #0x60   @ rvc <- DIEPCTL3
	tst	rvc, #(1 << 31)		     @ is EP enabled (waiting to send)?
	bne	usbhwrcxt		     @	if so,  jump to return
	set	rvc, 1<<19
	write	rvc, rva, #0x70		     @ DIEPTSIZ3 <- 0 bytes pkt to send
	read	rvc, rva, #0x60		     @ rvc <- DIEPCTL3
	orr	rvc, rvc, #(1 << 15)	     @ rvc <- EP active bit
	orr	rvc, rvc, #((1<<31)|(1<<26)) @ rvc <- enab EP bit, clr NAK gen
	write	rvc, rva, #0x60		     @ DIEPCTL3  <- enab EP for Tx
usbhwrcxt: @ finish-up
	swi	run_normal		     @ enable interrupts (user mode)
	set	pc,  lnk		     @ return


.endif	@ .ifdef connectivity_ln



