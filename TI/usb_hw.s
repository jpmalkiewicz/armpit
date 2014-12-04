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

		hardware configuration

------------------------------------------------------------------------------*/

_func_
usbcfg:	@ configure usb power and pins
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
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
	@ operational parameters
	write	fre, USB_CHUNK, #0x00	@ zero bytes remain to send at startup
  .ifdef USB_FSHS_MODE
	write	fre, USB_FSHS_MODE,#0	@ indic that USB is not yet in HS mode
  .endif
  .ifdef LM_3S1000
	@ configure clocks and pins
	rgrmw	env, #0x08, (1<<16)|(1<<1) @ RCGC2    <- enab USB0 & port B clk
	wait	2			@ wait a bit for power
	rgcpbt	ioportb_base+0x500, #0x1c, 0, 1	@ GPIODEN <- USBPB1=anlg,PB0=dig
	rgcpbt	rva, #0x28, 1, 1	@ GPIOAMSEL  <- analog for USB pin PB0,1
	sub	rva, rva, #0x100
	write	sv1, rva, #0x00		@ GPIODIR  <- set PB0 to output (USB0ID)
	sub	rva, rva, #0x400
	write	0xff, rva, #(1<<(0+2))	@ set PB0 high (device mode)
  .endif
  .ifdef LM_4Fxxx
    .ifdef EK_LM4F120
	@ configure clocks and pins
	rgcpbt	env, #0x08, 3, 1	@ RCGCGPIO <- enab portD, DM,DP,VBUS
	rgwfbt	glv, #0x08, 3, 1	@  wait for portD bit set in PR_GPIO
	rgrmw	ioportd_base+0x500, #0x28, 3<<4 @ GPIOAMSEL <- analg USB pins
    .endif @ EK_LM4F120
    .ifdef EK_LM4F232
	@ configure clocks and pins
	rgrmw	env, #0x08, (1<<10)|(1<<1)	@ RCGCGPIO <- enab port B,L clk
	rgwfbm	glv, #0x08, (1<<10)|(1<<1)	@ wait for all periph clocked
	rgrmw	ioportb_base+0x500, #0x28, 3	@ GPIOAMSEL <- analg USB pins
	rgrmw	ioportl_base+0x500, #0x28, 3<<6 @ GPIOAMSEL <- analg USB pins
    .endif @ EK_LM4F232
    .ifdef EK_TM4C1294
	@ configure clocks and pins
	rgrmw	env, #0x08, (1<<10)|(1<<1)	@ RCGCGPIO <- enab port B,L clk
	rgwfbm	glv, #0x08, (1<<10)|(1<<1)	@ wait for periph ready bits
	rgrmw	ioportb_base+0x500, #0x28, 3	@ GPIOAMSEL <- analg USB pins
	rgrmw	ioportl_base+0x500, #0x28, 3<<6 @ GPIOAMSEL <- analg USB pins
    .endif @ EK_TM4C1294
	@ power-up USB
	write	sv1, env, #0x28		@ RCGCUSB  <- enable clock for USB
	rgwfbt	glv, #0x28, 0, 1	@ wait for USB bit set in PR_USB
  .endif @ LM_4Fxxx
  .ifdef cortex
	@ configure peripheral (default on reset is device mode)
    .ifdef LM_3S1000
	write	sv3, usb_base+0x0400, #0x1c	@ USBGPCS  <- set device mode
    .endif
    .ifdef EK_TM4C1294
	write	0x0207, usb_base+0xf00, #0xc8	@ USBCC  <- CLKDIV=8 = 480/60MHz
    .endif
	write	sv2, usb_base, #0x0e	@ USBEPIDX       <- select EP2
	write8	sv3, rva, #0x63		@ USB_Rx_FIFOSZ  <- 64 bytes for EP2 Rx
	write16	  8, rva, #0x66		@ USB_Rx_FIFOADD <- EP2 Rx FIFO adrs=64
	write	sv3, rva, #0x0e		@ USBEPIDX       <- select EP3
	write8	sv3, rva, #0x62		@ USB_Tx_FIFOSZ  <- 64 bytes for EP3 Tx
	write16	 16, rva, #0x64		@ USB_Tx_FIFOADD <- EP3 Tx FIFO adrs=128
  .endif	@ cortex-M3/M4

  .ifdef cortex_a8	/* cortex-a8 and cortex-a9 */
    .ifdef AM335x
	@ enable USB module clock and configure pull-up
	swi	run_prvlgd		@ set Thread mode, privileged, no IRQ
	write	0x100, glv, #0x7c	@ CM_CLKDCOLDO_DPLL_PER <- enable
	write	  sv2, env, #0x1c	@ CM_PER_USB0_CLKCTRL <- enable
	write	0x3c180000, dts, #0x620	@ usb_ctrl0     <- pull-up on DP, etc...
	swi	run_no_irq		@ set Thread mode, unprvlgd, no IRQ, usr
	@ reset USB module
	write	sv1, usb_base-0x0400, #0x14 @ USB0CTRL <- reset usb otg
	rgwfbt	rva, #0x14, 0, 0	@ wait for reset complete (bit 0)
	@ set device mode, legacy ints, enable USB int
	write	 0x08, rva, #0x14	@ USB0CTRL <- UINT, use legacy ints
	write	 1<<9, rva, #0x3c	@ USB0IRQENABLESET1 <- enab lgcy USB_INT
	write	 3<<7, rva, #0xe8	@ USB0MODE <- B-type
	write	1<<18, int_base, #0x88	@ INTC_MIR_CLEAR0 <- enable ints/unmask
    .else
	@ save return address
	set	sv5, lnk		@ sv5 <- lnk, saved
     .ifdef cortex_a9	@ OMAP4
	@ configure usb dpll
	ldr	rva, =0x4A008180	@ rva <- CM_CLKMODE_DPLL_USB
	bl	pllbyp			@ bypass the USB DPLL
	write	fre, rva, #0x08		@ CM_AUTOIDLE_DPLL_USB <- no idle
	write	(400<<8)|15, rva, #0x0c	@ CM_CLKSEL_DPLL_USB <- M=400, N=15
	write	0x0102, rva, #0x10	@ CM_DIV_M2_DPLL_USB <- M2=2, no idle
	bl	plllok			@ lock the USB DPLL
	@ config USB OTG DM/DP pads (p.4143)  (use: glv <- SYSCTRL_PADCONF_CORE)
	rgrmw	glv, #0x194, 0xff0000, xxx @ _PAD0_USBA0_OTG_...<- mux=0,no-pud
	rgrmw	glv, #0x198,     0xff, xxx @ _PAD1_USBA0_OTG_DP <- mux=0,no-pud
	@ power-up OTG module and PHY (use: env <- L3INIT_CM2 base)
	write	sv1,   env, #0x60	@ CM_L3INIT_HSUSBOTG_CLKCTRL <- enab OTG
	write	0x101, env, #0xe0	@ CM_L3INIT_USBPHY_CLKCTRL <- enable PHY
	@ reset OTG and choose embedded interface, UTMI+
	write	sv2, usb_base+0x400, #4	@ OTG_SYSCONFIG <- reset otg
	rgwfbt	rva, #0x08, 0, 1	@ wait for reset done in OTG_SYSSTATUS
	write	0x1008, rva, #0x04	@ OTG_SYSCONFIG <- run with no idle clk
	write	fre,    rva, #0x0C	@ OTG_INTERFSEL  <- 8-bit UTMI+ embedded
	write	fre,    rva, #0x14	@ OTG_FORCESTDBY <- de-assert standby
	@ power-up vdda_usba0otg_3p3v in TWL6030
	ldr	rva, =i2c0_base		@ rva <- I2C0 base address
	hwi2wr	0xe1a248, i2		@ VUSB_CFG_STATE(#xa2) <- allgrp on #xe1
	hwi2wr	0x10e548, i2		@ MISC2(#xe5) <- VUSBsrc=VDDB3/VBAT #x10
	@ set session valid bits in OTG control reg
	write	0x17, 0x4A00233C, #0x00	   @ CONTROL_USBOTGHS_CONTROL <- AB/VBUS
	@ enable usb int 92 (2*32+28+32) in interrupt controller for cpu-0 only
	write	1<<(92%32), 0x48241100, #0 @ GICD_ISENABLER/ICDISER-3  <- int 92
	write	sv1,0x48241800,#(92+32)	   @ GICD_ITARGETSR/ICDIPTR-92 <- cpu0
     .else @  OMAP3 (i.e. not cortex_a9)
	@ enable HSOTGUSB interface clock (use: dts<-CM_CORE)
	rgcpbt	dts, #I_clck, 4, 1	@ cm_iclken_core <- enable HSOTGUSB Iclk
      .ifdef TI_Beagle_XM
	@ configure USB pads to mode 0
	ldr	rva, =SCM_base+0x0100
	set	rvc, 0x9c
usbpcf:	add	rvc, rvc, #4
	ldr	rvb, [rva, rvc]		@rvb <-  CONTROL_PADCONF_HSUSB0
	cmp	rvc, #0xa4
	bicpl	rvb, rvb, #(7<<0)
	cmp	rvc, #0xb8
	bicmi	rvb, rvb, #(7<<16)
	str	rvb, [rva, rvc]
	bmi	usbpcf
	@ reset
	write	sv2, usb_base+0x400, #4	@ reset usbotg, no clk auto-gtng
	rgwfbt	rva, #0x08, 0, 1	@ wait for reset done bit
	write	0x1008, rva, #0x04	@ run with no idle clock
	write	0x0000, rva, #0x14	@ de-assert standby
	@ power-up USB-OTG PHY in TPS69950/TWL4030 (DM3730 / beagleBoard-XM)
	ldr	rva, =i2c1_base		@ rva <- I2C1 base address
	hwi2wr	0x00d94b, i2		@ vusb_dedicated2(#xd9) <- clr sleep v=0
	hwi2wr	0xe0cc4b, i2		@ vusb1v5_dev_grp(#xcc) <- all grps #xe0
	hwi2wr	0xe0cf4b, i2		@ vusb1v8_dev_grp(#xcf) <- all grps #xe0
usbplw:	@ wait for USB-OTG PHY PLL lock
	wait	1 << 24			@ wait a bit
	hwi2rd	0x00ff48, i1		@ rvc <- PHY_CLK_CTRL_STS(#xff)
	tst	rvc, #0x01		@ is PLL locked?
	beq	usbplw			@	if not, jump to keep waiting
	@ enable USB-OTG power
	hwi2wr	0x20ac48, i2		@ POWER_CTRL(#xac) <- OTG_EN #x20
      .endif
	@ configure interrupts and peripheral (USB int 92)
	write	1<<(92%32), int_base, #(0xc0+(92/32)<<2) @ INTMSK <-enab int 92
     .endif	@ OMAP3 vs OMAP4
	@ restore values
	set	lnk, sv5		@ lnk <- lnk, restored
	set	sv5, 5			@ sv5 <- 5,   restored
    .endif	@ AM335x vs OMAP
  .endif	@ cortex_a8
	@ enable interrupts in USB subsystem
	write16	0xffff, usb_base, #0x06	@ INTRTXE  <- enab EP0-15 transmit ints
	write16	0xfffe, rva, #0x08	@ INTRRXE  <- enab EP1-15 receive  ints
	write8	   sv4, rva, #0x0b	@ INTRUSBE <- enab reset interrupt
	@ connect
  .ifndef has_HS_USB
	write8	0x40, rva, #0x01	@ POWER    <- set softcon
  .else
	write8	0x60, rva, #0x01	@ POWER    <- set softcon and HSEN
  .endif

	/* return */
	set	pc,  lnk		@ return


/*------------------------------------------------------------------------------

		response to device interrupts

	(Note: macros usbldr and usbstr are defined in device_family.h)

------------------------------------------------------------------------------*/

_func_
usbhwgetDevEPint: @ get EP int statuts into sv1
	@ on entry:	rva <- USB base address
	@ on exit:	sv1 <- interrupt status w/r EP and/or Device
	read	sv1, rva, #usb_istat_dv
	lsr	sv1, sv1, #16		@ sv1 <- USBTXIS shftd frm USBFADDR+TXIS
	read16	sv2, rva, #0x04		@ sv2 <- USBRXIS
	orr	sv1, sv1, sv2		@ sv1 <- TX and RX EP ints combined
	set	pc,  lnk

_func_
usbhwgetDevint: @ get Device int statuts into sv1
	@ on entry:	rva <- USB base address
	@ on exit:	sv1 <- interrupt status for Device
	read8	sv1, rva,#usb_istat_dv2	@ sv1 <- Dev Int Stat = dwStatus
	set	pc,  lnk

_func_
usbhwDeviceStatus: @ return device status in rvb
	read8	rvb, usb_base, #0x0a	@ rvb <- USBIS interrupt status
	set	pc,  lnk

_func_
usbhwReset:
	write	0, USB_SETUP_BUFFER, #0	@ clear the setup buffer
  .ifdef USB_FSHS_MODE
	read8	rvb, usb_base, #0x01
	tst	rvb, #0x10
	seteq	rvb, 0			@ rvb <- 0
	setne	rvb, 1
	write	rvb, USB_FSHS_MODE, #0	@ indicate that USB is not in HS mode
  .endif
	set	pc,  lnk


/*------------------------------------------------------------------------------

		response to endpoint interrupts 

------------------------------------------------------------------------------*/

_func_
usbhwEndpointStatus: @ get status of EP whose interrupt is in sv2 into sv3
	set	rva, usb_base
	set	sv2, sv1		@ sv2 <- Endpoint Int Stat (shifted)
	set	sv3, sv2
	tst	sv2, #1			@ EP0 (control) interrupt?
	it	eq
	seteq	pc,  lnk		@	if not, return w/int in sv2, sv3
	set	sv3, lnk
	set	env, 0
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	set	lnk, sv3
	usbldr	sv3, rva, usb_ctl_stat
	tst	sv3, #0x10
	itTTT	ne
	setne	sv2, 0
	setne	rvb, 0x80
	usbstrne rvb, rva, usb_ctl_stat	@ set SERVICED_SETUP_END in EP0_CSR
	setne	pc,  lnk
	tst	sv3, #0x04		@ responding to sent_stall?
	itTT	ne
	setne	sv2, 0
	usbstrne sv2, rva, usb_ctl_stat
	setne	pc,  lnk
	tst	sv3, #0x11		@ is OUT_PKT_RDY or SETUP_END set?
	itE	eq
	seteq	sv2, 2			@	if not, sv2 <- control In indic
	setne	sv2, 1			@	if so,  sv2 <- control out indic
	mvn	sv3, sv3
	set	pc,  lnk

/* BULK IN Enpoint Interrupt Response */


/* BULK OUT Enpoint Interrupt Response */

_func_
usbhwBOw: @ initiate input data echo (if needed)
	@ modifies:	rva, rvb, rvc
	@ returns via:	usbixt (direct exit)
	b	usbBIi			@ jump to write OUT data to IN EP

/* CONTROL IN Enpoint Interrupt Response */

_func_
usbhwCIw:  @ Control IN EP interrupt response
	read	rva, USB_SETUP_BUFFER, #0
	eq	rva, #0xff
	beq	usbEPx
	b	wrtEP

/* CONTROL OUT Enpoint Interrupt Response */

_func_
usbhwSetup: @ see also rdEP, here, env = 0
	@ on entry:	env <- Control OUT EndPoint (0)
	@ on entry:	dts <- Setup_buffer
	@ on exit:	setup packet data is loaded into setup_buffer
	@ modifies:	rva, rvb, rvc, sv3, cnt
	set	sv3, lnk		@ sv3 <- lnk, saved against bl
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	bl	usbhwReadEP2Buf
	set	lnk, sv3		@ lnk <- lnk, restored
	set	rvb, 0x40		@ rvb <- SERVICED_OUT_PKT_RDY 
	usbstr	rvb, rva, usb_ctl_stat	@ clear OUT_PKT_RDY bit in EP0_CSR
	read	rvb,USB_SETUP_BUFFER,#0	@ rvb <- reqtyp(8), request(8), val(16)
	tst	rvb, #0x80		@ is direction from device to host?
	it	ne
	setne	pc,  lnk		@	if so,  return
	read	sv5, rva, #4		@ sv5 <- index(16), length(16)
	lsrs	cnt, sv5, #16		@ cnt <- length of data to transfer byts
	it	eq
	seteq	pc,  lnk		@ may need to set data-end too here(?)
	set	sv3, usb_base
usbewt:	read16	rvc, sv3, #0x02		@ rvc <- USBTxIS
	tst	rvc, #1
	beq	usbewt
	usbldr	rvc, sv3, usb_ctl_stat	@ 
	@ here:		rva <- USB_SETUP_BUFFER
	@ here:		rvb <- reqtyp(8l), request(8h), val(16H)
	@ here:		sv5 <- index(16L), length(16H)
	@ here:		cnt <- num bytes to transfer (length)
	b	usbRQS

_func_
usbhwDGD: @ Get Descriptor of Device Standard request	
	@ on entry:	env <- Control IN EP
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	bl	usbhwWriteBuf2EP	@ write from buffer to IN EP
@	set	rvb, 0x02		@ rvb <- IN_PKT_RDY bit for EP0 
	usbstr	0x02, rva, usb_ctl_stat	@ set IN_PKT_RDY bit in EP0_CSR
	b	usbEPx

_func_
usbhwSetAddress: @ Set Device to Address in rvb
	set	sv5, rvb		@ sv5 <- address to set (value of reqst)
	set	env, UsbControlInEP		@ env <- Control IN EndPoint
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
@	set	rvb, 0x08		@	if so,  rvb <- PKT_RDY, DATA_END
	usbstr	0x08, rva, usb_ctl_stat	@ set IN_PKT_RDY bit in EP0_CSR
	r16wfbt	rva, #0x02, 0, 1	@ wait for control IN EP int asserted
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	usbldr	rvc, rva, usb_ctl_stat	@ 
	write8	sv5, rva, #usb_daddr	@ set address
	b	usbEPx

_func_
usbhwConfigure: @ Configure the device
	@ configure USB
  .ifdef usb_index_reg
	set	rvc, lnk
	set	env, UsbBulkOutEP
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	write16	0,   rva, #0x12		@ set direction to OUT in IN_CSR2_REG
	write16	rvb, rva, #0x16		@ set direction to OUT in IN_CSR2_REG
	set	rvb, USB_FSHS_MODE
	read	rvb, rvb, #0
	eq	rvb, #0
	seteq	rvb, 64
	setne	rvb, 512
	write16	rvb, rva, #0x10		@ set packet size to 64 bytes in MAXP_REG
	write16	rvb, rva, #0x14		@ set packet size to 64 bytes in MAXP_REG
	set	env, UsbBulkInEP
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	set	lnk, rvc
	write16	0x2000, rva, #0x12	@ set direction to IN in IN_CSR2_REG
	set	rvb, USB_FSHS_MODE
	read	rvb, rvb, #0
	eq	rvb, #0
	seteq	rvb, 64
	setne	rvb, 512
	write16	rvb, rva, #0x10		@ set packet size to 64 bytes in MAXP_REG
	write16	rvb, rva, #0x14		@ set packet size to 64 bytes in MAXP_REG
  .else
	write16	0,   usb_base, #0x122	@ set direction to OUT in USB_Tx_CSRL2
	write16	rvb, rva,      #0x126	@ clear USB_Rx_CSRL2
	write16	64,  rva,      #0x120	@ set pkt siz to 64 byts in USB_Tx_MAXP2
	write16	rvb, rva,      #0x124	@ set pkt siz to 64 byts in USB_Rx_MAXP2
	write16	0x2000, rva,   #0x132	@ set direction to IN in USB_Tx_CSRL3
	write16	64,  rva,      #0x130	@ set pkt siz to 64 byts in USB_Tx_MAXP3
	write16	rvb, rva,      #0x134	@ set pkt siz to 64 byts in USB_Rx_MAXP3
  .endif
	set	pc,  lnk

/* Status IN/OUT responses */

_func_
usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	set	env, UsbControlOutEP	@ env <- Control OUT EndPoint	
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
@	set	rvb, 0x80		@ rvb <- SERVICED_SETUP_END
	usbstr	0x80, rva, usb_ctl_stat	@ set SERVICED_SETUP_END in EP0_CSR
	b	usbEPx

_func_
usbhwSIX: @ status IN exit
	write	0xFF,USB_SETUP_BUFFER,#0
	write	0,   USB_CHUNK, #0	@ cnt <- 0 bytes remain to be sent	
	set	env, UsbControlInEP	@ env <- Control IN EndPoint
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
@	set	rvb, 0x08		@ rvb <- IN_PKT_RDY
	usbstr	0x08, rva, usb_ctl_stat	@ set IN_PKT_RDY bit in EP0_CSR
	r16wfbt	rva, #0x02, 0, 1
	usbldr	rvc, rva, usb_ctl_stat	@ set IN_PKT_RDY bit in EP0_CSR
	b	usbEPx

/* Enpoint stalling, unstalling */

_func_
usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80) -- i.e. Control IN
	set	env, UsbControlOutEP	@ env <- Control OUT EndPoint
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
@	set	rvb, 0x68
	usbstr	0x68, rva, usb_ctl_stat	@ set SERVICED_OUT_PKT_RDY & SEND_STALL	
	b	usbEPx

/*------------------------------------------------------------------------------

		common functions for response to endpoint interrupts:
		read, write and helper functions

------------------------------------------------------------------------------*/

_func_
rdEP:	@ read from endpoint in env to buffer in dts with count in cnt
	set	sv5, lnk
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	bl	usbhwReadEP2Buf
	set	lnk, sv5
	cmp	env, #2
	bpl	rdEPn
	@ EP0 post-read processing
	@ assumes payload is always 8-byte or less for cntrl EP (sets dataend)
@	set	rvb, 0x48
	usbstr	0x48, rva, usb_ctl_stat	@ set SERVICED_OUTPKT_RDY in EP0_CSR
	set	pc,  lnk
rdEPn:	@ BUlk OUT EP post-read processing
@	set	rvb, 0
	usbstr	0x00, rva, usb_ibulkout	@ clear OUT_PKT_RDY bit in OUT_CSR2_REG
	set	pc,  lnk

_func_
wrtEPU:	@ clear EP rcv interrupt then write data to Bulk In Endpoint
	@ (write cnt bytes starting at dts to enpoint in env)
wrtEP:	@ write data to Control IN or Bulk IN EP
	@ on entry:	cnt <- number of bytes to write
	@ on entry:	dts <- start address of data in buffer
	@ on entry:	env <- EP to write to
	set	sv5, lnk		@ sv5 <- lnk, saved
	bl	usbhwSelectEP		@ rva <- usb_base, selct EP in INDEX_REG
	bl	usbhwWriteBuf2EP	@ write from buffer to IN EP
	set	lnk, sv5		@ lnk <- lnk, restored
	cmp	env, #2			@ writing to control EP?
	bpl	wrtEPn			@	if not, jump to process Bulk EP
	@ EP0 post-write processing
	set	rvb, USB_CHUNK
	read	rvb, rvb, #0		@ rvb <- how many bytes remain to be sent
	eq	rvb, #0			@ more bytes to send after this?
	itE	eq
	seteq	rvb, 0x0a		@ 	if not, rvb <- PKT_RDY + DATAEND
	setne	rvb, 0x02		@	if so,  rvb <- IN_PKT_RDY
	usbstr	rvb, rva, usb_ctl_stat	@ update EP0_CSR
	beq	usbSOx			@	if not, goto Stat OUT (last pkt)
	set	pc,  lnk		@ return

wrtEPn:	@ Bulk IN post-write processing
	usbldr	rvb, rva, usb_ibulkin	@ rvb <- TxCSLR3
	tst	rvb, #usb_txrdy		@ EP ready to send?
	it	ne
	setne	pc,  lnk		@ 	if so,  return
	set	rvb, usb_txrdy		@ rvb <- IN_PKT_RDY bit
	usbstr	rvb, rva, usb_ibulkin	@ set PKT_RDY bit in TXCSR
	b	wrtEPn		 	@ jump to wait for EP TxRdy

/* helper functions */

_func_
usbhwSelectEP: @ select EP in INDEX_REG
	@ on entry:	env <- EP to select
	@ on exit:	rva <- usb base address
	@ modifies:	rva, rvb
	set	rva, usb_base		@ rva <- usb_base
  .ifdef usb_index_reg
	write8	env, rva,#usb_index_reg	@ select endpoint in INDEX_REG
usbslw:	read8	rvb, rva,#usb_index_reg	@ rvb <- selected EP
	eq	rvb, env		@ is it the desired EP?
	bne	usbslw			@	if not, jump back to wait
  .endif
	set	pc,  lnk		@ return

_func_
usbhwReadEP2Buf: @ read from OUT EP into buffer
	@ on entry:	env <- EP to read from (selected already)
	@ on entry:	rva <- usb base address
	@ on exit:	cnt <- number of bytes read
	@ modifies:	rvb, rvc, cnt
  .ifdef usb_index_reg
	read16	cnt, rva, #usb_rcvd_cnt	@ cnt <- byte count from OUT_FIFO_CNT1_REG
  .else
	eq	env, #0
	itE	eq
	read8eq	 cnt, rva, #0x108	@ cnt <- byte count for EP0 Rx FIFO
	read16ne cnt, rva, #0x128	@ cnt <- byte count for EP2 Rx FIFO	
  .endif
	add	rva, rva, #0x20		@ rva <- address of base of EP FIFOs
	set	rvb, 0
usbrwr:	@ read words
	sub	rvc, cnt, rvb
	cmp	rvc, #4
	bmi	usbrbt
	cmp	rvb, cnt
	bpl	usbrbt
	read	rvc, rva, env, lsl #2	@ rvc <- data word from EPn_FIFO
	write	rvc, dts, rvb		@ store it in buffer
	add	rvb, rvb, #4
	b	usbrwr
usbrbt:	@ read bytes
	eq	rvb, cnt
	itTT	ne
	read8ne	 rvc, rva, env, lsl #2	@ rvc <- data byte from EPn_FIFO
	write8ne rvc, dts, rvb		@ store it in buffer
	addne	rvb, rvb, #1
	bne	usbrbt
	set	rva, usb_base
	set	pc,  lnk

_func_
usbhwWriteBuf2EP: @ write from buffer to IN EP
	@ on entry:	rva <- usb_base
	@ on entry:	env <- IN EP to write to
	@ on entry:	dts <- buffer
	@ on entry:	cnt <- number of bytes to send
	@ modifies:	rvb, rvc
	eq	env, #0
	itE	eq
	seteq	rvb, 0
	setne	rvb, usb_txrdy
usbwwt:	usbldr	rvc, rva, usb_ibulkin
	tst	rvc, rvb
	bne	usbwwt
	add	rva, rva, #0x20		@ rva <- address of base of EP FIFOs
	set	rvb, 0
usbwwr:	@ write words
	sub	rvc, cnt, rvb
	cmp	rvc, #4
	bmi	usbwbt
	cmp	rvb, cnt
	bpl	usbwbt
	read	rvc, dts, rvb		@ rvc <- data from buffer
	write	rvc, rva, env, lsl #2	@ store it in EPn_FIFO
	add	rvb, rvb, #4
	b	usbwwr
usbwbt:	@ write bytes
	eq	rvb, cnt
	itTT	ne
	read8ne	 rvc, dts, rvb		@ rvc <- data from buffer
	write8ne rvc, rva, env, lsl #2	@ store it in EPn_FIFO
	addne	rvb, rvb, #1
	bne	usbwbt
	set	rva, usb_base
	set	pc,  lnk

/*------------------------------------------------------------------------------

		initiate USB character write from scheme (port function)

------------------------------------------------------------------------------*/

_func_
usbhwrc: @ initiate usb write, re-enable ints and return
	@ modifies:	rva, rvc
	@ returns via:	lnk
	set	rva, usb_base
  .ifdef usb_index_reg
	set	rvc, UsbBulkInEP
	write8	rvc, rva, #usb_index_reg   @ select endpoint in INDEX_REG
usbwcw:	read8	rvc, rva, #usb_index_reg   @ rvc <- selected EP from INDEX_REG
	eq	rvc, #UsbBulkInEP	   @ correct endpoint selected?
	bne	usbwcw			   @	if not, jump back to re-select
  .endif
usbwcl:	@ check for TxRdy or set-and-wait on TxRdy
	usbldr	rvc, rva, usb_ibulkin	   @ rvc <- status from IN_CSR1_REG
	tst	rvc, #usb_txrdy		   @ is IN_PKT_RDY set (dev wtng 2 snd)?
	bne	usbwcx
	set	rvc, usb_txrdy
	usbstr	rvc, rva, usb_ibulkin	   @ 	if not, set IN_PKT_RDY in TXCSR
	b	usbwcl
usbwcx:	@ return
	swi	run_normal		   @ enable interrupts (user mode)
	set	pc,  lnk		   @ return



