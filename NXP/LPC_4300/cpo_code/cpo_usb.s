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
@	Options and Board Configuration
@-----------------------------------------------------------------------------*/

@use_usb0	= 1			@ use USB0 (vs USB1) set opposite to M4
ua3isr		= noisr

MAIN_STACK 	= 0x2000c000		@ main stack: 4th bank top (5th bk strt)

usb_Q_heads	= 0x2000f000		@ 5th bank (2kb align) (if usb is used)

cpo_tx_buffer	= 0x2000fc00
cpo_rx_buffer	= 0x2000fd00
cpo_tx_msg	= 0x2000fe00		@ M0->M4 msg, 1=USB rx, 2=gc mark done
cpo_rx_msg	= 0x2000ff00		@ M4->M0 msg, 1=USB tx, 2/3=strt/stp gc

/*------------------------------------------------------------------------------
@	CONSTANTS
@-----------------------------------------------------------------------------*/

@ architecture
.cpu		cortex-m0

@ interrupts
cpo_int_num	= 1			@ M4 Tx event
.ifdef use_usb0
  usb_int_num	= 8			@ USB-0
.else
  usb_int_num	= 9			@ USB-1
.endif
ua3_int_num	= 27			@ USART-3
cpo_int		= 1 << cpo_int_num   	@ bit
usb_int		= 1 << usb_int_num   	@ bit
ua3_int		= 1 << ua3_int_num   	@ bit
int_base	= 0xe000e300		@ interrupt status base address
int_en_base	= 0xe000e100

@ gpio
io0_base 	= 0x400F6000 		@ gpio0 dir
io1_base 	= 0x400F6004 		@ gpio1 dir
io2_base 	= 0x400F6008 		@ gpio2 dir
io3_base 	= 0x400F600C 		@ gpio3 dir
io4_base 	= 0x400F6010 		@ gpio4 dir
io5_base 	= 0x400F6014 		@ gpio5 dir
io6_base 	= 0x400F6018 		@ gpio6 dir
io7_base 	= 0x400F601C 		@ gpio7 dir
io_set		= 0x0200		@ SET
io_dir		= 0x0000		@ DIR
io_clear	= 0x0280		@ CLR
io_state	= 0x0100		@ PIN -- read state of INPUT  pin
@io_state	= 0x0200		@ SET -- read state of OUTPUT pin
io_toggle	= 0x0300		@ NOT

@ pin configuration
SCU_SFSP0_n	= 0x40086000		@ SFSP0_0 to SFSP0_1  base address
SCU_SFSP1_n	= 0x40086080		@ SFSP1_0 to SFSP1_20 base address
SCU_SFSP2_n	= 0x40086100		@ SFSP2_0 to SFSP2_13 base address
SCU_SFSP3_n	= 0x40086180		@ SFSP3_0 to SFSP3_8  base address
SCU_SFSP4_n	= 0x40086200		@ SFSP4_0 to SFSP4_10 base address
SCU_SFSP5_n	= 0x40086280		@ SFSP5_0 to SFSP5_7  base address
SCU_SFSP6_n	= 0x40086300		@ SFSP6_0 to SFSP6_12 base address
SCU_SFSP7_n	= 0x40086380		@ SFSP7_0 to SFSP7_7  base address
SCU_SFSP8_n	= 0x40086400		@ SFSP8_0 to SFSP8_8  base address
SCU_SFSP9_n	= 0x40086480		@ SFSP9_0 to SFSP9_6  base address
SCU_SFSPA_n	= 0x40086500		@ SFSPA_0 to SFSPA_4  base address
SCU_SFSPB_n	= 0x40086580		@ SFSPB_0 to SFSPB_6  base address
SCU_SFSPC_n	= 0x40086600		@ SFSPC_0 to SFSPC_14 base address
SCU_SFSPD_n	= 0x40086680		@ SFSPD_0 to SFSPD_16 base address
SCU_SFSPE_n	= 0x40086700		@ SFSPE_0 to SFSPE_15 base address
SCU_SFSPF_n	= 0x40086780		@ SFSPF_0 to SFSPF_11 base address
SCU_SFSCLKn	= 0x40086C00		@ SFSCLK0 to SFSCLK3  base address
SCU_SFSUSB	= 0x40086C80		@ SFSUSB  base address

@ uarts	
uart0_base	= 0x40081000		@ UART0
uart1_base	= 0x40082000		@ UART1
uart2_base	= 0x400C1000		@ UART2
uart3_base	= 0x400C2000		@ UART3
uart_rhr	= 0x00			@ RBR
uart_thr	= 0x00			@ THR
uart_ier	= 0x04			@ IER
uart_istat	= 0x08
uart_status	= 0x14			@ offset to uart status register
uart_txrdy	= 0x20			@ bit indicating uart THR empty

@ usb registers
.ifdef use_usb0
  usb_base	= 0x40006100		@ USB-0 base
.else
  usb_base	= 0x40007100		@ USB-1 base
.endif
usb_istat_dv	= 0x44			@ USBDevIntSt
usb_iep_mask	= 0x01			@ mask for endpoint interrupt
usb_iclear_dv	= usb_istat_dv		@ USBDevIntClr
usb_idv_mask	= (1 << 6)		@ mask for device status interrupt
usb_busreset	= (1 << 6)		@ bus reset bit
usb_itxendp	= 0			@ Tx end of packet interrupt bit
usb_suspend	= 0			@ suspend bit
usb_istat_ep	= 0xbc			@ USBEpIntSt
usb_iclear_ep	= usb_istat_ep		@ USBEpIntClr
usbCO_ibit	= (1 <<  0)		@ bit indic int for Control OUT Endpoint
usbCI_ibit	= (1 << 16)		@ bit indic int for Control IN  Endpoint
usbBO_ibit	= (1 <<  2)		@ bit indic int for Bulk    OUT Endpoint
usbBI_ibit	= (1 << 18)		@ bit indic int for Bulk    IN  Endpoint
usbBulkINDescr	= 0x82			@ Bulk IN is EP 2 (for desc at end file)
usbCO_setupbit	= (1 << 0)		@ EP stat bit indic last tfer was SETUP
UsbControlOutEP	= 0x00			@ Control IN Endpoint (phys 0, log 0)
UsbControlInEP	= 0x01			@ Control IN Endpoint (phys 1, log 0)
UsbBulkOutEP	= 0x04			@ Bulk OUT EP (phys = 4, log = 2)
UsbBulkInEP	= 0x05			@ Bulk IN  EP (phys = 5, log = 2)
usb_ibulkin	= 0xb8			@ to find status of Bulk IN EP (primed?)
usb_txrdy	= (1 << 18)		@ Tx rdy bit in Bulk_IN (EP Tx primed)

@ usb data structures (uses 512 bytes total)
QH_CO		= usb_Q_heads + 0x0000	@ queue head         for Control OUT EP
QH_CI		= usb_Q_heads + 0x0040	@ queue head         for Control IN  EP
QH_BO		= usb_Q_heads + 0x0100	@ queue head         for Bulk    OUT EP
QH_BI		= usb_Q_heads + 0x0140	@ queue head         for Bulk    IN  EP
dTD_CO		= usb_Q_heads + 0x0180	@ data Rx descriptor for Control OUT EP
dTD_CI		= usb_Q_heads + 0x01a0	@ data Tx descriptor for Control IN  EP
dTD_BO		= usb_Q_heads + 0x01c0	@ data Rx descriptor for Bulk    OUT EP
dTD_BI		= usb_Q_heads + 0x01e0	@ data Tx descriptor for Bulk    IN  EP
BUFFER8_CO	= usb_Q_heads + 0x0030	@  8-byte Rx buffer  for Control OUT EP
BUFFER64_BO	= usb_Q_heads + 0x0080	@ 64-byte Rx buffer  for Bulk    OUT EP
BUFFER64_BI	= usb_Q_heads + 0x00c0	@ 64-byte Tx buffer  for Bulk    IN  EP
USB_SETUP	= usb_Q_heads + 0x0038
USB_CONF	= usb_Q_heads + 0x0070
USB_CHUNK	= usb_Q_heads + 0x0130
USB_ZERO	= usb_Q_heads + 0x0178

@ M0 - M4 Inter-Process Communication (IPC)
cpo_rxint_clear	= 0x40043130		@ M4TXEVENT register address
cpo_tx_status	= 0x40043400		@ M0TXEVENT register address

@ system
sys_config	= 0x40043000		@ CREG (M4-M0 mem remap, ETB RAM cfg,..)

@ clocks
CGU_base	= 0x40050000


/*------------------------------------------------------------------------------
@	REGISTER RENAMING
@-----------------------------------------------------------------------------*/

	fre	.req r0			@ free pointer
	cnt	.req r1			@ continuation (return address)
	rva	.req r2			@ raw value a
	rvb	.req r3			@ raw value b
	sv1	.req r4			@ scheme value 1
	sv2	.req r5			@ scheme value 2
	sv3	.req r6			@ scheme value 3
	sv4	.req r7			@ scheme value 4
	sv5	.req r8			@ scheme value 5
	env	.req r9			@ default environment
	dts	.req r10		@ data stack
	glv	.req r11		@ global vector
	rvc	.req r12		@ raw value c
	lnk	.req r14		@ jump link -- lr


/*------------------------------------------------------------------------------
@	MACROS
@-----------------------------------------------------------------------------*/

.macro _func_
	.thumb_func
.endm

.macro set var, expr
	mov	\var,  \expr
.endm

.macro sets var, expr
	movs	\var,  \expr
.endm

.macro enterisr
	mrs	rvb, psp		@ rvb <- psp stack
	set	sp,  rvb		@ sp  <- psp_stack
.endm

.macro exitisr
	@ return from interrupt
	ldr	rvb, =0xfffffffd	@ rvb <- return to thread mode, process stack
	bx	rvb			@ return to thread mode
.endm

/*------------------------------------------------------------------------------
@	CODE
@-----------------------------------------------------------------------------*/

.syntax unified					@ enable Thumb-2 mode

.global _start					@ make code start global

.text						@ code (_start) link at 0x00

_start:	.word	MAIN_STACK			@ 0x00 - 	Main Stack base
	.word	reset				@ 0x01 - 	Reset isr
	.word	nmi_hndlr			@ 0x02 - 	Non Maskable Int
	.word	fault_hndlr			@ 0x03 - 	Hard Fault isr
	.word	0x00, 0x00, 0x00, 0x00		@ 0x04 - 0x07 	reserved
	.word	0x00, 0x00, 0x00		@ 0x08 - 0x0A	reserved
	.word	svc_hndlr			@ 0x0B - 	soft int handler
	.word	0x00, 0x00			@ 0x0C - 0x0D	reserved
	.word	pends_hndlr			@ 0x0E - 	pendable service
	.word	tick_hndlr			@ 0x0F - 	SYS Tick handler
	.word	noisr, cpoisr, noisr, noisr	@ 0x10-0x13 	IRQ 00-03
	.word	noisr, noisr,  noisr, noisr	@ 0x14-0x17 	IRQ 04-07
  .ifdef use_usb0
	.word	usbisr,noisr,  noisr, noisr	@ 0x18-0x1B 	IRQ 08-11
  .else
	.word	noisr, usbisr, noisr, noisr	@ 0x18-0x1B 	IRQ 08-11
  .endif
	.word	noisr, noisr,  noisr, noisr	@ 0x1C-0x1F 	IRQ 12-15
	.word	noisr, noisr,  noisr, noisr	@ 0x20-0x23 	IRQ 16-19
	.word	noisr, noisr,  noisr, noisr	@ 0x24-0x27 	IRQ 20-23
	.word	noisr, noisr,  noisr, ua3isr	@ 0x28-0x2B 	IRQ 24-27
	.word	noisr, noisr,  noisr, noisr	@ 0x2C-0x2F 	IRQ 28-31

_func_
nmi_hndlr:
_func_
fault_hndlr:
_func_
svc_hndlr:
_func_
pends_hndlr:
_func_
tick_hndlr:
	b	nmi_hndlr


/*------------------------------------------------------------------------------
@	STARTUP CODE
@-----------------------------------------------------------------------------*/

_func_
reset:	@ pre-set common values
	sets	fre, #0
	sets	sv1, #1
	sets	sv2, #2
	sets	sv3, #3
	sets	sv4, #4
	@ enable sleep-on-interrupt-exit run mode
	ldr	rva, =0xE000ED10 	@ rva <- SCR
	sets	rvb, #2
	str	rvb, [rva]		@ set sleep on interrupt exit
	@ configure Process Stack, select it and drop to User mode
	ldr	rva, =MAIN_STACK - 88	@ rva  <- address of Process Stack
	msr	psp, rva		@ Set Process stack address
	mrs	rva, control		@ rva  <- content of Processor Cntrl reg
	orrs	rva, rva, sv2		@ rva  <- use Process stack unprivileged
	msr	control, rva		@ drop to unprivlgd mode, Process stack
	@ set-up cpo tx/rx circular buffers
	ldr	rva, =cpo_tx_buffer	@ rva <- address of USB->M0->M4 buffer
	strb	sv4, [rva, #0]		@ set read  pointer (offset)
	strb	sv4, [rva, #2]		@ set write pointer (offset)
	ldr	rva, =cpo_rx_buffer	@ rva <- address of M4->M0->USB buffer
	strb	sv4, [rva, #0]		@ set read  pointer (offset)
	strb	sv4, [rva, #2]		@ set write pointer (offset)
	@ initialize communications hardware
	bl	chwini
	@ initiate interrupt-response mode
	wfi				@ sleep in-between interrupts


.ltorg


/*------------------------------------------------------------------------------

		NO ISR ENTRY

------------------------------------------------------------------------------*/

_func_
noisr:	@ no isr for peripheral, return from interrupt (hoping it self-clears)
	enterisr			@ sp  <- selected stack (psp or msp)
	exitisr				@ return to thread mode, selected stack

/*------------------------------------------------------------------------------

		CPO ISR ENTRY

------------------------------------------------------------------------------*/

_func_
cpoisr:	@ USB interrupt service routine entry
	@ on exit:	rvb <- interrupt number (of interrupt to process)
	@ on exit:	sp  <- process stack
	@ fre,cnt,rva/b/c,lnkusr,pcusr,spsr->stk
	enterisr			@ sp  <- selected stack (psp or msp)	
	push	{sv1-sv4}		@ save user regs on stk
	bl	gtakev			@ rva <- M4 event (and event acknowledged to M4)
	@ branch
	cmp	rva, #1
	beq	cpo_rx
cpoixt:	@ exit: wait for handshake from M4 (prevents isr re-entry on jitter)
	bl	hndscl			@ wait on handshake, clear ack, clear Rx-int
	pop	{sv1-sv4}		@ restore regs from stack
	exitisr				@ return to thread mode, process stack

_func_
gtakev:	@ get and acknoledge M4 event
	@ on exit:	rva <- M4 event
	@ modifies:	cnt, rva, rvb
	@ side-effects:	cpo_tx_msg contents to ack
	ldr	rvb, =cpo_rx_msg
	ldr	rva, [rvb]
	sets	cnt, #0x80
	orrs	cnt, cnt, rva
	ldr	rvb, =cpo_tx_msg
	str	cnt, [rvb]
	set	pc,  lnk

_func_
hndscl:	@ wait on handshake, clear ack, clear Rx-int
	@ modifies:	rva, rvb
	ldr	rva, =cpo_rx_msg
	ldr	rvb, [rva]
	cmp	rvb, #0
	bne	hndscl
	@ exit: clear ack set on entry (prevents deadlock)
	ldr	rva, =cpo_tx_msg
	sets	rvb, #0
	str	rvb, [rva]
	@ exit: clear interrupt (M4TXEVENT)
	sets	rvb, #0
	ldr	rva, =cpo_rxint_clear	@ rva <- M4TXEVENT register address
	str	rvb, [rva]		@ clear the M4 TX event (ack interrupt)
	set	pc,  lnk


/*------------------------------------------------------------------------------

	Communications Hardware Initialization: USB

------------------------------------------------------------------------------*/

chwini:	@ initialize USB subsystem
  .ifdef use_usb0
	@ set USB0 clock to PLL0USB (480 MHz)
	ldr	rva, =CGU_base
	ldr	rvb, =((0x07 << 24) | (1 << 11))
	str	rvb, [rva, #0x60]	@ BASE_USB0_CLK  <- PLL0USB, autoblk
	@ power-up the PHY
	ldr	rva, =sys_config
	ldr	rvb, [rva, #0x04]	@ rvb   <- CREG0
	sets	cnt, #(1<<5)
	bics	rvb, rvb, cnt
	str	rvb, [rva, #0x04]	@ CREG0 <- USB0 PHY enabled
  .else
	@ set USB1 clock to IDIVB (60 MHz)
	ldr	rva, =CGU_base
	ldr	rvb, =((0x0d << 24) | (1 << 11))
	str	rvb, [rva, #0x68]	@ BASE_USB1_CLK  <- IDIVB, autoblk
	@ configure USB1 VBUS pin and internal Full-Speed PHY
	ldr	rva, =SCU_SFSP2_n	@ rva <- P2_n pins base config address
	sets	rvb, #0x52		@ rvb <- func 2, no pull-U/D, IN buffer
	str	rvb, [rva, #0x14]	@ P2_5 pin <- VBUS func
	ldr	rva, =SCU_SFSUSB
	sets	rvb, #0x12
	str	rvb, [rva]		@ enable USB1 intern FS PHY, normal PWR
  .endif
	@ initialization of USB variables
	ldr	rva,  =USB_CHUNK
	str	fre,  [rva]		@ zero bytes remaining to send at startup
	ldr	rva,  =USB_ZERO
	str	fre,  [rva]		@ alternate interface and device/interface status = 0
	ldr	rva,  =USB_CONF
	str	fre,  [rva]		@ USB device is not yet configured
	@ reset USB1
	ldr	rva, =usb_base
	str	sv2, [rva, #0x40]	@ USB_CMD  <- reset
usrswt:	ldr	rvb, [rva, #0x40]
	tst	rvb, sv2
	bne	usrswt
	@ configure full-speed device mode
	sets	sv3, #0x80
	adds	sv3, rva, sv3
  .ifdef use_usb0
	sets	rvb, #0x08
	str	rvb, [sv3, #0x24]	@ OTGSC    <- device mode pull-down
	sets	rvb, #1
  .else
	sets	rvb, #0xc1
  .endif
	lsls	rvb, rvb, #24
	str	rvb, [sv3, #0x04]	@ USB_PORTSC1 <- FS PHY, no HS chirp
	str	sv2, [sv3, #0x28]	@ USBMODE     <- device mode
	@ initialize Queue Heads
	ldr	cnt, =usb_Q_heads	@ cnt <- Queue Heads start address
	str	cnt, [rva, #0x58]	@ EPLISTADDRESS <- Queue Heads adddress
	ldr	rvb, =0x00800080
	str	rvb, [sv3, #0x40]	@ EPCTRL0  <- enable EP0 Rx/Tx ctrl EP
	ldr	rvb, =0x20088000	@ rvb <- no ZLT, 8B maxpkt, int on SETUP
	ldr	cnt, =QH_CO
	str	rvb, [cnt, #0x00]	@ QH0 OUT <- set capabilities
	str	fre, [cnt, #0x04]	@ QH0 OUT <- set current dTD
	str	sv1, [cnt, #0x08]	@ QH0 OUT <- set tail
	ldr	cnt, =QH_CI
	str	rvb, [cnt, #0x00]	@ QH0 IN  <- set capabilities
	str	fre, [cnt, #0x04]	@ QH0 IN  <- set current dTD
	str	sv1, [cnt, #0x08]	@ QH0 IN  <- set tail
	@ initialize USB device controller
	sets	rvb, #0x41
	str	rvb, [rva, #0x48]	@ USB_INTR <- enable usb int, reset int
	str	sv1, [rva, #0x40]	@ USB_CMD  <- enable usb (run)
	@ enable interrupts in NVIC
	ldr	cnt, =int_en_base
	ldr	rva, =( cpo_int | usb_int)
	str	rva, [cnt]
	set	pc,  lnk


/*------------------------------------------------------------------------------

		CPO ISR write data received from M4, out through M0 to USB

------------------------------------------------------------------------------*/

_func_
cpo_rx:	@ M4 event is USB Tx, i.e. M0 Rx (data rcvd from M4, send it to usb)
	@ initiate usb write (if needed)
	@ modifies:	rva, rvb
	bl	hwBIwr
	b	cpoixt

.ltorg

/*------------------------------------------------------------------------------

		USB ISR

------------------------------------------------------------------------------*/


_func_
usbisr:	@ USB interrupt service routine entry
	@ on exit:	rvb <- interrupt number (of interrupt to process)
	@ on exit:	sp  <- process stack
	@ fre,cnt,rva/b/c,lnkusr,pcusr,spsr->stk
	enterisr			@ sp  <- selected stack (psp or msp)
	@ save registers
	push	{sv1-sv4}		@ store remnng user regs on stk
	@ get and process interrupt
	ldr	rva, =usb_base
	ldr	sv1, [rva, #usb_istat_dv] @ sv1 <- Device Interrupt Status
	ldr	rvb, =usb_iep_mask
	tst	sv1, rvb		@ is this an Enpoint (Slow) Interrupt?
	bne	usbEPi			@	if so, jump to process it
	sets	sv2, #usb_idv_mask
	tst	sv1, sv2		@ is this a Device Status Interrupt?
	bne	usbDSi
usbDSx: @ clear Device Status Interrupt and exit
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv] @ clear USB interrupt register
usbixt:	@ restore registers
	pop	{sv1-sv4}		@ restore regs from stack
	@ exit from interrupt	
	exitisr				@ return to thread mode, process stack


/*------------------------------------------------------------------------------

		Device interrupt treatment (reset)

------------------------------------------------------------------------------*/

usbDSi: @ Process a Device Status Interrupt [internal entry]
	@ on entry:	rva <- usb_base
	@ modifies:	rva, rvb, sv2, sv4 and cnt (in hwprimeEP0rd)
	@ side-effect:	clears usb interrupts (global, not endpoint)
	str	sv1, [rva, #usb_iclear_dv] @ clear USB interrupt
	sets	rvb, #usb_busreset	@ rvb <- device status
	tst	sv1, rvb		@ did we device receive a bus reset?
	beq	usbixt			@	if not, jump to exit
	sets	sv2, #0xa0
	adds	sv2, rva, sv2
	ldr	rvb, [sv2, #0x0c]	@ rvb <- ENDPTSETUPSTAT
	str	rvb, [sv2, #0x0c]	@ clear EP setups
	ldr	rvb, [sv2, #0x1c]	@ rvb <- ENDPTCOMPLETE
	str	rvb, [sv2, #0x1c]	@ clear EP complete
urstw0:	ldr	rvb, [sv2, #0x10]	@ rvb <- ENDPTPRIME
	cmp	rvb, #0
	bne	urstw0
	mvns	rvb, rvb
	str	rvb, [sv2, #0x14]	@ ENDPTFLUSH <- flush endpoints
	sets	sv2, #0x04
urstw1:	ldr	rvb, [rva, #usb_istat_dv] @ rvb <- device status
	tst	rvb, sv2		@ port change detected?
	beq	urstw1
	str	sv2, [rva, #usb_iclear_dv]
	bl	hwprCO			@ prime Control OUT endpoint
	sets	rvb, #0			@ rvb <- 0
	ldr	rva, =USB_CONF		@ rva <- address of conf status in RAM
	str	rvb, [rva]		@ store zero as conf status
	ldr	rva, =USB_CHUNK		@ rva <- address of chunk in RAM
	str	rvb, [rva]		@ set remaining bytes to send to zero
	b	usbDSx			@ jump to exit


/*------------------------------------------------------------------------------

		Endpoint interrupt treatment

------------------------------------------------------------------------------*/

usbEPi:	@ Process an endpoint interrupt
	ldr	rva, =usb_base
	sets	rvb, #usb_istat_ep
	ldr	sv2, [rva, rvb]		@ sv2 <- Endpoint Interrupt Status (eisr)
	sets	rvb, #0xac
	ldr	sv3, [rva, rvb]		@ sv3 <- EPSetupSTAT
	@ clear selected interrupts
	sets	rvb, #usb_iclear_ep
	str	sv2, [rva, rvb]		@ clear the interrupt
	sets	sv4, #1
	tst	sv3, sv4
	bne	usbCOSjmp		@	if so, jump to EP0 Setup phase
	sets	sv4, #usbCO_ibit
	tst	sv2, sv4		@ is interrupt for Control OUT EP ?
	bne	usbCODjmp		@	if so, jump to EP0 Data/Status
	sets	sv4, #usbBO_ibit
	tst	sv2, sv4		@ is interrupt for Bulk Out EP ?
	bne	usbBOi			@	if so, jump to process EP4 int
	ldr	sv4, =usbBI_ibit
	tst	sv2, sv4		@ is interrupt for Bulk IN EP ?
	bne	usbBIi			@	if so, jump to process EP5 int
	ldr	sv4, =usbCI_ibit
	tst	sv2, sv4		@ is interrupt for Control IN EP ?
	bne	usbCIi			@	if so, jump to process EP1 int
usbEPx:	@ clear endpoint interrupt and exit
	b	usbDSx			@ jump to exit

usbCODjmp: @ long jump
	b	usbCOD

usbCOSjmp: @ long jump
	b	usbCOS

/*------------------------------------------------------------------------------

		Bulk OUT interrupt treatment

------------------------------------------------------------------------------*/

usbBOi:	@ Process interrupt for Bulk OUT endpoint
	@ read data from usb FIFO into WRITEBUFFER
	ldr	rva, =dTD_BO
	ldrb	cnt, [rva, #0x06]
	sets	fre, #64
	subs	cnt, fre, cnt
	ldr	sv2, =cpo_tx_buffer	@ sv2 <- address of USB->M0->M4 buffer
	ldrb	rva, [sv2, #0]		@ rva <- read  pointer (offset)
	ldrb	fre, [sv2, #2]		@ fre <- write pointer (offset)
	ldr	sv3, =BUFFER64_BO	@ sv3 <- USB source buffer
	sets	sv4, #0			@ sv4 <- initial USB source offset
	b	usbBO1
usbBO0:	@ reset write pointer offset to start of buffer
	sets	rvb, #0xfe
	cmp	rva, #4			@ collision of write and read pointers? 
	beq	usbBO2			@ 	if so, jump to put ctrl-c at #xfe
	sets	fre, #4
usbBO1:	@ read packet into data buffer
	cmp	sv4, cnt		@ done?
	bpl	usbBO3			@	if so, jump to finish up
	ldrb	rvb, [sv3, sv4]		@ rvb <- next data byte from USB
	strb	rvb, [sv2, fre]		@ write data to USB->M0->M4 buffer
	adds	sv4, sv4, #1		@ sv4 <- updated USB source offset
	cmp	fre, #0xff		@ wrote to last buffer position?
	beq	usbBO0
	adds	fre, fre, #1		@ fre <- updated write pointer (offset)	
	cmp	fre, rva		@ collision of write and read pointers?
	bne	usbBO1			@	if not, jump to get next byte
	@ collision
	subs	fre, fre, #1		@ fre <- last write pointer (offset)
	sets	rvb, #0xff
	cmp	fre, #4
	beq	usbBO2
	subs	rvb, fre, #1		@ sv4 <- offset for ctrl-c
usbBO2:	@ store ctrl-c before write pointer
	sets	rva, #3
	strb	rva, [sv2, rvb]		@ write ctrl-c to USB->M0->M4 buffer
usbBO3: @ keep going
	strb	fre, [sv2, #2]		@ store updated write pointer (offset)
	bl	hwprBO			@ prime the Bulk OUT EP for next Rx
usbBO4:	@ wait for M4 core to be event free
	ldr	rva, =cpo_tx_status	@ rva <- M0TXEVENT register address
	ldr	rvb, [rva]		@ rvb <- M0 TX event value
	cmp	rvb, #0			@ has M4 core processed last event?
	bne	usbBO4			@	if not, jump to keep waiting
	ldr	rva, =cpo_tx_msg
	sets	rvb, #1
	str	rvb, [rva]
	@ signal the write to M4
	dsb
	isb
	sev				@ send event to M4 core
usbBO8:	@ wait for acknowledge or M4 event prempting this (prevent deadlock)
	ldr	rva, =cpo_rxint_clear	@ rva <- M4TXEVENT register address
	ldr	rvb, [rva]
	cmp	rvb, #0
	bne	usbBO5
	ldr	rva, =cpo_rx_msg
	ldr	rvb, [rva]		@ set message in Tx-MSG
	cmp	rvb, #0x81
	bne	usbBO8
	@ clear tx-msg (handshake) (prevent M0 isr re-entry on interrupt jitter)
	ldr	rva, =cpo_tx_msg
	sets	rvb, #0			@ rvb <- 0, new message = do nothing
	str	rvb, [rva]		@ set message in Tx-MSG
	@ return
	b	usbixt			@ exit


usbBO5:	@ deadlock occured (M4 sent an event that preempts the current attempt
	@ to send a write event from M0 to M4)
	@ process the M4 event and go back to try and send ours
	@ 1- clear our aborted event
	ldr	rva, =cpo_tx_status	@ rva <- M0TXEVENT register address
	sets	rvb, #0
	str	rvb, [rva]
	@ 2- acknowledge M4 event
	bl	gtakev			@ rva <- M4 event (and event acknowledged to M4)
	@ 3- branch
	cmp	rva, #1
	beq	usbBO7
usbBO6:	@ wait for handshake from M4 (prevents isr re-entry on jitter)
	bl	hndscl			@ wait on handshake, clear ack, clear Rx-int
	@ return to SEV
	b	usbBO4			@ return to sev

usbBO7:	@ M4 event is USB Tx, i.e. M0 Rx (data rcvd from M4, send it to usb)
	@ initiate usb write (if needed)
	@ modifies:	rva, rvb
	bl	hwBIwr
	b	usbBO6


/*------------------------------------------------------------------------------

		Bulk IN interrupt treatment

------------------------------------------------------------------------------*/

usbBIi:	@ Process interrupt for Bulk IN endpoint
	@ write data from M4->M0->USB buffer to USB buffer
	@ and find value for cnt
	@ make sure endpoint is no longer primed
	ldr	rva, =usb_base
	adds	rva, rva, #0x80
	sets	sv3, #4
	lsls	rvb, sv3, #16
	@ may want to wait for bit to be set in ENDPTSTATUS here
usbBIa:	ldr	sv4, [rva, #0x38]	@ sv4 <- ENDPTSTATUS
	tst	sv4, rvb
	bne	usbBIa
	ldr	sv3, =cpo_rx_buffer	@ sv3 <- address of M4->M0->USB buffer
	ldrb	fre, [sv3, #0]		@ fre <- read  pointer (offset)
	ldrb	rva, [sv3, #2]		@ rva <- write pointer (offset)
	ldr	sv2, =BUFFER64_BI	@ sv2 <- USB target buffer
	sets	cnt, #0			@ cnt <- initial USB target offset/count
	b	usbBI1			@ jump to start copying data to USB bufr
usbBI0:	@ reset read pointer offset to start of buffer
	sets	fre, #4			@ fre <- updated read pointer
	cmp	cnt, #64		@ reached max count for bulk endpoint?
	bpl	usbBI2			@	if so, jump to finish up
usbBI1:	@ read packet into data buffer
	cmp	fre, rva		@ have we read the whole buffer?
	beq	usbBI2			@	if so,  jump to finish up
	ldrb	rvb, [sv3, fre]		@ rvb <- next data byte from M4->M0 bufr
	strb	rvb, [sv2, cnt]		@ write data to target USB buffer
	adds	cnt, cnt, #1		@ cnt <- updated USB target offset/count
	cmp	fre, #0xff		@ did we read from last buffer position?
	beq	usbBI0			@	if so,  jump to reset read ptr
	adds	fre, fre, #1		@ fre <- updated read pointer (offset)	
	cmp	cnt, #64		@ reached max count for bulk endpoint?
	bmi	usbBI1			@	if not, jump to get next byte
usbBI2: @ keep going
	strb	fre, [sv3, #0]		@ store updated read pointer (offset)
	@ prime Bulk IN endpoint and exit
	bl	hwprBI
	b	usbEPx
	

/*------------------------------------------------------------------------------

		Control IN interrupt treatment

------------------------------------------------------------------------------*/

usbCIi:	@ Process interrupt for Control IN Endpoint
	ldr	rva, =USB_CHUNK
	ldr	cnt, [rva]		@ cnt <- how many bytes remain to send
	ldr	sv2, [rva, #4]		@ sv2 <- start address of data to send
	sets	sv4, #0			@ sv4 <- 0=num bytes that may remain
	cmp	cnt, #8			@ want to send more than 8 bytes?
	bmi	usbCs0			@	if not, jump to continue
	sets	sv4, #8
	subs	sv4, cnt, sv4		@ sv4 <- remainng num byts
	sets	cnt, #8			@ cnt <- 8=num byts snd
	adds	rvb, sv2, cnt		@ rvb <- adrs remainng dat
	str	rvb, [rva, #4]		@ store that in USB_CHUNK
usbCs0:	@ keep going
	str	sv4, [rva]		@ store num remaining bytes in USB_CHUNK
	bl	hwprCI			@ write buffer to EP0 IN (normal)
	b	usbEPx			@ jump to exit

/*------------------------------------------------------------------------------

		Control OUT interrupt treatment

------------------------------------------------------------------------------*/

usbCOS:	@ Process interrupt for Control OUT Endpoint: Setup Phase
	@ on entry:	sv3 <- contents of EPSetupSTAT
	sets	rvb, #0			@ rvb <- 0
	ldr	rva, =USB_CHUNK		@ rva <- address of chunk in RAM
	str	rvb, [rva]		@ set remaining bytes to send to zero	
	ldr	sv4, =USB_SETUP		@ sv4 <- Setup buffer
	ldr	rva, =usb_Q_heads
	ldr	rvb, [rva, #0x28]
	str	rvb, [sv4]
	ldr	rvb, [rva, #0x2c]
	str	rvb, [sv4, #4]
	ldr	rva, =usb_base
	adds	rva, rva, #0x80
	str	sv3, [rva, #0x2c]	@ ENDPTSetupSTAT <- ack/clr setup rcvd
uhwsw0:	ldr	rvb, [rva, #0x2c]
	cmp	rvb, #0
	bne	uhwsw0
	ldr	rvb, =((1 << 16) | 1)
	str	rvb, [rva, #0x34]	@ flush pending cntrl IN/OUT tfr, if any
uhwsw1:	ldr	cnt, [rva, #0x38]
	tst	cnt, rvb
	bne	uhwsw1
	ldr	sv4, =USB_SETUP		@ sv4 <- Setup buffer
	ldr	rvb, [sv4]		@ rvb <- reqtyp(8), request(8), val(16)
	ldr	sv4, [sv4, #4]		@ sv4 <- index(16), length(16)
	lsrs	cnt, sv4, #16		@ cnt <- num byts to tfer, is it zero?
	beq	usbRQS			@	if so,  jump to process request
	sets	fre, #0x80
	tst	rvb, fre		@ is dir from dev to host?
	bne	usbRQS			@	if so,  jump to process request
	bl	hwprCO			@ NEW w/r M4 version 060 (testing)
					@ either that, or unstall EP, or both
	b	usbEPx			@	if not, exit

usbCOD:	@ Process interrupt for Control OUT Endpoint: Data/Status Phase
	ldr	rva, =USB_SETUP
	ldr	rvb, [rva]		@ rvb <- reqtyp(8), request(8), val(16)
	sets	fre, #0xff
	cmp	rvb, fre		@ is data OUT phase complete?
	beq	usbStO			@	if so,  jump to Status OUT phase
	@ Data OUT Phase
	ldr	sv4, [rva, #4]		@ sv4 <- index(16), length(16)
	lsrs	cnt, sv4, #16		@ cnt <- num byts to tfer, is it zero?
	bne	usbRQS
	b	usbEPx			@	if so,  jump to return
usbRQS:	@ process EP0 request
	@ on entry:	rvb <- reqtyp(8l), request(8h), val(16H)
	@ on entry:	cnt <- num bytes to transfer (length)
	lsrs	fre, rvb, #8
	sets	sv3, #0xff
	ands	fre, fre, sv3		@ fre <- request bits 15:8
	sets	sv3, #0x7f
	ands	sv3, rvb, sv3		@ sv3 <- request bits 7:0
	lsrs	rvb, rvb, #16		@ rvb <- value of request
	@ process possible Standard Requests
	cmp	sv3, #0			@ does request end with 0x00?
	bne	usbStl			@	if not, Stall
	@ requests ending in 00
	cmp	fre, #0x05		@ Set Address?
	beq	usbADR			@	if so,  jump to set the address
	cmp	fre, #0x06		@ Get Descriptor?
	beq	usbDGD			@	if so,  jump to process that
	cmp	fre, #0x09		@ Set Configuration?
	beq	usbDSC			@	if so,  jump to process that
usbStl: @ stall Control EP
	ldr	rva, =usb_base
	adds	rva, rva, #0xc0
	ldr	rvb, =0x00810081
	str	rvb, [rva]
	b	usbEPx


/*------------------------------------------------------------------------------

		Standard Requests

------------------------------------------------------------------------------*/


_func_
usbADR: @ 9.4.6 (USB 2.0) Set Address Standard Request
	@ on entry:	rvb <- address (8l) (val of request)
	@ modifies:	rva, rvb
	lsls	rvb, rvb, #25
	sets	rva, #1
	lsls	rva, rva, #24
	orrs	rvb, rvb, rva
	ldr	rva, =usb_base
	str	rvb, [rva, #0x54]
	b	usbSIx			@ jump to Status IN Phase and exit

usbDGD:	@ 9.4.3 (USB 2.0) Get Descriptor of Device Standard request
	@ on entry:	rvb <- descr index(8l), descr type(8h) (val of request)
	@ on entry:	cnt <- num bytes to transfer (length of request)
	sets	fre, #0xff
	ands	fre, rvb, fre		@ fre <- descriptor index
	lsrs	rvb, rvb, #8		@ rvb <- dscrptr typ (1-dev,2-cfg,3-str,
					@	4-if,5-ep,6-dq,7-osp,8-pow)
	ldr	sv2, =USB_DeviceDesc	@ sv2  <- adrs of dev desc
	sets	rva, #0			@ rva <- index = 0
	sets	sv3, #0			@ sv3 <- offset to next desc (init=0)
usbS61:	adds	sv2, sv2, sv3		@ sv2 <- address of next descriptor
	ldrb	sv3, [sv2]		@ sv3 <- size of descriptor
	cmp	sv3, #0			@ have we reached end of descrip table?
	beq	usbStl			@	if so, return Stall nothng found
	ldrb	sv4, [sv2, #1]		@ sv4 <- item at pos 1 in desc (typ)
	cmp	rvb, sv4		@ is type = descriptor type ?
	bne	usbS61			@	if not, go to scan rest of descs
	cmp	fre, rva		@ is index = Descriptor index?
	beq	usbS62
	adds	rva, rva, #1		@ rva <- index + 1
	b	usbS61
usbS62:	@ send cnt bytes of the descriptor
	ldr	rva, =USB_CHUNK
	sets	sv3, #0
	set	sv4, sv2
	cmp	cnt, #9
	bmi	usbS63
	sets	sv3, #8
	subs	sv3, cnt, sv3		@ sv4 <- nmbr bytes remain
	sets	cnt, #8	
	adds	sv4, sv2, cnt		@	if so,  sv4 <- adrs of next chnk
usbS63:	@ keep going
	str	sv3, [rva]		@ store that in USB_CHUNK
	str	sv4, [rva, #4]		@ store that in USB_CHUNK
	bl	hwprCI			@ prime Cntrl IN EP to wrt dscrp in sv2
usbSOx:	@ Prepare setup buffer for Status OUT Phase
	@ modifies:	rva, rvb, sv4
	ldr	rva, =USB_SETUP
	sets	rvb, #0xFF
	str	rvb, [rva]
usbStO:	@ Control OUT Interrupt, Status OUT Phase
	bl	hwprCO			@ prime Control Out endpoint for read
	b	usbEPx


usbDSC:	@ 9.4.7 (USB 2.0) Set Configuration of Device Standard request
	@ on entry:	rvb <- configuration to set (value of request)
	sets	fre, #0xff
	ands	fre, rvb, fre		@ fre <- config = lower byt of setup val
	cmp	fre, #0			@ is configuration zero ?
	bne	usbS90
	@ de-configure the device
	ldr	rva, =usb_base
	adds	rva, rva, #0x80
	ldr	rvb, =((0<<23) | (2<<18) | (0<<7) | (2<<2))
	str	rvb, [rva, #0x48]	@ EPCTRL2  <- disab EP 2 Rx,Tx, bulk EP
	b	usbSIx			@ jump to Status IN Phase and exit

usbS90:	@ see if we should configure the device
	cmp	fre, #1			@ is the selected configuration #1 ?
	bne	usbStl		@	if not, exit with Stall bad conf
	@ configure the device
	ldr	rva, =usb_base
	adds	rva, rva, #0x80
	ldr	rvb, =((1<<23) | (2<<18) | (1<<7) | (2<<2))
	str	rvb, [rva, #0x48]	@ EPCTRL2  <- enab EP 2 Rx,Tx,as bulk EP	
	ldr	sv4, =QH_BO
	ldr	sv3, =QH_BI
	ldr	rvb, =0x20400000	@ rvb <- no ZLT, 64-byte max packet
	str	rvb, [sv4, #0x00]	@ QH2 OUT <- set capabilities
	str	rvb, [sv3, #0x00]	@ QH2 IN  <- set capabilities
	sets	rvb, #0
	str	rvb, [sv4, #0x04]	@ QH2 OUT <- set current dTD
	str	rvb, [sv3, #0x04]	@ QH2 IN  <- set current dTD
	sets	rvb, #1
	str	rvb, [sv4, #0x08]	@ QH2 OUT <- set tail
	str	rvb, [sv3, #0x08]	@ QH2 IN  <- set tail
	bl	hwprBO
usbSIx:	@ USB Status IN  exit -- write null packet to Control EP
	ldr	sv2, =USB_ZERO		@ sv2 <- default buffer for wrt (zeros)
	sets	cnt, #0x00		@ cnt <- 0 bytes to send
	bl	hwprCI			@ write 0 bytes to EP
	b	usbEPx

.ltorg

/*------------------------------------------------------------------------------

		rdEP, wrtEP, wrtEPU

------------------------------------------------------------------------------*/

_func_
hwprBO: @ prime Bulk OUT endpoint for read
	ldr	sv4, =QH_BO
	ldr	rva, =dTD_BO
	ldr	sv3, =BUFFER64_BO
	sets	fre, #64
	b	hwprimeEPnrd

_func_
hwprCO: @ prime Control Out endpoint for read
	ldr	sv4, =QH_CO
	ldr	rva, =dTD_CO
	ldr	sv3, =BUFFER8_CO
	sets	fre, #8
hwprimeEPnrd: @ continue and [internal entry]
	@ on entry:	sv4 <- QHn  address for endpoint
	@ on entry:	rva <- dTDn address for endpoint
	@ on entry:	sv3 <- read buffer for endpoint
	@ on entry:	fre <- max transfer size for endpoint
	@ modifies:	rva, rvb, sv4
	str	rva, [sv4, #0x08]	@ store address of dTD in QH's next-dTD
	sets	rvb, #0
	str	rvb, [sv4, #0x0c]	@ clear QH STAT
	sets	rvb, #1
	str	rvb, [rva]		@ set dTD as list tail
	lsls	rvb, fre, #16
	ldr	sv4, =((1 << 15) | 0x80)
	orrs	rvb, rvb, sv4		@ dTD status = active
	str	rvb, [rva, #0x04]	@ store control info in dTD
	str	sv3, [rva, #0x08]	@ store buffer ptr 0
	sets	rvb, #1
	cmp	fre, #8
	beq	hwpmkg
	sets	rvb, #4
hwpmkg:	@ keep going
	ldr	rva, =usb_base
	adds	rva, rva, #0x80
	str	rvb, [rva, #0x30]	@ ENDPTPRIME <- prime endpoint
	@ may want to wait for bit to be set in ENDPTSTATUS here
hwpmwt:	ldr	sv4, [rva, #0x38]
	tst	sv4, rvb
	beq	hwpmwt
	set	pc,  lnk

_func_
hwprCI:	@ write to Control IN EP
	@ on entry:	cnt <- count or #0xff for 0 with interupt generation
	ldr	rva, =0x40043404	@ rva <- M0APPMEMMAP
	ldr	rva, [rva]		@ rva <- M0 remap address
	orrs	sv2, sv2, rva		@ sv2 <- address of data to send w/remap
	ldr	sv4, =QH_CI
	ldr	rva, =dTD_CI
	sets	sv3, #1			@ sv3 <- 1 for CI, 4 for BI
	b	hwprimeEPnwr

_func_
hwBIwr:	@ initiate write 
	ldr	rva, =usb_base
	adds	rva, rva, #usb_ibulkin
	ldr	rva, [rva]
	ldr	rvb, =usb_txrdy		@ rvb <- txpktrdy bit (EP primed)
	sets	cnt, #0xff
	tst	rva, rvb		@ is Bulk IN endpoint already primed?
	beq	hwprBI
	set	pc,  lnk

_func_
hwprBI:	@ prime Bulk IN endpoint for write
	@ on entry:	cnt <- count or #0xff for 0 with interupt generation
	ldr	sv2, =BUFFER64_BI
	ldr	sv4, =QH_BI
	ldr	rva, =dTD_BI
	sets	sv3, #4			@ sv3 <- 1 for CI, 4 for BI

hwprimeEPnwr:	@ Prime Control IN or Bulk IN EP for write
	@ on entry:	sv4 <- QH
	@ on entry:	rva <- dTD
	@ on entry:	sv2 <- data start address (buffer)
	@ on entry:	cnt <- number of bytes to write to USB
	@ on entry:	sv3 <- 1 for CI, 4 for BI
	@ modifies:	rva, rvb, sv4
	@ set write_enable bit, and endpoint to use in control register
	str	rva, [sv4, #0x08]	@ store address of dTD in QH next dTD
	sets	rvb, #0
	str	rvb, [sv4, #0x0c]	@ clear QH STAT
	sets	rvb, #1
	str	rvb, [rva]		@ set dTD as list tail
	sets	rvb, #0
	cmp	cnt, #0xff
	beq	wrtEP9
	lsls	rvb, cnt, #16		@ set data count for dTD
wrtEP9:	@ keep going
	cmp	cnt, #0			@ transferring zero bytes?
	beq	wrtEP3
	sets	sv4, #1
	lsls	sv4, sv4, #15
	orrs	rvb, rvb, sv4		@ set interrupt on completion for dTD
wrtEP3:	@keep going
	sets	sv4, #0x80
	orrs	rvb, rvb, sv4		@ dTD status = active
	str	rvb, [rva, #0x04]	@ store control info in dTD
	str	sv2, [rva, #0x08]	@ store buffer ptr 0 in dTD (for CI)
	ldr	rva, =usb_base
	adds	rva, rva, #0x80
	lsls	rvb, sv3, #16
	str	rvb, [rva, #0x30]	@ ENDPTPRIME <- prime endpoint
	@ may want to wait for bit to be set in ENDPTSTATUS here
wrtEP2:	ldr	sv4, [rva, #0x38]
	tst	sv4, rvb
	beq	wrtEP2
	set	pc,  lnk


/*~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~
@ dump literal constants (up to 4K of code before and after this point)
@~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~~~-~~~~~*/
.ltorg

/*------------------------------------------------------------------------------
@
@  USB descriptors and configuration
@
@-----------------------------------------------------------------------------*/

.balign	4

USB_DeviceDesc:

@ Device Descriptor:
@ ------------------
@	bLength		bDescriptorType bcdUSB(L,H)
@       18 bytes	1 = device	usb 1.10
@.byte   0x12,		0x01,		0x10,	0x01
.byte   0x12,		0x01,		0x00,	0x02

@	bDeviceClass	bDeviceSubClass bDeviceProtocol bMaxPacketSize0
@	2 = CDC		0 = none	0 = std USB	8 bytes
.byte	0x02,		0x00,		0x00,		0x08

@	idVendor(L, H)	idProduct(L, H)	bcdDevice(L,H)
@					release 1.0
.byte	0xFF,	0xFF,	0x05,	0x00,	0x00,	0x01

@	iManufacturer	iProduct	iSerialNumber	bNumConfigurations
@	is in string 1	is in string 2	is in string 3	1 config only
.byte	0x01,		0x02,		0x03,		0x01

@ Configuration Descriptor:
@ -------------------------
@	bLength		bDescriptorType wTotalLength	bNumInterfaces
@	9 bytes		2 = config	100 bytes (L,H)	2 interfaces
.byte	0x09,		0x02,		0x43,	0x00,	0x02
@.byte	0x09,		0x02,		0x3c,	0x00,	0x02

@.byte	0x09,		0x02,		0x25,	0x00,	0x01

@	bConfigValue	iConfiguration	bmAttributes	bMaxPower
@	config #1	0 = no string	0xC0 = usbpwr	250 x 2 mA
.byte	0x01,		0x00,		0xC0,		0xFA



@ Interface 0 Setting 0 CDC ACM Interface Descriptor:
@ ---------------------------------------------------
@	bLength		bDescriptorType bIntrfcNumber	bAlternateSetting
@	9 bytes		4 = interf	interface 0	setting 0
.byte	0x09,		0x04,		0x00,		0x00

@	bNumEndpoints	bIntrfcClss	bIntrfcSbClss	bIntrfcPrtcl	
@	uses 1 endpnt	2 = CDC		2 = ACM		1 = Hayes modem	
.byte	0x01,		0x02,		0x02,		0x01
@.byte	0x01,		0x02,		0x02,		0x00
@@.byte	0x00,		0x02,		0x02,		0x00

@	iIntrfc
@	0 = no string
.byte	0x00


@ Header Functional Descriptor (CDC):
@ -----------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bcdCDC (L,H)
@	5 bytes		CS_INTERFACE	0 = Header	1.10
.byte	0x05,		0x24,		0x00,		0x10,	0x01

@ ACM Functional Descriptor (CDC):
@ --------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bmCapabilities
@	4 bytes		CS_INTERFACE	2 = ACM	
@.byte	0x04,		0x24,		0x02,		0x02
.byte	0x04,		0x24,		0x02,		0x00

@ Union Functional Descriptor (CDC):
@ ----------------------------------
@	bFunctionLength	bDescriptorType bDescriptorSbTp	
@	5 bytes		CS_INTERFACE	6 = Union	
.byte	0x05,		0x24,		0x06		

@	bMasterInterfce	bSlaveInterface0
@	Interface 0	Interface 1
.byte	0x00,		0x01

@ Call Management Functional Descriptor (CDC):
@ --------------------------------------------
@	bFunctionLength	bDescriptorType bDescripSubType	bmCapabilities
@	5 bytes		CS_INTERFACE	1 = Call Mgmnt	1 = mgmt on CDC
@.byte	0x05,		0x24,		0x01,		0x01
.byte	0x05,		0x24,		0x01,		0x00

@	bDataInterface
@	interface 1 used for mgmnt
.byte	0x01


@ Endpoint 1 (Interrupt In, notification) Descriptor:
@ ---------------------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP1, IN
.byte	0x07,		0x05,		0x81

@	bmAttributes	wMaxPacketSize	bInterval
@	3 = interrupt	8 bytes	(L,H)	polling interval
@.byte	0x03,		0x08,	0x00,	0x0A
.byte	0x03,		0x08,	0x00,	0x10



@ Interface 1 Setting 0 CDC Data Class Interface Descriptor:
@ ----------------------------------------------------------
@	bLength		bDescriptorType bIntrfcNumber	bAlternateSetting
@	9 bytes		4 = interf	interface 1	setting 0
.byte	0x09,		0x04,		0x01,		0x00
@.byte	0x09,		0x04,		0x00,		0x00

@	bNumEndpoints	bIntrfcClss	bIntrfcSbClss	bIntrfcPrtcl
@	uses 2 endpnts	10 = CDC Data	0 = default	0 = no specific
.byte	0x02,		0x0A,		0x00,		0x00

@	iIntrfc
@	0 = no string
.byte	0x00

@ Endpoint 2 (bulk data OUT, phys=5):
@ ------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP2, OUT
.byte	0x07,		0x05,		0x02

@	bmAttributes	wMaxPacketSize	bInterval
@	2 = bulk	64 bytes (L,H)	bulk EP never NAKs
.byte	0x02,		0x40,	0x00,	0x00

@ Bulk IN Endpoint, LPC2000: 2 (bulk data IN, phys=5), AT91SAM7: 3
@ ----------------------------------------------------------------
@	bLength		bDescriptorType bEndpointAddress
@	7 bytes		5 = endpoint	EP2, IN
.byte	0x07,		0x05,		usbBulkINDescr

@	bmAttributes	wMaxPacketSize	bInterval
@	2 = bulk	64 bytes (L,H)	bulk EP never NAKs
.byte	0x02,		0x40,	0x00,	0x00

@ String Descriptor 0 (language ID):
@ ----------------------------------
@	bLength		bDescriptorType language ID
@	4 bytes		3 = string	English US (L,H)
.byte	0x04,		0x03,		0x09,	0x04

@ String Descriptor 1: Manufacturer
@ ---------------------------------
@	bLength		bDescriptorType
@	14 bytes	3 = string
.byte	0x0E,		0x03

@	String contents
@	A	r	m	p	i	t
.hword	0x41,	0x72,	0x6D,	0x70,	0x69,	0x74

@ String Descriptor 2: Product
@ ----------------------------
@	bLength		bDescriptorType
@	14 bytes	3 = string
.byte	0x0E,		0x03

@	String contents
@	S	c	h	e	m	e
.hword	0x53,	0x63,	0x68,	0x65,	0x6D,	0x65

@ String Descriptor 3: Version
@ ----------------------------
@	bLength		bDescriptorType
@	8 bytes	3 = string
.byte	0x08,		0x03

@	String contents
@	0	7	0
.hword	0x30,	0x37,	0x30

@ Terminating zero:
@ -----------------
.byte	0x00


.balign 4



/*------------------------------------------------------------------------------
@	End of code
@-----------------------------------------------------------------------------*/

.end


