@---------------------------------------------------------------------------------------------------
@
@  ARMPIT SCHEME Version 050
@
@  ARMPIT SCHEME is distributed under The MIT License.

@  Copyright (c) 2006-2012 Hubert Montas

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
@---------------------------------------------------------------------------------------------------

@-----------------------------------------------------------------------------------------
@
@ III.G. HARDWARE SPECIFIC COMPONENTS OF USB ISR FOR LPC_2800
@
@-----------------------------------------------------------------------------------------

.ltorg
	
usbhwDeviceStatus: @ return device status in r7
	set	rvb, sv1
	set	pc,  lnk

usbhwReset:
	ldr	rvb, =0x8004107c
	ldr	env, =0xaa37
	str	env, [rvb]
	set	rvb, #0
	ldr	rva, =USB_FSHS_MODE
	str	rvb, [rva]		@ indicate that USB is not yet in HS mode
	ldr	rva, =usb_base
	set	env, #0x20		@ rvb <- 0x20
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP 0 SETUP
	set	env, #0x08
	str	env, [rva, #usb_reep]	@ USB EP  Type  <- control, enabled
	set	env, #0x00		@ rvb <- 0x00
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP 0 SETUP
	set	env, #0x08
	str	env, [rva, #usb_reep]	@ USB EP  Type  <- control, enabled
	set	env, #0x01		@ rvb <- 0x20
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP 0 SETUP
	set	env, #0x08
	str	env, [rva, #usb_reep]	@ USB EP  Type  <- control, enabled
	set	rvb, #0x54		@ rvb <- 0x54 = ACK, STALL NYET, sometimes NAK
	str	rvb, [rva, #0x10]	@ USB Interrupt config <- int on ACK, STALL NYET, sometimes NAK
	set	rvb, #0xb9		@ rvb <- 0xb9
	str	rvb, [rva, #0x8c]	@ USB Dev Int Enable  <- bus reset, suspend, resume, EP 0 setup, HSFS
	set	rvb, #0x03		@ rvb <- 0x03
	str	rvb, [rva, #0x90]	@ USB EP  Int Enable  <- EP 0 Rx and Tx
	set	rvb, #0x80		@ rvb <- 0x80
	str	rvb, [rva]		@ USB Device Address <- 0, device enabled
	set	pc,  lnk

usbhwRemoteWakeUp:
	@ process change to HS, suspend, resume (do nothing on suspend/resume)
	tst	rvb, #0x20		@ is it a FS to HS interrupt?
	seteq	pc,  lnk		@	if not, return
	set	rvb, #1
	ldr	rva, =USB_FSHS_MODE
	str	rvb, [rva]		@ indicate that USB has switched to HS mode
	set	pc,  lnk
	
usbhwEndpointStatus: @ return endpoint status in r3
	ldr	rva, =usb_base
	ldr	sv2, [rva, #usb_istat_ep]	@ sv2 <- Endpoint Interrupt Status (eisr)
	str	sv2, [rva, #usb_iclear_ep]	@ clear the interrupt
	set	sv3, sv2
	set	pc,  lnk

usbhwCIi: @ Control IN interrupt response
	b	wrtEPU

usbhwSetup: @ Control OUT Interrupt, Setup Phase
	@ dts <- buffer
	set	env, #0x20		@ rvb <- EP 0 Setup is EP to read from here
	ldr	rva, =usb_base
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP in env
	ldr	cnt, [rva, #usb_rxplen]	@ cnt <- number of bytes to read + possible junk bits
	ldr	rvb, =0x3FF		@ rvb <- mask to get number of bytes
	and	cnt, cnt, rvb		@ cnt <- number of bytes to read
	ldr	rvb, [rva, #usb_rxdata]	@	if not, rvb <- next word read
	str	rvb, [dts]		@	if not, store next word in buffer
	ldr	rvb, [rva, #usb_rxdata]	@	if not, rvb <- next word read
	str	rvb, [dts, #4]		@	if not, store next word in buffer
	ldr	rvb, [dts, #4]
	lsr	rvb, rvb, #16
	eq	rvb, #0
	seteq	env, #UsbControlInEP	@ env  <- Control IN EndPoint
	streq	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP in env
	seteq	rvb, #2			@	if so,  rvb <- 4
	streq	rvb, [rva, #usb_ctrl]	@	if so,  set USBECtrl to initiate Status IN Phase
	seteq	pc,  lnk
	ldr	rvb, [dts]
	tst	rvb, #0x80
	seteq	env, #UsbControlOutEP	@	if so,  env  <- Control OUT EndPoint
	setne	env, #UsbControlInEP	@	if not, env  <- Control IN  EndPoint
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP in env
	ldr	rvb, [rva, #usb_ctrl]	@ rvb <- contents of USBECtrl
	orr	rvb, rvb, #4		@ rvb <- USBEctrl | 4 to initiate DATA phase
	str	rvb, [rva, #usb_ctrl]	@ set USBECtrl to initiate DATA phase
	set	pc,  lnk		@ return


usbhwStatusOut:	@ Control OUT Interrupt, Status OUT Phase
	b	usbEPx

usbhwDGD:	
	bl	wrtEP
	b	usbSOx
	
usbSOx:	@ Prepare setup buffer for Status OUT Phase 
	b	usbEPx

usbhwClearRxBk:
	set	pc,  lnk

usbhwwrtEPA:
	b	wrtEP			@ write data from r10 to host, return to point of call
	
usbhwixx:
	b	usbEPx
	
usbhwCleariTX: @ clear the txendpkt interrupt ---> NOT DONE *********
	set	pc,  lnk
	
usbhwEGS: @ Get Status of Endpoint in r5 into r7
	set	sv4, lnk
	bl	usbhwEPSet		@ rva <- usb_base, rvb <- EP control data (status)
	tst	rvb, #0x01		@ is the selected endpoint stalled?
	seteq	rvb, #0			@	if not, rvb <- 0 -- not stalled
	setne	rvb, #1			@	if so,  rvb <- 1 -- stalled
	set	pc,  sv4

usbhwSetAddress: @ Set Device to Address in r5
	ldr	rva, =USB_SETUP_BUFFER	@ rva <- address of setup buffer
	ldr	rvb, [rva]		@ rvb <- reqtyp(8), request(8), val(16)
	lsr	rvb, rvb, #16		@ rvb <- address = val(16)
	orr	rvb, rvb, #0x80		@ rvb <- address ored with Device Enable (0x80)
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_dev_adr]
	b	usbSIx			@ jump to Status IN Phase and exit

usbhwDeconfigure: @ Deconfigure the device
	set	sv4, lnk
	@ enable interrupts for physical EP 0 only (disable those for EP 3, 4 and 5)
	ldr	rva, =usb_base
	set	rvb, #0x03		@ rvb <- 0x03
	str	rvb, [rva, #0x90]	@ USB EP  Int Enable  <- EP 0 Rx and Tx only
	@ de-realize target endpoints
	ldr	sv5, =0x05040302	@ sv5 <- EP realize-code: 2<-0, 3<-0, 4<-0, 5<-0
	bl	usbhwReEPs		@ jump to de-realize EPs
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ set uart to generate Rx interrupts
	ldr	rva, =uart0_base
	set	rvb, #1
	str	rvb, [rva, #uart_ier]	@ U0IER <- disable UART0 RDA interrupt
	@ set default i/o port to uart
	ldr	rvb, =vuart0
	vcsti	glv, 4, rvb		@ default in/output port model (gc-safety WARNING: glv must be valid!)
	set	pc,  sv4

usbhwReEPs: @ realize/de-realize EPs
	@ on entry:	rva <- usb_base
	@ on entry:	sv5 <- EPs and EP configuration values, eg. #xv5v4v3v2 (4-bit value per EP)
	and	rvb, sv5, #0x0F
	str	rvb, [rva, #usb_epind]	@ Load endpoint index Reg with physical endpoint no
	lsr	sv5, sv5, #4
	and	rvb, sv5, #0x0F
	str	rvb, [rva, #usb_reep]	@ set EP type to 0, no type, disabled
	lsrs	sv5, sv5, #4
	bne	usbhwReEPs
	set	pc,  lnk
	
usbhwConfigure: @ Configure the device
	set	sv4, lnk
	@ stop uart from generating Rx interrupts (they cause noise on shared READBUFFER)
	ldr	rva, =uart0_base
	set	rvb, #0
	str	rvb, [rva, #uart_ier]	@ U0IER <- disable UART0 RDA interrupt
	@ clear the readbuffer
	ldr	rva, =BUFFER_START
	vcrfi	rva, rva, READ_BF_offset
	set	rvb, #i0
	vcsti	rva, 0, rvb
	@ de-realize target endpoints
	ldr	rva, =usb_base
	ldr	sv5, =0x05040302	@ sv5 <- EP realize-code: 2<-0, 3<-0, 4<-0, 5<-0
	bl	usbhwReEPs		@ de-realize EPs
	@ Realize the Interrupt Out Endpoint (phys 2, log 1, aka 0x01)
	set	rvb, #0x02		@ rvb <- EP phys number = 2
	str	rvb, [rva, #usb_epind]	@ Load endpoint index Reg with physical endpoint no
	set	rvb, #8			@ rvb <- max packet size = 8
	str	rvb, [rva, #usb_maxpsize]	@ set the max packet size
	@ Realize the Interrupt In Endpoint (phys 3, log 1, aka 0x81)
	set	rvb, #0x03		@ rvb <- EP phys number = 3
	str	rvb, [rva, #usb_epind]	@ Load endpoint index Reg with physical endpoint no
	set	rvb, #8			@ rvb <- max packet size = 8
	str	rvb, [rva, #usb_maxpsize]	@ set the max packet size
	@ Realize the BULK OUT Endpoint (phys 4, log 2, aka 0x02)
	set	rvb, #0x04		@ rvb <- EP phys number = 4
	str	rvb, [rva, #usb_epind]	@ Load endpoint index Reg with physical endpoint no
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	seteq	rvb, #64
	setne	rvb, #512
	str	rvb, [rva, #usb_maxpsize]	@ load the max packet size Register
	@ Realize the BULK IN Endpoint (phys 5, log 2, aka 0x02)
	set	rvb, #0x05		@ rvb <- EP phys number = 5
	str	rvb, [rva, #usb_epind]	@ Load endpoint index Reg with physical endpoint no
	ldr	rvb, =USB_FSHS_MODE
	ldr	rvb, [rvb]
	eq	rvb, #0
	seteq	rvb, #64
	setne	rvb, #512
	str	rvb, [rva, #usb_maxpsize] @ load the max packet size Register
	@ Realize the Interrupt Out Endpoint (phys 2, log 1, aka 0x01)
	@ Realize the Interrupt In Endpoint (phys 3, log 1, aka 0x81)
	@ Realize the BULK OUT Endpoint (phys 4, log 2, aka 0x02)
	@ Realize the BULK IN Endpoint (phys 5, log 2, aka 0x02)
	ldr	sv5, =0xa5a4b3b2	@ sv5 <- EP realize-code: 2<-#x0B, 3<-#x0B, 4<-#x0A, 5<-#x0A
	bl	usbhwReEPs		@ realize EPs
	@ enable interrupts for physical EP 3, 4 and 5 (EP 2 is not used)
	set	rvb, #0x44		@ rvb <- 0x44 = ACK STALL NYET / ACK / ACK STALL
	str	rvb, [rva, #0x10]	@ USB Interrupt config <- int on above choices
	set	rvb, #0x3B		@ rvb <- 0x3B
	str	rvb, [rva, #0x90]	@ USB EP  Int Enable  <- EP 0, 1, 4 and 5 Rx and Tx, EP 3 Tx (IN)
	@ set default i/o port to usb
	ldr	rvb, =vusb
	vcsti	glv, 4, rvb		@ default in/output port model (gc-safety WARNING: glv must be valid!)
	set	pc,  sv4

usbhwUnstallEP:	@ Unstall the EndPoint in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rva <- usb_base, rvb <- EP control data (status)
	bic	rvb, rvb, #0x01
	str	rvb, [rva, #usb_ctrl]
	set	pc,  sv4

usbhwStallEP: @ Stall EP in sv5
	set	sv4, lnk
	bl	usbhwEPSet		@ rva <- usb_base, rvb <- EP control data (status)
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #usb_ctrl]
	set	pc,  sv4

usbhwEPSet: @ get control data for EP in sv5
	@ on entry:	sv5 <- EP
	@ on exit:	rva <- usb_base
	@ on exit:	rvb <- EP control data
	and	rvb, sv5, #0x0F		@ rvb <- EP logical number
	lsl	rvb, rvb, #1		@ rvb <- EP physical number (if even)
	tst	sv5, #0x80
	addne	rvb, rvb, #1		@ rvb <- EP physical index
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_epind]	@ USB EP INDEX <- select EP
	ldr	rvb, [rva, #usb_ctrl]
	set	pc,  lnk
	

usbStall: @ stall endpoint 1 (phys 1, log 0, aka 0x80)
	set	rvb, #0x01
	ldr	rva, =usb_base
	str	rvb, [rva, #usb_epind]	@ USB EP INDEX <- select EP to stall
	ldr	rvb, [rva, #usb_ctrl]
	orr	rvb, rvb, #0x01
	str	rvb, [rva, #usb_ctrl]
	b	usbEPx

rdEP:	@ uses sv5, rva, rvb, env, dts, cnt, returns cnt = count
	@ env <- EPNum, dts <- buffer
	ldr	rva, =usb_base
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP in env
	eq	env, #UsbControlOutEP	@ were we reading from  EP 0 SETUP EndPoint?
	ldreq	rvb, [rva, #usb_ctrl]	@	if so,  set USBECtrl to initiate DATA phase
	orreq	rvb, rvb, #4
	streq	rvb, [rva, #usb_ctrl]	@	if so,  set USBECtrl to initiate DATA phase
	ldr	cnt, [rva, #usb_rxplen]	@ r11 <- number of bytes to read + possible junk bits
	ldr	rvb, =0x3FF		@ rvb <- mask to get number of bytes
	and	cnt, cnt, rvb		@ r11 <- number of bytes to read
	add	sv5, cnt, #3		@ r5  <- number of bytes to read + 3
	lsr	sv5, sv5, #2		@ r5  <- number of words to read
	@ read data
rdEP_1:	eq	sv5, #0			@ done reading?
	ldrne	rvb, [rva, #usb_rxdata]	@	if not, rvb <- next word read
	strne	rvb, [dts]		@	if not, store next word in buffer
	addne	dts, dts, #4		@	if not, r10 <- updated data storage address
	subne	sv5, sv5, #1		@	if not, r5  <- remaning number of words to read
	bne	rdEP_1			@	if not, jump to keep reading
	@ return
	set	pc,  lnk		@ return

	
wrtEP:	@ uses sv5, rva, rvb, env, dts, cnt
	@ env <- EPNum, dts <- buffer, cnt <- cnt
	@ set endpoint to use in control register
	ldr	rva, =usb_base
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP in env
	eq	env, #UsbControlInEP		@ were we writing 0 bytes to EP0 Control IN EndPoint?
	bne	wrtEPq
	ldr	rvb, [rva, #usb_ctrl]	@	if so,  set USBECtrl to initiate Status IN phase
	tst	rvb, #0x02
	eqeq	cnt, #0
	beq	wrtEPS			@	if so,  jump to possibly get ready for Status OUT phase
	tst	rvb, #0x02
	orreq	rvb, rvb, #4
	streq	rvb, [rva, #usb_ctrl]	@	if so,  set USBECtrl to initiate Status IN phase
wrtEPq:	@
	tst	env, #0x0E
	beq	wrtEP0
	ldr	rvb, [rva, #usb_ctrl]
	tst	rvb, #0x20		@ is buffer ready?
	subne	pc,  pc, #16
wrtEP0:	@
	str	cnt, [rva, #usb_txplen]	@ set number of bytes to write
	add	sv5, cnt, #3		@ sv5  <- number of bytes to write + 3
	lsr	sv5, sv5, #2		@ sv5  <- number of words to write
	@ write data
wrtEP1:	subs	sv5, sv5, #1		@ sv5  <- how many words to write after this one, is it zero?
	ldrne	rvb, [dts]		@	if not, rvb <- next word from buffer
	strne	rvb, [rva, #usb_txdata]	@	if not, write next word out to USB
	addne	dts, dts, #4		@	if not, dts <- updated data storage address
	bne	wrtEP1			@	if not, jump to keep writing
	@ shift last bytes if needed and write them out as a word
	ldr	rvb, [dts]		@ rvb <- last word to write, from buffer
	str	rvb, [rva, #usb_txdata]	@ write last word to USB
	@ return
	set	pc,  lnk		@ return
wrtEPS:	@ get ready for status OUT phase
	set	env, #UsbControlOutEP	@ were we writing 0 bytes to EP0 Control IN EndPoint?
	str	env, [rva, #usb_epind]	@ USB EP  INDEX <- select EP in env
	set	rvb, #2
	str	rvb, [rva, #usb_ctrl]	@	if so,  set USBECtrl to initiate Status IN phase
	set	rvb, #0xff
	ldr	rva, =USB_SETUP_BUFFER
	str	rvb, [rva]		@ rvb  <- reqtyp(8), request(8), val(16) or 0xFF if Status Phase
	ldr	rva, =usb_base
	set	pc,  lnk		@ return	

wrtEPU:	@ (eg. section 9.14)
	@ env <- EPNum, dts <- buffer, cnt <- count
	set	sv4, lnk
	bl	wrtEP
	ldr	rva, =usb_base
	str	sv1, [rva, #usb_iclear_dv] @ clear USB interrupt
	set	pc,  sv4

usbrdychk: @ check if write buffer is full for EP 5 (bulk IN)
	orr	lnk, sv3, #lnkbit0	@ lnk <- lnk, restored
	b	usbwr6

usbhwr:	@ write writebuffer to USB Bulk IN endpoint 5 (phys 5, log 2, aka 0x82) (eg. section 9.14)
	@ on entry:	sv5 <- USB Write Buffer
	@ on entry:	rvb <- number of bytes to send
	@ modifies:	sv3, sv5, rva, rvb
	@ set endpoint to use in control register
	set	sv3, #5
	ldr	rva, =usb_base
	str	sv3, [rva, #usb_epind]	@ USB EP  INDEX <- select EP 5
	str	rvb, [rva, #usb_txplen]	@ set number of bytes to write
	add	sv3, rvb, #3		@ r3  <- number of bytes to write + 3
	lsr	sv3, sv3, #2		@ r3  <- number of words to write
	@ write data
	add	sv5, sv5, #8
usbwr1:	subs	sv3, sv3, #1		@ r5  <- how many words to write after this one, is it zero?
	ldrne	rvb, [sv5]		@	if not, rvb <- next word from buffer
	strne	rvb, [rva, #usb_txdata]	@	if not, write next word out to USB
	addne	sv5, sv5, #4		@	if not, r1  <- updated data storage address
	bne	usbwr1			@	if not, jump to keep writing
	@ shift last bytes if needed and write them out as a word
	ldr	rvb, [sv5]		@ rvb <- last word to write, from buffer
	str	rvb, [rva, #usb_txdata]	@ write last word to USB
	@ return
	b	usbwcn				@ return



