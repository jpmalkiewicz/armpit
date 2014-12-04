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
@	SD card low-level interface, SPI and MMC/MCI
@
@	Targets:
@
@		STR7
@		STM32F1, F4
@
@-----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
@
@	SPI Interface
@
@	Code Entry Points:
@
@		sd_cfg		configure hardware
@		sd_fst		set high speed
@		sd_slo		set slow speed
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_spi

  .ifndef cortex

	/* STR711 */

_func_
sd_cfg:	@ configure spi power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- rcc_base = RCC base address
	@ ret:	via lnk (through sd_slo)
	@ disable BSPI
	write	fre, sd_spi, #0x08	@ CSR1    <- disabled
	@ configure CS pin as gpio out and set it hi
	rgcpbt	sd_cs_gpio,  #0x00, sd_cs_pin, 1
	rgcpbt	rva,         #0x04, sd_cs_pin, 0
	rgcpbt	rva,         #0x08, sd_cs_pin, 1
	rgcpbt	rva,     #io_state, sd_cs_pin, 1	@ set CS pin (de-sel SD)
	@ configure: P0.4 <- S1.MISO, P0.5 <- S1.MOSI, P0.6 <- S1.SCLK
	@ 	     P0.7 <- S1.CS (inbound/slave CS) with weak pull-up
	rgcpbf	sd_spi_gpio, #0x00, 4, 8, 0b1111
	rgcpbf	rva,         #0x04, 4, 8, 0b0111
	rgcpbf	rva,         #0x08, 4, 8, 0b1111
	rgcpbt	rva,     #io_state, 7, 1		@ set inbound CS, hi
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_	
sd_fst:	@ configure spi speed (high), phase, polarity
	@ modifies:	rva, rvb
	write	0x41, sd_spi, #0x0c	@ CSR2    <- disabled
	write	0x00, rva,    #0x08	@ CSR1    <- disabled
	write	0x0a, rva,    #0x10	@ CLK_DIV <- 48 MHz / 10 = 4.8 MHz
	write	0x01, rva,    #0x08	@ CSR1    <- enab, master, POL=PHA=0, 8b
	write	0x03, rva,    #0x08	@ CSR1    <- enab, master, POL=PHA=0, 8b
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	@ modifies:	rva, rvb
	write	0x41, sd_spi, #0x0c	@ CSR2    <- disabled
	write	0x00, rva,    #0x08	@ CSR1    <- disabled
	write	0x78, rva,    #0x10	@ CLK_DIV <- 48 MHz / 120 = 400 KHz
	write	0x01, rva,    #0x08	@ CSR1    <- enab, master, POL=PHA=0, 8b
	write	0x03, rva,    #0x08	@ CSR1    <- enab, master, POL=PHA=0, 8b
	set	pc,  lnk

  .endif @ .ifndef cortex

  .ifdef cortex

	/* STM32F1, STM32F4 */

_func_
sd_cfg:	@ configure spi power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- rcc_base = RCC base address
	@ ret:	via lnk (through sd_slo)
  .ifdef STM32F4
	@ Power clock for SPI module (SPI2, SPI4) and enable gpio port (B, E)
	rgcpbt	env, #sd_spi_APB_ofst,sd_spi_APB_bit, 1	@ RCC_APBnENR <- pwr SPI
	rgcpbt	env, #0x30, sd_gpio_AHB_bit, 1		@ RCC_AHB1ENR <- pwr prt
  .endif
	@ SPIn_CR1  <- disable SPIn (clear faults if any)
	read	rvb, sd_spi, #0x08		@ SPIn_SR, read to clr flt flags
	write	fre,    rva, #0x00		@ SPIn_CR1 <- disa SPIn clr flts
  .ifndef STM32F4
	@ configure CS pin as gpio out
	rgcpbf	sd_cs_gpio, #0x04, 0, 4, 1	@ _CRH <- PA.8 out,psh-pul,10MHz
	@ de-select sd
	write	(1<<sd_cs_pin), rva, #0x14	@ clear CS pin
	@ de-select inbound SS pin
	rgcpbf	sd_spi_gpio, #0x00, 16, 20, 8	@ _CRL <- PA.4/SS cfg as input
	write	(1<<4), rva, #0x10		@ set SS pin PA.4 high
	@ config sck, miso and mosi pins as spi (AF push-pull out, GPIOn_CRL/H)
	rgcpbf	rva, #0x00, 20, 32, 0xbbb	@ _CRL <- PA.5,6,7 AFIO (SPI)
  .else @ STM32F4
	@ PINS EITHER:
	@     PB11=CS/GPIO,PB12=NSS[CONNECT to 3V],PB13=CLK,PB14=MISO,PB15=MOSI
	@ OR: PE2=CLK,PE3=CS/GPIO,PE4=NSS[CONNECT to 3V],PE5=MISO,PE6=MOSI
	@ Note: all pins must be < 8 (eg. pn.2 to Pn.6) or > 7 (eg. Pn.8 ...)
	@ configure CS pin as gpio out (eg. PB.11, PE.3)
 	ldr	rva, =sd_cs_gpio
	rgcpbf	rva, #0, sd_cs_pin<<1, (sd_cs_pin+1)<<1, 0b01 @ _MODER   <- out
	rgcpbf	rva, #8, sd_cs_pin<<1, (sd_cs_pin+1)<<1, 0b10 @ _OSPEEDR <- fast
	@ de-select sd
	write	(1<<sd_cs_pin), rva, #0x18	@ GPIOx_BSRR <- set CS pin
	@ config sck, miso and mosi pins as spi (AF,push-pull,fast,pull-up)
	@ set 2-bit clear-mask to 0b11 and set-mask to 0b10 (AF) for SPI pins
	mask	rvc, 1, 0b11, sd_clk, sd_mosi, sd_miso, sd_nss	@ rvc <- clr msk
	mask	cnt, 1, 0b10, sd_clk, sd_mosi, sd_miso, sd_nss	@ cnt <- set msk
	rgrmw	sd_spi_gpio, #0, rvc, cnt	@ GPIOx_MODER   <- AF
	rgrmw	rva, #0x08, rvc, cnt		@ GPIOx_OSPEEDR <- fast
	lsr	cnt, cnt, #1			@ cnt <- 0b01 mask for pull-u/d
	rgrmw	rva, #0x0c, rvc, cnt		@ GPIOx_PUPDR   <- pullup
	@ set 4-bit clear-mask to 0xf and set-mask to 0x5 (AF5) for SPI pins
	mask	rvc, 2, 0xf, sd_clk, sd_mosi, sd_miso	@ rvc <- AFn clr msk
	mask	cnt, 2, 0x5, sd_clk, sd_mosi, sd_miso	@ cnt <- AF5 set msk
	rgrmw	rva, #(0x20+((sd_clk/8)<<2)), rvc, cnt	@ GPIOx_AFRL/H <- AF5
  .endif
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_	
sd_fst:	@ configure spi speed (high), phase, polarity
	@ modifies:	rva, rvb
	read	rvb, sd_spi, #0x08	@ rvb      <- SPIn_SR, rd to clr faults
	write	0x00, rva, #0x00	@ SPIn_CR1 <- disab SPIn, clr faults
  .if SYSTICK_RELOAD < 1000000
	write	0x044, rva, #0x00	@ SPIn_CR1 <- PHA0,POL0,8b,Mstr,Enab,18M
  .else	@ STM32F429 -- 168 MHz core, 84 MHz APB2
@	write	0x354, rva, #0x00	@ SPIn_CR1 <- PHA0,POL0,8b,Mstr,Enab,18M
@ 12/05/2014: speed reduced for STM32F429 (messes-up on write at higher speed)
	write	0x35c, rva, #0x00	@ SPIn_CR1 <- PHA0,POL0,8b,Mstr,Enab,5M
  .endif
	set	pc,  lnk

_func_	
sd_slo:	@ configure spi speed (low), phase, polarity
	@ modifies:	rva, rvb
	read	rvb, sd_spi, #0x08	@ rvb      <- SPIn_SR, rd to clr faults
	write	0x00, rva, #0x00	@ SPIn_CR1 <- disab SPIn, clr faults
  .if SYSTICK_RELOAD < 1000000
	write	0x074, rva, #0x00	@ SPIn_CR1 <- PHA0,POL0,8b,Mstr,Ena,280K
  .else	@ STM32F429 -- 168 MHz core, 84 MHz APB2
	write	0x37c, rva, #0x00	@ SPIn_CR1 <- PHA0,POL0,8b,Mstr,Ena,280K
  .endif
	set	pc,  lnk

  .endif @ .ifdef cortex

.endif  @ sd_is_on_spi

/*------------------------------------------------------------------------------
@
@	MMC/MCI Interface (STM32F1)
@
@	Code Entry Points:
@
@		sd_cfg		configure hardware
@		_sgbrs		get data block from SD card
@		_spbrs		write data block to SD card
@		sd_fst		set high speed
@		sd_slo		set slow speed
@		sd_cmd		send command
@		sdpcmd		send command during initialization
@
@-----------------------------------------------------------------------------*/

.ifdef sd_is_on_mci

_func_
sd_cfg:	@ configure MMC/MCI power and pins for SD card
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- rcc_base = RCC base address
	@ ret:	via lnk (through sd_slo)
	@ power/clock the SDIO peripheral and configure SDIO pins: PD2, PC8-12
	rgcpbt	env, #0x14, 10, 1		@ RCC_AHBENR <- power-up sdio
	rgcpbf	ioportd_base, #0, 8,12,0xb	@ PORTD CRL <- PD2=SDIO AF,PP
	rgcpbf	ioportc_base, #4, 0,20,0xbbbbb	@ PORTC CRH <- PC8-12=SDIO AF,PP
	@ power-up and power-on mci peripheral function
	write	sv2,    sd_mci, #0x00		@ set MCI to power-up phase
	write	0x41b2, rva,    #0x04		@ enab 400KHz MCI CLK,narrow bus
mcipw0:	write	sv3,    rva,    #0x00		@ set MCI to power-on phase
	read	rvb,    rva,    #0x00
	eq	rvb, #3
	bne	mcipw0
	@ return
	set	pc,  lnk			@ return


_func_
_sgbrs:	@ sd block read transfer start/restart
	@ in:	rvc <- block or byte address of block to read (raw int)
	@ in:	sv3 <- buffer to store block data (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ out:	sv3 <- updated buffer
	@ mods:	sv3, sv5, rva, rvb, rvc
	@ prepare for read-block
	bl	sd_pre			@ prepare mci
	@ send cmd 17 (read single block) with arg (block number) in rvc
	set	rvb, 17
	bl	sd_cmd
	eq	rva, #0
	bne	_sgbrs
	@ MCIDataCtl <- 512B, block, from card
	write	0x93, sd_mci, #0x2c
	@ get and save data
	set	rvc, 0
	adr	lnk, _sgbrs
sgb_gd:	@ get-data loop
	read	rvb, rva, #0x34		@ stat
	tst	rvb, #0x3f		@ error?
	bne	sd_cm1			@	if so,  jump to restart
	tst	rvb, #0x220000		@ is data available?
	beq	sgb_gd
	read	rvb, rva, #0x80
	write	rvb, sv3, rvc
	add	rvc, rvc, #4
	eq	rvc, #512
	bne	sgb_gd
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_func_
_spbrs:	@ sd block write transfer start/restart
	@ in:	rvc <- block or byte address of block to write (raw int)
	@ in:	sv3 <- buffer w/block data to wrt to sd (scheme bytevector)
	@ in:	sv5 <- return address (lnk w/o lnkbit0) (scheme int)
	@ mods:	sv5, rva, rvb, rvc
	bl	sd_pre			@ prepare mci
	@ send cmd 24 (write single block) with arg (block number) in rvc
	set	rvb, 24
	bl	sd_cmd
	eq	rva, #0
	bne	_spbrs
	@ MCIDataCtl <- 512B, block, to card
	write	0x91, sd_mci, #0x2c
	@ write data
	set	rvc, 0
	adr	lnk, _spbrs
spb_wd:	@ write-data loop
	read	rvb, rva, #0x34		@ stat
	tst	rvb, #0x3f		@ error?
	bne	sd_cm1			@ if so,  jump to restart
	tst	rvb, #0x044000
	beq	spb_wd
	read	rvb, sv3, rvc
	write	rvb, rva, #0x80
	add	rvc, rvc, #4
	eq	rvc, #512
	bne	spb_wd
	@ wait for DataBlockEnd
	adr	lnk, _spbrs
spb_wt:	@ wait loop
	read	rvb, rva, #0x34		@ stat
	tst	rvb, #0x3f		@ error?
	bne	sd_cm1			@ jump to restart
	tst	rvb, #0x0400
	beq	spb_wt
	read	rvc, rva, #0x14		@ rvc <- response0
spb_ts:	@ wait for card in ready-tran state
	bl	sd_pre			@ prepare mci
	set	rvc,  0			@ rvc <- arg
	set	rvb, 13			@ rvb <- cmd13 (card status)
	bl	sd_cmd
	eq	rva, #0
	it	ne
	eqne	rvb, #9
	bne	spb_ts
	@ return
	orr	lnk, sv5, #lnkbit0
	set	pc,  lnk

_func_
sd_pre:	@ mci-prep subroutine
	write	0,      sd_mci, #0x0c	@ clear previous MCI command
	write	0x07ff, rva,    #0x38	@ clear MCI Stat flags
	write	1<<27,  rva,    #0x24	@ set timeout to > 1e8 in MCIDataTimer
	write	512,    rva,    #0x28	@ set MCIDataLength to 512
	set	pc,  lnk
	
_func_	
sd_cmd:	@ mci-cmd subroutine (put cmd)
	@ on entry: rvb <- cmd
	@ on entry: rvc <- arg
	write	rvc, sd_mci, #0x08	@ set arg in MCIargument
	orr	rvb, rvb,  #0x0440
	write	rvb, rva,    #0x0c
sd_cm0:	@ comand wait loop
	read	rvb, rva,    #0x34	@ stat
	tst	rvb, #0x04		@ cmd timeout?
	bne	sd_cm1
	tst	rvb, #0x40
	beq	sd_cm0
	@ get response
	rgbf	rvb, rva, #0x14,8,12	@ rvb <- response
@	read	rvb, rva,    #0x14	@ response
@	lsr	rvb, rvb, #8
@	and	rvb, rvb, #0x0f
	eq	rvb, #9			@ cmd rcvd w/card rdy & in tran state?
	itT	eq
	seteq	rva, 0
	seteq	pc,  lnk
_func_	
sd_cm1:	@ wait then restart transfer
	@ [also: internal entry]
	wait	1<<18
	rgbf	rvb, sd_mci, #0x14,8,12	@ rvb <- response
@	read	rvb, sd_mci, #0x14	@ response
@	lsr	rvb, rvb, #8
@	and	rvb, rvb, #0x0f
	read	rvc, rva, #0x08		@ rvc <- arg restored (eg. for restart)
	set	pc,  lnk
	
_func_	
sd_slo:	@ configure mci speed (low = 400 KHz), 1-bit bus, clock enabled
	write	0x41b2, sd_mci, #0x04	@ 400KHz,1b bus,CLK enab,HW flow cntrl
	set	pc,  lnk

_func_	
sd_fst:	@ configure mci speed (high = 2 MHz), wide bus, clock enabled
	write	0x4922, sd_mci, #0x04	@ 2 MHz,wide bus,CLK enab,HW flow cntrl
	set	pc,  lnk

_func_	
sdpcmd:	@ function to write a command to SD/MMC card during initialization
	@ on entry:	sv4 <- cmd (scheme int)
	@ on entry:	rvc <- arg (raw int)
	@ on exit:	rvb <- response0
	@ modifies:	rva, rvb
	write	0,      sd_mci, #0x0c	@ clear previous cmd
	write	0x07ff, rva,    #0x38	@ clear stat flags
	write	rvc,    rva,    #0x08	@ set arg in MCIargument
	int2raw	rvb, sv4
	and	rvb, rvb, #0xff
	orr	rvb, rvb, #0x0400
	eq	sv4, #i0
	it	ne
	orrne	rvb, rvb, #0x40
	tst	sv4, #0x10000000
	it	ne
	orrne	rvb, rvb, #0x80
	write	rvb, rva, #0x0c		@ send cmd
sdpcmb:	@ wait for mci not busy
	read	rvb, rva, #0x34
	tst	rvb, #0x3800
	bne	sdpcmb
	wait	0x200000		@ wait a bit more (card needs this?)
	@ if CMD3 (get address), check status and exit with indicator if bad
	eq	sv4, #0x0d		@ CMD3?
	bne	sdpcmc
	read	rvb, rva, #0x34
	eq	rvb, #0x40
	itT	ne
	setne	rvb, 0
	setne	pc,  lnk
sdpcmc:	@ continue
	read	rvb, rva, #0x34
	lsl	rvb, rvb, #21
	lsr	rvb, rvb, #21
	write	rvb, rva, #0x38		@ clear status register
	read	rvb, rva, #0x14		@ rvb <- response0
	set	pc,  lnk
  	
.endif  @ sd_is_on_mci



