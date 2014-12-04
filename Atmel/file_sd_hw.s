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
@	SD card low-level interface
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

_func_
sd_cfg:	/* configure spi power and pins for SD card */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<- 0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	env	<- PMC_base
	@ ret:	via lnk (through sd_slo)
	@ clock (power-up) the SPI peripheral
	write	(1<<sd_spi_per_id), env, #0x10	@ PMC_PCER0 <- enab spi clk/pwr
    .ifdef sd_cs_pin
	write	(1<<sd_cs_pin), sd_cs_gpio, #0x10 @ PIOn_OER  <- CS pin = output
	write	           rvb,        rva, #0x30 @ PIOn_SODR <- CS hi =desel SD
    .endif
	mask	rvb, 0, 1, sd_clk, sd_mosi, sd_miso, sd_nss @ rvb <- spi pn msk
	write	rvb, sd_spi_gpio, #0x4	@ PIOn_PDR <- disa GPIO func for pins
    .ifndef cortex
	write	rvb, rva, #0x70	@ PIOn_ASR <- per-A SPI func for pins
    .else
	set	rvc, rvb		@ rvc          <- 1-bit SPI pin mask
	rgrmw	rva, #0x70, rvc, xxx	@ PIOn_ABCDSR1 <- SR1 SPI func for pins
	rgrmw	rva, #0x74, rvc, xxx	@ PIOn_ABCDSR2 <- SR2 SPI func for pins
    .endif
	@ jump to set speed, polarity, phase
	b	sd_slo

_func_
sd_fst:	/* configure spi speed (high), phase, polarity */
	@ modifies:	rva, rvb
	write	0x81, sd_spi, #0x00		@ SPI_CR <- reset SPI
	write	0x01,    rva, #0x00		@ SPI_CR <- enable SPI
	write	 rvb,    rva, #0x04		@ SPI_MR <- enable master mode
	write	((sd_spi_fst<<8)|2), rva, #0x30	@ SPI_CSR0 <- ~12MHz, POL/PHA=0
	set	pc,  lnk

_func_	
sd_slo:	/* configure spi speed (low), phase, polarity */
	@ modifies:	rva, rvb
	write	0x81, sd_spi, #0x00		@ SPI_CR <- reset SPI
	write	0x01,    rva, #0x00		@ SPI_CR <- enable SPI
	write	 rvb,    rva, #0x04		@ SPI_MR <- enable master mode
	write	((sd_spi_slo<<8)|2), rva, #0x30	@ SPI_CSR0 <- ~12MHz, POL/PHA=0
	set	pc,  lnk

.endif @ sd_is_on_spi

.ltorg



