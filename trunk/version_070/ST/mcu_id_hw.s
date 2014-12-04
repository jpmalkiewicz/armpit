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
id_cfg:	/* configure mcu ID */
	@ -- called during hardware init (scheme not yet running)
	@ in:	fre	<-   0 (raw int)
	@ in:	sv1-5	<- 1-5 (raw int)
	@ in:	STR7	env	<- rcc_base = RCC base adrs
	@ in:	STM32x	env	<- rcc_base = RCC base adrs
	@ in:	STM32F4	env	<- rcc_base = RCC base adrs
	@ in:	STR9	env	<- sys_ctrl = scu register base (0x5C002000)
	@ ret:	via lnk
  .ifdef STM32x
	rgcpbt	env, #0x1c, 21, 1	@ RCC_APB1ENR <- enable clock for I2C1
  .endif
  .ifdef STM32F4
	rgcpbt	env, #0x40, 21, 1	@ RCC_APB1ENR <- enable clock for I2C1
  .endif
	write	mcu_id, I2C0ADR, #0x00	@ I2C0ADR     <- mcu adrs for varid
	@ return
	set	pc,  lnk



