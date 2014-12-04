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
	@ in:	env	<- PMC_base
	@ ret:	via lnk
	@ initialize mcu-id for variables (I2c address if slave enabled)
  .ifdef LPC_13xx
	rgcpbt	env, #0x80, 5, 1	@ SYSAHBCLKCTRL <- power up I2C
	write	sv2, env, #0x04		@ PRESETCTRL    <- de-assert I2C reset
  .endif
	write	mcu_id, I2C0ADR, #0x00	@ I2C0ADR <- set mcu address
	@ return
	set	pc,  lnk



