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
@	MEMORY INPUT/OUTPUT PORT
@
@-------------------------------------------------------------------------------
@  II.A.6.     Standard Procedures
@  II.A.6.6    Input and Output
@  II.A.6.6.2. input  SUPPORT 3 - memory input port:	memipr, pmmrdc, pmmred
@  II.A.6.6.3. output SUPPORT 3 - memory output port:	memopr, pmmwrc, pmmwrt
@-----------------------------------------------------------------------------*/


	/* memory port, environment+obarray binding and port model	------*/
	BNDVAR	"MEM", vmemry		@ MEM  [keep at 1 for getprt]
	VCTR	vmemry, i0, memipr, memopr

	/* memory input and output port-vectors	------------------------------*/
	VCTR	memipr, i1, val_npofxt, val_memrdc, val_trufxt, val_memred
	VCTR	memopr, i2, val_npofxt, val_memwrc, val_memwrt


/*------------------------------------------------------------------------------
@ 	port functions
@-----------------------------------------------------------------------------*/

	/* read-char / peek-char function for memory input port */
	PRIMIT	memrdc, ufun, 2
	@ on entry:	sv1 <- ((port reg <n>) . port-vector) = full input port
	@ on exit:	sv1 <- char read
	@ modifies:	sv1, sv2, rvb
	@ returns via cnt
	car	sv1, sv1		@ sv1 <- (port reg <n>)
	snoc	sv1, sv2, sv1		@ sv1 <- port,		sv2 <- (reg <n>)
	car	sv2, sv2		@ sv2 <- reg
	lsr	rvb, sv1, #2		@ rvb <- port base adrs without upr 2bit
	lsl	rvb, rvb, #4		@ rvb <- full port base address
  .ifndef cortex
	bytref	rvb, rvb, sv2		@ rvb <- byte from register
  .else
	add	rvb, rvb, sv2, asr #2
	ldrb	rvb, [rvb]
  .endif
	raw2chr	sv1, rvb		@ sv1 <- byte, as scheme char
	set	pc,  cnt		@ return


  .ifndef exclude_read_write

	/* read function for memory input port */
	PRIMIT	memred, ufun, 2
	@ on entry:	sv1 <- ((port reg <n>) . port-vector) = full input port
	@ on exit:	sv1 <- object read
	@ modifies:	sv1, sv2, sv3, rva, rvb
	@ returns via cnt
	car	sv1, sv1		@ sv1 <- (port reg <n>)
	snoc	sv1, sv2, sv1		@ sv1 <- port,		sv2 <- (reg <n>)
	car	sv2, sv2		@ sv2 <- reg
	set	sv3, sv2
	tst	sv2, #(1<<31)		@ is offset negative?
	it	ne
	ngintne	sv2, sv2		@	if so,  sv2 <- positive offset
	bic	sv2, sv2, #0x0c		@ sv2 <- offset aligned to 32-bit
	lsr	rvb, sv1, #2		@ rvb <- port base adrs without upr 2bit
	lsl	rvb, rvb, #4		@ rvb <- full port base address
	wrdref	rva, rvb, sv2		@ rva <- value from register (raw  int)
	raw2int	sv1, rva		@ sv1 <- value (scheme int)
	tst	sv3, #(1<<31)		@ was offset 0+?
	it	eq
	seteq	pc, cnt			@	if so,  return with obj in sv1
	bic	rva, rva, #0x03
	orr	sv2, rva, #i0
	set	sv3, sv1
	mkvu84	sv1			@ sv1 <- #vu8(space-for-4-items)
	lsr	rvc, sv3, #2
	bic	rva, sv2, #0x03
	orr	rva, rva, rvc
	str	rva, [sv1]
	set	pc,  cnt
	
  .endif


	/* write-char function for memory output port */
	PRIMIT	memwrc, ufun, 2
	@ on entry:	sv1 <- char
	@ on entry:	sv2 <- ((port offset ...) . port-vector) = full out port
	@ returns via lnk
	car	sv2, sv2		@ sv2 <- (port offset ...)
	snoc	sv2, sv3, sv2		@ sv2 <- port,	sv3 <- (offset ...)
	car	sv3, sv3		@ sv3 <- offset
	lsr	rvb, sv2, #2		@ rvb <- port base adrs without upr 2bit
	lsl	rvb, rvb, #4		@ rvb <- full port base address
	chr2raw	rva, sv1		@ rva <- byte from schm char (ascii val)
	bytset	rvb, sv3, rva		@ write byte to register
	set	pc,  lnk		@ return

  .ifndef exclude_read_write

	/* write object in sv1 to port in sv2 --  function for memory output port */
	PRIMIT	memwrt, ufun, 2
	@ on entry:	sv1 <- object
	@ on entry:	sv2 <- ((port offset ...) . port-vector) = full out port
	car	sv2, sv2		@ sv2 <- (port offset ...)
	snoc	sv2, sv3, sv2		@ sv2 <- port,	sv3 <- (offset ...)
	car	sv3, sv3		@ sv3 <- offset
	lsr	rvb, sv2, #2		@ rvb <- port base adrs without upr 2bit
	lsl	rvb, rvb, #4		@ rvb <- full port base address
	tst	sv3, #(1<<31)		@ is offset 0+?
	itEE	eq
	int2raweq rva, sv1		@	if so,  rva <- object (raw int)
	ldrne	rva, [sv1]		@	if not, rva <- obj from bytevec
	ngintne	sv3, sv3		@	if not, sv3 <- positive offset
	bic	sv3, sv3, #0x0c		@ sv3 <- offset aligned to 32-bit
	wrdst	rvb, sv3, rva		@ store obj in register (base adrs+ofst)
	b	adr_npofxt		@ return with npo
	
  .endif



