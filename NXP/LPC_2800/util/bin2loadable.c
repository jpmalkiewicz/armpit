/*------------------------------------------------------------------------------
@
@ bin2loadable.c            --   LPC2888 version
@
@-------------------------------------------------------------------------------
@
@ Distributed under The MIT License.

@  Copyright (c) 2013-2014 Hubert Montas

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
//------------------------------------------------------------------------------
//
// bin2loadable.c
//
// Adds a DFU-download header to a LPC2888 bin file, computes crc and
// TEA-encrypts the image.
// The input file size is, at maximum, 62 KB
// so that the code+header fits in at most 64 KB
//
// The output file (.dfu) consists of header + TEA-encrypted code+padding,
// where the size of code+padding is 8-byte aligned
// and matches the size value in the header.
// The header also specifies the crc and TEA-encryption start values.
//
//------------------------------------------------------------------------------
//
// compile with:	gcc -o bin2loadable bin2loadable.c
//
// usage (no cpo):      bin2loadable nameofbinfile.bin
//
// usage (with cpo):    bin2loadable nameofbinfile.bin coprocessorcode.bin
//
// output:		nameofbinfile.bin.dfu
//
//------------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#define max_fname_size	100
#define	header_size	2048
#define code_array_size	0x10000
#define	max_code_size	code_array_size - header_size

int main(int argc, char *argv[])
{
   int	i, idx, itmp;
   char	infname[max_fname_size], outfname[max_fname_size];
   unsigned char code[code_array_size];
   char outfsuffix[] = ".dfu";
   FILE	* infile, * outfile;
   unsigned int * icode = (unsigned int *)code;
   unsigned int crc32, TEAsum, TEAn;

   // clear the code array
   for (idx = 0 ; idx < code_array_size ; idx++) code[idx] = 0;

   // quick check for input arguments
   if (argc < 2)
   {
      printf("\n\t%s ERROR: no input file specified.\n\n", argv[0]);
      return(-2);
   }

   // Open the main input file (.bin) and read the code.
   strcpy(infname,  argv[1]);
   infile = fopen(infname, "rb");
   if (!infile)
   {
      printf("\n\tERROR: input file %s not found.\n\n", infname);
      return(-1);
   }   
   idx = header_size;
   while( 1 )
   {
      itmp = fgetc(infile);
      if (feof(infile)) break;
      if (idx == code_array_size)
      {
         printf("\n\tERROR: input file is larger than %d bytes.\n\n", max_code_size);
         return(-5);
      }
      code[idx++] = itmp;
   }
   fclose(infile);

   // perform 8-byte alignment for end of image
   idx  += 8;
   itmp  = idx + 7;
   itmp &= 0xfffffff8;

   // compute 32-bit crc on aligned image
   crc32 = 0xffffffff;
   for (idx = header_size; idx < itmp; idx++)
   {
      crc32 ^= code[idx];
      for(i = 0; i < 8 ; i++) crc32 = (crc32 >> 1) ^ ((crc32 & 1) ? 0xedb88320 : 0);
   }
   crc32 ^= 0xffffffff;

   // update image header (for NXP TEA encryption Key index 0)
   icode[0]   = 0xe6000010;		// NXP "vector"(cf. LPC3131 UM)
   icode[3]   = 0x84eae58e;		// NXP TEA cipher init vec1
   icode[4]   = 0xb956efa8;		// NXP TEA cipher init vec2
   icode[5]   = crc32;			// 32-bit crc
   icode[6]   = itmp - header_size;	// image size (bytes)

   // perform TEA encryption with NXP Key0: 0x91EC6C69 EACEE0D0 6972503A F69228BF
   icode[(header_size >> 2) - 2] = icode[3];
   icode[(header_size >> 2) - 1] = icode[4];
   for (idx = (header_size >> 2); idx < (itmp>>2); idx += 2)
   {
      TEAn          = 32;
      TEAsum        = 0;
      icode[idx]   ^= icode[idx-2];
      icode[idx+1] ^= icode[idx-1];
      while(TEAn--)
      {
         TEAsum += 0x9e3779b9;
         icode[idx]   += ((icode[idx+1]<<4)+0x91ec6c69)^(icode[idx+1]+TEAsum)^((icode[idx+1]>>5)+0xeacee0d0);
         icode[idx+1] += ((icode[idx]  <<4)+0x6972503A)^(icode[idx]  +TEAsum)^((icode[idx]>>5)  +0xf69228bf);
      }
   }
   icode[(header_size >> 2) - 2] = 0;
   icode[(header_size >> 2) - 1] = 0;

   // Open the output file and write the header + encrypted code+padding
   strcpy(outfname, infname);
   strcat(outfname, outfsuffix);
   outfile = fopen(outfname, "wb");
   if (!outfile)
   {
      printf("\n\tERROR: cannot open output file %s.\n\n", outfname);
      return(-2);
   }
   idx = 0;
   while (idx < itmp) fputc(code[idx++], outfile);
   fclose(outfile);
   return( 0 );
}


