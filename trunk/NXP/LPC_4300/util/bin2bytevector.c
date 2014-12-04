/*------------------------------------------------------------------------------
@
@ bin2loadable.c            --   LPC43xx version
@
@-------------------------------------------------------------------------------
@
@ Distributed under The MIT License.

@  Copyright (c) 2012-2014 Hubert Montas

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
// bin2bytevector.c
//
// converts a binary file to a file containng a scheme bytevector.
// The input file size is, at maximum, 63.5 KB (65024 bytes)
//
//------------------------------------------------------------------------------
//
// compile with:	gcc -o bin2bytevector bin2bytevector.c
//
// usage:		bin2bytevector nameofbinfile.bin
//
// output:		nameofbinfile.bin.scm
//
//------------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#define max_fname_size	100
#define	max_code_size	65024

int main(int argc, char *argv[])
{
   int	idx, itmp;
   char	infname[max_fname_size], outfname[max_fname_size];
   unsigned char code[max_code_size];
   char outfsuffix[] = ".scm";
   FILE	* infile, * outfile;

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
   idx = 0;
   while( 1 )
   {
      itmp = fgetc(infile);
      if (feof(infile)) break;
      if (idx == max_code_size)
      {
         printf("\n\tERROR: input file is larger than %d bytes.\n\n", max_code_size);
         return(-5);
      }
      code[idx++] = itmp;
   }
   fclose(infile);

   // Open the output file and write the bytevector
   itmp = idx;
   strcpy(outfname, infname);
   strcat(outfname, outfsuffix);
   outfile = fopen(outfname, "wb");
   if (!outfile)
   {
      printf("\n\tERROR: cannot open output file %s.\n\n", outfname);
      return(-2);
   }
   fprintf(outfile, "#vu8(");
   idx = 0;
   while (idx < itmp) fprintf(outfile, "%u ", code[idx++]);
   fprintf(outfile, ")\n");
   fclose(outfile);
   return( 0 );
}


