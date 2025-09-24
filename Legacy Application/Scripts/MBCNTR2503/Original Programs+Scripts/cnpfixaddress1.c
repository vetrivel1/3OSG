/* ******************************************

	cnpfixaddress1.c

****************************************** */
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "/users/temp/cnp01.h"

typedef struct offset_s {
	int FromOffset;
   int ToOffset;
   int Length;
} offset_rec;

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, InLength, OutHandle, OutLength, r, w, rCount,
		wCount, wOffset, ResultOffset, cszOffset, cszLength, cszCount, i, done;
   char *s, *t, *rBuffer, *InPath, *OutPath, *wBuffer, *ExtractData, *Spaces;
   struct stat statbuf;
   parse_rec *p0, *p1;
   int p0Count;
   offset_rec *f0, *f1;
   int f0Count;

	printf("\n******* B E G I N   C N P F I X A D D R E S S 1 *******\n");
	if (argc<6)
   {	printf("\nUsage is \"cnpfixaddress1.out A B C D E F G H\" (F,G,H OPTIONAL)");
      printf("\n  A = Full path for input");
      printf("\n  B = Input record length");
      printf("\n  C = Output record length");
      printf("\n  D = Offset to result code from \"cnpparsecsz.out\"");
      printf("\n  E = FromOffset,ToOffset,Length,FromOffset,ToOffset,Length,...");
      printf("\n\n  \"cnpfixaddress1.out /users/public/12345.mrg.add0.csz 683 650 682 650,162,32\"");
      printf("\n");
      printf("\n  F = CSZ Offset for reset to spaces          (optional)");
      printf("\n  G = CSZ Length for reset to spaces          (optional)");
      printf("\n  H = CSZ Count for reset to spaces           (optional)");
		printf("\n  Use F, G, and H only when CSZ is being moved to a new location and");
      printf("\n     should be removed from original address field when result code = Y");
      printf("\n\n  F represents the largest offset within the original data for CSZ");
      printf("\n  G is the length of the CSZ field within the original data");
      printf("\n  H is the number of fields to look at for removal of original CSZ");
      printf("\n\n  For example: if CSZ could be found at offset 220 or 250 for 30 bytes");
      printf("\n         then \"F G H\" would be \"250 30 2\"\n");
   	exit(1);
   }
   else
      for(i=0; i<argc; i++)
         printf("\nargv[%d]=\"%s\"", i, argv[i]);
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   OutLength=atoi(argv[3]);
   ResultOffset=atoi(argv[4]);
   ExtractData=strdup(argv[5]);
   p0=ParseExpression(ExtractData, &p0Count, ',', 0, 0, 1);
   if(p0Count%3)
   {	printf("\np0Count (%d) must be divisible by 3", p0Count);
   	exit(1);
   }
   f0Count=p0Count/3;
   f0=(offset_rec *)malloc(f0Count*sizeof(offset_rec));
   memset(f0, 0, f0Count*sizeof(offset_rec));
   for(p1=p0, f1=f0; p1-p0<p0Count; p1+=3, f1++)
   {	f1->FromOffset=atoi(p1->String);
   	f1->ToOffset=atoi((p1+1)->String);
   	f1->Length=atoi((p1+2)->String);
      if(f1->FromOffset+f1->Length>InLength)
      {	printf("\nOffset+Length (%d+%d) must be <= InLength (%d)",
				f1->FromOffset, f1->Length, InLength);
      	exit(1);
      }
      if(f1->ToOffset+f1->Length>OutLength)
      {	printf("\nOffset+Length (%d+%d) must be <= OutLength (%d)",
      		f1->ToOffset, f1->Length, OutLength);
      	exit(1);
      }
   }
   if(argc==8 || argc==9)
   {  cszOffset=atoi(argv[6]);
   	cszLength=atoi(argv[7]);
      if(cszOffset<0)
      {  printf("\nCSZ Offset (%d) must be >= 0", cszOffset);
         exit(1);
      }
      if(cszLength<=0)
      {  printf("\nCSZ Length (%d) must be >= 1", cszLength);
         exit(1);
      }
      if(cszOffset+cszLength>OutLength)
      {	printf("\nCSZ Offset+Length (%d+%d) must be <= OutLength (%d)",
      		cszOffset, cszLength, OutLength);
      	exit(1);
      }
      if(argc==9)
         cszCount=atoi(argv[8]);
      else
         cszCount=1;
      if(cszCount<=0)
      {  printf("\nCSZ Count (%d) must be >= 1", cszCount);
         exit(1);
      }
      Spaces=(char *)malloc(cszLength);
      memset(Spaces, ' ', cszLength);
   }
   else
   {	cszOffset=0;
   	cszLength=0;
      cszCount=0;
   }
   sprintf(scratch, "%s.add1", InPath);
   OutPath=strdup(scratch);
   InHandle=OpenFile(InPath, "", IN, NULL);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   rBuffer=(char *)malloc(InLength);
   rCount=0;
   wBuffer=(char *)malloc(OutLength);
   wCount=0;
   while((r=read(InHandle, rBuffer, InLength))==InLength)
   {  if(*(rBuffer+ResultOffset)=='Y')
      {	memcpy(wBuffer, rBuffer, OutLength);
			if(*(rBuffer+InLength-1)=='Y')	// address is fixed
	   		for(f1=f0; f1-f0<f0Count; f1++)
   				memcpy(wBuffer+f1->ToOffset, rBuffer+f1->FromOffset, f1->Length);
         if(cszLength>0)
         {  for(s=wBuffer+cszOffset, done=0, i=0; i<=cszCount && s>=wBuffer && !done; s-=cszLength, i++)
               if(memcmp(s, Spaces, cszLength)!=0)
               { 	memset(s, ' ', cszLength);
                  done=1;
               }
         }
	   	w=write(OutHandle, wBuffer, OutLength);
      }
      else
      	w=write(OutHandle, rBuffer, OutLength);
   	rCount++;
      wCount++;
   }
   if(r==-1 || w!=OutLength)
   {	printf("\nInvalid output file \"%s\"", OutPath);
      printf("\n\n");
   	unlink(OutPath);
      exit(1);
   }
   free(rBuffer);
   close(InHandle);
   close(OutHandle);
   printf("\n\n\"%s\" has %d records", OutPath, wCount);
      printf("\n\n");
	return(0);
}

