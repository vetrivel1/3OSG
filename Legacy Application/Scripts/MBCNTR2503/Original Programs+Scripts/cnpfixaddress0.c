/* ******************************************

	ncpfixaddress0.c

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
	int Offset;
   int Length;
} offset_rec;

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, InLength, OutHandle, r, w, rCount, rLength,
		wLength, wCount, wOffset;
   char *s, *t, *rBuffer, *InPath, *OutPath, *wBuffer, *ExtractData;
   struct stat statbuf;
   parse_rec *p0, *p1;
   int p0Count;
   offset_rec *f0, *f1;
   int f0Count;

	printf("\n\n******* B E G I N   C N P F I X A D D R E S S 0 *******\n\n");
	if (argc<4)
   {	printf("\nUsage is \"cnpfixaddress0.out A B C\"");
      printf("\n  A = Full path for input");
      printf("\n  B = Input record length");
      printf("\n  C = Offset,Length,Offset,Length,...");
      printf("\n  For example: \"cnpfixaddress0.out /users/coupons/mercurg.mrg 650 162,32\"");
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   rLength=InLength=atoi(argv[2]);
   ExtractData=strdup(argv[3]);
   p0=ParseExpression(ExtractData, &p0Count, ',', 0, 0, 1);
   if(p0Count%2)
   {	printf("\np0Count (%d) must be even", p0Count);
   	exit(1);
   }
   f0Count=p0Count/2;
   f0=(offset_rec *)malloc(f0Count*sizeof(offset_rec));
   memset(f0, 0, f0Count*sizeof(offset_rec));
   for(p1=p0, f1=f0, wLength=0; p1-p0<p0Count; p1+=2, f1++)
   {	f1->Offset=atoi(p1->String);
   	f1->Length=atoi((p1+1)->String);
      wLength+=f1->Length;
      if(f1->Offset+f1->Length>InLength)
      {	printf("\nOffset+Length (%d+%d) must be <= RecordLength (%d)",
				f1->Offset, f1->Length, InLength);
      	exit(1);
      }
   }
   sprintf(scratch, "%s.add0", InPath);
   OutPath=strdup(scratch);
   InHandle=OpenFile(InPath, "", IN, NULL);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   rBuffer=(char *)malloc(rLength);
   rCount=0;
   wLength+=rLength+1;
   wBuffer=(char *)malloc(wLength);
   memset(wBuffer, ' ', wLength);
   wCount=0;
   while((r=read(InHandle, rBuffer, InLength))==InLength)
   {  memcpy(wBuffer, rBuffer, InLength);
   	for(f1=f0, wOffset=rLength; f1-f0<f0Count; f1++)
   	{	memcpy(wBuffer+wOffset, rBuffer+f1->Offset, f1->Length);
      	wOffset+=f1->Length;
      }
   	w=write(OutHandle, wBuffer, wLength);
   	rCount++;
      wCount++;
   }
   if(r==-1 || w!=wLength)
   {	printf("\nInvalid output file \"%s\"", OutPath);
      printf("\n\n");
   	unlink(OutPath);
      exit(1);
   }
   free(rBuffer);
   close(InHandle);
   close(OutHandle);
   sprintf(scratch, "%s.count", InPath);
   OutHandle=OpenFile(scratch, "", OUT, NULL);
   sprintf(scratch, "%d\r\n", wCount);
   write(OutHandle, scratch, strlen(scratch));
   close(OutHandle);
   printf("\n\n\"%s\" has %d records", OutPath, wCount);
      printf("\n\n");
	return(0);
}

