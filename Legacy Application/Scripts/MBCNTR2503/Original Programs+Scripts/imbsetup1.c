/* ******************************************

	compile:  cc -Ae imbsetup1.c /users/programs/cnp01.a -o imbsetup1.out -lm

	imbsetup1.c

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
#include <time.h>
#include <math.h>

#include "/users/eric/bctables.h"
#include "/users/temp/cnp01.h"

char *ddPath="/users2/letters/dd/aztest/imb700.dd";

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, rCount, ImbIdLength;
   char *InPath, *OutPath, *InBuffer, *BarCode, *TrackingCode, *s, *AorP,
   	*PostalCode, *Imb0;
	char **r1, **rFound;
   dd_rec *dd;
   field_rec *fIntelligentMailBar, *fPostalCodes, *fImbServiceType,
   	*fImbId, *fImbSequence, *fZip5, *fZip4, *fDpbc, *fError;

	printf("\n\n******* B E G I N   I M B S E T U P 1 *******\n\n");
	if (argc<4)
   {	printf("\nUsage is \"%s A B C\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  B = Record Length");
      printf("\n  C = AorP");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/cdgtest.out.grp 700 P\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   AorP=strdup(argv[3]);
   InHandle=OpenFile(InPath, "", IN, NULL);
   sprintf(scratch, "%s.imb1", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   rCount=0;
   dd=ddLoadFromFile(ddPath);
   fIntelligentMailBar=GetFieldRecByName(dd, "IntelligentMailBar");
   fPostalCodes=GetFieldRecByName(dd, "PostalCodes");
//   if(*AorP=='A')
   {	fImbServiceType=GetFieldRecByName(dd, "ImbServiceType");
   	fImbId=GetFieldRecByName(dd, "ImbId");
      fImbSequence=GetFieldRecByName(dd, "ImbSequence");
      fZip5=GetFieldRecByName(dd, "Zip5");
      fZip4=GetFieldRecByName(dd, "Zip4");
      fDpbc=GetFieldRecByName(dd, "Dpbc");
      fError=GetFieldRecByName(dd, "PostalError");
   }
   TrackingCode=scratch+5000;
   BarCode=scratch+5100;
   ImbIdLength=-1;
   bcsetup();
   while(read(InHandle, InBuffer, InLength)>0)
   {	PostalCode=InBuffer+fError->Offset;
   	Imb0=InBuffer+fIntelligentMailBar->Offset;
      if(*Imb0!=' ')
   	{	sprintf(BarCode, "%.11s", InBuffer+fIntelligentMailBar->Offset+20);
  	   	sprintf(TrackingCode, "%.20s", InBuffer+fIntelligentMailBar->Offset);
      }
      else
      {	strcpy(BarCode, "");
      	if(ImbIdLength<0)
      	{	sprintf(scratch, "%.9s", InBuffer+fImbId->Offset);
	         for(s=scratch+strlen(scratch)-1; s>=scratch && *s==' '; s--)
   	      	*s=0;
            ImbIdLength=strlen(scratch);
         }
         if(ImbIdLength==6)	// subscriber id (ImbId) is 6 digits
	      	sprintf(TrackingCode, "00%.3s%.6s%.9s", InBuffer+fImbServiceType->Offset,
		      	InBuffer+fImbId->Offset, InBuffer+fImbSequence->Offset);
         else
	      	sprintf(TrackingCode, "00%.3s%.9s%.6s", InBuffer+fImbServiceType->Offset,
		      	InBuffer+fImbId->Offset, InBuffer+fImbSequence->Offset);
      }
   	for(s=BarCode+strlen(BarCode)-1; s>=BarCode && *s==' '; s--)
  	   	*s=0;
      for(s=TrackingCode+strlen(TrackingCode)-1; s>=TrackingCode && *s==' '; s--)
        	*s=0;
      if(strlen(BarCode)>0 || strlen(TrackingCode)>0)
	   {	if(bccalc(BarCode, TrackingCode, scratch)==0)
	      {	memcpy(InBuffer+fPostalCodes->Offset, scratch, strlen(scratch));
	      	write(OutHandle, InBuffer, InLength);
	      }
   	   else
      	{	printf("\nBarCode=\"%s\" invalid for record %d", BarCode, rCount);
      		exit(1);
	      }
      }
      else
	     	write(OutHandle, InBuffer, InLength);
      rCount++;
   }
   close(InHandle);
   close(OutHandle);
	return(0);
}

