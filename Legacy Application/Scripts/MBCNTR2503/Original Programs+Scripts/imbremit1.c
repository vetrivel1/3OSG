/* ******************************************

	compile:  cc -Ae imbremit1.c /users/programs/cnp01.a -o imbremit1.out -lm

	imbremit1.c

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

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, OutLength, rCount, MailerIdLength,
		ImbSequence;
   char *InPath, *OutPath, *InBuffer, *OutBuffer, *BarCode, *TrackingCode, *s,
   	*MailerId, *ServiceType;

	printf("\n\n******* B E G I N   I M B R E M I T 1 *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"%s A B C\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  B = Record Length");
      printf("\n  C = Mailer Id");
      printf("\n  D = Service Type");
      printf("\n  For example:");
      printf("\n  > \"%s /users/eric/16860.grp.remit0 2700 000546 041\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
	MailerId=strdup(argv[3]);
   ServiceType=strdup(argv[4]);
   InHandle=OpenFile(InPath, "", IN, NULL);
   sprintf(scratch, "%s.remit1", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   OutLength=202;
   OutBuffer=(char *)malloc(OutLength);
   rCount=0;
   TrackingCode=scratch+5000;
   BarCode=scratch+5100;
   MailerIdLength=strlen(MailerId);
   bcsetup();
   while(read(InHandle, InBuffer, InLength)>0)
   {	sprintf(BarCode, "%.11s", InBuffer+418);
   	sprintf(scratch, "%.9s", InBuffer+431);
      ImbSequence=atoi(scratch);
      if(MailerIdLength==6)	// subscriber id (MailerId) is 6 digits
	     	sprintf(TrackingCode, "00%s%s%09d", ServiceType,
	      	MailerId, ImbSequence);
      else
      	sprintf(TrackingCode, "00%s%s%06d", ServiceType,
	      	MailerId, ImbSequence);
   	for(s=BarCode+strlen(BarCode)-1; s>=BarCode && *s==' '; s--)
  	   	*s=0;
      for(s=TrackingCode+strlen(TrackingCode)-1; s>=TrackingCode && *s==' '; s--)
        	*s=0;
	   if(bccalc(BarCode, TrackingCode, scratch)==0)
      {	memset(OutBuffer, ' ', OutLength);
        	sprintf(OutBuffer, "%.20s%11.11s%.20s%s%84s\r\n",
           	InBuffer+671, BarCode, TrackingCode, scratch, "");
         memcpy(OutBuffer+196, InBuffer+696, 4);
	     	write(OutHandle, OutBuffer, OutLength);
	   }
   	else
      {	printf("\nBarCode=\"%s\" invalid for record %d", BarCode, rCount);
      	exit(1);
	   }
      rCount++;
   }
   close(InHandle);
   close(OutHandle);
	return(0);
}

