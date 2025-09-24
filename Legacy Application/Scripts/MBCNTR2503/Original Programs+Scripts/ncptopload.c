/* ******************************************

	ncptopload.c

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

#include "cnp01.h"

char scratch[8192];

void main(int argc, char *argv[])

{	int InHandle, InLength, fOffset, fLength, fCount, i,
		OutHandle, r, rcount, w, wcount, ok;
   char *s0, *s1, *sEnd, *InBuffer, *InPath, *OutPath;

	printf("\n\n******* B E G I N   N C P T O P L O A D *******\n\n");
	if (argc<6)
   {	printf("\nUsage is \"%s A B C D E\"", argv[0]);
      printf("\n  A = Input File (full path)");
      printf("\n  B = Input Record Length");
      printf("\n  C = First field offset");
      printf("\n  D = Field length");
      printf("\n  E = Number of fields");
      printf("\n  For example: \"%s /users/eric/letters/j92703/92703.stop.payment.14.fixed.converted 600 20 50 6", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   fOffset=atoi(argv[3]);
   fLength=atoi(argv[4]);
   fCount=atoi(argv[5]);
   sprintf(scratch, "%s.topload", InPath);
   OutPath=strdup(scratch);
   if(InLength<=0)
   {	printf("\nInput Record Length (%d) must be > 0", InLength);
   	exit(1);
   }
   if(InLength<fOffset+fLength*fCount)
   {	printf("\nRecord Length (%d) must be >= (%d+%d*%d) fOffset+fLength*fCount", InLength, fOffset, fLength, fCount);
   	exit(1);
   }
   InBuffer=(char *)malloc(InLength);
   wcount=1;
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InHandle=OpenFile(InPath, "", IN, NULL);
   memset(scratch, ' ', sizeof(scratch));
   while(read(InHandle, InBuffer, InLength)>0)
   {	for(s0=InBuffer+fOffset, sEnd=s0+fLength*fCount; s0<sEnd; s0+=fLength)
   		if(memcmp(s0, scratch, fLength)==0)
         	for(s1=s0+fLength, ok=0; s1<sEnd && !ok; s1+=fLength)
            	if(memcmp(s1, scratch, fLength)!=0)
               {	memcpy(s0, s1, fLength);
               	memset(s1, ' ', fLength);
                  ok=1;
               }
   	write(OutHandle, InBuffer, InLength);
   	wcount++;
   }
   free(InBuffer);
   close(InHandle);
   close(OutHandle);
	return;
}

