/* ******************************************

	imbremit0.c

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

#include "/users/temp/cnp01.h"

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, rCount, ImbSequence,
		MaxImbSequence;
   char *InPath, *OutPath, *InBuffer, *Subscriber, *ServiceType;

	printf("\n\n******* B E G I N   I M B R E M I T 0 *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"%s A B C D E F\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  B = Record Length");
      printf("\n  C = Imb Starting Sequence Number");
      printf("\n  D = Subscriber");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/16860.grp 2700 123 000546\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   ImbSequence=atoi(argv[3]);
   Subscriber=strdup(argv[4]);
   if(strlen(Subscriber)==6)
   	MaxImbSequence=999999999;
   else
   	MaxImbSequence=999999;
   InHandle=OpenFile(InPath, "", IN, NULL);
   sprintf(scratch, "%s.remit0", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   rCount=0;
   while(read(InHandle, InBuffer, InLength)>0)
   {  if(MaxImbSequence==999999999)
   	{	sprintf(scratch, "%09d", ImbSequence);
	   	memcpy(InBuffer+431, scratch, strlen(scratch));
      }
   	{	sprintf(scratch, "%06d", ImbSequence);
	   	memcpy(InBuffer+431, scratch, strlen(scratch));
      }
      else
//   	memcpy(InBuffer+418, &ImbSequence, sizeof(ImbSequence));
      ImbSequence++;
      if(ImbSequence>MaxImbSequence)
      	ImbSequence=1;
      write(OutHandle, InBuffer, InLength);
      rCount++;
   }
   close(InHandle);
   close(OutHandle);
	return(0);
}

