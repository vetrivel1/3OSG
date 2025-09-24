/* ******************************************

	cnpcchangerecordlength.c

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

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, InLength, OutLength, OutHandle, r, w, FileSize, rcount, rLength,
		AddToFront, CopyOffset;
   char *s, *t, *rBuffer, *wBuffer, *InPath, *OutPath;
   struct stat statbuf;

	printf("\n\n******* B E G I N   C N P C H A N G E R E C O R D L E N G T H *******\n\n");
	if (argc<4)
   {	printf("\nUsage is \"%s A B C\"", argv[0]);
      printf("\n  A = Full path for input");
      printf("\n  B = Input record length");
      printf("\n  C = Output record length");
      printf("\n  D = ADD_TO_FRONT (optionally, all bytes are added to beginning of record)");
      printf("\n\n    \"%s /users/public/16860p.ace 2761 3361 ADD_TO_FRONT\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   OutLength=atoi(argv[3]);
   if(argc>4 && strcmp(argv[4], "ADD_TO_FRONT")==0 && OutLength>InLength)
   {	AddToFront=1;
      CopyOffset=OutLength-InLength;
   }
   else
   {	AddToFront=0;
   	CopyOffset=0;
   }
   sprintf(scratch, "%s.%d.%d", InPath, InLength, OutLength);
   OutPath=strdup(scratch);
   _stat(InPath, &statbuf);
   FileSize=statbuf.st_size;
   if(InLength<1 || OutLength<1)
   {	printf("\nRecord Lengths (In/Out: %d/%d) must be > 0", InLength, OutLength);
      printf("\n\n");
   	exit(1);
   }
   InHandle=OpenFile(InPath, "", IN, NULL);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   if(InLength<OutLength)
   	rLength=OutLength;
   else
   	rLength=InLength;
   rBuffer=(char *)malloc(rLength);
   if(AddToFront)
   {	wBuffer=(char *)malloc(OutLength);
   	memset(wBuffer, ' ', OutLength);
   }
   memset(rBuffer, ' ', rLength);
   rcount=0;
   while((r=read(InHandle, rBuffer, InLength))==InLength)
   {	if(AddToFront)
   	{	memcpy(wBuffer+CopyOffset, rBuffer, InLength);
      	w=write(OutHandle, wBuffer, OutLength);
      }
   	else
	   	w=write(OutHandle, rBuffer, OutLength);
   	rcount++;
   }
   if(r==-1 || rcount>0 && w!=OutLength)
   {	printf("\nInvalid output file \"%s\"", OutPath);
      printf("\n\n");
   	unlink(OutPath);
      exit(1);
   }
   free(rBuffer);
   close(InHandle);
   close(OutHandle);
   printf("\n\n\"%s\" has %d records", OutPath, rcount);
      printf("\n\n");
	return(0);
}

