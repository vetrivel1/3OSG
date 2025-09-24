/* ******************************************

	cnpcrlf.c

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

{	int InHandle, InLength, OutLength, MaxLength, i,
		OutHandle, r, rcount, w, wcount;
   char *s, *t, *ReadBuffer, *InPath, *OutPath;
   struct stat statbuf;

	printf("\n\n******* B E G I N   C N P C R L F *******\n\n");
	if (argc<4)
   {	printf("\nUsage is \"cnpcrlf.out A B C\"");
      printf("\n  A = Input Record Length");
      printf("\n  B = Output Record Length");
      printf("\n  C = Input File (full path)");
      printf("\n  For example: \"cnpcrlf.out 1200 1202 /users/public/out/ti407.mtc");
      printf("\n\n");
   	exit(1);
   }
   InLength=atoi(argv[1]);
   OutLength=atoi(argv[2]);
   InPath=strdup(argv[3]);
   sprintf(scratch, "%s.sec", InPath);
   OutPath=strdup(scratch);
   if(InLength<=0)
   {	printf("\nInput Record Length (%d) must be > 0", InLength);
   	exit(1);
   }
   if(OutLength<9)
   {	printf("\nOutput Record Length (%d) must be >= 9", InLength);
   	exit(1);
   }
   if(InLength<OutLength)
   	MaxLength=OutLength;
   else
   	MaxLength=InLength;
   ReadBuffer=(char *)malloc(MaxLength);
   memset(ReadBuffer, ' ', MaxLength);
   wcount=1;
	OutHandle = open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (OutHandle == -1)
	{	printf("\r\nFile open error %s", OutPath);
   	exit(1);
   }
	else
		printf("\r\nFile open successful %s", OutPath);
	InHandle=open(InPath, O_RDONLY | O_LARGEFILE);
   memset(ReadBuffer, ' ', OutLength);
   while(read(InHandle, ReadBuffer, InLength)>0)
   {	memcpy(ReadBuffer+OutLength-2, "\r\n", 2);
   	write(OutHandle, ReadBuffer, OutLength);
   	wcount++;
   }
   close(InHandle);
   free(ReadBuffer);
   close(OutHandle);
	return;
}

