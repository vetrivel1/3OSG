/* ******************************************

	cnpgetseq.c

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
#include <sys/file.h>
#include <time.h>

#include "cnp01.h"

char *ioSafePath="/users/safe/imbseqnum/cnpgetseq.001";
char *ioDefaultPath="/users/eric/cnpgetseq.000";
char scratch[8192];

int main(int argc, char *argv[])

{	int ioHandle, ret, Count, Start, OutHandle, Out2Handle, MaxImbSequence,
		ImbSequenceLength;
	char *Job, *OutPath, *Out2Path, *InFile, *Subscriber, *ioPath,
   	*SubscriberPath;
   struct stat statbuf;
   struct flock Lock;

	printf("\n\n******* B E G I N   C N P G E T S E Q *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"%s A B C D\"", argv[0]);
      printf("\n  A = Job number");
      printf("\n  B = Count ");
      printf("\n  C = StartPath");
      printf("\n  D = Subscriber Id");
      printf("\n  E = InFile (optional)");
      printf("\n  For example: \"%s 16860 360 /users/public/16860.grp.start 000546 16860.grp", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   Job=strdup(argv[1]);
   Count=atoi(argv[2]);
  	Out2Path=strdup(argv[3]);
   Subscriber=strdup(argv[4]);
   sprintf(scratch, "%s.%s", ioSafePath, Subscriber);
   SubscriberPath=strdup(scratch);
   if(stat(SubscriberPath, &statbuf)==0)
   {	ioPath=strdup(SubscriberPath);
   	if(strlen(Subscriber)==6)
		{	MaxImbSequence=999999999;
      	ImbSequenceLength=9;
	   }
      else
	   {  MaxImbSequence=999999;
   	   ImbSequenceLength=6;
      }
   }
   else
   // defaults to original selection logic if there is no subscriber path
   if(strlen(Subscriber)==6)
	{	ioPath=strdup(ioDefaultPath);
   	MaxImbSequence=999999999;
      ImbSequenceLength=9;
   }
   else
   {	sprintf(scratch, "%s.%s", ioDefaultPath, Subscriber);
   	ioPath=strdup(scratch);
      MaxImbSequence=999999;
      ImbSequenceLength=6;
   }
   if(argc>5)
	   InFile=strdup(argv[5]);
   else
   	InFile=NULL;
   memset(scratch, 0, 128);
   ret=0;
   if(stat(ioPath, &statbuf))
   {	printf("\nFile \"%s\" not found\n", ioPath);
      exit(1);
   }
   memset(&Lock, 0, sizeof(Lock));
   ioHandle=open(ioPath, O_RDWR);
   if(ioHandle==-1)
   {	printf("\n\"%s\" open failure", ioPath);
   	exit(1);
   }
   Lock.l_type=F_WRLCK;
   ret=fcntl(ioHandle, F_SETLKW, &Lock);
   if(ret==-1)
   {	printf("\nfcntl() write lock fails for %s", Job);
   	exit(1);
   }
   if(read(ioHandle, scratch, statbuf.st_size)==-1)
   {	printf("\nread() fails for %s", Job);
   	exit(1);
   }
	Start=atoi(scratch);
   printf("\n%s:  %d", Job, Start);
   if(InFile!=NULL)
	   sprintf(scratch, "/users/eric/log/%s.%s.start", Job, InFile);
   else
	   sprintf(scratch, "/users/eric/log/%s.start", Job);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   sprintf(scratch, "%d;%d", Start, Count);
   write(OutHandle, scratch, strlen(scratch));
   close(OutHandle);
   if(Out2Path!=NULL)
   {	Out2Handle=OpenFile(Out2Path, "", OUT, NULL);
	   sprintf(scratch, "%d\n", Start);
   	write(Out2Handle, scratch, strlen(scratch));
	   close(Out2Handle);
   }
   Start+=Count;
   if(Start>MaxImbSequence)
   	Start%=MaxImbSequence;
   sprintf(scratch, "%0*d", ImbSequenceLength, Start);
   lseek(ioHandle, 0L, SEEK_SET);
   write(ioHandle, scratch, strlen(scratch));
   close(ioHandle);
   Lock.l_type=F_UNLCK;
   fcntl(ioHandle, F_SETLKW, &Lock);
   if(ret==-1)
   {	printf("\nfcntl() unlock fails for %s", Job);
   	exit(1);
   }
	return(ret);
}

