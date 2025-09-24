/* ******************************************

	cnpgetsubscriber.c

   The logic is designed so that this program
   can be inserted into "cnpimb.script".

   If the full MAILERID is passed as the
   third parm, it will be used.
   If the DEPT is passed, the MAILERID table
   will be accessed and all MAILERIDs found for
   this CLIENT+DEPT combination will be returned
   (in most cases, one MAILERID.)  If more than
   one is returned, it will be the task of
   "cnpgetseq.c" to determine the correct one
   to use.

   Scripts will use the new table once
   the "Subscriber" parameter is removed
   from the script and replaced by the
   appropriate "Department" parameter.
   This new parameter would be passed
   to "cnpimb.script" calls instead of
   "Subscriber".

   The same approach should work for the
   "cnpimbremit" scripts.

   This approach does require that all of
   a client's scripts are modified at the
   same time, but I think that has to happen
   regardless of what approach we take.

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
char *SubscriberListPath="/users/safe/imbseqnum/mailer_id.txt";
char scratch[8192];

int main(int argc, char *argv[])

{	FILE *InHandle;
	int OutHandle, ret, FoundCount;
	char *Job, *OutPath, *InBuffer, *Subscriber, *s, *t,
   	*OverrideSubscriber, *ClientDept;
   struct stat statbuf;

	printf("\n\n******* B E G I N   C N P G E T S U B S C R I B E R *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Job number");
      printf("\n  B = ClientDept (or MAILER ID if not yet converted)");
      printf("\n  For example: \"%s 16860 014080", argv[0]);
      printf("\n               - OR -");
      printf("\n               \"%s 16860 000546", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   Job=strdup(argv[1]);
   ClientDept=strdup(argv[2]);
   OverrideSubscriber=NULL;
   if(strlen(ClientDept)==6
   && strcmp(ClientDept, "000546")!=0
   && strcmp(ClientDept, "200189")!=0
   && strcmp(ClientDept, "201125")!=0
   && strcmp(ClientDept, "895303")!=0
   && strcmp(ClientDept, "897705")!=0
   && strcmp(ClientDept, "898331")!=0
   && strcmp(ClientDept, "298139")!=0
   && strcmp(ClientDept, "206386")!=0
   && strcmp(ClientDept, "103668")!=0
   && strcmp(ClientDept, "206150")!=0)
   {	sprintf(scratch, "/users/public/%s.%s.subscriber", Job, ClientDept);
	  	OutPath=strdup(scratch);
	   if(stat(OutPath, &statbuf)==0)
   	{	sprintf(scratch, "rm %s", OutPath);
   		system(scratch);
	   }
   }
   else
   	return(0);
   InHandle=fopen(SubscriberListPath, "rt");
   if(InHandle==NULL)
   {	printf("\n\"%s\" open error", SubscriberListPath);
   	exit(1);
   }
   InBuffer=scratch+7000;
   FoundCount=0;
   Subscriber=scratch+4000;
   fgets(InBuffer, 512, InHandle);
   s=Subscriber;
   *s=0;
   while(!feof(InHandle))
   {	if(memcmp(InBuffer+2, ClientDept, strlen(ClientDept))==0)
   	{	FoundCount++;
         sprintf(scratch, "%s", InBuffer+9);
         for(t=scratch+strlen(scratch)-1; t>=scratch && (*t=='\r' || *t=='\n'); t--)
           	*t=0;
         sprintf(s, "%s ", scratch);
         s+=strlen(s);
      }
	   fgets(InBuffer, 512, InHandle);
   }
   fclose(InHandle);
   if(strlen(Subscriber)>0)
   {  s=Subscriber+strlen(Subscriber)-1;
   	*s=0;
   	OutHandle=OpenFile(OutPath, "", OUT, NULL);
	   sprintf(scratch, "%s\n", Subscriber);
   	write(OutHandle, scratch, strlen(scratch));
      close(OutHandle);
	   ret=0;
   }
   else
   	ret=1;
	return(ret);
}

