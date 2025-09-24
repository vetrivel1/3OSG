/* ******************************************

	ncpcontainer6.c

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

char *ddNcp10Path="/users/eric/ncp10cj.dd";

char scratch[8192];

void main(int argc, char *argv[])

{	int OutHandle, rcount, InLength, iHandle;
   char *s, *t0, *InBuffer, *InPath, *OutPath, Delimiter;
   FILE *InHandle;
   dd_rec *dd;
   field_rec *e0, *e1;
   int e0Count;

	printf("\n\n******* B E G I N   N C P C O N T A I N E R 6 *******\n\n");
	if (argc<2)
   {	printf("\nUsage is \"%s A\"", argv[0]);
      printf("\n  A = Input (full path)");
      printf("\n  For example: \"%s ", argv[0]);
      printf("\n           /users/public/16862.4100.tab.fixed.grp.container.new");
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   Delimiter='|';
   sprintf(scratch, "%s.delimited", InPath);
   OutPath=strdup(scratch);
   InLength=sizeof(scratch);
   InBuffer=(char *)malloc(InLength);
   dd=ddLoadFromFile(ddNcp10Path);
   e0=dd->Field;
   e0Count=dd->FieldCount-1;		// ignore the END OF RECORD field
   OutHandle=open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
		S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
   if(OutHandle==-1)
   {	printf("\n\"%s\" open failure", OutPath);
   	exit(1);
   }
   iHandle=open(InPath, O_RDONLY | O_LARGEFILE);
   if(iHandle==-1)
   {	printf("\r\nFile open error %s", InPath);
      exit(1);
   }
   else
      printf("\r\nFile open successful %s", InPath);
   InHandle=fdopen(iHandle, "r");
   if(InHandle==NULL)
   {  printf("\n_fdopen() returns NULL");
      exit(1);
   }
   else
      printf("\nfdopen() returns a FILE * handle");
//   InHandle=fopen(InPath, "rt");
   if(InHandle==NULL)
   {	printf("\n\"%s\" open error", InPath);
   	exit(1);
   }
   fgets(InBuffer, InLength, InHandle);
   t0=scratch+7000;
   while(!feof(InHandle))
   {	if(memcmp(InBuffer+3, "NCP10", 5)==0)
   	{	for(e1=e0, s=scratch; e1-e0<e0Count; e1++, s+=strlen(s))
      	{	if(strcmp(e1->FieldName, "FILLER")!=0)
	      		sprintf(s, "%.*s", e1->Length, InBuffer+e1->Offset);
         	else
            if(e1->Length==1)
	         	sprintf(s, "%c", Delimiter);
            else
	         	sprintf(s, "%*s%c", e1->Length-1, " ", Delimiter);
	      }
     		strcpy(s, "\r\n");
        	write(OutHandle, scratch, strlen(scratch));
      }
   	else
   	if(memcmp(InBuffer+3, "NCP89", 5)==0)
      {	// first 60 bytes
   		for(e1=e0, s=scratch; e1-e0<11; e1++, s+=strlen(s))
      	{	if(strcmp(e1->FieldName, "FILLER")!=0)
	      	{	sprintf(t0, "%.*s", e1->Length, InBuffer+e1->Offset);
	         	sprintf(s, "%s%c", t0, Delimiter);
   	      }
	      }
         write(OutHandle, scratch, strlen(scratch));
      	write(OutHandle, InBuffer+61, strlen(InBuffer+61));
      }
      else
      {	// first 30 bytes
   		for(e1=e0, s=scratch; e1-e0<10; e1++, s+=strlen(s))
      	{	if(strcmp(e1->FieldName, "FILLER")!=0)
	      	{	sprintf(t0, "%.*s", e1->Length, InBuffer+e1->Offset);
	         	sprintf(s, "%s%c", t0, Delimiter);
   	      }
	      }
         write(OutHandle, scratch, strlen(scratch));
      	write(OutHandle, InBuffer+30, strlen(InBuffer+30));
      }
	   fgets(InBuffer, InLength, InHandle);
	}
   fclose(InHandle);
   close(OutHandle);
	return;
}

