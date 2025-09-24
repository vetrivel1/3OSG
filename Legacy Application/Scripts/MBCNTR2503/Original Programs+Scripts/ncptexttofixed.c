/* ******************************************

	ncptexttofixed.c

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

{	int InLength, OutLength, OutHandle, r, w, FileSize, rcount, rMax,
		i, hCount;
   char *s, *t, *OutBuffer, *InPath, *OutPath;
   FILE *InHandle;
   struct stat statbuf;

	printf("\n\n******* B E G I N   N C P T E X T T O F I X E D *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B C\"", argv[0]);
      printf("\n  A = Full path for input");
      printf("\n  B = Output record length");
      printf("\n  C = Header records to be removed (optional)");
      printf("\n\n    \"%s /users/public/1303178.dat 326 1\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   OutLength=atoi(argv[2]);
   if(argc>3)
   	hCount=atoi(argv[3]);
   else
   	hCount=0;
   sprintf(scratch, "%s.%d", InPath, OutLength);
   OutPath=strdup(scratch);
   if(OutLength<10)
   {	printf("\nRecord Length (Out: %d) must be > 9", OutLength);
      printf("\n\n");
   	exit(1);
   }
   InHandle=fopen(InPath, "rt");
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   OutBuffer=(char *)malloc(OutLength);
   rcount=0;
   rMax=0;
   InLength=sizeof(scratch);
   for(i=0; i<hCount; i++)
   	fgets(scratch, InLength, InHandle);
   fgets(scratch, InLength, InHandle);
   while(!feof(InHandle))
   {	rcount++;
   	r=strlen(scratch);
   	if(r>rMax)
   	{	rMax=r;
      	printf("\nrMax=%d, rcount=%d", rMax, rcount);
      }
   	memset(OutBuffer, ' ', OutLength);
   	if(r<OutLength)
	   	memcpy(OutBuffer, scratch, r);
      else
      	memcpy(OutBuffer, scratch, OutLength);
    	w=write(OutHandle, OutBuffer, OutLength);
	   fgets(scratch, InLength, InHandle);
   }
   free(OutBuffer);
   fclose(InHandle);
   close(OutHandle);
   printf("\n");
   printf("\n\"%s\":  %d records", OutPath, rcount);
   printf("\n         OutLength=%d, InMaxLine=%d", OutLength, rMax);
   printf("\n\n");
	return(0);
}

