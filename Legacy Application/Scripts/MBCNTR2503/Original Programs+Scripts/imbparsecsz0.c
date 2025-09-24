/* ******************************************

	imbparsecsz0.c

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

{	int InHandle, OutHandle, InLength;
   char *s, *t, *tFound, *InBuffer, *InPath, *OutPath, *Spaces;

	printf("\n\n******* B E G I N   I M B P A R S E C S Z 0 *******\n\n");
	if (argc<2)
   {	printf("\nUsage is \"%s A\"", argv[0]);
		printf("\n  A = Input File (full path)");
      printf("\n  For example: \"%s 0 /users/public/23690.rem.imb0", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   sprintf(scratch, "%s.ready", InPath);
   OutPath=strdup(scratch);
   InLength=700;
   InBuffer=(char *)malloc(InLength);
   Spaces=scratch+7200;
   sprintf(Spaces, "%41.41s", "");
   s=InBuffer+324;
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InHandle=OpenFile(InPath, "", IN, NULL);
   while(read(InHandle, InBuffer, InLength)>0)
   {	for(t=s, tFound=NULL; s-t<240 && tFound==NULL; t-=60)
   	if(memcmp(t, Spaces, 41)!=0)
      	tFound=t;
	   if(tFound!=NULL && tFound<s)
   	{	memcpy(s, tFound, 41);
   		memset(tFound, ' ', 60);
	   }
      write(OutHandle, InBuffer, InLength);
   }
   // write until Max records written
   close(InHandle);
   close(OutHandle);
   free(InBuffer);
	return;
}

