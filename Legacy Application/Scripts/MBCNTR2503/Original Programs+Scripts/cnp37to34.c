/* ******************************************

	cnp37to34.c

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

int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, OutLength, rCount;
   char *s, *t, *InBuffer, *InPath, *OutPath;

	printf("\n\n******* B E G I N   C N P 3 7 T O 3 4 *******\n\n");
	if (argc<2)
   {	printf("\nUsage is \"%s A\"", argv[0]);
      printf("\n  A = Input File (full path)");
      printf("\n  For example: \"%s /users/public/16862.asc.extract", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   sprintf(scratch, "%s.3422", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InHandle=OpenFile(InPath, "", IN, NULL);
   InLength=3722;
   OutLength=3422;
   InBuffer=(char *)malloc(InLength);
   rCount=0;
   while(read(InHandle, InBuffer, InLength)>0)
   {	memcpy(InBuffer+3400, InBuffer+3622, 9);
    	write(OutHandle, InBuffer, OutLength);
   	rCount++;
   }
   close(InHandle);
   free(InBuffer);
   close(OutHandle);
	return;
}

