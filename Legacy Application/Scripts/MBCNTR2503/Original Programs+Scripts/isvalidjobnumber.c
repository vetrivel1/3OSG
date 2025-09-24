#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "/users/temp/cnp01.h"

char scratch[8192];

int main(int argc, char *argv[])

{	char *Job, *s;
	int i, ok;

	printf("\n\n******* B E G I N   I S V A L I D J O B N U M B E R *******\n\n");
   if(argc<2)
   {	printf("\nUsage is:  \"%s A\"", argv[0]);
   	printf("\n  A - 5 digit job number");
      printf("\nFor example:  \"%s 36060", argv[0]);
      printf("\n");
      exit(1);
   }
	Job=strdup(argv[1]);
   ok=1;
   if(strlen(Job)!=5)
   	ok=0;
   for(s=Job; *s!=0 && ok; s++)
   	if(*s<'0' || *s>'9')
      	ok=0;
   if(!ok)
	   printf("\n\"%s\" should be a valid 5-digit job number\n", Job);
   // for standard UNIX script checking of return codes
   // return 0 for OK and 1 for NOT OK
   return(!ok);
}

