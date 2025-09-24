#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "/users/devel/cnp01.h"

char scratch[8192];

int main(int argc, char *argv[])

{	char *s, *t, *InPath, *OutPath, *InBuffer;
   int InHandle, OutHandle;
   int InLength, Zip3, Use752660245454;

	printf("\n\n******* B E G I N   S E T M T Z I P . 0 1 4 0 *******\n\n");
	if (argc<2)
   {	printf("\n\nUsage is \"%s A\"\n\n", argv[0]);
   	printf("\nA = Full path to 700 input");
      printf("\n\n");
      printf("\nE.g., \"%s /users/eric/a49833rp.cntr.grp.imb0\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }

   InPath=strdup(argv[1]);
   if(strstr(InPath, "amp.cntr.grp")!=NULL)
   	Use752660245454=1;
   else
   	Use752660245454=0;
   InLength=700;
   InBuffer=(char *)malloc(InLength);
   InHandle=OpenFile(InPath, "", IN, NULL);
   sprintf(scratch, "%s.mtzip", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   while(read(InHandle, InBuffer, InLength)>0)
	{	if(Use752660245454)
         memcpy(InBuffer+418, "752660245454", 12);
   	else
      {	sprintf(scratch, "%.3s", InBuffer+356);
	   	Zip3=atoi(scratch);
   	   if(Zip3>=255 && Zip3<=257
      	|| Zip3>=500 && Zip3<=528
	      || Zip3>=610 && Zip3<=619
   	   || Zip3>=623 && Zip3<=629
      	|| Zip3>=640 && Zip3<=647
			|| Zip3==650
   	   || Zip3>=652 && Zip3<=653
      	|| Zip3>=660 && Zip3<=693
	      || Zip3>=739 && Zip3<=749
   	   || Zip3>=768 && Zip3<=769
      	|| Zip3>=780 && Zip3<=782
	      || Zip3==788
   	   || Zip3>=790 && Zip3<=999)
      		// la lockbox
         	memcpy(InBuffer+418, "917169306062", 12);
	      else
   	      memcpy(InBuffer+418, "282721230300", 12);
      }
   	write(OutHandle, InBuffer, InLength);
   }
   close(InHandle);
   close(OutHandle);
   free(InBuffer);
   return(0);
}

