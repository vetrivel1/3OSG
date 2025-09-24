/* ******************************************

	ncpflexfields.c

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

{	int OutHandle, i, r, rcount, InLength;
   char *s, *t, *InBuffer, *In1Path, *In2Path, *OutPath;
   FILE *InHandle;
   char **f0, **f1, **fFound;
   int f0Count, f0Limit;

	printf("\n\n******* B E G I N   N C P F L E X F I E L D S *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Input 1 (full path)");
      printf("\n  B = Input 2 (full path)");
      printf("\n  For example: \"%s /users/eric/03178.txt.lldbfields", argv[0]);
      printf("\n                        /users/public/1303178.dat.dd\"");
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   In2Path=strdup(argv[2]);
   sprintf(scratch, "%s.new", In2Path);
   OutPath=strdup(scratch);
   InLength=sizeof(scratch);
   InBuffer=(char *)malloc(InLength);
   f0Count=f0Limit=0;
   f0=NULL;
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InHandle=fopen(In1Path, "rt");
   if(InHandle==NULL)
   {	printf("\n\"%s\" open error", In1Path);
   	exit(1);
   }
   fgets(InBuffer, InLength, InHandle);
   while(!feof(InHandle))
   {	if(f0Count==f0Limit)
   	{	f0Limit+=10;
      	f0=(char **)realloc(f0, f0Limit*sizeof(char *));
         memset(f0+f0Count, 0, 10*sizeof(char *));
      }
      f1=f0+f0Count;
      f0Count++;
      for(s=InBuffer+strlen(InBuffer)-1; s>=InBuffer && (*s=='\r' || *s=='\n'); s--)
      	*s=0;
      *f1=strdup(InBuffer);
	   fgets(InBuffer, InLength, InHandle);
	}
   fclose(InHandle);
   InHandle=fopen(In2Path, "rt");
   if(InHandle==NULL)
   {	printf("\n\"%s\" open error", In2Path);
   	exit(1);
   }
   fgets(InBuffer, InLength, InHandle);
   while(!feof(InHandle))
   {	for(f1=f0, fFound=NULL; f1-f0<f0Count && fFound==NULL; f1++)
   		if(memcmp(InBuffer, *f1, strlen(*f1))==0)
         	fFound=f1;
   	if(fFound==NULL)
      	write(OutHandle, InBuffer, strlen(InBuffer));
      else
      {	s=strstr(InBuffer, ",");
      	if(s!=NULL)
         {	sprintf(scratch, "LLDBUser%c", *(*fFound+9));
         	write(OutHandle, scratch, strlen(scratch));
            write(OutHandle, s, strlen(s));
         }
         else
	      	write(OutHandle, InBuffer, strlen(InBuffer));
      }
	   fgets(InBuffer, InLength, InHandle);
	}
   fclose(InHandle);
   free(InBuffer);
   close(OutHandle);
	return;
}

