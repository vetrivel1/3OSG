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

{	char *OutPath, *InPath, *InBuffer, *s, *t;
	unsigned char x, y;
	FILE *InHandle;
   int OutHandle, InLength, ReadCount, wCount, sCount, ok;

	printf("\n\n******* B E G I N   N C P C N T R E X T R A C T V A L I D A T I O N *******\n\n");
	if (argc<2)
   {	printf("\n\nUsage is \"%s A\"\n\n", argv[0]);
   	printf("\n  A - Input (full path)");
      printf("\nFor example:");
      printf("\n     \"%s /users/public/16860.900.txt\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }

   InPath=strdup(argv[1]);

	sprintf(scratch, "%s.suspect", InPath);

   OutPath=strdup(scratch);
   InLength=sizeof(scratch);
	if((InHandle = fopen64(InPath, "rt")) == NULL)
	{
		printf(" Input File could not be opened  %s\n\n", InPath);
		exit(1);
	}
//	InHandle=fopen64(InPath, "rt");
   OutHandle = OpenFile(OutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   ReadCount=0;
   wCount=0;
   fgets(InBuffer, InLength, InHandle);
   while(!feof(InHandle))
   {	ReadCount++;
   	for(s=InBuffer, sCount=0; s-InBuffer<InLength && *s!=0; s++)
      {	x=(unsigned char)*s;
      	if(*s=='\n' && (s==InBuffer || *(s-1)!='\r'))
           	sCount+=4;
      	else
         if(x==255 && (s==InBuffer || *(s-1)=='|'))
         {	for(t=s+1, ok=1; ok; t++)
         	{	y=(unsigned char)*t;
            	if(y!=255)
               	ok=0;
            }
            t--;
         	// if end of field, then entire field
            // consists of HIGH-VALUES and is OK
//            printf("\n*t=%d, y=%d", *t, y);
         	if(*t=='|' || strcmp(t, "\r\n")==0)
            	s=t;
         }
      	if(!isprint(*s))
         	sCount++;
      }
      if(sCount>4)
	   {  write(OutHandle, InBuffer, strlen(InBuffer));
      	wCount++;
         printf("\nReadCount=%d", ReadCount);
      }
	   fgets(InBuffer, InLength, InHandle);
   }
   fclose(InHandle);
   close(OutHandle);
   printf("\n\n");
   printf("\nOutput file:  \"%s\", wCount=%d of %d", OutPath, wCount, ReadCount);
   printf("\n\n");
   return(0);
}

