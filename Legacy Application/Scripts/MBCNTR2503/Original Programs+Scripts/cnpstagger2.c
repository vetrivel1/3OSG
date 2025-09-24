/* ******************************************

	cnpstagger2.c

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

#define MIN_STAGGER 100
#define MIN_RECORD 10

#define ALPHA_LIMIT 26
char *Alpha = {"abcdefghijklmnopqrstuvwxyz\0"};

char scratch[8192];
int main(int argc, char *argv[])

{	int InHandle, InLength, StaggerSize, InCount, OutFileCount, i,
		OutHandle, r, rcount, w, wcount, WriteTotal;
   struct stat statbuf;
   char *s, *a, *InPath, *InBuffer, *OutPath, *OutFile;

	printf("\n\n******* B E G I N   C N P S T A G G E R 2 *******\n\n");
	if (argc<4)
   {	printf("\nUsage is \"%s A B C\"", argv[0]);
   	printf("\n  A = Input (full path)");
      printf("\n  B = RecordLength");
      printf("\n  E.g., \"%s /users/public/23275f.out 800 1000\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   StaggerSize=atoi(argv[3]);
   for(s=InPath+strlen(InPath)-1; s>=InPath && *s!='/'; s--)
   	;
   s++;
   sprintf(scratch, "%.*s %s", s-InPath, InPath, s);
   OutPath=strdup(scratch);
   OutFile=OutPath+(s-InPath);
   if(InLength<MIN_RECORD)
   {	printf("\nInLength (%d) must be >= %d", InLength, MIN_RECORD);
     	exit(1);
   }
  	if(StaggerSize<MIN_STAGGER)
   {	printf("\nStaggerSize (%d) must be >= %d", StaggerSize, MIN_STAGGER);
  		exit(1);
   }
   if(stat(InPath, &statbuf)!=0)
   {	printf("\n\"%s\" not found\n", InPath);
   	exit(1);
   }
   InHandle=OpenFile(InPath, "", IN, NULL);
   InBuffer=(char *)malloc(InLength);
   InCount=statbuf.st_size / InLength;
   if(statbuf.st_size%InLength!=0)
   {	printf("\n\"%s\" size (%d) is wrong for InLength (%d)\n", InPath, statbuf.st_size, InLength);
   	exit(1);
   }
   OutFileCount=InCount/StaggerSize;
   if(InCount%StaggerSize)
   	OutFileCount++;
   if(OutFileCount>ALPHA_LIMIT)
   {	printf("\nFile has %d splits (program limit is %d)\n", OutFileCount, ALPHA_LIMIT);
   	exit(1);
   }
   else
   if(OutFileCount==1)
   {	printf("\n\"%s\" does not need to be split\n", InPath);
   	exit(1);
   }
   StaggerSize=InCount/OutFileCount;
   if(InCount%OutFileCount)
   	StaggerSize++;
   if(OutFileCount==2)
   	StaggerSize-=3;
	for(i=0, a=Alpha, rcount=0, WriteTotal=0; i<OutFileCount; i++, a++)
   {	*OutFile=*a;
   	OutHandle = OpenFile(OutPath, "", OUT, NULL);
	   for(wcount=0; wcount<StaggerSize && rcount<InCount; wcount++)
   	{	r=read(InHandle, InBuffer, InLength);
      	if(r<0)
         {	printf("\n\"%s\" read error", InPath);
         	exit(1);
         }
      	rcount++;
   	   w=write(OutHandle, InBuffer, InLength);
         if(w!=InLength)
         {	printf("\n\"%s\" write error", OutPath);
         	exit(1);
         }
         WriteTotal++;
	   }
   	close(OutHandle);
      sprintf(scratch, "touch %s", OutPath);
      for(s=scratch+strlen(scratch)-1; s>=scratch && *s!='.' && *s!='/'; s--)
      	;
      if(*s=='.' && strcmp(s, ".imb0")==0)
      {	*s=0;
       	system(scratch);
      }
      StaggerSize+=11;
   }
   close(InHandle);
   free(InBuffer);
   if(WriteTotal!=InCount)
   {	printf("\nProblem!!!!!!!!!!!");
   	printf("\nIn Record Count = %d", InCount);
      printf("\ndoes not equal Out Record Count = %d", WriteTotal);
      exit(1);
   }
	return(0);
}

