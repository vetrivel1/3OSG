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

// first 200 of last 300 bytes is reserved for client-specific info
#define CLIENT_OFFSET 300
// last 100 bytes of output record are reserved for NCP'
#define NCP_OFFSET 100

char scratch[8192];

int main(int argc, char *argv[])

{	char *InPath, *In2Path, *OutPath, *InBuffer, *In2Buffer, *OutBuffer, *s;
	FILE *InHandle;
   int OutHandle, In2Handle, InLength, In2Length, OutLength;
   int i, rCount;

	printf("\n\n******* B E G I N   N C P C N T R 5 *******\n\n");
	if (argc<3)
   {	printf("\n\nUsage is \"%s A B\"\n\n", argv[0]);
      printf("\nA = Full path to extract file");
      printf("\nB = Record length of file from which \".tab\" file is derived");
      printf("\n\n");
      printf("\nE.g., \"%s /users/public/16862.4100.tab 4100", argv[0]);
      printf("\n\n");
   	exit(1);
   }

   InPath=strdup(argv[1]);

   InLength=sizeof(scratch);

   In2Length=atoi(argv[2]);

   In2Path=strdup(InPath);

	for(s=In2Path+strlen(In2Path)-1; s>In2Path && *s!='.'; s--)

   	;

   *s=0;

   OutLength=0;

   InBuffer=(char *)malloc(InLength);

   InHandle=fopen64(InPath, "rt");

   rCount=0;

   fgets(InBuffer, InLength, InHandle);

   while(!feof(InHandle))

   {	rCount++;

   	if(strlen(InBuffer)>OutLength)

      	OutLength=strlen(InBuffer);

      fgets(InBuffer, InLength, InHandle);

   }

   sprintf(scratch, "%s.length", InPath);

   OutHandle=OpenFile(scratch, "", OUT, NULL);

   OutLength+=CLIENT_OFFSET;

   sprintf(scratch, "%d\n", OutLength);

   write(OutHandle, scratch, strlen(scratch));

   close(OutHandle);

   printf("\nrCount=%d, OutLength=%d", rCount, OutLength);

   sprintf(scratch, "%s.new", InPath);

   OutPath=strdup(scratch);

	OutHandle = open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,

				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (OutHandle == -1)
	{	printf("\r\nFile open error %s", OutPath);
   	exit(1);
   }
	else
		printf("\r\nFile open successful %s", OutPath);
   In2Buffer=(char *)malloc(In2Length);

   In2Handle=open(In2Path, O_RDONLY | O_LARGEFILE);

  	if(In2Handle==-1)
   {	printf("\n\"%s\" open error", In2Path);
     	exit(1);
   }
   OutBuffer=(char *)malloc(OutLength);

	fseek(InHandle, 0L, SEEK_SET);

   rCount=0;

   fgets(InBuffer, InLength, InHandle);

   while(!feof(InHandle))

   {	rCount++;

      read(In2Handle, In2Buffer, In2Length);

		memset(OutBuffer, ' ', OutLength);

	   memcpy(OutBuffer, InBuffer, strlen(InBuffer));

   	memcpy(OutBuffer+OutLength-CLIENT_OFFSET, In2Buffer+In2Length-CLIENT_OFFSET, CLIENT_OFFSET);

      write(OutHandle, OutBuffer, OutLength);

      fgets(InBuffer, InLength, InHandle);

   }

   close(OutHandle);

   close(In2Handle);

   fclose(InHandle);

   return(0);

}


