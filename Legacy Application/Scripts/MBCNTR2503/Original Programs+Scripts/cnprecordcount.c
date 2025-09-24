/* ******************************************

	cnprecordcount.c

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

{	int InHandle, OutHandle, RecordCount, InLength;
   char *s, *InPath, *OutPath;
   struct stat statbuf;
   int64_t FileSize;

	printf("\n\n******* B E G I N   C N P R E C O R D C O U N T *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Full path for input");
      printf("\n  B = Record length");
      printf("\n  For example: \"%s /users/eric/cdgtest.grp 400\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   if(InLength<=0)
   {	printf("\nInLength (%d) must be > 0", InLength);
   	exit(1);
   }
   if(stat(InPath, &statbuf)==0)
   	FileSize=(int64_t)statbuf.st_size;
   else
   if(stat64(InPath, scratch)==0)
	{  // FileSize ends up at offset 24
      // this logic is used because program as written doesn't
      // compile with "struct stat64" rather than "struct stat" buffer
	   memcpy(&FileSize, scratch+24, sizeof(int64_t));
   }
   else
   {	printf("\n\"%s\" stat() failed", InPath);
   	exit(1);
   }
   if(FileSize%InLength!=0)
   {	for(s=InPath+strlen(InPath)-1; s>=InPath && *s!='/'; s--)
  			;
  		s++;
   	printf("\n\"%s\" size (%u)", s, FileSize);
   	printf("\n    should be divisible by InLength (%d)\n", InLength);
  		exit(1);
   }
   RecordCount=(int)(FileSize/InLength);
   sprintf(scratch, "%s.%d", InPath, InLength);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   sprintf(scratch, "%d\n", RecordCount);
   write(OutHandle, scratch, strlen(scratch));
   close(OutHandle);
	return(0);
}

