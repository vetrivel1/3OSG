/* ******************************************

	imbremit5.c

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
#include <time.h>

#include "/users/temp/cnp01.h"

char scratch[8192];

int main(int argc, char *argv[])

{	int In2Handle, OutHandle, In1Length, In2Length,
		rCount, In1Total, In2Total, ImbOffset;
	FILE *In1Handle;
   char *In1Path, *In2Path, *OutPath, *In1Buffer, *In2Buffer, *s,
   	*In1File;
   struct stat statbuf;

	printf("\n\n******* B E G I N   I M B R E M I T 5 *******\n\n");
	if (argc<2)
   {	printf("\nUsage is \"%s A\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/60504r.cntr.grp\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   In1Length=sizeof(scratch);
   ImbOffset=1682;
   if(ImbOffset+65>In1Length)
   {	printf("\nImbOffset+65 must be <= InLength (%d+65<=%d)\n",
   		ImbOffset, In1Length);
   	exit(1);
   }
   In1Handle=fopen(In1Path, "rt");
   if(In1Handle==NULL)
   {	printf("\n\"%s\" open failure", In1Path);
   	exit(1);
   }
	for(s=In1Path+strlen(In1Path)-1; s>=In1Path && *s!='/'; s--)
   	;
   s++;
   In1File=s;
   sprintf(scratch, "/users/public/imb0/%s.imb0.remit1", In1File);
   In2Path=strdup(scratch);
   In2Length=202;
   In2Handle=OpenFile(In2Path, "", IN, NULL);
   stat(In1Path, &statbuf);
   In1Total=0;
   stat(In2Path, &statbuf);
   In2Total=statbuf.st_size/In2Length;
   sprintf(scratch, "%s.remit2", In1Path);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   In1Buffer=(char *)malloc(In1Length);
   In2Buffer=(char *)malloc(In2Length);
   rCount=0;
   fgets(In1Buffer, In1Length, In1Handle);
   while(!feof(In1Handle))
   {	if(memcmp(In1Buffer+3, "NCP10", 5)==0)
   	{	read(In2Handle, In2Buffer, In2Length);
	   	memcpy(In1Buffer+ImbOffset, In2Buffer+51, 65);
         In1Total++;
      }
      write(OutHandle, In1Buffer, strlen(In1Buffer));
	   fgets(In1Buffer, In1Length, In1Handle);
      rCount++;
   }
   if(In1Total!=In2Total)
   {	unlink(OutPath);
   	printf("\n\"%s\" record total %d must equal", In1Path, In1Total);
   	printf("\n\"%s\" record total %d\n", In2Path, In2Total);
      exit(1);
   }
   fclose(In1Handle);
   close(In2Handle);
   close(OutHandle);
	return(0);
}

