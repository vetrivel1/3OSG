/* ******************************************

	imbsetup3.c

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

char *ddInPath="/users2/letters/dd/aztest/imb700.dd";

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, rCount;
   char *InPath, *OutPath, *InBuffer, *OutBuffer, *s0, *s1, *t0, *t1;
   dd_rec *ddIn;
   field_rec *fAccount, *foCszOffset, *foCszLength, *fRecordIndex;
   short oCszOffset, oCszLength;
   int RecordIndex, i, IsPacked, AccountLength;

	printf("\n\n******* B E G I N   I M B S E T U P 3 *******\n\n");
	if (argc<2)
   {	printf("\nUsage is \"%s A\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/imb0/19719.cntr.grp.imb0", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);;
  	sprintf(scratch, "%s.3", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InLength=700;
   InBuffer=(char *)malloc(InLength);
   OutBuffer=(char *)malloc(InLength);
   ddIn=ddLoadFromFile(ddInPath);
   fAccount=GetFieldRecByName(ddIn, "Account");
   foCszOffset=GetFieldRecByName(ddIn, "OrigCszOffset");
   foCszLength=GetFieldRecByName(ddIn, "OrigCszLength");
   fRecordIndex=GetFieldRecByName(ddIn, "RecordIndex");
   s0=InBuffer+fAccount->Offset;
   t0=OutBuffer+fAccount->Offset;
   t1=OutBuffer+fAccount->Offset+fAccount->Length-1;
   OutPath=scratch+7500;
   InHandle=OpenFile(InPath, "", IN, NULL);
   rCount=0;
   while(read(InHandle, InBuffer, InLength)>0)
  	{	memcpy(OutBuffer, InBuffer, InLength);
     	if(rCount==0)
      {	for(s1=s0+fAccount->Length-1; *s1==' '; s1--)
        		;
        	AccountLength=s1-s0+1;
         // can't unpack anything larger than 10 bytes into
         // the 20 byte account field in the 700 record
         if(FieldIsPrint(s0, AccountLength))
         	IsPacked=0;
         else
         if(AccountLength<11)
	   		IsPacked=FieldIsPacked(s0, AccountLength);
         else
         	IsPacked=0;
      }
  		if(IsPacked)
      {	unpackit((unsigned char *)s0, (unsigned char *)scratch, AccountLength);
        	memcpy(t0, scratch, AccountLength*2-1);
         *t1=*(scratch+AccountLength*2-1);
      }
      // ocsz
      memcpy(&oCszOffset, InBuffer+foCszOffset->Offset, foCszOffset->Length);
      memcpy(&oCszLength, InBuffer+foCszLength->Offset, foCszLength->Length);
      memset(OutBuffer+foCszOffset->Offset, ' ', foCszOffset->Length);
      memset(OutBuffer+foCszLength->Offset, ' ', foCszLength->Length);
      sprintf(scratch, "%05d/%03d", oCszOffset, oCszLength);
      memcpy(OutBuffer+656, scratch, strlen(scratch));
      // record index
      memcpy(&i, InBuffer+fRecordIndex->Offset, fRecordIndex->Length);
      sprintf(scratch, "%09d", i);
      memcpy(OutBuffer+InLength-9, scratch, 9);
      write(OutHandle, OutBuffer, InLength);
     	rCount++;
   }
   close(InHandle);
  	close(OutHandle);
	return(0);
}

