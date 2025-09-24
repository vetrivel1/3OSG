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

typedef struct s_check {
	field_rec *f1;	// field from input file dd
   field_rec *f2;	// field from postal file dd
} check_rec;

char *n0[] = {
	"Account"
};

char *ddBasePath="/users2/letters/dd/aztest";
char *ddImbPath="/users2/letters/dd/aztest/imb700.dd";

char scratch[8192];

int main(int argc, char *argv[])

{	char *s, *t, *In1Path, *In2Path, *OutPath, *In1Buffer, *In2Buffer,
		*ddPath, *OutBuffer;
   int In1Handle, In2Handle, OutHandle, In1Length, In2Length,
   	OutLength;
   long In1Seek;
   int i, InCount, rCount;
   char **n1;
   int n0Count;
   check_rec *c0, *c1;
   int c0Count;
   dd_rec *ddPostal, *ddInput;
   field_rec *fAccount, *foCszOffset, *foCszLength, *fRecordIndex;
   short oCszOffset, oCszLength;
   int RecordIndex, PackLength, IsPacked;
   char *s0, *s1, *t0, *t1, *PackSave, PackSign;

	printf("\n\n******* B E G I N   N C P M A I L M E R G E *******\n\n");
	if (argc<5)
   {	printf("\n\nUsage is \"%s A B C D\"\n\n", argv[0]);
      printf("\nA = Full path to input file");
      printf("\nB = Input record length");
      printf("\nC = Full path to \"grp\" file");
      printf("\nD = Input file DD");
      printf("\n\n");
      printf("\nE.g., \"%s /users/public/cdgtest.out 400", argv[0]);
      printf("\n              /users/public/cdgtest.out.grp imbcdg400.dd");
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   In1Length=atoi(argv[2]);
   In2Path=strdup(argv[3]);
   In2Length=700;
   OutLength=In1Length+In2Length;
   sprintf(scratch, "%s/%s", ddBasePath, argv[4]);
   ddPath=strdup(scratch);
   In1Buffer=(char *)malloc(In1Length);
   In2Buffer=(char *)malloc(In2Length);
   sprintf(scratch, "%s.merged", In1Path);
   OutPath=strdup(scratch);
   OutBuffer=(char *)malloc(OutLength);
   n0Count=sizeof(n0)/sizeof(char *);
   c0Count=n0Count;
   c0=(check_rec *)malloc(c0Count*sizeof(check_rec));
   memset(c0, 0, c0Count*sizeof(check_rec));
   ddPostal=ddLoadFromFile(ddImbPath);
   fAccount=GetFieldRecByName(ddPostal, "Account");
   foCszOffset=GetFieldRecByName(ddPostal, "OrigCszOffset");
   foCszLength=GetFieldRecByName(ddPostal, "OrigCszLength");
   fRecordIndex=GetFieldRecByName(ddPostal, "RecordIndex");
   s0=In2Buffer+fAccount->Offset;
   s1=In2Buffer+fAccount->Offset+fAccount->Length-1;
   ddInput=ddLoadFromFile(ddPath);
   for(n1=n0, c1=c0; n1-n0<n0Count; n1++, c1++)
   {	c1->f1=GetFieldRecByName(ddInput, *n1);
   	if(c1->f1==NULL)
      {	printf("\n\"%s\" not found in \"%s\"", *n1, ddPath);
      	exit(1);
      }
   	c1->f2=GetFieldRecByName(ddPostal, *n1);
   	if(c1->f2==NULL)
      {	printf("\n\"%s\" not found in \"%s\"", *n1, ddImbPath);
      	exit(1);
      }
   }
   In1Handle=OpenFile(In1Path, "", IN, NULL);
   In2Handle=OpenFile(In2Path, "", IN, NULL);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InCount=0;
   rCount=0;
   IsPacked=-1;
   PackSave=scratch+7500;
   while(read(In2Handle, In2Buffer, In2Length)>0)// && InCount<10)
   {//  memcpy(&i, In2Buffer+In2Length-sizeof(int), sizeof(int));
      sprintf(scratch, "%.9s", In2Buffer+In2Length-9);
      i=atoi(scratch);
      RecordIndex=i;
   	In1Seek=i*In1Length;
      lseek(In1Handle, In1Seek, SEEK_SET);
      read(In1Handle, In1Buffer, In1Length);
      if(IsPacked==-1)
      {	if(*s1=='c' || *s1=='f')	// see imbsetup3.c for how this gets here
      	{	IsPacked=1;
         	PackSign=*s1;
            for(s1=s0; s1-s0<fAccount->Length && *s1!=' '; s1++)
            	;
            PackLength=((s1-s0)+1)/2;
         }
      	else
         	IsPacked=0;
      }
      if(IsPacked)
      {	sprintf(scratch, "%.*s%c", s1-s0, s0, PackSign);
      	packit((unsigned char *)scratch, (unsigned char *)PackSave);
      }
      for(c1=c0, n1=n0; c1-c0<c0Count; c1++, n1++)
      {	if(IsPacked)
      		s=PackSave;
      	else
         	s=In2Buffer+c1->f2->Offset;
      	if(memcmp(In1Buffer+c1->f1->Offset, s, c1->f1->Length)!=0)
      	{  printf("\n\n");
         	printf("\nMerging record %d from \"%s\"", rCount, In2Path);
	         printf("\n   with record %d from \"%s\"", i, In1Path);
         	printf("\nField \"%s\" does not match for the two records", *n1);
            printf("\n\"%s\" %s:  \"%.*s\"", In2Path, *n1, c1->f2->Length, In2Buffer+c1->f2->Offset);
            printf("\n\"%s\" %s:  \"%.*s\"", In1Path, *n1, c1->f1->Length, In1Buffer+c1->f1->Offset);
            printf("\n\n");
            exit(1);
         }
      }
      memcpy(OutBuffer, In2Buffer, In2Length);
      memcpy(OutBuffer+In2Length, In1Buffer, In1Length);
      // account
      if(IsPacked)
      {	memset(OutBuffer+fAccount->Offset, ' ', fAccount->Length);
      	memcpy(OutBuffer+fAccount->Offset, PackSave, PackLength);
      }
      // ocsz
      sprintf(scratch, "%.5s", OutBuffer+656);
      oCszOffset=(short)atoi(scratch);
      sprintf(scratch, "%.3s", OutBuffer+662);
      oCszLength=(short)atoi(scratch);
      memset(OutBuffer+656, ' ', 9);
      memcpy(OutBuffer+foCszOffset->Offset, &oCszOffset, sizeof(oCszOffset));
      memcpy(OutBuffer+foCszLength->Offset, &oCszLength, sizeof(oCszLength));
      // record index
      memset(OutBuffer+691, ' ', 9);
      memcpy(OutBuffer+696, &RecordIndex, sizeof(RecordIndex));
      write(OutHandle, OutBuffer, OutLength);
      InCount++;
      rCount++;
   }
   close(In1Handle);
   close(In2Handle);
   close(OutHandle);
   return(0);
}

