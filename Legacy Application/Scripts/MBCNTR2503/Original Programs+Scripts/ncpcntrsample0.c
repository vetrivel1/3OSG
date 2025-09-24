/* ******************************************

	ncpcntrsample0.c

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

typedef struct s_sample {
	char *Value;
   int Count;
} sample_rec;

char scratch[8192];

void main(int argc, char *argv[])

{	int OutHandle, rcount, InLength, rCount, i, FieldNumber, SampleLimit,
		SampleTotal;
   char *s, *t0, *InBuffer, *InPath, *OutPath, *aCntrID, Delimiter;
   FILE *InHandle;
   sample_rec *m0, *m1, *m2, *mFound, mSave;
   int m0Count, m0Limit;
   parse_rec *p0, *p1;
   int p0Count;
   char **s0, **s1;
   int s0Count, s0Limit;

	printf("\n\n******* B E G I N   N C P C N T R S A M P L E 0 *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Container (full path)");
      printf("\n  B = Container ID of record with sample values");
      printf("\n  C = Field number of sample value in (A)");
      printf("\n  D = Max number of samples");
      printf("\n  For example: \"%s /users/coupons/16860sr.cntr.grp", argv[0]);
      printf("\n            A0310 5 1");
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   aCntrID=strdup(argv[2]);
   FieldNumber=atoi(argv[3]);
   SampleLimit=atoi(argv[4]);
   Delimiter='|';
   sprintf(scratch, "%s.sample", InPath);
   OutPath=strdup(scratch);
   InLength=sizeof(scratch);
   InBuffer=(char *)malloc(InLength);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InHandle=fopen(InPath, "rt");
   if(InHandle==NULL)
   {	printf("\n\"%s\" open error", InPath);
   	exit(1);
   }
   t0=scratch+7000;
   m0=NULL;
   m0Count=m0Limit=0;
   s0=NULL;
   s0Count=s0Limit=0;
   SampleTotal=0;
   fgets(InBuffer, InLength, InHandle);
   while(!feof(InHandle))
   {	rCount++;
   	if(memcmp(InBuffer+3, aCntrID, 5)==0)
      {	p0=ParseExpression(InBuffer, &p0Count, Delimiter, 1, 0, 1);
      	p1=p0+FieldNumber;
			for(m1=m0, mFound=NULL; m1-m0<m0Count && mFound==NULL; m1++)
      		if(strcmp(m1->Value, p1->String)==0)
            	mFound=m1;
      	if(mFound==NULL)
         {	if(m0Count==m0Limit)
         	{	m0Limit+=100;
            	m0=(sample_rec *)realloc(m0, m0Limit*sizeof(sample_rec));
               memset(m0+m0Count, 0, 100*sizeof(sample_rec));
            }
            mFound=m0+m0Count;
            m0Count++;
            mFound->Value=strdup(p1->String);
         }
         mFound->Count++;
   	   RemoveParse(p0, p0Count);
      }
      if(s0Count==s0Limit)
      {	s0Limit+=100;
      	s0=(char **)realloc(s0, s0Limit*sizeof(char *));
         memset(s0+s0Count, 0, 100*sizeof(char *));
      }
      s1=s0+s0Count;
      s0Count++;
      *s1=strdup(InBuffer);
   	if(memcmp(InBuffer+3, "NCP89", 5)==0)
      {	if(mFound->Count<=SampleLimit)
      	{	for(s1=s0; s1-s0<s0Count; s1++)
            	write(OutHandle, *s1, strlen(*s1));
	         SampleTotal++;
      	}
         for(s1=s0; s1-s0<s0Count; s1++)
         	free(*s1);
      	s0Count=0;
      }
	   fgets(InBuffer, InLength, InHandle);
	}
   fclose(InHandle);
   close(OutHandle);
/*   for(m1=m0; m1-m0<m0Count; m1++)
   	for(m2=m1+1; m2-m0<m0Count; m2++)
      	if(strcmp(m2->Value, m1->Value)<0)
         {	memcpy(&mSave, m1, sizeof(sample_rec));
         	memcpy(m1, m2, sizeof(sample_rec));
            memcpy(m2, &mSave, sizeof(sample_rec));
         }
   for(m1=m0, i=0; m1-m0<m0Count; m1++)
   {	printf("\nm1[%d]->Value=\"%s\", Count=%d", m1-m0, m1->Value, m1->Count);
   	i+=m1->Count;
   }
   printf("\nTotal=%d", i);*/
   sprintf(scratch, "%s.total", InPath);
   OutHandle=OpenFile(scratch, "", OUT, NULL);
   memset(scratch, ' ', sizeof(scratch));
   while(SampleTotal>sizeof(scratch))
   {	write(OutHandle, scratch, sizeof(scratch));
   	SampleTotal-=sizeof(scratch);
   }
   write(OutHandle, scratch, SampleTotal);
   close(OutHandle);
	return;
}

