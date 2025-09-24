/* ******************************************

	imbsetup2.c

   add ORIGINAL JOB# AND OPTION NAME to the SQL OUTPUT FILE

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

#define ACCOUNT_PCT 3.0
#define MINIMUM_RECORD_COUNT 100

char *m0[] = {
	"CertNum",
   "LLDBUser1",
   "LLDBUser2",
   "LLDBUser3",
   "LLDBUser4",
   "LLDBUser5",
   "LLDBUser6",		// next 5 fields are added to END OF RECORD
	"EbpPaymentDueDate",
	"EbpPaymentAmount",
	"EbpLatePaymentDueDate",
	"EbpLatePaymentAmount",
	"EbpEmailAddress"
};

char *n0[] = {
	"FormattedAccount",
	"Address1",
	"Address2",
	"Address3",
	"Address4",
	"Address5",
	"City",
	"State",
   "Zip5",
   "Zip4",
   "Dpbc",
   "Country"
};

typedef struct s_imbmap {
	field_rec *f0;
   field_rec *g0;
} imbmap_rec;


char *ddBase="/users2/letters/dd/aztest";
char *ddOutPath="/users2/letters/dd/aztest/imb700.dd";

char *Delimiter="|\t";

char scratch[8192];

char *Trim(char *s0);
int JobOk(char *s0);
int main(int argc, char *argv[])

{	int InHandle, CheckOutHandle, InLength, rCount, ImbLength,
		ImbIdLength, ShortCount;
   char *InPath, *InFile, *CheckOutPath, *OutPath, *OutFile, *InBuffer,
   	*OutBuffer, *ddInPath, *Job, *EmailString, *eorPart;
   char *s, *s0, *s1, *t1;
   dd_rec *ddOut, *ddIn;
   field_rec *fImbId, *fImbServiceType, *fImbSequence, *fPostalError,
   	*foCszOffset, *foCszLength, *fState;
   field_rec **fm0, **fm1;
   int fm0Count;
   char **m1, **n1;
   int m0Count, n0Count;
   imbmap_rec *i0, *i1;
   int i0Count, i0Limit;
   int FileOk, IsAccount, IsJob, ParseCsz, AddToEndOfRecord;
   short oCszLength, oCszOffset;

	printf("\n\n******* B E G I N   I M B S E T U P 2 *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"%s A B C D\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  B = Record Length");
      printf("\n  C = Input record layout");
      printf("\n  D = Original job number");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/16860.grp 1000 imbacq300.dd 1116860\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   for(s=InPath+strlen(InPath)-1; s>=InPath && *s!='/'; s--)
   	;
   s++;
   InFile=s;
   InLength=atoi(argv[2]);
   sprintf(scratch, "%s/%s", ddBase, argv[3]);
   ddInPath=strdup(scratch);
   Job=strdup(argv[4]);
   InHandle=OpenFile(InPath, "", IN, NULL);
   sprintf(scratch, "/users/public/plt/%s.imb5.txt", InFile);
   OutPath=strdup(scratch);
   for(s=OutPath+strlen(OutPath)-1; s>=OutPath && *s!='/'; s--)
   	;
   s++;
   OutFile=s;
   sprintf(scratch, "/users/public/plt/check/%s", OutFile);
   CheckOutPath=strdup(scratch);
   CheckOutHandle=OpenFile(CheckOutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   ImbLength=700;
   OutBuffer=(char *)malloc(sizeof(scratch));
   rCount=0;
   ddOut=ddLoadFromFile(ddOutPath);
   ddIn=ddLoadFromFile(ddInPath);
   m0Count=sizeof(m0)/sizeof(char *);
   fm0Count=m0Count;
   fm0=(field_rec **)malloc(fm0Count*sizeof(field_rec *));
   memset(fm0, 0, fm0Count*sizeof(field_rec *));
   for(fm1=fm0, m1=m0; m1-m0<m0Count; m1++, fm1++)
   {	*fm1=GetFieldRecByName(ddIn, *m1);
   	if(*fm1!=NULL)
      	(*fm1)->Offset+=ImbLength;
   }
   n0Count=sizeof(n0)/sizeof(char *);
   i0Limit=n0Count+1;
   i0Count=n0Count;
   i0=(imbmap_rec *)malloc(i0Limit*sizeof(imbmap_rec));
   memset(i0, 0, i0Limit*sizeof(imbmap_rec));
   for(n1=n0, i1=i0; n1-n0<n0Count; n1++, i1++)
   {	i1->f0=GetFieldRecByName(ddIn, *n1);
   	if(i1->f0!=NULL)
      	i1->f0->Offset+=ImbLength;
   	i1->g0=GetFieldRecByName(ddOut, *n1);
   }
   fImbId=GetFieldRecByName(ddOut, "ImbId");
   fImbServiceType=GetFieldRecByName(ddOut, "ImbServiceType");
   fImbSequence=GetFieldRecByName(ddOut, "ImbSequence");
   fPostalError=GetFieldRecByName(ddOut, "PostalError");
   foCszOffset=GetFieldRecByName(ddOut, "OrigCszOffset");
   foCszLength=GetFieldRecByName(ddOut, "OrigCszLength");
   fState=GetFieldRecByName(ddOut, "State");
   FileOk=1;
   IsJob=0;
   IsAccount=0;
   ShortCount=0;
   ImbIdLength=-1;
   eorPart=(char *)malloc(sizeof(scratch));
   while(read(InHandle, InBuffer, InLength)>0)
   {	AddToEndOfRecord=0;
   	rCount++;
   	if(rCount==1)
      {	memcpy(&oCszOffset, InBuffer+foCszOffset->Offset, sizeof(oCszOffset));
      	memcpy(&oCszLength, InBuffer+foCszLength->Offset, sizeof(oCszLength));
         if(oCszOffset>0 || oCszLength>0)
         {	ParseCsz=1;
         	oCszOffset+=ImbLength;
         	for(i1=i0; i1-i0<i0Count; i1++)
            {	if(i1->f0!=NULL)
            	{	if(strcmp(i1->f0->FieldName, "State")==0
	               || strcmp(i1->f0->FieldName, "Zip5")==0
   	            || strcmp(i1->f0->FieldName, "Zip4")==0)
	            		i1->f0=NULL;
                  else
                  if(strcmp(i1->f0->FieldName, "City")==0)
                  {	i1->f0->Offset=oCszOffset;
                  	i1->f0->Length=oCszLength;
                  }
               }
            }
         }
         else
         	ParseCsz=0;
      }
   	for(i1=i0, s1=s0=OutBuffer; i1-i0<i0Count; i1++, s1+=strlen(s1))
      {	memset(scratch, 0, sizeof(scratch));
  	   	sprintf(scratch, "%.*s", i1->g0->Length, InBuffer+i1->g0->Offset);
         sprintf(s1, "%s%s", Trim(scratch), Delimiter);
      }
     	if(ImbIdLength<0)
     	{	sprintf(scratch, "%.9s", InBuffer+fImbId->Offset);
         for(s=scratch+strlen(scratch)-1; s>=scratch && *s==' '; s--)
  	      	*s=0;
         ImbIdLength=strlen(scratch);
      }
      if(ImbIdLength==6)
      {	sprintf(s1, "%.6s%s", InBuffer+fImbId->Offset, Delimiter);
	      s1+=strlen(s1);
   	   sprintf(s1, "%.*s%s", fImbServiceType->Length, InBuffer+fImbServiceType->Offset, Delimiter);
      	s1+=strlen(s1);
	      sprintf(s1, "%.9s%s", InBuffer+fImbSequence->Offset, Delimiter);
   	   s1+=strlen(s1);
      }
      else
      {	sprintf(s1, "%.9s%s", InBuffer+fImbId->Offset, Delimiter);
	      s1+=strlen(s1);
   	   sprintf(s1, "%.*s%s", fImbServiceType->Length, InBuffer+fImbServiceType->Offset, Delimiter);
      	s1+=strlen(s1);
	      sprintf(s1, "%.6s%s", InBuffer+fImbSequence->Offset, Delimiter);
   	   s1+=strlen(s1);
      }
      sprintf(scratch, "%.*s", fPostalError->Length, InBuffer+fPostalError->Offset);
      sprintf(s1, "%s%s", Trim(scratch), Delimiter);
      s1+=strlen(s1);
   	for(i1=i0; i1-i0<i0Count; i1++, s1+=strlen(s1))
      {	memset(scratch, 0, sizeof(scratch));
      	if(strcmp(i1->g0->FieldName, "FormattedAccount")!=0)
      	{	if(i1->f0!=NULL)
	      	{	sprintf(scratch, "%.*s", i1->f0->Length, InBuffer+i1->f0->Offset);
	      	   sprintf(s1, "%s%s", Trim(scratch), Delimiter);
	         }
   	      else
	   	      strcpy(s1, Delimiter);
	      }
      }
   	for(fm1=fm0, t1=eorPart; fm1-fm0<fm0Count; fm1++, s1+=strlen(s1))
      {	memset(scratch, 0, sizeof(scratch));
      	if(fm1-fm0<7)
      	{	if(*fm1!=NULL)
	      	{	sprintf(scratch, "%.*s", (*fm1)->Length, InBuffer+(*fm1)->Offset);
   	   	   sprintf(s1, "%s%s", Trim(scratch), Delimiter);
      	   }
  	      	else
   	      	strcpy(s1, Delimiter);
         }
         else
      	{	if(*fm1!=NULL)
	      	{	sprintf(scratch, "%.*s", (*fm1)->Length, InBuffer+(*fm1)->Offset);
   	   	   sprintf(t1, "%s%s", Trim(scratch), Delimiter);
      	   }
  	      	else
   	      	strcpy(t1, Delimiter);
            t1+=strlen(t1);
//            printf("\nstrlen(eorPart)=%d", strlen(eorPart));
         }
      }
//      printf("\nA  strlen(eorPart)=%d", strlen(eorPart));
      *(t1-2)=0;
//      printf("\nB  strlen(eorPart)=%d", strlen(eorPart));
		sprintf(s1, "%s%s%s%s%d%s0%s%s\r\n",
      	InFile, Delimiter, Job, Delimiter, rCount, Delimiter, Delimiter, eorPart);
      if(!IsAccount)
      {	s0=OutBuffer;
	      s1=strstr(s0, Delimiter);
   	   if(s1==NULL)
      		FileOk=0;
	      else
         if(s1-s0==0)
         {	FileOk=0;
         	IsAccount=1;
         }
         else
   	   if(s1-s0<3)
      		ShortCount++;
      }
  	   if(rCount==1 && !JobOk(Job))
     	{	FileOk=0;
     		IsJob=1;
      }
      write(CheckOutHandle, OutBuffer, strlen(OutBuffer));
   }
   close(InHandle);
   close(CheckOutHandle);
//   printf("\nStateCount=%d, rCount=%d", StateCount, rCount);
   if(rCount>=MINIMUM_RECORD_COUNT
   && (double)ShortCount*100.0/(double)rCount>=ACCOUNT_PCT)
   {	FileOk=0;
      IsAccount=2;
   }
   if(FileOk)
   {	sprintf(scratch, "mv %s %s", CheckOutPath, OutPath);
   	system(scratch);
      sprintf(scratch, "%s.count", OutPath);
      CheckOutHandle=OpenFile(scratch, "", OUT, NULL);
      memset(scratch, ' ', sizeof(scratch));
      while(sizeof(scratch)<rCount)
      {	write(CheckOutHandle, scratch, sizeof(scratch));
      	rCount-=sizeof(scratch);
      }
      write(CheckOutHandle, scratch, rCount);
      close(CheckOutHandle);
   }
   else
	{	if(IsAccount==2 && IsJob)
   	{	sprintf(scratch, "%d.of.%d.CheckAccountAndJob", ShortCount, rCount);
   		EmailString=strdup(scratch);
      }
   	else
      if(IsAccount==2)
   	{	sprintf(scratch, "%d.of.%d.CheckAccount", ShortCount, rCount);
   		EmailString=strdup(scratch);
      }
      else
		if(IsAccount==1 && IsJob)
   		EmailString=strdup("CheckMissingAccountAndJob");
   	else
      if(IsAccount==1)
   		EmailString=strdup("CheckMissingAccount");
   	else
      if(IsJob)
      	EmailString=strdup("CheckJobNumber");
   	sprintf(scratch, "/users/scripts/email.notify.script %s.%s plt %s pas pmgr pej",
      	Job, InFile, EmailString);
   	system(scratch);
   }
	return(!FileOk);
}

char *Trim(char *s0)

{	char *s1;

   // remove leading spaces
   while(*s0==' ')
    	memmove(s0, s0+1, strlen(s0));
   // remove trailing spaces
	for(s1=s0+strlen(s0)-1; s1>=s0 && *s1==' '; s1--)
  		*s1=0;
   return(s0);
}

int JobOk(char *s0)

{	char *s1;
	int ok;

   ok=1;
   if(strlen(s0)!=7)
   	ok=0;
   else
   	for(s1=s0; *s1!=0 && ok; s1++)
      	if(*s1<'0' || *s1>'9')
         	ok=0;
   return(ok);
}

