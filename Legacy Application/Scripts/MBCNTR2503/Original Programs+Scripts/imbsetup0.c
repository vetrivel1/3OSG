/* ******************************************

	imbsetup0.c

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

typedef struct s_required {
	char *FieldName;
   int Required;			// 0-optional, 1-required
} required_rec;

required_rec n0[] = {
	{"Client", 0},
	{"Account", 0},
	{"Address1", 1},
	{"Address2", 0},
	{"Address3", 1},
	{"Address4", 0},
	{"Address5", 0},
	{"City", 1},
	{"ForeignAddress", 0},
	{"State", 1},
   {"Zip5", 1},
   {"Zip4", 0},
   {"Dpbc", 0},
   {"Country", 0},
   {"FormattedAccount", 1},
   {"Pocket2", 0},
   {"Pocket3", 0},
   {"Pocket4", 0},
   {"Pocket5", 0},
   {"Pocket6", 0},
   {"Pocket7", 0},
   {"ProductCode", 0},
   {"ViewProductCode", 0},
   {"StatementDate", 0}
};

typedef struct s_imbmap {
	field_rec *f0;
   field_rec *g0;
} imbmap_rec;


char *ddBase="/users2/letters/dd/aztest";
char *ddOutPath="/users2/letters/dd/aztest/imb700.dd";

char scratch[8192];

void CheckMailingAddress(dd_rec *dd, char *s0);
int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, OutLength, rCount, ImbLength, ImbSequence,
		ParseCsz, MaxImbSequence, ImbSequenceLength;
   char *InPath, *OutPath, *InBuffer, *OutBuffer, *ddInPath, *CszField,
   	*Subscriber, *ServiceType;
   dd_rec *ddOut, *ddIn;
   field_rec *fImbId, *fImbSequence, *fImbServiceType, *fCity, *fCsz, *foCszOffset,
   	*foCszLength, *fInCity;
   required_rec *n1;
   int n0Count;
   imbmap_rec *i0, *i1;
   int i0Count, i0Limit;
   short oCszOffset, oCszLength;

	printf("\n\n******* B E G I N   I M B S E T U P 0 *******\n\n");
	if (argc<7)
   {	printf("\nUsage is \"%s A B C D E F\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  B = Record Length");
      printf("\n  C = Input record layout");
      printf("\n  D = Imb Starting Sequence Number");
      printf("\n  E = Subscriber ID");
      printf("\n  F = Service Type");
      printf("\n  G = ParseCsz");
      printf("\n  H = Csz Field (optional)");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/16860rp.asc 1000 imbmb1000als.dd", argv[0]);
      printf("\n  >             12345 000546 036 0\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=atoi(argv[2]);
   sprintf(scratch, "%s/%s", ddBase, argv[3]);
   ddInPath=strdup(scratch);
   ImbSequence=atoi(argv[4]);
   Subscriber=strdup(argv[5]);
   ServiceType=strdup(argv[6]);
   ParseCsz=atoi(argv[7]);
   if(ParseCsz && argc>8 && strcmp(argv[8], "NOFIELD")!=0)
   	CszField=strdup(argv[8]);
   else
   	CszField=NULL;
   if(strlen(Subscriber)==6)
   {	MaxImbSequence=999999999;
      ImbSequenceLength=9;
   }
   else
   {  MaxImbSequence=999999;
      ImbSequenceLength=6;
   }
   InHandle=OpenFile(InPath, "", IN, NULL);
   sprintf(scratch, "%s.imb0", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   ImbLength=700;
   OutLength=ImbLength;
   OutBuffer=(char *)malloc(OutLength);
   rCount=0;
   ddOut=ddLoadFromFile(ddOutPath);
   ddIn=ddLoadFromFile(ddInPath);
   // check for minimal mailing address
   CheckMailingAddress(ddIn, "Address1");
//   CheckMailingAddress(ddIn, "Address2");
   CheckMailingAddress(ddIn, "Address3");
   CheckMailingAddress(ddIn, "City");
   CheckMailingAddress(ddIn, "State");
   CheckMailingAddress(ddIn, "Zip5");
   oCszOffset=oCszLength=0;
   n0Count=sizeof(n0)/sizeof(required_rec);
   i0Limit=n0Count+1;
   i0Count=n0Count;
   i0=(imbmap_rec *)malloc(i0Limit*sizeof(imbmap_rec));
   memset(i0, 0, i0Limit*sizeof(imbmap_rec));
   for(n1=n0, i1=i0; n1-n0<n0Count; n1++, i1++)
   {	i1->f0=GetFieldRecByName(ddIn, n1->FieldName);
   	if(n1->Required && i1->f0==NULL)
      {	printf("\nField \"%s\" is required in input DD", n1->FieldName);
      	exit(1);
      }
   	i1->g0=GetFieldRecByName(ddOut, n1->FieldName);
   }
   fImbId=GetFieldRecByName(ddOut, "ImbId");
   fImbServiceType=GetFieldRecByName(ddOut, "ImbServiceType");
   fImbSequence=GetFieldRecByName(ddOut, "ImbSequence");
   fCity=GetFieldRecByName(ddOut, "City");
   foCszOffset=GetFieldRecByName(ddOut, "OrigCszOffset");
   foCszLength=GetFieldRecByName(ddOut, "OrigCszLength");
   if(CszField!=NULL)
   {	CheckMailingAddress(ddIn, CszField);
   	fCsz=GetFieldRecByName(ddIn, CszField);
      oCszOffset=fCsz->Offset;
      oCszLength=fCsz->Length;
   }
   else
   if(ParseCsz)
   {	fInCity=GetFieldRecByName(ddIn, "City");
   	oCszOffset=fInCity->Offset;
   	oCszLength=fInCity->Length;
   }
   while(read(InHandle, InBuffer, InLength)>0)
   {	memset(OutBuffer, ' ', OutLength);
      for(i1=i0; i1-i0<i0Count; i1++)
      	if(i1->f0!=NULL)
	     		CopySourceToDestination(OutBuffer, i1->g0, InBuffer, i1->f0, 1, scratch);
      if(CszField!=NULL)
      {	memset(OutBuffer+fCity->Offset, ' ', 41);
      	memcpy(OutBuffer+fCity->Offset, InBuffer+fCsz->Offset, fCsz->Length);
      }
      memcpy(OutBuffer+fImbId->Offset, Subscriber, strlen(Subscriber));
      memcpy(OutBuffer+fImbServiceType->Offset, ServiceType, strlen(ServiceType));
      sprintf(scratch, "%0*d", ImbSequenceLength, ImbSequence);
      memcpy(OutBuffer+fImbSequence->Offset, scratch, strlen(scratch));
      ImbSequence++;
      if(ImbSequence>MaxImbSequence)
      	ImbSequence=1;
      memcpy(OutBuffer+foCszOffset->Offset, &oCszOffset, foCszOffset->Length);
      memcpy(OutBuffer+foCszLength->Offset, &oCszLength, foCszLength->Length);
      memcpy(OutBuffer+OutLength-4, &rCount, sizeof(int));
      write(OutHandle, OutBuffer, OutLength);
      rCount++;
   }
   close(InHandle);
   close(OutHandle);
	return(0);
}

void CheckMailingAddress(dd_rec *dd, char *s0)

{  if(GetFieldRecByName(dd, s0)==NULL)
   {	printf("\nMissing \"%s\" for mailing address", s0);
   	exit(1);
   }
}

