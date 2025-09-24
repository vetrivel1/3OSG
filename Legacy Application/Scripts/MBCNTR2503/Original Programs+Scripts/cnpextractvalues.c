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

typedef struct s_extract {
	char *ddPath;
	dd_rec *dd;
} extract_rec;

char scratch[8192];

int main(int argc, char *argv[])

{	char *OutPath, *InPath, *InBuffer, *ddPath, *s, *t;
   int InHandle, OutHandle, InLength, Offset, Length, ReadCount, i,
   	RecordType, k, ok;
   parse_rec *p0, *p1, *p2;
   int p0Count;
   extract_rec *e0, *e1;
   int e0Count;
   field_rec *f0, *f1;
   int f0Count;

	printf("\n\n******* B E G I N   C N P E X T R A C T V A L U E S *******\n\n");
	if (argc<4)
   {	printf("\n\nUsage is \"%s A B C\"\n\n", argv[0]);
   	printf("\n  A - Input (full path)");
      printf("\n  B - Record Length");
      printf("\n  C,D,E... - Fields for each record type (full path)");
//      printf("\n  D - Field headings list (optional)");
      printf("\nFor example:");
      printf("\n     \"%s /users/public/36060.1100 1100 ", argv[0]);
      printf("\n         /users/eric/mbpri.0628.extract.dd");
      printf("\n         /users/eric/mbsec.0628.extract.dd");
      printf("\n         /users/eric/mbdisb.0628.extract.dd");
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
	sprintf(scratch, "%s.tab", InPath);
   OutPath=strdup(scratch);
   InLength=atoi(argv[2]);
   e0Count=argc-3;
   e0=(extract_rec *)malloc(e0Count*sizeof(extract_rec));
   memset(e0, 0, e0Count*sizeof(extract_rec));
   for(e1=e0, i=3; i<argc; e1++, i++)
   {	e1->ddPath=strdup(argv[i]);
      e1->dd=ddLoadFromFile(e1->ddPath);
   }
   if(InLength<=0)
   {	printf("\nRecord Length (%d) must be > 0", InLength);
   	exit(1);
   }
   for(e1=e0; e1-e0<e0Count; e1++)
   {	f0=e1->dd->Field;
	   f0Count=e1->dd->FieldCount;
	   for(f1=f0; f1-f0<f0Count; f1++)
   	{	if(f1->Offset+f1->Length>InLength)
   		{	printf("\n%s:  Offset+Length must be <=InLength (%d+%d<=%d)", f1->FieldName, f1->Offset, f1->Length, InLength);
   			printf("\nddPath=\"%s\"", e1->dd->Path);
				exit(1);
	      }
   	}
   }
	InHandle=OpenFile(InPath, "", IN, NULL);
   OutHandle = OpenFile(OutPath, "", OUT, NULL);
   InBuffer=(char *)malloc(InLength);
   ReadCount=0;
   while(read(InHandle, InBuffer, InLength)>0)// && ReadCount<15)
   {	ReadCount++;
   	if(e0Count==1)
      	e1=e0;
      else
   	{	sprintf(scratch, "%.1s", InBuffer+InLength-1);
	      RecordType=atoi(scratch);
   	   if(RecordType>=e0Count)
      	{	printf("\nReadCount=%d, invalid record type %d", ReadCount, RecordType);
      		printf("\nValid record types are 0 through %d", e0Count-1);
	         exit(1);
   	   }
      	e1=e0+RecordType;
      }
      f0=e1->dd->Field;
      f0Count=e1->dd->FieldCount;
// this is the logic I was using for LPS extract for Bayview
//      sprintf(scratch, "%c|", *(InBuffer+InLength-2));
//		sprintf(scratch, "%.5s|", InBuffer+InLength-6);
   	for(f1=f0, s=scratch; f1-f0<f0Count; f1++, s+=strlen(s))
   	{//	if(RecordType==4)
       //		printf("\nf1[%d]=\"%s\", DataType=%d", f1-f0, f1->FieldName, f1->DataType);
			if(f1->DataType==8 && !FieldIsPacked(InBuffer+f1->Offset, f1->Length))
         	f1->DataType=0;
         else
			if(f1->DataType==8 && FieldIsPacked(InBuffer+f1->Offset, f1->Length))
         	f1->DataType=2;
      	if(f1->FieldFormat!=NULL && strcmp(f1->FieldFormat, "LZF")!=0)// && strcmp(f1->FieldFormat, "pymmdd")==0)
			{	CanonicalDate(s, InBuffer+f1->Offset, f1->Length, f1->FieldFormat);
         	strcat(s, "|");
         }
         else
      	if(f1->DataType==0)
	   	{	sprintf(s, "%.*s", f1->Length, InBuffer+f1->Offset);
         	while(*s==' ' || *s=='\r' || *s=='\n')
            	memmove(s, s+1, strlen(s));
         	for(t=s+strlen(s)-1; t>=s && (*t==' ' || *t=='\r' || *t=='\n'); t--)
            	;
            t++;
            strcpy(t, "|");
         }
      	else
         if(f1->DataType<3)
			{	if(f1->DataType==1 || f1->DataType==2 && FieldIsPacked(InBuffer+f1->Offset, f1->Length))
	         	ConvertNumberToString(s, InBuffer+f1->Offset, f1->Length, f1->DataType, f1->Decimals);
         	if(f1->DataType==2 && strcmp(f1->FieldFormat, "LZF")==0)
            {	k=f1->Length*2-1;
            	while(strlen(s)<k)
               {	memmove(s+1, s, strlen(s)+1);
               	*s='0';
               }
            }
         	strcat(s, "|");
         }
      }
      for(s=scratch, ok=1; s-scratch<strlen(scratch); s++)
      	if(*s=='\n' || *s==0)
         {	printf("\nLINEFEED or NULL character in record %d", ReadCount);
         	ok=0;
         }
      // replace double quote character (x22) with space (x20)
      for(s=scratch; *s!=0; s++)
      	if(*s=='\"')
         	*s=' ';
     	strcpy(s-1, "\r\n");
      write(OutHandle, scratch, strlen(scratch));
      if(!ok)
      	exit(1);
   }
   close(InHandle);
   close(OutHandle);
   printf("\n\n");
   printf("\nOutput file:  \"%s\"", OutPath);
   printf("\n\n");
   return(0);
}

