/* ******************************************

   CnpTabToFixed.c (Based on CnpDelimit.c)
   Input file is tab delimited text file
   Header records at beginning of input file become DD field names
   Output file is fixed length
   Each output field is sized so that no data is lost in conversion
      to fixed length records

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

char scratch[32768];

int OutputRecordLength = 0;
char *OutputRecord;

short CheckNumeric(char *s0);
void ClipString(char *s0, char *Begin, char *End);
void TrimLeft(char *s0);
void TrimRight(char *s0);

void main(int argc, char *argv[])

{  int TempHandle, OutHandle, ddHandle, ParseCount, i, r, PctDone, FirstUpdate,
       DigitCount;
   FILE *InHandle, *TextFieldHandle;
   long ReadCount;
   char *s, *t, **s1;
   dd_rec *tf0, *tf1;
   int NameLength, SaveNameLength;
   parse_rec *p0, *p1, *pParms;
   int pParmsCount;
   struct stat statbuf;
   fieldprops_rec *r0, *r1;
   int r0Count, r0Limit;
   char *InPath, *OutPath, *ddOutPath, *TextFieldPath, FieldDelimiter;
   int HeaderCount, Clip, Trim, PreserveSpaces;
   char **t0, **t1, **tFound;
   int t0Count, t0Limit;

   printf("\n\n******* B E G I N   C N P T A B T O F I X E D *******\n\n");
   if (argc<3)
   {  printf("\n\nUsage is \"cnptabtofixed.out A B\"\n\n");
      printf("\n  A = Full path to tab-delimited input file");
      printf("\n  B = # of heading rows in tab-delimited input");
      printf("\n  C = Field delimiter (if not TAB character (hex 09)");
      printf("\n  D = \"CLIP\" ... clip boundary <> from individual fields");
      printf("\n  E = \"TRIM\" ... trim leading and trailing spaces from fields");
      printf("\n  F = Full path to TextFieldNameList (overrides numeric logic)");
      printf("\n  F or G = \"PRESERVE_SPACES\" ... numeric field with spaces should");
      printf("\n                                   not be converted to ZERO");
      printf("\n  For example:  \"cnptabtofixed.out /users/eric/test4.txt 1\"");
      printf("\n\n");
      exit(1);
   }

   // get input parms
   InPath=strdup(argv[1]);
   HeaderCount=atoi(argv[2]);
   if(argc>3
   && strcmp(argv[3], "TAB")!=0)
      FieldDelimiter=*argv[3];
   else
      FieldDelimiter='\t';
   if(argc>4
   && strcmp(argv[4], "CLIP")==0)
   	Clip=1;
   else
   	Clip=0;
   if(argc>5
   && strcmp(argv[5], "TRIM")==0)
   	Trim=1;
   else
   	Trim=0;
   PreserveSpaces=0;
   if(argc>6)
   {	if(strcmp(argv[6], "PRESERVE_SPACES")==0)
   		PreserveSpaces=1;
   	else
	   	TextFieldPath=strdup(argv[6]);
   }
   else
   	TextFieldPath=NULL;
   if(argc>7)
   {	if(strcmp(argv[6], "PRESERVE_SPACES")==0)
   		PreserveSpaces=1;
   }
   t0=NULL;
   t0Count=t0Limit=0;
   if(TextFieldPath!=NULL)
   {	TextFieldHandle=fopen(TextFieldPath, "rt");
   	if(TextFieldHandle==NULL)
      {	printf("\n\"%s\" open failed", TextFieldPath);
      	exit(1);
      }
      fgets(scratch, sizeof(scratch), TextFieldHandle);
   	while(!feof(TextFieldHandle))
      {	for(s=scratch+strlen(scratch)-1; s>=scratch && (*s=='\r' || *s=='\n'); s--)
      		*s=0;
      	if(t0Count==t0Limit)
         {	t0Limit+=100;
         	t0=(char **)realloc(t0, t0Limit*sizeof(char *));
            memset(t0+t0Count, 0, 100*sizeof(char *));
         }
         t1=t0+t0Count;
         *t1=strdup(scratch);
         t0Count++;
	      fgets(scratch, sizeof(scratch), TextFieldHandle);
      }
      fclose(TextFieldHandle);
   }

   sprintf(scratch, "%s.fixed", InPath);
   OutPath=strdup(scratch);

   InHandle=fopen(InPath, "rt");
   if(InHandle==NULL)
   {  printf("\n%s open error", InPath);
      exit(1);
   }
   OutHandle=OpenFile(OutPath, "", OUT, NULL);

   ReadCount = 0;
   PctDone = 0;
   FirstUpdate=1;
   r0Count=0;
   // read header records
   for(i=0, SaveNameLength=NameLength=0; i<HeaderCount; i++)
   {  fgets(scratch, sizeof(scratch)-1, InHandle);
      p0=ParseExpression(scratch, &ParseCount, FieldDelimiter, 1, 0, 1);
      for(p1=p0; p1-p0<ParseCount; p1++)
         if(SaveNameLength+strlen(p1->String)>NameLength)
            NameLength=SaveNameLength+strlen(p1->String);
      SaveNameLength=NameLength;
      if(ParseCount>r0Limit)
      	r0Limit=ParseCount;
      RemoveParse(p0, ParseCount);
   }
   if(NameLength<20)
      NameLength=20;
   r0Count=r0Limit;
   r0=(fieldprops_rec *)malloc(r0Limit*sizeof(fieldprops_rec));
   memset(r0, 0, r0Limit*sizeof(fieldprops_rec));
   for(r1=r0; r1-r0<r0Count; r1++)
   {  r1->fName=(char *)malloc(NameLength*sizeof(char));
      memset(r1->fName, 0, NameLength*sizeof(char));
   }
   fseek(InHandle, 0, SEEK_SET);
   for(i=0; i<HeaderCount; i++)
   {  fgets(scratch, sizeof(scratch)-1, InHandle);
      p0=ParseExpression(scratch, &ParseCount, FieldDelimiter, 1, 0, 1);
      printf("\nParseCount=%d, r0Count=%d", ParseCount, r0Count);
      for(p1=p0, r1=r0; p1-p0<ParseCount; p1++, r1++)
         strcat(r1->fName, p1->String);
      RemoveParse(p0, ParseCount);
   }
   for(r1=r0; r1-r0<r0Count; r1++)
	{	if(strlen(r1->fName)==0)
   	   sprintf(r1->fName, "Column %d", r1-r0+1);
      for(t1=t0, tFound=NULL; t1-t0<t0Count && tFound==NULL; t1++)
      	if(strcmp(r1->fName, *t1)==0)
         	tFound=t1;
      if(tFound!=NULL)
      	r1->IsNumber=0;
      else
      	r1->IsNumber=1;
   }
   fgets(scratch, sizeof(scratch)-1, InHandle);
   while(!feof(InHandle))
   {  p0=ParseExpression(scratch, &ParseCount, FieldDelimiter, 1, 0, 1);
      if(r0Limit<ParseCount)
      {  r0Limit=ParseCount;
         r0 = (fieldprops_rec *)realloc(r0, r0Limit*sizeof(fieldprops_rec));
         memset(r0+r0Count, 0, (r0Limit-r0Count)*sizeof(fieldprops_rec));
         for(r1=r0+r0Count; r1-r0<r0Limit; r1++)
         {	r1->IsNumber=1;
  				r1->fName=(char *)malloc(NameLength*sizeof(char));
  			   sprintf(r1->fName, "Column %d", r1-r0+1);
		   }
         r0Count=ParseCount;
      }
      ReadCount++;
      if(ReadCount%1000==0)
         printf("\nReadCount=%d", ReadCount);
//
// ParseCount<FieldCount should be a warning
//
      if(ParseCount>r0Count)
      {  printf("\nParseCount %d unequal to FieldCount %d, ReadCount=%d", ParseCount, r0Count, ReadCount);
         for(p1=p0; p1-p0<ParseCount; p1++)
            printf("\nString[%d]=%s", p1-p0, p1->String);
         exit(1);
      }
      for(p1=p0, r1=r0; p1-p0<ParseCount; p1++, r1++)
      {  for(s=p1->String+strlen(p1->String)-1; s>=p1->String && (*s=='\n' || *s=='\r'); s--)
            *s=0;
      	if(Clip)
         	ClipString(p1->String, "<", ">");
         if(Trim)
         {	TrimLeft(p1->String);
         	TrimRight(p1->String);
         }
         if(r1->IsNumber)
         {  if(r1->IsNumber=CheckNumeric(p1->String))
            {  strcpy(scratch, p1->String);
               if(!r1->Minus && strstr(scratch, "-")!=NULL)
                  r1->Minus=1;
               s=strstr(scratch, ".");
               if(s!=NULL)
               {  for(t=s+1; *t>='0' && *t<='9'; t++)
                     ;
                  if(t-s-1>r1->fDecimals)
                     r1->fDecimals=t-s-1;
               }
               for(s=scratch, DigitCount=0; *s!='.' && *s!=0; s++)
                  if(*s>='0' && *s<='9')
                     DigitCount++;
               if(DigitCount>r1->fDigits)
                  r1->fDigits=DigitCount;
            }
         }
         if(r1->fLength<strlen(p1->String))
            r1->fLength=strlen(p1->String);
      }
      RemoveParse(p0, ParseCount);
      fgets(scratch, sizeof(scratch)-1, InHandle);
   }
   for(OutputRecordLength=0, r1=r0; r1-r0<r0Count; r1++)
   {  if(r1->IsNumber)
      {  r1->fLength=r1->fDigits+r1->fDecimals;
      	if(r1->fLength==0)
         	r1->IsNumber=0;
      }
      OutputRecordLength+=r1->fLength;
   }
   OutputRecord=(char *)malloc(OutputRecordLength);
   fclose(InHandle);
   InHandle=fopen(InPath, "rt");
   if(InHandle==NULL)
   {  printf("\n%s open error", InPath);
      exit(1);
   }
   // read past header records
   for(i=0; i<HeaderCount; i++)
      fgets(scratch, sizeof(scratch)-1, InHandle);

   sprintf(scratch, "%s.dd", OutPath);
   ddOutPath=strdup(scratch);
   ddHandle = OpenFile(ddOutPath, "", OUT, NULL);
   printf("\n");
   for(r1=r0, i=0; r1-r0<r0Count; r1++)
   {  for(s=r1->fName+strlen(r1->fName)-1; s>=r1->fName && (*s=='\n' || *s=='\r'); s--)
         *s=0;
   	TrimLeft(r1->fName);
      TrimRight(r1->fName);
      strcpy(scratch, r1->fName);
      for(s=scratch; *s!=0; s++)
         *s=tolower(*s);
      if(strstr(scratch, "zip")!=NULL)
	      r1->IsNumber=0;
      if(r1->IsNumber)
         sprintf(scratch, "%s, %d, %d, Number, %d\r\n", r1->fName, i, r1->fLength, r1->fDecimals);
      else
         sprintf(scratch, "%s, %d, %d, Text, 0\r\n", r1->fName, i, r1->fLength);
      write(ddHandle, scratch, strlen(scratch));
      r1->fOffset=i;
      i+=r1->fLength;
   }
   close(ddHandle);

   for(r1=r0; r1-r0<r0Count; r1++)
   {  r1->fBegin=OutputRecord+r1->fOffset;
      r1->fEnd=OutputRecord+r1->fOffset+r1->fLength;
      r1->dPoint=r1->fEnd-r1->fDecimals;
   }

   ReadCount=0;
   fgets(scratch, sizeof(scratch)-1, InHandle);
   while(!feof(InHandle))
   {  p0=ParseExpression(scratch, &ParseCount, FieldDelimiter, 1, 0, 1);
      memset(OutputRecord, ' ', OutputRecordLength);
      for(p1=p0, r1=r0, i=0; p1-p0<ParseCount; p1++, r1++)
      {  for(s=p1->String+strlen(p1->String)-1; s>=p1->String && (*s=='\n' || *s=='\r'); s--)
            *s=0;
      	if(Clip)
         	ClipString(p1->String, "<", ">");
         if(Trim)
         {	TrimLeft(p1->String);
         	TrimRight(p1->String);
         }
         if(r1->IsNumber)
         {  if(!IsBlank(p1->String))
               FormatNumber(r1, p1->String);
         	else
            if(PreserveSpaces)
            	memset(OutputRecord+i, ' ', r1->fLength);
            else
            	memset(OutputRecord+i, '0', r1->fLength);
         }
         else
         if(r1->fLength>0)
	         memcpy(OutputRecord+i, p1->String, strlen(p1->String));
      	i+=r1->fLength;
      }
      write(OutHandle, OutputRecord, OutputRecordLength);
      RemoveParse(p0, ParseCount);
      ReadCount++;
      if(ReadCount%1000==0)
         printf("\nReadCount=%d", ReadCount);
//      printf("\nReadCount=%d", ReadCount);
      fgets(scratch, sizeof(scratch)-1, InHandle);
   }
   close(OutHandle);
   fclose(InHandle);
   free(OutputRecord);
   return;
}

void ClipString(char *s0, char *Begin, char *End)

{
  	if(Begin!=NULL
   && memcmp(s0, Begin, strlen(Begin))==0)
     	memmove(s0, s0+strlen(Begin), strlen(s0)-strlen(Begin)+1);
   if(End!=NULL
	&& memcmp(s0+strlen(s0)-strlen(End), End, strlen(End))==0)
     	*(s0+strlen(s0)-strlen(End))=0;
   return;
}

void TrimLeft(char *s0)

{	while(*s0==' ')
		memmove(s0, s0+1, strlen(s0));
}

void TrimRight(char *s0)

{	char *s1;

	for(s1=s0+strlen(s0)-1; s1>=s0 && *s1==' '; s1--)
   	*s1=0;
}

