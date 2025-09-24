/* ******************************************

	ncpcntr6.c

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include "/users/temp/cnp01.h"

typedef struct s_dfrom {
	char *fValue;			// fixed value
   field_rec *f0;			// from field_rec
   int Type;				// 0-field, 1-fixed value
} dfrom_rec;

typedef struct s_dst {
   field_rec *g0;			// "To" field_rec
	char *From;
   dfrom_rec *m0;
   int m0Count, m0Limit;
} dto_rec;

//char *ddIomapTestPath="/users/eric/mblps.dd.iomap.save";
char *ddOutPath="/users2/letters/dd/aztest/ncpcontainer.cbl.dd.new";
char scratch[8192];

char *GetNextPart(char *s0, char *Part);
int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, OutLength, rCount, i;
	FILE *IomapHandle;
	char *InPath, *OutPath, *IomapPath, *ddInPath,
   	*InBuffer, *OutBuffer, *Project, *s, *t,
      Delimiter, *FromField, *FromPart, *sNext;
   int rtOffset, rtLength;
   parse_rec *p0, *p1;
   int p0Count;
   dd_rec *ddIn, *ddOut;
   field_rec *f0, *f1;
   int f0Count;
   dto_rec *d0, *d1, *dFound;
   int d0Count, d0Limit;
   dfrom_rec *m1;
   int Length;

	printf("\n\n******* B E G I N   N C P C N T R 6 *******\n\n");
	if (argc<3)
   {	printf("Usage is \"%s A B\"", argv[0]);
      printf("\n  A = Input (full path)");
      printf("\n  B = Project");
      printf("\n  For example: \"%s /users/public/16860241.cntr.wrk.tab.fixed mblps", argv[0]);
      for(i=1; i<argc; i++)
      	printf("\nargv[%d]=\"%s\"", i, argv[i]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   Project=strdup(argv[2]);
   OutLength=3422;
   sprintf(scratch, "%s.dd", InPath);
   ddInPath=strdup(scratch);
   ddIn=ddLoadFromFile(ddInPath);
   ddOut=ddLoadFromFile(ddOutPath);
   s=strstr(InPath, ".tab.fixed");
   if(s==NULL)
   {	printf("\n\"%s\":  Unexpected input file name", InPath);
   	exit(1);
   }
   sprintf(scratch, "%.*s.dd.extract.iomap", s-InPath, InPath);
   IomapPath=strdup(scratch);
//	IomapPath=ddIomapTestPath;
   IomapHandle=fopen(IomapPath, "rt");
   if(IomapHandle==NULL)
   {	printf("\n\"%s\" open failure", IomapPath);
   	printf("\n");
      exit(1);
   }
   d0Count=d0Limit=0;
   d0=NULL;
   FromField=scratch+5000;
   FromPart=scratch+5500;
   while(!feof(IomapHandle))
   {	fgets(scratch, sizeof(scratch), IomapHandle);
   	for(s=scratch+strlen(scratch)-1; s>=scratch && *s!=','; s--)
   		if(*s=='\r' || *s=='\n')
         	*s=0;
      if(s>scratch)
      {	*s=0;
      	s++;
      	if(d0Count==d0Limit)
      	{	d0Limit+=100;
         	d0=(dto_rec *)realloc(d0, d0Limit*sizeof(dto_rec));
            memset(d0+d0Count, 0, 100*sizeof(dto_rec));
         }
         d1=d0+d0Count;
         d0Count++;
         while(*s==' ')
         	s++;
			d1->g0=GetFieldRecByName(ddOut, s);
         if(d1->g0==NULL)
         {	printf("\nField \"%s\" not found", s);
         	exit(1);
         }
         d1->From=strdup(scratch);
      }
   }
   fclose(IomapHandle);
/*   for(d1=d0; d1-d0<d0Count; d1++)
   	printf("\nd1[%d]->From=\"%s\", To=\"%s\"", d1-d0, d1->From, d1->g0->FieldName);
   exit(1);*/
   for(d1=d0; d1-d0<d0Count; d1++)
   {  sNext=d1->From;
      while(*sNext!=0)
      {	if(d1->m0Count==d1->m0Limit)
	     	{	d1->m0Limit+=5;
   	     	d1->m0=(dfrom_rec *)realloc(d1->m0, d1->m0Limit*sizeof(dfrom_rec));
      	   memset(d1->m0+d1->m0Count, 0, 5*sizeof(dfrom_rec));
	      }
	      m1=d1->m0+d1->m0Count;
   	   d1->m0Count++;
      	sNext=GetNextPart(sNext, FromPart);
      	if(*FromPart=='"')
         {	m1->fValue=strdup(FromPart+1);
         	m1->Type=1;
         }
         else
	      {	m1->f0=GetFieldRecByName(ddIn, FromPart);
         	if(m1->f0==NULL)
            {	printf("\n%s:  \"%s\" not found", ddInPath, FromPart);
            	exit(1);
            }
   	   	m1->Type=0;
            if(m1->f0!=NULL)
	         	printf("\nd1[%d]:  FromPart=\"%s\", \"%s\"", d1-d0, FromPart, m1->f0->FieldName);
            else
	         	printf("\nd1[%d]:  FromPart=\"%s\"", d1-d0, FromPart);
      	}
      }
   }
   for(d1=d0; d1-d0<d0Count; d1++)
   {	// printf("\nd1[%d]->m0Count=%d", d1-d0, d1->m0Count);
   	for(m1=d1->m0; m1-d1->m0<d1->m0Count; m1++)
   	{	if(d1->m0->Type==0)
	      	printf("\nd1[%d]->m1[%d]->f0=%s", d1-d0, m1-d1->m0, m1->f0->FieldName);
   	  	else
      	if(d1->m0->Type==1)
	      	printf("\nd1[%d]->m1[%d]->fValue=\"%s\"", d1-d0, m1-d1->m0, m1->fValue);
      }
   }
   sprintf(scratch, "%s.3422", InPath);
   OutPath=strdup(scratch);
	OutHandle = open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (OutHandle == -1)
	{	printf("\r\nFile open error %s", OutPath);
   	exit(1);
   }
	else
		printf("\r\nFile open \"%s\"", OutPath);
	f0=ddIn->Field;
   f0Count=ddIn->FieldCount;
   f1=f0+f0Count-1;
   InLength=f1->Offset+f1->Length;
	InBuffer=(char *)malloc(InLength);
	memset(InBuffer, 0, InLength);
   OutBuffer=(char *)malloc(OutLength);
   InHandle=open(InPath, O_RDONLY | O_LARGEFILE);
  	if(InHandle==-1)
   {	printf("\n\"%s\" open error", InPath);
     	exit(1);
   }
	while(read(InHandle, InBuffer, InLength)>0)
	{	rCount++;
      memset(OutBuffer, ' ', OutLength);
      for(d1=d0; d1-d0<d0Count; d1++)
      {	for(m1=d1->m0, s=scratch; m1-d1->m0<d1->m0Count; m1++, s+=strlen(s))
      	{	if(m1->Type==1)
         		strcpy(s, m1->fValue);
         	else
            if(m1->Type==0)
            {	sprintf(s, "%.*s", m1->f0->Length, InBuffer+m1->f0->Offset);
            	for(t=s+strlen(s)-1; t>=s && *t==' '; t--)
               	*t=0;
            }
         }
         // remove leading spaces
         for(s=scratch; *s==' '; s++)
         	;
         Length=strlen(s);
         if(Length>d1->g0->Length)
         	Length=d1->g0->Length;
         memcpy(OutBuffer+d1->g0->Offset, s, Length);
      }
		write(OutHandle, OutBuffer, OutLength);
		if(rCount%1000==0)
			printf("\nrCount=%ld", rCount);
	}
  	close(InHandle);
	close(OutHandle);
	free(InBuffer);
   free(OutBuffer);
	printf("\nrCount=%ld", rCount);
	return(0);
}

char *GetNextPart(char *s0, char *Part)

{	char *s1, *s;

	if(*s0=='+')
   	s0++;
   while(*s0==' ')
   	s0++;
   if(*s0=='"')
   {	for(s1=s0+1; *s1!='"' && *s1!=0; s1++)
   		;
   	if(*s1==0)
      {	printf("\nIOMAP syntax error - no end quote: %s", s0);
        	exit(1);
      }
      sprintf(Part, "%.*s", s1-s0, s0);
      s1++;
      while(*s1==' ')
      	s1++;
   }
   else
   {	for(s1=s0+1; *s1!='+' && *s1!=0; s1++)
   		;
      sprintf(Part, "%.*s", s1-s0, s0);
      for(s=Part+strlen(Part)-1; s>=Part && *s==' '; s--)
      	*s=0;
   }
	return(s1);
}

