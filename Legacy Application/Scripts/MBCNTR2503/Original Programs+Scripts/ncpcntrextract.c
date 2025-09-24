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

#define PRIMARY 1
#define SECONDARY 2
#define HEADER 3

typedef struct s_extract {
	char *ddPath;
	dd_rec *dd;
} extract_rec;

typedef struct s_pc {
	char *ddName;
	char *RecordId;
   int RecordIdOffset;
   char *ContainerId;
//   char ddNumber;
	int Type;				// 1-PRIMARY, 2-SECONDARY, 3-HEADER
   extract_rec *e0;
   int UsePrevious;		// use "ddName.prev" layout rather than "ddName" layout
} pc_rec;

char scratch[8192];

int main(int argc, char *argv[])

{	char *OutPath, *InPath, *InBuffer, *ddPath, *s, *t, *pcPath, *Project,
      *Number, *OutBuffer, *ProjectBase;
	FILE *pcHandle;
   int InHandle, OutHandle, InLength, Offset, Length, ReadCount, i,
   	RecordType, k, n, ret;
   parse_rec *p0, *p1, *p2;
   int p0Count;
//   extract_rec *e0, *e1;
//   int e0Count;
   field_rec *f0, *f1;
   int f0Count;
   pc_rec *r0, *r1, *rFound;
   int r0Count, r0Limit;
   int ddStartList;
   char **a0, **a1, **aFound;		// list of ".prev" dds to use
   int a0Count;

	printf("\n\n******* B E G I N   N C P C N T R E X T R A C T *******\n\n");
	if (argc<5)
   {	printf("\n\nUsage is \"%s A B C D\"\n\n", argv[0]);
   	printf("\n  A - Input (full path)");
      printf("\n  B - Record Length");
      printf("\n  C - Project Name");
      printf("\n  D - Project Base");
      printf("\n  E = Previous dd versions (optional)");
      printf("\nFor example:");
      printf("\n     \"%s /users/public/36060.1100 1100 inv0140 /users/devel/container mbp.dd\"", argv[0]);
      for(i=1; i<argc; i++)
      	printf("\nargv[%d]=\"%s\"", i, argv[i]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
	sprintf(scratch, "%s.tab", InPath);
   OutPath=strdup(scratch);
   InLength=atoi(argv[2]);
   Project=strdup(argv[3]);
   ProjectBase=strdup(argv[4]);
   a0=NULL;
   a0Count=0;
   if(argc>5)
   {	p0=ParseExpression(argv[5], &p0Count, ',', 1, 0, 1);
   	a0Count=p0Count;
      a0=(char **)malloc(a0Count*sizeof(char *));
      for(p1=p0, a1=a0; p1-p0<p0Count; p1++, a1++)
      	*a1=strdup(p1->String);
      RemoveParse(p0, p0Count);
   }
   sprintf(scratch, "%s/%s/ddcontrol.txt", ProjectBase, Project);
   pcPath=strdup(scratch);
   pcHandle=fopen(pcPath, "rt");
   if(pcHandle==NULL)
   {	printf("\n\"%s\" open failure", pcPath);
   	printf("\n");
      exit(1);
   }
   ddStartList=0;
   r0=NULL;
   r0Count=r0Limit=0;
   fgets(scratch, sizeof(scratch), pcHandle);
   while(!feof(pcHandle))
   {	if(memcmp(scratch, "DDLIST", 6)==0)
   		ddStartList=1;
   	else
   	if(ddStartList)
      {	p0=ParseExpression(scratch, &p0Count, ',', 1, 0, 1);
      	if(r0Count==r0Limit)
         {	r0Limit+=10;
         	r0=(pc_rec *)realloc(r0, r0Limit*sizeof(pc_rec));
            memset(r0+r0Count, 0, 10*sizeof(pc_rec));
         }
         r1=r0+r0Count;
         r0Count++;
         for(p1=p0; p1-p0<p0Count; p1++)
           	switch(p1-p0) {
            case 0:	// ddName
               for(a1=a0, aFound=NULL; a1-a0<a0Count && aFound==NULL; a1++)
                 	if(strcmp(p1->String, *a1)==0)
                    	aFound=a1;
               if(aFound!=NULL)
               {	r1->UsePrevious=1;
                  sprintf(scratch, "%s.prev", p1->String);
                  r1->ddName=strdup(scratch);
               }
               else
	           		r1->ddName=strdup(p1->String);
               break;
            case 1:	// record id
            	r1->RecordId=strdup(p1->String);
               break;
            case 2:	// record id offset
            	r1->RecordIdOffset=atoi(p1->String);
               break;
            case 3:	// container id
            	r1->ContainerId=strdup(p1->String);
               break;
            case 4:	// ddNumber
//            	r1->ddNumber=*p1->String;
               break;
            case 5:	// type
            	if(strcmp(p1->String, "PRIMARY")==0)
               	r1->Type=PRIMARY;
               else
               if(strcmp(p1->String, "SECONDARY")==0)
               	r1->Type=SECONDARY;
               else
               if(strcmp(p1->String, "HEADER")==0)
               	r1->Type=HEADER;
               else
               if(strcmp(p1->String, "NCPJAX")==0)
               	r1->Type=-1;
               else
               if(strcmp(p1->String, "CLIENT")==0)
               	r1->Type=-1;
               else
               {	printf("\nInvalid record type \"%s\"", p1->String);
               	exit(1);
               }
               break;
            default:
            	break;
            }
      	RemoveParse(p0, p0Count);
      }
	   fgets(scratch, sizeof(scratch), pcHandle);
   }
   fclose(pcHandle);
//   e0Count=r0Count;
//   e0=(extract_rec *)malloc(e0Count*sizeof(extract_rec));
//   memset(e0, 0, e0Count*sizeof(extract_rec));
   for(r1=r0; r1-r0<r0Count; r1++)
   {	sprintf(scratch, "%s/%s/%s", ProjectBase, Project, r1->ddName);
	   r1->e0=(extract_rec *)malloc(sizeof(extract_rec));
   	memset(r1->e0, 0, sizeof(extract_rec));
   	r1->e0->ddPath=strdup(scratch);
      r1->e0->dd=ddLoadFromFile(r1->e0->ddPath);
   }
   if(InLength<=0)
   {	printf("\nRecord Length (%d) must be > 0", InLength);
   	exit(1);
   }
   for(r1=r0; r1-r0<r0Count; r1++)
   {	f0=r1->e0->dd->Field;
	   f0Count=r1->e0->dd->FieldCount;
	   for(f1=f0; f1-f0<f0Count; f1++)
   	{	if(f1->Offset+f1->Length>InLength)
   		{	printf("\n%s:  Offset+Length must be <=InLength (%d+%d<=%d)", f1->FieldName, f1->Offset, f1->Length, InLength);
   			printf("\nddPath=\"%s\"", r1->e0->dd->Path);
				exit(1);
	      }
   	}
   }
   InHandle=open(InPath, O_RDONLY | O_LARGEFILE);
  	if(InHandle==-1)
   {	printf("\n\"%s\" open error", InPath);
     	exit(1);
   }
	OutHandle = open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (OutHandle == -1)
	{	printf("\r\nFile open error %s", OutPath);
   	exit(1);
   }
	else
		printf("\r\nFile open successful %s", OutPath);
   InBuffer=(char *)malloc(InLength);
   ReadCount=0;
   Number=scratch+7900;
   OutBuffer=(char *)malloc(sizeof(scratch));
   while(read(InHandle, InBuffer, InLength)>0)// && ReadCount<5)
   {	ReadCount++;
   	sprintf(scratch, "%.5s", InBuffer+InLength-6);
      for(r1=r0, rFound=NULL; r1-r0<r0Count && rFound==NULL; r1++)
      	if(strcmp(r1->ContainerId, scratch)==0)
         	rFound=r1;
      if(rFound==NULL)
      {	printf("\nReadCount=%d, invalid id \"%s\"", ReadCount, scratch);
         exit(1);
      }
      f0=rFound->e0->dd->Field;
      f0Count=rFound->e0->dd->FieldCount;
   	for(f1=f0, s=OutBuffer; f1-f0<f0Count; f1++, s+=strlen(s))
   	{	if(f1->DataType==8 && !FieldIsPacked(InBuffer+f1->Offset, f1->Length))
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
         	for(t=s; *t!=0; t++)
            	if(*t=='\r' || *t=='\n' || *t=='|')
               	*t=' ';
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
	         {	ret=ConvertNumberToString(Number, InBuffer+f1->Offset, f1->Length, f1->DataType, f1->Decimals);
               if(ret==0)
                  strcpy(s, Number);
            }
         	if(f1->DataType==2 && strcmp(f1->FieldFormat, "LZF")==0)
            {	k=f1->Length*2-1;
            	while(strlen(s)<k)
               {	memmove(s+1, s, strlen(s)+1);
               	*s='0';
               }
            }
         	strcat(s, "|");
         }
         else
         if(f1->DataType==6)		// 8 byte integer
         {	ConvertNumberToString(s, InBuffer+f1->Offset, f1->Length, f1->DataType, f1->Decimals);
         	strcat(s, "|");
         }
         else
         //
         // using a placeholder for other data types
         //
         	strcat(s, "|");
/*         if(f1->DataType==5)
         {	memcpy(&n, InBuffer+f1->Offset, sizeof(int));
         	sprintf(s, "%d|", n);
         	printf("\n%d|%s", n, ctime((time_t *)&n));
			}*/
      }
      // replace double quote character (x22) with space (x20)
      for(s=OutBuffer; *s!=0; s++)
      	if(*s=='\"')
         	*s=' ';
     	strcpy(s-1, "\r\n");
      write(OutHandle, OutBuffer, strlen(OutBuffer));
   }
   close(InHandle);
   close(OutHandle);
   printf("\n\n");
   printf("\nOutput file:  \"%s\"", OutPath);
   printf("\n\n");
   return(0);
}

