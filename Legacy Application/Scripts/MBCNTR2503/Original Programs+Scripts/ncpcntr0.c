/* ******************************************

	ncpcntr0.c

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include "/users/temp/cnp01.h"

// first 200 of last 300 bytes is reserved for client-specific info
#define CLIENT_OFFSET 300
// last 100 bytes of output record are reserved for NCP'
#define NCP_OFFSET 100
#define F1_LENGTH 9			// rCount - input record counter
#define F2_LENGTH 6			// tCount - starts at 1 for each new account
#define F3_LENGTH 8			// pCount - account counter
#define RECORDTYPE_OFFSET 6	// appears 6 bytes from end of record
#define DDNUMBER_OFFSET 1		// appears in last byte of record

#define PRIMARY 1
#define SECONDARY 2
#define HEADER 3
#define NCPJAX 4

typedef struct s_pc {
	char *ddName;
	char *RecordId;
   int RecordIdOffset;
   char *ContainerId;
   char ddNumber;
	int Type;				// 1-PRIMARY, 2-SECONDARY, 3-HEADER, 4-NCPJAX
   int RecordIdLength;
   dd_rec *dd;
   int Count;
   int UsePrevious;		// use "ddName.prev" layout rather than "ddName" layout
} pc_rec;

typedef struct s_dfrom {
	char *From;				// from field name
   int j0;					// from column number
   int Type;				// -1=default, 0=field
   							// 1-fixed value
} dfrom_rec;

typedef struct s_dst {
	// perhaps change *s0 into *To and *From???
   char *To;
   field_rec *f0;			// "To" field_rec
   dfrom_rec *m0;
   int m0Count, m0Limit;
} dst_rec;

char *ddGMCStandardPath="/users/eric/gmcstandard.dd";
char scratch[8192];

int main(int argc, char *argv[])

{	int OutHandle, Out2Handle, InLength, OutLength, WriteLength, ClientNumber,
		rCount, i, e2aControl, pCount, tCount, IsEbcdic, IsFixed, r, fHandle;
	FILE *tHandle, *pcHandle, *ddHandle;
	char *InPath, *OutPath, *InBuffer, *OutBuffer, *Out2Buffer, *pBuffer,
   	*pcPath, *Project, *s, *t, Delimiter, *Out2Path, *FromField,
      *ProjectBase, *rtPath;
   int rtOffset, rtLength, ddStartList, IsDelimited, Use3422, NeedCRLF,
   	IsGMCStandard, IsMultiRecordDelimited, UseNcpjax;
   pc_rec *r0, *r1, *rFound;
   int r0Count, r0Limit;
   parse_rec *p0, *p1, *q0, *q1, *qFound;
   int p0Count, q0Count;
   dd_rec *ddHeader, *ddPrimary, *ddNcpjax, *ddGMCStandard;
   field_rec *f0, *f1, *f1From;
   int f0Count;
//   int *j0, *j1;
//   int j0Count;
//   // iomap data goes here
//   char **s0, **s1, **sFound;
//   int s0Count, s0Limit;
   dst_rec *d0, *d1, *dFound;
   int d0Count, d0Limit;
   dfrom_rec *m1;
   int Done, Length, tLength;
   char **a0, **a1, **aFound;		// list of ".prev" dds to use
   int a0Count;


	printf("\n\n******* B E G I N   N C P C N T R 0 *******\n\n");
	if (argc<6)
   {	printf("Usage is \"%s A B C D E\"", argv[0]);
      printf("\n  A = Client# (4 digit)");
      printf("\n  B = Input (full path)");
      printf("\n  C = Output record length (work2length)");
      printf("\n  D = Project Name");
      printf("\n  E = Project Base");
      printf("\n  F = Previous dd versions (optional)");
      printf("\n  For example: \"%s 0140 /users/public/36061.dat 1700 inv0140 /users/devel/container mbp.dd", argv[0]);
      for(i=1; i<argc; i++)
      	printf("\nargv[%d]=\"%s\"", i, argv[i]);
      printf("\n\n");
   	exit(1);
   }
   ClientNumber=atoi(argv[1]);
   InPath=strdup(argv[2]);
   OutLength=atoi(argv[3]);
   Project=strdup(argv[4]);
   ProjectBase=strdup(argv[5]);
   a0=NULL;
   a0Count=0;
   if(argc>6)
   {	p0=ParseExpression(argv[6], &p0Count, ',', 1, 0, 1);
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
   else
   	printf("\n\"%s\" open", pcPath);
   ddStartList=0;
   r0=NULL;
   r0Count=r0Limit=0;
   IsDelimited=0;
   Use3422=0;
   UseNcpjax=0;
   IsEbcdic=0;
   IsFixed=0;
   ddHeader=NULL;
   ddPrimary=NULL;
   ddNcpjax=NULL;
   IsGMCStandard=0;
   fgets(scratch, sizeof(scratch), pcHandle);
   while(!feof(pcHandle))
   {	if(memcmp(scratch, "GMC Standard", 12)==0)
   	{	IsGMCStandard=1;
      	ddGMCStandard=ddLoadFromFile(ddGMCStandardPath);
      }
   	else
   	if(memcmp(scratch, "EBCDIC", 6)==0)
   		IsEbcdic=1;
      else
      if(memcmp(scratch, "FIXED,", 6)==0)
      {	IsFixed=1;
      	s=scratch+6;
         while(*s==' ')
         	s++;
         InLength=atoi(s);
      }
      else
   	if(memcmp(scratch, "DELIMITER", 9)==0)
   	{	// test only for tfcu
      	// strcpy(scratch, "DELIMITER, 0xFD");
      	IsDelimited=1;
      	if(OutLength==3722)
	      	Use3422=1;
      	p0=ParseExpression(scratch, &p0Count, ',', 1, 0, 1);
         if(p0Count>1)
         {	p1=p0+1;
	         if(strcmp(p1->String, "PIPE")==0)
   	      	Delimiter='|';
      	   else
         	if(strcmp(p1->String, "TAB")==0)
         		Delimiter='\t';
	         else
   	      if(strcmp(p1->String, "COMMA")==0)
      	   	Delimiter=',';
            else
            if(strlen(p1->String)==1)
			      Delimiter=*p1->String;
            else
            if(strlen(p1->String)==4 && memcmp(p1->String, "0x", 2)==0)
			   {	memset(scratch+7000, 0, 8);
			   	packit((unsigned char *)p1->String+2, (unsigned char *)scratch+7000);
			      Delimiter=*(scratch+7000);
            }
            else
            {	printf("\nInvalid delimiter:  \"%s\"", p1->String);
            	exit(1);
            }
         }
         RemoveParse(p0, p0Count);
      }
      else
   	if(memcmp(scratch, "DDLIST", 6)==0)
   		ddStartList=1;
   	else
   	if(ddStartList)
      {	p0=ParseExpression(scratch, &p0Count, ',', 1, 0, 1);
/*      	for(p1=p0; p1-p0<p0Count; p1++)
         	printf("\np1[%d]->String=\"%s\"", p1-p0, p1->String);*/
      	if(p0Count>=6)
      	{	if(r0Count==r0Limit)
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
               	if(memcmp(p1->String, "HEX_", 4)==0
                  && strlen(p1->String)%2==0)
                  {	packit((unsigned char *)p1->String+4, (unsigned char *)scratch);
                     r1->RecordIdLength=(strlen(p1->String)-4)/2;
                     r1->RecordId=(char *)malloc(r1->RecordIdLength+1);
                  	memcpy(r1->RecordId, scratch, r1->RecordIdLength);
                  }
                  else
                  if(IsEbcdic)
                  {	asc2ebc(scratch, p1->String, strlen(p1->String)+1, 0);
                  	r1->RecordId=strdup(scratch);
                     r1->RecordIdLength=strlen(r1->RecordId);
                  }
                  else
      	      	{	r1->RecordId=strdup(p1->String);
                  	r1->RecordIdLength=strlen(r1->RecordId);
                  }
         	      break;
            	case 2:	// record id offset
            		r1->RecordIdOffset=atoi(p1->String);
	               break;
   	         case 3:	// container id
      	      	r1->ContainerId=strdup(p1->String);
         	      break;
            	case 4:	// ddNumber
            		r1->ddNumber=*p1->String;
	               break;
   	         case 5:	// type
      	      	if(strcmp(p1->String, "PRIMARY")==0)
         	      {	r1->Type=PRIMARY;
                  	if(IsDelimited)
						   {	sprintf(scratch, "%s/%s/%s", ProjectBase, Project, r1->ddName);
					      	ddPrimary=ddLoadFromFile(scratch);
                        if(ddPrimary->FieldCount==0)
                        {	ddHandle=fopen(ddPrimary->Path, "rt");
                        	fgets(scratch, sizeof(scratch), ddHandle);
                           while(!feof(ddHandle))
									{	for(s=scratch+strlen(scratch)-1; s>=scratch && (*s=='\r' || *s=='\n'); s--)
	                           	*s=0;
                           	if(ddPrimary->FieldCount==ddPrimary->FieldLimit)
									   {	ddPrimary->FieldLimit+=100;
											ddPrimary->Field=(field_rec *)realloc(ddPrimary->Field, ddPrimary->FieldLimit*sizeof(field_rec));
									      memset(ddPrimary->Field+ddPrimary->FieldCount, 0, 100*sizeof(field_rec));
									   }
									   f1=ddPrimary->Field+ddPrimary->FieldCount;
                              ddPrimary->FieldCount++;
										f1->FieldName=strdup(scratch);
	                        	fgets(scratch, sizeof(scratch), ddHandle);
                           }
                           fclose(ddHandle);
                        }
                        if(ddPrimary->FieldCount==0)
                        {	printf("\n\"%s\":  FieldCount=%d", ddPrimary->Path, ddPrimary->FieldCount);
                        	exit(1);
                        }
                     }
                  }
            	   else
               	if(strcmp(p1->String, "SECONDARY")==0)
               		r1->Type=SECONDARY;
	               else
   	            if(strcmp(p1->String, "HEADER")==0)
      	         {	r1->Type=HEADER;
                  	if(IsDelimited)
						   {	sprintf(scratch, "%s/%s/%s", ProjectBase, Project, r1->ddName);
					      	ddHeader=ddLoadFromFile(scratch);
                        if(ddHeader->FieldCount==0)
                        {	ddHandle=fopen(ddHeader->Path, "rt");
                        	fgets(scratch, sizeof(scratch), ddHandle);
                           while(!feof(ddHandle))
									{	for(s=scratch+strlen(scratch)-1; s>=scratch && (*s=='\r' || *s=='\n'); s--)
	                           	*s=0;
                           	if(ddHeader->FieldCount==ddHeader->FieldLimit)
									   {	ddHeader->FieldLimit+=100;
											ddHeader->Field=(field_rec *)realloc(ddHeader->Field, ddHeader->FieldLimit*sizeof(field_rec));
									      memset(ddHeader->Field+ddHeader->FieldCount, 0, 100*sizeof(field_rec));
									   }
									   f1=ddHeader->Field+ddHeader->FieldCount;
                              ddHeader->FieldCount++;
										f1->FieldName=strdup(scratch);
	                        	fgets(scratch, sizeof(scratch), ddHandle);
                           }
                           fclose(ddHandle);
                        }
                        if(ddHeader->FieldCount==0)
                        {	printf("\n\"%s\":  FieldCount=%d", ddHeader->Path, ddHeader->FieldCount);
                        	exit(1);
                        }
                     }
                  }
  	               else
   	            if(strcmp(p1->String, "NCPJAX")==0)
                  {	if(IsDelimited)
                  	{	r1->Type=NCPJAX;
                     	UseNcpjax=1;
						   	sprintf(scratch, "%s/%s/%s", ProjectBase, Project, r1->ddName);
					      	ddNcpjax=ddLoadFromFile(scratch);
                     }
                  	else
	      	         	r1->Type=PRIMARY;
                  }
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
         }
      	RemoveParse(p0, p0Count);
      }
	   fgets(scratch, sizeof(scratch), pcHandle);
   }
   fclose(pcHandle);
/*   if(IsDelimited && r0Count==1 && strcmp(r0->RecordId, "FIRST_RECORD")==0)
   {	r1=r0+1;
   	r0Count++;
      r1->ddName=strdup("/users2/letters/dd/aztest/ncpcontainer.cbl.dd");
      r1->RecordId=strdup("ALL_OTHER_RECORDS");
      r1->RecordIdOffset=-1;
      r1->ContainerId=strdup("DELIM");
      r1->ddNumber='x';
      r1->Type=-1;
   }*/
   d0Count=d0Limit=0;
   d0=NULL;
   FromField=scratch+5000;
   if(Use3422 || IsGMCStandard)
   {  if(Use3422)
	   	sprintf(scratch, "%s/%s/%s.dd.iomap", ProjectBase, Project, Project);
      else
      if(IsGMCStandard)
	   	sprintf(scratch, "%s/%s/%s.dd.option.iomap", ProjectBase, Project, Project);
	   pcHandle=fopen(scratch, "rt");
   	if(pcHandle==NULL)
	   {	printf("\n\"%s\" open failure", scratch);
   		printf("\n");
      	exit(1);
	   }
      else
	   	printf("\nFile open successful \"%s\"", scratch);
      fgets(scratch, sizeof(scratch), pcHandle);
      while(!feof(pcHandle))
      {	for(s=scratch+strlen(scratch)-1; s>=scratch && (*s=='\r' || *s=='\n'); s--)
      		*s=0;
      	if(d0Count==d0Limit)
      	{	d0Limit+=100;
         	d0=(dst_rec *)realloc(d0, d0Limit*sizeof(dst_rec));
            memset(d0+d0Count, 0, 100*sizeof(dst_rec));
         }
         d1=d0+d0Count;
	      d0Count++;
         if(*scratch=='"')
         {	memmove(scratch, scratch+1, strlen(scratch));
         	s=strstr(scratch, "\"");
            if(s!=NULL)
            {	*s=0;
		        	if(d1->m0Count==d1->m0Limit)
      		   {	d1->m0Limit=5;
           			d1->m0=(dfrom_rec *)realloc(d1->m0, d1->m0Limit*sizeof(dfrom_rec));
		            memset(d1->m0+d1->m0Count, 0, 5*sizeof(dfrom_rec));
      		   }
               m1=d1->m0+d1->m0Count;
               m1->Type=1;
               d1->m0Count++;
            	m1->From=strdup(scratch);
               s++;
               while(*s==' ')
               	s++;
               d1->To=strdup(s);
            }
            else
            {	printf("\niomap unmatched quote: \"\"%s\"", scratch);
            	exit(1);
            }
         }
         else
         if(*scratch=='+')
         {	for(s=scratch+1, Done=0; s-scratch<strlen(scratch) && !Done; s=t+1)
         	{	t=strstr(s, "+");
            	if(t==NULL)
               {	t=strstr(s, ",");
               	Done=1;
               }
               if(t==NULL)
	            {	printf("\niomap missing comma: \"\"%s\"", scratch);
   	         	exit(1);
      	      }
               sprintf(FromField, "%.*s", t-s, s);
               if(d1->m0Count==d1->m0Limit)
      		   {	d1->m0Limit=5;
           			d1->m0=(dfrom_rec *)realloc(d1->m0, d1->m0Limit*sizeof(dfrom_rec));
		            memset(d1->m0+d1->m0Count, 0, 5*sizeof(dfrom_rec));
      		   }
               m1=d1->m0+d1->m0Count;
               d1->m0Count++;
               m1->Type=0;
               m1->From=strdup(FromField);
            }
            t++;
            while(*t==' ')
            	t++;
            d1->To=strdup(t);
         }
         else
   	   {  s=strstr(scratch, ",");
   	      if(s!=NULL)
      	   {	*s=0;
		        	if(d1->m0Count==d1->m0Limit)
      		   {	d1->m0Limit=5;
           			d1->m0=(dfrom_rec *)realloc(d1->m0, d1->m0Limit*sizeof(dfrom_rec));
		            memset(d1->m0+d1->m0Count, 0, 5*sizeof(dfrom_rec));
      		   }
               m1=d1->m0+d1->m0Count;
               m1->Type=0;
               d1->m0Count++;
            	m1->From=strdup(scratch);
               s++;
               while(*s==' ')
               	s++;
               d1->To=strdup(s);
            }
            else
            {	printf("\niomap missing comma: \"\"%s\"", scratch);
            	exit(1);
            }
         }
	      fgets(scratch, sizeof(scratch), pcHandle);
      }
      fclose(pcHandle);
	   if(IsGMCStandard)
		{  for(d1=d0; d1-d0<d0Count; d1++)
	   	{ 	d1->f0=GetFieldRecByName(ddGMCStandard, d1->To);
   			printf("\nd1[%d]->To=\"%s\", d1->f0->FieldName=\"%s\", d1->f0->Length=%d", d1-d0, d1->To, d1->f0->FieldName, d1->f0->Length);
            for(m1=d1->m0; m1-d1->m0<d1->m0Count; m1++)
            {	if(m1->Type==0)
            	{	f0=ddHeader->Field;
               	f0Count=ddHeader->FieldCount;
            		for(f1=f0, f1From=NULL; f1-f0<f0Count && f1From==NULL; f1++)
                  	if(strcmp(f1->FieldName, m1->From)==0)
                     	f1From=f1;
                  if(f1From!=NULL)
                  	m1->j0=f1From-f0;
                  else
                  {	printf("\nColumn header field \"%s\" not found", m1->From);
                  	exit(1);
                  }
               }
            	printf("\nm1[%d]->From=\"%s\", Type=%d, m1->j0=%d", m1-d1->m0, m1->From, m1->Type, m1->j0);
            }
		   }
	   }
//      exit(1);
   }
   else
   if(UseNcpjax)
   {	if(ddPrimary==NULL)
   		ddPrimary=ddHeader;
   	f0=ddPrimary->Field;
   	f0Count=ddPrimary->FieldCount;
   	f0=ddNcpjax->Field;
   	f0Count=ddNcpjax->FieldCount;
    	for(f1=f0; f1-f0<f0Count; f1++)
     	{	f1From=GetFieldRecByName(ddPrimary, f1->FieldName);
      	if(f1From!=NULL)
         {	if(d0Count==d0Limit)
           	{	d0Limit+=100;
              	d0=(dst_rec *)realloc(d0, d0Limit*sizeof(dst_rec));
               memset(d0+d0Count, 0, 100*sizeof(dst_rec));
            }
            d1=d0+d0Count;
            d0Count++;
            if(d1->m0Count==d1->m0Limit)
            {	d1->m0Limit+=5;
             	d1->m0=(dfrom_rec *)realloc(d1->m0, d1->m0Limit*sizeof(dfrom_rec));
              	memset(d1->m0+d1->m0Count, 0, 5*sizeof(dfrom_rec));
            }
            m1=d1->m0+d1->m0Count;
            d1->m0Count++;
            m1->From=strdup(f1From->FieldName);
            m1->j0=f1From-ddPrimary->Field;
            d1->To=strdup(f1->FieldName);
            d1->f0=f1;
         }
		}
   }
   printf("\nUseNcpjax=%d, d0Count=%d", UseNcpjax, d0Count);
	for(d1=d0; d1-d0<d0Count; d1++)
   {	printf("\nd1->To=\"%s\", d1->m0->j0=%d", d1->To, d1->m0->j0);
   	for(m1=d1->m0; m1-d1->m0<d1->m0Count; m1++)
      	printf("\nm1[%d]->From=\"%s\"", m1-d1->m0, m1->From);
   }
//   exit(1);
	WriteLength=OutLength;
   if(!IsFixed)
	   InLength=OutLength-CLIENT_OFFSET;
   else
   if(InLength+CLIENT_OFFSET>OutLength)
   {	printf("\nOutLength (%d) must be >= InLength (%d)+%d", OutLength, InLength, CLIENT_OFFSET);
   	exit(1);
   }
   sprintf(scratch, "%s.asc", InPath);
   OutPath=strdup(scratch);
	OutHandle = open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (OutHandle == -1)
	{	printf("\r\nFile open error %s", OutPath);
   	exit(1);
   }
	else
		printf("\r\nFile open successful %s", OutPath);
	InBuffer=(char *)malloc(InLength);
	memset(InBuffer, 0, InLength);
   OutBuffer=(char *)malloc(OutLength);
   memset(OutBuffer, 0, OutLength);
	rCount=0;
   pCount=0;
   tCount=0;
   if(IsDelimited)
   {	for(r1=r0, rFound=NULL; r1-r0<r0Count && rFound==NULL; r1++)
   		if(r1->Type==PRIMARY
         && strcmp(r1->RecordId, "ALL_OTHER_RECORDS")==0
         || r1->Type==NCPJAX)
           	rFound=r1;
   	if(rFound!=NULL)
	   {	sprintf(scratch, "%s/%s/%s", ProjectBase, Project, rFound->ddName);
      	rFound->dd=ddLoadFromFile(scratch);
      	f0=rFound->dd->Field;
         f0Count=rFound->dd->FieldCount;
         if(strcmp(rFound->RecordId, "ALL_OTHER_RECORDS")==0)
	         IsMultiRecordDelimited=0;
         else
         	IsMultiRecordDelimited=1;
      }
      else
      {	printf("\nNo DD found for delimited primary");
      	exit(1);
      }
      sprintf(scratch, "%s.pri", InPath);
	   Out2Buffer=(char *)malloc(OutLength);
   	memset(Out2Buffer, 0, OutLength);
      Out2Path=strdup(scratch);
		Out2Handle=OpenFile(Out2Path, "", OUT, NULL);
   }
   else
   if(IsFixed)
   {	for(r1=r0; r1-r0<r0Count; r1++)
   	{	sprintf(scratch, "%s/%s/%s", ProjectBase, Project, r1->ddName);
      	r1->dd=ddLoadFromFile(scratch);
      }
	   Out2Buffer=(char *)malloc(OutLength);
   	memset(Out2Buffer, 0, OutLength);
   }
   if(IsFixed)
   {	fHandle=open(InPath, O_RDONLY | O_LARGEFILE);
   	if(fHandle==-1)
      {	printf("\n\"%s\" open error", InPath);
      	exit(1);
      }
   	r=read(fHandle, InBuffer, InLength);
   }
   else
	{	                               
                //tHandle=fopen(InPath, "rt");^M
                int iHandle;
                iHandle=open(InPath, O_RDONLY | O_LARGEFILE);
                if(iHandle==-1)
                   {
                   printf("\r\nFile open error %s", InPath);
                   exit(1);
                   }
                else
                  printf("\r\nFile open successful %s", InPath);
                tHandle=fdopen(iHandle, "r");
                if(tHandle==NULL)
                   {
                   printf("\n_fdopen() returns NULL");
                   exit(1);
                   }
                else
                   printf("\nfdopen() returns a FILE * handle");

   	fgets(InBuffer, InLength, tHandle);
      if(strstr(InBuffer, "\r\n")==NULL)
      	NeedCRLF=1;
      else
      	NeedCRLF=0;
      if(NeedCRLF)
      	strcpy(InBuffer+strlen(InBuffer)-1, "\r\n");
   }
	while(IsFixed && r>0 || !IsFixed && !feof(tHandle))
	{/* 	if(strcmp(Project, "mbfiserv")==0)
     	{	ebc2asc(scratch, InBuffer, 1, 0);
	     	printf("\nr=%d, rcount=%d, Offset 0:  '%c'", r, rCount, *scratch);
      }*/
      if(!IsFixed)
      {	tLength=strlen(InBuffer);
	      if(tLength>=InLength-1)
   	   {	printf("\n\n\"%s\": Record %d is too long for setup.", InPath, rCount);
      		printf("\nLine length should be < %d-300 (Outlength-300)", OutLength);
            printf("\n\n");
         	exit(1);
	      }
      }
   	rCount++;
   	for(r1=r0, rFound=NULL; r1-r0<r0Count && rFound==NULL; r1++)
      { 	if(r1->RecordIdLength>0 && memcmp(r1->RecordId, InBuffer+r1->RecordIdOffset, r1->RecordIdLength)==0
      	|| (rCount==1 && strcmp(r1->RecordId, "FIRST_RECORD")==0)
     	   || strcmp(r1->RecordId, "ALL_OTHER_RECORDS")==0)
         	rFound=r1;
      }
	   if(rFound==NULL)
		{	printf("\nRecord %d, no matching record type found", rCount-1);
      	if(strcmp(Project, "mbfiserv")==0)
      	{	ebc2asc(scratch, InBuffer, 1, 0);
	      	printf("\nOffset 0:  '%c'", *scratch);
         }
        	exit(1);
      }
      rFound->Count++;
      memset(OutBuffer, ' ', OutLength);
      if(rFound->Type==PRIMARY
      || IsDelimited && strcmp(rFound->RecordId, "ALL_OTHER_RECORDS")==0)
      {	tCount=1;
			pCount++;
      }
      else
      if(rFound->Type==SECONDARY)
        	tCount++;
      else
      if(!IsGMCStandard && rFound->Type==HEADER)
      {	tCount=0;
      	if(IsDelimited)
         {	q0=ParseExpression(InBuffer, &q0Count, Delimiter, 1, 0, 1);
            if(Use3422 || IsGMCStandard)
            {	for(d1=d0; d1-d0<d0Count; d1++)
            	{	for(m1=d1->m0; m1-d1->m0<d1->m0Count; m1++)
               	{	if(m1->Type==0)
                  	{	for(q1=q0, qFound=NULL; q1-q0<q0Count && qFound==NULL; q1++)
                     		if(strcmp(m1->From, q1->String)==0)
                           	qFound=q1;
                     	if(qFound!=NULL)
                        	m1->j0=qFound-q0;
                        else
                        {	printf("\nMissing column \"%s\"", m1->From);
                        	exit(1);
                        }
                     }
                  }
   	        		d1->f0=GetFieldRecByName(ddPrimary, d1->To);
               }
            }
         }
      }
      if(IsDelimited
      && (rFound->Type==PRIMARY
      ||  strcmp(rFound->RecordId, "ALL_OTHER_RECORDS")==0))
      {	memset(Out2Buffer, ' ', OutLength);
        	p0=ParseExpression(InBuffer, &p0Count, Delimiter, 1, 0, 1);
	      for(d1=d0; d1-d0<d0Count; d1++)
      	{	for(m1=d1->m0, s=scratch; m1-d1->m0<d1->m0Count; m1++, s+=strlen(s))
         	{	p1=p0+m1->j0;
           		if(d1->m0Count==1 || m1==d1->m0)
	           		strcpy(s, p1->String);
	            else
   	           	sprintf(s, " %s", p1->String);
               printf("\np1[%d]->String=\"%s\"", p1-p0, p1->String);
      	   }
            // remove leading spaces
            //   perhaps this should only be for combined fields,
            //   i.e., when d1->m0Count>1 ???
            while(*scratch==' ')
            	memmove(scratch, scratch+1, strlen(scratch));
            Length=strlen(scratch);
           	if(d1->f0->Length<Length)
           		Length=d1->f0->Length;
            printf("\nLength=%d", Length);
	         memcpy(Out2Buffer+d1->f0->Offset, scratch, Length);
   	   }
//         exit(1);
         sprintf(scratch, "%09d%06d%08d", rCount, tCount, pCount);
	  	   memcpy(Out2Buffer+WriteLength-NCP_OFFSET, scratch, strlen(scratch));
   	  	memcpy(Out2Buffer+WriteLength-RECORDTYPE_OFFSET, rFound->ContainerId, 5);
      	*(Out2Buffer+WriteLength-DDNUMBER_OFFSET)=rFound->ddNumber;
			write(Out2Handle, Out2Buffer, WriteLength);
     	   RemoveParse(p0, p0Count);
      }
      else
      if(IsFixed)
      {	memset(Out2Buffer, ' ', OutLength);
      	if(IsEbcdic)
      	{	f0=rFound->dd->Field;
	         f0Count=rFound->dd->FieldCount;
				for(f1=f0; f1-f0<f0Count && f1->Offset<InLength; f1++)
      	   {	if(f1->DataType==8 && !FieldIsPacked(InBuffer+f1->Offset, f1->Length))
         			f1->DataType=0;
	         	else
					if(f1->DataType==8 && FieldIsPacked(InBuffer+f1->Offset, f1->Length))
   	   	   	f1->DataType=2;
      	   	if(f1->DataType==0)
         			e2aControl=0;
         		else
	            if(f1->DataType==1)
   	         	e2aControl=2;
      	      else
         	   	e2aControl=1;
					ebc2asc(Out2Buffer+f1->Offset, InBuffer+f1->Offset, f1->Length, e2aControl);
	         }
         }
         else
         	memcpy(Out2Buffer, InBuffer, InLength);
      }
      if(IsFixed)
      	memcpy(OutBuffer, Out2Buffer, InLength);
      else
      {	if(IsDelimited && Delimiter!='|')
      		for(s=InBuffer; *s!=0; s++)
            {	if(ClientNumber==166
            	&& (*s=='|' || *s=='"'))
						*s=' ';
            	if(*s=='|')
            	{	printf("\n\"%s\" record %d has pipes", InPath, rCount);
               	exit(1);
               }
            	if(*s==Delimiter)
	         		*s='|';
            }
			memcpy(OutBuffer, InBuffer, strlen(InBuffer));
      }
      sprintf(scratch, "%09d%06d%08d", rCount, tCount, pCount);
      memcpy(OutBuffer+WriteLength-NCP_OFFSET, scratch, strlen(scratch));
      memcpy(OutBuffer+WriteLength-RECORDTYPE_OFFSET, rFound->ContainerId, 5);
      *(OutBuffer+WriteLength-DDNUMBER_OFFSET)=rFound->ddNumber;
		write(OutHandle, OutBuffer, WriteLength);
		memset(InBuffer, 0, InLength);
	   if(IsFixed)
   	  	r=read(fHandle, InBuffer, InLength);
	   else
   	{  fgets(InBuffer, InLength, tHandle);
	      if(NeedCRLF)
   	   	strcpy(InBuffer+strlen(InBuffer)-1, "\r\n");
      }
		if(rCount%1000==0)
			printf("\nrCount=%ld", rCount);
	}
   if(IsFixed)
   	close(fHandle);
   else
		fclose(tHandle);
	close(OutHandle);
   if(IsDelimited)
   	close(Out2Handle);
   sprintf(scratch, "%s.total", InPath);
   OutHandle=OpenFile(scratch, "", OUT, NULL);
   memset(scratch, ' ', sizeof(scratch));
   while(pCount>sizeof(scratch))
   {	write(OutHandle, scratch, sizeof(scratch));
   	pCount-=sizeof(scratch);
   }
	write(OutHandle, scratch, pCount);
	close(OutHandle);
   sprintf(scratch, "%s.rectype", InPath);
   rtPath=strdup(scratch);
   OutHandle=OpenFile(rtPath, "", OUT, NULL);
   for(r1=r0; r1-r0<r0Count; r1++)
   {	if(r1->Type==PRIMARY || r1->Type==SECONDARY || r1->Type==HEADER)
   	{	sprintf(scratch, "%-5.5s %09d", r1->ContainerId, r1->Count);
	   	write(OutHandle, scratch, strlen(scratch));
      }
   }
   close(OutHandle);
	free(InBuffer);
   free(OutBuffer);
	printf("\nrCount=%ld", rCount);
	return(0);
}

