/* ******************************************

	cnpautolabel2.c


   I think the logic suggested here
   requires that the OPTION record types
   precede the SAMPLE record types
   for a given JOBID

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

#define LABEL_PARSE_COUNT 11
#define TOTAL_PARSE_COUNT 8
#define STAGGER_JOBS 30000

typedef struct label_s {
	char *JobType;
   char *JobId;
   char *JobNumber;
   int JobIndex;
   char *Client;
   char *Product;
   char *Department;
   char *PrintProgram;
   char *OutExtension;
   int RecordLength;
   char *InPath;
   char *InMask;
   char *SortId;
} label_rec;

label_rec *b0, *b1, *b2, *bFound;
int b0Count, b0Limit;

typedef struct tot_s {
	char *Path;
   char *File;
   char *InMask;
   int RecordLength;
   int Size;
   int Count;
   char **d0;
   int d0Count;
} tot_rec;

tot_rec *t0, *t1, *t2;
int t0Count, t0Limit;

typedef struct file_s {
	char *Path;
   char *Name;
   char *Extension;
   int Size;
   int Count;
   char *Job6;
   char *Client;
   char *Department;
   char *Product;
   char *Description;
   int Samples;		// 0-print (normal) option, 1-online samples, 2-xfr file, 3-imb only
} file_rec;

typedef struct job_s {
	char *Job;
   char *Job6;
   char *Client;
   char *Department;
   char *Product;
   char *Description;
   int Total;
   int Options;
   file_rec **r0;
   int r0Count, r0Limit;
   int SampleTotal;
} job_rec;

job_rec *j0, *j1, *jFound;
int j0Count, j0Limit;

char *AutoLabelControlPath;
char *SqlAutoLabelControlPath="/users/public/sql/autolbl.000";
//char *SqlAutoLabelControlPath="/users/eric/autolbl.000";
char *ProdPath="/users/public/sql/jobs/";
char *TestPath="/users/public/sql/jobs/test/";
char *TempPath="/users/public/sql/tmp/";

struct stat statbuf;
char scratch[8192], ReadBuffer[8192];

void LoadLabelControls(char *jobtype, char *client);
void LoadLabelRec(parse_rec *p0, int p0Count, char *jobtype, char *client);
void LoadTotalRec(parse_rec *p0, int p0Count, char *jobtype, char *client);

void main(int argc, char *argv[])

{	int OutHandle, i, JobTotal, Options, SaveIndex, AutoJobOne, found, pTotal,
		NextJobNumber, Out2Handle, Samples, ret;
   char *s, *t, *s1, *s2, *JobType, *Client, *Source, *FileMask, *TempFile, *cJob,
   	*OutFile, *AutoJob, *OriginalJobNumber, *Out2File, *OriginalJob6, *SqlPath;
   char **d1;
   file_rec *f0, *f1, **r1;
   int f0Count;
   int TempHandle;
   char MaxJob[32];

	printf("\n\n******* B E G I N   C N P A U T O L A B E L 2 *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"cnpautolabel2.out A B C D\"");
		printf("\n  A = Job Type");
      printf("\n  B = Client number");
      printf("\n  C = Source File");
      printf("\n  D = Original Job Number");
      printf("\n  For example: \"cnpautolabel2.out QBILL 0918 homeside.source.input 112345\"");
      printf("\n  Use Source File = \"TEST\" in order to test modifications to the control file");
      printf("\n\n");
   	exit(1);
   }
   JobType=strdup(argv[1]);
   Client=strdup(argv[2]);
   Source=strdup(argv[3]);
  	AutoLabelControlPath=SqlAutoLabelControlPath;
   strcpy(scratch, Source);
   for(s=scratch; *s!=0; s++)
   	*s=tolower(*s);
   if(strstr(scratch, "test")!=NULL)
   	SqlPath=TestPath;
   else
   	SqlPath=ProdPath;
   OriginalJobNumber=strdup(argv[4]);
   if(strlen(OriginalJobNumber)==6)
   	memmove(OriginalJobNumber, OriginalJobNumber+1, strlen(OriginalJobNumber));
   if(strlen(OriginalJobNumber)>=6)
   	OriginalJob6=strdup(OriginalJobNumber);
   else
	{	ret=JobPrefix(atoi(OriginalJobNumber));
     	sprintf(scratch, "%d%s", ret, OriginalJobNumber);
      OriginalJob6=strdup(scratch);
   }
   LoadLabelControls(JobType, Client);
   NextJobNumber=0;
  	for(b1=b0, SaveIndex=b0->JobIndex; b1-b0<b0Count; b1++)
	{	if(b1->JobIndex!=STAGGER_JOBS)
   	{	for(b2=b0; b2<b1 && b1->JobNumber==NULL; b2++)
   			if(strcmp(b1->JobId, b2->JobId)==0)
            	b1->JobNumber=b2->JobNumber;
			if(b1->JobNumber==NULL)
	      {	sprintf(scratch, "JOB%03d", NextJobNumber);
     		  	b1->JobNumber=strdup(scratch);
            NextJobNumber++;
  		   }
      }
   }
   AutoJobOne=1;
   if(b0Count==0)
   {	printf("\nNo match found for JobType \"%s\", Client=\"%s\"", JobType, Client);
      printf("\n\n");
   	exit(1);
   }
   for(t1=t0; t1-t0<t0Count; t1++)
   {	s=strstr(t1->InMask, "(JobNumber)");
   	if(s==NULL)
      	sprintf(scratch, "%s%s%s", t1->Path, OriginalJobNumber, t1->InMask);
      else
      	sprintf(scratch, "%s%.*s%s%s", t1->Path, s-t1->InMask, t1->InMask, OriginalJobNumber, s+11);
      t1->File=strdup(scratch);
      if(_stat(t1->File, &statbuf)==0)
      {	t1->Size=statbuf.st_size;
	      t1->Count=statbuf.st_size / t1->RecordLength;
      }
      else
      {	t1->Size=0;
      	t1->Count=0;
      }
   }
	sprintf(scratch, "%s%s.tmp", TempPath, OriginalJobNumber);
   TempFile=strdup(scratch);
   FileMask=NULL;
   j0=NULL;
   j0Limit=j0Count=0;
	for(b1=b0; b1-b0<b0Count; b1++)
   {  if(AutoJobOne)
      	cJob=OriginalJobNumber;
      else
      if(b1->JobIndex==STAGGER_JOBS)
      {	sprintf(MaxJob, "JOB%03d", NextJobNumber);
         cJob=MaxJob;
      	NextJobNumber++;
      }
      else
	   	cJob=b1->JobNumber;
   	s=strstr(b1->InMask, "(JobNumber)");
   	if(s==NULL)
      	sprintf(scratch, "%s%s%s", b1->InPath, OriginalJobNumber, b1->InMask);
      else
      	sprintf(scratch, "%s%.*s%s%s", b1->InPath, s-b1->InMask, b1->InMask, OriginalJobNumber, s+11);
      if(FileMask!=NULL)
      	free(FileMask);
      FileMask=strdup(scratch);
      printf("\nFileMask=%s", FileMask);
      sprintf(scratch, "/users/programs/makefilelist.out %s %s", TempFile, FileMask);
      system(scratch);
      TempHandle=OpenFile(TempFile, "", IN, NULL);
		if(strcmp(b1->PrintProgram, "NOPROG")==0)
   	{	Options=0;
      	Samples=0;
      }
	   else
      if(strcmp(b1->SortId, "99")==0)
      {	Samples=1;
   		Options=1;
      }
      else
      if(strcmp(b1->PrintProgram, "FILEXFR")==0)
      {	Samples=2;
      	Options=1;
      }
      else
      if(strcmp(b1->PrintProgram, "IMB")==0)
      {	Samples=3;
      	Options=1;
      }
      else
      {	Samples=0;
      	Options=1;
      }
      _stat(TempFile, &statbuf);
      read(TempHandle, ReadBuffer, statbuf.st_size);
      close(TempHandle);
      *(ReadBuffer+statbuf.st_size)=0;
      s1=ReadBuffer;
      while((s2=strstr(s1, "\r\n"))!=NULL)
      {	*s2=0;
	   	for(j1=j0, jFound=NULL; jFound==NULL && j1-j0<j0Count; j1++)
   		{	if((Samples==1 || Samples==3) && strcmp(j1->Job, cJob)==0)
         		jFound=j1;
         	else
         	if(Samples!=1 && strcmp(j1->Job, cJob)==0
         	&& strcmp(j1->Product, b1->Product)==0)
         		jFound=j1;
		   }
         if(( Samples==1 || Samples==3) && jFound==NULL)
         {	printf("\nNo job record found for imb-only or online samples");
         	exit(1);
         }
	      if(jFound==NULL)
		   {	if(j0Count==j0Limit)
  				{	j0Limit+=10;
	      		j0=(job_rec *)realloc(j0, j0Limit*sizeof(job_rec));
     		   	memset(j0+j0Count, 0, 10*sizeof(job_rec));
		      }
   	      jFound=j0+j0Count;
      	   j0Count++;
         	jFound->Job=strdup(cJob);
            if(strlen(jFound->Job)>=6)
            	jFound->Job6=strdup(jFound->Job);
            else
            {	ret=JobPrefix(atoi(jFound->Job));
/*            	if(ret!=5 && ret!=6)
               {	printf("\n\nJobPrefix should be 5 or 6, not %d", ret);
               	printf("\nAutolabel logic error");
                  printf("\nSee programming");
                  printf("\n\n");
                  exit(1);
               }*/
            	sprintf(scratch, "%d%s", ret, jFound->Job);
               jFound->Job6=strdup(scratch);
               // this logic should be removed once we know
               // that "jobprefix()" works correctly
/*               if(strlen(jFound->Job6)!=6)
               {	printf("\n\njFound->Job6=\"%s\" should be 6 digits", jFound->Job6);
               	printf("\nAutolabel logic error");
                  printf("\nSee programming");
                  printf("\n\n");
                  exit(1);
               }*/
            }
	         jFound->Client=strdup(b1->Client);
   	      jFound->Product=strdup(b1->Product);
      	   jFound->Description=Source;
         	jFound->Department=strdup(b1->Department);
	         jFound->Options=Options;
   	   }
      	if(jFound->r0Count==jFound->r0Limit)
	      {	jFound->r0Limit+=10;
   	   	jFound->r0=(file_rec **)realloc(jFound->r0, jFound->r0Limit*sizeof(file_rec *));
      	   memset(jFound->r0+jFound->r0Count, 0, 10*sizeof(file_rec *));
	      }
   	   r1=jFound->r0+jFound->r0Count;
         *r1=(file_rec *)malloc(sizeof(file_rec));
         f1=*r1;
         f1->Path=strdup(s1);
         if(Samples==3)
         {  sprintf(scratch, "%s.count", f1->Path);
         	b1->RecordLength=1;
         	if(_stat(scratch, &statbuf)!=0)
            	memset(&statbuf, 0, sizeof(statbuf));
         }
         else
		   	_stat(f1->Path, &statbuf);
	   	f1->Size=statbuf.st_size;
   		if(f1->Size%b1->RecordLength)
      	{	printf("\n%s file size (%d) is not divisible by record length %d", f1->Path, f1->Size, b1->RecordLength);
      		f1->Count=0;
	      }
   	   else
      		f1->Count=f1->Size / b1->RecordLength;
	      for(s=f1->Path+strlen(f1->Path)-1, t=NULL; s>=f1->Path && *s!='/'; s--)
   	   	if(t==NULL && *s=='.')
      	   	t=s;
	      s++;
	      if(t==NULL)
   	   {	printf("\n%s file name is invalid (no extension)", f1->Path);
      		f1->Count=0;
	      }
   	   else
      		f1->Extension=t;
	      if(f1->Count>0)
   	   {	strcpy(scratch, s);
	   	   t=scratch+(f1->Extension-s)+1;
         	if(strcmp(b1->OutExtension, "NONE")!=0)
         		strcpy(t, b1->OutExtension);
	         f1->Name=strdup(scratch);
            if(AutoJobOne)
            {	AutoJobOne=0;
            	if(b1->JobIndex==STAGGER_JOBS)
               {	for(b2=b1+1; b2-b0<b0Count; b2++)
                  	if(strcmp(b2->JobId, "Job1")==0)
                     	b2->JobNumber=OriginalJobNumber;
               }
               else
			      	for(b2=b1+1; b2-b0<b0Count; b2++)
   	   		   	if(strcmp(b2->JobId, b1->JobId)==0
                     //
                     // the following condition should prevent the
                     // problem that can occur when there is an online
                     // sample file designated as "Job1" but there
                     // are no "Job1" files found in the job.  When this
                     // is the case, the following logic still associates
                     // the online sample file with the first job#
                     // This occurred for client 0102 coupons in a job
                     // where all the files had "Job4" designation,
                     // except for the online sample file, which had
                     // "Job1" designation.
                     //
                     || strcmp(b2->SortId, "99")==0
                     && strcmp(b2->JobId, "Job1")==0
                     || strcmp(b2->PrintProgram, "IMB")==0
                     && strcmp(b2->JobId, "Job1")==0
                     || strcmp(b2->JobNumber, "JOB000")==0)
      	      			b2->JobNumber=OriginalJobNumber;
            }
   	   }
      	else
	      	f1->Name=strdup(s);
	      f1->Description=b1->PrintProgram;
	      f1->Client=b1->Client;
	      f1->Job6=strdup(jFound->Job6);
	      f1->Department=b1->Department;
	      f1->Product=b1->Product;
         f1->Samples=Samples;
	      jFound->r0Count++;
         if(Samples==1)
         	jFound->SampleTotal+=f1->Count;
         else
         if(Samples!=3)
	   	   jFound->Total+=f1->Count;
         s1=s2+2;
	      if(b1->JobIndex==STAGGER_JOBS)
   	   {	sprintf(MaxJob, "JOB%03d", NextJobNumber);
         	b1->JobNumber=MaxJob;
            NextJobNumber++;
            cJob=MaxJob;
	      }
	   }
   }
   printf("\n\n");
   for(t1=t0; t1-t0<t0Count; t1++)
   {	for(j1=j0, pTotal=0; j1-j0<j0Count; j1++)
   	{	if(strcmp(Client, "0310")==0 && strcmp(JobType, "MBILLCNTR")==0)
	      {	for(d1=t1->d0, found=0; d1-t1->d0<t1->d0Count && !found; d1++)
   	   		if(strcmp(*d1, j1->Product)==0 && j1->Options)
      	      {	found=1;
         	   	pTotal+=j1->Total;
            	   printf("\nProduct=%s, Total=%d", j1->Product, j1->Total);
	            }
         }
         else
	      {	for(d1=t1->d0, found=0; d1-t1->d0<t1->d0Count && !found; d1++)
   	   		if(strcmp(*d1, j1->Product)==0)
      	      {	found=1;
         	   	pTotal+=j1->Total;
            	   printf("\nProduct=%s, Total=%d", j1->Product, j1->Total);
	            }
         }
      }
		for(t2=t1+1; t2-t0<t0Count && strcmp(*t2->d0, "EXCLUDED")==0; t2++)
      {	pTotal+=t2->Count;
      	printf("\nEXCLUDED Total=%d", t2->Count);
      }
      if(pTotal!=t1->Count)
      {	printf("\nBalancing check fails:");
      	printf("\nBALANCE total = %d", t1->Count);
         printf("\nPRODUCT total = %d\n  For products:", pTotal);
         for(d1=t1->d0; d1-t1->d0<t1->d0Count; d1++)
         	printf("\n      %s", *d1);
         exit(1);
      }
      else
      	printf("\n%s TOTAL (%d) equals PRODUCT TOTAL (%d)", t1->File, t1->Count, pTotal);
      t1=t2-1;
   }
   sprintf(scratch, "%s%s.%s", TempPath, OriginalJob6, Client);
   OutFile=strdup(scratch);
	OutHandle=OpenFile(OutFile, "", OUT, NULL);
   sprintf(scratch, "%s%s.options.paths", TempPath, OriginalJobNumber);
   Out2File=strdup(scratch);
   Out2Handle=OpenFile(Out2File, "", OUT, NULL);
   for(j1=j0; j1-j0<j0Count; j1++)
   {	if(j1->Total>0)
   	{	sprintf(scratch, "J|%s|%s|%s|%s|%06d|%-256s|%-35s\r\n",
	  			j1->Job6, j1->Client, j1->Department, j1->Product, j1->Total, j1->Description, "");
		   write(OutHandle, scratch, strlen(scratch));
         if(j1->Options)
	      	for(r1=j1->r0; r1-j1->r0<j1->r0Count; r1++)
		      {	f1=*r1;
   		   	if(f1->Count>0)
	   			{  if(f1->Samples==1)
	               	sprintf(scratch, "S|%s|%s|%s|%s|%06d|%-256s|%-35s\r\n",
   							j1->Job6, j1->Client, f1->Department, f1->Product, f1->Count, f1->Name, f1->Description);
               	else
	   				if(f1->Samples==2)
	               	sprintf(scratch, "T|%s|%s|%s|%s|%06d|%-256s|%-35s\r\n",
   							j1->Job6, j1->Client, f1->Department, f1->Product, f1->Count, f1->Name, f1->Description);
               	else
	   				if(f1->Samples==3)
	               	sprintf(scratch, "I|%s|%s|%s|%s|%06d|%-256s|%-35s\r\n",
   							j1->Job6, j1->Client, f1->Department, f1->Product, f1->Count, f1->Name, f1->Description);
               	else
      	         	sprintf(scratch, "O|%s|%s|%s|%s|%06d|%-256s|%-35s\r\n",
   							j1->Job6, j1->Client, j1->Department, j1->Product, f1->Count, f1->Name, f1->Description);
	   				write(OutHandle, scratch, strlen(scratch));
                  sprintf(scratch, "%s\r\n", f1->Path);
                  write(Out2Handle, scratch, strlen(scratch));
   		      }
      		}
      }
   }
   printf("\n\n");
   close(OutHandle);
   close(Out2Handle);
   sprintf(scratch, "mv %s %s", OutFile, SqlPath);
   system(scratch);
	return;
}

void LoadLabelControls(char *jobtype, char *client)

{	FILE *InHandle;
	parse_rec *p0, *p1;
   int p0Count;
   char *s;

	b0=NULL;
   b0Count=b0Limit=0;
	t0=NULL;
   t0Count=t0Limit=0;
	InHandle=fopen(AutoLabelControlPath, "r");
   if(InHandle==NULL)
   {	printf("\n\nControl File \"%s\" not found\n\n", AutoLabelControlPath);
   	exit(1);
   }
   if(_stat(AutoLabelControlPath, &statbuf)==0)
	{	InHandle=fopen(AutoLabelControlPath, "r");
   	if(InHandle==NULL)
   	{	printf("\n\nControl File \"%s\" not found\n\n", AutoLabelControlPath);
  			exit(1);
   	}
   	while(fgets(scratch, sizeof(scratch)-1, InHandle)!=NULL)
	   {	p0=ParseExpression(scratch, &p0Count, ',', 0, 0, 1);
     		for(p1=p0; p1-p0<p0Count; p1++)
        		for(s=p1->String+strlen(p1->String)-1; s>=p1->String && *s==' '; s--)
        			*s=0;
   		if(p0Count>1)
	   		if(strcmp((p0+1)->String, "TOTAL")==0)
  		   		LoadTotalRec(p0, p0Count, jobtype, client);
      		else
  	   			LoadLabelRec(p0, p0Count, jobtype, client);
      	RemoveParse(p0, p0Count);
	   }
  		fclose(InHandle);
   }
   else
  		printf("\nCannot find file %s", SqlAutoLabelControlPath);
}

void LoadLabelRec(parse_rec *p0, int p0Count, char *jobtype, char *client)

{	parse_rec *p1;
   int i;

	if(p0Count>0)
	{  if(p0Count!=LABEL_PARSE_COUNT)
  	   {	printf("\n%s:  Invalid record (%s)", AutoLabelControlPath, scratch);
			printf("\nParse count = %d, should be %d", p0Count, LABEL_PARSE_COUNT);
      	printf("\n\n");
        	exit(1);
      }
  	}
   if(strcmp(p0->String, jobtype)==0
   && strcmp((p0+2)->String, client)==0)
  	{	if(b0Count==b0Limit)
  		{	b0Limit+=10;
     		b0=(label_rec *)realloc(b0, b0Limit*sizeof(label_rec));
	      memset(b0+b0Count, 0, 10*sizeof(label_rec));
  	   }
      b1=b0+b0Count;
     	b0Count++;
      for(p1=p0; p1-p0<p0Count; p1++)
	   {	switch(p1-p0) {
  	   	case 0:	// jobtype
     	   	b1->JobType=strdup(p1->String);
        	   break;
         case 1:	// jobid
         	b1->JobId=strdup(p1->String);
            if(strcmp(b1->JobId, "STAGGER")==0)
            	b1->JobIndex=STAGGER_JOBS;
            else
               b1->JobIndex=atoi(b1->JobId+3)-1;
     	      break;
        	case 2:
        		b1->Client=strdup(p1->String);
            break;
         case 3:
  	      	b1->Product=strdup(p1->String);
     	      break;
        	case 4:
        		b1->Department=strdup(p1->String);
            break;
         case 5:
  	      	b1->PrintProgram=strdup(p1->String);
     	      break;
        	case 6:
        		b1->OutExtension=strdup(p1->String);
            break;
         case 7:
  	      	b1->RecordLength=atoi(p1->String);
     	      if(b1->RecordLength<=0)
        	   {	printf("\nInvalid record length (%d)", b1->RecordLength);
			  	   printf("\n\n");
           		exit(1);
            }
            break;
  	      case 8:
				b1->InPath=strdup(p1->String);
        	   break;
         case 9:
  	      	b1->InMask=strdup(p1->String);
     	      break;
         case 10:
         	b1->SortId=strdup(p1->String);
            break;
         default:
        		break;
         }
     	}
   }
}

void LoadTotalRec(parse_rec *p0, int p0Count, char *jobtype, char *client)

{	parse_rec *p1;
   int i;
   char **d1;

//COUPN, TOTAL, 0310, 650, /users/public/dovy/, .wrg, EXCLUDED

	if(p0Count>0)
	{  if(p0Count<TOTAL_PARSE_COUNT)
  	   {	printf("\n%s:  Invalid record (%s)", AutoLabelControlPath, scratch);
			printf("\nParse count = %d, should be >= %d", p0Count, TOTAL_PARSE_COUNT);
      	printf("\n\n");
        	exit(1);
      }
  	}
   if(strcmp(p0->String, jobtype)==0
   && strcmp((p0+2)->String, client)==0)
  	{	if(t0Count==t0Limit)
  		{	t0Limit+=10;
     		t0=(tot_rec *)realloc(t0, t0Limit*sizeof(tot_rec));
	      memset(t0+t0Count, 0, 10*sizeof(tot_rec));
  	   }
      t1=t0+t0Count;
     	t0Count++;
      t1->d0Count=p0Count-TOTAL_PARSE_COUNT+1;
      t1->d0=(char **)malloc(t1->d0Count*sizeof(char *));
      memset(t1->d0, 0, t1->d0Count*sizeof(char *));
      d1=t1->d0;
      for(p1=p0; p1-p0<p0Count-1; p1++)
	   {	switch(p1-p0) {
  	   	case 0:	// jobtype
        	   break;
         case 1:	// BALANCE
     	      break;
        	case 2:	// client
            break;
         case 3:
  	      	t1->RecordLength=atoi(p1->String);
     	      if(t1->RecordLength<=0)
        	   {	printf("\nInvalid record length (%d)", t1->RecordLength);
			  	   printf("\n\n");
           		exit(1);
            }
     	      break;
        	case 4:
        		t1->Path=strdup(p1->String);
            break;
         case 5:
  	      	t1->InMask=strdup(p1->String);
     	      break;
         default:
         	*d1=strdup(p1->String);
            d1++;
        		break;
         }
     	}
   }
}

