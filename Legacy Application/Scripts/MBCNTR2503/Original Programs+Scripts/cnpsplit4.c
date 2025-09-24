/* ******************************************

	cnpsplit4.c

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

#define ANY 0
#define ASCII 1

#include "cnp01.h"

typedef struct match_s {
	char *Match;
   int MatchCount;
} match_rec;

match_rec *m0;
int m0Count, m0Limit;

typedef struct rnum_s {
	match_rec *m1;
   int RecordNumber;
} rnum_rec;

typedef struct file4_s {
	char *Path;
   int Size;
   rnum_rec *r0;
   int r0Count, r0Limit;
} file4_rec;

char scratch[8192];

int TokenFound(char *Token, int TokenLength, char *Source, int SourceLength);

void main(int argc, char *argv[])

{	int InHandle, RecordLength, Offset, Length, i, MaxReads, MaxSearches,
		OutHandle, r, rcount, w, wcount, done, found, FoundTotal, Continue,
      MatchHandle, UnMatchHandle, SearchType;
   char *s, *t, *u, *ReadBuffer, *CountFile, *SelectionList, *FileName,
   	*OutFile;
   file4_rec *f0, *f1, *Found;
   int f0Count;
   parse_rec *p0, *p1;
   int p0Count;
   struct stat statbuf;
   match_rec *m1, *m2;
   rnum_rec *r1;
   FILE *fSelect;

	printf("\n\n******* B E G I N   C N P S P L I T 4 *******\n\n");
	if (argc<10)
   {	printf("\nUsage is \"cnpsplit4.out A B C D E F G H I\"");
      printf("\n  A = Record Length");
      printf("\n  B = Offset to selection field");
      printf("\n  C = Length of selection field");
      printf("\n  D = Selection value list");
      printf("\n      Use \"_\" character to represent spaces in selection list");
      printf("\n      Can be a unix path to a file of selection values");
      printf("\n  E = Maximum number of records to read within each file");
      printf("\n  F = Maximum number of matches to look for per selection item");
      printf("\n  G = Output File (full path)");
      printf("\n  H = Search Type (\"ANY\" or \"ASCII\")");
		printf("\n  I = Input File (full path)");
      printf("\n  For example: \"cnpsplit4.out 650 1 3 133,173,602 100 5 /users/coupons/boa.files ANY /users/coupons/*.srt\"");
      printf("\n\n");
   	exit(1);
   }
   RecordLength=atoi(argv[1]);
   Offset=atoi(argv[2]);
   Length=atoi(argv[3]);
   SelectionList=strdup(argv[4]);
   if(*SelectionList=='/' && _stat(SelectionList, &statbuf)==0)
   {  m0=NULL;
      m0Count=m0Limit=0;
   	fSelect=fopen(SelectionList, "rt");
      fgets(scratch, sizeof(scratch), fSelect);
   	while(!feof(fSelect))
      {	for(t=scratch+strlen(scratch)-1;
         t>=scratch && (*t=='\n' || *t=='\r');
         t--)
         	*t=0;
         if(strlen(scratch)>0)
         {	if(m0Count==m0Limit)
         	{	m0Limit+=100;
            	m0=(match_rec *)realloc(m0, m0Limit*sizeof(match_rec));
               memset(m0+m0Count, 0, 100*sizeof(match_rec));
            }
            m1=m0+m0Count;
            m1->Match=strdup(scratch);
            m0Count++;
         }
      	fgets(scratch, sizeof(scratch), fSelect);
      }
      fclose(fSelect);
   }
   else
	{	p0=ParseExpression(SelectionList, &p0Count, ',', 0, 0, 1);
/*      for(s=SelectionList; *s!=0; s++)
   		if(*s=='_')
      		*s=' ';*/
      m0Count=m0Limit=p0Count;
      m0=(match_rec *)malloc(m0Limit*sizeof(match_rec));
      memset(m0, 0, m0Limit*sizeof(match_rec));
      for(p1=p0, m1=m0; p1-p0<p0Count; p1++, m1++)
      {	m1->Match=strdup(p1->String);
	      for(s=m1->Match; *s!=0; s++)
   			if(*s=='_')
      			*s=' ';
      }
   }
   printf("\nm0Count=%d", m0Count);
   MaxReads=atoi(argv[5]);
   MaxSearches=atoi(argv[6]);
   OutFile=strdup(argv[7]);
   if(strcmp("ASCII", argv[8])==0)
   	SearchType=ASCII;
   else
   	SearchType=ANY;
   f0Count=argc-9;
   f0=(file4_rec *)malloc(f0Count*sizeof(file4_rec));
   memset(f0, 0, f0Count*sizeof(file4_rec));
   for(i=9, f1=f0; i<argc; i++, f1++)
   {  f1->Path=strdup(argv[i]);
   	printf("\nargv[%d]=%s", i, argv[i]);
   }
   if(RecordLength<=0)
   {	printf("\nRecordLength (%d) must be > 0", RecordLength);
   	exit(1);
   }
   if(Offset<0)
   {	printf("\nOffset (%d) must be >= 0", Offset);
   	exit(1);
   }
   if(Length<=0)
   {	printf("\nLength (%d) must be > 0", Length);
   	exit(1);
   }
   if(Offset+Length>RecordLength)
   {	printf("\nOffset+Length (%d+%d) must be <= RecordLength (%d)",
   		Offset, Length, RecordLength);
   	exit(1);
   }
//  	f0=GetFileList(InFile, Offset, Length);
   ReadBuffer=(char *)malloc(RecordLength);
   OutHandle=OpenFile(OutFile, "", OUT, NULL);
   for(f1=f0, Continue=1; f1-f0<f0Count && Continue; f1++)
   {	done=0;
      _stat(f1->Path, &statbuf);
		f1->Size=statbuf.st_size;
      if(f1->Size%RecordLength || f1->Size==0)
      	done=1;
      if(!done)
      {	InHandle=OpenFile(f1->Path, "", IN, NULL);
      	MatchHandle=-1;
         UnMatchHandle=-1;
	      for(rcount=0, r=1;
   	   r>0 && (MaxReads==0 || rcount<MaxReads);
      	rcount++)
			{	r=read(InHandle, ReadBuffer, RecordLength);
		      for(m1=m0, Found=NULL; m1-m0<m0Count && Found==NULL; m1++)
   		   {  if(MaxSearches==0 || m1->MatchCount<MaxSearches)
					{	if(TokenFound(m1->Match, strlen(m1->Match), ReadBuffer+Offset, Length))
	      				Found=f1;
		            else
                  if(SearchType==ANY)
   		         {	ebc2asc(scratch, ReadBuffer+Offset, Length, 0);
      		      //	if(memcmp(scratch, p1->String, Length)==0)
         		   	if(TokenFound(m1->Match, strlen(m1->Match), scratch, Length))
	      					Found=f1;
	            		else
		   	         {	unpackit((unsigned char *)ReadBuffer+Offset, (unsigned char *)scratch, Length);
   		   	      	if(strstr(scratch, m1->Match)!=NULL)
	   		   				Found=f1;
							}
            		}
               }
            }
           	if(Found!=NULL)
  	         {	m1--;
            	if(Found->r0Count==Found->r0Limit)
   	      	{	Found->r0Limit+=100;
  	   	      	Found->r0=(rnum_rec *)realloc(Found->r0, Found->r0Limit*sizeof(rnum_rec));
     	   	      memset(Found->r0+Found->r0Count, 0, 100*sizeof(rnum_rec));
        	   	}
           	   r1=Found->r0+Found->r0Count;
            	Found->r0Count++;
     	      	r1->m1=m1;
      	      r1->RecordNumber=rcount;
  	            m1->MatchCount++;
     	         if(m1->MatchCount==MaxSearches)
        	      {	for(m2=m0, Continue=0; m2-m0<m0Count && !Continue; m2++)
           	   		if(m2->MatchCount<MaxSearches)
              	      	Continue=1;
               }
               if(r>0)
               {	if(MatchHandle==-1)
                  {	sprintf(scratch, "%s.match", f1->Path);
                  	MatchHandle=OpenFile(scratch, "", OUT, NULL);
  	               }
     	            write(MatchHandle, ReadBuffer, RecordLength);
               }
            }
            else
            {	if(r>0)
               {	if(UnMatchHandle==-1)
                  {	sprintf(scratch, "%s.unmatch", f1->Path);
                  	UnMatchHandle=OpenFile(scratch, "", OUT, NULL);
  	               }
     	            write(UnMatchHandle, ReadBuffer, RecordLength);
               }
            }
         }
		   close(InHandle);
         if(MatchHandle!=-1)
         	close(MatchHandle);
         if(UnMatchHandle!=-1)
         	close(UnMatchHandle);
      }
   }
   free(ReadBuffer);
   for(f1=f0, FoundTotal=0; f1-f0<f0Count; f1++)
   {	for(r1=f1->r0; r1-f1->r0<f1->r0Count; r1++)
      {	if(r1==f1->r0)
	      {	sprintf(scratch, "\n\nFile \"%s\" (%d bytes)", f1->Path, f1->Size);
   	   	write(OutHandle, scratch, strlen(scratch));
         }
	      sprintf(scratch, "\n  %s:  Seq# %d", r1->m1->Match, r1->RecordNumber+1);
	   	write(OutHandle, scratch, strlen(scratch));
      }
      FoundTotal+=f1->r0Count;
   }
   close(OutHandle);
   printf("\n\n%d matches found",  FoundTotal);
   printf("\nType \"pg %s\" to see results\n\n", OutFile);
	return;
}

int TokenFound(char *Token, int TokenLength, char *Source, int SourceLength)

{	char *s;

	for(s=Source; s-Source<=SourceLength-TokenLength; s++)
   	if(memcmp(Token, s, TokenLength)==0)
      	return(1);
   return(0);
}

