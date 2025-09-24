#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <math.h>
#include <sys/mtio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "cnp01.h"

typedef struct sort_s {
	char *sKey;
   int sNum;
} sort_rec;

sort_rec *s0, *s1, *s2, *s3, sCandidate;
int s0Count, s0Limit;

typedef struct merge_s {
   char *Path;
   int Handle;
   int Size;
   int Length;
   char *Buffer;
   char *Key;
} merge_rec;

merge_rec *g0, *g1, **pg0, **pg1, *gCandidate;
int g0Count, g0Limit, pg0Count, MERGE_LIMIT;

char **f0, **f1;
int f0Count, f0Limit;

char **m0, **m1;
int m0Count, m0Limit;

char scratch[8192], *rBuffer, *r1, *SystemCommand;

// test path for temporary files
char *SortPath = "/users/eric/";

// production path for temporary files
//char *SortPath = "/users/public/";

int main(int argc, char *argv[])
{
   int InHandle, OutHandle, rTotal, rLength, rSegmentTotal,
   	sTotal, sOffset, sLength, wTotal, i, ReadLength,
      ret, MergeStep, rSegmentSeek, WriteCount;
   char *InPath, *InFileName, *s;
   struct stat statbuf;
   int ok;

	if(argc<5)
   {	printf("\n\nUsage is \"NcpHexSort A B C D\"");
   	printf("\n  A = Full path to input file");
      printf("\n  B = Record length");
      printf("\n  C = Offset to sort field");
      printf("\n  D = Length of sort field");
      printf("\n\n");
   	exit(1);
   }

   InPath=strdup(argv[1]);
   rLength=atoi(argv[2]);
   sOffset=atoi(argv[3]);
   sLength=atoi(argv[4]);
   InHandle=open(InPath, O_RDONLY);
   if(InHandle==-1)
   {	printf("\n%s open failure in NcpSort", InPath);
   	exit(1);
   }
   if(rLength<1)
   {	printf("\nRecord Length (%d) must be positive", rLength);
   	exit(1);
   }
   if(sOffset<0)
   {	printf("\nSort Offset (%d) must be >= 0", sOffset);
   	exit(1);
   }
   if(sLength<1)
   {	printf("\nSort Length (%d) must be >= 0", sLength);
   	exit(1);
   }
   if(sOffset+sLength>rLength)
   {	printf("\nSortOffset+SortLength must be <= RecordLength (%d+%d<=%d)",
   		sOffset, sLength, rLength);
   	exit(1);
   }
   for(s=InPath+strlen(InPath)-1; s>=InPath && *s!='/'; s--)
   	;
   s++;
   InFileName=strdup(s);
   stat(InPath, &statbuf);
   rTotal=statbuf.st_size / rLength;
   sTotal=rTotal;
//   s0Limit=(int)pow((double)sTotal, 0.5) + 1;
	for(s0Limit=1; s0Limit*s0Limit<sTotal; s0Limit++)
   	;
//   printf("\nsTotal=%d, s0Limit=%d", sTotal, s0Limit);
	s0=(sort_rec *)malloc((s0Limit+1)*sizeof(sort_rec));
   memset(s0, 0, (s0Limit+1)*sizeof(sort_rec));
   for(s1=s0; s1-s0<s0Limit; s1++)
   {	s1->sKey=(char *)malloc(sLength+1);
   	memset(s1->sKey, 0, sLength+1);
   }
   memset(scratch, 0, sizeof(scratch));
   rBuffer=(char *)malloc(rLength);
   memset(rBuffer, 0, rLength);
   f0Limit=s0Limit;
   f0Count=0;
   f0=(char **)malloc(f0Limit*sizeof(char *));
   memset(f0, 0, f0Limit*sizeof(char *));
   memset(&sCandidate, 0, sizeof(sort_rec));
   sCandidate.sKey=(char *)malloc(sLength+1);
   memset(sCandidate.sKey, 0, sLength+1);
   printf("\nSort started");
   for(f0Count=0; f0Count<s0Limit; f0Count++)
   {	rSegmentSeek=f0Count*s0Limit*rLength;
   	lseek(InHandle, rSegmentSeek, SEEK_SET);
   	for(rSegmentTotal=0;
   	rSegmentTotal<s0Limit && (ReadLength=read(InHandle, rBuffer, rLength))>0;
      rSegmentTotal++)
   	{	memcpy(sCandidate.sKey, rBuffer+sOffset, sLength);
     		sCandidate.sNum=rSegmentTotal;
      	for(s2=s0, s1=s0+rSegmentTotal, ok=0; s2<s1 && !ok; s2++)
  	   		if(memcmp(s2->sKey, sCandidate.sKey, sLength)>0)
            {	for(s3=s1; s3>s2; s3--)
  	         	{	memcpy(s3->sKey, (s3-1)->sKey, sLength);
     	         	s3->sNum=(s3-1)->sNum;
        	      }
           		s2->sNum=sCandidate.sNum;
              	memcpy(s2->sKey, sCandidate.sKey, sLength);
            	ok=1;
	         }
     	   if(!ok)
        	{	s1->sNum=sCandidate.sNum;
        		memcpy(s1->sKey, sCandidate.sKey, sLength);
         }
  	   }
   	sprintf(scratch, "%s%s.sort.%d", SortPath, InFileName, f0Count);
      *(f0+f0Count)=strdup(scratch);
  		OutHandle = open(scratch, O_CREAT | O_TRUNC | O_RDWR,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
     	if(OutHandle==-1)
	   {	printf("\n%s open failure in NcpSort", scratch);
  			exit(1);
   	}
      s0Count=rSegmentTotal;
	   for(s1=s0; s1-s0<s0Count; s1++)
  		{	lseek(InHandle, rSegmentSeek+s1->sNum*rLength, SEEK_SET);
      	read(InHandle, rBuffer, rLength);
         write(OutHandle, rBuffer, rLength);
	   }
  		close(OutHandle);
//     	printf("\nSort step %d of %d completed", f0Count, f0Limit);
  	}
	close(InHandle);
   printf("\nSort finished, Merge started");
	for(s1=s0; s1-s0<s0Limit+1; s1++)
   	if(s1->sKey!=NULL)
     		free(s1->sKey);
   free(s0);
   free(rBuffer);
   m0Limit=s0Limit;
   m0Count=0;
   m0=(char **)malloc(m0Limit*sizeof(char *));
   memset(m0, 0, m0Limit*sizeof(char *));
   MergeStep=0;
   SystemCommand=scratch+6000;
   for(MERGE_LIMIT=1; MERGE_LIMIT*MERGE_LIMIT<f0Count; MERGE_LIMIT++)
   	;
   g0Limit=MERGE_LIMIT;
//   printf("\nMERGE_LIMIT=%d", MERGE_LIMIT);
   g0=(merge_rec *)malloc(g0Limit*sizeof(merge_rec));
  	memset(g0, 0, g0Limit*sizeof(merge_rec));
   pg0=(merge_rec **)malloc(g0Limit*sizeof(merge_rec *));
   memset(pg0, 0, g0Limit*sizeof(merge_rec *));
   for(g1=g0; g1-g0<g0Limit; g1++)
   {	g1->Buffer=(char *)malloc(rLength);
   	g1->Key=g1->Buffer+sOffset;
   }
   WriteCount=0;
   while(f0Count>1)
   {  for(f1=f0, m0Count=0; f1-f0<f0Count; f1+=MERGE_LIMIT, m0Count++)
	   {	g0Count=f0Count-(f1-f0);
      	if(g0Count>MERGE_LIMIT)
         	g0Count=MERGE_LIMIT;
	  		for(g1=g0, pg1=pg0; g1-g0<g0Count; g1++, pg1++)
	   	{	g1->Path=*(f1+(g1-g0));
         	g1->Handle=open(g1->Path, O_RDONLY);
   	   	if(g1->Handle==-1)
      	   {	printf("\n%s open failure in NcpSort", g1->Path);
         		exit(1);
	         }
   	      stat(g1->Path, &statbuf);
      	   g1->Size=statbuf.st_size;
            if(g1->Size>0)
         	{	g1->Length=read(g1->Handle, g1->Buffer, rLength);
            	*pg1=g1;
            }
            else
            {	close(g1->Handle);
            	g1--;
            	pg1--;
            	g0Count--;
            }
	      }
	      if(f0Count>g0Count)
		      sprintf(scratch, "%s%s.%d.merge.%d", SortPath, InFileName, MergeStep, m0Count);
      	else
        		sprintf(scratch, "%s.%d.merge.%d", InPath, MergeStep, m0Count);
	      *(m0+m0Count)=strdup(scratch);
  			OutHandle = open(scratch, O_CREAT | O_TRUNC | O_RDWR,
					S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
      	if(OutHandle==-1)
		   {	printf("\n%s open failure in NcpSort", scratch);
  				exit(1);
	   	}
         pg0Count=g0Count;
         pg1=pg0;
         while(pg0Count>0)
		   {	for(pg1=pg0+1, gCandidate=*pg0, i=0; pg1-pg0<pg0Count; pg1++)
	            if(memcmp(gCandidate->Key, (*pg1)->Key, sLength)>0)
   	         {	gCandidate=*pg1;
               	i=pg1-pg0;
               }
         	write(OutHandle, gCandidate->Buffer, rLength);
            if(gCandidate->Length<gCandidate->Size)
            	gCandidate->Length+=read(gCandidate->Handle, gCandidate->Buffer, rLength);
            else
            {	memcpy(pg0+i, pg0+i+1, (pg0Count-i-1)*sizeof(merge_rec *));
            	pg0Count--;
            }
      	}
	      close(OutHandle);
         for(g1=g0; g1-g0<g0Count; g1++)
   	   {	close(g1->Handle);
         	unlink(g1->Path);
         }
      }
      MergeStep++;
      for(f1=f0; f1-f0<f0Count; f1++)
      	free(*f1);
      memset(f0, 0, f0Count*sizeof(char *));
      memcpy(f0, m0, m0Count*sizeof(char *));
      memset(m0, 0, m0Count*sizeof(char *));
      f0Count=m0Count;
//      printf("\nMerge step %d completed, next step merges %d files", MergeStep-1, f0Count);
   }
   printf("\nMerge finished\n");
//      printf("\nMerge finished: %s + %s", *f1, *f2);
	stat(*f0, &statbuf);
   wTotal=statbuf.st_size / rLength;
   if(wTotal!=rTotal)
   {	printf("\nSort failed:  Read (%d) unequal to Write (%d)", rTotal, wTotal);
   	exit(1);
   }
//   sprintf(scratch, "%s.sorted", InPath);
   sprintf(scratch, "%s.sort.input", InPath);
   if(stat(InPath, &statbuf)==0)
   {	sprintf(SystemCommand, "mv %s %s", InPath, scratch);
//   	system(SystemCommand);
   }
   sprintf(SystemCommand, "mv %s %s", *f0, InPath);
   system(SystemCommand);
   return(0);
}

