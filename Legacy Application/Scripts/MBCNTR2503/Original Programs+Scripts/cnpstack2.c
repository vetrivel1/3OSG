/* ******************************************

	cnpstack2.c

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

char scratch[8192];

typedef struct stack_s {
	int Handle;
   int FirstRecord;
   int StackNumber;
   int ReadCount;
   int ReadLimit;
} stack_rec;

typedef struct filestack_s {
	char *InPath;
   char *InName;
   int rTotal, rLimit, rCount;
   char *OutPath;
   int OutHandle;
   stack_rec *s0;
   int s0Count;
   int wCount, aCount;
   int OkToStack;
} filestack_rec;

filestack_rec *f0;
int f0Count;

int main(int argc, char *argv[])

{	int sOffset, sLength, rLength, adjust, PctDone, PctDoneSave, i,
		dCount, FirstPath, StackSize, GlobalReadCount, GlobalWriteCount,
      GlobalTotal;
	stack_rec *s1;
   filestack_rec *f1;
   char *rBuffer, *ExtensionToReplace, *ExtensionsToExclude, *s;
   char **e0, **e1;
   int e0Count;
   struct stat statbuf;
   parse_rec *p0, *p1;
   int p0Count;

	printf("\n\n******* B E G I N   C N P S T A C K 2 *******\n\n");
	if (argc<8)
   {	printf("\n\nUsage is \"%s A B C D E F G", argv[0]);
      printf("\n  A - Input Record Length");
      printf("\n  B - Stack Size");
      printf("\n  C - Stack Offset");
      printf("\n  D - Stack Length");
      printf("\n  E - Extension to replace with \"stk\" extension");
      printf("\n  F - Excluded extension list, comma-delimited, no spaces");
   	printf("\n  G - Input File Mask (full path)");
      printf("\n\nFor example:");
      printf("\n   %s 750 3 706 6 out sam,stk /users/public/out/ti0140*3.out*", argv[0]);
      printf("\n\n");
      exit(1);
   }
   rLength=atoi(argv[1]);
   if(rLength<=0)
   {	printf("\nRecord length (%d) must be > 0", rLength);
   	exit(1);
   }
	rBuffer=(char *)malloc(rLength);
   StackSize=atoi(argv[2]);
   if(StackSize<2 || StackSize>6)
   {	printf("\nInvalid StackSize (%d)", StackSize);
   	exit(1);
   }
	sOffset=atoi(argv[3]);
   sLength=atoi(argv[4]);
   if(sOffset<0)
   {	printf("\nStack Offset (%d) must be >= 0", sOffset);
   	exit(1);
   }
   if(sLength<4)
   {	printf("\nStack length (%d) must be >= 4", sLength);
   	exit(1);
   }
   if(sOffset+sLength>rLength)
   {	printf("\nStack offset+length (%d+%d) must be <= Record length (%d)",
   		sOffset, sLength, rLength);
   	exit(1);
   }
   ExtensionToReplace=strdup(argv[5]);
   ExtensionsToExclude=strdup(argv[6]);
   p0=ParseExpression(ExtensionsToExclude, &p0Count, ',', 0, 0, 1);
   e0Count=p0Count;
   e0=(char **)malloc(e0Count*sizeof(char *));
   memset(e0, 0, e0Count*sizeof(char *));
   for(e1=e0, p1=p0; e1-e0<e0Count; e1++, p1++)
   	*e1=strdup(p1->String);
   FirstPath=7;
   f0Count=argc-FirstPath;
   f0=(filestack_rec *)malloc(f0Count*sizeof(filestack_rec));
   memset(f0, 0, f0Count*sizeof(filestack_rec));
   for(f1=f0, i=FirstPath; f1-f0<f0Count; f1++, i++)
   {	f1->InPath=strdup(argv[i]);
   	for(s=f1->InPath+strlen(f1->InPath)-1; s>=f1->InPath && *s!='/'; s--)
      	;
      f1->InName=s+1;
   	for(e1=e0, f1->OkToStack=1; e1-e0<e0Count && f1->OkToStack; e1++)
      	if(strstr(f1->InName, *e1)!=NULL)
         	f1->OkToStack=0;
   	s=strstr(f1->InName, ExtensionToReplace);
      if(f1->OkToStack)
      {	if(s!=NULL)
      		sprintf(scratch, "%.*sstk%s", s-f1->InPath, f1->InPath, s+strlen(ExtensionToReplace));
	      else
   	   	sprintf(scratch, "%s.stk", f1->InPath);
      	f1->OutPath=strdup(scratch);
		   stat(f1->InPath, &statbuf);
         if(statbuf.st_size%rLength)
         	f1->OkToStack=0;
         else
   		{	f1->rTotal=statbuf.st_size / rLength;
		   	f1->rLimit=f1->rTotal/StackSize;
		   	if(adjust=f1->rTotal%StackSize)
   				f1->rLimit++;
      		f1->s0Count=StackSize;
			   f1->s0=(stack_rec *)malloc(f1->s0Count*sizeof(stack_rec));
   			memset(f1->s0, 0, f1->s0Count*sizeof(stack_rec));
	   		for(s1=f1->s0; s1-f1->s0<f1->s0Count; s1++)
	   		{	if(s1==f1->s0)
            		s1->FirstRecord=0;
            	else
               	s1->FirstRecord=(s1-1)->FirstRecord+(s1-1)->ReadLimit;
//               s1->FirstRecord=(s1-f1->s0)*f1->rLimit;
   				s1->StackNumber=s1->FirstRecord+1;
		   		s1->Handle=OpenFile(f1->InPath, "", IN, NULL);
   		   	lseek(s1->Handle, s1->FirstRecord*rLength, SEEK_SET);
               if(adjust==0)
               	s1->ReadLimit=f1->rLimit;
               else
	   	   	if(s1-f1->s0<adjust)
		   	   	s1->ReadLimit=f1->rLimit;
	   	   	else
   	   			s1->ReadLimit=f1->rLimit-1;
			   }
         }
   	}
   }
   GlobalReadCount=0;
   GlobalWriteCount=0;
   for(f1=f0, GlobalTotal=0; f1-f0<f0Count; f1++)
   	GlobalTotal+=f1->rTotal;
   PctDone=PctDoneSave=0;
  	dCount=0;
   for(f1=f0; f1-f0<f0Count; f1++)
   {	if(f1->OkToStack)
   	{	f1->OutHandle=OpenFile(f1->OutPath, "", OUT, NULL);
		   for(f1->rCount=0, f1->wCount=0, f1->aCount=0; f1->rCount<f1->rLimit; f1->rCount++)
   		{	for(s1=f1->s0; s1-f1->s0<f1->s0Count; s1++)
	   		{	if(s1->ReadCount<s1->ReadLimit)
   	   		{	read(s1->Handle, rBuffer, rLength);
            		GlobalReadCount++;
      	  			f1->aCount++;
		            dCount++;
   		   		sprintf(scratch, "%0*d", sLength, s1->StackNumber);
	   	      	memcpy(rBuffer+sOffset, scratch, sLength);
   	   			write(f1->OutHandle, rBuffer, rLength);
            	   GlobalWriteCount++;
            		f1->wCount++;
		            s1->StackNumber++;
   		         s1->ReadCount++;
      		   }
	      	}
	   	   PctDone=(int)((double)GlobalReadCount*100.0)/(double)GlobalTotal;
		      if(PctDone!=PctDoneSave || dCount>=5000)
   		   {	printf("\n%d %% done, %d records read (%s)", PctDone, GlobalReadCount, f1->InName);
      			PctDoneSave=PctDone;
         		dCount=0;
		      }
   		}
	   	for(s1=f1->s0; s1-f1->s0<f1->s0Count; s1++)
		   	close(s1->Handle);
	      close(f1->OutHandle);
   	}
   }
   printf("\n\n");
   printf("\n     Reads:  %7d", GlobalReadCount);
   for(f1=f0; f1-f0<f0Count; f1++)
   	if(!f1->OkToStack)
      	printf("\nFile %s not stacked", f1->InName);
   for(f1=f0; f1-f0<f0Count; f1++)
   {	if(f1->OkToStack)
   	{	printf("\nFile %s", f1->InName);
		   for(s1=f1->s0; s1-f1->s0<f1->s0Count; s1++)
   			printf("\n       In[%d]:  %7d", s1-f1->s0+1, s1->ReadCount);
      	printf("\nFile %s", f1->OutPath);
	      printf("\n       Out:  %7d", f1->wCount);
      }
   }
   printf("\n    Writes:  %7d", GlobalWriteCount);
   printf("\n\n");
	return(0);
}

