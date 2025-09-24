/* ******************************************

	NcpSplitAll.c

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

#include "cnp01.h"

typedef struct split_s {
	char *Value;
   char *vExtension;
   int Count;
   char *Path;
   int Handle;
} split_rec;

split_rec *s0, *s1, *Found;
int s0Count, s0Limit;

char scratch[8192];

int TranslateForFileExtension(char *value);

void main(int argc, char *argv[])

{	int InHandle, RecordLength, Offset, Length, i,
		r, rcount, w, wcount, done, found;
   char *s, *t, *ReadBuffer, *InPath;

	printf("\n\n******* B E G I N   N C P S P L I T A L L *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"ncpsplitall.out A B C D\"");
      printf("\n  A = Record Length");
      printf("\n  B = Offset to split field");
      printf("\n  C = Length of split field");
		printf("\n  D = Input File (full path)");
      printf("\n  For example: \"ncpsplitall.out 650 647 3 /users/coupons/73770a.srt\"");
      printf("\n\n");
   	exit(1);
   }
   RecordLength=atoi(argv[1]);
   Offset=atoi(argv[2]);
   Length=atoi(argv[3]);
   InPath=strdup(argv[4]);
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
   ReadBuffer=(char *)malloc(RecordLength);
   s0Limit=50;
   s0Count=0;
   s0=(split_rec *)malloc(s0Limit*sizeof(split_rec));
   memset(s0, 0, 50*sizeof(split_rec));
	InHandle=open(InPath, O_RDONLY | O_LARGEFILE);
	while((r=read(InHandle, ReadBuffer, RecordLength))>0)
   {	rcount++;
   	for(s1=s0, Found=NULL; s1-s0<s0Count && Found==NULL; s1++)
  			if(memcmp(s1->Value, ReadBuffer+Offset, Length)==0)
     	   	Found=s1;
   	if(Found==NULL)
  	   {	if(s0Count==s0Limit)
     		{	s0Limit+=10;
				s0=(split_rec *)realloc(s0, s0Limit*sizeof(split_rec));
           	memset(s0+s0Count, 0, 50*sizeof(split_rec));
         }
  	      Found=s0+s0Count;
     	   Found->Value=(char *)malloc(Length+1);
        	memcpy(Found->Value, ReadBuffer+Offset, Length);
         *(Found->Value+Length)=0;
         strcpy(scratch, Found->Value);
         if(!TranslateForFileExtension(scratch))
         {	printf("\nNon-ascii character in split field for record %d\n\n", rcount);
	      	for(s1=s0; s1-s0<s0Count; s1++)
   	      {	close(s1->Handle);
      	   	s1->Handle=OpenFile(s1->Path, "", OUT, NULL);
         	   close(s1->Handle);
	         }
         	exit(1);
         }
         else
         	Found->vExtension=strdup(scratch);
         sprintf(scratch, "%s.%d.%d.%s", InPath, Offset, Length, Found->vExtension);
//         Found->Path=strdup(scratch);
			Found->Path=MakeUnixPath(scratch, "");
//         Found->Handle=OpenFile(Found->Path, "", OUT, NULL);
			Found->Handle = open(Found->Path, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
			if (Found->Handle == -1)
			{	printf("\r\nFile open error %s", Found->Path);
   			exit(1);
		   }
			else
				printf("\r\nFile open successful %s", Found->Path);
  	      s0Count++;
     	}
      Found->Count++;
      w=write(Found->Handle, ReadBuffer, RecordLength);
      if(w==-1)
      {	printf("\n%s write error", Found->Path);
      	for(s1=s0; s1-s0<s0Count; s1++)
         {	close(s1->Handle);
         	s1->Handle=OpenFile(s1->Path, "", OUT, NULL);
            close(s1->Handle);
         }
         exit(1);
      }
  	}
   close(InHandle);
   for(s1=s0; s1-s0<s0Count; s1++)
   	close(s1->Handle);
   if(r==-1)
    	for(s1=s0; s1-s0<s0Count; s1++)
      { 	s1->Handle=OpenFile(s1->Path, "", OUT, NULL);
         close(s1->Handle);
         exit(1);
      }
   free(ReadBuffer);
	return;
}

int TranslateForFileExtension(char *value)

{	char *t;
	int ret;

	for(t=value, ret=1; *t!=0; t++)
   {	switch(*t) {
   	case '*':
      	memmove(t+2, t+1, strlen(t));
         memcpy(t, "ak", 2);
         break;
      case '?':
      	memmove(t+2, t+1, strlen(t));
         memcpy(t, "qn", 2);
         break;
      case ' ':
      	memmove(t+2, t+1, strlen(t));
         memcpy(t, "__", 2);
         break;
      case '"':
      	memmove(t+2, t+1, strlen(t));
         memcpy(t, "dq", 2);
         break;
      case '\'':
      	memmove(t+2, t+1, strlen(t));
         memcpy(t, "sq", 2);
         break;
      default:
      	if(!isascii(*t))
         {	printf("\nInvalid split value in record %d");
         	ret=0;
         }
         break;
      }
   }
   return(ret);
}

