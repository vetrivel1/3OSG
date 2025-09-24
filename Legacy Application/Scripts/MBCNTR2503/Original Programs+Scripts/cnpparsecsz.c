/* ******************************************

	CnpParseCSZ.c

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

#define STANDARD_ADDRESS_COUNT 7
#define EARLY_ADDRESS_COUNT 3
#define ADD1 0
#define ADD2 1
#define ADD3 2
#define CITY 3
#define STATE 4
#define ZIP5 5
#define ZIP4 6

char *StandardAddress[] = {
	"Address1",
   "Address2",
   "Address3",
   "City",
   "State",
   "Zip5",
   "Zip4",
   NULL
};

char scratch[8192];
char Spaces[1024];

int main(int argc, char *argv[])

{  int InHandle, OutHandle, RecordLength, CSZOffset, CSZLength, TryCount,
		r, i, PctDone, PctDoneSave, ReadCount, FileTotal, MaxReads, slen, ResultOffset;
   char *InPath, *OutPath, *InRecord, *s, *t;
   struct stat statbuf;
   address_rec ParseCsz;

	printf("\n\n******* B E G I N   C N P P A R S E C S Z *******\n\n");
	if (argc<7)
   {	printf("\n\nUsage is \"CnpParseCSZ.out A B C D E F\"\n\n");
      printf("\n  A = Input File (full path)");
      printf("\n      Output file will be \"Input.csz\"");
      printf("\n  B = Input Record Length");
      printf("\n  C = Offset to CSZ field");
      printf("\n  D = Length of CSZ field");
      printf("\n  E = Maximum number of records to process");
      printf("\n  F = Offset for result code (Y/N) of parsing logic");
      printf("\nFor example:");
      printf("\n   \"> CnpParseCSZ.out /users/public/66666p.asc 1000 853 35 100 888\"");
      printf("\n\n");
   	exit(1);
   }

   InPath=strdup(argv[1]);
   sprintf(scratch, "%s.csz", InPath);
   OutPath=strdup(scratch);
   RecordLength=atoi(argv[2]);
   if(RecordLength<30)
   {	printf("\n");
   	printf("\nRecord length (%d) must be >= 30", RecordLength);
      printf("\n");
   	exit(1);
   }
	CSZOffset=atoi(argv[3]);
   CSZLength=atoi(argv[4]);
   MaxReads=atoi(argv[5]);
   ResultOffset=atoi(argv[6]);
   if(CSZOffset<0)
   {	printf("\nCSZOffset (%d) must be >= 0", CSZOffset);
   	exit(1);
   }
   else
   if(CSZLength<20)
   {	printf("\nCSZLength (%d) must be >= 20", CSZLength);
   	exit(1);
   }
   else
   if(CSZOffset+CSZLength>RecordLength)
   {	printf("\nCSZOffset+CSZLength (%d+%d) must be <= RecordLength (%d)", CSZOffset, CSZLength, RecordLength);
   	exit(1);
   }
   if(ResultOffset<0 || ResultOffset>=RecordLength)
   {	printf("\nResult offset (%d) must be >=0 and < RecordLength (%d)", ResultOffset, RecordLength);
   	exit(1);
   }
   InHandle=OpenFile(InPath, "", IN, NULL);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InRecord=(char *)malloc(RecordLength);
   PctDone=0;
   PctDoneSave=0;
   ReadCount=0;
   _stat(InPath, &statbuf);
   FileTotal=statbuf.st_size/RecordLength;
   memset(Spaces, ' ', sizeof(Spaces));
   while((r=read(InHandle, InRecord, RecordLength))>0 && (!MaxReads || ReadCount<MaxReads))
   {	ReadCount++;
   	s=InRecord+CSZOffset;
      TryCount=0;
      while(s>=InRecord && memcmp(s, Spaces, CSZLength)==0 && TryCount<4)
      	s-=CSZLength;
      memcpy(ParseCsz.AddressScratch, s, CSZLength);
   	strcpy(ParseCsz.AddressScratch+CSZLength, ";");
   	if(ExtractAddressData(&ParseCsz))
      {	memset(s, ' ', CSZLength);
      	s=InRecord+CSZOffset+CSZLength-4;
         memcpy(s, ParseCsz.Zip4, strlen(ParseCsz.Zip4));
         s-=5;
         memcpy(s, ParseCsz.Zip5, strlen(ParseCsz.Zip5));
         s-=2;
         memcpy(s, ParseCsz.State, strlen(ParseCsz.State));
         s=InRecord+CSZOffset;
         if(strlen(ParseCsz.City)<CSZLength-11)
	         memcpy(s, ParseCsz.City, strlen(ParseCsz.City));
         else
         	memcpy(s, ParseCsz.City, CSZLength-11);
         *(InRecord+ResultOffset)='Y';
      }
      else
         *(InRecord+ResultOffset)='N';
  		write(OutHandle, InRecord, RecordLength);
		PctDone=(int)((double)ReadCount*100.0/(double)FileTotal);
      if(PctDone!=PctDoneSave)
  	   {	printf("\n%d %% done", PctDone);
      	PctDoneSave=PctDone;
     	}
   }
   close(InHandle);
   close(OutHandle);
   return(0);
}

