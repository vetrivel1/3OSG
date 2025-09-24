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

// first 200 of last 300 bytes is reserved for client-specific info
#define CLIENT_OFFSET 300
// last 100 bytes of output record are reserved for NCP'
#define NCP_OFFSET 100
#define F1_LENGTH 9			// rCount - input record counter
#define F2_LENGTH 6			// tCount - starts at 1 for each new account
#define F3_LENGTH 8			// pCount - account counter
#define RECORDTYPE_OFFSET 6	// appears 6 bytes from end of record
#define DDNUMBER_OFFSET 1		// appears in last byte of record

#define NCP10_LENGTH 2222

char scratch[8192];

int main(int argc, char *argv[])

{	char *s, *t, *In1Path, *In2Path, *In3Path, *Out1Path, *Out2Path,
		*In1Buffer, *In2Buffer, *In3Buffer, *ddOptionPath, *pSeqSave, *Client;
   int In1Handle, In2Handle, In3Handle, Out1Handle, Out2Handle, In1Length, In2Length,
		In3Length, In3Total, Out1Length, Out2Length;
   int i, r1Count;
   int In2Key, In2Counter, In2Next, IsTextFile, In2KeyCheck, offset,
   	In3Offset, In3Key;
   off64_t In2Offset;
   struct stat statbuf;

	printf("\n\n******* B E G I N   N C P C N T R 4 *******\n\n");
	if (argc<5)
   {	printf("\n\nUsage is \"%s A B C D\"\n\n", argv[0]);
      printf("\nA = Full path to preliminary NCP10 file");
      printf("\nB = Full path to original data converted to ASCII");
      printf("\nC = Original data record length");
      printf("\nD = CLient number");
      printf("\nE = Full path to option input (optional)");
      printf("\nF = 1-Output is a delimited text file (optional)");
      printf("\n\n");
      printf("\nE.g., \"%s /users/public/16862py.cntr.grp.container", argv[0]);
      printf("\n       /users/public/16862.4300.txt.new 1185 0547");
      printf("\n       /users/public/16862py.bil.option 1\"");
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   In2Path=strdup(argv[2]);
   In2Length=atoi(argv[3]);
   Client=strdup(argv[4]);
   printf("\nargc=%d", argc);
   if(argc==5)
   {	In3Path=NULL;
      IsTextFile=0;
   }
   else
   if(argc==6)
   {	if(*argv[5]=='/')
	   {	In3Path=strdup(argv[5]);
         IsTextFile=0;
      }
      else
	   {	In3Path=NULL;
      	IsTextFile=1;
	   }
   }
   else
   {	if(strcmp(argv[5], "NONE")!=0)
	   	In3Path=strdup(argv[5]);
   	else
      	In3Path=NULL;
      IsTextFile=1;
   }
   In1Length=2500;
   Out1Length=NCP10_LENGTH;
   Out2Length=NCP10_LENGTH+CLIENT_OFFSET;
   In1Buffer=(char *)malloc(In1Length);
   In2Buffer=(char *)malloc(In2Length);
   if(In3Path!=NULL)
   {	In3Length=In2Length;
   	In3Buffer=(char *)malloc(In3Length);
      In3Handle=OpenFile(In3Path, "", IN, NULL);
      stat(In3Path, &statbuf);
      if(statbuf.st_size%In3Length!=NULL)
      {	printf("\n\"%s\" file size (%d)", In3Path, statbuf.st_size);
      	printf("\n   not divisible by record length (%d)\n", In3Length);
         exit(1);
      }
      In3Total=statbuf.st_size/In3Length;
   }
   sprintf(scratch, "%s.new", In1Path);
   Out1Path=strdup(scratch);
   sprintf(scratch, "%s.carrier", In1Path);
   Out2Path=strdup(scratch);
   In1Handle=OpenFile(In1Path, "", IN, NULL);
   In2Handle=open(In2Path, O_RDONLY | O_LARGEFILE);
   if(In2Handle==-1)
   {	printf("\n\"%s\" open error", In2Path);
     	exit(1);
   }
   Out1Handle=open(Out1Path, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
		S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
   Out2Handle=open(Out2Path, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
		S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
   r1Count=0;
   pSeqSave=scratch+8100;
   while(read(In1Handle, In1Buffer, In1Length)>0)// && rCount<10)
   {	r1Count++;
   	sprintf(scratch, "%.9s", In1Buffer+Out1Length);
      In2Key=atoi(scratch)-1;
      In2Offset=(off64_t)In2Key*(off64_t)In2Length;
      lseek64(In2Handle, In2Offset, SEEK_SET);
      In2Counter=0;
      read(In2Handle, In2Buffer, In2Length);
   	sprintf(scratch, "%.8s", In1Buffer+9);
  	   In3Key=atoi(scratch)-1;
      sprintf(pSeqSave, "%.8s", In2Buffer+In2Length-NCP_OFFSET+F1_LENGTH+F2_LENGTH);
      memcpy(In1Buffer+9, pSeqSave, 8);
      write(Out1Handle, In1Buffer, Out1Length);
      write(Out2Handle, In1Buffer, Out1Length);
      sprintf(scratch, "%.*s", F1_LENGTH, In2Buffer+In2Length-NCP_OFFSET);
      In2KeyCheck=atoi(scratch);
      if(In2KeyCheck!=In2Key+1)
      {	printf("\nr1Count=%d, In2Key (%d) should equal In2KeyCheck-1 (%d-1)",
      		r1Count, In2Key, In2KeyCheck);
      	exit(1);
      }
		sprintf(scratch, "%.*s", F2_LENGTH, In2Buffer+In2Length-NCP_OFFSET+F1_LENGTH);
      In2Next=atoi(scratch);
      if(In2Next!=1)
      {	printf("\nr1Count=%d:  In2Next=%d, first record value should be 1", r1Count, In2Next);
      	exit(1);
      }
      write(Out2Handle, In2Buffer+In2Length-CLIENT_OFFSET, CLIENT_OFFSET);
      while(In2Counter<In2Next)
      {	// write 30 bytes of "preamble" data here
      	// 2-byte FileNumber, e.g., "01"
         // 5-byte RecordCode, e.g., "P", "S"
         // 8-byte SequenceNumber, e.g., "P" record sequence for the original
         //			data, repeated for all records associated with this account
         // 8-byte TranSeqNumber  ( This is In2Next value)
//         sprintf(pSeqSave, "%.8s", In2Buffer+In2Length-NCP_OFFSET+F1_LENGTH+F2_LENGTH);
         sprintf(scratch, "01 %.5s %.8s %08d    ", In2Buffer+In2Length-RECORDTYPE_OFFSET, In2Buffer+In2Length-NCP_OFFSET+F1_LENGTH+F2_LENGTH, In2Next);
         write(Out1Handle, scratch, strlen(scratch));
         if(IsTextFile)
         {	s=strstr(In2Buffer, "\r\n");
         	if(s!=NULL)
            {	s+=2;
            	write(Out1Handle, In2Buffer, s-In2Buffer);
            }
            else
            	write(Out1Handle, In2Buffer, In2Length);
         }
         else
      	{	write(Out1Handle, In2Buffer, In2Length);
	         write(Out1Handle, "\r\n", 2);
         }
      	In2Counter=In2Next;
	      read(In2Handle, In2Buffer, In2Length);
			sprintf(scratch, "%.*s", F2_LENGTH, In2Buffer+In2Length-NCP_OFFSET+F1_LENGTH);
      	In2Next=atoi(scratch);
      }
      if(In3Path!=NULL)
      {	// option data record, e.g., "A0310", is written here
//	   	sprintf(scratch, "%.8s", In1Buffer+9);
//   	   In3Key=atoi(scratch)-1;
         if(In3Key>=In3Total || In3Key<0)
         {	printf("\nIn3Key (%d) must be >=0 and < In3Total (%d)", In3Key, In3Total);
         	exit(1);
         }
      	In3Offset=In3Key*In3Length;
	      lseek(In3Handle, In3Offset, SEEK_SET);
         read(In3Handle, In3Buffer, In3Length);
         s=strstr(In3Buffer, "\r\n");
         if(s!=NULL)
	      {	In2Counter++;
	   	   sprintf(scratch, "%.3sA%s %s %08d    %.*s\r\n", In1Buffer, Client, pSeqSave, In2Counter++, s-In3Buffer, In3Buffer);
   		   write(Out1Handle, scratch, strlen(scratch));
         }
      }
      // NCP89 record is written here
      sprintf(scratch, "%.6s89%.53s%09d\r\n", In1Buffer, In1Buffer+8, In2Counter+1);
      write(Out1Handle, scratch, strlen(scratch));
   }
   close(In1Handle);
   close(In2Handle);
   if(In3Path!=NULL)
   	close(In3Handle);
   close(Out1Handle);
   close(Out2Handle);
   return(0);
}

