/* ******************************************

	mbcnvt0

   LPS "P" record at offset 745 for 30 bytes is variable by client.
   This is handled below with custom logic per client number.
   The initial conversion of this 30 bytes is as text, so custom
   logic can be restricted to PACKED data within these 30 bytes.

   We can expand or contract the size of this client-specific data
   as needed.
   1.  Change "mbpri.dd" appropriately.
   2.  Make changes as needed to custom client logic below for the "P" record.
   3.  Change "mb1100.cbl" and "mb2000.cbl" copy books.
   4.  Add logic to "setmb2000.cbl" to make sure the data reaches the
       mb2000 record.

11/12/14 - increased to 1500 bytes - needed more space - leaving copybook^M
name the same (mb1100.cbl)...cj^M


****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include "/users/temp/cnp01.h"

char *ddPriPath="/users/eric/mbpri.dd";
char *ddSecPath="/users/eric/mbsec.dd";
char *ddSecDisbPath="/users/eric/mbsecdisb.dd";
//char *ddSecSPSPath="/users/eric/mbsecsps.dd";
char *ddSecSPSPath="/users/eric/mbsecspscfpb.dd";
char *ddSec502Path="/users/eric/mbsec502.dd";	// IP 2131
char *ddXPath="/users/programs/container/mblps/mbx.dd";
char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, OutHandle, InLength, OutLength, WriteLength, ClientNumber,
		FileClientNumber, rCount, IsDisbType, i, e2aControl, LoanIsPacked,
      pCount, sCount, apCount, asCount, Out2Handle, Out2Length;
	char *InPath, *OutPath, *Out2Path, *InBuffer, *OutBuffer, *Out2Buffer, *Count;
	char RecordType, ProcessDate[7], Client[4];
   dd_rec *ddPri, *ddSec, *ddSecDisb, *ddSecSPS, *ddX;
   field_rec *f0, *f1;
   int f0Count;

	printf("\n\n******* B E G I N   M B C N V T 0 *******\n\n");
	if (argc<3)
   {	printf("Usage is \"%s A B\"", argv[0]);
      printf("\n  A = Client# (4 digit)");
      printf("\n  B = Input (full path)");
      printf("\n  For example: \"%s 0140 /users/public/36060.dat\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   ClientNumber=atoi(argv[1]);
   InPath=strdup(argv[2]);
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
   if(ClientNumber==346)
   {	sprintf(scratch, "%s.x", InPath);
	   Out2Path=strdup(scratch);
		Out2Handle = open(Out2Path, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
		if (Out2Handle == -1)
		{	printf("\r\nFile open error %s", Out2Path);
   		exit(1);
	   }
		else
			printf("\r\nFile open successful %s", Out2Path);
   }
	InLength=4000;
   OutLength=4100;
   Out2Length=4000;
        // increased record length to 1500.cj.11/17/14
	WriteLength=1500;
	InBuffer=(char *)malloc(InLength);
	memset(InBuffer, 0, InLength);
   OutBuffer=(char *)malloc(OutLength);
   memset(OutBuffer, 0, OutLength);
   Out2Buffer=(char *)malloc(Out2Length);
   memset(Out2Buffer, 0, Out2Length);
	rCount=0;
	InHandle=open(InPath, O_RDONLY | O_LARGEFILE);
   ddPri=ddLoadFromFile(ddPriPath);
   ddSec=ddLoadFromFile(ddSecPath);
   ddSecDisb=ddLoadFromFile(ddSecDisbPath);
   ddX=ddLoadFromFile(ddXPath);
   if(ClientNumber==277)
	   ddSecSPS=ddLoadFromFile(ddSecSPSPath);
   else
   if(ClientNumber==588)
	   ddSecSPS=ddLoadFromFile(ddSec502Path);
   FileClientNumber=-1;
   LoanIsPacked=-1;
   pCount=0;
   sCount=0;
	while(read(InHandle, InBuffer, InLength)>0)
	{	rCount++;
    	ebc2asc(&RecordType, InBuffer+11, 1, 0);
      if(RecordType=='D')
      {	memset(OutBuffer, ' ', OutLength);
      	ebc2asc(OutBuffer, InBuffer, InLength, 0);
         if(ClientNumber!=310)
				write(OutHandle, OutBuffer, WriteLength);
      }
      else
		if(RecordType=='P')
		{	// check client number
      	if(FileClientNumber==-1)
	      {	// verify client number
      	   memset(Client, 0, sizeof(Client));
         	ebc2asc(Client, InBuffer, 3, 0);
	         *(Client+3)=0;
   	      FileClientNumber=atoi(Client);
      	   if(ClientNumber!=FileClientNumber)
				{	printf("\n\nScript client = %d", ClientNumber);
					printf("\nData client = %d", FileClientNumber);
					printf("\nClient numbers must match!\n\n");
					exit(1);
				}
            LoanIsPacked=FieldIsPacked(InBuffer+4, 7);
			}
      	memset(OutBuffer, ' ', OutLength);
         f0=ddPri->Field;
         f0Count=ddPri->FieldCount;
			for(f1=f0; f1-f0<f0Count; f1++)
         {	if(f1->DataType==0)
         		e2aControl=0;
         	else
            if(f1->DataType==1)
            	e2aControl=2;
            else
            	e2aControl=1;
				ebc2asc(OutBuffer+f1->Offset, InBuffer+f1->Offset, f1->Length, e2aControl);
         }
                        // increased record length to 1500.cj.11/17/14
			//memcpy(OutBuffer+WriteLength-100, ProcessDate, 6);
			memcpy(OutBuffer+1000, ProcessDate, 6);
         ebc2asc(OutBuffer+1033, InBuffer+2088, 1, 0);
         ebc2asc(OutBuffer+1034, InBuffer+2148, 1, 0);
         if(ClientNumber==140)
         {	memcpy(OutBuffer+749, InBuffer+749, 7);
//         	memcpy(OutBuffer+761, InBuffer+761, 12);
         }
//         if(ClientNumber==628)
         {	ebc2asc(OutBuffer+1030, InBuffer+2142, 1, 0);
	         // arm-rate-pi-not-avail-ind
   	      memmove(OutBuffer+1031, OutBuffer+2144, 1);
      	   // no-change-sched-ind
         	memmove(OutBuffer+1032, OutBuffer+2179, 1);
      	   // Used for EBP - email & due date
         	ebc2asc(OutBuffer+1100, InBuffer+2296, 66, 0);
                memcpy(OutBuffer+1166, InBuffer+1970, 2);
           	ebc2asc(OutBuffer+1168, InBuffer+1972, 2, 0);
           	ebc2asc(OutBuffer+1170, InBuffer+1974, 2, 0);
           // Co-Borrwer added AL 3 clients as needed
           // must alter setmb2000.cbl also
            if(ClientNumber==102)
               ebc2asc(OutBuffer+1193, InBuffer+2455, 66, 0);
            if(ClientNumber==415)
               ebc2asc(OutBuffer+1193, InBuffer+2455, 66, 0);
            if(ClientNumber==628)
               ebc2asc(OutBuffer+1193, InBuffer+2455, 66, 0);
          // Used for e-statements
            ebc2asc(OutBuffer+1172, InBuffer+2453, 1, 0); 	
          // Draft Indicator
          //  ebc2asc(OutBuffer+1173, InBuffer+1969, 1, 0);
          
          // EBPP indicator
             ebc2asc(OutBuffer+1174, InBuffer+2411, 2, 0);

          // FORECLOSURE SALES DATE
             memcpy(OutBuffer+1176, InBuffer+2102, 2);
             ebc2asc(OutBuffer+1178, InBuffer+2104, 4, 0);

          // ACCELERATED PACKED DOLLAR AMOUNT 9(9)V99 COMP3.
             memcpy(OutBuffer+1182, InBuffer+2541, 6);

          // ACCELERATED PACKED INT DUE CAL AMOUNT 9(9)V99 COMP3.
             memcpy(OutBuffer+1259, InBuffer+2993, 6);
                      
          // ACCELERATED REASON CODE
             ebc2asc(OutBuffer+1188, InBuffer+2547, 2, 0);

          // DELQ DAYS 9(5) COMP3.
             memcpy(OutBuffer+1190, InBuffer+1982, 3);
                      
          // LANGUAGE CODE FOR FLEX3 DMI
             ebc2asc(OutBuffer+1265, InBuffer+782, 2, 0);
                                         
                                   

                     }
         if(!LoanIsPacked)
         	ebc2asc(OutBuffer+4, InBuffer+4, 7, 0);
         //
         // done to keep output record at 1100 bytes
         // until after BOA
         //
         // increased record length to 1500.cj.11/17/14
         //memmove(OutBuffer+WriteLength-94, OutBuffer+1558, 12);
         memmove(OutBuffer+1006, OutBuffer+1558, 12);
         // mb1100-off-schd-pend-date-1, ir-1, pi-1 fields
         memmove(OutBuffer+940, OutBuffer+1574, 17);
//         memmove(OutBuffer+WriteLength-82, OutBuffer+1574, 15);
			sprintf(scratch, "%09d", rCount);
         // increased record length to 1500.cj.11/17/14
         //memcpy(OutBuffer+WriteLength-9, scratch, strlen(scratch));
         if(ClientNumber==310)
         	ebc2asc(OutBuffer+1091, InBuffer+3700, 9, 0);
         else
	         memcpy(OutBuffer+1091, scratch, strlen(scratch));
			write(OutHandle, OutBuffer, WriteLength);
         pCount++;
		}
      else
		if(RecordType=='S')
      {	memset(OutBuffer, ' ', OutLength);
      	i=ConvertPackedToInt((unsigned char *)InBuffer+36, 1);
      	if(i==3)
				IsDisbType=1;
         else
         	IsDisbType=0;
         if(ClientNumber==277 || ClientNumber==588)
         {	ebc2asc(scratch, InBuffer+12, 3, 0);
           	if(memcmp(scratch, "350", 3)==0)
            {	f0=ddSecSPS->Field;
               f0Count=ddSecSPS->FieldCount;
            }
            else
            if(IsDisbType)
	         {	f0=ddSecDisb->Field;
   	      	f0Count=ddSecDisb->FieldCount;
      	   }
            else
	         {	f0=ddSec->Field;
   	        	f0Count=ddSec->FieldCount;
      	   }
         }
         else
         if(IsDisbType)
	      {	f0=ddSecDisb->Field;
   	    	f0Count=ddSecDisb->FieldCount;
      	}
         else
	      {	f0=ddSec->Field;
   	     	f0Count=ddSec->FieldCount;
         }
			for(f1=f0; f1-f0<f0Count; f1++)
         {	if(f1->DataType==0)
         		e2aControl=0;
         	else
            if(f1->DataType==1)
            	e2aControl=2;
            else
            	e2aControl=1;
				ebc2asc(OutBuffer+f1->Offset, InBuffer+f1->Offset, f1->Length, e2aControl);
         }
         if(!LoanIsPacked)
         	ebc2asc(OutBuffer+4, InBuffer+4, 7, 0);
         // this converts the "HAMP Incentive" code for SPS and places
         // it within the first 200 bytes of the record - Sept. 14, 2011
         // there was one byte of obvious filler available for this.
         // the next extra item we need to carry for SPS may require
         // increasing the size of this secondary, perhaps to 300 rather
         // than 200
         if(ClientNumber==277)
         	ebc2asc(OutBuffer+499, InBuffer+1000, 1, 0);
			write(OutHandle, OutBuffer, WriteLength);
         sCount++;
		}
      else
      if(RecordType=='X' && ClientNumber==346)
      {	memset(Out2Buffer, ' ', Out2Length);
         f0=ddX->Field;
         f0Count=ddX->FieldCount;
			for(f1=f0; f1-f0<f0Count; f1++)
         {	if(f1->DataType==0)
         		e2aControl=0;
         	else
            if(f1->DataType==1)
            	e2aControl=2;
            else
            	e2aControl=1;
				ebc2asc(Out2Buffer+f1->Offset, InBuffer+f1->Offset, f1->Length, e2aControl);
         }
         //
         // move the packed account number to the output record
         // shouldn't be needed, but "mbx.dd" defines account number as text
         // as of 10/9/14
         //
         memcpy(Out2Buffer+4, InBuffer+4, 7);
         write(Out2Handle, Out2Buffer, Out2Length);
      }
      else
      if(RecordType=='W')
      	sCount++;
		else
		{  if(RecordType!='V' && RecordType!='F' && RecordType!='U')
      		printf("\nRecordType=%c, rCount=%ld", RecordType, rCount);
			if (RecordType=='A')
			{	Count=scratch+8000;
         	memcpy(ProcessDate, InBuffer+15, 2);
				ebc2asc(ProcessDate+2, InBuffer+17, 4, 0);
            sprintf(scratch, "%.*s", 9, InBuffer+21);
            ebc2asc(Count, scratch, 9, 0);
            apCount=atoi(Count);
            sprintf(scratch, "%.*s", 9, InBuffer+30);
            ebc2asc(Count, scratch, 9, 0);
            asCount=atoi(Count);
			}
		}
		if(rCount%1000==0)
			printf("\nrCount=%ld", rCount);
	}
	close(InHandle);
	close(OutHandle);
   if(ClientNumber==346)
	   close(Out2Handle);
   if(apCount!=pCount)
   {	printf("\n\"P\" record count (%d) doesn't match expected total (%d)\n", pCount, apCount);
   	exit(1);
   }
   //
   // client number 310 is excluded from the following check
   // because 745 trans have been removed
   //
   if(asCount!=sCount && ClientNumber!=310)
   {	printf("\n\"S\"+\"W\" record count (%d) doesn't match expected total (%d)\n", sCount, asCount);
   	exit(1);
   }
	free(InBuffer);
   free(OutBuffer);
   free(Out2Buffer);
	printf("\nrCount=%ld", rCount);
	sprintf(scratch, "%s.total", InPath);
   OutHandle=OpenFile(scratch, "", OUT, NULL);
   memset(scratch, ' ', sizeof(scratch));
   while(pCount>sizeof(scratch))
   {	write(OutHandle, scratch, sizeof(scratch));
   	pCount-=sizeof(scratch);
   }
   write(OutHandle, scratch, pCount);
   close(OutHandle);
	return(0);
}

