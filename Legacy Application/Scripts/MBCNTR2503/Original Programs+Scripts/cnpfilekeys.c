/* ******************************************

	cnpfilekeys.c

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

int main(int argc, char *argv[])

{	int In1Handle, In2Handle, r1Length, r2Length, a1Offset, a2Offset,
		AccountLength, i, OutHandle, r1, r2, w, wCount, r2Count, PctDone,
		r2KeyOffset, r2KeyLength, r2KeyCountLength, PctDoneSave, rTotal,
      r2Key, r2KeyCount, ret, KeyFirst;
   char *s, *t, *r1Buffer, *r2Buffer, *In1Path, *In2Path, *OutPath;
   struct stat statbuf;

	printf("\n\n******* B E G I N   C N F I L E K E Y S *******\n\n");
	if (argc<11)
   {	printf("\nUsage is \"cnpfilekeys.out A B C D E F G H I J\"");
      printf("\n  A = Record 1 Length");
      printf("\n  B = Record 2 Length");
      printf("\n  C = Record 1 Account# Offset");
      printf("\n  D = Record 2 Account# Offset");
      printf("\n  E = Account# Length");
      printf("\n  F = Key Offset");
      printf("\n  G = Key Length");
      printf("\n  H = Count Length");
      printf("\n  I = Input File 1 (full path)");
		printf("\n  J = Input File 2 (full path)");
      printf("\n  K = C-Count before Key, K or BLANK-Key before Count");
      printf("\n  For example: \"cnpfilekeys.out 750 100 10 1 7 688 7 3 /users/public/out/ti0140.out /users/public/out/ti0140h.sec.merged");
      printf("\n\n");
   	exit(1);
   }
   r1Length=atoi(argv[1]);
   r2Length=atoi(argv[2]);
   a1Offset=atoi(argv[3]);
   a2Offset=atoi(argv[4]);
   AccountLength=atoi(argv[5]);
   r2KeyOffset=atoi(argv[6]);
   r2KeyLength=atoi(argv[7]);
   r2KeyCountLength=atoi(argv[8]);
   In1Path=strdup(argv[9]);
   In2Path=strdup(argv[10]);
   if(argc>11 && strcmp(argv[11], "C")==0)
   	KeyFirst=0;
   else
   	KeyFirst=1;
   sprintf(scratch, "%s.keyed", In1Path);
   OutPath=strdup(scratch);
   if(r1Length<=0)
   {	printf("\nRecordLength (%d) must be > 0", r1Length);
   	exit(1);
   }
   if(r2Length<=0)
   {	printf("\nRecordLength (%d) must be > 0", r2Length);
   	exit(1);
   }
   if(a1Offset<0)
   {	printf("\nAccount# offset (%d) must be >= 0", a1Offset);
   	exit(1);
   }
   if(a2Offset<0)
   {	printf("\nAccount# offset (%d) must be >= 0", a2Offset);
   	exit(1);
   }
   if(AccountLength<1)
   {	printf("\nAccount# length (%d) must be > 0", AccountLength);
   	exit(1);
   }
   if(a1Offset+AccountLength>r1Length)
   {	printf("\nAccount offset+length (%d+%d) must be <= Record length (%d)",
   		a1Offset, AccountLength, r1Length);
   	exit(1);
   }
   if(a2Offset+AccountLength>r2Length)
   {	printf("\nAccount offset+length (%d+%d) must be <= Record length (%d)",
   		a2Offset, AccountLength, r2Length);
   	exit(1);
   }
   if(r2KeyOffset+r2KeyLength+r2KeyCountLength>r1Length)
   {	printf("\nAccount offset+length (%d+%d+%d) must be <= Record length (%d)",
   		r2KeyOffset, r2KeyLength, r2KeyCountLength, r1Length);
   	exit(1);
   }
   r1Buffer=(char *)malloc(r1Length);
   r2Buffer=(char *)malloc(r2Length);
   w=wCount=r2Count=0;
   r2Key=-1;
   r2KeyCount=0;
   PctDone=PctDoneSave=0;
	_stat(In1Path, &statbuf);
   rTotal=statbuf.st_size / r1Length;
	OutHandle=open(OutPath, O_CREAT | O_RDWR | O_TRUNC | O_LARGEFILE,
		S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
   In1Handle=open(In1Path, O_RDONLY | O_LARGEFILE);
   In2Handle=open(In2Path, O_RDONLY | O_LARGEFILE);
   memset(scratch, 0, sizeof(scratch));
	r1=read(In1Handle, r1Buffer, r1Length);
   r2=read(In2Handle, r2Buffer, r2Length);
   if(r2>0)
   	r2Count++;
   while(r1>0 && r2>=0)
   {
      if(r2==0)
   	{	if(r2Key==-1)
	      	memset(r1Buffer+r2KeyOffset, '0', r2KeyLength+r2KeyCountLength);
      	else
         {	if(KeyFirst)
	         	sprintf(scratch, "%0.*d%0.*d", r2KeyLength, r2Key, r2KeyCountLength, r2KeyCount);
         	else
            	sprintf(scratch, "%0.*d%0.*d", r2KeyCountLength, r2KeyCount, r2KeyLength, r2Key);
           	memcpy(r1Buffer+r2KeyOffset, scratch, strlen(scratch));
         }
      	w=write(OutHandle, r1Buffer, r1Length);
         wCount++;
         r1=read(In1Handle, r1Buffer, r1Length);
         r2Key=-1;
         r2KeyCount=0;
      }
      else
      {	ret=memcmp(r1Buffer+a1Offset, r2Buffer+a2Offset, AccountLength);
	      if(ret>0)
   	   {	r2=read(In2Handle, r2Buffer, r2Length);
         	if(r2>0)
	         	r2Count++;
         }
      	else
         if(ret<0)
         {	if(r2Key==-1)
	         	memset(r1Buffer+r2KeyOffset, '0', r2KeyLength+r2KeyCountLength);
         	else
	         {	if(KeyFirst)
		         	sprintf(scratch, "%0.*d%0.*d", r2KeyLength, r2Key, r2KeyCountLength, r2KeyCount);
      	   	else
         	   	sprintf(scratch, "%0.*d%0.*d", r2KeyCountLength, r2KeyCount, r2KeyLength, r2Key);
   	        	memcpy(r1Buffer+r2KeyOffset, scratch, strlen(scratch));
      	   }
      		w=write(OutHandle, r1Buffer, r1Length);
            wCount++;
	         r1=read(In1Handle, r1Buffer, r1Length);
            r2Key=-1;
            r2KeyCount=0;
         }
         else
         {	if(r2Key==-1)
         		r2Key=r2Count;
           	r2KeyCount++;
            r2=read(In2Handle, r2Buffer, r2Length);
            if(r2>0)
	            r2Count++;
         }
      }
      if(w<0)
      {	printf("\nError writing merged file (%s)", OutPath);
      	close(OutHandle);
         unlink(OutPath);
         exit(1);
      }
      PctDone=(int)(100.0*(double)wCount/(double)rTotal);
      if(PctDone!=PctDoneSave)
      {	printf("\n%d %% complete", PctDone);
      	PctDoneSave=PctDone;
      }
   }
   close(In1Handle);
   close(In2Handle);
   free(r1Buffer);
   free(r2Buffer);
   close(OutHandle);
	return(0);
}

