/* ******************************************

	cnpformattedaccount.c

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
#include <time.h>   

#include "cnp01.h"

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, OutHandle, aLength, r, InCount, InLength, Length;
   char *s, *t, *InBuffer, *InPath, *OutPath, *Client, c;
	time_t t0;
	struct tm *gmt, *now;

	printf("\n\n******* B E G I N   C N P F O R M A T T E D A C C O U N T *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Input File (full path)");
      printf("\n  B = Client Number");
      printf("\n  For example: \"%s /users/coupons/16860ra.srt 0140", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   Client=strdup(argv[2]);
   sprintf(scratch, "%s.out", InPath);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   InHandle=OpenFile(InPath, "", IN, NULL);

	t0=time(NULL);
	gmt=gmtime(&t0);
   now=localtime(&t0);
   InCount=0;
   InLength=3422;
   InBuffer=(char *)malloc(InLength);
   while(read(InHandle, InBuffer, InLength)>0)
   {	if(strcmp(Client, "0140")==0
   	|| strcmp(Client, "0277")==0
   	|| strcmp(Client, "0310")==0
   	|| strcmp(Client, "5641")==0
   	|| strcmp(Client, "5834")==0
//    || strcmp(Client, "5698")==0     // Line was removed due to program checked out for 2 years by AS
   	|| strcmp(Client, "0976")==0
   	|| strcmp(Client, "0255")==0
   	|| strcmp(Client, "1021")==0
   	|| strcmp(Client, "0733")==0
   	|| strcmp(Client, "2503")==0
        || strcmp(Client, "0281")==0
   	|| strcmp(Client, "2415")==0)
   	{	sprintf(scratch, "%.20s", InBuffer);
      	while(*scratch==' ')
         	memmove(scratch, scratch+1, strlen(scratch));
         for(s=scratch+strlen(scratch)-1; s>=scratch && *s==' '; s--)
         	*s=0;
         Length=strlen(scratch);
         if(Length>10)
         	memmove(scratch, scratch+Length-10, 11);
			else
         if(Length<10)
         {	memmove(scratch+10-Length, scratch, Length+1);
            memset(scratch, '0', 10-Length);
         }
         memcpy(InBuffer+30, scratch, 10);
      }


      else 
         if (strcmp(Client, "0513")==0)
         {   sprintf(scratch, "%.20s", InBuffer);
             while(*scratch==' ')
             memmove(scratch, scratch+1, strlen(scratch));
             for(s=scratch+strlen(scratch)-1; s>=scratch && *s==' '; s--)
                *s=0;
             Length=strlen(scratch);
             if(Length>6)
                memmove(scratch, scratch+Length-6, 7);
             else
             if(Length<6)
             {      memmove(scratch+6-Length, scratch, Length+1);
                    memset(scratch, '0', 6-Length);
             }
             memcpy(InBuffer+30, scratch, 6);
          }


      else
      	memcpy(InBuffer+30, InBuffer, 20);
      if(strcmp(Client, "0628")==0)
      {	// reformat spanish statement dates from dd/mm/yyyy to mm/dd/yyyy
      	// for upload to database
         // spanish can be identified by "CS660, CS694, CS695" in
         // FlexField2 offset 2247
         // StatementDate offset 937 for 10 bytes
         if(memcmp(InBuffer+2247, "CS660", 5)==0
   		|| memcmp(InBuffer+2247, "CS694", 5)==0
   		|| memcmp(InBuffer+2247, "CS695", 5)==0)
         {	sprintf(scratch, "%.3s%.3s%.4s", InBuffer+940, InBuffer+937, InBuffer+943);
            memcpy(InBuffer+937, scratch, 10);
         }
      }
/*
  this logic is used to add a "statement date" to the NCP10 record
  whenever "statement date" is blank.  The logic is restricted to
  "0310" for now since it uses a "gmt" date rather than the "local
  processing date".
  We might want to use the "localtime()" function rather than the "gmtime()"
  function as the default. (See commented "else" below.)
*/
      if(memcmp(InBuffer+937, "          ", 10)==0)
      {	if(strcmp(Client, "0310")==0)
	      {	sprintf(scratch, "%02d/%02d/%d", gmt->tm_mon+1, gmt->tm_mday, (gmt->tm_year-100)+2000);
   	   	memcpy(InBuffer+937, scratch, 10);
      	}
/*         else
	      {	sprintf(scratch, "%02d/%02d/%d", now->tm_mon+1, now->tm_mday, (now->tm_year-100)+2000);
   	   	memcpy(InBuffer+937, scratch, 10);
      	}*/
      }
   	write(OutHandle, InBuffer, InLength);
   	InCount++;
   }
   close(InHandle);
   free(InBuffer);
   close(OutHandle);
	return;
}

