/* ******************************************

	imbremit6.c

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

#include "/users/temp/cnp01.h"

int MonthDays[] = {31,28,31,30,31,30,31,31,30,31,30,31};
int  LeapDays[] = {31,29,31,30,31,30,31,31,30,31,30,31};
char scratch[8192];

int CheckEndOfMonth(int mm, int dd, int yy);
int main(int argc, char *argv[])

{	int In2Handle, OutHandle, In1Length, In2Length, rCount, x,
		mm, dd, yy, ddPrint;
	FILE *In1Handle;
   char *In1Path, *In2Path, *OutPath, *In1Buffer, *In2Buffer, *s, *t, *Account,
   	*StatementDate, *Payment, *Job, *Product, *Client, *In1File, *Zip;

	printf("\n\n******* B E G I N   I M B R E M I T 6 *******\n\n");
	if (argc<5)
   {	printf("\nUsage is \"%s A B C D\"", argv[0]);
   	printf("\n  A = Input file (full path)");
      printf("\n  B = Job Number (all digits)");
      printf("\n  C = Product");
      printf("\n  D = Client");
      printf("\n  For example:");
      printf("\n  > \"%s /users/public/60504r.cntr.grp 1360504 1000 0628\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   Job=strdup(argv[2]);
   Product=strdup(argv[3]);
   Client=strdup(argv[4]);
   In1Length=sizeof(scratch);
   for(s=In1Path+strlen(In1Path)-1; s>=In1Path && *s!='/'; s--)
   	;
   s++;
   In1File=s;
   sprintf(scratch, "/users/public/imb0/%s.imb0.remit1", In1File);
   In2Path=strdup(scratch);
   In2Length=202;
   In1Handle=fopen(In1Path, "rt");
   if(In1Handle==NULL)
   {	printf("\n\"%s\" open error", In1Path);
   	exit(1);
   }
   In2Handle=OpenFile(In2Path, "", IN, NULL);
   sprintf(scratch, "%s.remit3", In1Path);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   In1Buffer=(char *)malloc(In1Length);
   In2Buffer=(char *)malloc(In2Length);
   rCount=0;
   Account=scratch+7800;
   StatementDate=scratch+7850;
   Payment=scratch+7900;
   Zip=scratch+7950;
   fgets(In1Buffer, In1Length, In1Handle);
   while(!feof(In1Handle))
   {	if(memcmp(In1Buffer+3, "NCP10", 5)==0)
		{	read(In2Handle, In2Buffer, In2Length);
			sprintf(Account, "%.20s", In1Buffer+2147);
	   	for(s=Account+19; s>=Account && *s==' '; s--)
   	   	*s=0;
	      while(*Account==' ')
   	   	memmove(Account, Account+1, strlen(Account));
         sprintf(StatementDate, "%.10s", In1Buffer+1911);
         sprintf(scratch, "%.2s", StatementDate);
         mm=atoi(scratch);
         sprintf(scratch, "%.2s", StatementDate+3);
         dd=atoi(scratch);
         sprintf(scratch, "%.4s", StatementDate+6);
         yy=atoi(scratch);
	      ddPrint=CheckEndOfMonth(mm, dd, yy);
   	  	sprintf(StatementDate, "%02d/%02d/%d", mm, ddPrint, yy);
   	   sprintf(Payment, "%.11s", In1Buffer+1933);
         t=Payment+strlen(Payment)-1;
         if(*t=='-')
         {	for(s=Payment; *s==' '; s++)
         		;
         	memmove(s+1, s, t-s);
            *s='-';
         }
         sprintf(Zip, "%.5s%.4s%.2s", In1Buffer+142, In1Buffer+148, In1Buffer+153);
	   	for(s=Zip+10; s>=Zip && *s==' '; s--)
   	   	*s=0;
	   	sprintf(scratch, "%.20s,%s,%s,%s,%.5s,%s,%s,%s,,,,%.11s\r\n", In2Buffer+31,
   	   	Account, StatementDate, Payment, Zip, Job, Product,
      	   Client, In2Buffer+20);
	   	write(OutHandle, scratch, strlen(scratch));
      }
		rCount++;
      fgets(In1Buffer, In1Length, In1Handle);
   }
   fclose(In1Handle);
   close(In2Handle);
   close(OutHandle);
	return(0);
}

int CheckEndOfMonth(int mm, int dd, int yy)

{	int mDays;

	if(dd<29	|| mm<1 || mm>12)
   	return(dd);
   else
	if(yy%4==0 && (yy%100!=0 || yy%400==0))
   	mDays=LeapDays[mm-1];
   else
   	mDays=MonthDays[mm-1];
   if(dd>mDays)
   	return(mDays);
   else
   	return(dd);
}

