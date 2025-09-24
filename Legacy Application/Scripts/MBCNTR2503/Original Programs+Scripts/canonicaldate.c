#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include <sys/stat.h>
#include "cnp01.h"

#define CENTURY_BASE 19		// yy>=DECIDE_CENTURY uses CENTURY_BASE, else CENTURY_BASE+1
#define DECIDE_CENTURY 50

int DayBase[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
int LeapBase[] = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335};
int DaysInMonth[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
int LeapInMonth[] = {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

char *Format[] = {
   "yymmdd",
   "mm/dd/yy",
   "January 1, 2000",
   "mm/yy",
   "mmddyy",
   "mm/dd/yyyy",
   "pymmdd",
   "dd-mmm-yy",
   "yyyymmdd",
   "1 Spanish 2004",
   NULL
};

char *Month[] = {
   "January",
   "February",
   "March",
   "April",
   "May",
   "June",
   "July",
   "August",
   "September",
   "October",
   "November",
   "December",
   "JANUARY",
   "FEBRUARY",
   "MARCH",
   "APRIL",
   "MAY",
   "JUNE",
   "JULY",
   "AUGUST",
   "SEPTEMBER",
   "OCTOBER",
   "NOVEMBER",
   "DECEMBER",
   "Jan",
   "Feb",
   "Mar",
   "Apr",
   "May",
   "Jun",
   "Jul",
   "Aug",
   "Sep",
   "Oct",
   "Nov",
   "Dec",
   "JAN",
   "FEB",
   "MAR",
   "APR",
   "MAY",
   "JUN",
   "JUL",
   "AUG",
   "SEP",
   "OCT",
   "NOV",
   "DEC",
   NULL
};

char *SpanishMonth[] = {
   "Enero",
   "Febrero",
   "Marzo",
   "Abril",
   "Mayo",
   "Junio",
   "Julio",
   "Agosto",
   "Septiembre",
   "Octubre",
   "Noviembre",
   "Diciembre",
   NULL
};

int GetFormatNumber(char *InFormat);
int DecideCentury(int yy);

int CanonicalDate(char *OutDate, char *InDate, int DateLength, char *InFormat)

{  int i, j, cc, yy, mm, dd;
   char *month, *day, *year, *comma, *decimal, *edit, scratch[512], *sDate, *s;

   switch(GetFormatNumber(InFormat)) {
   case 0:	// yymmdd
      j = 6 - DateLength;
      if(j<0)
         j=0;
      memcpy(scratch+j, InDate, DateLength);
      memset(scratch, '0', j);
      sDate=scratch+400;
      strncpy(sDate, scratch, 2);
      *(sDate+2)=0;
      yy=atoi(sDate);
      cc=DecideCentury(yy);
      sprintf(OutDate, "%d%.2s%.2s%.2s", cc, scratch, scratch+2, scratch+4);
      break;
   case 1:  // mm/dd/yy
      memcpy(scratch, InDate, DateLength);
      *(scratch+DateLength)=0;
      month=scratch;
      day=strstr(month, "/");
      if(day==NULL || (day-month!=1 && day-month!=2))
         return(1);
      day++;
      year=strstr(day, "/");
      if(year==NULL || (year-day!=1 && year-day!=2))
         return(1);
      year++;
      if(strlen(year)>2)
      	return(1);
      yy=atoi(year);
      cc=DecideCentury(yy);
      if(day-month==2)
      {  memmove(scratch+1,scratch, strlen(scratch)+1);
         *scratch='0';
         day++;
         year++;
      }
      if(year-day==2)
      {  memmove(day+1, day, strlen(day)+1);
         *day='0';
         year++;
      }
      sprintf(OutDate, "%d%02d%.2s%.2s", cc, yy, month, day);
      break;
   case 2:  // January 1, 2000
		while(*InDate==' ')
      	InDate++;
      mm = GetMonth(InDate);
      if(mm<0)
         return(1);
      else
         mm++;
      for(day=InDate+strlen(Month[mm-1]); day-InDate<DateLength && isspace(*day); day++)
         ;
      for(year=day, comma=NULL; year-InDate<DateLength && !isspace(*year); year++)
         if(*year==',')
            comma = year;
      if(comma==NULL)
         comma=year-1;
      while(year-InDate<DateLength && isspace(*year))
         year++;
      if(year-InDate==DateLength)
         return(1);
      strncpy(InDate, day, comma-day);
      *(InDate+(comma-day))=0;
      dd = atoi(InDate);
      strncpy(InDate, year, 4);
      *(InDate+4)=0;
      yy=atoi(InDate);
      sprintf(OutDate, "%04d%02d%02d", yy, mm, dd);
      break;
   case 3:  // mm/yy
      memcpy(scratch, InDate, DateLength);
      *(scratch+DateLength)=0;
      month=scratch;
      year=strstr(month, "/");
      if(year==NULL || (year-month!=1 && year-month!=2))
         return(1);
      if(year-month==1)
      {  memmove(scratch+1, scratch, strlen(scratch)+1);
         *scratch='0';
         year++;
      }
      year++;
      if(strlen(year)>2)
      	return(1);
      yy=atoi(year);
      cc=DecideCentury(yy);
      sprintf(OutDate, "%d%02d%.2s%.2s", cc, yy, month, "01");
      break;
   case 4:	// mmddyy
      j = 6 - DateLength;
      if(j<0)
      	return(1);
      memcpy(scratch+j, InDate, strlen(InDate));
      memset(scratch, '0', j);
      for(s=scratch; s-scratch<6; s++)
      	if(*s<'0' || *s>'9')
         	return(1);
      year=scratch+4;
      *(year+2)=0;
      yy=atoi(year);
      cc=DecideCentury(yy);
      sprintf(OutDate, "%d%.2s%.2s%.2s", cc, scratch+4, scratch, scratch+2);
      break;
   case 5:  // mm/dd/yyyy
      memcpy(scratch, InDate, DateLength);
      *(scratch+DateLength)=0;
      month=scratch;
      day=strstr(month, "/");
      if(day==NULL || (day-month!=1 && day-month!=2))
         return(1);
      if(day-month==1)
      {  memmove(scratch+1,scratch, strlen(scratch)+1);
         *scratch='0';
         day++;
      }
      day++;
      year=strstr(day, "/");
      if(year==NULL || (year-day!=1 && year-day!=2))
         return(1);
      if(year-day==1)
      {  memmove(day+1, day, strlen(day)+1);
         *day='0';
         year++;
      }
      year++;
      if(strlen(year)<3)
      {  yy=atoi(year);
         cc=DecideCentury(yy);
         sprintf(OutDate, "%d%02d%.2s%.2s", cc, yy, month, day);
      }
      else
      if(strlen(year)==3)
      {  memmove(year+1, year, strlen(year)+1);
         *year='2';
         sprintf(OutDate, "%.4s%.2s%.2s", year, month, day);
      }
      else
         sprintf(OutDate, "%.4s%.2s%.2s", year, month, day);
      break;
   case 6:	//pymmdd
      memcpy(scratch, InDate, 6);
      unpackit((unsigned char *)scratch, (unsigned char *)scratch+100, 2);
      edit=scratch+100+3;
      if(*edit!='f' && *edit!='c')
         return(1);
      strncpy(OutDate, scratch+100, 3);
      *(OutDate+3)=0;
      yy=atoi(OutDate);
      yy+=CENTURY_BASE*100;
      sprintf(OutDate, "%d%.2s%.2s", yy, scratch+2, scratch+4);
      break;
   case 7:	// dd-mmm-yy
      memcpy(scratch, InDate, DateLength);
      *(scratch+DateLength)=0;
      day=scratch;
      month=strstr(day, "-");
      if(month==NULL || (month-day!=1 && month-day!=2))
         return(1);
      if(month-day==1)
      {  memmove(scratch+1,scratch, strlen(scratch)+1);
         *scratch='0';
         month++;
      }
      month++;
      year=strstr(month, "-");
      if(year==NULL)
         return(1);
      year++;
      yy=atoi(year);
      cc=DecideCentury(yy);
      mm = GetMonth(month);
      if(mm<0)
         return(1);
      else
         mm++;
      sprintf(OutDate, "%d%02d%02d%.2s", cc, yy, mm, day);
      break;
   case 8:      // yyyymmdd
      sprintf(OutDate, "%.*s", DateLength, InDate);
      break;
   default:
      printf("Unknown field format \"%s\", %d", InFormat, GetFormatNumber(InFormat));
      return(1);
      break;
   }
   return(0);
}

int GetMonth(char *s)

{  int i, ret;

   for(i=0, ret=-1; Month[i]!=NULL && ret<0; i++)
      if(strncmp(s, Month[i], strlen(Month[i]))==0)
         ret=i;
      if(ret!=-1)
         ret%=12;
   return(ret);
}

void FormatDate(char *Output, char *Input, char *sFormat)

{  int i, j, yy, mm, dd, LessThanZero;
   char *decimal, *t;

   switch(GetFormatNumber(sFormat)) {
   case 0:	// yymmdd
      sprintf(Output, "%.6s", Input+2);
      break;
   case 1:  // mm/dd/yy
      sprintf(Output, "%.2s/%.2s/%.2s", Input+4, Input+6, Input+2);
      break;
   case 2:  // January 1, 2000
      strncpy(Output, Input, 4);
      *(Output+4)=0;
      yy=atoi(Output);
      strncpy(Output, Input+4, 2);
      *(Output+2)=0;
      mm=atoi(Output);
      strncpy(Output, Input+6, 2);
      dd=atoi(Output);
      sprintf(Output, "%s %d, %d", Month[mm-1], dd, yy);
      break;
   case 3:	// mm/yy
      sprintf(Output, "%.2s/%.2s", Input+4, Input+2);
      break;
   case 4:	// mmddyy
      sprintf(Output, "%.2s%.2s%.2s", Input+4, Input+6, Input+2);
      break;
   case 5:	// mm/dd/yyyy
      sprintf(Output, "%.2s/%.2s/%.4s", Input+4, Input+6, Input);
      break;
   case 6:	// pymmdd
		// skip for now
      break;
   case 7:	// dd-mmm-yy
      sprintf(Output, "%.2s", Input+4);
      mm=atoi(Output);
      sprintf(Output, "%.2s-%.3s-%.2s", Input+6, Month[mm+24-1], Input+2);
      break;
   case 8:      // yyyymmdd
      sprintf(Output, "%.8s", Input);
      break;
   case 9:		// 1 Spanish 2004
      strncpy(Output, Input, 4);
      *(Output+4)=0;
      yy=atoi(Output);
      strncpy(Output, Input+4, 2);
      *(Output+2)=0;
      mm=atoi(Output);
      strncpy(Output, Input+6, 2);
      dd=atoi(Output);
      sprintf(Output, "%d de %s de %d", dd, SpanishMonth[mm-1], yy);
      break;
   default:
      printf("\nInvalid format string:  \"%s\"", sFormat);
      exit(1);
      break;
   }
   return;
}

int GetFormatNumber(char *InFormat)

{  int i, ret;

   for(i=0, ret=-1; Format[i]!=NULL && ret==-1; i++)
      if(strcmp(InFormat, Format[i])==0)
         ret=i;
   return(ret);
}

int DecideCentury(int yy)

{
   if(yy<DECIDE_CENTURY)
      return(CENTURY_BASE+1);
   else
      return(CENTURY_BASE);
}

