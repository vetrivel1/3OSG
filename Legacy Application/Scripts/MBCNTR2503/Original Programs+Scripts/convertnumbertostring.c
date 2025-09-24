//---------------------------------------------------------------------------

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include "cnp01.h"

int ConvertNumberToString(char *String, void *Number, int NumLength,
		int DataType, int Decimals)

// canonical format for numbers is 0.12345-

{	int i, LessThanZero, ret;
	char c, *s, *t;
   long long L;
   double x;

	ret=0;
	switch(DataType) {
   case 1:	// unpacked numeric data
   			// check for negative
            // remove leading space or zero or + or -
            // remove trailing spaces
   	memcpy(String, Number, NumLength);
      for(s=String+NumLength-1; s>=String && *s==' '; s--)
      	*s=0;
      s++;
      NumLength=s-String;
      *(String+NumLength)=0;
      if(NumLength<Decimals)
      {	NumLength=Decimals;
      	ret=1;
      }
//   	printf("\nCase 1:  a)  Number=%s", String);
      c=*(String+NumLength-1);
      LessThanZero=0;
      if(c>='p' && c<='y')
      {	c-='p';
      	*(String+NumLength-1)=c+'0';
      	*(String+NumLength)='-';
         *(String+NumLength+1)=0;
         LessThanZero=1;
      }
      else
      if(c>='{' && c<='I')
      {	c-='{';
      	*(String+NumLength-1)=c+'0';
      }
      else
      if(c=='-')
      	LessThanZero=1;
      s=String;
      if(*s=='+' || *s=='-')
      {	if(*s=='-')
	      {	*(String+NumLength)=*s;
         	*(String+NumLength+1)=0;
            LessThanZero=1;
         }
      	memmove(String, String+1, strlen(String));
      }
      if(Decimals>0)
      {	s=String+strlen(String)-Decimals;
      	if(LessThanZero)
         	s--;
      	memmove(s+1, s, strlen(s)+1);
         *s='.';
      }
      for(s=String; *s=='0' || *s==' '; s++)
      	;
      if(*s=='.' || *s==0)
      	s--;
      if(s>String)
	      memmove(String, s, strlen(s)+1);
      s=String+strlen(String);
//   	printf("\nCase 1:  b)  Number=%s", String);
   	break;
   case 2:	// packed numeric data
		unpackit((unsigned char *)Number, (unsigned char *)String, NumLength);
      s=String+2*NumLength;
      *s=0;
      s--;
      LessThanZero=0;
      if(*s=='f' || *s=='c' || *s=='F' || *s=='C')
			*s=0;
      else
      if(*s=='d' || *s=='D')
      {	*s='-';
      	LessThanZero=1;
      }
      else
      	ret=1;
      if(Decimals>0)
      {	s=String+strlen(String)-Decimals;
      	if(LessThanZero)
         	s--;
      	memmove(s+1, s, strlen(s)+1);
      	*s='.';
      }
      for(s=String; *s=='0'; s++)
      	;
      if(s>String)
	      memmove(String, s, strlen(s)+1);
	   if(*String==0)
   		strcpy(String, "0");
   	break;
   case 3:	// double
   	sprintf(String, "%.*lf", Decimals, *((double *)Number));
   	break;
   case 4:	// float
   	sprintf(String, "%.*f", Decimals, *((float *)Number));
   	break;
   case 5:	// int
   	sprintf(String, "%d", *((int *)Number));
   	break;
   case 6:	// long int
   	memcpy(&L, Number, sizeof(L));
      if(Decimals==0)
      	sprintf(String, "%lld", L);
      else
      {	x=(double)L;
	      for(i=0; i<Decimals; i++)
   	   	x/=10.0;
     		sprintf(String, "%0.*lf", Decimals, x);
      }
   	break;
   case 7:	// short int
   	sprintf(String, "%hd", *((short *)Number));
   	break;
   default:	// invalid data type
   	printf("\n\nValid numeric data types are 1-7, DataType=%d\n\n", DataType);
      exit(1);
   	break;
   }
   if(DataType>2)
   {  s=String;
      if(*s=='-')
      {	memmove(s, s+1, strlen(String));
      	*(String+strlen(String))='-';
      }
   }
   s=String;
   if(*s=='.')
   {	memmove(s+1, s, strlen(s)+1);
   	*s='0';
   }
   return(ret);
}

void FormatNumberString(char *Result, char *sValue, char *FormatString)

{	int Decimals, Commas, DollarSign, PercentSign, Minus, LessThanZero,
      Lzf, DigitLimit, DigitCount;
	char *s0, *s1;
   double Value;

	s0=(char *)malloc(1000);
	if(FormatString==NULL)
 	  	strcpy(s0, sValue);
	else
   {	Decimals=*FormatString-'0';
   	if(strstr(FormatString, "Co")==NULL)
      	Commas=0;
      else
      	Commas=1;
      if(strstr(FormatString, "$")==NULL)
      	DollarSign=0;
      else
      	DollarSign=1;
      if(strstr(FormatString, "%")==NULL)
      	PercentSign=0;
      else
      	PercentSign=1;
      if(strstr(FormatString, "T-")!=NULL)
      	Minus=0;
      else
      if(strstr(FormatString, "L-")!=NULL)
      	Minus=1;
      else
      	Minus=2;
      if(strstr(FormatString, "Lzf")==NULL)
         Lzf=0;
      else
         Lzf=1;
      if((s1=strstr(FormatString, "Dg"))==NULL)
         DigitLimit=0;
      else
         DigitLimit=atoi(s1+2);
      Value=atof(sValue);
      sprintf(s0, "%.*lf", Decimals, Value);
      if(*s0=='-')
      	memmove(s0, s0+1, strlen(s0));
      if(Commas)
      {	s1=strstr(s0, ".");
      	if(s1==NULL)
         	s1=s0+strlen(s0);
         s1-=3;
         while(s1>s0)
         {	memmove(s1+1, s1, strlen(s1)+1);
         	*s1=',';
         	s1-=3;
         }
      }
      if(DollarSign)
      {	memmove(s0+1, s0, strlen(s0)+1);
      	*s0='$';
      }
      if(PercentSign)
      {	s1=s0+strlen(s0);
      	strcpy(s1, "%");
      }
      if(Value<0.0)
      {	switch(Minus) {
         case 0:	// trailing minus
         	s1=s0+strlen(s0);
            strcpy(s1, "-");
            break;
         case 1:	// leading minus
         	memmove(s0+1, s0, strlen(s0)+1);
            *s0='-';
            break;
         case 2:	// parens
            memmove(s0+1, s0, strlen(s0)+1);
            *s0='(';
            strcat(s0, ")");
            break;
         default:
         	break;
         }
      }
      for(s1=s0, DigitCount=0; *s1!=0; s1++)
         if(*s1>='0' && *s1<='9')
            DigitCount++;
      if(DigitCount<DigitLimit)
      {  memmove(s0+DigitLimit-DigitCount, s0, strlen(s0)+1);
         if(Lzf)
            memset(s0, '0', DigitLimit-DigitCount);
         else
            memset(s0, ' ', DigitLimit-DigitCount);
      }
   }
   strcpy(Result, s0);
   free(s0);
   return;
}

