/* ******************************************

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>

char dehexify(char c);

void unpackit(unsigned char *pack, unsigned char *unpack, int plen)

{  unsigned char c, *p1, *u1;

   for(p1=pack, u1=unpack; p1-pack<plen; p1++, u1+=2)
   {  c = *p1;
      *(u1+1) = c & 15;
      c = *p1;
      c = (c & 240) >> 4;
      *u1 = dehexify(c);
      *(u1+1) = dehexify(*(u1+1));
   }
   *u1 = 0;
   return;
}

int FieldIsPacked(char *s0, int Len)

{	char LastDigit, unpack[64], *s;
	int IsPacked;

   if(Len>31)
   // return 0, shouldn't have 62+ digits in a numeric field!!
      return(0);
	unpackit((unsigned char *)s0, (unsigned char *)unpack, Len);
   s=unpack+2*Len-1;
   if(*s!='c' && *s!='f' && *s!='d')
      return(0);
   for(s--, IsPacked=1; s>=unpack && IsPacked; s--)
      if(*s<'0' || *s>'9')
         IsPacked=0;
   return(IsPacked);

/*	LastDigit=unpack+2*Len-1;
   if(LastDigit>='0' && LastDigit<='9')
   	IsPacked=0;
   else
   	IsPacked=1;
   if(LastDigit=='C' || LastDigit=='D' || LastDigit=='F')
   	IsPacked=1;
   else
   	IsPacked=0;
   return(IsPacked);*/
}

int FieldIsPrint(char *s0, int Len)

{	char *s1;
	int IsPrint;

	for(s1=s0, IsPrint=1; s1-s0<Len && IsPrint; s1++)
   	if(!isprint(*s1))
      	IsPrint=0;
   return(IsPrint);
}

