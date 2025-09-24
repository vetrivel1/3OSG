/* ******************************************

	ncpcntr2.c

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

char *GetNextPart(char *s0, char *Part);
int main(int argc, char *argv[])

{	int InLength, Out1Handle, Out2Handle, Out3Handle, Out4Handle, r, w, rcount;
	FILE *In1Handle, *In2Handle;
   char *s, *t, *InBuffer, *In1Path, *In2Path, *Out1Path, *Out2Path,
   	*Out3Path, *Out4Path, *Out2Buffer, *aField, *bField,
      *FromPart, *sNext;
   char **d0, **d1, **dFound;
   int d0Count, d0Limit;
   char **e0, **e1;
   int e0Count, e0Limit;
   int KeyCountFound, WorkLength;

	printf("\n\n******* B E G I N   N C P C N T R 2 *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Full path for input");
      printf("\n  B = WorkLength (option file record length)");
      printf("\n\n    \"%s /users/public/36061.cntr.wrk.dd 3800\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   WorkLength=atoi(argv[2]);
   In1Handle=fopen(In1Path, "rt");
   if(In1Handle==NULL)
   {	printf("\n\"%s\" open failure", In1Path);
   	exit(1);
   }
   d0=NULL;
   d0Count=d0Limit=0;
   fgets(scratch, sizeof(scratch), In1Handle);
   while(!feof(In1Handle))
   {	if(d0Count==d0Limit)
   	{	d0Limit+=100;
      	d0=(char **)realloc(d0, d0Limit*sizeof(char *));
         memset(d0+d0Count, 0, 100*sizeof(char *));
      }
      d1=d0+d0Count;
      d0Count++;
      *d1=strdup(scratch);
	   fgets(scratch, sizeof(scratch), In1Handle);
   }
   fclose(In1Handle);
   sprintf(scratch, "%s.iomap", In1Path);
   In2Path=strdup(scratch);
   sprintf(scratch, "%s.extract", In1Path);
   Out1Path=strdup(scratch);
   sprintf(scratch, "%s.header", In1Path);
   Out2Path=strdup(scratch);
   sprintf(scratch, "%s.fields", In1Path);
   Out3Path=strdup(scratch);
   sprintf(scratch, "%s.extract.iomap", In1Path);
   Out4Path=strdup(scratch);
   Out1Handle=OpenFile(Out1Path, "", OUT, NULL);
   Out2Handle=OpenFile(Out2Path, "", OUT, NULL);
   Out3Handle=OpenFile(Out3Path, "", OUT, NULL);
   Out4Handle=OpenFile(Out4Path, "", OUT, NULL);
   In2Handle=fopen(In2Path, "rt");
   if(In2Handle==NULL)
   {	printf("\n\"%s\" open failure", In2Path);
   	exit(1);
   }
   e0=NULL;
   e0Count=e0Limit=0;
   Out2Buffer=(char *)malloc(sizeof(scratch));
   t=Out2Buffer;
//   strcpy(t, "RecordCode|");
//   t+=strlen(t);
   fgets(scratch, sizeof(scratch), In2Handle);
   while(!feof(In2Handle))
   {	if(e0Count==e0Limit)
   	{	e0Limit+=100;
      	e0=(char **)realloc(e0, e0Limit*sizeof(char *));
         memset(e0+e0Count, 0, 100*sizeof(char *));
      }
      e1=e0+e0Count;
      e0Count++;
      *e1=strdup(scratch);
	   fgets(scratch, sizeof(scratch), In2Handle);
   }
   fclose(In2Handle);
   for(e1=e0, KeyCountFound=0; e1-e0<e0Count && KeyCountFound<2; e1++)
   {	strcpy(scratch, *e1);
   	for(s=scratch+strlen(scratch)-1; s>=scratch && *s!=','; s--)
      	;
      s++;
      while(*s==' ')
        	s++;
      if(memcmp(s, "SecondaryKey", 12)==0)
        	KeyCountFound++;
   }
/*   if(KeyCountFound==2)
   	KeyCountFound=1;
   else
   	KeyCountFound=0;*/
   aField=scratch+4000;
   bField=scratch+4200;
   FromPart=scratch+4300;
   for(e1=e0; e1-e0<e0Count; e1++)
   { 	strcpy(scratch, *e1);
   	for(s=scratch+strlen(scratch)-1; s>=scratch && *s!=','; s--)
      	;
      if(*s==',')
      	*s=0;
   	sNext=scratch;
      while(*sNext!=0)
      {	sNext=GetNextPart(sNext, FromPart);
      	if(*FromPart!='"')
	      {	strcpy(aField, FromPart);
			  	for(d1=d0, dFound=NULL; d1-d0<d0Count && dFound==NULL; d1++)
	         {	s=strstr(*d1, ",");
   	      	sprintf(bField, "%.*s", s-*d1, *d1);
      	   	if(strcmp(aField, bField)==0)
         			dFound=d1;
         		if(dFound!=NULL)
	            {	write(Out1Handle, *d1, strlen(*d1));
   	         	sprintf(t, "%s|", aField);
      	         t+=strlen(t);
         	      write(Out3Handle, aField, strlen(aField));
            	   write(Out3Handle, "\n", 1);
	            }
   	      }
         }
      }
      write(Out4Handle, *e1, strlen(*e1));
   }
   if(!KeyCountFound)
   {  strcpy(t, "SecondaryKey|");
   	t+=strlen(t);
   	write(Out3Handle, "SecondaryKey\n", 13);
      sprintf(scratch, "SecondaryKey, %d, 9, Text, 0\r\n", WorkLength-100);
      write(Out1Handle, scratch, strlen(scratch));
      write(Out4Handle, "SecondaryKey, SecondaryKey\r\n", 28);
   }
   strcpy(t-1, "\n");
   write(Out2Handle, Out2Buffer, strlen(Out2Buffer));
   free(Out2Buffer);
   close(Out1Handle);
   close(Out2Handle);
   close(Out3Handle);
   close(Out4Handle);
	return(0);
}

char *GetNextPart(char *s0, char *Part)

{	char *s1, *s;

	if(*s0=='+')
   	s0++;
   while(*s0==' ')
   	s0++;
   if(*s0=='"')
   {	for(s1=s0+1; *s1!='"' && *s1!=0; s1++)
   		;
   	if(*s1==0)
      {	printf("\nIOMAP syntax error - no end quote: %s", s0);
        	exit(1);
      }
      sprintf(Part, "%.*s", s1-s0, s0);
      s1++;
      while(*s1==' ')
      	s1++;
   }
   else
   {	for(s1=s0+1; *s1!='+' && *s1!=0; s1++)
   		;
      sprintf(Part, "%.*s", s1-s0, s0);
      for(s=Part+strlen(Part)-1; s>=Part && *s==' '; s--)
      	*s=0;
   }
	return(s1);
}

