//---------------------------------------------------------------------------

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include "/users/temp/cnp01.h"

char scratch[1024];

dd_rec *ddLoadFromFile(char *ddPath)

{  FILE *fp;
	dd_rec *dd;
   parse_rec *p0;
   int ParseCount;

	dd = (dd_rec *)malloc(sizeof(dd_rec));
   memset(dd, 0, sizeof(dd_rec));
   dd->Path=strdup(ddPath);
   fp = fopen(dd->Path, "rt");
   if(fp==NULL)
   {	printf("\n%s open failure\n", dd->Path);
   	exit(1);
   }
   fgets(scratch, sizeof(scratch)-1, fp);
   while(!feof(fp))
   {  p0=ParseExpression(scratch, &ParseCount, ',', 1, 0, 1);
      AddField(dd, p0, ParseCount);
      fgets(scratch, sizeof(scratch)-1, fp);
	   RemoveParse(p0, ParseCount);
   }
   fclose(fp);
   return(dd);
}

void AddField(dd_rec *dd, parse_rec *p0, int ParseCount)

{  char **s;
	int i;
   field_rec *f;

	if(ParseCount<3)
   	return;
	if(dd->FieldCount==dd->FieldLimit)
   {	dd->FieldLimit+=100;
		dd->Field=(field_rec *)realloc(dd->Field, dd->FieldLimit*sizeof(field_rec));
      memset(dd->Field+dd->FieldCount, 0, 100*sizeof(field_rec));
   }
   f=dd->Field+dd->FieldCount;
	for(i=0; i<ParseCount; i++)
   {	switch(i) {
   	case 0:
			f->FieldName=strdup(p0->String);
         break;
      case 1:
			f->Offset=atoi((p0+1)->String);
         break;
      case 2:
			f->Length=atoi((p0+2)->String);
         break;
      case 3:
			for(s=DataTypes; *s!=NULL; s++)
				if(strcmp(*s, (p0+3)->String)==0)
				{  f->DataType = s-DataTypes;
					break;
				}
         break;
      case 4:
			f->Decimals=atoi((p0+4)->String);
         break;
      case 5:
			f->FieldFormat=strdup((p0+5)->String);
         break;
      }
	}
   dd->FieldCount++;
	return;
}

field_rec *GetFieldRecByName(dd_rec *dd, char *FieldName)

{	field_rec *fr, *found;

	for(fr=dd->Field, found=NULL; fr-dd->Field<dd->FieldCount && found==NULL; fr++)
   	if(strcmp(fr->FieldName, FieldName)==0)
      	found=fr;
   return(found);
}

