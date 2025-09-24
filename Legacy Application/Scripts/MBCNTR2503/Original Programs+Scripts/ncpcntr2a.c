/* ******************************************

	ncpcntr2a.c

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

{	int OutHandle;
   char *In1Path, *In2Path, *OutPath;
   dd_rec *d1, *d2;
   field_rec *f0, *f1, *g0, *g1;
   int f0Count, g0Count;

	printf("\n\n******* B E G I N   N C P C N T R 2 *******\n\n");
	if (argc<3)
   {	printf("\nUsage is \"%s A B\"", argv[0]);
      printf("\n  A = Full path for \"extract\" dd");
      printf("\n  B = Full path for \"fixed\" dd");
      printf("\n\n  \"%s /users/public/36062rnp.cntr.wrk.dd.extract", argv[0]);
      printf("\n       /users/public/36062rnp.cntr.wrk.tab.fixed.dd\"", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   In1Path=strdup(argv[1]);
   In2Path=strdup(argv[2]);
   d1=ddLoadFromFile(In1Path);
   d2=ddLoadFromFile(In2Path);
   f0=d1->Field;
   f0Count=d1->FieldCount;
   g0=d2->Field;
   g0Count=d2->FieldCount;
   if(f0Count!=g0Count)
   {	printf("\nf0Count (%d) must equal g0Count (%d)", f0Count, g0Count);
   	exit(1);
   }
   sprintf(scratch, "%s.new", In2Path);
   OutPath=strdup(scratch);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   for(f1=f0, g1=g0; g1-g0<g0Count; f1++, g1++)
   {	sprintf(scratch, "%s, %d, %d, %s, %d", g1->FieldName, g1->Offset, g1->Length, DataTypes[g1->DataType], g1->Decimals);
		if(f1->FieldFormat!=NULL && strlen(f1->FieldFormat)>0)
      	sprintf(scratch+strlen(scratch), ", %s\r\n", f1->FieldFormat);
      else
      	strcat(scratch, "\r\n");
      write(OutHandle, scratch, strlen(scratch));
   }
	return(0);
}

