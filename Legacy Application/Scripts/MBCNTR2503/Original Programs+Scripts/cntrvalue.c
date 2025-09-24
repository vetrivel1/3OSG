/* ******************************************

	cntrvalue.c

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

#include "cnp01.h"

typedef struct {
	char *ControlValue;
   char *Extension;
   char *OutPath;
   char *Value;
} cvrec;

cvrec cv0[] = {
	{"CNTRKEY", "cntrkey"},
   {"NCPJAX", "ncpjax"}
};

char scratch[8192];

int main(int argc, char *argv[])

{	int InHandle, i, OutHandle, sOffset;
	FILE *pcHandle;
   char *s, *t, *JobNumber, *OutPath, *Project, *pcPath, *ProjectBase,
   	*NcpJaxName;
   cvrec *cv1, *cvFound;
   int cv0Count;
   struct stat statbuf;

	printf("\n\n******* B E G I N   C N T R V A L U E *******\n\n");
	if (argc<4)
   {	printf("\nUsage is \"%s A B C\"", argv[0]);
   	printf("\n  A = Job Number");
      printf("\n  B = Project Name");
      printf("\n  C = Project Base");
      printf("\n  For example: \"%s 16860 couponlps /users/devel/container", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   JobNumber=strdup(argv[1]);
   Project=strdup(argv[2]);
   ProjectBase=strdup(argv[3]);
   cv0Count=sizeof(cv0)/sizeof(cvrec);
   for(cv1=cv0; cv1-cv0<cv0Count; cv1++)
   {	sprintf(scratch, "/users/public/%s.%s", JobNumber, cv1->Extension);
   	cv1->OutPath=strdup(scratch);
      if(stat(cv1->OutPath, &statbuf)==0)
      	unlink(cv1->OutPath);
   }
   sprintf(scratch, "%s/%s/ddcontrol.txt", ProjectBase, Project);
   pcPath=strdup(scratch);
   pcHandle=fopen(pcPath, "rt");
   if(pcHandle==NULL)
   {	printf("\n\"%s\" open failure", pcPath);
   	printf("\n");
      exit(1);
   }
   fgets(scratch, sizeof(scratch), pcHandle);
   while(!feof(pcHandle))
   {  if(memcmp(scratch, "CONTAINER KEY,", 14)==0)
      {	s=scratch+14;
      	while(*s==' ')
         	s++;
         for(t=s+strlen(s)-1; t>=s && (*t=='\r' || *t=='\n'); t--)
         	*t=0;
         for(cv1=cv0, cvFound=NULL; cv1-cv0<cv0Count && cvFound==NULL; cv1++)
         	if(strcmp(cv1->ControlValue, "CNTRKEY")==0)
            	cvFound=cv1;
         if(cvFound!=NULL)
	         cvFound->Value=strdup(s);
      }
      else
      if(strstr(scratch, ", NCPJAX")!=NULL)
      {	s=strstr(scratch, ",");
      	if(s!=NULL)
         {	*s=0;
	         for(cv1=cv0, cvFound=NULL; cv1-cv0<cv0Count && cvFound==NULL; cv1++)
   	      	if(strcmp(cv1->ControlValue, "NCPJAX")==0)
      	      	cvFound=cv1;
            if(cvFound!=NULL)
	         	cvFound->Value=strdup(scratch);
         }
      }
	   fgets(scratch, sizeof(scratch), pcHandle);
   }
   fclose(pcHandle);
   for(cv1=cv0; cv1-cv0<cv0Count; cv1++)
   	if(cv1->Value!=NULL)
      {  OutHandle=OpenFile(cv1->OutPath, "", OUT, NULL);
   		sprintf(scratch, "%s\n", cv1->Value);
		   write(OutHandle, scratch, strlen(scratch));
   		close(OutHandle);
      }
	return(0);
}

