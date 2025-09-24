#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "/users/temp/cnp01.h"

#define IMB_LENGTH 700
#define NCP10_LENGTH 2500

typedef struct s_convert {
   char *ToName;
	char *FromName;
   int Source;			// 1-700 imb, 2-default value, 3-ncpcontainer.cbl.dd
   char *Default;
   field_rec *To;
	field_rec *From;
} convert_rec;

convert_rec Convert[] = {
	{"RecordIndex", "RecordIndex", 1},
	{"Account", "Account", 1},
	{"ImbServiceTypeIDMailer", "ImbServiceType", 1},
	{"ImbMailerID", "ImbId", 1},
	{"ImbSerialNumber", "ImbSequence", 1},
	{"ImbZip5Mailer", "Zip5", 1},
	{"ImbZip4Mailer", "Zip4", 1},
	{"ImbDeliveryPointMailer", "Dpbc", 1},
   {"EncodedImbMailer", "PostalCodes", 1},
	{"PrintedAddressLine1", "Address1", 1},
	{"PrintedAddressLine2", "Address2", 1},
	{"PrintedAddressLine3", "Address3", 1},
	{"PrintedAddressLine4", "Address4", 1},
	{"PrintedAddressLine5", "Address5", 1},
	{"PrintedAddressLine6", "City", 1},
	{"State", "State", 1},
	{"Zip5", "Zip5", 1},
	{"Zip4", "Zip4", 1},
	{"OriginalAddressLine1", "Address1", 3},
	{"OriginalAddressLine2", "Address2", 3},
	{"OriginalAddressLine3", "Address3", 3},
	{"OriginalAddressLine4", "Address4", 3},
	{"OriginalAddressLine5", "Address5", 3},
   // need special logic if original file has UNPARSED CSZ
   // if that's the case, then we can use OrigCszOffset, OrigCszLength
	{"OriginalCity", "City", 3},
	{"OriginalState", "State", 3},
	{"OriginalZip5", "Zip5", 3},
	{"OriginalZip4", "Zip4", 3},
//	{"OriginalDpbc", "Dpbc", 3},
	{"CASSErrorCode", "PostalError", 1},
	{"FormattedAccount", "FormattedAccount", 1},
	{"ImbBarcodeIDMailer", "", 2, "00"},	// per "imbsetup1.c", always "00" for JAX
	{"RecordCode", "", 2, "NCP10"},
//	{"TransactionSequenceNumber", "", 0},
//	{"RerunRestart", "", 0, "N"},
//	{"PullType", "", 0},
//	{"AccountSortKey", "", 0, "999999999"},
//	{"OrigCszOffset", "", 0, "00000"},
//	{"OrigCszLength", "", 0, "00000"},
	{"EndOfRecord", "", 2, "\r\n"}
};
char *ddImb700Path="/users/eric/imb700.dd";
char *ddNcpContainerPath="/users/eric/ncpcontainer.cbl.dd";
char *ddNcp10Path="/users/eric/ncp10.container.dd";

char scratch[8192];

int main(int argc, char *argv[])

{	char *s, *t, *InPath, *OutPath, *InBuffer, *OutBuffer;
   int InHandle, OutHandle, InLength, OutLength, ImbLength;
   int i, rCount, length, UseStack, StackNumber;
   convert_rec *v0, *v1, *w0, *w1, *wFound;
   int v0Count, w0Count, w0Limit;
   dd_rec *ddImb700, *ddInput, *ddNcp10;
   field_rec *f0, *f1, *fFound;
   int f0Count;

	printf("\n\n******* B E G I N   N C P C N T N R 3 *******\n\n");
	if (argc<2)
   {	printf("\n\nUsage is \"%s A\"\n\n", argv[0]);
      printf("\nA = Full path to input file");
      printf("\n\n");
      printf("\nE.g., \"%s /users/public/16860.4100.tab.fixed.grp", argv[0]);
      printf("\n\n");
   	exit(1);
   }
   InPath=strdup(argv[1]);
   InLength=4122;
   ImbLength=IMB_LENGTH;
   InBuffer=(char *)malloc(InLength);
   OutLength=NCP10_LENGTH;
   OutBuffer=(char *)malloc(OutLength);
   sprintf(scratch, "%s.container", InPath);
   OutPath=strdup(scratch);
   v0Count=sizeof(Convert)/sizeof(convert_rec);
   v0=Convert;
   w0Count=w0Limit=v0Count;
   w0=(convert_rec *)malloc(w0Limit*sizeof(convert_rec));
   memcpy(w0, v0, v0Count*sizeof(convert_rec));
   ddImb700=ddLoadFromFile(ddImb700Path);
   ddNcp10=ddLoadFromFile(ddNcp10Path);
   ddInput=ddLoadFromFile(ddNcpContainerPath);
   f0=ddInput->Field;
   f0Count=ddInput->FieldCount;
   for(f1=f0; f1-f0<f0Count; f1++)
   {	f1->Offset+=ImbLength;
   	// check to make sure the field isn't explicitly mapped in w0 array
   	for(w1=w0, wFound=NULL; w1-w0<w0Count && wFound==NULL; w1++)
      	if(w1->Source==3
         && strcmp(w1->FromName, f1->FieldName)==0)
         	wFound=w1;
      if(wFound==NULL)
      {	for(w1=w0; w1-w0<w0Count && wFound==NULL; w1++)
	      	if(strcmp(w1->ToName, f1->FieldName)==0)
   	      	wFound=w1;
      	// if not the following special field names, then map it assuming
         // ToName in ncp10 equals FromName in ncpcontainer
      	if(wFound==NULL
	      && strcmp(f1->FieldName, "FILLER")!=0)
//         && strcmp(f1->FieldName, "NcpOptionInfo")!=0)
//   	   && strcmp(f1->FieldName, "SecondaryKey")!=0
//      	&& strcmp(f1->FieldName, "SecondaryCount")!=0)
	      {	if(w0Count==w0Limit)
   	   	{	w0Limit+=100;
      	   	w0=(convert_rec *)realloc(w0, w0Limit*sizeof(convert_rec));
         	   memset(w0+w0Count, 0, 100*sizeof(convert_rec));
	         }
   	      w1=w0+w0Count;
      	   w0Count++;
         	w1->ToName=w1->FromName=f1->FieldName;
            w1->Source=3;
	      }
   	}
   }
   for(w1=w0; w1-w0<w0Count; w1++)
   {	w1->To=GetFieldRecByName(ddNcp10, w1->ToName);
   	if(w1->To==NULL)
      {	printf("\nA:  \"%s\" not found in \"%s\"", w1->ToName, ddNcp10Path);
      	exit(1);
      }
   	if(w1->Source==1)
	   {	w1->From=GetFieldRecByName(ddImb700, w1->FromName);
	   	if(w1->From==NULL)
   	   {	printf("\nB:  \"%s\" not found in \"%s\"", w1->FromName, ddImb700Path);
      		exit(1);
	      }
      }
   	else
      if(w1->Source==3)
      {	w1->From=GetFieldRecByName(ddInput, w1->FromName);
	   	if(w1->From==NULL)
   	   {	printf("\nC:  \"%s\" not found in \"%s\"", w1->FromName, ddNcpContainerPath);
      		exit(1);
	      }
      }
   }
   InHandle=OpenFile(InPath, "", IN, NULL);
   OutHandle=OpenFile(OutPath, "", OUT, NULL);
   rCount=0;
   UseStack=-1;
   while(read(InHandle, InBuffer, InLength)>0)// && rCount<10)
   {	rCount++;
   	if(UseStack==-1)
      {	if(memcmp(InBuffer+4094, "      ", 6)!=0)
	      	UseStack=1;
   	   else
      		UseStack=0;
      }
   	memset(OutBuffer, ' ', OutLength);
		for(w1=w0; w1-w0<w0Count; w1++)
      {	if(w1->Source==1 || w1->Source==3)
      	{	if(strcmp(w1->To->FieldName, "RecordIndex")==0)
         	{	memcpy(&i, InBuffer+w1->From->Offset, sizeof(int));
            	sprintf(scratch, "%0*d", w1->To->Length, i+1);
            	memcpy(OutBuffer+w1->To->Offset, scratch, strlen(scratch));
            }
            else
         	{	length=w1->To->Length;
		      	if(w1->From->Length<w1->To->Length)
   		      	length=w1->From->Length;
               if(w1->To->FieldFormat==NULL || strcmp(w1->To->FieldFormat, "RightJustify")!=0)
	      			memcpy(OutBuffer+w1->To->Offset, InBuffer+w1->From->Offset, length);
               else
               {	sprintf(scratch, "%.*s", w1->From->Length, InBuffer+w1->From->Offset);
               	for(s=scratch+strlen(scratch)-1; s>=scratch && *s==' '; s--)
                  	*s=0;
						sprintf(scratch+1000, "%*s", w1->To->Length, scratch);
						memcpy(OutBuffer+w1->To->Offset, scratch+1000, w1->To->Length);
               }
         	}
         }
			else
         if(w1->Default!=NULL)
         	memcpy(OutBuffer+w1->To->Offset, w1->Default, strlen(w1->Default));
      }
      // special logic for StackNumber, offset 4094 for 6 bytes in the 4122
      if(UseStack)
      {	sprintf(scratch, "%.6s", InBuffer+4094);
      	StackNumber=atoi(scratch);
         sprintf(scratch, "%09d", StackNumber);
         memcpy(OutBuffer+90, scratch, 9);
      }
      write(OutHandle, OutBuffer, OutLength);
   }
   close(InHandle);
   close(OutHandle);
   return(0);
}

/*	NCP10 fields appear below sorted by Source value
   0 - further work required for some of these fields
   1 - all these come from specific fields in the IMB700
   2 - these fields have specific names in the "imb.dd" file from which the
       IMB700 is built.  The identification of input fields to be mapped
       to these specific names could occur in the existing WINDOWS version
       of TAB-TO-FIXED logic.

	{"FileNumber", "", 0, "01"},
	{"RecordCode", "", 0, "NCP10"},
	{"TransactionSequenceNumber", "", 0},
	{"OriginalTrackingId", "", 0, "000000001"},
   	- FILE ID found in the FILES RECEIVED table for JAX?
	{"TrackingId", "", 0, "000000001"},
   	- FILE ID found in the FILES RECEIVED table for JAX?
	{"JobNumber", "", 0},
	{"SequenceNumber", "", 0},
	{"HouseholdingSeq", "", 0, "0"},
	{"HouseholdingMax", "", 0, "0"},
	{"PrintDelivery", "", 0, "Y"},
	{"ElectronicDeliveryMethod", "", 0, "N"},
	{"OrderType", "", 0, "A"},
	{"RerunRestart", "", 0},
	{"PullType", "", 0},
	{"EditRejectIndicator", "", 0, "0000"},
	{"EditRejectMessage", "", 0},
	{"OptionNumber", "", 0, "opti"},
	{"ProductNumber", "", 0, "prod"},
	{"NumberOfPhysicalPages", "", 0},
	{"NumberOfLogicalPages", "", 0},
	{"Language", "", 0, "English"},
	{"Country", "", 0},
	{"RemitAddressLine1", "", 0},
	{"RemitAddressLine2", "", 0},
	{"RemitAddressLine3", "", 0},
	{"RemitAddressLine4", "", 0},
	{"RemitAddressLine5", "", 0},
	{"ImbBarcodeIDRemit", "", 0},
	{"ImbServiceTypeIDRemit", "", 0},
	{"ImbMailerIDRemit", "", 0},
	{"ImbSerialNumberRemit", "", 0},
	{"ImbZip5Remit", "", 0},
	{"ImbZip4Remit", "", 0},
	{"ImbDeliveryPointRemit", "", 0},
   {"EncodedImbRemit", "", 0},
	{"OriginMailTrackingBar", "", 0},
	{"NCOAErrorCode", "", 0},
	{"Pocket8ActiveBit", "", 0},
	{"Pocket9ActiveBit", "", 0},
	{"Pocket10ActiveBit", "", 0},
	{"Pocket11ActiveBit", "", 0},
	{"Pocket12ActiveBit", "", 0},
	{"FeedAssurance", "", 0},
	{"SSN", "", 0},
	{"StatementDate", "", 0},
	{"PaymentDueDate", "", 0},
	{"PaymentAmount", "", 0},
	{"LatePaymentDueDate", "", 0},
	{"LatePaymentAmount", "", 0},
	{"EmailAddress", "", 0},
	{"PrintProgramName", "", 0},
	{"OnlineViewProductNumber", "", 0},
	{"OrigCszOffset", "", 0, "00000"},
	{"OrigCszLength", "", 0, "00000"},
	{"2DRecordCode", "", 0},
	{"2DCustomArea", "", 0},
	{"EndOfRecord", "", 0, "\r\n"}

	{"RecordIndex", "RecordIndex", 1},
	{"Account", "Account", 1},
	{"ImbBarcodeIDMailer", "ImbServiceType", 1},		// first 2 bytes go here
	{"ImbMailerID", "ImbId", 1},							// last 3 bytes go here
	{"ImbSerialNumber", "ImbSequence", 1},
	{"ImbZip5Mailer", "Zip5", 1},
	{"ImbZip4Mailer", "Zip4", 1},
	{"ImbDeliveryPointMailer", "Dpbc", 1},
   {"EncodedImbMailer", "PostalCodes", 1},
	{"PrintedAddressLine1", "Address1", 1},
	{"PrintedAddressLine2", "Address2", 1},
	{"PrintedAddressLine3", "Address3", 1},
	{"PrintedAddressLine4", "Address4", 1},
	{"PrintedAddressLine5", "Address5", 1},
	{"PrintedAddressLine6", "Address6", 1},
	{"State", "State", 1},
	{"Zip5", "Zip5", 1},
	{"Zip4", "Zip4", 1},
	{"CASSErrorCode", "PostalError", 1},
	{"Height", "Height", 1},
	{"Width", "Width", 1},
	{"Thickness", "Thickness", 1},
	{"PostalWeight", "Weight", 1},
	{"PostalRate", "Rate", 1},
	{"EndorsementLine", "", 0},
	{"Pocket1ActiveBit", "", 0},
	{"Pocket2ActiveBit", "Pocket2", 1},
	{"Pocket3ActiveBit", "Pocket3", 1},
	{"Pocket4ActiveBit", "Pocket4", 1},
	{"Pocket5ActiveBit", "Pocket5", 1},
	{"Pocket6ActiveBit", "Pocket6", 1},
	{"Pocket7ActiveBit", "Pocket7", 1},
	{"FormattedAccount", "FormattedAccount", 1},

	{"FlexField1", "UserLLDB1", 2, "", 1},
	{"FlexField2", "UserLLDB2", 2, "", 1},
	{"FlexField3", "UserLLDB3", 2, "", 1},
	{"FlexField4", "UserLLDB4", 2, "", 1},
	{"FlexField5", "UserLLDB5", 2, "", 1},
	{"FlexField6", "UserLLDB6", 2, "", 1},
	{"OriginalAddressLine1", "Address1", 2},
	{"OriginalAddressLine2", "Address2", 2},
	{"OriginalAddressLine3", "Address3", 2},
	{"OriginalAddressLine4", "Address4", 2},
	{"OriginalAddressLine5", "Address5", 2},
//	{"OriginalAddressLine6", "Address6", 2},
	{"OriginalCity", "City", 2},
	{"OriginalState", "State", 2},
	{"OriginalZip5", "Zip5", 2},
	{"CertifiedMailArticleNumber", "CertNum", 2, "", 1},
*/

