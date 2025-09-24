//                         Container Logic
//          This allows adding the following fileds to the NCP10 Record.
//                Print Product Code
//                View Product Code
//                Statement Date
// Programmer   : Daren Donovan
// Last Changed : 09/20/2013
// Early stage needs work on checking when fields are empy or bypassed.


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include </users/programs/imbrec2.h>

struct NCP10
{
	char filler1[307];
	char prodCode[4];
	char filler2[1600];
	char statementDate[10];
	char filler3[217];
	char viewCode[4];
	char filler4[80];
}temp;


FILE *input, *output, *cert;

char recType[8];
char record[10000];
char CMD_line[500];

//char name[50];
//char name2[15];

char file_nameout[500];
char file_namein[500];

long screen_count = 0, ScreenPrint = 0;
long recordLength;
int length;
long offSet;
int end_file = 0;
char prodCode[6];
char viewCode[6];
char statementDate[12];


main(argc, argv)
int argc;
char *argv[];
{
	char certType;
	time_t t;
	struct tm *gmt;
	t = time(NULL);
	gmt = gmtime(&t);

//	if(argc != 5)
	if(argc != 5 && argc != 4 && argc != 3)
	{
		printf("Please the Following Parameters To Add To NCP 10 Record.\n\n");
		printf("(1)  Input file Full Path\n");
		printf("(2)  Print Product Code\n");
		printf("(3)  View  Product Code\n");
		printf("(4)  Statement Date\n");
		printf("     (a) 'Y' Pulls system date as Statement Date\n");
		printf("     (b) 'dd/mm/yyyy' will use entered date as Statement Date\n");
		printf("     (c) ' N' Does not update the Statement Date\n");
		exit(-1);
	}
	else
	{
		sprintf(file_namein, argv[1]);
		length = strlen(argv[2]);
		if(length == 4)
			sprintf(prodCode, "%.4s", argv[2]);
		else
		{
			printf("No Print Product code will be Added.\n");
			prodCode[0] = '\0';
		}
		length = strlen(argv[3]);
		if(length == 4)
			sprintf(viewCode, "%.4s", argv[3]);
		else
		{
			printf("No View Product code will be Added.\n");
			viewCode[0] = '\0';
		}
		length = strlen(argv[4]);
		if(toupper(argv[4][0]) == 'Y')
		{
			sprintf(statementDate, "%02d/%02d/%d", gmt->tm_mon+1, gmt->tm_mday, ((gmt->tm_year - 100)+2000));
		}
		else
		{
			if(length == 10)
				sprintf(statementDate, "%10s", argv[4]);
			else
			{
				printf("No Statement Date will be Added.\n");
				statementDate[0] = '\0';
			}
		}
	}
//	sprintf(file_nameout, "/users/public/del1.del");
	sprintf(file_nameout, argv[1]);
	strcat(file_nameout, ".add");

	if((input = fopen(file_namein, "rt")) == NULL)
	{
		printf(" Input File could not be opened. = %s\n\n", file_namein);
		exit(-1);
	}
	if((output = fopen(file_nameout, "wt")) == NULL)
	{
		printf(" Output File could not be opened. = %s\n\n", file_nameout);
		exit(-1);
	}

	make_field();
	GetfileInfo(recordLength, input);


	while(end_file != 1)
	{
		if(ScreenPrint == 1000)
		{
			printf("\n\n\n\n\nAccounts Read      =  %06ld\n\n", screen_count);

			ScreenPrint = 0;
		}
		if(strncmp(recType, "NCP10", 5) == 0)
		{
			++screen_count;
			++ScreenPrint;
			strncpy(temp.filler1, record, strlen(record));
			if(strlen(viewCode) > 0)
				strncpy(temp.viewCode, viewCode,4);
			if(strlen(prodCode) > 0)
				strncpy(temp.prodCode, prodCode,4);
			if(strlen(statementDate) > 0)
				strncpy(temp.statementDate, statementDate,10);
			fprintf(output, "%s", temp.filler1);
		}
		else
			fprintf(output, "%s", record);
		make_field();
	}

	fclose(output);
	if((output = fopen(file_nameout, "rb")) == NULL)
	{
		printf(" Input File could not be opened. = %s\n\n", file_nameout);
		exit(-1);
	}

	Check_Input_Output_File_Size(input, output);

}

make_field()
{
	if(fgets(record, 10000, input) == NULL)
	{
		end_file = 1;
	}
	length = strlen(record);
	sprintf(recType, "%.5s", record+3);

}


GetfileInfo(long RecordSize, FILE *INPUT)
{
	ldiv_t divide;
	struct stat info;

	if(fstat(fileno(INPUT), &info) != 0)
	{
		printf("File Status information for %s could not be retreived.\n", file_namein);
		exit(-1);
	}
}

Check_Input_Output_File_Size(FILE *INPUT, FILE *OUTPUT)
{
	struct stat info_in, info_out;

	if(fstat(fileno(INPUT), &info_in) != 0)
	{
		printf("File Status information for %s could not be retreived.\n", file_namein);
		exit(-1);
	}

	if(fstat(fileno(OUTPUT), &info_out) != 0)
	{
		printf("File Status information for %s could not be retreived.\n", file_nameout);
		exit(-1);
	}

	if(info_in.st_size != info_out.st_size)
	{
		printf("%s is not the same size as %s\n", file_namein, file_nameout);
		printf("Do not Process This file. \n");
		printf(" < Hit Any Key >"); getchar();
		fclose(OUTPUT);
		sprintf(CMD_line, "mv %s %s.pending", file_nameout);
		system(CMD_line);
		exit(-1);
	}
	else
	{
		printf("%s is the same size an the new output file %s\n", file_namein, file_nameout);
		printf("File is good to process. \n");
	}

}
