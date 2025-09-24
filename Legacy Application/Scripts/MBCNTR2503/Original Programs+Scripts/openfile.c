/* ******************************************

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>

#define IN 0
#define OUT 1

struct magtapeop {
	short mt_op;
	daddr_t mt_count;
} mtop;

char *MakeUnixPath(char *fname, char *path);

int OpenFile(char *fname, char *path, int io, char *FileType)

{	char StandardLabels[3], *FilePath, filetype;
	int afile;

	if(strcmp(fname, "TAPE")==0)
   {  FilePath = strdup("/dev/rmt/7mn");
	   filetype = 'B';
   }
   else
   if(strcmp(fname, "CARTRIDGE")==0)
   {	FilePath = strdup("/dev/rmt/4mn");
	   filetype = 'B';
   }
   else
   if(strcmp(fname, "8MM")==0)
   {	FilePath = strdup("/dev/rmt/5mn");
	   filetype = 'B';
   }
   else
   if(strcmp(fname, "4MM")==0)
   {	FilePath = strdup("/dev/rmt/0mn");
	   filetype = 'B';
   }
   else
   {	FilePath = MakeUnixPath(fname, path);
	   filetype = 'D';
   }

   if(io==IN)
		afile = open(FilePath, O_RDONLY);
   else
   if(io==OUT)
		afile = open(FilePath, O_CREAT | O_RDWR | O_TRUNC,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (afile == -1)
	{	printf("\r\nFile open error %s", FilePath);
   	exit(1);
   }
	else
		printf("\r\nFile open successful %s", FilePath);

   if(filetype=='B')
   {	printf("\r\nStandard Labels (y/n)?  ");
   	scanf("%s", StandardLabels);
   }
	if (StandardLabels[0] == 'y' || StandardLabels[0] == 'Y')
	/* these 3 instructions forward past label */
	{	mtop.mt_op = MTFSF;
		mtop.mt_count = 1;
		ioctl(afile, MTIOCTOP, &mtop);
   }
   if(FileType!=NULL)
   	*FileType = filetype;
   return(afile);
}

int XenosOpenFile(char *fname, char *path, int io, char *FileType)

{	char StandardLabels[3], *FilePath, filetype;
	int afile;

	if(strcmp(fname, "TAPE")==0)
   {  FilePath = strdup("/dev/rmt/7mn");
	   filetype = 'B';
   }
   else
   if(strcmp(fname, "CARTRIDGE")==0)
   {	FilePath = strdup("/dev/rmt/4mn");
	   filetype = 'B';
   }
   else
   if(strcmp(fname, "8MM")==0)
   {	FilePath = strdup("/dev/rmt/5mn");
	   filetype = 'B';
   }
   else
   if(strcmp(fname, "4MM")==0)
   {	FilePath = strdup("/dev/rmt/0mn");
	   filetype = 'B';
   }
   else
   {	FilePath = MakeUnixPath(fname, path);
	   filetype = 'D';
   }

   if(io==IN)
		afile = open(FilePath, O_RDONLY);
   else
   if(io==OUT)
		afile = open(FilePath, O_CREAT | O_RDWR | O_TRUNC,
				S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (afile == -1)
	{//	printf("\r\nFile open error %s", FilePath);
//   	exit(1);
      return(afile);
   }
//	else
//		printf("\r\nFile open successful %s", FilePath);

   if(filetype=='B')
   {	printf("\r\nStandard Labels (y/n)?  ");
   	scanf("%s", StandardLabels);
   }
	if (StandardLabels[0] == 'y' || StandardLabels[0] == 'Y')
	/* these 3 instructions forward past label */
	{	mtop.mt_op = MTFSF;
		mtop.mt_count = 1;
		ioctl(afile, MTIOCTOP, &mtop);
   }
   if(FileType!=NULL)
   	*FileType = filetype;
   return(afile);
}

