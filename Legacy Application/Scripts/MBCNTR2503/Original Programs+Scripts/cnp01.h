#define IN 0
#define OUT 1
#define IO 2
#define EXIT_ON_FAILURE 1
#define NO_EXIT_ON_FAILURE 0
#define MAX_BLOCK_READ 100000

static char *ddDefaultPath="/users2/letters/dd/aztest/";

static char *DataTypes[] = {
	"Text",
   "Number",
   "Packed Number",
   "Double",
   "Float",
   "Int",
   "Long Int",
   "Short Int",
   "Mixed",			// can be either Packed Number or Text
   NULL
};

static char Numerics[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
								  '-', '.', 0};

static char *Operators[] = {"=", "<", ">", "<=", ">=", NULL};

typedef struct parse_s {
   char *String;
   int Type;              /* 0-text, 1-field, -1-error */
} parse_rec;

typedef struct string_s {
	char **u0;
   int u0Count;
   int u0Limit;
} string_rec;

typedef struct comm_s {
	char *Path;                                     // communications file path
	int Handle;                                     // handle to communications file
   long UpdateSeek;                     // offset in file to unix update region
   int CancelOffset;                    // offset in file to windows cancel byte
   int Cancelled;                               // cancel button pressed
   int Stopped;                         // stop button pressed
   int Test;                                    // testing a new version of filesplit, cnpxerox, ...
   char *ParseThisString;       // portion of comm file to be parsed
   parse_rec *p0;                               // results of parsed comm file
   int ArgCount;                                // expected number of strings parsed from comm file
   int InputFileLength;
   int InputReadSum;
   int InputReadCount;
   int InputPctDone;
   char *Error;
} comm_rec;

typedef struct s_field {
   char *FieldName;
   int Offset;
   int Length;
   int DataType;
   int Decimals;
   char *FieldFormat;
   char *ValueToPrint;
} field_rec;

typedef struct fieldprops_s {
   short IsNumber;
   char *fName;
   int fOffset;
   int fLength;
   int fDecimals;
   int fDigits;
   int Minus;
   char *fBegin, *fEnd, *dPoint;
} fieldprops_rec;

/*
typedef struct oper_s {
   char *cFieldValue;
   double *dFieldValue;
   int Oper;    // 0 =, 1 <, 2 >, 3 <=, 4 >=, 5 (, 6 )
   char *cCompValue;
   double dCompValue;
   int LogicalNot;              // 0-ok, 1-NOT
   int NextConnector;   // 1-AND, 2-OR, 5-(, 6-)
   int ParenCount;
   int ret;                                     // return value from processing of oper->Next
   struct oper_s *Next;
} oper_rec;*/

typedef struct account_s {
	char *AccountsPath;
   int FileType;
   int RecLen;
   int FieldOffset;
   int FieldLength;
   field_rec *iField;
   field_rec *cField;
	char **Loan;
   int LoanCount;
   int LoanLimit;
   int LoanSize;
   int LoanCompLength;
   int Packed;
   int Alltel;
   int ComparingNumbers;
//   oper_rec *op0;
//   int op0Count, op0Limit;
   comm_rec *c0;
} account_rec;

typedef struct dd_s {
	char *Path;
   field_rec *Field;
   int FieldCount, FieldLimit;
} dd_rec;

typedef struct cobolfiletable_s {
   int RecordLength;
   char *ddPath;
   int RecordCount;
   int RecordLimit;
   char *ReadBuffer;
} cobolfiletable_rec;

typedef struct result_s {
   int DataType;		// 0-Text, 3-Double, 5-Int
   double dValue;
   int iValue;
   char *sValue;
   void **d0;
   int d0Count, d0Limit;
} result_rec;

typedef struct s_calc {
	int CalcType;			// 0-Date, 1-Minimum, 2-Maximum, 3-Compute
   int mmAdjust;
   int ddAdjust;
   int yyAdjust;
   int mmSet;
   int ddSet;
   int yySet;
} calc_rec;

typedef struct filetable_s {
	char *Path;
   char *FileName;
   int RecordLength;
   char *ddPath;
   int RecordCount;
   char *ReadBuffer;
   dd_rec *dd;
} filetable_rec;

typedef struct s_fieldcalc {
	field_rec *fr;
   calc_rec *calcr;
   filetable_rec *ft;
   result_rec *Result;
} fieldcalc_rec;

typedef struct s_text {
	fieldcalc_rec fcr;
   char *FormatString;
   char *text;
} text_rec;

typedef struct s_paragraph {
   char *Path;		// paragraph template path
   char *Font;		// font in which this paragraph appears
   double Width;	// width in inches of this paragraph on the printed page
   text_rec *t0;
   int t0Count, t0Limit;
} paragraph_rec;

typedef struct value_s {
	char *FieldName;
   field_rec *fr;
   char *sValue;
   int iValue;
   double dValue;
   short IsInvalid;
} value_rec;

typedef struct file2_s {
	char *PathHint;			// can be set to CARTRIDGE, TAPE, 4MM, 8MM
	char *Path;					// must be valid unix path unless PathHint is set
   int Handle;					// file handle
   char FileType;				// will be set to 'B' if input is a block device
   int CheckStandardLabels;// 1-get standard labels (y/n) from console
   char *StandardLabels;	// y-standard labels, n-no
   int RecordLength;			// record length
   int BufferAllocated;		// 1-true
   char *Buffer;				// read/write buffer
   char *BlockBuffer;		// buffer allocated for physical read/write on block devices
	char *BlockRecord;		// pointer to current record within BlockBuffer
   int BlockLength;			// length of current block
   char *Merge;				// merge field
   char *MergeSave;			// previous value of merge field
   int MergeOffset;			// offset in record to merge field
   int MergeLength;			// length of merge field
   int ReadResult;			// number of bytes read
   int ReadCount;				// number of records read
   int WriteResult;			// number of bytes written
   int WriteCount;			// number of records written
   int CheckSequence;		// 1-check that merge field is in ascending order (hex)
   int CheckDuplicates;		// 1-check for duplicate merge field values
   int DuplicateResult;		// 1-duplicate detected
   int NeedPreviousRecord;	// 1-save a copy of the previous record
   char *PreviousRecord;	// previous record buffer
   int OpenRecordTotal;		// # records in file at open (disk only)
   int CloseRecordTotal;	// # records in file at close (any file)
} file2_rec;

typedef struct address_s {
	char Add1[100];
   char Add2[100];
   char Add3[100];
   char City[100];
   char State[100];
   char Zip5[100];
   char Zip4[100];
   char AddressScratch[700];
   char AddressSave[700];
} address_rec;

typedef struct parm_s {
	void *Parm;
   int ParmType;	// 0-string, 1-field, 2-dd
} parm_rec;

typedef struct func_s {
	int Function;	// 1-CAT()  2-ToAddress()
	parm_rec *r0;
   int ParmCount;
} func_rec;

char dehexify(char c);
void packit(unsigned char *unpack, unsigned char *pack);
	// pack must be at least (strlen(unpack)+2)/2 bytes
void unpackit(unsigned char *pack, unsigned char *unpack, int plen);
	// unpack must be at least plen*2+1 bytes
int FieldIsPacked(char *s0, int Len);
	// checks last half byte of s0 for >='0' && <='9'
   // this works for well-defined packed / unpacked numeric fields
void asc2ebc(char *to, char *from, int copylen, int ispacked);
void ebc2asc(char *to, char *from, int copylen, int ispacked);
	// ispacked values:  0-text, 1-packed, 2-unpacked signed numeric
void AccountList(char *Account, int AccountLength, account_rec *a0);
void AccountListExtract(char *Account, int AccountLength, account_rec *a0, int cFileType, int cOffset, int cLength);
void ConvertIntToPacked(int Amount, char *Packed, int Length);
	// Length represents length in bytes of Packed
int ConvertPackedToInt(unsigned char *pack, int plen);
	// plen represents length in bytes of pack
void ConvertUnsignedToPacked(unsigned int Amount, char *Packed, int Length);
	// Length represents length in bytes of Packed
char *MakeUnixPath(char *path, char *directory);
	// path is relative to directory
	// directory is default directory
int OpenFile(char *fname, char *path, int io, char *FileType);
	// fname:  can be CARTRIDGE, TAPE
	// io values:  IN, OUT
   // FileType:  B=block device, D=disk
int XenosOpenFile(char *fname, char *path, int io, char *FileType);
	// same as OpenFile(), but returns -1 rather than exiting when open() fails
int CommOpenFile(char *fname, int io, comm_rec *c0);
int CommReadInputFile(int InHandle, FILE *fHandle, char *InBuffer, int InLength, comm_rec *c0);
parse_rec *ParseExpression(char *r, int *StringCount, char c, int UseQuotes, int UseParens, int UseEOR);
	// r:  string to parse
   // StringCount:  number of strings returned
   // c:  parse character
   // UseQuotes:  0-ignore, 1-characters within quotes are normal text
   // UseParens:  0-ignore, 1-characters within parentheses are parsed independently?
   // UseEOR:     0-ignore, 1-remove \r, \n from end of record
void GetCommRec(comm_rec *c0, char *FileName, int Count);
void CheckCancelByte(comm_rec *c0);
void CommUpdatePctDone(comm_rec *c0);
void CommUpdateInt(int x, comm_rec *c0);
void CommError(comm_rec *c0);
void ReverseInt(int x, char *s);
void RemoveParse(parse_rec *p0, int ParseCount);
field_rec *ddGetFields(char *ddPath, int *FieldCount, int *FieldLimit);
void SetOutput(char *In, char *Out, field_rec *fr, int Ascii);
int ConvertNumberToString(char *String, void *Number, int NumLength,
		int DataType, int Decimals);
void FormatNumberString(char *Result, char *sValue, char *FormatString);
dd_rec *ddLoadFromFile(char *ddPath);
void AddField(dd_rec *dd, parse_rec *p0, int ParseCount);
field_rec *GetFieldRecByName(dd_rec *dd, char *FieldName);
int ConvertNumberToComparisonString(char *String, char *Number,
	field_rec *f, int Ascii, comm_rec *c0, int cLength);
double LoadExpression(char *In, int *Stop);
void LogicalList(account_rec *a0);
void RoundIt(char *Result, char *Number, char *Basis, char Type);
int GetFieldDoubleValue(double *x, field_rec *fr, char *Buffer);
void ReadRecord(file2_rec *fr);
void WriteRecord(file2_rec *fr);
int OpenFile2(file2_rec *fr, int io, int ExitOnFailure);
file2_rec *CreateFileRecord(char *path, int RecordLength);
void SetFocusField(file2_rec *fr, int Offset, int Length);
void SetFileRecordFlags(file2_rec *fr, int CheckSequence, int CheckDuplicates, int NeedPreviousRecord, int CheckStandardLabels);
void CheckForStandardLabel(file2_rec *fr);
void SetStandardLabel(file2_rec *fr, char *label);
void CheckForSequence(file2_rec *fr);
void CheckForDuplicates(file2_rec *fr);
void NeedPreviousRecord(file2_rec *fr);
void UseSameBuffer(file2_rec *fr1, file2_rec *fr0);
int PercentDoneReading(file2_rec *fr);
void GetStringsFromParse(string_rec *sr, char *ParseString, char ParseChar);
void GetStringsFromFile(string_rec *sr, char *path);
int CanonicalDate(char *OutDate, char *InDate, int DateLength, char *InFormat);
void FormatDate(char *Out, char *In, char *FormatString);
int IsOriginCode(char *TrackingCode);
short CheckNumeric(char *s0);
short IsBlank(char *s0);
void FormatNumber(fieldprops_rec *r1, char *Number);
int ExtractAddressData(address_rec *a0);
void CopySourceToDestination(char *OutRec, field_rec *g, char *InRec, field_rec *f, int Ascii, char *scratch);
int JobPrefix(int Job5);

extern cobolfiletable_rec cftr0[];
extern paragraph_rec pgr0[];
extern int CobolFileTableCount, ParaTableCount;
extern char *LogFileRecordCounter, *LogFileName;
extern int LogFileHandle;
extern char *Format[];

