typedef struct s_hexchar {
	unsigned char *b0;
   int b0Count, b0Limit;
} hexchar_rec;

typedef struct s_barcodemap {
	short Char;
   short Bit;
} barcodemap_rec;

static char *m0[] = {
	"H2E3",
	"B10A0",
	"J12C8",
	"F5G11",
	"I9D1",
	"A1F12",
	"C5B8",
	"E4J11",
	"G3I10",
	"D9H6",
	"F11B4",
	"I5C12",
	"J10A2",
	"H1G7",
	"D6E9",
	"A3I6",
	"G4C7",
	"B1J9",
	"H10F2",
	"E0D8",
	"G2A4",
	"I11B0",
	"J8D12",
	"C6H7",
	"F1E10",
	"B12G9",
	"H3I0",
	"F8J7",
	"E6C10",
	"D4A5",
	"I4F7",
	"H11B9",
	"G0J6",
	"A6E8",
	"C1D2",
	"F9I12",
	"E11G1",
	"J5H4",
	"D3B2",
	"A7C0",
	"B3E1",
	"G10D5",
	"I7J4",
	"C11F6",
	"A8H12",
	"E2I1",
	"F10D0",
	"J3A9",
	"G5C4",
	"H8B7",
	"F0E5",
	"C3A10",
	"G12J2",
	"D11B6",
	"I8H9",
	"F4A11",
	"B5C2",
	"J1E12",
	"I3G6",
	"H0D7",
	"E7H5",
	"A12B11",
	"C9J0",
	"G8F3",
	"D10I2"};

void Multiply(hexchar_rec *h, unsigned char d);
void Add(hexchar_rec *h, unsigned char d);
unsigned short Divide(char *s0, int Divisor, char *Quotient);
unsigned short USPS_MSB_Math_CRC11GenerateFrameCheckSequence( unsigned char *ByteArrayPtr );
int InitializeNof13Table( int *TableNof13, int N , int TableLength );

extern unsigned short BitIsOn[13];
extern int Table5of13[1287];
extern int Table2of13[78];
extern barcodemap_rec *a0, *d0;
extern int a0Count, d0Count, m0Count;


