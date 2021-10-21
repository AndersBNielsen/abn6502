
#include	"extern.h"


//				P#, VDD, PA0,  PA3, PA4, PA5, PA6, PA7, GND, MASK1, MASK2, SHIF#
.writer package	16,	  5, 11,	9,	 10,  8,	7,	6,	 12, 0x00, 0x00, 0

VSYNC	BIT PA.4;
//HSYNC	BIT PA.4;
//HBLANK  BIT	PA.6;
VBLANK  BIT PA.7;
A13		BIT PA.3

byte mode = 0;
word line = 0;
word count = 0;

void	FPPA0 (void)
{
//	.ADJUST_IC	SYSCLK=IHRC/2, IHRC=15734375Hz, Vdd=5.0V		//	SYSCLK=IHRC/2
	.ADJUST_IC	SYSCLK=IHRC/2, IHRC=16000KHz, Vdd=5.0V		//	SYSCLK=IHRC/2

	//	clkmd.En_Reset	=	0;			//	PA.5 as Reset pin

	CLKMD = 0x35;
	//	Insert Initial Code

	paph.5 = 1; //Does this work?

	$ VSYNC HIGH, OUT;
	$ VBLANK HIGH, OUT;
	$ A13 LOW, OUT;

	PBC = 0xFF;
	PB = 0;

	line = 0;
	$ INTEN PA0;

		INTEGS = 0b0;
	INTRQ = 0;
	ENGINT;


	while (1)
	{
    if (line.8 == 1){
		A13 = 1;
		} else
			{
					A13 = 0;
			}
	if (line == 490) {
	$ VSYNC LOW;
		}

	if (line == 492) {
	$ VSYNC HIGH;
	}


	if (line == 524) {
	line = 0;
		   }

} //While 1
} //FPPA0

void	Interrupt (void)
{
	pushaf; //We don't care about flags - only timing... Or maybe we do?

	if (Intrq.PA0)
	{
		line++;
		PB = line;
		Intrq.PA0	=	0;

	}

	popaf;
}
