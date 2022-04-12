//This is a utility for wirelessly bootloading the ABNielsen.com 6502 SBC using an RF24 module of some sort.
//It is losely based on Daniel Beadle's nRF24L01 File Transfer - https://github.com/djbeadle/nRF24L01-File-Transfer
//Retransmit, checksum etc has been removed because the radio is reliable enough to detect bad packets - and we can
//simple restart if we  miss a packet. Max file size is 64k - but the ABNielsen.com SBC only has 16K of RAM available
//of which 2k is VRAM and 512b is zp+stack.
//If you have the RF24 library installed on a Raspberry Pi this will compile with: g++ -Ofast -Wall -pthread -I/usr/local/include/RF24/..  -L/usr/local/lib bootload.cpp -lrf24 -o bootload

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <RF24/RF24.h>
#include <unistd.h>

// For stat:
#include <sys/stat.h>
#include <sys/types.h>

// For signal handler:
#include <csignal>

// For stol:
#include <string>

using namespace std;

RF24 radio(22,0);

/****************** Linux (BBB,x86,etc) ***********************/
// See http://tmrh20.github.io/RF24/pages.html for more information on usage
// See http://iotdk.intel.com/docs/master/mraa/ for more information on MRAA
// See https://www.kernel.org/doc/Documentation/spi/spidev for more information on SPIDEV

/**************************************************************/

// Radio pipe addresses for the 2 nodes to communicate.
//const uint64_t pipes[2] = {0xE7E7E7E7E7LL, 0xC2C2C2C2C2LL};
const uint64_t pipes[2] = {0xC2C2C2C2C2LL,0xE7E7E7E7E7LL};

static volatile int interrupt_flag = 0;	// Catches Ctrl-c, for canceling transmisison

const int num_payload_bytes = 30; // 32 - 2 (header) - 1 (\0 at end) = 29 //No crc == 30
const int num_special_header_bytes = 2; // '\0' + some char
const int num_header_bytes = 2; // sizeof(uint16_t) = 2

void interrupt_handler(int nothing)
{
	cin.clear();
	cout << "Ctrl-c pressed! Ending transmission and truncuating file.\n";
	interrupt_flag = 1;
}

size_t getFilesize (const char* filename){
	struct stat st;
	if(stat(filename, &st) != 0){
		return 0;
	}
	return st.st_size;
}

int main(int argc, char** argv)
{
	signal(SIGINT, interrupt_handler); // Ctrl-c interrupt handler
	fstream *file;
	//uint8_t *packets; // buffer to store all of the packets
	char *filename = NULL;
	uint8_t *packets; // buffer to store all of the packets
	int errorcount = 0;
	int c;
	while ((c = getopt (argc, argv, "s:d:nmhD")) != -1)
	{
		switch (c)
		{
			case 'h':
				cout << "This is a bootloader utility for the the ABNielsen.com 6502 SBC to be used with a nRF24 radio!\n";
				cout << "It's built using TMRh20's C++ RF24 library, which can be found on Github:\n";
				cout << "https://github.com/nRF24/RF24";
				cout << "\n";
				cout << "Usage:\n";
				cout << "-h: Show this help text.\n";
				cout << "-s: The source file. Use this on the transmitter.\n";
				cout << "Examples:\n";
				cout << "sudo ./bootload -s codetobeloadedin6502RAM.bin \n";
				break;
			case 's': // Specify source file
				filename = optarg;
				break;

			case '?':
				fprintf (stderr, "Unknown option `-%c'.\n", optopt);
				return 6;
		}
	}
	// Open the file
	file = new fstream(filename, fstream::in);
	if(!file)
	{
		cout << "Could not open the file.\n";
		return 6;
	}

    radio.begin();
    radio.enableDynamicPayloads();
    radio.setRetries(15, 15);
    radio.setChannel(2);
    radio.setDataRate(RF24_2MBPS);

    radio.openWritingPipe(pipes[1]);
    radio.openReadingPipe(0, pipes[1]);
    radio.printDetails();

		// Send the very first packet with the filesize:
		uint8_t first[32];
		memset(&first, '\0', sizeof(first));
		first[1] = '1';
		uint32_t filesize = getFilesize(filename);
		if(filesize == 0)
		{
			cout << "Error: Will not transmit an empty file!\n";
			return 6;
		}

		if(filesize > 65536) {
			cout << "Error: 64k is maximum file size.\n";
                        return 6;
		}

		memcpy(first+2, &filesize, 2);
		cout << "Attempting to establish connection...";
		cout.flush();
		 while(interrupt_flag == 0)
		{
			if(radio.write(first, sizeof(first)) == false)
			{
				 cout << "Sending first packet failed.\n";
				 errorcount++;
				 if (errorcount > 10) {
				cout << "Error: Too many retries.\n";
				return 7;
				}

			}
			else break;
		}
		int i;
		for (i = 0;i<32; i++) {
		printf("%02x ", first[i]);
		}

		if(interrupt_flag == 0)
		{
			cout << "Success!\n";
		}
		else
		{
			cout << "Attempt to establish a connection was canceled by the user.\n";
			return 6;
		}

		// Calculate the number of pkts we're TX'ing
		uint16_t total_num_pkts = filesize / num_payload_bytes;
		// If filesize is not exactly divisible by
		// num_payload_bytes we need an extra packet
		if (filesize % num_payload_bytes != 0)
			total_num_pkts += 1;
		printf("Filesize: %d\n", filesize);
		printf("Total Number of Packets: %d\n", total_num_pkts);
		// Initalize the array we'll be transmitting
		char packet[32];
		memset(&packet, '\0', 32);

		// Things we'll need later:
		int eof = 0;
		uint16_t special_ctr = 1; // This IDs the data pkts. Starts at 1, 0 is reserved for special packets

		// Store all the packets in a buffer addressed by special_ctr
		// TODO: don't store entire file in memory, instead use fseek
		packets = (uint8_t*)malloc(num_payload_bytes * (total_num_pkts+1));

		cout << "Beginning Transmission.\n";
		// Read the entire file and store it into the packets array

		while(eof==0 && interrupt_flag == 0)
		{
			// Transmit normal data packets
			memset(packet, '\0', 32);
			memcpy(packet, &special_ctr, 2);
			for(int i = 0 + num_header_bytes; i < 32; i++)
			{
				file->get(packet[i]);
				if(!*file)
				{
						cout << "Hit EOF!\n";
					eof = 1;
					break;
				}
			}
		memcpy(packets + (num_payload_bytes * special_ctr), &packet[num_header_bytes], num_payload_bytes);

		int x;
		for (x = 0;x<32; x++) {
                printf("%02x ", packet[x]);
                };
		int y;
                for (y = 2;y<32; y++) {
                if(packet[y] != 0x0a) printf("%c", packet[y]);
                };

		cout << "\n";


			if(radio.write(&packet, 32))
			{
					cout << "  Sent!\n";
					usleep(40000); //This can be waaaaaaay smaller. We're not aiming for performance here though.
					special_ctr++;

			} else	{
					cout << "  Failed. Restarting...\n";
					sleep(1);
					radio.write(first, sizeof(first));
					special_ctr = 1;
					file->seekg(0);
			}

		}
		// Send the very last packet:
		//uint8_t last[32];
		//memset(&last, '\0', sizeof(last));
		//radio.write(&last, sizeof(last));
		free(packets);

}
