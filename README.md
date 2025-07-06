# ABNielsen.com 6502 Single Board Computer Revision 2

This is a 6502-based complete single board computer intended to help during the global IC shortage. Why use new chips when you can reuse scrap?

## Update Update! The newest ABN6502 SBC R2 board is available as:
- Blank PCBs 
- Fully mounted PCBs including sockets and headers
- Kits with SMD components presoldered, limiting soldering to though hole components/sockets. 

All gold plated (ENIG) PCBs: https://www.imania.dk/index.php?currency=EUR&cPath=204&sort=5a&language=en

Coming soon: R2 video. 

Complete hardware overview of R1:

<a href="http://www.youtube.com/watch?feature=player_embedded&v=w5cA64xof2I" target="_blank">
 <img src="http://img.youtube.com/vi/w5cA64xof2I/mqdefault.jpg" alt="Watch the video" width="240px" height="auto" border="10" />
</a>

Video about the wireless bootloader:

<a href="http://www.youtube.com/watch?feature=player_embedded&v=NABU7gQDtcs" target="_blank">
 <img src="http://img.youtube.com/vi/NABU7gQDtcs/mqdefault.jpg" alt="Watch the video" width="240px" height="auto" border="10" />
</a>

Check out the NEW project on HackADay if you want to read more and see some pretty pictures :)

https://hackaday.io/project/184725-abn6502-sbc-r1

For R1 I got rid of the Padauk MCU without increasing the chip count!

# Build instructions:

1) Send gerber files from /hardware to your favorite board house (see "Getting a PCB" below) (or get a board from [my store](https://www.imania.dk/index.php?currency=EUR&cPath=204&sort=5a&language=en)) 
2) Build the firmware using assemble.sh (macOS/Linux) - this will also try to burn the main ROM using Minipro. Dependencies: [CC65] (https://github.com/cc65/cc65) and a way to burn the ROM's.
3) Source the IC's - the spirit of this project is to contribute as little as possible to the global IC shortage and get used chips locally or from Ebay/AliExpress/etc.
4) Burn the ROMs. I use a TL866II Plus variant for the actual ROMs.
5) Solder away. If you can solder 0805 components(poorly), you should be fine. Some optional components (the microUSB connector, the 3v3 regulator, and the RF-module) require a bit more skill.  
6) For the PS/2-keyboard I use the 6522's Shift Register, PB6, and a 74xx74 flip flop.  See my R1 Hardware overview at 7:46: https://youtu.be/w5cA64xof2I?t=466 This is now included on the board - instead of a PS/2 connector which can be harder to source I went with a USB-A-connector. Keyboard plugs straight into the board. 
7) Enjoy!

Optional:
Setup SSH keys with a Raspberry Pi, connect an nRF24L01+ compatible module to it and compile the wireless bootloader code in /software_linux

Also optional:
Microsoft BASIC can be enabled by cloning the msbasic Github repo into the same folder you cloned this repo.

> git clone https://github.com/mist64/msbasic.git

Then uncomment ;BASIC := 1 (Remove the ";") and run assemble.sh - this will let you run BASIC by pressing F4.
To change settings for BASIC, values can be changed in defines_abn6502.s

Also, please note that the current code assumes VRAM starts at $0800 rather than the default $2000 in the schematic - resistors will have pads for configuring in the next build, but for now it requires jumpers.

License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) https://creativecommons.org/licenses/by-nc/4.0/

## Getting a PCB
This project is kindly sponsored by JLCPCB. They offer cheap, professional looking PCBs and super fast delivery.

Step 1: Get the gerber file zip package from the /hardware/jlc folder
[hardware/jlcpcb/production_files/GERBER-6502.zip](hardware/jlcpcb/production_files/GERBER-6502.zip) for instance.

Step 2: Upload to JLCPCB [https://jlcpcb.com/?from=Anders_N](https://jlcpcb.com/?from=Anders_N)

<img src="https://github.com/AndersBNielsen/65uino/blob/main/images/upload.png?raw=true" alt="Upload" style="width: 220px;">

Step 3: Pick your color, surface finish and order.

<img src="https://github.com/AndersBNielsen/65uino/blob/main/images/settings.png?raw=true" alt="Select settings" style="width: 220px;">

<img src="https://github.com/AndersBNielsen/65uino/blob/main/images/save.png?raw=true" alt="Save your choice" style="width: 220px;">


You can use these affiliate links to get a board for $2 and also get $54 worth of New User Coupons at: https://jlcpcb.com/?from=Anders_N

And in case you also want to order a 3D-printed case you can use this link. 
How to Get a $7 3D Printing Coupon: [https://3d.jlcpcb.com/?from=Anders3DP](https://jlc3dp.com/?from=Anders_N)

