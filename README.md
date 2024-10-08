# ABNielsen.com 6502 Single Board Computer R1

This is a 6502-based complete single board computer intended to help during the global IC shortage. Why use new chips when you can reuse scrap?

## Update! The newest ABN6502 SBC R1 board is available as a kit with gold plated (ENIG) PCB and (optionally) SMD components presoldered, limiting soldering to though hole components/sockets and configuration jumpers: https://www.imania.dk/index.php?currency=EUR&cPath=204&sort=5a&language=en

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

1) Send gerber files from /hardware to your favorite board house (or get a board from [my store](https://www.imania.dk/index.php?currency=EUR&cPath=204&sort=5a&language=en)) 
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
