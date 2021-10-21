ABNielsen.com 6502 Single Board Computer
This is a 6502-based complete single board computer intended to help during the global IC shortage. Why use new chips when you can reuse scrap?

Check out the project on HackADay if you want to read more and see some pretty pictures :)

https://hackaday.io/project/181897-abnielsencom-6502-sbc

Build instructions:

1) Send gerber files from /hardware to your favorite board house
2) Build the project using assemble.sh - this will also try to burn the main ROM using Minipro. Dependencies: [CC65] (https://github.com/cc65/cc65), the Padauk IDE (working on replacing the PMS171b with 7400 logic) and a way to burn the ROM's.
3) Burn the ROMs and PMS171B. I use a TL866II Plus variant for the actual ROMs and the Padauk official programmer for the PMS171B.
4) Source the IC's - the spirit of this project is to contribute as little as possible to the global IC shortage and get used chips from Ebay/AliExpress/etc.
5) Solder away
6) For the PS/2-keyboard I use Ben Eater's interface on a breadboard: https://www.reddit.com/r/beneater/comments/mjeilv/keyboard_interface_diagram/
7) Enjoy!

License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) https://creativecommons.org/licenses/by-nc/4.0/
