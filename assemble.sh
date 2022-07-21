#!/bin/zsh
ca65 -vv --cpu 6502 -l  build/listing.txt -o  build/abn6502rom.o abn6502rom.s
ca65 crom.s -o build/crom.o
ca65 userland.s --cpu 65c02 -l build/userlisting.txt -o build/userland.o
ld65 -o build/abn6502rom.bin -C memmap.cfg "./build/abn6502rom.o" "./build/crom.o" "./build/userland.o"
minipro -s -p "SST39SF010A" -w  build/abn6502rom.bin
#minipro -s -p "W27C512@DIP28" -w  build/abn6502rom.bin
#The lines below are used to copy the output to a Raspberry Pi and load it
ssh openhabian@openhabiandevice.local killall bootload
scp build/userland.bin openhabian@openhabiandevice.local:./

ssh openhabian@openhabiandevice.local bootload -s userland.bin
ssh openhabian@openhabiandevice.local killall bootload
