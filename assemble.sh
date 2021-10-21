#!/bin/bash
ca65 --cpu 6502 -l  build/listing.txt -o  build/abn6502rom.o abn6502rom.s
ca65 crom.s -o build/crom.o
ld65 -o build/abn6502rom.bin -C memmap.cfg "./build/abn6502rom.o" "./build/crom.o"
minipro -s -p "SST39SF010A" -w  build/abn6502rom.bin
