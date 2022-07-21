
.feature string_escapes ; Allow c-style string escapes when using ca65
;PRIMM = $FFC8 ; Userland can use ROM subroutines if we tell them where they are. Check listing.txt for current subroutine addresses
;.require "abn6502rom.s"
.import scrp: zeropage, scwp: zeropage
.autoimport + ; Try to import anything unknown from other modules

PORTB = $6000 ; PB0: SCK/SCL, PB1: RF CS, PB2: RF CE, PB3: SDA, PB4,PB5: MISO ,PB6: PS/2 Clock In, PB7: MOSI/T1 Out (Tape drive output)
PORTA = $6001
DDRB = $6002
DDRA = $6003
T1CL = $6004
T1CH = $6005
T1LL = $6006
T1LH = $6007
T2CL = $6008
T2CH = $6009
SR1   = $600A
ACR  = $600B ; [7] PB.7 T1 OUT, [6] T1 mode , [5] T2, [4:2] Shift register control, [1] PB Latch enable, [0] PA Latch Enable
PCR  = $600C ; [7:5] CB2 Control, [4] CBl Control, [3:1] CA2 Control, [0] CAl Control
IFR  = $600D ; [7:0] IRQ Tl T2 CBl CB2 SR CA1 CA2
IER  = $600E ; [7:0] S/C Tl T2 CBl CB2 SR CA1 CA2

MILLIS = $40

SVP = $42 ; Save pointer
SVPH =$43

TMP = $50;
TMP2 = $51;

kb_wptr = $00
kb_rptr = $01
ESC_KEY  =  $1b

.segment "USERLAND"
userland:
  lda #$0A
  jsr printk
  jsr PRIMM
  .asciiz "Testing CPU... \n"
cOhtwotest:
  bra see02
  jsr PRIMM
  .asciiz "Hi! Im an NMOS 6502!  \n"
  jmp exitnow
  see02:
  jsr PRIMM
  .asciiz "Hi! Im a CMOS 65C02! \n\n"
  exitnow:

brk
