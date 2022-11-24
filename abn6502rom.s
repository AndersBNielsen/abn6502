.feature string_escapes ; Allow c-style string escapes when using ca65
.feature org_per_seg
.feature c_comments

.export I2CADDR, I2CREG, i2c_read, i2c_test, main, kb_rptr, PRIMM, printk, printbyte, wkey, PORTB, TIMEOUT,exitirq, printa, newline, setpulses, scrp, scwp, simpledelay, selectbaudrate, MILLISH,resetkb, clrscn,checkkeyboard, kb_buffer, MONRDKEY,CRSRPNT, t2irqreg1, MILLIS

;BASIC := 1 ; 1 if BASIC is enabled
;DEBUG = 1
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
PORTANHS = $600F
CTRL = $7000

TIMEOUT = 7998 ; Should be around 2ms

RELEASE = %00000001
SHIFT   = %00000010
ECODE   = %00000100
CRSR    = %01000000
CRSRF   = %10000000
KTIMEOUT= %00100000
DBGFLAG = %00010000

kb_buffer = $0200  ; 128-byte kb buffer 0200-0280
scbuf     = $0280 ; Scan code buffer
USERLANDH = $03 ; RAM page userland code starts

; Screen constants
SCREENSTARTH = $08 ; Change this to the page your Video RAM starts! (VRAM at $0800 requires a few jumpers to change from the default $2000 - this will change in the next build of R1)
SCREENSTARTL = $4c ; Top left of screen - may differ between VGA screens
LINESTART = $0c
LINEEND = $3E
NUMLINES = 28

;Custom keyboard mappings
DN_ARR_KEY = $F3
UP_ARR_KEY = $F4
PGUP_KEY =   $F6
PGDN_KEY =   $F5
L_ARR_KEY = $F2
R_ARR_KEY = $F1
HOME_KEY = $F0
ESC_KEY  =  $1b
PRTSC_KEY = $EF
F12_KEY = $EC
F11_KEY = $EB
F10_KEY = $EA
F9_KEY = $E9

movedfromzp = scbuf+8
brkcnt = movedfromzp
RF_STS = movedfromzp+1
MONCNT = movedfromzp+2
TMP = movedfromzp+3;
TMP2 = movedfromzp+4
RF_ERR = movedfromzp+5
ERRS = movedfromzp+6
TMP3   = movedfromzp+7

ABUF = movedfromzp+8 ; 8 bytes
;to $2F
;ABUF = $28
TMP4 = movedfromzp+16
SVCSR = movedfromzp+17
;+1
I2CADDR = movedfromzp+19
I2CREG = movedfromzp+20

zp_s = $f8
kb_wptr = zp_s
kb_rptr = zp_s+1
kb_flags = zp_s+2
kb_last = zp_s+3
scwp = zp_s+4
scrp = zp_s+5
outb  = zp_s+6
inb = zp_s+7

zp_s2 = $e0
MONH = zp_s2+1
MONL = zp_s2
KEYBOARD = zp_s2+2
SPITMP = zp_s2+3
MILLIS = zp_s2+4
MILLISH = zp_s2+5
CRSRPNT = zp_s2+6
CRSRPNT2 = CRSRPNT+1

zp_s3 = $e8
SVP = zp_s3 ; Save pointer
SVPH =zp_s3+1
HXH = zp_s3+2
SLB =  zp_s3+3 ; Size low byte
SHB = zp_s3+4
CRSRCHR = zp_s3+5
PRIMMZP1 = zp_s3+6
PRIMMZP2 = zp_s3+7

zp_s4 = $f0
t2irqreg1 = zp_s4
t2irqreg2 = zp_s4+1
USERLANDP = zp_s4+2 ; JMP Pointer
USERLANDPH = zp_s4+3

DEBUGP = zp_s4+4
DEBUGPH = zp_s4+5

zp_s5 = $d8
SCRLPNT = zp_s5 ; also +1
SCRLPNT2 = zp_s5+2 ; also +3

; Tape constants
KCS300PL = 4 ; Kansas City Standard takes 4 1200 Hz pulses for a 0
KCS300PH = 8 ; and 8 2400Hz pulses for a 1
; Tape vars
TAPEVARS = movedfromzp+32
CBIT = TAPEVARS
RXBYTE = TAPEVARS+1
BITNO = TAPEVARS+1 ; Reusing
TAPEFLAGS = TAPEVARS+2
PULSES = TAPEVARS+3
NRZBYTE = TAPEVARS+4
onesinarow = TAPEVARS+5
plsperiod = TAPEVARS+6 ; also +7
PULSESL = TAPEVARS+8 ; Config variable if we do something other than KCS
PULSESH = TAPEVARS+9 ; ^^
plzpnt = zp_s4+6 ; also +7
CT1L = TAPEVARS+10
CT1H = TAPEVARS+11
LT1L = TAPEVARS+12
LT1H = TAPEVARS+13
LOADTIMEOUT = TAPEVARS+14

; CE = $69
MSGBUF = TAPEVARS+15
;+32bytes == To
RF24REGS = MSGBUF+32

uservia = PORTB
mosi  = %10000000
miso  = %00100000

SDA   = 8; PB3 bitmask
SDA_INV = $F7
SCL   = 16; PB4 bitmask
SCL_INV = $EF

.segment "RODATA"
;.org $8000 ; Not strictly needed with CA65 but shows correct address in listing.txt
.list on ; Does this work?
  nmi:
  reset:
          cld ; Because you never know

          ;CLEAR RAM
          sei ; In case this was not a hw reset
          ldx #$0
          lda #0
  clearzp:
          sta $00,x
          inx
          bne clearzp

          ;Assumes x and A are 0 from above
  clearstack:
          sta $0100,x
          inx
          bne clearstack
  clearp2:
          sta $0200,x
          inx
          bne clearp2
  clearp3:
          sta $0300,x
          inx
          bne clearp3

          ldx #$FF
          txs


noclear: ;Soft reset point - BRK

          jsr clrscn

          lda #$F1
          sta CTRL

          lda #SCREENSTARTL
          sta CRSRPNT
          lda #SCREENSTARTH
          sta CRSRPNT2

          ;lda #2
          ;sta PORTB ; Set SPI CS high
          lda #%10010111 ; Port B DDR for SPI
          sta DDRB

          lda #0
          sta DDRA ; Port A all inputs
          sta kb_rptr ; Init keyboard pointers before enabling interrupts
          sta kb_wptr
          sta RF_ERR ; Reset RF error
          ;cli    ; Enable interrupts
          lda #%11000000  ; Set T1
          sta IER
          LDA #%01000000
          STA ACR             ; T1 continuous, PB7 disabled

          LDA #<TIMEOUT
          STA T1CL            ; Set low byte of timer1 counter
          LDA #>TIMEOUT
          STA T1CH            ; Set high byte of timer1 counter

          ldx #$10 ; Read first tx addr byte = should be default, if not then no module connected
          stx kb_flags ; Debug enabled
          jsr rw_reg
          cmp #$E7
          bne norfmodule
          jsr initrf24
          bne welcome ; BRA

          norfmodule:
        ;debug, disable RF
          inc RF_ERR ; No module, set RF_ERR
welcome:
          jsr PRIMM
message:	.asciiz "GREETINGS PROFESSOR FALKEN.", "\n", "SHALL WE PLAY A GAME?", "\n"

welcomedone:
;Let's enable the keyboard
lda #$91
sta CTRL
jsr resetkb
lda #1
sta CTRL
cli

main: ; loop
lda RF_ERR
bne skiprf
bit ACR ; If ACR.7 is set then we can't use SPI since MOSI is outputting TM1.
bpl rfstuff
skiprf:
jmp nomsg
rfstuff:
;jsr readrf24regs ; Debug - we can also just do an rf_nop to read rf24 status (RF_STS)
jsr rf_nop ; Not debug
;If msg received, put it in MSGBUF
bit RF_STS
bvs gtgm ; Check irq
lda RF_STS
cmp #$0e ; Check if fifo empty
bne gtgm
;lda SLB
;bne nextpacket
jmp nomsg; No msg received
gtgm:
jsr getmessage

lda MSGBUF
bne datapacket  ; Check for control message
lda MSGBUF+1
cmp #$31 ; Trust but verify
beq ctrlmsg
jsr initrf24 ; Junk package. Reset radio.
jmp nomsg

ctrlmsg:
lda MSGBUF+2
sta SLB ; Data size low byte
lda MSGBUF+3
sta SHB ; Data size high byte
  jsr PRIMM
  .asciiz "Receiving $"
  lda SHB
  jsr printbyte
  lda SLB
  jsr printbyte
  jsr PRIMM
  .asciiz " bytes. \n"
  inc SHB
  lda #0
  sta SVP
  lda #USERLANDH
  sta SVPH ; Save pointer starts at #USERLANDH
  jmp main

datapacket:

getmsg:
;inc $d2 ; Debug
;lda $90
;cmp #1
;bne nextpacket ; Data package with ID > 1
nextpacket:
ldx #2
fetchpacket:

lda MSGBUF,x
ldy #0
sta (SVP),y
inc SVP
bne nnhb
inc SVPH
nnhb:
dec SLB
bne movealong
dec SHB
beq txdone ; 0 bytes left - All done!
movealong:
dec SHB
lda SHB
jsr printbyte
inc SHB
lda SLB
jsr printbyte
jsr PRIMM
.asciiz " bytes left(hex) "
lda CRSRPNT
and #%11000000 ; keep only section bits
ora #LINESTART ;
sta CRSRPNT
inx
cpx #32
bne fetchpacket
jmp main

txdone:
    lda #USERLANDH
    sta USERLANDPH
    lda #0
    sta SLB
    sta SHB
    sta SVPH ; Reset
    sta kb_rptr ; Reset the keyboard pointers here.
    sta kb_wptr
    sta USERLANDP
    jsr PRIMM
    .asciiz "\nData loaded into RAM at $"
    LDA #USERLANDH
    jsr printbyte
    ;lda #USERLAND
    ;jsr printbyte
    jsr PRIMM
    .asciiz "00. \nPress F5 or type \"run\" to start executing. \n"

nomsg:

    jsr checkcursor

.ifdef DEBUG
lda kb_flags
and #DBGFLAG
beq nodebug
    lda CRSRPNT ; Save cursor..
    sta SVCSR
    pha
    lda CRSRPNT2
    sta SVCSR+1
    pha

    LDA #$65  ; Print debug in top right corner of screen
    sta CRSRPNT
    lda #$08
    sta CRSRPNT2

    ;lda #$6f
    ;sta CRSRPNT
    lda (DEBUGP),y
    jsr printbyte
    inc CRSRPNT ; Space
    ;LDA #$72 ; Debug ERRS and show cursor pointer next to timer on screen
    ;sta CRSRPNT
    ;lda #$27
    ;sta CRSRPNT2
    lda ERRS
    jsr printbyte
    inc CRSRPNT ; Space
    lda SVCSR+1
    jsr printbyte
    lda SVCSR
    jsr printbyte
    inc CRSRPNT ; Space
    lda MILLISH
    jsr printbyte
    inc CRSRPNT ; Space
    lda IER
    jsr printbyte
    inc CRSRPNT ; Space
    lda IFR
    jsr printbyte
    inc CRSRPNT ; Space
    ;lda kbshift
    ;jsr printbyte
    ;inc CRSRPNT ; Space

    pla
    sta CRSRPNT2
    pla
    sta CRSRPNT
    nodebug:
.endif


jsr checkkeyboard
bne key_pressed

    jmp main

checkkeyboard: ; Returns Z flag if nothing ready, not Z if something, kb_rptr in x
    ;sei ; Instead of disabling IRQ we might want to use the re-compare method.
    ;Load to x, then do the compare, then check if changed. Redo if changed. Idea for later..
    lda #0
    ldx scrp
    cpx scwp
    beq nonewsc
    rescan:
    jsr keyboard_handling
    lda #0
    ldx scrp
    cpx scwp
    bne rescan
    cpx #7 ; Top of buffer
    bcc nonewsc
    sta scrp ; 0
    sta scwp ; 0
    nonewsc:
    ldx kb_rptr
    cpx kb_wptr
    ;bne gotkey
    ;sta kb_rptr
    ;sta kb_wptr
    gotkey:
    rts

      copyscreen:
      ldy #0
      copypages:
      lda (SVP),y
      sta (SLB),y
      iny
      bne copypages
      inc SVPH
      inc SHB
      dex
      bne copyscreen
      rts


      printscreen:
            lda #0
            sta SVP
            sta SLB
            lda #SCREENSTARTH
            sta SVPH
            lda #$38
            sta SHB
            ldx #8
            jsr copyscreen
            lda #0
            sta SVP
            sta SLB
            lda #$38
            sta SVPH
            lda #$40
            sta SHB ; End pointer
            jmp eom

loadscreen:
lda #0
sta SVP
sta SLB
lda #$38
sta SVPH
lda #SCREENSTARTH
sta SHB
ldx #8
jsr copyscreen
lda #0
sta SVP
sta SLB
sta SHB
jmp eom

key_pressed:
      ;ldx kb_rptr ; Should already be in x
      lda kb_buffer, x
      cmp #$0a           ; enter - go new line
      beq enter_pressed
      cmp #$0d
      beq enter_pressed
      cmp #$1b           ; escape - clear display
      beq esc
      cmp #$08
      beq back
      cmp #$FE  ;Scan code $05
      beq f1
      cmp #$FD
      beq f2
      cmp #$FC
      beq f3
      cmp #$FB
      beq f4
      cmp #$FA
      beq f5
      cmp #$F9
      beq f6
      cmp #$F8
      beq f7
      cmp #$F7
      beq f8
      cmp #PRTSC_KEY
      beq printscreen
      cmp #L_ARR_KEY
      beq golrudarr
      cmp #R_ARR_KEY
      beq golrudarr
      cmp #UP_ARR_KEY
      beq golrudarr
      cmp #DN_ARR_KEY
      beq golrudarr
      cmp #F12_KEY
      beq f12

      jsr printk
printedkey:
      inc kb_rptr

      jmp main

golrudarr:
      jmp lrudarr

back:
      jmp backspace_pressed
f12:
      jmp loadscreen

f8:
      jmp f8_pressed
f7:
      jmp f7_pressed
f6:
      jmp f6_pressed
f5:
      jmp f5_pressed
f4:
      jmp f4_pressed

f3:
      jmp f3_pressed
f2:
      jmp f2_pressed

f1:
      jmp f1_pressed

esc:
      jmp esc_pressed

enter_pressed:
      lda CRSRCHR
      sta (CRSRPNT),y
      ldx #0
      lda kb_buffer,x
parsecmd:
      cmp #'r'
      beq read
      cmp #'w'
      beq write
      cmp #'H'
      beq strangegame
      cmp #'h'
      beq strangegame
      jmp err
read:
      jmp jread

strangegame:
      jsr PRIMM
        .asciiz "\n", "A STRANGE GAME. THE ONLY WINNING ", "\n", "MOVE IS NOT TO PLAY.", "\n"
wrotegtnw:
  jmp eom

jerr:
  jmp err

write:
      ldy #$FF
      inx
      lda kb_buffer,x
      cmp #'r'
      bne jerr
      ldx #6
      jsr asctobyte
      sta MONH ; Maybe we'll find a better place
      inx
      jsr asctobyte
      sta MONL
nextbyte:
      inx
      lda kb_buffer,x
      cmp #' '
      beq nextbyte
      cmp '.'
      beq nextbyte
      cmp ','
      beq nextbyte
      cmp '$'
      beq nextbyte
      cmp #$0a
      beq exitwrite

stbyte:
      jsr asctobyte
      iny
      sta (MONL),y
      jmp nextbyte
exitwrite:
;      inc kb_rptr
;      jmp newmon ; Previous behavior
      jsr PRIMM
      .asciiz "\nWrote "
      iny
      tya
      jsr printbyte
      jsr PRIMM
      .asciiz " bytes. Press F1 to enter monitor\n"
      jmp eom


jread:
      inx
      lda kb_buffer,x
      cmp #'u'
      beq run
      cmp #'e'
      bne err
      ldx #5
      jsr asctobyte
      sta MONH
      inx
      jsr asctobyte
      sta MONL
;      inc kb_rptr
;      jmp newmon
      jsr PRIMM
      .asciiz "\nRead:\n"
      reread:
      lda CRSRPNT
      and #%11000000 ; keep only section bits
      ora #LINESTART ;
      sta CRSRPNT
      ldy #0
      sty kb_rptr
      sty kb_wptr
      lda (MONL),y
      jsr printbyte
      jsr checkkeyboard
      beq reread
      jsr PRIMM
      .asciiz "\nPress F1 to enter monitor\n"

      jmp eom
err:
      lda CRSRPNT
      jsr crnl
      sta CRSRPNT
      bcc clkbptr
      inc CRSRPNT2
      lda (CRSRPNT),y
      sta CRSRCHR
clkbptr:
      ; Clear keyboard pointers if this is the end of message
      inc kb_rptr
      lda kb_rptr
      cmp kb_wptr
      bne notdone
eom:
    ;  lda (CRSRPNT),y ; Save new char under cursor ; Here or in err:?
    ;  sta CRSRCHR
      lda #0
      sta kb_rptr
      sta kb_wptr
    notdone:
      jmp main

run:
      jmp (USERLANDP)

esc_pressed:
      jsr clrscn
      lda #SCREENSTARTL
      sta CRSRPNT
      lda #SCREENSTARTH
      sta CRSRPNT2
      jmp clkbptr

backspace_pressed:
        jsr rubout
        jmp main

f1_pressed:
        lda #0
        sta kb_rptr
        sta kb_wptr
        jsr clrscn
        jmp monmon

f2_pressed:
        lda #$10
        sta SVPH
        lda #$01
        sta SHB
        lda #0
        sta SVP
        sta SLB
        inc SLB
        jsr savetotape
        jmp eom

f3_pressed:
    .ifdef BASIC
        lda #>RAMSTART2
        sta SVPH
        lda #<RAMSTART2
        sta SVP
    .endif
        jsr loadfromtape
        jmp eom


f4_pressed:
.ifdef BASIC
jsr clrscn
        lda #0
        sta kb_rptr
        sta kb_wptr

        lda #SCREENSTARTH
        sta CRSRPNT+1
        lda #SCREENSTARTL
        sta CRSRPNT
        jmp COLD_START
.endif
        jmp eom

f5_pressed:
        ;jsr rf_nop
        jmp run

f6_pressed:
        ;jsr readrf24regs
        ;jsr PRIMM
        ;.asciiz "Read RF24 configuration!"
        ;jsr newline

        jmp eom

f7_pressed:
        jsr resetkb
        jmp eom

f8_pressed:
          jmp reset

        newmon:
            jsr clrscn
        monmon:
            lda #29
            sta MONCNT
            lda MONH
            pha
            lda MONL
            pha
            jsr mon
            pla
            sta MONL
            pla
            sta MONH

            jsr checkkeyboard
            beq monmon
parsekey:
            lda kb_buffer,x
            inc kb_rptr
            cmp #ESC_KEY           ; escape - exit
            beq exitmon
            cmp #PGUP_KEY
            beq monpgup
            cmp #PGDN_KEY
            beq monpgdn
            cmp #UP_ARR_KEY
            beq monup
            cmp #DN_ARR_KEY
            beq mondn

            jsr checkkeyboard
            bne parsekey
            ; Let's loop here until keypress
            jmp monmon ; No data - restart monitor

mondn:      ; Arrow keys invert the natural direction because pressing down naturally means you want to see another line
            clc
            lda MONL
            adc #$08
            sta MONL
            bcc monmon
            inc MONH
            jmp monmon

monup:      ; Pressing up means you want to see a line higher up == lower
            sec
            lda MONL
            sbc #$08
            sta MONL
            bcs monmon
            dec MONH
            jmp monmon


monpgdn:
            clc
            lda MONL
            adc #$D8
            sta MONL
            bcc monmon
            inc MONH
            jmp monmon

monpgup:
            sec
            lda MONL
            sbc #$D8
            sta MONL
            bcs monmon
            dec MONH
            jmp monmon

        exitmon:
            jsr clrscn ; Clear screen on monitor exit
            lda #SCREENSTARTL
            sta CRSRPNT
            lda #SCREENSTARTH
            sta CRSRPNT2
            jmp eom
        ;    lda #0
        ;    sta MILLISH

asctohex:
              cmp #$60
              bcc caps
              sbc #$21
caps:
              sbc #'0'-1
              cmp #10
              bcc nothex
              sbc #7
nothex:
        rts

        checkcursor:
            bit MILLIS
            bmi isneg
            lda kb_flags
            and #$7f
            sta kb_flags ; Reset flip
            bne skippedcursor ; BRA
        isneg:
            bit kb_flags ; Same as last?
            bpl flip
            bmi skippedcursor ; We already flipped

        flip:
            ldy #0
            lda kb_flags
            eor #CRSR
            ora #CRSRF ; Set flip bit
            sta kb_flags
            bit kb_flags
            bvs cursoroff
            cursoron:
            lda #'_'
            sta (CRSRPNT),y
            bne cursordone ; BRA
            cursoroff:
            lda CRSRCHR
            sta (CRSRPNT),y
            cursordone:
            skippedcursor:
        rts
        MONRDKEY:
        sty TMP4
        stx TMP3 ; X to TMP3
        chkkb:
        jsr checkcursor
        jsr checkkeyboard
        beq chkkb ; We can either block here or return $06 = ACK
        lda kb_buffer, x
        inc kb_rptr
      ;  cmp #$08
      ;  bne notback
      ;  lda #$5F ; OSI basic thinks _ is rubout. Fall through.
      ;  notback:
        cmp #$0A
        bne nonewline
        lda #$0D ; NL to CR
        nonewline:
        ldy TMP4
        ldx TMP3
        rts


    .ifdef BASIC
        MONISCNTC:
        sty TMP4
        stx TMP3 ; X to TMP3
        jsr checkkeyboard
        beq NOTCNTC
        lda kb_buffer, x
        inc kb_rptr
        cmp #'^'
        bne NOTCNTC
        lda kb_flags
        and #ECODE
        beq NOTCNTC ; If strangely not actually holding CTRL or other extended key
        ldy TMP4
        ldx TMP3
        sec
        ;jmp STOP
        jmp CONTROL_C_TYPED
        NOTCNTC:
        ldy TMP4
        ldx TMP3
        clc
        rts

bell:
        lda #$02
        sta CTRL
        jsr simpledelay
        lda #1
        sta CTRL
        bne donerubout


        MONCOUT:
        cmp #0
        beq skipcout
        sty TMP4
        stx TMP3 ; X to TMP3
        pha
        lda kb_flags
        ora #CRSRF ; Set flip bit
        sta kb_flags
        ldy #0
        lda #0
        sta (CRSRPNT),y
        pla
        cmp #7
        beq bell
        cmp #$0D
        bne notcr
        jmp donerubout
        notcr:
        cmp #$08
        bne notrubout
        lda #' '
        sta (CRSRPNT),y
        jsr normcrsr ; Backspace
        dec kb_rptr
        jmp donerubout
        notrubout:
        jsr printk
        donerubout:

        ldy TMP4
        ldx TMP3
      skipcout:
        rts
.endif

clrscn:
    lda #0
    sta CRSRPNT
    lda #SCREENSTARTH ; Clear before screen to after screen
    sta CRSRPNT2
    ldy #0
    tya
    ldx #8
    clrloop:
    sta (CRSRPNT),y
    iny
    bne clrloop
    inc CRSRPNT2  ; increasing HI-byte of the clearing address.
    dex
    bne clrloop
    rts

crnl: ; Carriage return new line - needs cursor pointer in A
    and #%11000000 ; keep only section bits
    ora #LINESTART ;
    clc
    adc #$40 ; CR
    rts

newline:
    lda CRSRPNT
    jsr crnl
    sta CRSRPNT
    and #$E0 ; If msn is 0 then ++section
    bne newlinedone
    inc CRSRPNT2
newlinedone:
    jsr checkbottom
    rts


hextoa:
; wozmon-style
;    and #%00001111  ; Mask LSD for hex print.
; Already masked when we get here.
    ora #'0'        ; Add '0'.
    cmp #'9'+1      ; Is it a decimal digit?
    bcc ascr        ; Yes, output it.
    adc #$06        ; Add offset for letter.
ascr:
    rts


bytetoa: ;This SR puts LSB in A and MSB in HXH - as ascii using hextoa.
    pha
    lsr
    lsr
    lsr
    lsr
    clc
    jsr hextoa
    sta HXH
    pla
    and #$0F
    jsr hextoa
    rts

asctobyte: ; Reads two hex characters from keyboard buffer, x indexed, and returns a byte in A
    lda kb_buffer,x ; MSD
    jsr asctohex
    asl a
    asl a
    asl a
    asl a
    sta TMP
    inx
    lda kb_buffer,x ; LSD
    jsr asctohex
    ora TMP
    ; Return value in A
    rts

mon:
; Print line starting address
; Print 8 ascii hex bytes separated by ' '
; Print same 8 bytes as ASCII
;newline
;Let's start by resetting start pos
    lda #SCREENSTARTH ; If we are, then reset to top of screen
    sta CRSRPNT2
    lda #SCREENSTARTL
    sta CRSRPNT
nextline:
    jsr checkkeyboard
    beq nokb
    rts
    nokb:
    ldx #8
    lda #'$'
    jsr printa
    lda MONH
    jsr printbyte
    lda MONL
    jsr printbyte
    lda #':'
    jsr printa
    lda #' '
    jsr printa
    ldy #0
nexthex:
    lda (MONL),y
    sta ABUF,x
    jsr printbyte
    inc CRSRPNT ; Make a space
    inc MONL
    bne notof
    inc MONH
notof:
    dex
    bne nexthex
    ; Print ascii
    ldx #8
printabuf:
    lda ABUF,x
    cmp #$0A
    bne notnl
    tya ; Zero A
    notnl:
    jsr printa
    dex
    bne printabuf
    ; Get ready for a new line
    lda CRSRPNT
    jsr crnl ; Carriage Return New Line
    sta CRSRPNT
    and #$E0 ; If msn is 0 then ++section
    bne gonext
    inc CRSRPNT2
gonext:
    dec MONCNT
    bne nextline ; Was this the last line? No, nextline.
    rts

printbyte:
    pha ; Save A
    jsr bytetoa
    pha
    lda HXH
    jsr printa
    pla
    jsr printa
    pla ; Restore A
    rts

printfast:
sta (CRSRPNT),y
inc CRSRPNT
bne printedfast
inc CRSRPNT2
printedfast:
rts

printpos:
ldy #0
tax
lda CRSRPNT
sta SLB ; Not doing this while using
lda CRSRPNT2
sta SHB
lda #08
sta CRSRPNT2
lda #$4f
sta CRSRPNT

lda #'A'
jsr printfast
txa
jsr printbyte
inc CRSRPNT

lda SHB
jsr printbyte
lda SLB
jsr printbyte
inc CRSRPNT

lda #'Y'
jsr printfast
lda TMP4
jsr printbyte
inc CRSRPNT

lda #'X'
jsr printfast
lda TMP3
jsr printbyte
inc CRSRPNT



ldy SHB
sty CRSRPNT2
ldy SLB
sty CRSRPNT
txa
rts

printa:
    sta TMP2 ; save A
    TYA			; copy Y
    PHA  			; save Y
    TXA			; copy X
    PHA  			; save X
    ldy #0
    lda TMP2

    cmp #$0A ; Do we need this?
    beq printnewline

    sta (CRSRPNT),y
    inc CRSRPNT
    bne printeda
    inc CRSRPNT2
printeda:
    PLA			; pull value
    TAX  			; restore X
    PLA			; pull value
    TAY  			; restore Y
    lda TMP2 ; Keep key in A
    rts
printnewline:
    jsr newline
    jmp printeda

lrudarr:
;    bit MILLIS
;    bpl correctchr Not worth testing...
    pha
    ldy #0
    lda CRSRCHR     ; Make sure we leave the character in the position and not the cursor.
    sta (CRSRPNT),y
correctchr:
    pla
    cmp #R_ARR_KEY
    beq rarr
    cmp #UP_ARR_KEY
    beq uparr
    cmp #DN_ARR_KEY
    beq dnarr
larr:
    dec CRSRPNT ; Move cursor left
    lda CRSRPNT
    cmp #255
    bne checkline
    dec CRSRPNT2
checkline:
    and #%00111111
    cmp #LINESTART
    bcs checkedarrows
    lda CRSRPNT
    sec
    sbc #$41
    ora #$3e
    sta CRSRPNT
    bcs checkedarrows
    dec CRSRPNT2
    bcc checkedarrows ; BRA
rarr:
    inc CRSRPNT
    lda CRSRPNT
    and #$3F   ; Discard MS bits since we only care about current line
    cmp #LINEEND+1
    bcc checkedarrows ; A < 62 == Not Front porch
    jsr newline
    ora #LINESTART
    sta CRSRPNT
    jmp checkedarrows
uparr:
    lda CRSRPNT
    sec
    sbc #$40
    sta CRSRPNT
    bcs checkedarrows
    dec CRSRPNT2
    bcc checkedarrows ; BRA
dnarr:
    lda CRSRPNT
    clc
    adc #$40
    sta CRSRPNT
    bcc checkedarrows
    inc CRSRPNT2
    jmp checkedarrows
checkedarrows:
jsr checkbottom
lda (CRSRPNT),y ; Save new char under cursor
sta CRSRCHR
lda kb_flags
ora #CRSR
sta kb_flags
lda #'_'
sta (CRSRPNT),y
jmp printedkey

printk:
      pha
checkl: ; This is user input, so we have to make sure we don't hit VGA blanking by mistake
    lda CRSRPNT
    and #$3F   ; Discard MS bits since we only care about current line
    cmp #LINEEND
    bcc chs ; A < 62 == Not Front porch
    jsr newline
chs:
    cmp #LINESTART
    bcs chs2
    ora CRSRPNT
    sta CRSRPNT
chs2:
    jsr checkbottom
rpa:
    pla
    jsr printa
    rts


checkbottom:
    tya
    pha
    lda CRSRPNT2 ; Check if we're off screen
    cmp #SCREENSTARTH
    bcc resetcursor ; Off screen
    cmp #$07+SCREENSTARTH
    bcc checkedbottom ; if we're not
lda CRSRPNT ; Check LSB as well if we're above $2700
cmp #$80
bcc checkedbottom
resetcursor:
jsr scrollslow
lda #SCREENSTARTH+7 ; If we are, then reset to bottom of screen
sta CRSRPNT2
lda #SCREENSTARTL
sta CRSRPNT
checkedbottom:
    pla
    tay
    rts

scrolldown:
lda #SCREENSTARTH+1
sta SCRLPNT2+1
sta SCRLPNT+1
dec SCRLPNT+1
lda #0
sta SCRLPNT
sta SCRLPNT2
nextblock:
ldy #$FF
moveblock:
lda (SCRLPNT2),y
sta (SCRLPNT),y
dey
bne moveblock
inc SCRLPNT+1
inc SCRLPNT2+1
lda SCRLPNT2+1
cmp #SCREENSTARTH+8
bne nextblock
lda #0
clearlast:
sta (SCRLPNT),y
dey
bne clearlast
sta (SCRLPNT),y ; Clear y = 0 too
rts

scrollslow:
clc
ldy #0
lda #SCREENSTARTH
sta SCRLPNT2+1
sta SCRLPNT+1
lda #SCREENSTARTL
sta SCRLPNT
adc #$40 ; Line down
sta SCRLPNT2
bcc moveline
inc SCRLPNT2+1
moveline:
lda (SCRLPNT2),y
sta (SCRLPNT),y
;lda #0
;sta (SCRLPNT2),y ; Lazy code - actually only need to do this for last line
iny
cpy #$40
bne moveline
clc
lda SCRLPNT2
sta SCRLPNT
adc #$40
sta SCRLPNT2
lda SCRLPNT2+1
sta SCRLPNT+1
ldy #0
bcc l1375
inc SCRLPNT2+1
l1375:
lda SCRLPNT2+1
cmp #SCREENSTARTH+7
bne moveline
lda SCRLPNT2
cmp #$7F
bcc moveline
ldy #$40
lda #0
clearlastline:
sta (SCRLPNT),y
dey
bne clearlastline
rts

    resetkb:
    sei
    lda #$11
    sta CTRL
     LDA #%01000000
     STA ACR               ; T1 continuous - disable Shift register
     lda #%00100100
     sta IER               ; Disable T2 + SR
     ;jsr delay20ms
     jsr simpledelay ; Should be checked and optimized... Hmm...
     jsr simpledelay
     jsr simpledelay
     jsr simpledelay
     jsr simpledelay
     jsr simpledelay
     jsr simpledelay
     jsr simpledelay
     jsr simpledelay
     LDA #%01101100
     STA ACR             ; T1 continuous, T2 count, PB7 disabled, Shift In External
     lda #5
     sta T2CL
     lda #%11100100  ; Set T1 + T2 + SR
     sta IER
     lda #0
     sta SR1
     sta scwp
     sta scrp
     sta kb_wptr
     sta kb_rptr
     sta t2irqreg1
     sta kb_flags
     sta T2CH
     lda #1
     sta CTRL
     bit T1CL ; Clear irq
     cli
    rts

; IRQ code starts here - enters at irq:

badpacket:
jsr resetkb
beq exitirq ; BRA

t2_irq:
;Time critical stuff
ldx SR1
lda t2irqreg1 ; If 0 this is first packet, not second
bne second
lda #4 ; 5 bits
sta T2CL
lda #0
sta T2CH
stx t2irqreg2
inc t2irqreg1
bne exitirq ; BRA
second:
lda #5 ; 6 bits
sta T2CL
lda #0
sta T2CH
sta SR1
sta t2irqreg1

txa
lsr
bcc badpacket ; Stop bit should be 1 - not checking parity.. yet
lsr
and #$0F
asl t2irqreg2
asl t2irqreg2
asl t2irqreg2
bcs badpacket ; Start bit should be 0 in C - if not we're in trouble anyway
ora t2irqreg2
ldx scwp ; Scan code write pointer
sta scbuf, x ; Store to scan code buffer
inc scwp

exitirq:
pla
tay ; Restore x and y
pla
tax
pla
rti

exitsrirq:
bit SR1 ; Clear SR IRQ
pla
rti

ca1_irq:
jmp ca1irq

  hitbrk:
  ;  jmp reset
    inc brkcnt
    lda $104,x
    sta $600
    lda #<main
    sta $104,x ; Return to main instead of breakpoint
    lda $105,x
    sta $601
    lda #>main
    sta $105,x
  jmp exitirq
  ; IRQ vector points here ; Thanks to Ben Eater for a very useful PS2->Ascii interface
  ;IFR is IRQ Tl T2 CBl CB2 SR CA1 CA2
irq:
  pha ; Save A
  lda IFR
  and #4
  bne exitsrirq

  txa
  pha ; Save X
  tya
  pha ; Save y

  lda #%00100000 ; We need T2 to fire super fast, so we check it first.;
  and IFR
  bne t2_irq

lda IER ; Priority to ca1 but only if enabled
and IFR
tax
and #2 ; CA1
bne ca1_irq ; Disabled so it doesn't mess up things...


;Alt approach
 ;lda IER
 ;and IFR ; We only care about active IRQ's
txa ; IER AND IFR
 asl ; IRQ in C
 asl ; T1 flag in C
 bcs t1_irq
; asl ; T2
 ;bcs t2_irq
; asl ; CB1
 ;asl ; CB2
; asl ; SR
;asl ; CA1
; bcs ca1_irq
; bcs keyboard_interrupt
; asl ; CA2

;  bit IFR ; T1 as fast as possible
;  bvs t1_irq

  tsx
  inx
  lda $0103,x ; Pull status register off stack and check break flag
  and #$10
  bne hitbrk

;  lda IFR
;  and #2
;  bne keyboard_interrupt
  inc ERRS ;Should never end up here...
  jmp exitirq

  gogmillis:
  jmp gmillis

    t1_irq:
        bit T1CL ; Clear irq

        bit ACR ; Are we saving to tape?
        bpl gmillis ; No - just increment millis
        dec PULSES
        bne gmillis
        ;lda #2
        ;sta PULSES ; Let's set pulses to KCS individually pr bit
        ; Here we need to keep track of the current byte and current bit - 16 IRQ(half pulses) pr bit
        ; SVP Save pointer low
        ; CBIT current bit mask

        ; Probably don't need this flag anymore
        ;lda TAPEFLAGS ; If we just started then send a 0 bit to make the 7F leader
        ;bpl running
        ;and #$7F ; Remove flag
        ;sta TAPEFLAGS
        ;inc BITNO
        ;lda PULSESL ; 8 Half pulses of 1200Hz
        ;asl ; PULSESL holds full cycles
        ;sta PULSES
        ;lda #3
        ;sta T1LH
        ;lda #$3f
        ;sta T1LL
        ;bne t1_irq_exit ; BRA

        running:
        inc BITNO
        lda BITNO
        cmp #13 ; This is a bit messy, could use a fix
        bne notnewbyte
        lda #1
        sta BITNO
        jmp send0
        notnewbyte:
        cmp #1
        beq send0
        cmp #10 ; Bit 10 and 11 should send a 1
        bcs send1

        lda CBIT
        bne here ; Mask bit has not been shifted out yet
        lda #1 ; Mask = LSB
        sta CBIT
here: ; New bit
        ldy #0
        and (SVP),y ; Assuming y is 0
        beq send0 ; Bit was 0
        send1:
        lda PULSESH ; 16 Half pulses of 2400Hz
        asl ; PULSESH holds whole cycles
        sta PULSES
        lda #1
        sta T1LH
        lda #$9f
        bne there ; BRA
        send0:
        lda PULSESL ; 8 Half pulses of 1200Hz
        asl ; PULSESL holds full cycles
        sta PULSES
        lda #3
        sta T1LH
        lda #$3f
there:
        sta T1LL

        lda BITNO
        cmp #1
        beq notdoneyet
        cmp #12
        beq gonextbit ; This little bit of juggling means we move on to the next byte after the stop bits
        cmp #9
        bcs notdoneyet

        gonextbit:
        clc ; Let's make sure we don't shift in a carry by mistake
        asl CBIT ;Next bit
        bne notdoneyet
        inc SVP  ; Increment pointer = New byte
        bne arewedone
        inc SVPH ; High byte
        ;lda SVPH
        ;cmp SHB ; Maybe not the right variable to use?
        ;bcc notdoneyet
        arewedone:
        dec SLB
        bne notdoneyet
        dec SHB
        bne notdoneyet
        lda ACR
        and #$7f
        sta ACR
        bne t1_irq_exit ; BRAnch always
        notdoneyet:

    gmillis:

        inc MILLIS
        bne t1_irq_exit
        inc MILLIS+1
        dec LOADTIMEOUT
    t1_irq_exit:
    jmp exitirq

;    cb1_IRQ:
;        dec kbbit
;        lda PORTB ; clear IRQ
;        pla
;        rti



keyboard_handling: ; SR, not IRQ any longer
        lda scbuf, x
        inc scrp
        tax
        lda reversebits, x
        tax

; Fall through to scancode -> ascii parsing -> key buffer

        keyboard_interrupt:
          lda kb_flags
          and #RELEASE   ; check if we're releasing a key
          beq read_key   ; otherwise, read the key

          lda kb_flags
          eor #RELEASE   ; flip the releasing bit
          sta kb_flags
        ;  lda PORTA      ; read key value that's being released
        ;  lda KEYBOARD
          txa
          cmp #$12       ; left shift
          beq shift_up
          cmp #$59       ; right shift
          beq shift_up
          cmp #$14
          beq ekey_up
          bne exit

          ekey_up:
          lda kb_flags
          and #%11111011 ; Turn off ecode
          sta kb_flags
          jmp exit

  shift_up:
    lda kb_flags
    eor #SHIFT  ; flip the shift bit
    sta kb_flags
    jmp exit

    break:
    jmp noclear

  read_key:
    txa
    cmp #$77 ; Either numlock or pause/break - we reset without clearing ram
    beq break
    cmp #$f0        ; if releasing a key
    beq key_release ; set the releasing bit
    cmp #$12        ; left shift
    beq shift_down
    cmp #$59        ; right shift
    beq shift_down
    cmp #$E0
    beq ekey_down
    cmp #$14
    beq ekey_down

    lda kb_flags
    and #ECODE
    bne ecode_key

    lda kb_flags
    and #SHIFT
    bne shifted_key

    txa
    sta kb_last
    lda keymap, x   ; map to character code
    jmp push_key

    ekey_down:
    sta kb_last
    lda kb_flags
    ora #ECODE
    sta kb_flags
    jmp exit

    ecode_key:
    txa
    sta kb_last
    lda keymap_ecode,x
    jmp push_key

  shifted_key:
    lda keymap_shifted, x   ; map to character code

  push_key:
    ldx kb_wptr
    sta kb_buffer, x
    inc kb_wptr
    jmp exit

  shift_down:
    lda kb_flags
    ora #SHIFT
    sta kb_flags
    jmp exit

  key_release:
    lda kb_flags
    ora #RELEASE
    sta kb_flags
    lda kb_last
    cmp #$e0
    bne exit
    jmp ekey_up

  exit:
  rts


; Thanks to Ben Eater for a very useful PS2->Ascii interface
    keymap:
      .byte "?",F9_KEY,"?",$FA,$FC,$FE,$FD,F12_KEY,"?",F10_KEY, $F7,$F9,$FB," `?" ; 00-0F
      ;F1, F2, F3,F4, F5, F6, F7, F8 key bound to $FE, $FD, $FC, $FB, $FA, $F9, $F8, $F7 for no particular reason
      .byte "?????q1???zsaw2?" ; 10-1F
      .byte "?cxde43?? vftr5?" ; 20-2F
      .byte "?nbhgy6???mju78?" ; 30-3F
      .byte "?,kio09??./l;p-?" ; 40-4F
      .byte "??\'?[=????",$0a,"]?\\??" ; 50-5F
      .byte "??????",$08,"??1?47???" ; 60-6F
      .byte "0.2568",ESC_KEY,F11_KEY,"?+3-*9??" ; 70-7F
      .byte "???",$F8,"????????????" ; 80-8F $F8 = F7 key
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 90-9F
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; A0-AF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; B0-BF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; C0-CF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; D0-DF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; E0-EF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; F0-FF
    keymap_shifted:
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED," ~?" ; 00-0F
      .byte "?????Q!???ZSAW@?" ; 10-1F
      .byte "?CXDE#$?? VFTR%?" ; 20-2F
      .byte "?NBHGY^???MJU&*?" ; 30-3F
      .byte "?<KIO)(??>?L:P_?" ; 40-4F
      .byte "??\"?{+?????}?|??" ; 50-5F
      .byte "?????????1?47???" ; 60-6F
      .byte "0.2568???+3-*9??" ; 70-7F
      .byte "???",$F8,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 80-8F $F8 = F7 key
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 90-9F
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; A0-AF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; B0-BF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; C0-CF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; D0-DF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; E0-EF
      .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; F0-FF
      keymap_ecode:
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 00-0F
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 10-1F
        .byte $ED,"^",$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED; 20-2F
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 30-3F
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 40-4F
        .byte $ED,$ED,$ED,$ED,$ED,'$',$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 50-5F
        .byte "0123456789A",L_ARR_KEY, HOME_KEY, "DEF" ; 60-6F
        .byte "01",DN_ARR_KEY,"3",R_ARR_KEY, UP_ARR_KEY, "6789",PGDN_KEY,"B",PRTSC_KEY, PGUP_KEY,"EF" ; 70-7F $F6 = PGUP(7D), $F5 = PGDOWN(7A), $F4 = UP Arr.(75), Down arr.(72) = $F3
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 80-8F $F8 = F7 key
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; 90-9F
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; A0-AF
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; B0-BF
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; C0-CF
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; D0-DF
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; E0-EF
        .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED ; F0-FF

;This is a table of all reversed bits - for example $0F is $F0 and $AA is $55.
;We need this to convert the scan code from LSB first to MSB first. The alternative is to rewrite the three tables above -- and I really don't have the patience for that right now.
reversebits:
        .byte 0, 128, 64, 192, 32, 160, 96, 224, 16, 144, 80, 208, 48, 176, 112, 240
        .byte 8, 136, 72, 200, 40, 168, 104, 232, 24, 152, 88, 216, 56, 184, 120
       .byte 248, 4, 132, 68, 196, 36, 164, 100, 228, 20, 148, 84, 212, 52, 180
       .byte 116, 244, 12, 140, 76, 204, 44, 172, 108, 236, 28, 156, 92, 220, 60
       .byte 188, 124, 252, 2, 130, 66, 194, 34, 162, 98, 226, 18, 146, 82, 210, 50
       .byte 178, 114, 242, 10, 138, 74, 202, 42, 170, 106, 234, 26, 154, 90, 218
       .byte 58, 186, 122, 250, 6, 134, 70, 198, 38, 166, 102, 230, 22, 150, 86, 214
       .byte 54, 182, 118, 246, 14, 142, 78, 206, 46, 174, 110, 238, 30, 158, 94
       .byte 222, 62, 190, 126, 254, 1, 129, 65, 193, 33, 161, 97, 225, 17, 145, 81
       .byte 209, 49, 177, 113, 241, 9, 137, 73, 201, 41, 169, 105, 233, 25, 153, 89
       .byte 217, 57, 185, 121, 249, 5, 133, 69, 197, 37, 165, 101, 229, 21, 149, 85
       .byte 213, 53, 181, 117, 245, 13, 141, 77, 205, 45, 173, 109, 237, 29, 157
       .byte 93, 221, 61, 189, 125, 253, 3, 131, 67, 195, 35, 163, 99, 227, 19, 147
       .byte 83, 211, 51, 179, 115, 243, 11, 139, 75, 203, 43, 171, 107, 235, 27
       .byte 155, 91, 219, 59, 187, 123, 251, 7, 135, 71, 199, 39, 167, 103, 231, 23
       .byte 151, 87, 215, 55, 183, 119, 247, 15, 143, 79, 207, 47, 175, 111, 239
       .byte 31, 159, 95, 223, 63, 191, 127, 255

    spibyte:
          ;SR borrowed from http://www.cyberspice.org.uk/blog/2009/08/25/bit-banging-spi-in-6502-assembler/
          ;I think GW used this way too?
          ;Bit 5 – MISO (Input data from the peripheral to the computer) ;
          ;Bit 7 – MOSI (Output data from the computer to the peripheral) We should prefer PB7 to be an output and not use PB6 because of T1 and T2 hw features(T1 out to PB7 and T2 in to PB6)
          ;Bit 1 – CS (Chip select)
          ;Bit 0 – SCLK (Output SPI clock)

                      sta outb

                      lda PORTB
                      and #%01011100
                      sta SPITMP ; Save bits not used by SPI


                      TXA
                      PHA
                      TYA
                      PHA

                      ldy #0
                      sty inb
                      ldx #8
              ;        sei ; No IRQs during transfer plz
    spibytelp:
                      tya		; (2) set A to 0
                      asl outb	; (5) shift MSB in to carry
                      bcc spibyte1	; (2)
                      ora #mosi	; (2) set MOSI if MSB set
    spibyte1:
                      ora SPITMP ; Debug - set preserved port B bits. If interrupts are enabled be careful when using this.
                      sta uservia	; (4) output (MOSI, SCS low, SCLK low)
                      tya		; (2) set A to 0 (Do it here for delay reasons)
                      inc uservia	; (6) toggle clock high (SCLK is bit 0)
                      clc		; (2) clear C (Not affected by bit)
                      ;bit uservia	; (4) copy MISO (bit 7) in to N (and MOSI in to V) ; 4 cycles
                      ;bpl spibyte2	; (2) bvc if miso bit 6, bpl if miso bit 7 ; 2/3 cycles
                      lda #miso ; 2 cycles
                      and uservia ; 4 cycles
                      beq spibyte2 ; 2/3 cycles
                      sec		; (2) set C if MISO bit is set (i.e. N) / Or V for miso on bit 6
    spibyte2:
                      rol inb		; (5) copy C (i.e. MISO bit) in to bit 0 of result
                      dec uservia	; (6) toggle clock low (SCLK is bit 0)
                      dex		; (2) next bit
                      bne spibytelp	; (2) loop
                ;      cli ; IRQs ok from here
                      PLA
                      TAY
                      PLA
                      TAX

                      lda inb		; get result
                      rts

                      exitnochange: ; Can jump here from save and load
                      ;lda #0 ; Reset KB pointers
                      ;sta kb_rptr
                      ;sta kb_wptr
                      ;sta scrp
                      ;sta scwp
                      sec ; Cancel flag
                      rts

                      selectbaudrate:
                      sty TMP4
                      jsr PRIMM
                      .asciiz "Press 1 for 300 baud KCS. Press 2 for 1200 baud.\n"
                      jsr PRIMM
                      .asciiz "Press ESC to cancel.\n"
                      jsr checkbottom
                      ;jsr resetkb

                      waitloop:
                      jsr wkey
                      cmp #$31
                      beq kcs
                      cmp #$32
                      beq kcs1200
                      cmp #ESC_KEY           ; escape - exit
                      beq exitnochange
                      bne waitloop
                      kcs:
                      lda #4
                      sta PULSESL
                      lda #8
                      sta PULSESH
                      bne readytosave ; BRA
                      kcs1200:
                      lda #1
                      sta PULSESL
                      lda #2
                      sta PULSESH

                      readytosave:
                      ldy TMP4
                      clc
                      rts

                      savetotape:

                      jsr PRIMM
                      .asciiz "Ready to save $"
                      dec SHB
                      lda SHB
                      ;sbc SVPH
                      jsr printbyte
                      inc SHB

                      dec SLB
                      lda SLB

                      jsr printbyte
                      inc SLB
                      jsr PRIMM
                      .asciiz " bytes of data starting from address $"

                      lda SVPH
                      jsr printbyte
                      lda SVP
                      jsr printbyte
                      ;jsr newline

                      jsr selectbaudrate

                      jsr PRIMM
                      .asciiz "\nStart rec on tape and press ENTER after leader. Press ESC to cancel..\n"

                      jsr setpulses

                    wrec:
                      jsr wkey
                      cmp #ESC_KEY           ; escape - exit
                      beq jmptoexit
                      cmp #$0A
                      beq srecord
                      bne wrec ; loop

                      jmptoexit:
                      jmp exitnochange

                    srecord:
                      jsr PRIMM
                      .asciiz "Starting recording... Press any key to stop.\n"

                       clc
                       lda scwp
                       adc #2
                       waitfor2:
                       cmp scwp
                       bne waitfor2

                     lda #0 ; Reset KB pointers
                     sta kb_rptr
                     sta kb_wptr
                     sta scrp
                     sta scwp

                      lda ACR
                      ora #$C0 ; Enable T1 PB7 output
                      sta ACR
                      lda #0
                      sta SVP
                      tay ; IRQ assumes y is 0.
                      lda #0
                      sta CBIT
                      sta BITNO

                      lda #135 ; Sine or cosine can be selected by odd or even leader pulses - let's make sure this number is high enough to count as a 7 "1" bits
                      sta PULSES ; Start with leader of x "1" pulses at TIMEOUT interval. This should be followed by #$7F to sync start bit.
                      lda #1
                      sta T1CH
                      lda #$9f
                      sta T1CL
                      lda #$80 ; Using the NRZ flag as first byte flag for tx
                      sta TAPEFLAGS

                    record:
                      ; Let's do the byte -> tone conversion in IRQ
                    lda scrp
                    cmp scwp
                      bne recorddone
                      bit ACR
                      bpl finished
                      bmi record
                    recorddone:
                      jsr PRIMM
                      .asciiz "Keyboard interrupt.\n"
                      LDA #%01101100
                      STA ACR             ; T1 continuous, T2 count, PB7 disabled, Shift In External
                      bne interrupted ; BRA
                    finished:
                      jsr PRIMM
                      .asciiz "Recording finished.\n"
                      interrupted:

                      LDA #<TIMEOUT
                      STA T1LL            ; Set low byte of timer1 counter
                      LDA #>TIMEOUT
                      STA T1LH            ; Set high byte of timer1 counter
                      lda #0 ; Reset KB pointers
                      sta kb_rptr
                      sta kb_wptr
                      rts

                      basicsavetotape:
                      .ifdef BASIC
                      lda #<RAMSTART2
                      sta SVP
                      lda #>RAMSTART2
                      sta SVPH
                      sec
                      ;lda VARTAB
                      .byte $A5 ; Explicitly using ZP addressing LDA to kill warning
                      .byte VARTAB
                      sbc #<RAMSTART2
                      sta SLB
                      inc SLB
                      .byte $A5
                      .byte VARTAB+1
                      sbc #>RAMSTART2
                      sta SHB
                      inc SHB
                      jsr savetotape
                      .endif
                      rts

                      basicloadfromtape:
                      .ifdef BASIC
                      lda #<RAMSTART2
                      sta SVP
                      lda #>RAMSTART2
                      sta SVPH
                      jsr loadfromtape
                      lda SVP
                      .byte $85
                      .byte VARTAB
                      lda SVPH
                      .byte $85
                      .byte VARTAB+1
                      .endif
                      rts

                      ; FIX HARDCODED ADDRESSES HERE!
                      loadfromtape:
                        jsr PRIMM
                        .asciiz "Loading from tape.\n"
                        jsr selectbaudrate
                        bcs statusloop

                        ldx #0
                        txa
                      cleardata:
                        sta $1000,x
                        dex
                        bne cleardata

                        lda #$FF
                        sta T1LL
                        sta T1LH
                        sta T1CH ; This should reset T1?
                        sta RXBYTE

                        ; Activate CA1 irq to trigger IRQ
                        lda #$82 ; S+CA1 bit
                        sta IER
                          ; Receive data in irq
                        lda #0
                        ;stx CBIT
                        ;sta TAPEFLAGS
                        sta plzpnt
                        sta PULSES
                        sta onesinarow
                        lda #8
                        sta CBIT
                        lda #2
                        sta LOADTIMEOUT
                        lda #$32
                        sta plzpnt+1
                        lda #$ff
                        sta NRZBYTE
                        jsr PRIMM
                        .asciiz "Press play on tape! \n"
                        ; Write status here
                        statusloop:
                        lda LOADTIMEOUT
                        bne noproblem
                        lda TAPEFLAGS
                        and #$20
                        beq timeout
                        lda #3
                        sta LOADTIMEOUT
                        noproblem:
                        lda TAPEFLAGS
                        and #$20
                        beq noleader
                        jsr PRIMM ;
                        .asciiz "Got leader.\n"
                        lda TAPEFLAGS
                        and #%11011111 ;
                        sta TAPEFLAGS
                        noleader:
                        lda SVP
                        .ifdef BASIC
                        cmp #<RAMSTART2
                        bne noz
                        lda SVPH
                        cmp #>RAMSTART2
                        beq notstarted
                        .else
                        cmp #<1000
                        bne noz
                        lda SVPH
                        cmp #>1000
                        beq notstarted
                        .endif

                        noz:
                        lda CRSRPNT
                        and #%11000000 ; keep only section bits
                        ora #LINESTART ;
                        sta CRSRPNT
                        ldy #0
                        lda #'$'
                        jsr printfast
                        sec
                        lda SVPH
                        .ifdef BASIC
                        sbc #>RAMSTART2
                        jsr printbyte
                        lda SVP
                        sbc #<RAMSTART2
                        .else
                        sbc #>1000
                        jsr printbyte
                        lda SVP
                        sbc #<1000
                        .endif
                        jsr printbyte
                        jsr PRIMM
                        .asciiz " bytes received"

                        notstarted:
                        lda #0
                        jsr checkkeyboard
                        beq l2020
                        lda kb_buffer,x
                        inc kb_rptr
                        cmp #ESC_KEY
                        beq exituserland
                        l2020:
                        lda IER
                        and #2
                        beq timeout
                        jmp statusloop
                        ; Disable CA1 in IRQ on timeout
                        timeout:
                        jsr PRIMM
                        .asciiz "\nFinished loading.\n"
                        exituserland:
                        lda #2 ; Disable CA1
                        sta IER

                        lda #0
                        sta TAPEFLAGS

                        lda #<TIMEOUT
                        sta T1LL
                        lda #>TIMEOUT
                        sta T1LH
                        sta T1CH
                        rts

                        ca1irq:
                        bit PORTA ; Clear IRQ flag
                        inc PULSES
                        readtime:
                        ldx T1CH
                        lda T1CL
                        cpx T1CH
                        bne readtime

                        sta CT1L ; T1CL
                        stx CT1H ; T1CH

                        sec
                        lda LT1L ; Last T1CL
                        sbc CT1L ; Current T1CL
                        sta plsperiod
                        lda LT1H ; Last T1CH
                        sbc CT1H ; Current T1CH

                        sta plsperiod+1 ; ResultH


                        lda TAPEFLAGS
                        bpl donedebug ; Only KCS, no need for the block below

                      ;  if last == 1 and this == 0  and pulses == 2 then insert 1 and proceed
                        lda TMP
                        and #1
                        beq nevermind
                        clc
                        lda plsperiod
                        adc #$40 ; Add some negative hysteresis
                        sta plsperiod
                        bcc nm
                          inc plsperiod+1
                          nm:
                        lda PULSES
                        cmp #2
                        bne nevermind

                        lda plsperiod+1
                        cmp #5
                        bcc nevermind
                        bne doit ; Higher than 5 so def. a 0
                        lda plsperiod
                        cmp #$08
                        bcc nevermind ; This was a 1, not a 0 after all
                        doit:
                        sec
                        jsr savebit
                        inc PULSES
                        nevermind:

                        lda plsperiod+1

                      ;Debug
                      ;  ldy #0
                      ;  sta (plzpnt),y
                      ;  inc plzpnt
                      ;  bne lbyte
                      ;  inc plzpnt+1
                      ;  lbyte:
                      ;  lda plsperiod
                      ;  sta (plzpnt),y
                      ;  inc plzpnt
                      ;  bne donedebug
                      ;  inc plzpnt+1

                      donedebug:
                        stx LT1H
                        lda CT1L
                        sta  LT1L

                        lda plsperiod+1
                        cmp #6
                        bcs was0 ; Definitely a 0, skip extra check

                        cmp #5 ; Should compare 16 bits instead of MSB, should compare to an avg of 8 "1"s.. But for now this is fine for 1200/~2400Hz
                        ;bcc was1 ; Was 1 (4 or less) Fall through instead
                        bcs maybe0 ; Was 0 (5 or more)
                        was1:
                        bit TAPEFLAGS ; Check NRZ
                        bpl checkpulses
                        noextra:
                        inc onesinarow ; This is only relevant for 1200/2200Hz modulation
                        lda onesinarow
                        cmp #9 ; Correct for the fact that the 5th one pulse is missing it's companion within the bit period at 2200Hz instead of 2400Hz
                        bne checkpulses
                        inc PULSES
                        lda #0
                        sta onesinarow
                        checkpulses:
                        lda PULSES
                        cmp PULSESH
                        bcc l8r
                        gtsb:
                        sec
                        jsr savebit

                        l8r:
                        sec
                        bcs printed ; BRA

                        maybe0:
                        bit TAPEFLAGS ; Check NRZ
                        bpl was0
                        lda plsperiod
                        cmp #$30
                        bcc was1 ; Low byte indicates this was in fact not a 0
                        lda plsperiod+1
                        was0:
                        cmp #9
                        bcs junk ; junk = noErrC, over = badErrC
                        corrected:
                        lda #0
                        sta onesinarow
                        lda PULSES
                        cmp PULSESL
                        bcc l0r
                        clc
                        jsr savebit
                        l0r:
                        clc
                        bcc printed ; BRA

                        junk:
                        lda #0
                        sta PULSES
                        sta onesinarow
                        beq gtx ; BRA

                      printed:
                        rol TMP ; Save carry bit for next round
                        gtx:
                        jmp exitirq ; IRQ exit in ROM

                        checkstart:
                        lda #0
                        plp
                        ror
                        beq afternosave
                        jmp morebits ; Bad start bit

                      savebit:
                        php ; Save carry
                        lda CBIT
                        cmp #11 ; Check start bit
                        beq checkstart
                        cmp #2 ; Throw away parity
                        beq nosave
                        cmp #1 ; Throw away stop
                        beq nosave
                        plp
                        ror RXBYTE ; Shift in bit from carry

                        jmp afternosave
                        nosave:
                        plp
                        afternosave:
                        bit TAPEFLAGS

                        ;Tapeflags
                        ;$80 = nrzbit
                        ;$40 = startbyte received
                        bpl rstplss ; Not saving NRZ too

                        lda RXBYTE
                        cmp #$7E
                        beq morebits
                        clc
                        and #$80
                        rol
                        rol
                        sta (plzpnt),y
                        inc plzpnt
                        bne rstplss
                        inc plzpnt+1
                        rstplss:
                        lda #0
                        sta PULSES
                        bit TAPEFLAGS
                        ;Tapeflags
                        ;$80 = nrzbit
                        ;$40 = startbyte received
                        bpl checkleader ; Not saving NRZ too
                        ldy NRZBYTE ; Backup
                        lda RXBYTE
                        ;cmp #$FE ; Check for another "flag" byte FE == 7E NRZ
                        ;beq aprsleader
                        and #$C0 ; Top two bits
                        beq one ; AND 0 == both bits zero
                        cmp #$C0
                        beq one ; CMP 0 = C0 = both bits 1
                        zero:
                        clc
                        bcc afterone; BRA
                        one:
                        sec
                        afterone:
                        ror NRZBYTE ; Shift in carry
                        ; Unstuffing
                        ;bit TAPEFLAGS
                        ;bvc printnrz ; Skip if not saving
                        lda NRZBYTE
                        cmp #$7E
                        beq aprsleader
                        ldy #0
                        ;sec
                        ;bcs morebits
                        printnrz:
                        ldy #0
                        ;inc $10e0
                      ;  lda NRZBYTE
                      ;  cmp #$7E
                      ;  beq aprsleader
                        bit TAPEFLAGS
                        bvs save
                        bvc morebits

                        checkleader:
                        bvs save; Already saving
                        lda RXBYTE
                        cmp #$7F
                        beq kcsleader
                        ;cmp #$FE ; FE == E7 in NRZ
                        ;beq aprsleader
                        rts ; Don't want to fall through



                        save:
                        dec CBIT
                        bne morebits
                        ;lda RXBYTE
                        savebyte:
                        lda TAPEFLAGS
                        bpl savekcs

                        ; Not for KCS stuff
                        and #$04
                        beq noshift
                        lda NRZBYTE
                        lsr ; Addresses are left shifted
                        jmp shifted
                        noshift:
                        lda NRZBYTE
                        shifted:
                        sta (plzpnt),y
                        inc plzpnt
                        bne rplss
                        inc plzpnt+1

                        savekcs:
                        lda RXBYTE
                        rplss:
                        ldy #0
                        sta (SVP),y
                        inc SVP
                        bne samepage
                        inc SVPH
                        samepage:
                        ;jsr printbyte
                        ;lda #' '
                        ;jsr printk
                        ; Here we should decide if we're going for 8 or 11 bits - buts lets assume 11.
                        lda #11
                        sta CBIT
                        morebits:
                        rts

                        kcsleader:
                        lda TAPEFLAGS
                        ora #$60  ; Set leader flag
                        sta TAPEFLAGS
                        lda #10 ; First time we already have the start bit
                        sta CBIT
                        rts

                        aprsleader:
                        bit TAPEFLAGS
                        bvs savebyte
                        lda TAPEFLAGS
                        ora #$e4 ; NRZ and save bits
                        sta TAPEFLAGS
                        bne savebyte ; BRA

simpledelay:
ldx #0
delay:
dex
NOP
NOP
NOP
NOP
NOP
NOP
bne delay
rts


i2c_start: ; Let's assume i2c addr is in A and RW bit is in C
  rol ; Move address to top bits and RW bit to bit0
  sta outb ; Save addr + rw bit

  lda #SCL_INV ; Start with SCL as INPUT HIGH
  and DDRB
  sta DDRB

  lda #SDA ; Insure SDA is output low before SCL is LOW
  ora DDRB
  sta DDRB
  lda #SDA_INV
  and PORTB
  sta PORTB

  lda #SCL_INV
  and PORTB
  sta PORTB
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
  ; Fall through to send address + RW bit

  ; From here on we can assume OUTPUTs are LOW and INPUTS are HIGH.
  ; Maybe some of the juggling above is not necessary but let's not assume for now
i2cbyteout:
  lda #SDA_INV ; In case this is a data byte we set SDA LOW
  and PORTB
  sta PORTB
  ldx #8
i2caddrloop: ; At start of loop SDA and SCL are both OUTPUT LOW
  asl outb ; Put MSB in carry
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
  bcc seti2cbit0 ; If data bit was low
  lda DDRB       ; else set it high
  and #SDA_INV
  sta DDRB
  bcs wasone ; BRA
seti2cbit0:
  lda DDRB
  ora #SDA
  sta DDRB
  wasone:
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input; Let SCL go HIGH by setting it to input
  dex
  bne i2caddrloop

  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT ; Set SCL low after last bit
  lda DDRB ; Set SDA to INPUT
  and #SDA_INV
  sta DDRB
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input
  ; NOP here?
  lda PORTB ; Check ACK bit
  clc
  and #SDA
  bne nack
  sec ; Set carry to indicate ACK
  nack:
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
  rts

i2cbytein: ; Assume SCL is LOW
  lda DDRB       ; Set SDA to input
  and #SDA_INV
  sta DDRB
  lda #0
  sta inb
  ldx #8
byteinloop:
  clc ; Clearing here for more even cycle
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input
  nop
  lda PORTB
  and #SDA
  beq got0
  sec
  got0:
  rol inb ; Shift carry into input byte
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
  dex
  bne byteinloop

  lda DDRB ; Send NACK == SDA high (because we're ony fetching single bytes)
  and #SDA_INV
  sta DDRB
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
rts ; Input byte in inb

i2c_stop:
  lda DDRB ; SDA low
  ora #SDA
  sta DDRB
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input
  nop
  nop
  lda DDRB       ; Set SDA high after SCL == Stop condition.
  and #SDA_INV
  sta DDRB
  rts

i2c_test:
  ;lda #$77 ; Address $77 = BMP180 address
  lda I2CADDR
  clc ; Write
  jsr i2c_start
  ; Fail check here. C == 1 == ACK
  bcc failed
  ;lda #$D0 ; BMP180 register $D0 == chipID == $55
  lda I2CREG ; VGA screen EDID usually has address $50
  sta outb
  jsr i2cbyteout
  ;lda #$77 ; Address
  lda I2CADDR
  sec ; Read
  jsr i2c_start
  jsr i2cbytein
  jsr i2c_stop
  lda inb
  sec
  rts
  failed:
  lda #0
  rts

i2c_read:
  lda I2CADDR
  clc ; We "write" the address we want to read from
  jsr i2c_start
  bcc readfail
  lda #0
  sta outb
  jsr i2cbyteout
  jsr i2c_stop
  lda I2CADDR
  sec ; Now we read the register data
  jsr i2c_start
  jsr i2cbytein
  jsr i2c_stop
  lda inb
  readfail:
  rts



rubout:
ldy #0
lda CRSRCHR
sta (CRSRPNT),y
normcrsr:
dec CRSRPNT
lda CRSRPNT
cmp #255
bne notunder
dec CRSRPNT2
notunder:
lda #' '
sta (CRSRPNT),y
dec kb_rptr
dec kb_wptr
dec kb_wptr
rts

    rf_nop:
              lda PORTB
              and #%11111101 ; Set CS Low
              sta PORTB
              lda #$FF
              jsr spibyte
              sta RF_STS
              lda PORTB
              ora #2 ; Set CS high
              sta PORTB
              rts

          ;rw_reg takes command in x and data in A. Returns data in A.
    rw_reg:
              pha
              lda PORTB
              and #%11111101 ; Set CS Low
              sta PORTB

              txa
              jsr spibyte
              sta RF_STS

              pla
              jsr spibyte
              ; A has return value
              pha

              lda PORTB
              ora #2 ; Set CS high
              sta PORTB

              pla ; A has return value
              rts


    initrf24:
              ldx #$E2 ; Flush RX
              lda #0
              jsr rw_reg

              ldx #$27 ; Clear RX_DR - $20 | $07, write address $07
              lda #$40 ; %01000000 RX_DR mask
              jsr rw_reg

              ldx #$3D ; Set FEATURE register - $20 | $1D, write address $1D
              lda #4
              jsr rw_reg

              ldx #$3C ; Set DYNDP register
              lda #$3F
              jsr rw_reg

              ldx #$20 ; Power up, RX
              lda #$0F
              jsr rw_reg

              lda PORTB ; Set CE high
              ora #4 ; CE is PB.2
              sta PORTB
              rts

    readrf24regs:
                  ldx #31
    readrf24:
                  lda #0 ; Let's read first byte of all registers
                  jsr rw_reg
                  sta RF24REGS,x
                  dex
                  bpl readrf24
                  rts

    getmessage:
    ; Here we read the msg
    ldx #$60 ; Get top of RX fifo length - R_RX_PL_WID
    lda #0
    jsr rw_reg
    sta RF24REGS+$11;$81 ; Overwrite P0 payload length location in mem

   ldy #0
   lda PORTB
   and #%11111101 ; Set CS Low
   sta PORTB
   readpayload:
   lda #$61
   jsr spibyte
   sta RF_STS
   bytes:
   jsr spibyte
   sta MSGBUF, y ;
   iny
   cpy RF24REGS+$11
   bne bytes
   lda PORTB
   ora #2 ; Set CS high
   sta PORTB

   ldx #$27 ; Clear RX_DR
   lda #$40
   jsr rw_reg
   rts

   ;  Wait for keypress subroutine -
                wkey:
                        jsr checkkeyboard
                        beq wkey
                havekey:
                        lda kb_buffer, x
                        inc kb_rptr
                        rts

                        delay20ms:
                        ldx MILLISH
                        lda MILLIS
                        cpx MILLISH
                        bne delay20ms ; In case we hit bad cycle
                        clc
                        adc #5 ; 20ms
                        bcc stallhere
                        inx
                        stallhere:
                        cpx MILLISH
                        bne stallhere
                        cmp MILLIS
                        bne stallhere
                        rts

setpulses:
                        ; Here we load some default values if not specified something non-zero
                        lda PULSESL ; Number of pulses pr lower frequency bit
                        bne splsh
                        lda #KCS300PL ; == 4 == 1200 Hz / 4 == 300 baud
                        sta PULSESL
                        splsh:
                        lda PULSESH
                        bne plss
                        lda #KCS300PH ; == 8 == 2400 Hz / 8 == 300 baud
                        sta PULSESH
                        plss:
                        rts

                      ;  APPLE := 1
                      ;  .include "msbasic/defines_apple.s"
.ifdef BASIC
                        OSI := 1
                        .include "defines_abn6502.s"

                        ;CBM2 := 1
                        ;.include "msbasic/defines_cbm2.s"

                        ;KIM := 1
                        ;.include "msbasic/defines_kim.s"

                       .include "msbasic/msbasic.s"
                      ;  lda #LIST
.else
.segment "INIT"
.segment "HEADER"
.segment "VECTORS"
.segment "KEYWORDS"
.segment "ERROR"
.segment "CODE"
.segment "CHRGET"
.segment "EXTRA"
.segment "DUMMY"
.endif

; Inline printing routine from http://6502.org/source/io/primm.htm
.segment "PRIMM"
.org $ffc8
   PRIMM:
   	PHA     		; save A
   	TYA			; copy Y
   	PHA  			; save Y
   	TXA			; copy X
   	PHA  			; save X
   	TSX			; get stack pointer
   	LDA $0104,X		; get return address low byte (+4 to
   				;   correct pointer)
   	STA PRIMMZP1			; save in page zero
   	LDA $0105,X		; get return address high byte (+5 to
   				;   correct pointer)
   	STA PRIMMZP2			; save in page zero
   	LDY #$01		; set index (+1 to allow for return
   				;   address offset)
   PRIM2:
   	LDA (PRIMMZP1),Y		; get byte from string
   	BEQ PRIM3		; exit if null (end of text)

   	JSR printk		; else display character
   	INY			; increment index
   	BNE PRIM2		; loop (exit if 256th character)

   PRIM3:
   	TYA			; copy index
   	CLC			; clear carry
   	ADC PRIMMZP1			; add string pointer low byte to index
   	STA $0104,X		; put on stack as return address low byte
   				; (+4 to correct pointer, X is unchanged)
   	LDA #$00		; clear A
   	ADC PRIMMZP2		; add string pointer high byte
   	STA $0105,X		; put on stack as return address high byte
   				; (+5 to correct pointer, X is unchanged)
   	PLA			; pull value
   	TAX  			; restore X
   	PLA			; pull value
   	TAY  			; restore Y
   	PLA  			; restore A
   	RTS


.segment "VECTORS6502"
.ORG $fffa
.word nmi,reset,irq
.reloc
