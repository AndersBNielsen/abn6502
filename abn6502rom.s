.feature string_escapes ; Allow c-style string escapes when using ca65
.feature org_per_seg
DEBUG = 0
PORTB = $6000
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
ACR  = $600B ; [7] PB.7 T1 OUT, [6] T1 mode , [5] T2, [4:2] Shift register control, [1] PB Latch enable, [2] PA Latch Enable
PCR  = $600C ; [7:5] CB2 Control, [4] CBl Control, [3:1] CA2 Control, [0] CAl Control
IFR  = $600D ; [7:0] IRQ Tl T2 CBl CB2 SR CA1 CA2
IER  = $600E ; [7:0] S/C Tl T2 CBl CB2 SR CA1 CA2
PORTANHS = $600F
CTRL = $5000

TIMEOUT = 7998 ; Should be around 4ms

kb_wptr = $0000
kb_rptr = $0001
kb_flags = $0002
kb_last = $0004

RELEASE = %00000001
SHIFT   = %00000010
ECODE   = %00000100
CRSR    = %01000000
CRSRF   = %10000000

kb_buffer = $0200  ; 256-byte kb buffer 0200-02ff

;Custom keyboard mappings
DN_ARR_KEY = $F3
UP_ARR_KEY = $F4
PGUP_KEY =   $F6
PGDN_KEY =   $F5
L_ARR_KEY = $F2
R_ARR_KEY = $F1
HOME_KEY = $F0
ESC_KEY  =  $1b

CRSRT   = $1B
CRSRCHR = $1C
RF_ERR = $1D
ERRS = $1F


MILLIS = $40
MILLISH = $41

HXH = $44
MONCNT = $46

SLB =  $47 ; Size low byte
SHB = $48
SVP = $42 ; Save pointer
SVPH =$43

TMP = $50;
TMP2 = $51;
CRSRPNT = $52
CRSRPNT2 = CRSRPNT+1

SCREENSTARTH = $20
SCREENSTARTL = $4B
LINESTART = 11
NUMLINES = 30

MONH = $59
MONL = $58
ABUF = $5A
;to $61

outb  = $56
inb = $57

RF_STS = $68
; CE = $69
MSGBUF = $90
;+32bytes == To $AF

uservia = PORTB
mosi  = %01000000

.segment "RODATA"
;.org $8000 ; Not strictly needed with CA65 but shows correct address in listing.txt
.list on ; Does this work?
  nmi:
  reset:
          cld ; Because you never know

          ;CLEAR RAM
          sei
          ldx #$0
  clearram:
          lda #0
          sta $00,x
          inx
          bne clearram

          ;Assumes x and A are 0 from above
  clearstack:
          sta $0100,x
          inx
          bne clearstack

noclear: ;Soft reset point - BRK
          lda #$01  ; CA1 positive active edge
          sta PCR
          sta CTRL ; Enable video, bank 0
          lda #%11000010  ; Set CA1 + T1
          sta IER
          LDA #%01011000
          STA ACR             ; T1 continuous, PB7 disabled, Shift Out Ø2
          ;cli       ; Enable interrupts Let's do this when KB is ready

          LDA #<TIMEOUT
          STA T1CL            ; Set low byte of timer1 counter
          LDA #>TIMEOUT
          STA T1CH            ; Set high byte of timer1 counter

          jsr clrscn

          lda #SCREENSTARTL
          sta CRSRPNT
          lda #SCREENSTARTH
          sta CRSRPNT2
          lda #$FF
          sta DDRA
          tax
          txs

          lda #2
          sta PORTB ; Set SPI CS high
          lda #%01010111 ; Port B DDR for SPI
          sta DDRB
          lda #0

          sta DDRA ; Port A all inputs for keyboard
          sta kb_rptr ; Init keyboard pointers before enabling interrupts
          sta kb_wptr
          sta RF_ERR ; Reset RF error
          cli

          ldx #$10 ; Read first tx addr byte = should be default, if not then no module connected
          jsr rw_reg
          cmp #$E7
          beq welcome
          inc RF_ERR ; No module, set RF_ERR
welcome:
          jsr PRIMM
message:	.asciiz "GREETINGS PROFESSOR FALKEN.", "\n", "SHALL WE PLAY A GAME?", "\n"

welcomedone:

          bit ACR
          bmi skiprf
          lda RF_ERR
          bne skiprf
          sei
          jsr initrf24
          cli

main:
lda RF_ERR
bne skiprf
bit ACR ; If ACR.7 is set then we can't use SPI since MOSI is outputting TM1.
bpl rfstuff
skiprf:
jmp nomsg
rfstuff:
;jsr readrf24regs ; Debug - we can also just do an rf_nop to read rf24 status (RF_STS)

jsr rf_nop ; Not debug
cli
;If msg received, put it in MSGBUF
bit RF_STS
bvs gtgm ; Check irq
lda RF_STS
cmp #$0e ; Check if fifo empty
bne gtgm
jmp nomsg; No msg received
gtgm:
sei
jsr getmessage

lda $90
bne datapacket  ; Check for control message
lda $91
cmp #$31 ; Trust but verify
beq ctrlmsg
jsr initrf24 ; Junk package. Reset radio.
jmp nomsg
ctrlmsg:
lda $92
sta $d0 ; Data size low byte
lda $93
sta $d1 ; Data size high byte

  jsr PRIMM
  .asciiz "Receiving $"
  lda $d1
  jsr printbyte
  lda $d0
  jsr printbyte
  jsr PRIMM
  .asciiz " bytes. \n"

datapacket:

getmsg:
inc $d2
lda $90
cmp #1
bne nextpacket ; Data package with ID > 1
lda $d1 ; Size high byte
sta SHB
inc SHB
lda $d0 ; Size low byte
sta SLB
lda #0
sta SVP
lda #3
sta SVP+1 ; Save pointer starts at $0300

nextpacket:
ldx #2
fetchpacket:
lda $90,x
ldy #0
sta (SVP),y
inc SVP
bne nnhb
inc SVP+1
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
.asciiz " bytes left(hex)"
lda CRSRPNT
and #%11000000 ; keep only section bits
ora #LINESTART ;
sta CRSRPNT
inx
cpx #32
bne fetchpacket
jmp main

txdone:
    lda #3
    sta $31
    lda #0
    sta kb_rptr ; Reset the keyboard pointers here.
    sta kb_wptr
    sta $30
    lda CRSRPNT ; Let's return to beginning of the line
    and #%11000000 ; keep only section bits
    ora #LINESTART ;
    sta CRSRPNT
    lda SHB
    jsr printbyte
    lda SLB
    jsr printbyte
    jsr PRIMM
    .asciiz " bytes left(hex)\n"
    jsr PRIMM
    .asciiz "\nData loaded into RAM at $0300. \nPress F5 or type \"run\" to start executing at $0300. \n"
    jsr rf_nop
    cli

nomsg:

    bit CRSRT
    bmi isneg
    lda kb_flags
    and #$7f
    sta kb_flags ; Reset flip
    sec
    bcs skippedcursor ; BRA

    isneg:
    bit kb_flags ; Same as last?
    bpl flip
    bmi skippedcursor ; We already flipped

flip:

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


.if DEBUG
    lda CRSRPNT ; Save cursor..
    sta $54
    pha
    lda CRSRPNT2
    sta $55
    pha
    LDA #$7c  ; Print timer in upper right corner of screen
    sta CRSRPNT
    lda #$27
    sta CRSRPNT2
    lda MILLISH
    jsr printbyte

    LDA #$74 ; Debug ERRS and show cursor pointer next to timer on screen
    sta CRSRPNT
    lda #$27
    sta CRSRPNT2
    lda ERRS
    jsr printbyte
    inc CRSRPNT
    lda $55
    jsr printbyte
    lda $54
    jsr printbyte

    pla
    sta CRSRPNT2
    pla
    sta CRSRPNT
.endif
    sei
    lda kb_rptr
    cmp kb_wptr
    cli
    bne key_pressed
    jmp main

key_pressed:
      ldx kb_rptr
      lda kb_buffer, x
      cmp #$0a           ; enter - go new line
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

      cmp #L_ARR_KEY
      beq golrudarr
      cmp #R_ARR_KEY
      beq golrudarr
      cmp #UP_ARR_KEY
      beq golrudarr
      cmp #DN_ARR_KEY
      beq golrudarr

      jsr printk
printedkey:
      inc kb_rptr

      jmp main

golrudarr:
      jmp lrudarr

back:
      jmp backspace_pressed
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
      ldy #0
      lda (MONL),y
      jsr PRIMM
      .asciiz "\nRead: "
      jsr printbyte
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
      sei
      lda kb_rptr
      cmp kb_wptr
      cli
      bne notdone
eom:
      lda (CRSRPNT),y ; Save new char under cursor ; Here or in err:?
      sta CRSRCHR
      lda #0
      sta kb_rptr
      sta kb_wptr
    notdone:
      jmp main

run:
      lda #0
      sta kb_rptr
      sta kb_wptr
      jmp ($0030)

esc_pressed:
      jsr clrscn
      lda #SCREENSTARTL
      sta CRSRPNT
      lda #SCREENSTARTH
      sta CRSRPNT2
      jmp clkbptr

backspace_pressed:
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
        jmp main

f1_pressed:
        lda #0
        sta kb_rptr
        sta kb_wptr
        jsr clrscn
        jmp monmon

f2_pressed:

        jmp eom

f3_pressed:

        jmp eom

f4_pressed:
        jsr getmessage
        jsr PRIMM
        .asciiz "Got message!"
        jsr newline
        jmp eom

f5_pressed:
        jsr rf_nop
        jmp run

f6_pressed:
        jsr readrf24regs
        jsr PRIMM
        .asciiz "Read RF24 configuration!"
        jsr newline
        jmp eom

f7_pressed:
        jsr initrf24
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

            sei
            lda kb_rptr
            cmp kb_wptr
            cli
            beq monmon
            ldx kb_rptr
            inc kb_rptr
            lda kb_buffer, x
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

clrscn:
    lda #0
    sta CRSRPNT
    lda #$20 ; Clear before screen to after screen
    sta CRSRPNT2
    ldy #0
    tya
    ldx #9
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
    jsr bytetoa
    pha
    lda HXH
    jsr printa
    pla
    jsr printa
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
    sbc #$40
    ora #$3E
    sta CRSRPNT
    bcs checkedarrows
    dec CRSRPNT2
    bcc checkedarrows ; BRA
rarr:
    inc CRSRPNT
    lda CRSRPNT
    and #$3F   ; Discard MS bits since we only care about current line
    cmp #$3f
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
    cmp #$3F
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
    lda CRSRPNT2 ; Check if we're off screen
    cmp #$20
    bcc resetcursor ; Off screen
    cmp #$27
    bcc checkedbottom ; if we're not
lda CRSRPNT ; Check LSB as well if we're above $2700
cmp #$80
bcc checkedbottom
resetcursor:
lda #SCREENSTARTH ; If we are, then reset to top of screen ; Or start scrolling?
sta CRSRPNT2
lda #SCREENSTARTL
sta CRSRPNT
checkedbottom:
    rts

  ; IRQ vector points here ; Thanks to Ben Eater for a very useful PS2->Ascii interface
  ;IFR is IRQ Tl T2 CBl CB2 SR CA1 CA2
irq:
  pha
;  lda #%00100000 ; We need T2 to fire super fast, so we check it first.
;  and IFR
;  bne t2_irq

;Alt approach
; lda IER
; and IFR ; We only care about active IRQ's
; asl ; IRQ in C
; asl ; T1 flag in C
; bcs t1_irq
; asl ; T2
; bcs t2_irq
; asl ; CB1
; asl ; CB2
; asl ; SR
; asl ; CA1
; bcs keyboard_interrupt
; asl ; CA2

  bit IFR ; T1 as fast as possible
  bvs t1_irq

  txa
  pha
  tsx
  lda $0103,x ; Pull status register off stack and check break flag
  and #$10
  bne hitbrk

  lda IFR
  and #2
  bne keyboard_interrupt
  inc ERRS ;Should never end up here...
  rti

hitbrk:
  ; jmp reset
  inc $08
  lda #<main
  sta $104,x ; Return to main instead of breakpoint
  lda #>main
  sta $105,x
jmp exit

  keyboard_interrupt:
    lda kb_flags
    and #RELEASE   ; check if we're releasing a key
    beq read_key   ; otherwise, read the key

    lda kb_flags
    eor #RELEASE   ; flip the releasing bit
    sta kb_flags
    lda PORTA      ; read key value that's being released
    cmp #$12       ; left shift
    beq shift_up
    cmp #$59       ; right shift
    beq shift_up
    jmp exit

    t1_irq:
        bit T1CL ; Clear irq

    gmillis:
        inc CRSRT
        inc CRSRT
        inc MILLIS
        bne t1_irq_exit
        inc MILLIS+1
    t1_irq_exit:
        pla
        rti



;        t2_irq:
;        lda #%00010000
;        eor PORTB
;        sta PORTB
;        lda #0
;        sta T2CH ; pla + rti should be enough time for IRQ line to go high again before leaving IRQ - at 2 mhz anyway
;        pla
;        rti

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
    lda PORTA
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

    tax
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
    pla
    tax
    pla
    rti


; Thanks to Ben Eater for a very useful PS2->Ascii interface
    keymap:
      .byte "???",$FA,$FC,$FE,$FD,"???",$F7,$F9,$FB," `?" ; 00-0F
      ;F1, F2, F3,F4, F5, F6, F7, F8 key bound to $FE, $FD, $FC, $FB, $FA, $F9, $F8, $F7 for no particular reason
      .byte "?????q1???zsaw2?" ; 10-1F
      .byte "?cxde43?? vftr5?" ; 20-2F
      .byte "?nbhgy6???mju78?" ; 30-3F
      .byte "?,kio09??./l;p-?" ; 40-4F
      .byte "??\'?[=????",$0a,"]?\\??" ; 50-5F
      .byte "??????",$08,"??1?47???" ; 60-6F
      .byte "0.2568",ESC_KEY,"??+3-*9??" ; 70-7F
      .byte "???",$F8,"????????????" ; 80-8F $F8 = F7 key
      .byte "????????????????" ; 90-9F
      .byte "????????????????" ; A0-AF
      .byte "????????????????" ; B0-BF
      .byte "????????????????" ; C0-CF
      .byte "????????????????" ; D0-DF
      .byte "????????????????" ; E0-EF
      .byte "????????????????" ; F0-FF
    keymap_shifted:
      .byte "????????????? ~?" ; 00-0F
      .byte "?????Q!???ZSAW@?" ; 10-1F
      .byte "?CXDE#$?? VFTR%?" ; 20-2F
      .byte "?NBHGY^???MJU&*?" ; 30-3F
      .byte "?<KIO)(??>?L:P_?" ; 40-4F
      .byte "??\"?{+?????}?|??" ; 50-5F
      .byte "?????????1?47???" ; 60-6F
      .byte "0.2568???+3-*9??" ; 70-7F
      .byte "???",$F8,"????????????" ; 80-8F $F8 = F7 key
      .byte "????????????????" ; 90-9F
      .byte "????????????????" ; A0-AF
      .byte "????????????????" ; B0-BF
      .byte "????????????????" ; C0-CF
      .byte "????????????????" ; D0-DF
      .byte "????????????????" ; E0-EF
      .byte "????????????????" ; F0-FF
      keymap_ecode:
        .byte "0000000000000000" ; 00-0F
        .byte "0000000000000000" ; 10-1F
        .byte "0000000000000000" ; 20-2F
        .byte "0000000000000000" ; 30-3F
        .byte "0000000000000000" ; 40-4F
        .byte "0000000000000000" ; 50-5F
        .byte "0123456789A",L_ARR_KEY, HOME_KEY, "DEF" ; 60-6F
        .byte "01",DN_ARR_KEY,"3",R_ARR_KEY, UP_ARR_KEY, "6789",PGDN_KEY,"BC",PGUP_KEY,"EF" ; 70-7F $F6 = PGUP(7D), $F5 = PGDOWN(7A), $F4 = UP Arr.(75), Down arr.(72) = $F3
        .byte "0000000000000000" ; 80-8F $F8 = F7 key
        .byte "????????????????" ; 90-9F
        .byte "????????????????" ; A0-AF
        .byte "????????????????" ; B0-BF
        .byte "????????????????" ; C0-CF
        .byte "????????????????" ; D0-DF
        .byte "????????????????" ; E0-EF
        .byte "????????????????" ; F0-FF


    spibyte:
          ;SR borrowed from http://www.cyberspice.org.uk/blog/2009/08/25/bit-banging-spi-in-6502-assembler/
          ;I think GW used this way too?
          ;Bit 7 – MISO (Input data from the slave to the computer) ; We can swap these by doing bvc spibyte2 instead of bpl spibyte2 and fixing DDRB - we should prefer PB7 to be an output and PB6 an input because of T1 and T2 hw features(T1 out to PB7 and T2 in to PB6)
          ;Bit 6 – MOSI (Output data from the computer to the slave)
          ;Bit 1 – SCS (Output slave chip select)
          ;Bit 0 – SCLK (Output SPI clock)
                      sta outb
                      lda PORTB
                      and #%00111100
                      sta $1E ; Save bits not used by SPI
                      TXA
                      PHA
                      TYA
                      PHA

                      ldy #0
                      sty inb
                      ldx #8
    spibytelp:
                      tya		; (2) set A to 0
                      asl outb	; (5) shift MSB in to carry
                      bcc spibyte1	; (2)
                      ora #mosi	; (2) set MOSI if MSB set
    spibyte1:
                      ora $1E ; Debug - set preserved port B bits. If interrupts are enabled be careful when using this.
                      sta uservia	; (4) output (MOSI, SCS low, SCLK low)
                      tya		; (2) set A to 0 (Do it here for delay reasons)
                      inc uservia	; (6) toggle clock high (SCLK is bit 0)
                      clc		; (2) clear C (Not affected by bit)
                      bit uservia	; (4) copy MISO (bit 7) in to N (and MOSI in to V)
                      bpl spibyte2	; (2) bvc if miso bit 6, bpl if miso bit 7
                      sec		; (2) set C if MISO bit is set (i.e. N) / Or V for miso on bit 6
    spibyte2:
                      rol inb		; (5) copy C (i.e. MISO bit) in to bit 0 of result
                      dec uservia	; (6) toggle clock low (SCLK is bit 0)
                      dex		; (2) next bit
                      bne spibytelp	; (2) loop

                      PLA
                      TAY
                      PLA
                      TAX

                      lda inb		; get result
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
                  sta $70,x
                  dex
                  bpl readrf24
                  rts

    getmessage:
    ; Here we read the msg
    ldx #$60 ; Get top of RX fifo length - R_RX_PL_WID
    lda #0
    jsr rw_reg
    sta $81 ; Overwrite P0 payload length location in mem

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
   sta $90, y ;
   iny
   cpy $81
   bne bytes
   lda PORTB
   ora #2 ; Set CS high
   sta PORTB

   ldx #$27 ; Clear RX_DR
   lda #$40
   jsr rw_reg
   rts


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
   	STA $BC			; save in page zero
   	LDA $0105,X		; get return address high byte (+5 to
   				;   correct pointer)
   	STA $BD			; save in page zero
   	LDY #$01		; set index (+1 to allow for return
   				;   address offset)
   PRIM2:
   	LDA ($BC),Y		; get byte from string
   	BEQ PRIM3		; exit if null (end of text)

   	JSR printk		; else display character
   	INY			; increment index
   	BNE PRIM2		; loop (exit if 256th character)

   PRIM3:
   	TYA			; copy index
   	CLC			; clear carry
   	ADC $BC			; add string pointer low byte to index
   	STA $0104,X		; put on stack as return address low byte
   				; (+4 to correct pointer, X is unchanged)
   	LDA #$00		; clear A
   	ADC $BD		; add string pointer high byte
   	STA $0105,X		; put on stack as return address high byte
   				; (+5 to correct pointer, X is unchanged)
   	PLA			; pull value
   	TAX  			; restore X
   	PLA			; pull value
   	TAY  			; restore Y
   	PLA  			; restore A
   	RTS


.segment "VECTORS"
.ORG $fffa
.word nmi,reset,irq
.reloc
