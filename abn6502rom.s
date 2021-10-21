.feature string_escapes ; Allow c-style string escapes when using ca65
.feature org_per_seg
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
ACR  = $600B
PCR  = $600C
IFR  = $600D
IER  = $600E
PORTANHS = $600F

TIMEOUT = 1999 ; Should be around 1ms

kb_wptr = $0000
kb_rptr = $0001
kb_flags = $0002

RELEASE = %00000001
SHIFT   = %00000010

kb_buffer = $0200  ; 256-byte kb buffer 0200-02ff

MILLIS = $40
MILLISH = $41

HXH = $44
MONCNT = $46

TMP = $50;

CRSRPNT = $52
CRSRPNT2 = CRSRPNT+1
CHPNT = $54
CHPNT2 = CHPNT+1
SCREENSTARTH = $20
SCREENSTARTL = $4B
LINESTART = 11

MONH = $59
MONL = $58
ABUF = $5A
;to $61

uservia = PORTB

.segment "RODATA"
;.org $8000 Not needed with CA65
  nmi:
  reset:
          cld ; Because you never know

          ;CLEAR RAM
          ldx #$0
  clearram:
          lda #0
          sta $00,x
          inx
          bne clearram

          lda #$01  ; CA1 positive active edge
          sta PCR
          lda #%11000010  ; Set CA1 + T1 interrupts
          sta IER
          LDA #%01011000
          STA ACR             ; T1 continuous, PB7 disabled, Shift Out Ø2
          cli       ; Enable interrupts

   noclear: ;Soft reset point
          LDA #<TIMEOUT
          STA T1CL            ; Set low byte of timer1 counter
          LDA #>TIMEOUT
          STA T1CH            ; Set high byte of timer1 counter

          lda #SCREENSTARTL
          sta CRSRPNT
          lda #SCREENSTARTH
          sta CRSRPNT2
          lda #$FF
          sta DDRA
          tax
          txs

          jsr clrscn

          lda #2
          sta PORTB ; Set SPI CS high
          lda #%01000111 ; Port B DDR for SPI
          sta DDRB
          lda #0
          sta DDRA ; Port A all inputs

          sta kb_rptr ; Init keyboard pointers before enabling interrupts
          sta kb_wptr
          cli

          ;lda #0 ; Don't have to do this because we start by clearing RAM
          ;sta MONL
          ;sta MONH

          ldx #0
loadwelcome:
          lda message,x
          beq main
          sta kb_buffer,x
          inc kb_wptr
          inx
          jmp loadwelcome
          lda CRSRPNT ; Newline after welcome.
          jsr crnl
          sta CRSRPNT

main:

nomsg:

    lda CRSRPNT
    pha
    lda CRSRPNT2
    pha
    LDA #$7c
    sta CRSRPNT
    lda #$20
    sta CRSRPNT2
    lda MILLISH
    jsr printbyte
    pla
    sta CRSRPNT2
    pla
    sta CRSRPNT

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


      jsr printk

      jmp main

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

strangegame:
  ldx #0
  lda #0
  sta kb_wptr
  sta kb_rptr
printgtnw:
  lda gtnw,x
  beq wrotegtnw
  sta kb_buffer,x
  inc kb_wptr
  inx
  jmp printgtnw
  lda CRSRPNT ; Newline after welcome.
  jsr crnl
  sta CRSRPNT
wrotegtnw:
  jmp main

write:
      ldy #$FF
      inx
      lda kb_buffer,x
      cmp #'r'
      bne err
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
      inc kb_rptr
      jmp newmon


read:
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
      inc kb_rptr
      jmp newmon


err:
      lda CRSRPNT
      jsr crnl
      sta CRSRPNT
      bcc clkbptr
      inc CRSRPNT2
clkbptr:
      ; Clear keyboard pointers if this is the end of message
      inc kb_rptr
      sei
      lda kb_rptr
      cmp kb_wptr
      cli
      bne notdone
eom:
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
        dec CRSRPNT
        lda CRSRPNT
        cmp #255
        bne notunder
        dec CRSRPNT2
        notunder:
        lda #' '
        ldy #0
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
        jmp eom

f5_pressed:

        jmp f2_pressed

f6_pressed:
        jmp f2_pressed

f7_pressed:
        jmp f2_pressed

f8_pressed:
        jmp f2_pressed

        newmon:
            jsr clrscn
        monmon:
            lda #27
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
            cmp #$1b           ; escape - exit
            beq exitmon

            ; Let's loop here until keypress
            jmp monmon ; No data - restart monitor

        exitmon:
            jsr clrscn ; Clear screen on monitor exit
            jmp eom
        ;    lda #0
        ;    sta MILLISH

asctohex:
              cmp #$60
              bcc caps
              sbc #$20
caps:
              sbc #'0'-1
              cmp #10
              bcc nothex
              sbc #8
nothex:
        rts

blink:
          lda #$AA
          sta PORTA
          jsr delay
          lda #$55
          sta PORTA
          jsr delay
          rts

  delay: ;
          lda #1
          sta $25
  inner:
          lda $25 ; Keep loading T1L until it rolls over
          bne inner
          rts

clrscn:
    lda #0
    sta CRSRPNT
    lda #$1F ; Clear before screen to after screen
    sta CRSRPNT+1
    ldy #0
    tya
    ldx #9
    clrloop:
    sta (CRSRPNT),y
    iny
    bne clrloop
    inc CRSRPNT+1  ; increasing HI-byte of the clearing address.
    dex
    bne clrloop
    rts

crnl: ; Carriage return new line - needs cursor pointer in A
    and #%11000000 ; keep only section bits
    ora #LINESTART ;
    clc
    adc #$40 ; CR
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
    ldy #0
    sta (CRSRPNT),y
    inc CRSRPNT
    bne printeda
    inc CRSRPNT2
printeda:
    rts

printk:
      pha
checkl: ; This is user input, so we have to make sure we don't hit VGA blanking by mistake
    lda CRSRPNT
    and #$3F   ; Discard MS bits since we only care about current line
    cmp #$3e
    bcc chs ; A < 62 == Not Front porch
    lda CRSRPNT
    jsr crnl
    sta CRSRPNT
    and #$E0 ; If msn is 0 then ++section
    bne chs
    inc CRSRPNT2
chs:
    lda CRSRPNT2 ; Check if we're off screen
    cmp #$27
    bcc rpa ; if we're not
    lda #SCREENSTARTH ; If we are, then reset to top of screen ; Or start scrolling?
    sta CRSRPNT2
    lda #SCREENSTARTL
    sta CRSRPNT

rpa:
    pla
    jsr printa
    inc kb_rptr
    rts

message:	.asciiz "GREETINGS PROFESSOR FALKEN.", "\n", "SHALL WE PLAY A GAME?", "\n"

gtnw:
  .asciiz "\n", "A STRANGE GAME. THE ONLY WINNING ", "\n", "MOVE IS NOT TO PLAY.", "\n"

  ; IRQ vector points here ; Thanks to Ben Eater for a very useful PS2->Ascii interface
irq:
  pha
  txa
  pha
  tsx
  lda $0103,x ; Pull status register off stack and check break flag
  and #$10
  bne hitbrk
  bit IFR
  bvs t1_irq
  lda IFR
  and #2
  bne keyboard_interrupt
  rti

hitbrk:
  ; jmp reset
  inc $08
  lda #$55
  sta $6000
  jmp noclear ; Exit brk irq by going to soft reset

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

  shift_up:
    lda kb_flags
    eor #SHIFT  ; flip the shift bit
    sta kb_flags
    jmp exit

  read_key:
    lda PORTA
    cmp #$f0        ; if releasing a key
    beq key_release ; set the releasing bit
    cmp #$12        ; left shift
    beq shift_down
    cmp #$59        ; right shift
    beq shift_down

    tax
    lda kb_flags
    and #SHIFT
    bne shifted_key

    lda keymap, x   ; map to character code
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

  exit:
    pla
    tax
    pla
    rti

t1_irq:
    bit T1CL ; Clear irq
    inc $25
    inc MILLIS
    bne t1_irq_exit
    inc MILLIS+1
t1_irq_exit:
    jmp exit

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
      .byte "0.2568",$1b,"??+3-*9??" ; 70-7F
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

.segment "VECTORS"
.ORG $fffa
.word nmi,reset,irq
.reloc
