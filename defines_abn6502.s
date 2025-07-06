; configuration
CONFIG_11A := 1

CONFIG_DATAFLG := 1
;CONFIG_NULL := 1
;CONFIG_PRINT_CR := 1 ; print CR when line end reached
CONFIG_SCRTCH_ORDER := 3
CONFIG_SMALL := 1

ZP_START1 = $00; $00
ZP_START2 = $0D ;$0D
ZP_START3 = $5b; $5B ;10b
ZP_START4 = $65 ;$65 ; 114b


CRLF_1 := $0A
CRLF_2 := $00

;extra ZP variables
USR             := $000A; $000A

; constants
STACK_TOP		:= $80
SPACE_FOR_GOSUB := $33
NULL_MAX		:= $0A
WIDTH			:= 40
WIDTH2			:= 40

; memory layout
RAMSTART2		:= $1000

; magic memory locations
L0200           := $0200

; monitor functions
;MONRDKEY        := $FFEB
;MONCOUT         := printk ;$FFEE
;MONISCNTC       := clcrts;$FFF1
LOAD            := basicloadfromtape ;$FFF4
SAVE            := basicsavetotape ;$FFF7
