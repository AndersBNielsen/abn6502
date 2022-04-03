
PRIMM = $FFC8 ; Userland can use ROM subroutines if we tell them where they are. Check listing.txt for current subroutine addresses
.feature string_escapes ; Allow c-style string escapes when using ca65

.segment "USERLAND"
userland:
  jsr PRIMM
  .asciiz "Testing CPU...\n"
cOhtwotest:
  bra see02
  jsr PRIMM
  .asciiz "Hi! Im an NMOS 6502! \n"
  jmp exitnow
  see02:
  jsr PRIMM
  .asciiz "Hi! Im a CMOS 65C02! \n"
  exitnow:
  jsr PRIMM
  .asciiz "Program finished. Returning to main.\n"
  brk
