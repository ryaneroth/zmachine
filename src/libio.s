; KIM-1 monitor I/O wrappers
; OUTCH/PRTBYT clobber Y. Preserve caller registers.

io_tmp_a = $06F0
io_tmp_x = $06F1
io_tmp_y = $06F2
io_tmp_c = $06F3

newline:
  lda #$0D
  jsr print_char
  lda #$0A
  jmp print_char

print_char:
  sta io_tmp_a
  stx io_tmp_x
  sty io_tmp_y
  lda io_tmp_a
  jsr OUTCH
  ldx io_tmp_x
  ldy io_tmp_y
  lda io_tmp_a
  rts

print_hex:
  sta io_tmp_a
  stx io_tmp_x
  sty io_tmp_y
  lda io_tmp_a
  jsr PRTBYT
  ldx io_tmp_x
  ldy io_tmp_y
  lda io_tmp_a
  rts

get_input:
  stx io_tmp_x
  sty io_tmp_y
  jsr GETCH
  sta io_tmp_c
  ldx io_tmp_x
  ldy io_tmp_y
  lda io_tmp_c
  rts
