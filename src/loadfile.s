;----------------------------------------------
; Copy data from SD card to memory location
;----------------------------------------------
copy_size = $27              ; 2 bytes
copy_destination = $29       ; 2 bytes
input_pointer = $2B          ; 11 bytes
print_pointer = $38          ; 2 bytes
zp_sd_address = $40          ; 2 bytes
zp_sd_currentsector = $42    ; 4 bytes
zp_fat32_variables = $46     ; 49 bytes
copy_swap = $7A              ; 4 bytes
menu_count = $7E             ; 1 byte
menu_scan_guard = $7F        ; 1 byte
fat32_workspace = $200       ; 2 pages
buffer = $400

.org $a000

reset:
  ldx #$ff
  txs
;----------------------------------------------
; SD card and FAT32 initilization
;----------------------------------------------
  jsr via_init
  jsr sd_init
  jsr fat32_init
  bcc initsuccess
  ; Error during FAT32 initialization
  lda #'Z'
  jsr print_char
  lda fat32_errorstage
  jsr print_hex
  jsr EXIT
initsuccess:
  ; Open root directory
  jsr fat32_openroot
;----------------------------------------------
; Input prompt
;----------------------------------------------
print_help:
  jsr newline
  ldx #<help_menu
  ldy #>help_menu
  stx print_pointer
  sty print_pointer+1
  jsr print_string
print_prompt:
  jsr newline
  lda #'>'
  jsr print_char
  lda #' '
  jsr print_char
read_prompt_input:
  jsr get_key
  cmp #'L'
  beq load_file
  cmp #'M'
  beq menu_files
  cmp #'H'
  beq print_help
  cmp #'E'
  bne print_prompt
  jsr EXIT
menu_files:
  jsr list_files
  jmp print_prompt
;----------------------------------------------
; File reading
;----------------------------------------------
load_file:
  jsr newline
  ldx #<input_filename
  ldy #>input_filename
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  jsr read_input
  ; Find file by name
  ldx #<input_pointer
  ldy #>input_pointer
  jsr fat32_finddirent
  bcc foundfile
  ; File not found
  jsr newline
  ldx #<file_not_found
  ldy #>file_not_found
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  ; Open root directory
  jsr fat32_openroot
  jmp print_prompt
foundfile:
  jsr newline
  ; Get destination address
  ldx #<memory_destination
  ldy #>memory_destination
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  jsr read_address
  jsr newline
  ; Open file
  jsr fat32_opendirent
  ; Store file size
  lda fat32_bytesremaining 
  sta copy_size
  lda fat32_bytesremaining+1
  sta copy_size+1
  ; Read file contents into buffer
  lda #<buffer
  sta fat32_address
  lda #>buffer
  sta fat32_address+1
  ldx #<reading
  ldy #>reading
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  ; Can hang on the file read sometimes
  jsr fat32_file_read
  ldx #<copying
  ldy #>copying
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  ; Start copy
  jsr start_copy
  ; Return to prompt
  jmp print_prompt
;----------------------------------------------
; Read in filename
;----------------------------------------------
read_input:
  ldx #0
read_prefix_next:
  jsr get_key
  cmp #'.'
  beq period
  cpx #8
  beq max_prefix_character
  sta input_pointer, x
  inx
  jmp read_prefix_next
period:
  cpx #8
  beq max_prefix_character
  lda #' '
  sta input_pointer, x
  inx
  jmp period
read_suffix_next:
  inx
max_prefix_character:
  jsr get_key
  cpx #11
  beq max_suffix_character
  cmp #$0D                   ; Enter key
  beq pad_suffix
  sta input_pointer, x
  jmp read_suffix_next
pad_suffix:
  lda #' '
  sta input_pointer, x
  cpx #11
  beq max_suffix_character
  inx
  jmp pad_suffix
max_suffix_character:
  rts
;----------------------------------------------
; Read in address to copy data to
;----------------------------------------------
read_address:
  ldx #0
read_address_next:
  cpx #2
  beq read_address_done
  jsr get_key                ; read first digit of hex address,
  and #$0F                   ; '0'-'9' -> 0-9
  asl                        ; multiply by 2
  sta copy_destination, x    ; temp store in temp
  asl                        ; again multiply by 2 (*4)
  asl                        ; again multiply by 2 (*8)
  clc
  adc copy_destination, x    ; as result, a = x*8 + x*2
  sta copy_destination, x
  jsr get_key                ; read second digit of hex address
  and #$0F                   ; '0'-'9' -> 0-9
  adc copy_destination, x
  jsr hex_to_dec
  sta copy_destination, x
  inx
  jmp read_address_next
read_address_done:
  rts
;----------------------------------------------
; Print string whos address is in print_pointer
;----------------------------------------------
print_string:
print_string_loop:
  ldy #0
  lda (print_pointer), y     ; get current char
  beq print_string_exit      ; end of string
  jsr print_char             ; OUTCH may clobber Y
  inc print_pointer
  bne print_string_loop
  inc print_pointer+1
  jmp print_string_loop
print_string_exit:
  rts
;----------------------------------------------
; Read one stable key from KIM monitor
; - blocks until non-zero
; - clears bit 7
;----------------------------------------------
get_key:
  jsr get_input
  and #$7F
  beq get_key
  rts
;----------------------------------------------
; Print root directory file menu
;----------------------------------------------
list_files:
  lda #0
  sta menu_count
  sta menu_scan_guard
  jsr fat32_openroot
  jsr newline
  ldx #<available_files
  ldy #>available_files
  stx print_pointer
  sty print_pointer+1
  jsr print_string
list_files_scan_next:
  inc menu_scan_guard
  lda menu_scan_guard
  beq list_files_done         ; stop after 256 scanned entries
  jsr fat32_readdirent
  bcs list_files_done
  and #$18                    ; skip directories and volume labels
  bne list_files_scan_next
  jsr has_valid_base_name
  bcc list_files_scan_next
  jsr is_playable_file
  bcc list_files_scan_next
  jsr newline
  lda #' '
  jsr print_char
  lda #' '
  jsr print_char
  jsr print_dirent_name
  inc menu_count
  lda menu_count
  cmp #16                     ; cap printed entries
  bcc list_files_scan_next
  jsr newline
  ldx #<files_truncated
  ldy #>files_truncated
  stx print_pointer
  sty print_pointer+1
  jsr print_string
list_files_done:
  lda menu_count
  bne list_files_restore_root
  jsr newline
  ldx #<no_playable_files
  ldy #>no_playable_files
  stx print_pointer
  sty print_pointer+1
  jsr print_string
list_files_restore_root:
  jsr fat32_openroot
  rts
;----------------------------------------------
; Print current dirent as NAME.EXT
;----------------------------------------------
print_dirent_name:
  ldx #0
print_dirent_base_loop:
  cpx #8
  beq print_dirent_maybe_ext
  txa
  tay
  lda (zp_sd_address), y
  cmp #' '
  beq print_dirent_maybe_ext
  jsr print_char
  inx
  jmp print_dirent_base_loop
print_dirent_maybe_ext:
  ldx #8
  txa
  tay
  lda (zp_sd_address), y
  cmp #' '
  beq print_dirent_done
  lda #'.'
  jsr print_char
print_dirent_ext_loop:
  cpx #11
  beq print_dirent_done
  txa
  tay
  lda (zp_sd_address), y
  cmp #' '
  beq print_dirent_done
  jsr print_char
  inx
  jmp print_dirent_ext_loop
print_dirent_done:
  rts
;----------------------------------------------
; Return C=1 if dirent extension is playable
; supports: .Z1/.Z2/.Z3/.Z4/.Z5/.Z8 and .DAT
;----------------------------------------------
is_playable_file:
  ldy #8
  lda (zp_sd_address), y
  cmp #'Z'
  beq is_playable_z
  cmp #'D'
  bne is_playable_no
  iny
  lda (zp_sd_address), y
  cmp #'A'
  bne is_playable_no
  iny
  lda (zp_sd_address), y
  cmp #'T'
  bne is_playable_no
  sec
  rts
is_playable_z:
  iny
  lda (zp_sd_address), y
  cmp #'1'
  beq is_playable_z_ok
  cmp #'2'
  beq is_playable_z_ok
  cmp #'3'
  beq is_playable_z_ok
  cmp #'4'
  beq is_playable_z_ok
  cmp #'5'
  beq is_playable_z_ok
  cmp #'8'
  bne is_playable_no
is_playable_z_ok:
  iny
  lda (zp_sd_address), y
  cmp #' '
  bne is_playable_no
  sec
  rts
is_playable_no:
  clc
  rts
;----------------------------------------------
; Return C=1 if name[0..7] has at least one char and all used chars are A-Z/0-9
;----------------------------------------------
has_valid_base_name:
  lda #0
  sta copy_destination
  ldy #0
has_valid_base_name_loop:
  cpy #8
  beq has_valid_base_name_done
  lda (zp_sd_address), y
  cmp #' '
  beq has_valid_base_name_done
  cmp #'0'
  bcc has_valid_base_name_no
  cmp #'9'+1
  bcc has_valid_base_name_mark_used
  cmp #'A'
  bcc has_valid_base_name_no
  cmp #'Z'+1
  bcs has_valid_base_name_no
has_valid_base_name_mark_used:
  lda #1
  sta copy_destination
  iny
  jmp has_valid_base_name_loop
has_valid_base_name_done:
  lda copy_destination
  bne has_valid_base_name_yes
has_valid_base_name_no:
  clc
  rts
has_valid_base_name_yes:
  sec
  rts
; Hex to decimal converter
;----------------------------------------------
hex_to_dec:
  sed                        ; switch to decimal mode
  tay                        ; transfer accumulator to y register
  lda #00                    ; reset accumulator
hex_loop:
  dey                        ; decrement x by 1
  cpy #00                    ; if y < 0,
  bmi hex_break              ; then break;
  clc                        ; else clear carry
  adc #01                    ; to increment accumulator by 1
  jmp hex_loop               ; branch always
hex_break:
  cld
  rts                        ; return from subroutine
;----------------------------------------------
; Relocate code
;----------------------------------------------
start_copy:
  lda #<buffer               ; set our source memory address to copy from
  sta copy_swap
  lda #>buffer
  sta copy_swap+1
  lda copy_destination+1     ; set our destination memory to copy to
  sta copy_swap+2
  lda copy_destination
  sta copy_swap+3
  ldx #$00                   ; reset x for our loop
  ldy #$00                   ; reset y for our loop
copy_loop:
  lda (copy_swap),y          ; indirect index source memory address
  sta (copy_swap+2),y        ; indirect index dest memory address
  iny
  bne copy_loop              ; loop until our dest goes over 255
  inc copy_swap+1            ; increment high order source memory address
  inc copy_swap+3            ; increment high order dest memory address
  cpx copy_size+1            ; compare with the last address we want to write
  beq stop_copy
  inx
  jmp copy_loop              ; if we're not there yet, loop
stop_copy:
  rts
;----------------------------------------------
; Strings
;----------------------------------------------
help_menu:
  .byte "L    Load file"
  .byte $0D, $0A
  .byte "M    Menu files"
  .byte $0D, $0A
  .byte "E    Exit"
  .byte $0D, $0A
  .asciiz "H    Print help"
available_files:
  .asciiz "Available files"
no_playable_files:
  .asciiz "  (no playable files found)"
files_truncated:
  .asciiz "  ...more files omitted"
input_filename:
  .asciiz "Input filename > "
file_not_found:
  .asciiz "File not found"
memory_destination:
  .asciiz "Memory destination > "
reading:
  .asciiz "Reading data from SD card"
copying:
  .asciiz "Copying data to destination"
;----------------------------------------------
; Includes
;----------------------------------------------
  .include "hwconfig.s"
  .include "libsd.s"
  .include "libfat32.s"
  .include "libio.s"
