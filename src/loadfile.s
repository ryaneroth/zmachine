;----------------------------------------------
; Copy data from SD card to memory location
;----------------------------------------------
copy_size = $27              ; 2 bytes
copy_destination = $29       ; 2 bytes
input_pointer = $2B          ; 11 bytes
print_pointer = $38          ; 2 bytes
zp_sd_address = $40          ; 2 bytes
zp_sd_currentsector = $42    ; 4 bytes
zp_fat32_variables = $B0     ; 49 bytes
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
  jsr via_init
  jsr sd_init
  jsr fat32_init
  bcc :+
  lda #'Z'
  jsr print_char
  lda fat32_errorstage
  jsr print_hex
  jmp print_prompt
:
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
  jsr via_init
  jsr sd_init
  jsr fat32_init
  bcc :+
  lda #'Z'
  jsr print_char
  lda fat32_errorstage
  jsr print_hex
  jmp print_prompt
:
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
  ; Reject files larger than 64K.
  lda fat32_bytesremaining+2
  ora fat32_bytesremaining+3
  beq file_size_ok
  ldx #<file_too_large
  ldy #>file_too_large
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  jsr fat32_openroot
  jmp print_prompt
file_size_ok:
  ; Destination must be in application RAM ($2000-$9FFF).
  lda copy_destination+1
  cmp #$20
  bcc destination_invalid
  cmp #$A0
  bcs destination_invalid
  ; Ensure file fits inside app RAM end at $9FFF.
  sec
  lda #$00
  sbc copy_destination
  sta copy_size
  lda #$A0
  sbc copy_destination+1
  sta copy_size+1
  lda <fat32_bytesremaining+1
  cmp copy_size+1
  bcc destination_ok
  bne destination_invalid
  lda fat32_bytesremaining
  cmp copy_size
  bcc destination_ok
  beq destination_ok
destination_invalid:
  ldx #<destination_out_of_range
  ldy #>destination_out_of_range
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  jsr fat32_openroot
  jmp print_prompt
destination_ok:
  ldx #<reading
  ldy #>reading
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  ; Stream file bytes to destination.
  jsr stream_copy_file
  jsr fat32_openroot
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
  jsr hex_char_to_nibble
  asl
  asl
  asl
  asl
  sta copy_destination, x
  jsr get_key                ; read second digit of hex address
  jsr hex_char_to_nibble
  ora copy_destination, x
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
  lda #<buffer
  sta copy_swap
  lda #>buffer
  sta copy_swap+1
  jsr fat32_openroot
list_files_entry_next:
  jsr fat32_readdirent
  bcs list_files_print
  and #$18
  bne list_files_entry_next
  jsr is_playable_file
  bcc list_files_entry_next
  jsr append_menu_entry
  lda menu_count
  cmp #16
  bcs list_files_print
  jmp list_files_entry_next
list_files_print:
  jsr newline
  ldx #<available_files
  ldy #>available_files
  stx print_pointer
  sty print_pointer+1
  jsr print_string
  lda menu_count
  sta copy_size
  lda menu_count
  beq list_files_done
  lda #<buffer
  sta copy_swap
  lda #>buffer
  sta copy_swap+1
list_files_print_next:
  lda menu_count
  beq list_files_done
  jsr cache_dirent_name
  jsr newline
  lda #' '
  jsr print_char
  lda #' '
  jsr print_char
  jsr print_cached_name
  clc
  lda copy_swap
  adc #12
  sta copy_swap
  bcc :+
  inc copy_swap+1
:
  dec menu_count
  jmp list_files_print_next
list_files_done:
  lda copy_size
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
; Append current dirent 8.3 into menu cache at copy_swap
;----------------------------------------------
append_menu_entry:
  ldy #0
append_menu_entry_loop:
  cpy #11
  beq append_menu_entry_end
  lda (zp_sd_address), y
  sta (copy_swap), y
  iny
  jmp append_menu_entry_loop
append_menu_entry_end:
  lda #0
  sta (copy_swap), y
  clc
  lda copy_swap
  adc #12
  sta copy_swap
  bcc :+
  inc copy_swap+1
:
  inc menu_count
  rts
;----------------------------------------------
; Copy current dirent 8.3 name into input_pointer[0..10]
;----------------------------------------------
cache_dirent_name:
  ldx #0
cache_dirent_name_loop:
  cpx #11
  beq cache_dirent_name_done
  txa
  tay
  lda (copy_swap), y
  sta buffer, x
  inx
  jmp cache_dirent_name_loop
cache_dirent_name_done:
  rts
;----------------------------------------------
; Print cached 8.3 name from input_pointer as NAME.EXT
;----------------------------------------------
print_cached_name:
  ldx #0
print_cached_base_loop:
  cpx #8
  beq print_cached_maybe_ext
  lda buffer, x
  cmp #' '
  beq print_cached_maybe_ext
  jsr print_char
  inx
  jmp print_cached_base_loop
print_cached_maybe_ext:
  ldx #8
  lda buffer, x
  cmp #' '
  beq print_cached_done
  lda #'.'
  jsr print_char
print_cached_ext_loop:
  cpx #11
  beq print_cached_done
  lda buffer, x
  cmp #' '
  beq print_cached_done
  jsr print_char
  inx
  jmp print_cached_ext_loop
print_cached_done:
  rts
;----------------------------------------------
; Return C=1 if dirent extension is playable
; supports: .Z1/.Z2/.Z3/.Z4/.Z5/.Z8 and .DAT
;----------------------------------------------
is_playable_file:
  ldy #8
  lda (zp_sd_address), y
  and #$DF
  cmp #'Z'
  beq is_playable_z
  cmp #'D'
  bne is_playable_no
  iny
  lda (zp_sd_address), y
  and #$DF
  cmp #'A'
  bne is_playable_no
  iny
  lda (zp_sd_address), y
  and #$DF
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
;----------------------------------------------
; Convert ASCII hex char in A to nibble in A.
; Invalid chars map to 0.
;----------------------------------------------
hex_char_to_nibble:
  cmp #'0'
  bcc hex_char_invalid
  cmp #'9'+1
  bcc hex_char_digit
  cmp #'A'
  bcc hex_char_check_lower
  cmp #'F'+1
  bcc hex_char_upper
hex_char_check_lower:
  cmp #'a'
  bcc hex_char_invalid
  cmp #'f'+1
  bcs hex_char_invalid
  sec
  sbc #'a'-10
  rts
hex_char_upper:
  sec
  sbc #'A'-10
  rts
hex_char_digit:
  sec
  sbc #'0'
  rts
hex_char_invalid:
  lda #0
  rts
;----------------------------------------------
; Relocate code
;----------------------------------------------
stream_copy_file:
  lda copy_destination       ; set destination pointer
  sta copy_swap+2
  lda copy_destination+1
  sta copy_swap+3
stream_copy_loop:
  jsr fat32_file_readbyte
  bcs stream_copy_done
  ldy #0
  sta (copy_swap+2), y
  inc copy_swap+2
  bne stream_copy_loop
  inc copy_swap+3
  jmp stream_copy_loop
stream_copy_done:
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
  .asciiz "Reading/copying data from SD card"
destination_out_of_range:
  .asciiz "Destination/file outside app RAM 2000-9FFF"
file_too_large:
  .asciiz "File too large (>64K)"
;----------------------------------------------
; Includes
;----------------------------------------------
  .include "hwconfig.s"
  .include "libsd.s"
  .include "libfat32.s"
  .include "libio.s"
