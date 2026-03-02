;------------------------------------------------------------
; KIM-1 Z-machine bring-up program
; - SD/FAT32 file menu for playable story files
; - Select/open a story by menu index
; - Stream story bytes from SD with rewind/reseek support
; - Parse header and start minimal opcode fetch/decode loop
;------------------------------------------------------------

; Zero page / shared workspace
z_pc                 = $20   ; 16-bit story PC
z_opcode             = $22   ; current opcode
z_print_ptr          = $23   ; 16-bit print pointer
z_story_selected     = $25   ; 0/1 story name selected
z_story_booted       = $26   ; 0/1 header parsed and PC set
z_story_version      = $27   ; story version
z_story_initialpc    = $28   ; 16-bit initial PC
z_story_target       = $2A   ; 24-bit target stream position
z_story_pos          = $2D   ; 24-bit current stream position
z_selected_index     = $30   ; 0-based menu index
z_tmp                = $31   ; scratch
z_idx                = $32   ; scratch index/count
z_static_base        = $33   ; 16-bit static memory base
z_abbrev_table       = $35   ; 16-bit abbrev table byte address
z_zs_ptr             = $37   ; 16-bit zstring decode pointer
z_zs_word_hi         = $39   ; current packed word high byte
z_zs_word_lo         = $3A   ; current packed word low byte
z_zs_shift           = $3B   ; 0=A0,1=A1,2=A2 (next-char shift for v3)
z_zs_abbrev          = $3C   ; abbrev prefix 1..3 pending, else 0
z_zs_escape          = $3D   ; 0=none,1=need high5,2=need low5
z_zs_escape_hi       = $3E   ; pending 5-bit high chunk for 10-bit ZSCII
z_zs_endflag         = $3F   ; current packed word end flag
z_opcount            = $50   ; decoded operand count
z_storevar           = $51   ; pending store variable id
z_op1_lo             = $52
z_op1_hi             = $53
z_op2_lo             = $54
z_op2_hi             = $55
z_op3_lo             = $56
z_op3_hi             = $57
z_op4_lo             = $58
z_op4_hi             = $59
z_branch_off         = $5A   ; signed 14-bit branch offset (low)
z_branch_off_hi      = $5B   ; signed 14-bit branch offset (high)
z_branch_cond        = $5C   ; branch sense bit (0/1)
z_global_table       = $5D   ; 16-bit globals table address
z_sp                 = $5F   ; 16-bit eval stack pointer (next free slot)
z_fp                 = $61   ; 16-bit frame pointer (current frame record)
z_callsp             = $63   ; 16-bit call stack pointer (next free slot)
z_work_ptr           = $65   ; generic 16-bit pointer scratch
z_work_cnt           = $67   ; generic 16-bit counter scratch
z_tmp2               = $69   ; scratch
z_trace_count        = $6A   ; trace loop counter
z_div_d_lo           = $6B   ; division/mul scratch
z_div_d_hi           = $6C
z_div_v_lo           = $6D
z_div_v_hi           = $6E
z_div_r_lo           = $6F
z_div_r_hi           = $70
z_div_q_lo           = $71
z_div_q_hi           = $72
z_div_signq          = $73
z_div_signr          = $74
z_rng_state_lo       = $75
z_rng_state_hi       = $76
z_object_table       = $77   ; 16-bit object table address
z_prop_ptr           = $80   ; 16-bit property scan/data pointer
z_prop_num           = $82   ; property number scratch
z_prop_size          = $83   ; property size scratch
z_dictionary         = $84   ; 16-bit dictionary table address
z_tok_start          = $86   ; token start index in input buffer
z_tok_len            = $87   ; token length
z_enc0               = $88   ; encoded dict key bytes (4)
z_enc1               = $89
z_enc2               = $8A
z_enc3               = $8B

zp_sd_address        = $40   ; 2 bytes (required by SD/FAT32 libs)
zp_sd_currentsector  = $42   ; 4 bytes (required by SD/FAT32 libs)
zp_fat32_variables   = $B0   ; 49+ bytes (required by FAT32 lib)

menu_ptr             = $7A   ; 2-byte pointer into menu_buffer
menu_count           = $7C   ; number of playable files cached
menu_saved_count     = $7D   ; temp during print
menu_attr            = $7E   ; temp attrs
menu_subdir_count    = $7F   ; number of root subdirs cached for scan

fat32_workspace      = $0200 ; 512-byte FAT/sector buffer
menu_buffer          = $0400 ; 12 bytes per entry (11-char 8.3 + NUL)
menu_cluster_table   = $0500 ; 16 entries * 4-byte start cluster
subdir_cluster_table = $0540 ; 8 entries * 4-byte start cluster
fake_dirent          = $0560 ; synthetic 32-byte dirent for cluster-open
story_name           = $0580 ; 12-byte 8.3 name buffer (must be RAM, not ROM)
z_dynamic_base       = $5000 ; dynamic story bytes (0 .. static_base-1)
z_eval_stack_base    = $7000 ; eval stack
z_call_stack_base    = $7800 ; call frame stack

.org $A000

reset:
  ldx #$FF
  txs

  jsr init_storage
  bcc :+
  jsr EXIT
:

  jsr newline
  ldx #<banner
  ldy #>banner
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string

  jmp main_loop

;------------------------------------------------------------
; Main loop: list files, select, and run
;------------------------------------------------------------
main_loop:
  jsr init_storage
  bcc :+
  jsr EXIT
:
  jsr list_files
  lda menu_count
  beq main_loop       ; no files found, reinit and retry

  jsr select_story    ; prompt user to pick a file
  bcs main_loop       ; invalid selection, re-list

  jsr run_vm          ; boot and run selected story
  jmp main_loop       ; after game ends, back to file list

;------------------------------------------------------------
; Init storage stack
;------------------------------------------------------------
init_storage:
  jsr via_init
  jsr sd_init
  jsr fat32_init
  bcc :+
  jsr newline
  ldx #<msg_fat_init_fail
  ldy #>msg_fat_init_fail
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  lda fat32_errorstage
  jsr print_hex
  sec
  rts
:
  jsr fat32_openroot
  lda #0
  sta z_story_selected
  sta z_story_booted
  sta menu_count
  clc
  rts

;------------------------------------------------------------
; Input helpers
;------------------------------------------------------------
get_key:
  jsr get_input
  and #$7F
  beq get_key
  rts

get_key_upper:
  jsr get_key
  cmp #'a'
  bcc :+
  cmp #'z'+1
  bcs :+
  and #$DF
:
  rts

;------------------------------------------------------------
; File menu scan/cache/print
;------------------------------------------------------------
list_files:
  lda #0
  sta menu_count

  lda #<menu_buffer
  sta menu_ptr
  lda #>menu_buffer
  sta menu_ptr+1

  jsr fat32_openroot
scan_next:
  jsr fat32_readdirent
  bcs print_menu
  sta menu_attr
  and #$18
  bne scan_next
  jsr is_playable_file
  bcc scan_next
  jsr append_menu_entry
  lda menu_count
  cmp #15
  bcs print_menu
  jmp scan_next

print_menu:
  jsr newline
  ldx #<msg_available
  ldy #>msg_available
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string

  lda menu_count
  sta menu_saved_count
  beq print_no_files

  lda #<menu_buffer
  sta menu_ptr
  lda #>menu_buffer
  sta menu_ptr+1
  ldx #0
print_menu_loop:
  cpx menu_count
  beq print_menu_done

  jsr newline
  lda #' '
  jsr print_char
  txa
  jsr print_menu_index
  lda #' '
  jsr print_char
  lda #')'
  jsr print_char
  lda #' '
  jsr print_char

  jsr copy_cached_name_to_buffer
  txa                       ; save loop counter across print_buffer_name
  pha
  jsr print_buffer_name
  pla                       ; restore loop counter
  tax

  ; Advance to next cached entry
  clc
  lda menu_ptr
  adc #12
  sta menu_ptr
  bcc :+
  inc menu_ptr+1
:
  inx
  jmp print_menu_loop

print_no_files:
  jsr newline
  ldx #<msg_no_files
  ldy #>msg_no_files
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  jsr dump_nonplayable_files

print_menu_done:
  jsr fat32_openroot
  rts

; Debug aid: when no playable files are found, dump regular files
; so we can see what FAT iteration is returning on hardware.
dump_nonplayable_files:
  jsr newline
  ldx #<msg_debug_files
  ldy #>msg_debug_files
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string

  lda #0
  sta z_idx
  jsr fat32_openroot
dump_nonplayable_next:
  jsr fat32_readdirent
  bcs dump_nonplayable_done
  sta menu_attr
  and #$18
  bne dump_nonplayable_next

  ; Copy dirent name immediately; monitor output calls can clobber ZP.
  jsr copy_dirent_name_to_buffer

  jsr newline
  lda #' '
  jsr print_char
  lda #' '
  jsr print_char
  lda #'['
  jsr print_char
  lda menu_attr
  jsr print_hex
  lda #']'
  jsr print_char
  lda #' '
  jsr print_char
  jsr print_story_name_hex11
  lda #' '
  jsr print_char
  jsr print_buffer_name

  inc z_idx
  lda z_idx
  cmp #12
  bcs dump_nonplayable_done
  jmp dump_nonplayable_next
dump_nonplayable_done:
  rts

copy_dirent_name_to_buffer:
  ldy #0
copy_dirent_name_loop:
  cpy #11
  beq copy_dirent_name_done
  lda (zp_sd_address),y
  sta story_name,y
  iny
  jmp copy_dirent_name_loop
copy_dirent_name_done:
  lda #0
  sta story_name,y
  rts

print_story_name_hex11:
  ldx #0
print_story_name_hex11_loop:
  cpx #11
  beq print_story_name_hex11_done
  lda story_name,x
  jsr print_hex
  cpx #10
  beq :+
  lda #' '
  jsr print_char
:
  inx
  jmp print_story_name_hex11_loop
print_story_name_hex11_done:
  rts

append_subdir_cluster:
  lda menu_subdir_count
  cmp #8
  bcs append_subdir_done
  asl
  asl
  tax
  ldy #26
  lda (zp_sd_address),y
  sta subdir_cluster_table,x
  iny
  lda (zp_sd_address),y
  sta subdir_cluster_table+1,x
  ldy #20
  lda (zp_sd_address),y
  sta subdir_cluster_table+2,x
  iny
  lda (zp_sd_address),y
  sta subdir_cluster_table+3,x
  inc menu_subdir_count
append_subdir_done:
  rts

open_subdir_by_index:
  txa
  asl
  asl
  tay

  lda #0
  ldx #31
open_subdir_clear_loop:
  sta fake_dirent,x
  dex
  bpl open_subdir_clear_loop

  lda #$10
  sta fake_dirent+11
  lda subdir_cluster_table,y
  sta fake_dirent+26
  lda subdir_cluster_table+1,y
  sta fake_dirent+27
  lda subdir_cluster_table+2,y
  sta fake_dirent+20
  lda subdir_cluster_table+3,y
  sta fake_dirent+21

  lda #<fake_dirent
  sta zp_sd_address
  lda #>fake_dirent
  sta zp_sd_address+1
  jmp fat32_opendirent

append_menu_entry:
  ldy #0
append_menu_copy_loop:
  cpy #11
  beq append_menu_done
  lda (zp_sd_address),y
  sta (menu_ptr),y
  iny
  jmp append_menu_copy_loop
append_menu_done:
  lda #0
  sta (menu_ptr),y

  ; Cache first-cluster (4 bytes) alongside this menu entry.
  lda menu_count
  asl
  asl
  tax
  ldy #26
  lda (zp_sd_address),y
  sta menu_cluster_table,x
  iny
  lda (zp_sd_address),y
  sta menu_cluster_table+1,x
  ldy #20
  lda (zp_sd_address),y
  sta menu_cluster_table+2,x
  iny
  lda (zp_sd_address),y
  sta menu_cluster_table+3,x

  clc
  lda menu_ptr
  adc #12
  sta menu_ptr
  bcc :+
  inc menu_ptr+1
:
  inc menu_count
  rts

; Returns C=1 if extension is playable:
; .Z1/.Z2/.Z3/.Z4/.Z5/.Z8 and .DAT
is_playable_file:
  ldy #8
  lda (zp_sd_address),y
  and #$DF
  cmp #'Z'
  beq playable_z
  cmp #'D'
  bne playable_no
  iny
  lda (zp_sd_address),y
  and #$DF
  cmp #'A'
  bne playable_no
  iny
  lda (zp_sd_address),y
  and #$DF
  cmp #'T'
  bne playable_no
  sec
  rts

playable_z:
  iny
  lda (zp_sd_address),y
  cmp #'1'
  beq playable_yes
  cmp #'2'
  beq playable_yes
  cmp #'3'
  beq playable_yes
  cmp #'4'
  beq playable_yes
  cmp #'5'
  beq playable_yes
  cmp #'8'
  bne playable_no
playable_yes:
  sec
  rts
playable_no:
  clc
  rts

; Print 0-based menu index as 1..9/A..F
print_menu_index:
  clc
  adc #1
  cmp #10
  bcc :+
  sec
  sbc #10
  clc
  adc #'A'
  jmp print_char
:
  clc
  adc #'0'
  jmp print_char

; Copy current cached 8.3 name entry (menu_ptr) to story_name buffer
copy_cached_name_to_buffer:
  ldy #0
copy_name_loop:
  cpy #11
  beq copy_name_done
  lda (menu_ptr),y
  sta story_name,y
  iny
  jmp copy_name_loop
copy_name_done:
  lda #0
  sta story_name+11
  rts

; Print story_name as NAME.EXT
print_buffer_name:
  ldx #0
print_base_loop:
  cpx #8
  beq print_maybe_ext
  lda story_name,x
  cmp #' '
  beq print_maybe_ext
  jsr print_char
  inx
  jmp print_base_loop

print_maybe_ext:
  ldx #8
  lda story_name,x
  cmp #' '
  beq print_name_done
  lda #'.'
  jsr print_char
print_ext_loop:
  cpx #11
  beq print_name_done
  lda story_name,x
  cmp #' '
  beq print_name_done
  jsr print_char
  inx
  jmp print_ext_loop
print_name_done:
  rts

;------------------------------------------------------------
; Story selection/opening
;------------------------------------------------------------
select_story:
  lda menu_count
  bne :+
  jsr list_files
:
  lda menu_count
  bne :+
  rts
:
  jsr newline
  ldx #<msg_select
  ldy #>msg_select
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string

  jsr get_key_upper
  jsr parse_menu_key_to_index
  bcs select_invalid

  cmp menu_count
  bcs select_invalid
  sta z_selected_index

  jsr open_selected_story
  bcs select_open_fail

  lda #1
  sta z_story_selected
  lda #0
  sta z_story_booted

  jsr newline
  ldx #<msg_selected
  ldy #>msg_selected
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  jsr print_buffer_name
  clc
  rts

select_invalid:
  jsr newline
  ldx #<msg_bad_index
  ldy #>msg_bad_index
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts

select_open_fail:
  jsr newline
  ldx #<msg_open_fail
  ldy #>msg_open_fail
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts

; Input: A=ASCII menu key, output A=0-based index, C=0 valid
parse_menu_key_to_index:
  cmp #'1'
  bcc parse_key_fail
  cmp #'9'+1
  bcc parse_key_digit
  cmp #'A'
  bcc parse_key_fail
  cmp #'F'+1
  bcs parse_key_fail
  sec
  sbc #'A'-10
  clc
  rts
parse_key_digit:
  sec
  sbc #'1'
  clc
  rts
parse_key_fail:
  sec
  rts

; Open story file by z_selected_index using cached menu names.
; On success leaves file open and stream position reset to 0.
open_selected_story:
  lda #<menu_buffer
  sta menu_ptr
  lda #>menu_buffer
  sta menu_ptr+1

  ldx z_selected_index
open_name_seek_loop:
  cpx #0
  beq open_name_found
  clc
  lda menu_ptr
  adc #12
  sta menu_ptr
  bcc :+
  inc menu_ptr+1
:
  dex
  jmp open_name_seek_loop

open_name_found:
  jsr copy_cached_name_to_buffer
  jmp story_rewind_open

; Re-open currently selected story cluster and reset stream position
story_rewind_open:
  jsr fat32_openroot
  ldx #<story_name
  ldy #>story_name
  jsr fat32_finddirent
  bcs story_rewind_fail
  jsr fat32_opendirent
  lda #0
  sta z_story_pos
  sta z_story_pos+1
  sta z_story_pos+2
  clc
  rts
story_rewind_fail:
  sec
  rts

;------------------------------------------------------------
; Story header bootstrap
;------------------------------------------------------------
boot_story:
  lda z_story_selected
  bne :+
  jsr newline
  ldx #<msg_no_story
  ldy #>msg_no_story
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts
:
  jsr story_rewind_open
  bcc :+
  jsr newline
  ldx #<msg_open_fail
  ldy #>msg_open_fail
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts
:
  ; Version byte @ 0x00
  lda #0
  sta z_story_target
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_byte_at_target
  bcc :+
  jmp boot_fail
:
  sta z_story_version

  ; Initial PC word @ 0x06
  lda #6
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_word_at_target
  bcc :+
  jmp boot_fail
:
  sta z_story_initialpc
  stx z_story_initialpc+1

  lda z_story_initialpc
  sta z_pc
  lda z_story_initialpc+1
  sta z_pc+1

  ; Static memory base @ 0x0E
  lda #$0E
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_word_at_target
  bcc :+
  jmp boot_fail
:
  stx z_static_base
  sta z_static_base+1

  ; Dictionary table @ 0x08
  lda #$08
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_word_at_target
  bcc :+
  jmp boot_fail
:
  stx z_dictionary
  sta z_dictionary+1

  ; Global variable table @ 0x0C
  lda #$0C
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_word_at_target
  bcc :+
  jmp boot_fail
:
  stx z_global_table
  sta z_global_table+1

  ; Object table @ 0x0A
  lda #$0A
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_word_at_target
  bcc :+
  jmp boot_fail
:
  stx z_object_table
  sta z_object_table+1

  ; Abbreviation table address @ 0x18
  lda #$18
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  jsr story_read_word_at_target
  bcc :+
  jmp boot_fail
:
  stx z_abbrev_table
  sta z_abbrev_table+1

  ; Load dynamic (writable) story region into RAM, then initialize stacks.
  jsr load_dynamic_memory
  bcc :+
  jmp boot_fail
:
  jsr z_vm_reset_stacks

  lda #1
  sta z_story_booted

  jsr newline
  ldx #<msg_boot_ok
  ldy #>msg_boot_ok
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string

  lda z_story_version
  jsr print_hex
  lda #' '
  jsr print_char
  lda z_story_initialpc+1
  jsr print_hex
  lda z_story_initialpc
  jsr print_hex
  lda #' '
  jsr print_char
  lda z_static_base+1
  jsr print_hex
  lda z_static_base
  jsr print_hex

  clc
  rts

boot_fail:
  jsr newline
  ldx #<msg_boot_fail
  ldy #>msg_boot_fail
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts

; Reads byte at absolute target z_story_target (24-bit)
; Returns A=byte, C=0 success / C=1 fail
story_read_byte_at_target:
  jsr story_seek_to_target
  bcs story_read_fail
  jsr fat32_file_readbyte
  bcs story_read_fail
  jsr story_pos_inc
  clc
  rts
story_read_fail:
  sec
  rts

; Reads big-endian word at z_story_target.
; Returns high byte in A, low byte in X.
story_read_word_at_target:
  jsr story_read_byte_at_target
  bcs :+
  pha
  jsr target_inc
  jsr story_read_byte_at_target
  bcs :+
  tax
  pla
  clc
  rts
:
  sec
  rts

; Ensure stream is positioned at z_story_target.
; Rewinds/open if target is behind current position.
story_seek_to_target:
  ; If target < pos, rewind.
  lda z_story_target+2
  cmp z_story_pos+2
  bcc seek_need_rewind
  bne seek_forward
  lda z_story_target+1
  cmp z_story_pos+1
  bcc seek_need_rewind
  bne seek_forward
  lda z_story_target
  cmp z_story_pos
  bcc seek_need_rewind

seek_forward:
  ; Consume forward bytes until pos == target
seek_loop:
  lda z_story_pos
  cmp z_story_target
  bne seek_step
  lda z_story_pos+1
  cmp z_story_target+1
  bne seek_step
  lda z_story_pos+2
  cmp z_story_target+2
  beq seek_done

seek_step:
  jsr fat32_file_readbyte
  bcs seek_fail
  jsr story_pos_inc
  jmp seek_loop

seek_need_rewind:
  jsr story_rewind_open
  bcs seek_fail
  jmp seek_forward

seek_done:
  clc
  rts
seek_fail:
  sec
  rts

story_pos_inc:
  inc z_story_pos
  bne :+
  inc z_story_pos+1
  bne :+
  inc z_story_pos+2
:
  rts

target_inc:
  inc z_story_target
  bne :+
  inc z_story_target+1
  bne :+
  inc z_story_target+2
:
  rts

;------------------------------------------------------------
; VM memory/stack foundations
;------------------------------------------------------------
z_vm_reset_stacks:
  lda #<z_eval_stack_base
  sta z_sp
  lda #>z_eval_stack_base
  sta z_sp+1
  lda #<z_call_stack_base
  sta z_callsp
  lda #>z_call_stack_base
  sta z_callsp+1
  lda #0
  sta z_fp
  sta z_fp+1
  lda #$01
  sta z_rng_state_lo
  lda #$00
  sta z_rng_state_hi
  rts

; Copy dynamic story bytes [0 .. static_base-1] to RAM z_dynamic_base.
load_dynamic_memory:
  ; Rewind story stream to byte 0.
  jsr story_rewind_open
  bcs load_dynamic_fail

  lda #<z_dynamic_base
  sta z_work_ptr
  lda #>z_dynamic_base
  sta z_work_ptr+1
  lda z_static_base
  sta z_work_cnt
  lda z_static_base+1
  sta z_work_cnt+1

load_dynamic_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq load_dynamic_done
  jsr fat32_file_readbyte
  bcs load_dynamic_fail
  ldy #0
  sta (z_work_ptr),y
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  lda z_story_pos
  clc
  adc #1
  sta z_story_pos
  bcc :+
  inc z_story_pos+1
  bne :+
  inc z_story_pos+2
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp load_dynamic_loop

load_dynamic_done:
  ; Rewind again so static memory reads start from stream byte 0.
  jsr story_rewind_open
  bcs load_dynamic_fail
  clc
  rts

load_dynamic_fail:
  sec
  rts

; Read story byte at address AX using RAM for dynamic and SD stream for static.
; Input: A=addr lo, X=addr hi. Output: A=byte, C clear on success.
z_mem_read_byte_ax:
  sta z_tmp
  stx z_tmp2
  ldx z_tmp2
  cpx z_static_base+1
  bcc z_mem_read_dynamic
  bne z_mem_read_static
  lda z_tmp
  cmp z_static_base
  bcc z_mem_read_dynamic

z_mem_read_static:
  lda z_tmp
  sta z_story_target
  lda z_tmp2
  sta z_story_target+1
  lda #0
  sta z_story_target+2
  jmp story_read_byte_at_target

z_mem_read_dynamic:
  ; pointer = z_dynamic_base + AX
  lda z_tmp
  clc
  adc #<z_dynamic_base
  sta z_work_ptr
  lda z_tmp2
  adc #>z_dynamic_base
  sta z_work_ptr+1
  ldy #0
  lda (z_work_ptr),y
  clc
  rts

; Write story byte at address AX if dynamic (< static_base).
; Input: A=addr lo, X=addr hi, Y=value
; Output: C clear on success, set if write refused.
z_mem_write_byte_ax:
  sta z_tmp
  stx z_tmp2
  ldx z_tmp2
  cpx z_static_base+1
  bcc z_mem_write_dynamic
  bne :+
  lda z_tmp
  cmp z_static_base
  bcc z_mem_write_dynamic
:
  sec
  rts

z_mem_write_dynamic:
  lda z_tmp
  clc
  adc #<z_dynamic_base
  sta z_work_ptr
  lda z_tmp2
  adc #>z_dynamic_base
  sta z_work_ptr+1
  tya
  ldy #0
  sta (z_work_ptr),y
  clc
  rts

; Push 16-bit word (A low, X high) on eval stack.
z_push_word:
  ldy #0
  sta (z_sp),y
  iny
  txa
  sta (z_sp),y
  clc
  lda z_sp
  adc #2
  sta z_sp
  bcc :+
  inc z_sp+1
:
  rts

; Pop 16-bit word from eval stack -> A low, X high.
z_pop_word:
  sec
  lda z_sp
  sbc #2
  sta z_sp
  bcs :+
  dec z_sp+1
:
  ldy #0
  lda (z_sp),y
  pha
  iny
  lda (z_sp),y
  tax
  pla
  rts

; Variable access:
; - var 0: stack
; - var 1..15: locals in current frame
; - var 16..255: globals table
z_get_var_word:
  cmp #0
  bne :+
  jmp z_pop_word
:
  cmp #16
  bcs z_get_global_word
  jmp z_get_local_word

z_get_global_word:
  sec
  sbc #16
  asl
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1
  clc
  lda z_work_ptr
  adc z_global_table
  sta z_work_ptr
  lda z_work_ptr+1
  adc z_global_table+1
  sta z_work_ptr+1
  ; globals are in dynamic memory
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_word_ax
  rts

; Set variable word: var id in Y, value low in A high in X.
z_set_var_word:
  cpy #0
  bne :+
  jmp z_push_word
:
  cpy #16
  bcs z_set_global_word
  jmp z_set_local_word

z_set_global_word:
  sty z_tmp
  pha
  txa
  pha
  tya
  sec
  sbc #16
  asl
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1
  clc
  lda z_work_ptr
  adc z_global_table
  sta z_work_ptr
  lda z_work_ptr+1
  adc z_global_table+1
  sta z_work_ptr+1
  pla
  tax
  pla
  jsr z_mem_write_word_ax
  rts

z_get_local_word:
  ; var id in A (1..15)
  lda z_fp
  ora z_fp+1
  bne :+
  lda #0
  tax
  rts
:
  sec
  sbc #1
  asl
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1
  clc
  lda z_fp
  adc #6
  adc z_work_ptr
  sta z_work_ptr
  lda z_fp+1
  adc #0
  adc z_work_ptr+1
  sta z_work_ptr+1
  ldy #0
  lda (z_work_ptr),y
  pha
  iny
  lda (z_work_ptr),y
  tax
  pla
  rts

z_set_local_word:
  ; var id in Y (1..15), value in A/X
  sty z_tmp2
  lda z_fp
  ora z_fp+1
  bne :+
  rts
:
  sta z_tmp
  txa
  pha
  lda z_tmp2
  sec
  sbc #1
  asl
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1
  clc
  lda z_fp
  adc #6
  adc z_work_ptr
  sta z_work_ptr
  lda z_fp+1
  adc #0
  adc z_work_ptr+1
  sta z_work_ptr+1
  ldy #0
  lda z_tmp
  sta (z_work_ptr),y
  iny
  pla
  sta (z_work_ptr),y
  rts

; Call a routine using decoded operands:
; op1 = packed routine address, op2.. = args.
; z_storevar must be set to destination variable, or $FF for no store.
z_call_common:
  lda z_op1_lo
  ora z_op1_hi
  bne z_call_nonzero
  ; Routine 0: immediately return false to store var (if any).
  lda z_storevar
  cmp #$FF
  beq :+
  tay
  lda #0
  tax
  jsr z_set_var_word
:
  clc
  rts

z_call_nonzero:
  ; routine byte address = packed * 2
  lda z_op1_lo
  asl
  sta z_work_ptr
  lda z_op1_hi
  rol
  sta z_work_ptr+1

  ; Preserve previous frame pointer.
  lda z_fp
  sta z_story_target
  lda z_fp+1
  sta z_story_target+1

  ; Create fixed-size frame (36 bytes).
  lda z_callsp
  sta z_fp
  lda z_callsp+1
  sta z_fp+1
  clc
  lda z_callsp
  adc #36
  sta z_callsp
  bcc :+
  inc z_callsp+1
:
  ; frame[0..1] = previous fp
  ldy #0
  lda z_story_target
  sta (z_fp),y
  iny
  lda z_story_target+1
  sta (z_fp),y
  ; frame[2..3] = return pc
  iny
  lda z_pc
  sta (z_fp),y
  iny
  lda z_pc+1
  sta (z_fp),y
  ; frame[4] = store var
  iny
  lda z_storevar
  sta (z_fp),y

  ; Read num_locals at routine start.
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcc :+
  jmp z_call_fail
:
  and #$0F
  sta z_tmp2
  ; frame[5] = num_locals
  ldy #5
  sta (z_fp),y

  ; Zero-initialize local slots [6..35]
  ldy #6
  lda #0
z_call_zero_locals:
  cpy #36
  beq z_call_load_defaults
  sta (z_fp),y
  iny
  jmp z_call_zero_locals

z_call_load_defaults:
  lda #0
  sta z_idx
z_call_default_loop:
  lda z_idx
  cmp z_tmp2
  beq z_call_apply_args
  ; addr = routine + 1 + (idx*2)
  lda z_work_ptr
  clc
  adc #1
  sta z_story_target
  lda z_work_ptr+1
  adc #0
  sta z_story_target+1
  lda #0
  sta z_story_target+2
  lda z_idx
  asl
  clc
  adc z_story_target
  sta z_story_target
  bcc :+
  inc z_story_target+1
:
  lda z_story_target
  ldx z_story_target+1
  jsr z_mem_read_word_ax
  bcc :+
  jmp z_call_fail
:
  ; local slot addr = frame + 6 + idx*2
  ldy z_idx
  tya
  asl
  tay
  iny
  iny
  iny
  iny
  iny
  iny
  sta (z_fp),y
  iny
  txa
  sta (z_fp),y
  inc z_idx
  jmp z_call_default_loop

z_call_apply_args:
  ; argc = opcount-1, cap to 3 and num_locals
  lda z_opcount
  beq z_call_set_pc
  sec
  sbc #1
  sta z_idx
  lda z_idx
  cmp #4
  bcc :+
  lda #3
  sta z_idx
:
  lda z_idx
  cmp z_tmp2
  bcc :+
  lda z_tmp2
  sta z_idx
:
  lda z_idx
  beq z_call_set_pc
  ; arg1 -> local1
  ldy #6
  lda z_op2_lo
  sta (z_fp),y
  iny
  lda z_op2_hi
  sta (z_fp),y
  lda z_idx
  cmp #1
  beq z_call_set_pc
  ; arg2 -> local2
  ldy #8
  lda z_op3_lo
  sta (z_fp),y
  iny
  lda z_op3_hi
  sta (z_fp),y
  lda z_idx
  cmp #2
  beq z_call_set_pc
  ; arg3 -> local3
  ldy #10
  lda z_op4_lo
  sta (z_fp),y
  iny
  lda z_op4_hi
  sta (z_fp),y

z_call_set_pc:
  ; new pc = routine + 1 + 2*num_locals
  lda z_tmp2
  asl
  sta z_story_target
  lda #0
  rol
  sta z_story_target+1
  clc
  lda z_work_ptr
  adc #1
  adc z_story_target
  sta z_pc
  lda z_work_ptr+1
  adc #0
  adc z_story_target+1
  sta z_pc+1
  clc
  rts

z_call_fail:
  sec
  rts

; Return from current routine with word value in A/X.
; C clear = continue VM, set = stop VM.
z_return_word:
  sta z_tmp
  txa
  sta z_tmp2
  lda z_fp
  ora z_fp+1
  bne :+
  sec
  rts
:
  ; load return context from frame
  ldy #0
  lda (z_fp),y
  sta z_work_ptr
  iny
  lda (z_fp),y
  sta z_work_ptr+1
  iny
  lda (z_fp),y
  sta z_pc
  iny
  lda (z_fp),y
  sta z_pc+1
  iny
  lda (z_fp),y
  sta z_storevar
  ; pop frame
  lda z_fp
  sta z_callsp
  lda z_fp+1
  sta z_callsp+1
  lda z_work_ptr
  sta z_fp
  lda z_work_ptr+1
  sta z_fp+1
  ; store return value unless call had no store target
  lda z_storevar
  cmp #$FF
  beq :+
  tay
  lda z_tmp
  ldx z_tmp2
  jsr z_set_var_word
:
  clc
  rts

; Read big-endian word at AX into A=low, X=high.
z_mem_read_word_ax:
  sta z_work_ptr
  stx z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs z_mem_read_word_fail
  sta z_tmp
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs z_mem_read_word_fail
  ldx z_tmp
  ; A already contains low byte, X now contains high byte.
  clc
  rts

z_mem_read_word_fail:
  sec
  rts

; Write big-endian word at AX from value A=low, X=high.
z_mem_write_word_ax:
  sta z_tmp
  txa
  sta z_tmp2
  ; high byte first
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_tmp2
  jsr z_mem_write_byte_ax
  bcs z_mem_write_word_fail
  ; low byte second
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_tmp
  jsr z_mem_write_byte_ax
  bcs z_mem_write_word_fail
  clc
  rts

z_mem_write_word_fail:
  sec
  rts

;------------------------------------------------------------
; Object table helpers (v3 layout)
;------------------------------------------------------------
; Input A = object number (1..255). Output z_work_ptr = object entry address.
; C set if object is 0.
z_obj_addr:
  beq z_obj_addr_fail
  sec
  sbc #1
  sta z_tmp
  lda z_object_table
  clc
  adc #62
  sta z_work_ptr
  lda z_object_table+1
  adc #0
  sta z_work_ptr+1
  ldx z_tmp
  beq z_obj_addr_ok
z_obj_addr_loop:
  clc
  lda z_work_ptr
  adc #9
  sta z_work_ptr
  bcc :+
  inc z_work_ptr+1
:
  dex
  bne z_obj_addr_loop
z_obj_addr_ok:
  clc
  rts
z_obj_addr_fail:
  sec
  rts

; Input A object number -> returns parent in A (0 on invalid object), C clear.
z_obj_get_parent:
  jsr z_obj_addr
  bcc :+
  lda #0
  clc
  rts
:
  lda z_work_ptr
  clc
  adc #4
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  clc
  rts

; Input A object number -> sibling in A.
z_obj_get_sibling:
  jsr z_obj_addr
  bcc :+
  lda #0
  clc
  rts
:
  lda z_work_ptr
  clc
  adc #5
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  clc
  rts

; Input A object number -> child in A.
z_obj_get_child:
  jsr z_obj_addr
  bcc :+
  lda #0
  clc
  rts
:
  lda z_work_ptr
  clc
  adc #6
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  clc
  rts

; Input A object, Y parent value
z_obj_set_parent:
  jsr z_obj_addr
  bcc :+
  clc
  rts
:
  lda z_work_ptr
  clc
  adc #4
  ldx z_work_ptr+1
  jsr z_mem_write_byte_ax
  clc
  rts

; Input A object, Y sibling value
z_obj_set_sibling:
  jsr z_obj_addr
  bcc :+
  clc
  rts
:
  lda z_work_ptr
  clc
  adc #5
  ldx z_work_ptr+1
  jsr z_mem_write_byte_ax
  clc
  rts

; Input A object, Y child value
z_obj_set_child:
  jsr z_obj_addr
  bcc :+
  clc
  rts
:
  lda z_work_ptr
  clc
  adc #6
  ldx z_work_ptr+1
  jsr z_mem_write_byte_ax
  clc
  rts

; Remove object A from tree.
z_obj_remove:
  sta z_tmp                 ; obj
  jsr z_obj_get_parent
  sta z_tmp2                ; parent
  beq z_obj_remove_clear_links

  ; if parent.child == obj, update to obj.sibling
  lda z_tmp2
  jsr z_obj_get_child
  cmp z_tmp
  bne z_obj_remove_scan_siblings
  lda z_tmp
  jsr z_obj_get_sibling
  tay
  lda z_tmp2
  jsr z_obj_set_child
  jmp z_obj_remove_clear_links

z_obj_remove_scan_siblings:
  lda z_tmp2
  jsr z_obj_get_child
  sta z_work_cnt            ; prev
z_obj_remove_scan_loop:
  lda z_work_cnt
  beq z_obj_remove_clear_links
  jsr z_obj_get_sibling
  cmp z_tmp
  beq z_obj_remove_patch_prev
  sta z_work_cnt
  jmp z_obj_remove_scan_loop

z_obj_remove_patch_prev:
  lda z_tmp
  jsr z_obj_get_sibling
  tay
  lda z_work_cnt
  jsr z_obj_set_sibling

z_obj_remove_clear_links:
  lda z_tmp
  ldy #0
  jsr z_obj_set_parent
  lda z_tmp
  ldy #0
  jsr z_obj_set_sibling
  clc
  rts

; Input A = object number.
; Output z_work_ptr = property table address.
; C set on invalid object/address read failure.
z_obj_get_prop_table_addr:
  jsr z_obj_addr
  bcc :+
  sec
  rts
:
  lda z_work_ptr
  clc
  adc #7
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs :+
  sta z_tmp2                 ; hi
  lda z_work_ptr
  clc
  adc #8
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs :+
  sta z_work_ptr             ; lo
  lda z_tmp2
  sta z_work_ptr+1           ; hi
  clc
  rts
:
  sec
  rts

; Inputs:
; - z_op1_lo = object number
; - z_op2_lo = property number (1..31)
; Outputs on found (C clear):
; - z_prop_ptr points to property data bytes
; - z_prop_num set
; - z_prop_size set (1..8)
; C set if not found/invalid.
z_prop_find_object_prop:
  lda z_op1_lo
  jsr z_obj_get_prop_table_addr
  bcc :+
  sec
  rts
:
  ; Skip short name: 1 + 2*name_words
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs z_prop_find_fail
  and #$1F
  asl
  sta z_tmp
  clc
  lda z_work_ptr
  adc #1
  adc z_tmp
  sta z_prop_ptr
  lda z_work_ptr+1
  adc #0
  sta z_prop_ptr+1

z_prop_scan_loop:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  bcs z_prop_find_fail
  beq z_prop_find_fail
  sta z_tmp2
  and #$1F
  sta z_prop_num
  lda z_tmp2
  lsr
  lsr
  lsr
  lsr
  lsr
  clc
  adc #1
  sta z_prop_size
  lda z_prop_num
  cmp z_op2_lo
  beq z_prop_found
  bcc z_prop_find_fail         ; list is descending, no future match
  ; advance ptr by header + size
  clc
  lda z_prop_ptr
  adc #1
  adc z_prop_size
  sta z_prop_ptr
  bcc :+
  inc z_prop_ptr+1
:
  jmp z_prop_scan_loop

z_prop_found:
  ; move pointer from header to data
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  clc
  rts

z_prop_find_fail:
  sec
  rts

; Print object short name for object number in A.
z_print_obj_name:
  jsr z_obj_get_prop_table_addr
  bcc :+
  rts
:
  ; first byte is name length in words
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs :+
  and #$1F
  sta z_work_cnt
  beq :+
  clc
  lda z_work_ptr
  adc #1
  sta z_zs_ptr
  lda z_work_ptr+1
  adc #0
  sta z_zs_ptr+1
  jsr z_decode_zstring_words_at_ptr
:
  rts

; Decode/print exactly z_work_cnt words at z_zs_ptr.
z_decode_zstring_words_at_ptr:
  lda #0
  sta z_zs_shift
  sta z_zs_abbrev
  sta z_zs_escape
z_decode_words_loop:
  lda z_work_cnt
  beq z_decode_words_done
  lda z_zs_ptr
  ldx z_zs_ptr+1
  jsr story_read_byte_ax
  bcc :+
  jmp z_decode_fail
:
  sta z_zs_word_hi
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
:
  lda z_zs_ptr
  ldx z_zs_ptr+1
  jsr story_read_byte_ax
  bcc :+
  jmp z_decode_fail
:
  sta z_zs_word_lo
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
:
  ; zchar 1
  lda z_zs_word_hi
  lsr
  lsr
  and #$1F
  jsr z_process_zchar
  bcc :+
  jmp z_decode_fail
:
  ; zchar 2
  lda z_zs_word_hi
  and #$03
  asl
  asl
  asl
  sta z_tmp
  lda z_zs_word_lo
  lsr
  lsr
  lsr
  lsr
  lsr
  and #$07
  ora z_tmp
  jsr z_process_zchar
  bcc :+
  jmp z_decode_fail
:
  ; zchar 3
  lda z_zs_word_lo
  and #$1F
  jsr z_process_zchar
  bcc :+
  jmp z_decode_fail
:
  dec z_work_cnt
  jmp z_decode_words_loop
z_decode_words_done:
  clc
  rts

; Uses op1 object and op2 attribute index (0..31).
; Sets z_tmp to 1 if attribute is set else 0.
z_obj_attr_test:
  lda #0
  sta z_tmp
  lda z_op2_hi
  bne z_obj_attr_test_done
  lda z_op2_lo
  cmp #32
  bcs z_obj_attr_test_done
  sta z_idx
  ; byte index = attr >> 3
  lsr
  lsr
  lsr
  sta z_tmp2
  ; mask index = attr & 7
  lda z_idx
  and #$07
  tay
  lda z_bit_mask,y
  sta z_work_cnt
  lda z_op1_lo
  jsr z_obj_addr
  bcc :+
  rts
:
  clc
  lda z_work_ptr
  adc z_tmp2
  sta z_work_ptr
  bcc :+
  inc z_work_ptr+1
:
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  and z_work_cnt
  beq z_obj_attr_test_done
  lda #1
  sta z_tmp
z_obj_attr_test_done:
  clc
  rts

; Uses op1 object and op2 attribute, z_tmp2 = 1 set / 0 clear.
z_obj_attr_write:
  lda z_op2_hi
  bne z_obj_attr_write_done
  lda z_op2_lo
  cmp #32
  bcs z_obj_attr_write_done
  sta z_idx
  lsr
  lsr
  lsr
  sta z_work_cnt+1          ; byte index
  lda z_idx
  and #$07
  tay
  lda z_bit_mask,y
  sta z_work_cnt            ; mask
  lda z_op1_lo
  jsr z_obj_addr
  bcc :+
  rts
:
  clc
  lda z_work_ptr
  adc z_work_cnt+1
  sta z_work_ptr
  bcc :+
  inc z_work_ptr+1
:
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  sta z_idx
  lda z_tmp2
  beq z_obj_attr_do_clear
  lda z_idx
  ora z_work_cnt
  jmp z_obj_attr_store
z_obj_attr_do_clear:
  lda z_work_cnt
  eor #$FF
  and z_idx
z_obj_attr_store:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_write_byte_ax
z_obj_attr_write_done:
  clc
  rts

;------------------------------------------------------------
; VM execution
;------------------------------------------------------------
run_vm:
  lda z_story_booted
  bne :+
  jsr boot_story
  bcs run_vm_done
:
  jsr newline
  ldx #<msg_vm_run
  ldy #>msg_vm_run
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string

vm_loop:
  jsr zm_step
  bcs run_vm_done
  jmp vm_loop

run_vm_done:
  rts

run_trace:
  lda z_story_booted
  bne :+
  jsr boot_story
  bcs run_trace_done
:
  jsr newline
  ldx #<msg_trace
  ldy #>msg_trace
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  lda #32
  sta z_trace_count
trace_loop:
  lda z_trace_count
  beq run_trace_done
  jsr trace_print_pc_op
  jsr zm_step
  bcs run_trace_done
  dec z_trace_count
  jmp trace_loop
run_trace_done:
  rts

trace_print_pc_op:
  jsr newline
  lda z_pc+1
  jsr print_hex
  lda z_pc
  jsr print_hex
  lda #':'
  jsr print_char
  lda #' '
  jsr print_char
  lda z_pc
  sta z_story_target
  lda z_pc+1
  sta z_story_target+1
  lda #0
  sta z_story_target+2
  jsr story_read_byte_at_target
  bcs :+
  jsr print_hex
:
  rts

zm_step:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_opcode

  ; Variable-form 2OP dispatch (C0..DF)
  lda z_opcode
  and #$E0
  cmp #$C0
  bne :+
  jmp zm_handle_var_2op
:

  ; Long-form 2OP dispatch (bit7 clear)
  lda z_opcode
  and #$80
  bne :+
  jmp zm_handle_long_2op
:

  ; Short-form 1OP/0OP dispatch
  lda z_opcode
  and #$C0
  cmp #$80
  bne :+
  lda z_opcode
  and #$30
  cmp #$30
  beq :+
  jmp zm_handle_short_1op
:

  ; 0OP subset
  cmp #$B0            ; rtrue
  bne :+
  jmp op_rtrue
:
  cmp #$B1            ; rfalse
  bne :+
  jmp op_rfalse
:
  cmp #$B2            ; print (inline Z-string)
  bne :+
  jmp op_print
:
  cmp #$B3            ; print_ret
  bne :+
  jmp op_print_ret
:
  cmp #$B4            ; nop
  bne :+
  jmp op_nop
:
  cmp #$B5            ; save (stub false)
  bne :+
  jmp op_save
:
  cmp #$B6            ; restore (stub false)
  bne :+
  jmp op_restore
:
  cmp #$B7            ; restart
  bne :+
  jmp op_restart
:
  cmp #$B8            ; ret_popped
  bne :+
  jmp op_ret_popped
:
  cmp #$B9            ; pop
  bne :+
  jmp op_pop
:
  cmp #$BA            ; quit
  bne :+
  jmp op_quit
:
  cmp #$BB            ; new_line
  bne :+
  jmp op_new_line
:
  cmp #$BD            ; verify
  bne :+
  jmp op_verify
:
  cmp #$E0            ; call (VAR form)
  bne :+
  jmp op_call_vs
:
  cmp #$E1            ; storew
  bne :+
  jmp op_storew_var
:
  cmp #$E2            ; storeb
  bne :+
  jmp op_storeb_var
:
  cmp #$E3            ; put_prop
  bne :+
  jmp op_put_prop_var
:
  cmp #$E4            ; sread
  bne :+
  jmp op_sread_var
:
  cmp #$E5            ; print_char
  bne :+
  jmp op_print_char_var
:
  cmp #$E6            ; print_num
  bne :+
  jmp op_print_num_var
:
  cmp #$E7            ; random
  bne :+
  jmp op_random_var
:
  cmp #$E8            ; push
  bne :+
  jmp op_push_var
:
  cmp #$E9            ; pull
  bne :+
  jmp op_pull_var
:
  cmp #$EC            ; call_vs2
  bne :+
  jmp op_call_vs2
:
  cmp #$F9            ; call_vn
  bne :+
  jmp op_call_vn
:
  cmp #$FA            ; call_vn2
  bne :+
  jmp op_call_vn2
:

  jmp zm_unknown_opcode

; Handle long-form 2OP instructions (bit7 clear, two operands).
zm_handle_long_2op:
  lda #2
  sta z_opcount
  ; operand 1
  lda z_opcode
  and #$40
  bne z_long_op1_var
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_lo
  lda #0
  sta z_op1_hi
  jmp z_long_op2_decode
z_long_op1_var:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  jsr z_get_var_word
  sta z_op1_lo
  stx z_op1_hi

z_long_op2_decode:
  lda z_opcode
  and #$20
  bne z_long_op2_var
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op2_lo
  lda #0
  sta z_op2_hi
  jmp z_long_dispatch
z_long_op2_var:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  jsr z_get_var_word
  sta z_op2_lo
  stx z_op2_hi

z_long_dispatch:
  lda z_opcode
  and #$1F
  cmp #$01            ; je
  bne :+
  jmp op_2op_je
:
  cmp #$02            ; jl
  bne :+
  jmp op_2op_jl
:
  cmp #$03            ; jg
  bne :+
  jmp op_2op_jg
:
  cmp #$06            ; jin
  bne :+
  jmp op_2op_jin
:
  cmp #$07            ; test
  bne :+
  jmp op_2op_test
:
  cmp #$0A            ; test_attr
  bne :+
  jmp op_2op_test_attr
:
  cmp #$0B            ; set_attr
  bne :+
  jmp op_2op_set_attr
:
  cmp #$0C            ; clear_attr
  bne :+
  jmp op_2op_clear_attr
:
  cmp #$08            ; or
  bne :+
  jmp op_2op_or
:
  cmp #$09            ; and
  bne :+
  jmp op_2op_and
:
  cmp #$0D            ; store
  bne :+
  jmp op_2op_store
:
  cmp #$0E            ; insert_obj
  bne :+
  jmp op_2op_insert_obj
:
  cmp #$0F            ; loadw
  bne :+
  jmp op_2op_loadw
:
  cmp #$10            ; loadb
  bne :+
  jmp op_2op_loadb
:
  cmp #$11            ; get_prop
  bne :+
  jmp op_2op_get_prop
:
  cmp #$12            ; get_prop_addr
  bne :+
  jmp op_2op_get_prop_addr
:
  cmp #$13            ; get_next_prop
  bne :+
  jmp op_2op_get_next_prop
:
  cmp #$14            ; add
  bne :+
  jmp op_2op_add
:
  cmp #$15            ; sub
  bne :+
  jmp op_2op_sub
:
  cmp #$16            ; mul
  bne :+
  jmp op_2op_mul
:
  cmp #$17            ; div
  bne :+
  jmp op_2op_div
:
  cmp #$18            ; mod
  bne :+
  jmp op_2op_mod
:
  jmp zm_unknown_opcode

; Handle variable-form 2OP opcodes (C0..DF) with type byte.
zm_handle_var_2op:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; Need at least two operands for 2OP semantics.
  lda z_opcount
  cmp #2
  bcs :+
  jmp zm_unknown_opcode
:
  lda z_opcode
  and #$1F
  cmp #$01            ; je
  bne :+
  jmp op_2op_je
:
  cmp #$02            ; jl
  bne :+
  jmp op_2op_jl
:
  cmp #$03            ; jg
  bne :+
  jmp op_2op_jg
:
  cmp #$04            ; dec_chk
  bne :+
  jmp op_2op_dec_chk
:
  cmp #$05            ; inc_chk
  bne :+
  jmp op_2op_inc_chk
:
  cmp #$06            ; jin
  bne :+
  jmp op_2op_jin
:
  cmp #$07            ; test
  bne :+
  jmp op_2op_test
:
  cmp #$08            ; or
  bne :+
  jmp op_2op_or
:
  cmp #$09            ; and
  bne :+
  jmp op_2op_and
:
  cmp #$0A            ; test_attr
  bne :+
  jmp op_2op_test_attr
:
  cmp #$0B            ; set_attr
  bne :+
  jmp op_2op_set_attr
:
  cmp #$0C            ; clear_attr
  bne :+
  jmp op_2op_clear_attr
:
  cmp #$0D            ; store
  bne :+
  jmp op_2op_store
:
  cmp #$0E            ; insert_obj
  bne :+
  jmp op_2op_insert_obj
:
  cmp #$0F            ; loadw
  bne :+
  jmp op_2op_loadw
:
  cmp #$10            ; loadb
  bne :+
  jmp op_2op_loadb
:
  cmp #$11            ; get_prop
  bne :+
  jmp op_2op_get_prop
:
  cmp #$12            ; get_prop_addr
  bne :+
  jmp op_2op_get_prop_addr
:
  cmp #$13            ; get_next_prop
  bne :+
  jmp op_2op_get_next_prop
:
  cmp #$14            ; add
  bne :+
  jmp op_2op_add
:
  cmp #$15            ; sub
  bne :+
  jmp op_2op_sub
:
  cmp #$16            ; mul
  bne :+
  jmp op_2op_mul
:
  cmp #$17            ; div
  bne :+
  jmp op_2op_div
:
  cmp #$18            ; mod
  bne :+
  jmp op_2op_mod
:
  cmp #$19            ; call_2s
  bne :+
  jmp op_2op_call_2s
:
  cmp #$1A            ; call_2n
  bne :+
  jmp op_2op_call_2n
:
  jmp zm_unknown_opcode

op_2op_je:
  lda z_op1_lo
  cmp z_op2_lo
  bne op_2op_je_check_more
  lda z_op1_hi
  cmp z_op2_hi
  bne op_2op_je_check_more
  lda #1
  jmp z_branch_on_bool

op_2op_je_check_more:
  lda z_opcount
  cmp #3
  bcc :+
  lda z_op1_lo
  cmp z_op3_lo
  bne :+
  lda z_op1_hi
  cmp z_op3_hi
  bne :+
  lda #1
  jmp z_branch_on_bool
:
  lda z_opcount
  cmp #4
  bcc :+
  lda z_op1_lo
  cmp z_op4_lo
  bne :+
  lda z_op1_hi
  cmp z_op4_hi
  bne :+
  lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_2op_jl:
  ; signed compare op1 < op2
  lda z_op1_hi
  eor #$80
  sta z_tmp
  lda z_op2_hi
  eor #$80
  cmp z_tmp
  bcc :+
  bne :++
  lda z_op1_lo
  cmp z_op2_lo
  bcc :+
: lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_2op_jg:
  ; signed compare op1 > op2
  lda z_op1_hi
  eor #$80
  sta z_tmp
  lda z_op2_hi
  eor #$80
  cmp z_tmp
  bcc :+
  bne :++
  lda z_op2_lo
  cmp z_op1_lo
  bcc :+
: lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_2op_dec_chk:
  ; op1 var-id, op2 value; branch if (--var) < op2 (signed)
  lda z_op1_lo
  jsr z_get_var_word
  sta z_work_ptr
  stx z_work_ptr+1
  sec
  lda z_work_ptr
  sbc #1
  sta z_work_ptr
  lda z_work_ptr+1
  sbc #0
  sta z_work_ptr+1
  ldy z_op1_lo
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  lda z_work_ptr
  sta z_op1_lo
  lda z_work_ptr+1
  sta z_op1_hi
  jmp op_2op_jl

op_2op_inc_chk:
  ; op1 var-id, op2 value; branch if (++var) > op2 (signed)
  lda z_op1_lo
  jsr z_get_var_word
  sta z_work_ptr
  stx z_work_ptr+1
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  ldy z_op1_lo
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  lda z_work_ptr
  sta z_op1_lo
  lda z_work_ptr+1
  sta z_op1_hi
  jmp op_2op_jg

op_2op_jin:
  lda z_op1_lo
  jsr z_obj_get_parent
  cmp z_op2_lo
  bne :+
  lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_2op_test:
  lda z_op1_lo
  and z_op2_lo
  cmp z_op2_lo
  bne :+
  lda z_op1_hi
  and z_op2_hi
  cmp z_op2_hi
  bne :+
  lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_2op_test_attr:
  jsr z_obj_attr_test
  lda z_tmp
  jmp z_branch_on_bool

op_2op_set_attr:
  lda #1
  sta z_tmp2
  jsr z_obj_attr_write
  clc
  rts

op_2op_clear_attr:
  lda #0
  sta z_tmp2
  jsr z_obj_attr_write
  clc
  rts

op_2op_or:
  lda z_op1_lo
  ora z_op2_lo
  sta z_work_ptr
  lda z_op1_hi
  ora z_op2_hi
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_and:
  lda z_op1_lo
  and z_op2_lo
  sta z_work_ptr
  lda z_op1_hi
  and z_op2_hi
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_store:
  ldy z_op1_lo
  lda z_op2_lo
  ldx z_op2_hi
  jsr z_set_var_word
  clc
  rts

op_2op_insert_obj:
  lda z_op1_lo
  jsr z_obj_remove
  lda z_op2_lo
  jsr z_obj_get_child
  tay
  lda z_op1_lo
  jsr z_obj_set_sibling
  lda z_op1_lo
  ldy z_op2_lo
  jsr z_obj_set_parent
  lda z_op2_lo
  ldy z_op1_lo
  jsr z_obj_set_child
  clc
  rts

op_2op_loadw:
  ; addr = op1 + (op2*2)
  lda z_op2_lo
  asl
  sta z_tmp
  lda z_op2_hi
  rol
  sta z_tmp2
  clc
  lda z_op1_lo
  adc z_tmp
  sta z_work_ptr
  lda z_op1_hi
  adc z_tmp2
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_word_ax
  bcc :+
  jmp zm_stop
:
  sta z_work_ptr
  stx z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_loadb:
  ; addr = op1 + op2
  clc
  lda z_op1_lo
  adc z_op2_lo
  sta z_work_ptr
  lda z_op1_hi
  adc z_op2_hi
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcc :+
  jmp zm_stop
:
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_get_prop:
  ; if prop absent, return default value from defaults table
  jsr z_prop_find_object_prop
  bcc op_2op_get_prop_found
  lda z_op2_lo
  beq op_2op_get_prop_zero
  sec
  sbc #1
  asl
  sta z_tmp
  clc
  lda z_object_table
  adc z_tmp
  sta z_work_ptr
  lda z_object_table+1
  adc #0
  sta z_work_ptr+1
  ; defaults are words, big-endian
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_cnt              ; hi
  lda z_work_ptr
  clc
  adc #1
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_ptr              ; lo
  lda z_work_cnt
  sta z_work_ptr+1            ; hi
  jmp op_2op_get_prop_store
op_2op_get_prop_found:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  lda z_prop_size
  cmp #1
  beq op_2op_get_prop_store
  ; read second byte for word-sized properties
  lda z_prop_ptr
  clc
  adc #1
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_ptr
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_ptr+1
  jmp op_2op_get_prop_store
op_2op_get_prop_zero:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
op_2op_get_prop_store:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_get_prop_addr:
  jsr z_prop_find_object_prop
  bcc :+
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  jmp op_2op_get_prop_addr_store
:
  lda z_prop_ptr
  sta z_work_ptr
  lda z_prop_ptr+1
  sta z_work_ptr+1
op_2op_get_prop_addr_store:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_get_next_prop:
  ; op2==0 => first prop
  lda z_op2_lo
  bne op_2op_get_next_prop_seek_current
  lda z_op1_lo
  jsr z_obj_get_prop_table_addr
  bcc :+
  jmp op_2op_get_next_prop_zero
:
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  and #$1F
  asl
  sta z_tmp
  clc
  lda z_work_ptr
  adc #1
  adc z_tmp
  sta z_prop_ptr
  lda z_work_ptr+1
  adc #0
  sta z_prop_ptr+1
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  and #$1F
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp op_2op_get_next_prop_store

op_2op_get_next_prop_seek_current:
  jsr z_prop_find_object_prop
  bcc :+
  jmp op_2op_get_next_prop_zero
:
  ; advance to next header
  clc
  lda z_prop_ptr
  adc z_prop_size
  sta z_prop_ptr
  bcc :+
  inc z_prop_ptr+1
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  and #$1F
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp op_2op_get_next_prop_store

op_2op_get_next_prop_zero:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
op_2op_get_next_prop_store:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_add:
  clc
  lda z_op1_lo
  adc z_op2_lo
  sta z_work_ptr
  lda z_op1_hi
  adc z_op2_hi
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_sub:
  sec
  lda z_op1_lo
  sbc z_op2_lo
  sta z_work_ptr
  lda z_op1_hi
  sbc z_op2_hi
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

; Minimal 16-bit signed multiply/div/mod (fast-enough bring-up path)
op_2op_mul:
  jsr z_mul_16
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_div:
  jsr z_divmod_16
  bcc :+
  jmp zm_stop
:
  ; quotient in z_work_ptr
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_2op_mod:
  jsr z_divmod_16
  bcc :+
  jmp zm_stop
:
  ; remainder in z_branch_off
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_branch_off
  ldx z_branch_off_hi
  jsr z_set_var_word
  clc
  rts

op_2op_call_2s:
  lda #2
  sta z_opcount
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_storevar
  jmp z_call_common

op_2op_call_2n:
  lda #2
  sta z_opcount
  lda #$FF
  sta z_storevar
  jmp z_call_common

; Handle short-form 1OP instructions (type bits != 11).
zm_handle_short_1op:
  ; Decode operand into z_op1.
  lda z_opcode
  and #$30
  cmp #$00
  bne zm_short_small_or_var
  ; large constant
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_hi
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_lo
  jmp zm_short_dispatch

zm_short_small_or_var:
  cmp #$10
  bne zm_short_var
  ; small constant
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_lo
  lda #0
  sta z_op1_hi
  jmp zm_short_dispatch

zm_short_var:
  ; variable
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  jsr z_get_var_word
  sta z_op1_lo
  stx z_op1_hi

zm_short_dispatch:
  lda z_opcode
  and #$0F
  sta z_idx
  cmp #$00            ; jz
  bne :+
  jmp op_1op_jz
:
  cmp #$01            ; get_sibling
  bne :+
  jmp op_1op_get_sibling
:
  cmp #$02            ; get_child
  bne :+
  jmp op_1op_get_child
:
  cmp #$03            ; get_parent
  bne :+
  jmp op_1op_get_parent
:
  cmp #$05            ; inc
  bne :+
  jmp op_1op_inc
:
  cmp #$06            ; dec
  bne :+
  jmp op_1op_dec
:
  cmp #$07            ; print_addr
  bne :+
  jmp op_1op_print_addr
:
  cmp #$08            ; call_1s
  bne :+
  jmp op_1op_call_1s
:
  cmp #$04            ; get_prop_len
  bne :+
  jmp op_1op_get_prop_len
:
  cmp #$09            ; remove_obj
  bne :+
  jmp op_1op_remove_obj
:
  cmp #$0A            ; print_obj
  bne :+
  jmp op_1op_print_obj
:
  cmp #$0B            ; ret
  bne :+
  jmp op_1op_ret
:
  cmp #$0C            ; jump
  bne :+
  jmp op_1op_jump
:
  cmp #$0D            ; print_paddr
  bne :+
  jmp op_1op_print_paddr
:
  cmp #$0E            ; load
  bne :+
  jmp op_1op_load
:
  cmp #$0F            ; not
  bne :+
  jmp op_1op_not
:

  ; Unimplemented 1OP opcode
  jmp zm_unknown_opcode

op_1op_jz:
  lda z_op1_lo
  ora z_op1_hi
  beq :+
  lda #0
  jmp z_branch_on_bool
:
  lda #1
  jmp z_branch_on_bool

op_1op_get_sibling:
  lda z_op1_lo
  jsr z_obj_get_sibling
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  lda z_work_ptr
  beq :+
  lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_1op_get_child:
  lda z_op1_lo
  jsr z_obj_get_child
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  lda z_work_ptr
  beq :+
  lda #1
  jmp z_branch_on_bool
:
  lda #0
  jmp z_branch_on_bool

op_1op_get_parent:
  lda z_op1_lo
  jsr z_obj_get_parent
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_1op_inc:
  lda z_op1_lo
  jsr z_get_var_word
  clc
  adc #1
  sta z_work_ptr
  txa
  adc #0
  sta z_work_ptr+1
  ldy z_op1_lo
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_1op_dec:
  lda z_op1_lo
  jsr z_get_var_word
  sec
  sbc #1
  sta z_work_ptr
  txa
  sbc #0
  sta z_work_ptr+1
  ldy z_op1_lo
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_1op_print_addr:
  ; op1 is byte address of zstring
  lda z_op1_lo
  sta z_zs_ptr
  lda z_op1_hi
  sta z_zs_ptr+1
  jsr z_decode_zstring_at_ptr
  clc
  rts

op_1op_get_prop_len:
  ; op1 is property data address; length from preceding size byte
  lda z_op1_lo
  ora z_op1_hi
  bne :+
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  jmp op_1op_get_prop_len_store
:
  sec
  lda z_op1_lo
  sbc #1
  sta z_work_ptr
  lda z_op1_hi
  sbc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  lsr
  lsr
  lsr
  lsr
  lsr
  clc
  adc #1
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
op_1op_get_prop_len_store:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_1op_remove_obj:
  lda z_op1_lo
  jsr z_obj_remove
  clc
  rts

op_1op_print_obj:
  lda z_op1_lo
  jsr z_print_obj_name
  clc
  rts

op_1op_ret:
  lda z_op1_lo
  ldx z_op1_hi
  jmp z_return_word

op_1op_jump:
  ; pc = pc + signed(operand) - 2
  clc
  lda z_pc
  adc z_op1_lo
  sta z_pc
  lda z_pc+1
  adc z_op1_hi
  sta z_pc+1
  sec
  lda z_pc
  sbc #2
  sta z_pc
  lda z_pc+1
  sbc #0
  sta z_pc+1
  clc
  rts

op_1op_print_paddr:
  ; packed address -> byte address
  lda z_op1_lo
  asl
  sta z_zs_ptr
  lda z_op1_hi
  rol
  sta z_zs_ptr+1
  jsr z_decode_zstring_at_ptr
  clc
  rts

op_1op_load:
  lda z_op1_lo
  jsr z_get_var_word
  sta z_work_ptr
  stx z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_1op_call_1s:
  lda #1
  sta z_opcount
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_storevar
  jmp z_call_common

op_1op_not:
  lda z_op1_lo
  eor #$FF
  sta z_work_ptr
  lda z_op1_hi
  eor #$FF
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

z_branch_on_bool:
  ; Input A: 0=false, nonzero=true
  sta z_tmp
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_tmp2

  lda z_tmp2
  and #$80
  beq :+
  lda #1
  bne z_branch_cond_set
:
  lda #0
z_branch_cond_set:
  sta z_branch_cond

  lda z_tmp2
  and #$40
  beq z_branch_two_byte

  ; one-byte branch offset (6-bit signed)
  lda z_tmp2
  and #$3F
  sta z_branch_off
  and #$20
  beq :+
  lda #$FF
  sta z_branch_off_hi
  lda z_branch_off
  ora #$C0
  sta z_branch_off
  jmp z_branch_eval
:
  lda #0
  sta z_branch_off_hi
  jmp z_branch_eval

z_branch_two_byte:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_branch_off
  lda z_tmp2
  and #$3F
  sta z_branch_off_hi
  lda z_branch_off_hi
  and #$20
  beq z_branch_eval
  lda z_branch_off_hi
  ora #$C0
  sta z_branch_off_hi

z_branch_eval:
  lda z_tmp
  beq :+
  lda #1
:
  cmp z_branch_cond
  beq z_branch_taken
  clc
  rts

z_branch_taken:
  lda z_branch_off
  ora z_branch_off_hi
  bne :+
  lda #0
  tax
  jmp z_return_word
:
  lda z_branch_off
  cmp #1
  bne :+
  lda z_branch_off_hi
  bne :+
  lda #1
  ldx #0
  jmp z_return_word
:
  clc
  lda z_pc
  adc z_branch_off
  sta z_pc
  lda z_pc+1
  adc z_branch_off_hi
  sta z_pc+1
  sec
  lda z_pc
  sbc #2
  sta z_pc
  lda z_pc+1
  sbc #0
  sta z_pc+1
  clc
  rts

; z_mul_16: signed 16-bit multiply z_op1 * z_op2
; result -> z_work_ptr (low/high)
z_mul_16:
  lda #0
  sta z_div_signq

  ; multiplicand = abs(op1)
  lda z_op1_lo
  sta z_div_d_lo
  lda z_op1_hi
  sta z_div_d_hi
  bpl :+
  jsr z_neg_d
  lda z_div_signq
  eor #1
  sta z_div_signq
:
  ; multiplier = abs(op2)
  lda z_op2_lo
  sta z_div_v_lo
  lda z_op2_hi
  sta z_div_v_hi
  bpl :+
  jsr z_neg_v
  lda z_div_signq
  eor #1
  sta z_div_signq
:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  ldx #16
z_mul_loop:
  lda z_div_v_lo
  and #1
  beq :+
  clc
  lda z_work_ptr
  adc z_div_d_lo
  sta z_work_ptr
  lda z_work_ptr+1
  adc z_div_d_hi
  sta z_work_ptr+1
:
  asl z_div_d_lo
  rol z_div_d_hi
  lsr z_div_v_hi
  ror z_div_v_lo
  dex
  bne z_mul_loop

  lda z_div_signq
  beq :+
  ; negate result
  lda z_work_ptr
  eor #$FF
  sta z_work_ptr
  lda z_work_ptr+1
  eor #$FF
  sta z_work_ptr+1
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
:
  clc
  rts

; z_divmod_16: signed 16-bit divide z_op1 / z_op2
; quotient -> z_work_ptr, remainder -> z_branch_off
; C set on divide-by-zero
z_divmod_16:
  lda z_op2_lo
  ora z_op2_hi
  bne :+
  sec
  rts
:
  lda #0
  sta z_div_signq
  sta z_div_signr

  ; dividend abs in d
  lda z_op1_lo
  sta z_div_d_lo
  lda z_op1_hi
  sta z_div_d_hi
  bpl :+
  jsr z_neg_d
  lda #1
  sta z_div_signr
  lda z_div_signq
  eor #1
  sta z_div_signq
:
  ; divisor abs in v
  lda z_op2_lo
  sta z_div_v_lo
  lda z_op2_hi
  sta z_div_v_hi
  bpl :+
  jsr z_neg_v
  lda z_div_signq
  eor #1
  sta z_div_signq
:
  lda #0
  sta z_div_r_lo
  sta z_div_r_hi
  sta z_div_q_lo
  sta z_div_q_hi
  ldx #16
z_div_loop:
  ; shift dividend left, carry out top bit into remainder
  asl z_div_d_lo
  rol z_div_d_hi
  rol z_div_r_lo
  rol z_div_r_hi
  ; q <<= 1
  asl z_div_q_lo
  rol z_div_q_hi
  ; if rem >= divisor: rem -= divisor; q|=1
  lda z_div_r_hi
  cmp z_div_v_hi
  bcc z_div_next
  bne z_div_sub
  lda z_div_r_lo
  cmp z_div_v_lo
  bcc z_div_next
z_div_sub:
  sec
  lda z_div_r_lo
  sbc z_div_v_lo
  sta z_div_r_lo
  lda z_div_r_hi
  sbc z_div_v_hi
  sta z_div_r_hi
  inc z_div_q_lo
  bne z_div_next
  inc z_div_q_hi
z_div_next:
  dex
  bne z_div_loop

  ; apply quotient sign
  lda z_div_signq
  beq :+
  lda z_div_q_lo
  eor #$FF
  sta z_div_q_lo
  lda z_div_q_hi
  eor #$FF
  sta z_div_q_hi
  clc
  lda z_div_q_lo
  adc #1
  sta z_div_q_lo
  lda z_div_q_hi
  adc #0
  sta z_div_q_hi
:
  ; apply remainder sign (same as dividend)
  lda z_div_signr
  beq :+
  lda z_div_r_lo
  eor #$FF
  sta z_div_r_lo
  lda z_div_r_hi
  eor #$FF
  sta z_div_r_hi
  clc
  lda z_div_r_lo
  adc #1
  sta z_div_r_lo
  lda z_div_r_hi
  adc #0
  sta z_div_r_hi
:
  lda z_div_q_lo
  sta z_work_ptr
  lda z_div_q_hi
  sta z_work_ptr+1
  lda z_div_r_lo
  sta z_branch_off
  lda z_div_r_hi
  sta z_branch_off_hi
  clc
  rts

z_neg_d:
  lda z_div_d_lo
  eor #$FF
  sta z_div_d_lo
  lda z_div_d_hi
  eor #$FF
  sta z_div_d_hi
  clc
  lda z_div_d_lo
  adc #1
  sta z_div_d_lo
  lda z_div_d_hi
  adc #0
  sta z_div_d_hi
  rts

z_neg_v:
  lda z_div_v_lo
  eor #$FF
  sta z_div_v_lo
  lda z_div_v_hi
  eor #$FF
  sta z_div_v_hi
  clc
  lda z_div_v_lo
  adc #1
  sta z_div_v_lo
  lda z_div_v_hi
  adc #0
  sta z_div_v_hi
  rts

; Print signed 16-bit number in A(low), X(high).
z_print_num_ax:
  sta z_work_ptr
  stx z_work_ptr+1
  lda z_work_ptr+1
  bpl :+
  lda #'-'
  jsr print_char
  ; negate
  lda z_work_ptr
  eor #$FF
  sta z_work_ptr
  lda z_work_ptr+1
  eor #$FF
  sta z_work_ptr+1
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
:
  lda #0
  sta z_tmp       ; printed flag

  ldx #0          ; 10000
  jsr z_print_digit_for_divisor
  ldx #1          ; 1000
  jsr z_print_digit_for_divisor
  ldx #2          ; 100
  jsr z_print_digit_for_divisor
  ldx #3          ; 10
  jsr z_print_digit_for_divisor
  ldx #4          ; 1
  jsr z_print_digit_for_divisor
  rts

; X indexes z_dec_div table (word big-endian).
z_print_digit_for_divisor:
  txa
  asl
  tay
  lda z_dec_div,y
  sta z_div_v_hi
  iny
  lda z_dec_div,y
  sta z_div_v_lo
  lda #0
  sta z_idx
z_print_digit_sub_loop:
  ; if work >= divisor then subtract and increment digit
  lda z_work_ptr+1
  cmp z_div_v_hi
  bcc z_print_digit_emit
  bne z_print_digit_do_sub
  lda z_work_ptr
  cmp z_div_v_lo
  bcc z_print_digit_emit
z_print_digit_do_sub:
  sec
  lda z_work_ptr
  sbc z_div_v_lo
  sta z_work_ptr
  lda z_work_ptr+1
  sbc z_div_v_hi
  sta z_work_ptr+1
  inc z_idx
  jmp z_print_digit_sub_loop

z_print_digit_emit:
  lda z_tmp
  bne z_print_digit_emit_yes
  cpx #4
  beq z_print_digit_emit_yes
  lda z_idx
  beq z_print_digit_skip
z_print_digit_emit_yes:
  lda z_idx
  clc
  adc #'0'
  jsr print_char
  lda #1
  sta z_tmp
z_print_digit_skip:
  rts

; Encode token at text buffer (op1) using z_tok_start/z_tok_len into z_enc0..z_enc3.
; Supports lowercase a-z directly, others map to pad char 5.
z_encode_token_key:
  lda #5
  sta z_div_d_lo
  sta z_div_d_hi
  sta z_div_v_lo
  sta z_div_v_hi
  sta z_div_r_lo
  sta z_div_r_hi
  lda #0
  sta z_idx
z_encode_loop:
  lda z_idx
  cmp #6
  bcs z_encode_pack
  cmp z_tok_len
  bcs z_encode_pack
  ; read input char text[1 + tok_start + idx]
  clc
  lda z_op1_lo
  adc #1
  adc z_tok_start
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  ; normalize uppercase to lowercase
  cmp #'A'
  bcc :+
  cmp #'Z'+1
  bcs :+
  ora #$20
:
  cmp #'a'
  bcc z_encode_char_done
  cmp #'z'+1
  bcs z_encode_char_done
  sec
  sbc #'a'-6
z_encode_store_char:
  ldx z_idx
  cpx #0
  bne :+
  sta z_div_d_lo
  jmp z_encode_char_done
:
  cpx #1
  bne :+
  sta z_div_d_hi
  jmp z_encode_char_done
:
  cpx #2
  bne :+
  sta z_div_v_lo
  jmp z_encode_char_done
:
  cpx #3
  bne :+
  sta z_div_v_hi
  jmp z_encode_char_done
:
  cpx #4
  bne :+
  sta z_div_r_lo
  jmp z_encode_char_done
:
  sta z_div_r_hi
z_encode_char_done:
  inc z_idx
  jmp z_encode_loop

z_encode_pack:
  ; enc0 = (z0<<2) | (z1>>3)
  lda z_div_d_lo
  asl
  asl
  sta z_tmp
  lda z_div_d_hi
  lsr
  lsr
  lsr
  ora z_tmp
  sta z_enc0
  ; enc1 = ((z1&7)<<5) | z2
  lda z_div_d_hi
  and #$07
  asl
  asl
  asl
  asl
  asl
  ora z_div_v_lo
  sta z_enc1
  ; enc2 = 0x80 | (z3<<2) | (z4>>3)
  lda z_div_v_hi
  asl
  asl
  sta z_tmp
  lda z_div_r_lo
  lsr
  lsr
  lsr
  ora z_tmp
  ora #$80
  sta z_enc2
  ; enc3 = ((z4&7)<<5) | z5
  lda z_div_r_lo
  and #$07
  asl
  asl
  asl
  asl
  asl
  ora z_div_r_hi
  sta z_enc3
  rts

; Lookup encoded token z_enc0..z_enc3 in dictionary z_dictionary.
; Returns dictionary entry address in z_work_ptr (0 if not found).
z_dict_lookup_token:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  lda z_tok_len
  bne :+
  rts
:
  lda z_dictionary
  sta z_prop_ptr
  lda z_dictionary+1
  sta z_prop_ptr+1
  ; separators count
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  clc
  adc #1
  sta z_tmp                     ; offset to entry_len
  ; ptr = dict + offset
  clc
  lda z_dictionary
  adc z_tmp
  sta z_prop_ptr
  lda z_dictionary+1
  adc #0
  sta z_prop_ptr+1
  ; entry length
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_prop_num                ; entry length
  ; num entries word
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_cnt+1              ; count hi
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_cnt                ; count lo
  ; entry pointer = ptr + 1
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
z_dict_scan_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_dict_not_found
  ; compare first 4 bytes
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  cmp z_enc0
  bne z_dict_next
  lda z_prop_ptr
  clc
  adc #1
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  cmp z_enc1
  bne z_dict_next
  lda z_prop_ptr
  clc
  adc #2
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  cmp z_enc2
  bne z_dict_next
  lda z_prop_ptr
  clc
  adc #3
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  cmp z_enc3
  bne z_dict_next
  ; found
  lda z_prop_ptr
  sta z_work_ptr
  lda z_prop_ptr+1
  sta z_work_ptr+1
  rts

z_dict_next:
  clc
  lda z_prop_ptr
  adc z_prop_num
  sta z_prop_ptr
  bcc :+
  inc z_prop_ptr+1
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_dict_scan_loop

z_dict_not_found:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  rts

zm_unknown_opcode:
  jsr newline
  ldx #<msg_unknown
  ldy #>msg_unknown
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  lda z_opcode
  jsr print_hex
  sec
  rts

op_rtrue:
  lda #1
  ldx #0
  jmp z_return_word

op_rfalse:
  lda #0
  tax
  jmp z_return_word

op_quit:
  jsr newline
  ldx #<msg_quit
  ldy #>msg_quit
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts

op_nop:
  clc
  rts

op_save:
  ; No save support yet: branch as false.
  lda #0
  jmp z_branch_on_bool

op_restore:
  ; No restore support yet: branch as false.
  lda #0
  jmp z_branch_on_bool

op_restart:
  jsr boot_story
  clc
  rts

op_ret_popped:
  jsr z_pop_word
  jmp z_return_word

op_pop:
  jsr z_pop_word
  clc
  rts

op_new_line:
  jsr newline
  clc
  rts

op_verify:
  ; Treat as verified for now.
  lda #1
  jmp z_branch_on_bool

op_print:
  jsr z_print_inline_zstring
  bcc :+
  jmp zm_stop
:
  clc
  rts

op_print_ret:
  jsr z_print_inline_zstring
  jsr newline
  lda #1
  ldx #0
  jmp z_return_word

zm_stop:
  jsr newline
  ldx #<msg_stream_end
  ldy #>msg_stream_end
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts

op_call_vs:
  jsr zm_decode_var_operands
  bcs zm_stop
  jsr zm_fetch_byte
  bcs zm_stop
  sta z_storevar
  jmp z_call_common

op_call_vs2:
  jsr zm_decode_var_operands_2b
  bcs zm_stop
  jsr zm_fetch_byte
  bcs zm_stop
  sta z_storevar
  jmp z_call_common

op_call_vn:
  jsr zm_decode_var_operands
  bcs zm_stop
  lda #$FF
  sta z_storevar
  jmp z_call_common

op_call_vn2:
  jsr zm_decode_var_operands_2b
  bcs zm_stop
  lda #$FF
  sta z_storevar
  jmp z_call_common

op_storew_var:
  jsr zm_decode_var_operands
  bcs zm_stop
  ; addr = op1 + 2*op2
  lda z_op2_lo
  asl
  sta z_tmp
  lda z_op2_hi
  rol
  sta z_tmp2
  clc
  lda z_op1_lo
  adc z_tmp
  sta z_work_ptr
  lda z_op1_hi
  adc z_tmp2
  sta z_work_ptr+1
  lda z_op3_lo
  ldx z_op3_hi
  jsr z_mem_write_word_ax
  clc
  rts

op_storeb_var:
  jsr zm_decode_var_operands
  bcs zm_stop
  ; addr = op1 + op2
  clc
  lda z_op1_lo
  adc z_op2_lo
  sta z_work_ptr
  lda z_op1_hi
  adc z_op2_hi
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_op3_lo
  jsr z_mem_write_byte_ax
  clc
  rts

op_put_prop_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; op1=obj, op2=prop, op3=value
  jsr z_prop_find_object_prop
  bcc :+
  clc
  rts
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  ldy z_op3_hi
  jsr z_mem_write_byte_ax
  lda z_prop_size
  cmp #1
  beq op_put_prop_var_done
  lda z_prop_ptr
  clc
  adc #1
  ldx z_prop_ptr+1
  ldy z_op3_lo
  jsr z_mem_write_byte_ax
op_put_prop_var_done:
  clc
  rts

op_sread_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; text buffer base in op1, parse buffer base in op2
  lda z_op1_lo
  ldx z_op1_hi
  jsr z_mem_read_byte_ax
  sta z_work_cnt              ; max chars
  lda #0
  sta z_idx                   ; count

z_sread_loop:
  lda z_idx
  cmp z_work_cnt
  bcs z_sread_done
  jsr get_key
  cmp #$0D
  beq z_sread_done
  ; normalize uppercase to lowercase
  cmp #'A'
  bcc :+
  cmp #'Z'+1
  bcs :+
  ora #$20
:
  sta z_tmp
  ; write char to text buffer at op1 + 1 + idx
  clc
  lda z_op1_lo
  adc #1
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_tmp
  jsr z_mem_write_byte_ax
  inc z_idx
  jmp z_sread_loop

z_sread_done:
  ; terminate with CR at text buffer[1+count]
  clc
  lda z_op1_lo
  adc #1
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy #$0D
  jsr z_mem_write_byte_ax
  ; parse_max = parse[0]
  lda z_op2_lo
  ldx z_op2_hi
  jsr z_mem_read_byte_ax
  sta z_work_cnt+1            ; parse max words
  lda #0
  sta z_work_cnt              ; parse word count
  ; parse entry pointer = parse + 2
  clc
  lda z_op2_lo
  adc #2
  sta z_prop_ptr
  lda z_op2_hi
  adc #0
  sta z_prop_ptr+1
  ; scan index
  lda #0
  sta z_tok_start

z_sread_scan_next:
  ; stop if parse full or input exhausted
  lda z_work_cnt
  cmp z_work_cnt+1
  bcc :+
  jmp z_sread_parse_finish
:
  lda z_tok_start
  cmp z_idx
  bcc :+
  jmp z_sread_parse_finish
:

z_sread_skip_spaces:
  clc
  lda z_op1_lo
  adc #1
  adc z_tok_start
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  cmp #' '
  bne z_sread_token_start_found
  inc z_tok_start
  lda z_tok_start
  cmp z_idx
  bcc z_sread_skip_spaces
  jmp z_sread_parse_finish

z_sread_token_start_found:
  lda #0
  sta z_tok_len
z_sread_count_token:
  lda z_tok_start
  clc
  adc z_tok_len
  cmp z_idx
  bcs z_sread_token_ready
  sta z_tmp
  clc
  lda z_op1_lo
  adc #1
  adc z_tmp
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  cmp #' '
  beq z_sread_token_ready
  inc z_tok_len
  jmp z_sread_count_token

z_sread_token_ready:
  jsr z_encode_token_key
  jsr z_dict_lookup_token      ; dict address in z_work_ptr
  ; write one parse entry at z_prop_ptr
  lda z_prop_ptr
  ldx z_prop_ptr+1
  ldy z_work_ptr+1             ; dict hi
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  ldy z_work_ptr               ; dict lo
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  ldy z_tok_len                ; token length
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_tok_start
  clc
  adc #1
  tay                          ; token offset in buffer
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  ; parse_count++
  inc z_work_cnt
  ; scan index += token length
  lda z_tok_start
  clc
  adc z_tok_len
  sta z_tok_start
  jmp z_sread_scan_next

z_sread_parse_finish:
  ; parse[1] = parse_count
  clc
  lda z_op2_lo
  adc #1
  sta z_work_ptr
  lda z_op2_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_work_cnt
  jsr z_mem_write_byte_ax
  clc
  rts

op_print_char_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  jsr print_char
  clc
  rts

op_print_num_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  ldx z_op1_hi
  jsr z_print_num_ax
  clc
  rts

op_random_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; n in op1 (signed)
  lda z_op1_hi
  bpl z_random_nonneg
  ; n < 0: seed RNG with abs(n), return 0
  lda z_op1_lo
  sta z_rng_state_lo
  lda z_op1_hi
  sta z_rng_state_hi
  lda z_rng_state_lo
  eor #$FF
  sta z_rng_state_lo
  lda z_rng_state_hi
  eor #$FF
  sta z_rng_state_hi
  clc
  lda z_rng_state_lo
  adc #1
  sta z_rng_state_lo
  lda z_rng_state_hi
  adc #0
  sta z_rng_state_hi
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  jmp z_random_store

z_random_nonneg:
  lda z_op1_lo
  ora z_op1_hi
  bne z_random_positive
  ; n == 0: return 0
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  jmp z_random_store

z_random_positive:
  ; preserve n in op2 for divmod
  lda z_op1_lo
  sta z_op2_lo
  lda z_op1_hi
  sta z_op2_hi
  ; LCG: state = state*5 + 1
  lda z_rng_state_lo
  sta z_tmp
  lda z_rng_state_hi
  sta z_tmp2
  asl z_rng_state_lo
  rol z_rng_state_hi
  asl z_rng_state_lo
  rol z_rng_state_hi
  clc
  lda z_rng_state_lo
  adc z_tmp
  adc #1
  sta z_rng_state_lo
  lda z_rng_state_hi
  adc z_tmp2
  adc #0
  sta z_rng_state_hi
  ; result = (state mod n) + 1, using z_divmod_16(op1/op2)
  lda z_rng_state_lo
  sta z_op1_lo
  lda z_rng_state_hi
  sta z_op1_hi
  jsr z_divmod_16
  bcc :+
  jmp zm_stop
:
  clc
  lda z_branch_off
  adc #1
  sta z_work_ptr
  lda z_branch_off_hi
  adc #0
  sta z_work_ptr+1

z_random_store:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_push_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  ldx z_op1_hi
  jsr z_push_word
  clc
  rts

op_pull_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  tay
  jsr z_pop_word
  jsr z_set_var_word
  clc
  rts

; Decode up to 4 operands using a VAR-form type byte.
; Result:
; - z_opcount set
; - z_op1..z_op4 words set
; C set on decode/read failure.
zm_decode_var_operands:
  lda #0
  sta z_opcount
  jsr zm_fetch_byte
  bcs zm_decode_var_fail
  sta z_tmp

  ; slot 1
  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp
  asl z_tmp

  ; slot 2
  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp
  asl z_tmp

  ; slot 3
  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp
  asl z_tmp

  ; slot 4
  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail

  clc
  rts

zm_decode_var_fail:
  sec
  rts

; Decode up to 8 operands using two VAR type bytes (used by *_vs2/*_vn2).
; Only first 4 operands are stored (current VM limit), but all encoded operands
; are consumed to keep PC aligned.
zm_decode_var_operands_2b:
  lda #0
  sta z_opcount
  jsr zm_fetch_byte
  bcs zm_decode_var_fail
  sta z_tmp
  jsr zm_fetch_byte
  bcs zm_decode_var_fail
  sta z_tmp2

  ; first type byte (4 slots)
  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp
  asl z_tmp

  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp
  asl z_tmp

  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp
  asl z_tmp

  lda z_tmp
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail

  ; second type byte (4 slots)
  lda z_tmp2
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp2
  asl z_tmp2

  lda z_tmp2
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp2
  asl z_tmp2

  lda z_tmp2
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail
  asl z_tmp2
  asl z_tmp2

  lda z_tmp2
  and #$C0
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail

  clc
  rts

; Input A = top two type bits (00 large, 01 small, 10 var, 11 omitted)
; Appends decoded operand into next z_op slot.
zm_decode_one_typed_operand:
  cmp #$C0
  bne :+
  ; omitted
  clc
  rts
:
  cmp #$00
  bne zm_decode_type_small_or_var
  ; large constant (word)
  jsr zm_fetch_byte
  bcs zm_decode_type_fail
  sta z_work_ptr+1
  jsr zm_fetch_byte
  bcs zm_decode_type_fail
  sta z_work_ptr
  jmp zm_append_operand_word

zm_decode_type_small_or_var:
  cmp #$40
  bne zm_decode_type_var
  ; small constant
  jsr zm_fetch_byte
  bcs zm_decode_type_fail
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp zm_append_operand_word

zm_decode_type_var:
  ; variable reference
  jsr zm_fetch_byte
  bcs zm_decode_type_fail
  jsr z_get_var_word
  sta z_work_ptr
  stx z_work_ptr+1
  jmp zm_append_operand_word

zm_decode_type_fail:
  sec
  rts

zm_append_operand_word:
  ldx z_opcount
  cpx #0
  bne :+
  lda z_work_ptr
  sta z_op1_lo
  lda z_work_ptr+1
  sta z_op1_hi
  inc z_opcount
  clc
  rts
:
  cpx #1
  bne :+
  lda z_work_ptr
  sta z_op2_lo
  lda z_work_ptr+1
  sta z_op2_hi
  inc z_opcount
  clc
  rts
:
  cpx #2
  bne :+
  lda z_work_ptr
  sta z_op3_lo
  lda z_work_ptr+1
  sta z_op3_hi
  inc z_opcount
  clc
  rts
:
  cpx #3
  bne :+
  lda z_work_ptr
  sta z_op4_lo
  lda z_work_ptr+1
  sta z_op4_hi
  inc z_opcount
  clc
  rts
:
  ; ignore extra operands
  clc
  rts

; Fetch one byte from story at z_pc, then increment z_pc.
; Uses RAM for dynamic region (< static_base), SD stream for static.
zm_fetch_byte:
  lda z_pc
  ldx z_pc+1
  jsr z_mem_read_byte_ax
  bcs zm_fetch_fail

  inc z_pc
  bne :+
  inc z_pc+1
:
  clc
  rts

zm_fetch_fail:
  sec
  rts

;------------------------------------------------------------
; Z-string decoding/printing (V3-focused)
;------------------------------------------------------------
; Print inline Z-string at current z_pc and advance z_pc past it.
z_print_inline_zstring:
  lda z_pc
  sta z_zs_ptr
  lda z_pc+1
  sta z_zs_ptr+1
  jsr z_decode_zstring_at_ptr
  bcs :+
  lda z_zs_ptr
  sta z_pc
  lda z_zs_ptr+1
  sta z_pc+1
  clc
  rts
:
  sec
  rts

; Decode and print Z-string at z_zs_ptr until end marker.
; Leaves z_zs_ptr pointing just after the terminating word.
z_decode_zstring_at_ptr:
  lda #0
  sta z_zs_shift
  sta z_zs_abbrev
  sta z_zs_escape

z_decode_word_loop:
  lda z_zs_ptr
  ldx z_zs_ptr+1
  jsr story_read_byte_ax
  bcs z_decode_fail
  sta z_zs_word_hi
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
:
  lda z_zs_ptr
  ldx z_zs_ptr+1
  jsr story_read_byte_ax
  bcs z_decode_fail
  sta z_zs_word_lo
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
:
  lda z_zs_word_hi
  and #$80
  sta z_zs_endflag

  ; zchar 1: bits 14..10 => (hi >> 2) & 0x1F
  lda z_zs_word_hi
  lsr
  lsr
  and #$1F
  jsr z_process_zchar
  bcs z_decode_fail

  ; zchar 2: bits 9..5 => ((hi & 0x03) << 3) | ((lo >> 5) & 0x07)
  lda z_zs_word_hi
  and #$03
  asl
  asl
  asl
  sta z_tmp
  lda z_zs_word_lo
  lsr
  lsr
  lsr
  lsr
  lsr
  and #$07
  ora z_tmp
  jsr z_process_zchar
  bcs z_decode_fail

  ; zchar 3: bits 4..0 => lo & 0x1F
  lda z_zs_word_lo
  and #$1F
  jsr z_process_zchar
  bcs z_decode_fail

  lda z_zs_endflag
  beq z_decode_word_loop
  clc
  rts

z_decode_fail:
  sec
  rts

; Process one 5-bit zchar in A.
z_process_zchar:
  sta z_idx

  ; If we're in 10-bit ZSCII escape state, consume this piece first.
  lda z_zs_escape
  beq z_process_no_escape
  cmp #1
  beq z_escape_take_hi
  ; state 2: this zchar is low 5 bits, emit assembled char.
  lda z_zs_escape_hi
  asl
  asl
  asl
  asl
  asl
  ora z_idx
  jsr z_emit_zscii
  lda #0
  sta z_zs_escape
  clc
  rts
z_escape_take_hi:
  lda z_idx
  and #$1F
  sta z_zs_escape_hi
  lda #2
  sta z_zs_escape
  clc
  rts

z_process_no_escape:
  ; Pending abbreviation prefix? This zchar is the 5-bit suffix.
  lda z_zs_abbrev
  beq z_process_not_abbrev
  jsr z_expand_abbrev
  bcs z_decode_fail
  lda #0
  sta z_zs_abbrev
  clc
  rts

z_process_not_abbrev:
  lda z_idx
  beq z_emit_space
  cmp #4
  bcc z_set_abbrev_prefix
  beq z_shift_a1
  cmp #5
  beq z_shift_a2

  ; Printable zchar 6..31 from selected alphabet (A0/A1/A2).
  lda z_zs_shift
  beq z_emit_a0
  cmp #1
  beq z_emit_a1
  jmp z_emit_a2

z_emit_space:
  lda #' '
  jsr print_char
  clc
  rts

z_set_abbrev_prefix:
  lda z_idx
  sta z_zs_abbrev
  clc
  rts

z_shift_a1:
  lda #1
  sta z_zs_shift
  clc
  rts

z_shift_a2:
  lda #2
  sta z_zs_shift
  clc
  rts

z_emit_a0:
  lda z_idx
  sec
  sbc #6
  clc
  adc #'a'
  jsr print_char
  lda #0
  sta z_zs_shift
  clc
  rts

z_emit_a1:
  lda z_idx
  sec
  sbc #6
  clc
  adc #'A'
  jsr print_char
  lda #0
  sta z_zs_shift
  clc
  rts

z_emit_a2:
  lda z_idx
  sec
  sbc #6
  beq z_begin_escape
  sec
  sbc #1
  tax
  lda z_a2_table, x
  jsr print_char
  lda #0
  sta z_zs_shift
  clc
  rts

z_begin_escape:
  lda #1
  sta z_zs_escape
  lda #0
  sta z_zs_shift
  clc
  rts

; Expand abbreviation entry (prefix in z_zs_abbrev, suffix in z_idx).
z_expand_abbrev:
  ; entry = 32 * (prefix-1) + suffix
  lda z_zs_abbrev
  sec
  sbc #1
  asl
  asl
  asl
  asl
  asl
  clc
  adc z_idx
  sta z_tmp

  ; table_addr = z_abbrev_table + 2*entry
  asl
  clc
  adc z_abbrev_table
  sta z_story_target
  lda #0
  adc z_abbrev_table+1
  sta z_story_target+1
  lda #0
  sta z_story_target+2

  ; Read packed address word from abbrev table.
  jsr story_read_word_at_target
  bcc :+
  jmp z_decode_fail
:

  ; Save current decode pointer.
  lda z_zs_ptr
  pha
  lda z_zs_ptr+1
  pha

  ; packed -> byte address (v3): addr = packed * 2
  pha
  txa
  asl
  sta z_zs_ptr
  pla
  rol
  sta z_zs_ptr+1

  jsr z_decode_zstring_at_ptr
  bcs z_expand_restore_fail

z_expand_restore:
  pla
  sta z_zs_ptr+1
  pla
  sta z_zs_ptr
  rts

z_expand_restore_fail:
  sec
  jmp z_expand_restore

; Emit ZSCII char in A (best-effort: printable ASCII or '?').
z_emit_zscii:
  cmp #$20
  bcc z_emit_qmark
  cmp #$7F
  bcs z_emit_qmark
  jmp print_char
z_emit_qmark:
  lda #'?'
  jmp print_char

; Read story byte from address in A(low), X(high). Returns A=byte, C status.
story_read_byte_ax:
  sta z_story_target
  stx z_story_target+1
  lda #0
  sta z_story_target+2
  jmp story_read_byte_at_target

;------------------------------------------------------------
; Console helper
;------------------------------------------------------------
print_string:
print_string_loop:
  ldy #0
  lda (z_print_ptr),y
  beq print_string_done
  jsr print_char
  inc z_print_ptr
  bne print_string_loop
  inc z_print_ptr+1
  jmp print_string_loop
print_string_done:
  rts

;------------------------------------------------------------
; Strings
;------------------------------------------------------------
banner:
  .asciiz "KIM-1 Z-MACHINE"

help_menu:
  .byte "M    Menu playable files"
  .byte $0D, $0A
  .byte "L    Select file"
  .byte $0D, $0A
  .byte "B    Boot selected story"
  .byte $0D, $0A
  .byte "G    Run VM"
  .byte $0D, $0A
  .byte "T    Trace 32 steps"
  .byte $0D, $0A
  .byte "E    Exit"
  .byte $0D, $0A
  .asciiz "H    Print help"

msg_available:
  .asciiz "Available files"
msg_no_files:
  .asciiz "  (no playable files found)"
msg_debug_files:
  .asciiz "  Files seen (all non-dir):"
msg_select:
  .asciiz "Select file (1-9/A-F) > "
msg_bad_index:
  .asciiz "Invalid menu index"
msg_selected:
  .asciiz "Selected: "
msg_no_story:
  .asciiz "No story selected"
msg_open_fail:
  .asciiz "Could not open selected story"
msg_boot_ok:
  .asciiz "Boot ok, version/PC: "
msg_boot_fail:
  .asciiz "Story boot failed"
msg_vm_run:
  .asciiz "Running VM"
msg_trace:
  .asciiz "Trace (PC:OP)"
msg_unknown:
  .asciiz "Unknown opcode: "
msg_rtrue:
  .asciiz "RTRUE"
msg_rfalse:
  .asciiz "RFALSE"
msg_quit:
  .asciiz "QUIT"
msg_printret_stop:
  .asciiz "(PRINT_RET stop: call stack not implemented yet)"
msg_stream_end:
  .asciiz "End of story stream"
msg_fat_init_fail:
  .asciiz "FAT init fail stage "

; A2 alphabet table for zchars 7..31 (zchar 6 is 10-bit ZSCII escape).
z_a2_table:
  .byte '0','1','2','3','4','5','6','7','8','9'
  .byte '.',',','!','?','_','#',$27,$22,'/',$5C,'-',':','(',')'

; Decimal divisors for z_print_num_ax (big-endian words)
z_dec_div:
  .byte $27,$10   ; 10000
  .byte $03,$E8   ; 1000
  .byte $00,$64   ; 100
  .byte $00,$0A   ; 10
  .byte $00,$01   ; 1

z_bit_mask:
  .byte $80,$40,$20,$10,$08,$04,$02,$01

;------------------------------------------------------------
; Includes
;------------------------------------------------------------
  .include "hwconfig.s"
  .include "libsd.s"
  .include "libfat32.s"
  .include "libio.s"
