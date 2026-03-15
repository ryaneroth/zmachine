;------------------------------------------------------------
; KIM-1 Z-machine bring-up program
; - SD/FAT32 file menu for playable story files
; - Select/open a story by menu index
; - Stream story bytes from SD with rewind/reseek support
; - Parse header and start minimal opcode fetch/decode loop
;------------------------------------------------------------

; Zero page / shared workspace
z_pc                 = $20   ; story PC low 16 bits (high byte in z_pc_ext)
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
z_zs_ptr             = $37   ; zstring decode pointer low 16 bits
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
z_op1_varid          = $79   ; raw 1OP variable id (for inc/dec/load)
z_story_startcluster = $8C   ; 4-byte first cluster of open story file
z_story_filesize     = $90   ; 4-byte total file size of open story file
z_seek_tsec_lo       = $94   ; seek scratch: target sector (lo) - private to story_seek_to_target
z_seek_tsec_hi       = $95   ; seek scratch: target sector (hi)
z_seek_csec_lo       = $96   ; seek scratch: current sector (lo)
z_seek_csec_hi       = $97   ; seek scratch: current sector (hi)
z_op1_raw            = $98   ; raw operand byte for op1 when operand is 8-bit encoded
z_op2_raw            = $99   ; raw operand byte for op2 when operand is 8-bit encoded
z_text_base_off      = $9A   ; sread text buffer char base offset (v<=4:1, v>=5:2)
z_pc_ext             = $9B   ; high byte for 24-bit story PC
z_zs_ptr_ext         = $9C   ; high byte for z_zs_ptr during decode
z_ptr_ext            = $9D   ; generic high byte for z_work_ptr-based code pointers
z_cache_base_hi      = $9E   ; high byte for runtime static-sector cache base
z_cache_enabled      = $9F   ; 0=disabled, 1=enabled
z_save_valid         = $A0   ; 0/1 SAVE snapshot availability (loaded/built)
z_out3_active        = $A1   ; 0/1 output stream 3 active
z_out3_ptr           = $A2   ; output stream 3 current write pointer
z_out3_len           = $A4   ; output stream 3 byte count
z_out3_table         = $A6   ; output stream 3 table base (word count destination)
z_zs_shift_once      = $A8   ; v1/v2: 1 when current alphabet shift is temporary
z_window_lines       = $A9   ; requested split height for upper window
z_window_active      = $AA   ; 0=lower, 1=upper
z_input_stream       = $AB   ; 0/1 selected input stream (keyboard/script)
z_load_dot_ctr       = $AC   ; 16-bit countdown for throttled load progress dots
z_call_nostore       = $AD   ; 0=call stores a result, 1=no-store call form
z_dict_cache_enabled = $AE   ; 0=disabled, 1=enabled
; Note: $AF = sd_read_bits (SD library scratch) — do not use.
z_cache_slot_mask    = $46   ; runtime slot-count mask: #$07 (8-slot) or #$0F (16-slot)
                              ; $46-$4F are free between zp_sd_currentsector ($42-$45) and z_opcount ($50)
z_timed_tenths       = $47   ; remaining 1/10s in timed input (0 = no timeout active)
z_timed_rtn_lo       = $48   ; packed timed callback routine address lo
z_timed_rtn_hi       = $49   ; packed timed callback routine address hi
z_timed_reset_val    = $4A   ; original tenths count (for restart after false callback)
z_sread_callsp_lo    = $4B   ; saved z_callsp lo during timed callback mini-loop
z_sread_callsp_hi    = $4C   ; saved z_callsp hi during timed callback mini-loop
z_script_pos         = $4D   ; current script-input byte offset
z_last_input_script  = $4F   ; 0=keyboard, 1=script file
z_prop_ptr           = $80   ; 16-bit property scan/data pointer
z_prop_num           = $82   ; property number scratch
z_prop_size          = $83   ; property size scratch
z_dictionary         = $84   ; 16-bit dictionary table address
z_tok_start          = $86   ; token start index in input buffer
z_tok_len            = $87   ; token length
z_enc0               = $88   ; encoded dict key bytes (v1-3: 4 bytes, v4+: first 4 of 6)
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
call_arg_buf         = $0590 ; 8 decoded operands (16 bytes, lo/hi pairs)
z_enc4_ram           = call_arg_buf+8  ; v4+ encoded dict byte 4
z_enc5_ram           = call_arg_buf+9  ; v4+ encoded dict byte 5
; Story static-sector cache: 8 or 16 direct-mapped slots (selected at boot time by
; story_cache_configure).  Each slot holds one 512-byte story sector (2 pages).
; Metadata arrays are 16 bytes each so the full 16-slot layout always fits here.
story_cache_valid    = $05A0 ; 16-byte valid flags  (only [0..mask] used at runtime)
story_cache_tag_lo   = $05B0 ; 16-byte sector tag low bytes
story_cache_tag_hi   = $05C0 ; 16-byte sector tag high bytes
; Dict-lookup result cache: 4 direct-mapped slots (shrunk from 8 to give story cache
; its extra 24 bytes of metadata space).  sread is I/O-dominated, so 4 slots suffice.
dict_cache_valid     = $05D0 ; 4-byte valid flags
dict_cache_key0      = $05D4 ; 4-byte encoded token byte 0
dict_cache_key1      = $05D8 ; 4-byte encoded token byte 1
dict_cache_key2      = $05DC ; 4-byte encoded token byte 2
dict_cache_key3      = $05E0 ; 4-byte encoded token byte 3
dict_cache_val_lo    = $05E4 ; 4-byte dictionary result lo
dict_cache_val_hi    = $05E8 ; 4-byte dictionary result hi
window_cursor_row    = $05F0 ; 2-byte per-window cursor row [0]=lower,[1]=upper
window_cursor_col    = $05F2 ; 2-byte per-window cursor column
window_height        = $05F4 ; 2-byte per-window height in rows
window_width         = $05F6 ; 2-byte per-window width in columns
window_style_ram     = $05F8 ; 2-byte per-window text style bitmask
window_font_ram      = $05FA ; 2-byte per-window font number
window_buffer_mode   = $05FC ; 0/1 line buffering hint
window_mouse_window  = $05FD ; selected mouse window id
window_true_fg       = $05FE ; current true foreground colour
window_true_bg       = $05FF ; current true background colour
save_buffer          = $0600 ; in-RAM SAVE snapshot
save_buffer_end      = $2000 ; exclusive end
save_hdr_pc_lo       = save_buffer+0
save_hdr_pc_hi       = save_buffer+1
save_hdr_pc_ext      = save_buffer+2
save_hdr_sp_lo       = save_buffer+3
save_hdr_sp_hi       = save_buffer+4
save_hdr_fp_lo       = save_buffer+5
save_hdr_fp_hi       = save_buffer+6
save_hdr_callsp_lo   = save_buffer+7
save_hdr_callsp_hi   = save_buffer+8
save_hdr_rng_lo      = save_buffer+9
save_hdr_rng_hi      = save_buffer+10
save_hdr_eval_len_lo = save_buffer+11
save_hdr_eval_len_hi = save_buffer+12
save_hdr_call_len_lo = save_buffer+13
save_hdr_call_len_hi = save_buffer+14
save_hdr_diff_lo     = save_buffer+15
save_hdr_diff_hi     = save_buffer+16
save_data_base       = save_buffer+17
z_dynamic_base       = $2000 ; dynamic story bytes (0 .. static_base-1)
z_eval_stack_base    = $8000 ; eval stack
z_call_stack_base    = $8800 ; call frame stack
transcript_buffer    = $9000 ; buffered transcript output (4 KB cap)
transcript_buffer_end = $A000
z_out2_len_ram       = menu_buffer
z_out2_trunc_ram     = menu_buffer+2
z_out2_active        = menu_buffer+3
z_stream_filename_buf = menu_buffer+4

; RIOT 6530-003 timer (base $1700).  Writing to $1707 arms the 8-bit countdown
; with a /1024 clock divider.  Reading $1708 returns status: bit 7 = underflow
; since last read (cleared on read).  Emulator-only: on real KIM-1 hardware
; GETCH blocks forever so the polling path in z_timed_get_char never executes
; and timed input degrades gracefully to no-op.
; At 1 MHz: TIMED_INPUT_COUNT x 1024 = 100352 cycles ≈ 100 ms (1/10 s).
RIOT_003_TIMER_WR_1024 = $1707
RIOT_003_TIMER_STATUS  = $1708
TIMED_INPUT_COUNT      = 98

.org $A000

reset:
  cld
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
  lda z_input_stream
  beq get_key_kbd
  jsr z_script_read_char
  bcc :+
  lda #0
  sta z_input_stream
get_key_kbd:
  lda #0
  sta z_last_input_script
  jsr get_input
  and #$7F
  beq get_key
  rts
:
  lda #1
  sta z_last_input_script
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
  jsr newline
  ldx #<msg_menu_quit
  ldy #>msg_menu_quit
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
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
  cmp #'9'
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

  jsr menu_read_line
  lda z_idx
  bne :+
  jmp select_invalid
:
  lda z_idx
  cmp #1
  bne :+
  lda story_name
  cmp #'Q'
  beq select_quit
:
  jsr parse_menu_line_to_index
  bcs select_invalid

  lda z_selected_index
  cmp menu_count
  bcs select_invalid

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
  lda #' '
  jsr print_char
  clc
  rts

select_quit:
  jsr EXIT
  sec
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

; Read one editable line into story_name, terminated by Enter.
; - Supports backspace/delete.
; - Converts lowercase to uppercase.
; - Stores NUL terminator.
; Output: z_idx = character count.
menu_read_line:
  lda #0
  sta z_idx
menu_read_loop:
  jsr get_key
  cmp #$0D
  beq menu_read_done
  cmp #$0A
  beq menu_read_loop
  cmp #$08
  beq menu_read_backspace
  cmp #$7F
  beq menu_read_backspace
  ; normalize lowercase to uppercase
  cmp #'a'
  bcc :+
  cmp #'z'+1
  bcs :+
  and #$DF
:
  ; keep printable ASCII only
  cmp #' '
  bcc menu_read_loop
  cmp #$7F
  bcs menu_read_loop
  ; max 5 chars (enough for decimal index + command char like Q)
  ldy z_idx
  cpy #5
  bcs menu_read_loop
  sta story_name,y
  inc z_idx
  jmp menu_read_loop

menu_read_backspace:
  lda z_idx
  beq menu_read_loop
  dec z_idx
  jmp menu_read_loop

menu_read_done:
  ldy z_idx
  lda #0
  sta story_name,y
  jsr newline
  rts

; Parse menu input line in story_name.
; Supports:
; - legacy single-char selection: 1..9, A..F
; - decimal multi-digit selection: 1..65535 (range checked by caller)
; Output: z_selected_index = 0-based index, C clear on success.
parse_menu_line_to_index:
  lda z_idx
  cmp #1
  bne parse_menu_decimal
  lda story_name
  jsr parse_menu_key_to_index
  bcs parse_menu_decimal
  sta z_selected_index
  clc
  rts

parse_menu_decimal:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  ldy #0
parse_menu_decimal_loop:
  cpy z_idx
  beq parse_menu_decimal_done
  lda story_name,y
  cmp #'0'
  bcc parse_menu_decimal_fail
  cmp #'9'+1
  bcs parse_menu_decimal_fail
  sec
  sbc #'0'
  pha
  ; value = value*10 + digit
  ; Save old value in z_tmp/z_tmp2.
  lda z_work_ptr
  sta z_tmp
  lda z_work_ptr+1
  sta z_tmp2
  ; z_work_ptr = old*2
  asl z_work_ptr
  rol z_work_ptr+1
  ; z_tmp/z_tmp2 = old*8
  asl z_tmp
  rol z_tmp2
  asl z_tmp
  rol z_tmp2
  asl z_tmp
  rol z_tmp2
  ; z_work_ptr = old*10
  clc
  lda z_work_ptr
  adc z_tmp
  sta z_work_ptr
  lda z_work_ptr+1
  adc z_tmp2
  sta z_work_ptr+1
  ; add digit
  pla
  clc
  adc z_work_ptr
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  iny
  jmp parse_menu_decimal_loop

parse_menu_decimal_done:
  lda z_work_ptr
  ora z_work_ptr+1
  beq parse_menu_decimal_fail
  ; convert to 0-based index in A (caller does bounds check against menu_count)
  sec
  lda z_work_ptr
  sbc #1
  sta z_selected_index
  lda z_work_ptr+1
  sbc #0
  bne parse_menu_decimal_fail
  clc
  rts

parse_menu_decimal_fail:
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
  ; Save first cluster and file size before fat32_opendirent walks the chain.
  ; fat32_finddirent leaves zp_sd_address pointing at the dirent.
  ldy #26
  lda (zp_sd_address),y
  sta z_story_startcluster
  iny
  lda (zp_sd_address),y
  sta z_story_startcluster+1
  ldy #20
  lda (zp_sd_address),y
  sta z_story_startcluster+2
  iny
  lda (zp_sd_address),y
  sta z_story_startcluster+3
  ldy #28
  lda (zp_sd_address),y
  sta z_story_filesize
  iny
  lda (zp_sd_address),y
  sta z_story_filesize+1
  iny
  lda (zp_sd_address),y
  sta z_story_filesize+2
  iny
  lda (zp_sd_address),y
  sta z_story_filesize+3
  jsr fat32_opendirent
  ; libfat32 leaves zp_sd_address at readbuffer+$1E0 after open.
  ; Force first story byte read to trigger a sector fetch.
  lda #<(fat32_readbuffer+$200)
  sta zp_sd_address
  lda #>(fat32_readbuffer+$200)
  sta zp_sd_address+1
  lda #0
  sta z_story_pos
  sta z_story_pos+1
  sta z_story_pos+2
  ; Do NOT call story_cache_invalidate here.  The static-sector cache holds
  ; read-only copies of story file sectors that never change; rewinding the
  ; sequential stream does not affect cache validity.  story_cache_configure
  ; handles boot-time invalidation and is always called after a new story is
  ; first loaded.  Calling story_cache_invalidate here would silently disable
  ; the cache for the rest of gameplay after every SAVE diff or RESTART.
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
  cmp #1
  bcc :+
  cmp #9
  bcc :++
: jmp boot_unsupported_version
:

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
  stx z_story_initialpc       ; lo byte
  sta z_story_initialpc+1     ; hi byte

  lda z_story_initialpc
  sta z_pc
  lda z_story_initialpc+1
  sta z_pc+1
  lda #0
  sta z_pc_ext

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
  ; With z_dynamic_base=$2000 and interpreter code loaded at $A000,
  ; dynamic memory must be <= $8000 bytes to avoid self-overwrite.
  lda z_static_base+1
  cmp #$80
  bcc :++
  bne :+
  lda z_static_base
  beq :++
: jmp boot_dynamic_too_large
:

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
  jsr story_cache_configure
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

boot_unsupported_version:
  jsr newline
  ldx #<msg_unsupported_version
  ldy #>msg_unsupported_version
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
  rts

boot_dynamic_too_large:
  jsr newline
  ldx #<msg_dynamic_too_large
  ldy #>msg_dynamic_too_large
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  sec
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
; If target is beyond the story file size, return 0 with C clear.
story_target_in_range:
  lda z_story_target+2
  cmp z_story_filesize+2
  bcc :+
  bne story_target_oob
  lda z_story_target+1
  cmp z_story_filesize+1
  bcc :+
  bne story_target_oob
  lda z_story_target
  cmp z_story_filesize
  bcc :+
story_target_oob:
  sec
  rts
:
  clc
  rts

story_read_byte_at_target:
  jsr story_target_in_range
  bcc :+
  lda #0
  clc
  rts
:
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

; Invalidate all static-sector cache lines.
story_cache_invalidate:
  lda #0
  sta z_cache_enabled
  ldx #15                  ; clear all 16 slots (unused upper slots are harmless)
:
  sta story_cache_valid,x
  dex
  bpl :-
  rts

z_dict_cache_invalidate:
  lda #1
  sta z_dict_cache_enabled
  lda #0
  ldx #3                   ; 4-entry dict cache
:
  sta dict_cache_valid,x
  dex
  bpl :-
  rts

; Configure cache placement after dynamic memory is loaded.
; cache_base = align_up(z_dynamic_base + z_static_base, 256)
; Tries 16-slot first (8KB, base <= $60 so cache ends by $7FFF), then
; 8-slot (4KB, base <= $70), then disables the cache if neither fits.
story_cache_configure:
  jsr story_cache_invalidate
  jsr z_dict_cache_invalidate
  lda z_static_base+1
  clc
  adc #>z_dynamic_base
  tax
  lda z_static_base
  beq :+
  inx
:
  ; Try 16 slots: need base_hi+$20 <= $80, i.e. base_hi <= $60.
  cpx #$61
  bcs :+
  stx z_cache_base_hi
  lda #$0F
  sta z_cache_slot_mask
  lda #1
  sta z_cache_enabled
  rts
:
  ; Try 8 slots: need base_hi+$10 <= $80, i.e. base_hi <= $70.
  cpx #$71
  bcs :+
  stx z_cache_base_hi
  lda #$07
  sta z_cache_slot_mask
  lda #1
  sta z_cache_enabled
:
  rts

; Compute story sector index (target >> 9) into z_seek_tsec_lo/hi.
; Contract:
; - input: z_story_target is a byte address in story space
; - output: z_seek_tsec_lo/hi = physical 512-byte sector index within the story
; - clobbers: A
story_target_to_sector:
  lda z_story_target+2
  lsr
  sta z_seek_tsec_hi
  lda z_story_target+1
  ror
  sta z_seek_tsec_lo
  rts

; Cached static-byte read at z_story_target.
;
; This is the generic helper for absolute static reads. It is still used by the
; slow path in z_mem_read_byte_axy, but the common gameplay path now probes the
; same cache inline before falling back here.
;
; Cache layout:
; - direct-mapped
; - 8 or 16 lines (selected at boot by story_cache_configure)
; - 512 bytes per line
; - slot = low bits of (target >> 9), masked by z_cache_slot_mask
;
; Contract:
; - input: z_story_target is the absolute story-space byte address
; - output: A=byte, C clear on success
; - preserves: z_work_ptr on the hit path, not guaranteed on miss path
; - clobbers: A, X, Y, z_seek_tsec_lo/hi
story_read_byte_cached_at_target:
  lda z_cache_enabled
  bne :+
  jmp story_read_byte_at_target
:
  jsr story_target_in_range
  bcc :+
  lda #0
  clc
  rts
:
  jsr story_target_to_sector
  lda z_seek_tsec_lo
  and z_cache_slot_mask
  tax
  lda story_cache_valid,x
  beq story_cache_miss
  lda story_cache_tag_lo,x
  cmp z_seek_tsec_lo
  bne story_cache_miss
  lda story_cache_tag_hi,x
  cmp z_seek_tsec_hi
  bne story_cache_miss

story_cache_hit:
  ; line base = runtime_cache_base + slot*512
  lda z_story_target
  sta z_work_ptr
  txa
  asl
  clc
  adc z_cache_base_hi
  sta z_work_ptr+1
  lda z_story_target+1
  and #$01
  clc
  adc z_work_ptr+1
  sta z_work_ptr+1
  ldy #0
  lda (z_work_ptr),y
  clc
  rts

story_cache_miss:
  ; Fallback to normal stream read, then cache the full sector.
  txa
  pha                            ; save cache slot across miss path
  jsr story_read_byte_at_target
  bcc :+
  pla
  rts
:
  tay                            ; preserve returned byte
  jsr story_target_to_sector     ; restore sector tag after seek/read clobbers
  pla
  tax
  tya
  pha                            ; preserve returned byte across sector copy
  lda z_seek_tsec_lo
  sta story_cache_tag_lo,x
  lda z_seek_tsec_hi
  sta story_cache_tag_hi,x
  lda #1
  sta story_cache_valid,x

  ; Copy fat32_readbuffer (512 bytes) into selected cache line.
  txa
  asl
  clc
  adc z_cache_base_hi
  sta z_work_ptr+1
  lda #0
  sta z_work_ptr
  ldy #0
story_cache_copy_page0:
  lda fat32_readbuffer,y
  sta (z_work_ptr),y
  iny
  bne story_cache_copy_page0
  inc z_work_ptr+1
  ldy #0
story_cache_copy_page1:
  lda fat32_readbuffer+$100,y
  sta (z_work_ptr),y
  iny
  bne story_cache_copy_page1

  pla
  clc
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

; Seek the story stream to exactly z_story_target (24-bit byte offset).
; Fast path:
;   A) Already positioned (target == pos): reuse current buffer, with
;      boundary recovery if the previous read ended exactly at $xxFF.
;   B) Same sector (target >> 9 == pos >> 9): reposition zp_sd_address only.
;   C) Next sequential sector: one fat32_readnextsector call.
;   D) Any other seek: FAT cluster-chain walk from file start + one sector read.
; On return: C clear = success, C set = error.
story_seek_to_target:
  ; ------------------------------------------------------------------
  ; Tier A: already at target position, nothing to do.
  ; ------------------------------------------------------------------
  lda z_story_target
  cmp z_story_pos
  bne seek_check_sector
  lda z_story_target+1
  cmp z_story_pos+1
  bne seek_check_sector
  lda z_story_target+2
  cmp z_story_pos+2
  bne seek_check_sector
  ; If we ended exactly at buffer+512, advance one sector now.
  lda zp_sd_address+1
  cmp #>(fat32_readbuffer+$200)
  bcc seek_same_pos_ready
  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1
  jsr fat32_readnextsector
  bcc :+
  ; Recover from stale chain state by forcing an absolute seek.
  jmp seek_force_full
:
seek_same_pos_ready:
  lda z_story_target
  sta zp_sd_address
  lda z_story_target+1
  and #$01
  ora #>fat32_readbuffer
  sta zp_sd_address+1
  ; Keep fat32_bytesremaining coherent on "already positioned" reads.
  jmp seek_update_pos

seek_check_sector:
  ; ------------------------------------------------------------------
  ; Compute target_sector = z_story_target >> 9 into z_seek_tsec.
  ; ------------------------------------------------------------------
  lda z_story_target+2
  lsr
  sta z_seek_tsec_hi
  lda z_story_target+1
  ror
  sta z_seek_tsec_lo

  ; ------------------------------------------------------------------
  ; Compute cur_sector = z_story_pos >> 9 into z_seek_csec.
  ; ------------------------------------------------------------------
  lda z_story_pos+2
  lsr
  sta z_seek_csec_hi
  lda z_story_pos+1
  ror
  sta z_seek_csec_lo

  ; ------------------------------------------------------------------
  ; Tier B: same sector, reposition zp_sd_address within the buffer.
  ; ------------------------------------------------------------------
  lda z_seek_tsec_lo
  cmp z_seek_csec_lo
  bne seek_check_next
  lda z_seek_tsec_hi
  cmp z_seek_csec_hi
  bne seek_check_next
  ; If the previous read ended at buffer+512, the next sector has not yet been
  ; loaded. Pull it in before doing an in-sector pointer reposition.
  lda zp_sd_address+1
  cmp #>(fat32_readbuffer+$200)
  bcc seek_same_sector_ready
  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1
  jsr fat32_readnextsector
  bcc seek_same_sector_ready
  ; Recover from stale chain state by forcing an absolute seek.
  jmp seek_force_full
seek_same_sector_ready:
  lda z_story_target
  sta zp_sd_address
  lda z_story_target+1
  and #$01
  ora #>fat32_readbuffer
  sta zp_sd_address+1
  jmp seek_update_pos

  ; ------------------------------------------------------------------
  ; Tier C: next sequential sector, one fat32_readnextsector call.
  ; ------------------------------------------------------------------
seek_check_next:
  clc
  lda z_seek_csec_lo
  adc #1
  cmp z_seek_tsec_lo
  bne seek_full
  lda z_seek_csec_hi
  adc #0
  cmp z_seek_tsec_hi
  bne seek_full
  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1
  jsr fat32_readnextsector
  bcc :+
  jmp seek_fail
:
  lda z_story_target
  sta zp_sd_address
  lda z_story_target+1
  and #$01
  ora #>fat32_readbuffer
  sta zp_sd_address+1
  jmp seek_update_pos

  ; ------------------------------------------------------------------
  ; Tier D: arbitrary seek, walk FAT chain from file start.
  ; ------------------------------------------------------------------
seek_force_full:
  lda z_story_target+2
  lsr
  sta z_seek_tsec_hi
  lda z_story_target+1
  ror
  sta z_seek_tsec_lo

seek_full:
  lda z_story_startcluster
  sta fat32_nextcluster
  lda z_story_startcluster+1
  sta fat32_nextcluster+1
  lda z_story_startcluster+2
  sta fat32_nextcluster+2
  lda z_story_startcluster+3
  sta fat32_nextcluster+3

  ; First cluster: force FAT sector load (clc), because a data-sector read may
  ; have overwritten fat32_readbuffer since the last fat32_seekcluster call.
  clc
  jmp seek_cluster_entry

seek_cluster_walk:
  ; Subsequent clusters within this walk: FAT sector is already in the buffer.
  sec

seek_cluster_entry:
  jsr fat32_seekcluster
  bcc :+
  ; fat32_seekcluster sets carry on end-of-chain, but still leaves the current
  ; cluster geometry valid. Treat that as usable when pending sectors are nonzero.
  lda fat32_pendingsectors
  bne :+
  jmp seek_fail
:
  lda z_seek_tsec_hi
  bne seek_subtract_spc
  lda z_seek_tsec_lo
  cmp fat32_pendingsectors
  bcc seek_cluster_found

seek_subtract_spc:
  sec
  lda z_seek_tsec_lo
  sbc fat32_pendingsectors
  sta z_seek_tsec_lo
  bcs :+
  dec z_seek_tsec_hi
:
  jmp seek_cluster_walk

seek_cluster_found:
  ; Advance zp_sd_currentsector to the target sector within the cluster.
  clc
  lda zp_sd_currentsector
  adc z_seek_tsec_lo
  sta zp_sd_currentsector
  bcc :+
  inc zp_sd_currentsector+1
  bne :+
  inc zp_sd_currentsector+2
  bne :+
  inc zp_sd_currentsector+3
:
  ; Remaining sectors in this cluster after the read.
  sec
  lda fat32_pendingsectors
  sbc z_seek_tsec_lo
  sbc #1
  sta fat32_pendingsectors
  ; Read the target sector.
  lda #<fat32_readbuffer
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta zp_sd_address+1
  jsr sd_readsector
  bcs seek_fail
  ; Advance sector pointer past the sector just read.
  inc zp_sd_currentsector
  bne :+
  inc zp_sd_currentsector+1
  bne :+
  inc zp_sd_currentsector+2
  bne :+
  inc zp_sd_currentsector+3
:
  ; Reposition within the buffer.
  lda z_story_target
  sta zp_sd_address
  lda z_story_target+1
  and #$01
  ora #>fat32_readbuffer
  sta zp_sd_address+1

seek_update_pos:
  ; Update fat32_bytesremaining = file_size - z_story_target.
  sec
  lda z_story_filesize
  sbc z_story_target
  sta fat32_bytesremaining
  lda z_story_filesize+1
  sbc z_story_target+1
  sta fat32_bytesremaining+1
  lda z_story_filesize+2
  sbc z_story_target+2
  sta fat32_bytesremaining+2
  lda z_story_filesize+3
  sbc #0
  sta fat32_bytesremaining+3
  bcs :+
  ; Clamp on underflow so out-of-range seeks cannot wrap remaining-bytes.
  lda #0
  sta fat32_bytesremaining
  sta fat32_bytesremaining+1
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
:

seek_pos_done:
  lda z_story_target
  sta z_story_pos
  lda z_story_target+1
  sta z_story_pos+1
  lda z_story_target+2
  sta z_story_pos+2
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
  sta z_pc_ext
  sta z_zs_ptr_ext
  sta z_ptr_ext
  lda #$01
  sta z_rng_state_lo
  lda #$00
  sta z_rng_state_hi
  sta z_save_valid
  sta z_out2_active
  sta z_out3_active
  sta z_out3_ptr
  sta z_out3_ptr+1
  sta z_out3_len
  sta z_out3_len+1
  sta z_out3_table
  sta z_out3_table+1
  sta z_zs_shift_once
  sta z_window_lines
  sta z_window_active
  sta z_input_stream
  sta z_script_pos
  sta z_script_pos+1
  sta z_last_input_script
  sta z_call_nostore
  sta z_out2_len_ram
  sta z_out2_len_ram+1
  sta z_out2_trunc_ram
  rts

; Copy dynamic story bytes [0 .. static_base-1] to RAM z_dynamic_base.
load_dynamic_memory:
  ; Rewind story stream to byte 0.
  jsr story_rewind_open
  bcs load_dynamic_fail

  ; Progress indicator: print one dot about every 1024 bytes copied.
  lda #$00
  sta z_load_dot_ctr
  lda #$04
  sta z_load_dot_ctr+1

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

  ; Throttled progress output while loading dynamic memory.
  lda z_load_dot_ctr
  bne :+
  dec z_load_dot_ctr+1
:
  dec z_load_dot_ctr
  lda z_load_dot_ctr
  ora z_load_dot_ctr+1
  bne :+
  lda #'.'
  jsr print_char
  lda #$00
  sta z_load_dot_ctr
  lda #$04
  sta z_load_dot_ctr+1
:
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
;
; Contract:
; - input: A/X = 16-bit story address
; - output: A=byte, C clear on success
; - always updates z_tmp/z_tmp2 with the original AX input
;   Several callers rely on this side effect after the read returns.
; - static reads force Y=0 before entering z_mem_read_static
; - dynamic reads may clobber z_work_ptr
z_mem_read_byte_ax:
  sta z_tmp
  stx z_tmp2
  cpx z_static_base+1
  bcs :+
  jmp z_mem_read_dynamic
:
  bne :+
  lda z_tmp
  cmp z_static_base
  bcs :+
  jmp z_mem_read_dynamic
:
  ldy #0
  jmp z_mem_read_static

; Read story byte at address AXY (24-bit) using RAM for dynamic when ext=0.
;
; This is the main VM memory-read entry point. The control flow is:
; 1) classify ext=0 reads as dynamic vs static using z_static_base
; 2) for static reads, probe the inline direct-hit cache path first
; 3) fall back to story_read_byte_cached_at_target only on cache miss / disabled cache
;
; Contract:
; - input: A/X/Y = 24-bit story address
; - output: A=byte, C clear on success
; - updates z_tmp/z_tmp2 with the low 16 bits of the input address
; - dynamic reads may clobber z_work_ptr
; - static slow path preserves z_work_ptr for callers that depend on it
z_mem_read_byte_axy:
  sta z_tmp
  stx z_tmp2
  cpy #0
  bne z_mem_read_static_ext_entry
  ldx z_tmp2
  cpx z_static_base+1
  bcs :+
  jmp z_mem_read_dynamic
:
  bne z_mem_read_static
  lda z_tmp
  cmp z_static_base
  bcs :+
  jmp z_mem_read_dynamic
:

z_mem_read_static:
  ; Common v3/v5 path: 16-bit static address with ext byte already zero in Y.
  ; Profiling note: this is the hottest remaining read bucket in gameplay traces.
  ; The watch labels below are intentional and used by tools/profile_hotspots.py.
  lda z_cache_enabled
  beq z_mem_read_static_slow_zero
  lda z_tmp2
  lsr
  sta z_seek_tsec_lo
  and z_cache_slot_mask
  tax
  lda story_cache_valid,x
  beq z_mem_read_static_slow_zero
  lda story_cache_tag_lo,x
  cmp z_seek_tsec_lo
  bne z_mem_read_static_slow_zero
  lda story_cache_tag_hi,x
  bne z_mem_read_static_slow_zero
z_mem_read_static_hit_zero:
  ; For ext=0 the cache-line page is:
  ;   z_cache_base_hi + slot*2 + ((addr_hi & 1) ? 1 : 0)
  ; = z_cache_base_hi + (addr_hi & page_mask)
  ; where page_mask = (slot_mask << 1) | 1  ($0F for 8-slot, $1F for 16-slot).
  lda z_tmp
  sta z_seek_csec_lo
  lda z_cache_slot_mask    ; $07 or $0F
  asl                       ; $0E or $1E
  ora #1                    ; $0F or $1F (= page_mask)
  and z_tmp2                ; addr_hi & page_mask
  clc
  adc z_cache_base_hi
  sta z_seek_csec_hi
  lda (z_seek_csec_lo),y
  clc
  rts

z_mem_read_static_slow_zero:
  ldy #0
  jmp z_mem_read_static_ext
z_mem_read_static_ext_entry:
  ; Separate label retained for bucket profiling.
  jmp z_mem_read_static_ext
z_mem_read_static_ext:
  ; Hot path: probe the static-sector cache directly from AX/Y and only
  ; fall back to the slower generic helper on cache miss.
  lda z_cache_enabled
  beq z_mem_read_static_slow
  ; Early out-of-range check: if (Y, z_tmp2, z_tmp) >= z_story_filesize, return
  ; 0 immediately without touching the slow path.  This saves ~35 cycles per call
  ; for legitimately-out-of-range reads (e.g. past story end) that correctly
  ; return 0 anyway.  Avoids ~800k slow-path round-trips on the 50-command trace.
  tya
  cmp z_story_filesize+2
  bcc :+                         ; ext < filesize+2 → in range
  bne z_mem_static_ext_oob       ; ext > filesize+2 → out of range
  lda z_tmp2                     ; ext == filesize+2: compare mid byte
  cmp z_story_filesize+1
  bcc :+                         ; mid < filesize+1 → in range
  bne z_mem_static_ext_oob       ; mid > filesize+1 → out of range
  lda z_tmp                      ; mid == filesize+1: compare low byte
  cmp z_story_filesize
  bcc :+                         ; low < filesize → in range
z_mem_static_ext_oob:
  lda #0
  clc
  rts
:
  tya
  lsr
  sta z_seek_tsec_hi
  lda z_tmp2
  ror
  sta z_seek_tsec_lo
  and z_cache_slot_mask
  tax
  lda story_cache_valid,x
  beq z_mem_read_static_slow
  lda story_cache_tag_lo,x
  cmp z_seek_tsec_lo
  bne z_mem_read_static_slow
  lda story_cache_tag_hi,x
  cmp z_seek_tsec_hi
  bne z_mem_read_static_slow
z_mem_read_static_hit_ext:
  ; ext!=0 path keeps the more explicit slot*512 + high-half calculation.
  lda z_tmp
  sta z_seek_csec_lo
  txa
  asl
  clc
  adc z_cache_base_hi
  sta z_seek_csec_hi
  lda z_tmp2
  and #$01
  beq :+
  inc z_seek_csec_hi
:
  ldy #0
  lda (z_seek_csec_lo),y
  clc
  rts

z_mem_read_static_slow:
  ; Slow path uses the generic absolute-target reader.
  ; Important invariant: preserve z_work_ptr across the helper call because some
  ; callers assume static reads do not destroy it.
  lda z_tmp
  sta z_story_target
  lda z_tmp2
  sta z_story_target+1
  tya
  sta z_story_target+2
  ; Preserve z_work_ptr for static reads. Some call paths depend on it
  ; remaining stable across z_mem_read_byte_ax.
  lda z_work_ptr+1
  pha
  lda z_work_ptr
  pha
  jsr story_read_byte_cached_at_target
  tay
  pla
  sta z_work_ptr
  pla
  sta z_work_ptr+1
  tya
  rts

z_mem_read_dynamic:
  ; pointer = z_dynamic_base + AX
  ; Dynamic reads are RAM-only and do not touch story stream state.
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
  sta z_tmp2
  lda z_fp
  ora z_fp+1
  bne :+
  lda #0
  tax
  rts
:
  lda z_tmp2
  sec
  sbc #1
  asl
  sta z_work_ptr
  clc
  adc #9
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1
  clc
  lda z_fp
  adc z_work_ptr
  sta z_work_ptr
  lda z_fp+1
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
  sta z_tmp
  txa
  pha
  lda z_fp
  ora z_fp+1
  bne :+
  pla
  rts
:
  lda z_tmp2
  sec
  sbc #1
  asl
  sta z_work_ptr
  clc
  adc #9
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1
  clc
  lda z_fp
  adc z_work_ptr
  sta z_work_ptr
  lda z_fp+1
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
; z_storevar holds the destination variable id.
; z_call_nostore is 1 for call forms that suppress the return-value store.
z_call_common:
  lda z_op1_lo
  ora z_op1_hi
  bne z_call_nonzero
  ; Routine 0: immediately return false to store var (if any).
  lda z_call_nostore
  bne :+
  ldy z_storevar
  lda #0
  tax
  jsr z_set_var_word
:
  clc
  rts

z_call_nonzero:
  ; Seed call_arg_buf[0..3] from decoded operand registers so non-VAR call
  ; forms (e.g. call_2s/call_2n) pass correct arguments.
  lda z_op1_lo
  sta call_arg_buf+0
  lda z_op1_hi
  sta call_arg_buf+1
  lda z_op2_lo
  sta call_arg_buf+2
  lda z_op2_hi
  sta call_arg_buf+3
  lda z_op3_lo
  sta call_arg_buf+4
  lda z_op3_hi
  sta call_arg_buf+5
  lda z_op4_lo
  sta call_arg_buf+6
  lda z_op4_hi
  sta call_arg_buf+7

  ; routine byte address from packed routine address (version-aware scale)
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
  jsr z_unpack_paddr_work

  ; Preserve previous frame pointer.
  lda z_fp
  sta z_story_target
  lda z_fp+1
  sta z_story_target+1

  ; Create fixed-size frame (39 bytes):
  ; [0..1] prev fp, [2..4] return pc, [5] store var,
  ; [6..7] caller eval-stack depth, [8] flags/local count, [9..38] locals.
  lda z_callsp
  sta z_fp
  lda z_callsp+1
  sta z_fp+1
  clc
  lda z_callsp
  adc #39
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
  ; frame[4] = return pc ext
  iny
  lda z_pc_ext
  sta (z_fp),y
  ; frame[5] = store var
  iny
  lda z_storevar
  sta (z_fp),y
  ; frame[6..7] = caller eval stack pointer
  iny
  lda z_sp
  sta (z_fp),y
  iny
  lda z_sp+1
  sta (z_fp),y

  ; Read num_locals at routine start.
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_ptr_ext
  jsr z_mem_read_byte_axy
  bcc :+
  jmp z_call_fail
:
  and #$0F
  sta z_tmp2
  ; frame[8]:
  ; - bit7    = no-store call flag
  ; - bits4-6 = arg count (0..7)
  ; - bits0-3 = num_locals
  lda z_opcount
  beq z_call_pack_frame
  sec
  sbc #1
  cmp #8
  bcc :+
  lda #7
:
  asl
  asl
  asl
  asl
z_call_pack_frame:
  sta z_tmp
  lda z_call_nostore
  beq :+
  lda z_tmp
  ora #$80
  sta z_tmp
:
  lda z_tmp
  ora z_tmp2
  ldy #8
  sta (z_fp),y

  ; Zero-initialize local slots [9..38]
  ldy #9
  lda #0
z_call_zero_locals:
  cpy #39
  beq z_call_load_defaults
  sta (z_fp),y
  iny
  jmp z_call_zero_locals

z_call_load_defaults:
  lda #0
  sta z_idx
  ; Advance z_work_ptr/z_ptr_ext to the first byte after the local-count header.
  ; In v1-v4 this is the first local default word. In v5+ locals are always
  ; zero-initialized, so execution starts immediately at routine_base + 1.
  jsr z_inc_work_ptr_ext
  lda z_story_version
  cmp #5
  bcs z_call_apply_args
z_call_default_loop:
  ldy #8
  lda (z_fp),y       ; num_locals from frame
  and #$0F
  cmp z_idx
  beq z_call_apply_args
  ; Read default word at running pointer.
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_ptr_ext
  jsr z_mem_read_byte_axy
  bcc :+
  jmp z_call_fail
:
  pha
  jsr z_inc_work_ptr_ext
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_ptr_ext
  jsr z_mem_read_byte_axy
  bcc :+
  pla
  jmp z_call_fail
:
  sta z_tmp          ; low byte
  pla                ; high byte
  tax
  lda z_tmp
  jsr z_inc_work_ptr_ext
  ; Store A=lo, X=hi into frame slot [9 + z_idx*2].
  ; Save lo (A) on stack while computing Y offset.
  pha
  lda z_idx
  asl
  clc
  adc #9
  tay
  pla
  sta (z_fp),y
  iny
  txa
  sta (z_fp),y
  inc z_idx
  jmp z_call_default_loop

z_call_apply_args:
  ; argc = opcount-1, cap to 7 and num_locals
  lda z_opcount
  beq z_call_set_pc
  sec
  sbc #1
  sta z_idx
  lda z_idx
  cmp #8
  bcc :+
  lda #7
  sta z_idx
:
  ldy #8
  lda (z_fp),y       ; num_locals from frame
  and #$0F
  cmp z_idx
  bcs :+             ; num_locals >= z_idx, keep z_idx
  sta z_idx          ; cap z_idx to num_locals
:
  lda z_idx
  beq z_call_set_pc
  ; Apply args 1..argc from decoded call_arg_buf entries [1..argc].
  ; localN is stored at frame offset 9 + (N-1)*2.
  ldx #0
z_call_apply_args_loop:
  cpx z_idx
  beq z_call_set_pc
  ; src offset = 2*(x+1)
  txa
  asl
  clc
  adc #2
  tay
  lda call_arg_buf,y
  sta z_tmp
  iny
  lda call_arg_buf,y
  sta z_tmp2
  ; dst offset = 9 + 2*x
  txa
  asl
  clc
  adc #9
  tay
  lda z_tmp
  sta (z_fp),y
  iny
  lda z_tmp2
  sta (z_fp),y
  inx
  jmp z_call_apply_args_loop

z_call_set_pc:
  ; z_work_ptr = routine_base + 1 + 2*num_locals = first instruction address.
  ; The default loop maintained z_work_ptr as a running pointer, so it is
  ; already correct regardless of how many locals were loaded.
  lda z_work_ptr
  sta z_pc
  lda z_work_ptr+1
  sta z_pc+1
  lda z_ptr_ext
  sta z_pc_ext
  clc
  rts

z_call_fail:
  sec
  rts

; Unpack packed address in z_work_ptr/z_work_ptr+1 into byte address.
; Output: z_work_ptr/z_work_ptr+1/z_ptr_ext (24-bit).
; Scale factor by version:
;   v1-3: *2
;   v4-7: *4
;   v8:   *8
z_unpack_paddr_work:
  lda #0
  sta z_ptr_ext
  lda z_story_version
  cmp #8
  bcs z_unpack_scale8
  cmp #4
  bcs z_unpack_scale4

z_unpack_scale2:
  asl z_work_ptr
  rol z_work_ptr+1
  rol z_ptr_ext
  rts

z_unpack_scale4:
  asl z_work_ptr
  rol z_work_ptr+1
  rol z_ptr_ext
  asl z_work_ptr
  rol z_work_ptr+1
  rol z_ptr_ext
  rts

z_unpack_scale8:
  asl z_work_ptr
  rol z_work_ptr+1
  rol z_ptr_ext
  asl z_work_ptr
  rol z_work_ptr+1
  rol z_ptr_ext
  asl z_work_ptr
  rol z_work_ptr+1
  rol z_ptr_ext
  rts

z_inc_work_ptr_ext:
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
  bne :+
  inc z_ptr_ext
:
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
  sta z_pc_ext
  iny
  lda (z_fp),y
  sta z_storevar
  iny
  lda (z_fp),y
  sta z_sp
  iny
  lda (z_fp),y
  sta z_sp+1
  iny
  lda (z_fp),y
  sta z_branch_cond
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
  lda z_branch_cond
  bmi :+
  lda z_storevar
  tay
  lda z_tmp
  ldx z_tmp2
  jsr z_set_var_word
:
  clc
  rts

; Read big-endian word at AX into A=low, X=high.
; z_mem_read_byte_ax sets z_tmp=addr_lo, z_tmp2=addr_hi at entry,
; and for dynamic reads clobbers z_work_ptr (sets it to z_dynamic_base+addr).
; We reconstruct original+1 from z_tmp/z_tmp2 to avoid this corruption.
z_mem_read_word_ax:
  sta z_work_ptr
  stx z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax        ; hi byte -> A; sets z_tmp=addr_lo, z_tmp2=addr_hi
  bcs z_mem_read_word_fail
  pha                            ; save hi byte (second jsr clobbers z_tmp)
  ; Compute original+1 from z_tmp/z_tmp2 (safe across both static and dynamic reads)
  clc
  lda z_tmp                      ; original addr lo
  adc #1
  sta z_work_ptr
  lda z_tmp2                     ; original addr hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax         ; lo byte -> A
  bcs z_mem_read_word_fail_pull
  sta z_tmp                      ; save lo byte
  pla                            ; hi byte -> A
  tax                            ; hi -> X
  lda z_tmp                      ; lo -> A
  clc
  rts

z_mem_read_word_fail_pull:
  pla
z_mem_read_word_fail:
  sec
  rts

; Write big-endian word at AX from value A=low, X=high.
z_mem_write_word_ax:
  ; Preserve value and original address across byte writes.
  sta z_story_target
  txa
  sta z_story_target+1
  lda z_work_ptr
  sta z_work_cnt
  lda z_work_ptr+1
  sta z_work_cnt+1
  ; high byte first
  lda z_work_cnt
  ldx z_work_cnt+1
  ldy z_story_target+1
  jsr z_mem_write_byte_ax
  bcs z_mem_write_word_fail
  ; low byte second at original address + 1
  clc
  lda z_work_cnt
  adc #1
  sta z_work_ptr
  lda z_work_cnt+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_story_target
  jsr z_mem_write_byte_ax
  bcs z_mem_write_word_fail
  clc
  rts

z_mem_write_word_fail:
  sec
  rts

;------------------------------------------------------------
; Object table helpers
; - v1-v3: 31 default-property words, 9-byte object entries
; - v4+:   63 default-property words, 14-byte object entries
;------------------------------------------------------------
; Input A = object number (1..255). Output z_work_ptr = object entry address.
; C set if object is 0.
z_obj_addr:
  beq z_obj_addr_fail
  sec
  sbc #1
  sta z_tmp
  lda z_story_version
  cmp #4
  bcc z_obj_addr_v3
  ; v4+: offset = 126 + 14*(object-1)
  lda z_tmp
  asl
  sta z_work_ptr
  lda #0
  rol
  sta z_work_ptr+1          ; z_work_ptr = 2*n
  lda z_work_ptr
  sta z_work_cnt
  lda z_work_ptr+1
  sta z_work_cnt+1          ; z_work_cnt = 2*n
  ldx #6
z_obj_addr_v4_mul_loop:
  clc
  lda z_work_ptr
  adc z_work_cnt
  sta z_work_ptr
  lda z_work_ptr+1
  adc z_work_cnt+1
  sta z_work_ptr+1
  dex
  bne z_obj_addr_v4_mul_loop
  clc
  lda z_work_ptr
  adc #126
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  jmp z_obj_addr_add_base
z_obj_addr_v3:
  ; offset = 62 + 9*(object-1)
  lda #0
  sta z_work_ptr+1
  lda z_tmp
  asl
  rol z_work_ptr+1
  asl
  rol z_work_ptr+1
  asl
  rol z_work_ptr+1
  clc
  adc z_tmp
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  clc
  lda z_work_ptr
  adc #62
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
z_obj_addr_add_base:
  clc
  lda z_work_ptr
  adc z_object_table
  sta z_work_ptr
  lda z_work_ptr+1
  adc z_object_table+1
  sta z_work_ptr+1
  ldx #0
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
  lda z_story_version
  cmp #4
  bcc z_obj_get_parent_v3
  clc
  lda z_work_ptr
  adc #7
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  clc
  rts
z_obj_get_parent_v3:
  clc
  lda z_work_ptr
  adc #4
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
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
  lda z_story_version
  cmp #4
  bcc z_obj_get_sibling_v3
  clc
  lda z_work_ptr
  adc #9
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  clc
  rts
z_obj_get_sibling_v3:
  clc
  lda z_work_ptr
  adc #5
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
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
  lda z_story_version
  cmp #4
  bcc z_obj_get_child_v3
  clc
  lda z_work_ptr
  adc #11
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  clc
  rts
z_obj_get_child_v3:
  clc
  lda z_work_ptr
  adc #6
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
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
  lda z_story_version
  cmp #4
  bcc z_obj_set_parent_v3
  sty z_tmp2
  clc
  lda z_work_ptr
  adc #6
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  ldy #0
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #7
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  ldy z_tmp2
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  rts
z_obj_set_parent_v3:
  clc
  lda z_work_ptr
  adc #4
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
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
  lda z_story_version
  cmp #4
  bcc z_obj_set_sibling_v3
  sty z_tmp2
  clc
  lda z_work_ptr
  adc #8
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  ldy #0
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #9
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  ldy z_tmp2
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  rts
z_obj_set_sibling_v3:
  clc
  lda z_work_ptr
  adc #5
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
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
  lda z_story_version
  cmp #4
  bcc z_obj_set_child_v3
  sty z_tmp2
  clc
  lda z_work_ptr
  adc #10
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  ldy #0
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #11
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  ldy z_tmp2
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  rts
z_obj_set_child_v3:
  clc
  lda z_work_ptr
  adc #6
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_write_byte_ax
  clc
  rts

; Remove object A from tree.
z_obj_remove:
  sta z_work_cnt+1          ; obj (preserved across helper calls)
  jsr z_obj_get_parent
  sta z_idx                 ; parent (preserved across helper calls)
  beq z_obj_remove_clear_links

  ; if parent.child == obj, update to obj.sibling
  lda z_idx
  jsr z_obj_get_child
  cmp z_work_cnt+1
  bne z_obj_remove_scan_siblings
  lda z_work_cnt+1
  jsr z_obj_get_sibling
  tay
  lda z_idx
  jsr z_obj_set_child
  jmp z_obj_remove_clear_links

z_obj_remove_scan_siblings:
  lda z_idx
  jsr z_obj_get_child
  sta z_work_cnt            ; prev
z_obj_remove_scan_loop:
  lda z_work_cnt
  beq z_obj_remove_clear_links
  jsr z_obj_get_sibling
  cmp z_work_cnt+1
  beq z_obj_remove_patch_prev
  sta z_work_cnt
  jmp z_obj_remove_scan_loop

z_obj_remove_patch_prev:
  lda z_work_cnt+1
  jsr z_obj_get_sibling
  tay
  lda z_work_cnt
  jsr z_obj_set_sibling

z_obj_remove_clear_links:
  lda z_work_cnt+1
  ldy #0
  jsr z_obj_set_parent
  lda z_work_cnt+1
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
  lda z_story_version
  cmp #4
  bcc z_obj_get_prop_table_addr_v3
  lda z_work_ptr
  clc
  adc #12
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_word_ax
  bcs z_obj_get_prop_table_addr_fail
  sta z_work_ptr
  stx z_work_ptr+1
  clc
  rts
z_obj_get_prop_table_addr_v3:
  ; Preserve object entry base on stack; z_mem_read_byte_ax clobbers z_work_ptr
  ; and z_tmp/z_tmp2 on dynamic reads.
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha

  lda z_work_ptr
  clc
  adc #7
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  bcc :+
  pla
  pla
  sec
  rts
:
  sta z_work_cnt              ; hi
  pla
  sta z_work_ptr+1
  pla
  sta z_work_ptr

  lda z_work_ptr
  clc
  adc #8
  sta z_tmp
  lda z_work_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  bcs z_obj_get_prop_table_addr_fail
  sta z_work_ptr             ; lo
  lda z_work_cnt
  sta z_work_ptr+1           ; hi
  clc
  rts
z_obj_get_prop_table_addr_fail:
  sec
  rts

; Decode the property header at z_prop_ptr.
; Outputs on success (C clear):
; - z_prop_num  = property number
; - z_prop_size = property data length in bytes
; - z_idx       = header length in bytes (1 or 2)
; C set on terminator or read failure.
z_prop_read_header:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  bcs z_prop_read_header_fail
  beq z_prop_read_header_fail
  sta z_tmp2
  lda z_story_version
  cmp #4
  bcc z_prop_read_header_v3
  lda z_tmp2
  and #$3F
  sta z_prop_num
  lda #1
  sta z_idx
  lda z_tmp2
  and #$80
  beq z_prop_read_header_v4_short
  clc
  lda z_prop_ptr
  adc #1
  sta z_work_ptr
  lda z_prop_ptr+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcs z_prop_read_header_fail
  and #$3F
  bne :+
  lda #64
:
  sta z_prop_size
  lda #2
  sta z_idx
  clc
  rts
z_prop_read_header_v4_short:
  lda z_tmp2
  and #$40
  beq :+
  lda #2
  bne :++
:
  lda #1
:
  sta z_prop_size
  clc
  rts
z_prop_read_header_v3:
  lda z_tmp2
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
  lda #1
  sta z_idx
  clc
  rts
z_prop_read_header_fail:
  sec
  rts

; Inputs:
; - z_op1_lo = object number
; - z_op2_lo = property number
; Outputs on found (C clear):
; - z_prop_ptr points to property data bytes
; - z_prop_num set
; - z_prop_size set
; C set if not found/invalid.
z_prop_find_object_prop:
  lda z_op1_lo
  jsr z_obj_get_prop_table_addr
  bcc :+
  sec
  rts
:
  ; Preserve property-table base; z_mem_read_byte_ax clobbers z_work_ptr.
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  ; Skip short name: 1 + 2*name_words
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcc :+
  pla
  pla
  jmp z_prop_find_fail
:
  sta z_tmp2
  pla
  sta z_work_ptr+1
  pla
  sta z_work_ptr
  lda z_tmp2
  and #$1F
  asl
  clc
  adc #1
  sta z_tmp
  lda z_work_ptr
  adc z_tmp
  sta z_prop_ptr
  lda z_work_ptr+1
  adc #0
  sta z_prop_ptr+1

z_prop_scan_loop:
  jsr z_prop_read_header
  bcs z_prop_find_fail
  lda z_prop_num
  cmp z_op2_lo
  beq z_prop_found
  bcc z_prop_find_fail         ; list is descending, no future match
  ; advance ptr by header + size
  clc
  lda z_prop_ptr
  adc z_idx
  adc z_prop_size
  sta z_prop_ptr
  bcc :+
  inc z_prop_ptr+1
:
  jmp z_prop_scan_loop

z_prop_found:
  ; move pointer from header to data
  clc
  lda z_prop_ptr
  adc z_idx
  sta z_prop_ptr
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
  ; Preserve property-table base; z_mem_read_byte_ax clobbers z_work_ptr.
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha

  ; first byte is name length in words
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcc :+
  pla
  pla
  rts
:
  and #$1F
  sta z_work_cnt
  lda #0
  sta z_work_cnt+1
  pla
  sta z_work_ptr+1
  pla
  sta z_work_ptr
  beq :+
  clc
  lda z_work_ptr
  adc #1
  sta z_zs_ptr
  lda z_work_ptr+1
  adc #0
  sta z_zs_ptr+1
  lda #0
  sta z_zs_ptr_ext
  jsr z_decode_zstring_words_at_ptr
:
  rts

; Decode/print exactly z_work_cnt words at z_zs_ptr.
z_decode_zstring_words_at_ptr:
  lda #0
  sta z_zs_shift
  sta z_zs_abbrev
  sta z_zs_escape
  sta z_zs_shift_once
z_decode_words_loop:
  lda z_work_cnt
  beq z_decode_words_done
  lda z_zs_ptr
  ldx z_zs_ptr+1
  ldy z_zs_ptr_ext
  jsr z_mem_read_byte_axy
  bcc :+
  jmp z_decode_fail
:
  sta z_zs_word_hi
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
  bne :+
  inc z_zs_ptr_ext
:
  lda z_zs_ptr
  ldx z_zs_ptr+1
  ldy z_zs_ptr_ext
  jsr z_mem_read_byte_axy
  bcc :+
  jmp z_decode_fail
:
  sta z_zs_word_lo
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
  bne :+
  inc z_zs_ptr_ext
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
  ldx z_story_version
  cpx #4
  bcc :+
  cmp #48
  bcs z_obj_attr_test_done
  bcc :++
:
  cmp #32
  bcs z_obj_attr_test_done
:
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
  beq :+
  lda #1
  sta z_tmp
  jmp z_obj_attr_test_done
:
  lda #0
  sta z_tmp
z_obj_attr_test_done:
  clc
  rts

; Uses op1 object and op2 attribute, z_tmp2 = 1 set / 0 clear.
z_obj_attr_write:
  lda z_op2_hi
  bne z_obj_attr_write_done
  lda z_op2_lo
  ldx z_story_version
  cpx #4
  bcc :+
  cmp #48
  bcs z_obj_attr_write_done
  bcc :++
:
  cmp #32
  bcs z_obj_attr_write_done
:
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
  ; z_mem_read_byte_ax clobbers z_work_ptr on dynamic reads.
  ; Preserve story-space attribute address and set/clear mode for the
  ; subsequent write.
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  lda z_tmp2
  pha
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  sta z_idx
  pla
  sta z_tmp2
  pla
  sta z_work_ptr+1
  pla
  sta z_work_ptr
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
  bne run_vm_after_boot
  jsr boot_story
  bcc run_vm_after_boot
  jmp run_vm_done
run_vm_after_boot:
  jsr newline
  ldx #<msg_vm_run
  ldy #>msg_vm_run
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  jsr newline
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
  lda z_pc_ext
  jsr print_hex
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
  lda z_pc_ext
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

  ; 0OP / VAR form subset ??? reload opcode (A was contaminated by and #$C0 above)
  lda z_opcode
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
  lda z_story_version
  cmp #5
  bcc zm_dispatch_pop_v14
  jmp op_catch
zm_dispatch_pop_v14:
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
  cmp #$BC            ; show_status
  bne :+
  jmp op_show_status
:
  cmp #$BD            ; verify
  bne :+
  jmp op_verify
:
  cmp #$BE            ; extended prefix (v5+)
  bne z_dispatch_check_piracy
  lda z_story_version
  cmp #5
  bcc z_dispatch_be_unsupported
  jmp zm_handle_ext
z_dispatch_be_unsupported:
  jmp zm_unknown_opcode

z_dispatch_check_piracy:
  cmp #$BF            ; piracy
  bne :+
  lda z_story_version
  cmp #5
  bcc z_dispatch_bf_unsupported
  jmp op_piracy
z_dispatch_bf_unsupported:
  jmp zm_unknown_opcode
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
  cmp #$EA            ; split_window
  bne :+
  jmp op_split_window_var
:
  cmp #$EB            ; set_window
  bne :+
  jmp op_set_window_var
:
  cmp #$EC            ; call_vs2
  bne :+
  jmp op_call_vs2
:
  cmp #$ED            ; erase_window
  bne :+
  jmp op_erase_window_var
:
  cmp #$EE            ; erase_line
  bne :+
  jmp op_erase_line_var
:
  cmp #$EF            ; set_cursor
  bne :+
  jmp op_set_cursor_var
:
  cmp #$F0            ; get_cursor
  bne :+
  jmp op_get_cursor_var
:
  cmp #$F1            ; set_text_style
  bne :+
  jmp op_set_text_style_var
:
  cmp #$F2            ; buffer_mode
  bne :+
  jmp op_buffer_mode_var
:
  cmp #$F3            ; output_stream
  bne :+
  jmp op_output_stream_var
:
  cmp #$F4            ; input_stream
  bne :+
  jmp op_input_stream_var
:
  cmp #$F5            ; sound_effect
  bne :+
  jmp op_sound_effect_var
:
  cmp #$F6            ; read_char
  bne :+
  jmp op_read_char_var
:
  cmp #$F7            ; scan_table
  bne :+
  jmp op_scan_table_var
:
  cmp #$F8            ; not (v5+)
  bne :+
  jmp op_not_var
:
  cmp #$F9            ; call_vn
  bne :+
  jmp op_call_vn
:
  cmp #$FA            ; call_vn2
  bne :+
  jmp op_call_vn2
:
  cmp #$FB            ; tokenise
  bne :+
  jmp op_tokenise_var
:
  cmp #$FC            ; encode_text
  bne :+
  jmp op_encode_text_var
:
  cmp #$FD            ; copy_table
  bne :+
  jmp op_copy_table_var
:
  cmp #$FE            ; print_table
  bne :+
  jmp op_print_table_var
:
  cmp #$FF            ; check_arg_count
  bne :+
  jmp op_check_arg_count_var
:

  jmp zm_unknown_opcode

; Handle v5+ extended-form instructions (0xBE prefix).
zm_handle_ext:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_opcode                  ; reuse opcode slot for ext opcode number
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_opcode
  cmp #$00            ; save
  bne :+
  jmp op_save
:
  cmp #$01            ; restore
  bne :+
  jmp op_restore
:
  cmp #$02            ; log_shift
  bne :+
  jmp op_ext_log_shift
:
  cmp #$03            ; art_shift
  bne :+
  jmp op_ext_art_shift
:
  cmp #$04            ; set_font
  bne :+
  jmp op_ext_set_font
:
  cmp #$05            ; draw_picture
  bne :+
  jmp op_ext_draw_picture
:
  cmp #$06            ; picture_data
  bne :+
  jmp op_ext_picture_data
:
  cmp #$07            ; erase_picture
  bne :+
  jmp op_ext_erase_picture
:
  cmp #$08            ; set_margins
  bne :+
  jmp op_ext_set_margins
:
  cmp #$09            ; save_undo
  bne :+
  jmp op_ext_save_undo
:
  cmp #$0A            ; restore_undo
  bne :+
  jmp op_ext_restore_undo
:
  cmp #$0B            ; print_unicode
  bne :+
  jmp op_ext_print_unicode
:
  cmp #$0C            ; check_unicode
  bne :+
  jmp op_ext_check_unicode
:
  cmp #$0D            ; set_true_colour
  bne :+
  jmp op_ext_set_true_colour
:
  cmp #$10            ; move_window
  bne :+
  jmp op_ext_move_window
:
  cmp #$11            ; window_size
  bne :+
  jmp op_ext_window_size
:
  cmp #$12            ; window_style
  bne :+
  jmp op_ext_window_style
:
  cmp #$13            ; get_wind_prop
  bne :+
  jmp op_ext_get_wind_prop
:
  cmp #$14            ; scroll_window
  bne :+
  jmp op_ext_scroll_window
:
  cmp #$15            ; pop_stack
  bne :+
  jmp op_ext_pop_stack
:
  cmp #$16            ; read_mouse
  bne :+
  jmp op_ext_read_mouse
:
  cmp #$17            ; mouse_window
  bne :+
  jmp op_ext_mouse_window
:
  cmp #$18            ; push_stack
  bne :+
  jmp op_ext_push_stack
:
  cmp #$19            ; put_wind_prop
  bne :+
  jmp op_ext_put_wind_prop
:
  cmp #$1A            ; print_form
  bne :+
  jmp op_ext_print_form
:
  cmp #$1B            ; make_menu
  bne :+
  jmp op_ext_make_menu
:
  cmp #$1C            ; picture_table
  bne :+
  jmp op_ext_picture_table
:
  cmp #$1D            ; buffer_screen
  bne :+
  jmp op_ext_buffer_screen
: 
  ; Later standards specify that unknown extended opcodes >= 29 should be ignored.
  cmp #$1D
  bcs :+
  jmp zm_unknown_opcode
:
  clc
  rts

; Handle long-form 2OP instructions (bit7 clear, two operands).
zm_handle_long_2op:
  lda #2
  sta z_opcount
  lda #0
  sta z_op1_raw
  sta z_op2_raw
  ; operand 1
  lda z_opcode
  and #$40
  bne z_long_op1_var
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_raw
  sta z_op1_lo
  lda #0
  sta z_op1_hi
  jmp z_long_op2_decode
z_long_op1_var:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_raw
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
  sta z_op2_raw
  sta z_op2_lo
  lda #0
  sta z_op2_hi
  jmp z_long_dispatch
z_long_op2_var:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op2_raw
  jsr z_get_var_word
  sta z_op2_lo
  stx z_op2_hi

z_long_dispatch:
  lda z_opcode
  and #$1F            ; long form: opcode is bits 4-0 (bit6=op1 type, bit5=op2 type)
  cmp #$00            ; undefined in standard (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
:
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
  cmp #$19            ; call_2s
  bne :+
  jmp op_2op_call_2s
:
  cmp #$1A            ; call_2n
  bne :+
  jmp op_2op_call_2n
:
  cmp #$1B            ; set_colour
  bne :+
  jmp op_2op_set_colour
:
  cmp #$1C            ; throw
  bne :+
  jmp op_2op_throw
:
  cmp #$1D            ; undefined in standard (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
:
  cmp #$1E            ; undefined/extended (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
:
  cmp #$1F            ; undefined/extended (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
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
  cmp #$00            ; undefined in standard (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
:
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
  cmp #$1B            ; set_colour
  bne :+
  jmp op_2op_set_colour
:
  cmp #$1C            ; throw
  bne :+
  jmp op_2op_throw
:
  cmp #$1D            ; undefined in standard (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
:
  cmp #$1E            ; undefined/extended (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
:
  cmp #$1F            ; undefined/extended (compat: treat as nop)
  bne :+
  jmp op_2op_compat_nop
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
  bcc op_2op_jl_false
  bne op_2op_jl_true
  lda z_op1_lo
  cmp z_op2_lo
  bcc op_2op_jl_true
op_2op_jl_false:
  lda #0
  jmp z_branch_on_bool
op_2op_jl_true:
  lda #1
  jmp z_branch_on_bool

op_2op_jg:
  ; signed compare op1 > op2
  lda z_op1_hi
  eor #$80
  sta z_tmp
  lda z_op2_hi
  eor #$80
  cmp z_tmp
  bcc op_2op_jg_true
  bne op_2op_jg_false
  lda z_op2_lo
  cmp z_op1_lo
  bcc op_2op_jg_true
op_2op_jg_false:
  lda #0
  jmp z_branch_on_bool
op_2op_jg_true:
  lda #1
  jmp z_branch_on_bool

op_2op_dec_chk:
  ; op1 var-id, op2 value; branch if (--var) < op2 (signed)
  lda z_op1_raw
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
  ; Preserve decremented value across z_set_var_word (clobbers z_tmp/z_tmp2).
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  ldy z_op1_raw
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  pla
  sta z_op1_hi
  pla
  sta z_op1_lo
  jmp op_2op_jl

op_2op_inc_chk:
  ; op1 var-id, op2 value; branch if (++var) > op2 (signed)
  lda z_op1_raw
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
  ; Preserve incremented value across z_set_var_word (clobbers z_tmp/z_tmp2).
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  ldy z_op1_raw
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  pla
  sta z_op1_hi
  pla
  sta z_op1_lo
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

op_2op_store:
  ldy z_op1_raw
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
  pha
  txa
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  jmp zm_stop
:
  tay
  pla
  ldx #0
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
  ; defaults are words, big-endian. Use word helper so z_work_ptr clobbering in
  ; dynamic reads cannot skew the second byte fetch.
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_word_ax       ; A=lo, X=hi
  bcc :+
  jmp zm_stop
:
  sta z_work_ptr
  stx z_work_ptr+1
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
  ; Word-sized property: use helper so z_work_ptr clobbering cannot corrupt low byte.
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_word_ax
  bcc :+
  jmp zm_stop
:
  sta z_work_ptr
  stx z_work_ptr+1
  jmp op_2op_get_prop_store
op_2op_get_prop_zero:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
op_2op_get_prop_store:
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  ; Preserve property-table base; z_mem_read_byte_ax clobbers z_work_ptr.
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  bcc :+
  pla
  pla
  jmp zm_stop
:
  sta z_tmp2
  pla
  sta z_work_ptr+1
  pla
  sta z_work_ptr
  lda z_tmp2
  and #$1F
  asl
  clc
  adc #1
  sta z_tmp
  lda z_work_ptr
  adc z_tmp
  sta z_prop_ptr
  lda z_work_ptr+1
  adc #0
  sta z_prop_ptr+1
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_prop_read_header
  bcs op_2op_get_next_prop_zero
  lda z_prop_num
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
  jsr z_prop_read_header
  bcs op_2op_get_next_prop_zero
  lda z_prop_num
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp op_2op_get_next_prop_store

op_2op_get_next_prop_zero:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
op_2op_get_next_prop_store:
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

; Minimal 16-bit signed multiply/div/mod (fast-enough bring-up path)
op_2op_mul:
  jsr z_mul_16
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

op_2op_div:
  jsr z_divmod_16
  bcc :+
  jmp zm_stop
:
  ; quotient in z_work_ptr
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda #0
  sta z_call_nostore
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_storevar
  jmp z_call_common

op_2op_call_2n:
  lda #2
  sta z_opcount
  lda #1
  sta z_call_nostore
  jmp z_call_common

op_2op_set_colour:
  ; Monochrome console target: honor opcode presence but ignore colours.
  clc
  rts

; Return the current catch token in A(low)/X(high).
; We use a small frame-depth token so v5 code sees a stable opaque value.
z_catch_token_current:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  lda z_fp
  sta z_story_target
  lda z_fp+1
  sta z_story_target+1
z_catch_token_loop:
  lda z_story_target
  ora z_story_target+1
  beq z_catch_token_done
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  ldy #0
  lda (z_story_target),y
  pha
  iny
  lda (z_story_target),y
  sta z_story_target+1
  pla
  sta z_story_target
  jmp z_catch_token_loop
z_catch_token_done:
  lda z_work_ptr
  ldx z_work_ptr+1
  rts

op_2op_throw:
  ; Unwind to the caught frame token in op2, then return op1 from that frame.
  jsr z_catch_token_current
  sta z_work_ptr
  stx z_work_ptr+1
z_throw_unwind_loop:
  lda z_work_ptr+1
  cmp z_op2_hi
  bne :+
  lda z_work_ptr
  cmp z_op2_lo
  beq z_throw_found
:
  lda z_work_ptr
  ora z_work_ptr+1
  bne :+
  sec
  rts
:
  ldy #0
  lda (z_fp),y
  sta z_story_target
  iny
  lda (z_fp),y
  sta z_story_target+1
  ldy #6
  lda (z_fp),y
  sta z_sp
  iny
  lda (z_fp),y
  sta z_sp+1
  lda z_fp
  sta z_callsp
  lda z_fp+1
  sta z_callsp+1
  lda z_story_target
  sta z_fp
  lda z_story_target+1
  sta z_fp+1
  sec
  lda z_work_ptr
  sbc #1
  sta z_work_ptr
  lda z_work_ptr+1
  sbc #0
  sta z_work_ptr+1
  jmp z_throw_unwind_loop

z_throw_found:
  lda z_op1_lo
  ldx z_op1_hi
  jmp z_return_word

; Non-standard compatibility path:
; 2OP opcode number $1D is undefined in the spec. Some malformed story paths
; can surface this byte; treat as a no-op so VM execution can continue.
op_2op_compat_nop:
  clc
  rts

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
  lda z_op1_lo
  sta z_op1_varid
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
  lda z_op1_lo
  sta z_op1_varid
  jmp zm_short_dispatch

zm_short_var:
  ; variable
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_op1_varid
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
  cmp #$0F            ; not / call_1n
  bne :+
  lda z_story_version
  cmp #5
  bcc zm_dispatch_not_v14
  jmp op_1op_call_1n
zm_dispatch_not_v14:
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
  lda z_work_ptr               ; preserve sibling for branch test
  pha
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  pla
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
  lda z_work_ptr               ; preserve child for branch test
  pha
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  pla
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

op_1op_inc:
  lda z_op1_varid
  jsr z_get_var_word
  clc
  adc #1
  sta z_work_ptr
  txa
  adc #0
  sta z_work_ptr+1
  ldy z_op1_varid
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_1op_dec:
  lda z_op1_varid
  jsr z_get_var_word
  sec
  sbc #1
  sta z_work_ptr
  txa
  sbc #0
  sta z_work_ptr+1
  ldy z_op1_varid
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
  lda #0
  sta z_zs_ptr_ext
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
  lda z_story_version
  cmp #4
  bcc op_1op_get_prop_len_v3
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
  sta z_tmp2
  and #$C0
  bne op_1op_get_prop_len_v4_short
  sec
  lda z_op1_lo
  sbc #2
  sta z_work_ptr
  lda z_op1_hi
  sbc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  and #$80
  beq op_1op_get_prop_len_v4_size1
  lda z_tmp2
  and #$3F
  bne :+
  lda #64
:
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp op_1op_get_prop_len_store
op_1op_get_prop_len_v4_short:
  lda z_tmp2
  and #$40
  beq op_1op_get_prop_len_v4_size1
  lda #2
  bne op_1op_get_prop_len_store_a
op_1op_get_prop_len_v4_size1:
  lda #1
op_1op_get_prop_len_store_a:
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp op_1op_get_prop_len_store
op_1op_get_prop_len_v3:
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda z_op1_hi
  bpl :+
  lda z_pc_ext
  sbc #0
  sta z_pc_ext
  jmp :++
:
  lda z_pc_ext
  adc #0
  sta z_pc_ext
:
  sec
  lda z_pc
  sbc #2
  sta z_pc
  lda z_pc+1
  sbc #0
  sta z_pc+1
  lda z_pc_ext
  sbc #0
  sta z_pc_ext
  clc
  rts

op_1op_print_paddr:
  ; packed address -> byte address (version-aware scale)
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
  jsr z_unpack_paddr_work
  lda z_work_ptr
  sta z_zs_ptr
  lda z_work_ptr+1
  sta z_zs_ptr+1
  lda z_ptr_ext
  sta z_zs_ptr_ext
  jsr z_decode_zstring_at_ptr
  clc
  rts

op_1op_load:
  lda z_op1_varid
  jsr z_get_var_word
  sta z_work_ptr
  stx z_work_ptr+1
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

op_1op_call_1s:
  lda #1
  sta z_opcount
  lda #0
  sta z_call_nostore
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_storevar
  jmp z_call_common

op_1op_call_1n:
  lda #1
  sta z_opcount
  lda #1
  sta z_call_nostore
  jmp z_call_common

op_1op_not:
  lda z_op1_lo
  eor #$FF
  sta z_work_ptr
  lda z_op1_hi
  eor #$FF
  sta z_work_ptr+1
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

z_branch_on_bool:
  ; Input A: 0=false, nonzero=true.
  ; Normalize and preserve bool in scratch across zm_fetch_byte.
  beq :+
  lda #1
  bne :++
:
  lda #0
:
  sta z_work_cnt

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

  ; one-byte branch offset: 6-bit unsigned value (0..63).
  lda z_tmp2
  and #$3F
  sta z_branch_off
  lda #0
  sta z_branch_off_hi
  jmp z_branch_eval

z_branch_two_byte:
  ; Compute z_branch_off_hi from z_tmp2 BEFORE the second fetch clobbers z_tmp2.
  lda z_tmp2
  and #$3F
  sta z_branch_off_hi
  lda z_branch_off_hi
  and #$20
  beq :+
  lda z_branch_off_hi
  ora #$C0
  sta z_branch_off_hi
:
  jsr zm_fetch_byte
  bcc :+
  jmp zm_stop
:
  sta z_branch_off

z_branch_eval:
  lda z_work_cnt
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
  lda z_branch_off_hi
  bpl :+
  lda z_pc_ext
  sbc #0
  sta z_pc_ext
  jmp :++
:
  lda z_pc_ext
  adc #0
  sta z_pc_ext
:
  sec
  lda z_pc
  sbc #2
  sta z_pc
  lda z_pc+1
  sbc #0
  sta z_pc+1
  lda z_pc_ext
  sbc #0
  sta z_pc_ext
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

; z_divmod_u16: unsigned 16-bit divide z_op1 / z_op2
; quotient -> z_work_ptr, remainder -> z_branch_off
; C set on divide-by-zero
z_divmod_u16:
  lda z_op2_lo
  ora z_op2_hi
  bne :+
  sec
  rts
:
  lda z_op1_lo
  sta z_div_d_lo
  lda z_op1_hi
  sta z_div_d_hi
  lda z_op2_lo
  sta z_div_v_lo
  lda z_op2_hi
  sta z_div_v_hi
  lda #0
  sta z_div_r_lo
  sta z_div_r_hi
  sta z_div_q_lo
  sta z_div_q_hi
  ldx #16
z_div_u_loop:
  asl z_div_d_lo
  rol z_div_d_hi
  rol z_div_r_lo
  rol z_div_r_hi
  asl z_div_q_lo
  rol z_div_q_hi
  lda z_div_r_hi
  cmp z_div_v_hi
  bcc z_div_u_next
  bne z_div_u_sub
  lda z_div_r_lo
  cmp z_div_v_lo
  bcc z_div_u_next
z_div_u_sub:
  sec
  lda z_div_r_lo
  sbc z_div_v_lo
  sta z_div_r_lo
  lda z_div_r_hi
  sbc z_div_v_hi
  sta z_div_r_hi
  inc z_div_q_lo
  bne z_div_u_next
  inc z_div_q_hi
z_div_u_next:
  dex
  bne z_div_u_loop

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
  jsr z_vm_put_char
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
  jsr z_vm_put_char
  lda #1
  sta z_tmp
z_print_digit_skip:
  rts

; Encode token at text buffer (op1) using z_tok_start/z_tok_len.
; V1-3 produce 4 bytes in z_enc0..z_enc3.
; V4+ produce 6 bytes in z_enc0..z_enc3 + z_enc4_ram/z_enc5_ram.
; Current encoder maps lowercase a-z directly and pads all other chars with 5.
z_encode_token_key:
  lda z_story_version
  cmp #4
  bcs :+
  jmp z_encode_token_key_v13
:

  ldx #0
  lda #5
z_encode_v45_pad:
  sta call_arg_buf,x
  inx
  cpx #9
  bcc z_encode_v45_pad
  ldx #0
z_encode_v45_loop:
  cpx #9
  bcs z_encode_v45_pack
  txa
  cmp z_tok_len
  bcs z_encode_v45_pack
  stx z_idx
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_tok_start
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  cmp #'A'
  bcc :+
  cmp #'Z'+1
  bcs :+
  ora #$20
:
  cmp #'a'
  bcc z_encode_v45_next
  cmp #'z'+1
  bcs z_encode_v45_next
  sec
  sbc #'a'-6
  ldx z_idx
  sta call_arg_buf,x
z_encode_v45_next:
  ldx z_idx
  inx
  jmp z_encode_v45_loop

z_encode_v45_pack:
  lda call_arg_buf+0
  asl
  asl
  sta z_tmp
  lda call_arg_buf+1
  lsr
  lsr
  lsr
  ora z_tmp
  sta z_enc0
  lda call_arg_buf+1
  and #$07
  asl
  asl
  asl
  asl
  asl
  ora call_arg_buf+2
  sta z_enc1
  lda call_arg_buf+3
  asl
  asl
  sta z_tmp
  lda call_arg_buf+4
  lsr
  lsr
  lsr
  ora z_tmp
  sta z_enc2
  lda call_arg_buf+4
  and #$07
  asl
  asl
  asl
  asl
  asl
  ora call_arg_buf+5
  sta z_enc3
  lda call_arg_buf+6
  asl
  asl
  sta z_tmp
  lda call_arg_buf+7
  lsr
  lsr
  lsr
  ora z_tmp
  ora #$80
  sta z_enc4_ram
  lda call_arg_buf+7
  and #$07
  asl
  asl
  asl
  asl
  asl
  ora call_arg_buf+8
  sta z_enc5_ram
  rts

z_encode_token_key_v13:
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
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_tok_start
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
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
  lda z_div_d_hi
  and #$07
  asl
  asl
  asl
  asl
  asl
  ora z_div_v_lo
  sta z_enc1
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

; Input A=ASCII char.
; C set if char is listed as a dictionary word-separator.
z_dict_is_separator:
  sta z_tmp
  lda z_dictionary
  sta z_prop_ptr
  lda z_dictionary+1
  sta z_prop_ptr+1
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  sta z_work_cnt
  beq z_dict_sep_no
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
z_dict_sep_loop:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  cmp z_tmp
  beq z_dict_sep_yes
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  dec z_work_cnt
  bne z_dict_sep_loop
z_dict_sep_no:
  clc
  rts
z_dict_sep_yes:
  sec
  rts

; Lookup encoded token in dictionary z_dictionary.
; Returns dictionary entry address in z_work_ptr (0 if not found).
z_dict_lookup_token:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  lda z_tok_len
  bne :+
  rts
:
  lda z_story_version
  cmp #4
  bcs z_dict_lookup_scan
  lda z_dict_cache_enabled
  beq z_dict_lookup_scan
  lda z_enc0
  eor z_enc1
  eor z_enc2
  eor z_enc3
  and #$03
  sta z_idx
  tax
  lda dict_cache_valid,x
  beq z_dict_lookup_scan
  lda dict_cache_key0,x
  cmp z_enc0
  bne z_dict_lookup_scan
  lda dict_cache_key1,x
  cmp z_enc1
  bne z_dict_lookup_scan
  lda dict_cache_key2,x
  cmp z_enc2
  bne z_dict_lookup_scan
  lda dict_cache_key3,x
  cmp z_enc3
  bne z_dict_lookup_scan
  lda dict_cache_val_lo,x
  sta z_work_ptr
  lda dict_cache_val_hi,x
  sta z_work_ptr+1
  rts

z_dict_lookup_scan:
  lda z_enc0
  eor z_enc1
  eor z_enc2
  eor z_enc3
  and #$03
  sta z_idx
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
  lda z_work_cnt+1
  bpl :+
  lda z_work_cnt
  eor #$FF
  sta z_work_cnt
  lda z_work_cnt+1
  eor #$FF
  sta z_work_cnt+1
  clc
  lda z_work_cnt
  adc #1
  sta z_work_cnt
  lda z_work_cnt+1
  adc #0
  sta z_work_cnt+1
: ; entry pointer = ptr + 1
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
z_dict_scan_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  bne :+
  jmp z_dict_not_found
: 
  ; compare first 4 bytes
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_read_byte_ax
  cmp z_enc0
  bne z_dict_next
  clc
  lda z_prop_ptr
  adc #1
  sta z_tmp
  lda z_prop_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  cmp z_enc1
  bne z_dict_next
  clc
  lda z_prop_ptr
  adc #2
  sta z_tmp
  lda z_prop_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  cmp z_enc2
  bne z_dict_next
  clc
  lda z_prop_ptr
  adc #3
  sta z_tmp
  lda z_prop_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  cmp z_enc3
  bne z_dict_next
  lda z_story_version
  cmp #4
  bcc z_dict_found
  clc
  lda z_prop_ptr
  adc #4
  sta z_tmp
  lda z_prop_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  cmp z_enc4_ram
  bne z_dict_next
  clc
  lda z_prop_ptr
  adc #5
  sta z_tmp
  lda z_prop_ptr+1
  adc #0
  tax
  lda z_tmp
  jsr z_mem_read_byte_ax
  cmp z_enc5_ram
  bne z_dict_next
z_dict_found:
  ; found
  lda z_prop_ptr
  sta z_work_ptr
  lda z_prop_ptr+1
  sta z_work_ptr+1
  jsr z_dict_cache_store
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
  jsr z_dict_cache_store
  rts

z_dict_cache_store:
  lda z_story_version
  cmp #4
  bcs :+
  lda z_dict_cache_enabled
  beq :+
  ldx z_idx
  lda #1
  sta dict_cache_valid,x
  lda z_enc0
  sta dict_cache_key0,x
  lda z_enc1
  sta dict_cache_key1,x
  lda z_enc2
  sta dict_cache_key2,x
  lda z_enc3
  sta dict_cache_key3,x
  lda z_work_ptr
  sta dict_cache_val_lo,x
  lda z_work_ptr+1
  sta dict_cache_val_hi,x
:
  rts

; Seek SD story stream to current VM PC for efficient subsequent static reads.
z_seek_stream_to_pc:
  ; SAVE/RESTORE and other FAT32 operations can leave fat32_readbuffer holding a
  ; non-story sector while z_story_pos still matches the current PC. Force the
  ; next seek to rebuild stream state from z_story_startcluster instead of taking
  ; the "already at target position" fast path against stale buffer contents.
  lda #$FF
  sta z_story_pos
  sta z_story_pos+1
  sta z_story_pos+2
  lda z_pc
  sta z_story_target
  lda z_pc+1
  sta z_story_target+1
  lda z_pc_ext
  sta z_story_target+2
  jsr story_seek_to_target
  rts

; Append byte A to save buffer at z_work_ptr.
; C set if out of save-buffer space.
z_save_write_byte_a:
  pha
  lda z_work_ptr+1
  cmp #>save_buffer_end
  bcs z_save_write_byte_fail
  ldy #0
  pla
  sta (z_work_ptr),y
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  clc
  rts
z_save_write_byte_fail:
  pla
  sec
  rts

; Read byte A from save buffer at z_work_ptr and advance.
; C set if read is out of save-buffer range.
z_save_read_byte_a:
  lda z_work_ptr+1
  cmp #>save_buffer_end
  bcs z_save_read_byte_fail
  ldy #0
  lda (z_work_ptr),y
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  clc
  rts
z_save_read_byte_fail:
  sec
  rts

; Copy z_work_cnt bytes from source z_prop_ptr into save buffer z_work_ptr.
; C set on save-buffer overflow.
z_save_copy_to_buffer:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_save_copy_done
z_save_copy_loop:
  ldy #0
  lda (z_prop_ptr),y
  jsr z_save_write_byte_a
  bcs z_save_copy_fail
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  lda z_work_cnt
  ora z_work_cnt+1
  bne z_save_copy_loop
z_save_copy_done:
  clc
  rts
z_save_copy_fail:
  sec
  rts

; Copy z_work_cnt bytes from save buffer z_work_ptr into destination z_prop_ptr.
; C set on malformed/overflowing snapshot.
z_restore_copy_from_buffer:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_restore_copy_done
z_restore_copy_loop:
  jsr z_save_read_byte_a
  bcs z_restore_copy_fail
  ldy #0
  sta (z_prop_ptr),y
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  lda z_work_cnt
  ora z_work_cnt+1
  bne z_restore_copy_loop
z_restore_copy_done:
  clc
  rts
z_restore_copy_fail:
  sec
  rts

z_save_snapshot:
  lda #0
  sta z_save_valid
  ; Save core VM registers.
  lda z_pc
  sta save_hdr_pc_lo
  lda z_pc+1
  sta save_hdr_pc_hi
  lda z_pc_ext
  sta save_hdr_pc_ext
  lda z_sp
  sta save_hdr_sp_lo
  lda z_sp+1
  sta save_hdr_sp_hi
  lda z_fp
  sta save_hdr_fp_lo
  lda z_fp+1
  sta save_hdr_fp_hi
  lda z_callsp
  sta save_hdr_callsp_lo
  lda z_callsp+1
  sta save_hdr_callsp_hi
  lda z_rng_state_lo
  sta save_hdr_rng_lo
  lda z_rng_state_hi
  sta save_hdr_rng_hi
  ; eval_len = z_sp - z_eval_stack_base
  sec
  lda z_sp
  sbc #<z_eval_stack_base
  sta save_hdr_eval_len_lo
  lda z_sp+1
  sbc #>z_eval_stack_base
  sta save_hdr_eval_len_hi
  ; call_len = z_callsp - z_call_stack_base
  sec
  lda z_callsp
  sbc #<z_call_stack_base
  sta save_hdr_call_len_lo
  lda z_callsp+1
  sbc #>z_call_stack_base
  sta save_hdr_call_len_hi
  lda #0
  sta save_hdr_diff_lo
  sta save_hdr_diff_hi
  ; save_ptr = save_data_base
  lda #<save_data_base
  sta z_work_ptr
  lda #>save_data_base
  sta z_work_ptr+1
  ; Save used eval stack bytes.
  lda #<z_eval_stack_base
  sta z_prop_ptr
  lda #>z_eval_stack_base
  sta z_prop_ptr+1
  lda save_hdr_eval_len_lo
  sta z_work_cnt
  lda save_hdr_eval_len_hi
  sta z_work_cnt+1
  jsr z_save_copy_to_buffer
  bcc :+
  jmp z_save_snapshot_fail
:
  ; Save used call stack bytes.
  lda #<z_call_stack_base
  sta z_prop_ptr
  lda #>z_call_stack_base
  sta z_prop_ptr+1
  lda save_hdr_call_len_lo
  sta z_work_cnt
  lda save_hdr_call_len_hi
  sta z_work_cnt+1
  jsr z_save_copy_to_buffer
  bcc :+
  jmp z_save_snapshot_fail
:
  ; Diff dynamic memory against original story bytes.
  jsr story_rewind_open
  bcc :+
  jmp z_save_snapshot_fail
:
  lda #<z_dynamic_base
  sta z_prop_ptr
  lda #>z_dynamic_base
  sta z_prop_ptr+1
  lda z_static_base
  sta z_work_cnt
  lda z_static_base+1
  sta z_work_cnt+1
  lda #0
  sta z_story_target
  sta z_story_target+1
  sta z_story_target+2
z_save_diff_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_save_snapshot_done
  jsr story_read_byte_at_target
  bcc :+
  jmp z_save_snapshot_fail
:
  sta z_tmp                    ; original byte
  ldy #0
  lda (z_prop_ptr),y           ; current dynamic byte
  cmp z_tmp
  beq z_save_diff_next
  sta z_tmp2                   ; preserve changed byte
  lda z_story_target
  jsr z_save_write_byte_a
  bcs z_save_snapshot_fail
  lda z_story_target+1
  jsr z_save_write_byte_a
  bcs z_save_snapshot_fail
  lda z_tmp2
  jsr z_save_write_byte_a
  bcs z_save_snapshot_fail
  inc save_hdr_diff_lo
  bne :+
  inc save_hdr_diff_hi
:
z_save_diff_next:
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  jsr target_inc
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_save_diff_loop

z_save_snapshot_done:
  jsr z_save_snapshot_sd_write
  bcc :+
  jmp z_save_snapshot_fail
:
  jsr story_rewind_open
  jsr z_seek_stream_to_pc
  lda #1
  sta z_save_valid
  clc
  rts

z_save_snapshot_fail:
  jsr story_rewind_open
  jsr z_seek_stream_to_pc
  lda #0
  sta z_save_valid
  sec
  rts

z_restore_snapshot:
  jsr z_save_snapshot_sd_read
  bcc :+
  jsr z_seek_stream_to_pc
  lda #0
  sta z_save_valid
  sec
  rts
:
  lda #1
  sta z_save_valid
  ; Rebuild dynamic memory before applying snapshot deltas.
  jsr load_dynamic_memory
  bcc :+
  jsr z_seek_stream_to_pc
  sec
  rts
:
  lda #<save_data_base
  sta z_work_ptr
  lda #>save_data_base
  sta z_work_ptr+1
  ; Restore eval stack bytes.
  lda #<z_eval_stack_base
  sta z_prop_ptr
  lda #>z_eval_stack_base
  sta z_prop_ptr+1
  lda save_hdr_eval_len_lo
  sta z_work_cnt
  lda save_hdr_eval_len_hi
  sta z_work_cnt+1
  jsr z_restore_copy_from_buffer
  bcc :+
  jmp z_restore_snapshot_fail
:
  ; Restore call stack bytes.
  lda #<z_call_stack_base
  sta z_prop_ptr
  lda #>z_call_stack_base
  sta z_prop_ptr+1
  lda save_hdr_call_len_lo
  sta z_work_cnt
  lda save_hdr_call_len_hi
  sta z_work_cnt+1
  jsr z_restore_copy_from_buffer
  bcc :+
  jmp z_restore_snapshot_fail
:
  ; Apply dynamic-memory diff records.
  lda save_hdr_diff_lo
  sta z_work_cnt
  lda save_hdr_diff_hi
  sta z_work_cnt+1
z_restore_diff_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_restore_snapshot_done
  jsr z_save_read_byte_a
  bcs z_restore_snapshot_fail
  sta z_tmp                    ; addr lo
  jsr z_save_read_byte_a
  bcs z_restore_snapshot_fail
  sta z_tmp2                   ; addr hi
  jsr z_save_read_byte_a
  bcs z_restore_snapshot_fail
  sta z_idx                    ; value
  clc
  lda z_tmp
  adc #<z_dynamic_base
  sta z_prop_ptr
  lda z_tmp2
  adc #>z_dynamic_base
  sta z_prop_ptr+1
  ldy #0
  lda z_idx
  sta (z_prop_ptr),y
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_restore_diff_loop

z_restore_snapshot_done:
  ; Restore saved VM registers.
  lda save_hdr_pc_lo
  sta z_pc
  lda save_hdr_pc_hi
  sta z_pc+1
  lda save_hdr_pc_ext
  sta z_pc_ext
  lda save_hdr_sp_lo
  sta z_sp
  lda save_hdr_sp_hi
  sta z_sp+1
  lda save_hdr_fp_lo
  sta z_fp
  lda save_hdr_fp_hi
  sta z_fp+1
  lda save_hdr_callsp_lo
  sta z_callsp
  lda save_hdr_callsp_hi
  sta z_callsp+1
  lda save_hdr_rng_lo
  sta z_rng_state_lo
  lda save_hdr_rng_hi
  sta z_rng_state_hi
  ; Stream 3 does not survive restore.
  lda #0
  sta z_out3_active
  jsr z_seek_stream_to_pc
  clc
  rts

z_restore_snapshot_fail:
  jsr z_seek_stream_to_pc
  sec
  rts

z_out3_flush_len:
  lda z_out3_table
  sta z_work_ptr
  lda z_out3_table+1
  sta z_work_ptr+1
  lda z_out3_len
  ldx z_out3_len+1
  jsr z_mem_write_word_ax
  rts

; Read global variable Y (0-based) into A=lo, X=hi.
z_status_get_global_word:
  tya
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
  lda z_work_ptr
  ldx z_work_ptr+1
  jmp z_mem_read_word_ax

; Compute serialized snapshot byte count into z_work_cnt (and A/X).
; size = 17 + eval_len + call_len + 3*diff_count
z_save_snapshot_size:
  lda #<(save_data_base-save_buffer)
  sta z_work_cnt
  lda #>(save_data_base-save_buffer)
  sta z_work_cnt+1
  clc
  lda z_work_cnt
  adc save_hdr_eval_len_lo
  sta z_work_cnt
  lda z_work_cnt+1
  adc save_hdr_eval_len_hi
  sta z_work_cnt+1
  clc
  lda z_work_cnt
  adc save_hdr_call_len_lo
  sta z_work_cnt
  lda z_work_cnt+1
  adc save_hdr_call_len_hi
  sta z_work_cnt+1
  ; diff3 = diff + 2*diff
  lda save_hdr_diff_lo
  sta z_tmp
  lda save_hdr_diff_hi
  sta z_tmp2
  lda z_tmp
  sta z_work_ptr
  lda z_tmp2
  sta z_work_ptr+1
  asl z_tmp
  rol z_tmp2
  clc
  lda z_tmp
  adc z_work_ptr
  sta z_tmp
  lda z_tmp2
  adc z_work_ptr+1
  sta z_tmp2
  clc
  lda z_work_cnt
  adc z_tmp
  sta z_work_cnt
  lda z_work_cnt+1
  adc z_tmp2
  sta z_work_cnt+1
  lda z_work_cnt
  ldx z_work_cnt+1
  rts

; Build an 8.3 companion filename from the current story name.
; Input A/X/Y = extension bytes 0..2. Output in z_stream_filename_buf.
build_stream_filename:
  sta z_tmp
  stx z_tmp2
  sty z_idx
  ldx #0
:
  cpx #8
  beq :+
  lda story_name,x
  sta z_stream_filename_buf,x
  inx
  bne :-
:
  lda z_tmp
  sta z_stream_filename_buf+8
  lda z_tmp2
  sta z_stream_filename_buf+9
  lda z_idx
  sta z_stream_filename_buf+10
  rts

build_script_filename:
  lda #'S'
  ldx #'C'
  ldy #'R'
  jmp build_stream_filename

build_transcript_filename:
  lda #'T'
  ldx #'R'
  ldy #'N'
  jmp build_stream_filename

; Buffer one character for transcript stream 2.
; Input: A=character. C clear on return.
z_out2_buffer_char:
  pha
  lda z_out2_active
  beq z_out2_buffer_done_pop
  lda z_out2_trunc_ram
  bne z_out2_buffer_done_pop
  lda z_out2_len_ram+1
  cmp #>(transcript_buffer_end-transcript_buffer)
  bcc :+
  bne z_out2_buffer_trunc
  lda z_out2_len_ram
  cmp #<(transcript_buffer_end-transcript_buffer)
  bcs z_out2_buffer_trunc
:
  clc
  lda z_out2_len_ram
  adc #<transcript_buffer
  sta z_work_ptr
  lda z_out2_len_ram+1
  adc #>transcript_buffer
  sta z_work_ptr+1
  pla
  ldy #0
  sta (z_work_ptr),y
  inc z_out2_len_ram
  bne :+
  inc z_out2_len_ram+1
: clc
  rts
z_out2_buffer_trunc:
  lda #1
  sta z_out2_trunc_ram
z_out2_buffer_done_pop:
  pla
  clc
  rts

; Write the current transcript buffer to the companion .TRN file.
; Overwrites any previous file. C clear on success.
z_out2_write_file:
  jsr build_transcript_filename
  jsr fat32_openroot
  ldx #<z_stream_filename_buf
  ldy #>z_stream_filename_buf
  jsr fat32_finddirent
  bcs :+
  jsr fat32_deletefile
:
  lda z_out2_len_ram
  ora z_out2_len_ram+1
  bne :+
  jsr z_seek_stream_to_pc
  clc
  rts
:
  lda z_out2_len_ram
  sta fat32_bytesremaining
  lda z_out2_len_ram+1
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_allocatefile
  bcc :+
  jsr z_seek_stream_to_pc
  sec
  rts
:
  jsr fat32_openroot
  lda #<z_stream_filename_buf
  sta fat32_filenamepointer
  lda #>z_stream_filename_buf
  sta fat32_filenamepointer+1
  lda z_out2_len_ram
  sta fat32_bytesremaining
  lda z_out2_len_ram+1
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_writedirent
  bcc :+
  jsr z_seek_stream_to_pc
  sec
  rts
:
  lda #<transcript_buffer
  sta fat32_address
  lda #>transcript_buffer
  sta fat32_address+1
  lda z_out2_len_ram
  sta fat32_bytesremaining
  lda z_out2_len_ram+1
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_file_write
  jsr z_seek_stream_to_pc
  clc
  rts

; Flush and disable transcript stream 2.
z_out2_finalize:
  lda z_out2_active
  beq z_out2_finalize_done
  jsr z_out2_write_file
  lda #0
  sta z_out2_active
  sta z_out2_len_ram
  sta z_out2_len_ram+1
  sta z_out2_trunc_ram
z_out2_finalize_done:
  rts

; Read the next byte from the per-story script companion file.
; Returns A=byte with C clear, or C set on EOF/missing file.
z_script_read_char:
  lda z_story_booted
  bne :+
  sec
  rts
:
  jsr build_script_filename
  jsr fat32_openroot
  ldx #<z_stream_filename_buf
  ldy #>z_stream_filename_buf
  jsr fat32_finddirent
  bcc :+
  jsr z_seek_stream_to_pc
  sec
  rts
:
  jsr fat32_opendirent
  lda #<(fat32_readbuffer+$200)
  sta zp_sd_address
  lda #>(fat32_readbuffer+$200)
  sta zp_sd_address+1
  lda z_script_pos
  sta z_work_cnt
  lda z_script_pos+1
  sta z_work_cnt+1
z_script_skip_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_script_read_now
  jsr fat32_file_readbyte
  bcc :+
  jsr z_seek_stream_to_pc
  sec
  rts
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_script_skip_loop
z_script_read_now:
  jsr fat32_file_readbyte
  bcc :+
  jsr z_seek_stream_to_pc
  sec
  rts
:
  pha
  inc z_script_pos
  bne :+
  inc z_script_pos+1
:
  jsr z_seek_stream_to_pc
  pla
  clc
  rts

; Build the per-story save filename in z_save_filename_buf.
; Takes the 8-byte name field from story_name (FAT32 8.3 format, space-padded)
; and appends "SAV" as the extension.  E.g. "ZORK1   DAT" -> "ZORK1   SAV".
; Called at the start of every SAVE/RESTORE operation.
build_save_filename:
  ldx #0
:
  cpx #8
  beq :+
  lda story_name,x
  sta z_save_filename_buf,x
  inx
  bne :-
:
  lda #'S'
  sta z_save_filename_buf+8
  lda #'A'
  sta z_save_filename_buf+9
  lda #'V'
  sta z_save_filename_buf+10
  rts

; Persist current save_buffer snapshot to SD card save file.
; C clear on success.
z_save_snapshot_sd_write:
  jsr build_save_filename
  jsr z_save_snapshot_size
  ; Keep size in z_div_q while FAT32 APIs clobber counters.
  sta z_div_q_lo
  stx z_div_q_hi
  ; Remove previous save file if present.
  jsr fat32_openroot
  ldx #<z_save_filename_buf
  ldy #>z_save_filename_buf
  jsr fat32_finddirent
  bcs :+
  jsr fat32_deletefile
:
  ; Allocate clusters for replacement save file.
  lda z_div_q_lo
  sta fat32_bytesremaining
  lda z_div_q_hi
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_allocatefile
  bcc :+
  sec
  rts
:
  ; Create root dir entry with the allocated cluster chain.
  jsr fat32_openroot
  lda #<z_save_filename_buf
  sta fat32_filenamepointer
  lda #>z_save_filename_buf
  sta fat32_filenamepointer+1
  lda z_div_q_lo
  sta fat32_bytesremaining
  lda z_div_q_hi
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_writedirent
  bcc :+
  sec
  rts
:
  ; Write snapshot bytes from save_buffer.
  lda #<save_buffer
  sta fat32_address
  lda #>save_buffer
  sta fat32_address+1
  lda z_div_q_lo
  sta fat32_bytesremaining
  lda z_div_q_hi
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_file_write
  clc
  rts

; Load snapshot bytes from SD save file into save_buffer.
; C clear on success.
z_save_snapshot_sd_read:
  jsr build_save_filename
  jsr fat32_openroot
  ldx #<z_save_filename_buf
  ldy #>z_save_filename_buf
  jsr fat32_finddirent
  bcc :+
  sec
  rts
:
  jsr fat32_opendirent
  ; libfat32 leaves zp_sd_address at readbuffer+$1E0 after open.
  ; Force first byte read to trigger a fresh sector fetch from byte 0.
  lda #<(fat32_readbuffer+$200)
  sta zp_sd_address
  lda #>(fat32_readbuffer+$200)
  sta zp_sd_address+1
  ; Require <= 16-bit size and <= save buffer capacity.
  lda fat32_bytesremaining+2
  ora fat32_bytesremaining+3
  beq :+
  sec
  rts
:
  lda fat32_bytesremaining+1
  cmp #>(save_buffer_end-save_buffer)
  bcc :+
  bne z_save_snapshot_sd_read_fail
  lda fat32_bytesremaining
  beq :+
z_save_snapshot_sd_read_fail:
  sec
  rts
:
  lda fat32_bytesremaining
  sta z_div_q_lo              ; source file byte count
  sta z_work_cnt
  lda fat32_bytesremaining+1
  sta z_div_q_hi
  sta z_work_cnt+1
  ; Need at least fixed header.
  lda z_div_q_hi
  bne :+
  lda z_div_q_lo
  cmp #<(save_data_base-save_buffer)
  bcs :+
  sec
  rts
:
  lda #<save_buffer
  sta fat32_address
  lda #>save_buffer
  sta fat32_address+1
  jsr fat32_file_read
z_save_sd_read_done:
  ; Validate declared snapshot size matches file size.
  jsr z_save_snapshot_size
  txa
  cmp z_div_q_hi
  bne z_save_snapshot_sd_read_fail
  lda z_work_cnt
  cmp z_div_q_lo
  bne z_save_snapshot_sd_read_fail
  clc
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
  lda #' '
  jsr print_char
  sec
  lda z_pc
  sbc #1
  sta z_tmp
  lda z_pc+1
  sbc #0
  sta z_tmp2
  lda z_pc_ext
  sbc #0
  sta z_work_cnt
  lda z_work_cnt
  jsr print_hex
  lda z_tmp2
  jsr print_hex
  lda z_tmp
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
  jsr z_out2_finalize
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

; Store A/X into the variable byte immediately following the current opcode.
; Used by v4+ SAVE/RESTORE and several v5+ store forms.
z_store_ax_following:
  pha
  txa
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  clc
  rts

op_save:
  jsr z_save_snapshot
  ; Preserve the save result before the version check clobbers carry.
  bcc :+
  lda #0
  bne :++
: lda #1
: sta z_tmp
  lda z_story_version
  cmp #4
  bcc z_save_branch
  jmp z_save_store

z_save_branch:
  lda z_tmp
  jmp z_branch_on_bool

z_save_store:
  lda z_tmp
  ldx #0
  jmp z_store_ax_following

op_restore:
  jsr z_restore_snapshot
  ; Save the restore outcome before version dispatch. In V1-3 restore resumes
  ; as though the original SAVE succeeded, so the branch must be taken on
  ; success and fall through on failure.
  bcc :+
  lda #0
  sta z_tmp
  beq :++
: lda z_story_version
  cmp #4
  bcc :+
  lda #2
  bne :++
: lda #1
: sta z_tmp
  lda z_story_version
  cmp #4
  bcc z_restore_branch
  jmp z_restore_store

z_restore_branch:
  lda z_tmp
  jmp z_branch_on_bool

z_restore_store:
  lda z_tmp
  ldx #0
  jmp z_store_ax_following

op_restart:
  jsr z_out2_finalize
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

op_catch:
  jsr z_catch_token_current
  jmp z_store_ax_following

op_new_line:
  jsr z_vm_newline
  clc
  rts

op_piracy:
  ; This build has no piracy detection; always take the "legitimate copy" path.
  lda #1
  jmp z_branch_on_bool

op_show_status:
  ; Console rendering of V3 status info:
  ; room name, then either "Score/Turns" or "Time".
  lda z_out3_active
  pha
  lda #0
  sta z_out3_active
  jsr newline
  ldx #<msg_status_bar
  ldy #>msg_status_bar
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  jsr newline
  ldy #0
  jsr z_status_get_global_word
  jsr z_print_obj_name
  ldx #<msg_status_sep
  ldy #>msg_status_sep
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  lda #1
  ldx #0
  jsr z_mem_read_byte_ax
  bcc :+
  lda #0
:
  and #$02
  bne z_show_status_time
  ldx #<msg_status_score
  ldy #>msg_status_score
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  ldy #1
  jsr z_status_get_global_word
  jsr z_print_num_ax
  ldx #<msg_status_turns
  ldy #>msg_status_turns
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  ldy #2
  jsr z_status_get_global_word
  jsr z_print_num_ax
  jmp z_show_status_done

z_show_status_time:
  ldx #<msg_status_time
  ldy #>msg_status_time
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  ldy #1
  jsr z_status_get_global_word
  sta z_tmp                      ; hour (0..23)
  lda #'A'
  sta z_div_d_lo                 ; AM/PM first char
  lda #'M'
  sta z_div_d_hi                 ; second char
  lda z_tmp
  cmp #12
  bcc z_show_status_time_am
  bne z_show_status_time_pm
  lda #'P'
  sta z_div_d_lo
  jmp z_show_status_time_hour
z_show_status_time_pm:
  lda #'P'
  sta z_div_d_lo
  sec
  lda z_tmp
  sbc #12
  sta z_tmp
  jmp z_show_status_time_hour
z_show_status_time_am:
  lda z_tmp
  bne z_show_status_time_hour
  lda #12
  sta z_tmp
z_show_status_time_hour:
  lda z_tmp
  ldx #0
  jsr z_print_num_ax
  lda #':'
  jsr print_char
  ldy #2
  jsr z_status_get_global_word
  sta z_tmp                      ; minute
  cmp #10
  bcs :+
  lda #'0'
  jsr print_char
:
  lda z_tmp
  ldx #0
  jsr z_print_num_ax
  lda #' '
  jsr print_char
  lda z_div_d_lo
  jsr print_char
  lda z_div_d_hi
  jsr print_char

z_show_status_done:
  jsr newline
  ldx #<msg_status_bar
  ldy #>msg_status_bar
  stx z_print_ptr
  sty z_print_ptr+1
  jsr print_string
  jsr newline
  pla
  sta z_out3_active
  clc
  rts

op_verify:
  ; V1-V3 checksum over bytes [0x40 .. file_length-1], modulo 16 bits.
  lda #$1A
  ldx #$00
  jsr z_mem_read_word_ax
  bcc :+
  lda #0
  jmp z_branch_on_bool
:
  ; Header file length is in 2-byte units for V1-V3.
  stx z_work_cnt+1
  sta z_work_cnt
  asl z_work_cnt
  rol z_work_cnt+1
  lda #0
  adc #0
  sta z_ptr_ext                 ; 24-bit file length high byte
  ; Missing file length => treat as success.
  lda z_work_cnt
  ora z_work_cnt+1
  ora z_ptr_ext
  bne :+
  lda #1
  jmp z_branch_on_bool
:
  ; Expected checksum word at header 0x1C.
  lda #$1C
  ldx #$00
  jsr z_mem_read_word_ax
  bcc :+
  lda #0
  jmp z_branch_on_bool
:
  sta z_div_q_lo                ; expected checksum lo
  stx z_div_q_hi                ; expected checksum hi
  ; If file length <= 0x40, checksum range is empty => sum 0.
  lda z_ptr_ext
  bne z_verify_have_bytes
  lda z_work_cnt+1
  cmp #$00
  bne z_verify_have_bytes
  lda z_work_cnt
  cmp #$41
  bcs z_verify_have_bytes
  lda #0
  sta z_div_r_lo
  sta z_div_r_hi
  jmp z_verify_compare

z_verify_have_bytes:
  ; count = file_len - 0x40
  sec
  lda z_work_cnt
  sbc #$40
  sta z_work_cnt
  lda z_work_cnt+1
  sbc #$00
  sta z_work_cnt+1
  lda z_ptr_ext
  sbc #$00
  sta z_ptr_ext
  ; target = 0x000040
  lda #$40
  sta z_story_target
  lda #0
  sta z_story_target+1
  sta z_story_target+2
  lda #0
  sta z_div_r_lo                ; running checksum lo
  sta z_div_r_hi                ; running checksum hi

z_verify_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  ora z_ptr_ext
  beq z_verify_compare
  jsr story_read_byte_at_target
  bcc :+
  lda #0
  jmp z_branch_on_bool
:
  clc
  adc z_div_r_lo
  sta z_div_r_lo
  lda z_div_r_hi
  adc #0
  sta z_div_r_hi
  jsr target_inc
  lda z_work_cnt
  bne z_verify_dec_lo
  lda z_work_cnt+1
  bne z_verify_dec_mid
  dec z_ptr_ext
z_verify_dec_mid:
  dec z_work_cnt+1
z_verify_dec_lo:
  dec z_work_cnt
  jmp z_verify_loop

z_verify_compare:
  jsr z_seek_stream_to_pc
  lda z_div_r_lo
  cmp z_div_q_lo
  bne :+
  lda z_div_r_hi
  cmp z_div_q_hi
  bne :+
  lda #1
  jmp z_branch_on_bool
:
  lda #0
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
  jsr z_vm_newline
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
  lda #0
  sta z_call_nostore
  jsr zm_fetch_byte
  bcs zm_stop
  sta z_storevar
  lda z_opcount
  bne :+
  ; call with omitted routine (routine 0): store false
  ldy z_storevar
  lda #0
  tax
  jsr z_set_var_word
  clc
  rts
:
  jmp z_call_common

op_call_vs2:
  jsr zm_decode_var_operands_2b
  bcs zm_stop
  lda #0
  sta z_call_nostore
  jsr zm_fetch_byte
  bcs zm_stop
  sta z_storevar
  lda z_opcount
  bne :+
  ; call with omitted routine (routine 0): store false
  ldy z_storevar
  lda #0
  tax
  jsr z_set_var_word
  clc
  rts
:
  jmp z_call_common

op_call_vn:
  jsr zm_decode_var_operands
  bcs zm_stop
  lda z_opcount
  bne :+
  ; call with omitted routine (routine 0): no store side-effect
  clc
  rts
:
  lda #1
  sta z_call_nostore
  jmp z_call_common

op_call_vn2:
  jsr zm_decode_var_operands_2b
  bcs zm_stop
  lda z_opcount
  bne :+
  ; call with omitted routine (routine 0): no store side-effect
  clc
  rts
:
  lda #1
  sta z_call_nostore
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
  bcc :+
  jmp zm_stop
:
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
  sec
  sbc #1
  sta z_work_cnt              ; max chars excluding terminator
  lda #1
  sta z_text_base_off         ; v1-v4 chars start at text+1
  lda z_story_version
  cmp #5
  bcc :+
  lda #2                      ; v5+ chars start at text+2
  sta z_text_base_off
:
  lda #0
  sta z_idx                   ; count
  ; Timed input: v5+ sread op3=time(1/10 s), op4=routine (0=timeout only).
  sta z_timed_tenths          ; default: no timeout
  lda z_story_version
  cmp #5
  bcc z_sread_no_timeout      ; v1-v4: timed input not defined
  lda z_opcount
  cmp #3
  bcc z_sread_no_timeout      ; op3 omitted: no timeout
  lda z_op3_lo                ; op3 = time in 1/10 s units
  beq z_sread_no_timeout      ; 0 = no timeout
  sta z_timed_tenths
  sta z_timed_reset_val       ; keep original for restart after false callback
  lda z_op4_lo
  sta z_timed_rtn_lo          ; op4 = packed routine addr (0 = timeout only)
  lda z_op4_hi
  sta z_timed_rtn_hi
  lda #TIMED_INPUT_COUNT
  sta RIOT_003_TIMER_WR_1024  ; arm the RIOT /1024 timer
z_sread_no_timeout:

z_sread_loop:
  jsr z_timed_get_char
  bne :+
  jmp z_sread_timeout         ; A=0 → timeout fired, callback said abort
:
  cmp #$0D
  beq z_sread_cr
  cmp #$0A
  beq z_sread_loop
  cmp #$08
  beq z_sread_backspace
  cmp #$7F
  beq z_sread_backspace
  sta z_tmp
  lda z_last_input_script
  beq :+
  lda z_tmp
  jsr z_vm_put_char
:
  lda z_idx
  cmp z_work_cnt
  bcs z_sread_loop
  lda z_tmp
  ; normalize uppercase to lowercase
  cmp #'A'
  bcc :+
  cmp #'Z'+1
  bcs :+
  ora #$20
:
  sta z_tmp
  ; write char to text buffer at op1 + base_off + idx
  clc
  lda z_op1_lo
  adc z_text_base_off
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

z_sread_backspace:
  lda z_idx
  beq z_sread_loop
  dec z_idx
  ; GETCH already echoed the BS/DEL key, moving the cursor back one.
  ; Overwrite the character now under the cursor with a space, then
  ; step back so the next typed character lands in the right place.
  lda #' '
  jsr print_char
  lda #$08
  jsr print_char
  jmp z_sread_loop

z_sread_cr:
  ; Ignore stray leading CR (e.g. file-select Enter).
  lda z_idx
  beq z_sread_loop
  jmp z_sread_done

z_sread_done:
  ; Move game output to next line after Enter.
  jsr z_vm_newline
  lda z_story_version
  cmp #5
  bcc z_sread_done_v14
  ; v5+: text[1] = count, chars at text+2.
  clc
  lda z_op1_lo
  adc #1
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_idx
  jsr z_mem_write_byte_ax
  ; Keep a NUL terminator for local convenience.
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy #$00
  jsr z_mem_write_byte_ax
  jmp z_sread_done_text

z_sread_done_v14:
  ; v1-v4: chars at text+1, terminated by NUL.
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_idx
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy #$00
  jsr z_mem_write_byte_ax

z_sread_done_text:
  ; If parse buffer operand is omitted (1-operand sread), stop here.
  lda z_opcount
  cmp #2
  bcs :+
  jmp z_sread_no_parse
:
  lda z_op2_lo
  ora z_op2_hi
  bne :+
  jmp z_sread_no_parse
:
  lda #0
  sta z_branch_cond            ; sread always keeps unknown words
  jsr z_tokenise_parse_buffer
  jmp z_sread_finish_result

z_sread_no_parse:
z_sread_finish_result:
  lda z_story_version
  cmp #5
  bcc :+
  lda #13                      ; Enter terminator for aread/read
  ldx #0
  jmp z_store_ax_following
: clc
  rts

; Timed-input timeout: callback returned true (or no routine, tenths hit 0).
; v5+: write count=0 to text+1, skip parse, store 0 as terminator.
; v1-v4: timed input not spec'd; just return cleanly with no parse.
z_sread_timeout:
  jsr z_vm_newline
  lda z_story_version
  cmp #5
  bcc :+
  ; Store count=0 into text buffer byte 1.
  clc
  lda z_op1_lo
  adc #1
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy #0
  jsr z_mem_write_byte_ax
  lda #0
  ldx #0
  jmp z_store_ax_following    ; store 0 = timeout terminator
: clc
  rts

;--------------------------------------------------------------
; z_timed_get_char -- poll for input, checking RIOT timer when
;   z_timed_tenths > 0.  When tenths expire: invokes callback
;   routine (if any) or signals timeout unconditionally.
;
; Returns: A = received key (nonzero), or A = 0 to abort input.
; Clobbers: A, X, Y and all scratch used by z_timed_invoke_callback.
;--------------------------------------------------------------
z_timed_get_char:
  lda z_timed_tenths
  bne :+
  jmp get_key                 ; no timeout: tail-call blocking read
:
z_tgc_poll:
  lda z_input_stream
  beq z_tgc_poll_kbd
  jsr z_script_read_char
  bcc z_tgc_script_done
  lda #0
  sta z_input_stream
z_tgc_poll_kbd:
  lda #0
  sta z_last_input_script
  jsr get_input               ; emulator: returns 0x00 when no key queued
  and #$7F
  bne z_tgc_done              ; got a key -> return it
  ; No key: check RIOT timer status (bit 7 = underflow, cleared on read).
  lda RIOT_003_TIMER_STATUS
  bpl z_tgc_poll              ; bit 7 clear -> still counting, keep polling
  ; Timer underflowed: consume one tenth.
  dec z_timed_tenths
  bne z_tgc_rearm             ; still tenths left -> re-arm and keep polling
  ; All tenths exhausted: invoke callback or signal unconditional timeout.
  lda z_timed_rtn_lo
  ora z_timed_rtn_hi
  beq z_tgc_timeout           ; no routine -> abort
  jsr z_timed_invoke_callback ; A=0 false / nonzero true
  bne z_tgc_timeout           ; true -> abort sread
  ; Callback returned false: restart the full original timeout.
  lda z_timed_reset_val
  sta z_timed_tenths
z_tgc_rearm:
  lda #TIMED_INPUT_COUNT
  sta RIOT_003_TIMER_WR_1024  ; re-arm timer
  jmp z_tgc_poll
z_tgc_timeout:
  lda #0                      ; signal abort
  rts
z_tgc_script_done:
  lda #1
  sta z_last_input_script
  rts
z_tgc_done:
  rts

;--------------------------------------------------------------
; z_timed_invoke_callback -- call Z-machine routine at
;   z_timed_rtn_lo/hi (packed), run it to completion, return
;   its result in A (0=false, nonzero=true).
;
; Saves and restores all sread-critical ZP state.
; Uses z_sread_callsp_lo/hi to detect when the callback returns.
;--------------------------------------------------------------
z_timed_invoke_callback:
  ; Push sread state onto the native stack (popped in reverse on return).
  lda z_text_base_off
  pha
  lda z_opcount
  pha
  lda z_work_cnt
  pha
  lda z_idx
  pha
  lda z_op1_lo
  pha
  lda z_op1_hi
  pha
  lda z_op2_lo
  pha
  lda z_op2_hi
  pha
  ; Record call stack depth: callback has returned when callsp equals this.
  lda z_callsp
  sta z_sread_callsp_lo
  lda z_callsp+1
  sta z_sread_callsp_hi
  ; Set up operand registers for a 0-arg call; result stored to var 0 (eval stack).
  lda z_timed_rtn_lo
  sta z_op1_lo
  lda z_timed_rtn_hi
  sta z_op1_hi
  lda #0
  sta z_op2_lo
  sta z_op2_hi
  sta z_op3_lo
  sta z_op3_hi
  sta z_op4_lo
  sta z_op4_hi
  lda #1
  sta z_opcount               ; opcount=1: only the routine address operand
  lda #0
  sta z_call_nostore          ; store the return value
  sta z_storevar              ; var 0 = push to eval stack
  jsr z_call_common           ; push call frame, redirect PC to callback
  ; Mini interpreter: run opcodes until callsp drops back to saved level.
z_tic_loop:
  lda z_callsp
  cmp z_sread_callsp_lo
  bne z_tic_step
  lda z_callsp+1
  cmp z_sread_callsp_hi
  beq z_tic_returned          ; callsp restored -> callback has returned
z_tic_step:
  jsr zm_step
  bcc z_tic_loop
  ; Fatal VM error during callback: treat as true (abort input).
  lda #1
  sta z_tmp2
  jmp z_tic_restore
z_tic_returned:
  ; Pop the callback's return value off the Z-machine eval stack.
  jsr z_pop_word              ; A=lo byte of result, X=hi
  sta z_tmp2                  ; save result
z_tic_restore:
  ; Restore sread state from native stack (reverse of push order above).
  pla
  sta z_op2_hi
  pla
  sta z_op2_lo
  pla
  sta z_op1_hi
  pla
  sta z_op1_lo
  pla
  sta z_idx
  pla
  sta z_work_cnt
  pla
  sta z_opcount
  pla
  sta z_text_base_off
  lda z_tmp2
  rts

; Tokenise text buffer op1 into parse buffer op2.
; Inputs:
; - z_idx = input length in chars
; - z_text_base_off = text character base offset
; - z_branch_cond = 0 to keep unknown words as zero dict entries, nonzero to skip them
; Uses current z_dictionary for lookup.
z_tokenise_parse_buffer:
  lda z_op2_lo
  ldx z_op2_hi
  jsr z_mem_read_byte_ax
  sta menu_saved_count
  lda #0
  sta menu_subdir_count
  lda z_idx
  sta menu_attr
  clc
  lda z_op2_lo
  adc #2
  sta z_prop_ptr
  lda z_op2_hi
  adc #0
  sta z_prop_ptr+1
  lda #0
  sta z_tok_start

z_tokenise_scan_next:
  lda menu_subdir_count
  cmp menu_saved_count
  bcc :+
  jmp z_tokenise_finish
:
  lda z_tok_start
  cmp menu_attr
  bcc :+
  jmp z_tokenise_finish
:

z_tokenise_skip_spaces:
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_tok_start
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  cmp #' '
  bne z_tokenise_token_start_found
  inc z_tok_start
  lda z_tok_start
  cmp menu_attr
  bcc z_tokenise_skip_spaces
  jmp z_tokenise_finish

z_tokenise_token_start_found:
  lda #0
  sta z_tok_len
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_tok_start
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  lda z_prop_ptr
  sta menu_ptr
  lda z_prop_ptr+1
  sta menu_ptr+1
  jsr z_dict_is_separator
  lda menu_ptr
  sta z_prop_ptr
  lda menu_ptr+1
  sta z_prop_ptr+1
  bcc z_tokenise_count_token
  lda #1
  sta z_tok_len
  jmp z_tokenise_token_ready

z_tokenise_count_token:
  lda z_tok_start
  clc
  adc z_tok_len
  cmp menu_attr
  bcs z_tokenise_token_ready
  sta z_tmp
  clc
  lda z_op1_lo
  adc z_text_base_off
  adc z_tmp
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  cmp #' '
  beq z_tokenise_token_ready
  lda z_prop_ptr
  sta menu_ptr
  lda z_prop_ptr+1
  sta menu_ptr+1
  jsr z_dict_is_separator
  lda menu_ptr
  sta z_prop_ptr
  lda menu_ptr+1
  sta z_prop_ptr+1
  bcs z_tokenise_token_ready
  inc z_tok_len
  jmp z_tokenise_count_token

z_tokenise_token_ready:
  lda z_prop_ptr
  sta menu_ptr
  lda z_prop_ptr+1
  sta menu_ptr+1
  jsr z_encode_token_key
  jsr z_dict_lookup_token
  lda menu_ptr
  sta z_prop_ptr
  lda menu_ptr+1
  sta z_prop_ptr+1
  lda z_work_ptr
  ora z_work_ptr+1
  bne z_tokenise_write_entry
  lda z_branch_cond
  bne z_tokenise_advance_only

z_tokenise_write_entry:
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  pla
  tay
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  pla
  tay
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_prop_ptr
  ldx z_prop_ptr+1
  ldy z_tok_len
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  lda z_tok_start
  clc
  adc z_text_base_off
  tay
  lda z_prop_ptr
  ldx z_prop_ptr+1
  jsr z_mem_write_byte_ax
  inc z_prop_ptr
  bne :+
  inc z_prop_ptr+1
:
  inc menu_subdir_count

z_tokenise_advance_only:
  lda z_tok_start
  clc
  adc z_tok_len
  sta z_tok_start
  jmp z_tokenise_scan_next

z_tokenise_finish:
  clc
  lda z_op2_lo
  adc #1
  sta z_work_ptr
  lda z_op2_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy menu_subdir_count
  jsr z_mem_write_byte_ax
  clc
  rts

op_tokenise_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda #2
  sta z_text_base_off
  clc
  lda z_op1_lo
  adc #1
  sta z_work_ptr
  lda z_op1_hi
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  sta z_idx
  lda #0
  sta z_branch_cond
  lda z_opcount
  cmp #4
  bcc :+
  lda z_op4_lo
  ora z_op4_hi
  beq :+
  lda #1
  sta z_branch_cond
:
  lda #0
  sta z_story_target+2
  lda z_opcount
  cmp #3
  bcc z_tokenise_go
  lda z_op3_lo
  ora z_op3_hi
  beq z_tokenise_go
  lda z_dictionary
  sta z_story_target
  lda z_dictionary+1
  sta z_story_target+1
  lda #1
  sta z_story_target+2
  lda z_op3_lo
  sta z_dictionary
  lda z_op3_hi
  sta z_dictionary+1
z_tokenise_go:
  jsr z_tokenise_parse_buffer
  lda z_story_target+2
  beq :+
  lda z_story_target
  sta z_dictionary
  lda z_story_target+1
  sta z_dictionary+1
: clc
  rts

op_encode_text_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  clc
  lda z_op1_lo
  adc z_op3_lo
  sta z_op1_lo
  lda z_op1_hi
  adc z_op3_hi
  sta z_op1_hi
  lda #0
  sta z_text_base_off
  sta z_tok_start
  lda z_op2_lo
  sta z_tok_len
  jsr z_encode_token_key
  lda z_op4_lo
  sta z_work_ptr
  lda z_op4_hi
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_enc0
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_enc1
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_enc2
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_enc3
  jsr z_mem_write_byte_ax
  lda z_story_version
  cmp #4
  bcc :+
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_enc4_ram
  jsr z_mem_write_byte_ax
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  lda z_work_ptr
  ldx z_work_ptr+1
  ldy z_enc5_ram
  jsr z_mem_write_byte_ax
: clc
  rts

op_copy_table_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op3_lo
  ora z_op3_hi
  bne :+
  clc
  rts
:
  lda z_op3_lo
  sta z_work_cnt
  lda z_op3_hi
  sta z_work_cnt+1
  lda #0
  sta z_branch_cond            ; 0=forward/zero, 1=backward
  lda z_work_cnt+1
  bpl :+
  lda z_work_cnt
  eor #$FF
  sta z_work_cnt
  lda z_work_cnt+1
  eor #$FF
  sta z_work_cnt+1
  clc
  lda z_work_cnt
  adc #1
  sta z_work_cnt
  lda z_work_cnt+1
  adc #0
  sta z_work_cnt+1
  jmp z_copy_table_forward
:
  lda z_op2_lo
  ora z_op2_hi
  beq z_copy_table_zero
  lda z_op2_hi
  cmp z_op1_hi
  bcc z_copy_table_forward
  bne :+
  lda z_op2_lo
  cmp z_op1_lo
  bcc z_copy_table_forward
: lda z_op1_lo
  clc
  adc z_work_cnt
  sta z_tmp
  lda z_op1_hi
  adc z_work_cnt+1
  sta z_tmp2
  lda z_op2_hi
  cmp z_tmp2
  bcc z_copy_table_backward_set
  bne z_copy_table_forward
  lda z_op2_lo
  cmp z_tmp
  bcc z_copy_table_backward_set
  beq z_copy_table_forward
  bcs z_copy_table_forward
z_copy_table_backward_set:
  lda #1
  sta z_branch_cond
  jmp z_copy_table_backward

z_copy_table_zero:
  lda z_op1_lo
  sta z_story_target
  lda z_op1_hi
  sta z_story_target+1
z_copy_table_zero_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  bne :+
  jmp z_copy_table_done
: 
  lda z_story_target
  ldx z_story_target+1
  ldy #0
  jsr z_mem_write_byte_ax
  inc z_story_target
  bne :+
  inc z_story_target+1
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_copy_table_zero_loop

z_copy_table_forward:
  lda z_op1_lo
  sta z_story_target
  lda z_op1_hi
  sta z_story_target+1
  lda z_op2_lo
  sta z_work_ptr
  lda z_op2_hi
  sta z_work_ptr+1
z_copy_table_forward_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_copy_table_done
  lda z_story_target
  ldx z_story_target+1
  jsr z_mem_read_byte_ax
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_write_byte_ax
  inc z_story_target
  bne :+
  inc z_story_target+1
:
  inc z_work_ptr
  bne :+
  inc z_work_ptr+1
:
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_copy_table_forward_loop

z_copy_table_backward:
  clc
  lda z_op1_lo
  adc z_work_cnt
  sta z_story_target
  lda z_op1_hi
  adc z_work_cnt+1
  sta z_story_target+1
  clc
  lda z_op2_lo
  adc z_work_cnt
  sta z_work_ptr
  lda z_op2_hi
  adc z_work_cnt+1
  sta z_work_ptr+1
z_copy_table_backward_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_copy_table_done
  sec
  lda z_story_target
  sbc #1
  sta z_story_target
  lda z_story_target+1
  sbc #0
  sta z_story_target+1
  sec
  lda z_work_ptr
  sbc #1
  sta z_work_ptr
  lda z_work_ptr+1
  sbc #0
  sta z_work_ptr+1
  lda z_story_target
  ldx z_story_target+1
  jsr z_mem_read_byte_ax
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_write_byte_ax
  lda z_work_cnt
  bne :+
  dec z_work_cnt+1
:
  dec z_work_cnt
  jmp z_copy_table_backward_loop

z_copy_table_done:
  clc
  rts

op_print_table_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op2_lo
  beq :+
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
  lda #1
  sta z_work_cnt+1            ; default height
  lda z_opcount
  cmp #3
  bcc :+
  lda z_op3_lo
  sta z_work_cnt+1
:
  lda #0
  sta z_work_cnt              ; skip
  lda z_opcount
  cmp #4
  bcc :+
  lda z_op4_lo
  sta z_work_cnt
:
z_print_table_row:
  lda z_work_cnt+1
  beq z_print_table_done
  lda #0
  sta z_idx
z_print_table_col:
  lda z_idx
  cmp z_op2_lo
  beq z_print_table_row_done
  lda z_work_ptr
  sta z_story_target
  lda z_work_ptr+1
  sta z_story_target+1
  lda z_idx
  clc
  adc z_story_target
  sta z_story_target
  lda z_story_target+1
  adc #0
  sta z_story_target+1
  lda z_story_target
  ldx z_story_target+1
  jsr z_mem_read_byte_ax
  jsr z_vm_put_char
  inc z_idx
  jmp z_print_table_col
z_print_table_row_done:
  jsr z_vm_newline
  clc
  lda z_work_ptr
  adc z_op2_lo
  adc z_work_cnt
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  dec z_work_cnt+1
  lda #0
  sta z_idx
  jmp z_print_table_row
z_print_table_done:
  clc
  rts

op_ext_log_shift:
  lda z_op2_hi
  bmi z_ext_log_shift_right
  lda z_op2_lo
  sta z_work_cnt
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
z_ext_log_shift_left_loop:
  lda z_work_cnt
  beq z_ext_log_shift_store
  asl z_work_ptr
  rol z_work_ptr+1
  dec z_work_cnt
  jmp z_ext_log_shift_left_loop
z_ext_log_shift_right:
  lda z_op2_lo
  eor #$FF
  clc
  adc #1
  sta z_work_cnt
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
z_ext_log_shift_right_loop:
  lda z_work_cnt
  beq z_ext_log_shift_store
  lsr z_work_ptr+1
  ror z_work_ptr
  dec z_work_cnt
  jmp z_ext_log_shift_right_loop
z_ext_log_shift_store:
  lda z_work_ptr
  ldx z_work_ptr+1
  jmp z_store_ax_following

op_ext_art_shift:
  lda z_op2_hi
  bmi z_ext_art_shift_right
  lda z_op2_lo
  sta z_work_cnt
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
z_ext_art_shift_left_loop:
  lda z_work_cnt
  beq z_ext_art_shift_store
  asl z_work_ptr
  rol z_work_ptr+1
  dec z_work_cnt
  jmp z_ext_art_shift_left_loop
z_ext_art_shift_right:
  lda z_op2_lo
  eor #$FF
  clc
  adc #1
  sta z_work_cnt
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
z_ext_art_shift_right_loop:
  lda z_work_cnt
  beq z_ext_art_shift_store
  lda z_work_ptr+1
  cmp #$80
  ror z_work_ptr+1
  ror z_work_ptr
  dec z_work_cnt
  jmp z_ext_art_shift_right_loop
z_ext_art_shift_store:
  lda z_work_ptr
  ldx z_work_ptr+1
  jmp z_store_ax_following

op_ext_set_font:
  lda #1
  ldx #0
  jmp z_store_ax_following

op_ext_draw_picture:
  clc
  rts

op_ext_picture_data:
  lda #0
  jmp z_branch_on_bool

op_ext_erase_picture:
  clc
  rts

op_ext_set_margins:
  clc
  rts

op_ext_save_undo:
  lda #0
  tax
  jmp z_store_ax_following

op_ext_restore_undo:
  lda #0
  tax
  jmp z_store_ax_following

op_ext_print_unicode:
  lda z_op1_hi
  bne z_ext_print_unicode_q
  lda z_op1_lo
  cmp #13
  beq z_ext_print_unicode_emit
  cmp #32
  bcc z_ext_print_unicode_q
  cmp #127
  bcc z_ext_print_unicode_emit
z_ext_print_unicode_q:
  lda #'?'
z_ext_print_unicode_emit:
  jsr z_vm_put_char
  clc
  rts

op_ext_check_unicode:
  lda #0
  sta z_work_ptr
  sta z_work_ptr+1
  lda z_op1_hi
  bne z_ext_check_unicode_store
  lda z_op1_lo
  cmp #13
  beq z_ext_check_unicode_yes
  cmp #32
  bcc z_ext_check_unicode_store
  cmp #127
  bcs z_ext_check_unicode_store
z_ext_check_unicode_yes:
  lda #3
  sta z_work_ptr
z_ext_check_unicode_store:
  lda z_work_ptr
  ldx z_work_ptr+1
  jmp z_store_ax_following

op_ext_set_true_colour:
  clc
  rts

op_ext_move_window:
  clc
  rts

op_ext_window_size:
  clc
  rts

op_ext_window_style:
  clc
  rts

op_ext_get_wind_prop:
  lda #0
  tax
  jmp z_store_ax_following

op_ext_scroll_window:
  clc
  rts

op_ext_pop_stack:
  lda z_opcount
  beq z_ext_pop_stack_done
  cmp #2
  bcc z_ext_pop_stack_eval
  lda z_op2_lo
  ora z_op2_hi
  bne z_ext_pop_stack_done
z_ext_pop_stack_eval:
  lda z_op1_lo
  ora z_op1_hi
  beq z_ext_pop_stack_done
  sta z_work_cnt
  lda z_op1_hi
  sta z_work_cnt+1
z_ext_pop_stack_loop:
  lda z_work_cnt
  ora z_work_cnt+1
  beq z_ext_pop_stack_done
  jsr z_pop_word
  sec
  lda z_work_cnt
  sbc #1
  sta z_work_cnt
  lda z_work_cnt+1
  sbc #0
  sta z_work_cnt+1
  jmp z_ext_pop_stack_loop
z_ext_pop_stack_done:
  clc
  rts

op_ext_read_mouse:
  lda z_op1_lo
  ora z_op1_hi
  beq z_ext_read_mouse_done
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
  lda #6
  sta z_idx
z_ext_read_mouse_loop:
  lda #0
  ldx #0
  jsr z_mem_write_word_ax
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  dec z_idx
  bne z_ext_read_mouse_loop
z_ext_read_mouse_done:
  clc
  rts

op_ext_mouse_window:
  clc
  rts

op_ext_push_stack:
  lda z_opcount
  cmp #2
  bcc z_ext_push_stack_eval
  lda z_op2_lo
  ora z_op2_hi
  bne z_ext_push_stack_fail
z_ext_push_stack_eval:
  lda z_op1_lo
  ldx z_op1_hi
  jsr z_push_word
  lda #1
  jmp z_branch_on_bool
z_ext_push_stack_fail:
  lda #0
  jmp z_branch_on_bool

op_ext_put_wind_prop:
  clc
  rts

op_ext_print_form:
  clc
  rts

op_ext_make_menu:
  lda #0
  jmp z_branch_on_bool

op_ext_picture_table:
  clc
  rts

op_ext_buffer_screen:
  lda #0
  tax
  jmp z_store_ax_following

op_print_char_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  jsr z_vm_put_char
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
  jsr z_divmod_u16
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
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
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
  lda z_op1_raw
  pha
  jsr z_pop_word
  sta z_work_ptr
  stx z_work_ptr+1
  pla
  tay
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_set_var_word
  clc
  rts

op_split_window_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; Track requested split height; if collapsed, force lower window active.
  lda z_op1_lo
  sta z_window_lines
  bne :+
  lda #0
  sta z_window_active
:
  clc
  rts

op_set_window_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; Only lower/upper windows are valid in V3.
  lda z_op1_lo
  and #$01
  sta z_window_active
  ; Can't stay in upper window with zero split lines.
  lda z_window_lines
  bne :+
  lda #0
  sta z_window_active
:
  clc
  rts

op_erase_window_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; No screen buffer to clear; treat as a supported no-op.
  clc
  rts

op_erase_line_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  clc
  rts

op_set_cursor_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  clc
  rts

op_get_cursor_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  sta z_work_ptr
  lda z_op1_hi
  sta z_work_ptr+1
  lda #1
  ldx #0
  jsr z_mem_write_word_ax
  bcs :+
  clc
  lda z_work_ptr
  adc #1
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  lda #1
  ldx #0
  jsr z_mem_write_word_ax
: clc
  rts

op_set_text_style_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  clc
  rts

op_buffer_mode_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  clc
  rts

op_input_stream_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_lo
  cmp #1
  beq z_input_stream_script
  lda #0
  sta z_input_stream
  sta z_last_input_script
  clc
  rts
z_input_stream_script:
  lda #0
  sta z_script_pos
  sta z_script_pos+1
  lda #1
  sta z_input_stream
  clc
  rts

op_sound_effect_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; No sound hardware support.
  clc
  rts

op_read_char_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; Timed input: op2=time(1/10 s), op3=routine (0=timeout only).
  lda #0
  sta z_timed_tenths          ; default: no timeout
  lda z_opcount
  cmp #2
  bcc z_rdc_no_timeout        ; op2 omitted: no timeout
  lda z_op2_lo                ; op2 = time in 1/10 s units
  beq z_rdc_no_timeout        ; 0 = no timeout
  sta z_timed_tenths
  sta z_timed_reset_val
  lda z_op3_lo
  sta z_timed_rtn_lo          ; op3 = packed routine addr
  lda z_op3_hi
  sta z_timed_rtn_hi
  lda #TIMED_INPUT_COUNT
  sta RIOT_003_TIMER_WR_1024
z_rdc_no_timeout:
z_rdc_poll:
  jsr z_timed_get_char
  beq z_rdc_timeout           ; A=0 → timeout, store 0
  cmp #$0A
  beq z_rdc_poll              ; skip LF
  cmp #$0D
  beq z_read_char_enter       ; Enter → ZSCII 13
  cmp #$08
  beq z_read_char_store       ; BS → ZSCII 8 (GETCH already moved cursor back)
  cmp #$7F
  beq z_read_char_del         ; DEL → ZSCII 8
  lda z_last_input_script
  bne z_read_char_store_keep
  ; Printable char: suppress the GETCH echo with BS+space+BS.
  pha
  lda #$08
  jsr print_char
  lda #' '
  jsr print_char
  lda #$08
  jsr print_char
  pla
z_read_char_store_keep:
z_read_char_store:
  ldx #0
  jmp z_store_ax_following
z_read_char_enter:
  lda #13
  ldx #0
  jmp z_store_ax_following
z_read_char_del:
  lda #8
  ldx #0
  jmp z_store_ax_following
z_rdc_timeout:
  lda #0
  ldx #0
  jmp z_store_ax_following

op_scan_table_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op4_lo
  bne :+
  lda #$82                     ; default: word compare, entry len 2
  sta z_op4_lo
:
  lda z_op4_lo
  and #$7F
  sta z_work_cnt               ; entry length
  lda z_op4_lo
  and #$80
  sta z_work_cnt+1             ; nonzero => word compare
  lda z_op2_lo
  sta z_work_ptr
  lda z_op2_hi
  sta z_work_ptr+1
z_scan_table_loop:
  lda z_op3_lo
  ora z_op3_hi
  beq z_scan_table_not_found
  lda z_work_ptr
  ldx z_work_ptr+1
  jsr z_mem_read_byte_ax
  sta z_tmp
  lda z_work_cnt+1
  beq z_scan_table_compare_byte
  clc
  lda z_work_ptr
  adc #1
  sta z_story_target
  lda z_work_ptr+1
  adc #0
  sta z_story_target+1
  lda z_story_target
  ldx z_story_target+1
  jsr z_mem_read_byte_ax
  cmp z_op1_lo
  bne z_scan_table_next
  lda z_tmp
  cmp z_op1_hi
  bne z_scan_table_next
  jmp z_scan_table_found
z_scan_table_compare_byte:
  lda z_tmp
  cmp z_op1_lo
  beq z_scan_table_found
z_scan_table_next:
  clc
  lda z_work_ptr
  adc z_work_cnt
  sta z_work_ptr
  lda z_work_ptr+1
  adc #0
  sta z_work_ptr+1
  sec
  lda z_op3_lo
  sbc #1
  sta z_op3_lo
  lda z_op3_hi
  sbc #0
  sta z_op3_hi
  jmp z_scan_table_loop
z_scan_table_found:
  lda z_work_ptr
  pha
  lda z_work_ptr+1
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  lda #1
  jmp z_branch_on_bool
z_scan_table_not_found:
  lda #0
  pha
  pha
  jsr zm_fetch_byte
  bcc :+
  pla
  pla
  jmp zm_stop
:
  tay
  pla
  tax
  pla
  jsr z_set_var_word
  lda #0
  jmp z_branch_on_bool

op_not_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_op1_hi
  eor #$FF
  tax
  lda z_op1_lo
  eor #$FF
  jmp z_store_ax_following

op_check_arg_count_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  lda z_fp
  ora z_fp+1
  bne :+
  lda #0
  jmp z_branch_on_bool
:
  lda z_op1_hi
  bne z_check_arg_count_false
  lda z_op1_lo
  beq z_check_arg_count_false
  sta z_tmp
  ldy #8
  lda (z_fp),y
  lsr
  lsr
  lsr
  lsr
  and #$07
  cmp z_tmp
  bcs z_check_arg_count_true
z_check_arg_count_false:
  lda #0
  jmp z_branch_on_bool
z_check_arg_count_true:
  lda #1
  jmp z_branch_on_bool

op_output_stream_var:
  jsr zm_decode_var_operands
  bcc :+
  jmp zm_stop
:
  ; Stream number is signed in op1.
  lda z_op1_hi
  bmi z_output_stream_disable
  lda z_op1_lo
  cmp #2
  bne :+
  lda z_out2_active
  bne z_output_stream_done
  lda #0
  sta z_out2_len_ram
  sta z_out2_len_ram+1
  sta z_out2_trunc_ram
  lda #1
  sta z_out2_active
  jmp z_output_stream_done
:
  cmp #3
  bne :+
  ; Enable stream 3 (memory table at operand 2).
  lda z_opcount
  cmp #2
  bcc :+
  lda z_op2_lo
  sta z_out3_table
  lda z_op2_hi
  sta z_out3_table+1
  clc
  lda z_op2_lo
  adc #2
  sta z_out3_ptr
  lda z_op2_hi
  adc #0
  sta z_out3_ptr+1
  lda #0
  sta z_out3_len
  sta z_out3_len+1
  lda #1
  sta z_out3_active
: z_output_stream_done:
  clc
  rts

z_output_stream_disable:
  lda z_op1_lo
  cmp #$FE                    ; -2
  bne :+
  jsr z_out2_finalize
  clc
  rts
:
  cmp #$FD                    ; -3
  bne :+
  lda z_out3_active
  beq :+
  jsr z_out3_flush_len
  lda #0
  sta z_out3_active
: clc
  rts

; Decode up to 4 operands using a VAR-form type byte.
; Result:
; - z_opcount set
; - z_op1..z_op4 words set
; C set on decode/read failure.
zm_decode_var_operands:
  lda #0
  sta z_opcount
  sta z_op1_raw
  sta z_op2_raw
  jsr zm_fetch_byte
  bcs zm_decode_var_fail
  sta z_tmp

  ; slot 1
  lda z_tmp
  pha               ; save type-shift register (zm_decode_one_typed_operand clobbers z_tmp)
  and #$C0
  cmp #$C0
  beq zm_decode_var_done_pull
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail_pull
  pla
  asl
  asl
  sta z_tmp

  ; slot 2
  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  beq zm_decode_var_done_pull
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail_pull
  pla
  asl
  asl
  sta z_tmp

  ; slot 3
  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  beq zm_decode_var_done_pull
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail_pull
  pla
  asl
  asl
  sta z_tmp

  ; slot 4
  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  beq zm_decode_var_done_pull
  jsr zm_decode_one_typed_operand
  bcs zm_decode_var_fail_pull
  pla               ; discard (done with shift register)

zm_decode_var_done:
  clc
  rts

zm_decode_var_done_pull:
  pla
  jmp zm_decode_var_done

zm_decode_var_fail_pull:
  pla
zm_decode_var_fail:
  sec
  rts

; Decode up to 8 operands using two VAR type bytes (used by *_vs2/*_vn2).
; Only first 4 operands are stored (current VM limit), but all encoded operands
; are consumed to keep PC aligned.
zm_decode_var_operands_2b:
  lda #0
  sta z_opcount
  sta z_op1_raw
  sta z_op2_raw
  jsr zm_fetch_byte
  bcs zm_decode_var_fail
  sta z_tmp
  jsr zm_fetch_byte
  bcs zm_decode_var_fail
  sta z_tmp2

  ; first type byte (4 slots): z_tmp is shift register.
  ; Decode calls clobber z_tmp2, so save it on stack now.
  pha               ; save z_tmp2 across z_tmp slots

  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull2_early
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull2
:
  pla
  asl
  asl
  sta z_tmp

  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull2_early
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull2
:
  pla
  asl
  asl
  sta z_tmp

  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull2_early
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull2
:
  pla
  asl
  asl
  sta z_tmp

  lda z_tmp
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull2_early
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull2
:
  pla               ; discard z_tmp (done with first type byte)
  pla               ; restore z_tmp2 (second type byte)
  sta z_tmp2

  ; second type byte (4 slots): z_tmp2 is shift register.
  ; Decode calls clobber z_tmp2, so save/restore via pha/pla.
  ; On error: branch forward to zm_decode_var_fail_pull_2b (1 pla + fail).
  lda z_tmp2
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull_2b
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull_2b
:
  pla
  asl
  asl
  sta z_tmp2

  lda z_tmp2
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull_2b
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull_2b
:
  pla
  asl
  asl
  sta z_tmp2

  lda z_tmp2
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull_2b
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull_2b
:
  pla
  asl
  asl
  sta z_tmp2

  lda z_tmp2
  pha
  and #$C0
  cmp #$C0
  bne :+
  jmp zm_decode_var_done_pull_2b
:
  jsr zm_decode_one_typed_operand
  bcc :+
  jmp zm_decode_var_fail_pull_2b
:
  pla               ; discard (done with second type byte)

  clc
  rts

zm_decode_var_done_pull2_early:
  pla               ; z_tmp (inner push)
  pla               ; z_tmp2 (outer push)
  clc
  rts

zm_decode_var_done_pull_2b:
  pla               ; z_tmp2 (shift register push)
  clc
  rts

zm_decode_var_fail_pull_2b:
  pla               ; z_tmp2 (shift register push)
  sec
  rts

zm_decode_var_fail_pull2:
  pla               ; z_tmp (inner push)
  pla               ; z_tmp2 (outer push)
  sec
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
  jsr z_record_raw_operand_byte
  sta z_work_ptr
  jmp zm_append_operand_word

zm_decode_type_small_or_var:
  cmp #$40
  bne zm_decode_type_var
  ; small constant
  jsr zm_fetch_byte
  bcs zm_decode_type_fail
  jsr z_record_raw_operand_byte
  sta z_work_ptr
  lda #0
  sta z_work_ptr+1
  jmp zm_append_operand_word

zm_decode_type_var:
  ; variable reference
  jsr zm_fetch_byte
  bcs zm_decode_type_fail
  jsr z_record_raw_operand_byte
  jsr z_get_var_word
  sta z_work_ptr
  stx z_work_ptr+1
  jmp zm_append_operand_word

zm_decode_type_fail:
  sec
  rts

; Record raw 8-bit operand byte for op1/op2 (used by opcodes that need var-id refs).
z_record_raw_operand_byte:
  ldx z_opcount
  cpx #0
  bne :+
  sta z_op1_raw
  rts
:
  cpx #1
  bne :+
  sta z_op2_raw
:
  rts

zm_append_operand_word:
  ldx z_opcount
  cpx #8
  bcc :+
  ; ignore operands beyond 8
  clc
  rts
:
  ; Save operand in call_arg_buf[opcount] so call opcodes can use all args.
  txa
  asl
  tay
  lda z_work_ptr
  sta call_arg_buf,y
  iny
  lda z_work_ptr+1
  sta call_arg_buf,y

  ldx z_opcount
  cpx #0
  bne :+
  lda z_work_ptr
  sta z_op1_lo
  lda z_work_ptr+1
  sta z_op1_hi
  jmp zm_append_operand_inc
:
  cpx #1
  bne :+
  lda z_work_ptr
  sta z_op2_lo
  lda z_work_ptr+1
  sta z_op2_hi
  jmp zm_append_operand_inc
:
  cpx #2
  bne :+
  lda z_work_ptr
  sta z_op3_lo
  lda z_work_ptr+1
  sta z_op3_hi
  jmp zm_append_operand_inc
:
  cpx #3
  bne :+
  lda z_work_ptr
  sta z_op4_lo
  lda z_work_ptr+1
  sta z_op4_hi
  jmp zm_append_operand_inc
:
  ; operands 5..8 are only stored in call_arg_buf.
zm_append_operand_inc:
  inc z_opcount
  clc
  rts

; Fetch one byte from story at z_pc, then increment z_pc.
; Uses RAM for dynamic region (< static_base), SD stream for static.
zm_fetch_byte:
zm_fetch_slow:
  lda z_pc
  ldx z_pc+1
  ldy z_pc_ext
  jsr z_mem_read_byte_axy
  bcs zm_fetch_fail

zm_fetch_inc_pc:
  inc z_pc
  bne :+
  inc z_pc+1
  bne :+
  inc z_pc_ext
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
  lda z_pc_ext
  sta z_zs_ptr_ext
  jsr z_decode_zstring_at_ptr
  bcs :+
  lda z_zs_ptr
  sta z_pc
  lda z_zs_ptr+1
  sta z_pc+1
  lda z_zs_ptr_ext
  sta z_pc_ext
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
  sta z_zs_shift_once

z_decode_word_loop:
  lda z_zs_ptr
  ldx z_zs_ptr+1
  ldy z_zs_ptr_ext
  jsr z_mem_read_byte_axy
  bcs z_decode_fail
  sta z_zs_word_hi
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
  bne :+
  inc z_zs_ptr_ext
:
  lda z_zs_ptr
  ldx z_zs_ptr+1
  ldy z_zs_ptr_ext
  jsr z_mem_read_byte_axy
  bcs z_decode_fail
  sta z_zs_word_lo
  inc z_zs_ptr
  bne :+
  inc z_zs_ptr+1
  bne :+
  inc z_zs_ptr_ext
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
  jsr z_reset_shift_after_emit
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
  lda z_story_version
  cmp #3
  bcc z_process_v12
  lda z_idx
  beq z_emit_space
  cmp #4
  bcs :+
  jmp z_set_abbrev_prefix
:
  bne :+
  jmp z_shift_a1
:
  cmp #5
  bne :+
  jmp z_shift_a2
:

  ; Printable zchar 6..31 from selected alphabet (A0/A1/A2).
  lda z_zs_shift
  bne :+
  jmp z_emit_a0
:
  cmp #1
  bne :+
  jmp z_emit_a1
:
  jmp z_emit_a2

z_process_v12:
  lda z_idx
  beq z_emit_space
  cmp #1
  beq z_v12_char1
  cmp #2
  beq z_v12_shift1
  cmp #3
  beq z_v12_shift2
  cmp #4
  beq z_v12_shiftlock1
  cmp #5
  beq z_v12_shiftlock2
  ; Printable zchar 6..31 from selected alphabet (A0/A1/A2).
  lda z_zs_shift
  bne :+
  jmp z_emit_a0
:
  cmp #1
  bne :+
  jmp z_emit_a1
:
  jmp z_emit_a2

z_v12_char1:
  lda z_story_version
  cmp #2
  bne z_v1_emit_newline
  ; V2: one abbreviation table only (entries 0..31).
  lda #1
  sta z_zs_abbrev
  clc
  rts
z_v1_emit_newline:
  jsr z_vm_newline
  jsr z_reset_shift_after_emit
  clc
  rts

z_v12_shift1:
  lda #1
  jsr z_v12_shift_temp
  clc
  rts

z_v12_shift2:
  lda #2
  jsr z_v12_shift_temp
  clc
  rts

z_v12_shiftlock1:
  lda #1
  jsr z_v12_shift_lock
  clc
  rts

z_v12_shiftlock2:
  lda #2
  jsr z_v12_shift_lock
  clc
  rts

z_emit_space:
  lda #' '
  jsr z_vm_put_char
  jsr z_reset_shift_after_emit
  clc
  rts

z_set_abbrev_prefix:
  lda z_idx
  sta z_zs_abbrev
  lda #0
  sta z_zs_shift
  sta z_zs_shift_once
  clc
  rts

z_shift_a1:
  lda #1
  sta z_zs_shift
  lda #1
  sta z_zs_shift_once
  clc
  rts

z_shift_a2:
  lda #2
  sta z_zs_shift
  lda #1
  sta z_zs_shift_once
  clc
  rts

z_v12_shift_temp:
  sta z_tmp
  lda z_zs_shift
  clc
  adc z_tmp
  cmp #3
  bcc z_v12_shift_temp_store
  sec
  sbc #3
z_v12_shift_temp_store:
  sta z_zs_shift
  lda #1
  sta z_zs_shift_once
  clc
  rts

z_v12_shift_lock:
  sta z_tmp
  lda z_zs_shift
  clc
  adc z_tmp
  cmp #3
  bcc z_v12_shift_lock_store
  sec
  sbc #3
z_v12_shift_lock_store:
  sta z_zs_shift
  lda #0
  sta z_zs_shift_once
  clc
  rts

z_reset_shift_after_emit:
  lda z_story_version
  cmp #3
  bcc z_reset_shift_v12
  lda #0
  sta z_zs_shift
  sta z_zs_shift_once
  rts
z_reset_shift_v12:
  lda z_zs_shift_once
  beq :+
  lda #0
  sta z_zs_shift
  sta z_zs_shift_once
: rts

;------------------------------------------------------------
; Alphabet emitters
;------------------------------------------------------------
z_emit_a0:
  lda z_idx
  sec
  sbc #6
  clc
  adc #'a'
  jsr z_vm_put_char
  jsr z_reset_shift_after_emit
  clc
  rts

z_emit_a1:
  lda z_idx
  sec
  sbc #6
  clc
  adc #'A'
  jsr z_vm_put_char
  jsr z_reset_shift_after_emit
  clc
  rts

z_emit_a2:
  lda z_story_version
  cmp #2
  bcs z_emit_a2_v2plus
  ; V1: zchar 6 is newline, zchars 7..31 use V1 A2 table.
  lda z_idx
  cmp #6
  bne z_emit_a2_v1_table
  jsr z_vm_newline
  jsr z_reset_shift_after_emit
  clc
  rts
z_emit_a2_v1_table:
  sec
  sbc #7
  tax
  lda z_a2_table_v1, x
  jsr z_vm_put_char
  jsr z_reset_shift_after_emit
  clc
  rts

z_emit_a2_v2plus:
  lda z_idx
  sec
  sbc #6
  beq z_begin_escape   ; z-char 6: start 10-bit ZSCII escape
  cmp #1
  bne z_emit_a2_table  ; z-char 7: newline
  jsr z_vm_newline
  jsr z_reset_shift_after_emit
  clc
  rts
z_emit_a2_table:
  ; A = z_idx - 6 (>= 2 for z-chars 8..31); subtract 2 more -> index 0..23
  sec
  sbc #2
  tax
  lda z_a2_table, x
  jsr z_vm_put_char
  jsr z_reset_shift_after_emit
  clc
  rts

z_begin_escape:
  lda #1
  sta z_zs_escape
  lda #0
  sta z_zs_shift
  sta z_zs_shift_once
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
  sta z_story_target
  lda #0
  rol
  sta z_story_target+1
  clc
  lda z_story_target
  adc z_abbrev_table
  sta z_story_target
  lda z_story_target+1
  adc z_abbrev_table+1
  sta z_story_target+1
  lda #0
  sta z_story_target+2

  ; Read packed address word from abbrev table.
  jsr story_read_word_at_target
  bcc :+
  jmp z_decode_fail
:
  sta z_tmp2                 ; save packed hi byte (low is still in X)

  ; Save outer packed word bytes plus current decode pointer/state.
  ; Remaining zchars from the outer word are processed after this call.
  lda z_zs_word_hi
  pha
  lda z_zs_word_lo
  pha
  lda z_zs_ptr
  pha
  lda z_zs_ptr+1
  pha
  lda z_zs_ptr_ext
  pha
  lda z_zs_shift       ; save shift state (abbrev padding leaves z_zs_shift=2)
  pha
  lda z_zs_shift_once
  pha
  lda z_zs_endflag     ; preserve outer-string termination flag across recursion
  pha

  ; packed -> byte address (v3): addr = packed * 2
  txa
  asl
  sta z_zs_ptr
  lda z_tmp2
  rol
  sta z_zs_ptr+1
  lda #0
  adc #0
  sta z_zs_ptr_ext

  jsr z_decode_zstring_at_ptr
  bcs z_expand_restore_fail

z_expand_restore:
  pla                  ; restore outer z_zs_endflag (pushed last)
  sta z_zs_endflag
  pla                  ; restore z_zs_shift_once
  sta z_zs_shift_once
  pla                  ; restore z_zs_shift
  sta z_zs_shift
  pla
  sta z_zs_ptr_ext
  pla
  sta z_zs_ptr+1
  pla
  sta z_zs_ptr
  pla
  sta z_zs_word_lo
  pla
  sta z_zs_word_hi
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
  jmp z_vm_put_char
z_emit_qmark:
  lda #'?'
  jmp z_vm_put_char

; Read story byte from address in A(low), X(high). Returns A=byte, C status.
story_read_byte_ax:
  sta z_story_target
  stx z_story_target+1
  lda #0
  sta z_story_target+2
  jmp story_read_byte_at_target

; VM output character helper (supports stream 3 memory table).
; Input: A=character.
z_vm_put_char:
  ldx z_out3_active
  beq z_vm_put_char_console
  sta z_tmp
  lda z_out3_ptr
  ldx z_out3_ptr+1
  ldy z_tmp
  jsr z_mem_write_byte_ax
  bcs z_vm_put_char_mem_fail
  inc z_out3_ptr
  bne :+
  inc z_out3_ptr+1
:
  inc z_out3_len
  bne :+
  inc z_out3_len+1
:
  clc
  rts
z_vm_put_char_mem_fail:
  clc
  rts
z_vm_put_char_console:
  ldx z_out2_active
  bne :+
  jmp print_char
:
  pha
  jsr print_char
  pla
  jmp z_out2_buffer_char

; VM newline helper honoring stream 3.
z_vm_newline:
  lda z_out3_active
  beq :+
  lda #$0D
  jmp z_vm_put_char
: lda z_out2_active
  bne :+
  jmp newline
: jsr newline
  lda #$0D
  jmp z_out2_buffer_char

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
msg_menu_quit:
  .asciiz " Q ) QUIT TO MONITOR"
msg_select:
  .asciiz "Select file > "
msg_bad_index:
  .asciiz "Invalid menu index"
msg_selected:
  .asciiz "Loading "
msg_ellipsis:
  .asciiz "..."
msg_no_story:
  .asciiz "No story selected"
msg_open_fail:
  .asciiz "Could not open selected story"
msg_boot_ok:
  .asciiz "Boot ok, version/PC: "
msg_boot_fail:
  .asciiz "Story boot failed"
msg_unsupported_version:
  .asciiz "Unsupported story version"
msg_dynamic_too_large:
  .asciiz "Story dynamic memory too large for this build"
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
msg_status_bar:
  .asciiz "----------------------------------------"
msg_status_sep:
  .asciiz " - "
msg_status_score:
  .asciiz "Score: "
msg_status_turns:
  .asciiz " Turns: "
msg_status_time:
  .asciiz "Time: "

; Save filename buffer (11 bytes): built at runtime from story_name by
; build_save_filename.  Lives in call_arg_buf ($0590) which is idle during
; 0OP SAVE/RESTORE execution (no operand decode in flight at that point).
z_save_filename_buf = call_arg_buf

; V1 A2 alphabet for zchars 7..31.
z_a2_table_v1:
  .byte '^'
  .byte '0','1','2','3','4','5','6','7','8','9'
  .byte '.',',','!','?','_','#',$27,$22,'/',$5C,'-',':','(',')'

; V2+ A2 alphabet table for zchars 8..31 (zchar 6 is 10-bit ZSCII escape,
; zchar 7 is newline).
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
  .include "../sdcard6502/src/hwconfig.s"
  .include "../sdcard6502/src/libsd.s"
  .include "../sdcard6502/src/libfat32.s"
  .include "../sdcard6502/src/libio.s"
