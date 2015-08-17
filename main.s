.import __MAIN_CODE_LOAD__

; Convert a hex digit ($00-$0F) to ASCII ('0'-'9' or 'A'-'F')
.macro hex2asc
    .local skip
    ora #$30        ; form the basic character code
    cmp #$3a        ; does the result need adjustment?
    bcc skip
    adc #$06        ; add 7 (6 and the carry) if needed
skip:
.endmacro

; Print byte in hexadecimal at screen address
.macro print_hex_byte addr
    tax
    and #$0f
    hex2asc
    sta addr + 1
    txa
    .repeat 4
      lsr
    .endrepeat
    hex2asc
    sta addr
.endmacro


.segment "DATA"
line1: .asciiz "            sid player  v0.1             "

SID_regs_base:
  ch1_freq:    .word 0
  ch1_pw:      .word 0
  ch1_cr:      .byte 0
  ch1_atk_dec: .byte 0
  ch1_sus_rel: .byte 0

  ch2_freq:    .word 0
  ch2_pw:      .word 0
  ch2_cr:      .byte 0
  ch2_atk_dec: .byte 0
  ch2_sus_rel: .byte 0

  ch3_freq:    .word 0
  ch3_pw:      .word 0
  ch3_cr:      .byte 0
  ch3_atk_dec: .byte 0
  ch3_sus_rel: .byte 0

  filter_cutoff_lo: .byte 0
  filter_cutoff_hi: .byte 0
  filter_cr:        .byte 0

  volume_filter: .byte 0


.segment "MUSIC"
    music_offset_data = $7c
    music_init = $152b
    music_play = $1200

    .incbin "music.dat", music_offset_data + 2


.segment "CODE"
    jmp __MAIN_CODE_LOAD__


.segment "MAIN_CODE"
    sei

    jsr init_screen   ; clear the screen
    jsr init_text     ; write lines of text

    ldy #$7f          ; $7f = %01111111
    sty $dc0d         ; turn off CIAs Timer interrupts ($7f = %01111111)
    sty $dd0d
    lda $dc0d         ; by reading $dc0d and $dd0d we cancel all CIA-IRQs
    lda $dd0d         ; in queue/unprocessed.

    lda #$01          ; set Interrupt Request Mask
    sta $d01a         ; we want IRQ by Rasterbeam (%00000001)

    lda #<irq         ; point IRQ Vector to our custom irq routine
    ldx #>irq
    sta $0314         ; store in $314/$315
    stx $0315

    lda #$00          ; trigger interrupt at row zero
    sta $d012

    lda #%00000011    ; VIC bank #0 ($0000-$3fff)
    sta $dd00

    lda #%00010110    ; Screen RAM #0001 ($0400-$07FF), Char ROM #011 ($1800-$1FFF)
    sta $d018

    lda #$00
    jsr music_init

    cli
    jmp *


irq:
    dec $d019       ; acknowledge IRQ / clear register for next interrupt
    jsr music_play
    jsr read_sid
    jmp $ea31       ; return to Kernel routine


init_screen:
    ldx #$00
    stx $d021     ; set background color
    stx $d020     ; set border color

@loop:
    lda #$20      ; #$20 is the spacebar Screen Code
    sta $0400, x  ; fill four areas with 256 spacebar characters
    sta $0500, x
    sta $0600, x
    sta $06e8, x

    lda #$01      ; set foreground to black in Color RAM
    sta $d800, x
    sta $d900, x
    sta $da00, x
    sta $dae8, x

    inx
    bne @loop
    rts


init_text:
    ldx #$00
@loop:
    lda line1, x
    sta $0590, x

    inx
    cpx #40         ; a line of text has 40 chars
    bne @loop
    rts

read_sid:
    lda ch1_cr
    print_hex_byte $05e0 + 14

    lda ch2_cr
    print_hex_byte $05e0 + 19

    lda ch3_cr
    print_hex_byte $05e0 + 24

    rts


.segment "SID_WRITE"
    .include "gen_sid_write.inc"
