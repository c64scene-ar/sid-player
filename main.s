.import __MAIN_CODE_LOAD__

.include "c64.inc"  ; C64 constants

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


CHAR_BACKGROUND_COLOR = $0
CHAR_MULTICOLOR_1 = $b
CHAR_MULTICOLOR_2 = $1
CH1_CHAR_COLOR = $5
CH2_CHAR_COLOR = $7
CH3_CHAR_COLOR = $a


.segment "DATA"
line1: .asciiz "            sid player  v0.1             "

.include "freq_pal.s"

SID_sh:
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
    music_init = $1000
    music_play = $1003

    .incbin  "gen_music.dat", music_offset_data + 2
    .include "gen_music.inc"


.segment "SPRITES"
    .incbin "sprites.dat"


.segment "CODE"
    jmp __MAIN_CODE_LOAD__


.segment "MAIN_CODE"
    sei

    jsr init_screen   ; clear the screen
    jsr init_text     ; write lines of text
    jsr init_sprite   ; enable sprite

    lda #%01111111
    sta CIA1_ICR      ; turn off CIAs Timer interrupts
    sta CIA2_ICR
    lda CIA1_ICR      ; by reading $dc0d and $dd0d we cancel all CIA-IRQs
    lda CIA2_ICR      ; in queue/unprocessed.

    lda #$01          ; set Interrupt Request Mask
    sta VIC_IMR       ; we want IRQ by Rasterbeam (%00000001)

    lda #<irq         ; point IRQ Vector to our custom irq routine
    ldx #>irq
    sta IRQVec        ; store in $314/$315
    stx IRQVec+1

    lda #$00          ; trigger interrupt at row zero
    sta VIC_HLINE

    lda #%00000011    ; VIC bank #0 ($0000-$3fff)
    sta CIA2_PRA

    lda #%00010110    ; Screen RAM #0001 ($0400-$07FF), Char ROM #011 ($1800-$1FFF)
    sta VIC_VIDEO_ADR

    lda #$00
    jsr music_init

    cli
    jmp *


irq:
    dec VIC_IRR     ; acknowledge IRQ / clear register for next interrupt
    jsr music_play
    jsr read_sid
    jmp $ea31       ; return to Kernel routine


init_screen:
    ldx #$00
    stx VIC_BG_COLOR0     ; set background color
    stx VIC_BORDERCOLOR   ; set border color
@loop:
    lda #$20      ; #$20 is the spacebar Screen Code
    ; TODO: Use constant SCREEN_RAM + offset
    sta $0400, x  ; fill four areas with 256 spacebar characters
    sta $0500, x
    sta $0600, x
    sta $06e8, x

    lda #$01      ; set foreground to black in Color RAM
    ; TODO: Use constant SCREEN_RAM + offset
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

init_sprite:
    lda #%00000111  ; enable 3 sprites
    sta VIC_SPR_ENA

    lda #%00000111  ; set multicolor mode for sprites
    sta VIC_SPR_MCOLOR

    lda #%00000000  ; all sprites have priority over background
    sta VIC_SPR_BG_PRIO

    ; set shared colors
    lda #CHAR_BACKGROUND_COLOR
    sta VIC_BG_COLOR0
    lda #CHAR_MULTICOLOR_1
    sta VIC_SPR_MCOLOR0
    lda #CHAR_MULTICOLOR_2
    sta VIC_SPR_MCOLOR1

    ; set sprite colors
    lda #CH1_CHAR_COLOR
    sta VIC_SPR0_COLOR
    lda #CH2_CHAR_COLOR
    sta VIC_SPR1_COLOR
    lda #CH3_CHAR_COLOR
    sta VIC_SPR2_COLOR

    lda #$c0
    .repeat 3, i
      sta $07f8 + i
    .endrepeat

    lda #$a8
    sta VIC_SPR0_Y
    sta VIC_SPR1_Y
    sta VIC_SPR2_Y

    lda #$30
    sta VIC_SPR0_X
    lda #$40
    sta VIC_SPR1_X
    lda #$50
    sta VIC_SPR2_X

    rts

read_sid:
    lda ch1_cr
    print_hex_byte $05e0 + 14

    lda ch2_cr
    print_hex_byte $05e0 + 19

    lda ch3_cr
    print_hex_byte $05e0 + 24

    lda ch1_cr
    adc #$60
    sta VIC_SPR0_X

    lda ch2_cr
    adc #$60
    sta VIC_SPR1_X

    lda ch3_cr
    adc #$60
    sta VIC_SPR2_X

    rts
