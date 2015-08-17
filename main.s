.import __MAIN_CODE_LOAD__


.segment "DATA"
line1: .asciiz "            sid player v0.1              "

SID_regs_base: .res $20, 0


.segment "MUSIC"
music:
    .incbin "tune.sid.1", $7e

music_init = music
music_play = music + 3


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
    lda SID_regs_base

    ; print in hex
    tax
    and #$0f
    jsr hex2asc
    sta $05e0 + 11
    txa
    .repeat 4
      lsr
    .endrepeat
    jsr hex2asc
    sta $05e0 + 10

    rts


; Convert a hex digit ($00-$0F) to ASCII ('0'-'9' or 'A'-'F')
hex2asc:
    ora #$30        ; form the basic character code
    cmp #$3a        ; does the result need adjustment?
    bcc @done
    adc #$06        ; add 7 (6 and the carry) if needed
@done:
    rts


.segment "SID_WRITE"
    .include "gen_sid_write.inc"
