.import __MAIN_CODE_LOAD__

.include "c64.inc"  ; C64 constants

.macpack macros     ; our macros
.macpack cbm        ; for scrcode

; from koala.s
.import init_koala


CHAR_BACKGROUND_COLOR = $0
CHAR_MULTICOLOR_1 = $b
CHAR_MULTICOLOR_2 = $1
CH1_CHAR_COLOR = $5
CH2_CHAR_COLOR = $7
CH3_CHAR_COLOR = $a

DEBUG = 0


.segment "DATA"
line1: scrcode "            sid player  v0.1             "

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
    .include "gen_music.inc"


.segment "SPRITES"
    .incbin "sprites.dat"


.segment "CODE"
    jmp __MAIN_CODE_LOAD__


.segment "MAIN_CODE"
    sei

    lda #$35
    sta $01           ; no basic, no kernel

    lda #%01111111
    sta CIA1_ICR      ; turn off CIAs Timer interrupts
    sta CIA2_ICR
    lda CIA1_ICR      ; by reading $dc0d and $dd0d we cancel all CIA-IRQs
    lda CIA2_ICR      ; in queue/unprocessed.

    lda #$01          ; set Interrupt Request Mask
    sta VIC_IMR       ; we want IRQ by Rasterbeam (%00000001)
    asl $d019

    lda #30
    sta $d012
    lda #<irq_top     ; point IRQ Vector to our custom irq routine
    ldx #>irq_top
    sta $fffe
    stx $ffff

    ; Vic bank 2: $8000-$BFFF
    lda $dd00
    and #$fc
    ora #1
    sta $dd00

    jsr init_screen   ; clear the screen
    jsr init_text     ; write lines of text
    jsr init_sprite   ; enable sprite
    jsr init_koala    ; display PVM logo


    lda #$00
    jsr music_init

    cli

@mainloop:
    lda sync    
    beq @mainloop

    dec sync

.if DEBUG=1
    inc $d020
.endif

    jsr read_sid
    jsr music_play

.if DEBUG=1
    dec $d020
.endif

    jmp @mainloop

irq_top:
    pha             ; saves A, X, Y
    txa
    pha
    tya
    pha

    sei
    ; set koala mode
    lda #%00111011
    sta $d011

    ; screen mem: $0400
    ; bitmap  at $a000
    ; charset: ignore
    lda #%00011000
    sta $d018

    lda #72
    sta $d012

    ldx #<irq_rasterbars
    ldy #>irq_rasterbars
    stx $fffe
    sty $ffff

    asl $d019
    cli

    pla         ; restores A, X, Y
    tay
    pla
    tax
    pla
    rti         ; restores previous PC, status

irq_rasterbars:
    pha             ; saves A, X, Y
    txa
    pha
    tya
    pha

    STABILIZE_RASTER

    sei

    .repeat 37
            nop
    .endrepeat

    .repeat 6
        ; 7 "Good" lines: I must consume 63 cycles
        .repeat 7
            lda colors,x    ; +4
            sta $d020       ; +4
            ;sta $d021      ; +4
            nop
            nop
            inx             ; +2
            .repeat 23
                nop         ; +2 * 23
            .endrepeat
            bit $00         ; +3 = 63 cycles
        .endrepeat

        ; 1 "Bad lines": I must consume 23 cycles
        lda colors,x        ; +4
        sta $d020           ; +4
        ;sta $d021          ; +4
        nop
        nop
        inx                 ; +2
        .repeat 3
            nop             ; +2 * 3
        .endrepeat
    .endrepeat

    lda #150
    sta $d012

    ldx #<irq_bottom
    ldy #>irq_bottom
    stx $fffe
    sty $ffff

    asl $d019
    cli

    pla         ; restores A, X, Y
    tay
    pla
    tax
    pla
    rti         ; restores previous PC, status

irq_bottom:
    pha             ; saves A, X, Y
    txa
    pha
    tya
    pha

    sei
    ; set text mode
    lda #%00011011
    sta $d011

    ; screen mem: $0400
    ; bitmap  at: ignore
    ; charset: $1800
    lda #%00010110
    sta $d018

    lda #40
    sta $d012

    ldx #<irq_top
    ldy #>irq_top
    stx $fffe
    sty $ffff

    asl $d019

    inc sync

    cli

    pla         ; restores A, X, Y
    tay
    pla
    tax
    pla
    rti         ; restores previous PC, status



init_screen:
    ldx #$00
    stx VIC_BG_COLOR0     ; set background color
    stx VIC_BORDERCOLOR   ; set border color
@loop:
    lda #$20      ; #$20 is the spacebar Screen Code
    ; TODO: Use constant SCREEN_RAM + offset
    sta $8400, x  ; fill four areas with 256 spacebar characters
    sta $8500, x
    sta $8600, x
    sta $86e8, x

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
    sta $8400 + 40*24, x

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

    lda #$00          ; sprites start at $0000 (relative to the bank)
    .repeat 3, i
        sta $87f8 + i
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
    print_hex_byte $8400+40*21 + 14

    lda ch2_cr
    print_hex_byte $8400+40*21 + 19

    lda ch3_cr
    print_hex_byte $8400+40*21 + 24

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

.align 64
colors:
    .byte $02,$02
    .byte $00,$00
    .byte $02,$02
    .byte $00,$00,$00,$00,$00,$00
    .byte $02,$02
    .byte $00,$00
    .byte $02,$02
    .byte $00,$00,$00,$00,$00,$00
    .byte $02,$02
    .byte $00,$00
    .byte $02,$02
    .byte $00,$00,$00,$00,$00,$00
    .byte $02,$02
    .byte $00,$00
    .byte $02,$02
    .byte $00,$00,$00,$00,$00
    .byte $00

sync: .byte  $00

