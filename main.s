; load PRG at $0801 to load "autostart routine"
.byte $01, $08
* = $0801

; BASIC autostart routine (aka "10 SYS 4096")
.byte $0c, $08, $0a, $00, $9e, $20
.byte $34, $39, $31, $35, $32            ; 49152 = $c000
.byte $00, $00, $00

.dsb $1000 - *  ; pad with zeroes from PC to $1000
* = $1000

music:
    .bin $7e, 0, "demo.sid"

music_init = music
music_play = music + 3

.dsb $c000 - *  ; pad with zeroes from PC to $c000
* = $c000

main:
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


init_screen:
    ldx #$00
    stx $d021     ; set background color
    stx $d020     ; set border color

clear:
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
    bne clear
    rts


line1: .asc "            sid player v0.1              "


init_text:
    ldx #$00
loop_text:
    lda line1, x
    sta $0590, x

    inx
    cpx #40         ; a line of text has 40 chars
    bne loop_text
    rts

read_sid:
    lda $d420 + $b       ; voice #1 control register (mirror)

    ; print in hex
    tax
    and #$0f
    jsr hex2asc
    sta $05e0 + 11
    txa
    lsr
    lsr
    lsr
    lsr
    jsr hex2asc
    sta $05e0 + 10

    rts

; Convert a hex digit ($00-$0F) to ASCII ('0'-'9' or 'A'-'F')
hex2asc: .(
    ora #$30        ; form the basic character code
    cmp #$3a        ; does the result need adjustment?
    bcc done
    adc #$06        ; add 7 (6 and the carry) if needed
done:
    rts
.)

irq:
    dec $d019       ; acknowledge IRQ / clear register for next interrupt
    jsr music_play
    jsr read_sid
    jmp $ea31       ; return to Kernel routine
