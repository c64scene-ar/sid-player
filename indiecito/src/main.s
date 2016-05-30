;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; PVM 2 - Indiecito
; http://pungas.space
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

; exported by the linker
.import __MAINCODE_LOAD__, __SIDMUSIC_LOAD__

; from utils.s
.import clear_screen, clear_color, get_key, read_joy2, detect_pal_paln_ntsc
.import vic_video_type, start_clean, setup_tod

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Constants
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.include "c64.inc"                      ; c64 constants

DEBUG = 0                               ; rasterlines for music if 1

MUSIC_INIT = __SIDMUSIC_LOAD__
MUSIC_PLAY = __SIDMUSIC_LOAD__ + 3

.segment "CODE"
        jmp __MAINCODE_LOAD__

.segment "MAINCODE"
        sei                             ; disable interrupts

        lda #$35                        ; no basic, no kernal
        sta $01

        lda #$00
        sta $d01a                       ; no raster IRQ
        lda #$7f
        sta $dc0d                       ; no timer A and B IRQ
        sta $dd0d

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK timer A interrupt
        lda $dd0d                       ; ACK timer B interrupt

        lda $dd00                       ; Vic bank 0: $0000-$3FFF (default)
        and #$fc
        ora #3
        sta $dd00

        lda #%00011000                  ; no scroll, multi-color,40-cols
        sta $d016

        lda #%00011110                  ; charset at $3800
        sta $d018

        lda #0                          ; no sprites
        sta VIC_SPR_ENA

        lda #0                          ; black for background color
        sta $d020
        sta $d021
        lda #2                          ; multicolor #1
        sta $d022
        lda #14
        sta $d023                       ; multicolor #2

        lda #$00                        ; empty char is scrcode 0. $20 is being used for something else
        jsr clear_screen
        lda #15                         ; light gray to clear the color screen  
        jsr clear_color


        jsr display_pvm_logo
        jsr display_credits
        jsr display_indiecito_logo

        jsr detect_pal_paln_ntsc        ; will disable/enable interrupts.
        sei                             ; interrupts must be disabled again

        jsr setup_tod                   ; must be called AFTER detect_pal_...
        lda #0
        sta $dc0b                       ; Set TOD-Clock to 0 (hours)
        sta $dc0a                       ;- (minutes)
        sta $dc09                       ;- (seconds)
        sta $dc08                       ;- (deciseconds)

        jsr MUSIC_INIT                  ; modifies $dc04 / $dc05!
                                        ; must be called before setting our own timer

        jsr setup_timer_interrupt

        lda $dc0e                       ; must be set right after calling "setup_timer_interrupt"
        ora #$11
        sta $dc0e                       ; start timer interrupt A
        lda #$81
        sta $dc0d                       ; enable timer A interrupts

        lda #<int_nmi                   ; NMI handler
        sta $fffa                       ; to catch unwanted NMIs
        lda #>int_nmi
        sta $fffb

        lda #<int_irq                   ; IRQ handler for timer interrupt
        sta $fffe
        lda #>int_irq
        sta $ffff

        cli                             ; enable interrups again

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; main loop
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
@mainloop:
        jsr update_playtime
        lda sync_timer_irq              ; wait until sync_timer flag is 1
        beq @mainloop

        dec sync_timer_irq              ; reset flag

.if (DEBUG & 1)
        dec $d020
.endif
        jsr MUSIC_PLAY                  ; play music

.if (DEBUG & 1)
        inc $d020
.endif
        lda stop_anims                  ; animate ?
        beq @normal_animations

        jsr animate_tripa0              ; clean tripa
        jmp @mainloop


@normal_animations:
        jsr animate_indiecito
        jsr animate_tripa

        jmp @mainloop


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; update_playtime
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc update_playtime

        lda $dc09                       ; seconds. digit
        tax
        ora #$f0
        sta $0400 + 40 * 15 + 26

        txa                             ; seconds. Ten digit
        lsr
        lsr
        lsr
        lsr
        ora #$f0
        sta $0400 + 40 * 15 + 25

        lda $dc0a                       ; minutes. digit
        tax
        and #%00001111
        ora #$f0
        sta $0400 + 40 * 15 + 23

        rts

@temp: .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_indiecito
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_indiecito

        ldx #0
        ldy #0
@loop:
        lda $189f,y                     ; SidTracker64 gates
        and #%00000001
        cmp @prev_gate,x                ; gate != prev gate?
        beq @cont                       ; if no, end loop

        sta @prev_gate,x                ; save new gate state
        cmp #1                          ; only animate if gate == 1
        bne @cont

        jsr @animate


@cont:  tya
        clc
        adc #7
        tay
        inx
        cpx #3
        bne @loop
        rts

@animate:

        ldy @y_table,x                  ; offset to place indians

        lda @indios_label_pre,x
        clc
        adc @frame_idx,x
        ora #$80
        sta $0400 + 40 * 11,y           ; upper part
        ora #$a0
        sta $0400 + 40 * 12,y           ; bottom part

        ldy @y_table_rev,x

        lda @indios_label_post,x        ; indian
        sec
        sbc @frame_idx,x
        ora #$80
        sta $0400 + 40 * 11 + 33,y      ; upper part
        ora #$a0
        sta $0400 + 40 * 12 + 33,y      ; bottom part

        inc @frame_idx,x
        lda @frame_idx,x
        cmp #2
        bne @end

        lda #0
        sta @frame_idx,x
@end:
        rts

@y_table:       .byte 0,3,6
@y_table_rev:   .byte 6,3,0

@frame_idx:     .byte 0,0,0
@prev_gate:     .byte 0,0,0
@indios_label_pre:
        scrcode "uwy"
@indios_label_post:
        scrcode "vxz"
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_tripa
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_tripa
        lda @delay_hi                   ; big delay before start anim
        cmp #$01
        bne :+

        lda #$00                        ; next jump table
        sta @delay_low
        sta @delay_hi
        inc @jump_table_idx
        lda @jump_table_idx
        cmp #@JUMP_TABLE_MAX
        bne :+

        lda #1                          ; stop all kind of animations
        sta stop_anims                  ; since music is over
        rts


:       inc @delay_low                  ; inc counter
        bne @jump
        inc @delay_hi

@jump:
        ldx @jump_table_idx
        lda @jump_table_hi,x
        pha
        lda @jump_table_lo,x
        pha
        rts

@jump_table_lo:
        .byte <(animate_tripa0-1)
        .byte <(animate_tripa0-1)
        .byte <(animate_tripa0-1)
        .byte <(animate_tripa0-1)
        .byte <(animate_tripa0-1)
        .byte <(animate_tripa0-1)

        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa1-1)
        .byte <(animate_tripa2-1)
        .byte <(animate_tripa2-1)

@jump_table_hi:
        .byte >(animate_tripa0-1)
        .byte >(animate_tripa0-1)
        .byte >(animate_tripa0-1)
        .byte >(animate_tripa0-1)
        .byte >(animate_tripa0-1)
        .byte >(animate_tripa0-1)

        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa1-1)
        .byte >(animate_tripa2-1)
        .byte >(animate_tripa2-1)

@JUMP_TABLE_MAX = * - @jump_table_hi

@jump_table_idx:
        .byte 0

@delay_low:     .byte 0
@delay_hi:      .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_tripa0
;------------------------------------------------------------------------------;
; paint nothing
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_tripa0

        lda #160                        ; char index
        sta $0400 + 40 * 11 + 9         ; upper part
        sta $0400 + 40 * 11 + 30        ; upper part
        sta $0400 + 40 * 12 + 9         ; bottom part
        sta $0400 + 40 * 12 + 30        ; bottom part
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_tripa1
;------------------------------------------------------------------------------;
; animate rotating tripa
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_tripa1

        dec @sync_anim
        bne @end

        lda #$03
        sta @sync_anim

        lda #155                        ; char index
        clc
        adc @frame_idx
        sta $0400 + 40 * 11 + 9         ; upper part
        sta $0400 + 40 * 11 + 30        ; upper part
        ora #$a0
        sta $0400 + 40 * 12 + 9         ; bottom part
        sta $0400 + 40 * 12 + 30        ; bottom part

        inc @frame_idx
        lda @frame_idx
        cmp #4
        bne @end

        lda #0
        sta @frame_idx
@end:
        rts

@sync_anim:     .byte 2
@frame_idx:     .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_tripa2
;------------------------------------------------------------------------------;
; animate inside-out circle
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_tripa2

        dec @sync_anim
        bne @end

        lda #$02
        sta @sync_anim

        lda #162                         ; tile index
        clc
        adc @frame_idx
        sta $0400 + 40 * 11 + 9         ; upper part
        sta $0400 + 40 * 11 + 30        ; upper part
        ora #$e0
        sta $0400 + 40 * 12 + 9         ; bottom part
        sta $0400 + 40 * 12 + 30        ; bottom part

        inc @frame_idx
        lda @frame_idx
        cmp #8
        bne @end

        lda #0
        sta @frame_idx
@end:
        rts

@sync_anim:     .byte 2
@frame_idx:     .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; NMI handler
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
int_nmi:
        rti

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ handler
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
int_irq:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        sei                             ; disables interrupts
;       asl $d019                       ; clears raster interrupt
;       bcs @raster

        lda $dc0d                       ; clears timer A interrupt

        cli                             ; enables interrupts

        inc sync_timer_irq

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; setup_timer_interrupt
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; PAL-B clock cycle:  985248 Hz
; NTSC clock cycle:  1022727 Hz
; PAL-N clock cycle: 1023440 Hz
PAL_35HZ = $6df5                        ; SidTracker sets the timer at $6df5 for PAL
NTSC_35HZ = $7223                       ; PAL_35HZ * 1022727 / 985248
PALN_35HZ = $7238                       ; PAL_35HZ * 1023440 / 985248

.proc setup_timer_interrupt
        lda vic_video_type              ; possible values
                                        ;   $01 --> PAL
                                        ;   $2F --> PAL-N
                                        ;   $28 --> NTSC
                                        ;   $2e --> NTSC-OLD
        cmp #$2f
        beq @paln
        cmp #$28
        beq @ntsc
        cmp #$2e
        beq @oldntsc

        ldx #<PAL_35HZ                  ; default is PAL
        ldy #>PAL_35HZ
        jmp @set_timer

@paln:
        ldx #<PALN_35HZ
        ldy #>PALN_35HZ
        jmp @set_timer

@ntsc:
@oldntsc:
        jsr patch_frequency_table
        ldx #<NTSC_35HZ
        ldy #>NTSC_35HZ

@set_timer:
        stx $dc04                       ; set timer A (low)
        sty $dc05                       ; set timer A (hi)

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; patch_frequency_table
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc patch_frequency_table

SID_FREQ_TABLE_LO = $2118
SID_FREQ_TABLE_HI = $2178

        ldx #0

:       lda freq_table_ntsc_lo,x
        sta SID_FREQ_TABLE_LO,x
        lda freq_table_ntsc_hi,x
        sta SID_FREQ_TABLE_HI,x
        inx
        cpx #FREQ_TABLE_COUNT
        bne :-
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; display_pvm_logo
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc display_pvm_logo
        ldx #$00
:       lda logo_label + $0000,x
        sta $0400      + $0000,x        ; screen chars

        tay
        lda logo_attrib_data,y
        sta $d800,x                     ; colors for the chars

        lda logo_label + (LOGO_COUNT .MOD 256),x
        sta $0400      + (LOGO_COUNT .MOD 256),x        ; screen chars

        tay
        lda logo_attrib_data,y
        sta $d800 + (LOGO_COUNT .MOD 256),x             ; colors for the chars

        inx
        bne :-

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; display_indiecito_logo
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc display_indiecito_logo
        ldx #39
:       lda indiecito_label,x           ; the label "indiecito"
        ora #$80
        sta $0400 + 40 * 11,x           ; upper part
        ora #$a0
        sta $0400 + 40 * 12,x           ; bottom part

        lda #$02                        ; indian have different color
        sta $d800 + 40 * 11,x           ; color for upper part
        sta $d800 + 40 * 12,x           ; color for bottom part

        dex
        bpl :-

        ldx #22
        lda #$01
:       sta $d800 + 40 * 11 + 08,x           ; color for upper part
        sta $d800 + 40 * 12 + 08,x           ; color for bottom part

        dex
        bpl :-

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; display_credits
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc display_credits
        ldx #$00
:       lda credits_label,x
        ora #$c0
        sta $0400 + 40 * 15,x
        lda credits_label + (CREDITS_COUNT .MOD 256),x
        ora #$c0
        sta $0400 + 40 * 15 + (CREDITS_COUNT .MOD 256),x
        inx
        bne :-

        ldx #$00                        ; first 6 lines in some color
        lda #05
:       sta $d800 + 15 * 40,x           ; set color #5
        inx
        cpx #40*6
        bne :-

        ldx #$00                        ; last 3 lines are less important, gray them out
        lda #06
:       sta $d800 + 22 * 40,x           ; set color #6
        inx
        cpx #40*3
        bne :-
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; VARIABLES
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
credits_label:
                ;0123456789|123456789|123456789|123456789|
        scrcode "             playtime: 0:00             "
        scrcode "                                        "
        scrcode "              author: naku              "
        scrcode "                                        "
        scrcode "           released: 24/10/15           "
        scrcode "                                        "
        scrcode "                                        "
        scrcode "              gfx: alakran              "
        scrcode "       charset: arlequin, the",255,"woz       "
        scrcode "               code:  riq               "
CREDITS_COUNT = * - credits_label


indiecito_label:
                ;0123456789|123456789|123456789|123456789|
        scrcode "u  w  y    klmknokpq mn prkst    y  w  u"

; Exported from CharPad 2
; MAP DATA : 1 (40x10) map : total size is 400 ($190) bytes.
logo_label:
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$00
        .byte $00,$00,$00,$00,$00,$04,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06,$06,$06
        .byte $06,$06,$06,$07,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0c,$0f,$10,$11
        .byte $0c,$12,$0d,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
        .byte $13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$14,$15,$16,$17,$18
        .byte $19,$15,$1a,$15,$1b,$1c,$1d,$1e,$15,$1f,$15,$20,$13,$13,$13,$13
        .byte $13,$13,$13,$13,$13,$13,$13,$13,$21,$21,$21,$21,$21,$22,$23,$24
        .byte $25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34
        .byte $35,$36,$37,$38,$39,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21
        .byte $06,$06,$06,$06,$0c,$0c,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44
        .byte $45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$07,$06,$06,$06
        .byte $06,$06,$06,$06,$06,$06,$06,$06,$13,$13,$13,$13,$51,$15,$52,$00
        .byte $00,$00,$00,$00,$53,$54,$00,$00,$55,$56,$57,$58,$59,$5a,$5b,$5c
        .byte $5d,$5e,$5f,$60,$15,$20,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13
        .byte $21,$21,$21,$21,$21,$21,$21,$21,$21,$61,$38,$62,$63,$64,$65,$38
        .byte $66,$67,$68,$69,$6a,$65,$38,$66,$6b,$6c,$6d,$6e,$6f,$70,$71,$72
        .byte $73,$74,$75,$3a,$21,$21,$21,$21,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$76,$77,$78,$00,$00,$00,$79,$7a,$7b,$5a,$00,$00,$00
        .byte $7c,$00,$7d,$7e,$7e,$7f,$80,$81,$82,$83,$84,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$85,$00,$00,$00,$00
        .byte $00,$00,$86,$87,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$88
        .byte $89,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8a,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
LOGO_COUNT = * - logo_label

; Exported from CharPad 2
; CHAR SET ATTRIBUTE DATA : 256 attributes : total size is 256 ($100) bytes.
; nb. Upper nybbles = Material, Lower nybbles = Colour.
logo_attrib_data:
        .byte $00,$0b,$0b,$0b,$0e,$0e,$0d,$0d,$0b,$0b,$0e,$0e,$0d,$0d,$0d,$0e
        .byte $0e,$0e,$0d,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0d,$0e,$0e,$0e,$0e,$0b
        .byte $0d,$0d,$0d,$0d,$0e,$0b,$0d,$0d,$0a,$0d,$0e,$0e,$0e,$0e,$0e,$0d
        .byte $0d,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0d,$0d,$0d,$0e,$0e,$0e,$0b,$0b
        .byte $0b,$0b,$0e,$0e,$0e,$0e,$0e,$0d,$0d,$0b,$0b,$0d,$0b,$0b,$0e,$0e
        .byte $0d,$0d,$0d,$0e,$0e,$0e,$0e,$0e,$0b,$0b,$0d,$0d,$0b,$0b,$0b,$0e
        .byte $0e,$0d,$0b,$0b,$0b,$0d,$0d,$0e,$0e,$0b,$0b,$0b,$0d,$0e,$0e,$0e
        .byte $0e,$0e,$0e,$0e,$0e,$0d,$0b,$0b,$0b,$0e,$0e,$0b,$0d,$0e,$0e,$0e
        .byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b,$0e,$0e,$0b,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01
        .byte $01,$01,$00,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

; Taken from: http://codebase64.org/doku.php?id=base:ntsc_frequency_table
freq_table_ntsc_lo:
        ;      C   C#  D   D#  E   F   F#  G   G#  A   A#  B
        .byte $0c,$1c,$2d,$3f,$52,$66,$7b,$92,$aa,$c3,$de,$fa  ; 1
        .byte $18,$38,$5a,$7e,$a4,$cc,$f7,$24,$54,$86,$bc,$f5  ; 2
        .byte $31,$71,$b5,$fc,$48,$98,$ee,$48,$a9,$0d,$79,$ea  ; 3
        .byte $62,$e2,$6a,$f8,$90,$30,$dc,$90,$52,$1a,$f2,$d4  ; 4
        .byte $c4,$c4,$d4,$f0,$20,$60,$b8,$20,$a4,$34,$e4,$a8  ; 5
        .byte $88,$88,$a8,$e0,$40,$c0,$70,$40,$48,$68,$c8,$50  ; 6
        .byte $10,$10,$50,$c0,$80,$80,$e0,$80,$90,$d0,$90,$a0  ; 7
        .byte $20,$20,$a0,$80,$00,$00,$c0,$00,$20,$a0,$20,$40  ; 8
     
freq_table_ntsc_hi:
        ;      C   C#  D   D#  E   F   F#  G   G#  A   A#  B
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ; 1
        .byte $02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03  ; 2
        .byte $04,$04,$04,$04,$05,$05,$05,$06,$06,$07,$07,$07  ; 3
        .byte $08,$08,$09,$09,$0a,$0b,$0b,$0c,$0d,$0e,$0e,$0f  ; 4
        .byte $10,$11,$12,$13,$15,$16,$17,$19,$1a,$1c,$1d,$1f  ; 5
        .byte $21,$23,$25,$27,$2a,$2c,$2f,$32,$35,$38,$3b,$3f  ; 6
        .byte $43,$47,$4b,$4f,$54,$59,$5e,$64,$6a,$70,$77,$7e  ; 7
        .byte $86,$8e,$96,$9f,$a9,$b3,$bd,$c9,$d5,$e1,$ef,$fd  ; 8
FREQ_TABLE_COUNT = * - freq_table_ntsc_hi

sync_timer_irq:         .byte $00               ; set to 1 by the timer interrupt
stop_anims:             .byte $00               ; when 1, no more animations

.segment "SIDMUSIC"
        .incbin "pvm2-indiecito.sid",$7e

.segment "CHARSET"
        .incbin "pvm2-charset-multicolor.bin"
