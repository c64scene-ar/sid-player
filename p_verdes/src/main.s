;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; PVM - Mongo
; http://pungas.space
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

; exported by the linker
.import __MAINCODE_LOAD__, __SIDMUSIC_LOAD__

; from utils.s
.import clear_screen, clear_color, get_key, read_joy2, detect_pal_paln_ntsc
.import vic_video_type, start_clean, setup_tod
.import music_patch_table_1

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

; Info taken from: https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_info.py
; SID_SHADOW1 = SID_SHADOW_VARS0 + 7
; SID_SHADOW2 = SID_SHADOW_VARS0 + 14
SID_SHADOW0 = $18da
; SID_GATE1 = SID_GATE0 + 7
; SID_GATE2 = SID_GATE0 + 14
SID_GATE0 = $1909
SID_FREQ_TABLE_LO = $25fa
SID_FREQ_TABLE_HI = $265a

PAL_FREQ = $5ab2
NTSC_FREQ = $5f1e                       ; PAL_FREQ * 1022727 / 985248
PALN_FREQ = $5f2f                       ; PAL_FREQ * 1023440 / 985248

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
        lda #11                         ; multicolor #1
        sta $d022
        lda #14
        sta $d023                       ; multicolor #2

        lda #$00                        ; empty char is scrcode 0. $20 is being used for something else
        jsr clear_screen
        lda #15                         ; light gray to clear the color screen
        jsr clear_color


        jsr display_pvm_logo
        jsr display_credits
        jsr display_turro_logo

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

        jsr vumeter_update
        jsr vumeter_draw

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
; vumeter_draw
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc vumeter_draw

        lda vumeter_values              ; vumeter for voice #0
        clc
        adc #147                        ; 147 = vumeter 0
        sta $0400 + 40 * 11 + 0
        sta $0400 + 40 * 11 + 39
        ora #32                         ; lower part
        sta $0400 + 40 * 12 + 0
        sta $0400 + 40 * 12 + 39

        lda vumeter_values + 1          ; vumeter for voice #1
        clc
        adc #147                        ; 147 = vumeter 0
        sta $0400 + 40 * 11 + 3
        sta $0400 + 40 * 11 + 36
        ora #32                         ; lower part
        sta $0400 + 40 * 12 + 3
        sta $0400 + 40 * 12 + 36

        lda vumeter_values + 2          ; vumeter for voice #2
        clc
        adc #147                        ; 147 = vumeter 0
        sta $0400 + 40 * 11 + 6
        sta $0400 + 40 * 11 + 33
        ora #32                         ; lower part
        sta $0400 + 40 * 12 + 6
        sta $0400 + 40 * 12 + 33

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; vumeter_anim
; Code taken from PVM Player by The Woz.
; Adapted for SidTracker shadow variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc vumeter_update

        ;first check if gate status changed
        ldx #$02
@loop:
        ldy _sidvoiceindex,x
        lda SID_GATE0,y                 ; gate shadow var
        and #%00000001
        cmp _vugate,x
        beq @cont                       ; no change

        sta _vugate,x
        bcs @vu3                        ; change to gate set -> attack

                                        ; change to gate clear -> release
        lda SID_SHADOW0+5,y             ; get Attack value
        lsr
        lsr
        lsr
        lsr
        tay
        lda _attackframes,y             ; load frameskip counter
        sta _vucnt,x
        lda #$01
        bne @vu4

@vu3:   lda SID_SHADOW0+6,y             ; get Release value
        and #$0f
        tay
        lda _drframes,y                 ; load frameskip counter
        sta _vucnt,x

        lda #$04

@vu4:   sta _vustat,x

@cont:  dex
        bpl @loop

        ; state machine

        ldx #$02
@vu5:   lda _vustat,x
        ;sta    $720,x                  ; <-debug
        bne @vu0
        jmp @vu6                        ; idle
@vu0:   cmp #$04
        bcc @vu7

        ;-----attack
        ;check if frameskip reached 0
        lda _vucnt,x
        beq @vu8b
        jmp @vu8                        ; no
@vu8b:
        ldy _sidvoiceindex,x
        lda SID_SHADOW0,y               ; get note (low freq)
        ;sta    $6f8,x                  ; Debug
        sta _vunote,x                   ; save for later
        lda SID_SHADOW0+5,y             ; get Attack value
        lsr
        lsr
        lsr
        lsr
        tay                             ; Yreg = Attack
        ;sta    $748,x  ;<-debug
        lda _attackframes,y             ; reload frame skip counter
        sta _vucnt,x
        lda _attackinc,y                ; get by how much we got to change sprite animation
        clc
        adc _vufrm,x                    ; and add to animation frame
        cmp #$0a
        bcc @vu9
        lda #$0a                        ; reached full volume
        dec _vustat,x                   ; go to decay state
        pha
        lda _drframes,y                 ; set new frameskip
        sta _vucnt,x
        pla
@vu9:   sta _vufrm,x
        jmp @vu6

@vu7:   cmp #$03
        bcc @vu10

        ;-----decay
        ;check if frameskip reached 0
        lda _vucnt,x
        beq @vu8a
        jmp @vu8                        ; no
@vu8a:
        ldy _sidvoiceindex,x
        lda SID_SHADOW0+5,y             ; get Delay value
        and #$0f
        ;sta    $770,x  ;<-debug
        tay         ;Yreg = Decay
        lda _drframes,y                 ; reload frame skip counter
        sta _vucnt,x
        lda _vufrm,x                    ; get animation frame
        sec
        sbc _drinc,y                    ; and subtract appropriate value
        pha
        ;sta    _vufrm,x
        ldy _sidvoiceindex,x
        lda SID_SHADOW0+6,y             ; get curret sustain level
        lsr
        lsr
        lsr
        lsr
        tay
        pla
        clc
        cmp _sustainmap,y               ; map sustain level to animation frame
        ;cmp _vufrm,x
        beq @vu11                       ; reached sustain level
        bcs @vu14                       ; not yet
        lda _sustainmap,y               ; sta    _vufrm,x        ;we were below sustain level
@vu11:  dec _vustat,x                   ; go to sustain state
@vu14:  sta _vufrm,x
        bne @vu6

@vu10:  cmp #$02
        bcc @vu12

        ;-----sustain
        lda _vucnt,x
        bne @vu8                        ; no
        lda #$02
        sta _vucnt,x
        ldy _sidvoiceindex,x
        lda SID_SHADOW0+6,x             ; get Sustain value
        lsr
        lsr
        lsr
        lsr
        ;sta    $798,x  ;<-debug
        tay                             ; Yreg = Sustain
        lda _sustainmap,y               ; map sustain level to animation frame
        cmp _vufrm,x
        bcs @vu15                       ; sustain level is greater or equal than current animation frame, nothing to do
        sta _vufrm,x                    ; update frame only if sustain level decreases
@vu15:  cmp #$0a                        ; animate it a little if note changes while in sustain
        beq @vu6                        ; if full volume, continue

        ldy _sidvoiceindex,x
        lda SID_SHADOW0,y               ; otherwise

        cmp _vunote,x                   ; check if note changed
        beq @vu6
        inc _vufrm,x                    ; increase frame if so
        sta _vunote,x
        bpl @vu6

@vu12:  ;-----release
        ;check if frameskip reached 0
        lda _vucnt,x
        bne @vu8                        ; no
        ldy _sidvoiceindex,x
        lda SID_SHADOW0+6,y             ; get Release value
        and #$0f
        ;sta    $7c0,x                  ;<-debug
        tay                             ; Yreg = Release
        lda _drframes,y                 ; reload frame skip counter
        sta _vucnt,x
        lda _vufrm,x                    ; get animation frame
        sec
        sbc _drinc,y                    ; and subtract appropriate value
        sta _vufrm,x
        beq @vu13                       ; reached 0
        bpl @vu6                        ; not yet
        lda #$00
        sta _vufrm,x                    ; we were below 0
@vu13:  dec _vustat,x                   ; go to idle state
        beq @vu6

@vu8:   dec _vucnt,x
@vu6:   lda _vufrm,x
        lsr
        sta vumeter_values,x            ; update value chars
        dex
        bmi @vue
        jmp @vu5
@vue:   rts

_vucnt:         .byte   0, 0, 0   ; VU Meter frame skip counter
_vunote:        .byte   0, 0, 0   ; VU Meter note played previous frame (for sustain effect)
_vufrm:         .byte   0, 0, 0   ; VU Meter current animframe
_vustat:        .byte   0, 0, 0   ; VU Meter state machine status (0 = idle, 4 = attack, 3 = decay, 2 = sustain, 1 = release)
_vugate:        .byte $ff, $ff, $ff   ; Gate status for previous frame

_sidvoiceindex: .byte   00, 07, 14

;***** sprite block table *****
_sustainmap:
        .byte   $00,$01,$01,$02,$03,$03,$04,$05,$05,$06,$07,$07,$08,$09,$09,$0a
;***** Attack frame skip *****
_attackframes:
        ; original: 50hz
;       .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$0e,$18,$27
        ; for turro: 38hz
;        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$02,$03,$0b,$12,$1e
        ; for porro: 28hz
       .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$02,$02,$08,$0d,$16
;***** Attack sprite frame increase
_attackinc:
        .byte   $0a,$0a,$0a,$0a,$05,$04,$04,$03,$02,$02,$02,$01,$01,$01,$01,$01
;***** Decay/Release frame skip *****
_drframes:
        ; original: 50hz
;       .byte   $01,$01,$01,$01,$01,$01,$00,$01,$02,$03,$06,$0b,$0e,$2c,$4a,$77
;       ; for turro: 38hz
;        .byte   $01,$01,$01,$01,$01,$01,$00,$01,$02,$02,$05,$08,$0b,$21,$38,$5a
        ; for porro: 28hz
       .byte   $01,$01,$01,$01,$01,$01,$00,$01,$01,$02,$03,$06,$08,$19,$29,$43
;***** Decay/Release sprite frame decrease *****
_drinc:
        .byte   $01,$01,$01,$01,$01,$01,$01,$02,$02,$01,$01,$01,$01,$01,$01,$01
;       .byte   $0a,$0a,$05,$03,$02,$03,$01,$02,$02,$01,$01,$01,$01,$01,$01,$01

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

        ldx #<PAL_FREQ                  ; default is PAL
        ldy #>PAL_FREQ
        jmp @set_timer

@paln:
        ldx #<PALN_FREQ
        ldy #>PALN_FREQ
        jmp @set_timer

@ntsc:
@oldntsc:
        jsr patch_frequency_table
        ldx #<NTSC_FREQ
        ldy #>NTSC_FREQ

@set_timer:
        stx $dc04                       ; set timer A (low)
        sty $dc05                       ; set timer A (hi)

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; patch_frequency_table
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc patch_frequency_table


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
        sta $0400      + $0000,x                        ; screen chars

        tay
        lda logo_attrib_data,y
        sta $d800,x                                     ; colors for the chars

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
; display_turro_logo
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc display_turro_logo
        ldx #39
:       lda turro_label,x               ; the label "T U R R O"
        ora #$80
        sta $0400 + 40 * 11,x           ; upper part
        ora #$a0
        sta $0400 + 40 * 12,x           ; bottom part

        lda #$08                        ; enable multicolor on vu-meters
        sta $d800 + 40 * 11,x           ; color for upper part
        sta $d800 + 40 * 12,x           ; color for bottom part

        dex
        bpl :-

        ldx #22
        lda #$01                        ; white
:       sta $d800 + 40 * 11 + 08,x      ; color for upper part
        sta $d800 + 40 * 12 + 08,x      ; color for bottom part

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

        ; color for the 8580
        lda #03
        sta $d800 + 40 * 24 + 38
        sta $d800 + 40 * 24 + 39
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; VARIABLES
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
credits_label:
                ;0123456789|123456789|123456789|123456789|
        scrcode "             playtime: 0:00             "
        scrcode "                                        "
        scrcode "        author:  los pat moritas        "
        scrcode "                                        "
        scrcode "           released: 16/06/18           "
        scrcode "                                        "
        scrcode "                                        "
        scrcode "              gfx: alakran              "
        scrcode "       charset: arlequin, the",255,"woz       "
        scrcode "           code: riq, the",255,"woz           "
CREDITS_COUNT = * - credits_label


turro_label:
                ;0123456789|123456789|123456789|123456789|
                ;            panuelos   verdes           ;
        scrcode "u  w  y     pqr",28,"l",29,30,"o   klmnlo    y  w  u"

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
        .byte $00,$0b,$0b,$0b,$0c,$0c,$0d,$0d,$0b,$0b,$0c,$0c,$0d,$0d,$0d,$0c
        .byte $0c,$0c,$0d,$0d,$0d,$0d,$0c,$0c,$0c,$0c,$0d,$0c,$0c,$0c,$0c,$0b
        .byte $0d,$0d,$0d,$0d,$0c,$0b,$0d,$0d,$0a,$0d,$0c,$0c,$0c,$0c,$0c,$0d
        .byte $0d,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0d,$0d,$0d,$0c,$0c,$0c,$0b,$0b
        .byte $0b,$0b,$0c,$0c,$0c,$0c,$0c,$0d,$0d,$0b,$0b,$0d,$0b,$0b,$0c,$0c
        .byte $0d,$0d,$0d,$0c,$0c,$0c,$0c,$0c,$0b,$0b,$0d,$0d,$0b,$0b,$0b,$0c
        .byte $0c,$0d,$0b,$0b,$0b,$0d,$0d,$0c,$0c,$0b,$0b,$0b,$0d,$0c,$0c,$0c
        .byte $0c,$0c,$0c,$0c,$0c,$0d,$0b,$0b,$0b,$0c,$0c,$0b,$0d,$0c,$0c,$0c
        .byte $0c,$0c,$0c,$0c,$0c,$0b,$0b,$0b,$0c,$0c,$0b,$01,$01,$01,$01,$01
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
vumeter_values:         .byte $00, $00, $00     ; one vumeter for each voice

.segment "SIDMUSIC"
        .incbin "pverdes.sid",$7e

.segment "CHARSET"
        .incbin "pvm5-charset-multicolor-charset.bin"
