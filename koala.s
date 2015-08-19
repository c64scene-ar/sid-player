; koala viewer

; exported by the linker
.import __KOALA_LOAD__

KOALA_BITMAP_DATA = __KOALA_LOAD__
KOALA_CHARMEM_DATA = KOALA_BITMAP_DATA + $1f40
KOALA_COLORMEM_DATA = KOALA_BITMAP_DATA + $2328
KOALA_BACKGROUND_DATA = KOALA_BITMAP_DATA + $2710


.export init_koala
.proc init_koala

    ; multi color
    lda #%00011000
    sta $d016

    ; default is:
    ; screen mem: $0400
    ; bitmap  at $a000
    lda #%00011100
    sta $d018

    lda #$00
    sta $d020
    lda KOALA_BACKGROUND_DATA
    sta $d021


    ; Koala format
    ; bitmap:           $0000 - $1f3f = $1f40 ( 8000) bytes
    ; color %01 - %10:  $1f40 - $2327 = $03e8 ( 1000) bytes
    ; color %11:        $2328 - $270f = $03e8 ( 1000) bytes
    ; color %00:        $2710         =     1 (    1) byte
    ; total:                    $2710 (10001) bytes

    ldx #$00
@loop:
    ; $0400: colors %01, %10
    ; only copy the 1st half of the screen. 2nd half will be text mode
    lda KOALA_CHARMEM_DATA,x
    sta $8400,x
    lda KOALA_CHARMEM_DATA+$0100,x
    sta $8400+$0100,x

    ; $d800: color %11
    ; only copy the 1st half of the screen. 2nd half will be text mode
    lda KOALA_COLORMEM_DATA,x
    sta $d800,x
    lda KOALA_COLORMEM_DATA+$0100,x
    sta $d800+$100,x

    inx
    bne @loop
    rts
.endproc

.segment "KOALA"
     .incbin "pvm.koa",2

