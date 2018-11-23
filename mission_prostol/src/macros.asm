; ********* Macros *********
; Original by Ricardo Quesada
; ACME port by The_Woz
; **************************

; STABILIZE_RASTER
; Double-IRQ Stable raster routine
; code and comments taken from: http://codebase64.org/doku.php?id=base:stable_raster_routine
;--------------------------------------------------------------------------
!macro STABILIZE_RASTER {
    ; A Raster Compare IRQ is triggered on cycle 0 on the current $d012 line
    ; The MPU needs to finish it's current OP code before starting the Interrupt Handler,
    ; meaning a 0 -> 7 cycles delay depending on OP code.
    ; Then a 7 cycle delay is spent invoking the Interrupt Handler (Push SR/PC to stack++)
    ; Then 13 cycles for storing registers (pha, txa, pha, tya, pha)

    ; prev cycle count: 20~27
    lda #<.irq_stable   ; +2, 2
    ldx #>.irq_stable   ; +2, 4
    sta $fffe       ; +4, 8
    stx $ffff       ; +4, 12
    inc $d012       ; +6, 18
    asl $d019       ; +6, 24
    tsx             ; +2, 26
    cli             ; +2, 28

    !for a, 10 {
        ; Next IRQ will be triggered while executing these nops
        nop         ; +2 * 10, 48.
    }
    ; cycle count: 68~75. New raster already triggered at this point
    
.irq_stable:
    ; cycle count: 7~8 .7 cycles for the interrupt handler + 0~1 cycle Jitter for the NOP
    txs         ; +2, 9~10

    ; 42 cycles
    ldx #$08        ; +2, 11~12
    dex             ; +2 * 8, 27~28
    bne *-1         ; +3 * 7, +2, 50~51
    bit $00         ; +3, 53~54

;   for a 21 {
;       nop         ; 2 * 21
;   }

    lda $d012       ; +4, 57~58
    cmp $d012       ; +4, 61~62
    beq *+2         ; +2/+3, 64
}

!macro PRINT1X1 source, length, col, row, base, color {
		ldx #length	;Use string length - 1 when calling
		ldy #color
.pt0:
		lda source,x
		sta base+(40*row)+col,x	
		tya
		sta $d800+(40*row)+col,x
		dex
		bpl .pt0
}

!macro PRINT1X2 source, length, col, row, base, color {
		ldx #length	;Use string length - 1 when calling
		ldy #color
.pt1:
		lda source,x
		sta base+(40*row)+col,x
		ora #$40
		sta base+(40*(row+1))+col,x
		tya
		sta $d800+(40*row)+col,x
		sta $d828+(40*row)+col,x
		dex
		bpl .pt1
}