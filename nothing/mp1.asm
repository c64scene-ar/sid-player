;********** PVM player ***********
; 19-08-2015 v0.01: Beat display at logo.
;            v0.02: Raster bars added - Stable raster from Ricardo's code
; xx-09-2015 v0.10: VUMeter added
; 02-09-2015 v0.15: Playtime timer added
; 04-09-2015 v0.16: Code cleanup


	!to "mp1.prg"
	!sl "labels.txt"

	!source "macros.asm"

; ***** variables *****

_charcount	= $FB		;16-bit char counter
_topline	= $05		;closing borders top raster
_bottomline	= $06		;closing borders bottom raster
_framecnt	= $07		;frameskip counter
_flag		= $23		;generic flag
_beat		= $02		;beat color cycle counter

_vucnt		= $10		;VU Meter frame skip counter $10, $11, $12 
_vunote		= $13		;VU Meter note played previous frame (for sustain effect) $13, $14, $15
_vufrm		= $16		;VU Meter current animframe $16, $17, $18
_vustat		= $19		;VU Meter state machine status $19, $1a, $1b (0 = idle, 4 = attack, 3 = decay, 2 = sustain, 1 = release)
_vugate		= $1c		;Gate status for previous frame $1c, $1d, $1e

_tframe		= $30		;playtime frame count
_tsecs		= $31		;playtime seconds
_tmins		= $32		;playtime minutes
_tpos		= $33		;playtime digit screen address $33,$34

; ***** Constants *****
BEATINST = $02 ;Instrument to use for beat effect
VUMETERS = $7f8 ;VU Meters sprite pointers

;**********************

	*= $0801
!zone
start
	!word	$080B,2015
	!raw	$9e,"2061",0,0,0

	lda	#$00
	sta	_flag
	sta _beat

	jsr	initmem

.xx06	bit	_flag
	bpl	.xx06

	lda	#$6b
	sta	$d011		;disable screen


;color fade
	ldx	#$04
.xx07	lda	_framecnt
	bne	.xx07
	lda	#$04
	sta	_framecnt
	lda	_colortable1,x
	sta	$d020
	dex
	bpl	.xx07

;set main irq

	sei

	lda	#$fb		;this is how to tell at which rasterline we want the irq to be triggered
	sta	$d012
	
	inc _flag		;make music wait for us to be ready
	
	lda	#<mainirq	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>mainirq
	sta	$ffff
	cli



;---

	jsr	initscr		;Init screen

	lda	#$1b
	sta	$d011		; enable screen
	
	dec _flag		;signal IRQ we're ready 

;**** Main Loop ****
;Print Playtime
.pp0	lda _tframe
		cmp #$31		;wait for tframe reset
		bne .cc0		;if not go check BEATINST
		lda _tsecs
		ldy #$04
		jsr printbcd	;print seconds
		dey
		lda _tmins
		jsr printbcd	;print minutes
		
	
;Color cycle logo bars when BEATINST is played
.cc0	ldy #$02
.cc2	lda shinst,y
		cmp _tinst,y	;check instrument playing changed
		beq .cc1
		sta _tinst,y
		cmp #BEATINST	;changed, now check that is BEATINST
		bne .cc1
		; trigger color cycle
		lda #$03
		sta _beat
		;
.cc1	dey
		bpl .cc2
		bmi .pp0
		
.end	jmp	.end

;**** Print BCD ****
printbcd:
	tax
	and #$0f
	jsr .pbcd
	txa
	lsr
	lsr
	lsr
	lsr
.pbcd
	ora #$30
	sta (_tpos),y
	dey
	rts

;***** White flash color cyble after initial screen close effect *****
_colortable1:

	!byte	$00,$0b,$0c,$0f,$01

;***** Color cycle for the beat effect *****
_colortable2:
	!byte	$02,$0a,$01


;***** temp inst status *****
_tinst:
	!byte	$00, $00, $00
	
;***** sprite block table *****
_sustainmap:
_spblock:
	!byte	$00,$01,$01,$02,$03,$03,$04,$05,$05,$06,$07,$07,$08,$09,$09,$0a

;***** Attack frame skip *****
_attackframes:
	!byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$0e,$18,$27
;***** Attack sprite frame increase
_attackinc:
	!byte	$0a,$0a,$0a,$0a,$05,$04,$04,$03,$02,$02,$02,$01,$01,$01,$01,$01
;***** Decay/Release frame skip *****
_drframes:
	!byte	$01,$01,$01,$01,$01,$01,$00,$01,$02,$03,$06,$0b,$0e,$2c,$4a,$77
;	!byte	$00,$00,$00,$00,$00,$01,$00,$01,$02,$03,$06,$0b,$0e,$2c,$4a,$77
;***** Decay/Release sprite frame decrease *****
_drinc:
	!byte	$01,$01,$01,$01,$01,$01,$01,$02,$02,$01,$01,$01,$01,$01,$01,$01
;	!byte	$0a,$0a,$05,$03,$02,$03,$01,$02,$02,$01,$01,$01,$01,$01,$01,$01

;********* Init Screen *********
initscr:
	ldx	#$00
	stx	$D021

	;clear screen
	lda	#$20
	ldx	#$00
.ic1	sta	$0400,x
	sta	$0500,x
	sta	$0600,x
	sta	$0700,x
	inx
	bne	.ic1

	;set color ram
	lda	#$03
	ldx	#$00
.ic0
	sta	$D900,x
	sta	$DA00,x
	sta	$DB00,x
	inx
	bne	.ic0

	;set color for logo - 10 rows unrolled
	
		ldx #$27 ;40 columns
.lx1		
		!set lrow = 0
		!do {
			lda map_data+lrow,x
			tay
			lda charset_attrib_data,y
			sta $d800+lrow,x
			!set lrow = lrow + 40
		} while lrow < 400
		dex
		bpl .lx1
		
	;Init sprites
	lda #$d3
	sta	VUMETERS	;set block
	sta	VUMETERS+1
	sta	VUMETERS+2
	
	ldx #$07
	stx $d01c	;set multicolor
	stx $d015	;enable
	ldx #$05	;colors
	stx $d027
	stx $d028
	stx $d029
	ldx #$0d
	stx $d025
	ldx #$0b
	stx	$d026
	lda #$a8	;Y-coordinates
	sta $d001
	sta $d003
	sta $d005
	ldx #$ff	;X-coordinates
	stx $d010
	stx $d017
	stx $d01d	;expand
	lda #$04
	sta $d000
	lda #$17
	sta $d002
	lda #$2A
	sta $d004
	
	;print data fields labels
	;Changed from single routine + init for each string, to dedicated hardcoded routine for each string, shorter and easier to read.

;Name 1x2 charset
	+PRINT1X2 _SNameLabel, $16, 8, 11, $400, $05

;Author 1x1 charset
	+PRINT1X1 _SAuthorLabel, $0f, 1, 17, $400, $0f

;Date 1x1 charset
	+PRINT1X1 _SDateLabel, $11, 1, 19, $400, $0f

;Playtime 1x1 charset
	+PRINT1X1 _SPlayLabel, $0e, 1, 15, $400, $0f
	
;Credits 1x1 charset
	+PRINT1X1 _SChipLabel, $0c, 13, 24, $400, $0b
	+PRINT1X1 _SCodeLabel, $0d, 13, 23, $400, $0b
	;+PRINT1X1 _SCharLabel, $11, 11, 22, $400, $0b
	+PRINT1X1 _SGFXLabel, $0d, 13, 22, $400, $0b
 
;VUMeter label
;	+PRINT1X1 _SVULabel, $04, 31, 18, $400, $0f

	rts

; **** Configure memory, set IRQ routine ****
initmem:
	sei        ;disable maskable IRQs

	lda	#$7f
	sta	$dc0d		;disable timer interrupts which can be generated by the two CIA chips
	sta	$dd0d		;the kernal uses such an interrupt to flash the cursor and scan the keyboard, so we better
		   		;stop it.

	lda	$dc0d		;by reading this two registers we negate any pending CIA irqs.
	lda	$dd0d		;if we don't do this, a pending CIA irq might occur after we finish setting up our irq.
		   		;we don't want that to happen.

	lda #COLR_CHAR_MC1
	sta $d022	;Multi color 1
	lda #COLR_CHAR_MC2
	sta $d023	;Multi color 2
				
	lda	#$01		;this is how to tell the VICII to generate a raster interrupt
	sta	$d01a

	lda	#$33
	sta	_topline

	lda	#$fa		;this is how to tell at which rasterline we want the irq to be triggered
	sta	_bottomline
	sta	$d012

	lda	$D020
	sta	_charcount	;temp

	lda	#$1b		;as there are more than 256 rasterlines, the topmost bit of $d011 serves as
	sta	$d011		;the 8th bit for the rasterline we want our irq to be triggered.
		   		;here we simply set up a character screen, leaving the topmost bit 0.

	lda	#$35		;we turn off the BASIC and KERNAL rom here
	sta	$01		;the cpu now sees RAM everywhere except at $d000-$e000, where still the registers of
		   		;SID/VICII/etc are visible

	lda	#<bottom_irq	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>bottom_irq
	sta	$ffff

; Init VU Meters variables
		ldx #$0c
		lda #$00
.im1	sta _vucnt,x
		dex
		bpl .im1
		sta _tsecs		; Init playtime
		sta _tmins

		lda #$fe
		sta _vugate
		sta _vugate+1
		sta _vugate+2

		lda	#$03
		sta	_framecnt	; Init frame counter
		lda	#$31		; Init playtime frame counter (for 50Hz)
		sta	_tframe
		
		lda #$63
		ldx #$06
		sta _tpos
		stx _tpos+1
		
		lda #$00

		jsr	$1000		; Init music
		cli        ;enable maskable interrupts again

		rts

; ******** IRQ Routine ********

; *main irq, play music (1x speed)
mainirq:
	pha
	txa
	pha
	tya
	pha

	lda #$3d	;set up next raster irq at line $40 (for raster bars)
	sta $d012
	lda	#<irq_rasterbars	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>irq_rasterbars
	sta	$ffff

	lda #%10111101	;chargen at $3000 - matrix at $2c00
	sta $d018
	lda $d016
	ora #%00010000	;set multicolor mode
	sta	$d016
;-----

	dec	_framecnt
	bpl	.ir01
;----
	;**** Beat color cycle ****
		ldy _beat	;check if color cycle is in progress
		beq .ir03
		dey
		sty _beat
		lda _colortable2,y
		sta $d022
		sta	rr1+1	;modify code for raster bars
	
.ir03	
	lda	#$03
	sta	_framecnt
	

	
.ir01
	bit _flag	;Wait until main program flags us to start playing music.
	bpl .ir02

	;inc $d020
	jsr	$1003	;Play music
	;dec $d020
	
	jsr VUpdate	;Update VU Meters
	jsr PTUpdate ;Update Playtime
	
.ir02
;-----
    asl $d019
	pla
	tay
	pla
	tax
	pla
	rti

; *raster bars IRQ - for logo bars
irq_rasterbars:
    pha             ; saves A, X, Y
    txa
    pha
    tya
    pha

    +STABILIZE_RASTER

    sei
	
	jsr tworeds
	ldy #$01
	jsr blacks1	;two blacks - bad line
	jsr tworeds
	ldy #$33
	jsr blacks1 ;six blacks
	jsr tworeds
	ldy #$08
	jsr blacks1	;two blacks
	jsr tworeds
	ldy #$33
	jsr blacks1 ;six blacks
	jsr tworeds
	ldy #$01	;two blacks - bad line
	jsr blacks1
	jsr tworeds
	ldy #$32
	jsr blacks1	;six blacks
	jsr tworeds
	ldy#$08
	jsr blacks1	;two blacks
	jsr tworeds
	
    asl $d019
    cli
	lda #$82	;set up next raster irq at line $82 (just before 10th text row)
	sta $d012
	lda	#<secirq	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>secirq
	sta $ffff
	pla         ; restores A, X, Y
    tay
    pla
    tax
    pla
    rti         ; restores previous PC, status 
	
tworeds:

		ldy #$09	;+2
.rra	dey			;+2
		bne	.rra	;+2 +1
		bit $00		;+3
		
rr1		lda#$02		;+2
		sta$d020	;+4
		

		ldy #$17	;+2
.rrb	dey			;+2
		bne	.rrb	;+2 +1
		bit $00		;+3
		
		lda #$00
		sta $d020

		rts

blacks1:

		;ldy #$01	;+2
.rrc	dey			;+2
		bne	.rrc	;+2 +1
		bit $00		;+3
		rts

		
; *secondary IRQ - switchs from logo charset to 'normal' one
secirq:
	pha
	txa
	pha
	tya
	pha

	lda #$fb	;set up next raster irq at line $fb (just after start of bottom border)
	sta $d012
	lda	#<mainirq	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>mainirq
	sta	$ffff
	
	lda #%00011111	;chargen at $1000 - matrix at $0400
	sta $d018
	lda $d016
	and #%11101111	;disable multicolor mode
	sta	$d016	
	
    asl $d019
	pla
	tay
	pla
	tax
	pla
	rti

; ******* subs IRQ *******

bottom_irq:
	pha
	txa
	pha
	tya
	pha

;-----

	lda	_bottomline
	and	#$07
	cmp	#$03
	bne	.bi01


	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
.bi01	nop
	nop
	nop
	nop
	nop
	nop
	nop
;	nop
;	nop


	lda	#$00
	sta	$d020

	lda	#$7b	;0b
	sta	$d011

	lda	_topline
	sta	$d012

	lda	#<top_irq	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>top_irq
	sta	$ffff

;-----
    asl $d019
	pla
	tay
	pla
	tax
	pla
	rti

top_irq:
	pha
	txa
	pha
	tya
	pha


;-----

	ldx	_topline
	inx
	cpx	_bottomline
	bne	.tpi01

	;center reached

	lda	#$04
	sta	_framecnt
	lda	#$0f		;this is how to tell at which rasterline we want the irq to be triggered
	sta	$d012

	dec	_flag		;flag main routine, we're ready for next part

	lda	#<idle_irq
	sta	$fffe
	lda	#>idle_irq
	bne	.tpi02		;sta	$ffff


.tpi01

	lda	_topline
	and	#$07
	cmp	#$03
	bne	.ti01		;bad line


;	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
;	nop
;	nop
;	nop
;	nop
;	nop
.ti01	nop
	nop
	nop
	nop

	lda	#$1b
	sta	$d011

	lda	_charcount
	sta	$d020

	stx	_topline
	dec	_bottomline

	lda	_bottomline
	sta	$d012


	lda	#<bottom_irq	;this is how we set up
	sta	$fffe		;the address of our interrupt code
	lda	#>bottom_irq
.tpi02	sta	$ffff
;-----
    asl $d019
	pla
	tay
	pla
	tax
	pla
	rti

idle_irq:
	pha
	txa
	pha
	tya
	pha


;-----

	dec	_framecnt

;-----
    asl $d019
	pla
	tay
	pla
	tax
	pla
	rti
;****

;***** VUMeters code
VUpdate:
	;first check if gate status changed
		ldx #$03
.vu1	lda	gate,x
		cmp	_vugate,x
		beq .vu2	;no change
		sta _vugate,x
		bcs .vu3	;change to gate set -> attack
		;change to gate clear -> release
		lda shad,x	;get Attack value
		lsr
		lsr
		lsr
		lsr
		tay
		lda _attackframes,y	;load frameskip counter
		sta	_vucnt,x
		lda #$01
		bne .vu4
.vu3	lda shsr			;ger Release value
		and #$15
		tay
		lda _drframes,y	;load frameskip counter
		sta _vucnt,x
		lda #$04
.vu4	sta _vustat,x
.vu2	dex
		bpl .vu1

	;state machine
		ldx #$02
.vu5	lda _vustat,x
		;sta	$720,x	;<-debug	
		bne	.vu0
		jmp	.vu6		;idle
.vu0	cmp #$04
		bcc	.vu7

		;-----attack
		;check if frameskip reached 0
		lda _vucnt,x
		beq .vu8b	
		jmp .vu8	;no
.vu8b	lda	shnote,x	;get note
		;sta	$6f8,x			;Debug
		sta	_vunote,x	;save for later
		lda	shad,x	;get Attack value
		lsr 
		lsr 
		lsr 
		lsr 
		tay			;Yreg = Attack
		;sta	$748,x	;<-debug	
		lda _attackframes,y	;reload frame skip counter
		sta _vucnt,x
		lda	_attackinc,y	;get by how much we got to change sprite animation
		clc
		adc _vufrm,x		;and add to animation frame
		cmp #$0a
		bcc .vu9
		lda #$0a			;reached full volume
		dec _vustat,x		;go to decay state
		pha
		lda _drframes,y		;set new frameskip
		sta	_vucnt,x
		pla
.vu9	sta _vufrm,x
		jmp .vu6
		
.vu7	cmp #$03
		bcc	.vu10
		
		;-----decay
		;check if frameskip reached 0
		lda _vucnt,x
		beq .vu8a	
		jmp .vu8	;no
.vu8a	lda	shad,x	;get Delay value
		and #$0f
		;sta	$770,x	;<-debug	
		tay			;Yreg = Decay
		lda _drframes,y		;reload frame skip counter
		sta _vucnt,x
		lda	_vufrm,x	;get animation frame
		sec
		sbc _drinc,y	;and subtract appropriate value
		pha
		;sta	_vufrm,x
		lda	shsr,x		;get curret sustain level
		lsr 
		lsr 
		lsr 
		lsr 
		tay
		pla
		clc
		cmp	_sustainmap,y	;map sustain level to animation frame
		;cmp _vufrm,x
		beq .vu11			;reached sustain level
		bcs .vu14			;not yet
		lda _sustainmap,y	;sta	_vufrm,x		;we were below sustain level
.vu11	dec _vustat,x		;go to sustain state
.vu14	sta _vufrm,x
		bne	.vu6
		
.vu10	cmp #$02
		bcc .vu12
		
		;-----sustain
		lda _vucnt,x
		bne .vu8	;no
		lda #$02
		sta _vucnt,x
		lda	shsr,x	;get Sustain value
		lsr 
		lsr 
		lsr 
		lsr 
		;sta	$798,x	;<-debug	
		tay			;Yreg = Sustain
		lda _sustainmap,y	;map sustain level to animation frame
		cmp _vufrm,x
		bcs	.vu15			;sustain level is greater or equal than current animation frame, nothing to do
		sta _vufrm,x		;update frame only if sustain level decreases
.vu15	cmp	#$0a			;animate it a little if note changes while in sustain
		beq .vu6			;if full volume, continue
		lda	shnote,x		;otherwise

		cmp _vunote,x		;check if note changed
		beq	.vu6
		inc	_vufrm,x		;increase frame if so
		sta	_vunote,x
		bpl .vu6
		
.vu12	;-----release
		;check if frameskip reached 0
		lda _vucnt,x
		bne .vu8	;no
		lda	shsr,x	;get Release value
		and #$0f
		;sta	$7c0,x	;<-debug	
		tay			;Yreg = Release
		lda _drframes,y		;reload frame skip counter
		sta _vucnt,x
		lda	_vufrm,x	;get animation frame
		sec
		sbc _drinc,y	;and subtract appropriate value
		sta _vufrm,x
		beq	.vu13		;reached 0
		bpl .vu6		;not yet
		lda #$00
		sta	_vufrm,x		;we were below 0
.vu13	dec _vustat,x		;go to idle state
		beq	.vu6
		
.vu8	dec _vucnt,x
.vu6	lda _vufrm,x
		clc
		adc	#$d3
		sta	VUMETERS,x		;update sprite pointers
		dex
		bmi	.vue
		jmp .vu5
.vue	rts

;**** Update Playtime
PTUpdate:
		dec _tframe
		bpl .pte
		lda #$31	;Reset _tframe
		sta _tframe
		sed
		clc
		lda #$01
		adc _tsecs
		cmp #$60
		bne .ptu0
		clc
		lda #$01
		adc _tmins
		sta _tmins
		lda #$00
.ptu0	sta _tsecs
		cld
.pte
;****
codeend:
		rts

;***** MUSIC *****
	;*=$1000
	!source "uc-nothing.s"

;***** Logo charset *****
	!source "logo2_.asm"
;***** Sprites *****
	*=$34C0
	!bin "sprites2.bin"
;***** Main charset *****
	*=$3800
	!bin "Arlek-05b_7bit_fixed_woz.bin" ;"charset2.bin"

;***** Text *****

_SNameLabel:
		!scrxor  $80,"4516 "
		!scrxor  $80,"nothing is free"
		!scrxor  $80," 0123"
		
_SAuthorLabel:	
		!scr "author:   "
		!scr "uctumi"

_SDateLabel:
		!scr "released: "
		!scr "18/03/16"
		
_SPlayLabel:
		!scr "playtime: 00:00"

_SGFXLabel:
		!scr "gfx:   alakran"

_SCharLabel:
		!scr "charset:  arlequin"

_SCodeLabel:
		!scr "code:  the"
		!8 95
		!scr "woz"

_SChipLabel:
		!scr "chip:    8580"

;_SVULabel:
;		!scrxor $80,"1 2 3"
