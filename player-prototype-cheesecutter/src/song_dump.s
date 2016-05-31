;;; ACME dump for uc-yasashisa_ni.ct

;;; ----------------------------------------
;;; CCUTTER 2.x musicplayer by abad
;;; Based on JCH NP 21.G4 by Laxity/VIB
;;; ----------------------------------------
!ifdef ZREG {
} else {
ZREG		= $fb
}
FALSE		= 0
TRUE		= 1
EXPORT= TRUE
MULTISPEED= FALSE
INSNO= 17
CIA_VALUE	= $4cc7				; for multispeed
MULTIPLIER	= 1				; for multispeed
BASEADDRESS= $1000
;;; ----------------------------------------
;;; instr table enums
;;; ----------------------------------------
INS_AD		= 0				;
INS_SR		= 1 * INSNO
INS_HR		= 2 * INSNO			; $x0 = HR type, $0x arp delay count
INS_4		= 3 * INSNO			; HR waveform
INS_FLTP	= 4 * INSNO			; 
INS_PULSP	= 5 * INSNO			; 
INS_7		= 6 * INSNO			; 
INS_ARP		= 7 * INSNO
;;; ----------------------------------------
;;; assembly conditionals
;;; ----------------------------------------
INCLUDE_CMD_SLUP= FALSE
INCLUDE_CMD_SLDOWN= TRUE
INCLUDE_CMD_VIBR= FALSE
INCLUDE_CMD_PORTA= TRUE
INCLUDE_CMD_SET_ADSR= FALSE
INCLUDE_CMD_SET_OFFSET= FALSE
INCLUDE_CMD_SET_LOVIB= TRUE
INCLUDE_CMD_SET_WAVE	= FALSE
INCLUDE_SEQ_SET_PULSE	= TRUE
INCLUDE_SEQ_SET_CHORD= TRUE
INCLUDE_SEQ_SET_ATT= TRUE
INCLUDE_SEQ_SET_DEC= FALSE
INCLUDE_SEQ_SET_SUS= FALSE
INCLUDE_SEQ_SET_REL= TRUE
INCLUDE_SEQ_SET_SPEED= TRUE
INCLUDE_SEQ_SET_VOL= FALSE
INCLUDE_DIRECT_PULSE    = TRUE
INCLUDE_VIBRAFEEL	= TRUE
INCLUDE_BREAKSPEED= TRUE
INCLUDE_CHORD= TRUE
INCLUDE_FILTER= TRUE
USE_MDRIVER		= FALSE
;;; ----------------------------------------
;;; new effect commands
;;; ----------------------------------------
CMD_SLIDE_UP	= $00				
CMD_SLIDE_DOWN	= $01				
CMD_VIBRATO	= $02				
CMD_SET_OFFSET	= $03				
CMD_SET_ADSR	= $04				
CMD_SET_LOVIB	= $05
CMD_SET_WAVE	= $06
CMD_PORTAMENTO	= $07				
CMD_STOP	= $08				;stop portamento/slide
;;; ----------------------------------------
SUPERHIGH = CMD_SET_OFFSET
;;; ----------------------------------------
;;; the following data describes the player features
;;; for the editor
;;; ----------------------------------------
!if EXPORT = FALSE {
*=$e00
features		= *
requestedTables	!8 %00001111
;;; 1 = points to wave table
;;; 2 = points to cmd table (NOT IMPLEMENTED)
;;; 3 = points to pulse table
;;; 4 = points to filter table
;;; ...
instrumentFlags	!8 0,0,0,0,4,3,0,1
;;; 1 = points to wave table
;;; 3 = points to pulse table
;;; 4 = points to filter table
cmdFlags	!8 0,0,0,0,0,0,0,0
		!8 0,0,0,0,0,0,0,0
instrumentDescriptionsHeader
        !16 idescr0,idescr1,idescr2,idescr3,idescr4,idescr5,idescr6,idescr7
pulseDescriptionsHeader
	!16 pdescr0,pdescr1,pdescr2,pdescr3
filterDescriptionsHeader
	!16 fdescr0,fdescr1,fdescr2,fdescr3
waveDescriptionsHeader
	!16 wdescr0,wdescr1
cmdDescriptionsHeader
	!16 mdescr0,mdescr1
}
;;; ----------------------------------------
;;; some pointers for the editor
;;; ----------------------------------------
!if EXPORT = FALSE {
		 *= $0fa0
ofa0		!16 features
ofa2		!16 volume
ofa4		!16 editorflag
ofa6		!16 songsets
ofa8		!16 playspeed
ofaa		!16 subnoteplay
ofac		!16 submplayplay
ofae		!16 instrumentDescriptionsHeader
ofb0		!16 pulseDescriptionsHeader
ofb2		!16 filterDescriptionsHeader
ofb4		!16 waveDescriptionsHeader
ofb6		!16 cmdDescriptionsHeader
ofb8		!16 dummy
ofba		!16 dummy
ofbc		!16 arp1	
ofbe		!16 arp2	
ofc0		!16 filttab	
ofc2		!16 pulstab	
ofc4		!16 inst	
ofc6		!16 track1	
ofc8		!16 track2	
ofca		!16 track3	
ofcc		!16 seqlo	
ofce		!16 seqhi	
ofd0		!16 cmd1    
ofd2		!16 s0	 
ofd4		!16 speed	 
ofd6		!16 tracklo
ofd8		!16 voice	 ;voice 1c X, for voice on/of flagging
ofda		!16 gate
ofdc		!16 chord
ofde		!16 trans
ofe0		!16 chordindex
ofe2		!16 shtrans
ofe6		!16 dummy
ofe8		!16 dummy
ofea		!16 dummy
ofec		!16 dummy
ofee		!16 newseq
version		!pet "cc4.04"
}
;;; ----------------------------------------
;;; editor specific player routines, will be 
;;; left out from a finalized tune
;;; ----------------------------------------
!if EXPORT = FALSE {
*=$f000
idescr0 !raw "Attack / Decay.",0
idescr1 !raw "Sustain / Release.",0
idescr2 !raw "Restart type / arpeggio speed.&$00 = 3 Frame Restart.&$40 = Soft restart.&$80 = Hard Restart. &$00-$0F = Arpeggio delay value.",0
idescr3 !raw "Hard Restart waveform.",0
idescr4 !raw "Filter Table pointer.",0 
idescr5 !raw "Pulse Table pointer $00-$3f.",0
idescr6 !raw "Hard restart SR envelope value.",0
idescr7 !raw "Wave Table pointer.",0
pdescr0	!raw "Duration and direction.&$00-$7F = Add n frames.&$80-$FF = Subtract n frames.",0
pdescr1	!raw "Add value.",0
pdescr2	!raw "Initial pulse value.&Note: Nibbles are reversed! $48 = $8400",0
pdescr3	!raw "Pointer to next set ($00-$3F) or $7F = stop pulse program.",0
fdescr0	!raw "Duration or filter type.&$00-$7f = Duration or $90-$F0 select filter type.",0
fdescr1	!raw "Add value or filter resonance and channel mask.",0
fdescr2	!raw "Initial filter value or $FF = skip.",0
fdescr3	!raw "Pointer to next set ($00-$3F) or $7F = stop filter program.",0
;;; wave table help
wdescr0 !raw "Transpose value / Loop.&$00-$5F = Relative transpose up,&"
	!raw "$80-$DF = Absolute tuning (unaffected by note/transpose value).&"
	!raw "$7E = loop to previous row, $7F = loop to row",0
wdescr1 !raw "Waveform / Wave delay / Loop pointer&"
	!raw "$00 = Do nothing.&"
	!raw "$01 - $0F = Override instrument's Wave Delay value for this row.&"
	!raw "$10 - $DF = Waveform; SID Control Register value.&"
	!raw "$E0 - $EF = SID Control Register value $00 - $0F.&"
	!raw "$00 - $FF = Loop pointer, if Byte 1 = $7F.",0
;;; Command table help
mdescr0 !raw "Command number.&"
	!raw "$0 = Slide up.&"
	!raw "$1 = Slide down.&"
	!raw "$2 = Hi-fi Vibrato.&"
	!raw "$3 = Detune current note.&"
	!raw "$4 = Set ADSR for the current note.&"
	!raw "$5 = Lo-fi vibrato.&"
	!raw "$6 = Set wave.&"
	!raw "$7 = Portamento a tie note.&"
	!raw "$8 = Stop portamento",0
mdescr1 !raw "Parameter values.&"
	!raw "Slide up/down: Slide speed (signed 16-bit).&"
	!raw "Vibrato: 1st byte, lonibble: vibrato 'feel'&"
	!raw "    2nd byte, hinibble: Speed.&"
	!raw "              lonibble: Depth divider (bigger value = narrower vibrato).&"
	!raw "Detune: (signed 16-bit).&"
	!raw "ADSR: (self-explanatory).&"
	!raw "Lo-fi vibrato: Speed / Depth."
	!raw "&Set waveform (in the last parameter byte).&"
;;; 	!raw "Portamento: (Portamento must be applied to tie notes only and reset with e.g. No command 8-00 00).",0	
	!raw "Portamento: Portamento speed. Runs until a command 8-00 00 is given).",0	
*=$f800
submplayplay	lda #$80
		sta state
		ldx #2
		jmp updsound
;;; ----------------------------------------
;;; used for triggering a note from th editor
;;; ----------------------------------------
subnoteplay	
		sta shnote,x
		cmp #3
		lda #0
		rol
		eor #1
		sta tienote,x
		tya
		sta shinst,x
		lda #3
		sta tsync,x
		lda #0
		sta synccnt,x
		lda #1
		sta newinsflag,x
		lda #0
		sta effstate,x
		sta chordvalue,x
		lda #$80
		sta state
		sta chordtpos,x
		jmp updsound
}
		 *= BASEADDRESS
init		
!if USE_MDRIVER = TRUE {
		jmp cinit		
} else {
		jmp subinit
}
play		
!if USE_MDRIVER = TRUE {
		jmp cplay
} else {
		jmp subplay
}
!if MULTISPEED = TRUE {
mplay		jmp submplay
}
;---------------------------------------
subinit		asl
		asl
		asl
		tay
		ldx #0
!if INCLUDE_BREAKSPEED = TRUE {
		stx speedsub
}
subinit0	lda songsets,y			
		sta twraplo,x
		iny
		lda songsets,y
		sta twraphi,x			
		iny
		inx
		cpx #3
		bne subinit0
		lda songsets,y			;set song speed
		sta speed
!if EXPORT = FALSE {
		cmp #2
		bcs subinit00
		sty inittemp
		tay
		lda chord,y
		ldy inittemp
subinit00	sta playspeed
		lda editorflag	
		beq subinit3	
}
		ldx #2
subinit1	lda songsets+1,y
		and bits,x
		sta voicon,x
		lda twraplo,x	; trackpointers not set from the editor
		sta tracklo,x
		lda twraphi,x
		sta trackhi,x
		lda #1		; newseq not set from the editor
		sta newseq,x
		dex				
		bpl subinit1
subinit3	lda #1
		sta state
		rts
;;; ----------------------------------------
;;; do only sound work, used for multispeed
;;; playback frames
;;; ----------------------------------------
!if MULTISPEED = TRUE {
submplay	lda #$40
		sta state
		ldx #2
		jmp syncskip
}
;;; ----------------------------------------
;;; regular play call
;;; ----------------------------------------
subplay
		lda state
		beq run
		lda #2
		sta speedcnt
		ldx #(clrlast-clrfirst)
		lda #0
subinit4	dex
		sta clrfirst,x
		bne subinit4
		;; reset synchonization
		ldx #2
subinit5	lda #2
		sta synccnt,x			; HR allowed
		lda #$fe
		sta tsync,x 			; sync done
		dex
		bpl subinit5
		lda #$0f
		sta volume
!if INCLUDE_FILTER = TRUE {
		lda #0
		sta filter
		sta bandpass
}
		lda #$f0
		sta $d417
		lda #0
		sta state
		rts
run		dec speedcnt
		bpl speeddone
		lda speed
!if INCLUDE_BREAKSPEED = TRUE {
		cmp #2
		bpl speedok
speedalt	ldy speedsub
		lda chord,y
		sta playspeed
		cmp #2
		bpl speedok
		lda #2
}
speedok		sta speedcnt
!if INCLUDE_BREAKSPEED = TRUE {
		iny
		lda chord,y
		bpl speednowrap
		and #$7f
		tay
speednowrap	sty speedsub
}
speeddone
;---------------------------------------
		ldx #2
main0		lda voicon,x
		bne trackon
		jmp next
trackon		inc synccnt,x
		lda speedcnt
		beq jupdseq
		cmp #1
		beq updtrack
		jmp updsound
jupdseq		jmp updseq
;---------------------------------------
updtrack	lda newseq,x
		beq skiptrack
		sec
		sbc #1		
		sta seqcnt,x
		lda #0
		sta newseq,x
		tay
		lda tracklo,x
		sta ZREG
		lda trackhi,x
		sta ZREG+1
		lda (ZREG),y
		bpl trk02
		cmp #$80			; get transpose value
		beq skiptrans
		sbc #$a0
		sta shtrans2,x
skiptrans	inc tracklo,x
		bne trk01
		inc trackhi,x
trk01		iny
		lda (ZREG),y
trk02		sta curseq,x
		iny
		lda (ZREG),y
		cmp #$f0
		bcc trk03
		pha
		iny
		lda (ZREG),y
		clc
		adc twraplo,x
		sta tracklo,x
		pla
		and #$07
		adc twraphi,x			; song wrap
		sta trackhi,x
		jmp updsound
trk03		inc tracklo,x
		bne skiptrack
		inc trackhi,x
skiptrack	jmp updsound
;---------------------------------------
updseq		dec durcnt,x
		bmi nextnote
		jmp updsound
nextnote	ldy curseq,x
		lda seqlo,y
		sta ZREG
		lda seqhi,y
		sta ZREG+1
		lda #2
		sta tsync,x
getseq		ldy seqcnt,x
seqnext		lda (ZREG),y
		cmp #$c0
		bcs command
		cmp #$60-1			; command coming up?
		bcc nocmdbyt
		sbc #$60
		bpl nottie
		inc tienote,x			; $5f = flag tienote
		iny
		jmp seqnext
nottie		pha				; store note value
		iny
		lda (ZREG),y			; fetch sequence command
		beq skipcmd
		sta shsuper,x
		inc newcmdflag,x
skipcmd		pla
nocmdbyt	sta shnote,x			; check for rest & gate flags
		cmp #3
		bcs sequpdtrans
settie		inc tienote,x
		jmp seqdone
command		cmp #$f0
		bmi notdur
setdur		and #$0f
		sta duration,x
		iny
		jmp seqnext
notdur		sbc #$c0-1
		sta shinst,x
		inc newinsflag,x
		iny
		jmp seqnext
sequpdtrans	lda shtrans2,x
		sta shtrans,x
seqdone		iny
		beq seqsetflag		       ; new seq automatically if seqcnt wrapped
		tya
		sta seqcnt,x
		lda (ZREG),y
		cmp #$bf		       ;seq end mark
		bne noteos
seqsetflag	inc newseq,x
noteos		
		lda duration,x
		sta durcnt,x			
!if INCLUDE_CMD_PORTA = TRUE {
		lda newcmdflag,x
		beq snotporta			; !
		;; Check for super commands that
		;; ;should be parsed immidiately
		ldy shsuper,x		       ;cmd byte?
		cpy #$40
		bpl updsound
		lda cmd1,y
		cmp #CMD_PORTAMENTO
		bne snotporta
		lda cmd2,y
		and #$0f
		sta portahi,x
		lda cmd3,y
		sta portalo,x
		lda #$81
		sta effstate,x
		;; formerly clrsuper
		lda #0
		sta newcmdflag,x
		jmp updsound
snotporta
}
;;; ----------------------------------------
;;; sound work
;;; ----------------------------------------
updsound	lda #0
		sta hardon,x
		lda tsync,x
		bpl dosync
		jmp syncskip
dosync		dec tsync,x
		lda tienote,x			
		beq syncnottied
		jmp syncskip
syncnottied	lda tsync,x
		cmp #1
		bne syncgate
		lda synccnt,x			; hard restart possible?
		cmp #2			
		bmi syncnohr
		ldy shinst,x			; use hard restart?
		lda inst+INS_HR,y		
		bpl syncnohr
		and #$20
		bne laxhr
		;; Hard restart
		lda cmd2			;Set adsr for HR
		sta ad,x			
laxhr		lda inst+INS_7,y
		sta sr,x
syncnohr	lda #$fe
		sta gate,x
		jmp dowave			;only update wavetable
syncgate	cmp #$ff
		beq syncgateon
		jmp syncskip
syncgateon	lda newinsflag,x
		beq checknote
		ldy shinst,x			
		lda inst,y			;Store ADSR of insturmen in shadow
		sta shad,x			;ADSR regs
		lda inst+INS_SR,y		
		sta shsr,x
		lda effstate,x			;don't reset porta
		bmi checknote
		lda #0
		sta effstate,x
checknote	lda shtrans,x			;set transpose to current
		sta trans,x
		lda shnote,x			
		clc				;get real (transposed) value of the note
		adc trans,x			
		sta notereal,x
		ldy effstate,x			;skip if portamento
		bmi skipsetfrq			
!if INCLUDE_CMD_PORTA = TRUE {
		tay
		lda freqtable_lo,y
		sta plo,x
		lda freqtable_hi,y
		sta phi,x
}		
		txa
		sta shfreqlo,x
		lda #0
		sta shfreqhi,x
skipsetfrq	ldy shinst,x
		lda inst+INS_ARP,y		;set wave pos
		sta wavepos,x
		lda shad,x			;set ADSR
		sta ad,x
		lda shsr,x
		sta sr,x
		lda inst+INS_PULSP,y		;set the pulse
		beq setflt
!if INCLUDE_DIRECT_PULSE = TRUE {                
		bpl skippdirect
		and #$0f
		sta pulsehi,x
                lda #0
		sta pulselo,x
		jmp pulsdirset
}
skippdirect	asl				
		asl
pulsdirset	sta pulsenxt,x			;pointer
		lda #0
		sta pulsecnt,x
setflt		
!if INCLUDE_FILTER = TRUE {		
		lda inst+INS_FLTP,y
		beq filterdone			;no filter reset
skipcutdirect	asl				
		asl
                sta filtnxt
		lda #0
		sta filtcnt
filterdone	
}
scmddone	lda #0
		sta newinsflag,x
!if INCLUDE_CHORD = TRUE {
		sta chordvalue,x
		lda #$80
		sta chordtpos,x
}
		lda inst+INS_HR,y		;set wave timer
		and #$0f
		sta wavetime,x
		lda #0
		sta wavecnt,x
		lda inst+INS_HR,y
		and #$c0			;Check for soft
		cmp #$40			;restart
		beq wavenotoff
		lda inst+INS_4,y
		ora #1
		sta waveform,x
		inc hardon,x
wavenotoff	lda #$ff			;Gate on
		sta gate,x
		lda #$00			;Reset sync
		sta synccnt,x			
		ldy effstate,x
		bmi noeffreset
		sta effstate,x
noeffreset	jmp checksuper
syncskip
;;; ----------------------------------------
;;; pulsework
;;; ----------------------------------------
updatepulse	ldy pulsecur,x
		dec pulsecnt,x
		bpl pulsenotnew
		lda pulsenxt,x
		sta pulsecur,x
		tay
		lda pulstab+2,y
		cmp #$ff
		beq pulseskipset
		sta ZREG
		and #$f0
		sta pulselo,x
		lda ZREG
		and #$0f
		sta pulsehi,x
pulseskipset	lda pulstab+0,y
		and #$7f
		sta pulsecnt,x
		lda pulstab+3,y
		bne pulsenotnxt
		lda pulsenxt,x
		clc
		adc #4
		jmp pulsesetnxt
pulsenotnxt	cmp #$7f
		bne pulsenotstop
		lda #0
		jmp pulsesetnxt
pulsenotstop	asl
		asl
pulsesetnxt	sta pulsenxt,x
pulsenotnew	lda pulstab,y
		bmi pulsesub
		lda pulselo,x
		clc
		adc pulstab+1,y
		sta pulselo,x
		bcc pulsedone
		inc pulsehi,x
		jmp pulsedone
pulsesub	lda pulselo,x
		sec
		sbc pulstab+1,y
		sta pulselo,x
		bcs pulsedone
		dec pulsehi,x
pulsedone		  
;;; ----------------------------------------
;;; sfx
;;; ----------------------------------------
		lda effstate,x
		bne effdo1
		jmp effdone
effdo1		
!if INCLUDE_CMD_SLUP {		
		cmp #$01
		bne effdo2
effslideup	lda shfreqlo,x
		clc
		adc slidelo,x
		sta shfreqlo,x
		lda shfreqhi,x
		adc slidehi,x
		sta shfreqhi,x
		jmp effdone
}
effdo2
!if INCLUDE_CMD_SLDOWN {
		cmp #$02
		bne effdo3
effslidedown	lda shfreqlo,x
		sec
		sbc slidelo,x
		sta shfreqlo,x
		lda shfreqhi,x
		sbc slidehi,x
		sta shfreqhi,x
		jmp effdone
}
effdo3			
!if INCLUDE_CMD_SET_LOVIB = TRUE {		
		cmp #$04
		bne effdo3a
		lda vibraamp,x
		sta ZREG
		lda #0
		asl ZREG
		rol
		asl ZREG
		rol
		sta ZREG+1
		jmp vibrealadd
}				
effdo3a
!if INCLUDE_CMD_VIBR = FALSE {
                jmp effdo4
} else {                
;;; !if INCLUDE_CMD_VIBR = TRUE | INCLUDE_CMD_SET_LOVIB = TRUE {
		cmp #$03
		beq effvibrato
		jmp effdo4
effvibrato	ldy notereal,x			
		sec
		lda freqtable_lo+1,y
		sbc freqtable_lo,y
		sta ZREG
		lda freqtable_hi+1,y
		sbc freqtable_hi+0,y
		sta ZREG+1
		lda vibracor,x
		clc
		adc vibraamp,x
		tay
		lda #0
		sta vibracor,x
viblessamp	dey
		bmi vibadd
		lsr ZREG+1
		ror ZREG
		jmp viblessamp
vibadd		
		lda ZREG
		clc
		adc vibrafl,x
		sta ZREG
		lda ZREG+1
		adc vibrafh,x
		sta ZREG+1
		lda vibrafl,x
		clc
		adc vibraflv,x
		sta vibrafl,x
		lda vibrafh,x
		adc #0
		sta vibrafh,x
}
!if INCLUDE_CMD_VIBR = TRUE | INCLUDE_CMD_SET_LOVIB = TRUE {
vibrealadd	lda vibradir,x
		and #1
		bne vibradown
		lda shfreqlo,x
		clc
		adc ZREG
		sta shfreqlo,x
		lda shfreqhi,x
		adc ZREG+1
		sta shfreqhi,x
		jmp vibrapost
vibradown	lda shfreqlo,x
		sec
		sbc ZREG
		sta shfreqlo,x
		lda shfreqhi,x
		sbc ZREG+1
		sta shfreqhi,x
vibrapost	clc
		lda vibracnt,x
		adc #1
		cmp vibrafrq,x
		bcc vibradirok
		inc vibradir,x
		lda #0
vibradirok	sta vibracnt,x
vibradone
}
effdo4
!if INCLUDE_CMD_PORTA = TRUE {
		cmp #$81			; Portamento
		beq effporta
		jmp effdone
effporta
		ldy notereal,x
		lda freqtable_lo,y
		sta ZREG
		lda freqtable_hi,y
		sta ZREG+1
		lda plo,x
		sec
		sbc ZREG
		sta plo,x
		lda phi,x
		sbc ZREG+1
		sta phi,x
		bmi portaup
		lda plo,x
		sec
		sbc portalo,x
		sta plo,x
		lda phi,x
		sbc portahi,x
		sta phi,x
		bpl portaclc
		jmp portaset
portaup		lda plo,x
		clc
		adc portalo,x
		sta plo,x
		lda phi,x
		adc portahi,x
		sta phi,x
		bmi portaclc
portaset	lda ZREG
		sta plo,x
		lda ZREG+1
		sta phi,x
		jmp portadone
portaclc	lda plo,x
		clc
		adc ZREG
		sta plo,x
		lda phi,x
		adc ZREG+1
		sta phi,x
portadone	ldy notereal,x
		lda plo,x
		sec
		sbc freqtable_lo,y
		sta shfreqlo,x
		lda phi,x
		sbc freqtable_hi,y
		sta shfreqhi,x
}
effdone
;;; ----------------------------------------
;;; update wavetable
;;; ----------------------------------------
dowave		lda hardon,x
		beq waveok
		jmp wavedone
waveok		dec wavecnt,x
		bpl waveprocess
		lda wavetime,x
		sta wavecnt,x
		ldy wavepos,x
		lda arp1,y
		sta wavetrans,x
		lda arp2,y
		cmp #$10
		bcc waveskip
		cmp #$e0
		bcc wavereg
		and #$0f
wavereg		sta waveform,x
waveskip	lda arp1+1,y
		cmp #$7e
		beq wavestore
		iny
wavenotend	cmp #$7f
		bne wavenotend2
		lda arp2,y
		tay
wavenotend2	lda arp2,y
		beq wavestore
		cmp #$10
		bcs wavestore
		sta wavecnt,x
wavestore	tya
		sta wavepos,x
!if INCLUDE_CHORD = TRUE {
		ldy chordtpos,x
		bmi chorddone
chordinit	lda chord,y
		cmp #$40
		bcc chordnotneg
		ora #$80
chordnotneg	sta chordvalue,x
		inc chordtpos,x
		lda chord+1,y
		bpl chorddone
		and #$7f
		sta chordtpos,x
chorddone	
}
waveprocess	lda wavetrans,x
		bpl wavenotabs
waveabs		and #$7f
		tay
		lda freqtable_lo,y
		sta freqlo,x
		lda freqtable_hi,y
		sta freqhi,x
		jmp wavedone
wavenotabs	clc
		adc notereal,x
!if INCLUDE_CHORD = TRUE {
		adc chordvalue,x
}
		tay
		lda freqtable_lo,y
		clc
		adc shfreqlo,x
		sta freqlo,x
		lda freqtable_hi,y
		adc shfreqhi,x
		sta freqhi,x
wavedone
;;; ------------------------------------------------------------
checksuper	lda tsync,x
		cmp #$ff
		beq supersync
		jmp superdone
supersync	lda newcmdflag,x
		bne superparse
		jmp superdone
superparse	lda #0
		sta newcmdflag,x
superparse2	ldy shsuper,x
		cpy #$40
		bcs *+5
		jmp iscmd
		tya
		cmp #$60
		bcs notpulse
		and #$1f
		asl
		asl
		sta pulsenxt,x
		lda #0
		sta pulsecnt,x
		jmp superdone
notpulse	
!if INCLUDE_FILTER = TRUE {
		cmp #$80
		bcs notfilt
		and #$1f
		asl
		asl
		sta filtnxt
		lda #0
		sta filtcnt
		jmp superdone
notfilt		
}	
!if INCLUDE_CHORD = TRUE {
		cmp #$a0
		bcs notchord
		and #$1f
		tay
		lda chordindex,y
		sta chordtpos,x
		jmp superdone
}
notchord
!if INCLUDE_SEQ_SET_ATT = TRUE {
		cmp #$b0
		bcs notatt
		asl
		asl
		asl
		asl
		sta ZREG
		lda ad,x
		and #$0f
		ora ZREG
		sta ad,x
		jmp superdone
}		
notatt		
!if INCLUDE_SEQ_SET_DEC = TRUE {
		cmp #$c0
		bcs notdec
		and #$0f
		sta ZREG
		lda ad,x
		and #$f0
		ora ZREG
		sta ad,x
		jmp superdone
}
notdec
!if INCLUDE_SEQ_SET_SUS = TRUE {
		cmp #$d0
		bcs notsus
		asl
		asl
		asl
		asl
		sta ZREG
		lda sr,x
		and #$0f
		ora ZREG
		sta sr,x
		jmp superdone
}
notsus		
!if INCLUDE_SEQ_SET_REL = TRUE {
		cmp #$e0
		bcs notrel
		and #$0f
		sta ZREG
		lda sr,x
		and #$f0
		ora ZREG
		sta sr,x
		jmp superdone
}		
notrel
!if INCLUDE_SEQ_SET_VOL = TRUE {
		cmp #$f0
		bcs notvol
		and #$0f
		sta volume
		jmp superdone
}
notvol		
!if INCLUDE_SEQ_SET_SPEED = TRUE {			
		and #$0f
		sta speed
		cmp #2
		bcs notvol2
		tay
		lda chord,y
notvol2		sta playspeed
		sta speedcnt
		dec speedcnt
}
		jmp superdone
;;; ----------------------------------------
;;; process a command table entry
;;; ----------------------------------------
iscmd		lda cmd2,y
		sta ZREG
		lda cmd1,y
;		   and #$0f
		sta ZREG+1
		cmp #SUPERHIGH
		bcc superlow
		jmp superhigh
		 ;SLIDE UP
superlow	
!if INCLUDE_CMD_SLUP = TRUE {		
		cmp #0
		bne snotslide1
		lda ZREG
		sta slidehi,x
		lda cmd3,y
		sta slidelo,x
		lda #1
		sta effstate,x
		jmp superdone
}		 ;SLIDE DOWN
snotslide1		
!if INCLUDE_CMD_SLDOWN = TRUE {		
		cmp #CMD_SLIDE_DOWN
		bne snotslide2
		lda ZREG
		sta slidehi,x
		lda cmd3,y	
		sta slidelo,x
		lda #2
		sta effstate,x
		jmp superdone
}		
		 ;VIBRATO
snotslide2	
!if INCLUDE_CMD_VIBR = TRUE {		
		cmp #CMD_VIBRATO
		bne snotvibrato
		lda #3
		sta effstate,x
		lda ZREG
		and #$0f
		sta vibraflv,x
		lda cmd3,y
		and #$0f
		sta vibraamp,x
		lda cmd3,y
		lsr 
		lsr 
		lsr 
		lsr 
		clc
		adc #1
		sta vibrafrq,x
		lsr 
		bcc novibcor
		inc vibracor,x			;Correction
novibcor	sta vibracnt,x
		lda #0
		sta vibradir,x
		sta vibrafl,x
		sta vibrafh,x
}		
snotvibrato	jmp superdone
superhigh
!if INCLUDE_CMD_SET_OFFSET = TRUE {
		cmp #CMD_SET_OFFSET
		bne snotoffset
		lda cmd2,y
		sta shfreqhi,x
		lda cmd3,y
		sta shfreqlo,x
		jmp superdone
snotoffset
}
!if INCLUDE_CMD_SET_ADSR = TRUE {
		cmp #CMD_SET_ADSR
		bne snotattdec
		lda cmd2,y
		sta ad,x
		lda cmd3,y
		sta sr,x
		jmp superdone
snotattdec
}
!if INCLUDE_CMD_SET_LOVIB {
		cmp #CMD_SET_LOVIB
		bne snotlovib
		lda #4
		sta effstate,x
		lda cmd2,y
		sta vibrafrq,x
		lsr
	;; 	adc #0
		sta vibracnt,x
		lda cmd3,y
		sta vibraamp,x
		lda #0
		sta vibradir,x
snotlovib
}
!if INCLUDE_CMD_SET_WAVE {
		cmp #CMD_SET_WAVE
		bne snotwave
		lda cmd3,y
		sta waveform,x
snotwave
}
!if INCLUDE_CMD_PORTA {
		cmp #CMD_STOP
		bne snotstop
		lda #0
		sta effstate,x
snotstop
}
superdone
;;; ----------------------------------------
setsid		ldy voice,x
		lda freqlo,x
		sta $d400,y
		lda freqhi,x
		sta $d401,y
		lda sr,x
		sta $d406,y
		lda ad,x
		sta $d405,y
		lda pulselo,x
		sta $d402,y
		lda pulsehi,x
		sta $d403,y
		lda waveform,x
		and gate,x
		sta $d404,y
;;; ----------------------------------------
!if MULTISPEED = TRUE {                
		bit state
		bvc *+5
		jmp next
}                
		;; check tie & super
		lda tsync,x			
		cmp #$ff
		beq postsync
		jmp skippostsync
postsync	dec tsync,x
		lda tienote,x
		bne tiednote
		jmp next
tiednote	lda shtrans,x			;check the note
		sta trans,x
		lda shnote,x
		beq tiestore
		cmp #3
		bcc setgatestat
		clc
		adc trans,x
		sta notereal,x
		ldy effstate,x
		bmi tieclear
		lda #0
		sta shfreqlo,x
		sta shfreqhi,x
		sta effstate,x
skiptiefrq	jmp tiestore
setgatestat	tay
		lda gatestat-1,y
		sta gate,x
tieclear	lda #0
tiestore	sta tienote,x
skippostsync
;---------------------------------------
next		dex
		bmi maindone
!if EXPORT = FALSE {
		bit state			; 7th bit, keyjam call
                bpl *+5
		jmp updsound
		bvs *+5				; 6th bit, multiplay call
		jmp main0
		jmp syncskip
} else {
!if MULTISPEED = TRUE {
                bit state
                bvs *+5
                jmp main0
                jmp syncskip
} else {
                jmp main0
}                
}
maindone	
!if MULTISPEED = TRUE | EXPORT = TRUE {                
                lda #0
		sta state
}                
;;; ----------------------------------------
;;; filter routine
;;; ----------------------------------------
;;; feb '12: sweeps now in 10 bits res to allow faster sweeps
;;; ($80 in 10 bits corresponds to $20 in 8 bits)
!if INCLUDE_FILTER = TRUE {		
		dec filtcnt
		bpl filtnotnew
		lda filtnxt
filtstart	sta filtcur
		tay
		lda filttab,y		;byte A = duration or bandpass
		bpl filtsetcnt
		and #$70
		sta bandpass
		lda filttab+1,y
		sta $d417
		lda #0
filtsetcnt	sta filtcnt
		lda filttab+1,y
		and #3
		asl
		sta filtadd+1
		lda filttab+1,y
		cmp #$80
		ror
		cmp #$80
		ror
		; cmp #$80
		; ror
		sta filtadd
filtjump	lda filttab+3,y			; check jump value
		bne filtnotnxt
		lda filtnxt
		clc
		adc #4
		jmp filtsetnxt
filtnotnxt	cmp #$7f
		bne filtnotstop
		lda #0
		jmp filtsetnxt		
filtnotstop	asl
		asl
filtsetnxt	sta filtnxt
 		lda filttab+2,y
 		cmp #$ff
 		beq filtnotset
 		sta filter
 		lda #0
 		sta filtlo
filtnotset	
		jmp filterskip
filtnotnew	lda filtadd+1
		clc
		adc filtlo
		cmp #8
		and #7				
		sta filtlo
		lda filter
		adc filtadd
		sta filter
filterskip	lda filtlo
		sta $d415
		lda filter
		sta $d416
}
		lda volume
!if INCLUDE_FILTER = TRUE {
		ora bandpass
}
		sta $d418
		rts
!if USE_MDRIVER = TRUE {
cplay	dec cntr
	bmi *+5
	jmp submplay
	lda #MULTIPLIER
	sta cntr
	jmp subplay
cinit	ldx #0
	stx cntr
timerlo	= *+1
	ldx #<CIA_VALUE
timerhi	= *+1
	ldy #>CIA_VALUE
	sty $dc05
	stx $dc04
	jmp subinit
cntr		!8 0
}
!if EXPORT = FALSE {
dummy		!8 0
}
;---------------------------------------
freqtable_lo
		!8 $16,$27,$38,$4b,$5f,$73
		!8 $8a,$a1,$ba,$d4,$f0,$0e
		!8 $2d,$4e,$71,$96,$bd,$e7
		!8 $13,$42,$74,$a9,$e0,$1b
		!8 $5a,$9b,$e2,$2c,$7b,$ce
		!8 $27,$85,$e8,$51,$c1,$37
		!8 $b4,$37,$c4,$57,$f5,$9c
		!8 $4e,$09,$d0,$a3,$82,$6e
		!8 $68,$6e,$88,$af,$eb,$39
		!8 $9c,$13,$a1,$46,$04,$dc
		!8 $d0,$dc,$10,$5e,$d6,$72
		!8 $38,$26,$42,$8c,$08,$b8
		!8 $a0,$b8,$20,$bc,$ac,$e4
		!8 $70,$4c,$84,$18,$10,$70
		!8 $40,$70,$40,$78,$58,$c8
		!8 $e0,$98,$08,$30,$20,$2e
freqtable_hi
		!8 $01,$01,$01,$01,$01,$01
		!8 $01,$01,$01,$01,$01,$02
		!8 $02,$02,$02,$02,$02,$02
		!8 $03,$03,$03,$03,$03,$04
		!8 $04,$04,$04,$05,$05,$05
		!8 $06,$06,$06,$07,$07,$08
		!8 $08,$09,$09,$0a,$0a,$0b
		!8 $0c,$0d,$0d,$0e,$0f,$10
		!8 $11,$12,$13,$14,$15,$17
		!8 $18,$1a,$1b,$1d,$1f,$20
		!8 $22,$24,$27,$29,$2b,$2e
		!8 $31,$34,$37,$3a,$3e,$41
		!8 $45,$49,$4e,$52,$57,$5c
		!8 $62,$68,$6e,$75,$7c,$83
		!8 $8b,$93,$9c,$a5,$af,$b9
		!8 $c4,$d0,$dd,$ea,$f8,$fd
;---------------------------------------
 ;Registers
voicon		!8 1,1,1			;flags channels on and off
!if EXPORT = FALSE {
editorflag	!8 0				; set to >0 from packer
}		
state		!8 0
inittemp	= state		; real state value overrides temp at the end of init
bits		!8 %00000001
		!8 %00000010
		!8 %00000100
gatestat	!8 $fe,$ff
voice		!8 0,7,14
tracklo		!8 0,0,0 ;track pointers low
trackhi		!8 0,0,0 ;track pointers hig
twraplo		!8 0,0,0 ;track wrap low
twraphi		!8 0,0,0 ;track wrap high
speed		!8 0
speedcnt	!8 0
playspeed	!8 0
!if INCLUDE_BREAKSPEED = TRUE {
speedsub	!8 0
}
newseq		!8 0,0,0
volume		!8 $0f
shtrans2	!8 0,0,0
clrfirst ;Clear variables from here
;;; ----------------------------------------
;;; State variables
;;; ----------------------------------------
newinsflag	!8 0,0,0			
newcmdflag	!8 0,0,0
hardon		!fi 3,0
duration	!8 0,0,0
durcnt		!8 0,0,0
tsync		!8 0,0,0
synccnt		!8 0,0,0
tienote		!8 0,0,0
notereal	!8 0,0,0
effstate	!8 0,0,0
		;; 0 = no effect
		;; 1 = slide up
		;; 2 = slide down
		;; 3 = vibrato
		;; 0x80 = portamento
;;; ----------------------------------------
;;; Shadow variables
;;; ----------------------------------------
		;; bit 0 = instrument was set
		;; bit 1 = cmd was set
shtrans		!8 0,0,0
shnote		!8 0,0,0
shinst		!8 0,0,0
shsuper		!8 0,0,0
shad		!8 0,0,0
shsr		!8 0,0,0
shfreqlo	!8 0,0,0
shfreqhi	!8 0,0,0
freqlo		!8 0,0,0
freqhi		!8 0,0,0
trans		!8 0,0,0    
gate		!8 0,0,0    
		 ;Sequence
curseq		!8 0,0,0
seqcnt		!8 0,0,0
		 ;Filter
!if INCLUDE_FILTER = TRUE {
bandpass	!8 0
filter		!8 0
filtcnt		!8 0
filtcur		!8 0
filtnxt		!8 0
filtadd		!8 0,0
filtlo		!8 0
}
		 ;Vibration variables
!if INCLUDE_CMD_VIBR = TRUE | INCLUDE_CMD_SET_LOVIB = TRUE {
vibracnt	!8 0,0,0 ;Count
vibradir	!8 0,0,0 ;Direction
vibraamp	!8 0,0,0 ;Amplitude
vibrafrq	!8 0,0,0 ;Frequency
}
!if INCLUDE_CMD_VIBR = TRUE {
vibracor	!8 0,0,0 ;Tune correction
vibrafl		!8 0,0,0 ;Vibrato feel low
vibrafh		!8 0,0,0 ;Vibrato feel high
vibraflv	!8 0,0,0 ;Vibrato feel add
}
		 ;Slide variables
!if INCLUDE_CMD_SLUP = TRUE | INCLUDE_CMD_SLDOWN = TRUE {
slidelo		 !8 0,0,0 ;Low fraction
slidehi		 !8 0,0,0 ;High fraction
}
		 ;Pulse
pulsecur	!8 0,0,0
pulsenxt	!8 0,0,0
pulsecnt	!8 0,0,0
pulselo		!8 0,0,0 ;-->d402
pulsehi		!8 0,0,0 ;-->d403
		 ;ADSR
ad		!8 0,0,0 ;-->d405
sr		!8 0,0,0 ;-->d406
		 ;Waveform
waveform	!8 0,0,0 ;-->d404
		 ;Wavetable
wavetrans	!8 0,0,0
wavecnt		!8 0,0,0
wavepos		!8 0,0,0
wavetime	!8 0,0,0
!if INCLUDE_CHORD = TRUE {
chordtpos	!8 0,0,0
chordvalue	!8 0,0,0
}
		 ;Portamento
!if INCLUDE_CMD_PORTA = TRUE {
portahi		!8 0,0,0
portalo		!8 0,0,0
plo		!8 0,0,0
phi		!8 0,0,0
}
clrlast		 ;Clear variables to here
!if EXPORT = FALSE {
*=$2000
;---------------------------------------
songsets	!16 track1,track2,track3
		!8 5,7
;---------------------------------------
		*= songsets + $200
track1		!8 $a0,$00,$f0,0
		!for .v, $1fe {
				!8 $f0,00
		}
		*= track1+$0400
track2		!8 $a0,$00,$f0,0
		!for .v, $1fe {
				!8 $f0,00
		}
		*= track2+$0400
track3		!8 $a0,$00,$f0,0
		!for .v, $1fe {
				!8 $f0,00
		}
;---------------------------------------
		*= track3 + $400
seqlo		!fi 128, <s0
seqhi		!for .v, 128 {
			!8 >s0+((.v-1)*256)
 		}
;---------------------------------------
		*= seqlo+$0100
!macro emptyseq {
		!8 $f0,$f0,$60,$00
		!8 $bf
}
s0		+emptyseq
!set curPC = s0
!for .v, 128-1 {
		*= curPC + $100
		!set curPC = *
		+emptyseq
}
;---------------------------------------
		*= curPC + $100
arp1		!8 0
;---------------------------------------
		*= arp1+$0100
arp2		!8 0
;---------------------------------------
		*= arp2+$0100
inst		!fi 48*8,0
;;; (more or less) dynamic tables follow......
		*= inst+$0200
supertab
cmd1		!8 $00
		*= cmd1+$40
cmd2		!8 $0f
		*= cmd2+$40
cmd3		!8 $00
		*= cmd3+$40
;---------------------------------------
;---------------------------------------
		*= supertab+$0100
filttab		!8 $7f,$00,$ff,$7f
;---------------------------------------
		*= filttab+$0100
pulstab		!8 $7f,$00,$ff,$7f
		*= pulstab+$0100
chord		!fi 128,0
chordindex	!fi 32,0
}
arp1 = *
		!byte $00,$0c,$18,$00,$7e,$00,$7e,$00,$7e,$df,$00,$7e,$df,$ac,$a8,$df
		!byte $7e,$df,$ac,$a8,$df,$00,$7f,$c8,$a2,$80,$00,$7f,$df,$00,$7e,$df
		!byte $00,$00,$7e
arp2 = *
		!byte $00,$11,$00,$11,$00,$41,$00,$41,$00,$81,$80,$00,$81,$41,$41,$80
		!byte $00,$81,$41,$41,$80,$41,$15,$81,$41,$10,$41,$1a,$81,$41,$00,$81
		!byte $02,$41,$00
filttab = *
		!byte $7f,$00,$ff,$7f
		!byte $01,$fb,$ff,$7f
		!byte $f0,$f1,$f2,$00
		!byte $04,$50,$5e,$00
		!byte $90,$51,$0a,$01
		!byte $90,$01,$0a,$00
		!byte $01,$fb,$ff,$7f
		!byte $f0,$01,$2e,$00
		!byte $00,$00,$ff,$05
		!byte $c0,$f1,$c0,$00
		!byte $01,$fe,$ff,$00
		!byte $90,$01,$0a,$7f
		!byte $90,$f7,$01,$00
		!byte $7f,$08,$ff,$7f
		!byte $00,$00,$00
pulstab = *
		!byte $7f,$00,$ff,$7f
		!byte $00,$00,$08,$00
		!byte $40,$01,$ff,$00
		!byte $c0,$01,$ff,$02
		!byte $25,$1b,$1b,$00
		!byte $d5,$1b,$ff,$04
		!byte $25,$1b,$1b,$7f
		!byte $25,$0a,$88,$7f
		!byte $25,$0a,$6b,$7f
		!byte $00,$00,$00
inst = *
inst0 = *
		!byte $04,$00,$00,$02,$00,$00,$0e,$40,$00,$00,$00,$00,$40,$00,$14,$04
		!byte $04
inst1 = *
		!byte $78,$6c,$f2,$80,$a9,$f4,$ea,$5a,$6a,$f0,$f4,$f4,$4a,$6a,$ea,$3a
		!byte $7a
inst2 = *
		!byte $10,$80,$80,$80,$80,$80,$80,$81,$82,$80,$80,$80,$81,$80,$10,$80
		!byte $80
inst3 = *
		!byte $00,$00,$81,$00,$00,$00,$00,$81,$81,$00,$81,$00,$81,$00,$00,$81
		!byte $81
inst4 = *
		!byte $00,$00,$0b,$09,$00,$02,$00,$00,$00,$09,$07,$02,$00,$00,$00,$00
		!byte $00
inst5 = *
		!byte $00,$01,$06,$00,$01,$88,$00,$07,$04,$06,$06,$06,$08,$00,$00,$04
		!byte $04
inst6 = *
		!byte $00,$00,$40,$20,$40,$40,$00,$00,$00,$20,$40,$40,$00,$00,$00,$00
		!byte $00
inst7 = *
		!byte $01,$05,$07,$09,$05,$0c,$03,$05,$05,$1c,$17,$11,$05,$03,$01,$1f
		!byte $1f
seqlo = *
		!8 <s00,<s01,<s02,<s03,<s04,<s05,<s06,<s07,<s08,<s09,<s0a,<s0b,<s0c,<s0d,<s0e,<s0f,<s10,<s11,<s12,<s13,<s14,<s15,<s16,<s17,<s18,<s19,<s1a,<s1b,<s1c,<s1d,<s1e,<s1f,<s20,<s21,<s22,<s23,<s24,<s25,<s26,<s27,<s28,<s29,<s2a,<s2b,<s2c,<s2d,<s2e,<s2f,<s30,<s31,<s32,<s33,<s34,<s35,<s36,<s37
seqhi = *
		!8 >s00,>s01,>s02,>s03,>s04,>s05,>s06,>s07,>s08,>s09,>s0a,>s0b,>s0c,>s0d,>s0e,>s0f,>s10,>s11,>s12,>s13,>s14,>s15,>s16,>s17,>s18,>s19,>s1a,>s1b,>s1c,>s1d,>s1e,>s1f,>s20,>s21,>s22,>s23,>s24,>s25,>s26,>s27,>s28,>s29,>s2a,>s2b,>s2c,>s2d,>s2e,>s2f,>s30,>s31,>s32,>s33,>s34,>s35,>s36,>s37
cmd1 = *
		!byte $00,$05,$05,$01,$05,$05,$07,$07,$07
cmd2 = *
		!byte $0f,$05,$06,$00,$04,$04,$00,$01,$02
cmd3 = *
		!byte $00,$0f,$03,$60,$07,$0b,$50,$50,$00
songsets = *
!word	 track0_0, track0_1, track0_2
		!byte 1, 7
track0_0 = *
		!byte $a0,$01,$29,$04,$08,$0b,$0d,$04,$08,$0b,$10,$16,$16,$13,$1a,$1d
		!byte $20,$23,$31,$04,$08,$0b,$0d,$04,$08,$0b,$10,$16,$16,$13,$1a,$1d
		!byte $30,$2d,$16,$16,$13,$1a,$1d,$32,$34,$37,$00,$f0,$00
track0_1 = *
		!byte $a0,$02,$2a,$05,$07,$0a,$0e,$05,$07,$0a,$11,$17,$17,$18,$1b,$1e
		!byte $21,$24,$27,$05,$07,$0a,$0e,$05,$07,$0a,$11,$17,$17,$18,$1b,$1e
		!byte $21,$2e,$17,$17,$18,$1b,$1e,$26,$35,$19,$00,$f0,$00
track0_2 = *
		!byte $a0,$03,$2b,$06,$09,$0c,$0f,$06,$09,$0c,$12,$15,$15,$1f,$1c,$1f
		!byte $22,$25,$28,$06,$09,$0c,$0f,$06,$09,$0c,$12,$15,$15,$1f,$1c,$1f
		!byte $2c,$2f,$15,$15,$1f,$1c,$1f,$33,$36,$14,$00,$f0,$00
s00 = *
		!byte $ff,$00,$00,$00,$00,$bf
s01 = *
		!byte $c0,$f1,$96,$6c,$f3,$01,$f0,$96,$04,$f4,$01,$f1,$96,$04,$f3,$01
		!byte $f1,$96,$04,$01,$96,$04,$01,$96,$04,$01,$96,$04,$93,$04,$f3,$01
		!byte $f0,$93,$04,$f4,$01,$f1,$93,$04,$f3,$01,$f1,$93,$04,$01,$93,$04
		!byte $01,$93,$04,$01,$93,$04,$bf
s02 = *
		!byte $c0,$f1,$9a,$f1,$f3,$01,$f0,$3a,$f4,$01,$f1,$3a,$f3,$01,$f1,$3a
		!byte $01,$3a,$01,$3a,$01,$3a,$36,$f3,$01,$f0,$36,$f4,$01,$f1,$36,$f3
		!byte $01,$f1,$36,$01,$36,$01,$36,$01,$36,$bf
s03 = *
		!byte $c0,$f1,$9d,$04,$9d,$04,$f0,$9d,$04,$01,$f1,$9d,$04,$9d,$04,$9d
		!byte $04,$f0,$9d,$04,$01,$f1,$9d,$04,$9d,$04,$9d,$04,$9d,$04,$9d,$04
		!byte $9d,$04,$9d,$04,$9d,$04,$9d,$04,$9b,$04,$9b,$04,$9b,$04,$9b,$04
		!byte $9b,$04,$9b,$04,$9b,$04,$9b,$04,$9b,$04,$9b,$04,$9b,$04,$9b,$04
		!byte $9b,$04,$9b,$04,$9b,$04,$9b,$04,$bf
s04 = *
		!byte $ca,$f1,$1e,$c3,$1e,$c5,$2a,$c9,$1e,$ca,$1e,$c3,$1e,$c5,$1e,$c3
		!byte $1e,$ca,$1e,$c3,$2a,$c5,$1e,$c3,$2a,$ca,$1e,$c3,$2a,$c5,$1e,$c3
		!byte $2a,$ca,$20,$c3,$20,$c5,$23,$c9,$20,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$20,$c3,$23,$c5,$20,$c3,$23,$ca,$20,$c3,$23,$c5,$20,$c3
		!byte $23,$bf
s05 = *
		!byte $c1,$fb,$96,$04,$f0,$5f,$42,$f2,$60,$04,$f0,$5f,$41,$f4,$60,$04
		!byte $f0,$5f,$3d,$f4,$60,$05,$f0,$5f,$3a,$f2,$60,$05,$f0,$5f,$3f,$fa
		!byte $60,$05,$f3,$60,$01,$f0,$5f,$38,$fe,$60,$04,$bf
s06 = *
		!byte $c7,$f1,$96,$81,$f3,$01,$cc,$f1,$96,$81,$f3,$01,$c7,$f1,$96,$81
		!byte $f3,$01,$cc,$f1,$96,$81,$f3,$01,$c7,$f1,$96,$81,$01,$cc,$96,$81
		!byte $01,$c7,$98,$81,$f3,$01,$cc,$f1,$98,$81,$f3,$01,$c7,$f1,$98,$81
		!byte $f3,$01,$cc,$f1,$98,$81,$f3,$01,$c7,$f1,$98,$81,$01,$cc,$98,$81
		!byte $01,$bf
s07 = *
		!byte $c1,$f0,$5f,$38,$fa,$60,$04,$f0,$5f,$3a,$f2,$60,$04,$f0,$5f,$3d
		!byte $f2,$60,$04,$f0,$5f,$38,$f2,$60,$04,$f0,$5f,$35,$f2,$60,$04,$f0
		!byte $5f,$38,$f2,$60,$04,$f0,$5f,$36,$f7,$60,$04,$f9,$60,$05,$f0,$60
		!byte $dc,$fb,$01,$bf
s08 = *
		!byte $ca,$f1,$1d,$c3,$1d,$c5,$29,$c9,$1d,$ca,$1d,$c3,$1d,$c5,$1d,$c3
		!byte $1d,$ca,$22,$c3,$2e,$c5,$22,$c3,$2e,$ca,$22,$c3,$2e,$c5,$22,$c3
		!byte $2e,$ca,$1b,$c3,$1b,$c5,$1e,$c9,$1b,$ca,$1b,$c3,$1b,$c5,$1b,$c3
		!byte $1b,$ca,$1b,$c3,$1e,$c5,$1b,$c3,$1e,$ca,$1b,$c3,$1e,$c5,$1b,$c3
		!byte $1e,$bf
s09 = *
		!byte $c7,$f1,$95,$82,$f3,$01,$cc,$f1,$95,$82,$f3,$01,$c7,$f1,$95,$82
		!byte $f3,$01,$cc,$f1,$9a,$85,$f3,$01,$c7,$f1,$9a,$85,$01,$cc,$9a,$85
		!byte $01,$c7,$9f,$86,$f3,$01,$cc,$f1,$9f,$86,$f3,$01,$c7,$f1,$9f,$86
		!byte $f3,$01,$cc,$f1,$9f,$86,$f3,$01,$c7,$f1,$9f,$86,$01,$cc,$9f,$86
		!byte $01,$bf
s0a = *
		!byte $c1,$ff,$96,$04,$f0,$5f,$3f,$f2,$60,$04,$f0,$5f,$3d,$f2,$60,$04
		!byte $f0,$5f,$3a,$60,$04,$5f,$38,$f1,$60,$04,$f0,$5f,$36,$f1,$60,$04
		!byte $f0,$5f,$38,$f5,$60,$04,$f0,$60,$05,$ff,$00,$f0,$5f,$38,$f2,$60
		!byte $04,$f0,$5f,$3a,$f2,$60,$04,$bf
s0b = *
		!byte $ca,$f1,$23,$c3,$23,$c5,$2f,$c9,$23,$ca,$23,$c3,$23,$c5,$23,$c3
		!byte $23,$ca,$23,$c3,$2f,$c5,$23,$c3,$2f,$ca,$23,$c3,$2f,$c5,$23,$c3
		!byte $2f,$ca,$20,$c3,$20,$c5,$23,$c9,$20,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$20,$c3,$23,$c5,$20,$c3,$23,$ca,$20,$c3,$23,$c5,$20,$c3
		!byte $23,$bf
s0c = *
		!byte $c7,$f1,$9b,$87,$f3,$01,$cc,$f1,$9b,$87,$f3,$01,$c7,$f1,$9b,$87
		!byte $f3,$01,$cc,$f1,$9b,$87,$f3,$01,$c7,$f1,$9b,$87,$01,$cc,$9b,$87
		!byte $01,$c7,$98,$82,$f3,$01,$cc,$f1,$98,$82,$f3,$01,$c7,$f1,$98,$82
		!byte $f3,$01,$cc,$f1,$98,$82,$f3,$01,$c7,$f1,$98,$82,$01,$cc,$98,$82
		!byte $01,$bf
s0d = *
		!byte $ca,$f1,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$25,$c3,$25,$c5,$25,$c3
		!byte $25,$ca,$25,$c3,$31,$c5,$25,$c3,$31,$ca,$25,$c3,$31,$c5,$25,$c3
		!byte $31,$ca,$25,$c3,$25,$c5,$28,$c9,$25,$ca,$25,$c3,$25,$c5,$25,$c3
		!byte $25,$ca,$25,$c3,$28,$c5,$25,$c3,$28,$ca,$25,$c3,$28,$c5,$25,$c3
		!byte $28,$bf
s0e = *
		!byte $c1,$f0,$5f,$3d,$f2,$60,$04,$ff,$00,$f0,$5f,$3f,$f2,$60,$04,$f0
		!byte $5f,$42,$f2,$60,$04,$f0,$5f,$3f,$f2,$60,$04,$f0,$5f,$3d,$f4,$60
		!byte $04,$f6,$60,$05,$f2,$60,$dd,$f1,$01,$fd,$60,$01,$bf
s0f = *
		!byte $c7,$f1,$9d,$87,$f3,$01,$cc,$f1,$9d,$87,$f3,$01,$c7,$f1,$9d,$87
		!byte $f3,$01,$cc,$f1,$9d,$87,$f3,$01,$c7,$f1,$9d,$87,$01,$cc,$9d,$87
		!byte $01,$c7,$9d,$88,$f3,$01,$cc,$f1,$9d,$88,$f3,$01,$c7,$f1,$9d,$88
		!byte $f3,$01,$cc,$f1,$9d,$88,$f3,$01,$c7,$f1,$9d,$88,$01,$cc,$9d,$88
		!byte $01,$bf
s10 = *
		!byte $ca,$f1,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$25,$c3,$25,$c5,$25,$c3
		!byte $25,$ca,$25,$c3,$31,$c5,$25,$c3,$31,$ca,$25,$c3,$31,$c5,$25,$c3
		!byte $31,$ca,$22,$c3,$22,$c5,$25,$c9,$22,$ca,$22,$c3,$22,$c5,$22,$c3
		!byte $22,$ca,$22,$c3,$25,$c5,$22,$c3,$25,$ca,$22,$c3,$25,$cb,$1d,$c3
		!byte $25,$bf
s11 = *
		!byte $c1,$f0,$3d,$f2,$60,$04,$ff,$00,$f0,$5f,$3f,$f2,$60,$04,$f0,$5f
		!byte $42,$f2,$60,$04,$f0,$5f,$3f,$f2,$60,$04,$f0,$5f,$3d,$f4,$60,$04
		!byte $f9,$60,$05,$f0,$5f,$3e,$f6,$60,$04,$f7,$01,$bf
s12 = *
		!byte $c7,$f1,$9d,$87,$f3,$01,$cc,$f1,$9d,$87,$f3,$01,$c7,$f1,$9d,$87
		!byte $f3,$01,$cc,$f1,$9d,$87,$f3,$01,$c7,$f1,$9d,$87,$01,$cc,$9d,$87
		!byte $01,$c7,$9a,$82,$f3,$01,$cc,$f1,$9a,$82,$f3,$01,$c7,$f1,$9a,$82
		!byte $f3,$01,$cc,$f1,$9d,$81,$f3,$01,$c7,$f1,$9d,$81,$01,$cc,$f2,$9d
		!byte $81,$f0,$01,$bf
s13 = *
		!byte $ca,$f1,$23,$c3,$23,$c5,$2f,$c9,$23,$ca,$1e,$c3,$1e,$c5,$1e,$c3
		!byte $1e,$ca,$23,$c3,$23,$c5,$2f,$c9,$23,$ca,$1e,$c3,$1e,$c5,$1e,$c3
		!byte $1e,$ca,$20,$c3,$20,$c5,$2c,$c9,$20,$ca,$1b,$c3,$1b,$c5,$1b,$c3
		!byte $1b,$ca,$20,$c3,$20,$c5,$2c,$c9,$20,$ca,$1b,$c3,$1b,$c5,$1b,$c3
		!byte $1b,$bf
s14 = *
		!byte $c7,$f0,$9b,$87,$f3,$01,$c8,$f0,$9b,$87,$f4,$01,$c7,$f0,$9b,$87
		!byte $f4,$01,$f0,$9b,$87,$f3,$01,$f0,$9b,$87,$f1,$01,$f0,$60,$f5,$c8
		!byte $9b,$87,$f2,$01,$f1,$60,$f6,$f0,$96,$83,$fe,$61,$dc,$ff,$00,$bf
s15 = *
		!byte $cf,$f1,$9f,$82,$93,$82,$9f,$82,$9f,$82,$9f,$82,$93,$82,$9f,$82
		!byte $9f,$82,$9f,$82,$93,$82,$9f,$82,$9f,$82,$9f,$82,$d0,$93,$82,$cf
		!byte $9f,$82,$9f,$82,$a7,$87,$9b,$87,$a7,$87,$a7,$87,$a7,$87,$9b,$87
		!byte $a7,$87,$a7,$87,$a7,$87,$9b,$87,$a7,$87,$a7,$87,$a7,$87,$9b,$87
		!byte $a7,$87,$a7,$87,$bf
s16 = *
		!byte $c2,$f3,$7b,$02,$f0,$7b,$a9,$60,$02,$f5,$82,$02,$f3,$76,$02,$7b
		!byte $02,$f0,$7b,$a9,$60,$02,$f5,$82,$02,$f3,$76,$02,$77,$02,$f0,$77
		!byte $a9,$60,$02,$f5,$7e,$02,$f3,$72,$02,$77,$02,$f0,$77,$a9,$60,$02
		!byte $cb,$f5,$7e,$02,$ca,$f3,$72,$02,$bf
s17 = *
		!byte $f7,$00,$c4,$f0,$36,$60,$04,$5f,$35,$f1,$60,$04,$f0,$5f,$35,$5f
		!byte $36,$f4,$60,$04,$f0,$5f,$35,$f2,$60,$04,$f0,$5f,$31,$f1,$60,$04
		!byte $f0,$5f,$2e,$f3,$60,$04,$f0,$5f,$31,$f2,$60,$04,$f0,$5f,$33,$f5
		!byte $60,$04,$f0,$5f,$32,$5f,$33,$f3,$60,$04,$f2,$60,$05,$f0,$60,$dc
		!byte $fa,$01,$bf
s18 = *
		!byte $f5,$00,$c4,$f3,$93,$04,$f0,$5f,$35,$f4,$60,$04,$f0,$5f,$36,$f2
		!byte $60,$04,$f0,$5f,$35,$60,$04,$5f,$33,$f5,$60,$04,$f8,$60,$04,$f3
		!byte $95,$04,$f0,$01,$02,$5f,$36,$f2,$60,$04,$f0,$5f,$38,$f2,$60,$04
		!byte $f0,$5f,$36,$60,$04,$5f,$35,$f4,$60,$04,$f0,$5f,$33,$f2,$60,$04
		!byte $bf
s19 = *
		!byte $cd,$f0,$5f,$47,$60,$06,$f8,$5f,$46,$f0,$60,$07,$f3,$5f,$4b,$f2
		!byte $60,$04,$01,$f0,$62,$08,$f1,$5f,$49,$f5,$60,$04,$f0,$60,$08,$f1
		!byte $5f,$4e,$f0,$61,$dc,$fc,$60,$04,$ff,$00,$bf
s1a = *
		!byte $ca,$f1,$22,$c3,$22,$c5,$2e,$c9,$22,$ca,$1d,$c3,$1d,$c5,$1d,$c3
		!byte $1d,$ca,$22,$c3,$22,$c5,$2e,$c9,$22,$ca,$1d,$c3,$1d,$c5,$1d,$c3
		!byte $1d,$ca,$1b,$c3,$1b,$c5,$27,$c9,$1b,$ca,$16,$c3,$16,$c5,$16,$c3
		!byte $16,$ca,$1b,$c3,$1b,$c5,$27,$c9,$1b,$ca,$16,$c3,$16,$c5,$16,$c3
		!byte $16,$bf
s1b = *
		!byte $c4,$f0,$5f,$31,$f6,$60,$04,$ff,$60,$05,$f0,$5f,$2e,$60,$04,$5f
		!byte $31,$f4,$60,$04,$f0,$5f,$33,$f9,$60,$04,$fd,$60,$05,$f2,$01,$f1
		!byte $91,$04,$01,$bf
s1c = *
		!byte $c8,$ff,$8e,$82,$00,$ff,$87,$82,$00,$bf
s1d = *
		!byte $ca,$f1,$23,$c3,$23,$c5,$2f,$c9,$23,$ca,$1e,$c3,$1e,$c5,$1e,$c3
		!byte $1e,$ca,$23,$c3,$23,$c5,$2f,$c9,$23,$ca,$22,$c3,$1e,$c5,$1e,$c3
		!byte $1e,$ca,$20,$c3,$20,$c5,$2c,$c9,$20,$ca,$1b,$c3,$1b,$c5,$1b,$c3
		!byte $1b,$ca,$20,$c3,$20,$c5,$2c,$c9,$20,$ca,$1b,$c3,$1b,$c5,$1b,$c3
		!byte $1b,$bf
s1e = *
		!byte $c4,$f5,$91,$04,$f1,$8f,$04,$f0,$8f,$04,$ff,$00,$fe,$60,$05,$f3
		!byte $8f,$04,$f2,$8f,$04,$f0,$60,$07,$5f,$9a,$07,$60,$07,$60,$07,$60
		!byte $04,$f3,$9a,$04,$f1,$98,$04,$f5,$96,$04,$bf
s1f = *
		!byte $c8,$ff,$8f,$81,$00,$ff,$8c,$82,$00,$bf
s20 = *
		!byte $ca,$f1,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$25,$c3,$25,$c5,$25,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c9
		!byte $20,$ca,$25,$c3,$25,$c5,$25,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c9
		!byte $20,$bf
s21 = *
		!byte $c4,$f0,$5f,$38,$f1,$60,$04,$f4,$60,$05,$fb,$60,$01,$f0,$5f,$33
		!byte $f2,$60,$04,$f0,$5f,$36,$f2,$60,$04,$f0,$5f,$38,$f6,$60,$04,$f1
		!byte $5f,$9a,$07,$f5,$60,$04,$f0,$5f,$38,$f2,$60,$04,$f7,$60,$05,$61
		!byte $01,$bf
s22 = *
		!byte $c8,$ff,$91,$83,$00,$91,$84,$fb,$91,$81,$f3,$01,$bf
s23 = *
		!byte $ca,$f1,$7e,$f1,$c3,$1e,$c5,$2a,$c9,$1e,$ca,$1e,$c3,$1e,$c5,$1e
		!byte $c3,$1e,$ca,$1e,$c3,$2a,$c5,$1e,$c9,$1e,$ca,$1e,$c3,$2a,$c5,$1e
		!byte $c3,$2a,$ca,$1e,$c3,$1e,$c5,$2a,$c9,$1e,$ca,$1e,$c3,$1e,$c5,$1e
		!byte $c3,$1e,$ca,$1e,$c3,$2a,$c5,$1e,$c9,$1e,$ca,$1e,$c3,$2a,$c5,$1e
		!byte $c3,$2a,$bf
s24 = *
		!byte $c6,$f0,$41,$f1,$5f,$a2,$06,$f0,$61,$da,$f2,$60,$04,$fc,$60,$05
		!byte $f3,$9f,$04,$f0,$5f,$3d,$f2,$60,$04,$f0,$5f,$3a,$f2,$60,$04,$f0
		!byte $31,$5f,$93,$07,$f1,$60,$08,$5f,$31,$f0,$5f,$2d,$f1,$5f,$2f,$fa
		!byte $61,$04,$f0,$a4,$04,$5f,$46,$5f,$44,$f2,$60,$04,$f0,$5f,$42,$f2
		!byte $60,$04,$f1,$01,$bf
s25 = *
		!byte $f0,$00,$cd,$41,$f1,$5f,$a2,$06,$f0,$61,$da,$f2,$60,$04,$fc,$60
		!byte $05,$f3,$9f,$04,$f0,$5f,$3d,$f2,$60,$04,$f0,$5f,$3a,$f2,$60,$04
		!byte $f0,$31,$5f,$93,$07,$f1,$60,$08,$5f,$31,$f0,$5f,$2d,$f1,$5f,$2f
		!byte $fa,$61,$04,$f0,$a4,$04,$5f,$46,$5f,$44,$f2,$60,$04,$f0,$5f,$42
		!byte $f2,$60,$04,$f0,$01,$bf
s26 = *
		!byte $c4,$f0,$5f,$38,$f1,$60,$04,$f4,$60,$05,$f7,$60,$01,$f3,$9a,$04
		!byte $f0,$5f,$38,$f2,$60,$04,$f0,$5f,$36,$f2,$60,$04,$f0,$5f,$38,$f6
		!byte $60,$04,$f0,$5f,$96,$df,$f6,$60,$04,$fb,$60,$05,$f6,$01,$bf
s27 = *
		!byte $c6,$f0,$41,$f1,$5f,$a2,$06,$f0,$61,$da,$f3,$60,$04,$fd,$60,$05
		!byte $f1,$a6,$04,$f0,$45,$5f,$46,$f1,$61,$04,$f0,$a6,$04,$f1,$01,$f0
		!byte $a5,$04,$f1,$5f,$46,$61,$03,$a4,$04,$a2,$04,$f0,$a1,$04,$5f,$42
		!byte $f1,$9f,$04,$a2,$04,$9f,$04,$a2,$04,$9f,$04,$a2,$04,$f0,$a3,$04
		!byte $5f,$44,$f1,$60,$04,$f5,$61,$05,$bf
s28 = *
		!byte $f0,$00,$cd,$a1,$04,$5f,$42,$60,$04,$61,$da,$f3,$60,$04,$fd,$60
		!byte $05,$f1,$a6,$04,$f0,$a5,$04,$5f,$46,$f1,$61,$04,$f0,$a6,$04,$f1
		!byte $01,$f0,$a5,$04,$f1,$5f,$46,$01,$a4,$04,$a2,$04,$f0,$a1,$04,$5f
		!byte $42,$f1,$9f,$04,$a2,$04,$9f,$04,$a2,$04,$9f,$04,$a2,$04,$f0,$a3
		!byte $04,$5f,$44,$f1,$60,$04,$f4,$61,$05,$bf
s29 = *
		!byte $c6,$f0,$4d,$f3,$5f,$ae,$06,$f0,$61,$04,$60,$05,$f4,$60,$01,$c0
		!byte $f1,$96,$04,$f3,$01,$f1,$96,$04,$01,$96,$04,$01,$96,$04,$01,$96
		!byte $04,$c6,$93,$04,$5f,$36,$5f,$38,$f0,$5f,$3b,$f4,$60,$04,$f1,$ab
		!byte $04,$5f,$4e,$5f,$50,$f0,$5f,$53,$60,$04,$f1,$01,$f3,$60,$04,$ca
		!byte $f1,$1e,$cb,$f3,$1e,$bf
s2a = *
		!byte $c0,$f1,$3a,$f3,$01,$f0,$3a,$f4,$01,$f1,$3a,$f3,$01,$f1,$3a,$01
		!byte $3a,$01,$3a,$01,$3a,$36,$f3,$01,$f0,$36,$f4,$01,$f1,$36,$f3,$01
		!byte $f1,$36,$01,$36,$01,$36,$01,$36,$bf
s2b = *
		!byte $c0,$f1,$3d,$3d,$f0,$3d,$01,$f1,$9d,$04,$3d,$3d,$f0,$3d,$01,$f1
		!byte $3d,$3d,$9d,$04,$3d,$9d,$04,$3d,$9d,$04,$3d,$9d,$04,$9b,$04,$3b
		!byte $3b,$9b,$04,$3b,$3b,$9b,$04,$3b,$3b,$9b,$04,$3b,$9b,$04,$3b,$9b
		!byte $04,$3b,$9b,$04,$bf
s2c = *
		!byte $c8,$ff,$91,$83,$00,$ce,$f0,$9d,$04,$f4,$01,$f0,$3f,$f4,$61,$04
		!byte $f0,$42,$f4,$61,$04,$f0,$44,$f4,$61,$04,$f0,$3f,$f2,$01,$f0,$3d
		!byte $f2,$61,$04,$bf
s2d = *
		!byte $cb,$f5,$25,$20,$25,$20,$f1,$25,$25,$26,$26,$bf
s2e = *
		!byte $ce,$f0,$9d,$04,$f4,$01,$f0,$9f,$04,$f4,$01,$f0,$a2,$04,$f4,$01
		!byte $f0,$a6,$04,$f4,$01,$f0,$a9,$01,$f2,$01,$f0,$aa,$01,$f2,$01,$bf
s2f = *
		!byte $ce,$f0,$a9,$04,$f4,$01,$f0,$ab,$04,$f4,$01,$f0,$ae,$04,$f4,$01
		!byte $f0,$b0,$04,$f4,$01,$f0,$b7,$01,$f2,$01,$f0,$b8,$01,$f2,$01,$bf
s30 = *
		!byte $ca,$f1,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$f5,$25,$25,$cb,$20,$ca,$25,$f3,$20,$cb,$f1,$20,$c9,$20
		!byte $bf
s31 = *
		!byte $ca,$f1,$7e,$f1,$c3,$1e,$c5,$2a,$c9,$1e,$ca,$1e,$c3,$1e,$c5,$1e
		!byte $c3,$1e,$ca,$1e,$c3,$2a,$c5,$1e,$c9,$1e,$ca,$1e,$c3,$2a,$c5,$1e
		!byte $c3,$2a,$ca,$1e,$c3,$1e,$c5,$2a,$c9,$1e,$ca,$1e,$c3,$1e,$c5,$1e
		!byte $c3,$1e,$cb,$19,$c2,$f3,$19,$f5,$1b,$f3,$19,$bf
s32 = *
		!byte $ca,$f1,$25,$c3,$25,$c5,$31,$c9,$25,$ca,$20,$c3,$20,$c5,$20,$c3
		!byte $20,$ca,$f3,$85,$f3,$fb,$25,$fe,$1e,$ff,$00,$bf
s33 = *
		!byte $c8,$ff,$91,$83,$c7,$f3,$91,$89,$c8,$fb,$91,$89,$c7,$f0,$96,$83
		!byte $f3,$01,$c8,$f0,$96,$83,$f4,$01,$c7,$f0,$96,$83,$f4,$01,$f0,$96
		!byte $83,$f4,$01,$f0,$96,$83,$f2,$01,$c8,$f0,$96,$83,$f2,$01,$bf
s34 = *
		!byte $ca,$fe,$1e,$ff,$00,$fd,$1e,$ff,$00,$bf
s35 = *
		!byte $fe,$00,$ff,$00,$cd,$f0,$41,$5f,$42,$f6,$60,$04,$f3,$60,$05,$01
		!byte $f0,$a6,$04,$60,$06,$f3,$5f,$47,$f0,$60,$04,$f5,$5f,$46,$bf
s36 = *
		!byte $c7,$f0,$9b,$87,$f3,$01,$c8,$f0,$9b,$87,$f4,$01,$c7,$f0,$9b,$87
		!byte $f4,$01,$f0,$9b,$87,$f3,$01,$f0,$9b,$87,$f2,$01,$c8,$f0,$9b,$87
		!byte $f3,$01,$c7,$f0,$96,$83,$f3,$01,$c8,$f0,$96,$83,$f4,$01,$c7,$f0
		!byte $96,$83,$f4,$01,$f0,$96,$83,$f3,$01,$f0,$96,$83,$f2,$01,$c8,$f0
		!byte $96,$83,$f2,$01,$bf
s37 = *
		!byte $ca,$ff,$1e,$00,$f0,$12,$f1,$60,$db,$fc,$01,$ff,$00,$bf
chord		!byte $04,$03,$02,$02,$80,$00,$04,$07,$85,$00,$03,$07,$89,$00,$02,$07
		!byte $8d,$00,$05,$07,$91,$7b,$03,$00,$95,$7b,$77,$00,$99,$7b,$78,$00
		!byte $9d,$7b,$00,$04,$a1,$00,$02,$05,$07,$a5
chordindex		!byte $00,$05,$09,$0d,$11,$15,$19,$1d,$21,$25
