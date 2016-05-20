;***** SID-Wizard labels *****


PLAYERADDR=$1000 ;the address of the included SID-Wizard music-player
inisub=PLAYERADDR+0
playsub=PLAYERADDR+3
SFXsub=PLAYERADDR+12

varpos=PLAYERADDR+$1e ;variables start here - must be fixed address from now on...
;variables for channel1, (for other channels 7 / 14 must be added to the address):
SPDCNT=varpos+1*3*7+1 ;speed/tempo-counter (to grab finer-grade moments)

SEQPOS1=varpos+1*3*7+2		;sequence-position on channel 1 - useful for demo-timing
SEQPOS2=varpos+1*3*7+2+7 	;sequence-position on channel 2 - useful for demo-timing
SEQPOS3=varpos+1*3*7+2+14	;sequence-position on channel 3 - useful for demo-timing

CURPTN1=varpos+2*3*7+0		;currently played pattern (for demo maybe)
CURPTN2=varpos+2*3*7+0+7	;currently played pattern (for demo maybe)
CURPTN3=varpos+2*3*7+0+14	;currently played pattern (for demo maybe)

CURNOT1=varpos+2*3*7+1		;current note
CURNOT2=varpos+2*3*7+1+7	;current note
CURNOT3=varpos+2*3*7+1+14	;current note

CURIFX1=varpos+2*3*7+3		;current instrument/FX column value
CURIFX2=varpos+2*3*7+3+7	;current instrument/FX column value
CURIFX3=varpos+2*3*7+3+14	;current instrument/FX column value

CURINS1=varpos+2*3*7+4		;currently selected instrument, must be changed temporarily
CURINS2=varpos+2*3*7+4+7	;currently selected instrument, must be changed temporarily
CURINS3=varpos+2*3*7+4+14	;currently selected instrument, must be changed temporarily

CURFX2=varpos+2*3*7+5 ;small pattern-effect can be used to control volume of SFX

PTNGATE1=varpos+0*3*7+5		;gate off/on status controlled by pattern (and mute/solo in editor?)
PTNGATE2=varpos+0*3*7+5+7	;gate off/on status controlled by pattern (and mute/solo in editor?)
PTNGATE3=varpos+0*3*7+5+14	;gate off/on status controlled by pattern (and mute/solo in editor?)

AD1=varpos+5*3*7+5			;sid's attack/decay ghost-register
AD2=varpos+5*3*7+5+7		;sid's attack/decay ghost-register
AD3=varpos+5*3*7+5+14		;sid's attack/decay ghost-register

SR1=varpos+5*3*7+6			;sid's sustain/release ghost-register
SR2=varpos+5*3*7+6+7		;sid's sustain/release ghost-register
SR3=varpos+5*3*7+6+14		;sid's sustain/release ghost-register
