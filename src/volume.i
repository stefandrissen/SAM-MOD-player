; calculate volume tables

; initially calculated by burstplayer
; recalculated with amplification by demo 

;---------------------------------------------------------------
;create volume tables for burstplayer (Amiga samples)
;B = number of bits

populate.volume.table:

	ld ix,volume.table
	push ix

	ld a,b
	cp 3
	jr nz,@not.saa
				
	ld c,16				; volume tables

	ld a,1
	ld (cv.skip.table+2),a

	ld a,opcode_nop

	jr @continue
	
@not.saa:	

	ld c,32				; volume tables

	xor a
	ld (cv.skip.table+2),a

	ld a,opcode_ret

@continue:

	ld (cv.no.double),a

	ld a,c
	ld (cv.max.tables+1),a
	dec a
	ld (cv.div.by+1),a

	dec b
	ld a,1
cv.getbits:
	rla
	djnz cv.getbits
	ld (cv.vol.base.1+2),a
	ld (cv.vol.base.2+2),a

	rla
	ld (cv.vol.bits+1),a
	ld a,0
	adc a,0
	ld (cv.vol.bits+2),a

	xor a
	ld (cv.volume+1),a

cv.loop:
cv.vol.bits:
	ld de,0				; DE=2^bits
	ld h,d
	ld l,e
	dec hl				; HL=2^bits-1
	ld a,l
	ld (max.vol+1),a

	ld hl,0

cv.volume:
	ld a,0				; [0-31] / [0-15]
	or a
	jr z,cv.no.mul
	ld b,a
cv.mul.vol:
	add hl,de
	djnz cv.mul.vol
						; HL=vol*2^bits

;here we need to ensure that HL gets multiplied by the factor!

	ex de,hl
amp.fac:
	ld bc,&0100			; amplification factor
	xor a
	ld h,a
	ld l,a
	ld (rest+1),a
mulamp:
rest:
	ld a,0
	add c
	ld (rest+1),a
	ld a,b
	adc a,l
	ld l,a
	jr nc,$+3
	inc h

	dec de
	ld a,d
	or e
	jr nz,mulamp
;
cv.no.mul:

	ld b,h
	ld c,l
cv.div.by:
	ld de,15			; tables-1
	call cv.bc.div.de
	ld (cv.range+1),bc

cv.vol.base.1:
	ld hl,&0800			; H=2^(bits-1) "central" vol.
	ld b,128

;2^bits * v/15

cv.range:
	ld de,15			; range (step)

cv.blp:
	ld (ix),h
	inc ix

	add hl,de

	ld a,h
max.vol:
	sub 0				; maximum volume (2^bits-1)
	jr z,$+4
	jr c,not.max
	ld a,(max.vol+1)
	ld h,a				; h=maximum volume
	ld de,0				; reset adder
not.max:

	djnz cv.blp

	ld c,127
	add ix,bc

	ld de,(cv.range+1)

cv.vol.base.2: 
	ld hl,&0800			; "central" volume
	ld b,128
cv.blp2:
	or a
	sbc hl,de

	jr nc,not.min
	ld h,0				; minimum volume
	ld d,h				; reset adder
	ld e,h
not.min:
	ld (ix),h
	dec ix
	djnz cv.blp2

cv.skip.table: 
	ld bc,129			; B=1 if SAA
	add ix,bc

	ld a,(cv.volume+1)
	inc a
	ld (cv.volume+1),a
cv.max.tables: 
	cp 0
	jp nz,cv.loop

	pop hl
cv.no.double:
	ret					; NOP if SAA

;------------------------------------------------------------------------------	
;copy tables with stereo flip for SAA1099

	ld d,h
	ld e,l
	inc d
	ld bc,16
cv.saa.one:
	ld a,(hl)
	add a,a				; correct 3 bits to effective bits (1-4)
	ld (hl),a
	inc hl
	add a,a
	add a,a
	add a,a
	add a,a
	ld (de),a
	inc de
	djnz cv.saa.one
	inc d
	inc h
	dec c
	jr nz,cv.saa.one
	ret

;------------------------------------------------------------------------------	
cv.bc.div.de:

	ld a,b			; divide BC by DE
	ld b,16			; result in BC
	ld hl,0			; DE is unchanged
cv.clcd1:
	rl c
	rla
	adc hl,hl
	sbc hl,de
	jr nc,cv.clcd2
	add hl,de
cv.clcd2:
	ccf
	djnz cv.clcd1
	rl c
	rla
	ld b,a
	ret

