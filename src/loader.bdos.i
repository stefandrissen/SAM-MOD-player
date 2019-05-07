;SAM MOD player - b-dos loader 

;(C) 2019 Stefan Drissen

; - Record list size (in sectors) = (all sectors / 1600 + 32) / 32
; - Selected record (disk drive = 0) = (PEEK DVAR 7 = 2) * DPEEK DVAR 25

;---------------------------------------------------------------
bdos.read.dir:

; first read in SAM directory
;---------------------------------------------------------------

	in a,(port.hmpr)
	and high.memory.page.mask
	ld c,a

	ld de,&0001
	ld hl,fat
	
cs.rd.lp:
	push bc
	push de
	push hl
	
	call bdos.read.sector
	pop hl

	ld a,d
	or a
	jr nz,cs.notfirst
	dec e
	jr nz,cs.notfirst
	
	push hl
	ld de,m.vollabel
	ld hl,fat+210
	ld a,(hl)
	cp "*"
	jr nz,$+4
	ld (hl),0
	ld bc,10
	ldir
	ld a," "
	ld (de),a
	pop hl
cs.notfirst:
	push hl
	push hl
	pop de
	ld bc,245
	add hl,bc
	ld a,e
	add 11
	ld e,a
	jr nc,$+3
	inc d
	ldi
	ldi
	inc de
	inc de
	ldi
	pop hl

	ld bc,16
	ld e,l
	ld d,h
	ex de,hl
	add hl,bc
	ex de,hl
	inc h

	push de
	push hl
	push hl
	pop de
	ld bc,245
	add hl,bc
	ld a,e
	add 11
	ld e,a
	jr nc,$+3
	inc d
	ldi
	ldi
	inc de
	inc de
	ldi
	pop hl
	pop de

	ld bc,16
	ldir
	ex de,hl
	pop de
	pop bc
	inc e
	ld a,e
	cp 11
	jr nz,cs.rd.lp
	ld e,1
	inc d
	ld a,d
	cp 4
	jr nz,cs.rd.lp

	ret

if defined (debug)

	text.track.sector:	
			defm "T:"
	@trk:	defm "00"
			defm "S:"
	@sec:	defm "00"
			defb 0
							
endif


;---------------------------------------------------------------
bdos.read.sector:

; read physical sector from disc
;	d = track (+128 for side 2)
;	e = sector
;	hl= address
;---------------------------------------------------------------

	di
	
	push ix
	push af
	push bc
	push de
	push hl
	
if defined(debug)	
	
	push hl
	push de 
	push bc
	
	ld hl,video.memory.high + 30 * video.memory.32.rows 
	
	ld a,"T"
	call print.chr
	ld a,":"
	call print.chr
	ld a,d
	call print.num
	
	ld a," "
	call print.chr
	
	ld a,"S"
	call print.chr
	ld a,":"
	call print.chr
	ld a,e
	call print.num

	pop bc	
	pop de
	pop hl
	
endif
	
	ld hl,@dos.exit.routine
	ld hl,0
	ld (svar.doser),hl
	
	pop hl
	push hl

	ld a,(loader.drive)
	ld ix,1
	
	rst 8
	defb dos.hmrsad	
	
	di
	
	pop hl
	inc h
	inc h
	pop de
	pop bc
	pop af
	pop ix
	
;	ei
	ret

@dos.exit.routine:

	ld hl,0
	ld (svar.doser),hl

	or a
	jr nz,@dos.error
	
	ret	

@dos.error:	
	di
	ld bc,port.border
	
@loop:	
	out (c),a
	out (c),b
	
	jr @loop
	
	push af

@nokey:	
	xor a
	in a,(port.keyboard)
	and %00011111
	cp %00011111
	jr nz,@nokey

@anykey:	
	xor a
	in a,(port.keyboard)
	and %00011111
	cp %00011111
	jr z,@anykey
	
	pop af

	ret

;---------------------------------------------------------------
bdos.get.dvar:

; get dvar value and return it in a
;	a = dvar to get
;---------------------------------------------------------------

	call relocate.low

@store.org:

	defw @get.dvar.len

	org inst.buffer

@get.dvar:	
	push hl
	push de
	
	ld e,a
	
	in a,(port.hmpr)
	ld (@store.hmpr+1),a
	
	ld a,(svar.dosflg)
	out (port.hmpr),a
	
	ld hl,(bdos.dvars)
	ld d,0
	add hl,de
	ld e,(hl)	
	
@store.hmpr:
	ld a,0
	out (port.hmpr),a
	
	ld a,e
	
	pop de
	pop hl	
	
	ret
	
@get.dvar.len:	equ $ - @get.dvar	

	org @store.org + @get.dvar.len + 2

	ret
	
;---------------------------------------------------------------
bdos.select.record.hl:

; select record hl
;---------------------------------------------------------------

    push bc
    
    ld a,0
    rst 8
    defb dos.hrecord
    
    pop bc

    ret
	