;SAM MOD player - b-dos loader

;(C) 2019-2021 Stefan Drissen

; - Record list size (in sectors) = (all sectors / 1600 + 32) / 32
; - Selected record (disk drive = 0) = (PEEK DVAR 7 = 2) * DPEEK DVAR 25

;@sector: equ fat
@sector: equ dos.sector

;-------------------------------------------------------------------------------
bdos.read.dir:

; first read in SAM directory

; read directory sectors and copy relevant 16 bytes per entry to screen memory
; for processing
;-------------------------------------------------------------------------------

    in a,(port.hmpr)
    and high.memory.page.mask
    ld c,a

    ld hl,fat
    ld d,0      ; track

@loop.tracks:
    ld e,1      ; sector

@loop.sectors:

    push hl
    ld hl,@sector
    call bdos.read.sector
    pop hl

    ld a,d
    dec e
    or e
    call z,@get.volume.label
    inc e

    push de

    ld de,@sector
    call @get.entry

    ld de,@sector + 0x100
    call @get.entry

    pop de

    inc e
    ld a,e
    cp 11
    jr nz,@-loop.sectors

    inc d
    ld a,d
    cp 4
    jr nz,@-loop.tracks

    ret

;-------------------------------------------------------------------------------
@get.entry:
; move 16 bytes into directory structure
;
; 0x00  filetype
; 0x01  filename
; 0x0b  day      <-
; 0x0c  month    <-
; 0x0d  track
; 0x0e  sector
; 0x0f  year     <-
;
; input:
;   de = @sector
;   hl = directory store

    ex de,hl

    ld bc,0x0b
    ldir        ; filetype + filename
    ld c,uifa.timestamp.day - 0x0b
    add hl,bc
    ldi         ; day
    inc c
    ldi         ; month
    inc c
    or a
    sbc hl,bc
    ldi         ; track
    ldi         ; sector
    add hl,bc
    ldi         ; year

    ex de,hl

    ret

;-------------------------------------------------------------------------------
@get.volume.label:

    push hl
    push de

    ld de,m.vollabel
    ld hl,@sector+uifa.diskname
    ld a,(hl)
    cp "*"
    jr nz,@has.label
    ld (hl),0
@has.label:
    ld bc,10
    ldir
    ld a," "
    ld (de),a

    pop de
    pop hl

    ret

;-------------------------------------------------------------------------------


if defined (debug)

    text.track.sector:
            defm "T:"
    @trk:   defm "00"
            defm "S:"
    @sec:   defm "00"
            defb 0

endif


;-------------------------------------------------------------------------------
bdos.read.sector:

; read physical sector from disc
;   d = track (+128 for side 2)
;   e = sector
;   hl= address
;-------------------------------------------------------------------------------

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

    rst 8
    defb dos.hrsad

    di

    pop hl
    inc h
    inc h
    pop de
    pop bc
    pop af
    pop ix

;   ei
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

;-------------------------------------------------------------------------------
bdos.get.dvar:

; get dvar value and return it in a
;   a = dvar to get
;-------------------------------------------------------------------------------

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

@get.dvar.len:  equ $ - @get.dvar

    org @store.org + @get.dvar.len + 2

    ret

;-------------------------------------------------------------------------------
bdos.get.dvar.word:

; get dvar word value and return it in hl
;   a = dvar to get
;-------------------------------------------------------------------------------
    push af
    call bdos.get.dvar
    ld l,a
    pop af
    inc a
    call bdos.get.dvar
    ld h,a

    ret

;-------------------------------------------------------------------------------
bdos.select.record.hl:

; select record hl
;-------------------------------------------------------------------------------

    push bc

    ld a,0
    rst 8
    defb dos.hrecord

    pop bc

    ret
