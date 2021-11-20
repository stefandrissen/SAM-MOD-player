;SAM MOD player - b-dos loader

;(C) 2019-2021 Stefan Drissen

; - Record list size (in sectors) = (all sectors / 1600 + 32) / 32
; - Selected record (disk drive = 0) = (PEEK DVAR 7 = 2) * DPEEK DVAR 25

;@sector: equ fat
@sector: equ dos.sector


;-------------------------------------------------------------------------------
read.directory.bdos:

    push de
    call bdos.read.dir

    ; now convert the SAM stuff to loader format

    pop de
    ld hl,fat

    ld a,80             ; 80 directory entries
@loop.directory.entries:

    push af

    ld a,(hl)
    and %00111111
    cp uifa.filetype.code
    jr nz,@next.file

    push hl
    call @check.file.extension
    pop hl
    jr nz,@next.file

    push hl
    pop ix

    push hl
    push de

    inc hl
    ld b,8
@loop:
    ld a,(hl)
    ld (de),a
    inc hl
    inc de

    djnz @-loop

    push de

    ld e,(ix+uifa.sector)
    ld d,(ix+uifa.track)    ; first sector

    in a,(port.hmpr)
    and high.memory.page.mask
    ld c,a

    call @read.first.sectors
    jr z,@next.file

    pop de

    ld a,2
    call file.check

    call fc.sam
    jr z,sm.file.ok

;not MOD, maybe compressed mod (4 bit)

    pop de
    push de

    ld hl,8
    add hl,de
    ex de,hl

    ld a,1              ; only add sample length once
    call file.check

    call fc.sam
    jr z,sm.file.ok

    pop de
    pop hl
    jr @next.file

sm.file.ok:

    call @insert.file.date

    for 6, inc de

    call insert.file.size

    ld hl,loader.entries
    inc (hl)
    pop hl
    ld bc,load.len
    add hl,bc
    ex de,hl
    pop hl

@next.file:

    ld bc,16
    add hl,bc

    pop af
    ld b,a
    ld a,(loader.entries)
    cp 27
    ret z

    ld a,b
    dec a
    jr nz,@-loop.directory.entries

    ret

;-------------------------------------------------------------------------------
@check.file.extension:

    inc hl
    inc hl

    ld b,8

@loop:

    ld a,(hl)
    inc hl
    cp "."
    jr nz,@next.chr

    ld a,(hl)
    set 5,a             ; -> lowercase
    cp "m"
    jr nz,@next.chr

    ret

@next.chr:
    djnz @-loop

    ld a,1
    or a                ; nz

    ret

;-------------------------------------------------------------------------------
@read.first.sectors:

; read first 3 sectors of file to check if it is a mod

    ld hl,temp.spc
    call @read.sector
    ret z

    ld hl,temp.spc + 510
    call @read.sector
    ret z

    ld hl,temp.spc + 2 * 510
    call @read.sector

    ld a,1
    or  a   ; -> NZ

    ret

;-------------------------------------------------------------------------------
@read.sector:
; input:
; - hl = destination
; - d  = track
; - e  = sector
;
; output:
; - z = no next sector

    push hl

    ld hl,dos.sector
    call bdos.read.sector

    pop de

    ld hl,dos.sector
    ld bc,510
    ldir
    ld d,(hl)
    inc hl
    ld e,(hl)
    ld a,d
    or e

    ret

;-------------------------------------------------------------------------------
@insert.file.date:

    push de
    ld a,"*"
    ld (de),a

sm.check.date:
    ld a,(ix+11)
    or a
    jr z,sm.done.date       ; 0 -> invalid date
    cp 32
    jr nc,sm.done.date      ; day > 31 = invalid date
    ld a,(ix+12)
    or a
    jr z,sm.done.date       ; 0 -> invalid date
    cp 13
    jr nc,sm.done.date      ; month > 12 = invalid date
    ld a,(ix+15)
    or a
    jr z,sm.done.date       ; 0 -> invalid date
    inc a
    jr z,sm.done.date       ; 255 -> invalid date

    ld a,(ix+11)
    call cnv.a.to.de
    ld a,(ix+12)
    call cnv.a.to.de
    ld a,(ix+15)
    call cnv.a.to.de
sm.done.date:
    pop de

    ret

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
    ld de,fat+1
    ld bc,16 * 80 - 1
    ld (hl),0
    ldir

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

    ld a,(de)   ; filetype
    cp uifa.filetype.code
    ret nz

    ex de,hl

    ld bc,0x0b
    ldir        ; filetype + filename

    ld a,(hl)   ; msb sectors
    or a
    jr nz,@ok

    inc l
    ld a,(hl)   ; lsb sectors
    dec l
    cp 5        ; smallest mod is 2108 bytes (https://sitomani.github.io/4champ/2020ds/ds_06.html)
    jr c,@file.too.small

 @ok:

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

 @file.too.small:

    ex de,hl

    ld c,0x0b
    or a
    sbc hl,bc
    ld (hl),0

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
    res 7,(hl)
    ld bc,10
    ldir

    ld hl,mes.no_label + 10      ; 6 spaces
    ld a,(loader.dos.version)
    cp dvar.version.b_dos.max + 1
    jr nc,@not.bdos

    ld hl,@sector+uifa.diskname.b_dos

@not.bdos:
    ld bc,6
    ldir

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
