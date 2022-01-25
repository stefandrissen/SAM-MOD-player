;SAM MOD player - b-dos loader

;(C) 2019-2022 Stefan Drissen

; - Record list size (in sectors) = (all sectors / 1600 + 32) / 32
; - Selected record (disk drive = 0) = (PEEK DVAR 7 = 2) * DPEEK DVAR 25

;@sector: equ fat
@sector: equ dos.sector


;-------------------------------------------------------------------------------
bdos.directory.read:

    push de
    call bdos.read.dir

    ; now convert the SAM stuff to loader format

    pop de
    ld hl,loader.directory

    ld a,80             ; 80 directory entries

    @loop.directory.entries:

        push af

        ld a,(hl)
        or a
        jr z,@leave

        push hl
        pop ix

        push hl
        push de

        ld b,8

        @loop:

            ld a,(hl)
            ld (de),a
            inc hl
            inc de

            djnz @-loop

        push de

        ld e,(ix + loader.directory.size_kb + 0)
        ld d,(ix + loader.directory.size_kb + 1)
        ld (file.size_kb + 1),de

        ld e,(ix + loader.directory.sector)
        ld d,(ix + loader.directory.track)  ; first sector

        in a,(port.hmpr)
        and high.memory.page.mask
        ld c,a

        call @read.first.sectors
        pop de
        jr z,@next.file.pop

        push ix
        push de
        ld hl,screen.free + 9
        call mod.determine.type
        pop de
        pop ix

        cp mod.type.invalid
        jr nz,@file.valid

     @next.file.pop:

        pop de
        pop hl
        jr @next.file

     @file.valid:

        ld hl,screen.free + 9 + mod.sample.title
        ld bc,mod.title.len
        ldir

        ld (de),a               ; type
        inc de

        call mod.get.patterns.a
        ld (de),a               ; patterns
        inc de

        call @insert.file.date

        for 8, inc de

        call insert.file.size

        ld hl,loader.entries
        inc (hl)
        pop hl
        ld bc,loader.dir.len
        add hl,bc
        ex de,hl
        pop hl

     @next.file:

        ld bc,loader.directory.len
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

 @leave:

    pop af
    ret

;-------------------------------------------------------------------------------

    include "mod.s"

;-------------------------------------------------------------------------------
@read.first.sectors:

    ; read first 3 sectors of file to check if it is a mod

    ld hl,screen.free
    call @read.sector
    ret z

    call @read.sector
    ret z

    call @read.sector

    ld a,1
    or a    ; -> NZ

    ret

;-------------------------------------------------------------------------------
@read.sector:
 ; input:
 ; - hl = destination
 ; - d  = track
 ; - e  = sector

 ; output:
 ; - hl = destination + 510
 ; - d  = next track
 ; - e  = next sector
 ; - z  = no next sector (de = 0)

    push hl

    ld hl,dos.sector
    call bdos.read.sector

    pop de

    ld hl,dos.sector
    ld bc,510
    ldir

    push de

    ld d,(hl)
    inc hl
    ld e,(hl)

    pop hl

    ld a,d
    or e

    ret

;-------------------------------------------------------------------------------
@insert.file.date:

 ; input
 ; - ix = directory entry (loader.directory format)

 ; output
 ; - (de) ddmmyyyy or * when invalid

    ld a,"*"
    ld (de),a

    ; day
    ld a,(ix + loader.directory.date.day)
    or a
    ret z                   ; = 0
    cp 32
    ret nc                  ; > 31

    ; month
    ld a,(ix + loader.directory.date.month)
    or a
    ret z                   ; = 0
    cp 13
    ret nc                  ; > 12

    ; year (100 -> 2000)
    ld a,(ix + loader.directory.date.year)
    or a
    ret z                   ; = 0
    inc a
    ret z                   ; = 255

    push de

    ld a,(ix + loader.directory.date.day)
    call cnv.a.to.de

    ld a,(ix + loader.directory.date.month)
    call cnv.a.to.de

    ld a,(ix + loader.directory.date.year)
    ld c,19
 @loop:
        sub 100
        jr c,@leave
        inc c
        jr @-loop

 @leave:

    add a,100
    push af
    ld a,c
    call cnv.a.to.de
    pop af
    call cnv.a.to.de

    pop de

    ret

;-------------------------------------------------------------------------------
bdos.read.dir:

 ; first read in SAM directory

 ; read directory sectors into loader.directory format for processing

    in a,(port.hmpr)
    and high.memory.page.mask
    ld c,a

    ld hl,loader.directory
    ld de,loader.directory + 1
    ld bc,loader.directory.len * 80 - 1
    ld (hl),0
    ldir

    ld hl,loader.directory

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
 ; move bytes into loader.directory structure

 ; input:
 ; - de = @sector
 ; - hl = directory store

    ld a,(de)   ; filetype
    cp samdos.filetype.code
    ret nz

    push hl

    ex de,hl

    ld l,samdos.dir.sectors
    ld a,(hl)   ; lsb sectors
    or a
    jr nz,@ok

    inc l
    ld a,(hl)   ; msb sectors
    cp 5        ; smallest mod is 2108 bytes (https://sitomani.github.io/4champ/2020ds/ds_06.html)
    jr c,@file.too.small

 @ok:

    ld l,samdos.dir.filename
    ld bc,loader.directory.filename.len
    ldir

    ld l,samdos.dir.track
    ldi         ; first track
    ldi         ; first sector

    ld l,samdos.dir.length.pages        ;   b        c
    ld b,(hl)                           ; 0b11111111            16 KB pages
    ld l,samdos.dir.length.bytes + 1
    ld c,(hl)                           ;          0b--111111   0x00 - 0x3f (* 0x100 B)
                                        ; 0b11111111 --111111

    sla c
    sla c                               ; 0b11111111 11111100

    srl b
    rr c                                ; 0b01111111 11111110
    srl b
    rr c                                ; 0b00111111 11111111

    ld a,c
    ld (de),a                           ; loader.directory.size_kb + 0
    inc de
    ld a,b
    ld (de),a                           ; loader.directory.size_kb + 1
    inc de

    ld l,samdos.dir.timestamp
    ldi ; day
    ldi ; month
    ldi ; year

    pop hl
    ex de,hl

    ret

 @file.too.small:

    pop hl
    ld (hl),0

    ret

;-------------------------------------------------------------------------------
@get.volume.label:

    push hl
    push de

    ld de,text.volume.label
    ld hl,@sector + samdos.dir.diskname
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

    ld hl,@sector + samdos.dir.diskname.b_dos

 @not.bdos:
    ld bc,6
    ldir

    pop de
    pop hl

    ret

;-------------------------------------------------------------------------------
bdos.read.sector:

    ; read physical sector from disc
    ;   d = track (+128 for side 2)
    ;   e = sector
    ;   hl= address (0x4000 to 0xfe00)

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

    ld hl,screen + 30 * screen.32.rows

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

    push bc

    ld a,0
    rst 8
    defb dos.hrecord

    pop bc

    ret
