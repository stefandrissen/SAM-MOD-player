;SAM MOD player - check mod type based on header

 ;(C) 2021 Stefan Drissen

 ; https://github.com/cmatsuoka/tracker-history/blob/master/reference/amiga/soundtracker/Ultimate_Soundtracker-format.txt
 ; http://helllabs.org/tracker-history/trackers.svg
 ; https://github.com/OpenMPT/openmpt/blob/master/soundlib/Load_mod.cpp

 ; (ultimate) soundtracker
 ; - samples: 15
 ; - sample length: in words (max 9999 bytes)
 ; - sample repeat, offset: in BYTES
 ; - sample: only loop is played (from offset)

 ; - pattern effects:
 ;      1 xy = arpeggio  -> PT 0 xy
 ;      2 0y = pitchbend -> PT 1 0y
 ;                       -> PT 2 0x

 ; soundtracker
 ; - samples: 15
 ; - sample length: bytes ?

 ; soundtracker 2.2:
 ; - samples: 31

 ; soundtracker 2.3:
 ; - id: M.K.

 ; soundtracker 2.5
 ; - repeat offset: words

 ; soundtracker 2.6
 ; - id: MTN

 ; noisetracker 1.0
 ; - samples: 31
 ; - repeat offset: words
 ; - id: N.T.

 ; protracker 2.3a
 ; - id: M!K!
 ; - patterns: > 64

;-------------------------------------------------------------------------------

    ld hl,0xc000
    jp mod.type

;-------------------------------------------------------------------------------

    @var.mod.type:      defb 0
    @var.mod.samples:   defb 0
    @var.mod.patterns:  defb 0
    @var.mod.offset:    defw 0
    @var.mod.page:      defb 0

;-------------------------------------------------------------------------------

mod.get.type.a:
    ld a,(@var.mod.type)
    ret

mod.get.samples.a:
    ld a,(@var.mod.samples)
    ret

mod.get.patterns.a:
    ld a,(@var.mod.patterns)
    ret

;-------------------------------------------------------------------------------

include "../constants/mod.i"

; volume
 ; https://www.un4seen.com/forum/?topic=14471.msg101020#msg101020
 ; SoundTracker sends volume directly to Paula, which means:
 ; - bit 7 ignored
 ; - bit 6 max volume
 ; for soundtracker through 2.4

; mod.types:
 mod.type.unknown:  equ 1
 mod.type.invalid:  equ 2

 ; 15 samples
 mod.type.ust.15:   equ 3   ; ultimate soundtracker max sample length 9999 bytes
 mod.type.st.15:    equ 4   ; soundtracker

 ; 31 samples
 mod.type.st22:     equ 5   ; soundtracker 2.2 max sample length 0x8000 bytes

                            ; id introduced
 mod.type.st23:     equ 6   ; M.K. soundtracker 2.3 / 2.4
                            ;      e00 | e01 = filter on/off
 mod.type.nt:       equ 7   ; N.T. noisetracker (based on st 2.3)
 mod.type.st25:     equ 8   ; M.K. soundtracker 2.5
                            ;      repeat offset in words
 mod.type.pt:       equ 9   ; M.K. protracker (based on st 2.5)
                            ;      exy extended effects
 mod.type.ptx:      equ 10  ; M!K! > 64 patterns (introduced in pt 2.3a)
 mod.type.flt:      equ 11  ; FLT4 startrekker (based on nt 2.0)

 @mod.ids:

    defm "N.T."
    defb mod.type.nt

    defm "M&K!"
    defb mod.type.nt

    defm "M.K."
    defb mod.type.pt

    defm "M!K!"
    defb mod.type.ptx

    defm "FLT4"
    defb mod.type.flt

@list.mod.id.names:

    defb mod.type.ust.15
    defw @txt.mod.type.ust.15
    defb mod.type.st.15
    defw @txt.mod.type.st.15
    defb mod.type.st22
    defw @txt.mod.type.st22
    defb mod.type.st23
    defw @txt.mod.type.st23
    defb mod.type.nt
    defw @txt.mod.type.nt
    defb mod.type.st25
    defw @txt.mod.type.st25
    defb mod.type.pt
    defw @txt.mod.type.pt
    defb mod.type.ptx
    defw @txt.mod.type.ptx
    defb mod.type.flt
    defw @txt.mod.type.flt
    defb mod.type.invalid
    defw @txt.mod.type.invalid


 @txt.mod.type.ust.15:  defm "Ultimate Soundtracker"
                        defb 0
 @txt.mod.type.st.15:   defm "Soundtracker"
                        defb 0
 @txt.mod.type.st22:    defm "Soundtracker 2.2"
                        defb 0
 @txt.mod.type.st23:    defm "Soundtracker 2.3"
                        defb 0
 @txt.mod.type.nt:      defm "Noisetracker"
                        defb 0
 @txt.mod.type.st25:    defm "Soundtracker 2.5"
                        defb 0
 @txt.mod.type.pt:      defm "Protracker"
                        defb 0
 @txt.mod.type.ptx:     defm "Protracker 2.3"
                        defb 0
 @txt.mod.type.flt:     defm "Startrekker"
                        defb 0
 @txt.mod.type.invalid: defm "Invalid"
                        defb 0

;-------------------------------------------------------------------------------
mod.text.de:

 ; input
 ; - a  = mod.type

 ; output
 ; - de = @txt.mod.type

    push hl
    ld hl,@list.mod.id.names - 3

    @loop:

        inc hl
        inc hl
        inc hl
        cp (hl)

        jr nz,@-loop

    inc hl
    ld e,(hl)
    inc hl
    ld d,(hl)

    pop hl

    ret

;-------------------------------------------------------------------------------
mod.type:

 ; input:
 ; - hl = mod start

 ; output:
 ; - a  = mod.type

 ; detect sample lengths - all < 9999 possible ST

    ld (@var.mod.offset),hl

    call @detect.id

    cp mod.type.unknown
    call z,@detect.ust

    cp mod.type.invalid
    call nz,@check.sample.repeats

    cp mod.type.invalid
    call nz,@check.sample.lengths

    cp mod.type.invalid
    call nz,@check.file.size

    ; ld c,a
    ; ld b,0

    ret z

    ; ld bc,404

    ld a,mod.type.invalid

    ret

;-------------------------------------------------------------------------------
@detect.id:

 ; input:
 ;  hl = mod start

    ld de,mod.pt.id
    add hl,de

    ld de,@mod.ids

    ld b,4

    @loop:

        push hl
        call @compare.hl.de
        pop hl
        jr z,@leave

        djnz @-loop

    ld a,mod.type.unknown

 @leave:

    ld (@var.mod.type),a

    ret


;-------------------------------------------------------------------------------
@compare.hl.de:

 ; input:
 ;  hl = compare string
 ;  de = compare string

 ; output:
 ;  nz = different
 ;  z  = same
 ;  a  = value after same string

    ex de,hl
    push bc

    ld b,4

    @loop:

        ld a,(de)
        inc de
        cp (hl)
        inc hl
        jr nz,@neq

        djnz @-loop

    xor a       ; set z
    ld a,(hl)

    jr @leave

 @neq:

    @loop:

        inc hl

        djnz @-loop

    dec b       ; set nz

 @leave:

    ex de,hl

    pop bc

    ret

;-------------------------------------------------------------------------------
@get.mod.samples.ix:

    push hl
    push de

    ld hl,(@var.mod.offset)
    ld de,mod.samples
    add hl,de

    push hl
    pop ix

    pop de
    pop hl

    ret

;-------------------------------------------------------------------------------
@detect.ust:

 ; if all sample lengths < 9999 bytes -> Ultimate Soundtracker

    call @get.mod.samples.ix

    ld b,15

    @loop:

        ld a,(ix+mod.sample.finetune)
        or a
        jr nz,@invalid

        ld h,(ix+mod.sample.len.words+0)
        ld l,(ix+mod.sample.len.words+1)
        ld de,9999
        sbc hl,de
        jr nc,@invalid  ; > 9999

        ld de,mod.sample.len
        add ix,de

        djnz @-loop

    ld a,mod.type.ust.15
    jr @leave

 @invalid:

    ld a,mod.type.invalid

 @leave:

    ld (@var.mod.type),a

    ret

;-------------------------------------------------------------------------------
@check.sample.repeats:

 ; check that all sample repeat offsets < sample.lengths
 ; check that all sample repeat offsets + sample repeat lengths < sample lengths

    call @get.mod.samples.ix

    ld b,31

    @loop:

        push bc
        call @check.sample.repeat
        pop bc
        jr nz,@leave

        ld de,mod.sample.len
        add ix,de

        djnz @-loop

        ld a,(@var.mod.type)
        ret

 @leave:

    ld a,b
    cp 17   ; if at least 15 samples ok -> soundtracker ( 31 - 15 = 16 but b not yet dec )
    jr nc,@invalid

    ld a,(@var.mod.type)
    cp mod.type.ust.15
    ret z

    ld a,mod.type.st.15
    ret

 @invalid:

    ld a,mod.type.invalid

    ret

;-------------------------------------------------------------------------------
@check.sample.repeat:

 ; check sample repeat offset < sample.length
 ; check sample repeat offset + sample repeat length < sample length

 ; input:
 ; - ix = sample

 ; output:
 ; - z  = true

    call @get.sample.length.bc
    ret z

    ld h,(ix+mod.sample.repeat.offset.words+0)
    ld l,(ix+mod.sample.repeat.offset.words+1)

    ld a,(@var.mod.type)
    cp mod.type.ust.15
    jr nz,@not.ust

    srl h   ; ust repeat offset is in bytes
    rr l

 @not.ust:

    or a
    sbc hl,bc
    jr nc,@fail ; repeat offset > length

    add hl,bc

    ld d,(ix+mod.sample.repeat.len.words+0)
    ld e,(ix+mod.sample.repeat.len.words+1)
    add hl,de

    inc bc
    or a
    sbc hl,bc
    jr nc,@fail ; repeat offset + repeat length > length

 @ok:

    xor a       ; set z
    ret

 @fail:

    xor a
    dec a       ; set nz
    ret

;-------------------------------------------------------------------------------
@get.sample.length.bc:

 ; input
 ; - ix = sample

 ; output
 ; - bc = sample length in words
 ; - z  = empty sample

    ld b,(ix+mod.sample.len.words+0)
    ld c,(ix+mod.sample.len.words+1)

    ld a,b
    or c
    ret z

    dec bc
    ld a,b
    or c    ; set z
    inc bc

    ret

;-------------------------------------------------------------------------------
@get.samples.format.b:

 ; get number of samples supported by format (15 or 31)

 ; input:
 ; - a = mod.type

 ; output:
 ; - b = samples

    ld b,15
    cp mod.type.ust.15
    ret z

    cp mod.type.st.15
    ret z

    ld b,31
    ret

;-------------------------------------------------------------------------------
@check.sample.lengths:

 ; check that at least one sample has a length

 ; output:
 ; - a  = mod.type
 ; - nz = has at least one sample with a length

    call @get.mod.samples.ix

    call @get.samples.format.b

    @loop:

        push bc
        call @get.sample.length.bc
        pop bc

        ld a,(@var.mod.type)
        ret nz

        ld de,mod.sample.len
        add ix,de

        djnz @-loop

    ld a,mod.type.invalid

    ret

;-------------------------------------------------------------------------------
@add.chl.de:

    add hl,de
    ret nc

    inc c
    ret

;-------------------------------------------------------------------------------
@check.file.size:

 ; add all patterns + sample lengths and compare with file size

 ; output
 ; - z = ok

    ld a,(@var.mod.type)

    ld c,0
    ld hl,mod.title.len + mod.pattern.table.len + 2

    cp mod.type.st23
    jr c,@no.id
    inc hl
    inc hl
    inc hl
    inc hl
 @no.id:

    ld de,mod.sample.len
    call @get.samples.format.b

    @loop:
        add hl,de
        djnz @-loop

    call @get.total.sample.length.chl
    call @get.highest.pattern.b

    ld de,0x40 * 4 * 4    ; 64 rows, 4 channels, 4 bytes

    @loop:
        call @add.chl.de

        djnz @-loop

    ; cp with file.len (populated by directory)
    ; ld a,h
    ; ld (file.len+1),a
    ; ld a,c
    ; ld (file.len+2),a

    xor a       ; set z

    ld a,(@var.mod.type)

    ret

;-------------------------------------------------------------------------------
@get.total.sample.length.chl:

    call @get.mod.samples.ix
    call @get.samples.format.b

    xor a
    ld (@var.mod.samples),a

    @loop:

        ld d,(ix + mod.sample.len.words + 0)
        ld e,(ix + mod.sample.len.words + 1)

        call @add.chl.de
        call @add.chl.de

        ld a,d
        or a
        jr nz,@is.sample

        ld a,e
        cp 2
        jr c,@not.sample

     @is.sample:

        ld a,(@var.mod.samples)
        inc a
        ld (@var.mod.samples),a

     @not.sample:

        ld de,mod.sample.len
        add ix,de

        djnz @-loop

    ret

;-------------------------------------------------------------------------------
@get.highest.pattern.b:

 ; get highest pattern from pattern table, the highest pattern is needed to get
 ; to the sample data

 ; output:
 ; - b = highest pattern


    push hl

    ld hl,(@var.mod.offset)
    ld de,mod.title.len
    add hl,de

    ld de,mod.sample.len

    ld a,(@var.mod.type)
    call @get.samples.format.b

    @loop:
        add hl,de

        djnz @-loop

    inc hl
    inc hl      ; -> hl mod.pattern.table
    xor a
    ld b,128

    @loop:
        cp (hl)
        jr nc,@next

        ld a,(hl)

     @next:

        inc hl
        djnz @-loop

    pop hl

    inc a       ; pattern 0 is also a pattern
    ld (@var.mod.patterns),a

    ld b,a

    ret

