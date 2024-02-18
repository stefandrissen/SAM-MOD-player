    ; put code in section AB to manipulate data in section CD

    di
    in a,(port.lmpr)
    ld (@im.lmpr),a
    ld (@im.stsp),sp
    in a,(port.hmpr)
    and low.memory.page.mask
    or low.memory.ram.0
    out (port.lmpr),a
    jp @install.mod.low

;===============================================================================
    org $ - 0x8000

    tracker.ptr.page.mod.low:   equ tracker.ptr.page.mod    - 0x8000
    tracker.octaves.low:        equ tracker.octaves         - 0x8000
    tracker.ram.low:            equ tracker.ram             - 0x8000
    tracker.samples.low:        equ tracker.samples         - 0x8000
    tracker.gap.low:            equ tracker.gap             - 0x8000

    sample.table.low:           equ sample.table            - 0x8000

;-------------------------------------------------------------------------------
@install.mod.low:

    ld sp,0x4000

    ld a,(tracker.ram.low)
    and %11100
    jr z,@no.megabyte

    ld a,high.memory.external
    out (port.hmpr),a

 @no.megabyte:

    ld a,(tracker.ptr.page.mod.low)
    ld (@page.mod.1),a
    ld (@page.mod.2),a
    ld (@page.mod.3),a
    ld (@page.mod.4),a
    ld (@page.mod.5),a
    ld (@page.mod.6),a

    call set.high.memory.a

    ; clear sample table

    ld hl,sample.table.low
    ld de,sample.table.low + 1
    ld bc,32 * sample.table.len - 1
    ld (hl),l
    ldir

    ld hl,0x8000
    call mod.determine.type

    call mod.get.max_samples.a
    ld (tracker.samples.low),a

    call mod.get.song_positions.a
    ld (song.length - 0x8000),a

    ; copy pattern table to area within tracker page
    call mod.get.pattern_table.hl
    set 7,h
    ld de,pattern.table - 0x8000
    ld bc,mod.pattern.table.len
    ldir

    ld a,(tracker.ptr.page.mod.low)
    call mod.determine.octaves
    ld (tracker.octaves.low),a

    ; give gap correct value, 3 for 3 octaves, 6 for 5 octaves

    cp 5
    ld a,3
    jr nz,$+3
    rlca
    ld (tracker.gap.low),a

    ld a,(tracker.ptr.page.mod.low)
    call set.high.memory.a

    call mod.get.pattern.hl
    set 7,h
    ld a,h
    ld (origpat.offsh + 1 - 0x8000),a
    ld a,l
    ld (origpat.offsl + 1 - 0x8000),a

    ld ix,mod.samples + 0x8000
    ld iy,sample.table.low
    call @create.sample.table

    ; in dummy sample only start address is valid data

    ld (iy + st.start + 0),l
    ld (iy + st.start + 1),h
    ld (iy + st.start + 2),b

    call @move.samples
    call @fix.sample.loops
    call @fill.gap.sample.loops
    call @update.unused.samples

    in a,(port.lmpr)
    and low.memory.page.mask
    out (port.hmpr),a

    jp @fs.high

;-------------------------------------------------------------------------------
@create.sample.table:

    ; - put starting addresses of samples in sample table by adding the sample
    ;   length to the current sample
    ; - fill in finetune, volume, does sample exist?
    ; - fill in loop type (0=none, 1=large, 2=small)

    ; input:
    ; - ix  = sample table in module
    ; - iy  = sample table internal

    call mod.get.sample.bhl
    set 7,h
   @page.mod.1: equ $+1
    ld a,0
    add a,b
    ld b,a

    ld a,(tracker.samples.low)  ; [15|31]
    ld c,a

    @loop.c.convall:

        ld (iy + st.start + 0),l
        ld (iy + st.start + 1),h
        ld (iy + st.start + 2),b

        ld a,(ix + mod.sample.finetune)
        and 0x0f
        ld (iy + st.finetune),a

        ld a,(ix + mod.sample.volume)
        cp 0x40
        jr c,@volume.ok
        ld a,0x3f               ; volume tables only go to 0x3f
     @volume.ok:
        ld (iy + st.vol),a

        ld d,(ix + mod.sample.len.words + 0)
        ld e,(ix + mod.sample.len.words + 1)

        ld a,d
        or e
        ld a,0
        jr z,$+3                ; !!! length 1 word should also be no sample
        dec a
        ld (iy + st.sample),a   ; -1 = no sample

        push hl

        call @get.loop.size
        ld (iy + st.loop),a

        pop hl

        call @add.bhl.de2

        call @next.entry.ix.iy

        dec c
        jr nz,@loop.c.convall

    ret

;-------------------------------------------------------------------------------
@next.entry.ix.iy:

    ld de,sample.table.len
    add iy,de

    ld de,mod.sample.len
    add ix,de

    ret
;-------------------------------------------------------------------------------

@get.loop.size:

    ; the original protracker idea is that ALL samples repeat,
    ; the ones that "do not repeat", repeat on first sample word which is 0x0000 -> silence

    ; output:
    ; - a = 0 - no loop
    ;       1 - large loop
    ;       2 - small loop

    ld h,(ix + mod.sample.repeat.len.words + 0)
    ld l,(ix + mod.sample.repeat.len.words + 1)
    sla l
    rl h                    ; -> bytes
    ld a,st.loop.large
    ret c

    ld a,(tracker.gap.low)
    dec a
    cp h
    ld a,st.loop.large
    ret c

    ld a,h
    or a
    ld a,st.loop.small
    ret nz

    ld a,l
    cp 4                    ; no looping if loop len < 4 bytes ( = 2 words = <= 1 word )
    ld a,st.loop.small
    ret nc

    xor a                   ; no loop
    ret

;-------------------------------------------------------------------------------
@move.samples:

    ; create the space necessary between samples by moving the samples to higher
    ; addresses

    call @get.required.space.b

    ; ix = entry past last sample table entry
    ; b  = total number of bytes (gap*256) that need to be spaced out

    @shift.all:

        push bc

        push bc

        ld l,(ix + st.start + 0)
        ld h,(ix + st.start + 1)
        ld b,(ix + st.start + 2)        ; BHL = address last byte of sample +1

        ld a,b
        call set.high.memory.a

        push hl

           @page.mod.2: equ $+1
            cp page.mod
            jr z,@nz

            set 6,h ; page abcD
            dec a
         @nz:
            dec hl

            ld (@source.lo),hl
            ld (@source.hi),a

        pop hl                          ; source: AHL in block D -> last byte of sample
        push hl

            ld e,(ix - st.prev.start + 0)
            ld d,(ix - st.prev.start + 1)
            ld c,(ix - st.prev.start + 2)
            xor a
            sbc hl,de
            jr nc,@nc

            ld de,0x4000
            add hl,de
            scf

         @nc:

            ld a,b
            sbc c
            ld (@result.lo),hl      ; number of bytes to be
            ld (@result.hi),a       ; copied put in result

        pop hl

        ld a,b
        pop bc

        ld e,a

        ld a,(tracker.gap.low)
        ld c,a

        @loop:
            ld a,b                      ; bytes to be shifted (* 256)
            add a,h
            ld h,a
            jr nc,@nc

            inc e
            inc e
            set 7,h
         @nc:
            dec c
            jr nz,@-loop

        bit 6,h
        res 6,h
        jr z,$+3
        inc e

        ld a,e
        call set.high.memory.a

        ld (ix + st.start + 0),l    ; new start address of
        ld (ix + st.start + 1),h    ; sample at shifted
        ld (ix + st.start + 2),a    ; position
       @page.mod.3: equ $+1
        cp page.mod
        jr z,@bottom.page

        dec a
        ld (@page),a
        call set.high.memory.a

        set 6,h
     @bottom.page:
        dec hl

        ld a,(ix - st.prev.sample)
        or a
        jr z,@noaddgap

        ; how much gap needed behind sample

        ld a,(ix - st.prev.loop)
        cp st.loop.small
        ld a,(tracker.gap.low)
        jr nz,@only1
        ld c,a
        add a,a
        add a,c                 ; 3 * gap
     @only1:

        ; clear needed gap * 256 to no sound

        ld c,a
        xor a                   ; 0 = no volume (sample is signed 8 bit integer)
        ld b,a

        @clearend:

                ld (hl),a
                dec hl
                djnz @clearend
            dec c
            jr nz,@clearend

     @noaddgap:

       @page: equ $+1
        ld a,0
       @page.mod.4: equ $+1
        cp page.mod
        jr z,@nz

        bit 6,h
        set 6,h
        jr nz,@nz
        dec a
     @nz:
        ; now pointing to address before gap

        ld (@target.lo),hl
        ld (@target.hi),a

        ; so lddr copy sample from old position to higher address

        @copy.loop:

           @result.lo: equ $+1
            ld hl,0
           @result.hi: equ $+1
            ld a,0
            ld bc,move.size
            or a
            sbc hl,bc
            jr nc,@keepcopying

            sub 1
            jr c,@nomoreadd

            ld de,0x4000
            add hl,de
            jr @keepcopying

         @nomoreadd:
            add hl,bc
            ld c,l
            ld b,h

         @keepcopying:
            ld (@result.lo),hl
            ld (@result.hi),a

            ld a,b
            or c
            jr z,@nocopy

           @source.lo: equ $+1
            ld hl,0
           @source.hi: equ $+1
            ld a,0
            call set.high.memory.a

            ld de,move.spc + move.size - 1
            push bc

            lddr                        ; copy from source to buffer

           @page.mod.5: equ $+1
            cp page.mod
            jr z,@nz

            bit 6,h
            set 6,h
            jr nz,@nz
            dec a
         @nz:
            ld (@source.hi),a
            ld (@source.lo),hl

            ld hl,move.spc + move.size - 1
           @target.lo: equ $+1
            ld de,0
           @target.hi: equ $+1
            ld a,0
            call set.high.memory.a

            pop bc

            lddr                        ; copy from buffer to target

           @page.mod.6: equ $+1
            cp page.mod
            jr z,@nz

            bit 6,d
            set 6,d
            jr nz,@nz
            dec a
         @nz:
            ld (@target.hi),a
            ld (@target.lo),de
         @nocopy:
            ld a,(@result.hi)
            inc a               ;stop when A=255

            jr nz,@-copy.loop

        ld bc,-sample.table.len
        add ix,bc

        ld a,(ix + st.sample)
        or a
        ld a,0
        jr z,@nosamplegap

        ;get number of gaps used

        ld a,(ix + st.loop)
        cp st.loop.small
        ld a,0
        jr nz,$+4
        add 2               ;for small loop
        inc a
     @nosamplegap:
        ld e,a

        pop bc

        ld a,b              ;update how much to move
        sub e
        ld b,a

        jp nz,@shift.all

    ret

;-------------------------------------------------------------------------------
@get.required.space.b:

    ; calculate how much space needs to be created to accommodate the endings
    ; and loopings of samples

    ; 1. a sample not looped needs one gap of silence following it
    ; 2. a sample with a loop greater than gap needs one gap with the start of
    ;    the loop following it
    ; 3. a sample with a loop smaller than gap needs three gaps with the whole
    ;    loop repeated in it - see below

    ld ix,sample.table.low
    ld de,sample.table.len
    ld a,(tracker.samples.low)
    ld b,a
    ld c,0

    @loop:

        ld a,(ix + st.sample)
        or a
        jr z,@no.sample

        ld a,(ix + st.loop)
        cp st.loop.small   ; small loop needs three gaps
        jr nz,@not.small

        inc c
        inc c

     @not.small:
        inc c

     @no.sample:

        add ix,de

        djnz @-loop

    ld b,c

    ret

;-------------------------------------------------------------------------------
@fix.sample.loops:

    call @get.sample.bug.a
    ld (loop.bug+1),a

    ; fill in end addresses of samples in sample table by adding sample length
    ; bytes (*2) to start address OR if the sample is looped by adding the
    ; offset + the sample length

    ld iy,sample.table.low
    ld ix,mod.samples + 0x8000

    ld a,(tracker.samples.low)
    ld c,a

    @loop:

        ld l,(iy + st.start + 0)
        ld h,(iy + st.start + 1)
        ld b,(iy + st.start + 2)

        ld d,(ix + mod.sample.len.words + 0)
        ld e,(ix + mod.sample.len.words + 1)

        ld a,(iy + st.loop)
        or a
        jr z,@notloop

        ld a,(loop.bug+1)
        or a
        jr z,@normal

        dec a
        jr nz,@soul.bug

     @noise.bug:
        ld d,(ix + mod.sample.repeat.offset.words + 0)  ; loop offset in bytes!
        ld e,(ix + mod.sample.repeat.offset.words + 1)
        jr @contnorm

     @soul.bug: ; soul-o-matic bug
        ld d,(ix + mod.sample.repeat.len.words + 0)
        ld e,(ix + mod.sample.repeat.len.words + 1)
        call @add.bhl.de2
        jr @got.gap

     @normal:

        ld d,(ix + mod.sample.repeat.offset.words + 0)
        ld e,(ix + mod.sample.repeat.offset.words + 1)

        call @add.bhl.de

     @contnorm:

        call @add.bhl.de
        ld d,(ix + mod.sample.repeat.len.words + 0)
        ld e,(ix + mod.sample.repeat.len.words + 1)

     @notloop:

        call @add.bhl.de2

     @got.gap:

        ld (iy + st.end + 0),l
        ld (iy + st.end + 1),h
        ld (iy + st.end + 2),b

        call @next.entry.ix.iy

        dec c
        jr nz,@-loop

    ret

;-------------------------------------------------------------------------------
@fill.gap.sample.loops:

    ; fill in GAP to accomodate looped samples, small and large

    ld iy,sample.table.low
    ld ix,mod.samples + 0x8000
    ld a,(tracker.samples.low)
    ld b,a

    @loop:

        push bc

        ld a,(tracker.ptr.page.mod.low)
        call set.high.memory.a

        ld a,(iy + st.loop)
        or a

        call nz,@fill.gap.sample.loop

     @next:
        ld bc,sample.table.len
        add iy,bc
        ld bc,mod.sample.len
        add ix,bc

        pop bc

        djnz @-loop

    ret

;-------------------------------------------------------------------------------
@fill.gap.sample.loop:

 ; input:
 ; - ix = mod sample table
 ; - iy = internal sample table

    ld h,(ix + mod.sample.len.words + 0)
    ld l,(ix + mod.sample.len.words + 1)
    ld d,(ix + mod.sample.repeat.offset.words + 0)
    ld e,(ix + mod.sample.repeat.offset.words + 1)

 loop.bug:
    ld a,0
    or a
    jr z,@+loop.ok

    dec a
    jr z,@noise.bug

 @soul.bug: ; in SOUL-O-MATIC the loop len is given as loop end offset
    or a
    sbc hl,de
    ld (ix + mod.sample.repeat.len.words + 0),h ; now loop len = loopend off -
    ld (ix + mod.sample.repeat.len.words + 1),l ;                loopstart off
    jr @+loop.ok

 @noise.bug: ; "noisetracker" loop offset in bytes instead of words
    srl d
    rr e

 @loop.ok:   ; DE = loop offset

    ld l,(iy + st.start + 0)
    ld h,(iy + st.start + 1)
    ld b,(iy + st.start + 2)

    call @add.bhl.de2

    ld a,(iy + st.loop)
    dec a

    push af
    call nz,@fill.gap.sample.loop.small
    pop af
    call z,@fill.gap.sample.loop.large

    ret

;-------------------------------------------------------------------------------
@fill.gap.sample.loop.small:

 ; input:
 ; - bhl = start sample
 ; - ix  = mod sample table
 ; - iy  = internal sample table

 ; Small loop, can be up to gap in size, it has to cover the end loop marker
 ; meaning that three gaps are needed if the loop is gap-1 then the first
 ; marker is after 2*(gap-1) at the next frame it is possible that the sample
 ; will then be at position 3*(gap-3).  Then the difference between the
 ; current position and the first repeat end after gap can be added to gap to
 ; get new position.
 ; - follow that?  It took me a while to think that one up... 8-)

    ld a,(tracker.gap.low)
    ld d,a
    add a,a
    add a,d
    ld d,a
    ld e,0
    ld (@totalgap+1),de  ; DE < gap

    ld a,b

    ; ld de,move.spc
    ld b,(ix + mod.sample.repeat.len.words + 0)
    ld c,(ix + mod.sample.repeat.len.words + 1)
    sla c
    rl b

    call set.high.memory.a

    ld (@sm.lp.src+1),hl

    ;  push bc
    ;  ldir
    ;  pop bc

    ld e,(iy + st.end + 0)
    ld d,(iy + st.end + 1)

    ; ld a,(iy+st.end+2) = equal to start loop
    ; out (port.hmpr),a

    or a
    sbc hl,de           ; if end lower than sample
    jr c,$+4            ; then increase page positon
    set 6,d             ; of end by setting bit 6

    push af             ; make small loop pointer
    ld hl,0             ; at least gap size
 @loop:
    add hl,bc           ; -> possibly 2 * ( gap - 1 )
    ld a,(tracker.gap.low)
    cp h
    jr nc,@-loop
    pop af

    add hl,de
    bit 6,h
    res 6,h
    jr z,$+3
    inc a
    ld (iy + st.loop.end + 0),l
    ld (iy + st.loop.end + 1),h
    ld (iy + st.loop.end + 2),a

    ; keep copying loop until 3 * gap space is filled

    @loop.copy:

     @totalgap:
        ld hl,0
        or a
        sbc hl,bc
        ret z
        jr c,@last.partial

        ld (@totalgap + 1),hl

     @sm.lp.src:
        ld hl,0
        push bc
        ldir
        pop bc

        jr @-loop.copy

 @last.partial:

    add hl,bc
    ld c,l
    ld b,h
    ld hl,(@sm.lp.src + 1)
    ldir

    ret

;-------------------------------------------------------------------------------
@fill.gap.sample.loop.large:

    ld (iy + st.loop.start + 0),l
    ld (iy + st.loop.start + 1),h
    ld (iy + st.loop.start + 2),b

    ; copy start of loop to gap after end loop for gap bytes

    ld a,b
    call set.high.memory.a

    ld de,move.spc
    ld c,0
    ld a,(tracker.gap.low)
    ld b,a
    push bc
    ldir

    ld e,(iy + st.end + 0)
    ld d,(iy + st.end + 1)
    ld a,(iy + st.end + 2)
    call set.high.memory.a

    ld hl,move.spc
    pop bc
    ldir                    ;copy to beginloop

    ret

;-------------------------------------------------------------------------------
@get.sample.bug.a:

 ; output:
 ; - a = 0 -> normal looping
 ;       1 -> noisetracker bugged loop
 ;       2 -> soul-o-matic bug

 ; check ALL samples for Noisetracker bug
 ; -> loop offset in bytes instead of in words
 ;    loopoffset * 2 + looplen * 2 > samplelen -> bugged

 ; "soul-o-matic bug"
 ; -> loop len = end loop offset

 ; "approximity bug"
 ; -> loop len > len sample -> loop len = len sample

    ld iy,sample.table.low
    ld ix,mod.samples + 0x8000
    ld a,(tracker.samples.low)
    ld b,a

    ld a,(tracker.ptr.page.mod.low)
    call set.high.memory.a

    @loop:

        ld a,(iy + st.loop)
        or a
        jr z,@next              ; no loop -> no bug

        ld h,(ix + mod.sample.len.words + 0)
        ld l,(ix + mod.sample.len.words + 1)
        ld d,(ix + mod.sample.repeat.len.words + 0)
        ld e,(ix + mod.sample.repeat.len.words + 1)
        or a
        sbc hl,de               ; assuming loop len < sample len
        jr nc,@loop.lt.len

        add hl,de
        ld (ix + mod.sample.repeat.len.words + 0),h ;set loop len to sample len
        ld (ix + mod.sample.repeat.len.words + 1),l
        ld hl,0                 ; sample len - loop len

     @loop.lt.len:
        ld d,(ix + mod.sample.repeat.offset.words + 0)  ;loop offs
        ld e,(ix + mod.sample.repeat.offset.words + 1)
        sbc hl,de

        call c,@sample.loop.issue

     @next:
        call @next.entry.ix.iy

        djnz @-loop

    xor a

    ret

;-------------------------------------------------------------------------------
@sample.loop.issue:

    pop af  ; chuck return address

    ;loop offs + len > sample len

    add hl,de
    srl d
    rr e
    or a
    sbc hl,de

    ld a,1
    adc a,0      ; in SOUL-O-MATIC the loop len is given as loop end offset

    ret

;-------------------------------------------------------------------------------
@update.unused.samples:

 ; create an empty sample if necessary

    call @find.silent.sample

    ld b,31
    ld iy,sample.table.low
    ld de,sample.table.len

    @loop:

        ld a,(iy + st.sample)
        or a
        call z,@update.sample.silence
        add iy,de
        djnz @-loop

    ret

;-------------------------------------------------------------------------------
@find.silent.sample:

 ; output
 ; - chl = silent gap

    ld b,31
    ld iy,sample.table.low
    ld de,sample.table.len

    @loop:

        ld a,(iy + st.sample)
        or a
        jr z,@+next             ; no sample -> no gap

        ld a,(iy + st.loop)
        or a
        jr nz,@+next            ; looped -> no gap

        ld l,(iy + st.end + 0)
        ld h,(iy + st.end + 1)
        ld c,(iy + st.end + 2)

        ret

     @next:

        add iy,de

        djnz @-loop

 @create.sample.silent:

  ; create silent sample at postion after all other samples

    ld l,(iy + st.start + 0)
    ld h,(iy + st.start + 1)
    ld c,(iy + st.start + 2)

    ld a,c
    call set.high.memory.a
    ld e,a

    push hl

    ld a,(tracker.gap.low)
    ld c,a
    ld b,0
    xor a

    @loop:

        ld (hl),a
        inc hl
        djnz @-loop

        dec c
        jr nz,@-loop

    ld c,e
    pop hl

    ret

;-------------------------------------------------------------------------------
@update.sample.silence:

    ld (iy + st.start + 0),l
    ld (iy + st.start + 1),h
    ld (iy + st.start + 2),c

    ld (iy + st.end + 0),l
    ld (iy + st.end + 1),h
    ld (iy + st.end + 2),c

    ret

;-------------------------------------------------------------------------------

    mod.no.check.file.size: equ 1
    mod.tracker:            equ 1
    include "../loader/mod.s"

;---------------------------------------------------------------
@add.bhl.de:

    ; add DE to BHL, result -> HL 0-16383 bit 7 unchanged, B 0-31

    push de
    ld a,d
    and %11000000
    rlca
    rlca
    add b
    ld b,a
    ld a,d
    and %00111111
    ld d,a
    add hl,de
    bit 6,h
    res 6,h
    jr z,$+3
    inc b
    pop de

    ret

;---------------------------------------------------------------
@add.bhl.de2:

    ; add DE * 2 to BHL, result -> same as above

    push de
    ld a,d
    and %11100000
    rlca
    rlca
    rlca
    add b
    ld b,a
    ld a,d
    and %00011111
    ld d,a
    add hl,de
    add hl,de
    bit 6,h
    res 6,h
    jr z,$+3
    inc b
    pop de

    ret

;---------------------------------------------------------------
set.high.memory.a:

    ;   a = page high memory

    push af

    ld a,(tracker.ram.low)
    and %11100

    jr z, @no.megabyte

    pop af
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a
    dec a

    ret

 @no.megabyte:

    pop af
    out (port.hmpr),a

    ret

;===============================================================
    org $ + 0x8000

@fs.high:

   rs.bp.page: equ $+1
    ld a,0      ;burst page
    or low.memory.ram.0
    out (port.lmpr),a
    ld sp,0x8000

    ; chl = silent sample

    ld a,c
 c1.bp.offset:
    ld (0),hl
 c1.bp.page:
    ld (0),a
    ld (c1 + sample_end.offset),hl
    ld (c1 + sample_end.page),a

    ld (c1 + cx.sample.page),a
    ld (c1 + cx.sample.offset),hl

 c2.bp.offset:
    ld (0),hl
 c2.bp.page:
    ld (0),a
    ld (c2 + sample_end.offset),hl
    ld (c2 + sample_end.page),a

    ld (c2 + cx.sample.page),a
    ld (c2 + cx.sample.offset),hl

 c3.bp.offset:
    ld (0),hl
 c3.bp.page:
    ld (0),a
    ld (c3 + sample_end.offset),hl
    ld (c3 + sample_end.page),a

    ld (c3 + cx.sample.page),a
    ld (c3 + cx.sample.offset),hl

 c4.bp.offset:
    ld (0),hl
 c4.bp.page:
    ld (0),a
    ld (c4 + sample_end.offset),hl
    ld (c4 + sample_end.page),a

    ld (c4 + cx.sample.offset),hl
    ld (c4 + cx.sample.page),a


    ld hl,31 * sample.table.len + sample.table - 1
    ld de,32 * sample.table.len + sample.table - 1
    ld bc,31 * sample.table.len
    lddr

    ld hl,sample.table
    ld de,sample.table + 1
    ld bc,sample.table.len - 1
    ld (hl),b
    ldir

    ld a,-1
    ld (sample.table + 2),a   ; page of no sample

 ; put correct maximum & minimum periods into tracker

    ld hl,113               ; B-3
    ld de,856               ; C-1
    ld a,(tracker.octaves)
    cp 5
    jr nz,mm.per.3
    ld hl,56                ; B-4 (=57)
    ld de,1023              ; A-0 (=1017) octave 0 not fully supported
mm.per.3:
    ld (c1 + period.min),hl
    ld (c1 + period.max),de
    ld (c2 + period.min),hl
    ld (c2 + period.max),de
    ld (c3 + period.min),hl
    ld (c3 + period.max),de
    ld (c4 + period.min),hl
    ld (c4 + period.max),de

reset.song:
    ld a,0xff
    ld hl,c1.on
    ld (hl),a
    inc l
    ld (hl),a
    inc l
    ld (hl),a
    inc l
    ld (hl),a
    inc l
    cpl
    ld (hl),a               ;update volume?

    ld hl,0x500
    ld (tick.fraction),hl
    call reset.speed

    ; ld (disable.pos),a    ;no position jumping (temp)

    xor a
    ld (pattern_delay.counter),a
    ld (song.position),a
    ld (pattern.row),a
    ld (mstatus),a

    ld ix,reset.list
    ld de,routine.len
    @loop:
        ld l,(ix+0)
        ld h,(ix+1)
        ld a,l
        or h
        jr z,@leave

        ld a,(ix+2)
        inc ix
        inc ix
        inc ix

        ld b,4
        @channels:
            ld (hl),a
            add hl,de
            djnz @-channels

        jr @-loop

 @leave:
    call c1 + bp.volume
    call c2 + bp.volume
    call c3 + bp.volume
    call c4 + bp.volume

    call c1 + period.nop
    call c2 + period.nop
    call c3 + period.nop
    call c4 + period.nop

;---------------------------------------------------------------

 @im.stsp: equ $+1
    ld sp,0

 @im.lmpr: equ $+1
    ld a,0
    out (port.lmpr),a

    ld a,(tracker.samples)

    ; ei
    ret

;---------------------------------------------------------------
reset.speed:

    ld a,6
    ld (speed),a
    ld hl,0x100
    ld (tempo),hl

    ret

;---------------------------------------------------------------

reset.list:
    defw c1 + repeat
    defb 0
    defw c1 + volume
    defb 0
    defw c1 + period + 0
    defb 0
    defw c1 + period + 1
    defb 0
    defw c1 + wanted.per + 0
    defb 0
    defw c1 + wanted.per + 1
    defb 0
    defw c1 + porta_up.mask
    defb 0xff
    defw c1 + porta_down.mask
    defb 0xff
    defw c1 + wav.cntrl
    defb 0
    defw c1 + tonespeed
    defb 0
    defw c1 + gliss
    defb 0
    defw c1 + vibr.cmnd
    defb 0
    defw c1 + vibr.pos
    defb 0
    defw c1 + trem.cmnd
    defb 0
    defw c1 + trem.pos
    defb 0
    defw c1 + sample_offset.offset
    defb 0
    defw c1 + jump_loop.loop_counter
    defb 0
    defw c1 + jump_loop.pattern.row
    defb 0
    defw c1 + instrument.new
    defb 0
    defw c1 + instrument.current
    defb 0
    defw 0

