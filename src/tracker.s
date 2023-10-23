;SAM MOD player - tracker

;(C) 1995-2023 Stefan Drissen

; first execute "BURST" and install "DEMO"

; https://pastebin.com/pg95YduC - 8bitbubsy
; https://github.com/cmatsuoka/tracker-history/blob/master/reference/amiga/soundtracker/Ultimate_Soundtracker-format.txt
; https://github.com/OpenMPT/openmpt/blob/master/soundlib/Load_mod.cpp
; https://wiki.openmpt.org/Manual:_Effect_Reference

include "memory.i"
include "ports/internal.i"
include "ports/megabyte.i"
include "constants/mod.i"

    org 0x8000

;-------------------------------------------------------------------------------

tracker.init:           jp @init.tracker
tracker.install.mod:    jp @install.mod

;-------------------------------------------------------------------------------

tracker.ptr.addr.demo:  defw 0              ; offset
tracker.ptr.page.demo:  defb 0              ; & page of demo (foreground)

tracker.ptr.page.mod:   defb page.mod       ; page mod loaded in at
tracker.octaves:        defb 3              ; number of octaves (3 or 5)
tracker.samples:        defb 0              ; [15|31]
tracker.ram:            defb 0              ; %XXXRR (RAM / 256K)
tracker.gap:            defb 0

;-------------------------------------------------------------------------------

@init.tracker:
    di
    in a,(port.lmpr)
    ld (@smc.lmpr + 1),a

    ld c,0

    @find.burst:

        ld a,c
        or low.memory.ram.0
        out (port.lmpr),a

        ld hl,bp.id
        ld a,(hl)
        cp "B"
        jr nz,@not.found
        inc l
        ld a,(hl)
        cp "U"
        jr nz,@not.found
        inc l
        ld a,(hl)
        cp "R"

        jr @found

     @not.found:

        inc c
        bit 5,c ;   256K: bit 4

        jr z,@find.burst

    xor a
    out (port.lmpr),a
    rst 0

 @found:

    ld a,c
    ld (rs.bp.page+1),a

    ;---------------------------------------------------------------------------
    ;set up the finetune tables

    ld hl,finet.tab
    ld de,finet.tab+1
    ld bc,0x0400 - 1
    ld (hl),0xff
    ldir

    ld de,finelist
    ld bc,4 * 12 * 256

    @loop.finetune:

        ld h,finet.tab / 256
        ld a,(de)
        inc de
        ld l,a
        ld a,(de)
        inc de
        add a,h
        ld h,a

        ld (hl),c
        inc c
        inc c

        djnz @-loop.finetune

    ;---------------------------------------------------------------------------

    ld hl,@c1
    ld de,@c2
    ld bc,( 4 - 1 ) * routine.len
    ldir

    ld ix,build.list

    @loop.relocate:

        ld l,(ix)
        ld h,(ix+1)
        inc ix
        inc ix
        ld a,h
        inc a

        jr z,@leave.relocate

        ld bc,@c1
        add hl,bc
        ld a,4

        @loop.reloc.4chan:

            ld e,(hl)
            inc hl
            ld d,(hl)
            ex de,hl
            add hl,bc
            ex de,hl
            ld (hl),d
            dec hl
            ld (hl),e
            ex de,hl
            ld hl,routine.len
            add hl,bc
            ld c,l
            ld b,h
            ld hl,routine.len
            add hl,de
            dec a

            jr nz,@-loop.reloc.4chan

        jr @-loop.relocate

 @leave.relocate:

    ld hl,@c1 + mk.cur.pat + 1
    ld bc,routine.len - 1
    ld de,mod.current.row
    ld a,4

    @mk.cur.lp:

        ld (hl),e
        inc hl
        ld (hl),d
        add hl,bc
        inc e
        inc e
        inc e
        inc e
        dec a

        jr nz,@-mk.cur.lp

    ld hl,@c1 + channel.on + 1
    ld de,c1.on
    ld a,4

    @mk.c.on.lp:

        ld (hl),e
        inc hl
        ld (hl),d
        add hl,bc
        inc e
        dec a

        jr nz,@-mk.c.on.lp

    ld iy,bp.device
    ld a,(iy)
    ld hl,@volume.normal
    dec a                       ; a=1 -> SAA
    jr nz,$+5
    ld hl,@volume.saa

    ld de,@c1 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld de,@c2 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld de,@c3 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld de,@c4 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld iy,bp.pointers

    ld l,(iy + bp.ptr.addr.tracker - bp.pointers)
    ld h,(iy + bp.ptr.addr.tracker - bp.pointers + 1)
    ld (hl),tracker \ 0x100
    inc hl
    ld (hl),tracker / 0x100

    in a,(port.hmpr)
    ld l,(iy + bp.ptr.page.tracker - bp.pointers)
    ld h,(iy + bp.ptr.page.tracker - bp.pointers + 1)
    ld (hl),a

    ld de,(tracker.ptr.addr.demo)
    ld l,(iy + bp.ptr.addr.demo - bp.pointers)
    ld h,(iy + bp.ptr.addr.demo - bp.pointers + 1)
    ld (hl),e
    inc hl
    ld (hl),d

    ld a,(tracker.ptr.page.demo)
    ld l,(iy + bp.ptr.page.demo - bp.pointers)
    ld h,(iy + bp.ptr.page.demo - bp.pointers + 1)
    ld (hl),a

    ld l,(iy + bp.ptr.addr.enable - bp.pointers)
    ld h,(iy + bp.ptr.addr.enable - bp.pointers + 1)
    ld (enable.burst),hl

    ld l,(iy + bp.ptr.addr.exit - bp.pointers)
    ld h,(iy + bp.ptr.addr.exit - bp.pointers + 1)
    ld (exit.burst),hl

    ; copy pointers (6) to tracker code for 4 channels

    ld iy,bp.pointers.sample

    ld ix,conv.list
    ld b,6 * 4

    @put.in.blp:

        ld e,(iy)
        ld d,(iy+1)
        inc iy
        inc iy

        @put.in.lp:

            ld l,(ix)
            ld h,(ix+1)
            inc ix
            inc ix
            ld a,h
            or l
            jr z,@put.done.item

            ld (hl),e
            inc hl
            ld (hl),d
            inc hl

            jr @put.in.lp

     @put.done.item:

        djnz @put.in.blp

  @smc.lmpr:
    ld a,0
    out (port.lmpr),a
    ; ei
    ret

;-------------------------------------------------------------------------------
@volume.normal:
    ; channel 1
    and %01111111
    add volume.table / 0x100
    ; channel 2
    and %01111111
    add volume.table / 0x100
    ; channel 3
    and %01111111
    add volume.table / 0x100
    ; channel 4
    and %01111111
    add volume.table / 0x100

;-------------------------------------------------------------------------------
@volume.saa:
    ; SAA has different volume tables for left and right

    ; channel 1
    and %01111110
    add volume.table / 0x100 + 0 ; left
    ; channel 2
    and %01111110
    add volume.table / 0x100 + 1 ; right
    ; channel 3
    and %01111110
    add volume.table / 0x100 + 1 ; right
    ; channel 4
    and %01111110
    add volume.table / 0x100 + 0 ; left

;-------------------------------------------------------------------------------
@install.mod:

    ; put code in section AB to manipulate data in section CD

    di
    in a,(port.lmpr)
    ld (im.lmpr+1),a
    ld (im.stsp+1),sp
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
    ld (@page.mod.1+1),a
    ld (@page.mod.2+1),a
    ld (@page.mod.3+1),a
    ld (@page.mod.4+1),a
    ld (@page.mod.5+1),a
    ld (@page.mod.6+1),a

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
    ld (song.len + 1 - 0x8000),a

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

    jp fs.high

;-------------------------------------------------------------------------------
@create.sample.table:

    ; - put starting addresses of samples in sample table by adding the sample
    ;   length to the current sample
    ; - fill in finetune, volume, does sample exist?
    ; - fill in loop type (0=none, 1=big, 2=small)

    ; input:
    ; - ix  = sample table in module
    ; - iy  = sample table internal

    call mod.get.sample.bhl
    set 7,h
 @page.mod.1:
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
    ; the ones that "do not repeat", repeat on first sample word which is 0x000 -> silence

    ; output:
    ; - a = 0 - no loop
    ;       1 - big loop
    ;       2 - small loop

    ld h,(ix + mod.sample.repeat.len.words + 0)
    ld l,(ix + mod.sample.repeat.len.words + 1)
    sla l
    rl h                    ; -> bytes
    ld a,@st.loop.big
    ret c

    ld a,(tracker.gap.low)
    dec a
    cp h
    ld a,@st.loop.big
    ret c

    ld a,h
    or a
    ld a,@st.loop.small
    ret nz

    ld a,l
    cp 4                    ; no looping if loop len < 4 bytes ( = 2 words = <= 1 word )
    ld a,@st.loop.small
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

         @page.mod.2:
            cp page.mod
            jr z,@nz

            set 6,h ; page abcD
            dec a
         @nz:
            dec hl

            ld (@source.lo + 1),hl
            ld (@source.hi + 1),a

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
            ld (@result.lo + 1),hl  ; number of bytes to be
            ld (@result.hi + 1),a   ; copied put in result

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
     @page.mod.3:
        cp page.mod
        jr z,@bottom.page

        dec a
        ld (@page + 1),a
        call set.high.memory.a

        set 6,h
     @bottom.page:
        dec hl

        ld a,(ix - st.prev.sample)
        or a
        jr z,@noaddgap

        ; how much gap needed behind sample

        ld a,(ix - st.prev.loop)
        cp @st.loop.small
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

     @page:
        ld a,0
     @page.mod.4:
        cp page.mod
        jr z,@nz

        bit 6,h
        set 6,h
        jr nz,@nz
        dec a
     @nz:
        ; now pointing to address before gap

        ld (@target.lo + 1),hl
        ld (@target.hi + 1),a

        ; so lddr copy sample from old position to higher address

        @copy.loop:

         @result.lo:
            ld hl,0
         @result.hi:
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
            ld (@result.lo + 1),hl
            ld (@result.hi + 1),a

            ld a,b
            or c
            jr z,@nocopy

         @source.lo:
            ld hl,0
         @source.hi:
            ld a,0
            call set.high.memory.a

            ld de,move.spc + move.size - 1
            push bc

            lddr                        ; copy from source to buffer

         @page.mod.5:
            cp page.mod
            jr z,@nz

            bit 6,h
            set 6,h
            jr nz,@nz
            dec a
         @nz:
            ld (@source.hi + 1),a
            ld (@source.lo + 1),hl

            ld hl,move.spc + move.size - 1
         @target.lo:
            ld de,0
         @target.hi:
            ld a,0
            call set.high.memory.a

            pop bc

            lddr                        ; copy from buffer to target

         @page.mod.6:
            cp page.mod
            jr z,@nz

            bit 6,d
            set 6,d
            jr nz,@nz
            dec a
         @nz:
            ld (@target.hi + 1),a
            ld (@target.lo + 1),de
         @nocopy:
            ld a,(@result.hi + 1)
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
        cp @st.loop.small
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
        cp @st.loop.small   ; small loop needs three gaps
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

        call add.bhl.de

     @contnorm:

        call add.bhl.de
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
    call nz,@fill.gap.sample.small_loop
    pop af
    call z,@fill.gap.sample.big_loop

    ret

;-------------------------------------------------------------------------------
@fill.gap.sample.small_loop:

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
 @loop.big:
    add hl,bc           ; -> possibly 2 * ( gap - 1 )
    ld a,(tracker.gap.low)
    cp h
    jr nc,@-loop.big
    pop af

    add hl,de
    bit 6,h
    res 6,h
    jr z,$+3
    inc a
    ld (iy + st.loope + 0),l
    ld (iy + st.loope + 1),h
    ld (iy + st.loope + 2),a

    ; keep copying loop until 3 * gap space is filled

    @loop.copy.small_loop:

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

        jr @-loop.copy.small_loop

 @last.partial:

    add hl,bc
    ld c,l
    ld b,h
    ld hl,(@sm.lp.src + 1)
    ldir

    ret

;-------------------------------------------------------------------------------
@fill.gap.sample.big_loop:

    ld (iy + st.loops + 0),l
    ld (iy + st.loops + 1),h
    ld (iy + st.loops + 2),b

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
    include "loader/mod.s"

;---------------------------------------------------------------
add.bhl.de:

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

fs.high:
rs.bp.page:
    ld a,0      ;burst page
    or low.memory.ram.0
    out (port.lmpr),a
    ld sp,0x8000

    ld a,c
@c1.bp.offset:
    ld (0),hl                   ; 2e36
@c1.bp.page:
    ld (0),a                    ; 2e34
    ld (@c1 + len + 1),hl       ; 9f87
    ld (@c1 + page.len + 1),a   ; 9f84

    ld (@c1 + sample.page + 1),a
    ld (@c1 + sample.offset + 1),hl

@c2.bp.offset:
    ld (0),hl
@c2.bp.page:
    ld (0),a
    ld (@c2 + len + 1),hl
    ld (@c2 + page.len + 1),a

    ld (@c2 + sample.page + 1),a
    ld (@c2 + sample.offset + 1),hl

@c3.bp.offset:
    ld (0),hl
@c3.bp.page:
    ld (0),a
    ld (@c3 + len + 1),hl
    ld (@c3 + page.len + 1),a

    ld (@c3 + sample.page + 1),a
    ld (@c3 + sample.offset + 1),hl

@c4.bp.offset:
    ld (0),hl
@c4.bp.page:
    ld (0),a
    ld (@c4 + len + 1),hl
    ld (@c4 + page.len + 1),a

    ld (@c4 + sample.offset + 1),hl
    ld (@c4 + sample.page + 1),a


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

;put correct maximum & minimum periods into tracker

    ld hl,113
    ld de,856
    ld a,(tracker.octaves)
    cp 5
    jr nz,mm.per.3
    ld hl,56
    ld de,1023              ; octave 0 not fully supported
mm.per.3:
    ld (@c1 + period.min + 1 ),hl
    ld (@c1 + period.max + 1 ),de
    ld (@c2 + period.min + 1 ),hl
    ld (@c2 + period.max + 1 ),de
    ld (@c3 + period.min + 1 ),hl
    ld (@c3 + period.max + 1 ),de
    ld (@c4 + period.min + 1 ),hl
    ld (@c4 + period.max + 1 ),de

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
    ld (pat.delay.c+1),a
    ld (song.pos),a
    ld (pattern.pos),a
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
    call @c1 + bp.volume
    call @c2 + bp.volume
    call @c3 + bp.volume
    call @c4 + bp.volume

    call @c1 + period.nop
    call @c2 + period.nop
    call @c3 + period.nop
    call @c4 + period.nop

;---------------------------------------------------------------

im.stsp:
    ld sp,0

im.lmpr:
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

reset.list:
    defw @c1 + @smc.repeat + 1
    defb 0
    defw @c1 + @smc.volume + 1
    defb 0
    defw @c1 + period + 1
    defb 0
    defw @c1 + period + 2
    defb 0
    defw @c1 + wanted.per + 1
    defb 0
    defw @c1 + wanted.per + 2
    defb 0
    defw @c1 + upmask + 1
    defb 0xff
    defw @c1 + dnmask + 1
    defb 0xff
    defw @c1 + wav.cntrl + 1
    defb 0
    defw @c1 + tonespeed + 1
    defb 0
    defw @c1 + gliss + 1
    defb 0
    defw @c1 + vibr.cmnd + 1
    defb 0
    defw @c1 + vibr.pos + 1
    defb 0
    defw @c1 + trem.cmnd + 1
    defb 0
    defw @c1 + trem.pos + 1
    defb 0
    defw @c1 + sampoffs + 1
    defb 0
    defw @c1 + loopcount+1
    defb 0
    defw @c1 + pattpos+1
    defb 0
    defw @c1 + new.ins+1
    defb 0
    defw @c1 + cur.ins+1
    defb 0
    defw 0

;===============================================================================
; tracker

    defs align 0x100

pattern.table:
    defs 0x100

sample.table:

    org 0   ; offsets

 st.start:      defw 0x0000 ; offset
                defb 0x00   ; page

 st.end:        defw 0x0000 ; offset
                defb 0x00   ; page = start gap

 st.loop:       defb 0x00
    @st.loop.none:  equ 0
    @st.loop.big:   equ 1
    @st.loop.small: equ 2

 st.loope:                  ; small -> loop end in gap
 st.loops:                  ; big   -> loop start
                defw 0x0000
                defb 0x00

 st.vol:        defb 0x00   ; volume
 st.finetune:   defb 0x00   ; fine tune value

 st.sample:     defb 0x00   ; empty sample?
                defb 0x00   ; \
                defb 0x00   ;  ) unused
                defb 0x00   ; /

 sample.table.len:    equ $
    assert sample.table.len == 16

    org $ + sample.table

st.prev.loop:   equ sample.table.len - st.loop
st.prev.start:  equ sample.table.len - st.start
st.prev.sample: equ sample.table.len - st.sample

    defs 31 * sample.table.len


finet.tab:  defs 1024

;-------------------------------------------------------------------------------
finelist:

    defw 856,808,762,720,678,640,604,570,538,508,480,453
    defw 428,404,381,360,339,320,302,285,269,254,240,226
    defw 214,202,190,180,170,160,151,143,135,127,120,113    ;tuning 0
    defw 107,101, 95, 90, 85, 80, 75, 71, 67, 63, 60, 56

    defs 128 - 96

    defw 850,802,757,715,674,637,601,567,535,505,477,450
    defw 425,401,379,357,337,318,300,284,268,253,239,225
    defw 213,201,189,179,169,159,150,142,134,126,119,113    ;tuning 1
    defw 106,100, 94, 89, 84, 79, 75, 71, 67, 63, 59, 56

    defs 128 - 96

    defw 844,796,752,709,670,632,597,563,532,502,474,447
    defw 422,398,376,355,335,316,298,282,266,251,237,224
    defw 211,199,188,177,167,158,149,141,133,125,118,112    ;tuning 2
    defw 105, 99, 94, 88, 83, 79, 74, 70, 66, 62, 59, 56

    defs 128 - 96

    defw 838,791,746,704,665,628,592,559,528,498,470,444
    defw 419,395,373,352,332,314,296,280,264,249,235,222
    defw 209,198,187,176,166,157,148,140,132,125,118,111    ;tuning 3
    defw 104, 99, 93, 88, 83, 78, 74, 70, 66, 62, 59, 55

    defs 128 - 96

    defw 832,785,741,699,660,623,588,555,524,495,467,441
    defw 416,392,370,350,330,312,294,278,262,247,233,220
    defw 208,196,185,175,165,156,147,139,131,124,117,110    ;tuning 4
    defw 104, 98, 92, 87, 82, 78, 73, 69, 65, 62, 58, 55

    defs 128 - 96

    defw 826,779,736,694,655,619,584,551,520,491,463,437
    defw 413,390,368,347,328,309,292,276,260,245,232,219
    defw 206,195,184,174,164,155,146,138,130,123,116,109    ;tuning 5
    defw 103, 97, 92, 87, 82, 77, 73, 69, 65, 61, 58, 54

    defs 128 - 96

    defw 820,774,730,689,651,614,580,547,516,487,460,434
    defw 410,387,365,345,325,307,290,274,258,244,230,217
    defw 205,193,183,172,163,154,145,137,129,122,115,109    ;tuning 6
    defw 102, 96, 91, 86, 81, 77, 72, 68, 64, 61, 57, 54

    defs 128 - 96

    defw 814,768,725,684,646,610,575,543,513,484,457,431
    defw 407,384,363,342,323,305,288,272,256,242,228,216
    defw 204,192,181,171,161,152,144,136,128,121,114,108    ;tuning 7
    defw 102, 96, 90, 85, 80, 76, 72, 68, 64, 60, 57, 54

    defs 128 - 96

    defw 907,856,808,762,720,678,640,604,570,538,508,480
    defw 453,428,404,381,360,339,320,302,285,269,254,240
    defw 226,214,202,190,180,170,160,151,143,135,127,120    ;tuning -8
    defw 113,107,101, 95, 90, 85, 80, 75, 71, 67, 63, 60

    defs 128 - 96

    defw 900,850,802,757,715,675,636,601,567,535,505,477
    defw 450,425,401,379,357,337,318,300,284,268,253,238
    defw 225,212,200,189,179,169,159,150,142,134,126,119    ;tuning -7
    defw 112,106,100, 94, 89, 84, 79, 75, 71, 67, 63, 59

    defs 128 - 96

    defw 894,844,796,752,709,670,632,597,563,532,502,474
    defw 447,422,398,376,355,335,316,298,282,266,251,237
    defw 223,211,199,188,177,167,158,149,141,133,125,118    ;tuning -6
    defw 111,105, 99, 94, 88, 83, 79, 74, 70, 66, 62, 59

    defs 128 - 96

    defw 887,838,791,746,704,665,628,592,559,528,498,470
    defw 444,419,395,373,352,332,314,296,280,264,249,235
    defw 222,209,198,187,176,166,157,148,140,132,125,118    ;tuning -5
    defw 111,104, 99, 93, 88, 83, 78, 74, 70, 66, 62, 59

    defs 128 - 96

    defw 881,832,785,741,699,660,623,588,555,524,494,467
    defw 441,416,392,370,350,330,312,294,278,262,247,233
    defw 220,208,196,185,175,165,156,147,139,131,123,117    ;tuning -4
    defw 110,104, 98, 92, 87, 82, 78, 73, 69, 65, 61, 58

    defs 128 - 96

    defw 875,826,779,736,694,655,619,584,551,520,491,463
    defw 437,413,390,368,347,328,309,292,276,260,245,232
    defw 219,206,195,184,174,164,155,146,138,130,123,116    ;tuning -3
    defw 109,103, 97, 92, 87, 82, 77, 73, 69, 65, 61, 58

    defs 128 - 96

    defw 868,820,774,730,689,651,614,580,547,516,487,460
    defw 434,410,387,365,345,325,307,290,274,258,244,230
    defw 217,205,193,183,172,163,154,145,137,129,122,115    ;tuning -2
    defw 108,102, 96, 91, 86, 81, 77, 72, 68, 64, 61, 57

    defs 128 - 96

    defw 862,814,768,725,684,646,610,575,543,513,484,457
    defw 431,407,384,363,342,323,305,288,272,256,242,228
    defw 216,203,192,181,171,161,152,144,136,128,121,114    ;tuning -1
    defw 108,101, 96, 90, 85, 80, 76, 72, 68, 64, 60, 57

    defs 128 - 96

;-------------------------------------------------------------------------------
;first 64 bytes of bpm table are not used, so use this space
;for arpeggio table (32 bytes) and vibrato table (32 bytes) instead.

bpm.table:  ;x / 125 * 256 (125 = standard bpm)

arpeggio.table: ;x mod 3
    defb 0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0
    defb 1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1

vibrato.table:
    defb   0, 24, 49, 74, 97,120,141,161
    defb 180,197,212,224,235,244,250,253
    defb 255,253,250,244,235,224,212,197
    defb 180,161,141,120, 97, 74, 49, 24

    ; defw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;  0
    ; defw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; 16
    defw  66, 68, 70, 72, 74, 76, 78, 80 ; 32
    defw  82, 84, 87, 89, 91, 93, 95, 97 ; 40
    defw  99,101,103,105,107,109,111,113 ; 48
    defw 115,117,119,121,123,125,127,130 ; 56
    defw 132,134,136,138,140,142,144,146 ; 64
    defw 148,150,152,154,156,158,160,162 ; 72
    defw 164,166,168,170,173,175,177,179 ; 80
    defw 181,183,185,187,189,191,193,195 ; 88
    defw 197,199,201,203,205,207,209,211 ; 96
    defw 213,216,218,220,222,224,226,228 ;104
    defw 230,232,234,236,238,240,242,244 ;112
    defw 246,248,250,252,254             ;120
    defw                    256          ;125 0x0100
    defw                         259,261 ;126
    defw 263,265,267,269,271,273,275,277 ;128
    defw 279,281,283,285,287,289,291,293 ;136
    defw 295,297,300,302,304,306,308,310 ;144
    defw 312,314,316,318,320,322,324,326 ;152
    defw 328,330,332,334,336,338,340,343 ;160
    defw 345,347,349,351,353,355,357,359 ;168
    defw 361,363,365,367,369,371,373,375 ;176
    defw 377,379,381,383,386,388,390,392 ;184
    defw 394,396,398,400,402,404,406,408 ;192
    defw 410,412,414,416,418,420,422,424 ;200
    defw 426,429,431,433,435,437,439,441 ;208
    defw 443,445,447,449,451,453,455,457 ;216
    defw 459,461,463,465,467,469,472,474 ;224
    defw 476,478,480,482,484,486,488,490 ;232
    defw 492,494,496,498,500,502,504,506 ;240
    defw 508,510,512,515,517,519,521,523 ;248

;-------------------------------------------------------------------------------
retrig.table:   ;tick / x = int ( tick / x )
    defb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  ;x=0
    defb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

    defb 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1  ;x=1
    defb 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

    defb 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0  ;x=2
    defb 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0

    defb 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1  ;x=3
    defb 0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0

    defb 1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0  ;x=4
    defb 1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0

    defb 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1  ;x=5
    defb 0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0

    defb 1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0  ;x=6
    defb 0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0

    defb 1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0  ;x=7
    defb 0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0

    defb 1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0  ;x=8
    defb 1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0

    defb 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0  ;x=9
    defb 0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0

    defb 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0  ;x=A
    defb 0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0

    defb 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0  ;x=B
    defb 0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0

    defb 1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0  ;x=C
    defb 0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0

    defb 1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0  ;x=D
    defb 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0

    defb 1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0  ;x=E
    defb 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0

    defb 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1  ;x=F
    defb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0

;-------------------------------------------------------------------------------
tracker:

    call @c1 + update.bp    ;check for sample boundaries
    call @c2 + update.bp    ;and loop the samples if
    call @c3 + update.bp    ;necessary
    call @c4 + update.bp

    ld hl,vol.update        ;ensures instant response to
    ld a,(hl)               ;a channel being toggled on
    or a                    ;or off
    jr z,@no.update

    call @c1 + bp.volume
    call @c2 + bp.volume
    call @c3 + bp.volume
    call @c4 + bp.volume

    ld (hl),0

 @no.update:

    ld hl,countint
    inc (hl)

    ld a,(int.rtn.pag)
    inc a
    jr z,@no.extra.int
    dec a
    ld hl,(int.routine)
    ld c,a
    call far.call

 @no.extra.int:

    ld a,(mstatus)
    dec a
    ret z

    ld hl,(tick.fraction)
    ld a,h
    ld de,(tempo)
    add hl,de
    ld (tick.fraction),hl
    cp h                    ;tick int not changed
    ret z

    ld a,(speed)
    ld c,a
    ld a,h
    sub c
    jr c,@no.new.note       ; tick <> speed

    ld (tick),a
    ld a,(pat.delay.c+1)    ; for pattern delay command
    or a
    jr z,@get.new.note      ; get note data if no delay

    call @no.new.all        ; else just do fx
    jp dskip

 @no.new.note:              ; tick <> speed
    call @no.new.all        ; do fx
    jp nonewposyet          ; check position change

;-------------------------------------------------------------------------------
@no.new.all:

    ;no new note data for all channels - fx only

    call @c1 + check.fx
    call @c2 + check.fx
    call @c3 + check.fx
    jp @c4 + check.fx

;-------------------------------------------------------------------------------
@get.new.note:

    ld a,(song.pos)
    ld l,a
    ld h,pattern.table / 0x100
    ld a,(hl)               ;get pattern
    ld (pattern.num),a
    ld d,a
    and 0xf0
    rlca
    rlca
    rlca
    rlca
    ld c,a
    ld a,(tracker.ptr.page.mod)
    add c
    ld c,a

    ld a,d
    and 0x0f
    add a,a
    add a,a
 origpat.offsh:
    add a,0                 ;pattern offset hi byte
    ld d,a
 origpat.offsl:
    ld e,0                  ;pattern offset lo byte
    bit 6,d
    res 6,d
    jr z,$+3
    inc c

    ld a,(pattern.pos)
    add a
    add a
    ld h,0
    ld l,a
    add hl,hl
    add hl,hl               ;*16
    add hl,de
    ld a,c

    call get.pattern        ;in lower memory

    call @c1 + play.voice
    call @c2 + play.voice
    call @c3 + play.voice
    call @c4 + play.voice

 dskip:
    ld hl,pattern.pos
    inc (hl)

 pat.delay.f:
    ld a,0
    or a
    jr z,no.new.patdel

    ld (pat.delay.c+1),a
    xor a
    ld (pat.delay.f+1),a

 no.new.patdel:

 pat.delay.c:
    ld a,0
    or a
    jr z,no.pat.delay

    dec a
    ld (pat.delay.c+1),a
    jr z,no.pat.delay

    dec (hl)                ;if pat delay -> undo inc (hl)

 no.pat.delay:
 pbreak.flag:
    ld a,0
    or a
    jr z,nnpysk

    xor a
    ld (pbreak.flag+1),a
 pbreak.pos:
    ld a,0
    ld (hl),a
    xor a
    ld (pbreak.pos+1),a

 nnpysk:

    ld a,(hl)
    cp 64
    jr c,nonewposyet

 next.position:
    ld a,(pbreak.pos+1)
    ld (pattern.pos),a
    xor a
    ld (pbreak.pos+1),a
    ld (posjump.flag+1),a
    ld hl,song.pos
    inc (hl)
    ld a,(hl)
    bit 7,a
    jr nz,loop.time   ;reached song position 128

 song.len:
    cp 0
    jr nz,nonewposyet

 loop.time:

    call reset.speed

 play.status:
    ld a,(disable.pos) ;0=keep repeating
    or a
    ret z

 quit:

    ld (mstatus),a
    ret

 nonewposyet:
 posjump.flag:
    ld a,0
    ;init=0, "B"=1, "D"=1

    or a
    jr nz,next.position

    ret

    defs align 32

;-------------------------------------------------------------------------------
routines:

; these routines are copied four times so that there is one for each channel the
; burst player addresses are put in by conv.list

;===============================================================================
    org 0

;tables for effect parsing

 chkmore.list:
    ; effects on tick 0
    r0.000: defw period.nop             ; 0
    r0.001: defw period.nop             ; 1
    r0.002: defw period.nop             ; 2
    r0.003: defw period.nop             ; 3 check earlier (tone porta)
    r0.004: defw period.nop             ; 4
    r0.005: defw period.nop             ; 5 check earlier (tone porta)
    r0.006: defw period.nop             ; 6
    r0.007: defw period.nop             ; 7
    r0.008: defw period.nop             ; 8
    r0.009: defw @effect.sample_offset  ; 9
    r0.010: defw period.nop             ; A
    r0.011: defw @effect.position_jump  ; B
    r0.012: defw @effect.set_volume     ; C
    r0.013: defw @effect.pattern_break  ; D
    r0.014: defw @effect.extended       ; E
    r0.015: defw @effect.set_speed      ; F

 checkfx.list:
    ; effects not on tick 0
    r0.016: defw @effect.arpeggio               ; 0
    r0.017: defw @effect.portamento_up          ; 1
    r0.018: defw @effect.portamento_down        ; 2
    r0.019: defw @effect.tone_portamento        ; 3
    r0.020: defw @effect.vibrato                ; 4
    r0.021: defw @effect.tone_volume_slide      ; 5
    r0.022: defw @effect.vibrato_volume_slide   ; 6
    r0.023: defw @effect.tremolo                ; 7
    r0.024: defw @effect.none                   ; 8
    r0.025: defw @effect.none                   ; 9
    r0.026: defw @effect.volume_slide           ; A
    r0.027: defw @effect.none                   ; B
    r0.028: defw @effect.none                   ; C
    r0.029: defw @effect.none                   ; D
    r0.030: defw @effect.extended               ; E
    r0.031: defw @effect.none                   ; F

 extended.effects.list:
    ; extended effects (effect E)
    r0.032: defw @eeffect.filter            ; 0
    r0.033: defw @eeffect.fineportup        ; 1
    r0.034: defw @eeffect.fineportdn        ; 2
    r0.035: defw @eeffect.glisscntrl        ; 3
    r0.036: defw @eeffect.vibracntrl        ; 4
    r0.037: defw @eeffect.setfinetun        ; 5
    r0.038: defw @eeffect.jumploop          ; 6
    r0.039: defw @eeffect.tremocntrl        ; 7
    r0.040: defw @effect.none               ; 8 not a command
    r0.041: defw @eeffect.retrignote        ; 9
    r0.042: defw @eeffect.volfineup         ; A
    r0.043: defw @eeffect.volfinedn         ; B
    r0.044: defw @eeffect.notecut           ; C
    r0.045: defw @eeffect.notedelay         ; D
    r0.046: defw @eeffect.pattdelay         ; E
    r0.047: defw @effect.none               ; F funk it not supported

;-------------------------------------------------------------------------------
update.bp:
    ; update sample addresses in burstplayer

 @bp.page.1:
    ld a,(0)
 page.len:
    sub 0
    ret c               ; not past marker yet (page)
 len:
    ld de,0
 @bp.offset.1:
    ld hl,(0)
    jr z,$+4
    set 6,h
    sbc hl,de           ; cf not set
    ret c               ; not past marker yet (offs)

 @smc.repeat:
    jr $+2              ; can be $+2, big_loop or small_loop
    ex de,hl
 r1.001:
    ld a,(page.len+1)
 @bp.page.2:
    ld (0),a
 @bp.offset.2:
    ld (0),hl
    ret

 big_loop:
 big_loop.page:
    ld a,0
 @bp.page.3:
    ld (0),a
 big_loop.offset:
    ld de,0
    add hl,de
 @bp.offset.3:
    ld (0),hl
    ret

 small_loop:
 small_loop.page:
    ld a,0
 @bp.page.4:
    ld (0),a
 small_loop.offset:
    ld de,0
    add hl,de
 @bp.offset.4:
    ld (0),hl

 sm.en.page:
    ld a,0
 sm.en.offs:
    ld hl,0
 r1.002:
    ld (len+1),hl
 r1.003:
    ld (page.len+1),a
    ret

 small_loop.jr: equ small_loop - @smc.repeat - 2
 big_loop.jr: equ big_loop - @smc.repeat - 2

;-------------------------------------------------------------------------------
play.voice:

    ; hl = pattern row
    ;
    ; SPppsECC
    ;
    ; S   s     = sample
    ;  Ppp      = note period
    ;      E    = effect
    ;       xy  = parameter

 mk.cur.pat:
    ld hl,0             ; hl = pattern row

    ld a,(hl)           ; a = SP
    inc l
    and 0x0f            ; a = P
    ld d,a              ; d = P
    ld e,(hl)           ; e = pp
    dec l
 r2.001:
    ld (note+1),de      ; -> Ppp
    or e                ; if no period given then use
 r1.004:
    call z,period.nop   ; last given period

    ex de,hl            ; DE = pattern row

    ld h,sample.table / 0x100
    ld a,(de)           ; SP
    and 0x10            ; S
    jr z,@sample.lt.16  ; 16 bytes per sample table entry
    inc h
 @sample.lt.16:
    inc e
    inc e
    ld a,(de)           ; a = sE
    and 0xf0            ; a = s
    ld l,a              ; l = s -> hl = sample.table entry (16 bytes per entry)
                        ; de = pattern row + 2

    ld c,(hl)           ; sample offset
    inc l
    ld b,(hl)           ; sample offset
    inc l
    ld a,(hl)           ; sample page
    inc a

 r1.005:
    jp z,set.regs       ; page -1 -> no sample
    dec a
    inc l
 r2.002:
    ld (sample.offset+1),bc
 r1.006:
    ld (sample.page+1),a

    ld a,l              ; highest 4 bits are significant
    xor h               ; lowest bit is significant, -> xor = unique id

 r1.007:
    ld (new.ins+1),a

    ld c,(hl)           ; sample end offset
    inc l
    ld b,(hl)           ; sample end offset
    inc l
    ld a,(hl)           ; sample end page
    inc l
 r2.003:
    ld (len+1),bc
 r1.008:
    ld (page.len+1),a

    ld a,(hl)           ; sample loop type
    inc l

    dec a
    jr z,get.big
    dec a
    jr z,get.small
    inc l
    inc l
    inc l
    xor a
    jr got.loop

 get.small:
 r1.009:
    ld a,(page.len+1)
 r1.010:
    ld (small_loop.page+1),a
 r2.004:
    ld (small_loop.offset+1),bc
    ld c,(hl)
    inc l
    ld b,(hl)
    inc l
    ld a,(hl)
    inc l
 r1.011:
    ld (sm.en.page+1),a
 r2.005:
    ld (sm.en.offs+1),bc
    ld a,small_loop.jr
    jr got.loop

 get.big:
    ld c,(hl)
    inc l
    ld b,(hl)
    inc l
    ld a,(hl)
    inc l              ;start of repeat
 r1.012:
    ld (big_loop.page + 1),a
 r2.006:
    ld (big_loop.offset + 1),bc
    ld a,big_loop.jr

 got.loop:
 r1.013:
    ld (@smc.repeat + 1),a

    ld a,(hl)
    inc l
 r1.014:
    ld (@smc.volume + 1),a
 r1.015:
    call bp.volume

    ld a,(hl)
 r1.016:
    ld (finetune + 1),a

 set.regs:
    ex de,hl            ; hl = pattern row + 2

    ld a,(hl)           ; a = sE
    inc l               ; hl = pattern row + 3
    and 0x0f            ; a = _E
 r1.017:
    ld (command + 1),a
    ld c,a              ; c = _E
    ld a,(hl)           ; a = xy
 r1.018:
    ld (cmdlo + 1),a
    ld b,a              ; b = xy

 r2.007:
    ld de,(note+1)
    ld a,d
    or e
 r1.019:
    jp z,chknewins

    ld a,b              ; a = xy
    and 0xf0            ; a = x_
    or c                ; c = _E hi = param, lo = command
    cp 0x5e             ; E5 = fine tune
    jr z,do.fine
    ld a,c              ; a = _E (only command now)
    cp 3                ; 3 = tone portamento
    jr z,chk.tone
    cp 5                ; 5 = tone port + vol slide
    jr z,chk.tone
    jr set.per

 do.fine:
 r1.020:
    call @eeffect.setfinetun
    jr set.per

 chk.tone:
 r1.021:
    call set.tone
 r1.022:
    jp chkmorefx

 set.per:
 r1.023:
    ld hl,(note+1)
    ld a,h
    add finet.tab / 256
    ld h,a

    ld l,(hl)           ; get note number (*2)
    ld b,l
    inc l               ; 255 = note not found
    jr nz,foundfine

 r1.024:
    ld hl,(note+1)
    jr notune

 foundfine:
    dec l
 finetune:
    ld a,0
    srl a
    jr nc,$+4
    set 7,l
    add finelist / 256
    ld h,a

    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
 notune:
 r1.025:
    ld (period+1),hl
    ld a,b
 r1.026:
    ld (note.num+1),a   ; 0-71, 255=unknown

 r1.027:
    ld a,(cmdlo+1)
    and 0xf0
    ld c,a
 r1.028:
    ld a,(command+1)    ; a = _E
    or c
    cp 0xde             ; ED = note delay
    jr z,chkmorefx

 wav.cntrl:
    ld c,0
    xor a
    bit 2,c             ;-> retrigger vibrato
    jr z,vibnoc

 r1.029:
    ld (vibr.pos+1),a
 vibnoc:
    bit 6,c             ;-> retrigger tremolo
    jr z,trenoc

 r1.030:
    ld (trem.pos+1),a
 trenoc:

 sample.offset:
    ld hl,0
 sample.page:
    ld a,0
 @bp.page.5:
    ld (0),a
 @bp.offset.5:
    ld (0),hl
    xor a
 @bp.speed.fraction.1:
    ld (0),a

 period:
    ld de,0
 r1.031:
    call period.nop.de

 r1.032:
    ld a,(new.ins+1)
 r1.033:
    ld (cur.ins+1),a

    ; ld a,1
    ; ld (trigger),a

 chkmorefx:
 r1.034:
    ld hl,chkmore.list
 r1.035:
    ld a,(command+1)
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    jp (hl)

 chknewins:
 new.ins:
    ld a,0
 cur.ins:
    cp 0
    jr z,chkmorefx

 r1.036:
    ld (cur.ins+1),a
 r1.037:
    ld hl,(sample.offset+1)
 r1.038:
    ld a,(sample.page+1)
 @bp.page.6:
    ld (0),a
 @bp.offset.6:
    ld (0),hl
    xor a
 @bp.speed.fraction.2:
    ld (0),a
    jr chkmorefx

 check.fx:
 command:
    ld c,0
 cmdlo:
    ld a,0
    or c
    jr z,period.nop     ; no command - use old period in case of arpeg

 r1.039:
    ld hl,checkfx.list
    ld a,c
    add a,a
    add a,l
    ld l,a
    ; jr nc,$+4         ;within boundary
    ; inc h
    ld a,(hl)
    inc l               ;within boundary
    ld h,(hl)
    ld l,a
    jp (hl)

;-------------------------------------------------------------------------------
 period.nop:
 r2.008:
    ld de,(period+1)

 period.nop.de:
    sla e               ;convert pitch
    rl d
    ld a,d
    add pitch.table / 0x100
    ld d,a
    ld a,(de)
 @bp.speed.low:
    ld (0),a
    inc e
    ld a,(de)
 @bp.speed.high:
    ld (0),a

    ret

;-------------------------------------------------------------------------------
bp.volume:
 channel.on:
    ld a,(0)                    ; fill in variable
    or a
    jr z,@chan.off

 @smc.volume:
    ld a,0
    rra
 saa.exvol:
    and %01111111               ; %01111110 when SAA
 @chan.off:
    add volume.table \ 0x100    ; 3 when SAA channel 2/3
 @bp.volume_table:
    ld (0),a

    ret

;---------------------------------------------------------------
; Effect 0 - Arpeggio
;---------------------------------------------------------------
@effect.arpeggio:
    ld a,(tick)                 ; [1-31]
    ld h,arpeggio.table / 0x100 ; table on 256 boundary
    ld l,a
    ld a,(hl)                   ; -> a = tick mod 3
    or a
    jr z,@arpeggio.0

    cp 2
    jr z,@arpeggio.2

 r1.040:
    ld a,(cmdlo+1)
    and 0xf0                    ; 1
    rrca
    rrca
    rrca
    rrca
    jr @arpeggio.calc.period

 @arpeggio.2:
 r1.041:
    ld a,(cmdlo+1)
    and 0x0f                    ; 2
    jr @arpeggio.calc.period

 @arpeggio.0:
 r2.009:
    ld de,(period+1)            ; 0
    jr @arpeggio.got.period

 @arpeggio.calc.period:
    add a,a
 note.num:
    add 0
    ret c               ;note number unknown

    cp 2*36
    ret nc              ;new note too high

    ld l,a
 r1.042:
    ld a,(finetune+1)
    srl a
    jr nc,$+4
    set 7,l
    add a,finelist / 0x100
    ld h,a
    ld e,(hl)
    inc l
    ld d,(hl)

 @arpeggio.got.period:

    jr period.nop.de

;---------------------------------------------------------------
; Effect 1 - Portamento Up
;---------------------------------------------------------------
@effect.portamento_up:
 r1.043:
    ld hl,(period+1)
 r1.044:
    ld a,(cmdlo+1)
 upmask:
    and 0xff            ;change to 0x0f for fine porta
    ld c,a
    ld b,0
    ld a,0xff
 r1.045:
    ld (upmask+1),a
    sbc hl,bc
 period.min:
    ld bc,113           ;minimum Amiga period
    jr c,porttoofar
    sbc hl,bc
    jr nc,portauskip
 porttoofar:
    ld hl,0
 portauskip:
    add hl,bc
 r1.046:
    ld (period+1),hl
    ex de,hl
 r1.047:
    jp period.nop.de

;---------------------------------------------------------------
; Effect 2 - Portamento Down
;---------------------------------------------------------------
@effect.portamento_down:
 r1.048:
    ld hl,(period+1)
 r1.049:
    ld a,(cmdlo+1)
 dnmask:
    and 0xff            ;change to 0x0f by fine porta
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld a,0xff
 r1.050:
    ld (dnmask+1),a
 period.max:
    ld bc,856           ;maximum Amiga period
    or a
    sbc hl,bc
    jr c,portadskip
    ld hl,0
 portadskip:
    add hl,bc
 r1.051:
    ld (period+1),hl
    ex de,hl
 r1.052:
    jp period.nop.de

;---------------------------------------------------------------
set.tone:
 note:
    ld hl,0
    ld a,h
    add finet.tab / 256
    ld h,a

    ld l,(hl)           ;get note number (*2)
    ld b,l
    inc l               ;255 = note not found
    jr nz,foundfine2
 r1.053:
    ld hl,(note+1)
    jr notune2
 foundfine2:
    dec l
 r1.054:
    ld a,(finetune+1)
    srl a
    jr nc,$+4
    set 7,l
    add finelist / 256
    ld h,a

    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
 notune2:
 r1.055:
    ld (wanted.per+1),hl
 r2.010:
    ld de,(period+1)
    xor a
    sbc hl,de
    jr z,cleartone
    adc a,0
 r1.056:
    ld (tonedirec+1),a      ;0=porta dn, 1=porta up
    ret
 cleartone:
 r1.057:
    ld (wanted.per+1),hl
    ret

;---------------------------------------------------------------
; Effect 3 - Tone Portamento
;---------------------------------------------------------------
@effect.tone_portamento:
 r1.058:
    ld a,(cmdlo+1)
    or a
    jr z,tonenochng
 r1.059:
    ld (tonespeed+1),a      ; TODO: needs correction when BPM not 125 - see Nearly There by Jogeir (BPM 128) use tempo
    xor a
 r1.060:
    ld (cmdlo+1),a
 tonenochng:
 wanted.per:
    ld de,0
    ld a,d
    or e
    ret z
 tonespeed:
    ld bc,0
 r1.061:
    ld hl,(period+1)
 tonedirec:
    ld a,0
    or a
    jr nz,toneportup
 toneportdn:
    add hl,bc
    sbc hl,de
    jr c,tonesetper
    jr portoff
 toneportup:
    sbc hl,bc
    jr c,portoff
    sbc hl,de
    jr nc,tonesetper
 portoff:
    ld hl,0
 r1.062:
    ld (wanted.per+1),hl
 tonesetper:
    add hl,de
 gliss:
    ld a,0
    or a
    jr z,glissskip

    ex de,hl

    ld c,0
 r1.063:
    ld a,(finetune+1)
    srl a
    jr nc,$+4
    set 7,c
    add finelist / 256
    ld b,a
 glissloop:
    ld a,(bc)
    inc c
    ld l,a
    ld a,(bc)
    inc c
    ld h,a
    or a
    sbc hl,de
    jr c,glissfound
    ld a,c
    and %01111111
    cp 2*36
    jr c,glissloop
 glissfound:
    dec c
    ld a,(bc)
    ld h,a
    dec c
    ld a,(bc)
    ld l,a
 glissskip:
 r1.064:
    ld (period+1),hl
    ex de,hl
 r1.065:
    jp period.nop.de

;---------------------------------------------------------------
; Effect 4 - Vibrato
;---------------------------------------------------------------
@effect.vibrato:
 r1.066:
    ld a,(cmdlo+1)
    or a
    jr z,vibrato2
 vibr.cmnd:
    ld b,0
    and 0x0f
    jr z,vibskip
    ld c,a
    ld a,b
    and 0xf0
    or c
    ld b,a
 vibskip:
 r1.067:
    ld a,(cmdlo+1)
    and 0xf0
    jr z,vibskip2
    ld c,a
    ld a,b
    and 0x0f
    or c
    ld b,a
 vibskip2:
    ld a,b
 r1.068:
    ld (vibr.cmnd+1),a
 vibrato2:
 vibr.pos:
    ld a,0
    rrca
    rrca
    and 0x1f
    ld b,a
 r1.069:
    ld a,(wav.cntrl+1)
    and 0x03
    jr z,vib.sine
    sla b
    sla b
    sla b
    dec a
    jr z,vib.ramp     ;                 _ _ _
    ld e,255          ;square waveform   _ _ _
    jr vib.set
 vib.ramp:
 r1.070:
    ld a,(vibr.pos+1)
    bit 7,a
    jr nz,vib.ramp2
    ld a,255                    ;rampdown waveform \ \ \ \
    sub b                       ;                   \ \ \ \
    ld e,a
    jr vib.set
 vib.ramp2:
    ld e,b
    jr vib.set
 vib.sine:
    ld h,vibrato.table / 256
    ld l,b                      ;sine waveform  /\  /\
    set 5,l                 ;table offset 32  \/  \/
    ld e,(hl)
 vib.set:
    ld hl,0
 r1.071:
    ld a,(vibr.cmnd+1)
    and 0x0f
    jr z,skip.mul
    ld b,a
    ld d,0
 vib.mul:
    add hl,de
    djnz vib.mul
 skip.mul:
    sla l
    rl h
    ld b,h
 r1.072:
    ld hl,(period+1)
 r1.073:
    ld a,(vibr.pos+1)
    bit 7,a
    jr nz,vibr.neg
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    jr vibrato3
 vibr.neg:
    ld a,l
    sub b
    ld l,a
    jr nc,$+3
    dec h
 vibrato3:
    ex de,hl
 r1.074:
    call period.nop.de
 r1.075:
    ld a,(vibr.cmnd+1)
    rrca
    rrca
    and %00111100
 r1.076:
    ld hl,vibr.pos+1
    add (hl)
    ld (hl),a
    ret

;---------------------------------------------------------------
; Effect 5 - Tone and Volume Slide
;---------------------------------------------------------------
@effect.tone_volume_slide:
 r1.077:
    call tonenochng
 r1.078:
    jp volslide

;---------------------------------------------------------------
; Effect 6 - Vibrato and Volume Slide
;---------------------------------------------------------------
@effect.vibrato_volume_slide:
 r1.079:
    call vibrato2
 r1.080:
    jp volslide

;---------------------------------------------------------------
; Effect 7 - Tremolo
;---------------------------------------------------------------
@effect.tremolo:
 r1.081:
    call period.nop
 tremolo:
 r1.082:
    ld a,(cmdlo+1)
    or a
    jr z,tremolo2
 trem.cmnd:
    ld b,0
    and 0x0f
    jr z,treskip
    ld c,a
    ld a,b
    and 0xf0
    or c
    ld b,a
 treskip:
 r1.083:
    ld a,(cmdlo+1)
    and 0xf0
    jr z,treskip2
    ld c,a
    ld a,b
    and 0x0f
    or c
    ld b,a
 treskip2:
    ld a,b
 r1.084:
    ld (trem.cmnd+1),a
 tremolo2:
 trem.pos:
    ld a,0
    rrca
    rrca
    and 0x1f
    ld b,a
 r1.085:
    ld a,(wav.cntrl+1)
    rrca
    rrca
    rrca
    rrca
    and 0x03
    jr z,tre.sine
    sla b
    sla b
    sla b
    dec a
    jr z,tre.ramp
    ld e,255
    jr tre.set
 tre.ramp:
 r1.086:
    ld a,(trem.pos+1)
    bit 7,a
    jr nz,tre.ramp2
    ld a,255
    sub b
    ld e,a
    jr tre.set
 tre.ramp2:
    ld e,b
    jr tre.set
 tre.sine:
    ld h,vibrato.table / 256
    ld l,b
    set 5,l                 ;table offset 32
    ld e,(hl)
 tre.set:
    ld hl,0
 r1.087:
    ld a,(trem.cmnd+1)
    and 0x0f
    jr z,skiptremul
    ld b,a
    ld d,0
 tre.mul:
    add hl,de
    djnz tre.mul
 skiptremul:
    sla l
    rl h
 r1.088:
    ld a,(trem.pos+1)
    bit 7,a
 r1.089:
    ld a,(@smc.volume + 1)
    jr nz,trem.neg
    add h
    jr nc,$+4
    ld a,63
    cp 64
    jr c,$+4
    ld a,63
    jr tremolo3
 trem.neg:
    sub h
    jr nc,$+3
    xor a
 tremolo3:
    ld c,a
 r1.090:
    ld a,(@smc.volume + 1)
    ld b,a
    ld a,c

 r1.091:
    ld (@smc.volume + 1),a

 r1.092:
    call bp.volume

    ld a,b
 r1.093:
    ld (@smc.volume + 1),a

 r1.094:
    ld a,(trem.cmnd+1)
    rrca
    rrca
    and %00111100
 r1.095:
    ld hl,trem.pos+1
    add (hl)
    ld (hl),a
    ret

;---------------------------------------------------------------
; Effect 9 - Sample Offset  if offset too large -> start of loop
;
; - effect 9 without offset       -> use previous sample offset
; - note triggered without sample -> use previous sample offset
;
; more info - https://github.com/libxmp/libxmp/blob/master/docs/tracker_notes.txt
;           - https://github.com/steffest/BassoonTracker/blob/7e24c34b59c9c52304a90c42ccb6e8ed542c274e/script/src/tracker.js#L1028
;---------------------------------------------------------------
@effect.sample_offset:
 r1.096:
    ld a,(cmdlo+1)
    or a
    jr z,@sample_offset.zero
 r1.097:
    ld (sampoffs+1),a
 @sample_offset.zero:
 sampoffs:
    ld a,0
 r1.098:
    ld hl,(sample.offset+1)
    ld b,a
    and %11000000       ; %01000000 = 0x40 -> 0x4000 = 16384 = 1 page
    rlca
    rlca
    ld c,a              ; offset in pages
    ld a,b
    and %00111111       ; offset in bytes excluding pages
    add a,h
    ld h,a
 r1.099:
    ld a,(sample.page+1)
    add c
    bit 6,h             ; if pointer in bank D, move pointer down to bank C
    res 6,h
    jr z,$+3
    inc a               ; increasing page

    ex de,hl
    ld c,a
 r1.100:
    ld a,(page.len+1)
    sub c
    jr nc,@+ok
 r1.101:
    ld hl,(len+1)
    or a
    sbc hl,de
    jr nc,@+ok
    add c               ; -> a = (page.len+1)
    add hl,de           ; -> hl = (len+1)
    jr @bp.page.7
 @ok:
    ld a,c
    ex de,hl
 @bp.page.7:
    ld (0),a
 @bp.offset.7:
    ld (0),hl

    ret

;---------------------------------------------------------------
; Effect A - Volume Slide
;------------------------------------------------------------
@effect.volume_slide:
 r1.102:
    call period.nop

 volslide:
 r1.103:
    ld a,(cmdlo+1)
    and 0xf0
    jr z,volsli.dn

    rrca
    rrca
    rrca
    rrca
 volsli.up:
    ld b,a
 r1.104:
    ld a,(@smc.volume + 1)
    add b
    cp 0x40
    jr c,$+4
    ld a,0x3f
 r1.105:
    ld (@smc.volume + 1),a
 r1.106:
    jp bp.volume

 volsli.dn:
 r1.107:
    ld a,(cmdlo+1)
    and 0x0f
    ld b,a
 r1.108:
    ld a,(@smc.volume + 1)
    sub b
    jr nc,$+3
    xor a
 r1.109:
    ld (@smc.volume + 1),a
 r1.110:
    jp bp.volume

;---------------------------------------------------------------
; Effect B - Position Jump
;---------------------------------------------------------------
@effect.position_jump:
    ld a,(disable.pos)
    or a
    ret nz
 r1.111:
    ld a,(cmdlo+1)
    dec a
    ld (song.pos),a
 pj2:
    xor a
    ld (pbreak.pos+1),a
    inc a
    ld (posjump.flag+1),a
    ret

;-------------------------------------------------------------------------------
@effect.set_volume:

 ; Effect C - Set Volume

 ; https://www.un4seen.com/forum/?topic=14471.msg101020#msg101020
 ; SoundTracker sends volume directly to Paula, which means:
 ; - bit 7 ignored
 ; - bit 6 max volume

 ; -> move all bounds checking to setup mod?

 r1.112:
    ld a,(cmdlo+1)
    cp 0x40
    jr c,$+4
    ld a,0x3f
 r1.113:
    ld (@smc.volume + 1),a
 r1.114:
    jp bp.volume

;---------------------------------------------------------------
; Effect D - Pattern Break
;---------------------------------------------------------------
@effect.pattern_break:
 r1.115:
    ld a,(cmdlo+1)
    ld e,a
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    ld c,a
    add a,a
    add a,a
    add a,c
    add a,a   ;*10
    ld c,a
    ld a,e
    and 0x0f
    add c
    cp 64
    jr nc,pj2
    ld (pbreak.pos+1),a
    ld a,1
    ld (posjump.flag+1),a
    ret

;---------------------------------------------------------------
; Effect F - Set Speed
;            Sort of handles BPM alterations (not 100% accurate)
;---------------------------------------------------------------
@effect.set_speed:
 r1.116:
    ld a,(cmdlo+1)
    or a
    ret z
    cp 32                   ;speed <32
    jr nc,setbpm            ;so this is BPM
    ld (speed),a
    ; xor a
    ; ld (tick),a
    ret

 setbpm:
    ld h,bpm.table / 0x100
    add a,a
    jr nc,$+3
    inc h
    ld l,a
    ld e,(hl)
    inc l
    ld d,(hl)
    ld (tempo),de
    ret

;---------------------------------------------------------------
; Effect E - Extended
;            x = extended effect
;            y = parameter
;---------------------------------------------------------------
@effect.extended:
 r1.117:
    ld a,(cmdlo+1)
    and 0xf0
    rrca
    rrca
    rrca
 r1.118:
    ld hl,extended.effects.list
    add a,l
    ld l,a
    ; jr nc,$+3         ;within 256
    ; inc h
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    jp (hl)


@effect.none:           ; simply continue through to filter RET

;---------------------------------------------------------------
; Extended Effects
;---------------------------------------------------------------
; Extended Effect 0 - Filter On/Off         Amiga hardware rubbish

@eeffect.filter:
    ret

;---------------------------------------------------------------
; Extended Effect 1 - Fine Porta Up

@eeffect.fineportup:
    ld a,(tick)
    or a
    ret nz
    ld a,0x0f
 r1.119:
    ld (upmask+1),a
 r1.120:
    jp @effect.portamento_up

;---------------------------------------------------------------
; Extended Effect 2 - Fine Porta Down

@eeffect.fineportdn:
    ld a,(tick)
    or a
    ret nz
    ld a,0x0f
 r1.121:
    ld (dnmask+1),a
 r1.122:
    jp @effect.portamento_down

;---------------------------------------------------------------
; Extended Effect 3 - Set Gliss Control

@eeffect.glisscntrl:
 r1.123:
    ld a,(cmdlo+1)
    and 0x0f
 r1.124:
    ld (gliss+1),a
    ret

;---------------------------------------------------------------
; Extended Effect 4 - Set Vibrato Control

@eeffect.vibracntrl:
 r1.125:
    ld a,(cmdlo+1)
    and 0x0f
    ld b,a
 r1.126:
    ld hl,wav.cntrl+1
    ld a,(hl)
    and 0xf0
    or b
    ld (hl),a
    ret

;---------------------------------------------------------------
; Extended Effect 5 - Set Fine Tune

@eeffect.setfinetun:
 r1.127:
    ld a,(cmdlo+1)
    and 0x0f
 r1.128:
    ld (finetune+1),a
    ret

;---------------------------------------------------------------
; Extended Effect 6 - Jump Loop

@eeffect.jumploop:
    ld a,(tick)
    or a
    ret nz
 r1.129:
    ld a,(cmdlo+1)
    and 0x0f
    jr z,setloop
    ld b,a
 loopcount:
    ld a,0
    or a
    jr z,jump.cnt
    dec a
 r1.130:
    ld (loopcount+1),a
    ret z
 jmploop:
 pattpos:
    ld a,0
    ld (pbreak.pos+1),a
    ld a,1
    ld (pbreak.flag+1),a
    ret
 jump.cnt:
    ld a,b
 r1.131:
    ld (loopcount+1),a
    jr jmploop
 setloop:
    ld a,(pattern.pos)
 r1.132:
    ld (pattpos+1),a
    ret

;---------------------------------------------------------------
; Extended Effect 7 - Set Tremolo Control

@eeffect.tremocntrl:
 r1.133:
    ld a,(cmdlo+1)
    and 0x0f
    rlca
    rlca
    rlca
    rlca
    ld b,a
 r1.134:
    ld hl,wav.cntrl+1
    ld a,(hl)
    and 0x0f
    or b
    ld (hl),a
    ret

;---------------------------------------------------------------
; Extended Effect 9 - Retrig Note

@eeffect.retrignote:
 r1.135:
    ld a,(cmdlo+1)
    and 0x0f
    ret z
    ld hl,retrig.table
    bit 3,a
    res 3,a
    jr z,$+3
    inc h
    rrca
    rrca
    rrca
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld a,(tick)
    add a,l
    ld l,a            ;can't overflow
    ld a,(hl)
    or a
    ret z
 doretrig:
 r1.136:
    ld a,(new.ins+1)
 r1.137:
    ld (cur.ins+1),a
 r1.138:
    ld hl,(sample.offset+1)
 r1.139:
    ld a,(sample.page+1)
 @bp.page.8:
    ld (0),a
 @bp.offset.8:
    ld (0),hl
    ret

;---------------------------------------------------------------
; Extended Effect A - Volume Fine Up

@eeffect.volfineup:
    ld a,(tick)
    or a
    ret nz
 r1.140:
    ld a,(cmdlo+1)
    and 0x0f
 r1.141:
    jp volsli.up

;---------------------------------------------------------------
; Extended Effect B - Volume Fine Down

@eeffect.volfinedn:
    ld a,(tick)
    or a
    ret nz
 r1.142:
    ld a,(cmdlo+1)
    and 0x0f
 r1.143:
    jp volsli.dn

;---------------------------------------------------------------
; Extended Effect C - Note Cut

@eeffect.notecut:
 r1.144:
    ld a,(cmdlo+1)
    and 0x0f
    ld b,a
    ld a,(tick)
    cp b
    ret nz
    xor a
 r1.145:
    ld (@smc.volume + 1),a
 r1.146:
    jp bp.volume

;---------------------------------------------------------------
; Extended Effect D - Note Delay

@eeffect.notedelay:
 r1.147:
    ld a,(cmdlo+1)
    and 0x0f
    ld b,a
    ld a,(tick)
    cp b
    ret nz

    or a
    ret z
    jr doretrig

;---------------------------------------------------------------
; Extended Effect E - Pattern Delay

@eeffect.pattdelay:
    ld a,(tick)
    or a
    ret nz
 r1.148:
    ld a,(cmdlo+1)
    and 0x0f
    ld b,a
    ld a,(pat.delay.c+1)
    or a
    ret nz              ; still delaying pattern

    ld a,b              ; so do not reset tick
    inc b
    ld (pat.delay.f+1),a

    ret

;---------------------------------------------------------------
;tables need to fit within 32 byte boundary

    defs align 32

routine.len:    ;routine start ORGs at 0 -> routine.len = length

;===============================================================
length: equ routine.len + routines - 0x8000

@c1:    equ 0 * routine.len + routines
@c2:    equ 1 * routine.len + routines
@c3:    equ 2 * routine.len + routines
@c4:    equ 3 * routine.len + routines

    defs ( 4 - 1 ) * routine.len

;===============================================================
    org ( 4 * routine.len ) + routines - 0x8000
;---------------------------------------------------------------

move.spc:
move.size:  equ 6 * 256     ;move size = gap size

    ; for octave 4 -> 6 (5 oct)
    ; for octave 3 -> 3 (3 oct)

    ; maximum amount of bytes (*256) needed in one sample frame at:
    ;
    ; * Amiga pitch (3 octaves) = 108 -> burst speed = 808
    ;   808 / 256 * 208 = 656.5 -> 3 * 256 bytes

    ; * extended PC pitch (5 octaves) = 54 -> burst speed = 1616
    ;   1616 / 256 * 208 = 1313 -> 6 * 256 bytes

;-------------------------------------------------------------------------------

    org $ + 0x8000

conv.list:

   ; channel 1

    ; sample page
    defw @c1 + @bp.page.1 + 1
    defw @c1 + @bp.page.2 + 1
    defw @c1 + @bp.page.3 + 1
    defw @c1 + @bp.page.4 + 1
    defw @c1 + @bp.page.5 + 1
    defw @c1 + @bp.page.6 + 1
    defw @c1 + @bp.page.7 + 1
    defw @c1 + @bp.page.8 + 1
    defw @c1.bp.page + 1, 0
    ; sample offset
    defw @c1 + @bp.offset.1 + 1
    defw @c1 + @bp.offset.2 + 1
    defw @c1 + @bp.offset.3 + 1
    defw @c1 + @bp.offset.4 + 1
    defw @c1 + @bp.offset.5 + 1
    defw @c1 + @bp.offset.6 + 1
    defw @c1 + @bp.offset.7 + 1
    defw @c1 + @bp.offset.8 + 1
    defw @c1.bp.offset + 1, 0
    ; volume table
    defw @c1 + @bp.volume_table + 1, 0
    ; speed low
    defw @c1 + @bp.speed.low + 1, 0
    ; speed high
    defw @c1 + @bp.speed.high + 1, 0
    ; speed fraction
    defw @c1 + @bp.speed.fraction.1 + 1
    defw @c1 + @bp.speed.fraction.2 + 1, 0

   ; channel 2

    defw @c2 + @bp.page.1 + 1
    defw @c2 + @bp.page.2 + 1
    defw @c2 + @bp.page.3 + 1
    defw @c2 + @bp.page.4 + 1
    defw @c2 + @bp.page.5 + 1
    defw @c2 + @bp.page.6 + 1
    defw @c2 + @bp.page.7 + 1
    defw @c2 + @bp.page.8 + 1
    defw @c2.bp.page + 1, 0

    defw @c2 + @bp.offset.1 + 1
    defw @c2 + @bp.offset.2 + 1
    defw @c2 + @bp.offset.3 + 1
    defw @c2 + @bp.offset.4 + 1
    defw @c2 + @bp.offset.5 + 1
    defw @c2 + @bp.offset.6 + 1
    defw @c2 + @bp.offset.7 + 1
    defw @c2 + @bp.offset.8 + 1
    defw @c2.bp.offset + 1, 0

    defw @c2 + @bp.volume_table + 1, 0

    defw @c2 + @bp.speed.low + 1, 0

    defw @c2 + @bp.speed.high + 1, 0

    defw @c2 + @bp.speed.fraction.1 + 1
    defw @c2 + @bp.speed.fraction.2 + 1, 0

   ; channel 3

    defw @c3 + @bp.page.1 + 1
    defw @c3 + @bp.page.2 + 1
    defw @c3 + @bp.page.3 + 1
    defw @c3 + @bp.page.4 + 1
    defw @c3 + @bp.page.5 + 1
    defw @c3 + @bp.page.6 + 1
    defw @c3 + @bp.page.7 + 1
    defw @c3 + @bp.page.8 + 1
    defw @c3.bp.page + 1, 0

    defw @c3 + @bp.offset.1 + 1
    defw @c3 + @bp.offset.2 + 1
    defw @c3 + @bp.offset.3 + 1
    defw @c3 + @bp.offset.4 + 1
    defw @c3 + @bp.offset.5 + 1
    defw @c3 + @bp.offset.6 + 1
    defw @c3 + @bp.offset.7 + 1
    defw @c3 + @bp.offset.8 + 1
    defw @c3.bp.offset + 1, 0

    defw @c3 + @bp.volume_table + 1, 0

    defw @c3 + @bp.speed.low + 1, 0

    defw @c3 + @bp.speed.high + 1, 0

    defw @c3 + @bp.speed.fraction.1 + 1
    defw @c3 + @bp.speed.fraction.2 + 1, 0

   ; channel 4

    defw @c4 + @bp.page.1 + 1
    defw @c4 + @bp.page.2 + 1
    defw @c4 + @bp.page.3 + 1
    defw @c4 + @bp.page.4 + 1
    defw @c4 + @bp.page.5 + 1
    defw @c4 + @bp.page.6 + 1
    defw @c4 + @bp.page.7 + 1
    defw @c4 + @bp.page.8 + 1
    defw @c4.bp.page + 1, 0

    defw @c4 + @bp.offset.1 + 1
    defw @c4 + @bp.offset.2 + 1
    defw @c4 + @bp.offset.3 + 1
    defw @c4 + @bp.offset.4 + 1
    defw @c4 + @bp.offset.5 + 1
    defw @c4 + @bp.offset.6 + 1
    defw @c4 + @bp.offset.7 + 1
    defw @c4 + @bp.offset.8 + 1
    defw @c4.bp.offset + 1, 0

    defw @c4 + @bp.volume_table + 1, 0

    defw @c4 + @bp.speed.low + 1, 0

    defw @c4 + @bp.speed.high + 1, 0

    defw @c4 + @bp.speed.fraction.1 + 1
    defw @c4 + @bp.speed.fraction.2 + 1, 0

;-------------------------------------------------------------------------------
build.list:

    ; absolute addresses that need to be relocated

    defw r0.000,r0.001,r0.002,r0.003,r0.004
    defw r0.005,r0.006,r0.007,r0.008,r0.009
    defw r0.010,r0.011,r0.012,r0.013,r0.014
    defw r0.015,r0.016,r0.017,r0.018,r0.019
    defw r0.020,r0.021,r0.022,r0.023,r0.024
    defw r0.025,r0.026,r0.027,r0.028,r0.029
    defw r0.030,r0.031,r0.032,r0.033,r0.034
    defw r0.035,r0.036,r0.037,r0.038,r0.039
    defw r0.040,r0.041,r0.042,r0.043,r0.044
    defw r0.045,r0.046,r0.047

    ; absolute addresses that need to be relocated

    defw r1.001+1,r1.002+1,r1.003+1,r1.004+1,r1.005+1
    defw r1.006+1,r1.007+1,r1.008+1,r1.009+1,r1.010+1
    defw r1.011+1,r1.012+1,r1.013+1,r1.014+1,r1.015+1
    defw r1.016+1,r1.017+1,r1.018+1,r1.019+1,r1.020+1
    defw r1.021+1,r1.022+1,r1.023+1,r1.024+1,r1.025+1
    defw r1.026+1,r1.027+1,r1.028+1,r1.029+1,r1.030+1
    defw r1.031+1,r1.032+1,r1.033+1,r1.034+1,r1.035+1
    defw r1.036+1,r1.037+1,r1.038+1,r1.039+1,r1.040+1
    defw r1.041+1,r1.042+1,r1.043+1,r1.044+1,r1.045+1
    defw r1.046+1,r1.047+1,r1.048+1,r1.049+1,r1.050+1
    defw r1.051+1,r1.052+1,r1.053+1,r1.054+1,r1.055+1
    defw r1.056+1,r1.057+1,r1.058+1,r1.059+1,r1.060+1
    defw r1.061+1,r1.062+1,r1.063+1,r1.064+1,r1.065+1
    defw r1.066+1,r1.067+1,r1.068+1,r1.069+1,r1.070+1
    defw r1.071+1,r1.072+1,r1.073+1,r1.074+1,r1.075+1
    defw r1.076+1,r1.077+1,r1.078+1,r1.079+1,r1.080+1
    defw r1.081+1,r1.082+1,r1.083+1,r1.084+1,r1.085+1
    defw r1.086+1,r1.087+1,r1.088+1,r1.089+1,r1.090+1
    defw r1.091+1,r1.092+1,r1.093+1,r1.094+1,r1.095+1
    defw r1.096+1,r1.097+1,r1.098+1,r1.099+1,r1.100+1
    defw r1.101+1,r1.102+1,r1.103+1,r1.104+1,r1.105+1
    defw r1.106+1,r1.107+1,r1.108+1,r1.109+1,r1.110+1
    defw r1.111+1,r1.112+1,r1.113+1,r1.114+1,r1.115+1
    defw r1.116+1,r1.117+1,r1.118+1,r1.119+1,r1.120+1
    defw r1.121+1,r1.122+1,r1.123+1,r1.124+1,r1.125+1
    defw r1.126+1,r1.127+1,r1.128+1,r1.129+1,r1.130+1
    defw r1.131+1,r1.132+1,r1.133+1,r1.134+1,r1.135+1
    defw r1.136+1,r1.137+1,r1.138+1,r1.139+1,r1.140+1
    defw r1.141+1,r1.142+1,r1.143+1,r1.144+1,r1.145+1
    defw r1.146+1,r1.147+1,r1.148+1

    defw r2.001+2,r2.002+2,r2.003+2,r2.004+2,r2.005+2
    defw r2.006+2,r2.007+2,r2.008+2,r2.009+2,r2.010+2

    defw -1

;-------------------------------------------------------------------------------

    defs move.size - ( $ - conv.list )

    assert $ < 0xc000