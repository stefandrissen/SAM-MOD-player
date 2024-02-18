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
    ld (rs.bp.page),a

    ;---------------------------------------------------------------------------
    ;set up the finetune tables

    ld hl,table.finetune
    ld de,table.finetune + 1
    ld bc,0x400 - 1
    ld (hl),0xff
    ldir

    ld de,finelist
    ld bc,4 * 12 * 256

    @loop:

        ld h,table.finetune / 0x100
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

        djnz @-loop

    ;---------------------------------------------------------------------------

    ld hl,c1
    ld de,c2
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

        ld bc,c1
        add hl,bc
        ld a,4

        @loop.relocate.4chan:

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

            jr nz,@-loop.relocate.4chan

        jr @-loop.relocate

 @leave.relocate:

    ld hl,c1 + mk.cur.pat + 1
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

    ld hl,c1 + channel.on + 1
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

    ld de,c1 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld de,c2 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld de,c3 + saa.exvol
    ldi
    ldi
    ldi
    ldi

    ld de,c4 + saa.exvol
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
