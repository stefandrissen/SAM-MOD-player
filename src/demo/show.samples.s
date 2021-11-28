show.samples:

 ; F2

    call get.samples    ; returns c = # samples, de = sample table

    @loop:

        ld b,mod.sample.title.len
        call print.de.b

        inc l

        ld a,(de)
        ld b,a
        inc de
        ld a,(de)
        or b
        jr nz,@pr.ins.exist

        ld a,e
        add a,3
        ld e,a
        jr nc,$+3
        inc d
        ld a,l
        add 7
        ld l,a
        jr nc,$+3
        inc h
        jr @pr.next.ins

     @pr.ins.exist:

        ld a,b
        call print.num
        ld a,(de)
        inc de
        call print.num

        inc l
        inc de
        ld a,(de)
        inc de
        call print.num

     @pr.next.ins:

        ld a,e
        add 4
        ld e,a
        jr nc,$+3
        inc d

        ld a,l
        add video.memory.32.rows - 30
        ld l,a
        jr nc,$+3
        inc h

        dec c
        jp nz,@-loop

set.sample.interrupt:

    ld hl,sample.cursors.interrupt
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1

    ret

;-------------------------------------------------------------------------------

@text.samples.colour:
    defb 1,3,31,1

;-------------------------------------------------------------------------------
get.samples:

 ; output
 ; - c  = samples [15,31]
 ; - de = sample table

    xor a
    ld (c1.inst+1),a
    ld (c2.inst+1),a
    ld (c3.inst+1),a
    ld (c4.inst+1),a
    cpl
    ld (int.rtn.pag),a
    call cls

    ld ix,@text.samples.colour
    ld a,6
    call colour.scrn

    ld hl,video.memory.32.rows * 1 + 0 + video.memory.high.attributes
    ld b,31 * 6
    ld de,31
    xor a
    @loop:
        ld (hl),a
        add hl,de
        ld (hl),a
        inc hl
        djnz @-loop

    ld hl,video.memory.32.rows * 1 + 0 + video.memory.high
    ld c,31

    @loop.rows:

        ld ix,@show.samples.cursor
        ld b,6

        @loop:
            ld a,(ix)
            inc ix
            ld (hl),a
            add hl,de
            ld a,(ix)
            inc ix
            ld (hl),a
            inc hl
            djnz @-loop

        dec c
        jr nz,@-loop.rows

    call print.title

    ld c,31
    ld hl,mod.header + mod.pt.id
    ld a,(hl)
    cp "M"             ; M.K. / M!K!
    jr nz,@not.m

    inc hl
    ld a,(hl)
    cp "."
    jr z,@ins.31

    cp "!"
    jr z,@ins.31

    jr @ins.15

 @not.m:
    cp "F"              ; FLT4
    jr nz,@ins.15

    inc hl
    ld a,(hl)
    cp "L"
    jr z,@ins.31

 @ins.15:
    ld c,15

 @ins.31:
    ld hl,video.memory.32.rows * 1 + 1 + video.memory.high
    ld de,mod.header + mod.title.len

    ret

;-------------------------------------------------------------------------------
@show.samples.cursor:

    defb %10000000,%00000001
    defb %11000000,%00000011
    defb %11100000,%00000111
    defb %11000000,%00000011
    defb %10000000,%00000001

;-------------------------------------------------------------------------------
sample.cursors.interrupt:

 ; display cursors on sample name screen

    ld a,(counter)
    or a
    ret nz

    ld b,32

    ld a,(c1.inst+1)
    or a
    ld c,0
    call nz,clear.cursor

    ld a,(c4.inst+1)
    or a
    ld c,0
    call nz,clear.cursor

    ld a,(c2.inst+1)
    or a
    ld c,31
    call nz,clear.cursor

    ld a,(c3.inst+1)
    or a
    ld c,31
    call nz,clear.cursor

    ld hl,mod.current.row
    ld a,(hl)
    and 0x10
    ld c,a
    inc l
    inc l
    ld a,(hl)
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    or c
    jr z,$+5
    ld (c1.inst+1),a
    inc l
    inc l

    ld a,(hl)
    and 0x10
    ld c,a
    inc l
    inc l
    ld a,(hl)
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    or c
    jr z,$+5
    ld (c2.inst+1),a
    inc l
    inc l

    ld a,(hl)
    and 0x10
    ld c,a
    inc l
    inc l
    ld a,(hl)
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    or c
    jr z,$+5
    ld (c3.inst+1),a
    inc l
    inc l

    ld a,(hl)
    and 0x10
    ld c,a
    inc l
    inc l
    ld a,(hl)
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    or c
    jr z,$+5
    ld (c4.inst+1),a
    inc l
    inc l

    ld a,(c1.on)
    or a
    jr z,@skip.c1

 c1.inst:
    ld a,0
    or a
    ld c,0
    call nz,colour.cursor.1

 @skip.c1:

    ld a,(c4.on)
    or a
    jr z,@skip.c4

 c4.inst:
    ld a,0
    or a
    ld c,0
    call nz,colour.cursor.2

 @skip.c4:

    ld a,(c2.on)
    or a
    jr z,@skip.c2

 c2.inst:
    ld a,0
    or a
    ld c,31
    call nz,colour.cursor.1

 @skip.c2:

    ld a,(c3.on)
    or a
    jr z,@skip.c3

 c3.inst:
    ld a,0
    or a
    ld c,31
    call nz,colour.cursor.2

 @skip.c3:

    ret

;-------------------------------------------------------------------------------
clear.cursor:

    ld hl,line.table
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    add c
    ld l,a
    ld c,%00000000
    ld (hl),c
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),c
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),c
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),c
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),c

    ret

;-------------------------------------------------------------------------------
colour.cursor.1:

    ld hl,line.table
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    add c
    ld l,a

    ld (hl),4
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),5
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),6
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),5
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),4

    ret

;-------------------------------------------------------------------------------
colour.cursor.2:

    ld hl,line.table
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    add c
    ld l,a

    ld (hl),64+5
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),64+6
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),6
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),64+6
    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h
    ld (hl),64+5

    ret
