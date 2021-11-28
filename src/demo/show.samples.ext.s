show.samples.ext:

 ; F3 show samples with extended info (finetune and loop)

    call get.samples    ; returns c = # samples, de = sample table

    @loop:

        ld b,9

        call print.de.b

        inc l

        ld a,e
        add mod.sample.title.len - 9
        ld e,a
        jr nc,$+3
        inc d

        ld a,(de)
        ld b,a
        inc de
        ld a,(de)
        or b
        jr nz,@pr.ins.exis2

        ld a,e
        add a,7
        ld e,a
        jr nc,$+3
        inc d
        ld a,l
        add 30-10
        ld l,a
        jr nc,$+3
        inc h
        jr @pr.next.in2

     @pr.ins.exis2:

        ld a,b
        call print.num
        ld a,(de)
        inc de
        call print.num
        inc l

        ld a,(de)
        and 0x0f
        bit 3,a
        jr z,@tune.plus

        ld a,"-"
        call print.chr
        ld a,(de)
        and 0x0f
        ld b,a
        ld a,16
        sub b
        jr @got.tune

     @tune.plus:
        ld a,"+"
        call print.chr
        ld a,(de)
        and 0x0f

     @got.tune:
        add  "0"
        call print.chr
        inc l
        inc de

        ld a,(de)
        inc de
        call print.num

        inc de
        inc de
        ld a,(de)
        inc de
        or a
        jr nz,@has.loop

        ld a,(de)
        cp 2
        jr nc,@has.loop

        inc de
        ld a,l
        add video.memory.32.rows - 30 + 10

        jr @fin.loop

     @has.loop:

        inc l

        dec de
        dec de
        dec de

        ld a,(de)           ;loop offset
        inc de
        call print.num
        ld a,(de)
        inc de
        call print.num
        inc l

        ld a,(de)           ;loop length
        inc de
        call print.num
        ld a,(de)
        inc de
        call print.num

     @pr.next.in2:

        ld a,l
        add video.memory.32.rows - 30

     @fin.loop:

        ld l,a
        jr nc,$+3
        inc h

        dec c

        jp nz,@-loop

    jp set.sample.interrupt

;-------------------------------------------------------------------------------
