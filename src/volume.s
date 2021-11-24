; calculate volume tables

; initially calculated by burstplayer
; recalculated with amplification by demo

;---------------------------------------------------------------
; create volume tables for burstplayer (Amiga samples)
; B = number of bits

populate.volume.table:

    ld a,b
    cp 3
    jr nz,@not.saa

    ld c,16             ; volume tables

    ld a,1
    ld (@skip.table+2),a

    call @calculate.volumes
    call @populate.flipped.volumes

    ret

 @not.saa:

    ld c,32             ; volume tables

    xor a
    ld (@skip.table+2),a

;-------------------------------------------------------------------------------
@calculate.volumes:

    ld ix,volume.table

    ld a,c
    ld (@max.tables+1),a
    dec a
    ld (@div.by+1),a

    dec b
    ld a,1
 @getbits:
    rla
    djnz @-getbits

    ld (@vol.base.1+2),a
    ld (@vol.base.2+2),a

    rla
    ld (@vol.bits+1),a
    ld a,0
    adc a,0
    ld (@vol.bits+2),a

    xor a
    ld (@volume+1),a

    @loop.volume.tables:

     @vol.bits:
        ld de,0             ; DE=2^bits
        ld h,d
        ld l,e
        dec hl              ; HL=2^bits-1
        ld a,l
        ld (max.vol+1),a

        ld hl,0

     @volume:
        ld a,0              ; [0-31] / [0-15]
        or a
        jr z,@no.mul

        ld b,a
        @mul.vol:
            add hl,de
            djnz @-mul.vol
                            ; HL=vol*2^bits

        ; here we need to ensure that HL gets multiplied by the factor!

        ex de,hl
     amplification.factor:
        ld bc,0x0100
        xor a
        ld h,a
        ld l,a
        ld (@rest+1),a

        @mul.amp:

         @rest:
            ld a,0
            add c
            ld (@rest+1),a
            ld a,b
            adc a,l
            ld l,a
            jr nc,$+3
            inc h

            dec de
            ld a,d
            or e
            jr nz,@-mul.amp

     @no.mul:

        ld b,h
        ld c,l
     @div.by:
        ld de,15            ; tables-1
        call @bc.div.de
        ld (@range+1),bc

     @vol.base.1:
        ld hl,0x0800        ; H=2^(bits-1) "central" vol.
        ld b,128

        ; 2^bits * v/15

     @range:
        ld de,15            ; range (step)

        @loop:
            ld (ix),h
            inc ix

            add hl,de

            ld a,h
         max.vol:
            sub 0               ; maximum volume (2^bits-1)
            jr z,$+4
            jr c,@not.max
            ld a,(max.vol+1)
            ld h,a              ; h=maximum volume
            ld de,0             ; reset adder
         @not.max:

            djnz @-loop

        ld c,127
        add ix,bc

        ld de,(@range+1)

     @vol.base.2:
        ld hl,0x0800        ; "central" volume
        ld b,128

        @loop:

            or a
            sbc hl,de

            jr nc,@not.min
            ld h,0              ; minimum volume
            ld d,h              ; reset adder
            ld e,h
         @not.min:
            ld (ix),h
            dec ix
            djnz @-loop

     @skip.table:
        ld bc,129           ; B=1 if SAA
        add ix,bc

        ld a,(@volume+1)
        inc a
        ld (@volume+1),a

     @max.tables:
        cp 0

        jp nz,@-loop.volume.tables

    ret

;-------------------------------------------------------------------------------
@populate.flipped.volumes:
    ; copy tables with stereo flip for SAA1099

    ld hl,volume.table
    ld de,volume.table + 0x100
    ld bc,16

    @loop:

        ld a,(hl)
        add a,a             ; correct 3 bits to effective bits (1-4)
        ld (hl),a
        inc hl

        add a,a
        add a,a
        add a,a
        add a,a
        ld (de),a
        inc de

        djnz @-loop

        inc d
        inc h
        dec c

        jr nz,@-loop

    ret

;------------------------------------------------------------------------------
@bc.div.de:

    ld a,b          ; divide BC by DE
    ld b,16         ; result in BC
    ld hl,0         ; DE is unchanged

    @loop:

        rl c
        rla
        adc hl,hl
        sbc hl,de
        jr nc,@nc
        add hl,de
     @nc:
        ccf

        djnz @-loop

    rl c
    rla
    ld b,a

    ret

