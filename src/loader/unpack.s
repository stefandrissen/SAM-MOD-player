;SAM MOD player - unpack

 ;(C) 1995-2021 Stefan Drissen

 ; When the internal sound chip was only output device, 4 bits per sample was
 ; enough. Packed mods pack two sample nibbles per byte, saving space on disk.

 ; 00 11 22 33 44 55 is packed as 03 14 25

;-------------------------------------------------------------------------------
loader.unpack:

    ld c,a

    call relocate.low

 @unpack.org:

    defw @unpack.len

    org inst.buffer

 @unpack:

    in a,(port.hmpr)
    ld (@store.hmpr+1),a

    call @pages.init

    ld a,c
    res 6,a

    ld hl,20 + 31 * 30 + 2 + 0x8000 ; !!!
    ld de,4
    or a
    ld a,31
    jr nz,@not.noisetracker

    ld hl,20 + 15 * 30 + 2 + 0x8000
    ld e,d
    ld a,15

  @not.noisetracker:

    ld (@samples+1),a

    ; get highest pattern

    ld b,0x80       ; patterns
    xor a
    @loop.patterns:
        cp (hl)
        jr nc,@lower
        ld a,(hl)
     @lower:
        inc hl
        djnz @-loop.patterns

    inc a

    add hl,de       ; hl -> first sample byte

    ld b,a
  @page.mod:
    ld e,page.mod

    @loop:
        ld a,h
        add 4
        ld h,a
        bit 6,h
        res 6,h
        jr z,$+3
        inc e

        djnz @-loop

    ;sample starts directly after last pattern

    ld (@sample.offset+1),hl
    ld a,e
    ld (@sample.offset.page+1),a

    ld ix,0x8000 + mod.samples  ; start sample table

  @samples:
    ld b,0

    ld hl,0                 ; length in bytes
    xor a                   ; length in 16K pages
    @loop:
        ld d,(ix + mod.sample.len.words + 0)
        ld e,(ix + mod.sample.len.words + 1)
        add hl,de           ; only once, two sample nibbles per byte
        jr nc,$+4
        add 4               ; > 64K -> +4 pages

        ld de,mod.sample.len
        add ix,de

        djnz @-loop

    ; ahl = total sample length

    bit 7,h
    res 7,h
    jr z,$+4
    add 2

    bit 6,h
    res 6,h
    jr z,$+3
    inc a

    ; ahl = total sample length, hl < 16K

    ld (@total.sample.len+1),hl
    ld (@total.sample.len.pages+1),a

  @sample.offset.page:
    add a,0
  @sample.offset:
    ld de,0
    add hl,de
    jr nc,$+4
    add a,2

    set 7,h
    bit 6,h
    res 6,h
    jr z,$+3
    inc a

    ld d,a                          ; dhl = eof

    exx

    ld hl,(@sample.offset+1)        ; first byte of first sample
    ld a,(@sample.offset.page+1)
    ld d,a

  @total.sample.len:
    ld bc,0                         ; < 0x4000
    inc b                           ; full 0x100 blocks only
  @total.sample.len.pages:
    ld e,0                          ; ebc = length of samples (in bytes not words)

    @loop:

        push bc

        call @page.d

        push de

        ld b,0
        ld de,@buffer

        @loop.buffer:
            ld a,(hl)                   ; get sample byte
            ld (de),a
            inc e
            and 0xf0
            ld (hl),a
            inc hl

            djnz @-loop.buffer

        pop de

        exx

        call @page.d

        push de

        ld b,0
        ld de,@buffer

        @loop.buffer:
            ld a,(de)
            inc e
            and 0x0f
            rlca
            rlca
            rlca
            rlca
            ld (hl),a
            inc hl

            djnz @-loop.buffer

        pop de

        exx

        pop bc

        djnz @-loop

     @next.page:

        bit 6,h
        res 6,h
        jr z,$+3
        inc d

        exx

        bit 6,h
        res 6,h
        jr z,$+3
        inc d

        exx

        ld b,0x40
        ld a,e
        dec e
        or a

        jr nz,@-loop

  @store.hmpr:
    ld a,0
    out (port.hmpr),a

    ret

 ;---------------------------------------------------------------
 @pages.init:

    ld a,(loader.ram)       ; loader is still in section C
    ld (@page.d+1),a
    and %11100
    jr z,@+no.megabyte

    ld a,high.memory.external
    out (port.hmpr),a
    ld a,page.mod.megabyte
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a
    dec a

    jr @+continue

  @no.megabyte:

    ld a,page.mod
    out (port.hmpr),a

  @continue:
    ld (@page.mod+1),a

    ret

 ;---------------------------------------------------------------
 @page.d:

    ld a,0                  ; loader is no longer in section C
    and %11100

    ld a,d

    jr z,@+no.megabyte

    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a

    ret

  @no.megabyte:

    out (port.hmpr),a

    ret

 @unpack.len:  equ $ - @unpack

    org @unpack.org + @unpack.len + 2

 ;---------------------------------------------------------------

    ret

@buffer: equ dos.sector

assert @buffer \ 0x100 == 0