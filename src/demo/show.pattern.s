show.pattern:

 ; F4 show row of pattern being played

    ld a,-1
    ld (int.rtn.pag),a
    xor a
    ld (@print.pos+1),a
    call cls

    ld ix,@text.pattern.colours
    ld a,8
    call colour.scrn

    ld hl,screen.attributes + screen.24.rows * 3 + 1
    ld c,24 - 3 - 3
    ld de,29

    @loop.rows:

        ld ix,2 * 8 + colours
        ld b,8

        @loop:
            ld a,(ix)
            inc ix
            ld (hl),a
            add hl,de
            ld (hl),a
            inc hl
            inc hl
            inc hl
            djnz @-loop

        dec c
        ld a,c
        cp 9
        jr nz,$+3
        inc h
        or a
        jr nz,@-loop.rows

    call print.title

    ld de,@text.volume
    ld hl,screen + screen.24.rows * 1 + 0
    ld b,32
    call print.de.b

    call print.amplification.factor

    ld de,@text.keys
    ld hl,screen + screen.24.rows * 22 + 0
    ld b,32
    call print.de.b

    ld de,@text.author
    ld hl,screen + screen.24.rows * 23 + 0
    ld b,32
    call print.de.b

    ld de,@text.channel
    ld hl,screen + screen.24.rows * 2 + 1
    ld b,8
    call print.de.b
    ld a,"1"
    call print.chr

    ld de,@text.channel
    ld hl,screen + screen.24.rows * 2 + 22
    ld b,8
    call print.de.b
    ld a,"2"
    call print.chr

    ld de,@text.channel
    ld hl,screen + screen.24.rows * 12 + 1
    ld b,8
    call print.de.b
    ld a,"4"
    call print.chr

    ld de,@text.channel
    ld hl,screen + screen.24.rows * 12 + 22
    ld b,8
    call print.de.b
    ld a,"3"
    call print.chr

    call print.loop.status

    ld a,2
    ld (@pr.set+1),a

    ld hl,pattern.interrupt
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1

    ret

;-------------------------------------------------------------------------------
@text.channel:  defm "Channel "

@text.volume:   defm "Vol: 000%  Speed: 00  Tempo: 000"
@text.keys:     defm "F1-F6 1234 C <> P ESC -+ Loop:  "
@text.author:   include "../constants/text.copyright.i"
                defm "    "
                include "../constants/text.version.i"

@text.pattern.colours:
    defb 1,3,1,1,1,4,9,1,1,4,9,1,1,5,1,2

;-------------------------------------------------------------------------------
print.amplification.factor:

    ld hl,screen + screen.24.rows * 1 + 5
    ld bc,(amplification.factor+1)

print.percent:
    ; convert 0x##.## to %

    ; input:
    ; - bc = fixed 8.8 (b = integer, c = fraction)

    ld a," "
    ld (@leading.zero+1),a

    ld a,b
    call @print.decimal ; integer (100%)
    ex de,hl

    ld hl,0
    ld b,h
    add hl,bc

    add hl,hl
    add hl,hl
    add hl,bc
    add hl,hl           ; * 10

    ld a,h

    ex de,hl
    call @print.decimal ; fraction (10%)
    ex de,hl

    ld c,l
    ld b,0
    ld h,b

    add hl,hl
    add hl,hl
    add hl,bc
    add hl,hl           ; * 10

    ld a,h              ; h = next digit, but unrounded
    bit 7,l
    jr z,$+3
    inc a               ; round up

    ex de,hl
    jr @print           ; fraction (1%)

 @print.decimal:

    or a
    jr z,@leading.zero

 @print:

    add a,"0"
    call print.chr
    ld a,"0"
    ld (@leading.zero+1),a
    ret

 @leading.zero:

    ld a," "
    jp print.chr

;-------------------------------------------------------------------------------
; PATTERN TRACKER for MOD player

 ; runs off tracker frame interrupt
 ; only run when frame counter (tick) <> 0

pattern.interrupt:

    ld hl,(tick.fraction)
    ld de,(tempo)
    add hl,de
    ld a,(speed)
    dec a
    cp h

    ret c

 @pr.set:

    ld a,0
    or a
    jp z,@print.channel.4_3

    ld a,(tick)
    or a
    ret nz

    ld (@pr.set+1),a

 @print.channel.1_2:

    ld hl,screen + screen.24.rows * 3 + 3

  @print.pos:
    ld a,0
    add h
    ld h,a

    ld b,32                 ; used
    ld ix,mod.current.row
    call @print.channel

    ld ix,mod.current.row + 4
    ld l,18
    call @print.channel

    ld hl,c1.on
    ld d,(hl)
    inc l
    ld e,(hl)
    ld a,(@print.pos+1)
    add 128
    ld l,1   ;width offset
    ld c,3   ;height offset
    add c
    ld h,a

  @print.cursor:

    ld a,d
    or a
    jr z,@skip.left

    ld (hl),%10000000
    ld a,l
    add b
    ld l,a
    ld (hl),%11000000
    ld a,l
    add b
    ld l,a
    ld (hl),%11100000
    ld a,l
    add b
    ld l,a
    ld (hl),%11000000
    ld a,l
    add b
    ld l,a
    ld (hl),%10000000

  @skip.left:

    ld a,e
    or a
    jr z,@skip.right

    ld l,30
    ld (hl),%00000001
    ld a,l
    add b
    ld l,a
    ld (hl),%00000011
    ld a,l
    add b
    ld l,a
    ld (hl),%00000111
    ld a,l
    add b
    ld l,a
    ld (hl),%00000011
    ld a,l
    add b
    ld l,a
    ld (hl),%00000001

  @skip.right:

    ld a,h
    sub c
    dec a
    and 7
    add c
    add 128
    ld h,a
    ld l,30
    ld c,0
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c

    ld l,1
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c
    ld a,l
    add b
    ld l,a
    ld (hl),c

    ret

 @print.channel.4_3:

    inc a
    ld (@pr.set+1),a
    ld a,(@print.pos+1)
    add 13+128
    ld h,a
    ld l,18

    ld b,32

    ld ix,mod.current.row+8
    call @print.channel

    ld l,3
    ld ix,mod.current.row+12
    call @print.channel

    ld hl,c4.on
    ld d,(hl)
    dec l
    ld e,(hl)
    ld a,(@print.pos+1)
    ld c,13
    add c
    add 128
    ld h,a
    ld l,1
    call @print.cursor
    ld hl,@print.pos+1
    ld a,(hl)
    inc a
    and %10000111
    ld (hl),a

    ret

;-------------------------------------------------------------------------------
@print.channel:

 ; prints information about channel

 ; input:
 ; - ix = pattern.row channel
 ; - hl = screen address

    ld a,(ix+0)             ; instrument hi
    ld d,a
    and 0xf0
    ld a,(ix+2)
    ld e,a
    jr nz,@not.blank
    and 0xf0
    jr z,@blank

 @not.blank:

    ld a,d
    call print.hi.nibble
    ld a,e
    call print.hi.nibble

    jr @continue.1

 @blank:

    call print.space
    call print.space.c

 @continue.1:

    inc l

 tracker.display:
    jr display.period_values    ; replaced with nops when 3 octaves selected

 display.notes:

    ; use note table - only possible with 3 octave due to low byte collision

    ld a,(ix+1)
    or a
    jr z,@blank.2

    add a,a
    ld e,a
    ld d,period.note.table / 256
    jr nc,@no.overflow.2
    inc d
 @no.overflow.2:
    ld a,(de)               ; note
    bit 7,a
    jr nz,@sharp

    or a
    jr z,@invalid.note

    call print.fast.char
    ld a,17                 ; -
    call print.fast.char

    jr @not.sharp

 @sharp:
    res 7,a
    call print.fast.char
    ld a,18                 ; #
    call print.fast.char

 @not.sharp:

    inc e
    ld a,(de)               ; octave
    call print.fast.char

    jr @continue.2

 @invalid.note:

    ld d,(ix+0)

 display.period_values:

    ld a,d                  ; period value
    and 0x0f
    ld e,(ix+1)
    jr nz,@not.blank
    ld a,e
    or a
    jr z,@blank.2

 @not.blank:

    ld a,d
    call print.lo.nibble
    ld a,e
    call print.hex

    jr @continue.2

 @blank.2:

    call print.space
    call print.space.c
    call print.space.c

 @continue.2:

    inc l

    ld a,(ix+2)             ; command
    and 0x0f

    jr nz,@+not.blank
    ld c,a
    ld a,(ix+3)
    or a
    jr z,@+blank
    ld a,c

@not.blank:

    call print.lo.nibble
    inc l
    ld a,(ix+3)             ; command parameter
    jp print.hex

 @blank:

    call print.space
    inc l
    call print.space.c
    jp print.space.c

;-------------------------------------------------------------------------------
print.loop.status:

    ld a,(disable.pos)
    or a
    ld a,"Y"
    jr z,$+4
    ld a,"N"
    ld hl,screen + screen.24.rows * 22 + 31
    jp print.chr
