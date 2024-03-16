;SAM MOD player - demo

;(C) 1996-2024 Stefan Drissen

include "../memory.i"
include "../ports/internal.i"
include "../ports/megabyte.i"
include "../ports/keyboard.i"
include "../constants/mod.i"
include "../constants/opcodes.i"

;-------------------------------------------------------------------------------
; demo is the program that runs in "foreground" mode
;   the tracker is called by the burst routine every frame

    org demo.setup  ; 0x6000 - aBcd

    @demo.help:         equ 1
    @demo.samples:      equ 2
    @demo.samples.ext:  equ 3
    @demo.tracker:      equ 4
    @demo.effects:      equ 5
    @demo.burst:        equ 6

;-------------------------------------------------------------------------------
demo.setup:

    jp @setupmod

;-------------------------------------------------------------------------------

demo.ram:           defb 0  ; %XXXRR (RAM / 256K)
demo.samples:       defb 0  ; [15|31]
    demo.samples.high:  equ demo.samples + 0x8000

;-------------------------------------------------------------------------------
@fix.page:
    and high.memory.page.mask

    ret

;-------------------------------------------------------------------------------
@setupmod:

 ; input:
 ; - c = RAM

    in a,(port.hmpr)
    ld (@store.hmpr+1),a

    bit 1,c
    jr nz,@is.512K
    ld a,high.memory.page.mask.256k
    ld (@fix.page+1),a

 @is.512K:

    ld a,c
    ld (demo.ram),a

    and %11100
    ld a,page.mod
    jr z,@no.megabyte

    ld a,page.mod.megabyte
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a
    ld a,high.memory.external

 @no.megabyte:

    out (port.hmpr),a

    ld hl,0x8000
    ld de,mod.header - 0x8000
    ld bc,mod.pattern
    ldir

    ld a,page.tracker
    call @fix.page
    out (port.hmpr),a
    ld hl,demo
    ld (tracker.ptr.addr.demo),hl
    ld a,page.demo - 1  ; - 1 since demo is in D
    call @fix.page
    ld (tracker.ptr.page.demo),a

    ld a,(demo.ram)
    ld (tracker.ram),a
    and %11100
    ld a,page.mod
    jr z,@no.megabyte

    ld a,page.mod.megabyte

 @no.megabyte:

    ld (tracker.ptr.page.mod),a

 tracker.setup:
    ld a,0
    or a
    call z,tracker.init

    ld a,1
    ld (tracker.setup+1),a

    call tracker.install.mod

    ld (demo.samples),a

    ld a,(tracker.octaves)
    ld hl,tracker.display - 0x8000
    cp 3
    jr nz,@display.periods

    ld a,opcode.nop
    ld (hl),a
    jr @continue

 @display.periods:

    ld (hl),opcode.jr_n
    ld a,display.period_values - display.notes

 @continue:

    inc hl
    ld (hl),a

    ld a,page.burstplayer
    call @fix.page
    out (port.hmpr),a

    call burstplayer.start

 @store.hmpr:
    ld a,0
    out (port.hmpr),a

    ret

;-------------------------------------------------------------------------------

    org  $ + 0x8000  ; abcD

demo.palette:
    ;     GRB!grb         pen
    defb %0000000   ;     0

    defb %0011101   ; 3 1 1 BLUE + green
    defb %1011001   ; 3 2 2
    defb %1011101   ; 3 3 3

    defb %0101110   ; 3 1 4 RED + green
    defb %1101010   ; 3 2 5
    defb %1101110   ; 3 3 6

    defb %1001101   ; 3 1 7 GREEN + blue

    defb %0000000   ;     8 bright background

    defb %1011100   ; 3 2 9
  ; defb %1011101   ; 3 3   same as pen 3

    defb %0101011   ; 3 1 A RED + blue
    defb %0111010   ; 3 2 B
    defb %0111011   ; 3 3 C

    defb %1001110   ; 3 1 D GREEN + red
    defb %1101100   ; 3 2 E
  ; defb %1101110   ; 3 3   same as pen 6

    defb %1110111   ;     F

demo.palette.two:
    defw 0,0,0,0,0,0,0,0

;-------------------------------------------------------------------------------
demo:

 ;this is the routine that is running in "foreground mode"

    ld bc,0x0f00 + port.clut
    xor a
 @black:
    out (c),a
    djnz @-black
    out (c),a

    ld a,page.screen
    call @fix.page + 0x8000
    or video.mode.2
    out (port.vmpr),a

    call cls
    call set.palette

    call create.fast.print
    call create.line.table
    call create.note.table

 @first.time:
    ld a,0
    or a
    jr nz,@skip.intro
    cpl
    ld (int.rtn.pag),a
    ld (@first.time+1),a

    ld a,@demo.tracker
    ld (@trackon+1),a

 @skip.intro:

    ld a,(@trackon+1)
    dec a
    call z,show.help
    dec a
    call z,show.samples
    dec a
    call z,show.samples.ext
    dec a
    call z,show.pattern
    dec a
    call z,show.summary
    dec a
    call z,show.burst

    ld hl,(enable.burst)
    ld (@mk.enable+1),hl
 @mk.enable:
    call 0

;-------------------------------------------------------------------------------

@demo.loop:

 @trackon:
    ld a,0
    cp @demo.samples
    jr c,@skip.patpos
    cp @demo.effects
    jr nc,@skip.patpos

    ; print pattern position

    ld b,32                     ;for print routine
    ld hl,screen + screen.24.rows * 0 + 24
    ld a,(song.position)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    inc l

    ld a,(pattern.num)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    inc l

    ld a,(pattern.row)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble

 @skip.patpos:

    ld a,(@trackon+1)
    cp @demo.tracker
    jr nz,@skip.speed

    ; print speed and tempo

    ld hl,screen + screen.24.rows * 1 + 18
    ld a,(speed)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble

    ld hl,(tempo)
    ld b,h
    ld c,l              ; bc = tempo
    srl h
    rr l                ; hl = tempo / 2
    srl h
    rr l                ; hl = tempo / 4
    adc hl,bc           ; hl = tempo + tempo / 4 (-> * 1.25)
    ld b,h
    ld c,l
    ld hl,screen + screen.24.rows * 1 + 29
    call print.percent

 @skip.speed:

 ; scan cursor left / right

    ld bc,keyboard.cursors_cntrl * 256 + port.keyboard
    in c,(c)
    bit 3,c
    jr nz,@not.left

    ld hl,pattern.row
    dec (hl)
    dec (hl)
    bit 7,(hl)
    jr z,@wait.left

    ld (hl),62
    dec l
    dec l
    dec (hl)
    bit 7,(hl)
    jr z,@wait.left

    ld (hl),0
    inc l
    inc l
    ld (hl),0

  @wait.left:

    ei              ;just in case we're pausing
    ld a,(tick)
    or a
    jr nz,@wait.left

  @not.left:

    bit 4,c
    jr nz,@not.right
    ld a,(speed)
    ld hl,tick
    ld (hl),a

  @wait.right:

    ei
    ld a,(mstatus)    ;just in case end of tune
    dec a
    jp z,@exit

    ld a,(hl)
    dec a
    jr nz,@wait.right

  @not.right:

 ; scan 1 2 3 4

    ld bc,keyboard.12345 * 256 + port.keyboard
    in c,(c)
    xor a
    bit 0,c
    jr nz,@not.key.1

  @still.key.1:

    ld a,0
    or a
    jr nz,@key.2

    ld hl,c1.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a

  @not.key.1:

    ld (@still.key.1+1),a

  @key.2:

    xor a
    bit 1,c
    jr nz,@not.key.2

  @still.key.2:

    ld a,0
    or a
    jr nz,@key.3

    ld hl,c2.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a

  @not.key.2:

    ld (@still.key.2+1),a

  @key.3:

    xor a
    bit 2,c
    jr nz,@not.key.3

  @still.key.3:

    ld a,0
    or a
    jr nz,@key.4
    ld hl,c3.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a

  @not.key.3:

    ld (@still.key.3+1),a

  @key.4:

    xor a
    bit 3,c
    jr nz,@not.key.4

  @still.key.4:

    ld a,0
    or a
    jr nz,@key.p
    ld hl,c4.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a

  @not.key.4:

    ld (@still.key.4+1),a

 ; scan p

  @key.p:

    xor a
    ld bc,keyboard.yuiop * 256 + port.keyboard
    in c,(c)
    bit 0,c
    jr nz,@not.key.p

  @still.key.p:

    ld a,0
    or a
    jr nz,@still.p

  @ints.on:
    ld a,0
    or a
    jr z,@pause
    xor a
    ei
    jr @cont.p

  @pause:

    ld a,1
    di

  @cont.p:

    ld (@ints.on+1),a
    ld a,1

  @not.key.p:

    ld (@still.key.p+1),a
  @still.p:

    ld hl,@trackon+1
    ld a,(hl)

 ; scan f1 f2 f3

    ld bc,keyboard.f3_f2_f1 * 256 + port.status
    in c,(c)

    bit 5,c
    jr nz,@not.f1

    cp @demo.help
    jr z,@not.f1

    ld (hl),@demo.help
    call show.help
    jr @skip.f

  @not.f1:

    bit 6,c
    jr nz,@not.f2

    cp @demo.samples
    jr z,@not.f2

    ld (hl),@demo.samples
    call show.samples
    jr @skip.f

  @not.f2:

    bit 7,c
    jr nz,@not.f3

    cp @demo.samples.ext
    jr z,@not.f3

    ld (hl),@demo.samples.ext
    call show.samples.ext
    jr @skip.f

  @not.f3:

 ; scan f4 f5 f6

    ld bc,keyboard.f6_f5_f4 * 256 + port.status
    in c,(c)

    bit 5,c
    jr nz,@not.f4

    cp @demo.tracker
    jr z,@not.f4

    ld (hl),@demo.tracker
    call show.pattern
    jr @skip.f

  @not.f4:

    bit 6,c
    jr nz,@not.f5

    cp @demo.effects
    jr z,@not.f5

    ld (hl),@demo.effects
    call show.summary
    jr @skip.f

  @not.f5:

    bit 7,c
    jr nz,@not.f6
    ld (hl),@demo.burst
    call show.burst
    jr @skip.f

  @not.f6:
  @skip.f:

 ; scan l

    ld a,keyboard.hjkl_return
    in a,(port.keyboard)
    cpl
    and %00000010
    jr z,@not.l

  @still.l:

    ld a,0
    or a
    jr nz,@not.l
    ld a,(disable.pos)
    xor 1
    ld (disable.pos),a

    ld a,(@trackon+1)
    cp @demo.tracker
    call z,print.loop.status

    ld a,1

  @not.l:

    ld (@still.l+1),a

 ; scan c

    ld a,keyboard.vcxz_shift
    in a,(port.keyboard)
    cpl
    and %00001000
    jr z,@not.c

  @still.c:

    ld a,0
    or a
    jr nz,@not.c

    ld a,(set.palette+1)
    cpl
    ld (set.palette+1),a
    call set.palette
    ld a,1

  @not.c:
    ld (@still.c+1),a

 ; scan shift

    ld a,keyboard.vcxz_shift
    in a,(port.keyboard)
    and %00000001
    ld bc,0x0040    ; delta 25%
    jr nz,$+5
    ld bc,0x0100    ; delta 100%

 ; scan +

    ld a,keyboard.delete_plus_minus
    in a,(port.status)
    and %01000000
    jr nz,@not.plus

    ld hl,(amplification.factor+1)
    add hl,bc
    ld a,h
    cp 10
    jr z,@not.plus

    ld (amplification.factor+1),hl
    call calculate.volume.tables

    ld a,(@trackon+1)
    cp @demo.tracker
    call z,print.amplification.factor

  @not.plus:

 ; scan -

    ld a,keyboard.delete_plus_minus
    in a,(port.status)
    and %00100000
    jr nz,@not.minus
    ld hl,(amplification.factor+1)
    sbc hl,bc
    jr c,@not.minus

    ld (amplification.factor+1),hl
    call calculate.volume.tables

    ld a,(@trackon+1)
    cp @demo.tracker
    call z,print.amplification.factor

  @not.minus:

 ; scan esc

    ld bc,0
    ld a,keyboard.caps_tab_esc
    in a,(port.status)
    and %00100000
    jr z,@exit

    ld a,(mstatus)    ;1=music stopped
    dec a
    jr z,@exit

    ; scan f9

    ld a,keyboard.f9_f8_f7
    in a,(port.status)
    and %10000000
    jr nz,@not.f9
    inc bc             ;bc <> 0

@exit:

    ld a,(@trackon+1)
    or a
    jr nz,$+3
    inc a
    ld (@trackon+1),a

    ld hl,(exit.burst)
    jp (hl)

 @not.f9:

    jp @demo.loop

;-------------------------------------------------------------------------------

    include "show.help.s"          ; F1
    include "show.samples.s"       ; F2
    include "show.samples.ext.s"   ; F3
    include "show.pattern.s"       ; F4
    include "show.summary.s"       ; F5
    include "show.burst.s"         ; F6

;-------------------------------------------------------------------------------
set.palette:

    ld a,0
    or a
    ld hl,demo.palette
    jr z,$+5
    ld hl,demo.palette.two

    ld de,frame.palette
    ld bc,16
    ldir
    ret

;-------------------------------------------------------------------------------
print.hi.nibble:

 ; print high nibble of A register at HL

    and 0xf0
    rrca
    rrca
    rrca
    push hl
    ld h,char.list / 256
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    jp (hl)

;-------------------------------------------------------------------------------
print.hex:

 ; print A as hex at hl

    ld c,a
    call print.hi.nibble
    ld a,c

;-------------------------------------------------------------------------------
print.lo.nibble:

 ; print low nibble of A register at HL

    and 0x0f
    rlca
    push hl
    ld h,char.list / 256
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    jp (hl)

;-------------------------------------------------------------------------------
print.fast.char:

 ; print A-G #

    rlca
    push hl
    ld h,char.list / 256
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    jp (hl)

;------------------------------------------------------------------------------
print.num:

    ld b,a
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    call @print.num.hex
    ld a,b
    and 0x0f

 @print.num.hex:

    add "0"
    cp ":"
    jr c,$+4
    add 7
    jp print.chr

;-------------------------------------------------------------------------------
print.space:

 ; blank out character at HL - assumes b = 32

    ld c,%00000000

 print.space.c:

    ld a,l
    ld (hl),c
    add b
    ld l,a
    ld (hl),c
    add b
    ld l,a
    ld (hl),c
    add b
    ld l,a
    ld (hl),c
    add b
    ld l,a
    ld (hl),c
    res 7,l     ; l has been increased by 4 * 32 = 128
    inc l

    ret

;------------------------------------------------------------------------------
@error:

    di
    ld a,4
    out (port.border),a
    jr @error

;------------------------------------------------------------------------------
print.chr:

 ; print chr$ A at HL

    push bc
    push de
    push hl

    cp " "
    jr c,@unprintable

    ld c,a
    cp 0x80
    jr c,@printable

 @unprintable:

    ld c," "

 @printable:

    ex de,hl
    ld b,0
    ld l,c
    ld h,b
    add hl,hl
    add hl,hl
    add hl,bc
    ld bc,loader.font_high - ( " " * 5 )
    add hl,bc
    ld b,5

    @loop:

        ld a,(hl)
        ld (de),a
        inc hl
        ld a,e
        add 32
        ld e,a
        jr nc,$+3
        inc d
        djnz @-loop

    pop hl
    pop de
    pop bc
    inc l

    ret

;------------------------------------------------------------------------------
print.screen:

    push de
    ld a,6
    call colour.scrn

    ld hl,screen
    pop de
    ld c,32

    @loop.rows:

        ld b,32
        push hl

        @loop.line:

            ld a,(de)
            inc de
            or a
            jr z,@eol
            call print.chr

            djnz@-loop.line

     @eol:

        pop hl
        ld a,l
        add screen.32.rows
        ld l,a
        jr nc,$+3
        inc h
        dec c

        jr nz,@-loop.rows

    ld a,-1

    ret

;-------------------------------------------------------------------------------
print.title:

    ld hl,screen
    ld de,mod.header
    ld b,mod.title.len
    call print.de.b

    ret

;-------------------------------------------------------------------------------
cls:

    ld hl,screen
    ld de,screen + 1
    ld bc,6143
    ld (hl),l
    ldir

    ret

;-------------------------------------------------------------------------------
print.de.b:

 ; print zero-terminated string (DE) for B characters

    @loop:
        ld a,(de)
        or a
        jr z,@eop

        inc de
        call print.chr

        djnz @-loop

    ret

    @eop:

        ld a," "
        inc de
        call print.chr
        djnz @eop

    ret

;-------------------------------------------------------------------------------
colour.scrn:

    ld (@line.size+1),a

    ld hl,screen.attributes

    @loop.data:

        ld c,(ix)
        inc ix

        @loop.rows:

            ld a,(ix)

            ld de,colours
            add a,a
            add a,a
            add a,a
            add a,e
            ld e,a
            jr nc,$+3
            inc d

         @line.size:
            ld b,8

            @loop.line:

                ld a,(de)
                inc de

                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l

                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l

                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l

                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc l
                ld (hl),a
                inc hl

                djnz @-loop.line

            dec c
            jr nz,@-loop.rows

        inc ix

        ld a,h
        cp ( screen.attributes + 0x1800 ) / 0x100

        jr nz,@-loop.data

    ret
;-------------------------------------------------------------------------------

colours:
    defb 0,0,0,0,0,0,0,0                    ;0 black
    defb 1,2,3,2,1,0,0,0                    ;1 blue
    defb 4,5,6,5,4,0,0,0                    ;2 orange
    defb 7,9+56,3,9+56,7,0,0,0              ;3 green
    defb 10+56,11+56,12+56,11+56,10+56,0,0,0;4 red
    defb 13+56,14+56,6,14+56,13+56,0,0,0    ;5 yellow

;-------------------------------------------------------------------------------
calculate.volume.tables:

    ld a,(loader.device)
    ld hl,@bits.per.device
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld b,(hl)         ;output bits

    include "../volume.s"

;-------------------------------------------------------------------------------
@bits.per.device:

    defb 3  ; saa
    defb 7  ; samdac
    defb 6  ; dac
    defb 6  ; blue alpha
    defb 8  ; quazar surround
    defb 6  ; clut

;-------------------------------------------------------------------------------
create.fast.print:

 ; create a fast print routine for hex digits (and G + # for notes)

    ld ix,char.list
    ld de,( "0" - " " ) * 5 + loader.font_high
    ld hl,build.font
    ld b,0

    @build.blp:

        ld a,b
        cp 10
        jr nz,$+5
        ld de,( "A" - " " ) * 5 + loader.font_high

        cp 17
        jr nz,$+5
        ld de,( "-" - " " ) * 5 + loader.font_high

        cp 18
        jr nz,$+5
        ld de,( "#" - " " ) * 5 + loader.font_high

        ld (ix+0),l
        ld (ix+1),h
        inc ix
        inc ix
        ld (hl),opcode.pop_hl
        inc hl
        ld (hl),opcode.ld_a_l
        inc hl
        ld c,4

        @build.clp:

            ld (hl),opcode.ld_hl_n
            inc hl
            ld a,(de)
            inc de
            ld (hl),a
            inc hl
            ld (hl),opcode.add_a_b
            inc hl
            ld (hl),opcode.ld_l_a
            inc hl
            dec c
            jr nz,@-build.clp

        ld (hl),opcode.ld_hl_n
        inc hl
        ld a,(de)
        inc de
        ld (hl),a
        inc hl
        ld (hl),opcode.cb
        inc hl
        ld (hl),opcode.res_7_l
        inc hl
        ld (hl),opcode.inc_l
        inc hl
        ld (hl),opcode.ret
        inc hl
        inc b
        ld a,b
        cp 19

        jr nz,@-build.blp

    ret

;-------------------------------------------------------------------------------
create.line.table:

    ld ix,line.table
    ld hl,screen.attributes
    ld de,screen.32.rows
    ld b,32

    @loop:

        ld (ix),l
        inc ix
        ld (ix),h
        inc ix
        add hl,de

        djnz @-loop

    ret

;-------------------------------------------------------------------------------
create.note.table:

    ld hl,period.note.table
    ld de,period.note.table + 1
    ld bc,511
    ld (hl),0
    ldir

    ld ix,period.note.periods

    ld c,1

    @loop.three.octaves:

        ld hl,period.note.notes

        ld b,12

        @loop.twelve.notes:

            ld a,(ix)
            add a,a
            ld e,a
            ld d,period.note.table / 256
            jr nc,@no.over
            inc d
        @no.over:

            ld a,(hl)
            sub "A" - 10    ; A -> 10 - F -> 15, G -> 16
            ld (de),a
            inc hl
            ld a,(hl)
            cp "#"
            jr nz,@not.sharp

            ld a,(de)
            set 7,a
            ld (de),a

        @not.sharp:

            inc de
            ld a,c
            ld (de),a

            inc ix
            inc ix          ; ignoring hi period
            inc hl

        djnz @-loop.twelve.notes

        inc c
        ld a,c
        cp 4

        jr c,@-loop.three.octaves

    ret

length: equ $-demo.setup

;-------------------------------------------------------------------------------
mod.header:     defs mod.pattern    ; first 1084 bytes of mod containing song name + sample info

;-------------------------------------------------------------------------------
    defs align 256

char.list:

 ; addresses to print routines for 0-9 A-F G - #

    defs 19 * 2

;-------------------------------------------------------------------------------
line.table:

 ; contains screen address per row in 32 row mode (all but pattern screen)

    defs 32*2

;-------------------------------------------------------------------------------
period.note.notes:

 ; for three octave mods the low byte can identify the note, each entry is two bytes

    defm "C "
    defm "C#"
    defm "D "
    defm "D#"
    defm "E "
    defm "F "
    defm "F#"
    defm "G "
    defm "G#"
    defm "A "
    defm "A#"
    defm "B "

period.note.periods:

  ; octave 1
    defw 0x358
    defw 0x328
    defw 0x2fa
    defw 0x2d0
    defw 0x2a6
    defw 0x280
    defw 0x25c
    defw 0x23a
    defw 0x21a
    defw 0x1fc
    defw 0x1e0
    defw 0x1c5

  ; octave 2
    defw 0x1ac
    defw 0x194
    defw 0x17d
    defw 0x168
    defw 0x153
    defw 0x140
    defw 0x12e
    defw 0x11d
    defw 0x10d
    defw 0x0fe
    defw 0x0f0
    defw 0x0e2

  ; octave 3
    defw 0x0d6
    defw 0x0ca
    defw 0x0be
    defw 0x0b4
    defw 0x0aa
    defw 0x0a0
    defw 0x097
    defw 0x08f
    defw 0x087
    defw 0x07f
    defw 0x078
    defw 0x071

    defs align 0x100

period.note.table:

     defs 0x200
;-------------------------------------------------------------------------------

build.font:
