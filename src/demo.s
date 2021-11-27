;SAM MOD player - demo

;(C) 1996-2021 Stefan Drissen

include "memory.i"
include "ports/internal.i"
include "ports/megabyte.i"
include "ports/keyboard.i"
include "constants/mod.i"
include "constants/opcodes.i"

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

;-------------------------------------------------------------------------------
@fix.page:
    and high.memory.page.mask

    ret

;-------------------------------------------------------------------------------
@setupmod:

 ; input:
 ; - a = octaves [3,5]
 ; - c = RAM

    ex af,af'

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
    ld bc,mod.header.len
    ldir

    ld a,page.tracker
    call @fix.page
    out (port.hmpr),a
    ld hl,demo
    ld (tracker.ptr.addr.demo),hl
    ld a,page.demo - 1  ; - 1 since demo is in D
    call @fix.page
    ld (tracker.ptr.page.demo),a
    ex af,af'
    ld (tracker.octaves),a
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

col.pattern:
    defb 1,3,1,1,1,4,9,1,1,4,9,1,1,5,1,2

col.samples:
    defb 1,3,31,1

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
    call z,show.sizes
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
    ld hl,video.memory.24.rows * 0 + 24 + video.memory.high
    ld a,(song.pos)
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

    ld a,(pattern.pos)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble

 @skip.patpos:

    ld a,(@trackon+1)
    cp @demo.tracker
    jr nz,@skip.speed

    ; print speed and tempo

    ld hl,video.memory.24.rows * 1 + 18 + video.memory.high
    ld a,(speed)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble

    ld hl,(tempo)
    ld b,h
    ld c,l
    srl h
    rr l
    srl h
    rr l
    adc hl,bc
    ld b,h
    ld c,l
    ld hl,video.memory.24.rows * 1 + 29 + video.memory.high
    call do.percent

 @skip.speed:

 ; scan cursor left / right

    ld bc,keyboard.cursors_cntrl * 256 + port.keyboard
    in c,(c)
    bit 3,c
    jr nz,@not.left

    ld hl,pattern.pos
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
    ld a,(counter)
    or a
    jr nz,@wait.left

  @not.left:

    bit 4,c
    jr nz,@not.right
    ld a,(speed)
    ld hl,counter
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
    call show.sizes
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
    call print.loop.status
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
    call tables
    ld a,(@trackon+1)
    cp @demo.tracker
    call z,pr.amp.fac

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
    call tables
    ld a,(@trackon+1)
    cp @demo.tracker
    call z,pr.amp.fac

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
; PATTERN TRACKER for MOD player

 ; runs off tracker frame interrupt
 ; only run when frame counter <> 0

pattern.interrupt:

    ld hl,(counter.fract)
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

    ld a,(counter)
    or a
    ret nz

    ld (@pr.set+1),a

 @print.channel.1_2:

    ld hl,video.memory.24.rows * 3 + 3 + video.memory.high

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
    call print.hi.nibble
    ld a,e
    call print.lo.nibble

    jr @continue.2

 @blank.2:

    call print.space
    call print.space.c
    call print.space.c

 @continue.2:

    inc l

    ld a,(ix+2)             ; command
    and 0x0f

    jr z,@blank

    call print.lo.nibble
    inc l
    ld a,(ix+3)             ; command parameter
    ld e,a
    call print.hi.nibble
    ld a,e
    call print.lo.nibble

    ret

 @blank:

    call print.space
    inc l
    call print.space.c
    call print.space.c

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

    ld hl,video.memory.high
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
        add video.memory.32.rows
        ld l,a
        jr nc,$+3
        inc h
        dec c

        jr nz,@-loop.rows

    ld a,-1

    ret

;------------------------------------------------------------------------------
show.help:

 ; F1

    ld a,-1
    ld (int.rtn.pag),a
    call cls
    ld ix,col.help
    ld de,txt.help

    jp print.screen

;------------------------------------------------------------------------------
show.summary:

 ; F5

    ld a,-1
    ld (int.rtn.pag),a
    call cls
    ld ix,col.pro
    ld de,txt.prosummary
    jp print.screen

;------------------------------------------------------------------------------
show.burst:

 ; F6

    ld hl,(int.routine)
    ld de,burst.interrupt
    or a
    sbc hl,de
    jr nz,@sb.no.inc    ; only up channel if in this mode already

    ld hl,burst.num+1
    ld a,(hl)
    inc a
    and 3
    ld (hl),a

 @sb.no.inc:

    ld a,-1
    ld (int.rtn.pag),a
    call cls
    ld ix,col.burst
    ld a,8
    call colour.scrn

    ld de,txt.burst
    ld hl,video.memory.24.rows * 0 + 0 + video.memory.high
    ld b,32
    call print.de.b
    ld hl,video.memory.24.rows * 1 + 0 + video.memory.high
    ld b,23
    call print.de.b

    ld hl,video.memory.24.rows * 3 + 0 + video.memory.high
    ld b,8
    call print.de.b
 burst.num:
    ld a,0
    add "1"
    call print.chr

    ld hl,video.memory.24.rows * 4 + 0 + video.memory.high
    ld b,21
    call print.de.b

    ld hl,bp.pointers.sample
    ld de,bp.pointers.length
    ld a,(burst.num+1)
    or a
    jr z,@ok

    ld b,a
    @loop:
        add hl,de
        djnz @-loop

 @ok:

    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (bi.page+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (bi.offs2+1),de
    inc de
    ld (bi.offs1+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (bi.vol+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (bi.slo+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    ld (bi.shi+1),de

    ld hl,burst.interrupt
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1
    ret

 ;------------------------------------------------------------------------------
 burst.interrupt:

    ld a,(counter)
    or a
    ret nz

  burst.pr.pos:
    ld hl,video.memory.24.rows * 5 + 2 + video.memory.high
    ld b,32
  bi.page:
    ld a,(0)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    inc l
 bi.offs1:
    ld a,(0)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
 bi.offs2:
    ld a,(0)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    inc l
    inc l
 bi.vol:
    ld a,(0)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    inc l
    inc l
 bi.slo:
    ld a,(0)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    inc l
    inc l
 bi.shi:
    ld a,(0)
    ld c,a
    call print.hi.nibble
    ld a,c
    call print.lo.nibble
    ld hl,burst.pr.pos+2
    ld a,(hl)
    inc a
    cp 24 + 128
    jr nz,$+4
    ld a,5 + 128
    ld (hl),a
    ret

;-------------------------------------------------------------------------------
show.pattern:

 ; F4

    ld a,-1
    ld (int.rtn.pag),a
    xor a
    ld (@print.pos+1),a
    call cls

    ld ix,col.pattern
    ld a,8
    call colour.scrn

    ld hl,video.memory.24.rows * 3 + 1 + video.memory.high.attributes
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

    call pr.title

    ld de,txt.volume
    ld hl,video.memory.24.rows * 1 + 0 + video.memory.high
    ld b,32
    call print.de.b

    call pr.amp.fac

    ld de,txt.keys
    ld hl,video.memory.24.rows * 22 + 0 + video.memory.high
    ld b,32
    call print.de.b

    ld de,txt.author
    ld hl,video.memory.24.rows * 23 + 0 + video.memory.high
    ld b,32
    call print.de.b

    ld de,txt.channel
    ld hl,video.memory.24.rows * 2 + 1 + video.memory.high
    ld b,8
    call print.de.b
    ld a,"1"
    call print.chr

    ld de,txt.channel
    ld hl,video.memory.24.rows * 2 + 22 + video.memory.high
    ld b,8
    call print.de.b
    ld a,"2"
    call print.chr

    ld de,txt.channel
    ld hl,video.memory.24.rows * 12 + 1 + video.memory.high
    ld b,8
    call print.de.b
    ld a,"4"
    call print.chr

    ld de,txt.channel
    ld hl,video.memory.24.rows * 12 + 22 + video.memory.high
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
print.loop.status:

    ld a,(@trackon+1)
    cp @demo.tracker
    ret nz

    ld a,(disable.pos)
    or a
    ld a,"Y"
    jr z,$+4
    ld a,"N"
    ld hl,video.memory.24.rows * 22 + 31 + video.memory.high
    jp print.chr

;-------------------------------------------------------------------------------
ss.cursor:

    defb %10000000,%00000001
    defb %11000000,%00000011
    defb %11100000,%00000111
    defb %11000000,%00000011
    defb %10000000,%00000001

;-------------------------------------------------------------------------------
show.samples:

 ; F2

    call set.show.smp   ; returns c = # samples, de = sample table

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

set.samp.int:

    ld hl,sample.cursors
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1
    ret

;-------------------------------------------------------------------------------
set.show.smp:

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

    ld ix,col.samples
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

        ld ix,ss.cursor
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

    call pr.title

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
show.sizes:

 ; F3

    call set.show.smp   ; returns c = # samples, de = sample table

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

    jp set.samp.int

;-------------------------------------------------------------------------------
pr.title:

    ld hl,video.memory.high
    ld de,mod.header
    ld b,mod.title.len
    call print.de.b

    ret

;-------------------------------------------------------------------------------
cls:

    ld hl,video.memory.high
    ld de,video.memory.high + 1
    ld bc,6143
    ld (hl),l
    ldir

    ret

;-------------------------------------------------------------------------------
print.de.b:

 ; print zero-terminated string (DE) for B characters

    ld a,(de)
    or a
    jr z,@eop
    inc de
    call print.chr

    djnz print.de.b

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

    ld hl,video.memory.high.attributes

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
        cp ( video.memory.high.attributes + 6144 ) / 256

        jr nz,@-loop.data

    ret

;-------------------------------------------------------------------------------
sample.cursors:

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

;-------------------------------------------------------------------------------

txt.help:
    defm "SAM MOD player             "
    include "constants/txt.version.i"
    include "constants/txt.copyright.i"
    defb 0,0
    defm "           HELP PAGE"
    defb 0,0
    defm "* F1: help page, F2: list names "
    defm "  F3: list sizes, F4: tracker,  "
    defm "  F5: summary effects (column 3)"
    defm "  F6: techy page - burst info"
    defb 0,0
    defm "* 1, 2, 3, 4: un/mute channel"
    defb 0,0
    defm "* P: pause/play"
    defb 0,0
    defm "* Cursors: rewind/fast forward"
    defb 0,0
    defm "* ESC: stop tune, load another"
    defb 0,0
    defm "* L: loop tune on/off"
    defb 0,0
    defm "* On F2/F3 screen, the first    "
    defm "  number is the sample length   "
    defm "  in words, the second number is"
    defm "  the default volume."
    defb 0,0
    defm "* Track-mode skips rows if the  "
    defm "  song speed is too fast."
    defb 0,0
    defm "* The three digits in the top   "
    defm "  right corner of the screen are"
    defm "  song position, pattern number "
    defm "  and pattern row."
    defb 0
col.help:
    defb 3,2,2,4,5,1,2,3,2,1,2,3,2,1,2,3,5,1,3,3,4,1

txt.prosummary:
    defm "SAM MOD player             "
    include "constants/txt.version.i"
    include "constants/txt.copyright.i"
    defb 0,0
    defm " SUMMARY OF PROTRACKER EFFECTS"
    defb 0,0
    defm "0 Arpeggio"
    defb 0
    defm "1 Portamento Up       (speed xy)"
    defm "2 Portamento Down     (speed xy)"
    defm "3 Tone Portamento     (speed xy)"
    defm "4 Vibrato (speed x, amplitude y)"
    defm "5 Tone and Volume Slide"
    defb 0
    defm "6 Vibrato and Volume Slide"
    defb 0
    defm "7 Tremolo (speed x, amplitude y)"
    defm "8 Undefined"
    defb 0
    defm "9 Sample Offset (512 bytes * xy)"
    defm "A Volume Slide    (up x, down y)"
    defm "B Position Jump          (to xy)"
    defm "C Volume Change          (to xy)"
    defm "D Pattern Break  (to row xy dec)"
    defm "E Extra effects (x=com, y=param)"
    defm "F Set Speed or Tempo if xy > 20 "
    defb 0
    defm "   EXTRA EFFECTS (E-command)"
    defb 0,0
    defm "0 filter      * 8 undefined"
    defb 0
    defm "1 fine porta up 9 retrigger note"
    defm "2 fine porta dn A volume fine up"
    defm "3 gliss control B volume fine dn"
    defm "4 vibrato cntrl C note cut"
    defb 0
    defm "5 set fine tune D note delay    "
    defm "6 jump loop     E pattern delay "
    defm "7 tremolo cntrl F no standard  *"
col.pro:
    defb 3,2,2,4,17,1,2,4,8,3

txt.burst:
    defm "SAM MOD player             "
    include "constants/txt.version.i"
    include "constants/txt.copyright.i"
    defm "CHANNEL "
    defm "Page Offs Vol SLo SHi"

col.burst:
    defb 3,2,1,4,1,5,19,1

colours:
    defb 0,0,0,0,0,0,0,0                    ;0 black
    defb 1,2,3,2,1,0,0,0                    ;1 blue
    defb 4,5,6,5,4,0,0,0                    ;2 orange
    defb 7,9+56,3,9+56,7,0,0,0              ;3 green
    defb 10+56,11+56,12+56,11+56,10+56,0,0,0;4 red
    defb 13+56,14+56,6,14+56,13+56,0,0,0    ;5 yellow

txt.channel:    defm "Channel "

txt.volume:     defm "Vol: 000%  Speed: 00  Tempo: 000"
txt.keys:       defm "F1-F6 1234 C <> P ESC -+ Loop:  "
txt.author:     include "constants/txt.copyright.i"
                defm "    "
                include "constants/txt.version.i"


;-------------------------------------------------------------------------------
pr.amp.fac:

    ld hl,video.memory.24.rows * 1 + 5 + video.memory.high
    ld bc,(amplification.factor+1)

 do.percent:
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

    ld a,h

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
tables:

    ld a,(loader.device)
    ld hl,@bits.per.device
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld b,(hl)         ;output bits

    include "volume.s"

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
    ld hl,video.memory.high.attributes
    ld de,video.memory.32.rows
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
mod.header.len: equ mod.pt.pattern
mod.header:     defs mod.header.len ; first 1084 bytes of mod containing song name + sample info

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
