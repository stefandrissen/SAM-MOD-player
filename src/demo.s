;SAM MOD player - demo

;(C) 1996-2021 Stefan Drissen

include "memory.i"
include "ports/internal.i"
include "ports/megabyte.i"
include "ports/keyboard.i"
include "constants/opcodes.i"

;---------------------------------------------------------------

demo.device:    equ 49152+3     ; device, set by loader [0-5]   ; !!! external ram seems to be there now


;===============================================================
; demo is the program that runs in "foreground" mode
;   the tracker is called by the burst routine every frame

    org demo.setup  ; 0x6000 - aBcd

;---------------------------------------------------------------

demo.setup: jp @setupmod

;---------------------------------------------------------------

demo.ram:           defb 0  ; %XXXRR (RAM / 256K)

@fix.page:
    and high.memory.page.mask

    ret

@setupmod:
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

;---------------------------------------------------------------

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

;===============================================================
;this is the routine that is running in "foreground mode"

demo:

    ld bc,0x0f00 + port.clut
    xor a
black:
    out (c),a
    djnz black
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

first.time:
    ld a,0
    or a
    jr nz,skip.intro
    cpl
    ld (int.rtn.pag),a
    ld (first.time+1),a

    ld a,4
    ld (trackon+1),a

skip.intro:

    ld a,(trackon+1)
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
    ld (mk.enable+1),hl
mk.enable:
    call 0

;---------------------------------------------------------------

demo.loop:

trackon:
    ld a,0
    cp 2
    jr c,skip.patpos
    cp 5
    jr nc,skip.patpos

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

skip.patpos:

    ld a,(trackon+1)
    cp 4
    jr nz,skip.speed

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

skip.speed:

    ld bc,keyboard.cursors_cntrl * 256 + port.keyboard
    in c,(c)
    bit 3,c
    jr nz,not.left
    ld hl,pattern.pos
    dec (hl)
    dec (hl)
    bit 7,(hl)
    jr z,wait.left
    ld (hl),62
    dec l
    dec l
    dec (hl)
    bit 7,(hl)
    jr z,wait.left
    ld (hl),0
    inc l
    inc l
    ld (hl),0
wait.left:
    ei              ;just in case we're pausing
    ld a,(counter)
    or a
    jr nz,wait.left
not.left:
    bit 4,c
    jr nz,not.right
    ld a,(speed)
    ld hl,counter
    ld (hl),a
wait.right:
    ei
    ld a,(mstatus)    ;just in case end of tune
    dec a
    jp z,exit
    ld a,(hl)
    dec a
    jr nz,wait.right
not.right:

    ld bc,keyboard.12345 * 256 + port.keyboard
    in c,(c)
    xor a
    bit 0,c
    jr nz,not.key.1
still.key.1:
    ld a,0
    or a
    jr nz,key.2
    ld hl,c1.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a
not.key.1:
    ld (still.key.1+1),a

key.2:
    xor a
    bit 1,c
    jr nz,not.key.2
still.key.2:
    ld a,0
    or a
    jr nz,key.3
    ld hl,c2.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a
not.key.2:
    ld (still.key.2+1),a

key.3:
    xor a
    bit 2,c
    jr nz,not.key.3
still.key.3:
    ld a,0
    or a
    jr nz,key.4
    ld hl,c3.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a
not.key.3:
    ld (still.key.3+1),a

key.4:
    xor a
    bit 3,c
    jr nz,not.key.4
still.key.4:
    ld a,0
    or a
    jr nz,key.p
    ld hl,c4.on
    ld a,(hl)
    cpl
    ld (hl),a
    ld a,1
    ld (vol.update),a
not.key.4:
    ld (still.key.4+1),a

key.p:
    xor a
    ld bc,keyboard.yuiop * 256 + port.keyboard
    in c,(c)
    bit 0,c
    jr nz,not.key.p
still.key.p:
    ld a,0
    or a
    jr nz,still.p
ints.on:
    ld a,0
    or a
    jr z,pause
    xor a
    ei
    jr cont.p
pause:
    ld a,1
    di
cont.p:
    ld (ints.on+1),a
    ld a,1
not.key.p:
    ld (still.key.p+1),a
still.p:


    ld hl,trackon+1
    ld a,(hl)

    ld bc,keyboard.f3_f2_f1 * 256 + port.status
    in c,(c)
    bit 5,c
    jr nz,not.f1
    cp 1
    jr z,not.f1
    ld (hl),1
    call show.help
    jr skip.f
not.f1:
    bit 6,c
    jr nz,not.f2
    cp 2
    jr z,not.f2
    ld (hl),2
    call show.samples
    jr skip.f
not.f2:
    bit 7,c
    jr nz,not.f3
    cp 3
    jr z,not.f3
    ld (hl),3
    call show.sizes
    jr skip.f
not.f3:
    ld bc,keyboard.f6_f5_f4 * 256 + port.status
    in c,(c)
    bit 5,c
    jr nz,not.f4
    cp 4
    jr z,not.f4
    ld (hl),4
    call show.pattern
    jr skip.f
not.f4:
    bit 6,c
    jr nz,not.f5
    cp 5
    jr z,not.f5
    ld (hl),5
    call show.summary
    jr skip.f
not.f5:
    bit 7,c
    jr nz,not.f6
    ld (hl),6
    call show.burst
    jr skip.f
not.f6:
skip.f:
    ld a,keyboard.hjkl_return
    in a,(port.keyboard)
    cpl
    and %00000010
    jr z,not.l
still.l:
    ld a,0
    or a
    jr nz,not.l
    ld a,(disable.pos)
    xor  1
    ld (disable.pos),a
    call pr.loop.st
    ld a,1
not.l:
    ld (still.l+1),a

    ld a,keyboard.vcxz_shift
    in a,(port.keyboard)
    cpl
    and %00001000
    jr z,not.c
still.c:
    ld a,0
    or a
    jr nz,not.c
    ld a,(set.palette+1)
    cpl
    ld (set.palette+1),a
    call set.palette
    ld a,1
not.c:
    ld (still.c+1),a

    ld a,keyboard.vcxz_shift
    in a,(port.keyboard)
    and %00000001   ; shift
    ld bc,64
    jr nz,$+5
    ld bc,256

    ld a,keyboard.delete_plus_minus
    in a,(port.status)
    and %01000000   ; +
    jr nz,not.plus

    ld hl,(amp.fac+1)
    add hl,bc
    ld a,h
    cp 10
    jr z,not.plus
    ld (amp.fac+1),hl
    call tables
    ld a,(trackon+1)
    cp 4
    call z,pr.amp.fac
not.plus:

    ld a,keyboard.delete_plus_minus
    in a,(port.status)
    and %00100000   ; -
    jr nz,not.minus
    ld hl,(amp.fac+1)
    sbc hl,bc
    jr c,not.minus
    ld (amp.fac+1),hl
    call tables
    ld a,(trackon+1)
    cp 4
    call z,pr.amp.fac
not.minus:
    ld bc,0
    ld a,keyboard.caps_tab_esc
    in a,(port.status)
    and %00100000
    jr z,exit

    ld a,(mstatus)    ;1=music stopped
    dec a
    jr z,exit

    ld a,keyboard.f9_f8_f7
    in a,(port.status)
    and %10000000
    jr nz,not.f9
    inc bc             ;bc <> 0
exit:
    ld a,(trackon+1)
    or a
    jr nz,$+3
    inc a
    ld (trackon+1),a

    ld hl,(exit.burst)
    jp (hl)
not.f9:
    ld a,keyboard.caps_tab_esc
    in a,(port.status)
    and %10000000           ; caps
    jr nz,@not.reset
    ld a,keyboard.cursors_cntrl
    in a,(port.keyboard)
    and %00000001           ; control
    jr nz,@not.reset
    ld a,keyboard.edit_colon_semicolon
    in a,(port.status)
    and %10000000           ; edit
    jr nz,@not.reset

    di
    xor a
    out (port.vmpr),a
still.res2:
    ld a,keyboard.caps_tab_esc
    in a,(port.status)
    and %10000000           ; caps
    jr z,still.res2
    ld a,keyboard.cursors_cntrl
    in a,(port.keyboard)
    and %00000001           ; control
    jr z,still.res2
    ld a,keyboard.edit_colon_semicolon
    in a,(port.status)
    and %10000000           ; edit
    jr z,still.res2
reset:
    xor a
    out (port.lmpr),a
    rst 0

@not.reset:

    jp demo.loop


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

;===============================================================

;PATTERN TRACKER for MOD player
;(C) 1995-2021 Stefan Drissen
;
;runs off tracker frame interrupt
;only run when frame counter <> 0

printer:
    ld hl,(counter.fract)
    ld de,(tempo)
    add hl,de
    ld a,(speed)
    dec a
    cp h
    ret c

pr.set:
    ld a,0
    or a
    jp z,print43

    ld a,(counter)
    or a
    ret nz
    ld (pr.set+1),a
print12:
    ld hl,video.memory.24.rows * 3 + 3 + video.memory.high
print.pos:
    ld a,0
    add h
    ld h,a

    ld b,32                 ; used
    ld ix,mod.current.row
    call print.channel

    ld ix,mod.current.row + 4
    ld l,18
    call print.channel

    ld hl,c1.on
    ld d,(hl)
    inc l
    ld e,(hl)
    ld a,(print.pos+1)
    add 128
    ld l,1   ;width offset
    ld c,3   ;height offset
    add c
    ld h,a
print.pointer:
    ld a,d
    or a
    jr z,pp.skipleft

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

pp.skipleft:
    ld a,e
    or a
    jr z,pp.skiprite
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
pp.skiprite:
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

print43:
    inc a
    ld (pr.set+1),a
    ld a,(print.pos+1)
    add 13+128
    ld h,a
    ld l,18

    ld b,32

    ld ix,mod.current.row+8
    call print.channel

    ld l,3
    ld ix,mod.current.row+12
    call print.channel

    ld hl,c4.on
    ld d,(hl)
    dec l
    ld e,(hl)
    ld a,(print.pos+1)
    ld c,13
    add c
    add 128
    ld h,a
    ld l,1
    call print.pointer
    ld hl,print.pos+1
    ld a,(hl)
    inc a
    and %10000111
    ld (hl),a
    ret

;==============================================================================
print.channel:

; prints information about channel

; input: ix = pattern.row channel
; input: hl = screen address
;------------------------------------------------------------------------------

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

    jr @continue.3

@blank:

    call print.space
    inc l
    call print.space.c
    call print.space.c

@continue.3:

    ret

;==============================================================================
print.hi.nibble:

; print high nibble of A register at HL
;------------------------------------------------------------------------------

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

;==============================================================================
print.lo.nibble:

; print low nibble of A register at HL
;------------------------------------------------------------------------------

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

;==============================================================================
print.fast.char:

; print A-G #
;------------------------------------------------------------------------------

    rlca
    push hl
    ld h,char.list / 256
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a
    jp (hl)

print.num:
    ld b,a
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    call pr.num.hex
    ld a,b
    and 0x0f
pr.num.hex:
    add "0"
    cp ":"
    jr c,$+4
    add 7
    jp print.chr


;==============================================================================
print.space:

; blank out character at HL - assumes b = 32
;------------------------------------------------------------------------------

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

@error:
    di
    ld a,4
    out (port.border),a
    jr @error


;print chr$ A at HL

print.chr:
    push bc
    push de
    push hl
    ld c," "
    cp " "
    jr c,unprintable
    cp 128
    jr nc,unprintable
    ld c,a
unprintable:
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
pr.chr.blp:
    ld a,(hl)
    ld (de),a
    inc hl
    ld a,e
    add 32
    ld e,a
    jr nc,$+3
    inc d
    djnz pr.chr.blp
    pop hl
    pop de
    pop bc
    inc l
    ret

print.screen:
    push de
    ld a,6
    call colour.scrn

    ld hl,video.memory.high
    pop de
    ld c,32
wel.all:
    ld b,32
    push hl

pr.scr.blp:
    ld a,(de)
    inc de
    or a
    jr z,end.of.line
    call print.chr
    djnz pr.scr.blp
end.of.line:
    pop hl
    ld a,l
    add video.memory.32.rows
    ld l,a
    jr nc,$+3
    inc h
    dec c
    jr nz,wel.all
    ld a,255
    ret

;------------------------------------------------------------------------------
show.help:

; F1
;------------------------------------------------------------------------------

    ld a,-1
    ld (int.rtn.pag),a
    call cls
    ld ix,col.help
    ld de,txt.help
    jp print.screen

;------------------------------------------------------------------------------
show.summary:

; F5
;------------------------------------------------------------------------------

    ld a,-1
    ld (int.rtn.pag),a
    call cls
    ld ix,col.pro
    ld de,txt.prosummary
    jp print.screen

;------------------------------------------------------------------------------
show.burst:

; F6
;------------------------------------------------------------------------------

    ld hl,(int.routine)
    ld de,burst.int
    or a
    sbc hl,de
    jr nz,sb.no.inc     ;only up channel if in this mode already
    ld hl,burst.num+1
    ld a,(hl)
    inc a
    and 3
    ld (hl),a
sb.no.inc:
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
    djnz @loop
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


    ld hl,burst.int
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1
    ret

burst.int:

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

;------------------------------------------------------------------------------
show.pattern:

; F4
;------------------------------------------------------------------------------

    ld a,-1
    ld (int.rtn.pag),a
    xor a
    ld (print.pos+1),a
    call cls

    ld ix,col.pattern
    ld a,8
    call colour.scrn

    ld hl,video.memory.24.rows * 3 + 1 + video.memory.high.attributes
    ld c,24 - 3 - 3
    ld de,29
col.lp2:
    ld ix,2 * 8 + colours
    ld b,8
col.lp1:
    ld a,(ix)
    inc ix
    ld (hl),a
    add hl,de
    ld (hl),a
    inc hl
    inc hl
    inc hl
    djnz col.lp1

    dec c
    ld a,c
    cp 9
    jr nz,$+3
    inc h
normal:
    or a
    jr nz,col.lp2

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

    call pr.loop.st

    ld a,2
    ld (pr.set+1),a

    ld hl,printer
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1
    ret

pr.loop.st:
    ld a,(trackon+1)    ;if not on track page then don't print loop status
    cp 4
    ret nz
    ld a,(disable.pos)
    or a
    ld a,"Y"
    jr z,$+4
    ld a,"N"
    ld hl,video.memory.24.rows * 22 + 31 + video.memory.high
    jp print.chr


ss.pointer:
    defb %10000000,%00000001
    defb %11000000,%00000011
    defb %11100000,%00000111
    defb %11000000,%00000011
    defb %10000000,%00000001

;------------------------------------------------------------------------------
show.samples:

; F2
;------------------------------------------------------------------------------

    call set.show.smp
pr.ins.clp:
    ld b,22

    call print.de.b

    inc l

    ld a,(de)
    ld b,a
    inc de
    ld a,(de)
    or b
    jr nz,pr.ins.exist
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
    jr pr.next.ins
pr.ins.exist:
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
pr.next.ins:
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
    jp nz,pr.ins.clp
set.samp.int:
    ld hl,instr.point
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1
    ret

set.show.smp:
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
ss.col.lp1:
    ld (hl),a
    add hl,de
    ld (hl),a
    inc hl
    djnz ss.col.lp1

    ld hl,video.memory.32.rows * 1 + 0 + video.memory.high
    ld c,31
ss.pr.pntc:
    ld ix,ss.pointer
    ld b,6
ss.pr.pntb:
    ld a,(ix)
    inc ix
    ld (hl),a
    add hl,de
    ld a,(ix)
    inc ix
    ld (hl),a
    inc hl
    djnz ss.pr.pntb
    dec c
    jr nz,ss.pr.pntc

    call pr.title

    ld c,31
    ld hl,mod.header+1080
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
    ld de,mod.header+20
    ret

;------------------------------------------------------------------------------
show.sizes:

; F3
;------------------------------------------------------------------------------

    call set.show.smp
pr.size.clp:
    ld b,9

    call print.de.b

    inc l

    ld a,e
    add 22-9
    ld e,a
    jr nc,$+3
    inc d

    ld a,(de)
    ld b,a
    inc de
    ld a,(de)
    or b
    jr nz,pr.ins.exis2

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
    jr pr.next.in2
pr.ins.exis2:
    ld a,b
    call print.num
    ld a,(de)
    inc de
    call print.num
    inc l

    ld a,(de)
    and 0x0f
    bit 3,a
    jr z,tune.plus
    ld a,"-"
    call print.chr
    ld a,(de)
    and 0x0f
    ld b,a
    ld a,16
    sub b
    jr got.tune
tune.plus:
    ld a,"+"
    call print.chr
    ld a,(de)
    and 0x0f
got.tune:
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

pr.next.in2:
    ld a,l
    add video.memory.32.rows - 30
@fin.loop:
    ld l,a
    jr nc,$+3
    inc h

    dec c
    jp nz,pr.size.clp

    jp set.samp.int

pr.title:
    ld hl,video.memory.high
    ld de,mod.header
    ld b,20
    call print.de.b
    ret
cls:
    ld hl,video.memory.high
    ld de,video.memory.high + 1
    ld bc,6143
    ld (hl),l
    ldir
    ret

;------------------------------------------------------------------------------
print.de.b:

; print zero-terminated string (DE) for B characters
;------------------------------------------------------------------------------

    ld a,(de)
    or a
    jr z,eop
    inc de
    call print.chr
    djnz print.de.b
    ret

eop:
    ld a," "
    inc de
    call print.chr
    djnz eop
    ret

colour.scrn:
    ld (line.size+1),a

    ld hl,video.memory.high.attributes
col.loop:
    ld c,(ix)
    inc ix
col.clp1:
    ld a,(ix)

    ld de,colours
    add a,a
    add a,a
    add a,a
    add a,e
    ld e,a
    jr nc,$+3
    inc d

line.size:
    ld b,8
col.blp2:
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

    djnz col.blp2

    dec c
    jr nz,col.clp1
    inc ix

    ld a,h
    cp ( video.memory.high.attributes + 6144 ) / 256
    jr nz,col.loop
    ret

;---------------------------------------------------------------
;display pointers on sample name screen

instr.point:
    ld a,(counter)
    or a
    ret nz

    ld b,32

    ld a,(c1.inst+1)
    or a
    ld c,0
    call nz,clr.point

    ld a,(c4.inst+1)
    or a
    ld c,0
    call nz,clr.point

    ld a,(c2.inst+1)
    or a
    ld c,31
    call nz,clr.point

    ld a,(c3.inst+1)
    or a
    ld c,31
    call nz,clr.point

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
    jr z,skip.c1
c1.inst:
    ld a,0
    or a
    ld c,0
    call nz,col.point1
skip.c1:
    ld a,(c4.on)
    or a
    jr z,skip.c4
c4.inst:
    ld a,0
    or a
    ld c,0
    call nz,col.point2
skip.c4:
    ld a,(c2.on)
    or a
    jr z,skip.c2
c2.inst:
    ld a,0
    or a
    ld c,31
    call nz,col.point1
skip.c2:
    ld a,(c3.on)
    or a
    jr z,skip.c3
c3.inst:
    ld a,0
    or a
    ld c,31
    call nz,col.point2
skip.c3:
    ret

clr.point:
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

col.point1:
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

col.point2:
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

;---------------------------------------------------------------

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


pr.amp.fac:
    ld hl,video.memory.24.rows * 1 + 5 + video.memory.high
    ld bc,(amp.fac+1)

;convert &xx.xx to %
;entry = BC

do.percent:
    ld a,b
    or a
    jr nz,$+4
    ld a," " - "0"
    add "0"
    call print.chr
    ex de,hl
    ld hl,0
    ld b,h
    add hl,bc
    add hl,hl
    add hl,hl
    add hl,bc
    add hl,hl
    ld a,h
    add "0"
    ex de,hl
    call print.chr
    ex de,hl
    ld c,l
    ld b,0
    ld h,b
    add hl,hl
    add hl,hl
    add hl,bc
    add hl,hl
    ld a,h
    add "0"
    ex de,hl
    jp print.chr


;---------------------------------------------------------------

tables:

    ld a,(demo.device)
    ld hl,bits.per.dev
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld b,(hl)         ;output bits

include "volume.i"

;===============================================================
bits.per.dev:

    defb 3  ; saa
    defb 7  ; samdac
    defb 6  ; dac
    defb 6  ; blue alpha
    defb 8  ; quazar surround
    defb 6  ; clut

;==============================================================================

;------------------------------------------------------------------------------
create.fast.print:

; create a fast print routine for hex digits (and G + # for notes)

    ld ix,char.list
    ld de,( "0" - " " ) * 5 + loader.font_high
    ld hl,build.font
    ld b,0
build.blp:
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
build.clp:
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
    jr nz,build.clp
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
    jr nz,build.blp

    ret

;------------------------------------------------------------------------------
create.line.table:

    ld ix,line.table
    ld hl,video.memory.high.attributes
    ld de,video.memory.32.rows
    ld b,32
b.line:
    ld (ix),l
    inc ix
    ld (ix),h
    inc ix
    add hl,de
    djnz b.line

    ret

;------------------------------------------------------------------------------
create.note.table:

    ld hl,period.note.table
    ld de,period.note.table + 1
    ld bc,511
    ld (hl),0
    ldir

    ld ix,period.note.periods

    ld c,1

@three.octaves:

    ld hl,period.note.notes

    ld b,12
@twelve.notes:
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

    djnz @twelve.notes

    inc c
    ld a,c
    cp 4

    jr c,@three.octaves

    ret

length: equ $-demo.setup

;------------------------------------------------------------------------------
mod.header.len: equ 1084
mod.header: defs mod.header.len ; first 1084 bytes of mod containing song name + sample info

;   20 bytes module title
;  930 bytes for sample (31 or 15) info:
;       22 bytes sample name
;        2 bytes sample length in big-endian words
;        1 byte finetune value [-8..7]
;        1 byte volume [0..64]]
;        2 bytes repeat offset in big-endian words (must be > 1)
;        2 bytes repeat length in big-endian words (must be > 1)
;       --
;       30 bytes * 31 = 930
;    1 byte song positions
;    1 byte unused
;  128 bytes pattern table
;    4 bytes signature "M.K." / "FLT4" / "M!K!"
; ----
; 1084

;------------------------------------------------------------------------------
    defs align 256

char.list:

; addresses to print routines for 0-9 A-F G - #

    defs 19 * 2

;------------------------------------------------------------------------------
line.table:

; contains screen address per row in 32 row mode (all but pattern screen)

    defs 32*2

;------------------------------------------------------------------------------
; for three octave mods the low byte can identify the note, each entry is two bytes

period.note.notes:

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

    defw 0x358  ; octave 1
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
    defw 0x1ac  ; octave 2
    defw 0x194
    defw 0x17d
    defw 0x168
    defw 0x153
    defw 0x140
    defw 0x12E
    defw 0x11d
    defw 0x10d
    defw 0xfe
    defw 0xf0
    defw 0xe2
    defw 0xd6   ; octave 3
    defw 0xca
    defw 0xbe
    defw 0xb4
    defw 0xaa
    defw 0xa0
    defw 0x97
    defw 0x8f
    defw 0x87
    defw 0x7f
    defw 0x78
    defw 0x71

    defs align 0x100

period.note.table:

     defs 0x200
;------------------------------------------------------------------------------

build.font:
