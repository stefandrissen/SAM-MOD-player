; device selection screen
;
; (C) 2021 Stefan Drissen
;
; after selection, memory can be reclaimed

;---------------------------------------------------------------

device.device:          defb device.samdac  ; [0-5]

    device.saa:             equ 0
    device.samdac:          equ 1
    device.dac:             equ 2
    device.bluealpha:       equ 3
    device.quazar:          equ 4
    device.clut:            equ 5

device.device.port:     defb 0 ; [0-1]
device.speed:           defb 0 ; [0-1]

    speed.pal:              equ 0
    speed.ntsc:             equ 1

;---------------------------------------------------------------
@fix.page:

    and high.memory.page.mask.256k
    ret

;---------------------------------------------------------------
device.start:
;---------------------------------------------------------------

    di

    call cls

    call @check.memory
    call @init.screen

@loop:

    call @select.device
    call @select.speed

    jr z,@-loop

    call cls

    call @create.burstplayer

    jp loader

;---------------------------------------------------------------
@check.memory:

    ld hl,@relocate.scan
    ld de,inst.buffer
    ld bc,@scan.memory.len
    ldir

    call @scan.memory
    ld (loader.ram),a

    bit 1,a
    call nz,@set.memory.512k
    and %11100
    call nz,@set.memory.mb

    ret

;---------------------------------------------------------------

@relocate.scan:

    org inst.buffer

@scan.memory:

    in a,(port.hmpr)
    ld c,a

    ld hl,0x8000
    ld b,0

    ld a,high.memory.external
    out (port.hmpr),a

@scan.megs:

    ld a,b
    out (port.xmpr.c),a

    xor a
    ld (hl),a
    cp (hl)
    jr nz,@not.ram
    dec a
    ld (hl),a
    cp (hl)
    jr nz,@not.ram

    ld a,b
    add 0x40
    ld b,a
    jr nc,@scan.megs

    ld b,1

@not.ram:

    ld a,b
    rlca
    rlca    ; [0-4]
    rlca
    rlca
    ld b,a  ; %XXX00

    inc b   ; 256K

    ld a,0x10
    out (port.hmpr),a
    xor a
    ld (hl),a
    cp (hl)
    jr nz,@not.512k
    dec a
    ld (hl),a
    cp (hl)
    jr nz,@not.512k

    inc b   ; 512K

@not.512k:

    ld a,c
    out (port.hmpr),a

    ld a,b

    ret

@scan.memory.len: equ $ - @scan.memory

    org @relocate.scan + @scan.memory.len

;---------------------------------------------------------------
@set.memory.512k:

    ld hl,device.screen.memory
    ld (hl),"5"
    inc hl
    ld (hl),"1"
    inc hl
    ld (hl),"2"

    ld hl,@fix.page+1
    ld (hl),high.memory.page.mask

    ret

;---------------------------------------------------------------
@set.memory.mb:

    ld hl,device.screen.memory.mb
    ld de,device.screen.memory
    rrca
    rrca
    add a,"0"
    ld (de),a
    inc de
    ld bc,device.screen.memory.mb.len
    ldir

    ret

;---------------------------------------------------------------
@init.screen:

    in a,(port.hmpr)
    and high.memory.page.mask & %11111110
    or video.mode.2
    out (port.vmpr),a

    ld (svar.cuscrnp),a
    ld a,1
    ld (svar.mode),a

    ld de,device.screen
    ld ix,device.attributes
    call print.screen

    ld de,load.screen
    call print.screen.no.attr

    ld b,row.device
    call cursor.init

    ld a,(device.device)
    call cursor.print.first

    ld b,row.speed
    call cursor.init

    ld a,(device.speed)
    call row.a.to.line
    ld (var.cursor.previous.row),a
    xor a
    ld (var.cursor.blink.timer),a
    call cursor.print

    ret


;---------------------------------------------------------------
@select.device:

    ld b,row.device
    ld a,5                  ; max device
    call cursor.init

    ld a,(device.device)
    call cursor.print.first

@loop:

    call cursor.select

    ld a,c

    cp device.samdac
    jr c,@no.port
    cp device.dac + 1
    jr nc,@no.port

    push bc
    call scan.keyboard.left.right
    pop bc
    jr z,@not.pressed

    ld a,(device.device.port)
    xor 1
    ld (device.device.port),a

@not.pressed:

    ld a,c

@no.port:

    call print.device.details

    call scan.keyboard.return
    jr z,@-loop

    ld a,(device.device)
    jp cursor.print.first

;---------------------------------------------------------------
@select.speed:

    ld b,row.speed
    ld a,1              ; max
    call cursor.init

    ld a,(device.speed)
    call cursor.print.first

    ld a,-1
    ld (device.speed),a

@loop:
    call cursor.select

    ld a,c
    call speed.details

    call scan.keyboard.return
    jr nz,@leave

    call scan.escape
    jr z,@-loop

    xor a
@leave:
    push af
    ld a,(device.speed)
    call cursor.print.first
    pop af

    ret

;---------------------------------------------------------------
; map new loader screen to burstplayer device numbers - fix burstplayer later on

@device.mapping:
    defb 1  ; sound chip
    defb 2  ; samdac
    defb 4  ; dac
    defb 6  ; blue alpha
    defb 7  ; quazar
    defb 0  ; screen

;---------------------------------------------------------------
@create.burstplayer:

    ld hl,@relocate.call.burstplayer.create
    ld de,inst.buffer
    ld bc,@call.burstplayer.create.len
    ldir

    ld a,(device.device)
    ld hl,@device.mapping
    add a,l
    ld l,a
    jr nc,@nc
    inc h
@nc:
    ld a,(hl)
    ld hl,device.device.port
    add a,(hl)

    ld (@device.device+1),a
    ld a,(device.speed)
    ld (@device.speed+1),a
    ld a,(loader.ram)
    ld (@loader.ram+1),a

    call @call.burstplayer.create

    ret

@relocate.call.burstplayer.create:

    org inst.buffer

@call.burstplayer.create:

    in a,(port.hmpr)
    ld (@store.hmpr+1),a

    ld a,page.create.burstplayer
    call @fix.page
    out (port.hmpr),a

@device.device:
    ld a,0
    ld (burstplayer.device),a
@device.speed:
    ld a,0
    ld (burstplayer.amiga),a
@loader.ram:
    ld a,0
    ld (burstplayer.ram),a

    call burstplayer.create

@store.hmpr:
    ld a,0
    out (port.hmpr),a

    ret

@call.burstplayer.create.len: equ $ - @call.burstplayer.create

    org @relocate.call.burstplayer.create + @call.burstplayer.create.len

;---------------------------------------------------------------

print.device.details:

    push af
    ld hl,device.details
    add a
    ld e,a
    ld d,0
    add hl,de
    ld a,(hl)
    add "0"
    push hl
    ld hl,video.memory.32.rows * 30 + video.memory.high
    call print.chr
    ld de,text.bits
    ld b,7
    call print.de.b
    pop hl
    inc hl
    ld a,(hl)
    add a
    ld e,a
    ld d,0
    ld hl,device.texts
    add hl,de
    ld e,(hl)
    inc hl
    ld d,(hl)
    ld b,32 - 8
    ld hl,video.memory.32.rows * 30 + 8 + video.memory.high
    call print.de.b

    pop af
    push af

    ld hl,video.memory.32.rows * 30 + 26 + video.memory.high
    ld b,6
    ld de,text.blank

    cp device.samdac
    jr c,@no.port
    cp device.dac+1
    jr nc,@no.port

    ld de,text.port

    ld a,(device.device.port)
    add "1"
    ld (text.port+5),a
@no.port:
    call print.de.b

    pop af

@same:

    ld (device.device),a
    ret

text.bits:
    defm " bits, "
    defb 0

text.port:
    defm "port x"
text.blank:
    defb 0



;---------------------------------------------------------------
speed.details:

    ld hl,device.speed
    cp (hl)
    ret z

    ld (hl),a

print.speed.details:

    push af

    ld hl,text.speed
    add a,a
    add a,l
    ld l,a
    jr nc,@nc
    inc h
@nc:
    ld e,(hl)
    inc hl
    ld d,(hl)

    ld b,32
    ld hl,video.memory.32.rows * 30 + video.memory.high
    call print.de.b

    pop af

    ret

text.speed:         defw text.speed.pal,text.speed.ntsc

text.speed.pal:     defm "7.0937892 MHz"
                    defb 0
text.speed.ntsc:    defm "7.1590905 MHz"
                    defb 0


;---------------------------------------------------------------


device.screen:
    defb 0,3
row.device: equ 5
    defm "SOUND DEVICE"
    defb 0,2
    defm " Sound chip"
    defb 0
    defm " SAMdac"
    defb 0
    defm " DAC"
    defb 0
    defm " Blue Alpha Sampler"
    defb 0
    defm " Quazar Soundcard"
    defb 0
    defm " Screen"
    defb 0,2
row.speed:  equ 14
    defm "AMIGA SPEED"
    defb 0,2
    defm " PAL"
    defb 0
    defm " NTSC"
    defb 0,2
    defm "Memory: "
device.screen.memory:
    defm "256 KB"
    defb 0,0

device.screen.memory.mb:
    defm " MB  "
    device.screen.memory.mb.len: equ $ - device.screen.memory.mb

device.attributes:
    defb 3,colour.orange
    defb 2,colour.purple
    defb 6,colour.blue
    defb 2,colour.purple
    defb 3,colour.blue
    defb 2,colour.green
    defb 13,colour.green
    defb 1,colour.yellow

device.details:
    defb 3, stereo
    defb 7, stereo
    defb 6, mono
    defb 6, mono
    defb 8, surround
    defb 7, visual

    visual:     equ 0
    mono:       equ 1
    stereo:     equ 2
    surround:   equ 3

device.texts:   defw text.visual,text.mono,text.stereo,text.surround

text.visual:
    defm "visual"
    defb 0
text.mono:
    defm "mono"
    defb 0
text.stereo:
    defm "stereo"
    defb 0
text.surround:
    defm "surround"
    defb 0

