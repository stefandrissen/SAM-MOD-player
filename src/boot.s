; SAM MOD player

; (C) 2019-2021 Stefan Drissen

; boot file to load the rest

;---------------------------------------------------------------

    include "memory.i"
    include "opcodes.i"
    include "ports/internal.i"
    include "dos.i"

;---------------------------------------------------------------

    org 0x8000

    autoexec

;---------------------------------------------------------------

    jp @boot

;---------------------------------------------------------------

file.loading.scr:   defm "loading.$    "
file.sequencer:     defm "sequencer    "
file.burstplayer:   defm "burstplayer  "
file.loader:        defm "loader       "
file.demo:          defm "demo         "

@palette:   defb 0x7f,0x00,0x6b,0x63,0x2c,0x24,0x77,0x78,0x70,0x2a,0x22,0x28,0x20,0x7d,0x5f,0x6a

@boot:

; first set palette (hard coded from loading.$)

    ld hl,@palette + 0x0f
    ld c,port.color_look_up_table
    ld b,0x10
    otdr

    ld hl,@palette
    ld de,palette.table
    ld bc,0x10
    ldir

    ld hl,@palette
    ld de,palette.table + 0x14
    ld bc,0x10
    ldir

    ld a,page.burstplayer + video.mode.4
    out (port.vmpr),a
    ld (svar.cuscrnp),a
    and video.memory.page.mask

    ld hl,file.loading.scr
    call load.file

    ld a,page.sequencer
    ld hl,file.sequencer
    call load.file

    ld a,page.create.burstplayer
    ld hl,file.burstplayer
    call load.file

    ld a,page.loader
    ld hl,file.loader
    call load.file

    ld a,page.loader
    ld de,0x8000 + 0x2000
    ld hl,file.demo
    call load.file.address

    ld a,page.loader - 1
    ld hl,0xc000
    jp inst.buffer.jump_ahl

@exit:

    ret

    ld bc,port.border
    ld a,1
    di
@loop:
    out (c),a
    out (c),b
    jr @loop

    ret

;------------------------------------------------------------------------------
load.file:

;   hl -> file name
;   a  =  page
;   de =  address (when load.file.address - must be >= 0x8000)

;------------------------------------------------------------------------------

    ld de,0x8000

load.file.address:

    push af
    push de
    push hl

    ld hl,dos.exit.routine
    ld (svar.doser),hl

    ld hl,uifa
    ld de,uifa+1
    ld (hl),0
    ld bc,0x30-1
    ldir

    ld hl,@load.relocate
    ld de,inst.buffer
    ld bc,@load.len
    ldir

    pop hl

    ld de,uifa+1
    ld bc,13
    ldir
    ld ix,uifa
    ld (ix),uifa.filetype.code

    ld hl,dos.exit.routine
    ld (svar.doser),hl

    rst 8
    defb dos.hgthd

    pop de

    ld a,(difa.length.pages)
    ld c,a
    ld hl,(difa.length.bytes)
    res 7,h
    ex de,hl

    pop af

    jp inst.buffer.load

; the loader code is copied to inst.buffer which resides in AB allowing CD to be paged in

@load.relocate:

    org inst.buffer

inst.buffer.load:

    push af
    in a,(port.hmpr)
    ld (@store.hmpr+1),a
    pop af

    out (port.hmpr),a

    rst 8
    defb dos.hload

@store.hmpr:
    ld a,0
    out (port.hmpr),a

    ret

dos.exit.routine:

    ld hl,0
    ld (svar.doser),hl

    or a
    jr nz,@dos.error

    ret

@dos.error:
    di
    ld bc,port.border

@loop:
    out (c),a
    out (c),b

    jr @loop


inst.buffer.jump_ahl:

    out (port.hmpr),a
    jp (hl)


@load.len:  equ $ - inst.buffer.load

    org @load.relocate + @load.len

