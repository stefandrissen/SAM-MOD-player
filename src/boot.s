; SAM MOD player

; (C) 2019-2021 Stefan Drissen

; boot file to load the rest

;---------------------------------------------------------------

    include "memory.i"
    include "constants/opcodes.i"
    include "ports/internal.i"
    include "constants/dos.i"
    include "constants/rom.i"

;---------------------------------------------------------------

    org 0x8000

    autoexec

;---------------------------------------------------------------

    jp @boot

;---------------------------------------------------------------
@file.loading.scr:  defm "loading.$    "
@file.tracker:      defm "tracker      "
@file.burstplayer:  defm "burstplayer  "
@file.loader:       defm "loader       "
@file.demo:         defm "demo         "

@palette:   defb 0x7f,0x00,0x6b,0x63,0x2c,0x24,0x77,0x78,0x70,0x2a,0x22,0x28,0x20,0x7d,0x5f,0x6a

@boot:

    di

    call @set.palette
    call @detect.memory

    ld a,page.burstplayer
    call @fix.page
    or video.mode.4
    out (port.vmpr),a
    ld (svar.cuscrnp),a
    and video.memory.page.mask

    ld hl,@file.loading.scr
    call @load.file

    ld a,page.tracker
    ld hl,@file.tracker
    call @load.file

    ld a,page.create.burstplayer
    ld hl,@file.burstplayer
    call @load.file

    call @set.burstplayer.page

    ld a,page.loader
    ld hl,@file.loader
    call @load.file

    ld a,page.loader
    ld de,0x8000 + 0x2000
    ld hl,@file.demo
    call @load.file.address

    ld a,dvar.version
    call @get.dvar
    ld e,a
    push de
    ld a,dvar.drive_2.tracks
    call @get.dvar
    pop de
    ld d,a

    ld a,page.loader - 1
    call @fix.page
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
@set.burstplayer.page:

    in a,(port.lmpr)
    ld c,a

    ld a,page.burstplayer
    call @fix.page
    ld b,a

    ld a,page.create.burstplayer
    call @fix.page
    or low.memory.ram.0
    out (port.lmpr),a

    ld a,b
    ld (burstplayer.page-0x8000),a

    ld a,c
    out (port.lmpr),a
    ret

;------------------------------------------------------------------------------
@set.palette:

; set palette (hard coded from loading.$)

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

    ret

;------------------------------------------------------------------------------
@detect.memory:

    in a,(port.lmpr)
    ld c,a

    ld b,high.memory.page.mask.256k

    ld a,low.memory.ram.0 + 0x10
    out (port.lmpr),a

    ld hl,0x0000
    xor a
    ld (hl),a
    cp (hl)
    jr nz,@not.512k
    dec a
    ld (hl),a
    cp (hl)
    jr nz,@not.512k

    ld b,high.memory.page.mask

@not.512k:

    ld a,c
    out (port.lmpr),a

    ld a,b
    ld (@fix.page+1),a

    ret

;------------------------------------------------------------------------------
@fix.page:

    and 0
    ret

;------------------------------------------------------------------------------
@load.file:

;   hl -> file name
;   a  =  page
;   de =  address (when load.file.address - must be >= 0x8000)

;------------------------------------------------------------------------------

    ld de,0x8000

@load.file.address:

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
    call @fix.page

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

;------------------------------------------------------------------------------
@get.dvar:

; use floating point calculator to get address of dvar and then get value
;
; input
; - a = dvar
; output
; - a = dvar value

    ld (@smc.dvar),a

    rst fpc
        defb fpc.onelit
@smc.dvar:
        defb 0
        defb fpc.dvar
        defb fpc.fivelit
            defb 0          ; special form
            defb 0          ; positive
            defw 0x4000
            defb 0
        defb fpc.mod        ; dvar mod 16384 (to allow simple jgetint)
        defb fpc.exit

    call rom.jgetint        ; dvar -> hl

    in a,(port.lmpr)
    ld b,a
    ld a,(svar.dosflg)
    or low.memory.ram.0
    out (port.lmpr),a

    ld c,(hl)

    ld a,b
    out (port.lmpr),a

    ld a,c

    ret

;------------------------------------------------------------------------------
