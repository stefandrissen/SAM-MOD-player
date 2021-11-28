;SAM MOD player - DOS loader

 ;(C) 1996-2021 Stefan Drissen

 ; to do:
 ; - handle dos errors better (doser)
 ; + add sample runways while loading

; constants
    include "memory.i"
    include "ports/internal.i"
    include "ports/megabyte.i"
    include "ports/keyboard.i"
    include "ports/fdc.i"
    include "constants/dos.i"
    include "constants/mod.i"
    include "constants/opcodes.i"

load.offs:  equ 0x8000

    org 0xc000

    ld a,e
    ld (loader.dos.version),a
    ld a,d
    ld (loader.drive_2.tracks),a

    jp device.start

;---------------------------------------------------------------
; loader.variables
    loader.ram:             defb 0 ; %XXXRR (RAM / 256K)

    loader.dos.version:     defb 0 ; (dvar 7)
    loader.drive_2.tracks:  defb 0 ; (dvar 2)
    loader.drive:           defb 0 ; [1-2]
    loader.record:          defw 0
    loader.device:          defb 0 ; [0-5]
;---------------------------------------------------------------
loader.font_high:

    mdat "../res/font.bin"

;---------------------------------------------------------------
cursor.init:

    ; input
    ; - a = max value
    ; - b = row

    ld (max.select+1),a

    ld hl,video.memory.high - video.memory.32.rows - video.memory.bytes.per.row
    ld de,video.memory.32.rows
    inc b
    @loop:
        add hl,de
        djnz @-loop

    ld (@cursor.offset+1),hl

    ret

;---------------------------------------------------------------
row.a.to.line:

    ; input:
    ; - a = row [0x00-0x1f]

    ld c,a

@row.to.line:

    ; input:
    ; - c = row [0x00-0x1f]

    ; output
    ; - c = line [0-x00-0xbf]

    ld a,c
    add a,a
    add a,c
    add a,a
    ld c,a

    ret

;---------------------------------------------------------------
cursor.select:

    ; selection routine

    ; input:
    ; - c = current row
    ; - min selection = 0
    ; - max selection = (max.selec+1)

    push bc

    call @row.to.line
    call cursor.print
    pop bc

    ld a,keyboard.cursors_cntrl
    in a,(port.keyboard)
    bit 1,a
    jr z,@cursor.move.up
    bit 2,a
    jr z,@cursor.move.down

    ld a,keyboard.67890
    in a,(port.keyboard)
    bit 1,a
    jr z,@cursor.move.up
    bit 2,a
    jr z,@cursor.move.down

    ret

;---------------------------------------------------------------
@cursor.move.up:

    ld a,c
    or a
    ret z
    push bc

    call @row.to.line

    ld b,6
    @loop:
        dec c
        call cursor.print

        djnz @-loop

    pop bc
    dec c

    ret

;---------------------------------------------------------------
@cursor.move.down:

    max.select:
    ld a,6
    or a
    ret z

    dec a
    cp c
    ret c

    push bc

    call @row.to.line

    ld b,6
    @loop:
        inc c
        call cursor.print
        djnz @-loop
    pop bc
    inc c

    ret

;---------------------------------------------------------------
scan.escape:
    ld a,keyboard.caps_tab_esc
    in a,(port.status)
    and %00100000
    jr z,@still.esc

    xor a
    ret

    @still.esc:
        ld a,keyboard.caps_tab_esc
        in a,(port.status)
        and %00100000
        jr z,@still.esc

        ret

;---------------------------------------------------------------
scan.keyboard.return:

    ld a,keyboard.hjkl_return
    in a,(port.keyboard)
    and %00001
    jr z,@still.return

    ld a,keyboard.67890
    in a,(port.keyboard)
    and %00001
    jr z,@still.0

    @leave:
        xor a
        ret

    @still.return:
        ld a,keyboard.hjkl_return
        in a,(port.keyboard)
        and %00001
        jr z,@still.return

        ret

    @still.0:
        ld a,keyboard.67890
        in a,(port.keyboard)
        and %00001
        jr z,@still.0

        ret

;---------------------------------------------------------------
@scan.keyboard.left.right.shifted:

    ld c,1

    ld a,keyboard.vcxz_shift
    in a,(port.keyboard)
    bit 0,a
    jr nz,@not.shift
    ld c,10
    jr scan.keyboard.left.right

    @not.shift:
        ld a,keyboard.bnm_symbol_space
        in a,(port.keyboard)
        bit 1,a
        jr nz,scan.keyboard.left.right

        ld c,50

    scan.keyboard.left.right:

        ld a,keyboard.cursors_cntrl
        in a,(port.keyboard)
        bit 3,a
        jr z,@still.cursor.left
        bit 4,a
        jr z,@still.cursor.right

        ld a,keyboard.67890
        in a,(port.keyboard)
        bit 4,a
        jr z,@still.6
        bit 3,a
        jr z,@still.7

        xor a
        ret

    @still.cursor.left:
        set 7,c
        ld a,keyboard.cursors_cntrl
        in a,(port.keyboard)
        bit 3,a
        jr z,@still.cursor.left

        ret

    @still.cursor.right:
        ld a,keyboard.cursors_cntrl
        in a,(port.keyboard)
        bit 4,a
        jr z,@still.cursor.right

        ret

    @still.6:
        set 7,c
        ld a,keyboard.67890
        in a,(port.keyboard)
        bit 4,a
        jr z,@still.6

        ret

    @still.7:
        ld a,keyboard.67890
        in a,(port.keyboard)
        bit 3,a
        jr z,@still.7

        ret

;===============================================================
loader.palette:
    ;     GRB!grb
    defb %0000000 ;    0

    defb %0011101 ;3 1 1;BLUE+green
    defb %1011001 ;3 2 2
    defb %1011101 ;3 3 3

    defb %0101110 ;3 1 4;RED+green
    defb %1101010 ;3 2 5
    defb %1101110 ;3 3 6

    defb %1001101 ;3 1 7;GREEN+blue

    defb %0000000 ;    8;bright background

    defb %1011100 ;3 2 9
    ; defb %1011101 ;3 3 same as pen 3

    defb %0101011 ;3 1 A;RED+blue
    defb %0111010 ;3 2 B
    defb %0111011 ;3 3 C

    defb %1001110 ;3 1 D;GREEN+red
    defb %1101100 ;3 2 E
    ; defb %1101110 ;3 3  ;same as pen 6

    defb %1110111 ;    F

;---------------------------------------------------------------
@show.screen:

    ; print header / footer

    call cls

    ld de,load.screen
    ld ix,@load.attributes
    call print.screen

    ret

;---------------------------------------------------------------
@loader.init:

    ld hl,mes.no_disc
    ld de,m.vollabel
    ld bc,@mes.label.len
    ldir

    ld a,1
    ld (msdos+1),a

    xor a
    ld (nodisc+1),a

    ret

;---------------------------------------------------------------
@check.drives:

    ; output:
    ; - de = address next file entry

    ld hl,option.dir
    ld de,loader.dir
    ld bc,load.len
    ldir

    ld a,(loader.dos.version)
    cp dvar.version.b_dos.max + 1
    jr c,@is.bdos

    ld a,(loader.drive_2.tracks)
    or a
    ld a,1
    jr z,@no.drive.2

    ld bc,load.len
    ldir
    ld a,2

    @no.drive.2:

        ld (loader.entries),a

        ld a,(loader.drive)
        or a
        ret nz

        ld a,1
        ld (loader.drive),a
        ret

    @is.bdos:

        ld a,dvar.records
        call bdos.get.dvar.word
        ld a,h
        or l
        ld a,1
        jr z,@no.drive.2

        ld a,2
        ld (loader.entries),a

        ld hl,record.dir
        ld bc,load.len
        ldir

        push de
        call @display.record.number
        pop de

        ld a,(loader.drive)
        or a
        ret nz

        xor a
        ld (msdos+1),a

        ld a,2
        ld (loader.drive),a

        ret

;---------------------------------------------------------------
@display.record.number:

    ld a,dvar.record
    call bdos.get.dvar.word

    ld de,loader.dir + load.len + 15
    jp text.hl.decimal

;---------------------------------------------------------------
loader:

    call @loader.init
    call @show.screen
    call @check.drives

    push de
    call fat.read.dir
    pop de

    nodisc:
        ld a,0  ; set by errnodisc
        or a
        jr nz,@select.file

    push de
    ld ix,black.attributes
    ld a,6
    call set.attributes
    pop de

    msdos:
        ld a,0
        or a
        push af
        call z,read.directory.bdos
        pop af
        call nz,read.directory.fat

    @select.file:

        call @show.screen
        call @print.octave
        call @print.drive.number
        call @print.drive.label
        call @print.files

        call @still.esc ; to prevent immediate exit when escape used to exit "DEMO"

        ld b,4                  ; row
        ld a,(loader.entries)
        dec a                   ; max
        call cursor.init

        ld a,(loader.drive)
        dec a
        ld c,a
        call cursor.print

    cursor.lp:

    call get.entry.ix.from.c
    ld a,(ix+28)
    res 6,a
    ld de,mes.noisetracker
    or a
    jr z,got.mes
    ld de,mes.protracker
    dec a
    jr z,got.mes
    ld de,mes.startrekker
    dec a
    jr z,got.mes
    ld de,mes.protracker    ; m!k!
    dec a
    jr z,got.mes
    ld de,mes.drv
 got.mes:
    ld a,(ix+28)
    bit 7,a
    jp nz,disc.mess     ; new disc message
    push ix
    push de
    pop ix
    ld (ix+26),"8"
    bit 6,a
    jr z,gm.is.8
    ld (ix+26),"4"
 gm.is.8:
    pop ix
    push bc             ; c = select position
    push de
    ld a,e
    add 14
    ld e,a
    jr nc,$+3
    inc d
    ld a,(ix+38)
    call cnv.a.to.de
    pop de

    ld b,32
    ld hl,video.memory.32.rows * 29 + video.memory.high
    call print.de.b

    ld hl,mes.size
    ld a,(ix+29)        ; length in patterns
    ld b,"0"
    cp 100
    jr c,gm.len.100
    inc b
    sub 100
 gm.len.100:
    ld (hl),b
    inc hl
    ex de,hl
    call cnv.a.to.de

    ld l,(ix+36)
    ld h,(ix+37)

    ld bc,100
    ld a,"0"-1
    or a
 gm.get.big:
    sbc hl,bc
    inc a
    jr nc,gm.get.big
    add hl,bc
    ld de,mes.size+18
    ld (de),a
    inc de
    ld a,l
    call cnv.a.to.de

    ld de,mes.size+24
    ld a,(ix+30)
    cp "*"
    jr nz,gm.is.date
    ld hl,mes.no_date
    ld bc,8
    ldir
    jr gm.got.date

 gm.is.date:
    ex de,hl
    ld (hl),a
    inc hl
    ld a,(ix+31)
    ld (hl),a
    inc hl
    ld (hl),"-"
    inc hl
    ld a,(ix+32)
    ld (hl),a
    inc hl
    ld a,(ix+33)
    ld (hl),a
    inc hl
    ld (hl),"-"
    inc hl
    ld a,(ix+34)
    ld (hl),a
    inc hl
    ld a,(ix+35)
    ld (hl),a

 gm.got.date:

    ld de,mes.size
    pop bc
    jr normal.mess

 disc.mess:
    ld hl,video.memory.32.rows * 29 + video.memory.high
    ld b,32
    @loop:
        ld a," "
        call print.chr
        djnz @-loop

 normal.mess:
    ld b,32
    ld hl,video.memory.32.rows * 30 + video.memory.high
    call print.de.b

    call cursor.select

    ld a,keyboard.caps_tab_esc
    in a,(port.status)
    bit 5,a
    jp z,loader.quit

    call scan.keyboard.return
    jp nz,select.key

    ld a,keyboard.yuiop
    in a,(port.keyboard)
    and %00010
    ld a,opcode.nop
    jr nz,@not.o
 @still.o:
    scf
    jr c,@not.o.nc
    ld a,(loader.octaves+1)
    xor %110
    ld (loader.octaves+1),a
    call @print.octave
    ld a,opcode.scf
 @not.o:
    ld (@still.o),a
 @not.o.nc:

    ld a,(ix+28)
    cp 0x81     ; drive 2
    jr nz,@not.drive_2

    push bc
    call @scan.keyboard.left.right.shifted
    jr nz,@record.up.down
    pop bc

 @not.drive_2:
    jp cursor.lp

 @record.up.down:
    ld b,0
    bit 7,c
    jr z,@record.up

 @record.down:

    ld a,dvar.record
    call bdos.get.dvar.word

    res 7,c
    or a
    sbc hl,bc
    jr nc,@change.record
    ld hl,1
    jr @change.record

 @record.up:

    ld a,dvar.record
    call bdos.get.dvar.word

    add hl,bc
    ex de,hl

    ld a,dvar.records
    call bdos.get.dvar.word

    or a
    sbc hl,de
    jr nc,@not.last.record

    add hl,de
    ex de,hl

 @not.last.record:
    ex de,hl

 @change.record:

    call bdos.select.record.hl
    pop bc                      ; toss bc
    ld c,1
    jp select.key

;---------------------------------------------------------------
@print.octave:

    ld hl,video.memory.32.rows * 1 + 26 + video.memory.high
    ld de,mes.oct
    ld b,5
    call print.de.b

    ld a,(loader.octaves+1)
    add "0"
    call print.chr

    ret

;---------------------------------------------------------------
@print.drive.number:

    ld a,(loader.drive)
    add a,"0"
    ld (mes.drive+6),a

    ret

;---------------------------------------------------------------
@set.drive.label:

    ; sets drive label (from m.vollabel) or no label if empty

    ld hl,m.vollabel
    ld de,mes.label
    ld bc,@mes.label.len

    ld a,(hl)
    or a
    jr nz,@has.label

        ld hl,mes.no_label

    @has.label:

    ldir

    ret

;---------------------------------------------------------------
@print.drive.label:

    call @set.drive.label

    ld hl,video.memory.32.rows * 3 + video.memory.high
    ld de,mes.drive
    ld b,9 + @mes.label.len
    call print.de.b

    ret

;---------------------------------------------------------------
@print.files:

    ld a,(loader.entries)
    cp 24
    jr c,@max.24
        ld a,24
    @max.24:
    ld c,a
    ld de,loader.dir
    ld hl,video.memory.32.rows * 4 + 1 + video.memory.high

    @loop:
        push de
        push hl

        ld b,8
        call print.de.b ; file name
        inc l

        ld b,20
        call print.de.b ; module name

        pop hl

        ld de,video.memory.32.rows
        add hl,de

        pop de
        ld a,e
        add load.len
        ld e,a
        jr nc,$+3
        inc d

        dec c
        jr nz,@-loop

    ret

;---------------------------------------------------------------
loader.quit:

    xor a
    out (port.lmpr),a
    rst 0

;---------------------------------------------------------------
file.check:

    ;check to see if the sum of sample lengths + patterns = file len

    ; input:
    ; a = 1 (compressed) or 2 (normal)

    ld hl,temp.spc + 9
    ld bc,mod.title.len
    ldir

    ld (bytes.per+1),a

    push ix
    ld bc,(temp.spc + 9 + mod.pt.id)

    ld a,1      ; 1 = protracker
    or a
    ld hl,"M" + "." * 0x100
    sbc hl,bc

    jr z,@got.module.type

    inc a       ; 2 = startrekker
    or a
    ld hl,"F" + "L" * 0x100
    sbc hl,bc

    jr z,@got.module.type

    inc a       ; 3 = protracker extended
    or a
    ld hl,"M" + "!" * 0x100
    sbc hl,bc

    jr z,@got.module.type

    xor a       ; 0 = noisetracker

 @got.module.type:

    ld (de),a
    ld a,(bytes.per+1)
    dec a
    ld a,(de)
    jr nz,@not.compressed

    set 6,a                 ; compressed 4 bit
    ld (de),a

  @not.compressed:

    inc de
    ld hl, temp.spc + 9 + mod.pt.song.positions
    and 63
    jr nz,@not.noisetracker

    ld hl, temp.spc + 9 + mod.nt.song.positions

 @not.noisetracker:

    ldi
    push de

    ld bc,0x1f00                    ; b = 31 samples
    ld hl,mod.pt.pattern
    and 63
    jr nz,@not.noisetracker

    ld b,0x0f                       ; b = 15 samples
    ld hl,mod.nt.pattern

 @not.noisetracker:

    xor a
    ld (sample.count+1),a
    ld ix,temp.spc + 9 + mod.samples

 @loop.add_all_samples:

    ld d,(ix+mod.sample.len.words+0)
    ld e,(ix+mod.sample.len.words+1)
  bytes.per:
    ld a,2
    @times.sample:
        add hl,de
        jr nc,$+3
        inc c
        dec a
        jr nz,@times.sample

    ld a,d
    or a
    jr nz,@is.sample    ; length >= 0x0100

    ld a,e
    cp 2
    jr c,@next.sample   ; length < 0x0002

  @is.sample:
    ld a,(sample.count+1)
    inc a
    ld (sample.count+1),a

  @next.sample:
    ld de,mod.sample.len
    add ix,de

    djnz @-loop.add_all_samples
 ; loop.add_all_samples

    inc ix
    inc ix
    ld b,128            ; length pattern table
    ld e,0

    @loop.get_highest_pattern:
        ld a,(ix)
        inc ix
        cp e
        jr c,$+3
        ld e,a
        djnz @-loop.get_highest_pattern

    inc e
    ld b,e
    ld de,mod.pattern.len

    @loop.add_all_patterns:
        add hl,de
        jr nc,$+3
        inc c
        djnz @-loop.add_all_patterns
        ;so now chl = calc. size

    ld a,(bytes.per+1)
    dec a
    jr z,fl.is.half

    ld a,h
    ld (file.len+1),a
    ld a,c
    ld (file.len+2),a
 fl.is.half:
    pop de
    pop ix

    ret

;---------------------------------------------------------------
insert.file.size:

 file.len:
    ld hl,0
    srl h
    rr l
    srl h
    rr l
    ex de,hl
    ld (hl),e   ; size in k
    inc hl
    ld (hl),d
    inc hl
 sample.count:
    ld a,0
    ld (hl),a

    ret

;---------------------------------------------------------------
fc.sam:
    push de

    ld de,(temp.spc + 1);length mod 16384
    ld a,(temp.spc + 7) ;length in pages (16384)
    and %00000011
    rrca
    rrca
    add d
    ld d,a
    ld a,(temp.spc + 7)
    jr nc,$+4
    add 4
    srl a
    srl a

    ex de,hl
 sm.resub:
    or a
    sbc hl,de
    sbc c
    jr nc,sm.got.maxmin
    adc c
    add hl,de
    ex de,hl
    ld b,a
    ld a,c
    ld c,b
    jr sm.resub

 sm.got.maxmin:      ; ahl=difference calc len & file len
    pop de
    or h
    ret


mes.load:   defm " Loading: "

;---------------------------------------------------------------
select.key:

    ; c = current record [0-25]

    call get.entry.ix.from.c

    ld a,(ix+28)            ;mod type ,+128=drive
    bit 7,a
    jp nz,new.read
    push af

    ld de,mes.load
    ld b,10
    ld hl,video.memory.32.rows * 31 + video.memory.high
    call print.de.b
    push ix
    pop de
    ld a,e
    add 8
    ld e,a
    jr nc,$+3
    inc d
    ld b,20
    call print.de.b
    xor a
    call print.chr
    xor a
    call print.chr

    push ix
    pop hl
    ld de,fat.parafile
    ld bc,8
    ldir
    ex de,hl
    ld (hl),"M"
    inc hl
    ld (hl),"O"
    inc hl
    ld (hl),"D"

    ld hl,fat.parafile
    call findfile
    ld a,(msdos+1)
    or a
    jr z,sam.load

;---------------------------------------------------------------

msdos.load: ; !!! does not work yet, needs to be moved to inst.buffer

    push hl
    pop ix
    ld e,(ix+26)
    ld d,(ix+27)

    ld a,(loader.ram)
    and %11100
    jr z,@+no.megabyte

    ld a,high.memory.external
    out (port.hmpr),a
    ld a,page.mod.megabyte
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a
    ld (@external+1),a

    jr @+continue

 @no.megabyte:

    ld a,page.mod
    out (port.hmpr),a

 @continue:

    ld hl,load.offs
 pc.load.all:
    call fat.read_cluster
    bit 6,h
    res 6,h
    jr z,@page.ok

    ld a,(loader.ram)
    and %11100
    jr z,@+no.megabyte

 @external:
    ld a,0
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a
    ld (@external+1),a

    jr @page.ok

 @no.megabyte:

    in a,(port.hmpr)
    inc a
    out (port.hmpr),a

 @page.ok:

    call fat.get_entry
    ld a,d
    cp 0x0f
    jr nz,pc.load.all
    ld a,e
    cp 0xf8
    jr c,pc.load.all

    jp file.loaded


;---------------------------------------------------------------
sam.load:

    ld d,0
 @loop.tracks:
    ld e,1
 @loop.sectors:
    ld hl,dos.sector
    call bdos.read.sector

    ld hl,dos.sector
    call sam.match

    ld hl,dos.sector + 0x100
    call sam.match

    inc e
    ld a,e
    cp 11
    jr nz,@-loop.sectors

    inc d
    ld a,d
    cp 4
    jr nz,@-loop.tracks

 file.notfound:
    jp loader

;---------------------------------------------------------------
sam.match:
    push ix
    ld a,(hl)
    cp uifa.filetype.code
    jr nz,sam.no.match

    ld b,8
 @compare.filename:
    ld a,(ix)
    inc ix
    inc l
    cp (hl)
    jr nz,sam.no.match
    djnz @-compare.filename

    ; inc l
    ; ld a,(hl)
    ; cp "."
    ; jr nz,sam.no.match
    ; inc l
    ; ld a,(hl)
    ; cp "m"
    ; jr nz,sam.no.match

    pop ix

    pop af          ; chuck return address

    ld a,l
    and 128
    or 13
    ld l,a
    ld d,(hl)       ; first track
    inc l
    ld e,(hl)       ; first sector

    push hl
    push de

    ld hl,relocate.load.mod
    ld bc,load.mod.len
    ld a,(loader.ram)
    and %11100
    jr z,@no.meg
    ld hl,relocate.meg.load.mod
    ld bc,meg.load.mod.len
 @no.meg:
    ld de,inst.buffer
    ldir
    pop de
    pop hl

    ld a,(loader.drive)

    call load.mod

    jp file.loaded

 sam.no.match:
    pop ix
    ret

;---------------------------------------------------------------
relocate.load.mod:

    org inst.buffer

 load.mod:

    ld (@+drive+1),a
    ld hl,dos.sector

    rst 8
    defb dos.hrsad
    di

    in a,(port.hmpr)
    ld (@+store.hmpr+1),a

    ld a,page.mod
    out (port.hmpr),a
    ld (@page+1),a

    ld hl,dos.sector + 9
    ld de,load.offs
    ld bc,510-9
    ldir
    ld d,(hl)
    inc l
    ld e,(hl)

    ld hl,load.offs+510-9

  @loop:

    push hl

    ld hl,dos.sector

   @drive:
    ld a,0

    rst 8
    defb dos.hrsad
    di

   @page:
    ld a,0
    out (port.hmpr),a

    pop de

    ld hl,dos.sector
    ld bc,510
    ldir

    push de
    ld d,(hl)
    inc hl
    ld e,(hl)
    pop hl

    ld a,e
    or d
    jr z,@+file.loaded
    bit 6,h
    jr z,@-loop

    res 6,h
    ld a,(@-page+1)
    inc a
    ld (@-page+1),a

    jr @-loop

  @file.loaded:
  @store.hmpr:
    ld a,0
    out (port.hmpr),a
    ret

 load.mod.len: equ $ - load.mod

    org relocate.load.mod + load.mod.len

;---------------------------------------------------------------
relocate.meg.load.mod:

    org inst.buffer

 meg.load.mod:

    ld (@+drive+1),a
    ld hl,dos.sector

    rst 8
    defb dos.hrsad
    di

    in a,(port.hmpr)
    ld (@+store.hmpr+1),a

    ld a,page.mod.megabyte
    ld (@external+1),a
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a
    ld a,high.memory.external
    out (port.hmpr),a

    ld hl,dos.sector + 9
    ld de,load.offs
    ld bc,510-9
    ldir
    ld d,(hl)
    inc l
    ld e,(hl)

    ld hl,load.offs+510-9

  @loop:

    ld a,(@+store.hmpr+1)
    out (port.hmpr),a

    push hl

    ld hl,dos.sector

   @drive:
    ld a,0

    rst 8
    defb dos.hrsad
    di

    ld a,high.memory.external
    out (port.hmpr),a
   @external:
    ld a,0
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a

    pop de

    ld hl,dos.sector
    ld bc,510
    ldir

    push de
    ld d,(hl)
    inc hl
    ld e,(hl)
    pop hl

    ld a,e
    or d
    jr z,@+file.loaded
    bit 6,h
    jr z,@-loop

    res 6,h
    ld a,(@external+1)
    inc a
    ld (@external+1),a

    jr @-loop

  @file.loaded:
  @store.hmpr:
    ld a,0
    out (port.hmpr),a
    ret

 meg.load.mod.len: equ $ - meg.load.mod

    org relocate.meg.load.mod + meg.load.mod.len

;---------------------------------------------------------------
file.loaded:

    pop af
    bit 6,a
    call nz,loader.unpack

    call cls

    in a,(port.lmpr)
    ld (@store.lmpr+1),a
    ld (@store.sp+1),sp

    ld sp,0x8000
    in a,(port.hmpr)
    and low.memory.page.mask
    or low.memory.ram.0
    out (port.lmpr),a

    ld a,(loader.ram)
    ld c,a

 loader.octaves:
    ld a,3                  ; 3 or 5 octave
    call demo.setup         ; aBcd

 @store.lmpr:
    ld a,0
    out (port.lmpr),a
 @store.sp:
    ld sp,0

    ld a,c
    dec a
    jp nz,@select.file

    jp loader.quit

;---------------------------------------------------------------
get.entry.ix.from.c:

 ; -> c
 ; <- ix

    ld ix,loader.dir-load.len
    ld de,load.len
    ld b,c
    inc b
    @loop:
        add ix,de
        djnz @loop

    ret

;---------------------------------------------------------------
new.read:

    and 0x7f
    or a
    push af
    call z,select.drive.1
    pop af
    call nz,select.drive.2

    jp loader

;---------------------------------------------------------------
var.cursor.blink.timer:     defb 0
var.cursor.previous.row:    defb 0  ; relative to offset

;---------------------------------------------------------------
cursor.print.first:

 ; input
 ; - a = position

    push af

    call row.a.to.line
    ld (var.cursor.previous.row),a

    xor a
    ld (var.cursor.blink.timer),a

    call cursor.print

    pop af

    ld c,a

    ret

;---------------------------------------------------------------
cursor.print:

    ; input
    ; - c = line

 @delay:
    in a,(port.status)
    and frame.interrupt
    jr nz,@delay

    ; clear cursor at previous position

    ld a,(var.cursor.previous.row)
    call @cursor.get.address

    xor a
    ld de,video.memory.bytes.per.row
    ld (hl),a
    add hl,de
    ld (hl),a
    add hl,de
    ld (hl),a
    add hl,de
    ld (hl),a
    add hl,de
    ld (hl),a

    ld hl,var.cursor.blink.timer
    inc (hl)
    bit 3,(hl)
    ret nz

    ld a,c
    ld (var.cursor.previous.row),a

    call @cursor.get.address

    ld ix, colours + ( colour.purple * 8 )
    ld de,video.memory.bytes.per.row

    ld (hl),%10000000
    ld a,(ix)
    inc ix
    set 5,h
    ld (hl),a

    res 5,h
    add hl,de
    ld (hl),%11000000
    ld a,(ix)
    inc ix
    set 5,h
    ld (hl),a

    res 5,h
    add hl,de
    ld (hl),%11100000
    ld a,(ix)
    inc ix
    set 5,h
    ld (hl),a

    res 5,h
    add hl,de
    ld (hl),%11000000
    ld a,(ix)
    inc ix
    set 5,h
    ld (hl),a

    res 5,h
    add hl,de
    ld (hl),%10000000
    ld a,(ix)
    inc ix
    set 5,h
    ld (hl),a

    ret

;---------------------------------------------------------------
@cursor.get.address:

 ; input
 ; - a = line

    ld de,video.memory.bytes.per.row
 @cursor.offset:
    ld hl,0                     ; -0x0020
    inc a
    @loop:
        add hl,de
        dec a
        jr nz,@-loop

    ret

;---------------------------------------------------------------
; messages
 mes.oct:            defm "Oct: "
 mes.drive:          defm "Drive 1: "

 mes.label:          defm "Solar Flare     "
 @mes.label.len:     equ $ - mes.label
 mes.no_label:       defm "No label        "
 mes.no_disc:        defm "No disc         "

 mes.noisetracker:   defm "Noisetracker, 15 samples, 8 bits"
 mes.protracker:     defm "Protracker,   31 samples, 8 bits"
 mes.startrekker:    defm "Startrekker,  31 samples, 8 bits"
 mes.drv:            defm "Press RETURN for new directory. "
 mes.size:           defm "127 song entries, 999k,         "
 mes.no_date:        defm "no date "

option.dir:
    defm " < 1: > "
    defm "new disc for drive 1"
    defb 128
    defb 0
    defm "xxxxxx"
    defw 0
    defb 0

    defm " < 2: > "
    defm "new disc for drive 2"
    defb 129
    defb 0
    defm "xxxxxx"
    defw 0
    defb 0

record.dir:
    defm " < 2: > "
    defm "record x            "
    defb 129
    defb 0
    defm "xxxxxx"
    defw 0
    defb 0

;---------------------------------------------------------------
text.hl.decimal:

 ;put decimal value of hl at address de

    ld bc,10000
    call divide.hl.bc
    ld (@var.buffer+0),a

    ld bc,1000
    call divide.hl.bc
    ld (@var.buffer+1),a

    ld bc,100
    call divide.hl.bc
    ld (@var.buffer+2),a

    ld bc,10
    call divide.hl.bc
    ld (@var.buffer+3),a

    ld a,l
    ld (@var.buffer+4),a

    ld hl,@var.buffer
    ld b,5
    @loop:
        ld a,(hl)
        or a
        jr nz,@digit

        inc hl

        djnz @-loop

        inc b
    @digit:
        add "0"
        ld (de),a
        inc de
        inc hl
        ld a,(hl)

        djnz @-digit

    ret

    @var.buffer: defs 5

;---------------------------------------------------------------
divide.hl.bc:

 ; a = hl / bc
 ; hl = remainder
 ; z when a = 0

    ld a,-1
    or a
    @div.loop:
        sbc hl,bc
        inc a
        jr nc,@div.loop

    add hl,bc
    or a

    ret

;---------------------------------------------------------------
cnv.a.to.de:

 ; put a at address de and de+1 in ascii format

    ld c,-1

 @get.tens:
    sub 10
    inc c
    jr nc,@-get.tens

    add 10 + "0"
    inc de
    ld (de),a
    dec de

    ld a,c
    add "0"
    ld (de),a
    inc de
    inc de

    ret

;---------------------------------------------------------------
print.num:

    ld b,a
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    call @pr.num.hex
    ld a,b
    and 0x0f

 @pr.num.hex:
    add "0"
    cp ":"
    jr c,$+4
    add 7

;---------------------------------------------------------------
print.chr:

 ; a = character to print

    push bc
    push de
    push hl
    ld c," "
    cp " "
    jr c,@unprintable
    cp 128
    jr nc,@unprintable
    ld c,a
 @unprintable:
    ex de,hl
    ld b,0
    ld l,c
    ld h,b
    add hl,hl
    add hl,hl
    add hl,bc
    ld bc,loader.font_high - 160    ; -" "*5
    add hl,bc
    ld b,5
    @loop:
        ld a,(hl)
        ld (de),a
        inc hl
        ld a,e
        add video.memory.bytes.per.row
        ld e,a
        jr nc,$+3
        inc d

        djnz @-loop

    pop hl
    pop de
    pop bc
    inc l
    ret

;---------------------------------------------------------------
print.screen:

    push de
    ld a,6
    call set.attributes
    pop de

 print.screen.no.attr:

    ld hl,video.memory.high
    ld c,32

    @rows:

        ld b,32
        push hl

        @columns:

            ld a,(de)
            inc de
            or a
            jr z,@end.of.line
            call print.chr
            djnz @-columns

     @normal.line:

        pop hl
        ld a,l
        add video.memory.32.rows
        ld l,a
        jr nc,$+3
        inc h
        dec c
        jr nz,@-rows

 @exit:

    ld a,255
    ret

 @end.of.line:

    ld a,(de)
    cp " "
    jr nc,@-normal.line

    pop hl
    or a
    jr z,@exit

    inc de
    ld b,a
    ld a,l

    @empty.lines:

        add video.memory.32.rows
        ld l,a
        jr nc,$+3
        inc h
        dec c
        jr z,@exit

        djnz @-empty.lines

    jr @-rows

;---------------------------------------------------------------
cls:

 ; clear mode 2 screen

    ld hl,video.memory.high
    ld de,video.memory.high + 1
    ld bc,0x1800 - 1
    ld (hl),l
    ldir

    ld hl,video.memory.high.attributes
    ld de,video.memory.high.attributes + 1
    ld bc,0x1800 - 1
    ld (hl),l
    ldir

    ld hl,loader.palette + 0x0f
    ld bc,0x0100 * 0x10 + port.clut
    otdr

    ld hl,loader.palette
    ld de,palette.table
    ld bc,0x10
    ldir

    ld hl,loader.palette
    ld de,palette.table + 0x14
    ld bc,0x10
    ldir

    ret

;---------------------------------------------------------------
print.de.b:

 ; print text at DE, with padded out to B characters

    ld a,(de)
    inc de
    or a
    jr z,@loop.padding

    call print.chr
    djnz print.de.b
    ret

    @loop.padding:

        ld a," "
        call print.chr
        djnz @-loop.padding

    ret


;---------------------------------------------------------------
set.attributes:

 ; apply attributes to mode 2 screen

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

 @fill: equ for 31

    ld (hl),a
    inc l

 next @fill

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

colours:
 colour.black:   equ 0
    defb 0,0,0,0,0,0,0,0

 colour.blue:    equ 1
    defb 1,2,3,2,1,0,0,0

 colour.orange:  equ 2
    defb 4,5,6,5,4,0,0,0

 colour.green:   equ 3
    defb 7,9+56,3,9+56,7,0,0,0

 colour.purple:  equ 4
    defb 10+56,11+56,12+56,11+56,10+56,0,0,0

 colour.yellow:  equ 5
    defb 13+56,14+56,6,14+56,13+56,0,0,0


load.screen:
    defm "SAM MOD player             "
    include "constants/text.version.i"
    include "constants/text.copyright.i"
    defb 0,30
    defm "Use CURSORS + RETURN or JOYSTICK"


@load.attributes:
    defb  3,colour.orange
    defb  1,colour.green
    defb 25,colour.blue
    defb  2,colour.green
    defb  1,colour.yellow

black.attributes:
    defb  2,colour.orange
    defb 27,colour.black
    defb  2,colour.green
    defb  1,colour.yellow

;---------------------------------------------------------------

m.vollabel: defm "0123456789abcdef"

;---------------------------------------------------------------
errnodisc:

    ; this WAS called by the FDC routines

    ld sp,(save.sam.sp+1)
    ld a,1
    ld (nodisc+1),a
    ret

;---------------------------------------------------------------
findfile:

    ld (save.sam.sp+1),sp

    ld de,fat.matchfile
    ld bc,11
    ldir
    call fat.readroot
    call fat.load_path
    call c,fat.reset_path
    ld bc,(fat.dir_entries)
    ld hl,(fat.data)
 fmclp:
    push hl
    push bc
    ld b,11
    ld de,fat.matchfile
 fmblp:
    ld a,(de)
    cp (hl)
    inc de
    inc hl
    jr nz,nomatch
    djnz fmblp
    pop bc
    pop hl
    ret
 nomatch:
    pop bc
    pop hl
    ld de,32
    add hl,de
    dec bc
    ld a,b
    or c
    jr nz,fmclp
    pop af              ; chuck return address
    jp file.notfound

;---------------------------------------------------------------
select.drive.1:
    ld a,1
    ld hl,fat.path_a
 @select.drive:
    ld (loader.drive),a
    ld (fat.path),hl
    ret

;---------------------------------------------------------------
select.drive.2:
    ld a,2
    ld hl,fat.path_b
    jr @select.drive

;---------------------------------------------------------------
relocate.low:

 ; move code after call to inst.buffer and execute it

    ld (@store.hl+1),hl
    pop hl

    push de
    push bc

    ld c,(hl)
    inc hl
    ld b,(hl)
    inc hl

    ld de,inst.buffer
    ldir

    pop bc
    pop de

    push hl ; return address

 @store.hl:
    ld hl,0

    jp inst.buffer

;---------------------------------------------------------------

include "loader/bdos.s"
include "loader/unpack.s"
include "loader/fat.s"

;---------------------------------------------------------------

loader.entries: defb 0

loader.dir:
    defm "filename"
    defm "20 char module title"
    defb 0          ; module type 0=noise, 1=pro, 2=star +64 = 4 bit compressed
    defb 0          ; length in patterns
    defm "100672"   ; date stamp
    defw 0          ; total size in k
    defb 0          ; number of samples (len>1)

    load.len:   equ $ - loader.dir

    ; defs 27 * load.len  ;max 27 on screen (25 files) - overwrite device selection


; in screen area

fat:            equ video.memory.high + 2 * video.memory.32.rows

temp.spc:       equ video.memory.high + video.memory.32.rows * 32

;===============================================================

    include "device.s"

assert $ + 0x0200 < 0xe000


