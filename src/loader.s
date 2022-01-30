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

;-------------------------------------------------------------------------------
; loader.variables
    loader.ram:             defb 0 ; %XXXRR (RAM / 256K)

    loader.dos.version:     defb 0 ; (dvar 7)
    loader.drive_2.tracks:  defb 0 ; (dvar 2)
    loader.drive:           defb 0 ; [1-2]
    loader.record:          defw 0
    loader.device:          defb 0 ; [0-5]
;-------------------------------------------------------------------------------
loader.font_high:

    mdat "../res/font.bin"

;-------------------------------------------------------------------------------
cursor.init:

    ; input
    ; - a = max value
    ; - b = row

    ld (max.select+1),a

    ld hl,screen - screen.32.rows - screen.width
    ld de,screen.32.rows
    inc b
    @loop:
        add hl,de
        djnz @-loop

    ld (@cursor.offset+1),hl

    ret

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
@show.screen:

    ; print header / footer

    call cls

    ld de,load.screen
    ld ix,@load.attributes
    call print.screen

    ret

;-------------------------------------------------------------------------------
@loader.init:

    ld hl,mes.no_disc
    ld de,text.volume.label
    ld bc,@mes.label.len
    ldir

    ld a,1
    ld (msdos+1),a

    xor a
    ld (nodisc+1),a

    ret

;-------------------------------------------------------------------------------
@check.drives:

    ; output:
    ; - de = address next file entry

    ld hl,option.dir
    ld de,loader.dir
    ld bc,loader.dir.len
    ldir

    ld a,(loader.dos.version)
    cp dvar.version.b_dos.max + 1
    jr c,@is.bdos

    ld a,(loader.drive_2.tracks)
    or a
    ld a,1
    jr z,@no.drive.2

    ld bc,loader.dir.len
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
        ld bc,loader.dir.len
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

;-------------------------------------------------------------------------------
@display.record.number:

    ld a,dvar.record
    call bdos.get.dvar.word

    ld de,loader.dir + loader.dir.len + 15
    jp text.hl.decimal

;-------------------------------------------------------------------------------
loader:

    call @loader.init
    call @show.screen
    call @check.drives

    push de
    call fat.disk.read
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

    call @directory.read

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
    ld a,(ix + @loader.dir.type)
    bit 7,a
    jr nz,@disc.mess

    call @insert.sample.bits

    call mod.text.de

    push bc             ; c = select position

    ld b,screen.width - 10
    ld hl,screen + screen.32.rows * 29
    call print.de.b

    ld hl,screen + screen.32.rows * 29 + screen.width - 10
    call @print.date

    call @insert.samples
    call @insert.patterns
    call @insert.size

    pop bc

    ld de,@mes.details
    jr @+continue

 @disc.mess:

    ld hl,screen + screen.32.rows * 29
    ld b,32
    @loop:
        ld a," "
        call print.chr
        djnz @-loop

    ld de,mes.drv

 @continue:

    ld b,32
    ld hl,screen + screen.32.rows * 30
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

    ld a,(ix + @loader.dir.type)
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

;-------------------------------------------------------------------------------
@directory.read:

 msdos:
    ld a,0
    or a
    jp z,bdos.directory.read

    jp fat.directory.read

;-------------------------------------------------------------------------------
@print.date:

    ld a,(ix + @loader.dir.date)        ; day (10)
    cp "*"
    jr z,@no.date

    call print.chr
    ld a,(ix + @loader.dir.date + 1)    ; day (1)
    call print.chr
    ld a,"/"
    call print.chr
    ld a,(ix + @loader.dir.date + 2)    ; month (10)
    call print.chr
    ld a,(ix + @loader.dir.date + 3)    ; month (1)
    call print.chr
    ld a,"/"
    call print.chr
    ld a,(ix + @loader.dir.date + 4)    ; year (1000)
    call print.chr
    ld a,(ix + @loader.dir.date + 5)    ; year (100)
    call print.chr
    ld a,(ix + @loader.dir.date + 6)    ; year (10)
    call print.chr
    ld a,(ix + @loader.dir.date + 7)    ; year (1)
    call print.chr

    ret

 @no.date:

    ld b,10

    @loop:

        ld a," "
        call print.chr
        djnz @-loop

    ret

;-------------------------------------------------------------------------------
@insert.sample.bits:

 ; insert 4b when mod is compressed 4 bit mod

    push bc

    ld bc," " * 0x101
    bit 6,a
    jr z,@insert.bc

    res 6,a
    ld bc,"4" + "b" * 0x100

 @insert.bc:

    ld hl,@mes.details.sample.bits
    ld (hl),c
    inc hl
    ld (hl),b

    pop bc

    ret

;-------------------------------------------------------------------------------
@insert.samples:

    ld l,(ix + @loader.dir.samples)
    ld h,0
    ld de,@mes.details.samples

    jp @text.hl.decimal.10.right

;-------------------------------------------------------------------------------
@insert.patterns:

    ld l,(ix + @loader.dir.patterns)
    ld h,0
    ld de,@mes.details.patterns

    jp @text.hl.decimal.100.right

;-------------------------------------------------------------------------------
@insert.size:

    ld l,(ix + @loader.dir.size + 0)
    ld h,(ix + @loader.dir.size + 1)

    ld de,@mes.details.size

    jp @text.hl.decimal.100.right

;-------------------------------------------------------------------------------
@print.octave:

    ld hl,screen + screen.32.rows * 1 + 26
    ld de,mes.oct
    ld b,5
    call print.de.b

    ld a,(loader.octaves+1)
    add "0"
    call print.chr

    ret

;-------------------------------------------------------------------------------
@print.drive.number:

    ld a,(loader.drive)
    add a,"0"
    ld (mes.drive+6),a

    ret

;-------------------------------------------------------------------------------
@set.drive.label:

    ; sets drive label (from text.volume.label) or no label if empty

    ld hl,text.volume.label
    ld de,mes.label
    ld bc,@mes.label.len

    ld a,(hl)
    or a
    jr nz,@has.label

    ld hl,mes.no_label

 @has.label:

    ldir

    ret

;-------------------------------------------------------------------------------
@print.drive.label:

    call @set.drive.label

    ld hl,screen + screen.32.rows * 3
    ld de,mes.drive
    ld b,9 + @mes.label.len
    call print.de.b

    ret

;-------------------------------------------------------------------------------
@print.files:

    ld a,(loader.entries)
    cp 24
    jr c,@max.24
        ld a,24
    @max.24:
    ld c,a
    ld de,loader.dir
    ld hl,screen + screen.32.rows * 4 + 1

    @loop:
        push de
        push hl

        ld b,8
        call print.de.b ; file name
        inc l

        ld b,20
        call print.de.b ; module name

        pop hl

        ld de,screen.32.rows
        add hl,de

        pop de
        ld a,e
        add loader.dir.len
        ld e,a
        jr nc,$+3
        inc d

        dec c
        jr nz,@-loop

    ret

;-------------------------------------------------------------------------------
loader.quit:

    xor a
    out (port.lmpr),a
    rst 0

;-------------------------------------------------------------------------------
insert.file.size:

 file.size_kb:
    ld hl,0     ; ch (-> in 256 bytes)
    srl h
    rr l
    srl h
    rr l
    jr nc,@no.round
    inc hl
 @no.round:
    ex de,hl
    ld (hl),e   ; size in k
    inc hl
    ld (hl),d
    inc hl

    call mod.get.samples.a
    ld (hl),a

    ret

;-------------------------------------------------------------------------------
fc.sam:
    push de

    ld de,(screen.free + 1);length mod 16384
    ld a,(screen.free + 7) ;length in pages (16384)
    and %00000011
    rrca
    rrca
    add d
    ld d,a
    ld a,(screen.free + 7)
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

;-------------------------------------------------------------------------------
select.key:

    ; c = current record [0-25]

    call get.entry.ix.from.c

    ld a,(ix + @loader.dir.type)    ;mod type ,+128=drive
    bit 7,a
    jp nz,new.read
    push af

    ld de,mes.load
    ld b,10
    ld hl,screen + screen.32.rows * 31
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
    call fat.file.find
    ld a,(msdos+1)
    or a
    call nz,fat.load

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
sam.match:
    push ix
    ld a,(hl)
    cp samdos.filetype.code
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
    and %10000000
    or samdos.dir.track
    ld l,a
    ld d,(hl)       ; first track
    inc l
    ld e,(hl)       ; first sector

    push hl
    push de

    ld hl,@relocate.load.mod
    ld bc,@load.mod.len
    ld a,(loader.ram)
    and %11100
    jr z,@no.meg
    ld hl,@relocate.meg.load.mod
    ld bc,@meg.load.mod.len
 @no.meg:
    ld de,inst.buffer
    ldir
    pop de
    pop hl

    ld a,(loader.drive)

    call @load.mod

    jp file.loaded

 sam.no.match:
    pop ix
    ret

;-------------------------------------------------------------------------------
@relocate.load.mod:

    org inst.buffer

 @load.mod:

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

 @load.mod.len: equ $ - @load.mod

    org @relocate.load.mod + @load.mod.len

;-------------------------------------------------------------------------------
@relocate.meg.load.mod:

    org inst.buffer

 @meg.load.mod:

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

 @meg.load.mod.len: equ $ - @meg.load.mod

    org @relocate.meg.load.mod + @meg.load.mod.len

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
get.entry.ix.from.c:

 ; input:
 ; - c  = record #

 ; output:
 ; - ix = pointer to directory entry

    ld ix,loader.dir - loader.dir.len
    ld de,loader.dir.len
    ld b,c
    inc b
    @loop:
        add ix,de
        djnz @loop

    ret

;-------------------------------------------------------------------------------
new.read:

    and 0x7f
    or a
    push af
    call z,select.drive.1
    pop af
    call nz,select.drive.2

    jp loader

;-------------------------------------------------------------------------------
var.cursor.blink.timer:     defb 0
var.cursor.previous.row:    defb 0  ; relative to offset

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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
    ld de,screen.width
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

    ld ix,colours + ( colour.purple * 8 )
    ld de,screen.width

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

;-------------------------------------------------------------------------------
@cursor.get.address:

 ; input
 ; - a = line

    ld de,screen.width
 @cursor.offset:
    ld hl,0                     ; -0x0020
    inc a
    @loop:
        add hl,de
        dec a
        jr nz,@-loop

    ret

;-------------------------------------------------------------------------------
; messages
 mes.oct:               defm "Oct: "
 mes.drive:             defm "Drive 1: "

 mes.label:             defm "Solar Flare     "
    @mes.label.len:        equ $ - mes.label
 mes.no_label:          defm "No label        "
 mes.no_disc:           defm "No disc         "

 @mes.details:
    @mes.details.samples:       defm "00 samples "
    @mes.details.sample.bits:   defm "4b "
    @mes.details.patterns:      defm "000 patterns  "
    @mes.details.size:          defm "000K"

 mes.drv:               defm "Press RETURN for new directory. "

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

;-------------------------------------------------------------------------------
text.hl.decimal:

 ;put decimal value of hl at address de, left aligned

    ld bc,10000
    call divide.hl.bc
    ld (@var.buffer + 0),a

    ld bc,1000
    call divide.hl.bc
    ld (@var.buffer + 1),a

    ld bc,100
    call divide.hl.bc
    ld (@var.buffer + 2),a

    ld bc,10
    call divide.hl.bc
    ld (@var.buffer + 3),a

    ld a,l
    ld (@var.buffer + 4),a

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

;-------------------------------------------------------------------------------
@text.hl.decimal.100.right:

 ;put decimal value of hl at address de, right aligned

 ; input:
 ; - hl = number
 ; - de = destination

    ld bc,100
    call divide.hl.bc
    ld (@var.buffer + 0),a

    ld bc,10
    call divide.hl.bc
    ld (@var.buffer + 1),a

    ld a,l
    ld (@var.buffer + 2),a

    ld hl,@var.buffer
    ld b,3

 @text.buffer.to.de:

    @loop:

        ld a,(hl)
        or a
        jr nz,@digit

        ld a," "
        ld (de),a
        inc de
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

;-------------------------------------------------------------------------------
@text.hl.decimal.10.right:

 ;put decimal value of hl at address de, right aligned

 ; input:
 ; - hl = number
 ; - de = destination

    ld bc,10
    call divide.hl.bc
    ld (@var.buffer + 0),a

    ld a,l
    ld (@var.buffer + 1),a

    ld hl,@var.buffer
    ld b,2

    jr @text.buffer.to.de

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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
        add screen.width
        ld e,a
        jr nc,$+3
        inc d

        djnz @-loop

    pop hl
    pop de
    pop bc
    inc l
    ret

;-------------------------------------------------------------------------------
print.screen:

    push de
    ld a,6
    call set.attributes
    pop de

 print.screen.no.attr:

    ld hl,screen
    ld c,32

    @rows:

        ld b,screen.width
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
        add screen.32.rows
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

        add screen.32.rows
        ld l,a
        jr nc,$+3
        inc h
        dec c
        jr z,@exit

        djnz @-empty.lines

    jr @-rows

;-------------------------------------------------------------------------------
cls:

 ; clear mode 2 screen

    ld hl,screen
    ld de,screen + 1
    ld bc,0x1800 - 1
    ld (hl),l
    ldir

    ld hl,screen.attributes
    ld de,screen.attributes + 1
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

;-------------------------------------------------------------------------------
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


;-------------------------------------------------------------------------------
set.attributes:

 ; apply attributes to mode 2 screen

    ld (line.size+1),a

    ld hl,screen.attributes
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
    cp ( screen.attributes + 0x1800 ) / 0x100
    jr nz,col.loop
    ret

;-------------------------------------------------------------------------------

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

;-------------------------------------------------------------------------------

text.volume.label:  defm "0123456789abcdef"

;-------------------------------------------------------------------------------
errnodisc:

    ; this WAS called by the FDC routines

    ld sp,(save.sam.sp+1)
    ld a,1
    ld (nodisc+1),a
    ret

;-------------------------------------------------------------------------------
select.drive.1:
    ld a,1
    ld hl,fat.path_a
 @select.drive:
    ld (loader.drive),a
    ld (fat.path),hl
    ret

;-------------------------------------------------------------------------------
select.drive.2:
    ld a,2
    ld hl,fat.path_b
    jr @select.drive

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------

include "loader/bdos.s"
include "loader/unpack.s"
include "loader/fat.s"

;-------------------------------------------------------------------------------

loader.entries: defb 0

loader.dir:
 @loader.dir.file:      equ $ - loader.dir
    defm "filename"
 @loader.dir.title:     equ $ - loader.dir
    defm "20 char module title"
 @loader.dir.type:      equ $ - loader.dir
    defb 0          ; module type (see mod.s) +64 = 4 bit compressed, +128 = drive
 @loader.dir.patterns:  equ $ - loader.dir
    defb 0          ; length in patterns
 @loader.dir.date:      equ $ - loader.dir
    defm "10061972" ; our stamp
 @loader.dir.size:      equ $ - loader.dir
    defw 0          ; total size in K
 @loader.dir.samples:   equ $ - loader.dir
    defb 0          ; number of samples (len>1)

    loader.dir.len:     equ $ - loader.dir

    ; defs 27 * loader.dir.len  ;max 27 on screen (25 files) - overwrite device selection


; in screen area

loader.directory:   equ screen + 2 * screen.32.rows
    loader.directory.filename:      equ 0x00        ; 10 characters
        loader.directory.filename.len:  equ 0x0a
    loader.directory.track:         equ 0x0a        ; byte
    loader.directory.sector:        equ 0x0b        ; byte
    loader.directory.size_kb:       equ 0x0c        ; word
    loader.directory.date:          equ 0x0e        ;
        loader.directory.date.day:      equ 0x0e    ;
        loader.directory.date.month:    equ 0x0f    ;
        loader.directory.date.year:     equ 0x10    ;

loader.directory.len:               equ loader.directory.date + 3

;===============================================================

    include "device.s"

assert $ + 0x0200 < 0xe000


