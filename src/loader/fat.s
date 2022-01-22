; SAM MOD player - loader: fat (aka ms-dos / pc-dos)

; https://en.wikipedia.org/wiki/Design_of_the_FAT_file_system
; https://en.wikipedia.org/wiki/File_Allocation_Table#FAT12
; https://www.eit.lth.se/fileadmin/eit/courses/eitn50/Literature/fat12_description.pdf

; to do:
; - fix msdos

;(C) 2019-2022 Stefan Drissen

;-------------------------------------------------------------------------------

@directory_entry:

    @dir.file_name:                 equ 0x00
    @dir.file_extension:            equ 0x08
        @dir.file_name_len:             equ 0x0b
    @dir.file_attributes:           equ 0x0b
        @dir.attribute.volume_label:    equ 3
        @dir.attribute.subdirectory:    equ 4

    @dir.last_modified_date:        equ 0x18    ; yyyyyyy mmmm ddddd (0 = 1980)
    @dir.start_cluster:             equ 0x1a    ; word
    @dir.file_size_bytes:           equ 0x1c    ; dword

    @directory_entry.size:          equ 0x20

;-------------------------------------------------------------------------------
fat.disk.read:

 ; check if disk is a fat formatted disk

    ld (save.sam.sp+1),sp   ; for quick exits

    call @root.read

    xor a
    ld (text.volume.label),a

    ld a,(@var.dir_entries)
    ld hl,(@fat.data)
    ld de,@dir.file_attributes
    add hl,de
    ld de,@directory_entry.size
    ld b,a

    @find.label:

        bit @dir.attribute.volume_label,(hl)
        jr nz,@found.label

        add hl,de
        djnz @-find.label

    jr z,@no.label

 @found.label:

    ld de,@dir.file_attributes
    xor a
    sbc hl,de
    ld b,@dir.file_name_len
    ld de,text.volume.label

    @loop:

        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        djnz @-loop

 @no.label:

    call @path.copy
    call @path.load
    call c,@path.reset

    ret

;-------------------------------------------------------------------------------
fat.directory.read:

    ld hl,(@fat.data)

    @loop.directory.entries:

        ld a,(hl)
        or a                ; 0 = end of directory
        ret z

        cp 0xe5             ; deleted file
        jp z,@next.file

        push hl
        pop ix
        bit @dir.attribute.volume_label,(ix + @dir.file_attributes)
        jp nz,@next.file

        ld a,(ix + @dir.file_extension + 0)
        cp "M"
        jp nz,@next.file
        ld a,(ix + @dir.file_extension + 1)
        cp "O"
        jp nz,@next.file
        ld a,(ix + @dir.file_extension + 2)
        cp "D"
        jp nz,@next.file    ; not MOD extension

        push hl
        push de
        ld b,8

        @loop:
            ld a,(hl)
            ld (de),a
            inc hl
            inc de

            djnz @-loop

        push de

        ld e,(ix + @dir.start_cluster + 0)
        ld d,(ix + @dir.start_cluster + 1)

        ld hl,dos.sector

        @read.more:

            call @cluster.read
            call @entry.get
            ld a,h
            cp ( screen.free + mod.pt.pattern ) / 256 + 1

            jr c,@-read.more

        pop de

        ; ld a,2
        ; call file.check

        push de

        ld e,(ix + @dir.file_size_bytes + 0)
        ld d,(ix + @dir.file_size_bytes + 1)
        ld a,(ix + @dir.file_size_bytes + 2)
        ex de,hl
     @pc.resub:
        or a
        sbc hl,de
        sbc c
        jr nc,@pc.got.maxmin
        adc c
        add hl,de
        ex de,hl
        ld b,a
        ld a,c
        ld c,b
        jr @pc.resub

     @pc.got.maxmin:        ; ahl = difference calc len & file len
        pop de
        or h
        jr z,@file.ok

        pop de
        pop hl
        jr @next.file

     @file.ok:

     ;get date
        ld a,(ix + @dir.last_modified_date + 0) ; date yyyyyyy m|mmm ddddd
        ld b,a
        and %00011111       ; day
        call cnv.a.to.de
        ld a,b
        and %11100000       ; low bits month
        rlca
        rlca
        rlca
        ld c,a
        ld a,(ix + @dir.last_modified_date + 1) ; date yyyyyyy m|mmm ddddd
        ld b,a
        and %00000001       ; high bit month
        rlca
        rlca
        rlca
        or c                ; month
        call cnv.a.to.de
        ld a,b
        and %11111110       ; year
        rrca
        add 80
        sub 100
        jr nc,$-2
        add 100             ; year
        call cnv.a.to.de

        call insert.file.size
        ld hl,loader.entries
        inc (hl)
        pop hl
        ld bc,loader.dir.len
        add hl,bc
        ex de,hl
        pop hl

     @next.file:

        ld bc,@directory_entry.size
        add hl,bc
        ld a,(loader.entries)
        cp 27
        ret z

        jp @-loop.directory.entries

;-------------------------------------------------------------------------------
@root.read:

 ; read root directory

 ; input
 ; - hl = address

    call @boot_sector.read
    call @fat.read
    call @startcluster
    push de
    call @startroot
    pop hl
    xor a
    sbc hl,de
    ld b,l
    push bc
    call @startroot
    pop bc
    ld hl,(@fat.data)

    @loop:

        call @logical_sector.read
        inc de
        djnz @-loop

    ld a,1
    or a

    ret

;-------------------------------------------------------------------------------
@path.reset:

    ld hl,(fat.path)
    ld (hl),"\"
    inc hl
    ld (hl),0
    call @path.copy
    jp @root.read

;-------------------------------------------------------------------------------
@path.copy:

    push hl
    push de
    push bc
    ld hl,(fat.path)
    ld de,@path.temp
    ld bc,64
    ldir
    pop bc
    pop de
    pop hl
    ret

;-------------------------------------------------------------------------------
@path.load:

    ld a,(@path.temp+1)
    or a
    ret z
    ld hl,@path.temp
    ld (@parameter.last),hl
    call @parameter.get

 lploop:

    ld a,(@parameter)
    or a
    ret z
    call @getinputfile

    push hl
    ld hl,(@fat.data)
    ld bc,(@var.dir_entries)

 lpmatchlp:

    ld de,fat.parafile
    push hl
    push bc
    ld b,@dir.file_name_len

    @loop:

        ld a,(de)
        cp (hl)
        jr nz,lpnomatch
        inc hl
        inc de
        djnz @-loop

    pop bc
    pop ix
    bit @dir.attribute.subdirectory,(ix + @dir.file_attributes)
    jr nz,lpisdir

    push ix
    push bc

    jp lpnomatch

 lpisdir:

    ld e,(ix + @dir.start_cluster)
    ld d,(ix + @dir.start_cluster + 1)

    ld hl,(@fat.data)
    ld bc,0

    @read.more:

        push bc
        call @cluster.read
        pop bc
        inc bc

        call @entry.get

        ld a,d
        cp 0x0f
        jr nz,@-read.more

        ld a,e
        cp 0xf8
        jr c,@-read.more

    ld hl,(@var.bytes_cluster)
    srl h
    rr l
    srl h
    rr l
    srl h
    rr l
    srl h
    rr l
    srl h
    rr l
    ex de,hl
    ld hl,0

    @loop:

        add hl,de
        dec bc
        ld a,b
        or c
        jr nz,@-loop

    ld (@var.dir_entries),hl
    pop hl

    jp lploop

 lpnomatch:

    pop bc
    pop hl
    ld de,@directory_entry.size
    add hl,de
    dec bc
    ld a,b
    or c
    jr nz,lpmatchlp

    pop hl
    scf
    ret

;-------------------------------------------------------------------------------
@input_path.get:

    call @path.copy
    ld hl,@parameter
    ld de,@path.temp
    ld a,(hl)
    cp "\"
    jr z,gipnewpath
    ld a,(de)
    or a
    jr z,gipnewpath

    @find.end:
        inc de
        ld a,(de)
        or a
        jr nz,@-find.end

    ld a,e
    cp ( @path.temp + 1 ) \ 256
    jr nz,gipnewpath
    dec de

 gipnewpath:

    ld a,"\"
    ld (de),a
    inc de

    ld a,(@parameter)
    cp "\"
    jr nz,gipns
    ld a,(@parameter+1)
    or a
    jr nz,gipns
    push de
    call @parameter.get
    pop de
    ld a,(@parameter)
 gipns:
    cp "."
    jr nz,gipnotdot
    push de
    call @parameter.get
    pop de
    ld a,(@parameter)
    cp "."
    jr nz,gipfndlstp
    dec de
 gipfndlstp:
    dec de
    ld a,(de)
    cp "\"
    jr nz,gipfndlstp
    ld a,e
    cp @path.temp \ 256
    jr nz,$+3
    inc de
    xor a
    ld (de),a

    push de
    call @parameter.get
    pop de
    jp gipdoneext

 gipnotdot:

    push de
    call @getinputfile
    pop de
    jp c,@directory.invalid

    ld hl,fat.parafile
    ld b,8

    @loop:

        ld a,(hl)
        cp " "
        jr z,gipdonename
        ld (de),a
        inc hl
        inc de
        djnz @-loop

 gipdonename:

    ld hl,fat.parafile+8
    ld a,(hl)
    cp " "
    jr z,gipdoneext
    ld a,"."
    ld (de),a
    inc de
    ld b,3

    @copy.ext:

        ld a,(hl)
        cp " "
        jr z,gipdoneext
        ld (de),a
        inc hl
        inc de
        djnz @-copy.ext

 gipdoneext:
    ld a,(@parameter)
    cp "\"
    jr z,gipnewpath

 gipend:

    xor a
    ld (de),a
    ret

;-------------------------------------------------------------------------------
@getinputfile:

    ld hl,fat.parafile
    ld b,@dir.file_name_len

    @loop:

        ld (hl)," "
        inc hl
        djnz @-loop

    ld hl,@parameter
    ld de,fat.parafile
    ld a,(hl)
    cp "."
    jr z,gifextonly
    ld b,9

    @loop:
        ld a,(hl)
        inc hl
        or a
        jr z,gifendname

        cp "\"
        jr z,@-loop

        ld (de),a
        inc de
        djnz @-loop

    scf                 ; file longer than 8 chars
    ret

 gifendname:
    call @parameter.get
    jr z,gifendext

    ld hl,@parameter
    ld a,(hl)
    cp "."
    jr nz,gifendext
 gifextonly:
    inc hl
    ld de,fat.parafile+8
    ld b,4

    @copy.ext:
        ld a,(hl)
        or a
        jr z,gifendext
        ld (de),a
        inc hl
        inc de
        djnz @-copy.ext

    call @parameter.get
    scf
    ret             ; extension longer than 3 chars

 gifendext:

    xor a
    ret

;-------------------------------------------------------------------------------
@sector.read:

 ; input:
 ; - de = track / sector
 ; - hl = address (in current page)

    push hl

    ld hl,dos.sector
    push hl
    call bdos.read.sector
    pop hl

    pop de

    ld bc,512
    ldir

    ret

;-------------------------------------------------------------------------------
@boot_sector.read:

 ; read boot sector from disc at fixed address

    ld de,0x0001                    ; track 0, sector 1
    ld hl,@boot_sector
    call @sector.read

    ld hl,(@bs.total_sectors)        ; 3.5" DD -> 1440
    ld de,(@bs.sectors_per_track)    ; 3.5" DD -> 9
    ld a,d
    or e
    jr z,notpcdisc  ; sectors / track > 0

    xor a
    ld bc,0

    @loop:

        inc bc
        sbc hl,de
        jr nc,@-loop

    dec bc
    add hl,de
    ld a,h
    or l
    jr nz,notpcdisc ; total sectors / sectors per track

    ld h,b
    ld l,c
    ld de,(@bs.number_of_heads)      ; 3.5" DD -> 2
    ld a,d
    or e
    jr z,notpcdisc

    xor a

    @loop:

        sbc hl,de
        jr nc,@-loop

    add hl,de
    ld a,h
    or l
    jr nz,notpcdisc

    ld hl,0
    ld bc,(@bs.bytes_per_sector)    ; 3.5" DD -> 512
    ld a,(@bs.sectors_per_cluster)  ; 3.5" DD -> 2

    @loop:

        add hl,bc
        dec a
        jr nz,@-loop

    ld (@var.bytes_cluster),hl      ; 3.5" DD -> 1024
    ld hl,(@bs.max_root_entries)    ; 3.5" DD -> 112
    ld (@var.dir_entries),hl

    ld ix,black.attributes
    ld a,6
    jp set.attributes

 notpcdisc:
    xor a
    ld (msdos+1),a
 save.sam.sp:
    ld sp,0
    ret

;-------------------------------------------------------------------------------
@fat.read:

 ; read FAT at fixed address, (data) -> first address after FAT

    push hl

    ld hl,loader.directory
    ld a,(@bs.sectors_per_fat)
    ld b,a
    ld de,1                 ; logical sector

    @loop:
        call @logical_sector.read
        inc de
        djnz @-loop

    ld (@fat.data),hl

    pop hl

    ret

;-------------------------------------------------------------------------------
@cluster.read:
 ; read cluster from disc

 ; input:
 ; - de = cluster number (2-711)
 ; - hl = address

    push de
    push hl
    dec de
    dec de
    ld hl,0
    ld a,(@bs.sectors_per_cluster)
    ld b,a

    @loop:

        add hl,de
        djnz @-loop

    call @startcluster
    add hl,de
    ex de,hl
    pop hl

    ld a,(@bs.sectors_per_cluster)
    ld b,a

    @loop:

        call @logical_sector.read
        inc de
        djnz @-loop

    pop de
    ret

;-------------------------------------------------------------------------------
@startroot:

 ; calculate start sector of root directory
 ; returns de with

    ld de,1
    ld a,(@bs.number_of_fats)
    ld b,a

    @loop:

        ld a,(@bs.sectors_per_fat)
        add a,e
        ld e,a
        djnz @-loop

    ret

;-------------------------------------------------------------------------------
calcclusters:

 ; calculate total number of clusters on disc

    ld hl,(@bs.total_sectors)
    call @startcluster
    xor a
    sbc hl,de
    ld a,(@bs.sectors_per_cluster)

    @loop:

        srl a
        jr z,@leave

        srl h
        rr l
        jr @-loop

 @leave:

    ld c,l
    ld b,h
    ret

;-------------------------------------------------------------------------------
@startcluster:

 ; calculate start sector of cluster 2 (first data cluster)

 ; output:
 ; - de = logical sector of first data cluster

    push hl
    ld hl,(@bs.max_root_entries)
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl   ; * 32 bytes per entry
    ld de,0
    ld bc,(@bs.bytes_per_sector)
    xor a

    @loop:
        sbc hl,bc
        inc e
        jr nc,@-loop

    dec e
    ex de,hl
    call @startroot
    add hl,de
    ex de,hl
    pop hl

    ret

;-------------------------------------------------------------------------------
@entry.get:
 ; get FAT entry
 ; entries are 12 bits

 ; input:
 ;   de = cluster

    push hl

    ld h,d
    ld l,e
    add hl,hl
    add hl,de

    ld de,loader.directory ; fat
    srl h
    rr l
    jr c,@odd

    add hl,de
    ld e,(hl)

    inc hl
    ld a,(hl)
    jr @set.d

 @odd:

    add hl,de
    ld a,(hl)
    rrca
    rrca
    rrca
    rrca
    and 0x0f
    ld e,a

    inc hl
    ld a,(hl)
    rlca
    rlca
    rlca
    rlca
    ld d,a
    and 0xf0
    or e
    ld e,a

    ld a,d

 @set.d:

    and 0x0f
    ld d,a

    pop hl
    ret

;-------------------------------------------------------------------------------
@logical_sector.read:
 ; read logical sector from disc

 ; input:
 ; - de = sector number [0 - 1439] (for 720k disc)
 ; - hl = address

    push bc
    push de
    push hl
    ex de,hl
    ld bc,(@bs.sectors_per_track)
    ld de,0
    xor a

    @loop:

        sbc hl,bc
        inc d
        jr nc,@-loop

    adc hl,bc
    dec d
    ld e,l
    ld a,(@bs.number_of_heads)
    cp 2
    jr nz,$+4
    rrc d
    pop hl

    call @sector.read
    pop de
    pop bc

    ret

;-------------------------------------------------------------------------------
fat.file.find:

 ; input
 ; - hl -> file name

    ld (save.sam.sp+1),sp

    ld de,@file.match
    ld bc,@dir.file_name_len
    ldir

    call @root.read
    call @path.load
    call c,@path.reset
    ld bc,(@var.dir_entries)
    ld hl,(@fat.data)

    @loop.files:

        push hl
        push bc

        ld b,@dir.file_name_len
        ld de,@file.match

        @loop:

            ld a,(de)
            cp (hl)
            inc de
            inc hl
            jr nz,@leave

            djnz @-loop

     @leave:

        pop bc
        pop hl

        ret z

        ld de,@directory_entry.size
        add hl,de
        dec bc
        ld a,b
        or c

        jr nz,@-loop.files

    pop af              ; chuck return address

    jp file.notfound

;-------------------------------------------------------------------------------

fat.load: ; !!! does not work yet, needs to be moved to inst.buffer

    push hl
    pop ix
    ld e,(ix + @dir.start_cluster)
    ld d,(ix + @dir.start_cluster + 1)

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
    call @cluster.read
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

    call @entry.get
    ld a,d
    cp 0x0f
    jr nz,pc.load.all
    ld a,e
    cp 0xf8
    jr c,pc.load.all

    jp file.loaded

;-------------------------------------------------------------------------------
@directory.change:

    call @parameter.get
    jp z,@command.bad

    call @input_path.get
    call @root.read
    call @path.load
    jp c,@directory.invalid

    ld hl,@path.temp
    ld de,(fat.path)
    ld bc,64
    ldir

    ret

;-------------------------------------------------------------------------------
@parameter.get:

    ld hl,(@parameter.last)
    ld de,@parameter

    @loop:

        ld a,(hl)
        ld (de),a
        or a
        ret z

        inc hl
        cp " "
        jr z,@-loop

    @loop:

        ld (de),a
        inc de
        ld a,(hl)
        inc hl
        or a
        jr z,@leave
        cp " "
        jr z,@leave
        cp "\"
        jr z,@leave
        cp "."
        jr z,@leave

        jr @-loop

 @leave:

    dec hl
    ld (@parameter.last),hl
    xor a
    ld (de),a
    dec a

    ret

;-------------------------------------------------------------------------------

@command.bad:
    ld hl,@msbadfile
    ret

@filename.bad:
    ld hl,@msbadname
    ret

@file.not_found:
    ld hl,@msfilenot
    ret

@directory.invalid:
    ld hl,@msinvdir
    ret

;-------------------------------------------------------------------------------

@msbadfile:
    defb 13
    defm "Bad command or file name"
    defb 13,0

@msbadname:
    defb 13
    defm "Invalid file name"
    defb 13,0

@msfilenot:
    defb 13
    defm "File not found"
    defb 13,0

@msinvdir:
    defb 13
    defm "Invalid subdirectory"
    defb 13,0

;-------------------------------------------------------------------------------
@parameter.last:    defw 0
@parameter:         defs 255
fat.parafile:       defs @dir.file_name_len
@file.match:        defs @dir.file_name_len

fat.path:           defw fat.path_a

fat.path_a:
    defb "\",0
    defs 63
    defb 0

fat.path_b:
    defb "\",0
    defs 63
    defb 0

@path.temp:
    defb "\"
    defs 64

;-------------------------------------------------------------------------------
@boot_sector:               ; equ dos.sector

 ; https://en.wikipedia.org/wiki/Design_of_the_FAT_file_system#Bootsector

   ;@bs.oem_name:               equ @boot_sector + 0x003

  ; BIOS Parameter Block

    @bs.bytes_per_sector:       equ @boot_sector + 0x00b
    @bs.sectors_per_cluster:    equ @boot_sector + 0x00d
    @bs.reserved_sectors:       equ @boot_sector + 0x00e
    @bs.number_of_fats:         equ @boot_sector + 0x010
    @bs.max_root_entries:       equ @boot_sector + 0x011
    @bs.total_sectors:          equ @boot_sector + 0x013
   ;@bs.media_descriptor:       equ @boot_sector + 0x015
    @bs.sectors_per_fat:        equ @boot_sector + 0x016
    @bs.sectors_per_track:      equ @boot_sector + 0x018
    @bs.number_of_heads:        equ @boot_sector + 0x01a
   ;@bs.hidden_sectors:         equ @boot_sector + 0x01c
   ;@bs.big_total_sectors:      equ @boot_sector + 0x020

  ; Extended BIOS Parameter Block

   ;@bs.physical_drive_number:  equ @boot_sector + 0x024
    @bs.volume_id:              equ @boot_sector + 0x027
    @bs.volume_label:           equ @boot_sector + 0x02b
    @bs.volume_label.len:       equ 11

;-------------------------------------------------------------------------------

@var.bytes_cluster:         equ @bs.volume_label   + @bs.volume_label.len
@var.dir_entries:           equ @var.bytes_cluster + 2
@fat.data:                  equ @var.dir_entries   + 2  ; points to first address after FAT

defs 512