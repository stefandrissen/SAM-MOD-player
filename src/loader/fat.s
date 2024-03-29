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

        ld hl,screen.free

        @read.more:

            call @cluster.read
            call @cluster.next
            jr z,@eof
            ld a,h
            cp ( screen.free + mod.pattern ) / 256 + 1

            jr c,@-read.more

     @eof:
        ld l,(ix + @dir.file_size_bytes + 1)    ; 256 B = 0.25 KB
        ld h,(ix + @dir.file_size_bytes + 2)    ; 64 KB

        ld (file.size_kb + 1),hl                ; ??? why hl = 256B units

        push ix

        ld hl,screen.free
        call mod.determine.type

        pop ix
        pop de

        cp mod.type.invalid
        jr nz,@file.ok

        pop de
        pop hl
        jr @next.file

     @file.ok:

        ld hl,screen.free + mod.sample.title
        ld bc,mod.title.len
        ldir

        ld (de),a
        inc de

        call mod.get.patterns.a
        ld (de),a
        inc de

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

        ld h,0
        add 80              ; 0 = 1980
        ld l,a
        jr nc,$+3
        inc h

        ld a,19             ; century
        ld bc,100

        @loop:

            or a
            sbc hl,bc
            jr c,@leave
            inc a
            jr @-loop

     @leave:

        add hl,bc
        call cnv.a.to.de
        ld a,l
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

    call @data_area.logical_sector
    ex de,hl                        ; hl = first sector data area
    call @root.logical_sector       ; de = first sector root directory

    xor a
    sbc hl,de   ; -> hl = sectors used by root
    ld b,l

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
    call @input_file.get

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

        call @cluster.next
        jr nz,@-read.more

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
    jr z,@path.new

    ld a,(de)
    or a
    jr z,@path.new

    @find.end:
        inc de
        ld a,(de)
        or a
        jr nz,@-find.end

    ld a,e
    cp ( @path.temp + 1 ) \ 0x100
    jr nz,@path.new

    dec de

 @path.new:

    ld a,"\"
    ld (de),a
    inc de

    ld a,(@parameter)
    cp "\"
    jr nz,@ns

    ld a,(@parameter+1)
    or a
    jr nz,@ns

    push de
    call @parameter.get
    pop de
    ld a,(@parameter)

 @ns:                       ; ns = not subdirectory?

    cp "."
    jr nz,@not.dot

    push de
    call @parameter.get
    pop de
    ld a,(@parameter)
    cp "."
    jr nz,@fndlstp

    dec de

 @fndlstp:                  ; find last path?

    dec de
    ld a,(de)
    cp "\"
    jr nz,@-fndlstp

    ld a,e
    cp @path.temp \ 0x100
    jr nz,$+3
    inc de
    xor a
    ld (de),a

    push de
    call @parameter.get
    pop de
    jp @done.ext

 @not.dot:

    push de
    call @input_file.get
    pop de
    jp c,@directory.invalid

    ld hl,fat.parafile
    ld b,8

    @loop:

        ld a,(hl)
        cp " "
        jr z,@done.name
        ld (de),a
        inc hl
        inc de
        djnz @-loop

 @done.name:

    ld hl,fat.parafile + 8
    ld a,(hl)
    cp " "
    jr z,@done.ext
    ld a,"."
    ld (de),a
    inc de
    ld b,3

    @copy.ext:

        ld a,(hl)
        cp " "
        jr z,@done.ext
        ld (de),a
        inc hl
        inc de
        djnz @-copy.ext

 @done.ext:
    ld a,(@parameter)
    cp "\"
    jr z,@-path.new

    xor a
    ld (de),a
    ret

;-------------------------------------------------------------------------------
@input_file.get:

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

    bit 7,h
    jr z,@direct

    ld hl,dos.sector

 @direct:

    push hl
    call bdos.read.sector
    pop hl

    pop de

    ld bc,(@bs.bytes_per_sector)

    bit 7,d
    jr z,@+direct

    ldir
    ex de,hl

    ret

 @direct:

    ex de,hl
    add hl,bc

    ret

;-------------------------------------------------------------------------------
@boot_sector.read:

 ; read boot sector from disc at fixed address

    ld hl,0x200
    ld (@bs.bytes_per_sector),hl            ; boot sector always 512 bytes

    ld de,0x0001                            ; track 0, sector 1
    ld hl,@boot_sector
    call @sector.read

    ld hl,(@bs.signature)
    ld de,0xaa55
    or a
    sbc hl,de
    jr nz,@boot_sector.invalid

    ld de,(@bs.sectors_per_track)           ; 3.5" DD -> 9
    ld a,d
    or e
    jr z,@boot_sector.invalid               ; sectors / track > 0

    ld hl,(@bs.total_sectors)               ; 3.5" DD -> 1440
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
    jr nz,@boot_sector.invalid              ; total sectors / sectors per track

    ld h,b
    ld l,c
    ld de,(@bs.heads)                       ; 3.5" DD -> 2
    ld a,d
    or e
    jr z,@boot_sector.invalid

    xor a

    @loop:

        sbc hl,de
        jr nc,@-loop

    add hl,de
    ld a,h
    or l
    jr nz,@boot_sector.invalid

    ld hl,0
    ld bc,(@bs.bytes_per_sector)        ; 3.5" DD -> 512
    ld a,(@bs.sectors_per_cluster)      ; 3.5" DD -> 2

    @loop:

        add hl,bc
        dec a
        jr nz,@-loop

    ld (@var.bytes_cluster),hl          ; 3.5" DD -> 1024
    ld hl,(@bs.root_directory_entries)  ; 3.5" DD -> 112
    ld (@var.dir_entries),hl

    ld ix,black.attributes
    ld a,6
    jp set.attributes

 @boot_sector.invalid:
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

    call @data_area.logical_sector
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
@root.logical_sector:

 ; calculate start sector of root directory

 ; output
 ; - de = logical sector (always < 0x100)

    ld de,1
    ld a,(@bs.fat_copies)
    ld b,a

    @loop:

        ld a,(@bs.sectors_per_fat)
        add a,e
        ld e,a
        djnz @-loop

    ret

;-------------------------------------------------------------------------------
@data_area.logical_sector:

 ; calculate start sector of data area (cluster 2)

 ; output:
 ; - de = logical sector data area

    push hl

    ld hl,(@bs.root_directory_entries)
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl   ; * 32 -> hl = bytes needed for root directory

    ld de,0
    ld bc,(@bs.bytes_per_sector)
    xor a

    @loop:
        sbc hl,bc
        inc e
        jr nc,@-loop

    dec e       ; -> e = sectors needed for root
    ex de,hl
    call @root.logical_sector
    add hl,de
    ex de,hl    ; -> de = logical sector data

    pop hl

    ret

;-------------------------------------------------------------------------------
@cluster.next:
 ; get next cluster from FAT - entries are 12 bits

 ; input:
 ; - de = cluster

 ; output:
 ; - de = next cluster
 ; - nz = keep reading
 ; - z  = eof (0xff8)

    push hl

    ld h,d                  ;
    ld l,e                  ;
    add hl,hl               ; hl = de * 3
    add hl,de               ;

    ld de,loader.directory  ; fat

    srl h                   ;
    rr l                    ; hl = hl / 2
    jr c,@+odd

    add hl,de
    ld e,(hl)

    inc hl
    ld a,(hl)
    jr @+set.d

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

    cp 0x0f
    ret nz  ; nz

    ld a,e
    cp 0xf8
    ret c   ; nz

    xor a
    ret     ; z

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
    ld a,(@bs.heads)
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
fat.load:

; input:
; - hl = directory entry

    push hl
    pop ix

    ld hl,@relocate.to.hmpr
    ld bc,@relocate.to.hmpr.len

    ld a,(loader.ram)
    and %11100
    ld a,page.mod

    jr z,@+no.megabyte

    ld a,page.mod.megabyte
    ld hl,@relocate.to.xmpr
    ld bc,@relocate.to.xmpr.len

 @no.megabyte:

    ld (@page),a

    ld de,inst.buffer
    ldir

    ld e,(ix + @dir.start_cluster + 0)
    ld d,(ix + @dir.start_cluster + 1)

    ld hl,load.offs

 @load.all:

    push de             ; cluster
    push hl             ; target

    ld hl,dos.sector
    call @cluster.read

    pop de              ; target

   @page: equ $+1
    ld h,0
    ld bc,(@var.bytes_cluster)
    call inst.buffer

    bit 6,h
    res 6,h
    jr z,@page.ok

    ld a,(@page)
    inc a
    ld (@page),a

 @page.ok:

    pop de              ; cluster

    call @cluster.next
    jr nz,@-load.all

    pop af              ; chuck return address

    jp file.loaded

;-------------------------------------------------------------------------------
@relocate.to.hmpr:

 ; input:
 ; - h  = page
 ; - de = destination
 ; - bc = cluster size

    org inst.buffer

    in a,(port.hmpr)
    push af

    ld a,h
    out (port.hmpr),a

    ld hl,dos.sector
    ldir

    ex de,hl

    pop af
    out (port.hmpr),a

    ret

 @relocate.to.hmpr.len: equ $ - inst.buffer

    org @relocate.to.hmpr + @relocate.to.hmpr.len

;-------------------------------------------------------------------------------
@relocate.to.xmpr:

    org inst.buffer

    in a,(port.hmpr)
    push af

    ld a,high.memory.external
    out (port.hmpr),a

    ld a,h
    out (port.xmpr.c),a
    inc a
    out (port.xmpr.d),a

    ld hl,dos.sector
    ldir

    ex de,hl

    pop af
    out (port.hmpr),a

    ret

 @relocate.to.xmpr.len: equ $ - inst.buffer

    org @relocate.to.xmpr + @relocate.to.xmpr.len

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

@directory.invalid:
    ld hl,@msinvdir
    ret

;-------------------------------------------------------------------------------

@msbadfile:
    defb 13
    defm "Bad command or file name"
    defb 13,0

@msinvdir:
    defb 13
    defm "inv"
    ; defm "Invalid subdirectory"
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

   @bs.jump_instruction:            defb 0,0,0          ; 0x000
   @bs.oem_name:                    defm "01234567"     ; 0x003

  ; BIOS Parameter Block

    @bs.bytes_per_sector:           defw 0              ; 0x00b [ 512 1024 2048 4096 ]
    @bs.sectors_per_cluster:        defb 0              ; 0x00d [ 1 2 4 8 16 32 64 128 ]
    @bs.reserved_sectors:           defw 0              ; 0x00e
    @bs.fat_copies:                 defb 0              ; 0x010
    @bs.root_directory_entries:     defw 0              ; 0x011
    @bs.total_sectors:              defw 0              ; 0x013
    @bs.media_descriptor:           defb 0              ; 0x015
    @bs.sectors_per_fat:            defw 0              ; 0x016
    @bs.sectors_per_track:          defw 0              ; 0x018
    @bs.heads:                      defw 0              ; 0x01a
    @bs.hidden_sectors:             defw 0,0            ; 0x01c

    @bs.signature:                  equ @boot_sector + 510  ; 55 aa

;-------------------------------------------------------------------------------

@var.bytes_cluster:         defw 0
@var.dir_entries:           defw 0
@fat.data:                          ; points to first address after FAT

defs 512 - ( @fat.data - @boot_sector )