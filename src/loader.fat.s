; SAM MOD player - loader: fat (aka ms-dos / pc-dos)

; https://en.wikipedia.org/wiki/Design_of_the_FAT_file_system
; https://en.wikipedia.org/wiki/File_Allocation_Table#FAT12

; to do:
; - fix msdos

;(C) 2019-2021 Stefan Drissen

;---------------------------------------------------------------
read.directory.fat:

    ld hl,(fat.data)

@loop.directory.entries:

    ld a,(hl)
    or a
    ret z

    cp 229
    jp z,@next.file    ; deleted file

    push hl
    pop ix
    ld a,(ix+11)
    and 8
    jp nz,@next.file   ; volume label

    ld a,(ix+8)
    cp "M"
    jp nz,@next.file
    ld a,(ix+9)
    cp "O"
    jp nz,@next.file
    ld a,(ix+10)
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

    ld e,(ix+26)
    ld d,(ix+27)        ; first cluster

    ld hl,dos.sector
pc.rd.more:
    call fat.read_cluster
    call fat.get_entry
    ld a,h
    cp ( temp.spc + 1084 ) / 256 + 1
    jr c,pc.rd.more

    ld hl,temp.spc+1083
    ld de,temp.spc+1083
    ld bc,1084
    lddr

    pop de

    ld a,2
    call file.check

    push de

    ld e,(ix+28)
    ld d,(ix+29)
    ld a,(ix+30)
    ex de,hl
pc.resub:
    or a
    sbc hl,de
    sbc c
    jr nc,pc.got.maxmin
    adc c
    add hl,de
    ex de,hl
    ld b,a
    ld a,c
    ld c,b
    jr pc.resub
pc.got.maxmin:          ; ahl = difference calc len & file len
    pop de
    or h
    jr z,pc.file.ok

    pop de
    pop hl
    jr @next.file

pc.file.ok:

;get date
    ld a,(ix+24)
    ld b,a
    and %00011111       ; day
    call cnv.a.to.de
    ld a,b
    and %11100000
    rlca
    rlca
    rlca
    ld c,a
    ld a,(ix+25)
    ld b,a
    and %00000001
    rlca
    rlca
    rlca
    or c                ; month
    call cnv.a.to.de
    ld a,b
    and %11111110
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
    ld bc,load.len
    add hl,bc
    ex de,hl
    pop hl

@next.file:

    ld bc,32
    add hl,bc
    ld a,(loader.entries)
    cp 27
    ret z

    jp @-loop.directory.entries

;---------------------------------------------------------------
fat.read.dir:

    ld (save.sam.sp+1),sp   ; for quick exits

    call fat.readroot

    xor a
    ld (m.vollabel),a

    ld a,(fat.dir_entries)
    ld hl,(fat.data)
    ld de,11
    add hl,de
    ld de,32
    ld b,a
prfindlabel:
    ld a,(hl)
    and 8
    jr nz,prlabel
    add hl,de
    djnz prfindlabel
prlabel:
    jr z,prnolabel
    ld de,11
    xor a
    sbc hl,de
    ld b,11
    ld de,m.vollabel
prlabblp:
    ld a,(hl)
    ld (de),a
    inc hl
    inc de
    djnz prlabblp
prnolabel:
prdonelabel:
    call fat.copy_path
    call fat.load_path
    call c,fat.reset_path

    ret

;---------------------------------------------------------------
fat.readroot:

; read root directory
;   hl = address
;---------------------------------------------------------------

    call fat.read_boot_sector
    call fat.read_fat
    call fat.startcluster
    push de
    call fat.startroot
    pop hl
    xor a
    sbc hl,de
    ld b,l
    push bc
    call fat.startroot
    pop bc
    ld hl,(fat.data)
rdrtlp:
    call fat.rdlogsec
    inc de
    djnz rdrtlp
    ld a,1
    or a
    ret

;---------------------------------------------------------------
fat.reset_path:

    ld hl,(fat.path)
    ld (hl),"\"
    inc hl
    ld (hl),0
    call fat.copy_path
    jp fat.readroot

;---------------------------------------------------------------
fat.copy_path:

    push hl
    push de
    push bc
    ld hl,(fat.path)
    ld de,fat.path_temp
    ld bc,64
    ldir
    pop bc
    pop de
    pop hl
    ret

;---------------------------------------------------------------
fat.load_path:

    ld a,(fat.path_temp+1)
    or a
    ret z
    ld hl,fat.path_temp
    ld (fat.parlast),hl
    call fat.getparameter
lploop:
    ld a,(fat.parameter)
    or a
    ret z
    call getinputfile

    push hl
    ld hl,(fat.data)
    ld bc,(fat.dir_entries)
lpmatchlp:
    ld de,fat.parafile
    push hl
    push bc
    ld b,11
lpmatchblp:
    ld a,(de)
    cp (hl)
    jr nz,lpnomatch
    inc hl
    inc de
    djnz lpmatchblp
    pop bc
    pop ix
    ld a,(ix+11)
    and %00010000
    jr nz,lpisdir
    push ix
    push bc
    jp lpnomatch
lpisdir:
    ld e,(ix+26)
    ld d,(ix+27)

    ld hl,(fat.data)
    ld bc,0
lpreadmore:
    push bc
    call fat.read_cluster
    pop bc
    inc bc
    call fat.get_entry
    ld a,d
    cp 15
    jr nz,lpreadmore
    ld a,e
    cp 0xf8
    jr c,lpreadmore
    ld hl,(fat.bytes_cluster)
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
lpaddlp:
    add hl,de
    dec bc
    ld a,b
    or c
    jr nz,lpaddlp

    ld (fat.dir_entries),hl
    pop hl
    jp lploop
lpnomatch:
    pop bc
    pop hl
    ld de,32
    add hl,de
    dec bc
    ld a,b
    or c
    jr nz,lpmatchlp
    pop hl
    scf
    ret

;---------------------------------------------------------------
fat.get_input_path:

    call fat.copy_path
    ld hl,fat.parameter
    ld de,fat.path_temp
    ld a,(hl)
    cp "\"
    jr z,gipnewpath
    ld a,(de)
    or a
    jr z,gipnewpath
gipfindend:
    inc de
    ld a,(de)
    or a
    jr nz,gipfindend
    ld a,e
    cp ( fat.path_temp + 1 ) \ 256
    jr nz,gipnewpath
    dec de
gipnewpath:
    ld a,"\"
    ld (de),a
    inc de

    ld a,(fat.parameter)
    cp "\"
    jr nz,gipns
    ld a,(fat.parameter+1)
    or a
    jr nz,gipns
    push de
    call fat.getparameter
    pop de
    ld a,(fat.parameter)
gipns:
    cp "."
    jr nz,gipnotdot
    push de
    call fat.getparameter
    pop de
    ld a,(fat.parameter)
    cp "."
    jr nz,gipfndlstp
    dec de
gipfndlstp:
    dec de
    ld a,(de)
    cp "\"
    jr nz,gipfndlstp
    ld a,e
    cp fat.path_temp \ 256
    jr nz,$+3
    inc de
    xor a
    ld (de),a

    push de
    call fat.getparameter
    pop de
    jp gipdoneext
gipnotdot:
    push de
    call getinputfile
    pop de
    jp c,fat.invaliddir

    ld hl,fat.parafile
    ld b,8
gipcopyname:
    ld a,(hl)
    cp " "
    jr z,gipdonename
    ld (de),a
    inc hl
    inc de
    djnz gipcopyname
gipdonename:
    ld hl,fat.parafile+8
    ld a,(hl)
    cp " "
    jr z,gipdoneext
    ld a,"."
    ld (de),a
    inc de
    ld b,3
gipcopyext:
    ld a,(hl)
    cp " "
    jr z,gipdoneext
    ld (de),a
    inc hl
    inc de
    djnz gipcopyext
gipdoneext:
    ld a,(fat.parameter)
    cp "\"
    jr z,gipnewpath
gipend:
    xor a
    ld (de),a
    ret

;---------------------------------------------------------------
getinputfile:

    ld hl,fat.parafile
    ld b,11
clearpf:
    ld (hl)," "
    inc hl
    djnz clearpf

    ld hl,fat.parameter
    ld de,fat.parafile
    ld a,(hl)
    cp "."
    jr z,gifextonly
    ld b,9
gifcopynm:
    ld a,(hl)
    inc hl
    or a
    jr z,gifendname
    cp "\"
    jr z,gifcopynm
    ld (de),a
    inc de
    djnz gifcopynm
    scf                 ; file longer than 8 chars
    ret
gifendname:
    call fat.getparameter
    jr z,gifendext

    ld hl,fat.parameter
    ld a,(hl)
    cp "."
    jr nz,gifendext
gifextonly:
    inc hl
    ld de,fat.parafile+8
    ld b,4
gifcopyext:
    ld a,(hl)
    or a
    jr z,gifendext
    ld (de),a
    inc hl
    inc de
    djnz gifcopyext
    call fat.getparameter
    scf
    ret             ; extension longer than 3 chars
gifendext:
    xor a
    ret

;---------------------------------------------------------------
fat.read_boot_sector:

; read boot sector from disc at fixed address
;---------------------------------------------------------------

    ld de,0x0001
    ld hl,fat.boot_sector

    in a,(port.hmpr)
    and high.memory.page.mask
    ld c,a

    call bdos.read.sector
    ld hl,(bstotsecs)
    ld de,(bssecstrack)
    ld a,d
    or e
    jr z,notpcdisc
    xor a
    ld bc,0
rblp:
    inc bc
    sbc hl,de
    jr nc,rblp
    dec bc
    add hl,de
    ld a,h
    or l
    jr nz,notpcdisc
    ld h,b
    ld l,c
    ld de,(bssides)
    ld a,d
    or e
    jr z,notpcdisc
    xor a
rblp2:
    sbc hl,de
    jr nc,rblp2
    add hl,de
    ld a,h
    or l
    jr nz,notpcdisc

    ld hl,0
    ld bc,(bssecsize)
    ld a,(bsclusize)
rbcalcsz:
    add hl,bc
    dec a
    jr nz,rbcalcsz
    ld (fat.bytes_cluster),hl
    ld hl,(bsrootentries)
    ld (fat.dir_entries),hl

    ld ix,black.attributes
    ld a,6
    jp set.attributes

notpcdisc:
    xor a
    ld (msdos+1),a
save.sam.sp:
    ld sp,0
    ret


;---------------------------------------------------------------
fat.read_fat:

; read FAT at fixed address, (data) -> first address after FAT
;---------------------------------------------------------------

    push hl

    ld de,1
    ld hl,fat
    ld a,(bssecsfat)
    ld b,a
rfblp:
    call fat.rdlogsec
    inc de
    djnz rfblp
    ld (fat.data),hl

    pop hl

    ret

;---------------------------------------------------------------
fat.read_cluster:

; read cluster from disc
;   de = cluster number (2-711)
;   hl = address
;---------------------------------------------------------------

    push de
    push hl
    dec de
    dec de
    ld hl,0
    ld a,(bsclusize)
    ld b,a
rccalc:
    add hl,de
    djnz rccalc
    call fat.startcluster
    add hl,de
    ex de,hl
    pop hl

    ld a,(bsclusize)
    ld b,a
rcclusrep:
    call fat.rdlogsec
    inc de
    djnz rcclusrep
    pop de
    ret

;---------------------------------------------------------------
fat.startroot:

; calculate start sector of root directory
; returns de with
;---------------------------------------------------------------

    ld de,1
    ld a,(bsnumfats)
    ld b,a
rcfats:
    ld a,(bssecsfat)
    add a,e
    ld e,a
    djnz rcfats
    ret

;---------------------------------------------------------------
calcclusters:

; calculate total number of clusters on disc
;---------------------------------------------------------------

    ld hl,(bstotsecs)
    call fat.startcluster
    xor a
    sbc hl,de
    ld a,(bsclusize)
ccdiv:
    srl a
    jr z,ccdonediv
    srl h
    rr l
    jr ccdiv
ccdonediv:
    ld c,l
    ld b,h
    ret

;---------------------------------------------------------------
fat.startcluster:

; calculate start sector of cluster 2 (first data cluster)
; returns de with logical sector of first data cluster
;---------------------------------------------------------------

    push hl
    ld hl,(bsrootentries)
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl   ; * 32 bytes per entry
    ld de,0
    ld bc,(bssecsize)
    xor a
scdivsecs:
    sbc hl,bc
    inc e
    jr nc,scdivsecs
    dec e
    ex de,hl
    call fat.startroot
    add hl,de
    ex de,hl
    pop hl
    ret

;---------------------------------------------------------------
fat.get_entry:

; get FAT entry
;   de = cluster
;---------------------------------------------------------------

    push hl
    ld h,d
    ld l,e
    add hl,hl
    add hl,de

    ld de,fat
    srl h
    rr l
    jr c,oddfat

    add hl,de
    ld e,(hl)
    inc hl
    ld a,(hl)
    and 15
    ld d,a
    pop hl
    ret

oddfat:
    add hl,de
    ld a,(hl)
    rrca
    rrca
    rrca
    rrca
    and 15
    ld e,a
    inc hl
    ld a,(hl)
    ld d,a
    rlca
    rlca
    rlca
    rlca
    and 240
    or e
    ld e,a
    srl d
    srl d
    srl d
    srl d
    pop hl
    ret

;---------------------------------------------------------------
fat.rdlogsec:

; read logical sector from disc
;   de = sector number (0 - 1439 (for 720k disc))
;   hl = address
;---------------------------------------------------------------

    push bc
    push de
    push hl
    ex de,hl
    ld bc,(bssecstrack) ;number of secs per track
    ld de,0
    xor a
rlsdiv9:
    sbc hl,bc
    inc d
    jr nc,rlsdiv9
    adc hl,bc
    dec d
    ld e,l
    ld a,(bssides)
    cp 2
    jr nz,$+4
    rrc d
    pop hl

    in a,(port.hmpr)
    and high.memory.page.mask
    ld c,a

    call bdos.read.sector
    pop de
    pop bc
    ret

;---------------------------------------------------------------
fat.chdir:

;---------------------------------------------------------------

    call fat.getparameter
    jp z,fat.badcommand

    call fat.get_input_path
    call fat.readroot
    call fat.load_path
    jp c,fat.invaliddir

    ld hl,fat.path_temp
    ld de,(fat.path)
    ld bc,64
    ldir

    ret

;---------------------------------------------------------------
fat.getparameter:

;---------------------------------------------------------------

    ld hl,(fat.parlast)
    ld de,fat.parameter
getparlp2:
    ld a,(hl)
    ld (de),a
    or a
    ret z
    inc hl
    cp " "
    jr z,getparlp2

getparlp:
    ld (de),a
    inc de
    ld a,(hl)
    inc hl
    or a
    jr z,gpnomore
    cp " "
    jr z,gpnomore
    cp "\"
    jr z,gpnomore
    cp "."
    jr z,gpnomore
    jr getparlp
gpnomore:
    dec hl
    ld (fat.parlast),hl
    xor a
    ld (de),a
    dec a

    ret

;---------------------------------------------------------------

fat.badcommand:
    ld hl,fat.msbadfile
    ret

fat.badfilename:
    ld hl,fat.msbadname
    ret

fat.filenotfound:
    ld hl,fat.msfilenot
    ret

fat.invaliddir:
    ld hl,fat.msinvdir
    ret

;---------------------------------------------------------------

fat.msbadfile:
    defb 13
    defm "Bad command or file name"
    defb 13,0

fat.msbadname:
    defb 13
    defm "Invalid file name"
    defb 13,0

fat.msfilenot:
    defb 13
    defm "File not found"
    defb 13,0

fat.msinvdir:
    defb 13
    defm "Invalid subdirectory"
    defb 13,0


fat.parlast:        defw 0

fat.parameter:      defs 255

fat.parafile:       defs 11

fat.matchfile:      defs 11

fat.path:           defw fat.path_a

fat.path_a:
    defb "\",0
    defs 63
    defb 0

fat.path_b:
    defb "\",0
    defs 63
    defb 0

fat.path_temp:
    defb "\"
    defs 64


;---------------------------------------------------------------

fat.boot_sector:    equ dos.sector
    bssysid:            equ fat.boot_sector +  3
    bssecsize:          equ fat.boot_sector + 11
    bsclusize:          equ fat.boot_sector + 13
    bsressec:           equ fat.boot_sector + 14
    bsnumfats:          equ fat.boot_sector + 16
    bsrootentries:      equ fat.boot_sector + 17
    bstotsecs:          equ fat.boot_sector + 19
    bsformatid:         equ fat.boot_sector + 21
    bssecsfat:          equ fat.boot_sector + 22
    bssecstrack:        equ fat.boot_sector + 24
    bssides:            equ fat.boot_sector + 26
    bshiddensecs:       equ fat.boot_sector + 28
    bsbigtot:           equ fat.boot_sector + 32
    bsphysdrv:          equ fat.boot_sector + 36
    bsvolserial:        equ fat.boot_sector + 39
    bsvolname:          equ fat.boot_sector + 43
fat.bytes_cluster:  equ fat.boot_sector + 54
fat.dir_entries:    equ fat.boot_sector + 56
fat.data:           equ fat.boot_sector + 58    ; points to first address after FAT
