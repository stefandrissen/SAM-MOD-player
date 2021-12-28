; SAM MOD player - dos interface

; (C) 2019-2021 Stefan Drissen

dos.sector:     equ 0x4900  ; 512 bytes for sector

;===============================================================================
; FILE AREAS
;-------------------------------------------------------------------------------

samdos.dir:                         ; samdos directory entry as stored on disk

    samdos.dir.filetype:            equ 0x00
        samdos.filetype.basic:          equ 0x10
        samdos.filetype.code:           equ 0x13
        samdos.filetype.screen:         equ 0x14
    samdos.dir.filename:            equ 0x01 ; 10 characters
    samdos.dir.sectors:             equ 0x0b ; msb,lsb number of sectors used
    samdos.dir.track:               equ 0x0d ; track number of start of file
    samdos.dir.sector:              equ 0x0e ; sector number of start of file
    samdos.dir.sector_address_map:  equ 0x0f ; 195 bytes (1 bit per used sector)

    samdos.dir.diskname:            equ 0xd2 ; 10 characters (first entry only)

    samdos.dir.length.pages:        equ 0xef ; each page is 16384 bytes
    samdos.dir.length.bytes:        equ 0xf0 ; length modulo 16384

    samdos.dir.timestamp:           equ 0xf5
    samdos.dir.timestamp.day:       equ 0xf5
    samdos.dir.timestamp.month:     equ 0xf6
    samdos.dir.timestamp.year:      equ 0xf7
    samdos.dir.timestamp.hour:      equ 0xf8
    samdos.dir.timestamp.minute:    equ 0xf9

    samdos.dir.diskname.b_dos:      equ 0xfa ; 6 extra characters (first entry only)

;-------------------------------------------------------------------------------
uifa:           equ 0x4b00  ; user information file area

    xuifa.filetype:          equ samdos.dir.filetype
    xuifa.filename:          equ samdos.dir.filename
    xuifa.sectors:           equ samdos.dir.sectors

    xuifa.track:             equ samdos.dir.track
    xuifa.sector:            equ samdos.dir.sector

    xuifa.length.pages:      equ 0x22
    xuifa.length.bytes:      equ 0x23

;-------------------------------------------------------------------------------

difa:           equ 0x4b50  ; disk information file area

    difa.length.pages:  equ difa + 0x22
    difa.length.bytes:  equ difa + 0x23

;===============================================================================
; SVAR - System VARiables
;-------------------------------------------------------------------------------

svar.dosflg:    equ 0x5bc2  ; zero if no DOS loaded, else page number containing DOS.
svar.doser:     equ 0x5bc0  ; (2) If address is non-zero, DOS jumps there on exit.
svar.mode:      equ 0x5a40  ; MODE of current screen. 0-3 for modes 1-4
svar.cuscrnp:   equ 0x5a78  ; Current screen page. Bit 7=0, bits 6 and 5=MODE (0-3) and bits 4-0=page number. Set by SCREEN command

;===============================================================================
; DVAR - Dos VARiables
;-------------------------------------------------------------------------------

bdos.dvars:     equ 0x8000

dvar.border.mask:       equ 0   ; Border mask 0=no border change,1-7 border changed

dvar.drive_1.tracks:    equ 1   ; tracks + 0x80 if double sided
dvar.drive_2.tracks:    equ 2   ;

dvar.drive_1.step_rate: equ 3   ; 0 = 3ms, 3 = 6ms
dvar.drive_2.step_rate: equ 4   ;

dvar.version:           equ 7   ; Version number divided by 10 minus 10 (version 1.1 = 1)
    dvar.version.b_dos.min:     equ  1  ;  4 B-DOS 1.4
                                        ;  5 B-DOS 1.5
                                        ;  6 B-DOS 1.6
    dvar.version.b_dos.max:     equ  9  ;  7 B-DOS 1.7

    dvar.version.samdos.min:    equ 10  ; 13 samdos 1.3
    dvar.version.samdos.max:    equ 29  ; 20 samdos 2.0

    dvar.version.masterdos.min: equ 30  ; 33 masterdos 1.3
                                        ; 43 masterdos 2.3

dvar.reserved:          equ 21  ; Number of reserved sectors on hard disk for BOOT sector
                                ; and RECORD list. Equal to INT ((records+63)/32))
                                ; May be altered to access the Record names list. This DVAR
                                ; must be restored to its old value before the hard disk is
                                ; used again.
dvar.records:           equ 23  ; Total number of records available
dvar.record:            equ 25  ; current record selected. May be DPOKEd manually to select
                                ; a record. The write protect status of a record is not
                                ; updated if this DVAR is DPOKEd.
dvar.record.protected:  equ 27  ; Write protect status of current record. Note only updated
                                ; If a record is selected using the RECORD command.

;-------------------------------------------------------------------------------

palette.table:  equ 0x55d8

;===============================================================================
; DOS HOOK CODES - RST 8
;-------------------------------------------------------------------------------

dos.hgthd:  equ 0x81    ; get file header

; Get file header. This routine should be called with IX pointing
; to the UIFA, which should contain the file type and filename
; required. When completed the complete file header will be
; transferred in DIFA form to IX+80 bytes.

;-------------------------------------------------------------------------------

dos.hload:  equ 0x82    ; load file

; Load file in UIFA pointed to by IX register. The C register
; contains the number of 16K pages used by the file, while DE
; must contain the length modulo 16K. The HL register pair must
; point to a destination between 0x8000 to 0xbfff, while the
; destination page must be paged in using the HMPR register.
; These values can be obtained from the header loaded by HGTHD.

;-------------------------------------------------------------------------------

dos.hrecord: equ 0x9c   ; select a record

; if A = 0 then select record number HL,
; else select record by name. HL points to the 16 char. name

;-------------------------------------------------------------------------------

dos.hrsad:  equ 0xa0    ; read a sector from disk

; D contains the track number, and E contains the sector number.
; The Accumulator holds the drive number (1 or 2). Reads the
; sector pointed to by the DE register pair. The Accumulator
; contains the drive number, while the HL register pair is the
; pointer to the destination.

;   A = drive
;   D = track
;   E = sector
;   HL = memory address (0x4000 to 0xfe00)

;-------------------------------------------------------------------------------

dos.hmrsad: equ 0xa2    ; read multiple sectors

;   A = drive number
;   D = track, E = sector
;   C = memory page
;   HL = memory offset (0x8000 to 0xbfff)
;   IX = number of sectors

;-------------------------------------------------------------------------------
