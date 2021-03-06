; SAM MOD player - dos interface

; (C) 2019 Stefan Drissen

dos.sector:		equ &4900	; 512 bytes for sector 

;===============================================================
; FILE AREAS
;---------------------------------------------------------------

uifa:			equ &4b00	; user information file area

	uifa.filetype:	equ uifa + &00
		uifa.filetype.basic:	equ &10
		uifa.filetype.code:		equ &13
		uifa.filetype.screen:	equ &14
		
	uifa.filename:	equ uifa + &01
	uifa.flags:		equ uifa + &0e

;---------------------------------------------------------------

difa:			equ &4b50	; disk information file area

	difa.length.pages:	equ difa + &22
	difa.length.bytes:	equ difa + &23

;===============================================================
; SVAR - System VARiables
;---------------------------------------------------------------

svar.dosflg:	equ &5bc2	; zero if no DOS loaded, else page number containing DOS.
svar.doser:		equ	&5bc0	; (2) If address is non-zero, DOS jumps there on exit.
svar.mode:		equ &5a40	; MODE of current screen. 0-3 for modes 1-4
svar.cuscrnp:	equ &5a78	; Current screen page. Bit 7=0, bits 6 and 5=MODE (0-3) and bits 4-0=page number. Set by SCREEN command

;===============================================================
; DVAR - Dos VARiables
;---------------------------------------------------------------

bdos.dvars:		equ &8000

dvar.border.mask:		equ 0	; Border mask 0=no border change,1-7 border changed
dvar.version:			equ 7	; Version number divided by 10 minus 10 (version 1.1 = 1)
dvar.reserved:			equ 21	; Number of reserved sectors on hard disk for BOOT sector
      							; and RECORD list. Equal to INT ((records+63)/32))
      							; May be altered to access the Record names list. This DVAR
      							; must be restored to its old value before the hard disk is
      							; used again.
dvar.records:			equ 23	; Total number of records available
dvar.record:			equ 25	; current record selected. May be DPOKEd manually to select
      							; a record. The write protect status of a record is not
      							; updated if this DVAR is DPOKEd.
dvar.record.protected:	equ	27	; Write protect status of current record. Note only updated
      							; If a record is selected using the RECORD command.

;---------------------------------------------------------------

palette.table:	equ &55d8

;===============================================================
; DOS HOOK CODES - RST 8
;---------------------------------------------------------------

dos.hgthd:	equ &81	; get file header

; Get file header. This routine should be called with IX pointing
; to the UIFA, which should contain the file type and filename
; required. When completed the complete file header will be
; transferred in DIFA form to IX+80 bytes. 

;---------------------------------------------------------------

dos.hload:	equ &82	; load file 

; Load file in UIFA pointed to by IX register. The C register
; contains the number of 16K pages used by the file, while DE
; must contain the length modulo 16K. The HL register pair must
; point to a destination between 8OOOH to BFFFH, while the
; destination page must be paged in using the HMPR register.
; These values can be obtained from the header loaded by HGTHD. 

;---------------------------------------------------------------

dos.hrecord: equ &9c ; select a record

; if A = 0 then select record number HL,
; else select record by name. HL points to the 16 char. name

;---------------------------------------------------------------

dos.hrsad:	equ &a0	; read a sector from disk

; D contains the track number, and E contains the sector number.
; The Accumulator holds the drive number (1 or 2). Reads the
; sector pointed to by the DE register pair. The Accumulator
; contains the drive number, while the HL register pair is the
; pointer to the destination.	

;	A = drive
;	D = track
;	E = sector
;	HL = memory address (16384 to 65024)

;---------------------------------------------------------------

dos.hmrsad:	equ &a2 ; read multiple sectors

;	A = drive number
;	D = track, E = sector
;	C = memory page
;	HL = memory offset (32768 to 49151)
;	IX = number of sectors

;---------------------------------------------------------------
