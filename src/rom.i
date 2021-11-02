; SAM Basic ROM routines and floating point calculator defines
;
; (C) 2021 Stefan Drissen
;
; source: https://www.worldofsam.org/products/sam-coupe-technical-manual
;-------------------------------------------------------------------------------
; floating point calculator

fpc:            equ 0x28

fpc.mod:        equ 0x08 ; N1 MOD N2
fpc.onelit:     equ 0x26 ; Stack next byte on FPCS (as a number between 0x00 and 0xff).
fpc.fivelit:    equ 0x27 ; Stack next 5 bytes on FPCS (as any number).
fpc.exit:       equ 0x33 ; Finish using floating point calculator.
fpc.dvar:       equ 0x49

;-------------------------------------------------------------------------------
rom.jgetint:    equ 0x0121

; Unstack number from calculator stack into HL. BC holds a copy of HL, and A
; holds a copy of L. An error is generated if the rounded number is not in the
; range 0x0000-0xffff.

;-------------------------------------------------------------------------------
rom.jstkfetch:  equ 0x0124

; Unstack last value from calculator stack to AEDCB.
;
; If the value is a floating-point number, the 5 bytes are in CPC/Spectrum
; format.
;   A 93 1001 0011
;   E 70 0111 0000
;   D 17 0001 0111
;   C c0 1100 0000
;   B 00 0000 0000
;
; If the value is a whole number between 0x0000 and 0xffff (which may be
; negative) it may be in a special form where
; - A = 0x00 (showing special form);
; - E = SGN (0x00 = positive, 0xff = negative)
; - D and C are less and more significant bytes
;
; If the value is a string:
; - A holds the page the string text starts in
; - DE holds the start address of the text start within the page (0x8000-0xbfff)
; - BC holds the string length
;-------------------------------------------------------------------------------
