; an attempt at a source code mod file

	org 0

song.name:
	defm "song name           "
	
;---------------------------------------------------------------
sample.info:
	
	defm "sample one            "
	defb ( sample.1.length // 2 ) // 256 ; - in big endian WORDS
	defb ( sample.1.length // 2 ) \ 256  ; - in big endian WORDS
	defb 0		; finetune
	defb &40	; volume
	defb 0		; repeat point - in big endian in WORDS
	defb 0
	defb ( sample.1.length // 2 ) // 256	; repeat length - in big endian WORDS
	defb ( sample.1.length // 2 ) \ 256
	
	defs 30 * ( $ - sample.info )	

assert ( $ == 950 )

song.length:	defb 1
				defb 0
;---------------------------------------------------------------
song.positions:

	defb 0
	defs 127
				
	defm "M.K."

assert ( $ == 1084 )
	
;---------------------------------------------------------------
patterns:

	note.C_1:	equ 856 ; &358
	note.Cs1:	equ 808
	note.D_1:	equ 762
	note.Ds1:	equ 720
	note.E_1:	equ 678
	note.F_1:	equ 640
	note.Fs1:	equ 604
	note.G_1:	equ	570
	note.Gs1:	equ	538
	note.A_1:	equ	508
	note.As1:	equ	480
	note.B_1:	equ	453

	note.C_2:	equ 428	; &1ac
	note.Cs2:	equ 404
	note.D_2:	equ 381
	note.Ds2:	equ 360
	note.E_2:	equ 339
	note.F_2:	equ 320
	note.Fs2:	equ 302
	note.G_2:	equ	285
	note.Gs2:	equ	269
	note.A_2:	equ	254
	note.As2:	equ	240
	note.B_2:	equ	226

	note.C_3:	equ 214 ; &0d6
	note.Cs3:	equ 202
	note.D_3:	equ 190
	note.Ds3:	equ 180
	note.E_3:	equ 170
	note.F_3:	equ 160
	note.Fs3:	equ 151
	note.G_3:	equ	143
	note.Gs3:	equ	135 ; &087
	note.A_3:	equ	127 ; &07f
	note.As3:	equ	120	; &078
	note.B_3:	equ	113	; &071
	
	note.test:	equ 341	; pitch has speed 1, fraction 0
	

row.1:

	; SPPPSECC S = sample, p = period, e = effect, c = command

	@channel.1:	
		defb note.test // 256	; H  = upper sample, L upper note
		defb note.test \ 256	; HL = note
		defb &1C				; H  = lower sample, L = effect
		defb &40				; HL = command
	@channel.2:
		defb note.test // 256	; H  = upper sample, L upper note
		defb note.test \ 256	; HL = note
		defb &1C				; H  = lower sample, L = effect
		defb &40				; HL = command
	@channel.3:
		defb note.test // 256	; H  = upper sample, L upper note
		defb note.test \ 256	; HL = note
		defb &1C				; H  = lower sample, L = effect
		defb &40				; HL = command
	@channel.4:
		defb note.test // 256	; H  = upper sample, L upper note
		defb note.test \ 256	; HL = note
		defb &1C				; H  = lower sample, L = effect
		defb &40				; HL = command

row.2:

	@channel.1:	
		defb &00		; H  = upper sample, L upper note
		defb &00		; HL = note
		defb &00		; H  = lower sample, L = effect
		defb &00		; HL = command
	@channel.2:
		defb &00
		defb &00
		defb &0D		; pattern jump
		defb &00		; to row 0
	@channel.3:
		defb &00
		defb &00
		defb &00
		defb &00
	@channel.4:
		defb &00
		defb &00
		defb &00
		defb &00

row.63:

	defs 62 * 16


assert ( ( $ - patterns ) \ 1024 == 0 )
	
;---------------------------------------------------------------
samples:

sample.1:

	defb &ff,&ff,&00,&00,&80,&80

sample.1.length: equ $ - sample.1		