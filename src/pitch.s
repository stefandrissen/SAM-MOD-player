;---------------------------------------------------------------
 ; populate pitch table for Amiga -> SAM Coupé sample rate

 ; SAM Coupé 50.08 Hz * 208 bytes per frame = 10416.64 Hz

 ; Amiga PAL
 ; - clock = 28.37516 MHz                 - divided by 4 = 7.093790 MHz
 ; - PAL colour carrier frequency = 4.43361875 MHz * 1.6 = 7.093790 MHz

 ; Amiga NTSC
 ; - clock = 28.63636 MHz                 - divided by 4 = 7.159090 MHz
 ; - NTSC color carrier frequency = 3.579545 MHz   * 2.0 = 7.159090 MHz

 ; Amiga: frequency / ( 2 * period ) = sample bytes per second

 ; SAM sample playback speed (PAL Amiga):

 ; note period  speed (byte / fraction)
 ; C-1  856     0 102
 ; C#1  808     0 108
 ; D-1  762     0 114
 ; D#1  720     0 121
 ; E-1  678     0 129
 ; F-1  640     0 136
 ; F#1  604     0 144
 ; G-1  570     0 153
 ; G#1  538     0 162
 ; A-1  508     0 172
 ; A#1  480     0 182
 ; B-1  453     0 192

 ; C-2  428     0 204
 ; C#2  404     0 216
 ; D-2  381     0 229
 ; D#2  360     0 242
 ;      341     1   0
 ; E-2  339     1   1
 ; F-2  320     1  16
 ; F#2  302     1  33
 ; G-2  285     1  50
 ; G#2  269     1  68
 ; A-2  254     1  87
 ; A#2  240     1 107
 ; B-2  226     1 130

 ; C-3  214     1 151
 ; C#3  202     1 176
 ; D-3  190     1 203
 ; D#3  180     1 228
 ; E-3  170     2   1
 ; F-3  160     2  33
 ; F#3  151     2  65
 ; G-3  143     2  98
 ; G#3  135     2 134
 ; A-3  127     2 174
 ; A#3  120     2 214
 ; B-3  113     3   3
 ;---------------------------------------------------------------

if defined( test-populate.pitch.table )

    pitch.table: equ 0x8000

    org pitch.table + 0x1000
    dump $

    autoexec

 endif

;---------------------------------------------------------------
populate.pitch.table:

    ld ix,pitch.table

    ld hl,43265     ; PAL  -> hl = ( 7093790 Hz * 256 / 10416.64 Hz ) \ 65536
    ld a,(burstplayer.amiga)
    cp amiga.pal
    jr z,@not.ntsc

    ld hl,44870     ; NTSC -> hl = ( 7159090 Hz * 256 / 10416.64 Hz ) \ 65536
 @not.ntsc:
    ld (@lo.amiga),hl

    ld bc,0x0400
    @loop:
        ld (ix),c       ; for nocalc on lowest two div
        inc ix
        djnz @-loop

 ; divide abc by de, out: abc = result, hl = remainder
 ; put result in (ix+0), (ix+1)

    ld de,0x0004    ; n = 4 to 2046 step 2 (divisor)

    @loop:
        ld hl,0

     @lo.amiga: equ $+1
        ld bc,0
        ld a,2          ; abc = value

        ex af,af'
        ld a,24

        @mp.divlp1:

            ex af,af'

            sl1 c       ; sll c
            rl b
            rla
            adc hl,hl
            sbc hl,de
            jr nc,$+4
            add hl,de
            dec c

            ex af,af'
            dec a

            jr nz,@mp.divlp1

        add hl,hl       ; hl <= 4090 (2045 * 2)
        sbc hl,de
        jr c,$+3
        inc bc
        ld (ix),c       ; (a)bc = value / n (rounded)
        ld (ix+1),b
        inc ix
        inc ix

        inc de
        inc de
        ld a,d
        cp 0x800 / 0x100

        jr nz,@-loop

    ret

;-------------------------------------------------------------------------------

if defined( test-populate.pitch.table )

    amiga.pal:  equ 0
    amiga.ntsc: equ 1

    burstplayer.amiga: defb amiga.pal

endif