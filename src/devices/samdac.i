; samdac / eddac on printer port 1 or 2

;-------------------------------------------------------------------------------

include "../ports/printer.i"

;-------------------------------------------------------------------------------

properties.samdac.1:

    defw 0                      ; init
    defb 0
    defw @out
    defb @out.len
    defw @buffer.init
    defb @buffer.init.len
    defw port.printer_1.data    ; bc
    defb 1                      ; e - strobe
    defb 0                      ; d - strobe
    defw @timing.ram
    defw @timing.megabyte
    defb 7                      ; bits per channel

properties.samdac.2:

    defw 0
    defb 0
    defw @out
    defb @out.len
    defw @buffer.init
    defb @buffer.init.len
    defw port.printer_2.data
    defb 1
    defb 0
    defw @timing.ram
    defw @timing.megabyte
    defb 7

;-------------------------------------------------------------------------------

@out:

    ;             t-states
    ;         border | screen
    outi        ; 20 | 24       data: sample out (left)
    inc c       ;  4 |  8
    out (c),e   ; 12 | 16       strobe: 1
    dec c       ;  4 |  8
    outi        ; 20 | 24       data: sample out (right)
    inc c       ;  4 |  8
    out (c),d   ; 12 | 16       strobe: 0
    dec c       ;  4 |  8

    @out.len: equ $ - @out

;-------------------------------------------------------------------------------

@buffer.init:
    ld c,2                      ; buffers
    ld hl,bp.audio_buffer.1

    @buffers:

        ld b,0x80               ; amplitude zero
        ld a,0

        @loop:

            ld (hl),a           ; left
            inc hl
            ld (hl),a           ; right
            inc hl
            inc a
            djnz @-loop

        ld b,bp.audio_buffer.bytes - 0x80

        @loop:

            ld (hl),a           ; left
            inc hl
            ld (hl),a           ; right
            inc hl
            djnz @-loop

        dec c
        jr nz,@-buffers

@buffer.init.len: equ $ - @buffer.init

;-------------------------------------------------------------------------------

@timing.ram:

    ; bpio e9 if peek(pc+1)==ed and dline >= 0n258

    defb      34    ; 258:383 260:195
    defb 122,122    ; 262:003 263:195
    defb 122,122    ; 265:003 266:195
    defb 122,122    ; 268:003 269:195
    defb 122,122    ; 271:003 272:195
    defb 122,122    ; 274:003 275:195
    defb 122,122    ; 277:003 278:195
    defb 122,122    ; 280:003 281:195
    defb 122,122    ; 283:003 284:195
    defb 122,122    ; 286:003 287:195
    defb 122,122    ; 289:003 290:195
    defb 122,122    ; 292:003 293:195
    defb 122,122    ; 295:003 296:195
    defb 123,122    ; 298:003 299:195
    defb 122,122    ; 301:003 302:195
    defb 122,122    ; 304:003 305:195
    defb 122,122    ; 307:003 308:195
    defb 122,122    ; 310:003 311:195
    defb 122,122    ; 001:003 002:195
    defb 122,122    ; 004:003 005:195
    defb 122,122    ; 007:003 008:195
    defb 122,122    ; 010:003 011:195
    defb 122,122    ; 013:003 014:195
    defb 122,122    ; 016:003 017:195
    defb 122,122    ; 019:003 020:195
    defb 122,122    ; 022:003 023:195
    defb 122,122    ; 025:003 026:195
    defb 123,124    ; 028:003 029:195
    defb 122,122    ; 031:003 032:195
    defb 122,122    ; 034:003 035:195
    defb 122,122    ; 037:003 038:195
    defb 122,122    ; 040:003 041:195
    defb 122,122    ; 043:003 044:195
    defb 122,122    ; 046:003 047:195
    defb 122,122    ; 049:003 050:195
    defb 122,122    ; 052:003 053:195
    defb 122,122    ; 055:003 056:195
    defb 122,122    ; 058:003 059:195
    defb 122,122    ; 061:003 062:195
    defb 122,122    ; 064:003 065:195
    defb 123,108    ; 067:003 068:195
    defb  81, 79    ; 070:003 071:195
    defb  82, 80    ; 073:003 074:195
    defb  79, 78    ; 076:003 077:195
    defb  80, 77    ; 079:003 080:195
    defb  82, 81    ; 082:003 083:195
    defb  81, 79    ; 085:003 086:195
    defb  79, 77    ; 088:003 089:195
    defb  81, 79    ; 091:003 092:195
    defb  82, 80    ; 094:003 095:195
    defb  79, 78    ; 097:003 098:195
    defb  80, 77    ; 100:003 101:195
    defb  82, 81    ; 103:003 104:195
    defb  81, 79    ; 106:003 107:195
    defb  79, 77    ; 109:003 110:195
    defb  81, 79    ; 112:003 113:195
    defb  82, 80    ; 115:003 116:195
    defb  79, 78    ; 118:003 119:195
    defb  80, 77    ; 121:003 122:195
    defb  82, 81    ; 124:003 125:195
    defb  81, 81    ; 127:003 128:195
    defb 0

    defb -1,-1,-1,-1,-1,-1,-1

    defb  82

    assert $ - @timing.ram == 130

;-------------------------------------------------------------------------------

@timing.megabyte:

    ; 384 t-states per scan line, + 3 + 1.5 * 384 = 579 -> ++ 195

    ;      0|1.5      scan line 0 (bpio e9 w -> break after strobe out)
    ;                              bpio e9 w if dline >= 0n69 and dline <= 0n100
    ;                     + = on next scan line
    defb      35    ; 258 first sample from line interrupt line 191
    defb 122,122    ; 261 + 3
    defb 122,122    ; 264 + 3
    defb 122,122    ; 267 + 3
    defb 122,122    ; 270 + 3
    defb 122,122    ; 273 + 3
    defb 122,122    ; 276 + 3
    defb 122,122    ; 279 + 3
    defb 122,122    ; 282 + 3
    defb 122,122    ; 285 + 3
    defb 122,122    ; 288 + 3
    defb 122,122    ; 291 + 3
    defb 122,122    ; 294 + 3
    defb 125,122    ; 297 + 3 <- changing page
    defb 122,122    ; 300 + 3
    defb 122,122    ; 303 + 3
    defb 122,122    ; 306 + 3
    defb 122,122    ; 309 + 3
    defb 122,122    ;   0 + 3
    defb 122,122    ;   3 + 3
    defb 122,122    ;   6 + 3
    defb 122,122    ;   9 + 3
    defb 122,122    ;  12 + 3
    defb 122,122    ;  15 + 3
    defb 122,122    ;  18 + 3
    defb 122,122    ;  21 + 3
    defb 122,122    ;  24 + 3
    defb 122,127    ;  27 + 3 <-
    defb 122,122    ;  30 + 3
    defb 122,122    ;  33 + 3
    defb 122,122    ;  36 + 3
    defb 122,122    ;  39 + 3
    defb 122,122    ;  42 + 3
    defb 122,122    ;  45 + 3
    defb 122,122    ;  48 + 3
    defb 122,122    ;  51 + 3
    defb 122,122    ;  54 + 3
    defb 122,122    ;  57 + 3
    defb 122,122    ;  60 + 3
    defb 122,122    ;  63 + 3
    defb 125,111    ;  66 + 3
    defb  85, 81    ;  69 + 3 <
    defb  80, 79    ;  72 + 3
    defb  84, 85    ;  75 + 3
    defb  82, 80    ;  78 + 3
    defb  85, 84    ;  81 + 3
    defb  85, 82    ;  84 + 3
    defb  83, 80    ;  87 + 3
    defb  85, 83    ;  90 + 3
    defb  80, 81    ;  93 + 3
    defb  84, 84    ;  96 + 3
    defb  85, 82    ;  99 + 3
    defb  83, 80    ; 102 + 3
    defb  85, 83    ; 105 + 3
    defb  80, 81    ; 108 + 3
    defb  84, 84    ; 111 + 3
    defb  85, 82    ; 114 + 3
    defb  83, 80    ; 117 + 3
    defb  85, 83    ; 120 + 3
    defb  80, 81    ; 123 + 3
    defb  82, 83    ; 126 + 3
    defb 0

    defb -1,-1,-1,-1,-1,-1,-1

    defb 83         ; t-states to 1.5 from line interrupt

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------
