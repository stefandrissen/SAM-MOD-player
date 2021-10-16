; samdac / eddac on printer port 1 or 2

;-------------------------------------------------------------------------------

include "../ports/printer.i"

;-------------------------------------------------------------------------------

properties.samdac.1:

    defw 0,0                    ; init
    defw @out,@out.len
    defw port.printer_1.data    ; bc
    defb 1                      ; e - strobe
    defb 0                      ; d - strobe
    defw @timing.ram
    defw @timing.megabyte
    defb 7                      ; bits per channel

properties.samdac.2:

    defw 0,0
    defw @out,@out.len
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

@timing.ram:

    defb  32,122,122,122,122,122,122,122    ;   0
    defb 122,122,122,122,122,122,122,122    ;   8
    defb 122,122,122,122,122,122,122,122    ;  16
    defb 122,122,122,122,122,122,122,122    ;  24
    defb 122,122,122,122,122,122,122,122    ;  32
    defb 122,122,122,122,122,122,122,122    ;  40
    defb 122,122,122,122,122,123,124,122    ;  48
    defb 122,122,122,122,122,122,122,122    ;  56
    defb 122,122,122,122,122,122,122,122    ;  64
    defb 122,122,122,122,122,122,122,123    ;  72
    defb 109, 80, 80, 81, 81, 78, 80, 78    ;  80
    defb  78, 81, 82, 79, 79, 78, 78, 80    ;  88
    defb  80, 81, 81, 78, 80, 78, 78, 81    ;  96
    defb  82, 79, 79, 78, 78, 80, 80, 81    ; 104
    defb  81, 78, 80, 78, 78, 81, 82, 79    ; 112
    defb  81, 92, 97                        ; 120
    defb 0,-1,-1,-1,-1,-1

    defb  83

    assert $ - @timing.ram == 130

;-------------------------------------------------------------------------------

@timing.megabyte:   ; !!! only recalibrated samples on 0, need to recalibrate 1.5

    ;      0|1.5      scan line 0 (bpio e9 w -> break after strobe out)
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
    defb 122,122    ;  27 + 3
    defb 127,122    ;  30 + 3 <-
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
    defb 125,109    ;  66 + 3
    defb  86, 80    ;  69 + 3
    defb  82, 81    ;  72 + 3
    defb  84, 80    ;  75 + 3
    defb  90, 78    ;  78 + 3
    defb  87, 82    ;  81 + 3
    defb  83, 79    ;  84 + 3
    defb  83, 78    ;  87 + 3
    defb  87, 80    ;  90 + 3
    defb  88, 81    ;  93 + 3
    defb  85, 80    ;  96 + 3
    defb  87, 78    ;  99 + 3
    defb  84, 82    ; 102 + 3
    defb  82, 79    ; 105 + 3
    defb  89, 78    ; 108 + 3
    defb  86, 80    ; 111 + 3
    defb  87, 81    ; 114 + 3
    defb  80, 80    ; 117 + 3
    defb  83, 78    ; 120 + 3
    defb  91, 82    ; 123 + 3
    defb  83, 81    ; 126 + 3
    defb 0

    defb -1,-1,-1,-1,-1,-1,-1

    defb 83         ; t-states to 1.5 from line interrupt

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------
