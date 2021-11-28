show.help:

 ; F1

    ld a,-1
    ld (int.rtn.pag),a

    call cls

    ld de,@text.help
    ld ix,@text.help.colours

    jp print.screen

;------------------------------------------------------------------------------

@text.help:

    defm "SAM MOD player             "
    include "../constants/text.version.i"
    include "../constants/text.copyright.i"
    defb 0,0
    defm "           HELP PAGE"
    defb 0,0
    defm "* F1: help page, F2: list names "
    defm "  F3: list sizes, F4: tracker,  "
    defm "  F5: summary effects (column 3)"
    defm "  F6: techy page - burst info"
    defb 0,0
    defm "* 1, 2, 3, 4: un/mute channel"
    defb 0,0
    defm "* P: pause/play"
    defb 0,0
    defm "* Cursors: rewind/fast forward"
    defb 0,0
    defm "* ESC: stop tune, load another"
    defb 0,0
    defm "* L: loop tune on/off"
    defb 0,0
    defm "* On F2/F3 screen, the first    "
    defm "  number is the sample length   "
    defm "  in words, the second number is"
    defm "  the default volume."
    defb 0,0
    defm "* Track-mode skips rows if the  "
    defm "  song speed is too fast."
    defb 0,0
    defm "* The three digits in the top   "
    defm "  right corner of the screen are"
    defm "  song position, pattern number "
    defm "  and pattern row."
    defb 0

@text.help.colours:

       ; # colour
    defb 3,2
    defb 2,4
    defb 5,1
    defb 2,3
    defb 2,1
    defb 2,3
    defb 2,1
    defb 2,3
    defb 5,1
    defb 3,3
    defb 4,1
