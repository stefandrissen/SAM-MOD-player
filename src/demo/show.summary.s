show.summary:

 ; F5

    ld a,-1
    ld (int.rtn.pag),a

    call cls

    ld de,@text.summary
    ld ix,@text.summary.colours

    jp print.screen

;------------------------------------------------------------------------------

@text.summary:

    defm "SAM MOD player             "
    include "../constants/text.version.i"
    include "../constants/text.copyright.i"
    defb 0,0
    defm " SUMMARY OF PROTRACKER EFFECTS"
    defb 0,0
    defm "0 Arpeggio"
    defb 0
    defm "1 Portamento Up       (speed xy)"
    defm "2 Portamento Down     (speed xy)"
    defm "3 Tone Portamento     (speed xy)"
    defm "4 Vibrato (speed x, amplitude y)"
    defm "5 Tone and Volume Slide"
    defb 0
    defm "6 Vibrato and Volume Slide"
    defb 0
    defm "7 Tremolo (speed x, amplitude y)"
    defm "8 Undefined"
    defb 0
    defm "9 Sample Offset (512 bytes * xy)"
    defm "A Volume Slide    (up x, down y)"
    defm "B Position Jump          (to xy)"
    defm "C Volume Change          (to xy)"
    defm "D Pattern Break  (to row xy dec)"
    defm "E Extra effects (x=com, y=param)"
    defm "F Set Speed or Tempo if xy > 20 "
    defb 0
    defm "   EXTRA EFFECTS (E-command)"
    defb 0,0
    defm "0 filter      * 8 undefined"
    defb 0
    defm "1 fine porta up 9 retrigger note"
    defm "2 fine porta dn A volume fine up"
    defm "3 gliss control B volume fine dn"
    defm "4 vibrato cntrl C note cut"
    defb 0
    defm "5 set fine tune D note delay    "
    defm "6 jump loop     E pattern delay "
    defm "7 tremolo cntrl F no standard  *"

@text.summary.colours:

       ;  # colour
    defb  3,2
    defb  2,4
    defb 17,1
    defb  2,4
    defb  8,3
