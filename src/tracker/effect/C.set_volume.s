 ;---------------------------------------------------------------
 ; Effect C - Set Volume
 ;---------------------------------------------------------------

 ; https://www.un4seen.com/forum/?topic=14471.msg101020#msg101020
 ; SoundTracker sends volume directly to Paula, which means:
 ; - bit 7 ignored
 ; - bit 6 max volume

 ; -> move all bounds checking to setup mod?

 r1.112:
    ld a,(parameter)
    cp 0x40
    jr c,$+4
    ld a,0x3f
 r1.113:
    ld (volume),a
 r1.114:
    jp bp.volume
