; small source for sanity checking 

autoexec
dump 1,0
org 32768

ld a,127 // 256	; -> 0
ld a,128 // 256	; -> 0

ld a,127 / 256	; -> 0
ld a,128 / 256	; -> 1

ret
