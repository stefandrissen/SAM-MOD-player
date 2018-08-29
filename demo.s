
;SAM MOD player version 2.10
;DEMO routine for BURST+SEQUENCER
;(C) 1996 Stefan Drissen
;last update: 7 May 1996, 19:58

;changes in 2.10:
;+ plus & minus keys now allow volume amplification!
;  great for the good ol' SAA1099!
;* version number change

;changes in 2.05:
;* version number change & telephone number & web page
;+ C now toggles between two palette sets (set two at +75)
;- mod ascii data now terminated by a 0

;changes in 2.04:
;* version number change

;changes in 2.03:
;* version number change


ld.dev:        EQU  49152+3        ;device, set by loader

bp.page:       EQU  2
burst.player:  EQU  32768
bp.sequence:   EQU  105

get.patt:      EQU  10752
gp.ret.p:      EQU  10790          ;set with return page

far.call:      EQU  10794          ;C=set with return page!

sq.page:       EQU  4
init.seq:      EQU  32768
install.mod:   EQU  32771
sq.demo:       EQU  32774
sq.demo.p:     EQU  32776
sq.octaves:    EQU  32778

mod.page:      EQU  5


;now follow variables which are located in the burst page
;this way they can also be read by the extra routines

current.row:   EQU  256            ;16 bytes
palette.tab:   EQU  256+16         ;16 bytes
framescreen:   EQU  256+32         ; 1 byte

int.routine:   EQU  256+33
int.rtn.pag:   EQU  256+35

c1.on:         EQU  256+36
c2.on:         EQU  256+37
c3.on:         EQU  256+38
c4.on:         EQU  256+39
vol.update:    EQU  256+40

countint:      EQU  256+41
counter.fract: EQU  256+42  ;fraction part of counter
counter:       EQU  256+43  ;integer part of counter
speed:         EQU  256+44
tempo:         EQU  256+45
song.pos:      EQU  256+47
pattern.num:   EQU  256+48
pattern.pos:   EQU  256+49
enable.burst:  EQU  256+50  ;word
exit.burst:    EQU  256+52  ;word
disable.pos:   EQU  256+54  ;disable position jumps
mstatus:       EQU  256+55  ;0=playing, 1=stopped

buffer:        EQU  256+128        ;128 bytes used by far.ldir
                                   ;maximum +255

;===============================================================
;demo is the program that runs in "foreground" mode
;     the sequencer is called by the burst routine every frame

               ORG  57344
               DUMP 2,57344-49152

setup.demo:
             ; DI
test.1:
             ; XOR  A
             ; OUT  (254),A

             ; IN   A,(250)
             ; LD   (dm.lmpr+1),A
             ; LD   A,32
             ; OUT  (250),A
               JP   setupmod

               ORG  $-32768
setupmod:
               EX   AF,AF'
               LD   A,mod.page
               OUT  (251),A
               LD   HL,32768
               LD   DE,mod.header-32768
               LD   BC,1084
               LDIR

               LD   A,sq.page
               OUT  (251),A
               LD   HL,demo
               LD   (sq.demo),HL
               XOR  A
               LD   (sq.demo.p),A
               EX   AF,AF'
               LD   (sq.octaves),A

seq.setup:     LD   A,0
               OR   A
               CALL Z,init.seq
test.2:
             ; LD   A,1
             ; OUT  (254),A

               LD   A,1
               LD   (seq.setup+1),A

               CALL install.mod
test.3:
             ; LD   A,0
             ; OUT  (254),A

               LD   A,bp.page
               OUT  (251),A

               CALL burst.player
test.4:
             ; LD   A,3
             ; OUT  (254),A

             ; XOR  A
             ; OUT  (251),A
             ; JP   dm.lmpr

               RET

               ORG  $+32768

dm.lmpr:     ; LD   A,0
             ; OUT  (250),A
             ; EI
             ; RET


palette:       DEFB %0000000 ;    0

               DEFB %0011101 ;3 1 1;BLUE+green
               DEFB %1011001 ;3 2 2
               DEFB %1011101 ;3 3 3

               DEFB %0101110 ;3 1 4;RED+green
               DEFB %1101010 ;3 2 5
               DEFB %1101110 ;3 3 6

               DEFB %1001101 ;3 1 7;GREEN+blue

               DEFB %0000000 ;    8;bright background

               DEFB %1011100 ;3 2 9
             ; DEFB %1011101 ;3 3  same as pen 3

               DEFB %0101011 ;3 1 A;RED+blue
               DEFB %0111010 ;3 2 B
               DEFB %0111011 ;3 3 C

               DEFB %1001110 ;3 1 D;GREEN+red
               DEFB %1101100 ;3 2 E
             ; DEFB %1101110 ;3 3  ;same as pen 6

               DEFB %1110111 ;    F

palette.two:   DEFW 0,0,0,0,0,0,0,0

col.pattern:
               DEFB 1,3,1,1,1,4,9,1,1,4,9,1,1,5,1,2

col.samples:
               DEFB 1,3,31,1

;===============================================================
;this is the routine that is running in "foreground mode"

demo:
               LD   BC,15*256+248
               XOR  A
black:         OUT  (C),A
               DJNZ black
               OUT  (C),A

               LD   A,32
               OUT  (252),A

               CALL cls
               CALL set.palette

               LD   IX,char.list
               LD   DE,"0"-" "*5+font
               LD   HL,build.font
               LD   B,10+6
build.blp:
               LD   A,B
               CP   6
               JR   NZ,$+5
               LD   DE,"A"-" "*5+font

               LD   (IX+0),L
               LD   (IX+1),H
               INC  IX
               INC  IX
               LD   (HL),&E1       ;pop hl
               INC  HL
               LD   C,4
build.clp:
               LD   (HL),&36
               INC  HL
               LD   A,(DE)
               INC  DE
               LD   (HL),A         ;ld (hl),n
               INC  HL
               LD   (HL),&7D       ;ld a,l
               INC  HL
               LD   (HL),&80       ;add a,b
               INC  HL
               LD   (HL),&6F       ;ld l,a
               INC  HL
               DEC  C
               JR   NZ,build.clp
               LD   (HL),&36
               INC  HL
               LD   A,(DE)
               INC  DE
               LD   (HL),A         ;ld (hl),n
               INC  HL
               LD   (HL),&2C       ;inc l
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL
               DJNZ build.blp

               LD   IX,line.table
               LD   HL,32768+8192
               LD   DE,192
               LD   B,32
b.line:        LD   (IX),L
               INC  IX
               LD   (IX),H
               INC  IX
               ADD  HL,DE
               DJNZ b.line

first.time:    LD   A,0
               OR   A
               JR   NZ,skip.intro
               CPL
               LD   (int.rtn.pag),A
               LD   (first.time+1),A

               LD   IX,col.intro
               LD   DE,welcome
               CALL print.screen

               LD   A,0
               LD   (trackon+1),A

skip.intro:

               LD   A,(trackon+1)
               DEC  A
               CALL Z,show.help
               DEC  A
               CALL Z,show.samples
               DEC  A
               CALL Z,show.sizes
               DEC  A
               CALL Z,show.pattern
               DEC  A
               CALL Z,show.summary
               DEC  A
               CALL Z,show.burst

               LD   HL,(enable.burst)
               LD   (mk.enable+1),HL
mk.enable:     CALL 0

demo.loop:
trackon:       LD   A,0
               CP   2
               JR   C,skip.patpos
               CP   5
               JR   NC,skip.patpos

               LD   A,(song.pos)
               LD   B,32           ;for print routine
               LD   HL,0*256+24+32768
               AND  &F0
               CALL printhi
               LD   A,(song.pos)
               AND  &0F
               CALL printlo
               INC  L

               LD   A,(pattern.num)
               AND  &F0
               CALL printhi
               LD   A,(pattern.num)
               AND  &0F
               CALL printlo
               INC  L

               LD   A,(pattern.pos)
               AND  &F0
               CALL printhi
               LD   A,(pattern.pos)
               AND  &0F
               CALL printlo
skip.patpos:
               LD   A,(trackon+1)
               CP   4
               JR   NZ,skip.speed

               LD   HL,256+32768+18
               LD   A,(speed)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo

               LD   HL,(tempo)
               LD   B,H
               LD   C,L
               SRL  H
               RR   L
               SRL  H
               RR   L
               ADC  HL,BC
               LD   B,H
               LD   C,L
               LD   HL,256+32768+29
               CALL do.percent

skip.speed:
               LD   BC,255*256+254
               IN   C,(C)
               BIT  3,C
               JR   NZ,not.left
               LD   HL,pattern.pos
               DEC  (HL)
               DEC  (HL)
               BIT  7,(HL)
               JR   Z,wait.left
               LD   (HL),62
               DEC  L
               DEC  L
               DEC  (HL)
               BIT  7,(HL)
               JR   Z,wait.left
               LD   (HL),0
               INC  L
               INC  L
               LD   (HL),0
wait.left:     EI                  ;just in case we're pausing
               LD   A,(counter)
               OR   A
               JR   NZ,wait.left
not.left:
               BIT  4,C
               JR   NZ,not.right
               LD   A,(speed)
               LD   HL,counter
               LD   (HL),A
wait.right:    EI
               LD   A,(mstatus)    ;just in case end of tune
               DEC  A
               JP   Z,exit
               LD   A,(HL)
               DEC  A
               JR   NZ,wait.right
not.right:

               LD   BC,247*256+254 ;12345
               IN   C,(C)
               XOR  A
               BIT  0,C
               JR   NZ,not.key.1
still.key.1:   LD   A,0
               OR   A
               JR   NZ,key.2
               LD   HL,c1.on
               LD   A,(HL)
               CPL
               LD   (HL),A
               LD   A,1
               LD   (vol.update),A
not.key.1:
               LD   (still.key.1+1),A

key.2:
               XOR  A
               BIT  1,C
               JR   NZ,not.key.2
still.key.2:   LD   A,0
               OR   A
               JR   NZ,key.3
               LD   HL,c2.on
               LD   A,(HL)
               CPL
               LD   (HL),A
               LD   A,1
               LD   (vol.update),A
not.key.2:
               LD   (still.key.2+1),A

key.3:
               XOR  A
               BIT  2,C
               JR   NZ,not.key.3
still.key.3:   LD   A,0
               OR   A
               JR   NZ,key.4
               LD   HL,c3.on
               LD   A,(HL)
               CPL
               LD   (HL),A
               LD   A,1
               LD   (vol.update),A
not.key.3:
               LD   (still.key.3+1),A

key.4:
               XOR  A
               BIT  3,C
               JR   NZ,not.key.4
still.key.4:   LD   A,0
               OR   A
               JR   NZ,key.p
               LD   HL,c4.on
               LD   A,(HL)
               CPL
               LD   (HL),A
               LD   A,1
               LD   (vol.update),A
not.key.4:
               LD   (still.key.4+1),A

key.p:
               XOR  A
               LD   BC,223*256+254
               IN   C,(C)
               BIT  0,C
               JR   NZ,not.key.p
still.key.p:   LD   A,0
               OR   A
               JR   NZ,still.p
ints.on:       LD   A,0
               OR   A
               JR   Z,pause
               XOR  A
               EI
               JR   cont.p
pause:         LD   A,1
               DI
cont.p:        LD   (ints.on+1),A
               LD   A,1
not.key.p:
               LD   (still.key.p+1),A
still.p:


               LD   HL,trackon+1
               LD   A,(HL)

               LD   BC,254*256+249
               IN   C,(C)
               BIT  5,C
               JR   NZ,not.f1
               CP   1
               JR   Z,not.f1
               LD   (HL),1
               CALL show.help
               JR   skip.f
not.f1:
               BIT  6,C
               JR   NZ,not.f2
               CP   2
               JR   Z,not.f2
               LD   (HL),2
               CALL show.samples
               JR   skip.f
not.f2:
               BIT  7,C
               JR   NZ,not.f3
               CP   3
               JR   Z,not.f3
               LD   (HL),3
               CALL show.sizes
               JR   skip.f
not.f3:
               LD   BC,253*256+249
               IN   C,(C)
               BIT  5,C
               JR   NZ,not.f4
               CP   4
               JR   Z,not.f4
               LD   (HL),4
               CALL show.pattern
               JR   skip.f
not.f4:
               BIT  6,C
               JR   NZ,not.f5
               CP   5
               JR   Z,not.f5
               LD   (HL),5
               CALL show.summary
               JR   skip.f
not.f5:
               BIT  7,C
               JR   NZ,not.f6
               LD   (HL),6
               CALL show.burst
               JR   skip.f
not.f6:
skip.f:
               LD   A,%10111111
               IN   A,(254)
               CPL
               AND  %00000010
               JR   Z,not.l
still.l:       LD   A,0
               OR   A
               JR   NZ,not.l
               LD   A,(disable.pos)
               XOR  1
               LD   (disable.pos),A
               CALL pr.loop.st
               LD   A,1
not.l:
               LD   (still.l+1),A

               LD   A,%11111110
               IN   A,(254)
               CPL
               AND  %00001000
               JR   Z,not.c
still.c:       LD   A,0
               OR   A
               JR   NZ,not.c
               LD   A,(set.palette+1)
               CPL
               LD   (set.palette+1),A
               CALL set.palette
               LD   A,1
not.c:
               LD   (still.c+1),A

               LD   A,%11111110    ;shift pressed
               IN   A,(254)
               AND  %00000001
               LD   BC,64
               JR   NZ,$+5
               LD   BC,256

               LD   A,%11101111    ;+
               IN   A,(249)
               AND  %01000000
               JR   NZ,not.plus

               LD   HL,(amp.fac+1)
               ADD  HL,BC
               LD   A,H
               CP   10
               JR   Z,not.plus
               LD   (amp.fac+1),HL
               CALL tables
               CALL pr.amp.fac
not.plus:

               LD   A,%11101111    ;-
               IN   A,(249)
               AND  %00100000
               JR   NZ,not.minus
               LD   HL,(amp.fac+1)
               SBC  HL,BC
               JR   C,not.minus
               LD   (amp.fac+1),HL
               CALL tables
               CALL pr.amp.fac
not.minus:
               LD   BC,0
               LD   A,247
               IN   A,(249)        ;escape
               AND  32
               JR   Z,exit

               LD   A,(mstatus)    ;1=music stopped
               DEC  A
               JR   Z,exit

               LD   A,251
               IN   A,(249)
               AND  128
               JR   NZ,not.f9
               INC  BC             ;bc <> 0
exit:
               LD   A,(trackon+1)
               OR   A
               JR   NZ,$+3
               INC  A
               LD   (trackon+1),A

               LD   HL,(exit.burst)
               JP   (HL)
not.f9:
               LD   A,247
               IN   A,(249)
               AND  128            ;caps
               JR   NZ,not.reset2
               LD   A,255
               IN   A,(254)
               AND  1              ;control
               JR   NZ,not.reset2
               LD   A,191
               IN   A,(249)
               AND  128            ;edit
               JR   NZ,not.reset2

               DI
               XOR  A
               OUT  (252),A
still.res2:
               LD   A,247
               IN   A,(249)
               AND  128            ;caps
               JR   Z,still.res2
               LD   A,255
               IN   A,(254)
               AND  1              ;control
               JR   Z,still.res2
               LD   A,191
               IN   A,(249)
               AND  128            ;edit
               JR   Z,still.res2
reset:
               XOR  A
               OUT  (250),A
               RST  0
not.reset2:

               JP   demo.loop


set.palette:
               LD   A,0
               OR   A
               LD   HL,palette
               JR   Z,$+5
               LD   HL,palette.two

               LD   DE,palette.tab
               LD   BC,16
               LDIR
               RET

;===============================================================

;PATTERN TRACKER for MOD player
;(C) 1995 Stefan Drissen
;last update: 3 January 1994, 01:00
;
;runs off SEQUENCER frame interrupt
;only run when frame counter <> 0

printer:
               LD   HL,(counter.fract)
               LD   DE,(tempo)
               ADD  HL,DE
               LD   A,(speed)
               DEC  A
               CP   H
               RET  C

pr.set:        LD   A,0
               OR   A
               JP   Z,print43

               LD   A,(counter)
               OR   A
               RET  NZ
               LD   (pr.set+1),A
print12:
               LD   HL,256*3+3+32768
print.pos:     LD   A,0
               ADD  H
               LD   H,A

               LD   DE,current.row
               LD   B,32

               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  E
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  L

               DEC  E
               DEC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L

               INC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo

               LD   L,18

               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  E
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  L

               DEC  E
               DEC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L

               INC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               LD   HL,c1.on
               LD   D,(HL)
               INC  L
               LD   E,(HL)
               LD   A,(print.pos+1)
               ADD  128
               LD   L,1   ;width offset
               LD   C,3   ;height offset
               ADD  C
               LD   H,A
print.pointer:
               LD   A,D
               OR   A
               JR   Z,pp.skipleft

               LD   (HL),%10000000
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%11000000
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%11100000
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%11000000
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%10000000

pp.skipleft:
               LD   A,E
               OR   A
               JR   Z,pp.skiprite
               LD   L,30
               LD   (HL),%00000001
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%00000011
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%00000111
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%00000011
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),%00000001
pp.skiprite:
               LD   A,H
               SUB  C
               DEC  A
               AND  7
               ADD  C
               ADD  128
               LD   H,A
               LD   L,30
               LD   C,0
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C

               LD   L,1
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               LD   (HL),C

               RET

print43:
               INC  A
               LD   (pr.set+1),A
               LD   A,(print.pos+1)
               ADD  13+128
               LD   H,A
               LD   L,18
               LD   DE,current.row+8
               LD   B,32

               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  E
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  L

               DEC  E
               DEC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L

               INC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  E

               LD   L,3

               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  E
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               INC  L

               DEC  E
               DEC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L

               INC  E
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  L
               INC  E
               LD   A,(DE)
               AND  &F0
               CALL printhi
               LD   A,(DE)
               AND  &0F
               CALL printlo
               INC  E

               LD   HL,c4.on
               LD   D,(HL)
               DEC  L
               LD   E,(HL)
               LD   A,(print.pos+1)
               LD   C,13
               ADD  C
               ADD  128
               LD   H,A
               LD   L,1
               CALL print.pointer
               LD   HL,print.pos+1
               LD   A,(HL)
               INC  A
               AND  %10000111
               LD   (HL),A
               RET


printhi:
               RES  7,L
               RRCA
               RRCA
               RRCA
               PUSH HL
               LD   H,char.list/256
               LD   L,A
               LD   A,(HL)
               INC  L
               LD   H,(HL)
               LD   L,A
               JP   (HL)

printlo:
               RES  7,L
               RLCA
               PUSH HL
               LD   H,char.list/256
               LD   L,A
               LD   A,(HL)
               INC  L
               LD   H,(HL)
               LD   L,A
               JP   (HL)

print.num:
               LD   B,A
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               CALL pr.num.hex
               LD   A,B
               AND  &0F
pr.num.hex:
               ADD  "0"
               CP   ":"
               JR   C,$+4
               ADD  7
               JP   print.chr

;print chr$ A at HL

print.chr:
               PUSH BC
               PUSH DE
               PUSH HL
               LD   C," "
               CP   " "
               JR   C,unprintable
               CP   128
               JR   NC,unprintable
               LD   C,A
unprintable:
               EX   DE,HL
               LD   B,0
               LD   L,C
               LD   H,B
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,BC
               LD   BC,font-160    ;-" "*5
               ADD  HL,BC
               LD   B,5
pr.chr.blp:    LD   A,(HL)
               LD   (DE),A
               INC  HL
               LD   A,E
               ADD  32
               LD   E,A
               JR   NC,$+3
               INC  D
               DJNZ pr.chr.blp
               POP  HL
               POP  DE
               POP  BC
               INC  L
               RET

print.screen:
               PUSH DE
               LD   A,6
               CALL colour.scrn

               LD   HL,32768
               POP  DE
               LD   C,32
wel.all:
               LD   B,32
               PUSH HL

pr.scr.blp:    LD   A,(DE)
               INC  DE
               OR   A
               JR   Z,end.of.line
               CALL print.chr
               DJNZ pr.scr.blp
end.of.line:
               POP  HL
               LD   A,L
               ADD  192
               LD   L,A
               JR   NC,$+3
               INC  H
               DEC  C
               JR   NZ,wel.all
               LD   A,255
               RET

show.help:
               LD   A,255
               LD   (int.rtn.pag),A
               CALL cls
               LD   IX,col.help
               LD   DE,help.page
               JP   print.screen

show.summary:
               LD   A,255
               LD   (int.rtn.pag),A
               CALL cls
               LD   IX,col.pro
               LD   DE,prosummary
               JP   print.screen

show.burst:
               LD   HL,(int.routine)
               LD   DE,burst.int
               OR   A
               SBC  HL,DE
               JR   NZ,sb.no.inc   ;only up channel if in this
                                   ;mode already
               LD   HL,burst.num+1
               LD   A,(HL)
               INC  A
               AND  3
               LD   (HL),A
sb.no.inc:
               LD   A,255
               LD   (int.rtn.pag),A
               CALL cls
               LD   IX,col.burst
               LD   A,8
               CALL colour.scrn

               LD   DE,burst
               LD   HL,32768
               LD   B,32
               CALL print.de.b
               LD   HL,32768+256
               LD   B,23
               CALL print.de.b

               LD   HL,3*256+32768
               LD   B,8
               CALL print.de.b
burst.num:     LD   A,0
               ADD  "1"
               CALL print.chr

               LD   HL,4*256+32768
               LD   B,21
               CALL print.de.b

               LD   HL,bp.sequence+1
               LD   DE,12
               LD   A,(burst.num+1)
               INC  A
               LD   B,A
bu.get.bi:     ADD  HL,DE
               DJNZ bu.get.bi

               LD   E,(HL)
               INC  L
               LD   D,(HL)
               INC  L
               LD   (bi.page+1),DE
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               INC  L
               LD   (bi.offs2+1),DE
               INC  DE
               LD   (bi.offs1+1),DE
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               INC  L
               LD   (bi.vol+1),DE
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               INC  L
               LD   (bi.slo+1),DE
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               LD   (bi.shi+1),DE


               LD   HL,burst.int
               LD   (int.routine),HL
               IN   A,(251)
               LD   (int.rtn.pag),A
               LD   A,255
               RET

burst.int:
               LD   A,(counter)
               OR   A
               RET  NZ

burst.pr.pos:
               LD   HL,5*256+32768+2
               LD   B,32
bi.page:       LD   A,(0)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo
               INC  L
bi.offs1:      LD   A,(0)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo
bi.offs2:      LD   A,(0)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo
               INC  L
               INC  L
bi.vol:        LD   A,(0)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo
               INC  L
               INC  L
bi.slo:        LD   A,(0)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo
               INC  L
               INC  L
bi.shi:        LD   A,(0)
               LD   C,A
               AND  &F0
               CALL printhi
               LD   A,C
               AND  &0F
               CALL printlo
               LD   HL,burst.pr.pos+2
               LD   A,(HL)
               INC  A
               CP   24+128
               JR   NZ,$+4
               LD   A,5+128
               LD   (HL),A
               RET

show.pattern:
               LD   A,255
               LD   (int.rtn.pag),A
               XOR  A
               LD   (print.pos+1),A
               CALL cls

               LD   IX,col.pattern
               LD   A,8
               CALL colour.scrn

               LD   HL,256*3+32768+8192+1
               LD   C,24-3-3
               LD   DE,29
col.lp2:
               LD   IX,2*8+colours
               LD   B,8
col.lp1:       LD   A,(IX)
               INC  IX
               LD   (HL),A
               ADD  HL,DE
               LD   (HL),A
               INC  HL
               INC  HL
               INC  HL
               DJNZ col.lp1

               DEC  C
               LD   A,C
               CP   9
               JR   NZ,$+3
               INC  H
normal:
               OR   A
               JR   NZ,col.lp2

               CALL pr.title

               LD   DE,volume
               LD   HL,1*256+32768
               LD   B,32
               CALL print.de.b

               CALL pr.amp.fac

               LD   DE,keys
               LD   HL,22*256+32768
               LD   B,32
               CALL print.de.b

               LD   DE,author
               LD   HL,23*256+32768
               LD   B,32
               CALL print.de.b

               LD   DE,channel
               LD   HL,2*256+1+32768
               LD   B,8
               CALL print.de.b
               LD   A,"1"
               CALL print.chr

               LD   DE,channel
               LD   HL,2*256+22+32768
               LD   B,8
               CALL print.de.b
               LD   A,"2"
               CALL print.chr

               LD   DE,channel
               LD   HL,12*256+1+32768
               LD   B,8
               CALL print.de.b
               LD   A,"4"
               CALL print.chr

               LD   DE,channel
               LD   HL,12*256+22+32768
               LD   B,8
               CALL print.de.b
               LD   A,"3"
               CALL print.chr

               CALL pr.loop.st

               LD   A,2
               LD   (pr.set+1),A

               LD   HL,printer
               LD   (int.routine),HL
               IN   A,(251)
               LD   (int.rtn.pag),A

               LD   A,255
               RET

pr.loop.st:
               LD   A,(trackon+1)  ;if not on track page then
               CP   4              ;don't print loop status
               RET  NZ
               LD   A,(disable.pos)
               OR   A
               LD   A,"Y"
               JR   Z,$+4
               LD   A,"N"
               LD   HL,32*8*22+31+32768
               JP   print.chr


ss.pointer:    DEFB %10000000,%00000001
               DEFB %11000000,%00000011
               DEFB %11100000,%00000111
               DEFB %11000000,%00000011
               DEFB %10000000,%00000001

show.samples:
               CALL set.show.smp
pr.ins.clp:
               LD   B,22

               CALL print.de.b

               INC  L

               LD   A,(DE)
               LD   B,A
               INC  DE
               LD   A,(DE)
               OR   B
               JR   NZ,pr.ins.exist
               LD   A,E
               ADD  A,3
               LD   E,A
               JR   NC,$+3
               INC  D
               LD   A,L
               ADD  7
               LD   L,A
               JR   NC,$+3
               INC  H
               JR   pr.next.ins
pr.ins.exist:
               LD   A,B
               CALL print.num
               LD   A,(DE)
               INC  DE
               CALL print.num

               INC  L
               INC  DE
               LD   A,(DE)
               INC  DE
               CALL print.num
pr.next.ins:
               LD   A,E
               ADD  4
               LD   E,A
               JR   NC,$+3
               INC  D

               LD   A,L
               ADD  6*32-30
               LD   L,A
               JR   NC,$+3
               INC  H

               DEC  C
               JP   NZ,pr.ins.clp
set.samp.int:
               LD   HL,instr.point
               LD   (int.routine),HL
               IN   A,(251)
               LD   (int.rtn.pag),A

               LD   A,255
               RET

set.show.smp:
               XOR  A
               LD   (c1.inst+1),A
               LD   (c2.inst+1),A
               LD   (c3.inst+1),A
               LD   (c4.inst+1),A
               CPL
               LD   (int.rtn.pag),A
               CALL cls

               LD   IX,col.samples
               LD   A,6
               CALL colour.scrn

               LD   HL,192*1+32768+8192+0
               LD   B,31*6
               LD   DE,31
               XOR  A
ss.col.lp1:
               LD   (HL),A
               ADD  HL,DE
               LD   (HL),A
               INC  HL
               DJNZ ss.col.lp1

               LD   HL,192*1+32768
               LD   C,31
ss.pr.pntc:
               LD   IX,ss.pointer
               LD   B,6
ss.pr.pntb:
               LD   A,(IX)
               INC  IX
               LD   (HL),A
               ADD  HL,DE
               LD   A,(IX)
               INC  IX
               LD   (HL),A
               INC  HL
               DJNZ ss.pr.pntb
               DEC  C
               JR   NZ,ss.pr.pntc

               CALL pr.title

               LD   C,31
               LD   HL,(mod.header+1080)
               OR   A
               LD   DE,&2E4D       ;M.
               SBC  HL,DE
               JR   Z,got.ins
               LD   HL,(mod.header+1080)
               OR   A
               LD   DE,&4C46       ;FL
               SBC  HL,DE
               JR   Z,got.ins
               LD   C,15
got.ins:
               LD   HL,6*32+32768+1
               LD   DE,mod.header+20
               RET

show.sizes:
               CALL set.show.smp
pr.size.clp:
               LD   B,9

               CALL print.de.b

               INC  L

               LD   A,E
               ADD  22-9
               LD   E,A
               JR   NC,$+3
               INC  D

               LD   A,(DE)
               LD   B,A
               INC  DE
               LD   A,(DE)
               OR   B
               JR   NZ,pr.ins.exis2

               LD   A,E
               ADD  A,7
               LD   E,A
               JR   NC,$+3
               INC  D
               LD   A,L
               ADD  30-10
               LD   L,A
               JR   NC,$+3
               INC  H
               JR   pr.next.in2
pr.ins.exis2:
               LD   A,B
               CALL print.num
               LD   A,(DE)
               INC  DE
               CALL print.num
               INC  L

               LD   A,(DE)
               AND  &0F
               BIT  3,A
               JR   Z,tune.plus
               LD   A,"-"
               CALL print.chr
               LD   A,(DE)
               AND  &0F
               LD   B,A
               LD   A,16
               SUB  B
               JR   got.tune
tune.plus:
               LD   A,"+"
               CALL print.chr
               LD   A,(DE)
               AND  &0F
got.tune:
               ADD  "0"
               CALL print.chr
               INC  L
               INC  DE

               LD   A,(DE)
               INC  DE
               CALL print.num
               INC  L

               LD   A,(DE)         ;loop offset
               INC  DE
               CALL print.num
               LD   A,(DE)
               INC  DE
               CALL print.num
               INC  L

               LD   A,(DE)         ;loop length
               INC  DE
               CALL print.num
               LD   A,(DE)
               INC  DE
               CALL print.num
pr.next.in2:
               LD   A,L
               ADD  6*32-30
               LD   L,A
               JR   NC,$+3
               INC  H

               DEC  C
               JP   NZ,pr.size.clp

               JP   set.samp.int

pr.title:
               LD   HL,32768
               LD   DE,mod.header
               LD   B,20
               CALL print.de.b
               RET
cls:
               LD   HL,32768
               LD   DE,32769
               LD   BC,6143
               LD   (HL),L
               LDIR
               RET

print.de.b:    LD   A,(DE)
               OR   A
               JR   Z,eop
               INC  DE
               CALL print.chr
               DJNZ print.de.b
               RET

eop:           LD   A," "
               INC  DE
               CALL print.chr
               DJNZ eop
               RET

colour.scrn:
               LD   (line.size+1),A

               LD   HL,32768+8192
col.loop:
               LD   C,(IX)
               INC  IX
col.clp1:
               LD   A,(IX)

               LD   DE,colours
               ADD  A,A
               ADD  A,A
               ADD  A,A
               ADD  A,E
               LD   E,A
               JR   NC,$+3
               INC  D

line.size:     LD   B,8
col.blp2:
               LD   A,(DE)
               INC  DE
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L

               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L

               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L

               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  HL

               DJNZ col.blp2

               DEC  C
               JR   NZ,col.clp1
               INC  IX

               LD   A,H
               CP   32768+8192+6144/256
               JR   NZ,col.loop
               RET

;---------------------------------------------------------------
;display pointers on sample name screen

instr.point:
               LD   A,(counter)
               OR   A
               RET  NZ

               LD   B,32

               LD   A,(c1.inst+1)
               OR   A
               LD   C,0
               CALL NZ,clr.point

               LD   A,(c4.inst+1)
               OR   A
               LD   C,0
               CALL NZ,clr.point

               LD   A,(c2.inst+1)
               OR   A
               LD   C,31
               CALL NZ,clr.point

               LD   A,(c3.inst+1)
               OR   A
               LD   C,31
               CALL NZ,clr.point

               LD   HL,current.row
               LD   A,(HL)
               AND  &10
               LD   C,A
               INC  L
               INC  L
               LD   A,(HL)
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               OR   C
               JR   Z,$+5
               LD   (c1.inst+1),A
               INC  L
               INC  L

               LD   A,(HL)
               AND  &10
               LD   C,A
               INC  L
               INC  L
               LD   A,(HL)
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               OR   C
               JR   Z,$+5
               LD   (c2.inst+1),A
               INC  L
               INC  L

               LD   A,(HL)
               AND  &10
               LD   C,A
               INC  L
               INC  L
               LD   A,(HL)
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               OR   C
               JR   Z,$+5
               LD   (c3.inst+1),A
               INC  L
               INC  L

               LD   A,(HL)
               AND  &10
               LD   C,A
               INC  L
               INC  L
               LD   A,(HL)
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               OR   C
               JR   Z,$+5
               LD   (c4.inst+1),A
               INC  L
               INC  L

               LD   A,(c1.on)
               OR   A
               JR   Z,skip.c1
c1.inst:       LD   A,0
               OR   A
               LD   C,0
               CALL NZ,col.point1
skip.c1:
               LD   A,(c4.on)
               OR   A
               JR   Z,skip.c4
c4.inst:       LD   A,0
               OR   A
               LD   C,0
               CALL NZ,col.point2
skip.c4:
               LD   A,(c2.on)
               OR   A
               JR   Z,skip.c2
c2.inst:       LD   A,0
               OR   A
               LD   C,31
               CALL NZ,col.point1
skip.c2:
               LD   A,(c3.on)
               OR   A
               JR   Z,skip.c3
c3.inst:       LD   A,0
               OR   A
               LD   C,31
               CALL NZ,col.point2
skip.c3:
               RET

clr.point:
               LD   HL,line.table
               ADD  A,A
               ADD  A,L
               LD   L,A
               LD   A,(HL)
               INC  L
               LD   H,(HL)
               ADD  C
               LD   L,A
               LD   C,0
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),C
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),C
               RET

col.point1:
               LD   HL,line.table
               ADD  A,A
               ADD  A,L
               LD   L,A
               LD   A,(HL)
               INC  L
               LD   H,(HL)
               ADD  C
               LD   L,A

               LD   (HL),4
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),5
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),6
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),5
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),4
               RET

col.point2:
               LD   HL,line.table
               ADD  A,A
               ADD  A,L
               LD   L,A
               LD   A,(HL)
               INC  L
               LD   H,(HL)
               ADD  C
               LD   L,A

               LD   (HL),64+5
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),64+6
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),6
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),64+6
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   (HL),64+5
               RET

;---------------------------------------------------------------

welcome:       DEFM "SAM MOD player             v2.10"
               DEFM "(C) 1996 Stefan Drissen"
               DEFB 0,0
               DEFM "            WELCOME!"
               DEFB 0,0
               DEFM "Please note that this program is"
               DEFM "NOT public domain, please only"
               DEFB 0
               DEFM "use this program if YOU bought"
               DEFB 0
               DEFM "it. (What's 5 pounds???)"
               DEFB 0
               DEFB 0
               DEFM "My uttermost thanks go out to"
               DEFB 0
               DEFM "Edwin Blink without whom the SAM"
               DEFM "MOD player probably would never "
               DEFM "have seen the light of day, I"
               DEFB 0
               DEFM "really hope that Thailand gives "
               DEFM "you what you were looking for."
               DEFB 0
               DEFM "Thanks also to Robert van der"
               DEFB 0
               DEFM "Veeke for the loading screen and"
               DEFM "moral support."
               DEFB 0
               DEFM "And to Martijn Groen for always "
               DEFM "having something silly to ask :)"
               DEFM "To Joshua Jensen (PC) for his"
               DEFB 0
               DEFM "documented PC MOD player source."
               DEFB 0
               DEFM "For commercial use contact me..."
               DEFM "                         F1=HELP"
               DEFM "       Stefan Drissen"
               DEFB 0
               DEFM "       Zevende Herven 6"
               DEFB 0
               DEFM "       5232 JZ  's-Hertogenbosch"
               DEFM "       The Netherlands"
               DEFB 0
               DEFM "       phone +31-73-6414969"
               DEFB 0
               DEFM "   http://www.pi.net/~drissen"
col.intro:
               DEFB 3,2,2,4,5,1,6,3,3,1,2,3,2,1,2,3,1,4,4,2,1,5
               DEFB 1,1
help.page:
               DEFM "SAM MOD player             v2.10"
               DEFM "(C) 1996 Stefan Drissen"
               DEFB 0,0
               DEFM "           HELP PAGE"
               DEFB 0,0
               DEFM "* F1: help page, F2: list names "
               DEFM "  F3: list sizes, F4: tracker,  "
               DEFM "  F5: summary effects (column 3)"
               DEFM "  F6: techy page - burst info"
               DEFB 0,0
               DEFM "* 1, 2, 3, 4: un/mute channel"
               DEFB 0,0
               DEFM "* P: pause/play"
               DEFB 0,0
               DEFM "* Cursors: rewind/fast forward"
               DEFB 0,0
               DEFM "* ESC: stop tune, load another"
               DEFB 0,0
               DEFM "* L: loop tune on/off"
               DEFB 0,0
               DEFM "* On F2/F3 screen, the first    "
               DEFM "  number is the sample length   "
               DEFM "  in words, the second number is"
               DEFM "  the default volume."
               DEFB 0,0
               DEFM "* Track-mode skips rows if the  "
               DEFM "  song speed is too fast."
               DEFB 0,0
               DEFM "* The three digits in the top   "
               DEFM "  right corner of the screen are"
               DEFM "  song position, pattern number "
               DEFM "  and pattern row."
               DEFB 0
col.help:
               DEFB 3,2,2,4,5,1,2,3,2,1,2,3,2,1,2,3,5,1,3,3,4,1

prosummary:
               DEFM "SAM MOD player             v2.10"
               DEFM "(C) 1996 Stefan Drissen"
               DEFB 0,0
               DEFM " SUMMARY OF PROTRACKER EFFECTS"
               DEFB 0,0
               DEFM "0 Arpeggio"
               DEFB 0
               DEFM "1 Portamento Up       (speed xy)"
               DEFM "2 Portamento Down     (speed xy)"
               DEFM "3 Tone Portamento     (speed xy)"
               DEFM "4 Vibrato (speed x, amplitude y)"
               DEFM "5 Tone and Volume Slide"
               DEFB 0
               DEFM "6 Vibrato and Volume Slide"
               DEFB 0
               DEFM "7 Tremolo (speed x, amplitude y)"
               DEFM "8 Undefined"
               DEFB 0
               DEFM "9 Sample Offset (512 bytes * xy)"
               DEFB 0
               DEFM "A Volume Slide    (up x, down y)"
               DEFM "B Position Jump          (to xy)"
               DEFM "C Volume Change          (to xy)"
               DEFM "D Pattern Break  (to row xy dec)"
               DEFM "E Extra effects (x=com, y=param)"
               DEFM "F Set Speed or Tempo if xy > 20 "
               DEFB 0
               DEFM "   EXTRA EFFECTS (E-command)"
               DEFB 0,0
               DEFM "0 filter      * 8 undefined"
               DEFB 0
               DEFM "1 fine porta up 9 retrigger note"
               DEFM "2 fine porta dn A volume fine up"
               DEFM "3 gliss control B volume fine dn"
               DEFM "4 vibrato cntrl C note cut"
               DEFB 0
               DEFM "5 set fine tune D note delay    "
               DEFM "6 jump loop     E pattern delay "
               DEFM "7 tremolo cntrl F no standard  *"
col.pro:
               DEFB 3,2,2,4,17,1,2,4,8,3

burst:
               DEFM "SAM MOD player             v2.10"
               DEFM "(C) 1996 Stefan Drissen"
               DEFM "CHANNEL "
               DEFM "Page Offs Vol SLo SHi"

col.burst:
               DEFB 3,2,1,4,1,5,19,1

colours:
               DEFB 0,0,0,0,0,0,0,0                    ;0 black
               DEFB 1,2,3,2,1,0,0,0                    ;1 blue
               DEFB 4,5,6,5,4,0,0,0                    ;2 orange
               DEFB 7,9+56,3,9+56,7,0,0,0              ;3 green
               DEFB 10+56,11+56,12+56,11+56,10+56,0,0,0;4 red
               DEFB 13+56,14+56,6,14+56,13+56,0,0,0    ;5 yellow

channel:       DEFM "Channel "

volume:        DEFM "Vol: 000%  Speed: 00  Tempo: 000"
keys:          DEFM "F1-F6 1234 C <> P ESC -+ Loop:  "
author:        DEFM "(C) 1996 Stefan Drissen    v2.10"

;font is already used in load routine - if it looks funny make
;sure to check that the values are the same!

font:          EQU  21412+32768
             ; MDAT "font"


pr.amp.fac:    LD   HL,256+32768+5
               LD   BC,(amp.fac+1)

;convert &xx.xx to %
;entry = BC

do.percent:    LD   A,B
               OR   A
               JR   NZ,$+4
               LD   A," "-"0"
               ADD  "0"
               CALL print.chr
               EX   DE,HL
               LD   HL,0
               LD   B,H
               ADD  HL,BC
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,BC
               ADD  HL,HL
               LD   A,H
               ADD  "0"
               EX   DE,HL
               CALL print.chr
               EX   DE,HL
               LD   C,L
               LD   B,0
               LD   H,B
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,BC
               ADD  HL,HL
               LD   A,H
               ADD  "0"
               EX   DE,HL
               JP   print.chr

volume.tab:    EQU  512

;---------------------------------------------------------------

tables:
               LD   A,(ld.dev)
               LD   HL,bits.per.dev
               ADD  A,L
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   B,(HL)         ;output bits

               LD   C,32           ;num vol tabs
               LD   A,B
               CP   4
               JR   NZ,$+4         ;saa?
               LD   C,16

;---------------------------------------------------------------
;create volume tables for burstplayer (Amiga samples)
;B = number of bits
;C = number of tables

make.vol.tab:
               LD   IX,volume.tab
               PUSH IX

               XOR  A
               BIT  4,C
               JR   Z,$+3
               INC  A
               LD   (cv.skip.table+2),A

               LD   A,201          ;RET
               BIT  4,C
               JR   Z,$+3
               XOR  A
               LD   (cv.no.double),A

               LD   A,C
               LD   (cv.max.tables+1),A
               DEC  A
               LD   (cv.div.by+1),A

               DEC  B
               LD   A,1
cv.getbits:
               RLA
               DJNZ cv.getbits
               LD   (cv.vol.base.1+2),A
               LD   (cv.vol.base.2+2),A

               RLA
               LD   (cv.vol.bits+1),A
               LD   A,0
               ADC  A,0
               LD   (cv.vol.bits+2),A

               XOR  A
               LD   (cv.volume+1),A

cv.loop:
cv.vol.bits:   LD   DE,0           ;DE=2^bits
               LD   H,D
               LD   L,E
               DEC  HL             ;HL=2^bits-1
               LD   A,L
               LD   (max.vol+1),A

               LD   HL,0

cv.volume:     LD   A,0            ;[0-31]/[0-15]
               OR   A
               JR   Z,cv.no.mul
               LD   B,A
cv.mul.vol:    ADD  HL,DE
               DJNZ cv.mul.vol
                                   ;HL=vol*2^bits

;here we need to ensure that HL gets multiplied by the factor!

               EX   DE,HL
amp.fac:       LD   BC,&0100       ;amplification factor
               XOR  A
               LD   H,A
               LD   L,A
               LD   (rest+1),A
mulamp:
rest:          LD   A,0
               ADD  C
               LD   (rest+1),A
               LD   A,B
               ADC  A,L
               LD   L,A
               JR   NC,$+3
               INC  H

               DEC  DE
               LD   A,D
               OR   E
               JR   NZ,mulamp
;
cv.no.mul:

               LD   B,H
               LD   C,L
cv.div.by:     LD   DE,15          ;tables-1
               CALL cv.bc.div.de
               LD   (cv.range+1),BC

cv.vol.base.1: LD   HL,&0800       ;H=2^(bits-1) "central" vol.
               LD   B,128

;2^bits * v/15

cv.range:      LD   DE,15          ;range (step)

cv.blp:
               LD   (IX),H
               INC  IX

               ADD  HL,DE

               LD   A,H
max.vol:       SUB  0              ;maximum volume (2^bits-1)
               JR   Z,$+4
               JR   C,not.max
               LD   A,(max.vol+1)
               LD   H,A            ;h=maximum volume
               LD   DE,0           ;reset adder
not.max:

               DJNZ cv.blp

               LD   C,127
               ADD  IX,BC

               LD   DE,(cv.range+1)

cv.vol.base.2: LD   HL,&0800       ;"central" volume
               LD   B,128
cv.blp2:
               OR   A
               SBC  HL,DE

               JR   NC,not.min
               LD   H,0            ;minimum volume
               LD   D,H            ;reset adder
               LD   E,H
not.min:
               LD   (IX),H
               DEC  IX
               DJNZ cv.blp2

cv.skip.table: LD   BC,129         ;B=1 if SAA
               ADD  IX,BC

               LD   A,(cv.volume+1)
               INC  A
               LD   (cv.volume+1),A
cv.max.tables: CP   0
               JP   NZ,cv.loop

               POP  HL
cv.no.double:  RET                 ;NOP if SAA

;copy tables with stereo flip for SAA1099

               LD   D,H
               LD   E,L
               INC  D
               LD   BC,16
cv.saa.one:
               LD   A,(HL)
               INC  HL
               ADD  A,A
               ADD  A,A
               ADD  A,A
               ADD  A,A
               LD   (DE),A
               INC  DE
               DJNZ cv.saa.one
               INC  D
               INC  H
               DEC  C
               JR   NZ,cv.saa.one
               RET

cv.bc.div.de:
               LD   A,B            ;divide BC by DE
               LD   B,16           ;result in BC
               LD   HL,0           ;DE is unchanged
cv.clcd1:      RL   C
               RLA
               ADC  HL,HL
               SBC  HL,DE
               JR   NC,cv.clcd2
               ADD  HL,DE
cv.clcd2:      CCF
               DJNZ cv.clcd1
               RL   C
               RLA
               LD   B,A
               RET

;===============================================================
bits.per.dev:

               DEFB 6,4,7,7,6,6,6,8      ;number of bits


length:        EQU  $-57344

mod.header:    DEFS 1084

chargap:
               DEFS chargap/256+1*256-chargap
char.list:
               DEFS 16*2
line.table:
               DEFS 32*2

build.font:
