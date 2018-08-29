
;SAM MOD player version 2.10
;DOS loader for SAM MOD player
;(C) 1996 Stefan Drissen
;last update: 7 May 1996, 00:55

;in version 2.10
;* version number changed
;in version 2.05
;* version number changed
;- names not printed past terminating 0
;in version 2.04
;* version number changed
;in version 2.03
;* version number changed, SAMdac instead of EDDAC
;in version 2.02
;- one drive no files bug removed


load.page:     EQU  5
load.offs:     EQU  32768

mode_2:        EQU  32

               ORG  32768
               DUMP 1,0

               JP   go.loader

               ORG  $-16384

ld.dev:        DEFB 2
ld.spd:        DEFB 0     ;0=pal, 1=ntsc
drv1.sr:       DEFB 2     ;0=6ms, 1=12ms, 2=2ms, 3=3ms
drv2.sr:       DEFB 1

               ORG  $+16384

go.loader:
               DI
               LD   A,32
               OUT  (250),A
               JP   loader.low

               ORG  $-16384

loader.low:
               LD   SP,32768

               CALL black.pal
               LD   A,mode_2
               OUT  (252),A
               CALL cls

               LD   A,(drv1.sr)
               LD   (steprate.1+1),A
               LD   A,(drv2.sr)
               LD   (steprate.2+1),A


               LD   DE,device.scrn
               LD   IX,col.device
               CALL print.screen

               LD   HL,5*192
               LD   (curs.offs+1),HL

               LD   A,(ld.dev)
               LD   C,A
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               LD   (old.cursor+1),A
               XOR  A
               LD   (curs.blink+1),A
               CALL print.cursor

               LD   HL,20*192
               LD   (curs.offs+1),HL

               LD   A,(ld.spd)
               LD   C,A
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               LD   (old.cursor+1),A
               XOR  A
               LD   (curs.blink+1),A
               CALL print.cursor
select.dev:
               LD   HL,5*192
               LD   (curs.offs+1),HL
               LD   A,7
               LD   (max.select+1),A

               LD   A,(ld.dev)
               LD   C,A
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   (old.cursor+1),A
sd.lp:
               CALL curs.select

               LD   A,C
               LD   (ld.dev),A

               CALL scan.return
               JR   Z,selected

               CALL scan.space
               JR   NC,sd.lp

               XOR  A
               LD   (curs.blink+1),A
               LD   A,C
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               CALL print.cursor

               JR   select.spd

select.spd:
               LD   HL,20*192
               LD   (curs.offs+1),HL
               LD   A,1
               LD   (max.select+1),A

               LD   A,(ld.spd)
               LD   C,A
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   (old.cursor+1),A
ss.lp:
               CALL curs.select

               LD   A,C
               LD   (ld.spd),A

               CALL scan.return
               JR   Z,selected

               CALL scan.space
               JR   NC,ss.lp

               XOR  A
               LD   (curs.blink+1),A
               LD   A,C
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               CALL print.cursor

               JR   select.dev

selected:
               CALL cls

               LD   A,load.page    ;load.page=burst.page
               OUT  (251),A
               LD   HL,0
               LD   A,(ld.dev)
               LD   (32771),A
               LD   A,(ld.spd)
               LD   (32772),A
               CALL 32768          ;make burst

               JP   loader

;selection routine
;C = current position
;min selection = 0
;max selection = (max.selec+1)

curs.select:
               PUSH BC
               LD   A,C
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               CALL print.cursor
               POP  BC

               LD   A,255
               IN   A,(254)
               LD   B,A
               BIT  1,B
               CALL Z,curs.up
               BIT  2,B
               CALL Z,curs.dn

               LD   A,%11101111
               IN   A,(254)
               LD   B,A
               BIT  1,B
               CALL Z,curs.up
               BIT  2,B
               CALL Z,curs.dn
               RET
curs.up:
               LD   A,C
               OR   A
               RET  Z
               PUSH BC
               LD   A,C
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               LD   B,6
sd.curs.up:    DEC  C
               CALL print.cursor
               DJNZ sd.curs.up
               POP  BC
               DEC  C
               RET

curs.dn:
max.select:    LD   A,6
               OR   A
               RET  Z
               DEC  A
               CP   C
               RET  C
               PUSH BC
               LD   A,C
               ADD  A,A
               ADD  A,C
               ADD  A,A
               LD   C,A
               LD   B,6
sd.curs.dn:    INC  C
               CALL print.cursor
               DJNZ sd.curs.dn
               POP  BC
               INC  C
               RET

scan.space:
               LD   A,127
               IN   A,(254)
               OR   A
               BIT  0,A
               RET  NZ
sd.stillspc:   LD   A,127
               IN   A,(254)
               BIT  0,A
               JR   Z,sd.stillspc
               SCF
               RET
scan.return:
               LD   A,%10111111    ;RETURN
               IN   A,(254)
               AND  1
               RET  Z
               LD   A,%11101111    ;0
               IN   A,(254)
               AND  1
               RET

;===============================================================

path:          DEFW patha

patha:         DEFB "\",0
               DEFS 63
               DEFB 0

pathb:         DEFB "\",0
               DEFS 63
               DEFB 0

temppath:      DEFB "\"
               DEFS 64

;===============================================================
;loader

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

;===============================================================
black.pal:
               LD   BC,15*256+248
               XOR  A
black:         OUT  (C),A
               DJNZ black
               OUT  (C),A
               RET

show.screen:
               CALL cls

               LD   DE,load.screen
               LD   IX,col.load
               CALL print.screen
               RET

loader:
               LD   SP,32768

               CALL show.screen

               LD   HL,mes.nodisc
               LD   DE,m.vollabel
               LD   BC,11
               LDIR

               LD   A,1
               LD   (msdos+1),A

               XOR  A
               LD   (nodisc+1),A

               CALL dir

               LD   A,1
               LD   (load.entries),A

               LD   HL,option.dir
               LD   DE,loader.dir
               LD   BC,load.len*2
               LDIR

               LD   DE,loader.dir+load.len
               LD   C,225+16
               LD   B,0
is.2.pres:     OUT  (C),B

               LD   A,12           ;approx 32 micro second delay
               DEC  A
               JR   NZ,$-1

               IN   A,(C)
               CP   B
               JR   NZ,no.drive2
               DJNZ is.2.pres
               LD   A,2
               LD   (load.entries),A
               LD   DE,2*load.len+loader.dir
no.drive2:

nodisc:        LD   A,0            ;set by errnodisc
               OR   A
               JP   NZ,converted

msdos:         LD   A,0
               OR   A
               JP   Z,cnv.sam

;convert pc dir -> loader dir

               LD   HL,(data)
pc.to.loader:
               LD   A,(HL)
               OR   A
               JP   Z,converted
               CP   229
               JP   Z,pl.skip      ;deleted file
               PUSH HL
               POP  IX
               LD   A,(IX+11)
               AND  8
               JP   NZ,pl.skip     ;volume label

               LD   A,(IX+8)
               CP   "M"
               JP   NZ,pl.skip
               LD   A,(IX+9)
               CP   "O"
               JP   NZ,pl.skip
               LD   A,(IX+10)
               CP   "D"
               JP   NZ,pl.skip     ;not MOD extension

               PUSH HL
               PUSH DE
               LD   B,8
pl.copy.name:  LD   A,(HL)
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ pl.copy.name

               PUSH DE

               LD   E,(IX+26)
               LD   D,(IX+27)      ;first cluster

               LD   HL,temp.spc-9  ;space between scrn & attrib
pc.rd.more:    CALL readcluster
               CALL getfatentry
               LD   A,H
               CP   temp.spc+1084-9/256+1
               JR   C,pc.rd.more

               LD   HL,temp.spc+1083-9
               LD   DE,temp.spc+1083
               LD   BC,1084
               LDDR

               POP  DE

               LD   A,2
               CALL file.check

               PUSH DE

               LD   E,(IX+28)
               LD   D,(IX+29)
               LD   A,(IX+30)
               EX   DE,HL
pc.resub:      OR   A
               SBC  HL,DE
               SBC  C
               JR   NC,pc.got.maxmin
               ADC  C
               ADD  HL,DE
               EX   DE,HL
               LD   B,A
               LD   A,C
               LD   C,B
               JR   pc.resub
pc.got.maxmin:            ;AHL=difference calc len & file len
               POP  DE
               OR   H
               JR   Z,pc.file.ok

               POP  DE
               POP  HL
               JR   pl.skip

pc.file.ok:

;get date
               LD   A,(IX+24)
               LD   B,A
               AND  %00011111      ;day
               CALL cnv.a.to.de
               LD   A,B
               AND  %11100000
               RLCA
               RLCA
               RLCA
               LD   C,A
               LD   A,(IX+25)
               LD   B,A
               AND  %00000001
               RLCA
               RLCA
               RLCA
               OR   C              ;month
               CALL cnv.a.to.de
               LD   A,B
               AND  %11111110
               RRCA
               ADD  80
               SUB  100
               JR   NC,$-2
               ADD  100            ;year
               CALL cnv.a.to.de

               CALL insert.size
               LD   HL,load.entries
               INC  (HL)
               POP  HL
               LD   BC,load.len
               ADD  HL,BC
               EX   DE,HL
               POP  HL
pl.skip:
               LD   BC,32
               ADD  HL,BC
               LD   A,(load.entries)
               CP   27
               JP   Z,converted
               JP   pc.to.loader

cnv.sam:
               PUSH DE
               LD   IX,col.black
               LD   A,6
               CALL colour.scrn

;first read in SAM directory
               LD   DE,1
               LD   HL,fat
cs.rd.lp:
               PUSH DE
               PUSH HL
               CALL rdphysec
               POP  HL

               LD   A,D
               OR   A
               JR   NZ,cs.notfirst
               DEC  E
               JR   NZ,cs.notfirst

               PUSH HL
               LD   DE,m.vollabel
               LD   HL,fat+210
               LD   A,(HL)
               CP   "*"
               JR   NZ,$+4
               LD   (HL),0
               LD   BC,10
               LDIR
               LD   A," "
               LD   (DE),A
               POP  HL
cs.notfirst:
               PUSH HL
               PUSH HL
               POP  DE
               LD   BC,245
               ADD  HL,BC
               LD   A,E
               ADD  11
               LD   E,A
               JR   NC,$+3
               INC  D
               LDI
               LDI
               INC  DE
               INC  DE
               LDI
               POP  HL

               LD   BC,16
               LD   E,L
               LD   D,H
               EX   DE,HL
               ADD  HL,BC
               EX   DE,HL
               INC  H

               PUSH DE
               PUSH HL
               PUSH HL
               POP  DE
               LD   BC,245
               ADD  HL,BC
               LD   A,E
               ADD  11
               LD   E,A
               JR   NC,$+3
               INC  D
               LDI
               LDI
               INC  DE
               INC  DE
               LDI
               POP  HL
               POP  DE

               LD   BC,16
               LDIR
               EX   DE,HL
               POP  DE
               INC  E
               LD   A,E
               CP   11
               JR   NZ,cs.rd.lp
               LD   E,1
               INC  D
               LD   A,D
               CP   4
               JR   NZ,cs.rd.lp

;now convert the SAM stuff to loader format

               POP  DE
               LD   HL,fat

               LD   A,80           ;80 directory entries
sam.to.loader:
               PUSH AF
               LD   A,(HL)
               AND  %00111111
               CP   19
               JP   NZ,sl.skip

               PUSH HL

               INC  HL
               INC  HL
               LD   B,8
ext.find.m:    LD   A,(HL)
               INC  HL
               CP   "."
               JR   NZ,ext.not.fnd
               LD   A,(HL)
               RES  5,A            ;->uppercase
               CP   "M"
               JR   NZ,ext.not.fnd
               JR   sam.found.m
ext.not.fnd:
               DJNZ ext.find.m
               POP  HL
               JP   sl.skip

sam.found.m:
               POP  HL
               PUSH HL
               POP  IX

               PUSH HL
               PUSH DE
               INC  HL
               LD   B,8
sl.copy.name:  LD   A,(HL)
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ sl.copy.name

               PUSH DE

               LD   E,(IX+14)
               LD   D,(IX+13)      ;first sector

               LD   HL,temp.spc-9
               CALL rdphysec
               DEC  HL
               LD   E,(HL)
               DEC  HL
               LD   D,(HL)
               CALL rdphysec
               DEC  HL
               LD   E,(HL)
               DEC  HL
               LD   D,(HL)
               CALL rdphysec

               POP  DE

               LD   A,2
               CALL file.check

               CALL fc.sam
               JR   Z,sm.file.ok

;not MOD, maybe compressed mod (4 bit)

               POP  DE
               PUSH DE

               LD   HL,8
               ADD  HL,DE
               EX   DE,HL

               LD   A,1   ;only add sample length once
               CALL file.check

               CALL fc.sam
               JR   Z,sm.file.ok

               POP  DE
               POP  HL
               JR   sl.skip

sm.file.ok:

;get date
               PUSH DE
               LD   A,"*"
               LD   (DE),A

sm.check.date: LD   A,(IX+11)
               OR   A
               JR   Z,sm.done.date   ;0->invalid date
               CP   32
               JR   NC,sm.done.date  ;day>31 = invalid date
               LD   A,(IX+12)
               OR   A
               JR   Z,sm.done.date   ;0->invalid date
               CP   13
               JR   NC,sm.done.date  ;month>12 = invalid date
               LD   A,(IX+15)
               OR   A
               JR   Z,sm.done.date ;0->invalid date
               INC  A
               JR   Z,sm.done.date ;255->invalid date

               LD   A,(IX+11)
               CALL cnv.a.to.de
               LD   A,(IX+12)
               CALL cnv.a.to.de
               LD   A,(IX+15)
               CALL cnv.a.to.de
sm.done.date:
               POP  DE
               LD   A,E
               ADD  6
               LD   E,A
               JR   NC,$+3
               INC  D

               CALL insert.size

               LD   HL,load.entries
               INC  (HL)
               POP  HL
               LD   BC,load.len
               ADD  HL,BC
               EX   DE,HL
               POP  HL
sl.skip:
               LD   BC,16
               ADD  HL,BC

               POP  AF
               LD   B,A
               LD   A,(load.entries)
               CP   27
               JR   Z,converted
               LD   A,B
               DEC  A
               JP   NZ,sam.to.loader

converted:
               CALL show.screen

               CALL print.oct

               LD   A,(driveselect+1)
               OR   A
               LD   A,"1"
               JR   Z,$+3
               INC  A
               LD   (mes.drive+6),A

               LD   DE,mes.label
               LD   HL,m.vollabel
               LD   A,(HL)
               OR   A
               JR   NZ,$+5
               LD   HL,mes.nolabel
               LD   BC,11
               LDIR

               LD   HL,192*3
               LD   DE,mes.drive
               LD   B,9+11
               CALL print.de.b

               LD   A,(load.entries)
               CP   24
               JR   C,$+4
               LD   A,24
               LD   C,A
               LD   DE,loader.dir
               LD   HL,192*4+1
le.loop:
               PUSH DE
               PUSH HL
               LD   B,8
               CALL print.de.b
               INC  L
               LD   B,20
               CALL print.de.b
               POP  HL
               LD   DE,192
               ADD  HL,DE
               POP  DE
               LD   A,E
               ADD  load.len
               LD   E,A
               JR   NC,$+3
               INC  D

               DEC  C
               JR   NZ,le.loop

still.esc:     LD   A,247          ;to prevent immediate exit
               IN   A,(249)        ;when escape used to exit
               BIT  5,A            ;"DEMO"
               JR   Z,still.esc

               LD   HL,4*192
               LD   (curs.offs+1),HL
               LD   A,(load.entries)
               DEC  A
               LD   (max.select+1),A

               LD   C,0
               CALL print.cursor
cursor.lp:
               CALL get.entry
               LD   A,(IX+28)
               RES  6,A
               LD   DE,mes.noi
               OR   A
               JR   Z,got.mes
               LD   DE,mes.pro
               DEC  A
               JR   Z,got.mes
               LD   DE,mes.sta
               DEC  A
               JR   Z,got.mes
               LD   DE,mes.drv
got.mes:
               LD   A,(IX+28)
               BIT  7,A
               JP   NZ,disc.mess   ;new disc message
               PUSH IX
               PUSH DE
               POP  IX
               LD   (IX+26),"8"
               BIT  6,A
               JR   Z,gm.is.8
               LD   (IX+26),"4"
gm.is.8:
               POP  IX
               PUSH BC             ;c = select position
               PUSH DE
               LD   A,E
               ADD  14
               LD   E,A
               JR   NC,$+3
               INC  D
               LD   A,(IX+38)
               CALL cnv.a.to.de
               POP  DE

               LD   B,32
               LD   HL,29*192
               CALL print.de.b

               LD   HL,mes.size
               LD   A,(IX+29)      ;length in patterns
               LD   B,"0"
               CP   100
               JR   C,gm.len.100
               INC  B
               SUB  100
gm.len.100:
               LD   (HL),B
               INC  HL
               EX   DE,HL
               CALL cnv.a.to.de

               LD   L,(IX+36)
               LD   H,(IX+37)

               LD   BC,100
               LD   A,"0"-1
               OR   A
gm.get.big:    SBC  HL,BC
               INC  A
               JR   NC,gm.get.big
               ADD  HL,BC
               LD   DE,mes.size+18
               LD   (DE),A
               INC  DE
               LD   A,L
               CALL cnv.a.to.de

               LD   DE,mes.size+24
               LD   A,(IX+30)
               CP   "*"
               JR   NZ,gm.is.date
               LD   HL,mes.no.date
               LD   BC,8
               LDIR
               JR   gm.got.date
gm.is.date:
               EX   DE,HL
               LD   (HL),A
               INC  HL
               LD   A,(IX+31)
               LD   (HL),A
               INC  HL
               LD   (HL),"-"
               INC  HL
               LD   A,(IX+32)
               LD   (HL),A
               INC  HL
               LD   A,(IX+33)
               LD   (HL),A
               INC  HL
               LD   (HL),"-"
               INC  HL
               LD   A,(IX+34)
               LD   (HL),A
               INC  HL
               LD   A,(IX+35)
               LD   (HL),A
gm.got.date:
               LD   DE,mes.size
               POP  BC
               JR   normal.mess

disc.mess:     LD   HL,29*192
               LD   B,32
blnk.line:     LD   A," "
               CALL print.chr
               DJNZ blnk.line
normal.mess:
               LD   B,32
               LD   HL,30*192
               CALL print.de.b

               CALL curs.select

               LD   A,247
               IN   A,(249)
               BIT  5,A
               JR   Z,quit

               CALL scan.return
               JP   Z,select.key

               LD   A,%11011111
               IN   A,(254)
               BIT  1,A
               LD   A,0
               JR   NZ,not.o
still.o:       SCF
               JR   C,not.o.nc
               LD   A,(octaves+1)
               XOR  %110
               LD   (octaves+1),A
               CALL print.oct
               LD   A,55           ;scf
not.o:         LD   (still.o),A
not.o.nc:
               JP   cursor.lp

print.oct:
               LD   HL,192+26
               LD   DE,mes.oct
               LD   B,5
               CALL print.de.b

               LD   A,(octaves+1)
               ADD  "0"
               CALL print.chr

               RET

quit:
               IN   A,(250)
               AND  31
               OUT  (251),A
               JP   quit.hi

               ORG  $+32768
quit.hi:
               XOR  A
               OUT  (250),A
               RST  0

               ORG  $-32768

;check to see if the sum of sample lengths + patterns = file len

file.check:
               LD   HL,temp.spc
               LD   BC,20
               LDIR

               LD   (bytes.per+1),A  ;2=normal, 1=compressed?

               PUSH IX
               LD   BC,(temp.spc+1080)
               LD   A,1
               OR   A
               LD   HL,&2E4D       ;M.
               SBC  HL,BC
               JR   Z,pl.got.type
               INC  A
               OR   A
               LD   HL,&4C46       ;FL
               SBC  HL,BC
               JR   Z,pl.got.type
               XOR  A
pl.got.type:                       ;0=nst, 1=m.k., 2=flt4
               LD   (DE),A
               LD   A,(bytes.per+1)
               DEC  A
               LD   A,(DE)
               JR   NZ,pl.not.comp
               SET  6,A            ;compressed 4 bit
               LD   (DE),A
pl.not.comp:
               INC  DE
               LD   HL,30*31+20+temp.spc
               AND  63
               JR   NZ,$+5
               LD   HL,30*15+20+temp.spc
               LDI
               PUSH DE
               LD   BC,31*256
               LD   HL,30*31+20+130+4
               AND  63
               JR   NZ,fc.is.nst
               LD   B,15
               LD   HL,30*15+20+130
fc.is.nst:
               XOR  A
               LD   (sample.count+1),A
               LD   IX,temp.spc+20
add.all.smp:   LD   D,(IX+22)
               LD   E,(IX+23)
bytes.per:     LD   A,2
times.sample:  ADD  HL,DE
               JR   NC,$+3
               INC  C
               DEC  A
               JR   NZ,times.sample
               LD   A,D
               OR   A
               JR   NZ,fc.is.samp
               LD   A,E
               CP   2
               JR   C,fc.not.samp
fc.is.samp:
               LD   A,(sample.count+1)
               INC  A
               LD   (sample.count+1),A
fc.not.samp:
               LD   DE,30
               ADD  IX,DE
               DJNZ add.all.smp
               INC  IX
               INC  IX
               LD   B,128
               LD   E,0
get.hi.patt:   LD   A,(IX)
               INC  IX
               CP   E
               JR   C,$+3
               LD   E,A
               DJNZ get.hi.patt
               INC  E
               LD   B,E
               LD   DE,1024
add.all.pat:   ADD  HL,DE
               JR   NC,$+3
               INC  C
               DJNZ add.all.pat
                                   ;so now CHL = calc. size
               LD   A,(bytes.per+1)
               DEC  A
               JR   Z,fl.is.half
               LD   A,H
               LD   (file.len+1),A
               LD   A,C
               LD   (file.len+2),A
fl.is.half:
               POP  DE
               POP  IX
               RET

insert.size:
file.len:
               LD   HL,0
               SRL  H
               RR   L
               SRL  H
               RR   L
               EX   DE,HL
               LD   (HL),E         ;size in k
               INC  HL
               LD   (HL),D
               INC  HL
sample.count:  LD   A,0
               LD   (HL),A
               RET


fc.sam:
               PUSH DE

               LD   DE,(temp.spc-9+1);length mod 16384
               LD   A,(temp.spc-9+7) ;length in pages (16384)
               AND  %00000011
               RRCA
               RRCA
               ADD  D
               LD   D,A
               LD   A,(temp.spc-9+7)
               JR   NC,$+4
               ADD  4
               SRL  A
               SRL  A

               EX   DE,HL
sm.resub:      OR   A
               SBC  HL,DE
               SBC  C
               JR   NC,sm.got.maxmin
               ADC  C
               ADD  HL,DE
               EX   DE,HL
               LD   B,A
               LD   A,C
               LD   C,B
               JR   sm.resub
sm.got.maxmin:            ;AHL=difference calc len & file len
               POP  DE
               OR   H
               RET


mes.load:      DEFM " Loading: "

select.key:
               CALL get.entry

               LD   DE,mes.load
               LD   B,10
               LD   HL,192*31
               CALL print.de.b
               PUSH IX
               POP  DE
               LD   A,E
               ADD  8
               LD   E,A
               JR   NC,$+3
               INC  D
               LD   B,20
               CALL print.de.b
               XOR  A
               CALL print.chr
               XOR  A
               CALL print.chr

               LD   A,(IX+28)      ;mod type ,+128=drive
               BIT  7,A
               JP   NZ,new.read

               PUSH AF

               CALL checknodisc
               JP   Z,loader

               PUSH IX
               POP  HL
               LD   DE,parafile
               LD   BC,8
               LDIR
               EX   DE,HL
               LD   (HL),"M"
               INC  HL
               LD   (HL),"O"
               INC  HL
               LD   (HL),"D"

               LD   HL,parafile
               CALL findfile
               LD   A,(msdos+1)
               OR   A
               JR   Z,sam.load

               PUSH HL
               POP  IX
               LD   E,(IX+26)
               LD   D,(IX+27)

               LD   A,load.page
               OUT  (251),A
               LD   HL,load.offs
pc.load.all:
               CALL readcluster
               IN   A,(251)
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  A
               OUT  (251),A
               CALL getfatentry
               LD   A,D
               CP   15
               JR   NZ,pc.load.all
               LD   A,E
               CP   &F8
               JR   C,pc.load.all

               JP   file.loaded


sam.load:
               LD   DE,1
sam.load.dir:  LD   HL,6144
               CALL rdphysec
               LD   HL,6144
               CALL sam.match
               LD   HL,6144+256
               CALL sam.match
               INC  E
               LD   A,E
               CP   11
               JR   NZ,sam.load.dir
               LD   E,1
               INC  D
               LD   A,D
               CP   4
               JR   NZ,sam.load.dir
file.notfound:
               JP   loader


sam.match:
               PUSH IX
               LD   A,(HL)
               CP   19
               JR   NZ,sam.no.match

               LD   B,8
sam.match.blp: LD   A,(IX)
               INC  IX
               INC  L
               CP   (HL)
               JR   NZ,sam.no.match
               DJNZ sam.match.blp
             ; INC  L
             ; LD   A,(HL)
             ; CP   "."
             ; JR   NZ,sam.no.match
             ; INC  L
             ; LD   A,(HL)
             ; CP   "m"
             ; JR   NZ,sam.no.match
               POP  IX

               POP  AF             ;chuck return address

               LD   A,L
               AND  128
               OR   13
               LD   L,A
               LD   D,(HL)         ;first track
               INC  L
               LD   E,(HL)         ;first sector

               LD   HL,6144
               CALL rdphysec

               LD   A,load.page
               OUT  (251),A
               LD   HL,6144+9
               LD   DE,load.offs
               LD   BC,510-9
               LDIR
               LD   D,(HL)
               INC  L
               LD   E,(HL)

               LD   HL,load.offs+510-9
sam.load.lp:
               CALL rdphysec
               DEC  HL
               LD   E,(HL)
               DEC  HL
               LD   D,(HL)
               LD   A,E
               OR   D
               JR   Z,file.loaded
               IN   A,(251)
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  A
               OUT  (251),A
               JR   sam.load.lp

sam.no.match:
               POP  IX
               RET


file.loaded:
               POP  AF
               BIT  6,A
               JP   Z,no.decompress
decompress:
               LD   C,A
               LD   A,load.page
               OUT  (251),A
               LD   A,C
               RES  6,A

               LD   HL,31*30+20+2+32768
               LD   DE,4
               OR   A
               LD   A,31
               JR   NZ,dc.not.noise
               LD   HL,15*30+20+2+32768
               LD   E,D
               LD   A,15
dc.not.noise:
               LD   (instruments+1),A
               LD   B,128
               LD   A,(HL)
searchtable:
               CP   (HL)
               JR   NC,alreadyhi
               LD   A,(HL)
alreadyhi:
               INC  HL
               DJNZ searchtable
               INC  A

               ADD  HL,DE

               LD   B,A
               LD   E,load.page
convallppats:
               LD   A,H
               ADD  4
               LD   H,A
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  E
               DJNZ convallppats

;sample starts directly after last pattern

               LD   (smp1.offs+1),HL
               LD   A,E
               LD   (smp1.page+1),A

               LD   IX,32768+20

;put starting addresses of samples in sample table

instruments:   LD   B,0

               LD   HL,0
               XOR  A
gsmp.blp:
               LD   D,(IX+22)
               LD   E,(IX+23)
               ADD  HL,DE
               JR   NC,$+4
               ADD  4
               LD   DE,30
               ADD  IX,DE
               DJNZ gsmp.blp

               BIT  7,H
               RES  7,H
               JR   Z,$+4
               ADD  2
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  A

               LD   (samplen+1),HL
               LD   (samppag+1),A

smp1.page:     ADD  0
smp1.offs:     LD   DE,0
               ADD  HL,DE
               JR   NC,$+4
               ADD  2
               SET  7,H
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  A
               LD   D,A

               EXX
               LD   HL,(smp1.offs+1)
               LD   A,(smp1.page+1)
               LD   D,A

samplen:       LD   BC,0
samppag:       LD   E,0

loop2:
               LD   A,B
               OR   C
               JR   Z,end2

               LD   A,D
               OUT  (251),A
               LD   A,(HL)
               RLCA
               RLCA
               RLCA
               RLCA
               EXX

               EX   AF,AF'
               LD   A,D
               OUT  (251),A
               EX   AF,AF'
               AND  %11110000
               LD   (HL),A
               INC  HL
               EXX
               LD   A,D
               OUT  (251),A
               LD   A,(HL)
               AND  %11110000
               LD   (HL),A
               INC  HL

               DEC  BC
               JR   loop2
end2:
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  D
               EXX
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  D
               EXX
               LD   BC,16384
               LD   A,E
               DEC  E
               OR   A
               JR   NZ,loop2

no.decompress:
               CALL cls
               IN   A,(250)
               AND  31
               OUT  (251),A
octaves:       LD   A,3            ;3 or 5 octave
               CALL 57344 ;demo
               LD   A,C
               DEC  A
               JP   NZ,converted
               JP   quit

get.entry:
               LD   IX,loader.dir-load.len
               LD   DE,load.len
               LD   B,C
               INC  B
g.load.entry:  ADD  IX,DE
               DJNZ g.load.entry
               RET


new.read:
               AND  127
               OR   A
               PUSH AF
               CALL Z,select1
               POP  AF
               CALL NZ,select2
               JP   loader

print.cursor:
wait4int:      IN   A,(249)
               AND  8
               JR   NZ,wait4int

old.cursor:    LD   A,0
               CALL get.pos
               XOR  A
               LD   DE,32
               LD   (HL),A
               ADD  HL,DE
               LD   (HL),A
               ADD  HL,DE
               LD   (HL),A
               ADD  HL,DE
               LD   (HL),A
               ADD  HL,DE
               LD   (HL),A

curs.blink:    LD   A,0
               INC  A
               BIT  3,A
               LD   (curs.blink+1),A
               RET  NZ

               LD   A,C
               LD   (old.cursor+1),A

               CALL get.pos
               LD   IX,4*8+colours
               LD   DE,32
               LD   (HL),%10000000
               LD   A,(IX)
               INC  IX
               SET  5,H
               LD   (HL),A
               RES  5,H
               ADD  HL,DE
               LD   (HL),%11000000
               LD   A,(IX)
               INC  IX
               SET  5,H
               LD   (HL),A
               RES  5,H
               ADD  HL,DE
               LD   (HL),%11100000
               LD   A,(IX)
               INC  IX
               SET  5,H
               LD   (HL),A
               RES  5,H
               ADD  HL,DE
               LD   (HL),%11000000
               LD   A,(IX)
               INC  IX
               SET  5,H
               LD   (HL),A
               RES  5,H
               ADD  HL,DE
               LD   (HL),%10000000
               LD   A,(IX)
               INC  IX
               SET  5,H
               LD   (HL),A
               RET

get.pos:
curs.offs:     LD   HL,3*192
               LD   DE,32
               OR   A
get.pos.lp:    RET  Z
               ADD  HL,DE
               DEC  A
               JR   get.pos.lp

;---------------------------------------------------------------

mes.oct:       DEFM "Oct: "
mes.drive:     DEFM "Drive 1: "
mes.label:     DEFM "Solar Flare"
mes.nolabel:   DEFM "No label   "
mes.nodisc:    DEFM "No disc    "
mes.noi:       DEFM "Noisetracker, 15 samples, 8 bits"
mes.pro:       DEFM "Protracker,   31 samples, 8 bits"
mes.sta:       DEFM "Startrekker,  31 samples, 8 bits"
mes.drv:       DEFM "Press RETURN for new directory. "
mes.size:      DEFM "127 song entries, 999k,         "
mes.no.date:   DEFM "no date "

option.dir:    DEFM " < 1: > "
               DEFM "new disc for drive 1"
               DEFB 128
               DEFB 0
               DEFM "xxxxxx"
               DEFW 0
               DEFB 0

               DEFM " < 2: > "
               DEFM "new disc for drive 2"
               DEFB 129
               DEFB 0
               DEFM "xxxxxx"
               DEFW 0
               DEFB 0

load.entries:  DEFB 0

loader.dir:    DEFM "filename"
               DEFM "20 char module title"
               DEFB 0        ;module type 0=noise, 1=pro, 2=star
                             ;  +64 = 4 bit compressed
               DEFB 0        ;length in patterns
               DEFM "010195" ;date stamp
               DEFW 0        ;total size in k
               DEFB 0        ;number of samples (len>1)

load.len:      EQU  8+20+1+1+6+2+1

               DEFS 27*load.len    ;max 27 on screen (25 files)

;put A at address DE and DE+1 in ascii format

cnv.a.to.de:   LD   C,-1
cad.getten:    SUB  10
               INC  C
               JR   NC,cad.getten
               ADD  10+"0"
               INC  DE
               LD   (DE),A
               DEC  DE
               LD   A,C
               ADD  "0"
               LD   (DE),A
               INC  DE
               INC  DE
               RET



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

               LD   HL,0
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
cls:
               LD   HL,0
               LD   DE,1
               LD   BC,6143
               LD   (HL),L
               LDIR

               LD   HL,8192
               LD   DE,8193
               LD   BC,6143
               LD   (HL),L
               LDIR

               LD   HL,palette+15
               LD   BC,16*256+248
               OTDR

               RET

print.de.b:    LD   A,(DE)
               INC  DE
               OR   A
               JR   Z,eop
               CALL print.chr
               DJNZ print.de.b
               RET

eop:           LD   A," "
               CALL print.chr
               DJNZ eop
               RET


colour.scrn:
               LD   (line.size+1),A

               LD   HL,8192
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
               CP   8192+6144/256
               JR   NZ,col.loop
               RET


;---------------------------------------------------------------

load.screen:   DEFM "SAM MOD player             v2.10"
               DEFM "(C) 1996 Stefan Drissen"
               DEFB 0,0,0,0,0,0,0
               DEFB 0,0,0,0,0,0,0,0
               DEFB 0,0,0,0,0,0,0,0
               DEFB 0,0,0,0,0,0,0
               DEFM "Use CURSORS + RETURN or JOYSTICK"


col.load:
               DEFB 3,2,1,3,25,1,2,3,1,5

device.scrn:   DEFM "SAM MOD player             v2.10"
               DEFM "(C) 1996 Stefan Drissen"
               DEFB 0,0
               DEFM "       SELECT SOUND DEVICE"
               DEFB 0,0
               DEFM " Colour Look Up Table     (test)"
               DEFM " Soundchip       (4 bits stereo)"
               DEFM " SAMdac on port 1(7 bits stereo)"
               DEFM " SAMdac on port 2"
               DEFB 0
               DEFM " DAC on port 1     (6 bits mono)"
               DEFM " DAC on port 2"
               DEFB 0
               DEFM " Blue Alpha Sampler(6 bits mono)"
               DEFM " Quazar Soundcard (8 bits surr.)"
               DEFB 0
               DEFM "The (SAM)DAC can be connected to"
               DEFM "parallel printer port 1 or 2."
               DEFB 0,0,0
               DEFM "       SELECT AMIGA SPEED"
               DEFB 0,0
               DEFM " PAL  (7.0937892 MHz)"
               DEFB 0
               DEFM " NTSC (7.1590905 MHz)"
               DEFB 0,0
               DEFM "This will only make a           "
               DEFM "noticeable difference if a MOD  "
               DEFM "consists of very long samples.  "
               DEFB 0,0
               DEFM "Use the CURSORS to make a       "
               DEFM "selection, press SPACE to toggle"
               DEFM "between DEVICE and SPEED, press "
               DEFM "RETURN to continue."
               DEFB 0

col.device:    DEFB 3,2,2,4,8,1,5,3,2,4,3,1,5,3,4,5

col.black:     DEFB 2,2,27,0,2,3,1,5

colours:
               DEFB 0,0,0,0,0,0,0,0                    ;0
               DEFB 1,2,3,2,1,0,0,0                    ;1
               DEFB 4,5,6,5,4,0,0,0                    ;2
               DEFB 7,9+56,3,9+56,7,0,0,0              ;3
               DEFB 10+56,11+56,12+56,11+56,10+56,0,0,0;4
               DEFB 13+56,14+56,6,14+56,13+56,0,0,0    ;5


font:
               MDAT "font"

;---------------------------------------------------------------
;directory stuff
dir.stuff:

m.vollabel:    DEFM "01234567890"


               LD   A,(driveselect+1)
               RLCA
               RLCA
               RLCA
               RLCA
               ADD  "A"
               RST  16
               LD   A,":"
               RST  16
               LD   HL,(path)

;---------------------------------------------------------------
program:


badcommand:
               LD   HL,msbadfile
               RET
badfilename:
               LD   HL,msbadname
               RET
filenotfound:
               LD   HL,msfilenot
               RET

invaliddir:    LD   HL,msinvdir
               RET

errnodisc:     LD   SP,(save.sam.sp+1)
               LD   A,1
               LD   (nodisc+1),A
               RET

commands:

msbadfile:     DEFB 13
               DEFM "Bad command or file name"
               DEFB 13,0

msbadname:     DEFB 13
               DEFM "Invalid file name"
               DEFB 13,0

msfilenot:     DEFB 13
               DEFM "File not found"
               DEFB 13,0

msinvdir:      DEFB 13
               DEFM "Invalid subdirectory"
               DEFB 13,0


parlast:       DEFW 0

parameter:     DEFS 255

parafile:      DEFS 11

matchfile:     DEFS 11

getparameter:
               LD   HL,(parlast)
               LD   DE,parameter
getparlp2:
               LD   A,(HL)
               LD   (DE),A
               OR   A
               RET  Z
               INC  HL
               CP   " "
               JR   Z,getparlp2

getparlp:
               LD   (DE),A
               INC  DE
               LD   A,(HL)
               INC  HL
               OR   A
               JR   Z,gpnomore
               CP   " "
               JR   Z,gpnomore
               CP   "\"
               JR   Z,gpnomore
               CP   "."
               JR   Z,gpnomore
               JR   getparlp
gpnomore:      DEC  HL
               LD   (parlast),HL
               XOR  A
               LD   (DE),A
               DEC  A
               RET


chdir:
               CALL getparameter
               JP   Z,badcommand
               CALL getinputpath
               CALL readroot
               CALL loadpath
               JP   C,invaliddir
               LD   HL,temppath
               LD   DE,(path)
               LD   BC,64
               LDIR
               RET


findfile:
               LD   (save.sam.sp+1),SP

               LD   DE,matchfile
               LD   BC,11
               LDIR
               CALL readroot
               CALL loadpath
               CALL C,resetpath
               LD   BC,(direntries)
               LD   HL,(data)
fmclp:         PUSH HL
               PUSH BC
               LD   B,11
               LD   DE,matchfile
fmblp:         LD   A,(DE)
               CP   (HL)
               INC  DE
               INC  HL
               JR   NZ,nomatch
               DJNZ fmblp
               POP  BC
               POP  HL
               RET
nomatch:
               POP  BC
               POP  HL
               LD   DE,32
               ADD  HL,DE
               DEC  BC
               LD   A,B
               OR   C
               JR   NZ,fmclp
               POP  AF             ;chuck return address
               JP   file.notfound


resetpath:
               LD   HL,(path)
               LD   (HL),"\"
               INC  HL
               LD   (HL),0
               CALL copypath
               JP   readroot
copypath:
               PUSH HL
               PUSH DE
               PUSH BC
               LD   HL,(path)
               LD   DE,temppath
               LD   BC,64
               LDIR
               POP  BC
               POP  DE
               POP  HL
               RET

loadpath:
               LD   A,(temppath+1)
               OR   A
               RET  Z
               LD   HL,temppath
               LD   (parlast),HL
               CALL getparameter
lploop:
               LD   A,(parameter)
               OR   A
               RET  Z
               CALL getinputfile

               PUSH HL
               LD   HL,(data)
               LD   BC,(direntries)
lpmatchlp:     LD   DE,parafile
               PUSH HL
               PUSH BC
               LD   B,11
lpmatchblp:    LD   A,(DE)
               CP   (HL)
               JR   NZ,lpnomatch
               INC  HL
               INC  DE
               DJNZ lpmatchblp
               POP  BC
               POP  IX
               LD   A,(IX+11)
               AND  %00010000
               JR   NZ,lpisdir
               PUSH IX
               PUSH BC
               JP   lpnomatch
lpisdir:
               LD   E,(IX+26)
               LD   D,(IX+27)

               LD   HL,(data)
               LD   BC,0
lpreadmore:
               PUSH BC
               CALL readcluster
               POP  BC
               INC  BC
               CALL getfatentry
               LD   A,D
               CP   15
               JR   NZ,lpreadmore
               LD   A,E
               CP   &F8
               JR   C,lpreadmore
               LD   HL,(bytescluster)
               SRL  H
               RR   L
               SRL  H
               RR   L
               SRL  H
               RR   L
               SRL  H
               RR   L
               SRL  H
               RR   L
               EX   DE,HL
               LD   HL,0
lpaddlp:       ADD  HL,DE
               DEC  BC
               LD   A,B
               OR   C
               JR   NZ,lpaddlp

               LD   (direntries),HL
               POP  HL
               JP   lploop
lpnomatch:
               POP  BC
               POP  HL
               LD   DE,32
               ADD  HL,DE
               DEC  BC
               LD   A,B
               OR   C
               JR   NZ,lpmatchlp
               POP  HL
               SCF
               RET


getinputpath:
               CALL copypath
               LD   HL,parameter
               LD   DE,temppath
               LD   A,(HL)
               CP   "\"
               JR   Z,gipnewpath
               LD   A,(DE)
               OR   A
               JR   Z,gipnewpath
gipfindend:
               INC  DE
               LD   A,(DE)
               OR   A
               JR   NZ,gipfindend
               LD   A,E
               CP   temppath+1\256
               JR   NZ,gipnewpath
               DEC  DE
gipnewpath:
               LD   A,"\"
               LD   (DE),A
               INC  DE

               LD   A,(parameter)
               CP   "\"
               JR   NZ,gipns
               LD   A,(parameter+1)
               OR   A
               JR   NZ,gipns
               PUSH DE
               CALL getparameter
               POP  DE
               LD   A,(parameter)
gipns:
               CP   "."
               JR   NZ,gipnotdot
               PUSH DE
               CALL getparameter
               POP  DE
               LD   A,(parameter)
               CP   "."
               JR   NZ,gipfndlstp
               DEC  DE
gipfndlstp:    DEC  DE
               LD   A,(DE)
               CP   "\"
               JR   NZ,gipfndlstp
               LD   A,E
               CP   temppath\256
               JR   NZ,$+3
               INC  DE
               XOR  A
               LD   (DE),A

               PUSH DE
               CALL getparameter
               POP  DE
               JP   gipdoneext
gipnotdot:
               PUSH DE
               CALL getinputfile
               POP  DE
               JP   C,invaliddir

               LD   HL,parafile
               LD   B,8
gipcopyname:   LD   A,(HL)
               CP   " "
               JR   Z,gipdonename
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ gipcopyname
gipdonename:
               LD   HL,parafile+8
               LD   A,(HL)
               CP   " "
               JR   Z,gipdoneext
               LD   A,"."
               LD   (DE),A
               INC  DE
               LD   B,3
gipcopyext:    LD   A,(HL)
               CP   " "
               JR   Z,gipdoneext
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ gipcopyext
gipdoneext:
               LD   A,(parameter)
               CP   "\"
               JR   Z,gipnewpath
gipend:
               XOR  A
               LD   (DE),A
               RET


getinputfile:
               LD   HL,parafile
               LD   B,11
clearpf:       LD   (HL)," "
               INC  HL
               DJNZ clearpf

               LD   HL,parameter
               LD   DE,parafile
               LD   A,(HL)
               CP   "."
               JR   Z,gifextonly
               LD   B,9
gifcopynm:     LD   A,(HL)
               INC  HL
               OR   A
               JR   Z,gifendname
               CP   "\"
               JR   Z,gifcopynm
               LD   (DE),A
               INC  DE
               DJNZ gifcopynm
               SCF                 ;file longer than 8 chars
               RET
gifendname:
               CALL getparameter
               JR   Z,gifendext

               LD   HL,parameter
               LD   A,(HL)
               CP   "."
               JR   NZ,gifendext
gifextonly:    INC  HL
               LD   DE,parafile+8
               LD   B,4
gifcopyext:    LD   A,(HL)
               OR   A
               JR   Z,gifendext
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ gifcopyext
               CALL getparameter
               SCF
               RET                ;extension longer than 3 chars
gifendext:
               XOR  A
               RET

select1:
               LD   A,0
               CALL setdrive
               LD   HL,patha
               LD   (path),HL
               RET

select2:
               LD   A,1
               CALL setdrive
               LD   HL,pathb
               LD   (path),HL
               RET


filecount:     DEFW 0

dir:
               LD   (save.sam.sp+1),SP

               CALL readroot

               XOR  A
               LD   (m.vollabel),A

               LD   A,(direntries)
               LD   HL,(data)
               LD   DE,11
               ADD  HL,DE
               LD   DE,32
               LD   B,A
prfindlabel:   LD   A,(HL)
               AND  8
               JR   NZ,prlabel
               ADD  HL,DE
               DJNZ prfindlabel
prlabel:
               JR   Z,prnolabel
               LD   DE,11
               XOR  A
               SBC  HL,DE
               LD   B,11
               LD   DE,m.vollabel
prlabblp:      LD   A,(HL)
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ prlabblp
prnolabel:
prdonelabel:
               CALL copypath
               CALL loadpath
               CALL C,resetpath

               RET


;===============================================================

checknodisc:
               PUSH BC
               PUSH HL
               LD   C,%11010000    ;force interrupt
               CALL dsendc
               LD   C,dstatcom
               CALL dconvert
               LD   HL,0
clearbit:
               DEC  HL
               LD   A,H
               OR   L
               JR   Z,cdisdisc
               IN   A,(C)
               BIT  1,A
               JR   NZ,clearbit
               LD   HL,0
cndlp:
               IN   A,(C)
               NOP
               NOP
               BIT  1,A
               JR   NZ,cdisdisc
               DEC  HL
               LD   A,H
               OR   L
               JR   NZ,cndlp
cdisdisc:      POP  BC
               POP  HL
               RET

;---------------------------------------------------------------
;get FAT entry
;DE= cluster

getfatentry:
               PUSH HL
               LD   H,D
               LD   L,E
               ADD  HL,HL
               ADD  HL,DE

               LD   DE,fat
               SRL  H
               RR   L
               JR   C,oddfat

               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   A,(HL)
               AND  15
               LD   D,A
               POP  HL
               RET

oddfat:
               ADD  HL,DE
               LD   A,(HL)
               RRCA
               RRCA
               RRCA
               RRCA
               AND  15
               LD   E,A
               INC  HL
               LD   A,(HL)
               LD   D,A
               RLCA
               RLCA
               RLCA
               RLCA
               AND  240
               OR   E
               LD   E,A
               SRL  D
               SRL  D
               SRL  D
               SRL  D
               POP  HL
               RET


;---------------------------------------------------------------
;read FAT at fixed address, (data) -> first address after FAT

readfat:
               PUSH HL
               LD   DE,1
               LD   HL,fat
               LD   A,(bssecsfat)
               LD   B,A
rfblp:         CALL rdlogsec
               INC  DE
               DJNZ rfblp
               LD   (data),HL
               POP  HL
               RET

;---------------------------------------------------------------
;read root directory
;hl= address

readroot:
               CALL rdboot
               CALL readfat
               CALL startcluster
               PUSH DE
               CALL startroot
               POP  HL
               XOR  A
               SBC  HL,DE
               LD   B,L
               PUSH BC
               CALL startroot
               POP  BC
               LD   HL,(data)
rdrtlp:
               CALL rdlogsec
               INC  DE
               DJNZ rdrtlp
               LD   A,1
               OR   A
               RET



;---------------------------------------------------------------
;read cluster from disc
;de= cluster number (2-711)
;hl= address

readcluster:
               PUSH DE
               PUSH HL
               DEC  DE
               DEC  DE
               LD   HL,0
               LD   A,(bsclusize)
               LD   B,A
rccalc:        ADD  HL,DE
               DJNZ rccalc
               CALL startcluster
               ADD  HL,DE
               EX   DE,HL
               POP  HL

               LD   A,(bsclusize)
               LD   B,A
rcclusrep:     CALL rdlogsec
               INC  DE
               DJNZ rcclusrep
               POP  DE
               RET

;---------------------------------------------------------------
;calculate start sector of root directory
;returns DE with

startroot:
               LD   DE,1
               LD   A,(bsnumfats)
               LD   B,A
rcfats:        LD   A,(bssecsfat)
               ADD  A,E
               LD   E,A
               DJNZ rcfats
               RET

;calculate total number of clusters on disc

calcclusters:
               LD   HL,(bstotsecs)
               CALL startcluster
               XOR  A
               SBC  HL,DE
               LD   A,(bsclusize)
ccdiv:
               SRL  A
               JR   Z,ccdonediv
               SRL  H
               RR   L
               JR   ccdiv
ccdonediv:
               LD   C,L
               LD   B,H
               RET

;calculate start sector of cluster 2 (first data cluster)
;returns DE with logical sector of first data cluster

startcluster:
               PUSH HL
               LD   HL,(bsrootentries)
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL
               ADD  HL,HL          ; * 32 bytes per entry
               LD   DE,0
               LD   BC,(bssecsize)
               XOR  A
scdivsecs:     SBC  HL,BC
               INC  E
               JR   NC,scdivsecs
               DEC  E
               EX   DE,HL
               CALL startroot
               ADD  HL,DE
               EX   DE,HL
               POP  HL
               RET

;---------------------------------------------------------------
;read logical sector from disc
;de= sector number (0 - 1439 (for 720k disc))
;hl= address

rdlogsec:
               PUSH BC
               PUSH DE
               PUSH HL
               EX   DE,HL
               LD   BC,(bssecstrack) ;number of secs per track
               LD   DE,0
               XOR  A
rlsdiv9:       SBC  HL,BC
               INC  D
               JR   NC,rlsdiv9
               ADC  HL,BC
               DEC  D
               LD   E,L
               LD   A,(bssides)
               CP   2
               JR   NZ,$+4
               RRC  D
               POP  HL
               CALL rdphysec
               POP  DE
               POP  BC
               RET


;---------------------------------------------------------------
;read physical sector from disc
;d = track (+128 for side 2)
;e = sector
;hl= address

dstatcom:      EQU  224
dtrack:        EQU  225
dsec:          EQU  226
ddata:         EQU  227

rdphysec:
             ; DI

               XOR  A
               LD   (derrcount+1),A
               LD   (rpshl+1),HL
rpsretry:

rpshl:         LD   HL,0
               PUSH DE
               LD   A,D
               AND  127
               LD   D,A
rpsnxtrk:
               CALL busy
               LD   C,dtrack
               CALL dconvert
               IN   A,(C)
               CP   D
               JR   Z,rpright
               LD   C,%01011000
               CALL mergestep
               JR   C,$+4
               SET  5,C
               CALL dsendc
               JR   rpsnxtrk
rpright:
               POP  DE
               LD   C,dsec
               CALL dconvert
               LD   A,E
               OUT  (C),A

               LD   C,%10000000
               CALL dsendc
               LD   C,dstatcom
               CALL dconvert
               LD   A,C
               LD   (dnodata+1),A
               LD   C,ddata
               CALL dconvert
               JR   dnodata
dread:
               INI
dnodata:
               IN   A,(dstatcom)
               BIT  1,A
               JR   NZ,dread
               RRCA
               JR   C,dnodata
               AND  %00001110

               JR   Z,finread
derrcount:
               LD   A,0
               INC  A

               LD   (derrcount+1),A
               CP   5
               JP   C,rpsretry

               PUSH HL
               PUSH DE
               PUSH BC
               LD   A,"0"
               BIT  7,D
               JR   Z,$+3
               INC  A
               LD   (mes.sec+12),A
               LD   A,D
               AND  127
               PUSH DE
               LD   DE,mes.sec+21
               CALL cnv.a.to.de
               POP  DE
               LD   A,E
               LD   DE,mes.sec+29
               CALL cnv.a.to.de

               LD   HL,31*6*32
               LD   DE,mes.sec
               LD   B,32
               CALL print.de.b

               POP  BC
               POP  DE
               POP  HL
               INC  H
               INC  H

finread:
             ; EI
               RET

mes.sec:       DEFM "ERROR: side 0, track 00, sec 00"

dsendc:
               PUSH BC
               CALL busy
               POP  BC
dsendnow:
               LD   A,C
               LD   C,dstatcom
               CALL dconvert
               OUT  (C),A
               LD   A,16
dwait:         DEC  A
               JR   NZ,dwait
               RET

busy:
               LD   C,dstatcom
               CALL dconvert
busylp:
               IN   A,(C)
               BIT  0,A
               JR   NZ,busylp
               RET

dconvert:
               PUSH AF
               BIT  7,D
               JR   Z,$+4
               SET  2,C
driveselect:   LD   A,0            ; +16 for drive 2
               OR   C
               LD   C,A
               POP  AF
               RET

setdrive:
               RLCA
               RLCA
               RLCA
               RLCA
               LD   (driveselect+1),A
               RET

mergestep:
               PUSH AF
               LD   A,(driveselect+1)
               OR   A
steprate.1:    LD   A,%11
               JR   Z,$+4
steprate.2:    LD   A,%00
               OR   C
               LD   C,A
               POP  AF
               RET

;---------------------------------------------------------------
;read boot sector from disc at fixed address

rdboot:
               CALL checknodisc
               JP   Z,errnodisc
               LD   C,%00000000
               CALL mergestep
               CALL dsendc
               LD   DE,1
               LD   HL,bootsector
               CALL rdphysec
               LD   HL,(bstotsecs)
               LD   DE,(bssecstrack)
               LD   A,D
               OR   E
               JR   Z,notpcdisc
               XOR  A
               LD   BC,0
rblp:          INC  BC
               SBC  HL,DE
               JR   NC,rblp
               DEC  BC
               ADD  HL,DE
               LD   A,H
               OR   L
               JR   NZ,notpcdisc
               LD   H,B
               LD   L,C
               LD   DE,(bssides)
               LD   A,D
               OR   E
               JR   Z,notpcdisc
               XOR  A
rblp2:         SBC  HL,DE
               JR   NC,rblp2
               ADD  HL,DE
               LD   A,H
               OR   L
               JR   NZ,notpcdisc

               LD   HL,0
               LD   BC,(bssecsize)
               LD   A,(bsclusize)
rbcalcsz:      ADD  HL,BC
               DEC  A
               JR   NZ,rbcalcsz
               LD   (bytescluster),HL
               LD   HL,(bsrootentries)
               LD   (direntries),HL

               LD   IX,col.black
               LD   A,6
               JP   colour.scrn


notpcdisc:
               XOR  A
               LD   (msdos+1),A
save.sam.sp:   LD   SP,0
               RET

bootsector:
               DEFS 3
bssysid:       DEFM "01234567"
bssecsize:     DEFW 0
bsclusize:     DEFB 0
bsressec:      DEFW 0
bsnumfats:     DEFB 0
bsrootentries: DEFW 0
bstotsecs:     DEFW 0
bsformatid:    DEFB 0
bssecsfat:     DEFW 0
bssecstrack:   DEFW 0
bssides:       DEFW 0
bshiddensecs:  DEFW 0,0
bsbigtot:      DEFW 0,0
bsphysdrv:     DEFB 0
               DEFW 0
bsvolserial:   DEFB 0,0,0,0
bsvolname:     DEFM "01234567890"

bytescluster:  DEFW 0

direntries:    DEFW 0

data:          DEFW 0     ;points to first address after FAT

fat:           EQU  2*192

temp.spc:      EQU  6144+9

;===============================================================

length:        EQU  $-16384

