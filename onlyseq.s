
;SAM MOD player version 2.04
;SEQUENCER (first execute "BURST" and install "DEMO")
;(C) 1995 Stefan Drissen
;last update: 2 December 1995, 17:40

;fixes in 2.04:
;- bug with tempo add (counter always reset to 0)

;fixes in 2.03:
;- bug when pattern break after loop command (break to loop row)
;- bug in pattern break to row (wrong row)
;- bug which stuffed loops which were near gap size
;+ speed and tempo reset upon looping
;* some code optimization


bp.page:                  ;found by searching for "BUR" at 00053
sq.page:       EQU  4     ;only used for assembly - relocatable
bp.sequence:   EQU  105   ;offset list located in burst page
mod.page:      EQU  5

get.patt:      EQU  10752 ;copies AHL -> current.row, 16 bytes
gp.ret.p:      EQU  10790 ;set with return page

far.call:      EQU  10794 ;call CHL
                          ;
far.ldir.1:    EQU  10813 ;copy BHL to buffer, C bytes (max128)
far.ldir.2:    EQU  10833 ;copy buffer to BDE, C bytes (max128)
                          ;buffer size = 128 bytes

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

buffer:        EQU  256+128 ;used by far.ldir, can be used for
                            ;other purposes if far.ldir not used

               ;volume tables start at 512

               ORG  32768
               DUMP sq.page,0

               JP   init.seq
               JP   install.mod

demo.loc:      DEFW 0              ;offset
               DEFB 0              ;& page of demo (foreground)
mp.peek:       DEFB mod.page       ;page mod loaded in at
octaves:       DEFB 3              ;number of octaves (3 or 5)
gap:           DEFB 0
instruments:   DEFB 0

               DEFM "MOD SEQUENCER 2.04 (C) 1995 Stefan Drissen"
               DEFM " Needs BURST to run, also (C) 1995 S.D. "

init.seq:
               DI
               IN   A,(250)
               LD   (is.lmpr+1),A
               LD   C,31
find.burst:
               INC  C
               LD   A,C

               CP   32+32+1
               JR   C,fb.pageok

               XOR  A
               OUT  (250),A
               RST  0
fb.pageok:
               OUT  (250),A
               LD   HL,53
               LD   A,(HL)
               CP   "B"
               JR   NZ,find.burst
               INC  L
               LD   A,(HL)
               CP   "U"
               JR   NZ,find.burst
               INC  L
               LD   A,(HL)
               CP   "R"
               JR   NZ,find.burst

               LD   A,C
               LD   (rs.bp.page+1),A

;---------------------------------------------------------------
;set up the finetune tables

               LD   HL,finet.tab
               LD   DE,finet.tab+1
               LD   BC,1023
               LD   (HL),255
               LDIR

               LD   DE,finelist
               LD   BC,4*12*256
set.finetune:
               LD   H,finet.tab/256
               LD   A,(DE)
               INC  DE
               LD   L,A
               LD   A,(DE)
               INC  DE
               ADD  A,H
               LD   H,A

               LD   (HL),C
               INC  C
               INC  C
               DJNZ set.finetune

;---------------------------------------------------------------

               LD   HL,c1
               LD   DE,c2
               LD   BC,4-1*routine.len
               LDIR

               LD   IX,build.list
reloc.loop:
               LD   L,(IX)
               LD   H,(IX+1)
               INC  IX
               INC  IX
               LD   A,H
               INC  A
               JR   Z,done.all
               LD   BC,c1
               ADD  HL,BC
               LD   A,4
reloc.4chan:
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               EX   DE,HL
               ADD  HL,BC
               EX   DE,HL
               LD   (HL),D
               DEC  HL
               LD   (HL),E
               EX   DE,HL
               LD   HL,routine.len
               ADD  HL,BC
               LD   C,L
               LD   B,H
               LD   HL,routine.len
               ADD  HL,DE
               DEC  A
               JR   NZ,reloc.4chan
               JR   reloc.loop
done.all:
               LD   HL,c1+mk.cur.pat+1
               LD   BC,routine.len-1
               LD   DE,current.row ;where pat data is dumped to
               LD   A,4
mk.cur.lp:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               ADD  HL,BC
               INC  E
               INC  E
               INC  E
               INC  E
               DEC  A
               JR   NZ,mk.cur.lp

               LD   HL,c1+channel.on+1
               LD   DE,c1.on
               LD   A,4
mk.c.on.lp:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               ADD  HL,BC
               INC  E
               DEC  A
               JR   NZ,mk.c.on.lp

               LD   IY,bp.sequence
               LD   A,(IY+00)      ;device
               LD   HL,is.normvol
               DEC  A              ;A=1 -> SAA
               JR   NZ,$+5
               LD   HL,is.saavol
               LD   DE,c1+saa.exvol
               LDI
               LDI
               LDI
               LDI
               LD   DE,c2+saa.exvol
               LDI
               LDI
               LDI
               LDI
               LD   DE,c3+saa.exvol
               LDI
               LDI
               LDI
               LDI
               LD   DE,c4+saa.exvol
               LDI
               LDI
               LDI
               LDI

               LD   IY,bp.sequence
               LD   L,(IY+01)
               LD   H,(IY+02)      ;sequencer call
               LD   (HL),sequencer\256
               INC  HL
               LD   (HL),sequencer/256
               LD   L,(IY+03)
               LD   H,(IY+04)
               IN   A,(251)
               LD   (HL),A         ;sequencer page
               LD   L,(IY+05)
               LD   H,(IY+06)      ;demo call
               LD   DE,(demo.loc)
               LD   (HL),E
               INC  HL
               LD   (HL),D
               LD   L,(IY+07)
               LD   H,(IY+08)
               LD   A,(demo.loc+2)
               LD   (HL),A         ;demo page
               LD   L,(IY+09)
               LD   H,(IY+10)      ;enable player
               LD   (enable.burst),HL

               IN   A,(251)
               LD   (gp.ret.p),A   ;return page for get.patt

               LD   L,(IY+11)
               LD   H,(IY+12)
               LD   (exit.burst),HL

               LD   BC,13
               ADD  IY,BC

               LD   IX,conv.list
               LD   B,6*4
put.in.blp:
               LD   E,(IY)
               LD   D,(IY+1)
               INC  IY
               INC  IY
put.in.lp:
               LD   L,(IX)
               LD   H,(IX+1)
               INC  IX
               INC  IX
               LD   A,H
               OR   L
               JR   Z,put.done.item

               LD   (HL),E
               INC  HL
               LD   (HL),D
               INC  HL
               JR   put.in.lp
put.done.item:
               DJNZ put.in.blp

is.lmpr:       LD   A,0
               OUT  (250),A
             ; EI
               RET

is.normvol:
               AND  %01111111
               ADD  2
               AND  %01111111
               ADD  2
               AND  %01111111
               ADD  2
               AND  %01111111
               ADD  2
is.saavol:
               AND  %01111110
               ADD  2
               AND  %01111110
               ADD  3
               AND  %01111110
               ADD  3
               AND  %01111110
               ADD  2
conv.list:
               DEFW c1+mk.pag1+1,c1+mk.pag2+1,c1+mk.pag3+1
               DEFW c1+mk.pag4+1,c1+mk.pag5+1,c1+mk.pag6+1
               DEFW c1+mk.pag7+1,c1+mk.pag8+1
               DEFW c1.mk.pag9+1,0
               DEFW c1+mk.off1+1,c1+mk.off2+1,c1+mk.off3+1
               DEFW c1+mk.off4+1,c1+mk.off5+1,c1+mk.off6+1
               DEFW c1+mk.off7+1,c1+mk.off8+1
               DEFW c1.mk.off9+1,0
               DEFW c1+mk.tab+1,0
               DEFW c1+mk.slo+1,0
               DEFW c1+mk.shi+1,0
               DEFW c1+mk.spfr+1,c1+mk.spfr2+1,0

               DEFW c2+mk.pag1+1,c2+mk.pag2+1,c2+mk.pag3+1
               DEFW c2+mk.pag4+1,c2+mk.pag5+1,c2+mk.pag6+1
               DEFW c2+mk.pag7+1,c2+mk.pag8+1
               DEFW c2.mk.pag9+1,0
               DEFW c2+mk.off1+1,c2+mk.off2+1,c2+mk.off3+1
               DEFW c2+mk.off4+1,c2+mk.off5+1,c2+mk.off6+1
               DEFW c2+mk.off7+1,c2+mk.off8+1
               DEFW c2.mk.off9+1,0
               DEFW c2+mk.tab+1,0
               DEFW c2+mk.slo+1,0
               DEFW c2+mk.shi+1,0
               DEFW c2+mk.spfr+1,c2+mk.spfr2+1,0

               DEFW c3+mk.pag1+1,c3+mk.pag2+1,c3+mk.pag3+1
               DEFW c3+mk.pag4+1,c3+mk.pag5+1,c3+mk.pag6+1
               DEFW c3+mk.pag7+1,c3+mk.pag8+1
               DEFW c3.mk.pag9+1,0
               DEFW c3+mk.off1+1,c3+mk.off2+1,c3+mk.off3+1
               DEFW c3+mk.off4+1,c3+mk.off5+1,c3+mk.off6+1
               DEFW c3+mk.off7+1,c3+mk.off8+1
               DEFW c3.mk.off9+1,0
               DEFW c3+mk.tab+1,0
               DEFW c3+mk.slo+1,0
               DEFW c3+mk.shi+1,0
               DEFW c3+mk.spfr+1,c3+mk.spfr2+1,0

               DEFW c4+mk.pag1+1,c4+mk.pag2+1,c4+mk.pag3+1
               DEFW c4+mk.pag4+1,c4+mk.pag5+1,c4+mk.pag6+1
               DEFW c4+mk.pag7+1,c4+mk.pag8+1
               DEFW c4.mk.pag9+1,0
               DEFW c4+mk.off1+1,c4+mk.off2+1,c4+mk.off3+1
               DEFW c4+mk.off4+1,c4+mk.off5+1,c4+mk.off6+1
               DEFW c4+mk.off7+1,c4+mk.off8+1
               DEFW c4.mk.off9+1,0
               DEFW c4+mk.tab+1,0
               DEFW c4+mk.slo+1,0
               DEFW c4+mk.shi+1,0
               DEFW c4+mk.spfr+1,c4+mk.spfr2+1,0


build.list:
               DEFW r0.000,r0.001,r0.002,r0.003,r0.004
               DEFW r0.005,r0.006,r0.007,r0.008,r0.009
               DEFW r0.010,r0.011,r0.012,r0.013,r0.014
               DEFW r0.015,r0.016,r0.017,r0.018,r0.019
               DEFW r0.020,r0.021,r0.022,r0.023,r0.024
               DEFW r0.025,r0.026,r0.027,r0.028,r0.029
               DEFW r0.030,r0.031,r0.032,r0.033,r0.034
               DEFW r0.035,r0.036,r0.037,r0.038,r0.039
               DEFW r0.040,r0.041,r0.042,r0.043,r0.044
               DEFW r0.045,r0.046,r0.047

               DEFW r1.001+1,r1.002+1,r1.003+1,r1.004+1,r1.005+1
               DEFW r1.006+1,r1.007+1,r1.008+1,r1.009+1,r1.010+1
               DEFW r1.011+1,r1.012+1,r1.013+1,r1.014+1,r1.015+1
               DEFW r1.016+1,r1.017+1,r1.018+1,r1.019+1,r1.020+1
               DEFW r1.021+1,r1.022+1,r1.023+1,r1.024+1,r1.025+1
               DEFW r1.026+1,r1.027+1,r1.028+1,r1.029+1,r1.030+1
               DEFW r1.031+1,r1.032+1,r1.033+1,r1.034+1,r1.035+1
               DEFW r1.036+1,r1.037+1,r1.038+1,r1.039+1,r1.040+1
               DEFW r1.041+1,r1.042+1,r1.043+1,r1.044+1,r1.045+1
               DEFW r1.046+1,r1.047+1,r1.048+1,r1.049+1,r1.050+1
               DEFW r1.051+1,r1.052+1,r1.053+1,r1.054+1,r1.055+1
               DEFW r1.056+1,r1.057+1,r1.058+1,r1.059+1,r1.060+1
               DEFW r1.061+1,r1.062+1,r1.063+1,r1.064+1,r1.065+1
               DEFW r1.066+1,r1.067+1,r1.068+1,r1.069+1,r1.070+1
               DEFW r1.071+1,r1.072+1,r1.073+1,r1.074+1,r1.075+1
               DEFW r1.076+1,r1.077+1,r1.078+1,r1.079+1,r1.080+1
               DEFW r1.081+1,r1.082+1,r1.083+1,r1.084+1,r1.085+1
               DEFW r1.086+1,r1.087+1,r1.088+1,r1.089+1,r1.090+1
               DEFW r1.091+1,r1.092+1,r1.093+1,r1.094+1,r1.095+1
               DEFW r1.096+1,r1.097+1,r1.098+1,r1.099+1,r1.100+1
               DEFW r1.101+1,r1.102+1,r1.103+1,r1.104+1,r1.105+1
               DEFW r1.106+1,r1.107+1,r1.108+1,r1.109+1,r1.110+1
               DEFW r1.111+1,r1.112+1,r1.113+1,r1.114+1,r1.115+1
               DEFW r1.116+1,r1.117+1,r1.118+1,r1.119+1,r1.120+1
               DEFW r1.121+1,r1.122+1,r1.123+1,r1.124+1,r1.125+1
               DEFW r1.126+1,r1.127+1,r1.128+1,r1.129+1,r1.130+1
               DEFW r1.131+1,r1.132+1,r1.133+1,r1.134+1,r1.135+1
               DEFW r1.136+1,r1.137+1,r1.138+1,r1.139+1,r1.140+1
               DEFW r1.141+1,r1.142+1,r1.143+1,r1.144+1,r1.145+1
               DEFW r1.146+1,r1.147+1,r1.148+1

               DEFW r2.001+2,r2.002+2,r2.003+2,r2.004+2,r2.005+2
               DEFW r2.006+2,r2.007+2,r2.008+2,r2.009+2,r2.010+2

               DEFW 65535

;------------- install mod -------------------------------------

;put code in section AB to manipulate data in section CD

install.mod:
               DI
               IN   A,(250)
               LD   (im.lmpr+1),A
               LD   (im.stsp+1),SP
               IN   A,(251)
               AND  31
               OR   32
               OUT  (250),A
               JP   im.low

               ORG  $-32768

im.low:
               LD   SP,16384

do.it:
               LD   A,(mp.peek-32768)
               OUT  (251),A

;give gap correct value, 3 for 3 octaves, 6 for 5 octaves

               LD   A,(octaves-32768)
               CP   5
               LD   A,3
               JR   NZ,$+3
               RLCA
               LD   (gap-32768),A

;clear sample table

               LD   HL,sample.table-32768
               LD   DE,sample.table-32767
               LD   BC,32*16-1
               LD   (HL),L
               LDIR

;figure out how many sample entries the mod contains (15 or 31)

               LD   DE,(32768+1080);20+31*30+2+128
findmk:        LD   HL,-&2E4D      ;"M."=  PROTRACKER 31.inst.
               OR   A
               ADC  HL,DE
               LD   A,31
               JR   Z,set.instr
               LD   HL,-&4C46      ;"FL"=  STARTREKKER 31 inst.
               OR   A
               ADC  HL,DE
               JR   Z,set.instr
               LD   A,15
set.instr:
               LD   (instruments-32768),A
               LD   HL,32768+20

;get start address of song table

               LD   DE,30          ;each sample takes 30 bytes
getpat:
               ADD  HL,DE
               DEC  A
               JR   NZ,getpat

               LD   A,(HL)         ;number of songtable entries
               LD   (song.len-32767),A
               INC  HL
               INC  HL

;copy song table to area within sequencer page

               PUSH HL             ;move song.tab
               LD   DE,song.tab-32768
               LD   BC,128
               LDIR
               POP  HL
               LD   B,128

;find highest pattern number in song table

               LD   A,(HL)
searchtable:
               CP   (HL)
               JR   NC,alreadyhi
               LD   A,(HL)
alreadyhi:
               INC  HL
               DJNZ searchtable
               INC  A

               PUSH AF

;get start address of first pattern

               LD   A,(instruments-32768)
               CP   31
               LD   DE,4           ;4 bytes extra for tag field
               JR   Z,m.k.
               LD   E,0
m.k.:
               ADD  HL,DE
               LD   A,H
               LD   (origpat.offsh-32767),A
               LD   A,L
               LD   (origpat.offsl-32767),A
               POP  AF

;A = number of pats, BHL = first pattern pointer
;each pattern = 1024 bytes

               LD   E,A
               IN   A,(251)
               LD   B,A

               LD   A,E
               AND  %00001111
               ADD  A,A
               ADD  A,A            ;*4
               ADD  H
               LD   H,A

               LD   A,E
               AND  %11110000
               RLCA
               RLCA
               RLCA
               RLCA
               ADD  B
               LD   B,A

;first sample starts directly after last pattern = BHL

get.samp.add:
               LD   IX,32768+20
               LD   IY,sample.table-32768

;- put starting addresses of samples in sample table by adding
;  the sample length to the current sample
;- fill in finetune, volume, does sample exist?
;- fill in loop type (0=none, 1=big, 2=small)


               LD   A,(instruments-32768)
               LD   C,A
convall:
               LD   (IY+st.start+0),L
               LD   (IY+st.start+1),H
               LD   (IY+st.start+2),B

               LD   A,(IX+24)
               AND  %00001111
               LD   (IY+st.finetune),A

               LD   A,(IX+25)      ;for volume
               CP   64
               JR   C,$+4
               LD   A,63
               LD   (IY+st.vol),A

               LD   D,(IX+22)      ;sample length
               LD   E,(IX+23)

               LD   A,D
               OR   E
               LD   A,0
               JR   Z,$+3
               DEC  A
               LD   (IY+st.sample),A

               PUSH HL
               LD   H,(IX+28)      ;loop len hi
               LD   L,(IX+29)      ;loop len lo
               SLA  L
               RL   H
               LD   A,1
               JR   C,nc.gotloop   ;bigloop
               LD   A,(gap-32768)
               DEC  A              ;new in 2.03
               CP   H              ;this is needed cos it should
               LD   A,1            ;be big if >= 3 and not > 3
               JR   C,nc.gotloop   ;bigloop
               LD   A,H
               OR   A
               LD   A,2
               JR   NZ,nc.gotloop  ;smallloop
               LD   A,L
               CP   4              ;no looping if loop len < 4
               LD   A,2
               JR   NC,nc.gotloop  ;no loop
               XOR  A
nc.gotloop:    ;A: 0=noloop, 1=bigloop, 2=smallloop

               LD   (IY+st.loop),A

               POP  HL

               CALL add.bhl.de2

               LD   DE,smp.tab.len
               ADD  IY,DE

               LD   DE,30
               ADD  IX,DE

               DEC  C
               JR   NZ,convall

;in dummy instrument only start address is valid data

               LD   (IY+st.start+0),L
               LD   (IY+st.start+1),H
               LD   (IY+st.start+2),B

;calculate how much space needs to be created to accomodate the
;endings and loopings of samples
;
;- a sample not looped needs one gap of silence following it
;- a sample with a loop greater than gap needs one gap with the
;  start of the loop following it
;- a sample with a loop smaller than gap needs three gap with
;  the whole loop repeated within it


               LD   IX,sample.table-32768
               LD   BC,smp.tab.len
               LD   A,(instruments-32768)
               LD   D,A
               LD   E,0
calct:
               LD   A,(IX+st.sample)
               OR   A
               JR   Z,calctnosmp

               LD   A,(IX+st.loop)
               CP   2              ;small loop needs three gaps
               LD   A,E
               JR   NZ,cs.not.sm
               ADD  2
cs.not.sm:
               INC  A
               LD   E,A
calctnosmp:
               ADD  IX,BC
               DEC  D
               JR   NZ,calct


;create the space necessary by moving the samples to higher add.

;IX = entry past last sample table entry
;E  = number of bytes (gap*256) that needs to be spaced out

               LD   B,E
shift.all:
               PUSH BC

               PUSH BC

               LD   L,(IX+st.start+0)
               LD   H,(IX+st.start+1)
               LD   B,(IX+st.start+2)

               LD   A,B
               OUT  (251),A        ;BHL=address last byte of
                                   ;    sample +1
               PUSH HL
               SET  6,H
               DEC  HL
               LD   (source.lo+1),HL
               DEC  A
               LD   (source.hi+1),A
               POP  HL             ;source is AHL in block D
                                   ;last byte of sample
               PUSH HL
               LD   E,(IX-min.start+2)
               LD   D,(IX-min.start+1)
               LD   C,(IX-min.start+0)
               XOR  A
               SBC  HL,DE
               JR   NC,nooverflow
               LD   DE,16384
               ADD  HL,DE
               SCF
nooverflow:
               LD   A,B
               SBC  C
               LD   (result.lo+1),HL ;number of bytes to be
               LD   (result.hi+1),A  ;copied put in result
               POP  HL

               POP  BC

               IN   A,(251)
               LD   E,A

               LD   A,(gap-32768)
               LD   C,A
im.new.start:
               LD   A,B            ;bytes to be shifted (* 256)
               ADD  A,H
               LD   H,A
               JR   NC,nopinc1
               INC  E
               INC  E
               SET  7,H
nopinc1:
               DEC  C
               JR   NZ,im.new.start

               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  E
               LD   A,E
               OUT  (251),A

               LD   (IX+st.start+0),L ;new start address of
               LD   (IX+st.start+1),H ;sample at shifted
               LD   (IX+st.start+2),A ;position

               DEC  A
               OUT  (251),A
               SET  6,H
               DEC  HL

               LD   A,(IX-min.sample)       ;st.sample-16
               OR   A
               JR   Z,noaddgap

;how much gap needed behind sample

               LD   A,(IX-min.loop)      ;-16+st.loop
               CP   2
               LD   A,(gap-32768)
               JR   NZ,gp.only1
               LD   C,A
               ADD  A,A
               ADD  A,C            ;3*gap
gp.only1:

;clear needed gap * 256 to no sound

               LD   C,A
               XOR  A               ;0=no volume
               LD   B,A
clearend:
               LD   (HL),A
               DEC  HL
               DJNZ clearend
               DEC  C
               JR   NZ,clearend
noaddgap:
               IN   A,(251)
               BIT  6,H
               SET  6,H
               JR   NZ,$+3
               DEC  A

;now pointing to address before gap

               LD   (target.lo+1),HL
               LD   (target.hi+1),A

;so lddr copy sample from old position to higher address

copy.loop:
result.lo:     LD   HL,0
result.hi:     LD   A,0
               LD   BC,move.size
               OR   A
               SBC  HL,BC
               JR   NC,keepcopying
               SUB  1
               JR   C,nomoreadd
               LD   DE,16384
               ADD  HL,DE
               JR   keepcopying
nomoreadd:
               ADD  HL,BC
               LD   C,L
               LD   B,H
keepcopying:
               LD   (result.lo+1),HL
               LD   (result.hi+1),A

               LD   A,B
               OR   C
               JR   Z,nocopy

source.lo:     LD   HL,0
source.hi:     LD   A,0
               OUT  (251),A
               LD   DE,move.spc+move.size-1
               PUSH BC
               LDDR
               BIT  6,H
               SET  6,H
               JR   NZ,$+3
               DEC  A
               LD   (source.hi+1),A
               LD   (source.lo+1),HL

               LD   HL,move.spc+move.size-1
target.lo:     LD   DE,0
target.hi:     LD   A,0
               OUT  (251),A
               POP  BC
               LDDR
               BIT  6,D
               SET  6,D
               JR   NZ,$+3
               DEC  A
               LD   (target.hi+1),A
               LD   (target.lo+1),DE
nocopy:
               LD   A,(result.hi+1)
               INC  A              ;stop when A=255
               JR   NZ,copy.loop

               LD   BC,-smp.tab.len
               ADD  IX,BC

               LD   A,(IX+st.sample)
               OR   A
               LD   A,0
               JR   Z,nosamplegap

;get number of gaps used

               LD   A,(IX+st.loop)
               CP   2
               LD   A,0
               JR   NZ,$+4
               ADD  2              ;for small loop
               INC  A
nosamplegap:
               LD   E,A

               POP  BC
               LD   A,B            ;update how much to move
               SUB  E
               LD   B,A
               JP   NZ,shift.all

;check ALL samples for Noisetracker bug
;-> loop offset in bytes instead of in words
;   loopoffset*2+looplen*2>samplelen -> bugged
;"soul-o-matic bug"
;-> loop len = end loop offset
;"approximity byg"
;-> loop len > len sample -> loop len = len sample


               LD   IY,sample.table-32768
               LD   IX,32768+20
               LD   A,(instruments-32768)
               LD   B,A
               LD   A,(mp.peek-32768)
               OUT  (251),A
find.bug.lp:
               PUSH BC
               LD   A,(IY+st.loop)
               OR   A
               JR   Z,find.lp.ok   ;no loop -> no bug
               LD   H,(IX+22)      ;sample len
               LD   L,(IX+23)
               LD   D,(IX+28)      ;loop len
               LD   E,(IX+29)
               OR   A
               SBC  HL,DE          ;assuming loop len<sample len
               JR   NC,assumpt.ok
               ADD  HL,DE
               LD   (IX+28),H      ;set loop len to sample len
               LD   (IX+29),L
               LD   HL,0           ;sample len-loop len
assumpt.ok:
               LD   D,(IX+26)      ;loop offs
               LD   E,(IX+27)
               SBC  HL,DE
               JR   C,found.bug    ;loop offs+len>sample len
               JR   find.lp.ok
found.bug:
               ADD  HL,DE
               SRL  D
               RR   E
               OR   A
               SBC  HL,DE
               JR   C,loop.bug.2
               LD   A,1
               JR   found.bugged

;in SOUL-O-MATIC the loop len is given as loop end offset

loop.bug.2:
               LD   A,2
               JR   found.bugged

find.lp.ok:
               LD   BC,smp.tab.len
               ADD  IY,BC
               LD   BC,30
               ADD  IX,BC
               POP  BC
               DJNZ find.bug.lp
               XOR  A
found.bugged:             ;A=0 -> normal looping
                          ;A=1 -> noisetracker bugged loop
                          ;A=2 -> soul-o-matic bug
               LD   (loop.bug+1),A

;fill in end addresses of samples in sample table by adding
;sample length bytes (*2) to start address OR if the sample is
;looped by adding the offset + the sample length

               LD   IY,sample.table-32768
               LD   IX,32768+20

               LD   A,(instruments-32768)
               LD   C,A
nc.fillend:
               LD   L,(IY+st.start+0)
               LD   H,(IY+st.start+1)
               LD   B,(IY+st.start+2)

               LD   D,(IX+22)      ;sample length
               LD   E,(IX+23)

               LD   A,(IY+st.loop)
               OR   A
               JR   Z,nc.notloop

               LD   A,(loop.bug+1)
               OR   A
               JR   Z,nc.normal

               DEC  A
               JR   Z,nc.noisebug

;soul-o-matic bug
               LD   D,(IX+28)      ;loop len hi
               LD   E,(IX+29)      ;loop len lo
               CALL add.bhl.de2
               JR   nc.got.gap

nc.normal:
               LD   D,(IX+26)      ;loop offs (in words)
               LD   E,(IX+27)
               CALL add.bhl.de
               JR   nc.contnorm
nc.noisebug:
               LD   D,(IX+26)      ;loop offs (in bytes!)
               LD   E,(IX+27)
nc.contnorm:   CALL add.bhl.de
               LD   D,(IX+28)      ;loop len (in words)
               LD   E,(IX+29)
nc.notloop:    CALL add.bhl.de2
nc.got.gap:
               LD   (IY+st.end+0),L
               LD   (IY+st.end+1),H
               LD   (IY+st.end+2),B

               LD   DE,smp.tab.len
               ADD  IY,DE
               LD   DE,30
               ADD  IX,DE

               DEC  C
               JR   NZ,nc.fillend


;fill in GAP to accomodate looped samples, small and large

               LD   IY,sample.table-32768
               LD   IX,32768+20
               LD   A,(instruments-32768)
               LD   B,A
conv.looping:
               PUSH BC
               LD   A,(mp.peek-32768)
               OUT  (251),A

               LD   A,(IY+st.loop)
               OR   A
               JP   Z,conv.donelp

               LD   H,(IX+22)      ;sample len
               LD   L,(IX+23)
               LD   D,(IX+26)      ;loop offs
               LD   E,(IX+27)
loop.bug:      LD   A,0
               OR   A
               JR   Z,loop.ok
               DEC  A
               JR   Z,noise.bug
               JR   soul.bug

;bug in noisetracker code, loop offset in bytes instead of words

noise.bug:
               SRL  D
               RR   E
               JR   loop.ok

;in SOUL-O-MATIC the loop len is given as loop end offset

soul.bug:
               OR   A
               SBC  HL,DE
               LD   (IX+28),H      ;now loop len = loopend off -
               LD   (IX+29),L      ;               loopstart off


loop.ok:                           ;DE = loop offset
               LD   L,(IY+st.start+0)
               LD   H,(IY+st.start+1)
               LD   B,(IY+st.start+2)

               CALL add.bhl.de2

               LD   A,(IY+st.loop)
               DEC  A
               JR   Z,bigloop

;small loop, can be up to gap in size, it has to cover the end
;loop marker meaning that three gaps are needed
;if the loop is gap-1 then the first marker is after 2*(gap-1)
;at the next frame it is possible that the sample will then be
;at position 3*(gap-3).  Then the difference between the current
;position and the first repeat end after gap can be added to gap
;to get new position. - follow that?  It took me a while to
;think that one up... 8-)

               LD   A,(gap-32768)
               LD   D,A
               ADD  A,A
               ADD  A,D
               LD   D,A
               LD   E,0
               LD   (totalgap+1),DE ;DE<gap

               LD   A,B

            ;  LD   DE,move.spc
               LD   B,(IX+28)      ;loop length
               LD   C,(IX+29)
               SLA  C
               RL   B

               OUT  (251),A
               LD   (sm.lp.src+1),HL

            ;  PUSH BC
            ;  LDIR
            ;  POP  BC

               LD   E,(IY+st.end+0)
               LD   D,(IY+st.end+1)

            ;  LD   A,(IY+st.end+2) = equal to start loop
            ;  OUT  (251),A

               OR   A
               SBC  HL,DE          ;if end lower than sample
               JR   C,$+4          ;then increase page positon
               SET  6,D            ;of end by setting bit 6

               PUSH AF             ;make small loop pointer
               LD   HL,0           ;at least gap size
sm.loop.big:   ADD  HL,BC          ;-> possibly 2*(gap-1)
               LD   A,(gap-32768)
               CP   H
               JR   NC,sm.loop.big
               POP  AF

               ADD  HL,DE
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  A
               LD   (IY+st.loope+0),L
               LD   (IY+st.loope+1),H
               LD   (IY+st.loope+2),A

;keep copying loop until 3*gap space is filled

copysmalllp:
totalgap:      LD   HL,0
               OR   A
               SBC  HL,BC
               LD   (totalgap+1),HL
               JR   C,im.donemost
               JR   Z,im.doneall
sm.lp.src:     LD   HL,0
               PUSH BC
               LDIR
               POP  BC
               JR   copysmalllp
im.donemost:
               ADD  HL,BC
               LD   C,L
               LD   B,H
               LD   HL,(sm.lp.src+1)
               LDIR
im.doneall:
               JP   conv.donelp

bigloop:
               LD   (IY+st.loops+0),L
               LD   (IY+st.loops+1),H
               LD   (IY+st.loops+2),B

;copy start of loop to gap after end loop for gap bytes

               LD   A,B
               OUT  (251),A
               LD   DE,move.spc
               LD   C,0
               LD   A,(gap-32768)
               LD   B,A
               PUSH BC
               LDIR

               LD   E,(IY+st.end+0)
               LD   D,(IY+st.end+1)
               LD   A,(IY+st.end+2)
               OUT  (251),A
               LD   HL,move.spc
               POP  BC
               LDIR                ;copy to beginloop


conv.donelp:
               LD   BC,smp.tab.len
               ADD  IY,BC
               LD   BC,30
               ADD  IX,BC
               POP  BC
               DEC  B
               JP   NZ,conv.looping

             ; LD   A,31
             ; LD   HL,sample.table-32768
             ; LD   BC,16

;increase sample start with two bytes (Amiga wierdo)

upfirst:
             ; LD   E,(HL)
             ; INC  HL
             ; LD   D,(HL)
             ; INC  DE
             ; INC  DE             ; or maybe not?
             ; LD   (HL),D
             ; DEC  HL
             ; LD   (HL),E
             ; ADD  HL,BC
             ; DEC  A
             ; JR   NZ,upfirst
exit.install:

;create an empty sample if necessary

               LD   B,31
               LD   IY,sample.table-32768
               LD   DE,16
ce.findblank:  LD   A,(IY+st.sample)
               OR   A
               JR   Z,ce.nosample  ;no sample -> no gap
               LD   A,(IY+st.loop)
               OR   A
               JR   NZ,ce.nosample ;looped -> no gap
               LD   L,(IY+st.end+0)
               LD   H,(IY+st.end+1)
               LD   C,(IY+st.end+2)
               JR   ce.gotblank
ce.nosample:
               ADD  IY,DE
               DJNZ ce.findblank
               LD   L,(IY+st.start+0)
               LD   H,(IY+st.start+1)
               LD   C,(IY+st.start+2)
               LD   A,C
               OUT  (251),A
               LD   A,(gap-32768)
               LD   C,A
               LD   B,0
ce.dosilence:  LD   (HL),0         ;create silent sample
               INC  HL             ;at postion after all other
               DJNZ ce.dosilence   ;samples
               DEC  C
               JR   NZ,ce.dosilence
               IN   A,(251)
               LD   C,A

;now CHL is address of silent sound gap
;replace all blank samples with address for silence

ce.gotblank:
               LD   IY,sample.table-32768
               LD   B,31
fs.blp:        LD   A,(IY+st.sample)
               OR   A
               JR   NZ,fs.issamp
               LD   (IY+st.start+0),L
               LD   (IY+st.start+1),H
               LD   (IY+st.start+2),C
               LD   (IY+st.end+0),L
               LD   (IY+st.end+1),H
               LD   (IY+st.end+2),C
fs.issamp:
               ADD  IY,DE
               DJNZ fs.blp

               IN   A,(250)
               AND  31
               OUT  (251),A

               JP   fs.high


;add DE to BHL, result -> HL 0-16383 bit 7 unchanged, B 0-31

add.bhl.de:
               PUSH DE
               LD   A,D
               AND  %11000000
               RLCA
               RLCA
               ADD  B
               LD   B,A
               LD   A,D
               AND  %00111111
               LD   D,A
               ADD  HL,DE
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  B
               POP  DE
               RET

;add DE * 2 to BHL, result -> same as above

add.bhl.de2:
               PUSH DE
               LD   A,D
               AND  %11100000
               RLCA
               RLCA
               RLCA
               ADD  B
               LD   B,A
               LD   A,D
               AND  %00011111
               LD   D,A
               ADD  HL,DE
               ADD  HL,DE
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  B
               POP  DE
               RET


               ORG  $+32768
fs.high:
rs.bp.page:    LD   A,0               ;burst page
               OR   32
               OUT  (250),A
               LD   SP,32768

               LD   A,C
c1.mk.off9:    LD   (0),HL
c1.mk.pag9:    LD   (0),A
               LD   (c1+len+1),HL
               LD   (c1+page.len+1),A
c2.mk.off9:    LD   (0),HL
c2.mk.pag9:    LD   (0),A
               LD   (c2+len+1),HL
               LD   (c2+page.len+1),A
c3.mk.off9:    LD   (0),HL
c3.mk.pag9:    LD   (0),A
               LD   (c3+len+1),HL
               LD   (c3+page.len+1),A
c4.mk.off9:    LD   (0),HL
c4.mk.pag9:    LD   (0),A
               LD   (c4+len+1),HL
               LD   (c4+page.len+1),A

               LD   HL,31*16+sample.table-1
               LD   DE,32*16+sample.table-1
               LD   BC,31*16
               LDDR
               LD   HL,sample.table
               LD   DE,sample.table+1
               LD   BC,smp.tab.len-1
               LD   (HL),B
               LDIR

;put correct maximum & minimum periods into sequencer

               LD   HL,113
               LD   DE,856
               LD   A,(octaves)
               CP   5
               JR   NZ,mm.per.3
               LD   HL,56
               LD   DE,1023        ;octave 0 not fully supported
mm.per.3:
               LD   (c1+min.period+1),HL
               LD   (c1+max.period+1),DE
               LD   (c2+min.period+1),HL
               LD   (c2+max.period+1),DE
               LD   (c3+min.period+1),HL
               LD   (c3+max.period+1),DE
               LD   (c4+min.period+1),HL
               LD   (c4+max.period+1),DE

reset.song:
               LD   A,255
               LD   HL,c1.on
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               LD   (HL),A
               INC  L
               CPL
               LD   (HL),A         ;update volume?

               LD   HL,&500
               LD   (counter.fract),HL
               LD   A,H
               INC  A
               LD   (speed),A
               LD   HL,&100
               LD   (tempo),HL

             ; LD   (disable.pos),A ;no position jumping (temp)

               XOR  A
               LD   (pat.delay.c+1),A
               LD   (song.pos),A
               LD   (pattern.pos),A
               LD   (mstatus),A

               LD   IX,reset.list
               LD   DE,routine.len
reset.loop:
               LD   L,(IX+0)
               LD   H,(IX+1)
               LD   A,L
               OR   H
               JR   Z,reset.all
               LD   A,(IX+2)
               INC  IX
               INC  IX
               INC  IX
               LD   B,4
reset.blp:     LD   (HL),A
               ADD  HL,DE
               DJNZ reset.blp
               JR   reset.loop
reset.all:
               CALL c1+bp.volume
               CALL c2+bp.volume
               CALL c3+bp.volume
               CALL c4+bp.volume

               CALL c1+per.nop
               CALL c2+per.nop
               CALL c3+per.nop
               CALL c4+per.nop

;---------------------------------------------------------------

im.stsp:       LD   SP,0

im.lmpr:       LD   A,0
               OUT  (250),A

             ; EI
               RET

reset.list:
               DEFW c1+repeat+1
               DEFB 0
               DEFW c1+volume+1
               DEFB 0
               DEFW c1+period+1
               DEFB 0
               DEFW c1+period+2
               DEFB 0
               DEFW c1+wanted.per+1
               DEFB 0
               DEFW c1+wanted.per+2
               DEFB 0
               DEFW c1+upmask+1
               DEFB 255
               DEFW c1+dnmask+1
               DEFB 255
               DEFW c1+wav.cntrl+1
               DEFB 0
               DEFW c1+tonespeed+1
               DEFB 0
               DEFW c1+gliss+1
               DEFB 0
               DEFW c1+vibr.cmnd+1
               DEFB 0
               DEFW c1+vibr.pos+1
               DEFB 0
               DEFW c1+trem.cmnd+1
               DEFB 0
               DEFW c1+trem.pos+1
               DEFB 0
               DEFW c1+sampoffs+1
               DEFB 0
               DEFW c1+loopcount+1
               DEFB 0
               DEFW c1+pattpos+1
               DEFB 0
               DEFW c1+new.ins+1
               DEFB 0
               DEFW c1+cur.ins+1
               DEFB 0
               DEFW 0


;===============================================================


volume.table:  EQU  512

pitch.table:   EQU  32*256+512     ;pitch conversion table


;============= sequencer =======================================

gap256:
               DEFS gap256/256+1*256-$

song.tab:      DEFS 256

smp.tab.len:   EQU  16

sample.table:
st.start:      EQU  0              ;start.o,start.p
               DEFB 0,0,0
st.end:        EQU  3              ;end.o,end.p = start gap
               DEFB 0,0,0
st.loop:       EQU  6              ;loop: 0=none, 1=small, 2=big
               DEFB 0
st.loope:      EQU  7              ;small -> loop end in gap
st.loops:      EQU  7              ;big   -> loop start
               DEFB 0,0,0
st.vol:        EQU  10             ;volume
               DEFB 0
st.finetune:   EQU  11             ;fine tune value
               DEFB 0
st.sample:     EQU  12             ;empty sample?
               DEFB 0
               DEFB 0,0,0          ;unused

min.loop:      EQU  16-st.loop
min.start:     EQU  16-st.start-2
min.sample:    EQU  16-st.sample

               DEFS 31*smp.tab.len


finet.tab:     DEFS 1024


finelist:      DEFW 856,808,762,720,678,640,604,570,538,508,480
               DEFW 453
               DEFW 428,404,381,360,339,320,302,285,269,254,240
               DEFW 226
               DEFW 214,202,190,180,170,160,151,143,135,127,120
               DEFW 113                           ;tuning 0
               DEFW 107,101,95,90,85,80,75,71,67,63,60,56
               DEFS 128-96

               DEFW 850,802,757,715,674,637,601,567,535,505,477
               DEFW 450
               DEFW 425,401,379,357,337,318,300,284,268,253,239
               DEFW 225
               DEFW 213,201,189,179,169,159,150,142,134,126,119
               DEFW 113                           ;tuning 1
               DEFW 106,100,94,89,84,79,75,71,67,63,59,56
               DEFS 128-96

               DEFW 844,796,752,709,670,632,597,563,532,502,474
               DEFW 447
               DEFW 422,398,376,355,335,316,298,282,266,251,237
               DEFW 224
               DEFW 211,199,188,177,167,158,149,141,133,125,118
               DEFW 112                           ;tuning 2
               DEFW 105,99,94,88,83,79,74,70,66,62,59,56
               DEFS 128-96

               DEFW 838,791,746,704,665,628,592,559,528,498,470
               DEFW 444
               DEFW 419,395,373,352,332,314,296,280,264,249,235
               DEFW 222
               DEFW 209,198,187,176,166,157,148,140,132,125,118
               DEFW 111                           ;tuning 3
               DEFW 104,99,93,88,83,78,74,70,66,62,59,55
               DEFS 128-96

               DEFW 832,785,741,699,660,623,588,555,524,495,467
               DEFW 441
               DEFW 416,392,370,350,330,312,294,278,262,247,233
               DEFW 220
               DEFW 208,196,185,175,165,156,147,139,131,124,117
               DEFW 110                           ;tuning 4
               DEFW 104,98,92,87,82,78,73,69,65,62,58,55
               DEFS 128-96

               DEFW 826,779,736,694,655,619,584,551,520,491,463
               DEFW 437
               DEFW 413,390,368,347,328,309,292,276,260,245,232
               DEFW 219
               DEFW 206,195,184,174,164,155,146,138,130,123,116
               DEFW 109                           ;tuning 5
               DEFW 103,97,92,87,82,77,73,69,65,61,58,54
               DEFS 128-96

               DEFW 820,774,730,689,651,614,580,547,516,487,460
               DEFW 434
               DEFW 410,387,365,345,325,307,290,274,258,244,230
               DEFW 217
               DEFW 205,193,183,172,163,154,145,137,129,122,115
               DEFW 109                           ;tuning 6
               DEFW 102,96,91,86,81,77,72,68,64,61,57,54
               DEFS 128-96

               DEFW 814,768,725,684,646,610,575,543,513,484,457
               DEFW 431
               DEFW 407,384,363,342,323,305,288,272,256,242,228
               DEFW 216
               DEFW 204,192,181,171,161,152,144,136,128,121,114
               DEFW 108                           ;tuning 7
               DEFW 102,96,90,85,80,76,72,68,64,60,57,54
               DEFS 128-96

               DEFW 907,856,808,762,720,678,640,604,570,538,508
               DEFW 480
               DEFW 453,428,404,381,360,339,320,302,285,269,254
               DEFW 240
               DEFW 226,214,202,190,180,170,160,151,143,135,127
               DEFW 120                           ;tuning -8
               DEFW 113,107,101,95,90,85,80,75,71,67,63,60
               DEFS 128-96

               DEFW 900,850,802,757,715,675,636,601,567,535,505
               DEFW 477
               DEFW 450,425,401,379,357,337,318,300,284,268,253
               DEFW 238
               DEFW 225,212,200,189,179,169,159,150,142,134,126
               DEFW 119                           ;tuning -7
               DEFW 112,106,100,94,89,84,79,75,71,67,63,59
               DEFS 128-96

               DEFW 894,844,796,752,709,670,632,597,563,532,502
               DEFW 474
               DEFW 447,422,398,376,355,335,316,298,282,266,251
               DEFW 237
               DEFW 223,211,199,188,177,167,158,149,141,133,125
               DEFW 118                           ;tuning -6
               DEFW 111,105,99,94,88,83,79,74,70,66,62,59
               DEFS 128-96

               DEFW 887,838,791,746,704,665,628,592,559,528,498
               DEFW 470
               DEFW 444,419,395,373,352,332,314,296,280,264,249
               DEFW 235
               DEFW 222,209,198,187,176,166,157,148,140,132,125
               DEFW 118                           ;tuning -5
               DEFW 111,104,99,93,88,83,78,74,70,66,62,59
               DEFS 128-96

               DEFW 881,832,785,741,699,660,623,588,555,524,494
               DEFW 467
               DEFW 441,416,392,370,350,330,312,294,278,262,247
               DEFW 233
               DEFW 220,208,196,185,175,165,156,147,139,131,123
               DEFW 117                           ;tuning -4
               DEFW 110,104,98,92,87,82,78,73,69,65,61,58
               DEFS 128-96

               DEFW 875,826,779,736,694,655,619,584,551,520,491
               DEFW 463
               DEFW 437,413,390,368,347,328,309,292,276,260,245
               DEFW 232
               DEFW 219,206,195,184,174,164,155,146,138,130,123
               DEFW 116                           ;tuning -3
               DEFW 109,103,97,92,87,82,77,73,69,65,61,58
               DEFS 128-96

               DEFW 868,820,774,730,689,651,614,580,547,516,487
               DEFW 460
               DEFW 434,410,387,365,345,325,307,290,274,258,244
               DEFW 230
               DEFW 217,205,193,183,172,163,154,145,137,129,122
               DEFW 115                           ;tuning -2
               DEFW 108,102,96,91,86,81,77,72,68,64,61,57
               DEFS 128-96

               DEFW 862,814,768,725,684,646,610,575,543,513,484
               DEFW 457
               DEFW 431,407,384,363,342,323,305,288,272,256,242
               DEFW 228
               DEFW 216,203,192,181,171,161,152,144,136,128,121
               DEFW 114                           ;tuning -1
               DEFW 108,101,96,90,85,80,76,72,68,64,60,57
               DEFS 128-96

;first 64 bytes of bpm table are not used, so use this space
;for arpeggio table (32 bytes) and vibrato table (32 bytes)
;instead.

bpm.table:         ;x/125*256 (125 = standard bpm)

arpeg.table:                       ;x mod 3
               DEFB 0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0
               DEFB 1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1
vibrato.table:
               DEFB 000,024,049,074,097,120,141,161
               DEFB 180,197,212,224,235,244,250,253
               DEFB 255,253,250,244,235,224,212,197
               DEFB 180,161,141,120,097,074,049,024

             ; DEFW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;  0
             ; DEFW 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; 16
               DEFW 066,068,070,072,074,076,078,080 ; 32
               DEFW 082,084,087,089,091,093,095,097 ; 40
               DEFW 099,101,103,105,107,109,111,113 ; 48
               DEFW 115,117,119,121,123,125,127,130 ; 56
               DEFW 132,134,136,138,140,142,144,146 ; 64
               DEFW 148,150,152,154,156,158,160,162 ; 72
               DEFW 164,166,168,170,173,175,177,179 ; 80
               DEFW 181,183,185,187,189,191,193,195 ; 88
               DEFW 197,199,201,203,205,207,209,211 ; 96
               DEFW 213,216,218,220,222,224,226,228 ;104
               DEFW 230,232,234,236,238,240,242,244 ;112
               DEFW 246,248,250,252,254,256,259,261 ;120
               DEFW 263,265,267,269,271,273,275,277 ;128
               DEFW 279,281,283,285,287,289,291,293 ;136
               DEFW 295,297,300,302,304,306,308,310 ;144
               DEFW 312,314,316,318,320,322,324,326 ;152
               DEFW 328,330,332,334,336,338,340,343 ;160
               DEFW 345,347,349,351,353,355,357,359 ;168
               DEFW 361,363,365,367,369,371,373,375 ;176
               DEFW 377,379,381,383,386,388,390,392 ;184
               DEFW 394,396,398,400,402,404,406,408 ;192
               DEFW 410,412,414,416,418,420,422,424 ;200
               DEFW 426,429,431,433,435,437,439,441 ;208
               DEFW 443,445,447,449,451,453,455,457 ;216
               DEFW 459,461,463,465,467,469,472,474 ;224
               DEFW 476,478,480,482,484,486,488,490 ;232
               DEFW 492,494,496,498,500,502,504,506 ;240
               DEFW 508,510,512,515,517,519,521,523 ;248


retrig.table:                      ;counter/x = int (counter/x)
               DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  ;x=0
               DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

               DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1  ;x=1
               DEFB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

               DEFB 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0  ;x=2
               DEFB 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0

               DEFB 1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1  ;x=3
               DEFB 0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0

               DEFB 1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0  ;x=4
               DEFB 1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0

               DEFB 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1  ;x=5
               DEFB 0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0

               DEFB 1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0  ;x=6
               DEFB 0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0

               DEFB 1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0  ;x=7
               DEFB 0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0

               DEFB 1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0  ;x=8
               DEFB 1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0

               DEFB 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0  ;x=9
               DEFB 0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0

               DEFB 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0  ;x=A
               DEFB 0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0

               DEFB 1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0  ;x=B
               DEFB 0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0

               DEFB 1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0  ;x=C
               DEFB 0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0

               DEFB 1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0  ;x=D
               DEFB 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0

               DEFB 1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0  ;x=E
               DEFB 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0

               DEFB 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1  ;x=F
               DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0




sequencer:
bar.1:
             ; LD   A,&10
             ; OUT  (248),A

               CALL c1+update.bp   ;check for sample boundaries
               CALL c2+update.bp   ;and loop the samples if
               CALL c3+update.bp   ;necessary
               CALL c4+update.bp

               LD   HL,vol.update  ;ensures instant response to
               LD   A,(HL)         ;a channel being toggled on
               OR   A              ;or off
               JR   Z,no.update

               CALL c1+bp.volume
               CALL c2+bp.volume
               CALL c3+bp.volume
               CALL c4+bp.volume

               LD   (HL),0
no.update:
bar.2:
             ; LD   A,&20
             ; OUT  (248),A

               LD   HL,countint
               INC  (HL)

               LD   A,(int.rtn.pag)
               INC  A
               JR   Z,no.extra.int
               DEC  A
               LD   HL,(int.routine)
               LD   C,A
               CALL far.call
no.extra.int:
bar.3:
             ; LD   A,&30
             ; OUT  (248),A

               LD   A,(mstatus)
               DEC  A
               RET  Z

               LD   HL,(counter.fract)
               LD   A,H
               LD   DE,(tempo)
               ADD  HL,DE
               LD   (counter.fract),HL
               CP   H              ;counter int not changed
               RET  Z

               LD   A,(speed)
               LD   C,A
               LD   A,H
               SUB  C              ;was CP C (in 2.03)
               JR   C,no.new.note  ;counter <> speed
             ; XOR  A   (2.03)     ;counter = speed
               LD   (counter),A
               LD   A,(pat.delay.c+1);for pattern delay command
               OR   A
               JR   Z,get.new.note ;get note data if no delay
               CALL no.new.all     ;else just do fx
               JP   dskip

no.new.note:                       ;counter <> speed
               CALL no.new.all     ;do fx
               JP   nonewposyet    ;check position change

;no new note data for all channels - fx only

no.new.all:
               CALL c1+check.fx
               CALL c2+check.fx
               CALL c3+check.fx
               JP   c4+check.fx

get.new.note:
               LD   A,(song.pos)
               LD   L,A
               LD   H,song.tab/256
               LD   A,(HL)         ;get pattern
               LD   (pattern.num),A
               LD   D,A
               AND  %11110000
               RLCA
               RLCA
               RLCA
               RLCA
               LD   C,A
               LD   A,(mp.peek)    ;mod page
               ADD  C
               LD   C,A

               LD   A,D
               AND  %00001111
               ADD  A,A
               ADD  A,A
origpat.offsh: ADD  A,0            ;pattern offset hi byte
               LD   D,A
origpat.offsl: LD   E,0            ;pattern offset lo byte
               BIT  6,D
               RES  6,D
               JR   Z,$+3
               INC  C

               LD   A,(pattern.pos)
               ADD  A
               ADD  A
               LD   H,0
               LD   L,A
               ADD  HL,HL
               ADD  HL,HL          ;*16
               ADD  HL,DE
               LD   A,C

               CALL get.patt       ;in lower memory

               CALL c1+play.voice
               CALL c2+play.voice
               CALL c3+play.voice
               CALL c4+play.voice

dskip:
               LD   HL,pattern.pos
               INC  (HL)

pat.delay.f:   LD   A,0
               OR   A
               JR   Z,no.new.patdel
               LD   (pat.delay.c+1),A
               XOR  A
               LD   (pat.delay.f+1),A
no.new.patdel:
pat.delay.c:   LD   A,0
               OR   A
               JR   Z,no.pat.delay
               DEC  A
               LD   (pat.delay.c+1),A
               JR   Z,no.pat.delay
               DEC  (HL)          ;if pat delay -> undo inc (hl)
no.pat.delay:
pbreak.flag:   LD   A,0
               OR   A
               JR   Z,nnpysk
               XOR  A
               LD   (pbreak.flag+1),A
pbreak.pos:    LD   A,0
               LD   (HL),A
               XOR  A
               LD   (pbreak.pos+1),A
nnpysk:
               LD   A,(HL)
               CP   64
               JR   C,nonewposyet
next.position:
               LD   A,(pbreak.pos+1)
               LD   (pattern.pos),A
               XOR  A
               LD   (pbreak.pos+1),A
               LD   (posjump.flag+1),A
               LD   HL,song.pos
               INC  (HL)
               LD   A,(HL)
               BIT  7,A
               JR   NZ,loop.time   ;reached song position 128
song.len:      CP   0
               JR   NZ,nonewposyet
loop.time:
               LD   (HL),0
               LD   A,6            ;reset speed (new in 2.03)
               LD   (speed),A
               LD   HL,&0100       ;and tempo
               LD   (tempo),HL

play.status:   LD   A,(disable.pos) ;0=keep repeating
               OR   A
               RET  Z
quit:
               LD   (mstatus),A
               RET
nonewposyet:
posjump.flag:  LD   A,0
               ;init=0, "B"=1, "D"=1

               OR   A
               JR   NZ,next.position
               RET

               DEFS $/32+1*32-$

;---------------------------------------------------------------
routines:
               ORG  0

;tables for command parsing

;table for commands on counter 0

chkmore.tab:
r0.000:        DEFW per.nop        ;0
r0.001:        DEFW per.nop        ;1
r0.002:        DEFW per.nop        ;2
r0.003:        DEFW per.nop        ;3 check earlier (tone porta)
r0.004:        DEFW per.nop        ;4
r0.005:        DEFW per.nop        ;5 check earlier (tone porta)
r0.006:        DEFW per.nop        ;6
r0.007:        DEFW per.nop        ;7
r0.008:        DEFW per.nop        ;8
r0.009:        DEFW sampleoffs     ;9
r0.010:        DEFW per.nop        ;A
r0.011:        DEFW pos.jump       ;B
r0.012:        DEFW volchange      ;C
r0.013:        DEFW patbreak       ;D
r0.014:        DEFW e.command      ;E
r0.015:        DEFW setspeed       ;F

;table for commands not on counter 0

checkfx.tab:
r0.016:        DEFW arpeggio       ;0
r0.017:        DEFW porta.up       ;1
r0.018:        DEFW porta.dn       ;2
r0.019:        DEFW tone.port      ;3
r0.020:        DEFW vibrato        ;4
r0.021:        DEFW tonevolsl      ;5
r0.022:        DEFW vibrvolsl      ;6
r0.023:        DEFW per.tremolo    ;7
r0.024:        DEFW set.back       ;8
r0.025:        DEFW set.back       ;9
r0.026:        DEFW per.volslid    ;A
r0.027:        DEFW set.back       ;B
r0.028:        DEFW set.back       ;C
r0.029:        DEFW set.back       ;D
r0.030:        DEFW e.command      ;E
r0.031:        DEFW set.back       ;F

;table for Extended commands

ecom.tab:
r0.032:        DEFW filter         ;0
r0.033:        DEFW fineportup     ;1
r0.034:        DEFW fineportdn     ;2
r0.035:        DEFW glisscntrl     ;3
r0.036:        DEFW vibracntrl     ;4
r0.037:        DEFW setfinetun     ;5
r0.038:        DEFW jumploop       ;6
r0.039:        DEFW tremocntrl     ;7
r0.040:        DEFW nothing        ;8 not a command
r0.041:        DEFW retrignote     ;9
r0.042:        DEFW volfineup      ;A
r0.043:        DEFW volfinedn      ;B
r0.044:        DEFW notecut        ;C
r0.045:        DEFW notedelay      ;D
r0.046:        DEFW pattdelay      ;E
r0.047:        DEFW nothing        ;F funk it not supported


;update sample addresses in burstplayer

update.bp:
mk.pag1:       LD   A,(0)
page.len:      SUB  0
               RET  C              ;not past marker yet (page)
len:           LD   DE,0
mk.off1:       LD   HL,(0)
               JR   Z,$+4
               SET  6,H
               SBC  HL,DE          ;cf not set
               RET  C              ;not past marker yet (offs)
repeat:        JR   $+2
               EX   DE,HL
r1.001:        LD   A,(page.len+1)
mk.pag2:       LD   (0),A
mk.off2:       LD   (0),HL
               RET

big.loop:
blp.page:      LD   A,0
mk.pag3:       LD   (0),A
blp.offs:      LD   DE,0
               ADD  HL,DE
mk.off3:       LD   (0),HL
               RET

small.loop:
slp.page:      LD   A,0
mk.pag4:       LD   (0),A
slp.offs:      LD   DE,0
               ADD  HL,DE
mk.off4:       LD   (0),HL

sm.en.page:    LD   A,0
sm.en.offs:    LD   HL,0
r1.002:        LD   (len+1),HL
r1.003:        LD   (page.len+1),A
               RET


sml.jr:        EQU  small.loop-repeat-2
blp.jr:        EQU  big.loop-repeat-2

;---------------------------------------------------------------

play.voice:
mk.cur.pat:    LD   HL,0

               LD   A,(HL)         ;HL = d
               INC  L
               AND  %00001111
               LD   D,A
               LD   E,(HL)
               DEC  L
r2.001:        LD   (note+1),DE
               OR   E              ;if no period given then use
r1.004:        CALL Z,per.nop      ;last given period

               EX   DE,HL          ;DE = d

               LD   H,sample.table/256
               LD   A,(DE)
               AND  &10
               JR   Z,$+3
               INC  H
               INC  E
               INC  E
               LD   A,(DE)
               AND  &F0
               LD   L,A
                                   ;DE = d + 2
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               INC  L
               LD   A,(HL)
               OR   A
r1.005:        JP   Z,set.regs     ;if no page -> no ins
               INC  L
r2.002:        LD   (smp.offs+1),BC
r1.006:        LD   (smp.page+1),A

               LD   A,L
               XOR  H
r1.007:        LD   (new.ins+1),A

               LD   C,(HL)
               INC  L
               LD   B,(HL)
               INC  L
               LD   A,(HL)
               INC  L
r2.003:        LD   (len+1),BC
r1.008:        LD   (page.len+1),A

               LD   A,(HL)         ;repeat type
               INC  L

               DEC  A
               JR   Z,get.big
               DEC  A
               JR   Z,get.small
               LD   A,L
               ADD  3
               LD   L,A
               XOR  A
               JR   got.loop
get.small:
r1.009:        LD   A,(page.len+1)
r1.010:        LD   (slp.page+1),A
r2.004:        LD   (slp.offs+1),BC
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               INC  L
               LD   A,(HL)
               INC  L
r1.011:        LD   (sm.en.page+1),A
r2.005:        LD   (sm.en.offs+1),BC
               LD   A,sml.jr
               JR   got.loop
get.big:
               LD   C,(HL)
               INC  L
               LD   B,(HL)
               INC  L
               LD   A,(HL)
               INC  L              ;start of repeat
r1.012:        LD   (blp.page+1),A
r2.006:        LD   (blp.offs+1),BC
               LD   A,blp.jr
got.loop:
r1.013:        LD   (repeat+1),A

               LD   A,(HL)
               INC  L
r1.014:        LD   (volume+1),A
r1.015:        CALL bp.volume

               LD   A,(HL)
r1.016:        LD   (finetune+1),A

set.regs:
               EX   DE,HL          ;HL = d + 2

               LD   A,(HL)
               INC  L              ;HL = d + 3
               AND  &0F
r1.017:        LD   (command+1),A
               LD   C,A
               LD   A,(HL)
r1.018:        LD   (cmdlo+1),A
               LD   B,A

r2.007:        LD   DE,(note+1)
               LD   A,D
               OR   E
r1.019:        JP   Z,chknewins

               LD   A,B
               AND  &F0
               OR   C              ;hi = param, lo = command
               CP   &5E            ;E5 = fine tune
               JR   Z,do.fine
               LD   A,C            ;only command now
               CP   3              ;3 = tone portamento
               JR   Z,chk.tone
               CP   5              ;5 = tone port + vol slide
               JR   Z,chk.tone
               JR   set.per
do.fine:
r1.020:        CALL setfinetun
               JR   set.per
chk.tone:
r1.021:        CALL set.tone
r1.022:        JP   chkmorefx

set.per:
r1.023:        LD   HL,(note+1)
               LD   A,H
               ADD  finet.tab/256
               LD   H,A

               LD   L,(HL)         ;get note number (*2)
               LD   B,L
               INC  L              ;255 = note not found
               JR   NZ,foundfine

r1.024:        LD   HL,(note+1)
               JR   notune
foundfine:
               DEC  L
finetune:      LD   A,0
               SRL  A
               JR   NC,$+4
               SET  7,L
               ADD  finelist/256
               LD   H,A

               LD   A,(HL)
               INC  L
               LD   H,(HL)
               LD   L,A
notune:
r1.025:        LD   (period+1),HL
               LD   A,B
r1.026:        LD   (note.num+1),A ;0-71, 255=unknown

r1.027:        LD   A,(cmdlo+1)
               AND  &F0
               LD   C,A
r1.028:        LD   A,(command+1)
               OR   C
               CP   &DE            ;ED = note delay
               JR   Z,chkmorefx

wav.cntrl:     LD   C,0
               XOR  A
               BIT  2,C            ;-> retrigger vibrato
               JR   Z,vibnoc
r1.029:        LD   (vibr.pos+1),A
vibnoc:
               BIT  6,C            ;-> retrigger tremolo
               JR   Z,trenoc
r1.030:        LD   (trem.pos+1),A
trenoc:
smp.offs:      LD   HL,0
smp.page:      LD   A,0
mk.pag5:       LD   (0),A
mk.off5:       LD   (0),HL
               XOR  A
mk.spfr:       LD   (0),A

period:        LD   DE,0
r1.031:        CALL per.nop2

r1.032:        LD   A,(new.ins+1)
r1.033:        LD   (cur.ins+1),A

             ; LD   A,1
             ; LD   (trigger),A

chkmorefx:
r1.034:        LD   HL,chkmore.tab
r1.035:        LD   A,(command+1)
               ADD  A,A
               ADD  A,L
               LD   L,A
             ; JR   NC,$+3         ;on 256
             ; INC  H
               LD   A,(HL)
               INC  L              ;on 256
               LD   H,(HL)
               LD   L,A
               JP   (HL)

chknewins:
new.ins:       LD   A,0
cur.ins:       CP   0
               JR   Z,chkmorefx
r1.036:        LD   (cur.ins+1),A
r1.037:        LD   HL,(smp.offs+1)
r1.038:        LD   A,(smp.page+1)
mk.pag6:       LD   (0),A
mk.off6:       LD   (0),HL
               XOR  A
mk.spfr2:      LD   (0),A
               JR   chkmorefx

check.fx:
command:       LD   C,0
cmdlo:         LD   A,0
               OR   C
               JR   Z,per.nop   ;no command - use old period
                                ;             in case of arpeg
r1.039:        LD   HL,checkfx.tab
               LD   A,C
               ADD  A,A
               ADD  A,L
               LD   L,A
             ; JR   NC,$+4         ;within boundary
             ; INC  H
               LD   A,(HL)
               INC  L              ;within boundary
               LD   H,(HL)
               LD   L,A
               JP   (HL)


per.nop:
r2.008:        LD   DE,(period+1)
per.nop2:
               SLA  E              ;convert pitch
               RL   D
               LD   A,D
               ADD  pitch.table/256
               LD   D,A
               LD   A,(DE)
mk.slo:        LD   (0),A
               INC  E
               LD   A,(DE)
mk.shi:        LD   (0),A
               RET

bp.volume:
channel.on:    LD   A,(0)          ;fill in variable
               OR   A
               JR   Z,chan.off
volume:        LD   A,0
               RRA
saa.exvol:     AND  %01111111
chan.off:      ADD  2
mk.tab:        LD   (0),A
               RET

;---------------------------------------------------------------
; Effect 0 - Arpeggio
;---------------------------------------------------------------
arpeggio:
               LD   A,(counter)  ;1-31
               LD   H,arpeg.table/256  ;table on 256 boundary
               LD   L,A
               LD   A,(HL)         ;counter mod 3
               OR   A
               JR   Z,arpeggio2
               CP   2
               JR   Z,arpeggio1
r1.040:        LD   A,(cmdlo+1)
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               JR   arpeggio3
arpeggio1:
r1.041:        LD   A,(cmdlo+1)
               AND  &0F
               JR   arpeggio3
arpeggio2:
r2.009:        LD   DE,(period+1)
               JR   arpeggio4
arpeggio3:
               ADD  A,A
note.num:      ADD  0
               RET  C               ;note number unknown
               CP   2*36
               RET  NC              ;new note too high
               LD   L,A
r1.042:        LD   A,(finetune+1)
               SRL  A
               JR   NC,$+4
               SET  7,L
               ADD  finelist/256
               LD   H,A
               LD   E,(HL)
               INC  L
               LD   D,(HL)
arpeggio4:
               JR   per.nop2

;---------------------------------------------------------------
; Effect 1 - Portamento Up
;---------------------------------------------------------------
porta.up:
r1.043:        LD   HL,(period+1)
r1.044:        LD   A,(cmdlo+1)
upmask:        AND  &FF            ;change to &0f for fine porta
               LD   C,A
               LD   B,0
               LD   A,&FF
r1.045:        LD   (upmask+1),A
               SBC  HL,BC
min.period:    LD   BC,113         ;minimum Amiga period
               JR   C,porttoofar
               SBC  HL,BC
               JR   NC,portauskip
porttoofar:    LD   HL,0
portauskip:
               ADD  HL,BC
r1.046:        LD   (period+1),HL
               EX   DE,HL
r1.047:        JP   per.nop2

;---------------------------------------------------------------
; Effect 2 - Portamento Down
;---------------------------------------------------------------
porta.dn:
r1.048:        LD   HL,(period+1)
r1.049:        LD   A,(cmdlo+1)
dnmask:        AND  &FF            ;change to &0f by fine porta
               ADD  A,L
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   A,&FF
r1.050:        LD   (dnmask+1),A
max.period:    LD   BC,856         ;maximum Amiga period
               OR   A
               SBC  HL,BC
               JR   C,portadskip
               LD   HL,0
portadskip:
               ADD  HL,BC
r1.051:        LD   (period+1),HL
               EX   DE,HL
r1.052:        JP   per.nop2

;---------------------------------------------------------------
set.tone:
note:          LD   HL,0
               LD   A,H
               ADD  finet.tab/256
               LD   H,A

               LD   L,(HL)         ;get note number (*2)
               LD   B,L
               INC  L              ;255 = note not found
               JR   NZ,foundfine2
r1.053:        LD   HL,(note+1)
               JR   notune2
foundfine2:
               DEC  L
r1.054:        LD   A,(finetune+1)
               SRL  A
               JR   NC,$+4
               SET  7,L
               ADD  finelist/256
               LD   H,A

               LD   A,(HL)
               INC  L
               LD   H,(HL)
               LD   L,A
notune2:
r1.055:        LD   (wanted.per+1),HL
r2.010:        LD   DE,(period+1)
               XOR  A
               SBC  HL,DE
               JR   Z,cleartone
               ADC  A,0
r1.056:        LD   (tonedirec+1),A ;0=porta dn, 1=porta up
               RET
cleartone:
r1.057:        LD   (wanted.per+1),HL
               RET

;---------------------------------------------------------------
; Effect 3 - Tone Portamento
;---------------------------------------------------------------
tone.port:
r1.058:        LD   A,(cmdlo+1)
               OR   A
               JR   Z,tonenochng
r1.059:        LD   (tonespeed+1),A
               XOR  A
r1.060:        LD   (cmdlo+1),A
tonenochng:
wanted.per:    LD   DE,0
               LD   A,D
               OR   E
               RET  Z
tonespeed:     LD   BC,0
r1.061:        LD   HL,(period+1)
tonedirec:     LD   A,0
               OR   A
               JR   NZ,toneportup
toneportdn:
               ADD  HL,BC
               SBC  HL,DE
               JR   C,tonesetper
               JR   portoff
toneportup:
               SBC  HL,BC
               JR   C,portoff
               SBC  HL,DE
               JR   NC,tonesetper
portoff:
               LD   HL,0
r1.062:        LD   (wanted.per+1),HL
tonesetper:
               ADD  HL,DE
gliss:         LD   A,0
               OR   A
               JR   Z,glissskip

               EX   DE,HL

               LD   C,0
r1.063:        LD   A,(finetune+1)
               SRL  A
               JR   NC,$+4
               SET  7,C
               ADD  finelist/256
               LD   B,A
glissloop:
               LD   A,(BC)
               INC  C
               LD   L,A
               LD   A,(BC)
               INC  C
               LD   H,A
               OR   A
               SBC  HL,DE
               JR   C,glissfound
               LD   A,C
               AND  %01111111
               CP   2*36
               JR   C,glissloop
glissfound:
               DEC  C
               LD   A,(BC)
               LD   H,A
               DEC  C
               LD   A,(BC)
               LD   L,A
glissskip:
r1.064:        LD   (period+1),HL
               EX   DE,HL
r1.065:        JP   per.nop2

;---------------------------------------------------------------
; Effect 4 - Vibrato
;---------------------------------------------------------------
vibrato:
r1.066:        LD   A,(cmdlo+1)
               OR   A
               JR   Z,vibrato2
vibr.cmnd:     LD   B,0
               AND  &0F
               JR   Z,vibskip
               LD   C,A
               LD   A,B
               AND  &F0
               OR   C
               LD   B,A
vibskip:
r1.067:        LD   A,(cmdlo+1)
               AND  &F0
               JR   Z,vibskip2
               LD   C,A
               LD   A,B
               AND  &0F
               OR   C
               LD   B,A
vibskip2:
               LD   A,B
r1.068:        LD   (vibr.cmnd+1),A
vibrato2:
vibr.pos:      LD   A,0
               RRCA
               RRCA
               AND  &1F
               LD   B,A
r1.069:        LD   A,(wav.cntrl+1)
               AND  &03
               JR   Z,vib.sine
               SLA  B
               SLA  B
               SLA  B
               DEC  A
               JR   Z,vib.ramp     ;                 _ _ _
               LD   E,255          ;square waveform   _ _ _
               JR   vib.set
vib.ramp:
r1.070:        LD   A,(vibr.pos+1)
               BIT  7,A
               JR   NZ,vib.ramp2
               LD   A,255          ;rampdown waveform \ \ \ \
               SUB  B              ;                   \ \ \ \
               LD   E,A
               JR   vib.set
vib.ramp2:
               LD   E,B
               JR   vib.set
vib.sine:
               LD   H,vibrato.table/256
               LD   L,B            ;sine waveform  /\  /\
               SET  5,L            ;table offset 32  \/  \/
               LD   E,(HL)
vib.set:
               LD   HL,0
r1.071:        LD   A,(vibr.cmnd+1)
               AND  &0F
               JR   Z,skip.mul
               LD   B,A
               LD   D,0
vib.mul:       ADD  HL,DE
               DJNZ vib.mul
skip.mul:
               SLA  L
               RL   H
               LD   B,H
r1.072:        LD   HL,(period+1)
r1.073:        LD   A,(vibr.pos+1)
               BIT  7,A
               JR   NZ,vibr.neg
               LD   A,L
               ADD  B
               LD   L,A
               JR   NC,$+3
               INC  H
               JR   vibrato3
vibr.neg:
               LD   A,L
               SUB  B
               LD   L,A
               JR   NC,$+3
               DEC  H
vibrato3:
               EX   DE,HL
r1.074:        CALL per.nop2
r1.075:        LD   A,(vibr.cmnd+1)
               RRCA
               RRCA
               AND  %00111100
r1.076:        LD   HL,vibr.pos+1
               ADD  (HL)
               LD   (HL),A
               RET

;---------------------------------------------------------------
; Effect 5 - Tone and Volume Slide
;---------------------------------------------------------------
tonevolsl:
r1.077:        CALL tonenochng
r1.078:        JP   volslide

;---------------------------------------------------------------
; Effect 6 - Vibrato and Volume Slide
;---------------------------------------------------------------
vibrvolsl:
r1.079:        CALL vibrato2
r1.080:        JP   volslide

;---------------------------------------------------------------
; Effect 7 - Tremolo
;---------------------------------------------------------------
per.tremolo:
r1.081:        CALL per.nop
tremolo:
r1.082:        LD   A,(cmdlo+1)
               OR   A
               JR   Z,tremolo2
trem.cmnd:     LD   B,0
               AND  &0F
               JR   Z,treskip
               LD   C,A
               LD   A,B
               AND  &F0
               OR   C
               LD   B,A
treskip:
r1.083:        LD   A,(cmdlo+1)
               AND  &F0
               JR   Z,treskip2
               LD   C,A
               LD   A,B
               AND  &0F
               OR   C
               LD   B,A
treskip2:
               LD   A,B
r1.084:        LD   (trem.cmnd+1),A
tremolo2:
trem.pos:      LD   A,0
               RRCA
               RRCA
               AND  &1F
               LD   B,A
r1.085:        LD   A,(wav.cntrl+1)
               RRCA
               RRCA
               RRCA
               RRCA
               AND  &03
               JR   Z,tre.sine
               SLA  B
               SLA  B
               SLA  B
               DEC  A
               JR   Z,tre.ramp
               LD   E,255
               JR   tre.set
tre.ramp:
r1.086:        LD   A,(trem.pos+1)
               BIT  7,A
               JR   NZ,tre.ramp2
               LD   A,255
               SUB  B
               LD   E,A
               JR   tre.set
tre.ramp2:
               LD   E,B
               JR   tre.set
tre.sine:
               LD   H,vibrato.table/256
               LD   L,B
               SET  5,L            ;table offset 32
               LD   E,(HL)
tre.set:
               LD   HL,0
r1.087:        LD   A,(trem.cmnd+1)
               AND  &0F
               JR   Z,skiptremul
               LD   B,A
               LD   D,0
tre.mul:       ADD  HL,DE
               DJNZ tre.mul
skiptremul:
               SLA  L
               RL   H
r1.088:        LD   A,(trem.pos+1)
               BIT  7,A
r1.089:        LD   A,(volume+1)
               JR   NZ,trem.neg
               ADD  H
               JR   NC,$+4
               LD   A,63
               CP   64
               JR   C,$+4
               LD   A,63
               JR   tremolo3
trem.neg:
               SUB  H
               JR   NC,$+3
               XOR  A
tremolo3:
               LD   C,A
r1.090:        LD   A,(volume+1)
               LD   B,A
               LD   A,C

r1.091:        LD   (volume+1),A

r1.092:        CALL bp.volume

               LD   A,B
r1.093:        LD   (volume+1),A

r1.094:        LD   A,(trem.cmnd+1)
               RRCA
               RRCA
               AND  %00111100
r1.095:        LD   HL,trem.pos+1
               ADD  (HL)
               LD   (HL),A
               RET

;---------------------------------------------------------------
; Effect 9 - Sample Offset  if offset too large -> start of loop
;---------------------------------------------------------------
sampleoffs:
r1.096:        LD   A,(cmdlo+1)
               OR   A
               JR   Z,sononew
r1.097:        LD   (sampoffs+1),A
sononew:
sampoffs:      LD   A,0
               LD   C,0
r1.098:        LD   HL,(smp.offs+1)
               ADD  H
               LD   H,A
               JR   NC,so.noover
               SET  7,H
               LD   C,2
so.noover:
r1.099:        LD   A,(smp.page+1)
               ADD  C
               BIT  6,H
               RES  6,H
               JR   Z,$+3
               INC  A

               EX   DE,HL
               LD   C,A
r1.100:        LD   A,(page.len+1)
               SUB  C
               JR   NC,so.ok
r1.101:        LD   HL,(len+1)
               OR   A
               SBC  HL,DE
               JR   NC,so.ok
               ADD  C     ;-> a=(page.len+1)
               ADD  HL,DE ;-> hl=(len+1)
               JR   mk.pag7
so.ok:
               LD   A,C
               EX   DE,HL
mk.pag7:       LD   (0),A
mk.off7:       LD   (0),HL
               RET

;---------------------------------------------------------------
; Effect A - Volume Slide
;------------------------------------------------------------
per.volslid:
r1.102:        CALL per.nop

volslide:
r1.103:        LD   A,(cmdlo+1)
               AND  &F0
               JR   Z,volsli.dn
               RRCA
               RRCA
               RRCA
               RRCA
volsli.up:
               LD   B,A
r1.104:        LD   A,(volume+1)
               ADD  B
               CP   64
               JR   C,$+4
               LD   A,63
r1.105:        LD   (volume+1),A
r1.106:        JP   bp.volume
volsli.dn:
r1.107:        LD   A,(cmdlo+1)
               AND  &0F
               LD   B,A
r1.108:        LD   A,(volume+1)
               SUB  B
               JR   NC,$+3
               XOR  A
r1.109:        LD   (volume+1),A
r1.110:        JP   bp.volume

;---------------------------------------------------------------
; Effect B - Position Jump
;---------------------------------------------------------------
pos.jump:
               LD   A,(disable.pos)
               OR   A
               RET  NZ
r1.111:        LD   A,(cmdlo+1)
               DEC  A
               LD   (song.pos),A
pj2:           XOR  A
               LD   (pbreak.pos+1),A
               INC  A
               LD   (posjump.flag+1),A
               RET

;---------------------------------------------------------------
; Effect C - Volume Change
;---------------------------------------------------------------
volchange:
r1.112:        LD   A,(cmdlo+1)
               CP   64
               JR   C,$+4
               LD   A,63
r1.113:        LD   (volume+1),A
r1.114:        JP   bp.volume

;---------------------------------------------------------------
; Effect D - Pattern Break
;---------------------------------------------------------------
patbreak:
r1.115:        LD   A,(cmdlo+1)
               LD   E,A
               AND  &F0
               RRCA
               RRCA
               RRCA
               RRCA
               LD   C,A
               ADD  A,A
               ADD  A,A
               ADD  A,C
               ADD  A,A   ;*10
               LD   C,A
               LD   A,E
               AND  &0F
               ADD  C              ;was OR C in 2.02 (BUG!!!!)
               CP   63
               JR   NC,pj2
               LD   (pbreak.pos+1),A
               LD   A,1
               LD   (posjump.flag+1),A
               RET

;---------------------------------------------------------------
; Effect F - Set Speed
;            Sort of handles BPM alterations (not 100% accurate)
;---------------------------------------------------------------
setspeed:
r1.116:        LD   A,(cmdlo+1)
               OR   A
               RET  Z
               CP   32             ;speed <32 (was 33 in 2.02)
               JR   NC,setbpm      ;so this is BPM
               LD   (speed),A
             ; XOR  A
             ; LD   (counter),A
               RET

setbpm:
               LD   H,bpm.table/256
               ADD  A,A
               JR   NC,$+3
               INC  H
               LD   L,A
               LD   E,(HL)
               INC  L
               LD   D,(HL)
               LD   (tempo),DE
               RET

;---------------------------------------------------------------
e.command:
r1.117:        LD   A,(cmdlo+1)
               AND  &F0
               RRCA
               RRCA
               RRCA
r1.118:        LD   HL,ecom.tab
               ADD  A,L
               LD   L,A
            ;  JR   NC,$+3         ;within 256
            ;  INC  H
               LD   A,(HL)
               INC  L
               LD   H,(HL)
               LD   L,A
               JP   (HL)


nothing:       ;simply continue through to filter RET
set.back:      ; "

;---------------------------------------------------------------
; Effect E
;---------------------------------------------------------------
; Effect 0 - Filter On/Off         Amiga hardware rubbish
filter:
               RET

; Effect 1 - Fine Porta Up
fineportup:
               LD   A,(counter)
               OR   A
               RET  NZ
               LD   A,&0F
r1.119:        LD   (upmask+1),A
r1.120:        JP   porta.up

; Effect 2 - Fine Porta Down
fineportdn:
               LD   A,(counter)
               OR   A
               RET  NZ
               LD   A,&0F
r1.121:        LD   (dnmask+1),A
r1.122:        JP   porta.dn

; Effect 3 - Set Gliss Control
glisscntrl:
r1.123:        LD   A,(cmdlo+1)
               AND  &0F
r1.124:        LD   (gliss+1),A
               RET

; Effect 4 - Set Vibrato Control
vibracntrl:
r1.125:        LD   A,(cmdlo+1)
               AND  &0F
               LD   B,A
r1.126:        LD   HL,wav.cntrl+1
               LD   A,(HL)
               AND  &F0
               OR   B
               LD   (HL),A
               RET

; Effect 5 - Set Fine Tune
setfinetun:
r1.127:        LD   A,(cmdlo+1)
               AND  &0F
r1.128:        LD   (finetune+1),A
               RET

; Effect 6 - Jump Loop
jumploop:
               LD   A,(counter)
               OR   A
               RET  NZ
r1.129:        LD   A,(cmdlo+1)
               AND  &0F
               JR   Z,setloop
               LD   B,A
loopcount:     LD   A,0
               OR   A
               JR   Z,jump.cnt
               DEC  A
r1.130:        LD   (loopcount+1),A
               RET  Z
jmploop:
pattpos:       LD   A,0
               LD   (pbreak.pos+1),A
               LD   A,1
               LD   (pbreak.flag+1),A
               RET
jump.cnt:
               LD   A,B
r1.131:        LD   (loopcount+1),A
               JR   jmploop
setloop:
               LD   A,(pattern.pos)
r1.132:        LD   (pattpos+1),A
               RET

; Effect 7 - Set Tremolo Control
tremocntrl:
r1.133:        LD   A,(cmdlo+1)
               AND  &0F
               RLCA
               RLCA
               RLCA
               RLCA
               LD   B,A
r1.134:        LD   HL,wav.cntrl+1
               LD   A,(HL)
               AND  &0F
               OR   B
               LD   (HL),A
               RET

; Effect 9 - Retrig Note
retrignote:
r1.135:        LD   A,(cmdlo+1)
               AND  &0F
               RET  Z
               LD   HL,retrig.table
               BIT  3,A
               RES  3,A
               JR   Z,$+3
               INC  H
               RRCA
               RRCA
               RRCA
               ADD  A,L
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   A,(counter)
               ADD  A,L
               LD   L,A            ;can't overflow
               LD   A,(HL)
               OR   A
               RET  Z
doretrig:
r1.136:        LD   A,(new.ins+1)
r1.137:        LD   (cur.ins+1),A
r1.138:        LD   HL,(smp.offs+1)
r1.139:        LD   A,(smp.page+1)
mk.pag8:       LD   (0),A
mk.off8:       LD   (0),HL
               RET


; Effect A - Volume Fine Up
volfineup:
               LD   A,(counter)
               OR   A
               RET  NZ
r1.140:        LD   A,(cmdlo+1)
               AND  &0F
r1.141:        JP   volsli.up

; Effect B - Volume Fine Down
volfinedn:
               LD   A,(counter)
               OR   A
               RET  NZ
r1.142:        LD   A,(cmdlo+1)
               AND  &0F
r1.143:        JP   volsli.dn

; Effect C - Note Cut
notecut:
r1.144:        LD   A,(cmdlo+1)
               AND  &0F
               LD   B,A
               LD   A,(counter)
               CP   B
               RET  NZ
               XOR  A
r1.145:        LD   (volume+1),A
r1.146:        JP   bp.volume

; Effect D - Note Delay
notedelay:
r1.147:        LD   A,(cmdlo+1)
               AND  &0F
               LD   B,A
               LD   A,(counter)
               CP   B
               RET  NZ

               OR   A
               RET  Z
               JR   doretrig

; Effect E - Pattern Delay
pattdelay:
               LD   A,(counter)
               OR   A
               RET  NZ
r1.148:        LD   A,(cmdlo+1)
               AND  &0F
               LD   B,A
               LD   A,(pat.delay.c+1)
               OR   A
               RET  NZ             ;still delaying pattern
               LD   A,B            ;so don't reset counter
               INC  B
               LD   (pat.delay.f+1),A
               RET

;tables need to fit within 32 byte boundary

               DEFS $/32+1*32-$

routine.len:   ;routine start ORGs at 0 -> routine.len = length

;===============================================================
length:        EQU  routine.len+routines-32768

c1:            EQU  0*routine.len+routines
c2:            EQU  1*routine.len+routines
c3:            EQU  2*routine.len+routines
c4:            EQU  3*routine.len+routines

               DEFS 4-1*routine.len

               ORG  4*routine.len+routines-32768

move.size:     EQU  6*256          ;move size = gap size
move.spc:      DEFS move.size      ;for octave 4 -> 6 (5 oct)
                                   ;for octave 3 -> 3 (3 oct)

;maximum amount of bytes (*256) needed in one sample frame at:
;
;* Amiga pitch (3 octaves) = 108 -> burst speed = 808
;  808/256*208 = 656.5 -> 3 * 256 bytes
;
;* extended PC pitch (5 octaves) = 54 -> burst speed = 1616
;  1616/256*208 = 1313 -> 6 * 256 bytes
