
;MAKE burstplayer (for SAM MOD player version 2.00)
;(C) 1995 Stefan Drissen
;last update: 3 April 1995, 21:07

output:        EQU  7     ;0=clut, 1=saa, 2=eddac, 3=eddac 2
                          ;4=dac, 5=dac 2, 6=blue alpha, 7=qss
                          ;EDDAC routine uses most memory!

pal:           EQU  0     ;use which Amiga to calculate sample
ntsc:          EQU  1     ;speeds


count:         EQU  255   ;number of outs before border change
linetest:      EQU  0

mk.page:       EQU  5     ;only used for assembly address
sq.page:       EQU  4     ;not important except when testing

ras.int:       EQU  249   ;line interrupt
int.stat:      EQU  249   ;interrupt status

               ORG  32768
               DUMP mk.page,0

               JP   go.burst

device:        DEFB output
amiga:         DEFB pal
bp.page:       DEFB 2

               DEFM "                          "
               DEFM "MAKEBURST (C)1995 Stefan Drissen"
               DEFM "Thanks to Edwin Blink for the   "
               DEFM "original burst idea and code...."

go.burst:
               DI
               IN   A,(250)
               LD   (mk.lmpr+1),A

               LD   A,(bp.page)
               OR   32
               OUT  (250),A
               LD   (mk.sp+1),SP
               LD   SP,49152
               CALL maker
mk.exit:
mk.lmpr:       LD   A,0
               OUT  (250),A
mk.sp:         LD   SP,0
             ; EI
               RET

;---------------------------------------------------------------

maker:
               LD   A,count
               LD   (poke.count+1),A
               XOR  A
               LD   (counter+1),A

               LD   HL,0
               LD   DE,1
               LD   BC,32767
               LD   (HL),L
               LDIR

               LD   HL,2*208+playtab1
               LD   BC,2*208
               LD   A,(device)
               CP   7              ;if QSS -> playtab2 higher
               JR   NZ,not.qss.pt
               ADD  HL,BC
               LD   BC,4*208
not.qss.pt:
               EX   AF,AF'
               LD   A,L
               LD   (qs.playtab2.1+1),A
               LD   A,H
               LD   (qs.playtab2.2+1),A
               ADD  HL,BC
               LD   (no.function+1),HL
               EX   AF,AF'

               LD   HL,device.list
               ADD  A,A
               ADD  A,A
               ADD  A,A
               ADD  A,A
               ADD  A,L
               LD   L,A
               JR   NC,$+3
               INC  H
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               LD   C,(HL)
               INC  HL
               LD   B,(HL)
               INC  HL
               LD   (mk.ro.add+1),DE
               LD   (mk.ro.len+1),BC
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               LD   C,(HL)
               INC  HL
               LD   B,(HL)
               INC  HL
               LD   (sound.driver+1),DE
               LD   (sound.dr.len+1),BC
               LD   C,(HL)
               INC  HL
               LD   B,(HL)
               INC  HL
               LD   (sample.port+1),BC
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               LD   (sample.ctrl+1),DE
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               LD   (mk.timing+2),DE
               EX   DE,HL
               LD   BC,129
               ADD  HL,BC
               LD   A,(HL)
               LD   (no.func.wait+1),A
               SUB  15             ;+1 for JR being done
                                   ;+2 for LD A,jr
                                   ;+4 for LD (jr+1),A
                                   ;+4 for LD (cp+1),A
               LD   (no.func.wait2+1),A
               EX   DE,HL

               LD   A,(HL)
               INC  HL
               LD   (ras.start.1+1),A
               LD   (ras.start.2+1),A

               LD   A,(HL)
               LD   (output.bits+1),A

               LD   HL,mk.movecode
               LD   DE,0
               LD   BC,mk.mv.end
               LDIR

;---------------------------------------------------------------
;interrupt routine (00056)
interrupt:
               LD   HL,00056
               LD   (HL),&08       ;ex af,af'
               INC  HL
               LD   (HL),&D9       ;exx
               INC  HL
               CALL insert.outs
               LD   (HL),&FE
               INC  HL
               LD   (HL),191-3     ;cp 191-3
               INC  HL
               LD   (HL),&28
               INC  HL
               LD   (mk.sto1+1),HL ;jr z,prep.bord.play
               LD   (mk.sto1.1+1),HL
               LD   (mk.recjradd+1),HL
               INC  HL
               LD   (HL),&C6
               INC  HL
               LD   (HL),&03       ;add a,3
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),ras.int   ;out (249),a
               INC  HL
               LD   (HL),&C3
               INC  HL
               LD   (mk.sto2+1),HL ;jp no.function
               INC  HL
               INC  HL

mk.sto1.1:     LD   DE,0
               PUSH HL
               SCF
               SBC  HL,DE
               LD   A,L
               POP  HL
               LD   (mk.stojr188+1),A
               LD   (HL),&3E
               INC  HL
               LD   (mk.recjr191+1),HL
               INC  HL
               LD   (HL),&32
               INC  HL
mk.recjradd:   LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (jr+1),a
               INC  HL
               LD   (HL),&3E
               INC  HL
               LD   (HL),191       ;ld a,191
               INC  HL
               LD   (HL),&32
               INC  HL
               DEC  DE
               DEC  DE
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (cp191+1),a
               INC  HL

               LD   (HL),&D3
               INC  HL
               LD   (HL),249       ;out (249),a
               INC  HL
               LD   (HL),&C3
               INC  HL
               LD   (mk.sto2.1+1),HL ;jp nofunction2
               INC  HL
               INC  HL

;---------------------------------------------------------------
;reset output device

               LD   DE,reset.output
mk.ro.add:     LD   HL,0
mk.ro.len:     LD   BC,0
               LD   A,B
               OR   C
               JR   Z,ro.no.silence
               LDIR
ro.no.silence:
               EX   DE,HL
               LD   (HL),&C9       ;ret
               INC  HL

;---------------------------------------------------------------
;select border player

mk.sto1:       LD   DE,0
               PUSH HL
               SCF
               SBC  HL,DE
               EX   DE,HL
               LD   (HL),E
mk.recjr191:   LD   HL,0
               LD   (HL),E
               POP  HL             ;prep.bord.play:
               LD   (HL),&3E
               INC  HL
mk.stojr188:   LD   (HL),0         ;ld a,jr188
               INC  HL
               LD   (HL),&32
               INC  HL
               LD   DE,(mk.recjradd+1)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (jradd+1),a
               INC  HL
               LD   (HL),&3E
               INC  HL
               LD   (HL),188       ;ld a,188
               INC  HL
               DEC  DE
               DEC  DE
               LD   (HL),&32
               INC  HL
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (cp188+1),a
               INC  HL

               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL
               LD   (HL),&32
               INC  HL
               LD   (mk.sto3+1),HL ;ld (prog.p+1),a
               INC  HL
               INC  HL
               LD   (HL),&08       ;ex af,af'
               INC  HL
               LD   (HL),&D9       ;exx
               INC  HL
               LD   (HL),&F5       ;push af
               INC  HL
               LD   (HL),&C5       ;push bc
               INC  HL
               LD   (HL),&D5       ;push de
               INC  HL
               LD   (HL),&E5       ;push hl
               INC  HL
               LD   (HL),&DD
               INC  HL
               LD   (HL),&E5       ;push ix
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&73
               INC  HL
               LD   (mk.sto4+1),HL ;ld (prog.sp+1),sp
               INC  HL
               INC  HL
               LD   (mk.rec2+1),HL ;playerselect:
               LD   (HL),&C3
               INC  HL
               LD   (mk.sto5+1),HL ;jp borderplay1
               INC  HL
               INC  HL

;---------------------------------------------------------------
;store for current pattern row + other common variables
;there are 256 bytes reserved for this

cur.pat.data:  EQU  256            ;16
paltab:        EQU  256+16         ;16
frame.scr:     EQU  256+32         ;1  of not 0 -> new screen

buffer:        EQU  256+128        ;128

;---------------------------------------------------------------

tables:

output.bits:   LD   B,0
               LD   C,32           ;num vol tabs
               LD   A,B
               CP   4
               JR   NZ,$+4         ;saa?
               LD   C,16

               CALL make.vol.tab

               CALL make.pitch

volume.tab:    EQU  buffer+128        ;32*256

pitch.table:   EQU  32*256+volume.tab ;1024*2

;---------------------------------------------------------------
;get pattern data routine
get.pat:
               LD   HL,1024*2+pitch.table

               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&11       ;ld de,cur.pat.data
               INC  HL
               LD   (HL),cur.pat.data\256
               INC  HL
               LD   (HL),cur.pat.data/256
               INC  HL
               LD   B,15
mk.gp.blp:     LD   (HL),&ED
               INC  HL
               LD   (HL),&A0       ;ldi
               INC  HL
               DJNZ mk.gp.blp
               LD   (HL),&7E       ;ld a,(hl)
               INC  HL
               LD   (HL),&12       ;ld (de),a
               INC  HL
               LD   (HL),&3E
               INC  HL             ;ld a,sq.page
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL                                     ;42

;---------------------------------------------------------------
;call far routine, C=page, HL=address
call.far:
               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL
               LD   (HL),&32
               INC  HL
               EX   DE,HL
               LD   HL,12
               ADD  HL,DE
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (nn),a
               INC  HL
               LD   (HL),&79       ;ld a,c
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&22
               INC  HL
               EX   DE,HL
               LD   HL,3
               ADD  HL,DE
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (call+1),hl
               INC  HL
               LD   (HL),&CD
               INC  HL
               INC  HL             ;call hl
               INC  HL
               LD   (HL),&3E
               INC  HL             ;ld a,sq.page
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL                                      ;19

;---------------------------------------------------------------
;far block move 1 - copies C bytes (max 128) to store
;B = source page, HL = source offset
ldir.far.1:
               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL
               LD   (HL),&32
               INC  HL
               LD   (buf.sto1+1),HL
               INC  HL             ;ld (nn),a
               INC  HL
               LD   (HL),&78       ;ld a,b
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&11
               INC  HL
               LD   (HL),buffer\256
               INC  HL
               LD   (HL),buffer/256;ld de,buffer
               INC  HL
               LD   (HL),&06
               INC  HL
             ; LD   (HL),0         ;ld b,0
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&B0       ;ldir
               INC  HL
               LD   (HL),&3E
               INC  HL
               EX   DE,HL
buf.sto1:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               INC  HL             ;ld a,n
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL                                     ;18

;---------------------------------------------------------------
;far block move 2 - copies C bytes (max 128) from store
;B = target page, DE = target offset
ldir.far.2:
               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL
               LD   (HL),&32
               INC  HL
               LD   (buf.sto2+1),HL
               INC  HL             ;ld (nn),a
               INC  HL
               LD   (HL),&78       ;ld a,b
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (HL),buffer\256
               INC  HL
               LD   (HL),buffer/256;ld hl,buffer
               INC  HL
               LD   (HL),&06
               INC  HL
             ; LD   (HL),0         ;ld b,0
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&B0       ;ldir
               INC  HL
               LD   (HL),&3E
               INC  HL
               EX   DE,HL
buf.sto2:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               INC  HL             ;ld a,n
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL                                     ;18

;---------------------------------------------------------------

;play tables (208*2*2)    if QSS -> 208*4*2

playtab1:      EQU  2048+pitch.table+101   ;2*208
                                   ; ^^^ previous routines!
;---------------------------------------------------------------
;interrupt function
no.function:
               LD   DE,0           ;after playtab2 (QSS or not)
mk.sto2:       LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

no.func.wait:  LD   E,0

               LD   A,linetest
               OR   A
               LD   A,0
               JR   Z,$+4
               LD   A,-4
               ADD  A,E

ins.nf.nop:
               CP   3
               JR   C,ins.nf.notjr
               LD   (HL),&18
               INC  HL
;              LD   (HL),0         ;jr $+2
               INC  HL
               SUB  3
               JR   ins.nf.nop
ins.nf.notjr:
               OR   A
               JR   Z,ins.nf.all
ins.nf.lp:
;              LD   (HL),&00       ;nop
               INC  HL
               DEC  A
               JR   NZ,ins.nf.lp
ins.nf.all:

             ; LD   A,linetest
             ; OR   A
             ; CALL NZ,timeline

               CALL insert.outs
               LD   (HL),&D9       ;exx
               INC  HL
               LD   (HL),&08       ;ex af,af'
               INC  HL
               LD   (HL),&FB       ;ei
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL

no.function2:
               EX   DE,HL
mk.sto2.1:     LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

no.func.wait2: LD   E,0

               LD   A,linetest
               OR   A
               LD   A,0
               JR   Z,$+4
               LD   A,-4
               ADD  A,E

ins.nf.nop2:
               CP   3
               JR   C,ins.nf.notjr2
               LD   (HL),&18
               INC  HL
;              LD   (HL),0         ;jr $+2
               INC  HL
               SUB  3
               JR   ins.nf.nop2
ins.nf.notjr2:
               OR   A
               JR   Z,ins.nf.all2
ins.nf.lp2:
;              LD   (HL),&00       ;nop
               INC  HL
               DEC  A
               JR   NZ,ins.nf.lp2
ins.nf.all2:

             ; LD   A,linetest
             ; OR   A
             ; CALL NZ,timeline

               CALL insert.outs
               LD   (HL),&D9       ;exx
               INC  HL
               LD   (HL),&08       ;ex af,af'
               INC  HL
               LD   (HL),&FB       ;ei
               INC  HL
               LD   (HL),&76       ;halt
               INC  HL
               LD   (HL),&C9       ;ret
               INC  HL


;---------------------------------------------------------------
;border player 1  - all get data routines included in here
border.play.1:

mk.timing:     LD   IX,0
               LD   A,(IX)
               INC  IX

               EX   DE,HL           ;borderplay1:
mk.sto5:       LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               LD   (mk.rec32+1),HL
               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto10+1),HL ;ld ix,bord.pl11
               INC  HL
               INC  HL
               LD   (HL),&C3
               INC  HL
               LD   (mk.sto24+1),HL
               INC  HL             ;jp get.c1.data
               INC  HL

;=====----- get channel 1 data

;fetch channel 1 data

               LD   (mk.getc1data+1),HL
               EX   DE,HL
mk.sto24:      LD   HL,0           ;get.c1.data:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c1.page),HL;c1.page:
;              LD   (HL),4         ;ld a,4
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
               LD   (bp.c1.offs),HL ;c1.off:
;              LD   (HL),0
               INC  HL
;              LD   (HL),128       ;ld hl,32768
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&16
               INC  HL
               LD   (bp.c1.vol),HL ;c1.tab:
;              LD   (HL),32        ;ld d,32
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&0E
               INC  HL
               LD   (bp.c1.speedlo),HL ;c1.speedlo:
;              LD   (HL),128       ;ld c,128
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&31
               INC  HL
               LD   (bp.c1.speedhi),HL ;c1.speedhi:
;              LD   (HL),1
               INC  HL
;              LD   (HL),0         ;ld sp,1
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c1.sp.frct),HL ;c1.sp.frct:
;              LD   (HL),0         ;ld a,0
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&DD
               INC  HL
               LD   (HL),&E9       ;jp (ix)
               INC  HL

;=====----- end get channel 1 data

               LD   (mk.ix1+2),IX
               LD   (mk.a1+1),A
               LD   E,A
               LD   A,(counter+1)
               LD   (mk.c1+1),A
               LD   A,E

mk.sto10:      LD   DE,0
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CALL mk.bp11         ;inc "bp11"

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto11+1),HL ;ld ix,bord.pl14
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
               LD   (mk.sto25+1),HL
               INC  HL              ;jp get.c4.data
               INC  HL

;=====----- start get channel 4 data

;fetch channel 4 data

               LD   (mk.getc4data+1),HL
               EX   DE,HL
mk.sto25:      LD   HL,0            ;get.c4.data:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c4.page),HL ;c4.pag:
;              LD   (HL),4          ;ld a,4
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
               LD   (bp.c4.offs),HL ;c4.off:
;              LD   (HL),0
               INC  HL
;              LD   (HL),128        ;ld hl,32768
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),251        ;out (251),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&16
               INC  HL
               LD   (bp.c4.vol),HL ;c4.tab:
;              LD   (HL),32         ;ld d,32
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&0E
               INC  HL
               LD   (bp.c4.speedlo),HL ;c4.speedlo:
;              LD   (HL),128        ;ld c,128
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&31
               INC  HL
               LD   (bp.c4.speedhi),HL ;c4.speedhi:
;              LD   (HL),1
               INC  HL
;              LD   (HL),0          ;ld sp,1
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c4.sp.frct),HL ;c4.sp.frct:
;              LD   (HL),0          ;ld a,0
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&DD
               INC  HL
               LD   (HL),&E9        ;jp (ix)
               INC  HL

;=====----- end get channel 4 data

               LD   (mk.ix4+2),IX
               LD   (mk.a4+1),A
               LD   E,A
               LD   A,(counter+1)
               LD   (mk.c4+1),A
               LD   A,E

mk.sto11:      LD   DE,0
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CALL mk.bp14         ;inc "bp14"

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto12+1),HL ;ld ix,bordpl1f
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
               LD   (mk.paltabsel+1),HL
               INC  HL              ;jp paltabselect
               INC  HL

;=====----- start paltabselect

;frame palette and screen select

               LD   (mk.paltabselr+1),HL
               EX   DE,HL
mk.paltabsel:  LD   HL,0            ;paltabselect
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
               LD   (HL),paltab+15\256
               INC  HL
               LD   (HL),paltab+15/256 ;ld hl,paltab1+15
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&01
               INC  HL
               LD   (HL),248
               INC  HL
               LD   (HL),16        ;ld bc,16*256+248
               INC  HL
               LD   B,16
mk.outd.1:
               CP   6
               CALL C,insert.xout
               SUB  6

               LD   (HL),&ED
               INC  HL
               LD   (HL),&AB       ;outd (16*)
               INC  HL
               DJNZ mk.outd.1

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DB
               INC  HL
               LD   (HL),252       ;in a,(252)
               INC  HL

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&5F       ;ld e,a
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&3A
               INC  HL
               LD   (HL),frame.scr\256
               INC  HL
               LD   (HL),frame.scr/256 ;ld a,(frame.scr)
               INC  HL

               CP   1+3
               CALL C,insert.xout
               SUB  1+3

               LD   (HL),&A7       ;and a
               INC  HL
               LD   (HL),&20
               INC  HL
               LD   (HL),1         ;jr nz,$+3
               INC  HL
               LD   (HL),&7B       ;ld a,e
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),252       ;out (252),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&DD
               INC  HL
               LD   (HL),&E9       ;jp (ix)
               INC  HL

;=====----- end paltabselect

               LD   (mk.ixp+2),IX
               LD   (mk.ap+1),A
               LD   E,A
               LD   A,(counter+1)
               LD   (mk.cp+1),A
               LD   A,E

               EX   DE,HL
mk.sto12:      LD   HL,0            ;bordpl1f:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto13+1),HL ;ld ix,bord.pl12
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
               LD   (mk.sto26+1),HL
               INC  HL              ;jp get.c2.data
               INC  HL

;=====----- start get channel 2 data

;fetch channel 2 data

               LD   (mk.getc2data+1),HL
               EX   DE,HL
mk.sto26:      LD   HL,0            ;get.c2.data:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c2.page),HL ;c2.pag:
;              LD   (HL),4          ;ld a,4
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
               LD   (bp.c2.offs),HL ;c2.off:
;              LD   (HL),0
               INC  HL
;              LD   (HL),128        ;ld hl,32768
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),251        ;out (251),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&16
               INC  HL
               LD   (bp.c2.vol),HL ;c2.tab:
;              LD   (HL),32         ;ld d,32
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&0E
               INC  HL
               LD   (bp.c2.speedlo),HL ;c2.speedlo:
;              LD   (HL),128        ;ld c,128
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&31
               INC  HL
               LD   (bp.c2.speedhi),HL ;c2.speedhi:
;              LD   (HL),1
               INC  HL
;              LD   (HL),0          ;ld sp,1
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c2.sp.frct),HL ;c2.sp.frct:
;              LD   (HL),0          ;ld a,0
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&DD
               INC  HL
               LD   (HL),&E9        ;jp (ix)
               INC  HL

;=====----- end get channel 2 data

               LD   (mk.ix2+2),IX
               LD   (mk.a2+1),A
               LD   E,A
               LD   A,(counter+1)
               LD   (mk.c2+1),A
               LD   A,E

               EX   DE,HL
mk.sto13:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CALL mk.bp12         ;inc "bp12"

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto14+1),HL ;ld ix,bord.pl13
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
               LD   (mk.sto27+1),HL
               INC  HL              ;jp get.c3.data
               INC  HL

;=====----- start get channel 3 data

;fetch channel 3 data

               LD   (mk.getc3data+1),HL
               EX   DE,HL
mk.sto27:      LD   HL,0            ;get.c3.data:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c3.page),HL ;c3.pag:
;              LD   (HL),4          ;ld a,4
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
               LD   (bp.c3.offs),HL ;c3.off:
;              LD   (HL),0
               INC  HL
;              LD   (HL),128        ;ld hl,32768
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),251        ;out (251),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&16
               INC  HL
               LD   (bp.c3.vol),HL ;c3.tab:
;              LD   (HL),32         ;ld d,32
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&0E
               INC  HL
               LD   (bp.c3.speedlo),HL ;c3.speedlo:
;              LD   (HL),128        ;ld c,128
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&31
               INC  HL
               LD   (bp.c3.speedhi),HL ;c3.speedhi:
;              LD   (HL),1
               INC  HL
;              LD   (HL),0          ;ld sp,1
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.c3.sp.frct),HL ;c3.sp.frct:
;              LD   (HL),0          ;ld a,0
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&DD
               INC  HL
               LD   (HL),&E9        ;jp (ix)
               INC  HL

;=====----- end get channel 3 data

               LD   (mk.ix3+2),IX
               LD   (mk.a3+1),A
               LD   E,A
               LD   A,(counter+1)
               LD   (mk.c3+1),A
               LD   A,E

               EX   DE,HL
mk.sto14:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CALL mk.bp13         ;inc "bp13"

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
               LD   (mk.sto15+1),HL ;ld hl,borderplay2
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&11
               INC  HL
qs.playtab2.1: LD   (HL),0
               INC  HL
qs.playtab2.2: LD   (HL),0            ;ld de,playtab2
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
               LD   (mk.sto16+1),HL ;jp player.rejoin
               INC  HL
               INC  HL

;---------------------------------------------------------------
border.play.2:

               LD   A,(maker+1)
               LD   (poke.count+1),A
               XOR  A
               LD   (counter+1),A

               LD   IX,(mk.timing+2)
               LD   A,(IX)
               INC  IX

               EX   DE,HL
mk.sto15:      LD   HL,0            ;borderplay2:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto17+1),HL ;ld ix,bord.pl21
               INC  HL
               INC  HL
               LD   (HL),&C3
               INC  HL
mk.getc1data:  LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;jp get.c1.data
               INC  HL
mk.sto17:      LD   DE,0
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

mk.c1:         LD   A,0
               LD   (counter+1),A
mk.ix1:        LD   IX,0
mk.a1:         LD   A,0

               CALL mk.bp21         ;inc "bp21"

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto18+1),HL ;ld ix,bord.pl24
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
mk.getc4data:  LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;jp get.c4.data
               INC  HL
mk.sto18:      LD   DE,0
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

mk.c4:         LD   A,0
               LD   (counter+1),A
mk.ix4:        LD   IX,0
mk.a4:         LD   A,0

               CALL mk.bp24         ;inc "bp24"

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto19+1),HL ;ld ix,bordpl2f
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
mk.paltabselr: LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;jp paltabselect
               INC  HL

mk.cp:         LD   A,0
               LD   (counter+1),A
mk.ixp:        LD   IX,0
mk.ap:         LD   A,0


               EX   DE,HL
mk.sto19:      LD   HL,0            ;bordpl2f:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto20+1),HL ;ld ix,bord.pl22
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
mk.getc2data:  LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;jp get.c2.data
               INC  HL
               EX   DE,HL
mk.sto20:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

mk.c2:         LD   A,0
               LD   (counter+1),A
mk.ix2:        LD   IX,0
mk.a2:         LD   A,0

               CALL mk.bp22         ;inc "bp22"

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DD
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (mk.sto21+1),HL ;ld ix,bord.pl23
               INC  HL
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
mk.getc3data:  LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;jp get.c3.data
               INC  HL
               EX   DE,HL
mk.sto21:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

mk.c3:         LD   A,0
               LD   (counter+1),A
mk.ix3:        LD   IX,0
mk.a3:         LD   A,0

               CALL mk.bp23         ;inc "bp23"

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&21
               INC  HL
mk.rec32:      LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;ld hl,borderplay1
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&11
               INC  HL
               LD   (HL),playtab1\256
               INC  HL
               LD   (HL),playtab1/256 ;ld de,playtab1
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&C3
               INC  HL
               LD   (mk.sto22+1),HL ;jp player.rejoin
               INC  HL
               INC  HL

;---------------------------------------------------------------
player.rejoin:

               EX   DE,HL
mk.sto16:      LD   HL,0            ;player.rejoin:
               LD   (HL),E
               INC  HL
               LD   (HL),D
mk.sto22:      LD   HL,0
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL

               CP   5
               CALL C,insert.xout
               SUB  5

               LD   (HL),&22
               INC  HL
mk.rec2:       LD   DE,0
               INC  DE
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;ld (playerselect+1),hl
               INC  HL

               CP   6
               CALL C,insert.xout
               SUB  6

               LD   (HL),&ED
               INC  HL
               LD   (HL),&53
               INC  HL
               LD   (mk.sto23+1),HL ;ld (playtabselect+1),de
               INC  HL
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
               LD   (bp.seq.p),HL
             ; LD   (HL),sq.page    ;ld a,sq.page
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),251        ;out (251),a
               INC  HL

               CP   3
               CALL C,insert.xout
               SUB  3

               LD   (HL),&31
               INC  HL
               EX   DE,HL
mk.sto4:       LD   HL,0            ;prog.sp:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL           ;ld sp,0000
               INC  HL
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&3E
               INC  HL
ras.start.1:   LD   (HL),0          ;ld a,ras.start
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&D3
               INC  HL
               LD   (HL),ras.int    ;out (ras.int),a
               INC  HL

               CALL insert.xout
               CP   255
               CALL NZ,insert.xout

               LD   (HL),&D9        ;exx
               INC  HL

               LD   (HL),&08        ;ex af,af'
               INC  HL

               LD   (HL),&21
               INC  HL
               EX   DE,HL
mk.sto23:      LD   HL,0            ;playtabselect:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL           ;ld hl,00000
               INC  HL
               INC  HL
               LD   (HL),&D9        ;exx
               INC  HL

               LD   (HL),&FB        ;ei
               INC  HL
               LD   (HL),&CD
               INC  HL
               LD   (bp.sequence),HL
             ; LD   (HL),0
               INC  HL
             ; LD   (HL),0          ;call sequencer
               INC  HL
               LD   (HL),&AF
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),248
               INC  HL
               LD   (HL),&DD
               INC  HL
               LD   (HL),&E1        ;pop ix
               INC  HL
               LD   (HL),&E1        ;pop hl
               INC  HL
               LD   (HL),&D1        ;pop de
               INC  HL
               LD   (HL),&C1        ;pop bc
               INC  HL
               LD   (HL),&3E
               INC  HL
               EX   DE,HL
mk.sto3:       LD   HL,0            ;prog.p:
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL           ;ld a,0
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251        ;out (251),a
               INC  HL
               LD   (HL),&F1        ;pop af
               INC  HL
               LD   (HL),&C9        ;ret
               INC  HL

;---------------------------------------------------------------
;enable burstplayer
enable:
               LD   (bp.enable),HL  ;enableplayer:

               LD   (HL),&DB
               INC  HL
               LD   (HL),int.stat   ;in a,(int.stat)
               INC  HL
               LD   (HL),&E6
               INC  HL
               LD   (HL),&08        ;and 8
               INC  HL
               LD   (HL),&20
               INC  HL
               LD   (HL),-6         ;jr nz,$-4
               INC  HL
               LD   (HL),&DB
               INC  HL
               LD   (HL),int.stat   ;in a,(int.stat)
               INC  HL
               LD   (HL),&E6
               INC  HL
               LD   (HL),&08        ;and 8
               INC  HL
               LD   (HL),&28
               INC  HL
               LD   (HL),-6         ;jr z,$-4
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   DE,(mk.rec32+1)
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;ld hl,borderplay1
               INC  HL
               LD   (HL),&22
               INC  HL
               LD   DE,(mk.rec2+1)
               INC  DE
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;ld (playerselect+1),hl
               INC  HL
               LD   (HL),&08        ;ex af,af'
               INC  HL
               LD   (HL),&D9        ;exx
               INC  HL
               LD   (HL),&01
               INC  HL
sample.port:   LD   DE,232
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;ld bc,sample.port
               INC  HL
               LD   (HL),&21
               INC  HL
               LD   (HL),playtab1\256
               INC  HL
               LD   (HL),playtab1/256 ;ld hl,playtab1
               INC  HL
               LD   (HL),&11
               INC  HL
sample.ctrl:   LD   DE,1
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;ld de,sample.ctrl
               INC  HL
               LD   (HL),&3E
               INC  HL
ras.start.2:   LD   (HL),0          ;ld a,ras.start
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),ras.int    ;out (ras.int),a
               INC  HL
               LD   (HL),&D9        ;exx
               INC  HL
               LD   (HL),&08        ;ex af,af'
               INC  HL
               LD   (HL),&FB        ;ei
               INC  HL
               LD   (HL),&C9        ;ret
               INC  HL

;---------------------------------------------------------------
;set silence
set.silence:
               LD   (mk.rec37+1),HL ;set.silence
               LD   (HL),&21
               INC  HL
               LD   (HL),playtab1\256
               INC  HL
               LD   (HL),playtab1/256 ;ld hl,playtab1
               INC  HL
               LD   (HL),&54       ;ld d,h
               INC  HL
               LD   (HL),&5D       ;ld e,l
               INC  HL
               LD   (HL),&13       ;inc de
               INC  HL
               LD   (HL),&36
               INC  HL
               LD   A,(device)
               DEC  A
               LD   A,%10001000
               JR   Z,$+4
               LD   A,%10000000    ;ld (hl),n
               LD   (HL),A         ;%10000000 for all devices
               INC  HL             ;except soundchip %10001000
               LD   (HL),&01
               INC  HL
               LD   DE,8*208-1
               LD   A,(device)
               CP   7
               JR   Z,$+5
               LD   DE,4*208-1
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld bc,4*208-1 (8* = QSS)
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&B0       ;ldir
               INC  HL
               LD   (HL),&C9        ;ret
               INC  HL

;---------------------------------------------------------------
;test

               LD   (run.program+1),HL
               LD   (HL),&CD
               INC  HL
               LD   (HL),reset.output\256
               INC  HL
               LD   (HL),reset.output/256  ;call reset sound dev
               INC  HL
               LD   (HL),&CD
               INC  HL
mk.rec37:      LD   DE,0
               LD   (HL),E
               INC  HL
               LD   (HL),D          ;call set.silence
               INC  HL

               LD   (HL),&3E
               INC  HL
               LD   (bp.demo.p),HL
             ; LD   (HL),sq.page   ;ld a,sq.page
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),251       ;out (251),a
               INC  HL
               LD   (HL),&C3
               INC  HL
               LD   (bp.demo),HL
;              LD   (HL),0
               INC  HL
;              LD   (HL),0
               INC  HL

;---------------------------------------------------------------
;swap channel 3 and 4 addresses if device is SAA

               LD   A,(device)
               LD   (bp.device),A
               DEC  A
               RET  NZ

               LD   HL,bp.c3.page
               LD   DE,bp.c4.page
               LD   B,12
mk.swap.lp:
               LD   C,(HL)
               LD   A,(DE)
               LD   (HL),A
               LD   A,C
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ mk.swap.lp

               RET                 ;!!!!!!!!!!!!!!!!!!


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
cv.vol.bits:   LD   DE,0
               LD   HL,0

cv.volume:     LD   A,0
               OR   A
               JR   Z,cv.no.mul
               LD   B,A
cv.mul.vol:    ADD  HL,DE
               DJNZ cv.mul.vol
cv.no.mul:
               LD   B,H
               LD   C,L
cv.div.by:     LD   DE,15          ;tables-1
               CALL cv.bc.div.de
               LD   (cv.range+1),BC

cv.vol.base.1: LD   HL,&0800       ;H=2^(bits-1)
               LD   B,128

;2^bits * v/15

cv.range:      LD   DE,15          ;range

cv.blp:
               LD   (IX),H
               INC  IX

               ADD  HL,DE

               DJNZ cv.blp

               LD   C,127
               ADD  IX,BC

cv.vol.base.2: LD   HL,&0800
               LD   B,128
cv.blp2:
               OR   A
               SBC  HL,DE

               LD   (IX),H
               DEC  IX
               DJNZ cv.blp2

cv.skip.table: LD   BC,129         ;B=1 if SAA
               ADD  IX,BC

               LD   A,(cv.volume+1)
               INC  A
               LD   (cv.volume+1),A
cv.max.tables: CP   0
               JR   NZ,cv.loop

               POP  HL
cv.no.double:  RET                 ;NOP if SAA

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

;---------------------------------------------------------------
;make pitch table for Amiga -> SAM sample rate

make.pitch:
               LD   IX,pitch.table+4
             ; LD   B,4
pitch.clp:   ; LD   (IX),0         ;for nocalc on lowest two div
             ; INC  IX
             ; DJNZ pitch.clp

               LD   HL,43544       ;PAL  -> CHL=7093789.2
               LD   A,(amiga)
               CP   pal
               JR   Z,not.ntsc
               LD   HL,45152       ;NTSC -> CHL=7159090.5
not.ntsc:
               LD   (lo.amiga+1),HL

               LD   DE,2
               LD   C,0

;divide CDE by CDE'
;put result in (IX+0), (IX+1)

pitch.loop:
               EXX

lo.amiga:      LD   DE,43544       ;CHL=7093789.2*128/10400
               LD   C,2            ;value in table = HL/offs
                                   ;*2 for rounding at end
               LD   B,24
               EXX
;divide:
               LD   B,0
               LD   HL,0
               EXX
mp.divlp1:
               RL   E
               RL   D
               RL   C
               EXX
               ADC  HL,HL
               LD   A,B
               ADC  A,A
               LD   B,A
               SBC  HL,DE
               LD   A,B
               SBC  A,C
               LD   B,A
               JR   NC,mp.divskip1
               ADD  HL,DE
               LD   A,B
               ADC  A,C
               LD   B,A
mp.divskip1:
               CCF
               EXX
               DJNZ mp.divlp1

               LD   HL,0
               ADC  HL,DE

               LD   (IX),L
               LD   (IX+1),H
               INC  IX
               INC  IX
               EXX

               INC  DE

               LD   A,D
               CP   1024/256
               JR   NZ,pitch.loop
               RET

;---------------------------------------------------------------
;insert output commands for sound device

insert.outs:
               EX   DE,HL
               PUSH BC
sound.driver:  LD   HL,0
sound.dr.len:  LD   BC,0
               LDIR
               POP  BC
               EX   DE,HL
               RET

insert.xout:
               CP   3
               JR   C,ins.notjr
               LD   (HL),&18       ;a = 3 or 4 or 5
               INC  HL
               LD   (HL),0         ;jr $+2
               INC  HL
               SUB  3
               JR   insert.xout
ins.notjr:
               OR   A
               JR   Z,ins.allnop
ins.noplp:
               LD   (HL),&00       ;nop
               INC  HL
               DEC  A
               JR   NZ,ins.noplp
ins.allnop:
               LD   A,(maker+1)
               AND  1
               CALL NZ,time

               LD   (HL),&D9       ;exx
               INC  HL
               CALL insert.outs
               LD   (HL),&D9       ;exx
               INC  HL

               LD   A,(maker+1)
               AND  1
               CALL Z,time

               LD   A,(IX)
               INC  IX
               SCF
               RET

time:
               PUSH AF
counter:       LD   A,0
               INC  A
poke.count:    CP   0
               JR   NZ,notborder

               LD   (HL),&08       ;ex af,af'
               INC  HL
               LD   (HL),&3E
               INC  HL
border.col:    LD   (HL),&70       ;ld a,3
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),248       ;out (248),a
               INC  HL
               LD   (HL),&08       ;ex af,af'
               INC  HL

notborder:     LD   (counter+1),A
               POP  AF
               RET

;---------------------------------------------------------------
;make channel 1 for burstplayer 1

mk.bp11:
               LD   DE,2*208+playtab1
               LD   (mk.playtab),DE

               LD   DE,(bp.c1.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c1.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c1.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get1st

               LD   DE,4*208+playtab1+2  ;left
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 4 for burstplayer 1

mk.bp14:
               LD   DE,(bp.c4.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c4.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c4.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get2nd

               LD   DE,4*208+playtab1+0  ;left
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 2 for burstplayer 1

mk.bp12:
               LD   DE,2*208+playtab1+1   ;playtab2
               LD   (mk.playtab),DE

               LD   DE,(bp.c2.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c2.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c2.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get1st

               LD   DE,4*208+playtab1+3  ;right
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 3 for burstplayer 1

mk.bp13:
               LD   DE,(bp.c3.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c3.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c3.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get2nd

               LD   DE,4*208+playtab1+1  ;right
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 1 for burstplayer 2

mk.bp21:
               LD   DE,playtab1
               LD   (mk.playtab),DE

               LD   DE,(bp.c1.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c1.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c1.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get1st

               LD   DE,playtab1+2  ;left
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 4 for burstplayer 2

mk.bp24:
               LD   DE,(bp.c4.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c4.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c4.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get2nd

               LD   DE,playtab1+0  ;left
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 2 for burstplayer 2

mk.bp22:
               LD   DE,playtab1+1
               LD   (mk.playtab),DE

               LD   DE,(bp.c2.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c2.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c2.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get1st

               LD   DE,playtab1+3  ;right
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make channel 3 for burstplayer 2

mk.bp23:
               LD   DE,(bp.c3.sp.frct)
               LD   (mk.gd.spfr),DE

               LD   DE,(bp.c3.page)
               LD   (mk.gd.page),DE

               LD   DE,(bp.c3.offs)
               LD   (mk.gd.offs),DE

               PUSH AF
               LD   A,(device)
               CP   7
               JP   NZ,mk.bp.get2nd

               LD   DE,playtab1+1  ;right
               LD   (mk.playtab),DE
               JP   mk.qss.get

;---------------------------------------------------------------
;make get first sample byte routine

mk.bp.get1st:
               POP  AF
               LD   IY,mk.store

               LD   B,208/3        ;bytes per frame
blp1.1:
               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&46       ;ld b,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&08       ;ex af,af'
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   (IY+0),L
               LD   (IY+1),H
               INC  IY
               INC  IY
;              LD   (HL),0
               INC  HL
;              LD   (HL),0         ;ld (nn),a
               INC  HL

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&58       ;ld e,b
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   (IY+0),L
               LD   (IY+1),H
               INC  IY
               INC  IY
;              LD   (HL),0
               INC  HL
;              LD   (HL),0         ;ld (nn),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   (IY+0),L
               LD   (IY+1),H
               INC  IY
               INC  IY
;              LD   (HL),0
               INC  HL
;              LD   (HL),0         ;ld (nn),a
               INC  HL

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&08       ;ex af,af'
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               DEC  B
               JP   NZ,blp1.1

;now get last byte and store sample pointer

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   BC,(mk.gd.spfr)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (speedfract+1),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   (IY+0),L
               LD   (IY+1),H
               INC  IY
               INC  IY
;              LD   (HL),0
               INC  HL
;              LD   (HL),0         ;ld (nn),a
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL

               CP   2+3
               CALL C,insert.xout
               SUB  2+3

               LD   (HL),&CB
               INC  HL
               LD   (HL),&74       ;bit 6,h
               INC  HL
               LD   (HL),&28
               INC  HL
               LD   (HL),1         ;jr z,$+3
               INC  HL
               LD   (HL),&3C       ;inc a
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   BC,(mk.gd.page)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (samplepage+1),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&CB
               INC  HL
               LD   (HL),&B4       ;res 6,h
               INC  HL

               CP   5
               CALL C,insert.xout
               SUB  5

               LD   (HL),&22
               INC  HL
               LD   BC,(mk.gd.offs)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (sample.offs+1),hl
               INC  HL

               RET

;---------------------------------------------------------------
;make get second sample byte and add to first routine

mk.bp.get2nd:
               POP  AF
               LD   IY,mk.store

               LD   B,208/3        ;bytes per frame
blp1.4:
               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&46       ;ld b,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&08       ;ex af,af'
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&C6
               INC  HL
;              LD   (HL),0         ;add a,n
               EX   DE,HL
               LD   L,(IY+0)
               LD   H,(IY+1)
               INC  IY
               INC  IY
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (2*x+playtab2),a
               INC  HL
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&58       ;ld e,b
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&C6
               INC  HL
;              LD   (HL),0         ;add a,n
               EX   DE,HL
               LD   L,(IY+0)
               LD   H,(IY+1)
               INC  IY
               INC  IY
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (2*x+playtab2),a
               INC  HL
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&C6
               INC  HL
;              LD   (HL),0         ;add a,n
               EX   DE,HL
               LD   L,(IY+0)
               LD   H,(IY+1)
               INC  IY
               INC  IY
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (2*x+playtab2),a
               INC  HL
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&08       ;ex af,af'
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               DEC  B
               JP   NZ,blp1.4

;now get last byte and store sample pointer

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   BC,(mk.gd.spfr)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (speedfract+1),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&C6
               INC  HL
;              LD   (HL),0         ;add a,n
               EX   DE,HL
               LD   L,(IY+0)
               LD   H,(IY+1)
               INC  IY
               INC  IY
               LD   (HL),E
               INC  HL
               LD   (HL),D
               EX   DE,HL
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (2*x+playtab2),a
               INC  HL
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL

               CP   2+3
               CALL C,insert.xout
               SUB  2+3

               LD   (HL),&CB
               INC  HL
               LD   (HL),&74       ;bit 6,h
               INC  HL
               LD   (HL),&28
               INC  HL
               LD   (HL),1         ;jr z,$+3
               INC  HL
               LD   (HL),&3C       ;inc a
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   BC,(mk.gd.page)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (samplepage+1),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&CB
               INC  HL
               LD   (HL),&B4       ;res 6,h
               INC  HL

               CP   5
               CALL C,insert.xout
               SUB  5

               LD   (HL),&22
               INC  HL
               LD   BC,(mk.gd.offs)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (sample.offs+1),hl
               INC  HL

               RET

;---------------------------------------------------------------
;make get sample byte for QSS - no mixing -> same routine 4*

mk.qss.get:
               POP  AF

               LD   B,208/3        ;bytes per frame
q.blp1.4:
               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&46       ;ld b,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&08       ;ex af,af'
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (4*x+playtabx),a
               INC  HL
               INC  DE
               INC  DE
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&58       ;ld e,b
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (4*x+playtabx),a
               INC  HL
               INC  DE
               INC  DE
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (4*x+playtabx),a
               INC  HL
               INC  DE
               INC  DE
               INC  DE
               INC  DE
               LD   (mk.playtab),DE

               CP   1
               CALL C,insert.xout
               SUB  1

               LD   (HL),&08       ;ex af,af'
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               DEC  B
               JP   NZ,q.blp1.4

;now get last byte and store sample pointer

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&5E       ;ld e,(hl)
               INC  HL

               CP   1+4
               CALL C,insert.xout
               SUB  1+4

               LD   (HL),&81       ;add a,c
               INC  HL
               LD   (HL),&ED
               INC  HL
               LD   (HL),&7A       ;adc hl,sp
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   BC,(mk.gd.spfr)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (speedfract+1),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&1A       ;ld a,(de)
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   DE,(mk.playtab)
               LD   (HL),E
               INC  HL
               LD   (HL),D         ;ld (4*x+playtabx),a
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&DB
               INC  HL
               LD   (HL),251       ;in a,(251)
               INC  HL

               CP   2+3
               CALL C,insert.xout
               SUB  2+3

               LD   (HL),&CB
               INC  HL
               LD   (HL),&74       ;bit 6,h
               INC  HL
               LD   (HL),&28
               INC  HL
               LD   (HL),1         ;jr z,$+3
               INC  HL
               LD   (HL),&3C       ;inc a
               INC  HL

               CP   4
               CALL C,insert.xout
               SUB  4

               LD   (HL),&32
               INC  HL
               LD   BC,(mk.gd.page)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (samplepage+1),a
               INC  HL

               CP   2
               CALL C,insert.xout
               SUB  2

               LD   (HL),&CB
               INC  HL
               LD   (HL),&B4       ;res 6,h
               INC  HL

               CP   5
               CALL C,insert.xout
               SUB  5

               LD   (HL),&22
               INC  HL
               LD   BC,(mk.gd.offs)
               LD   (HL),C
               INC  HL
               LD   (HL),B         ;ld (sample.offs+1),hl
               INC  HL

               RET


;---------------------------------------------------------------
timeline:
               LD   (HL),&5F       ;ld e,a
               INC  HL
               LD   (HL),&3E
               INC  HL
               LD   (HL),&70       ;ld a,&70
               INC  HL
               LD   (HL),&D3
               INC  HL
               LD   (HL),248       ;out (248),a
               INC  HL
               LD   (HL),&7B       ;ld a,e
               INC  HL
               RET

;---------------------------------------------------------------
;memory needed for mk.bp routines

mk.playtab:    DEFW 0
mk.gd.spfr:    DEFW 0
mk.gd.page:    DEFW 0
mk.gd.offs:    DEFW 0

mk.store:      DEFS 208*2          ;stores adresses

;---------------------------------------------------------------
;timing tables for sound devices
;last byte in table is delay during line interrupt

timing.clut:
               DEFB 033,121,121,121,121,121,121,121,121,121  ; 1
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 2
               DEFB 121,121,121,121,121,122,121,121,121,121  ; 3
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 4
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 5
               DEFB 121,121,121,122,123,121,121,121,121,121  ; 6
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 7
               DEFB 121,121,121,121,121,121,121,121,121,122  ; 8
               DEFB 110,076,079,076,078,076,079,077,082,077  ; 9
               DEFB 080,075,079,076,078,076,079,077,082,077  ;10
               DEFB 080,075,079,076,078,076,079,077,082,077  ;11
               DEFB 080,075,079,076,078,076,079,077,082,077  ;12
               DEFB 080,075,080,090,097,255,255,255,255,083  ;13
timing.saa:
               DEFB 028,119,119,119,119,119,119,119,119,119  ; 1
               DEFB 119,119,119,119,119,119,119,119,119,119  ; 2
               DEFB 119,119,119,119,119,119,119,119,119,119  ; 3
               DEFB 119,119,119,119,119,119,119,119,119,119  ; 4
               DEFB 119,119,119,119,119,119,119,119,119,119  ; 5
               DEFB 119,119,119,119,121,119,119,119,119,119  ; 6
               DEFB 119,119,119,119,119,119,119,119,119,119  ; 7
               DEFB 119,119,119,119,119,119,119,119,119,119  ; 8
               DEFB 107,076,081,075,080,074,079,073,078,075  ; 9
               DEFB 077,075,079,076,081,075,080,074,079,073  ;10
               DEFB 078,075,077,075,079,076,081,075,080,074  ;11
               DEFB 079,073,078,075,077,075,079,076,081,075  ;12
               DEFB 080,074,079,073,080,089,095,255,255,078  ;13
timing.eddac:
               DEFB 032,122,122,122,122,122,122,122,122,122  ; 1
               DEFB 122,122,122,122,122,122,122,122,122,122  ; 2
               DEFB 122,122,122,122,122,122,122,122,122,122  ; 3
               DEFB 122,122,122,122,122,122,122,122,122,122  ; 4
               DEFB 122,122,122,122,122,122,122,122,122,122  ; 5
               DEFB 122,122,122,123,124,122,122,122,122,122  ; 6
               DEFB 122,122,122,122,122,122,122,122,122,122  ; 7
               DEFB 122,122,122,122,122,122,122,122,122,123  ; 8
               DEFB 109,080,080,081,081,078,080,078,078,081  ; 9
               DEFB 082,079,079,078,078,080,080,081,081,078  ;10
               DEFB 080,078,078,081,082,079,079,078,078,080  ;11
               DEFB 080,081,081,078,080,078,078,081,082,079  ;12
               DEFB 081,092,097,255,255,255,255,255,255,083  ;13
timing.dac:
               DEFB 040,129,129,129,129,129,129,129,129,129  ; 1
               DEFB 129,129,129,129,129,129,129,129,129,129  ; 2
               DEFB 129,129,129,129,129,129,129,129,129,129  ; 3
               DEFB 129,129,129,129,129,129,129,129,129,129  ; 4
               DEFB 129,129,129,129,129,129,129,129,129,129  ; 5
               DEFB 130,129,130,129,129,129,129,129,129,129  ; 6
               DEFB 129,129,129,129,129,129,129,129,129,129  ; 7
               DEFB 129,129,129,129,129,129,129,129,129,129  ; 8
               DEFB 118,084,085,084,088,084,085,084,088,084  ; 9
               DEFB 085,084,088,084,085,084,088,084,085,084  ;10
               DEFB 088,084,085,084,088,084,085,084,088,084  ;11
               DEFB 085,083,089,255,255,255,255,255,255,255  ;12
               DEFB 255,255,255,255,255,255,255,255,255,093  ;13
timing.qss:
               DEFB 032,121,121,121,121,121,121,121,121,121  ; 1
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 2
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 3
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 4
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 5
               DEFB 121,123,122,121,121,121,121,121,121,121  ; 6
               DEFB 121,121,121,121,121,121,121,121,121,121  ; 7
               DEFB 121,121,121,121,121,121,122,121,121,121  ; 8
               DEFB 109,081,078,081,080,080,081,079,080,080  ; 9
               DEFB 078,081,080,080,081,079,080,080,078,081  ;10
               DEFB 080,080,081,079,080,080,078,081,080,080  ;11
               DEFB 081,079,093,255,255,255,255,255,255,255  ;12
               DEFB 255,255,255,255,255,255,255,255,255,083  ;13

;===============================================================
;device list

device.list:   DEFW 0,0                  ;clut
               DEFW sd.clut,10           ;sound device, length
               DEFW 248                  ;output port
               DEFW &1734                ;control
               DEFW timing.clut          ;timing table
               DEFB 22*3+2               ;rasi
               DEFB 6                    ;number of bits

               DEFW i.saa,e.saa-i.saa    ;saa
               DEFW sd.saa,10
               DEFW 511
               DEFW &0205
               DEFW timing.saa
               DEFB 23*3+2
               DEFB 4

               DEFW 0,0                  ;eddac
               DEFW sd.eddac,12
               DEFW 232
               DEFW &0001
               DEFW timing.eddac
               DEFB 21*3+2
               DEFB 7

               DEFW 0,0                  ;eddac 2
               DEFW sd.eddac,12
               DEFW 234
               DEFW &0001
               DEFW timing.eddac
               DEFB 21*3+2
               DEFB 7

               DEFW 0,0                  ;dac
               DEFW sd.dac,8
               DEFW 232
               DEFW 0
               DEFW timing.dac
               DEFB 16*3+2
               DEFB 6

               DEFW 0,0                  ;dac 2
               DEFW sd.dac,8
               DEFW 234
               DEFW 0
               DEFW timing.dac
               DEFB 16*3+2
               DEFB 6

               DEFW i.bla,e.bla-i.bla    ;blue alpha
               DEFW sd.dac,8
               DEFW 124*256+127
               DEFW 0
               DEFW timing.dac
               DEFB 16*3+2
               DEFB 6

               DEFW i.qss,e.qss-i.qss    ;quazar surround soundc
               DEFW sd.qss,9
               DEFW &06D0
               DEFW &0006                ;+1 for OUTI
               DEFW timing.qss
               DEFB 16*3+2
               DEFB 8
;---------------------------------------------------------------
;initialise device subroutines

i.saa:
               LD   A,%10001000    ;A=silence value
               LD   BC,511
               LD   DE,32*256+31
               XOR  A
bp.res.saa:    OUT  (C),E
               OUT  (255),A
               DEC  E
               DEC  D
               JR   NZ,bp.res.saa
               LD   HL,bp.soundtab
               LD   B,6
               OTIR
e.saa:
i.bla:
               LD   BC,127*256+127
               LD   A,255
               OUT  (C),A
               LD   B,125
               LD   A,253
               OUT  (C),A
e.bla:
i.qss:
               LD   BC,&06D0
               IN   A,(C)          ;mode 1
               LD   A,128
               DEC  B
               OUT  (C),A          ;rear right
               DEC  B
               OUT  (C),A          ;rear left
               DEC  B
               OUT  (C),A          ;front right
               DEC  B
               OUT  (C),A          ;front left
e.qss:
;---------------------------------------------------------------
;sound drivers

sd.clut:
               OUT  (C),E          ; 16   2
               INC  B              ;  4   1
               OUTI                ; 24   2
               OUT  (C),D          ; 16   2
               INC  B              ;  4   1
               OUTI                ; 24   2  = 22

sd.saa:
               OUT  (C),E          ; 16   2
               OUTI                ; 24   2
               INC  B              ;  4   1
               OUT  (C),D          ; 16   2
               OUTI                ; 24   2
               INC  B              ;  4   1  = 26

sd.eddac:
               OUTI                ; 20   2
               INC  C              ;  4   1
               OUT  (C),E          ; 12   2
               DEC  C              ;  4   1
               OUTI                ; 20   2
               INC  C              ;  4   1
               OUT  (C),D          ; 20   2
               DEC  C              ;  4   1

sd.dac:
               LD   E,A            ;  4   1
               LD   A,(HL)         ;  8   1
               INC  HL             ;  8   1
               ADD  (HL)           ;  8   1
               INC  HL             ;  8   1
               OUT  (C),A          ; 12   2
               LD   A,E            ;  4   1

sd.qss:
               LD   B,E            ;  4   1
               OUTI                ; 20   2
               OUTI                ; 20   2
               OUTI                ; 20   2
               OUTI                ; 20   2  = 21

;===============================================================
mk.movecode:
               ORG  0

               DI
               IN   A,(252)
               PUSH AF
               LD   (bp.stsp+32769),SP
               IN   A,(250)
               LD   (bp.stlmpr+32769),A
               IN   A,(251)
               AND  31
               LD   (bp.exitpage+32769),A
               OR   32
               OUT  (250),A
               LD   SP,32768
run.program:   JP   0              ;test

bp.exit:       DI
               LD   A,255
               OUT  (249),A
bp.exitpage:   LD   A,0
               OUT  (251),A
               JP   bp.stlmpr+32768

bp.stlmpr:     LD   A,0
               OUT  (250),A
bp.stsp:       LD   SP,0
               POP  AF
               OUT  (252),A
             ; EI
               RET

               DEFM "BUR"          ;ID code (at 00053)

;---------------------------------------------------------------
               DEFS 102-$          ;NMI - corrupts player

               JP   bp.exit


bp.device:     DEFB 0
bp.sequence:   DEFW 0
bp.seq.p:      DEFW 0
bp.demo:       DEFW 0
bp.demo.p:     DEFW 0
bp.enable:     DEFW 0

               DEFW bp.exit

bp.c1.page:    DEFW 0
bp.c1.offs:    DEFW 0
bp.c1.vol:     DEFW 0
bp.c1.speedlo: DEFW 0
bp.c1.speedhi: DEFW 0
bp.c1.sp.frct: DEFW 0

bp.c2.page:    DEFW 0
bp.c2.offs:    DEFW 0
bp.c2.vol:     DEFW 0
bp.c2.speedlo: DEFW 0
bp.c2.speedhi: DEFW 0
bp.c2.sp.frct: DEFW 0

bp.c3.page:    DEFW 0
bp.c3.offs:    DEFW 0
bp.c3.vol:     DEFW 0
bp.c3.speedlo: DEFW 0
bp.c3.speedhi: DEFW 0
bp.c3.sp.frct: DEFW 0

bp.c4.page:    DEFW 0
bp.c4.offs:    DEFW 0
bp.c4.vol:     DEFW 0
bp.c4.speedlo: DEFW 0
bp.c4.speedhi: DEFW 0
bp.c4.sp.frct: DEFW 0


bp.soundtab:
               DEFB 28,1,25,130,24,130   ;set saa for samples



mk.mv.end:
reset.output:

length:        EQU  mk.movecode-32768+reset.output

;===============================================================

;rasterline = 384 T-states / 4 = 96 * 1.5 = 144
;bytes per frame = 10400 Hz / 50 = 208
;192 screen lines
;120 border lines
;312 total lines / 208 = 1.5

;===============================================================

               ORG  32768
               DUMP sq.page,0

               JP   init.sq

demo:
               CALL 0

               LD   BC,247*256+249
demoloop:
               XOR  A
               OUT  (248),A
               IN   A,(C)
               BIT  5,A
               JR   NZ,demoloop
still:
               IN   A,(C)
               BIT  5,A
               JR   Z,still

exit:          JP   bp.exit

init.sq:
               DI
               IN   A,(250)
               LD   (is.stlmpr+1),A
               LD   A,2+32         ;burst page
               OUT  (250),A
               LD   HL,(bp.sequence)
               LD   (HL),sequencer\256
               INC  HL
               LD   (HL),sequencer/256
               LD   HL,demo+1
               LD   DE,(bp.enable)
               LD   (HL),E
               INC  HL
               LD   (HL),D
               LD   HL,(bp.demo)
               LD   (HL),demo\256
               INC  HL
               LD   (HL),demo/256

is.stlmpr:     LD   A,0
               OUT  (250),A
               EI
               RET


sequencer:
               RET
