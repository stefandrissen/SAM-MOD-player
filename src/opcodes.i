;opcode defines for generating code

opcode_nop:			equ &00 ; nop
opcode_ex_af_af:	equ &08	; ex af,af'
opcode_ld_de_nn:	equ &11 ; ld de,nn
opcode_ld_nn_hl:	equ &22 ; ld (nn),hl
opcode_jr_z_n:		equ &28 ; jr z,n
opcode_inc_l:		equ &2C ; inc l
opcode_ld_nn_a:		equ &32 ; ld (nn),a
opcode_ld_hl_n:		equ &36 ; ld (hl),n
opcode_inc_a:		equ &3C	; inc a
opcode_ld_a_n:		equ &3E ; ld a,n
opcode_ld_l_a:		equ &6F ; ld l,a
opcode_ld_a_l:		equ &7D ; ld a,l
opcode_add_a_b:		equ &80 ; ld a,b
opcode_jp_nn:		equ &C3	; jp nn
opcode_push_bc:		equ &C5 ; push bc
opcode_add_a_n:		equ &C6 ; add a,n
opcode_ret:			equ &C9 ; ret
opcode_call_nn:		equ &CD ; call nn
opcode_out_n_a:		equ &D3 ; out (n),a
opcode_push_de:		equ &D5 ; push de
opcode_exx:			equ &D9 ; exx
opcode_in_a_n:		equ &DB ; in a,(n)
opcode_pop_hl:		equ &E1 ; pop hl
opcode_push_hl:		equ &E5 ; push hl
opcode_push_af:		equ &F5 ; push af
opcode_ei:			equ &FB	; ei
opcode_cp_n:		equ &FE ; cp n

opcode_cb:			equ &CB	; cb modifier

opcode_bit_6_h:		equ &74	; bit 6,h
opcode_res_6_h:		equ &B4	; res 6,h 	
opcode_res_7_l:		equ &BD	; res 7,l

opcode_ed:			equ &ED	; ed modifier

opcode_ld_nn_sp:	equ &73 ; ld (nn),sp


opcode_ix:			equ &DD	; ix modifier

opcode_push_ix:		equ &E5 ; push ix
