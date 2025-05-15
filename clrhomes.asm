;;; C. L. R. "Shiftlock" Home, Solitary consulting detective
abillen	.byte	(+)-ability
ability	.byte	$09,$0e,$16,$05	; INVE
	.byte	$13,$14,$40,$20	; ST_
	.byte	$1f,$14,$08,$12	; <THR
	.byte	$05,$01,$14,$20	; EAT
	.byte	$1f,$09,$0e,$16	; <INV
	.byte	$05,$13,$14,$20	; EST
	.byte	$06,$09,$0e,$04	; FIND
	.byte	$20,$0f,$0e,$05	;  ONE
+

double1	jmp	inv_l2r

double2	jmp	thr_r2l

double3	jmp	inv_r2l

double4	jmp	findone
